! 
! Copyright (C) 2004 WanT Group
!
! This file is distributed under the terms of the
! GNU General Public License. See the file `License'
! in the root directory of the present distribution,
! or http://www.gnu.org/copyleft/gpl.txt .
!
! Based on a previous version by Nicola Marzari and David Vanderbilt
! See the file README in the root directory for a full list of credits
!
!=====================================================
   PROGRAM dos_prog
   !=====================================================
   !  
   ! Interpolates the electronic structure from the knowledge of
   ! the direct lattice hamiltonian on Wannier function basis
   ! and compute the Density of states (DOS)
   !
   USE kinds
   USE parameters,           ONLY : nstrx
   USE constants,            ONLY : CZERO, ZERO, ONE, TWO, TPI
   USE io_module,            ONLY : stdout, stdin, io_name, ham_unit, aux_unit, space_unit
   USE io_module,            ONLY : work_dir, prefix, postfix
   USE files_module,         ONLY : file_open, file_close
   USE version_module,       ONLY : version_number
   USE util_module,          ONLY : mat_hdiag
   USE converters_module,    ONLY : cry2cart, cart2cry
   USE parser_module,        ONLY : log2char, int2char
   USE lattice_module,       ONLY : avec, bvec
   USE kpoints_module,       ONLY : nrtot, vr, wr 
   USE windows_module,       ONLY : windows_read, nspin
   USE subspace_module,      ONLY : subspace_read
   USE smearing_module,      ONLY : smearing_func
   USE hamiltonian_module,   ONLY : dimwann, rham, hamiltonian_read, hamiltonian_init
   USE parser_module
   USE want_interfaces_module
   !
   IMPLICIT NONE 

   !
   ! input variables
   !
   INTEGER      :: nk(3)         ! kpt generators
   INTEGER      :: s(3)          ! kpt shifts
   INTEGER      :: ne            ! dimension of the energy grid
   REAL(dbl)    :: emin          ! egrid extrema
   REAL(dbl)    :: emax
   REAL(dbl)    :: delta         ! smearing parameter
   CHARACTER(nstrx) :: smearing_type
   CHARACTER(nstrx) :: fileout   ! output filename
   LOGICAL      :: projdos       ! whether to write WF projected DOS
   LOGICAL      :: use_nn(3)     ! use nearest neighb. in direction i, i=1,3
                                 ! DEBUG and transport purposes
   !
   ! loval variables
   !
   INTEGER      :: nkpts_int     ! Number of interpolated k-points
   INTEGER      :: nrtot_nn
   REAL(dbl)    :: arg, cost, aux
   COMPLEX(dbl) :: phase, caux
   !
   INTEGER,      ALLOCATABLE :: r_index(:)
   COMPLEX(dbl), ALLOCATABLE :: ham(:,:)  
   COMPLEX(dbl), ALLOCATABLE :: z(:,:)        
   REAL(dbl),    ALLOCATABLE :: egrid(:), dos(:), pdos(:,:)
   REAL(dbl),    ALLOCATABLE :: vkpt_int(:,:), wk(:), vr_cry(:,:)
   REAL(dbl),    ALLOCATABLE :: eig_int(:)  ! interpolated band structure   
   CHARACTER(nstrx)          :: filename
   CHARACTER(4)              :: ctmp
   !
   INTEGER      :: i, j, ie, ik, ir, ir_eff
   INTEGER      :: ierr
   LOGICAL      :: lfound

   !
   ! input namelist
   !
   NAMELIST /INPUT/ prefix, postfix, work_dir, nk, s, delta, smearing_type, fileout, &
                    emin, emax, ne, use_nn, projdos
   !
   ! end of declariations
   !   

!
!------------------------------
! main body
!------------------------------
!
      CALL startup(version_number,'dos')


!
! ... Read INPUT namelist from stdin
!
      prefix                      = 'WanT' 
      postfix                     = ' ' 
      work_dir                    = './' 
      fileout                     = ' '
      delta                       = 0.1    ! eV
      nk(:)                       = -1
      s(:)                        =  0
      emin                        = -10.0
      emax                        =  10.0
      ne                          =  1000
      smearing_type               = 'gaussian'
      use_nn(1:3)                 = .FALSE.
      projdos                     = .FALSE.
      
      CALL input_from_file ( stdin, ierr )
      IF ( ierr /= 0 )  CALL errore('dos','error in input from file',ABS(ierr))
      !
      READ(stdin, INPUT, IOSTAT=ierr)
      IF ( ierr /= 0 )  CALL errore('dos','Unable to read namelist INPUT',ABS(ierr))

      IF ( LEN_TRIM(fileout) == 0 ) &
           fileout = TRIM(work_dir)//'/'//TRIM(prefix)//TRIM(postfix)//'_dos.dat'

      !
      ! Some checks 
      !
      IF ( ANY( nk(:) <=0 ) )  CALL errore('dos', 'Invalid nk', 1)
      IF ( ANY( s(:)  < 0 ) )  CALL errore('dos', 'Invalid s', 1)
      IF ( ANY( s(:)  > 1 ) )  CALL errore('dos', 'Invalid s', 2)
      IF ( delta <= 0.0 )      CALL errore('dos', 'Invalid delta', 3)
      IF ( emin > emax  )      CALL errore('dos', 'emin larger than emax', 4)
      IF ( ne <= 0  )          CALL errore('dos', 'invalid ne', 4)
      !
      ! the check of SMEARING_TYPE is done inside the function smearing_func
      ! justmove to lower_case
      CALL change_case(smearing_type,'lower')


!
! ... Getting previous WanT data
!
      CALL want_dftread ( WINDOWS=.FALSE., LATTICE=.TRUE.,  IONS=.TRUE., KPOINTS=.TRUE. )
      CALL want_init    ( INPUT=.FALSE.,   WINDOWS=.FALSE., BSHELLS=.FALSE. )

      !
      ! Read Subspace data
      !
      CALL io_name('space',filename)
      CALL file_open(space_unit,TRIM(filename),PATH="/",ACTION="read")
          !
          CALL windows_read(space_unit,"WINDOWS",lfound)
          IF ( .NOT. lfound ) CALL errore('dos',"unable to find WINDOWS",1)
          !
          CALL subspace_read(space_unit,"SUBSPACE",lfound)
          IF ( .NOT. lfound ) CALL errore('dos',"unable to find SUBSPACE",1)
          !
      CALL file_close(space_unit,PATH="/",ACTION="read")

      CALL io_name('space',filename,LPATH=.FALSE.)
      WRITE( stdout,"(/,2x,'Space data read from file: ',a)") TRIM(filename)

      !
      ! Read hamiltonian data
      !
      CALL io_name('hamiltonian',filename)
      CALL file_open(ham_unit,TRIM(filename),PATH="/",ACTION="read")
          !
          CALL hamiltonian_read(ham_unit,"HAMILTONIAN",lfound)
          IF ( .NOT. lfound ) CALL errore('dos',"unable to find HAMILTONIAN",1)
          !
      CALL file_close(ham_unit,PATH="/",ACTION="read")

      CALL io_name('hamiltonian',filename,LPATH=.FALSE.)
      WRITE( stdout,"(2x,'Hamiltonian data read from file: ',a)") TRIM(filename)


      !
      ! input summary
      !
      WRITE( stdout, "(2/,2x,70('='))" )
      WRITE( stdout, "(2x,'=',27x,'INPUT Summary',28x,'=')" )
      WRITE( stdout, "(2x,70('='),/)" )
      !
      WRITE( stdout, "(   7x,'               fileout :',5x,   a)") TRIM(fileout)
      WRITE( stdout, "(   7x,'                  type :',5x,   a)") TRIM(smearing_type)
      WRITE( stdout, "(   7x,'                 delta :',3x, f9.5, ' eV')" ) delta
      WRITE( stdout, "(   7x,'                    nk :',3x,3i4 )") nk(:)
      WRITE( stdout, "(   7x,'                     s :',3x,3i4 )") s(:)
      WRITE( stdout, "(   7x,'                 nktot :',5x,i6  )") nkpts_int
      !
      IF ( ANY( use_nn(:) ) ) THEN
          WRITE( stdout, "(   7x,'             use_nn(1) :',5x,   a)") TRIM( log2char(use_nn(1)) )
          WRITE( stdout, "(   7x,'             use_nn(2) :',5x,   a)") TRIM( log2char(use_nn(2)) )
          WRITE( stdout, "(   7x,'             use_nn(3) :',5x,   a)") TRIM( log2char(use_nn(3)) )
      ENDIF
      
      !
      ! Print data to output
      !
      CALL summary( stdout, INPUT=.FALSE., IONS=.FALSE., KPOINTS=.FALSE., WINDOWS=.FALSE. )

!
! ... Main task 
!
      WRITE( stdout, "(/,2x,70('='))" )
      WRITE( stdout, "(2x,'=',14x,'DOS computation using Wannier Functions',15x,'=')" )
      WRITE( stdout, "(2x,70('='),/)" )

      !
      ! few local allocations
      !
      nkpts_int = PRODUCT( nk(1:3) )
      IF ( nkpts_int <=0 ) CALL errore( 'dos', 'unexpected nkpts_int ', -nkpts_int + 1)

      ALLOCATE( vkpt_int( 3, nkpts_int ), wk( nkpts_int ), STAT=ierr )
          IF( ierr /=0 ) CALL errore('dos', 'allocating vkpt_int, wk', ABS(ierr) )
      ALLOCATE( egrid( ne ), STAT=ierr )
          IF( ierr /=0 ) CALL errore('dos', 'allocating egrid', ABS(ierr) )
      ALLOCATE( dos( ne ), STAT=ierr )
          IF( ierr /=0 ) CALL errore('dos', 'allocating dos', ABS(ierr) )
      ALLOCATE( pdos( ne, dimwann ), STAT=ierr )
          IF( ierr /=0 ) CALL errore('dos', 'allocating pdos', ABS(ierr) )

      !
      ! setting egrid
      !
      DO ie = 1, ne
          egrid (ie) = emin + REAL(ie-1, dbl) * (emax-emin) / REAL( ne -1, dbl)
      ENDDO

      !
      ! generate monkhorst-pack grid, using nk(:) and s(:)
      ! kpts gen are in crystal coords
      !
      CALL monkpack( nk, s, vkpt_int )
      !
      wk(1:nkpts_int) = TWO / REAL( nspin * nkpts_int ,dbl)
      !
      ! mv kpts in cartesian coords (bohr^-1)
      CALL cry2cart( vkpt_int, bvec )

      !
      ! kpt summary
      !
      WRITE( stdout, "(2x, 'nktot = ',i5 ) " ) nkpts_int
      WRITE( stdout, "(2x, 'Monkhorst-Pack grid:      nk = (',3i4,' ),', &
                                         & 6x,'shift = (',3i4,' )' ) " ) nk(:), s(:)
      WRITE( stdout, "(2x, 'Generated kpt mesh: (cart. coord. in Bohr^-1)',/)" )
      !
      DO ik=1,nkpts_int
           WRITE( stdout, " (4x, 'k (', i5, ') =    ( ',3f9.5,' ),   weight = ', f11.7 )") &
           ik, ( vkpt_int(i,ik), i=1,3 ), wk(ik)
      ENDDO
      WRITE( stdout, "()" )


      !
      ! Determine the k-points used in the band structure interpolation
      !
      ALLOCATE( ham( dimwann, dimwann ), STAT=ierr )
          IF( ierr /=0 ) CALL errore('dos','allocating ham', ABS(ierr) )
      ALLOCATE( z( dimwann, dimwann ), STAT=ierr )
          IF( ierr /=0 ) CALL errore('dos', 'allocating z', ABS(ierr) )
      ALLOCATE( eig_int( dimwann ), STAT=ierr )
          IF( ierr /=0 ) CALL errore('dos','allocating eig_int', ABS(ierr) )

      !
      ! if chosen from input, select only R corresponding to nearest-neighb
      ! along some (crystal) directions
      ! nn_index will point only to the selected nrtot_nn vectors
      !
      ALLOCATE( vr_cry(3, nrtot), STAT=ierr )
          IF( ierr /=0 ) CALL errore('dos','allocating vr_cry', ABS(ierr) )
      ALLOCATE( r_index(nrtot), STAT=ierr )
          IF( ierr /=0 ) CALL errore('dos','allocating r_index', ABS(ierr) )

      !
      ! move vr_int to crystal coords
      !
      vr_cry(:,:) = vr(:,:)
      CALL cart2cry( vr_cry, avec )
      !
      nrtot_nn = 0
      !
      DO ir = 1, nrtot
          !
          IF (  ( .NOT. use_nn(1)  .OR.  ABS(NINT(vr_cry(1,ir))) <= 1 ) .AND. &
                ( .NOT. use_nn(2)  .OR.  ABS(NINT(vr_cry(2,ir))) <= 1 ) .AND. &
                ( .NOT. use_nn(3)  .OR.  ABS(NINT(vr_cry(3,ir))) <= 1 )       )  THEN
                !
                nrtot_nn = nrtot_nn + 1
                !
                r_index( nrtot_nn ) = ir
                !
          ENDIF
          !
      ENDDO

      !
      ! DOS normalization costant
      ! the factor TWO is included to avoid spin doubling
      ! (internal coherence repsect to transport)
      !
      cost = ONE / ( TWO * delta )

      !
      ! Interpolate H_ij(k') at those k-points by fourier interpolation
      ! H_ij(k') ~ sum_R e^{ik'R} H_ij(R), where the sum over R is over a 
      ! finite grid (truncation)
      !
      ! add each contribution to the DOS directly, to save memeory when
      ! WF pDOS are requested.
      !
      dos  ( 1: ne ) = ZERO
      pdos ( 1: ne, 1: dimwann ) = ZERO
      ! 
      kpt_loop: &
      DO ik = 1, nkpts_int
          !
          ham(:,:) = CZERO
          !
          DO ir = 1, nrtot_nn
               !
               ir_eff = r_index( ir ) 
               !
               arg =   DOT_PRODUCT( vkpt_int(:,ik), vr(:, ir_eff ) )
               phase = CMPLX( COS(arg), SIN(arg), dbl ) * wr(ir_eff)
               ! 
               DO j = 1, dimwann
               DO i = 1, dimwann
                    ham(i,j) = ham(i,j) + phase * rham(i,j,ir_eff) 
               ENDDO
               ENDDO
               !
          ENDDO

          !
          ! Diagonalize the hamiltonian at the present k-point
          !
          CALL mat_hdiag( z, eig_int(:), ham(:,:), dimwann)

          !
          ! compute DOS and pDOS
          !
          energy_loop1: &
          DO ie = 1, ne
              !
              DO i = 1, dimwann 
                  !
                  arg = ( egrid( ie ) - eig_int( i ) ) / delta
                  aux = smearing_func( arg, smearing_type )
                  !
                  dos ( ie )    = dos(ie) + cost * wk(ik) * aux
                  !
              ENDDO
              !
          ENDDO energy_loop1

          !
          ! compute pDOS if requested
          !
          IF ( projdos ) THEN  
              !
              energy_loop2: &
              DO ie = 1, ne
                  !
                  DO i = 1, dimwann 
                      !
                      ! compute the smearing function
                      !
                      arg = ( egrid( ie ) - eig_int( i ) ) / delta
                      aux = smearing_func( arg, smearing_type )

                      !
                      ! ensure the normalization of eigenvectors in z
                      !
                      caux = CZERO
                      DO j = 1, dimwann
                           caux = caux + z(j,i) * CONJG(z(j,i))
                      ENDDO
                      z(:,i) = z(:,i) / REAL( caux, dbl)
                      !
                      DO j = 1, dimwann
                          !
                          pdos( ie, j ) = pdos( ie, j ) + cost * wk(ik) * aux * &
                                          REAL( z(j,i) * CONJG(z(j,i)) , dbl )
                      ENDDO
                      !
                  ENDDO
                  !
              ENDDO energy_loop2
              !
          ENDIF
          !       
      ENDDO kpt_loop 


! 
! ... Write final interpolated band structure to file
! 
      filename=TRIM(fileout)
      !
      OPEN( aux_unit, FILE=TRIM(filename), FORM='formatted', IOSTAT=ierr )
      IF (ierr/=0) CALL errore('dos','opening '//TRIM(filename),ABS(ierr))
         !
         WRITE( aux_unit, *) "# E (eV)   ldos(E)"
         DO ie = 1, ne
             WRITE(aux_unit, "(f9.4,1E15.4E3)") egrid(ie), dos(ie)
         ENDDO
         !
      CLOSE( aux_unit )
      !
      WRITE( stdout, "(/,2x,'Total DOS written on file:',4x,a)" ) TRIM(fileout)

      !
      ! write pDOS if the case
      !
      IF ( projdos ) THEN
          !
          DO i = 1, dimwann
              !
              WRITE( ctmp , "(i4.4)" ) i
              filename= TRIM(work_dir)//'/'//TRIM(prefix)//TRIM(postfix)// &
                        '_dos-'//ctmp//'.dat'
              !
              OPEN( aux_unit, FILE=TRIM(filename), FORM='formatted', IOSTAT=ierr )
              IF (ierr/=0) CALL errore('dos','opening '//TRIM(filename),ABS(ierr))
              !
              WRITE( aux_unit, *) "# E (eV)   ldos(E)"
              DO ie = 1, ne
                  WRITE(aux_unit, "(f9.4,1E15.4E3)") egrid(ie), pdos( ie, i)
              ENDDO
              !
              CLOSE( aux_unit )
              !
          ENDDO
          !
      ENDIF
              
!
! ... Shutdown
!

      !
      ! Clean local memory
      !
      DEALLOCATE( vkpt_int, wk, STAT=ierr)
          IF( ierr /=0 ) CALL errore('dos', 'deallocating vkpt_int, wk', ABS(ierr) )
      DEALLOCATE( eig_int, STAT=ierr)
          IF( ierr /=0 ) CALL errore('dos', 'deallocating eig_int', ABS(ierr) )
      DEALLOCATE( ham, STAT=ierr)
          IF( ierr /=0 ) CALL errore('dos', 'deallocating ham', ABS(ierr) )
      DEALLOCATE( z, STAT=ierr )
          IF( ierr /=0 ) CALL errore('dos', 'deallocating z', ABS(ierr))
      DEALLOCATE( egrid, dos, pdos, STAT=ierr )
          IF( ierr /=0 ) CALL errore('dos', 'deallocating egrid, dos, pdos', ABS(ierr))
      DEALLOCATE( vr_cry, r_index, STAT=ierr )
          IF( ierr /=0 ) CALL errore('dos', 'deallocating vr_cry, r_index', ABS(ierr) )

      !
      ! Clean global memory
      !
      CALL cleanup()

      !
      ! finalize
      !
      CALL shutdown( 'dos' )

END PROGRAM dos_prog

