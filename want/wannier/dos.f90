! 
! Copyright (C) 2006 WanT Group
!
! This file is distributed under the terms of the
! GNU General Public License. See the file `License'
! in the root directory of the present distribution,
! or http://www.gnu.org/copyleft/gpl.txt .
!
!=====================================================
   PROGRAM dos_main
   !=====================================================
   !  
   ! Interpolates the electronic structure from the knowledge of
   ! the direct lattice hamiltonian on Wannier function basis
   ! and compute the Density of states (DOS)
   !
   USE kinds
   USE parameters,           ONLY : nstrx
   USE constants,            ONLY : CZERO, ZERO, ONE, CI, TWO, PI, TPI, EPS_m4, EPS_m6
   USE io_module,            ONLY : stdout, stdin, io_name, ham_unit, aux_unit, sgm_unit, space_unit
   USE io_module,            ONLY : work_dir, prefix, postfix
   USE files_module,         ONLY : file_open, file_close
   USE version_module,       ONLY : version_number
   USE util_module,          ONLY : mat_hdiag, zmat_herm
   USE converters_module,    ONLY : cry2cart, cart2cry
   USE lattice_module,       ONLY : avec, bvec
   USE kpoints_module,       ONLY : nrtot, vr, wr 
   USE windows_module,       ONLY : windows_read, nspin
   USE subspace_module,      ONLY : subspace_read
   USE smearing_module,      ONLY : smearing_func
   USE hamiltonian_module,   ONLY : dimwann, rham, hamiltonian_read, hamiltonian_init
   USE operator_module
   USE parser_module
   USE want_interfaces_module
   !
   IMPLICIT NONE 

   !
   ! input variables
   !
   INTEGER          :: nk(3)         ! kpt generators
   INTEGER          :: s(3)          ! kpt shifts
   INTEGER          :: ne            ! dimension of the energy grid
   INTEGER          :: ne_sgm        ! dimension of the energy grid from sgm file
   REAL(dbl)        :: emin          ! egrid extrema
   REAL(dbl)        :: emax
   REAL(dbl)        :: delta         ! smearing parameter
   CHARACTER(nstrx) :: smearing_type
   CHARACTER(nstrx) :: fileout       ! output filename
   CHARACTER(nstrx) :: datafile_sgm  ! self-energy file
   LOGICAL          :: projdos       ! whether to write WF projected DOS
   LOGICAL          :: use_nn(3)     ! use nearest neighb. in direction i, i=1,3
                                     ! DEBUG and transport purposes
   !
   ! loval variables
   !
   INTEGER      :: nkpts_int     ! Number of interpolated k-points
   INTEGER      :: nrtot_nn
   REAL(dbl)    :: arg, cost, raux
   COMPLEX(dbl) :: caux, ze
   !
   INTEGER,      ALLOCATABLE :: r_index(:)
   COMPLEX(dbl), ALLOCATABLE :: kham(:,:), rham_nn(:,:,:), k_sgm(:,:), r_sgm(:,:,:)  
   COMPLEX(dbl), ALLOCATABLE :: z(:,:,:)        
   COMPLEX(dbl), ALLOCATABLE :: GF(:,:), GF0(:,:)
   REAL(dbl),    ALLOCATABLE :: egrid(:)
   REAL(dbl),    ALLOCATABLE :: dos(:), dos0(:), pdos(:,:)
   REAL(dbl),    ALLOCATABLE :: vkpt_int(:,:), wk(:)
   REAL(dbl),    ALLOCATABLE :: eig_int(:,:)
   REAL(dbl),    ALLOCATABLE :: vr_cry(:,:), vr_nn(:,:), wr_nn(:), vr_sgm(:,:)
   CHARACTER(nstrx)          :: filename, analyticity_sgm
   CHARACTER(4)              :: ctmp
   !
   INTEGER      :: i, j, ie, ik, ir
   INTEGER      :: ierr
   INTEGER      :: dimwann_sgm, nrtot_sgm
   LOGICAL      :: lfound, lhave_sgm, ldynam_sgm

   !
   ! input namelist
   !
   NAMELIST /INPUT/ prefix, postfix, work_dir, nk, s, delta, smearing_type, fileout, &
                    emin, emax, ne, use_nn, projdos, datafile_sgm
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
      datafile_sgm                = ' '
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
      lhave_sgm = .FALSE.
      IF ( LEN_TRIM(datafile_sgm) > 0 ) lhave_sgm = .TRUE.
      !
      nkpts_int = PRODUCT( nk(1:3) )
      IF ( nkpts_int <=0 ) CALL errore( 'dos', 'unexpected nkpts_int ', -nkpts_int + 1)
      !
      ! the check of SMEARING_TYPE is done inside the function smearing_func
      ! just move to lower_case
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
      WRITE( stdout,"(2x,'Hamiltonian data read from file: ',a,/)") TRIM(filename)


      !
      ! input summary
      !
      CALL write_header( stdout, "INPUT Summary" )
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
      WRITE( stdout, "(   7x,'       compute projdos :',5x,   a)") TRIM( log2char( projdos ) )
      WRITE( stdout, "(   7x,'            have sigma :',5x, a  )") TRIM( log2char(lhave_sgm) )
      IF ( lhave_sgm ) THEN
          WRITE( stdout, "(   7x,'        sigma datafile :',5x,   a)") TRIM( datafile_sgm )
      ENDIF
      
      !
      ! Print data to output
      !
      CALL summary( stdout, INPUT=.FALSE., IONS=.FALSE., KPOINTS=.FALSE., WINDOWS=.FALSE. )


      !
      ! if required, get data from sgm datafile
      !
      ldynam_sgm = .FALSE.
      !
      IF ( lhave_sgm ) THEN
          !
          CALL file_open(sgm_unit, TRIM(datafile_sgm), PATH="/", ACTION="read")
          !
          CALL operator_read_aux( sgm_unit, DIMWANN=dimwann_sgm, NR=nrtot_sgm,        &
                                            DYNAMICAL=ldynam_sgm, NOMEGA=ne_sgm,      &
                                            ANALYTICITY=analyticity_sgm, IERR=ierr )
                                            !
          IF ( ierr/=0 ) CALL errore('dos','reading DIMWANN--ANALYTICITY', ABS(ierr) )

          !
          ! few checks
          !
          CALL change_case( analyticity_sgm, 'lower' )
          IF ( TRIM(analyticity_sgm) /= 'retarded' .AND. ldynam_sgm )  &
                       CALL errore('dos','invalid analyticity = '//TRIM(analyticity_sgm),1)

          IF ( dimwann_sgm /= dimwann ) CALL errore('dos','invalid dimwann_sgm',1)
          IF ( nrtot_sgm /= nrtot )     CALL errore('dos','invalid nrtot_sgm',1)
          ! 
          ALLOCATE( vr_sgm( 3, nrtot ), STAT=ierr )
          IF ( ierr /=0 ) CALL errore('dos','allocating vr_sgm', ABS(ierr) ) 
          !
          ! 
          IF ( ldynam_sgm ) THEN 
             !
             ne = ne_sgm
             !
             ALLOCATE( egrid( ne ), STAT=ierr )
             IF ( ierr /=0 ) CALL errore('dos','allocating egrid', ABS(ierr) ) 
             !
             CALL operator_read_aux( sgm_unit, VR=vr_sgm, GRID=egrid, IERR=ierr )
             IF ( ierr/=0 ) CALL errore('dos','reading VR, GRID', ABS(ierr) )
             !
          ELSE
             !
             CALL operator_read_aux( sgm_unit, VR=vr_sgm, IERR=ierr )
             IF ( ierr/=0 ) CALL errore('dos','reading VR', ABS(ierr) )
             !
          ENDIF
 
          ! 
          ! here we check that the order of R vectors from file is 
          ! the same of VR
          !    
          DO ir = 1, nrtot
             !
             raux = DOT_PRODUCT( vr(:,ir)-vr_sgm(:,ir), vr(:,ir)-vr_sgm(:,ir) ) 
             IF ( raux > EPS_m6 ) CALL errore('dos','invalid R vectors from sgm file',ir)
             !
          ENDDO
          !
          !
          DEALLOCATE( vr_sgm, STAT=ierr)
          IF ( ierr/=0 ) CALL errore('dos','deallocating vr_sgm',ABS(ierr))

          !
          ! now allocate global quantities
          !
          ALLOCATE( r_sgm(dimwann, dimwann, nrtot), STAT=ierr )
          IF ( ierr/=0 ) CALL errore('dos','allocating r_sgm',ABS(ierr))
          ALLOCATE( k_sgm(dimwann, dimwann), STAT=ierr )
          IF ( ierr/=0 ) CALL errore('dos','allocating k_sgm',ABS(ierr))
          !
      ENDIF

      !
      ! furhter checks on non-implemented special cases
      !
      IF ( ldynam_sgm .AND. projdos )   CALL errore('dos','projdos and sigma NOT impl.', 10)
      IF ( ldynam_sgm .AND. ANY( use_nn )  ) &
           CALL errore('dos','nearest enighb. NOT impl. with sgm', 10)

!
! ... Main task 
!
      CALL write_header( stdout, "DOS computation using Wannier Functions" )
      CALL flush_unit( stdout )

      !
      ! few local allocations
      !
      ALLOCATE( vkpt_int( 3, nkpts_int ), wk( nkpts_int ), STAT=ierr )
      IF( ierr /=0 ) CALL errore('dos', 'allocating vkpt_int, wk', ABS(ierr) )
      !
      ALLOCATE( dos( ne ), dos0( ne ), STAT=ierr )
      IF( ierr /=0 ) CALL errore('dos', 'allocating dos, dos0', ABS(ierr) )
      !
      ALLOCATE( pdos( ne, dimwann ), STAT=ierr )
      IF( ierr /=0 ) CALL errore('dos', 'allocating pdos', ABS(ierr) )

      !
      ! setting egrid
      !
      IF ( ldynam_sgm ) THEN
         !
         CALL warning( stdout , "energy grid is forced from SGM datafile" )
         WRITE( stdout, "()")
         !
         emin = egrid( 1 )
         emax = egrid( ne )
         !
      ELSE
         !
         ALLOCATE( egrid( ne ), STAT=ierr )
         IF( ierr /=0 ) CALL errore('dos', 'allocating egrid', ABS(ierr) )
         !
         DO ie = 1, ne
            egrid (ie) = emin + REAL(ie-1, dbl) * (emax-emin) / REAL( ne -1, dbl)
         ENDDO
         !
      ENDIF

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
      ALLOCATE( kham( dimwann, dimwann ), STAT=ierr )
      IF( ierr /=0 ) CALL errore('dos','allocating kham', ABS(ierr) )
      !
      ALLOCATE( z( dimwann, dimwann, nkpts_int ), STAT=ierr )
      IF( ierr /=0 ) CALL errore('dos', 'allocating z', ABS(ierr) )
      !
      ALLOCATE( eig_int( dimwann, nkpts_int ), STAT=ierr )
      IF( ierr /=0 ) CALL errore('dos','allocating eig_int', ABS(ierr) )

      !
      ! if chosen from input, select only R corresponding to nearest-neighb
      ! along some (crystal) directions
      ! nn_index will point only to the selected nrtot_nn vectors
      !
      ALLOCATE( vr_cry(3, nrtot), STAT=ierr )
      IF( ierr /=0 ) CALL errore('dos','allocating vr_cry', ABS(ierr) )
      !
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
      !
      ALLOCATE( vr_nn( 3, nrtot_nn ), wr_nn( nrtot_nn ), STAT=ierr )
      IF( ierr /=0 ) CALL errore('dos','allocating vr_nn, wr_nn', ABS(ierr) )
      !
      ALLOCATE( rham_nn( dimwann,dimwann, nrtot_nn ), STAT=ierr )
      IF( ierr /=0 ) CALL errore('dos','allocating rham_nn', ABS(ierr) )
      !
      DO ir = 1, nrtot_nn
          !
          vr_nn(:, ir )  = vr ( :, r_index(ir) )
          wr_nn( ir )    = wr( r_index(ir) )
          ! 
          rham_nn( :, :, ir ) = rham( :, :, r_index(ir) ) 
          !
      ENDDO
      !
      DEALLOCATE( vr_cry, r_index, STAT=ierr )
      IF( ierr /=0 ) CALL errore('dos', 'deallocating vr_cry, r_index', ABS(ierr) )


      !
      ! Interpolate H_ij(k') at those k-points by fourier interpolation
      ! H_ij(k') ~ sum_R e^{ik'R} H_ij(R), where the sum over R is over a 
      ! finite grid (truncation)
      !
      IF ( .NOT. lhave_sgm  .OR. .NOT. ldynam_sgm ) THEN

          !
          ! read static self-energy if the case
          !
          IF ( lhave_sgm ) THEN
             !
             CALL operator_read_data( sgm_unit, R_OPR=r_sgm, IERR=ierr )
             IF ( ierr/=0 ) CALL errore('dos','reading static r_sgm', 11)
             !
          ENDIF

          kpt_loop: &
          DO ik = 1, nkpts_int

              !
              ! compute the Hamiltonian with the correct bloch symmetry
              !
              CALL compute_kham( dimwann, nrtot_nn, vr_nn, wr_nn, rham_nn,  &
                                 vkpt_int(:,ik), kham)

              IF ( lhave_sgm ) THEN
                  !
                  CALL compute_kham( dimwann, nrtot_nn, vr_nn, wr_nn, r_sgm,  &
                                     vkpt_int(:,ik), k_sgm)
                  !
                  ! symmetryze the static sgm in order to make it hermitean
                  !
                  CALL zmat_herm( k_sgm, dimwann )
                  ! 
                  ! 
                  kham(:,:) = kham(:,:) + k_sgm(:,:)
                  !
              ENDIF

              !
              ! Diagonalize the hamiltonian at the present k-point
              !
              CALL mat_hdiag( z(:,:,ik), eig_int(:,ik), kham(:,:), dimwann)
              !
          ENDDO kpt_loop
          !
      ENDIF

      !
      ! DOS normalization costant
      ! the factor TWO is included to avoid spin doubling
      ! (internal coherence repsect to transport)
      !
      !
      IF ( lhave_sgm .AND. ldynam_sgm ) THEN
          !
          cost = ONE / ( TWO * PI )
          !
          ALLOCATE( GF0( dimwann, dimwann ), STAT=ierr )
          IF( ierr /=0 ) CALL errore('dos','allocating GF0', ABS(ierr) )
          !
          ALLOCATE( GF( dimwann, dimwann ), STAT=ierr )
          IF( ierr /=0 ) CALL errore('dos','allocating GF', ABS(ierr) )
          !
      ELSE
          cost = ONE / ( TWO * delta )
      ENDIF
     
      !
      ! compute DOS and pDOS
      !
      energy_loop1: &
      DO ie = 1, ne
          !
          dos  ( ie ) = ZERO
          !
          IF ( lhave_sgm .AND. ldynam_sgm ) THEN

             !
             ! include external self-energy in the calc
             !
             CALL operator_read_data( sgm_unit, IE=ie, R_OPR=r_sgm, IERR=ierr )
             IF ( ierr/=0 ) CALL errore('dos','reading r_sgm', ie)
             !
             DO ik = 1, nkpts_int
                !
                ! interpolate r_sgm on the required kpt
                !
                CALL compute_kham( dimwann, nrtot_nn, vr_nn, wr_nn, rham_nn,  &
                                   vkpt_int(:,ik), kham )
                CALL compute_kham( dimwann, nrtot_nn, vr_nn, wr_nn, r_sgm,  &
                                   vkpt_int(:,ik), k_sgm )
                !
                ! solve dyson equation to obtain the interacting Green function
                ! NOTE: here the smearing of GF0 (non-interacting) is still
                !       lorentzian whatever value from input
                !
                ze = egrid( ie ) + CI * delta
                !
                CALL dyson_solver( ze, dimwann, kham(:,:), k_sgm(:,:), GF0, GF )
                !
                DO i  = 1, dimwann 
                   !
                   dos0( ie )    = dos0( ie ) - cost * wk(ik) * AIMAG( GF0(i,i) )
                   dos(  ie )    = dos(  ie ) - cost * wk(ik) * AIMAG( GF(i,i)  )
                   !
                ENDDO
             ENDDO
             !
          ELSE
             !
             ! standard DOS calculation (no sgm or static sgm)
             !
             DO ik = 1, nkpts_int
             DO i  = 1, dimwann 
                !
                arg  = ( egrid( ie ) - eig_int( i, ik ) ) / delta
                raux = smearing_func( arg, smearing_type )
                !
                dos ( ie )    = dos(ie) + cost * wk(ik) * raux
                !
             ENDDO
             ENDDO
             !
          ENDIF
          !
      ENDDO energy_loop1
      !
      !
      IF ( lhave_sgm ) CALL file_close(sgm_unit, PATH="/", ACTION="read" )


      !
      ! compute pDOS if requested
      !
      IF ( projdos ) THEN  
          !
          pdos ( 1: ne, 1: dimwann ) = ZERO
          ! 
          energy_loop2: &
          DO ie = 1, ne
              !
              DO ik = 1, nkpts_int
              DO i  = 1, dimwann 
                  !
                  ! compute the smearing function
                  !
                  arg  = ( egrid( ie ) - eig_int( i, ik ) ) / delta
                  raux = smearing_func( arg, smearing_type )

                  !
                  ! ensure the normalization of eigenvectors in z
                  !
                  caux = CZERO
                  DO j = 1, dimwann
                       caux = caux + z(j, i, ik) * CONJG( z( j, i, ik) )
                  ENDDO
                  z( :, i, ik) = z( :, i, ik) / REAL( caux, dbl)
                  !
                  DO j = 1, dimwann
                      !
                      pdos( ie, j ) = pdos( ie, j ) + cost * wk(ik) * raux * &
                                      REAL( z( j, i, ik) * CONJG( z( j, i, ik)) , dbl )
                  ENDDO
                  !
              ENDDO
              ENDDO
              !
          ENDDO energy_loop2
          !
      ENDIF


! 
! ... Write final interpolated band structure to file
! 
      filename=TRIM(fileout)
      !
      OPEN( aux_unit, FILE=TRIM(filename), FORM='formatted', IOSTAT=ierr )
      IF (ierr/=0) CALL errore('dos','opening '//TRIM(filename),ABS(ierr))
         !
         IF ( lhave_sgm ) THEN
            !
            WRITE( aux_unit, *) "# E (eV)   ldos(E)    ldos0(E)"
            DO ie = 1, ne
                WRITE(aux_unit, "(f9.4,2E15.4E3)") egrid(ie), dos(ie), dos0(ie)
            ENDDO
            !
         ELSE
            !
            WRITE( aux_unit, *) "# E (eV)   ldos(E)"
            DO ie = 1, ne
                WRITE(aux_unit, "(f9.4,1E15.4E3)") egrid(ie), dos(ie)
            ENDDO
            !
         ENDIF
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
      !
      DEALLOCATE( eig_int, STAT=ierr)
      IF( ierr /=0 ) CALL errore('dos', 'deallocating eig_int', ABS(ierr) )
      !
      DEALLOCATE( z, STAT=ierr )
      IF( ierr /=0 ) CALL errore('dos', 'deallocating z', ABS(ierr))
      !
      DEALLOCATE( egrid, dos, dos0, pdos, STAT=ierr )
      IF( ierr /=0 ) CALL errore('dos', 'deallocating egrid, dos, pdos', ABS(ierr))
      !
      DEALLOCATE( vr_nn, wr_nn, STAT=ierr )
      IF( ierr /=0 ) CALL errore('dos', 'deallocating vr_nn, wr_nn', ABS(ierr) )
      !
      DEALLOCATE( kham, rham_nn, STAT=ierr)
      IF( ierr /=0 ) CALL errore('dos', 'deallocating kham, rham_nn', ABS(ierr) )
      !
      IF ( lhave_sgm ) THEN
         !
         DEALLOCATE( r_sgm, STAT=ierr )
         IF ( ierr/=0 ) CALL errore('dos','deallocating r_sgm',ABS(ierr))
         DEALLOCATE( k_sgm, STAT=ierr )
         IF ( ierr/=0 ) CALL errore('dos','deallocating k_sgm',ABS(ierr))
         !
      ENDIF

      !
      ! Clean global memory
      !
      CALL cleanup()

      !
      ! finalize
      !
      CALL shutdown( 'dos' )

END PROGRAM dos_main

