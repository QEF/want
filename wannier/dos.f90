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
   USE constants,            ONLY : CZERO, ZERO, ONE, TWO
   USE parameters,           ONLY : nstrx
   USE io_module,            ONLY : stdout, stdin, io_name, ham_unit, aux_unit, space_unit
   USE io_module,            ONLY : work_dir, prefix, postfix
   USE files_module,         ONLY : file_open, file_close
   USE timing_module,        ONLY : timing, timing_overview, global_list
   USE parser_module
   USE want_init_module,     ONLY : want_init
   USE summary_module,       ONLY : summary
   USE version_module,       ONLY : version_number
   USE util_module,          ONLY : mat_hdiag
   USE converters_module,    ONLY : cry2cart
   USE lattice_module,       ONLY : bvec
   USE kpoints_module,       ONLY : nrtot, vr, wr 
   USE windows_module,       ONLY : windows_read, nspin
   USE subspace_module,      ONLY : subspace_read
   USE smearing_module,      ONLY : smearing_func
   USE hamiltonian_module,   ONLY : dimwann, rham, hamiltonian_read, hamiltonian_init
   IMPLICIT NONE 

   !
   ! local variables
   !
   INTEGER      :: nk(3)         ! kpt generators
   INTEGER      :: s(3)          ! kpt shifts
   INTEGER      :: ne            ! dimension of the energy grid
   REAL(dbl)    :: emin          ! egrid extrema
   REAL(dbl)    :: emax
   REAL(dbl)    :: delta         ! smearing parameter
   CHARACTER(nstrx)          :: smearing_type
   !
   INTEGER      :: nkpts_int     ! Number of interpolated k-points
   REAL(dbl)    :: arg, cost
   COMPLEX(dbl) :: phase
   COMPLEX(dbl), ALLOCATABLE :: ham(:,:)  
   COMPLEX(dbl), ALLOCATABLE :: z(:,:)        
   !
   REAL(dbl),    ALLOCATABLE :: egrid(:), dos(:)
   REAL(dbl),    ALLOCATABLE :: vkpt_int(:,:), wk(:)
   REAL(dbl),    ALLOCATABLE :: eig_int(:,:)  ! interpolated band structure   
   CHARACTER(nstrx)          :: filename
   INTEGER :: i, j, ie, ik, ir
   INTEGER :: ierr
   LOGICAL :: lfound
   !
   ! input namelist
   !
   NAMELIST /INPUT/ prefix, postfix, work_dir, nk, s, delta, smearing_type, emin, emax, ne
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
      delta                       = 0.1    ! eV
      nk(:)                       = -1
      s(:)                        =  0
      emin                        = -10.0
      emax                        =  10.0
      ne                          =  1000
      smearing_type               = 'gaussian'
      
      READ(stdin, INPUT, IOSTAT=ierr)
      IF ( ierr /= 0 )  CALL errore('dos','Unable to read namelist INPUT',ABS(ierr))

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
      CALL want_init( WANT_INPUT=.FALSE., WINDOWS=.FALSE., BSHELLS=.FALSE. )

      !
      ! Read Subspace data
      !
      CALL io_name('space',filename)
      CALL file_open(space_unit,TRIM(filename),PATH="/",ACTION="read")
          CALL windows_read(space_unit,"WINDOWS",lfound)
          IF ( .NOT. lfound ) CALL errore('dos',"unable to find WINDOWS",1)
          CALL subspace_read(space_unit,"SUBSPACE",lfound)
          IF ( .NOT. lfound ) CALL errore('dos',"unable to find SUBSPACE",1)
      CALL file_close(space_unit,PATH="/",ACTION="read")

      CALL io_name('space',filename,LPATH=.FALSE.)
      WRITE( stdout,"(2x,'Space data read from file: ',a)") TRIM(filename)

      !
      ! Read hamiltonian data
      !
      CALL io_name('hamiltonian',filename)
      CALL file_open(ham_unit,TRIM(filename),PATH="/",ACTION="read")
          CALL hamiltonian_read(ham_unit,"HAMILTONIAN",lfound)
          IF ( .NOT. lfound ) CALL errore('dos',"unable to find HAMILTONIAN",1)
      CALL file_close(ham_unit,PATH="/",ACTION="read")

      CALL io_name('hamiltonian',filename,LPATH=.FALSE.)
      WRITE( stdout,"(2x,'Hamiltonian data read from file: ',a)") TRIM(filename)

      !
      ! Print data to output
      !
      CALL summary( stdout, LINPUT=.FALSE., LATOMS=.FALSE., LEIG=.FALSE. )

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

      !
      ! setting egrid
      !
      DO ie = 1, ne
          egrid (ie) = emin + REAL(ie-1, dbl) * (emax-emin) / REAL( ne -1, dbl)
      ENDDO

      !
      ! generate monkhorst-pack grid, using nk(:) and s(:)
      ! kpts are in crystal coords
      !
      CALL monkpack( nk, s, vkpt_int )
      !
      wk(1:nkpts_int) = TWO / REAL( nspin * nkpts_int ,dbl)
      !
      ! convert kpts to internal cartesian representation (bohr^-1)
      CALL cry2cart( vkpt_int, bvec )


      !
      ! short summary
      !
      WRITE( stdout, "(/2x, 'broadening parameters: ',/)" )
      WRITE( stdout, "( 5x,    'delta : ',f9.5, ' eV')" ) delta
      WRITE( stdout, "( 5x,    ' type : ',2x,a)" ) TRIM(smearing_type)
      !
      WRITE( stdout, "(/2x, 'kpt mesh, interpolation on: ',/)" )
      WRITE( stdout, "( 5x,    '   nk : ',3i4)" ) nk(:)
      WRITE( stdout, "( 5x,    '    s : ',3i4)" ) s(:)
      WRITE( stdout, "( 5x,    'nktot : ',i6)" ) nkpts_int
      !
      WRITE( stdout, "(2/2x, 'Generated kpt mesh: (cart. coord. in Bohr^-1) ',/)" )
      !
      DO ik=1,nkpts_int
           WRITE( stdout, " (4x, 'k point', i4, ':   ( ',3f9.5,' ),   weight = ', f11.7 )") &
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
      ALLOCATE( eig_int( dimwann, nkpts_int ), STAT=ierr )
          IF( ierr /=0 ) CALL errore('dos','allocating eig_int', ABS(ierr) )

      !
      ! Interpolate H_ij(k') at those k-points by fourier interpolation
      ! H_ij(k') ~ sum_R e^{ik'R} H_ij(R), where the sum over R is over a 
      ! finite grid (truncation)
      ! 

      DO ik = 1, nkpts_int
           DO j = 1, dimwann
           DO i = 1, dimwann
                ham(i,j) = CZERO
                DO ir = 1, nrtot
                    arg = DOT_PRODUCT( vkpt_int(:,ik), vr(:,ir) )
                    phase = CMPLX( COS(arg), SIN(arg), dbl ) * wr(ir)
                    ham(i,j) = ham(i,j) + phase * rham(i,j,ir) 
                ENDDO
           ENDDO
           ENDDO
 
           !
           ! Diagonalize the hamiltonian at the present k-point
           !
           CALL mat_hdiag( z, eig_int(:,ik), ham(:,:), dimwann)
      ENDDO 


      !
      ! compute DOS
      !
      cost = ONE / delta

      energy_loop:&
      DO ie = 1, ne
            !
            dos ( ie ) = ZERO 
            !
            DO ik=1, nkpts_int
            DO i =1, dimwann 
                 !
                 arg = ( egrid(ie) - eig_int(i,ik) ) / delta
                 dos (ie) = dos(ie) + cost * wk(ik) * smearing_func( arg, smearing_type )
                 !
            ENDDO
            ENDDO
            !
      ENDDO energy_loop
       


! 
! ... Write final interpolated band structure to file
! 
      filename=TRIM(work_dir)//TRIM(prefix)//TRIM(postfix)//'_dos.dat'
      !
      OPEN( aux_unit, FILE=TRIM(filename), STATUS='unknown', FORM='formatted', IOSTAT=ierr )
      IF (ierr/=0) CALL errore('dos','opening '//TRIM(filename),ABS(ierr))
      !
      WRITE( aux_unit, *) "# E (eV)   ldos(E)"
      DO ie = 1, ne
          WRITE(aux_unit, "(f9.4,1E15.4E3)") egrid(ie), dos(ie)
      ENDDO
      !
      CLOSE( ham_unit )

!
! ... Shutdown
!

      !
      ! Finalize timing
      !
      CALL timing('dos',OPR='stop')
      CALL timing_overview(stdout,LIST=global_list,MAIN_NAME='dos')

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
      DEALLOCATE( egrid, dos, STAT=ierr )
          IF( ierr /=0 ) CALL errore('dos', 'deallocating egrid, dos', ABS(ierr))

      !
      ! Clean global memory
      !
      CALL cleanup()

END PROGRAM dos_prog






