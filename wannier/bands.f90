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
   PROGRAM bands
   !=====================================================
   !  
   ! Interpolates the band structure from the knowledge of
   ! the direct lattice hamiltonian on Wannier function basis
   !
   USE kinds
   USE constants,            ONLY : CZERO, TWO
   USE parameters,           ONLY : nstrx, nkpts_inx
   USE io_module,            ONLY : stdout, stdin, ioname, ham_unit, space_unit
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
   USE kpoints_module,       ONLY : nkpts, nrtot, vr, wr 
   USE windows_module,       ONLY : nbnd, imin, imax, eig, efermi, windows_read, &
                                    spin_component
   USE subspace_module,      ONLY : subspace_read
   USE hamiltonian_module,   ONLY : dimwann, rham, wan_eig, &
                                    hamiltonian_read, hamiltonian_init
   IMPLICIT NONE 

   !
   ! local variables
   !
   INTEGER :: nkpts_in     ! Number of k-points generating the line (edges)
   INTEGER :: nkpts_max    ! maximum number of interpolated points
   INTEGER :: nkpts_tot    ! actual number of point in the line
   !
   REAL(dbl)    :: arg
   COMPLEX(dbl) :: phase
   COMPLEX(dbl), ALLOCATABLE :: ham(:,:)  
   COMPLEX(dbl), ALLOCATABLE :: z(:,:)        
   !
   REAL(dbl),    ALLOCATABLE :: kpt_in(:,:), xval_in(:) 
   REAL(dbl),    ALLOCATABLE :: kpt(:,:),    xval(:) 
   REAL(dbl),    ALLOCATABLE :: eig_int(:,:)  ! interpolated band structure   
   CHARACTER(2), ALLOCATABLE :: kptname_in(:)    
   CHARACTER(nstrx)          :: filename
   INTEGER :: i, j, ik, ir
   INTEGER :: ierr
   LOGICAL :: lfound
   !
   ! input namelist
   !
   NAMELIST /INPUT/ prefix, postfix, work_dir, nkpts_in, nkpts_max, spin_component
   !
   ! end of declariations
   !   

!
!------------------------------
! main body
!------------------------------
!
      CALL startup(version_number,'bands')


!
! ... Read INPUT namelist from stdin
!
      prefix                      = 'WanT' 
      postfix                     = ' ' 
      work_dir                    = './' 
      nkpts_in                    = 0
      nkpts_max                   = 100
      spin_component              = 'none'
      
      READ(stdin, INPUT, IOSTAT=ierr)
      IF ( ierr /= 0 )  CALL errore('bands','Unable to read namelist INPUT',ABS(ierr))

      !
      ! Some checks 
      !
      IF ( nkpts_in > nkpts_inx ) CALL errore('bands', 'nkpts_in too large',  nkpts_in)
      IF ( nkpts_in <= 0 )  CALL errore('bands', 'Invalid nkpts_in', ABS(nkpts_in)+1)
      IF ( nkpts_max <= 0 ) CALL errore('bands', 'Invalid nkpts_max', ABS(nkpts_max)+1)
      CALL change_case(spin_component,'lower')
      IF ( TRIM(spin_component) /= "none" .AND. TRIM(spin_component) /= "up" .AND. &
           TRIM(spin_component) /= "down" ) &
           CALL errore('bands', 'Invalid spin_component = '//TRIM(spin_component), 3)


      !
      ! few local allocations
      !
      ALLOCATE( kptname_in( nkpts_in ), STAT=ierr )
          IF( ierr /=0 ) CALL errore('bands', 'allocating kptname_in', ABS(ierr) )
      ALLOCATE( kpt_in( 3, nkpts_in ), STAT=ierr )
          IF( ierr /=0 ) CALL errore('bands', 'allocating kpt_in', ABS(ierr) )

      !
      ! read the edge points of the required kpt-line
      ! kpts should be in CRYSTAL coordinates
      !
      DO j = 1, nkpts_in
        READ (stdin, FMT=*, IOSTAT=ierr) kptname_in(j), ( kpt_in(i,j), i=1,3 )
           IF (ierr/=0) CALL errore('bands', 'reading kpt_in', j)
      ENDDO


!
! ... Getting previous WanT data
!
      CALL want_init( WANT_INPUT=.FALSE., WINDOWS=.FALSE., BSHELLS=.FALSE. )

      !
      ! Read Subspace data
      !
      CALL ioname('space',filename)
      CALL file_open(space_unit,TRIM(filename),PATH="/",ACTION="read",FORM="formatted")
          CALL windows_read(space_unit,"WINDOWS",lfound)
          IF ( .NOT. lfound ) CALL errore('bands',"unable to find WINDOWS",1)
          CALL subspace_read(space_unit,"SUBSPACE",lfound)
          IF ( .NOT. lfound ) CALL errore('bands',"unable to find SUBSPACE",1)
      CALL file_close(space_unit,PATH="/",ACTION="read")

      CALL ioname('space',filename,LPATH=.FALSE.)
      WRITE( stdout,"(2x,'Space data read from file: ',a)") TRIM(filename)

      !
      ! Read hamiltonian data
      !
      CALL ioname('hamiltonian',filename)
      CALL file_open(ham_unit,TRIM(filename),PATH="/",ACTION="read",FORM="formatted")
          CALL hamiltonian_read(ham_unit,"HAMILTONIAN",lfound)
          IF ( .NOT. lfound ) CALL errore('bands',"unable to find HAMILTONIAN",1)
      CALL file_close(ham_unit,PATH="/",ACTION="read")

      CALL ioname('hamiltonian',filename,LPATH=.FALSE.)
      WRITE( stdout,"(2x,'Hamiltonian data read from file: ',a)") TRIM(filename)

      !
      ! Print data to output
      !
      CALL summary( stdout, LINPUT=.FALSE., LATOMS=.FALSE., LEIG=.FALSE. )



!
! ... Main task 
!
      WRITE( stdout, "(/,2x,70('='))" )
      WRITE( stdout, "(2x,'=',21x,'Band interpolation by WFs',22x,'=')" )
      WRITE( stdout, "(2x,70('='),/)" )


      !
      ! Determine the k-points used in the band structure interpolation
      !
      ALLOCATE( ham( dimwann, dimwann ), STAT=ierr )
          IF( ierr /=0 ) CALL errore('bands','allocating ham', ABS(ierr) )
      ALLOCATE( z( dimwann, dimwann ), STAT=ierr )
          IF( ierr /=0 ) CALL errore('bands', 'allocating z', ABS(ierr) )
      ALLOCATE( xval( nkpts_max ), STAT=ierr )
          IF( ierr /=0 ) CALL errore('bands', 'allocating xval', ABS(ierr) )
      ALLOCATE( xval_in( nkpts_in ), STAT=ierr )
          IF( ierr /=0 ) CALL errore('bands', 'allocating xval_in', ABS(ierr) )
      ALLOCATE( kpt(3, nkpts_max), STAT=ierr )
          IF( ierr /=0 ) CALL errore('bands', 'allocating kpt', ABS(ierr) )
 
      !
      ! convert kpts to internal cartesian representation (bohr^-1)
      CALL cry2cart( kpt_in, bvec )
      !
      CALL get_points(nkpts_in, nkpts_max, kpt_in, xval_in,  &
                      kptname_in, kpt, xval, nkpts_tot )
 
      ALLOCATE( eig_int( dimwann, nkpts_tot ), STAT=ierr )
          IF( ierr /=0 ) CALL errore('bands','allocating eig_int', ABS(ierr) )

      !
      ! Interpolate H_ij(k') at those k-points by fourier interpolation
      ! H_ij(k') ~ sum_R e^{ik'R} H_ij(R), where the sum over R is over a 
      ! finite grid (truncation)
      ! 

      DO ik = 1, nkpts_tot
           DO j = 1, dimwann
           DO i = 1, dimwann
                ham(i,j) = CZERO
                DO ir = 1, nrtot
                    arg = DOT_PRODUCT( kpt(:,ik), vr(:,ir) )
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
! ... Write final interpolated band structure to file
! 
      filename=TRIM(work_dir)//TRIM(prefix)//TRIM(postfix)//'_intband.dat'
      OPEN( ham_unit, FILE=TRIM(filename), STATUS='unknown', FORM='formatted', IOSTAT=ierr )
      IF (ierr/=0) CALL errore('bands','opening '//TRIM(filename),ABS(ierr))
      !
      DO i = 1, dimwann
          DO ik = 1, nkpts_tot
            WRITE (ham_unit, "(2e16.8)") xval(ik)/(TWO*xval(nkpts_tot)), eig_int(i,ik)
            !
            ! Note that the factor TWO appear in order to be consistent with the 
            ! x units of the other two plots (wanband and dftband) in the case where
            ! a 1D BZ is treated, dft kpts are uniform in the BZ and the interpolated
            ! kpts go from Gamma to X,Y, or Z.
            ! 
            ! This is a particular case, bu the most common in WF calculation for transport
            !
          ENDDO
          WRITE( ham_unit, "()") 
      ENDDO
      CLOSE( ham_unit )

      !
      ! as a check
      ! these eigenvalues (and also the following ones) are not aligned 
      ! to the fermi level. We impose the alignment manually...
      !
      filename=TRIM(work_dir)//TRIM(prefix)//TRIM(postfix)//'_wanband.dat'
      OPEN( ham_unit, FILE=TRIM(filename), STATUS='unknown', FORM='formatted', IOSTAT=ierr )
      IF (ierr/=0) CALL errore('bands','opening '//TRIM(filename),ABS(ierr))
      !
      DO i = 1, dimwann
          DO ik = 1, nkpts
            WRITE (ham_unit, "(2e16.8)") REAL(ik-1, dbl)/REAL(nkpts, dbl), &
                               wan_eig(i,ik) -efermi
          ENDDO
          WRITE( ham_unit, "()") 
      ENDDO
      CLOSE( ham_unit )

      filename=TRIM(work_dir)//TRIM(prefix)//TRIM(postfix)//'_dftband.dat'
      OPEN( ham_unit, FILE=TRIM(filename), STATUS='unknown', FORM='formatted', IOSTAT=ierr )
      IF (ierr/=0) CALL errore('bands','opening '//TRIM(filename),ABS(ierr))
      !
      DO i = 1, nbnd
          DO ik = 1, nkpts
             IF ( i >= imin(ik) .AND. i <= imax(ik) ) THEN
                 WRITE (ham_unit, "(2e16.8)") REAL(ik-1, dbl)/REAL(nkpts, dbl), &
                                              eig(i,ik) -efermi
             ELSE
                 WRITE (ham_unit, "()")
             ENDIF
          ENDDO
          WRITE( ham_unit, "()") 
      ENDDO
      CLOSE( ham_unit )

!
! ... Shutdown
!

      !
      ! Finalize timing
      !
      CALL timing('bands',OPR='stop')
      CALL timing_overview(stdout,LIST=global_list,MAIN_NAME='bands')

      !
      ! Clean local memory
      !
      DEALLOCATE( kptname_in, STAT=ierr)
          IF( ierr /=0 ) CALL errore('bands', 'deallocating kptname_in', ABS(ierr) )
      DEALLOCATE( kpt_in, STAT=ierr)
          IF( ierr /=0 ) CALL errore('bands', 'deallocating kpt_in', ABS(ierr) )
      DEALLOCATE( xval, STAT=ierr)
          IF( ierr /=0 ) CALL errore('bands', 'deallocating xval', ABS(ierr) )
      DEALLOCATE( xval_in, STAT=ierr)
          IF( ierr /=0 ) CALL errore('bands', 'deallocating xval_in', ABS(ierr) )
      DEALLOCATE( kpt, STAT=ierr)
          IF( ierr /=0 ) CALL errore('bands', 'deallocating kpt', ABS(ierr) )
      DEALLOCATE( ham, STAT=ierr)
          IF( ierr /=0 ) CALL errore('bands', 'deallocating ham', ABS(ierr) )
      DEALLOCATE( eig_int, STAT=ierr)
          IF( ierr /=0 ) CALL errore('bands', 'deallocating eig_int', ABS(ierr) )
      DEALLOCATE( z, STAT=ierr )
          IF( ierr /=0 ) CALL errore('bands', 'deallocating z', ABS(ierr))

      !
      ! Clean global memory
      !
      CALL cleanup()

END PROGRAM bands






