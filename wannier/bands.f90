! 
! Copyright (C) 2004 WanT Group
!
! This file is distributed under the terms of the
! GNU General Public License. See the file `License'
! in the root directory of the present distribution,
! or http://www.gnu.org/copyleft/gpl.txt .
!
! Based on a previous version by Nicola Marzari and David Vanderbilt
! See the CREDITS file in the ~want directory for a full description
!
!=====================================================
   PROGRAM bands
   !=====================================================
   !  
   ! Interpolates the band structure from the knowledge of
   ! the direct lattice hamiltonian on Wannier function basis
   !
   USE version_module,       ONLY : version_number
   USE parameters,           ONLY : nstrx, nkpts_inx
   USE io_module,            ONLY : stdout, stdin
   USE io_module,            ONLY : work_dir, prefix, postfix
   USE io_module,            ONLY : datafile_dft => dftdata_file, datafile_sgm
   USE datafiles_module,     ONLY : datafiles_init
   USE timing_module,        ONLY : timing
   USE log_module,           ONLY : log_push, log_pop
   USE correlation_module,   ONLY : lhave_sgm
   USE want_interfaces_module
   USE parser_module

   !
   IMPLICIT NONE 

   !
   ! input variables
   !
   INTEGER            :: nkpts_in       ! Number of k-points generating the line (edges)
   INTEGER            :: nkpts_max      ! maximum number of interpolated points
   INTEGER            :: ircut(3)       ! real space curoff in terms of unit cells
                                        ! for directions i=1,2,3  (0 means no cutoff)
   CHARACTER(nstrx)   :: fileout        ! output filename

   !
   ! input namelist
   !
   NAMELIST /INPUT/ prefix, postfix, work_dir, datafile_dft, datafile_sgm, &
                    fileout, nkpts_in, nkpts_max, ircut
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
      ! read input
      !
      CALL bands_input( )

      !
      ! init post processing (reading previous WanT and DFT data )
      !
      CALL write_header( stdout, "Post Processing Init" )
      !
      CALL datafiles_init()
      !
      CALL postproc_init ()

      !
      ! print data to output
      !
      CALL summary( stdout, INPUT=.FALSE., IONS=.FALSE., WINDOWS=.FALSE. )

      !
      ! do the main task
      !
      CALL do_bands( fileout, nkpts_in, nkpts_max, ircut ) 

      !
      ! Clean global memory
      !
      CALL cleanup()

      !
      ! finalize
      !
      CALL shutdown( 'bands' )

CONTAINS

!********************************************************
   SUBROUTINE bands_input()
   !********************************************************
   !
   ! Read INPUT namelist from stdin
   !
   USE io_module,            ONLY : ionode, ionode_id
   USE mp,                   ONLY : mp_bcast
   IMPLICIT NONE

      CHARACTER(11)    :: subname = 'bands_input'
      INTEGER          :: ierr
      !
      ! end of declarations
      !

      CALL timing( subname, OPR='start' )
      CALL log_push( subname )

      !   
      ! init input namelist   
      !   
      prefix                      = 'WanT' 
      postfix                     = ' ' 
      work_dir                    = './' 
      datafile_sgm                = ' '
      datafile_dft                = ' '
      fileout                     = ' '
      nkpts_in                    = 0
      nkpts_max                   = 100
      ircut(1:3)                  = 0
      
      CALL input_from_file ( stdin )
      !
      READ(stdin, INPUT, IOSTAT=ierr)
      IF ( ierr /= 0 )  CALL errore(subname,'Unable to read namelist INPUT',ABS(ierr))

      IF ( LEN_TRIM(fileout) == 0 ) &
           fileout = TRIM(work_dir)//'/'//TRIM(prefix)//TRIM(postfix)//'_bands.dat'

      lhave_sgm = .FALSE.
      IF ( LEN_TRIM(datafile_sgm) > 0 ) lhave_sgm = .TRUE.

      !
      ! Some checks 
      !
      IF ( nkpts_in > nkpts_inx ) CALL errore(subname, 'nkpts_in too large',  nkpts_in)
      IF ( nkpts_in <= 0 )        CALL errore(subname, 'Invalid nkpts_in', ABS(nkpts_in)+1)
      IF ( nkpts_max <= 0 )       CALL errore(subname, 'Invalid nkpts_max', ABS(nkpts_max)+1)
      IF ( ANY( ircut(:) < 0 ) )  CALL errore(subname,'Invalid ircut', 10)


      !
      ! input summary
      !
      CALL write_header( stdout, "INPUT Summary" )
      !
      WRITE( stdout, "(   7x,'              work dir :',5x,   a)") TRIM(work_dir)
      WRITE( stdout, "(   7x,'                prefix :',5x,   a)") TRIM(prefix)
      WRITE( stdout, "(   7x,'               postfix :',5x,   a)") TRIM(postfix)
      IF ( LEN_TRIM( datafile_dft ) /= 0 ) &
         WRITE( stdout, "(7x,'          datafile_dft :',5x,   a)") TRIM(datafile_dft)
      WRITE( stdout, "(   7x,'               fileout :',5x,   a)") TRIM(fileout)
      WRITE( stdout, "(   7x,'             nkpts_in  :',3x,3i4 )") nkpts_in
      WRITE( stdout, "(   7x,'             nkpts_max :',3x,3i4 )") nkpts_max
      !
      IF ( ANY( ircut(:) > 0 ) ) THEN
          WRITE( stdout, "(   7x,'                 ircut :',3x,3i4)") ircut(:)
      ENDIF
      !
      IF ( LEN_TRIM( datafile_dft ) /=0 ) THEN
          WRITE( stdout,"(7x,'          DFT datafile :',5x,   a)") TRIM( datafile_dft )
      ENDIF
      !
      WRITE( stdout, "(   7x,'            have sigma :',5x, a  )") TRIM( log2char(lhave_sgm) )
      IF ( lhave_sgm ) THEN
          WRITE( stdout,"(7x,'        sigma datafile :',5x,   a)") TRIM( datafile_sgm )
      ENDIF
      !
      WRITE( stdout, "()" )

      CALL timing(subname,OPR='stop')
      CALL log_pop(subname)
      !
   END SUBROUTINE bands_input

END PROGRAM bands


!********************************************************
   SUBROUTINE do_bands( fileout, nkpts_in, nkpts_max, ircut )
   !********************************************************
   !
   ! perform the main task of the calculation
   !
   USE kinds
   USE constants,            ONLY : CZERO, TWO, EPS_m6
   USE parameters,           ONLY : nstrx, nkpts_inx
   USE io_module,            ONLY : stdout, stdin, io_name, ham_unit, sgm_unit
   USE io_module,            ONLY : work_dir, prefix, postfix
   USE io_module,            ONLY : datafile_dft => dftdata_file, datafile_sgm
   USE io_module,            ONLY : ionode, ionode_id
   USE mp,                   ONLY : mp_bcast
   USE files_module,         ONLY : file_open, file_close
   USE util_module,          ONLY : mat_hdiag, zmat_herm
   USE converters_module,    ONLY : cry2cart, cart2cry
   USE lattice_module,       ONLY : avec, bvec
   USE kpoints_module,       ONLY : nkpts, nrtot, vr, wr 
   USE windows_module,       ONLY : nbnd, imin, imax, eig, efermi
   USE hamiltonian_module,   ONLY : dimwann, rham, rovp, lhave_overlap, wan_eig
   USE correlation_module,   ONLY : lhave_sgm, ldynam_sgm, rsgm, correlation_allocate
   USE timing_module,        ONLY : timing
   USE log_module,           ONLY : log_push, log_pop
   USE operator_module
   !
   IMPLICIT NONE

      !
      ! input vars
      !
      CHARACTER(*),  INTENT(IN) :: fileout
      INTEGER,       INTENT(IN) :: nkpts_in, nkpts_max
      INTEGER,       INTENT(IN) :: ircut(3)
   
      !
      ! local vars
      !
      CHARACTER(8)      :: subname = 'do_bands'

      INTEGER           :: nkpts_tot            ! actual number of point in the line
      INTEGER           :: nrtot_sgm, dimwann_sgm
      INTEGER           :: nrtot_nn
      LOGICAL           :: lhave_nn(3)
      REAL(dbl)         :: raux
      !
      INTEGER,      ALLOCATABLE :: r_index(:)
      COMPLEX(dbl), ALLOCATABLE :: kham(:,:), rham_nn(:,:,:), z(:,:)
      COMPLEX(dbl), ALLOCATABLE :: kovp(:,:), rovp_nn(:,:,:)
      COMPLEX(dbl), ALLOCATABLE :: ksgm(:,:), rsgm_nn(:,:,:)
      !
      REAL(dbl),    ALLOCATABLE :: kpt_in(:,:), xval_in(:) 
      REAL(dbl),    ALLOCATABLE :: vkpt_int(:,:), xval(:)
      REAL(dbl),    ALLOCATABLE :: vr_cry(:,:), vr_nn(:,:), wr_nn(:), vr_sgm(:,:) 
      REAL(dbl),    ALLOCATABLE :: eig_int(:,:)
      CHARACTER(2), ALLOCATABLE :: kptname_in(:)    
      !
      CHARACTER(nstrx)  :: filename
      !
      INTEGER           :: i, j, ik, ir
      INTEGER           :: ierr
      LOGICAL           :: lfound
      !
      ! end of declarations
      !

!
!------------------------------
! main body 
!------------------------------
!

      CALL timing(subname,OPR='start')
      CALL log_push(subname)


      !  
      ! complete the read in of data from input 
      !   
      ALLOCATE( kptname_in( nkpts_in ), STAT=ierr )
      IF( ierr /=0 ) CALL errore(subname, 'allocating kptname_in', ABS(ierr) )
      !
      ALLOCATE( kpt_in( 3, nkpts_in ), STAT=ierr )
      IF( ierr /=0 ) CALL errore(subname, 'allocating kpt_in', ABS(ierr) )

      !
      ! read the edge points of the required kpt-line
      ! kpts should be in CRYSTAL coordinates
      !
      DO j = 1, nkpts_in
          !
          READ (stdin, FMT=*, IOSTAT=ierr) kptname_in(j), ( kpt_in(i,j), i=1,3 )
          IF (ierr/=0) CALL errore(subname, 'reading kpt_in', j)
          !
      ENDDO
      !
      CALL write_header( stdout, 'Band interpolation by WFs' )
      CALL flush_unit( stdout )


      !
      ! if required, get data from sgm datafile
      !
      IF ( lhave_sgm ) THEN
          !
          CALL file_open(sgm_unit, TRIM(datafile_sgm), PATH="/", ACTION="read", IERR=ierr)
          IF ( ierr/=0 ) CALL errore(subname,'opening '//TRIM(datafile_sgm), ABS(ierr) )
          !
          CALL operator_read_aux( sgm_unit, DIMWANN=dimwann_sgm, NR=nrtot_sgm,        &
                                            DYNAMICAL=ldynam_sgm, IERR=ierr )
                                            !
          IF ( ierr/=0 ) CALL errore(subname,'reading DIMWANN--DYNAMICAL', ABS(ierr) )

          !
          ! few checks
          !
          IF ( ldynam_sgm )             CALL errore(subname,'sgm is dynamical', 71 )
          IF ( dimwann_sgm /= dimwann ) CALL errore(subname,'invalid dimwann_sgm',1)
          IF ( nrtot_sgm /= nrtot )     CALL errore(subname,'invalid nrtot_sgm',1)


          !
          ! here we check that the order of R vectors from file is
          ! the same of VR
          !
          ALLOCATE( vr_sgm( 3, nrtot ), STAT=ierr )
          IF ( ierr /=0 ) CALL errore(subname,'allocating vr_sgm', ABS(ierr) )
          !
          CALL operator_read_aux( sgm_unit, VR=vr_sgm, IERR=ierr )
          IF ( ierr/=0 ) CALL errore(subname,'reading VR', ABS(ierr) )
          !
          DO ir = 1, nrtot
             !
             raux = DOT_PRODUCT( vr(:,ir)-vr_sgm(:,ir), vr(:,ir)-vr_sgm(:,ir) )
             IF ( raux > EPS_m6 ) CALL errore(subname,'invalid R vectors from sgm file',ir)
             !
          ENDDO
          !
          DEALLOCATE( vr_sgm, STAT=ierr)
          IF ( ierr/=0 ) CALL errore(subname,'deallocating vr_sgm',ABS(ierr))

          !
          ! now allocate and read global quantities (rsgm, basically)
          !
          CALL correlation_allocate( )
          !
          CALL operator_read_data( sgm_unit, R_OPR=rsgm, IERR=ierr )
          IF ( ierr/=0 ) CALL errore(subname,'reading static rsgm', 11)
          !
          CALL file_close(sgm_unit, PATH="/", ACTION="read", IERR=ierr)
          IF ( ierr/=0 ) CALL errore(subname,'closing '//TRIM(datafile_sgm), ABS(ierr) )
          !
      ENDIF


      !
      ! Local memory
      !
      ALLOCATE( kham( dimwann, dimwann ), STAT=ierr )
      IF( ierr /=0 ) CALL errore(subname,'allocating kham', ABS(ierr) )
      !
      IF ( lhave_overlap ) THEN
         ALLOCATE( kovp( dimwann, dimwann ), STAT=ierr )
         IF( ierr /=0 ) CALL errore(subname,'allocating kovp', ABS(ierr) )
      ENDIF
      !
      IF ( lhave_sgm ) THEN
         ALLOCATE( ksgm(dimwann, dimwann), STAT=ierr )
         IF ( ierr/=0 ) CALL errore(subname,'allocating ksgm',ABS(ierr))
      ENDIF
      !
      ALLOCATE( z( dimwann, dimwann ), STAT=ierr )
      IF( ierr /=0 ) CALL errore(subname, 'allocating z', ABS(ierr) )
      !
      ALLOCATE( xval( nkpts_max ), STAT=ierr )
      IF( ierr /=0 ) CALL errore(subname, 'allocating xval', ABS(ierr) )
      !
      ALLOCATE( xval_in( nkpts_in ), STAT=ierr )
      IF( ierr /=0 ) CALL errore(subname, 'allocating xval_in', ABS(ierr) )
      !
      ALLOCATE( vkpt_int(3, nkpts_max), STAT=ierr )
      IF( ierr /=0 ) CALL errore(subname, 'allocating vkpt_int', ABS(ierr) )
      !
      ALLOCATE( vr_cry(3, nrtot), STAT=ierr )
      IF( ierr /=0 ) CALL errore(subname, 'allocating vr_cry', ABS(ierr) )
      !
      ALLOCATE( r_index(nrtot), STAT=ierr )
      IF( ierr /=0 ) CALL errore(subname, 'allocating r_index', ABS(ierr) )

   !
   !  Determine the k-points used in the band structure interpolation
   !
      !
      ! convert kpts to internal cartesian representation (bohr^-1)
      CALL cry2cart( kpt_in, bvec )
      !
      CALL get_points(nkpts_in, nkpts_max, kpt_in, xval_in,  &
                      kptname_in, vkpt_int, xval, nkpts_tot )
 
      ALLOCATE( eig_int( dimwann, nkpts_tot ), STAT=ierr )
      IF( ierr /=0 ) CALL errore(subname,'allocating eig_int', ABS(ierr) )


      !
      ! find the real space R-vectors to be used (according to ircut)
      !
      lhave_nn(:) = ( ircut(:) /= 0 )
      !
      !
      vr_cry(:,:) = vr(:,:)
      CALL cart2cry( vr_cry, avec )
      !
      nrtot_nn = 0
      !
      DO ir = 1, nrtot
          !
          IF (  ( .NOT. lhave_nn(1) .OR.  ABS(NINT(vr_cry(1,ir))) <= ircut(1) ) .AND. &
                ( .NOT. lhave_nn(2) .OR.  ABS(NINT(vr_cry(2,ir))) <= ircut(2) ) .AND. &
                ( .NOT. lhave_nn(3) .OR.  ABS(NINT(vr_cry(3,ir))) <= ircut(3) )       )  THEN
                !
                nrtot_nn = nrtot_nn + 1
                !
                r_index( nrtot_nn ) = ir
                !
          ENDIF
          !
      ENDDO
      !
      DEALLOCATE( vr_cry, STAT=ierr )
      IF( ierr /=0 ) CALL errore(subname, 'deallocating vr_cry', ABS(ierr) )
      !
      !
      ALLOCATE( vr_nn( 3, nrtot_nn ), wr_nn( nrtot_nn ), STAT=ierr )
      IF( ierr /=0 ) CALL errore(subname,'allocating vr_nn, wr_nn', ABS(ierr) )
      !
      ALLOCATE( rham_nn( dimwann, dimwann, nrtot_nn ), STAT=ierr )
      IF( ierr /=0 ) CALL errore(subname,'allocating rham_nn', ABS(ierr) )
      !
      IF ( lhave_overlap ) THEN
         ALLOCATE( rovp_nn( dimwann, dimwann, nrtot_nn ), STAT=ierr )
         IF( ierr /=0 ) CALL errore(subname,'allocating rovp_nn', ABS(ierr) )
      ENDIF
      !
      IF ( lhave_sgm ) THEN
         ALLOCATE( rsgm_nn( dimwann, dimwann, nrtot_nn ), STAT=ierr )
         IF( ierr /=0 ) CALL errore(subname,'allocating rsgm_nn', ABS(ierr) )
      ENDIF
      !
      DO ir = 1, nrtot_nn
          !
          vr_nn(:, ir )  = vr ( :, r_index(ir) )
          wr_nn( ir )    = wr( r_index(ir) )
          !
          rham_nn( :, :, ir ) = rham( :, :, r_index(ir) )
          !
          IF ( lhave_overlap ) rovp_nn( :, :, ir ) = rovp( :, :, r_index(ir) )
          IF ( lhave_sgm )     rsgm_nn( :, :, ir ) = rsgm( :, :, r_index(ir) )
          !
      ENDDO


      !
      ! Interpolate H_ij(k') at those k-points by fourier interpolation
      ! H_ij(k') ~ sum_R e^{ik'R} H_ij(R), where the sum over R is over a 
      ! finite grid (truncation)
      ! 
      kpt_loop: &
      DO ik = 1, nkpts_tot
          !
          ! compute the Hamiltonian with the correct bloch symmetry
          !
          CALL compute_kham( dimwann, nrtot_nn, vr_nn, wr_nn, rham_nn,  &
                             vkpt_int(:,ik), kham)

          IF ( lhave_overlap ) THEN
              !
              CALL compute_kham( dimwann, nrtot_nn, vr_nn, wr_nn, rovp_nn,  &
                                 vkpt_int(:,ik), kovp)
              !
          ENDIF

          IF ( lhave_sgm ) THEN
              !
              CALL compute_kham( dimwann, nrtot_nn, vr_nn, wr_nn, rsgm_nn,  &
                                 vkpt_int(:,ik), ksgm)
              !
              ! symmetrize the input static sgm to make it hermitean
              !
              CALL zmat_herm( ksgm, dimwann )
              !
              !
              kham(:,:) = kham(:,:) + ksgm(:,:)
              !
          ENDIF
 
          !
          ! Diagonalize the hamiltonian at the present k-point
          ! taking care of overlaps if the case
          !
          IF ( .NOT. lhave_overlap ) THEN
              !
              CALL mat_hdiag( z, eig_int(:,ik), kham(:,:), dimwann)
              !
          ELSE
              !
              CALL mat_hdiag( z, eig_int(:,ik), kham(:,:), kovp(:,:), dimwann)
              !
          ENDIF   
          !
      ENDDO kpt_loop


! 
! ... Write final interpolated band structure to file
! 
      filename=TRIM(fileout)
      !
      OPEN( ham_unit, FILE=TRIM(filename), STATUS='unknown', FORM='formatted', IOSTAT=ierr )
      IF (ierr/=0) CALL errore(subname,'opening '//TRIM(filename),ABS(ierr))
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
      filename=TRIM(work_dir)//'/'//TRIM(prefix)//TRIM(postfix)//'_wanband.dat'
      !
      OPEN( ham_unit, FILE=TRIM(filename), STATUS='unknown', FORM='formatted', IOSTAT=ierr )
      IF (ierr/=0) CALL errore(subname,'opening '//TRIM(filename),ABS(ierr))
      !
      DO i = 1, dimwann
          DO ik = 1, nkpts
            WRITE (ham_unit, "(2e16.8)") REAL(ik-1, dbl)/REAL(nkpts, dbl), &
                               wan_eig(i,ik) -efermi
          ENDDO
          WRITE( ham_unit, "()") 
      ENDDO
      CLOSE( ham_unit )


      filename=TRIM(work_dir)//'/'//TRIM(prefix)//TRIM(postfix)//'_dftband.dat'
      !
      OPEN( ham_unit, FILE=TRIM(filename), STATUS='unknown', FORM='formatted', IOSTAT=ierr )
      IF (ierr/=0) CALL errore(subname,'opening '//TRIM(filename),ABS(ierr))
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
      ! Clean local memory
      !
      DEALLOCATE( kptname_in, STAT=ierr)
      IF( ierr /=0 ) CALL errore(subname, 'deallocating kptname_in', ABS(ierr) )
      !
      DEALLOCATE( kpt_in, STAT=ierr)
      IF( ierr /=0 ) CALL errore(subname, 'deallocating kpt_in', ABS(ierr) )
      !
      DEALLOCATE( xval, STAT=ierr)
      IF( ierr /=0 ) CALL errore(subname, 'deallocating xval', ABS(ierr) )
      !
      DEALLOCATE( xval_in, STAT=ierr)
      IF( ierr /=0 ) CALL errore(subname, 'deallocating xval_in', ABS(ierr) )
      !
      DEALLOCATE( vkpt_int, STAT=ierr)
      IF( ierr /=0 ) CALL errore(subname, 'deallocating kpt', ABS(ierr) )
      !
      DEALLOCATE( eig_int, STAT=ierr)
      IF( ierr /=0 ) CALL errore(subname, 'deallocating eig_int', ABS(ierr) )
      !
      DEALLOCATE( z, STAT=ierr )
      IF( ierr /=0 ) CALL errore(subname, 'deallocating z', ABS(ierr))
      !
      DEALLOCATE( vr_nn, wr_nn, STAT=ierr )
      IF( ierr /=0 ) CALL errore(subname, 'deallocating vr_nn, wr_nn', ABS(ierr) )
      !
      DEALLOCATE( r_index, STAT=ierr )
      IF( ierr /=0 ) CALL errore(subname, 'deallocating r_index', ABS(ierr) )
      !
      DEALLOCATE( kham, rham_nn, STAT=ierr)
      IF( ierr /=0 ) CALL errore(subname, 'deallocating kham, rham_nn', ABS(ierr) )
      !
      IF ( lhave_overlap ) THEN
          !
          DEALLOCATE( kovp, rovp_nn, STAT=ierr )
          IF ( ierr/=0 ) CALL errore(subname,'deallocating kovp, rovp_nn',ABS(ierr))
          !
      ENDIF
      !
      IF ( lhave_sgm ) THEN
          !
          DEALLOCATE( ksgm, rsgm_nn, STAT=ierr )
          IF ( ierr/=0 ) CALL errore(subname,'deallocating ksgm, rsgm_nn',ABS(ierr))
          !
      ENDIF

      CALL timing(subname,OPR='stop')
      CALL log_pop(subname)
      !
END SUBROUTINE do_bands

