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
   USE constants,            ONLY : CZERO, TWO, EPS_m6
   USE parameters,           ONLY : nstrx, nkpts_inx
   USE io_module,            ONLY : stdout, stdin, io_name, ham_unit, space_unit, sgm_unit
   USE io_module,            ONLY : work_dir, prefix, postfix
   USE files_module,         ONLY : file_open, file_close
   USE version_module,       ONLY : version_number
   USE util_module,          ONLY : mat_hdiag, zmat_herm
   USE converters_module,    ONLY : cry2cart, cart2cry
   USE lattice_module,       ONLY : avec, bvec
   USE kpoints_module,       ONLY : nkpts, nrtot, vr, wr 
   USE windows_module,       ONLY : nbnd, imin, imax, eig, efermi, windows_read
   USE subspace_module,      ONLY : subspace_read
   USE hamiltonian_module,   ONLY : dimwann, rham, wan_eig, hamiltonian_read, hamiltonian_init
   USE operator_module
   USE parser_module
   USE want_interfaces_module
   !
   IMPLICIT NONE 

   !
   ! input variables
   !
   INTEGER            :: nkpts_in       ! Number of k-points generating the line (edges)
   INTEGER            :: nkpts_max      ! maximum number of interpolated points
   INTEGER            :: nkpts_tot      ! actual number of point in the line
   LOGICAL            :: use_nn(3)      ! use nearest neighb. in direction i, i=1,3
                                        ! DEBUG and transport purposes
   CHARACTER(nstrx)   :: fileout        ! output filename
   CHARACTER(nstrx)   :: datafile_sgm   ! (eventual) self-energy file

   !
   ! local variables
   !
   INTEGER      :: nrtot_nn, nrtot_sgm, dimwann_sgm
   REAL(dbl)    :: raux
   INTEGER,      ALLOCATABLE :: r_index(:)
   COMPLEX(dbl), ALLOCATABLE :: kham(:,:), rham_nn(:,:,:), z(:,:), r_sgm(:,:,:), k_sgm(:,:)  
   !
   REAL(dbl),    ALLOCATABLE :: kpt_in(:,:), xval_in(:) 
   REAL(dbl),    ALLOCATABLE :: vkpt_int(:,:), xval(:)
   REAL(dbl),    ALLOCATABLE :: vr_cry(:,:), vr_nn(:,:), wr_nn(:), vr_sgm(:,:) 
   REAL(dbl),    ALLOCATABLE :: eig_int(:,:)
   CHARACTER(2), ALLOCATABLE :: kptname_in(:)    
   CHARACTER(nstrx)          :: filename
   !
   INTEGER      :: i, j, ik, ir
   INTEGER      :: ierr
   LOGICAL      :: lfound, lhave_sgm, ldynam_sgm
   !
   ! input namelist
   !
   NAMELIST /INPUT/ prefix, postfix, work_dir, fileout, datafile_sgm, &
                    nkpts_in, nkpts_max, use_nn
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
      fileout                     = ' '
      datafile_sgm                = ' '
      nkpts_in                    = 0
      nkpts_max                   = 100
      use_nn(1:3)                 = .FALSE.
      
      CALL input_from_file ( stdin, ierr )
      IF ( ierr /= 0 )  CALL errore('bands','error in input from file',ABS(ierr))
      !
      READ(stdin, INPUT, IOSTAT=ierr)
      IF ( ierr /= 0 )  CALL errore('bands','Unable to read namelist INPUT',ABS(ierr))

      IF ( LEN_TRIM(fileout) == 0 ) &
           fileout = TRIM(work_dir)//'/'//TRIM(prefix)//TRIM(postfix)//'_bands.dat'

      lhave_sgm = .FALSE.
      IF ( LEN_TRIM(datafile_sgm) > 0 ) lhave_sgm = .TRUE.

      !
      ! Some checks 
      !
      IF ( nkpts_in > nkpts_inx ) CALL errore('bands', 'nkpts_in too large',  nkpts_in)
      IF ( nkpts_in <= 0 )  CALL errore('bands', 'Invalid nkpts_in', ABS(nkpts_in)+1)
      IF ( nkpts_max <= 0 ) CALL errore('bands', 'Invalid nkpts_max', ABS(nkpts_max)+1)


      !
      ! few local allocations
      !
      ALLOCATE( kptname_in( nkpts_in ), STAT=ierr )
      IF( ierr /=0 ) CALL errore('bands', 'allocating kptname_in', ABS(ierr) )
      !
      ALLOCATE( kpt_in( 3, nkpts_in ), STAT=ierr )
      IF( ierr /=0 ) CALL errore('bands', 'allocating kpt_in', ABS(ierr) )

      !
      ! read the edge points of the required kpt-line
      ! kpts should be in CRYSTAL coordinates
      !
      DO j = 1, nkpts_in
          !
          READ (stdin, FMT=*, IOSTAT=ierr) kptname_in(j), ( kpt_in(i,j), i=1,3 )
          IF (ierr/=0) CALL errore('bands', 'reading kpt_in', j)
          !
      ENDDO


!
! ... Getting previous WanT data
!
      CALL want_dftread ( WINDOWS=.FALSE., LATTICE=.TRUE., IONS=.TRUE., KPOINTS=.TRUE.  )
      CALL want_init    ( INPUT=.FALSE.,   WINDOWS=.FALSE., BSHELLS=.FALSE. )

      !
      ! Read Subspace data
      !
      CALL io_name('space',filename)
      !
      CALL file_open(space_unit,TRIM(filename),PATH="/",ACTION="read")
          !
          CALL windows_read(space_unit,"WINDOWS",lfound)
          IF ( .NOT. lfound ) CALL errore('bands',"unable to find WINDOWS",1)
          !
          CALL subspace_read(space_unit,"SUBSPACE",lfound)
          IF ( .NOT. lfound ) CALL errore('bands',"unable to find SUBSPACE",1)
          !
      CALL file_close(space_unit,PATH="/",ACTION="read")
      !
      CALL io_name('space',filename,LPATH=.FALSE.)
      WRITE( stdout,"(/,2x,'Space data read from file: ',a)") TRIM(filename)

      !
      ! Read hamiltonian data
      !
      CALL io_name('hamiltonian',filename)
      !
      CALL file_open(ham_unit,TRIM(filename),PATH="/",ACTION="read")
          !
          CALL hamiltonian_read(ham_unit,"HAMILTONIAN",lfound)
          IF ( .NOT. lfound ) CALL errore('bands',"unable to find HAMILTONIAN",1)
          !
      CALL file_close(ham_unit,PATH="/",ACTION="read")
      !
      CALL io_name('hamiltonian',filename,LPATH=.FALSE.)
      WRITE( stdout,"(2x,'Hamiltonian data read from file: ',a,/)") TRIM(filename)

      !
      ! input summary
      !
      CALL write_header( stdout, "INPUT Summary" )
      !
      WRITE( stdout, "(   7x,'               fileout :',5x,   a)") TRIM(fileout)
      WRITE( stdout, "(   7x,'             nkpts_in  :',3x,3i4 )") nkpts_in
      WRITE( stdout, "(   7x,'             nkpts_max :',3x,3i4 )") nkpts_max
      !
      IF ( ANY( use_nn(:) ) ) THEN
          WRITE( stdout, "(   7x,'             use_nn(1) :',5x,   a)") TRIM( log2char(use_nn(1)) )
          WRITE( stdout, "(   7x,'             use_nn(2) :',5x,   a)") TRIM( log2char(use_nn(2)) )
          WRITE( stdout, "(   7x,'             use_nn(3) :',5x,   a)") TRIM( log2char(use_nn(3)) )
      ENDIF
      !
      WRITE( stdout, "(   7x,'            have sigma :',5x, a  )") TRIM( log2char(lhave_sgm) )
      IF ( lhave_sgm ) THEN
          WRITE( stdout, "(   7x,'        sigma datafile :',5x,   a)") TRIM( datafile_sgm )
      ENDIF


      !
      ! Print data to output
      !
      CALL summary( stdout, INPUT=.FALSE., IONS=.FALSE., WINDOWS=.FALSE. )


      !
      ! if required, get data from sgm datafile
      !
      IF ( lhave_sgm ) THEN
          !
          CALL file_open(sgm_unit, TRIM(datafile_sgm), PATH="/", ACTION="read")
          !
          CALL operator_read_aux( sgm_unit, DIMWANN=dimwann_sgm, NR=nrtot_sgm,        &
                                            DYNAMICAL=ldynam_sgm, IERR=ierr )
                                            !
          IF ( ierr/=0 ) CALL errore('bands','reading DIMWANN--DYNAMICAL', ABS(ierr) )

          !
          ! few checks
          !
          IF ( ldynam_sgm )             CALL errore('bands','sgm is dynamical', 71 )
          IF ( dimwann_sgm /= dimwann ) CALL errore('bands','invalid dimwann_sgm',1)
          IF ( nrtot_sgm /= nrtot )     CALL errore('bands','invalid nrtot_sgm',1)


          !
          ! here we check that the order of R vectors from file is
          ! the same of VR
          !
          ALLOCATE( vr_sgm( 3, nrtot ), STAT=ierr )
          IF ( ierr /=0 ) CALL errore('bands','allocating vr_sgm', ABS(ierr) )
          !
          CALL operator_read_aux( sgm_unit, VR=vr_sgm, IERR=ierr )
          IF ( ierr/=0 ) CALL errore('bands','reading VR', ABS(ierr) )
          !
          DO ir = 1, nrtot
             !
             raux = DOT_PRODUCT( vr(:,ir)-vr_sgm(:,ir), vr(:,ir)-vr_sgm(:,ir) )
             IF ( raux > EPS_m6 ) CALL errore('bands','invalid R vectors from sgm file',ir)
             !
          ENDDO
          !
          DEALLOCATE( vr_sgm, STAT=ierr)
          IF ( ierr/=0 ) CALL errore('bands','deallocating vr_sgm',ABS(ierr))

          !
          ! now allocate and read global quantities
          !
          ALLOCATE( r_sgm(dimwann, dimwann, nrtot), STAT=ierr )
          IF ( ierr/=0 ) CALL errore('bands','allocating r_sgm',ABS(ierr))
          ALLOCATE( k_sgm(dimwann, dimwann), STAT=ierr )
          IF ( ierr/=0 ) CALL errore('bands','allocating k_sgm',ABS(ierr))
          !
          !
          CALL operator_read_data( sgm_unit, R_OPR=r_sgm, IERR=ierr )
          IF ( ierr/=0 ) CALL errore('bands','reading static r_sgm', 11)
          !
          CALL file_close(sgm_unit, PATH="/", ACTION="read")
          !
      ENDIF


!
! ... Main task 
!
      CALL write_header( stdout, 'Band interpolation by WFs' )
      CALL flush_unit( stdout )

      !
      ! Determine the k-points used in the band structure interpolation
      !
      ALLOCATE( kham( dimwann, dimwann ), STAT=ierr )
      IF( ierr /=0 ) CALL errore('bands','allocating ham', ABS(ierr) )
      !
      ALLOCATE( z( dimwann, dimwann ), STAT=ierr )
      IF( ierr /=0 ) CALL errore('bands', 'allocating z', ABS(ierr) )
      !
      ALLOCATE( xval( nkpts_max ), STAT=ierr )
      IF( ierr /=0 ) CALL errore('bands', 'allocating xval', ABS(ierr) )
      !
      ALLOCATE( xval_in( nkpts_in ), STAT=ierr )
      IF( ierr /=0 ) CALL errore('bands', 'allocating xval_in', ABS(ierr) )
      !
      ALLOCATE( vkpt_int(3, nkpts_max), STAT=ierr )
      IF( ierr /=0 ) CALL errore('bands', 'allocating vkpt_int', ABS(ierr) )
      !
      ALLOCATE( vr_cry(3, nrtot), STAT=ierr )
      IF( ierr /=0 ) CALL errore('bands', 'allocating vr_cry', ABS(ierr) )
      !
      ALLOCATE( r_index(nrtot), STAT=ierr )
      IF( ierr /=0 ) CALL errore('bands', 'allocating r_index', ABS(ierr) )
 
      !
      ! convert kpts to internal cartesian representation (bohr^-1)
      CALL cry2cart( kpt_in, bvec )
      !
      CALL get_points(nkpts_in, nkpts_max, kpt_in, xval_in,  &
                      kptname_in, vkpt_int, xval, nkpts_tot )
 
      ALLOCATE( eig_int( dimwann, nkpts_tot ), STAT=ierr )
      IF( ierr /=0 ) CALL errore('bands','allocating eig_int', ABS(ierr) )


      !
      ! find the real space R -vectors to be used (according to use_nn)
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
      IF( ierr /=0 ) CALL errore('bands','allocating vr_nn, wr_nn', ABS(ierr) )
      !
      ALLOCATE( rham_nn( dimwann,dimwann, nrtot_nn ), STAT=ierr )
      IF( ierr /=0 ) CALL errore('bands','allocating rham_nn', ABS(ierr) )
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
      IF( ierr /=0 ) CALL errore('bands', 'deallocating vr_cry, r_index', ABS(ierr) )


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

          IF ( lhave_sgm ) THEN
               !
               CALL compute_kham( dimwann, nrtot_nn, vr_nn, wr_nn, r_sgm,  &
                                  vkpt_int(:,ik), k_sgm)
               !
               ! symmetrize the input static sgm to make it hermitean
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
          CALL mat_hdiag( z, eig_int(:,ik), kham(:,:), dimwann)
          !
      ENDDO kpt_loop


! 
! ... Write final interpolated band structure to file
! 
      filename=TRIM(fileout)
      !
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
      filename=TRIM(work_dir)//'/'//TRIM(prefix)//TRIM(postfix)//'_wanband.dat'
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

      filename=TRIM(work_dir)//'/'//TRIM(prefix)//TRIM(postfix)//'_dftband.dat'
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
      ! Clean local memory
      !
      DEALLOCATE( kptname_in, STAT=ierr)
      IF( ierr /=0 ) CALL errore('bands', 'deallocating kptname_in', ABS(ierr) )
      !
      DEALLOCATE( kpt_in, STAT=ierr)
      IF( ierr /=0 ) CALL errore('bands', 'deallocating kpt_in', ABS(ierr) )
      !
      DEALLOCATE( xval, STAT=ierr)
      IF( ierr /=0 ) CALL errore('bands', 'deallocating xval', ABS(ierr) )
      !
      DEALLOCATE( xval_in, STAT=ierr)
      IF( ierr /=0 ) CALL errore('bands', 'deallocating xval_in', ABS(ierr) )
      !
      DEALLOCATE( vkpt_int, STAT=ierr)
      IF( ierr /=0 ) CALL errore('bands', 'deallocating kpt', ABS(ierr) )
      !
      DEALLOCATE( eig_int, STAT=ierr)
      IF( ierr /=0 ) CALL errore('bands', 'deallocating eig_int', ABS(ierr) )
      !
      DEALLOCATE( z, STAT=ierr )
      IF( ierr /=0 ) CALL errore('bands', 'deallocating z', ABS(ierr))
      !
      DEALLOCATE( vr_nn, wr_nn, STAT=ierr )
      IF( ierr /=0 ) CALL errore('bands', 'deallocating vr_nn, wr_nn', ABS(ierr) )
      !
      DEALLOCATE( kham, rham_nn, STAT=ierr)
      IF( ierr /=0 ) CALL errore('bands', 'deallocating kham, rham_nn', ABS(ierr) )
      !
      IF ( lhave_sgm ) THEN
         !
         DEALLOCATE( r_sgm, STAT=ierr )
         IF ( ierr/=0 ) CALL errore('bands','deallocating r_sgm',ABS(ierr))
         DEALLOCATE( k_sgm, STAT=ierr )
         IF ( ierr/=0 ) CALL errore('bands','deallocating k_sgm',ABS(ierr))
         !
      ENDIF

      !
      ! Clean global memory
      !
      CALL cleanup()

      !
      ! finalize
      !
      CALL shutdown( 'bands' )

END PROGRAM bands






