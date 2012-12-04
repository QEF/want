! 
! Copyright (C) 2012 Andrea Ferretti
!
! This file is distributed under the terms of the
! GNU General Public License. See the file `License'
! in the root directory of the present distribution,
! or http://www.gnu.org/copyleft/gpl.txt .
!
!=====================================================
   PROGRAM unfold
   !=====================================================
   !  
   ! Reads the Hamiltonian (of a Gamma surpercell) on
   ! the wannier basis and unfold it according to an extra
   ! spatial periodicity specified by input
   !
   USE version_module,       ONLY : version_number
   USE parameters,           ONLY : nstrx
   USE io_module,            ONLY : stdout, stdin
   USE io_module,            ONLY : work_dir, prefix, postfix
   USE io_module,            ONLY : datafile_dft => dftdata_file, datafile_sgm
   USE control_module,       ONLY : debug_level, use_debug_mode, verbosity, read_pseudo
   USE control_module,       ONLY : nwfc_buffer, nkb_buffer
   USE datafiles_module,     ONLY : datafiles_init
   USE timing_module,        ONLY : timing
   USE log_module,           ONLY : log_push, log_pop
   USE input_parameters_module,    ONLY : string_check, assume_ncpp
   USE want_interfaces_module
   USE parser_module

   !
   IMPLICIT NONE 

   !
   ! input variables
   !
   INTEGER            :: ndiv(3)         ! number of subdivision of the input unic cell
                                         ! into smaller unit cells (with higer symmetry)
                                         ! for directions i=1,2,3 
   CHARACTER(nstrx)   :: translations    ! = ( "from_file", "from_scratch")
                                         ! written on the Bloch or Wannier basis
   CHARACTER(nstrx)   :: postfix_unfld   ! postfix to describe the unfolded data
   CHARACTER(nstrx)   :: datafile_transl ! specifies the name of the file with the translations
                                         ! usually not needed.

   !
   ! input namelist
   !
   NAMELIST /INPUT/ prefix, postfix, postfix_unfld, work_dir, datafile_dft, datafile_transl, &
                    translations, ndiv, debug_level, verbosity, nkb_buffer, nwfc_buffer, assume_ncpp

   LOGICAL            :: lhave_transl
   !
   CHARACTER(nstrx)   :: translations_allowed(2)
   DATA  translations_allowed / 'from_file', 'from_scratch' /

   !
   ! end of declariations
   !   

!
!------------------------------
! main body
!------------------------------
!
      CALL startup(version_number,'unfold')

      !
      ! read input
      !
      CALL unfold_input()

      !
      ! init post processing (reading previous WanT and DFT data )
      !
      CALL write_header( stdout, "Post Processing Init" )
      !
      CALL datafiles_init()
      !
      CALL postproc_init ( BSHELLS=.TRUE., IONS=.TRUE., PSEUDO=read_pseudo, &
                           WANNIER=.TRUE., SUBSPACE=.TRUE. )

      !
      ! print data to output
      !
      CALL summary( stdout, INPUT=.FALSE., IONS=.FALSE., WINDOWS=.FALSE. )

      !
      ! do the main task
      !
      CALL do_unfold( postfix_unfld, ndiv, lhave_transl, datafile_transl )

      !
      ! Clean global memory
      !
      CALL cleanup()

      !
      ! finalize
      !
      CALL shutdown( 'unfold' )

CONTAINS

!********************************************************
   SUBROUTINE unfold_input()
   !********************************************************
   !
   ! Read INPUT namelist from stdin
   !
   USE mp,                   ONLY : mp_bcast
   USE io_module,            ONLY : io_init, ionode, ionode_id
   USE control_module,       ONLY : read_pseudo, use_pseudo
   !
   IMPLICIT NONE

      CHARACTER(12)    :: subname = 'unfold_input'
      INTEGER          :: ierr
      !
      ! end of declarations
      !

      CALL timing( subname, OPR='start' )

      !   
      ! init input namelist   
      !   
      prefix                      = 'WanT' 
      postfix                     = ' ' 
      postfix_unfld               = ' '
      translations                = ' '
      work_dir                    = './' 
      datafile_dft                = ' '
      datafile_transl             = ' '
      ndiv(1:3)                   = 1
      debug_level                 = 0
      verbosity                   = 'medium'
      nwfc_buffer                 = -1
      nkb_buffer                  = -1
      assume_ncpp                 = .FALSE.
      
      CALL input_from_file ( stdin )
      !
      IF ( ionode ) READ(stdin, INPUT, IOSTAT=ierr)
      !
      CALL mp_bcast( ierr, ionode_id )
      IF ( ierr /= 0 )  CALL errore(subname,'Unable to read namelist INPUT',ABS(ierr))

      !
      ! broadcast
      !
      CALL mp_bcast( prefix,          ionode_id )
      CALL mp_bcast( postfix,         ionode_id )
      CALL mp_bcast( postfix_unfld,   ionode_id )
      CALL mp_bcast( work_dir,        ionode_id )
      CALL mp_bcast( translations,    ionode_id )
      CALL mp_bcast( datafile_dft,    ionode_id )
      CALL mp_bcast( datafile_transl, ionode_id )
      CALL mp_bcast( ndiv,            ionode_id )
      CALL mp_bcast( debug_level,     ionode_id )
      CALL mp_bcast( verbosity,       ionode_id )
      CALL mp_bcast( nwfc_buffer,     ionode_id )
      CALL mp_bcast( nkb_buffer,      ionode_id )
      CALL mp_bcast( assume_ncpp,     ionode_id )

      !
      ! init
      !
      use_debug_mode = .FALSE.
      IF ( debug_level > 0  )     use_debug_mode = .TRUE.
      !   
      !   
      lhave_transl = .FALSE.
      CALL change_case( translations, 'lower' )
      !
      CALL string_check( translations, translations_allowed, ierr)
      IF ( ierr/=0 ) CALL errore(subname,"invalid translations: "//TRIM(translations),10)
      !
      IF ( TRIM(translations) == "from_scratch" ) lhave_transl = .FALSE.
      IF ( TRIM(translations) == "from_file" )    lhave_transl = .TRUE.
      !
      !
      use_pseudo   = .NOT. assume_ncpp
      read_pseudo  = .NOT. assume_ncpp


      !
      ! we don't need wfc if translations have been already calculated
      !
      IF ( lhave_transl ) THEN
          !
          CALL io_init( NEED_WFC=.FALSE. )
          !
      ELSE
          !
          CALL io_init( NEED_WFC=.TRUE. )
          !
      ENDIF


      !
      ! Some checks 
      !
      IF ( ANY( ndiv(:) < 0 ) )  CALL errore(subname,'Invalid ircut', 10)


      !
      ! input summary
      !
      IF ( ionode ) THEN
          !
          CALL write_header( stdout, "INPUT Summary" )
          !
          WRITE( stdout, "(   7x,'              work dir :',5x,   a)") TRIM(work_dir)
          WRITE( stdout, "(   7x,'                prefix :',5x,   a)") TRIM(prefix)
          WRITE( stdout, "(   7x,'               postfix :',5x,   a)") TRIM(postfix)
          WRITE( stdout, "(   7x,'         postfix_unfld :',5x,   a)") TRIM(postfix_unfld)
          IF ( LEN_TRIM( datafile_dft ) /= 0 ) &
             WRITE( stdout, "(7x,'          datafile_dft :',5x,   a)") TRIM(datafile_dft)
          IF ( LEN_TRIM( datafile_transl ) /= 0 ) &
             WRITE( stdout, "(7x,'       datafile_transl :',5x,   a)") TRIM(datafile_transl)
          !
          WRITE( stdout, "(   7x,'          translations :',5x,   a)") TRIM(translations)
          WRITE( stdout, "(   7x,'                  ndiv :',3x, 3i4)") ndiv(:)
          WRITE( stdout, "(   7x,'            nkb_buffer :',3x,  i4)") nkb_buffer
          WRITE( stdout, "(   7x,'           nwfc_buffer :',3x,  i4)") nwfc_buffer
          WRITE( stdout, "(   7x,'           assume_ncpp :',3x,   a)") TRIM( log2char( assume_ncpp ) )
          !
          WRITE( stdout, "()" )
          !
      ENDIF

      CALL timing(subname,OPR='stop')
      !
   END SUBROUTINE unfold_input

END PROGRAM unfold


!********************************************************
   SUBROUTINE do_unfold( postfix_unfld, ndiv, lhave_transl, datafile_transl )
   !********************************************************
   !
   ! perform the main task of the calculation
   !
   USE kinds
   USE constants,            ONLY : ZERO, CZERO, TWO, EPS_m6
   USE parameters,           ONLY : nstrx, nkpts_inx
   USE io_module,            ONLY : stdout, stdin, io_name, ionode, ionode_id, ham_unit, sgm_unit
   USE io_module,            ONLY : work_dir, prefix, postfix
   USE mp,                   ONLY : mp_bcast, mp_sum
   USE mp_global,            ONLY : mpime, nproc
   USE grids_module,         ONLY : grids_get_rgrid
   USE files_module,         ONLY : file_open, file_close
   USE util_module,          ONLY : mat_hdiag, mat_herm, mat_mul
   USE converters_module,    ONLY : cry2cart
   USE lattice_module,       ONLY : avec, bvec
   USE windows_module,       ONLY : nbnd, imin, imax, dimwin, dimwinx, nspin
   USE kpoints_module,       ONLY : nrtot, nkpts_g
   USE subspace_module,      ONLY : eamp
   USE localization_module,  ONLY : cu, rave
   USE hamiltonian_module,   ONLY : dimwann, rham, rovp, lhave_overlap
   USE translations_module,  ONLY : transl
   USE timing_module,        ONLY : timing
   USE log_module,           ONLY : log_push, log_pop
   USE operator_module
   !
   IMPLICIT NONE

      !
      ! input vars
      !
      CHARACTER(*),  INTENT(IN) :: postfix_unfld
      INTEGER,       INTENT(IN) :: ndiv(3)
      LOGICAL,       INTENT(IN) :: lhave_transl
      CHARACTER(*),  INTENT(IN) :: datafile_transl
   
      !
      ! local vars
      !
      CHARACTER(9)      :: subname = 'do_unfold'

      INTEGER           :: dimwann_unfld
      INTEGER           :: ndir, dir_map(3)
      !
      INTEGER           :: nrtot_unfld, nr_unfld(3)
      INTEGER           :: nkpts_unfld, nk_unfld(3), s_unfld(3)
      REAL(dbl)         :: avec_unfld(3,3), bvec_unfld(3,3)
      REAL(dbl)         :: rvect_unfld(3,3)
      !
      REAL(dbl),    ALLOCATABLE :: vkpt_unfld(:,:), vr_unfld(:,:)
      REAL(dbl),    ALLOCATABLE :: wk_unfld(:), wr_unfld(:)
      INTEGER,      ALLOCATABLE :: ivr_unfld(:,:)
      COMPLEX(dbl), ALLOCATABLE :: rham_unfld(:,:,:), rovp_unfld(:,:,:)
      COMPLEX(dbl), ALLOCATABLE :: work(:,:), work_ovp(:,:), trmat(:,:,:)
      INTEGER,      ALLOCATABLE :: orb_map(:)
      !
      CHARACTER(nstrx)  :: fileham, filespace
      CHARACTER(1)      :: op
      !
      INTEGER           :: i, j, k, ir
      INTEGER           :: ierr
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
      ! init
      !
      CALL write_header( stdout, 'Initialization' )
      CALL flush_unit( stdout )
      !
      !
      IF ( ALL( ndiv(:) == 1) ) CALL warning(subname,"no division required")
      !
      IF ( nkpts_g /= 1 ) CALL errore(subname,"unfolding implemented only for gamma",10)
      !
      IF ( MOD( dimwann, PRODUCT(ndiv) ) /= 0 ) CALL errore(subname,"dimwann & ndiv not consistent",10)
      dimwann_unfld = dimwann / PRODUCT( ndiv )
      
      !
      ! lattice redefinition
      !
      DO i = 1, 3
          !
          avec_unfld(:,i) = avec(:,i) / REAL( ndiv(i), dbl ) 
          bvec_unfld(:,i) = bvec(:,i) * REAL( ndiv(i), dbl )
          !
      ENDDO

      !
      ! direct and reciprocal lattice
      !
      nkpts_unfld = PRODUCT( ndiv(:) )
      nk_unfld    = ndiv
      s_unfld     = 0
      !
      ALLOCATE( vkpt_unfld(3,nkpts_unfld), STAT=ierr )
      IF ( ierr/=0 ) CALL errore(subname,"allocating vkpt_unfld",ABS(ierr))
      ALLOCATE( wk_unfld(nkpts_unfld), STAT=ierr )
      IF ( ierr/=0 ) CALL errore(subname,"allocating wk_unfld",ABS(ierr))
      !
      ! generate the kpt grid
      ! vkpt_unfld in crystal units
      !
      CALL monkpack( nk_unfld, s_unfld, vkpt_unfld )
      wk_unfld(:) = 1.0_dbl/REAL( nkpts_unfld )
      !
      !! convert to cartesian units
      !!CALL cry2cart( vkpt_unfld, bvec_unfld )
      !
      ! real space grid
      nr_unfld(1:3) = ndiv(1:3)
      !
      CALL grids_get_rgrid(nr_unfld, NRTOT=nrtot_unfld )
      !
      ALLOCATE( ivr_unfld(3,nrtot_unfld), vr_unfld(3,nrtot_unfld), STAT=ierr )
      IF ( ierr/=0 ) CALL errore(subname,"allocating ivr_unfld, vr_unfld",ABS(ierr))
      ALLOCATE( wr_unfld(nrtot_unfld), STAT=ierr )
      IF ( ierr/=0 ) CALL errore(subname,"wr_unfld",ABS(ierr))
      !
      CALL grids_get_rgrid(nr_unfld, WR=wr_unfld, IVR=ivr_unfld )
      !   
      vr_unfld(:,:) = REAL( ivr_unfld, dbl)
      CALL cry2cart( vr_unfld, avec_unfld)
      
      !
      ! select the directions to unfold
      !
      ndir = 0
      DO i = 1, 3
          !
          IF ( ndiv(i) > 1 ) THEN
              !
              ndir = ndir+1
              rvect_unfld(:,ndir) = avec_unfld(:,i)
              dir_map(ndir)=i
              !
          ENDIF
          !
      ENDDO

      !
      ! translation operators
      !
      CALL write_header( stdout, 'Translation operators' )
      CALL flush_unit( stdout )
      !
      ! translations are read or computed and moved to the current
      ! WF basis
      !
      CALL translations_drv( dimwann, dimwinx, ndir, rvect_unfld, lhave_transl, datafile_transl )



      !
      ! basis selection
      !
      ALLOCATE( orb_map(dimwann_unfld), STAT=ierr )
      IF ( ierr/=0 ) CALL errore(subname,"allocating orb_map",ABS(ierr))

      ! XXX to be properly implemented
      DO i = 1, dimwann_unfld
          orb_map(i)=i
      ENDDO


      !
      ! redefine the hamiltonian
      !
      CALL write_header( stdout, 'Hamiltonian redefinition' )
      CALL flush_unit( stdout )
      !
      ALLOCATE( rham_unfld(dimwann_unfld,dimwann_unfld,nrtot_unfld), STAT=ierr )
      IF ( ierr/=0 ) CALL errore(subname,"allocating rham_unfld",ABS(ierr))
      ALLOCATE( rovp_unfld(dimwann_unfld,dimwann_unfld,nrtot_unfld), STAT=ierr )
      IF ( ierr/=0 ) CALL errore(subname,"allocating rovp_unfld",ABS(ierr))
      ALLOCATE( work(dimwann,dimwann), STAT=ierr )
      IF ( ierr/=0 ) CALL errore(subname,"allocating work",ABS(ierr))
      ALLOCATE( work_ovp(dimwann,dimwann), STAT=ierr )
      IF ( ierr/=0 ) CALL errore(subname,"allocating work_ovp",ABS(ierr))
      ALLOCATE( trmat(dimwann,dimwann,ndir), STAT=ierr )
      IF ( ierr/=0 ) CALL errore(subname,"allocating trmat",ABS(ierr))
      ! 
      DO ir = 1, nrtot_unfld
          !
          work(:,:) = rham(:,:,1)
          !
          IF ( lhave_overlap ) THEN
              work_ovp(:,:) = rovp(:,:,1)
          ENDIF
          !
          ! build T1^a T2^b T3^c
          !
          DO j = 1, ndir
              !
              op="N"
              IF ( ivr_unfld( dir_map(j) ,ir) < 0 ) op="C"
              !
              trmat(:,:,j) =0.0d0 
              DO i = 1, dimwann
                  trmat(i,i,j) =1.0d0
              ENDDO
              !
              DO k = 1, ABS( ivr_unfld( dir_map(j), ir) )
                  CALL mat_mul( trmat(:,:,j), trmat(:,:,j), 'N', transl(:,:,j), op, dimwann, dimwann, dimwann )
              ENDDO
              !
              CALL mat_mul( work, work, 'N', trmat(:,:,j), 'N', dimwann, dimwann, dimwann )
              !
          ENDDO
          !
          ! selecting the submatrix
          !
          DO j = 1, dimwann_unfld
          DO i = 1, dimwann_unfld
              rham_unfld(i,j,ir) = work(orb_map(i),orb_map(j))
          ENDDO
          ENDDO
          !
          ! do the same for overlaps if needed
          !
          IF ( lhave_overlap ) THEN
              !
              DO j = 1, ndir
                  CALL mat_mul( work_ovp, work_ovp, 'N', trmat(:,:,j), 'N', dimwann, dimwann, dimwann )
              ENDDO
              !
              DO j = 1, dimwann_unfld
              DO i = 1, dimwann_unfld
                  rovp_unfld(i,j,ir) = work_ovp(orb_map(i),orb_map(j))
              ENDDO
              ENDDO
              !
          ENDIF
          !
      ENDDO


      !
      ! dump new datafiles
      !
      CALL io_name('space', filespace, LPOSTFIX=.TRUE., POSTFIX_LOC=TRIM(postfix_unfld) )
      CALL io_name('ham',   fileham,   LPOSTFIX=.TRUE., POSTFIX_LOC=TRIM(postfix_unfld) )
      !
      IF ( ionode ) THEN
         WRITE(stdout,"(/,2x,'Unfold data dumped to files:')")
         WRITE(stdout,"(  2x,'        ',a)") TRIM(fileham)
         WRITE(stdout,"(  2x,'        ',a)") TRIM(filespace)
      ENDIF
      !
      CALL write_unfld_data( .TRUE., fileham, .TRUE., filespace, dimwann_unfld, nspin, &
                             nkpts_unfld, nk_unfld, s_unfld, vkpt_unfld, wk_unfld, &
                             nrtot_unfld, nr_unfld, ivr_unfld, wr_unfld, &
                             avec_unfld, bvec_unfld, lhave_overlap, 0.0_dbl, &
                             rham_unfld, rovp_unfld )

      !
      ! Clean local memory
      !
      DEALLOCATE( work, STAT=ierr)
      IF( ierr /=0 ) CALL errore(subname, 'deallocating work', ABS(ierr) )
      DEALLOCATE( trmat, STAT=ierr)
      IF( ierr /=0 ) CALL errore(subname, 'deallocating trmat', ABS(ierr) )

      !
      !
      !
      CALL timing(subname,OPR='stop')
      CALL log_pop(subname)
      !
END SUBROUTINE do_unfold


!********************************************************
   SUBROUTINE translations_drv( ndim, ndimx, nvect, rvect, lhave_transl, datafile_transl )
   !********************************************************
   !
   ! allocate, compute or read the translation matrix elements
   ! 
   USE kinds
   USE constants,                 ONLY : ZERO, CZERO, EPS_m6
   USE io_module,                 ONLY : stdout, ionode, io_name
   USE files_module,              ONLY : file_open, file_close, file_exist
   USE timing_module,             ONLY : timing
   USE log_module,                ONLY : log_pop, log_push
   USE translations_module,       ONLY : translations_allocate, translations_write, translations_read, &
                                         transl, transl_alloc => alloc, basis, nvect_mod => nvect, &
                                         rvect_mod => rvect, ndimx_mod => ndimx, ndim_mod => ndim
   USE iotk_module,               ONLY : iotk_free_unit
   USE subspace_module,           ONLY : eamp, dimwann, dimwin, dimwinx, nkpts_g, subspace_alloc => alloc
   USE localization_module,       ONLY : cu
   USE util_module,               ONLY : mat_mul
   USE want_interfaces_module
   !
   IMPLICIT NONE
   
      !
      ! I/O vars
      !
      INTEGER,      INTENT(IN)  :: ndim, ndimx, nvect
      REAL(dbl),    INTENT(IN)  :: rvect(3,nvect)
      LOGICAL,      INTENT(IN)  :: lhave_transl
      CHARACTER(*), INTENT(IN)  :: datafile_transl
     
      !
      ! local vars
      !
      CHARACTER(16)  :: subname="translations_drv"
      CHARACTER(256) :: filename
      INTEGER        :: i, ik, iunit, ierr
      !
      COMPLEX(dbl), ALLOCATABLE :: work(:,:)
      

!
!---------------------------------
! main body
!---------------------------------
!
      CALL log_push(subname)
      CALL timing(subname,OPR='start')
      !
      CALL iotk_free_unit( iunit ) 
      !
      ! check whether translations are read or computed
      !
      IF ( lhave_transl ) THEN
          !
          ! operators are read from file 
          ! and converted to the local WF basis if needed
          !
          IF ( LEN_TRIM( datafile_transl ) /= 0 ) THEN
              filename=TRIM( datafile_transl )
          ELSE
              CALL io_name( "translations", filename)
          ENDIF
          !
          IF ( ionode ) WRITE( stdout, "(2x,'Translation operators read from file:   ',a,/)") TRIM(filename)
          !
          IF ( .NOT. file_exist( filename ) ) &
              CALL errore(subname,"file does not exist: "//TRIM(filename),10 )
          !
          CALL file_open(iunit,TRIM(filename),PATH="/",ACTION="read", IERR=ierr )
          IF ( ierr/=0 ) CALL errore(subname,"opening "//TRIM(filename),10 )
          !
          CALL translations_read( iunit, nvect, rvect )
          !
          CALL file_close(iunit,PATH="/",ACTION="read", IERR=ierr )
          IF ( ierr/=0 ) CALL errore(subname,"closing "//TRIM(filename),10 )
          !
          !
          IF ( TRIM(basis) /= "wannier" ) CALL errore(subname,"invalid basis: "//TRIM(basis),10)
          IF ( ndim_mod  /= ndim )        CALL errore(subname,"invalid ndim",10)
          IF ( ndimx_mod /= ndimx )       CALL errore(subname,"invalid ndimx",10)
          IF ( nvect_mod /= nvect )       CALL errore(subname,"invalid nvect",10)
          !
          DO i = 1, nvect
              IF ( ABS( DOT_PRODUCT( rvect(:,i)-rvect_mod(:,i), rvect(:,i)-rvect_mod(:,i)) ) > EPS_m6 ) &
                 CALL errore(subname,"invalid rvect",i)
          ENDDO
          !
          !
      ELSE
          !
          ! translation operators are computed (output on the WF basis)
          !
          IF ( ionode ) WRITE( stdout, "(2x,'Translation operators are computed',/)")
          !
          IF ( transl_alloc ) CALL errore(subname,"unexpected transl alloc",10)
          !
          CALL translations_allocate( ndimx, nvect )
          !
          rvect_mod(:,:) = rvect(:,:)
          ndim_mod       = ndim
          !
          CALL wfc_drv( DO_PROJ=.FALSE., DO_OVP=.FALSE., DO_TRANSL=.TRUE. )
          !
          basis = "dft_wfc"

          !
          ! convert to the wannier basis
          !
          IF ( .NOT. subspace_alloc ) CALL errore(subname,"subspace not alloc",10)
          !
          ALLOCATE( work(dimwinx, dimwinx), STAT=ierr )
          IF ( ierr/=0 ) CALL errore(subname,"allocatign work",ABS(ierr))
          !
          IF ( nkpts_g /= 1) CALL errore(subname,"invalid nkpts /= 1: not yet implemented",10)
          !
          ik = 1
          ! 
          DO i = 1, nvect
              !
              work(:,:) = ZERO
              !
              CALL mat_mul( work, eamp(:,:,ik), 'C', transl(:,:,i), 'N', dimwann, dimwin(ik), dimwin(ik) )
              CALL mat_mul( work, work, 'N', eamp(:,:,ik), 'N', dimwann, dimwann, dimwin(ik) )
              !
              CALL mat_mul( work, cu(:,:,ik), 'C', work, 'N', dimwann, dimwann, dimwann )
              CALL mat_mul( work, work, 'N', cu(:,:,ik), 'N', dimwann, dimwann, dimwann )
              !
              transl(:,:,i) = ZERO
              transl(1:dimwann,1:dimwann,i) = work(1:dimwann,1:dimwann)
              !
          ENDDO
          !
          DEALLOCATE( work, STAT=ierr )
          IF ( ierr/=0 ) CALL errore(subname,"deallocating work",ABS(ierr))
          !
          basis = "wannier"

          !
          ! dump translation vectors to file
          !
          IF ( ionode ) THEN
              !
              CALL io_name( "translations", filename)
              !
              CALL file_open(iunit,TRIM(filename),PATH="/",ACTION="write", IERR=ierr )
              IF ( ierr/=0 ) CALL errore(subname,"opening "//TRIM(filename),11)
              !
              CALL translations_write( iunit )
              !
              CALL file_close(iunit,PATH="/",ACTION="write", IERR=ierr )
              IF ( ierr/=0 ) CALL errore(subname,"closing "//TRIM(filename),11)
              !
          ENDIF
          !
          !
      ENDIF
      !
      !
      CALL timing(subname,OPR='stop')
      CALL log_pop(subname)
      !
      RETURN
      !
END SUBROUTINE translations_drv


!********************************************************
   SUBROUTINE write_unfld_data( lwrite_ham, fileham, lwrite_space, filespace, &
                                dimwann, nspin, nkpts, nk, s, vkpt, wk, &
                                nrtot, nr, ivr, wr, &
                                avec, bvec, lhave_overlap, efermi, &
                                rham, rovp )
   !********************************************************
   !
   ! read translation operators from file
   !
   USE kinds
   USE io_module,            ONLY : ionode, ionode_id
   USE files_module,         ONLY : file_open, file_close, file_exist
   USE timing_module,        ONLY : timing
   USE log_module,           ONLY : log_push, log_pop
   USE iotk_module
   !
   IMPLICIT NONE
      !
      LOGICAL,            INTENT(IN)  :: lwrite_ham, lwrite_space
      CHARACTER(*),       INTENT(IN)  :: fileham, filespace
      INTEGER,            INTENT(IN)  :: dimwann, nspin, nkpts, nk(3), s(3)
      INTEGER,            INTENT(IN)  :: nrtot, nr(3) 
      REAL(dbl),          INTENT(IN)  :: avec(3,3), bvec(3,3)
      REAL(dbl),          INTENT(IN)  :: vkpt(3,nkpts), wk(nkpts)
      REAL(dbl),          INTENT(IN)  :: wr(nrtot), efermi
      INTEGER,            INTENT(IN)  :: ivr(3,nrtot)
      LOGICAL,            INTENT(IN)  :: lhave_overlap
      COMPLEX(dbl),       INTENT(IN)  :: rham(dimwann,dimwann,nrtot)
      COMPLEX(dbl),       INTENT(IN)  :: rovp(dimwann,dimwann,nrtot)
      
      CHARACTER(256) :: attr
      CHARACTER(16)  :: subname="write_unfld_data"
      LOGICAL        :: binary = .TRUE.
      INTEGER        :: ir, isp, ierr
      INTEGER        :: ounit
      !
      INTEGER, ALLOCATABLE :: itmp(:)
      REAL,    ALLOCATABLE :: eig_(:,:)
     

!
!---------------------------------
! dump to file
!---------------------------------
!

   CALL iotk_free_unit( ounit )
   !
   IF ( nspin /= 1) CALL errore(subname,"invalid nspin==2",10)

   IF ( lwrite_ham .AND. ionode ) THEN
       !
       CALL iotk_open_write( ounit, FILE=TRIM(fileham), BINARY=binary )
       CALL iotk_write_begin( ounit, "HAMILTONIAN" )
       !
       !
       CALL iotk_write_attr( attr,"dimwann",dimwann,FIRST=.TRUE.)
       CALL iotk_write_attr( attr,"nkpts",nkpts)
       CALL iotk_write_attr( attr,"nspin",nspin)
       CALL iotk_write_attr( attr,"nk",nk)
       CALL iotk_write_attr( attr,"shift",s)
       CALL iotk_write_attr( attr,"nrtot",nrtot)
       CALL iotk_write_attr( attr,"nr",nr)
       CALL iotk_write_attr( attr,"have_overlap", lhave_overlap )
       CALL iotk_write_attr( attr,"fermi_energy", efermi )
       CALL iotk_write_empty( ounit,"DATA",ATTR=attr)
       !
       CALL iotk_write_attr( attr,"units","bohr",FIRST=.TRUE.)
       CALL iotk_write_dat( ounit,"DIRECT_LATTICE", avec, ATTR=attr, COLUMNS=3)
       !
       CALL iotk_write_attr( attr,"units","bohr^-1",FIRST=.TRUE.)
       CALL iotk_write_dat( ounit,"RECIPROCAL_LATTICE", bvec, ATTR=attr, COLUMNS=3)
       !
       CALL iotk_write_attr( attr,"units","crystal",FIRST=.TRUE.)
       CALL iotk_write_dat( ounit,"VKPT", vkpt, ATTR=attr, COLUMNS=3)
       CALL iotk_write_dat( ounit,"WK", wk)
       !
       CALL iotk_write_dat( ounit,"IVR", ivr, ATTR=attr, COLUMNS=3)
       CALL iotk_write_dat( ounit,"WR", wr)
       !
       !
       spin_loop: &
       DO isp = 1, nspin
          !
          IF ( nspin == 2 ) THEN
              !
              CALL iotk_write_begin( ounit, "SPIN"//TRIM(iotk_index(isp)) )
              !
          ENDIF
          !
          !
          CALL iotk_write_begin( ounit,"RHAM")
          !
          DO ir = 1, nrtot
              !
              CALL iotk_write_dat( ounit,"VR"//TRIM(iotk_index(ir)), rham(:,:,ir ) )
              IF ( lhave_overlap ) &
                 CALL iotk_write_dat( ounit,"OVERLAP"//TRIM(iotk_index(ir)), rovp(:,:,ir) )
              !
          ENDDO
          !
          CALL iotk_write_end( ounit,"RHAM")
          !
          IF ( nspin == 2 ) THEN
              !
              CALL iotk_write_end( ounit, "SPIN"//TRIM(iotk_index(isp)) )
              !
          ENDIF
          !
       ENDDO spin_loop
       !
       CALL iotk_write_end( ounit, "HAMILTONIAN" )
       CALL iotk_close_write( ounit )
       !
   ENDIF

   IF ( lwrite_space .AND. ionode ) THEN
       !
       CALL iotk_open_write( ounit, FILE=TRIM(filespace), BINARY=binary )
       !
       !
       CALL iotk_write_begin( ounit, "WINDOWS" )
       !
       !
       CALL iotk_write_attr( attr,"nbnd",dimwann,FIRST=.TRUE.)
       CALL iotk_write_attr( attr,"nkpts",nkpts)
       CALL iotk_write_attr( attr,"nspin",nspin)
       CALL iotk_write_attr( attr,"spin_component","none")
       CALL iotk_write_attr( attr,"efermi", efermi )
       CALL iotk_write_attr( attr,"dimwinx", dimwann )
       CALL iotk_write_empty( ounit,"DATA",ATTR=attr)
       !
       ALLOCATE( itmp(nkpts), STAT=ierr )
       IF ( ierr/=0 ) CALL errore(subname, 'allocating itmp', ABS(ierr))
       ALLOCATE( eig_(dimwann,nkpts), STAT=ierr )
       IF ( ierr/=0 ) CALL errore(subname, 'allocating eig_', ABS(ierr))
       !
       eig_(:,:) = 0.0d0
       !
       itmp(:) = dimwann
       CALL iotk_write_dat( ounit, "DIMWIN", itmp, COLUMNS=8 )
       itmp(:) = 1
       CALL iotk_write_dat( ounit, "IMIN", itmp, COLUMNS=8 )
       itmp(:) = dimwann
       CALL iotk_write_dat( ounit, "IMAX", itmp, COLUMNS=8 )
       !
       DO isp = 1, nspin
           !
           IF ( nspin == 2 ) THEN
               CALL iotk_write_begin( ounit, "SPIN"//TRIM(iotk_index(isp)) )
           ENDIF
           !
           CALL iotk_write_dat( ounit, "EIG", eig_(:,:), COLUMNS=4)
           !
           IF ( nspin == 2 ) THEN
               CALL iotk_write_end( ounit, "SPIN"//TRIM(iotk_index(isp)) )
           ENDIF
           !
       ENDDO
       !
       CALL iotk_write_end( ounit, "WINDOWS" )
       !
       !
       CALL iotk_write_begin( ounit, "SUBSPACE" )
       !
       CALL iotk_write_attr( attr,"dimwinx",dimwann,FIRST=.TRUE.)
       CALL iotk_write_attr( attr,"nkpts",nkpts)
       CALL iotk_write_attr( attr,"dimwann", dimwann)
       CALL iotk_write_empty( ounit,"DATA",ATTR=attr)
       !
       itmp(:) = dimwann
       CALL iotk_write_dat( ounit, "DIMWIN", itmp, COLUMNS=8 )
       !
       CALL iotk_write_end( ounit, "SUBSPACE" )
       !
       !
       CALL iotk_close_write( ounit )
       !
       DEALLOCATE( itmp, STAT=ierr )
       IF ( ierr/=0 ) CALL errore(subname, 'deallocating itmp', ABS(ierr))
       DEALLOCATE( eig_, STAT=ierr )
       IF ( ierr/=0 ) CALL errore(subname, 'deallocating eig_', ABS(ierr))
       !
   ENDIF

END SUBROUTINE write_unfld_data


