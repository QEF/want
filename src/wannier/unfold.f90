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
   USE kinds,                      ONLY : dbl
   USE version_module,             ONLY : version_number
   USE parameters,                 ONLY : nstrx
   USE io_module,                  ONLY : stdout, stdin
   USE io_module,                  ONLY : io_init, work_dir, prefix, postfix
   USE control_module,             ONLY : debug_level, use_debug_mode, verbosity, read_pseudo
   USE control_module,             ONLY : nwfc_buffer, nkb_buffer
   USE datafiles_module,           ONLY : datafiles_init
   USE timing_module,              ONLY : timing
   USE log_module,                 ONLY : log_push, log_pop
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
   CHARACTER(nstrx)   :: orb_mapping     ! ("automatic","first_orbitals")
   CHARACTER(nstrx)   :: datafile_dft
                                         ! 
   REAL(dbl)          :: atmproj_sh      ! shifthing: energy shift when computing the proj Hamiltonian
   REAL(dbl)          :: atmproj_thr     ! filtering: thr on projections 
   INTEGER            :: atmproj_nbnd    ! filtering: # of bands
   LOGICAL            :: do_orthoovp

   !
   ! input namelist
   !
   NAMELIST /INPUT/ prefix, postfix, postfix_unfld, work_dir, datafile_dft, datafile_transl, &
                    translations, ndiv, debug_level, verbosity, nkb_buffer, nwfc_buffer, assume_ncpp, &
                    atmproj_sh, atmproj_thr, atmproj_nbnd, do_orthoovp, orb_mapping

   LOGICAL            :: lhave_transl
   !
   CHARACTER(nstrx)   :: translations_allowed(2)
   DATA  translations_allowed / 'from_file', 'from_scratch' /
   !
   CHARACTER(nstrx)   :: orb_mapping_allowed(3)
   DATA  orb_mapping_allowed / 'automatic', 'first', 'first_orbitals' /

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
      CALL datafiles_init( do_orthoovp )
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
      CALL do_unfold( postfix_unfld, ndiv, lhave_transl, datafile_transl, orb_mapping )

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
   USE io_module,            ONLY : ionode, ionode_id
   USE io_module,            ONLY : datafile_dft_ => dftdata_file
   USE control_module,       ONLY : read_pseudo, use_pseudo
   USE atmproj_tools_module, ONLY : atmproj_sh_ => atmproj_sh, &
                                    atmproj_thr_ => atmproj_thr, &
                                    atmproj_nbnd_ => atmproj_nbnd, &
                                    spin_component_atmproj => spin_component
   !
   IMPLICIT NONE

      CHARACTER(12)    :: subname = 'unfold_input'
      LOGICAL          :: lneed_wfc
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
      do_orthoovp                 = .FALSE.
      atmproj_sh                  = 5.0d0
      atmproj_thr                 = 0.9d0
      atmproj_nbnd                = 0
      orb_mapping                 = "first_orbitals"
 
      
      CALL input_from_file ( stdin )
      !
      ierr = 0
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
      CALL mp_bcast( atmproj_sh,      ionode_id )
      CALL mp_bcast( atmproj_thr,     ionode_id )
      CALL mp_bcast( atmproj_nbnd,    ionode_id )
      CALL mp_bcast( do_orthoovp,     ionode_id )
      CALL mp_bcast( orb_mapping,     ionode_id )

      !
      ! passing input vars to vars in io_module
      ! (this is done explicitly as a fix to a problem with gfortran)
      !
      datafile_dft_ = TRIM( datafile_dft )
      !datafile_sgm_ = TRIM( datafile_sgm )

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
      !
      IF ( atmproj_thr > 1.0d0 .OR. atmproj_thr < 0.0d0) &
                                  CALL errore(subname, 'invalid atmproj_thr', 10 )
      IF ( atmproj_nbnd < 0)      CALL errore(subname, 'invalid atmproj_nbnd', 10 )
      !
      IF ( TRIM(translations) == "from_scratch" ) lhave_transl = .FALSE.
      IF ( TRIM(translations) == "from_file" )    lhave_transl = .TRUE.
      !
      !
      use_pseudo   = .NOT. assume_ncpp
      read_pseudo  = .NOT. assume_ncpp

      !
      ! in case we need this
      ! pass atmproj_ vars to atmproj_tools_module
      !
      atmproj_sh_ = atmproj_sh
      atmproj_thr_ = atmproj_thr
      atmproj_nbnd_ = atmproj_nbnd




      !
      ! we don't need wfc if translations have been already calculated
      !
      IF ( lhave_transl ) THEN
          !
          lneed_wfc= .FALSE.
          !
      ELSE
          !
          lneed_wfc= .TRUE.
          !
      ENDIF
      !
      CALL io_init( lneed_wfc )


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
          WRITE( stdout, "(   7x,'           orb_mapping :',5x,   a)") TRIM(orb_mapping)
          !
          IF ( LEN_TRIM( datafile_dft ) /= 0 ) THEN
              WRITE( stdout,"(7x,'          datafile_dft :',5x,   a)") TRIM(datafile_dft)
              WRITE( stdout,"(7x,'       use ortho basis :',5x,   a)") TRIM( log2char(do_orthoovp) )
              WRITE( stdout,"(7x,'         atmproj shift :',5x,  f12.6)") atmproj_sh
              WRITE( stdout,"(7x,'          atmproj nbnd :',5x,   i5)") atmproj_nbnd
              WRITE( stdout,"(7x,'           atmproj thr :',5x,  f12.6)") atmproj_thr
          ENDIF
          !
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
   SUBROUTINE do_unfold( postfix_unfld, ndiv, lhave_transl, datafile_transl, orb_mapping )
   !********************************************************
   !
   ! perform the main task of the calculation
   !
   USE kinds
   USE constants,            ONLY : ZERO, CZERO, TWO, EPS_m6, EPS_m2
   USE parameters,           ONLY : nstrx, nkpts_inx
   USE io_module,            ONLY : stdout, stdin, io_name, ionode, ionode_id, ham_unit, sgm_unit
   USE io_module,            ONLY : work_dir, prefix, postfix
   USE mp,                   ONLY : mp_bcast, mp_sum
   USE mp_global,            ONLY : mpime, nproc
   USE grids_module,         ONLY : grids_get_rgrid
   USE files_module,         ONLY : file_open, file_close
   USE util_module,          ONLY : mat_hdiag, mat_herm, mat_mul
   USE converters_module,    ONLY : cry2cart
   USE lattice_module,       ONLY : avec, bvec, alat
   USE ions_module,          ONLY : nsp, nat, tau, ityp
   USE windows_module,       ONLY : nbnd, imin, imax, dimwin, dimwinx, nspin
   USE kpoints_module,       ONLY : nrtot, nkpts_g
   USE subspace_module,      ONLY : eamp
   USE localization_module,  ONLY : cu, rave
   USE hamiltonian_module,   ONLY : dimwann, rham, rovp, lhave_overlap
   USE translations_module,  ONLY : transl
   USE timing_module,        ONLY : timing
   USE log_module,           ONLY : log_push, log_pop
   USE atmproj_tools_module, ONLY : atmproj_get_index, atmproj_get_natomwfc
   USE uspp_param,           ONLY : upf
   USE datafiles_module,     ONLY : datafiles_fmt
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
      CHARACTER(*),  INTENT(IN) :: orb_mapping
   
      !
      ! local vars
      !
      CHARACTER(9)      :: subname = 'do_unfold'

      INTEGER           :: dimwann_unfld
      INTEGER           :: ndir, dir_map(3)
      INTEGER           :: ndimx_
      !
      INTEGER           :: nrtot_unfld, nr_unfld(3)
      INTEGER           :: nkpts_unfld, nk_unfld(3), s_unfld(3)
      REAL(dbl)         :: avec_unfld(3,3), bvec_unfld(3,3)
      REAL(dbl)         :: rvect_unfld(3,3)
      REAL(dbl)         :: ref_rave(3)
      !
      REAL(dbl),    ALLOCATABLE :: vkpt_unfld(:,:), vkpt_cry_unfld(:,:), vr_unfld(:,:)
      REAL(dbl),    ALLOCATABLE :: wk_unfld(:), wr_unfld(:)
      INTEGER,      ALLOCATABLE :: ivr_unfld(:,:)
      COMPLEX(dbl), ALLOCATABLE :: rham_unfld(:,:,:), rovp_unfld(:,:,:)
      COMPLEX(dbl), ALLOCATABLE :: work(:,:), work_ovp(:,:), trmat(:,:,:)
      COMPLEX(dbl), ALLOCATABLE :: kham_aux(:,:,:), kovp_aux(:,:,:)
      !
      INTEGER,      ALLOCATABLE :: orb_map(:), orb_map_tmp(:)
      REAL(dbl),    ALLOCATABLE :: rtmp(:)
      INTEGER,      ALLOCATABLE :: indx(:)
      INTEGER,      ALLOCATABLE :: natomwfc(:)
      REAL(dbl),      PARAMETER :: toll_dist = 5.0d0 * EPS_m2
      !
      CHARACTER(nstrx)  :: fileham, filespace
      CHARACTER(1)      :: op
      !
      INTEGER           :: i, j, k, ir, ik, ia, ind
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
      ALLOCATE( vkpt_cry_unfld(3,nkpts_unfld), STAT=ierr )
      IF ( ierr/=0 ) CALL errore(subname,"allocating vkpt_cry_unfld",ABS(ierr))
      ALLOCATE( wk_unfld(nkpts_unfld), STAT=ierr )
      IF ( ierr/=0 ) CALL errore(subname,"allocating wk_unfld",ABS(ierr))
      !
      ! generate the kpt grid
      ! vkpt_cry_unfld in crystal units
      !
      CALL monkpack( nk_unfld, s_unfld, vkpt_cry_unfld )
      wk_unfld(:) = 1.0_dbl/REAL( nkpts_unfld )
      !
      ! convert to cartesian units
      vkpt_unfld = vkpt_cry_unfld 
      CALL cry2cart( vkpt_unfld, bvec_unfld )
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
      ndimx_ = MAX( dimwann, dimwinx)
      CALL translations_drv( dimwann, ndimx_, ndir, rvect_unfld, lhave_transl, datafile_transl )
      !
      ! basis selection
      !
      ALLOCATE( orb_map(dimwann_unfld), STAT=ierr )
      IF ( ierr/=0 ) CALL errore(subname,"allocating orb_map",ABS(ierr))
      ALLOCATE( work(dimwann,dimwann), STAT=ierr )
      IF ( ierr/=0 ) CALL errore(subname,"allocating work",ABS(ierr))
      ALLOCATE( trmat(dimwann,dimwann,ndir), STAT=ierr )
      IF ( ierr/=0 ) CALL errore(subname,"allocating trmat",ABS(ierr))


      SELECT CASE ( TRIM(orb_mapping) )
      CASE ( "automatic" )
          !
          ALLOCATE( orb_map_tmp(dimwann), STAT=ierr )
          IF ( ierr/=0 ) CALL errore(subname,"allocating orb_map_tmp",ABS(ierr))
          !
          ALLOCATE( rtmp(dimwann), STAT=ierr )
          IF ( ierr/=0 ) CALL errore(subname,"allocating rtmp",ABS(ierr))
          !
          ALLOCATE( indx(dimwann), STAT=ierr )
          IF ( ierr/=0 ) CALL errore(subname,"allocating indx",ABS(ierr))
          !
          ! reconstruct center positions (rave) for atmproj orbitals 
          !
          IF ( datafiles_fmt == "atmproj" ) THEN
              !
              ALLOCATE( natomwfc(nsp), STAT=ierr )
              IF ( ierr/=0 ) CALL errore(subname,"allocating natomwfc",ABS(ierr))
              !
              CALL atmproj_get_natomwfc( nsp, upf(1:nsp), natomwfc )
              !
              DO ia = 1, nat
                  !
                  DO i = 1, natomwfc( ityp(ia) )
                      !
                      ind = atmproj_get_index( i, ia, ityp, natomwfc )
                      !
                      rave( 1:3, ind) = tau(1:3,ia) * alat
                      !
                  ENDDO
                  !
              ENDDO
              !
              DEALLOCATE( natomwfc, STAT=ierr)
              IF (ierr/=0) CALL errore(subname,"deallocating natomwfc",ABS(ierr))
              !
          ENDIF

          !
          ! take first wannier function ( wf )
          orb_map(1)=1
          !
          ref_rave(1:3)=rave(1:3,1)
          !
          ! compute distance of other wfs from it
          !
          do i=1,dimwann
             !
             rtmp(i) = SQRT ( DOT_PRODUCT( rave(:,i)-ref_rave(:), rave(:,i)-ref_rave(:) ) )
             indx(i) = i
             !
          enddo
          !
          ! this is to make sure the first wavefuntion will be the 
          ! "closest to itself" (other wfs might have the same center)
          !
          rtmp(1)=-1.d0
          !
          ! sort wf by center distance from first wf center
          !
          !write(6,*) "rtmp", rtmp(:), "f_index"
          !write(6,*) "index", indx(:), "f_index"
          !
          CALL hpsort_eps(dimwann, rtmp(:), indx(:), toll_dist )
          !
          !write(6,*) "index", indx(:), "f_index"
          !
          ! we now start from the identity matrix and write it 
          ! and its translated along all sublattice directions
          ! (treating it as a "hamiltonian" to unfold). IF we selected
          ! a good dimwann_unfld subset, we should have that we get no
          ! off-diagonal elements. As soon as we get an off-diagonal
          ! element, this tells us that one of the functions in the  
          !  subset is the translated of another, so we kill it
          !
          orb_map_tmp(:) = indx(:)
          !
          DO ir = 1, nrtot_unfld
              !
              !
              ! Initialize work to identity matrix... its translated 
              ! is the translation matrix itself
              !
              work(:,:) = 0.d0
              !
              DO j=1,dimwann
                  work(j,j)=1.d0
              ENDDO
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
                  ! nb 
                  DO k = 1, ABS( ivr_unfld( dir_map(j), ir) )
                      CALL mat_mul( trmat(:,:,j), trmat(:,:,j), 'N', transl(:,:,j), op, dimwann, dimwann, dimwann )
                  ENDDO
                  !
                  CALL mat_mul( work, work, 'N', trmat(:,:,j), 'N', dimwann, dimwann, dimwann )
                  !
              ENDDO
              !
              ! now we eliminate those wfs which are ir-translate of a
              ! previous one 
              !
              DO j = 1, dimwann-1
                  !
                  IF(orb_map_tmp(j) > 0) THEN
                      ! check only wf farther from wf 1 than j                 
                      DO i = j+1, dimwann
                          !
                          ! is wf i translated of wf j? 
                          ! (are they connected by a translation)
                          !
                          IF( ABS(work(indx(j),indx(i))) > EPS_m2 ) THEN
                              ! discard the wf
                              orb_map_tmp(i) = -1 
                              !
                          ENDIF
                          !
                      ENDDO
                  ENDIF
              ENDDO
          ENDDO
          !
          ! Now we should have selected the correct subset of wfs
          !
          k=2
          !
          DO j=2,dimwann
              !
              IF( orb_map_tmp(j) > 0 ) THEN
                  !
                  orb_map(k)=orb_map_tmp(j)
                  !
                  k=k+1
                  ! 
                  IF( k > dimwann_unfld ) EXIT
                  !
              ENDIF
              !
          ENDDO
          !
          !write(6,*) "this is orb_map", orb_map(:)
          !write(6,*) "this is orb_map_tmp", orb_map_tmp(:)
          !
          ! find the distance between wannier function one and all other wannier functions: 
          ! store distance in array of size (dimwann_unfld)
          ! compute the nearest and next nearest neighbors of wf N=1 by translation
          ! take the first wf closest to N=1 wf, check it is not a n or nn neighbor of wf N=1
          ! if not, call this wf N+1, set N=N+1 and goto 3
          ! if it is, skip this, go to next wf, goto 4
          !
          ! as an alternative, one can check the neighbors and next-nearest neighbors 
          ! by having operators T and T^2 in each direction at hand 
          ! (for 3d we have 36+6 operators... is it feasible?)
      
      CASE ( "first", "first_orbitals" )
          !
          DO i = 1, dimwann_unfld
               orb_map(i)=i
          ENDDO
          !
      CASE DEFAULT
          !
          CALL errore(subname,"invalid orb_mapping "//TRIM(orb_mapping), 10)
          !
      END SELECT
      !
      IF ( ionode ) THEN
          !
          WRITE(stdout, "(/,2x,'Orbital Mapping: ( ',a,' )')") TRIM(orb_mapping)
          WRITE(stdout, "(8i5)") orb_map(:)
          !
      ENDIF
       


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
      ALLOCATE( work_ovp(dimwann,dimwann), STAT=ierr )
      IF ( ierr/=0 ) CALL errore(subname,"allocating work_ovp",ABS(ierr))
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
              ! nb 
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
      ! since numerical noise in the calculation of transl may lead
      ! to non-hermitean Hamiltonians, here we force them to
      ! be hermietan by construction
      !
      ALLOCATE( kham_aux(dimwann_unfld,dimwann_unfld,nkpts_unfld), &
                kovp_aux(dimwann_unfld,dimwann_unfld,nkpts_unfld), STAT=ierr)
      IF ( ierr/=0 ) CALL errore(subname,"allocating kham_unfld, kovp_unfld")
      !
      DO ik = 1, nkpts_unfld
          !
          CALL compute_kham( dimwann_unfld, nrtot_unfld, vr_unfld, wr_unfld, rham_unfld, &
                             vkpt_unfld(:,ik), kham_aux(:,:,ik) ) 
          !
          CALL mat_herm( kham_aux(:,:,ik), dimwann_unfld )
          !
          IF ( lhave_overlap ) THEN
              !
              CALL compute_kham( dimwann_unfld, nrtot_unfld, vr_unfld, wr_unfld, rovp_unfld, &
                                 vkpt_unfld(:,ik), kovp_aux(:,:,ik) ) 
              !
              CALL mat_herm( kovp_aux(:,:,ik), dimwann_unfld )
              !
          ENDIF
          !
      ENDDO
      !
      DO ir = 1, nrtot_unfld
          !
          CALL compute_rham( dimwann_unfld, vr_unfld(:,ir), rham_unfld(:,:,ir), &
                             nkpts_unfld, vkpt_unfld, wk_unfld, kham_aux )
          !
          IF ( lhave_overlap ) THEN
              !
              CALL compute_rham( dimwann_unfld, vr_unfld(:,ir), rovp_unfld(:,:,ir), &
                                 nkpts_unfld, vkpt_unfld, wk_unfld, kovp_aux )
              !
          ENDIF
          !
      ENDDO 
      !
      DEALLOCATE( kovp_aux, kham_aux, STAT=ierr)
      IF ( ierr/=0 ) CALL errore(subname,"deallocating kovp_aux, kham_aux", ABS(ierr))



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
                             nkpts_unfld, nk_unfld, s_unfld, vkpt_cry_unfld, wk_unfld, &
                             nrtot_unfld, nr_unfld, ivr_unfld, wr_unfld, &
                             avec_unfld, bvec_unfld, lhave_overlap, 0.0_dbl, &
                             rham_unfld, rovp_unfld )

      !
      ! Clean local memory
      !
      DEALLOCATE( trmat, STAT=ierr)
      IF( ierr /=0 ) CALL errore(subname, 'deallocating trmat', ABS(ierr) )
      DEALLOCATE( vkpt_cry_unfld, vkpt_unfld, wk_unfld, STAT=ierr)
      IF( ierr /=0 ) CALL errore(subname, 'deallocating vkpt_cry_unfld, vkpt_unfld, wk_unfld', ABS(ierr) )
      DEALLOCATE( ivr_unfld, vr_unfld, wr_unfld, STAT=ierr)
      IF( ierr /=0 ) CALL errore(subname, 'deallocating ivr_unfld, vr_unfld, wr_unfld', ABS(ierr) )
      DEALLOCATE( rham_unfld, rovp_unfld, STAT=ierr)
      IF( ierr /=0 ) CALL errore(subname, 'deallocating rham_unfld, rovp_unfld', ABS(ierr) )
      DEALLOCATE( work, work_ovp, STAT=ierr)
      IF( ierr /=0 ) CALL errore(subname, 'deallocating work, work_ovp', ABS(ierr) )
      !
      DEALLOCATE( orb_map, STAT=ierr)
      IF( ierr /=0 ) CALL errore(subname, 'deallocating orb_map', ABS(ierr) )
      !
      IF(ALLOCATED(orb_map_tmp)) DEALLOCATE( orb_map_tmp, STAT=ierr)
      IF( ierr /=0 ) CALL errore(subname, 'deallocating orb_map_tmp', ABS(ierr) )
      IF(ALLOCATED(rtmp)) DEALLOCATE( rtmp, STAT=ierr)
      IF( ierr /=0 ) CALL errore(subname, 'deallocating rtmp', ABS(ierr) )
      IF(ALLOCATED(indx)) DEALLOCATE( indx, STAT=ierr)
      IF( ierr /=0 ) CALL errore(subname, 'deallocating indx', ABS(ierr) )
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
   USE hamiltonian_module,        ONLY : lhave_overlap, rovp
   USE localization_module,       ONLY : cu
   USE util_module,               ONLY : mat_mul
   USE datafiles_module,          ONLY : datafiles_fmt
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
      INTEGER        :: nwork
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
      if_compute: &
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
      ELSE if_compute
          !
          ! translation operators are computed (output on the WF basis)
          !
          IF ( ionode ) WRITE( stdout, "(2x,'Translation operators are computed')")
          !
          IF ( nkpts_g /= 1) CALL errore(subname,"invalid nkpts /= 1: not yet implemented",10)
          ik = 1
          !
          IF ( transl_alloc ) CALL errore(subname,"unexpected transl alloc",10)
          !
          CALL translations_allocate( ndimx, nvect )
          !
          rvect_mod(:,:) = rvect(:,:)
          ndim_mod       = ndim

          !
          ! exploit the atomic basis if present
          !
          if_algorithm: &
          IF ( TRIM(datafiles_fmt) == "atmproj" ) THEN
              !
              IF ( ionode ) WRITE( stdout, "(2x,'ATMPROJ algorithm used',/)")
              !
              ALLOCATE( work(ndimx,ndimx), STAT=ierr )
              IF ( ierr/=0 ) CALL errore(subname,"allocating work", ABS(ierr))
              !
              work = 0.0d0
              !IF ( lhave_overlap ) THEN
              !    work(1:dimwann,1:dimwann) = rovp(1:dimwann,1:dimwann,1)
              !ELSE
                  DO i = 1, ndimx
                      work(i,i) = 1.0d0
                  ENDDO
              !ENDIF
              !
              DO i = 1, nvect
                  !
                  CALL translations_calc_atmproj( rvect(:,i), dimwann, ndimx, work, transl(:,:,i) )
                  !
              ENDDO
              !
              DEALLOCATE( work, STAT=ierr )
              IF ( ierr/=0 ) CALL errore(subname,"allocating work", ABS(ierr))
              !
          ELSE 
              !
              IF ( ionode ) WRITE( stdout, "(2x,'WFs algorithm used',/)")
              !
              CALL wfc_drv( DO_PROJ=.FALSE., DO_OVP=.FALSE., DO_TRANSL=.TRUE. )
              !
              basis = "dft_wfc"

              !
              ! convert to the wannier basis
              !
              IF ( .NOT. subspace_alloc ) CALL errore(subname,"subspace not alloc",10)
              !
              ! there could be cases (atmproj) such that
              ! dimwann > dimwinx
              nwork=MAX(dimwinx, dimwann)
              !
              ALLOCATE( work(nwork, nwork), STAT=ierr )
              IF ( ierr/=0 ) CALL errore(subname,"allocatign work",ABS(ierr))
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
          ENDIF if_algorithm

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
      ENDIF if_compute
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


