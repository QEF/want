
! Copyright (C) 2006 WanT Group
!
! This file is distributed under the terms of the
! GNU General Public License. See the file `License'
! in the root directory of the present distribution,
! or http://www.gnu.org/copyleft/gpl.txt .
!
!=====================================================
   PROGRAM epsilon_main
   !=====================================================
   !  
   ! Interpolates the electronic structure from the knowledge of
   ! the direct lattice hamiltonian on Wannier function basis
   ! and compute the Dielectric Function (Epsilon)
   !
   !
   USE kinds
   USE version_module,       ONLY : version_number
   USE parameters,           ONLY : nstrx
   USE io_module,            ONLY : stdout, stdin
   USE io_module,            ONLY : prefix, postfix, work_dir
   USE control_module,       ONLY : debug_level, use_debug_mode, verbosity
   USE correlation_module,   ONLY : lhave_sgm
   USE timing_module,        ONLY : timing
   USE log_module,           ONLY : log_push, log_pop
   USE datafiles_module,     ONLY : datafiles_init
   USE parser_module,        ONLY : change_case, log2char
   USE want_interfaces_module

   !
   IMPLICIT NONE 

   !
   ! input variables
   !
   INTEGER          :: nk(3)         ! kpt generators
   INTEGER          :: s(3)          ! kpt shifts
   INTEGER          :: ne            ! dimension of the energy grid
   REAL(dbl)        :: emin          ! egrid extrema
   REAL(dbl)        :: emax
   REAL(dbl)        :: delta         ! smearing parameter
   REAL(dbl)        :: temperature   ! temperature in eV for conductivity calculation
   REAL(dbl)        :: shift         ! shift energy grid
   REAL(dbl)        :: scale         ! scale resulting dos
   CHARACTER(nstrx) :: smearing_type
   CHARACTER(nstrx) :: fileout       ! output filename for real part of epsilon
   CHARACTER(nstrx) :: fileout2      ! output filename for imaginary part of epsilon
   CHARACTER(nstrx) :: fileout3      ! output filename for eels function
   CHARACTER(nstrx) :: datafile_dft  !
   CHARACTER(nstrx) :: datafile_sgm  !
   INTEGER          :: ircut(3)      ! real space curoff in terms of unit cells
                                     ! for directions i=1,2,3  (0 means no cutoff)
   INTEGER          :: nprint        ! print every "nprint" iterations
   REAL(dbl)        :: atmproj_sh    ! shifthing: energy shift when computing the proj Hamiltonian
   REAL(dbl)        :: atmproj_thr   ! filtering: thr on projections 
   INTEGER          :: atmproj_nbnd  ! filtering: # of bands
   LOGICAL          :: do_orthoovp
   CHARACTER(nstrx) :: spin_component
   LOGICAL          :: atmproj_do_norm
   !
   ! input namelist
   !
   NAMELIST /INPUT/ prefix, postfix, work_dir, datafile_dft, datafile_sgm, &
                    nk, s, delta, temperature, smearing_type, fileout,     &
                    fileout2, fileout3,                                    &  
                    emin, emax, ne, ircut, nprint, verbosity,              &
                    shift, scale, do_orthoovp, atmproj_sh, atmproj_thr,    &
                    atmproj_nbnd, spin_component, atmproj_do_norm
                    
   !
   ! end of declarations
   !   

!
!------------------------------
! main body
!------------------------------
!
      CALL startup(version_number,'epsilon')

      !
      ! read input
      !
      CALL epsilon_input( )

      !
      ! init post processing (reading previous WanT and DFT data )
      !
      CALL write_header( stdout, "Post Processing Init" )
      !
      CALL datafiles_init( do_orthoovp )
      !
      CALL postproc_init ( )

      !
      ! print data to output
      !
      CALL summary( stdout, INPUT=.FALSE., IONS=.FALSE., WINDOWS=.FALSE. )

      !
      ! do the main task
      !
      CALL do_epsilon( fileout, fileout2, fileout3, nk, s, delta,  &
                   temperature, smearing_type, emin, emax, ne, &
                   shift, scale, ircut,                        &
                   nprint, verbosity )
      !
      ! clean global memory
      !
      CALL cleanup()

      !
      ! finalize
      !
      CALL shutdown( 'epsilon' )

CONTAINS

!********************************************************
   SUBROUTINE epsilon_input()
   !********************************************************
   !
   ! Read INPUT namelist from stdin
   !
   USE mp,                   ONLY : mp_bcast
   USE io_module,            ONLY : io_init, ionode, ionode_id
   USE io_module,            ONLY : datafile_dft_ => dftdata_file, datafile_sgm_ => datafile_sgm
   USE atmproj_tools_module, ONLY : atmproj_sh_ => atmproj_sh, &
                                    atmproj_thr_ => atmproj_thr, &
                                    atmproj_nbnd_ => atmproj_nbnd, &
                                    spin_component_atmproj => spin_component, &
                                    atmproj_do_norm_ => atmproj_do_norm
   !
   IMPLICIT NONE

      CHARACTER(9)     :: subname = 'epsilon_input'
      INTEGER          :: ierr, nkpts_int
      !
      ! end of declarations
      !


      CALL timing( subname, OPR='start' )

      !
      ! init input namelist
      !
      prefix                      = 'WanT' 
      postfix                     = ' ' 
      work_dir                    = './' 
      datafile_sgm                = ' '
      datafile_dft                = ' '
      fileout                     = ' '
      fileout2                    = ' '
      fileout3                    = ' '
      delta                       = 0.1    ! eV
      temperature                 = 0.025852    ! eV, room temperature
      nk(:)                       = -1
      s(:)                        =  0
      emin                        = -10.0
      emax                        =  10.0
      ne                          =  1000
      shift                       = 0.0
      scale                       = 1.0
      smearing_type               = 'gaussian'
      ircut(1:3)                  =  0
      nprint                      = 50
      debug_level                 = 0
      verbosity                   = 'medium'
      do_orthoovp                 = .FALSE.
      atmproj_sh                  = 5.0d0
      atmproj_thr                 = 0.9d0
      atmproj_nbnd                = 0
      spin_component              = 'all'
      atmproj_do_norm             = .FALSE.
      
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
      CALL mp_bcast( work_dir,        ionode_id )
      CALL mp_bcast( datafile_sgm,    ionode_id )
      CALL mp_bcast( datafile_dft,    ionode_id )
      CALL mp_bcast( fileout,         ionode_id )
      CALL mp_bcast( fileout2,        ionode_id )
      CALL mp_bcast( fileout3,        ionode_id )
      CALL mp_bcast( delta,           ionode_id )
      CALL mp_bcast( temperature,     ionode_id )
      CALL mp_bcast( nk,              ionode_id )
      CALL mp_bcast( s,               ionode_id )
      CALL mp_bcast( emin,            ionode_id )
      CALL mp_bcast( emax,            ionode_id )
      CALL mp_bcast( ne,              ionode_id )
      CALL mp_bcast( shift,           ionode_id )
      CALL mp_bcast( scale,           ionode_id )
      CALL mp_bcast( smearing_type,   ionode_id )
      CALL mp_bcast( ircut,           ionode_id )      
      CALL mp_bcast( nprint,          ionode_id )      
      CALL mp_bcast( debug_level,     ionode_id )      
      CALL mp_bcast( verbosity,       ionode_id )      
      CALL mp_bcast( do_orthoovp,     ionode_id )
      CALL mp_bcast( atmproj_sh,      ionode_id )
      CALL mp_bcast( atmproj_thr,     ionode_id )
      CALL mp_bcast( atmproj_nbnd,    ionode_id )      
      CALL mp_bcast( spin_component,  ionode_id )
      CALL mp_bcast( atmproj_do_norm,  ionode_id )
      !
      ! passing input vars to vars in io_module
      ! (this is done explicitly as a fix to a problem with gfortran)
      !
      datafile_dft_ = TRIM( datafile_dft )
      datafile_sgm_ = TRIM( datafile_sgm )

      !
      ! Init
      !
      IF ( LEN_TRIM(fileout) == 0 ) &
           fileout = TRIM(work_dir)//'/'//TRIM(prefix)//TRIM(postfix)//'_epsi.dat'
      !
      IF ( LEN_TRIM(fileout2) == 0 ) &
           fileout2 = TRIM(work_dir)//'/'//TRIM(prefix)//TRIM(postfix)//'_epsr.dat'
      !
      IF ( LEN_TRIM(fileout3) == 0 ) &
           fileout3 = TRIM(work_dir)//'/'//TRIM(prefix)//TRIM(postfix)//'_eels.dat'
      !
      use_debug_mode = .FALSE.
      IF ( debug_level > 0  )     use_debug_mode = .TRUE.
      !
      lhave_sgm = .FALSE.
      IF ( LEN_TRIM(datafile_sgm) > 0 ) lhave_sgm = .TRUE.
      !
      CALL io_init( NEED_WFC=.FALSE. )


      !
      ! Some checks 
      !
      IF ( ANY( nk(:) <=0 ) )     CALL errore(subname, 'Invalid nk', 1)
      IF ( ANY( s(:)  < 0 ) )     CALL errore(subname, 'Invalid s', 1)
      IF ( ANY( s(:)  > 1 ) )     CALL errore(subname, 'Invalid s', 2)
      IF ( delta <= 0.0 )         CALL errore(subname, 'Invalid delta', 3)
      IF ( temperature <= 0.0 )   CALL errore(subname, 'Invalid temperature', 3)
      IF ( emin > emax  )         CALL errore(subname, 'emin larger than emax', 4)
      IF ( ne <= 0  )             CALL errore(subname, 'Invalid ne', 4)
      IF ( ANY( ircut(:) < 0 ) )  CALL errore(subname, 'Invalid ircut', 10)
      IF ( nprint <= 0  )         CALL errore(subname, 'invalid nprint', 5)
      IF ( scale  <= 0.0 )        CALL errore(subname, 'invalid scale', 5)
      !
      IF ( atmproj_thr > 1.0d0 .OR. atmproj_thr < 0.0d0) &
                                  CALL errore(subname, 'invalid atmproj_thr', 10 )
      IF ( atmproj_nbnd < 0)      CALL errore(subname, 'invalid atmproj_nbnd', 10 )
      !
      nkpts_int = PRODUCT( nk(1:3) )
      IF ( ANY( nk(:) <= 0 ) ) CALL  errore(subname, 'invalid nk', 71) 
      !
      ! the check of SMEARING_TYPE is done inside the function smearing_func
      ! just move to lower_case
      !
      CALL change_case(smearing_type,'lower')
      !
      CALL change_case(spin_component,'lower')
      spin_component_atmproj = spin_component

      !
      ! in case we need this
      ! pass atmproj_ vars to atmproj_tools_module
      !
      atmproj_sh_ = atmproj_sh
      atmproj_thr_ = atmproj_thr
      atmproj_nbnd_ = atmproj_nbnd
      atmproj_do_norm_ = atmproj_do_norm

      !
      ! input summary
      !
      CALL write_header( stdout, "INPUT Summary" )
      !
      IF ( ionode ) THEN
          !
          WRITE( stdout, "(   7x,'               fileout :',5x,   a)") TRIM(fileout)
          WRITE( stdout, "(   7x,'               fileout2 :',5x,   a)") TRIM(fileout2)
          WRITE( stdout, "(   7x,'               fileout3 :',5x,   a)") TRIM(fileout3)
          WRITE( stdout, "(   7x,'                  type :',5x,   a)") TRIM(smearing_type)
          WRITE( stdout, "(   7x,'        spin component :',5x,   a)") TRIM(spin_component)
          WRITE( stdout, "(   7x,'                 delta :',3x, f9.5, ' eV')" ) delta
          WRITE( stdout, "(   7x,'                 temperature :',3x, f9.5, ' eV')" ) temperature
          WRITE( stdout, "(   7x,'                    nk :',3x,3i4 )") nk(:)
          WRITE( stdout, "(   7x,'                     s :',3x,3i4 )") s(:)
          WRITE( stdout, "(   7x,'                 nktot :',5x,i6  )") nkpts_int
          WRITE( stdout, "(   7x,'                  emin :',3x,f8.3 )") emin 
          WRITE( stdout, "(   7x,'                  emax :',3x,f8.3 )") emax 
          WRITE( stdout, "(   7x,'                    ne :',3x,i6 )") ne
          WRITE( stdout, "(   7x,'                 shift :',3x,f8.3 )") shift
          WRITE( stdout, "(   7x,'                 scale :',3x,f8.3 )") scale
          WRITE( stdout, "(   7x,'                nprint :',3x,i6 )") nprint
          !
          IF ( ANY( ircut(:) > 0 ) ) THEN
              WRITE( stdout,"(7x,'                 ircut :',3x,3i4)") ircut(:)
          ENDIF
          !
          IF ( LEN_TRIM( datafile_dft ) /=0 ) THEN
              WRITE( stdout,"(7x,'          DFT datafile :',5x,   a)") TRIM( datafile_dft )
              WRITE( stdout,"(7x,'       use ortho basis :',5x,   a)") TRIM( log2char(do_orthoovp) )
              WRITE( stdout,"(7x,'         atmproj shift :',5x,  f12.6)") atmproj_sh
              WRITE( stdout,"(7x,'          atmproj nbnd :',5x,   i5)") atmproj_nbnd
              WRITE( stdout,"(7x,'           atmproj thr :',5x,  f12.6)") atmproj_thr
              WRITE( stdout,"(7x,'       atmproj do_norm :',5x,  L)") atmproj_do_norm
          ENDIF
          !
          WRITE( stdout, "(   7x,'            have sigma :',5x, a  )") TRIM( log2char(lhave_sgm) )
          IF ( lhave_sgm ) THEN
              WRITE( stdout,"(7x,'        sigma datafile :',5x,   a)") TRIM( datafile_sgm )
          ENDIF
!          WRITE( stdout, "(   7x,'            have overlaps :',5x, a  )") TRIM( log2char(lhave_overlap) )
          !
      ENDIF

      CALL timing(subname,OPR='stop')
      !
   END SUBROUTINE epsilon_input
   !
END PROGRAM epsilon_main


!********************************************************
   SUBROUTINE do_epsilon( fileout, fileout2, fileout3, nk, s, delta, temperature, smearing_type, &
                      emin, emax, ne, shift, scale, ircut,                         &
                      nprint, verbosity )
   !********************************************************
   !
   ! perform the main task of the calculation
   !
   USE kinds
   USE parameters,           ONLY : nstrx
   USE constants,            ONLY : CZERO, ZERO, ONE, CI, TWO, FOUR, PI, TPI,SQRTPI, EPS_m4, EPS_m6, EPS_m8
   USE constants,            ONLY : H_OVER_TPI, K_BOLTZMAN_SI, ELECTRONVOLT_SI, BOHR_RADIUS_SI, K_BOLTZMAN_SI
   USE constants,            ONLY : evtory, eps0, e2
   USE io_module,            ONLY : stdout, stdin, ionode, ionode_id, aux_unit, sgm_unit
   USE io_module,            ONLY : work_dir, prefix, postfix
   USE io_module,            ONLY : datafile_sgm
   USE mp,                   ONLY : mp_bcast, mp_sum
   USE mp_global,            ONLY : mpime, nproc
   USE files_module,         ONLY : file_open, file_close
   USE util_module,          ONLY : mat_hdiag, mat_herm, mat_is_herm, mat_mul, mat_inv
   USE converters_module,    ONLY : cry2cart, cart2cry
   USE lattice_module,       ONLY : avec, bvec, omega
   USE kpoints_module,       ONLY : nrtot, vr, wr 
   USE windows_module,       ONLY : nspin
   USE smearing_base_module, ONLY : smearing_func
   USE hamiltonian_module,   ONLY : dimwann, rham, rovp, lhave_overlap
   USE correlation_module,   ONLY : lhave_sgm, ldynam_sgm, rsgm, correlation_allocate
   USE correlation_module,   ONLY : omg_grid, omg_nint
   USE timing_module,        ONLY : timing, timing_upto_now
   USE log_module,           ONLY : log_push, log_pop
   USE parser_module,        ONLY : change_case, int2char
   USE dyson_solver_module,  ONLY : dyson_solver
   USE operator_module
   !
   IMPLICIT NONE

      !
      ! input vars
      !
      CHARACTER(*),  INTENT(IN)    :: fileout
      CHARACTER(*),  INTENT(IN)    :: fileout2
      CHARACTER(*),  INTENT(IN)    :: fileout3
      INTEGER,       INTENT(IN)    :: nk(3), s(3)
      REAL(dbl),     INTENT(IN)    :: delta
      REAL(dbl),     INTENT(IN)    :: temperature
      INTEGER,       INTENT(INOUT) :: ne
      REAL(dbl),     INTENT(INOUT) :: emin, emax
      REAL(dbl),     INTENT(IN)    :: shift, scale
      CHARACTER(*),  INTENT(IN)    :: smearing_type
      INTEGER,       INTENT(IN)    :: ircut(3)
      INTEGER,       INTENT(IN)    :: nprint
      CHARACTER(*),  INTENT(IN)    :: verbosity

      !
      ! local vars
      !
      CHARACTER(6) :: subname = 'do_epsilon'
      !
      INTEGER      :: nkpts_int     ! Number of interpolated k-points
      INTEGER      :: nrtot_nn
      LOGICAL      :: lhave_nn(3)
      REAL(dbl)    :: arg, arg2, arg3, cost, raux, raux2, raux3, efermi, x0(3), FSUM
      COMPLEX(dbl) :: ze
      !
      INTEGER,      ALLOCATABLE :: r_index(:), ind_plot(:)
      COMPLEX(dbl), ALLOCATABLE :: kham(:,:), rham_nn(:,:,:)
      COMPLEX(dbl), ALLOCATABLE :: work(:,:), work2(:,:), work3(:,:), gradkham(:,:,:)!, hessiankham(:,:,:,:) 
      COMPLEX(dbl), ALLOCATABLE :: aux1(:,:,:), aux2(:,:,:) 
      COMPLEX(dbl), ALLOCATABLE :: gradkovp(:,:,:)
      COMPLEX(dbl), ALLOCATABLE :: ksgm(:,:), rsgm_nn(:,:,:)
      COMPLEX(dbl), ALLOCATABLE :: kovp(:,:), rovp_nn(:,:,:)
      COMPLEX(dbl), ALLOCATABLE :: z(:,:,:), zc(:,:,:)
      COMPLEX(dbl), ALLOCATABLE :: GF(:,:), GF0(:,:)
      COMPLEX(dbl), ALLOCATABLE :: momentum(:,:,:,:)
!      REAL(dbl),    ALLOCATABLE :: vel(:,:,:), hessian(:,:,:,:), inv_MASS(:,:,:,:)
      REAL(dbl),    ALLOCATABLE :: IM_EPSILON(:,:,:), RE_EPSILON(:,:,:), EELS(:,:,:)
      REAL(dbl),    ALLOCATABLE :: egrid(:)
      REAL(dbl),    ALLOCATABLE :: dos(:), dos0(:), pdos(:,:)
      REAL(dbl),    ALLOCATABLE :: vkpt_int(:,:), vkpt_int_cry(:,:), wk(:)
      REAL(dbl),    ALLOCATABLE :: eig_int(:,:)
      REAL(dbl),    ALLOCATABLE :: eig_band(:,:,:,:), eig_coll(:,:)
      REAL(dbl),    ALLOCATABLE :: vr_cry(:,:), vr_nn(:,:), wr_nn(:), vr_sgm(:,:)
      CHARACTER(nstrx)          :: filename, filename2, filename3, analyticity_sgm, aux_fmt
      CHARACTER(4)              :: ctmp
      !
      INTEGER      :: iks, ike
      INTEGER      :: i, j, k, ie, ie2, ik, ik_g, ir, ib, l, m, n 
      INTEGER      :: ierr, icount, nbndx_plot
      INTEGER      :: dimwann_sgm, nrtot_sgm
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

      CALL write_header( stdout, "Epsilon computation using Wannier Functions" )
      CALL flush_unit( stdout )


      !
      ! if required, get data from sgm datafile
      !
      ldynam_sgm = .FALSE.
      !
      IF ( lhave_sgm ) THEN
          !
          CALL file_open(sgm_unit, TRIM(datafile_sgm), PATH="/", ACTION="read", IERR=ierr)
          IF ( ierr/=0 ) CALL errore(subname,'opening '//TRIM(datafile_sgm), ABS(ierr) )
          !
          IF ( ionode ) THEN
              !
              CALL operator_read_aux( sgm_unit, DIMWANN=dimwann_sgm, NR=nrtot_sgm,        &
                                                DYNAMICAL=ldynam_sgm, NOMEGA=omg_nint,    &
                                                ANALYTICITY=analyticity_sgm, IERR=ierr )
                                                !
              IF ( ierr/=0 ) CALL errore(subname,'reading DIMWANN--ANALYTICITY', ABS(ierr) )
              !
          ENDIF
          !
          CALL mp_bcast( dimwann_sgm,      ionode_id )
          CALL mp_bcast( nrtot_sgm,        ionode_id )
          CALL mp_bcast( ldynam_sgm,       ionode_id )
          CALL mp_bcast( omg_nint,         ionode_id )
          CALL mp_bcast( analyticity_sgm,  ionode_id )


          !
          ! few checks
          !
          CALL change_case( analyticity_sgm, 'lower' )
          IF ( TRIM(analyticity_sgm) /= 'retarded' .AND. ldynam_sgm )  &
                       CALL errore(subname,'invalid analyticity = '//TRIM(analyticity_sgm),1)

          IF ( dimwann_sgm /= dimwann ) CALL errore(subname,'invalid dimwann_sgm',1)
          IF ( nrtot_sgm /= nrtot )     CALL errore(subname,'invalid nrtot_sgm',1)
          ! 
          ALLOCATE( vr_sgm( 3, nrtot ), STAT=ierr )
          IF ( ierr /=0 ) CALL errore(subname,'allocating vr_sgm', ABS(ierr) ) 
          !
          ! 
          CALL correlation_allocate( )
          !
          IF ( ldynam_sgm ) THEN 
              !
              IF ( ionode ) THEN
                  !
                  CALL operator_read_aux( sgm_unit, VR=vr_sgm, GRID=omg_grid, IERR=ierr )
                  IF ( ierr/=0 ) CALL errore(subname,'reading VR, GRID', ABS(ierr) )
                  !
              ENDIF
              !
              CALL mp_bcast( vr_sgm,    ionode_id )
              CALL mp_bcast( omg_grid,  ionode_id )
              !
          ELSE
              !
              IF ( ionode ) THEN
                  !
                  CALL operator_read_aux( sgm_unit, VR=vr_sgm, IERR=ierr )
                  IF ( ierr/=0 ) CALL errore(subname,'reading VR', ABS(ierr) )
                  !
              ENDIF
              !
              CALL mp_bcast( vr_sgm,    ionode_id )
              !
          ENDIF
 
          ! 
          ! here we check that the order of R vectors from file is 
          ! the same of VR
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
      ENDIF

      !
      ! furhter checks on non-implemented special cases
      !
      IF ( ldynam_sgm )      CALL errore(subname,'projdos and dyn-sigma NOT impl.', 10)
      IF ( ldynam_sgm ) CALL errore(subname,'fermi surf and dyn-sigma NOT impl.', 10)


      !
      ! setting energy grid
      !
      IF ( ldynam_sgm ) THEN
         !
         CALL warning(subname, 'energy grid is forced from SGM datafile' )
         IF( ionode ) WRITE( stdout, '()')
         !
         ne = omg_nint
         !
         emin = omg_grid( 1 )
         emax = omg_grid( omg_nint )
         !
         ALLOCATE( egrid( ne ), STAT=ierr )
         IF( ierr /=0 ) CALL errore(subname, 'allocating egrid', ABS(ierr) )
         !
         egrid(:) = omg_grid(:)
         !
      ELSE
         !
         ALLOCATE( egrid( ne ), STAT=ierr )
         IF( ierr /=0 ) CALL errore(subname, 'allocating egrid', ABS(ierr) )
         !
         DO ie = 1, ne
            egrid (ie) = emin + REAL(ie-1, dbl) * (emax-emin) / REAL( ne -1, dbl)
         ENDDO
         !
      ENDIF

      !
      ! local workspace
      !
      nkpts_int = PRODUCT( nk )
      !
      !
      ALLOCATE( vkpt_int( 3, nkpts_int ), wk( nkpts_int ), STAT=ierr )
      IF( ierr /=0 ) CALL errore(subname, 'allocating vkpt_int, wk', ABS(ierr) )
      !
      ! generate monkhorst-pack grid, using nk(:) and s(:)
      ! kpts gen are in crystal coords
      !
      CALL monkpack( nk, s, vkpt_int )
      !
      wk(1:nkpts_int) = ONE / REAL( nspin * nkpts_int ,dbl)
      !
      ! mv kpts in cartesian coords (bohr^-1)
      !
      CALL cry2cart( vkpt_int, bvec )


      !
      ! setup parallelism
      !
      CALL divide_et_impera( 1, nkpts_int, iks, ike, mpime, nproc )      


      !
      ! kpt summary
      !
      IF ( ionode ) THEN
          !
          WRITE( stdout, "(2x, 'nktot = ',i5 ) " ) nkpts_int
          WRITE( stdout, "(2x, 'Monkhorst-Pack grid:      nk = (',3i4,' ),', &
                               & 6x,'shift = (',3i4,' )' ) " ) nk(:), s(:)
          WRITE( stdout, "(2x, 'Generated kpt mesh: (cart. coord. in Bohr^-1)',/)" )
          !
          DO ik=1,nkpts_int
              !
              WRITE( stdout, " (4x, 'k (', i5, ') =    ( ',3f9.5,' ),   &
                               & weight = ', f11.7 )") ik, ( vkpt_int(i,ik), i=1,3 ), wk(ik)
          ENDDO
          !
          WRITE( stdout, "()" )
          !
          IF ( TRIM(verbosity) == 'high' ) THEN
              !
              ALLOCATE( vkpt_int_cry(3,nkpts_int) )
              !
              vkpt_int_cry = vkpt_int
              CALL cart2cry( vkpt_int_cry, bvec)
              ! 
              WRITE( stdout, "(2x, 'Generated kpt mesh: (crystal units)',/)" )
              DO ik=1,nkpts_int
                  !
                  WRITE( stdout, " (4x, 'k (', i5, ') =    ( ',3f9.5,' ),   &
                               & weight = ', f11.7 )") ik, ( vkpt_int_cry(i,ik), i=1,3 ), wk(ik)
              ENDDO
              !
              WRITE( stdout, "()" )
              DEALLOCATE( vkpt_int_cry )
              !        
          ENDIF
          !
      ENDIF
      !
      IF ( lhave_overlap ) THEN
          WRITE( stdout, " ('l have overlap') " ) 
          !
      ELSE 
          WRITE( stdout, " ('l does not have overlap') " )  
      ENDIF

      !
      ! Determine the k-points used in the band structure interpolation
      !
      ALLOCATE( kham( dimwann, dimwann ), STAT=ierr )
      IF( ierr /=0 ) CALL errore(subname,'allocating kham', ABS(ierr) )
      !
      IF ( lhave_sgm ) THEN
          ALLOCATE( ksgm(dimwann, dimwann), STAT=ierr )
          IF ( ierr/=0 ) CALL errore(subname,'allocating ksgm',ABS(ierr))
      ENDIF
      !
      IF ( lhave_overlap ) THEN
          WRITE( stdout, " ('l have overlap') " ) 
          ALLOCATE( kovp(dimwann, dimwann), STAT=ierr )
          IF ( ierr/=0 ) CALL errore(subname,'allocating kovp',ABS(ierr))
      ENDIF
      !
      ALLOCATE( z( dimwann, dimwann, ike-iks+1 ), STAT=ierr )
      IF( ierr /=0 ) CALL errore(subname, 'allocating z', ABS(ierr) )
      !
      ALLOCATE( eig_int( dimwann, ike-iks+1 ), STAT=ierr )
      IF( ierr /=0 ) CALL errore(subname,'allocating eig_int', ABS(ierr) )
      !

      !
      ! if chosen from input, select only R corresponding to nearest-neighb
      ! along some (crystal) directions
      ! nn_index will point only to the selected nrtot_nn vectors
      !
      ALLOCATE( vr_cry(3, nrtot), STAT=ierr )
      IF( ierr /=0 ) CALL errore(subname,'allocating vr_cry', ABS(ierr) )
      !
      ALLOCATE( r_index(nrtot), STAT=ierr )
      IF( ierr /=0 ) CALL errore(subname,'allocating r_index', ABS(ierr) )

      !
      ! find the real space R-vectors to be used (according to ircut)
      ! move vr_int to crystal coords
      !
      lhave_nn(:) = ( ircut(:) /= 0 )
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
                ( .NOT. lhave_nn(3) .OR.  ABS(NINT(vr_cry(3,ir))) <= ircut(3) ) )  THEN
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
      ALLOCATE( rham_nn( dimwann,dimwann, nrtot_nn ), STAT=ierr )
      IF( ierr /=0 ) CALL errore(subname,'allocating rham_nn', ABS(ierr) )
      !
      IF ( lhave_sgm ) THEN
          ALLOCATE( rsgm_nn(dimwann, dimwann, nrtot_nn), STAT=ierr )
          IF ( ierr/=0 ) CALL errore(subname,'allocating rsgm_nn',ABS(ierr))
      ENDIF
      !
      IF ( lhave_overlap ) THEN
          ALLOCATE( rovp_nn(dimwann, dimwann, nrtot_nn), STAT=ierr )
          IF ( ierr/=0 ) CALL errore(subname,'allocating rovp_nn',ABS(ierr))
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
          !
      ENDDO


      !
      ! Interpolate H_ij(k') at those k-points by fourier interpolation
      ! H_ij(k') ~ sum_R e^{ik'R} H_ij(R), where the sum over R is over a 
      ! finite grid (truncation)
      !
      IF ( lhave_overlap ) THEN
          !
          ALLOCATE( zc( dimwann, dimwann, ike-iks+1), STAT=ierr )
          IF ( ierr/=0 ) CALL errore(subname, 'allocating zc', ABS(ierr) )
          !
      ENDIF
      IF ( .NOT. lhave_sgm  .OR. .NOT. ldynam_sgm ) THEN

          !
          ! read static self-energy if the case
          !
          IF ( lhave_sgm ) THEN
              !
              IF ( ionode ) THEN
                  !
                  CALL operator_read_data( sgm_unit, R_OPR=rsgm, IERR=ierr )
                  IF ( ierr/=0 ) CALL errore(subname,'reading static rsgm', 11)
                  !
              ENDIF
              !
              CALL mp_bcast( rsgm,  ionode_id )
              !
              DO ir = 1, nrtot_nn
                  rsgm_nn( :, :, ir ) = rsgm( :, :, r_index(ir) )
              ENDDO
              !
          ENDIF

          !
          !
          z( :, :, : )    = CZERO
          eig_int( :, : ) = ZERO
          !
          ! local workspace
          !
          ALLOCATE( work( dimwann, dimwann ), STAT=ierr )
          IF( ierr /=0 ) CALL errore(subname,'allocating work', ABS(ierr) )
          !
          ALLOCATE( work2( dimwann, dimwann ), STAT=ierr )
          IF( ierr /=0 ) CALL errore(subname,'allocating work2', ABS(ierr) )
          !
          ALLOCATE( work3( dimwann, dimwann ), STAT=ierr )
          IF( ierr /=0 ) CALL errore(subname,'allocating work3', ABS(ierr) )
          !
          ALLOCATE( gradkham( dimwann, dimwann, 3 ), STAT=ierr )
          IF( ierr /=0 ) CALL errore(subname,'allocating gradkham', ABS(ierr) )
          !
!          ALLOCATE( hessiankham( dimwann, dimwann, 3, 3 ), STAT=ierr )
!          IF( ierr /=0 ) CALL errore(subname,'allocating hessiankham', ABS(ierr) )
          !
          ALLOCATE( gradkovp( dimwann, dimwann, 3 ), STAT=ierr )
          IF( ierr /=0 ) CALL errore(subname,'allocating gradkovp', ABS(ierr) )
          !
          ALLOCATE( aux1( dimwann, dimwann, 3 ), STAT=ierr )
          IF( ierr /=0 ) CALL errore(subname,'allocating aux1', ABS(ierr) )
          !
          ALLOCATE( aux2( dimwann, dimwann, 3 ), STAT=ierr )
          IF( ierr /=0 ) CALL errore(subname,'allocating aux2', ABS(ierr) )
          !
!          ALLOCATE( vel( 3, dimwann, ike - iks + 1 ), STAT=ierr )
!          IF( ierr /=0 ) CALL errore(subname,'allocating vel', ABS(ierr) )
!          !
!          ALLOCATE( hessian( 3, 3, dimwann, ike - iks + 1 ), STAT=ierr )
!          IF( ierr /=0 ) CALL errore(subname,'allocating hessian', ABS(ierr) )
!          !
!          ALLOCATE( inv_MASS( 3, 3, dimwann, ike - iks + 1 ), STAT=ierr )
!          IF( ierr /=0 ) CALL errore(subname,'allocating inv_MASS', ABS(ierr) )
!          !
          ALLOCATE( momentum( 3, dimwann, dimwann, ike - iks + 1 ), STAT=ierr )
          IF( ierr /=0 ) CALL errore(subname,'allocating momentum', ABS(ierr) )
          !
          kpt_loop: &
          DO ik_g = iks, ike
              !
              ik = ik_g -iks +1
              !
              ! compute the Hamiltonian with the correct bloch symmetry
              !
              CALL compute_kham( dimwann, nrtot_nn, vr_nn, wr_nn, rham_nn,  &
                                 vkpt_int(:,ik_g), kham)

              IF ( .NOT. mat_is_herm(dimwann, kham, TOLL=EPS_m8) ) &
                  CALL errore(subname,'kham not herm',ik)

              IF ( lhave_overlap ) THEN
                  !
                  CALL compute_kham( dimwann, nrtot_nn, vr_nn, wr_nn, rovp_nn,  &
                                     vkpt_int(:,ik_g), kovp(:,:))
                  !
                  IF ( .NOT. mat_is_herm(dimwann, kovp, TOLL=EPS_m8) ) &
                      CALL errore(subname,'kovp not herm',ik)
                  !
              ENDIF

              IF ( lhave_sgm ) THEN
                  !
                  CALL compute_kham( dimwann, nrtot_nn, vr_nn, wr_nn, rsgm_nn,  &
                                     vkpt_int(:,ik_g), ksgm)
                  !
                  ! symmetryze the static sgm in order to make it hermitean
                  !
                  CALL mat_herm( ksgm, dimwann )
                  ! 
                  kham(:,:) = kham(:,:) + ksgm(:,:)
                  !
              ENDIF


              !
              ! Diagonalize the hamiltonian at the present k-point
              ! taking care of the overlaps if the case
              !
              CALL log_push("mat_hdiag")
              IF ( .NOT. lhave_overlap ) THEN
                  !
                  CALL mat_hdiag( z(:,:,ik), eig_int(:,ik), kham(:,:), dimwann)
                  !
              ELSE
                  !
                  CALL mat_hdiag( z(:,:,ik), eig_int(:,ik), kham(:,:), kovp(:,:), dimwann)
!                  DO i  = 1, dimwann 
!                      !
!                      raux = REAL( DOT_PRODUCT( z(:,i,ik), zc(:,i,ik) ) )
!                      z( :, i, ik) = z( :, i, ik) / SQRT(raux)
!                      !
!                  ENDDO
                  !
              ENDIF
              CALL log_pop("mat_hdiag")




              IF ( .NOT. lhave_overlap ) THEN
                  !
                  DO i  = 1, dimwann 
                      !
                      raux = REAL( DOT_PRODUCT( z(:,i,ik), z(:,i,ik) ) )
                      z( :, i, ik) = z( :, i, ik) / SQRT(raux)
                      !
                  ENDDO
                  ! 
              ELSE
                  ! 
                  ! diagonalizing H phi = eps S phi, where H_ij and S_ij are
                  ! definied on the covariant orbitals, then the z coefficients
                  ! are indeed contravariant.
                  !

                  ! zc = S * z
                  !
                  CALL mat_mul( zc(:,:,ik),  kovp(:,:), 'N', z(:,:,ik), 'N', dimwann, dimwann, dimwann )
                  !
                  DO i  = 1, dimwann 
                      !
                      raux = REAL( DOT_PRODUCT( z(:,i,ik), zc(:,i,ik) ) )
                      z( :, i, ik) = z( :, i, ik) / SQRT(raux)
                      !
                  ENDDO
                  !
              ENDIF
              !
                  CALL compute_grad_kham( dimwann, nrtot_nn, vr_nn, wr_nn, rham_nn,  &
                             vkpt_int(:,ik_g), gradkham)
                  !
                  IF ( lhave_overlap ) THEN
                      !  
!                      CALL mat_inv( dimwann, kovp, invkovp )
                      DO i  = 1, dimwann 
                          !
                          raux = REAL( DOT_PRODUCT( z(:,i,ik), z(:,i,ik) ) )
                          z( :, i, ik) = z( :, i, ik) / 2*SQRT(raux)
                          !
                      ENDDO
                  !
                      arg  = ( egrid( ie ) - eig_int( i, ik ) ) / delta 
                      raux = smearing_func( arg, smearing_type )

                      DO l = 1, 3
                          CALL mat_mul( work,  gradkham(:,:,l), 'N', z(:,:,ik), 'N', dimwann, dimwann, dimwann )
                          CALL mat_mul( work,  z(:,:,ik),       'C', work,      'N', dimwann, dimwann, dimwann )
                          !
                          CALL mat_mul( work2,  gradkovp(:,:,l), 'N', z(:,:,ik), 'N', dimwann, dimwann, dimwann )
                          CALL mat_mul( work2,  z(:,:,ik),       'C', work2,      'N', dimwann, dimwann, dimwann )
                          !
                          CALL mat_mul( work3,  kovp(:,:), 'N', z(:,:,ik), 'N', dimwann, dimwann, dimwann )
                          CALL mat_mul( work3,  z(:,:,ik),       'C', work3,      'N', dimwann, dimwann, dimwann )
                          !
                          DO i = 1, dimwann
                              !
       momentum(l,i,i,ik) = (REAL( work(i,i), dbl ) - eig_int(i,ik)*REAL( work2(i,i), dbl))/(REAL( work3(i,i), dbl))                             
                              !
                          ENDDO
                      ENDDO
                  ELSE
                      DO l = 1, 3
                          CALL mat_mul( work,  gradkham(:,:,l), 'N', z(:,:,ik), 'N', dimwann, dimwann, dimwann )
                          CALL mat_mul( work2,  z(:,:,ik),       'C', work,      'N', dimwann, dimwann, dimwann )
                          !
                          DO i = 1, dimwann
                              !
                              DO j = 1, dimwann
                                  !
                                  momentum(l,i,j,ik) = work2(i,j) 
                                  !print *, l, i, j, momentum(l, i, j, ik) 
                                  !
                              ENDDO    
                              ! 
                          ENDDO
                          !
                      ENDDO
                      !
                  ENDIF
              !
          ENDDO kpt_loop
          !
    ENDIF

      !
      ! DOS normalization costant
      ! spin doubling is avoided
      ! (internal coherence respect to transport)
      !
      !
      IF ( lhave_sgm .AND. ldynam_sgm ) THEN
          !
          cost = ONE / ( PI )
          !
          ALLOCATE( GF0( dimwann, dimwann ), STAT=ierr )
          IF( ierr /=0 ) CALL errore(subname,'allocating GF0', ABS(ierr) )
          !
          ALLOCATE( GF( dimwann, dimwann ), STAT=ierr )
          IF( ierr /=0 ) CALL errore(subname,'allocating GF', ABS(ierr) )
          !
      ELSE
          cost = ONE / ( delta )
      ENDIF
     
      !
      ! stdout report
      !
      CALL write_header( stdout, "Loop over energies" )
      CALL flush_unit( stdout )
          
      !
      ! compute Epsilon 
      !
      ALLOCATE( IM_EPSILON( 3, 3, ne ), STAT=ierr )
      IF( ierr /=0 ) CALL errore(subname,'allocating IM_EPSILON', ABS(ierr) )
      !
      ALLOCATE( RE_EPSILON( 3, 3, ne ), STAT=ierr )
      IF( ierr /=0 ) CALL errore(subname,'allocating RE_EPSILON', ABS(ierr) )
      !
      ALLOCATE( EELS( 3, 3, ne ), STAT=ierr )
      IF( ierr /=0 ) CALL errore(subname,'allocating RE_EPSILON', ABS(ierr) )
      !

      energy_loop1: &
      DO ie = 1, ne
          
          !
          ! stdout report
          !
          IF ( (MOD( ie, nprint) == 0 .OR. ie == 1) .AND. ionode ) THEN
              WRITE(stdout,"(2x, 'Computing E( ',i5,' ) = ', f12.5, ' eV' )") &
                            ie, egrid(ie)
          ENDIF
          IM_EPSILON(:,:,ie) = ZERO
          RE_EPSILON(:,:,ie) = ZERO
          EELS(:,:,ie)       = ZERO
          !
          
          !The following IF has to be canceled/handled. 
          !For our needs the logical condition is always false...
          !in any case the dos arrays are not allocated
   
          IF ( lhave_sgm .AND. ldynam_sgm ) THEN

             !
             ! include external self-energy in the calc
             !
             IF ( ionode ) THEN
                 !
                 CALL operator_read_data( sgm_unit, IE=ie, R_OPR=rsgm, IERR=ierr )
                 IF ( ierr/=0 ) CALL errore(subname,'reading rsgm', ie)
                 !
             ENDIF
             !
             CALL mp_bcast( rsgm,   ionode_id )
             !
             DO ir = 1, nrtot
                 rsgm_nn( :, :, ir ) = rsgm( :, :, r_index(ir) )
             ENDDO
             !
             !
             DO ik_g = iks, ike
                 !
                 ik = ik_g -iks +1
                 !
                 ! interpolate rsgm on the required kpt
                 ! in principles kham and kovp could be computed out of the
                 ! energy loop (saving time)
                 !
                 CALL compute_kham( dimwann, nrtot_nn, vr_nn, wr_nn, rham_nn,  &
                                    vkpt_int(:,ik_g), kham )
                 CALL compute_kham( dimwann, nrtot_nn, vr_nn, wr_nn, rsgm_nn,  &
                                    vkpt_int(:,ik_g), ksgm )
                 !
                 IF ( lhave_overlap ) THEN
                     !
                     CALL compute_kham( dimwann, nrtot_nn, vr_nn, wr_nn, rovp_nn,  &
                                        vkpt_int(:,ik_g), kovp(:,:) )
                     !
                 ENDIF
                 !
                 ! solve dyson equation to obtain the interacting Green function
                 ! NOTE: here the smearing of GF0 (non-interacting) is still
                 !       lorentzian whatever value from input
                 !
                 ze = egrid( ie ) + CI * delta
                 !
                 IF ( .NOT. lhave_overlap ) THEN
                     !
                     CALL dyson_solver( GF0, GF, ze, dimwann, kham(:,:), ksgm(:,:) )
                     !
                 ELSE
                     !
                     CALL dyson_solver( GF0, GF, ze, dimwann, kham(:,:), ksgm(:,:), kovp(:,:) )
                     !
                 ENDIF
                 !
                 !
                 DO i  = 1, dimwann 
                    !
                    dos0( ie )    = dos0( ie ) - cost * wk(ik_g) * AIMAG( GF0(i,i) )
                    dos(  ie )    = dos(  ie ) - cost * wk(ik_g) * AIMAG( GF(i,i)  )
                    !
                 ENDDO
                 !
             ENDDO
             !
          ELSE
             !
             ! standard Epsilon calculation (no sgm or static sgm)
             !
             DO ik_g = iks, ike
                 !
                 ik = ik_g -iks +1
                 !
                 DO n = 1, dimwann 
                     !
                     arg2  = ( eig_int( n, ik ) ) / temperature
                     raux2 = ONE / (EXP (arg2) + 1)
                     !                     
                     DO m = 1, dimwann
!                     if (m.ne.n) then
                         !
                         arg3  = ( eig_int( m, ik ) ) / temperature
                         raux3 = ONE / (EXP (arg3) + 1) 
                         !
                         arg  = ( egrid( ie ) - ( eig_int( m, ik ) - eig_int( n, ik) ) ) / delta
                         raux = smearing_func( arg, smearing_type )
!                     else
!                         arg3  = ( eig_int( n, ik ) + 0.05 ) / temperature
!                         raux3 = ONE / (EXP (arg3) + 1)
!                         !
!                         arg  = ( egrid( ie ) ) / delta
!                         raux = smearing_func( arg, smearing_type )
                         !
!                     endif  
                         DO j = 1, 3
                             !
                             DO i = 1, 3
!                                 IF ((raux2.gt.0.01).and.(raux3.lt.0.0001)) THEN
                                 !                     
                                 ! Logical condition to insert "manually" the Dirac delta function entering
                                 ! the expression of the imaginary part of the dielectric function Eq. 22.
                                 ! Comment or uncomment depending on the calculation to be performed,
                                 ! remembering to include or exclude the representation of the Dirac delta
                                 ! as Gaussian smearing function.
                                 !
!                         IF ( ( egrid( ie ) - ( eig_int( m, ik ) - eig_int( n, ik) ) )**2.lt.0.1*delta  ) THEN
                                 !
                                 ! Imaginary part of the dielectric function given by Eq. 22
                                 !
                                 if (n.ne.m) then
!                                 IF ((raux2.gt.0.01).and.(raux3.lt.0.0001)) THEN 
                                 IM_EPSILON( i, j, ie ) = IM_EPSILON( i, j, ie ) + & 
                                                          ( (egrid(ie)**2 + delta**2) /(egrid( ie )**2 + delta**2)**2 * &
!                                                          (cost * smearing_func( egrid(ie)/delta, 'lorentzian'))**2 * &
!                                                          1 / egrid(ie)**2 * &
                                                          wk(ik_g) * cost * raux * &
                                                          ( raux2 - raux3 ) * &
                                                          SQRT ( REAL( momentum( i, n, m, ik) * momentum( j, m, n, ik))**2 + &
                                                          AIMAG( momentum( i, n, m, ik) * momentum( j, m, n, ik))**2))
!                                 ENDIF 
!                                      if(ie.eq.1)    print *, i,n,m,ik, raux2, raux3 , momentum(i,n,m,ik), momentum(j,m,n,ik)
!                                 else
!                                 IF (raux2.gt.0.03) THEN
!                                 IM_EPSILON( i, j, ie ) = IM_EPSILON( i, j, ie ) + &
!                                              ( ONE /(egrid( ie ))* &
!                                                          wk(ik_g) * (raux*cost) * & !*raux
!                                                          ( smearing_func(arg2, 'fermi-dirac' )/temperature ) * &
!                                                          SQRT ( REAL( momentum( i, n, m, ik) * momentum( j, m, n, ik))**2 + &
!                                                          AIMAG( momentum( i, n, m, ik) * momentum( j, m, n, ik))**2))
!                                 endif
                                 ENDIF
!                         ENDIF            !
                                 !
                                 ! Logical condition to avoid the singularity in order to espress manually
                                 ! the principal value of the equation representing the real part of the 
                                 ! dielectric function.
                                 ! Comment or uncomment depending on the calculation to be performed,
                                 ! remembering to include or exclude the representation of the principal value
                                 ! as analytic function.
                                 ! 
!       IF ( ( eig_int( m, ik ) .ne. eig_int( n, ik) ) .and. ( eig_int( m, ik )-eig_int( n, ik).ne.egrid(ie) )) THEN
                                 ! 
                                 ! Real part of the dielectric function obtained by making a Kramers-Kronig transformation
                                 ! of the imaginary part expressed in Eq. 22.
                                 ! Comment or uncomment depending on the calculation to be performed.
                                 !                          
!                                 RE_EPSILON( i, j, ie ) = RE_EPSILON( i, j, ie ) + &
!                                                          !
!                                                          2 * wk(ik_g) * (eig_int( m, ik ) - eig_int( n, ik)) * & 
!                                                          !
!                                                         ( 1/ ( (eig_int( m, ik ) - eig_int( n, ik))**2 + (delta) ) ) *&
!                                                          !
!                                                          ((eig_int( m, ik ) - eig_int( n, ik))**2 - egrid(ie)**2) * &
!                                                          !
!                                          (1/(((eig_int( m, ik ) - eig_int( n, ik))**2 - egrid(ie)**2)**2  + (delta )) ) * &
!                                                          !
!                                                          ( raux2 - raux ) * &
!                                                          !
!                                                          SQRT ( REAL( momentum( i, n, m, ik) * momentum( j, m, n, ik))**2 + & 
!                                                          !
!                                                         AIMAG( momentum( i, n, m, ik) * momentum( j, m, n, ik))**2)
                                 !
                                 ! Real part of the dielectric function in the formulation reported by Graf and Vogl Eq.23.
                                 ! The effective mass term is included as reported in the original work.
                                 ! Comment or uncomment depending on the calculation to be performed.
                                 !
!                                 RE_EPSILON( i, j, ie ) = RE_EPSILON( i, j, ie ) + &
!                                                          !
!                                                wk(ik_g) * ((eig_int( n, ik ) - eig_int( m, ik))**2+delta**2 ) * &
!                                                          !
!                                                ( 1/ ( (eig_int( n, ik ) - eig_int( m, ik))**2 + delta**2 )**2 )*&
!                                                          !                                                        
!                                                          SIGN(ONE, (eig_int( m, ik ) - eig_int( n, ik) - egrid(ie))) * &
!                                                          !  
!                                                          sqrt((eig_int( m, ik ) - eig_int( n, ik) - egrid(ie))**2+delta**2 )* &
!                                                          !
!                                          (1/(((eig_int( m, ik ) - eig_int( n, ik)) - egrid(ie))**2 + (delta )**2 )) * & !+ (delta )**2) inside the last parentesis
!                                                          !
!                                                          ( raux2 - raux3 ) * &
!                                                          !
!                                                          SQRT ( REAL( momentum( i, n, m, ik) * momentum( j, m, n, ik))**2 + &
!                                                          !
!                                                          AIMAG( momentum( i, n, m, ik) * momentum( j, m, n, ik))**2)
!                                 IF (n.eq.m) THEN
!                                     !
!                                     RE_EPSILON( i, j, ie) = RE_EPSILON( i, j, ie ) - &
!                                                          !
!                                                         wk(ik_g) * (egrid( ie )**2+delta**2)/ (egrid( ie )**2+delta**2)**2 * &
!                                                          !
!                                                         raux2 * inv_MASS( i, j, n, ik )
!                                 ENDIF
!                                                   
!                                        
!
!                         ENDIF
                                 !
                             ENDDO
                             !
                         ENDDO
                          
                         ! 
                    ! endif 
                     ENDDO
                         !   
                 ENDDO
             !
             ENDDO  
             !
          ENDIF

          !
          ! stdout report
          !
          IF ( MOD( ie, nprint) == 0 .OR. ie == 1 .OR. ie == ne ) THEN
               !
               CALL timing_upto_now(stdout)
               CALL flush_unit(stdout)
               !
          ENDIF          
          !
      ENDDO energy_loop1
      !
      ! recover over kpt-parallelism
      !
      CALL mp_sum( IM_EPSILON )
      !
      ! If the real part is calculated transforming 
      ! numerically (with Kramers-Kronig) the imaginary
      ! part the following line can be commented 
      !
!      CALL mp_sum( RE_EPSILON )
      !
      ! Multiplying prefactors given in Eq.22-23. 
      ! The calculation in the code is performed using eV and bohr as
      ! energy and length units respectively. By converting eV into Rydberg (evtory)
      ! Rydberg units can be used. e2 is the square of the electronic charge, eps0
      ! the vacuum dielectric constant and omega the volume of the unit cell.
      ! The initial 2 factor takes into account the spin-degeneracy.
      ! If the real part is numerically integrated from the imaginary one,
      ! the second line can be commented.
      !        
      IM_EPSILON(:,:,:) = TWO * (PI * e2 / (eps0 * evtory * omega)) *  IM_EPSILON(:,:,:)
!      RE_EPSILON(:,:,:) = 1 + TWO * (e2 / (eps0 * evtory * omega)) *  RE_EPSILON(:,:,:)
      !
      ! Calculation of the real part of the dielectric function as the Kramers - Kronig
      ! transformation of the computed imaginary part given by Eq.22.
      ! Comment or uncomment depending on the calculation to be performed.
      !  
      DO ie=1,ne
          !
          RE_EPSILON(:,:,ie) = ZERO
          !
          DO j = 1, 3
              !
              DO i = 1, 3
                  !
                  DO ie2=2,ie-1
                      !
                      RE_EPSILON( i, j, ie )=RE_EPSILON( i, j, ie )+ &
                                 TWO / PI * egrid(2)*egrid(ie2) * (IM_EPSILON(i,j,ie2))/(egrid(ie2)**2 - egrid(ie)**2)
                      !
                  ENDDO
                  !
                  DO ie2=ie+1,ne
                      RE_EPSILON( i, j, ie )=RE_EPSILON( i, j, ie )+ &
                                 TWO / PI * egrid(2)*egrid(ie2) * (IM_EPSILON(i,j,ie2))/(egrid(ie2)**2 - egrid(ie)**2)
                      !
                  ENDDO
                  !
              ENDDO
              !
          ENDDO
          !
      ENDDO
      !
      RE_EPSILON(:,:,:) = 1 + RE_EPSILON(:,:,:)
      !
      EELS(:,:,:) = IM_EPSILON(:,:,:) / ( IM_EPSILON(:,:,:)**2 + RE_EPSILON(:,:,:)**2 )
      !
      !
      ! Calculation of the imaginary part of the dielectric function as the Kramers - Kronig
      ! transformation of the computed real part given by Eq.23.
      ! Comment or uncomment depending on the calculation to be performed.
      !  
!      DO ie=1,ne
!          !
!          IM_EPSILON(:,:,ie) = ZERO
!          !
!          DO j = 1, 3
!              !
!              DO i = 1, 3
!                  !
!                  DO ie2=2,ie-1
!                      !
!                      IM_EPSILON( i, j, ie )=IM_EPSILON( i, j, ie ) - &
!                                 TWO / PI * egrid(ie)*egrid(2) * (RE_EPSILON(i,j,ie2))/(egrid(ie2)**2 - egrid(ie)**2)
!                      !
!                  ENDDO
!                  !
!                  DO ie2=ie+1,ne
!                      IM_EPSILON( i, j, ie )=IM_EPSILON( i, j, ie ) - &
!                                 TWO / PI * egrid(ie)*egrid(2) * (RE_EPSILON(i,j,ie2))/(egrid(ie2)**2 - egrid(ie)**2)
!                      !
!                  ENDDO
!                  !
!              ENDDO
!              !
!          ENDDO
!          !
!      ENDDO


      ! 
      ! Sum rule expressed by Eq.24. Not yet carefully considered
      !         
!      FSUM = 0.0
!      DO ie = 1, ne
!          FSUM = FSUM + egrid(2) * egrid(ie) * IM_EPSILON(1,1,ie)
!      ENDDO
!      FSUM = evtory**2 * FSUM * ((omega / 2) / (2 * PI**2 * e2))
!      print *, "FSUM", FSUM
!
      !
      IF ( lhave_sgm ) THEN
          !
          CALL file_close(sgm_unit, PATH="/", ACTION="read", IERR=ierr )
          IF ( ierr/=0 ) CALL errore(subname,'closing '//TRIM(datafile_sgm), ABS(ierr) )
           !
      ENDIF
      !
      ! write Dielectric Function
      ! 
      filename  = TRIM(fileout)
      filename2 = TRIM(fileout2)
      filename3 = TRIM(fileout3)
      !
     ! !


      IF (ionode) THEN
          !
          OPEN( aux_unit, FILE=TRIM(filename), FORM='formatted', IOSTAT=ierr )
          IF (ierr/=0) CALL errore(subname,'opening '//TRIM(filename),ABS(ierr))
          !
          WRITE( aux_unit, *) "# E (eV)   Im_Epsilon(i,j,E)"
          DO ie = 1, ne
              WRITE(aux_unit, "(f9.4,9E15.4E3)") egrid(ie)+shift, IM_EPSILON(:,:,ie)
          ENDDO
          !
          CLOSE( aux_unit )
          !
          WRITE( stdout, "(/,2x,'Imaginary part of epsilon written on file:',4x,a)" ) TRIM(filename)
          ! 
          OPEN( aux_unit, FILE=TRIM(filename2), FORM='formatted', IOSTAT=ierr )
          IF (ierr/=0) CALL errore(subname,'opening '//TRIM(filename2),ABS(ierr))
          !    
          WRITE( aux_unit, *) "# E (eV)   Re_Epsilon(i,j,E)"
          DO ie = 1, ne
              WRITE(aux_unit, "(f9.4,9E15.4E3)") egrid(ie)+shift, RE_EPSILON(:,:,ie)
          ENDDO
          !
          CLOSE( aux_unit )
          !
          WRITE( stdout, "(/,2x,'Real part of epsilon written on file:',4x,a)" ) TRIM(filename2)
          !
          OPEN( aux_unit, FILE=TRIM(filename3), FORM='formatted', IOSTAT=ierr )
          IF (ierr/=0) CALL errore(subname,'opening '//TRIM(filename3),ABS(ierr))
          !
          WRITE( aux_unit, *) "# E (eV)   EELS(i,j,E)"
          DO ie = 1, ne
              WRITE(aux_unit, "(f9.4,9E15.4E3)") egrid(ie)+shift, EELS(:,:,ie)
          ENDDO
          !
          CLOSE( aux_unit )
          !
          WRITE( stdout, "(/,2x,'EELS written on file:',4x,a)" ) TRIM(filename3)
          ! 
      ENDIF
!
! ... Shutdown
!

      !
      ! Clean local memory
      !
      DEALLOCATE( vkpt_int, wk, STAT=ierr)
      IF( ierr /=0 ) CALL errore(subname, 'deallocating vkpt_int, wk', ABS(ierr) )
      !
      DEALLOCATE( eig_int, STAT=ierr)
      IF( ierr /=0 ) CALL errore(subname, 'deallocating eig_int', ABS(ierr) )
      !
      DEALLOCATE( z, STAT=ierr )
      IF( ierr /=0 ) CALL errore(subname, 'deallocating z', ABS(ierr))
      !
      DEALLOCATE( egrid, STAT=ierr )
      IF( ierr /=0 ) CALL errore(subname, 'deallocating egrid', ABS(ierr))
      !
      DEALLOCATE( vr_nn, wr_nn, STAT=ierr )
      IF( ierr /=0 ) CALL errore(subname, 'deallocating vr_nn, wr_nn', ABS(ierr) )
      !
      DEALLOCATE( kham, rham_nn, STAT=ierr)
      IF( ierr /=0 ) CALL errore(subname, 'deallocating kham, rham_nn', ABS(ierr) )
      !
      DEALLOCATE( gradkham, work, momentum, STAT=ierr)
      IF( ierr /=0 ) CALL errore(subname, 'deallocating gradkham, work, momentum', ABS(ierr) )
      !
      DEALLOCATE( IM_EPSILON, RE_EPSILON, EELS, STAT=ierr)
      IF( ierr /=0 ) CALL errore(subname, 'deallocating EPSILON, EELS ', ABS(ierr) )
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
END SUBROUTINE do_epsilon

