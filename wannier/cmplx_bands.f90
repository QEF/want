! 
! Copyright (C) 2008 WanT Group
!
! This file is distributed under the terms of the
! GNU General Public License. See the file `License'
! in the root directory of the present distribution,
! or http://www.gnu.org/copyleft/gpl.txt .
!
!=====================================================
   PROGRAM cmplx_bands
   !=====================================================
   !  
   ! Computes the complex band structure moving from 
   ! the knowledge of the Hamiltonian matrix elements on a
   ! localized basis sert (either WFs or non-orthogonal sets)
   !
   USE kinds
   USE version_module,       ONLY : version_number
   USE parameters,           ONLY : nstrx
   USE io_module,            ONLY : stdout, stdin
   USE io_module,            ONLY : prefix, postfix, work_dir
   USE io_module,            ONLY : datafile_dft => dftdata_file, datafile_sgm
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
   INTEGER          :: idir          ! the direction along which to compute the
                                     ! complex bands structure
   INTEGER          :: nk(2)         ! kpt generators (along dirs /= idir )
   INTEGER          :: s(2)          ! kpt shifts
   REAL(dbl)        :: toll          ! tolerance to define Ham matrix elements
                                     ! to be negligible
   REAL(dbl)        :: toll2         ! toll^2
   INTEGER          :: ne            ! dimension of the energy grid
   REAL(dbl)        :: emin          ! egrid extrema
   REAL(dbl)        :: emax
   CHARACTER(nstrx) :: fileout       ! output filename
   INTEGER          :: ircut(3)      ! real space curoff in terms of unit cells
                                     ! for directions i=1,2,3  (0 means no cutoff)
   INTEGER          :: nprint        ! print every "nprint" iterations

   !
   ! input namelist
   !
   NAMELIST /INPUT/ prefix, postfix, work_dir, datafile_dft, datafile_sgm, &
                    nk, s, toll, idir, emin, emax, ne, ircut, fileout, nprint
   !
   ! end of declariations
   !   

!
!------------------------------
! main body
!------------------------------
!
      CALL startup(version_number,'cmplx_bands')

      !
      ! read input
      !
      CALL cmplx_bands_input( )

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
      CALL do_cmplx_bands( fileout, idir, toll2, nk, s, emin, emax, ne, ircut, nprint )
      !
      ! clean global memory
      !
      CALL cleanup()

      !
      ! finalize
      !
      CALL shutdown( 'cmplx_bands' )

CONTAINS

!********************************************************
   SUBROUTINE cmplx_bands_input()
   !********************************************************
   !
   ! Read INPUT namelist from stdin
   !
   IMPLICIT NONE

      CHARACTER(17)    :: subname = 'cmplx_bands_input'
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
      idir                        =  0
      nk(1:2)                     = -1
      s(1:2)                      =  0
      toll                        =  1.0d-4
      emin                        = -10.0
      emax                        =  10.0
      ne                          =  1000
      ircut(1:3)                  =  0
      nprint                      = 50
      
      CALL input_from_file ( stdin )
      !
      READ(stdin, INPUT, IOSTAT=ierr)
      IF ( ierr /= 0 )  CALL errore(subname,'Unable to read namelist INPUT',ABS(ierr))

      IF ( LEN_TRIM(fileout) == 0 ) &
           fileout = TRIM(work_dir)//'/'//TRIM(prefix)//TRIM(postfix)// &
                                     '_cmplx_bands.dat'

      !
      ! Some checks 
      !
      IF ( idir < 1 .OR. idir > 3 ) CALL errore(subname, 'Invalid idir', 71)
      !
      IF ( ANY( nk(:) <=0 ) )    CALL errore(subname, 'Invalid nk', 1)
      IF ( ANY( s(:)  < 0 ) )    CALL errore(subname, 'Invalid s', 1)
      IF ( ANY( s(:)  > 1 ) )    CALL errore(subname, 'Invalid s', 2)
      IF ( emin > emax  )        CALL errore(subname, 'emin larger than emax', 4)
      IF ( ne <= 0  )            CALL errore(subname, 'invalid ne', 4)
      IF ( nprint <= 0  )        CALL errore(subname, 'invalid nprint', 5)
      IF ( toll < 0.0  )         CALL errore(subname, 'invalid toll', 5)
      IF ( ANY( ircut(:) < 0 ) ) CALL errore(subname,'Invalid ircut', 10)
      !
      lhave_sgm = .FALSE.
      IF ( LEN_TRIM(datafile_sgm) > 0 ) lhave_sgm = .TRUE.
      !
      toll2 = toll**2

      !
      ! input summary
      !
      CALL write_header( stdout, "INPUT Summary" )
      !
      WRITE( stdout, "(   7x,'               fileout :',5x,   a)") TRIM(fileout)
      WRITE( stdout, "(   7x,'        band direction :',3x,i4 )") idir
      WRITE( stdout, "(   7x,'                    nk :',3x,2i4 )") nk(1:2)
      WRITE( stdout, "(   7x,'                     s :',3x,2i4 )") s(1:2)
      WRITE( stdout, "(   7x,'                  toll :',3x,e12.4 )") toll
      WRITE( stdout, "(   7x,'                  emin :',3x,f8.3 )") emin
      WRITE( stdout, "(   7x,'                  emax :',3x,f8.3 )") emax
      WRITE( stdout, "(   7x,'                    ne :',3x,i6 )") ne
      WRITE( stdout, "(   7x,'                nprint :',3x,i6 )") nprint
      !
      IF ( ANY( ircut(:) > 0 ) ) THEN
          WRITE( stdout,"(7x,'                 ircut :',3x,3i4)") ircut(:)
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
      WRITE(stdout, "()")

   END SUBROUTINE cmplx_bands_input
   !
END PROGRAM cmplx_bands


!********************************************************
   SUBROUTINE do_cmplx_bands( fileout, idir, toll2, nk, s, emin, emax, ne, ircut, nprint )
   !********************************************************
   !
   ! perform the main task of the calculation
   !
   USE kinds
   USE parameters,           ONLY : nstrx
   USE constants,            ONLY : ZERO, ONE, TWO, CZERO, CONE, EPS_m6, BOHR => bohr_radius_angs
   USE io_module,            ONLY : ionode, stdout, stdin, aux_unit, sgm_unit
   USE io_module,            ONLY : datafile_sgm
   USE files_module,         ONLY : file_open, file_close
   USE util_module,          ONLY : mat_hdiag, zmat_herm
   USE converters_module,    ONLY : cry2cart, cart2cry
   USE lattice_module,       ONLY : avec, bvec
   USE kpoints_module,       ONLY : nrtot, nr, vr, wr 
   USE windows_module,       ONLY : nspin
   USE hamiltonian_module,   ONLY : dimwann, rham, rovp, lhave_overlap
   USE correlation_module,   ONLY : lhave_sgm, ldynam_sgm, rsgm, correlation_allocate
   USE correlation_module,   ONLY : omg_grid, omg_nint
   USE timing_module,        ONLY : timing, timing_upto_now
   USE log_module,           ONLY : log_push, log_pop
   USE parser_module,        ONLY : change_case
   USE util_module,          ONLY : mat_mul, mat_svd, mat_inv, zmat_diag
   USE operator_module
   !
   IMPLICIT NONE

      !
      ! input vars
      !
      CHARACTER(*),  INTENT(IN)    :: fileout
      INTEGER,       INTENT(IN)    :: idir
      REAL(dbl),     INTENT(IN)    :: toll2
      INTEGER,       INTENT(IN)    :: nk(2), s(2)
      INTEGER,       INTENT(INOUT) :: ne
      REAL(dbl),     INTENT(INOUT) :: emin, emax
      INTEGER,       INTENT(IN)    :: ircut(3)
      INTEGER,       INTENT(IN)    :: nprint

      !
      ! local vars
      !
      CHARACTER(14)     :: subname = 'do_cmplx_bands'
      !
      INTEGER           :: nkpts_2D
      INTEGER           :: nrtot_nn, nrtot_1D
      LOGICAL           :: lhave_nn(3)
      INTEGER           :: nrs, nrsx, ir0_1D
      INTEGER           :: ndim_r, ndim_n
      !
      INTEGER,      ALLOCATABLE :: r_index(:), rmap_1D(:)
      REAL(dbl),    ALLOCATABLE :: egrid(:)
      REAL(dbl),    ALLOCATABLE :: vkpt_2D(:,:), wk_2D(:)
      REAL(dbl),    ALLOCATABLE :: vr_cry(:,:), vr_nn(:,:), wr_nn(:), vr_sgm(:,:)
      REAL(dbl),    ALLOCATABLE :: vr_cry_1D(:)
      !
      COMPLEX(dbl), ALLOCATABLE :: rham_1D(:,:,:), rham_nn(:,:,:)
      COMPLEX(dbl), ALLOCATABLE :: rsgm_1D(:,:,:), rsgm_nn(:,:,:)
      COMPLEX(dbl), ALLOCATABLE :: rovp_1D(:,:,:), rovp_nn(:,:,:)
      !
      COMPLEX(dbl), ALLOCATABLE :: zz(:,:), yy(:,:,:), ww(:,:), qq(:,:)
      COMPLEX(dbl), ALLOCATABLE :: u(:,:), vt(:,:)
      REAL(dbl),    ALLOCATABLE :: sngv(:)
      INTEGER,      ALLOCATABLE :: subn_map(:) !, subr_map(:)
      COMPLEX(dbl), ALLOCATABLE :: proj_r(:,:), proj_n(:,:)
      COMPLEX(dbl), ALLOCATABLE :: block_r(:,:), block_n(:,:)
      COMPLEX(dbl), ALLOCATABLE :: fact_r(:,:), fact_n1(:,:), fact_n2(:,:)
      !
      COMPLEX(dbl), ALLOCATABLE :: mtrx(:,:)
      COMPLEX(dbl), ALLOCATABLE :: caux(:,:), zaux(:,:), waux(:)
      REAL(dbl),    ALLOCATABLE :: beta(:,:,:)
      ! 
      CHARACTER(nstrx)  :: filename, analyticity_sgm
      CHARACTER(20)     :: ctmp
      !
      REAL(dbl)         :: cost, raux, avg_nrs
      !
      INTEGER           :: i, j, l, ie, ik, ir
      INTEGER           :: ierr
      INTEGER           :: nk_(3), s_(3)
      INTEGER           :: dimwann_sgm, nrtot_sgm
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

      CALL write_header( stdout, "Calculation of Complex Band Structure" )
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
          CALL operator_read_aux( sgm_unit, DIMWANN=dimwann_sgm, NR=nrtot_sgm,        &
                                            DYNAMICAL=ldynam_sgm, NOMEGA=omg_nint,    &
                                            ANALYTICITY=analyticity_sgm, IERR=ierr )
                                            !
          IF ( ierr/=0 ) CALL errore(subname,'reading DIMWANN--ANALYTICITY', ABS(ierr) )

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
             CALL operator_read_aux( sgm_unit, VR=vr_sgm, GRID=omg_grid, IERR=ierr )
             IF ( ierr/=0 ) CALL errore(subname,'reading VR, GRID', ABS(ierr) )
             !
          ELSE
             !
             CALL operator_read_aux( sgm_unit, VR=vr_sgm, IERR=ierr )
             IF ( ierr/=0 ) CALL errore(subname,'reading VR', ABS(ierr) )
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
      ! setting energy grid
      !
      IF ( ldynam_sgm ) THEN
         !
         CALL warning(subname, 'energy grid is forced from SGM datafile' )
         WRITE( stdout, '()')
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
      ! parallel kpt mesh
      !
      nkpts_2D = PRODUCT( nk(1:2) )
      !
      ALLOCATE( vkpt_2D( 3, nkpts_2D ), wk_2D( nkpts_2D ), STAT=ierr )
      IF( ierr /=0 ) CALL errore(subname, 'allocating vkpt_2D, wk_2D', ABS(ierr) )

      !
      ! generate a 3D monkhorst-pack grid, corresponding to nk(1:2) and s(1:2)
      ! kpts gen are in crystal coords
      !
      nk_(idir) = 1
      s_(idir)  = 0
      !
      j = 0
      DO i = 1, 3
         IF ( i /= idir ) THEN
            j=j+1
            nk_(i) = nk(j)
            s_(i)  = s(j)
         ENDIF
      ENDDO
      !
      CALL monkpack( nk_, s_, vkpt_2D )
      !
      wk_2D(1:nkpts_2D) = TWO / REAL( nspin * nkpts_2D ,dbl)
      !
      ! mv kpts in cartesian coords (bohr^-1)
      CALL cry2cart( vkpt_2D, bvec )

      !
      ! kpt summary
      !
      IF ( ionode ) THEN
          !
          WRITE( stdout, "(2x, ' idir = ',i5 ) " ) idir
          WRITE( stdout, "(2x, 'nktot = ',i5,/ ) " ) nkpts_2D
          WRITE( stdout, "(2x, 'Monkhorst-Pack grid:      nk = (',3i4,' ),', &
                                             & 6x,'shift = (',3i4,' )' ) " ) nk_(:), s_(:)
          WRITE( stdout, "(2x, 'Generated kpt mesh: (cart. coord. in Bohr^-1)',/)" )
          !
          DO ik=1,nkpts_2D
              WRITE( stdout, " (4x, 'k (', i5, ') =    ( ',3f9.5,' ),   weight = ', f11.7 )") &
                     ik, ( vkpt_2D(i,ik), i=1,3 ), wk_2D(ik)
          ENDDO
          WRITE( stdout, "(/)" )
          !
      ENDIF



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
          IF (  ( .NOT. lhave_nn(1) .OR. ABS(NINT(vr_cry(1,ir))) <= ircut(1) ) .AND. &
                ( .NOT. lhave_nn(2) .OR. ABS(NINT(vr_cry(2,ir))) <= ircut(2) ) .AND. &
                ( .NOT. lhave_nn(3) .OR. ABS(NINT(vr_cry(3,ir))) <= ircut(3) )       )  THEN
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
      ! read and convert static self-energy, if needed
      !
      IF ( lhave_sgm  .AND. .NOT. ldynam_sgm ) THEN
          !
          CALL operator_read_data( sgm_unit, R_OPR=rsgm, IERR=ierr )
          IF ( ierr/=0 ) CALL errore(subname,'reading static rsgm', 11) 
          !
          DO ir = 1, nrtot_nn
               rsgm_nn( :, :, ir ) = rsgm( :, :, r_index(ir) )
          ENDDO
          !
      ENDIF

      !
      ! define nrtot_1D
      !
      ALLOCATE( vr_cry( 3, nrtot_nn), STAT=ierr )
      IF ( ierr/=0 ) CALL errore(subname, 'allocating vr_cry', ABS(ierr) )
      !
      vr_cry = vr_nn
      CALL cart2cry( vr_cry, avec)
      !
      nrtot_1D = nr( idir )
      IF ( lhave_nn(idir) ) nrtot_1D = MIN ( nrtot_1D, 2 * ircut(idir) + 1 )
      !
      i = NINT ( MINVAL( vr_cry(idir, :) ) )
      j = NINT ( MAXVAL( vr_cry(idir, :) ) )
      !
      IF ( j -i +1 /= nrtot_1D ) CALL errore(subname,'1D lattice dimensions wrong',1 )
      !
      !
      ALLOCATE( vr_cry_1D(nrtot_1D), STAT=ierr )
      IF ( ierr/=0 ) CALL errore(subname, 'allocating vr_cry_1D', ABS(ierr) )
      !
      !
      ir0_1D = -1
      !
      DO ir = 1, nrtot_1D
         vr_cry_1D( ir ) = REAL( i + ir-1, dbl )
         !
         ! set the index of the central reference cell
         IF ( i + ir-1 == 0 ) ir0_1D = ir
      ENDDO
      !
      !
      IF ( NINT( vr_cry_1D( nrtot_1D ) ) /= j ) &
         CALL errore(subname,'unexpected error on vr_cry_1D', 72)
      !
      DEALLOCATE( vr_cry, STAT=ierr )
      IF ( ierr/=0 ) CALL errore(subname, 'deallocating vr_cry', ABS(ierr) )


      !
      ! local workspace
      !
      ALLOCATE( rham_1D( dimwann, dimwann, nrtot_1D ), STAT=ierr )
      IF( ierr /=0 ) CALL errore(subname,'allocating rham_1D', ABS(ierr) )
      !
      ALLOCATE( rovp_1D(dimwann, dimwann, nrtot_1D), STAT=ierr )
      IF ( ierr/=0 ) CALL errore(subname,'allocating rovp_1D',ABS(ierr))
      !
      IF ( lhave_sgm ) THEN
          ALLOCATE( rsgm_1D(dimwann, dimwann, nrtot_1D ), STAT=ierr )
          IF ( ierr/=0 ) CALL errore(subname,'allocating rsgm_1D',ABS(ierr))
      ENDIF
      !
      ALLOCATE( rmap_1D(nrtot_1D), STAT=ierr )
      IF ( ierr/=0 ) CALL errore(subname,'allocating rmap_1D',ABS(ierr))
      !
      !
      ALLOCATE( zz(dimwann, dimwann), STAT=ierr )
      IF ( ierr/=0 ) CALL errore(subname,'allocating zz',ABS(ierr))
      ALLOCATE( yy(dimwann, dimwann, nrtot_1D), STAT=ierr )
      IF ( ierr/=0 ) CALL errore(subname,'allocating yy',ABS(ierr))
      ALLOCATE( ww(dimwann, dimwann), STAT=ierr )
      IF ( ierr/=0 ) CALL errore(subname,'allocating ww',ABS(ierr))
      ALLOCATE( qq(dimwann, dimwann), STAT=ierr )
      IF ( ierr/=0 ) CALL errore(subname,'allocating qq',ABS(ierr))
      !
      ALLOCATE( u(dimwann, dimwann), STAT=ierr )
      IF ( ierr/=0 ) CALL errore(subname,'allocating u',ABS(ierr))
      ALLOCATE( vt(dimwann, dimwann), STAT=ierr )
      IF ( ierr/=0 ) CALL errore(subname,'allocating vt',ABS(ierr))
      ALLOCATE( sngv(dimwann), STAT=ierr )
      IF ( ierr/=0 ) CALL errore(subname,'allocating sngv',ABS(ierr))
      !ALLOCATE( subr_map(dimwann), STAT=ierr )
      !IF ( ierr/=0 ) CALL errore(subname,'allocating subr_map',ABS(ierr))
      ALLOCATE( subn_map(dimwann), STAT=ierr )
      IF ( ierr/=0 ) CALL errore(subname,'allocating subn_map',ABS(ierr))
      ALLOCATE( proj_r(dimwann, dimwann), STAT=ierr )
      IF ( ierr/=0 ) CALL errore(subname,'allocating proj_r',ABS(ierr))
      ALLOCATE( proj_n(dimwann, dimwann), STAT=ierr )
      IF ( ierr/=0 ) CALL errore(subname,'allocating proj_n',ABS(ierr))
      !
      ALLOCATE( block_r(dimwann, dimwann), STAT=ierr )
      IF ( ierr/=0 ) CALL errore(subname,'allocating block_r',ABS(ierr))
      ALLOCATE( block_n(dimwann, dimwann), STAT=ierr )
      IF ( ierr/=0 ) CALL errore(subname,'allocating block_n',ABS(ierr))
      ALLOCATE( fact_r(dimwann, dimwann), STAT=ierr )
      IF ( ierr/=0 ) CALL errore(subname,'allocating fact_r',ABS(ierr))
      ALLOCATE( fact_n1(dimwann, dimwann), STAT=ierr )
      IF ( ierr/=0 ) CALL errore(subname,'allocating fact_n1',ABS(ierr))
      ALLOCATE( fact_n2(dimwann, dimwann), STAT=ierr )
      IF ( ierr/=0 ) CALL errore(subname,'allocating fact_n2',ABS(ierr))
      ALLOCATE( caux(dimwann, dimwann), STAT=ierr )
      IF ( ierr/=0 ) CALL errore(subname,'allocating caux',ABS(ierr))
      !
      ALLOCATE( beta(dimwann*(nrtot_1D-1), nkpts_2D, ne), STAT=ierr )
      IF ( ierr/=0 ) CALL errore(subname,'allocating beta',ABS(ierr))


!
! ... Main energy loop
!
!     From here on, we are implementing the algorithm reported in
!     John K. Tomfohr and Ottto F. Sankey, Phys.Rev.B 65, 245105 (2002)
!
!     When the case we make rererence to the equations of the above paper
!

      !
      ! set conversion factor
      ! NOTE: beta = 2 * | Im(k) |   [Ang^-1]
      !
      cost = DOT_PRODUCT( avec(:,idir), avec(:,idir) )
      cost = TWO / ( SQRT( cost ) * BOHR )


      energy_loop: &
      DO ie = 1, ne

          !
          ! stdout report
          !
          IF ( (MOD( ie, nprint) == 0 .OR. ie == 1) .AND. ionode ) THEN
               WRITE(stdout,"(2x, 'Computing E( ',i5,' ) = ', f12.5, ' eV' )") &
                             ie, egrid(ie)
          ENDIF
          !          
          avg_nrs = ZERO
 

          !
          ! read dynamical sgm, if the case
          !
          IF ( lhave_sgm .AND. ldynam_sgm ) THEN
              !
              CALL operator_read_data( sgm_unit, IE=ie, R_OPR=rsgm, IERR=ierr )
              IF ( ierr/=0 ) CALL errore(subname,'reading rsgm', ie)
              !
              DO ir = 1, nrtot
                  rsgm_nn( :, :, ir ) = rsgm( :, :, r_index(ir) )
              ENDDO
              !
          ENDIF
          !
          !
          kpt_2D_loop: &
          DO ik = 1, nkpts_2D

              !
              ! compute Hamiltonian, Self-Energy, and Overlaps 
              ! with the correct 2D bloch symmetry for each 1D rvect
              !
              nrs = 0
              nrsx = 0
              rmap_1D(:) = -1
              !
              DO ir = 1, nrtot_1D
                  !
                  CALL compute_kham_2D( dimwann, idir, vr_cry_1D(ir), nrtot_nn, vr_nn, wr_nn, &
                                        rham_nn, vkpt_2D(:,ik), rham_1D(:,:,ir) )

                  IF ( lhave_overlap ) THEN
                      !
                      CALL compute_kham_2D( dimwann, idir, vr_cry_1D(ir), nrtot_nn, vr_nn, wr_nn, &
                                            rovp_nn, vkpt_2D(:,ik), rovp_1D(:,:,ir) )
                      !
                  ELSE
                      !
                      ! overlaps are set equal to the identity for R_1D = 0
                      !
                      rovp_1D(:,:,ir) = CZERO
                      !
                      IF ( ir == ir0_1D ) THEN
                          !
                          DO i = 1, dimwann
                              rovp_1D(i,i,ir) = CONE
                          ENDDO
                          !
                      ENDIF
                      !
                  ENDIF
                  !
                  !
                  IF ( lhave_sgm ) THEN
                      !
                      CALL compute_kham_2D( dimwann, idir, vr_cry_1D(ir), nrtot_nn, vr_nn, wr_nn, &
                                            rsgm_nn, vkpt_2D(:,ik), rsgm_1D(:,:,ir) )
                      ! 
                      rham_1D(:,:, ir) = rham_1D(:,:, ir) + rsgm_1D(:,:, ir)
                      !
                  ENDIF
                  
                  ! 
                  ! setup Z = Ham + Sgm(ze) -E * Ovp
                  ! the self-energy has already been added to the hamiltonian
                  !
                  zz(:,:) = rham_1D(:,:,ir) - egrid(ie) * rovp_1D(:,:,ir)
                  !
                  ! determine the range of Z
                  ! along idir
                  !
                  lfound = .FALSE.
                  !
                  DO j = 1, dimwann
                  DO i = 1, dimwann
                      !
                      raux = REAL( zz(i,j) * CONJG( zz(i,j) ), dbl )
                      IF ( raux > toll2 ) lfound = .TRUE.
                      !
                  ENDDO
                  ENDDO
                  !
                  IF ( lfound ) THEN
                      !
                      nrs = nrs + 1
                      rmap_1D(nrs) = ir
                      !
                  ENDIF
                  !
                  !
              ENDDO
              !
              nrsx    = MAX( nrsx, nrs)
              avg_nrs = avg_nrs + REAL(nrs)


              !
              ! allocate the big matrix and related quantities
              ALLOCATE( mtrx(dimwann*(nrs-1), dimwann*(nrs-1)), STAT=ierr )
              IF ( ierr/=0 ) CALL errore(subname,'allocating mtrx',ABS(ierr))
              ALLOCATE( zaux(dimwann*(nrs-1), dimwann*(nrs-1)), STAT=ierr )
              IF ( ierr/=0 ) CALL errore(subname,'allocating zaux',ABS(ierr))
              ALLOCATE( waux(dimwann*(nrs-1)), STAT=ierr )
              IF ( ierr/=0 ) CALL errore(subname,'allocating waux',ABS(ierr))


              !
              ! set Z^(N)
              !
              ir = nrs
              zz(:,:) = rham_1D(:,:, rmap_1D(ir) ) - egrid(ie) * rovp_1D(:,:, rmap_1D(ir) )
 
              !
              ! singular value decomposition of set Z^(N)
              !
              CALL mat_svd( dimwann, dimwann, zz, sngv, u, vt )

              !
              ! determine the dimension of the r- and n-subspaces
              !
              ndim_r = 0 
              ndim_n = 0 
              !
              !subr_map(:) = 0 
              subn_map(:) = 0 
              ! 
              proj_r(:,:) = CZERO
              proj_n(:,:) = CZERO
              !
              DO i = 1, dimwann
                  !
                  ! singular values must be non negative
                  IF ( sngv(i) < -EPS_m6 ) THEN 
                     !
                     CALL errore(subname,'negative singular value', i)
                     !
                  ELSE IF ( sngv(i) >  EPS_m6 ) THEN
                     !
                     ndim_r              = ndim_r + 1
                     !subr_map( ndim_r )  = i
                     proj_r(i,i)         = CONE
                     !
                  ELSE
                     !
                     ndim_n              = ndim_n + 1
                     subn_map( ndim_n )  = i
                     proj_n( i, i )      = CONE
                     !
                  ENDIF
                  !
              ENDDO
              !
              IF ( ndim_r + ndim_n /= dimwann ) CALL errore(subname,'invalid r+n decomp.', 5)

              !
              ! define Wi = W_d + I_n, just after Eq.(24), which is now
              ! invertible
              !
              ww(:,:) = CZERO
              !
              DO i = 1, dimwann
                 ww( i, i ) = sngv( i )
              ENDDO
              !
              ww(:,:) = ww(:,:) + proj_n(:,:)

              !   
              ! invert W and overwirte it with the inverse
              CALL mat_inv( dimwann, ww, caux )
              ww = caux

              !
              ! define the Y matrices 
              ! [ end of page 5 in the paper and Eqs.(23-24) ]
              !
              DO ir = 1, nrs-1
                  !
                  zz(:,:) = rham_1D(:,:, rmap_1D(ir) ) -egrid(ie)*rovp_1D(:,:,rmap_1D(ir))
                  !
                  CALL mat_mul( caux, zz, 'N', vt, 'C', dimwann, dimwann, dimwann)
                  CALL mat_mul( yy(:,:,ir), u, 'C', caux, 'N', dimwann, dimwann, dimwann)
                  ! 
              ENDDO

              !
              ! define Q = I_n * Y^(N-1) *I_n + I_r
              ! Eqs.(28,29)
              !
              qq(:,:) = CZERO
              !
              DO j = 1, ndim_n
              DO i = 1, ndim_n
                  !
                  qq( subn_map(i), subn_map(j) ) = yy( subn_map(i), subn_map(j), nrs-1)
                  !
              ENDDO
              ENDDO
              !
              qq = qq + proj_r
              !
              ! invert qq and overwirte it with the inverse
              CALL mat_inv( dimwann, qq, caux )
              qq = caux

              !
              ! define some prefactors
              ! compare with Eq.(31)
              !
              ! fact_r  = -I_r * W^-1
              ! fact_n1 = -I_n * Q^-1
              ! fact_n2 =  I_n * Q^-1 * Y^(N-1) * I_r * W^-1 
              !         =  fact_n1 * Y^(N-1) * fact_r
              !
              !
              CALL mat_mul( fact_r, proj_r, 'N', ww, 'N', dimwann, dimwann, dimwann )
              fact_r = -fact_r
              !
              CALL mat_mul( fact_n1, proj_n, 'N', qq, 'N', dimwann, dimwann, dimwann )
              fact_n1 = -fact_n1
              !
              CALL mat_mul( caux, yy(:,:,nrs-1), 'N', fact_r, 'N', dimwann, dimwann, dimwann )
              CALL mat_mul( fact_n2, fact_n1, 'N', caux, 'N', dimwann, dimwann, dimwann )


              !
              ! fill the big matrix
              ! compare with Eq.(31)
              !
              mtrx(:,:) = CZERO

              !
              ! start with the first row
              !
              DO ir = 1, nrs-1
                  !
                  ! block_r 
                  CALL mat_mul( block_r, fact_r, 'N', yy(:,:,ir), 'N', &
                                dimwann, dimwann, dimwann )
                  !
                  ! block_n
                  CALL mat_mul( block_n, fact_n2, 'N', yy(:,:,ir), 'N', &
                                dimwann, dimwann, dimwann )
                  !
                  IF ( ir > 1 ) THEN
                      !
                      CALL mat_mul( caux, fact_n1, 'N', yy(:,:,ir-1), 'N', &
                                    dimwann, dimwann, dimwann )
                      !
                      block_n = block_n + caux
                      !
                  ENDIF
                  !
                  ! plug the blocks in the big matrix
                  !
                  j = (nrs -ir -1)*dimwann
                  mtrx( 1:dimwann, j+1:j+dimwann ) = block_r(:,:) + block_n(:,:)
                  !
              ENDDO
               
              !
              ! setup the remaining part of the big matrix
              !
              DO j  = 1, nrs-2
                   !
                   i = j+1
                   DO l = 1, dimwann
                       !
                       mtrx( (i-1)*dimwann +l, (j-1)*dimwann + l ) = CONE
                       !
                  ENDDO
                  !
              ENDDO
  

              !
              ! diagonalize the big matrix 
              ! and get the complex band structure
              ! note that the matrix is not hermitean and we
              ! take the left eigenvectors
              !
              CALL timing( "zmat_diag", OPR="start")
              !
              CALL zmat_diag( zaux, waux, mtrx, dimwann*(nrs-1), 'L')
              !
              CALL timing( "zmat_diag", OPR="stop")
              !
              beta( :, ik, ie) = -TWO
              !
              DO i = 1, dimwann*(nrs-1)
                  !
                  IF ( REAL( waux(i)*CONJG(waux(i)), dbl) > EPS_m6 ) THEN
                     !
                     beta( i, ik, ie ) = ABS( REAL( LOG( waux(i) ), dbl ) )
                     !
                     ! convert to Ang^-1
                     beta( i, ik, ie ) = beta( i, ik, ie ) * cost
                     !
                  ELSE
                     !
                     beta( i, ik, ie ) = -ONE
                     !
                  ENDIF
                  !
              ENDDO


              !
              ! local cleanup
              !
              DEALLOCATE( mtrx, STAT=ierr )
              IF ( ierr/=0 ) CALL errore(subname,'deallocating mtrx',ABS(ierr))
              DEALLOCATE( zaux, waux, STAT=ierr )
              IF ( ierr/=0 ) CALL errore(subname,'deallocating zaux, waux',ABS(ierr))
              !
              !
          ENDDO kpt_2D_loop
          !
          !
          avg_nrs = avg_nrs/REAL(nkpts_2D)
          !
          IF ( (MOD( ie, nprint) == 0 .OR. &
               ie == 1 .OR. ie == ne ) .AND. ionode) THEN
               !
               WRITE(stdout,"(7x,'avg. range of Z : ',i7)") NINT( avg_nrs )
               WRITE(stdout,"(7x,'  avg. mtrx dim : ',i7,/)") NINT( (avg_nrs-1.0) ) * dimwann
               !
               CALL timing_upto_now(stdout)
               CALL flush_unit(stdout)
               ! 
          ENDIF
          !
          !
      ENDDO energy_loop
      !
      !
      IF ( lhave_sgm ) THEN
          !
          CALL file_close(sgm_unit, PATH="/", ACTION="read", IERR=ierr )
          IF ( ierr/=0 ) CALL errore(subname,'closing '//TRIM(datafile_sgm), ABS(ierr) )
          !
      ENDIF



! 
! ... Write final interpolated band structure to file
! 
      filename=TRIM(fileout)
      !
      OPEN( aux_unit, FILE=TRIM(filename), FORM='formatted', IOSTAT=ierr )
      IF (ierr/=0) CALL errore(subname,'opening '//TRIM(filename),ABS(ierr))
         !
         WRITE( ctmp, "(i5)") dimwann * (nrsx-1) +1
         !
         DO ik = 1, nkpts_2D
             WRITE( aux_unit, "(a,i5)") "# CBS for kpt = ", ik
             !
             DO ie = 1, ne
                 WRITE(aux_unit, "("//TRIM(ctmp)//"f15.9)") egrid(ie), &
                       beta( 1:dimwann*(nrsx-1), ik, ie)
             ENDDO
             !
         ENDDO
         !
      CLOSE( aux_unit )
      !
      WRITE( stdout, "(/,2x,'CBS written on file:',4x,a)" ) TRIM(fileout)

!
! ... Shutdown
!

      !
      ! Clean local memory
      !
      DEALLOCATE( vkpt_2D, wk_2D, STAT=ierr)
      IF( ierr /=0 ) CALL errore(subname, 'deallocating vkpt_2D, wk_2D', ABS(ierr) )
      !
      DEALLOCATE( egrid, STAT=ierr )
      IF( ierr /=0 ) CALL errore(subname, 'deallocating egrid', ABS(ierr))
      !
      DEALLOCATE( vr_nn, wr_nn, STAT=ierr )
      IF( ierr /=0 ) CALL errore(subname, 'deallocating vr_nn, wr_nn', ABS(ierr) )
      !
      DEALLOCATE( vr_cry_1D, STAT=ierr )
      IF( ierr /=0 ) CALL errore(subname, 'deallocating vr_cry_1D', ABS(ierr) )
      !
      DEALLOCATE( r_index, STAT=ierr )
      IF( ierr /=0 ) CALL errore(subname, 'deallocating r_index', ABS(ierr) )
      !
      DEALLOCATE( rmap_1D, STAT=ierr )
      IF( ierr /=0 ) CALL errore(subname, 'deallocating rmap_1D', ABS(ierr) )
      !
      DEALLOCATE( rham_1D, rham_nn, STAT=ierr)
      IF( ierr /=0 ) CALL errore(subname, 'deallocating kham, rham_nn', ABS(ierr) )
      !
      IF ( lhave_sgm ) THEN
         DEALLOCATE( rsgm_1D, rsgm_nn, STAT=ierr )
         IF ( ierr/=0 ) CALL errore(subname,'deallocating rsgm_1D, rsgm_nn',ABS(ierr))
      ENDIF
      !
      DEALLOCATE( rovp_1D, STAT=ierr )
      IF ( ierr/=0 ) CALL errore(subname,'deallocating rovp_1D',ABS(ierr))
      !
      IF ( lhave_overlap ) THEN
         DEALLOCATE( rovp_nn, STAT=ierr )
         IF ( ierr/=0 ) CALL errore(subname,'deallocating rovp_nn',ABS(ierr))
      ENDIF
      !
      ! deallocate workspace specific of CBS calc
      !
      DEALLOCATE( zz, yy, ww, qq, STAT=ierr)
      IF ( ierr/=0 ) CALL errore(subname,'deallocating zz -- qq',ABS(ierr))
      !
      DEALLOCATE( u, vt, sngv, STAT=ierr)
      IF ( ierr/=0 ) CALL errore(subname,'deallocating u, vt, sngv',ABS(ierr))
      !
      !DEALLOCATE( subr_map, subn_map, STAT=ierr)
      !IF ( ierr/=0 ) CALL errore(subname,'deallocating subr_map, subn_map',ABS(ierr))
      !
      DEALLOCATE( proj_r, proj_n, block_r, block_n, STAT=ierr)
      IF ( ierr/=0 ) CALL errore(subname,'deallocating proj_r -- block_n',ABS(ierr))
      !
      DEALLOCATE( fact_r, fact_n1, fact_n2, STAT=ierr)
      IF ( ierr/=0 ) CALL errore(subname,'deallocating fact_r, fact_n1, fact_n2',ABS(ierr))
      !
      DEALLOCATE( caux, STAT=ierr)
      IF ( ierr/=0 ) CALL errore(subname,'deallocating caux',ABS(ierr))
      !
      DEALLOCATE( beta, STAT=ierr)
      IF ( ierr/=0 ) CALL errore(subname,'deallocating beta',ABS(ierr))


      CALL timing(subname,OPR='stop')
      CALL log_pop(subname)
      !
END SUBROUTINE do_cmplx_bands

