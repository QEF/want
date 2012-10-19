! 
! Copyright (C) 2005 WanT Group
!
! This file is distributed under the terms of the
! GNU General Public License. See the file `License'
! in the root directory of the present distribution,
! or http://www.gnu.org/copyleft/gpl.txt .
!
!=====================================================
   PROGRAM blc2wan
   !=====================================================
   !  
   ! Given a (dynamical) operator from file in the bloch basis
   ! it is calculated on the WFs basis
   !
   USE version_module,       ONLY : version_number
   USE kinds,                ONLY : dbl
   USE parameters,           ONLY : nstrx
   USE io_module,            ONLY : stdout, stdin
   USE io_module,            ONLY : work_dir, prefix, postfix
   USE control_module,       ONLY : verbosity, debug_level, use_debug_mode
   USE timing_module,        ONLY : timing
   USE log_module,           ONLY : log_push, log_pop
   USE parser_module,        ONLY : log2char, change_case
   USE datafiles_module,     ONLY : datafiles_init
   USE want_interfaces_module
   !
   IMPLICIT NONE 

   !
   ! input variables
   !
   CHARACTER(nstrx)          :: filein, fileout
   CHARACTER(nstrx)          :: datafile_urot
   CHARACTER(10)             :: spin_component
   LOGICAL                   :: binary
   LOGICAL                   :: do_extrapolation
   LOGICAL                   :: do_cmplxconjg
   REAL(dbl)                 :: energy_ref
   INTEGER                   :: nprint

   !
   ! input namelist
   !
   NAMELIST /INPUT/ prefix, postfix, work_dir, filein, fileout, datafile_urot, &
                    binary, energy_ref, spin_component, nprint, &
                    debug_level, do_extrapolation, do_cmplxconjg, verbosity
   !
   ! end of declariations
   !   

!
!------------------------------
! main body
!------------------------------
!
      CALL startup(version_number,'blc2wan')

      !
      ! read input
      !
      CALL blc2wan_input( )

      !
      ! init post processing (reading previous WanT and DFT data )
      !
      CALL write_header( stdout, "Post Processing Init" )
      !
      CALL datafiles_init( )
      !
      CALL postproc_init ( WANNIER=.TRUE., SUBSPACE=.TRUE. )

      !
      ! print data to output
      !
      CALL summary( stdout, INPUT=.FALSE., IONS=.FALSE., WINDOWS=.FALSE. )

      !
      ! do the main task 
      ! 
      CALL do_blc2wan( filein, fileout, datafile_urot, energy_ref, spin_component, &
                       binary, nprint, verbosity, do_extrapolation, &
                       do_cmplxconjg )

      !
      ! clean global memory
      !
      CALL cleanup()

      !
      ! finalize
      !
      CALL shutdown( 'blc2wan' )

CONTAINS

!********************************************************
   SUBROUTINE blc2wan_input()
   !********************************************************
   !
   ! Read INPUT namelist from stdin
   !
   USE mp,            ONLY : mp_bcast
   USE io_module,     ONLY : io_init, ionode, ionode_id
   !
   IMPLICIT NONE

      CHARACTER(13)    :: subname = 'blc2wan_input'
      LOGICAL          :: lhave_extra_urot
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
      work_dir                    = './'
      filein                      = ' '
      fileout                     = ' '
      datafile_urot               = ' '
      binary                      = .TRUE.
      spin_component              = 'none'
      energy_ref                  = 0.0
      nprint                      = 10
      verbosity                   = 'medium'
      debug_level                 = 0
      do_extrapolation            = .FALSE.
      do_cmplxconjg               = .FALSE.


      CALL input_from_file ( stdin )
      !
      IF (ionode) READ(stdin, INPUT, IOSTAT=ierr)
      !
      CALL mp_bcast( ierr, ionode_id )
      IF ( ierr /= 0 )  CALL errore(subname,'Unable to read namelist INPUT',ABS(ierr))

      !
      ! broadcast
      !
      CALL mp_bcast( prefix,            ionode_id )
      CALL mp_bcast( postfix,           ionode_id )
      CALL mp_bcast( work_dir,          ionode_id )
      CALL mp_bcast( filein,            ionode_id )
      CALL mp_bcast( fileout,           ionode_id )
      CALL mp_bcast( datafile_urot,     ionode_id )
      CALL mp_bcast( binary,            ionode_id )
      CALL mp_bcast( energy_ref,        ionode_id )
      CALL mp_bcast( spin_component,    ionode_id )
      CALL mp_bcast( nprint,            ionode_id )
      CALL mp_bcast( debug_level,       ionode_id )
      CALL mp_bcast( do_extrapolation,  ionode_id )
      CALL mp_bcast( do_cmplxconjg,     ionode_id )
      CALL mp_bcast( verbosity,         ionode_id )

      !
      ! Init
      !
      use_debug_mode = .FALSE.
      IF ( debug_level > 0  )     use_debug_mode = .TRUE.
      !
      CALL io_init( NEED_WFC=.FALSE. )


      !
      ! Some checks
      !
      IF ( LEN_TRIM(filein) == 0 )  CALL errore(subname,'invalid empty filein',1)
      IF ( LEN_TRIM(fileout) == 0 ) CALL errore(subname,'invalid empty fileout',2)
      !
      IF ( TRIM(filein) == TRIM(fileout) ) &
                CALL errore(subname,'filein and fileout equal',3)

      CALL change_case(spin_component,'lower')
      !
      IF ( TRIM(spin_component) /= "none" .AND. TRIM(spin_component) /= "up" .AND. &
           TRIM(spin_component) /= "down" .AND. TRIM(spin_component) /= "dw") &
           CALL errore(subname,'invalid spin_component = '//TRIM(spin_component),3 )

      CALL change_case(verbosity,'lower')
      !
      IF ( TRIM(verbosity) /= "low" .AND. TRIM(verbosity) /= "medium" .AND. &
           TRIM(verbosity) /= "high" ) &
           CALL errore(subname,'invalid verbosity = '//TRIM(verbosity),3 )

      lhave_extra_urot = .FALSE.
      IF ( LEN_TRIM(datafile_urot) /= 0 ) lhave_extra_urot = .TRUE.


      !
      ! input summary
      !
      CALL write_header( stdout, "INPUT Summary" )
      !
      IF ( ionode ) THEN
          !
          WRITE( stdout,"(/,2x,'      Input file :',3x,a)") TRIM(filein)
          WRITE( stdout,"(  2x,'     Output file :',3x,a)") TRIM(fileout)
          !
          IF ( binary ) THEN
             WRITE( stdout,"(  2x,'      Output fmt :',3x,a)") "binary"
          ELSE
             WRITE( stdout,"(  2x,'      Output fmt :',3x,a)") "textual"
          ENDIF
          !
          WRITE( stdout,"(  2x,'  Spin component :',3x,a)") TRIM(spin_component)
          !
          WRITE( stdout,"(  2x,'do extrapolation :',3x,l1)") do_extrapolation
          WRITE( stdout,"(  2x,'  do cmplx-conjg :',3x,l1,/)") do_cmplxconjg
          !
          WRITE( stdout,"(  2x,'have extra U-rot :',3x,l1)") lhave_extra_urot
          !
          IF ( lhave_extra_urot ) &
             WRITE(stdout,"(2x,'  U-rot datafile :',3x,a)") TRIM(datafile_urot)
          !
      ENDIF
      !
      CALL timing(subname,OPR='stop')
      !
   END SUBROUTINE blc2wan_input

END PROGRAM blc2wan
      

!********************************************************
   SUBROUTINE do_blc2wan( filein, fileout, datafile_urot, energy_ref, spin_component, &
                          binary, nprint, verbosity, do_extrapolation, &
                          do_cmplxconjg  )
   !********************************************************
   !
   ! perform the main task of the calculation
   !
   USE kinds
   USE constants,            ONLY : ZERO, CZERO, TWO, RYD, EPS_m6, EPS_m8
   USE parameters,           ONLY : nstrx
   USE io_module,            ONLY : ionode, ionode_id
   USE io_module,            ONLY : stdout, work_dir, prefix, postfix
   USE io_module,            ONLY : in_unit => aux1_unit, out_unit => aux2_unit, aux_unit
   USE mp,                   ONLY : mp_bcast, mp_sum
   USE mp_global,            ONLY : mpime, nproc
   USE parser_module,        ONLY : log2char, change_case
   USE converters_module,    ONLY : cry2cart
   USE lattice_module,       ONLY : bvec, tpiba
   USE windows_module,       ONLY : nbnd, imin, imax, dimwin, dimwinx
   USE kpoints_module,       ONLY : nrtot, nkpts_g, vkpt_g, ivr, vr
   USE subspace_module,      ONLY : eamp
   USE localization_module,  ONLY : cu, rave
   USE hamiltonian_module,   ONLY : dimwann, rham
   USE util_module,          ONLY : mat_mul
   USE timing_module,        ONLY : timing, timing_upto_now
   USE log_module,           ONLY : log_push, log_pop
   USE operator_module,      ONLY : operator_write_init, operator_write_close, &
                                    operator_write_aux,  operator_write_data
   USE iotk_module
   USE files_module
   !
   IMPLICIT NONE
   
      !
      ! input vars
      !
      CHARACTER(*),  INTENT(IN) :: filein, fileout, datafile_urot
      CHARACTER(*),  INTENT(IN) :: spin_component
      REAL(dbl),     INTENT(IN) :: energy_ref
      INTEGER,       INTENT(IN) :: nprint
      LOGICAL,       INTENT(IN) :: binary
      CHARACTER(*),  INTENT(IN) :: verbosity
      LOGICAL,       INTENT(IN) :: do_extrapolation
      LOGICAL,       INTENT(IN) :: do_cmplxconjg

      !
      ! local vars
      !
      CHARACTER(10)      :: subname = 'do_blc2wan'       
      !
      INTEGER,      ALLOCATABLE :: ikmap(:)
      REAL(dbl),    ALLOCATABLE :: grid(:), vkpt_file(:,:)
      REAL(dbl),    ALLOCATABLE :: vrr(:), norms(:)
      COMPLEX(dbl), ALLOCATABLE :: phase(:,:)
      COMPLEX(dbl), ALLOCATABLE :: opr_in(:,:), opr_out(:,:,:)
      COMPLEX(dbl), ALLOCATABLE :: oprk(:,:,:), cutot(:,:,:)
      COMPLEX(dbl), ALLOCATABLE :: cu_extra(:,:,:)
      COMPLEX(dbl), ALLOCATABLE :: aux(:), work(:,:)
      !
      REAL(dbl)                 :: arg
      CHARACTER(nstrx)          :: filename
      CHARACTER(nstrx)          :: attr, str, grid_units, analyticity
      !
      INTEGER :: ierr
      INTEGER :: i, j, m, ie, ik_g, ik_f, ir, isup, iinf
      INTEGER :: nbnd_file, nkpts_file, nspin_file, nomega
      INTEGER :: ibnd_start, ibnd_end, ispin
      INTEGER :: nbmin, nbmax
      LOGICAL :: lfound, ldynam, lband_diag
      LOGICAL :: lhave_extra_urot
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
! ... Init operator data from filein
!     filein is left opened as well as fileout
!
      filename = TRIM(filein)
      CALL iotk_open_read(in_unit, TRIM(filename), IERR=ierr)
         IF (ierr/=0)  CALL errore(subname,'opening '//TRIM(filename),ABS(ierr))
      !
      ! reading main info
      !
      CALL iotk_scan_empty(in_unit,"DATA",ATTR=attr,IERR=ierr)
      IF (ierr/=0)  CALL errore(subname,'searching data',ABS(ierr))
      !
      CALL iotk_scan_attr(attr,'nbnd',nbnd_file,IERR=ierr)
      IF (ierr/=0)  CALL errore(subname,'searching nbnd',ABS(ierr))
      !
      CALL iotk_scan_attr(attr,'iband_start',ibnd_start, FOUND=lfound ,IERR=ierr)
      IF ( .NOT. lfound ) ibnd_start=1
      IF (ierr > 0)  CALL errore(subname,'searching iband_start',ABS(ierr))
      !
      CALL iotk_scan_attr(attr,'iband_end',ibnd_end, FOUND=lfound ,IERR=ierr)
      IF ( .NOT. lfound ) ibnd_end=nbnd_file
      IF (ierr > 0)  CALL errore(subname,'searching iband_end',ABS(ierr))
      !
      CALL iotk_scan_attr(attr,'nkpts',nkpts_file,IERR=ierr)
      IF (ierr/=0)  CALL errore(subname,'searching nkpts',ABS(ierr))
      !
      CALL iotk_scan_attr(attr,'nspin',nspin_file,IERR=ierr)
      IF (ierr/=0)  CALL errore(subname,'searching nspin',ABS(ierr))
      !
      CALL iotk_scan_attr(attr,'band_diagonal',lband_diag,IERR=ierr)
      IF (ierr/=0)  CALL errore(subname,'searching band_diagonal',ABS(ierr))
      !
      CALL iotk_scan_attr(attr,'dynamical',ldynam,IERR=ierr)
      IF (ierr/=0)  CALL errore(subname,'searching dynamical',ABS(ierr))
      !
      nomega = 1
      analyticity = " "
      !
      IF ( ldynam ) THEN
          !
          CALL iotk_scan_attr(attr,'nomega',nomega,IERR=ierr)
          IF (ierr/=0)  CALL errore(subname,'searching nomega',ABS(ierr))
          !
          CALL iotk_scan_attr(attr,'analyticity', analyticity, IERR=ierr)
          IF (ierr/=0)  CALL errore(subname,'searching nomega',ABS(ierr))
          !
      ENDIF

      !
      ! internal checks
      !
      IF ( ibnd_end - ibnd_start +1 /= nbnd_file ) &
                                  CALL errore(subname,'invalid nbnd_file',3)
      IF ( nbnd < nbnd_file )     CALL errore(subname,'nbnd_file too large',3)
      IF ( nkpts_file <= 0  )     CALL errore(subname,'invalid nkpts from file',3)
      !
      IF ( .NOT. lband_diag .AND. nkpts_g /= nkpts_file ) &
                                  CALL errore(subname,'symmetries not allowed when'//&
                                                      ' opr not diagonal',10)
      !
      IF ( nspin_file /= 1 .AND. nspin_file /= 2 ) &
                                  CALL errore(subname,'invalid nspin',3)
      !
      IF ( TRIM( spin_component) /= "none" .AND. nspin_file == 1 ) &
                                  CALL errore(subname,'calculation is spin-unpolarized',3)
      !
      IF ( ABS(energy_ref) > EPS_m6 .AND. .NOT. ldynam ) &
                                  CALL warning( subname, 'energy_ref specified for a static operator' )


      !
      ! few local allocations
      !
      ALLOCATE( opr_in(nbnd,nbnd), STAT=ierr )
      IF (ierr/=0) CALL errore(subname,'allocating opr_in',ABS(ierr))
      !
      ALLOCATE( oprk(dimwann,dimwann,nkpts_g), STAT=ierr )
      IF (ierr/=0) CALL errore(subname,'allocating oprk',ABS(ierr))
      !
      ALLOCATE( opr_out(dimwann,dimwann,nrtot), STAT=ierr )
      IF (ierr/=0) CALL errore(subname,'allocating opr_out',ABS(ierr))
      !
      ALLOCATE( cutot(dimwinx,dimwann,nkpts_g), STAT=ierr )
      IF (ierr/=0) CALL errore(subname,'allocating opr_out',ABS(ierr))
      !
      ALLOCATE( aux(nbnd_file), STAT=ierr )
      IF (ierr/=0) CALL errore(subname,'allocating aux',ABS(ierr))
      !
      ALLOCATE( work(dimwinx,dimwinx), STAT=ierr )
      IF (ierr/=0) CALL errore(subname,'allocating work',ABS(ierr))
      !
      ALLOCATE( vkpt_file(3,nkpts_file), STAT=ierr )
      IF (ierr/=0) CALL errore(subname,'allocating vkpt_file',ABS(ierr))
      !
      ALLOCATE( ikmap(nkpts_g), STAT=ierr )
      IF (ierr/=0) CALL errore(subname,'allocating vkpt_file',ABS(ierr))
      !
      ALLOCATE( phase(nkpts_g,nrtot), STAT=ierr )
      IF (ierr/=0) CALL errore(subname,'allocating phase',ABS(ierr))
      !
      ALLOCATE( norms(nrtot), STAT=ierr )     
      IF (ierr/=0) CALL errore(subname,'allocating norms',ABS(ierr))
      !
      ALLOCATE( vrr(nrtot), STAT=ierr )     
      IF (ierr/=0) CALL errore(subname,'allocating vrr',ABS(ierr))


      !
      ! read kpt list
      !
      CALL iotk_scan_dat(in_unit, 'VKPT', vkpt_file, ATTR=attr, IERR=ierr )
      IF (ierr/=0)  CALL errore(subname,'searching VKPT',ABS(ierr))
      !
      CALL iotk_scan_attr(attr,"units",str, IERR=ierr)
      IF (ierr/=0)  CALL errore(subname,'searching units',ABS(ierr))
      !
      ! units conversion when possible
      !
      CALL change_case( str, "lower")
      !
      SELECT CASE ( TRIM(str) )
      CASE ( "bohr^-1" )
          !
          ! nothing to do
          !
      CASE ( "2 pi / a" )
          ! 
          vkpt_file(:,:) = vkpt_file(:,:) * tpiba
          ! 
      CASE ( "crystal", "relative" )
          ! 
          CALL cry2cart( vkpt_file, bvec )
          ! 
      CASE DEFAULT
          !
          CALL errore(subname,'unknown kpt units ='//TRIM(str), 3)
          !
      END SELECT
      
      !
      ! define a mapping between kpts in WanT and kpts from filein
      ! kpts are given in cartesian units (bohr^-1)
      !
      CALL get_vectmap( nkpts_g, vkpt_g, nkpts_file, vkpt_file, .TRUE., EPS_m8, ikmap ) 
      !
      IF ( ANY( ikmap(:) == 0) ) CALL errore(subname,'invalid ikmap value',10)
      !
      DEALLOCATE( vkpt_file, STAT=ierr)
      IF ( ierr/=0 ) CALL errore(subname,'deallocating vkpt_file', ABS(ierr) )


      !
      IF ( ldynam ) THEN
          !
          ALLOCATE( grid(nomega), STAT=ierr )
          IF (ierr/=0)  CALL errore(subname,'allocating grid',ABS(ierr))
          ! 
          CALL iotk_scan_dat(in_unit,"GRID",grid, ATTR=attr, IERR=ierr)
          IF (ierr/=0)  CALL errore(subname,'reading grid',ABS(ierr))
          !
          CALL iotk_scan_attr( attr, "units", grid_units, FOUND=lfound, IERR=ierr) 
          IF ( .NOT. lfound ) grid_units = ""
          IF (ierr > 0)  CALL errore(subname,'reading grid units',ABS(ierr))
          !
      ENDIF

      !
      ! extra unitary rotation to be applied
      !
      lhave_extra_urot = .FALSE.
      !
      IF ( LEN_TRIM( datafile_urot ) /= 0 ) lhave_extra_urot = .TRUE.
      !
      IF ( lhave_extra_urot ) THEN
          !
          ALLOCATE( cu_extra(nbnd, nbnd, nkpts_file), STAT=ierr )
          IF ( ierr/=0 ) CALL errore(subname,'allocating cu_extra', ABS(ierr))
          !
          CALL iotk_open_read( aux_unit, TRIM(datafile_urot), IERR=ierr )
          IF ( ierr/= 0 ) CALL errore(subname,'opening '//TRIM(datafile_urot), ABS(ierr)) 
          !
          CALL iotk_scan_dat( aux_unit, "nbmin", nbmin, IERR=ierr)
          IF ( ierr/= 0 ) CALL errore(subname,'scanning nbmin', ABS(ierr)) 
          CALL iotk_scan_dat( aux_unit, "nbmax", nbmax, IERR=ierr)
          IF ( ierr/= 0 ) CALL errore(subname,'scanning nbmax', ABS(ierr)) 
          !
          IF ( nbmin /= ibnd_start ) CALL errore(subname,"ibnd_start inconsistent", 10)
          IF ( nbmax /= ibnd_end )   CALL errore(subname,"ibnd_end inconsistent", 10)
          !
          IF ( ANY( dimwin(:) /= ibnd_end-ibnd_start+1 ) ) &
              CALL errore(subname,"missing implementation: incompat. nbnd_file", 11)
          !
          !
          DO ik_f = 1, nkpts_file
              !
              WRITE( str, '( I3.3 )' ) ik_f
              !
              CALL iotk_scan_dat( aux_unit, "eigenvec"//TRIM(str), &
                   cu_extra(nbmin:nbmax,nbmin:nbmax,ik_f), IERR=ierr )
              IF ( ierr/= 0 ) CALL errore(subname,'scanning eigenvec', ik_f) 
              !
          ENDDO
          !
          CALL iotk_close_read( aux_unit, IERR=ierr )
          IF ( ierr/= 0 ) CALL errore(subname,'closing '//TRIM(datafile_urot), ABS(ierr)) 
          !
      ENDIF
 


      !
      ! report data from file
      !
      IF ( ionode ) THEN
          !
          WRITE( stdout,"(/,2x,'  Data from file',/)")
          WRITE( stdout,"(  2x,'            nbnd :',3x,i5)") nbnd_file
          WRITE( stdout,"(  2x,'           nkpts :',3x,i5)") nkpts_file
          WRITE( stdout,"(  2x,'           nspin :',3x,i5)") nspin_file
          WRITE( stdout,"(  2x,' dynam. operator :',3x,a)") TRIM( log2char(ldynam) )
          !
          IF ( ldynam ) THEN
             WRITE( stdout,"(  2x,'          nomega :',3x,i5)") nomega
             WRITE( stdout,"(  2x,'     analitycity :',3x,a)") TRIM(analyticity)
          ENDIF
          !
          WRITE( stdout,"(  2x,'  diag. on bands :',3x,a)") TRIM( log2char(lband_diag) )
          WRITE( stdout,"(  2x,'      ibnd_start :',3x,i5)") ibnd_start
          WRITE( stdout,"(  2x,'        ibnd_end :',3x,i5)") ibnd_end
          WRITE( stdout,"(  2x,'   extrapolation :',3x,a)") &
                    TRIM( log2char(do_extrapolation) )
          WRITE( stdout,"(  2x,'energy reference :',3x,f10.4)") energy_ref
          !
          WRITE( stdout, "()")
          !
      ENDIF


!
! ... Main task 
!
      CALL write_header( stdout, "Conversion to Wannier Function basis" )
      CALL flush_unit( stdout )


      !
      ! fileout is initializated
      !
      filename = TRIM(fileout)
      !
      IF ( ionode ) THEN
          CALL operator_write_init(out_unit, TRIM(filename), BINARY=binary)
      ENDIF

      !
      ! apply energy ref (whatever units)
      !
      IF (ldynam) THEN
          !
          grid(:) = grid(:) - energy_ref
          !
          IF (ionode)  CALL operator_write_aux(out_unit, dimwann, ldynam, nomega, 1, &
                                               nomega, grid, grid_units, analyticity, &
                                               nrtot, vr, ivr)
          !
      ELSE
          !
          IF (ionode) CALL operator_write_aux(out_unit, dimwann, ldynam, NOMEGA=nomega, &
                                              NRTOT=nrtot, VR=vr, IVR=ivr)
          !
      ENDIF

      !
      ! set the phase factors
      !
      DO ir = 1, nrtot
          !
          DO ik_g = 1, nkpts_g
              !
              arg = DOT_PRODUCT( vkpt_g(:,ik_g), vr(:,ir) )
              phase(ik_g,ir) = CMPLX( COS(arg), -SIN(arg), dbl )
              !
          ENDDO
          !
          ! compute also the norms of the vr vectors
          !
          vrr(ir) = SQRT( DOT_PRODUCT( vr(:,ir), vr(:,ir) ) ) 
          !
      ENDDO


      !
      ! set the full rotation in k-space
      !
      DO ik_g = 1, nkpts_g
          !
          cutot(:,:,ik_g) = CZERO
          !
          CALL mat_mul( cutot(:,:,ik_g), eamp(:,:,ik_g), 'N', cu(:,:,ik_g), 'N' ,  &
                        dimwin(ik_g), dimwann, dimwann)
          !
          IF ( lhave_extra_urot ) THEN
              !
              work = CZERO
              CALL mat_mul( work,  cu_extra(:,:,ikmap(ik_g)), 'C', cutot(:,:,ik_g), 'N' ,  &
                            dimwin(ik_g), dimwann, dimwin(ik_g))
              !
              cutot(:,:,ik_g) = work(:,:)
              !
          ENDIF
          !
      ENDDO
      !     
      IF ( ALLOCATED( cu_extra ) ) THEN
          DEALLOCATE( cu_extra, STAT=ierr )
          IF (ierr/=0 ) CALL errore(subname,"deallocating cu_extra",ABS(ierr)) 
      ENDIF


  
      !
      ! set the measure of the localization
      ! in real space to zero
      norms(:) = ZERO
     
      !
      ! set spin stuff
      !
      SELECT CASE ( TRIM(spin_component) )
      CASE( "none" )
         ispin = 0
      CASE( "up")
         ispin = 1
      CASE( "down", "dw")
         ispin = 2
      CASE DEFAULT
         CALL errore(subname,'invalid spin_component = '//TRIM(spin_component),33)
      END SELECT
      

      !
      ! loop over frequencies
      !
      energies: &
      DO ie=1,nomega
         
          !
          ! a brief report
          !
          IF ( ionode .AND. ldynam .AND. ( MOD( ie, nprint) == 0 .OR. ie == 1 )  ) THEN
              !
              WRITE( stdout ,"(2x, 'Converting OPR for E( ',i5,' ) = ', f9.5 )") &
                               ie, grid(ie)
          ENDIF
 
          IF ( ldynam ) THEN
             str="OPR"//TRIM(iotk_index(ie))
          ELSE
             !
             ! in this case just the static operator is searched
             str="OPR"
          ENDIF
          !
          !
          CALL iotk_scan_begin(in_unit, TRIM(str), IERR=ierr)
          IF (ierr/=0) CALL errore(subname,'searching for '//TRIM(str),ABS(ierr))
          !
          ! spin stuff 
          !
          IF ( nspin_file == 2 ) THEN
              !
              CALL iotk_scan_begin(in_unit, "SPIN"//TRIM(iotk_index(ispin)), IERR=ierr)
              IF (ierr/=0) CALL errore(subname,'searching for SPIN',ABS(ispin)+10)
              !
          ENDIF

          kpoints: &
          DO ik_g = 1, nkpts_g
               
               !
               ! reading data from filein
               !
               opr_in(:,:) = CZERO
               !
               IF ( lband_diag ) THEN 
                   !
                   CALL iotk_scan_dat(in_unit, "KPT"//TRIM(iotk_index( ikmap(ik_g) ) ), aux(:), IERR=ierr)
                   IF (ierr/=0) CALL errore(subname,'reading diag KPT' , ikmap(ik_g) )
                   !
                   DO m = ibnd_start, ibnd_end
                       !
                       opr_in(m,m) = aux( m -ibnd_start + 1 )
                       !
                   ENDDO
                   !
                   ! try to complete the missing terms in finein
                   !
                   IF ( do_extrapolation ) THEN
                       !
                       DO m = 1, ibnd_start -1
                           opr_in(m,m) = aux( 1 )
                       ENDDO
                       !
                       DO m = ibnd_end+1, nbnd
                           opr_in(m,m) = aux( nbnd_file )
                       ENDDO
                       !
                   ENDIF
                   !
               ELSE
                   ! 
                   CALL iotk_scan_dat(in_unit,"KPT"//TRIM(iotk_index( ikmap(ik_g) )),  &
                                      opr_in(ibnd_start:ibnd_end, ibnd_start:ibnd_end), &
                                      IERR=ierr)
                   IF (ierr/=0) CALL errore(subname,'reading full KPT' ,ikmap(ik_g) )
                   !
                   ! workaround to fix an old bug in SaX
                   !
                   IF ( do_cmplxconjg ) THEN
                       !
                       opr_in(ibnd_start:ibnd_end, ibnd_start:ibnd_end) = &
                                  CONJG( opr_in(ibnd_start:ibnd_end, ibnd_start:ibnd_end) )
                       !
                   ENDIF
                   !
               ENDIF

               !
               ! converting data to oprk(ik) = cutot^dag(ik) * opr_in(ik) * cutot(ik)
               !
               CALL mat_mul( work, opr_in( imin(ik_g):imax(ik_g), imin(ik_g):imax(ik_g)), 'N', &
                             cutot(:,:,ik_g), 'N', dimwin(ik_g), dimwann, dimwin(ik_g) )
               CALL mat_mul( oprk(:,:,ik_g), cutot(:,:,ik_g), 'C', &
                             work(:,:), 'N', dimwann, dimwann, dimwin(ik_g) )
               !
          ENDDO kpoints
          
          !
          ! spin stuff 
          !
          IF ( nspin_file == 2 ) THEN
              !
              CALL iotk_scan_end(in_unit, "SPIN"//TRIM(iotk_index(ispin)), IERR=ierr)
              IF (ierr/=0) CALL errore(subname,'searching end for SPIN',ABS(ispin)+10)
              !
          ENDIF



          !
          ! final Fourier transform
          !
          rlattice: &
          DO ir =1, nrtot
               ! 
               ! 
               opr_out(:,:,ir) = CZERO
               !
               DO ik_g = 1, nkpts_g
                   !
                   DO j=1,dimwann
                   DO i=1,dimwann
                        opr_out(i,j,ir) = opr_out(i,j,ir) + phase(ik_g,ir) * oprk(i,j,ik_g)
                   ENDDO
                   ENDDO
                   !
               ENDDO
               !
               opr_out(:,:,ir) = opr_out(:,:,ir) / REAL(nkpts_g, dbl)


               !
               ! add the contribution to the localization measure
               !
               DO j=1,dimwann
               DO i=1,dimwann
                   !
                   norms(ir) = norms(ir) + &
                                REAL(  CONJG( opr_out(i,j,ir)) * opr_out(i,j,ir), dbl )
                   !
               ENDDO
               ENDDO

          ENDDO rlattice
          
          !
          ! writing to file
          !
          IF (ionode) CALL operator_write_data( out_unit, opr_out, ldynam, IE=ie)

          !
          ! ending the sections
          !
          CALL iotk_scan_end(in_unit, TRIM(str), IERR=ierr)
          IF (ierr/=0) CALL errore(subname,'ending '//TRIM(str),ABS(ierr))

          !
          IF ( (MOD( ie, nprint) ==0 .OR. ie == 1) ) THEN
             !
             CALL timing_upto_now( stdout )
             CALL flush_unit( stdout )
             !
          ENDIF
          !
      ENDDO energies


      !
      ! close files
      !
      CALL iotk_close_read(in_unit, IERR=ierr)
      IF (ierr/=0) CALL errore(subname,'closing IN_UNIT',ABS(ierr))
      !
      IF (ionode) CALL operator_write_close( out_unit )


!
! ... print the localization measure 
!
      !
      IF ( ionode ) THEN 
          !
          WRITE(stdout,"(/,2x,'Real space decay of OPR:',/)")
          WRITE(stdout,"(  5x,'R [cry]     |R| [Bohr]      Norm of Opr(R) [eV]')")
          !
          isup = 3    
          IF ( TRIM(verbosity) == "high" ) isup = MAXVAL( ivr(:,:) ) 
          !
          iinf = 0
          IF ( TRIM(verbosity) == "high" ) iinf = MINVAL( ivr(:,:) ) 
          !
          DO ir = 1, nrtot
              !
              IF ( ALL( ivr(:,ir) >= iinf .AND. ivr(:,ir) <= isup )  ) THEN
                  !
                  WRITE(stdout,"(1x,3i4,3x,f11.7,4x,f15.9)") &
                                ivr(:,ir), vrr(ir), SQRT(  norms(ir) / REAL(dimwann*nomega, dbl) )
              ENDIF
              !
          ENDDO
          !
      ENDIF
      

      !
      ! write full information if the case
      !
      IF ( ionode .AND. TRIM(verbosity) == "high" .AND. .NOT. ldynam ) THEN
          !
          filename = TRIM(work_dir) // '/' // TRIM(prefix) // TRIM(postfix) // &
                     '_decay_opr.dat'
          CALL write_decay( filename, dimwann, nrtot, rave, vr, opr_out )
          !
          filename = TRIM(work_dir) // '/' // TRIM(prefix) // TRIM(postfix) // &
                     '_decay_ham.dat'
          CALL write_decay( filename, dimwann, nrtot, rave, vr, rham )
          !
      ENDIF

!
! ... Shutdown
!

      !
      ! Clean local memory
      !
      DEALLOCATE( opr_in, opr_out, oprk, STAT=ierr)
      IF( ierr /=0 ) CALL errore(subname, 'deallocating opr_in -- opr_out', ABS(ierr) )
      !
      DEALLOCATE( ikmap, STAT=ierr)
      IF( ierr /=0 ) CALL errore(subname, 'deallocating ikmap', ABS(ierr) )
      !
      DEALLOCATE( cutot, STAT=ierr)
      IF( ierr /=0 ) CALL errore(subname, 'deallocating cutot', ABS(ierr) )
      !
      DEALLOCATE( aux, work, STAT=ierr)
      IF( ierr /=0 ) CALL errore(subname, 'deallocating aux, work', ABS(ierr) )
      !
      DEALLOCATE( phase, STAT=ierr)
      IF( ierr /=0 ) CALL errore(subname, 'deallocating phase, kpt', ABS(ierr) )
      !
      DEALLOCATE( vrr, norms, STAT=ierr)
      IF( ierr /=0 ) CALL errore(subname, 'deallocating vrr, norms', ABS(ierr) )
      !
      IF ( ALLOCATED(grid) ) THEN 
          DEALLOCATE( grid, STAT=ierr)
          IF( ierr /=0 ) CALL errore(subname, 'deallocating grid', ABS(ierr) )
      ENDIF
      !
      !
      CALL timing(subname,OPR='stop')
      CALL log_pop(subname)
      !
END SUBROUTINE do_blc2wan


