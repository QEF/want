!
! Copyright (C) 2005 WanT Group
!
! This file is distributed under the terms of the
! GNU General Public License. See the file `License'
! in the root directory of the present distribution,
! or http://www.gnu.org/copyleft/gpl.txt .
!
!=====================================================
   PROGRAM plot_main
   !=====================================================
   !
   ! Real space representation and plot of Wannier functions
   !
   USE kinds
   USE version_module,       ONLY : version_number
   USE parameters,           ONLY : nstrx
   USE io_module,            ONLY : stdout, stdin
   USE io_module,            ONLY : prefix, postfix, work_dir
   USE control_module,       ONLY : debug_level, use_debug_mode, read_pseudo
   USE timing_module,        ONLY : timing
   USE log_module,           ONLY : log_push, log_pop
   USE datafiles_module,     ONLY : datafiles_init
   USE parser_module,        ONLY : change_case
   USE want_interfaces_module
   !
   IMPLICIT NONE

   !
   ! input variables
   !
   CHARACTER(nstrx) :: wann              ! contains the list of WF indexes to plot 
                                         ! in the fmt e.g. "1-3,4,7-9"
   REAL(dbl)        :: r1min, r1max      ! plot cell dim along a1 (cry units)
   REAL(dbl)        :: r2min, r2max      ! the same but for a2
   REAL(dbl)        :: r3min, r3max      ! the same but for a3
   INTEGER          :: nr1, nr2, nr3     ! the FFT mesh for interpolation on real space
   CHARACTER( 20 )  :: datatype          ! ( "modulus" "module" | "real" | "imaginary"  )
   CHARACTER( 20 )  :: output_fmt        ! ( "txt" | "plt" | "cube" | "xsf" )
   LOGICAL          :: assume_ncpp       ! If .TRUE. pp's are not read
   LOGICAL          :: uspp_augmentation ! If .TRUE. USPP augmentation is computed
   LOGICAL          :: collect_wf        ! move the centers of WF in a unit cell centered
                                         ! around the selected lotting cell
   !
   ! input namelist
   !
   NAMELIST /INPUT/ prefix, postfix, work_dir, wann, &
                    datatype, assume_ncpp, uspp_augmentation, output_fmt, collect_wf,   &
                    r1min, r1max, r2min, r2max, r3min, r3max, nr1, nr2, nr3, debug_level


!
!------------------------------
! main body
!------------------------------
!
      CALL startup(version_number,'plot')


      !
      ! read input
      !
      CALL plot_input( )

      !
      ! init post processing (reading previous WanT and DFT data )
      !
      CALL write_header( stdout, "Post Processing Init" )
      !
      CALL datafiles_init( )
      !
      CALL postproc_init ( BSHELLS=.TRUE., IONS=.TRUE., PSEUDO=read_pseudo, &
                           WANNIER=.TRUE., SUBSPACE=.TRUE. )

      !
      ! print data to output
      !
      CALL summary( stdout, INPUT=.FALSE., IONS=.FALSE., BSHELLS=.FALSE., WINDOWS=.FALSE. )

      !
      ! do the main task
      !      
      CALL do_plot( wann, datatype, uspp_augmentation, output_fmt, &
                    collect_wf, r1min, r1max, r2min, r2max, r3min, r3max, nr1, nr2, nr3)

      !
      ! clean global memory
      !
      CALL cleanup()

      !
      ! finalize
      !
      CALL shutdown( 'plot' )


CONTAINS

!********************************************************
   SUBROUTINE plot_input()
   !********************************************************
   !
   ! Read INPUT namelist from stdin
   !
   USE constants,            ONLY : EPS_m4
   USE mp,                   ONLY : mp_bcast
   USE io_module,            ONLY : io_init, ionode, ionode_id
   !
   IMPLICIT NONE

      CHARACTER(10)    :: subname = 'plot_input'
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
      assume_ncpp                 = .FALSE.
      uspp_augmentation           = .FALSE.
      debug_level                 = 0
      collect_wf                  = .TRUE.
      wann                        = ' '
      datatype                    = 'modulus'
      output_fmt                  = 'plt'
      r1min                       = -0.5
      r1max                       =  0.5
      r2min                       = -0.5
      r2max                       =  0.5
      r3min                       = -0.5
      r3max                       =  0.5
      !
      ! the following defaults will be set later after 
      ! FFT mesh is read
      !
      nr1                         =  -1
      nr2                         =  -1
      nr3                         =  -1
      
      CALL input_from_file ( stdin )
      !
      IF ( ionode ) READ(stdin, INPUT, IOSTAT=ierr)
      !
      CALL mp_bcast( ierr, ionode_id )
      IF ( ierr /= 0 )  CALL errore(subname,'Unable to read namelist INPUT',ABS(ierr))

      !
      ! broadcast
      !
      CALL mp_bcast( prefix,               ionode_id )
      CALL mp_bcast( postfix,              ionode_id )
      CALL mp_bcast( work_dir,             ionode_id )
      CALL mp_bcast( assume_ncpp,          ionode_id )
      CALL mp_bcast( uspp_augmentation,    ionode_id )
      CALL mp_bcast( debug_level,          ionode_id )
      CALL mp_bcast( collect_wf,           ionode_id )
      CALL mp_bcast( wann,                 ionode_id )
      CALL mp_bcast( datatype,             ionode_id )
      CALL mp_bcast( output_fmt,           ionode_id )
      CALL mp_bcast( r1min,                ionode_id )
      CALL mp_bcast( r1max,                ionode_id )
      CALL mp_bcast( r2min,                ionode_id )
      CALL mp_bcast( r2max,                ionode_id )
      CALL mp_bcast( r3min,                ionode_id )
      CALL mp_bcast( r3max,                ionode_id )
      CALL mp_bcast( nr1,                  ionode_id )
      CALL mp_bcast( nr2,                  ionode_id )
      CALL mp_bcast( nr3,                  ionode_id )


      !
      ! Some checks
      !
      IF ( LEN_TRIM( wann) == 0 ) CALL errore(subname, 'wann not supplied ', 1)
      !
      CALL change_case( datatype, 'lower')
      IF ( TRIM(datatype) /= "modulus" .AND. TRIM(datatype) /= "module" .AND. &
           TRIM(datatype) /= "real"    .AND. TRIM(datatype) /= "imaginary"  ) &
           CALL errore(subname,'invalid DATATYPE = '//TRIM(datatype),2)
           !
      CALL change_case(output_fmt,'lower')
      IF ( TRIM(output_fmt) /= "txt" .AND. TRIM(output_fmt) /= "plt" .AND. &
           TRIM(output_fmt) /= "cube" .AND. TRIM(output_fmt) /= "xsf" ) &
           CALL errore(subname, 'Invalid output_fmt = '//TRIM(output_fmt), 4)

      IF ( nr1 /= -1 .AND. nr1 <= 0 ) CALL errore(subname,'Invalid nr1',-nr1+1)
      IF ( nr2 /= -1 .AND. nr2 <= 0 ) CALL errore(subname,'Invalid nr2',-nr2+1)
      IF ( nr3 /= -1 .AND. nr3 <= 0 ) CALL errore(subname,'Invalid nr3',-nr3+1)
      !
      IF ( r1min > r1max ) CALL errore(subname, 'r1min > r1max',1)
      IF ( r2min > r2max ) CALL errore(subname, 'r2min > r2max',2)
      IF ( r3min > r3max ) CALL errore(subname, 'r3min > r3max',3)
      IF ( ABS(r1max -r1min) < EPS_m4 ) CALL errore(subname, 'r1 too small',1)
      IF ( ABS(r2max -r2min) < EPS_m4 ) CALL errore(subname, 'r2 too small',2)
      IF ( ABS(r3max -r3min) < EPS_m4 ) CALL errore(subname, 'r3 too small',3)
      !
      read_pseudo = .NOT. assume_ncpp
      !
      use_debug_mode = .FALSE.
      IF ( debug_level > 0 ) use_debug_mode = .TRUE.
      !
      !
      CALL timing( subname, OPR='stop' )
      !
   END SUBROUTINE plot_input
   !
END PROGRAM plot_main


!********************************************************
   SUBROUTINE do_plot( wann, datatype, uspp_augmentation, output_fmt, &
                       collect_wf, r1min, r1max, r2min, r2max, r3min, r3max, nr1, nr2, nr3 )
   !********************************************************
   !
   ! Perform the main task of constructing and plotting 
   ! WFs in real space
   !
   USE kinds,                     ONLY : dbl
   USE constants,                 ONLY : ZERO, CZERO, ONE, TWO, CONE, CI, TPI, &
                                         bohr => bohr_radius_angs, EPS_m6, EPS_m4
   USE parameters,                ONLY : ntypx, natx, nstrx
   USE fft_scalar,                ONLY : cfft3d, good_fft_order
   USE timing_module,             ONLY : timing, timing_upto_now
   USE log_module,                ONLY : log_push, log_pop
   USE io_module,                 ONLY : prefix, postfix, work_dir, stdout, ionode
   USE io_module,                 ONLY : io_name, dftdata_fmt, aux_unit, aux1_unit 
   USE io_module,                 ONLY : io_open_dftdata, io_close_dftdata
   USE mp,                        ONLY : mp_sum
   USE control_module,            ONLY : use_uspp
   USE files_module,              ONLY : file_delete
   USE util_module,               ONLY : mat_mul
   USE converters_module,         ONLY : cry2cart, cart2cry
   USE atomic_module,             ONLY : atomic_name2num, atomic_num2name
   !
   USE lattice_module,            ONLY : avec, bvec, alat, tpiba, omega
   USE ions_module,               ONLY : symb, tau, nat
   USE kpoints_module,            ONLY : nkpts, nkpts_g, vkpt_g, iks, ike
   USE windows_module,            ONLY : imin, dimwin, dimwinx
   USE subspace_module,           ONLY : dimwann, eamp
   USE localization_module,       ONLY : cu, rave
   USE ggrids_module,             ONLY : nfft, nffts, igv, &
                                         ggrids_read_ext, ggrids_deallocate, &
                                         ggrids_gk_indexes, ggrids_summary
   USE wfc_info_module
   USE wfc_data_module,           ONLY : npwkx, npwk, igsort, evc, evc_info, &
                                         wfc_data_grids_read, wfc_data_grids_summary, &
                                         wfc_data_kread, wfc_data_deallocate
   USE uspp,                      ONLY : nkb, vkb, vkb_ik
   USE becmod,                    ONLY : becp
   USE parser_module
   USE struct_fact_data_module
   USE want_interfaces_module
   !
   IMPLICIT NONE

      !
      ! input variables
      !
      CHARACTER(*)     :: wann
      CHARACTER(*)     :: datatype          
      LOGICAL          :: uspp_augmentation 
      CHARACTER(*)     :: output_fmt        
      LOGICAL          :: collect_wf        
      REAL(dbl)        :: r1min, r1max      
      REAL(dbl)        :: r2min, r2max      
      REAL(dbl)        :: r3min, r3max      
      INTEGER          :: nr1, nr2, nr3     

      !
      ! local variables
      !
      CHARACTER(7)      :: subname = 'do_plot'
      !
      INTEGER :: nrxl, nryl, nrzl
      INTEGER :: nrxh, nryh, nrzh
      INTEGER :: nnrx, nnry, nnrz
      INTEGER :: nx, ny, nz, nzz, nyy, nxx
      !
      INTEGER :: ia, ib, ik_g, ik, ig, ir
      INTEGER :: natot, nplot
      INTEGER :: m, n, i, j, ierr
      INTEGER :: zatom
      !
      !
      REAL(dbl)    :: tmaxx, tmax, xk(3)
      REAL(dbl)    :: avecl(3,3), raux(3), r0(3), r1(3), rmin(3), rmax(3)
      REAL(dbl)    :: arg, cost, norm, norm_us
      COMPLEX(dbl) :: phase
      COMPLEX(dbl) :: caux, cmod
      !
      INTEGER,      ALLOCATABLE :: map(:), iwann(:)
      REAL(dbl),    ALLOCATABLE :: rwann_out(:,:,:)
      REAL(dbl),    ALLOCATABLE :: tautot(:,:), tau_cry(:,:)
      REAL(dbl),    ALLOCATABLE :: vkpt_cry(:,:)
      REAL(dbl),    ALLOCATABLE :: rave_cry(:,:), rave_shift(:,:)
      CHARACTER(3), ALLOCATABLE :: symbtot(:)
      COMPLEX(dbl), ALLOCATABLE :: cwann(:,:,:,:), cwann_aug(:,:,:,:)
      COMPLEX(dbl), ALLOCATABLE :: kwann(:,:), wfc_aux(:,:)
      COMPLEX(dbl), ALLOCATABLE :: cutot(:,:)

      CHARACTER( nstrx )  :: filename
      CHARACTER( 5 )      :: str, aux_fmt
      LOGICAL             :: do_modulus
      LOGICAL             :: okp( 3 )
      !
      ! end of declariations
      !

      CALL timing( subname, OPR='start')
      CALL log_push( subname )

      !
      ! Few definitions
      !
      IF ( TRIM(datatype) == "module" ) datatype = "modulus"
      IF ( TRIM(datatype) == "modulus") do_modulus = .TRUE.
      

      !
      ! ... Opening the file containing the PW-DFT data
      !
      CALL io_open_dftdata( LSERIAL=.FALSE. )
      CALL io_name('dft_data',filename,LPATH=.FALSE.)

      !
      ! ... Read grids
      CALL write_header( stdout, "Grids" )

      IF (ionode) WRITE( stdout,"(  2x,'Reading density G-grid from file: ',a)") TRIM(filename)
      CALL ggrids_read_ext( dftdata_fmt )
      !
      ! ... Read wfcs
      IF (ionode) WRITE( stdout,"(  2x,'Reading Wfc grids from file: ',a)") TRIM(filename)
      CALL wfc_data_grids_read( dftdata_fmt )
      !
      ! ... closing the main data file
      CALL io_close_dftdata( LSERIAL=.FALSE. )


      !
      ! set FFT mesh
      !
      rmin(1) = r1min 
      rmin(2) = r2min
      rmin(3) = r3min
      rmax(1) = r1max 
      rmax(2) = r2max
      rmax(3) = r3max
      !
      uspp_augmentation = uspp_augmentation .AND. use_uspp
      !
      IF ( uspp_augmentation ) THEN
         !
         ! use the full grid if required
         !
         IF( nr1 == -1 ) nr1 = nfft(1)
         IF( nr2 == -1 ) nr2 = nfft(2)
         IF( nr3 == -1 ) nr3 = nfft(3)
         !
      ELSE
         !
         ! use the smooth grid
         !
         IF( nr1 == -1 ) nr1 = nffts(1)      
         IF( nr2 == -1 ) nr2 = nffts(2)      
         IF( nr3 == -1 ) nr3 = nffts(3)      
         !
      ENDIF
      !
      nr1 = good_fft_order ( nr1 )
      nr2 = good_fft_order ( nr2 )
      nr3 = good_fft_order ( nr3 )
      !
      ! for nrx too small, the mapping procedure in  ggrids_gk_indexes
      ! may fail giving rise to out-of-bounds usage of arrays
      !
      IF ( uspp_augmentation ) THEN
         !
         IF ( nr1 < nfft(1) ) CALL errore(subname,'non-safe nr1 adopted', ABS(nr1) +1 )
         IF ( nr2 < nfft(2) ) CALL errore(subname,'non-safe nr2 adopted', ABS(nr2) +1 )
         IF ( nr3 < nfft(3) ) CALL errore(subname,'non-safe nr3 adopted', ABS(nr3) +1 )
         !
      ELSE
         !
         IF ( nr1 < nffts(1) ) CALL errore(subname,'non-safe nr1 adopted', ABS(nr1) +1 )
         IF ( nr2 < nffts(2) ) CALL errore(subname,'non-safe nr2 adopted', ABS(nr2) +1 )
         IF ( nr3 < nffts(3) ) CALL errore(subname,'non-safe nr3 adopted', ABS(nr3) +1 )
         !
      ENDIF


      !
      ! ... stdout summary about grids
      !
      IF ( ionode ) WRITE(stdout, "()")
      !
      CALL ggrids_summary( stdout )
      CALL wfc_data_grids_summary( stdout )
      !
      IF ( ionode ) WRITE(stdout, "()")


!
! ... final settings on input
!
      !
      ! get the exact number of plot
      CALL parser_replica( wann, nplot, IERR=ierr )
      IF ( ierr/=0 ) CALL errore(subname,'wrong FMT in wann string I',ABS(ierr))
      !
      ALLOCATE( iwann(nplot), STAT=ierr )
      IF ( ierr/=0 ) CALL errore(subname,'allocating iwann',ABS(ierr))
      !
      ! get the WF indexes
      CALL parser_replica( wann, nplot, iwann, ierr )
      IF ( ierr/=0 ) CALL errore(subname,'wrong FMT in wann string II',ABS(ierr))
      !
      DO m = 1, nplot
         IF ( iwann(m) <= 0  .OR. iwann(m) > dimwann ) &
              CALL errore(subname,'iwann too large',m)
      ENDDO
      !
      !
      ! if lattice is not orthorombic, only .cube or .xsf output fmts are allowed
      !
      IF ( ABS( DOT_PRODUCT( avec(:,1), avec(:,2) )) > EPS_m6 .OR. &
           ABS( DOT_PRODUCT( avec(:,1), avec(:,3) )) > EPS_m6 .OR. &
           ABS( DOT_PRODUCT( avec(:,2), avec(:,3) )) > EPS_m6   ) THEN 
           !
           IF ( TRIM(output_fmt) == "plt" .OR. TRIM(output_fmt) == "txt" ) &
                CALL errore(subname,'lattice not orthorombic: use xsf or cube output_fmt',4)
      ENDIF

      !
      ! set the FFT mesh
      ! the upper values get one point less to avoid
      ! replica of the same points 
      !
      nrxl = NINT( r1min * nr1 )
      nrxh = NINT( r1max * nr1 ) -1
      nryl = NINT( r2min * nr2 )
      nryh = NINT( r2max * nr2 ) -1
      nrzl = NINT( r3min * nr3 )
      nrzh = NINT( r3max * nr3 ) -1

      !
      ! summary of the input
      !
      CALL write_header( stdout, "Wannier function plotting" )
      !
      IF ( ionode ) THEN
          !
          WRITE( stdout,"(2x,'nplot = ',i4, ' Wannier func.s to be plotted')") nplot
          DO m=1,nplot
              WRITE( stdout,"(5x,'wf (',i4,' ) = ',i4 )") m, iwann(m)
          ENDDO
          WRITE( stdout,"(/,2x,'Data type  :',3x,a)") TRIM(datatype)
          WRITE( stdout,"(  2x,'Output fmt :',3x,a)") TRIM(output_fmt)
          WRITE( stdout,"(/,2x,'Plotting Cell extrema [cryst. coord]:')") 
          WRITE( stdout,"(  6x, ' r1 :  ', f8.4, ' --> ', f8.4 )" ) r1min, r1max
          WRITE( stdout,"(  6x, ' r2 :  ', f8.4, ' --> ', f8.4 )" ) r2min, r2max
          WRITE( stdout,"(  6x, ' r3 :  ', f8.4, ' --> ', f8.4 )" ) r3min, r3max
          WRITE( stdout,"(/,2x,'Grid dimensions:')") 
          WRITE( stdout,"(  6x, 'nrx :  ', i8, ' --> ', i8 )" ) nrxl, nrxh 
          WRITE( stdout,"(  6x, 'nry :  ', i8, ' --> ', i8 )" ) nryl, nryh 
          WRITE( stdout,"(  6x, 'nrz :  ', i8, ' --> ', i8 )" ) nrzl, nrzh 
          WRITE( stdout,"()")
          !
      ENDIF
      !
      IF ( uspp_augmentation .AND. .NOT. do_modulus .AND. ionode ) THEN
         !
         ! in this case we cannot perform the full PAW reconstruction:
         ! a warning is given
         !
         WRITE(stdout, "(6x, 'data type: ', a )" ) TRIM(datatype)
         CALL warning(subname,'USPP do not allow for a full PAW reconstruction.')
         !
      ENDIF

      CALL flush_unit( stdout )


!
! Initialize the data used for the fast fourier transforms
!
      IF( nrxh - nrxl < 0 ) CALL errore( subname, 'wrong nrxl and nrxh ', 1 )
      IF( nryh - nryl < 0 ) CALL errore( subname, 'wrong nryl and nryh ', 1 )
      IF( nrzh - nrzl < 0 ) CALL errore( subname, 'wrong nrzl and nrzh ', 1 )

      
      !
      ! allocate WF and local data
      !
      ALLOCATE ( cwann( nrxl:nrxh, nryl:nryh, nrzl:nrzh, nplot ), STAT=ierr ) 
      IF( ierr /=0 ) CALL errore(subname, 'allocating cwann ', ABS(ierr) )
      !
      ALLOCATE( kwann( nr1*nr2*nr3, nplot ), STAT=ierr )
      IF( ierr /=0 ) CALL errore(subname, 'allocating kwann ', ABS(ierr) )
      !
      ALLOCATE( vkpt_cry(3, nkpts_g), STAT=ierr )
      IF( ierr /=0 ) CALL errore(subname, 'allocating vkpt_cry ', ABS(ierr) )
      !
      ALLOCATE( rave_cry(3, dimwann), STAT=ierr )
      IF( ierr /=0 ) CALL errore(subname, 'allocating rave_cry ', ABS(ierr) )
      !
      ALLOCATE( rave_shift(3, dimwann), STAT=ierr )
      IF( ierr /=0 ) CALL errore(subname, 'allocating rave_shift ', ABS(ierr) )
      !
      ALLOCATE( cutot(dimwinx, dimwann), STAT=ierr )
      IF( ierr /=0 ) CALL errore(subname, 'allocating cutot ', ABS(ierr) )
      !
      ALLOCATE( map(npwkx), STAT=ierr )
      IF( ierr /=0 ) CALL errore(subname, 'allocating map ', ABS(ierr) )


      !
      ! convert vkpt from cart coord (bohr^-1) to cryst 
      ! the same for rave, from cart coord (bohr) to cryst
      !
      vkpt_cry(:,:) = vkpt_g(:,:)
      rave_cry(:,:) = rave(:,:)
      CALL cart2cry( vkpt_cry, bvec )
      CALL cart2cry( rave_cry, avec)

      !
      ! determine the shift to rave to set the WF in the selected plotting cell
      ! select the starting point of the shift cell as the mid point of 
      ! rmin(i) and rmax(i)-1.0
      !
      rave_shift(:,:) = ZERO
      IF ( collect_wf ) THEN
         !
         DO m=1,nplot
            !
            DO i=1,3
               j = 0
               IF ( rave_cry(i,iwann(m)) -( rmin(i)+rmax(i)-ONE )/TWO < ZERO ) j = 1
               rave_shift(i,iwann(m)) = REAL( INT( rave_cry(i,iwann(m)) &
                                              -( rmin(i)+rmax(i)-ONE)/TWO ) -j ) 
            ENDDO
            !
         ENDDO
         !
      ENDIF
      !
      !
      IF ( ionode ) THEN
          !
          WRITE( stdout, " (/,2x, 'Centers for the required Wannier functions:')")
          WRITE( stdout, " (/,8x,'in cartesian coord (Bohr)' )")
          !
          DO m=1,nplot
             WRITE( stdout, " (4x,'Wf(',i4,' ) = (', 3f13.6, ' )' )" ) &
                    iwann(m), rave(:,iwann(m))
          ENDDO
          !
          WRITE( stdout, " (8x,'in crystal coord' )")
          !
          DO m=1,nplot
             WRITE( stdout, " (4x,'Wf(',i4,' ) = (', 3f13.6, ' )' )" ) &
                    iwann(m), rave_cry(:,iwann(m))
          ENDDO
          !
          IF ( collect_wf ) THEN
              !
              WRITE( stdout,"(/,2x,'Collecting WFs: ')")
              WRITE( stdout,"(2x,'Plotting Cell dimensions [cryst. coord]:')") 
              !
              DO i=1,3
                  WRITE( stdout,"(  6x, ' r',i1,' :  ', f8.4, ' --> ', f8.4 )" ) &
                            i, (rmin(i)+rmax(i)-ONE)/TWO, (rmin(i)+rmax(i)+ONE)/TWO
              ENDDO
              !
              WRITE( stdout,"(/,2x,'New center positions [cryst. coord]:')") 
              !
              DO m=1,nplot
                  WRITE( stdout, " (4x,'Wf(',i4,' ) = (', 3f13.6, ' )' )" ) &
                         iwann(m), rave_cry(:,iwann(m))-rave_shift(:,iwann(m))
              ENDDO
              !
          ENDIF
          !
          WRITE( stdout, "(/)" )
          !
      ENDIF


!
! Get the atoms in the plotting cell
!
      ALLOCATE( tau_cry(3, nat), STAT=ierr  )  
      IF( ierr /=0 ) CALL errore(subname, 'allocating tau_cry', ABS(ierr) ) 
      !
      ALLOCATE( tautot( 3, 125*nat ), STAT=ierr )
      IF( ierr /=0 ) CALL errore(subname, 'allocating tautot ', ABS(ierr) ) 
      !
      ALLOCATE( symbtot( 125*nat ), STAT=ierr )
      IF( ierr /=0 ) CALL errore(subname, 'allocating symbtot ', ABS(ierr) ) 

      tau_cry(:,:) = tau(:,:) * alat
      CALL cart2cry( tau_cry, avec )  

      natot = 0
      DO ia = 1, nat
           DO nx = -2, 2
           DO ny = -2, 2
           DO nz = -2, 2
               !
               raux(1) = ( tau_cry(1,ia) + REAL(nx, dbl) ) * REAL( nr1, dbl )
               raux(2) = ( tau_cry(2,ia) + REAL(ny, dbl) ) * REAL( nr2, dbl )
               raux(3) = ( tau_cry(3,ia) + REAL(nz, dbl) ) * REAL( nr3, dbl )
               !
               okp(1) = ( raux(1) >= (nrxl - 1) ) .AND. ( raux(1) < nrxh )
               okp(2) = ( raux(2) >= (nryl - 1) ) .AND. ( raux(2) < nryh ) 
               okp(3) = ( raux(3) >= (nrzl - 1) ) .AND. ( raux(3) < nrzh ) 
               !
               IF( okp(1) .AND. okp(2) .AND. okp(3) ) THEN
                    !
                    natot = natot+1
                    !
                    tautot(1,natot) = raux(1) / REAL( nr1, dbl)
                    tautot(2,natot) = raux(2) / REAL( nr2, dbl)
                    tautot(3,natot) = raux(3) / REAL( nr3, dbl)
                    !
                    symbtot(natot)  = symb( ia )
               ENDIF
               !
           ENDDO
           ENDDO
           ENDDO
      ENDDO

      !
      ! convert atoms to cartesian coords (bohr)
      !
      CALL cry2cart( tautot(:,1:natot), avec )
      ! 
      IF ( ionode ) THEN
          !
          WRITE(stdout, " (2x,'Atoms in the selected cell: (cart. coord. in Bohr)' ) " )
          !
          DO ia = 1, natot
               WRITE( stdout, "(5x, a, 2x,'tau( ',I3,' ) = (', 3F12.7, ' )' )" ) &
                      symbtot(ia), ia, (tautot( i, ia ), i = 1, 3)
          ENDDO
          !
          WRITE(stdout, "(/)" )
          !
      ENDIF


!
! wfc allocations
!
      CALL wfc_info_allocate(npwkx, dimwinx, nkpts, dimwinx, evc_info)
      !
      ALLOCATE( evc(npwkx, dimwinx ), STAT=ierr )
      IF (ierr/=0) CALL errore(subname,'allocating EVC',ABS(ierr))

      !
      ! re-opening the file containing the PW-DFT data
      !
      CALL io_open_dftdata( LSERIAL=.FALSE. )
      
     
!
! ... Pseudo initializations
!
      !
      ! ... if pseudo are used do the required allocations
      !
      IF ( uspp_augmentation ) THEN
          !
          IF (ionode) WRITE( stdout,"(2x,'Initializing global dft data')")
          !
          ! ... data required by USPP and atomic WFC
          CALL allocate_nlpot()
          !
          ! ... structure factors
          CALL struct_fact_data_init()

          !
          ! ... quantities strictly related to USPP
          IF (ionode) WRITE( stdout,"(2x,'Initializing US pseudopot. data')")

          !
          ! first initialization
          ! here we compute (among other quantities) \int dr Q_ij(r)
          !                                              \int dr e^ibr Q_ij(r)
          CALL init_us_1()
          IF (ionode) WRITE( stdout, '(2x, "Total number Nkb of beta functions: ",i5 ) ') nkb

          !
          ! space for beta functions in reciproc space within struct_facts
          !
          IF ( nkb <= 0 ) CALL errore(subname,'no beta functions within USPP',-nkb+1)
          !
          ALLOCATE( vkb(npwkx, nkb), STAT=ierr )
          IF (ierr/=0) CALL errore(subname,'allocating vkb',ABS(ierr))
          !
          ALLOCATE( becp(nkb, nplot, nkpts_g), STAT=ierr )
          IF (ierr/=0) CALL errore(subname,'allocating becp',ABS(ierr))
          !
          becp(:,:,:) = CZERO
          !
          IF (ionode) WRITE( stdout, "(/)") 
          !
      ENDIF

!#############

!
! ... Main loop on wfcs
!

      !
      ! init real space WFs
      !
      cwann( :, :, :, :) = CZERO

      kpoint_loop: &
      DO ik_g = iks, ike
          !
          ik = ik_g -iks +1
          ! 
          IF (ionode) WRITE(stdout, "(4x,'Wfc Fourier Transf. for k-point ',i4 )") ik

          !
          ! getting wfc 
          !
          CALL wfc_data_kread( dftdata_fmt, ik_g, "IK", evc, evc_info)

          !
          ! built the right transformation to rotate the original wfcs.
          !
          CALL mat_mul( cutot, eamp(:,:,ik_g), 'N', cu(:,:,ik_g), 'N' ,  &
                        dimwin(ik_g), dimwann, dimwann)

          !
          ! set the FFT map
          !
          map(:) = 0
          CALL ggrids_gk_indexes( igv, igsort(:,ik_g), npwk(ik_g), & 
                                  nr1, nr2, nr3, GK2FFT=map ) 
         
          !
          ! set the kwann function
          !
          DO m = 1, nplot

              CALL timing('kwann_calc',OPR='start')

              !
              ! apply a global shift to set the WF in the required cell
              !
              arg = TPI * DOT_PRODUCT( vkpt_cry(:,ik_g), rave_shift(:,iwann(m)) )
              phase = CMPLX( COS(arg), SIN(arg), dbl )


              kwann( :, m ) = CZERO
              !
              DO ig = 1, npwk(ik_g)
                 !
                 DO n = 1, dimwin(ik_g)
     
                     ib =  wfc_info_getindex(imin(ik_g)+n-1, ik_g, "IK", evc_info )
                     !
                     kwann( map(ig), m ) = kwann( map(ig), m ) + &
                                           cutot(n, iwann(m) ) * evc( ig, ib )
                 ENDDO
                 !
                 ! apply the translation phase
                 kwann( map(ig) ,m) = kwann( map(ig) ,m) * phase 
                 !
              ENDDO
              !
              CALL timing('kwann_calc',OPR='stop')
              !
          ENDDO

          !
          ! compute auxiliary quantities to evaluate the 
          ! auxmentation charge
          !
          IF ( uspp_augmentation ) THEN
              ! 
              xk(:) = vkpt_g(:,ik_g) / tpiba
              CALL init_us_2( npwk(ik_g), igsort(1,ik_g), xk, 1, nkb, vkb )
              !
              vkb_ik = ik_g

              !
              ! we ned to reset the order of G indexes in KWANN from the
              ! FFT grid to the k-dipendent grid of wfcs
              !
              ALLOCATE( wfc_aux( npwkx, nplot ), STAT=ierr )
              IF( ierr /=0 ) CALL errore(subname, 'allocating wfc_aux', ABS(ierr) )
              !
              DO m  = 1, nplot
              DO ig = 1, npwk(ik_g)
                 wfc_aux( ig, m ) = kwann( map(ig), m )
              ENDDO
              ENDDO
              !
              wfc_aux( npwk(ik_g)+1: npwkx, : ) = CZERO
              !
              ! 
              CALL ccalbec( nkb, nkb, npwkx, npwk(ik_g), nplot, becp( 1:nkb, 1:nplot, ik_g), &
                            vkb, wfc_aux )
              !
              DEALLOCATE( wfc_aux, STAT=ierr )
              IF( ierr /=0 ) CALL errore(subname, 'deallocating wfc_aux', ABS(ierr) )
              !
          ENDIF

          !
          ! logically clean wfcs (but memory is NOT free)
          !
          CALL wfc_info_delete(evc_info, LABEL="IK")


          !
          ! prepar for real-space loop
          !
          nnrx = ABS( nrxl / nr1 ) + 2
          nnry = ABS( nryl / nr2 ) + 2
          nnrz = ABS( nrzl / nr3 ) + 2
          !
          !
          raux(1) = vkpt_cry(1,ik_g) / REAL( nr1, dbl)
          raux(2) = vkpt_cry(2,ik_g) / REAL( nr2, dbl)
          raux(3) = vkpt_cry(3,ik_g) / REAL( nr3, dbl)

          ! 
          ! FFT call and real-space loop
          ! 
          DO m=1,nplot

             CALL timing('cfft3d',OPR='start')
             CALL cfft3d( kwann(:,m), nr1, nr2, nr3,  &
                                      nr1, nr2, nr3,  1 )
             CALL timing('cfft3d',OPR='stop')

             !
             ! loop over FFT grid
             !
             CALL timing('cwann_calc',OPR='start')
             !
             DO nzz = nrzl, nrzh
                 nz = MOD( nzz + nnrz * nr3 , nr3 ) + 1
                 !
                 DO nyy = nryl, nryh
                     ny = MOD( nyy + nnry * nr2 , nr2 ) + 1
                     !
                     DO nxx = nrxl, nrxh
                         nx = MOD( nxx + nnrx * nr1 , nr1 ) + 1
                         !
                         ir = nx + (ny-1) * nr1 + (nz-1) * nr1 * nr2
                         !
                         arg   = raux(1) * REAL(nxx, dbl) + &
                                 raux(2) * REAL(nyy, dbl) + &
                                 raux(3) * REAL(nzz, dbl)
                                 !
                         caux  = CMPLX( COS( TPI*arg ), SIN( TPI*arg), dbl ) * &
                                 kwann( ir, m )
                                 !
                         cwann( nxx, nyy, nzz, m) = cwann( nxx, nyy, nzz, m) + caux
                         !
                     ENDDO
                 ENDDO
             ENDDO
             !
             CALL timing('cwann_calc',OPR='stop')
          ENDDO 
          !
          IF (ionode) CALL timing_upto_now(stdout)
          !
      ENDDO kpoint_loop

      !
      ! recovering kpt parallelism
      !
      DO m = 1, nplot
          !
          CALL mp_sum( cwann(:,:,:,m) )
          !
      ENDDO
      !
      IF ( uspp_augmentation ) THEN
          !
          CALL mp_sum( becp ) 
          !
      ENDIF
      

      !
      ! clean the large amount of memory used by wfcs and grids
      !
      DEALLOCATE( kwann, STAT=ierr)
      IF (ierr/=0) CALL errore(subname,'deallocating kwann',ABS(ierr))
      !
      DEALLOCATE( map, STAT=ierr )
      IF( ierr /=0 ) CALL errore(subname, 'deallocating map', ABS(ierr) )
      !
      DEALLOCATE( vkpt_cry, STAT=ierr )
      IF( ierr /=0 ) CALL errore(subname, 'deallocating vkpt_cry', ABS(ierr) )
      !
      CALL wfc_data_deallocate()
      !
      !
      CALL io_close_dftdata( LSERIAL=.FALSE. )
      IF (ionode) WRITE(stdout, "()")


!
! Compute USPP augmentation if the case
!
      IF ( uspp_augmentation ) THEN

          ALLOCATE ( cwann_aug( nrxl:nrxh, nryl:nryh, nrzl:nrzh, nplot ), STAT=ierr ) 
          IF( ierr /=0 ) CALL errore(subname, 'allocating cwann_aug', ABS(ierr) )

          !
          ! compute the augmentation in reciprocal space
          !
          cwann_aug( :,:,:,:) = CZERO
          !
          CALL wf2_augment( nrxl, nrxh, nryl,nryh, nrzl,nrzh, &
                            nr1, nr2, nr3, nplot, cwann_aug ) 

          DEALLOCATE( becp, STAT=ierr)
          IF( ierr /=0 ) CALL errore(subname, 'deallocating becp', ABS(ierr) )
          !
      ENDIF
      !
      CALL ggrids_deallocate()


! 
! Fix the global phase of WFs
!
      !
      ! WF are made be almost real by setting the phase
      ! at the point where they have max modulus
      ! WF square moduli are written in units of bohr^-3 
      !
      cost =  ONE / ( REAL(nkpts_g, dbl) * SQRT(omega) )
      !
      IF ( ionode ) THEN
          !
          WRITE(stdout, " (2x,'WF normalization:')")
          !
          IF ( uspp_augmentation ) THEN
               WRITE(stdout, " (2x,'  Index', 14x, 'Max value', 5x, 'Normaliz.', 5x, &
                             & 'Wfc comp.',3x,'Aug comp.')")
               WRITE(stdout, " (2x,71('-'))")
          ELSE
               WRITE(stdout, " (2x,'  Index', 14x, 'Max value', 5x, 'Normaliz.')" )
               WRITE(stdout, " (2x,45('-'))")
          ENDIF
          !
      ENDIF
      !
      !
      DO m = 1, nplot
          !
          norm     = ZERO
          norm_us  = ZERO
          tmaxx    = ZERO
          cmod     = CONE
          !
          DO nzz = nrzl, nrzh
          DO nyy = nryl, nryh
          DO nxx=  nrxl, nrxh
              !
              cwann( nxx, nyy, nzz, m ) = cwann( nxx, nyy, nzz, m ) * cost
              !
              tmax = REAL(cwann( nxx, nyy, nzz, m) * &
                          CONJG( cwann( nxx, nyy, nzz, m) ), dbl )
              !
              IF ( tmax > tmaxx ) THEN
                   tmaxx = tmax
                   cmod = cwann( nxx, nyy, nzz, m)
              ENDIF
              !
              norm = norm + tmax
              !
          ENDDO
          ENDDO
          ENDDO
          !
          norm = norm * omega / REAL( nr1*nr2*nr3, dbl )
          !
          norm_us = ZERO
          IF ( uspp_augmentation ) THEN
              !
              norm_us = REAL( SUM( cwann_aug(:,:,:, m))  )
              norm_us = norm_us * omega / REAL( nr1*nr2*nr3, dbl )
              !
          ENDIF
          !
          ! report
          IF ( ionode ) THEN
              !
              IF ( uspp_augmentation ) THEN
                  WRITE(stdout, " (4x,'Wf(',i4,' )   --> ',f12.6, 2x, f12.6, 2x, 2f12.6) " ) &
                                m, ABS(cmod), norm+norm_us, norm, norm_us
              ELSE
                  WRITE(stdout, " (4x,'Wf(',i4,' )   --> ',f12.6, 2x, f12.6 ) " ) &
                                m, ABS(cmod), norm
              ENDIF
              !
          ENDIF
          !
          !
          ! set the phase and invert it
          cmod = CONJG( cmod ) / SQRT( cmod * CONJG(cmod) ) 
          !
          cwann(:,:,:,m) = cwann(:,:,:,m) * cmod
          !
      ENDDO
      !
      IF ( ionode ) WRITE(stdout, "(/)")

      
!
! We prepare a graphic output for gopenmol, dans or xcrysden; 
!

      !
      ! Offset for position and WF's allignment
      ! compute r0, r1 in crystal units
      !
      r0(1) = REAL( nrxl, dbl ) / REAL( nr1, dbl )
      r0(2) = REAL( nryl, dbl ) / REAL( nr2, dbl )
      r0(3) = REAL( nrzl, dbl ) / REAL( nr3, dbl )
      !
      r1(1) = REAL( nrxh+1, dbl ) / REAL( nr1, dbl )
      r1(2) = REAL( nryh+1, dbl ) / REAL( nr2, dbl )
      r1(3) = REAL( nrzh+1, dbl ) / REAL( nr3, dbl )
      !
      ! move r0, r1 to bohr
      !
      CALL cry2cart( r0, avec )
      CALL cry2cart( r1, avec )
      


      !
      ! output and internal formats
      ! when .plt is required, a .cube file is written and then converted to .plt
      !
      SELECT CASE ( TRIM(output_fmt) )
      CASE ( "txt")
           aux_fmt = ".txt"
      CASE ( "xsf" )
           aux_fmt = ".xsf"
      CASE ( "plt", "cube" )
           aux_fmt = ".cube"
      CASE DEFAULT
           CALL errore(subname,'invalid OUTPUT_FMT '//TRIM(output_fmt),4)
      END SELECT

      !
      ! workspace for output writing
      !
      ALLOCATE( rwann_out( nrxl:nrxh, nryl:nryh, nrzl:nrzh ), STAT=ierr ) 
      IF (ierr/=0) CALL errore(subname,'allocating rwann_out',ABS(ierr))


      !
      ! final loop on different wfs
      !
      DO m = 1, nplot

          !
          ! output filename
          !
          SELECT CASE ( TRIM(datatype) )
          CASE( "modulus" )    
              str = "_WFM"
              !
              IF ( uspp_augmentation ) THEN
                  rwann_out(:,:,:) = REAL( cwann(:,:,:,m) * CONJG( cwann(:,:,:,m) ) ) +  &
                                     REAL( cwann_aug(:,:,:,m), dbl ) 
              ELSE
                  rwann_out(:,:,:) = REAL( cwann(:,:,:,m) * CONJG( cwann(:,:,:,m) ) )
              ENDIF
              !
          CASE( "real" )    
              str = "_WFR"
              rwann_out(:,:,:) = REAL( cwann(:,:,:,m) )
          CASE( "imaginary" )    
              str = "_WFI"
              rwann_out(:,:,:) = AIMAG( cwann(:,:,:,m) )
          CASE DEFAULT
              CALL errore(subname,'invalid DATATYPE '//TRIM(datatype),3)
          END SELECT 


          filename=TRIM(work_dir)//"/"//TRIM(prefix)//TRIM(postfix)
          !
          IF ( iwann(m) <= 9 ) THEN
               !
               filename=TRIM(filename)//TRIM(str)//"00"//TRIM(int2char(iwann(m)))
          ELSE IF ( iwann(m) <= 99 ) THEN
               !
               filename=TRIM(filename)//TRIM(str)//"0"//TRIM(int2char(iwann(m)))
          ELSE IF ( iwann(m) <= 999 ) THEN
               !
               filename=TRIM(filename)//TRIM(str)//TRIM(int2char(iwann(m)))
          ELSE
              CALL errore(subname,'iwann(m) > 999', iwann(m))
          ENDIF
          !
          !
          IF ( ionode ) THEN 
               !
               WRITE( stdout,"(2x,'writing WF(',i4,') plot on file: ',a)") &
                      iwann(m), TRIM(filename)//TRIM(aux_fmt)

               OPEN ( aux_unit, FILE=TRIM(filename)//TRIM(aux_fmt), FORM='formatted', &
                                STATUS='unknown', IOSTAT=ierr )
               IF (ierr/=0) CALL errore(subname,'opening file '//TRIM(filename)//TRIM(aux_fmt),1)
               !
          ENDIF


          SELECT CASE ( TRIM(output_fmt) )
          CASE( "cube", "plt" )

              ! 
              ! bohr
              avecl(:,1) = avec(:,1) / REAL( nr1, dbl) 
              avecl(:,2) = avec(:,2) / REAL( nr2, dbl) 
              avecl(:,3) = avec(:,3) / REAL( nr3, dbl) 

              IF ( ionode ) THEN
                  !
                  WRITE(aux_unit, '( " WanT" )') 
                  WRITE(aux_unit, '( " plot output - cube format" )' ) 
                  WRITE(aux_unit, '(i4,3f12.6)' ) natot, r0(:) 
                  WRITE(aux_unit, '(i4,3f12.6)' ) (nrxh-nrxl+1),  avecl(:,1) 
                  WRITE(aux_unit, '(i4,3f12.6)' ) (nryh-nryl+1),  avecl(:,2) 
                  WRITE(aux_unit, '(i4,3f12.6)' ) (nrzh-nrzl+1),  avecl(:,3) 
    
                  DO ia = 1, natot
                      CALL atomic_name2num( symbtot(ia), zatom )
                      WRITE(aux_unit, '(i4,4e13.5)' ) zatom, ONE, tautot( :, ia )
                  ENDDO
    
                  DO nx = nrxl, nrxh
                  DO ny = nryl, nryh
                      WRITE( aux_unit, "(6e13.5)" ) rwann_out( nx, ny, : )
                  ENDDO
                  ENDDO
                  !
              ENDIF

          CASE( "txt" )

              IF ( ionode ) THEN
                  !
                  WRITE(aux_unit, '( " 3 2" )') 
                  WRITE(aux_unit, '( 3i5 )' ) nrzh-nrzl+1, nryh-nryl+1, nrxh-nrxl+1
                  WRITE(aux_unit, '(6f10.4)' ) r0(3) * bohr, r1(3) * bohr,  &
                                               r0(2) * bohr, r1(2) * bohr,  & 
                                               r0(1) * bohr, r1(1) * bohr

                  DO nz = nrzl, nrzh
                  DO ny = nryl, nryh
                  DO nx = nrxl, nrxh
                      !
                      WRITE( aux_unit, "(f20.10)" ) rwann_out( nx, ny, nz )
                      !
                  ENDDO
                  ENDDO
                  ENDDO
                  !
              ENDIF

          CASE( "xsf" )

              ! 
              ! bohr
              avecl(:,1) = avec(:,1) * REAL(nrxh-nrxl+1, dbl) / REAL( nr1, dbl) 
              avecl(:,2) = avec(:,2) * REAL(nryh-nryl+1, dbl) / REAL( nr2, dbl) 
              avecl(:,3) = avec(:,3) * REAL(nrzh-nrzl+1, dbl) / REAL( nr3, dbl) 
              
              IF ( ionode ) THEN
                  !
                  ! tau is temporarily converted to bohr 
                  ! avec and tau passed in bohr, but converted to Ang in the routine
                  !
                  tau = tau * alat
                  CALL xsf_struct ( avec, nat, tau, symb, aux_unit )
                  tau = tau / alat
                  !
                  CALL xsf_datagrid_3d ( rwann_out(nrxl:nrxh, nryl:nryh, nrzl:nrzh),  &
                                         nrxh-nrxl+1, nryh-nryl+1, nrzh-nrzl+1,    &
                                         r0, avecl(:,1), avecl(:,2), avecl(:,3), aux_unit )
                  !
              ENDIF

          CASE DEFAULT
             
              CALL errore(subname,'invalid OUTPUT_FMT '//TRIM(output_fmt),5)

          END SELECT
          !
          IF ( ionode ) CLOSE( aux_unit )

        
          !
          ! ... write a XYZ file for the atomic positions, when the case
          !
          IF ( ionode .AND. ( TRIM( output_fmt ) == "txt" .OR. TRIM( output_fmt ) == "plt" ) ) THEN
              !
              WRITE( stdout,"(7x,'atomic positions on file: ',a)") TRIM(filename)//".xyz"
              OPEN ( aux1_unit, FILE=TRIM(filename)//".xyz", FORM='formatted', &
                                STATUS='unknown', IOSTAT=ierr )
              IF (ierr/=0) CALL errore(subname,'opening file '//TRIM(filename)//".xyz",1)
              !
              WRITE(aux1_unit,"(i6,/)") natot
              DO ia = 1, natot
                  WRITE(aux1_unit, '(a2,3x,3f15.9)' ) symbtot(ia) , tautot( :, ia )*bohr
              ENDDO
              !
              CLOSE( aux1_unit )
              !
          ENDIF


          !
          ! ... convert output to PLT fmt, if the case
          !
          IF ( TRIM( output_fmt ) == "plt" ) THEN
               !
               CALL timing('gcubeplt',OPR='start')
               IF ( ionode ) CALL gcube2plt( filename, LEN_TRIM(filename) )
               CALL timing('gcubeplt',OPR='stop')
               !
               ! removing temporary .cube file
               !
               IF ( ionode ) THEN
                   !
                   WRITE(stdout,"(2x,'deleting tmp files: ',a)" ) TRIM(filename)//".cube"
                   WRITE(stdout,"(22x,a,2/)" )                    TRIM(filename)//".crd"
                   CALL file_delete( TRIM(filename)//".cube" )
                   CALL file_delete( TRIM(filename)//".crd" )
                   !
               ENDIF
               !
          ENDIF

      ENDDO
      !
      IF ( ionode ) WRITE( stdout, "()" )


!
! ... Clean up memory
!
      DEALLOCATE( iwann, STAT=ierr )
      IF( ierr /=0 ) CALL errore(subname, 'deallocating iwann', ABS(ierr) )
      !
      DEALLOCATE( tautot, STAT=ierr )
      IF( ierr /=0 ) CALL errore(subname, 'deallocating tautot', ABS(ierr) )
      !
      DEALLOCATE( symbtot, STAT=ierr )
      IF( ierr /=0 ) CALL errore(subname, 'deallocating symbtot', ABS(ierr) )
      !
      DEALLOCATE( rave_cry, STAT=ierr )
      IF( ierr /=0 ) CALL errore(subname, 'deallocating rave_cry', ABS(ierr) )
      !
      DEALLOCATE( rave_shift, STAT=ierr )
      IF( ierr /=0 ) CALL errore(subname, 'deallocating rave_shift', ABS(ierr) )
      !
      DEALLOCATE( tau_cry, STAT=ierr )
      IF( ierr /=0 ) CALL errore(subname, 'deallocating tau_cry', ABS(ierr) )
      !
      DEALLOCATE( cutot, STAT=ierr )
      IF( ierr /=0 ) CALL errore(subname, 'deallocating cutot ', ABS(ierr) )
      !
      DEALLOCATE( rwann_out, STAT=ierr )
      IF( ierr /=0 ) CALL errore(subname, 'deallocating rwann_out', ABS(ierr) )
      !
      DEALLOCATE( cwann, STAT=ierr)
      IF( ierr /=0 ) CALL errore(subname, 'deallocating cwann', ABS(ierr) )
      !
      IF ( uspp_augmentation ) THEN
          !
          DEALLOCATE( cwann_aug, STAT=ierr)
          IF( ierr /=0 ) CALL errore(subname, 'deallocating cwann_aug', ABS(ierr) )
          !
      ENDIF
      !
      CALL timing( subname, OPR="STOP")
      CALL log_pop( subname )
      ! 
      RETURN
      !
END SUBROUTINE do_plot

