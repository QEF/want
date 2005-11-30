!
! Copyright (C) 2005 WanT Group
!
! This file is distributed under the terms of the
! GNU General Public License. See the file `License'
! in the root directory of the present distribution,
! or http://www.gnu.org/copyleft/gpl.txt .
!
!=====================================================
   PROGRAM plot
   !=====================================================
   !
   ! real space plot of the computed Wannier functions
   !
   USE kinds
   USE constants,          ONLY : ZERO, CZERO, ONE, TWO, CONE, CI, TPI, &
                                  bohr => bohr_radius_angs, EPS_m6, EPS_m4
   USE parameters,         ONLY : ntypx, natx, nstrx
   USE fft_scalar,         ONLY : cfft3d
   USE timing_module,      ONLY : timing, timing_upto_now, timing_overview, global_list
   USE io_module,          ONLY : prefix, postfix, work_dir, stdin, stdout
   USE io_module,          ONLY : ioname, space_unit, wan_unit, dft_unit, &
                                  aux_unit, aux1_unit 
   USE control_module,     ONLY : read_pseudo, use_uspp
   USE files_module,       ONLY : file_open, file_close, file_delete
   USE parser_module
   USE want_init_module,   ONLY : want_init
   USE version_module,     ONLY : version_number
   USE util_module,        ONLY : mat_mul
   USE converters_module,  ONLY : cry2cart, cart2cry
   USE atomic_module,      ONLY : atomic_name2num, atomic_num2name
   USE summary_module,     ONLY : summary
   USE parser_module
   !
   USE lattice_module,     ONLY : avec, bvec, alat
   USE ions_module,        ONLY : symb, tau, nat
   USE kpoints_module,     ONLY : nkpts, vkpt
   USE windows_module,     ONLY : imin, dimwin, dimwinx, windows_read, &
                                  spin_component
   USE subspace_module,    ONLY : dimwann, eamp, subspace_read
   USE localization_module,ONLY : cu, rave, localization_read
   USE ggrids_module,      ONLY : nfft, npw_rho, ecutwfc, ecutrho, igv, &
                                  ggrids_read_ext, ggrids_deallocate, &
                                  ggrids_gk_indexes
   USE wfc_info_module
   USE wfc_data_module,    ONLY : npwkx, npwk, igsort, evc, evc_info, &
                                  wfc_data_grids_read, wfc_data_kread, wfc_data_deallocate
      
   IMPLICIT NONE

   !
   ! input variables
   !
   CHARACTER(nstrx) :: wann             ! contains the list of WF indexes to plot 
                                        ! in the fmt e.g. "1-3,4,7-9"
   REAL(dbl)        :: r1min, r1max     ! plot cell dim along a1 (cry units)
   REAL(dbl)        :: r2min, r2max     ! the same but for a2
   REAL(dbl)        :: r3min, r3max     ! the same but for a3
   CHARACTER( 20 )  :: datatype         ! ( "modulus" | "real" | "imaginary"  )
   CHARACTER( 20 )  :: output_fmt       ! ( "txt" | "plt" | "cube" | "xsf" )
   LOGICAL          :: assume_ncpp      ! If .TRUE. pp's are not read
   LOGICAL          :: locate_wf        ! move the centers of WF in a unit cell centered
                                        ! around the origin
   !
   ! input namelist
   !
   NAMELIST /INPUT/ prefix, postfix, work_dir, wann, &
                    datatype, assume_ncpp, output_fmt, locate_wf, &
                    r1min, r1max, r2min, r2max, r3min, r3max, spin_component

   !
   ! local variables
   !
   INTEGER :: nrxl, nryl, nrzl
   INTEGER :: nrxh, nryh, nrzh
   INTEGER :: nnrx, nnry, nnrz
   INTEGER :: nx, ny, nz, nzz, nyy, nxx
   !
   INTEGER :: ia, ib, ik, ig, ir
   INTEGER :: natot, nplot
   INTEGER :: m, n, i, j, ierr
   INTEGER :: zatom
   INTEGER, ALLOCATABLE :: map(:), iwann(:)

   COMPLEX(dbl) :: caux, cmod
   COMPLEX(dbl), ALLOCATABLE :: cwann(:,:,:,:)   
   COMPLEX(dbl), ALLOCATABLE :: kwann(:,:)       
   COMPLEX(dbl), ALLOCATABLE :: cutot(:,:)

   REAL(dbl)    :: tmaxx, tmax
   REAL(dbl)    :: avecl(3,3), raux(3), r0(3), r1(3), rmin(3), rmax(3)
   REAL(dbl),    ALLOCATABLE :: rwann_out(:,:,:)
   REAL(dbl),    ALLOCATABLE :: tautot(:,:), tau_cry(:,:)
   REAL(dbl),    ALLOCATABLE :: vkpt_cry(:,:)
   REAL(dbl),    ALLOCATABLE :: rave_cry(:,:), rave_shift(:,:)
   REAL(dbl)    :: arg
   COMPLEX(dbl) :: phase
   CHARACTER(3), ALLOCATABLE :: symbtot(:)

   CHARACTER( nstrx )  :: filename
   CHARACTER( 5 )      :: str, aux_fmt
   LOGICAL             :: lfound
   LOGICAL             :: okp( 3 )
   !
   ! end of declariations
   !

!
!------------------------------
! main body
!------------------------------
!
      CALL startup(version_number,'plot')


!
! ... Read INPUT namelist from stdin
!
      prefix                      = 'WanT'
      postfix                     = ' '
      work_dir                    = './'
      assume_ncpp                 = .FALSE.
      locate_wf                   = .TRUE.
      wann                        = ' '
      datatype                    = 'modulus'
      output_fmt                  = 'plt'
      spin_component              = 'none'
      r1min                       = -0.5
      r1max                       =  0.5
      r2min                       = -0.5
      r2max                       =  0.5
      r3min                       = -0.5
      r3max                       =  0.5
      

      READ(stdin, INPUT, IOSTAT=ierr)
      IF ( ierr /= 0 )  CALL errore('plot','Unable to read namelist INPUT',ABS(ierr))

      !
      ! Some checks
      !
      IF ( LEN_TRIM( wann) == 0 ) CALL errore('plot', 'wann not supplied ', 1)
      !
      CALL change_case( datatype, 'lower')
      IF ( TRIM(datatype) /= "modulus" .AND. TRIM(datatype) /= "real" .AND. &
           TRIM(datatype) /= "imaginary"  ) &
           CALL errore('plot','invalid DATATYPE = '//TRIM(datatype),2)
           !
      CALL change_case(spin_component,'lower')
      IF ( TRIM(spin_component) /= "none" .AND. TRIM(spin_component) /= "up" .AND. &
           TRIM(spin_component) /= "down" ) &
           CALL errore('plot', 'Invalid spin_component = '//TRIM(spin_component), 3)
           !
      CALL change_case(output_fmt,'lower')
      IF ( TRIM(output_fmt) /= "txt" .AND. TRIM(output_fmt) /= "plt" .AND. &
           TRIM(output_fmt) /= "cube" .AND. TRIM(output_fmt) /= "xsf" ) &
           CALL errore('plot', 'Invalid output_fmt = '//TRIM(output_fmt), 4)

      IF ( r1min > r1max ) CALL errore('plot', 'r1min > r1max',1)
      IF ( r2min > r2max ) CALL errore('plot', 'r2min > r2max',2)
      IF ( r3min > r3max ) CALL errore('plot', 'r3min > r3max',3)
      IF ( ABS(r1max -r1min) < EPS_m4 ) CALL errore('plot', 'r1 too small',1)
      IF ( ABS(r2max -r2min) < EPS_m4 ) CALL errore('plot', 'r2 too small',2)
      IF ( ABS(r3max -r3min) < EPS_m4 ) CALL errore('plot', 'r3 too small',3)
      rmin(1) = r1min 
      rmin(2) = r2min
      rmin(3) = r3min
      rmax(1) = r1max 
      rmax(2) = r2max
      rmax(3) = r3max

      read_pseudo = .NOT. assume_ncpp



!
! ... Getting previous WanT data
!
      CALL want_init( WANT_INPUT = .FALSE., WINDOWS=.FALSE., BSHELLS=.FALSE., &
                      PSEUDO=read_pseudo)

      !
      ! Read Subspace data
      !
      CALL ioname('space',filename)
      CALL file_open(space_unit,TRIM(filename),PATH="/",ACTION="read",FORM="formatted")
          CALL windows_read(space_unit,"WINDOWS",lfound)
          IF ( .NOT. lfound ) CALL errore('plot',"unable to find WINDOWS",1)
          CALL subspace_read(space_unit,"SUBSPACE",lfound)
          IF ( .NOT. lfound ) CALL errore('plot',"unable to find SUBSPACE",1)
      CALL file_close(space_unit,PATH="/",ACTION="read")

      CALL ioname('space',filename,LPATH=.FALSE.)
      WRITE( stdout,"(2x,'Subspace data read from file: ',a)") TRIM(filename)

      !
      ! Read unitary matrices U(k) that rotate the bloch states
      !
      CALL ioname('wannier',filename)
      CALL file_open(wan_unit,TRIM(filename),PATH="/",ACTION="read",FORM="formatted")
          CALL localization_read(wan_unit,"WANNIER_LOCALIZATION",lfound)
          IF ( .NOT. lfound ) CALL errore('plot','searching WANNIER_LOCALIZATION',1)
      CALL file_close(wan_unit,PATH="/",ACTION="read")

      CALL ioname('wannier',filename,LPATH=.FALSE.)
      WRITE( stdout,"('  Wannier data read from file: ',a,/)") TRIM(filename)

     
      !
      ! Print data to output
      !
      CALL summary( stdout, LINPUT=.FALSE., LATOMS=.FALSE., LEIG=.FALSE. )

      !
      ! should be eliminated ASAP
      !
      IF ( use_uspp ) & !  CALL errore('plot','USPP not yet implemented',1)
             WRITE(stdout,"(/,2x,'WARNING: USPP not fully implemented',/)") 
     
      !
      ! opening the file containing the PW-DFT data
      !
      CALL ioname('export',filename,LPOSTFIX=.FALSE.)
      CALL file_open(dft_unit,TRIM(filename),PATH="/",ACTION="read", &
                              FORM='formatted')
      CALL ioname('export',filename,LPATH=.FALSE.,LPOSTFIX=.FALSE.)

      !
      ! ... Read grids
      WRITE( stdout,"(/,2x,'Reading density G-grid from file: ',a)") TRIM(filename)
      CALL ggrids_read_ext(dft_unit)
      !
      ! ... Read wfcs
      WRITE( stdout,"(  2x,'Reading Wfc grids from file: ',a)") TRIM(filename)
      CALL wfc_data_grids_read(dft_unit)
      !
      ! ... closing the main data file
      CALL file_close(dft_unit,PATH="/",ACTION="read")

      !
      ! ... stdout summary about grids
      !
      WRITE(stdout,"(2/,10x,'Energy cut-off for wfcs =  ',5x,F7.2,' (Ry)' )") ecutwfc
      WRITE(stdout, "(25x,'for rho  =  ', 5x, F7.2, ' (Ry)' )")  ecutrho
      WRITE(stdout, "(6x,'Total number of PW for rho  =  ',i9)") npw_rho
      WRITE(stdout, "(6x,'  Max number of PW for wfc  =  ',i9)") npwkx
      WRITE(stdout, "(6x,'Total number of PW for wfcs =  ',i9)") MAXVAL(igsort(:,:))+1
      WRITE(stdout, "(6x,'  FFT grid components (rho) =  ( ', 3i5,' )' )") nfft(:)


!
! ... final settings on input
!
      !
      ! get the exact number of plot
      CALL parser_replica( wann, nplot, IERR=ierr )
      IF ( ierr/=0 ) CALL errore('plot','wrong FMT in wann string I',ABS(ierr))
      !
      ALLOCATE( iwann(nplot), STAT=ierr )
      IF ( ierr/=0 ) CALL errore('plot','allocating iwann',ABS(ierr))
      !
      ! get the WF indexes
      CALL parser_replica( wann, nplot, iwann, ierr )
      IF ( ierr/=0 ) CALL errore('plot','wrong FMT in wann string II',ABS(ierr))
      !
      DO m = 1, nplot
         IF ( iwann(m) <= 0  .OR. iwann(m) > dimwann ) &
              CALL errore('plot','iwann too large',m)
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
                CALL errore('plot','lattice not orthorombic: use xsf or cube output_fmt',4)
      ENDIF

      !
      ! set the FFT mesh
      !
      nrxl = NINT( r1min * nfft(1) )
      nrxh = NINT( r1max * nfft(1) )
      nryl = NINT( r2min * nfft(2) )
      nryh = NINT( r2max * nfft(2) )
      nrzl = NINT( r3min * nfft(3) )
      nrzh = NINT( r3max * nfft(3) )

      !
      ! summary of the input
      !
      WRITE( stdout, "(2/,2x,70('='))" )
      WRITE( stdout, "(2x,'=',21x,'Wannier function plotting',22x,'=')" )
      WRITE( stdout, "(2x,70('='),2/)" )

      WRITE( stdout,"(2x,'nplot = ',i4, ' Wannier func.s to be plotted')") nplot
      DO m=1,nplot
          WRITE( stdout,"(5x,'wf (',i4,' ) = ',i4 )") m, iwann(m)
      ENDDO
      WRITE( stdout,"(/,2x,'Data type  :',3x,a)") TRIM(datatype)
      WRITE( stdout,"(  2x,'Output fmt :',3x,a)") TRIM(output_fmt)
      WRITE( stdout,"(/,2x,'Plotting Cell dimensions [cryst. coord]:')") 
      WRITE( stdout,"(  6x, ' r1 :  ', f8.4, ' --> ', f8.4 )" ) r1min, r1max
      WRITE( stdout,"(  6x, ' r2 :  ', f8.4, ' --> ', f8.4 )" ) r2min, r2max
      WRITE( stdout,"(  6x, ' r3 :  ', f8.4, ' --> ', f8.4 )" ) r3min, r3max
      WRITE( stdout,"(/,2x,'Grid dimensions:')") 
      WRITE( stdout,"(  6x, 'nrx :  ', i8, ' --> ', i8 )" ) nrxl, nrxh 
      WRITE( stdout,"(  6x, 'nry :  ', i8, ' --> ', i8 )" ) nryl, nryh 
      WRITE( stdout,"(  6x, 'nrz :  ', i8, ' --> ', i8 )" ) nrzl, nrzh 
      WRITE( stdout,"()")


!
! Initialize the data used for the fast fourier transforms
!
      IF( nrxh - nrxl < 0 ) CALL errore( 'plot', 'wrong nrxl and nrxh ', 1 )
      IF( nryh - nryl < 0 ) CALL errore( 'plot', 'wrong nryl and nryh ', 1 )
      IF( nrzh - nrzl < 0 ) CALL errore( 'plot', 'wrong nrzl and nrzh ', 1 )
      !
      nnrx = ABS( nrxl / nfft(1) ) + 2
      nnry = ABS( nryl / nfft(2) ) + 2
      nnrz = ABS( nrzl / nfft(3) ) + 2

      
      !
      ! allocate WF and local data
      !
      ALLOCATE ( cwann( nrxl:nrxh, nryl:nryh, nrzl:nrzh, nplot ), STAT=ierr ) 
         IF( ierr /=0 ) CALL errore('plot', 'allocating cwann ', ABS(ierr) )
      ALLOCATE( kwann( PRODUCT(nfft), nplot ), STAT=ierr )
         IF( ierr /=0 ) CALL errore('plot', 'allocating kwann ', ABS(ierr) )
      ALLOCATE( vkpt_cry(3, nkpts), STAT=ierr )
         IF( ierr /=0 ) CALL errore('plot', 'allocating vkpt_cry ', ABS(ierr) )
      ALLOCATE( rave_cry(3, dimwann), STAT=ierr )
         IF( ierr /=0 ) CALL errore('plot', 'allocating rave_cry ', ABS(ierr) )
      ALLOCATE( rave_shift(3, dimwann), STAT=ierr )
         IF( ierr /=0 ) CALL errore('plot', 'allocating rave_shift ', ABS(ierr) )
      ALLOCATE( cutot(dimwinx, dimwann), STAT=ierr )
         IF( ierr /=0 ) CALL errore('plot', 'allocating cutot ', ABS(ierr) )
      ALLOCATE( map(npwkx), STAT=ierr )
         IF( ierr /=0 ) CALL errore('plot', 'allocating map ', ABS(ierr) )


      !
      ! convert vkpt from cart coord (bohr^-1) to cryst 
      ! the same for rave, from cart coord (bohr) to cryst
      !
      vkpt_cry(:,:) = vkpt(:,:)
      rave_cry(:,:) = rave(:,:)
      CALL cart2cry( vkpt_cry, bvec )
      CALL cart2cry( rave_cry, avec)

      !
      ! determine the shift to rave to set the WF in the selected plotting cell
      ! select the starting point of the shift cell as the mid point of 
      ! rmin(i) and rmax(i)-1.0
      !
      rave_shift(:,:) = ZERO
      IF ( locate_wf ) THEN
         DO m=1,nplot
            DO i=1,3
               j = 0
               IF ( rave_cry(i,iwann(m)) -( rmin(i)+rmax(i)-ONE )/TWO < ZERO ) j = 1
               rave_shift(i,iwann(m)) = REAL( INT( rave_cry(i,iwann(m)) &
                                              -( rmin(i)+rmax(i)-ONE)/TWO ) -j ) 
            ENDDO
         ENDDO
      ENDIF

      !
      WRITE( stdout, " (/,2x, 'Centers for the required Wannier functions:')")
      WRITE( stdout, " (/,8x,'in cartesian coord (Bohr)' )")
      DO m=1,nplot
         WRITE( stdout, " (4x,'Wf(',i4,' ) = (', 3f13.6, ' )' )" ) &
                iwann(m), rave(:,iwann(m))
      ENDDO
      !
      WRITE( stdout, " (8x,'in crystal coord' )")
      DO m=1,nplot
         WRITE( stdout, " (4x,'Wf(',i4,' ) = (', 3f13.6, ' )' )" ) &
                iwann(m), rave_cry(:,iwann(m))
      ENDDO
      !
      IF ( locate_wf ) THEN
          WRITE( stdout,"(/,2x,'Locating WFs: ')")
          WRITE( stdout,"(2x,'Plotting Cell dimensions [cryst. coord]:')") 
          DO i=1,3
             WRITE( stdout,"(  6x, ' r',i1,' :  ', f8.4, ' --> ', f8.4 )" ) &
                    i, (rmin(i)+rmax(i)-ONE)/TWO, (rmin(i)+rmax(i)+ONE)/TWO
          ENDDO
          WRITE( stdout,"(/,2x,'New center positions [cryst. coord]:')") 
          WRITE( stdout,"(8x,'in crystal coord' )")
          DO m=1,nplot
             WRITE( stdout, " (4x,'Wf(',i4,' ) = (', 3f13.6, ' )' )" ) &
                    iwann(m), rave_cry(:,iwann(m))-rave_shift(:,iwann(m))
          ENDDO
          !
      ENDIF
      WRITE( stdout, "(/)" )


!
! Get the atoms in the plotting cell
!
      ALLOCATE( tau_cry(3, nat), STAT=ierr  )  
         IF( ierr /=0 ) CALL errore('plot', 'allocating tau_cry', ABS(ierr) ) 
      ALLOCATE( tautot( 3, 125*nat ), STAT=ierr )
         IF( ierr /=0 ) CALL errore('plot', 'allocating tautot ', ABS(ierr) ) 
      ALLOCATE( symbtot( 125*nat ), STAT=ierr )
         IF( ierr /=0 ) CALL errore('plot', 'allocating symbtot ', ABS(ierr) ) 

      tau_cry(:,:) = tau(:,:) * alat
      CALL cart2cry( tau_cry, avec )  

      natot = 0
      DO ia = 1, nat
           DO nx = -2, 2
           DO ny = -2, 2
           DO nz = -2, 2
                raux(1) = ( tau_cry(1,ia) + REAL(nx, dbl) ) * REAL( nfft(1), dbl )
                raux(2) = ( tau_cry(2,ia) + REAL(ny, dbl) ) * REAL( nfft(2), dbl )
                raux(3) = ( tau_cry(3,ia) + REAL(nz, dbl) ) * REAL( nfft(3), dbl )

                okp(1) = ( raux(1) >= (nrxl - 1) ) .AND. ( raux(1) < nrxh )
                okp(2) = ( raux(2) >= (nryl - 1) ) .AND. ( raux(2) < nryh ) 
                okp(3) = ( raux(3) >= (nrzl - 1) ) .AND. ( raux(3) < nrzh ) 
                IF( okp(1) .AND. okp(2) .AND. okp(3) ) THEN
                     natot = natot+1
                     tautot(:,natot) = raux(:) / REAL(nfft(:), dbl)
                     symbtot(natot)  = symb( ia )
                ENDIF
           ENDDO
           ENDDO
           ENDDO
      ENDDO

      !
      ! convert atoms to cartesian coords (bohr)
      !
      CALL cry2cart( tautot(:,1:natot), avec )
      
      WRITE(stdout, " (2x,'Atoms in the selected cell: (cart. coord. in Bohr)' ) " )
      DO ia = 1, natot
           WRITE( stdout, "(5x, a, 2x,'tau( ',I3,' ) = (', 3F12.7, ' )' )" ) &
                  symbtot(ia), ia, (tautot( i, ia ), i = 1, 3)
      ENDDO
      WRITE(stdout, "(/)" )


!
! wfc allocations
!
      CALL wfc_info_allocate(npwkx, dimwinx, nkpts, dimwinx, evc_info)
      ALLOCATE( evc(npwkx, dimwinx ), STAT=ierr )
         IF (ierr/=0) CALL errore('plot','allocating EVC',ABS(ierr))

      !
      ! re-opening the file containing the PW-DFT data
      !
      CALL ioname('export',filename,LPOSTFIX=.FALSE.)
      CALL file_open(dft_unit,TRIM(filename),PATH="/Eigenvectors/", &
                              ACTION="read", FORM='formatted')
      
     
!
! ... Main loop on wfcs
!

      cwann( :, :, :, : ) = CZERO
    
      kpoint_loop: &
      DO ik = 1, nkpts
          ! 
          WRITE(stdout, "(4x,'Wfc Fourier Transf. for k-point ',i4 )") ik

          !
          ! getting wfc 
          !
          CALL wfc_data_kread(dft_unit, ik, "IK", evc, evc_info)

          !
          ! built the right transformation to rotate the original wfcs.
          !
          CALL mat_mul( cutot, eamp(:,:,ik), 'N', cu(:,:,ik), 'N' ,  &
                        dimwin(ik), dimwann, dimwann)

          !
          ! set the FFT map
          !
          map(:) = 0
          CALL ggrids_gk_indexes( igv, igsort(:,ik), npwk(ik), & 
                                  nfft(1), nfft(2), nfft(3), GK2FFT=map ) 
         
          !
          ! set the kwann function
          !
          DO m = 1, nplot

              CALL timing('kwann_calc',OPR='start')

              !
              ! apply a globgal shift to set the WF in the required cell
              !
              arg = TPI * DOT_PRODUCT( vkpt_cry(:,ik), rave_shift(:,iwann(m)) )
              phase = CMPLX( COS(arg), SIN(arg), dbl )


              kwann( :, m ) = CZERO
              !
              DO ig = 1, npwk(ik)
                 !
                 DO n = 1, dimwin(ik)
     
                     ib =  wfc_info_getindex(imin(ik)+n-1, ik, "IK", evc_info )
                     !
                     kwann( map(ig), m ) = kwann( map(ig), m ) + &
                                           cutot(n, iwann(m) ) * evc( ig, ib )
                 ENDDO
                 !
                 ! apply the translation phase
                 kwann( map(ig) ,m) = kwann( map(ig) ,m) * phase 
              ENDDO
              !
              CALL timing('kwann_calc',OPR='stop')
          ENDDO

          !
          ! logically clean wfcs (but memory is NOT free)
          !
          CALL wfc_info_delete(evc_info, LABEL="IK")


          ! 
          ! FFT call 
          ! 
          DO m=1,nplot
             CALL timing('cfft3d',OPR='start')
             CALL cfft3d( kwann(:,m), nfft(1), nfft(2), nfft(3),  &
                                      nfft(1), nfft(2), nfft(3),  1 )
             CALL timing('cfft3d',OPR='stop')

             CALL timing('cwann_calc',OPR='start')
             !
             ! loop over FFT grid
             !
             DO nzz = nrzl, nrzh
                 nz = MOD( nzz + nnrz * nfft(3) , nfft(3) ) + 1
                 DO nyy = nryl, nryh
                     ny = MOD( nyy + nnry * nfft(2) , nfft(2) ) + 1
                     DO nxx = nrxl, nrxh
                         nx = MOD( nxx + nnrx * nfft(1) , nfft(1) ) + 1

                         ir = nx + (ny-1) * nfft(1) + (nz-1) * nfft(1) * nfft(2)

                         arg   = vkpt_cry(1,ik) * REAL(nxx, dbl) / REAL(nfft(1), dbl) + &
                                 vkpt_cry(2,ik) * REAL(nyy, dbl) / REAL(nfft(2), dbl) + &
                                 vkpt_cry(3,ik) * REAL(nzz, dbl) / REAL(nfft(3), dbl)
                         caux  = CMPLX( COS( TPI*arg ), SIN( TPI*arg), dbl ) * &
                                 kwann( ir, m ) 

                         cwann( nxx, nyy, nzz, m) = cwann( nxx, nyy, nzz, m) + caux
                     ENDDO
                 ENDDO
             ENDDO
             !
             CALL timing('cwann_calc',OPR='stop')
          ENDDO 
          !
          CALL timing_upto_now(stdout)
      ENDDO kpoint_loop
      !
      WRITE(stdout, "()")

      !
      ! clean the large amount of memory used by wfcs and grids
      !
      DEALLOCATE( kwann, STAT=ierr)
         IF (ierr/=0) CALL errore('plot','deallocating kwann',ABS(ierr))
      CALL wfc_data_deallocate()
      CALL ggrids_deallocate()
      CALL file_close(dft_unit,PATH="/Eigenvectors/",ACTION="read")


      ! 
      ! Fix the global phase by setting the wannier to be real 
      ! at the point where it has max modulus
      ! 

      arg = ONE / ( SQRT( REAL(nkpts, dbl) * REAL( SIZE(cwann(:,:,:,1)), dbl) ) ) 
      !
      DO m = 1, nplot
          !
          tmaxx = ZERO
          cmod = CONE
          !
          DO nzz = nrzl, nrzh
          DO nyy = nryl, nryh
          DO nxx=  nrxl, nrxh
               cwann( nxx, nyy, nzz, m ) = cwann( nxx, nyy, nzz, m ) * arg
               tmax = REAL (cwann( nxx, nyy, nzz, m) * CONJG( cwann( nxx, nyy, nzz, m) ) )
               IF ( tmax > tmaxx ) THEN
                    tmaxx = tmax
                    cmod = cwann( nxx, nyy, nzz, m)
               ENDIF
          ENDDO
          ENDDO
          ENDDO

          ! set the phase and invert it
          cmod = cmod / SQRT( cmod * CONJG(cmod) ) 
          cmod = CONJG( cmod )   
          !
          cwann(:,:,:,m) = cwann(:,:,:,m) * cmod
      ENDDO

      
!
! We prepare a graphic output for gopenmol, dans or xcrysden; 
!

      !
      ! Offset for position and WF's allignment
      ! r0, r1  (bohr)
      !
      r0(1) = REAL( nrxl, dbl ) / REAL( nfft(1), dbl )
      r0(2) = REAL( nryl, dbl ) / REAL( nfft(2), dbl )
      r0(3) = REAL( nrzl, dbl ) / REAL( nfft(3), dbl )
      !
      r1(1) = REAL( nrxh, dbl ) / REAL( nfft(1), dbl )
      r1(2) = REAL( nryh, dbl ) / REAL( nfft(2), dbl )
      r1(3) = REAL( nrzh, dbl ) / REAL( nfft(3), dbl )
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
           CALL errore('plot','invalid OUTPUT_FMT '//TRIM(output_fmt),4)
      END SELECT

      !
      ! workspace for output writing
      !
      ALLOCATE( rwann_out( nrxl:nrxh, nryl:nryh, nrzl:nrzh ), STAT=ierr ) 
         IF (ierr/=0) CALL errore('plot','allocating rwann_out',ABS(ierr))


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
              rwann_out(:,:,:) = REAL( cwann(:,:,:,m) * CONJG( cwann(:,:,:,m) ) )
          CASE( "real" )    
              str = "_WFR"
              rwann_out(:,:,:) = REAL( cwann(:,:,:,m) )
          CASE( "imaginary" )    
              str = "_WFI"
              rwann_out(:,:,:) = AIMAG( cwann(:,:,:,m) )
          CASE DEFAULT
              CALL errore('plot','invalid DATATYPE '//TRIM(datatype),3)
          END SELECT 


          filename=TRIM(work_dir)//"/"//TRIM(prefix)//TRIM(postfix)
          IF ( iwann(m) <= 9 ) THEN
               filename=TRIM(filename)//TRIM(str)//"00"//TRIM(int2char(iwann(m)))
          ELSE IF ( iwann(m) <= 99 ) THEN
               filename=TRIM(filename)//TRIM(str)//"0"//TRIM(int2char(iwann(m)))
          ELSE IF ( iwann(m) <= 999 ) THEN
               filename=TRIM(filename)//TRIM(str)//TRIM(int2char(iwann(m)))
          ELSE
              CALL errore('plot','iwann(m) > 999', iwann(m))
          ENDIF
          !
          !
          WRITE( stdout,"(2x,'writing WF(',i4,') plot on file: ',a)") &
                 iwann(m), TRIM(filename)//TRIM(aux_fmt)
          OPEN ( aux_unit, FILE=TRIM(filename)//TRIM(aux_fmt), FORM='formatted', &
                           STATUS='unknown', IOSTAT=ierr )
          IF (ierr/=0) CALL errore('plot','opening file '//TRIM(filename)//TRIM(aux_fmt),1)


          SELECT CASE ( TRIM(output_fmt) )
          CASE( "cube", "plt" )

              ! 
              ! bohr
              DO i=1,3
                 avecl(:,i) = avec(:,i) / REAL(nfft(i), dbl) 
              ENDDO

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

          CASE( "txt" )

              WRITE(aux_unit, '( " 3 2" )') 
              WRITE(aux_unit, '( 3i5 )' ) nrzh-nrzl+1, nryh-nryl+1, nrxh-nrxl+1
              WRITE(aux_unit, '(6f10.4)' ) r0(3) * bohr, r1(3) * bohr,  &
                                           r0(2) * bohr, r1(2) * bohr,  & 
                                           r0(1) * bohr, r1(1) * bohr

              DO nz = nrzl, nrzh
              DO ny = nryl, nryh
              DO nx = nrxl, nrxh
                  WRITE( aux_unit, "(f20.10)" ) rwann_out( nx, ny, nz )
              ENDDO
              ENDDO
              ENDDO

          CASE( "xsf" )

              ! 
              ! bohr
              avecl(:,1) = avec(:,1) * REAL(nrxh-nrxl+1, dbl) / REAL(nfft(1), dbl) 
              avecl(:,2) = avec(:,2) * REAL(nryh-nryl+1, dbl) / REAL(nfft(2), dbl) 
              avecl(:,3) = avec(:,3) * REAL(nrzh-nrzl+1, dbl) / REAL(nfft(3), dbl) 
              
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

          CASE DEFAULT
              CALL errore('plot','invalid OUTPUT_FMT '//TRIM(output_fmt),5)
          END SELECT
          !
          CLOSE(aux_unit)

        
          !
          ! ... write a XYZ file for the atomic positions, when the case
          !
          IF ( TRIM( output_fmt ) == "txt" .OR. TRIM( output_fmt ) == "plt" ) THEN
              WRITE( stdout,"(18x,'atomic positions on file: ',a)") TRIM(filename)//".xyz"
              OPEN ( aux1_unit, FILE=TRIM(filename)//".xyz", FORM='formatted', &
                                STATUS='unknown', IOSTAT=ierr )
              IF (ierr/=0) CALL errore('plot','opening file '//TRIM(filename)//".xyz",1)
              !
              WRITE(aux1_unit,"(i6,/)") natot
              DO ia = 1, natot
                  WRITE(aux1_unit, '(a2,3x,3f15.9)' ) symbtot(ia) , tautot( :, ia )*bohr
              ENDDO
              !
              CLOSE( aux1_unit )
          ENDIF


          !
          ! ... convert output to PLT fmt, if the case
          !
          IF ( TRIM( output_fmt ) == "plt" ) THEN
               CALL timing('gcubeplt',OPR='start')
               CALL gcube2plt( filename, LEN_TRIM(filename) )
               CALL timing('gcubeplt',OPR='stop')
               !
               ! removing temporary .cube file
               WRITE(stdout,"(2x,'deleting tmp files: ',a)" ) TRIM(filename)//".cube"
               WRITE(stdout,"(22x,a,2/)" )                    TRIM(filename)//".crd"
               CALL file_delete( TRIM(filename)//".cube" )
               CALL file_delete( TRIM(filename)//".crd" )
               !
          ENDIF

      ENDDO

      WRITE( stdout, "(/,2x,70('='))" )



!
! ... Finalize timing
!
      CALL timing('plot',OPR='stop')
      CALL timing_overview(stdout,LIST=global_list,MAIN_NAME='plot')

!
! ... Clean up memory
!
      DEALLOCATE( iwann, STAT=ierr )
         IF( ierr /=0 ) CALL errore('plot', 'deallocating iwann', ABS(ierr) )
      DEALLOCATE( tautot, STAT=ierr )
         IF( ierr /=0 ) CALL errore('plot', 'deallocating tautot', ABS(ierr) )
      DEALLOCATE( symbtot, STAT=ierr )
         IF( ierr /=0 ) CALL errore('plot', 'deallocating symbtot', ABS(ierr) )
      DEALLOCATE( vkpt_cry, STAT=ierr )
         IF( ierr /=0 ) CALL errore('plot', 'deallocating vkpt_cry', ABS(ierr) )
      DEALLOCATE( rave_cry, STAT=ierr )
         IF( ierr /=0 ) CALL errore('plot', 'deallocating rave_cry', ABS(ierr) )
      DEALLOCATE( rave_shift, STAT=ierr )
         IF( ierr /=0 ) CALL errore('plot', 'deallocating rave_shift', ABS(ierr) )
      DEALLOCATE( tau_cry, STAT=ierr )
         IF( ierr /=0 ) CALL errore('plot', 'deallocating tau_cry', ABS(ierr) )
      DEALLOCATE( cutot, STAT=ierr )
         IF( ierr /=0 ) CALL errore('plot', 'deallocating cutot ', ABS(ierr) )
      DEALLOCATE( map, STAT=ierr )
         IF( ierr /=0 ) CALL errore('plot', 'deallocating map ', ABS(ierr) )
      DEALLOCATE( rwann_out, STAT=ierr )
         IF( ierr /=0 ) CALL errore('plot', 'deallocating rwann_out', ABS(ierr) )

      CALL cleanup 

END PROGRAM plot

