!
! Copyright (C) 2005 WanT Group
!
! This file is distributed under the terms of the
! GNU General Public License. See the file `License'
! in the root directory of the present distribution,
! or http://www.gnu.org/copyleft/gpl.txt .
!
!******************************************
   PROGRAM plot
   !******************************************
   !
   ! real space plot of the computed Wannier functions
   !
   USE kinds
   USE constants,          ONLY : ZERO, CZERO, ONE, CONE, CI, TPI, &
                                  bohr => bohr_radius_angs, EPS_m6
   USE parameters,         ONLY : ntypx, natx, nstrx
   USE fft_scalar,         ONLY : cfft3d
   USE timing_module,      ONLY : timing, timing_overview, global_list
   USE io_module,          ONLY : prefix, postfix, work_dir, stdin, stdout
   USE io_module,          ONLY : space_unit, wan_unit, dft_unit, aux_unit, ioname
   USE control_module,     ONLY : read_pseudo, use_uspp
   USE files_module,       ONLY : file_open, file_close, file_delete
   USE parser_module
   USE startup_module,     ONLY : startup
   USE cleanup_module,     ONLY : cleanup
   USE want_init_module,   ONLY : want_init
   USE version_module,     ONLY : version_number
   USE util_module,        ONLY : zmat_mul
   USE converters_module,  ONLY : cry2cart, cart2cry
   USE summary_module,     ONLY : summary
   USE parser_module
   !
   USE lattice_module,     ONLY : avec, bvec, alat, omega
   USE ions_module,        ONLY : nsp, symb, na, tau, ityp, nat
   USE kpoints_module,     ONLY : nkpts, vkpt
   USE windows_module,     ONLY : imin, imax, dimwin, dimwinx, windows_read, &
                                  spin_component
   USE subspace_module,    ONLY : dimwann, eamp, subspace_read
   USE localization_module,ONLY : cu, localization_read
   USE ggrids_module,      ONLY : nfft, npw_rho, ecutwfc, ecutrho, igv, &
                                  ggrids_read_ext, ggrids_deallocate, &
                                  ggrids_gk_indexes
   USE wfc_info_module
   USE wfc_data_module,    ONLY : npwkx, npwk, igsort, evc, evc_info, &
                                  wfc_data_grids_read, wfc_data_kread, wfc_data_deallocate
      
   IMPLICIT NONE

   !
   ! local variables
   !
   CHARACTER(nstrx) :: wann      ! contains the list of WF indexes to plot 
                                 ! in the fmt e.g. "1-3,4,7-9"
   INTEGER :: nrxl, nryl, nrzl
   INTEGER :: nrxh, nryh, nrzh
   INTEGER :: nnrx, nnry, nnrz
   CHARACTER( 20 )  :: datatype   ! ( "modulus" | "real" | "imaginary"  )
   CHARACTER( 20 )  :: output_fmt ! ( "plt" | "gau" )
   LOGICAL          :: assume_ncpp

   INTEGER :: nx, ny, nz, nzz, nyy, nxx
   INTEGER :: ia, ib, ik, ig, isp, ir
   INTEGER :: natot, nplot
   INTEGER :: m, n, i, j, ierr
   INTEGER :: zatom
   INTEGER, ALLOCATABLE :: map(:), iwann(:)

   COMPLEX(dbl) :: caux, cmod
   COMPLEX(dbl), ALLOCATABLE :: cwann(:,:,:,:)   
   COMPLEX(dbl), ALLOCATABLE :: kwann(:,:)       
   COMPLEX(dbl), ALLOCATABLE :: cutot(:,:)

   REAL(dbl)    :: arg, tmaxx, tmax
   REAL(dbl)    :: avecl(3,3), raux(3), r0(3)
   REAL(dbl),    ALLOCATABLE :: tautot(:,:), tau_cry(:,:)
   REAL(dbl),    ALLOCATABLE :: vkpt_cry(:,:)
   CHARACTER(3), ALLOCATABLE :: symbtot(:)

   CHARACTER( nstrx )  :: filename
   CHARACTER( 4 )      :: str
   LOGICAL             :: lfound
   LOGICAL             :: okp( 3 )
   !
   ! input namelist
   !
   NAMELIST /INPUT/ prefix, postfix, work_dir, wann, &
                    datatype, assume_ncpp, output_fmt, &
                    nrxl, nrxh, nryl, nryh, nrzl, nrzh, spin_component
   !
   ! end of declariations
   !

!
!------------------------------
! main body
!------------------------------
!
      CALL startup(version_number,MAIN_NAME='plot')


!
! ... Read INPUT namelist from stdin
!
      prefix                      = 'WanT'
      postfix                     = ' '
      work_dir                    = './'
      assume_ncpp                 = .FALSE.
      wann                        = ' '
      datatype                    = 'modulus'
      output_fmt                  = 'plt'
      spin_component              = 'none'
      nrxl                        = -50000
      nrxh                        =  50000
      nryl                        = -50000
      nryh                        =  50000
      nrzl                        = -50000
      nrzh                        =  50000
      

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
      IF ( TRIM(output_fmt) /= "plt" .AND. TRIM(output_fmt) /= "gau" ) &
           CALL errore('plot', 'Invalid output_fmt = '//TRIM(output_fmt), 4)
   
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
      ! get the WF indexes
      !
      ALLOCATE( iwann(dimwann), STAT=ierr )
      IF ( ierr/=0 ) CALL errore('plot','allocating iwann',ABS(ierr))
      !
      CALL parser_replica( wann, nplot, iwann, ierr )
      IF ( ierr/=0 ) CALL errore('plot','wrong FMT in wann string',ABS(ierr))
      !
      DO m = 1, nplot
         IF ( iwann(m) <= 0  .OR. iwann(m) > dimwann ) &
              CALL errore('plot','iwann too large',m)
      ENDDO
      !
      !
      ! if lattice is not orthorombic, only .gau output fmt is allowed
      !
      IF ( ABS( DOT_PRODUCT( avec(:,1), avec(:,2) )) > EPS_m6 .OR. &
           ABS( DOT_PRODUCT( avec(:,1), avec(:,3) )) > EPS_m6 .OR. &
           ABS( DOT_PRODUCT( avec(:,2), avec(:,3) )) > EPS_m6   ) THEN 
           !
           IF ( TRIM(output_fmt) == "plt" ) &
                CALL errore('plot','plt is allowed only in orthorombic latt',4)
      ENDIF

      !
      ! set the default FFT mesh
      !
      IF ( nrxl == -50000) nrxl = -nfft(1)/2
      IF ( nrxh ==  50000) nrxh =  nfft(1) -1 -nfft(1)/2
      IF ( nryl == -50000) nryl = -nfft(2)/2
      IF ( nryh ==  50000) nryh =  nfft(2) -1 -nfft(2)/2
      IF ( nrzl == -50000) nrzl = -nfft(3)/2
      IF ( nrzh ==  50000) nrzh =  nfft(3) -1 -nfft(3)/2

      !
      ! summary of the input
      !
      WRITE( stdout, "(2/,2x,70('='))" )
      WRITE( stdout, "(2x,'=',21x,'Wannier function plotting',22x,'=')" )
      WRITE( stdout, "(2x,70('='),2/)" )

      WRITE( stdout,"(2x,'nplot = ',i3, ' Wannier func.s to be plotted')") nplot
      DO m=1,nplot
          WRITE( stdout,"(5x,'wf (',i3,' ) = ',i4 )") m, iwann(m)
      ENDDO
      WRITE( stdout,"(/,2x,'Data type  :',3x,a)") TRIM(datatype)
      WRITE( stdout,"(  2x,'Output fmt :',3x,a)") TRIM(output_fmt)
      WRITE( stdout,"(/,2x,'Grid dimensions:')") 
      WRITE( stdout,"( 6x, 'nrx', i6, ' --> ', i6 )" ) nrxl, nrxh 
      WRITE( stdout,"( 6x, 'nry', i6, ' --> ', i6 )" ) nryl, nryh 
      WRITE( stdout,"( 6x, 'nrz', i6, ' --> ', i6 )" ) nrzl, nrzh 
      WRITE( stdout,"()")


!
! ... Initialize the data used for the fast fourier transforms
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
      ALLOCATE( cutot(dimwinx, dimwann), STAT=ierr )
         IF( ierr /=0 ) CALL errore('plot', 'allocating cutot ', ABS(ierr) )
      ALLOCATE( map(npwkx), STAT=ierr )
         IF( ierr /=0 ) CALL errore('plot', 'allocating map ', ABS(ierr) )


      !
      ! convert vkpt from cart coord (bohr^-1) to cryst 
      !
      vkpt_cry(:,:) = vkpt(:,:)
      CALL cart2cry( vkpt_cry, bvec )
 

      !
      ! ... allocating wfcs
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
          ! getting wfc 
          !
          CALL wfc_data_kread(dft_unit, ik, "IK", evc, evc_info)

          !
          ! built the right transformation to rotate the original wfcs.
          !
          CALL zmat_mul( cutot, eamp(:,:,ik), 'N', cu(:,:,ik), 'N' ,  &
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

              kwann( :, m ) = CZERO
              DO ig = 1, npwk(ik)
                 !
                 DO n = 1, dimwin(ik)
     
                     ib =  wfc_info_getindex(imin(ik)+n-1, ik, "IK", evc_info )
                     !
                     kwann( map(ig), m ) = kwann( map(ig), m ) + &
                                           cutot(n, iwann(m) ) * evc( ig, ib )
                 ENDDO
              ENDDO
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
          ENDDO


          !
          ! loop over FFT grid
          !
          DO m = 1, nplot 

              DO nzz = nrzl, nrzh
                  nz = MOD( nzz + nnrz * nfft(3) , nfft(3) ) + 1
                  DO nyy = nryl, nryh
                      ny = MOD( nyy + nnry * nfft(2) , nfft(2) ) + 1
                      DO nxx = nrxl, nrxh
                          nx = MOD( nxx + nnrx * nfft(1) , nfft(1) ) + 1

                          ir = nx + (ny-1) * nfft(1) + (nz-1) * nfft(1) * nfft(2)

                          arg   = vkpt_cry(1,ik) * DBLE(nxx) / DBLE(nfft(1)) + &
                                  vkpt_cry(2,ik) * DBLE(nyy) / DBLE(nfft(2)) + &
                                  vkpt_cry(3,ik) * DBLE(nzz) / DBLE(nfft(3))
                          caux  = CMPLX( COS( TPI*arg ), SIN( TPI*arg) ) * &
                                  kwann( ir, m ) 

                         cwann( nxx, nyy, nzz, m) = cwann( nxx, nyy, nzz, m) + caux
                      ENDDO
                  ENDDO
              ENDDO
          ENDDO 
      ENDDO kpoint_loop

      !
      ! clean the large amount of memory used by wfcs and grids
      !
      CALL wfc_data_deallocate()
      CALL ggrids_deallocate()
      CALL file_close(dft_unit,PATH="/Eigenvectors/",ACTION="read")


      ! 
      ! Fix the global phase by setting the wannier to be real 
      ! at the point where it has max modulus
      ! 

      arg = ONE / ( SQRT( REAL(nkpts) * SIZE(cwann(:,:,:,1) )  ) ) 

      DO m = 1, nplot
          !
          tmaxx = ZERO
          cmod = CONE
          !
          DO nzz = nrzl, nrzh
          DO nyy = nryl, nryh
          DO nxx=  nrxl, nrxh
               cwann( nxx, nyy, nzz, m ) = cwann( nxx, nyy, nzz, m ) * arg
               tmax = cwann( nxx, nyy, nzz, m) * CONJG( cwann( nxx, nyy, nzz, m) )
               IF ( tmax > tmaxx ) THEN
                   tmaxx = tmax
                   cmod = cwann( nxx, nyy, nzz, m)
               ENDIF
          ENDDO
          ENDDO
          ENDDO

          cmod = cmod / SQRT( cmod * CONJG(cmod) ) 
          DO nzz = nrzl, nrzh
          DO nyy = nryl, nryh
          DO nxx = nrxl, nryh
              cwann(nxx,nyy,nzz,m) = cwann(nxx,nyy,nzz,m) / cmod
          ENDDO
          ENDDO
          ENDDO
      ENDDO


!
! ... We prepare a graphic output for gopenmol or for dans; 
!     depending on the geometry of the cell, if it is orthorombic, gopenmol is used 
!     (this includes the cubic case); otherwise, dan is used.
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
                raux(1) = ( tau_cry(1,ia) + DBLE(nx) ) * nfft(1)
                raux(2) = ( tau_cry(2,ia) + DBLE(ny) ) * nfft(2)
                raux(3) = ( tau_cry(3,ia) + DBLE(nz) ) * nfft(3)

                okp(1) = ( raux(1) >= (nrxl - 1) ) .AND. ( raux(1) < nrxh )
                okp(2) = ( raux(2) >= (nryl - 1) ) .AND. ( raux(2) < nryh ) 
                okp(3) = ( raux(3) >= (nrzl - 1) ) .AND. ( raux(3) < nrzh ) 
                IF( okp(1) .AND. okp(2) .AND. okp(3) ) THEN
                     natot = natot+1
                     tautot(:,natot) = raux(:) / nfft(:)
                     symbtot(natot)  = symb( ia )
                ENDIF
           ENDDO
           ENDDO
           ENDDO
      ENDDO


      !
      ! convert atoms to cartesian coords (bohr)
      !
      CALL cry2cart( tautot, avec )
      
      WRITE(stdout, " (2x,'Atoms in the selected cell: (cart. coord. in Bohr)' ) " )
      DO ia = 1, natot
           WRITE( stdout, "(5x, a, 2x,'tau( ',I3,' ) = (', 3F12.7, ' )' )" ) &
                  symbtot(ia), ia, (tautot( i, ia ), i = 1, 3)
      ENDDO
      WRITE(stdout, "(/)" )

      
      !
      ! set avecl (bohr)
      !
      DO i=1,3
         avecl(:,i) = avec(:,i) / nfft(i) 
      ENDDO

      !
      ! Offset for position and WF's allignment
      !
      r0(1) = REAL( nrxl ) / REAL( nfft(1) )
      r0(2) = REAL( nryl ) / REAL( nfft(2) )
      r0(3) = REAL( nrzl ) / REAL( nfft(3) )
      CALL cry2cart( r0, avec )
      

      !
      ! output filename
      !
      SELECT CASE ( TRIM(datatype) )
      CASE( "modulus" )    
           str = "_WFM"
      CASE( "real" )    
           str = "_WFR"
      CASE( "imaginary" )    
           str = "_WFI"
      CASE DEFAULT
           CALL errore('plot','invalid DATATYPE '//TRIM(datatype),3)
      END SELECT 


      !
      ! final loop on different wfs
      !
      DO m = 1, nplot

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
          WRITE( stdout,"(2x,'writing WF(',i3,') plot on file: ',a)") &
                 iwann(m), TRIM(filename)//".gau"
          OPEN ( aux_unit, FILE=TRIM(filename)//".gau", FORM='formatted', STATUS='unknown' )

          WRITE(aux_unit, '(i4,3f12.6)' ) natot, r0(:) 
          WRITE(aux_unit, '(i4,3f12.6)' ) (nrxh-nrxl+1),  avecl(:,1) 
          WRITE(aux_unit, '(i4,3f12.6)' ) (nryh-nryl+1),  avecl(:,2) 
          WRITE(aux_unit, '(i4,3f12.6)' ) (nrzh-nrzl+1),  avecl(:,3) 

          DO ia = 1, natot
              CALL convert_label( symbtot(ia), zatom )
              WRITE(aux_unit, '(i4,4e13.5)' ) zatom, ONE, tautot( :, ia )
          ENDDO

          DO nx = nrxl, nrxh
          DO ny = nryl, nryh
              SELECT CASE ( TRIM(datatype) )

              CASE( "modulus" )    
                  WRITE( aux_unit, "(6e13.5)" ) &
                     REAL ( cwann( nx, ny, :, m) * CONJG( cwann( nx, ny, :, m)) )  

              CASE( "real" )    
                  WRITE( aux_unit, "(6e13.5)" ) REAL ( cwann( nx, ny, :, m) )

              CASE( "imaginary" )    
                  WRITE( aux_unit, "(6e13.5)" ) AIMAG( cwann( nx, ny, :, m) )

              CASE DEFAULT
                  CALL errore('plot','invalid DATATYPE '//TRIM(datatype),4)
              END SELECT 
          ENDDO
          ENDDO

          CLOSE(aux_unit)

          !
          ! ... convert output to PLT fmt, if the case
          !
          IF ( TRIM( output_fmt ) == "plt" ) THEN
               CALL timing('gcubeplt',OPR='start')
               CALL gcube2plt( filename, LEN_TRIM(filename) )
               CALL timing('gcubeplt',OPR='stop')
               !
               ! removing temporary .gau file
               WRITE(stdout,"(2x,'deleting tmp file: ',a,2/)" ) TRIM(filename)//".gau"
               CALL file_delete( TRIM(filename)//".gau" )
               !
          ELSEIF ( TRIM( output_fmt ) == "gau" ) THEN
               ! do nothing
          ELSE
               CALL errore('plot','Invalid output_fmt = '//TRIM(output_fmt),6)
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
      DEALLOCATE( kwann, STAT=ierr )
         IF( ierr /=0 ) CALL errore('plot', 'deallocating kwann', ABS(ierr) )
      DEALLOCATE( cwann, STAT=ierr )
         IF( ierr /=0 ) CALL errore('plot', 'deallocating cwann', ABS(ierr) )
      DEALLOCATE( tautot, STAT=ierr )
         IF( ierr /=0 ) CALL errore('plot', 'deallocating tautot', ABS(ierr) )
      DEALLOCATE( symbtot, STAT=ierr )
         IF( ierr /=0 ) CALL errore('plot', 'deallocating symbtot', ABS(ierr) )
      DEALLOCATE( vkpt_cry, STAT=ierr )
         IF( ierr /=0 ) CALL errore('plot', 'deallocating vkpt_cry', ABS(ierr) )
      DEALLOCATE( tau_cry, STAT=ierr )
         IF( ierr /=0 ) CALL errore('plot', 'deallocating tau_cry', ABS(ierr) )
      DEALLOCATE( cutot, STAT=ierr )
         IF( ierr /=0 ) CALL errore('plot', 'deallocating cutot ', ABS(ierr) )
      DEALLOCATE( map, STAT=ierr )
         IF( ierr /=0 ) CALL errore('plot', 'deallocating map ', ABS(ierr) )

      CALL cleanup 


   STOP '*** THE END *** (plot.x)'
   END PROGRAM plot

