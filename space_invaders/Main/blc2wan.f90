! 
! Copyright (C) 2005 WanT Group
!
! This file is distributed under the terms of the
! GNU General Public License. See the file `License'
! in the root directory of the present distribution,
! or http://www.gnu.org/copyleft/gpl.txt .
!
!*****************************************************
   PROGRAM blc2wan
   !*****************************************************
   !  
   ! Given a (dynamical) operator from file in the bloch basis
   ! it is calculated on the WFs basis
   !
   USE kinds
   USE constants,            ONLY: ZERO, CZERO
   USE parameters,           ONLY : nstrx
   USE io_module,            ONLY : stdout, stdin, ioname, space_unit, wan_unit, &
                                    in_unit => aux1_unit, out_unit => aux2_unit, &
                                    work_dir, prefix, postfix
   USE iotk_module
   USE files_module
   USE timing_module,        ONLY : timing, timing_overview, global_list
   USE startup_module,       ONLY : startup
   USE cleanup_module,       ONLY : cleanup
   USE want_init_module,     ONLY : want_init
   USE summary_module,       ONLY : summary
   USE version_module,       ONLY : version_number
   USE util_module,          ONLY : zmat_hdiag
   USE windows_module,       ONLY : nbnd, imin, imax, dimwin, dimwinx, windows_read
   USE subspace_module,      ONLY : eamp, subspace_read
   USE localization_module,  ONLY : cu, localization_read
   USE hamiltonian_module,   ONLY : dimwann, nws, nkpts, indxws, vws, &
                                    hamiltonian_init
   USE util_module,          ONLY : zmat_mul
   IMPLICIT NONE 

   !
   ! local variables
   !
   REAL(dbl),    ALLOCATABLE :: grid(:), kpt(:,:)
   REAL(dbl),    ALLOCATABLE :: vwss(:), norms(:)
   COMPLEX(dbl), ALLOCATABLE :: phase(:,:)
   COMPLEX(dbl), ALLOCATABLE :: opr_in(:,:,:), opr_out(:,:,:)
   COMPLEX(dbl), ALLOCATABLE :: oprk(:,:,:), cutot(:,:,:)
   COMPLEX(dbl), ALLOCATABLE :: aux(:), work(:,:)
   !
   REAL(dbl)                 :: arg
   CHARACTER(nstrx)          :: filename, filein, fileout
   CHARACTER(nstrx)          :: attr, name
   !
   INTEGER :: i, j, m, ie, ik, iws, ierr
   INTEGER :: nbnd_, nkpts_, nomega
   LOGICAL :: lfound, ldynamical, lband_diag, ascii
   !
   ! input namelist
   !
   NAMELIST /INPUT/ prefix, postfix, work_dir, filein, fileout, ascii
   !
   ! end of declariations
   !   

!
!------------------------------
! main body
!------------------------------
!
      CALL startup(version_number,MAIN_NAME='blc2wan')


!
! ... Read INPUT namelist from stdin
!
      prefix                      = 'WanT' 
      postfix                     = ' ' 
      work_dir                    = './' 
      filein                      = ' '
      fileout                     = ' '
      ascii                       = .FALSE.
      
      READ(stdin, INPUT, IOSTAT=ierr)
      IF ( ierr /= 0 )  CALL errore('blc2wan','Unable to read namelist INPUT',ABS(ierr))

      !
      ! Some checks 
      !
      IF ( LEN_TRIM(filein) == 0 )  CALL errore('blc2wan','invalid void filein',1)
      IF ( LEN_TRIM(fileout) == 0 ) CALL errore('blc2wan','invalid void fileout',2)
      IF ( TRIM(filein) == TRIM(fileout) ) CALL errore('blc2wan','filein and fileout equal',3)


!
! ... Getting previous WanT data
!

      CALL want_init( WANT_INPUT=.FALSE., WINDOWS=.FALSE., BSHELLS=.FALSE. )

      !
      ! Read Subspace data
      !
      CALL ioname('space',filename)
      CALL file_open(space_unit,TRIM(filename),PATH="/",ACTION="read",FORM="formatted")
          CALL windows_read(space_unit,"WINDOWS",lfound)
          IF ( .NOT. lfound ) CALL errore('bands',"unable to find WINDOWS",1)
          CALL subspace_read(space_unit,"SUBSPACE",lfound)
          IF ( .NOT. lfound ) CALL errore('blc2wan',"unable to find SUBSPACE",1)
      CALL file_close(space_unit,PATH="/",ACTION="read")

      CALL ioname('space',filename,LPATH=.FALSE.)
      WRITE( stdout,"(2x,'Space data read from file: ',a)") TRIM(filename)


      !
      ! Read wannier data
      !
      CALL ioname('wannier',filename)
      CALL file_open(wan_unit,TRIM(filename),PATH="/",ACTION="read",FORM="formatted")
          CALL localization_read(wan_unit,"WANNIER_LOCALIZATION",lfound)
          IF ( .NOT. lfound ) CALL errore('blc2wan',"unable to find WANNIER_LOCALIZATION",1)
      CALL file_close(wan_unit,PATH="/",ACTION="read")

      CALL ioname('space',filename,LPATH=.FALSE.)
      WRITE( stdout,"(2x,'Wannier data read from file: ',a)") TRIM(filename)


      !
      ! Print data to output
      !
      CALL summary( stdout, LINPUT=.FALSE., LATOMS=.FALSE., LEIG=.FALSE. )

      !
      ! setup real space quantities (such as those used to move the hamiltonian 
      ! to WFs basis)
      !
      CALL hamiltonian_init()


      !
      ! few local allocations
      !
      ALLOCATE( opr_in(nbnd,nbnd,nkpts), STAT=ierr )
         IF (ierr/=0) CALL errore('blc2wan','allocating opr_in',ABS(ierr))
      ALLOCATE( oprk(dimwann,dimwann,nkpts), STAT=ierr )
         IF (ierr/=0) CALL errore('blc2wan','allocating oprk',ABS(ierr))
      ALLOCATE( opr_out(dimwann,dimwann,nws), STAT=ierr )
         IF (ierr/=0) CALL errore('blc2wan','allocating opr_out',ABS(ierr))
      ALLOCATE( cutot(dimwinx,dimwann,nkpts), STAT=ierr )
         IF (ierr/=0) CALL errore('blc2wan','allocating opr_out',ABS(ierr))
      ALLOCATE( aux(nbnd), STAT=ierr )
         IF (ierr/=0) CALL errore('blc2wan','allocating aux',ABS(ierr))
      ALLOCATE( work(dimwinx,dimwinx), STAT=ierr )
         IF (ierr/=0) CALL errore('blc2wan','allocating work',ABS(ierr))
      ALLOCATE( kpt(3,nkpts), STAT=ierr )
         IF (ierr/=0) CALL errore('blc2wan','allocating kpt',ABS(ierr))
      ALLOCATE( phase(nkpts,nws), STAT=ierr )
         IF (ierr/=0) CALL errore('blc2wan','allocating phase',ABS(ierr))
      ALLOCATE( norms(nws), STAT=ierr )     
         IF (ierr/=0) CALL errore('blc2wan','allocating norms',ABS(ierr))
      ALLOCATE( vwss(nws), STAT=ierr )     
         IF (ierr/=0) CALL errore('blc2wan','allocating vwss',ABS(ierr))


!
! ... Init operator data from filein
!     filein is left opened as well as fileout
!
      filename=TRIM(work_dir)//"/"//TRIM(filein)
      CALL iotk_open_read(in_unit, TRIM(filename), IERR=ierr)
         IF (ierr/=0)  CALL errore('blc2wan','opening '//TRIM(filename),ABS(ierr))
      !
      ! reading main info
      !
      CALL iotk_scan_empty(in_unit,"DATA",ATTR=attr,IERR=ierr)
         IF (ierr/=0)  CALL errore('blc2wan','searching data',ABS(ierr))
      CALL iotk_scan_attr(attr,'nbnd',nbnd_,IERR=ierr)
         IF (ierr/=0)  CALL errore('blc2wan','searching nbnd_',ABS(ierr))
      CALL iotk_scan_attr(attr,'nkpts',nkpts_,IERR=ierr)
         IF (ierr/=0)  CALL errore('blc2wan','searching nkpts_',ABS(ierr))
      CALL iotk_scan_attr(attr,'lband_diag',lband_diag,IERR=ierr)
         IF (ierr/=0)  CALL errore('blc2wan','searching lband_diag',ABS(ierr))
      CALL iotk_scan_attr(attr,'ldynamical',ldynamical,IERR=ierr)
         IF (ierr/=0)  CALL errore('blc2wan','searching ldynamical',ABS(ierr))

      IF ( nbnd_ /= nbnd ) CALL errore('blc2wan','invalid nbnd',2)
      IF ( nkpts_ /= nkpts ) CALL errore('blc2wan','invalid nkpts',3)

      !
      ! read kpt list (units assumed in bohr^-1)
      !
      CALL iotk_scan_dat(in_unit, 'VKPT', kpt, IERR=ierr )
         IF (ierr/=0)  CALL errore('blc2wan','searching VKPT',ABS(ierr))


      nomega = 1
      IF ( ldynamical ) THEN
          CALL iotk_scan_attr(attr,'nomega',nomega,IERR=ierr)
             IF (ierr/=0)  CALL errore('blc2wan','searching nomega',ABS(ierr))

          ALLOCATE( grid(nomega), STAT=ierr )
             IF (ierr/=0)  CALL errore('blc2wan','allocating grid',ABS(ierr))
      
          CALL iotk_scan_dat(in_unit,"GRID",grid,IERR=ierr)
             IF (ierr/=0)  CALL errore('blc2wan','reading grid',ABS(ierr))
      ENDIF

      !
      ! fileout is initializated
      !
      filename=TRIM(work_dir)//"/"//TRIM(fileout)
      CALL iotk_open_write(out_unit, TRIM(filename), BINARY=.NOT. ascii)

      CALL iotk_write_attr(attr,"dimwann",dimwann,FIRST=.TRUE.)
      CALL iotk_write_attr(attr,"nws",nws)
      CALL iotk_write_attr(attr,"nomega",nomega)
      CALL iotk_write_empty(out_unit,"DATA",ATTR=attr)
      CALL iotk_write_dat(out_unit,"VWS",vws, COLUMNS=3)
      IF ( ldynamical ) CALL iotk_write_dat(out_unit,"GRID",grid)

           
!
! ... Main task 
!
      WRITE( stdout, "(/,2x,70('='))" )
      WRITE( stdout, "(2x,'=',16x,'Conversion to Wannier Function basis',16x,'=')" )
      WRITE( stdout, "(2x,70('='),/)" )


      !
      ! set the phase factors
      !
      DO iws = 1, nws
          DO ik=1,nkpts
              arg = DOT_PRODUCT( kpt(:,ik), vws(:,iws) )
              phase(ik,iws) = CMPLX( COS(arg), -SIN(arg) )
          ENDDO
          !
          ! compute also the norms of the vws vectors
          !
          vwss(iws) = SQRT( DOT_PRODUCT(vws(:,iws), vws(:,iws)) ) 
      ENDDO


      !
      ! set the full rotation in k-space
      !
      DO ik=1,nkpts
          cutot(:,:,ik) = CZERO
          CALL zmat_mul( cutot(:,:,ik), eamp(:,:,ik), 'N', cu(:,:,ik), 'N' ,  &
                         dimwin(ik), dimwann, dimwann)
      ENDDO

  
      !
      ! set the measure of the localization
      ! in real space to zero
      norms(:) = ZERO


      !
      ! loop over frequencies
      !
      energies: &
      DO ie=1,nomega
         
          IF ( ldynamical ) THEN
             name="OPR"//TRIM(iotk_index(ie))
          ELSE
             !
             ! in this case just the static operator is searched
             name="OPR"
          ENDIF

          CALL iotk_scan_begin(in_unit, TRIM(name), IERR=ierr)
               IF (ierr/=0) CALL errore('blc2wan','searching for '//TRIM(name),ABS(ierr))
          CALL iotk_write_begin(out_unit, TRIM(name), IERR=ierr)
               IF (ierr/=0) CALL errore('blc2wan','writing for '//TRIM(name),ABS(ierr))


          kpoints: &
          DO ik =1, nkpts
               
               !
               ! reading data from filein
               !
               IF ( lband_diag ) THEN 

                   CALL iotk_scan_dat(in_unit, "KPT"//TRIM(iotk_index(ik)), aux(:), &
                                      IERR=ierr)
                   IF (ierr/=0) CALL errore('blc2wan','reading diag KPT' ,ik)
                   !
                   opr_in(:,:,ik) = CZERO
                   DO m=1,nbnd
                       opr_in(m,m,ik) = aux(m)
                   ENDDO
               ELSE
                   CALL iotk_scan_dat(in_unit,"KPT"//TRIM(iotk_index(ik)),  &
                                      opr_in(:,:,ik), IERR=ierr)
                   IF (ierr/=0) CALL errore('blc2wan','reading full KPT' ,ik)
               ENDIF

               !
               ! converting data to oprk(ik) = cutot^dag(ik) * opr_in(ik) * cutot(ik)
               !
               CALL zmat_mul( work, opr_in(imin(ik):imax(ik),imin(ik):imax(ik),ik), 'N', &
                              cutot(:,:,ik), 'N', dimwin(ik), dimwann, dimwin(ik) )
               CALL zmat_mul( oprk(:,:,ik), cutot(:,:,ik), 'C', &
                              work(:,:), 'N', dimwann, dimwann, dimwin(ik) )

               !
               ! eventually this writing call may be eliminated
               !
               CALL iotk_write_dat(out_unit,"KPT"//TRIM(iotk_index(ik)), oprk(:,:,ik))
          ENDDO kpoints


          !
          ! final Fourier transform
          !
          rlattice: &
          DO iws =1, nws
               
               opr_out(:,:,iws) = CZERO
               DO ik = 1, nkpts
                    DO j=1,dimwann
                    DO i=1,dimwann
                         opr_out(i,j,iws) = opr_out(i,j,iws) + phase(ik,iws) * oprk(i,j,ik) 
                    ENDDO
                    ENDDO
               ENDDO
               opr_out(:,:,iws) = opr_out(:,:,iws) / REAL(nkpts)

               !
               ! write to file
               !
               CALL iotk_write_dat(out_unit,"WS"//TRIM(iotk_index(iws)), opr_out(:,:,iws))

               !
               ! add the contribution to the localization measure
               !
               DO j=1,dimwann
               DO i=1,dimwann
                   norms(iws) = norms(iws) + &
                                REAL(  CONJG( opr_out(i,j,iws)) * opr_out(i,j,iws) )
               ENDDO
               ENDDO

          ENDDO rlattice

          !
          ! ending the sections
          !
          CALL iotk_scan_end(in_unit, TRIM(name), IERR=ierr)
               IF (ierr/=0) CALL errore('blc2wan','ending '//TRIM(name),ABS(ierr))
          CALL iotk_write_end(out_unit, TRIM(name), IERR=ierr)
               IF (ierr/=0) CALL errore('blc2wan','writing end '//TRIM(name),ABS(ierr))

      ENDDO energies


      !
      ! close files
      !
      CALL iotk_close_read(in_unit, IERR=ierr)
           IF (ierr/=0) CALL errore('blc2wan','closing IN_UNIT',ABS(ierr))
      CALL iotk_close_write(out_unit)
      !



!
! ... print the localization measure 
!
      !
      WRITE(stdout,"(/,2x,'Real space decay of OPR:',/)")
      WRITE(stdout,"(  5x,'R [cry]     |R| [Bohr]      Norm of Opr(R) [eV]')")
      !
      DO iws = 1, nws
          WRITE(stdout,"(1x,3i4,3x,f11.7,4x,f15.9)") &
                         indxws(:,iws), vwss(iws), SQRT(  norms(iws) / REAL(dimwann*nomega) )
      ENDDO
      


!
! ... Shutdown
!

      !
      ! Finalize timing
      !
      CALL timing('blc2wan',OPR='stop')
      CALL timing_overview(stdout,LIST=global_list,MAIN_NAME='blc2wan')

      !
      ! Clean local memory
      !
      DEALLOCATE( opr_in, opr_out, oprk, STAT=ierr)
         IF( ierr /=0 ) CALL errore('blc2wan', 'deallocating opr_in -- opr_out', ABS(ierr) )
      DEALLOCATE( cutot, STAT=ierr)
         IF( ierr /=0 ) CALL errore('blc2wan', 'deallocating cutot', ABS(ierr) )
      DEALLOCATE( aux, work, STAT=ierr)
         IF( ierr /=0 ) CALL errore('blc2wan', 'deallocating aux, work', ABS(ierr) )
      DEALLOCATE( phase, kpt, STAT=ierr)
         IF( ierr /=0 ) CALL errore('blc2wan', 'deallocating phase, kpt', ABS(ierr) )
      DEALLOCATE( vwss, norms, STAT=ierr)
         IF( ierr /=0 ) CALL errore('blc2wan', 'deallocating vwss, norms', ABS(ierr) )
      IF ( ALLOCATED(grid) ) THEN 
           DEALLOCATE( grid, STAT=ierr)
           IF( ierr /=0 ) CALL errore('blc2wan', 'deallocating grid', ABS(ierr) )
      ENDIF

      !
      ! Clean global memory
      !
      CALL cleanup()

END PROGRAM blc2wan






