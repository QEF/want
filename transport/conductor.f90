!
!      Copyright (C) 2004 WanT Group
!      Copyright (C) 1999 Marco Buongiorno Nardelli
!
!      This file is distributed under the terms of the
!      GNU General Public License. See the file `License'
!      in the root directory of the present distribution,
!      or http://www.gnu.org/copyleft/gpl.txt .
!
!***********************************************
   PROGRAM conductor
   !***********************************************
   USE kinds
   USE constants,        ONLY : PI, ZERO, CZERO, CONE, CI, EPS_m5, EPS_m2
   USE parameters,       ONLY : nstrx 
   USE startup_module,   ONLY : startup
   USE version_module,   ONLY : version_number
   USE cleanup_module,   ONLY : cleanup
   USE io_module,        ONLY : stdout, stdin, sgm_unit => aux_unit,   &
                                dos_unit => aux1_unit, cond_unit => aux2_unit
   USE parser_module,    ONLY : change_case
   USE iotk_module
   USE files_module,     ONLY : file_open, file_close
   USE timing_module,    ONLY : timing, timing_deallocate, timing_overview, global_list
   IMPLICIT NONE


      INTEGER      :: nmxa, nmxb, nmxc 
      INTEGER      :: nmaxa, nmaxb, nmaxc
      INTEGER      :: niterx
      INTEGER      :: ne
      REAL(dbl)    :: emin, emax
      REAL(dbl)    :: bias, delta
      LOGICAL      :: loverlap, lcorrelation
      CHARACTER(20):: conduct_formula             !( "landauer"  | "generalized" )
      CHARACTER(20):: calculation_type            !( "conductor" | "bulk" )
      !
      CHARACTER(nstrx) :: attr, sgmtag, sgmfile, filename
      REAL(dbl)    :: rtmp
      COMPLEX(dbl) :: ene
      INTEGER      :: dimwann_, nws_, nomega_
      INTEGER      :: i, j, ie, iws, info
      INTEGER      :: ierr
      
      REAL(dbl),    ALLOCATABLE :: vws(:,:)
      REAL(dbl),    ALLOCATABLE :: egrid(:)
      REAL(dbl),    ALLOCATABLE :: dos(:,:), conduct(:,:)

      INTEGER,      ALLOCATABLE :: ipiv2(:)
      COMPLEX(dbl), ALLOCATABLE :: h00_a(:,:), h01_a(:,:)
      COMPLEX(dbl), ALLOCATABLE :: h00_b(:,:), h01_b(:,:)
      COMPLEX(dbl), ALLOCATABLE :: h00_c(:,:)
      COMPLEX(dbl), ALLOCATABLE :: hci_ac(:,:), hci_ca(:,:)
      COMPLEX(dbl), ALLOCATABLE :: hci_cb(:,:), hci_bc(:,:)
      ! 
      COMPLEX(dbl), ALLOCATABLE :: s00_a(:,:), s01_a(:,:) 
      COMPLEX(dbl), ALLOCATABLE :: s00_b(:,:), s01_b(:,:)
      COMPLEX(dbl), ALLOCATABLE :: s00_c(:,:)
      COMPLEX(dbl), ALLOCATABLE :: sci_ac(:,:), sci_ca(:,:)
      COMPLEX(dbl), ALLOCATABLE :: sci_cb(:,:), sci_bc(:,:)
      !
      COMPLEX(dbl), ALLOCATABLE :: c00_a(:,:), c01_a(:,:)
      COMPLEX(dbl), ALLOCATABLE :: c00_b(:,:), c01_b(:,:)
      COMPLEX(dbl), ALLOCATABLE :: c00_c(:,:)
      COMPLEX(dbl), ALLOCATABLE :: cci_ac(:,:), cci_ca(:,:)
      COMPLEX(dbl), ALLOCATABLE :: cci_cb(:,:), cci_bc(:,:)
      !
      COMPLEX(dbl), ALLOCATABLE :: totA(:,:)
      COMPLEX(dbl), ALLOCATABLE :: totB(:,:)
      COMPLEX(dbl), ALLOCATABLE :: tottA(:,:)
      COMPLEX(dbl), ALLOCATABLE :: tottB(:,:)
      COMPLEX(dbl), ALLOCATABLE :: gR(:,:)
      COMPLEX(dbl), ALLOCATABLE :: gL(:,:)
      COMPLEX(dbl), ALLOCATABLE :: gA(:,:)
      COMPLEX(dbl), ALLOCATABLE :: gB(:,:)
      COMPLEX(dbl), ALLOCATABLE :: gintr(:,:)
      COMPLEX(dbl), ALLOCATABLE :: aux(:,:), aux1(:,:), aux2(:,:)
      COMPLEX(dbl), ALLOCATABLE :: sLr(:,:)
      COMPLEX(dbl), ALLOCATABLE :: sRr(:,:)
      COMPLEX(dbl), ALLOCATABLE :: s1(:,:)
      COMPLEX(dbl), ALLOCATABLE :: s2(:,:)
      COMPLEX(dbl), ALLOCATABLE :: c1(:,:)
      COMPLEX(dbl), ALLOCATABLE :: c2(:,:)
      COMPLEX(dbl), ALLOCATABLE :: tran(:,:)
      COMPLEX(dbl), ALLOCATABLE :: sgm_r(:,:)
     
      !
      ! manelist definition
      !
      NAMELIST /INPUT_CONDUCTOR/ nmxa, nmxb, nmxc, ne, niterx, emin, emax, delta, bias, &
                                 calculation_type, loverlap, &
                                 lcorrelation, conduct_formula, sgmfile


!
!------------------------------
! main body
!------------------------------
!
      CALL startup(version_number,MAIN_NAME='conductor')


!
!...  Read namelist
!
      nmxa = 0
      nmxb = 0
      nmxc = 0
      calculation_type = "conductor"
      ne = 1000
      niterx = 200
      emin = -10.0
      emax =  10.0
      delta = EPS_m5
      bias = 0.0
      loverlap = .FALSE.
      lcorrelation = .FALSE.
      conduct_formula = 'landauer'
      sgmfile = ' '

      READ( 5, input_conductor, IOSTAT=ierr)
      IF ( ierr/= 0) CALL errore('conductor','reading input namelist',ABS(ierr))
      
      IF ( nmxc <= 0) CALL errore('conductor','Invalid NMXC',1)
      IF ( emax <= emin ) CALL errore('conductor','Invalid EMIN EMAX',1)
      IF ( ne <= 1 ) CALL errore('conductor','Invalid NE',1)
      IF ( niterx <= 0 ) CALL errore('conductor','Invalid NITERX',1)
      IF ( delta < 0.0 ) CALL errore('conductor','Invalid DELTA',1)
      IF ( delta > EPS_m2 ) CALL errore('conductor','DELTA too large',1)

      CALL change_case(calculation_type, 'lower')
      IF ( TRIM(calculation_type) /= 'conductor' .AND. &
           TRIM(calculation_type) /= 'bulk' ) &
           CALL errore('conductor','invalid CALCULATION_TYPE = '//TRIM(calculation_type),1)
           !
      IF ( TRIM(calculation_type) == 'conductor' ) THEN
           IF ( nmxa <= 0) CALL errore('conductor','Invalid NMXA',1)
           IF ( nmxb <= 0) CALL errore('conductor','Invalid NMXB',1)
      ELSE
           !
           ! bulk case
           !
           IF ( nmxa /= 0) CALL errore('conductor','NMXA should not be specified',1)
           IF ( nmxb /= 0) CALL errore('conductor','NMXB should not be specified',1)
           nmxa = nmxc
           nmxb = nmxc
      ENDIF
      !
      CALL change_case(conduct_formula, 'lower')
      IF ( TRIM(conduct_formula) /='landauer' .AND. TRIM(conduct_formula) /= 'generalized')&
           CALL errore('conductor','invalid CONDUCT_FORMULA = '//TRIM(conduct_formula),1)
           !
      IF ( TRIM(conduct_formula) /= 'landauer' .AND. .NOT. lcorrelation ) &
           CALL errore('conductor','invalid conduct formula',1)
           !
      IF ( lcorrelation .AND. LEN_TRIM(sgmfile) == 0 ) &
           CALL errore('conductor','sgmfile unspecified',1)

      nmaxa=nmxa
      nmaxb=nmxb
      nmaxc=nmxc


!
! ... local variables
!
      ALLOCATE ( ipiv2(nmaxc), STAT=ierr )
           IF( ierr /=0 ) CALL errore('conductor', ' allocating ipiv2 ', 1 ) 

      ALLOCATE ( h00_a(nmaxa,nmaxa), STAT=ierr )
           IF( ierr /=0 ) CALL errore('conductor', ' allocating h00_a ', 1 )
      ALLOCATE ( h01_a(nmaxa,nmaxa), STAT=ierr )
           IF( ierr /=0 ) CALL errore('conductor', ' allocating h01_a ', 1 )
      ALLOCATE ( h00_b(nmaxb,nmaxb), STAT=ierr )
           IF( ierr /=0 ) CALL errore('conductor', ' allocating h00_b ', 1 )
      ALLOCATE ( h01_b(nmaxb,nmaxb), STAT=ierr )
           IF( ierr /=0 ) CALL errore('conductor', ' allocating h01_b ', 1 )
      ALLOCATE ( h00_c(nmaxc,nmaxc), STAT=ierr )
           IF( ierr /=0 ) CALL errore('conductor', ' allocating h00_c ', 1 )
      ALLOCATE ( hci_ac(nmaxa,nmaxc), STAT=ierr )
           IF( ierr /=0 ) CALL errore('conductor', ' allocating hci_ac ', 1 )
      ALLOCATE ( hci_cb(nmaxc,nmaxb), STAT=ierr )
           IF( ierr /=0 ) CALL errore('conductor', ' allocating hac_cb ', 1 )

      ALLOCATE ( hci_ca(nmaxc,nmaxa), STAT=ierr )
           IF( ierr /=0 ) CALL errore('conductor', ' allocating hci_ca ', 1 )
      ALLOCATE ( hci_bc(nmaxb,nmaxc), STAT=ierr )
           IF( ierr /=0 ) CALL errore('conductor', ' allocating hci_bc ', 1 )

      ALLOCATE ( s00_a(nmaxa,nmaxa), STAT=ierr )
           IF( ierr /=0 ) CALL errore('conductor', ' allocating s00_a ', 1 )
      ALLOCATE ( s01_a(nmaxa,nmaxa), STAT=ierr )
           IF( ierr /=0 ) CALL errore('conductor', ' allocating s01_a ', 1 )
      ALLOCATE ( s00_b(nmaxb,nmaxb), STAT=ierr )
           IF( ierr /=0 ) CALL errore('conductor', ' allocating s00_b ', 1 )
      ALLOCATE ( s01_b(nmaxb,nmaxb), STAT=ierr )
           IF( ierr /=0 ) CALL errore('conductor', ' allocating s01_b ', 1 )
      ALLOCATE ( s00_c(nmaxc,nmaxc), STAT=ierr )
           IF( ierr /=0 ) CALL errore('conductor', ' allocating s00_c ', 1 )
      ALLOCATE ( sci_ac(nmaxa,nmaxc), STAT=ierr )
           IF( ierr /=0 ) CALL errore('conductor', ' allocating sci_ac ', 1 )
      ALLOCATE ( sci_cb(nmaxc,nmaxb), STAT=ierr )
           IF( ierr /=0 ) CALL errore('conductor', ' allocating sci_cb ', 1 )

      ALLOCATE ( sci_ca(nmaxc,nmaxa), STAT=ierr) 
           IF( ierr /=0 ) CALL errore('conductor', ' allocating sci_ca ', 1 )
      ALLOCATE ( sci_bc(nmaxb,nmaxc), STAT=ierr )
           IF( ierr /=0 ) CALL errore('conductor', ' allocating sci_bc ', 1 )

      ALLOCATE ( c00_a(nmaxa,nmaxa), STAT=ierr )
           IF( ierr /=0 ) CALL errore('conductor', ' allocating c00_a ', 1 )
      ALLOCATE ( c01_a(nmaxa,nmaxa), STAT=ierr )
           IF( ierr /=0 ) CALL errore('conductor', ' allocating c01_a ', 1 )
      ALLOCATE ( c00_b(nmaxb,nmaxb), STAT=ierr )
           IF( ierr /=0 ) CALL errore('conductor', ' allocating c00_b ', 1 )
      ALLOCATE ( c01_b(nmaxb,nmaxb), STAT=ierr )
           IF( ierr /=0 ) CALL errore('conductor', ' allocating c01_b ', 1 )
      ALLOCATE ( c00_c(nmaxc,nmaxc), STAT=ierr )
           IF( ierr /=0 ) CALL errore('conductor', ' allocating c00_c ', 1 )
      ALLOCATE ( cci_ac(nmaxa,nmaxc), STAT=ierr )
           IF( ierr /=0 ) CALL errore('conductor', ' allocating cci_ac ', 1 )
      ALLOCATE ( cci_cb(nmaxc,nmaxb), STAT=ierr )
           IF( ierr /=0 ) CALL errore('conductor', ' allocating cci_cb ', 1 )
      ALLOCATE ( cci_ca(nmaxc,nmaxa), STAT=ierr )
           IF( ierr /=0 ) CALL errore('conductor', ' allocating cci_ca ', 1 )
      ALLOCATE ( cci_bc(nmaxb,nmaxc), STAT=ierr )
           IF( ierr /=0 ) CALL errore('conductor', ' allocating cci_bc ', 1 )

      ALLOCATE ( totA(nmaxa,nmaxa), STAT=ierr )
           IF( ierr /=0 ) CALL errore('conductor', ' allocating totA ', 1 )
      ALLOCATE ( totB(nmaxb,nmaxb), STAT=ierr )
           IF( ierr /=0 ) CALL errore('conductor', ' allocating totB ', 1 )
      ALLOCATE ( tottA(nmaxa,nmaxa), STAT=ierr )
           IF( ierr /=0 ) CALL errore('conductor', ' allocating tottA ', 1 )
      ALLOCATE ( tottB(nmaxb,nmaxb), STAT=ierr )
           IF( ierr /=0 ) CALL errore('conductor', ' allocating tottB ', 1 )

      ALLOCATE ( gR(nmaxc,nmaxc), STAT=ierr )
           IF( ierr /=0 ) CALL errore('conductor', ' allocating gR ', 1 )
      ALLOCATE ( gL(nmaxc,nmaxc), STAT=ierr )
           IF( ierr /=0 ) CALL errore('conductor', ' allocating gL ', 1 )
      ALLOCATE ( gA(nmaxa,nmaxa), STAT=ierr )
           IF( ierr /=0 ) CALL errore('conductor', ' allocating gA ', 1 )
      ALLOCATE ( gB(nmaxb,nmaxb), STAT=ierr )
           IF( ierr /=0 ) CALL errore('conductor', ' allocating gB ', 1 )

      ALLOCATE ( gintr(nmaxc,nmaxc), STAT=ierr )
           IF( ierr /=0 ) CALL errore('conductor', ' allocating gintr ', 1 )
      ALLOCATE ( aux(nmaxc,nmaxc), aux1(nmaxc,nmaxc), aux2(nmaxc,nmaxc), STAT=ierr )
           IF( ierr /=0 ) CALL errore('conductor', ' allocating aux,aux1,aux2 ', 1 )
      ALLOCATE ( sLr(nmaxc,nmaxc), STAT=ierr )
           IF( ierr /=0 ) CALL errore('conductor', ' allocating sLr ', 1 )
      ALLOCATE ( sRr(nmaxc,nmaxc), STAT=ierr )
           IF( ierr /=0 ) CALL errore('conductor', ' allocating sRr ', 1 )
      ALLOCATE ( s1(nmaxc,nmaxc), STAT=ierr )
           IF( ierr /=0 ) CALL errore('conductor', ' allocating s1 ', 1 )
      ALLOCATE ( s2(nmaxc,nmaxc), STAT=ierr )
           IF( ierr /=0 ) CALL errore('conductor', ' allocating s2 ', 1 )
      ALLOCATE ( c1(nmaxc,nmaxa), STAT=ierr )
           IF( ierr /=0 ) CALL errore('conductor', ' allocating c1 ', 1 )
      ALLOCATE ( c2(nmaxc,nmaxb), STAT=ierr )
           IF( ierr /=0 ) CALL errore('conductor', ' allocating c2 ', 1 )
      ALLOCATE ( tran(nmaxc,nmaxc), STAT=ierr )
           IF( ierr /=0 ) CALL errore('conductor', ' allocating tran ', 1 )

      ALLOCATE ( dos(nmaxc,ne), STAT=ierr )
           IF( ierr /=0 ) CALL errore('conductor', ' allocating dos ', 1 )
      ALLOCATE ( conduct(nmaxc,ne), STAT=ierr )
           IF( ierr /=0 ) CALL errore('conductor', ' allocating conduct ', 1 )
      ALLOCATE ( sgm_r(nmaxc,nmaxc), STAT=ierr )
           IF( ierr /=0 ) CALL errore('conductor', ' allocating sgm_r ', 1 )
      ALLOCATE ( egrid(ne), STAT=ierr )
           IF( ierr /=0 ) CALL errore('conductor', ' allocating egrid ', 1 )

!
!...  Set up the layer hamiltonians
!     different layers in different files - read only version

      CALL readH( nmaxa, nmaxb, nmaxc, loverlap, calculation_type, &
                  h00_a, h01_a, s00_a, s01_a, h00_b, h01_b, s00_b, &
                  s01_b, h00_c, s00_c, hci_ac, sci_ac, hci_cb, sci_cb)

      hci_ca(:,:) = CONJG( TRANSPOSE(hci_ac(:,:)) )
      sci_ca(:,:) = CONJG( TRANSPOSE(sci_ac(:,:)) )
      hci_bc(:,:) = CONJG( TRANSPOSE(hci_cb(:,:)) )
      sci_bc(:,:) = CONJG( TRANSPOSE(sci_cb(:,:)) )

!
! ... get the correlation self-energy if the case
!     (at the end leave the file opened)
!
      IF ( lcorrelation ) THEN
           CALL file_open( sgm_unit, TRIM(sgmfile), PATH="/", ACTION="read", FORM="formatted")

           CALL iotk_scan_empty(sgm_unit, "DATA", ATTR=attr, IERR=ierr)
              IF (ierr/=0) CALL errore('conductor','searching DATA',ABS(ierr))
           CALL iotk_scan_attr(attr,"dimwann",dimwann_, IERR=ierr)
              IF (ierr/=0) CALL errore('conductor','searching DIMWANN',ABS(ierr))
           CALL iotk_scan_attr(attr,"nws",nws_, IERR=ierr)
              IF (ierr/=0) CALL errore('conductor','searching nws_',ABS(ierr))
           CALL iotk_scan_attr(attr,"nomega",nomega_, IERR=ierr)
              IF (ierr/=0) CALL errore('conductor','searching NOMEGA_',ABS(ierr))

           IF (nomega_ /= ne) CALL errore('conductor','invalid nomega from SGM',2)
           IF (dimwann_ /= nmaxc) CALL errore('conductor','invalid dimwann from SGM',2)
           IF (nws_  <= 0 ) CALL errore('conductor','invalid nws from SGM',2)
 
           !
           ! real space lattice vector
           !
           ALLOCATE( vws(3,nws_), STAT=ierr )
              IF (ierr/=0) CALL errore('conductor','allocating vws',ABS(ierr))
           CALL iotk_scan_dat(sgm_unit, "VWS",  vws, IERR=ierr)
              IF (ierr/=0) CALL errore('conductor','searching VWS',ABS(ierr))
           
           !
           ! chose the R=0 vector
           !
           DO iws = 1, nws_
                IF ( DOT_PRODUCT(vws(:,iws), vws(:,iws))  < EPS_m5 ) THEN 
                   sgmtag = "WS"//TRIM(iotk_index(iws))
                   EXIT
                ENDIF
           ENDDO
         
           DEALLOCATE( vws, STAT=ierr )
              IF (ierr/=0) CALL errore('conductor','deallocating vws',ABS(ierr))

           !
           ! read the energy grid
           !
           CALL iotk_scan_dat(sgm_unit, "GRID",  egrid, IERR=ierr)
              IF (ierr/=0) CALL errore('conductor','searching GRID',ABS(ierr))
           
      ENDIF 

!
! ... setting the energy grid
!
      DO ie = 1, ne
          rtmp = emin + REAL( ie -1) * (emax - emin) / REAL(ne -1)
          IF ( lcorrelation ) THEN
               IF ( ABS( egrid(ie ) - rtmp) > EPS_m5 ) &
                    CALL errore('conductor', 'grids not equal',ie)
          ELSE
               egrid(ie) = rtmp
          ENDIF
      ENDDO


!
! ... main loop over frequency
! 

      energies: &
      DO ie = 1, ne

         !
         ! compute Retarded quantities
         !
         ene =  egrid(ie)  + delta * CI

         !
         ! init
         !
         c00_a(:,:)  = h00_a(:,:)  -ene * s00_a(:,:)
         c01_a(:,:)  = h01_a(:,:)  -ene * s01_a(:,:)
         !
         c00_b(:,:)  = h00_b(:,:)  -ene * s00_b(:,:)
         c01_b(:,:)  = h01_b(:,:)  -ene * s01_b(:,:)
         !
         c00_c(:,:)  = h00_c(:,:)  -ene * s00_c(:,:)
         !
         cci_ac(:,:) = hci_ac(:,:) -ene * sci_ac(:,:) 
         cci_bc(:,:) = hci_bc(:,:) -ene * sci_bc(:,:) 
         !
         cci_ca(:,:) = hci_ca(:,:) -ene * sci_ca(:,:)
         cci_cb(:,:) = hci_cb(:,:) -ene * sci_cb(:,:)


         !
         ! get correlaiton self-energy if the case
         !
         IF ( lcorrelation ) THEN
             CALL iotk_scan_begin(sgm_unit, "OPR"//TRIM(iotk_index(ie)), IERR=ierr)
                 IF (ierr/=0) CALL errore('conductor','searching for OPR',ie)
             CALL iotk_scan_dat(sgm_unit, TRIM(sgmtag), sgm_r, IERR=ierr)
                 IF (ierr/=0) CALL errore('conductor','searching for '//TRIM(sgmtag),ie)
             CALL iotk_scan_end(sgm_unit, "OPR"//TRIM(iotk_index(ie)), IERR=ierr)
                 IF (ierr/=0) CALL errore('conductor','searching end for OPR',ie)
         ELSE
             sgm_r(:,:) = CZERO
         ENDIF
         

         ! 
         ! construct leads self-energies 
         ! 
! ene + bias
         CALL transfer( nmaxb, niterx, totB, tottB, c00_b, c01_b )
         CALL green( nmaxb, totB, tottB, c00_b, c01_b, ene+bias, gB, 1, 1 )

         CALL zgemm('N','N', nmaxc, nmaxb, nmaxb, CONE, cci_cb, nmaxc, gB, nmaxb, &
                             CZERO, c2, nmaxc )
         CALL zgemm('N','N', nmaxc, nmaxc, nmaxb, CONE, c2, nmaxc, cci_bc, nmaxb, &
                             CZERO, sRr, nmaxc )

! ene
         CALL transfer( nmaxa, niterx, totA, tottA, c00_a, c01_a )
         CALL green( nmaxa, totA, tottA, c00_a, c01_a, ene, gA, -1, 1 )

         CALL zgemm('N','N', nmaxc, nmaxa, nmaxa, CONE, cci_ca, nmaxc, gA, nmaxa, &
                             CZERO, c1, nmaxc )
         CALL zgemm('N','N', nmaxc, nmaxc, nmaxa, CONE, c1, nmaxc, cci_ac, nmaxa, &
                             CZERO, sLr, nmaxc )

         !
         ! gL and gR
         !
         gL(:,:) = CI * (  sLr(:,:) - CONJG( TRANSPOSE(sLr(:,:)) )   )
         gR(:,:) = CI * (  sRr(:,:) - CONJG( TRANSPOSE(sRr(:,:)) )   )

         !
         ! Construct the conductor green's function
         ! G_C (retarded)
         !
         aux(:,:) = -c00_c(:,:) -sLr(:,:) -sRr(:,:) -sgm_r(:,:)
 
         gintr(:,:) = CZERO
         DO i = 1, nmaxc
            gintr(i,i)= CONE
         ENDDO

         CALL ZGESV( nmaxc, nmaxc, aux, nmaxc, ipiv2, gintr, nmaxc, info )
            IF ( info /= 0 )  CALL errore('conductor', ' ZGESV (I) ', info )

         !
         ! Compute density of states for the conductor layer
         !
         DO i = 1, nmaxc
            dos(i,ie) = -AIMAG( gintr(i,i) ) / PI
         ENDDO

         !
         ! evaluate the transmittance according to the Fisher-Lee formula
         ! or (in the correlated case) to the generalized expression as 
         ! from PRL 94, 116802 (2005)
         !
         CALL transmittance(nmaxc, gL, gR, gintr, sgm_r, TRIM(conduct_formula),  &
                            conduct(1,ie) )

      ENDDO energies

      !
      ! close sgm file
      !
      IF ( lcorrelation ) CALL file_close(sgm_unit, PATH="/", ACTION="read")


!
! ... write DOS and CONDUCT data on files
!

      filename = 'cond.dat'
      OPEN ( cond_unit, FILE=TRIM(filename), FORM='formatted' )
      DO ie = 1, ne
          WRITE ( cond_unit, '(2(f15.9))' ) egrid(ie), SUM( conduct(:,ie) )
      ENDDO
      CLOSE( cond_unit )

      filename = 'dos.dat'
      OPEN ( dos_unit, FILE=TRIM(filename), FORM='formatted' )
      DO ie = 1, ne
          WRITE ( dos_unit, '(2(f15.9))' ) egrid(ie), SUM( dos(:,ie) )
      ENDDO
      CLOSE( dos_unit )
      

!
! ...  Finalize timing
!
      CALL timing('conductor',OPR='stop')
      CALL timing_overview(stdout,LIST=global_list,MAIN_NAME='conductor')


!
!...  free memory
!
      DEALLOCATE ( ipiv2, STAT=ierr )
           IF( ierr /=0 ) CALL errore('conductor', ' deallocating ipiv2 ', 1 )

      DEALLOCATE ( h00_a, h01_a, STAT=ierr )
           IF( ierr /=0 ) CALL errore('conductor', ' deallocating h00_a, h01_a ', 1 )
      DEALLOCATE ( h00_b, h01_b, STAT=ierr )
           IF( ierr /=0 ) CALL errore('conductor', ' deallocating h00_b, h01_b ', 1 )
      DEALLOCATE ( h00_c, STAT=ierr )
           IF( ierr /=0 ) CALL errore('conductor', ' deallocating h00_c ', 1 )
      DEALLOCATE ( hci_ac, hci_ca, STAT=ierr )
           IF( ierr /=0 ) CALL errore('conductor', ' deallocating hci_ac, hci_ca ', 1 )
      DEALLOCATE ( hci_cb, hci_bc, STAT=ierr )
           IF( ierr /=0 ) CALL errore('conductor', ' deallocating hac_cb, hci_bc ', 1 )

      DEALLOCATE ( s00_a, s01_a, STAT=ierr )
           IF( ierr /=0 ) CALL errore('conductor', ' deallocating s00_a, s01_a ', 1 )
      DEALLOCATE ( s00_b, s01_b, STAT=ierr )
           IF( ierr /=0 ) CALL errore('conductor', ' deallocating s00_b, s01_b ', 1 )
      DEALLOCATE ( s00_c, STAT=ierr )
           IF( ierr /=0 ) CALL errore('conductor', ' deallocating s00_c ', 1 )
      DEALLOCATE ( sci_ac, sci_ca, STAT=ierr )
           IF( ierr /=0 ) CALL errore('conductor', ' deallocating sci_ac, sci_ca ', 1 )
      DEALLOCATE ( sci_cb, sci_bc, STAT=ierr )
           IF( ierr /=0 ) CALL errore('conductor', ' deallocating sci_cb, sci_bc ', 1 )

      DEALLOCATE ( c00_a, c01_a, STAT=ierr )
           IF( ierr /=0 ) CALL errore('conductor', ' deallocating c00_a, c01_a ', 1 )
      DEALLOCATE ( c00_b, c01_b, STAT=ierr )
           IF( ierr /=0 ) CALL errore('conductor', ' deallocating c01_b, c01_b ', 1 )
      DEALLOCATE ( c00_c, STAT=ierr )
           IF( ierr /=0 ) CALL errore('conductor', ' deallocating c00_c ', 1 )
      DEALLOCATE ( cci_ac, cci_ca, STAT=ierr )
           IF( ierr /=0 ) CALL errore('conductor', ' deallocating cci_ac, cci_ca ', 1 )
      DEALLOCATE ( cci_cb, cci_bc, STAT=ierr )
           IF( ierr /=0 ) CALL errore('conductor', ' deallocating cci_cb, cci_bc ', 1 )

      DEALLOCATE ( totA, STAT=ierr )
           IF( ierr /=0 ) CALL errore('conductor', ' deallocating totA ', 1 )
      DEALLOCATE ( totB, STAT=ierr )
           IF( ierr /=0 ) CALL errore('conductor', ' deallocating totB ', 1 )
      DEALLOCATE ( tottA, STAT=ierr )
           IF( ierr /=0 ) CALL errore('conductor', ' deallocating tottA ', 1 )
      DEALLOCATE ( tottB, STAT=ierr )
           IF( ierr /=0 ) CALL errore('conductor', ' deallocating tottB ', 1 )

      DEALLOCATE ( gR, gL, STAT=ierr )
           IF( ierr /=0 ) CALL errore('conductor', ' deallocating gR, gL ', 1 )
      DEALLOCATE ( gA, gB, STAT=ierr )
           IF( ierr /=0 ) CALL errore('conductor', ' deallocating gA, gB ', 1 )

      DEALLOCATE ( gintr, STAT=ierr )
           IF( ierr /=0 ) CALL errore('conductor', ' deallocating gintr ', 1 )
      DEALLOCATE ( aux,aux1,aux2, STAT=ierr )
           IF( ierr /=0 ) CALL errore('conductor', ' deallocating aux,aux1,aux2 ', 1 )
      DEALLOCATE ( sRr, sLr, STAT=ierr )
           IF( ierr /=0 ) CALL errore('conductor', ' deallocating sRr, sLr ', 1 )
      DEALLOCATE ( s1, s2, STAT=ierr )
           IF( ierr /=0 ) CALL errore('conductor', ' deallocating s1, s2 ', 1 )
      DEALLOCATE ( c1, c2, STAT=ierr )
           IF( ierr /=0 ) CALL errore('conductor', ' deallocating c1, c2 ', 1 )
      DEALLOCATE ( tran, STAT=ierr )
           IF( ierr /=0 ) CALL errore('conductor', ' deallocating tran ', 1 )
      DEALLOCATE ( dos, STAT=ierr )
           IF( ierr /=0 ) CALL errore('conductor', ' deallocating dos ', 1 )
      DEALLOCATE ( conduct, STAT=ierr )
           IF( ierr /=0 ) CALL errore('conductor', ' deallocating conduct ', 1 )
      DEALLOCATE ( sgm_r, STAT=ierr )
           IF( ierr /=0 ) CALL errore('conductor', ' deallocating sgm_r ', 1 )
      DEALLOCATE ( egrid, STAT=ierr )
           IF( ierr /=0 ) CALL errore('conductor', ' deallocating egrid ', 1 )

      CALL cleanup()

      STOP '*** THE END *** (conductor.x)'
   END PROGRAM conductor
  
