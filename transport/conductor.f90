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
   USE constants,            ONLY : PI, ZERO, CZERO, CONE, CI, EPS_m5, EPS_m2
   USE parameters,           ONLY : nstrx 
   USE version_module,       ONLY : version_number
   USE io_module,            ONLY : stdout, stdin, sgm_unit => aux_unit,   &
                                    dos_unit => aux1_unit, cond_unit => aux2_unit
   USE T_control_module,     ONLY : use_overlap, use_correlation, calculation_type, &
                                    conduct_formula, niterx, bias, datafile_sgm 
   USE T_egrid_module,       ONLY : egrid_init, ne, egrid, delta
   USE T_kpoints_module,     ONLY : kpoints_init, nkpts_par , wk_par
   USE T_hamiltonian_module, ONLY : dimL, dimR, dimC,                  &
                                    h00_L, h01_L, h00_R, h01_R, h00_C, & 
                                    s00_L, s01_L, s00_R, s01_R, s00_c, &
                                    h_LC, h_CR, s_LC, s_CR
   USE iotk_module
   USE parser_module,    ONLY : change_case
   USE files_module,     ONLY : file_open, file_close
   USE timing_module,    ONLY : timing, timing_overview, global_list
   USE util_module,      ONLY : mat_mul, mat_sv
   USE T_input_module,   ONLY : input_manager

   IMPLICIT NONE

   !
   ! local variables
   !
   INTEGER          :: dimwann_, nws_, nomega_
   INTEGER          :: i, ie, iws, ik
   INTEGER          :: ierr
   COMPLEX(dbl)     :: ene
   CHARACTER(nstrx) :: attr, sgmtag, filename
   !   
   REAL(dbl),    ALLOCATABLE :: vws(:,:)
   REAL(dbl),    ALLOCATABLE :: dos(:,:), conduct(:,:)
   REAL(dbl),    ALLOCATABLE :: cond_aux(:)
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
   COMPLEX(dbl), ALLOCATABLE :: aux(:,:)
   COMPLEX(dbl), ALLOCATABLE :: sLr(:,:)
   COMPLEX(dbl), ALLOCATABLE :: sRr(:,:)
   COMPLEX(dbl), ALLOCATABLE :: c1(:,:)
   COMPLEX(dbl), ALLOCATABLE :: c2(:,:)
   COMPLEX(dbl), ALLOCATABLE :: sgm_r(:,:)
     

!
!------------------------------
! main body
!------------------------------
!
   CALL startup(version_number,'conductor')


!
! read input file
!
   CALL input_manager()
      

!
! init
!
   !
   ! energy grid
   !
   CALL egrid_init()

   !
   ! initialize kpoints and R vectors
   !
   CALL kpoints_init()

   !
   ! summarize the first part of the initializaton
   !
!   CALL summary(stdout)


!
! Set up the layer hamiltonians
!
   CALL hamiltonian_init( use_overlap, calculation_type )


!
! local variable allocations
!
   ALLOCATE ( c00_a(dimL,dimL), STAT=ierr )
      IF( ierr /=0 ) CALL errore('conductor', ' allocating c00_a ', 1 )
   ALLOCATE ( c01_a(dimL,dimL), STAT=ierr )
      IF( ierr /=0 ) CALL errore('conductor', ' allocating c01_a ', 1 )
   ALLOCATE ( c00_b(dimR,dimR), STAT=ierr )
      IF( ierr /=0 ) CALL errore('conductor', ' allocating c00_b ', 1 )
   ALLOCATE ( c01_b(dimR,dimR), STAT=ierr )
      IF( ierr /=0 ) CALL errore('conductor', ' allocating c01_b ', 1 )
   ALLOCATE ( c00_c(dimC,dimC), STAT=ierr )
      IF( ierr /=0 ) CALL errore('conductor', ' allocating c00_c ', 1 )
   ALLOCATE ( cci_ac(dimL,dimC), STAT=ierr )
      IF( ierr /=0 ) CALL errore('conductor', ' allocating cci_ac ', 1 )
   ALLOCATE ( cci_cb(dimC,dimR), STAT=ierr )
      IF( ierr /=0 ) CALL errore('conductor', ' allocating cci_cb ', 1 )
   ALLOCATE ( cci_ca(dimC,dimL), STAT=ierr )
      IF( ierr /=0 ) CALL errore('conductor', ' allocating cci_ca ', 1 )
   ALLOCATE ( cci_bc(dimR,dimC), STAT=ierr )
      IF( ierr /=0 ) CALL errore('conductor', ' allocating cci_bc ', 1 )

   ALLOCATE ( totA(dimL,dimL), STAT=ierr )
      IF( ierr /=0 ) CALL errore('conductor', ' allocating totA ', 1 )
   ALLOCATE ( totB(dimR,dimR), STAT=ierr )
      IF( ierr /=0 ) CALL errore('conductor', ' allocating totB ', 1 )
   ALLOCATE ( tottA(dimL,dimL), STAT=ierr )
      IF( ierr /=0 ) CALL errore('conductor', ' allocating tottA ', 1 )
   ALLOCATE ( tottB(dimR,dimR), STAT=ierr )
      IF( ierr /=0 ) CALL errore('conductor', ' allocating tottB ', 1 )

   ALLOCATE ( gR(dimC,dimC), STAT=ierr )
      IF( ierr /=0 ) CALL errore('conductor', ' allocating gR ', 1 )
   ALLOCATE ( gL(dimC,dimC), STAT=ierr )
      IF( ierr /=0 ) CALL errore('conductor', ' allocating gL ', 1 )
   ALLOCATE ( gA(dimL,dimL), STAT=ierr )
      IF( ierr /=0 ) CALL errore('conductor', ' allocating gA ', 1 )
   ALLOCATE ( gB(dimR,dimR), STAT=ierr )
      IF( ierr /=0 ) CALL errore('conductor', ' allocating gB ', 1 )

   ALLOCATE ( gintr(dimC,dimC), STAT=ierr )
      IF( ierr /=0 ) CALL errore('conductor', ' allocating gintr ', 1 )
   ALLOCATE ( aux(dimC,dimC), STAT=ierr )
      IF( ierr /=0 ) CALL errore('conductor', ' allocating aux', 1 )
   ALLOCATE ( sLr(dimC,dimC), STAT=ierr )
      IF( ierr /=0 ) CALL errore('conductor', ' allocating sLr ', 1 )
   ALLOCATE ( sRr(dimC,dimC), STAT=ierr )
      IF( ierr /=0 ) CALL errore('conductor', ' allocating sRr ', 1 )
   ALLOCATE ( c1(dimC,dimL), STAT=ierr )
      IF( ierr /=0 ) CALL errore('conductor', ' allocating c1 ', 1 )
   ALLOCATE ( c2(dimC,dimR), STAT=ierr )
      IF( ierr /=0 ) CALL errore('conductor', ' allocating c2 ', 1 )

   ALLOCATE ( dos(dimC,ne), STAT=ierr )
      IF( ierr /=0 ) CALL errore('conductor', ' allocating dos ', 1 )
   ALLOCATE ( conduct(dimC,ne), STAT=ierr )
      IF( ierr /=0 ) CALL errore('conductor', ' allocating conduct ', 1 )
   ALLOCATE ( cond_aux(dimC), STAT=ierr )
      IF( ierr /=0 ) CALL errore('conductor', ' allocating cond_aux ', 1 )
   ALLOCATE ( sgm_r(dimC,dimC), STAT=ierr )
      IF( ierr /=0 ) CALL errore('conductor', ' allocating sgm_r ', 1 )

!
! get the correlation self-energy if the case
! (at the end leave the file opened)
!
   IF ( use_correlation ) THEN
        CALL file_open( sgm_unit, TRIM(datafile_sgm), PATH="/", ACTION="read", &
                        FORM="formatted")

           CALL iotk_scan_empty(sgm_unit, "DATA", ATTR=attr, IERR=ierr)
              IF (ierr/=0) CALL errore('conductor','searching DATA',ABS(ierr))
           CALL iotk_scan_attr(attr,"dimwann",dimwann_, IERR=ierr)
              IF (ierr/=0) CALL errore('conductor','searching DIMWANN',ABS(ierr))
           CALL iotk_scan_attr(attr,"nws",nws_, IERR=ierr)
              IF (ierr/=0) CALL errore('conductor','searching nws_',ABS(ierr))
           CALL iotk_scan_attr(attr,"nomega",nomega_, IERR=ierr)
              IF (ierr/=0) CALL errore('conductor','searching NOMEGA_',ABS(ierr))

           IF (nomega_ /= ne) CALL errore('conductor','invalid nomega from SGM',2)
           IF (dimwann_ /= dimC) CALL errore('conductor','invalid dimwann from SGM',2)
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
           ! XXX add a check on the energy grid
           !
   ENDIF 


!
! main loop over frequency
! 
   WRITE(stdout,"()")

   energies: &
   DO ie = 1, ne

      !
      ! grids and misc
      !
      ene =  egrid(ie)  + delta * CI
      WRITE(stdout,"(2x, 'Computing E = ', f9.5, ' eV' )") egrid(ie)

      dos(:,ie) = ZERO
      conduct(:,ie) = ZERO

      !
      ! parallel kpt loop
      !
      kpt_loop: &
      DO ik = 1, nkpts_par

          !
          ! init
          !
          c00_a(:,:)  = h00_L(:,:,ik)  -ene * s00_L(:,:,ik)
          c01_a(:,:)  = h01_L(:,:,ik)  -ene * s01_L(:,:,ik)
          !
          c00_b(:,:)  = h00_R(:,:,ik)  -ene * s00_R(:,:,ik)
          c01_b(:,:)  = h01_R(:,:,ik)  -ene * s01_R(:,:,ik)
          !
          c00_c(:,:)  = h00_C(:,:,ik)  -ene * s00_c(:,:,ik)
          !
          cci_ac(:,:) = h_LC(:,:,ik) -ene * s_LC(:,:,ik) 
          cci_cb(:,:) = h_CR(:,:,ik) -ene * s_CR(:,:,ik)
          !
          cci_ca(:,:) = CONJG( TRANSPOSE( h_LC(:,:,ik) -CONJG(ene)*s_LC(:,:,ik) ))
          cci_bc(:,:) = CONJG( TRANSPOSE( h_CR(:,:,ik) -CONJG(ene)*s_CR(:,:,ik) ))
 
 
          !
          ! get correlaiton self-energy if the case
          !
! XXX aggiornare con kpt
          IF ( use_correlation ) THEN
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
          CALL transfer( dimR, niterx, totB, tottB, c00_b, c01_b )
          CALL green( dimR, totB, tottB, c00_b, c01_b, ene+bias, gB, 1 )
          !
          CALL mat_mul(c2, cci_cb, 'N', gB, 'N', dimC, dimR, dimR)
          CALL mat_mul(sRr, c2, 'N', cci_bc, 'N', dimC, dimC, dimR)
 
          ! ene
          CALL transfer( dimL, niterx, totA, tottA, c00_a, c01_a )
          CALL green( dimL, totA, tottA, c00_a, c01_a, ene, gA, -1 )
          !
          CALL mat_mul(c1, cci_ca, 'N', gA, 'N', dimC, dimL, dimL)
          CALL mat_mul(sLr, c1, 'N', cci_ac, 'N', dimC, dimC, dimL) 
 
          !
          ! gL and gR
          !
          gL(:,:) = CI * (  sLr(:,:) - CONJG( TRANSPOSE(sLr(:,:)) )   )
          gR(:,:) = CI * (  sRr(:,:) - CONJG( TRANSPOSE(sRr(:,:)) )   )
 
          !
          ! Construct the conductor green's function
          ! G_C = aux^-1  (retarded)
          !
          aux(:,:) = -c00_c(:,:) -sLr(:,:) -sRr(:,:) -sgm_r(:,:)
  
          gintr(:,:) = CZERO
          DO i = 1, dimC
             gintr(i,i)= CONE
          ENDDO
 
          CALL mat_sv(dimC, dimC, aux, gintr)
 
          !
          ! Compute density of states for the conductor layer
          !
          DO i = 1, dimC
             dos(i,ie) = dos(i,ie) - wk_par(ik) * AIMAG( gintr(i,i) ) / PI
          ENDDO
 
          !
          ! evaluate the transmittance according to the Fisher-Lee formula
          ! or (in the correlated case) to the generalized expression as 
          ! from PRL 94, 116802 (2005)
          !
          CALL transmittance(dimC, gL, gR, gintr, sgm_r, TRIM(conduct_formula),  &
                             cond_aux )
          conduct(:,ie) = conduct(:,ie) + wk_par(ik) * cond_aux(:)
      
      ENDDO kpt_loop 
   ENDDO energies

   !
   ! close sgm file
   !
   IF ( use_correlation ) CALL file_close(sgm_unit, PATH="/", ACTION="read")


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
   DEALLOCATE ( aux, STAT=ierr )
        IF( ierr /=0 ) CALL errore('conductor', ' deallocating aux', 1 )
   DEALLOCATE ( sRr, sLr, STAT=ierr )
        IF( ierr /=0 ) CALL errore('conductor', ' deallocating sRr, sLr ', 1 )
   DEALLOCATE ( c1, c2, STAT=ierr )
        IF( ierr /=0 ) CALL errore('conductor', ' deallocating c1, c2 ', 1 )
   DEALLOCATE ( dos, STAT=ierr )
        IF( ierr /=0 ) CALL errore('conductor', ' deallocating dos ', 1 )
   DEALLOCATE ( conduct, STAT=ierr )
        IF( ierr /=0 ) CALL errore('conductor', ' deallocating conduct ', 1 )
   DEALLOCATE ( sgm_r, STAT=ierr )
        IF( ierr /=0 ) CALL errore('conductor', ' deallocating sgm_r ', 1 )

   CALL cleanup()

END PROGRAM conductor
  
