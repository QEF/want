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
   USE iotk_module
   USE kinds,                ONLY : dbl
   USE constants,            ONLY : PI, ZERO, CZERO, CONE, CI, EPS_m5
   USE parameters,           ONLY : nstrx 
   USE version_module,       ONLY : version_number
   USE parser_module,        ONLY : change_case
   USE files_module,         ONLY : file_open, file_close
   USE timing_module,        ONLY : timing, timing_overview, global_list
   USE util_module,          ONLY : mat_mul, mat_sv
   USE T_input_module,       ONLY : input_manager
   USE io_module,            ONLY : stdout, stdin, sgm_unit => aux_unit,   &
                                    dos_unit => aux1_unit, cond_unit => aux2_unit
   USE T_control_module,     ONLY : use_overlap, use_correlation, calculation_type, &
                                    conduct_formula, niterx, bias, datafile_sgm 
   USE T_egrid_module,       ONLY : egrid_init, ne, egrid, delta
   USE T_kpoints_module,     ONLY : kpoints_init, nkpts_par , wk_par
   USE T_hamiltonian_module, ONLY : dimL, dimR, dimC, dimx,            &
                                    h00_L, h01_L, h00_R, h01_R, h00_C, & 
                                    s00_L, s01_L, s00_R, s01_R, s00_c, &
                                    h_LC, h_CR, s_LC, s_CR
   USE T_workspace_module,   ONLY : aux00_L, aux01_L, aux00_R, aux01_R, aux00_C, &
                                    aux_LC, aux_CL, aux_CR, aux_RC,    &
                                    totL, tottL, totR, tottR,          &
                                    gR, gL, gC, gamma_R, gamma_L, sgm_L, sgm_R, &
                                    workspace_allocate

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
   COMPLEX(dbl), ALLOCATABLE :: work(:,:)
   COMPLEX(dbl), ALLOCATABLE :: sgm_corr(:,:)
     

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
   CALL workspace_allocate()

   ALLOCATE ( dos(dimC,ne), STAT=ierr )
      IF( ierr /=0 ) CALL errore('conductor','allocating dos', ABS(ierr) )
   ALLOCATE ( conduct(dimC,ne), STAT=ierr )
      IF( ierr /=0 ) CALL errore('conductor','allocating conduct', ABS(ierr) )
   ALLOCATE ( cond_aux(dimC), STAT=ierr )
      IF( ierr /=0 ) CALL errore('conductor','allocating cond_aux', ABS(ierr) )
   ALLOCATE ( sgm_corr(dimC,dimC), STAT=ierr )
      IF( ierr /=0 ) CALL errore('conductor','allocating sgm_corr', ABS(ierr) )
   ALLOCATE ( work(dimx,dimx), STAT=ierr )
      IF( ierr /=0 ) CALL errore('conductor','allocating work', ABS(ierr) )

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
          aux00_L(:,:)  = h00_L(:,:,ik)  -ene * s00_L(:,:,ik)
          aux01_L(:,:)  = h01_L(:,:,ik)  -ene * s01_L(:,:,ik)
          !
          aux00_R(:,:)  = h00_R(:,:,ik)  -ene * s00_R(:,:,ik)
          aux01_R(:,:)  = h01_R(:,:,ik)  -ene * s01_R(:,:,ik)
          !
          aux00_C(:,:)  = h00_C(:,:,ik)  -ene * s00_c(:,:,ik)
          !
          aux_LC(:,:) = h_LC(:,:,ik) -ene * s_LC(:,:,ik) 
          aux_CR(:,:) = h_CR(:,:,ik) -ene * s_CR(:,:,ik)
          !
          aux_CL(:,:) = CONJG( TRANSPOSE( h_LC(:,:,ik) -CONJG(ene)*s_LC(:,:,ik) ))
          aux_RC(:,:) = CONJG( TRANSPOSE( h_CR(:,:,ik) -CONJG(ene)*s_CR(:,:,ik) ))
 
 
          !
          ! get correlaiton self-energy if the case
          !
! XXX aggiornare con kpt
          IF ( use_correlation ) THEN
              CALL iotk_scan_begin(sgm_unit, "OPR"//TRIM(iotk_index(ie)), IERR=ierr)
                  IF (ierr/=0) CALL errore('conductor','searching for OPR',ie)
              CALL iotk_scan_dat(sgm_unit, TRIM(sgmtag), sgm_corr, IERR=ierr)
                  IF (ierr/=0) CALL errore('conductor','searching for '//TRIM(sgmtag),ie)
              CALL iotk_scan_end(sgm_unit, "OPR"//TRIM(iotk_index(ie)), IERR=ierr)
                  IF (ierr/=0) CALL errore('conductor','searching end for OPR',ie)
          ELSE
              sgm_corr(:,:) = CZERO
          ENDIF
          
 
          ! 
          ! construct leads self-energies 
          ! 
          ! ene + bias
          CALL transfer( dimR, niterx, totR, tottR, aux00_R, aux01_R )
          CALL green( dimR, totR, tottR, aux00_R, aux01_R, ene+bias, gR, 1 )
          !
          CALL mat_mul(work, aux_CR, 'N', gR, 'N', dimC, dimR, dimR)
          CALL mat_mul(sgm_R, work, 'N', aux_RC, 'N', dimC, dimC, dimR)
 
          ! ene
          CALL transfer( dimL, niterx, totL, tottL, aux00_L, aux01_L )
          CALL green( dimL, totL, tottL, aux00_L, aux01_L, ene, gL, -1 )
          !
          CALL mat_mul(work, aux_CL, 'N', gL, 'N', dimC, dimL, dimL)
          CALL mat_mul(sgm_L, work, 'N', aux_LC, 'N', dimC, dimC, dimL) 
 
          !
          ! gamma_L and gamma_R
          !
          gamma_L(:,:) = CI * (  sgm_L(:,:) - CONJG( TRANSPOSE(sgm_L(:,:)) )   )
          gamma_R(:,:) = CI * (  sgm_R(:,:) - CONJG( TRANSPOSE(sgm_R(:,:)) )   )
 
          !
          ! Construct the conductor green's function
          ! gC = work^-1  (retarded)
          !
          work(1:dimC,1:dimC) = -aux00_C(:,:) -sgm_L(:,:) -sgm_R(:,:) -sgm_corr(:,:)
  
          gC(:,:) = CZERO
          DO i = 1, dimC
             gC(i,i)= CONE
          ENDDO
 
          CALL mat_sv(dimC, dimC, work, gC)
 
          !
          ! Compute density of states for the conductor layer
          !
          DO i = 1, dimC
             dos(i,ie) = dos(i,ie) - wk_par(ik) * AIMAG( gC(i,i) ) / PI
          ENDDO
 
          !
          ! evaluate the transmittance according to the Fisher-Lee formula
          ! or (in the correlated case) to the generalized expression as 
          ! from PRL 94, 116802 (2005)
          !
          CALL transmittance(dimC, gamma_L, gamma_R, gC, sgm_corr, &
                             TRIM(conduct_formula), cond_aux )
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
   DEALLOCATE ( dos, STAT=ierr )
        IF( ierr /=0 ) CALL errore('conductor','deallocating dos', ABS(ierr) )
   DEALLOCATE ( conduct, STAT=ierr )
        IF( ierr /=0 ) CALL errore('conductor','deallocating conduct', ABS(ierr) )
   DEALLOCATE ( cond_aux, STAT=ierr )
        IF( ierr /=0 ) CALL errore('conductor','deallocating cond_aux', ABS(ierr) )
   DEALLOCATE ( sgm_corr, STAT=ierr )
        IF( ierr /=0 ) CALL errore('conductor','deallocating sgm_corr', ABS(ierr) )
   DEALLOCATE ( work, STAT=ierr )
        IF( ierr /=0 ) CALL errore('conductor','deallocating work', ABS(ierr) )

   CALL cleanup()

END PROGRAM conductor
  
