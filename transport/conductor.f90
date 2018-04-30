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
   USE constants,            ONLY : PI, ZERO, ONE, CZERO, CONE, CI
   USE parameters,           ONLY : nstrx 
   USE version_module,       ONLY : version_number
   USE parser_module,        ONLY : change_case
   USE files_module,         ONLY : file_open, file_close
   USE timing_module,        ONLY : timing_upto_now
   USE util_module,          ONLY : mat_mul, mat_sv
   USE T_input_module,       ONLY : input_manager
   USE io_module,            ONLY : stdout, stdin, sgm_unit => aux_unit,   &
                                    dos_unit => aux1_unit, cond_unit => aux2_unit, &
                                    work_dir, prefix, postfix, aux_unit
   USE T_control_module,     ONLY : use_overlap, use_correlation, calculation_type, &
                                    conduct_formula, niterx, nprint, datafile_sgm, write_kdata
   USE T_egrid_module,       ONLY : egrid_init, ne, egrid
   USE T_smearing_module,    ONLY : smearing_init, smearing_type
   USE T_kpoints_module,     ONLY : kpoints_init, nkpts_par, wk_par
   USE T_hamiltonian_module, ONLY : dimL, dimR, dimC, dimx,            &
                                    h00_L, h01_L, h00_R, h01_R, h00_C, & 
                                    s00_L, s01_L, s00_R, s01_R, s00_C, &
                                    h_LC, h_CR, s_LC, s_CR
   USE T_workspace_module,   ONLY : aux00_L, aux01_L, aux00_R, aux01_R, aux00_C, &
                                    aux00eff_L, aux01eff_L, aux00eff_R, aux01eff_R, &
                                    aux_LC, aux_CL, aux_CR, aux_RC,    &
                                    totL, tottL, totR, tottR,          &
                                    gR, gL, gC, gamma_R, gamma_L, sgm_L, sgm_R, &
                                    workspace_allocate
   USE T_correlation_module, ONLY : sgm_corr, correlation_sgmread, correlation_allocate, &
                                    correlation_init

   IMPLICIT NONE

   !
   ! local variables
   !
   COMPLEX(dbl)     :: ene
   CHARACTER(nstrx) :: filename
   INTEGER          :: i, ie, ik, ierr, ncount, skip_count, niter
   REAL(dbl)        :: avg_iter
   CHARACTER(4)     :: ctmp


   !   
   REAL(dbl),    ALLOCATABLE :: dos(:,:,:), conduct(:,:,:)
   REAL(dbl),    ALLOCATABLE :: cond_aux(:)
   COMPLEX(dbl), ALLOCATABLE :: work(:,:), work1(:,:)

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
   ! smearing functions
   !
   CALL smearing_init()

   !
   ! initialize kpoints and R vectors
   !
   CALL kpoints_init()

   !
   ! Set up the layer hamiltonians
   !
   CALL hamiltonian_init( use_overlap, calculation_type )

   !
   ! write input data on the output file
   !
   CALL summary( stdout )

   !
   ! setup correlation data, if the case
   !
   CALL correlation_allocate()
   !
   sgm_corr(:,:,:) = CZERO
   !
   IF ( use_correlation ) THEN 
       !
       CALL file_open( sgm_unit, TRIM(datafile_sgm), PATH="/", ACTION="read" )
       CALL correlation_init( sgm_unit )
       !
   ENDIF   


   !
   ! local variable allocations
   !
   CALL workspace_allocate()

   ALLOCATE ( dos(dimC,nkpts_par,ne), STAT=ierr )
      IF( ierr /=0 ) CALL errore('conductor','allocating dos', ABS(ierr) )
   ALLOCATE ( conduct(dimC,nkpts_par,ne), STAT=ierr )
      IF( ierr /=0 ) CALL errore('conductor','allocating conduct', ABS(ierr) )
   ALLOCATE ( cond_aux(dimC), STAT=ierr )
      IF( ierr /=0 ) CALL errore('conductor','allocating cond_aux', ABS(ierr) )
   ALLOCATE ( work(dimx,dimx), STAT=ierr )
      IF( ierr /=0 ) CALL errore('conductor','allocating work', ABS(ierr) )
   ALLOCATE ( work1(dimC,dimC), STAT=ierr )
      IF( ierr /=0 ) CALL errore('conductor','allocating work1', ABS(ierr) )

!
! actual calculation
!
   WRITE( stdout, "(/,2x,70('='))" )
   WRITE( stdout, "(2x,'=',24x, 'Transport calculation', 23x,'=')" )
   WRITE( stdout, "(2x,70('='),/)" )

!
! main loop over frequencies
! 
   WRITE(stdout,"()")
   skip_count = 0
   !
   !
   energy_loop: &
   DO ie = 1, ne
      !
      ncount = ie
      !
      ! grids and misc
      !
      ene =  egrid(ie)   
      !
      IF ( MOD( ncount, nprint) == 0 .OR. ncount == 1 ) THEN
           !
           WRITE(stdout,"(/,2x, 'Computing E( ',i5,' ) = ', f9.5, ' eV' )") &
                         ncount, egrid(ie)
      ENDIF

      !
      ! get correlaiton self-energy if the case
      !
      IF ( use_correlation ) &
          CALL correlation_sgmread(sgm_unit, ie, sgm_corr)
      !
      ! initialization of the average number of iteration 
      !
      avg_iter = ZERO
      !
      ! parallel kpt loop
      !
      kpt_loop: &
      DO ik = 1, nkpts_par

          !
          ! init 
          !
          dos(:,ik,ie)     = ZERO
          conduct(:,ik,ie) = ZERO

          !
          aux00_L(:,:)  = ene * s00_L(:,:,ik) -h00_L(:,:,ik)
          aux01_L(:,:)  = ene * s01_L(:,:,ik) -h01_L(:,:,ik)
          ! 
          aux00_R(:,:)  = ene * s00_R(:,:,ik) -h00_R(:,:,ik)
          aux01_R(:,:)  = ene * s01_R(:,:,ik) -h01_R(:,:,ik)
          !
          aux00_C(:,:)  = ene * s00_C(:,:,ik) -h00_C(:,:,ik)
          !
          aux_LC(:,:)   = ene * s_LC(:,:,ik)  -h_LC(:,:,ik)
          aux_CR(:,:)   = ene * s_CR(:,:,ik)  -h_CR(:,:,ik)
          !
          aux_CL(:,:)   = CONJG( TRANSPOSE( CONJG(ene)*s_LC(:,:,ik) -h_LC(:,:,ik) ))
          aux_RC(:,:)   = CONJG( TRANSPOSE( CONJG(ene)*s_CR(:,:,ik) -h_CR(:,:,ik) ))

!!
!! XXXX Development for including smearing in a consistent way
          !
          ! define effective quantities using smearing
          !
          CALL define_smear_ham( dimL, aux00_L, aux01_L, s00_L(:,:,ik), s01_L(:,:,ik), &
                                 aux00eff_L, aux01eff_L, 'numeric' )
                                 !
          CALL define_smear_ham( dimR, aux00_R, aux01_R, s00_R(:,:,ik), s01_R(:,:,ik), &
                                 aux00eff_R, aux01eff_R, 'numeric' )

          aux_LC = aux01eff_L
          aux_CR = aux01eff_R
          aux_CL(:,:)   = CONJG( TRANSPOSE( aux01eff_L ) )
          aux_RC(:,:)   = CONJG( TRANSPOSE( aux01eff_L ) )
          !
          aux00_C = aux00eff_L
!
!! XXXX
!!
        
 
          ! 
          ! construct leads self-energies 
          ! 
          ! ene + bias
          CALL transfer( dimR, niterx, totR, tottR, aux00eff_R, aux01eff_R, niter, ierr )
          !
          IF ( ierr/=0) THEN
               !
               skip_count     = skip_count + 1
               !
               CALL warning ( stdout , "bad convergence, discarting frequency")
               !
               dos(:,:,ie)     = -ONE / REAL( dimC, dbl )
               conduct(:,:,ie) = -ONE / REAL( dimC, dbl )
               !
               CYCLE energy_loop
               !
          ENDIF
          !
          avg_iter = avg_iter + REAL(niter, dbl)
          !
          CALL green( dimR, totR, tottR, aux00eff_R, aux01eff_R, gR, 1 )
          !
          !
          CALL mat_mul(work, aux_CR, 'N', gR, 'N', dimC, dimR, dimR)
          CALL mat_mul(sgm_R, work,  'N', aux_RC, 'N', dimC, dimC, dimR)
 
          ! ene
          CALL transfer( dimL, niterx, totL, tottL, aux00eff_L, aux01eff_L, niter, ierr )
          !
          IF ( ierr/=0) THEN
               !
               skip_count     = skip_count + 1
               !
               CALL warning ( stdout , "bad convergence, discarting frequency")
               !
               dos(:,:,ie)     = -ONE / REAL( dimC, dbl )
               conduct(:,:,ie) = -ONE / REAL( dimC, dbl )
               !
               CYCLE energy_loop
               !
          ENDIF
          !
          avg_iter = avg_iter + REAL(niter, dbl)
          !
          CALL green( dimL, totL, tottL, aux00eff_L, aux01eff_L, gL, -1 )
          !
          !
          CALL mat_mul(work, aux_CL, 'N', gL, 'N', dimC, dimL, dimL)
          CALL mat_mul(sgm_L, work,  'N', aux_LC, 'N', dimC, dimC, dimL) 
 
          !
          ! gamma_L and gamma_R
          !
          gamma_L(:,:) = CI * (  sgm_L(:,:) - CONJG( TRANSPOSE(sgm_L(:,:)) )   )
          gamma_R(:,:) = CI * (  sgm_R(:,:) - CONJG( TRANSPOSE(sgm_R(:,:)) )   )
 
          !
          ! Construct the conductor green's function
          ! gC = work^-1  (retarded)
          !
          CALL gzero_maker ( dimC, aux00_C(:,:), s00_C(:,:,ik), work, 'inverse', smearing_type )
          !
! XXX
!work(:,:) = aux00_C(:,:)
! XXX
          !
          work(:,:) = work(:,:) -sgm_L(:,:) -sgm_R(:,:) -sgm_corr(:,:,ik)  
  

          gC(:,:) = CZERO
          !
          DO i = 1, dimC
             gC(i,i)= CONE
          ENDDO
 
          CALL mat_sv(dimC, dimC, work, gC)
          !
          ! Compute density of states for the conductor layer
          !
          DO i = 1, dimC
             dos(i,ik,ie) = - wk_par(ik) * AIMAG( gC(i,i) ) / PI
          ENDDO
          !
          ! evaluate the transmittance according to the Fisher-Lee formula
          ! or (in the correlated case) to the generalized expression as 
          ! from PRL 94, 116802 (2005)
          !
          CALL transmittance(dimC, gamma_L, gamma_R, gC, sgm_corr(1,1,ik), &
                             TRIM(conduct_formula), cond_aux )

          conduct(:,ik,ie) =  wk_par(ik) * cond_aux(:)
      
      ENDDO kpt_loop 


      avg_iter = avg_iter/REAL(2*nkpts_par)
      !
      IF ( MOD( ncount, nprint) == 0 .OR. ncount == 1 ) THEN
           !
           WRITE(stdout,"(2x,'T matrix converged after avg. # of iterations ',f8.3)") &
                 avg_iter
           !
           CALL timing_upto_now(stdout)
           CALL flush_unit(stdout)
           ! 
      ENDIF

   ENDDO energy_loop
   !
   !
   IF ( skip_count /= 0 ) THEN
       !
       ! report about that some frequencies have been skipped due to mis-converged
       ! lead self-energies
       !
       WRITE(stdout,"(/,2x,'WARNING: ',i4,' freq. not computed (Lead sgm NOT converged)')") &
                     skip_count
       !
   ENDIF

   !
   ! close sgm file
   !
   IF ( use_correlation ) CALL file_close(sgm_unit, PATH="/", ACTION="read")


!
! write DOS and CONDUCT data on files
!
   filename = TRIM(work_dir)//'/'//TRIM(prefix)//'cond'//TRIM(postfix)//'.dat'
   OPEN ( cond_unit, FILE=TRIM(filename), FORM='formatted' )
   !
   DO ie = 1, ne
       !
       WRITE ( cond_unit, '(2(f15.9))' ) egrid(ie), SUM( conduct(:,:,ie) )
       !
   ENDDO
   !
   CLOSE( cond_unit )
   !
   filename = TRIM(prefix)//'cond'//TRIM(postfix)//'.dat'
   WRITE(stdout,"(/,2x,'Conductance written on file: ',3x,a)") TRIM(filename)
                        


   filename = TRIM(work_dir)//'/'//TRIM(prefix)//'doscond'//TRIM(postfix)//'.dat'
   OPEN ( dos_unit, FILE=TRIM(filename), FORM='formatted' )
   !
   DO ie = 1, ne
       WRITE ( dos_unit, '(2(f15.9))' ) egrid(ie), SUM( dos(:,:,ie) )
   ENDDO
   !
   CLOSE( dos_unit )
   !
   filename = TRIM(prefix)//'doscond'//TRIM(postfix)//'.dat'
   WRITE(stdout,"(/,2x,'DOS written on file: ',3x,a)") TRIM(filename)


!
!  write kpoint-resolved dos and transmittance data on files
!
   IF ( write_kdata ) THEN
      !
      DO ik = 1, nkpts_par
         !
         WRITE( ctmp , "(i4.4)" ) ik
         filename= TRIM(work_dir)//'/'//TRIM(prefix)// &
                                        '_cond-'//ctmp//TRIM(postfix)//'.dat'
         !
         OPEN( aux_unit, FILE=TRIM(filename), FORM='formatted', IOSTAT=ierr )
         !
         IF (ierr/=0) CALL errore('conductor','opening '//TRIM(filename),ABS(ierr))
         !
         WRITE( aux_unit, *) "# E (eV)   cond(E)"
         !
         DO ie = 1, ne
              WRITE( aux_unit, '(2(f15.9))') egrid(ie), SUM( conduct(:,ik,ie) )
         ENDDO
         !
         CLOSE( aux_unit )         
         !
      ENDDO
      !
      !
      DO ik = 1, nkpts_par
         !
         WRITE( ctmp , "(i4.4)" ) ik
         filename= TRIM(work_dir)//'/'//TRIM(prefix)// &
                                       '_doscond-'//ctmp//TRIM(postfix)//'.dat'

         OPEN( aux_unit, FILE=TRIM(filename), FORM='formatted', IOSTAT=ierr )
         !
         IF (ierr/=0) CALL errore('conductor','opening '//TRIM(filename),ABS(ierr))
         !
         WRITE( aux_unit, *) "# E (eV)   doscond(E)"
         !
         DO ie = 1, ne
             WRITE( aux_unit, '(2(f15.9))') egrid(ie), SUM( dos(:,ik,ie) )
         ENDDO
         !
         CLOSE( aux_unit )         
         !
      ENDDO
      !
   ENDIF
   

!
!...  shutdown
!

   !
   ! clean local memory
   !
   DEALLOCATE ( dos, STAT=ierr )
        IF( ierr /=0 ) CALL errore('conductor','deallocating dos', ABS(ierr) )
   DEALLOCATE ( conduct, STAT=ierr )
        IF( ierr /=0 ) CALL errore('conductor','deallocating conduct', ABS(ierr) )
   DEALLOCATE ( cond_aux, STAT=ierr )
        IF( ierr /=0 ) CALL errore('conductor','deallocating cond_aux', ABS(ierr) )
   DEALLOCATE ( work, work1, STAT=ierr )
        IF( ierr /=0 ) CALL errore('conductor','deallocating work, work1', ABS(ierr) )

   !
   ! clean global memory
   !
   CALL cleanup()

   !
   ! finalize
   !
   CALL shutdown ( 'conductor' ) 

END PROGRAM conductor
  
