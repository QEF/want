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
   !
   USE iotk_module
   USE kinds,                ONLY : dbl
   USE constants,            ONLY : PI, ZERO, CZERO, CONE, CI, EPS_m5
   USE parameters,           ONLY : nstrx 
   USE version_module,       ONLY : version_number
   USE parser_module,        ONLY : change_case
   USE files_module,         ONLY : file_open, file_close
   USE timing_module,        ONLY : timing, timing_upto_now
   USE util_module,          ONLY : mat_mul, mat_sv
   USE mp_global,            ONLY : mpime, nproc
   USE mp,                   ONLY : mp_sum
   USE io_module,            ONLY : ionode, stdout, stdin, sgm_unit, &
                                    dos_unit => aux1_unit, cond_unit => aux2_unit, &
                                    work_dir, prefix, postfix, aux_unit
   USE T_input_module,       ONLY : input_manager
   USE T_control_module,     ONLY : calculation_type, &
                                    conduct_formula, niterx, nprint, datafile_sgm,  &
                                    write_kdata
   USE T_egrid_module,       ONLY : egrid_init, ne, egrid, egrid_alloc => alloc
   USE T_smearing_module,    ONLY : smearing_init
   USE T_kpoints_module,     ONLY : kpoints_init, nkpts_par, wk_par
   USE T_hamiltonian_module, ONLY : dimL, dimR, dimC, dimx,            &
                                    h00_L, h01_L, h00_R, h01_R, h00_C, & 
                                    s00_L, s01_L, s00_R, s01_R, s00_C, &
                                    h_LC, h_CR, s_LC, s_CR,            &
                                    shift_L, shift_C, shift_R
   USE T_workspace_module,   ONLY : aux00_L, aux01_L, aux00_R, aux01_R, aux00_C, &
                                    aux_LC, aux_CL, aux_CR, aux_RC,    &
                                    totL, tottL, totR, tottR,          &
                                    gR, gL, gC, gamma_R, gamma_L, sgm_L, sgm_R, &
                                    workspace_allocate
   USE T_correlation_module, ONLY : sgm_corr, lhave_corr, ldynam_corr, shift_corr, &
                                    correlation_sgmread, correlation_allocate, &
                                    correlation_init
   USE T_datafiles_module,   ONLY : datafiles_init

   IMPLICIT NONE

   !
   ! local variables
   !
   CHARACTER(9)     :: subname='conductor'
   !
   COMPLEX(dbl)     :: ene
   CHARACTER(nstrx) :: filename
   INTEGER          :: i, ie, ik, ierr, niter
   INTEGER          :: nomg_s, nomg_e
   REAL(dbl)        :: avg_iter
   CHARACTER(4)     :: ctmp


   !   
   REAL(dbl),    ALLOCATABLE :: dos(:,:), conduct(:,:)
   REAL(dbl),    ALLOCATABLE :: cond_aux(:)
   COMPLEX(dbl), ALLOCATABLE :: work(:,:)

!
!------------------------------
! main body
!------------------------------
!
   CALL startup(version_number,subname)

!
! read input file
!
   CALL input_manager()
      

!
! init
!
   !
   ! check whether data files should be internally converted
   !
   CALL datafiles_init()

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
   CALL hamiltonian_init( calculation_type )



   !
   ! setup correlation data and energy grids
   !
   ! If correlation is used, the energy grid is read
   ! from datafile_sgm and input parameters are overwirtten
   !
   ! otherwise, grid is built indipendently
   !

   CALL correlation_allocate()
   sgm_corr(:,:,:) = CZERO
   !
   IF ( lhave_corr ) THEN 
       !
       CALL file_open( sgm_unit, TRIM(datafile_sgm), PATH="/", ACTION="read", IERR=ierr ) 
       IF ( ierr/=0 ) CALL errore(subname,'opening '//TRIM(datafile_sgm), ABS(ierr) )
       !
       CALL correlation_init( sgm_unit )

       !
       ! Read correlation data if not dynamical
       !
       IF ( .NOT. ldynam_corr ) THEN
           !
           CALL correlation_sgmread(sgm_unit, sgm_corr )
           !
           sgm_corr(:,:,:) = sgm_corr(:,:,:) +shift_corr * S00_C(:,:,:)
           !
       ENDIF
       !
   ENDIF   
   !
   IF ( .NOT. egrid_alloc ) THEN
       !
       CALL egrid_init()
       !
   ENDIF

   !
   ! write input data on the output file
   !
   IF (ionode) CALL summary( stdout )



   !
   ! local variable allocations
   !
   CALL workspace_allocate()

   ALLOCATE ( dos(nkpts_par,ne), STAT=ierr )
   IF( ierr /=0 ) CALL errore(subname,'allocating dos', ABS(ierr) )
   !
   ALLOCATE ( conduct(nkpts_par,ne), STAT=ierr )
   IF( ierr /=0 ) CALL errore(subname,'allocating conduct', ABS(ierr) )
   !
   ALLOCATE ( cond_aux(dimC), STAT=ierr )
   IF( ierr /=0 ) CALL errore(subname,'allocating cond_aux', ABS(ierr) )
   !
   ALLOCATE ( work(dimx,dimx), STAT=ierr )
   IF( ierr /=0 ) CALL errore(subname,'allocating work', ABS(ierr) )


!
! main loop over frequency
! 

   IF ( ionode ) THEN
      !
      CALL write_header( stdout, "Frequency Loop" )
      CALL flush_unit( stdout )
      !
   ENDIF

   !
   ! init parallelism over frequencies
   !
   CALL divide_et_impera( 1, ne,  nomg_s, nomg_e, mpime, nproc )


   dos(:,:)     = ZERO
   conduct(:,:) = ZERO
   !
   energy_loop: &
   DO ie = nomg_s, nomg_e
      
      !
      ! grids and misc
      !
      ene =  egrid(ie)   

      IF ( (MOD( ie, nprint) == 0 .OR. ie == 1) .AND. ionode ) THEN
           WRITE(stdout,"(2x, 'Computing E( ',i5,' ) = ', f12.5, ' eV' )") &
                         ie, egrid(ie)
      ENDIF


      !
      ! get correlaiton self-energy if the case
      !
      IF ( lhave_corr .AND. ldynam_corr ) THEN
          !
          CALL correlation_sgmread(sgm_unit, sgm_corr, IE=ie )
          !
          sgm_corr(:,:,:) = sgm_corr(:,:,:) +shift_corr * S00_C(:,:,:)
          !
      ENDIF


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
          !
          aux00_L(:,:)  = h00_L(:,:,ik)  -(ene -shift_L) * s00_L(:,:,ik)
          aux01_L(:,:)  = h01_L(:,:,ik)  -(ene -shift_L) * s01_L(:,:,ik)
          !
          aux00_R(:,:)  = h00_R(:,:,ik)  -(ene -shift_R) * s00_R(:,:,ik)
          aux01_R(:,:)  = h01_R(:,:,ik)  -(ene -shift_R) * s01_R(:,:,ik)
          !
          aux00_C(:,:)  = h00_C(:,:,ik)  -(ene -shift_C) * s00_C(:,:,ik)
          !
          aux_LC(:,:) = h_LC(:,:,ik) -(ene -shift_C) * s_LC(:,:,ik)  
          aux_CR(:,:) = h_CR(:,:,ik) -(ene -shift_C) * s_CR(:,:,ik) 
          !
          aux_CL(:,:) = CONJG( TRANSPOSE( h_LC(:,:,ik) -CONJG(ene -shift_C) * s_LC(:,:,ik) ))
          aux_RC(:,:) = CONJG( TRANSPOSE( h_CR(:,:,ik) -CONJG(ene -shift_C) * s_CR(:,:,ik) ))
 
          ! 
          ! construct leads self-energies 
          ! 
          CALL transfer( dimR, s00_R(:,:,ik),  niterx, totR, tottR, aux00_R, aux01_R, niter )
          avg_iter = avg_iter + REAL(niter)
          !
          CALL green( dimR, totR, tottR, aux00_R, aux01_R, s00_R(:,:,ik), gR, 1 )
          !
          !
          CALL mat_mul(work, aux_CR, 'N', gR, 'N', dimC, dimR, dimR)
          CALL mat_mul(sgm_R, work, 'N', aux_RC, 'N', dimC, dimC, dimR)
 
          ! ene
          CALL transfer( dimL, s00_L(:,:,ik), niterx, totL, tottL, aux00_L, aux01_L, niter )
          avg_iter = avg_iter + REAL(niter)
          !
          CALL green( dimL, totL, tottL, aux00_L, aux01_L,  s00_L(:,:,ik), gL, -1 )
          !
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
          work = CZERO
          !
          CALL gzero_maker ( dimC, -aux00_C, s00_C(:,:,ik), work(1:dimC,1:dimC), 'inverse')
          work(1:dimC,1:dimC) = work(1:dimC,1:dimC) -sgm_L(:,:) -sgm_R(:,:) -sgm_corr(:,:,ik)  
  
          gC(:,:) = CZERO
          DO i = 1, dimC
             gC(i,i)= CONE
          ENDDO
 
          CALL mat_sv(dimC, dimC, work, gC)
          !
          ! Compute density of states for the conductor layer
          !
          DO i = 1, dimC
             dos(ik,ie) = dos(ik,ie) - wk_par(ik) * AIMAG( gC(i,i) ) / PI
          ENDDO
          !
          ! evaluate the transmittance according to the Fisher-Lee formula
          ! or (in the correlated case) to the generalized expression as 
          ! from PRL 94, 116802 (2005)
          !
          CALL transmittance(dimC, gamma_L, gamma_R, gC, sgm_corr(:,:,ik), &
                             TRIM(conduct_formula), cond_aux )
          DO i=1,dimC
             conduct(ik,ie) =  conduct(ik,ie) + wk_par(ik) * cond_aux(i)
          ENDDO
      
      ENDDO kpt_loop 


      avg_iter = avg_iter/REAL(2*nkpts_par)
      !
      IF ( (MOD( ie, nprint) == 0 .OR. &
            ie == nomg_s .OR. ie == nomg_e ) .AND. ionode) THEN
           !
           WRITE(stdout,"(2x,'T matrix converged after avg. # of iterations ',f8.3,/)") &
                 avg_iter
           !
           CALL timing_upto_now(stdout)
           CALL flush_unit(stdout)
           ! 
      ENDIF

   ENDDO energy_loop

   !
   ! recover over frequencies
   !
   CALL mp_sum( conduct )
   CALL mp_sum( dos )

   !
   ! close sgm file
   !
   IF ( lhave_corr ) THEN
       !
       CALL file_close(sgm_unit, PATH="/", ACTION="read", IERR=ierr)
       IF ( ierr/=0 ) CALL errore(subname,'closing '//TRIM(datafile_sgm), ABS(ierr) )
       !
   ENDIF
       


!
! write DOS and CONDUCT data on files
!

   IF ( ionode ) THEN
       !
       CALL write_header( stdout, "Writing data" )
       CALL flush_unit( stdout )
   

       filename = TRIM(work_dir)//'/'//TRIM(prefix)//'cond'//TRIM(postfix)//'.dat'
       OPEN ( cond_unit, FILE=TRIM(filename), FORM='formatted' )
       !
       DO ie = 1, ne
           !
           WRITE ( cond_unit, '(2(f15.9))' ) egrid(ie), SUM( conduct(:,ie) )
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
           WRITE ( dos_unit, '(2(f15.9))' ) egrid(ie), SUM( dos(:,ie) )
       ENDDO
       !
       CLOSE( dos_unit )
       !
       filename = TRIM(prefix)//'doscond'//TRIM(postfix)//'.dat'
       WRITE(stdout,"(  2x,'        DOS written on file: ',3x,a)") TRIM(filename)
   
   ENDIF


!
!  write kpoint-resolved dos and transmittance data on files
!
   IF ( write_kdata .AND. ionode ) THEN
      !
      DO ik = 1, nkpts_par
         !
         WRITE( ctmp , "(i4.4)" ) ik
         filename= TRIM(work_dir)//'/'//TRIM(prefix)// &
                                        '_cond-'//ctmp//TRIM(postfix)//'.dat'
         !
         OPEN( aux_unit, FILE=TRIM(filename), FORM='formatted', IOSTAT=ierr )
         !
         IF (ierr/=0) CALL errore(subname,'opening '//TRIM(filename),ABS(ierr))
         !
         WRITE( aux_unit, *) "# E (eV)   cond(E)"
         !
         DO ie = 1, ne
              WRITE( aux_unit, '(2(f15.9))') egrid(ie), conduct(ik,ie) 
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
         IF (ierr/=0) CALL errore(subname,'opening '//TRIM(filename),ABS(ierr))
         !
         WRITE( aux_unit, *) "# E (eV)   doscond(E)"
         !
         DO ie = 1, ne
             WRITE( aux_unit, '(2(f15.9))') egrid(ie), dos(ik,ie) 
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
   IF( ierr /=0 ) CALL errore(subname,'deallocating dos', ABS(ierr) )
   !
   DEALLOCATE ( conduct, STAT=ierr )
   IF( ierr /=0 ) CALL errore(subname,'deallocating conduct', ABS(ierr) )
   !
   DEALLOCATE ( cond_aux, STAT=ierr )
   IF( ierr /=0 ) CALL errore(subname,'deallocating cond_aux', ABS(ierr) )
   !
   DEALLOCATE ( work, STAT=ierr )
   IF( ierr /=0 ) CALL errore(subname,'deallocating work', ABS(ierr) )

   !
   ! clean global memory
   !
   CALL cleanup()

   !
   ! finalize
   !
   CALL shutdown ( subname ) 

END PROGRAM conductor
  
