!
! Copyright (C) 2004 WanT Group
!
! This file is distributed under the terms of the
! GNU General Public License. See the file `License'
! in the root directory of the present distribution,
! or http://www.gnu.org/copyleft/gpl.txt .
!
! Based on a previous version by Marco Buongiorno Nardelli
! See the CREDITS file in the ~want directory for a full description
!
!***********************************************
   PROGRAM conductor
   !***********************************************
   !
   ! Computes the quantum transmittance across a junction.
   !
   USE version_module,       ONLY : version_number
   USE io_module,            ONLY : stdout, sgm_unit
   USE timing_module,        ONLY : timing
   USE log_module,           ONLY : log_push, log_pop
   USE T_input_module,       ONLY : input_manager
   USE T_control_module,     ONLY : datafile_C, transport_dir
   USE T_smearing_module,    ONLY : smearing_init
   USE T_egrid_module,       ONLY : egrid_init, egrid_alloc => alloc
   USE T_kpoints_module,     ONLY : kpoints_init
   USE T_datafiles_module,   ONLY : datafiles_init
   USE T_correlation_module, ONLY : lhave_corr, ldynam_corr, correlation_init, &
                                    correlation_read
   !
   IMPLICIT NONE

   !
   ! local variables
   !
   CHARACTER(9)    :: subname="conductor"

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
   ! Init main data
   !
   CALL write_header( stdout, "Conductor Init" )
   !
   CALL datafiles_init()
   !
   CALL smearing_init()
   !
   CALL kpoints_init( datafile_C, transport_dir )
   !
   CALL hamiltonian_init( )

   !
   ! Init correlation data and energy grid
   !
   ! If correlation is used, the energy grid is read
   ! from datafile_sgm and input parameters are overwirtten
   !
   ! otherwise, grid is built indipendently
   !
   IF ( lhave_corr ) THEN 
       !
       CALL correlation_init( sgm_unit )
       !
       IF ( .NOT. ldynam_corr ) CALL correlation_read( )
       !
   ENDIF   
   !
   IF ( .NOT. egrid_alloc ) CALL egrid_init( )

   !
   ! print data to output
   !
   CALL summary( stdout )


   !
   ! do the main task
   !
   CALL do_conductor() 

   !
   ! clean global memory
   !
   CALL cleanup()

   !
   ! finalize
   !
   CALL shutdown ( subname ) 

END PROGRAM conductor
  

!********************************************************
   SUBROUTINE do_conductor()
   !********************************************************
   !
   ! perform the main task of the calculation
   !
   USE kinds,                ONLY : dbl
   USE constants,            ONLY : PI, ZERO, CI
   USE parameters,           ONLY : nstrx 
   USE files_module,         ONLY : file_open, file_close
   USE timing_module,        ONLY : timing, timing_upto_now
   USE log_module,           ONLY : log_push, log_pop
   USE util_module,          ONLY : mat_mul, mat_inv
   USE mp_global,            ONLY : mpime, nproc
   USE mp,                   ONLY : mp_sum
   USE io_module,            ONLY : io_name, ionode, stdout, stdin, sgm_unit, &
                                    sgmL_unit => aux3_unit, sgmR_unit => aux4_unit, &
                                    work_dir, prefix, postfix, aux_unit
   USE operator_module,      ONLY : operator_write_init, operator_write_close, &
                                    operator_write_aux, operator_write_data
   USE T_control_module,     ONLY : conduct_formula, nprint, datafile_sgm,  &
                                    write_kdata, write_lead_sgm, transport_dir, &
                                    do_eigenchannels, neigchn, neigchnx, &
                                    do_eigplot, ie_eigplot, ik_eigplot
   USE T_egrid_module,       ONLY : ne, egrid
   USE T_kpoints_module,     ONLY : nkpts_par, vkpt_par3D, wk_par, ivr_par3D, &
                                    vr_par3D, nrtot_par
   USE T_hamiltonian_module, ONLY : dimL, dimR, dimC, dimx,             &
                                    blc_00L, blc_01L, blc_00R, blc_01R, &
                                    blc_00C, blc_LC,  blc_CR
   USE T_workspace_module,   ONLY : totL, tottL, totR, tottR, &
                                    gR, gL, gC, gamma_R, gamma_L, sgm_L, sgm_R, &
                                    rsgm_L, rsgm_R, workspace_allocate
   USE T_correlation_module, ONLY : lhave_corr, ldynam_corr, correlation_read
   USE T_write_data_module,  ONLY : wd_write_data, wd_write_eigchn
   USE T_operator_blc_module
   !
   IMPLICIT NONE

   !
   ! local variables
   !
   CHARACTER(12)    :: subname="do_conductor"
   !
   INTEGER          :: i, ir, ik, ierr, niter
   INTEGER          :: ie_g
   INTEGER          :: iomg_s, iomg_e
   LOGICAL          :: write_eigchn
   REAL(dbl)        :: avg_iter
   CHARACTER(4)     :: ctmp
   CHARACTER(nstrx) :: filename
   !   
   REAL(dbl),    ALLOCATABLE :: conduct_k(:,:,:), conduct(:,:)
   REAL(dbl),    ALLOCATABLE :: dos_k(:,:), dos(:), cond_aux(:)
   COMPLEX(dbl), ALLOCATABLE :: z_eigplot(:,:)
   COMPLEX(dbl), ALLOCATABLE :: work(:,:)
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
   ! local variable allocations
   !
   CALL workspace_allocate()

   ALLOCATE ( dos_k(ne,nkpts_par), STAT=ierr )
   IF( ierr /=0 ) CALL errore(subname,'allocating dos_k', ABS(ierr) )
   ALLOCATE ( dos(ne), STAT=ierr )
   IF( ierr /=0 ) CALL errore(subname,'allocating dos', ABS(ierr) )
   !
   !
   IF ( do_eigenchannels ) THEN
       !
       neigchn = MIN( dimC,dimR,dimL,  neigchnx )
       !
   ELSE
       !
       neigchn = 0
       !
   ENDIF
   !
   ALLOCATE ( conduct(1+neigchn,ne), STAT=ierr )
   IF( ierr /=0 ) CALL errore(subname,'allocating conduct', ABS(ierr) )
   !
   ALLOCATE ( conduct_k(1+neigchn, nkpts_par,ne), STAT=ierr )
   IF( ierr /=0 ) CALL errore(subname,'allocating conduct_k', ABS(ierr) )
   !
   !
   IF ( do_eigenchannels .AND. do_eigplot ) THEN
       !
       ALLOCATE ( z_eigplot(dimC, neigchn ), STAT=ierr )
       IF( ierr /=0 ) CALL errore(subname,'allocating z_eigplot', ABS(ierr) )
       !
   ENDIF
   !
   !
   ALLOCATE ( cond_aux(dimC), STAT=ierr )
   IF( ierr /=0 ) CALL errore(subname,'allocating cond_aux', ABS(ierr) )
   !
   ALLOCATE ( work(dimx,dimx), STAT=ierr )
   IF( ierr /=0 ) CALL errore(subname,'allocating work', ABS(ierr) )


!
!================================
! main loop over frequency
!================================
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
   CALL divide_et_impera( 1, ne,  iomg_s, iomg_e, mpime, nproc )

   
   !
   ! init lead sgm output files, if the case
   !
   IF ( write_lead_sgm ) THEN
       !
       CALL io_name( "sgm", filename, BODY="sgmlead_L" )
       CALL operator_write_init(sgmL_unit, filename)
       CALL operator_write_aux( sgmL_unit, dimC, .TRUE., ne, iomg_s, iomg_e, &
                                NRTOT=nrtot_par, IVR=ivr_par3D, GRID=egrid, &
                                ANALYTICITY="retarded", EUNITS="eV" )
       !
       CALL io_name( "sgm", filename, BODY="sgmlead_R" )
       CALL operator_write_init(sgmR_unit, filename)
       CALL operator_write_aux( sgmR_unit, dimC, .TRUE., ne, iomg_s, iomg_e, &
                                NRTOT=nrtot_par, IVR=ivr_par3D, GRID=egrid, &
                                ANALYTICITY="retarded", EUNITS="eV" )
       !
   ENDIF


   dos(:)           = ZERO
   dos_k(:,:)       = ZERO
   conduct(:,:)     = ZERO
   conduct_k(:,:,:) = ZERO
   !
   energy_loop: &
   DO ie_g = iomg_s, iomg_e

      ! XXX
      !ie = ie_g -iomg_s+1
      
      !
      ! grids and misc
      !
      IF ( (MOD( ie_g, nprint) == 0 .OR. ie_g == iomg_s .OR. ie_g == iomg_e ) &
           .AND. ionode ) THEN
           WRITE(stdout,"(2x, 'Computing E( ',i5,' ) = ', f12.5, ' eV' )") &
                         ie_g, egrid(ie_g)
      ENDIF


      !
      ! get correlaiton self-energy if the case
      !
      IF ( lhave_corr .AND. ldynam_corr ) THEN
          !
          CALL correlation_read( IE=ie_g )
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
          ! define aux quantities for each data block
          !
          CALL hamiltonian_setup( ik, ie_g )

 
          ! 
          !=================================== 
          ! construct leads self-energies 
          !=================================== 
          ! 

          !
          ! right lead
          CALL transfer_mtrx( dimR, blc_00R, blc_01R, totR, tottR, niter )
          avg_iter = avg_iter + REAL(niter)
          !
          CALL green( dimR, blc_00R, blc_01R, totR, tottR, gR, 1 )
          !
          CALL mat_mul(work, blc_CR%aux, 'N', gR,    'N', dimC, dimR, dimR)
          CALL mat_mul(sgm_R(:,:,ik), work, 'N', blc_CR%aux, 'C', dimC, dimC, dimR)
 
          ! 
          ! left lead 
          CALL transfer_mtrx( dimL, blc_00L, blc_01L, totL, tottL, niter )
          avg_iter = avg_iter + REAL(niter)
          !
          CALL green( dimL, blc_00L, blc_01L, totL, tottL, gL, -1 )
          !
          CALL mat_mul(work, blc_LC%aux, 'C', gL,    'N', dimC, dimL, dimL)
          CALL mat_mul(sgm_L(:,:,ik), work, 'N', blc_LC%aux, 'N', dimC, dimC, dimL) 
 
          !
          ! gamma_L & gamma_R
          !
          gamma_L(:,:) = CI * (  sgm_L(:,:,ik) - CONJG( TRANSPOSE( sgm_L(:,:,ik) ) )  )
          gamma_R(:,:) = CI * (  sgm_R(:,:,ik) - CONJG( TRANSPOSE( sgm_R(:,:,ik) ) )  )
 

          !
          !=================================== 
          ! Construct the conductor green's function
          ! gC = work^-1  (retarded)
          !=================================== 
          !
          CALL gzero_maker ( dimC, blc_00C, work(1:dimC,1:dimC), 'inverse')
          !
          work(1:dimC,1:dimC) = work(1:dimC,1:dimC) -sgm_L(:,:,ik) -sgm_R(:,:,ik)
          !
          CALL mat_inv( dimC, work, gC)


          !
          ! Compute density of states for the conductor layer
          !
          DO i = 1, dimC
             dos_k(ie_g,ik) = dos_k(ie_g,ik) - wk_par(ik) * AIMAG( gC(i,i) ) / PI
          ENDDO
          !
          dos(ie_g) = dos(ie_g) + dos_k(ie_g,ik)


          !
          !=================================== 
          ! Transmittance
          !=================================== 
          !
          ! evaluate the transmittance according to the Fisher-Lee formula
          ! or (in the correlated case) to the generalized expression as 
          ! from PRL 94, 116802 (2005)
          !
          CALL transmittance( dimC, gamma_L, gamma_R, gC, blc_00C, conduct_formula, &
                              cond_aux, do_eigenchannels, do_eigplot, work(1:dimC,1:dimC) )
          !
          ! get the total trace
          DO i=1,dimC
              !
              conduct(1,ie_g)       = conduct(1,ie_g)      + wk_par(ik) * cond_aux(i)
              conduct_k(1,ik,ie_g)  = conduct_k(1,ik,ie_g) + wk_par(ik) * cond_aux(i)
              !
          ENDDO
          !
          ! resolve over eigenchannels
          ! NOTE: the # of non-null eigenchannels is lower than MIN( dimC, dimR, dimL )
          !
          IF ( do_eigenchannels ) THEN
              !
              conduct( 2:neigchn+1, ie_g )   = conduct( 2:neigchn+1, ie_g ) &
                                                  + wk_par(ik) * cond_aux( 1:neigchn )
              !
              conduct_k(2:neigchn+1,ik,ie_g) = conduct_k(2:neigchn+1,ik,ie_g) &
                                                  + wk_par(ik) * cond_aux( 1:neigchn )
              !
          ENDIF
          !
          IF ( do_eigenchannels .AND. do_eigplot .AND. &
               ik == ik_eigplot .AND. ie_g == ie_eigplot ) THEN
              !
              z_eigplot = work( 1:dimC, 1:neigchn )
              write_eigchn = .TRUE.
              !
          ELSE
              !
              write_eigchn = .FALSE.
              !
          ENDIF
          
          !
          ! write auxiliary data for eigenchannel analysis
          !
          IF ( write_eigchn ) THEN
              !
              CALL wd_write_eigchn( aux_unit, ie_eigplot, ik_eigplot, vkpt_par3D(:,ik) , &
                                    transport_dir, dimC, neigchn, z_eigplot)
              !
          ENDIF
          ! 
      ENDDO kpt_loop 

      !
      ! write massive data for lead sgm if the case
      !
      IF ( write_lead_sgm ) THEN
          !
          DO ir = 1, nrtot_par
              !
              CALL compute_rham(dimC, vr_par3D(:,ir), rsgm_L(:,:,ir), nkpts_par, vkpt_par3D, wk_par, sgm_L)
              CALL compute_rham(dimC, vr_par3D(:,ir), rsgm_R(:,:,ir), nkpts_par, vkpt_par3D, wk_par, sgm_R)
              !
          ENDDO
          !
          CALL operator_write_data( sgmL_unit, rsgm_L, .TRUE., ie_g )
          CALL operator_write_data( sgmR_unit, rsgm_R, .TRUE., ie_g )
          !
      ENDIF
 
      !
      ! report to stdout
      !
      avg_iter = avg_iter/REAL(2*nkpts_par)
      !
      IF ( MOD( ie_g, nprint) == 0 .OR.  ie_g == iomg_s .OR. ie_g == iomg_e ) THEN
          !
          IF ( ionode ) WRITE(stdout,"(2x,'T matrix converged after avg. # of iterations ',&
                                      & f8.3,/)") avg_iter
          !
          CALL timing_upto_now(stdout)
          !
      ENDIF
      !
      CALL flush_unit(stdout)

   ENDDO energy_loop

   !
   ! recover over frequencies
   !
   CALL mp_sum( dos )
   CALL mp_sum( dos_k )
   CALL mp_sum( conduct )
   CALL mp_sum( conduct_k )

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
   ! close lead sgm output files
   !
   IF ( write_lead_sgm ) THEN
       !
       CALL operator_write_close(sgmL_unit)
       CALL operator_write_close(sgmR_unit)
       !
   ENDIF

   !
   ! write DOS and CONDUCT data on files
   !
   CALL write_header( stdout, "Writing data" )
   CALL flush_unit( stdout )
   ! 
   CALL wd_write_data(aux_unit, ne, egrid, SIZE(conduct,1), conduct, 'conductance' ) 
   !
   CALL wd_write_data(aux_unit, ne, egrid, 1, dos, 'doscond' ) 
                            
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
         DO ie_g = 1, ne
             WRITE( aux_unit, '(2000(f15.9))') egrid(ie_g), conduct_k(:,ik,ie_g) 
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
         DO ie_g = 1, ne
             WRITE( aux_unit, '(2(f15.9))') egrid(ie_g), dos_k(ie_g,ik) 
         ENDDO
         !
         CLOSE( aux_unit )         
         !
      ENDDO
      !
   ENDIF
   

!
! ... shutdown
!

   !
   ! clean local memory
   !
   DEALLOCATE ( dos, STAT=ierr )
   IF( ierr /=0 ) CALL errore(subname,'deallocating dos', ABS(ierr) )
   !
   DEALLOCATE ( conduct_k, conduct, STAT=ierr )
   IF( ierr /=0 ) CALL errore(subname,'deallocating conduct_k, conduct', ABS(ierr) )
   !
   DEALLOCATE ( cond_aux, STAT=ierr )
   IF( ierr /=0 ) CALL errore(subname,'deallocating cond_aux', ABS(ierr) )
   !
   DEALLOCATE ( work, STAT=ierr )
   IF( ierr /=0 ) CALL errore(subname,'deallocating work', ABS(ierr) )
   !
   IF ( do_eigenchannels .AND. do_eigplot ) THEN
       !
       DEALLOCATE ( z_eigplot, STAT=ierr )
       IF( ierr /=0 ) CALL errore(subname,'deallocating z_eigplot', ABS(ierr) )
       !
   ENDIF


   CALL timing(subname,OPR='stop')
   CALL log_pop(subname)
      !
END SUBROUTINE do_conductor
   

