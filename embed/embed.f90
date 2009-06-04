!
!      Copyright (C) 2009 AF & DP
!
!      This file is distributed under the terms of the
!      GNU General Public License. See the file `License'
!      in the root directory of the present distribution,
!      or http://www.gnu.org/copyleft/gpl.txt .
!
!***********************************************
   PROGRAM embed
   !***********************************************
   !
   USE iotk_module
   USE kinds,                ONLY : dbl
   USE constants,            ONLY : PI, ZERO, CZERO, CONE, CI, EPS_m5
   USE parameters,           ONLY : nstrx 
   USE version_module,       ONLY : version_number
   USE parser_module,        ONLY : change_case, int2char
   USE files_module,         ONLY : file_open, file_close
   USE timing_module,        ONLY : timing, timing_upto_now
   USE util_module,          ONLY : mat_mul, mat_inv
   USE mp_global,            ONLY : mpime, nproc
   USE mp,                   ONLY : mp_sum
   USE io_module,            ONLY : io_name, ionode, stdout, stdin, sgm_unit, &
                                    dos_unit => aux1_unit, cond_unit => aux2_unit, &
                                    sgmL_unit => aux3_unit, sgmR_unit => aux4_unit, &
                                    work_dir, prefix, postfix, aux_unit
   USE operator_module,      ONLY : operator_write_init, operator_write_close, &
                                    operator_write_aux, operator_write_data
   USE T_egrid_module,       ONLY : egrid_init, ne, egrid, egrid_alloc => alloc
   USE T_kpoints_module,     ONLY : kpoints_init, nkpts_par, vkpt_par, wk_par, vr_par, nrtot_par
   USE T_smearing_module,    ONLY : smearing_init
   USE T_operator_blc_module
   !
   USE E_input_module,       ONLY : input_manager
   USE E_control_module,     ONLY : nprint, datafile_C, datafile_sgm, datafile_sgm_emb, transport_dir
   USE E_hamiltonian_module, ONLY : dimC, dim_emb, blc_C, blc_emb
   USE E_correlation_module, ONLY : lhave_corr, ldynam_corr, correlation_init, correlation_read
   USE E_datafiles_module,   ONLY : datafiles_init
                                    
!   USE T_workspace_module,   ONLY : totL, tottL, totR, tottR, &
!                                    gR, gL, gC, gamma_R, gamma_L, sgm_L, sgm_R, &
!                                    rsgm_L, rsgm_R, workspace_allocate
   !
   IMPLICIT NONE

   !
   ! local variables
   !
   CHARACTER(5)     :: subname='embed'
   !
   CHARACTER(nstrx) :: filename
   INTEGER          :: i, ie, ir, ik, ierr, niter
   INTEGER          :: iomg_s, iomg_e
   REAL(dbl)        :: avg_iter
   CHARACTER(4)     :: ctmp, str
   !   
   REAL(dbl),    ALLOCATABLE :: dos(:)

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
   CALL kpoints_init( datafile_C, transport_dir )

   !
   ! Set up the layer hamiltonians
   !
   CALL hamiltonian_init()


   !
   ! otherwise, grid is built indipendently
   !
   IF ( lhave_corr ) THEN 
       !
       CALL correlation_init( sgm_unit )

       !
       ! Read correlation data if not dynamical
       !
       IF ( .NOT. ldynam_corr ) THEN
           !
           CALL correlation_read( )
           !
       ENDIF
       !
   ENDIF   
   !
   !
   IF ( .NOT. egrid_alloc ) THEN
       !
       CALL egrid_init( )
       !
   ENDIF

   !
   ! write input data on the output file
   !
   IF (ionode) CALL summary( stdout )

#ifdef __CULO

   !
   ! local variable allocations
   !
   CALL workspace_allocate()

   ALLOCATE ( dos_k(ne,nkpts_par), STAT=ierr )
   IF( ierr /=0 ) CALL errore(subname,'allocating dos_k', ABS(ierr) )
   ALLOCATE ( dos(ne), STAT=ierr )
   IF( ierr /=0 ) CALL errore(subname,'allocating dos', ABS(ierr) )
   !
   ALLOCATE ( conduct_k(ne,nkpts_par), STAT=ierr )
   IF( ierr /=0 ) CALL errore(subname,'allocating conduct_k', ABS(ierr) )
   !
   IF ( do_eigenchannels ) THEN
       ALLOCATE ( conduct(ne,1+dimC), STAT=ierr )
   ELSE
       ALLOCATE ( conduct(ne,1), STAT=ierr )
   ENDIF
   IF( ierr /=0 ) CALL errore(subname,'allocating conduct', ABS(ierr) )
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
   ! init lead sgm output files, if the case
   !
   IF ( write_lead_sgm ) THEN
       !
       CALL io_name( "sgm", filename, BODY="sgmlead_L" )
       CALL operator_write_init(sgmL_unit, filename)
       CALL operator_write_aux( sgmL_unit, dimC, .TRUE., ne, iomg_s, iomg_e, &
                                NRTOT=nrtot_par, VR=vr_par, GRID=egrid, &
                                ANALYTICITY="retarded", EUNITS="eV" )
       !
       CALL io_name( "sgm", filename, BODY="sgmlead_R" )
       CALL operator_write_init(sgmR_unit, filename)
       CALL operator_write_aux( sgmR_unit, dimC, .TRUE., ne, iomg_s, iomg_e, &
                                NRTOT=nrtot_par, VR=vr_par, GRID=egrid, &
                                ANALYTICITY="retarded", EUNITS="eV" )
       !
   ENDIF


   dos(:)           = ZERO
   !
   energy_loop: &
   DO ie = iomg_s, iomg_e
      
      !
      ! grids and misc
      !
      IF ( (MOD( ie, nprint) == 0 .OR. ie == iomg_s .OR. ie == iomg_e ) &
           .AND. ionode ) THEN
           WRITE(stdout,"(2x, 'Computing E( ',i5,' ) = ', f12.5, ' eV' )") &
                         ie, egrid(ie)
      ENDIF


      !
      ! get correlaiton self-energy if the case
      !
      IF ( lhave_corr .AND. ldynam_corr ) THEN
          !
          CALL correlation_read( IE=ie )
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
          CALL hamiltonian_setup( ik, ie )

 
          ! 
          !=================================== 
          ! construct the embedding sgm
          !=================================== 
          ! 

 

          !
          CALL mat_inv( dimC, work, gC)


          ! 
          !=================================== 
          ! Verify data
          !=================================== 
          !
          ! Compute density of states for the conductor layer
          !
          DO i = 1, dimC
             dos_k(ie,ik) = dos_k(ie,ik) - wk_par(ik) * AIMAG( gC(i,i) ) / PI
          ENDDO
          !
          dos(ie) = dos(ie) + dos_k(ie,ik)


          ! 
      ENDDO kpt_loop 

      !
      ! write SGM and H_reduced to file
      !
      DO ir = 1, nrtot_par
           !
           CALL compute_rham(dimC, vr_par(:,ir), rsgm_L(:,:,ir), nkpts_par, vkpt_par, wk_par, sgm_L)
           CALL compute_rham(dimC, vr_par(:,ir), rsgm_R(:,:,ir), nkpts_par, vkpt_par, wk_par, sgm_R)
           !
      ENDDO
      !
      CALL operator_write_data( sgmL_unit, rsgm_L, .TRUE., ie )
      CALL operator_write_data( sgmR_unit, rsgm_R, .TRUE., ie )
 
      !
      CALL flush_unit(stdout)

   ENDDO energy_loop


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
!...  shutdown
!

   !
   ! clean local memory
   !
   DEALLOCATE ( dos, STAT=ierr )
   IF( ierr /=0 ) CALL errore(subname,'deallocating dos', ABS(ierr) )
   !
   DEALLOCATE ( work, STAT=ierr )
   IF( ierr /=0 ) CALL errore(subname,'deallocating work', ABS(ierr) )

#endif

   !
   ! clean global memory
   !
   CALL cleanup()

   !
   ! finalize
   !
   CALL shutdown ( subname ) 

END PROGRAM embed
  
