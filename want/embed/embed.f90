!
! Copyright (C) 2009 WanT Group
!
! This file is distributed under the terms of the
! GNU General Public License. See the file `License'
! in the root directory of the present distribution,
! or http://www.gnu.org/copyleft/gpl.txt .
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
                                    dos_unit => aux1_unit, sgmB_unit => aux3_unit, &
                                    work_dir, prefix, postfix, aux_unit
   USE operator_module,      ONLY : operator_write_init, operator_write_close, &
                                    operator_write_aux, operator_write_data
   USE T_egrid_module,       ONLY : egrid_init, ne, egrid, egrid_alloc => alloc
   USE T_kpoints_module,     ONLY : kpoints_init, nkpts_par, vkpt_par, wk_par, vr_par, nrtot_par
   USE T_smearing_module,    ONLY : smearing_init
   USE T_operator_blc_module
   !
   USE E_input_module,       ONLY : input_manager
   USE E_control_module,     ONLY : nprint, datafile_tot, datafile_sgm, datafile_sgm_emb, transport_dir
   USE E_hamiltonian_module, ONLY : dimx, dimT, dimE, dimB, blc_T, blc_E, blc_B, blc_EB
   USE E_correlation_module, ONLY : lhave_corr, ldynam_corr, correlation_init, correlation_read
   USE E_datafiles_module,   ONLY : datafiles_init
   USE E_workspace_module,   ONLY : gB, gE, sgm_B, rsgm_B, workspace_allocate
   !
   IMPLICIT NONE

   !
   ! local variables
   !
   CHARACTER(5)     :: subname='embed'
   !
   INTEGER          :: i, ie, ir, ik, ierr
   INTEGER          :: iomg_s, iomg_e
   !   
   REAL(dbl),    ALLOCATABLE :: dos(:)
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
   CALL kpoints_init( datafile_tot, transport_dir )

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


   !
   ! local variable allocations
   !
   CALL workspace_allocate()

   ALLOCATE ( dos(ne), STAT=ierr )
   IF( ierr /=0 ) CALL errore(subname,'allocating dos', ABS(ierr) )
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
   ! init embedding sgm output
   !
   CALL operator_write_init(sgmB_unit, TRIM(datafile_sgm_emb) )
   CALL operator_write_aux( sgmB_unit, dimE, .TRUE., ne, iomg_s, iomg_e, &
                            NRTOT=nrtot_par, VR=vr_par, GRID=egrid, &
                            ANALYTICITY="retarded", EUNITS="eV" )


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
      ! get correlation self-energy if the case
      !
      IF ( lhave_corr .AND. ldynam_corr ) THEN
          !
          CALL correlation_read( IE=ie )
          !
      ENDIF


      !
      ! main kpt loop
      !
      kpt_loop: &
      DO ik = 1, nkpts_par

          !
          ! define aux quantities for each data block
          !
          CALL hamiltonian_setup( ik, ie )

 
          !
          !=================================== 
          ! Construct the bath (B) green's function
          ! gB = (omg -H_B +id )^-1  (retarded)
          !=================================== 
          !
          CALL gzero_maker ( dimB, blc_B, gB, 'direct')
          

          ! 
          !=================================== 
          ! construct the embedding sgm
          !=================================== 
          ! 
          ! work = (omg S -H_EB ) * gB
          CALL mat_mul( work(1:dimE, 1:dimB), blc_EB%aux, 'N', gB, 'N', dimE, dimB, dimB )
          ! 
          ! sgmB = (omg S -H_EB ) * gB * (omg S -H_EB )^dag = work * (omg S -H_EB )^dag 
          CALL mat_mul( sgm_B(:,:,ik), work(1:dimE, 1:dimB), 'N', blc_EB%aux, 'C', dimE, dimE, dimB )
 

          ! 
          !=================================== 
          ! construct GF of the embedded region
          ! gE = (omg -H_E -sgmB )^-1  (retarded)
          !=================================== 
          !
          CALL gzero_maker ( dimE, blc_E, work(1:dimE, 1:dimE), 'inverse')
          !
          work(1:dimE, 1:dimE) = work(1:dimE, 1:dimE) -sgm_B(:,:,ik)
          !
          CALL mat_inv( dimE, work(1:dimE, 1:dimE), gE)


          ! 
          !=================================== 
          ! Verify data
          !=================================== 
          !
          ! Compute density of states for the conductor layer
          !
          DO i = 1, dimE
             dos(ie) = dos(ie) - wk_par(ik) * AIMAG( gE(i,i) ) / PI
          ENDDO
          ! 
      ENDDO kpt_loop 

      !
      ! write sgm_B to file
      !
      DO ir = 1, nrtot_par
          !
          CALL compute_rham(dimE, vr_par(:,ir), rsgm_B(:,:,ir), nkpts_par, vkpt_par, wk_par, sgm_B)
          !
      ENDDO
      !
      CALL operator_write_data( sgmB_unit, rsgm_B, .TRUE., ie )
 
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
   CALL operator_write_close(sgmB_unit)



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


   !
   ! clean global memory
   !
   CALL cleanup()

   !
   ! finalize
   !
   CALL shutdown ( subname ) 

END PROGRAM embed
  
