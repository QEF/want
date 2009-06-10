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
   USE T_input_module,       ONLY : input_manager
   USE T_control_module,     ONLY : conduct_formula, nprint, datafile_sgm,  &
                                    write_kdata, write_lead_sgm, do_eigenchannels, &
                                    datafile_C, transport_dir
   USE T_egrid_module,       ONLY : egrid_init, ne, egrid, egrid_alloc => alloc
   USE T_smearing_module,    ONLY : smearing_init
   USE T_kpoints_module,     ONLY : kpoints_init, nkpts_par, vkpt_par3D, wk_par, ivr_par3D, vr_par3D, nrtot_par
   USE T_hamiltonian_module, ONLY : dimL, dimR, dimC, dimx,             &
                                    blc_00L, blc_01L, blc_00R, blc_01R, &
                                    blc_00C, blc_LC,  blc_CR
                                    
   USE T_workspace_module,   ONLY : totL, tottL, totR, tottR, &
                                    gR, gL, gC, gamma_R, gamma_L, sgm_L, sgm_R, &
                                    rsgm_L, rsgm_R, workspace_allocate
   USE T_correlation_module, ONLY : lhave_corr, ldynam_corr, correlation_init, &
                                    correlation_read
   USE T_datafiles_module,   ONLY : datafiles_init
   USE T_operator_blc_module
   !
   IMPLICIT NONE

   !
   ! local variables
   !
   CHARACTER(9)     :: subname='conductor'
   !
   CHARACTER(nstrx) :: filename
   INTEGER          :: i, ie, ir, ik, ierr, niter
   INTEGER          :: iomg_s, iomg_e
   REAL(dbl)        :: avg_iter
   CHARACTER(4)     :: ctmp, str
   !   
   REAL(dbl),    ALLOCATABLE :: conduct_k(:,:), conduct(:,:)
   REAL(dbl),    ALLOCATABLE :: dos_k(:,:), dos(:), cond_aux(:)
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
   CALL kpoints_init( datafile_C, transport_dir )

   !
   ! Set up the layer hamiltonians
   !
   CALL hamiltonian_init( )


   !
   ! Setup correlation data and energy grids
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
   conduct_k(:,:)   = ZERO
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
             dos_k(ie,ik) = dos_k(ie,ik) - wk_par(ik) * AIMAG( gC(i,i) ) / PI
          ENDDO
          !
          dos(ie) = dos(ie) + dos_k(ie,ik)


          !
          !=================================== 
          ! Transmittance
          !=================================== 
          !
          ! evaluate the transmittance according to the Fisher-Lee formula
          ! or (in the correlated case) to the generalized expression as 
          ! from PRL 94, 116802 (2005)
          !
          CALL transmittance( cond_aux, dimC, gamma_L, gamma_R, gC, blc_00C, &
                              TRIM(conduct_formula), do_eigenchannels )
          !
          ! get the total trace
          DO i=1,dimC
              !
              conduct(ie,1)    = conduct(ie,1)    + wk_par(ik) * cond_aux(i)
              conduct_k(ie,ik) = conduct_k(ie,ik) + wk_par(ik) * cond_aux(i)
              !
          ENDDO
          !
          ! resolve over eigenchannels
          IF ( do_eigenchannels ) THEN
              !
              conduct(ie,2:dimC+1) = conduct(ie,2:dimC+1) + wk_par(ik) * cond_aux(1:dimC)
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
          CALL operator_write_data( sgmL_unit, rsgm_L, .TRUE., ie )
          CALL operator_write_data( sgmR_unit, rsgm_R, .TRUE., ie )
          !
      ENDIF
 
      !
      ! report to stdout
      !
      avg_iter = avg_iter/REAL(2*nkpts_par)
      !
      IF ( MOD( ie, nprint) == 0 .OR.  ie == iomg_s .OR. ie == iomg_e ) THEN
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

   IF ( ionode ) THEN
       !
       CALL write_header( stdout, "Writing data" )
       CALL flush_unit( stdout )
   

       CALL io_name( "conductance", filename )
       !
       OPEN ( cond_unit, FILE=TRIM(filename), FORM='formatted' )
       !
       str = TRIM( int2char(dimC+2))
       DO ie = 1, ne
           WRITE ( cond_unit, '('//TRIM(str)//'(f15.9))' ) egrid(ie), conduct(ie,:)
       ENDDO
       !
       CLOSE( cond_unit )
       !
       CALL io_name( "conductance", filename, LPATH=.FALSE. )
       WRITE(stdout,"(/,2x,'Conductance written on file: ',3x,a)") TRIM(filename)
                            

       CALL io_name( "doscond", filename )
       !
       OPEN ( dos_unit, FILE=TRIM(filename), FORM='formatted' )
       !
       DO ie = 1, ne
           WRITE ( dos_unit, '(2(f15.9))' ) egrid(ie), dos(ie)
       ENDDO
       !
       CLOSE( dos_unit )
       !
       CALL io_name( "doscond", filename, LPATH=.FALSE. )
       WRITE(stdout,"(  2x,'        DOS written on file: ',3x,a)") TRIM(filename)
       ! 
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
              WRITE( aux_unit, '(2(f15.9))') egrid(ie), conduct_k(ie,ik) 
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
             WRITE( aux_unit, '(2(f15.9))') egrid(ie), dos_k(ie,ik) 
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
   DEALLOCATE ( conduct_k, conduct, STAT=ierr )
   IF( ierr /=0 ) CALL errore(subname,'deallocating conduct_k, conduct', ABS(ierr) )
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
  
