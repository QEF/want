
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
                                    aux_unit
   USE operator_module,      ONLY : operator_write_init, operator_write_close, &
                                    operator_write_aux, operator_write_data
   USE T_egrid_module,       ONLY : egrid_init, ne, egrid, egrid_alloc => alloc, &
                                    ne_buffer, egrid_buffer_doread, egrid_buffer_iend
   USE T_kpoints_module,     ONLY : kpoints_init, nkpts_par, vkpt_par3D, wk_par, ivr_par3D, vr_par3D, nrtot_par
   USE T_smearing_module,    ONLY : smearing_init, smearing_type_null
   USE T_operator_blc_module
   !
   USE E_input_module,       ONLY : input_manager
   USE E_control_module,     ONLY : nprint, datafile_tot, datafile_sgm, &
                                    datafile_sgm_emb, write_embed_sgm, transport_dir
   USE E_hamiltonian_module, ONLY : dimx, dimT, dimE, dimB, blc_T, blc_E, blc_B, blc_EB, blc_BE
   USE E_correlation_module, ONLY : lhave_corr, ldynam_corr, correlation_init, correlation_read
   USE E_datafiles_module,   ONLY : datafiles_init
   USE E_workspace_module,   ONLY : gB, gE, gT, sgm_B, rsgm_B, workspace_allocate
   !
   IMPLICIT NONE

   !
   ! local variables
   !
   CHARACTER(5)     :: subname='embed'
   CHARACTER(256)   :: filename
   !
   INTEGER          :: i, ie_g, ir, ik, ierr
   INTEGER          :: iomg_s, iomg_e
   INTEGER          :: ie_buff, ie_buff_s, ie_buff_e
   LOGICAL          :: read_buffer
   !   
   REAL(dbl),    ALLOCATABLE :: dos_E(:), gamma_E(:), dos_T(:)
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

   ALLOCATE ( dos_E(ne), gamma_E(ne), dos_T(ne), STAT=ierr )
   IF( ierr /=0 ) CALL errore(subname,'allocating dos_E, gamma_E, dos_T', ABS(ierr) )
   !
   ALLOCATE ( work(dimx,dimx), STAT=ierr )
   IF( ierr /=0 ) CALL errore(subname,'allocating work', ABS(ierr) )

   !
   ! memory usage
   !
   IF ( ionode ) WRITE( stdout, "()" )
   CALL memusage( stdout )
   !
   CALL flush_unit( stdout )


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
   ! init embedding sgm output
   !
   CALL io_name( "free", filename, lpostfix=.FALSE., lbody=.TRUE., &
                 body=TRIM(datafile_sgm_emb), lpath=.FALSE., lproc=.TRUE. )
   !
   CALL operator_write_init(sgmB_unit, TRIM(filename) )
   CALL operator_write_aux( sgmB_unit, dimE, .TRUE., ne, iomg_s, iomg_e, &
                            NRTOT=nrtot_par, IVR=ivr_par3D,  GRID=egrid, &
                            ANALYTICITY="retarded", EUNITS="eV" )


   dos_E(:)   = ZERO
   dos_T(:)   = ZERO
   gamma_E(:) = ZERO
   !
   ie_buff = 1
   !
   energy_loop: &
   DO ie_g = iomg_s, iomg_e
      
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
          read_buffer = egrid_buffer_doread ( ie_g, iomg_s, iomg_e, ne_buffer )
          !
          IF ( read_buffer ) THEN
              !
              ie_buff_s = ie_g
              ie_buff_e = egrid_buffer_iend( ie_g, iomg_s, iomg_e, ne_buffer )
              !
              CALL correlation_read( IE_S=ie_buff_s, IE_E=ie_buff_e )
              !
              ie_buff = 1
              !
          ELSE
              !
              ie_buff = ie_buff +1
              !
          ENDIF
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
          CALL hamiltonian_setup( ik, ie_g, ie_buff )


          !
          !=================================== 
          ! Construct the bath (B) green's function
          ! gB = (omg -H_B +id )^-1  (retarded)
          !=================================== 
          !
          IF ( dimB > 0 ) THEN
              !
              CALL gzero_maker ( dimB, blc_B, dimB, gB, 'direct', smearing_type_null )
              !
          ENDIF

          ! 
          !=================================== 
          ! construct the embedding sgm
          !=================================== 
          ! 
          ! work = (omg S_EB -H_EB ) * gB
          IF ( dimB > 0 ) THEN
              !
              CALL mat_mul( work, blc_EB%aux, 'N', gB, 'N', dimE, dimB, dimB )
              !
          ENDIF

          ! 
          ! sgmB = (omg S_EB -H_EB ) * gB * (omg S -H_BE ) 
          !
          ! note that we do not use EB^dag because of the possiblity to have non-hermitean
          ! self-energies built into the hamiltonians
          !
          IF ( dimB > 0 ) THEN
              !
              CALL mat_mul( sgm_B(:,:,ik), work, 'N', blc_BE%aux, 'N', dimE, dimE, dimB )
              !
          ELSE
              !
              sgm_B(:,:,ik) = CZERO 
              !
          ENDIF


          ! 
          !=================================== 
          ! construct GF of the embedded region
          ! gE = (omg -H_E -sgmB )^-1  (retarded)
          !=================================== 
          !
          work = CZERO
          CALL gzero_maker ( dimE, blc_E, dimx, work, 'inverse', smearing_type_null )
          !
          work(1:dimE, 1:dimE) = work(1:dimE, 1:dimE) -sgm_B(:,:,ik)
          !
          CALL mat_inv( dimE, work, gE)
          ! 
          ! Compute density of states for the conductor layer
          !
          DO i = 1, dimE
              dos_E(ie_g)   = dos_E(ie_g)   - wk_par(ik) * AIMAG( gE(i,i) ) / PI
              gamma_E(ie_g) = gamma_E(ie_g) + wk_par(ik) * AIMAG( work(i, i) -sgm_B(i,i,ik) )  
          ENDDO


          ! 
          !=================================== 
          ! construct the overall GF and compute the total DOS
          ! gT = (omg -H_T )^-1  (retarded)
          !=================================== 
          !
          CALL gzero_maker ( dimT, blc_T, dimT, gT, 'direct', smearing_type_null )
          ! 
          DO i = 1, dimT
              dos_T(ie_g) = dos_T(ie_g) - wk_par(ik) * AIMAG( gT(i,i) ) / PI
          ENDDO
          ! 
      ENDDO kpt_loop 


      !
      ! write sgm_B to file
      !
      IF ( write_embed_sgm ) THEN
          !
          DO ir = 1, nrtot_par
              !
              CALL compute_rham(dimE, vr_par3D(:,ir), rsgm_B(:,:,ir), nkpts_par, vkpt_par3D, wk_par, sgm_B)
              !
          ENDDO 
          !
          CALL operator_write_data( sgmB_unit, rsgm_B, .TRUE., ie_g )
          !
      ENDIF
 
      !
      CALL flush_unit(stdout)

      !
      ! time report
      !
      IF ( MOD( ie_g, nprint) == 0 .OR.  ie_g == iomg_s .OR. ie_g == iomg_e ) THEN
          !
          IF ( ionode ) WRITE(stdout,"()")
          CALL timing_upto_now(stdout)
          !
      ENDIF

   ENDDO energy_loop

   !
   ! recover over frequencies
   !
   CALL mp_sum( dos_E )
   CALL mp_sum( dos_T )
   CALL mp_sum( gamma_E )


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
! write DOS data to file
!

   IF ( ionode ) THEN
       !
       CALL write_header( stdout, "Writing data" )
       CALL flush_unit( stdout )

       CALL io_name( "dos", filename )
       !
       OPEN ( dos_unit, FILE=TRIM(filename), FORM='formatted' )
       !
       WRITE( dos_unit, "('# Energy [eV]    dos_E    gamma_E    dos_T')") 
       DO ie_g = 1, ne
           WRITE ( dos_unit, '(4(f15.9))' ) egrid(ie_g), dos_E(ie_g), gamma_E(ie_g), dos_T(ie_g)
       ENDDO
       !
       CLOSE( dos_unit )
       !
       CALL io_name( "dos", filename, LPATH=.FALSE. )
       WRITE(stdout,"(  2x,'        DOS written on file: ',3x,a)") TRIM(filename)
       ! 
   ENDIF



!
!...  shutdown
!

   !
   ! clean local memory
   !
   DEALLOCATE ( dos_E, gamma_E, dos_T, STAT=ierr )
   IF( ierr /=0 ) CALL errore(subname,'deallocating dos_E, gamma_E, dos_T', ABS(ierr) )
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
  
