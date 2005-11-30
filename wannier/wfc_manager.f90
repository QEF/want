! 
! Copyright (C) 2004 WanT Group
! 
! This file is distributed under the terms of the 
! GNU General Public License. See the file `License' 
! in the root directory of the present distribution, 
! or http://www.gnu.org/copyleft/gpl.txt . 
! 
!*********************************************************
   SUBROUTINE wfc_manager()
   !*********************************************************
   !
   ! This subroutine permforms all the
   ! operations needed to obtain the internal data from DFT wfcs
   ! (OVERLAP and PROJECTIONS matrix elements).
   !
   ! Tasks performed:
   ! * read and init G grids
   ! * compute the overlaps (reading the needed wfcs)
   ! * compute the projections (reading the needed wfcs)
   ! * write overlap and projections to file
   ! * waste ggrids and wfcs data
   !
   USE kinds
   USE parameters,     ONLY : nstrx
   USE constants,      ONLY : CZERO, ZERO
   USE iotk_module
   USE io_module,      ONLY : stdout, dft_unit, ovp_unit, ioname
   USE timing_module,  ONLY : timing, timing_upto_now
   USE files_module,   ONLY : file_open, file_close
   
   USE control_module, ONLY : do_overlaps, do_projections, &
                              use_atomwfc, use_pseudo, use_uspp, use_blimit, &
                              read_overlaps, read_projections
   USE lattice_module, ONLY : tpiba
   USE subspace_module,ONLY : dimwann
   USE trial_center_data_module,   ONLY : trial
   USE windows_module, ONLY : windows_alloc => alloc, dimwin, dimwinx, imin, imax
   USE kpoints_module, ONLY : kpoints_alloc, bshells_alloc, nkpts, vkpt, &
                              nb, nnlist, nncell, nnrev, nnpos
   USE overlap_module, ONLY : Mkb, ca, overlap_alloc => alloc, overlap_write, overlap_read
   USE ggrids_module,  ONLY : nfft, npw_rho, ecutwfc, ecutrho, &
                              ggrids_read_ext, ggrids_deallocate
   USE wfc_data_module,ONLY : npwkx, npwk, igsort, evc, evc_info, &
                              wfc_data_grids_read, wfc_data_kread, wfc_data_deallocate 
   USE wfc_info_module
   USE struct_fact_data_module, ONLY : struct_fact_data_init
   USE uspp,           ONLY : nkb, vkb, vkb_ik
   USE becmod,         ONLY : becp

   !
   ! few local variables
   !
   IMPLICIT NONE
      CHARACTER(11)             :: subname="wfc_manager"
      CHARACTER(nstrx)          :: filename
      REAL(dbl)                 :: xk(3)
      COMPLEX(dbl), ALLOCATABLE :: aux(:,:)
      LOGICAL                   :: lfound
      INTEGER                   :: ib, ikb, ik, inn
      INTEGER                   :: indin, indout, index
      INTEGER                   :: ierr


      CALL timing('wfc_manager',OPR='start')

!
! Some checks
!

      IF ( .NOT. kpoints_alloc) CALL errore(subname,'kpoints NOT alloc',2) 
      IF ( .NOT. bshells_alloc) CALL errore(subname,'bshells NOT alloc',3) 
      IF ( .NOT. windows_alloc) CALL errore(subname,'windows NOT alloc',4) 
      IF ( .NOT. overlap_alloc) CALL errore(subname,'overlap NOT alloc',5) 

!
! Here compute the neede quantites 
! (either overlaps or projections or both)
!
      IF ( do_overlaps .OR. do_projections ) THEN


          !
          ! ... opening the file containing the PW-DFT data
          !
          CALL ioname('export',filename,LPOSTFIX=.FALSE.)
          CALL file_open(dft_unit,TRIM(filename),PATH="/",ACTION="read", &
                                  FORM='formatted')
          CALL ioname('export',filename,LPATH=.FALSE.,LPOSTFIX=.FALSE.)

          !
          ! ... Read grids
          WRITE( stdout,"(/,2x,'Reading density G-grid from file: ',a)") TRIM(filename)
          CALL ggrids_read_ext(dft_unit)

          !
          ! ... Read wfcs
          WRITE( stdout,"(  2x,'Reading Wfc grids from file: ',a)") TRIM(filename)
          CALL wfc_data_grids_read(dft_unit)

          !
          ! ... closing the main data file
          CALL file_close(dft_unit,PATH="/",ACTION="read")


          !
          ! ... stdout summary about grids
          !
          WRITE(stdout,"(2/,10x,'Energy cut-off for wfcs =  ',5x,F7.2,' (Ry)' )") ecutwfc
          WRITE(stdout, "(25x,'for rho  =  ', 5x, F7.2, ' (Ry)' )")  ecutrho
          WRITE(stdout, "(6x,'Total number of PW for rho  =  ',i9)") npw_rho
          WRITE(stdout, "(6x,'  Max number of PW for wfc  =  ',i9)") npwkx
          WRITE(stdout, "(6x,'Total number of PW for wfcs =  ',i9)") MAXVAL(igsort(:,:))+1
          WRITE(stdout, "(6x,'  FFT grid components (rho) =  ( ', 3i5,' )' )") nfft(:)
          WRITE(stdout, "()")

    
          !
          ! ... if pseudo are used do the required allocations
          !
          IF ( use_pseudo ) THEN
              WRITE( stdout,"(/,2x,'Initializing global dft data')")
              !
              ! ... data required by USPP and atomic WFC
              CALL allocate_nlpot()
              !
              ! ... structure factors 
              CALL struct_fact_data_init()
          ENDIF

          ! 
          ! ... if USPP are used, initialize the related quantities
          !
          IF ( use_uspp ) THEN
              IF ( .NOT. use_pseudo ) CALL errore(subname,'pseudo should be used',4)
              WRITE( stdout,"(/,2x,'Initializing US pseudopot. data')")
              
              !
              ! ... first initialization
              !     here we compute (among other quantities) \int dr Q_ij(r)
              !                                              \int dr e^ibr Q_ij(r)
              CALL init_us_1()
              IF ( use_blimit ) &
                 WRITE( stdout, '(2x, "WARNING: setting b = 0 in qb (overlap augment.)" )')
                 !
              WRITE( stdout, '(2x, "Total number Nkb of beta functions: ",i5,2/ ) ') nkb

              !
              ! ... space for beta functions in reciproc space within struct_facts
              ! 
              IF ( nkb <= 0 ) CALL errore(subname,'no beta functions within USPP',-nkb+1)
              ALLOCATE( becp(nkb, dimwinx, nkpts), STAT=ierr )
                  IF (ierr/=0) CALL errore(subname,'allocating becp',ABS(ierr))

          ENDIF

          !
          ! ... if ATOMWFC are used
          !
          IF ( use_atomwfc ) THEN
              IF ( .NOT. use_pseudo ) CALL errore(subname,'pseudo should be used',5)
              WRITE( stdout,"(/,2x,'Initializing atomic wfc')")
              !
              ! ... atomic init
              CALL init_at_1()
          ENDIF



          !
          ! we need anyway to have becp allocated even if it is not used
          ! becp(i,j,ik) = <beta_i |psi_j_ik >
          !
          IF ( .NOT. ALLOCATED(becp) ) THEN
               ALLOCATE( becp(1,1,nkpts), STAT=ierr )
               IF (ierr/=0) CALL errore(subname,'allocating becp',ABS(ierr))
          ENDIF
          !
          ! local workspace
          ALLOCATE( aux(dimwinx, dimwinx), STAT=ierr )
             IF (ierr/=0) CALL errore(subname,'allocating aux',ABS(ierr))

          !
          ! ... allocating wfcs
          CALL wfc_info_allocate(npwkx, dimwinx, nkpts, 2*dimwinx, evc_info)
          ALLOCATE( evc(npwkx, 2*dimwinx ), STAT=ierr )
             IF (ierr/=0) CALL errore(subname,'allocating EVC',ABS(ierr))

          !
          ! ... re-open the main data file
          CALL ioname('export',filename,LPOSTFIX=.FALSE.)
          CALL file_open(dft_unit,TRIM(filename),PATH="/Eigenvectors/",ACTION="read", &
                                  FORM='formatted')


          !
          ! initializing Ca and Mkb
          ca(:,:,:) = CZERO
          Mkb(:,:,:,:) = CZERO
      

          !
          ! main loop on kpts
          !
          kpoints : &
          DO ik=1,nkpts
             !
             WRITE(stdout,"( 4x,'Overlaps or Projections calculation for k-point: ',i4)") ik
             WRITE(stdout,"( 4x,'npw = ',i6,',', 4x,'dimwin = ',i4)") npwk(ik), dimwin(ik)

             CALL wfc_data_kread(dft_unit, ik, "IK", evc, evc_info)

             IF ( use_uspp ) THEN
                 !
                 ! determine the index related to the first wfc of the current ik
                 ! to be used in evc to get the right starting point
                 !
                 index = wfc_info_getindex(imin(ik), ik, "IK", evc_info )
                 !
                 xk(:) = vkpt(:,ik) / tpiba
                 CALL init_us_2( npwk(ik), igsort(1,ik), xk, vkb )
                 !
                 vkb_ik = ik
                 CALL ccalbec( nkb, npwkx, npwk(ik), dimwin(ik), becp(1,1,ik), vkb, &
                               evc(1,index))
             ENDIF

             IF ( do_overlaps ) THEN
                !
                ! overlap
                !
                neighbours : &
                DO inn=1,nb/2
                    !
                    ! here impose the symmetrization on Mkb: i.e.
                    ! M_ij(k,b) = CONJG( M_ji (k+b, -b) )
                    !
                    ! In order to do that we compute the Mkb integrals only 
                    ! for half of the defined b vectors and then impose the
                    ! other values by symmetry
                    !
                    ib = nnpos(inn)
                    ikb = nnlist( ib , ik)
                    !
                    CALL wfc_data_kread(dft_unit, ikb, "IKB", evc, evc_info)
                    !
                    IF( use_uspp ) THEN
                          !
                          index = wfc_info_getindex(imin(ikb), ikb, "IKB", evc_info)
                          !
                          xk(:) = vkpt(:,ikb) / tpiba
                          CALL init_us_2( npwk(ikb), igsort(1,ikb), xk, vkb )
                          vkb_ik = ikb
                          CALL ccalbec( nkb, npwkx, npwk(ikb), dimwin(ikb), becp(1,1,ikb),&
                                        vkb, evc(1,index))
                    ENDIF
                    !
                    CALL overlap( ik, ikb, dimwin(ik), dimwin(ikb), &
                                  imin(ik), imin(ikb), dimwinx, evc, evc_info,  &
                                  igsort, nncell(1,ib,ik), Mkb(1,1,ib,ik) )

                    !
                    ! ... add the augmentation term fo USPP
                    !
                    IF ( use_uspp ) THEN
                         CALL overlap_augment(dimwinx, dimwin(ik), dimwin(ikb), &
                                              ik, ikb, ib, aux)
                         Mkb(1:dimwin(ik), 1:dimwin(ikb), ib, ik) =        &
                                Mkb(1:dimwin(ik), 1:dimwin(ikb), ib, ik) + &
                                aux(1:dimwin(ik), 1:dimwin(ikb))
                    ENDIF
              
                    !
                    ! clean nn wfc data (but not free memory!)
                    !
                    CALL wfc_info_delete(evc_info, LABEL="IKB" )

                    !
                    ! apply the symmetrization
                    ! M_ij(k,b) = CONJG( M_ji (k+b, -b) )
                    !
                    Mkb(:,:, nnrev(ib), ikb) = CONJG( TRANSPOSE( Mkb(:,:,ib,ik)))
                ENDDO neighbours
             ENDIF


             IF ( do_projections ) THEN
                !
                ! projections
                !
                ! construct S \psi and use them istead of the base \psi 
                ! (they are equal if NCPP case)
                ! after the call to s_psi evc will contain the S \psi wfc
                ! with the label SPSI_IK
                ! 
                indin = wfc_info_getindex(imin(ik), ik, "IK", evc_info)
                !
                DO ib = imin(ik), imax(ik)
                     CALL wfc_info_add(npwk(ik), ib, ik, 'SPSI_IK', evc_info)
                ENDDO
                !
                indout = wfc_info_getindex(imin(ik), ik, "SPSI_IK", evc_info)

                IF( use_uspp ) THEN
                     xk(:) = vkpt(:,ik) / tpiba
                     CALL init_us_2( npwk(ik), igsort(1,ik), xk, vkb )
                     vkb_ik = ik
                ENDIF
                !
                CALL s_psi(npwkx, npwk(ik), dimwin(ik), ik, evc(1,indin), evc(1,indout) )
   
                CALL projection( ik, dimwin(ik), imin(ik), dimwinx, evc, evc_info, dimwann, &
                                 trial, ca(1,1,ik) )
                !
                ! clean the ik wfc data
                CALL wfc_info_delete(evc_info, LABEL="SPSI_IK")
             ENDIF

             CALL wfc_info_delete(evc_info, LABEL="IK")
             CALL timing_upto_now(stdout)
          ENDDO kpoints


          !
          ! ... re-closing the main data file
          CALL file_close(dft_unit,PATH="/Eigenvectors/",ACTION="read")


          !
          ! ... local cleaning
          DEALLOCATE( becp, STAT=ierr )
             IF (ierr/=0) CALL errore(subname,'deallocating becp',ABS(ierr))
          DEALLOCATE( aux, STAT=ierr )
             IF (ierr/=0) CALL errore(subname,'deallocating aux',ABS(ierr))


          !
          ! ... clean the large amount of memory used by the wfcs and grids
          !
          CALL wfc_data_deallocate()
          CALL ggrids_deallocate()

      !
      ! end of the newly computed quantities
      !
      ENDIF


!
! ... Here read overlap or projections (or both) if needed
!
      IF ( read_overlaps .OR. read_projections ) THEN
          CALL ioname('overlap_projection',filename)
          CALL file_open(ovp_unit,TRIM(filename),PATH="/",ACTION="read",FORM="formatted")
              CALL overlap_read(ovp_unit,"OVERLAP_PROJECTION", lfound, &  
                   LOVERLAP=read_overlaps, LPROJECTION=read_projections)
              IF ( .NOT. lfound ) CALL errore(subname,'reading ovp and proj',1)
          CALL file_close(ovp_unit,PATH="/",ACTION="read")

          CALL ioname('overlap_projection',filename,LPATH=.FALSE.)
          WRITE(stdout, "(/)")
          IF ( read_overlaps ) &
             WRITE( stdout,"(2x,'Overlaps read from file: ',a)") TRIM(filename)
          IF ( read_projections ) &
             WRITE( stdout,"(2x,'Projections read from file: ',a)") TRIM(filename)
          WRITE(stdout,"()")

      ENDIF

!
! ... Finally write projections and overlaps on file (if the case)
!
      IF ( .NOT. (read_overlaps .AND. read_projections)  ) THEN

          CALL ioname('overlap_projection',filename)
          CALL file_open(ovp_unit,TRIM(filename),PATH="/",ACTION="write",FORM="formatted")
               CALL overlap_write(ovp_unit,"OVERLAP_PROJECTION")
          CALL file_close(ovp_unit,PATH="/",ACTION="write")

          CALL ioname('overlap_projection',filename,LPATH=.FALSE.)
          WRITE( stdout,"(/,'  Overlaps and projections written on file: ',a)") &
                 TRIM(filename)
      ENDIF

      CALL timing_upto_now(stdout)
      CALL timing('wfc_manager',OPR='stop')
      RETURN

   END SUBROUTINE wfc_manager


