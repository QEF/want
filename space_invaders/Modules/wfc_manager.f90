! 
! Copyright (C) 2004 Andrea Ferretti
! 
! This file is distributed under the terms of the 
! GNU General Public License. See the file `License' 
! in the root directory of the present distribution, 
! or http://www.gnu.org/copyleft/gpl.txt . 
! 
! <INFO>
!*********************************************
   MODULE wfc_manager_module
!*********************************************
   USE kinds
   USE parameters,     ONLY : nstrx
   USE constants,      ONLY : ZERO
   USE iotk_module
   USE io_module,      ONLY : stdout, dft_unit, ovp_unit, ioname, pseudo_dir
   USE timing_module,  ONLY : timing
   USE ions_module,    ONLY : psfile, uspp_calculation
   USE files_module,   ONLY : file_open, file_close
   USE converters_module, ONLY : cry2cart
   USE util_module,    ONLY : zmat_mul
   
   USE lattice_module, ONLY : avec, bvec, tpiba, alat
   USE subspace_module,ONLY : dimwann
   USE trial_center_data_module,   ONLY : trial
   USE windows_module, ONLY : windows_alloc => alloc, dimwin, dimwinx, dimfroz
   USE kpoints_module, ONLY : kpoints_alloc, bshells_alloc, nkpts, vkpt, mxdnn, &
                              nntot, nnlist, nncell
   USE overlap_module, ONLY : cm, ca, overlap_alloc => alloc, overlap_write
   USE ggrids_module,  ONLY : npw, nr, ecutwfc, ecutrho, igv, &
                              ggrids_read_ext, ggrids_deallocate
   USE wfc_module,     ONLY : npwkx, npwx_g, npwk, igsort, evc, &
                              wfc_read_ext, wfc_deallocate 
   USE struct_fact_data_module, ONLY : struct_fact_data_init
   USE uspp,           ONLY : nkb, vkb
   USE becmod,         ONLY : becp
   IMPLICIT NONE
   PRIVATE
!
! This module contains a subroutine that permforms all the
! operations needed to obtain the internal data from DFT wfcs
! (OVERLAP and PROJECTIONS matrix elements).
!
! Interface:
! SUBROUTINE wfc_manager()
!
! Tasks performed:
! * read and init G grids
! * read and init wfcs
! * compute the projections
! * compute the overlap
! * write overlap and projections to file
! * waste ggrids and wfcs data
!
! </INFO>

   PUBLIC :: wfc_manager

CONTAINS

!*********************************************************
   SUBROUTINE wfc_manager(lamp)
   !*********************************************************
   IMPLICIT NONE
      COMPLEX(dbl), INTENT(out) :: lamp(:,:,:)
      CHARACTER(11)             :: subname="wfc_manager"
      CHARACTER(nstrx)          :: filename
      REAL(dbl), ALLOCATABLE    :: xk(:,:)
      REAL(dbl)                 :: tmp
      INTEGER                   :: ierr, ik, idum
      INTEGER                   :: i, j, ig 


      CALL timing('wfc_manager',OPR='start')


!
! ... Read ggrids and wfcs
!
      !
      ! ... opening the file containing the PW-DFT data
      CALL ioname('export',filename,LPOSTFIX=.FALSE.)
      CALL file_open(dft_unit,TRIM(filename),PATH="/",ACTION="read", &
                               FORM='formatted')
      CALL ioname('dft_data',filename,LPATH=.FALSE.,LPOSTFIX=.FALSE.)

      !
      ! ... grids
      WRITE( stdout,"(/,2x,'Reading density G-grid from file: ',a)") TRIM(filename)
      CALL ggrids_read_ext(dft_unit)

      
      !
      ! ... wfcs
      WRITE( stdout,"(  2x,'Reading Wfcs from file: ',a)") TRIM(filename)
      CALL wfc_read_ext(dft_unit)

      !
      ! ... closing the main data file
      CALL file_close(dft_unit,PATH="/",ACTION="read")


      ! 
      ! ... if USPP are used, initialize the related quantities
      WRITE( stdout,"(/,2x,'Initizlizing US pseudopot. data')")
      IF ( uspp_calculation ) THEN
          !
          ! ... data required by USPP
          CALL allocate_nlpot()
          !
          ! ... first initialization
          !     here we compute (among other quantities) \int dr Q_ij(r)
          !                                              \int dr e^ibr Q_ij(r)
          CALL init_us_1()
          !
          ! ... structure factors 
          CALL struct_fact_data_init()

          !
          ! ... beta functions in reciproc space within struct_facts
          !     and their projections <beta|psi>
          ! 
          IF ( nkb <= 0 ) CALL errore(subname,'no beta functions while using USPP',-nkb+1)
          ALLOCATE( becp(nkb, dimwinx, nkpts), STAT=ierr )
              IF (ierr/=0) CALL errore(subname,'allocating becp',ABS(ierr))

              !
              ! kpts should be in the same unit as g, gg (i.e. tpiba) while they currently
              ! are in crystal units
              !
          ALLOCATE( xk(3,nkpts), STAT=ierr)
              IF (ierr/=0) CALL errore(subname,'allocating xk',ABS(ierr))
          xk(:,:) = vkpt(:,:)
          CALL cry2cart( xk, bvec / tpiba )

          DO ik=1,nkpts 
              CALL init_us_2( npwk(ik), igsort(1,ik), xk(1,ik), vkb(1,1,ik) )
              CALL ccalbec( nkb, npwkx, npwk(ik), dimwinx, becp(1,1,ik), &
                            vkb(1,1,ik), evc(1,1,ik) )
          ENDDO
      ENDIF



!
! ... stdout summary about grids and wfcs
!
      WRITE(stdout, "(/)")
      WRITE(stdout, "(2x,70('='))" )
      WRITE(stdout, "(2x,'=',23x,'Overlap and Projections',22x,'=')" )
      WRITE(stdout, "(2x,70('='),/)" )
      WRITE(stdout, "(2x,'Kinetic energy cut-off for wfcs =  ', 5x, F7.2, ' (Ry)' )") ecutwfc
      WRITE(stdout, "(2x,'                       for rho  =  ', 5x, F7.2, ' (Ry)' )") ecutrho
      WRITE(stdout, "(2x,'    Total number of PW for rho  =  ', i9 )") npw
      WRITE(stdout, "(2x,'      Max number of PW for wfc  =  ', i9 )") npwkx
      WRITE(stdout, "(2x,'    Total number of PW for wfcs =  ', i9 )") npwx_g
      WRITE(stdout, "(2x,'      FFT grid components (rho) =  ( ', 3i5,' )' )") nr(:)
      WRITE(stdout, "()")

!
! ... actual calculation 
!
      IF ( .NOT. kpoints_alloc) CALL errore(subname,'kpoints NOT alloc',2) 
      IF ( .NOT. bshells_alloc) CALL errore(subname,'bshells NOT alloc',3) 
      IF ( .NOT. windows_alloc) CALL errore(subname,'windows NOT alloc',4) 
      IF ( .NOT. overlap_alloc) CALL errore(subname,'overlap NOT alloc',5) 


      CALL overlap( evc, igsort, npwk, dimwin,                     &
                    nntot, nnlist, nncell, cm, npw, npwkx, nkpts,  &
                    mxdnn, nr(1), nr(2), nr(3), dimwinx )

      CALL projection( avec, lamp, ca, evc, vkpt,                  &
                       npwk, dimwin, dimwann, dimfroz,             &
                       npwkx, nkpts, dimwinx, trial)

      !
      ! ... clean a large amount of memory
      CALL ggrids_deallocate()
      CALL wfc_deallocate()

      IF ( uspp_calculation ) THEN
         DEALLOCATE( becp, STAT=ierr )
         IF (ierr/=0) CALL errore(subname,'deallocating becp',ABS(ierr))
         DEALLOCATE( xk, STAT=ierr )
         IF (ierr/=0) CALL errore(subname,'deallocating xk',ABS(ierr))
      ENDIF

!
! ... writing projections and overlap on file
!

      CALL ioname('overlap_projection',filename)
      CALL file_open(ovp_unit,TRIM(filename),PATH="/",ACTION="write",FORM="formatted")
           CALL overlap_write(ovp_unit,"OVERLAP_PROJECTION")
      CALL file_close(ovp_unit,PATH="/",ACTION="write")

      CALL ioname('overlap_projection',filename,LPATH=.FALSE.)
      WRITE( stdout,"(/,'  Overlap and projections written on file: ',a)") TRIM(filename)


      CALL timing('wfc_manager',OPR='stop')
      RETURN
   END SUBROUTINE wfc_manager

END MODULE wfc_manager_module

