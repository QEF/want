!
! Copyright (C) 2004 WanT Group
!
! This file is distributed under the terms of the
! GNU General Public License. See the file `License\'
! in the root directory of the present distribution,
! or http://www.gnu.org/copyleft/gpl.txt .
!

!*********************************************
   MODULE wfc_data_module
!*********************************************
   !
   USE kinds,             ONLY : dbl
   USE constants,         ONLY : CZERO, ZERO
   USE parameters,        ONLY : nstrx
   USE windows_module,    ONLY : nbnd, nkpts, dimwinx, imin, imax, ispin, nspin
   USE timing_module,     ONLY : timing
   USE log_module,        ONLY : log_push, log_pop
   USE ggrids_module,     ONLY : ecutwfc
   USE wfc_info_module
   USE qexml_module
   USE qexpt_module
   !
   IMPLICIT NONE
   PRIVATE
   SAVE

! This module handles data referring PW representation
! of wave-functions, including the grids
!
! routines in this module:
! SUBROUTINE wfc_data_deallocate()
! SUBROUTINE wfc_data_grids_read( filefmt )
! SUBROUTINE wfc_data_grids_summary( iunit )
! SUBROUTINE wfc_data_kread(unit,ik,label,wfc,lwfc[,iband_min][,iband_max])
!

!
! declarations of common variables
!   

   INTEGER                   :: npwkx            ! max number of G vects over kpts +1 
                                                 ! the last position is used in overlap
   INTEGER,      ALLOCATABLE :: npwk(:)          ! number of G for each kpt, DIM: nkpts
   INTEGER,      ALLOCATABLE :: igsort(:,:)      ! G map between the global IGV array and
                                                 ! the local ordering for each kpt
                                                 ! DIM: npwkx, nkpts

!
! ... Bloch eigenvectors
!
   COMPLEX(dbl), ALLOCATABLE :: evc(:,:)         ! wfc data; store wfc related to one or more
                                                 ! kpts: current impl store wfc for k and b
                                                 ! DIM: npwx, nvect
   TYPE(wfc_info)            :: evc_info         ! the descriptof of the wfcs in EVC

   !
   LOGICAL :: alloc = .FALSE.

!
! end of declarations
!

   PUBLIC :: npwkx
   PUBLIC :: npwk
   PUBLIC :: igsort
   PUBLIC :: evc
   PUBLIC :: evc_info
   PUBLIC :: ecutwfc
   PUBLIC :: alloc

   PUBLIC :: wfc_data_deallocate
   PUBLIC :: wfc_data_grids_read 
   PUBLIC :: wfc_data_grids_summary
   PUBLIC :: wfc_data_kread

CONTAINS

!
! subroutines
!

!**********************************************************
   SUBROUTINE wfc_data_grids_read( filefmt )
   !**********************************************************
   IMPLICIT NONE
       CHARACTER(*), INTENT(IN) :: filefmt
       CHARACTER(19)       :: subname="wfc_data_grids_read"
       INTEGER             :: ik, ierr 


       CALL timing ( subname, OPR="START")
       CALL log_push( subname )
       !
       !
       ! few checks
       !
       IF ( dimwinx <= 0 )   CALL errore(subname,'dimwinx <= 0', ABS(dimwinx)+1)
       IF ( nkpts <= 0 )     CALL errore(subname,'nkpts <= 0',   ABS(nkpts)+1)
       IF ( nbnd <= 0 )      CALL errore(subname,'nbnd <= 0',    ABS(nbnd)+1)
       !
       ALLOCATE( npwk(nkpts), STAT=ierr )
       IF (ierr/=0) CALL errore(subname,'allocating npwk',ABS(ierr))

       !
       !
       ! ... reads dimensions for grids
       !     npwkx is obtained from the loop and not read because of 
       !     problems with the formats and thier back readability
       !
       SELECT CASE ( TRIM(filefmt) )
       !
       CASE ( 'qexml' )
            !
            DO ik = 1, nkpts
                !
                CALL qexml_read_gk( ik, NPWK=npwk(ik), IERR=ierr )
                IF ( ierr/=0) CALL errore(subname,'getting npwk',ik)
                !
            ENDDO
            !
            npwkx = MAXVAL( npwk( 1: nkpts ) )
            !
       CASE ( 'pw_export' )
            !
            DO ik = 1, nkpts
                !
                CALL qexpt_read_gk( ik, NPWK=npwk(ik), IERR=ierr )
                IF ( ierr/=0) CALL errore(subname,'getting npwk',ik)
                !
            ENDDO
            !
            npwkx = MAXVAL( npwk( 1: nkpts ) )
            !
       CASE DEFAULT
            !
            CALL errore(subname,'invalid filefmt = '//TRIM(filefmt), 1)
       END SELECT
       !
       IF ( ierr/=0) CALL errore(subname,'getting grids dimensions',ABS(ierr))
       !

       !
       ! Allocations
       !

       IF ( npwkx <= 0 )     CALL errore(subname,'npwkx <= 0',   ABS(npwkx)+1)
       !
       ! WARNING: nasty redefinition to have one component of the array free
       !
       npwkx = npwkx + 1
       !
       ALLOCATE( igsort(npwkx,nkpts), STAT=ierr )
       IF (ierr/=0) CALL errore(subname,'allocating igsort',ABS(ierr))

       !
       ! init
       igsort(:,:) = 0

       !
       ! read massive data
       !
       SELECT CASE ( TRIM(filefmt) )
       !
       CASE ( 'qexml' )
            !
            DO ik = 1, nkpts
                !
                CALL qexml_read_gk( ik, NPWK=npwk(ik), &
                                    INDEX=igsort( 1:npwk(ik), ik), IERR=ierr )
                IF ( ierr/=0) CALL errore(subname,'getting igsort',ik)
                !
            ENDDO
            !
       CASE ( 'pw_export' )
            !
            DO ik = 1, nkpts
                !
                CALL qexpt_read_gk( ik, NPWK=npwk(ik), &
                                    INDEX=igsort( 1:npwk(ik), ik), IERR=ierr )
                IF ( ierr/=0) CALL errore(subname,'getting igsort',ik)
                !
            ENDDO
            !
       CASE DEFAULT
            !
            CALL errore(subname,'invalid filefmt = '//TRIM(filefmt), 1)
       END SELECT
       !
       IF ( npwkx /= MAXVAL(npwk(:)) +1 ) CALL errore(subname,'Invalid npwkx II',5)
       !
       alloc = .TRUE.
       !
       CALL timing ( subname, OPR="STOP")
       CALL log_pop( subname )
       !
   END SUBROUTINE wfc_data_grids_read


!**********************************************************
   SUBROUTINE wfc_data_deallocate()
   !**********************************************************
   IMPLICIT NONE
       CHARACTER(19)      :: subname="wfc_data_deallocate"
       INTEGER            :: ierr

       CALL log_push( subname )
       !
       IF ( ALLOCATED(npwk) ) THEN
            DEALLOCATE(npwk, STAT=ierr)
            IF (ierr/=0)  CALL errore(subname,' deallocating npwk ',ABS(ierr))
       ENDIF
       IF ( ALLOCATED(igsort) ) THEN
            DEALLOCATE(igsort, STAT=ierr)
            IF (ierr/=0)  CALL errore(subname,' deallocating igsort ',ABS(ierr))
       ENDIF
       IF ( ALLOCATED(evc) ) THEN
            DEALLOCATE(evc, STAT=ierr)
            IF (ierr/=0)  CALL errore(subname,' deallocating evc ',ABS(ierr))
       ENDIF
       IF ( evc_info%alloc ) CALL wfc_info_deallocate(evc_info)
       !
       alloc = .FALSE.
       !
       CALL log_pop( subname )
       !
   END SUBROUTINE wfc_data_deallocate


!*********************************************************
   SUBROUTINE wfc_data_kread( filefmt, ik, label, wfc, lwfc, iband_min, iband_max )
   !*********************************************************
   !
   ! read wfc of a selected kpt from file to the type LWFC. 
   ! the object may contain other wfc and free slots are used to
   ! store the new ones.
   ! Using IBAND_MIN = IBAND_MAX it can be used to read one wfc
   ! at the time
   !
   IMPLICIT NONE
       INTEGER,         INTENT(in)    :: ik
       CHARACTER(*),    INTENT(in)    :: label, filefmt
       COMPLEX(dbl),    INTENT(inout) :: wfc(:,:)
       TYPE(wfc_info),  INTENT(inout) :: lwfc
       INTEGER, OPTIONAL, INTENT(in)  :: iband_min,iband_max

       CHARACTER(14)      :: subname="wfc_data_kread"
       INTEGER            :: ib, ibs, ibe, lindex, ierr

       !
       CALL timing ( subname,OPR='start')
       CALL log_push( subname )

       !
       ! lwfc is supposed to be already allocated
       IF ( .NOT. lwfc%alloc ) CALL errore(subname,'lwfc not yet allocated',1)
       IF ( ik <= 0 .OR. ik > nkpts ) CALL errore(subname,'invalid ik',2)

       ibs = imin(ik)
       ibe = imax(ik)
       IF ( PRESENT(iband_min) ) ibs = iband_min
       IF ( PRESENT(iband_max) ) ibe = iband_max

       IF ( ibs < imin(ik) ) CALL errore(subname,'Invalid iband_min',2)
       IF ( ibe > imax(ik) ) CALL errore(subname,'Invalid iband_max',3)

       !
       ! setting the wfc descriptor
       !
       CALL wfc_info_add(npwk(ik), ibs, ik, TRIM(label), lwfc, INDEX=lindex )
       !
       DO ib = ibs+1, ibe
            !
            CALL wfc_info_add(npwk(ik), ib, ik, TRIM(label), lwfc )
            !
       ENDDO
  
       !
       ! ... actual reading
       !
       SELECT CASE ( TRIM(filefmt) )
       !
       CASE ( 'qexml' )
            !
            IF ( nspin == 2 ) THEN
               !
               CALL qexml_read_wfc( ibs, ibe, ik, ISPIN= ispin, IGK=igsort(:,ik), &
                                    WF=wfc(:, lindex: ), IERR=ierr )
               !
            ELSE
               !
               CALL qexml_read_wfc( ibs, ibe, ik, IGK=igsort(:,ik), &
                                    WF=wfc(:, lindex: ), IERR=ierr )
               !
            ENDIF
            !
       CASE ( 'pw_export' )
            !
            CALL qexpt_read_wfc( ibs, ibe, ik, ispin, IGK=igsort(:,ik), &
                                 WF=wfc(:, lindex: ), IERR=ierr )
            !
       CASE DEFAULT
            !
            CALL errore(subname,'invalid filefmt = '//TRIM(filefmt), 1)
            !
       END SELECT
       !
       IF ( ierr/=0) CALL errore(subname,'reading wfcs',ABS(ierr))
       !
       !
       CALL timing(subname,OPR='stop')
       CALL log_pop( subname )
       ! 
   END SUBROUTINE wfc_data_kread


!**********************************************************
   SUBROUTINE wfc_data_grids_summary( iunit )
   !**********************************************************
   !
   ! Writes summary of the main quantities and dimensions in the
   ! module
   !
   IMPLICIT NONE
       INTEGER, INTENT(IN) :: iunit
       !
       !
       WRITE(iunit, "(/,6x,'    Energy cut-off for wfcs =  ',5x,F7.2,' (Ry)' )") ecutwfc
       !
       ! we subtract 1 because of the internal redefinition of the parameter npwkx
       ! wrt the one used in DFT (see wfc_data_grids_read)
       !
       WRITE(iunit, "(  6x,'  Max number of PW for wfc  =  ',i9)") npwkx -1
       WRITE(iunit, "(  6x,'Total number of PW for wfcs =  ',i9)") MAXVAL(igsort(:,:))
       !
   END SUBROUTINE wfc_data_grids_summary

END MODULE wfc_data_module

