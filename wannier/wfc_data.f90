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
   USE kinds, ONLY : dbl
   USE constants, ONLY : CZERO, ZERO
   USE parameters, ONLY : nstrx
   USE wfc_info_module
   USE windows_module, ONLY : nbnd, nkpts, dimwinx, imin, imax
   USE kpoints_module, ONLY : iks, ike
   USE iotk_module
   USE timing_module, ONLY: timing
   IMPLICIT NONE
   PRIVATE
   SAVE

! This module handles data referring PW representation
! of wave-functions, including the grids
!
! routines in this module:
! SUBROUTINE wfc_data_deallocate()
! SUBROUTINE wfc_data_grids_read(unit)
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
   PUBLIC :: alloc

   PUBLIC :: wfc_data_deallocate
   PUBLIC :: wfc_data_grids_read 
   PUBLIC :: wfc_data_kread

CONTAINS

!
! subroutines
!

!**********************************************************
   SUBROUTINE wfc_data_grids_read(unit)
   !**********************************************************
   IMPLICIT NONE
       INTEGER, INTENT(in) :: unit
       CHARACTER(19)       :: subname="wfc_data_grids_read"
       CHARACTER(nstrx)    :: attr
       INTEGER             :: ik, ierr 

       !
       ! ... reads wfc grids
       !
       CALL iotk_scan_begin(unit,'Wfc_grids',ATTR=attr,IERR=ierr)
       IF (ierr/=0)  CALL errore(subname,'Unable to find Wfc_grids',ABS(ierr))
       CALL iotk_scan_attr(attr,'npwx',npwkx,IERR=ierr)
       IF (ierr/=0)  CALL errore(subname,'Unable to find npwkx',ABS(ierr))

       !
       ! WARNING: nasty redefinition
       npwkx = npwkx + 1

       IF ( npwkx <= 0 )  CALL errore(subname,'npwkx <= 0',ABS(npwkx)+1)
       IF ( dimwinx <= 0 )CALL errore(subname,'dimwinx <= 0',ABS(dimwinx)+1)
       IF ( nkpts <= 0 )  CALL errore(subname,'nkpts <= 0',ABS(nkpts)+1)
       IF ( nbnd <= 0 ) CALL errore(subname,'nbnd <= 0',ABS(nbnd)+1)

       ALLOCATE( npwk(nkpts), STAT=ierr )
          IF (ierr/=0) CALL errore(subname,'allocating npwk',nkpts)
       ALLOCATE( igsort(npwkx,nkpts), STAT=ierr )
          IF (ierr/=0) CALL errore(subname,'allocating igsort',npwkx*nkpts)
 
       ! init
       igsort(:,:) = 0

       DO ik = 1,nkpts
           CALL iotk_scan_begin(unit,'Kpoint'//TRIM(iotk_index(ik)),ATTR=attr,IERR=ierr)
           IF (ierr/=0)  CALL errore(subname,'Unable to find Kpoint (grids)',ik)
           CALL iotk_scan_attr(attr,'npw',npwk(ik),IERR=ierr)
           IF (ierr/=0)  CALL errore(subname,'Unable to find npwk',ik)
           IF ( npwk(ik) <= 0 ) CALL errore(subname,'Invalid npwk',ABS(npwk)+1)
           CALL iotk_scan_dat(unit,'index',igsort(1:npwk(ik),ik),IERR=ierr)
           IF (ierr/=0)  CALL errore(subname,'Unable to find dat index',ik)
           CALL iotk_scan_end(unit,'Kpoint'//TRIM(iotk_index(ik)),IERR=ierr)
           IF (ierr/=0)  CALL errore(subname,'Unable to end tag Kpoint',ik)
       ENDDO
       IF ( npwkx /= MAXVAL(npwk(:)) +1 ) CALL errore(subname,'Invalid npwkx II',5)
       CALL iotk_scan_end(unit,'Wfc_grids',IERR=ierr)
       IF (ierr/=0)  CALL errore(subname,'Unable to end tag Wfc_grids',ABS(ierr))

       alloc = .TRUE.
   END SUBROUTINE wfc_data_grids_read


!**********************************************************
   SUBROUTINE wfc_data_deallocate()
   !**********************************************************
   IMPLICIT NONE
       CHARACTER(19)      :: subname="wfc_data_deallocate"
       INTEGER            :: ierr

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
       alloc = .FALSE.

   END SUBROUTINE wfc_data_deallocate


!*********************************************************
   SUBROUTINE wfc_data_kread(unit,ik,label,wfc,lwfc,iband_min,iband_max)
   !*********************************************************
   !
   ! read wfc of a selected kpt from file to the type LWFC. 
   ! the object may contain other wfc and free slots are used to
   ! store the new ones.
   ! Using IBAND_MIN = IBAND_MAX it can be used to read one wfc
   ! at the time
   !
   IMPLICIT NONE
       INTEGER,         INTENT(in)    :: unit
       INTEGER,         INTENT(in)    :: ik
       CHARACTER(*),    INTENT(in)    :: label
       COMPLEX(dbl),    INTENT(inout) :: wfc(:,:)
       TYPE(wfc_info),  INTENT(inout) :: lwfc
       INTEGER, OPTIONAL, INTENT(in)  :: iband_min,iband_max

       CHARACTER(14)      :: subname="wfc_data_kread"
       CHARACTER(nstrx)   :: attr, name
       INTEGER            :: ib, ibs, ibe, lindex, idum, ierr

       CALL timing(subname,OPR='start')

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
       ! here we considere the doubling of kpoints when nspin == 2
       ! by using iks and ike
       !
       CALL iotk_scan_begin(unit,'Kpoint'//TRIM(iotk_index(iks +ik -1)),IERR=ierr)
       IF (ierr/=0)  CALL errore(subname,'Unable to find Kpoint (vectors)',iks +ik -1)
       CALL iotk_scan_empty(unit,'Info',ATTR=attr,IERR=ierr)
       IF (ierr/=0)  CALL errore(subname,'Unable to find Info',iks+ik-1)
       CALL iotk_scan_attr(attr,'nbnd',idum,IERR=ierr)
       IF (ierr/=0)  CALL errore(subname,'Unable to find nbnd',iks+ik-1)
       IF ( idum /= nbnd ) CALL errore(subname,'Invalid nbnd',6)
           
       DO ib=ibs,ibe
            name = 'Wfc'//TRIM(iotk_index(ib))
            CALL wfc_info_add(npwk(ik), ib, ik, TRIM(label), lwfc, INDEX=lindex )
            !
            CALL iotk_scan_dat(unit,TRIM(name), wfc(1:npwk(ik),lindex),IERR=ierr)
            IF (ierr/=0 ) CALL errore(subname,'reading '//TRIM(name),iks+ik-1)
            !
            wfc(npwk(ik)+1:,lindex) = CZERO
       ENDDO
       !
       ! also here recorrect for the spin
       CALL iotk_scan_end(unit,'Kpoint'//TRIM(iotk_index(iks+ik-1)),IERR=ierr)
       IF (ierr/=0)  CALL errore(subname,'Unable to end tag Kpoint (vectors)',iks+ik-1)

       CALL timing(subname,OPR='stop')
       RETURN
   END SUBROUTINE wfc_data_kread


END MODULE wfc_data_module

