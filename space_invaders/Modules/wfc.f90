!
! Copyright (C) 2004 Andrea Ferretti
!
! This file is distributed under the terms of the
! GNU General Public License. See the file `License\'
! in the root directory of the present distribution,
! or http://www.gnu.org/copyleft/gpl.txt .
!

!*********************************************
   MODULE wfc_module
!*********************************************
   USE kinds, ONLY : dbl
   USE constants, ONLY : CZERO
   USE windows_module, ONLY : nbnd, nkpts, dimwinx, imin, imax
   USE iotk_module
   USE parameters, ONLY : nstrx
   IMPLICIT NONE
   PRIVATE
   SAVE

! This module handles data referring PW representation
! of wave-functions, including the grids
!
! routines in this module:
! SUBROUTINE wfc_allocate()
! SUBROUTINE wfc_deallocate()
! SUBROUTINE wfc_read_ext(unit)
!

!
! declarations of common variables
!   

   INTEGER                   :: npwkx            ! max number of G vects over kpts
   INTEGER,      ALLOCATABLE :: npwk(:)          ! number of G for each kpt, DIM: nkpts
   INTEGER,      ALLOCATABLE :: igsort(:,:)      ! G map between the global IGV array and
                                                 ! the local ordering for each kpt
                                                 ! DIM: npwkx, nkpts

   !
   ! ... Bloch  EIGVEC
   COMPLEX(dbl), ALLOCATABLE :: evc(:,:,:)       ! wfc, DIM: npwkx+1, dimwinx, nkpts

   LOGICAL :: alloc = .FALSE.


   !
   ! ... Bloch EIGENVECTORS
   TYPE wfc
        COMPLEX(dbl), POINTER   :: evc(:)      
        INTEGER                 :: npwk
        LOGICAL                 :: alloc
   END TYPE wfc

   TYPE (wfc), ALLOCATABLE      :: evcs(:,:)     ! DIM: nbnd, nkpts

   

!
! end of declarations
!

   PUBLIC :: npwkx
   PUBLIC :: npwk
   PUBLIC :: igsort
   PUBLIC :: evc
   PUBLIC :: alloc

   PUBLIC :: wfc_allocate, wfc_deallocate, wfc_read_ext

CONTAINS

!
! subroutines
!

!**********************************************************
   SUBROUTINE wfc_allocate()
   !**********************************************************
   IMPLICIT NONE
       CHARACTER(12)      :: subname="wfc_allocate"
       INTEGER            :: ierr 

       IF ( npwkx <= 0 )  CALL errore(subname,'npwkx <= 0',ABS(npwkx)+1)
       IF ( dimwinx <= 0 )CALL errore(subname,'dimwinx <= 0',ABS(dimwinx)+1)
       IF ( nkpts <= 0 )  CALL errore(subname,'nkpts <= 0',ABS(nkpts)+1)
       IF ( nbnd <= 0 ) CALL errore(subname,'nbnd <= 0',ABS(nbnd)+1)

       ALLOCATE( npwk(nkpts), STAT=ierr )
          IF (ierr/=0) CALL errore(subname,'allocating npwk',nkpts)
       ALLOCATE( igsort(npwkx,nkpts), STAT=ierr )
          IF (ierr/=0) CALL errore(subname,'allocating igsort',npwkx*nkpts)
       ALLOCATE( evc(npwkx+1,dimwinx,nkpts), STAT=ierr )
          IF (ierr/=0) CALL errore(subname,'allocating evc',(npwkx+1)*dimwinx*nkpts)
 
       alloc = .TRUE.
      
   END SUBROUTINE wfc_allocate


!**********************************************************
   SUBROUTINE wfc_deallocate()
   !**********************************************************
   IMPLICIT NONE
       CHARACTER(14)      :: subname="wfc_deallocate"
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
            IF (ierr/=0)  CALL errore(subname,' deallocating ecv ',ABS(ierr))
       ENDIF
       alloc = .FALSE.

   END SUBROUTINE wfc_deallocate


!*********************************************************
   SUBROUTINE wfc_read_ext(unit)
   !*********************************************************
   IMPLICIT NONE
       INTEGER,           INTENT(in) :: unit
       CHARACTER(nstrx)   :: attr
       CHARACTER(12)      :: subname="wfc_read_ext"
       INTEGER            :: ik,ib,ig, index, idum, ierr
       COMPLEX(dbl), ALLOCATABLE :: wtmp(:)

       !
       ! ... wfc grids
       !
       CALL iotk_scan_begin(unit,'Wfc_grids',ATTR=attr,IERR=ierr)
       IF (ierr/=0)  CALL errore(subname,'Unable to find Wfc_grids',ABS(ierr))
       CALL iotk_scan_attr(attr,'npwx',npwkx,IERR=ierr)
       IF (ierr/=0)  CALL errore(subname,'Unable to find npwkx',ABS(ierr))
   
       CALL wfc_allocate()
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
       IF ( npwkx /= MAXVAL(npwk(:)) ) CALL errore(subname,'Invalid npwkx II',5)
       CALL iotk_scan_end(unit,'Wfc_grids',IERR=ierr)
       IF (ierr/=0)  CALL errore(subname,'Unable to end tag Wfc_grids',ABS(ierr))

       !
       ! ... wfc components
       !
       CALL iotk_scan_begin(unit,'Eigenvectors',IERR=ierr)
       IF (ierr/=0)  CALL errore(subname,'Unable to find Eigenvector',ABS(ierr))

       ALLOCATE( wtmp(npwkx), STAT=ierr )
          IF (ierr/=0) CALL errore(subname,'allocating wtmp',ABS(npwkx)+1)

       DO ik = 1,nkpts
           CALL iotk_scan_begin(unit,'Kpoint'//TRIM(iotk_index(ik)),IERR=ierr)
           IF (ierr/=0)  CALL errore(subname,'Unable to find Kpoint (vectors)',ik)
           CALL iotk_scan_empty(unit,'Info',ATTR=attr,IERR=ierr)
           IF (ierr/=0)  CALL errore(subname,'Unable to find Info',ik)
           CALL iotk_scan_attr(attr,'nbnd',idum,IERR=ierr)
           IF (ierr/=0)  CALL errore(subname,'Unable to find nbnd',ik)
           IF ( idum /= nbnd ) CALL errore(subname,'Invalid nbnd',6)
           
           DO ib=imin(ik),imax(ik)
               index = ib - imin(ik) +1
               evc(:,index,ik) = CZERO
               CALL iotk_scan_dat(unit,'Wfc'//TRIM(iotk_index(ib)), &
                    wtmp(1:npwk(ik)),IERR=ierr)
               IF (ierr/=0)  CALL errore(subname,'Unable to find Wfc',ABS(ierr))
               evc( 1:npwk(ik),index,ik) = wtmp( 1:npwk(ik) )
           ENDDO
           CALL iotk_scan_end(unit,'Kpoint'//TRIM(iotk_index(ik)),IERR=ierr)
           IF (ierr/=0)  CALL errore(subname,'Unable to end tag Kpoint (vectors)',ik)
       ENDDO
       DEALLOCATE( wtmp, STAT=ierr )
          IF (ierr/=0) CALL errore(subname,'deallocating wtmp',ABS(ierr))

       CALL iotk_scan_end(unit,'Eigenvectors',IERR=ierr)
       IF (ierr/=0)  CALL errore(subname,'Unable to end tag Eigenvector',ABS(ierr))
        
   END SUBROUTINE wfc_read_ext


END MODULE wfc_module

