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
   USE windows_module, ONLY : mxdbnd, nkpts, dimwinx
   USE ggrids_module, ONLY : npwx, npwk 
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
! SUBROUTINE wfc_write(unit,name)
! SUBROUTINE wfc_read(unit,name,found)

!
! declarations of common variables
!   

   INTEGER,      ALLOCATABLE :: igsort(:,:)      ! G map between the global IGV array and
                                                 ! the local ordering for each kpt
                                                 ! DIM: npwx, nkpts
   !
   ! ... Bloch  EIGVEC
   COMPLEX(dbl), ALLOCATABLE :: evc(:,:,:)       ! wfc, DIM: npwkx+1, dimwinx, nkpts
   

!
! end of declarations
!

   PUBLIC :: npwx
   PUBLIC :: npwk
   PUBLIC :: igsort
   PUBLIC :: evc
   PUBLIC :: wfc_allocate, wfc_deallocate

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

       IF ( npwx <= 0 )   CALL errore(subname,'npwx <= 0',ABS(npwx)+1)
       IF ( dimwinx <= 0 )CALL errore(subname,'dimwinx <= 0',ABS(dimwinx)+1)
       IF ( nkpts <= 0 )  CALL errore(subname,'nkpts <= 0',ABS(nkpts)+1)
       IF ( mxdbnd <= 0 ) CALL errore(subname,'mxdbnd <= 0',ABS(mxdbnd)+1)

       ALLOCATE( igsort(npwx,nkpts), STAT=ierr )
          IF (ierr/=0) CALL errore(subname,'allocating igsort',npwx*nkpts)
       ALLOCATE( evc(npwx+1,dimwinx,nkpts), STAT=ierr )
          IF (ierr/=0) CALL errore(subname,'allocating eiw',(npwx+1)*dimwinx*nkpts)
      
   END SUBROUTINE wfc_allocate


!**********************************************************
   SUBROUTINE wfc_deallocate()
   !**********************************************************
   IMPLICIT NONE
       CHARACTER(14)      :: subname="wfc_deallocate"
       INTEGER            :: ierr

       IF ( ALLOCATED(igsort) ) THEN
            DEALLOCATE(igsort, STAT=ierr)
            IF (ierr/=0)  CALL errore(subname,' deallocating igsort ',ABS(ierr))
       ENDIF
       IF ( ALLOCATED(evc) ) THEN
            DEALLOCATE(evc, STAT=ierr)
            IF (ierr/=0)  CALL errore(subname,' deallocating ecv ',ABS(ierr))
       ENDIF

   END SUBROUTINE wfc_deallocate


END MODULE wfc_module

