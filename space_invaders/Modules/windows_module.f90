!
! Copyright (C) 2004 Andrea Ferretti
!
! This file is distributed under the terms of the
! GNU General Public License. See the file `License\'
! in the root directory of the present distribution,
! or http://www.gnu.org/copyleft/gpl.txt .
!

!*********************************************
   MODULE windows_module
!*********************************************
   USE kinds, ONLY : dbl
   USE kpoints_module, ONLY : nkpts
   USE iotk_module
   IMPLICIT NONE
   PRIVATE
   SAVE

! This module handles data referring to the definition of 
! the initial windows (actual and frozen) given by input
!
! routines in this module:
! SUBROUTINE windows_allocate()
! SUBROUTINE windows_deallocate()

!
! declarations of common variables
!   

   INTEGER                     :: mxdbnd             ! = MAX(dimwin (:)) over kpts
   !
   ! ... starting states within the energy window
   INTEGER,      POINTER       :: dimwin(:)          ! define which eigenv are in the
   INTEGER,      POINTER       :: imin(:)            ! chosen energy window
   INTEGER,      POINTER       :: imax(:)            ! dim: nkpts
   REAL(dbl),    POINTER       :: eig(:,:)           ! DFT eigenv; dim: mxdbnd, nkpts
   LOGICAL                     :: lcompspace=.TRUE.  ! whether COMPLEMENT space is NOT null

   !
   ! ... frozen states
   INTEGER,      POINTER       :: dimfroz(:)         ! variable for using frozen (dim: nkpts)
   INTEGER,      POINTER       :: indxfroz(:,:)      ! states which are kept equal
   INTEGER,      POINTER       :: indxnfroz(:,:)     ! dim: mxdbnd nkpts
   LOGICAL                     :: lfrozen =.FALSE.   ! whether FROZEN states are present
   LOGICAL,      POINTER       :: frozen(:,:)        ! which are the frozen states
                                                     ! dim: mxdbnd, nkpts
!
! end of declarations
!

   PUBLIC :: nkpts, mxdbnd
   PUBLIC :: dimwin, imin, imax, eig, lcompspace
   PUBLIC :: dimfroz, indxfroz, indxnfroz, lfrozen, frozen

   PUBLIC :: windows_allocate
   PUBLIC :: windows_deallocate

CONTAINS

!
! subroutines
!

!**********************************************************
   SUBROUTINE windows_allocate()
   !**********************************************************
   IMPLICIT NONE
       CHARACTER(16)      :: subname="windows_allocate"
       INTEGER            :: ierr

       IF ( mxdbnd <= 0 .OR. nkpts <= 0 ) &
           CALL errore(subname,' Invalid MXDBND or NKPTS ',1)

       ALLOCATE( dimwin(nkpts), STAT=ierr )
           IF ( ierr/=0 ) CALL errore(subname,' allocating dimwin ',nkpts)      
       ALLOCATE( imin(nkpts), STAT=ierr )
           IF ( ierr/=0 ) CALL errore(subname,' allocating imin ',nkpts)      
       ALLOCATE( imax(nkpts), STAT=ierr )
           IF ( ierr/=0 ) CALL errore(subname,' allocating imax ',nkpts)      
       ALLOCATE( eig(mxdbnd,nkpts), STAT=ierr )
           IF ( ierr/=0 ) CALL errore(subname,' allocating eig ',mxdbnd*nkpts)      

       ALLOCATE( dimfroz(nkpts), STAT=ierr )
           IF ( ierr/=0 ) CALL errore(subname,' allocating dimfroz ',nkpts)      
       ALLOCATE( indxfroz(mxdbnd,nkpts), STAT=ierr )
           IF ( ierr/=0 ) CALL errore(subname,' allocating indxfroz ',nkpts)      
       ALLOCATE( indxnfroz(mxdbnd,nkpts), STAT=ierr )
           IF ( ierr/=0 ) CALL errore(subname,' allocating indxnfroz ',nkpts)      
       ALLOCATE( frozen(mxdbnd,nkpts), STAT=ierr )
           IF ( ierr/=0 ) CALL errore(subname,' allocating frozen ',mxdbnd*nkpts)      

   END SUBROUTINE windows_allocate


!**********************************************************
   SUBROUTINE windows_deallocate()
   !**********************************************************
   IMPLICIT NONE
       CHARACTER(18)      :: subname="windows_deallocate"
       INTEGER            :: ierr

       IF ( ASSOCIATED(dimwin) ) THEN
            DEALLOCATE(dimwin, STAT=ierr)
            IF (ierr/=0)  CALL errore(subname,' deallocating dimwin ',ABS(ierr))
       ENDIF
       IF ( ASSOCIATED(imin) ) THEN
            DEALLOCATE(imin, STAT=ierr)
            IF (ierr/=0)  CALL errore(subname,' deallocating imin ',ABS(ierr))
       ENDIF
       IF ( ASSOCIATED(imax) ) THEN
            DEALLOCATE(imax, STAT=ierr)
            IF (ierr/=0)  CALL errore(subname,' deallocating imax ',ABS(ierr))
       ENDIF
       IF ( ASSOCIATED(eig) ) THEN
            DEALLOCATE(eig, STAT=ierr)
            IF (ierr/=0)  CALL errore(subname,' deallocating eig ',ABS(ierr))
       ENDIF
       IF ( ASSOCIATED(dimfroz) ) THEN
            DEALLOCATE(dimfroz, STAT=ierr)
            IF (ierr/=0)  CALL errore(subname,' deallocating dimfroz ',ABS(ierr))
       ENDIF
       IF ( ASSOCIATED(indxfroz) ) THEN
            DEALLOCATE(indxfroz, STAT=ierr)
            IF (ierr/=0)  CALL errore(subname,' deallocating indxfroz ',ABS(ierr))
       ENDIF
       IF ( ASSOCIATED(indxnfroz) ) THEN
            DEALLOCATE(indxnfroz, STAT=ierr)
            IF (ierr/=0)  CALL errore(subname,' deallocating indxnfroz ',ABS(ierr))
       ENDIF
       IF ( ASSOCIATED(frozen) ) THEN
            DEALLOCATE(frozen, STAT=ierr)
            IF (ierr/=0)  CALL errore(subname,' deallocating frozen ',ABS(ierr))
       ENDIF
   END SUBROUTINE windows_deallocate

END MODULE windows_module
