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
   USE parameters, ONLY : nstrx
   USE kpoints_module, ONLY : nkpts, kpoints_alloc => alloc
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
! SUBROUTINE windows_write(unit,name)
! SUBROUTINE windows_read(unit,name,found)

!
! declarations of common variables
!   

   INTEGER                     :: mxdbnd             ! number of DFT bands
   INTEGER                     :: dimwinx            ! MAX (dimwin(:)) over kpts
   !
   ! ... starting states within the energy window
   INTEGER,      ALLOCATABLE   :: dimwin(:)          ! define which eigenv are in the
   INTEGER,      ALLOCATABLE   :: imin(:)            ! chosen energy window
   INTEGER,      ALLOCATABLE   :: imax(:)            ! dim: nkpts
   REAL(dbl),    ALLOCATABLE   :: eiw(:,:)           ! DFT eigenv; dim: mxdbnd, nkpts
   LOGICAL                     :: lcompspace=.TRUE.  ! whether COMPLEMENT space is NOT null

   !
   ! ... frozen states
   INTEGER,      ALLOCATABLE   :: dimfroz(:)         ! variable for using frozen (dim: nkpts)
   INTEGER,      ALLOCATABLE   :: indxfroz(:,:)      ! states which are kept equal
   INTEGER,      ALLOCATABLE   :: indxnfroz(:,:)     ! dim: mxdbnd nkpts
   LOGICAL                     :: lfrozen =.FALSE.   ! whether FROZEN states are present
   LOGICAL,      ALLOCATABLE   :: frozen(:,:)        ! which are the frozen states
                                                     ! dim: mxdbnd, nkpts
   LOGICAL                     :: alloc=.FALSE.      ! 
!
! end of declarations
!

   PUBLIC :: nkpts, mxdbnd, dimwinx
   PUBLIC :: dimwin, imin, imax, eiw, lcompspace
   PUBLIC :: dimfroz, indxfroz, indxnfroz, lfrozen, frozen
   PUBLIC :: alloc

   PUBLIC :: windows_allocate
   PUBLIC :: windows_deallocate
   PUBLIC :: windows_write
   PUBLIC :: windows_read

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
       ALLOCATE( eiw(mxdbnd,nkpts), STAT=ierr )
           IF ( ierr/=0 ) CALL errore(subname,' allocating eiw ',mxdbnd*nkpts)

       ALLOCATE( dimfroz(nkpts), STAT=ierr )
           IF ( ierr/=0 ) CALL errore(subname,' allocating dimfroz ',nkpts) 
       ALLOCATE( indxfroz(mxdbnd,nkpts), STAT=ierr )
           IF ( ierr/=0 ) CALL errore(subname,' allocating indxfroz ',nkpts)
       ALLOCATE( indxnfroz(mxdbnd,nkpts), STAT=ierr )
           IF ( ierr/=0 ) CALL errore(subname,' allocating indxnfroz ',nkpts)
       ALLOCATE( frozen(mxdbnd,nkpts), STAT=ierr )
           IF ( ierr/=0 ) CALL errore(subname,' allocating frozen ',mxdbnd*nkpts)      

       alloc = .TRUE.

   END SUBROUTINE windows_allocate


!**********************************************************
   SUBROUTINE windows_deallocate()
   !**********************************************************
   IMPLICIT NONE
       CHARACTER(18)      :: subname="windows_deallocate"
       INTEGER            :: ierr

       IF ( ALLOCATED(dimwin) ) THEN
            DEALLOCATE(dimwin, STAT=ierr)
            IF (ierr/=0)  CALL errore(subname,' deallocating dimwin ',ABS(ierr))
       ENDIF
       IF ( ALLOCATED(imin) ) THEN
            DEALLOCATE(imin, STAT=ierr)
            IF (ierr/=0)  CALL errore(subname,' deallocating imin ',ABS(ierr))
       ENDIF
       IF ( ALLOCATED(imax) ) THEN
            DEALLOCATE(imax, STAT=ierr)
            IF (ierr/=0)  CALL errore(subname,' deallocating imax ',ABS(ierr))
       ENDIF
       IF ( ALLOCATED(eiw) ) THEN
            DEALLOCATE(eiw, STAT=ierr)
            IF (ierr/=0)  CALL errore(subname,' deallocating eiw ',ABS(ierr))
       ENDIF
       IF ( ALLOCATED(dimfroz) ) THEN
            DEALLOCATE(dimfroz, STAT=ierr)
            IF (ierr/=0)  CALL errore(subname,' deallocating dimfroz ',ABS(ierr))
       ENDIF
       IF ( ALLOCATED(indxfroz) ) THEN
            DEALLOCATE(indxfroz, STAT=ierr)
            IF (ierr/=0)  CALL errore(subname,' deallocating indxfroz ',ABS(ierr))
       ENDIF
       IF ( ALLOCATED(indxnfroz) ) THEN
            DEALLOCATE(indxnfroz, STAT=ierr)
            IF (ierr/=0)  CALL errore(subname,' deallocating indxnfroz ',ABS(ierr))
       ENDIF
       IF ( ALLOCATED(frozen) ) THEN
            DEALLOCATE(frozen, STAT=ierr)
            IF (ierr/=0)  CALL errore(subname,' deallocating frozen ',ABS(ierr))
       ENDIF
       alloc = .FALSE.
   END SUBROUTINE windows_deallocate


!**********************************************************
   SUBROUTINE windows_write(unit,name)
   !**********************************************************
   IMPLICIT NONE
       INTEGER,         INTENT(in) :: unit
       CHARACTER(*),    INTENT(in) :: name
       CHARACTER(nstrx)   :: attr
       CHARACTER(13)      :: subname="windows_write"

       IF ( .NOT. alloc ) RETURN
       
       CALL iotk_write_begin(unit,TRIM(name))
       CALL iotk_write_attr(attr,"mxdbnd",mxdbnd,FIRST=.TRUE.)
       CALL iotk_write_attr(attr,"nkpts",nkpts)
       CALL iotk_write_attr(attr,"dimwinx",dimwinx)
       CALL iotk_write_attr(attr,"lcompspace",lcompspace)
       CALL iotk_write_attr(attr,"lfrozen",lfrozen)
       CALL iotk_write_empty(unit,"DATA",ATTR=attr)

       CALL iotk_write_dat(unit,"DIMWIN",dimwin)
       CALL iotk_write_dat(unit,"IMIN",imin)
       CALL iotk_write_dat(unit,"IMAX",imax)
       CALL iotk_write_dat(unit,"EIW",eiw)

       CALL iotk_write_dat(unit,"DIMFROZ",dimfroz)
       CALL iotk_write_dat(unit,"INDXFROZ",indxfroz)
       CALL iotk_write_dat(unit,"INDXNFROZ",indxnfroz)
       CALL iotk_write_dat(unit,"FROZEN",frozen)

       CALL iotk_write_end(unit,TRIM(name))
   END SUBROUTINE windows_write
   

!**********************************************************
   SUBROUTINE windows_read(unit,name,found)
   !**********************************************************
   IMPLICIT NONE
       INTEGER,           INTENT(in) :: unit
       CHARACTER(*),      INTENT(in) :: name
       LOGICAL,           INTENT(out):: found
       CHARACTER(nstrx)   :: attr
       CHARACTER(12)      :: subname="windows_read"
       INTEGER            :: nkpts_
       INTEGER            :: ierr

       IF ( alloc ) CALL windows_deallocate()
    
       CALL iotk_scan_begin(unit,TRIM(name),FOUND=found,IERR=ierr)
       IF (.NOT. found) RETURN
       IF (ierr>0)  CALL errore(subname,'Wrong format in tag '//TRIM(name),ierr)
       found = .TRUE.

       CALL iotk_scan_empty(unit,'DATA',ATTR=attr,IERR=ierr)
       IF (ierr/=0) CALL errore(subname,'Unable to find tag DATA',ABS(ierr))
       CALL iotk_scan_attr(attr,'mxdbnd',mxdbnd,IERR=ierr)
       IF (ierr/=0) CALL errore(subname,'Unable to find attr MXDBND',ABS(ierr))
       CALL iotk_scan_attr(attr,'nkpts',nkpts_,IERR=ierr)
       IF (ierr/=0) CALL errore(subname,'Unable to find attr NKPTS',ABS(ierr))
       CALL iotk_scan_attr(attr,'dimwinx',dimwinx,IERR=ierr)
       IF (ierr/=0) CALL errore(subname,'Unable to find attr DIMWINX',ABS(ierr))
       CALL iotk_scan_attr(attr,'lcompspace',lcompspace,IERR=ierr)
       IF (ierr/=0) CALL errore(subname,'Unable to find attr LCOMPSPACE',ABS(ierr))
       CALL iotk_scan_attr(attr,'lfrozen',lfrozen,IERR=ierr)
       IF (ierr/=0) CALL errore(subname,'Unable to find attr LFROZEN',ABS(ierr))

       IF ( kpoints_alloc ) THEN
            IF ( nkpts_ /= nkpts ) CALL errore(subname,'Invalid NKPTS',ABS(nkpts_)+1)
       ELSE
            nkpts = nkpts_
       ENDIF
       CALL windows_allocate()

       CALL iotk_scan_dat(unit,'DIMWIN',dimwin,IERR=ierr)
       IF (ierr/=0) CALL errore(subname,'Unable to find DIMWIN',ABS(ierr))
       CALL iotk_scan_dat(unit,'IMIN',imin,IERR=ierr)
       IF (ierr/=0) CALL errore(subname,'Unable to find IMIN',ABS(ierr))
       CALL iotk_scan_dat(unit,'IMAX',imax,IERR=ierr)
       IF (ierr/=0) CALL errore(subname,'Unable to find IMAX',ABS(ierr))
       CALL iotk_scan_dat(unit,'EIW',eiw,IERR=ierr)
       IF (ierr/=0) CALL errore(subname,'Unable to find EIW',ABS(ierr))

       CALL iotk_scan_dat(unit,'DIMFROZ',dimfroz,IERR=ierr)
       IF (ierr/=0) CALL errore(subname,'Unable to find DIMFROZ',ABS(ierr))
       CALL iotk_scan_dat(unit,'INDXFROZ',indxfroz,IERR=ierr)
       IF (ierr/=0) CALL errore(subname,'Unable to find INDXFROZ',ABS(ierr))
       CALL iotk_scan_dat(unit,'INDXNFROZ',indxnfroz,IERR=ierr)
       IF (ierr/=0) CALL errore(subname,'Unable to find INDXNFROZ',ABS(ierr))
       CALL iotk_scan_dat(unit,'FROZEN',frozen,IERR=ierr)
       IF (ierr/=0) CALL errore(subname,'Unable to find FROZEN',ABS(ierr))

       CALL iotk_scan_end(unit,TRIM(name),IERR=ierr)
       IF (ierr/=0)  CALL errore(subname,'Unable to end tag '//TRIM(name),ABS(ierr))
   END SUBROUTINE windows_read

END MODULE windows_module
