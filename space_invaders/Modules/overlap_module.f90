!
! Copyright (C) 2004 Andrea Ferretti
!
! This file is distributed under the terms of the
! GNU General Public License. See the file `License\'
! in the root directory of the present distribution,
! or http://www.gnu.org/copyleft/gpl.txt .
!

!*********************************************
   MODULE overlap_module
!*********************************************
   USE kinds, ONLY : dbl
   USE windows_module, ONLY : mxdbnd, windows_alloc => alloc
   USE kpoints_module, ONLY : nkpts, mxdnn, kpoints_alloc => alloc
   USE input_module,  ONLY : dimwann
   USE iotk_module
   USE parameters, ONLY : nstrx
   IMPLICIT NONE
   PRIVATE
   SAVE

! This module handles data referring to OVERLAP among
! the periodic part of bloch wfcs and thier projections
! onto the localized starting orbitals.
!
! routines in this module:
! SUBROUTINE overlap_allocate()
! SUBROUTINE overlap_deallocate()
! SUBROUTINE overlap_write(unit,name)
! SUBROUTINE overlap_read(unit,name,found)

!
! declarations of common variables
!   

   COMPLEX(dbl), ALLOCATABLE   :: cm(:,:,:,:)    ! <u_nk|u_mk+b> overlap
                                                 ! DIM: mxdbnd,mxdbnd,mxdnn,nkpts
   COMPLEX(dbl), ALLOCATABLE   :: ca(:,:,:)      ! <u_nk|phi_lk> projection
                                                 ! DIM: mxdbnd,dimwann,nkpts
   LOGICAL :: alloc = .FALSE.
   

!
! end of declarations
!

   PUBLIC :: cm, ca 
   PUBLIC :: overlap_allocate
   PUBLIC :: overlap_deallocate
   PUBLIC :: overlap_write
   PUBLIC :: overlap_read

CONTAINS

!
! subroutines
!

!**********************************************************
   SUBROUTINE overlap_allocate()
   !**********************************************************
   IMPLICIT NONE
       CHARACTER(16)      :: subname="overlap_allocate"
       INTEGER            :: ierr 

       IF ( mxdbnd <= 0 .OR. nkpts <= 0 .OR. dimwann <= 0) &
           CALL errore(subname,' Invalid MXDBND, NKPTS, DIMWANN ',1)

       ALLOCATE( cm(mxdbnd,mxdbnd,mxdnn,nkpts), STAT=ierr )       
           IF ( ierr/=0 ) CALL errore(subname,' allocating cm ',mxdbnd**2*mxdnn*nkpts)

       ALLOCATE( ca(mxdbnd,dimwann,nkpts), STAT=ierr )       
           IF ( ierr/=0 ) CALL errore(subname,' allocating ca ',mxdbnd*dimwann*nkpts)

       alloc = .TRUE. 
      
   END SUBROUTINE overlap_allocate


!**********************************************************
   SUBROUTINE overlap_deallocate()
   !**********************************************************
   IMPLICIT NONE
       CHARACTER(18)      :: subname="overlap_deallocate"
       INTEGER            :: ierr 

       IF ( ALLOCATED(cm) ) THEN
            DEALLOCATE(cm, STAT=ierr)
            IF (ierr/=0)  CALL errore(subname,' deallocating cm ',ABS(ierr))
       ENDIF
       IF ( ALLOCATED(ca) ) THEN
            DEALLOCATE(ca, STAT=ierr)
            IF (ierr/=0)  CALL errore(subname,' deallocating ca ',ABS(ierr))
       ENDIF
       alloc = .FALSE.

   END SUBROUTINE overlap_deallocate


!**********************************************************
   SUBROUTINE overlap_write(unit,name)
   !**********************************************************
   IMPLICIT NONE
       INTEGER,         INTENT(in) :: unit
       CHARACTER(*),    INTENT(in) :: name
       CHARACTER(nstrx)   :: attr
       CHARACTER(13)      :: subname="overlap_write"

       IF ( .NOT. alloc ) RETURN
       CALL iotk_write_begin(unit,TRIM(name))
       CALL iotk_write_attr(attr,"mxdbnd",mxdbnd,FIRST=.TRUE.)
       CALL iotk_write_attr(attr,"dimwann",dimwann)
       CALL iotk_write_attr(attr,"mxdnn",mxdnn)
       CALL iotk_write_attr(attr,"nkpts",nkpts)
       CALL iotk_write_empty(unit,"DATA",ATTR=attr)

       CALL iotk_write_dat(unit,"OVERLAP",cm,FMT="(2f15.9)")
       CALL iotk_write_dat(unit,"PROJECTIONS",ca,FMT="(2f15.9)")

       CALL iotk_write_end(unit,TRIM(name))
   END SUBROUTINE overlap_write


!**********************************************************
   SUBROUTINE overlap_read(unit,name,found)
   !**********************************************************
   IMPLICIT NONE
       INTEGER,           INTENT(in) :: unit
       CHARACTER(*),      INTENT(in) :: name
       LOGICAL,           INTENT(out):: found
       CHARACTER(nstrx)   :: attr
       CHARACTER(12)      :: subname="overlap_read"
       INTEGER            :: mxdbnd_, dimwann_, mxdnn_, nkpts_
       INTEGER            :: ierr

       IF ( alloc ) CALL overlap_deallocate()
       
       CALL iotk_scan_begin(unit,TRIM(name),FOUND=found,IERR=ierr)
       IF (.NOT. found) RETURN
       IF (ierr>0)  CALL errore(subname,'Wrong format in tag '//TRIM(name),ierr)
       found = .TRUE.

       CALL iotk_scan_empty(unit,'DATA',ATTR=attr,IERR=ierr)
       IF (ierr/=0) CALL errore(subname,'Unable to find tag DATA',ABS(ierr))

       CALL iotk_scan_attr(attr,'mxdbnd',mxdbnd_,IERR=ierr)
          IF (ierr/=0) CALL errore(subname,'Unable to find attr MXDBND',ABS(ierr))
       CALL iotk_scan_attr(attr,'dimwann',dimwann_,IERR=ierr)
          IF (ierr/=0) CALL errore(subname,'Unable to find attr DIMWANN',ABS(ierr))
       CALL iotk_scan_attr(attr,'mxdnn',mxdnn_,IERR=ierr)
          IF (ierr/=0) CALL errore(subname,'Unable to find attr MXDNN',ABS(ierr))
       CALL iotk_scan_attr(attr,'nkpts',nkpts_,IERR=ierr)
          IF (ierr/=0) CALL errore(subname,'Unable to find attr NKPTS',ABS(ierr))

       !
       ! ... various checks
       IF ( windows_alloc ) THEN
          IF ( mxdbnd_ /= mxdbnd) CALL errore(subname,'Invalid MXDBND',ABS(mxdbnd_-mxdbnd))
       ELSE
          mxdbnd = mxdbnd_
       ENDIF
       !
       IF ( kpoints_alloc ) THEN
          IF ( nkpts_ /= nkpts) CALL errore(subname,'Invalid NKPTS',ABS(nkpts_-nkpts))
          IF ( mxdnn_ /= mxdnn) CALL errore(subname,'Invalid MXDNN',ABS(mxdnn_-mxdnn))
       ELSE
          nkpts = nkpts_
       ENDIF
       IF ( dimwann_ /= dimwann) CALL errore(subname,'Invalid DIMWANN',ABS(dimwann_-dimwann))

       !
       CALL overlap_allocate()       

       CALL iotk_scan_dat(unit,'OVERLAP',cm,IERR=ierr)
          IF (ierr/=0) CALL errore(subname,'Unable to find OVERLAP',ABS(ierr))
       CALL iotk_scan_dat(unit,'PROJECTIONS',ca,IERR=ierr)
          IF (ierr/=0) CALL errore(subname,'Unable to find PROJECTIONS',ABS(ierr))

       CALL iotk_scan_end(unit,TRIM(name),IERR=ierr)
       IF (ierr/=0)  CALL errore(subname,'Unable to end tag '//TRIM(name),ABS(ierr))
   END SUBROUTINE overlap_read

END MODULE overlap_module

