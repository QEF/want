!
! Copyright (C) 2004 Andrea Ferretti
!
! This file is distributed under the terms of the
! GNU General Public License. See the file `License\'
! in the root directory of the present distribution,
! or http://www.gnu.org/copyleft/gpl.txt .
!

!*********************************************
   MODULE ggrids_module
!*********************************************
   USE kinds, ONLY : dbl
   USE windows_module, ONLY : nkpts
   USE iotk_module
   IMPLICIT NONE
   PRIVATE
   SAVE

! This module handles data referring the G vector Grids
! used in PW representation
!
! routines in this module:
! SUBROUTINE ggrids_allocate()
! SUBROUTINE ggrids_deallocate()
! SUBROUTINE ggrids_write(unit,name)
! SUBROUTINE ggrids_read(unit,name,found)

!
! declarations of common variables
!   

   INTEGER                   :: mxdgve           ! max number of G vects for the density
   INTEGER                   :: npwx             ! max number of G vects over kpts
   INTEGER,      ALLOCATABLE :: npwk(:)          ! number of G for each kpt, DIM: nkpts
   !
   REAL(dbl)                 :: ecut             ! energy cutoff (Ha)
   INTEGER,      ALLOCATABLE :: igv(:,:)         ! G vect components, DIM: 3*mxdgve
   

!
! end of declarations
!

   PUBLIC :: mxdgve, npwx
   PUBLIC :: npwk
   PUBLIC :: ecut, igv
   PUBLIC :: ggrids_allocate, ggrids_deallocate

CONTAINS

!
! subroutines
!

!**********************************************************
   SUBROUTINE ggrids_allocate()
   !**********************************************************
   IMPLICIT NONE
       CHARACTER(18)      :: subname="ggrids_allocate"
       INTEGER            :: ierr 

       IF ( mxdgve <= 0 ) CALL errore(subname,'mxdgve <= 0',ABS(mxdgve)+1)
       IF ( nkpts <= 0 )  CALL errore(subname,'nkpts <= 0',ABS(nkpts)+1)

       ALLOCATE( npwk(nkpts), STAT=ierr )
          IF (ierr/=0) CALL errore(subname,'allocating npwk',nkpts)
       ALLOCATE( igv(3,mxdgve), STAT=ierr )
          IF (ierr/=0) CALL errore(subname,'allocating mxdgve',3*mxdgve)
      
   END SUBROUTINE ggrids_allocate


!**********************************************************
   SUBROUTINE ggrids_deallocate()
   !**********************************************************
   IMPLICIT NONE
       CHARACTER(20)      :: subname="ggrids_deallocate"
       INTEGER            :: ierr

       IF ( ALLOCATED(npwk) ) THEN
            DEALLOCATE(npwk, STAT=ierr)
            IF (ierr/=0)  CALL errore(subname,' deallocating npwk ',ABS(ierr))
       ENDIF
       IF ( ALLOCATED(igv) ) THEN
            DEALLOCATE(igv, STAT=ierr)
            IF (ierr/=0)  CALL errore(subname,' deallocating igv ',ABS(ierr))
       ENDIF

   END SUBROUTINE ggrids_deallocate


END MODULE ggrids_module

