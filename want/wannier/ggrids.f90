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
   USE parameters, ONLY : nstrx
   USE windows_module, ONLY : nkpts
   USE iotk_module
   USE parser_module, ONLY : change_case
   IMPLICIT NONE
   PRIVATE
   SAVE

! This module handles data referring the G vector Grids
! used in PW representation
!
! routines in this module:
! SUBROUTINE ggrids_allocate()
! SUBROUTINE ggrids_deallocate()
! SUBROUTINE ggrids_read_ext(unit)

!
! declarations of common variables
!   

   INTEGER                   :: npw              ! number of G vects for the density
   INTEGER                   :: nr(3)            ! dimension of the FFT space grid
   !
   REAL(dbl)                 :: ecutwfc          ! energy cutoff for wfc(Ry)
   REAL(dbl)                 :: ecutrho          ! energy cutoff for dthe density (Ry)
   INTEGER,      ALLOCATABLE :: igv(:,:)         ! G vect components, DIM: 3*npw
   
   LOGICAL :: alloc = .FALSE.

!
! end of declarations
!

   PUBLIC :: npw, nr 
   PUBLIC :: ecutwfc, ecutrho, igv
   PUBLIC :: alloc

   PUBLIC :: ggrids_allocate, ggrids_deallocate
   PUBLIC :: ggrids_read_ext

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

       IF ( npw <= 0 ) CALL errore(subname,'npw <= 0',ABS(npw)+1)
       IF ( nkpts <= 0 )  CALL errore(subname,'nkpts <= 0',ABS(nkpts)+1)

       ALLOCATE( igv(3,npw), STAT=ierr )
          IF (ierr/=0) CALL errore(subname,'allocating npw',3*npw)
       alloc = .TRUE.
      
   END SUBROUTINE ggrids_allocate


!**********************************************************
   SUBROUTINE ggrids_deallocate()
   !**********************************************************
   IMPLICIT NONE
       CHARACTER(20)      :: subname="ggrids_deallocate"
       INTEGER            :: ierr

       IF ( ALLOCATED(igv) ) THEN
            DEALLOCATE(igv, STAT=ierr)
            IF (ierr/=0)  CALL errore(subname,' deallocating igv ',ABS(ierr))
       ENDIF
       alloc = .FALSE.

   END SUBROUTINE ggrids_deallocate


!*********************************************************
   SUBROUTINE ggrids_read_ext(unit)
   !*********************************************************
   IMPLICIT NONE
       INTEGER,           INTENT(in) :: unit
       CHARACTER(nstrx)   :: attr
       CHARACTER(nstrx)   :: str
       CHARACTER(15)      :: subname="ggrids_read_ext"
       INTEGER            :: ierr

       !
       ! ... Various parameters
       !
       CALL iotk_scan_begin(unit,'Other_parameters',IERR=ierr)
       IF (ierr/=0)  CALL errore(subname,'Unable to find Other_parameters',ABS(ierr))
       !
       CALL iotk_scan_empty(unit,"Cutoff",ATTR=attr,IERR=ierr)
       IF (ierr/=0) CALL errore(subname,'unable to find Cutoff',ABS(ierr))
       CALL iotk_scan_attr(attr,"wfc",ecutwfc,IERR=ierr)
       IF (ierr/=0) CALL errore(subname,'unable to find ecutwfc',ABS(ierr))
       CALL iotk_scan_attr(attr,"rho",ecutrho,IERR=ierr)
       IF (ierr/=0) CALL errore(subname,'unable to find ecutrho',ABS(ierr))
       CALL iotk_scan_attr(attr,"units",str,IERR=ierr)
       IF (ierr/=0) CALL errore(subname,'unable to find units',ABS(ierr))
       CALL change_case(str,'UPPER')
       IF ( TRIM(str) /= 'RYDBERG' .AND. TRIM(str) /= 'RY' .AND. TRIM(str) /= 'RYD') &
            CALL errore(subname,'Cutoff units not in Rydberg',3)

       CALL iotk_scan_empty(unit,"Space_grid",ATTR=attr,IERR=ierr)
       IF (ierr/=0) CALL errore(subname,'unable to find Space_grid',ABS(ierr))
       CALL iotk_scan_attr(attr,"nr1",nr(1),IERR=ierr)
       IF (ierr/=0) CALL errore(subname,'unable to find nr1',ABS(ierr))
       CALL iotk_scan_attr(attr,"nr2",nr(2),IERR=ierr)
       IF (ierr/=0) CALL errore(subname,'unable to find nr2',ABS(ierr))
       CALL iotk_scan_attr(attr,"nr3",nr(3),IERR=ierr)
       IF (ierr/=0) CALL errore(subname,'unable to find nr3',ABS(ierr))
       !
       CALL iotk_scan_end(unit,'Other_parameters',IERR=ierr)
       IF (ierr/=0)  CALL errore(subname,'Unable to end tag Other_parameters',ABS(ierr))

       !
       ! ... Main G grid (density)
       !
       CALL iotk_scan_begin(unit,'Main_grid',ATTR=attr,IERR=ierr)
       IF (ierr/=0)  CALL errore(subname,'Unable to find Main_grid',ABS(ierr))
       !
       CALL iotk_scan_attr(attr,"npw",npw,IERR=ierr)
       IF (ierr/=0) CALL errore(subname,'unable to find npw',ABS(ierr))
       CALL ggrids_allocate() 

       CALL iotk_scan_dat(unit,"g",igv(:,:),IERR=ierr)
       IF (ierr/=0) CALL errore(subname,'unable to find igv',ABS(ierr))
       !
       CALL iotk_scan_end(unit,'Main_grid',IERR=ierr)
       IF (ierr/=0)  CALL errore(subname,'Unable to end tag Main_grid',ABS(ierr))

   END SUBROUTINE ggrids_read_ext

END MODULE ggrids_module

