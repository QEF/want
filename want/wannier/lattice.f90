!
! Copyright (C) 2004 WanT Group
!
! This file is distributed under the terms of the
! GNU General Public License. See the file `License'
! in the root directory of the present distribution,
! or http://www.gnu.org/copyleft/gpl.txt .
!
!*********************************************************
MODULE lattice_module
   !*********************************************************
   USE kinds
   USE constants, ONLY : ZERO
   USE parameters, ONLY : nstrx
   USE iotk_module
   IMPLICIT NONE
   PRIVATE
   SAVE
!
! <INFO>
! This module contains all the variables and the subroutines 
! used to manage the lattice data.
!
! Subroutines in this module:
! SUBROUTINE lattice_init()
! SUBROUTINE lattice_read_ext(unit, name, found)
!
! </INFO>
!

  REAL(dbl)        :: alat  = ZERO        ! dir space units (Bohr)
  REAL(dbl)        :: tpiba = ZERO        ! 2pi/alat (Bohr^-1)
  REAL(dbl)        :: omega = ZERO        ! volume of the unit cell (Bohr^3)
  REAL(dbl)        :: avec(3,3)           ! dir lattice vects (Bohr)
  REAL(dbl)        :: bvec(3,3)           ! rec lattice vects (Bohr^-1)

  LOGICAL          :: alloc = .FALSE.

!
! ... end of declarations
!

  PUBLIC :: alat
  PUBLIC :: tpiba
  PUBLIC :: omega
  PUBLIC :: avec, bvec
  PUBLIC :: alloc
  PUBLIC :: lattice_read_ext, lattice_init

CONTAINS

!
! Subroutines
!

!*********************************************************
   SUBROUTINE lattice_init()
   !*********************************************************
    USE constants, ONLY: PI, ZERO, ONE, TPI
    USE io_module, ONLY: stdout
    IMPLICIT NONE

    !
    ! avec and bvec are in units of bohr and bohr^-1 respectively
    ! omega in bohr^3, alat and tpiba in bohr and bohr^-1
    !
    CALL recips( avec(:,1), avec(:,2), avec(:,3), bvec(:,1), bvec(:,2), bvec(:,3), omega)
    bvec = bvec * TPI

    alloc = .TRUE.

    RETURN
   END SUBROUTINE lattice_init

!*********************************************************
   SUBROUTINE lattice_read_ext(unit, name, found)
   !*********************************************************
   IMPLICIT NONE
       INTEGER,           INTENT(in) :: unit
       CHARACTER(*),      INTENT(in) :: name
       LOGICAL,           INTENT(out):: found
       CHARACTER(nstrx)   :: attr
       CHARACTER(16)      :: subname="lattice_read_ext"
       INTEGER            :: ierr

       CALL iotk_scan_begin(unit,TRIM(name),FOUND=found,IERR=ierr)
       IF (.NOT. found) RETURN
       IF (ierr>0)  CALL errore(subname,'Wrong format in tag '//TRIM(name),ierr)
       found = .TRUE.

       CALL iotk_scan_empty(unit,"Data",ATTR=attr,IERR=ierr)
       IF (ierr/=0) CALL errore(subname,'unable to find Data',ABS(ierr))
       CALL iotk_scan_attr(attr,"alat",alat,IERR=ierr)
       IF (ierr/=0) CALL errore(subname,'unable to find alat',ABS(ierr))
       CALL iotk_scan_attr(attr,"tpiba",tpiba,IERR=ierr)
       IF (ierr/=0) CALL errore(subname,'unable to find tpiba',ABS(ierr))

       CALL iotk_scan_empty(unit,"a1",ATTR=attr,IERR=ierr)
       IF (ierr/=0) CALL errore(subname,'unable to find a1',ABS(ierr))
       CALL iotk_scan_attr(attr,"xyz",avec(:,1),IERR=ierr)
       IF (ierr/=0) CALL errore(subname,'unable to find xyz in a1',ABS(ierr))
       CALL iotk_scan_empty(unit,"a2",ATTR=attr,IERR=ierr)
       IF (ierr/=0) CALL errore(subname,'unable to find a2',ABS(ierr))
       CALL iotk_scan_attr(attr,"xyz",avec(:,2),IERR=ierr)
       IF (ierr/=0) CALL errore(subname,'unable to find xyz in a2',ABS(ierr))
       CALL iotk_scan_empty(unit,"a3",ATTR=attr,IERR=ierr)
       IF (ierr/=0) CALL errore(subname,'unable to find a3',ABS(ierr))
       CALL iotk_scan_attr(attr,"xyz",avec(:,3),IERR=ierr)
       IF (ierr/=0) CALL errore(subname,'unable to find xyz in a3',ABS(ierr))

       CALL iotk_scan_end(unit,TRIM(name),IERR=ierr)
       IF (ierr/=0)  CALL errore(subname,'Unable to end tag '//TRIM(name),ABS(ierr))
   END SUBROUTINE lattice_read_ext

END MODULE lattice_module
