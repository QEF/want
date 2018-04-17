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
   USE constants,  ONLY : ZERO, TPI
   USE parameters, ONLY : nstrx
   USE log_module, ONLY : log_push, log_pop
   USE qexml_module
   USE qexpt_module
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
    USE io_module, ONLY: stdout
    IMPLICIT NONE

       CALL log_push ( "lattice_init" )
       !
       ! avec and bvec are in units of bohr and bohr^-1 respectively
       ! omega in bohr^3, alat and tpiba in bohr and bohr^-1
       !
       CALL recips( avec(:,1), avec(:,2), avec(:,3), bvec(:,1), bvec(:,2), bvec(:,3), omega)
       bvec = bvec * TPI
       !
       alloc = .TRUE.
       !
       CALL log_pop ( "lattice_init" )
       !
   END SUBROUTINE lattice_init


!*********************************************************
   SUBROUTINE lattice_read_ext(filefmt)
   !*********************************************************
   IMPLICIT NONE
       CHARACTER(*)       :: filefmt
       CHARACTER(16)      :: subname="lattice_read_ext"
       INTEGER            :: ierr

       CALL log_push ( subname )
       ierr = 0
       !
       SELECT CASE ( TRIM(filefmt) )
       !
       CASE ( 'qexml' )
           !
           CALL qexml_read_cell( ALAT=alat, A1=avec(:,1), A2=avec(:,2),  &
                                            A3=avec(:,3), IERR=ierr)
           !
       CASE ( 'pw_export' )
           !
           CALL qexpt_read_cell( ALAT=alat, A1=avec(:,1), A2=avec(:,2),  &
                                            A3=avec(:,3), IERR=ierr)
           !
       CASE DEFAULT
           !
           CALL errore(subname,'invalid fmt ='//TRIM(filefmt),1)
       END SELECT
       !
       IF (ierr/=0) CALL errore(subname,'reading lattice',ABS(ierr))
       !
       ! impose the normalization
       !
       tpiba = TPI / alat
       !
       CALL log_pop ( subname )
       !
   END SUBROUTINE lattice_read_ext

END MODULE lattice_module
