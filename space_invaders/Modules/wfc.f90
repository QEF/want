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
   USE windows_module, ONLY : mxdbnd, nkpts
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

   COMPLEX(dbl), POINTER       :: evc(:,:,:)       ! wfc, dim: npwkx, mxdbnd, nkpts
   

!
! end of declarations
!

   PUBLIC :: evc

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
      
   END SUBROUTINE wfc_allocate

END MODULE wfc_module

