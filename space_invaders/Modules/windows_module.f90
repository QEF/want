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
   USE kpts_module, ONLY : nkpts
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
! SUBROUTINE windows_write(unit)
! SUBROUTINE windows_read(unit)

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
   LOGICAL                     :: comp_flag =.FALSE. ! whether COMPLEMENT subspace is null

   COMPLEX(dbl), POINTER       :: evc(:,:,:)         ! wfc, dim: npwk, mxdbnd, nkpts

   !
   ! ... frozen states
   INTEGER,      POINTER       :: dimfroz(:)         ! variable for using frozen
   INTEGER,      POINTER       :: indxfroz(:)        ! states which are kept equal
   INTEGER,      POINTER       :: indxnfroz(:)       ! dim: nkpts
   LOGICAL                     :: froz_flag =.FALSE. ! whether FROZEN states are present
   LOGICAL,      POINTER       :: frozen(:,:)        ! which are the frozen states
                                                     ! dim: mxdbnd, nkpts
!
! end of declarations
!

   PUBLIC :: nkpts, mxdbnd
   PUBLIC :: dimwin, imin, imax, eig, comp_flag
   PUBLIC :: evc
   PUBLIC :: dimfroz, indxfroz, indxnfroz, froz_flag, frozen

CONTAINS

!
! subroutines
!

!**********************************************************
   SUBROUTINE allocate(iatm,pdos)
   !**********************************************************
   USE grids_module, ONLY : icalc, egrid_set
   IMPLICIT NONE
      INTEGER,                        INTENT(in) :: iatm
      TYPE(projdos),                  INTENT(in) :: pdos

END MODULE windows_module
