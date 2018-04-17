!
! Copyright (C) 2005 WanT Group
!
! This file is distributed under the terms of the
! GNU General Public License. See the file `License'
! in the root directory of the present distribution,
! or http://www.gnu.org/copyleft/gpl.txt .
!
!
!*********************************************
   MODULE dft_interface_module
!*********************************************
   USE iotk_module
   IMPLICIT NONE
   PRIVATE

! This module contains basic routines to read data from
! DFT data file
!
! routines in this module:
! SUBROUTINE dft_interface_read_spin(unit,nkpts,nspin)
!

  PUBLIC :: dft_interface_read_spin


CONTAINS

!**********************************************************
   SUBROUTINE dft_interface_read_spin(unit,nkpts,nspin)
   !**********************************************************
   IMPLICIT NONE
      INTEGER, INTENT(in)   :: unit
      INTEGER, INTENT(out)  :: nkpts, nspin

      INTEGER :: ierr
      LOGICAL :: lfound
      CHARACTER(iotk_attlenx):: attr
      CHARACTER(23) :: subname="dft_interface_read_spin"

      !
      !
      CALL iotk_scan_begin(unit,"Dimensions", FOUND=lfound, IERR=ierr)
      IF ( .NOT. lfound ) CALL errore(subname,'searching for Dimensions',1)
      IF (ierr/=0) CALL errore(subname,'Invalid fmt in Dimensions',ABS(ierr))

      CALL iotk_scan_empty(unit,"Kpoints",ATTR=attr,IERR=ierr)
      IF (ierr/=0) CALL errore(subname,'searching for Kpoints',ABS(ierr))

      CALL iotk_scan_attr(attr,"nktot",nkpts,IERR=ierr)
      IF (ierr/=0) CALL errore(subname,'searching for nktot',ABS(ierr))
      CALL iotk_scan_attr(attr,"nspin",nspin,IERR=ierr)
      IF (ierr/=0) CALL errore(subname,'searching for nspin',ABS(ierr))

      CALL iotk_scan_end(unit,"Dimensions", IERR=ierr)
      IF (ierr/=0) CALL errore(subname,'searching for Dimensions endtag',ABS(ierr))

   END SUBROUTINE dft_interface_read_spin

END MODULE dft_interface_module

