!
! Copyright (C) 2005 WanT Group
!
! This file is distributed under the terms of the
! GNU General Public License. See the file `License\'
! in the root directory of the present distribution,
! or http://www.gnu.org/copyleft/gpl.txt .
!

!*********************************************
   MODULE trial_center_data_module
!*********************************************
   USE kinds,           ONLY : dbl
   USE trial_center_module
   USE subspace_module, ONLY : dimwann
   IMPLICIT NONE
   PRIVATE
   SAVE

! This module contains data related to the trial orbitals
! for WF localization.
! Basic definitions are in trial_center_module
!
! routines in this module:
! SUBROUTINE trial_center_data_allocate()
! SUBROUTINE trial_center_data_deallocate()
!

!
   TYPE(trial_center), ALLOCATABLE  :: trial(:)
   LOGICAL                          :: alloc = .FALSE.

!
! end of declarations
!

   PUBLIC :: dimwann
   PUBLIC :: trial
   PUBLIC :: trial_center_data_allocate
   PUBLIC :: trial_center_data_deallocate
   PUBLIC :: alloc

CONTAINS

!
! subroutines
!

!**********************************************************
   SUBROUTINE trial_center_data_allocate()
   !**********************************************************
   IMPLICIT NONE
      CHARACTER(26)  :: subname='trial_center_data_allocate'
      INTEGER        :: ierr

      IF ( alloc ) CALL errore(subname,'trial_centers already allocated',1)
      IF ( dimwann <= 0 ) CALL errore(subname,'invalid dimwann',-dimwann+1)
      ALLOCATE( trial(dimwann), STAT=ierr )
         IF (ierr/=0)  CALL errore(subname,'allocating trial_centers',ABS(ierr))
      alloc = .TRUE.
   END SUBROUTINE trial_center_data_allocate


!**********************************************************
   SUBROUTINE trial_center_data_deallocate()
   !**********************************************************
   IMPLICIT NONE
      CHARACTER(28)  :: subname='trial_center_data_deallocate'
      INTEGER        :: ierr

      IF ( .NOT. alloc ) CALL errore(subname,'trial_centers not yet allocated',1)
      DEALLOCATE( trial, STAT=ierr )
         IF (ierr/=0)  CALL errore(subname,'deallocating trial_centers',ABS(ierr))
      RETURN
   END SUBROUTINE trial_center_data_deallocate


END MODULE trial_center_data_module

