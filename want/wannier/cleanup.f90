! 
! Copyright (C) 2004 Andrea Ferretti
! 
! This file is distributed under the terms of the 
! GNU General Public License. See the file `License' 
! in the root directory of the present distribution, 
! or http://www.gnu.org/copyleft/gpl.txt . 
! 
! <INFO>
!*********************************************
   MODULE cleanup_module
!*********************************************
   IMPLICIT NONE
   PRIVATE

! This module contains the routine CLEANUP that
! that deallocates all the data stored in modules
! in the WanT code. If data is not allocated the routine
! goes through and nothing is done.
!
! routines in this module:
! SUBROUTINE cleanup()
! </INFO>


   PUBLIC :: cleanup


CONTAINS 
   
!
! Subroutines
!

!**********************************************************
   SUBROUTINE cleanup()
!**********************************************************
      USE input_module,        ONLY : input_deallocate, input_alloc => alloc 
      USE kpoints_module,      ONLY : kpoints_deallocate, kpoints_alloc 
      USE kpoints_module,      ONLY : bshells_deallocate, bshells_alloc
      USE windows_module,      ONLY : windows_deallocate, windows_alloc => alloc 
      USE subspace_module,     ONLY : subspace_deallocate, subspace_alloc => alloc 
      USE overlap_module,      ONLY : overlap_deallocate, overlap_alloc => alloc 
      USE ggrids_module,       ONLY : ggrids_deallocate, ggrids_alloc => alloc 
      USE wfc_module,          ONLY : wfc_deallocate, wfc_alloc => alloc 
      USE localization_module, ONLY : localization_deallocate, loc_alloc => alloc 
      USE timing_module,       ONLY : timing_deallocate, timing_alloc => alloc 
      USE struct_fact_data_module,  &
                               ONLY : struct_fact_data_deallocate, strf_alloc => alloc 
      USE us_module,           ONLY : us_deallocate
      USE uspp,                ONLY : uspp_deallocate
      IMPLICIT NONE
      
      IF ( input_alloc )    CALL input_deallocate()
      IF ( kpoints_alloc )  CALL kpoints_deallocate()
      IF ( bshells_alloc )  CALL bshells_deallocate()
      IF ( windows_alloc )  CALL windows_deallocate()
      IF ( subspace_alloc ) CALL subspace_deallocate()
      IF ( overlap_alloc )  CALL overlap_deallocate()
      IF ( ggrids_alloc )   CALL ggrids_deallocate()
      IF ( wfc_alloc )      CALL wfc_deallocate()
      IF ( loc_alloc )      CALL localization_deallocate()
      IF ( timing_alloc )   CALL timing_deallocate()
      IF ( strf_alloc )     CALL struct_fact_data_deallocate()
                            CALL us_deallocate()
                            CALL uspp_deallocate()

   END SUBROUTINE cleanup

END MODULE cleanup_module
