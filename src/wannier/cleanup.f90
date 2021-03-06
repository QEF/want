! 
! Copyright (C) 2004 WanT Group
! 
! This file is distributed under the terms of the 
! GNU General Public License. See the file `License' 
! in the root directory of the present distribution, 
! or http://www.gnu.org/copyleft/gpl.txt . 
! 
!**********************************************************
   SUBROUTINE cleanup()
   !**********************************************************
   !
   ! This routine deallocates all the data stored in modules
   ! in the WanT code. If data is not allocated the routine
   ! goes through and nothing happens.
   !
   USE kpoints_module,           ONLY : kpoints_deallocate, kpoints_alloc 
   USE kpoints_module,           ONLY : rgrid_deallocate,   rgrid_alloc
   USE kpoints_module,           ONLY : bshells_deallocate, bshells_alloc
   USE ions_module,              ONLY : ions_deallocate, ions_alloc => alloc    
   USE symmetry_module,          ONLY : symmetry_deallocate, symm_alloc => alloc
   USE windows_module,           ONLY : windows_deallocate, windows_alloc => alloc 
   USE subspace_module,          ONLY : subspace_deallocate, subspace_alloc => alloc 
   USE overlap_module,           ONLY : overlap_deallocate, overlap_alloc => alloc 
   USE ggrids_module,            ONLY : ggrids_deallocate, ggrids_alloc => alloc 
   USE wfc_data_module,          ONLY : wfc_data_deallocate, wfc_data_alloc => alloc 
   USE localization_module,      ONLY : localization_deallocate, loc_alloc => alloc 
   USE trial_center_data_module, ONLY : trial_center_data_deallocate, trial_alloc => alloc 
   USE struct_fact_data_module,  ONLY : struct_fact_data_deallocate, strf_alloc => alloc 
   USE us_module,                ONLY : us_deallocate
   USE uspp,                     ONLY : uspp_deallocate
   USE uspp_param,               ONLY : uspp_param_deallocate
   USE atom_module,              ONLY : atom_deallocate
   USE hamiltonian_module,       ONLY : hamiltonian_deallocate, ham_alloc => alloc
   USE correlation_module,       ONLY : correlation_deallocate, corr_alloc => alloc
   USE workspace_wan_module,     ONLY : workspace_wan_deallocate, workwan_alloc => alloc
   USE workspace_dis_module,     ONLY : workspace_dis_deallocate, workdis_alloc => alloc
   USE paratools_module,         ONLY : paratools_deallocate, paratools_alloc => alloc
   !
   IMPLICIT NONE
      
      IF ( kpoints_alloc )  CALL kpoints_deallocate()
      IF ( rgrid_alloc )    CALL rgrid_deallocate()
      IF ( bshells_alloc )  CALL bshells_deallocate()
      IF ( ions_alloc )     CALL ions_deallocate()
      IF ( symm_alloc )     CALL symmetry_deallocate()
      IF ( windows_alloc )  CALL windows_deallocate()
      IF ( subspace_alloc ) CALL subspace_deallocate()
      IF ( overlap_alloc )  CALL overlap_deallocate()
      IF ( ggrids_alloc )   CALL ggrids_deallocate()
      IF ( wfc_data_alloc ) CALL wfc_data_deallocate()
      IF ( loc_alloc )      CALL localization_deallocate()
      IF ( trial_alloc )    CALL trial_center_data_deallocate()
      IF ( ham_alloc )      CALL hamiltonian_deallocate()
      IF ( corr_alloc )     CALL correlation_deallocate()
      IF ( strf_alloc )     CALL struct_fact_data_deallocate()
      IF ( workwan_alloc )  CALL workspace_wan_deallocate()
      IF ( workdis_alloc )  CALL workspace_dis_deallocate()
      IF ( paratools_alloc )CALL paratools_deallocate()
                            CALL us_deallocate()
                            CALL uspp_deallocate()
                            CALL uspp_param_deallocate()
                            CALL atom_deallocate()
                            CALL iotk_cleanup()

END SUBROUTINE cleanup

