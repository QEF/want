! 
! Copyright (C) 2008 WanT Group
! 
! This file is distributed under the terms of the 
! GNU General Public License. See the file `License' 
! in the root directory of the present distribution, 
! or http://www.gnu.org/copyleft/gpl.txt . 
!
# include "f_defs.h"
! 
!**********************************************************
   SUBROUTINE memusage( iunit )
   !**********************************************************
   !
   ! This subroutine writes a summary fo the memory 
   ! currently allocated in the data modules.
   !
   USE io_module,                ONLY : ionode 
   !
   USE kpoints_module,           ONLY : kpoints_memusage, kpoints_alloc 
   USE kpoints_module,           ONLY : rgrid_memusage,   rgrid_alloc
   USE kpoints_module,           ONLY : bshells_memusage, bshells_alloc
   USE ions_module,              ONLY : ions_memusage, ions_alloc => alloc    
   USE symmetry_module,          ONLY : symmetry_memusage, symm_alloc => alloc
   USE windows_module,           ONLY : windows_memusage, windows_alloc => alloc 
   USE subspace_module,          ONLY : subspace_memusage, subspace_alloc => alloc 
   USE overlap_module,           ONLY : overlap_memusage, overlap_alloc => alloc 
   USE ggrids_module,            ONLY : ggrids_memusage, ggrids_alloc => alloc 
   USE wfc_data_module,          ONLY : wfc_data_memusage, wfc_data_alloc => alloc 
   USE localization_module,      ONLY : localization_memusage, loc_alloc => alloc 
   USE trial_center_data_module, ONLY : trial_center_data_memusage, trial_alloc => alloc 
   USE struct_fact_data_module,  ONLY : struct_fact_data_memusage, strf_alloc => alloc 
   USE us_module,                ONLY : us_memusage
   USE uspp,                     ONLY : uspp_memusage
   USE hamiltonian_module,       ONLY : hamiltonian_memusage, ham_alloc => alloc
   USE correlation_module,       ONLY : correlation_memusage, corr_alloc => alloc
   USE workspace_wan_module,     ONLY : workspace_wan_memusage, workwan_alloc => alloc
   !
   IMPLICIT NONE
   !
   INTEGER, INTENT(IN) :: iunit
   !
#ifdef __HAVE_MALLINFO
   INTEGER  :: tmem
#endif
 
   IF ( ionode ) THEN
      ! 
      WRITE( iunit, "( ' <MEMORY_USAGE>' )" ) 
      !
      IF ( kpoints_alloc )  WRITE(iunit, 100) "kpoints",      kpoints_memusage()
      IF ( rgrid_alloc )    WRITE(iunit, 100) "rgrid",        rgrid_memusage()
      IF ( bshells_alloc )  WRITE(iunit, 100) "bshells",      bshells_memusage()
      IF ( ions_alloc )     WRITE(iunit, 100) "ions",         ions_memusage()
      IF ( symm_alloc )     WRITE(iunit, 100) "symmetry",     symmetry_memusage()
      IF ( windows_alloc )  WRITE(iunit, 100) "windows",      windows_memusage()
      IF ( subspace_alloc ) WRITE(iunit, 100) "subspace",     subspace_memusage()
      IF ( overlap_alloc )  WRITE(iunit, 100) "overlap",      overlap_memusage()
      IF ( ggrids_alloc )   WRITE(iunit, 100) "ggrids",       ggrids_memusage()
      IF ( wfc_data_alloc ) WRITE(iunit, 100) "wfc_data",     wfc_data_memusage()
      IF ( loc_alloc )      WRITE(iunit, 100) "localization", localization_memusage()
      IF ( trial_alloc )    WRITE(iunit, 100) "trial_center", trial_center_data_memusage()
      IF ( ham_alloc )      WRITE(iunit, 100) "hamiltonian",  hamiltonian_memusage()
      IF ( corr_alloc )     WRITE(iunit, 100) "correlation",  correlation_memusage()
      IF ( strf_alloc )     WRITE(iunit, 100) "struct_fact",  struct_fact_data_memusage()
      IF ( workwan_alloc )  WRITE(iunit, 100) "workspace_wan",workspace_wan_memusage()
                            WRITE(iunit, 100) "us",           us_memusage()
                            WRITE(iunit, 100) "uspp",         uspp_memusage()

#ifdef __HAVE_MALLINFO
      CALL memstat( tmem )
      WRITE( iunit, 100 ) "Total Memory",  REAL( tmem )/ 1000.0_dbl
#endif

      WRITE( iunit, "( ' </MEMORY_USAGE>',/ )" ) 
      !
   ENDIF

100 FORMAT ( 4x, a20':', f15.3 ' MB')

END SUBROUTINE memusage

