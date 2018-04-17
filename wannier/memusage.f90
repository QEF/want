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
   USE kinds,                    ONLY : dbl
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
   USE workspace_dis_module,     ONLY : workspace_dis_memusage, workdis_alloc => alloc
   !
   IMPLICIT NONE
   !
   INTEGER, INTENT(IN) :: iunit
   !
   REAL(dbl) :: memsum, mtmp
   !
#ifdef HAVE_MALLINFO
   INTEGER  :: tmem
#endif

100 FORMAT ( 4x, a20,':', f15.3, ' MB')

   memsum = 0.0_dbl
   !
   IF ( ionode ) THEN
      ! 
      WRITE( iunit, "( ' <MEMORY_USAGE>' )" ) 
      !
      IF ( kpoints_alloc )  THEN
          mtmp    =  kpoints_memusage()   
          memsum  =  memsum + mtmp
          WRITE(iunit, 100) "kpoints", mtmp 
      ENDIF
      IF ( rgrid_alloc ) THEN
          mtmp    =  rgrid_memusage()
          memsum  =  memsum + mtmp
          WRITE(iunit, 100) "rgrid", mtmp
      ENDIF
      IF ( bshells_alloc ) THEN
          mtmp    =  bshells_memusage()
          memsum  =  memsum + mtmp
          WRITE(iunit, 100) "bshells", mtmp
      ENDIF
      IF ( ions_alloc ) THEN
          mtmp    =  ions_memusage()
          memsum  =  memsum + mtmp
          WRITE(iunit, 100) "ions", mtmp
      ENDIF
      IF ( symm_alloc ) THEN
          mtmp    =  symmetry_memusage()
          memsum  =  memsum + mtmp
          WRITE(iunit, 100) "symmetry", mtmp
      ENDIF
      IF ( windows_alloc ) THEN
          mtmp    =  windows_memusage()
          memsum  =  memsum + mtmp
          WRITE(iunit, 100) "windows", mtmp
      ENDIF
      IF ( subspace_alloc ) THEN
          mtmp    =  subspace_memusage()
          memsum  =  memsum + mtmp
          WRITE(iunit, 100) "subspace", mtmp
      ENDIF
      IF ( overlap_alloc ) THEN
          mtmp    =  overlap_memusage()
          memsum  =  memsum + mtmp
          WRITE(iunit, 100) "overlap", mtmp
      ENDIF
      IF ( ggrids_alloc ) THEN
          mtmp    =  ggrids_memusage()
          memsum  =  memsum + mtmp
          WRITE(iunit, 100) "ggrids", mtmp
      ENDIF
      IF ( wfc_data_alloc ) THEN
          mtmp    =  wfc_data_memusage()
          memsum  =  memsum + mtmp
          WRITE(iunit, 100) "wfc_data", mtmp
      ENDIF
      IF ( loc_alloc ) THEN
          mtmp    =  localization_memusage()
          memsum  =  memsum + mtmp
          WRITE(iunit, 100) "localization", mtmp
      ENDIF
      IF ( trial_alloc ) THEN
          mtmp    =  trial_center_data_memusage()
          memsum  =  memsum + mtmp
          WRITE(iunit, 100) "trial_center", mtmp
      ENDIF
      IF ( ham_alloc ) THEN
          mtmp    =  hamiltonian_memusage()
          memsum  =  memsum + mtmp
          WRITE(iunit, 100) "hamiltonian", mtmp
      ENDIF
      IF ( corr_alloc ) THEN
          mtmp    =  correlation_memusage()
          memsum  =  memsum + mtmp
          WRITE(iunit, 100) "correlation", mtmp
      ENDIF
      IF ( strf_alloc ) THEN
          mtmp    =  struct_fact_data_memusage()
          memsum  =  memsum + mtmp
          WRITE(iunit, 100) "struct_fact", mtmp
      ENDIF
      IF ( workwan_alloc ) THEN
          mtmp    =  workspace_wan_memusage()
          memsum  =  memsum + mtmp
          WRITE(iunit, 100) "workspace_wan", mtmp
      ENDIF
      IF ( workdis_alloc ) THEN
          mtmp    =  workspace_dis_memusage()
          memsum  =  memsum + mtmp
          WRITE(iunit, 100) "workspace_dis", mtmp
      ENDIF
          !
          mtmp    =  us_memusage()
          memsum  =  memsum + mtmp
          WRITE(iunit, 100) "us", mtmp
          !
          mtmp    =  uspp_memusage()
          memsum  =  memsum + mtmp
          WRITE(iunit, 100) "uspp", mtmp
      !
      !
      WRITE( iunit, "()")
      WRITE( iunit, 100 ) "Total alloc. Memory", memsum

#ifdef HAVE_MALLINFO
      CALL memstat( tmem )
      WRITE( iunit, 100 ) " Real alloc. Memory",  REAL( tmem )/ 1000.0_dbl
#endif

      WRITE( iunit, "( ' </MEMORY_USAGE>',/ )" ) 
      !
   ENDIF


END SUBROUTINE memusage

