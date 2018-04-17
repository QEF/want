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
   ! This module contains the routine CLEANUP that
   ! that deallocates all the data stored in modules
   ! in the WanT-transport code. 
   ! If data is not allocated the routine goes through 
   ! and nothing happens.
   !
   USE timing_module,        ONLY : timing_deallocate, timing_alloc => alloc 
   USE T_workspace_module,   ONLY : workspace_deallocate, work_alloc => alloc
   USE T_egrid_module,       ONLY : egrid_deallocate, egrid_alloc => alloc
   USE T_hamiltonian_module, ONLY : hamiltonian_deallocate, ham_alloc => alloc
   USE T_correlation_module, ONLY : correlation_deallocate, corr_alloc => alloc
   USE T_kpoints_module,     ONLY : kpoints_deallocate, kpoints_alloc => alloc
   IMPLICIT NONE
      
      IF ( work_alloc )     CALL workspace_deallocate()
      IF ( egrid_alloc )    CALL egrid_deallocate()
      IF ( timing_alloc )   CALL timing_deallocate()
      IF ( ham_alloc )      CALL hamiltonian_deallocate()
      IF ( corr_alloc )     CALL correlation_deallocate()
      IF ( kpoints_alloc )  CALL kpoints_deallocate()

END SUBROUTINE cleanup


