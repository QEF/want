!
!      Copyright (C) 2009 WanT Group
!
!      This file is distributed under the terms of the
!      GNU General Public License. See the file `License'
!      in the root directory of the present distribution,
!      or http://www.gnu.org/copyleft/gpl.txt .
!
!*******************************************************************
   SUBROUTINE hamiltonian_init( )
   !*******************************************************************
   !
   !     Read hamiltonian data from file
   !     Units: energies are supposed to be in eV
   !
   USE kinds
   USE io_module,            ONLY : stdin, ionode, ionode_id
   USE log_module,           ONLY : log_push, log_pop
   USE timing_module,        ONLY : timing
   USE mp,                   ONLY : mp_bcast
   !
   USE E_control_module,     ONLY : idir => transport_dir, datafile_C
   USE E_hamiltonian_module, ONLY : hamiltonian_allocate, ispin, blc_C, blc_emb
   !
   USE T_operator_blc_module
   USE iotk_module
   !
   IMPLICIT NONE

   !
   ! local variables
   !
   CHARACTER(16) :: subname="hamiltonian_init"
   INTEGER       :: ierr

   !
   ! end of declarations
   !

!
!----------------------------------------
! main Body
!----------------------------------------
!
   CALL timing( subname, OPR='start')
   CALL log_push( subname )

   !
   ! allocations
   !
   CALL hamiltonian_allocate()


   !
   ! Read the HAMILTONIAN_DATA card from input file
   !
   IF ( ionode ) THEN
       !
       CALL iotk_scan_begin( stdin, 'HAMILTONIAN_DATA', IERR=ierr )
       IF (ierr/=0) CALL errore(subname,'searching HAMILTONIAN_DATA',ABS(ierr))
       !
       ! these data must always be present
       !
       CALL iotk_scan_empty( stdin, "H_C", ATTR=blc_C%tag, IERR=ierr)
       IF (ierr/=0) CALL errore(subname, 'searching for tag H_C', ABS(ierr) )       
       !
       CALL iotk_scan_empty( stdin, "H_EMB", ATTR=blc_EMB%tag, IERR=ierr)
       IF (ierr/=0) CALL errore(subname, 'searching for tag H_EMB', ABS(ierr) )       
       !
       CALL iotk_scan_end( stdin, 'HAMILTONIAN_DATA', IERR=ierr )
       IF (ierr/=0) CALL errore(subname,'searching end for HAMILTONIAN_DATA',ABS(ierr))
       !
   ENDIF
   !
   CALL mp_bcast(  blc_C%tag,      ionode_id )
   CALL mp_bcast(  blc_EMB%tag,    ionode_id )

   !
   ! Read basic quantities from datafile
   !
   CALL read_matrix( datafile_C, ispin, idir, blc_C )
   CALL read_matrix( datafile_C, ispin, idir, blc_emb )

   
   CALL timing( subname, OPR='STOP' )
   CALL log_pop( subname )
   !
   RETURN
   !
END SUBROUTINE hamiltonian_init

