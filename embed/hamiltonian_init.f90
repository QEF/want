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
   USE io_module,            ONLY : stdin, stdout, ionode, ionode_id
   USE log_module,           ONLY : log_push, log_pop
   USE timing_module,        ONLY : timing
   USE mp,                   ONLY : mp_bcast
   !
   USE E_control_module,     ONLY : idir => transport_dir, datafile_tot
   USE E_hamiltonian_module, ONLY : hamiltonian_allocate, ispin, &
                                    blc_T, blc_E, blc_B, blc_EB, blc_BE
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
       CALL iotk_scan_empty( stdin, "H_T", ATTR=blc_T%tag, IERR=ierr)
       IF (ierr/=0) CALL errore(subname, 'searching for tag H_T', ABS(ierr) )       
       !
       CALL iotk_scan_empty( stdin, "H_E", ATTR=blc_E%tag, IERR=ierr)
       IF (ierr/=0) CALL errore(subname, 'searching for tag H_E', ABS(ierr) )       
       !
       CALL iotk_scan_empty( stdin, "H_B", ATTR=blc_B%tag, IERR=ierr)
       IF (ierr/=0) CALL errore(subname, 'searching for tag H_B', ABS(ierr) )       
       !
       CALL iotk_scan_empty( stdin, "H_EB", ATTR=blc_EB%tag, IERR=ierr)
       IF (ierr/=0) CALL errore(subname, 'searching for tag H_EB', ABS(ierr) )       
       !
       CALL iotk_scan_empty( stdin, "H_BE", ATTR=blc_BE%tag, IERR=ierr)
       IF (ierr/=0) CALL errore(subname, 'searching for tag H_BE', ABS(ierr) )       
       !
       CALL iotk_scan_end( stdin, 'HAMILTONIAN_DATA', IERR=ierr )
       IF (ierr/=0) CALL errore(subname,'searching end for HAMILTONIAN_DATA',ABS(ierr))
       !
   ENDIF
   !
   CALL mp_bcast(  blc_T%tag,    ionode_id )
   CALL mp_bcast(  blc_E%tag,    ionode_id )
   CALL mp_bcast(  blc_B%tag,    ionode_id )
   CALL mp_bcast(  blc_EB%tag,   ionode_id )
   CALL mp_bcast(  blc_BE%tag,   ionode_id )

   !
   ! Read basic quantities from datafile
   !
   CALL read_matrix( datafile_tot, ispin, idir, blc_T )
   CALL read_matrix( datafile_tot, ispin, idir, blc_E )
   CALL read_matrix( datafile_tot, ispin, idir, blc_B )
   CALL read_matrix( datafile_tot, ispin, idir, blc_EB )
   CALL read_matrix( datafile_tot, ispin, idir, blc_BE )

   
   CALL timing( subname, OPR='STOP' )
   CALL log_pop( subname )
   !
   RETURN
   !
END SUBROUTINE hamiltonian_init

