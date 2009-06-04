!
!      Copyright (C) 2005 WanT Group
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
   ! Initialize hamiltonian data:
   !
   !...  Matrix definition
   !
   !     Given a conductor (C) bonded to a right lead (A) and a left lead (B)
   !
   !       H01_L    H00_L   H_LC    H00_C     H_CR   H00_R   H01_R
   !       S01_L    S00_L   S_LC    S00_C     S_CR   S00_R   S01_R
   !   ...--------------------------------------------------------------...
   !         |                |                   |                | 
   !         |     LEAD L     |    CONDUCTOR C    |     LEAD R     |
   !         |                |                   |                | 
   !   ...--------------------------------------------------------------...
   !
   !     H00_L, H00_R    = on site hamiltonian of the leads (from bulk calculation)
   !     H01_L, H01_R    = hopping hamiltonian of the leads (from bulk calculation)
   !     H00_C           = on site hamiltonian of the conductor (from supercell calculation)
   !     H_LC, H_CR  = coupling matrix between leads and conductor 
   !                       (from supercell calculation)
   !
   !     S00_L, S00_R, S00_C  = on site overlap matrices
   !     S01_L, S01_R         = hopping overlap matrices
   !     S_LC, S_CR           = coupling overlap matrices
   !
   !...  Units
   !     energies are supposed to be in eV
   !
   USE kinds
   USE io_module,            ONLY : stdin, ionode, ionode_id
   USE log_module,           ONLY : log_push, log_pop
   USE timing_module,        ONLY : timing
   USE mp,                   ONLY : mp_bcast
   USE T_control_module,     ONLY : calculation_type, idir => transport_dir, &
                                    datafile_L, datafile_C, datafile_R
   USE T_hamiltonian_module, ONLY : hamiltonian_allocate, ispin,        &
                                    blc_00L, blc_01L, blc_00R, blc_01R, &
                                    blc_00C, blc_LC,  blc_CR
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
       CALL iotk_scan_empty( stdin, "H00_C", ATTR=blc_00C%tag, IERR=ierr)
       IF (ierr/=0) CALL errore(subname, 'searching for tag H00_C', ABS(ierr) )       
       !
       CALL iotk_scan_empty( stdin, "H_CR", ATTR=blc_CR%tag, IERR=ierr)
       IF (ierr/=0) CALL errore(subname, 'searching for tag H_CR', ABS(ierr) )       
       !
       ! read the remaing data if the case
       !
       IF ( TRIM(calculation_type) == "conductor" ) THEN
           !
           CALL iotk_scan_empty( stdin, "H_LC", ATTR=blc_LC%tag, IERR=ierr)
           IF (ierr/=0) CALL errore(subname, 'searching for tag H_LC', ABS(ierr) )       
           !
           CALL iotk_scan_empty( stdin, "H00_L", ATTR=blc_00L%tag, IERR=ierr)
           IF (ierr/=0) CALL errore(subname, 'searching for tag H00_L', ABS(ierr) )       
           CALL iotk_scan_empty( stdin, "H01_L", ATTR=blc_01L%tag, IERR=ierr)
           IF (ierr/=0) CALL errore(subname, 'searching for tag H01_L', ABS(ierr) )       
           !
           CALL iotk_scan_empty( stdin, "H00_R", ATTR=blc_00R%tag, IERR=ierr)
           IF (ierr/=0) CALL errore(subname, 'searching for tag H00_R', ABS(ierr) )       
           CALL iotk_scan_empty( stdin, "H01_R", ATTR=blc_01R%tag, IERR=ierr)
           IF (ierr/=0) CALL errore(subname, 'searching for tag H01_R', ABS(ierr) )       
           !
       ENDIF
       !
       CALL iotk_scan_end( stdin, 'HAMILTONIAN_DATA', IERR=ierr )
       IF (ierr/=0) CALL errore(subname,'searching end for HAMILTONIAN_DATA',ABS(ierr))
       !
   ENDIF
   !
   CALL mp_bcast(  blc_00C%tag,      ionode_id )
   CALL mp_bcast(  blc_CR%tag,       ionode_id )
   CALL mp_bcast(  blc_LC%tag,       ionode_id )
   CALL mp_bcast(  blc_00L%tag,      ionode_id )
   CALL mp_bcast(  blc_01L%tag,      ionode_id )
   CALL mp_bcast(  blc_00R%tag,      ionode_id )
   CALL mp_bcast(  blc_01R%tag,      ionode_id )



   !
   ! Read basic quantities from datafile
   !
   CALL read_matrix( datafile_C, ispin, idir, blc_00C )
   CALL read_matrix( datafile_C, ispin, idir, blc_CR )

   !
   ! chose whether to do 'conductor' or 'bulk'
   !
   SELECT CASE ( TRIM(calculation_type) )

   CASE ( "conductor" )
       !
       ! read the missing data
       !
       CALL read_matrix( datafile_C, ispin, idir, blc_LC )
       CALL read_matrix( datafile_L, ispin, idir, blc_00L )
       CALL read_matrix( datafile_L, ispin, idir, blc_01L )
       CALL read_matrix( datafile_R, ispin, idir, blc_00R )
       CALL read_matrix( datafile_R, ispin, idir, blc_01R )
       !
   CASE ( "bulk" )
       !
       ! rearrange the data already read
       !
       blc_00L = blc_00C
       blc_00R = blc_00C
       blc_01L = blc_CR
       blc_01R = blc_CR
       blc_LC  = blc_CR
       !
   CASE DEFAULT
       !
       CALL errore(subname,'Invalid calculation_type = '// TRIM(calculation_type),5)
       !
   END SELECT

   
   CALL timing( subname, OPR='STOP' )
   CALL log_pop( subname )
   !
   RETURN
   !
END SUBROUTINE hamiltonian_init

