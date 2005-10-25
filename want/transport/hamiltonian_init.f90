!
!      Copyright (C) 2005 WanT Group
!
!      This file is distributed under the terms of the
!      GNU General Public License. See the file `License'
!      in the root directory of the present distribution,
!      or http://www.gnu.org/copyleft/gpl.txt .
!
!*******************************************************************
   SUBROUTINE hamiltonian_init( use_overlap, calculation_type )
   !*******************************************************************
   !
   ! Initialize hamiltonian data:
   !
   !...  Matrix definition
   !
   !     Given a conductor (C) bonded to a right lead (A) and a left lead (B)
   !
   !       H01_A    H00_A   HCI_AC    H00_C     HCI_CB   H00_B   H01_B
   !       S01_A    S00_A   SCI_AC    S00_C     SCI_CB   S00_B   S01_B
   !   ...--------------------------------------------------------------...
   !         |                |                   |                | 
   !         |     LEAD A     |    CONDUCTOR C    |     LEAD B     |
   !         |                |                   |                | 
   !   ...--------------------------------------------------------------...
   !
   !     H00_A, H00_B    = on site hamiltonian of the leads (from bulk calculation)
   !     H01_A, H01_B    = hopping hamiltonian of the leads (from bulk calculation)
   !     H00_C           = on site hamiltonian of the conductor (from supercell calculation)
   !     HCI_AC, HCI_CB  = coupling matrix between leads and conductor 
   !                       (from supercell calculation)
   !
   !     S00_A, S00_B, S00_C  = on site overlap matrices
   !     S01_A, S01_B         = hopping overlap matrices
   !     SCI_AC, SCI_CB       = coupling overlap matrices
   !
   !...  Overlap
   !     if  use_overlap = .FALSE. (default) the basis set is orthonormal =>
   !         S00_A, S00_B, S00_C = Identity
   !         S01_A, S01_B, SCI_AC, SCI_CB = Zero
   !     if  use_overlap=  .TRUE. => external reading  
   !                (not implemented within the current interface)
   !
   !...  Units
   !     energies are supposed to be in eV
   !
   USE kinds
   USE constants,            ONLY : CZERO, CONE
   USE io_global_module,     ONLY : stdin
   USE T_hamiltonian_module, ONLY : hamiltonian_allocate,   &
                                    dimA, dimB, dimC,       &
                                    h00_a, h01_a, h00_b, h01_b, h00_c, &
                                    s00_a, s01_a, s00_b, s01_b, s00_c, &
                                    hci_ac, hci_cb, sci_ac, sci_cb
   USE iotk_module
   IMPLICIT NONE

   ! 
   ! input variables
   !
   LOGICAL,      INTENT(in)  :: use_overlap
   CHARACTER(*), INTENT(in)  :: calculation_type
   !
   ! local variables
   !
   CHARACTER(16) :: subname="hamiltonian_init"
   INTEGER       :: i, ierr

   !
   ! end of declarations
   !

!
!----------------------------------------
! main Body
!----------------------------------------
!

   !
   ! missing implementation
   !
   IF ( use_overlap ) CALL errore(subname,'overlaps not currently implemented',1)

   !
   ! allocate main quantities
   !
   CALL hamiltonian_allocate()

   !
   ! set defaults for overlaps
   ! NOTE: at the moment overlaps are not impemented 
   !
   s00_a(:,:)  = CZERO
   s01_a(:,:)  = CZERO
   s00_b(:,:)  = CZERO
   s01_b(:,:)  = CZERO
   s00_c(:,:)  = CZERO
   sci_ac(:,:) = CZERO
   sci_cb(:,:) = CZERO

   DO i = 1, dimA
       s00_a(i,i) = CONE
   ENDDO
   DO i = 1, dimB
       s00_b(i,i) = CONE
   ENDDO
   DO i = 1, dimC
       s00_c(i,i) = CONE
   ENDDO


   !
   ! open the IOTK tag
   !
   CALL iotk_scan_begin( stdin, 'HAMILTONIAN_DATA', IERR=ierr )
      IF (ierr/=0) CALL errore(subname,'searching HAMILTONIAN_DATA',ABS(ierr))


   !
   ! read basic quantities
   !
   CALL read_matrix( stdin, 'H00_C',  dimC, dimC, h00_c)
   CALL read_matrix( stdin, 'HCI_CB', dimC, dimB, hci_cb)

   IF ( use_overlap ) THEN
       CALL read_matrix( stdin, 'S00_C',  dimC, dimC, s00_c)
       CALL read_matrix( stdin, 'SCI_CB', dimC, dimB, sci_cb)
   ENDIF

   !
   ! chose whether to do 'conductor' or 'bulk'
   !
   SELECT CASE ( TRIM(calculation_type) )

   CASE ( "conductor" )
       !
       ! read the missing data
       !
       CALL read_matrix( stdin, 'HCI_AC', dimA, dimC, hci_ac)
       CALL read_matrix( stdin, 'H00_A',  dimA, dimA, h00_a)
       CALL read_matrix( stdin, 'H01_A',  dimA, dimA, h01_a)
       CALL read_matrix( stdin, 'H00_B',  dimB, dimB, h00_b)
       CALL read_matrix( stdin, 'H01_B',  dimB, dimB, h01_b)
       !
       IF ( use_overlap ) THEN
           CALL read_matrix( stdin, 'SCI_AC', dimA, dimC, sci_ac)
           CALL read_matrix( stdin, 'S00_A',  dimA, dimA, s00_a)
           CALL read_matrix( stdin, 'S01_A',  dimA, dimA, s01_a)
           CALL read_matrix( stdin, 'S00_B',  dimB, dimB, s00_b)
           CALL read_matrix( stdin, 'S01_B',  dimB, dimB, s01_b)
       ENDIF

   CASE ( "bulk" )
       !
       ! rearrange the data already read
       !
       h00_a(:,:) = h00_c(:,:)
       h00_b(:,:) = h00_c(:,:)
       h01_a(:,:) = hci_cb(:,:)
       h01_b(:,:) = hci_cb(:,:)
       hci_ac(:,:)= hci_cb(:,:)
       !
       IF ( use_overlap ) THEN
           s00_a(:,:) = s00_c(:,:)
           s00_b(:,:) = s00_c(:,:)
           s01_a(:,:) = sci_cb(:,:)
           s01_b(:,:) = sci_cb(:,:)
           sci_ac(:,:)= sci_cb(:,:)
       ENDIF

   CASE DEFAULT
       CALL errore(subname,'Invalid calculation_type = '// &
                   TRIM(calculation_type),5)
   END SELECT

   CALL iotk_scan_end( stdin, 'HAMILTONIAN_DATA', IERR=ierr )
      IF (ierr/=0) CALL errore(subname,'searching end for HAMILTONIAN_DATA',ABS(ierr))

END SUBROUTINE hamiltonian_init


