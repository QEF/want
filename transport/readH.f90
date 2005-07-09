!
!      Copyright (C) 2004 WanT Group
!      Copyright (C) 1999 Marco Buongiorno Nardelli
!
!      This file is distributed under the terms of the
!      GNU General Public License. See the file `License'
!      in the root directory of the present distribution,
!      or http://www.gnu.org/copyleft/gpl.txt .
!
!***************************************************************************
   SUBROUTINE readH( nmaxa, nmaxb, nmaxc, loverlap, calculation_type, &
                     h00_a, h01_a, s00_a, s01_a, h00_b, h01_b, s00_b,  &
                     s01_b, h00_c, s00_c, hci_ac, sci_ac, hci_cb, sci_cb)
   !***************************************************************************
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
   !     HCI_AC, HCI_CB  = coupling matrix between leads and conductor (from supercell calculation)
   !
   !     S00_A, S00_B, S00_C  = on site overlap matrices
   !     S01_A, S01_B         = hopping overlap matrices
   !     SCI_AC, SCI_CB       = coupling overlap matrices
   !
   !...  Overlap
   !     if  loverlap=.FALSE. (default) the basis set is orthonormal =>
   !         S00_A, S00_B, S00_C = Identity
   !         S01_A, S01_B, SCI_AC, SCI_CB = Zero
   !     if  loverlap=.TRUE. => external reading
   !
   !...  Units
   !     energies are supposed to be in eV
   !
   USE KINDS
   USE constants, ONLY : CZERO, CONE
   IMPLICIT NONE

   ! 
   ! input variables
   !
   INTEGER,      INTENT(in)  :: nmaxa, nmaxb, nmaxc
   LOGICAL,      INTENT(in)  :: loverlap
   CHARACTER(*), INTENT(in)  :: calculation_type
   !
   COMPLEX(dbl), INTENT(out) :: h00_a(nmaxa,nmaxa)
   COMPLEX(dbl), INTENT(out) :: h01_a(nmaxa,nmaxa)
   COMPLEX(dbl), INTENT(out) :: h00_b(nmaxb,nmaxb)
   COMPLEX(dbl), INTENT(out) :: h01_b(nmaxb,nmaxb)
   COMPLEX(dbl), INTENT(out) :: h00_c(nmaxc,nmaxc)
   COMPLEX(dbl), INTENT(out) :: hci_ac(nmaxa,nmaxc)
   COMPLEX(dbl), INTENT(out) :: hci_cb(nmaxc,nmaxb)
   ! 
   COMPLEX(dbl), INTENT(out) :: s00_a(nmaxa,nmaxa)
   COMPLEX(dbl), INTENT(out) :: s01_a(nmaxa,nmaxa)
   COMPLEX(dbl), INTENT(out) :: s00_b(nmaxb,nmaxb)
   COMPLEX(dbl), INTENT(out) :: s01_b(nmaxb,nmaxb)
   COMPLEX(dbl), INTENT(out) :: s00_c(nmaxc,nmaxc)
   COMPLEX(dbl), INTENT(out) :: sci_ac(nmaxa,nmaxc)
   COMPLEX(dbl), INTENT(out) :: sci_cb(nmaxc,nmaxb)

   !
   ! local variables
   !
   INTEGER :: i, j

   !
   ! end of declarations
   !

!
!----------------------------------------
! main Body
!----------------------------------------
!

!
! set defaults for overlaps
!
      s00_a(:,:) = CZERO
      s01_a(:,:) = CZERO
      s00_b(:,:) = CZERO
      s01_b(:,:) = CZERO
      s00_c(:,:) = CZERO
      sci_ac(:,:) = CZERO
      sci_cb(:,:) = CZERO

      DO i = 1, nmaxa
          s00_a(i,i) = CONE
      ENDDO
      DO i = 1, nmaxb
          s00_b(i,i) = CONE
      ENDDO
      DO i = 1, nmaxc
          s00_c(i,i) = CONE
      ENDDO


!
! read basic quantities
!
      CALL read_matrix( nmaxc, nmaxc, h00_c, 'H00_C')
      CALL read_matrix( nmaxc, nmaxb, hci_cb,'HCI_CB')

      IF ( loverlap ) THEN
          CALL read_matrix( nmaxc, nmaxc, s00_c, 'S00_C')
          CALL read_matrix( nmaxc, nmaxb, sci_cb,'SCI_CB')
      ENDIF

!
! chose whether to do 'conductor' or 'bulk'
!
      IF ( TRIM(calculation_type) == "conductor"  ) THEN
          !
          ! read the missing data
          !
          CALL read_matrix( nmaxa, nmaxa, h00_a, 'H00_A')
          CALL read_matrix( nmaxa, nmaxa, h01_a, 'H01_A')
          CALL read_matrix( nmaxb, nmaxa, h00_b, 'H00_B')
          CALL read_matrix( nmaxb, nmaxa, h01_b, 'H01_B')
          CALL read_matrix( nmaxa, nmaxc, hci_ac,'HCI_AC')
          !
          IF ( loverlap ) THEN
              CALL read_matrix( nmaxa, nmaxa, s00_a, 'S00_A')
              CALL read_matrix( nmaxa, nmaxa, s01_a, 'S01_A')
              CALL read_matrix( nmaxb, nmaxa, s00_b, 'S00_B')
              CALL read_matrix( nmaxb, nmaxa, s01_b, 'S01_B')
              CALL read_matrix( nmaxa, nmaxc, sci_ac,'SCI_AC')
          ENDIF

      ELSEIF ( TRIM(calculation_type) == "bulk"  ) THEN
          !
          ! rearrange the data already read
          !
          h00_a(:,:) = h00_c(:,:)
          h00_b(:,:) = h00_c(:,:)
          h01_a(:,:) = hci_cb(:,:)
          h01_b(:,:) = hci_cb(:,:)
          hci_ac(:,:)= hci_cb(:,:)
          !
          IF ( loverlap ) THEN
              s00_a(:,:) = s00_c(:,:)
              s00_b(:,:) = s00_c(:,:)
              s01_a(:,:) = sci_cb(:,:)
              s01_b(:,:) = sci_cb(:,:)
              sci_ac(:,:)= sci_cb(:,:)
          ENDIF

      ELSE
          CALL errore('conductor','Invalid calculation_type = '// &
                      TRIM(calculation_type),5)
      ENDIF


END SUBROUTINE readH


