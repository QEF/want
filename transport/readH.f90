!
!      Copyright (C) 2004 Arrigo Calzolari, Marco Buongiorno Nardelli
!      Copyright (C) 1999 Marco Buongiorno Nardelli
!
!      This file is distributed under the terms of the
!      GNU General Public License. See the file `License'
!      in the root directory of the present distribution,
!      or http://www.gnu.org/copyleft/gpl.txt .
!
!---------------------------------------------------------------------
      SUBROUTINE readH( nmaxa, nmaxb, nmaxc, h00_a, h01_a, s00_a, s01_a,  &
                         h00_b, h01_b, s00_b, s01_b, h00_c, s00_c, hci_ac, &
                         sci_ac, hci_cb, sci_cb , l_overlap )
!---------------------------------------------------------------------
 
!...  Energy conversion factor
!     efac=1.0d0       ! energy in eV (default)
!     efac=13.6058     ! energy in Ry
!     efac=27.2116     ! energy in Ry

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
!     if  l_overlap=false (default) the basis set is orthonormal =>
!         S00_A, S00_B, S00_C = Identity
!         S01_A, S01_B, SCI_AC, SCI_CB = Zero
!     if  l_overlap=true => external reading
!
!=-------------------------------------------------------------------------------------------
 
      USE KINDS

      IMPLICIT NONE

      INTEGER :: i, j, k, l, m, n
      INTEGER :: nmaxa, nmaxb, nmaxc
      INTEGER :: nwa, nwb, nwc

      REAL(dbl) :: efac
      REAL(dbl) :: h00_a(nmaxa,nmaxa)
      REAL(dbl) :: h01_a(nmaxa,nmaxa)
      REAL(dbl) :: h00_b(nmaxb,nmaxb)
      REAL(dbl) :: h01_b(nmaxb,nmaxb)
      REAL(dbl) :: h00_c(nmaxc,nmaxc)
      REAL(dbl) :: hci_ac(nmaxa,nmaxc)
      REAL(dbl) :: hci_cb(nmaxc,nmaxb)
      
      REAL(dbl) :: s00_a(nmaxa,nmaxa)
      REAL(dbl) :: s01_a(nmaxa,nmaxa)
      REAL(dbl) :: s00_b(nmaxb,nmaxb)
      REAL(dbl) :: s01_b(nmaxb,nmaxb)
      REAL(dbl) :: s00_c(nmaxc,nmaxc)
      REAL(dbl) :: sci_ac(nmaxa,nmaxc)
      REAL(dbl) :: sci_cb(nmaxc,nmaxb)

      LOGICAL :: l_overlap

      efac=1.0d0

!...  Read from external file

      PRINT*, ' ....reading from external file....'

!=--------------------------------------------------------------

      OPEN( 30, FILE="H00_A", STATUS='OLD' )

      REWIND 30
      READ ( 30, * ) nwa
      IF ( nwa /= nmaxa ) CALL errore(' SreadH ', ' wrong dimension in reading H00_A ', nwa )

      DO i = 1, nwa
         READ ( 30, * ) 
         DO j = 1, nwa
            READ ( 30, * ) h00_a(i,j)
 !          h00_a(i,j) = efac * h00(i,j)
         END DO
      END DO

      CLOSE(30)

!=--------------------------------------------------------------
          
      OPEN( 31, FILE="H01_A", STATUS='OLD' )

      REWIND 31
      READ ( 31, * ) nwa
      IF ( nwa /= nmaxa ) CALL errore(' SreadH ', ' wrong dimension in reading H01_A ', nwa )

      DO i = 1, nwa
         READ ( 31, * ) 
         DO j = 1, nwa
            READ ( 31, * ) h01_a(i,j)
!           h01_a(i,j) = efac * h01_a(i,j)
         END DO
      END DO

      CLOSE(31)

!=--------------------------------------------------------------

      OPEN( 40, FILE="H00_B", STATUS='OLD' )

      REWIND 40
      READ ( 40, * ) nwb
      IF ( nwb /= nmaxb ) CALL errore(' SreadH ', ' wrong dimension in reading H00_B ', nwb )

      DO i = 1, nwb
         READ ( 40, * ) 
         DO j = 1, nwb
            READ ( 40, * ) h00_b(i,j)
!           h00_b(i,j) = efac * h00_b(i,j)
         END DO
      END DO

      CLOSE(40)

!=--------------------------------------------------------------
          
      OPEN( 41, FILE="H01_B", STATUS='OLD' )

      REWIND 41
      READ( 41, * ) nwb
      IF ( nwb /= nmaxb ) CALL errore(' SreadH ', ' wrong dimension in reading H01_B ', nwb )

      DO i = 1, nwb
         READ ( 41, * ) 
         DO j = 1, nwb
            READ ( 41, * ) h01_b(i,j)
!           h01_b(i,j) = efac * h01_b(i,j)
         END DO
      END DO

      CLOSE(41)

!=--------------------------------------------------------------

      OPEN( 51, FILE="H00_C", STATUS='OLD' )

      REWIND 51
      READ ( 51, * ) nwc
      IF ( nwc /= nmaxc ) CALL errore(' SreadH ', ' wrong dimension in reading H00_C ', nwc )

      DO i = 1, nwc
         READ ( 51, * ) 
         DO j = 1, nwc
            READ( 51, * ) h00_c(i,j)
!           h00_c(i,j) = efac * h00_c(i,j)
         END DO
      END DO

      CLOSE(51)

!=--------------------------------------------------------------

      OPEN( 61, FILE="HCI_AC", STATUS='OLD' )

      REWIND 61
      READ ( 61, * ) nwa, nwc
      IF ( nwa /= nmaxa .OR. nwc /= nmaxc ) CALL errore(' SreadH ', ' wrong dimension in reading HCI_AC ', nwc )

      DO i = 1, nwa
         READ ( 61, * ) 
         DO j = 1, nwc
            READ ( 61, * ) hci_ac(i,j)
!           hci_ac(i,j) = efac * hci_ac(i,j)
         END DO
      END DO

      CLOSE(61)

!=--------------------------------------------------------------

      OPEN( 71, FILE="HCI_CB", STATUS='OLD' )

      REWIND 71
      READ ( 71, * ) nwc, nwb
      IF ( nwc /= nmaxc .OR. nwb /= nmaxb ) CALL errore(' SreadH ', ' wrong dimension in reading HCI_CB ', nwb )

      DO j = 1, nwc
         READ ( 71, * ) 
         DO i = 1, nwb
            READ ( 71, * ) hci_cb(i,j)
!           hci_cb(i,j) = efac * hci_cb(i,j)
         END DO
      END DO

      CLOSE(71)

!...  Check overlap

      IF ( l_overlap ) THEN


        OPEN( 32, FILE="S00_A", STATUS='OLD' )
  
        REWIND 32
        READ ( 32, * ) nwa
        IF ( nwa /= nmaxa ) CALL errore(' SreadH ', ' wrong dimension in reading S00_A ', nwa )

        DO i = 1, nwa
           READ ( 32, * ) 
           DO j = 1, nwa
              READ ( 32, *) s00_a(i,j)
           END DO
        END DO

        CLOSE(32)

!=--------------------------------------------------------------

        OPEN( 33, FILE="S01_A", STATUS='OLD' )

        REWIND 33
        READ ( 33, * ) nwa
        IF ( nwa /= nmaxa ) CALL errore(' SreadH ', ' wrong dimension in reading S01_A ', nwa )

        DO i = 1, nwa
           READ ( 33, * ) 
           DO j = 1, nwa
            READ(33,*) s01_a(i,j)
           END DO
        END DO

        CLOSE(33)

!=--------------------------------------------------------------

        OPEN( 42, FILE="S00_B", STATUS='OLD' )

        REWIND 42
        READ ( 42, * ) nwb
        IF ( nwb /= nmaxb ) CALL errore(' SreadH ', ' wrong dimension in reading S00_B ', nwb )

        DO i = 1, nwb
           READ ( 42, * ) 
           DO j = 1, nwb
              READ( 42, * ) s00_B(i,j)
           END DO
        END DO

        CLOSE(42)

!=--------------------------------------------------------------

        OPEN( 43, FILE="S01_B", STATUS='OLD' )

        REWIND 43
        READ(43,*) nwb
        IF( nwb /= nmaxb ) CALL errore(' SreadH ', ' wrong dimension in reading S01_B ', nwb )

        DO i = 1, nwb
           READ( 43, * ) 
           DO j = 1, nwb
              READ( 43, * ) s01_b(i,j)
           END DO
        END DO

        CLOSE(33)

!=--------------------------------------------------------------

        OPEN( 52, FILE="S00_C", STATUS='OLD' ) 

        REWIND 52
        READ ( 52, * ) nwc
        IF ( nwc /= nmaxc ) CALL errore(' SreadH ', ' wrong dimension in reading S00_C ', nwc )

        DO i = 1, nwc
         READ ( 52, * ) 
           DO j = 1, nwc
              READ ( 52, * ) s00_c(i,j)
           END DO
        END DO

        CLOSE(52)

!=--------------------------------------------------------------

        OPEN( 62, FILE="SCI_AC", STATUS='OLD' )

        REWIND 62
        READ ( 62, * ) nwa, nwc
        IF ( nwa /= nmaxa .OR. nwc /= nmaxc ) CALL errore(' SreadH ', ' wrong dimension in reading SCI_AC ', nwa )

        DO i =1, nwa
           READ ( 62, * ) 
           DO j = 1, nwc
              READ ( 62, * ) sci_ac(i,j)
           END DO
        END DO

        CLOSE(62)

!=--------------------------------------------------------------

        OPEN( 72, FILE="SCI_CB", STATUS='OLD' )

        REWIND 72
        READ(72,*) nwc, nwb
        IF( nwc /= nmaxc .OR. nwb /= nmaxb ) CALL errore(' SreadH ', ' wrong dimension in reading SCI_CB ', nwc )

        DO i = 1, nwc
           READ ( 72, * ) 
           DO j = 1, nwb
              READ ( 72, * ) sci_cb(i,j)
           END DO
        END DO

        CLOSE(72)

      ELSE
      
        s00_a(:,:) = 0.0d0
        s01_a(:,:) = 0.0d0
        s00_b(:,:) = 0.0d0
        s01_b(:,:) = 0.0d0
        s00_c(:,:) = 0.0d0
        sci_ac(:,:) = 0.0d0
        sci_ac(:,:) = 0.0d0

        DO i = 1, nmaxa
           s00_a(i,i) = 1.d0
        END DO
        DO i = 1, nmaxb
           s00_b(i,i) = 1.d0
        END DO
        DO i = 1, nmaxb
           s00_c(i,i) = 1.d0
        END DO


      END IF

      RETURN
      END
