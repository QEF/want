!
!      Copyright (C) 2004 Arrigo Calzolari, Marco Buongiorno Nardelli
!      Copyright (C) 1999 Marco Buongiorno Nardelli
!
!      This file is distributed under the terms of the
!      GNU General Public License. See the file `License'
!      in the root directory of the present distribution,
!      or http://www.gnu.org/copyleft/gpl.txt .
!
!----------------------------------------------------------------------
      SUBROUTINE Sgreena( nmaxa, tot, tott, c00_a, c01_a, ene, gm1, g, igreen, invert)
!----------------------------------------------------------------------

!...  Construct green's functions
!     
!     igreen = -1  right surface - left transfer
!     igreen =  1  left surface - right transfer
!     igreen =  0  bulk

!...  invert = 0 computes g^-1
!     invert = 1 computes g^-1 and g

      USE kinds

      IMPLICIT NONE

      INTEGER :: nmaxa
      INTEGER :: i, j, k, l, m
      INTEGER :: igreen, invert, info
      INTEGER :: ipiv(nmaxa)

      COMPLEX(dbl) :: c00_a(nmaxa,nmaxa)
      COMPLEX(dbl) :: c01_a(nmaxa,nmaxa)

      COMPLEX(dbl) :: tot(nmaxa,nmaxa)
      COMPLEX(dbl) :: tott(nmaxa,nmaxa)
      COMPLEX(dbl) :: eh0(nmaxa,nmaxa)
      COMPLEX(dbl) :: s1(nmaxa,nmaxa)
      COMPLEX(dbl) :: s2(nmaxa,nmaxa)
      COMPLEX(dbl) :: g(nmaxa,nmaxa)
      COMPLEX(dbl) :: gm1(nmaxa,nmaxa)
      COMPLEX(dbl) :: ene, dos, alpha, beta

!...  Scalar for BLAS calls
      alpha = ( 1.d0, 0.d0 )
      beta = ( 0.d0, 0.d0 )

      IF ( igreen == 1) THEN 

!...     Construct the surface green's function g00 

         CALL setv0( nmaxa*nmaxa, s1 )
         CALL zgemm( 'N', 'N', nmaxa, nmaxa, nmaxa, alpha, c01_a, nmaxa, tot, nmaxa, beta, s1, nmaxa )
         DO j = 1, nmaxa
            DO i = 1, nmaxa
               eh0(i,j) = -1.d0 * c00_a(i,j) - 1.d0 * s1(i,j)
            END DO
         END DO
       
         DO i = 1, nmaxa
            DO j = 1, nmaxa
               gm1(i,j) = eh0(i,j)
               g(i,j) = (0.d0,0.d0)
               IF ( i == j ) g(i,j) = ( 1.d0, 0.d0 )
            END DO
         END DO
      
         IF ( invert == 1 ) THEN
            CALL zgesv( nmaxa, nmaxa, eh0, nmaxa, ipiv, g, nmaxa, info )
            IF ( info /= 0 )  CALL errore(' Sgreena ', ' zgesv (I) ', info )
         END IF

      END IF

      IF ( igreen == -1 ) THEN

!...  Construct the dual surface green's function gbar00 

         CALL setv0( nmaxa*nmaxa, s1 )
         CALL zgemm( 'C', 'N', nmaxa, nmaxa, nmaxa, alpha, c01_a, nmaxa, tott, nmaxa,  beta, s1, nmaxa )

         DO j = 1, nmaxa
            DO i = 1, nmaxa
               eh0(i,j) = -1.d0 * c00_a(i,j) - 1.d0 * s1(i,j)
            END DO
         END DO
       
         DO i = 1, nmaxa
            DO j = 1, nmaxa
               gm1(i,j) = eh0(i,j)
               g(i,j) = ( 0.d0, 0.d0 )
               IF ( i == j ) g(i,j) = ( 1.d0, 0.d0 )
            END DO
         END DO

         IF ( invert == 1 ) THEN
            CALL zgesv( nmaxa, nmaxa, eh0, nmaxa, ipiv, g, nmaxa, info )
            IF ( info /= 0 )  CALL errore(' Sgreena ', ' zgesv (II) ', info )
         END IF
      
      END IF

      IF ( igreen == 0 ) THEN

!...  Construct the bulk green's function gnn or (if surface=.true.) the
!     sub-surface green's function

         CALL setv0( nmaxa*nmaxa, s1 )
         CALL zgemm( 'N', 'N', nmaxa, nmaxa, nmaxa, alpha, c01_a, nmaxa, tot, nmaxa, beta, s1, nmaxa )
         CALL setv0( nmaxa*nmaxa, s2 )
         CALL zgemm( 'C', 'N', nmaxa, nmaxa, nmaxa, alpha, c01_a, nmaxa, tott, nmaxa, beta, s2, nmaxa )

         DO j = 1, nmaxa
            DO i = 1, nmaxa
               eh0(i,j) = -1.d0 * c00_a(i,j) -1.0d0 * s1(i,j) -1.0d0 * s2(i,j)
            END DO
         END DO
       
         DO i = 1, nmaxa
            DO j = 1, nmaxa
               gm1(i,j) = eh0(i,j)
               g(i,j) = ( 0.d0, 0.d0 )
               IF ( i == j ) g(i,j) = ( 1.d0, 0.d0 )
            END DO
         END DO

         IF ( invert == 1 ) THEN
            CALL zgesv( nmaxa, nmaxa, eh0, nmaxa, ipiv, g, nmaxa, info )
            IF ( info /= 0 )  CALL errore(' Sgreena ', ' zgesv (III) ', info )
         END IF

      END IF

      RETURN
      END 
