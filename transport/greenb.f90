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
      SUBROUTINE greenb( nmaxb, tot, tott, c00_b, c01_b, ene, gm1, g, igreen, invert )
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

      INTEGER :: nmaxb
      INTEGER :: ipiv(nmaxb)
      INTEGER :: i, j, k, l, m
      INTEGER :: igreen, invert, info

      COMPLEX(dbl) c00_b(nmaxb,nmaxb)
      COMPLEX(dbl) c01_b(nmaxb,nmaxb)

      COMPLEX(dbl) :: tot(nmaxb,nmaxb)
      COMPLEX(dbl) :: tott(nmaxb,nmaxb)
      COMPLEX(dbl) :: eh0(nmaxb,nmaxb)
      COMPLEX(dbl) :: s1(nmaxb,nmaxb)
      COMPLEX(dbl) :: s2(nmaxb,nmaxb)
      COMPLEX(dbl) :: g(nmaxb,nmaxb)
      COMPLEX(dbl) :: gm1(nmaxb,nmaxb)
      COMPLEX(dbl) :: ene, dos, alpha, beta

!...  Scalar for BLAS calls
      alpha = ( 1.d0, 0.d0 )
      beta = ( 0.d0, 0.d0 )

      IF ( igreen == 1 ) THEN 

!...  Construct the surface green's function g00 

         CALL setv0( nmaxb*nmaxb, s1 )
         CALL zgemm( 'N', 'N', nmaxb, nmaxb, nmaxb, alpha, c01_b, nmaxb, tot, nmaxb,     &
                     beta, s1, nmaxb )

         DO j = 1, nmaxb
            DO i = 1, nmaxb
               eh0(i,j) = -1.d0 * c00_b(i,j) - 1.d0 * s1(i,j)
            END DO
         END DO
       
         DO i = 1, nmaxb
            DO j = 1, nmaxb
               gm1(i,j) = eh0(i,j)
               g(i,j) = (0.d0,0.d0)
               IF ( i == j ) g(i,j) = ( 1.d0, 0.d0 )
            END DO
         END DO
      
         IF ( invert == 1 ) THEN
            CALL zgesv( nmaxb, nmaxb, eh0, nmaxb, ipiv, g, nmaxb, info )
            IF ( info /= 0 )  CALL errore(' Sgreenb ', ' zgesv (I) ', info )
         END IF

      END IF

      IF ( igreen == -1 ) THEN

!...  Construct the dual surface green's function gbar00 

         CALL setv0( nmaxb*nmaxb, s1 )
         CALL zgemm( 'C', 'N', nmaxb, nmaxb, nmaxb, alpha, c01_b, nmaxb, tott, nmaxb,    &
                     beta, s1, nmaxb )

         DO j = 1, nmaxb
            DO i = 1, nmaxb
               eh0(i,j) = -1.d0 * c00_b(i,j) -1.d0 * s1(i,j)
            END DO
         END DO
       
         DO i = 1, nmaxb
            DO j = 1, nmaxb
               gm1(i,j) = eh0(i,j)
               g(i,j) = ( 0.d0, 0.d0 )
               IF ( i == j ) g(i,j) = ( 1.d0, 0.d0 )
            END DO
         END DO

         IF ( invert == 1 ) THEN
            CALL zgesv( nmaxb, nmaxb, eh0, nmaxb, ipiv, g, nmaxb, info )
            IF ( info /= 0 )  CALL errore(' Sgreenb ', ' zgesv (II) ', info )
         END IF
      
      END IF

      IF ( igreen == 0 ) THEN

!    Construct the bulk green's function gnn or (if surface=.true.) the
!    sub-surface green's function

         CALL setv0( nmaxb*nmaxb, s1 )
         CALL zgemm( 'N', 'N', nmaxb, nmaxb, nmaxb, alpha, c01_b, nmaxb, tot, nmaxb,   &
                     beta, s1, nmaxb )
         CALL setv0( nmaxb*nmaxb, s2 )
         CALL zgemm( 'C', 'N', nmaxb, nmaxb, nmaxb, alpha, c01_b, nmaxb, tott, nmaxb,  &
                     beta, s2, nmaxb )

         DO j = 1, nmaxb
            DO i = 1, nmaxb
               eh0(i,j) = -1.d0 * c00_b(i,j) -1.0d0 * s1(i,j) -1.0d0 * s2(i,j)
            END DO
         END DO
       
         DO i = 1, nmaxb
            DO j = 1, nmaxb
               gm1(i,j) = eh0(i,j)
               g(i,j) = ( 0.d0, 0.d0 )
               IF( i == j ) g(i,j) = ( 1.d0, 0.d0 )
            END DO
         END DO

         IF ( invert == 1 ) THEN
            CALL zgesv( nmaxb, nmaxb, eh0, nmaxb, ipiv, g, nmaxb, info )
            IF ( info /= 0 )  CALL errore(' Sgreenb ', ' zgesv (III) ', info )
         END IF

      END IF

      RETURN
      END 
