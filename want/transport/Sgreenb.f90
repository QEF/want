!----------------------------------------------------------------------
      SUBROUTINE Sgreenb( nmaxb, tot, tott, c00_b, c01_b, ene, gm1, g, igreen, invert )
!----------------------------------------------------------------------

!...  Construct green's functions
!     
!     igreen = -1  right surface - left transfer
!     igreen =  1  left surface - right transfer
!     igreen =  0  bulk

!...  invert = 0 computes g^-1
!     invert = 1 computes g^-1 and g

      IMPLICIT NONE

      INTEGER :: nmaxb
      INTEGER :: ipiv(nmaxb)
      INTEGER :: i, j, k, l, m
      INTEGER :: igreen, invert, info

      COMPLEX*16 c00_b(nmaxb,nmaxb), c01_b(nmaxb,nmaxb)

      COMPLEX*16 :: tot(nmaxb,nmaxb)
      COMPLEX*16 :: tott(nmaxb,nmaxb)
      COMPLEX*16 :: eh0(nmaxb,nmaxb)
      COMPLEX*16 :: s1(nmaxb,nmaxb)
      COMPLEX*16 :: s2(nmaxb,nmaxb)
      COMPLEX*16 :: g(nmaxb,nmaxb)
      COMPLEX*16 :: gm1(nmaxb,nmaxb)
      COMPLEX*16 :: ene, dos, alpha, beta

!...  Scalar for BLAS calls
      alpha = ( 1.d0, 0.d0 )
      beta = ( 0.d0, 0.d0 )

      IF (igreen.eq.1) THEN 

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
            IF ( info /= 0 ) THEN
              PRINT*,'error in ZGESV - INFO = ', info
              STOP
            END IF
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
            IF ( info /= 0 ) THEN
               PRINT*,'error in ZGESV - INFO = ', info
            STOP
            END IF
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
            IF ( info /= 0 ) THEN
              PRINT*,'error in ZGESV - INFO = ', info
              STOP
            END IF
         END IF

      END IF

      RETURN
      END 
