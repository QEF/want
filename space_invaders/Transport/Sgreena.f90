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

      IMPLICIT NONE

      INTEGER :: nmaxa
      INTEGER ::i,j,k,l,m,igreen,invert,info
      INTEGER ::ipiv(nmaxa)

      COMPLEX*16 :: c00_a(nmaxa,nmaxa)
      COMPLEX*16 :: c01_a(nmaxa,nmaxa)

      COMPLEX*16 :: tot(nmaxa,nmaxa)
      COMPLEX*16 :: tott(nmaxa,nmaxa)
      COMPLEX*16 :: eh0(nmaxa,nmaxa)
      COMPLEX*16 :: s1(nmaxa,nmaxa)
      COMPLEX*16 :: s2(nmaxa,nmaxa)
      COMPLEX*16 :: g(nmaxa,nmaxa)
      COMPLEX*16 :: gm1(nmaxa,nmaxa)
      COMPLEX*16 :: ene, dos, alpha, beta

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
            IF ( info /= 0 ) THEN
              PRINT*,'error in ZGESV - INFO = ', info
              STOP
            END IF
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
            IF ( info /= 0 ) THEN
               PRINT*,'error in ZGESV - INFO = ', info
            STOP
            END IF
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
            IF( info /= 0 ) THEN
              PRINT*,'error in ZGESV - INFO = ', info
              STOP
            END IF
         END IF

      END IF

      RETURN
      END 
