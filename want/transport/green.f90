!=---------------------------------------------------------------------=
      SUBROUTINE green( nmax, tot, tott, h0, h1, ene, gm1, g, igreen, invert )
!=---------------------------------------------------------------------=

      IMPLICIT NONE

!...  Construct green's functions
!     
!     igreen = -1  left surface
!     igreen =  1  right surface
!     igreen =  0  bulk

!     invert = 0 computes g^-1
!     invert = 1 computes g^-1 and g

      INTEGER :: nmax
      INTEGER :: i, j, k, l, m
      INTEGER :: igreen, invert, info
      INTEGER :: ipiv(nmax)

      COMPLEX*16 :: alpha,beta

      REAL*8 :: h0(nmax,nmax) 
      REAL*8 :: h1(nmax,nmax)

      COMPLEX*16 :: tot(nmax,nmax), tott(nmax,nmax)
      COMPLEX*16 :: eh0(nmax,nmax), c1(nmax,nmax)
      COMPLEX*16 :: s1(nmax,nmax), s2(nmax,nmax)
      COMPLEX*16 :: g(nmax,nmax), gm1(nmax,nmax)
      COMPLEX*16 :: ene, dos

!...  Scalar for BLAS calls
      alpha = ( 1.d0, 0.d0 )
      beta = ( 0.d0, 0.d0 )

      DO i = 1, nmax
         DO j = 1, nmax
            c1(i,j) = ( 1.d0, 0.d0 ) * h1(i,j)
         END DO
      END DO

      IF ( igreen == 1 ) THEN 

!...  Construct the surface green's function g00 

         CALL setv0( nmax * nmax, s1 )
         CALL zgemm( 'N', 'N', nmax, nmax, nmax, alpha, c1, nmax, tot, nmax, beta, s1, nmax )

         DO j = 1, nmax
            DO i = 1, nmax
               eh0(i,j) = ( -1.d0, 0.d0 ) * h0(i,j) - 1.d0 * s1(i,j)
            END DO
         END DO
         DO i = 1, nmax
            eh0(i,i) = ene + eh0(i,i)
         END DO
       
         DO i = 1, nmax
            DO j = 1, nmax
               gm1(i,j) = eh0(i,j)
               g(i,j) = ( 0.d0, 0.d0 )
               IF ( i == j ) g(i,j) = ( 1.d0, 0.d0 )
            END DO
         END DO
      
         IF ( invert == 1 ) THEN
            CALL zgesv( nmax, nmax, eh0, nmax, ipiv, g, nmax, info )
            IF ( info /= 0 ) THEN
              PRINT*,'error in ZGESV - INFO = ', info
              STOP
            END IF
         END IF

      END IF

      IF ( igreen == -1 ) THEN

!...  Construct the dual surface green's function gbar00 

         CALL setv0( nmax * nmax, s1 )
         CALL zgemm( 'C', 'N', nmax, nmax, nmax, alpha, c1, nmax, tott, nmax, beta, s1, nmax )

         DO j = 1, nmax
            DO i = 1, nmax
               eh0(i,j) = ( -1.d0, 0.d0 ) * h0(i,j) - 1.d0 * s1(i,j)
            END DO
         END DO
         DO i = 1, nmax
            eh0(i,i) = ene + eh0(i,i)
         END DO
       
         DO i = 1, nmax
            DO j = 1, nmax
               gm1(i,j) = eh0(i,j)
               g(i,j) = (0.d0,0.d0)
               IF ( i == j ) g(i,j) = ( 1.d0, 0.d0 )
            END DO
         END DO

         IF ( invert == 1 ) THEN
            CALL zgesv( nmax, nmax, eh0, nmax, ipiv, g, nmax, info )
            IF ( info /=  0 ) THEN
               PRINT*,'error in ZGESV - INFO = ', info
            STOP
            END IF
         END IF
      
      END IF

      IF ( igreen == 0 ) THEN

!...  Construct the bulk green's function gnn or (if surface=.true.) the
!     sub-surface green's function

         CALL SETV0( nmax * nmax, s1 )
         CALL zgemm( 'N', 'N', nmax, nmax, nmax, alpha, c1, nmax, tot, nmax, beta, s1, nmax )
         CALL setv0( nmax * nmax, s2 )
         CALL zgemm( 'C', 'N', nmax, nmax, nmax, alpha, c1, nmax, tott, nmax, beta, s2, nmax )

         DO j = 1, nmax
            DO i = 1, nmax
               eh0(i,j) = ( -1.d0, 0.d0 ) * h0(i,j) -1.0d0 * s1(i,j) -1.0d0 * s2(i,j)
            END DO
         END DO
         DO i = 1, nmax
            eh0(i,i) = ene + eh0(i,i)
         END DO
       
         DO i = 1, nmax
            DO j =1, nmax
               gm1(i,j) = eh0(i,j)
               g(i,j) = ( 0.d0, 0.d0 )
               IF ( i == j ) g(i,j) = ( 1.d0, 0.d0 )
            END DO
         END DO

         IF ( invert == 1 ) THEN
            CALL zgesv( nmax, nmax, eh0, nmax, ipiv, g, nmax, info )
            IF ( info /= 0 ) THEN
              PRINT*,'error in zgesv - INFO = ', info
              STOP
            END IF
         END IF

      END IF

      RETURN
      END 
