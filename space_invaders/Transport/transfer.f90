!----------------------------------------------------------------------
      SUBROUTINE transfer( nmax, nterx, tot, tott, h0, h1, ene )
!----------------------------------------------------------------------

      IMPLICIT NONE

!...  Iterative construction of the transfer matrix
!     as Lopez-Sancho and Rubio, J.Phys.F:Met.Phys., v.14, 1205 (1984)
!     and ibid. v.15, 851 (1985)

      INTEGER :: nmax, nterx
      INTEGER :: ipiv(nmax)
      INTEGER :: i, j, k, l, m, info

      REAL*8 :: h0(nmax,nmax)
      REAL*8 :: h1(nmax,nmax)
      REAL*8 :: conver, conver2


      COMPLEX*16 :: tau( nmax, nmax, 2 )
      COMPLEX*16 :: taut( nmax, nmax, 2 )
      COMPLEX*16 :: tot( nmax, nmax ), tott( nmax, nmax )
      COMPLEX*16 :: tsum( nmax, nmax ), tsumt( nmax, nmax )
      COMPLEX*16 :: t11( nmax, nmax ), t12( nmax, nmax )
      COMPLEX*16 :: s1(nmax,nmax), s2( nmax, nmax )
      COMPLEX*16 :: ene
      COMPLEX*16 :: alpha, beta

!...  Scalar for BLAS calls
      alpha = ( 1.d0, 0.d0 )
      beta = ( 0.d0, 0.d0 )

!...  Zero the transfer matrices

      tot(:,:) = ( 0.d0, 0.d0 )
      tott(:,:) = ( 0.d0, 0.d0 )

!...  Construction of the transfer matrix

      DO j = 1, nmax
         DO i = 1, nmax
            t12(i,j) = ( -1.d0, 0.d0 ) * h0(i,j)
         END DO
      END DO
      DO i = 1, nmax
         t12(i,i) = ene + t12(i,i)
      END DO

!...  Compute (ene - h0)^-1 and store it in t11 

      DO i = 1, nmax
         DO j = 1, nmax
            t11(i,j) = ( 0.d0, 0.d0 )
            IF ( i == j ) t11(i,j) = ( 1.d0, 0.d0 )
         END DO
      END DO

      CALL zgesv( nmax, nmax, t12, nmax, ipiv, t11, nmax, info )

      IF ( info /= 0 ) THEN
        PRINT*, 'error 1 in ZGESV - INFO = ', info
        STOP
      END IF

!...  Compute intermediate t-matrices (defined as tau(nmax,nmax,niter)
!     and taut(...)):

!...  Initialize  arrays
      tau(:,:,:) = ( 0.d0, 0.d0 )
      taut(:,:,:) = ( 0.d0, 0.d0 )

!...  Compute the starting matrix t_0

      DO i = 1, nmax
         DO j = 1, nmax
            t12(i,j) = ( 1.d0, 0.d0 ) * h1(i,j)
         END DO
      END DO

      CALL zgemm( 'N', 'C', nmax, nmax, nmax, alpha, t11, nmax, t12, nmax, beta, tau(1,1,1), nmax )
      CALL zgemm( 'N', 'N', nmax, nmax, nmax, alpha, t11, nmax, t12, nmax, beta, taut(1,1,1), nmax )

!...  Initialize T
      DO i = 1, nmax
         DO j = 1, nmax
            tot(i,j) = tau(i,j,1)
            tsum(i,j) = taut(i,j,1)
         END DO
      END DO

!...  Initialize T^bar 
      DO i = 1, nmax
         DO j = 1, nmax
            tott(i,j) = taut(i,j,1)
            tsumt(i,j) = tau(i,j,1)
         END DO
      END DO

!...  Main loop 
      DO m = 1, nterx
                  
         t11(:,:) = ( 0.d0, 0.d0 ) 
         t12(:,:) = ( 0.d0, 0.d0 ) 

         CALL zgemm( 'N', 'N', nmax, nmax, nmax, alpha, tau(1,1,1), nmax, taut(1,1,1), nmax,   &
                     beta, t11, nmax )
         CALL zgemm( 'N', 'N', nmax, nmax, nmax, alpha, taut(1,1,1), nmax, tau(1,1,1), nmax,   &
                     beta,t12,nmax)

         DO i = 1, nmax
            DO j = 1, nmax
               s1(i,j) = -( t11(i,j) + t12(i,j) )
               IF ( i == j ) s1(i,j) = 1.d0 - ( t11(i,j) + t12(i,j) )
            END DO
         END DO

         DO i = 1, nmax
            DO j = 1, nmax
               s2(i,j) = ( 0.d0, 0.d0 )
               IF ( i == j ) s2(i,j) = ( 1.d0, 0.d0 )
            END DO
         END DO

         CALL zgesv( nmax, nmax, s1, nmax, ipiv, s2, nmax, info )
         IF ( info /= 0 ) THEN
           PRINT*,'error 2 in ZGESV - INFO = ', info
           STOP
         END IF

         t11(:,:) = ( 0.d0, 0.d0 ) 
         t12(:,:) = ( 0.d0, 0.d0 ) 

         CALL zgemm( 'N', 'N', nmax, nmax, nmax, alpha, tau(1,1,1), nmax, tau(1,1,1), nmax,   &
                     beta,t11,nmax)
         CALL zgemm( 'N', 'N', nmax, nmax, nmax, alpha, taut(1,1,1), nmax, taut(1,1,1), nmax, &
                     beta,t12,nmax)
         CALL zgemm( 'N', 'N', nmax, nmax, nmax, alpha, s2, nmax, t11, nmax, beta, tau(1,1,2), nmax )
         CALL zgemm( 'N', 'N', nmax, nmax, nmax, alpha, s2, nmax, t12, nmax, beta, taut(1,1,2), nmax )

!...  Put the transfer matrices together

         t11(:,:) = ( 0.d0, 0.d0 ) 
         s1(:,:) = ( 0.d0, 0.d0 ) 

         CALL zgemm( 'N', 'N', nmax, nmax, nmax, alpha, tsum, nmax, tau(1,1,2), nmax,  &
                     beta, t11, nmax )
         CALL zgemm( 'N', 'N', nmax, nmax, nmax, alpha, tsum, nmax, taut(1,1,2), nmax, &
                     beta, s1, nmax )
         CALL zcopy( nmax*nmax, t11, 1, s2, 1 )
         CALL zaxpy( nmax*nmax, alpha, tot, 1, s2, 1 )

         DO i = 1, nmax
            DO j = 1, nmax
               tot(i,j) = s2(i,j)
               tsum(i,j) = s1(i,j)
            END DO
         END DO

         t11(:,:)= ( 0.d0, 0.d0 ) 
         s1(:,:)= ( 0.d0, 0.d0 ) 

         CALL zgemm( 'N', 'N', nmax, nmax, nmax, alpha, tsumt, nmax, taut(1,1,2), nmax,    &
                     beta,t11,nmax )
         CALL zgemm( 'N', 'N', nmax, nmax, nmax, alpha, tsumt, nmax, tau(1,1,2), nmax,     &
                     beta, s1, nmax )
         CALL zcopy( nmax*nmax, t11, 1, s2, 1 )
         CALL zaxpy( nmax*nmax, alpha, tott, 1, s2, 1 )

         DO i = 1, nmax
            DO j = 1, nmax
               tott(i,j) = s2(i,j)
               tsumt(i,j) = s1(i,j)
               tau(i,j,1) = tau(i,j,2)
               taut(i,j,1) = taut(i,j,2)
           END DO
         END DO

!...  Convergence chech on the t-matrices

         conver = 0.d0
         conver2 = 0.d0
         DO i = 1, nmax
            DO j = 1, nmax
                conver  = conver  + DSQRT( REAL( tau(i,j,2)  )**2 + AIMAG( tau(i,j,2)  )**2 )
                conver2 = conver2 + DSQRT( REAL( taut(i,j,2) )**2 + AIMAG( taut(i,j,2) )**2 )
            END DO
         END DO
         IF ( conver < 1.d-7 .AND. conver2 <  1.d-7 ) RETURN
      END DO 

      IF ( conver > 1.d-7 .OR. conver2 > 1.d-7 ) THEN
        PRINT*,' bad t-matrix convergence', conver, conver2
        STOP
      END IF

      RETURN 
      END

