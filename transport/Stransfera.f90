!----------------------------------------------------------------------
      SUBROUTINE Stransfera( nmaxa, nterx, tot, tott, c00_a, c01_a, ene )
!----------------------------------------------------------------------

      USE kinds

      IMPLICIT NONE

!...  Iterative construction of the transfer matrix
!     as Lopez-Sancho and Rubio, J.Phys.F:Met.Phys., v.14, 1205 (1984)
!     and ibid. v.15, 851 (1985)

      INTEGER :: nmaxa, nterx
      INTEGER :: ipiv(nmaxa)
      INTEGER :: i, j, k, l, m, info

      REAL(dbl) :: conver, conver2
      COMPLEX(dbl) :: c00_a(nmaxa,nmaxa)
      COMPLEX(dbl) :: c01_a(nmaxa,nmaxa)

      COMPLEX(dbl) :: tau(nmaxa,nmaxa,2)
      COMPLEX(dbl) :: taut(nmaxa,nmaxa,2)
      COMPLEX(dbl) :: tot(nmaxa,nmaxa)
      COMPLEX(dbl) :: tott(nmaxa,nmaxa)
      COMPLEX(dbl) :: tsum(nmaxa,nmaxa)
      COMPLEX(dbl) :: tsumt(nmaxa,nmaxa)
      COMPLEX(dbl) :: t11(nmaxa,nmaxa)
      COMPLEX(dbl) :: t12(nmaxa,nmaxa)
      COMPLEX(dbl) :: s1(nmaxa,nmaxa)
      COMPLEX(dbl) :: s2(nmaxa,nmaxa)
      COMPLEX(dbl) :: ene,alpha,beta

!...  Scalar for BLAS calls
      alpha = ( 1.d0, 0.d0 )
      beta = ( 0.d0, 0.d0 )

!...  Construction of the transfer matrix

      DO j = 1, nmaxa
         DO i = 1, nmaxa
            t12(i,j) = -1.d0 * c00_a(i,j)
         END DO
      END DO

!...  Compute (ene - c00_a)^-1 and store it in t11 

      DO i = 1, nmaxa
         DO j = 1, nmaxa
            t11(i,j) = ( 0.d0, 0.d0 )
            IF ( i == j ) t11(i,j) = ( 1.d0, 0.d0 )
         END DO
      END DO

      CALL zgesv( nmaxa, nmaxa, t12, nmaxa, ipiv, t11, nmaxa, info )
      IF ( info /= 0 )  CALL errore(' Stransfera ', ' zgesv (I) ', info )

!...  Compute intermediate t-matrices (defined as tau(nmaxa,nmaxa,niter)
!     and taut(...)):

!...  Initialize

      tau(:,:,:) = ( 0.0, 0.0 )
      taut(:,:,:) = ( 0.0, 0.0 )

!...  Compute t_0

      CALL zgemm( 'N', 'C', nmaxa, nmaxa, nmaxa, alpha, t11, nmaxa, c01_a, nmaxa,    &
                  beta, tau(1,1,1), nmaxa )
      CALL zgemm( 'N', 'N', nmaxa, nmaxa, nmaxa, alpha, t11, nmaxa, c01_a, nmaxa,    &
                  beta, taut(1,1,1), nmaxa )

!...  Initialize T

      DO i = 1, nmaxa
         DO j = 1, nmaxa
            tot(i,j) = tau(i,j,1)
            tsum(i,j) = taut(i,j,1)
         END DO
      END DO

!...  Initialize T^bar

      DO i = 1, nmaxa
         DO j = 1, nmaxa
            tott(i,j) = taut(i,j,1)
            tsumt(i,j) = tau(i,j,1)
         END DO
      END DO

!...  Main loop:

      DO m = 1, nterx
                  
         t11(:,:) = ( 0.0, 0.0 ) 
         t12(:,:) = ( 0.0, 0.0 ) 

         CALL zgemm( 'N', 'N', nmaxa, nmaxa, nmaxa, alpha, tau(1,1,1), nmaxa, taut(1,1,1), nmaxa, &
                     beta, t11, nmaxa )
         CALL zgemm( 'N', 'N', nmaxa, nmaxa, nmaxa, alpha, taut(1,1,1), nmaxa, tau(1,1,1), nmaxa, &
                     beta, t12, nmaxa )

         DO i = 1, nmaxa
            DO j = 1, nmaxa
               s1(i,j) = -( t11(i,j) + t12(i,j) )
               IF ( i == j ) s1(i,j) = 1.d0 -( t11(i,j) + t12(i,j) )
            END DO
         END DO

         DO i = 1, nmaxa
            DO j = 1, nmaxa
               s2(i,j) = ( 0.d0, 0.d0 )
               IF ( i == j ) s2(i,j) = ( 1.d0, 0.d0 )
            END DO
         END DO

         CALL zgesv( nmaxa, nmaxa, s1, nmaxa, ipiv, s2, nmaxa, info )
         IF ( info /= 0 )  CALL errore(' Stransfera ', ' zgesv (II) ', info )

         t11(:,:) = ( 0.0, 0.0 ) 
         t12(:,:) = ( 0.0, 0.0 ) 

         CALL zgemm( 'N', 'N', nmaxa, nmaxa, nmaxa, alpha, tau(1,1,1), nmaxa, tau(1,1,1), nmaxa,   &
                      beta, t11, nmaxa )
         CALL zgemm( 'N', 'N', nmaxa, nmaxa, nmaxa, alpha, taut(1,1,1), nmaxa, taut(1,1,1), nmaxa, &
                     beta, t12, nmaxa )
         CALL zgemm( 'N', 'N', nmaxa, nmaxa, nmaxa, alpha, s2, nmaxa, t11, nmaxa,                  &
                     beta, tau(1,1,2), nmaxa )
         CALL zgemm( 'N', 'N', nmaxa, nmaxa, nmaxa, alpha, s2, nmaxa, t12, nmaxa,                  &
                     beta, taut(1,1,2), nmaxa )

!...  Put the transfer matrices together

         t11(:,:) = ( 0.0, 0.0 ) 
         s1(:,:)  = ( 0.0, 0.0 ) 

         CALL zgemm( 'N', 'N', nmaxa, nmaxa, nmaxa, alpha, tsum, nmaxa, tau(1,1,2), nmaxa,         &
                     beta, t11, nmaxa )
         CALL zgemm( 'N', 'N', nmaxa, nmaxa, nmaxa, alpha, tsum, nmaxa, taut(1,1,2), nmaxa,        &
                     beta, s1, nmaxa )
         CALL zcopy( nmaxa*nmaxa, t11, 1, s2, 1 )
         CALL zaxpy( nmaxa*nmaxa, alpha, tot, 1, s2, 1 )

         DO i = 1, nmaxa
            DO j = 1, nmaxa
               tot(i,j) = s2(i,j)
               tsum(i,j) = s1(i,j)
            END DO
         END DO

         t11(:,:) = ( 0.0, 0.0 ) 
         s1(:,:)  = ( 0.0, 0.0 ) 

         CALL zgemm( 'N', 'N', nmaxa, nmaxa, nmaxa, alpha, tsumt, nmaxa, taut(1,1,2), nmaxa,      &
                     beta, t11, nmaxa )
         CALL zgemm( 'N', 'N', nmaxa, nmaxa, nmaxa, alpha, tsumt, nmaxa, tau(1,1,2), nmaxa,       &
                     beta, s1, nmaxa )
         CALL zcopy( nmaxa*nmaxa, t11, 1, s2, 1 )
         CALL zaxpy( nmaxa*nmaxa, alpha, tott, 1, s2, 1 )

         DO i=1,nmaxa
            DO j=1,nmaxa
               tott(i,j) = s2(i,j)
               tsumt(i,j) = s1(i,j)
               tau(i,j,1) = tau(i,j,2)
               taut(i,j,1) = taut(i,j,2)
           END DO
         END DO

!...     Convergence chech on the t-matrices

         conver = 0.d0
         conver2 = 0.d0

         DO i = 1, nmaxa
            DO j = 1, nmaxa
                conver = conver + DSQRT( REAL( tau(i,j,2) )**2 + AIMAG( tau(i,j,2) )**2 )
                conver2= conver2 + DSQRT( REAL( taut(i,j,2) )**2 + AIMAG( taut(i,j,2) )**2 )
            END DO
         END DO
         IF ( conver < 1.d-7 .AND. conver2 < 1.d-7 ) RETURN

      END DO 

      IF ( conver > 1.d-7 .OR. conver2 > 1.d-7 ) CALL errore(' Stransfera ', ' bad t-matrix convergence ', conver )

      RETURN 
      END
