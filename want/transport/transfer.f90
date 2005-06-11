!
!      Copyright (C) 2004 WanT Group
!      Copyright (C) 1999 Marco Buongiorno Nardelli
!
!      This file is distributed under the terms of the
!      GNU General Public License. See the file `License'
!      in the root directory of the present distribution,
!      or http://www.gnu.org/copyleft/gpl.txt .
!
!***************************************************************
   SUBROUTINE transfer( nmax, nterx, tot, tott, h0, h1, ene )
   !***************************************************************
   !
   !...  Iterative construction of the transfer matrix
   !     as Lopez-Sancho and Rubio, J.Phys.F:Met.Phys., v.14, 1205 (1984)
   !     and ibid. v.15, 851 (1985)
   !
   USE kinds
   USE constants, ONLY : CZERO, CONE, ZERO, EPS_m7
   IMPLICIT NONE

      INTEGER :: nmax, nterx
      INTEGER :: ipiv(nmax)
      INTEGER :: i, j, k, l, m, info

      REAL(dbl) :: h0(nmax,nmax)
      REAL(dbl) :: h1(nmax,nmax)
      REAL(dbl) :: conver, conver2


      COMPLEX(dbl) :: tau( nmax, nmax, 2 )
      COMPLEX(dbl) :: taut( nmax, nmax, 2 )
      COMPLEX(dbl) :: tot( nmax, nmax ), tott( nmax, nmax )
      COMPLEX(dbl) :: tsum( nmax, nmax ), tsumt( nmax, nmax )
      COMPLEX(dbl) :: t11( nmax, nmax ), t12( nmax, nmax )
      COMPLEX(dbl) :: s1(nmax,nmax), s2( nmax, nmax )
      COMPLEX(dbl) :: ene

!------------------------------------------------------------

!...  Zero the transfer matrices

      tot(:,:) = CZERO
      tott(:,:) = CZERO

!...  Construction of the transfer matrix

      t12(:,:) = -h0(:,:)
      DO i = 1, nmax
         t12(i,i) = ene + t12(i,i)
      ENDDO

!...  Compute (ene - h0)^-1 and store it in t11 

      DO i = 1, nmax
      DO j = 1, nmax
            t11(i,j) = CZERO
            IF ( i == j ) t11(i,j) = CONE
      ENDDO
      ENDDO

      CALL zgesv( nmax, nmax, t12, nmax, ipiv, t11, nmax, info )
      IF ( info /= 0 )  CALL errore(' transfer ', ' zgesv (I) ', info )

!...  Compute intermediate t-matrices (defined as tau(nmax,nmax,niter)
!     and taut(...)):

!...  Initialize  arrays
      tau(:,:,:) = CZERO
      taut(:,:,:) = CZERO

!...  Compute the starting matrix t_0

      t12(:,:) = h1(:,:)

      CALL zgemm( 'N', 'C', nmax, nmax, nmax, CONE, t11, nmax, t12, nmax, CZERO, tau(1,1,1), nmax )
      CALL zgemm( 'N', 'N', nmax, nmax, nmax, CONE, t11, nmax, t12, nmax, CZERO, taut(1,1,1), nmax )

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
                  
         CALL zgemm( 'N', 'N', nmax, nmax, nmax, CONE, tau(1,1,1), nmax, taut(1,1,1), nmax,   &
                     CZERO, t11, nmax )
         CALL zgemm( 'N', 'N', nmax, nmax, nmax, CONE, taut(1,1,1), nmax, tau(1,1,1), nmax,   &
                     CZERO,t12,nmax)

         DO i = 1, nmax
            DO j = 1, nmax
               s1(i,j) = -( t11(i,j) + t12(i,j) )
               IF ( i == j ) s1(i,j) = CONE - ( t11(i,j) + t12(i,j) )
            END DO
         END DO

         DO i = 1, nmax
            DO j = 1, nmax
               s2(i,j) = CZERO
               IF ( i == j ) s2(i,j) = CONE
            END DO
         END DO

         CALL zgesv( nmax, nmax, s1, nmax, ipiv, s2, nmax, info )
         IF ( info /= 0 )  CALL errore(' transfer ', ' zgesv (II) ', info )

         CALL zgemm( 'N', 'N', nmax, nmax, nmax, CONE, tau(1,1,1), nmax, tau(1,1,1), nmax,   &
                     CZERO,t11,nmax)
         CALL zgemm( 'N', 'N', nmax, nmax, nmax, CONE, taut(1,1,1), nmax, taut(1,1,1), nmax, &
                     CZERO,t12,nmax)
         CALL zgemm( 'N', 'N', nmax, nmax, nmax, CONE, s2, nmax, t11, nmax, CZERO, tau(1,1,2), nmax )
         CALL zgemm( 'N', 'N', nmax, nmax, nmax, CONE, s2, nmax, t12, nmax, CZERO, taut(1,1,2), nmax)

!...  Put the transfer matrices together

         CALL zgemm( 'N', 'N', nmax, nmax, nmax, CONE, tsum, nmax, tau(1,1,2), nmax,  &
                     CZERO, t11, nmax )
         CALL zgemm( 'N', 'N', nmax, nmax, nmax, CONE, tsum, nmax, taut(1,1,2), nmax, &
                     CZERO, s1, nmax )
         CALL zcopy( nmax*nmax, t11, 1, s2, 1 )
         CALL zaxpy( nmax*nmax, CONE, tot, 1, s2, 1 )

         DO i = 1, nmax
            DO j = 1, nmax
               tot(i,j) = s2(i,j)
               tsum(i,j) = s1(i,j)
            END DO
         END DO

         CALL zgemm( 'N', 'N', nmax, nmax, nmax, CONE, tsumt, nmax, taut(1,1,2), nmax,    &
                     CZERO,t11,nmax )
         CALL zgemm( 'N', 'N', nmax, nmax, nmax, CONE, tsumt, nmax, tau(1,1,2), nmax,     &
                     CZERO, s1, nmax )
         CALL zcopy( nmax*nmax, t11, 1, s2, 1 )
         CALL zaxpy( nmax*nmax, CONE, tott, 1, s2, 1 )

         DO i = 1, nmax
            DO j = 1, nmax
               tott(i,j) = s2(i,j)
               tsumt(i,j) = s1(i,j)
               tau(i,j,1) = tau(i,j,2)
               taut(i,j,1) = taut(i,j,2)
           END DO
         END DO

!...  Convergence chech on the t-matrices

         conver = ZERO
         conver2 = ZERO
         DO i = 1, nmax
            DO j = 1, nmax
                conver  = conver  + DSQRT( REAL( tau(i,j,2)  )**2 + AIMAG( tau(i,j,2)  )**2 )
                conver2 = conver2 + DSQRT( REAL( taut(i,j,2) )**2 + AIMAG( taut(i,j,2) )**2 )
            END DO
         END DO
         IF ( conver < EPS_m7 .AND. conver2 <  EPS_m7 ) RETURN
      END DO 

      IF ( conver > EPS_m7 .OR. conver2 > EPS_m7 ) &
           CALL errore('transfer', 'bad t-matrix convergence', 10 )

      RETURN 
   END SUBROUTINE transfer

