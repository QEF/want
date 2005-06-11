!
!      Copyright (C) 2004 WanT Group
!      Copyright (C) 1999 Marco Buongiorno Nardelli
!
!      This file is distributed under the terms of the
!      GNU General Public License. See the file `License'
!      in the root directory of the present distribution,
!      or http://www.gnu.org/copyleft/gpl.txt .
!
!***********************************************************************
   SUBROUTINE transferb( nmaxb, nterx, tot, tott, c00_b, c01_b, ene )
   !***********************************************************************
   !
   !...  Iterative construction of the transfer matrix
   !     as Lopez-Sancho and Rubio, J.Phys.F:Met.Phys., v.14, 1205 (1984)
   !     and ibid. v.15, 851 (1985)
   !
   USE kinds
   USE constants, ONLY : CZERO, CONE, ZERO, EPS_m7
   IMPLICIT NONE


      INTEGER :: nmaxb, nterx
      INTEGER :: ipiv(nmaxb)
      INTEGER :: i, j, k, l, m, info

      REAL(dbl) :: conver, conver2
      COMPLEX(dbl) :: c00_b(nmaxb,nmaxb)
      COMPLEX(dbl) :: c01_b(nmaxb,nmaxb)

      COMPLEX(dbl) :: tau(nmaxb,nmaxb,2)
      COMPLEX(dbl) :: taut(nmaxb,nmaxb,2)
      COMPLEX(dbl) :: tot(nmaxb,nmaxb)
      COMPLEX(dbl) :: tott(nmaxb,nmaxb)
      COMPLEX(dbl) :: tsum(nmaxb,nmaxb)
      COMPLEX(dbl) :: tsumt(nmaxb,nmaxb)
      COMPLEX(dbl) :: t11(nmaxb,nmaxb)
      COMPLEX(dbl) :: t12(nmaxb,nmaxb)
      COMPLEX(dbl) :: s1(nmaxb,nmaxb)
      COMPLEX(dbl) :: s2(nmaxb,nmaxb)
      COMPLEX(dbl) :: ene

!----------------------------------------------------


!...  Construction of the transfer matrix

      t12(:,:) = -c00_b(:,:)

!...  Compute (ene - c00_b)^-1 and store it in t11 

      DO i = 1, nmaxb
         DO j = 1, nmaxb
            t11(i,j) = CZERO
            IF ( i == j ) t11(i,j) = CONE
         END DO
      END DO

      CALL zgesv( nmaxb, nmaxb, t12, nmaxb, ipiv, t11, nmaxb, info )
      IF ( info /= 0 )  CALL errore(' Stransfreb ', ' zgesv (I) ', info )

!...  Compute intermediate t-matrices (defined as tau(nmaxb,nmaxb,niter)
!     and taut(...))

!...  Compute t_0

      CALL zgemm( 'N', 'C', nmaxb, nmaxb, nmaxb, CONE, t11, nmaxb, c01_b, nmaxb,    &
                  CZERO, tau(1,1,1), nmaxb )
      CALL zgemm( 'N', 'N', nmaxb, nmaxb, nmaxb, CONE, t11, nmaxb, c01_b, nmaxb,    &
                  CZERO, taut(1,1,1), nmaxb )

!...  Initialize T

      DO i = 1, nmaxb
         DO j = 1, nmaxb
            tot(i,j) = tau(i,j,1)
            tsum(i,j) = taut(i,j,1)
         END DO
      END DO

!...  Initialize T^bar

      DO i = 1, nmaxb
         DO j = 1, nmaxb
            tott(i,j) = taut(i,j,1)
            tsumt(i,j) = tau(i,j,1)
         END DO
      END DO

!...  Main loop

      DO m = 1, nterx
                  

         CALL zgemm( 'N', 'N', nmaxb, nmaxb, nmaxb, CONE, tau(1,1,1), nmaxb, taut(1,1,1), nmaxb, &
                     CZERO, t11, nmaxb )
         CALL zgemm( 'N', 'N', nmaxb, nmaxb, nmaxb, CONE, taut(1,1,1), nmaxb, tau(1,1,1), nmaxb, &
                     CZERO, t12, nmaxb )

         DO i = 1, nmaxb
            DO j = 1, nmaxb
               s1(i,j) = -( t11(i,j) + t12(i,j) )
               IF ( i == j ) s1(i,j) = 1.d0 -( t11(i,j) + t12(i,j) )
            END DO
         END DO

         DO i = 1, nmaxb
            DO j = 1, nmaxb
               s2(i,j) = CZERO
               IF ( i == j ) s2(i,j) = CONE
            END DO
         END DO

         CALL zgesv( nmaxb, nmaxb, s1, nmaxb, ipiv, s2, nmaxb, info )
         IF ( info /= 0 )  CALL errore(' Stransfreb ', ' zgesv (II) ', info )


         CALL zgemm( 'N', 'N', nmaxb, nmaxb, nmaxb, CONE, tau(1,1,1), nmaxb, tau(1,1,1), nmaxb,   &
                     CZERO, t11, nmaxb )
         CALL zgemm( 'N', 'N', nmaxb, nmaxb, nmaxb, CONE, taut(1,1,1), nmaxb, taut(1,1,1), nmaxb, &
                     CZERO, t12, nmaxb )
         CALL zgemm( 'N', 'N', nmaxb, nmaxb, nmaxb, CONE, s2, nmaxb, t11, nmaxb,                  &
                     CZERO, tau(1,1,2), nmaxb )
         CALL zgemm( 'N', 'N', nmaxb, nmaxb, nmaxb, CONE, s2, nmaxb, t12, nmaxb,                  &
                     CZERO, taut(1,1,2), nmaxb )

!...     Put the transfer matrices together


         CALL zgemm( 'N', 'N', nmaxb, nmaxb, nmaxb, CONE, tsum, nmaxb, tau(1,1,2), nmaxb,         &
                     CZERO, t11, nmaxb )
         CALL zgemm( 'N', 'N', nmaxb, nmaxb, nmaxb, CONE, tsum, nmaxb, taut(1,1,2), nmaxb,        & 
                     CZERO, s1, nmaxb )
         CALL zcopy( nmaxb*nmaxb, t11, 1, s2, 1 )
         CALL zaxpy( nmaxb*nmaxb, CONE, tot, 1, s2, 1 )

         DO i = 1, nmaxb
            DO j = 1, nmaxb
               tot(i,j) = s2(i,j)
               tsum(i,j) = s1(i,j)
            END DO
         END DO


         CALL zgemm( 'N', 'N', nmaxb, nmaxb, nmaxb, CONE, tsumt, nmaxb, taut(1,1,2), nmaxb,       &
                     CZERO, t11, nmaxb )
         CALL zgemm( 'N', 'N', nmaxb, nmaxb, nmaxb, CONE, tsumt, nmaxb, tau(1,1,2), nmaxb,        &
                     CZERO, s1, nmaxb )
         CALL zcopy( nmaxb*nmaxb, t11, 1, s2, 1 )
         CALL zaxpy( nmaxb*nmaxb, CONE, tott, 1, s2, 1 )

         DO i = 1, nmaxb
            DO j = 1, nmaxb
               tott(i,j) = s2(i,j)
               tsumt(i,j) = s1(i,j)
               tau(i,j,1) = tau(i,j,2)
               taut(i,j,1) = taut(i,j,2)
           END DO
         END DO

!...    Convergence chech on the t-matrices

         conver = ZERO
         conver2 = ZERO

         DO i = 1, nmaxb
            DO j = 1, nmaxb
                conver = conver + DSQRT( REAL( tau(i,j,2) )**2 + AIMAG( tau(i,j,2) )**2 )
                conver2 = conver2 + DSQRT( REAL( taut(i,j,2) )**2 + AIMAG( taut(i,j,2) )**2 )
            END DO
         END DO
         IF ( conver < EPS_m7 .AND. conver2 < EPS_m7 ) RETURN

      END DO 

      IF( conver > EPS_m7 .OR. conver2 > EPS_m7 )  &
          CALL errore('transfreb', 'bad t-matrix convergence', 10 )

      RETURN 
   END SUBROUTINE transferb

