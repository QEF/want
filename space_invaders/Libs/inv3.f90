      SUBROUTINE inv3( a, b, det )
 
      IMPLICIT NONE
      INTEGER :: i, j, k, l, kk, ll
      REAL*8 ::  det
      REAL*8 ::  a(3,3), b(3,3)
      REAL*8 ::  z(6,6)

! ... Quick routine to invert 3x3 matrix
!     inverse is of a(i,j) is b(i,j)/det

      DO  i = 1, 2
        DO  j = 1, 2
          DO  k = 1, 3
            DO  l = 1, 3
              kk = 3 * (i-1) + k
              ll = 3 * (j-1) + l
              z(kk,ll) = a(k,l)
            END DO
          END DO
        END DO
      END DO

      det= 0.d0

      DO i=1,3
        det = det + z(1,i) * z(2,i+1) * z(3,i+2)
      END DO

      DO  i = 4, 6
        det = det - z(1,i) * z(2,i-1) * z(3,i-2)
      END DO

      DO j = 1, 3
        DO i = 1, 3
          b(j,i) = ( z(i+1,j+1) * z(i+2,j+2) - z(i+1,j+2) * z(i+2,j+1) )
        END DO
      END DO

      RETURN
      END SUBROUTINE
