      SUBROUTINE spline( x, y, n, y2 )

      IMPLICIT NONE

      INTEGER :: n, nmax, i ,k
      PARAMETER( nmax = 500 )
      REAL*8 :: qn, un, sig, p
      REAL*8 ::  x(n), y(n), y2(n), u(nmax)

      y2(1) = 0.d0
      u(1) = 0.d0
      qn = 0.d0
      un = 0.d0

      DO i=2,n-1
        sig = ( x(i) - x(i-1) ) / ( x(i+1) - x(i-1) )
        p = sig * y2(i-1) + 2.d0
        y2(i) = ( sig - 1.d0 ) / p
        u(i) = ( 6.d0 * ( ( y(i+1) - y(i) ) / ( x(i+1) - x(i) ) - ( y(i) - y(i-1) ) / ( x(i) - x(i-1) ) ) /   &
               ( x(i+1) - x(i-1) ) - sig * u(i-1) ) / p
      END DO

      y2(n) = ( un - qn * u(n-1) ) / ( qn * y2(n-1) + 1.d0 )
      DO k = n-1, 1, -1
        y2(k) = y2(k) * y2(k+1) + u(k)
      END DO

      RETURN
      END SUBROUTINE
