      SUBROUTINE splint( xa, ya, y2a, n, x, y )

      IMPLICIT NONE  
      REAL*8 :: xa(n), ya(n), y2a(n)
      REAL*8 :: x, y, a, b, rh
      INTEGER :: N, klo, khi, k


      klo = 1
      khi = n
1     IF ( khi - klo > 1 ) THEN
        k = ( khi + klo ) / 2
        IF ( xa(k) > x ) THEN
          khi = k
        ELSE
          klo = k
        END IF
        GO TO 1
      END IF

      rh = xa(khi) - xa(klo)

      IF ( rh == 0.d0 ) PAUSE 'bad xa input in splint'

      a = ( xa(khi) - x ) / rh
      b = ( x - xa(klo) ) / rh
      y = a * ya(klo) + b * ya(khi) + ( (a**3-a) * y2a(klo) + (b**3-b) * y2a(khi) ) * ( rh**2 ) / 6.d0
 
      RETURN
      END SUBROUTINE
