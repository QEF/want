      SUBROUTINE compar(a,b,ifpos,ifneg)

        USE kinds

        IMPLICIT NONE

        REAL(dbl) :: a(3),b(3), rrp, rrm
        INTEGER :: ifpos, ifneg

        rrp = ( a(1) - b(1) )**2 + ( a(2) - b(2) )**2 + ( a(3) - b(3) )**2
        rrm = ( a(1) + b(1) )**2 + ( a(2) + b(2) )**2 + ( a(3) + b(3) )**2

        ifpos = 0
        IF ( ABS(rrp) < 1.e-08 ) ifpos = 1

        ifneg = 0
        IF ( ABS(rrm) < 1.e-08 ) ifneg = 1

      RETURN
      END SUBROUTINE
