      FUNCTION ranf(idum)

      IMPLICIT NONE

      INTEGER :: m, ia, ic, maxir
      REAL*8 :: rm, ranf
      PARAMETER ( m = 714025 ) 
      PARAMETER ( ia = 1366 )
      PARAMETER ( ic = 150889 ) 
      PARAMETER ( rm = 1.d0 / m )
      PARAMETER  ( maxir = 97 )

      INTEGER :: ir(maxir)
      INTEGER :: idum, iff, j, iy 

      DATA iff / 0 /

      IF ( ( idum < 0 ) .OR. ( iff == 0 ) ) THEN

        iff = 1
        idum = MOD( ic - idum, m )

        DO  j = 1, maxir
           idum = mod( ia * idum + ic, m )
           ir(j) = idum
        END DO

        idum = MOD( ia * idum + ic, m )
        iy = idum

      END IF

      j = 1 + ( maxir * iy ) / m

      IF ( ( j > maxir ) .OR. ( j < 1 ) ) THEN
        PRINT *,' RANF: J=', j
        STOP
      END IF

      iy = ir(j)
      ranf = iy * rm
      idum = MOD( ia * idum + ic, m )
      ir(j) = idum

      RETURN
      END FUNCTION
