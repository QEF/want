!
! Copyright (C) 2004 Arrigo Calzolari, Carlo Cavazzoni, Marco Buongiorno Nardelli
! Copyright (C) 2002 Nicola Marzari, Ivo Souza, David Vanderbilt
! Copyright (C) 1997 Nicola Marzari, David Vanderbilt
!
! This file is distributed under the terms of the
! GNU General Public License. See the file `License'
! in the root directory of the present distribution,
! or http://www.gnu.org/copyleft/gpl.txt .
!
!=----------------------------------------------------------------------------------=
      FUNCTION ranf(idum)
!=----------------------------------------------------------------------------------=

      USE kinds

      IMPLICIT NONE

      INTEGER :: m, ia, ic, maxir
      REAL(dbl) :: rm, ranf
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

      IF ( ( j > maxir ) .OR. ( j < 1 ) ) CALL errore(' ranf ', ' ranf: j ', j ) 

      iy = ir(j)
      ranf = iy * rm
      idum = MOD( ia * idum + ic, m )
      ir(j) = idum

      RETURN
      END FUNCTION
