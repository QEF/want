! 
! Copyright (C) 2004 WanT Group
! Copyright (C) 2002 Nicola Marzari, Ivo Souza, David Vanderbilt 
! Copyright (C) 1997 Nicola Marzari, David Vanderbilt 
! 
! This file is distributed under the terms of the 
! GNU General Public License. See the file `License' 
! in the root directory of the present distribution, 
! or http://www.gnu.org/copyleft/gpl.txt . 
! 
      SUBROUTINE compar(a,b,ifpos,ifneg)

        USE kinds
        USE constants, ONLY : EPS_m8

        IMPLICIT NONE

        REAL(dbl) :: a(3),b(3), rrp, rrm
        INTEGER :: ifpos, ifneg

        rrp = ( a(1) - b(1) )**2 + ( a(2) - b(2) )**2 + ( a(3) - b(3) )**2
        rrm = ( a(1) + b(1) )**2 + ( a(2) + b(2) )**2 + ( a(3) + b(3) )**2

        ifpos = 0
        IF ( ABS(rrp) < EPS_m8 ) ifpos = 1

        ifneg = 0
        IF ( ABS(rrm) < EPS_m8 ) ifneg = 1

      RETURN
      END SUBROUTINE compar
