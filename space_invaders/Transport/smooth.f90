!
!      Copyright (C) 2004 Arrigo Calzolari, Marco Buongiorno Nardelli
!      Copyright (C) 1999 Marco Buongiorno Nardelli
!
!      This file is distributed under the terms of the
!      GNU General Public License. See the file `License'
!      in the root directory of the present distribution,
!      or http://www.gnu.org/copyleft/gpl.txt .
!
!----------------------------------------------------------------------
      FUNCTION smooth( x )
!----------------------------------------------------------------------

!...  Cut-off function to smoothly remove the long range interactions
!     for Ho et al. TB parametrization

      USE kinds

      IMPLICIT NONE


      REAL(dbl) :: x, xx
      REAL(dbl) :: smooth

      REAL(dbl), PARAMETER ::  rctb = 2.18d0 
      REAL(dbl), PARAMETER ::  ro = 1.536329d0 
      REAL(dbl), PARAMETER ::  xnc = 6.5d0 
      REAL(dbl), PARAMETER ::  aa2 = 0.10284431 
      REAL(dbl), PARAMETER ::  ttb0 = 6.7392620074314d-03 
      REAL(dbl), PARAMETER ::  ttb1 = -8.1885359517898d-02 
      REAL(dbl), PARAMETER ::  ttb2 = 0.1932365259144d0 
      REAL(dbl), PARAMETER ::  ttb3 = 0.354287433238d0 
      
      IF ( x < 2.60d0 ) THEN
         IF ( x < 2.00d0 ) THEN
            smooth = ( ( ro/x )**2 ) * EXP( 2.0 * ( aa2 - ( x/rctb )**xnc ) )
         ELSE
            xx = x - 2.45d0
            smooth = ttb0 + xx * ( ttb1 + xx * ( ttb2 + xx * ttb3 ) )
         END IF
      ELSE
         smooth = 0.0d0
      END IF

      RETURN
      END
