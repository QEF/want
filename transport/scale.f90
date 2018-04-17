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
      SUBROUTINE scale( x, apu, tbpar )
!----------------------------------------------------------------------

!...  Cut-off function to smoothly remove the long range interactions
!     for Charlier et al. parametrization

      USE kinds

      IMPLICIT NONE
    
      REAL(dbl) :: tbpar(6), x
      REAL(dbl) :: apu
      REAL(dbl), PARAMETER :: ro=3.335d0

      IF ( x < 2.60d0 ) THEN
         IF ( x > 1.85d0 ) THEN
            apu = ( ro/x )**2
            tbpar(3) = -0.18d0
            tbpar(4) = 0.d0
            tbpar(5) = 0.35d0
            tbpar(6) = -0.10d0
         ELSE
            apu = 1.0d0
            tbpar(3) = -4.3
            tbpar(4) = 4.98
            tbpar(5) = 6.38
            tbpar(6) = -2.66
         END IF
      ELSE
         apu = 0.0d0
         tbpar(3) = 0.0d0
         tbpar(4) = 0.0d0
         tbpar(5) = 0.0d0
         tbpar(6) = 0.0d0
      END IF

      RETURN
      END
