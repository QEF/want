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
      SUBROUTINE setv0( n, a )
!----------------------------------------------------------------------
      USE kinds

      IMPLICIT NONE

      INTEGER :: n, i

      COMPLEX(dbl) :: a(n)

      DO i = 1, n
         a(i) = ( 0.d0, 0.d0 )
      END DO

      RETURN
      END

!----------------------------------------------------------------------
      SUBROUTINE setd0( n, a )
!----------------------------------------------------------------------
      USE kinds

      IMPLICIT NONE
 
      INTEGER :: n, i

      REAL(dbl) :: a(n)

      DO i = 1, n
         a(i) = 0.d0
      END DO

      RETURN
      END

!----------------------------------------------------------------------
      SUBROUTINE seti0( n, a )
!----------------------------------------------------------------------
      USE kinds

      IMPLICIT NONE

      INTEGER :: i, n

      INTEGER :: a(n)

      DO i = 1, n
         a(i) = 0
      END DO

      RETURN
      END
