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
      SUBROUTINE bastr( dir, rec, vol )
!=----------------------------------------------------------------------------------=
!
! This subroutine calculates the set of basis vectors in reciprocal space.
!                                          
!     Lattice vectors:
!                ( dir(1,1) dir(2,1) dir(3,1) )
!     dir(I,J) = ( dir(1,2) dir(2,2) dir(3,2) ) 
!                ( dir(1,3) dir(2,3) dir(3,3) )  
!     
!     Reciprocal vectors:
!                ( rec(1,1) rec(2,1) rec(3,1) )
!     rec(I,J) = ( rec(1,2) rec(2,2) rec(3,2) )
!                ( rec(1,3) rec(2,3) rec(3,3) ) 
!
!=-----------------------------------------------------------------------------------

      USE kinds
      USE constants, ONLY: pi, twopi => tpi

      IMPLICIT NONE

      REAL(dbl) :: dir(3,3)
      REAL(dbl) :: rec(3,3)
      REAL(dbl) :: vol
 
      INTEGER :: i, j
 

      rec(1,1) = dir(2,2) * dir(3,3) - dir(3,2) * dir(2,3)
      rec(1,2) = dir(2,3) * dir(3,1) - dir(3,3) * dir(2,1)
      rec(1,3) = dir(2,1) * dir(3,2) - dir(3,1) * dir(2,2)
      rec(2,1) = dir(3,2) * dir(1,3) - dir(1,2) * dir(3,3)
      rec(2,2) = dir(3,3) * dir(1,1) - dir(1,3) * dir(3,1)
      rec(2,3) = dir(3,1) * dir(1,2) - dir(1,1) * dir(3,2)
      rec(3,1) = dir(1,2) * dir(2,3) - dir(2,2) * dir(1,3)
      rec(3,2) = dir(1,3) * dir(2,1) - dir(2,3) * dir(1,1)
      rec(3,3) = dir(1,1) * dir(2,2) - dir(2,1) * dir(1,2)

      vol = dir(1,1) * rec(1,1) + dir(1,2) * rec(1,2) + dir(1,3) * rec(1,3)

      DO i = 1, 3
        DO j = 1, 3
          rec(i,j) = twopi * rec(i,j) / vol
        END DO
      END DO
      vol = ABS(vol)

      RETURN
      END SUBROUTINE
