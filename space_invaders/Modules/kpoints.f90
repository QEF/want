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
!

MODULE kpoints
  !

  USE kinds, ONLY: dbl
  USE parameters, ONLY: npkx => npk

  IMPLICIT NONE
  SAVE

  LOGICAL :: first = .TRUE.

  INTEGER :: nk(3)
  REAL(dbl) :: s(3)
  REAL(dbl) :: vkpt( 3, npkx )
  REAL(dbl) :: wtkpt( npkx )
  REAL(dbl) :: wtktot

CONTAINS

  SUBROUTINE kpoints_init( nkpts )
    INTEGER :: nkpts
    INTEGER :: i1, i2, i3, nkp
      nkp = 0
      DO i1 = 0, nk(1)-1
        DO i2 = 0, nk(2)-1
          DO i3 = 0, nk(3)-1
            nkp = nkp + 1
            vkpt(1,nkp) = DBLE(i1)/DBLE(nk(1)) + s(1)
            vkpt(2,nkp) = DBLE(i2)/DBLE(nk(2)) + s(2)
            vkpt(3,nkp) = DBLE(i3)/DBLE(nk(3)) + s(3)
          END DO
        END DO
      END DO

      wtkpt( 1 : nkpts ) = 1.0d0/DBLE( nkpts )
      wtktot = SUM( wtkpt( 1 : nkpts ) )

    RETURN
  END SUBROUTINE

END MODULE
