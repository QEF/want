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

MODULE sph_har
  !

  USE kinds, ONLY: dbl

  IMPLICIT NONE
  SAVE

  REAL(dbl) :: sph00, sph1m1, sph10, sph11, sph2m2
  REAL(dbl) :: sph2m1, sph20, sph21, sph22

CONTAINS

  SUBROUTINE sph_har_init( )
    USE constants, ONLY: pi, twopi => tpi
    sph00 = 1.d0/sqrt( 2.d0 * twopi )
    sph1m1 = sqrt( 1.5d0 / twopi )
    sph10 = sqrt( 1.5d0 / twopi )
    sph11 = sqrt( 1.5d0 / twopi )
    sph2m2 = sqrt( 15.d0 / 2.d0 / twopi )
    sph2m1 = sqrt( 15.d0 / 2.d0 / twopi )
    sph20 = sqrt(  5.d0 / 8.d0 / twopi )
    sph21 = sqrt( 15.d0 / 2.d0 / twopi )
    sph22 = sqrt( 15.d0 / 2.d0 / twopi )
  END SUBROUTINE

  !
END MODULE sph_har

