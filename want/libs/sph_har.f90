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

  USE constants, ONLY : EPS_m9, PI, TPI, ZERO
  USE kinds, ONLY: dbl

  IMPLICIT NONE
  SAVE

  REAL(dbl) :: sph00, sph1m1, sph10, sph11, sph2m2
  REAL(dbl) :: sph2m1, sph20, sph21, sph22

  LOGICAL :: first = .TRUE.

CONTAINS

  SUBROUTINE sph_har_init( )
    sph00 = 1.d0/SQRT( 2.d0 * TPI )
    sph1m1 = SQRT( 1.5d0 / TPI )
    sph10 = SQRT( 1.5d0 / TPI )
    sph11 = SQRT( 1.5d0 / TPI )
    sph2m2 = SQRT( 15.d0 / 2.d0 / TPI )
    sph2m1 = SQRT( 15.d0 / 2.d0 / TPI )
    sph20 = SQRT(  5.d0 / 8.d0 / TPI )
    sph21 = SQRT( 15.d0 / 2.d0 / TPI )
    sph22 = SQRT( 15.d0 / 2.d0 / TPI )
    RETURN
  END SUBROUTINE

  
  SUBROUTINE gauss1( cphi, ndir, l_wann, m_wann, rpos1, dist1 )
    IMPLICIT NONE

    INTEGER :: ndir, l_wann, m_wann
    REAL(dbl) :: rpos1(3), dist1
    COMPLEX(dbl) :: cphi

    REAL(dbl) :: dist_pl, dist_cos
    REAL(dbl) :: th_cos, th_sin
    REAL(dbl) :: ph_cos, ph_sin
   
    IF( first ) THEN
      CALL sph_har_init
      first = .FALSE.
    END IF

 
    SELECT CASE ( ndir )
    CASE ( 3 )
        dist_pl  = SQRT( rpos1(1)**2 + rpos1(2)**2 )
        dist_cos = rpos1(3)
    CASE ( 2 )
        dist_pl  = SQRT( rpos1(1)**2 + rpos1(3)**2 )
        dist_cos = rpos1(2)
    CASE ( 1 )
        dist_pl  = SQRT( rpos1(2)**2 + rpos1(3)**2 )
        dist_cos = rpos1(1)
    CASE DEFAULT
        CALL errore(' gauss1 ', ' wrong z- direction ', ABS(ndir)+1 )
    END SELECT


    ! ... IF  rpos is on the origin, or on the z axis, I give arbitrary
    !     values to cos/sin of theta, or of phi, respectively

    IF  ( ABS( dist1 ) <= EPS_m9 ) THEN
      th_cos = ZERO
      th_sin = ZERO
    ELSE
      th_cos = dist_cos / dist1
      th_sin = dist_pl / dist1
    END IF

    IF  (ABS( dist_pl ) <= 1.d-10 ) THEN
      ph_cos = ZERO
      ph_sin = ZERO
    ELSE
      IF ( ndir == 3 ) THEN
        ph_cos = rpos1(1) / dist_pl
        ph_sin = rpos1(2) / dist_pl
      ELSE IF ( ndir == 2 ) THEN
        ph_cos = rpos1(3) / dist_pl
        ph_sin = rpos1(1) / dist_pl
      ELSE
        ph_cos = rpos1(2) / dist_pl
        ph_sin = rpos1(3) / dist_pl
      END IF
    END IF

    IF ( l_wann == 2 ) THEN

      IF ( m_wann == -2 ) THEN
        cphi = sph2m2 * cphi * ( th_sin**2 ) * ( ph_cos**2 - ph_sin**2 )
      ELSE IF ( m_wann == -1 ) THEN
        cphi = sph2m1 * cphi * th_sin * th_cos * ph_cos
      ELSE IF ( m_wann == 0 ) THEN
        cphi = sph20 * cphi * ( 3.0d0 * th_cos**2 - 1.0d0 )
      ELSE IF ( m_wann == 1 ) THEN
        cphi = sph21 * cphi * th_sin * th_cos * ph_sin
      ELSE IF ( m_wann == 2 ) THEN
        cphi = sph22 * cphi * ( th_sin**2 ) * 2.0d0 * ph_sin * ph_cos
      ELSE
        CALL errore(' gauss1 ', ' check the spherical harmonics (I)', m_wann )
      END IF

    ELSE IF ( l_wann == 1 ) THEN

      IF ( m_wann == -1 ) THEN
        cphi = sph1m1 * cphi * th_sin * ph_cos
      ELSE IF ( m_wann == 0 ) THEN
        cphi = sph10 * cphi * th_cos
      ELSE IF ( m_wann == 1 ) THEN
        cphi = sph11 * cphi * th_sin * ph_sin
      ELSE
        CALL errore(' gauss1 ', ' check the spherical harmonics (II)', m_wann )
      END IF

    ELSE IF ( l_wann == 0 ) THEN

      cphi = sph00 * cphi

    ELSE IF ( l_wann == -1 ) THEN

      ! ...  sp^3 orbitals

      IF ( m_wann == 1 ) THEN
        ! ... sp^3 along 111 direction IF  ndir_wann(nwann)=3
        cphi = cphi * ( sph00 + sph1m1 * th_sin * ph_cos +        &
               sph11 * th_sin * ph_sin + sph10 * th_cos ) / 2.0d0
      ELSE IF ( m_wann == 2 ) THEN
        ! ... sp^3 along 1,-1,-1 direction IF  ndir_wann(nwann)=3
        cphi = cphi * ( sph00 + sph1m1 * th_sin * ph_cos -        &
               sph11 * th_sin * ph_sin - sph10 * th_cos ) / 2.0d0
      ELSE IF ( m_wann == 3 ) THEN
        ! ...  sp^3 along -1,1,-1 direction IF  ndir_wann(nwann)=3
        cphi = cphi * ( sph00 - sph1m1 * th_sin * ph_cos +        &
               sph11 * th_sin * ph_sin - sph10 * th_cos ) / 2.0d0
      ELSE IF ( m_wann == 4 ) THEN
        ! ... sp^3 along -1,-1,1 direction IF  ndir_wann(nwann)=3
        cphi = cphi * ( sph00 - sph1m1 * th_sin * ph_cos -        &
               sph11 * th_sin * ph_sin + sph10 * th_cos ) / 2.0d0
      ELSE IF ( m_wann == -1 ) THEN
        ! ...  sp^3 along -1,-1,-1 direction IF  ndir_wann(nwann)=3
        cphi = cphi * ( sph00 - sph1m1 * th_sin * ph_cos -        &
               sph11 * th_sin * ph_sin - sph10 * th_cos ) / 2.0d0
      ELSE IF ( m_wann == -2 ) THEN
        ! ...  sp^3 along -1,1,1 direction IF  ndir_wann(nwann)=3
        cphi = cphi * ( sph00 - sph1m1 * th_sin * ph_cos +        &
               sph11 * th_sin * ph_sin + sph10 * th_cos ) / 2.0d0
      ELSE IF ( m_wann == -3 ) THEN
        ! ...  sp^3 along 1,-1,1 direction IF  ndir_wann(nwann)=3
        cphi = cphi * ( sph00 + sph1m1 * th_sin * ph_cos -        &
               sph11 * th_sin * ph_sin + sph10 * th_cos ) / 2.0d0
      ELSE IF ( m_wann == -4 ) THEN
        ! ...  sp^3 along 1,1,-1 direction IF  ndir_wann(nwann)=3
        cphi = cphi * ( sph00 + sph1m1 * th_sin * ph_cos +        &
               sph11 * th_sin * ph_sin - sph10 * th_cos ) / 2.0d0
      ELSE
        CALL errore(' gauss1 ', ' sp^3 hybrid gaussian ', m_wann )
      END IF
    ELSE
      CALL errore(' gauss1 ', ' check the spherical harmonics (III)', m_wann )
    END IF

    RETURN
  END SUBROUTINE

  !
END MODULE sph_har

