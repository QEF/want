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

  LOGICAL :: first = .TRUE.

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
    RETURN
  END SUBROUTINE

  
  SUBROUTINE gauss1( cphi, ndir, l_wann, m_wann, rpos1, dist1 )

    USE io_module, ONLY : stdout

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

    IF  ( ndir == 3 ) THEN
      dist_pl  = SQRT( rpos1(1)**2 + rpos1(2)**2 )
      dist_cos = rpos1(3)
    ELSE IF  ( ndir == 2 ) THEN
      dist_pl  = SQRT( rpos1(1)**2 + rpos1(3)**2 )
      dist_cos = rpos1(2)
    ELSE IF  ( ndir == 1 ) THEN
      dist_pl  = SQRT( rpos1(2)**2 + rpos1(3)**2 )
      dist_cos = rpos1(1)
    ELSE 
      WRITE(stdout,*) 'ERROR: Wrong z-direction'
      CALL errore(' gauss1 ', ' wrong z- direction ', ndir )
    END IF 

    ! ... IF  rpos is on the origin, or on the z axis, I give arbitrary
    !     values to cos/sin of theta, or of phi, respectively

    IF  ( ABS( dist1 ) <= 1.d-10 ) THEN
      th_cos = 0.0d0
      th_sin = 0.0d0
    ELSE
      th_cos = dist_cos / dist1
      th_sin = dist_pl / dist1
    END IF

    IF  (ABS( dist_pl ) <= 1.d-10 ) THEN
      ph_cos = 0.0d0
      ph_sin = 0.0d0
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
        WRITE(stdout,*) 'ERROR: check the spherical harmonics (I)'
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
        WRITE(stdout,*) 'ERROR: check the spherical harmonics (II)'
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
        WRITE (stdout, *)  '*** ERROR *** in sp^3 hybrid gaussian: check m_wann'
        CALL errore(' gauss1 ', ' sp^3 hybrid gaussian ', m_wann )
      END IF
    ELSE
      WRITE(stdout,*) '*** ERROR *** : check the spherical harmonics (III)'
      CALL errore(' gauss1 ', ' check the spherical harmonics (III)', m_wann )
    END IF

    RETURN
  END SUBROUTINE

  !
END MODULE sph_har

