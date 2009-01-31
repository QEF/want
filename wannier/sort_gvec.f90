!
! Copyright (C) 2001 PWSCF group
! This file is distributed under the terms of the
! GNU General Public License. See the file `License'
! in the root directory of the present distribution,
! or http://www.gnu.org/copyleft/gpl.txt .
!
!
!---------------------------------------------------------------------

subroutine sort_gvec( ng, g2, mill, idx)
  !---------------------------------------------------------------------
  !
  !     first the input variables
  !
  use kinds, ONLY: dbl
  use constants, ONLY: eps8 => EPS_m8
  implicit none
  INTEGER,   INTENT(IN)    :: ng
  REAL(dbl), INTENT(INOUT) :: g2( * )
  INTEGER,   INTENT(INOUT) :: mill( 3, * )
  INTEGER,   INTENT(OUT)   :: idx( * )

  REAL(dbl), ALLOCATABLE :: gsort( : )
  INTEGER :: ig, icurr, it, im
  REAL(dbl) :: gsq

  ALLOCATE( gsort( ng ) )

  DO ig = 1, ng
    IF ( g2(ig) > eps8 ) THEN
      gsort(ig) = g2(ig)
    ELSE
      gsort(ig) = 0.d0
    END IF
  END DO

  idx(1) = 0
  CALL hpsort_eps( ng, gsort( 1 ), idx( 1 ), eps8 )

  ! ... sort indices accordingly
  DO ig = 1, ng-1
    icurr = ig
30  IF( idx(icurr) /= ig ) THEN
      ! ...     swap g-vec indices
      im = mill(1,icurr); mill(1,icurr) = mill(1,idx(icurr)); mill(1,idx(icurr)) = im
      im = mill(2,icurr); mill(2,icurr) = mill(2,idx(icurr)); mill(2,idx(icurr)) = im
      im = mill(3,icurr); mill(3,icurr) = mill(3,idx(icurr)); mill(3,idx(icurr)) = im
      ! ...     swap modules
      gsq = g2( icurr ); g2( icurr ) = g2( idx(icurr) ); g2( idx(icurr) ) = gsq
      ! ...     swap indices
      it = icurr; icurr = idx(icurr); idx(it) = it
      IF( idx(icurr) == ig ) THEN
        idx(icurr) = icurr
      ELSE
        GOTO 30
      END IF
    END IF
  END DO

  DEALLOCATE( gsort )

  return
end subroutine sort_gvec
