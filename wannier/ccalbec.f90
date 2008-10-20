!
! Copyright (C) 2001-2004 PWSCF group
! This file is distributed under the terms of the
! GNU General Public License. See the file `License'
! in the root directory of the present distribution,
! or http://www.gnu.org/copyleft/gpl.txt .
!
!-----------------------------------------------------------------------
SUBROUTINE ccalbec( nkbx, nkb, npwx, npw, nbnd, bec, vkb, psi )
  !-----------------------------------------------------------------------
  !
  !    This subroutine computes the dot product of the beta functions
  !    and the wavefunctions, and save them in the array bec.
  !
  USE kinds,        ONLY : dbl
  USE constants,    ONLY : ZERO, ONE
  USE log_module,   ONLY : log_push, log_pop
  USE timing_module
  !
  IMPLICIT NONE
  !
  ! ... here the dummy variables
  !
  INTEGER :: nkbx, nkb, npwx, npw, nbnd
    ! input: the total number of beta functions
    ! input: the maximum number of plane waves
    ! input: the length of the vectors
    ! input: the number of bands
  COMPLEX(KIND=dbl) ::  vkb(npwx,nkb), psi(npwx,nbnd), bec(nkbx,nbnd)
    ! input: the FT of the beta functions
    ! input: the wavefunctions
    ! output: dot product of the beta and the wavefunctions
  !
  !
  IF ( nkb == 0 ) RETURN
  !
  CALL timing( 'ccalbec', OPR='start' )
  CALL log_push( 'ccalbec' )
  !
! Also GAMMA_ONLY is avoided
!  IF ( gamma_only ) THEN
!     !
!     CALL pw_gemm( 'Y', nkb, nbnd, npw, vkb, npwx, psi, npwx, bec, nkb )
!     !
!  ELSE
     !   
     CALL ZGEMM( 'C', 'N', nkb, nbnd, npw, (1.0d0,0.0d0) , &
                 vkb, npwx, psi, npwx, (0.0d0,0.0d0), bec, nkbx )
     !
     ! as before, we are serial and therefore REDUCE is commented
     !
     ! CALL reduce( 2 * nkb * nbnd, bec )
     !
!  END IF
  !
  CALL timing( 'ccalbec', OPR='stop' )
  CALL log_pop( 'ccalbec' )
  !
END SUBROUTINE ccalbec
