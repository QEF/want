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
  USE kinds,          ONLY : dbl
  USE constants,      ONLY : ZERO, ONE
  USE log_module,     ONLY : log_push, log_pop
  USE ggrids_module,  ONLY : gamma_only
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
  REAL(dbl), ALLOCATABLE :: betapsi(:,:)
  INTEGER :: ierr

  !
  IF ( nkb == 0 ) RETURN
  !
  CALL timing( 'ccalbec', OPR='start' )
  CALL log_push( 'ccalbec' )
  !
  !
  IF ( gamma_only ) THEN
     !
     !CALL pw_gemm( 'Y', nkb, nbnd, npw, vkb, npwx, psi, npwx, bec, nkb )
     !
     ALLOCATE( betapsi(nkb, nbnd), STAT=ierr )
     IF (ierr/=0 ) CALL errore('ccalbec','allocating betapsi',ABS(ierr))
     !
     !
     IF ( nbnd == 1 ) THEN
        !   
        CALL DGEMV( 'C', 2*npw, nkb, 2.0_dbl, vkb, 2*npwx, psi, 1, 0.0_dbl, &
                     betapsi, 1 ) 
        !IF ( gstart == 2 ) betapsi(:,1) = betapsi(:,1) - beta(1,:)*psi(1,1)
        betapsi(:,1) = betapsi(:,1) - vkb(1,:)*psi(1,1)
        !   
     ELSE
        !   
        CALL DGEMM( 'C', 'N', nkb, nbnd, 2*npw, 2.0_dbl, vkb, 2*npwx, psi, &
                    2*npwx, 0.0_dbl, betapsi, nkb )
        !IF ( gstart == 2 ) & 
           CALL DGER( nkb, nbnd, -1.0_dbl, vkb, 2*npwx, psi, 2*npwx, betapsi, nkb )
        !   
     ENDIF  
     !
     bec(1:nkb,:) = CMPLX( betapsi(:,:), 0.0_dbl, KIND=dbl)
     !
     DEALLOCATE( betapsi, STAT=ierr )
     IF (ierr/=0 ) CALL errore('ccalbec','deallocating betapsi',ABS(ierr))
     !
     !
  ELSE
     !   
     CALL ZGEMM( 'C', 'N', nkb, nbnd, npw, (1.0d0,0.0d0) , &
                 vkb, npwx, psi, npwx, (0.0d0,0.0d0), bec, nkbx )
  ENDIF
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
