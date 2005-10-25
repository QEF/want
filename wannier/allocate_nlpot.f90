!
! Copyright (C) 2001 PWSCF group
! This file is distributed under the terms of the
! GNU General Public License. See the file `License'
! in the root directory of the present distribution,
! or http://www.gnu.org/copyleft/gpl.txt .
!
! Taken form the file allocate_nlpot.f90 of Espresso package
! and further modified by AF (2004)
! 
#include "machine.h"
!
!-----------------------------------------------------------------------
subroutine allocate_nlpot
  !-----------------------------------------------------------------------
  !
  ! This routine computes the dimension of the Hamiltonian matrix and
  ! allocates arrays containing the non-local part of the pseudopotential
  !
  ! It computes the following global quantities:
  !
  !     nkb           !  number of beta functions for the solid
  !     nqx           !  number of points of the interpolation table
  !     nh            !  number of beta functions for each atom type
  !     nhm           !  maximum number of beta functions
  !
  !
  USE kinds,           ONLY : dbl
  USE constants,       ONLY : ZERO, ONE
  USE parameters,      ONLY : nbrx, nchix
  USE ions_module,     ONLY : nat, ntyp => nsp, ityp
  USE lattice_module,  ONLY : tpiba
  USE windows_module,  ONLY : nspin
  USE wfc_data_module, ONLY : npwkx
  USE ggrids_module,   ONLY : ecutwfc, ecutrho

  USE us_module,       ONLY : qrad, tab, tab_at, dq, nqx, nqxq
  USE uspp,            ONLY : indv, nhtol, nhtolm, qq, qb, dvan, deeq, vkb, nkb, &
                              nkbus, nhtoj, becsum
  USE uspp_param,      ONLY : lmaxq, lmaxkb, lll, nbeta, nh, nhm, tvanp
  !
  ! added for WFs
  USE kpoints_module,  ONLY : nb
  !
  IMPLICIT NONE
  !
  !    a few local variables
  !
  integer :: nt, na, ib, ierr  
  ! counters on atom type, atoms, beta functions, ierr
  !

  REAL(dbl) :: gcutm, xqq(3)
  !
  ! defining gcutm, xqq
  !
  gcutm = ecutrho / tpiba**2
  xqq = ZERO
  
  !
  !     calculate the number of beta functions for each atomic type
  !
  lmaxkb = - 1
  do nt = 1, ntyp
     nh (nt) = 0
     do ib = 1, nbeta (nt)
        nh (nt) = nh (nt) + 2 * lll (ib, nt) + 1
        lmaxkb = max (lmaxkb, lll (ib, nt) )
     enddo
  enddo
  !
  ! calculate the maximum number of beta functions
  !
  nhm = MAXVAL (nh (1:ntyp))
  !
  ! calculate the number of beta functions of the solid
  !
  nkb = 0
  nkbus = 0
  do na = 1, nat
     nt = ityp(na)
     nkb = nkb + nh (nt)
     if (tvanp(nt)) nkbus = nkbus + nh (nt)
  enddo
  !
  ALLOCATE (indv( nhm, ntyp), STAT=ierr)    
     IF (ierr/=0) CALL errore('allocate_nlpot','allocating indv',ABS(ierr))
  ALLOCATE (nhtol(nhm, ntyp), STAT=ierr)    
     IF (ierr/=0) CALL errore('allocate_nlpot','allocating nhtol',ABS(ierr))
  ALLOCATE (nhtolm(nhm, ntyp), STAT=ierr)    
     IF (ierr/=0) CALL errore('allocate_nlpot','allocating nhtolm',ABS(ierr))
  ALLOCATE (nhtoj(nhm, ntyp), STAT=ierr)    
     IF (ierr/=0) CALL errore('allocate_nlpot','allocating nntoj',ABS(ierr))
  ALLOCATE (deeq( nhm, nhm, nat, nspin), STAT=ierr)    
     IF (ierr/=0) CALL errore('allocate_nlpot','allocating deeq',ABS(ierr))

  ALLOCATE (qq(   nhm, nhm, ntyp), STAT=ierr)    
     IF (ierr/=0) CALL errore('allocate_nlpot','allocating qq',ABS(ierr))
  ALLOCATE (dvan( nhm, nhm, ntyp), STAT=ierr)    
     IF (ierr/=0) CALL errore('allocate_nlpot','allocating dvan',ABS(ierr))
  !
  ! added for Wannier calc. (ANDREA)
  ALLOCATE (qb( nhm, nhm, ntyp, nb ), STAT=ierr)
     IF (ierr/=0) CALL errore('allocate_nlpot','allocating qb',ABS(ierr))
  !
  nqxq = INT (( SQRT(gcutm) + SQRT(xqq(1)**2 + xqq(2)**2 + xqq(3)**2) ) / dq ) + 4
  lmaxq = 2*lmaxkb+1
  !
  IF (lmaxq > 0) THEN 
       ALLOCATE (qrad( nqxq, nbrx*(nbrx+1)/2, lmaxq, ntyp), STAT=ierr)    
       IF (ierr/=0) CALL errore('allocate_nlpot','allocating qrad',ABS(ierr))
  ENDIF
  !
  ! WFs: here we want to manage all projections at the same time
  !      an extra kpt index is added
  !
  IF (nkb > 0) THEN 
       ALLOCATE (vkb( npwkx,  nkb), STAT=ierr)    
       IF (ierr/=0) CALL errore('allocate_nlpot','allocating vkb',ABS(ierr))
  ENDIF
  ALLOCATE (becsum( nhm * (nhm + 1)/2, nat, nspin), STAT=ierr)    
       IF (ierr/=0) CALL errore('allocate_nlpot','allocating becsum',ABS(ierr))
  !
  !     Calculate dimensions for array tab (including a possible factor
  !     coming from cell contraction during variable cell relaxation/MD)
  !
  nqx = INT (SQRT (ecutwfc) / dq ) + 4

  ALLOCATE (tab( nqx , nbrx , ntyp), STAT=ierr)    
      IF (ierr/=0) CALL errore('allocate_nlpot','allocating tab',ABS(ierr))
  ALLOCATE (tab_at( nqx , nchix , ntyp), STAT=ierr)
      IF (ierr/=0) CALL errore('allocate_nlpot','allocating tab_at',ABS(ierr))

  RETURN
end subroutine allocate_nlpot

