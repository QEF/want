!
! Copyright (C) 2004 Andrea Ferretti
! Copyright (C) 2001 PWSCF group
! This file is distributed under the terms of the
! GNU General Public License. See the file `License'
! in the root directory of the present distribution,
! or http://www.gnu.org/copyleft/gpl.txt .
!
! Taken form the file allocate_nlpot.f90 of Espresso package
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
  USE pseud_module,    ONLY : lmax, lloc
  USE ions_module,     ONLY : nat, ntyp => nsp, ityp
  USE lattice_module,  ONLY : tpiba
  USE windows_module,  ONLY : nspin
  USE wfc_module,      ONLY : npwx => npwkx
  USE ggrids_module,   ONLY : ecutwfc, ecutrho

  USE us_module,       ONLY : qrad, tab, tab_at, dq, nqx, nqxq
  USE uspp,            ONLY : indv, nhtol, nhtolm, qq, qb, dvan, deeq, vkb, nkb, &
                              nkbus, nhtoj, becsum, qq_so, dvan_so, deeq_nc
  USE uspp_param,      ONLY : lmaxq, lmaxkb, lll, nbeta, nh, nhm, tvanp
  USE spin_orb_module, ONLY : lspinorb, fcoef
  !
  ! added for WFs
  USE kpoints_module,  ONLY : mxdnn, nkpts
  !
  IMPLICIT NONE
  !
  !    a few local variables
  !
  integer :: nt, na, nb, ldim, ierr  
  ! counters on atom type, atoms, beta functions, ierr
  !

  REAL(dbl) :: gcutm, xqq(3)
  REAL(dbl) :: cell_factor
  !
  ! defining gcutm, xqq, and cell_factor
  !
  gcutm = ecutrho / tpiba**2
  xqq = ZERO
  cell_factor = ONE
  
  !
  !     calculate the number of beta functions for each atomic type
  !
  lmaxkb = - 1
  do nt = 1, ntyp
     nh (nt) = 0
     do nb = 1, nbeta (nt)
        nh (nt) = nh (nt) + 2 * lll (nb, nt) + 1
        lmaxkb = max (lmaxkb, lll (nb, nt) )
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

  IF (lspinorb) then
    ! spin orbit is not implemented
    CALL errore('allocate_nlpot','spinorb not implemented',1)
    !ALLOCATE (qq_so(nhm, nhm, 4, ntyp))    
    !ALLOCATE (dvan_so( nhm, nhm, nspin, ntyp))    
    !ALLOCATE (fcoef(nhm,nhm,2,2,ntyp))
  else
    ALLOCATE (qq(   nhm, nhm, ntyp), STAT=ierr)    
       IF (ierr/=0) CALL errore('allocate_nlpot','allocating qq',ABS(ierr))
    ALLOCATE (dvan( nhm, nhm, ntyp), STAT=ierr)    
       IF (ierr/=0) CALL errore('allocate_nlpot','allocating dvan',ABS(ierr))
    !
    ! added for Wannier calc. (ANDREA)
    ALLOCATE (qb(   nhm, nhm, ntyp, mxdnn, nkpts ), STAT=ierr)
       IF (ierr/=0) CALL errore('allocate_nlpot','allocating qb',ABS(ierr))
  endif
  !
  nqxq = ( (sqrt(gcutm) + sqrt(xqq(1)**2 + xqq(2)**2 + xqq(3)**2) ) &
          / dq + 4) * cell_factor
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
       ALLOCATE (vkb( npwx,  nkb, nkpts), STAT=ierr)    
       IF (ierr/=0) CALL errore('allocate_nlpot','allocating vkb',ABS(ierr))
  ENDIF
  ALLOCATE (becsum( nhm * (nhm + 1)/2, nat, nspin), STAT=ierr)    
       IF (ierr/=0) CALL errore('allocate_nlpot','allocating becsum',ABS(ierr))
  !
  !     Calculate dimensions for array tab (including a possible factor
  !     coming from cell contraction during variable cell relaxation/MD)
  !
  nqx = (sqrt (ecutwfc) / dq + 4) * cell_factor

  ALLOCATE (tab( nqx , nbrx , ntyp), STAT=ierr)    
      IF (ierr/=0) CALL errore('allocate_nlpot','allocating tab',ABS(ierr))
  ALLOCATE (tab_at( nqx , nchix , ntyp), STAT=ierr)
      IF (ierr/=0) CALL errore('allocate_nlpot','allocating tab_at',ABS(ierr))

  RETURN
end subroutine allocate_nlpot

