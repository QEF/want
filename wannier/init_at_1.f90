!
! Copyright (C) 2001 PWSCF group
! This file is distributed under the terms of the
! GNU General Public License. See the file `License'
! in the root directory of the present distribution,
! or http://www.gnu.org/copyleft/gpl.txt .
!
!-----------------------------------------------------------------------
SUBROUTINE init_at_1()
  !-----------------------------------------------------------------------
  !
  ! This routine computes a table with the radial Fourier transform 
  ! of the atomic wavefunctions.
  !
  USE parameters, ONLY : nchix
  USE constants,  ONLY : ZERO, FPI
  USE kinds,      ONLY : dbl
  USE atom_module,     ONLY : nchi, lchi, chi, oc, r, rab, msh
  USE lattice_module,  ONLY : omega
  USE ions_module,     ONLY : ntyp => nsp
  USE us_module,       ONLY : tab_at, nqx, dq
  USE timing_module,   ONLY : timing
  !
  IMPLICIT NONE
  !
  INTEGER :: nt, nb, iq, ir, l, startq, lastq, ndm, ierr
  !
  REAL(dbl), allocatable :: aux (:), vchi (:)
  REAL(dbl) :: vqint, pref, q


  CALL timing ('init_at_1', OPR='start')
  ndm = MAXVAL (msh(1:ntyp))

  ALLOCATE (aux(ndm),vchi(ndm), STAT=ierr)
     IF (ierr/=0) CALL errore('init_at_1','allocating aux, vchi', ABS(ierr))

  !
  ! chiq = radial fourier transform of atomic orbitals chi
  !
  pref = FPI/SQRT(omega)
  ! needed to normalize atomic wfcs (not a bad idea in general and 
  ! necessary to compute correctly lda+U projections)
  !
  ! instead of the divide routine
  startq = 1
  lastq = nqx

  tab_at(:,:,:) = ZERO
  DO nt = 1, ntyp
     DO nb = 1, nchi (nt)
        IF (oc (nb, nt) >= 0.d0) then
           l = lchi (nb, nt)
           DO iq = startq, lastq
              q = dq * (iq - 1)
              call sph_bes (msh(nt), r(1,nt), q, l, aux)
              DO ir = 1, msh(nt)
                 vchi(ir) = chi(ir,nb,nt) * aux(ir) * r(ir,nt)
              ENDDO
              CALL simpson (msh(nt), vchi, rab(1,nt), vqint)
              tab_at (iq, nb, nt) = vqint * pref
           ENDDO
        ENDIF
     ENDDO
 ENDDO
#ifdef __PARA
  CALL reduce (nqx * nchix * ntyp, tab_at)
#endif

  DEALLOCATE(aux ,vchi, STAT=ierr)
     IF (ierr/=0) CALL errore('init_at_1','deallocating aux, vchi', ABS(ierr))

  CALL timing ('init_at_1', OPR='stop')

END SUBROUTINE init_at_1

