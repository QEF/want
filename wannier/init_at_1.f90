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
  USE kinds,           ONLY : dbl
  USE constants,       ONLY : ZERO, FPI
  USE log_module,      ONLY : log_push, log_pop
  USE atom_module,     ONLY : rgrid, msh
  USE lattice_module,  ONLY : omega
  USE ions_module,     ONLY : nsp
  USE us_module,       ONLY : tab_at, nqx, dq
  USE uspp_param,      ONLY : upf
  USE timing_module,   ONLY : timing
  !
  IMPLICIT NONE
  !
  INTEGER :: nt, nb, iq, ir, l, startq, lastq, ndm, ierr
  !
  REAL(dbl), ALLOCATABLE :: aux (:), vchi (:)
  REAL(dbl) :: vqint, pref, q


  CALL timing ('init_at_1', OPR='start')
  CALL log_push ('init_at_1')

  ndm = MAXVAL (msh(1:nsp))

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
  DO nt = 1, nsp
     DO nb = 1, upf(nt)%nwfc
        IF ( upf(nt)%oc(nb) >= 0.d0) then
           l = upf(nt)%lchi (nb)
           DO iq = startq, lastq
              q = dq * (iq - 1)
              call sph_bes (msh(nt), rgrid(nt)%r, q, l, aux)
              DO ir = 1, msh(nt)
                 vchi(ir) = upf(nt)%chi(ir,nb) * aux(ir) * rgrid(nt)%r(ir)
              ENDDO
              CALL simpson (msh(nt), vchi, rgrid(nt)%rab, vqint)
              tab_at (iq, nb, nt) = vqint * pref
           ENDDO
        ENDIF
     ENDDO
 ENDDO
!#ifdef __PARA
!  CALL reduce (nqx * nchix * nsp, tab_at)
!#endif

  DEALLOCATE(aux ,vchi, STAT=ierr)
     IF (ierr/=0) CALL errore('init_at_1','deallocating aux, vchi', ABS(ierr))

  CALL timing ('init_at_1', OPR='stop')
  CALL log_pop ('init_at_1')

END SUBROUTINE init_at_1

