!
! Copyright (C) 2005 WanT Group
!
! This file is distributed under the terms of the
! GNU General Public License. See the file `License'
! in the root directory of the present distribution,
! or http://www.gnu.org/copyleft/gpl.txt .
!
! Based on a previous version from the Espresso package
!
!-----------------------------------------------------------------------
SUBROUTINE atomic_wfc (ik, xk, iatom, il, npw, vkg, ylm, wfcatom)
  !-----------------------------------------------------------------------
  !
  ! This routine computes a
  ! given k-point.
  !
  USE kinds,           ONLY : dbl
  USE constants,       ONLY : ZERO, CI
  USE atom_module,     ONLY : nchi, lchi, oc
  USE ions_module,     ONLY : ityp, tau
  USE lattice_module,  ONLY : alat
  USE us_module,       ONLY : tab_at, dq
  USE wfc_data_module, ONLY : igsort
  USE ggrids_module,   ONLY : igv
  USE struct_fact_data_module, &
                       ONLY : eigts1, eigts2, eigts3
  USE timing_module
  IMPLICIT NONE

  !
  ! input variables
  !
  INTEGER,      INTENT(in)  :: ik, npw, iatom, il
  REAL(dbl),    INTENT(in)  :: xk(3)
  REAL(dbl),    INTENT(in)  :: vkg(npw), ylm(npw)
  COMPLEX(dbl), INTENT(out) :: wfcatom (npw)

  !
  ! local variables
  !
  INTEGER :: ierr 
  INTEGER :: nt, nb, ib, ig, iig, i0, i1, i2, i3
  REAL(dbl) :: arg, px, ux, vx, wx
  REAL(dbl), ALLOCATABLE :: chiq (:)
  COMPLEX(dbl) :: sk, kphase, lphase

!
!--------------------------
! Main body
!--------------------------
!

  CALL timing ('atomic_wfc',OPR='start')

  ALLOCATE ( chiq(npw), STAT=ierr)
     IF (ierr/=0) CALL errore('atomic_wfc','allocating data',ABS(ierr))

  !
  ! get indexes related to the IATOM and IL requested 
  ! atomic wfc
  !
  IF ( iatom <= 0 ) CALL errore('atomic_wfc','invalid iatom',-iatom+1)
  nt = ityp( iatom )
  !
  nb = -1
  DO ib = 1, nchi(nt)
     IF ( lchi(ib,nt) == il .AND. oc(ib,nt) >= ZERO ) nb = ib
  ENDDO
  IF ( nb == -1 ) CALL errore('atomic_wfc','searching for nb with the required param', iatom)
  

  !
  ! chiq = radial fourier transform of atomic orbitals chi
  ! vkg in bohr^-1
  !
  DO ig = 1, npw
       px = vkg(ig) / dq - INT ( vkg(ig) / dq)
       ux = 1.0_dbl - px
       vx = 2.0_dbl - px
       wx = 3.0_dbl - px
       i0 = INT( vkg(ig) / dq ) + 1
       i1 = i0 + 1
       i2 = i0 + 2
       i3 = i0 + 3
       chiq (ig) = &
              tab_at (i0, nb, nt) * ux * vx * wx / 6.0_dbl + &
              tab_at (i1, nb, nt) * px * vx * wx / 2.0_dbl - &
              tab_at (i2, nb, nt) * px * ux * wx / 2.0_dbl + &
              tab_at (i3, nb, nt) * px * ux * vx / 6.0_dbl
  ENDDO

  !
  ! tau is in alat units (and then converted to bohr), 
  ! xk in bohr^-1 
  !
  arg = DOT_PRODUCT(xk(:), tau(:,iatom) ) * alat
  kphase = CMPLX (cos (arg), - sin (arg), dbl )
  !
  !  the factor i^l MUST BE PRESENT in order to produce
  !  wavefunctions for k=0 that are real in real space
  !
  lphase = CI**il

  DO ig = 1, npw
      iig = igsort (ig,ik)
      sk  = kphase * eigts1 (igv(1,iig), iatom) * eigts2 (igv(2,iig), iatom) * &
                         eigts3 (igv(3,iig), iatom)
      !
      wfcatom (ig) = lphase * sk * ylm (ig) * chiq (ig)
  ENDDO
           

  !
  ! local cleaning
  !
  DEALLOCATE(chiq, STAT=ierr )
     IF (ierr/=0) CALL errore('atomic_wfc','deallocating data',ABS(ierr))

  CALL timing ('atomic_wfc',OPR='stop')
END SUBROUTINE atomic_wfc

