!
! Copyright (C) 2001 PWSCF group
! This file is distributed under the terms of the
! GNU General Public License. See the file `License'
! in the root directory of the present distribution,
! or http://www.gnu.org/copyleft/gpl.txt .
!
!-----------------------------------------------------------------------
SUBROUTINE atomic_wfc (ik, xk, natomwfc, wfcatom)
  !-----------------------------------------------------------------------
  !
  ! This routine computes the  superposition of atomic wavefunctions for a
  ! given k-point.
  !
  USE kinds,           ONLY : dbl
  USE constants,       ONLY : TPI, ZERO, CI
  USE parameters,      ONLY : nchix
  USE atom_module,     ONLY : nchi, lchi, chi, oc, r, rab, msh
  USE lattice_module,  ONLY : omega, tpiba
  USE ions_module,     ONLY : nat, ntyp => nsp, ityp, tau
  USE struct_fact_data_module, ONLY : eigts1, eigts2, eigts3
  USE ggrids_module,   ONLY : igv, g
  USE wfc_data_module, ONLY : npwkx, npwk, igsort 
  USE windows_module,  ONLY : nbnd
  USE us_module,       ONLY : tab_at, dq
  USE timing_module
  !
  IMPLICIT NONE
  !
  INTEGER,      INTENT(in)  :: ik, natomwfc
  REAL(dbl),    INTENT(in)  :: xk(3)
  COMPLEX(dbl), INTENT(out) :: wfcatom (npwkx, natomwfc) 
  !
  INTEGER :: n_starting_wfc, lmax_wfc, npw, ierr 
  INTEGER :: nt, l, nb, na, m, lm, ig, iig, i0, i1, i2, i3
  !
  REAL(dbl), ALLOCATABLE :: qg(:), ylm (:,:), chiq (:,:,:), aux (:), &
       gk (:,:), vchi (:)
  COMPLEX(dbl), ALLOCATABLE :: sk (:)
  REAL(dbl) :: vqint, arg, px, ux, vx, wx
  COMPLEX(dbl) :: kphase  , lphase

!
!--------------------------
! Main body
!--------------------------
!

  CALL timing ('atomic_wfc',OPR='start')
  npw = npwk(ik)

  ALLOCATE ( qg(npw), chiq(npw,nchix,ntyp), gk(3,npw), sk(npw), STAT=ierr)
     IF (ierr/=0) CALL errore('atomic_wfc','allocating data',ABS(ierr))

  ! calculate max angular momentum required in wavefunctions
  lmax_wfc = 0
  DO nt = 1, ntyp
     DO nb = 1, nchi (nt)
        lmax_wfc = max (lmax_wfc, lchi (nb, nt) )
     ENDDO
  ENDDO
  !
  ALLOCATE(ylm (npw,(lmax_wfc+1)**2) )
  !
  do ig = 1, npw
     gk (1,ig) = xk(1) + g(1, igsort(ig,ik) )
     gk (2,ig) = xk(2) + g(2, igsort(ig,ik) )
     gk (3,ig) = xk(3) + g(3, igsort(ig,ik) )
     qg(ig) = gk(1, ig)**2 +  gk(2, ig)**2 + gk(3, ig)**2
  enddo
  !
  !  ylm = spherical harmonics
  !
  CALL ylmr2 ((lmax_wfc+1)**2, npw, gk, qg, ylm)
  !
  ! set now q=|k+G| in atomic units
  !
  DO ig = 1, npw
     qg(ig) = SQRT(qg(ig))*tpiba
  ENDDO
  !
  n_starting_wfc = 0
  !
  ! chiq = radial fourier transform of atomic orbitals chi
  !
  DO nt = 1, ntyp
     DO nb = 1, nchi (nt)
        IF ( oc (nb, nt) >= ZERO) then
           DO ig = 1, npw
              px = qg (ig) / dq - int (qg (ig) / dq)
              ux = 1.0_dbl - px
              vx = 2.0_dbl - px
              wx = 3.0_dbl - px
              i0 = qg (ig) / dq + 1
              i1 = i0 + 1
              i2 = i0 + 2
              i3 = i0 + 3
              chiq (ig, nb, nt) = &
                     tab_at (i0, nb, nt) * ux * vx * wx / 6.0_dbl + &
                     tab_at (i1, nb, nt) * px * vx * wx / 2.0_dbl - &
                     tab_at (i2, nb, nt) * px * ux * wx / 2.0_dbl + &
                     tab_at (i3, nb, nt) * px * ux * vx / 6.0_dbl
           ENDDO
        ENDIF
     ENDDO
  ENDDO

  DO na = 1, nat
     arg = (xk(1)*tau(1,na) + xk(2)*tau(2,na) + xk(3)*tau(3,na)) * TPI
     kphase = DCMPLX (cos (arg), - sin (arg) )
     !
     !     sk is the structure factor
     !
     DO ig = 1, npw
        iig = igsort (ig,ik)
        sk (ig) = kphase * eigts1 (igv(1,iig), na) * eigts2 (igv(2,iig), na) * &
                           eigts3 (igv(3,iig), na)
     ENDDO
     !
     nt = ityp (na)
     DO nb = 1, nchi (nt)
        IF (oc (nb, nt) >= ZERO ) THEN
           l = lchi (nb, nt)
           lphase = CI**l
           !  the factor i^l MUST BE PRESENT in order to produce
           !  wavefunctions for k=0 that are real in real space
            DO m = 1, 2 * l + 1
              lm = l**2 + m
              n_starting_wfc = n_starting_wfc + 1
              IF (n_starting_wfc.gt.natomwfc) CALL errore ('atomic_wfc', 'too many wfcs', 1)
              DO ig = 1, npw
                 wfcatom (ig, n_starting_wfc) = lphase * &
                      sk (ig) * ylm (ig, lm) * chiq (ig, nb, nt)
              ENDDO
           ENDDO
        ENDIF
     ENDDO
  ENDDO

  IF (n_starting_wfc.ne.natomwfc) CALL errore ('atomic_wfc', 'something wrong', 1)

  DEALLOCATE(qg, chiq ,gk, sk ,ylm, STAT=ierr )
     IF (ierr/=0) CALL errore('atomic_wfc','deallocating data',ABS(ierr))

  CALL timing ('atomic_wfc',OPR='stop')
  RETURN
END SUBROUTINE atomic_wfc

