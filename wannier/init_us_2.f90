!
! Copyright (C) 2001 PWSCF group
! This file is distributed under the terms of the
! GNU General Public License. See the file `License'
! in the root directory of the present distribution,
! or http://www.gnu.org/copyleft/gpl.txt .
!
#include "machine.h"
!
!----------------------------------------------------------------------
subroutine init_us_2 ( npw_, igk_, q_, vkb_)
  !----------------------------------------------------------------------
  !
  !   Calculates beta functions (Kleinman-Bylander projectors), with
  !   structure factor, for all atoms, in reciprocal space
  !
  USE kinds,          ONLY : dbl
  USE constants,      ONLY : TPI, CZERO, CI
  USE ions_module,    ONLY : nat, ntyp => nsp, ityp, tau
  USE lattice_module, ONLY : tpiba
  USE struct_fact_data_module, ONLY : eigts1, eigts2, eigts3
  USE ggrids_module,  ONLY : g, igv
  USE wfc_data_module,ONLY : npwkx


  USE us_module,  ONLY : dq, tab
  USE uspp,       ONLY : nkb, nhtol, nhtolm, indv
  USE uspp_param, ONLY : lmaxkb, nbeta, nhm, nh
  USE timing_module
  !
  IMPLICIT NONE
  !
  INTEGER      :: npw_           ! input: number of PW's
  INTEGER      :: igk_ (npw_)    ! input: indices of q+G
  REAL(dbl)    :: q_(3)          ! input: q vector
  COMPLEX(dbl) :: vkb_ (npwkx, nkb)    ! output: beta functions
  !
  !     Local variables
  !
  INTEGER :: i0,i1,i2,i3, ig, lm, na, nt, nb, ih, jkb

  REAL(dbl) :: px, ux, vx, wx, arg
  REAL(dbl), allocatable :: gk (:,:), qg (:), vq (:), ylm (:,:), vkb1(:,:)

  COMPLEX(dbl) :: phase, pref
  COMPLEX(dbl), ALLOCATABLE :: sk(:)
  !
  !
  IF (lmaxkb < 0) RETURN
  CALL timing ('init_us_2', OPR='start')

  ALLOCATE (vkb1( npw_,nhm))    
  ALLOCATE (  sk( npw_))    
  ALLOCATE (  qg( npw_))    
  ALLOCATE (  vq( npw_))    
  ALLOCATE ( ylm( npw_, (lmaxkb + 1) **2))    
  ALLOCATE (  gk( 3, npw_))    
  !
  DO ig = 1, npw_
     gk (1,ig) = q_(1) + g(1, igk_(ig) )
     gk (2,ig) = q_(2) + g(2, igk_(ig) )
     gk (3,ig) = q_(3) + g(3, igk_(ig) )
     qg (ig) = gk(1, ig)**2 +  gk(2, ig)**2 + gk(3, ig)**2
  ENDDO
  !
  CALL ylmr2 ((lmaxkb+1)**2, npw_, gk, qg, ylm)
  !
  ! set now qg=|q+G| in atomic units
  !
  DO ig = 1, npw_
     qg(ig) = SQRT(qg(ig))*tpiba
  ENDDO

  jkb = 0
  vkb_(:,:) = CZERO
  !
  DO nt = 1, ntyp
     ! calculate beta in G-space using an interpolation table
     DO nb = 1, nbeta (nt)
        DO ig = 1, npw_
           px = qg (ig) / dq - int (qg (ig) / dq)
           ux = 1.d0 - px
           vx = 2.d0 - px
           wx = 3.d0 - px
           i0 = INT (qg (ig) / dq) + 1
           i1 = i0 + 1
           i2 = i0 + 2
           i3 = i0 + 3
           vq (ig) = tab (i0, nb, nt) * ux * vx * wx / 6.d0 + &
                     tab (i1, nb, nt) * px * vx * wx / 2.d0 - &
                     tab (i2, nb, nt) * px * ux * wx / 2.d0 + &
                     tab (i3, nb, nt) * px * ux * vx / 6.d0
        ENDDO
        ! add spherical harmonic part
        DO ih = 1, nh (nt)
           IF ( nb  == indv (ih, nt) ) THEN
              lm =nhtolm (ih, nt)
              DO ig = 1, npw_
                 vkb1 (ig,ih) = ylm (ig, lm) * vq (ig)
              ENDDO
           ENDIF
        ENDDO
     ENDDO
     !
     ! vkb1 contains all betas including angular part for type nt
     ! now add the structure factor and factor (-i)^l
     !
     DO na = 1, nat
        ! ordering: first all betas for atoms of type 1
        !           then  all betas for atoms of type 2  and so on
        !
        IF (ityp (na) == nt) THEN
           arg = (q_(1) * tau (1, na) + &
                  q_(2) * tau (2, na) + &
                  q_(3) * tau (3, na) ) * TPI
           phase = CMPLX( COS(arg), -SIN(arg), dbl )
           DO ig = 1, npw_
              sk (ig) = eigts1 ( igv(1, igk_(ig)), na) * &
                        eigts2 ( igv(2, igk_(ig)), na) * &
                        eigts3 ( igv(3, igk_(ig)), na)
           ENDDO
           DO ih = 1, nh (nt)
              jkb = jkb + 1
              pref = -CI **nhtol (ih, nt) * phase
              DO ig = 1, npw_
                 vkb_(ig, jkb) = vkb1 (ig,ih) * sk (ig) * pref
              ENDDO
           ENDDO
        ENDIF
     ENDDO
  ENDDO

  DEALLOCATE (gk)
  DEALLOCATE (ylm)
  DEALLOCATE (vq)
  DEALLOCATE (qg)
  DEALLOCATE (sk)
  DEALLOCATE (vkb1)

  CALL timing ('init_us_2',OPR='stop')
  RETURN
END SUBROUTINE init_us_2

