!
! Copyright (C) 2005 Andrea Ferretti
! Copyright (C) 2001-2003 PWSCF group
! This file is distributed under the terms of the
! GNU General Public License. See the file `License'
! in the root directory of the present distribution,
! or http://www.gnu.org/copyleft/gpl.txt .
!
! Modified by Andrea Ferretti from the original version s_psi.f90
! of Espresso package
! 
#include "machine.h"
!
!----------------------------------------------------------------------------
SUBROUTINE augment_psi( lda, n, m, ik, aug_qq, psi, spsi )
  !----------------------------------------------------------------------------
  !
  !    This routine applies the S_aug matrix to m wavefunctions psi
  !    and puts the results in spsi.
  !    Requires the products of psi with all beta functions
  !    in array becp(nkb,m,ik) (calculated in h_psi or by ccalbec)
  ! input:
  !     lda        leading dimension of arrays psi, spsi
  !     n          true dimension of psi, spsi
  !     m          number of states psi
  !     ik         index of the kpt of psi
  !     aug_qq     the augmentation integrals
  !     psi
  ! output:
  !     spsi  S_aug*psi
  !
  !  where    S_aug = I + \sum_{ijI} aug_qq_{ij}^I   | beta_i > < beta_j |
  !
  !
  USE kinds,      ONLY : dbl
  USE constants,  ONLY : ZERO, CZERO
  USE us_module,  ONLY : okvan
  USE uspp,       ONLY : vkb, nkb
  USE uspp_param, ONLY : nh, nhm, tvanp
  USE ions_module,ONLY : nat, ntyp => nsp, ityp
  USE timing_module

  IMPLICIT NONE
  !
  ! ... First the dummy variables
  !
  INTEGER           :: lda, n, m, ik
  COMPLEX(KIND=dbl) :: psi(lda,m), spsi(lda,m)
  COMPLEX(KIND=dbl) :: aug_qq(nhm,nhm,ntyp)
  !
  CALL timing( 'augment_psi', OPR='start' )  
  !
  CALL augment_psi_k()
  !
  CALL timing( 'augment_psi',OPR='stop' )
  !
  RETURN
  !
  CONTAINS
     !
     !-----------------------------------------------------------------------
     SUBROUTINE augment_psi_k()
       !-----------------------------------------------------------------------
       !
       ! ... k-points version
       !
       USE becmod,  ONLY : becp
       !
       IMPLICIT NONE
       !
       ! ... local variables
       !
       INTEGER :: ikb, jkb, ih, jh, na, nt, ijkb0, ibnd
       ! counters
       COMPLEX(KIND=dbl), ALLOCATABLE :: ps(:,:)
       ! the product vkb and psi
       !
       !
       ! ... initialize  spsi
       !
       CALL ZCOPY( lda * m, psi, 1, spsi, 1 )
       !
       ! ... The product with the beta functions
       !
       IF ( nkb == 0 .OR. .NOT. okvan ) RETURN
       !
       ALLOCATE( ps( nkb, m ) )    
       !
       ps(:,:) = CZERO
       !
       ijkb0 = 0
       DO nt = 1, ntyp
          IF ( tvanp(nt) ) THEN
             DO na = 1, nat
                IF ( ityp(na) == nt ) THEN
                   DO ibnd = 1, m
                      DO jh = 1, nh(nt)
                         jkb = ijkb0 + jh
                         DO ih = 1, nh(nt)
                            ikb = ijkb0 + ih
                            ps(ikb,ibnd) = ps(ikb,ibnd) + &
                                           aug_qq(ih,jh,nt) * becp(jkb,ibnd,ik)
                         END DO
                      END DO
                   END DO
                   ijkb0 = ijkb0 + nh(nt)
                END IF
             END DO
          ELSE
             DO na = 1, nat
                IF ( ityp(na) == nt ) ijkb0 = ijkb0 + nh(nt)
             END DO
          END IF
       END DO
       !
       CALL ZGEMM( 'N', 'N', n, m, nkb, (1.D0, 0.D0), vkb, &
                   lda, ps, nkb, (1.D0, 0.D0), spsi, lda )
       !
       DEALLOCATE( ps )
       !
       RETURN
       !
     END SUBROUTINE augment_psi_k     
     !
END subroutine augment_psi

