!
! Copyright (C) 2001-2003 PWSCF group
! This file is distributed under the terms of the
! GNU General Public License. See the file `License'
! in the root directory of the present distribution,
! or http://www.gnu.org/copyleft/gpl.txt .
!
#include "machine.h"
!
!----------------------------------------------------------------------------
SUBROUTINE sb_psi( lda, n, m, ik, ish, psi, spsi )
  !----------------------------------------------------------------------------
  !
  !    This routine applies the Sb matrix to m wavefunctions psi
  !    and puts the results in spsi.
  !    Requires the products of psi with all beta functions
  !    in array becp(nkb,m,ik) (calculated in h_psi or by ccalbec) and the
  !    qb quantities.
  !
  !    The operator Sb is 
  !     Sb = I + \sum_{ij} q_ij(b) | \beta_i > < \beta_j |
  !
  !    the quantities q_ij(b) are stored in the qb array
  !
  ! input:
  !     lda   leading dimension of arrays psi, spsi
  !     n     true dimension of psi, spsi
  !     m     number of states psi
  !     ik    index of the kpt of psi
  !     ish   index of shell corresponding to the modulus of b
  !     psi
  ! output:
  !     spsi  Sb*psi
  !
  USE kinds,      ONLY : dbl
  USE constants,  ONLY : ZERO, CZERO
  USE us_module,  ONLY : okvan
  USE uspp,       ONLY : vkb, nkb, qb
  USE uspp_param, ONLY : nh, tvanp
  USE ions_module,ONLY : nat, ntyp => nsp, ityp
  USE kpoints_module, ONLY : 
  USE timing_module
  !
  IMPLICIT NONE
  !
  ! ... First the dummy variables
  !
  INTEGER          :: lda, n, m, ik, ish
  COMPLEX(KIND=dbl) :: psi(lda,m), spsi(lda,m)
  !
  CALL timing( 'sb_psi', OPR='start' )  
!
! Avoid here the GAMMA_ONLY case
!  IF ( gamma_only ) THEN
!     !
!     CALL s_psi_gamma()
!     !
!  ELSE
     !
     CALL sb_psi_k()
     !
!  END IF    
  !
  CALL timing( 'sb_psi',OPR='stop' )
  !
  RETURN
  !
  CONTAINS
     !
     !-----------------------------------------------------------------------
     SUBROUTINE sb_psi_k()
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
                                           qb(ih,jh,nt,ish) * becp(jkb,ibnd,ik)
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
     END SUBROUTINE sb_psi_k     
     !
END subroutine sb_psi
