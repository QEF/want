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
SUBROUTINE s_psi( lda, n, m, ik, psi, spsi )
  !----------------------------------------------------------------------------
  !
  !    This routine applies the S matrix to m wavefunctions psi
  !    and puts the results in spsi.
  !    Requires the products of psi with all beta functions
  !    in array becp(nkb,m,ik) (calculated in h_psi or by ccalbec)
  ! input:
  !     lda   leading dimension of arrays psi, spsi
  !     n     true dimension of psi, spsi
  !     m     number of states psi
  !     ik    idnex of the current kpt
  !     psi
  ! output:
  !     spsi  S*psi
  !
  USE kinds,      ONLY : dbl
  USE constants,  ONLY : ZERO, CZERO
  USE us_module,  ONLY : okvan
  USE uspp,       ONLY : nkb, vkb, vkb_ik, qq
  USE becmod,     ONLY : becp
  USE uspp_param, ONLY : nh, tvanp
  USE ions_module,ONLY : nat, ntyp => nsp, ityp
  USE timing_module
  IMPLICIT NONE

  !
  ! ... First the dummy variables
  !
  INTEGER,           INTENT(in) :: lda, n, m, ik
  COMPLEX(KIND=dbl), INTENT(in) :: psi(lda,m)
  COMPLEX(KIND=dbl), INTENT(out):: spsi(lda,m)
  !
  CALL timing( 's_psi', OPR='start' )  
     !
     CALL s_psi_k()
     !
  !
  CALL timing( 's_psi',OPR='stop' )
  !
  RETURN
  !
  CONTAINS
     !
     !-----------------------------------------------------------------------
     SUBROUTINE s_psi_k()
       !-----------------------------------------------------------------------
       !
       ! ... k-points version
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
       ! NOTE: vkb should have been calculated for the right k 
       !       we check this
       IF ( ik /= vkb_ik ) CALL errore('s_psi','Invalid ik', ik)
       !
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
                                           qq(ih,jh,nt) * becp(jkb,ibnd,ik)
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
     END SUBROUTINE s_psi_k     
     !
END subroutine s_psi
