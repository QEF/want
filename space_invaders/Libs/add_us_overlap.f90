!
! Copyright (C) 2005 Andrea Ferretti
! This file is distributed under the terms of the
! GNU General Public License. See the file `License'
! in the root directory of the present distribution,
! or http://www.gnu.org/copyleft/gpl.txt .
!
!***************************************************************
SUBROUTINE add_us_overlap( dimwinx, dimw1, dimw2, ish, becp1, becp2, Mkb )
   !***************************************************************
   !    This routine add the augmentation part due to USPP to the
   !    overlap matrix.
   !    Requires the products of psi with all beta functions
   !    in array becp(nkb,m,ik) 
   ! input:
   !     dimwinx    leading dimension of arrays Mkb(:,:)
   !     dimw1,
   !     dimw2      true dimensions of the array Mkb
   !     ish        index of the b-kpt nn vector
   !     becp1      < beta | psi_ik1 > projections
   !     becp2      < beta | psi_ik2 > projections
   ! input / output:
   !     Mkb        the overlap matrix
   !
   ! The task performed is:
   !  Mkb(m,n) = Mkb(m,n) + \sum_{ijI} qb_{ij}^I   *
   !                    <psi_m,k1| beta_i,k1 > < beta_j,k2 | psi_n,k2 > 
   !
   !
   USE kinds,      ONLY : dbl
   USE constants,  ONLY : ZERO, CZERO
   USE us_module,  ONLY : okvan
   USE uspp,       ONLY : nkb, qb
   USE uspp_param, ONLY : nh, nhm, tvanp
   USE ions_module,ONLY : nat, ntyp => nsp, ityp
   USE timing_module
   IMPLICIT NONE
   !

   INTEGER,           INTENT(in)    :: dimwinx, dimw1, dimw2, ish 
   COMPLEX(KIND=dbl), INTENT(in)    :: becp1(nkb,dimw1), becp2(nkb,dimw2)
   COMPLEX(KIND=dbl), INTENT(inout) :: Mkb(dimwinx,dimwinx)
   !
   ! ... local variables
   !
   INTEGER :: ikb, jkb, ih, jh, na, nt, ijkb0, ibnd1, ibnd2

   IF ( nkb == 0 .OR. .NOT. okvan ) RETURN
   CALL timing( 'add_us_overlap', OPR='start' )  
   !
   !
   ijkb0 = 0
   DO nt = 1, ntyp
      IF ( tvanp(nt) ) THEN
         DO na = 1, nat
            IF ( ityp(na) == nt ) THEN
               DO jh = 1, nh(nt)
                  jkb = ijkb0 + jh
                  DO ih = 1, nh(nt)
                     ikb = ijkb0 + ih

                     DO ibnd2 = 1, dimw2
                     DO ibnd1 = 1, dimw1
                         Mkb(ibnd1,ibnd2) = Mkb(ibnd1,ibnd2) +  &
                                 CONJG( becp1(ikb,ibnd1) ) * qb(ih,jh,nt,ish) * &
                                        becp2(jkb,ibnd2)
                     ENDDO
                     ENDDO
                  ENDDO
               ENDDO
               ijkb0 = ijkb0 + nh(nt)
            ENDIF
         ENDDO
      ELSE
         DO na = 1, nat
            IF ( ityp(na) == nt ) ijkb0 = ijkb0 + nh(nt)
         END DO
      ENDIF
   ENDDO
   !
   CALL timing( 'add_us_overlap',OPR='stop' )
   !
   RETURN
   !
END subroutine add_us_overlap

