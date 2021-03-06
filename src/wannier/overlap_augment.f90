!
! Copyright (C) 2005 WanT Group
!
! This file is distributed under the terms of the
! GNU General Public License. See the file `License'
! in the root directory of the present distribution,
! or http://www.gnu.org/copyleft/gpl.txt .
!
!***************************************************************
SUBROUTINE overlap_augment( dimx, dimw1, dimw2, ib, Mkb )
   !***************************************************************
   !
   !    This routine add the augmentation part due to USPP to the
   !    overlap matrix.
   !    Requires the products of psi with all beta functions
   !    in array becp(nkb,m,ik) 
   ! input:
   !     dimx              leading dimension of arrays Mkb(:,:)
   !     dimw1,
   !     dimw2             true dimensions of the array Mkb
   !     ib                index of the nearest neighbour b
   ! input / output:
   !     Mkb               the overlap matrix
   !
   ! The task performed is:
   !  Mkb(m,n) = Mkb(m,n) + \sum_{ijI} qb_{ij}^I * e^i(b*tau_I)
   !                    <psi_m,k1| beta_i,k1 > < beta_j,k2 | psi_n,k2 > 
   !
   !
   USE kinds,           ONLY : dbl
   USE constants,       ONLY : ZERO, CZERO
   USE kpoints_module,  ONLY : vb
   USE lattice_module,  ONLY : alat
   USE us_module,       ONLY : okvan
   USE uspp,            ONLY : nkb, qb
   USE uspp_param,      ONLY : upf, nh
   USE ions_module,     ONLY : nat, nsp, ityp, tau
   USE becmod,          ONLY : becp
   USE log_module,      ONLY : log_push, log_pop
   USE timing_module
   !
   IMPLICIT NONE
   !

   INTEGER,           INTENT(in)    :: dimx
   INTEGER,           INTENT(in)    :: dimw1, dimw2
   INTEGER,           INTENT(in)    :: ib
   COMPLEX(KIND=dbl), INTENT(inout) :: Mkb(dimx,dimx)
   !
   ! ... local variables
   !
   INTEGER      :: ikb, jkb, ih, jh, na, nt, ijkb0, ibnd1, ibnd2
   REAL(dbl)    :: arg
   COMPLEX(dbl) :: phase

!
!--------------------------
! routine body
!--------------------------
!

   Mkb = CZERO

   IF ( nkb == 0 .OR. .NOT. okvan ) RETURN
   CALL timing( 'overlap_augment', OPR='start' )  
   CALL log_push( 'overlap_augment' )  
   !
   !
   ijkb0 = 0
   DO nt = 1, nsp
      IF ( upf(nt)%tvanp ) THEN
         DO na = 1, nat
            !
            ! compute the phase factor e^i(b*tau)
            ! vb in bohr^-1, tau in alat and directly converted
            !
            arg = DOT_PRODUCT( vb(:,ib), tau(:,na) ) * alat
            phase = CMPLX ( COS(arg), -SIN(arg), dbl )
            !
            IF ( ityp(na) == nt ) THEN
               DO jh = 1, nh(nt)
                  jkb = ijkb0 + jh
                  DO ih = 1, nh(nt)
                     ikb = ijkb0 + ih

                     DO ibnd2 = 1, dimw2
                     DO ibnd1 = 1, dimw1
                         !
                         Mkb(ibnd1,ibnd2) = Mkb(ibnd1,ibnd2) +       &
                                 CONJG( becp(ikb,ibnd1,1) )        * &
                                        becp(jkb,ibnd2,2)          * &
                                 phase * qb(ih,jh,nt,ib) 
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
   CALL timing( 'overlap_augment',OPR='stop' )
   CALL log_pop( 'overlap_augment' )
   !
   RETURN
   !
END subroutine overlap_augment

