!
! Copyright (C) 2005 Andrea Ferretti
! This file is distributed under the terms of the
! GNU General Public License. See the file `License'
! in the root directory of the present distribution,
! or http://www.gnu.org/copyleft/gpl.txt .
!
!***************************************************************
SUBROUTINE check_us( dimwinx, dimw, becp, Mkb )
   !***************************************************************
   USE kinds,      ONLY : dbl
   USE constants,  ONLY : ZERO, CZERO
   USE us_module,  ONLY : okvan
   USE uspp,       ONLY : nkb, qq
   USE uspp_param, ONLY : nh, nhm, tvanp
   USE ions_module,ONLY : nat, ntyp => nsp, ityp
   USE timing_module
   IMPLICIT NONE
   !

   INTEGER,           INTENT(in)    :: dimwinx, dimw
   COMPLEX(KIND=dbl), INTENT(in)    :: becp(nkb,dimw)
   COMPLEX(KIND=dbl), INTENT(inout) :: Mkb(dimwinx,dimwinx)
   !
   ! ... local variables
   !
   INTEGER :: ikb, jkb, ih, jh, na, nt, ijkb0, ibnd1, ibnd2

   IF ( nkb == 0 .OR. .NOT. okvan ) RETURN
   CALL timing( 'check_us', OPR='start' )  
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

                     DO ibnd2 = 1, dimw
                     DO ibnd1 = 1, dimw
                         Mkb(ibnd1,ibnd2) = Mkb(ibnd1,ibnd2) +  &
                                 CONJG( becp(ikb,ibnd1) ) * qq(ih,jh,nt) * &
                                        becp(jkb,ibnd2)
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
   CALL timing( 'check_us',OPR='stop' )
   !
   RETURN
   !
END subroutine check_us

