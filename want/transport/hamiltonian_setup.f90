!
!      Copyright (C) 2008 WanT Group
!
!      This file is distributed under the terms of the
!      GNU General Public License. See the file `License'
!      in the root directory of the present distribution,
!      or http://www.gnu.org/copyleft/gpl.txt .
!
!*******************************************************************
   SUBROUTINE hamiltonian_setup( ik, ie )
   !*******************************************************************
   !
   ! For each block (00C, 00L, 00R, 01L, 01R, CR, LC) define
   ! the aux quantity:
   !
   !  aux = E*ovp -ham -sgm_corr
   !
   ! for a given kpt
   !
   !
   USE kinds,                ONLY : dbl
   USE T_hamiltonian_module, ONLY : shift_L, shift_C, shift_R, shift_corr, &
                                    blc_00L, blc_01L, blc_00R, blc_01R,    &
                                    blc_00C, blc_LC,  blc_CR
   USE T_egrid_module,       ONLY : egrid
   USE T_operator_blc_module
   !
   USE timing_module,        ONLY : timing
   USE log_module,           ONLY : log_push, log_pop
   !
   IMPLICIT NONE

   !
   ! local variables
   !
   INTEGER,   INTENT(IN) :: ie, ik

   !
   ! local variables
   !
   CHARACTER(17) :: subname="hamiltonian_setup"
   REAL(dbl)     :: omg

   !
   ! end of declarations
   !

!
!----------------------------------------
! main Body
!----------------------------------------
!
   CALL timing( subname, OPR='start')
   CALL log_push( subname )

   omg = egrid(ie)

   !
   ! hamiltonian and overlap
   !
   blc_00L%aux(:,:)  =  (omg -shift_L) * blc_00L%S(:,:,ik) -blc_00L%H(:,:,ik)
   blc_01L%aux(:,:)  =  (omg -shift_L) * blc_01L%S(:,:,ik) -blc_01L%H(:,:,ik)
   !
   blc_00R%aux(:,:)  =  (omg -shift_R) * blc_00R%S(:,:,ik) -blc_00R%H(:,:,ik)
   blc_01R%aux(:,:)  =  (omg -shift_R) * blc_01R%S(:,:,ik) -blc_01R%H(:,:,ik)
   ! 
   blc_00C%aux(:,:)  =  (omg -shift_C) * blc_00C%S(:,:,ik) -blc_00C%H(:,:,ik)
   blc_LC%aux(:,:)   =  (omg -shift_C) * blc_LC%S(:,:,ik) -blc_LC%H(:,:,ik)
   blc_CR%aux(:,:)   =  (omg -shift_C) * blc_CR%S(:,:,ik) -blc_CR%H(:,:,ik)
   
   !
   ! correlation
   !
   IF ( ASSOCIATED( blc_00L%sgm ) ) THEN
       blc_00L%aux(:,:) = blc_00L%aux(:,:) -blc_00L%sgm(:,:,ik)
   ENDIF
   IF ( ASSOCIATED( blc_01L%sgm ) ) THEN
       blc_01L%aux(:,:) = blc_01L%aux(:,:) -blc_01L%sgm(:,:,ik)
   ENDIF
   !
   IF ( ASSOCIATED( blc_00R%sgm ) ) THEN
       blc_00R%aux(:,:) = blc_00R%aux(:,:) -blc_00R%sgm(:,:,ik)
   ENDIF
   IF ( ASSOCIATED( blc_01L%sgm ) ) THEN
       blc_01R%aux(:,:) = blc_01R%aux(:,:) -blc_01R%sgm(:,:,ik)
   ENDIF
   !
   IF ( ASSOCIATED( blc_00C%sgm ) ) THEN
       blc_00C%aux(:,:) = blc_00C%aux(:,:) -blc_00C%sgm(:,:,ik) &
                                           -shift_corr * blc_00C%S(:,:,ik)
   ENDIF
   IF ( ASSOCIATED( blc_LC%sgm ) ) THEN
       blc_LC%aux(:,:)  = blc_LC%aux(:,:)  -blc_LC%sgm(:,:,ik) &
                                           -shift_corr * blc_LC%S(:,:,ik)
   ENDIF
   IF ( ASSOCIATED( blc_CR%sgm ) ) THEN
       blc_CR%aux(:,:)  = blc_CR%aux(:,:)  -blc_CR%sgm(:,:,ik) &
                                           -shift_corr * blc_CR%S(:,:,ik)
   ENDIF

   !
   ! finalize setup
   !
   CALL operator_blc_update( IE=ie, IK=ik, OBJ=blc_00C )
   CALL operator_blc_update( IE=ie, IK=ik, OBJ=blc_LC  )
   CALL operator_blc_update( IE=ie, IK=ik, OBJ=blc_CR  )
   CALL operator_blc_update( IE=ie, IK=ik, OBJ=blc_00L )
   CALL operator_blc_update( IE=ie, IK=ik, OBJ=blc_01L )
   CALL operator_blc_update( IE=ie, IK=ik, OBJ=blc_00R )
   CALL operator_blc_update( IE=ie, IK=ik, OBJ=blc_01R )
   
   CALL timing( subname, OPR='STOP' )
   CALL log_pop( subname )
   !
   RETURN
   !
END SUBROUTINE hamiltonian_setup

