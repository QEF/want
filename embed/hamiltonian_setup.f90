!
!      Copyright (C) 2009 WanT Group
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
   ! For each block (tot, emb, bulk, eb) define
   ! the aux quantity:
   !
   !  aux = E*ovp -ham -sgm_corr
   !
   ! for a given kpt
   !
   !
   USE kinds,                ONLY : dbl
   USE timing_module,        ONLY : timing
   USE log_module,           ONLY : log_push, log_pop
   !
   USE E_hamiltonian_module, ONLY : shift_T, blc_T, blc_E, blc_B, blc_EB
   !
   USE T_egrid_module,       ONLY : egrid
   USE T_operator_blc_module
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
   blc_T%aux(:,:)  =  (omg -shift_T) * blc_T%S(:,:,ik)  -blc_T%H(:,:,ik)
   blc_E%aux(:,:)  =  (omg -shift_T) * blc_E%S(:,:,ik)  -blc_E%H(:,:,ik)
   blc_B%aux(:,:)  =  (omg -shift_T) * blc_B%S(:,:,ik)  -blc_B%H(:,:,ik)
   blc_EB%aux(:,:) =  (omg -shift_T) * blc_EB%S(:,:,ik) -blc_EB%H(:,:,ik)
   
   !
   ! correlation
   !
   IF ( ASSOCIATED( blc_T%sgm ) ) THEN
       blc_T%aux(:,:) = blc_T%aux(:,:) -blc_T%sgm(:,:,ik)
   ENDIF
   IF ( ASSOCIATED( blc_E%sgm ) ) THEN
       blc_E%aux(:,:) = blc_E%aux(:,:) -blc_E%sgm(:,:,ik)
   ENDIF
   IF ( ASSOCIATED( blc_B%sgm ) ) THEN
       blc_B%aux(:,:) = blc_B%aux(:,:) -blc_B%sgm(:,:,ik)
   ENDIF
   IF ( ASSOCIATED( blc_EB%sgm ) ) THEN
       blc_EB%aux(:,:) = blc_EB%aux(:,:) -blc_EB%sgm(:,:,ik)
   ENDIF

   !
   ! finalize setup
   !
   CALL operator_blc_update( IE=ie, IK=ik, OBJ=blc_T  )
   CALL operator_blc_update( IE=ie, IK=ik, OBJ=blc_E  )
   CALL operator_blc_update( IE=ie, IK=ik, OBJ=blc_B  )
   CALL operator_blc_update( IE=ie, IK=ik, OBJ=blc_EB )
   
   CALL timing( subname, OPR='STOP' )
   CALL log_pop( subname )
   !
   RETURN
   !
END SUBROUTINE hamiltonian_setup

