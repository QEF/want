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
   ! For each block (tot T, emb E, bath B, emb bath EB) define
   ! the aux quantity:
   !
   !  aux = E*ovp -ham -sgm_corr
   !
   ! for a given kpt
   !
   !
   USE kinds,                ONLY : dbl
   USE constants,            ONLY : CZERO
   USE timing_module,        ONLY : timing
   USE log_module,           ONLY : log_push, log_pop
   !
   USE E_hamiltonian_module, ONLY : shift_T, blc_T, blc_E, blc_B, blc_EB, blc_BE
   !
   USE T_egrid_module,       ONLY : egrid
   USE T_smearing_module,    ONLY : smearing_type
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
   INTEGER       :: dimT, ierr
   COMPLEX(dbl), ALLOCATABLE :: work(:,:)

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
   blc_BE%aux(:,:) =  (omg -shift_T) * blc_BE%S(:,:,ik) -blc_BE%H(:,:,ik)

   !
   ! if smearing is different from lorentzian, add a smearing self-energy
   !
   IF ( TRIM(smearing_type) /= "lorentzian" ) THEN
       !
       dimT=blc_T%dim1
       !
       ALLOCATE( work(dimT,dimT), STAT=ierr )
       IF ( ierr/=0 ) CALL errore(subname,'allocating dimT',ABS(ierr))

       !
       ! build  omg -H -Sigma_smear, for the T region
       ! no contribution to smearing sigma comes from en eventual correlation sigma
       !
       CALL gzero_maker ( dimT, blc_T, blc_T%dim1, work, 'inverse', smearing_type )

       !
       ! update blc_*%sgm_aux
       !
       blc_T%sgm_aux = -( work -blc_T%aux )
       !
       blc_E%sgm_aux = CZERO
       blc_B%sgm_aux = CZERO
       blc_EB%sgm_aux = CZERO
       blc_BE%sgm_aux = CZERO
       !
       CALL ham_matrix_mask( blc_E,  blc_E%sgm_aux,  blc_T, blc_T%sgm_aux)
       CALL ham_matrix_mask( blc_B,  blc_B%sgm_aux,  blc_T, blc_T%sgm_aux)
       CALL ham_matrix_mask( blc_EB, blc_EB%sgm_aux, blc_T, blc_T%sgm_aux)
       CALL ham_matrix_mask( blc_BE, blc_BE%sgm_aux, blc_T, blc_T%sgm_aux)
       !
       DEALLOCATE( work, STAT=ierr )
       IF ( ierr/=0 ) CALL errore(subname,'deallocating dimT',ABS(ierr))
       !
   ENDIF
   
   !
   ! correlation
   !
   IF ( ASSOCIATED( blc_T%sgm ) ) THEN
       blc_T%aux(:,:) = blc_T%aux(:,:) -blc_T%sgm(:,:,ik)
   ENDIF
   IF ( ASSOCIATED( blc_T%sgm_aux ) ) THEN
       blc_T%aux(:,:) = blc_T%aux(:,:) -blc_T%sgm_aux(:,:)
   ENDIF
   !
   IF ( ASSOCIATED( blc_E%sgm ) ) THEN
       blc_E%aux(:,:) = blc_E%aux(:,:) -blc_E%sgm(:,:,ik)
   ENDIF
   IF ( ASSOCIATED( blc_E%sgm_aux ) ) THEN
       blc_E%aux(:,:) = blc_E%aux(:,:) -blc_E%sgm_aux(:,:)
   ENDIF
   !
   IF ( ASSOCIATED( blc_B%sgm ) ) THEN
       blc_B%aux(:,:) = blc_B%aux(:,:) -blc_B%sgm(:,:,ik)
   ENDIF
   IF ( ASSOCIATED( blc_B%sgm_aux ) ) THEN
       blc_B%aux(:,:) = blc_B%aux(:,:) -blc_B%sgm_aux(:,:)
   ENDIF
   !
   IF ( ASSOCIATED( blc_EB%sgm ) ) THEN
       blc_EB%aux(:,:) = blc_EB%aux(:,:) -blc_EB%sgm(:,:,ik)
   ENDIF
   IF ( ASSOCIATED( blc_EB%sgm_aux ) ) THEN
       blc_EB%aux(:,:) = blc_EB%aux(:,:) -blc_EB%sgm_aux(:,:)
   ENDIF
   !
   IF ( ASSOCIATED( blc_BE%sgm ) ) THEN
       blc_BE%aux(:,:) = blc_BE%aux(:,:) -blc_BE%sgm(:,:,ik)
   ENDIF
   IF ( ASSOCIATED( blc_BE%sgm_aux ) ) THEN
       blc_BE%aux(:,:) = blc_BE%aux(:,:) -blc_BE%sgm_aux(:,:)
   ENDIF

   !
   ! finalize setup
   !
   CALL operator_blc_update( IE=ie, IK=ik, OBJ=blc_T  )
   CALL operator_blc_update( IE=ie, IK=ik, OBJ=blc_E  )
   CALL operator_blc_update( IE=ie, IK=ik, OBJ=blc_B  )
   CALL operator_blc_update( IE=ie, IK=ik, OBJ=blc_EB )
   CALL operator_blc_update( IE=ie, IK=ik, OBJ=blc_BE )
   
   CALL timing( subname, OPR='STOP' )
   CALL log_pop( subname )
   !
   RETURN
   !
END SUBROUTINE hamiltonian_setup


!*******************************************************************
   SUBROUTINE ham_matrix_mask( blc_O, zmat_O, blc_T, zmat_T )
   !*******************************************************************
   !
   USE kinds,                ONLY : dbl
   USE timing_module,        ONLY : timing
   USE log_module,           ONLY : log_push, log_pop
   USE T_operator_blc_module
   !
   IMPLICIT NONE
   !
   TYPE(operator_blc), INTENT(IN)  :: blc_O, blc_T
   COMPLEX(dbl),       INTENT(IN)  :: zmat_T(blc_T%dim1,blc_T%dim2)
   COMPLEX(dbl),       INTENT(OUT) :: zmat_O(blc_O%dim1,blc_O%dim2)
   !
   CHARACTER(15)  :: subname='ham_matrix_mask'
   INTEGER        :: i, j, it, jt, is, js 
   INTEGER        :: dim1_O, dim2_O, dim1_T, dim2_T  
   INTEGER        :: imap( blc_O%dim1 )
   INTEGER        :: jmap( blc_O%dim2 )

!
!----------------------------------------
! main Body
!----------------------------------------
!

   CALL timing( subname, OPR='start')
   CALL log_push( subname )
   !
   dim1_O = blc_O%dim1
   dim2_O = blc_O%dim2
   dim1_T = blc_T%dim1
   dim2_T = blc_T%dim2
   !
   imap(:) = 0
   jmap(:) = 0
   !
   DO i = 1, dim1_O
       !
       DO is = 1, dim1_T
           IF ( blc_O%irows(i) == blc_T%irows(is) ) THEN
               imap(i) = is
               EXIT
           ENDIF
       ENDDO
       !
   ENDDO
   !
   DO j = 1, dim2_O
       !
       DO js = 1, dim2_T
           IF ( blc_O%icols(j) == blc_T%icols(js) ) THEN
               jmap(j) = js
               EXIT
           ENDIF
       ENDDO
       !
   ENDDO
   !
   IF ( ANY( imap(:) == 0 ) ) CALL errore(subname,'invalid imap',10)
   IF ( ANY( jmap(:) == 0 ) ) CALL errore(subname,'invalid jmap',10)

   !
   ! copy array
   !
   DO j = 1, dim2_O
   DO i = 1, dim1_O
       !
       zmat_O( i, j ) = zmat_T( imap(i), jmap(j) )
       !
   ENDDO
   ENDDO
   !
   CALL timing( subname, OPR='STOP' )
   CALL log_pop( subname )
   !
   RETURN
   !
END SUBROUTINE ham_matrix_mask


