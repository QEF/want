!
!      Copyright (C) 2005 WanT Group
!
!      This file is distributed under the terms of the
!      GNU General Public License. See the file `License'
!      in the root directory of the present distribution,
!      or http://www.gnu.org/copyleft/gpl.txt .
!
!***********************************************
   SUBROUTINE transmittance(dimC, gL, gR, gintr, opr00, formula, conduct)
   !***********************************************
   !
   ! Calculates the matrix involved in the quantum transmittance, 
   ! returned in CONDUCT variable, following
   ! LANDAUER formula in the MEAN FILED case
   ! otherwise uses another formula derived in the
   ! paper PRL 94, 
   !
   USE kinds,          ONLY : dbl
   USE constants,      ONLY : CZERO, CONE, CI, ZERO , EPS_m5
   USE util_module,    ONLY : mat_mul, mat_sv
   USE timing_module,  ONLY : timing
   USE log_module,     ONLY : log_push, log_pop
   USE T_operator_blc_module
   !
   IMPLICIT NONE

   !
   ! input/output variables
   !
   INTEGER,             INTENT(IN) ::  dimC
   COMPLEX(dbl),        INTENT(IN) ::  gL(dimC,dimC), gR(dimC,dimC)
   COMPLEX(dbl),        INTENT(IN) ::  gintr(dimC,dimC)
   TYPE(operator_blc),  INTENT(IN) ::  opr00
   CHARACTER(*),        INTENT(IN) ::  formula
   REAL(dbl),           INTENT(OUT)::  conduct(dimC)

   !
   ! local variables
   !
   CHARACTER(13)             :: subname='transmittance'
   COMPLEX(dbl), ALLOCATABLE :: tmp(:,:), tmp1(:,:)
   COMPLEX(dbl), ALLOCATABLE :: lambda(:,:)
   INTEGER :: i, j, ik, ierr
   !
   ! end of declarations
   !

!
!------------------------------
! main body
!------------------------------
!
   CALL timing(subname, OPR='start')
   CALL log_push(subname)

   ALLOCATE( tmp(dimC,dimC), tmp1(dimC,dimC), STAT=ierr )
   IF (ierr/=0) CALL errore(subname,'allocating tmp,tm1',ABS(ierr))
   !
   ALLOCATE( lambda(dimC,dimC), STAT=ierr )
   IF (ierr/=0) CALL errore(subname,'allocating lambda',ABS(ierr))

!
! if FORMULA = "generalized"
! calculates the correction term (lambda)
!
! lambda = (gR + gL +2*eta)^{-1} * ( g_corr + gR + gL + 2*eta )
!         = I + (gR + gL +2*eta)^{-1} * ( g_corr )
!
! where g_corr = i (sgm_corr - sgm_corr^\dag)
! 

   IF ( TRIM(formula) == "generalized" )  THEN
       !
       ik = opr00%ik
       !
       DO j=1,dimC
           !
           DO i=1,dimC
               lambda(i,j) =  CI * ( opr00%sgm(i,j,ik) - CONJG(opr00%sgm(j,i,ik))  )
               tmp(i,j) =  gL(i,j) + gR(i,j) 
           ENDDO
           !
           tmp(j,j) = tmp(j,j) + 2*EPS_m5
           !
       ENDDO
       !
       CALL mat_sv(dimC, dimC, tmp, lambda)
       !
   ELSE
       !
       ! ordinary formula
       !
       lambda(:,:) = CZERO
       !
   ENDIF

   ! 
   ! adding the identity matrix
   ! 
   DO i=1,dimC
       !
       lambda(i,i) = lambda(i,i) + CONE
       !
   ENDDO


!
! calculates the matrix product 
! whose trace (CONDUCT) is the main term of the transmittance 
! units of 2e^2/h 
! 

   !
   ! gL * gintr -> tmp
   !
   CALL mat_mul(tmp, gL, 'N', gintr, 'N', dimC, dimC, dimC)
   !
   ! gL * gintr * gR -> tmp1
   !
   CALL mat_mul(tmp1, tmp, 'N', gR, 'N', dimC, dimC, dimC)
   !
   ! gL * gintr * gR * lambda -> tmp
   !
   CALL mat_mul(tmp, tmp1, 'N', lambda, 'N', dimC, dimC, dimC)
   !
   ! gL * gintr * gR * lambda * ginta -> tmp1
   !
   CALL mat_mul(tmp1, tmp, 'N', gintr, 'C', dimC, dimC, dimC)
       
      
   DO i=1,dimC
      conduct(i) = REAL( tmp1(i,i), dbl )
   ENDDO

   !
   ! local memopry clean
   !
   DEALLOCATE( tmp, tmp1, STAT=ierr )
   IF (ierr/=0) CALL errore(subname,'deallocating tmp,tm1',ABS(ierr))
   DEALLOCATE( lambda, STAT=ierr )
   IF (ierr/=0) CALL errore(subname,'deallocating lambda',ABS(ierr))
      
   CALL timing(subname, OPR='stop')
   CALL log_pop(subname)
   !
   RETURN
   !
END SUBROUTINE transmittance

