!
!      Copyright (C) 2005 WanT Group
!
!      This file is distributed under the terms of the
!      GNU General Public License. See the file `License'
!      in the root directory of the present distribution,
!      or http://www.gnu.org/copyleft/gpl.txt .
!
!***********************************************
   SUBROUTINE transmittance( conduct, dimC, gamma_L, gamma_R, G_ret, opr00, &
                             formula, do_eigenchannels)
   !***********************************************
   !
   ! Calculates the matrix involved in the quantum transmittance, 
   ! returned in CONDUCT variable, following
   ! LANDAUER formula in the MEAN FIELD case
   ! otherwise uses another formula derived in the
   ! paper PRL 94, 
   !
   USE kinds,                   ONLY : dbl
   USE constants,               ONLY : CZERO, CONE, CI, ZERO , EPS_m5, EPS_m6
   USE util_module,             ONLY : mat_mul, mat_sv, mat_hdiag
   USE timing_module,           ONLY : timing
   USE log_module,              ONLY : log_push, log_pop
   USE T_operator_blc_module
   !
   IMPLICIT NONE

   !
   ! input/output variables
   !
   INTEGER,             INTENT(IN) ::  dimC
   COMPLEX(dbl),        INTENT(IN) ::  gamma_L(dimC,dimC), gamma_R(dimC,dimC)
   COMPLEX(dbl),        INTENT(IN) ::  G_ret(dimC,dimC)
   TYPE(operator_blc),  INTENT(IN) ::  opr00
   CHARACTER(*),        INTENT(IN) ::  formula
   LOGICAL,             INTENT(IN) ::  do_eigenchannels
   REAL(dbl),           INTENT(OUT)::  conduct(dimC)

   !
   ! local variables
   !
   CHARACTER(13)             :: subname='transmittance'
   COMPLEX(dbl), ALLOCATABLE :: work(:,:), work1(:,:), work2(:,:)
   COMPLEX(dbl), ALLOCATABLE :: z(:,:), lambda(:,:)
   REAL(dbl),    ALLOCATABLE :: w(:)
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

   ALLOCATE( work(dimC,dimC), work1(dimC,dimC), work2(dimC,dimC), STAT=ierr )
   IF (ierr/=0) CALL errore(subname,'allocating work, work1, work2',ABS(ierr))
   !
   ALLOCATE( lambda(dimC,dimC), STAT=ierr )
   IF (ierr/=0) CALL errore(subname,'allocating lambda',ABS(ierr))

!
! if FORMULA = "generalized"
! calculates the correction term (lambda)
!
! lambda = (gamma_R + gamma_L +2*eta)^{-1} * ( g_corr + gamma_R + gamma_L + 2*eta )
!         = I + (gamma_R + gamma_L +2*eta)^{-1} * ( g_corr )
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
               work(i,j) =  gamma_L(i,j) + gamma_R(i,j) 
           ENDDO
           !
           work(j,j) = work(j,j) + 2*EPS_m5
           !
       ENDDO
       !
       CALL mat_sv(dimC, dimC, work, lambda)
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
   ! WORK  = gamma_L * G_ret
   !
   CALL mat_mul(work, gamma_L, 'N', G_ret, 'N', dimC, dimC, dimC)

   !
   ! WORK2 = G_adv * gamma_L * G_ret
   ! this array will be stored to be used to compute eigenchannels
   !
   CALL mat_mul(work2, G_ret, 'C', work, 'N', dimC, dimC, dimC)

   !
   ! WORK  = G_adv * gamma_L * G_ret * gamma_R
   !
   CALL mat_mul(work, work2, 'N', gamma_R, 'N', dimC, dimC, dimC)

   !
   ! WORK1 = G_adv * gamma_L * G_ret * gamma_R * Lambda
   ! This is calculated only if needed
   !
   IF ( TRIM(formula) ==  'generalized' ) THEN
       !
       CALL mat_mul(work1, work, 'N', lambda, 'N', dimC, dimC, dimC)
       !
   ELSE
       !
       work1 = work
       !
   ENDIF

!   !
!   ! WORK1 = gamma_L * G_ret * gammma_R
!   !
!   CALL mat_mul(work1, work, 'N', gamma_R, 'N', dimC, dimC, dimC)
!
!   !
!   ! WORK  = gamma_L * G_ret * gamma_R * lambda
!   !
!   CALL mat_mul(work, work1, 'N', lambda, 'N', dimC, dimC, dimC)
!
!   !
!   ! WORK1 = gamma_L * G_ret * gamma_R * lambda * G_adv
!   !
!   CALL mat_mul(work1, work, 'N', G_ret, 'C', dimC, dimC, dimC)
       
      
   !
   ! If we are not interested in the eigenchannels analysis
   ! we just take the diagonal matrix elements, otherwise
   ! we diagonalize and take the eigenvalues.
   !
   IF ( do_eigenchannels ) THEN
       !
       ! Here we compute and diagonalize the product
       !
       ! work1 = gamma_L^1/2 * G_ret * gamma_R * G_adv * gamma_L^1/2
       !       = gamma_L^1/2 * work2 * gamma_L^1/2
       !
       ! which is a hermitean matrix.
       !
       ! To do this, we need to compute gamma_L^1/2  ( stored in work )
       !
       ALLOCATE( z(dimC,dimC), w(dimC), STAT=ierr )
       IF (ierr/=0) CALL errore(subname,'allocating z, w',ABS(ierr))

       CALL mat_hdiag( z, w, gamma_L, dimC ) 
       !
       DO i = 1, dimC
           !
           IF ( w(i) < -EPS_m6 ) &
                 CALL errore(subname,'gamma_L not positive defined', 10)
           !
           ! clean any numerical noise leading to eigenvalues  -eps_m6 < w < 0.0
           IF ( w(i) < ZERO ) w(i) = ZERO
           !
           w(i) = SQRT( w(i) )
           !
       ENDDO
       !
       !
       DO j = 1, dimC
       DO i = 1, dimC
           !
           work1(i,j) = z(i,j) * w(j)
           !
       ENDDO
       ENDDO
       !
       CALL mat_mul( work, work1, 'N', z, 'C', dimC, dimC, dimC)
       
       !
       ! get the hermiteanized T matrix
       !
       CALL mat_mul( work1, work,  'N', work2, 'N', dimC, dimC, dimC)
       CALL mat_mul( work2, work1, 'N', work,  'N', dimC, dimC, dimC)
       
       !
       ! get the eigenvalues
       ! we invert the sign of work2 in such a way to have an increasing
       ! ordering of eigenvalues
       !
       work2 = -work2
       !
       CALL mat_hdiag( z, conduct, work2, dimC ) 
       !
       conduct = -conduct
       
       DEALLOCATE( z, w, STAT=ierr )
       IF (ierr/=0) CALL errore(subname,'deallocating z, w',ABS(ierr))
       !
   ELSE
       !
       DO i=1,dimC
           conduct(i) = REAL( work1(i,i), dbl )
       ENDDO
       !
   ENDIF

   !
   ! local memopry clean
   !
   DEALLOCATE( work, work1, work2, STAT=ierr )
   IF (ierr/=0) CALL errore(subname,'deallocating work, work1, work2',ABS(ierr))
   !
   DEALLOCATE( lambda, STAT=ierr )
   IF (ierr/=0) CALL errore(subname,'deallocating lambda',ABS(ierr))
      
   CALL timing(subname, OPR='stop')
   CALL log_pop(subname)
   !
   RETURN
   !
END SUBROUTINE transmittance

