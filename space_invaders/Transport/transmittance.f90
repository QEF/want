!
!      Copyright (C) 2005 WanT Group
!
!      This file is distributed under the terms of the
!      GNU General Public License. See the file `License'
!      in the root directory of the present distribution,
!      or http://www.gnu.org/copyleft/gpl.txt .
!
!***********************************************
   SUBROUTINE transmittance(nmaxc, gL, gR, gintr, sgm_r, formula, conduct)
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
   IMPLICIT NONE

   !
   ! input/output variables
   !
   INTEGER,      INTENT(in) ::  nmaxc
   COMPLEX(dbl), INTENT(in) ::  gL(nmaxc,nmaxc), gR(nmaxc,nmaxc)
   COMPLEX(dbl), INTENT(in) ::  gintr(nmaxc,nmaxc)
   COMPLEX(dbl), INTENT(in) ::  sgm_r(nmaxc,nmaxc)
   CHARACTER(*), INTENT(in) ::  formula
   REAL(dbl),    INTENT(out)::  conduct(nmaxc)

   !
   ! local variables
   !
   INTEGER,      ALLOCATABLE :: ipiv(:)
   COMPLEX(dbl), ALLOCATABLE :: tmp(:,:), tmp1(:,:)
   COMPLEX(dbl), ALLOCATABLE :: lambda(:,:)
   INTEGER :: i, j, ierr, info
   !
   ! end of declarations
   !

!
!------------------------------
! main body
!------------------------------
!
   CALL timing('transmittance', OPR='start')

   ALLOCATE( tmp(nmaxc,nmaxc), tmp1(nmaxc,nmaxc), STAT=ierr )
      IF (ierr/=0) CALL errore('transmittance','allocating tmp,tm1',ABS(ierr))
   ALLOCATE( lambda(nmaxc,nmaxc), STAT=ierr )
      IF (ierr/=0) CALL errore('transmittance','allocating lambda',ABS(ierr))
   ALLOCATE( ipiv(nmaxc), STAT=ierr )
      IF (ierr/=0) CALL errore('transmittance','allocating ipiv',ABS(ierr))

!
! if FORMULA = "generalized"
! calculates the correction term (lambda)
!
! lambda = (gR + gL +2*eta)^{-1} * ( g_corr + gR + gL + 2*eta )
!         = I + (gR + gL +2*eta)^{-1} * ( g_corr )
!
! where g_corr = i (sgm_r - sgm_r^\dag)
! 

   IF ( TRIM(formula) == "generalized" )  THEN
       DO j=1,nmaxc
           DO i=1,nmaxc
               lambda(i,j) =  CI * ( sgm_r(i,j) - CONJG(sgm_r(j,i))  )
               tmp(i,j) =  gL(i,j) + gR(i,j) 
           ENDDO
           tmp(j,j) = tmp(j,j) + 2*EPS_m5
       ENDDO

       CALL mat_sv(nmaxc, nmaxc, tmp, lambda)


   ELSE
       !
       ! ordinary formula
       !
       lambda(:,:) = CZERO
   ENDIF

   ! 
   ! adding the identity matrix
   ! 
   DO i=1,nmaxc
       lambda(i,i) = lambda(i,i) + CONE
   ENDDO


!
! calculates the matrix product 
! whose trace (CONDUCT) is the main term of the transmittance 
! units of 2e^2/h 
! 

   !
   ! gL * gintr -> tmp
   !
   CALL mat_mul(tmp, gL, 'N', gintr, 'N', nmaxc, nmaxc, nmaxc)
   !
   ! gL * gintr * gR -> tmp1
   !
   CALL mat_mul(tmp1, tmp, 'N', gR, 'N', nmaxc, nmaxc, nmaxc)
   !
   ! gL * gintr * gR * lambda -> tmp
   !
   CALL mat_mul(tmp, tmp1, 'N', lambda, 'N', nmaxc, nmaxc, nmaxc)
   !
   ! gL * gintr * gR * lambda * ginta -> tmp1
   !
   CALL mat_mul(tmp1, tmp, 'N', gintr, 'C', nmaxc, nmaxc, nmaxc)
       
      
   DO i=1,nmaxc
      conduct(i) = tmp1(i,i)
   ENDDO

!
! local memopry clean
!
   DEALLOCATE( tmp, tmp1, STAT=ierr )
      IF (ierr/=0) CALL errore('transmittance','deallocating tmp,tm1',ABS(ierr))
   DEALLOCATE( lambda, STAT=ierr )
      IF (ierr/=0) CALL errore('transmittance','deallocating lambda',ABS(ierr))
   DEALLOCATE( ipiv, STAT=ierr )
      IF (ierr/=0) CALL errore('transmittance','deallocating ipiv',ABS(ierr))
      
   CALL timing('transmittance', OPR='stop')
END SUBROUTINE transmittance

