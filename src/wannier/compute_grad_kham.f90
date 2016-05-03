!
! Copyright (C) 2006 WanT Group
!
! This file is distributed under the terms of the
! GNU General Public License. See the file `License'
! in the root directory of the present distribution,
! or http://www.gnu.org/copyleft/gpl.txt .
!
!***************************************************
    SUBROUTINE compute_grad_kham( dimwann, nr, vr, wr, rham, vkpt, gradkham )
   !***************************************************
   !
   ! Calculates the gradient of the hamiltonian on the bloch basis 
   ! once given on the Wannier basis
   !
   ! gradkham(:,:,k) = sum_R  i * R * w(R) * e^( i kR ) ham(:,:,R)    
   !
   ! units of vr and vkpt are considered consistent in
   ! such a way that  sum_i vr(i) * vkpt(i) 
   ! is the adimensional scalr product k dot R (given in cartesian coordinates)
   !
   USE kinds
   USE constants,      ONLY : CZERO, CI
   USE log_module,     ONLY : log_push, log_pop
   USE timing_module,  ONLY : timing
   !
   IMPLICIT NONE
 
   !
   ! input variables
   !
   INTEGER,      INTENT(in)  :: dimwann, nr
   REAL(dbl),    INTENT(in)  :: vr( 3, nr),  wr(nr), vkpt(3)
   COMPLEX(dbl), INTENT(in)  :: rham( dimwann, dimwann, nr)
   COMPLEX(dbl), INTENT(out) :: gradkham( dimwann, dimwann, 3 )
 
   !
   ! local variables
   !
   INTEGER      :: i, j, ir, l 
   REAL(dbl)    :: arg
   COMPLEX(dbl) :: phase
   !
   ! end of declariations
   !

!
!------------------------------
! main body
!------------------------------
!
   CALL timing('compute_grad_kham',OPR='start') 
   CALL log_push('compute_grad_kham')
  
   gradkham( :, :, :) = CZERO
   !
   DO ir = 1, nr
       !
       arg =   DOT_PRODUCT( vkpt(:), vr(:, ir ) )
       phase = CMPLX( COS(arg), SIN(arg), dbl ) * wr(ir)
       !
       DO l = 1, 3
       DO j = 1, dimwann
       DO i = 1, dimwann
           gradkham(i,j,l) = gradkham(i,j,l) + CI * vr(l, ir) * phase * rham(i, j, ir)
       ENDDO
       ENDDO
       ENDDO
       !
   ENDDO

   CALL timing('compute_grad_kham',OPR='stop') 
   CALL log_pop ('compute_grad_kham')
    
END SUBROUTINE compute_grad_kham

