! 
! Copyright (C) 2004 Andrea Ferretti
! 
! This file is distributed under the terms of the 
! GNU General Public License. See the file `License' 
! in the root directory of the present distribution, 
! or http://www.gnu.org/copyleft/gpl.txt . 
! 
!*********************************************************
SUBROUTINE check_sgm_kpt(Nk,Vck,vkpt,bvec)
   !*********************************************************
   USE kinds
   IMPLICIT NONE

! <INFO>
! Check if the kpt-grid associated with INPUT sigma is
! consistent with the grid used in the WANNIER code
! 
! Nk       number of kpt common to sigma and wannier
! Vck      kpt coordinates from sigma (PW units)
! vkpt     kpt components in terms of BVEC from wannier
! bvec     reciprocal lattice generator
!
! </INFO>

   REAL(dbl), PARAMETER                :: toll=1.0e-6

   INTEGER, INTENT(in)                 :: Nk
   REAL(dbl), INTENT(in)               :: Vck(3,Nk),    &
                                          vkpt(3,Nk),   &
                                           bvec(3,3)
   

   INTEGER                             :: i,j,k,l
   REAL(dbl)                           :: Vck_wan(3,Nk),&
                                          norm


!------------------------------------------------


!
! generates Vck_wan
!
   Vck_wan(:,:) = 0.0
   DO k=1,Nk
       DO i=1,3
       DO j=1,3
         Vck_wan(i,k) = Vck_wan(i,k) + vkpt(j,k)  *  bvec(i,j)
       END DO
       END DO
   END DO
       

!
! one by one check
!   
   Vck_wan(:,:) = Vck_wan(:,:) - Vck(:,:)
   DO k=1,Nk
       norm = DOT_PRODUCT(  Vck_wan(:,k), Vck_wan(:,k) )
       IF ( norm > toll )  THEN
           CALL errore('check_sgm_kpt','Inconsistency with sigma-input kpt',1)
       END IF
   END DO


   END SUBROUTINE  check_sgm_kpt



