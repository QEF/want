!
! Copyright (C) 2004 WanT Group
!
! This file is distributed under the terms of the
! GNU General Public License. See the file `License'
! in the root directory of the present distribution,
! or http://www.gnu.org/copyleft/gpl.txt .
!
!***************************************************
    SUBROUTINE omegai( Omega_I, dimwann, nkpts, Mkb )
   !***************************************************
   !
   ! Calculates the contribution of a given k-point to Omega_I
   ! Omega_I(ik) = - \Sum_b wb * \Sum_m,n  | Mkb_upd_{m,n} |^2  
   ! where
   ! Mkb_upd = Lamp(ik)^dag * Mkb0 * Lamp(ikb)
   !
   ! NOTA: m = 1, mdim
   !       n = 1, dimwann     and other matrix dimensions accordingly
   ! 
   !
   USE kinds
   USE constants, ONLY : ZERO
   USE timing_module
   USE kpoints_module, ONLY : nb, wb, wbtot

   IMPLICIT NONE
 
   !
   ! input variables
   !
   REAL(dbl),    INTENT(out) :: Omega_I
   INTEGER,      INTENT(in)  :: dimwann, nkpts
   COMPLEX(dbl), INTENT(in)  :: Mkb(dimwann,dimwann,nb,nkpts)
 
   !
   ! local variables
   !
   INTEGER :: m, n 
   INTEGER :: ik, ib

!----------------------------------------------------------------

   CALL timing('omegai',OPR='start') 
  
   Omega_I = ZERO
   !
   ! ...  Loop over k- and b-vectors
   !
   DO ik = 1, nkpts
   DO ib = 1, nb

       DO n = 1, dimwann     
       DO m = 1, dimwann
            Omega_I = Omega_I - wb(ib) * &
                      REAL( CONJG( Mkb(m,n,ib,ik) ) * Mkb(m,n,ib,ik) )
       ENDDO 
       ENDDO 

   ENDDO 
   ENDDO 
   !
   Omega_I = Omega_I / DBLE(nkpts) + DBLE(dimwann) * wbtot

   CALL timing('omegai',OPR='stop') 
    
END SUBROUTINE omegai

