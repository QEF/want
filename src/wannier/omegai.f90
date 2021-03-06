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
   ! Calculates the invariant spread functional Omega_I
   !
   ! Omega_I = 1/Nk \Sum_{ik, ib} wb(ib) * ( Nwann - \Sum_mn | Mkb(m,n,ib,ik) |^2 )
   !
   ! where
   ! Mkb_upd = Lamp(ik)^dag * Mkb0 * Lamp(ikb)
   !
   USE kinds
   USE constants,      ONLY : ZERO, TWO
   USE kpoints_module, ONLY : nb, nkpts_g, wb, wbtot, nnpos
   USE log_module,     ONLY : log_push, log_pop
   USE timing_module,  ONLY : timing
   USE mp,             ONLY : mp_sum
   !
   IMPLICIT NONE
 
   !
   ! input variables
   !
   REAL(dbl),    INTENT(out) :: Omega_I
   INTEGER,      INTENT(in)  :: dimwann, nkpts
   COMPLEX(dbl), INTENT(in)  :: Mkb(dimwann,dimwann,nb/2,nkpts)
 
   !
   ! local variables
   !
   INTEGER   :: m, n 
   INTEGER   :: ik, ib, inn
   REAL(dbl) :: cost
   !
   ! end of declariations
   !

!
!------------------------------
! main body
!------------------------------
!
   CALL timing('omegai',OPR='start') 
   CALL log_push('omegai')
  
   Omega_I = ZERO
   !
   ! ...  Loop over k- and b-vectors
   !
   DO ik  = 1, nkpts
   DO inn = 1, nb / 2
       !
       ib = nnpos( inn )
       !
!$omp parallel do reduction (+:Omega_I)
       DO n = 1, dimwann     
       DO m = 1, dimwann
            !
            Omega_I = Omega_I - wb(ib) * &
                      REAL( CONJG( Mkb(m,n,inn,ik) ) * Mkb(m,n,inn,ik), dbl )
            !
       ENDDO 
       ENDDO 
!$omp end parallel do
       !
   ENDDO 
   ENDDO 
   !
   ! recover over parallelism
   !
   CALL timing ( 'mp_sum_omega', OPR='start' )
   CALL mp_sum( Omega_I )
   CALL timing ( 'mp_sum_omega', OPR='stop' )
   
   !
   ! Omega_I is moltiplied by two to account for the -b terms which
   ! have not been summed up in the previous loop
   !
   cost = REAL( nkpts_g, dbl )
   !
   Omega_I = TWO * Omega_I / cost + REAL(dimwann, dbl) * wbtot

   CALL timing('omegai',OPR='stop') 
   CALL log_pop ('omegai')
    
END SUBROUTINE omegai

