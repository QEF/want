!
! Copyright (C) 2004 WanT Group
!
! This file is distributed under the terms of the
! GNU General Public License. See the file `License'
! in the root directory of the present distribution,
! or http://www.gnu.org/copyleft/gpl.txt .
!
!***************************************************
    FUNCTION komegai( ik, mdim, dimwann, dimwin, dimwinx, lamp, Mkb )
   !***************************************************
   !
   ! Calculates the contribution of a given k-point to Omega_I
   ! Omega_I(ik) = - \Sum_b wb * \Sum_m,n  | AUX_m,n |^2  
   ! where
   ! AUX = Lamp(ik)^dag * Mkb * Lamp(ikb)
   !
   ! NOTA: m = 1, mdim
   !       n = 1, dimwann     and other matrix dimensions accordingly
   ! 
   !
   USE kinds
   USE constants, ONLY : ZERO
   USE timing_module
   USE kpoints_module, ONLY : nnlist, nntot, wb, wbtot
   USE util_module, ONLY : zmat_mul

   IMPLICIT NONE
 
   REAL(dbl) :: komegai
   INTEGER   :: ik
   INTEGER   :: mdim
   INTEGER   :: dimwann, dimwin(*), dimwinx
   COMPLEX(dbl) :: lamp(dimwinx,dimwinx,*)
   COMPLEX(dbl) :: Mkb(dimwinx,dimwinx,*)
 
   !
   ! local variables
   !
   INTEGER :: m, n 
   INTEGER :: inn, ikb, ierr
   COMPLEX(dbl), ALLOCATABLE :: aux(:,:), aux1(:,:)

!----------------------------------------------------------------

   CALL timing('komegai',OPR='start') 
   komegai = ZERO
  
   IF ( mdim < 0 .OR. mdim > dimwann ) CALL errore('komegai','invalid mdim',ABS(mdim)+1)
   
   ALLOCATE( aux(dimwinx, dimwann), aux1(mdim,dimwann), STAT=ierr )
      IF (ierr/=0) CALL errore('komegai','allocating aux, aux1',ABS(ierr))

   !
   ! ...  Loop over b-vectors
   !
   DO inn = 1, nntot(ik)
       ikb = nnlist(ik, inn)

       !     
       ! compute aux1 = Lamp(ik)^{\dag} * Mkb * Lamp(ikb)
       ! aux = Mkb * Lamp(ikb)
       ! aux1 = Lamp(ik)^{\dag} * aux
       !     
       CALL zmat_mul(aux,  Mkb(:,:,inn), 'N', lamp(:,:,ikb), 'N', dimwin(ik), dimwann, dimwin(ikb) )
       CALL zmat_mul(aux1, lamp(:,:,ik), 'C', aux, 'N',  mdim, dimwann, dimwin(ik) )

       DO n = 1, dimwann     
       DO m = 1, mdim   
            komegai = komegai - wb(ik,inn) * REAL( CONJG( aux1(m,n) ) * aux1(m,n) )
       ENDDO 
       ENDDO 

   ENDDO 

   !
   ! cleaning
   !
   DEALLOCATE( aux, aux1, STAT=ierr )
      IF (ierr/=0) CALL errore('komegai','deallocating aux',ABS(ierr))

   CALL timing('komegai',OPR='stop') 
    
END FUNCTION komegai

