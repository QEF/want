! 
! Copyright (C) 2005 WanT Group
! 
! This file is distributed under the terms of the 
! GNU General Public License. See the file `License' 
! in the root directory of the present distribution, 
! or http://www.gnu.org/copyleft/gpl.txt . 
! 
!*********************************************************
SUBROUTINE overlap_update(dimwann, nkpts, U, Mkb)
   !*********************************************************
   !
   ! This subroutine updates the overlaps integrals by a unitary
   ! transformation, according to the formula
   !
   ! Mkb(k,b) =  U(k)^dag * Mkb(k,b) * U(k+b)
   !
   USE kinds, ONLY : dbl
   USE timing_module, ONLY : timing
   USE util_module,  ONLY : mat_mul
   USE kpoints_module,  ONLY : nb, nnlist 
   IMPLICIT NONE

   
   !
   ! input variables
   !
   INTEGER,         INTENT(in)    :: dimwann, nkpts
   COMPLEX(dbl),    INTENT(in)    :: U(dimwann,dimwann,nkpts) 
   COMPLEX(dbl),    INTENT(inout) :: Mkb(dimwann,dimwann,nb,nkpts) 

   !
   ! local variables
   !
   COMPLEX(dbl), ALLOCATABLE :: aux(:,:)
   INTEGER                   :: ik, ikb, inn, ierr
   ! 
   ! ... end of declarations
   ! 

!
!-----------------------------
! routine Main body
!-----------------------------
!

   CALL timing('overlap_update',OPR='start')

   ALLOCATE( aux(dimwann,dimwann), STAT=ierr ) 
      IF (ierr/=0) CALL errore("overlap_update","allocating aux",ABS(ierr))


   DO ik = 1, nkpts
      DO inn= 1, nb
         ikb = nnlist( inn, ik )

         !
         ! aux1 = U(ik)^dag * Mkb * U(ikb)
         !
         CALL mat_mul(aux, U(:,:,ik), 'C', Mkb(:,:,inn,ik), 'N', &
                      dimwann, dimwann, dimwann)
         CALL mat_mul(Mkb(:,:,inn,ik), aux, 'N', U(:,:,ikb), 'N', & 
                      dimwann, dimwann, dimwann)
      ENDDO
   ENDDO

   !
   ! cleaning
   !
   DEALLOCATE( aux, STAT=ierr ) 
      IF (ierr/=0) CALL errore("overlap_update","deallocating aux",ABS(ierr))

   CALL timing('overlap_update',OPR='stop')

END SUBROUTINE overlap_update


