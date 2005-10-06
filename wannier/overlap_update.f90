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
   ! This subroutine update the overlaps integrals by a unitary
   ! transformation, according to the formula
   !
   ! Mkb(k,b) =  U(k)^dag * Mkb(k,b) * U(k+b)
   !
   USE kinds, ONLY : dbl
   USE timing_module, ONLY : timing
   USE util_module,  ONLY : zmat_mul
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
   COMPLEX(dbl), ALLOCATABLE :: aux(:,:), aux1(:,:)
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
   ALLOCATE( aux1(dimwann,dimwann), STAT=ierr ) 
      IF (ierr/=0) CALL errore("overlap_update","allocating aux1",ABS(ierr))


   DO ik = 1, nkpts
      DO inn= 1, nb
         ikb = nnlist( inn, ik )

         !
         ! aux1 = U(ik)^dag * Mkb * U(ikb)
         !
         CALL zmat_mul(aux, U(:,:,ik), 'C', Mkb(:,:,inn,ik), 'N', &
                       dimwann, dimwann, dimwann)
         CALL zmat_mul(aux1(:,:), aux, 'N', U(:,:,ikb), 'N', & 
                       dimwann, dimwann, dimwann)
         !
         ! update Mkb
         !
         Mkb(:,:,inn,ik) = aux1(:,:)

      ENDDO
   ENDDO

   !
   ! ... cleaning
   !
   DEALLOCATE( aux, STAT=ierr ) 
      IF (ierr/=0) CALL errore("overlap_update","deallocating aux",ABS(ierr))
   DEALLOCATE( aux1, STAT=ierr ) 
      IF (ierr/=0) CALL errore("overlap_update","deallocating aux1",ABS(ierr))

   CALL timing('overlap_update',OPR='stop')

END SUBROUTINE overlap_update


