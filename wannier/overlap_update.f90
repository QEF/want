! 
! Copyright (C) 2005 WanT Group
! 
! This file is distributed under the terms of the 
! GNU General Public License. See the file `License' 
! in the root directory of the present distribution, 
! or http://www.gnu.org/copyleft/gpl.txt . 
! 
!*********************************************************
SUBROUTINE overlap_update(dimwann, nkpts, U, Mkb_in, Mkb_out)
   !*********************************************************
   !
   ! This subroutine updates the overlaps integrals by a unitary
   ! transformation, according to the formula
   !
   ! Mkb(k,b)_out =  U(k)^dag * Mkb(k,b)_in * U(k+b)
   !
   USE kinds,             ONLY : dbl
   USE timing_module,     ONLY : timing
   USE log_module,        ONLY : log_push, log_pop
   USE util_module,       ONLY : mat_mul
   USE kpoints_module,    ONLY : nb, nnpos, nnrev, nnlist 
   IMPLICIT NONE

   
   !
   ! input variables
   !
   INTEGER,         INTENT(in)    :: dimwann, nkpts
   COMPLEX(dbl),    INTENT(in)    :: U(dimwann,dimwann,nkpts) 
   COMPLEX(dbl),    INTENT(in)    :: Mkb_in (dimwann,dimwann,nb,nkpts) 
   COMPLEX(dbl),    INTENT(out)   :: Mkb_out(dimwann,dimwann,nb,nkpts) 

   !
   ! local variables
   !
   COMPLEX(dbl), ALLOCATABLE :: aux(:,:)
   INTEGER                   :: ik, ikb, ib, inn, ierr
   ! 
   ! end of declarations
   ! 

!
!-----------------------------
! main body
!-----------------------------
!
   CALL timing('overlap_update',OPR='start')
   CALL log_push('overlap_update')


   ALLOCATE( aux(dimwann,dimwann), STAT=ierr ) 
      IF (ierr/=0) CALL errore("overlap_update","allocating aux",ABS(ierr))


   DO ik = 1, nkpts
      !
      ! take advantage on the symmetry properties of Mkb
      ! M_ij(k,b) = CONJG( M_ji (k+b, -b) )
      !
      ! perform the loop only for the "positive" b, 
      ! and symmetrize at the end
      !
      DO inn= 1, nb/2
         !
         ib  = nnpos (inn)
         ikb = nnlist( ib, ik )

         !
         ! aux1 = U(ik)^dag * Mkb * U(ikb)
         !
         CALL mat_mul(aux, U(:,:,ik), 'C', Mkb_in(:,:,ib,ik), 'N', &
                      dimwann, dimwann, dimwann)
         CALL mat_mul(Mkb_out(:,:,ib,ik), aux, 'N', U(:,:,ikb), 'N', & 
                      dimwann, dimwann, dimwann)

         !
         ! symmetrize
         !
         Mkb_out(:,:, nnrev(ib), ikb) = CONJG( TRANSPOSE( Mkb_out(:,:,ib,ik)))
      ENDDO
   ENDDO

   !
   ! cleaning
   !
   DEALLOCATE( aux, STAT=ierr ) 
      IF (ierr/=0) CALL errore("overlap_update","deallocating aux",ABS(ierr))

   CALL timing('overlap_update',OPR='stop')
   CALL log_pop('overlap_update')
   !
END SUBROUTINE overlap_update


