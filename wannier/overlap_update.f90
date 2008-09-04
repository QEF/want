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
   USE kpoints_module,    ONLY : iks, nb, nnpos, nnlist 
   !
   IMPLICIT NONE

   
   !
   ! input variables
   !
   INTEGER,         INTENT(in)    :: dimwann, nkpts
   COMPLEX(dbl),    INTENT(in)    :: U(dimwann,dimwann,*) 
   COMPLEX(dbl),    INTENT(in)    :: Mkb_in (dimwann,dimwann,nb/2,nkpts) 
   COMPLEX(dbl),    INTENT(out)   :: Mkb_out(dimwann,dimwann,nb/2,nkpts) 

   !
   ! local variables
   !
   CHARACTER(14)             :: subname='overlap_update'
   COMPLEX(dbl), ALLOCATABLE :: caux1(:,:), caux2(:,:)
   INTEGER                   :: ik, ik_g, ikb, ikb_g, ib, inn, ierr
   ! 
   ! end of declarations
   ! 

!
!-----------------------------
! main body
!-----------------------------
!
   CALL timing(subname,OPR='start')
   CALL log_push(subname)


   ALLOCATE( caux1(dimwann,dimwann), STAT=ierr ) 
   IF (ierr/=0) CALL errore(subname,"allocating caux1",ABS(ierr))


   DO ik = 1, nkpts
       !
       ! do only the Mkb overlaps corresponding to positive b-vectors
       !
       DO inn = 1, nb / 2
           !
           ib    = nnpos (inn)
           ik_g  = ik +iks -1
           ikb_g = nnlist( ib, ik_g )
 
           !
           ! aux1 = U(ik)^dag * Mkb * U(ikb)
           ! 
           CALL mat_mul(caux1, U(:,:,ik_g), 'C', Mkb_in(:,:,inn,ik), 'N', &
                        dimwann, dimwann, dimwann)

           CALL mat_mul(Mkb_out(:,:,inn,ik), caux1, 'N', U(:,:, ikb_g), 'N', & 
                        dimwann, dimwann, dimwann)
           !
       ENDDO
       !
   ENDDO

   !
   ! cleaning
   !
   DEALLOCATE( caux1, STAT=ierr ) 
   IF (ierr/=0) CALL errore(subname,"deallocating caux1",ABS(ierr))

   CALL timing(subname,OPR='stop')
   CALL log_pop(subname)
   !
END SUBROUTINE overlap_update


