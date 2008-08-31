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
   USE kpoints_module,    ONLY : nkpts_g, iks, iproc_g, nb, nnpos, nnrev, nnlist 
   USE mp_global,         ONLY : mpime
   USE mp,                ONLY : mp_get
   !
   IMPLICIT NONE

   
   !
   ! input variables
   !
   INTEGER,         INTENT(in)    :: dimwann, nkpts
   COMPLEX(dbl),    INTENT(in)    :: U(dimwann,dimwann,nkpts) 
   COMPLEX(dbl),    INTENT(in)    :: Mkb_in (dimwann,dimwann,nb/2,nkpts) 
   COMPLEX(dbl),    INTENT(out)   :: Mkb_out(dimwann,dimwann,nb/2,nkpts) 

   !
   ! local variables
   !
   COMPLEX(dbl), ALLOCATABLE :: caux1(:,:), caux2(:,:)
   INTEGER                   :: ik_proc, ikb_proc
   INTEGER                   :: ik, ik_g, ikb, ikb_g, ib, inn, ierr
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


   ALLOCATE( caux1(dimwann,dimwann), STAT=ierr ) 
   IF (ierr/=0) CALL errore("overlap_update","allocating caux1",ABS(ierr))
   ALLOCATE( caux2(dimwann,dimwann), STAT=ierr ) 
   IF (ierr/=0) CALL errore("overlap_update","allocating caux2",ABS(ierr))


   DO ik_g = 1, nkpts_g
       !
       !
       ! take advantage on the symmetry properties of Mkb
       ! M_ij(k,b) = CONJG( M_ji (k+b, -b) )
       ! only positive b-vectors are considered
       !
       DO inn = 1, nb / 2
           !
           ib    = nnpos (inn)
           ikb_g = nnlist( ib, ik_g )
           !
           ik_proc  = iproc_g ( ik_g )
           ikb_proc = iproc_g ( ikb_g )

           !
           ! get U(ikb) in the current pool
           !
           CALL timing( 'mp_get', OPR='start' )
           !
           IF ( mpime == ikb_proc ) THEN
               caux2 =  U(:,:,ikb_g -iks +1 )
           ENDIF
           !
           CALL mp_get( caux2, caux2, mpime, ik_proc, ikb_proc, 1 )           
           !
           CALL timing( 'mp_get', OPR='stop' )

 
           !
           ! aux1 = U(ik)^dag * Mkb * U(ikb)
           !
           IF ( mpime == ik_proc ) THEN
               !
               ik = ik_g - iks +1
               !
               CALL mat_mul(caux1, U(:,:,ik), 'C', Mkb_in(:,:,inn,ik), 'N', &
                            dimwann, dimwann, dimwann)

               CALL mat_mul(Mkb_out(:,:,inn,ik), caux1, 'N', caux2, 'N', & 
                            dimwann, dimwann, dimwann)
               !
           ENDIF
           !
       ENDDO
       !
   ENDDO

   !
   ! cleaning
   !
   DEALLOCATE( caux1, STAT=ierr ) 
   IF (ierr/=0) CALL errore("overlap_update","deallocating caux1",ABS(ierr))
   DEALLOCATE( caux2, STAT=ierr ) 
   IF (ierr/=0) CALL errore("overlap_update","deallocating caux2",ABS(ierr))

   CALL timing('overlap_update',OPR='stop')
   CALL log_pop('overlap_update')
   !
END SUBROUTINE overlap_update


