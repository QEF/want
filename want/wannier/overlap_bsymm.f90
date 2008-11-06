! 
! Copyright (C) 2008 WanT Group
! 
! This file is distributed under the terms of the 
! GNU General Public License. See the file `License' 
! in the root directory of the present distribution, 
! or http://www.gnu.org/copyleft/gpl.txt . 
! 
!*********************************************************
SUBROUTINE overlap_bsymm(dimwinx, dimwann, nkpts, Mkb)
   !*********************************************************
   !
   ! This subroutine perform the symmetrization of overlap
   ! with +b vectors to obtain those with -b.
   !
   ! M_ij(k,b) = CONJG( M_ji (k+b, -b) )
   !
   ! Mkb(:,:, nnrev(ib), ikb) = CONJG( TRANSPOSE( Mkb(:,:,ib,ik)))
   !
   USE kinds,             ONLY : dbl
   USE timing_module,     ONLY : timing
   USE log_module,        ONLY : log_push, log_pop
   USE kpoints_module,    ONLY : nkpts_g, iks, iproc_g, nb, nnpos, nnrev, nnlist 
   USE mp_global,         ONLY : mpime
   USE mp,                ONLY : mp_put, mp_barrier
   !
   IMPLICIT NONE

   
   !
   ! input variables
   !
   INTEGER,         INTENT(IN)    :: dimwinx, dimwann, nkpts
   COMPLEX(dbl),    INTENT(INOUT) :: Mkb(dimwinx,dimwinx,nb,nkpts) 

   !
   ! local variables
   !
   CHARACTER(13)             :: subname='overlap_bsymm'
   COMPLEX(dbl), ALLOCATABLE :: caux(:,:)
   INTEGER                   :: ik_proc, ikb_proc
   INTEGER                   :: ik_g, ikb_g, ib, inn, ierr
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


   ALLOCATE( caux(dimwinx,dimwinx), STAT=ierr ) 
   IF (ierr/=0) CALL errore(subname,"allocating caux",ABS(ierr))

   DO ik_g = 1, nkpts_g
       !
       ! take advantage on the symmetry properties of Mkb
       ! M_ji (k+b, -b) = CONJG ( M_ij(k,b) )
       ! Knowing M for positive b, reconstruct the negative ones
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
           IF ( mpime == ik_proc ) THEN
               caux =  Mkb(:,:, ib, ik_g -iks + 1 )
           ENDIF
           !
           CALL timing( 'mp_put', OPR='start' )
           CALL mp_put( caux, caux, mpime, ik_proc, ikb_proc, 1 )           
           CALL timing( 'mp_put', OPR='stop' )
           !
           IF ( mpime == ikb_proc ) THEN
               Mkb(:,:, nnrev(ib), ikb_g -iks +1 ) = CONJG( TRANSPOSE( caux ) )
           ENDIF
           ! 
       ENDDO
       !
   ENDDO
   !
   CALL mp_barrier()

   !
   ! cleaning
   !
   DEALLOCATE( caux, STAT=ierr ) 
   IF (ierr/=0) CALL errore(subname,"deallocating caux",ABS(ierr))

   CALL timing(subname,OPR='stop')
   CALL log_pop(subname)
   !
END SUBROUTINE overlap_bsymm


