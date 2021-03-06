!
! Copyright (C) 2004 WanT Group
!
! This file is distributed under the terms of the
! GNU General Public License. See the file `License'
! in the root directory of the present distribution,
! or http://www.gnu.org/copyleft/gpl.txt .
!
! Based on a previous version by I. Souza, N. Marzari and D. Vanderbilt
! See the CREDITS file in the ~want directory for a full description
!
!*********************************************************************
   SUBROUTINE projection_frozen( lamp, dimwann, dimwin, dimwinx, nkpts, dimfroz, frozen)
   !*********************************************************************
   !
   ! this routine enforce the "frozening condition" on the lamp matrices.
   ! note that no use of the projection is done.
   !
   USE kinds
   USE constants,        ONLY : ZERO, CONE, CZERO, ONE, EPS_m8
   USE io_module,        ONLY : stdout
   USE timing_module,    ONLY : timing
   USE log_module,       ONLY : log_push, log_pop
   USE util_module,      ONLY : mat_hdiag, zmat_is_unitary, mat_mul
   USE control_module,   ONLY : unitary_thr
   USE kpoints_module,   ONLY : iks
   !
   IMPLICIT NONE

   !
   ! input variables
   !
   INTEGER,         INTENT(in) :: dimwann, nkpts
   INTEGER,         INTENT(in) :: dimwin(*), dimwinx
   INTEGER,         INTENT(in) :: dimfroz(*)
   LOGICAL,         INTENT(in) :: frozen(dimwinx,*)
   COMPLEX(dbl), INTENT(inout) :: lamp(dimwinx,dimwann,*)

   !
   ! local variables 
   ! 
   CHARACTER(17)             :: subname= 'projection_frozen'
   REAL(dbl),    ALLOCATABLE :: w(:)
   COMPLEX(dbl), ALLOCATABLE :: z(:,:)
   COMPLEX(dbl), ALLOCATABLE :: p_s(:,:)
   COMPLEX(dbl), ALLOCATABLE :: q_froz(:,:)
   COMPLEX(dbl), ALLOCATABLE :: pq(:,:)
   COMPLEX(dbl), ALLOCATABLE :: qpq(:,:)
   ! 
   INTEGER :: ik, ik_g, l, n
   INTEGER :: il, iu, ierr
   !
   ! end of declariations
   !

!
!------------------------------
! main body
!------------------------------
!
   CALL timing(subname,OPR='start')
   CALL log_push(subname)
 
!
! local workspace
!
   ALLOCATE( z(dimwinx,dimwinx), STAT = ierr )
   IF( ierr /= 0 ) CALL errore(subname, 'allocating z ', ABS(ierr) )
   !
   ALLOCATE( w(dimwinx), STAT = ierr )
   IF( ierr /= 0 ) CALL errore(subname, 'allocating w ', ABS(ierr) )
   !
   ALLOCATE( p_s(dimwinx,dimwinx), STAT = ierr )
   IF( ierr /= 0 ) CALL errore(subname, 'allocating p_s ', ABS(ierr) )
   !
   ALLOCATE( q_froz(dimwinx,dimwinx), STAT = ierr )
   IF( ierr /= 0 ) CALL errore(subname, 'allocating q_froz ',ABS(ierr) )
   !
   ALLOCATE( pq(dimwinx,dimwinx), STAT = ierr )
   IF( ierr /= 0 ) CALL errore(subname, 'allocating pq', ABS(ierr) )
   !
   ALLOCATE( qpq(dimwinx,dimwinx), STAT = ierr )
   IF( ierr /= 0 ) CALL errore(subname, 'allocating qpq', ABS(ierr) )

!
! main loop over kpt
!

   kpoints_loop: &
   DO ik =1, nkpts
       !
       ik_g = ik + iks -1
       !
       ! If there are less frozen states than the target number of bands at the 
       ! present k-point, compute the dimwann-dimfroz(ik) leading eigenvectors 
       ! of the QPQ matrix
       !

       IF ( dimwann > dimfroz(ik_g) ) THEN

           q_froz(:,:) = CZERO
           !
           DO n=1, dimwin(ik_g)
               IF( .NOT. frozen(n,ik_g) )  q_froz(n,n) = CONE
           ENDDO
           !      
           ! p_s = lamp * lamp^{dag}
           CALL mat_mul( p_s, lamp(:,:,ik_g), 'N', lamp(:,:,ik_g), 'C', &
                              dimwin(ik_g), dimwin(ik_g), dimwann )
     
           !
           ! pq = p_s * q_froz
           CALL mat_mul( pq, p_s, 'N', q_froz, 'N', &
                              dimwin(ik_g), dimwin(ik_g), dimwin(ik_g) )
          
           !
           ! qpq = q_froz * pq
           CALL mat_mul( qpq, q_froz, 'N', pq, 'N', &
                              dimwin(ik_g), dimwin(ik_g), dimwin(ik_g) )
          
           !
           ! diagonalize qpq
           !
           CALL mat_hdiag(z, w, qpq, dimwin(ik_g) )

           !
           ! Pick the dimwann-dimfroz(ik) leading eigenvectors to be trial states; 
           ! put them right after the frozen states in lamp
           !
           il = dimwin(ik_g) - ( dimwann - dimfroz(ik_g) ) + 1
           iu = dimwin(ik_g)

           !
           ! set lamp
           !
           DO l = dimfroz(ik_g) + 1, dimwann
               !
               lamp( 1:dimwin(ik_g), l, ik_g) = z( 1:dimwin(ik_g) ,il)  
               il = il + 1   
           ENDDO
           !
           IF( il - 1 /= iu ) CALL errore(subname, 'check failed', il )

           !
           ! check LEFT unitariery (lamp^dag * lamp = I)
           !
           IF ( .NOT. zmat_is_unitary( dimwin(ik_g), dimwann-dimfroz(ik_g),  &
                                       lamp(:,dimfroz(ik_g)+1:dimwann,ik_g), &
                                       SIDE='left', TOLL=unitary_thr ) )     &
              CALL errore(subname, 'Vectors in lamp not orthonormal',ik_g)

       ENDIF 
       !
   ENDDO kpoints_loop

   DEALLOCATE( z, STAT=ierr )
   IF (ierr/=0) CALL errore(subname,'deallocating z',ABS(ierr))
   !
   DEALLOCATE( w, STAT=ierr )
   IF (ierr/=0) CALL errore(subname,'deallocating w',ABS(ierr))
   !
   DEALLOCATE( p_s, STAT=ierr )
   IF (ierr/=0) CALL errore(subname,'deallocating p_s',ABS(ierr))
   !
   DEALLOCATE( q_froz, STAT=ierr )
   IF (ierr/=0) CALL errore(subname,'deallocating q_froz',ABS(ierr))
   !
   DEALLOCATE( pq, STAT=ierr )
   IF (ierr/=0) CALL errore(subname,'deallocating pq',ABS(ierr))
   !
   DEALLOCATE( qpq, STAT=ierr )
   IF (ierr/=0) CALL errore(subname,'deallocating qpq',ABS(ierr))

   CALL timing(subname,OPR='stop')
   CALL log_pop(subname)
   !
END SUBROUTINE projection_frozen

