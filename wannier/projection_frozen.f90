!
! Copyright (C) 2004 WanT Group
!
! This file is distributed under the terms of the
! GNU General Public License. See the file `License'
! in the root directory of the present distribution,
! or http://www.gnu.org/copyleft/gpl.txt .
!
! Based on a previous version by I. Souza, N. Marzari and D. Vanderbilt
! See the file README in the root directory for a full list of credits
!
!*******************************************
   SUBROUTINE projection_frozen( lamp, dimwann, dimwin, dimwinx, nkpts, dimfroz, frozen)
   !*******************************************
   USE kinds
   USE constants, ONLY : ZERO, CONE, CZERO, ONE, EPS_m8
   USE timing_module, ONLY : timing
   USE util_module, ONLY : mat_hdiag, zmat_unitary, mat_mul
   USE io_module, ONLY : stdout
   USE control_module, ONLY : unitary_thr
   IMPLICIT NONE

       INTEGER :: dimwann 
       INTEGER :: nkpts
       INTEGER :: dimwin(nkpts), dimwinx
       INTEGER :: dimfroz(nkpts)
       COMPLEX(dbl) :: lamp(dimwinx,dimwann,nkpts)
       LOGICAL :: frozen(dimwinx,nkpts)

       REAL(dbl),    ALLOCATABLE :: w(:)
       COMPLEX(dbl), ALLOCATABLE :: z(:,:)
       COMPLEX(dbl), ALLOCATABLE :: p_s(:,:)
       COMPLEX(dbl), ALLOCATABLE :: q_froz(:,:)
       COMPLEX(dbl), ALLOCATABLE :: pq(:,:)
       COMPLEX(dbl), ALLOCATABLE :: qpq(:,:)
 
       INTEGER :: ik, j, l, n
       INTEGER :: il, iu, ierr

! ...  End of declarations

       CALL timing('projection_frozen',OPR='start')
 
! ...  Local allocations
       ALLOCATE( z(dimwinx,dimwinx), STAT = ierr )
         IF( ierr /= 0 ) CALL errore( 'projection_frozen', 'allocating z ', ABS(ierr) )
       ALLOCATE( w(dimwinx), STAT = ierr )
         IF( ierr /= 0 ) CALL errore( 'projection_frozen', 'allocating w ', ABS(ierr) )
       ALLOCATE( p_s(dimwinx,dimwinx), STAT = ierr )
         IF( ierr /= 0 ) CALL errore( 'projection_frozen', 'allocating p_s ', ABS(ierr) )
       ALLOCATE( q_froz(dimwinx,dimwinx), STAT = ierr )
         IF( ierr /= 0 ) CALL errore( 'projection_frozen', 'allocating q_froz ',ABS(ierr) )
       ALLOCATE( pq(dimwinx,dimwinx), STAT = ierr )
         IF( ierr /= 0 ) CALL errore( 'projection_frozen', 'allocating pq', ABS(ierr) )
       ALLOCATE( qpq(dimwinx,dimwinx), STAT = ierr )
         IF( ierr /= 0 ) CALL errore( 'projection_frozen', 'allocating qpq', ABS(ierr) )


       DO ik =1, nkpts
 
! ...    If there are less frozen states than the target number of bands at the 
!        present k-point, compute the dimwann-dimfroz(ik) leading eigenvectors 
!        of the matrix qpq
 
         IF ( dimwann > dimfroz(ik) ) THEN
 
              q_froz(:,:) = CZERO
              DO n=1, dimwin(ik)
                  IF( .NOT. frozen(n,ik) )  q_froz(n,n) = CONE
              ENDDO
              !      
              ! p_s = lamp * lamp^{dag}
              CALL mat_mul( p_s, lamp(:,:,ik), 'N', lamp(:,:,ik), 'C', &
                                 dimwin(ik), dimwin(ik), dimwann )
         
              !
              ! pq = p_s * q_froz
              CALL mat_mul( pq, p_s, 'N', q_froz, 'N', &
                                 dimwin(ik), dimwin(ik), dimwin(ik) )
              
              !
              ! qpq = q_froz * pq
              CALL mat_mul( qpq, q_froz, 'N', pq, 'N', &
                                 dimwin(ik), dimwin(ik), dimwin(ik) )
              
!
! ...      Diagonalize qpq and check its hermiticity

           CALL mat_hdiag(z, w, qpq, dimwin(ik) )

! ...      Pick the dimwann-dimfroz(ik) leading eigenvectors to be trial states; 
!          put them right after the frozen states in lamp

           il = dimwin(ik) - ( dimwann - dimfroz(ik) ) + 1
           iu = dimwin(ik)

           !
           ! ... set lamp
           DO l = dimfroz(ik) + 1, dimwann
             DO j = 1, dimwin(ik)
               lamp(j,l,ik) = z(j,il)  
             END DO
             il = il + 1   
           END DO
           IF( il - 1 /= iu ) CALL errore(' projection_frozen ', ' check failed ', il )

           !
           ! ... check LEFT unitariery (lamp^dag * lamp = I)
           !
           IF ( .NOT. zmat_unitary( dimwin(ik), dimwann-dimfroz(ik),  &
                                    lamp(:,dimfroz(ik)+1:dimwann,ik), &
                                    SIDE='left', TOLL=unitary_thr ) ) &
                CALL errore(' projection_frozen ', 'Vectors in lamp not orthonormal',ik)

         ENDIF ! dimwann>dimfroz(ik)
       ENDDO 

       DEALLOCATE( z, STAT=ierr )
          IF (ierr/=0) CALL errore('projection_frozen','deallocating z',ABS(ierr))
       DEALLOCATE( w, STAT=ierr )
          IF (ierr/=0) CALL errore('projection_frozen','deallocating w',ABS(ierr))
       DEALLOCATE( p_s, STAT=ierr )
          IF (ierr/=0) CALL errore('projection_frozen','deallocating p_s',ABS(ierr))
       DEALLOCATE( q_froz, STAT=ierr )
          IF (ierr/=0) CALL errore('projection_frozen','deallocating q_froz',ABS(ierr))
       DEALLOCATE( pq, STAT=ierr )
          IF (ierr/=0) CALL errore('projection_frozen','deallocating pq',ABS(ierr))
       DEALLOCATE( qpq, STAT=ierr )
          IF (ierr/=0) CALL errore('projection_frozen','deallocating qpq',ABS(ierr))

       CALL timing('projection_frozen',OPR='stop')

       RETURN
       END SUBROUTINE







