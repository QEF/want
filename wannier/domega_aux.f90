! 
! Copyright (C) 2005 WanT Group
! 
! This file is distributed under the terms of the 
! GNU General Public License. See the file `License' 
! in the root directory of the present distribution, 
! or http://www.gnu.org/copyleft/gpl.txt . 
! 
!
!*****************************************************************
   SUBROUTINE domega_aux( dimwann, nkpts, Mkb, rave, trial, a, domg )
   !*****************************************************************
   !
   ! This routine compute the additional part of the omega variation
   ! accounting for the term driving the conditioned minimization
   !
   ! domg / dW_k = 2/N \Sum_b w_b S[ At^kb ]     where
   !
   ! At^{kb}_{mn} = a * w_n * (b * Dr_n)/ Mkb_nn  * Mkb_mn
   ! a:    coupling constant 
   ! Dr_n: <r>_n - r_n0,   r_n0 initial center position
   ! w_n:  weight (from input) of the n-th center
   ! 
   !
   USE kinds
   USE constants, ONLY : ZERO, ONE, CZERO, CI, TWO
   USE timing_module, ONLY : timing
   USE kpoints_module, ONLY : nb, vb, wb
   USE trial_center_module
   IMPLICIT NONE 

   !  
   ! input variables 
   !  
   INTEGER,            INTENT(in)  :: dimwann, nkpts
   REAL(dbl),          INTENT(in)  :: rave(3,dimwann), a
   TYPE(trial_center), INTENT(in)  :: trial(dimwann)
   COMPLEX(dbl),       INTENT(in)  :: Mkb(dimwann,dimwann,nb,nkpts)
   COMPLEX(dbl),       INTENT(out) :: domg(dimwann,dimwann,nkpts)

   !
   ! local variables
   !
   INTEGER   :: ik, inn
   INTEGER   :: m, n, ierr
   REAL(dbl) :: fact
   REAL(dbl),    ALLOCATABLE :: qb(:), Dr(:,:) 
   COMPLEX(dbl), ALLOCATABLE :: At(:,:)
   COMPLEX(dbl), ALLOCATABLE :: aux2(:,:)
   !
   ! end of declarations 
   !

!
!----------------------------------------
! main Body
!----------------------------------------
!
   CALL timing('domega_aux',OPR='start')

   ALLOCATE( qb(dimwann), STAT=ierr )
        IF( ierr /=0 ) CALL errore('domega_aux', 'allocating qb', ABS(ierr) )
   ALLOCATE( At(dimwann,dimwann), STAT=ierr )
        IF( ierr /=0 ) CALL errore('domega_aux', 'allocating At', ABS(ierr) )
   ALLOCATE( Dr(3,dimwann), STAT=ierr )
        IF( ierr /=0 ) CALL errore('domega_aux', 'allocating Dr', ABS(ierr) )
   ALLOCATE( aux2(dimwann,dimwann),  STAT=ierr )
        IF( ierr /=0 ) CALL errore('domega_aux', 'allocating aux2', ABS(ierr) )

   !
   ! build Dr_n = <r>_n - r_n0
   !
   DO n=1,dimwann
      IF ( TRIM(trial(n)%type) == "1gauss" .OR. TRIM(trial(n)%type) == "atomic" ) THEN 
          Dr(:,n) = rave(:,n) - trial(n)%x1
      ELSEIF ( TRIM(trial(n)%type) == "2gauss" ) THEN
          Dr(:,n) = rave(:,n) - ( trial(n)%x1 + trial(n)%x2 ) / TWO
      ELSE 
          CALL errore('domega_aux','Invalid trial_center type = '//TRIM(trial(n)%type),n )
      ENDIF
   ENDDO

   !
   ! domg_aux is calculated
   !
   fact = a / REAL( nkpts, dbl)

   DO ik = 1, nkpts
       !
       aux2(:,:) = CZERO
       !
       DO inn = 1, nb

           !
           ! Compute:
           !       qb_n  = w_n *  b * Dr_n
           !       At_mn = w_n * (b * Dr_n) * Mkb_mn / Mkb_nn 
           !
           DO n = 1, dimwann
               qb(n) = trial(n)%weight * DOT_PRODUCT( vb(:,inn), Dr(:,n) ) 
               DO m = 1, dimwann
                   At(m,n) = qb(n) * Mkb(m,n,inn,ik) / Mkb(n,n,inn,ik)
               ENDDO
           ENDDO

           !
           ! compute the contribution to the variation of the functional
           ! note that a term -i has been factorized out ot make dOmega/dW
           ! hermitean
           !
           DO n = 1, dimwann
           DO m = 1, dimwann
                !
                ! S[At^{k,b}] = (At+At\dag)/2 
                !
                aux2(m,n) = aux2(m,n) - wb(inn) * ( At(m,n) + CONJG(At(n,m)) )

           ENDDO
           ENDDO
       ENDDO

       !
       ! dOmega/dW(k) = 2 * a * \Sum_b wb * (  S[T] )
       !
       domg(:,:,ik) = fact * aux2(:,:)
   ENDDO

   DEALLOCATE( qb, STAT=ierr )
       IF( ierr /=0 ) CALL errore('domega_aux', 'deallocating qb', ABS(ierr) )
   DEALLOCATE( At, Dr, STAT=ierr )
       IF( ierr /=0 ) CALL errore('domega_aux', 'deallocating At, Dr', ABS(ierr) )
   DEALLOCATE( aux2, STAT=ierr )
       IF( ierr /=0 ) CALL errore('domega_aux', 'deallocating aux2', ABS(ierr) )

   CALL timing('domega_aux',OPR='stop')
END SUBROUTINE domega_aux

