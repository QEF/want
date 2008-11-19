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
   ! domg/dW_k = domg/dW_k + 2/N \Sum_b w_b S[ At^kb ]     where
   !
   ! At^{kb}_{mn} = a * w_n * (b * Dr_n)/ Mkb_nn  * Mkb_mn
   ! a:    coupling constant 
   ! Dr_n: <r>_n - r_n0,   r_n0 initial center position
   ! w_n:  weight (from input) of the n-th center
   ! 
   !
   USE kinds
   USE constants,          ONLY : ZERO, ONE, CZERO, CI, TWO
   USE timing_module,      ONLY : timing
   USE log_module,         ONLY : log_push, log_pop
   USE lattice_module,     ONLY : avec
   USE kpoints_module,     ONLY : nkpts_g, iks, iproc_g, nb, vb, wb, nnpos, nnrev, nnlist
   USE mp,                 ONLY : mp_sum
   USE converters_module,  ONLY : cry2cart, cart2cry
   USE trial_center_module
   !
   IMPLICIT NONE 

   !  
   ! input variables 
   !  
   INTEGER,            INTENT(in)    :: dimwann, nkpts
   REAL(dbl),          INTENT(in)    :: rave(3,dimwann), a
   TYPE(trial_center), INTENT(in)    :: trial(dimwann)
   COMPLEX(dbl),       INTENT(in)    :: Mkb(dimwann,dimwann,nb/2,nkpts)
   COMPLEX(dbl),       INTENT(inout) :: domg(dimwann,dimwann,nkpts_g)

   !
   ! local variables
   !
   CHARACTER(10)   :: subname='domega_aux'
   !
   INTEGER         :: ikk_g, ik, ik_g, ikb, ikb_g, ib, inn, ipos
   INTEGER         :: i, m, n, ierr
   REAL(dbl)       :: fact
   REAL(dbl),    ALLOCATABLE :: qb(:), Dr(:,:) 
   COMPLEX(dbl), ALLOCATABLE :: At(:,:)
   COMPLEX(dbl), ALLOCATABLE :: aux2(:,:), Mkb_aux(:,:)
   !
   ! end of declarations 
   !

!
!----------------------------------------
! main Body
!----------------------------------------
!
   CALL timing(subname,OPR='start')
   CALL log_push(subname)


   ALLOCATE( qb(dimwann), STAT=ierr )
   IF( ierr /=0 ) CALL errore(subname, 'allocating qb', ABS(ierr) )
   !
   ALLOCATE( At(dimwann,dimwann), STAT=ierr )
   IF( ierr /=0 ) CALL errore(subname, 'allocating At', ABS(ierr) )
   !
   ALLOCATE( Mkb_aux(dimwann,dimwann), STAT=ierr )
   IF( ierr /=0 ) CALL errore(subname, 'allocating Mkb_aux', ABS(ierr) )
   !
   ALLOCATE( Dr(3,dimwann), STAT=ierr )
   IF( ierr /=0 ) CALL errore(subname, 'allocating Dr', ABS(ierr) )
   !
   ALLOCATE( aux2(dimwann,dimwann),  STAT=ierr )
   IF( ierr /=0 ) CALL errore(subname, 'allocating aux2', ABS(ierr) )

   !
   ! build Dr_n = <r>_n - r_n0
   !
   DO n=1,dimwann
      !
      SELECT CASE ( TRIM(trial(n)%type) )
      CASE ( "1gauss", "atomic" )
          Dr(:,n) = rave(:,n) - trial(n)%x1
      CASE ( "2gauss" )
          Dr(:,n) = rave(:,n) - ( trial(n)%x1 + trial(n)%x2 ) / TWO
      CASE DEFAULT
          CALL errore(subname,'Invalid trial_center type = '//TRIM(trial(n)%type),n )
      END SELECT

      !
      ! Dr should be smaller than a lattice vector
      ! move it into the cell -0.5 : 0.5  (crystal units)
      !
      CALL cart2cry( Dr(:,n), avec )
      !
      DO i = 1, 3
         Dr(i,n) = MODULO( Dr(i,n) +0.5_dbl, ONE ) - 0.5_dbl
      ENDDO
      !
      CALL cry2cart( Dr(:,n), avec )
      !
   ENDDO

   !
   ! domg_aux is calculated
   !
   fact = a / REAL( nkpts_g, dbl)

   kpoints_loop: &
   DO ik = 1, nkpts
       !
       ik_g = ik +iks -1
       !
       bvectors_loop: &
       DO inn = 1, nb / 2 
           !
           DO ipos = 1, 2
               !
               ! here we compute basic quantites separately for "positive"
               ! and "negative" b-vectors
               !
               SELECT CASE ( ipos )
               CASE ( 1 )
                  !
                  ! positive b vectors
                  !
                  ikk_g  = ik_g
                  !
                  ib     = nnpos( inn )
                  !
                  Mkb_aux( :, :)  = Mkb( :, :, inn, ik)
                  !
               CASE ( 2 )
                  !
                  ! negative b vectors
                  !
                  ib       = nnrev( nnpos( inn ) )
                  ikb_g    = nnlist( nnpos(inn), ik_g )
                  !
                  ikk_g    = ikb_g
                  !
                  Mkb_aux( :, :)  = CONJG( TRANSPOSE( Mkb( :, :, inn, ik) ) )
                  !
               CASE DEFAULT
                  CALL errore(subname,'invalid ipos value',71)
               END SELECT
               
               !
               ! Compute:
               !       qb_n  = w_n *  b * Dr_n
               !       At_mn = w_n * (b * Dr_n) * Mkb_mn / Mkb_nn 
               !
               DO n = 1, dimwann
                   !
                   qb(n) = trial(n)%weight * DOT_PRODUCT( vb(:,ib), Dr(:,n) ) 
                   !
                   DO m = 1, dimwann
                       !
                       At(m,n) = qb(n) * Mkb_aux(m,n) / Mkb_aux(n,n)
                       !
                   ENDDO
                   !
               ENDDO
               !
               !
               ! compute the contribution to the variation of the functional
               ! note that a term -i has been factorized out ot make dOmega/dW
               ! hermitean
               !
               aux2(:,:) = CZERO
               !
               DO n = 1, dimwann
               DO m = 1, dimwann
                   !
                   ! S[At^{k,b}] = (At+At\dag)/2 
                   !
                   aux2(m,n) = aux2(m,n) - wb(inn) * ( At(m,n) + CONJG(At(n,m)) )
                 
                   !
                   ! dOmega/dW(k) = 2 * a * \Sum_b wb * (  S[T] )
                   !
                   domg(m,n,ikk_g) = domg(m,n,ikk_g) + fact * aux2(m,n)
                   !
               ENDDO
               ENDDO
               !
           ENDDO
           !
       ENDDO bvectors_loop
       ! 
       !
   ENDDO kpoints_loop

   !   
   ! recover over parallelism
   !   
   CALL timing( 'mp_sum', OPR='start' )
   !
   DO ik_g = 1, nkpts_g
       CALL mp_sum( domg(:,:,ik_g) )
   ENDDO
   !
   CALL timing( 'mp_sum', OPR='stop' )


   !
   ! local cleanup
   !
   DEALLOCATE( qb, STAT=ierr )
   IF( ierr /=0 ) CALL errore(subname, 'deallocating qb', ABS(ierr) )
   !
   DEALLOCATE( At, Dr, STAT=ierr )
   IF( ierr /=0 ) CALL errore(subname, 'deallocating At, Dr', ABS(ierr) )
   !
   DEALLOCATE( Mkb_aux, STAT=ierr )
   IF( ierr /=0 ) CALL errore(subname, 'deallocating Mkb_aux', ABS(ierr) )
   !
   DEALLOCATE( aux2, STAT=ierr )
   IF( ierr /=0 ) CALL errore(subname, 'deallocating aux2', ABS(ierr) )


   CALL timing(subname,OPR='stop')
   CALL log_pop(subname)
   !
END SUBROUTINE domega_aux

