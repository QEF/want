! 
! Copyright (C) 2004 WanT Group
! 
! This file is distributed under the terms of the 
! GNU General Public License. See the file `License' 
! in the root directory of the present distribution, 
! or http://www.gnu.org/copyleft/gpl.txt . 
!
!*****************************************************************
   SUBROUTINE domega( dimwann, nkpts, Mkb, csheet, sheet, rave, &
                      do_condmin, a_condmin, domg )
   !*****************************************************************
   !
   ! This routine compute the gradinet of the spread functional
   ! respect to W  (U = e^iW):
   !
   ! dOmega/dW(k) = 4 * \Sum_b wb * (  A[R] - S[T] )
   ! the detailed definitions of terms are given below
   !
   ! Note that the first term ( A[R] ) comes from the
   ! gradiend of Omega_I+OD, while the second from that
   ! of Omega_D
   !
   USE kinds
   USE constants,          ONLY : ZERO, ONE, TWO, CZERO, CI
   USE timing_module,      ONLY : timing
   USE log_module,         ONLY : log_push, log_pop
   USE kpoints_module,     ONLY : nkpts_g, iks, nb, vb, wb, nnpos, nnrev, nnlist
   USE mp,                 ONLY : mp_sum
   !
   USE trial_center_data_module, ONLY : trial
   !
   IMPLICIT NONE 

   !  
   ! input variables 
   !  
   INTEGER,      INTENT(IN)  :: dimwann, nkpts
   REAL(dbl),    INTENT(IN)  :: rave(3,dimwann)
   REAL(dbl),    INTENT(IN)  :: sheet(dimwann,nb,nkpts_g)
   COMPLEX(dbl), INTENT(IN)  :: csheet(dimwann,nb,nkpts_g)
   COMPLEX(dbl), INTENT(IN)  :: Mkb(dimwann,dimwann,nb/2,nkpts)
   LOGICAL,      INTENT(IN)  :: do_condmin
   REAL(dbl),    INTENT(IN)  :: a_condmin
   COMPLEX(dbl), INTENT(OUT) :: domg(dimwann*(dimwann+1)/2,nkpts_g)

   !
   ! local variables
   !
   CHARACTER(6)    :: subname='domega'
   !
   INTEGER         :: ikk_g, ik, ik_g, ikb_g, ib, inn, ipos
   INTEGER         :: m, n, l, ierr
   REAL(dbl)       :: fact
   COMPLEX(dbl)    :: aux1, aux2
   REAL(dbl),    ALLOCATABLE :: qkb(:), sheet_aux(:) 
   COMPLEX(dbl), ALLOCATABLE :: R(:,:), T(:,:)
   COMPLEX(dbl), ALLOCATABLE :: Mkb_aux(:,:), csheet_aux(:)
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
      

   ALLOCATE( qkb(dimwann), STAT=ierr )
   IF( ierr /=0 ) CALL errore(subname, 'allocating qkb', ABS(ierr) )
   !
   ALLOCATE( R(dimwann,dimwann), T(dimwann,dimwann), STAT=ierr )
   IF( ierr /=0 ) CALL errore(subname, 'allocating R,T', ABS(ierr) )
   !
   ALLOCATE( Mkb_aux(dimwann,dimwann),  STAT=ierr )
   IF( ierr /=0 ) CALL errore(subname, 'allocating Mkb_aux', ABS(ierr) )
   !
   ALLOCATE( csheet_aux(dimwann), sheet_aux(dimwann),  STAT=ierr )
   IF( ierr /=0 ) CALL errore(subname, 'allocating sheets', ABS(ierr) )


   !
   ! domg is calculated
   !
   fact = TWO / REAL( nkpts_g, dbl )
   domg(:,:) = CZERO
   !
   !
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
                  csheet_aux( : ) = csheet(:, ib, ik_g)
                  sheet_aux( : )  =  sheet(:, ib, ik_g)
                  !
               CASE ( 2 )
                  !
                  ! negative b vectors
                  !
                  ib       = nnrev( nnpos( inn ) )
                  ikb_g    = nnlist( nnpos(inn), ik_g)
                  !
                  ikk_g    = ikb_g
                  !
                  Mkb_aux( :, :)  = CONJG( TRANSPOSE( Mkb( :, :, inn, ik) ) )
                  csheet_aux( : ) = csheet(:, ib, ikb_g )
                  sheet_aux( : )  =  sheet(:, ib, ikb_g ) 
                  !
               CASE DEFAULT
                  CALL errore(subname,'invalid ipos value',71)
               END SELECT
              
               !
               ! Compute:
               !       qbk_n = Im Log Mkb_nn + b * r_n 
               !
               !      R (mn) = Mkb_mn * CONJG( Mkb_nn ) 
               !      T (mn) = Mkb_mn / Mkb_nn * qkb_n
               !
               !
               DO n = 1, dimwann
                   !
                   qkb(n) = AIMAG( LOG( csheet_aux(n)* Mkb_aux(n,n) ) - sheet_aux(n) ) + &
                            DOT_PRODUCT( vb(:,ib), rave(:,n) ) 
                   !
                   DO m = 1, dimwann
                       !
                       R (m,n) = Mkb_aux(m,n) * CONJG( Mkb_aux(n,n) )
                       T (m,n) = Mkb_aux(m,n) / Mkb_aux(n,n) * qkb(n)
                       !
                   ENDDO
                   !
               ENDDO
               !
               !
               ! compute the contribution to the variation of the functional
               ! note that a term -i has been factorized out to make dOmega/dW
               ! hermitean
               !
               DO n = 1, dimwann
               DO m = 1, n
                   !
                   l =  (n-1) * n /2 + m

                   !
                   ! A[R^{k,b}] = -i ( R - R^dag )/2
                   ! where    R_mn = Mkb(m,n) * CONJG( Mkb(n,n) )
                   ! which is different from what reported on the paper PRB 56, 12852 (97)
                   !
                   aux1 = - wb(ib) * CI * ( R(m,n) - CONJG( R(n,m)) )
     
                   !
                   ! S[T^{k,b}] = (T+Tdag)/2 
                   !
                   aux2 = + wb(ib) * ( T(m,n) + CONJG(T(n,m)) )
                   
                   !
                   ! dOmega/dW(k) = 4 * \Sum_b wb * (  A[R] - S[T] )
                   !
                   domg(l,ikk_g) = domg(l,ikk_g) + fact * ( aux1 + aux2 )
                   !
               ENDDO
               ENDDO
               !
           ENDDO
           !
       ENDDO bvectors_loop
       !    
   ENDDO kpoints_loop

   !
   ! if the case, compute the penalty functional terms
   ! note that the action of this subroutine is to add the
   ! penalty contributions to domg
   !
   IF ( do_condmin ) THEN
       !
       CALL domega_aux( dimwann, nkpts, Mkb, rave, trial, a_condmin, domg)
       !
   ENDIF

   !
   ! recover over parallelism
   !
   CALL timing( 'mp_sum_domega', OPR='start' )
   DO ik_g = 1, nkpts_g
       !
       CALL mp_sum( domg(:,ik_g) )
       !
   ENDDO
   CALL timing( 'mp_sum_domega', OPR='stop' )


   !
   ! local cleanup
   !
   DEALLOCATE( qkb, STAT=ierr )
   IF( ierr /=0 ) CALL errore(subname, 'deallocating qkb', ABS(ierr) )
   !
   DEALLOCATE( R, T, STAT=ierr )
   IF( ierr /=0 ) CALL errore(subname, 'deallocating R, T', ABS(ierr) )
   !
   DEALLOCATE( Mkb_aux, STAT=ierr )
   IF( ierr /=0 ) CALL errore(subname, 'deallocating Mkb_aux', ABS(ierr) )
   !
   DEALLOCATE( csheet_aux, sheet_aux, STAT=ierr )
   IF( ierr /=0 ) CALL errore(subname, 'deallocating csheet_aux, sheet_aux', ABS(ierr) )


   CALL timing(subname,OPR='stop')
   CALL log_pop(subname)
   !
END SUBROUTINE domega

