! 
! Copyright (C) 2004 WanT Group
! 
! This file is distributed under the terms of the 
! GNU General Public License. See the file `License' 
! in the root directory of the present distribution, 
! or http://www.gnu.org/copyleft/gpl.txt . 
!
!*****************************************************************
   SUBROUTINE domega( dimwann, nkpts, Mkb, csheet, sheet, rave, domg )
   !*****************************************************************
   USE kinds
   USE constants, ONLY : ZERO, ONE, TWO, CZERO, CI
   USE timing_module, ONLY : timing
   USE kpoints_module, ONLY : nb, vb, wb
   IMPLICIT NONE 

   !  
   ! input variables 
   !  
   INTEGER,      INTENT(in)  :: dimwann, nkpts
   REAL(dbl),    INTENT(in)  :: rave(3,dimwann)
   REAL(dbl),    INTENT(in)  :: sheet(dimwann,nb,nkpts)
   COMPLEX(dbl), INTENT(in)  :: csheet(dimwann,nb,nkpts)
   COMPLEX(dbl), INTENT(in)  :: Mkb(dimwann,dimwann,nb,nkpts)
   COMPLEX(dbl), INTENT(out) :: domg(dimwann,dimwann,nkpts)

   !
   ! local variables
   !
   INTEGER   :: ik, inn
   INTEGER   :: m, n, ierr
   REAL(dbl) :: fact
   REAL(dbl),    ALLOCATABLE :: qkb(:) 
   COMPLEX(dbl), ALLOCATABLE :: R(:,:), Rt(:,:), T(:,:) 
   COMPLEX(dbl), ALLOCATABLE :: aux1(:,:), aux2(:,:)
   !
   ! end of declarations 
   !

!
!----------------------------------------
! main Body
!----------------------------------------
!
   CALL timing('domega',OPR='start')
      

   ALLOCATE( qkb(dimwann), STAT=ierr )
        IF( ierr /=0 ) CALL errore('domega', 'allocating qkb', ABS(ierr) )
   ALLOCATE( R(dimwann,dimwann), T(dimwann,dimwann), STAT=ierr )
        IF( ierr /=0 ) CALL errore('domega', 'allocating R,T', ABS(ierr) )
   ALLOCATE( Rt(dimwann,dimwann), STAT=ierr )
        IF( ierr /=0 ) CALL errore('domega', 'allocating Rt', ABS(ierr) )
   ALLOCATE( aux1(dimwann,dimwann),  STAT=ierr )
        IF( ierr /=0 ) CALL errore('domega', 'allocating aux1', ABS(ierr) )
   ALLOCATE( aux2(dimwann,dimwann),  STAT=ierr )
        IF( ierr /=0 ) CALL errore('domega', 'allocating aux2', ABS(ierr) )


   !
   ! domg is calculated
   !
   fact = TWO / REAL( nkpts, dbl )

   DO ik = 1, nkpts

       aux1(:,:) = CZERO
       aux2(:,:) = CZERO

       DO inn = 1, nb

           !
           ! Compute:
           !       qbk_n = Im Log Mkb_nn + b * r_n 
           !       Rt_mn = Mkb_mn / Mkb_nn 
           !        R_mn = Mkb_mn * CONJG( Mkb_nn ) 
           !        T_mn = Rt_mn * qkb_n
           !
           DO n = 1, dimwann
               qkb(n) = AIMAG( LOG( csheet(n,inn,ik)* Mkb(n,n,inn,ik) ) - &
                               sheet(n,inn,ik) ) + DOT_PRODUCT( vb(:,inn), rave(:,n) ) 
               DO m = 1, dimwann
                   R (m,n) = Mkb(m,n,inn,ik) * CONJG( Mkb(n,n,inn,ik) )
                   Rt(m,n) = Mkb(m,n,inn,ik) / Mkb(n,n,inn,ik)
                   T (m,n) = Rt(m,n) * qkb(n)
               ENDDO
           ENDDO

           !
           ! compute the contribution to the variation of the functional
           ! note that a term -i has been factorized out to make dOmega/dW
           ! hermitean
           !
           DO n = 1, dimwann
           DO m = 1, dimwann
                !
                ! A[R^{k,b}] = -i ( R - R^dag )/2
                ! where    R_mn = Mkb(m,n) * CONJG( Mkb(n,n) )
                ! which is different from what reported on the paper PRB 56, 12852 (97)
                !
                aux1(m,n) = aux1(m,n) - wb(inn) * CI * ( R(m,n) - CONJG( R(n,m)) )

                !
                ! S[T^{k,b}] = (T+Tdag)/2 
                !
                aux2(m,n) = aux2(m,n) + wb(inn) * ( T(m,n) + CONJG(T(n,m)) )

           ENDDO
           ENDDO
       ENDDO

       !
       ! dOmega/dW(k) = 4 * \Sum_b wb * (  A[R] - S[T] )
       !
       domg(:,:,ik) = fact * ( aux1(:,:) + aux2(:,:) )
   ENDDO

   DEALLOCATE( qkb, STAT=ierr )
       IF( ierr /=0 ) CALL errore('domega', 'deallocating qkb', ABS(ierr) )
   DEALLOCATE( R, Rt, T, STAT=ierr )
       IF( ierr /=0 ) CALL errore('domega', 'deallocating R, Rt, T', ABS(ierr) )
   DEALLOCATE( aux1, aux2, STAT=ierr )
       IF( ierr /=0 ) CALL errore('domega', 'deallocating aux1, aux2', ABS(ierr) )

   CALL timing('domega',OPR='stop')
END SUBROUTINE domega

