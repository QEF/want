!
! Copyright (C) 2004 WanT Group
!
! This file is distributed under the terms of the
! GNU General Public License. See the file `License'
! in the root directory of the present distribution,
! or http://www.gnu.org/copyleft/gpl.txt .
!
!********************************************************
   SUBROUTINE omega( dimwann, nkpts, Mkb, csheet, sheet,  &
                     rave, r2ave, rave2, Omega_D, Omega_OD )
   !********************************************************
   !
   ! This routine computes <r>, <r^2> values for each WF, 
   ! and then evaluate the components of the spread functionals
   ! \Omega_D, \Omega_OD . The last part of the spread, Omega_I
   ! is computed in omegai.f90
   !
   USE kinds
   USE constants,        ONLY : ZERO, ONE, TWO, CI 
   USE io_module,        ONLY : stdout
   USE timing_module,    ONLY : timing
   USE log_module,       ONLY : log_push, log_pop
   USE kpoints_module,   ONLY : nkpts_g, nb, vb, wb, nnpos
   USE mp,               ONLY : mp_sum
   !
   IMPLICIT NONE

   !
   ! input variables
   !
   INTEGER,      INTENT(in) :: dimwann, nkpts
   REAL(dbl),    INTENT(in) :: sheet(dimwann,nb,nkpts)
   COMPLEX(dbl), INTENT(in) :: csheet(dimwann,nb,nkpts)
   COMPLEX(dbl), INTENT(in) :: Mkb(dimwann,dimwann,nb/2,nkpts)
   !
   REAL(dbl),   INTENT(out) :: r2ave(dimwann), rave2(dimwann), rave(3,dimwann)
   REAL(dbl),   INTENT(out) :: Omega_D, Omega_OD

   !
   ! local variables
   !
   INTEGER   :: ik, ib, inn
   INTEGER   :: m, n, ierr 
   REAL(dbl) :: rtmp, rtmp1
   REAL(dbl), ALLOCATABLE :: aux(:,:,:)
   !
   ! end of declariations
   !

!
!------------------------------
! main body 
!------------------------------
!
   CALL timing('omega',OPR='start')
   CALL log_push('omega')


!
! computing wannier centers and spreads
!

   !
   ! first compute the auxiliary quantity
   ! aux(m,ib,ik) =  Im Log (Mkb(m,m))
   ! for the positive b-vectors only
   !
   ALLOCATE( aux(dimwann, nb/2, nkpts), STAT=ierr)
   IF (ierr/=0) CALL errore('omega','allocating aux', ABS(ierr))
  
   DO ik  = 1, nkpts
   DO inn = 1, nb / 2
       !
       ib = nnpos( inn )
       !
       DO m = 1, dimwann
            !
            aux(m,inn,ik) = AIMAG(LOG( csheet(m,ib,ik) * Mkb(m,m,inn,ik) ) ) &
                           - sheet(m,ib,ik) 
       ENDDO
       !
   ENDDO
   ENDDO


   !
   ! compute <r(i)_m> = -1/Nk \Sum{k,b} wb * b(i) * Im Log Mkb(m,m)
   !         <r^2_m>  = -1/Nk \Sum{k,b} wb * &
   !                         [ 1 - |Mkb(m,m)|^2 + ( Im Log (Mkb(m,m)) )^2 ]
   !
   ! note the use of csheet and sheet to get the right Riemann sheet for the 
   ! evaluation of the complex LOG
   !
   rave(:,:) = ZERO
   r2ave(:)  = ZERO

   DO ik  = 1, nkpts
   DO inn = 1, nb / 2
       !
       ib = nnpos( inn )
       !
       DO m = 1, dimwann
           !
           r2ave(m) = r2ave(m) + wb(ib) *  &
               ( ONE - REAL( Mkb(m,m,inn,ik) * CONJG( Mkb(m,m,inn,ik)) ) + &
               aux(m,inn,ik) ** 2 )
               !
           rave(:,m) = rave(:,m) + wb(ib) * aux(m,inn,ik) * vb(:,ib) 
           !
       ENDDO
       !
   ENDDO
   ENDDO
   !
   CALL mp_sum( rave ) 
   CALL mp_sum( r2ave ) 
   !
   rave(:,:) = - TWO * rave(:,:) / REAL(nkpts_g, dbl)
   r2ave(:) =    TWO * r2ave(:) / REAL(nkpts_g, dbl)

   !
   ! compute | <r_n> |^2
   !
   DO m = 1, dimwann
       !
       rave2(m) = rave(1,m)**2 + rave(2,m)**2 + rave(3,m)**2
       !
   ENDDO


!
! computing the functionals
!

   !
   ! Omega_OD = 1/Nk \Sum_{ik, nn} wb(nn) * \Sum_{m/=n} | Mkb(m,n,nn,ik) |^2
   !
   Omega_OD = ZERO
   !
   DO ik  = 1, nkpts
   DO inn = 1, nb / 2
       !
       ib   = nnpos( inn )
       rtmp = ZERO
       !
       DO n = 1, dimwann
           !
           DO m = 1, dimwann
               rtmp = rtmp + REAL( Mkb(m,n,inn,ik)*CONJG( Mkb(m,n,inn,ik)), dbl )
           ENDDO
           !
           ! eliminate the diagonal term
           rtmp = rtmp - REAL( Mkb(n,n,inn,ik) * CONJG( Mkb(n,n,inn,ik)), dbl)
           !
       ENDDO
       !
       Omega_OD = Omega_OD + wb(ib) * rtmp
       !
   ENDDO
   ENDDO
   !
   CALL mp_sum( Omega_OD )
   !
   Omega_OD = TWO * Omega_OD / REAL(nkpts_g, dbl)


   !
   ! Omega_D =  1/Nk \Sum_{ik, nn} wb(nn) * \Sum_m ( Im Log Mkb(m,m) + b * <r>_m )**2
   !
   Omega_D = ZERO
   !
   DO ik  = 1, nkpts
   DO inn = 1, nb / 2
       !
       ib   = nnpos( inn )
       rtmp = ZERO
       !
       DO m = 1, dimwann
           !
           rtmp1 = DOT_PRODUCT( vb(:, ib), rave(:, m) )
           rtmp = rtmp + ( aux(m,inn,ik) + rtmp1 ) **2
           !
       ENDDO
       !
       Omega_D = Omega_D + wb(ib) * rtmp
       !
   ENDDO
   ENDDO
   !
   CALL mp_sum( Omega_D )
   !
   Omega_D = TWO * Omega_D / REAL(nkpts_g, dbl)


   !
   ! cleaning
   !
   DEALLOCATE( aux, STAT=ierr)
   IF (ierr/=0) CALL errore('omega','deallocating aux', ABS(ierr))


   CALL timing('omega',OPR='stop')
   CALL log_pop('omega')
   !
END SUBROUTINE omega


