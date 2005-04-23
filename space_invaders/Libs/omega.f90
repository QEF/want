!
! Copyright (C) 2004 WanT Group
!
! This file is distributed under the terms of the
! GNU General Public License. See the file `License'
! in the root directory of the present distribution,
! or http://www.gnu.org/copyleft/gpl.txt .
!
! Based on a previous version by I.Souza, N.Marzari and D.Vanderbilt
!
!********************************************************
   SUBROUTINE omega( dimwann, nkpts, Mkb, csheet, sheet,  &
                     rave, r2ave, rave2, Omega_I, Omega_D, Omega_OD, Omega_tot )
   !********************************************************
   USE kinds
   USE constants, ONLY : ZERO, ONE, CI 
   USE io_module, ONLY : stdout
   USE timing_module, ONLY : timing
   USE kpoints_module, ONLY : nnx, nntot, nnlist, bk, wb
   IMPLICIT NONE

   !
   ! input variables
   !
   INTEGER,      INTENT(in) :: dimwann, nkpts
   REAL(dbl),    INTENT(in) :: sheet(dimwann,nkpts,*)
   COMPLEX(dbl), INTENT(in) :: Mkb(dimwann,dimwann,nnx,nkpts)
   COMPLEX(dbl), INTENT(in) :: csheet(dimwann,nkpts,*)

   REAL(dbl), INTENT(out) :: r2ave(dimwann), rave2(dimwann), rave(3,dimwann)
   REAL(dbl), INTENT(out) :: Omega_I, Omega_D, Omega_OD, Omega_tot

   !
   ! local variables
   !
   REAL(dbl) :: aux(3), rtmp, rtmp1
   INTEGER :: ik, inn
   INTEGER :: i, m, n 
   !
   ! end of declariations
   !

!
!------------------------------
! main body 
!------------------------------
!
      CALL timing('omega',OPR='start')


!
! computing wannier centers and spreads
!
      !
      ! compute <r(i)_m> = -1/Nk \Sum{k,b} wb * b(i) * Im Mkb(m,m)
      !
      DO m = 1, dimwann
      DO i = 1, 3
           rave(i,m) = ZERO

           DO ik = 1, nkpts
           DO inn = 1, nntot(ik)
                 rave(i,m) = rave(i,m) + wb(ik,inn) * bk(i,ik,inn) *     &
                     ( AIMAG(LOG( csheet(m,ik,inn) * Mkb(m,m,inn,ik) ) ) &
                     - sheet(m,ik,inn) )
           ENDDO
           ENDDO
           rave(i,m) = - rave(i,m) / DBLE(nkpts)
      ENDDO
      ENDDO
      !
      ! compute | <r_n> |^2
      !
      DO m = 1, dimwann
          rave2(m) = rave(1,m)**2 + rave(2,m)**2 + rave(3,m)**2
      ENDDO


      !
      ! compute <r^2_m> = -1/Nk \Sum{k,b} wb * &
      !                         [ 1 - |Mkb(m,m)|^2 + ( Im Log (Mkb(m,m)) )^2 ]
      !
      ! note the use of csheet and sheet to get the right Riemann sheet for the 
      ! evaluation of the complex LOG
      !
      DO m = 1, dimwann
           r2ave(m) = ZERO

           DO ik = 1, nkpts
           DO inn = 1, nntot(ik)
               r2ave(m) = r2ave(m) + wb(ik,inn) *  &
                    ( ONE - DBLE( Mkb(m,m,inn,ik) * CONJG( Mkb(m,m,inn,ik)) ) + &
                    ( AIMAG( LOG( csheet(m,ik,inn) * Mkb(m,m,inn,ik)) ) &
                    - sheet(m,ik,inn) )**2 )
           ENDDO
           ENDDO
           r2ave(m) = r2ave(m) / DBLE(nkpts)
      ENDDO


!
! computing the functionals
!


      !
      ! Omega_I = 1/Nk \Sum_{ik, nn} wb(ik,nn) * ( Nwann - \Sum_mn | Mkb(m,n,nn,ik) |^2 )
      !
      Omega_I = ZERO
      DO ik = 1, nkpts
      DO inn = 1, nntot(ik)
           rtmp = ZERO
           DO n = 1, dimwann
           DO m = 1, dimwann
                rtmp = rtmp + REAL( Mkb(m,n,inn,ik) * CONJG( Mkb(m,n,inn,ik) ) )
           ENDDO
           ENDDO
           Omega_I = Omega_I + wb(ik,inn) * ( DBLE(dimwann)- rtmp )
      ENDDO
      ENDDO
      Omega_I = Omega_I / DBLE(nkpts)


      !
      ! Omega_OD = 1/Nk \Sum_{ik, nn} wb(ik,nn) * \Sum_{m/=n} | Mkb(m,n,nn,ik) |^2
      !
      Omega_OD = ZERO
      DO ik = 1, nkpts
      DO inn = 1, nntot(ik)

           rtmp = ZERO
           DO n = 1, dimwann
                DO m = 1, dimwann
                    rtmp = rtmp + DBLE( Mkb(m,n,inn,ik)*CONJG( Mkb(m,n,inn,ik)) )
                ENDDO
                !
                ! eliminate the diagonal term
                rtmp = rtmp - DBLE ( Mkb(n,n,inn,ik) * CONJG( Mkb(n,n,inn,ik) ))
           ENDDO
           Omega_OD = Omega_OD + wb(ik,inn) * rtmp
      ENDDO
      ENDDO
      Omega_OD = Omega_OD / DBLE(nkpts)


      !
      ! func_D =  1/Nk \Sum_{ik, nn} wb(ik,nn) * \Sum_m ( Im Log Mkb(m,m) + b * <r>_m )**2
      !
      Omega_D = ZERO
      DO ik = 1, nkpts
      DO inn = 1, nntot(ik)
           rtmp = ZERO
           DO m = 1, dimwann
                 rtmp1 = DOT_PRODUCT( bk(:, ik, inn), rave(:, m) )
                 rtmp = rtmp + ( AIMAG( LOG( csheet(m,ik,inn) * Mkb(m,m,inn,ik) ) ) &
                               - sheet(m,ik,inn) + rtmp1 )**2
           ENDDO
           Omega_D = Omega_D + wb(ik,inn) * rtmp
      ENDDO
      ENDDO
      Omega_D = Omega_D / DBLE(nkpts)


      !
      ! he total functional
      !
      Omega_tot = Omega_I + Omega_D + Omega_OD


      CALL timing('omega',OPR='stop')
   END SUBROUTINE omega


