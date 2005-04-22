!
! Copyright (C) 2004 WanT Group
! Copyright (C) 2002 Nicola Marzari, Ivo Souza, David Vanderbilt
! Copyright (C) 1997 Nicola Marzari, David Vanderbilt
!
! This file is distributed under the terms of the
! GNU General Public License. See the file `License'
! in the root directory of the present distribution,
! or http://www.gnu.org/copyleft/gpl.txt .
!
!********************************************************
   SUBROUTINE omega( dimwann, nkpts, Mkb, csheet, sheet,  &
                     rave, r2ave, rave2, func_om1, func_om2, func_om3 )
   !********************************************************
   USE kinds
   USE constants, ONLY : ZERO, ONE, CI 
   USE io_module, ONLY : stdout
   USE timing_module, ONLY : timing
   USE kpoints_module, ONLY : nnx, nntot, nnlist, bk, wb
   USE localization_module, ONLY : Omega_I, Omega_OD, Omega_D, Omega_V, Omega_tot
   IMPLICIT NONE

   !
   ! input variables
   !
   INTEGER,      INTENT(in) :: dimwann, nkpts
   REAL(dbl),    INTENT(in) :: sheet(dimwann,nkpts,*)
   COMPLEX(dbl), INTENT(in) :: Mkb(dimwann,dimwann,nnx,nkpts)
   COMPLEX(dbl), INTENT(in) :: csheet(dimwann,nkpts,*)

   REAL(dbl), INTENT(out) :: r2ave(dimwann), rave2(dimwann), rave(3,dimwann)
   REAL(dbl), INTENT(out) :: func_om1, func_om2, func_om3 

   !
   ! local variables
   !
   REAL(dbl) :: aux(3), rtmp
   INTEGER :: iwann, ik, inn
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
      ! compute <r(i)_n> = -1/Nk \Sum{k,b} wb * b(i) * Im Mkb(n,n)
      !
      DO iwann = 1, dimwann
      DO i = 1, 3
           rave(i,iwann) = ZERO

           DO ik = 1, nkpts
           DO inn = 1, nntot(ik)
                 rave(i,iwann) = rave(i,iwann) + wb(ik,inn) * bk(i,ik,inn) *        &
                     ( AIMAG(LOG( csheet(iwann,ik,inn) * Mkb(iwann,iwann,inn,ik) ) ) &
                     - sheet(iwann,ik,inn) )
           ENDDO
           ENDDO
           rave(i,iwann) = - rave(i,iwann) / DBLE(nkpts)
      ENDDO
      ENDDO

      !
      ! compute <r>^2
      !
      DO iwann = 1, dimwann
          rave2(iwann) = rave(1,iwann)**2 + rave(2,iwann)**2 + rave(3,iwann)**2
      ENDDO

      !
      ! compute <r^2_n> = -1/Nk \Sum{k,b} wb * [ 1 - |Mkb(n,n)|^2 + ( Im Log (Mkb(n,n)) )^2 ]
      !
      DO iwann = 1, dimwann
           r2ave(iwann) = ZERO

           DO ik = 1, nkpts
           DO inn = 1, nntot(ik)
               r2ave(iwann) = r2ave(iwann) + wb(ik,inn) * ( ONE -                     &
                      Mkb(iwann,iwann,inn,ik) * CONJG( Mkb(iwann,iwann,inn,ik) ) +      &
                    ( AIMAG( LOG( csheet(iwann,ik,inn) * Mkb(iwann,iwann,inn,ik) ) )   &
                     -sheet(iwann,ik,inn) )**2 )
           ENDDO
           ENDDO
           r2ave(iwann) = r2ave(iwann) / DBLE(nkpts)
      ENDDO


!
! computing the functionals
!

      !
      ! func_OM1 = 1/Nk \Sum_{ik, nn} \Sum_i   wb(ik,nn) * ( 1 - | Mkb(i,i,nn,ik) |^2 )
      !
      func_om1 = ZERO
      DO ik = 1, nkpts
      DO inn = 1, nntot(ik)
          DO iwann = 1, dimwann
              func_om1 = func_om1 + wb(ik,inn) *                                    &
                 ( ONE - REAL( Mkb(iwann,iwann,inn,ik) * CONJG(Mkb(iwann,iwann,inn,ik)) ) )
          ENDDO
      ENDDO
      ENDDO
      func_om1 = func_om1 / DBLE(nkpts)


      !
      ! func_OM2 = 1/Nk \Sum_{ik, nn} \Sum_i   wb(ik,nn) * Im( Log( csheet * Mkb) - sheet )
      !
      func_om2 = ZERO
      DO ik = 1, nkpts
      DO inn = 1, nntot(ik)
          DO iwann = 1, dimwann
               func_om2 = func_om2 + wb(ik,inn) * ( ( AIMAG( LOG( csheet(iwann,ik,inn) * &
                          Mkb(iwann,iwann,inn,ik) ) ) - sheet(iwann,ik,inn) )**2 )
          ENDDO
      ENDDO
      ENDDO
      func_om2 = func_om2 / DBLE(nkpts)


      !
      ! func_OM3 = 1/Nk \Sum_i  \Sum_{ik, nn}  wb(ik,nn) * | bk |^2 
      !
      func_om3 = ZERO
      DO iwann = 1, dimwann
           DO i = 1, 3
               aux(i)= ZERO
               DO ik = 1, nkpts
               DO inn = 1, nntot(ik)
                   aux(i) = aux(i) + wb(ik,inn) * bk(i,ik,inn) *             &
                            ( AIMAG( LOG( csheet(iwann,ik,inn) * Mkb(iwann,iwann,inn,ik) )) &
                            - sheet(iwann,ik,inn) )
               ENDDO
               ENDDO
           ENDDO
           func_om3 = func_om3 - ( aux(1)**2 + aux(2)**2 + aux(3)**2 )
      ENDDO
      func_om3 = func_om3 / DBLE(nkpts)**2


      !
      ! Omega_I
      !
      Omega_I = ZERO
      DO ik = 1, nkpts
      DO inn = 1, nntot(ik)
          rtmp = ZERO
          DO n = 1, dimwann
          DO m = 1, dimwann
              rtmp = rtmp + Mkb(m,n,inn,ik) * CONJG( Mkb(m,n,inn,ik) )
          ENDDO
          ENDDO
          Omega_I = Omega_I + wb(ik,inn) * ( DBLE(dimwann)- rtmp )
      ENDDO
      ENDDO
      Omega_I = Omega_I / DBLE(nkpts)


      !
      ! func_OD
      !
      Omega_OD = ZERO
      DO ik = 1, nkpts
      DO inn = 1, nntot(ik)
          DO n = 1, dimwann
              DO m = 1, dimwann
                 Omega_OD = Omega_OD + wb(ik,inn) * Mkb(m,n,inn,ik)*CONJG( Mkb(m,n,inn,ik) )
              ENDDO
              !
              ! eliminate the diagonal term
              Omega_OD = Omega_OD - wb(ik,inn) * Mkb(n,n,inn,ik) * CONJG( Mkb(n,n,inn,ik) )
          ENDDO
      ENDDO
      ENDDO
      Omega_OD = Omega_OD / DBLE(nkpts)


      !
      ! func_D
      !
      Omega_D = ZERO
      DO ik = 1, nkpts
      DO inn = 1, nntot(ik)
          DO n = 1, dimwann
               rtmp = DOT_PRODUCT( bk(:, ik, inn), rave(:, n) )
               Omega_D = Omega_D + wb(ik,inn) *  &
                               ( AIMAG( LOG( csheet(n,ik,inn) * Mkb(n,n,inn,ik) ) ) &
                               - sheet(n,ik,inn) + rtmp )**2
          ENDDO
      ENDDO
      ENDDO
      Omega_D = Omega_D / DBLE(nkpts)


      Omega_tot = Omega_I + Omega_D + Omega_OD
      Omega_V = Omega_D + Omega_OD


      CALL timing('omega',OPR='stop')
   END SUBROUTINE omega


