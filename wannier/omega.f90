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
   USE kinds
   USE constants, ONLY : ZERO, ONE, CI 
   USE io_module, ONLY : stdout
   USE timing_module, ONLY : timing
   USE kpoints_module, ONLY : nb, vb, wb
   IMPLICIT NONE

   !
   ! input variables
   !
   INTEGER,      INTENT(in) :: dimwann, nkpts
   REAL(dbl),    INTENT(in) :: sheet(dimwann,nb,nkpts)
   COMPLEX(dbl), INTENT(in) :: csheet(dimwann,nb,nkpts)
   COMPLEX(dbl), INTENT(in) :: Mkb(dimwann,dimwann,nb,nkpts)

   REAL(dbl), INTENT(out) :: r2ave(dimwann), rave2(dimwann), rave(3,dimwann)
   REAL(dbl), INTENT(out) :: Omega_D, Omega_OD

   !
   ! local variables
   !
   INTEGER   :: ik, ib
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


!
! computing wannier centers and spreads
!

      !
      ! first compute the auxiliary quantity
      ! aux(m,ib,ik) =  Im Log (Mkb(m,m))
      !
      ALLOCATE( aux(dimwann,nb,nkpts), STAT=ierr)
         IF (ierr/=0) CALL errore('omega','allocating aux', ABS(ierr))
     
      DO ik = 1, nkpts
      DO ib = 1, nb
           DO m = 1, dimwann
                !
                aux(m,ib,ik) = AIMAG(LOG( csheet(m,ib,ik) * Mkb(m,m,ib,ik) ) ) &
                               - sheet(m,ib,ik) 
           ENDDO
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

      DO ik = 1, nkpts
      DO ib = 1, nb
           !
           !
           DO m = 1, dimwann
                !
                r2ave(m) = r2ave(m) + wb(ib) *  &
                    ( ONE - REAL( Mkb(m,m,ib,ik) * CONJG( Mkb(m,m,ib,ik)) ) + &
                    aux(m,ib,ik) ** 2 )
                    !
                rave(:,m) = rave(:,m) + wb(ib) * aux(m,ib,ik) * vb(:,ib) 
           ENDDO
      ENDDO
      ENDDO
      !
      rave(:,:) = - rave(:,:) / REAL(nkpts, dbl)
      r2ave(:) =     r2ave(:) / REAL(nkpts, dbl)

      !
      ! compute | <r_n> |^2
      !
      DO m = 1, dimwann
          rave2(m) = rave(1,m)**2 + rave(2,m)**2 + rave(3,m)**2
      ENDDO


!
! computing the functionals
!

      !
      ! Omega_OD = 1/Nk \Sum_{ik, nn} wb(nn) * \Sum_{m/=n} | Mkb(m,n,nn,ik) |^2
      !
      Omega_OD = ZERO
      DO ik = 1, nkpts
      DO ib = 1, nb

           rtmp = ZERO
           DO n = 1, dimwann
                DO m = 1, dimwann
                    rtmp = rtmp + REAL( Mkb(m,n,ib,ik)*CONJG( Mkb(m,n,ib,ik)), dbl )
                ENDDO
                !
                ! eliminate the diagonal term
                rtmp = rtmp - REAL( Mkb(n,n,ib,ik) * CONJG( Mkb(n,n,ib,ik)), dbl)
           ENDDO
           Omega_OD = Omega_OD + wb(ib) * rtmp
      ENDDO
      ENDDO
      Omega_OD = Omega_OD / REAL(nkpts, dbl)


      !
      ! func_D =  1/Nk \Sum_{ik, nn} wb(nn) * \Sum_m ( Im Log Mkb(m,m) + b * <r>_m )**2
      !
      Omega_D = ZERO
      DO ik = 1, nkpts
      DO ib = 1, nb
           rtmp = ZERO
           DO m = 1, dimwann
                 rtmp1 = DOT_PRODUCT( vb(:, ib), rave(:, m) )
                 rtmp = rtmp + ( aux(m,ib,ik) + rtmp1 ) **2
           ENDDO
           Omega_D = Omega_D + wb(ib) * rtmp
      ENDDO
      ENDDO
      Omega_D = Omega_D / REAL(nkpts, dbl)


      !
      ! cleaning
      !
      DEALLOCATE( aux, STAT=ierr)
         IF (ierr/=0) CALL errore('omega','deallocating aux', ABS(ierr))

      CALL timing('omega',OPR='stop')
   END SUBROUTINE omega


