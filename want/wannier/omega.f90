!
! Copyright (C) 2004 Arrigo Calzolari, Carlo Cavazzoni, Marco Buongiorno Nardelli
! Copyright (C) 2002 Nicola Marzari, Ivo Souza, David Vanderbilt
! Copyright (C) 1997 Nicola Marzari, David Vanderbilt
!
! This file is distributed under the terms of the
! GNU General Public License. See the file `License'
! in the root directory of the present distribution,
! or http://www.gnu.org/copyleft/gpl.txt .
!
!=-------------------------------------------------------------------------------------=
      SUBROUTINE omega( nbands, nkpts, nkpts2, nntot, nnmx, nnlist, bk, wb, cm,           &
                 csheet, sheet, rave, r2ave, rave2, func_om1, func_om2, func_om3, func_o, &
                 rtot, r2tot, func_i, func_d, func_od, func_v )
!=--------------------------------------------------------------------------------------=
      
      USE kinds
      USE constants, ONLY : ZERO, ONE, CI 
      USE io_module, ONLY : stdout

      IMPLICIT NONE

      INTEGER :: nbands
      INTEGER :: nkpts 
      INTEGER :: nkpts2 
      INTEGER :: nnmx 
      INTEGER :: nwann
      INTEGER :: ind
      INTEGER :: nkp
      INTEGER :: nn
      INTEGER :: i, m, n 


      INTEGER :: nnlist(nkpts,nnmx)
      INTEGER :: nntot(nkpts)
      REAL(dbl) :: wb(nkpts,nnmx)
      REAL(dbl) :: bk(3,nkpts,nnmx), rave(3,nbands), rtot(3)
      REAL(dbl) :: r2ave(nbands), rave2(nbands), bim(3)
      REAL(dbl) :: sheet(nbands,nkpts,nnmx)
      COMPLEX(dbl) :: csumt(3)
      COMPLEX(dbl) :: cm(nbands,nbands,nnmx,nkpts)

      COMPLEX(dbl) :: csheet(nbands,nkpts,nnmx)
      COMPLEX(dbl) :: craven(3,nbands), crtotn(3)
      REAL(dbl) :: r2aven(nbands), rave2n(nbands)

      REAL(dbl) :: func_om1 
      REAL(dbl) :: func_om2 
      REAL(dbl) :: func_om3 
      REAL(dbl) :: func_i
      REAL(dbl) :: func_od
      REAL(dbl) :: func_d 
      REAL(dbl) :: func_o
      REAL(dbl) :: func_v
      REAL(dbl) :: func_om
      REAL(dbl) :: r2tot
      REAL(dbl) :: sqim 
      REAL(dbl) :: bim2
      REAL(dbl) :: func_n 
      REAL(dbl) :: sum 
      REAL(dbl) :: brn

      DO nwann = 1, nbands
        DO ind = 1, 3
          rave(ind,nwann) = ZERO
          DO nkp = 1, nkpts2
            DO nn = 1, nntot(nkp)
              rave(ind,nwann) = rave(ind,nwann) + wb(nkp,nn) * bk(ind,nkp,nn) *      &
                  ( AIMAG(LOG( csheet(nwann,nkp,nn) * cm(nwann,nwann,nn,nkp) ) )     &
                   - sheet(nwann,nkp,nn) )
            END DO
          END DO
          rave(ind,nwann) = -rave(ind,nwann) / DBLE(nkpts2)
        END DO
      END DO

      DO nwann = 1, nbands
        rave2(nwann) = ZERO
        DO ind = 1, 3
          rave2(nwann) = rave2(nwann) + rave(ind,nwann)**2
        END DO
      END DO
      DO ind = 1, 3
        rtot(ind) = ZERO
        DO nwann = 1, nbands
          rtot(ind) = rtot(ind) + rave(ind,nwann)
        END DO
      END DO

      DO nwann = 1, nbands
        r2ave(nwann) = ZERO
        DO nkp = 1, nkpts2
          DO nn = 1, nntot(nkp)
            r2ave(nwann) = r2ave(nwann) + wb(nkp,nn) * ( ONE -                       &
                 cm(nwann,nwann,nn,nkp) * CONJG( cm(nwann,nwann,nn,nkp) ) +          &
                 ( AIMAG( LOG( csheet(nwann,nkp,nn) * cm(nwann,nwann,nn,nkp) ) ) -   &
                 sheet(nwann,nkp,nn) )**2 )
          END DO
        END DO
        r2ave(nwann) = r2ave(nwann) / DBLE(nkpts2)
      END DO

      r2tot = ZERO
      DO nwann=1,nbands
        r2tot = r2tot + r2ave(nwann) - rave2(nwann)
      END DO

      DO nwann = 1, nbands
        DO ind = 1, 3
          craven(ind,nwann) = ZERO
          DO nkp = 1, nkpts2
            DO nn = 1, nntot(nkp)
              craven(ind,nwann) = craven(ind,nwann) + wb(nkp,nn) * & 
                                  bk(ind,nkp,nn) * cm(nwann,nwann,nn,nkp)
            END DO
          END DO
          craven(ind,nwann) = CI * craven(ind,nwann) / DBLE(nkpts2)
        END DO
      END DO

      DO nwann = 1, nbands
        rave2n(nwann) = ZERO
        DO ind = 1, 3
          rave2n(nwann) = rave2n(nwann) + craven(ind,nwann) * CONJG( craven(ind,nwann) )
        END DO
      END DO

      DO ind = 1, 3
        crtotn(ind) = ZERO
        DO nwann = 1, nbands
          crtotn(ind) = crtotn(ind) + craven(ind,nwann)
        END DO
      END DO
      

      func_om = ZERO
      DO nwann = 1, nbands
        rave2(nwann) = ZERO
        DO i = 1, 3
          rave2(nwann) = rave2(nwann) + rave(i,nwann) * rave(i,nwann)
        END DO
        func_om = func_om + r2ave(nwann) - rave2(nwann)
      END DO

      func_om1 = ZERO
      DO nkp = 1, nkpts2
        DO nn = 1, nntot(nkp)
          DO nwann = 1, nbands
            func_om1 = func_om1 + wb(nkp,nn) *                                    &
            ( ONE - cm(nwann,nwann,nn,nkp) * CONJG( cm(nwann,nwann,nn,nkp) ) )
          END DO
        END DO
      END DO
      func_om1 = func_om1 / DBLE(nkpts2)

      func_om2 = ZERO
      DO nwann = 1, nbands
        sqim = ZERO
        DO nkp = 1, nkpts2
          DO nn = 1, nntot(nkp)
            sqim = sqim + wb(nkp,nn) * ( ( AIMAG( LOG( csheet(nwann,nkp,nn) *     &
            cm(nwann,nwann,nn,nkp) ) ) - sheet(nwann,nkp,nn) )**2 )
          END DO
        END DO
        sqim = sqim / DBLE(nkpts2)
        func_om2 = func_om2 + sqim
      END DO

      func_om3 = ZERO
      DO nwann = 1, nbands
        DO ind = 1, 3
          bim(ind)= ZERO
          DO nkp = 1, nkpts2
            DO nn = 1, nntot(nkp)
              bim(ind) = bim(ind) + wb(nkp,nn) * bk(ind,nkp,nn) *             &
              ( AIMAG( LOG( csheet(nwann,nkp,nn) * cm(nwann,nwann,nn,nkp) ) ) &
               -sheet(nwann,nkp,nn) )
            END DO
          END DO
          bim(ind) = bim(ind) / DBLE(nkpts2)
        END DO
        bim2 = ZERO
        DO ind = 1, 3
          bim2 = bim2 + bim(ind) * bim(ind)
        END DO
        func_om3 = func_om3 - bim2
      END DO

      func_n = ZERO
      DO m = 1, nbands
        DO n= 1, nbands
          IF (n /= m ) THEN
            DO nkp = 1, nkpts2
              DO nn = 1, nntot(nkp)
                func_n = func_n + wb(nkp,nn) * CONJG( cm(m,n,nn,nkp) ) * cm(m,n,nn,nkp)
              END DO
            END DO
          END IF
        END DO
      END DO
      func_n = func_n / DBLE(nkpts2)

      func_n = ZERO
      DO m = 1, nbands
        DO n = 1, nbands
          IF ( n /= m ) THEN
            DO nkp = 1, nkpts2
              sum = ZERO
              DO ind = 1, 3
                csumt(ind) = ZERO
                DO nn= 1, nntot(nkp)
                  csumt(ind) = csumt(ind)+ wb(nkp,nn) * bk(ind,nkp,nn) * cm(m,n,nn,nkp)
                END DO
                sum = sum + csumt(ind) * CONJG( csumt(ind) )
              END DO
              func_n = func_n + sum
            END DO
          END IF
        END DO
      END DO
      func_n = func_n / DBLE(nkpts2)
      

      func_n = ZERO
      DO m = 1, nbands
        DO n= 1, nbands
          DO nkp = 1, nkpts2
            sum = ZERO
            DO ind = 1, 3
              csumt(ind) = ZERO
              DO nn = 1, nntot(nkp)
                IF ( m ==n ) THEN
                  csumt(ind) = csumt(ind) + wb(nkp,nn) * bk(ind,nkp,nn) * CI * &
     &            ( AIMAG( LOG( csheet(n,nkp,nn) * cm(n,n,nn,nkp) ) ) - sheet(n,nkp,nn) )
                ELSE
                  csumt(ind) = csumt(ind) + wb(nkp,nn) * bk(ind,nkp,nn) * cm(m,n,nn,nkp)
                END IF
              END DO
              sum = sum + csumt(ind) * CONJG(csumt(ind))
            END DO
            func_n = func_n + sum
          END DO
        END DO
      END DO
      func_n = func_n / DBLE(nkpts2)

      func_i = ZERO
      DO n = 1, nbands
        func_n = func_n - rave2(n)
        func_i = func_i + r2ave(n)
      END DO
      func_i = func_i - func_n

      func_i = ZERO
      DO nkp = 1, nkpts2
        DO nn = 1, nntot(nkp)
          sum = ZERO
          DO m = 1, nbands
            DO n = 1, nbands
              sum = sum + cm(m,n,nn,nkp) * CONJG( cm(m,n,nn,nkp) )
            END DO
          END DO
          func_i = func_i + wb(nkp,nn) * ( DBLE(nbands)-sum )
        END DO
      END DO
      func_i = func_i / DBLE(nkpts2)

      func_od = ZERO
      DO nkp = 1, nkpts2
        DO nn = 1, nntot(nkp)
          sum = ZERO
          DO m = 1, nbands
            DO n = 1, nbands
              sum = sum + wb(nkp,nn) * cm(m,n,nn,nkp) * CONJG( cm(m,n,nn,nkp) )
              IF ( m == n ) sum = sum - wb(nkp,nn) * cm(m,n,nn,nkp) * CONJG( cm(m,n,nn,nkp) )
            END DO
          END DO
          func_od = func_od + sum
        END DO
      END DO
      func_od = func_od / DBLE(nkpts2)

      func_d = ZERO
      DO nkp = 1, nkpts2
        DO nn = 1, nntot(nkp)
          sum = ZERO
          DO n = 1, nbands
            brn = ZERO
            DO ind = 1, 3
              brn = brn + bk(ind,nkp,nn) * rave(ind,n)
            END DO
            sum = sum + wb(nkp,nn) * ( AIMAG( LOG( csheet(n,nkp,nn) * cm(n,n,nn,nkp) ) ) &
                  - sheet(n,nkp,nn) + brn )**2
          END DO
          func_d=func_d+sum
        END DO
      END DO
      func_d = func_d / DBLE(nkpts2)

      func_o = func_i + func_d + func_od
      func_v = func_d + func_od

      RETURN
      END SUBROUTINE
