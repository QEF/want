      SUBROUTINE omega( nbands, nkpts, nkpts2, nntot, nnmx, nnlist, bk, wb, cm,       &
                 csheet, sheet, rave, r2ave, func_om1, func_om2, func_om3, func_o)
      
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
      REAL*8 :: wb(nkpts,nnmx)
      REAL*8 :: bk(3,nkpts,nnmx), rave(3,nbands), rtot(3)
      REAL*8 :: r2ave(nbands), rave2(nbands), bim(3)
      REAL*8 :: sheet(nbands,nkpts,nnmx)
      COMPLEX*16 :: csumt(3)
      COMPLEX*16 :: cm(nbands,nbands,nkpts,nnmx)

      COMPLEX*16 :: csheet(nbands,nkpts,nnmx)
      COMPLEX*16 :: craven(3,nbands), crtotn(3)
      REAL*8 :: r2aven(nbands), rave2n(nbands)

      REAL*8 :: func_om1 
      REAL*8 :: func_om2 
      REAL*8 :: func_om3 
      REAL*8 :: func_i
      REAL*8 :: func_od
      REAL*8 :: func_d 
      REAL*8 :: func_o
      REAL*8 :: func_om
      REAL*8 :: r2tot
      REAL*8 :: sqim 
      REAL*8 :: bim2
      REAL*8 :: func_n 
      REAL*8 :: sum 
      REAL*8 :: brn

      DO nwann = 1, nbands
        DO ind = 1, 3
          rave(ind,nwann) = 0.d0 
          DO nkp = 1, nkpts2
            DO nn = 1, nntot(nkp)
              rave(ind,nwann) = rave(ind,nwann) + wb(nkp,nn) * bk(ind,nkp,nn) *      &
                  ( AIMAG(LOG( csheet(nwann,nkp,nn) * cm(nwann,nwann,nkp,nn) ) )     &
                   - sheet(nwann,nkp,nn) )
              ! WRITE(6, FMT="( ' DEBUG 1 omega: ', 10D18.10)" ) rave(ind,nwann),      &
              !       wb(nkp,nn), bk(ind,nkp,nn), csheet(nwann,nkp,nn), cm(nwann,nwann,nkp,nn)
            END DO
          END DO
          rave(ind,nwann) = -rave(ind,nwann) / DBLE(nkpts2)
        END DO
      END DO

      ! WRITE (6, fmt="( ' DEBUG 2 omega: ', 3D18.10)" ) rave

      DO nwann = 1, nbands
        rave2(nwann) = 0.d0
        DO ind = 1, 3
          rave2(nwann) = rave2(nwann) + rave(ind,nwann)**2
        END DO
      END DO
      DO ind = 1, 3
        rtot(ind) = 0.d0
        DO nwann = 1, nbands
          rtot(ind) = rtot(ind) + rave(ind,nwann)
        END DO
      END DO

      DO nwann = 1, nbands
        r2ave(nwann) = 0.d0 
        DO nkp = 1, nkpts2
          DO nn = 1, nntot(nkp)
            r2ave(nwann) = r2ave(nwann) + wb(nkp,nn) * ( 1.d0 -                      &
                 cm(nwann,nwann,nkp,nn) * CONJG( cm(nwann,nwann,nkp,nn) ) +          &
                 ( AIMAG( LOG( csheet(nwann,nkp,nn) * cm(nwann,nwann,nkp,nn) ) ) -   &
                 sheet(nwann,nkp,nn) )**2 )
          END DO
        END DO
        r2ave(nwann) = r2ave(nwann) / DBLE(nkpts2)
        write(*,1000) nwann,( rave(ind,nwann), ind=1,3 ), r2ave(nwann) - rave2(nwann)
      END DO

      r2tot = 0.d0
      DO nwann=1,nbands
        r2tot = r2tot + r2ave(nwann) - rave2(nwann)
      END DO
      WRITE(*,*) ' '
      WRITE(*,1001) (rtot(ind),ind=1,3), r2tot
      WRITE(*,*) ' '
1000  format( 1x, 'Wannier center and spread', i3, 2x, '(',f10.6,',',f10.6,',',f10.6,')', f15.8 )
1001  format( 1x, 'Sum of centers and spreads',    4x, '(',f10.6,',',f10.6,',',f10.6,')', f15.8 )


      DO nwann = 1, nbands
        DO ind = 1, 3
          craven(ind,nwann) = 0.d0 
          DO nkp = 1, nkpts2
            DO nn = 1, nntot(nkp)
              craven(ind,nwann) = craven(ind,nwann) + wb(nkp,nn) * bk(ind,nkp,nn) * cm(nwann,nwann,nkp,nn)
            END DO
          END DO
          craven(ind,nwann) = ( 0.d0, 1.d0 ) * craven(ind,nwann) / DBLE(nkpts2)
        END DO
      END DO

      DO nwann = 1, nbands
        rave2n(nwann) = 0.d0
        DO ind = 1, 3
          rave2n(nwann) = rave2n(nwann) + craven(ind,nwann) * CONJG( craven(ind,nwann) )
        END DO
      END DO

      DO ind = 1, 3
        crtotn(ind) = 0.d0
        DO nwann = 1, nbands
          crtotn(ind) = crtotn(ind) + craven(ind,nwann)
        END DO
      END DO
      

      func_om = 0.d0
      DO nwann = 1, nbands
        rave2(nwann) = 0.d0
        DO i = 1, 3
          rave2(nwann) = rave2(nwann) + rave(i,nwann) * rave(i,nwann)
        END DO
        func_om = func_om + r2ave(nwann) - rave2(nwann)
      END DO

      func_om1 = 0.d0
      DO nkp = 1, nkpts2
        DO nn = 1, nntot(nkp)
          DO nwann = 1, nbands
            func_om1 = func_om1 + wb(nkp,nn) *                                    &
            ( 1.d0 - cm(nwann,nwann,nkp,nn) * CONJG( cm(nwann,nwann,nkp,nn) ) )
          END DO
        END DO
      END DO
      func_om1 = func_om1 / DBLE(nkpts2)

      func_om2 = 0.d0
      DO nwann = 1, nbands
        sqim = 0.d0
        DO nkp = 1, nkpts2
          DO nn = 1, nntot(nkp)
            sqim = sqim + wb(nkp,nn) * ( ( AIMAG( LOG( csheet(nwann,nkp,nn) *     &
            cm(nwann,nwann,nkp,nn) ) ) - sheet(nwann,nkp,nn) )**2 )
          END DO
        END DO
        sqim = sqim / DBLE(nkpts2)
        func_om2 = func_om2 + sqim
      END DO

      func_om3 = 0.d0
      DO nwann = 1, nbands
        DO ind = 1, 3
          bim(ind)= 0.d0
          DO nkp = 1, nkpts2
            DO nn = 1, nntot(nkp)
              bim(ind) = bim(ind) + wb(nkp,nn) * bk(ind,nkp,nn) *             &
              ( AIMAG( LOG( csheet(nwann,nkp,nn) * cm(nwann,nwann,nkp,nn) ) ) &
               -sheet(nwann,nkp,nn) )
            END DO
          END DO
          bim(ind) = bim(ind) / DBLE(nkpts2)
        END DO
        bim2 = 0.d0 
        DO ind = 1, 3
          bim2 = bim2 + bim(ind) * bim(ind)
        END DO
        func_om3 = func_om3 - bim2
      END DO

      func_n = 0.d0
      DO m = 1, nbands
        DO n= 1, nbands
          IF (n /= m ) THEN
            DO nkp = 1, nkpts2
              DO nn = 1, nntot(nkp)
                func_n = func_n + wb(nkp,nn) * CONJG( cm(m,n,nkp,nn) ) * cm(m,n,nkp,nn)
              END DO
            END DO
          END IF
        END DO
      END DO
      func_n = func_n / DBLE(nkpts2)
      WRITE(*,*) '------ func_n', func_n

      func_n = 0.d0
      DO m = 1, nbands
        DO n = 1, nbands
          IF ( n /= m ) THEN
            DO nkp = 1, nkpts2
              sum = 0.d0
              DO ind = 1, 3
                csumt(ind) = 0.d0
                DO nn= 1, nntot(nkp)
                  csumt(ind) = csumt(ind)+ wb(nkp,nn) * bk(ind,nkp,nn) * cm(m,n,nkp,nn)
                END DO
                sum = sum + csumt(ind) * CONJG( csumt(ind) )
              END DO
              func_n = func_n + sum
            END DO
          END IF
        END DO
      END DO
      func_n = func_n / DBLE(nkpts2)
      WRITE(*,*) '------ func_n', func_n
      

      func_n = 0.d0
      DO m = 1, nbands
        DO n= 1, nbands
          DO nkp = 1, nkpts2
            sum = 0.d0
            DO ind = 1, 3
              csumt(ind) = 0.d0
              DO nn = 1, nntot(nkp)
                IF ( m ==n ) THEN
                  csumt(ind) = csumt(ind) + wb(nkp,nn) * bk(ind,nkp,nn) * ( 0.d0, 1.d0 )* &
     &            ( AIMAG( LOG( csheet(n,nkp,nn) * cm(n,n,nkp,nn) ) ) - sheet(n,nkp,nn) )
                ELSE
                  csumt(ind) = csumt(ind) + wb(nkp,nn) * bk(ind,nkp,nn) * cm(m,n,nkp,nn)
                END IF
              END DO
              sum = sum + csumt(ind) * CONJG(csumt(ind))
            END DO
            func_n = func_n + sum
          END DO
        END DO
      END DO
      func_n = func_n / DBLE(nkpts2)

      func_i = 0.d0
      DO n = 1, nbands
        func_n = func_n - rave2(n)
        func_i = func_i + r2ave(n)
      END DO
      func_i = func_i - func_n
      WRITE(*,1029) func_i
      WRITE(*,1030) func_n

      WRITE(*,1005) func_om1
      WRITE(*,1006) func_om2
      WRITE(*,1007) func_om3
      WRITE(*,1008) func_om2+func_om3
      WRITE(*,*) ' '
1005  format( 1x,'Omega1   is   ', f15.9 )
1006  format( 1x,'Omega2   is   ', f15.9 )
1007  format( 1x,'Omega3   is   ', f15.9 )
1008  format( 1x,'Omega2+3 is   ', f15.9 )
1029  format( 1x,'Omega_nI is   ', f15.9 )
1030  format( 1x,'Omega_n~ is   ', f15.9 )

      func_i = 0.d0
      DO nkp = 1, nkpts2
        DO nn = 1, nntot(nkp)
          sum = 0.d0
          DO m = 1, nbands
            DO n = 1, nbands
              sum = sum + cm(m,n,nkp,nn) * CONJG( cm(m,n,nkp,nn) )
            END DO
          END DO
          func_i = func_i + wb(nkp,nn) * ( DBLE(nbands)-sum )
        END DO
      END DO
      func_i = func_i / DBLE(nkpts2)

      func_od = 0.d0
      DO nkp = 1, nkpts2
        DO nn = 1, nntot(nkp)
          sum = 0.d0
          DO m = 1, nbands
            DO n = 1, nbands
              sum = sum + wb(nkp,nn) * cm(m,n,nkp,nn) * CONJG( cm(m,n,nkp,nn) )
              IF ( m == n ) sum = sum - wb(nkp,nn) * cm(m,n,nkp,nn) * CONJG( cm(m,n,nkp,nn) )
            END DO
          END DO
          func_od = func_od + sum
        END DO
      END DO
      func_od = func_od / DBLE(nkpts2)

      func_d = 0.d0
      DO nkp = 1, nkpts2
        DO nn = 1, nntot(nkp)
          sum = 0.d0
          DO n = 1, nbands
            brn = 0.d0
            DO ind = 1, 3
              brn = brn + bk(ind,nkp,nn) * rave(ind,n)
            END DO
            sum = sum + wb(nkp,nn) * ( AIMAG( LOG( csheet(n,nkp,nn) * cm(n,n,nkp,nn) ) ) &
                  - sheet(n,nkp,nn) + brn )**2
          END DO
          func_d=func_d+sum
        END DO
      END DO
      func_d = func_d / DBLE(nkpts2)

      WRITE (*,1010) func_i
      WRITE (*,1011) func_d
      WRITE (*,1012) func_od
      WRITE (*,1014) func_od + func_d
      WRITE (*,*) ' '
      func_o = func_i + func_d + func_od
      WRITE(*,1013) func_o
      WRITE(*,*) ' '
1010  FORMAT ( 1x, 'Omega I  is   ', f25.19 )
1011  FORMAT ( 1x, 'Omega D  is   ', f15.9 )
1012  FORMAT ( 1x, 'Omega OD is   ', f15.9 )
1014  FORMAT ( 1x, 'Omegatld is   ', f15.9 )
1013  FORMAT ( 1x, 'Omega    is   ', f15.9 )

      RETURN
      END SUBROUTINE
