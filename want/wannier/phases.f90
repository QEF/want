      SUBROUTINE phases( nbands, nkpts, nkpts2, nnmx, nnmxh, nntot, nnh, neigh, &
                 bk, bka, cm, csheet, sheet, rguide, irguide )
      
      IMPLICIT NONE

      INTEGER :: nbands
      INTEGER :: nkpts
      INTEGER :: nkpts2
      INTEGER :: nnmx
      INTEGER :: nnmxh
      INTEGER :: nnh
      INTEGER :: irguide
      INTEGER :: nwann
      INTEGER :: na
      INTEGER :: nkp
      INTEGER :: nn
      INTEGER :: j
      INTEGER :: i
      INTEGER :: m
      INTEGER :: ind
      INTEGER :: n

      COMPLEX*16 :: cm(nbands,nbands,nkpts,nnmx)
      COMPLEX*16 :: csum(nnmxh)
      REAL*8 ::  bk(3,nkpts,nnmx)
      REAL*8 ::  bka(3,nnmxh)
      INTEGER :: nntot(nkpts)
      INTEGER :: neigh(nkpts,nnmxh)
      REAL*8 :: rguide(3,nbands)
      REAL*8 :: xx(nnmx)
      REAL*8 :: smat(3,3), svec(3), sinv(3,3)
      COMPLEX*16 :: csheet(nbands,nkpts,nnmx)
      REAL*8 :: sheet(nbands,nkpts,nnmx)

      REAL*8 rave(3,nbands), rnkb(nbands,nkpts,nnmx)

      COMPLEX*16 ci
      REAL*8 pi
      PARAMETER ( ci = ( 0.0d0, 1.0d0 ) )
      PARAMETER ( pi = 3.14159265358979323846d0 )

      COMPLEX*16 :: csumt
      REAL*8 :: xx0
      REAL*8 :: det
      REAL*8 :: brn
      REAL*8 :: pherr

! ... Report problem to solve for each band, csum is determined and then its appropriate
!     guiding center rguide(3,nwann)

      do nwann=1,nbands

!       get average phase for each unique bk direction

        do na=1,nnh
          csum(na)=(0.0,0.0)
          do nkp=1,nkpts2
            nn=neigh(nkp,na)
            csum(na)=csum(na)+cm(nwann,nwann,nkp,nn)
          end do
        end do

! ...   Initialize smat and svec

        DO j = 1, 3
          DO i = 1, 3
            smat(j,i) = 0.d0
          END DO
          svec(j) = 0.d0
        END DO

        write(*,*) ' '

        DO nn = 1, nnh

          IF ( nn <= 3 ) THEN

! ...       Obtain xx with arbitrary branch cut choice
            xx(nn) = -AIMAG( LOG( csum(nn) ) )

          ELSE

! ...       Obtain xx with branch cut choice guided by rguide
            xx0 = 0.d0
            DO j = 1, 3
              xx0 = xx0 + bka(j,nn) * rguide(j,nwann)
            END DO

! ...       xx0 is expected value for xx
            csumt = EXP( ci * xx0 )

! ...       csumt has opposite of expected phase of csum(nn)
            xx(nn) = xx0 - AIMAG( LOG( csum(nn) * csumt ) )

          END IF

          WRITE (*,'(a,i5,3f7.3,2f10.5)') 'nn, bka, xx, mag =',                    &
                nn, (bka(j,nn), j=1,3 ), xx(nn), ABS( csum(nn) ) / DBLE(nkpts2)

! ...     Update smat and svec

          DO j = 1, 3
            DO i = 1, 3
              smat(j,i) = smat(j,i) + bka(j,nn) * bka(i,nn)
            END DO
            svec(j) = svec(j) + bka(j,nn) * xx(nn)
          END DO

          IF ( nn >= 3 ) THEN

! ...       Determine rguide
            CALL inv3( smat, sinv, det )

! ...       The inverse of smat is sinv/det

            IF ( ABS(det) < 1.e-06 ) THEN

! ...         Evidently first three bka vectors are linearly dependent this is not allowed
              WRITE (*,*) ' *** ERROR *** in findr: dependency'
              STOP

            END IF

            IF ( irguide /= 0 ) THEN
              DO j = 1, 3
                rguide(j,nwann) = 0.d0
                DO i = 1, 3
                  rguide(j,nwann) = rguide(j,nwann) + sinv(j,i) * svec(i) / det
                END DO
              END DO
            END IF
            WRITE (*,'(a,3f10.5)') 'rguide =', ( rguide(i,nwann), i=1,3 )
          ENDIF

        END DO

      END DO

! ... Obtain branch cut choice guided by rguid
  
      DO nkp = 1, nkpts2
        DO nwann = 1, nbands
          DO nn = 1, nntot(nkp)
            sheet(nwann,nkp,nn) = 0.d0
            DO j = 1, 3
              sheet(nwann,nkp,nn) = sheet(nwann,nkp,nn) + bk(j,nkp,nn) * rguide(j,nwann)
            END DO
            csheet(nwann,nkp,nn) = EXP( ci * sheet(nwann,nkp,nn) )
          END DO
        END DO
      END DO

! ... Now check that we picked the proper sheet for the log
!     of cm. criterion: q_n^{k,b}=Im(ln(M_nn^{k,b})) + b \cdot r_n are
!     circa 0 for a good solution, circa multiples of 2 pi  for a bad one.
!     I use the guiding center, instead of r_n, to understand which could be
!     right sheet

      DO nkp = 1, nkpts2
        DO m = 1, nbands
          DO nn = 1, nntot(nkp)
            rnkb(m,nkp,nn) = 0.d0
            brn = 0.d0
            DO ind = 1, 3
              brn = brn + bk(ind,nkp,nn) * rguide(ind,m)
            END DO
            rnkb(m,nkp,nn) = rnkb(m,nkp,nn) + brn
          END DO
        END DO
      END DO

      DO nkp = 1, nkpts2
        DO n = 1, nbands
          DO nn = 1, nntot(nkp)
            pherr = AIMAG( LOG( csheet(n,nkp,nn) * cm(n,n,nkp,nn) ) )           &
            -sheet(n,nkp,nn) + rnkb(n,nkp,nn) - AIMAG( LOG( cm(n,n,nkp,nn) ) )
            IF ( ABS(pherr) > pi ) THEN
              WRITE (*,'(3i4,f18.9,3f10.5)') nkp, n, nn, pherr, ( bk(i,nkp,nn), i=1,3 )
            END IF
          END DO
        END DO
      END DO

      RETURN
      END SUBROUTINE
