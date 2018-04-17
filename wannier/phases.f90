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
!=----------------------------------------------------------------------------------=
      SUBROUTINE phases( nbands, nkpts, nkpts2, nnmx, nnmxh, nntot, nnh, neigh, &
                 bk, bka, cm, csheet, sheet, rguide, irguide )
!=----------------------------------------------------------------------------------=

      USE kinds
      USE timing_module, ONLY : timing 
      USE constants, ONLY: pi
      USE io_global, ONLY : stdout

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

      INTEGER :: nntot(nkpts)
      INTEGER :: neigh(nkpts,nnmxh)
      COMPLEX(dbl) :: cm(nbands,nbands,nkpts,nnmx)
      COMPLEX(dbl) :: csum(nnmxh)
      COMPLEX(dbl) :: csheet(nbands,nkpts,nnmx)
      REAL(dbl) ::  bk(3,nkpts,nnmx)
      REAL(dbl) ::  bka(3,nnmxh)
      REAL(dbl) :: rguide(3,nbands)
      REAL(dbl) :: xx(nnmx)
      REAL(dbl) :: smat(3,3), svec(3), sinv(3,3)
      REAL(dbl) :: sheet(nbands,nkpts,nnmx)

      REAL(dbl) rave(3,nbands), rnkb(nbands,nkpts,nnmx)

      COMPLEX(dbl) ci
      PARAMETER ( ci = ( 0.0d0, 1.0d0 ) )

      COMPLEX(dbl) :: csumt
      REAL(dbl) :: xx0
      REAL(dbl) :: det
      REAL(dbl) :: brn
      REAL(dbl) :: pherr

    
      CALL timing('phases',OPR='start')

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

        write( stdout,*) ' '

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

          WRITE (stdout ,  fmt= " (2x, 'nn = ', i4, 2x, 'bka = ', 3f7.3, 2x, 'xx = ', f9.5, 2x, 'mag = ', f9.5 ) " )  &
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

! ...       Evidently first three bka vectors are linearly dependent this is not allowed
            IF ( ABS(det) < 1.e-06 ) CALL errore(' phase ', ' wrong dependency in findr', ABS(det) )

            IF ( irguide /= 0 ) THEN
              DO j = 1, 3
                rguide(j,nwann) = 0.d0
                DO i = 1, 3
                  rguide(j,nwann) = rguide(j,nwann) + sinv(j,i) * svec(i) / det
                END DO
              END DO
            END IF
            WRITE (stdout, fmt= " (2x, 'rguide = ', 3f9.4) " )( rguide(i,nwann), i=1,3 )
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
              WRITE( stdout, fmt=" ( 2x, 3i4,f18.9,3f10.5 ) ")  nkp, n, nn, pherr, ( bk(i,nkp,nn), i=1,3 )
            END IF
          END DO
        END DO
      END DO

      CALL timing('phases',OPR='stop')
      RETURN
      END SUBROUTINE
