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
!
!=------------------------------------------------------------------------------------------=
      SUBROUTINE domega( nbands, nkpts, nkpts2, nntot, nnmx, nnlist, bk, wb,               &
                 cm, csheet, sheet, rave, r2ave, cdodq1, cdodq2, cdodq3, cdodq )
!=------------------------------------------------------------------------------------------=
      
      USE kinds

      IMPLICIT NONE 

      INTEGER :: nbands
      INTEGER :: nkpts
      INTEGER :: nkpts2
      INTEGER :: nnmx
      INTEGER :: nwann
      INTEGER :: ind
      INTEGER :: nkp
      INTEGER :: nn

      INTEGER :: nnlist(nkpts,nnmx), nntot(nkpts)
      REAL(dbl) :: wb(nkpts,nnmx)
      REAL(dbl) :: bk(3,nkpts,nnmx), rave(3,nbands)
      REAL(dbl) :: r2ave(nbands)
      REAL(dbl) :: sheet(nbands,nkpts,nnmx)
      REAL(dbl) :: brn 
      COMPLEX(dbl) :: csheet(nbands,nkpts,nnmx)
      COMPLEX(dbl) :: cm(nbands,nbands,nnmx,nkpts)
      COMPLEX(dbl) :: cdodq(nbands,nbands,nkpts)
      COMPLEX(dbl) :: cdodq1(nbands,nbands,nkpts)
      COMPLEX(dbl) :: cdodq2(nbands,nbands,nkpts)
      COMPLEX(dbl) :: cdodq3(nbands,nbands,nkpts)

      INTEGER :: m, n, ierr

      REAL(dbl), ALLOCATABLE :: rnkb(:,:,:) ! rnkb(nbands,nkpts,nnmx)
      COMPLEX(dbl), ALLOCATABLE :: cr(:,:,:,:) ! (nbands,nbands,nkpts,nnmx)
      COMPLEX(dbl), ALLOCATABLE :: crt(:,:,:,:) ! (nbands,nbands,nkpts,nnmx)

      ALLOCATE( cr(nbands,nbands,nnmx,nkpts), STAT=ierr )
           IF( ierr /=0 ) CALL errore(' domega ', ' allocating cr ', nbands**2*nkpts*nnmx )
      ALLOCATE( crt(nbands,nbands,nnmx,nkpts), STAT=ierr )
           IF( ierr /=0 ) CALL errore(' domega ', ' allocating crt ', nbands**2*nkpts*nnmx )
      ALLOCATE( rnkb(nbands,nnmx,nkpts), STAT=ierr )
           IF( ierr /=0 ) CALL errore(' domega ', ' allocating rnkb ', nbands*nkpts*nnmx )


! ... Recalculate rave

      DO nwann = 1, nbands
        DO ind = 1, 3
          rave(ind,nwann) = 0.d0 
          DO nkp = 1, nkpts2
            DO nn = 1, nntot(nkp)
              rave(ind,nwann) = rave(ind,nwann) + wb(nkp,nn) * bk(ind,nkp,nn) *    &
              ( AIMAG( LOG( csheet(nwann,nkp,nn) * cm(nwann,nwann,nn,nkp) ) )      &
               - sheet(nwann,nkp,nn) )
            END DO
          END DO
          rave(ind,nwann) = - rave(ind,nwann) / DBLE(nkpts2)
        END DO
      END DO

! ... R_mn=M_mn/M_nn and q_m^{k,b} = Im phi_m^{k,b} + b.r_n are calculated

      DO nkp = 1, nkpts2
        DO m = 1, nbands
          DO nn = 1, nntot(nkp)
            DO n = 1, nbands

! ...         Old minimization 
              crt(m,n,nn,nkp) = cm(m,n,nn,nkp) / cm(n,n,nn,nkp)

! ...         New minimization 
              cr(m,n,nn,nkp) = cm(m,n,nn,nkp) * CONJG( cm(n,n,nn,nkp) )


            END DO
            rnkb(m,nn,nkp) = 0.d0
            brn = 0.d0
            DO ind = 1, 3
              brn = brn + bk(ind,nkp,nn) * rave(ind,m)
            END DO
            rnkb(m,nn,nkp) = rnkb(m,nn,nkp) + brn
          END DO
        END DO
      END DO

! ... cd0dq(m,n,nkp) is calculated

      DO nkp = 1, nkpts2
        DO m = 1, nbands
          DO n = 1, nbands
            cdodq1(m,n,nkp) = ( 0.d0, 0.d0 )
            cdodq2(m,n,nkp) = ( 0.d0, 0.d0 )
            cdodq3(m,n,nkp) = ( 0.d0, 0.d0 )

            DO nn = 1, nntot(nkp)

! ...         A[R^{k,b}]=(R-Rdag)/2

              cdodq1(m,n,nkp) = cdodq1(m,n,nkp) + wb(nkp,nn) * ( cr(m,n,nn,nkp) / 2.d0 - &
                                CONJG( cr(n,m,nn,nkp) ) / 2.d0 )

! ...         -S[T^{k,b}]=-(T+Tdag)/2i ; T_mn = Rt_mn q_n

              cdodq2(m,n,nkp) = cdodq2(m,n,nkp) - wb(nkp,nn) * ( crt(m,n,nn,nkp) *          &
                              ( AIMAG( LOG( csheet(n,nkp,nn) * cm(n,n,nn,nkp) ) ) -         &
                                sheet(n,nkp,nn) ) + CONJG( crt(n,m,nn,nkp) *                &
                              ( AIMAG( LOG( csheet(m,nkp,nn) * cm(m,m,nn,nkp) ) ) -         &
                                sheet(m,nkp,nn) ) ) ) / ( 0.d0, 2.d0 )

              cdodq3(m,n,nkp) = cdodq3(m,n,nkp) - wb(nkp,nn) * ( crt(m,n,nn,nkp) *          &
                                rnkb(n,nn,nkp) + CONJG( crt(n,m,nn,nkp) * &
                                rnkb(m,nn,nkp) ) ) / ( 0.d0, 2.d0 )

            END DO

            cdodq1(m,n,nkp) = 4.d0 * cdodq1(m,n,nkp) / DBLE(nkpts2)
            cdodq2(m,n,nkp) = 4.d0 * cdodq2(m,n,nkp) / DBLE(nkpts2)
            cdodq3(m,n,nkp) = 4.d0 * cdodq3(m,n,nkp) / DBLE(nkpts2)
            cdodq(m,n,nkp) = cdodq1(m,n,nkp) + cdodq2(m,n,nkp) + cdodq3(m,n,nkp)

          END DO
        END DO
      END DO

      DEALLOCATE( cr, STAT=ierr )
           IF( ierr /=0 ) CALL errore(' domega ', ' deallocating cr ', ABS(ierr) )
      DEALLOCATE( crt, STAT=ierr )
           IF( ierr /=0 ) CALL errore(' domega ', ' deallocating crt ', ABS(ierr) )
      DEALLOCATE( rnkb, STAT=ierr )
           IF( ierr /=0 ) CALL errore(' domega ', ' deallocating rnkb ', ABS(ierr) )


      RETURN
      END SUBROUTINE
