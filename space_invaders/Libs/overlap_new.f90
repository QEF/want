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
       SUBROUTINE overlap( igv, evecr, eveci, igsort, npwk, dimwin, nntot, nnlist,  &
                           nncell, cm, mxdgve, npwx, nkpts, mxdnn, mxdbnd, ngx,     &
                           ngy, ngz, ndwinx )
!=----------------------------------------------------------------------------------=
 
      USE kinds
      USE timing_module, ONLY : timing 
      USE io_global, ONLY : stdout

      IMPLICIT NONE

      ! ... Input Variables

      INTEGER :: mxdgve, npwx, nkpts, mxdnn, mxdbnd 
      INTEGER :: ngx, ngy, ngz, ndwinx
      INTEGER :: igv( 3, mxdgve ), igsort( npwx, nkpts )
      INTEGER :: npwk( nkpts )
      INTEGER :: nnlist( nkpts, mxdnn )
      INTEGER :: nntot( nkpts )
      INTEGER :: nncell( 3, nkpts, mxdnn )
      INTEGER :: dimwin( nkpts )
      REAL(dbl) :: evecr( npwx, ndwinx, nkpts )
      REAL(dbl) :: eveci( npwx, ndwinx, nkpts )
      COMPLEX(dbl) :: cm( mxdbnd, mxdbnd, mxdnn, nkpts )

      ! ... Local Variables
 
      COMPLEX(dbl) :: czero
      PARAMETER( czero = ( 0.0d0, 0.0d0 ) )

      INTEGER :: nnx, ndnn, nnsh
      INTEGER :: l, m, n, i, j ,nx, ny, nz, igk, ipw1, ipw2
      INTEGER :: nkp, npoint
      INTEGER :: nkp2, npoint2, nn, nb
      INTEGER :: nx2(ngx), ny2(ngy), nz2(ngz)

      COMPLEX(dbl), ALLOCATABLE :: cptwfp(:,:,:)
      INTEGER, ALLOCATABLE  :: ninvpw(:,:)

      INTEGER :: ierr

! ... END declarations

      CALL timing('overlap',OPR='start')
      !WRITE( stdout , fmt= "( /,2x,'Starting OVERLAP ',/)")


      IF( ndwinx /= MAXVAL( dimwin(:) ) ) THEN
        CALL errore(' overlap ', ' inconsistent window ', ndwinx )
      END IF

      ALLOCATE( cptwfp( npwx+1 , ndwinx, nkpts ), STAT=ierr )
      IF( ierr /= 0 ) THEN
        CALL errore(' overlap ', ' allocating cptwfp ', ( (npwx+1) * ndwinx * nkpts ) )
      END IF
      ALLOCATE( ninvpw( 0:(ngx*ngy*ngz), nkpts ), STAT = ierr )
      IF( ierr /= 0 ) THEN
        CALL errore(' overlap ', ' allocating ninvpw ', ( (ngx*ngy*ngz+1)*nkpts ) ) 
      END IF

      ninvpw = npwx + 1

! ... Transform wave-functions in CASTEP format

      cptwfp = czero

      DO nkp = 1, nkpts

        DO j = 1, npwk( nkp )

          igk = igsort(j,nkp)
          IF ( igv(1,igk ) >= 0 ) nx = igv( 1,igk ) + 1
          IF ( igv(1,igk ) <  0 ) nx = igv( 1,igk ) + 1 + ngx
          IF ( igv(2,igk ) >= 0 ) ny = igv( 2,igk ) + 1
          IF ( igv(2,igk ) <  0 ) ny = igv( 2,igk ) + 1 + ngy
          IF ( igv(3,igk ) >= 0 ) nz = igv( 3,igk ) + 1
          IF ( igv(3,igk ) <  0 ) nz = igv( 3,igk ) + 1 + ngz
          npoint = nx + (ny-1) * ngx + (nz-1) * ngx * ngy

          ninvpw( npoint, nkp ) = j

! ...       Npoint is the absolute (k-independent) castep index of the gvector
!           igv(*,igsort(j,nkp)). Note that in general it is not equal to nindpw(j,nkp),
!           since both the ordering of the g-vectors at any given k-point and their
!           absolute orderinging is different in cpw and in castep. (In cpw the
!           absolute ordering is by stars of increasing length.) Think more about this,
!           to make sure all that I said is correct.

        END DO ! g-vectors at present k (J)


        DO nb = 1, dimwin( nkp )
 
! ...     Go through all the g-vectors inside the cutoff radius at the present k-point
 
          DO j = 1, npwk( nkp )

! ...       Bloch state in the g-space grid
 
            cptwfp( j, nb, nkp ) = CONJG( CMPLX( evecr(j,nb,nkp), eveci(j,nb,nkp) ) )

          END DO ! g-vectors at present k (J)

        END DO   ! nb  loop 
      END DO     ! nkp loop

      CALL overlap_base( dimwin, nntot, nnlist, nncell, cm, cptwfp,    &
          ninvpw, npwx, nkpts, mxdnn, mxdbnd, ngx, ngy, ngz, ndwinx )

      DEALLOCATE( cptwfp, STAT=ierr )
         IF (ierr/=0) CALL errore(' overlap ',' deallocating cptwfp',ABS(ierr))
      DEALLOCATE( ninvpw, STAT=ierr )
         IF (ierr/=0) CALL errore(' overlap ',' deallocating ninvpw',ABS(ierr))
 
      CALL timing('overlap',OPR='stop')

      RETURN
      END SUBROUTINE

!
!=----------------------------------------------------------------------------------=
       SUBROUTINE overlap_base( dimwin, nntot, nnlist, nncell, cm, cptwfp,    &
          ninvpw, npwx, nkpts, mxdnn, mxdbnd, ngx, ngy, ngz, ndwinx )
!=----------------------------------------------------------------------------------=
 
      USE kinds
      USE timing_module, ONLY : timing 
      USE io_global, ONLY : stdout

      IMPLICIT NONE

      ! ... Input Variables

      INTEGER :: npwx, nkpts, mxdnn, mxdbnd 
      INTEGER :: ngx, ngy, ngz, ndwinx
      INTEGER :: nnlist( nkpts, mxdnn )
      INTEGER :: nntot( nkpts )
      INTEGER :: nncell( 3, nkpts, mxdnn )
      INTEGER :: dimwin( nkpts )
      COMPLEX(dbl) :: cm( mxdbnd, mxdbnd, mxdnn, nkpts )
      COMPLEX(dbl) :: cptwfp( npwx+1 , ndwinx, nkpts )
      INTEGER :: ninvpw( 0:(ngx*ngy*ngz), nkpts )

      ! ... Local Variables
 
      COMPLEX(dbl) :: czero
      PARAMETER( czero = ( 0.0d0, 0.0d0 ) )

      INTEGER :: nnx, ndnn, nnsh
      INTEGER :: j ,nx, ny, nz, igk, ipw1, ipw2
      INTEGER :: nkp, npoint
      INTEGER :: nkp2, npoint2, nn
      INTEGER :: nx2(ngx), ny2(ngy), nz2(ngz)

! ... END declarations

      CALL timing('overlap_base',OPR='start')


      IF( ndwinx /= MAXVAL( dimwin(:) ) ) THEN
        CALL errore(' overlap_base ', ' inconsistent window ', ndwinx )
      END IF

! ... Calculate cm(i,j,nkp,nn)=<u_i k|u_j k+dk> (keeping into account
!     that if k+dk is outside (or should be outside) the first BZ it must be
!     brought from there (or there)
!     with a exp(-iG.r) factor (given the proper convention for the sign)
!     for a standard, non-Castep convention:
!     psi_nk=psi_nk+G -> u_nk exp(ikr)=u_nk+G exp(ikr) exp (iGr) ->
!                        u_nk+G = u_nk exp(-iGr)
 
! ... Now, we are using the Castep u_nk, so the conjugation to go to the
!     standard Bloch formalism (and that was done in real space above)
!     implies that we deal with v_nk=u_nk*=sum_G c_nk,G* exp(iGr).
!     Additionally, we might have a exp (iG_0r) that has to be introduced,
!     if we need a nkp2 that lies outside the BZ. In our reciprocal space
!     products, that means that we have <v_m,k1|v_n,k2>=\sum_G1,G2 c_m,k1,G1
!     c_n,k2,G2* \int exp [i(G2-G1-G0)r], if G0 is the vector that, say,
!     brings a k2 inside the BZ just outside it, to be a neighbour of a
!     k1 that lies just inside the boundary (and the u are now in standard
!     Bloch notation). The integral gives a delta, and so we take G2s that
!     are G1s+G0, i.e. nx+nncell, etc...

      cm(:,:,:,:) = (0.d0, 0.d0)

      DO nkp = 1, nkpts

        DO nn = 1, nntot(nkp)

          nkp2 = nnlist(nkp,nn)

          ! set up indices
          DO nx = 1, ngx
            nx2(nx) = nx + nncell(1,nkp,nn)
            IF( nx2(nx) < 1   ) nx2(nx) = nx2(nx) + ngx
            IF( nx2(nx) > ngx ) nx2(nx) = nx2(nx) - ngx
          END DO
          DO ny = 1, ngy
            ny2(ny) = ny + nncell(2,nkp,nn)
            IF( ny2(ny) < 1   ) ny2(ny) = ny2(ny) + ngy
            IF( ny2(ny) > ngy ) ny2(ny) = ny2(ny) - ngy
            ny2(ny) = (ny2(ny) - 1) * ngx
          END DO
          DO nz = 1, ngz
            nz2(nz) = nz + nncell(3,nkp,nn)
            IF( nz2(nz) < 1   ) nz2(nz) = nz2(nz) + ngz
            IF( nz2(nz) > ngz ) nz2(nz) = nz2(nz) - ngz
            nz2(nz) = (nz2(nz) - 1) * ngx * ngy
          END DO

          DO nz = 1, ngz
            DO ny = 1, ngy
              DO nx = 1, ngx
                npoint = nx + (ny-1) * ngx + (nz-1) * ngx * ngy
                npoint2 = nx2(nx) + ny2(ny) + nz2(nz)
                ipw1 = ninvpw(npoint,nkp)
                ipw2 = ninvpw(npoint2,nkp2)

                DO j = 1, dimwin(nkp2)
                  cm( 1:dimwin(nkp), j, nn, nkp ) = cm( 1:dimwin(nkp), j, nn, nkp ) + &
                      cptwfp( ipw1, 1:dimwin(nkp), nkp ) * CONJG( cptwfp( ipw2, j, nkp2 ) )
                END DO

              END DO
            END DO
          END DO

        END DO
      END DO

      CALL timing('overlap_base',OPR='stop')

      RETURN
      END SUBROUTINE
