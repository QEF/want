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
       SUBROUTINE overlap( igv, evec, igsort, npwk, dimwin, nntot, nnlist,  &
                           nncell, cm, mxdgve, npwx, nkpts, mxdnn, mxdbnd, ngx,     &
                           ngy, ngz, ndwinx )
!=----------------------------------------------------------------------------------=
 
      USE kinds
      USE timing_module, ONLY : timing 
      USE io_global, ONLY : stdout
      USE util, ONLY: gv_indexes

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
      COMPLEX(dbl) :: evec( npwx + 1, ndwinx, nkpts )
      COMPLEX(dbl) :: cm( mxdbnd, mxdbnd, mxdnn, nkpts )

      ! ... Local Variables
 
      COMPLEX(dbl) :: czero
      PARAMETER( czero = ( 0.0d0, 0.0d0 ) )

      INTEGER :: nnx, ndnn, nnsh
      INTEGER :: l, m, n, i, j ,nx, ny, nz, igk, ipw1, ipw2
      INTEGER :: nkp, npoint
      INTEGER :: nkp2, npoint2, nn, nb
      INTEGER :: nx2(ngx), ny2(ngy), nz2(ngz)

      INTEGER, ALLOCATABLE  :: ninvpw(:,:)

      INTEGER :: ierr

! ... END declarations

      CALL timing('overlap',OPR='start')
      !WRITE( stdout , fmt= "( /,2x,'Starting OVERLAP ',/)")


      IF( ndwinx /= MAXVAL( dimwin(:) ) ) THEN
        CALL errore(' overlap ', ' inconsistent window ', ndwinx )
      END IF

      ALLOCATE( ninvpw( 0:(ngx*ngy*ngz), nkpts ), STAT = ierr )
      IF( ierr /= 0 ) THEN
        CALL errore(' overlap ', ' allocating ninvpw ', ( (ngx*ngy*ngz+1)*nkpts ) ) 
      END IF

      ninvpw = npwx + 1

! ... Transform wave-functions in CASTEP format

      DO nkp = 1, nkpts

        CALL gv_indexes( igv, igsort(:,nkp), npwk(nkp), ngx, ngy, ngz, ninvpw = ninvpw(:,nkp) )

      END DO     ! nkp loop

      CALL overlap_base( dimwin, nntot, nnlist, nncell, cm, evec,    &
          ninvpw, npwx, nkpts, mxdnn, mxdbnd, ngx, ngy, ngz, ndwinx )

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

      INTEGER :: nnx, ndnn, nnsh, nn
      INTEGER :: i, j ,nx, ny, nz, igk, ipw1, ipw2
      INTEGER :: nkp1, npoint1, dimw1
      INTEGER :: nkp2, npoint2, dimw2
      INTEGER :: nx2(ngx), ny2(ngy), nz2(ngz)
      COMPLEX(dbl) :: cwin1( ndwinx ), cwin2

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

      DO nkp1 = 1, nkpts

        DO nn = 1, nntot( nkp1 )

          nkp2 = nnlist( nkp1, nn )

          ! set up indices
          DO nx = 1, ngx
            nx2(nx) = nx + nncell( 1, nkp1, nn )
            IF( nx2(nx) < 1   ) nx2(nx) = nx2(nx) + ngx
            IF( nx2(nx) > ngx ) nx2(nx) = nx2(nx) - ngx
          END DO
          DO ny = 1, ngy
            ny2(ny) = ny + nncell( 2, nkp1, nn )
            IF( ny2(ny) < 1   ) ny2(ny) = ny2(ny) + ngy
            IF( ny2(ny) > ngy ) ny2(ny) = ny2(ny) - ngy
            ny2(ny) = (ny2(ny) - 1) * ngx
          END DO
          DO nz = 1, ngz
            nz2(nz) = nz + nncell( 3, nkp1, nn )
            IF( nz2(nz) < 1   ) nz2(nz) = nz2(nz) + ngz
            IF( nz2(nz) > ngz ) nz2(nz) = nz2(nz) - ngz
            nz2(nz) = (nz2(nz) - 1) * ngx * ngy
          END DO

          dimw1 = dimwin(nkp1)
          dimw2 = dimwin(nkp2)

          DO nz = 1, ngz
            DO ny = 1, ngy
              DO nx = 1, ngx
                npoint1 = nx + (ny-1) * ngx + (nz-1) * ngx * ngy
                npoint2 = nx2(nx) + ny2(ny) + nz2(nz)
                ipw1 = ninvpw( npoint1, nkp1 )
                ipw2 = ninvpw( npoint2, nkp2 )

                cwin1( 1:dimw1 ) = CONJG( cptwfp( ipw1, 1:dimw1, nkp1 ) ) 
                DO j = 1, dimw2
                  cwin2 = cptwfp( ipw2, j, nkp2 )
                  DO i = 1, dimw1
                    cm( i, j, nn, nkp1 ) = cm( i, j, nn, nkp1 ) + cwin1( i ) * cwin2
                  END DO
                END DO

!                DO j = 1, dimwin(nkp2)
!                  cm( 1:dimwin(nkp1), j, nn, nkp1 ) = cm( 1:dimwin(nkp1), j, nn, nkp1 ) + &
!                      CONJG( cptwfp( ipw1, 1:dimwin(nkp1), nkp1 ) ) * cptwfp( ipw2, j, nkp2 )
!                END DO

              END DO
            END DO
          END DO

        END DO
      END DO

      CALL timing('overlap_base',OPR='stop')

      RETURN
      END SUBROUTINE
