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
       SUBROUTINE overlap( igv, evc, igsort, npwk, dimwin, nntot, nnlist,  &
                           nncell, cm, npw, npwkx, nkpts, mxdnn, nbnd, ngx,     &
                           ngy, ngz, dimwinx )
!=----------------------------------------------------------------------------------=
 
      USE kinds
      USE constants,     ONLY : CZERO
      USE timing_module, ONLY : timing 
      USE io_module,     ONLY : stdout
      USE util_module,   ONLY : gv_indexes

      IMPLICIT NONE

      ! ... Input Variables

      INTEGER :: npw, npwkx, nkpts, mxdnn, nbnd 
      INTEGER :: ngx, ngy, ngz, dimwinx
      INTEGER :: igv( 3, npw ), igsort( npwkx, nkpts )
      INTEGER :: npwk( nkpts )
      INTEGER :: nnlist( nkpts, mxdnn )
      INTEGER :: nntot( nkpts )
      INTEGER :: nncell( 3, nkpts, mxdnn )
      INTEGER :: dimwin( nkpts )
      COMPLEX(dbl) :: evc( npwkx, dimwinx, nkpts )
      COMPLEX(dbl) :: cm( nbnd, nbnd, mxdnn, nkpts )

      ! ... Local Variables
 
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


      IF( dimwinx /= MAXVAL( dimwin(:) ) ) THEN
        CALL errore(' overlap ', ' inconsistent window ', dimwinx )
      END IF

      ALLOCATE( ninvpw( 0:(ngx*ngy*ngz), nkpts ), STAT = ierr )
      IF( ierr /= 0 ) THEN
        CALL errore(' overlap ', ' allocating ninvpw ', ( (ngx*ngy*ngz+1)*nkpts ) ) 
      END IF

      !
      ! NOTE: npwkx is the maximum of npwk(:) + 1
      ninvpw = npwkx 

      DO nkp = 1, nkpts
           CALL gv_indexes( igv, igsort(:,nkp), npwk(nkp), ngx, ngy, ngz, &
                            NINVPW=ninvpw(:,nkp) )
      ENDDO     

      CALL overlap_base( dimwin, nntot, nnlist, nncell, cm, evc,    &
          ninvpw, npwkx, nkpts, mxdnn, nbnd, ngx, ngy, ngz, dimwinx )

      DEALLOCATE( ninvpw, STAT=ierr )
         IF (ierr/=0) CALL errore(' overlap ',' deallocating ninvpw',ABS(ierr))
 
      CALL timing('overlap',OPR='stop')

      RETURN
      END SUBROUTINE

!
!=----------------------------------------------------------------------------------=
       SUBROUTINE overlap_base( dimwin, nntot, nnlist, nncell, cm, evc,    &
          ninvpw, npwkx, nkpts, mxdnn, nbnd, ngx, ngy, ngz, dimwinx )
!=----------------------------------------------------------------------------------=
 
      USE kinds
      USE constants,      ONLY : CZERO, EPS_m8, BOHR => bohr_radius_angs
      USE timing_module,  ONLY : timing 
      USE io_module,      ONLY : stdout
      USE uspp,           ONLY : qb
      USE kpoints_module, ONLY : dnn, ndnntot, bk   ! XXXX
      USE wfc_module,     ONLY : npwk
      USE becmod,         ONLY : becp
      USE ions_module,    ONLY : uspp_calculation

      IMPLICIT NONE

      ! ... Input Variables

      INTEGER :: npwkx, nkpts, mxdnn, nbnd 
      INTEGER :: ngx, ngy, ngz, dimwinx
      INTEGER :: nnlist( nkpts, mxdnn )
      INTEGER :: nntot( nkpts )
      INTEGER :: nncell( 3, nkpts, mxdnn )
      INTEGER :: dimwin( nkpts )
      COMPLEX(dbl) :: cm( nbnd, nbnd, mxdnn, nkpts )
      COMPLEX(dbl) :: evc( npwkx , dimwinx, nkpts )
      INTEGER :: ninvpw( 0:(ngx*ngy*ngz), nkpts )

      ! ... Local Variables
 
      INTEGER :: nnx, ndnn, nnsh, nn, ish, ierr
      INTEGER :: i, j ,nx, ny, nz, igk, ipw1, ipw2
      INTEGER :: nkp1, npoint1, dimw1
      INTEGER :: nkp2, npoint2, dimw2
      INTEGER :: nx2(ngx), ny2(ngy), nz2(ngz)
      COMPLEX(dbl) :: cwin1( dimwinx ), cwin2
      COMPLEX(dbl), ALLOCATABLE :: aux(:,:)
      REAL(dbl) :: norm

! ... END declarations

      CALL timing('overlap_base',OPR='start')


      IF( dimwinx /= MAXVAL( dimwin(:) ) ) THEN
        CALL errore(' overlap_base ', ' inconsistent window ', dimwinx )
      END IF
      ALLOCATE( aux(dimwinx, dimwinx), STAT=ierr )
         IF (ierr/=0) CALL errore('overlap_base','allocating aux',dimwinx**2)

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

      cm(:,:,:,:) = CZERO

      DO nkp1 = 1, nkpts
          DO nn = 1, nntot( nkp1 )

              nkp2 = nnlist( nkp1, nn )

              ! set up indices
              DO nx = 1, ngx
                  nx2(nx) = nx + nncell( 1, nkp1, nn )
                  IF( nx2(nx) < 1   ) nx2(nx) = nx2(nx) + ngx
                  IF( nx2(nx) > ngx ) nx2(nx) = nx2(nx) - ngx
              ENDDO
              DO ny = 1, ngy
                  ny2(ny) = ny + nncell( 2, nkp1, nn )
                  IF( ny2(ny) < 1   ) ny2(ny) = ny2(ny) + ngy
                  IF( ny2(ny) > ngy ) ny2(ny) = ny2(ny) - ngy
                  ny2(ny) = (ny2(ny) - 1) * ngx
              ENDDO
              DO nz = 1, ngz
                  nz2(nz) = nz + nncell( 3, nkp1, nn )
                  IF( nz2(nz) < 1   ) nz2(nz) = nz2(nz) + ngz
                  IF( nz2(nz) > ngz ) nz2(nz) = nz2(nz) - ngz
                  nz2(nz) = (nz2(nz) - 1) * ngx * ngy
              ENDDO

              dimw1 = dimwin(nkp1)
              dimw2 = dimwin(nkp2)

              !
              ! ... get ish, the index of kpt2 in the shell list
              !
              ish = 0
              norm = bk(1,nkp1,nn)**2 + bk(2,nkp1,nn)**2 + bk(3,nkp1,nn)**2 
              norm = SQRT( norm )
              !
              DO ish = 1, ndnntot
                 IF ( ABS(norm - dnn(ish) ) < EPS_m8 ) EXIT 
                 IF ( ish == ndnntot)  &
                    CALL errore('overlap_base','unable to find the shell',1)
              ENDDO
              ! XXXX
              ! this loop should be improved

              DO nz = 1, ngz
              DO ny = 1, ngy
              DO nx = 1, ngx
                   npoint1 = nx + (ny-1) * ngx + (nz-1) * ngx * ngy
                   npoint2 = nx2(nx) + ny2(ny) + nz2(nz)
                   ipw1 = ninvpw( npoint1, nkp1 )
                   ipw2 = ninvpw( npoint2, nkp2 )

                   cwin1( 1:dimw1 ) = CONJG( evc( ipw1, 1:dimw1, nkp1 ) ) 
                   DO j = 1, dimw2
                       cwin2 = evc( ipw2, j, nkp2 )
                       DO i = 1, dimw1
                            cm( i, j, nn, nkp1 ) = cm( i, j, nn, nkp1 ) + cwin1(i) * cwin2
                       ENDDO
                   ENDDO

              ENDDO
              ENDDO
              ENDDO

              !
              ! ... add the augmentation term fo USPP
              !
              IF ( uspp_calculation ) THEN
                  aux(:,:) = CZERO
                  CALL add_us_overlap(dimwinx, dimw1, dimw2, ish,  &
                                      becp(1,1,nkp1), becp(1,1,nkp2), aux)
                  cm(1:dimw1,1:dimw2,nn,nkp1)= cm(1:dimw1,1:dimw2,nn,nkp1) + &
                                               aux(1:dimw1,1:dimw2)
              ENDIF

          ENDDO
      ENDDO

      DEALLOCATE( aux, STAT=ierr)
         IF (ierr/=0) CALL errore('overlap_base','deallocating aux',ABS(ierr))

      CALL timing('overlap_base',OPR='stop')

      RETURN
      END SUBROUTINE
