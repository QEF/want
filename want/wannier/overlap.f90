!
! Copyright (C) 2004 Andrea Ferretti, Arrigo Calzolari, Carlo Cavazzoni, Marco Buongiorno Nardelli
!
! This file is distributed under the terms of the
! GNU General Public License. See the file `License'
! in the root directory of the present distribution,
! or http://www.gnu.org/copyleft/gpl.txt .
!
! From a previous version by Nicola Marzari and David Vanderbilt
!
!=----------------------------------------------------------------------------------=
       SUBROUTINE overlap( evc, igsort, npwk, dimwin, nntot, nnlist,       &
                           nncell, cm, npw, npwkx, nkpts, mxdnn, ngx,      &
                           ngy, ngz, dimwinx )
!=----------------------------------------------------------------------------------=
 
      USE kinds
      USE constants,      ONLY : CZERO
      USE timing_module,  ONLY : timing, timing_upto_now 
      USE io_module,      ONLY : stdout

      USE uspp,           ONLY : qb
      USE kpoints_module, ONLY : dnn, ndnntot, bk   ! XXXX
      USE ggrids_module,  ONLY : igv                ! XXXX
      USE wfc_module,     ONLY : npwx_g             ! XXXX
      USE becmod,         ONLY : becp
      USE ions_module,    ONLY : uspp_calculation

      IMPLICIT NONE
      !
      ! ... Input Variables
      !
      INTEGER :: npw, npwkx, nkpts, mxdnn
      INTEGER :: ngx, ngy, ngz, dimwinx
      INTEGER :: igsort( npwkx, nkpts )
      INTEGER :: npwk( nkpts )
      INTEGER :: nnlist( nkpts, mxdnn )
      INTEGER :: nntot( nkpts )
      INTEGER :: nncell( 3, nkpts, mxdnn )
      INTEGER :: dimwin( nkpts )
      COMPLEX(dbl) :: evc( npwkx, dimwinx, nkpts )
      COMPLEX(dbl) :: cm( dimwinx, dimwinx, mxdnn, nkpts )
      !
      ! ... Local Variables
      !
      INTEGER :: ndnn, nn, ierr
      INTEGER :: i, j ,nx, ny, nz, igk, ig
      INTEGER :: ik1, j1, dimw1
      INTEGER :: ik2, j2, dimw2

      INTEGER, ALLOCATABLE      :: map(:)
      COMPLEX(dbl), ALLOCATABLE :: aux(:,:), aux1(:), aux2(:)

!
! ... END declarations
!
      CALL timing('overlap',OPR='start')


      IF( dimwinx /= MAXVAL( dimwin(:) ) ) THEN
        CALL errore(' overlap_base ', ' inconsistent window ', dimwinx )
      END IF
      ALLOCATE( map(npwkx), STAT=ierr )
         IF (ierr/=0) CALL errore('overlap_base','allocating map',npwkx)
      ALLOCATE( aux(dimwinx, dimwinx), STAT=ierr )
         IF (ierr/=0) CALL errore('overlap_base','allocating aux',dimwinx**2)
      ALLOCATE( aux1(npwx_g), STAT=ierr )
         IF (ierr/=0) CALL errore('overlap_base','allocating aux1',npwx_g)
      ALLOCATE( aux2(npwx_g), STAT=ierr )
         IF (ierr/=0) CALL errore('overlap_base','allocating aux2',npwx_g)

!
! ... Calculate cm(i,j,ik,nn)=<u_i k|u_j k+dk> (keeping into account
!     that if k+dk is outside (or should be outside) the first BZ it must be
!     brought from there (or there)
!     with a exp(-iG.r) factor (given the proper convention for the sign)
!     for a standard, non-Castep convention:
!     psi_nk=psi_nk+G -> u_nk exp(ikr)=u_nk+G exp(ikr) exp (iGr) ->
!                        u_nk+G = u_nk exp(-iGr)
! 
!     Additionally, we might have a exp (iG_0r) that has to be introduced,
!     if we need a ik2 that lies outside the BZ. In our reciprocal space
!     products, that means that we have <v_m,k1|v_n,k2>=\sum_G1,G2 c_m,k1,G1
!     c_n,k2,G2* \int exp [i(G2-G1-G0)r], if G0 is the vector that, say,
!     brings a k2 inside the BZ just outside it, to be a neighbour of a
!     k1 that lies just inside the boundary (and the u are now in standard
!     Bloch notation). The integral gives a delta, and so we take G2s that
!     are G1s+G0, i.e. nx+nncell, etc...
!

! XXX
cm = CZERO

      DO ik1 = 1, nkpts
          WRITE( stdout , "( 4x,'Overlap calculation for k-point ',i3)") ik1

          DO nn = 1, nntot( ik1 )
              ik2 = nnlist( ik1, nn )

              dimw1 = dimwin(ik1)
              dimw2 = dimwin(ik2)

              !
              ! creates the map from the PW of ik2 to those of ik1 
              ! the PWs not found are set to npwkx hwich has zero coefficients by def.
              !
              ! this mapping takes into account the e^{iGr} factor when k1 and k2 are 
              ! in different Brillouin zones.
              !
              CALL set_overlap_map( npwk(ik2), npwx_g, ngx, ngy, ngz, igsort(1,ik2), &
                                    nncell(1,ik1,nn), map)
              map( npwk(ik2)+1: npwkx ) = 0

!! XXXX
!    IF ( nncell(1,ik1,nn) /= 0 .OR. nncell(2,ik1,nn) /= 0 .OR. nncell(3,ik1,nn) /= 0) &
!    THEN
!
!   WRITE(0,*) 'XXX'
!   WRITE(0,*) 'ik1 ik2', ik1, ik2
!   WRITE(0,*) 'cell', nncell(:,ik1,nn)
!   DO ig=1,npwk(ik2)
!       WRITE(0,*) igv(:,igsort(ig,ik2) ), '<><><>', igv(:,map(ig) )
!   ENDDO
!   WRITE(0,*) 

              !
              ! loops over bands
              !
              DO j2 = 1, dimw2
                  aux2(:) = CZERO
                  DO ig=1, npwk(ik2)
                     aux2( map( ig ) ) = evc( ig, j2, ik2) 
                  ENDDO

                  DO j1 = 1, dimw1
                      aux1(:) = CZERO
                      DO ig=1, npwk(ik1)
                         aux1( igsort( ig, ik1 ) ) = evc( ig, j1, ik1) 
                      ENDDO

                      cm( j1, j2, nn, ik1 ) = CZERO
                      !
                      ! last index is dumm:,ik1,nny
                      DO ig=1, npwx_g -1  
                         cm( j1, j2, nn, ik1 ) = cm( j1, j2, nn, ik1 ) + &
                                                 CONJG( aux1(ig) ) * aux2(ig)
                      ENDDO
                  ENDDO
              ENDDO

              !
              ! ... add the augmentation term fo USPP
              !
              IF ( uspp_calculation ) THEN
                  CALL add_us_overlap(dimwinx, dimw1, dimw2, ik1, ik2, nn, aux)
                  cm(:,:,nn,ik1)= cm(:,:,nn,ik1) + aux(:,:)
              ENDIF
!! XXXX
!ENDIF

          ENDDO
          CALL timing_upto_now(stdout)
      ENDDO

      DEALLOCATE( aux, STAT=ierr)
         IF (ierr/=0) CALL errore('overlap_base','deallocating aux',ABS(ierr))
      DEALLOCATE( aux1, STAT=ierr)
         IF (ierr/=0) CALL errore('overlap_base','deallocating aux1',ABS(ierr))
      DEALLOCATE( aux2, STAT=ierr)
         IF (ierr/=0) CALL errore('overlap_base','deallocating aux2',ABS(ierr))
      DEALLOCATE( map, STAT=ierr)
         IF (ierr/=0) CALL errore('overlap_base','deallocating map',ABS(ierr))

      CALL timing('overlap',OPR='stop')

      RETURN
   END SUBROUTINE overlap

