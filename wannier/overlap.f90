!
! Copyright (C) 2004 WanT Group
!
! This file is distributed under the terms of the
! GNU General Public License. See the file `License'
! in the root directory of the present distribution,
! or http://www.gnu.org/copyleft/gpl.txt .
!
!************************************************************
   SUBROUTINE overlap( ik1, ik2, dimw1, dimw2, imin1, imin2, dimwinx, evc, evc_info, &
                       igsort, lnncell, Mkb )
   !************************************************************
   USE kinds
   USE constants,      ONLY : CZERO
   USE timing_module,  ONLY : timing
   USE wfc_info_module 
   USE ggrids_module,  ONLY : nfft

   IMPLICIT NONE
      !
      ! ... Input Variables
      !
      TYPE(wfc_info), INTENT(in) :: evc_info
      COMPLEX(dbl),   INTENT(in) :: evc( evc_info%npwx, evc_info%nwfc )

      INTEGER,        INTENT(in) :: ik1, ik2
      INTEGER,        INTENT(in) :: dimw1, dimw2, dimwinx
      INTEGER,        INTENT(in) :: imin1, imin2
      INTEGER,        INTENT(in) :: igsort( evc_info%npwx , *)
      INTEGER,        INTENT(in) :: lnncell( 3 )
      COMPLEX(dbl),   INTENT(out):: Mkb( dimwinx, dimwinx )

      !
      ! ... Local Variables
      !
      INTEGER :: ierr
      INTEGER :: ig
      INTEGER :: npwkx, npwx_g
      INTEGER :: j1, npwk1, ind1
      INTEGER :: j2, npwk2, ind2

      INTEGER, ALLOCATABLE      :: map(:)
      COMPLEX(dbl), ALLOCATABLE :: aux1(:), aux2(:)
      !
      ! ... end declarations
      !

!
!------------------------------
! main body
!------------------------------
!
      CALL timing('overlap',OPR='start')

      !
      ! the maximun number of PW for wfcs
      npwkx = evc_info%npwx
      !
      ! is the total number of G in k-indipendent wfc representation
      ! among ik1 and ik2
      ! the "+1" at the end of the line is used to have one void position
      !
      npwx_g = MAX( MAXVAL(igsort(:,ik1)), MAXVAL(igsort(:,ik2)) ) +1

      IF ( dimw1 > dimwinx ) CALL errore('overlap', 'Invalid dimw1', dimw1)
      IF ( dimw2 > dimwinx ) CALL errore('overlap', 'Invalid dimw2', dimw2)

      ALLOCATE( map(npwkx), STAT=ierr )
         IF (ierr/=0) CALL errore('overlap','allocating map',npwkx)
      ALLOCATE( aux1(npwx_g), STAT=ierr )
         IF (ierr/=0) CALL errore('overlap','allocating aux1',npwx_g)
      ALLOCATE( aux2(npwx_g), STAT=ierr )
         IF (ierr/=0) CALL errore('overlap','allocating aux2',npwx_g)

!
! ... Calculate cm(i,j)=<u_i k|u_j k+dk> (keeping into account
!     that if k+dk is outside (or should be outside) the first BZ it must be
!     brought from there (or there)
!     with a exp(-iG.r) factor (given the proper convention for the sign):
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

      !
      ! Here imin* take into account the fact that dimwin 
      ! may not start from the first band
      !
      ind1  = wfc_info_getindex(imin1, ik1, "IK", evc_info)
      ind2  = wfc_info_getindex(imin2, ik2, "IKB", evc_info)
      npwk1 = evc_info%npw( ind1 )
      npwk2 = evc_info%npw( ind2 )

      !
      ! creates the map from the PW of ik2 to those of ik1 
      ! the PWs not found are set to the index npwx_g which is not summed
      !
      ! this mapping takes into account the e^{iGr} factor when k1 and k2 are 
      ! in different Brillouin zones.
      !
      CALL overlap_setmap( npwk2, npwx_g, nfft(1), nfft(2), nfft(3), igsort(1,ik2), &
                           lnncell(1), map)
      map( npwk2+1: npwkx ) = 0


      !
      ! loops over bands
      !
      DO j2 = 1, dimw2
          aux2(:) = CZERO
          ind2 = wfc_info_getindex(imin2 +j2 -1, ik2, "IKB", evc_info)
          DO ig=1, npwk2
             aux2( map( ig ) ) = evc( ig, ind2) 
          ENDDO

          DO j1 = 1, dimw1
              aux1(:) = CZERO
              ind1 = wfc_info_getindex(imin1 +j1 -1, ik1, "IK", evc_info)
              DO ig=1, npwk1
                 aux1( igsort( ig, ik1 ) ) = evc( ig, ind1) 
              ENDDO

              Mkb( j1, j2 ) = CZERO
              !
              ! last position for ig is dummy
              DO ig=1, npwx_g -1  
                 Mkb( j1, j2 ) = Mkb( j1, j2 ) + &
                                         CONJG( aux1(ig) ) * aux2(ig)
              ENDDO
          ENDDO
      ENDDO

      DEALLOCATE( aux1, STAT=ierr)
         IF (ierr/=0) CALL errore('overlap','deallocating aux1',ABS(ierr))
      DEALLOCATE( aux2, STAT=ierr)
         IF (ierr/=0) CALL errore('overlap','deallocating aux2',ABS(ierr))
      DEALLOCATE( map, STAT=ierr)
         IF (ierr/=0) CALL errore('overlap','deallocating map',ABS(ierr))

      CALL timing('overlap',OPR='stop')

      RETURN
   END SUBROUTINE overlap

