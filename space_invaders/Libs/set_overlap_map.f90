! 
! Copyright (C) 2004 WanT Group
! 
! This file is distributed under the terms of the 
! GNU General Public License. See the file `License' 
! in the root directory of the present distribution, 
! or http://www.gnu.org/copyleft/gpl.txt . 
! 
!*********************************************************
SUBROUTINE set_overlap_map(npwk,npwx_g,ngx,ngy,ngz,igsort,nncell,map)
   !*********************************************************
   USE kinds
   USE ggrids_module, ONLY : igv, ggrids_gv_indexes
   USE timing_module
   
   IMPLICIT NONE

! <INFO>
! This subroutine set the map between the G coefficients of the
! calculated wfc and the vectors entering the overlap scalar
! product eventually accounting for the e^{iGr} phase
! (giving an overall permutation of the G indexes)
!
! The routine may be optimized eliminating some IF in the
! loops whether necessary
! 
! </INFO>
   
   !
   ! ... input variables
   !
   INTEGER,  INTENT(in)      :: npwk                ! max pw number for wfc among kpts
   INTEGER,  INTENT(in)      :: npwx_g              ! total number of G for wfcs
   INTEGER,  INTENT(in)      :: ngx,ngy,ngz         ! FFT dimensions
   INTEGER,  INTENT(in)      :: igsort(npwk)        ! indexes of the G coeff for wfcs
   INTEGER,  INTENT(in)      :: nncell(3)           ! BZ of the required neighbour
   INTEGER,  INTENT(out)     :: map(npwk)           ! map between the actual G ordering for 
                                                    ! wfcs and the one required for overlaps 
                                                    ! eventually including the permutation
                                                    ! due to the inclusion of the e^iGr phase
   !
   ! ... local variables
   !
   CHARACTER(15)    :: subname="set_overlap_map"
   INTEGER          :: ix,  iy,  iz
   INTEGER          :: ix2, iy2, iz2
   INTEGER          :: ngm
   INTEGER          :: itmp, ipoint, ipoint_new
   INTEGER          :: ig, ierr
   INTEGER, ALLOCATABLE :: fft2gk(:), gk2fft(:)

!
! ... end of declarations

   CALL timing('set_overlap_map',OPR='start')

   ALLOCATE( gk2fft(npwk), STAT=ierr )
      IF (ierr/=0) CALL errore(subname,'allocating gk2fft', ABS(ierr) )
   ALLOCATE( fft2gk(0:ngx*ngy*ngz), STAT=ierr )
      IF (ierr/=0) CALL errore(subname,'allocating fft2gk', ABS(ierr) )

   CALL ggrids_gv_indexes(igv, igsort, npwk, ngx, ngy, ngz, gk2fft, fft2gk )
   ngm = ngx*ngy*ngz+1

   DO ig=1, npwk
      
      ipoint = gk2fft(ig)
      !
      ! ipoint index in the FFT grid is built as
      ! 
      ! ipoint = ix + (iy-1)*ngx + (iz-1)*ngx*ngy
      !
     
      ix = MOD( ipoint, ngx )
      IF( ix < 1   ) ix = ix + ngx
      IF( ix > ngx ) ix = ix - ngx
      !
      ix2 = ix - nncell( 1 )
      IF( ix2 < 1   ) ix2 = ix2 + ngx
      IF( ix2 > ngx ) ix2 = ix2 - ngx

      iy = ( MOD( ipoint, ngx*ngy) - ix ) / ngx +1
      IF ( MOD( MOD( ipoint, ngx*ngy) - ix, ngx ) /= 0 )  &
           CALL errore(subname,'invalid FFT grid',1)
      IF( iy < 1   ) iy = iy + ngy
      IF( iy > ngy ) iy = iy - ngy
      !
      iy2 = iy - nncell( 2 )
      IF( iy2 < 1   ) iy2 = iy2 + ngy
      IF( iy2 > ngy ) iy2 = iy2 - ngy

      iz = ( ipoint - ix -(iy-1)*ngx ) / (ngx*ngy) +1
      IF ( MOD(ipoint - ix -(iy-1)*ngx, ngx*ngy) /= 0 )  &
           CALL errore(subname,'invalid FFT grid',2)
      IF( iz < 1   ) iz = iz + ngz
      IF( iz > ngz ) iz = iz - ngz
      !
      iz2 = iz - nncell( 3 )
      IF( iz2 < 1   ) iz2 = iz2 + ngz
      IF( iz2 > ngz ) iz2 = iz2 - ngz

      ipoint_new = ix2 + (iy2-1)*ngx + (iz2-1)*ngx*ngy
      !
      IF ( ipoint_new > ngm ) CALL errore(subname,'invalid indexes',ipoint_new)
      itmp = fft2gk(ipoint_new)
      !
      ! if the translation of the G go over a point out of the 
      ! initial G group, we set the related inxed to a dummy position
      !
      IF ( itmp > 0 ) THEN
         map(ig) = igsort( itmp )
      ELSE
         map(ig) = npwx_g
      ENDIF
   ENDDO

   DEALLOCATE( gk2fft, STAT=ierr )
      IF (ierr/=0) CALL errore(subname,'deallocating gk2fft', ABS(ierr) )
   DEALLOCATE( fft2gk, STAT=ierr )
      IF (ierr/=0) CALL errore(subname,'deallocating fft2gk', ABS(ierr) )

   CALL timing('set_overlap_map',OPR='stop')

END SUBROUTINE set_overlap_map


