! 
! Copyright (C) 2004 Andrea Ferretti
! 
! This file is distributed under the terms of the 
! GNU General Public License. See the file `License' 
! in the root directory of the present distribution, 
! or http://www.gnu.org/copyleft/gpl.txt . 
! 
!*********************************************************
SUBROUTINE ordering(dimwann, nkpts, rave, rave2, r2ave, cu)
   !*********************************************************
   USE kinds
   IMPLICIT NONE

! <INFO>
! This subroutine performs an ordering of the wannier function
! indexing according to some criteria.
!
! At the moment the only implemented criterium is a spherical
! ordering based first on the distance of the wannier center from a
! fixed point (like the origin) and then equally distant WF
! will be ordered for increasing spread 
! 
! </INFO>

   REAL(dbl), PARAMETER        :: toll_dist = 1.0d-4
   REAL(dbl), PARAMETER        :: toll_spread = 1.0d-4

   INTEGER, INTENT(in)         :: dimwann, nkpts
   REAL(dbl), INTENT(inout)    :: rave(3,dimwann), rave2(dimwann), r2ave(dimwann)
   COMPLEX(dbl), INTENT(inout) :: cu(dimwann,dimwann,nkpts)

   REAL(dbl), ALLOCATABLE      :: rtmp(:), rtmp2(:)
   INTEGER, ALLOCATABLE        :: index(:)
   INTEGER                     :: i,is,ie, ierr


!------------------------------------------------


   ALLOCATE(index(dimwann), STAT=ierr)
   IF (ierr/=0) CALL errore('ordering','allocating INDEX',ABS(ierr))
   ALLOCATE(rtmp(dimwann), STAT=ierr)
   IF (ierr/=0) CALL errore('ordering','allocating RTMP',ABS(ierr))
   ALLOCATE(rtmp2(dimwann), STAT=ierr)
   IF (ierr/=0) CALL errore('ordering','allocating RTMP2',ABS(ierr))
   
   !
   ! distance of the center from the origin
   !
   DO i=1,dimwann
      rtmp(i) = SQRT ( DOT_PRODUCT( rave(:,i), rave(:,i) ) )
      rtmp2(i) = r2ave(i) - rave2(i) 
      index(i) = 0
   ENDDO
   !
   ! sorting by distance
   !
   CALL hpsort_eps(dimwann, rtmp(:), index(:), toll_dist )
   rtmp2(:) = rtmp2( index(:) )
   !
   ! sorting by spread
   !
   is = 1
   ie = 1
   DO i=2,dimwann
      IF ( ABS(rtmp(is) - rtmp(i)) < toll_dist ) THEN 
           ie = i
      ELSEIF ( ie > is ) THEN
           CALL hpsort_eps( ie-is +1, rtmp2(is:ie), index(is:ie), toll_spread)
           is = i
           ie = i
      ENDIF
   ENDDO

   !
   ! ordering main quantities
   !
   rave(:,:) = rave( :, index(:) )
   rave2(:)  = rave2( index(:) )
   r2ave(:)  = r2ave( index(:) )
   cu(:,:,:) = cu( :, index(:), : ) 

   DEALLOCATE( index, STAT=ierr)
   IF (ierr/=0) CALL errore('ordering','deallocating INDEX',ABS(ierr))
   DEALLOCATE( rtmp, rtmp2, STAT=ierr)
   IF (ierr/=0) CALL errore('ordering','deallocating RTMP RTMP2',ABS(ierr))

   END SUBROUTINE ordering
