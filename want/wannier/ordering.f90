! 
! Copyright (C) 2004 WanT Group
! 
! This file is distributed under the terms of the 
! GNU General Public License. See the file `License' 
! in the root directory of the present distribution, 
! or http://www.gnu.org/copyleft/gpl.txt . 
! 
!*********************************************************
SUBROUTINE ordering(dimwann, nkpts, rave, rave2, r2ave, cu, ordering_mode)
   !*********************************************************
   USE kinds,     ONLY : dbl
   USE constants, ONLY : EPS_m6, EPS_m2
   IMPLICIT NONE

! <INFO>
! This subroutine performs an ordering of the wannier function
! indexing according to some criteria.
!
! At the moment the only implemented criterium is a spherical
! ordering based first on the distance of the wannier center from a
! fixed point (like the origin) and then equally distant WF
! will be ordered for increasing spread.
! Different ordering options are given by ORDERING_MODE 
!
! ORDERING_MODE : 
!    * none           no ordering 
!    * spatial        ordering based on WF center positions only
!    * spread         ordering based on WF spreads only
!    * complete       spatial + spread ordering
!
! </INFO>

   REAL(dbl), PARAMETER        :: toll_dist = 5.0 * EPS_m2
   REAL(dbl), PARAMETER        :: toll_spread = EPS_m6

   INTEGER, INTENT(in)         :: dimwann, nkpts
   REAL(dbl), INTENT(inout)    :: rave(3,dimwann), rave2(dimwann), r2ave(dimwann)
   COMPLEX(dbl), INTENT(inout) :: cu(dimwann,dimwann,nkpts)
   CHARACTER(*), INTENT(in)    :: ordering_mode

   REAL(dbl), ALLOCATABLE      :: rtmp(:), rtmp2(:), rswap(:)
   COMPLEX(dbl), ALLOCATABLE   :: cswap(:,:,:)
   INTEGER, ALLOCATABLE        :: index(:)
   LOGICAL                     :: lspatial, lspread
   INTEGER                     :: i, is, ie, ierr


!------------------------------------------------

   IF ( TRIM(ordering_mode) == 'none' ) THEN
      RETURN
   ELSEIF ( TRIM(ordering_mode) == 'spatial' ) THEN
      lspatial = .TRUE.
   ELSEIF ( TRIM(ordering_mode) == 'spread' ) THEN
      lspread = .TRUE.
   ELSEIF ( TRIM(ordering_mode) == 'complete' ) THEN
      lspatial = .TRUE.
      lspread = .TRUE.
   ELSE 
      CALL errore( 'ordering', 'invalid ORDERING_MODE = '//TRIM(ordering_mode), 1 )
   ENDIF

   ALLOCATE(index(dimwann), STAT=ierr)
   IF (ierr/=0) CALL errore('ordering','allocating INDEX',ABS(ierr))
   ALLOCATE(rtmp(dimwann), STAT=ierr)
   IF (ierr/=0) CALL errore('ordering','allocating RTMP',ABS(ierr))
   ALLOCATE(rtmp2(dimwann), STAT=ierr)
   IF (ierr/=0) CALL errore('ordering','allocating RTMP2',ABS(ierr))
   ALLOCATE(rswap(dimwann), STAT=ierr)
   IF (ierr/=0) CALL errore('ordering','allocating rswap',ABS(ierr))
   ALLOCATE(cswap(dimwann,dimwann,nkpts), STAT=ierr)
   IF (ierr/=0) CALL errore('ordering','allocating cswap',ABS(ierr))
   
   !
   ! distance of the center from the origin
   !
   DO i=1,dimwann
      rtmp(i) = SQRT ( DOT_PRODUCT( rave(:,i), rave(:,i) ) )
      rtmp2(i) = r2ave(i) - rave2(i) 
      index(i) = i
   ENDDO
   !
   ! sorting by distance
   !
   IF ( lspatial ) THEN
       CALL hpsort_eps(dimwann, rtmp(:), index(:), toll_dist )
       rswap(:) = rtmp2(:)
       rtmp2(:) = rswap( index(:) )
   ENDIF
   !
   ! sorting by spread
   !
   IF ( lspread ) THEN

      IF ( .NOT. lspatial ) THEN
         CALL hpsort_eps( dimwann, rtmp2(:), index(:), toll_spread)

      ELSE
         is = 1
         ie = 1

         DO i=2,dimwann
            IF ( ABS(rtmp(is) - rtmp(i)) < toll_dist ) THEN 
                ie = i
                IF ( i == dimwann ) &
                    CALL hpsort_eps( ie-is +1, rtmp2(is:ie), index(is:ie), toll_spread)
            ELSE
                IF ( ie > is ) &
                    CALL hpsort_eps( ie-is +1, rtmp2(is:ie), index(is:ie), toll_spread)
                is = i
                ie = i
            ENDIF
         ENDDO

      ENDIF
   ENDIF

   !
   ! ordering main quantities
   !
   DO i=1,3
      rswap(:) = rave(i,:)
      rave(i,:) = rswap( index(:) )
   ENDDO
   rswap(:)  = rave2(:)
   rave2(:)  = rswap( index(:) )
   rswap(:)  = r2ave(:)
   r2ave(:)  = rswap( index(:) )
   cswap(:,:,:) = cu(:,:,:)
   cu(:,:,:) = cswap( :, index(:), : ) 

   DEALLOCATE( index, STAT=ierr)
   IF (ierr/=0) CALL errore('ordering','deallocating INDEX',ABS(ierr))
   DEALLOCATE( rtmp, rtmp2, STAT=ierr)
   IF (ierr/=0) CALL errore('ordering','deallocating RTMP, RTMP2',ABS(ierr))
   DEALLOCATE( rswap, cswap, STAT=ierr)
   IF (ierr/=0) CALL errore('ordering','deallocating RSWAP, CSWAP',ABS(ierr))

   END SUBROUTINE ordering

