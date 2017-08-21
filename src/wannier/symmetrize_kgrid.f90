! 
! Copyright (C) 2007 WanT Group
! 
! This file is distributed under the terms of the 
! GNU General Public License. See the file `License' 
! in the root directory of the present distribution, 
! or http://www.gnu.org/copyleft/gpl.txt . 
! 
MODULE symmetrize_kgrid_module
CONTAINS
!*********************************************************
SUBROUTINE symmetrize_kgrid( nkpts, vkpt, bvec, nkpts_all, vkpt_all, kpteq_map, kpteq_symm )
   !*********************************************************
   !
   ! This subroutine ...
   !
   !
   USE kinds
   USE constants,         ONLY : EPS_m6, ZERO, ONE, TWO
   USE converters_module, ONLY : cart2cry, cry2cart
   USE control_module,    ONLY : use_symmetry, use_timerev
   USE symmetry_module,   ONLY : nsym, srot, symmetry_rotate, symmetry_alloc => alloc
   USE log_module,        ONLY : log_push, log_pop
   USE timing_module,     ONLY : timing
   !
   IMPLICIT NONE

   !
   ! input variables
   !
   INTEGER,    INTENT(IN) :: nkpts
   REAL(dbl),  INTENT(IN) :: vkpt(3,nkpts)
   REAL(dbl),  INTENT(IN) :: bvec(3,3)
   INTEGER,    INTENT(INOUT) :: nkpts_all
   REAL(dbl), OPTIONAL, INTENT(INOUT) :: vkpt_all(3,nkpts_all)
   INTEGER,   OPTIONAL, INTENT(INOUT) :: kpteq_map(nkpts_all)
   INTEGER,   OPTIONAL, INTENT(INOUT) :: kpteq_symm(nkpts_all)

   !
   ! local variables
   !
   CHARACTER(16)             :: subname="symmetrize_kgrid"
   REAL(dbl)                 :: vect(3)
   REAL(dbl), ALLOCATABLE    :: vkpt_cry(:,:), vkpt_symm(:,:)
   LOGICAL                   :: found
   INTEGER                   :: ifact, isym, ik, i, j, ierr

!
!------------------------------
! main body
!------------------------------
!
   IF ( .NOT. symmetry_alloc ) CALL errore(subname,"symmetry mod not alloc",10)
   !IF ( .NOT. symmetry_alloc ) RETURN   ! XXXX
   IF ( nkpts == 0 ) RETURN

   CALL timing(subname,OPR='start')
   CALL log_push(subname)

   !
   ! first allocate local workspace and take kpts to crystal coordinates
   !
   ifact = 1
   IF ( use_symmetry ) ifact = ifact * nsym
   IF ( use_timerev )  ifact = ifact * 2
   !
   ALLOCATE(vkpt_cry(3, nkpts), STAT=ierr)
   IF (ierr/=0) CALL errore(subname,'allocating vkpt_cry',ABS(ierr))
   ALLOCATE(vkpt_symm(3, ifact * nkpts), STAT=ierr)
   IF (ierr/=0) CALL errore(subname,'allocating vkpt_symm',ABS(ierr))
   !
   vkpt_cry(:,:) = vkpt(:,:)
   !
   ! vkpt_loc in crystal coords
   !
   CALL cart2cry( vkpt_cry, bvec )

   !
   ! symmetrize the input kpts
   !
   ik = 1
   vkpt_symm( :, ik) = vkpt_cry( :, 1)
   IF (PRESENT(kpteq_map))  kpteq_map( ik )   = 1
   IF (PRESENT(kpteq_symm)) kpteq_symm( ik )  = 1
   !
   DO i = 1, nkpts
       !
       !
       symmetries_loop: &
       DO isym = 1, 2*nsym
           !
           ! get the rotated vector
           ! isym > nsym are the 1:nsym spatial symmetries 
           ! combined with the time reversal operation
           ! all vectors in crystal units
           !
           IF ( isym > nsym ) THEN
              !
              IF ( .NOT. use_timerev ) CYCLE symmetries_loop
              !
              vect( : ) = - vkpt_cry(:,i)
              CALL symmetry_rotate( vect, srot(:,:,isym-nsym) )
              !
           ELSE
              !
              IF ( .NOT. use_symmetry ) CYCLE symmetries_loop
              !
              vect(:) = vkpt_cry(:,i)
              CALL symmetry_rotate( vect, srot(:,:,isym) )
              ! 
           ENDIF
           !
           ! bring the rotate kpt in the BZ around 0 
           ! 
           vect(:) = MODULO( vect(:) + 0.5_dbl, ONE ) -0.5_dbl 

           ! 
           ! check whether the newly generated point is equivalent
           ! to any of those previously found
           !
           found = .FALSE.
           !
           DO j = 1, ik
               !
               found = kpt_equiv( vect, vkpt_symm(:,j) )
               !
               IF ( found ) EXIT
               !
           ENDDO
           !
           IF ( .NOT. found ) THEN
               !
               ik = ik + 1
               vkpt_symm( :, ik) = vect(:)
               IF (PRESENT(kpteq_symm)) kpteq_symm( ik )  = isym
               IF (PRESENT(kpteq_map))  kpteq_map( ik )   = i
               ! 
           ENDIF
           !
       ENDDO symmetries_loop
       !
   ENDDO
   ! 
   nkpts_all = ik
   !
   IF ( PRESENT(vkpt_all) ) THEN
       vkpt_all(:,1:nkpts_all) = vkpt_symm( :, 1:nkpts_all)
       CALL cry2cart( vkpt_all(:,1:nkpts_all), bvec)
   ENDIF

!#define __DEBUG
#ifdef __DEBUG
   WRITE( 0, * ) 
   WRITE( 0, * ) "nkpts = ", nkpts
   WRITE( 0, * ) 
   DO ik = 1, nkpts
       !
       WRITE( 0, "( i5, 3f12.6)") ik, vkpt_cry( :, ik)
       !
   ENDDO

   WRITE( 0, * ) "=============================="
   WRITE( 0, * ) "nkpts_all = ", nkpts_all
   WRITE( 0, * ) 
   WRITE( 0, * ) "ik    vkpt              kpt_eq    symm"
   !
   IF ( PRESENT(kpteq_map) .AND. PRESENT(kpteq_symm) .AND. PRESENT(vkpt_all) ) THEN
      DO ik = 1, nkpts_all
          WRITE( 0, "( i5, 3f12.6, i5, i5)") ik, vkpt_symm( :, ik), kpteq_map( ik ), kpteq_symm( ik )
      ENDDO
   ELSE IF ( PRESENT(vkpt_all) ) THEN
      DO ik = 1, nkpts_all
          WRITE( 0, "( i5, 3f12.6, i5, i5)") ik, vkpt_symm( :, ik)
      ENDDO
   ENDIF
#endif
   !       
   DEALLOCATE( vkpt_cry, vkpt_symm, STAT=ierr)
   IF (ierr/=0) CALL errore(subname,'deallocating vkpt_cry, vkpt_symm',ABS(ierr))

   CALL timing(subname,OPR='stop')
   CALL log_pop(subname)
   !
CONTAINS
   !
   !*******************************************************
      LOGICAL FUNCTION kpt_equiv( v1, v2 )
      !*******************************************************
      ! 
      ! assumes v1 and v2 to be in crystal units 
      !
      IMPLICIT NONE
      !
      REAL(dbl) :: v1(3), v2(3)
      REAL(dbl) :: rtmp(3)
      !
      kpt_equiv = .FALSE.
      !
      rtmp(1) = MODULO( v1(1) -v2(1), ONE )
      rtmp(2) = MODULO( v1(2) -v2(2), ONE )
      rtmp(3) = MODULO( v1(3) -v2(3), ONE )
      !
      IF ( ( rtmp(1) < EPS_m6 .OR. rtmp(1) > ONE-EPS_m6 )  .AND.  &
           ( rtmp(2) < EPS_m6 .OR. rtmp(2) > ONE-EPS_m6 )  .AND.  &
           ( rtmp(3) < EPS_m6 .OR. rtmp(3) > ONE-EPS_m6 )  ) THEN
          !
          kpt_equiv = .TRUE.
          !
      ENDIF
      !
   END FUNCTION kpt_equiv
   
END SUBROUTINE symmetrize_kgrid

END MODULE symmetrize_kgrid_module
