! 
! Copyright (C) 2007 WanT Group
! 
! This file is distributed under the terms of the 
! GNU General Public License. See the file `License' 
! in the root directory of the present distribution, 
! or http://www.gnu.org/copyleft/gpl.txt . 
! 
!*********************************************************
SUBROUTINE symmetrize_kgrid( )
   !*********************************************************
   !
   ! This subroutine ...
   !
   !
   USE kinds
   USE constants,         ONLY : EPS_m6, ZERO, ONE, TWO
   USE converters_module, ONLY : cart2cry
   USE control_module,    ONLY : use_symmetry, use_timerev
   USE lattice_module,    ONLY : bvec, lattice_alloc => alloc
   USE symmetry_module,   ONLY : nsym, srot, symmetry_rotate, symmetry_alloc => alloc
   USE kpoints_module,    ONLY : nkpts, vkpt, nkpts_all, vkpt_all, kpoints_alloc
   USE log_module,        ONLY : log_push, log_pop
   USE timing_module,     ONLY : timing
   !
   IMPLICIT NONE

   !
   ! input variables
   !
!   INTEGER,    INTENT(inout) :: nkpts
!   REAL(dbl),  INTENT(inout) :: vkpt(3,nkpts)
!   INTEGER,    INTENT(inout) :: nkpts_all
!   REAL(dbl),  INTENT(inout) :: vkpt_all(3,*)

   !
   ! local variables
   !
   CHARACTER(16)             :: subname="symmetrize_kgrid"
   REAL(dbl)                 :: vect(3)
   REAL(dbl), ALLOCATABLE    :: vkpt_cry(:,:), vkpt_symm(:,:)
   INTEGER,   ALLOCATABLE    :: symm_map(:), kpteq_map(:)
   LOGICAL                   :: found
   INTEGER                   :: ifact, isym, ik, i, j, ierr, m,n

!
!------------------------------
! main body
!------------------------------
!
   IF ( .NOT. kpoints_alloc ) CALL errore(subname,'kpoints not alloc', 1)
   IF ( .NOT. lattice_alloc ) CALL errore(subname,'lattice not alloc', 1)
   IF ( .NOT. symmetry_alloc ) RETURN

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
   !
   ALLOCATE(vkpt_symm(3, ifact * nkpts), STAT=ierr)
   IF (ierr/=0) CALL errore(subname,'allocating vkpt_symm',ABS(ierr))
   !
   ALLOCATE( symm_map(ifact * nkpts), STAT=ierr)
   IF (ierr/=0) CALL errore(subname,'allocating symm_map',ABS(ierr))
   !
   ALLOCATE( kpteq_map(ifact * nkpts), STAT=ierr)
   IF (ierr/=0) CALL errore(subname,'allocating kpteq_map',ABS(ierr))
   !
   vkpt_cry( :, 1: nkpts) = vkpt( :, 1:nkpts )
   !
   ! vkpt_loc in crystal coords
   !
   CALL cart2cry( vkpt_cry, bvec )

   !
   ! symmetrize the input kpts
   !
   ik = 1
   vkpt_symm( :, ik) = vkpt_cry( :, 1)
   symm_map( ik )    = 1
   kpteq_map( ik )   = 1
   !
   DO i = 1, nkpts
       !
       !
       symmetries_loop: &
       DO isym = 0, nsym
           !
           ! get the rotated vector
           ! isym == 0 is the time reversal operation
           ! all vectors in crystal units
           !
           IF ( isym == 0 ) THEN
              !
              IF ( .NOT. use_timerev ) CYCLE symmetries_loop
              !
              vect( : ) = - vkpt_cry(:,i)
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

!WRITE(0,*)
!WRITE(0,*) "vkpt", vkpt_cry(:,i)
!WRITE(0,*) "vect", vect(:)
!WRITE(0,*) "vect", vect(:)
!WRITE(0,*) "isym", isym
!IF ( isym /= 0 ) THEN
!   WRITE(0,"(3i5)") ((srot(m,n,isym), n = 1,3), m=1,3)
!ENDIF

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
               symm_map( ik )    = isym
               kpteq_map( ik )   = i
               ! 
           ENDIF
           !
       ENDDO symmetries_loop
       !
   ENDDO
   ! 
   nkpts_all = ik

!
! <DEBUG>
!
   WRITE( 0, * ) 
   WRITE( 0, * ) "nkpts = ", nkpts
   WRITE( 0, * ) 
   DO ik = 1, nkpts
       !
       WRITE( 0, "( i3, 3f12.6)") ik, vkpt_cry( :, ik)
       !
   ENDDO

   WRITE( 0, * ) "=============================="
   WRITE( 0, * ) "nkpts_all = ", nkpts_all
   WRITE( 0, * ) 
   WRITE( 0, * ) "ik    vkpt              kpt_eq    symm"
   !
   DO ik = 1, nkpts_all
       !
       WRITE( 0, "( i3, 3f12.6, i5, i5)") ik, vkpt_symm( :, ik), kpteq_map( ik ), symm_map( ik )
       !
   ENDDO
!
! </DEBUG>
!
       

   DEALLOCATE( vkpt_cry, vkpt_symm, STAT=ierr)
   IF (ierr/=0) CALL errore(subname,'deallocating vkpt_cry, vkpt_symm',ABS(ierr))
   !
   DEALLOCATE( symm_map, kpteq_map, STAT=ierr)
   IF (ierr/=0) CALL errore(subname,'deallocating symm_map, kpteq_map',ABS(ierr))


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

