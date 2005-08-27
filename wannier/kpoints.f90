!
! Copyright (C) 2004 WanT Group
!
! This file is distributed under the terms of the
! GNU General Public License. See the file `License'
! in the root directory of the present distribution,
! or http://www.gnu.org/copyleft/gpl.txt .
!
!

!*********************************************
   MODULE kpoints_module
!*********************************************
   USE kinds, ONLY: dbl
   USE constants, ONLY : ONE, TWO, TPI, EPS_m6
   USE parameters, ONLY : npkx, nstrx, nnx, nnhx 
   USE lattice_module, ONLY : alat, avec, bvec, lattice_alloc => alloc
   USE iotk_module

   IMPLICIT NONE
   PRIVATE
   SAVE

! This module handles kpoints data (including the treatment
! of their nearest neighbors b, bshells).
!
! routines in this module:
! SUBROUTINE kpoints_allocate()
! SUBROUTINE kpoints_deallocate()
! SUBROUTINE bshells_allocate()
! SUBROUTINE bshells_deallocate()
! SUBROUTINE bshells_init( )
! SUBROUTINE kpoints_read_ext( unit, name, found )

!
! declarations
!

  !
  ! ... usual kpt data (k vectors)
  INTEGER                 :: nkpts         ! kpt number (NOT doubled when nspin=2)
  !
  ! ... added to manage collinear spin from Espresso
  !     when nspin == 2, ik = 1, nkpts are for isp=1,
  !     while ik=nkpts+1, 2*nkpts are for isp=2
  INTEGER                 :: nkpts_tot     ! the total number of kpoint (including spin)
  INTEGER                 :: iks           ! the starting ik (at the current spin)
  INTEGER                 :: ike           ! the ending ik (at the current spin)
  !
  INTEGER                 :: nk(3)         ! component of the MP kgrid
  INTEGER                 :: s(3)          ! fractional shifts of the MP grid
  REAL(dbl), ALLOCATABLE  :: vkpt(:,:)     ! kpt components; DIM: 3*nkpts (Bohr^-1)
  REAL(dbl), ALLOCATABLE  :: wk(:)         ! weight of each kpt for BZ sums 
  REAL(dbl)               :: wksum         ! sum of the weights

  !
  ! ... Nearest neighbor data (b vectors)

  INTEGER                 :: nshells       ! input number of shells to be used
  INTEGER                 :: nwhich(nnx)   ! the chosen shells
  INTEGER, ALLOCATABLE    :: nntot(:)      ! DIM: nkpts
  INTEGER, ALLOCATABLE    :: nnshell(:,:)  ! DIM: nkpts*nnx
  INTEGER, ALLOCATABLE    :: nnlist(:,:)   ! DIM: nkpts*nnx
  INTEGER, ALLOCATABLE    :: nncell(:,:,:) ! DIM: 3*nnx*nkpts
  INTEGER, ALLOCATABLE    :: neigh(:,:)    ! DIM: nkpts*nnhx
  INTEGER, ALLOCATABLE    :: nreverse(:,:) ! DIM: nnx*nkpts
  REAL(dbl), ALLOCATABLE  :: bk(:,:,:)     ! DIM: 3*nkpts*nnx (bohr^-1)
  REAL(dbl), ALLOCATABLE  :: wb(:,:)       ! b-weights, DIM: nkpts*nnx
  REAL(dbl), ALLOCATABLE  :: bka(:,:)      ! DIM: 3*nnhx (bohr^-1)
  REAL(dbl), ALLOCATABLE  :: dnn(:)        ! DIM: nnx (bohr^-1)
  REAL(dbl)               :: wbtot         ! sum of the b-weights

  LOGICAL :: kpoints_alloc = .FALSE.
  LOGICAL :: bshells_alloc = .FALSE.

!
! end of declaration scope 
!

  PUBLIC :: nkpts
  PUBLIC :: nkpts_tot
  PUBLIC :: iks, ike
  PUBLIC :: nk, s
  PUBLIC :: vkpt
  PUBLIC :: wk, wksum

  PUBLIC :: nnx, nnhx
  PUBLIC :: nshells
  PUBLIC :: nwhich
  PUBLIC :: nntot
  PUBLIC :: nnshell
  PUBLIC :: nnlist
  PUBLIC :: nncell
  PUBLIC :: neigh
  PUBLIC :: nreverse
  PUBLIC :: bk
  PUBLIC :: wb
  PUBLIC :: dnn
  PUBLIC :: bka
  PUBLIC :: wbtot
  PUBLIC :: kpoints_alloc, bshells_alloc

  PUBLIC :: bshells_init
  PUBLIC :: kpoints_allocate
  PUBLIC :: bshells_allocate
  PUBLIC :: kpoints_read_ext
  PUBLIC :: kpoints_deallocate
  PUBLIC :: bshells_deallocate


CONTAINS

!**********************************************************
   SUBROUTINE kpoints_allocate()
   !**********************************************************
   IMPLICIT NONE
      INTEGER   :: ierr
      CHARACTER(16)     :: subname="kpoints_allocate"

      !
      ! kpt data
      !
      IF ( nkpts <= 0) CALL errore(subname,'Invalid NKPTS',ABS(nkpts)+1)
      ALLOCATE( vkpt(3,nkpts),STAT=ierr )
         IF (ierr/=0) CALL errore(subname,'allocating vkpt',3*nkpts)
      ALLOCATE( wk(nkpts),STAT=ierr )
         IF (ierr/=0) CALL errore(subname,'allocating wk',nkpts)

      kpoints_alloc = .TRUE.
   END SUBROUTINE kpoints_allocate


!**********************************************************
   SUBROUTINE bshells_allocate()
   !**********************************************************
   IMPLICIT NONE
      INTEGER   :: ierr
      CHARACTER(16)     :: subname="bshells_allocate"

      IF ( nkpts <= 0)  CALL errore(subname,'Invalid NKPTS',ABS(nkpts)+1)
      IF ( nnx <= 0 ) CALL errore(subname,'Invalid NNX',ABS(nnx)+1)
      IF ( nnhx <= 0) CALL errore(subname,'Invalid NNHX ',ABS(nnhx)+1)

      ALLOCATE( nntot(nkpts), STAT=ierr )
         IF (ierr /=0 ) CALL errore(subname,'allocating nntot',nkpts)
      ALLOCATE( nnshell(nkpts,nnx), STAT = ierr )
         IF( ierr /=0 ) CALL errore(subname, ' allocating nnshell ', nkpts*nnx )
      ALLOCATE( nnlist(nkpts,nnx), STAT = ierr )
         IF( ierr /=0 ) CALL errore(subname, ' allocating nnlist ', nkpts*nnx )
      ALLOCATE( nncell(3,nnx,nkpts), STAT = ierr )
         IF( ierr /=0 ) CALL errore(subname, ' allocating nncell ', 3*nnx*nkpts )
      ALLOCATE( neigh(nkpts,nnhx), STAT = ierr )
         IF( ierr /=0 ) CALL errore(subname, ' allocating neigh ', nkpts*nnhx )
      ALLOCATE( nreverse(nnx,nkpts), STAT = ierr )
         IF( ierr /=0 ) CALL errore(subname, ' allocating nreverse ', nkpts*nnx )
      ALLOCATE( bk(3,nkpts,nnx), STAT = ierr )
         IF( ierr /=0 ) CALL errore(subname, ' allocating bk ', 3*nkpts*nnx )
      ALLOCATE( wb(nkpts,nnx), STAT = ierr )
         IF( ierr /=0 ) CALL errore(subname, ' allocating wb ', nkpts*nnx )
      ALLOCATE( dnn(nnx), STAT = ierr )
         IF( ierr /=0 ) CALL errore(subname, ' allocating dnn ', nnx )
      ALLOCATE( bka(3,nnhx), STAT = ierr )
         IF( ierr /=0 ) CALL errore(subname, ' allocating bka ', 3*nnhx )

      bshells_alloc = .TRUE.
   END SUBROUTINE bshells_allocate


!**********************************************************
   SUBROUTINE bshells_init( )
   !**********************************************************
    IMPLICIT NONE

      IF ( .NOT. lattice_alloc ) CALL errore('bshells_init','Lattice NOT allocated',1)
      ! ... Setup the shells of b-vectors around each K-point
      ! 
      CALL bshells_allocate()
      CALL bshells(bvec, nshells, nwhich)

    RETURN
   END SUBROUTINE bshells_init


!**********************************************************
   SUBROUTINE kpoints_read_ext( unit, name, found )
   !**********************************************************
    IMPLICIT NONE
       INTEGER,           INTENT(in) :: unit
       CHARACTER(*),      INTENT(in) :: name
       LOGICAL,           INTENT(out):: found
       INTEGER            :: lnkpts
       REAL(dbl),ALLOCATABLE :: lwk(:), lvkpt(:,:)
       CHARACTER(nstrx)   :: attr, string
       CHARACTER(16)      :: subname='kpoints_read_ext'
       INTEGER            :: ik, ierr
       
       IF ( kpoints_alloc ) CALL kpoints_deallocate()
       IF ( nkpts <=0 .OR. nkpts_tot <=0 ) CALL errore(subname,'nkpts should be set',2) 
       IF ( iks <=0 .OR. ike <=0 ) CALL errore(subname,'iks or ike not set',3) 
       IF ( iks > ike ) CALL errore(subname,'iks > ike',4) 
       IF ( ike - iks + 1 /= nkpts ) CALL errore(subname,'Invalid ike or iks',5)

       CALL iotk_scan_begin(unit,TRIM(name),ATTR=attr,FOUND=found,IERR=ierr)
       IF (.NOT. found) RETURN
       IF (ierr>0)  CALL errore(subname,'Wrong format in tag '//TRIM(name),ierr)
       found = .TRUE.

       CALL iotk_scan_attr(attr,'nk',lnkpts,IERR=ierr)
       IF (ierr/=0) CALL errore(subname,'Unable to find NK',ABS(ierr))
       IF ( lnkpts <= 0) CALL errore(subname,'Invalid lnkpts',ABS(lnkpts)+1)
       !
       ! checking the spin polarized case
       !
       IF ( lnkpts /= nkpts_tot ) CALL errore(subname,'invalid nkpts_tot',5)
       !
       ! local variables
       ALLOCATE( lwk(nkpts_tot), STAT=ierr )
          IF (ierr/=0) CALL errore(subname,'allocating lwk',ABS(ierr))
       ALLOCATE( lvkpt(3,nkpts_tot), STAT=ierr )
          IF (ierr/=0) CALL errore(subname,'allocating lvkpt',ABS(ierr))

     
       CALL kpoints_allocate( )

       CALL iotk_scan_dat(unit,'weights',lwk(:),IERR=ierr)
       IF (ierr/=0) CALL errore(subname,'reading tag weights',ABS(ierr))
       !
       ! passing to internal values
       wk(:) = lwk(iks:ike)
       !
       ! due to a different convention in Espresso
       wk(:) = wk(:) * nkpts_tot / ( TWO * nkpts )
       wksum = SUM(wk(:))
       DO ik = 1,nkpts
          IF( ABS( wk(ik) - ONE/REAL(nkpts) ) > EPS_m6 ) &
              CALL errore(subname,'Invalid kpt weight',ik)
       ENDDO

       !
       ! ... kpoints are in cartesian units, in 2PI/alat
       !
       CALL iotk_scan_dat(unit,'k',lvkpt(:,:),IERR=ierr)
       IF (ierr/=0) CALL errore(subname,'reading tag k',ABS(ierr))
       !
       ! passing to the internal variable 
       vkpt(:,:) = lvkpt(:,iks:ike)
       !
       ! convert them to bohr^-1 cartesian units 
       vkpt(:,:) = vkpt(:,:) * TPI/alat

       !
       ! cleanup
       DEALLOCATE( lwk, lvkpt, STAT=ierr)
         IF(ierr/=0) CALL errore(subname,'deallocating lwk, lvkpt', ABS(ierr))

       CALL iotk_scan_end(unit,TRIM(name),IERR=ierr)
       IF (ierr/=0)  CALL errore(subname,'Unable to end tag '//TRIM(name),ABS(ierr)) 

   END SUBROUTINE kpoints_read_ext


!**********************************************************
   SUBROUTINE kpoints_deallocate()
   !**********************************************************
   IMPLICIT NONE
    INTEGER   :: ierr

    IF ( ALLOCATED( vkpt ) ) THEN
       DEALLOCATE( vkpt, STAT=ierr )
       IF (ierr/=0) CALL errore('kpoints_deallocate','deallocating vkpt',ABS(ierr))
    ENDIF
    IF ( ALLOCATED( wk ) ) THEN
       DEALLOCATE( wk, STAT=ierr )
       IF (ierr/=0) CALL errore('kpoints_deallocate','deallocating wk',ABS(ierr))
    ENDIF

    kpoints_alloc = .FALSE.
   END SUBROUTINE kpoints_deallocate


!**********************************************************
   SUBROUTINE bshells_deallocate()
   !**********************************************************
   IMPLICIT NONE
    INTEGER   :: ierr

    IF ( ALLOCATED( nntot ) ) THEN
       DEALLOCATE( nntot, STAT=ierr )
       IF (ierr/=0) CALL errore('bshells_deallocate','deallocating nntot',ABS(ierr))
    ENDIF
    IF ( ALLOCATED( nnshell ) ) THEN
       DEALLOCATE( nnshell, STAT=ierr )
       IF (ierr/=0) CALL errore('bshells_deallocate','deallocating nnshell',ABS(ierr))
    ENDIF
    IF ( ALLOCATED( nnlist ) ) THEN
       DEALLOCATE( nnlist, STAT=ierr )
       IF (ierr/=0) CALL errore('bshells_deallocate','deallocating nnlist',ABS(ierr))
    ENDIF
    IF ( ALLOCATED( nncell ) ) THEN
       DEALLOCATE( nncell, STAT=ierr )
       IF (ierr/=0) CALL errore('bshells_deallocate','deallocating nncell',ABS(ierr))
    ENDIF
    IF ( ALLOCATED( neigh ) ) THEN
       DEALLOCATE( neigh, STAT=ierr )
       IF (ierr/=0) CALL errore('bshells_deallocate','deallocating neigh',ABS(ierr))
    ENDIF
    IF ( ALLOCATED( nreverse ) ) THEN
       DEALLOCATE( nreverse, STAT=ierr )
       IF (ierr/=0) CALL errore('bshells_deallocate','deallocating nreverse',ABS(ierr))
    ENDIF
    IF ( ALLOCATED( bk ) ) THEN
       DEALLOCATE( bk, STAT=ierr )
       IF (ierr/=0) CALL errore('bshells_deallocate','deallocating bk',ABS(ierr))
    ENDIF
    IF ( ALLOCATED( wb ) ) THEN
       DEALLOCATE( wb, STAT=ierr )
       IF (ierr/=0) CALL errore('bshells_deallocate','deallocating wb',ABS(ierr))
    ENDIF
    IF ( ALLOCATED( dnn ) ) THEN
       DEALLOCATE( dnn, STAT=ierr )
       IF (ierr/=0) CALL errore('bshells_deallocate','deallocating dnn',ABS(ierr))
    ENDIF
    IF ( ALLOCATED( bka ) ) THEN
       DEALLOCATE( bka, STAT=ierr )
       IF (ierr/=0) CALL errore('bshells_deallocate','deallocating bka',ABS(ierr))
    ENDIF

    bshells_alloc = .FALSE.
   END SUBROUTINE bshells_deallocate

END MODULE kpoints_module
