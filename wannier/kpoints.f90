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
!

!*********************************************
   MODULE kpoints_module
!*********************************************
   USE kinds, ONLY: dbl
   USE constants, ONLY : ONE, TWO, TPI, EPS_m6
   USE parameters, ONLY : npkx, nstrx 
   USE converters_module, ONLY : cart2cry
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
  INTEGER                        :: nkpts         ! total kpt number
  INTEGER                        :: nk(3)         ! component of the MP kgrid
  REAL(dbl)                      :: s(3)          ! fractional shifts of the MP grid
  REAL(dbl), ALLOCATABLE         :: vkpt(:,:)     ! kpt components; DIM: 3*nkpts
  REAL(dbl), ALLOCATABLE         :: wk(:)         ! weight of each kpt for BZ sums 
  REAL(dbl)                      :: wksum         ! sum of the weights

  !
  ! ... Nearest neighbor data (b vectors)
  INTEGER, PARAMETER             :: mxdnn  = 12   ! maximum number of NN
  INTEGER, PARAMETER             :: mxdnnh = mxdnn/2 

  INTEGER                        :: nshells       ! input number of shells to be used
  INTEGER                        :: nwhich(mxdnn) ! the chosen shells
  INTEGER                        :: ndnntot       ! number of dnn shells
  INTEGER, ALLOCATABLE           :: nntot(:)      ! DIM: nkpts
  INTEGER, ALLOCATABLE           :: nnshell(:,:)  ! DIM: nkpts*mxdnn
  INTEGER, ALLOCATABLE           :: nnlist(:,:)   ! DIM: nkpts*mxdnn
  INTEGER, ALLOCATABLE           :: nncell(:,:,:) ! DIM: 3*nkpts*mxdnn
  INTEGER, ALLOCATABLE           :: neigh(:,:)    ! DIM: nkpts*mxdnnh
  REAL(dbl), ALLOCATABLE         :: bk(:,:,:)     ! DIM: 3*nkpts*mxdnn (bohr^-1)
  REAL(dbl), ALLOCATABLE         :: wb(:,:)       ! b-weights, DIM: nkpts*mxdnn
  REAL(dbl), ALLOCATABLE         :: bka(:,:)      ! DIM: 3*mxdnnh (bohr^-1)
  REAL(dbl), ALLOCATABLE         :: dnn(:)        ! DIM: mxdnn (bohr^-1)
  REAL(dbl)                      :: wbtot         ! sum of the b-weights

  LOGICAL :: kpoints_alloc = .FALSE.
  LOGICAL :: bshells_alloc = .FALSE.

!
! end of declaration scope 
!

  PUBLIC :: nk, s, nkpts
  PUBLIC :: vkpt
  PUBLIC :: wk, wksum

  PUBLIC :: mxdnn, mxdnnh
  PUBLIC :: nshells
  PUBLIC :: nwhich
  PUBLIC :: nntot
  PUBLIC :: nnshell
  PUBLIC :: nnlist
  PUBLIC :: nncell
  PUBLIC :: neigh
  PUBLIC :: bk
  PUBLIC :: wb
  PUBLIC :: dnn, ndnntot
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
      IF ( mxdnn <= 0 ) CALL errore(subname,'Invalid MXDNN',ABS(mxdnn)+1)
      IF ( mxdnnh <= 0) CALL errore(subname,'Invalid MXDNNH ',ABS(mxdnnh)+1)

      ALLOCATE( nntot(nkpts), STAT=ierr )
         IF (ierr /=0 ) CALL errore(subname,'allocating nntot',nkpts)
      ALLOCATE( nnshell(nkpts,mxdnn), STAT = ierr )
         IF( ierr /=0 ) CALL errore(subname, ' allocating nnshell ', nkpts*mxdnn )
      ALLOCATE( nnlist(nkpts,mxdnn), STAT = ierr )
         IF( ierr /=0 ) CALL errore(subname, ' allocating nnlist ', nkpts*mxdnn )
      ALLOCATE( nncell(3,nkpts,mxdnn), STAT = ierr )
         IF( ierr /=0 ) CALL errore(subname, ' allocating nncell ', 3*nkpts*mxdnn )
      ALLOCATE( neigh(nkpts,mxdnnh), STAT = ierr )
         IF( ierr /=0 ) CALL errore(subname, ' allocating neigh ', nkpts*mxdnnh )
      ALLOCATE( bk(3,nkpts,mxdnn), STAT = ierr )
         IF( ierr /=0 ) CALL errore(subname, ' allocating bk ', 3*nkpts*mxdnn )
      ALLOCATE( wb(nkpts,mxdnn), STAT = ierr )
         IF( ierr /=0 ) CALL errore(subname, ' allocating wb ', nkpts*mxdnn )
      ALLOCATE( dnn(mxdnn), STAT = ierr )
         IF( ierr /=0 ) CALL errore(subname, ' allocating dnn ', mxdnn )
      ALLOCATE( bka(3,mxdnnh), STAT = ierr )
         IF( ierr /=0 ) CALL errore(subname, ' allocating bka ', 3*mxdnnh )

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
       CHARACTER(nstrx)   :: attr, string
       CHARACTER(16)      :: subname='kpoints_read_ext'
       INTEGER            :: ik, ierr
       
       IF ( kpoints_alloc ) CALL kpoints_deallocate()

       CALL iotk_scan_begin(unit,TRIM(name),ATTR=attr,FOUND=found,IERR=ierr)
       IF (.NOT. found) RETURN
       IF (ierr>0)  CALL errore(subname,'Wrong format in tag '//TRIM(name),ierr)
       found = .TRUE.

       CALL iotk_scan_attr(attr,'nk',nkpts,IERR=ierr)
       IF (ierr/=0) CALL errore(subname,'Unable to find NK',ABS(ierr))
       IF ( nkpts <= 0) CALL errore(subname,'Invalid nkpts',ABS(nkpts)+1)
     
       CALL kpoints_allocate( )

       CALL iotk_scan_dat(unit,'weights',wk(:),IERR=ierr)
       IF (ierr/=0) CALL errore(subname,'Unable to find tag WIEGHTS',ABS(ierr))
       !
       ! due to a different convention in Espresso
       wk(:) = wk(:) / TWO
       wksum = SUM(wk(:))
       DO ik = 1,nkpts
          IF( ABS( wk(ik) - ONE/REAL(nkpts) ) > EPS_m6 ) &
              CALL errore(subname,'Invalid kpt weight',ik)
       ENDDO

       !
       ! ... kpoints are in cartesian units, in 2PI/alat
       !
       CALL iotk_scan_dat(unit,'k',vkpt(:,:),IERR=ierr)
       IF (ierr/=0) CALL errore(subname,'Unable to find tag K',ABS(ierr))
       !
       ! ... convert them to crystal units as required throughout the code
       !
       CALL cart2cry(vkpt, alat/TPI * bvec )

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
