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
   USE constants, ONLY : ONE
   USE parameters, ONLY : npkx 

   IMPLICIT NONE
   PRIVATE
   SAVE

! This module handles kpoint data (including the treatment
! of their nearest neighbors b).
!
! routines in this module:
! SUBROUTINE kpoints_allocate()
! SUBROUTINE kpoints_deallocate()
! SUBROUTINE kpoints_init( nkpts )

!
! declarations
!
  LOGICAL :: first = .TRUE.

  !
  ! ... usual kpt data (k vectors)
  INTEGER                        :: nkpts         ! total kpt number
  INTEGER                        :: nk(3)         ! component of the MP kgrid
  REAL(dbl)                      :: s(3)          ! fractional shifts of the MP grid
  REAL(dbl), ALLOCATABLE         :: vkpt(:,:)     ! kpt components; DIM: 3*nkpts
  REAL(dbl), ALLOCATABLE         :: wtkpt(:)      ! weight of each kpt for BZ sums 
  REAL(dbl)                      :: wtktot        ! sum of the weights

  !
  ! ... Nearest neighbor data (b vectors)
  INTEGER, PARAMETER             :: mxdnn  = 12   ! maximum number of NN
  INTEGER, PARAMETER             :: mxdnnh = mxdnn/2 

  INTEGER, ALLOCATABLE           :: nntot(:)      ! DIM: nkpts
  INTEGER, ALLOCATABLE           :: nnshell(:,:)  ! DIM: nkpts*mxdnn
  INTEGER, ALLOCATABLE           :: nnlist(:,:)   ! DIM: nkpts*mxdnn
  INTEGER, ALLOCATABLE           :: nncell(:,:,:) ! DIM: 3*nkpts*mxdnn
  INTEGER, ALLOCATABLE           :: neigh(:,:)    ! DIM: nkpts*mxdnnh
  REAL(dbl), ALLOCATABLE         :: bk(:,:,:)     ! DIM: 3*nkpts*mxdnn
  REAL(dbl), ALLOCATABLE         :: wb(:,:)       ! b-weights, DIM: nkpts*mxdnn
  REAL(dbl), ALLOCATABLE         :: dnn(:)        ! DIM: mxdnn
  REAL(dbl), ALLOCATABLE         :: bka(:,:)      ! DIM: 3*mxdnnh
  REAL(dbl)                      :: wbtot         ! sum of the b-weights

  LOGICAL :: alloc = .FALSE.

!
! end of declaration scope 
!

  PUBLIC :: nk, s, nkpts
  PUBLIC :: vkpt
  PUBLIC :: wtkpt, wtktot

  PUBLIC :: mxdnn, mxdnnh
  PUBLIC :: nntot
  PUBLIC :: nnshell
  PUBLIC :: nnlist
  PUBLIC :: nncell
  PUBLIC :: neigh
  PUBLIC :: bk
  PUBLIC :: wb
  PUBLIC :: dnn
  PUBLIC :: bka
  PUBLIC :: wbtot
  PUBLIC :: alloc

  PUBLIC :: kpoints_init
  PUBLIC :: kpoints_allocate
  PUBLIC :: kpoints_deallocate


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
      ALLOCATE( wtkpt(nkpts),STAT=ierr )
         IF (ierr/=0) CALL errore(subname,'allocating wtkpt',nkpts)

      !
      ! b vectors
      !
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

      alloc = .TRUE.

   END SUBROUTINE kpoints_allocate


!**********************************************************
   SUBROUTINE kpoints_init( nkpts_, bshell )
   !**********************************************************
   USE lattice, ONLY : recc
   USE input_module,  ONLY : nshells, nwhich
    IMPLICIT NONE
    INTEGER,           INTENT(in) :: nkpts_
    LOGICAL, OPTIONAL, INTENT(in) :: bshell
    LOGICAL :: bshell_
    INTEGER :: i1, i2, i3, nkp

      nkpts = nkpts_
      IF ( nkpts > npkx ) CALL errore('kpoints_init','Nkpts too large',nkpts)
      CALL kpoints_allocate()

      bshell_ = .TRUE.
      IF (PRESENT(bshell)) bshell_ = bshell

      nkp = 0
      DO i1 = 0, nk(1)-1
        DO i2 = 0, nk(2)-1
          DO i3 = 0, nk(3)-1
            nkp = nkp + 1
            vkpt(1,nkp) = DBLE(i1)/DBLE(nk(1)) + s(1)
            vkpt(2,nkp) = DBLE(i2)/DBLE(nk(2)) + s(2)
            vkpt(3,nkp) = DBLE(i3)/DBLE(nk(3)) + s(3)
          END DO
        END DO
      END DO

      IF( nkp /= nkpts ) &
        CALL errore( ' kpoints_init ', ' nkp and nkpts differs ', nkpts )
      wtkpt( 1 : nkpts ) = ONE/DBLE( nkpts )
      wtktot = SUM( wtkpt( 1 : nkpts ) )

      ! ... Setup the shells of b-vectors around each K-point
      ! 
      IF ( bshell_ ) CALL bshells(recc, nshells, nwhich)

    RETURN
   END SUBROUTINE


!**********************************************************
   SUBROUTINE kpoints_deallocate()
   !**********************************************************
   IMPLICIT NONE
    INTEGER   :: ierr

    IF ( ALLOCATED( vkpt ) ) THEN
       DEALLOCATE( vkpt, STAT=ierr )
       IF (ierr/=0) CALL errore('kpoints_deallocate','deallocating vkpt',ABS(ierr))
    ENDIF
    IF ( ALLOCATED( wtkpt ) ) THEN
       DEALLOCATE( wtkpt, STAT=ierr )
       IF (ierr/=0) CALL errore('kpoints_deallocate','deallocating wtkpt',ABS(ierr))
    ENDIF

    IF ( ALLOCATED( nntot ) ) THEN
       DEALLOCATE( nntot, STAT=ierr )
       IF (ierr/=0) CALL errore('kpoints_deallocate','deallocating nntot',ABS(ierr))
    ENDIF
    IF ( ALLOCATED( nnshell ) ) THEN
       DEALLOCATE( nnshell, STAT=ierr )
       IF (ierr/=0) CALL errore('kpoints_deallocate','deallocating nnshell',ABS(ierr))
    ENDIF
    IF ( ALLOCATED( nnlist ) ) THEN
       DEALLOCATE( nnlist, STAT=ierr )
       IF (ierr/=0) CALL errore('kpoints_deallocate','deallocating nnlist',ABS(ierr))
    ENDIF
    IF ( ALLOCATED( nncell ) ) THEN
       DEALLOCATE( nncell, STAT=ierr )
       IF (ierr/=0) CALL errore('kpoints_deallocate','deallocating nncell',ABS(ierr))
    ENDIF
    IF ( ALLOCATED( neigh ) ) THEN
       DEALLOCATE( neigh, STAT=ierr )
       IF (ierr/=0) CALL errore('kpoints_deallocate','deallocating neigh',ABS(ierr))
    ENDIF
    IF ( ALLOCATED( bk ) ) THEN
       DEALLOCATE( bk, STAT=ierr )
       IF (ierr/=0) CALL errore('kpoints_deallocate','deallocating bk',ABS(ierr))
    ENDIF
    IF ( ALLOCATED( wb ) ) THEN
       DEALLOCATE( wb, STAT=ierr )
       IF (ierr/=0) CALL errore('kpoints_deallocate','deallocating wb',ABS(ierr))
    ENDIF
    IF ( ALLOCATED( dnn ) ) THEN
       DEALLOCATE( dnn, STAT=ierr )
       IF (ierr/=0) CALL errore('kpoints_deallocate','deallocating dnn',ABS(ierr))
    ENDIF
    IF ( ALLOCATED( bka ) ) THEN
       DEALLOCATE( bka, STAT=ierr )
       IF (ierr/=0) CALL errore('kpoints_deallocate','deallocating bka',ABS(ierr))
    ENDIF
    alloc = .FALSE.

   END SUBROUTINE kpoints_deallocate

END MODULE kpoints_module
