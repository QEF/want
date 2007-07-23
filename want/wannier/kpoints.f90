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
   !
   USE kinds,             ONLY : dbl
   USE constants,         ONLY : ONE, TWO, TPI, EPS_m6
   USE parameters,        ONLY : nstrx, nnx
   USE io_module,         ONLY : stdout
   USE log_module,        ONLY : log_push, log_pop
   USE lattice_module,    ONLY : alat, avec, bvec, lattice_alloc => alloc
   USE converters_module, ONLY : cart2cry
   USE qexml_module
   USE qexpt_module
   !
   IMPLICIT NONE
   PRIVATE
   SAVE

! This module handles kpoints data (including the treatment
! of their nearest neighbors b, bshells).
!
! routines in this module:
! SUBROUTINE kpoints_allocate()
! SUBROUTINE kpoints_deallocate()
! SUBROUTINE kpoints_init()
! SUBROUTINE bshells_allocate()
! SUBROUTINE bshells_deallocate()
! SUBROUTINE kpoints_read_ext( unit, name, found )

!
! declarations
!

  !
  ! ... usual kpt data (k vectors)
  INTEGER                 :: nkpts         ! kpt number (NOT doubled when nspin=2)
  !
  INTEGER                 :: iks           ! the starting ik (at the current spin)
  INTEGER                 :: ike           ! the ending ik (at the current spin)
  !
  INTEGER                 :: nk(3)         ! component of the MP kgrid
  INTEGER                 :: s(3)          ! fractional shifts of the MP grid
  REAL(dbl), ALLOCATABLE  :: vkpt(:,:)     ! kpt components; DIM: 3*nkpts (Bohr^-1)
  REAL(dbl), ALLOCATABLE  :: wk(:)         ! weight of each kpt for BZ sums 
  REAL(dbl)               :: wksum         ! sum of the weights

  LOGICAL :: kpoints_alloc = .FALSE.

  !
  ! ... Real space lattice vectors (R vectors)
  INTEGER                 :: nrtot         ! total number R vects
  INTEGER                 :: nr(3)         ! uniform grid of R vectors
  INTEGER,   ALLOCATABLE  :: ivr(:,:)      ! R vector components (crystal)
  REAL(dbl), ALLOCATABLE  :: vr(:,:)       ! R vector components (bohr)
  REAL(dbl), ALLOCATABLE  :: wr(:)         ! R vector weights 

  !
  ! ... Nearest neighbor data (b vectors)
  INTEGER                 :: nb            ! number of neighbours
  INTEGER,   ALLOCATABLE  :: nnlist(:,:)   ! k+b list for each k, DIM: nb*nkpts
  INTEGER,   ALLOCATABLE  :: nncell(:,:,:) ! k+b cell, DIM: 3*nb*nkpts
  INTEGER,   ALLOCATABLE  :: nnrev(:)      ! -b index for each b, DIM: nb
  INTEGER,   ALLOCATABLE  :: nnpos(:)      ! DIM: nb/2 "positive" def b-vectors
  REAL(dbl), ALLOCATABLE  :: vb (:,:)      ! b coords, DIM: 3*nb (bohr^-1)
  REAL(dbl), ALLOCATABLE  :: wb(:)         ! b-weights, DIM: nb
  REAL(dbl)               :: wbtot         ! sum of the b-weights

  LOGICAL :: bshells_alloc = .FALSE.


!
! end of declaration scope 
!

  PUBLIC :: nkpts
  PUBLIC :: iks, ike
  PUBLIC :: nk, s
  PUBLIC :: vkpt
  PUBLIC :: wk, wksum
  !
  PUBLIC :: nrtot, nr
  PUBLIC :: ivr, vr, wr
  !
  PUBLIC :: nb
  PUBLIC :: vb
  PUBLIC :: wb
  PUBLIC :: wbtot
  PUBLIC :: nnlist
  PUBLIC :: nncell
  PUBLIC :: nnrev
  PUBLIC :: nnpos
  !
  PUBLIC :: kpoints_alloc, bshells_alloc
  !
  PUBLIC :: kpoints_read_ext
  PUBLIC :: kpoints_init
  PUBLIC :: kpoints_deallocate
  PUBLIC :: bshells_allocate
  PUBLIC :: bshells_deallocate


CONTAINS

!**********************************************************
   SUBROUTINE kpoints_allocate()
   !**********************************************************
   IMPLICIT NONE
      INTEGER   :: ierr
      CHARACTER(16)     :: subname="kpoints_allocate"

      CALL log_push ( subname )
      !
      ! kpt data
      !
      IF ( nkpts <= 0) CALL errore(subname,'Invalid NKPTS',ABS(nkpts)+1)
      !
      ALLOCATE( vkpt(3,nkpts),STAT=ierr )
      IF (ierr/=0) CALL errore(subname,'allocating vkpt',3*nkpts)
      !
      ALLOCATE( wk(nkpts),STAT=ierr )
      IF (ierr/=0) CALL errore(subname,'allocating wk',nkpts)

      kpoints_alloc = .TRUE.
      !
      CALL log_pop( subname )
      !
   END SUBROUTINE kpoints_allocate


!**********************************************************
   SUBROUTINE kpoints_init( )
   !**********************************************************
   IMPLICIT NONE
      INTEGER           :: ierr
      CHARACTER(12)     :: subname="kpoints_init"
      REAL(dbl), ALLOCATABLE :: vr_cry(:,:)

      CALL log_push ( subname )
      !
      IF ( .NOT. lattice_alloc ) CALL errore(subname,'lattice NOT alloc',1) 
      IF ( .NOT. kpoints_alloc ) CALL errore(subname,'kpoints NOT alloc',1) 

      !
      ! get the monkhorst pack grid
      !
      CALL get_monkpack(nk,s,nkpts,vkpt,'CARTESIAN',bvec,ierr)
      IF ( ierr /= 0) CALL errore(subname,'kpt grid not Monkhorst-Pack',ABS(ierr))

      !
      ! init the R vectors, corresponding to the given MP grid
      !
      nr(1:3) = nk(1:3)
      nrtot   = PRODUCT(nr)
      !
      ALLOCATE( vr( 3, nrtot ), STAT=ierr )
      IF ( ierr /= 0) CALL errore(subname,'allocating vr',ABS(ierr))
      !
      ALLOCATE( ivr( 3, nrtot ), STAT=ierr )
      IF ( ierr /= 0) CALL errore(subname,'allocating ivr',ABS(ierr))
      !
      ALLOCATE( wr( nrtot ), STAT=ierr )
      IF ( ierr /= 0) CALL errore(subname,'allocating wr',ABS(ierr))
      !
      !
      CALL get_rgrid(nr, nrtot, wr, vr, avec )

      !
      ! compute ivr, crystal compononet of vr
      !
      ALLOCATE( vr_cry( 3, nrtot ), STAT=ierr )
      IF ( ierr /= 0) CALL errore(subname,'allocating vr_cry',ABS(ierr))
      !
      vr_cry(:,:) = vr(:,:) 
      CALL cart2cry(vr_cry, avec)
      !
      ivr(:,:) = NINT( vr_cry(:,:) )
      !
      DEALLOCATE( vr_cry, STAT=ierr )
      IF ( ierr /= 0) CALL errore(subname,'deallocating vr_cry',ABS(ierr))
      
      CALL log_pop ( subname )
      !
   END SUBROUTINE kpoints_init


!**********************************************************
   SUBROUTINE bshells_allocate( )
   !**********************************************************
   IMPLICIT NONE
      INTEGER   :: ierr
      CHARACTER(16)     :: subname="bshells_allocate"

      CALL log_push ( subname )
      !
      IF ( nkpts <= 0)  CALL errore(subname,'Invalid NKPTS',ABS(nkpts)+1)
      IF ( nb <= 0 .OR. nb > nnx )  CALL errore(subname,'Invalid NB',ABS(nb)+1)

      ALLOCATE( nnlist(nb,nkpts), STAT = ierr )
      IF( ierr /=0 ) CALL errore(subname, 'allocating nnlist', ABS(ierr) )
      !
      ALLOCATE( nncell(3,nb,nkpts), STAT = ierr )
      IF( ierr /=0 ) CALL errore(subname, 'allocating nncell', ABS(ierr) )
      !
      ALLOCATE( nnrev(nb), STAT = ierr )
      IF( ierr /=0 ) CALL errore(subname, 'allocating nnrev', ABS(ierr) )
      !
      ALLOCATE( vb(3,nb), STAT = ierr )
      IF( ierr /=0 ) CALL errore(subname, 'allocating vb', ABS(ierr) )
      !
      ALLOCATE( nnpos(nb/2), STAT = ierr )
      IF( ierr /=0 ) CALL errore(subname, 'allocating nnpos', ABS(ierr) )
      !
      ALLOCATE( wb(nb), STAT = ierr )
      IF( ierr /=0 ) CALL errore(subname, 'allocating wb', ABS(ierr) )

      bshells_alloc = .TRUE.
      !
      CALL log_pop ( subname )
      !
   END SUBROUTINE bshells_allocate


!**********************************************************
   SUBROUTINE kpoints_read_ext( filefmt )
   !**********************************************************
    IMPLICIT NONE
       CHARACTER(*),      INTENT(in) :: filefmt
       !
       CHARACTER(16)      :: subname='kpoints_read_ext'
       INTEGER            :: nspin, ik, ierr
       LOGICAL            :: lsda, lfound
       
       CALL log_push ( subname )
       !
       IF ( kpoints_alloc ) CALL kpoints_deallocate()
       !
       !
       SELECT CASE ( TRIM(filefmt) )
       !
       CASE ( 'qexml' )
            !
            CALL qexml_read_spin( LSDA=lsda, IERR=ierr )
            IF ( ierr/=0) CALL errore(subname,'getting spin dimensions',ABS(ierr))
            !
            nspin = 1
            IF ( lsda ) nspin = 2
            !
            CALL qexml_read_bz( NUM_K_POINTS=nkpts, IERR=ierr )
            !
       CASE ( 'pw_export' )
            !
            CALL qexpt_read_spin( NSPIN=nspin, IERR=ierr )
            IF ( ierr/=0) CALL errore(subname,'getting spin dimensions',ABS(ierr))
            !
            CALL qexpt_read_bz( NUM_K_POINTS=nkpts, IERR=ierr )
            !
       CASE DEFAULT
            !
            CALL errore(subname,'invalid filefmt = '//TRIM(filefmt), 1)
       END SELECT
       !
       IF ( ierr/=0 )   CALL errore(subname,'getting bz dimensions',ABS(ierr))
       IF ( nkpts <=0 ) CALL errore(subname,'invalid nkpts',1)  
       IF ( nspin <=0 ) CALL errore(subname,'invalid nspin',2)  
       IF ( nspin > 2 ) CALL errore(subname,'invalid nspin',3)  
       !
       !
       CALL kpoints_allocate( )
      
       !
       ! massive data reading
       !
       SELECT CASE ( TRIM(filefmt) )
       !
       CASE ( 'qexml' )
            !
            CALL qexml_read_bz( XK=vkpt, WK=wk, IERR=ierr )
            !
       CASE ( 'pw_export' )
            !
            CALL qexpt_read_bz( XK=vkpt, WK=wk, IERR=ierr )
            !
       CASE DEFAULT
            !
            CALL errore(subname,'invalid filefmt = '//TRIM(filefmt), 2)
       END SELECT

       !
       ! due to a different convention in Espresso
       wk(:) = wk(:) * REAL( nspin, dbl) / TWO 
       wksum = SUM(wk(:))
       !
       lfound = .FALSE.
       DO ik = 1, nkpts
          !
          IF( ABS( wk(ik) - ONE/REAL(nkpts, dbl) ) > EPS_m6 ) lfound = .TRUE.
       ENDDO
       !
       IF ( lfound ) THEN
          !
          CALL warning( stdout, 'Invalid kpt weights from DFT data. Recalculated')
          wk(:) = ONE/REAL(nkpts, dbl)
       ENDIF

       !
       ! convert them to bohr^-1 cartesian units 
       vkpt(:,:) = vkpt(:,:) * TPI/alat
       !
       CALL log_pop ( subname )
       !
   END SUBROUTINE kpoints_read_ext


!**********************************************************
   SUBROUTINE kpoints_deallocate()
   !**********************************************************
   IMPLICIT NONE
       INTEGER          :: ierr
       CHARACTER(18)    :: subname='kpoints_deallocate'
   
       CALL log_push( subname )
       !
       IF ( ALLOCATED( vkpt ) ) THEN
          DEALLOCATE( vkpt, STAT=ierr )
          IF (ierr/=0) CALL errore(subname,'deallocating vkpt',ABS(ierr))
       ENDIF
       IF ( ALLOCATED( wk ) ) THEN
          DEALLOCATE( wk, STAT=ierr )
          IF (ierr/=0) CALL errore(subname,'deallocating wk',ABS(ierr))
       ENDIF
       !
       IF ( ALLOCATED( ivr ) ) THEN
          DEALLOCATE( ivr, STAT=ierr )
          IF (ierr/=0) CALL errore(subname,'deallocating ivr',ABS(ierr))
       ENDIF
       IF ( ALLOCATED( vr ) ) THEN
          DEALLOCATE( vr, STAT=ierr )
          IF (ierr/=0) CALL errore(subname,'deallocating vr',ABS(ierr))
       ENDIF
       IF ( ALLOCATED( wr ) ) THEN
          DEALLOCATE( wr, STAT=ierr )
          IF (ierr/=0) CALL errore(subname,'deallocating wr',ABS(ierr))
       ENDIF
       !
       kpoints_alloc = .FALSE.
       !
       CALL log_pop( subname )
       !
   END SUBROUTINE kpoints_deallocate


!**********************************************************
   SUBROUTINE bshells_deallocate()
   !**********************************************************
   IMPLICIT NONE
       INTEGER   :: ierr
       CHARACTER(18)    :: subname='bshells_deallocate'
   
       CALL log_push( subname )
       !
       IF ( ALLOCATED( nnlist ) ) THEN
          DEALLOCATE( nnlist, STAT=ierr )
          IF (ierr/=0) CALL errore(subname,'deallocating nnlist',ABS(ierr))
       ENDIF
       IF ( ALLOCATED( nncell ) ) THEN
          DEALLOCATE( nncell, STAT=ierr )
          IF (ierr/=0) CALL errore(subname,'deallocating nncell',ABS(ierr))
       ENDIF
       IF ( ALLOCATED( nnrev ) ) THEN
          DEALLOCATE( nnrev, STAT=ierr )
          IF (ierr/=0) CALL errore(subname,'deallocating nnrev',ABS(ierr))
       ENDIF
       IF ( ALLOCATED( vb ) ) THEN
          DEALLOCATE( vb, STAT=ierr )
          IF (ierr/=0) CALL errore(subname,'deallocating vb',ABS(ierr))
       ENDIF
       IF ( ALLOCATED( nnpos ) ) THEN
          DEALLOCATE( nnpos, STAT=ierr )
          IF (ierr/=0) CALL errore(subname,'deallocating nnpos',ABS(ierr))
       ENDIF
       IF ( ALLOCATED( wb ) ) THEN
          DEALLOCATE( wb, STAT=ierr )
          IF (ierr/=0) CALL errore(subname,'deallocating wb',ABS(ierr))
       ENDIF
       !
       bshells_alloc = .FALSE.
       !
       CALL log_pop( subname )
       !
   END SUBROUTINE bshells_deallocate

END MODULE kpoints_module
