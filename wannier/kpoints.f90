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
   USE constants,         ONLY : ZERO, ONE, TWO, TPI, EPS_m6, BOHR => bohr_radius_angs
   USE parameters,        ONLY : nstrx, nnx, nkptx => npkx
   USE io_module,         ONLY : stdout
   USE log_module,        ONLY : log_push, log_pop
   USE lattice_module,    ONLY : alat, avec, bvec, lattice_alloc => alloc
   USE parser_module,     ONLY : change_case
   USE converters_module, ONLY : cart2cry, cry2cart
   USE io_global_module,  ONLY : ionode, ionode_id
   USE mp_global,         ONLY : mpime, nproc
   USE mp,                ONLY : mp_bcast
   USE paratools_module,  ONLY : para_get_poolindex
   USE grids_module,      ONLY : grids_get_rgrid
   !
   USE qexml_module
   USE qexpt_module
   USE crystal_io_module
   USE wannier90_tools_module
   USE internal_tools_module
   !
#ifdef __ETSF_IO
   USE etsf_io
   USE etsf_io_tools
   USE etsf_io_data_module
#endif
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
  INTEGER                 :: nkpts         ! number of kpts (NOT doubled when nspin=2)
  INTEGER                 :: nkpts_g       ! global number of kpts (/= nkpts for parallel runs)
  INTEGER                 :: nkpts_all     ! number of kpts over the whole BZ
  !
  INTEGER                 :: iks           ! the starting ik (at the current spin)
  INTEGER                 :: ike           ! the ending ik (at the current spin)
  INTEGER,   ALLOCATABLE  :: iproc_g(:)    ! index of the proc of ik, DIM: nkpts_g
  !
  INTEGER                 :: nk(3)         ! component of the MP kgrid
  INTEGER                 :: s(3)          ! fractional shifts of the MP grid
  REAL(dbl), ALLOCATABLE  :: vkpt(:,:)     ! kpt components; DIM: 3*nkpts (Bohr^-1)
  REAL(dbl), ALLOCATABLE  :: vkpt_g(:,:)   ! global kpt components (parallel case); DIM: 3*nkpts_g (Bohr^-1)
  REAL(dbl), ALLOCATABLE  :: vkpt_all(:,:) ! kpt components (whole BZ); DIM: 3*nkpts_all (Bohr^-1)
  REAL(dbl), ALLOCATABLE  :: wk(:)         ! kpt weights 
  REAL(dbl), ALLOCATABLE  :: wk_g(:)       ! kpt weights (global)
  REAL(dbl)               :: wksum         ! sum of the weights
  LOGICAL                 :: kgrid_from_file = .FALSE.     ! whether nk,s are read or not

  LOGICAL :: kpoints_alloc = .FALSE.

  !
  ! ... Real space lattice vectors (R vectors)
  INTEGER                 :: nrtot         ! total number R vects
  INTEGER                 :: nr(3)         ! generators of the uniform Rgrid
  INTEGER,   ALLOCATABLE  :: ivr(:,:)      ! R vector components (crystal)
  REAL(dbl), ALLOCATABLE  :: vr(:,:)       ! R vector components (bohr)
  REAL(dbl), ALLOCATABLE  :: wr(:)         ! R vector weights 
  LOGICAL                 :: rgrid_from_file = .FALSE.     ! whether nr,wr are read or not

  LOGICAL :: rgrid_alloc = .FALSE.

  !
  ! ... Nearest neighbor data (b vectors)
  INTEGER                 :: nb            ! number of neighbours
  INTEGER,   ALLOCATABLE  :: nnlist(:,:)   ! k+b list for each k, DIM: nb*nkpts_g
  INTEGER,   ALLOCATABLE  :: nncell(:,:,:) ! k+b cell, DIM: 3*nb*nkpts_g
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
  PUBLIC :: iks, ike, iproc_g
  PUBLIC :: nk, s
  PUBLIC :: vkpt
  PUBLIC :: wk, wksum
  PUBLIC :: nkpts_g, vkpt_g, wk_g
  PUBLIC :: nkpts_all, vkpt_all
  PUBLIC :: kgrid_from_file
  !
  PUBLIC :: nrtot, nr
  PUBLIC :: ivr, vr, wr
  PUBLIC :: rgrid_from_file
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
  PUBLIC :: kpoints_alloc, rgrid_alloc, bshells_alloc
  !
  PUBLIC :: kpoints_read_ext
  PUBLIC :: kpoints_init
  PUBLIC :: kpoints_allocate
  PUBLIC :: kpoints_deallocate
  PUBLIC :: kpoints_memusage
  !
  PUBLIC :: rgrid_allocate
  PUBLIC :: rgrid_deallocate
  PUBLIC :: rgrid_memusage
  !
  PUBLIC :: bshells_allocate
  PUBLIC :: bshells_deallocate
  PUBLIC :: bshells_memusage


CONTAINS

!**********************************************************
   SUBROUTINE kpoints_allocate( klocal, kglobal )
   !**********************************************************
   IMPLICIT NONE
      !
      LOGICAL,   INTENT(IN) :: klocal, kglobal
      !
      INTEGER       :: ierr
      CHARACTER(16) :: subname="kpoints_allocate"

      CALL log_push ( subname )

      IF ( nkpts   > nkptx ) CALL errore(subname,'too many kpts', 10)
      IF ( nkpts_g > nkptx ) CALL errore(subname,'too many kpts', 11)
      !
      IF ( nkpts_g < nproc ) CALL warning(subname,'too many CPU used')

      !
      ! local quantities (within each pool)
      !
      IF ( klocal ) THEN
          !
          IF ( nkpts < 0) CALL errore(subname,'Invalid NKPTS',ABS(nkpts)+1)
          !
          ALLOCATE( vkpt(3,nkpts),STAT=ierr )
          IF (ierr/=0) CALL errore(subname,'allocating vkpt',ABS(ierr))
          !
          ALLOCATE( wk(nkpts),STAT=ierr )
          IF (ierr/=0) CALL errore(subname,'allocating wk',ABS(ierr))
          !
      ENDIF

      !
      ! global quantities (over the kpt pools)
      !
      IF ( kglobal ) THEN
          !
          IF ( nkpts_g <= 0) CALL errore(subname,'Invalid NKPTS_G',ABS(nkpts_g)+1)
          !
          ALLOCATE( vkpt_g(3,nkpts_g),STAT=ierr )
          IF (ierr/=0) CALL errore(subname,'allocating vkpt_g',ABS(ierr))
          !
          ALLOCATE( wk_g(nkpts_g),STAT=ierr )
          IF (ierr/=0) CALL errore(subname,'allocating wk_g',ABS(ierr))
          !
          ALLOCATE( iproc_g(nkpts_g),STAT=ierr )
          IF (ierr/=0) CALL errore(subname,'allocating iproc_g',ABS(ierr))
          !
      ENDIF
      !
      kpoints_alloc = .TRUE.
      !
      CALL log_pop( subname )
      !
   END SUBROUTINE kpoints_allocate


!**********************************************************
   SUBROUTINE kpoints_init( )
   !**********************************************************
   !
   ! init kpts, 
   !   - manage symmetries
   !   - manage parallelism over kpts
   !
   IMPLICIT NONE
      CHARACTER(12)     :: subname="kpoints_init"
      LOGICAL           :: lfound
      INTEGER           :: ierr, ik
      REAL(dbl), ALLOCATABLE :: vr_cry(:,:)

      CALL log_push ( subname )
      !
      IF ( .NOT. lattice_alloc ) CALL errore(subname,'lattice NOT alloc',1) 
      IF ( .NOT. kpoints_alloc ) CALL errore(subname,'kpoints NOT alloc',1) 

      !
      ! get the kpt grid over the full BZ
      !
      CALL symmetrize_kgrid() 

      !
      ! set data for parallelism
      !
      DO ik = 1, nkpts_g
          !
          CALL para_get_poolindex( iproc_g(ik),  ik, nkpts_g )
          !
      ENDDO


      !
      ! set the correct weight if the case
      !
      lfound = .FALSE.
      DO ik = 1, nkpts_g
          !
          IF( ABS( wk_g(ik) - ONE/REAL(nkpts_g, dbl) ) > EPS_m6 ) lfound = .TRUE.
          !
      ENDDO
      !
      IF ( lfound ) THEN
          !
          WRITE(stdout, "()")
          CALL warning( subname, 'Invalid kpt weights from DFT data. Recalculated')
          wk_g(:) = ONE/REAL(nkpts_g, dbl)
          !
      ENDIF

      !
      ! if needed get the monkhorst pack grid
      ! when nk is read from file, no need to check the
      ! M-P grid here
      !
      IF ( kgrid_from_file ) THEN
          !
          IF ( ANY(nk(:) <= 0 ) ) CALL errore(subname,'invalid nk from file',10)
          !
      ELSE
          !
          CALL get_monkpack( nk, s, nkpts_g, vkpt_g,'CARTESIAN',bvec,ierr)
          IF ( ierr /= 0) CALL errore(subname,'kpt grid not Monkhorst-Pack',ABS(ierr))
          !
      ENDIF


      !
      ! init the R vectors, corresponding to the given MP grid
      ! nr is initialized with nk only if it is not read from file
      ! (see the case crystal in kpoints_read_ext)
      !
      nr(:) = -1
      !
      IF ( .NOT. rgrid_from_file ) THEN
          !
          !! consider only odd R grids
          !nr(1:3) = 2 * ( nk(1:3)/2 ) +1
          !
          nr(1:3) = nk(1:3)
          !
          ! get the grid simension
          CALL grids_get_rgrid(nr, NRTOT=nrtot )
          !
          CALL rgrid_allocate( )
          !
          CALL grids_get_rgrid(nr, WR=wr, IVR=ivr )
          !
          vr(:,:) = REAL( ivr, dbl)
          CALL cry2cart( vr, avec)
          !
      ELSE
          !
          IF ( .NOT. rgrid_alloc )    CALL errore(subname, 'rgrid not alloc', 20)
          IF ( .NOT. ALLOCATED(vr) )  CALL errore(subname, 'vr not alloc',    10)
          IF ( .NOT. ALLOCATED(ivr) ) CALL errore(subname, 'ivr not alloc',   11)
          IF ( .NOT. ALLOCATED(wr) )  CALL errore(subname, 'wr not alloc',    12)
         
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
          !
      ENDIF
      !


      !
      ! determine nr if needed
      ! (nr is taken from ivr)
      !
      IF ( rgrid_from_file ) THEN
          !
          nr ( 1 ) = MAXVAL( ivr(1,:) ) - MINVAL( ivr(1,:) ) +1
          nr ( 2 ) = MAXVAL( ivr(2,:) ) - MINVAL( ivr(2,:) ) +1
          nr ( 3 ) = MAXVAL( ivr(3,:) ) - MINVAL( ivr(3,:) ) +1           
          !
      ENDIF
      ! 
      CALL log_pop ( subname )
      !
   END SUBROUTINE kpoints_init


!**********************************************************
   SUBROUTINE rgrid_allocate( )
   !**********************************************************
   IMPLICIT NONE
      INTEGER   :: ierr
      CHARACTER(14)     :: subname="rgrid_allocate"

      CALL log_push ( subname )
      !
      IF ( nrtot <= 0)  CALL errore(subname,'Invalid nrtot',ABS(nrtot)+1)
      !
      ALLOCATE( vr(3, nrtot), STAT=ierr )
      IF (ierr/=0) CALL errore(subname,'allocating vr',ABS(ierr))
      !
      ALLOCATE( ivr(3, nrtot), STAT=ierr )
      IF (ierr/=0) CALL errore(subname,'allocating ivr',ABS(ierr))
      !
      ALLOCATE( wr(nrtot), STAT=ierr )
      IF (ierr/=0) CALL errore(subname,'allocating wr',ABS(ierr))
      !
      rgrid_alloc = .TRUE.
      !
      CALL log_pop ( subname )
      !
   END SUBROUTINE rgrid_allocate


!**********************************************************
   SUBROUTINE bshells_allocate( )
   !**********************************************************
   IMPLICIT NONE
      INTEGER   :: ierr
      CHARACTER(16)     :: subname="bshells_allocate"

      CALL log_push ( subname )
      !
      IF ( nkpts_g <= 0)  CALL errore(subname,'Invalid NKPTS',ABS(nkpts_g)+1)
      IF ( nb <= 0 .OR. nb > nnx )  CALL errore(subname,'Invalid NB',ABS(nb)+1)

      ALLOCATE( nnlist(nb,nkpts_g), STAT = ierr )
      IF( ierr /=0 ) CALL errore(subname, 'allocating nnlist', ABS(ierr) )
      !
      ALLOCATE( nncell(3,nb,nkpts_g), STAT = ierr )
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
       CHARACTER(16)       :: subname='kpoints_read_ext'
       CHARACTER(256)      :: k_units, r_units, b_units
       REAL(dbl)           :: lbvec(3,3)
       INTEGER             :: ierr
       !
#ifdef __ETSF_IO
       REAL(dbl)           :: volume
       TYPE(etsf_kpoints)  :: kpoints
       TYPE(etsf_geometry) :: geometry
       !
       DOUBLE PRECISION, ALLOCATABLE, TARGET :: primitive_vectors(:,:)
       DOUBLE PRECISION, ALLOCATABLE, TARGET :: reduced_coordinates_of_kpoints(:,:)
       DOUBLE PRECISION, ALLOCATABLE, TARGET :: kpoint_weights(:)
#endif
       
       CALL log_push ( subname )
       !
       IF ( kpoints_alloc ) CALL kpoints_deallocate()
       !
       !
       SELECT CASE ( TRIM(filefmt) )
       !
       CASE ( 'qexml' )
            !
            IF (ionode) CALL qexml_read_bz( NUM_K_POINTS=nkpts_g, IERR=ierr )
            CALL mp_bcast( nkpts_g,   ionode_id )
            CALL mp_bcast( ierr,      ionode_id )
            IF ( ierr/=0 )   CALL errore(subname,'QEXML: getting bz dimensions',ABS(ierr))
            !
            ! assuming k_units are not crystal
            lbvec = 0.0
            !
            kgrid_from_file = .FALSE.
            rgrid_from_file = .FALSE.
            !
       CASE ( 'pw_export' )
            !
            IF (ionode) CALL qexpt_read_bz( NUM_K_POINTS=nkpts_g, IERR=ierr )
            CALL mp_bcast( nkpts_g,   ionode_id )
            CALL mp_bcast( ierr,      ionode_id )
            IF ( ierr/=0 )   CALL errore(subname,'QEXPT: getting bz dimensions',ABS(ierr))
            !
            ! assuming k_units are not crystal
            lbvec = 0.0
            !
            kgrid_from_file = .FALSE.
            rgrid_from_file = .FALSE.
            !
       CASE ( 'etsf_io' )
            !
#ifdef __ETSF_IO
            !
            nkpts_g = dims%number_of_kpoints
            !
            kgrid_from_file = .FALSE.
            rgrid_from_file = .FALSE.
            !
            ALLOCATE( primitive_vectors(dims%number_of_cartesian_directions, &
                                        dims%number_of_vectors) )
            !
            geometry%primitive_vectors                   => primitive_vectors
            IF (ionode) CALL etsf_io_geometry_get(ncid, geometry, lstat, error_data)
            !
            geometry%primitive_vectors                   => null()
            !
            CALL mp_bcast( primitive_vectors,   ionode_id )
            CALL mp_bcast( lstat,               ionode_id )
            !
            IF(.NOT.lstat) CALL etsf_error(error_data,subname,'ETSF_IO: reading geometry',10)
            !
            CALL recips( primitive_vectors(:,1), primitive_vectors(:,2), primitive_vectors(:,3), & 
                         lbvec(:,1), lbvec(:,2), lbvec(:,3), volume )
            !
            lbvec = lbvec * TPI
            !
            DEALLOCATE( primitive_vectors )
            !
#else
            CALL errore(subname,'ETSF_IO not configured',10)
#endif
            !
       CASE ( 'crystal' )
            !
            IF (ionode) CALL crio_open_section( "GEOMETRY", ACTION='read', IERR=ierr )
            CALL mp_bcast( ierr,      ionode_id )
            IF ( ierr/=0 ) CALL errore(subname, 'CRIO: opening sec. GEOMETRY', ABS(ierr) )
            !
            IF (ionode) CALL crio_read_periodicity( BVEC=lbvec, B_UNITS=b_units, IERR=ierr)
            CALL mp_bcast( lbvec,     ionode_id )
            CALL mp_bcast( b_units,   ionode_id )
            CALL mp_bcast( ierr,      ionode_id )
            IF ( ierr/=0 )   CALL errore(subname,'CRIO: getting bvec',ABS(ierr))
            !
            IF (ionode) CALL crio_close_section( "GEOMETRY", ACTION='read', IERR=ierr )
            CALL mp_bcast( ierr,      ionode_id )
            IF ( ierr/=0 ) CALL errore(subname, 'CRIO: closing sec. GEOMETRY', ABS(ierr) )
            !
            !
            CALL change_case( b_units, 'lower' )
            !
            SELECT CASE ( TRIM(b_units) )
            CASE ( 'au', 'bohr^-1' )
                !
                ! do nothing
            CASE ( 'ang^-1' ) 
                bvec = bvec * BOHR
            CASE DEFAULT
                CALL errore(subname, 'invalid b_units: '//TRIM(b_units), 10 )
            END SELECT
            !
            IF (ionode) CALL crio_open_section( "METHOD", ACTION='read', IERR=ierr )
            CALL mp_bcast( ierr,      ionode_id )
            IF ( ierr/=0 ) CALL errore(subname, 'CRIO: opening sec. METHOD', ABS(ierr) )
            !
            ! read Rgrid dimensions from file
            !
            IF (ionode) CALL crio_read_direct_lattice( NRTOT=nrtot, IERR=ierr )
            CALL mp_bcast( nrtot,     ionode_id )
            CALL mp_bcast( ierr,      ionode_id )
            IF ( ierr/=0 )   CALL errore(subname,'CRIO: getting bz dimensions',ABS(ierr))
            !
            IF (ionode) CALL crio_read_bz( NUM_K_POINTS=nkpts_g, NK=nk, IERR=ierr )
            CALL mp_bcast( nkpts_g,   ionode_id )
            CALL mp_bcast( nk,        ionode_id )
            IF ( ierr/=0 )   CALL errore(subname,'CRIO: getting bz dimensions',ABS(ierr))
            !
            s(1:3) = 0
            !
            kgrid_from_file = .TRUE.
            rgrid_from_file = .TRUE.
            !
       CASE( 'wannier90' )
            !
            IF (ionode) CALL wannier90_tools_get_dims( NKPTS=nkpts_g )
            CALL mp_bcast( nkpts_g,   ionode_id )
            !
       CASE( 'internal' )
            !
            IF (ionode) CALL internal_tools_get_dims( NKPTS=nkpts_g )
            CALL mp_bcast( nkpts_g,   ionode_id )
            !
       CASE DEFAULT
            !
            CALL errore(subname,'invalid filefmt = '//TRIM(filefmt), 1)
            !
       END SELECT
       !
       IF ( nkpts_g > nkptx ) CALL errore(subname,'too many kpts', 12)
       !
       !
       CALL kpoints_allocate( KLOCAL = .FALSE., KGLOBAL = .TRUE.)
       IF( rgrid_from_file ) CALL rgrid_allocate( )
      
       !
       ! massive data reading
       !
       SELECT CASE ( TRIM(filefmt) )
       !
       CASE ( 'qexml' )
            !
            IF (ionode) CALL qexml_read_bz( XK=vkpt_g, WK=wk_g, K_UNITS=k_units, IERR=ierr )
            CALL mp_bcast( vkpt_g,    ionode_id )
            CALL mp_bcast( wk_g,      ionode_id )
            CALL mp_bcast( k_units,   ionode_id )
            CALL mp_bcast( ierr,      ionode_id )
            IF ( ierr/=0 )   CALL errore(subname,'QEXML: reading bz',ABS(ierr))
            !
       CASE ( 'pw_export' )
            !
            IF (ionode) CALL qexpt_read_bz( XK=vkpt_g, WK=wk_g, IERR=ierr )
            CALL mp_bcast( vkpt_g,    ionode_id )
            CALL mp_bcast( wk_g,      ionode_id )
            CALL mp_bcast( ierr,      ionode_id )
            IF ( ierr/=0 )   CALL errore(subname,'QEXPT: reading bz',ABS(ierr))
            !
            k_units = '2 pi / alat'
            !
       CASE ( 'etsf_io' )
            !
#ifdef __ETSF_IO
            !
            ALLOCATE( reduced_coordinates_of_kpoints( dims%number_of_cartesian_directions, &
                                                      dims%number_of_kpoints) )
            ALLOCATE( kpoint_weights(dims%number_of_kpoints) )
            !
            kpoints%reduced_coordinates_of_kpoints  => reduced_coordinates_of_kpoints
            kpoints%kpoint_weights                  => kpoint_weights
            IF ( ionode ) CALL etsf_io_kpoints_get(ncid, kpoints, lstat, error_data)
            !
            kpoints%reduced_coordinates_of_kpoints  => null()
            kpoints%kpoint_weights                  => null()
            !
            CALL mp_bcast( reduced_coordinates_of_kpoints,   ionode_id )
            CALL mp_bcast( kpoint_weights,                   ionode_id )
            CALL mp_bcast( lstat,                            ionode_id )
            !
            IF ( .NOT. lstat ) CALL etsf_error(error_data,subname,'ETSF_IO reading data',10)
            !
            vkpt_g  = reduced_coordinates_of_kpoints
            wk_g    = kpoint_weights
            k_units = 'crystal'
            !
            DEALLOCATE( reduced_coordinates_of_kpoints )
            DEALLOCATE( kpoint_weights )
            !
#endif
            !
       CASE ( 'crystal' )
            !
            IF (ionode) CALL crio_read_direct_lattice( RVEC=vr, R_UNITS=r_units, IERR=ierr )
            CALL mp_bcast( vr,        ionode_id )
            CALL mp_bcast( r_units,   ionode_id )
            CALL mp_bcast( ierr,      ionode_id )
            IF ( ierr/=0 )   CALL errore(subname,'CRIO: reading rgrid',ABS(ierr))
            !
            wr(:) = ONE
            !
            CALL change_case( r_units, 'lower' )
            !
            SELECT CASE ( TRIM(r_units) )
            CASE ( 'au', 'bohr' )
                !
                ! do nothing
            CASE ( 'ang', 'angstrom' ) 
                vr = vr / BOHR
            CASE DEFAULT
                CALL errore(subname, 'invalid r_units: '//TRIM(r_units), 10 )
            END SELECT
            !
            !
            IF (ionode) CALL crio_read_bz( XK=vkpt_g, WK=wk_g, K_UNITS=k_units, IERR=ierr )
            CALL mp_bcast( vkpt_g,    ionode_id )
            CALL mp_bcast( wk_g,      ionode_id )
            CALL mp_bcast( k_units,   ionode_id )
            CALL mp_bcast( ierr,      ionode_id )
            IF ( ierr/=0 )   CALL errore(subname,'CRIO: reading bz',ABS(ierr))
            !
            IF (ionode) CALL crio_close_section( "METHOD", ACTION='read', IERR=ierr )
            CALL mp_bcast( ierr,      ionode_id )
            IF ( ierr/=0 ) CALL errore(subname, 'CRIO: closing sec. METHOD', ABS(ierr) )
            !
       CASE ( 'wannier90' )
            !
            IF (ionode) CALL wannier90_tools_get_kpoints( nkpts_g, VKPT=vkpt_g, &
                                                          WK=wk_g, BVEC=lbvec )
            CALL mp_bcast( vkpt_g,    ionode_id )
            CALL mp_bcast( wk_g,      ionode_id )
            CALL mp_bcast( lbvec,     ionode_id )
            !
            k_units = 'crystal'
            !
       CASE ( 'internal' )
            !
            IF (ionode) CALL internal_tools_get_kpoints( nkpts_g, VKPT=vkpt_g, &
                                                         WK=wk_g, BVEC=lbvec )
            CALL mp_bcast( vkpt_g,    ionode_id )
            CALL mp_bcast( wk_g,      ionode_id )
            CALL mp_bcast( lbvec,     ionode_id )
            !
            k_units = 'crystal'
            !
       CASE DEFAULT
            !
            CALL errore(subname,'invalid filefmt = '//TRIM(filefmt), 2)
            !
       END SELECT

       !
       ! manage units
       ! convert to bohr^-1 cartesian units  
       !
       IF ( LEN_TRIM(k_units) == 0 ) CALL errore(subname,'invalid k_units',1)  
       !
       CALL change_case( k_units, 'lower' )
       !
       SELECT CASE ( TRIM(k_units) )
       CASE ( 'relative', 'crystal' )
           !
           ! the espresso interfaces doesn't lead to
           ! crystal units; therefore the search for bvec is avoided
           ! and an error is given if we arrive here
           !
           IF ( TRIM(filefmt) == "qexml" .OR. TRIM(filefmt) == "pw_export" ) &
              CALL errore(subname,'unexpected units for kpts', 71)
           !
           ! do the actual conversion
           !
           CALL cry2cart( vkpt_g, lbvec )
           !
       CASE ( '2 pi / alat', '2 pi / a', '2pi/alat', '2pi/a' )
           !
           ! typical espresso units
           ! 
           vkpt_g(:,:) = vkpt_g(:,:) * TPI/alat
           !
       CASE ( 'au', 'bohr^-1' )
           ! do nothing
       CASE DEFAULT
           !
           CALL errore(subname,'invalid units for kpts: '//TRIM(k_units),72 ) 
           !
       END SELECT

       !
       ! weights should sum to one
       ! 
       wksum = SUM(wk_g(:))
       wk_g(:) = wk_g(:) / wksum
       !
       wksum = ONE

       !
       ! init vkpt and nkpts according to the parallelism 
       ! over the global kpt grid
       !
       CALL divide_et_impera( 1, nkpts_g, iks, ike, mpime, nproc  )
       nkpts = ike - iks + 1
       !
       CALL kpoints_allocate( KLOCAL=.TRUE., KGLOBAL=.FALSE. )  
       !
       vkpt( :, 1:nkpts ) = vkpt_g(:, iks:ike )
       wk( 1:nkpts )      = wk_g( iks:ike )


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
       IF ( ALLOCATED( vkpt_g ) ) THEN
          DEALLOCATE( vkpt_g, STAT=ierr )
          IF (ierr/=0) CALL errore(subname,'deallocating vkpt_g',ABS(ierr))
       ENDIF
       IF ( ALLOCATED( wk_g ) ) THEN
          DEALLOCATE( wk_g, STAT=ierr )
          IF (ierr/=0) CALL errore(subname,'deallocating wk_g',ABS(ierr))
       ENDIF
       !
       IF ( ALLOCATED( vkpt_all ) ) THEN
          DEALLOCATE( vkpt_all, STAT=ierr )
          IF (ierr/=0) CALL errore(subname,'deallocating vkpt_all',ABS(ierr))
       ENDIF
       !
       IF ( ALLOCATED( iproc_g ) ) THEN
          DEALLOCATE( iproc_g, STAT=ierr )
          IF (ierr/=0) CALL errore(subname,'deallocating iproc_g',ABS(ierr))
       ENDIF
       !
       kpoints_alloc = .FALSE.
       !
       CALL log_pop( subname )
       !
   END SUBROUTINE kpoints_deallocate


!**********************************************************
   REAL(dbl) FUNCTION kpoints_memusage()
   !**********************************************************
   IMPLICIT NONE
       !
       REAL(dbl) :: cost
       !
       cost = ZERO
       IF ( ALLOCATED(vkpt) )      cost = cost + REAL(SIZE(vkpt))      * 8.0_dbl
       IF ( ALLOCATED(wk) )        cost = cost + REAL(SIZE(wk))        * 8.0_dbl
       IF ( ALLOCATED(vkpt_g) )    cost = cost + REAL(SIZE(vkpt_g))    * 8.0_dbl
       IF ( ALLOCATED(wk_g) )      cost = cost + REAL(SIZE(wk_g))      * 8.0_dbl
       IF ( ALLOCATED(vkpt_all) )  cost = cost + REAL(SIZE(vkpt_all))  * 8.0_dbl
       IF ( ALLOCATED(iproc_g) )   cost = cost + REAL(SIZE(iproc_g))   * 4.0_dbl
       !
       kpoints_memusage = cost / 1000000.0_dbl
       !
   END FUNCTION kpoints_memusage


!**********************************************************
   SUBROUTINE rgrid_deallocate()
   !**********************************************************
   IMPLICIT NONE
       INTEGER          :: ierr
       CHARACTER(16)    :: subname='rgrid_deallocate'
   
       CALL log_push( subname )
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
       rgrid_alloc = .FALSE.
       !
       CALL log_pop( subname )
       !
   END SUBROUTINE rgrid_deallocate


!**********************************************************
   REAL(dbl) FUNCTION rgrid_memusage()
   !**********************************************************
   IMPLICIT NONE
       !
       REAL(dbl) :: cost
       !
       cost = ZERO
       IF ( ALLOCATED(ivr) )       cost = cost + REAL(SIZE(ivr))       * 4.0_dbl
       IF ( ALLOCATED(vr) )        cost = cost + REAL(SIZE(vr))        * 8.0_dbl
       IF ( ALLOCATED(wr) )        cost = cost + REAL(SIZE(wr))        * 8.0_dbl
       !
       rgrid_memusage = cost / 1000000.0_dbl
       !
   END FUNCTION rgrid_memusage


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


!**********************************************************
   REAL(dbl) FUNCTION bshells_memusage()
   !**********************************************************
   IMPLICIT NONE
       !
       REAL(dbl) :: cost
       !
       cost = ZERO
       IF ( ALLOCATED(ivr) )       cost = cost + REAL(SIZE(ivr))       * 4.0_dbl
       IF ( ALLOCATED(nnlist) )    cost = cost + REAL(SIZE(nnlist))    * 4.0_dbl
       IF ( ALLOCATED(nncell) )    cost = cost + REAL(SIZE(nncell))    * 4.0_dbl
       IF ( ALLOCATED(nnrev) )     cost = cost + REAL(SIZE(nnrev))     * 4.0_dbl
       IF ( ALLOCATED(vb) )        cost = cost + REAL(SIZE(vb))        * 8.0_dbl
       IF ( ALLOCATED(nnpos) )     cost = cost + REAL(SIZE(nnpos))     * 4.0_dbl
       IF ( ALLOCATED(wb) )        cost = cost + REAL(SIZE(wb))        * 8.0_dbl
       !
       bshells_memusage = cost / 1000000.0_dbl
       !
   END FUNCTION bshells_memusage

END MODULE kpoints_module

