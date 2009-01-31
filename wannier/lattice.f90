!
! Copyright (C) 2004 WanT Group
!
! This file is distributed under the terms of the
! GNU General Public License. See the file `License'
! in the root directory of the present distribution,
! or http://www.gnu.org/copyleft/gpl.txt .
!
!*********************************************************
   MODULE lattice_module
   !*********************************************************
   !
   USE kinds,                    ONLY : dbl
   USE constants,                ONLY : ZERO, TPI, BOHR => bohr_radius_angs
   USE parameters,               ONLY : nstrx
   USE log_module,               ONLY : log_push, log_pop
   USE parser_module,            ONLY : change_case
   USE io_global_module,         ONLY : ionode, ionode_id
   USE mp,                       ONLY : mp_bcast
   USE qexml_module
   USE qexpt_module
   USE crystal_io_module
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
!
! <INFO>
! This module contains all the variables and the subroutines 
! used to manage the lattice data.
!
! Subroutines in this module:
! SUBROUTINE lattice_init()
! SUBROUTINE lattice_read_ext(unit, name, found)
!
! </INFO>
!

  REAL(dbl)        :: alat  = ZERO        ! dir space units (Bohr)
  REAL(dbl)        :: tpiba = ZERO        ! 2pi/alat (Bohr^-1)
  REAL(dbl)        :: omega = ZERO        ! volume of the unit cell (Bohr^3)
  REAL(dbl)        :: avec(3,3)           ! dir lattice vects (Bohr)
  REAL(dbl)        :: bvec(3,3)           ! rec lattice vects (Bohr^-1)

  LOGICAL          :: alloc = .FALSE.

!
! ... end of declarations
!

  PUBLIC :: alat
  PUBLIC :: tpiba
  PUBLIC :: omega
  PUBLIC :: avec, bvec
  PUBLIC :: alloc
  PUBLIC :: lattice_read_ext, lattice_init

CONTAINS

!
! Subroutines
!

!*********************************************************
   SUBROUTINE lattice_init()
   !*********************************************************
    USE io_module, ONLY: stdout
    IMPLICIT NONE

       CALL log_push ( "lattice_init" )
       !
       ! avec and bvec are in units of bohr and bohr^-1 respectively
       ! omega in bohr^3, alat and tpiba in bohr and bohr^-1
       !
       CALL recips( avec(:,1), avec(:,2), avec(:,3), bvec(:,1), bvec(:,2), bvec(:,3), omega)
       bvec = bvec * TPI
       !
       alloc = .TRUE.
       !
       CALL log_pop ( "lattice_init" )
       !
   END SUBROUTINE lattice_init


!*********************************************************
   SUBROUTINE lattice_read_ext(filefmt)
   !*********************************************************
   IMPLICIT NONE
       CHARACTER(*)       :: filefmt
       CHARACTER(16)      :: subname="lattice_read_ext"
       CHARACTER(256)     :: a_units
       INTEGER            :: ierr
       !
#ifdef __ETSF_IO
       TYPE(etsf_geometry)                   :: geometry
       DOUBLE PRECISION, ALLOCATABLE, TARGET :: primitive_vectors(:,:)
#endif

       CALL log_push ( subname )
       ierr = 0
       !
       SELECT CASE ( TRIM(filefmt) )
       !
       CASE ( 'qexml' )
           !
           IF ( ionode ) &
           CALL qexml_read_cell( ALAT=alat, A1=avec(:,1), A2=avec(:,2),  &
                                            A3=avec(:,3), IERR=ierr)
           !
           CALL mp_bcast( avec,  ionode_id )
           CALL mp_bcast( alat,  ionode_id )
           CALL mp_bcast( ierr,  ionode_id )
           !
           IF (ierr/=0) CALL errore(subname,'QEXML: reading lattice',ABS(ierr))
           !
       CASE ( 'pw_export' )
           !
           IF ( ionode ) &
           CALL qexpt_read_cell( ALAT=alat, A1=avec(:,1), A2=avec(:,2),  &
                                            A3=avec(:,3), IERR=ierr)
           !
           CALL mp_bcast( avec,  ionode_id )
           CALL mp_bcast( alat,  ionode_id )
           CALL mp_bcast( ierr,  ionode_id )
           !
           IF (ierr/=0) CALL errore(subname,'QEXPT: reading lattice',ABS(ierr))
           !
       CASE ( 'etsf_io' )
           !
#ifdef __ETSF_IO
           !
           ALLOCATE( primitive_vectors(dims%number_of_cartesian_directions, &
                                       dims%number_of_vectors ) ) 
           !
           geometry%primitive_vectors                 => primitive_vectors
           !
           IF ( ionode ) CALL etsf_io_geometry_get(ncid, geometry, lstat, error_data)
           !
           geometry%primitive_vectors                 => null()
           !
           CALL mp_bcast( primitive_vectors,  ionode_id )
           CALL mp_bcast( lstat,  ionode_id )
           !
           IF ( .NOT. lstat ) CALL errore(subname,'ETSF_IO: reading lattice',10)
           ! 
           ! define internal quantities
           !
           avec(:,1) = primitive_vectors(:,1)
           avec(:,2) = primitive_vectors(:,2)
           avec(:,3) = primitive_vectors(:,3)
           !
           a_units = "bohr"
           !
           alat      = DOT_PRODUCT( avec(:,1), avec(:,1) )
           alat      = SQRT( alat )
           !
           DEALLOCATE( primitive_vectors )
           !
#else
           CALL errore(subname,'ETSF_IO fmt not configured',71)
#endif
           !
       CASE ( 'crystal' )
           !
           IF ( ionode ) CALL crio_open_section( "GEOMETRY", ACTION='read', IERR=ierr )
           CALL mp_bcast( ierr,  ionode_id )
           IF ( ierr/=0 ) CALL errore(subname, 'CRIO: opening sec. GEOMETRY', ABS(ierr) )
           !
           IF ( ionode ) CALL crio_read_periodicity( AVEC=avec, A_UNITS=a_units, IERR=ierr)
           CALL mp_bcast( a_units,  ionode_id )
           CALL mp_bcast( avec,     ionode_id )
           CALL mp_bcast( ierr,     ionode_id )
           IF ( ierr/=0 ) CALL errore(subname, 'CRIO: reading lattice', ABS(ierr) )
           !
           IF ( ionode ) CALL crio_close_section( "GEOMETRY", ACTION='read', IERR=ierr )
           CALL mp_bcast( ierr,  ionode_id )
           IF ( ierr/=0 ) CALL errore(subname, 'CRIO: closing sec. GEOMETRY', ABS(ierr) )
           !
           CALL change_case( a_units, 'lower' )
           !
           SELECT CASE( TRIM(a_units) )
           CASE ( "b", "bohr", "au" )
              !
              ! do nothing
           CASE ( "ang", "angstrom" )
              !
              avec = avec / BOHR
              !
           CASE DEFAULT
              CALL errore(subname, 'unknown units for A: '//TRIM(a_units), 71)
           END SELECT

           !
           ! compute alat [bohr]
           !
           alat = DOT_PRODUCT( avec(:,1), avec(:,1) )
           alat = SQRT( alat )
           !
       CASE DEFAULT
           !
           CALL errore(subname,'invalid fmt ='//TRIM(filefmt),1)
       END SELECT
       !
       !
       ! impose the normalization
       !
       tpiba = TPI / alat
       !
       CALL log_pop ( subname )
       !
   END SUBROUTINE lattice_read_ext

END MODULE lattice_module
