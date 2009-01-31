!
! Copyright (C) 2004 WanT Group
! 
! This file is distributed under the terms of the
! GNU General Public License. See the file `License'
! in the root directory of the present distribution,
! or http://www.gnu.org/copyleft/gpl.txt .
!
! Part of the implementation is based on the ions_base.f90 file 
! from the PWscf package.
!
! <INFO>
!*********************************************************
   MODULE ions_module
   !*********************************************************
   !
   USE kinds,             ONLY : dbl
   USE constants,         ONLY : ZERO, BOHR => bohr_radius_angs
   USE parameters,        ONLY : ntypx, natx, nstrx
   USE io_module,         ONLY : pseudo_dir, prefix, work_dir
   USE log_module,        ONLY : log_push, log_pop
   USE parser_module,     ONLY : change_case
   USE io_global_module,  ONLY : ionode, ionode_id
   USE converters_module, ONLY : cry2cart
   USE mp,                ONLY : mp_bcast
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
! This module contains all the data related to atomic positions
! and atomic species.
!
! Subroutines in this module:
! SUBROUTINE ions_allocate( nat_, nsp_ )
! SUBROUTINE ions_deallocate()
! SUBROUTINE ions_init()
! SUBROUTINE ions_read_ext( filefmt )
! SUBROUTINE ions_tau_sort( tausrt, isrt, tau_, isp, na_ )
!
! </INFO>
!

      !     nsp       = number of species
      !     na(is)    = number of atoms of species is
      !     nax       = max number of atoms of a given species
      !     nat       = total number of atoms of all species

      INTEGER              :: nsp     = 0
      INTEGER, ALLOCATABLE :: na(:) 
      INTEGER              :: nax     = 0
      INTEGER              :: nat     = 0

      !     ityp( i ) = the type of i-th atom 
      !     atm_symb( j )  = name of the type of the j-th atomic specie
      !
      INTEGER,   ALLOCATABLE :: ityp(:)
      LOGICAL                :: uspp_calculation = .FALSE. 
      REAL(dbl)              :: ion_charge = ZERO      !  total ionic charge
      REAL(dbl), ALLOCATABLE :: zv(:)                  !  pseudo atomic charge
      REAL(dbl), ALLOCATABLE :: tau(:,:)               !  atomic positions (alat)
      REAL(dbl), ALLOCATABLE :: tau_srt(:,:)           !  tau sorted by specie (alat)
      INTEGER,   ALLOCATABLE :: ind_srt( : )           !  index of tau sorted by sp.

      CHARACTER(3),     ALLOCATABLE :: atm_symb(:)     !  DIM: nsp
      CHARACTER(3),     ALLOCATABLE :: symb(:)         !  DIM: nat
      CHARACTER(nstrx), ALLOCATABLE :: psfile(:)       !  DIM: nsp

      LOGICAL :: alloc = .FALSE.

!
! end of declaration scope
!

   PUBLIC :: nsp, na, nax, nat
   PUBLIC :: tau
   PUBLIC :: tau_srt, ind_srt
   PUBLIC :: zv, ion_charge
   PUBLIC :: ityp, atm_symb, symb
   PUBLIC :: uspp_calculation
   PUBLIC :: psfile
   PUBLIC :: alloc

   PUBLIC :: ions_allocate, ions_deallocate, ions_memusage
   PUBLIC :: ions_read_ext, ions_init


CONTAINS

!**********************************************************
   SUBROUTINE ions_allocate( nat_, nsp_)
   !**********************************************************
   IMPLICIT NONE
      INTEGER :: nat_, nsp_
      CHARACTER(13)  :: subname='ions_allocate'
      INTEGER :: ierr

      CALL log_push( subname )
      !
      IF ( nat_ <= 0)      CALL errore(subname,'Invalid nat_',ABS(nat_)+1)
      IF ( nsp_ <= 0)      CALL errore(subname,'Invalid nsp_',ABS(nsp_)+1)
      IF ( nat_ > natx )   CALL errore(subname,'Nat too large',nat_)
      IF ( nsp_ > ntypx )  CALL errore(subname,'Nsp too large',nsp_)
      nat = nat_
      nsp = nsp_

      ALLOCATE(na(nsp), STAT=ierr) 
      IF(ierr/=0) CALL errore(subname,'allocating na',nsp)
      !
      ALLOCATE(zv(nsp), STAT=ierr) 
      IF(ierr/=0) CALL errore(subname,'allocating zv',nsp)
      !
      ALLOCATE(ityp(nat), STAT=ierr) 
      IF(ierr/=0) CALL errore(subname,'allocating ityp',nat)
      !
      ALLOCATE(symb(nat), STAT=ierr) 
      IF(ierr/=0) CALL errore(subname,'allocating symb',nat)
      !
      ALLOCATE(tau(3,nat), STAT=ierr) 
      IF(ierr/=0) CALL errore(subname,'allocating tau',nat*nsp)
      !
      ALLOCATE(tau_srt(3,nat), STAT=ierr) 
      IF(ierr/=0) CALL errore(subname,'allocating tau_srt',nat*nsp)
      !
      ALLOCATE(ind_srt(nat), STAT=ierr) 
      IF(ierr/=0) CALL errore(subname,'allocating ind_srt',nat)
      !
      ALLOCATE(atm_symb(nsp), STAT=ierr) 
      IF(ierr/=0) CALL errore(subname,'allocating atm_symb',nsp)
      !
      ALLOCATE(psfile(nsp), STAT=ierr) 
      IF(ierr/=0) CALL errore(subname,'allocating psfile',nsp)

      alloc = .TRUE.
      !
      CALL log_pop ( subname )
      !
  END SUBROUTINE ions_allocate

!**********************************************************
   SUBROUTINE ions_deallocate( )
   !**********************************************************
   IMPLICIT NONE
      CHARACTER(15)  :: subname='ions_deallocate'
      INTEGER :: ierr

      CALL log_push( subname )
      !
      IF ( ALLOCATED( na ) ) THEN
          DEALLOCATE( na, STAT=ierr )
          IF (ierr/=0) CALL errore(subname,'deallocating na',ABS(ierr))
      ENDIF
      IF ( ALLOCATED( zv ) ) THEN
          DEALLOCATE( zv, STAT=ierr )
          IF (ierr/=0) CALL errore(subname,'deallocating zv',ABS(ierr))
      ENDIF
      IF ( ALLOCATED( ityp ) ) THEN
          DEALLOCATE( ityp, STAT=ierr )
          IF (ierr/=0) CALL errore(subname,'deallocating ityp',ABS(ierr))
      ENDIF
      IF ( ALLOCATED( symb ) ) THEN
          DEALLOCATE( symb, STAT=ierr )
          IF (ierr/=0) CALL errore(subname,'deallocating symb',ABS(ierr))
      ENDIF
      IF ( ALLOCATED( tau ) ) THEN
          DEALLOCATE( tau, STAT=ierr )
          IF (ierr/=0) CALL errore(subname,'deallocating tau',ABS(ierr))
      ENDIF
      IF ( ALLOCATED( tau_srt ) ) THEN
          DEALLOCATE( tau_srt, STAT=ierr )
          IF (ierr/=0) CALL errore(subname,'deallocating tau_srt',ABS(ierr))
      ENDIF
      IF ( ALLOCATED( ind_srt ) ) THEN
          DEALLOCATE( ind_srt, STAT=ierr )
          IF (ierr/=0) CALL errore(subname,'deallocating ind_srt',ABS(ierr))
      ENDIF
      IF ( ALLOCATED( atm_symb ) ) THEN
          DEALLOCATE( atm_symb, STAT=ierr )
          IF (ierr/=0) CALL errore(subname,'deallocating atm_symb',ABS(ierr))
      ENDIF
      IF ( ALLOCATED( psfile ) ) THEN
          DEALLOCATE( psfile, STAT=ierr )
          IF (ierr/=0) CALL errore(subname,'deallocating psfile',ABS(ierr))
      ENDIF
      !
      alloc = .FALSE.
      !
      CALL log_pop ( subname )
      !
  END SUBROUTINE ions_deallocate


!**********************************************************
   REAL(dbl) FUNCTION ions_memusage()
   !**********************************************************
   IMPLICIT NONE
       !
       REAL(dbl) :: cost
       !
       cost = ZERO
       IF ( ALLOCATED(na) )       cost = cost + REAL(SIZE(na))         * 4.0_dbl
       IF ( ALLOCATED(zv) )       cost = cost + REAL(SIZE(zv))         * 8.0_dbl
       IF ( ALLOCATED(ityp) )     cost = cost + REAL(SIZE(ityp))       * 4.0_dbl
       IF ( ALLOCATED(symb) )     cost = cost + REAL(SIZE(symb))       * 4.0_dbl
       IF ( ALLOCATED(tau) )      cost = cost + REAL(SIZE(tau))        * 8.0_dbl
       IF ( ALLOCATED(tau_srt) )  cost = cost + REAL(SIZE(tau_srt))    * 8.0_dbl
       IF ( ALLOCATED(ind_srt) )  cost = cost + REAL(SIZE(ind_srt))    * 4.0_dbl
       IF ( ALLOCATED(atm_symb) ) cost = cost + REAL(SIZE(atm_symb))   * 8.0_dbl
       IF ( ALLOCATED(psfile) )   cost = cost + REAL(SIZE(psfile))     * nstrx * 4.0_dbl
       !
       ions_memusage = cost / 1000000.0_dbl
       !
   END FUNCTION ions_memusage


!*********************************************************
   SUBROUTINE ions_read_ext( filefmt )
   !*********************************************************
   IMPLICIT NONE
       CHARACTER(*),      INTENT(in) :: filefmt
       !
       CHARACTER(13)             :: subname="ions_read_ext"
       CHARACTER(256)            :: units
       INTEGER,      ALLOCATABLE :: ityp_tmp(:)
       CHARACTER(3), ALLOCATABLE :: symb_tmp(:), atm_symb_tmp(:)
       INTEGER                   :: i, ia, ierr
       LOGICAL                   :: found
       !   
#ifdef __ETSF_IO
       TYPE(etsf_geometry)                   :: geometry
       DOUBLE PRECISION, ALLOCATABLE, TARGET :: primitive_vectors(:,:)
       INTEGER,          ALLOCATABLE, TARGET :: atom_species(:)
       DOUBLE PRECISION, ALLOCATABLE, TARGET :: reduced_atom_positions(:,:)
       CHARACTER(LEN=etsf_chemlen), ALLOCATABLE, TARGET :: chemical_symbols(:)
#endif

       CALL log_push( subname )
       !
       SELECT CASE ( TRIM(filefmt) )
       !
       CASE ( 'qexml' )
            !
            IF (ionode) CALL qexml_read_ions( NAT=nat, NSP=nsp, IERR=ierr )
            CALL mp_bcast( nat,   ionode_id )
            CALL mp_bcast( nsp,   ionode_id )
            CALL mp_bcast( ierr,  ionode_id )
            !
            IF ( ierr/=0) CALL errore(subname,'QEXML: getting ion dimensions',ABS(ierr))
            !
       CASE ( 'pw_export' )
            !
            IF (ionode) CALL qexpt_read_ions( NAT=nat, NSP=nsp, IERR=ierr )
            CALL mp_bcast( nat,   ionode_id )
            CALL mp_bcast( nsp,   ionode_id )
            CALL mp_bcast( ierr,  ionode_id )
            !
            IF ( ierr/=0) CALL errore(subname,'QEXPT: getting ion dimensions',ABS(ierr))
            !
       CASE ( 'etsf_io' )
            !
#ifdef __ETSF_IO
            !
            nat  = dims%number_of_atoms
            nsp  = dims%number_of_atom_species
            ierr = 0
            !
#else
            CALL errore(subname,'ETSF_IO fmt not configured',71)
#endif
            !
       CASE ( 'crystal' )
            !
            IF (ionode) CALL crio_open_section( "GEOMETRY", ACTION='read', IERR=ierr )
            CALL mp_bcast( ierr,  ionode_id )
            IF ( ierr/=0 ) CALL errore(subname, 'CRIO: opening sec. GEOMETRY', ABS(ierr) )
            !
            IF (ionode) CALL crio_read_atoms( NUM_OF_ATOMS=nat, IERR=ierr )
            CALL mp_bcast( nat,   ionode_id )
            CALL mp_bcast( ierr,  ionode_id )
            IF ( ierr/=0) CALL errore(subname,'CRIO: getting ion dimensions',ABS(ierr))

            !
            ! verbose workaround to determine the number 
            ! of atomic species, currently missing in CRYSTAL xml
            !
            ALLOCATE( symb_tmp(nat), atm_symb_tmp(ntypx), STAT=ierr )
            IF ( ierr/=0) CALL errore(subname,'allocating symb_tmp',ABS(ierr))
            !
            IF (ionode) CALL crio_read_atoms( SYMB=symb_tmp, IERR=ierr )
            CALL mp_bcast( symb_tmp,   ionode_id )
            CALL mp_bcast( ierr,       ionode_id )
            IF ( ierr/=0) CALL errore(subname,'CRIO: getting SYMB',ABS(ierr))
            !
            nsp = 1
            atm_symb_tmp(1) = TRIM(symb_tmp(1))
            !
            DO ia = 2, nat
                !
                found = .FALSE.
                inner_loop:&
                DO  i = 1, nsp 
                    !
                    IF ( TRIM(atm_symb_tmp(i)) == TRIM(symb_tmp(ia)) ) THEN
                       found = .TRUE.
                       EXIT inner_loop                        
                    ENDIF
                    !
                ENDDO inner_loop
                !
                IF ( .NOT. found ) THEN
                    !
                    nsp = nsp + 1
                    atm_symb_tmp( nsp ) = TRIM(symb_tmp(ia))
                    !
                ENDIF
                !
            ENDDO
            !    
       CASE DEFAULT
            !
            CALL errore(subname,'invalid filefmt = '//TRIM(filefmt), 1)
            !
       END SELECT
       !
       IF ( ierr/=0) CALL errore(subname,'getting ion dimensions',ABS(ierr))
       !
       !
       CALL ions_allocate( nat, nsp ) 
       !
       ! some simple initializations
       zv(:) = ZERO

       !
       ! read massive data
       ! NOTE: atomic positions are read in bohr
       !
       !
       SELECT CASE ( TRIM(filefmt) )
       !
       CASE ( 'qexml' )
            !
            ALLOCATE( ityp_tmp(nat), STAT=ierr )
            IF ( ierr/=0) CALL errore(subname,'allocating ityp_tmp', ABS(ierr))
            !
            IF ( ionode ) &
            CALL qexml_read_ions( ATM=atm_symb, ITYP=ityp_tmp, &
                                  PSFILE=psfile, TAU=tau,  IERR=ierr )
            !
            CALL mp_bcast( atm_symb,   ionode_id )
            CALL mp_bcast( ityp_tmp,   ionode_id )
            CALL mp_bcast( psfile,     ionode_id )
            CALL mp_bcast( tau,        ionode_id )
            CALL mp_bcast( ierr,       ionode_id )
            !
            IF (ierr/=0) CALL errore(subname,'QEXML: reading data' , ABS(ierr))
            !
            ! pseudo pot are in the .save directory
            !
            pseudo_dir = TRIM(work_dir)// "/" // TRIM(prefix) // ".save/" 
            !
            ! conversions
            DO i = 1, nat
                 symb( i ) = atm_symb( ityp_tmp (i) )
            ENDDO
            !
            DEALLOCATE( ityp_tmp, STAT=ierr )
            IF ( ierr/=0) CALL errore(subname,'deallocating ityp_tmp', ABS(ierr))
            !
       CASE ( 'pw_export' )
            !
            IF ( ionode ) &
            CALL qexpt_read_ions( ATM=atm_symb, SYMB=symb, PSEUDO_DIR=pseudo_dir, &
                                  PSFILE=psfile, TAU=tau,  IERR=ierr )
            !
            CALL mp_bcast( atm_symb,   ionode_id )
            CALL mp_bcast( symb,       ionode_id )
            CALL mp_bcast( pseudo_dir, ionode_id )
            CALL mp_bcast( psfile,     ionode_id )
            CALL mp_bcast( tau,        ionode_id )
            CALL mp_bcast( ierr,       ionode_id )
            !
            IF (ierr/=0) CALL errore(subname,'QEXPT: reading data' , ABS(ierr))
            !
       CASE ( 'etsf_io' )
            !
#ifdef __ETSF_IO
            !
            ALLOCATE( primitive_vectors(dims%number_of_cartesian_directions, dims%number_of_vectors) )
            ALLOCATE( chemical_symbols(dims%number_of_atom_species) )
            ALLOCATE( atom_species(dims%number_of_atoms) )
            ALLOCATE( reduced_atom_positions(dims%number_of_reduced_dimensions, &
                                             dims%number_of_atoms ) )
            !
            geometry%primitive_vectors         => primitive_vectors
            geometry%chemical_symbols          => chemical_symbols
            geometry%atom_species              => atom_species
            geometry%reduced_atom_positions    => reduced_atom_positions
            !
            IF ( ionode ) CALL etsf_io_geometry_get(ncid, geometry, lstat, error_data) 
            !
            geometry%primitive_vectors         => null()
            geometry%chemical_symbols          => null()
            geometry%atom_species              => null()
            geometry%reduced_atom_positions    => null()
            !
            CALL mp_bcast( primitive_vectors,       ionode_id )
            CALL mp_bcast( chemical_symbols,        ionode_id )
            CALL mp_bcast( atom_species,            ionode_id )
            CALL mp_bcast( reduced_atom_positions,  ionode_id )
            CALL mp_bcast( lstat,                   ionode_id )
            !
            IF (.NOT. lstat) CALL errore(subname,'ETSF_IO: reading data' , 11 )
            !
            pseudo_dir  = " "
            psfile(:)   = " "
            !
            atm_symb( 1: nsp )  = chemical_symbols( 1: dims%number_of_atom_species )
            !
            DO i = 1, nat
                symb( i ) = chemical_symbols( atom_species( i ) )
            ENDDO
            !
            !
            tau( :, :) = reduced_atom_positions( :, :)
            CALL cry2cart( tau, primitive_vectors)
            !
            DEALLOCATE( primitive_vectors )
            DEALLOCATE( chemical_symbols )
            DEALLOCATE( atom_species )
            DEALLOCATE( reduced_atom_positions )
            !
#endif
            !
       CASE ( 'crystal' )
            !
            IF ( ionode ) &
            CALL crio_read_atoms( SYMB=symb, COORDS=tau, UNITS=units, IERR=ierr )
            !
            CALL mp_bcast( symb,       ionode_id )
            CALL mp_bcast( tau,        ionode_id )
            CALL mp_bcast( units,      ionode_id )
            CALL mp_bcast( ierr,       ionode_id )
            !
            IF ( ierr/=0) CALL errore(subname,'CRIO: getting ion dimensions',ABS(ierr))
            !
            ! set tau in bohr
            CALL change_case( units, 'lower' )
            !
            SELECT CASE( TRIM(units) )
            CASE ( "b", "bohr", "au" )
               !
               ! do nothing
            CASE ( "ang", "angstrom" )
               !
               tau = tau / BOHR
               !
            CASE DEFAULT
               CALL errore(subname, 'unknown units for A: '//TRIM(units), 71)
            END SELECT
            !
            ! set unsused vars
            pseudo_dir  = " "
            psfile(:)   = " "
            !
            DO i = 1, nsp
               atm_symb(i) = TRIM( atm_symb_tmp(i) )
            ENDDO
            !
            IF (ionode) CALL crio_close_section( "GEOMETRY", ACTION='read', IERR=ierr )
            CALL mp_bcast( ierr,       ionode_id )
            IF ( ierr/=0 ) CALL errore(subname, 'CRIO: closing sec. GEOMETRY', ABS(ierr) )
            !
            ! local cleanup
            DEALLOCATE( symb_tmp, atm_symb_tmp, STAT=ierr)
            IF ( ierr/=0) CALL errore(subname,'deallocating symb_tmp, atm_symb_tmp',ABS(ierr))
            !
       CASE DEFAULT
            !
            CALL errore(subname,'invalid filefmt = '//TRIM(filefmt), 1)
       END SELECT
       !
       CALL log_pop( subname )
       !
   END SUBROUTINE ions_read_ext


!*********************************************************
   SUBROUTINE ions_init( )
   !*********************************************************
   !
   ! get the internal ordering of atoms and species
   ! ATM(1:ntyp) contains the symbols of atoms in the same order
   ! of pseudopot files. This will be assumed as internal ordering 
   !
   IMPLICIT NONE
       CHARACTER(9)       :: subname="ions_init"
       INTEGER            :: ia, i
 
       CALL log_push( subname )
       !
       IF ( .NOT. alloc ) CALL errore(subname,'IONS not allocated',1)

       !
       ! setting species and atomic symbols
       !
       na(:) = 0
       DO i=1,nsp
          na(i) = 0
          DO ia=1,nat
             IF ( symb(ia) == atm_symb(i) ) THEN
                  ityp(ia) = i
                  na(i) = na(i) + 1
             ENDIF
          ENDDO
       ENDDO
       IF ( SUM( na(1:nsp) ) /= nat ) CALL errore(subname,'some species are missing',1)
       nax = MAXVAL( na(:) )
     
       !
       ! sorting atoms by species
       !
       CALL ions_sort_tau(tau_srt, ind_srt, tau, ityp, na )
       !
       CALL log_pop( subname )
       !
   END SUBROUTINE ions_init


!*********************************************************
   SUBROUTINE ions_sort_tau( tausrt, isrt, tau_, isp, na_ )
   !*********************************************************
   !
   ! Freely inspired to the similar subroutine in ESPRESSO package
   !
      IMPLICIT NONE
      REAL(dbl),   INTENT(OUT) :: tausrt( :, : )
      INTEGER,     INTENT(OUT) :: isrt( : )
      REAL(dbl),    INTENT(IN) :: tau_( :, : )
      INTEGER,      INTENT(IN) :: isp( : ), na_(:)
      INTEGER :: ina( SIZE(na_) ), na_tmp( SIZE(na_) )
      INTEGER :: nsp_, is, ia

      CALL log_push( 'ions_sort_tau' )
      !
      nsp_ = SIZE(na_)
      IF ( nsp_ /= nsp) CALL errore('ions_sort_tau','Invalid nsp',ABS(nsp-nsp_))

      ! ... compute the index of the first atom in each specie
      ina( 1 ) = 0
      DO is = 2, nsp_
        !
        ina( is ) = ina( is - 1 ) + na_( is - 1 )
        !
      ENDDO

      ! ... sort the position according to atomic specie
      na_tmp  = 0
      DO ia = 1, nat
        !
        is  =  isp( ia )
        na_tmp( is ) = na_tmp( is ) + 1
        tausrt( :, na_tmp(is) + ina(is) ) = tau_(:, ia )
        isrt  (    na_tmp(is) + ina(is) ) = ia
        !
      ENDDO
      !
      CALL log_pop( 'ions_sort_tau' )
      !
    END SUBROUTINE ions_sort_tau


END MODULE ions_module

