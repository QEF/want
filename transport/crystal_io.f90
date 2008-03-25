!
! Copyright (C) 2008 WanT Group
! This file is distributed under the terms of the
! GNU General Public License. See the file `License'
! in the root directory of the present distribution,
! or http://www.gnu.org/copyleft/gpl.txt .
!
!----------------------------------------------------------------------------
MODULE crystal_io_module
  !----------------------------------------------------------------------------
  !
  USE iotk_module
  USE crystal_io_base_module
  !
  IMPLICIT NONE
  !
  PRIVATE
  SAVE
  !
  ! definitions for the fmt
  !
  CHARACTER(10), PARAMETER :: crio_fmt_name = "CRYSTAL_IO"
  CHARACTER(5),  PARAMETER :: crio_fmt_version = "0.1.0"
  
  !
  ! internal data to be set
  !
  INTEGER          :: iunit, ounit
  !
  CHARACTER(iotk_attlenx) :: attr
  !
  !
  ! end of declarations
  !

  PUBLIC :: crio_fmt_name, crio_fmt_version
  PUBLIC :: crio_atm_orbital, crio_atm_orbital_allocate,           &
            crio_atm_orbital_deallocate
  !
  PUBLIC :: crio_open_file,    crio_close_file
  PUBLIC :: crio_open_section, crio_close_section
  !
  PUBLIC :: crio_write_header,                                     &
            crio_write_periodicity,     crio_write_cell,           &
            crio_write_cell_reciprocal, crio_write_symmetry,       &
            crio_write_atoms,                                      &
            crio_write_atomic_orbitals,                            &
            crio_write_direct_lattice , crio_write_bz 
  !
  PUBLIC :: crio_read_header,                                      & 
            crio_read_periodicity,      crio_read_cell,            &
            crio_read_cell_reciprocal,  crio_read_symmetry,        &
            crio_read_atoms,                                       &
            crio_read_atomic_orbitals,                             &
            crio_read_direct_lattice,   crio_read_bz,              &
            crio_read_hamiltonian,      crio_read_overlap 

CONTAINS

!
!-------------------------------------------
! ... basic (public) subroutines
!-------------------------------------------
!
    !
    !------------------------------------------------------------------------
    SUBROUTINE crio_open_file( unit, filename, action, binary, ierr)
      !------------------------------------------------------------------------
      !
      ! open data file
      !
      IMPLICIT NONE
      !
      INTEGER,            INTENT(IN)  :: unit
      CHARACTER(*),       INTENT(IN)  :: filename
      CHARACTER(*),       INTENT(IN)  :: action      ! ("read"|"write")
      LOGICAL, OPTIONAL,  INTENT(IN)  :: binary
      INTEGER,            INTENT(OUT) :: ierr
      !
      LOGICAL :: binary_

      ierr = 0
      binary_ = .FALSE.
      IF ( PRESENT(binary) ) binary_ = binary 
      !
      SELECT CASE ( TRIM(action) )
      CASE ( "read", "READ" )
          !
          iunit = unit
          CALL iotk_open_read ( iunit, FILE = TRIM(filename), IERR=ierr )
          !
      CASE ( "write", "WRITE" )
          !
          ounit = unit
          CALL iotk_open_write( ounit, FILE = TRIM(filename), BINARY=binary_, IERR=ierr )
          !
      CASE DEFAULT
          ierr = 1
      END SELECT
          
    END SUBROUTINE crio_open_file
    !  
    !  
    !------------------------------------------------------------------------
    SUBROUTINE crio_close_file( action, ierr)
      !------------------------------------------------------------------------
      !
      ! close data file
      !
      IMPLICIT NONE
      !
      CHARACTER(*),  INTENT(IN)  :: action      ! ("read"|"write")
      INTEGER,       INTENT(OUT) :: ierr
      !
      ierr = 0
      !
      SELECT CASE ( TRIM(action) )
      CASE ( "read", "READ" )
          !
          CALL iotk_close_read( iunit, IERR=ierr )
          !
      CASE ( "write", "WRITE" )
          !
          CALL iotk_close_write( ounit, IERR=ierr )
          !
      CASE DEFAULT
          ierr = 2
      END SELECT
      !
    END SUBROUTINE crio_close_file
    !
    !
    !------------------------------------------------------------------------
    SUBROUTINE crio_open_section( tag, action, ierr)
      !------------------------------------------------------------------------
      !
      ! open data file
      !
      IMPLICIT NONE
      !
      CHARACTER(*),       INTENT(IN)  :: tag
      CHARACTER(*),       INTENT(IN)  :: action      ! ("read"|"write")
      INTEGER,            INTENT(OUT) :: ierr
      !
      !
      ierr = 0
      !
      SELECT CASE( TRIM(action) )
      CASE ( 'read', 'READ' )
         !
         CALL iotk_scan_begin( iunit, tag, IERR=ierr)
         !
      CASE ( 'write', 'WRITE' )
         !
         CALL iotk_write_begin( ounit, tag, IERR=ierr)
         !
      CASE DEFAULT 
         !
         ierr = 2
         !
      END SELECT
      !
    END SUBROUTINE crio_open_section
    !
    !
    !------------------------------------------------------------------------
    SUBROUTINE crio_close_section( tag, action, ierr)
      !------------------------------------------------------------------------
      !
      ! open data file
      !
      IMPLICIT NONE
      !
      CHARACTER(*),       INTENT(IN)  :: tag
      CHARACTER(*),       INTENT(IN)  :: action      ! ("read"|"write")
      INTEGER,            INTENT(OUT) :: ierr
      !
      !
      ierr = 0
      !
      SELECT CASE( TRIM(action) )
      CASE ( 'read', 'READ' )
         !
         CALL iotk_scan_end( iunit, tag, IERR=ierr)
         !
      CASE ( 'write', 'WRITE' )
         !
         CALL iotk_write_end( ounit, tag, IERR=ierr)
         !
      CASE DEFAULT 
         !
         ierr = 2
         !
      END SELECT
      !
    END SUBROUTINE crio_close_section

!
!-------------------------------------------
! ... basic (private) subroutines
!-------------------------------------------
!
    !------------------------------------------------------------------------
    FUNCTION int_to_char( i )
      !------------------------------------------------------------------------
      !
      IMPLICIT NONE
      !
      INTEGER, INTENT(IN) :: i
      CHARACTER (LEN=6)   :: int_to_char
      !
      !
      IF ( i < 10 ) THEN
         !
         WRITE( UNIT = int_to_char , FMT = "(I1)" ) i
         !
      ELSE IF ( i < 100 ) THEN
         !
         WRITE( UNIT = int_to_char , FMT = "(I2)" ) i
         !
       ELSE IF ( i < 1000 ) THEN
         !
         WRITE( UNIT = int_to_char , FMT = "(I3)" ) i
         !
       ELSE IF ( i < 10000 ) THEN
         !
         WRITE( UNIT = int_to_char , FMT = "(I4)" ) i
         !
       ELSE
         !
       WRITE( UNIT = int_to_char , FMT = "(I5)" ) i
       !
      END IF
      !
    END FUNCTION int_to_char
    !
    !
    !------------------------------------------------------------------------
    SUBROUTINE copy_file( file_in, file_out, ierr )
      !------------------------------------------------------------------------
      !
      CHARACTER(LEN=*),  INTENT(IN) :: file_in, file_out
      INTEGER,           INTENT(OUT):: ierr
      !
      CHARACTER(LEN=256) :: string
      INTEGER            :: iun_in, iun_out, ios
      !
      !
      ierr = 0
      !
      CALL iotk_free_unit( iun_in,  ierr )
      IF ( ierr /= 0) RETURN
      CALL iotk_free_unit( iun_out, ierr )
      IF ( ierr /= 0) RETURN
      !
      OPEN( UNIT = iun_in,  FILE = file_in,  STATUS = "OLD", IOSTAT=ierr )
      IF ( ierr /= 0) RETURN
      OPEN( UNIT = iun_out, FILE = file_out, STATUS = "UNKNOWN", IOSTAT=ierr )         
      IF ( ierr /= 0) RETURN
      !
      copy_loop: DO
         !
         READ( UNIT = iun_in, FMT = '(A256)', IOSTAT = ios ) string
         !
         IF ( ios < 0 ) EXIT copy_loop
         !
         WRITE( UNIT = iun_out, FMT = '(A)' ) TRIM( string )
         !
      END DO copy_loop
      !
      CLOSE( UNIT = iun_in )
      CLOSE( UNIT = iun_out )
      !
      RETURN
      !
    END SUBROUTINE copy_file
    !
    !
    !------------------------------------------------------------------------
    FUNCTION check_file_exst( filename )
      !------------------------------------------------------------------------
      !    
      IMPLICIT NONE 
      !    
      LOGICAL          :: check_file_exst
      CHARACTER(LEN=*) :: filename
      !    
      LOGICAL :: lexists
      !    
      INQUIRE( FILE = TRIM( filename ), EXIST = lexists )
      !    
      check_file_exst = lexists
      RETURN
      !    
    END FUNCTION check_file_exst
    !    
    !
!
!-------------------------------------------
! ... write subroutines
!-------------------------------------------
!
    !
    !
    !------------------------------------------------------------------------
    SUBROUTINE crio_write_header( creator_name, creator_version, title ) 
      !------------------------------------------------------------------------
      !
      IMPLICIT NONE
      CHARACTER(LEN=*), INTENT(IN) :: creator_name, creator_version, title
      !
      CALL iotk_write_begin( ounit, "HEADER" )
      !
      CALL iotk_write_attr(attr, "name",TRIM(crio_fmt_name), FIRST=.TRUE.)
      CALL iotk_write_attr(attr, "version",TRIM(crio_fmt_version) )
      CALL iotk_write_empty( ounit, "FORMAT", ATTR=attr )
      !
      CALL iotk_write_attr(attr, "name",TRIM(creator_name), FIRST=.TRUE.)
      CALL iotk_write_attr(attr, "version",TRIM(creator_version) )
      CALL iotk_write_empty( ounit, "CREATOR", ATTR=attr )
      !
      CALL iotk_write_dat( ounit, "TITLE", TRIM(title) )
      !
      CALL iotk_write_end( ounit, "HEADER" )
      !
    END SUBROUTINE crio_write_header
    !
    !
    !------------------------------------------------------------------------
    SUBROUTINE crio_write_periodicity( num_of_periodic_dir, periodic_dir )
      !------------------------------------------------------------------------
      !
      INTEGER,               INTENT(IN) :: num_of_periodic_dir
      INTEGER,     OPTIONAL, INTENT(IN) :: periodic_dir(:)
      !
      !
      CALL iotk_write_begin( ounit, "PERIODICITY" )
      !
      CALL iotk_write_dat( ounit, "NUMBER_OF_PERIODIC_DIRECTIONS", num_of_periodic_dir )
      !
      IF ( PRESENT ( periodic_dir) ) THEN
          !
          CALL iotk_write_dat( ounit, "PERIODIC_DIRECTIONS", &
                               periodic_dir(1:num_of_periodic_dir), COLUMNS=3 )
          !
      ENDIF
      !
      CALL iotk_write_end( ounit, "PERIODICITY" )
      !
    END SUBROUTINE crio_write_periodicity
    !
    !
    !------------------------------------------------------------------------
    SUBROUTINE crio_write_cell( avec, units, periodicity )
      !------------------------------------------------------------------------
      !
      REAL(dbl),         OPTIONAL, INTENT(IN) :: avec(3,3)
      CHARACTER(LEN=*),  OPTIONAL, INTENT(IN) :: units
      INTEGER,           OPTIONAL, INTENT(IN) :: periodicity
      !
      !
      CALL iotk_write_begin( ounit, "CELL" )
      !
      attr = " "
      IF (PRESENT( units ) )       CALL iotk_write_attr( attr, "unit", TRIM(units) )
      IF (PRESENT( periodicity ) ) CALL iotk_write_attr( attr, "periodicity", periodicity)
      !
      CALL iotk_write_empty( ounit, "CELL_INFO", ATTR=attr )
      !
      IF (PRESENT (avec) ) THEN
          !
          CALL iotk_write_dat( ounit, "CELL_VECTOR_A", avec(:,1), COLUMNS=3)
          CALL iotk_write_dat( ounit, "CELL_VECTOR_B", avec(:,2), COLUMNS=3)
          CALL iotk_write_dat( ounit, "CELL_VECTOR_C", avec(:,3), COLUMNS=3)
          !
      ENDIF
      !
      CALL iotk_write_end( ounit, "CELL" )
      !
    END SUBROUTINE crio_write_cell
    !
    !
    !------------------------------------------------------------------------
    SUBROUTINE crio_write_cell_reciprocal( bvec, units, periodicity )
      !------------------------------------------------------------------------
      !
      REAL(dbl),         OPTIONAL, INTENT(IN) :: bvec(3,3)
      CHARACTER(LEN=*),  OPTIONAL, INTENT(IN) :: units
      INTEGER,           OPTIONAL, INTENT(IN) :: periodicity
      !
      !
      CALL iotk_write_begin( ounit, "CELL_RECIPROCAL" )
      !
      attr = " "
      IF (PRESENT( units ) )       CALL iotk_write_attr( attr, "unit", TRIM( units ))
      IF (PRESENT( periodicity ) ) CALL iotk_write_attr( attr, "periodicity", periodicity)
      !
      CALL iotk_write_empty( ounit, "CELL_RECIPROCAL_INFO", ATTR=attr )
      !
      IF (PRESENT (bvec) ) THEN
          !
          CALL iotk_write_dat( ounit, "CELL_RECIPROCAL_VECTOR_A", bvec(:,1), COLUMNS=3)
          CALL iotk_write_dat( ounit, "CELL_RECIPROCAL_VECTOR_B", bvec(:,2), COLUMNS=3)
          CALL iotk_write_dat( ounit, "CELL_RECIPROCAL_VECTOR_C", bvec(:,3), COLUMNS=3)
          !
      ENDIF
      !
      CALL iotk_write_end( ounit, "CELL_RECIPROCAL" )
      !
    END SUBROUTINE crio_write_cell_reciprocal
    !
    !
    !------------------------------------------------------------------------
    SUBROUTINE crio_write_symmetry( num_of_symmetries )
      !------------------------------------------------------------------------
      !
      INTEGER,  INTENT(IN) :: num_of_symmetries
      !
      !
      CALL iotk_write_begin( ounit, "SYMMETRY" )
      !
      CALL iotk_write_dat( ounit, "NUMBER_OF_SYMMETRY_OPERATORS", num_of_symmetries)
      !
      CALL iotk_write_end( ounit, "SYMMETRY" )
      !
    END SUBROUTINE crio_write_symmetry
    !
    !
    !------------------------------------------------------------------------
    SUBROUTINE crio_write_atoms( num_of_atoms, periodicity, atm_symb, atm_number, &
                                coords, units, coords_cry )
      !------------------------------------------------------------------------
      !
      INTEGER,                     INTENT(IN) :: num_of_atoms
      INTEGER,           OPTIONAL, INTENT(IN) :: periodicity
      REAL(dbl),         OPTIONAL, INTENT(IN) :: coords(:,:), coords_cry(:,:)
      CHARACTER(LEN=*),  OPTIONAL, INTENT(IN) :: atm_symb(:), units
      INTEGER,           OPTIONAL, INTENT(IN) :: atm_number(:)
      !
      INTEGER :: ia
      !
      CALL iotk_write_begin( ounit, "ATOMS" )
      !
      CALL iotk_write_dat( ounit, "NUMBER_OF_ATOMS", num_of_atoms )
      !
      IF ( PRESENT( coords) ) THEN
          !
          CALL iotk_write_attr( attr, "number_of_items", num_of_atoms, FIRST=.TRUE.)
          IF ( PRESENT(units) ) CALL iotk_write_attr( attr, "unit", TRIM(units) )
          !
          CALL iotk_write_begin( ounit, "CARTESIAN_COORDINATES", ATTR=attr )
          ! 
          DO ia = 1, num_of_atoms
              !
              attr=" "
              IF (PRESENT( atm_symb))   CALL iotk_write_attr(attr, "atomic_symbol", TRIM(atm_symb(ia)) )
              IF (PRESENT( atm_number)) CALL iotk_write_attr(attr, "atomic_number", atm_number(ia) )
              !
              CALL iotk_write_dat( ounit, "ATOM"//TRIM(iotk_index(ia)), coords(1:3,ia), &
                                   ATTR=attr, COLUMNS=3 )
              !
          ENDDO
          ! 
          CALL iotk_write_end( ounit, "CARTESIAN_COORDINATES" )
          !
      ENDIF
      !
      IF ( PRESENT( coords_cry ) ) THEN
          !
          CALL iotk_write_attr( attr, "number_of_items", num_of_atoms, FIRST=.TRUE.)
          CALL iotk_write_attr( attr, "unit", "relative" )
          IF ( PRESENT(periodicity) ) CALL iotk_write_attr( attr, "periodicity", periodicity )
          !
          CALL iotk_write_begin( ounit, "FRACTIONARY_COORDINATES", ATTR=attr )
          ! 
          DO ia = 1, num_of_atoms
              !
              attr=" "
              IF (PRESENT( atm_symb))   CALL iotk_write_attr(attr, "atomic_symbol", TRIM(atm_symb(ia)))
              IF (PRESENT( atm_number)) CALL iotk_write_attr(attr, "atomic_number", atm_number(ia) )
              !
              CALL iotk_write_dat( ounit, "ATOM"//TRIM(iotk_index(ia)), coords_cry(1:3,ia), &
                                   ATTR=attr, COLUMNS=3 ) 
              !
          ENDDO
          ! 
          CALL iotk_write_end( ounit, "FRACTIONARY_COORDINATES" )
          !
      ENDIF
      !
      CALL iotk_write_end( ounit, "ATOMS" )
      !
    END SUBROUTINE crio_write_atoms
    !
    !
    !------------------------------------------------------------------------
    SUBROUTINE crio_write_atomic_orbitals( num_of_atomic_orbitals, num_of_atoms, atm_orbital )
      !------------------------------------------------------------------------
      !
      INTEGER,                          INTENT(IN) :: num_of_atomic_orbitals, num_of_atoms
      TYPE(crio_atm_orbital), OPTIONAL, INTENT(IN) :: atm_orbital(:)
      !
      INTEGER        :: i

      !
      CALL iotk_write_begin( ounit, "ATOMIC_ORBITALS" )
      !
      CALL iotk_write_attr( attr, "number_of_atoms", num_of_atoms, FIRST=.TRUE. )
      CALL iotk_write_empty( ounit, "ATOMIC_ORBITALS_INFO", ATTR=attr )
      !
      CALL iotk_write_dat( ounit, "TOTAL_NUMBER_OF_ATOMIC_ORBITALS", num_of_atomic_orbitals )
      !
      IF ( PRESENT ( atm_orbital ) ) THEN
          !
          DO i = 1, num_of_atoms
              !
              CALL iotk_write_attr( attr, "number_of_atomic_orbitals", &
                                    atm_orbital(i)%norb, FIRST=.TRUE. )
              CALL iotk_write_attr( attr, "unit", TRIM(atm_orbital(i)%units) )
              CALL iotk_write_attr( attr, "centre", atm_orbital(i)%coord )
              !
              CALL iotk_write_begin( ounit, "ATOMIC_ORBITALS_OF_ATOM"//TRIM(iotk_index(i)), &
                                     ATTR=attr)
              !
              CALL iotk_write_dat( ounit, "SEQUENCE_NUMBER_LABEL", atm_orbital(i)%seq(:), COLUMNS=10 )
              CALL iotk_write_dat( ounit, "TYPE", atm_orbital(i)%orb_type(:), COLUMNS=10 )
              !
              CALL iotk_write_end( ounit, "ATOMIC_ORBITALS_OF_ATOM"//TRIM(iotk_index(i)) )
              !
          ENDDO
          !
      ENDIF
      !
      CALL iotk_write_end( ounit, "ATOMIC_ORBITALS" )
      !
    END SUBROUTINE crio_write_atomic_orbitals
    !
    !
    !------------------------------------------------------------------------
    SUBROUTINE crio_write_direct_lattice( nrtot, rvec, rvec_cry, r_units )
      !------------------------------------------------------------------------
      !
      INTEGER,                     INTENT(IN) :: nrtot
      REAL(dbl),         OPTIONAL, INTENT(IN) :: rvec(:,:)
      INTEGER,           OPTIONAL, INTENT(IN) :: rvec_cry(:,:)
      CHARACTER(LEN=*),  OPTIONAL, INTENT(IN) :: r_units
      !
      INTEGER :: ir

      !
      CALL iotk_write_begin( ounit, "DIRECT_LATTICE" )
      !
      CALL iotk_write_dat( ounit, "NUMBER_OF_DIRECT_LATTICE_VECTORS", nrtot )
      !
      IF ( PRESENT( rvec ) ) THEN
          !
          CALL iotk_write_begin( ounit, "CARTESIAN_VECTORS" )
          !
          CALL iotk_write_attr( attr, "number_of_items", nrtot, FIRST=.TRUE.)
          IF (PRESENT( r_units ) ) &
               CALL iotk_write_attr( attr, "unit", TRIM(r_units) )
          CALL iotk_write_empty( ounit, "CARTESIAN_VECTORS_INFO", ATTR=attr )
          !
          DO ir = 1, nrtot
              !
              CALL iotk_write_dat( ounit, "CVDL"//TRIM(iotk_index(ir)), rvec(:,ir), COLUMNS=3)
              !
          ENDDO
          !
          CALL iotk_write_end( ounit, "CARTESIAN_VECTORS" )
          !
      ENDIF
      !
      IF ( PRESENT( rvec_cry ) ) THEN
          !
          CALL iotk_write_begin( ounit, "INTEGER_VECTORS" )
          !
          CALL iotk_write_attr( attr, "number_of_items", nrtot, FIRST=.TRUE.)
          CALL iotk_write_attr( attr, "unit", "relative" )
          CALL iotk_write_empty( ounit, "INTEGER_VECTORS_INFO", ATTR=attr )
          !
          DO ir = 1, nrtot
              !
              CALL iotk_write_dat( ounit, "IVDL"//TRIM(iotk_index(ir)), rvec_cry(:,ir), COLUMNS=3)
              !
          ENDDO
          !
          CALL iotk_write_end( ounit, "INTEGER_VECTORS" )
          !
      ENDIF
      !
      CALL iotk_write_end( ounit, "DIRECT_LATTICE" )
      !
    END SUBROUTINE crio_write_direct_lattice
    !
    !
    !------------------------------------------------------------------------
    SUBROUTINE crio_write_bz( num_k_points, nk, xk, k_units, wk, wfc_type )
      !------------------------------------------------------------------------
      !
      INTEGER,                 INTENT(IN) :: num_k_points
      INTEGER,       OPTIONAL, INTENT(IN) :: nk(:)
      REAL(dbl),     OPTIONAL, INTENT(IN) :: xk(:,:), wk(:)
      CHARACTER(*),  OPTIONAL, INTENT(IN) :: wfc_type(:)
      CHARACTER(*),  OPTIONAL, INTENT(IN) :: k_units
      !
      INTEGER :: ik

      !
      CALL iotk_write_begin( ounit, "BRILLOUIN_ZONE" )
      !
      CALL iotk_write_dat( ounit, "NUMBER_OF_IRREDUCIBLE_K_VECTORS", num_k_points )
      !
      CALL iotk_write_attr( attr, "alias", &
                            "IRREDUCIBLE_FRACTIONARY_VECTORS_IN_RECIPROCAL_LATTICE", FIRST=.TRUE.)
      CALL iotk_write_begin( ounit, "IRREDUCIBLE_K_VECTORS", ATTR=attr )
      !
      CALL iotk_write_attr( attr, "number_of_items", num_k_points, FIRST=.TRUE.) 
      IF ( PRESENT( nk ) ) CALL iotk_write_attr( attr, "monkhorst_pack_grid", nk )
      IF ( PRESENT( k_units ) ) CALL iotk_write_attr( attr, "unit", TRIM(k_units) )
      !
      CALL iotk_write_empty( ounit, "IRREDUCIBLE_K_VECTORS_INFO", ATTR=attr)
      !
      IF ( PRESENT( xk ) ) THEN
          !
          DO ik = 1, num_k_points
              !
              attr = " "
              IF ( PRESENT( wk )) CALL iotk_write_attr( attr, "weight", wk(ik) )
              IF ( PRESENT( wfc_type )) CALL iotk_write_attr( attr, "wavefunction_type", &
                                                              TRIM( wfc_type(ik)) )
              CALL iotk_write_dat( ounit, "K_VECTOR"//TRIM(iotk_index(ik)), xk(:,ik), &
                                   COLUMNS=3, ATTR=attr )
              !
          ENDDO
          !
      ENDIF
      !
      CALL iotk_write_end( ounit, "IRREDUCIBLE_K_VECTORS" )
      !
      CALL iotk_write_end( ounit, "BRILLOUIN_ZONE" )
      !
    END SUBROUTINE crio_write_bz

!
!-------------------------------------------
! ... read subroutines
!-------------------------------------------
!
    !
    !------------------------------------------------------------------------
    SUBROUTINE crio_read_header( creator_name, creator_version, &
                                 format_name, format_version, title, ierr )
      !------------------------------------------------------------------------
      !
      IMPLICIT NONE
      CHARACTER(LEN=*),  OPTIONAL, INTENT(OUT) :: creator_name, creator_version
      CHARACTER(LEN=*),  OPTIONAL, INTENT(OUT) :: format_name, format_version
      CHARACTER(LEN=*),  OPTIONAL, INTENT(OUT) :: title
      INTEGER,                     INTENT(OUT) :: ierr

      CHARACTER(256) :: creator_name_, creator_version_
      CHARACTER(256) :: format_name_,     format_version_

      ierr = 0
      !
      !
      CALL iotk_scan_begin( iunit, "HEADER", IERR=ierr )
      IF (ierr/=0) RETURN
      !
      IF ( PRESENT( format_name ) .OR. PRESENT( format_version) ) THEN
          !
          CALL iotk_scan_empty( iunit, "FORMAT", ATTR=attr, IERR=ierr )
          IF (ierr/=0) RETURN
          !
          CALL iotk_scan_attr(attr, "name", format_name_, IERR=ierr)
          IF (ierr/=0) RETURN
          CALL iotk_scan_attr(attr, "version", format_version_, IERR=ierr )
          IF (ierr/=0) RETURN
          !
      ENDIF
      !
      CALL iotk_scan_empty( iunit, "CREATOR", ATTR=attr, IERR=ierr )
      IF (ierr/=0) RETURN
      !
      CALL iotk_scan_attr(attr, "name", creator_name_, IERR=ierr)
      IF (ierr/=0) RETURN
      CALL iotk_scan_attr(attr, "version", creator_version_, IERR=ierr )
      IF (ierr/=0) RETURN
      !
      IF ( PRESENT( title ) ) THEN
          !
          CALL iotk_scan_dat( iunit, "TITLE", title, IERR=ierr )
          IF (ierr/=0) RETURN
          !
      ENDIF
      !
      CALL iotk_scan_end( iunit, "HEADER", IERR=ierr )
      IF (ierr/=0) RETURN
      !
      IF ( PRESENT(creator_name) )     creator_name    = TRIM(creator_name_)
      IF ( PRESENT(creator_version) )  creator_version = TRIM(creator_version_)
      IF ( PRESENT(format_name) )      format_name     = TRIM(format_name_)
      IF ( PRESENT(format_version) )   format_version  = TRIM(format_version_)
      !
    END SUBROUTINE crio_read_header
    !
    !
    !------------------------------------------------------------------------
    SUBROUTINE crio_read_periodicity( num_of_periodic_dir, periodic_dir, ierr )
      !------------------------------------------------------------------------
      !
      INTEGER,           OPTIONAL, INTENT(OUT) :: num_of_periodic_dir
      INTEGER,           OPTIONAL, INTENT(OUT) :: periodic_dir(:)
      INTEGER,                     INTENT(OUT) :: ierr
      !
      INTEGER :: num_of_periodic_dir_
      !

      ierr=0
      !
      CALL iotk_scan_begin( iunit, "PERIODICITY", IERR=ierr )
      IF ( ierr /= 0 ) RETURN
      !
      CALL iotk_scan_dat( iunit, "NUMBER_OF_PERIODIC_DIRECTIONS", num_of_periodic_dir_, IERR=ierr )
      IF ( ierr /= 0 ) RETURN
      !
      IF ( PRESENT( periodic_dir) ) THEN
          !
          CALL iotk_scan_dat( iunit, "PERIODIC_DIRECTIONS", periodic_dir(1:num_of_periodic_dir_), IERR=ierr )
          IF ( ierr /= 0 ) RETURN
          !
      ENDIF
      !
      CALL iotk_scan_end( iunit, "PERIODICITY", IERR=ierr )
      IF ( ierr /= 0 ) RETURN
      !
      !
      IF ( PRESENT( num_of_periodic_dir ) )      num_of_periodic_dir = num_of_periodic_dir_
      !
    END SUBROUTINE crio_read_periodicity
    !
    !
    !------------------------------------------------------------------------
    SUBROUTINE crio_read_cell( avec, units, periodicity, ierr )
      !------------------------------------------------------------------------
      !
      REAL(dbl),         OPTIONAL, INTENT(OUT) :: avec(3,3)
      CHARACTER(LEN=*),  OPTIONAL, INTENT(OUT) :: units, periodicity
      INTEGER,                     INTENT(OUT) :: ierr
      !

      ierr=0
      !
      !
      CALL iotk_scan_begin( iunit, "CELL", IERR=ierr )
      IF ( ierr /= 0 ) RETURN
      !
      CALL iotk_scan_empty( iunit, "CELL_INFO", ATTR=attr, IERR=ierr )
      IF ( ierr /= 0 ) RETURN
      !
      IF ( PRESENT( units )) THEN
         CALL iotk_scan_attr( attr, "unit", units, IERR=ierr )
         IF ( ierr /= 0 ) RETURN
      ENDIF
      !
      IF ( PRESENT( periodicity )) THEN
         CALL iotk_scan_attr( attr, "periodicity", periodicity, IERR=ierr )
         IF ( ierr /= 0 ) RETURN
      ENDIF
      !
      IF ( PRESENT( avec ) ) THEN
         !
         CALL iotk_scan_dat( iunit, "CELL_VECTOR_A", avec(:,1), IERR=ierr )
         IF (ierr/=0) RETURN
         CALL iotk_scan_dat( iunit, "CELL_VECTOR_B", avec(:,2), IERR=ierr )
         IF (ierr/=0) RETURN
         CALL iotk_scan_dat( iunit, "CELL_VECTOR_C", avec(:,3), IERR=ierr )
         IF (ierr/=0) RETURN
         !
      ENDIF
      !
      CALL iotk_scan_end( iunit, "CELL", IERR=ierr )
      IF ( ierr /= 0 ) RETURN
      !
    END SUBROUTINE crio_read_cell
    !
    !
    !------------------------------------------------------------------------
    SUBROUTINE crio_read_cell_reciprocal( bvec, units, periodicity, ierr )
      !------------------------------------------------------------------------
      !
      REAL(dbl),         OPTIONAL, INTENT(OUT) :: bvec(3,3)
      CHARACTER(LEN=*),  OPTIONAL, INTENT(OUT) :: units, periodicity
      INTEGER,                     INTENT(OUT) :: ierr
      !

      ierr=0
      !
      !
      CALL iotk_scan_begin( iunit, "CELL_RECIPROCAL", IERR=ierr )
      IF ( ierr /= 0 ) RETURN
      !
      CALL iotk_scan_empty( iunit, "CELL_RECIPROCAL_INFO", ATTR=attr, IERR=ierr )
      IF ( ierr /= 0 ) RETURN
      !
      IF ( PRESENT( units )) THEN
         CALL iotk_scan_attr( attr, "unit", units, IERR=ierr )
         IF ( ierr /= 0 ) RETURN
      ENDIF
      !
      IF ( PRESENT( periodicity )) THEN
         CALL iotk_scan_attr( attr, "periodicity", periodicity, IERR=ierr )
         IF ( ierr /= 0 ) RETURN
      ENDIF
      !
      IF ( PRESENT( bvec ) ) THEN
         !
         CALL iotk_scan_dat( iunit, "CELL_RECIPROCAL_VECTOR_A", bvec(:,1), IERR=ierr )
         IF (ierr/=0) RETURN
         CALL iotk_scan_dat( iunit, "CELL_RECIPROCAL_VECTOR_B", bvec(:,2), IERR=ierr )
         IF (ierr/=0) RETURN
         CALL iotk_scan_dat( iunit, "CELL_RECIPROCAL_VECTOR_C", bvec(:,3), IERR=ierr )
         IF (ierr/=0) RETURN
         !
      ENDIF
      !
      CALL iotk_scan_end( iunit, "CELL_RECIPROCAL", IERR=ierr )
      IF ( ierr /= 0 ) RETURN
      !
    END SUBROUTINE crio_read_cell_reciprocal
    !
    !
    !------------------------------------------------------------------------
    SUBROUTINE crio_read_atoms( num_of_atoms, periodicity, atm_symb, atm_number, &
                                coords, units, coords_cry, ierr )
      !------------------------------------------------------------------------
      !
      INTEGER,           OPTIONAL, INTENT(OUT) :: num_of_atoms, periodicity
      REAL(dbl),         OPTIONAL, INTENT(OUT) :: coords(:,:), coords_cry(:,:)
      CHARACTER(LEN=*),  OPTIONAL, INTENT(OUT) :: atm_symb(:), units
      INTEGER,           OPTIONAL, INTENT(OUT) :: atm_number(:)
      INTEGER,                     INTENT(OUT) :: ierr
      !
      INTEGER            :: num_of_atoms_, periodicity_, ia
      CHARACTER(256)     :: units_
      !

      ierr=0
      units_ = ' '
      !
      CALL iotk_scan_begin( iunit, "ATOMS", IERR=ierr )
      IF ( ierr /= 0 ) RETURN
      !
      CALL iotk_scan_dat( iunit, "NUMBER_OF_ATOMS", num_of_atoms_, IERR=ierr )
      IF ( ierr /= 0 ) RETURN
      !
      IF ( PRESENT( coords ) ) THEN 
          !
          CALL iotk_scan_begin( iunit, "CARTESIAN_COORDINATES", ATTR=attr, IERR=ierr )
          IF ( ierr /= 0 ) RETURN
          !
          CALL iotk_scan_attr( attr, "unit", units_, IERR=ierr )
          IF ( ierr /= 0 ) RETURN
          !
          DO ia = 1, num_of_atoms_
              !
              CALL iotk_scan_dat( iunit, "ATOM"//TRIM(iotk_index(ia)), coords(:,ia), &
                                  ATTR=attr, IERR=ierr )
              IF ( ierr /= 0 ) RETURN
              !
              IF ( PRESENT( atm_symb ) ) THEN
                  !
                  CALL iotk_scan_attr( attr, "atomic_symbol", atm_symb(ia), IERR=ierr )
                  IF ( ierr /= 0 ) RETURN
                  !
              ENDIF
              !
              IF ( PRESENT( atm_number ) ) THEN
                  !
                  CALL iotk_scan_attr( attr, "atomic_number", atm_number(ia), IERR=ierr )
                  IF ( ierr /= 0 ) RETURN
                  !
              ENDIF
              !
          ENDDO
          !
          CALL iotk_scan_end( iunit, "CARTESIAN_COORDINATES", IERR=ierr )
          IF ( ierr /= 0 ) RETURN
          !
      ENDIF
      !
      !
      IF ( PRESENT( coords_cry ) .OR. PRESENT( periodicity ) ) THEN 
          !
          CALL iotk_scan_begin( iunit, "FRACTIONARY_COORDINATES", ATTR=attr, IERR=ierr )
          IF ( ierr /= 0 ) RETURN
          !
          CALL iotk_scan_attr( attr, "periodicity", periodicity_, IERR=ierr )
          IF ( ierr /= 0 ) RETURN
          !
          IF ( PRESENT( coords_cry) ) THEN
              !
              DO ia = 1, num_of_atoms_
                  !
                  CALL iotk_scan_dat( iunit, "ATOM"//TRIM(iotk_index(ia)), coords_cry(1:periodicity_,ia), &
                                      ATTR=attr, IERR=ierr )
                  IF ( ierr /= 0 ) RETURN
                  !
                  IF ( PRESENT( atm_symb ) ) THEN
                      !
                      CALL iotk_scan_attr( attr, "atomic_symbol", atm_symb(ia), IERR=ierr )
                      IF ( ierr /= 0 ) RETURN
                      !
                  ENDIF
                  !
                  IF ( PRESENT( atm_number ) ) THEN
                      !
                      CALL iotk_scan_attr( attr, "atomic_number", atm_number(ia), IERR=ierr )
                      IF ( ierr /= 0 ) RETURN
                      !
                  ENDIF
                  !
              ENDDO
              !
          ENDIF
          !
          CALL iotk_scan_end( iunit, "FRACTIONARY_COORDINATES", IERR=ierr )
          IF ( ierr /= 0 ) RETURN
          !
      ENDIF
      !
      !
      CALL iotk_scan_end( iunit, "ATOMS", IERR=ierr )
      IF ( ierr /= 0 ) RETURN
      !
      !
      IF ( PRESENT( num_of_atoms ) )       num_of_atoms = num_of_atoms_
      IF ( PRESENT( periodicity ) )        periodicity  = periodicity_
      IF ( PRESENT( units ) )              units        = TRIM(units_)
      !
    END SUBROUTINE crio_read_atoms
    !
    !
    !------------------------------------------------------------------------
    SUBROUTINE crio_read_symmetry( num_of_symmetries, ierr )
      !------------------------------------------------------------------------
      !
      INTEGER,           OPTIONAL, INTENT(OUT) :: num_of_symmetries
      INTEGER,                     INTENT(OUT) :: ierr
      !
      INTEGER            :: num_of_symmetries_
      !
      ierr=0
      !
      CALL iotk_scan_begin( iunit, "SYMMETRY", IERR=ierr )
      IF ( ierr /= 0 ) RETURN
      !
      CALL iotk_scan_dat( iunit, "NUMBER_OF_SYMMETRY_OPERATORS", num_of_symmetries_, IERR=ierr )
      IF ( ierr /= 0 ) RETURN
      !
      CALL iotk_scan_end( iunit, "SYMMETRY", IERR=ierr )
      IF ( ierr /= 0 ) RETURN
      !
      !
      IF ( PRESENT( num_of_symmetries ) )   num_of_symmetries = num_of_symmetries_
      !
    END SUBROUTINE crio_read_symmetry
    !
    !
    !------------------------------------------------------------------------
    SUBROUTINE crio_read_atomic_orbitals( num_of_atomic_orbitals, num_of_atoms, atm_orbital, ierr )
      !------------------------------------------------------------------------
      !
      INTEGER,                OPTIONAL, INTENT(OUT) :: num_of_atomic_orbitals, num_of_atoms
      TYPE(crio_atm_orbital), OPTIONAL, INTENT(OUT) :: atm_orbital(:)
      INTEGER,                          INTENT(OUT) :: ierr
      !
      INTEGER        :: num_of_atomic_orbitals_, num_of_atoms_, norb_
      INTEGER        :: i
      CHARACTER(256) :: str
      !

      ierr=0
      !
      CALL iotk_scan_begin( iunit, "ATOMIC_ORBITALS", IERR=ierr)
      IF ( ierr/=0 ) RETURN
      !
      CALL iotk_scan_empty( iunit, "ATOMIC_ORBITALS_INFO", ATTR=attr, IERR=ierr)
      IF ( ierr/=0 ) RETURN
      CALL iotk_scan_attr( attr, "number_of_atoms", num_of_atoms_, IERR=ierr)
      IF ( ierr/=0 ) RETURN
      !
      CALL iotk_scan_dat( iunit, "TOTAL_NUMBER_OF_ATOMIC_ORBITALS", num_of_atomic_orbitals_, IERR=ierr)
      IF ( ierr/=0 ) RETURN
      !
      !
      IF ( PRESENT( atm_orbital ) ) THEN
          !
          DO i = 1, num_of_atoms_
              !
              str="ATOMIC_ORBITALS_OF_ATOM"//TRIM(iotk_index( i ))
              !
              CALL iotk_scan_begin( iunit, TRIM(str), ATTR=attr, IERR=ierr ) 
              IF ( ierr/=0 ) RETURN
              !
              CALL iotk_scan_attr( attr, "number_of_atomic_orbitals", norb_, IERR=ierr ) 
              IF ( ierr/=0 ) RETURN
              CALL iotk_scan_attr( attr, "unit", atm_orbital(i)%units, IERR=ierr ) 
              IF ( ierr/=0 ) RETURN
              CALL iotk_scan_attr( attr, "centre", atm_orbital(i)%coord, IERR=ierr ) 
              IF ( ierr/=0 ) RETURN
              !
              CALL crio_atm_orbital_allocate( norb_, atm_orbital(i), ierr )
              IF ( ierr/=0 ) RETURN
              !
              CALL iotk_scan_dat( iunit, "SEQUENCE_NUMBER_LABEL", atm_orbital(i)%seq, IERR=ierr ) 
              IF ( ierr/=0 ) RETURN
              !
              CALL iotk_scan_dat( iunit, "TYPE", atm_orbital(i)%orb_type, IERR=ierr ) 
              IF ( ierr/=0 ) RETURN
              !
              !
              CALL iotk_scan_end( iunit, TRIM(str), IERR=ierr ) 
              IF ( ierr/=0 ) RETURN
              !
          ENDDO
          !
      ENDIF
      !
      !
      CALL iotk_scan_end( iunit, "ATOMIC_ORBITALS", IERR=ierr)
      IF ( ierr/=0 ) RETURN
      !
      !
      IF ( PRESENT( num_of_atomic_orbitals ) )     num_of_atomic_orbitals = num_of_atomic_orbitals_
      IF ( PRESENT( num_of_atoms ) )               num_of_atoms           = num_of_atoms_
      !
    END SUBROUTINE crio_read_atomic_orbitals
    !
    !
    !------------------------------------------------------------------------
    SUBROUTINE crio_read_direct_lattice( nrtot, rvec, rvec_cry, r_units, ierr )
      !------------------------------------------------------------------------
      !
      INTEGER,           OPTIONAL, INTENT(OUT) :: nrtot
      REAL(dbl),         OPTIONAL, INTENT(OUT) :: rvec(:,:)
      INTEGER,           OPTIONAL, INTENT(OUT) :: rvec_cry(:,:)
      CHARACTER(LEN=*),  OPTIONAL, INTENT(OUT) :: r_units
      INTEGER,                     INTENT(OUT) :: ierr
      !
      CHARACTER(256)     :: r_units_
      INTEGER            :: nrtot_, ntmp_, ir
      !

      ierr=0
      r_units_ = ' '
      !
      CALL iotk_scan_begin( iunit, "DIRECT_LATTICE", IERR=ierr )
      IF ( ierr /= 0 ) RETURN
      !
      CALL iotk_scan_dat( iunit, "NUMBER_OF_DIRECT_LATTICE_VECTORS", nrtot_, IERR=ierr )
      IF ( ierr /= 0 ) RETURN
      !
      IF ( PRESENT( rvec ) ) THEN 
          !
          CALL iotk_scan_begin( iunit, "CARTESIAN_VECTORS", IERR=ierr )
          IF ( ierr /= 0 ) RETURN
          !
          CALL iotk_scan_empty( iunit, "CARTESIAN_VECTORS_INFO", ATTR=attr, IERR=ierr )
          IF ( ierr /= 0 ) RETURN
          !
          CALL iotk_scan_attr( attr, "number_of_items", ntmp_, IERR=ierr )
          IF ( ierr /= 0 ) RETURN
          CALL iotk_scan_attr( attr, "unit", r_units_, IERR=ierr )
          IF ( ierr /= 0 ) RETURN
          !
          IF ( ntmp_ /= nrtot_ ) RETURN
          !
          DO ir = 1, nrtot_
              !
              CALL iotk_scan_dat( iunit, "CVDL"//TRIM(iotk_index(ir)), &
                                  rvec(1:3,ir), IERR=ierr )
              IF (ierr/=0) RETURN
              !
          ENDDO
          !
          CALL iotk_scan_end( iunit, "CARTESIAN_VECTORS", IERR=ierr )
          IF ( ierr /= 0 ) RETURN
          !
      ENDIF
      !
      IF ( PRESENT( rvec_cry ) ) THEN 
          !
          CALL iotk_scan_begin( iunit, "INTEGER_VECTORS", IERR=ierr )
          IF ( ierr /= 0 ) RETURN
          !
          CALL iotk_scan_empty( iunit, "INTEGER_VECTORS_INFO", ATTR=attr, IERR=ierr )
          IF ( ierr /= 0 ) RETURN
          !
          CALL iotk_scan_attr( attr, "number_of_items", ntmp_, IERR=ierr )
          IF ( ierr /= 0 ) RETURN
          !
          IF ( ntmp_ /= nrtot_ ) THEN
               ierr = 71
               RETURN
          ENDIF
          !
          DO ir = 1, nrtot_
              !
              CALL iotk_scan_dat( iunit, "IVDL"//TRIM(iotk_index(ir)), &
                                  rvec_cry(1:3,ir), IERR=ierr )
              IF (ierr/=0) RETURN
              !
          ENDDO
          !
          CALL iotk_scan_end( iunit, "INTEGER_VECTORS", IERR=ierr )
          IF ( ierr /= 0 ) RETURN
          !
      ENDIF
      !
      CALL iotk_scan_end( iunit, "DIRECT_LATTICE", IERR=ierr )
      IF ( ierr /= 0 ) RETURN
      !
      !
      IF ( PRESENT( nrtot ) )       nrtot  = nrtot_
      IF ( PRESENT(r_units) )      r_units = TRIM( r_units_ )
      !
    END SUBROUTINE crio_read_direct_lattice
    !
    !
    !------------------------------------------------------------------------
    SUBROUTINE crio_read_bz( num_k_points, nk, xk, k_units, wk, wfc_type, ierr )
      !------------------------------------------------------------------------
      !
      INTEGER,       OPTIONAL, INTENT(OUT) :: num_k_points, nk(:)
      REAL(dbl),     OPTIONAL, INTENT(OUT) :: xk(:,:), wk(:)
      CHARACTER(*),  OPTIONAL, INTENT(OUT) :: k_units, wfc_type(:)
      INTEGER,                 INTENT(OUT) :: ierr
      !
      INTEGER                :: num_k_points_, ntmp_
      REAL(dbl), ALLOCATABLE :: xk_(:,:)
      !
      INTEGER :: ik
      !

      ierr = 0
      !
      CALL iotk_scan_begin( iunit, "BRILLOUIN_ZONE", IERR=ierr )
      IF ( ierr/=0 ) RETURN
      !
      CALL iotk_scan_dat( iunit, "NUMBER_OF_IRREDUCIBLE_K_VECTORS", &
                                  num_k_points_, IERR=ierr )
      IF ( ierr/=0 ) RETURN
      !
      CALL iotk_scan_begin( iunit, "IRREDUCIBLE_K_VECTORS", IERR=ierr )
      IF ( ierr/=0 ) RETURN
      !
      CALL iotk_scan_empty( iunit, "IRREDUCIBLE_K_VECTORS_INFO", ATTR=attr, IERR=ierr )
      IF ( ierr/=0 ) RETURN
      CALL iotk_scan_attr( attr, "number_of_items", ntmp_, IERR=ierr )
      IF ( ierr/=0 ) RETURN
      !
      IF ( PRESENT( nk )) THEN
          CALL iotk_scan_attr( attr, "monkhorst_pack_grid", nk, IERR=ierr )
          IF ( ierr/=0 ) RETURN
      ENDIF
      !
      IF ( PRESENT( k_units )) THEN
          CALL iotk_scan_attr( attr, "unit", k_units, IERR=ierr )
          IF ( ierr/=0 ) RETURN
      ENDIF
      !
      IF ( ntmp_ /= num_k_points_ ) RETURN
      !
      !
      ALLOCATE( xk_( 3, num_k_points_ ) )
      !
      IF ( PRESENT(xk) .OR. PRESENT(wk) .OR. PRESENT(wfc_type) ) THEN
          !
          DO ik = 1, num_k_points_
              !
              CALL iotk_scan_dat( iunit, "K_VECTOR" // TRIM( iotk_index(ik) ), &
                                  xk_(:,ik), ATTR=attr, IERR=ierr )
              IF ( ierr/=0 ) RETURN
              !
              IF ( PRESENT( wk ) ) THEN
                  CALL iotk_scan_attr( attr, "weight", wk(ik), IERR=ierr )
                  IF ( ierr/=0 ) RETURN
              ENDIF
              !
              IF ( PRESENT( wfc_type ) ) THEN
                  CALL iotk_scan_attr( attr, "wavefunction_type", wfc_type(ik), IERR=ierr )
                  IF ( ierr/=0 ) RETURN
              ENDIF
              !
          ENDDO
          !
      ENDIF
      !
      CALL iotk_scan_end( iunit, "IRREDUCIBLE_K_VECTORS", IERR=ierr )
      IF ( ierr/=0 ) RETURN
      !
      CALL iotk_scan_end( iunit, "BRILLOUIN_ZONE", IERR=ierr )
      IF ( ierr/=0 ) RETURN
      !
      !
      IF ( PRESENT( num_k_points ) )       num_k_points  = num_k_points_
      IF ( PRESENT( xk ) )                 xk(1:3,1:num_k_points_) = xk_(:,:)
      !
      DEALLOCATE( xk_ )
      !
    END SUBROUTINE crio_read_bz
    !
    !
    !------------------------------------------------------------------------
    SUBROUTINE crio_read_overlap( label, form_of_data, units, dim_basis, nrtot, &
                                  ovp_matrix, ierr )
      !------------------------------------------------------------------------
      !
      CHARACTER(LEN=*),  OPTIONAL, INTENT(OUT) :: label, form_of_data, units
      INTEGER,           OPTIONAL, INTENT(OUT) :: dim_basis, nrtot
      REAL(dbl),         OPTIONAL, INTENT(OUT) :: ovp_matrix(:,:,:)
      INTEGER,                     INTENT(OUT) :: ierr
      !
      INTEGER :: dim_basis_, nrtot_
      INTEGER :: ir
      !

      ierr=0
      !
      !
      CALL iotk_scan_begin( iunit, "DIRECT_OVERLAP_MATRIX", IERR=ierr )
      IF ( ierr /= 0 ) RETURN
      !
      IF ( PRESENT( label ) ) THEN
          CALL iotk_scan_dat( iunit, "LABEL", label, IERR=ierr ) 
          IF ( ierr /= 0 ) RETURN
      ENDIF
      !
      IF ( PRESENT( form_of_data ) ) THEN
          CALL iotk_scan_dat( iunit, "FORM", form_of_data, IERR=ierr ) 
          IF ( ierr /= 0 ) RETURN
      ENDIF
      !
      IF ( PRESENT( units ) ) THEN
          CALL iotk_scan_dat( iunit, "UNITS", units, IERR=ierr ) 
          IF ( ierr /= 0 ) RETURN
      ENDIF
      !
      CALL iotk_scan_empty( iunit, "DIMENSIONS", ATTR=attr, IERR=ierr ) 
      IF ( ierr /= 0 ) RETURN
      !
      CALL iotk_scan_attr( attr, "dim", dim_basis_, IERR=ierr)
      IF ( ierr /= 0 ) RETURN
      CALL iotk_scan_attr( attr, "ngtot", nrtot_, IERR=ierr)
      IF ( ierr /= 0 ) RETURN
      !
      IF ( PRESENT( ovp_matrix ) ) THEN
          !
          CALL iotk_scan_dat( iunit, "OVERLAPS", ovp_matrix(:,:,:), IERR=ierr)
          IF ( ierr /= 0 ) RETURN
          !
!          DO ir = 1, nrtot_
!              !
!              CALL iotk_scan_dat( iunit, "OVP_G"//TRIM(iotk_index(ir)), &
!                                  ovp_matrix(:,:,ir), IERR=ierr )
!              IF ( ierr /= 0 ) RETURN
!              !
!          ENDDO
          !
      ENDIF
      !
      !
      CALL iotk_scan_end( iunit, "DIRECT_OVERLAP_MATRIX", IERR=ierr )
      IF (ierr/=0) RETURN
      !
      ! 
      IF ( PRESENT(dim_basis) ) dim_basis = dim_basis_
      IF ( PRESENT(nrtot) )         nrtot = nrtot_
      !
    END SUBROUTINE crio_read_overlap
    !
    !
    !------------------------------------------------------------------------
    SUBROUTINE crio_read_hamiltonian( label, form_of_data, units, dim_basis, nrtot, &
                                      ham_matrix, ierr )
      !------------------------------------------------------------------------
      !
      CHARACTER(LEN=*),  OPTIONAL, INTENT(OUT) :: label, form_of_data, units
      INTEGER,           OPTIONAL, INTENT(OUT) :: dim_basis, nrtot
      REAL(dbl),         OPTIONAL, INTENT(OUT) :: ham_matrix(:,:,:)
      INTEGER,                     INTENT(OUT) :: ierr
      !
      INTEGER :: dim_basis_, nrtot_
      INTEGER :: ir
      !

      ierr=0
      !
      !
      CALL iotk_scan_begin( iunit, "DIRECT_FOCK_KOHN-SHAM_MATRIX", IERR=ierr )
      IF ( ierr /= 0 ) RETURN
      !
      IF ( PRESENT( label ) ) THEN
          CALL iotk_scan_dat( iunit, "LABEL", label, IERR=ierr ) 
          IF ( ierr /= 0 ) RETURN
      ENDIF
      !
      IF ( PRESENT( form_of_data ) ) THEN
          CALL iotk_scan_dat( iunit, "FORM", form_of_data, IERR=ierr ) 
          IF ( ierr /= 0 ) RETURN
      ENDIF
      !
      IF ( PRESENT( units ) ) THEN
          CALL iotk_scan_dat( iunit, "UNITS", units, IERR=ierr ) 
          IF ( ierr /= 0 ) RETURN
      ENDIF
      !
      CALL iotk_scan_empty( iunit, "DIMENSIONS", ATTR=attr, IERR=ierr ) 
      IF ( ierr /= 0 ) RETURN
      !
      CALL iotk_scan_attr( attr, "dim", dim_basis_, IERR=ierr)
      IF ( ierr /= 0 ) RETURN
      CALL iotk_scan_attr( attr, "ngtot", nrtot_, IERR=ierr)
      IF ( ierr /= 0 ) RETURN
      !
      IF ( PRESENT( ham_matrix ) ) THEN
          !
          CALL iotk_scan_dat( iunit, "HAMILTONIAN", ham_matrix(:,:,:), IERR=ierr)
          IF ( ierr /= 0 ) RETURN
          !
!          DO ir = 1, nrtot_
!              !
!              CALL iotk_scan_dat( iunit, "HAM_G"//TRIM(iotk_index(ir)), &
!                                  ham_matrix(:,:,ir), IERR=ierr )
!              IF ( ierr /= 0 ) RETURN
!              !
!          ENDDO
!          !
      ENDIF
      !
      !
      CALL iotk_scan_end( iunit, "DIRECT_FOCK_KOHN-SHAM_MATRIX", IERR=ierr )
      IF (ierr/=0) RETURN
      !
      ! 
      IF ( PRESENT(dim_basis) ) dim_basis = dim_basis_
      IF ( PRESENT(nrtot) )         nrtot = nrtot_
      !
    END SUBROUTINE crio_read_hamiltonian
    !
    !
END MODULE crystal_io_module

