!
! Copyright (C) 2007 WanT Group
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
  IMPLICIT NONE
  !
  PRIVATE
  SAVE
  !
  ! definitions for the fmt
  !
  CHARACTER(10), PARAMETER :: fmt_name = "CRYSTAL_IO"
  CHARACTER(5),  PARAMETER :: fmt_version = "0.1.0"
  !
  ! some default for kinds
  !
  INTEGER,   PARAMETER :: dbl = SELECTED_REAL_KIND( 14, 200 )
  REAL(dbl), PARAMETER :: e2 = 2.0_dbl
  !
  ! internal data to be set
  !
  CHARACTER(256)   :: file_in, file_out
  INTEGER          :: iunit, ounit
  !
  CHARACTER(iotk_attlenx) :: attr
  !
  !
  ! end of declarations
  !

  PUBLIC :: fmt_name, fmt_version
  PUBLIC :: iunit, ounit
  !
  PUBLIC :: crio_init,  crio_open_file,    crio_close_file
  PUBLIC ::             crio_open_section, crio_close_section
  !
  PUBLIC :: crio_write_header
  !
  PUBLIC :: crio_read_header,          crio_read_cell,      &
            crio_read_symmetry,        crio_read_atoms,     &
            crio_read_direct_lattice,  crio_read_bz,        &
            crio_read_hamiltonian,     crio_read_overlap 

CONTAINS

!
!-------------------------------------------
! ... basic (public) subroutines
!-------------------------------------------
!
    !------------------------------------------------------------------------
    SUBROUTINE crio_init( unit_in, unit_out, filein, fileout )
      !------------------------------------------------------------------------
      !
      ! just init module data
      !
      IMPLICIT NONE
      INTEGER,                INTENT(IN) :: unit_in
      INTEGER,      OPTIONAL, INTENT(IN) :: unit_out
      CHARACTER(*), OPTIONAL, INTENT(IN) :: filein, fileout
      !
      iunit       = unit_in
      ounit       = unit_in
      IF ( PRESENT( unit_out ) ) ounit  = unit_out
      !
      !
      file_in  = " "
      file_out = "./"
      !
      !
      IF ( PRESENT( filein ) ) THEN
          file_in  = TRIM(filein)
      ENDIF
      !
      IF ( PRESENT( fileout ) ) THEN
          file_out  = TRIM(fileout)
      ENDIF
      !
    END SUBROUTINE crio_init
    !
    !
    !------------------------------------------------------------------------
    SUBROUTINE crio_open_file( filename, action, binary, ierr)
      !------------------------------------------------------------------------
      !
      ! open data file
      !
      IMPLICIT NONE
      !
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
          CALL iotk_open_read ( iunit, FILE = TRIM(filename), IERR=ierr )
          !
      CASE ( "write", "WRITE" )
          !
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
    SUBROUTINE crio_write_header( creator_name, creator_version ) 
      !------------------------------------------------------------------------
      !
      IMPLICIT NONE
      CHARACTER(LEN=*), INTENT(IN) :: creator_name, creator_version
      !
      CALL iotk_write_begin( ounit, "HEADER" )
      !
      CALL iotk_write_attr(attr, "NAME",TRIM(fmt_name), FIRST=.TRUE.)
      CALL iotk_write_attr(attr, "VERSION",TRIM(fmt_version) )
      CALL iotk_write_empty( ounit, "FORMAT", ATTR=attr )
      !
      CALL iotk_write_attr(attr, "NAME",TRIM(creator_name), FIRST=.TRUE.)
      CALL iotk_write_attr(attr, "VERSION",TRIM(creator_version) )
      CALL iotk_write_empty( ounit, "CREATOR", ATTR=attr )
      !
      CALL iotk_write_end( ounit, "HEADER" )
      !
    END SUBROUTINE crio_write_header
    !

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
      CALL iotk_scan_end( iunit, "HEADER", IERR=ierr )
      IF (ierr/=0) RETURN
      !
      IF ( PRESENT( title ) ) THEN
          !
          CALL iotk_scan_dat( iunit, "TITLE", title, IERR=ierr )
          IF (ierr/=0) RETURN
          !
      ENDIF
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
    SUBROUTINE crio_read_cell( avec, bvec, a_units, b_units, ierr )
      !------------------------------------------------------------------------
      !
      REAL(dbl),         OPTIONAL, INTENT(OUT) :: avec(3,3)
      REAL(dbl),         OPTIONAL, INTENT(OUT) :: bvec(3,3)
      CHARACTER(LEN=*),  OPTIONAL, INTENT(OUT) :: a_units, b_units
      INTEGER,                     INTENT(OUT) :: ierr
      !
      CHARACTER(256)     :: a_units_, b_units_
      REAL(dbl)          :: avec_(3,3), bvec_(3,3)
      !

      ierr=0
      !
      !
      CALL iotk_scan_begin( iunit, "CELL", IERR=ierr )
      IF ( ierr /= 0 ) RETURN
      !
      CALL iotk_scan_empty( iunit, "CELL_INFO", ATTR=attr, IERR=ierr )
      IF ( ierr /= 0 ) RETURN
      CALL iotk_scan_attr( attr, "unit", a_units_, IERR=ierr )
      IF ( ierr /= 0 ) RETURN
      !
      CALL iotk_scan_dat( iunit, "CELL_VECTOR_A", avec_(:,1), IERR=ierr )
      IF (ierr/=0) RETURN
      CALL iotk_scan_dat( iunit, "CELL_VECTOR_B", avec_(:,2), IERR=ierr )
      IF (ierr/=0) RETURN
      CALL iotk_scan_dat( iunit, "CELL_VECTOR_C", avec_(:,3), IERR=ierr )
      IF (ierr/=0) RETURN
      !
      CALL iotk_scan_end( iunit, "CELL", IERR=ierr )
      IF ( ierr /= 0 ) RETURN
      !
      !
      CALL iotk_scan_begin( iunit, "CELL_RECIPROCAL", IERR=ierr )
      IF ( ierr /= 0 ) RETURN
      !
      CALL iotk_scan_empty( iunit, "CELL_RECIPROCAL_INFO", ATTR=attr, IERR=ierr )
      IF ( ierr /= 0 ) RETURN
      CALL iotk_scan_attr( attr, "unit", b_units_, IERR=ierr )
      IF ( ierr /= 0 ) RETURN
      !
      CALL iotk_scan_dat( iunit, "CELL_RECIPROCAL_VECTOR_A", bvec_(:,1), IERR=ierr )
      IF (ierr/=0) RETURN
      CALL iotk_scan_dat( iunit, "CELL_RECIPROCAL_VECTOR_B", bvec_(:,2), IERR=ierr )
      IF (ierr/=0) RETURN
      CALL iotk_scan_dat( iunit, "CELL_RECIPROCAL_VECTOR_C", bvec_(:,3), IERR=ierr )
      IF (ierr/=0) RETURN
      !
      CALL iotk_scan_end( iunit, "CELL_RECIPROCAL", IERR=ierr )
      IF ( ierr /= 0 ) RETURN
      !
      ! 
      IF ( PRESENT(avec) )       avec = avec_
      IF ( PRESENT(bvec) )       bvec = bvec_
      IF ( PRESENT(a_units) ) a_units = TRIM( a_units_ )
      IF ( PRESENT(b_units) ) b_units = TRIM( b_units_ )
      !
    END SUBROUTINE crio_read_cell
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
      CALL iotk_scan_begin( iunit, "GEOMETRY", IERR=ierr )
      IF ( ierr /= 0 ) RETURN
      !
      CALL iotk_scan_empty( iunit, "CELL_INFO", ATTR=attr, IERR=ierr )
      IF ( ierr /= 0 ) RETURN
      CALL iotk_scan_attr( attr, "periodicity", periodicity_, IERR=ierr )
      IF ( ierr /= 0 ) RETURN
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
      IF ( PRESENT( coords_cry ) ) THEN 
          !
          CALL iotk_scan_begin( iunit, "FRACTIONARY_COORDINATES", ATTR=attr, IERR=ierr )
          IF ( ierr /= 0 ) RETURN
          !
          CALL iotk_scan_attr( attr, "unit", units_, IERR=ierr )
          IF ( ierr /= 0 ) RETURN
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
          CALL iotk_scan_end( iunit, "FRACTIONARY_COORDINATES", IERR=ierr )
          IF ( ierr /= 0 ) RETURN
          !
      ENDIF
      !
      !
      CALL iotk_scan_end( iunit, "ATOMS", IERR=ierr )
      IF ( ierr /= 0 ) RETURN
      !
      CALL iotk_scan_end( iunit, "GEOMETRY", IERR=ierr )
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
      CALL iotk_scan_begin( iunit, "GEOMETRY", IERR=ierr )
      IF ( ierr /= 0 ) RETURN
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
      CALL iotk_scan_end( iunit, "GEOMETRY", IERR=ierr )
      IF ( ierr /= 0 ) RETURN
      !
      !
      IF ( PRESENT( num_of_symmetries ) )   num_of_symmetries = num_of_symmetries_
      !
    END SUBROUTINE crio_read_symmetry
    !
    !
    !------------------------------------------------------------------------
    SUBROUTINE crio_read_direct_lattice( nrtot, rvec, rvec_cry, r_units, ierr )
      !------------------------------------------------------------------------
      !
      INTEGER,           OPTIONAL, INTENT(OUT) :: nrtot
      REAL(dbl),         OPTIONAL, INTENT(OUT) :: rvec(:,:), rvec_cry(:,:)
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
          CALL iotk_scan_attr( attr, "unit", r_units_, IERR=ierr )
          IF ( ierr /= 0 ) RETURN
          !
          IF ( ntmp_ /= nrtot_ ) RETURN
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
    SUBROUTINE crio_read_bz( num_k_points, xk, wk, nk1, nk2, nk3, &
                             k_units, ierr )
      !------------------------------------------------------------------------
      !
      INTEGER,       OPTIONAL, INTENT(OUT) :: num_k_points, nk1, nk2, nk3
      REAL(dbl),     OPTIONAL, INTENT(OUT) :: xk(:,:), wk(:)
      CHARACTER(*),  OPTIONAL, INTENT(OUT) :: k_units
      INTEGER,                 INTENT(OUT) :: ierr
      !
      INTEGER                :: num_k_points_, nk_(3), ntmp_
      CHARACTER(256)         :: k_units_
      REAL(dbl), ALLOCATABLE :: xk_(:,:), wk_(:)
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
      CALL iotk_scan_attr( attr, "monkhorst_pack_grid", nk_(1:3), IERR=ierr )
      IF ( ierr/=0 ) RETURN
      CALL iotk_scan_attr( attr, "unit", k_units_, IERR=ierr )
      IF ( ierr/=0 ) RETURN
      !
      IF ( ntmp_ /= num_k_points_ ) RETURN
      !
      !
      ALLOCATE( xk_( 3, num_k_points_ ) )
      ALLOCATE( wk_(    num_k_points_ ) )
      !
      DO ik = 1, num_k_points_
          !
          CALL iotk_scan_dat( iunit, "K_VECTOR" // TRIM( iotk_index(ik) ), &
                              xk_(:,ik), ATTR=attr, IERR=ierr )
          IF ( ierr/=0 ) RETURN
          !
          CALL iotk_scan_attr( attr, "weight", wk_(ik), IERR=ierr )
          IF ( ierr/=0 ) RETURN
          !
      ENDDO
      !
      CALL iotk_scan_end( iunit, "IRREDUCIBLE_K_VECTORS", IERR=ierr )
      IF ( ierr/=0 ) RETURN
      !
      CALL iotk_scan_end( iunit, "BRILLOUIN_ZONE", IERR=ierr )
      IF ( ierr/=0 ) RETURN
      !
      !
      IF ( PRESENT( num_k_points ) )       num_k_points  = num_k_points_
      IF ( PRESENT( nk1 ) )                nk1           = nk_(1)
      IF ( PRESENT( nk2 ) )                nk2           = nk_(2)
      IF ( PRESENT( nk3 ) )                nk3           = nk_(3)
      IF ( PRESENT( k_units ) )            k_units       =  TRIM(k_units_)
      IF ( PRESENT( xk ) )                 xk(1:3,1:num_k_points_) = xk_(:,:)
      IF ( PRESENT( wk ) )                 wk(1:num_k_points_)     = wk_(:)
      !
      DEALLOCATE( xk_ )
      DEALLOCATE( wk_ )
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

