!
! Copyright (C) 2011 Andrea Ferretti
!
! This file is distributed under the terms of the
! GNU General Public License. See the file `License'
! in the root directory of the present distribution,
! or http://www.gnu.org/copyleft/gpl.txt .
!

!**************************
   MODULE decay_module
!**************************
   !
   USE kinds
   IMPLICIT NONE
   !
   PRIVATE
   !
   PUBLIC :: decay_read_file
   PUBLIC :: decay_valid_type
   !
CONTAINS

!*****************************************************************
   SUBROUTINE decay_read_file( filename, dimwann, nrtot, vr, ivr, opr, filetype, ierr)
   !*****************************************************************
   !
   USE iotk_module
   USE converters_module, ONLY : cry2cart
   !
   IMPLICIT NONE
      !
      CHARACTER(*),            INTENT(IN)  :: filename
      INTEGER,       OPTIONAL, INTENT(OUT) :: dimwann, nrtot
      REAL(dbl),     OPTIONAL, INTENT(OUT) :: vr(:,:)
      INTEGER,       OPTIONAL, INTENT(OUT) :: ivr(:,:)
      COMPLEX(dbl),  OPTIONAL, INTENT(OUT) :: opr(:,:,:)
      CHARACTER(*),  OPTIONAL, INTENT(OUT) :: filetype
      INTEGER,                 INTENT(OUT) :: ierr
     
      LOGICAL   :: lham, lsgm
      INTEGER   :: iunit, ir
      INTEGER   :: nrtot_, dimwann_
      REAL(dbl) :: avec(3,3)
      CHARACTER(256)       :: attr
      CHARACTER(15)        :: subname="decay_read_file"
      INTEGER, ALLOCATABLE :: ivr_(:,:)

      !
      ! main body
      !
      ierr = 0
      !
      lham = file_is_ham( filename ) 
      lsgm = file_is_sgm( filename ) 
      !
      IF ( PRESENT(filetype) ) THEN
          filetype=""
          IF ( lham ) filetype = "ham"
          IF ( lsgm ) filetype = "sgm"
      ENDIF
      !
      IF ( .NOT. ( lham .OR. lsgm ) ) CALL errore(subname,"unknown data file",10)
      !
      !
      CALL iotk_free_unit( iunit )
      !
      CALL iotk_open_read( iunit, filename, IERR=ierr)
      IF ( ierr/=0 ) RETURN
      !
      IF ( lham ) THEN
          !
          CALL iotk_scan_begin( iunit, "HAMILTONIAN", IERR=ierr)
          IF ( ierr/=0 ) RETURN
          !
      ENDIF
      !
      CALL iotk_scan_empty( iunit, "DATA", ATTR=attr, IERR=ierr)
      IF ( ierr/=0 ) RETURN
      !
      CALL iotk_scan_attr( attr, "dimwann", dimwann_, IERR=ierr)
      IF ( ierr/=0 ) RETURN
      CALL iotk_scan_attr( attr, "nrtot", nrtot_, IERR=ierr)
      IF ( ierr/=0 ) RETURN
      !
      IF ( PRESENT(dimwann) ) dimwann=dimwann_
      IF ( PRESENT(nrtot) )   nrtot=nrtot_

      !
      ! VR / IVR
      !
      IF ( ( PRESENT(vr) .OR. PRESENT(ivr) ) .AND. lham ) THEN 
          !
          CALL iotk_scan_dat( iunit, "DIRECT_LATTICE", avec, IERR=ierr)
          IF ( ierr/=0 ) RETURN
          !
          ALLOCATE( ivr_(3,nrtot_), STAT=ierr )
          IF ( ierr/=0 ) RETURN
          !
          CALL iotk_scan_dat( iunit, "IVR", ivr_, IERR=ierr)
          IF ( ierr/=0 ) RETURN
          !
          IF ( PRESENT( ivr ) ) ivr(1:3,1:nrtot_) = ivr_(1:3,1:nrtot_)
          !
          IF ( PRESENT( vr ) ) THEN
              vr(1:3,1:nrtot_) = ivr_(1:3,1:nrtot_)
              CALL cry2cart( vr, avec )
          ENDIF
          !
          DEALLOCATE( ivr_, STAT=ierr)
          IF ( ierr/=0 ) RETURN
          !
      ENDIF
      !
      IF ( PRESENT(vr) .AND. lsgm ) THEN 
          !
          CALL iotk_scan_dat( iunit, "VR", vr, IERR=ierr)
          IF ( ierr/=0 ) RETURN
          !
      ENDIF

      !
      ! IVR lsgm
      !
      IF ( PRESENT( ivr ) .AND. lsgm ) THEN
          CALL iotk_scan_dat( iunit, "IVR", ivr, IERR=ierr)
          IF ( ierr/=0 ) RETURN
      ENDIF

      !
      ! OPR
      !
      IF ( PRESENT( opr ) .AND. lham ) THEN
          !
          CALL iotk_scan_begin( iunit, "RHAM", IERR=ierr)
          IF ( ierr/=0 ) RETURN
          !
          DO ir = 1, nrtot_
              CALL iotk_scan_dat( iunit, "VR"//TRIM(iotk_index(ir)), opr(:,:,ir), IERR=ierr )
              IF ( ierr/=0 ) RETURN
          ENDDO
          !
          CALL iotk_scan_end( iunit, "RHAM", IERR=ierr)
          IF ( ierr/=0 ) RETURN
          !
      ENDIF
      !
      IF ( PRESENT( opr ) .AND. lsgm ) THEN
          !
          CALL iotk_scan_begin( iunit, "OPR", IERR=ierr)
          IF ( ierr/=0 ) RETURN
          !
          DO ir = 1, nrtot_
              CALL iotk_scan_dat( iunit, "VR"//TRIM(iotk_index(ir)), opr(:,:,ir), IERR=ierr )
              IF ( ierr/=0 ) RETURN
          ENDDO
          !
          CALL iotk_scan_end( iunit, "OPR", IERR=ierr)
          IF ( ierr/=0 ) RETURN
          !
      ENDIF
      !
      IF ( lham ) THEN
          !
          CALL iotk_scan_end( iunit, "HAMILTONIAN", IERR=ierr)
          IF ( ierr/=0 ) RETURN
          !
      ENDIF
      !
      RETURN
      !
END SUBROUTINE decay_read_file

!*****************************************************************
   LOGICAL FUNCTION file_is_ham( filename )
   !*****************************************************************
   !
   USE iotk_module
   IMPLICIT NONE
      !
      CHARACTER(*)  :: filename
      !
      INTEGER  :: iunit, ierr
      !
      file_is_ham = .FALSE.
      !
      CALL iotk_free_unit( iunit )
      CALL iotk_open_read( iunit, filename, IERR=ierr)
      IF ( ierr/=0 ) RETURN
      !
      CALL iotk_scan_begin( iunit, "HAMILTONIAN", IERR=ierr) 
      IF ( ierr == 0 ) THEN
          file_is_ham = .TRUE.
      ENDIF
      !
      RETURN
      !
END FUNCTION file_is_ham

!*****************************************************************
   LOGICAL FUNCTION file_is_sgm( filename )
   !*****************************************************************
   !
   USE iotk_module
   IMPLICIT NONE
      !
      CHARACTER(*)  :: filename
      !
      CHARACTER(256) :: attr
      INTEGER        :: iunit, ierr
      LOGICAL        :: ldum
      !
      file_is_sgm = .FALSE.
      !
      CALL iotk_free_unit( iunit )
      CALL iotk_open_read( iunit, filename, IERR=ierr)
      IF ( ierr/=0 ) RETURN
      !
      CALL iotk_scan_empty( iunit, "DATA", ATTR=attr, IERR=ierr) 
      IF ( ierr /= 0 ) RETURN
      !
      CALL iotk_scan_attr( attr, "dynamical", ldum, IERR=ierr) 
      IF ( ierr == 0 ) THEN
          file_is_sgm = .TRUE.
      ENDIF
      !
      RETURN
      !
END FUNCTION file_is_sgm


!*****************************************************************
   LOGICAL FUNCTION decay_valid_type( str )
   !*****************************************************************
   !
   USE iotk_module
   IMPLICIT NONE
      !
      CHARACTER(*)  :: str
      !
      SELECT CASE ( TRIM(str) )
      CASE ( "ham", "sgm" )
          decay_valid_type = .TRUE.
      CASE DEFAULT
          decay_valid_type = .FALSE.
      END SELECT
      !
      RETURN
      ! 
END FUNCTION decay_valid_type

END MODULE decay_module

