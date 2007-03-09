!
! Copyright (C) 2007 WanT Group
! This file is distributed under the terms of the
! GNU General Public License. See the file `License'
! in the root directory of the present distribution,
! or http://www.gnu.org/copyleft/gpl.txt .
!
!----------------------------------------------------------------------------
MODULE operator_module
  !----------------------------------------------------------------------------
  !
  ! This module contains subroutines used to read and write
  ! operators and dynamical operators on the Wannier basis in
  ! iotk-XML fmt
  !
  USE kinds
  USE constants, ONLY : ZERO
  USE iotk_module
  IMPLICIT NONE
  !
  PRIVATE
  !
  CHARACTER( iotk_attlenx ) :: attr
  !
  !
  PUBLIC :: operator_read_aux
  PUBLIC :: operator_read_data
  !
CONTAINS

!
!-------------------------------------------
! ... basic (public) subroutines
!-------------------------------------------
!
    !
    !------------------------------------------------------------------------
    SUBROUTINE operator_read_aux( iun, dimwann, dynamical, nomega, grid, analyticity, &
                                  nr, vr, ivr, ierr ) 
      !------------------------------------------------------------------------
      !
      INTEGER,                     INTENT(IN)  :: iun
      INTEGER,           OPTIONAL, INTENT(OUT) :: dimwann, nomega, nr
      LOGICAL,           OPTIONAL, INTENT(OUT) :: dynamical
      REAL(dbl),         OPTIONAL, INTENT(OUT) :: grid(:), vr(:,:)
      INTEGER,           OPTIONAL, INTENT(OUT) :: ivr(:,:)
      CHARACTER(LEN=*),  OPTIONAL, INTENT(OUT) :: analyticity
      INTEGER,                     INTENT(OUT) :: ierr
      !
      INTEGER           :: dimwann_, nomega_, nr_
      LOGICAL           :: dynamical_
      CHARACTER(256)    :: analyticity_
      !

      ierr=0
      !
      !
      CALL iotk_scan_empty( iun, "DATA", ATTR=attr, IERR=ierr )
      IF ( ierr /= 0 ) RETURN
      !
      CALL iotk_scan_attr( attr, "dimwann", dimwann_, IERR=ierr )
      IF (ierr/=0) RETURN
      CALL iotk_scan_attr( attr, "nrtot", nr_, IERR=ierr )
      IF ( ierr /= 0 ) RETURN
      CALL iotk_scan_attr( attr, "dynamical", dynamical_, IERR=ierr )
      IF ( ierr /= 0 ) RETURN
      !
      IF ( dynamical_ ) THEN
         !
         CALL iotk_scan_attr( attr, "nomega", nomega_, IERR=ierr )
         IF ( ierr /= 0 ) RETURN
         !
         CALL iotk_scan_attr( attr, "analyticity", analyticity_, IERR=ierr )
         IF ( ierr /= 0 ) RETURN
         !
      ELSE
         nomega_ = 1
         analyticity_ = ""
      ENDIF
      !
      IF ( PRESENT ( vr ) ) THEN
         !
         CALL iotk_scan_dat( iun, "VR", vr, IERR=ierr )
         IF (ierr/=0) RETURN
         !
      ENDIF
      !
      IF ( PRESENT ( ivr ) ) THEN
         !
         CALL iotk_scan_dat( iun, "IVR", ivr, IERR=ierr )
         IF (ierr/=0) RETURN
         !
      ENDIF
      !
      IF ( PRESENT ( grid ) ) THEN
         !
         IF ( dynamical_ ) THEN
            !
            CALL iotk_scan_dat( iun, "GRID", grid, IERR=ierr )
            IF (ierr/=0) RETURN
            !
         ELSE
            grid(:) = ZERO
         ENDIF
         !
      ENDIF
      !
      ! 
      IF ( PRESENT(dimwann) )      dimwann      = dimwann_
      IF ( PRESENT(nr) )           nr           = nr_
      IF ( PRESENT(dynamical) )    dynamical    = dynamical_
      IF ( PRESENT(nomega) )       nomega       = nomega_
      IF ( PRESENT(analyticity) )  analyticity  = TRIM( analyticity_ )
      !
    END SUBROUTINE operator_read_aux
    !
    !
    !------------------------------------------------------------------------
    SUBROUTINE operator_read_data( iun, ie, r_opr, ierr )
      !------------------------------------------------------------------------
      !
      INTEGER,                 INTENT(IN)  :: iun
      INTEGER,       OPTIONAL, INTENT(IN)  :: ie
      COMPLEX(dbl),  OPTIONAL, INTENT(OUT) :: r_opr(:,:,:)
      INTEGER,                 INTENT(OUT) :: ierr
      !
      CHARACTER( 256 ) :: str
      INTEGER          :: ir
      LOGICAL          :: ldynam
      !
      ierr = 0
      !
      ldynam = .FALSE.
      IF ( PRESENT (ie) ) ldynam = .TRUE.
      !
      IF ( PRESENT ( r_opr ) ) THEN
         !
         str = "OPR"
         IF ( ldynam ) str = TRIM(str)//TRIM( iotk_index(ie) ) 
         !
         CALL iotk_scan_begin( iun, TRIM(str), IERR=ierr )
         IF ( ierr/=0 ) RETURN
         !
         DO ir = 1, SIZE( r_opr, 3 )
            !
            CALL iotk_scan_dat( iun, "VR"//TRIM(iotk_index(ir)), r_opr(:,:,ir), IERR=ierr )
            IF ( ierr/=0 ) RETURN
            !
         ENDDO
         !
         CALL iotk_scan_end( iun, TRIM(str), IERR=ierr )
         IF ( ierr/=0 ) RETURN
         !
      ENDIF
      !
    END SUBROUTINE operator_read_data
    !
    !
END MODULE operator_module

