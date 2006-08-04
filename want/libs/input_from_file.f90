!
! Copyright (C) 2002-2005 PWSCF group
! This file is distributed under the terms of the
! GNU General Public License. See the file `License'
! in the root directory of the present distribution,
! or http://www.gnu.org/copyleft/gpl.txt .
!
! Slightly modified by Andrea Ferretti
!
#include "machine.h"
!
!----------------------------------------------------------------------------
SUBROUTINE input_from_file( iunit, ierr)
  !
  ! This subroutine checks program arguments and, if input file is present,
  ! attach input unit IUNIT to the specified file
  !
  ! use the macro defined in machine.h to
  ! take care of name redefinitions of c-funcitons
  !
  IMPLICIT NONE
  !
  ! input variables
  !
  INTEGER,  INTENT(IN)  :: iunit
  INTEGER,  INTENT(OUT) :: ierr

  !
  ! local variables
  !
  INTEGER  :: ilen, iiarg, nargs
  !
  ! do not define iargc as external: g95 does not like it
  INTEGER             :: iargc
  CHARACTER(LEN=256)  :: input_file
  !
  ! end of declariations
  !

!
!------------------------------
! main body
!------------------------------
!

  !
  ! ... Input from file ?
  !
  ierr  = 0
  nargs = iargc ()
  !
  DO iiarg = 1, ( nargs - 1 )
     !
     CALL getarg ( iiarg, input_file )
     !
     IF ( TRIM( input_file ) == '--input' .OR. &
          TRIM( input_file ) == '-input'  .OR. &
          TRIM( input_file ) == '--inp'   .OR. &
          TRIM( input_file ) == '-inp'    .OR. &
          TRIM( input_file ) == '--in'    .OR. & 
          TRIM( input_file ) == '-in'     .OR. &
          TRIM( input_file ) == '-i'        ) THEN
        !
        CALL getarg ( ( iiarg + 1 ) , input_file )
        !
        OPEN ( UNIT = iunit, FILE = input_file, FORM = 'FORMATTED', &
               STATUS = 'OLD', IOSTAT = ierr )
     ENDIF
     !
  ENDDO

END SUBROUTINE input_from_file

