! 
! Copyright (C) 2005 WanT Group
! 
! This file is distributed under the terms of the 
! GNU General Public License. See the file `License' 
! in the root directory of the present distribution, 
! or http://www.gnu.org/copyleft/gpl.txt . 
! 
! based on previous routines by Carlo Cavazzoni
!
!********************************************
   MODULE input_base_module
!********************************************
   USE kinds, ONLY : dbl
   USE io_module, ONLY : stdout
   IMPLICIT NONE
   PRIVATE
!
! This module contains basic routines to read
! cards from input
!
! routines in this module:
! SUBROUTINE read_cards(unit)
! SUBROUTINE card_wannier_centers(unit,line)
! 

   PUBLIC :: read_cards
   PUBLIC :: card_wannier_centers


CONTAINS

!
! subroutines
!

!**********************************************************
   SUBROUTINE read_cards(unit)
   !**********************************************************
   !
   ! read the following cards:
   ! - WANNIER_CENTERS
   !
   USE parser_module, ONLY: read_line, capital
   IMPLICIT NONE
      INTEGER, INTENT(in):: unit

      CHARACTER(LEN=256) :: input_line
      CHARACTER(LEN=80)  :: card
      LOGICAL            :: tend
      LOGICAL            :: wannier_centers_found
      INTEGER :: ios
      INTEGER :: i, ierr
      !

      !
      wannier_centers_found = .FALSE.
 100  CALL read_line(unit, input_line, END_OF_FILE=tend )
      !
      IF( tend ) GO TO 120
      IF( input_line == ' ' .OR. input_line(1:1) == '#' ) GO TO 100
      !
      READ (input_line, *) card
       
      DO i = 1, LEN_TRIM( input_line )
         input_line( i : i ) = capital( input_line( i : i ) )
      ENDDO
       
      !
      IF ( TRIM(card) == 'WANNIER_CENTERS' ) THEN
         !
         CALL card_wannier_centers(unit,input_line)
         wannier_centers_found = .TRUE.
         !
      ELSE
         !
         WRITE( stdout,'(A)') 'Warning: card '//TRIM(input_line)//' ignored'
         !
      ENDIF
      !
      ! ...     END OF LOOP ... !
      !
      GOTO 100
      !
120   CONTINUE
      !

      IF ( .NOT. wannier_centers_found ) &
            CALL errore('read_cards','Card WANNIER_CENTERS not found',1)
    RETURN
  END SUBROUTINE read_cards


!**********************************************************
   SUBROUTINE card_wannier_centers(unit, input_line )
   !**********************************************************
      USE parser_module, ONLY: read_line, matches
      USE trial_center_data_module, ONLY : list => trial
      USE trial_center_module
      IMPLICIT NONE
      !
      INTEGER,            INTENT(in) :: unit
      CHARACTER(LEN=256), INTENT(in) :: input_line

      LOGICAL, SAVE      :: tread = .FALSE.
      CHARACTER(LEN=256) :: tmp_line
      INTEGER            :: dim
      INTEGER            :: i, iwann, ierr
      CHARACTER(LEN=10)  :: adum, units
      !
      !
      IF ( tread ) CALL errore( 'card_wannier_centers', ' two occurrence ', 2 )

      dim = SIZE( list )

      IF ( matches('CRYSTAL', input_line ) ) THEN
          units = 'crystal'
      ELSEIF ( matches('BOHR', input_line ) ) THEN
          units = 'bohr'
      ELSEIF ( matches('ANGSTROM', input_line ) ) THEN
          units = 'angstrom'
      ELSE
          IF ( TRIM( ADJUSTL( input_line ) ) /= 'WANNIER_CENTERS' ) THEN
               CALL errore( 'card_wannier_centers', &
                          & ' unknow unit option for WANNIER_CENTERS: '&
                          & //input_line, 1 )
          ENDIF
          units = 'crystal'
      ENDIF


      !
      ! through the trial centers
      !
      DO iwann = 1, dim

           !
           ! ... init center
           CALL trial_center_init(list(iwann))
           list(iwann)%units = TRIM(units)

           CALL read_line(unit, tmp_line )
           READ(tmp_line,*) list(iwann)%type 

           !
           ! ... chose the center type
           SELECT CASE ( TRIM(list(iwann)%type) )
           CASE ( "1gauss" )
               READ(tmp_line,*, IOSTAT=ierr) adum, list(iwann)%x1(1:3), &
                     list(iwann)%l, list(iwann)%m, list(iwann)%ndir, list(iwann)%decay
               IF (ierr/=0) CALL errore('card_wannier_centers','reading line',ABS(ierr))

           CASE ( "2gauss" ) 
               !
               ! gaussians has explicitly s symmetry
               !
               list(iwann)%l = 0
               list(iwann)%m = 0
               list(iwann)%ndir = 3
               READ(tmp_line,*, IOSTAT=ierr) adum, &
                     list(iwann)%x1(1:3), list(iwann)%x2(1:3), list(iwann)%decay
               IF (ierr/=0) CALL errore('card_wannier_centers','reading line',ABS(ierr))

           CASE ( "atomic" )
               CALL errore('card_wannier_centers','ATOMIC trial type not implem',iwann)
           CASE DEFAULT
               CALL errore('card_wannier_centers','Wrong wannier center type',iwann)
           END SELECT
           list(iwann)%alloc = .TRUE.

       ENDDO
  END SUBROUTINE card_wannier_centers

END MODULE input_base_module

