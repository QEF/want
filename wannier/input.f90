! 
! Copyright (C) 2004 Arrigo Calzolari, Carlo Cavazzoni, Marco Buongiorno Nardelli 
! 
! This file is distributed under the terms of the 
! GNU General Public License. See the file `License' 
! in the root directory of the present distribution, 
! or http://www.gnu.org/copyleft/gpl.txt . 
! 
!********************************************
   MODULE input_module
!********************************************
   USE kinds
   USE constants, ONLY : ZERO
   USE io_module, ONLY : prefix, postfix, work_dir, title
   USE kpoints_module, ONLY : nshells, nwhich
   USE subspace_module, ONLY : dimwann
   USE windows_module, ONLY : win_min, win_max, froz_min, froz_max
   IMPLICIT NONE
   PRIVATE
   SAVE


   REAL(dbl) :: alpha
   REAL(dbl) :: disentangle_thr      ! disentangle threshold for convergence
   REAL(dbl) :: wannier_thr          ! wannier threshold 
   LOGICAL   :: assume_ncpp          ! implicitly assume NCPP without reading them
   INTEGER :: maxiter, itrial
   INTEGER :: nprint                 ! each nprint iterations write to stdout

   INTEGER :: iphase
   REAL(dbl)  :: alphafix0, alphafix
   INTEGER :: niter, niter0, ncg
   CHARACTER(10) :: ordering_type    ! ( "spatial" | "complete" | "none" ) 
   CHARACTER(6)  :: verbosity        ! ( "none" | "low" | "medium" | "high" )

   LOGICAL              :: alloc = .FALSE.

   NAMELIST / input_wan / win_min, win_max, froz_min, froz_max, dimwann, &
     alpha, maxiter, disentangle_thr, wannier_thr, itrial, iphase, alphafix0, &
     alphafix, niter, niter0, ncg, nshells, nwhich, ordering_type, verbosity, &
     nprint, assume_ncpp, prefix, postfix, work_dir, title
  
   PUBLIC :: alpha
   PUBLIC :: disentangle_thr
   PUBLIC :: wannier_thr
   PUBLIC :: assume_ncpp
   PUBLIC :: maxiter, itrial
   PUBLIC :: nprint
   PUBLIC :: iphase
   PUBLIC :: alphafix0, alphafix
   PUBLIC :: niter, niter0, ncg
   PUBLIC :: ordering_type
   PUBLIC :: verbosity

   PUBLIC :: alloc
   PUBLIC :: input_wan

   PUBLIC :: input_read

!
CONTAINS
!
!
! subroutines
!

!**********************************************************
   SUBROUTINE input_read
   !**********************************************************
      USE io_module, ONLY: stdin, stdout, ionode, ionode_id
      USE trial_center_data_module, ONLY : trial, trial_center_data_allocate
      USE mp, ONLY: mp_bcast
      USE parser_module, ONLY: read_line, capital
      IMPLICIT NONE

      INTEGER :: ios
      INTEGER :: i, ierr

      CHARACTER(LEN=256) :: input_line
      CHARACTER(LEN=80)  :: card
      LOGICAL            :: tend
      !

      prefix = "WanT"
      postfix = " "
      work_dir = "./"
      title = "WanT Calculation"

      win_min = ZERO
      win_max = ZERO
      froz_min = -1.1d10 
      froz_max = -1.0d10
      alpha = 0.5d0
      maxiter = 1000
      disentangle_thr = 1d-8
      wannier_thr = 1d-6
      iphase  = 1
      niter0 = 500
      alphafix0 = 0.5d0
      niter = 500  
      alphafix = 0.5d0
      ncg = 3
      nprint = 10
      dimwann = 1
      nshells = 1
      nwhich = 1 
      itrial = 3
      ordering_type = 'none'
      verbosity = 'medium'
      assume_ncpp = .FALSE.

      ios = 0
      IF( ionode ) THEN
         READ( stdin, input_wan, iostat = ios )
      END IF
      CALL mp_bcast( ios, ionode_id )
      IF( ios /= 0 ) &
         CALL errore( ' input_read ',' reading namelist input_wan ', ABS(ios) )

      IF ( win_max <= win_min ) &
          CALL errore( ' input_read ', ' win_max is not larger than win_min ', 1 )
      IF ( froz_max <= froz_min ) &
          CALL errore( ' input_read ', ' froz_max is not larger than froz_min ', 1 )
      IF ( dimwann <= 0 ) &
          CALL errore( ' input_read ', ' dimwann too small ', 1 )
      IF ( alpha <= 0.0d0 ) &
          CALL errore( ' input_read ', ' alpha must be positive ', 1 )
      IF ( maxiter <= 0 ) &
          CALL errore( ' input_read ', ' maxiter must be positive ', 1 )
      IF ( disentangle_thr <= 0 ) &
          CALL errore( ' input_read ', ' disentangle_thr must be positive ', 1 )
      IF ( wannier_thr <= 0 ) &
          CALL errore( ' input_read ', ' wannier_thr must be positive ', 1 )
      IF ( iphase /= 1 ) &
          CALL errore( ' input_read ', ' iphase must be 1 (ONE) ', 1 )
      IF ( niter0 < 0 ) &
          CALL errore( ' input_read ', ' niter0 must be non negative ', 1 )
      IF ( niter <= 0 ) &
          CALL errore( ' input_read ', ' niter must be positive ', 1 )
      IF ( alphafix0 <= 0.0d0 ) &
          CALL errore( ' input_read ', ' alphafix0 must be positive ', 1 )
      IF ( alphafix <= 0.0d0 ) &
          CALL errore( ' input_read ', ' alphafix must be positive ', 1 )
      IF ( nprint <= 0 ) &
          CALL errore( ' input_read ', ' nprint must be positive ', ABS(nprint)+1 )
      IF ( nshells <= 0 ) &
          CALL errore( ' input_read ', ' nshells must be greater than 0 ', 1 )
      IF ( ANY( nwhich( 1:nshells ) <= 0 ) ) &
          CALL errore( ' input_read ', ' values for nwhich must be greater than 0 ', 1 )
      IF ( itrial < 1 .OR. itrial > 3 ) &
          CALL errore( ' input_read ', ' itrial out of range ', 1 )

      DO i = 1, LEN_TRIM( ordering_type )
          ordering_type( i : i ) = capital( ordering_type( i : i ) )
      ENDDO
      IF ( TRIM(ordering_type) /= 'NONE'   .AND. TRIM(ordering_type) /= 'SPATIAL' .AND. &
           TRIM(ordering_type) /= 'SPREAD' .AND. TRIM(ordering_type) /= 'COMPLETE' ) &
            CALL errore( ' input_read ', ' invalid ORDERING_TYPE = '//TRIM(ordering_type),1)

      DO i = 1, LEN_TRIM( verbosity )
          verbosity( i : i ) = capital( verbosity( i : i ) )
      ENDDO
      IF ( TRIM(verbosity) /= 'NONE'   .AND. TRIM(verbosity) /= 'LOW' .AND. &
           TRIM(verbosity) /= 'MEDIUM' .AND. TRIM(verbosity) /= 'HIGH' ) &
           CALL errore( ' input_read ', ' invalid verbosity = '//TRIM(verbosity),1)

      CALL trial_center_data_allocate()

      !
      !
 100  CALL read_line( input_line, end_of_file=tend )
      !
      IF( tend ) GO TO 120
      IF( input_line == ' ' .OR. input_line(1:1) == '#' ) GO TO 100
      !
      READ (input_line, *) card
       
      DO i = 1, LEN_TRIM( input_line )
         input_line( i : i ) = capital( input_line( i : i ) )
      ENDDO
       
      !
      IF ( TRIM(card) == 'WANNIER_CENTER' ) THEN
         !
         CALL card_wannier_center( input_line, dimwann, trial )
         !
      ELSE
         !
         IF ( ionode ) &
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

      alloc = .TRUE.
    RETURN
  END SUBROUTINE input_read


!**********************************************************
   SUBROUTINE card_wannier_center( input_line, dimwann, list )
   !**********************************************************
      USE parser_module, ONLY: read_line, matches
      USE trial_center_module
      IMPLICIT NONE
      !
      CHARACTER(LEN=256)             :: input_line
      INTEGER,            INTENT(in) :: dimwann
      TYPE(trial_center), INTENT(out):: list(dimwann)

      LOGICAL, SAVE      :: tread = .FALSE.
      INTEGER            :: i, iwann, ierr
      CHARACTER(LEN=10)  :: adum, units
      !
      !
      IF ( tread ) CALL errore( ' card_wannier_center  ', ' two occurrence ', 2 )


      IF ( matches('CRYSTAL', input_line ) ) THEN
          units = 'crystal'
      ELSEIF ( matches('BOHR', input_line ) ) THEN
          units = 'bohr'
      ELSEIF ( matches('ANGSTROM', input_line ) ) THEN
          units = 'angstrom'
      ELSE
          IF ( TRIM( ADJUSTL( input_line ) ) /= 'WANNIER_CENTER' ) THEN
               CALL errore( ' read_cards ', &
                          & ' unknow unit option for WANNIER_CENTER: '&
                          & //input_line, 1 )
          ENDIF
          units = 'crystal'
      ENDIF


      !
      ! through the trial centers
      !
      DO iwann = 1, dimwann

           !
           ! ... init center
           CALL trial_center_init(list(iwann))
           list(iwann)%units = TRIM(units)

           CALL read_line( input_line )
           READ(input_line,*) list(iwann)%type 

           !
           ! ... chose the center type
           SELECT CASE ( TRIM(list(iwann)%type) )
           CASE ( "1gauss" )
               READ(input_line,*, IOSTAT=ierr) adum, list(iwann)%x1(1:3), &
                     list(iwann)%l, list(iwann)%m, list(iwann)%ndir, list(iwann)%decay
               IF (ierr/=0) CALL errore('card_wannier_center','reading line',ABS(ierr))

           CASE ( "2gauss" ) 
               !
               ! gaussians has explicitly s symmetry
               !
               list(iwann)%l = 0
               list(iwann)%m = 0
               list(iwann)%ndir = 3
               READ(input_line,*, IOSTAT=ierr) adum, &
                     list(iwann)%x1(1:3), list(iwann)%x2(1:3), list(iwann)%decay
               IF (ierr/=0) CALL errore('card_wannier_center','reading line',ABS(ierr))

           CASE ( "atomic" )
               CALL errore('card_wannier_center','ATOMIC trial type not implem',iwann)
           CASE DEFAULT
               CALL errore('card_wannier_center','Wrong wannier center type',iwann)
           END SELECT
           list(iwann)%alloc = .TRUE.

       ENDDO
  END SUBROUTINE card_wannier_center

END MODULE input_module
