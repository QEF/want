! 
! Copyright (C) 2004 Arrigo Calzolari, Carlo Cavazzoni, Marco Buongiorno Nardelli 
! 
! This file is distributed under the terms of the 
! GNU General Public License. See the file `License' 
! in the root directory of the present distribution, 
! or http://www.gnu.org/copyleft/gpl.txt . 
! 
!=----------------------------------------------------------------------------=!
MODULE input_module
!=----------------------------------------------------------------------------=!

  USE kinds
  USE constants, ONLY : ZERO
  USE parameters, ONLY : nshx      
  USE io_module, ONLY : prefix, postfix, work_dir, title
  IMPLICIT NONE
  SAVE


  REAL(dbl) :: win_min, win_max     ! outer energy window
  REAL(dbl) :: froz_min, froz_max   ! inner energy window
  INTEGER   :: dimwann              ! number of Wannier functions
 
  REAL(dbl) :: alpha
  REAL(dbl) :: disentangle_thr      ! disentangle threshold for convergence
  REAL(dbl) :: wannier_thr          ! wannier threshold 
  INTEGER :: maxiter, itrial
  INTEGER :: nprint                 ! each nprint iterations write to stdout

  INTEGER :: iphase
  REAL(dbl)  :: alphafix0, alphafix
  INTEGER :: niter, niter0, ncg
  CHARACTER(10) :: ordering_type    ! should be 'spatial', 'complete', 'none' 
  CHARACTER(6)  :: verbosity        ! should be 'none', 'low', 'medium', 'high' 

  INTEGER :: nshells
  INTEGER :: nwhich( nshx )

  namelist / input_wan / win_min, win_max, froz_min, froz_max, dimwann, &
    alpha, maxiter, disentangle_thr, wannier_thr, itrial, iphase, alphafix0, &
    alphafix, niter, niter0, ncg, nshells, nwhich, ordering_type, verbosity, &
    nprint, prefix, postfix, work_dir, title
  
  CHARACTER(15)          :: wannier_center_units
  REAL(dbl), ALLOCATABLE :: rphiimx1(:,:)
  REAL(dbl), ALLOCATABLE :: rphiimx2(:,:)
  REAL(dbl), ALLOCATABLE :: rloc(:)
  INTEGER, ALLOCATABLE :: gauss_typ(:)
  INTEGER, ALLOCATABLE :: l_wann(:)
  INTEGER, ALLOCATABLE :: m_wann(:)
  INTEGER, ALLOCATABLE :: ndir_wann(:)

  LOGICAL              :: alloc = .FALSE.

!=----------------------------------------------------------------------------=!
CONTAINS
!=----------------------------------------------------------------------------=!


  SUBROUTINE input_read

       USE io_module, ONLY: stdout, ionode, ionode_id
       USE mp, ONLY: mp_bcast
       USE parser_module, ONLY: read_line, capital

       !  win_min, win_max are the eigenvalues window bounds (in eV)
       !  froz_min, froz_max are the frozen eigenvalues window bounds (in eV)
       !  dimwann is the minimal dimension of the window

       INTEGER :: ios
       INTEGER :: nwann
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

       ios = 0
       IF( ionode ) THEN
          READ( 5, input_wan, iostat = ios )
       END IF
       CALL mp_bcast( ios, ionode_id )
       IF( ios /= 0 ) THEN
          CALL errore( ' input_read ', &
                     & ' reading namelist input_wan ', ABS(ios) )
       END IF

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
       END DO
       IF ( TRIM(ordering_type) /= 'NONE'   .AND. TRIM(ordering_type) /= 'SPATIAL' .AND. &
            TRIM(ordering_type) /= 'SPREAD' .AND. TRIM(ordering_type) /= 'COMPLETE' ) &
            CALL errore( ' input_read ', ' invalid ORDERING_TYPE = '//TRIM(ordering_type),1)

       DO i = 1, LEN_TRIM( verbosity )
          verbosity( i : i ) = capital( verbosity( i : i ) )
       END DO
       IF ( TRIM(verbosity) /= 'NONE'   .AND. TRIM(verbosity) /= 'LOW' .AND. &
            TRIM(verbosity) /= 'MEDIUM' .AND. TRIM(verbosity) /= 'HIGH' ) &
            CALL errore( ' input_read ', ' invalid verbosity = '//TRIM(verbosity),1)

       ALLOCATE( gauss_typ(dimwann), STAT=ierr )
          IF ( ierr/=0 ) CALL errore( ' input_read ', ' allocating gauss_typ ', dimwann )
       ALLOCATE( rphiimx1(3,dimwann), STAT=ierr )
          IF ( ierr/=0 ) CALL errore( ' input_read ', ' allocating rphiimx1 ', 3*dimwann )
       ALLOCATE( rphiimx2(3,dimwann), STAT=ierr )
          IF ( ierr/=0 ) CALL errore( ' input_read ', ' allocating rphiimx2 ', 3*dimwann )
       ALLOCATE( l_wann(dimwann), STAT=ierr )
          IF ( ierr/=0 ) CALL errore( ' input_read ', ' allocating l_wann ', dimwann )
       ALLOCATE( m_wann(dimwann), STAT=ierr )
          IF ( ierr/=0 ) CALL errore( ' input_read ', ' allocating m_wann ', dimwann )
       ALLOCATE( ndir_wann(dimwann), STAT=ierr )
          IF ( ierr/=0 ) CALL errore( ' input_read ', ' allocating ndir_wann ', dimwann )
       ALLOCATE( rloc(dimwann), STAT=ierr )
          IF ( ierr/=0 ) CALL errore( ' input_read ', ' allocating rloc ', dimwann )
       !
       !
 100   CALL read_line( input_line, end_of_file=tend )
       !
       IF( tend ) GO TO 120
       IF( input_line == ' ' .OR. input_line(1:1) == '#' ) GO TO 100
       !
       READ (input_line, *) card
       
       DO i = 1, LEN_TRIM( input_line )
          input_line( i : i ) = capital( input_line( i : i ) )
       END DO
       
       !
       IF ( TRIM(card) == 'WANNIER_CENTER' ) THEN
          !
          CALL card_wannier_center( input_line )
          !
       ELSE
          !
          IF ( ionode ) &
             WRITE( stdout,'(A)') 'Warning: card '//TRIM(input_line)//' ignored'
          !
       END IF
       !
       ! ...     END OF LOOP ... !
       !
       GOTO 100
       !
120    CONTINUE
       !

     alloc = .TRUE.
     RETURN
  END SUBROUTINE

!=----------------------------------------------------------------------------=!

  SUBROUTINE card_wannier_center( input_line )

    USE parser_module, ONLY: read_line, matches

    IMPLICIT NONE

       !
       CHARACTER(LEN=256) :: input_line
       LOGICAL, SAVE      :: tread = .FALSE.
       INTEGER            :: i, nwann, idum
       !
       !
       IF ( tread ) THEN
          CALL errore( ' card_wannier_center  ', ' two occurrence ', 2 )
       END IF

       IF ( matches('CRYSTAL', input_line ) ) THEN
            wannier_center_units = 'crystal'
       ELSEIF ( matches('BOHR', input_line ) ) THEN
            wannier_center_units = 'bohr'
       ELSEIF ( matches('ANGSTROM', input_line ) ) THEN
            wannier_center_units = 'angstrom'
       ELSE
          IF ( TRIM( ADJUSTL( input_line ) ) /= 'WANNIER_CENTER' ) THEN
             CALL errore( ' read_cards ', &
                        & ' unknow unit option for WANNIER_CENTER: '&
                        & //input_line, 1 )
          END IF
          wannier_center_units = 'crystal'
       END IF

       IF( itrial == 3 ) THEN
         DO nwann = 1, dimwann
           CALL read_line( input_line )
           READ(input_line,*) gauss_typ(nwann)
           IF ( gauss_typ(nwann) == 1 ) THEN
             l_wann(nwann) = 0
             m_wann(nwann) = 0
             ndir_wann(nwann) = 3
             READ(input_line,*) idum, ( rphiimx1(i,nwann), i=1,3 ), &
               l_wann(nwann), m_wann(nwann), ndir_wann(nwann), rloc(nwann)

! ...        Values below don't really matter, since rphiimx2 is not used when gauss_typ=1

             rphiimx2(1,nwann)=ZERO
             rphiimx2(2,nwann)=ZERO
             rphiimx2(3,nwann)=ZERO

           ELSE IF ( gauss_typ(nwann) == 2 ) THEN
             READ(input_line,*) idum, ( rphiimx1(i,nwann), i=1,3 ), &
               ( rphiimx2(i,nwann), i=1,3 ), rloc(nwann)

           ELSE
              CALL errore('card_wannier_center','Wrong wannier center type',nwann)
           END IF
         END DO
       ELSE
         gauss_typ = 0
         rphiimx1 = ZERO
         rphiimx2 = ZERO
         l_wann = 0
         m_wann = 0
         ndir_wann = 0
         rloc = ZERO
       END IF

     RETURN
  END SUBROUTINE

!=----------------------------------------------------------------------------=!

  SUBROUTINE wannier_center_init( alat, avec )
    USE constants, ONLY: bohr => bohr_radius_angs, ZERO
    USE converters_module, ONLY : cart2cry

    IMPLICIT NONE
    REAL(dbl), INTENT(IN) :: alat, avec(3,3)
!
! ...  Converting WANNIER centers from INPUT to CRYSTAL units
!      AVEC is in units of ALAT which is in Bohr
!
!      RLOC should be in Bohr and is converted here if is the case
!      if wannier_center_units == crystal it is supposed to be already in Bohr
!
       SELECT CASE ( TRIM(wannier_center_units) )
       CASE ( 'angstrom' )
           CALL cart2cry(rphiimx1,alat*bohr*avec(:,:),wannier_center_units)
           CALL cart2cry(rphiimx2,alat*bohr*avec(:,:),wannier_center_units)
           rloc(:) = rloc(:) / bohr  
       CASE ( 'bohr' )
           CALL cart2cry(rphiimx1,alat*avec(:,:),wannier_center_units)
           CALL cart2cry(rphiimx2,alat*avec(:,:),wannier_center_units)
       CASE ( 'crystal' )
       CASE DEFAULT
           CALL errore('wannier_center_init','Invalid wannier center units : '  &
                                 //TRIM(wannier_center_units),1 )
       END SELECT

    RETURN
  END SUBROUTINE

!=----------------------------------------------------------------------------=!

  SUBROUTINE input_deallocate
    IMPLICIT NONE
    INTEGER :: ierr

    ierr = 0
    IF( ALLOCATED( gauss_typ ) )   DEALLOCATE( gauss_typ, STAT=ierr )
        IF (ierr/=0) CALL errore('deallocate_input','deallocating gauss_type', ABS(ierr))
    IF( ALLOCATED( rphiimx1 ) )   DEALLOCATE( rphiimx1, STAT=ierr )
        IF (ierr/=0) CALL errore('deallocate_input','deallocating rphiimx1', ABS(ierr))
    IF( ALLOCATED( rphiimx2 ) )   DEALLOCATE( rphiimx2, STAT=ierr )
        IF (ierr/=0) CALL errore('deallocate_input','deallocating rphiimx2', ABS(ierr))
    IF( ALLOCATED( l_wann ) )   DEALLOCATE( l_wann, STAT=ierr )
        IF (ierr/=0) CALL errore('deallocate_input','deallocating l_wann', ABS(ierr))
    IF( ALLOCATED( m_wann ) )   DEALLOCATE( m_wann, STAT=ierr )
        IF (ierr/=0) CALL errore('deallocate_input','deallocating m_wann', ABS(ierr))
    IF( ALLOCATED( ndir_wann ) )   DEALLOCATE( ndir_wann, STAT=ierr )
        IF (ierr/=0) CALL errore('deallocate_input','deallocating ndir_wann', ABS(ierr))
    IF( ALLOCATED( rloc ) )   DEALLOCATE( rloc, STAT=ierr )
        IF (ierr/=0) CALL errore('deallocate_input','deallocating rloc', ABS(ierr))

    alloc = .FALSE.
    RETURN
  END SUBROUTINE input_deallocate

!=----------------------------------------------------------------------------=!
END MODULE input_module
!=----------------------------------------------------------------------------=!
