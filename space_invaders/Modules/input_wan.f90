!=----------------------------------------------------------------------------=!
MODULE input_wannier
!=----------------------------------------------------------------------------=!

  USE kinds
  IMPLICIT NONE
  SAVE

  INTEGER, PARAMETER :: nshx = 100  ! maximum value for nshells

  REAL(dbl) :: win_min, win_max     ! outer energy window
  REAL(dbl) :: froz_min, froz_max   ! inner energy window
  INTEGER   :: dimwann             ! number of Wannier functions
 
  REAL(dbl) :: alpha
  INTEGER :: maxiter, itrial

  INTEGER :: iphase
  REAL(dbl)  :: alphafix0, alphafix
  INTEGER :: niter, niter0, ncg

  INTEGER :: nshells
  INTEGER :: nwhich( nshx )

  namelist / input_wan / win_min, win_max, froz_min, froz_max, dimwann, &
    alpha, maxiter, itrial, iphase, alphafix0, alphafix, niter, niter0, ncg, &
    nshells, nwhich

  REAL(dbl), ALLOCATABLE :: rphiimx1(:,:)
  REAL(dbl), ALLOCATABLE :: rphiimx2(:,:)
  REAL(dbl), ALLOCATABLE :: rloc(:)
  INTEGER, ALLOCATABLE :: gauss_typ(:)
  INTEGER, ALLOCATABLE :: l_wann(:)
  INTEGER, ALLOCATABLE :: m_wann(:)
  INTEGER, ALLOCATABLE :: ndir_wann(:)


!=----------------------------------------------------------------------------=!
CONTAINS
!=----------------------------------------------------------------------------=!


  SUBROUTINE read_input

       USE io_global, ONLY: ionode, ionode_id
       USE mp, ONLY: mp_bcast
       USE parser, ONLY: read_line

       !  win_min, win_max are the eigenvalues window bounds (in eV)
       !  froz_min, froz_max are the frozen eigenvalues window bounds (in eV)
       !  dimwann is the minimal dimension of the window

       INTEGER :: ios
       INTEGER :: nwann
       INTEGER :: i

       CHARACTER(LEN=256) :: input_line
       CHARACTER(LEN=80)  :: card
       CHARACTER(LEN=1), EXTERNAL :: capital
       LOGICAL            :: tend
       !

       win_min = 0.0d0
       win_max = 0.0d0
       froz_min = -1.1d10 
       froz_max = -1.0d10
       alpha = 0.5d0
       maxiter = 1000
       iphase  = 1
       niter0 = 500
       alphafix0 = 0.5d0
       niter = 500  
       alphafix = 0.5d0
       ncg = 3
       dimwann = 1
       nshells = 1
       nwhich = 1 
       itrial = 3

       ios = 0
       IF( ionode ) THEN
          READ( 5, input_wan, iostat = ios )
       END IF
       CALL mp_bcast( ios, ionode_id )
       IF( ios /= 0 ) THEN
          CALL errore( ' read_input ', &
                     & ' reading namelist input_wan ', ABS(ios) )
       END IF

       IF ( win_max <= win_min ) THEN
         CALL errore( ' read_input ', ' win_max is not larger than win_min ', 1 )
       END IF
       IF ( froz_max <= froz_min ) THEN
         CALL errore( ' read_input ', ' froz_max is not larger than froz_min ', 1 )
       END IF
       IF ( dimwann <= 0 ) THEN
         CALL errore( ' read_input ', ' dimwann too small ', 1 )
       END IF
       IF ( alpha <= 0.0d0 ) THEN
         CALL errore( ' read_input ', ' alpha must be positive ', 1 )
       END IF
       IF ( maxiter <= 0 ) THEN
         CALL errore( ' read_input ', ' maxiter must be positive ', 1 )
       END IF
       IF ( iphase /= 1 ) THEN
         CALL errore( ' read_input ', ' iphase must be 1 (ONE) ', 1 )
       END IF
       IF ( niter0 <= 0 ) THEN
         CALL errore( ' read_input ', ' niter0 must be positive ', 1 )
       END IF
       IF ( niter <= 0 ) THEN
         CALL errore( ' read_input ', ' niter must be positive ', 1 )
       END IF
       IF ( alphafix0 <= 0.0d0 ) THEN
         CALL errore( ' read_input ', ' alphafix0 must be positive ', 1 )
       END IF
       IF ( alphafix <= 0.0d0 ) THEN
         CALL errore( ' read_input ', ' alphafix must be positive ', 1 )
       END IF
       IF ( nshells <= 0 ) THEN
         CALL errore( ' read_input ', ' nshells must be greater than 0 ', 1 )
       END IF
       IF ( ANY( nwhich( 1:nshells ) <= 0 ) ) THEN
         CALL errore( ' read_input ', ' values for nwhich must be greater than 0 ', 1 )
       END IF
       IF ( itrial < 1 .OR. itrial > 3 ) THEN
         CALL errore( ' read_input ', ' itrial out of range ', 1 )
       END IF

       ALLOCATE( gauss_typ(dimwann) )
       ALLOCATE( rphiimx1(3,dimwann) )
       ALLOCATE( rphiimx2(3,dimwann) )
       ALLOCATE( l_wann(dimwann) )
       ALLOCATE( m_wann(dimwann) )
       ALLOCATE( ndir_wann(dimwann) )
       ALLOCATE( rloc(dimwann) )
       !
       !
 100   CALL read_line( input_line, end_of_file=tend )
       !
       IF( tend ) GO TO 120
       IF( input_line == ' ' .OR. input_line(1:1) == '#' ) GO TO 100
       !
       READ (input_line, *) card
       !
       !DO i = 1, LEN_TRIM( input_line )
       !   input_line( i : i ) = capital( input_line( i : i ) )
       !END DO
       !
       !
       IF ( TRIM(card) == 'WANNIER_CENTER' ) THEN
          !
          CALL card_wannier_center( input_line )
          !
       ELSE
          !
          IF ( ionode ) &
             WRITE( 6,'(A)') 'Warning: card '//TRIM(input_line)//' ignored'
          !
       END IF
       !
       ! ...     END OF LOOP ... !
       !
       GOTO 100
       !
120    CONTINUE
       !

     RETURN
  END SUBROUTINE

!=----------------------------------------------------------------------------=!

  SUBROUTINE card_wannier_center( input_line )

    USE parser, ONLY: read_line

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

             rphiimx2(1,nwann)=0.0d0
             rphiimx2(2,nwann)=0.0d0
             rphiimx2(3,nwann)=0.0d0

           ELSE IF ( gauss_typ(nwann) == 2 ) THEN
             READ(input_line,*) idum, ( rphiimx1(i,nwann), i=1,3 ), &
               ( rphiimx2(i,nwann), i=1,3 ), rloc(nwann)

           ELSE
             WRITE(*,*) 'ERROR in trial Wannier centers: wrong gauss_typ'
             STOP
           END IF
         END DO
       ELSE
         gauss_typ = 0
         rphiimx1 = 0.0d0
         rphiimx2 = 0.0d0
         l_wann = 0
         m_wann = 0
         ndir_wann = 0
         rloc = 0.0d0
       END IF

     RETURN
  END SUBROUTINE

!=----------------------------------------------------------------------------=!

  SUBROUTINE deallocate_input

    IF( ALLOCATED( gauss_typ ) )   DEALLOCATE( gauss_typ )
    IF( ALLOCATED( rphiimx1 ) )   DEALLOCATE( rphiimx1 )
    IF( ALLOCATED( rphiimx2 ) )   DEALLOCATE( rphiimx2 )
    IF( ALLOCATED( l_wann ) )   DEALLOCATE( l_wann )
    IF( ALLOCATED( m_wann ) )   DEALLOCATE( m_wann )
    IF( ALLOCATED( ndir_wann ) )   DEALLOCATE( ndir_wann )
    IF( ALLOCATED( rloc ) )   DEALLOCATE( rloc )

    RETURN
  END SUBROUTINE deallocate_input

!=----------------------------------------------------------------------------=!
END MODULE
!=----------------------------------------------------------------------------=!
