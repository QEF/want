! 
! Copyright (C) 2004 WanT Group
! 
! This file is distributed under the terms of the 
! GNU General Public License. See the file `License' 
! in the root directory of the present distribution, 
! or http://www.gnu.org/copyleft/gpl.txt . 
! 

MODULE want_init_module
CONTAINS

!*********************************************************
SUBROUTINE want_init(want_input, lattice, ions, windows, kpoints, bshells, pseudo)
   !*********************************************************
   ! 
   ! This subroutine performs all the allocations and 
   ! initializations required in the WanT code.
   ! Input data are assumed to be already read.
   ! The logical flag in input manage the tasks to be performed.
   !
   ! Interface:
   ! SUBROUTINE want_init()
   !
   ! Tasks performed:
   ! * init IO data         ( always )
   ! * init want input data (if required by WANT_INPUT = .TRUE.)
   ! * init lattice data    (if required by LATTICE    = .TRUE.)
   ! * init windows data    (if required by WINDOWS    = .TRUE.)
   ! * init ions data       (if required by IONS       = .TRUE.)
   ! * init kpoints data    (if required by KPOINTS    = .TRUE.)
   ! * init bshells data    (if required by BSHELLS    = .TRUE.)
   ! * init pseudo data     (if required by PSEUDO     = .TRUE.)
   !

   USE kinds
   USE constants,                ONLY : ZERO, EPS_m6
   USE parameters,               ONLY : nstrx
   USE timing_module,            ONLY : timing
   USE io_module,                ONLY : stdout, dft_unit, io_init, io_name, dftdata_fmt
   USE files_module,             ONLY : file_open, file_close
   !
   USE control_module,           ONLY : use_uspp, do_polarization
   USE trial_center_module,      ONLY : trial_center_convert
   USE trial_center_data_module, ONLY : trial, dimwann
   USE lattice_module,           ONLY : lattice_read_ext, lattice_init, alat, avec
   USE ions_module,              ONLY : ions_read_ext, ions_init, tau, nat, zv, ityp, ion_charge
   USE windows_module,           ONLY : windows_read_ext, windows_init, eig
   USE kpoints_module,           ONLY : kpoints_read_ext, kpoints_init
   USE us_module,                ONLY : okvan
   USE uspp_param,               ONLY : tvanp
   USE ions_module,              ONLY : uspp_calculation
   ! 
   IMPLICIT NONE

   !
   ! input variables
   !
   LOGICAL, OPTIONAL, INTENT(in) :: want_input
   LOGICAL, OPTIONAL, INTENT(in) :: lattice
   LOGICAL, OPTIONAL, INTENT(in) :: ions
   LOGICAL, OPTIONAL, INTENT(in) :: windows
   LOGICAL, OPTIONAL, INTENT(in) :: kpoints
   LOGICAL, OPTIONAL, INTENT(in) :: bshells
   LOGICAL, OPTIONAL, INTENT(in) :: pseudo

   !
   ! local variables
   !
   CHARACTER(9)              :: subname="want_init"
   CHARACTER(nstrx)          :: filename 
   LOGICAL                   :: want_input_, read_lattice_, read_ions_, read_windows_, &
                                             read_kpoints_, read_bshells_, read_pseudo_
   INTEGER                   :: ia, iwann
   !   
   ! end of declarations
   !    
   
!
!------------------------------
! main body
!------------------------------
!
    CALL timing('want_init',OPR='start')

!   
! setting up   
!   
    want_input_      = .FALSE.
    read_lattice_    = .TRUE.
    read_ions_       = .TRUE.
    read_kpoints_    = .TRUE.
    read_windows_    = .TRUE.
    read_bshells_    =  read_kpoints_
    read_pseudo_     = .FALSE.
    IF ( PRESENT(want_input) ) want_input_ = want_input
    IF ( PRESENT(lattice) )  read_lattice_ = lattice
    IF ( PRESENT(ions) )        read_ions_ = ions
    IF ( PRESENT(windows) )  read_windows_ = windows
    IF ( PRESENT(kpoints) )  read_kpoints_ = kpoints
    IF ( PRESENT(bshells) )  read_bshells_ = bshells
    IF ( PRESENT(pseudo) )    read_pseudo_ = pseudo 

    IF ( read_ions_    .AND. .NOT. read_lattice_ ) CALL errore(subname,'ions needs lattice',1)
    IF ( read_bshells_ .AND. .NOT. read_kpoints_ ) CALL errore(subname,'bshells needs kpoints',1)

!
! ... init IO
!
    CALL io_init()


!
! ... opening the file containing the PW-DFT data
!
    CALL io_name('dft_data',filename,LPOSTFIX=.FALSE.)
    CALL file_open(dft_unit,TRIM(filename),PATH="/",ACTION="read", &
                            FORM='formatted')

!
! ... read lattice data
!
    IF ( read_lattice_ ) THEN
        CALL lattice_read_ext(dftdata_fmt)
        !
        ! ...  allocations and initializations
        CALL lattice_init()
        !
        ! ... want_input if required
        IF ( want_input_ ) THEN 
            DO iwann=1,dimwann
                CALL trial_center_convert( avec, trial(iwann) )
            ENDDO 
        ENDIF
    ENDIF


!
! ... read ions data
!
    IF ( read_ions_ ) THEN
        !
        CALL ions_read_ext( dftdata_fmt )
        !
        ! tau are read in bohr and converted here to alat
        tau(:,:) = tau(:,:) / alat
        !
        CALL ions_init()
        !
        ! set the centers in the atomic wfc if the case
        !
        IF ( want_input_ ) THEN
            DO iwann = 1, dimwann
                !
                IF ( TRIM(trial(iwann)%type) == 'atomic' ) THEN
                     !
                     ia = trial(iwann)%iatom
                     trial(iwann)%x1(:) = tau(:, ia ) * alat
                     !
                ENDIF
                !
            ENDDO
        ENDIF
        !
    ENDIF


!
! ... read kpoints data
!
    IF ( read_kpoints_ ) THEN
        !
        CALL kpoints_read_ext( dftdata_fmt )
        !
        CALL kpoints_init()
        !
    ENDIF

    !
    ! b-vectors initializations
    !
    IF ( read_bshells_ ) THEN 
        !
        CALL bshells_init( )
        !
    ENDIF


!
! ... eigenvalues data read
!
    IF ( read_windows_ ) THEN
        !
        CALL windows_read_ext( dftdata_fmt )
        !
        ! init windows
        !
        CALL windows_init( eig(:,:), dimwann )
        !
    ENDIF


!
! ... closing the main data file 
!
   CALL file_close(dft_unit,PATH="/",ACTION="read")

   CALL io_name('dft_data',filename,LPATH=.FALSE.,LPOSTFIX=.FALSE.)
   WRITE( stdout,"(2x,'DFT-data read from file : ',a)") TRIM(filename)   
    

!
! ... read pseudopotentials (according to Espresso fmts)
!
   IF ( read_pseudo_  ) THEN
      !
      CALL readpp()
      !
      okvan = ANY( tvanp(:) )
      uspp_calculation = okvan
      use_uspp = okvan
  
      !
      ! compute ionic charge
      ion_charge = ZERO
      !
      DO ia = 1, nat 
          !
          ion_charge = ion_charge + zv( ityp(ia) ) 
          !
      ENDDO
      !
   ENDIF


!
! ... polarization
!
    do_polarization = .FALSE.
    !
    IF ( read_windows_ .AND. read_pseudo_ ) THEN
         IF ( ABS(ion_charge - REAL(2*dimwann,dbl)) < EPS_m6 ) &
              do_polarization = .TRUE.
    ENDIF


   CALL timing('want_init',OPR='stop')

END SUBROUTINE want_init


END MODULE want_init_module
