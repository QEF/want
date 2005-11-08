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
SUBROUTINE want_init(want_input, windows, kpoints, bshells, pseudo)
   !*********************************************************
   USE kinds
   USE constants,  ONLY : CZERO, RYD
   USE parameters, ONLY : nstrx
   USE timing_module, ONLY : timing
   USE io_module,  ONLY : stdout, dft_unit, ioname
   USE files_module, ONLY : file_open, file_close
   USE iotk_module
   USE parser_base_module, ONLY : change_case

   USE control_module, ONLY : use_uspp
   USE trial_center_module, ONLY : trial_center_convert
   USE trial_center_data_module, ONLY : trial, dimwann
   USE lattice_module,  ONLY : lattice_read_ext, lattice_init, alat, avec
   USE ions_module,  ONLY : ions_read_ext, ions_init, tau
   USE windows_module,  ONLY : windows_read_ext, windows_init, eig, nspin, spin_component
   USE kpoints_module,  ONLY : nkpts, nkpts_tot, iks, ike, &
                               kpoints_read_ext, kpoints_init
   USE dft_interface_module, ONLY : dft_interface_read_spin
   USE us_module,   ONLY : okvan
   USE uspp_param,  ONLY : tvanp
   USE ions_module, ONLY : uspp_calculation
   IMPLICIT NONE

   LOGICAL, OPTIONAL, INTENT(in) :: want_input
   LOGICAL, OPTIONAL, INTENT(in) :: windows
   LOGICAL, OPTIONAL, INTENT(in) :: kpoints
   LOGICAL, OPTIONAL, INTENT(in) :: bshells
   LOGICAL, OPTIONAL, INTENT(in) :: pseudo

! <INFO>
! This subroutine performs all the allocations and 
! initializations required in the WanT code.
! Input data are assumed to be already read.
! The logical flag in input manage the tasks to be performed.
!
! Interface:
! SUBROUTINE want_init()
!
! Tasks performed:
! * init lattice data
! * init want input data (if required by WANT_INPUT = .TRUE.)
! * init windows data    (if required by WINDOWS = .TRUE.)
! * init ions data    
! * init kpoints data    (if required by KPOINTS = .TRUE.)
! * init bshells data    (if required by BSHELLS = .TRUE.)
! * init pseudo data     (if required by PSEUDO=.TRUE.)
!
! </INFO>

   CHARACTER(9)              :: subname="want_init"
   CHARACTER(nstrx)          :: filename 
   LOGICAL                   :: lfound
   LOGICAL                   :: want_input_, windows_, kpoints_, bshells_, pseudo_
   INTEGER                   :: ia, iwann
   

! ... end of declarations

   CALL timing('want_init',OPR='start')

!   
! ... setting up   
!   
    want_input_ = .FALSE.
    IF ( PRESENT(want_input) ) want_input_ = want_input
    windows_ = .TRUE.
    IF ( PRESENT(windows) ) windows_ = windows
    kpoints_ = .TRUE.
    IF ( PRESENT(kpoints) ) kpoints_ = kpoints
    bshells_ = kpoints_
    IF ( PRESENT(bshells) ) bshells_ = bshells
    pseudo_ = .FALSE.
    IF ( PRESENT(pseudo) ) pseudo_ = pseudo

    IF ( bshells_ .AND. .NOT. kpoints_ ) CALL errore(subname,'bshells need kpoints',1)

!
! ... opening the file containing the PW-DFT data
!
    CALL ioname('export',filename,LPOSTFIX=.FALSE.)
    CALL file_open(dft_unit,TRIM(filename),PATH="/",ACTION="read", &
                             FORM='formatted')


!
! ... managing the spin components
!
    CALL dft_interface_read_spin(dft_unit,nkpts_tot,nspin)
    !
    IF ( nspin == 1 ) THEN
        nkpts = nkpts_tot
        iks = 1
        ike = nkpts
        IF ( TRIM(spin_component) /= 'none' ) & 
             CALL errore(subname,'Invalid spin component = '//TRIM(spin_component),1 )
    ELSE
        !
        ! this is because the Espresso convention which double the kpt
        ! number instead of adding a second spin component when nspin == 2
        !
        nkpts = nkpts_tot / 2 
        SELECT CASE ( TRIM(spin_component) )
        CASE ( 'up' )
            iks = 1
            ike = nkpts
        CASE ( 'down' )
            iks = nkpts+1
            ike = 2*nkpts
        CASE DEFAULT
            CALL errore(subname,'Invalid spin component = '//TRIM(spin_component),2 )
        END SELECT
    ENDIF
    

!
! ... read lattice data
!
    CALL lattice_read_ext(dft_unit,"Cell",lfound)
    IF ( .NOT. lfound ) CALL errore(subname,'Tag Cell not found',1)
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


!
! ... read ions data
!
    CALL ions_read_ext(dft_unit, "Atoms", lfound)
    IF ( .NOT. lfound ) CALL errore(subname,'Tag Atoms not found',2)
    CALL ions_init()
    !
    ! set the centers in the atomic wfc if the case
    !
    DO iwann = 1, dimwann
         IF ( TRIM(trial(iwann)%type) == 'atomic' ) THEN
               ia = trial(iwann)%iatom
               trial(iwann)%x1(:) = tau(:, ia ) * alat
         ENDIF
    ENDDO


!
! ... read kpoints data
!
    IF ( kpoints_ ) THEN
        CALL kpoints_read_ext(dft_unit, "Kmesh", lfound)
        IF ( .NOT. lfound ) CALL errore(subname,'Tag '//'Kmesh'//' not found',2)
        CALL kpoints_init()
    ENDIF

    !
    ! ... b-vectors initializations
    IF ( bshells_ ) THEN 
        CALL bshells_init( )
    ENDIF

!
! ... eigenvalues data read
!
    IF ( windows_ ) THEN
        CALL windows_read_ext(dft_unit,'Eigenvalues',lfound)
           IF ( .NOT. lfound ) CALL errore('want_init','Unable to find Eigenvalues',6)
        !
        ! ... init windows
        CALL windows_init( eig(:,:), dimwann )
    ENDIF

!
! ... closing the main data file 
!
   CALL file_close(dft_unit,PATH="/",ACTION="read")

   CALL ioname('export',filename,LPATH=.FALSE.,LPOSTFIX=.FALSE.)
   WRITE( stdout,"(/,'  PW-DFT data read from file: ',a)") TRIM(filename)   
    

!
! ... read pseudopotentials (according to Espresso fmts)
!     use ASSUME_NCPP = .TRUE. to skip this section (meaningful only if all PPs are NCPP)
!
   IF ( pseudo_ ) THEN
      CALL readpp()
      okvan = ANY( tvanp(:) )
      uspp_calculation = okvan
      use_uspp = okvan
   ENDIF


   CALL timing('want_init',OPR='stop')

END SUBROUTINE want_init


END MODULE want_init_module
