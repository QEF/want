! 
! Copyright (C) 2004 Andrea Ferretti
! 
! This file is distributed under the terms of the 
! GNU General Public License. See the file `License' 
! in the root directory of the present distribution, 
! or http://www.gnu.org/copyleft/gpl.txt . 
! 

!*********************************************
   MODULE want_init_module
!*********************************************

CONTAINS

!*********************************************************
SUBROUTINE want_init(want_input, windows, bshells)
   !*********************************************************
   USE kinds
   USE constants,  ONLY : CZERO, RYD
   USE parameters, ONLY : nstrx
   USE timing_module, ONLY : timing
   USE io_module,  ONLY : stdout, dft_unit, ioname
   USE files_module, ONLY : file_open, file_close
   USE iotk_module
   USE parser_base_module, ONLY : change_case
   USE input_module,    ONLY : wannier_center_init, input_alloc => alloc, assume_ncpp
   USE lattice_module,  ONLY : lattice_read_ext, lattice_init, alat, avec, bvec
   USE ions_module,  ONLY : ions_read_ext, ions_init
   USE windows_module,  ONLY : windows_read_ext, windows_init, eig
   USE kpoints_module,  ONLY : nk, s, nkpts, vkpt, &
                               kpoints_read_ext, bshells_init
   IMPLICIT NONE

   LOGICAL, OPTIONAL, INTENT(in) :: want_input
   LOGICAL, OPTIONAL, INTENT(in) :: windows
   LOGICAL, OPTIONAL, INTENT(in) :: bshells

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
! * init kpoints data    (including bshells if required)
!
! </INFO>

   CHARACTER(9)              :: subname="want_init"
   CHARACTER(nstrx)          :: filename 
   CHARACTER(nstrx)          :: attr
   CHARACTER(nstrx)          :: string
   LOGICAL                   :: lfound
   LOGICAL                   :: want_input_
   LOGICAL                   :: windows_
   LOGICAL                   :: bshells_
   INTEGER                   :: ierr, ik, idum
   

! ... end of declarations

   CALL timing('want_init',OPR='start')

!   
! ... setting up   
!   
    want_input_ = .FALSE.
    IF ( PRESENT(want_input) ) want_input_ = want_input
    windows_ = .FALSE.
    IF ( PRESENT(windows) ) windows_ = windows
    bshells_ = .FALSE.
    IF ( PRESENT(bshells) ) bshells_ = bshells


!
! ... opening the file containing the PW-DFT data
!
    CALL ioname('export',filename,LPOSTFIX=.FALSE.)
    CALL file_open(dft_unit,TRIM(filename),PATH="/",ACTION="read", &
                             FORM='formatted')

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
    IF ( want_input_ ) CALL wannier_center_init( avec )


!
! ... read ions data
!
    CALL ions_read_ext(dft_unit, "Atoms", lfound)
    IF ( .NOT. lfound ) CALL errore(subname,'Tag Atoms not found',2)
    CALL ions_init()

!
! ... read kpoints data
!
    CALL kpoints_read_ext(dft_unit, "Kmesh", lfound)
    IF ( .NOT. lfound ) CALL errore(subname,'Tag '//'Kmesh'//' not found',2)
    CALL get_monkpack(nk,s,nkpts,vkpt,'CRYSTAL',bvec,ierr)
    IF ( ierr /= 0) CALL errore(subname,'kpt grid not Monkhorst-Pack',ABS(ierr))
    !
    ! ...  allocations and initializations
    IF ( bshells_ .AND. .NOT. input_alloc ) &
          CALL errore(subname,'Input NOT read while doing bshells',3)
    IF ( bshells_ ) CALL bshells_init( )

!
! ... eigenvalues data read
!
    IF ( windows_ ) THEN
        CALL windows_read_ext(dft_unit,'Eigenvalues',lfound)
           IF ( .NOT. lfound ) CALL errore('want_init','Unable to find Eigenvalues',6)
        !
        ! ... init windows
        CALL windows_init( eig(:,:) )
    ENDIF

!
! ... closing the main data file 
!
   CALL file_close(dft_unit,PATH="/",ACTION="read")

   CALL ioname('dft_data',filename,LPATH=.FALSE.,LPOSTFIX=.FALSE.)
   WRITE( stdout,"(/,'  PW-DFT data read from file: ',a)") TRIM(filename)   
    

!
! ... read pseudopotentials (according to Espresso fmts)
!     use ASSUME_NCPP = .TRUE. to skip this section (meaningful only if all PPs are NCPP)
!
   IF ( .NOT. assume_ncpp ) THEN
      CALL readpp()
   ENDIF


   CALL timing('want_init',OPR='stop')

END SUBROUTINE want_init
END MODULE want_init_module

