! 
! Copyright (C) 2004 WanT Group
! 
! This file is distributed under the terms of the 
! GNU General Public License. See the file `License' 
! in the root directory of the present distribution, 
! or http://www.gnu.org/copyleft/gpl.txt . 
! 
!
!**********************************************************
   MODULE want_init_module
   !**********************************************************
   !   
   IMPLICIT NONE
   PRIVATE
   !   
   PUBLIC :: want_init
   !   
CONTAINS
!
!*********************************************************
SUBROUTINE want_init(input, lattice, ions, windows, kpoints, bshells, pseudo)
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
   ! * init want input data (if required by WANT_INPUT = .TRUE.)
   ! * init lattice data    (if required by LATTICE    = .TRUE.)
   ! * init windows data    (if required by WINDOWS    = .TRUE.)
   ! * init ions data       (if required by IONS       = .TRUE.)
   ! * init kpoints data    (if required by KPOINTS    = .TRUE.)
   ! * init bshells data    (if required by BSHELLS    = .TRUE.)
   ! * init pseudo data     (if required by PSEUDO     = .TRUE.)
   !
   !
   USE kinds
   USE constants,                ONLY : ZERO, EPS_m6
   USE parameters,               ONLY : nstrx
   USE timing_module,            ONLY : timing
   USE log_module,               ONLY : log_push, log_pop
   !
   USE io_module,                ONLY : io_init, io_alloc => alloc
   USE control_module,           ONLY : do_polarization
   USE trial_center_module,      ONLY : trial_center_convert
   USE trial_center_data_module, ONLY : trial, dimwann
   USE lattice_module,           ONLY : lattice_init, alat, avec
   USE ions_module,              ONLY : ions_init, tau, nat, zv, ityp, ion_charge
   USE windows_module,           ONLY : windows_init, eig
   USE kpoints_module,           ONLY : kpoints_init
   ! 
   IMPLICIT NONE

   !
   ! input variables
   !
   LOGICAL, OPTIONAL, INTENT(in) :: input
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
   LOGICAL                   :: input_, init_lattice_, init_ions_, init_windows_, &
                                        init_kpoints_, init_bshells_, init_pseudo_
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
    CALL log_push('want_init')

!   
! setting up   
!   
    input_           = .FALSE.
    init_lattice_    = .TRUE.
    init_ions_       = .TRUE.
    init_kpoints_    = .TRUE.
    init_windows_    = .TRUE.
    init_bshells_    =  init_kpoints_
    init_pseudo_     = .FALSE.
    IF ( PRESENT(input) )           input_ = input
    IF ( PRESENT(lattice) )  init_lattice_ = lattice
    IF ( PRESENT(ions) )        init_ions_ = ions
    IF ( PRESENT(windows) )  init_windows_ = windows
    IF ( PRESENT(kpoints) )  init_kpoints_ = kpoints
    IF ( PRESENT(bshells) )  init_bshells_ = bshells
    IF ( PRESENT(pseudo) )    init_pseudo_ = pseudo 

    IF ( init_ions_    .AND. .NOT. init_lattice_ ) CALL errore(subname,'ions needs lattice',1)
    IF ( init_bshells_ .AND. .NOT. init_kpoints_ ) CALL errore(subname,'bshells needs kpoints',1)


!
! ... if the case init IO
!
    IF ( .NOT. io_alloc ) CALL io_init ( )


!
! ... lattice data
!
    IF ( init_lattice_ ) THEN
        !
        CALL lattice_init()
        !
        ! ... want input if required
        IF ( input_ ) THEN 
            !
            DO iwann=1,dimwann
                CALL trial_center_convert( avec, trial(iwann) )
            ENDDO 
            !
        ENDIF
    ENDIF


!
! ... ion data
!
    IF ( init_ions_ ) THEN
        !
        ! tau are read in bohr and converted here to alat
        tau(:,:) = tau(:,:) / alat
        !
        CALL ions_init()
        !
        ! set the centers in the atomic wfc if the case
        !
        IF ( input_ ) THEN
            !
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
! ... kpoint data
!
    IF ( init_kpoints_ ) THEN
        !
        CALL kpoints_init()
        !
    ENDIF

    !
    ! b-vectors initializations
    !
    IF ( init_bshells_ ) THEN 
        !
        CALL bshells_init( )
        !
    ENDIF


!
! ... eigenvalue data
!
    IF ( init_windows_ ) THEN
        !
        CALL windows_init( eig(:,:), dimwann )
        !
    ENDIF


!
! ... pseudopotentials
!
   IF ( init_pseudo_  ) THEN
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
    IF ( init_windows_ .AND. init_pseudo_ ) THEN
         IF ( ABS(ion_charge - REAL(2*dimwann,dbl)) < EPS_m6 ) &
              do_polarization = .TRUE.
    ENDIF


    CALL timing('want_init',OPR='stop')
    CALL log_pop('want_init')

END SUBROUTINE want_init

END MODULE want_init_module
