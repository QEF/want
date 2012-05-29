! 
! Copyright (C) 2006 WanT Group
! 
! This file is distributed under the terms of the 
! GNU General Public License. See the file `License' 
! in the root directory of the present distribution, 
! or http://www.gnu.org/copyleft/gpl.txt . 
! 
!**********************************************************
   MODULE want_dftread_module
   !**********************************************************
   !   
   IMPLICIT NONE
   PRIVATE
   !   
   PUBLIC :: want_dftread
   !   
CONTAINS
!
!*********************************************************
   SUBROUTINE want_dftread(lattice, ions, windows, symmetry, kpoints, pseudo, need_wfc)
   !*********************************************************
   ! 
   ! This subroutine performs all the allocations and 
   ! initializations required in the WanT code.
   ! Input data are assumed to be already read.
   ! The logical flag in input manage the tasks to be performed.
   !
   ! Interface:
   ! SUBROUTINE want_dftread()
   !
   ! Tasks performed:
   ! * read lattice data    (if required by LATTICE    = .TRUE.)
   ! * read windows data    (if required by WINDOWS    = .TRUE.)
   ! * read ions data       (if required by IONS       = .TRUE.)
   ! * read symmetry data   (if required by SYMMETRY   = .TRUE.)
   ! * read kpoints data    (if required by KPOINTS    = .TRUE.)
   ! * read pseudo data     (if required by PSEUDO     = .TRUE.)
   !
   !
   USE kinds
   USE parameters,               ONLY : nstrx
   USE timing_module,            ONLY : timing
   USE log_module,               ONLY : log_push, log_pop
   USE io_global_module,         ONLY : ionode
   USE io_module,                ONLY : stdout, io_name, dftdata_fmt, &
                                        io_init, io_alloc => alloc,   &
                                        io_open_dftdata, io_close_dftdata
   USE lattice_module,           ONLY : lattice_read_ext
   USE ions_module,              ONLY : ions_read_ext
   USE kpoints_module,           ONLY : kpoints_read_ext
   USE windows_module,           ONLY : windows_read_ext
   USE symmetry_module,          ONLY : symmetry_read_ext
   !
   USE control_module,           ONLY : use_uspp
   USE us_module,                ONLY : okvan
   USE uspp_param,               ONLY : upf
   USE ions_module,              ONLY : uspp_calculation
   !
#ifdef __ETSF_IO
   USE etsf_io
   USE etsf_io_tools
#endif
   ! 
   IMPLICIT NONE

   !
   ! input variables
   !
   LOGICAL, OPTIONAL, INTENT(in) :: lattice
   LOGICAL, OPTIONAL, INTENT(in) :: ions
   LOGICAL, OPTIONAL, INTENT(in) :: windows
   LOGICAL, OPTIONAL, INTENT(in) :: symmetry
   LOGICAL, OPTIONAL, INTENT(in) :: kpoints
   LOGICAL, OPTIONAL, INTENT(in) :: pseudo
   LOGICAL, OPTIONAL, INTENT(in) :: need_wfc

   !
   ! local variables
   !
   !CHARACTER(12)             :: subname='want_dftread'
   CHARACTER(nstrx)          :: filename 
   LOGICAL                   :: read_lattice_,  read_ions_, read_windows_, &
                                read_symmetry_, read_kpoints_, read_pseudo_, &
                                need_wfc_
   !   
   ! end of declarations
   !    
   
!
!------------------------------
! main body
!------------------------------
!

    !
    ! if the case, init IO
    !
    need_wfc_ = .TRUE.
    IF ( PRESENT( need_wfc) ) need_wfc_ = need_wfc
    !
    IF ( .NOT. io_alloc ) CALL io_init ( need_wfc_ ) 

    CALL timing('want_dftread',OPR='start')
    CALL log_push('want_dftread')

!   
! setting up   
!   
    read_lattice_    = .TRUE.
    read_ions_       = .TRUE.
    read_windows_    = .TRUE.
    read_symmetry_   = .FALSE.
    read_kpoints_    = .TRUE.
    read_pseudo_     = .FALSE.
    IF ( PRESENT(lattice) )   read_lattice_ = lattice
    IF ( PRESENT(ions) )         read_ions_ = ions
    IF ( PRESENT(windows) )   read_windows_ = windows
    IF ( PRESENT(symmetry) ) read_symmetry_ = symmetry
    IF ( PRESENT(kpoints) )   read_kpoints_ = kpoints
    IF ( PRESENT(pseudo) )     read_pseudo_ = pseudo 



!
! ... opening the file containing the PW-DFT data
!
     CALL io_open_dftdata( LSERIAL=.FALSE. ) 
     !
     IF ( TRIM(dftdata_fmt) == 'etsf_io' ) THEN
         read_pseudo_ = .FALSE.
     ENDIF

!
! ... read lattice data
!
    IF ( read_lattice_ ) THEN
        !
        CALL lattice_read_ext(dftdata_fmt)
        !
    ENDIF


!
! ... read ions data
!
    IF ( read_ions_ ) THEN
        !
        CALL ions_read_ext( dftdata_fmt )
        !
    ENDIF


!
! ... read symmetry data
!
    IF ( read_symmetry_ ) THEN
        !
        CALL symmetry_read_ext( dftdata_fmt )
        !
    ENDIF


!
! ... read kpoints data
!
    IF ( read_kpoints_ ) THEN
        !
        CALL kpoints_read_ext( dftdata_fmt )
        !
    ENDIF


!
! ... eigenvalues data read
!
    IF ( read_windows_ ) THEN
        !
        CALL windows_read_ext( dftdata_fmt )
        !
    ENDIF


!
! ... closing the main data file 
!
    CALL io_close_dftdata( LSERIAL=.FALSE. )
    !
    CALL io_name('dft_data',filename,LPATH=.FALSE. )
    IF (ionode) WRITE( stdout,"(2x,'DFT-data read from file: ',a)") TRIM(filename)   
    

!
! ... read pseudopotentials (according to Espresso fmts)
!
    IF ( read_pseudo_  ) THEN
        !
        CALL readpp()
        !
        okvan = ANY( upf(:)%tvanp )
        uspp_calculation = okvan
        use_uspp = okvan
        ! 
    ENDIF


    CALL timing('want_dftread',OPR='stop')
    CALL log_pop('want_dftread')

END SUBROUTINE want_dftread

END MODULE want_dftread_module

