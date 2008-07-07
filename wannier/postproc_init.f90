! 
! Copyright (C) 2008 WanT Group
!
! This file is distributed under the terms of the
! GNU General Public License. See the file `License'
! in the root directory of the present distribution,
! or http://www.gnu.org/copyleft/gpl.txt .
!
!**********************************************************
   MODULE postproc_init_module
   !**********************************************************
   !   
   IMPLICIT NONE
   PRIVATE
   !   
   PUBLIC :: postproc_init
   !   
CONTAINS
!
!********************************************************
   SUBROUTINE postproc_init( windows, subspace, hamiltonian, wannier)
   !********************************************************
   !
   ! This is  a driver routine to manage almost the whole
   ! I/O during postprocessing operation
   !
   USE parameters,           ONLY : nstrx
   USE io_module,            ONLY : stdout, io_name, ham_unit, space_unit, wan_unit
   USE files_module,         ONLY : file_open, file_close
   USE windows_module,       ONLY : windows_read
   USE subspace_module,      ONLY : subspace_read
   USE hamiltonian_module,   ONLY : hamiltonian_read
   USE localization_module,  ONLY : localization_read
   USE timing_module,        ONLY : timing
   USE log_module,           ONLY : log_push, log_pop
   USE want_dftread_module,  ONLY : want_dftread
   USE want_init_module,     ONLY : want_init
   !
   IMPLICIT NONE 

   !
   ! input variables
   !
   LOGICAL, OPTIONAL, INTENT(IN) :: windows
   LOGICAL, OPTIONAL, INTENT(IN) :: subspace
   LOGICAL, OPTIONAL, INTENT(IN) :: hamiltonian
   LOGICAL, OPTIONAL, INTENT(IN) :: wannier

   !
   ! local variables
   !
   CHARACTER(13)      :: subname = 'postproc_init'
   CHARACTER(nstrx)   :: filename
   LOGICAL            :: lfound
   !
   LOGICAL            :: lwindows
   LOGICAL            :: lsubspace
   LOGICAL            :: lhamiltonian
   LOGICAL            :: lwannier

!
!------------------------------
! main body
!------------------------------
!
      CALL timing( subname, OPR='start' )
      CALL log_push( subname )

      !
      ! define defaults
      ! wannier is set to false mainly because of the
      ! interface to CRYSTAL06
      !
      lwindows        = .TRUE.
      lsubspace       = .TRUE.
      lhamiltonian    = .TRUE.
      lwannier        = .FALSE.
      !
      IF ( PRESENT( windows ) )           lwindows = windows
      IF ( PRESENT( subspace ) )         lsubspace = subspace
      IF ( PRESENT( hamiltonian ) )   lhamiltonian = hamiltonian
      IF ( PRESENT( wannier ) )           lwannier = wannier
      

      !
      ! ... Getting previous WanT data
      !
      CALL want_dftread ( WINDOWS=.FALSE., LATTICE=.TRUE., IONS=.TRUE., KPOINTS=.TRUE.  )
      CALL want_init    ( INPUT=.FALSE.,   WINDOWS=.FALSE., BSHELLS=.FALSE. )


      !
      ! Read windows data
      !
      IF ( lwindows ) THEN
          !
          CALL io_name('space',filename)
          !
          CALL file_open(space_unit,TRIM(filename),PATH="/",ACTION="read")
              !
              CALL windows_read(space_unit,"WINDOWS",lfound)
              IF ( .NOT. lfound ) CALL errore(subname,"unable to find WINDOWS",1)
              !
              CALL subspace_read(space_unit,"SUBSPACE",lfound)
              IF ( .NOT. lfound ) CALL errore(subname,"unable to find SUBSPACE",1)
              !
          CALL file_close(space_unit,PATH="/",ACTION="read")
          !
          CALL io_name('space',filename,LPATH=.FALSE.)
          WRITE( stdout,"(/,2x,'Windows data read from file: ',a)") TRIM(filename)
          !
      ENDIF

      !
      ! Read subspace data
      !
      IF ( lsubspace ) THEN
          !
          CALL io_name('space',filename)
          !
          CALL file_open(space_unit,TRIM(filename),PATH="/",ACTION="read")
              !
              CALL subspace_read(space_unit,"SUBSPACE",lfound)
              IF ( .NOT. lfound ) CALL errore(subname,"unable to find SUBSPACE",1)
              !
          CALL file_close(space_unit,PATH="/",ACTION="read")
          !
          CALL io_name('space',filename,LPATH=.FALSE.)
          WRITE( stdout,"(/,2x,'Subspace data read from file: ',a)") TRIM(filename)
          !
      ENDIF

      !
      ! Read hamiltonian data
      !
      IF ( lhamiltonian ) THEN
          !
          CALL io_name('hamiltonian',filename)
          !
          CALL file_open(ham_unit,TRIM(filename),PATH="/",ACTION="read")
              !
              CALL hamiltonian_read(ham_unit,"HAMILTONIAN",lfound)
              IF ( .NOT. lfound ) CALL errore(subname,"unable to find HAMILTONIAN",1)
              !
          CALL file_close(ham_unit,PATH="/",ACTION="read")
          !
          CALL io_name('hamiltonian',filename,LPATH=.FALSE.)
          WRITE( stdout,"(2x,'Hamiltonian data read from file: ',a,/)") TRIM(filename)
          !
      ENDIF

      !
      ! Read wannier data
      !
      IF ( lwannier ) THEN
          !
          CALL io_name('wannier',filename)
          !
          CALL file_open(wan_unit,TRIM(filename),PATH="/",ACTION="read")
              !
              CALL localization_read(wan_unit,"WANNIER_LOCALIZATION",lfound)
              IF ( .NOT. lfound ) CALL errore(subname,"unable to find WANNIER_LOCALIZATION",1)
              !
          CALL file_close(wan_unit,PATH="/",ACTION="read")
          !
          CALL io_name('wannier',filename,LPATH=.FALSE.)
          WRITE( stdout,"(2x,'Wannier data read from file: ',a,/)") TRIM(filename)
          !
      ENDIF

      !
      ! exit the routine
      !
      CALL timing( subname, OPR='stop' )
      CALL log_pop( subname )
      !
END SUBROUTINE postproc_init

END MODULE postproc_init_module

