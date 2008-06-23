! 
! Copyright (C) 2008 WanT Group
!
! This file is distributed under the terms of the
! GNU General Public License. See the file `License'
! in the root directory of the present distribution,
! or http://www.gnu.org/copyleft/gpl.txt .
!
!********************************************************
   SUBROUTINE postproc_init( )
   !********************************************************
   !
   ! This is  a driver routine to manage almost the whole
   ! I/O during postprocessing operation
   !
   USE parameters,           ONLY : nstrx
   USE io_module,            ONLY : stdout, io_name, ham_unit, space_unit
   USE files_module,         ONLY : file_open, file_close
   USE windows_module,       ONLY : windows_read
   USE subspace_module,      ONLY : subspace_read
   USE hamiltonian_module,   ONLY : hamiltonian_read
   USE timing_module,        ONLY : timing
   USE log_module,           ONLY : log_push, log_pop
   USE want_interfaces_module
   !
   IMPLICIT NONE 

   !
   ! local variables
   !
   CHARACTER(nstrx)   :: filename
   LOGICAL            :: lfound

!
!------------------------------
! main body
!------------------------------
!
      CALL timing( 'postproc_init', OPR='start' )
      CALL log_push( 'postproc_init' )

      !
      ! ... Getting previous WanT data
      !
      CALL want_dftread ( WINDOWS=.FALSE., LATTICE=.TRUE., IONS=.TRUE., KPOINTS=.TRUE.  )
      CALL want_init    ( INPUT=.FALSE.,   WINDOWS=.FALSE., BSHELLS=.FALSE. )

      !
      ! Read Subspace data
      !
      CALL io_name('space',filename)
      !
      CALL file_open(space_unit,TRIM(filename),PATH="/",ACTION="read")
          !
          CALL windows_read(space_unit,"WINDOWS",lfound)
          IF ( .NOT. lfound ) CALL errore('bands',"unable to find WINDOWS",1)
          !
          CALL subspace_read(space_unit,"SUBSPACE",lfound)
          IF ( .NOT. lfound ) CALL errore('bands',"unable to find SUBSPACE",1)
          !
      CALL file_close(space_unit,PATH="/",ACTION="read")
      !
      CALL io_name('space',filename,LPATH=.FALSE.)
      WRITE( stdout,"(/,2x,'Space data read from file: ',a)") TRIM(filename)

      !
      ! Read hamiltonian data
      !
      CALL io_name('hamiltonian',filename)
      !
      CALL file_open(ham_unit,TRIM(filename),PATH="/",ACTION="read")
          !
          CALL hamiltonian_read(ham_unit,"HAMILTONIAN",lfound)
          IF ( .NOT. lfound ) CALL errore('bands',"unable to find HAMILTONIAN",1)
          !
      CALL file_close(ham_unit,PATH="/",ACTION="read")
      !
      CALL io_name('hamiltonian',filename,LPATH=.FALSE.)
      WRITE( stdout,"(2x,'Hamiltonian data read from file: ',a,/)") TRIM(filename)


      !
      ! exit the routine
      !
      CALL timing( 'postproc_init', OPR='stop' )
      CALL log_pop( 'postproc_init' )
      !
END SUBROUTINE postproc_init

