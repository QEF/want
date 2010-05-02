!
! Copyright (C) 2008 WanT Group
!
! This file is distributed under the terms of the
! GNU General Public License. See the file `License\'
! in the root directory of the present distribution,
! or http://www.gnu.org/copyleft/gpl.txt .
!
!*********************************************
   MODULE datafiles_module
   !*********************************************
   !
   ! This module is intended to check the formal of 
   ! the provided datafiles and to internally convert them if needed.
   !
   USE parameters,              ONLY : nstrx
   USE io_module,               ONLY : ionode, ionode_id, stdout, aux_unit
   USE io_module,               ONLY : work_dir, prefix, postfix, datafile => dftdata_file
   USE io_module,               ONLY : file_is_internal => io_file_is_internal
   USE mp,                      ONLY : mp_bcast
   USE timing_module,           ONLY : timing
   USE log_module,              ONLY : log_push, log_pop
   USE crystal_tools_module,    ONLY : crystal_to_internal, file_is_crystal
   USE wannier90_tools_module,  ONLY : wannier90_to_internal, file_is_wannier90
   USE iotk_module
   !
   IMPLICIT NONE
   PRIVATE 


   PUBLIC :: datafiles_init
   PUBLIC :: datafiles_check_fmt
   PUBLIC :: file_is_internal


CONTAINS

!
! subroutines
!

!**********************************************************
   SUBROUTINE datafiles_init()
   !**********************************************************
   !
   ! First, check whether files exist, 
   ! then determine their format and eventually convert them
   ! to the internal WanT format.
   !
   IMPLICIT NONE

   !
   ! local variables
   !
   CHARACTER(14)    :: subname="datafiles_init"
   CHARACTER(nstrx) :: fmtstr
   CHARACTER(nstrx) :: filein, fileout
   LOGICAL          :: exists

!
!-------------------
! main body
!-------------------
!
   
   !
   ! if the name of the file is empty, exit the routine
   !
   IF ( LEN_TRIM(datafile) == 0 ) RETURN

   !
   CALL timing( subname, OPR='start')
   CALL log_push( subname )

   !
   ! loop over the files and check them
   !
   filein = TRIM(datafile)

   !
   ! check whether the file exists
   !
   IF ( ionode ) THEN
       !
       INQUIRE( FILE=filein, EXIST=exists ) 
       !
   ENDIF
   !
   CALL mp_bcast( exists, ionode_id ) 
   ! 
   IF (.NOT. exists ) CALL errore(subname, 'unable to find '//TRIM(filein),1 )

   !
   ! convert the file if the case
   !
   IF ( ionode ) THEN
       !
       CALL datafiles_check_fmt( filein, fmtstr )
       !
       SELECT CASE( TRIM(fmtstr) )
       CASE ( 'crystal' )
           !
           fileout = TRIM(work_dir)//'/'//TRIM(prefix)//TRIM(postfix)//'.ham'
           CALL crystal_to_internal( filein, fileout, 'hamiltonian' )
           !
           fileout = TRIM(work_dir)//'/'//TRIM(prefix)//TRIM(postfix)//'.space'
           CALL crystal_to_internal( filein, fileout, 'subspace' )
           !
           WRITE( stdout, "(2x, A,' converted to internal fmt' )") TRIM( filein )
           !
       CASE ( 'wannier90' )
           !
           fileout = TRIM(work_dir)//'/'//TRIM(prefix)//TRIM(postfix)//'.ham'
           CALL wannier90_to_internal( TRIM(filein), TRIM(fileout), 'hamiltonian' )
           !
           fileout = TRIM(work_dir)//'/'//TRIM(prefix)//TRIM(postfix)//'.space'
           CALL wannier90_to_internal( filein, fileout, 'subspace' )
           !
           WRITE( stdout, "(2x, A,' converted to internal fmt' )") TRIM( filein )
           !
       CASE ( 'internal' )
           !
           ! nothing to do
           WRITE( stdout, "(2x, A,' used in internal fmt' )") TRIM( filein )
           !
       CASE DEFAULT
           CALL errore(subname,'invalid FMT = '//TRIM(fmtstr),10 )
       END SELECT
       !
   ENDIF
   !
   CALL mp_bcast( fmtstr,      ionode_id )
   !
   IF ( LEN_TRIM(fmtstr) == 0 ) CALL errore(subname, 'no input fmt detected', 71)


   CALL log_pop( subname )
   CALL timing( subname, OPR='stop')
   !
END SUBROUTINE datafiles_init


!**********************************************************
   SUBROUTINE datafiles_check_fmt( filename, fmtstr )
   !**********************************************************
   !
   ! determine the fmt of the datafile provided in input
   ! possible fmts are:
   !
   ! * internal
   ! * crystal
   ! * wannier90
   ! 
   ! an empty string is returned when no known fmt is found
   !
   IMPLICIT NONE
     !
     CHARACTER(*), INTENT(IN)  :: filename 
     CHARACTER(*), INTENT(OUT) :: fmtstr
     !
     fmtstr=' '
     !
     IF ( file_is_internal( filename ) ) THEN 
         !
         fmtstr = 'internal'
         RETURN
         !
     ENDIF
     !
     IF ( file_is_crystal( filename ) ) THEN 
         !
         fmtstr = 'crystal'
         RETURN
         !
     ENDIF
     !
     IF ( file_is_wannier90( filename ) ) THEN 
         !
         fmtstr = 'wannier90'
         RETURN
         !
     ENDIF
     !
     RETURN
   END SUBROUTINE datafiles_check_fmt

END MODULE datafiles_module

