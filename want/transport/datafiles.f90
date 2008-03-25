!
! Copyright (C) 2008 WanT Group
!
! This file is distributed under the terms of the
! GNU General Public License. See the file `License\'
! in the root directory of the present distribution,
! or http://www.gnu.org/copyleft/gpl.txt .
!
!*********************************************
   MODULE T_datafiles_module
!*********************************************
!
! This module is intended to check the formal of 
! the provided datafiles and to internally convert them if needed.
!
   USE parameters,       ONLY : nstrx
   USE io_module,        ONLY : ionode, ionode_id, stdout, aux_unit
   USE mp,               ONLY : mp_bcast
   USE timing_module,    ONLY : timing
   USE log_module,       ONLY : log_push, log_pop
   USE T_control_module, ONLY : datafile_L, datafile_C, datafile_R, calculation_type
   USE crystal_io_module
   USE iotk_module
   !
   IMPLICIT NONE
   PRIVATE 


   PUBLIC :: datafiles_init
   PUBLIC :: datafiles_check_fmt


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
   ! to the interal xml format.
   ! In this last case, an '.xml' extension will be appended
   !
   IMPLICIT NONE

   !
   ! local variables
   !
   CHARACTER(14)          :: subname="datafiles_init"
   CHARACTER(nstrx)       :: fmtstr
   !
   INTEGER                :: i, nfile
   LOGICAL                :: exists
   CHARACTER(nstrx)       :: filelist(3), filename

   !
   !-------------------
   ! main body
   !-------------------
   !
   CALL timing( subname, OPR='start')
   CALL log_push( subname )

   !
   ! determine the files to be checked
   !
   nfile=1
   filelist(1) = TRIM( datafile_C )
   !
   IF ( TRIM(calculation_type) == 'conductor' ) THEN
       !
       nfile=3
       !
       filelist(2) = TRIM( datafile_L )
       filelist(3) = TRIM( datafile_R )
       !
   ENDIF
       

   !
   ! loop over the files and check them
   !
   file_loop:&
   DO i = 1, nfile
       !
       filename = filelist( i )

       !
       ! check whether the file exists
       !
       IF ( ionode ) THEN
           !
           INQUIRE( FILE=filename, EXIST=exists ) 
       ENDIF
       !
       CALL mp_bcast( exists, ionode_id ) 
       !
       IF (.NOT. exists ) CALL errore(subname, 'unable to find '//TRIM(filename),1 )

       !
       ! convert the file if the case
       !
       IF ( ionode ) THEN
           !
           CALL datafiles_check_fmt( filename, fmtstr )
           !
           IF ( TRIM( fmtstr) == 'crystal' ) THEN
               !
               CALL crystal_to_internal( filename, TRIM(filename)//'.xml' )
               !
               WRITE( stdout, "(2x, A,' converted to internal fmt' )") &
                   TRIM( filename )
               !
               filelist(i) = TRIM(filelist(i))//'.xml' 
               !
           ENDIF
           !
       ENDIF
       !
       CALL mp_bcast( fmtstr,      ionode_id )
       CALL mp_bcast( filelist(i), ionode_id )
       !
       IF ( LEN_TRIM(fmtstr) == 0 ) CALL errore(subname, 'no input fmt detected', 71)
       !
   ENDDO file_loop

   !
   ! update the name of the files
   !
   datafile_C = TRIM( filelist(1) )
   !
   IF ( TRIM(calculation_type) == 'conductor' ) THEN
       !
       datafile_L = TRIM( filelist(2) )
       datafile_R = TRIM( filelist(3) )
       !
   ENDIF


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
   ! 
   ! an empty string is returned when no knwon fmt is found
   !
   IMPLICIT NONE
     !
     CHARACTER(*), INTENT(IN)  :: filename 
     CHARACTER(*), INTENT(OUT) :: fmtstr
     !
     fmtstr=' '
     !
     IF ( file_is_internal( filename) ) THEN 
        !
        fmtstr = 'internal'
        RETURN
        !
     ENDIF
     !
     IF ( file_is_crystal( filename) ) THEN 
        !
        fmtstr = 'crystal'
        RETURN
        !
     ENDIF
     !
     !
   END SUBROUTINE datafiles_check_fmt


!**********************************************************
   LOGICAL FUNCTION file_is_internal( filename )
   !**********************************************************
   !
   ! check for internal fmt
   ! 
   CHARACTER(*) :: filename 
   !
   LOGICAL   :: lerror, lopnd
   INTEGER   :: ierr
     !
     file_is_internal = .FALSE.
     lerror = .FALSE.
     !
     CALL iotk_open_read( aux_unit, TRIM(filename), IERR=ierr )
     IF ( ierr /= 0 ) lerror = .TRUE.
     !
     CALL iotk_scan_begin( aux_unit, "HAMILTONIAN", IERR=ierr )
     IF ( ierr /= 0 ) lerror = .TRUE.
     !
     CALL iotk_scan_end( aux_unit, "HAMILTONIAN", IERR=ierr )
     IF ( ierr /= 0 ) lerror = .TRUE.
     !
     CALL iotk_close_read( aux_unit, IERR=ierr ) 
     IF ( ierr /= 0 ) lerror = .TRUE.
     !
     !
     IF ( lerror ) THEN
         !
         INQUIRE( aux_unit, OPENED=lopnd )
         IF( lopnd ) CLOSE( aux_unit )
         !
         RETURN
         !
     ENDIF
     !
     file_is_internal = .TRUE.
     !
  END FUNCTION file_is_internal
  !
  !
!**********************************************************
   LOGICAL FUNCTION file_is_crystal( filename )
   !**********************************************************
   !
   ! check for crystal fmt
   ! 
   CHARACTER(*)     :: filename 
   !
   INTEGER          :: ierr
   LOGICAL          :: lerror, lopnd
   CHARACTER(nstrx) :: prog
     !
     file_is_crystal = .FALSE.
     lerror = .FALSE.
     !
     CALL crio_open_file( aux_unit, FILENAME=TRIM(filename), ACTION='read', IERR=ierr )
     IF ( ierr/= 0 ) lerror = .TRUE.
     !
     CALL crio_read_header( CREATOR_NAME=prog, IERR=ierr)
     IF ( ierr/= 0 ) lerror = .TRUE.
     !
     IF ( TRIM(prog) /= "CRYSTAL06" ) lerror = .TRUE.
     !
     CALL crio_close_file( ACTION='read', IERR=ierr )
     IF ( ierr/= 0 ) lerror = .TRUE.

     IF ( lerror ) THEN
         !
         INQUIRE( aux_unit, OPENED=lopnd )
         IF( lopnd ) CLOSE( aux_unit )
         !
         RETURN
         !
     ENDIF
     !
     file_is_crystal = .TRUE.
     !
  END FUNCTION file_is_crystal

END MODULE T_datafiles_module

