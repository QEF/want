!
! Copyright (C) 2009 WanT Group
!
! This file is distributed under the terms of the
! GNU General Public License. See the file `License\'
! in the root directory of the present distribution,
! or http://www.gnu.org/copyleft/gpl.txt .
!
!*********************************************
   MODULE E_datafiles_module
   !*********************************************
   !
   ! This module is intended to check the formal of 
   ! the provided datafiles and to internally convert them if needed.
   !
   USE parameters,              ONLY : nstrx
   USE io_module,               ONLY : ionode, ionode_id, stdout, aux_unit
   USE mp,                      ONLY : mp_bcast
   USE timing_module,           ONLY : timing
   USE log_module,              ONLY : log_push, log_pop
   USE crystal_tools_module,    ONLY : crystal_to_internal
   USE wannier90_tools_module,  ONLY : wannier90_to_internal
   USE cp2k_tools_module,       ONLY : cp2k_to_internal
   USE atmproj_tools_module,    ONLY : atmproj_to_internal
   USE datafiles_module,        ONLY : datafiles_check_fmt
   USE E_control_module,        ONLY : datafile_tot, do_orthoovp 
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
   ! to the internal WanT format.
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
   CHARACTER(nstrx)       :: filelist(1), filename

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
   filelist(1) = TRIM( datafile_tot )

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
           WRITE( stdout, "(2x, A,' file fmt: ', A )") TRIM(filename), TRIM(fmtstr)
           !
           SELECT CASE( TRIM(fmtstr) )
           CASE ( 'crystal' )
               !
               CALL crystal_to_internal( filename, TRIM(filename)//'.ham', 'hamiltonian', do_orthoovp )
               !
               WRITE( stdout, "(2x, A,' converted from CRYSTAL to internal fmt' )") &
                   TRIM( filename )
               !
               filelist(i) = TRIM(filelist(i))//'.ham' 
               !
           CASE( 'wannier90' )
               !
               CALL wannier90_to_internal( TRIM(filename), TRIM(filename)//'.ham', 'hamiltonian' )
               !
               WRITE( stdout, "(2x, A,' converted from Wannier90 to internal fmt' )") &
                   TRIM( filename )
               !
               filelist(i) = TRIM(filelist(i))//'.ham' 
               !
           CASE( 'cp2k' )
               !
               CALL cp2k_to_internal( TRIM(filename), TRIM(filename)//'.ham', 'hamiltonian', do_orthoovp )
               !
               WRITE( stdout, "(2x, A,' converted from CP2K to internal fmt' )") &
                   TRIM( filename )
               !
               filelist(i) = TRIM(filelist(i))//'.ham' 
               !
           CASE( 'atmproj' )
               !
               CALL atmproj_to_internal( TRIM(filename), TRIM(filename)//'.ham', 'hamiltonian', do_orthoovp )
               !
               WRITE( stdout, "(2x, A,' converted from ATMPROJ to internal fmt' )") &
                   TRIM( filename )
               !
               filelist(i) = TRIM(filelist(i))//'.ham' 
               !
           CASE ( 'internal' )
               !
               ! nothing to do
               !
           CASE DEFAULT
               CALL errore(subname,'invalid FMT = '//TRIM(fmtstr),10 )
           END SELECT

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
   datafile_tot = TRIM( filelist(1) )


   CALL log_pop( subname )
   CALL timing( subname, OPR='stop')
   !
END SUBROUTINE datafiles_init
  !
END MODULE E_datafiles_module

