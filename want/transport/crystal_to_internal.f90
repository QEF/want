!
! Copyright (C) 2008 WanT Group
!
! This file is distributed under the terms of the
! GNU General Public License. See the file `License\'
! in the root directory of the present distribution,
! or http://www.gnu.org/copyleft/gpl.txt .
!
!**********************************************************
   SUBROUTINE crystal_to_internal( filein, fileout )
   !**********************************************************
   !
   ! Convert datafiles written by the CRYSTAL06 program to
   ! the internal representation
   !
   USE timing_module,    ONLY : timing
   USE log_module,       ONLY : log_push, log_pop
   IMPLICIT NONE
   
   !
   ! input variables
   !
   CHARACTER(*), INTENT(IN) :: filein
   CHARACTER(*), INTENT(IN) :: fileout
   
   !
   ! local variables
   !
   CHARACTER(19)      :: subname="crystal_to_internal"

!
!------------------------------
! main body
!------------------------------
!
   CALL timing( subname, OPR='start' )
   CALL log_push( subname )


   CALL log_pop( subname )
   CALL timing( subname, OPR='stop' )
   !
   RETURN
   !
END SUBROUTINE crystal_to_internal

