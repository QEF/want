! 
! Copyright (C) 2004 Andrea Ferretti
! 
! This file is distributed under the terms of the 
! GNU General Public License. See the file `License' 
! in the root directory of the present distribution, 
! or http://www.gnu.org/copyleft/gpl.txt . 
! 
#include "configure.h"
!
!**********************************************************
   SUBROUTINE startup(version,main_name)
   !**********************************************************
   !
   ! This routine initializes the code.
   ! Parallel MPI initializations should also
   ! be handled by this routines
   !
   USE io_module,     ONLY : stdout, ionode, ionode_id, &
                             io_global_start, io_global_getionode
   USE timing_module, ONLY : nclockx, timing, timing_allocate
   USE mp,            ONLY : mp_start, mp_end, mp_env
   USE mp_global,     ONLY : mpime, nproc, root, group, mp_global_start
   !
   IMPLICIT NONE

   !
   ! input variables
   !
   CHARACTER(*), INTENT(in) :: version
   CHARACTER(*), INTENT(in) :: main_name

   !
   ! local variables
   !
   CHARACTER(9)             :: cdate, ctime
      
!--------------------------------------------------

   !
   ! MPI initializations
   !
   root = 0
   CALL mp_start()
   CALL mp_env(nproc,mpime,group)
   CALL mp_global_start( root, mpime, group, nproc)
   !  mpime = procesor number, starting from 0
   !  nproc = number of processors
   !  group = group index
   !  root  = index of the root processor

   !
   ! IO initializations
   !
   CALL io_global_start( mpime, root )
   CALL io_global_getionode( ionode, ionode_id )

   !
   ! initilize clocks and timing
   !
   CALL timing_allocate(nclockx)
   CALL timing(TRIM(main_name),OPR="start")
   !
   ! description
   ! 
   CALL date_and_tim(cdate,ctime)
   WRITE( stdout, "(2x,70('=') )" ) 
   WRITE( stdout, "(a)" ) '              =                                            ='
   WRITE( stdout, "(a)" ) '              =     *** WanT *** Wannier Transport Code    ='
   WRITE( stdout, "(a)" ) '              =        (www.wannier-transport.org)         ='
   WRITE( stdout, "(a)" ) '              =      Ultra Soft Pseudopotential Implem.    ='
   WRITE( stdout, "(a)" ) '              =                                            ='
   WRITE( stdout, "(2x,70('='),2/ )" ) 
   WRITE(stdout, FMT='(2x,"Program <",a,">  v. ",A5,"  starts ..." )') &
                 TRIM(main_name),version(6:10) 
   WRITE(stdout, FMT='(2x,"Date ",A9," at ",A9,/ )') cdate, ctime

   !
   ! architecture / compilation details
   !
#ifdef __MYARCHITECTURE
   WRITE(stdout, "(2x,'Arch. / Op.Sys: ',a)" ) __MYARCHITECTURE 
#endif
#ifdef __MYCOMPILER
   WRITE(stdout, "(2x,'      Compiler: ',a)" ) __MYCOMPILER
#endif
#if defined __MYARCHITECTURE || defined __MYCOMPILER
   WRITE(stdout, "()" )
#endif

END SUBROUTINE startup

