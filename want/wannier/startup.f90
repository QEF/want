! 
! Copyright (C) 2004 WanT Group
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
   USE log_module,    ONLY : log_init
   USE mp,            ONLY : mp_start, mp_env
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
   !
   IF ( ionode ) THEN
       !
       WRITE( stdout, "(2x,70('=') )" ) 
       WRITE( stdout, "(a)" ) '              =                                            ='
       WRITE( stdout, "(a)" ) '              =     *** WanT *** Wannier Transport Code    ='
       WRITE( stdout, "(a)" ) '              =        (www.wannier-transport.org)         ='
       WRITE( stdout, "(a)" ) '              =      Ultra Soft Pseudopotential Implem.    ='
       WRITE( stdout, "(a)" ) '              =                                            ='
       WRITE( stdout, "(2x,70('='),2/ )" ) 
       !
       WRITE( stdout, FMT='(2x,"Program <",a,">  v. ",A5,"  starts ..." )') &
                      TRIM(main_name),TRIM(version) 
       WRITE( stdout, FMT='(2x,"Date ",A9," at ",A9,/ )') cdate, ctime
       !
       IF ( nproc > 1 ) THEN
           WRITE( stdout, FMT='(5x,"Parallel run, # proc: ",i4,/ )') nproc
       ELSE
           WRITE( stdout, FMT='(5x,"Serial run.",/ )')
       ENDIF
       !
   ENDIF

   !
   ! architecture / compilation details
   !
   IF ( ionode ) THEN
       !
#ifdef __HAVE_CONFIG_INFO
       !
       WRITE( stdout, "(2x,'        BUILT :',4x,a)" ) TRIM( ADJUSTL( __CONF_BUILT_DATE  ))
       WRITE( stdout, "(2x,'         HOST :',4x,a)" ) TRIM( ADJUSTL( __CONF_HOST        ))
       WRITE( stdout, "(2x,'         ARCH :',4x,a)" ) TRIM( ADJUSTL( __CONF_ARCH        ))
       WRITE( stdout, "(2x,'           CC :',4x,a)" ) TRIM( ADJUSTL( __CONF_CC          ))
       WRITE( stdout, "(2x,'          CPP :',4x,a)" ) TRIM( ADJUSTL( __CONF_CPP         ))
       WRITE( stdout, "(2x,'          F90 :',4x,a)" ) TRIM( ADJUSTL( __CONF_MPIF90      ))
       WRITE( stdout, "(2x,'          F77 :',4x,a)" ) TRIM( ADJUSTL( __CONF_F77         ))
       WRITE( stdout, "(2x,'       DFLAGS :',4x,a)" ) TRIM( ADJUSTL( __CONF_DFLAGS      ))
       WRITE( stdout, "(2x,'    BLAS LIBS :',4x,a)" ) TRIM( ADJUSTL( __CONF_BLAS_LIBS   ))
       WRITE( stdout, "(2x,'  LAPACK LIBS :',4x,a)" ) TRIM( ADJUSTL( __CONF_LAPACK_LIBS ))
       WRITE( stdout, "(2x,'     FFT LIBS :',4x,a)" ) TRIM( ADJUSTL( __CONF_FFT_LIBS    ))
       WRITE( stdout, "(2x,'    MASS LIBS :',4x,a)" ) TRIM( ADJUSTL( __CONF_MASS_LIBS   ))
#endif
       !
       WRITE( stdout, "(/)")
       !
   ENDIF

END SUBROUTINE startup

