
! <INFO>
!*********************************************
   MODULE startup_module
!*********************************************
   USE io_global, ONLY : stdout
   USE timing_module, ONLY : nclockx, timing, timing_allocate
   IMPLICIT NONE
   PRIVATE

! This module contains the routine STARTUP
! that initilizes the code
!
! routines in this module:
! SUBROUTINE startup(version)
! </INFO>


   PUBLIC :: startup


CONTAINS 
   
!
! Subroutines
!

!**********************************************************
   SUBROUTINE startup(version,main_name)
!**********************************************************
      IMPLICIT NONE
      CHARACTER(*), INTENT(in) :: version
      CHARACTER(*), INTENT(in) :: main_name
      CHARACTER(9)             :: cdate, ctime
      
      !
      ! initilize clocks and timing
      !
      CALL timing_allocate(nclockx)
      CALL timing(TRIM(main_name),OPR="start")
      CALL timing("global",OPR="start")
      !
      ! description
      ! 
      CALL date_and_tim(cdate,ctime)
      WRITE( stdout, * ) ' ======================================================================'
      WRITE( stdout, * ) '             =                                            ='            
      WRITE( stdout, * ) '             =     *** WanT *** Wannier Transport Code    ='   
      WRITE( stdout, * ) '             =        (www.wannier_transport.org)         ='
      WRITE( stdout, * ) '             =       Norm Conserv. Pseudopot. Impl.       ='
      WRITE( stdout, * ) '             =                                            ='            
      WRITE( stdout, * ) ' ======================================================================'
      WRITE( stdout, * ) ' '
      WRITE( stdout, * ) ' '
      WRITE(stdout, FMT='(2x,"Program <",a,">  v. ",A5,"  starts ..." )') &
                   TRIM(main_name),version(5:9) 
      WRITE(stdout, FMT='(2x,"Date ",A9," at ",A9,/ )') cdate, ctime


   END SUBROUTINE startup

END MODULE startup_module
