
! <INFO>
!*********************************************
   MODULE timing_module
!*********************************************
   IMPLICIT NONE
   PRIVATE
   SAVE

! This module contains the definition of CLOCK type
! and handles the timing all over the code
! 
! routines in this module:
! SUBROUTINE  timing(name[,opr])
! SUBROUTINE  timing_allocate()
! SUBROUTINE  timing_deallocate()
! SUBROUTINE  timing_overview(unit)
! SUBROUTINE  timing_upto_now(unit,name)
! SUBROUTINE  clock_start(obj,name)
! SUBROUTINE  clock_stop(obj)
! SUBROUTINE  clock_update(obj)
! SUBROUTINE  clock_find(unit,found,name)
! </INFO>
!
 
   INTEGER, PARAMETER             :: nclockx = 500
   INTEGER, PARAMETER             :: str_len = 200

   TYPE clock
      CHARACTER(str_len)          :: name              ! clock name
      INTEGER                     :: call_number       ! number of runs for this clock
      INTEGER                     :: start             ! last start
      INTEGER                     :: stop              ! last stop 
      INTEGER                     :: rate              ! rate
      REAL                        :: total_time        ! total time up to now
      LOGICAL                     :: running           ! true if clock is counting
      LOGICAL                     :: alloc = .FALSE.
   END TYPE clock
      
   INTEGER                        :: nclock            ! actual number of clocks
   INTEGER                        :: nclock_max        ! max number of clocks
   TYPE(clock), POINTER           :: clocks(:)
     

!
! end of declarations
!

   PUBLIC ::  nclockx
   PUBLIC ::  clock, clocks
   PUBLIC ::  timing
   PUBLIC ::  timing_allocate
   PUBLIC ::  timing_deallocate
   PUBLIC ::  timing_overview
   PUBLIC ::  timing_upto_now


CONTAINS

!
! Subroutines
!   

!**********************************************************
   SUBROUTINE timing(name,opr)
   !**********************************************************
      IMPLICIT NONE
      CHARACTER(*),           INTENT(in)    :: name
      CHARACTER(*), OPTIONAL, INTENT(in)    :: opr
      CHARACTER(5)                          :: opr_
      LOGICAL                               :: found
      INTEGER                               :: index

      IF ( LEN( TRIM(name)) == 0 )  CALL errore('timing','Invalid name',1)
      opr_ = " "
      IF ( PRESENT(opr) ) opr_ = TRIM(opr)

      CALL clock_find(name,found,index)
      !
      ! clock NOT found
      !
      IF ( .NOT. found ) THEN
         IF ( .NOT. PRESENT(opr) .OR. TRIM(opr_) == "start" )  THEN
            opr_ = "start"
         ELSE 
            CALL errore('timing','Clock NOT found for operation '//TRIM(opr_)//' in '&
                        //TRIM(name),1)
         ENDIF
      ELSE
      !
      ! clock found
      !
         IF ( clocks(index)%running )  THEN
            IF ( PRESENT(opr) .AND. TRIM(opr_) /= "stop" )  &
               CALL errore('timing','Operation '//TRIM(opr_)//' NOT permitted in '&
                           //TRIM(name),1)
            opr_ = "stop"
         ELSE
            IF ( .NOT. PRESENT(opr) )  opr_ = "start"
         ENDIF
            
      ENDIF


      ! 
      ! case selection
      ! 
      SELECT CASE ( TRIM(opr_) )  
      CASE("start") 
         CALL clock_start(clocks(index),NAME=TRIM(name)) 
      CASE("stop")
         CALL clock_stop(clocks(index) ) 
      CASE DEFAULT
         CALL errore('timing','Invalid operation '//TRIM(opr_),1)
      END SELECT

   END SUBROUTINE timing


!**********************************************************
   SUBROUTINE timing_allocate(nclock_max_)
   !**********************************************************
      IMPLICIT NONE
      INTEGER,             INTENT(in)  :: nclock_max_
      INTEGER                          :: ierr
 
      IF ( nclock_max_ < 1 ) CALL errore('timing_allocate','Invalid NCLOCK_MAX',1)
      IF ( ASSOCIATED(clocks) ) CALL errore('timing_allocate','Clocks already allocated',1)

      ALLOCATE( clocks(nclock_max_), STAT=ierr )
      IF ( ierr /= 0 ) CALL errore('timing_allocate','Unable to allocate TIMING',1)

      nclock = 0
      nclock_max = nclock_max_
   END SUBROUTINE timing_allocate


!**********************************************************
   SUBROUTINE timing_deallocate()
   !**********************************************************
      IMPLICIT NONE
      IF ( .NOT. ASSOCIATED(clocks) ) &
          CALL errore('timing_allocate','Clocks NOT allocated',1)
      DEALLOCATE( clocks )
      nclock = 0
      nclock_max = 0
   END SUBROUTINE timing_deallocate
   

!**********************************************************
   SUBROUTINE clock_allocate(name,obj)
   !**********************************************************
      IMPLICIT NONE
      CHARACTER(*),          INTENT(in)    :: name
      TYPE(clock),           INTENT(inout) :: obj    

      IF ( obj%alloc ) CALL errore('clock_allocate','Clock already allocated',1)
      IF ( LEN( TRIM(name)) == 0 )  CALL errore('clock_allocate','Invalid name',1)
      IF ( nclock == nclock_max )    &
             CALL errore('clock_allocate','Max number of clocks reached',nclock)

      nclock = nclock + 1
      obj%name=TRIM(name)
      obj%call_number=0
      obj%start=0
      obj%stop=0
      obj%rate=0
      obj%total_time=0.0
      obj%running=.FALSE.
      obj%alloc=.TRUE.
   
   END SUBROUTINE clock_allocate


!**********************************************************
   SUBROUTINE clock_find(name,found,index)
   !**********************************************************
      IMPLICIT NONE
      CHARACTER(*),          INTENT(in)    :: name
      LOGICAL,               INTENT(out)   :: found
      INTEGER,               INTENT(out)   :: index
      INTEGER                              :: i

      IF ( LEN( TRIM(name)) == 0 )  CALL errore('clock_find','Invalid name',1)
      
      found = .false.
      index = 0
      
      DO i=1,nclock
          IF ( TRIM(clocks(i)%name) == TRIM(name) .AND. clocks(i)%alloc ) THEN 
                 index = i
                 found = .TRUE.
                 EXIT
          ENDIF
      ENDDO

      !
      ! clock not found, pointing to next available clock
      !
      IF ( .NOT. found ) index = nclock + 1

   END SUBROUTINE clock_find


!**********************************************************
   SUBROUTINE clock_start(obj,name)
   !**********************************************************
      IMPLICIT NONE
      TYPE(clock),            INTENT(inout) :: obj    
      CHARACTER(*), OPTIONAL, INTENT(in)    :: name

      IF ( PRESENT(name) ) THEN
         IF ( .NOT. obj%alloc  ) CALL clock_allocate(TRIM(name),obj)
      ELSE
         IF ( .NOT. obj%alloc  ) CALL errore('clock_start','NAME should be present',1)
      ENDIF
      
      CALL SYSTEM_CLOCK( COUNT=obj%start, COUNT_RATE=obj%rate )
      obj%running = .TRUE.
      obj%call_number = obj%call_number + 1
      
   END SUBROUTINE clock_start
   

!**********************************************************
   SUBROUTINE clock_stop(obj)
   !**********************************************************
      IMPLICIT NONE
      TYPE(clock),           INTENT(inout) :: obj    
      INTEGER                              :: rate

      IF ( .NOT. obj%alloc  )   CALL errore('clock_stop','Clock NOT allocated',1)
      IF ( .NOT. obj%running  ) CALL errore('clock_stop','Clock NOT running',1)
      
      CALL SYSTEM_CLOCK( COUNT=obj%stop, COUNT_RATE=rate )
      IF ( rate /= obj%rate )  CALL errore('clock_stop','Invalid rate',1)
      obj%total_time = obj%total_time + REAL( obj%stop - obj%start ) / REAL( obj%rate )
      obj%running = .FALSE.
      
   END SUBROUTINE clock_stop


!**********************************************************
   SUBROUTINE clock_update(obj)
   !**********************************************************
      IMPLICIT NONE
      TYPE(clock),           INTENT(inout) :: obj    

      IF ( obj%running ) THEN 
          CALL clock_stop(obj) 
          CALL clock_start(obj,obj%name) 
          obj%call_number = obj%call_number -1 
      ENDIF
   END SUBROUTINE clock_update


!**********************************************************
   SUBROUTINE clock_write(unit,obj,form)
   !**********************************************************
      IMPLICIT NONE
      INTEGER,                INTENT(in) :: unit
      TYPE(clock),         INTENT(inout) :: obj    
      CHARACTER(*), OPTIONAL, INTENT(in) :: form
      CHARACTER(3)                       :: form_
      INTEGER                            :: nhour,nmin
      REAL                               :: nsec

      form_="sec"
      IF ( PRESENT(form) ) form_ = TRIM(form)
      CALL clock_update(obj)

      SELECT CASE ( TRIM(form_) ) 
      CASE ( "hms" )
         nhour = obj%total_time / 3600
         nmin = (obj%total_time-3600 * nhour) / 60
         nsec = (obj%total_time-3600 * nhour) - 60 * nmin
         IF ( obj%call_number == 1 )  THEN
            IF (nhour > 0) THEN
               WRITE (unit, '(5x,a20," : ",3x,i2,"h",i2,"m CPU "/)') &
                              TRIM(obj%name), nhour, nmin
            ELSEIF (nmin > 0) THEN
               WRITE (unit, '(5x,a20," : ",i2,"m",f5.2,"s CPU "/)') &
                    TRIM(obj%name), nmin, nsec
            ELSE
               WRITE (unit, '(5x,a20," : ",3x,f5.2,"s CPU "/)') &
                    TRIM(obj%name), nsec
            ENDIF
         ELSE
            IF (nhour > 0) THEN
               WRITE(unit,'(5x,a20," : ",3x,i2,"h",i2,"m CPU (", &
                          &  i8," calls,",f8.3," s avg)")') TRIM(obj%name), nhour, nmin, &
                              obj%call_number , obj%total_time / REAL( obj%call_number )
            ELSEIF (nmin > 0) THEN
               WRITE (unit, '(5x,a20," : ",i2,"m",f5.2,"s CPU (", &
                          &    i8," calls,",f8.3," s avg)")') TRIM(obj%name), nmin, nsec, &
                              obj%call_number , obj%total_time / REAL( obj%call_number )
            ELSE
               WRITE (unit, '(5x,a20," : ",3x,f5.2,"s CPU (", &
                          &    i8," calls,",f8.3," s avg)")') TRIM(obj%name), nsec, &
                              obj%call_number , obj%total_time / REAL( obj%call_number )
            ENDIF
         ENDIF

      CASE ( "sec" )
         !
         ! time in seconds
         !
         IF ( obj%call_number == 1) THEN
            WRITE (unit, '(5x,a20," :",f9.2,"s CPU")') TRIM(obj%name) , obj%total_time
         ELSE
            WRITE (unit, '(5x,a20," :",f9.2,"s CPU (", i8," calls,",f8.3," s avg)")')  &
                  TRIM(obj%name) , obj%total_time , obj%call_number ,   &
                  obj%total_time / REAL( obj%call_number )
         ENDIF
      CASE DEFAULT
         CALL errore('clock_write','Invalid FORM '//TRIM(form_),1 )
      END SELECT

   END SUBROUTINE clock_write        


!**********************************************************
   SUBROUTINE timing_upto_now(unit,name)
   !**********************************************************
      IMPLICIT NONE
      INTEGER,                INTENT(in) :: unit
      CHARACTER(*),           INTENT(in) :: name
      LOGICAL                            :: found
      INTEGER                            :: index

      CALL clock_find(name,found,index)
      IF ( .NOT. found ) CALL errore('timing_upto_now','Clock '//TRIM(name)//' not found',1)
      CALL clock_update(clocks(index))
      WRITE(unit,"(30x,'Total time spent up to now :',F9.2,' secs',/)") &
            clocks(index)%total_time

  END SUBROUTINE timing_upto_now    


!**********************************************************
   SUBROUTINE timing_overview(unit,main_name)
   !**********************************************************
      IMPLICIT NONE
      INTEGER,                INTENT(in) :: unit
      CHARACTER(*),           INTENT(in) :: main_name
      CHARACTER(20)                      :: form
      INTEGER                            :: i

      INQUIRE(UNIT=unit,UNFORMATTED=form)
      IF ( TRIM(form) ==  "YES" ) &
           CALL errore('Timing_overview','UNIT unformatted',1)
      WRITE(unit,"(2x,a)") "Timing overview"
      IF ( nclock == 0 ) THEN
         WRITE(unit,"(7x,'No clock to display',/)") 
         RETURN
      ENDIF

      WRITE(unit,"(13x,'clock number : ',i5,/)") nclock
      DO i=1,nclock 
         IF ( TRIM(clocks(i)%name) == TRIM(main_name) .OR. &
              clocks(i)%total_time >= 1000           ) THEN 
            CALL clock_write(unit,clocks(i),FORM="hms")
         ELSE
            CALL clock_write(unit,clocks(i),FORM="hms")
         ENDIF
      ENDDO
      WRITE(unit,"(/)")
         
   END SUBROUTINE timing_overview


END MODULE timing_module

