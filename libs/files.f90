!
! Copyright (C) 2004 WanT Group
!
! This file is distributed under the terms of the
! GNU General Public License. See the file `License\'
! in the root directory of the present distribution,
! or http://www.gnu.org/copyleft/gpl.txt .
!

! <INFO>
!*********************************************
   MODULE files_module
!*********************************************
   USE parameters, ONLY: nstrx
   USE parser_module, ONLY: parser_path, change_case
   USE iotk_module
   IMPLICIT NONE
   PRIVATE

! This module contains some utilities to manage
! IO from files 
!
! routines in this module:
! SUBROUTINE  file_open(unit,filename[,path][,root][,status][,access][,recl] 
!                       [,form][,position][,action])
! SUBROUTINE  file_close(unit[,path][,action])
! SUBROUTINE  get_free_unit(unit)
! SUBROUTINE  file_delete(name)
! SUBROUTINE  file_exist(name,exist)
! SUBROUTINE  file_rename(oldname,newname)
!
! PATH is the IOTK_PATH (root NOT included) for the
! eventual XML tree. The path is a string as
! "/tag1/tag2/...../tagN " (path string)
! A VOID string indicates the ROOT path ("/")
! while PATH = "none" avoids the use of IOTK.
!
! In OPENING READ the file will be positioned inside the
! desidered folder, while in CLOSING READ the file is
! supposed to be in the specified folder. 
! OPENING and CLOSING WRITE only works from the ROOT fld.
!
! </INFO>
!


!
! end of declarations
!

   INTEGER, PARAMETER :: unitx = 100

   PUBLIC ::  file_open
   PUBLIC ::  file_close
   PUBLIC ::  file_delete
   PUBLIC ::  file_exist
   PUBLIC ::  get_free_unit

CONTAINS

!
! Subroutines
!   

!**********************************************************
   SUBROUTINE get_free_unit(unit)
   !**********************************************************
   IMPLICIT NONE
      INTEGER, INTENT(out) :: unit
      INTEGER  :: i 
      LOGICAL  :: opnd

      DO i=1,unitx
         unit = i
         INQUIRE(UNIT=1,OPENED=opnd)
         IF (.NOT. opnd) RETURN
      ENDDO
      CALL errore('get_free_unit','Unable to find an available unit',1)
   END SUBROUTINE get_free_unit


!**********************************************************
   SUBROUTINE file_delete(filename)
   !**********************************************************
   IMPLICIT NONE
      CHARACTER(*),  INTENT(in)  :: filename
      LOGICAL :: exist
      INTEGER :: unit,ierr
   
      INQUIRE(FILE=TRIM(filename),EXIST=exist)
      IF (.NOT. exist) RETURN
      CALL get_free_unit(unit)
      OPEN(unit,FILE=TRIM(filename),IOSTAT=ierr)
           IF (ierr/=0) CALL errore('file_delete','Unable to open file '//TRIM(filename),1)
      CLOSE(unit,STATUS='delete',IOSTAT=ierr)
           IF (ierr/=0) CALL errore('file_delete','Unable to close file '//TRIM(filename),1)
   END SUBROUTINE file_delete
      

!**********************************************************
   SUBROUTINE file_exist(filename,exist)
   !**********************************************************
   IMPLICIT NONE
      CHARACTER(*),  INTENT(in)  :: filename
      LOGICAL,       INTENT(out) :: exist
   
      INQUIRE(FILE=TRIM(filename),EXIST=exist)
      RETURN
   END SUBROUTINE file_exist


!**********************************************************
   SUBROUTINE file_open(unit,filename,path,root,       &
                       status,access,recl,form,position,action)
   !**********************************************************
   IMPLICIT NONE
      INTEGER,                INTENT(in)  :: unit
      CHARACTER(*),           INTENT(in)  :: filename
      CHARACTER(*), OPTIONAL, INTENT(in)  :: path
      CHARACTER(*), OPTIONAL, INTENT(in)  :: root
      CHARACTER(*), OPTIONAL, INTENT(in)  :: status
      CHARACTER(*), OPTIONAL, INTENT(in)  :: access
      INTEGER,      OPTIONAL, INTENT(in)  :: recl
      CHARACTER(*), OPTIONAL, INTENT(in)  :: form
      CHARACTER(*), OPTIONAL, INTENT(in)  :: position
      CHARACTER(*), OPTIONAL, INTENT(in)  :: action

      CHARACTER(9)                        :: subname='file_open'
      CHARACTER(7)                        :: status_
      CHARACTER(10)                       :: access_
      CHARACTER(11)                       :: form_
      CHARACTER(6)                        :: position_
      CHARACTER(9)                        :: action_
      CHARACTER(10*nstrx)                 :: path_
       
      LOGICAL                             :: fmt_iotk, tmp, binary
      INTEGER                             :: ierr, ndir, i
      CHARACTER(nstrx), POINTER           :: tags(:)

      !
      ! Allowed value for OPENING attributes
      !
      ! STATUS     = "old", "replace", "unknown"
      ! ACCESS     = "direct", "sequential"
      ! FORM       = "formatted", "unformatted"
      ! POSITION   = "asis", "rewind", "append"
      ! ACTION     = "read", "write", "readwrite"
      !
      ! If PATH is not present is assumed to be "/"
      ! that correspond to the IOTK ROOT file folder
      ! If PATH = "none" the iotk interface format is not used.
      ! PATH="/" just open the IOTK file in the main folder
      ! otherwise the requested path is searched.
      ! ROOT can be used only for WRITE opening
      !
     
      !
      ! Set defaults
      ! 
      status_   = "UNKNOWN"
      access_   = "SEQUENTIAL"
      form_     = "FORMATTED"
      position_ = "ASIS"
      action_   = "READWRITE"
      path_     = "/"

      !
      ! Passing Values
      ! 
      IF (PRESENT(status))      status_ = TRIM(status)
      IF (PRESENT(access))      access_ = TRIM(access)
      IF (PRESENT(form))          form_ = TRIM(form)
      IF (PRESENT(position))  position_ = TRIM(position)
      IF (PRESENT(action))      action_ = TRIM(action)
      IF (PRESENT(path))          path_ = TRIM(path)

      CALL change_case(status_,'UPPER')
      CALL change_case(access_,'UPPER')
      CALL change_case(form_,'UPPER')
      CALL change_case(position_,'UPPER')
      CALL change_case(action_,'UPPER')
      
      !
      ! Whether using IOTK
      ! 
      fmt_iotk = .TRUE.
      IF ( TRIM(path_) == "none" .OR. TRIM(path_) == "NONE" ) THEN 
         fmt_iotk = .FALSE.
      ELSE
         IF ( TRIM(path_) == "/") THEN
            ndir = 0
         ELSE
            CALL parser_path(TRIM(path_),ndir,tags)
         ENDIF
      ENDIF


      !
      ! Checking allowed values
      ! 
      IF ( TRIM(status_) /= "OLD" .AND. TRIM(status_) /= "NEW" .AND.         &
           TRIM(status_) /= "REPLACE" .AND.  TRIM(status_) /= "UNKNOWN")     &
           CALL errore(subname,"Invalid STATUS attribute = "//TRIM(status_),1)
      IF ( TRIM(access_) /= "DIRECT" .AND. TRIM(access_) /= "SEQUENTIAL")    &
           CALL errore(subname,"Invalid ACCESS attribute = "//TRIM(access_),1)
      IF ( TRIM(form_) /= "FORMATTED" .AND. TRIM(form_) /= "UNFORMATTED")    &
           CALL errore(subname,"Invalid FORM attribute = "//TRIM(form_),1)
      IF ( TRIM(position_) /= "ASIS" .AND. TRIM(position_) /= "REWIND" .AND. &
           TRIM(position_) /= "APPEND")    &
           CALL errore(subname,"Invalid POSITION attribute = "//TRIM(position_),1)
      IF ( TRIM(action_) /= "READ" .AND. TRIM(action_) /= "WRITE" .AND. &
           TRIM(action_) /= "READWRITE")    &
           CALL errore(subname,"Invalid ACTION attribute = "//TRIM(action_),1)

      !
      ! Compatibility
      ! 
      INQUIRE(unit,OPENED=tmp)
      IF ( tmp ) CALL errore(subname,"Unit already connected",1)
      
      IF ( fmt_iotk .AND. .NOT. PRESENT(action) ) &
           CALL errore(subname,"Action must be specified when using IOTK",1)
      IF ( fmt_iotk .AND. TRIM(action_) == "READWRITE" ) &
           CALL errore(subname,"ACTION == readwrite NOT allowed within IOTK",1)
      IF ( fmt_iotk .AND. (PRESENT(access) .OR. PRESENT(position)) ) &
           CALL errore(subname,"ACCESS or POSITION incompatible with IOTK",1)
      IF ( fmt_iotk .AND. TRIM(action_) == "READ" .AND. PRESENT(status) ) &
           CALL errore(subname,"STATUS is incompatible with IOTK read",1)

      IF ( TRIM(access_) == "DIRECT" .AND. .NOT. PRESENT(recl) ) &
           CALL errore(subname,"RECL must be specified for direct ACCESS",1)
      IF ( PRESENT(recl) ) THEN
           IF ( recl <= 0 ) CALL errore(subname,"RECL must be positive",1)
           IF ( TRIM(access_) /= "DIRECT" ) &
           CALL errore(subname,"RECL present while sequential access",1)
      ENDIF
      IF ( TRIM(access_) == "DIRECT" .AND. PRESENT(position) ) &
           CALL errore(subname,"DIRECT access and POSITION are incompatible",1)
      IF ( .NOT. fmt_iotk .AND. PRESENT(root) ) &
           CALL errore(subname,"ROOT should not be present",1)
      IF ( PRESENT(root) .AND. TRIM(action_) /= "WRITE") &
           CALL errore(subname,"ROOT must be used only for writing purposes",1)
      IF ( TRIM(action_) == "WRITE" .AND. ndir /= 0 ) &
           CALL errore(subname,"During WRITE PATH must be / "//TRIM(action_),1)
      
      !
      ! IOTK opening
      !
      IF ( fmt_iotk )  THEN
         
         binary = .FALSE.  
         IF ( TRIM(form_) == "UNFORMATTED" ) binary = .TRUE.
         
         SELECT CASE (TRIM(action_))
         CASE("READ")
               CALL iotk_open_read(unit,FILE=TRIM(filename),BINARY=binary,IERR=ierr)
         CASE("WRITE")
            IF ( PRESENT(root) ) THEN
               CALL iotk_open_write(unit,FILE=TRIM(filename),BINARY=binary, &
                                   ROOT=root,IERR=ierr)
            ELSE 
               CALL iotk_open_write(unit,FILE=TRIM(filename),BINARY=binary,IERR=ierr)
            ENDIF

         CASE DEFAULT
            CALL errore(subname,"Invalid ACTION for IOTK",1)

         END SELECT
         IF ( ierr/= 0) CALL errore(subname,"During opening of file: "//TRIM(filename),1)

         !
         ! goes into path
         !
         DO i=1,ndir
            CALL iotk_scan_begin(unit,TRIM(tags(i)),IERR=ierr )
            IF ( ierr /= 0 ) CALL errore(subname,"Unable to find tag: "//TRIM(tags(i)),1)
         ENDDO
           

      !
      ! ORDINARY opening
      !
      ELSE
         IF ( TRIM(access_ ) == "DIRECT" ) THEN
             OPEN(unit,FILE=filename,STATUS=status_,ACCESS=access_,RECL=recl,   &
                       ACTION=action_,FORM=form_,IOSTAT=ierr)
         ELSE
             OPEN(unit,FILE=filename,STATUS=status_,POSITION=position_,   &
                       ACTION=action_,FORM=form_,IOSTAT=ierr)
         ENDIF
         IF ( ierr/= 0) CALL errore(subname,"During opening of file"//TRIM(filename),1)
      ENDIF    

      !
      ! cleaning
      !
      IF ( ASSOCIATED(tags) ) DEALLOCATE(tags)

   END SUBROUTINE file_open


!**********************************************************
   SUBROUTINE file_close(unit,path,action)
   !**********************************************************
   IMPLICIT NONE
      INTEGER,                INTENT(in)  :: unit
      CHARACTER(*), OPTIONAL, INTENT(in)  :: path
      CHARACTER(*), OPTIONAL, INTENT(in)  :: action

      CHARACTER(10)                       :: subname='file_close'
      CHARACTER(9)                        :: action_
      CHARACTER(10*nstrx)                 :: path_
       
      LOGICAL                             :: fmt_iotk, tmp
      INTEGER                             :: ierr, ndir, i
      CHARACTER(nstrx), POINTER           :: tags(:)

      !
      ! Set defaults
      ! 
      action_   = "READWRITE"
      path_     = "/"

      !
      ! Passing Values
      ! 
      IF (PRESENT(action))      action_ = TRIM(action)
      IF (PRESENT(path))          path_ = TRIM(path)

      CALL change_case(action_,'UPPER')

      !
      ! Whether using IOTK
      ! 
      fmt_iotk = .TRUE.
      IF ( TRIM(path_) == "none" .OR. TRIM(path_) == "NONE") THEN 
         fmt_iotk = .FALSE.
      ELSE
         IF ( TRIM(path_) == "/") THEN
            ndir = 0
         ELSE
            CALL parser_path(TRIM(path_),ndir,tags)
         ENDIF
      ENDIF


      !
      ! Checking allowed values
      ! 
      IF ( TRIM(action_) /= "READ" .AND. TRIM(action_) /= "WRITE" .AND. &
           TRIM(action_) /= "READWRITE")    &
           CALL errore(subname,"Invalid ACTION attribute = "//TRIM(action_),1)
      IF ( TRIM(action_) == "WRITE" .AND. ndir /= 0 ) &
           CALL errore(subname,"During WRITE PATH must be / "//TRIM(action_),1)

      !
      ! Compatibility
      ! 
      INQUIRE(unit,OPENED=tmp)
      IF ( .NOT. tmp ) CALL errore(subname,"Unit NOT connected",1)
      
      IF ( fmt_iotk .AND. .NOT. PRESENT(action) ) &
           CALL errore(subname,"Action must be specified when using IOTK",1)
      IF ( fmt_iotk .AND. TRIM(action_) == "READWRITE" ) &
           CALL errore(subname,"ACTION == readwrite NOT allowed within IOTK",1)

      
      !
      ! IOTK closing
      !
      IF ( fmt_iotk )  THEN

         !
         ! moving to the ROOT folder
         !
         DO i=1,ndir
            CALL iotk_scan_end(unit,TRIM(tags(i)),IERR=ierr )
            IF (ierr/= 0 ) CALL errore(subname,"Unable to close tag: "//TRIM(tags(i)),1)
         ENDDO
         
         SELECT CASE (TRIM(action_))
         CASE("READ")
            CALL iotk_close_read(unit,IERR=ierr)

         CASE("WRITE")
            CALL iotk_close_write(unit,IERR=ierr)

         CASE DEFAULT
            CALL errore(subname,"Invalid ACTION for IOTK",1)

         END SELECT
         IF ( ierr/= 0) CALL errore(subname,"During closing",1)


      !
      ! ORDINARY closing
      !
      ELSE
         CLOSE(unit,IOSTAT=ierr)
         IF ( ierr/= 0) CALL errore(subname,"During closing",2)

      ENDIF

      !
      ! cleaning
      !
      IF ( ASSOCIATED(tags) ) DEALLOCATE(tags)

   END SUBROUTINE file_close

END MODULE files_module



