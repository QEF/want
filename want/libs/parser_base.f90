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
   MODULE parser_base_module
!*********************************************
   IMPLICIT NONE
   PRIVATE

! This module contains some utilities to
! handle formatted strings for general purpose.
! The INFO (INTEGER) optional flag when present
! takes the following values:
! INFO < 0      empty string
! INFO = 0      right string fmt
! INFO > 0      wrong string fmt
!
! * REPLICA STRING format:
!      "INTERVAL_1,INTERVAL_2,....,INTERVAL_N"
!       where INTERVAL can be:
!       i1-i2   OR  i1
!       indicating the interval between i1 ans i2,
!       and the index i1 iteself respectively
!
! * PATH STRING format:
!       "/TAG_1/...../TAG_N"
!
! * VERSION STRING
!      "NAME-MAJOR.MINOR.PATCH"
! 
! routines in this module:
! FUNCTION char2int(c,[ierr])    ! From IOTK package
! FUNCTION int2char(i)
! FUNCTION log2char(l)
! FUNCTION log2int(l)
! FUNCTION upper_case(char)
! FUNCTION lower_case(char)
! SUBROUTINE change_case(str,case)
! SUBROUTINE parser_replica(str,nint,intervals[,info])
! SUBROUTINE parser_path(str,ndir,directories[,info])
! SUBROUTINE parser_version(str,name,major,minor,patch[,info])
!
! </INFO>
!


!
! end of declarations
!

   PUBLIC ::  int2char,       &
              char2int,       &
              log2char,       &
              log2int,        &
              upper_case,     &
              lower_case,     &
              change_case,    &
              parser_replica, &
              parser_path,    &
              parser_version

CONTAINS

!
! Subroutines
!   

!***********************
   FUNCTION int2char(i)
   !***********************
      INTEGER, INTENT(in) :: i
      CHARACTER(len=15)   :: int2char
      WRITE(int2char,"(i15)") i
      int2char = ADJUSTL(int2char)
   END FUNCTION int2char


!***********************
   FUNCTION char2int(c,ierr)
   !***********************
      CHARACTER(*), INTENT(in) :: c
      INTEGER                  :: char2int
      INTEGER, OPTIONAL        :: ierr
      CHARACTER(20) :: fmt
      fmt = "(i"//TRIM(int2char(len(c)))//")"
      IF ( PRESENT(ierr) ) THEN
         READ(c,TRIM(fmt), IOSTAT=ierr) char2int
      ELSE
         READ(c,TRIM(fmt)) char2int
      ENDIF
   END FUNCTION char2int


!***********************
   FUNCTION log2char(l)
   !***********************
      LOGICAL,      INTENT(in) :: l
      CHARACTER(5)             :: log2char
      log2char="TRUE"
      IF ( .NOT. l) log2char="FALSE"
   END FUNCTION log2char


!***********************
   FUNCTION log2int(l)
   !***********************
      LOGICAL,      INTENT(in) :: l
      INTEGER                  :: log2int
      log2int = 1
      IF ( .NOT. l) log2int = -1
   END FUNCTION log2int


!****************************************
   FUNCTION upper_case (character)
    !****************************************
    !
    !   From FPMD code:
    !   converts character to uppercase if lowercase
    !   copy character to output in all other cases
    !
    IMPLICIT NONE
    CHARACTER (len=1) :: upper_case, character
    !
    CHARACTER (len=26) :: minuscole='abcdefghijklmnopqrstuvwxyz', &
                          maiuscole='ABCDEFGHIJKLMNOPQRSTUVWXYZ'
    INTEGER :: i
    !
    DO i=1,26
       IF (character.EQ.minuscole(i:i)) THEN
          upper_case=maiuscole(i:i)
          RETURN
       ENDIF
    ENDDO
    upper_case = character
    !
    RETURN
  END FUNCTION upper_case


!****************************************
   FUNCTION lower_case (character)
    !****************************************
    IMPLICIT NONE
    CHARACTER (len=1) :: lower_case, character
    !
    CHARACTER (len=26) :: minuscole='abcdefghijklmnopqrstuvwxyz', &
                          maiuscole='ABCDEFGHIJKLMNOPQRSTUVWXYZ'
    INTEGER :: i
    !
    DO i=1,26
       IF (character.EQ.maiuscole(i:i)) THEN
          lower_case=minuscole(i:i)
          RETURN
       ENDIF
    ENDDO
    lower_case = character
    !
    RETURN
  END FUNCTION lower_case


!**********************************************************
   SUBROUTINE change_case(str,case)
   !**********************************************************
   !
   ! Change the case of a given STRING according to the desidered CASE
   ! CASE has two alloed values: UPPER, LOWER
   !
   IMPLICIT NONE
      CHARACTER(*),     INTENT(inout)    :: str        
      CHARACTER(*),     INTENT(in)       :: case

      CHARACTER(5) :: case_                 
      INTEGER  :: i,length

      length = LEN_TRIM(str)
      IF ( length == 0) RETURN
      SELECT CASE ( TRIM(case) )
      CASE ( 'upper', 'UPPER' ) 
           DO i=1,length
              str(i:i) = upper_case(str(i:i))
           ENDDO
      CASE ( 'lower', 'LOWER')
           DO i=1,length
              str(i:i) = lower_case(str(i:i))
           ENDDO
      CASE DEFAULT
           CALL errore('change_case','Invalid CASE = '//TRIM(case), 1)
      END SELECT

   END SUBROUTINE change_case


!**********************************************************
   SUBROUTINE parser_replica(str,nint,intervals,info)
   !**********************************************************
   IMPLICIT NONE
   CHARACTER(*),     INTENT(in)    :: str        
   INTEGER,          INTENT(out)   :: nint
   INTEGER, POINTER                :: intervals(:,:)
   INTEGER, OPTIONAL,INTENT(out)   :: info

   CHARACTER(15), ALLOCATABLE      :: intstr(:)
   INTEGER                         :: length
   INTEGER                         :: istart,iend
   INTEGER                         :: index
   INTEGER                         :: i,iint, ierr
   !
   ! ierr < 0     void string
   ! ierr = 0     no problem
   ! ierr > 0     wrong fmt
   !

   length=LEN_TRIM(str)
   NULLIFY( intervals )
   nint = 0
   IF ( PRESENT(info) ) info = -1
   IF ( length == 0 ) RETURN


   IF ( PRESENT(info) ) info = 0
   nint=1
   DO i=1,length
      IF ( str(i:i) == "," ) nint=nint+1
   ENDDO
   ALLOCATE( intstr(nint), STAT=ierr )
     IF ( ierr /= 0 ) CALL errore('parser_replica','Unable to allocate INTSTR',ABS(ierr))
   ALLOCATE( intervals(nint,2), STAT=ierr )
     IF ( ierr /= 0 ) CALL errore('parser_replica','Unable to allocate intervals',ABS(ierr))

!
! recognize different intervals
!
   istart=1
   iint=1
   DO i=1,length
      IF ( str(i:i) == "," )  THEN
          iend=i-1
          intstr(iint) = TRIM(str(istart:iend))
          iint=iint+1
          istart=i+1
      ENDIF
   ENDDO
   intstr(nint) = TRIM(str(istart:length))
   
!
! for each interval determine the extrema
!
   DO iint=1,nint   
      index=SCAN(intstr(iint),"-")
      length=LEN_TRIM(intstr(iint))
      IF ( index == 0 )  THEN
          intervals(iint,:) = char2int( intstr(iint),IERR=ierr )
          IF ( ierr/= 0 ) CALL errore('parser_replica','Wrong internal fmt',ABS(ierr))
      ELSE
          intervals(iint,1) = char2int( intstr(iint)(1:index-1), IERR=ierr )
          IF ( ierr/= 0 ) CALL errore('parser_replica','Wrong internal fmt',ABS(ierr))
          intervals(iint,2) = char2int( intstr(iint)(index+1:length), IERR=ierr )
          IF ( ierr/= 0 ) CALL errore('parser_replica','Wrong internal fmt',ABS(ierr))
      ENDIF
   ENDDO
   DEALLOCATE( intstr, STAT = ierr)   
     IF ( ierr /= 0 ) CALL errore('parser_replica','Unable to deallocate INTSTR',ABS(ierr))

   END SUBROUTINE parser_replica


!**********************************************************
   SUBROUTINE parser_path(str,ndir,directories,info)
   !**********************************************************
   IMPLICIT NONE
   CHARACTER(*),     INTENT(in)    :: str
   INTEGER,          INTENT(out)   :: ndir
   CHARACTER(*),     POINTER       :: directories(:)
   INTEGER, OPTIONAL,INTENT(out)   :: info

   CHARACTER(2000)                 :: str_
   INTEGER, ALLOCATABLE            :: intstr(:)
   INTEGER                         :: length
   INTEGER                         :: i, ierr
   !
   ! info < 0     void string
   ! info = 0     no problem
   ! info > 0     fatal error
   !

   str_ = TRIM(ADJUSTL(str))
   length=LEN_TRIM(str_)
   ndir = 0
   NULLIFY( directories )
   IF ( PRESENT(info) ) info = -1
   IF ( length == 0 .AND. PRESENT(info) ) RETURN
   IF ( length == 0 )  &
      CALL errore('parser_path','STR is VOID',1)
   IF ( str_(1:1) /= "/") &
      CALL errore('parser_path','First non-null character MUST be /',1)


   IF ( PRESENT(info) ) info = 0
   ALLOCATE( intstr(length), STAT=ierr )
     IF ( ierr /= 0 ) CALL errore('parser_path','Unable to allocate INTSTR',ABS(ierr))

   !
   ! put a / at the end of string if needed
   !
   IF ( str_(length:length) /= "/") THEN 
      str_ = TRIM(str_)//"/"
      length = length +1 
   ENDIF

   !
   ! serch for /
   !
   ndir=0
   DO i=1,length
      IF ( str_(i:i) == "/" ) THEN
         ndir=ndir +1
         intstr(ndir) = i
      ENDIF
   ENDDO
   ndir = ndir-1
   ALLOCATE( directories(ndir), STAT=ierr )
     IF ( ierr /= 0 ) CALL errore('parser_path','Unable to allocate DIRECTORIES',ABS(ierr))

   !
   ! parse directory names
   !
   DO i=1,ndir
      directories(i) = str_(intstr(i)+1:intstr(i+1)-1)
   ENDDO
   DEALLOCATE( intstr, STAT = ierr)   
     IF ( ierr /= 0 ) CALL errore('parser_path','Unable to deallocate INTSTR',ABS(ierr))

   END SUBROUTINE parser_path


!**********************************************************
   SUBROUTINE parser_version(str,name,major,minor,patch,info)
   !**********************************************************
   IMPLICIT NONE
   CHARACTER(*),     INTENT(in)    :: str
   CHARACTER(*),     INTENT(out)   :: name
   INTEGER,          INTENT(out)   :: major,minor,patch
   INTEGER, OPTIONAL,INTENT(out)   :: info
   !
   ! info < 0     void string
   ! info = 0     no problem
   ! info > 0     fatal error
   !
   CHARACTER(2000)                 :: str_
   INTEGER                         :: i1,i2,i3,length
   INTEGER                         :: ierr, ierrtot
   CHARACTER(10)                   :: number(3)
    
      name = " "
      major = 0
      minor = 0
      patch = 0
      str_ = TRIM( str )

      IF ( PRESENT(info) ) info = 0
      length = LEN( str_ )
      IF ( length == 0 ) THEN 
           info = -1 
           RETURN
      ENDIF

      i1 = SCAN(str_,"-")
      i2 = SCAN(str_,".")
      i3 = SCAN(str_,".",BACK=.TRUE.)
      IF ( i1 == 0 .OR. i2 == 0 .OR. i3 == 0 .OR. i2 == i3 ) THEN 
         IF ( PRESENT(info) ) THEN
            info = 1
            RETURN
         ELSE
            CALL errore('parser_version','Invalid VERSION fmt',1)
         ENDIF
      ENDIF
   
      name = str_( 1:i1-1 )
      number(1) = str_( i1+1 : i2-1 )
      number(2) = str_( i2+1 : i3-1 )
      number(3) = str_( i3+1 : length )
      
      ierrtot = 0
      major = char2int( number(1), IERR=ierr )
         IF (ierr/=0) ierrtot = ierrtot + ABS(ierr)
      minor = char2int( number(2), IERR=ierr )
         IF (ierr/=0) ierrtot = ierrtot + ABS(ierr)
      patch = char2int( number(3), IERR=ierr )
         IF (ierr/=0) ierrtot = ierrtot + ABS(ierr)
   
      IF (ierrtot /= 0) THEN
         IF (PRESENT(info)) THEN 
            info = +2
            RETURN
         ELSE
            CALL errore('parser_version','Invalid NUMBER fmt',1)
         ENDIF
      ENDIF

   END SUBROUTINE parser_version

END MODULE parser_base_module

