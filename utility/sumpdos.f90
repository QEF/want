!
! Copyright (C) 2005 Andrea Ferretti
! This file is distributed under the terms of the
! GNU General Public License. See the file `License'
! in the root directory of the present distribution,
! or http://www.gnu.org/copyleft/gpl.txt .
!
#if defined(__ABSOFT)
#  define getarg getarg_
#  define iargc  iargc_
#endif
!
PROGRAM sumpdos
  IMPLICIT NONE
  !
  ! AUTHOR: Andrea Ferretti
  !
  ! this program reads and sum pdos from different
  ! files (which are related to different atoms)
  !
  ! file names are read from stdin
  ! USAGE: sumpdos <file1> ... <fileN>
  !
  INTEGER             :: iargc              ! function giving no of arguments

  INTEGER             :: ngrid              ! dimension of the energy grid
  INTEGER             :: nfile              ! number of files to sum
  INTEGER             :: ncols              ! number of columns to sum


  CHARACTER(256), ALLOCATABLE    :: file(:) ! names of the files to sum
  CHARACTER(256)      :: filein, header
  CHARACTER(10)       :: cdum, str, str1

  LOGICAL             :: lexist
  REAL                :: efermi = 0.0       ! translate the input grid 
  REAL                :: rdum
  REAL, ALLOCATABLE   :: datain(:,:,:)     
  REAL, ALLOCATABLE   :: egrid(:)
  REAL, ALLOCATABLE   :: mysum(:,:)
  
  INTEGER :: ios, ierr, iarg, ie, ic, ifile, i


!**************************************************************

!
! get and check the number of arguments
!
   nfile = iargc ()
   IF ( nfile == 0 ) THEN
      WRITE(0,"( 'No files to sum' )")
      STOP
   ENDIF

   CALL getarg ( 1, str )
   !
   SELECT CASE ( TRIM(str) )
   CASE ( "-h" )
      !
      ! write the manual
      !
      WRITE(0,"(/,'USAGE: sumpdos [-h] [-f <filein>] [<file1> ... <fileN>]', /, & 
                &'  Sum the data from the file specified in input and write the sum ', /, &
                &'  to stdout', /, &
                &'     -h           : write this manual',/, &
                &'     -f <filein>  : takes the list of data files from <filein> ', /, &
                &'                    (one per line) instead of command line',/, &
                &'     <fileM>      : the M-th data file', &
                & / )")
      STOP
      !
   CASE ( "-f" )
      !
      ! read file names from file
      !
      CALL getarg ( 2, filein )
      IF ( LEN_TRIM(filein) == 0 ) CALL errore('sumpdos','provide filein name',2)

      INQUIRE( FILE=TRIM(filein), EXIST=lexist )
      IF (.NOT. lexist) CALL errore('sumpdos','file '//TRIM(filein)//' does not exist',3)
      OPEN( 10, FILE=TRIM(filein), IOSTAT=ios ) 
      IF (ios/=0) CALL errore('sumpdos','opening '//TRIM(filein),ABS(ios))

      !
      ! get the number of non-empty lines in the file 
      ! (which is assumed to be the number of files to sum)
      !
      ios = 0
      nfile = 0 
      !
      DO WHILE ( ios == 0 ) 
         nfile = nfile + 1
         READ(10, *, IOSTAT=ios ) str
         IF ( ios ==0 .AND. LEN_TRIM(str)==0 ) nfile = nfile -1
      ENDDO
      nfile = nfile -1 

      !
      IF (nfile ==0 ) CALL errore('sumpdos','no file to sum in '//TRIM(filein),4)
      ! 
      ALLOCATE( file(nfile), STAT=ierr )
      IF (ierr/=0) CALL errore('sumpdos','allocating FILE',ABS(ierr))
      !
      REWIND(10)

      DO i = 1, nfile
         file(i) = ' '
         DO WHILE( LEN_TRIM(file(i)) == 0 )
            READ(10,"(A256)", IOSTAT=ios) file(i)
            IF (ios /=0 ) CALL errore('sumpdos','reading from '//TRIM(filein),i)
         ENDDO
      ENDDO

   CASE DEFAULT

      !
      ! get the names of the files
      ! here we use GETARG
      !
      ALLOCATE( file(nfile), STAT=ierr )
      IF (ierr/=0) CALL errore('sumpdos','allocating FILE',ABS(ierr))
      DO iarg = 1, nfile
         CALL getarg ( iarg, file(iarg) )
      ENDDO

   END SELECT

!
! open the first file and get data about spin
! and grid dimensions
!
   INQUIRE( FILE=TRIM(file(1)), EXIST=lexist )
   IF (.NOT. lexist) CALL errore('sumpdos','file '//TRIM(file(1))//' does not exist',3)
   !
   WRITE(0,"('Reading dimensions from file: ',a)") TRIM(file(1))
   !
   OPEN(10, FILE=TRIM(file(1)), IOSTAT=ios)
      IF (ios/=0) CALL errore("sumpdos", "error opening "//TRIM(file(1)), 1)
      
      !
      ! read the header line
      !
      READ(10,"(A256)", IOSTAT=ios ) header
      IF (ios/=0) CALL errore("sumpdos", "reading header of "//TRIM(file(1)), 1)

      !
      ! get the number of columns to sum
      ! if a pDOS file is recognize determin ncols from the header,
      ! otherwise, set ncols to 1
      !
      READ(header,*, IOSTAT=ios) cdum, cdum, cdum, str1
      IF (ios/=0) CALL errore("sumpdos", "reading from header of "//TRIM(file(1)), 1)
      !
      !
      ! set here the defaults
      ncols = 1
      !
      IF ( TRIM(str1) == 'ldos(E)' ) THEN
          ncols = 1
      ELSEIF ( TRIM(str1) == 'ldosup(E)' ) THEN
          ncols = 2
      ENDIF

      !         
      ! determine the dimension fo the energy mesh        
      ! no further control will be done on the consistency of the energy
      ! grid of each file
      !         
      ie = 0
      READ(10, *)
      !
      DO WHILE ( .TRUE.  )
         READ( 10, *, IOSTAT=ios ) rdum
         IF ( ios /= 0 ) EXIT
         ie = ie + 1
      ENDDO
      !
      ngrid = ie

   CLOSE(10)

!
! allocations
!
   ALLOCATE( datain( ngrid, ncols, nfile), STAT=ierr )
      IF (ierr/=0) CALL errore("sumpdos", "allocating datain", ierr)
   ALLOCATE( mysum( ngrid, ncols), STAT=ierr )
      IF (ierr/=0) CALL errore("sumpdos", "allocating mysum", ierr)
   ALLOCATE( egrid( ngrid) )
      IF (ierr/=0) CALL errore("sumpdos", "allocating egrid", ierr)


!
! get data
!
   WRITE(0,"('Reading the following ',i5,' files: ')") nfile
   !
   DO ifile = 1, nfile
      !
      INQUIRE( FILE=TRIM(file(ifile)), EXIST=lexist )
      IF (.NOT. lexist) &
         CALL errore('sumpdos','file '//TRIM(file(ifile))//' does not exist',ifile)
      !
      WRITE(0,"(2x,'Reading file: ',a)") TRIM(file(ifile))
      OPEN(10, FILE=TRIM(file(ifile)), IOSTAT=ios)
         IF (ios/=0) CALL errore("sumpdos", "error opening "//TRIM(file(ifile)), ios )
         !
         READ(10,*, IOSTAT=ios)
         IF (ios/=0) &
            CALL errore("sumpdos", "reading first line in "//TRIM(file(ifile)), ios )
         !
         ! egrid is overwritten every time
         !
         DO ie = 1, ngrid
            READ(10, *, IOSTAT=ios ) egrid(ie), datain(ie, 1:ncols, ifile)
            IF (ios/=0) &
            CALL errore("sumpdos", "reading first line in "//TRIM(file(ifile)), ie )
         ENDDO
     CLOSE(10)
   ENDDO

!
! sum and write
!
   WRITE(6,"(a)") TRIM(header)
   !
   mysum = 0.0
   DO ie=1,ngrid
      !
      DO ic=1,ncols
         mysum(ie,ic) = SUM( datain(ie,ic,:) )
      ENDDO
      !
      WRITE(6,"(3f15.9)") egrid(ie) -efermi, mysum(ie,1:ncols)
   ENDDO

!
! clean
!
   DEALLOCATE( file, STAT=ierr )
      IF (ierr/=0) CALL errore("sumpdos", "deallocating file", ierr)
   DEALLOCATE( datain, STAT=ierr )
      IF (ierr/=0) CALL errore("sumpdos", "deallocating datain", ierr)
   DEALLOCATE( mysum, STAT=ierr )
      IF (ierr/=0) CALL errore("sumpdos", "deallocating mysum", ierr)
   DEALLOCATE( egrid, STAT=ierr )
      IF (ierr/=0) CALL errore("sumpdos", "deallocating egrid", ierr)

END PROGRAM sumpdos


!*************************************************
SUBROUTINE errore(routine, msg, ierr)
   !*************************************************
   IMPLICIT NONE
   CHARACTER(*),    INTENT(in) :: routine, msg
   INTEGER,         INTENT(in) :: ierr

   !
   WRITE( UNIT = 0, FMT = '(/,1X,78("*"))')
   WRITE( UNIT = 0, &
          FMT = '(5X,"from ",A," : error #",I10)' ) routine, ierr
   WRITE( UNIT = 0, FMT = '(5X,A)' ) msg
   WRITE( UNIT = 0, FMT = '(1X,78("*"),/)' )
   !
   STOP
   RETURN
END SUBROUTINE errore





