!
! Copyright (C) 2011 Andrea Ferretti
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
PROGRAM sum_sgm
  !
  USE kinds
  USE constants,            ONLY : CZERO
  USE io_module,            ONLY : iunit=> aux1_unit, ounit=> aux2_unit, stdout, stderr
  USE iotk_module
  USE operator_module,      ONLY : operator_read_init, operator_read_aux, &
                                   operator_read_data, operator_read_close, &
                                   operator_write_init, operator_write_aux, &
                                   operator_write_data, operator_write_close
  !
  IMPLICIT NONE
  !
  ! AUTHOR: Andrea Ferretti
  !
  ! this program reads and sums self-energies (want fmt)
  ! from different files 
  !
  ! file names are read from stdin
  ! USAGE: sum_sgm [-h,--help] [<file1> ... <fileN>] [-f <file_list>]
  !                [-b,--binary] [-t,--textual]
  !
  INTEGER             :: iargc              ! function giving no of arguments

  INTEGER             :: ngrid              ! dimension of the energy grid
  INTEGER             :: nfiles             ! number of files to sum


  CHARACTER(256), ALLOCATABLE  :: files(:) ! names of the files to sum
  CHARACTER(256)      :: filein, fileout
  CHARACTER(7)        :: subname="sum_sgm"
  !
  LOGICAL             :: lhave_list
  LOGICAL             :: lprint_man
  LOGICAL             :: lexist, binary, strem
  LOGICAL             :: ldynam, ldynam_
  INTEGER             :: nomega, nomega_
  INTEGER             :: iomg_s, iomg_e, iomg_s_, iomg_e_
  INTEGER             :: dimwann, dimwann_, nr, nr_
  CHARACTER(20)       :: analyticity, analyticity_
  CHARACTER(20)       :: egrid_units
  CHARACTER(256)      :: str
  !
  INTEGER,       ALLOCATABLE :: ivr(:,:), ivr_(:,:)
  REAL(dbl),     ALLOCATABLE :: vr(:,:)
  REAL(dbl),     ALLOCATABLE :: egrid(:)
  COMPLEX(dbl),  ALLOCATABLE :: sgm(:,:,:), sgm_aux(:,:,:)
  ! 
  INTEGER :: ios, ierr, narg, iarg, ie, ifile, i, ind


!**************************************************************

!
! get and check the number of arguments
!
   narg   = iargc ()
   ind    = 0
   nfiles = 0 
   !
   ALLOCATE( files(narg), STAT=ierr )
   IF (ierr/=0) CALL errore(subname,'allocating FILES',ABS(ierr))
   !
   lprint_man = .FALSE.
   lhave_list = .FALSE.
   fileout    = "./sgm_sum.dat"
   binary     = .TRUE.
 

   !
   ! parse arguments
   !
   DO  WHILE ( ind < narg )
       !
       ind = ind + 1 
       !
       CALL getarg( ind, str )
       !
       SELECT CASE ( TRIM(str) )
       !
       CASE ( "-h", "--help")
          !
          lprint_man = .TRUE.
          !
       CASE ( "-o" )
          !
          ! read the name of the output file
          !
          ind = ind+1
          CALL getarg ( ind, fileout )
          IF ( LEN_TRIM(fileout) == 0 ) CALL errore(subname,'no fileout given',2)
          !
       CASE ( "-f" )
          !
          ! read the name of the input list
          !
          ind = ind+1
          CALL getarg ( ind, filein )
          IF ( LEN_TRIM(fileout) == 0 ) CALL errore(subname,'no fileout given',2)
          !
          lhave_list = .TRUE.
          !
       CASE ( "-b", "--bin", "--binary" )
          !
          binary = .TRUE.
          !
       CASE ( "-t", "--text", "--xml", "--textual" )
          !
          binary = .FALSE.
          !
       CASE DEFAULT
          !
          ! get the names of the files
          !
          nfiles = nfiles + 1
          files(nfiles) = TRIM(str)
          ! 
       END SELECT
       !
   ENDDO

   
   IF ( lprint_man ) THEN
       !
       ! write the manual
       !
       WRITE(stderr,"(/,'USAGE: sum_sgm [-h,--help] [-f <filein>] [<file1> ... <fileN>]', /, & 
                 &'  Sum the Sigma from the files specified in input and write', /, &
                 &'  the sum to file', /, &
                 &'     -h, --help   : write this manual',/, &
                 &'     -o <fileout> : name of the output file [./sgm_sum.dat] ', /, &
                 &'     -b, --binary : output file is binary [default] ', /, &
                 &'     -t, --textual: output file is textual', /, &
                 &'     -f <filein>  : takes the list of data files from <filein> ', /, &
                 &'                    (one per line) instead of command line',/, &
                 &'     <fileM>      : the M-th data file', &
                 & / )")
       STOP
       !
   ENDIF
   !
   !
   IF ( lhave_list ) THEN
       !
       INQUIRE( FILE=TRIM(filein), EXIST=lexist )
       IF (.NOT. lexist) CALL errore(subname,'file '//TRIM(filein)//' does not exist',3)
       OPEN( iunit, FILE=TRIM(filein), IOSTAT=ios ) 
       IF (ios/=0) CALL errore(subname,'opening '//TRIM(filein),ABS(ios))
  
       !
       ! get the number of non-empty lines in the file 
       ! (which is assumed to be the number of files to sum)
       !
       IF ( nfiles /= 0 ) CALL errore(subname,"files already specified on cmd line", 10)
       !
       IF ( ALLOCATED(files) ) THEN
           DEALLOCATE( files, STAT=ierr)
           IF ( ierr/=0 ) CALL errore(subname,'deallocating files', 10)
       ENDIF
       !
       ios = 0
       nfiles = 0
       !
       DO WHILE ( ios == 0 ) 
          nfiles = nfiles + 1
          READ(iunit, *, IOSTAT=ios ) str
          IF ( ios ==0 .AND. LEN_TRIM(str)==0 ) nfiles = nfiles -1
       ENDDO
       nfiles = nfiles -1 
  
       !
       IF (nfiles ==0 ) CALL errore(subname,'no file to sum in '//TRIM(filein),4)
       ! 
       ALLOCATE( files(nfiles), STAT=ierr )
       IF (ierr/=0) CALL errore(subname,'allocating FILES',ABS(ierr))
       !
       REWIND(iunit)
  
       DO i = 1, nfiles
          files(i) = ' '
          DO WHILE( LEN_TRIM(files(i)) == 0 )
             READ(iunit,"(A256)", IOSTAT=ios) files(i)
             IF (ios /=0 ) CALL errore(subname,'reading from '//TRIM(filein),i)
          ENDDO
       ENDDO
       !
       CLOSE( iunit, IOSTAT=ios )
       IF (ios /=0 ) CALL errore(subname,'closing unit file_list',ABS(ios))
       !
   ENDIF
   !
   IF ( nfiles == 0 ) CALL errore(subname,"no files to sum",10)
   !
   ! report
   !
   WRITE(stderr,"(2x,'    Output file: ',a)") TRIM(fileout)
   IF ( lhave_list ) THEN 
       WRITE(stderr,"(2x,'      List file: ',a)") TRIM(filein)
   ENDIF
   WRITE(stderr,"(2x,'         Binary: ',l)") binary
   WRITE(stderr,"(2x,'Number of files: ',i5)") nfiles
   !
   DO ifile = 1, nfiles
       WRITE(stderr,"(6x,'file( ',i3,' ) = ',a)") ifile, TRIM(files(ifile)) 
   ENDDO



   !
   ! open the first file and get data
   ! and grid dimensions
   !
   INQUIRE( FILE=TRIM(files(1)), EXIST=lexist )
   IF (.NOT. lexist) CALL errore(subname,'file '//TRIM(files(1))//' does not exist',3)
   !
   WRITE(stderr,"(2x,'Reading dimensions from file: ',a)") TRIM(files(1))
   !
   ! opening
   !
   CALL operator_read_init( iunit, files(1), IERR=ierr )
   IF ( ierr/=0 ) CALL errore(subname,'opening '//TRIM(files(1)), 10)
   !
   ! get dims
   !
   CALL operator_read_aux( iunit, dimwann, ldynam, NOMEGA=nomega, & 
                           IOMG_S=iomg_s, IOMG_E=iomg_e, &
                           ANALYTICITY=analyticity, NR=nr, IERR=ierr)
   IF (ierr/=0 ) CALL errore(subname,"reading aux data I", ABS(ierr))
   !
   ! allocate
   !
   ALLOCATE( vr(3,nr), ivr(3,nr), STAT=ierr )
   IF (ierr/=0 ) CALL errore(subname,"allocating vr, ivr", ABS(ierr))
   ALLOCATE( ivr_(3,nr), STAT=ierr )
   IF (ierr/=0 ) CALL errore(subname,"allocating ivr_", ABS(ierr))
   ALLOCATE( sgm(dimwann,dimwann,nr), STAT=ierr )
   IF (ierr/=0 ) CALL errore(subname,"allocating sgm", ABS(ierr))
   ALLOCATE( sgm_aux(dimwann,dimwann,nr), STAT=ierr )
   IF (ierr/=0 ) CALL errore(subname,"allocating sgm_aux", ABS(ierr))
   ALLOCATE( egrid(nomega), STAT=ierr )
   IF (ierr/=0 ) CALL errore(subname,"allocating egrid", ABS(ierr))

   !
   ! other aux data
   !
   IF ( ldynam ) THEN
       CALL operator_read_aux( iunit, GRID=egrid, VR=vr, IVR=ivr, IERR=ierr)
       IF (ierr/=0 ) CALL errore(subname,"reading aux data II", ABS(ierr))
   ELSE
       CALL operator_read_aux( iunit, VR=vr, IVR=ivr, IERR=ierr)
       IF (ierr/=0 ) CALL errore(subname,"reading aux data II", ABS(ierr))
   ENDIF
   !
   CALL operator_read_close( iunit, IERR=ierr )
   IF ( ierr/=0 ) CALL errore(subname,'closing '//TRIM(files(1)), 10)


   
   !
   ! init aux data
   !
   CALL operator_write_init(ounit, TRIM(fileout), BINARY=binary)
   !   
   IF (ldynam) THEN
       !
       egrid_units = "eV"
       !
       CALL operator_write_aux(ounit, dimwann, ldynam, nomega, 1, &
                               nomega, egrid, egrid_units, analyticity, &
                               nr, vr, ivr)
       !
   ELSE
       !
       CALL operator_write_aux(ounit, dimwann, ldynam, NOMEGA=nomega, &
                               NRTOT=nr, VR=vr, IVR=ivr)
       !
   ENDIF


   !
   ! get all data
   !
   energy_loop: &
   DO ie = 1, nomega
       !
       sgm(:,:,:) = CZERO
       !
       file_loop: &
       DO ifile = 1, nfiles
           !
           CALL operator_read_init( iunit, files(ifile), IERR=ierr )
           IF ( ierr/=0 ) CALL errore(subname,'opening '//TRIM(files(ifile)), 10)
           !
           ! dims and checks
           !
           IF ( ie == 1 ) THEN
               !
               CALL operator_read_aux( iunit, dimwann_, ldynam_, NOMEGA=nomega_, & 
                                       IOMG_S=iomg_s_, IOMG_E=iomg_e_, &
                                       ANALYTICITY=analyticity_, NR=nr_, IERR=ierr)
               IF (ierr/=0 ) CALL errore(subname,"reading aux data III", ABS(ierr))
               !
               IF ( dimwann /= dimwann_ ) CALL errore(subname,"invalid dimwann_", ifile)
               IF ( ldynam  /= ldynam_ )  CALL errore(subname,"invalid ldynam_", ifile)
               IF ( nomega  /= nomega_ )  CALL errore(subname,"invalid nomega_", ifile)
               IF ( iomg_s  /= iomg_s_ )  CALL errore(subname,"invalid iomg_s_", ifile)
               IF ( iomg_e  /= iomg_e_ )  CALL errore(subname,"invalid iomg_e_", ifile)
               IF ( nr      /= nr_ )      CALL errore(subname,"invalid nr_", ifile)
               IF ( TRIM(analyticity) /= TRIM(analyticity_) ) &
                                          CALL errore(subname,"invalid analyticity", ifile)
               !
           ENDIF
           !
           IF ( ldynam ) THEN 
               !
               CALL operator_read_data( iunit, IE=ie, R_OPR=sgm_aux, IERR=ierr )
               !
           ELSE
               !
               CALL operator_read_data( iunit, R_OPR=sgm_aux, IERR=ierr )
               !
           ENDIF
           !
           IF ( ierr/=0 ) CALL errore(subname,"reading data", ifile)
           !
           CALL operator_read_close( iunit, IERR=ierr )
           IF ( ierr/=0 ) CALL errore(subname,'closing '//TRIM(files(ifile)), 10)
           !
           sgm(:,:,:) = sgm(:,:,:) + sgm_aux(:,:,:)
           !
       ENDDO file_loop

       !
       ! write to file
       !
       CALL operator_write_data( ounit, sgm, ldynam, ie)
       !
   ENDDO energy_loop

   !
   ! finalize
   !
   CALL operator_write_close( ounit )
   !
   WRITE(stderr,"(/,2x,'Data written on file: ',a)") TRIM(fileout)
           


   !
   ! clean up
   !
   DEALLOCATE( files, STAT=ierr )
   IF (ierr/=0) CALL errore(subname, "deallocating files", ierr)
   DEALLOCATE( vr, ivr, ivr_, STAT=ierr )
   IF (ierr/=0) CALL errore(subname, "deallocating vr, ivr, ivr_", ierr)
   DEALLOCATE( egrid, STAT=ierr )
   IF (ierr/=0) CALL errore(subname, "deallocating egrid", ierr)
   DEALLOCATE( sgm, sgm_aux, STAT=ierr )
   IF (ierr/=0) CALL errore(subname, "deallocating sgm, sgm_aux", ierr)

CONTAINS

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

END PROGRAM sum_sgm

