!
! Copyright (C) 2011 Andrea Ferretti
!
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
!
!=====================================================
   PROGRAM decay_prog
   !=====================================================
   !
   ! this is a simple utility to plot the real space decay
   ! of the hamiltonian or self-energy operator.
   !
   ! Usage:  decay.x <file_1> ... <file_n>  < input_file
   !
   ! input file layout
   !
   !   &INPUT
   !      work_dir         the directory where the datafiles are stored.
   !      filein(*)        files to be summed 
   !      measure          measure of the decay
   !                       ( "norm2" | "max" | "diag" | "diag_element" )
   !      iwan             index of the WF to be used to sample the decay
   !   /
   !
   ! If <file_1> <file_n> are specified as arguments, this overwirtes
   ! the filein(*) var from input_file
   !
   USE kinds
   USE parameters,         ONLY : nstrx
   USE constants,          ONLY : CZERO, ZERO
   USE io_module,          ONLY : work_dir, stdin, stdout
   USE version_module,     ONLY : version_number
   USE files_module,       ONLY : file_exist
   USE subspace_module,    ONLY : dimwann
   USE kpoints_module,     ONLY : nrtot
   USE parser_module 
   USE timing_module
   !
   USE decay_module,       ONLY : decay_read_file, decay_valid_type
   !
   IMPLICIT NONE

   INTEGER, PARAMETER :: nfilex = 5
   !
   ! input variables
   !
   CHARACTER( 20 )  :: measure 
   CHARACTER( 256)  :: filein(nfilex)
   INTEGER          :: iwan
   !
   ! input namelist
   !
   NAMELIST /INPUT/ work_dir, filein, measure, iwan

   !
   ! local variables
   !
   INTEGER      :: nfiles, narg
   CHARACTER(5) :: subname = "decay"
   CHARACTER(15):: str
   !
   INTEGER      :: i, j, ir, ierr
   INTEGER      :: dimwann_, nrtot_
   REAL(dbl)    :: rmod
   !
   INTEGER,       ALLOCATABLE :: ivr(:,:)
   REAL(dbl),     ALLOCATABLE :: vr(:,:), rtmp(:,:)
   REAL(dbl),     ALLOCATABLE :: decay(:)
   COMPLEX(dbl),  ALLOCATABLE :: opr(:,:,:), opr_aux(:,:,:)
   !
   INTEGER, EXTERNAL :: iargc
   !
   ! end of declariations
   !

!
!------------------------------
! main body
!------------------------------
!
      CALL startup(version_number,'decay')

!
! ... Read INPUT namelist from stdin
!
      work_dir                    = " "
      measure                     = "norm2"
      iwan                        = 0
      !
      DO i = 1, nfilex
          filein(i)  = " "
      ENDDO

      !CALL input_from_file ( stdin )
      !
      READ(stdin, INPUT, IOSTAT=ierr)
      IF ( ierr /= 0 )  CALL errore(subname,'Unable to read namelist INPUT',ABS(ierr))

      !
      ! Some checks
      !
      CALL change_case(measure,'lower')
      IF ( TRIM(measure) /= "norm2"        .AND. TRIM(measure) /= "diag" .AND. &
           TRIM(measure) /= "diag-element" .AND. TRIM(measure) /= "max" ) &
           CALL errore(subname, 'Invalid measure = '//TRIM(measure), 4)

      IF ( TRIM(measure) == "diag-element" .AND. iwan <= 0 ) &
           CALL errore(subname, 'Invalid iwan', 4)

      !
      ! loop over arguments
      !
      narg = iargc ()
      !
      IF ( narg == 0 ) THEN
          ! 
          ! files are given from input
          ! 
          nfiles = 0
          file_loop:&
          DO i = 1, nfilex
              IF ( LEN_TRIM( filein(i)) /= 0 ) THEN 
                  nfiles = nfiles+1
                  IF ( LEN_TRIM(work_dir) /= 0 ) &
                       filein(i) = TRIM( work_dir) // '/' // TRIM( filein(i) )
              ELSE
                  EXIT file_loop
              ENDIF
          ENDDO file_loop  
          !
      ELSE
          ! 
          ! files are given as arguments
          ! 
          nfiles = narg
          IF ( nfiles > nfilex) CALL errore(subname,"nfile too large", nfiles)
          !
          DO i = 1, nfiles
              CALL getarg ( i, filein(i) )
              IF ( LEN_TRIM(work_dir) /= 0 ) &
                   filein(i) = TRIM( work_dir) // '/' // TRIM( filein(i) )
          ENDDO
          !
      ENDIF      
 

      !
      ! summary of the input
      !
      CALL write_header( stdout, "Computing operator decay" )
      !
      WRITE( stdout,"(  2x,'   decay measure :',3x,a)") TRIM(measure)
      IF ( TRIM(measure) == "diag_element" ) &
         WRITE(stdout,"(2x,'            iwan :',3x,i5)") iwan
      WRITE( stdout,"(  2x,'        work_dir :',3x,a)") TRIM(work_dir)
      WRITE( stdout,"(  2x,'          nfiles :',3x,i5)") nfiles
      WRITE( stdout,"(/)")
      !
      WRITE(stdout, "(2x,'Files:')")
      DO i = 1, nfiles
         WRITE(stdout,"(2x,'   file(',i3,' ) = ',3x,a)") i, TRIM(filein(i))
      ENDDO
      !
      IF ( nfiles == 0 ) CALL errore(subname,"no files from input",10)
      !
      CALL flush_unit( stdout )

      !
      ! further checks
      !
      DO i = 1, nfiles
          IF ( .NOT. file_exist( filein(i)) ) &
             CALL errore(subname,"file does not exist: "//TRIM(filein(i)), i) 
      ENDDO

      !
      ! get dimensions and general data from the first file
      !
      CALL decay_read_file( TRIM(filein(1)), DIMWANN=dimwann, NRTOT=nrtot, FILETYPE=str, IERR=ierr)
      IF ( ierr/=0 ) CALL errore(subname,"reading data I", ABS(ierr))
      !
      WRITE( stdout,"( 2/, 2x,'    reading from :',3x,a)") TRIM( filein(1) )
      WRITE( stdout,"(     2x,'       file type :',3x,a)") TRIM( str )
      WRITE( stdout,"(     2x,'         dimwann :',3x,i5)") dimwann
      WRITE( stdout,"(     2x,'           nrtot :',3x,i5)") nrtot
      WRITE( stdout, "()" )
      !
      IF ( .NOT. decay_valid_type(str) ) CALL errore(subname,"invalid file type: "//TRIM(str), 10)
      !
      !
      ALLOCATE( ivr(3, nrtot), vr(3,nrtot), STAT=ierr )
      IF ( ierr/=0 ) CALL errore(subname,"allocating ivr, vr", ABS(ierr))
      !
      ALLOCATE( opr(dimwann,dimwann, nrtot), STAT=ierr )
      IF ( ierr/=0 ) CALL errore(subname,"allocating opr", ABS(ierr))
      ALLOCATE( opr_aux(dimwann,dimwann, nrtot), STAT=ierr )
      IF ( ierr/=0 ) CALL errore(subname,"allocating opr_aux", ABS(ierr))
      !
      ALLOCATE( decay(nrtot), STAT=ierr )
      IF ( ierr/=0 ) CALL errore(subname,"allocating decay", ABS(ierr))
      ! 
      ! 
      CALL decay_read_file( TRIM(filein(1)), VR=vr, IVR=IVR, IERR=ierr)
      IF ( ierr/=0 ) CALL errore(subname,"reading data II", ABS(ierr))

      !
      ! report
      !
      DO ir=1,nrtot
          !
          WRITE( stdout, " (4x, 'R (', i5, ') =    ( ',3i5,' ) ')") &
                          ir, ( ivr(i,ir), i=1,3 )
      ENDDO
      !
      WRITE( stdout, "(/)" )
     

      !
      ! reading the main data
      !
      opr = CZERO
      WRITE( stdout, "(/)" )
      !
      DO i = 1, nfiles
          !
          CALL decay_read_file( TRIM(filein(i)), DIMWANN=dimwann_, NRTOT=NRTOT_, &
                                OPR=opr_aux, FILETYPE=str, IERR=ierr)
          IF ( ierr/=0 ) CALL errore(subname,"reading data III", ABS(ierr))
          !
          IF ( dimwann_ /= dimwann ) CALL errore(subname,"invalid dimwann",i)
          IF ( nrtot_   /= nrtot_ )  CALL errore(subname,"invalid nrtot",i)
          !
          opr(:,:,:) = opr(:,:,:) + opr_aux(:,:,:)
          !
          WRITE( stdout,"( 2x,'    reading from :',3x,'( ',a,' )', 3x, a)") &
                 TRIM(str), TRIM( filein(i) )
          IF ( .NOT. decay_valid_type(str) ) CALL errore(subname,"invalid file type: "//TRIM(str), 10)
          !
      ENDDO
      !
      WRITE( stdout, "(/)" )


      !
      ! compute decay
      !
      ALLOCATE( rtmp(dimwann, dimwann), STAT=ierr )
      IF ( ierr/=0 ) CALL errore(subname,"allocating rtmp", ABS(ierr))
      !
      DO ir = 1, nrtot
          !
          decay(ir) = ZERO
          !
          DO j = 1, dimwann
          DO i = 1, dimwann
              rtmp(i,j) = REAL( opr(i,j,ir) * CONJG( opr(i,j,ir) ), dbl )
          ENDDO
          ENDDO
          !
          SELECT CASE ( TRIM(measure) )
          CASE ( "norm2" )
             !
             decay(ir) = SUM( rtmp(:,:) )
             !
          CASE ( "diag" )
             !
             DO i = 1, dimwann
                 decay(ir) = decay(ir) + rtmp(i,i)
             ENDDO
             !
          CASE ( "diag-element" )
             !
             decay(ir) = rtmp(iwan,iwan)
             !
          CASE ( "max" )
             !
             decay(ir) = MAXVAL( rtmp(:,:) )
             !
          CASE DEFAULT
             CALL errore(subname,'invalid measure: '//TRIM(measure), 10)
          END SELECT
          !
      ENDDO
      !
      DEALLOCATE( rtmp, STAT=ierr )
      IF ( ierr/=0 ) CALL errore(subname,"deallocating rtmp", ABS(ierr))


      !
      ! dump to file
      !
      WRITE(stdout,"( 2/, 4x,'#       R [cry]        |R| [Bohr]      Norm of H(R) [eV]')")
      ! 
      DO ir = 1, nrtot
          !
          rmod = SQRT( DOT_PRODUCT( vr(:,ir), vr(:,ir) ))
          !
          WRITE(stdout,"(1x,i4,3x,3i4,3x,f15.9,3x,f15.9)") &
                   ir,ivr(:,ir), rmod, SQRT( decay(ir) / REAL(dimwann, dbl) )
          !
      ENDDO


      !
      ! cleanup
      !
      DEALLOCATE( vr, ivr, STAT=ierr)
      IF ( ierr/=0 ) CALL errore(subname,"deallocating ivr, vr", ABS(ierr))
      DEALLOCATE( opr, opr_aux, STAT=ierr)
      IF ( ierr/=0 ) CALL errore(subname,"deallocating opr, opr_aux", ABS(ierr))
      DEALLOCATE( decay, STAT=ierr)
      IF ( ierr/=0 ) CALL errore(subname,"deallocating decay", ABS(ierr))

      !
      ! finalize
      !
      CALL shutdown( subname )


END PROGRAM decay_prog

