!
! Copyright (C) 2017 WanT Group
!
! This file is distributed under the terms of the
! GNU General Public License. See the file `License'
! in the root directory of the present distribution,
! or http://www.gnu.org/copyleft/gpl.txt .
!
!**********************************************************
   SUBROUTINE io_init( need_wfc )
   !**********************************************************
   !
   ! init some data related to IO and taken from input
   !
   USE io_module
   USE parameters,              ONLY : nstrx
   USE control_module,          ONLY : read_symmetry, use_pseudo, read_pseudo, use_debug_mode, debug_level 
   USE files_module,            ONLY : file_open, file_close
   USE log_module,              ONLY : log_init, log_push, log_pop
   !
   USE crystal_io_module,       ONLY : crio_open_file, crio_read_header, crio_close_file
   USE crystal_tools_module,    ONLY : file_is_crystal
   USE wannier90_tools_module,  ONLY : file_is_wannier90
   USE internal_tools_module,   ONLY : file_is_internal
   USE qexml_module
   USE qexpt_module
   !
#ifdef __ETSF_IO
   USE etsf_io
   USE etsf_io_tools
   USE etsf_io_data_module,  ONLY : ncid, lstat, dims, error_data, etsf_io_bcast
#endif
   !
   IMPLICIT NONE
      !
      LOGICAL, OPTIONAL, INTENT(IN) :: need_wfc
      !
      INTEGER           :: ierr
      LOGICAL           :: lneed_wfc
      CHARACTER(7)      :: subname="io_init"
      CHARACTER(nstrx)  :: dirname, logfile, filename

      !
      lneed_wfc = .TRUE.
      IF ( PRESENT(need_wfc) ) lneed_wfc = need_wfc
      !
      !
      SELECT CASE ( TRIM(wantdata_fmt) ) 
      !
      CASE ( 'textual' )
           !
           wantdata_form   = 'formatted'
           wantdata_binary = .FALSE.
           !
      CASE ( 'binary' )
           !
           wantdata_form   = 'unformatted'
           wantdata_binary = .TRUE.
           !
      END SELECT
      
      !
      ! init libs for all fmts
      !
      dirname = TRIM(work_dir) // '/' // TRIM(prefix) // '.save/'
      CALL qexml_init( dft_unit, DIR=dirname )
      !
      dirname  = TRIM(work_dir) // '/' // TRIM(prefix) // '.export/'
      CALL qexpt_init( dft_unit, dirname )
      !
      !
      IF ( LEN_TRIM( dftdata_fmt ) == 0 ) THEN 
          !
          CALL io_get_dftdata_fmt ( prefix, work_dir, dftdata_file, dftdata_fmt, lneed_wfc )
          !
      ENDIF

      !
      ! if dftdata_fmt is still empty, it means no complete dtf dataset
      ! has been found
      !
      IF ( LEN_TRIM( dftdata_fmt ) == 0 ) &
           CALL errore(subname,'No DFT dataset found',1)


      SELECT CASE ( TRIM(dftdata_fmt) )
      !
      CASE ( 'qexml' )
           !
           suffix_qe_data = ".save/data-file.xml"
           dirname = TRIM(work_dir) // '/' // TRIM(prefix) // '.save/'
           filename = TRIM( dirname) // "data-file.xml"
           !
           CALL qexml_openfile( filename, "read", IERR=ierr )
           IF ( ierr/=0) CALL errore(subname,'opening dftdata file',ABS(ierr))
           !
           CALL qexml_read_header( FORMAT_VERSION=dftdata_fmt_version, IERR=ierr)
           IF ( ierr/=0) CALL errore(subname,'no Header in dftdata file',1)
           !
           CALL qexml_closefile ( "read", IERR=ierr )
           IF ( ierr/=0) CALL errore(subname,'closing dftdata file',ABS(ierr))
           !
      CASE ( 'pw_export' )
           !
           suffix_qe_data = ".export/index.xml"
           dirname  = TRIM(work_dir) // '/' // TRIM(prefix) // '.export/'
           filename = TRIM( dirname) // "index.xml"
           !
           CALL qexpt_openfile( filename, "read", IERR=ierr )
           IF ( ierr/=0) CALL errore(subname,'opening dftdata file',ABS(ierr))
           !
           CALL qexpt_read_header( FORMAT_VERSION=dftdata_fmt_version, IERR=ierr)
           !
           IF ( ierr /= 0 ) THEN 
                !
                dftdata_fmt_version = "0.0.0" 
                read_symmetry = .FALSE.
                !
           ENDIF
           !
           CALL qexpt_closefile ( "read", IERR=ierr )
           IF ( ierr/=0) CALL errore(subname,'closing dftdata file',ABS(ierr))
           !
      CASE ( 'etsf_io' )
           !
#ifdef __ETSF_IO
           !
           dirname  = TRIM(work_dir) // '/' // TRIM(prefix) 
           filename = TRIM( dirname) // TRIM(suffix_etsf_io_data)

           CALL etsf_io_low_open_read(ncid, filename, lstat, &
                                      ERROR_DATA=error_data, &
                                      VERSION_MIN=etsf_io_version_min)
           IF (.NOT. lstat) &
               CALL etsf_error(error_data,subname,'opening '//TRIM(filename),1)
           !
           CALL etsf_io_dims_get(ncid, dims, lstat, error_data)
           IF (.NOT. lstat) CALL etsf_error(error_data,subname,'reading dims',1)
           !
           ! To be fixed
           dftdata_fmt_version="1.0.0"
           !
           CALL etsf_io_low_close(ncid, lstat, error_data)
           IF (.NOT. lstat) CALL etsf_error(error_data,subname,'closing '//TRIM(filename),1)

           !
           ! switch off pseudo readin and bcast dims
           !
           use_pseudo  = .FALSE.
           read_pseudo = .FALSE.
           !
#else
           CALL errore(subname,'ETSF_IO fmt not configured', 10 )
#endif
           !
      CASE ( 'crystal' )
           !
           filename = TRIM( dftdata_file )
           !
           CALL crio_open_file( UNIT=dft_unit, FILENAME=filename, ACTION='read', IERR=ierr )
           IF ( ierr/=0 ) CALL errore(subname, 'opening dftdata file: '//TRIM(filename), ABS(ierr) )
           !
           CALL crio_read_header( CREATOR_VERSION=dftdata_fmt_version, IERR=ierr)
           IF ( ierr/=0 ) CALL errore(subname, 'reading CRIO header', ABS(ierr) )
           !
           CALL crio_close_file( ACTION='read', IERR=ierr )
           IF ( ierr/=0) CALL errore(subname,'closing crio datafile',ABS(ierr))
           !
      CASE ( 'wannier90' )
           !
           dftdata_fmt_version = 'unknown'
           !
      CASE ( 'internal' )
           !
           dftdata_fmt_version = 'unknown'
           !
      CASE DEFAULT
           !
           CALL errore(subname,'invalid dftdata_fmt = '//TRIM(dftdata_fmt),1)
      END SELECT
      !
      !
      ! init writing to logfile if required
      !
      CALL io_name ( "log", logfile )
      CALL log_init( log_unit, use_debug_mode, logfile, debug_level )
      CALL log_push( "main" )
      !
      alloc = .TRUE.
      !
   END SUBROUTINE io_init


!**********************************************************
   SUBROUTINE io_get_dftdata_fmt(prefix_, work_dir_, dftdata_file_, dftdata_fmt_, lneed_wfc)
   !**********************************************************
   !
   ! get the fmt of the dftdata file (use the names)
   !
   USE io_module
   USE parameters,              ONLY : nstrx
   USE crystal_tools_module,    ONLY : file_is_crystal
   USE wannier90_tools_module,  ONLY : file_is_wannier90
   USE internal_tools_module,   ONLY : file_is_internal
   USE qexml_module
   USE qexpt_module
   USE mp,                      ONLY : mp_bcast
   !
#ifdef __ETSF_IO
   USE etsf_io
   USE etsf_io_tools
   USE etsf_io_data_module,  ONLY : ncid, lstat, dims, error_data, etsf_io_bcast
#endif
   !
   IMPLICIT NONE
      CHARACTER(*),      INTENT(IN)  :: prefix_, work_dir_, dftdata_file_
      CHARACTER(*),      INTENT(OUT) :: dftdata_fmt_
      LOGICAL,           INTENT(IN)  :: lneed_wfc
      !
      CHARACTER(18)    :: subname='io_get_dftdata_fmt'  
      !
      CHARACTER(nstrx) :: filename, version
      CHARACTER(nstrx) :: fmt_searched(6)
      CHARACTER(nstrx) :: fmt_filename(6)
      LOGICAL          :: lfound, lfound1
      INTEGER          :: i, ierr

      !
      ! Setting fmts to be searched
      !
      fmt_searched(1) = 'internal'
      fmt_searched(2) = 'wannier90'
      fmt_searched(3) = 'crystal'
      fmt_searched(4) = 'qexml'
      fmt_searched(5) = 'pw_export'
      fmt_searched(6) = 'etsf_io'
      !
      fmt_filename(1) = TRIM(dftdata_file_)
      fmt_filename(2) = TRIM(dftdata_file_)
      fmt_filename(3) = TRIM(dftdata_file_)
      fmt_filename(4) = '.save/data-file.xml'
      fmt_filename(5) = '.export/index.xml'
      fmt_filename(6) = '_WFK-etsf.nc'

      !
      ! init
      lfound    = .FALSE.
      !
      !
      DO i = 1, SIZE( fmt_searched )
           !
           ! set the filename
           !
           IF ( ionode ) WRITE(stdout, "(2x, 'checking for fmt ',a,'... ')", advance='no' ) &
                         TRIM( fmt_searched(i) )
           !
           IF ( TRIM( fmt_searched(i) ) == 'crystal'    .OR.  &
                TRIM( fmt_searched(i) ) == 'wannier90'  .OR.  &
                TRIM( fmt_searched(i) ) == 'internal'   ) THEN
               !
               ! in these cases, the presence of 
               ! a non-null dftdata_file is required
               !
               IF ( LEN_TRIM(fmt_filename( i )) == 0 ) THEN
                   IF ( ionode ) WRITE(stdout, "('no')" ) 
                   CYCLE
               ENDIF
               !
               filename = TRIM( fmt_filename( i ) )
               !
           ELSE
               filename = TRIM( work_dir_ ) //'/'// TRIM(prefix_) // TRIM( fmt_filename( i ) )
           ENDIF
           !
           ! check the existence of the file
           IF (ionode) INQUIRE ( FILE=TRIM(filename), EXIST=lfound )
           CALL mp_bcast( lfound,   ionode_id )
           !
           IF ( lfound .AND. lneed_wfc .AND. TRIM( fmt_searched(i) ) == 'qexml'  )  THEN
               !
               ! check also the existence of evc.dat or evc1.dat
               ! this means that file produced by espresso are fine for WanT
               !
               filename = TRIM( work_dir_ ) //'/'// TRIM(prefix_) // ".save/K00001/evc.dat"
               IF (ionode) INQUIRE ( FILE=TRIM(filename), EXIST=lfound )
               CALL mp_bcast( lfound,   ionode_id )
               !
               filename = TRIM( work_dir_ ) //'/'// TRIM(prefix_) // ".save/K00001/evc1.dat"
               IF (ionode) INQUIRE ( FILE=TRIM(filename), EXIST=lfound1 )
               CALL mp_bcast( lfound1,   ionode_id )
               !            
               lfound = lfound .OR. lfound1
               !
               !
               ! check  the version of the format.
               ! At the moment, if the header section exist, 
               ! the fmt is supported whatever version
               !
               filename = TRIM(work_dir_) //'/'// TRIM(prefix_) // '.save/data-file.xml'
               !
               IF (ionode) CALL qexml_openfile( filename, "read", IERR=ierr )
               CALL mp_bcast( ierr,   ionode_id )
               IF ( ierr /= 0 ) CALL errore(subname,'opening dftdata file',ABS(ierr))
               !
               IF (ionode) CALL qexml_read_header( FORMAT_VERSION=version, IERR=ierr )
               CALL mp_bcast( ierr,   ionode_id )
               CALL mp_bcast( version,   ionode_id )
               !
               IF ( ierr /= 0 )  lfound = .FALSE.
               !
               ! any check on the version should be placed here
               !
               IF (ionode) CALL qexml_closefile ( "read", IERR=ierr )
               CALL mp_bcast( ierr,   ionode_id )
               IF ( ierr /= 0 ) CALL errore(subname,'closing dftdata file',ABS(ierr))
               !
           ENDIF
           !
           IF ( lfound .AND. TRIM( fmt_searched(i) ) == 'internal'  )  THEN
               !
               lfound = file_is_internal( filename )
               !
           ENDIF
           !
           IF ( lfound .AND. TRIM( fmt_searched(i) ) == 'crystal'  )  THEN
               !
               lfound = file_is_crystal( filename )
               !
           ENDIF
           !
           IF ( lfound .AND. TRIM( fmt_searched(i) ) == 'wannier90'  )  THEN
               !
               lfound = file_is_wannier90( filename )
               !
           ENDIF
           !
#ifdef __ETSF_IO
           IF ( lfound .AND. TRIM( fmt_searched(i) ) == 'etsf_io')  THEN
               !
               ! Try to open the file and to get dimensions to check that
               ! the file is actually ETSF_IO formatted
               !
               IF ( ionode ) THEN
                   CALL etsf_io_low_open_read(ncid, filename, lstat, &
                                              ERROR_DATA=error_data, &
                                              VERSION_MIN=etsf_io_version_min )
               ENDIF
               CALL mp_bcast( lstat,   ionode_id )
               IF ( .NOT. lstat ) EXIT
               !
               IF ( ionode ) THEN
                   CALL etsf_io_dims_get(ncid, dims, lstat, error_data)
               ENDIF
               CALL mp_bcast( lstat,   ionode_id )
               IF ( .NOT. lstat ) EXIT
               !
               IF ( ionode ) THEN
                   CALL etsf_io_low_close(ncid, lstat, error_data)
               ENDIF
               CALL mp_bcast( lstat,   ionode_id )
               !
               lfound = lstat
               !
           ENDIF
#endif
           !
           IF ( lfound ) THEN
               IF ( ionode ) WRITE(stdout, "('ok')" ) 
               EXIT
           ELSE
               IF ( ionode ) WRITE(stdout, "('no')" ) 
           ENDIF
           !
      ENDDO
      !
      IF ( .NOT. lfound ) THEN
           dftdata_fmt_ = ""
      ELSE
           dftdata_fmt_ = TRIM( fmt_searched( i ) )
           !
           IF (ionode) WRITE( stdout , "(/,2x, 'DFT-data fmt automaticaly detected: ',a )" ) &
                  TRIM( dftdata_fmt_)
           !
      ENDIF
      !
   END SUBROUTINE io_get_dftdata_fmt
      

!**********************************************************
   SUBROUTINE io_open_dftdata( lserial )
   !**********************************************************
   !
   ! open the dftdata files, either for a serial or a parallel read 
   !
   USE io_module
   USE parameters,              ONLY : nstrx
   USE crystal_io_module,       ONLY : crio_open_file
   USE files_module,            ONLY : file_open, file_close
   USE log_module,              ONLY : log_push, log_pop
   !
#ifdef __ETSF_IO
   USE etsf_io
   USE etsf_io_tools
   USE etsf_io_data_module,  ONLY : ncid, lstat, dims, error_data, etsf_io_bcast
#endif
   !
   IMPLICIT NONE
      !
      LOGICAL,     INTENT(IN) :: lserial
      !
      CHARACTER(15)    :: subname='io_open_dftdata'
      CHARACTER(nstrx) :: filename
      INTEGER          :: ierr
      
      
      IF ( lserial .AND. .NOT. ionode ) RETURN
      !
      CALL log_push(subname) 
      ! 
      !
      CALL io_name('dft_data',filename)
      !
      SELECT CASE ( TRIM(dftdata_fmt) )
      CASE ( 'qexml', 'pw_export' )
          !
          CALL file_open(dft_unit,TRIM(filename),PATH="/",ACTION="read", IERR=ierr )
          IF ( ierr/=0 ) CALL errore(subname, 'QEXML-PWEXP: opening '//TRIM(filename), ABS(ierr)) 
          !
      CASE ( 'etsf_io' )
          !
#ifdef __ETSF_IO
          !
          CALL etsf_io_low_open_read(ncid, TRIM(filename), lstat,  &
                                     ERROR_DATA=error_data,        &
                                     VERSION_MIN=etsf_io_version_min)   
          IF (.NOT. lstat) CALL etsf_error(error_data,subname,'ETSF_IO: opening '//TRIM(filename), 10) 
          !
#else
          CALL errore(subname,'ETSF_IO not configured',10)
#endif
          !
      CASE ( 'crystal' )
          !
          CALL crio_open_file( dft_unit, TRIM(filename), ACTION='read', IERR=ierr)
          IF ( ierr/=0 ) CALL errore(subname, 'CRIO: opening '//TRIM(filename), ABS(ierr)) 
          !
      CASE ( 'wannier90', 'internal' )
          !
          ! nothing to do
          !
      CASE DEFAULT 
          CALL errore(subname,'invalid dftdata_fmt = '//TRIM(dftdata_fmt),10)
      END SELECT
      !
      CALL log_pop(subname)
      RETURN
      !
  END SUBROUTINE io_open_dftdata

          
!**********************************************************
   SUBROUTINE io_close_dftdata( lserial )
   !**********************************************************
   !
   ! close the dftdata files, either after a serial or a parallel read 
   !
   USE io_module
   USE crystal_io_module,       ONLY : crio_close_file
   USE files_module,            ONLY : file_open, file_close
   USE log_module,              ONLY : log_push, log_pop
   !
#ifdef __ETSF_IO
   USE etsf_io
   USE etsf_io_tools
   USE etsf_io_data_module,  ONLY : ncid, lstat, dims, error_data, etsf_io_bcast
#endif
   !
   IMPLICIT NONE
      !
      LOGICAL,     INTENT(IN) :: lserial
      !
      CHARACTER(16)    :: subname='io_close_dftdata'
      INTEGER          :: ierr
      
      
      IF ( lserial .AND. .NOT. ionode ) RETURN
      !
      CALL log_push(subname) 
      ! 
      !
      SELECT CASE ( TRIM(dftdata_fmt) )
      CASE ( 'qexml', 'pw_export' )
          !
          CALL file_close(dft_unit,PATH="/",ACTION="read", IERR=ierr )
          IF ( ierr/=0 ) CALL errore(subname, 'QEXML-PWEXP: closing DFT datafile', ABS(ierr)) 
          !
      CASE ( 'etsf_io' )
          !
#ifdef __ETSF_IO
          !
          CALL etsf_io_low_close(ncid, lstat, error_data)
          IF (.NOT. lstat) CALL etsf_error(error_data,subname,'ETSF_IO: closing DFT datafile', 10) 
          !
#else
          CALL errore(subname,'ETSF_IO not configured',10)
#endif
          !
      CASE ( 'crystal' )
          !
          CALL crio_close_file( ACTION='read', IERR=ierr)
          IF ( ierr/=0 ) CALL errore(subname, 'CRIO: closing DFT datafile', ABS(ierr)) 
          !
      CASE ( 'wannier90', 'internal' )
          !
          ! nothing to do
          !
      CASE DEFAULT 
          CALL errore(subname,'invalid dftdata_fmt = '//TRIM(dftdata_fmt),10)
      END SELECT
      !
      CALL log_pop(subname) 
      RETURN
      !
  END SUBROUTINE io_close_dftdata

