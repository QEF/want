!
! Copyright (C) 2004 WanT Group
!
! This file is distributed under the terms of the
! GNU General Public License. See the file `License'
! in the root directory of the present distribution,
! or http://www.gnu.org/copyleft/gpl.txt .
!
!*********************************************
   MODULE io_module
!*********************************************
   USE parameters,       ONLY : nstrx
   USE io_global_module, ONLY : stdout, stdin, ionode, ionode_id, &
                                io_global_start, io_global_getionode
   USE control_module,   ONLY : read_symmetry, use_debug_mode, debug_level
   USE log_module,       ONLY : log_init, log_push
   USE qexml_module
   USE qexpt_module
   USE iotk_module
   IMPLICIT NONE
   PRIVATE
   SAVE
   !
   ! Contains basic data concerning IO
   ! 
   ! contains:
   ! SUBROUTINE  io_init()
   ! SUBROUTINE  io_name(data,filename[,lpostfix][,lpath])
   ! SUBROUTINE  io_read_data(unit,name,prefix,postfix,work_dir,title,found)
   ! SUBROUTINE  io_write_data(unit,name,prefix,postfix,work_dir,title)
   !
   ! DATA in io_name routine should be:
   ! 
   ! * 'dft_data'
   ! * 'overlap_projection'
   ! * 'space'
   ! * 'wannier'
   ! * 'hamiltonian'
   ! * 'save'
   ! * 'log'
   !
   INTEGER, PARAMETER         ::   &
       dft_unit = 10,              &! input file (DFT data) unit
    pseudo_unit = 11,              &! input pseudopotential data unit
       log_unit = 12,              &! input pseudopotential data unit
       ovp_unit = 20,              &! overlap and projections unit
     space_unit = 21,              &! space unit
       wan_unit = 22,              &! wannier stuff unit
       ham_unit = 23,              &! hamiltonian unit
       aux_unit = 30,              &! auxiliary units
      aux1_unit = 31,              &! 
      aux2_unit = 32,              &! 
      aux3_unit = 33,              &! 
      aux4_unit = 34,              &! 
      save_unit = 60                ! restart file unit


   CHARACTER(7), PARAMETER    ::  suffix_space=".space"
   CHARACTER(4), PARAMETER    ::  suffix_ovp=".ovp"
   CHARACTER(4), PARAMETER    ::  suffix_wannier=".wan"
   CHARACTER(4), PARAMETER    ::  suffix_hamiltonian=".ham"
   CHARACTER(5), PARAMETER    ::  suffix_save=".save"
   CHARACTER(4), PARAMETER    ::  suffix_log=".log"
   CHARACTER(nstrx)           ::  suffix_dft_data=" "
   
   CHARACTER(nstrx)           :: prefix
   CHARACTER(nstrx)           :: postfix
   CHARACTER(nstrx)           :: work_dir
   CHARACTER(nstrx)           :: title
   CHARACTER(nstrx)           :: pseudo_dir
         
   CHARACTER(nstrx)           :: dftdata_fmt = ' '
   CHARACTER(nstrx)           :: dftdata_fmt_version
   CHARACTER(nstrx)           :: wantdata_fmt
   CHARACTER(nstrx)           :: wantdata_form
   LOGICAL                    :: wantdata_binary

   LOGICAL                    :: alloc = .FALSE.
!
! end delcarations
!


   PUBLIC ::  io_global_start, io_global_getionode
   PUBLIC ::  ionode, ionode_id

   PUBLIC ::  stdin, stdout 
   PUBLIC ::  dftdata_fmt, dftdata_fmt_version
   PUBLIC ::  wantdata_fmt, wantdata_form, wantdata_binary
   PUBLIC ::  dft_unit, pseudo_unit 
   PUBLIC ::  ovp_unit, space_unit, wan_unit, ham_unit 
   PUBLIC ::  aux_unit, aux1_unit, aux2_unit, aux3_unit, aux4_unit
   PUBLIC ::  save_unit

   PUBLIC ::  prefix, postfix, work_dir, title, pseudo_dir
   PUBLIC ::  alloc
   PUBLIC ::  io_init
   PUBLIC ::  io_name
   PUBLIC ::  io_read_data
   PUBLIC ::  io_write_data


   CONTAINS

!
! subroutines
!
!**********************************************************
   SUBROUTINE get_dftdata_fmt(prefix_,work_dir_, dftdata_fmt_)
   !**********************************************************
      !
      ! get the fmt of the dftdata file (use the names)
      !
      IMPLICIT NONE
      CHARACTER(*),  INTENT(IN)  ::  prefix_, work_dir_
      CHARACTER(*),  INTENT(OUT) ::  dftdata_fmt_
      !
      CHARACTER(nstrx) :: filename, version
      CHARACTER(nstrx) :: fmt_searched(2)
      CHARACTER(nstrx) :: fmt_filename(2)
      LOGICAL          :: lexist, lexist1
      INTEGER          :: i, ierr
      !
      ! Setting fmts to be searched
      !
      DATA fmt_searched /'qexml', 'pw_export'  /
      DATA fmt_filename /'.save/data-file.xml', '.export/index.xml'  /
      !
      lexist = .FALSE.
      !
      DO i = 1, SIZE( fmt_searched )
           !
           filename = TRIM( work_dir_ ) //'/'// TRIM(prefix_) // TRIM( fmt_filename( i ) )
           INQUIRE ( FILE=TRIM(filename), EXIST=lexist )
           !
           IF ( lexist .AND. TRIM( fmt_searched(i) ) == 'qexml'  )  THEN
                !
                ! check olso the existence of evc.dat or evc1.dat
                ! this means that file produced by espresso are usable by WanT
                !
                filename = TRIM( work_dir_ ) //'/'// TRIM(prefix_) // ".save/K00001/evc.dat"
                INQUIRE ( FILE=TRIM(filename), EXIST=lexist )
                filename = TRIM( work_dir_ ) //'/'// TRIM(prefix_) // ".save/K00001/evc1.dat"
                INQUIRE ( FILE=TRIM(filename), EXIST=lexist1 )
                !            
                lexist = lexist .OR. lexist1
                !
                !
                ! check  the version of the format.
                ! At the moment, if the header section exist, 
                ! the fmt is supported whatever version
                !
                filename = TRIM(work_dir_) //'/'// TRIM(prefix_) // '.save/data-file.xml'
                !
                CALL qexml_openfile( filename, "read", IERR=ierr )
                IF ( ierr /= 0 ) CALL errore('get_dftdata_fmt','opening dftdata file',ABS(ierr))
                !
                CALL qexml_read_header( FORMAT_VERSION=version, IERR=ierr )
                !
                IF ( ierr /= 0 )  lexist = .FALSE.
                !
                ! any check on the version should be placed here
                !
                CALL qexml_closefile ( "read", IERR=ierr )
                IF ( ierr /= 0 ) CALL errore('get_dftdata_fmt','closing dftdata file',ABS(ierr))
                !
           ENDIF
           !
           IF ( lexist ) EXIT
      ENDDO
      !
      IF ( .NOT. lexist ) THEN
           dftdata_fmt_ = ""
      ELSE
           dftdata_fmt_ = TRIM( fmt_searched( i ) )
           !
           WRITE( stdout , "(2x, 'DFT-data fmt automaticaly detected: ',a )" ) &
                  TRIM( dftdata_fmt_)
           !
      ENDIF
      !
   END SUBROUTINE get_dftdata_fmt
      

!**********************************************************
   SUBROUTINE io_init()
   !**********************************************************
   !
   ! init some data related to IO and taken from input
   !
   IMPLICIT NONE
      INTEGER           :: ierr
      CHARACTER(nstrx)  :: dirname, logfile, filename
      !
      ionode = .TRUE.
      ionode_id = 0
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
      CALL qexml_init( dft_unit, dirname, aux_unit )
      !
      dirname  = TRIM(work_dir) // '/' // TRIM(prefix) // '.export/'
      CALL qexpt_init( dft_unit, dirname )
      !
      !
      IF ( LEN_TRIM( dftdata_fmt ) == 0 ) THEN 
           !
           CALL get_dftdata_fmt ( prefix, work_dir, dftdata_fmt )
      ENDIF
      !
      !
      SELECT CASE ( TRIM(dftdata_fmt) )
      !
      CASE ( 'qexml' )
           !
           suffix_dft_data = ".save/data-file.xml"
           dirname = TRIM(work_dir) // '/' // TRIM(prefix) // '.save/'
           filename = TRIM( dirname) // "data-file.xml"
           !
           CALL qexml_openfile( filename, "read", IERR=ierr )
           IF ( ierr/=0) CALL errore('io_init','opening dftdata file',ABS(ierr))
           !
           CALL qexml_read_header( FORMAT_VERSION=dftdata_fmt_version, IERR=ierr)
           IF ( ierr/=0) CALL errore('io_init','no Header in dftdata file',1)
           !
           CALL qexml_closefile ( "read", IERR=ierr )
           IF ( ierr/=0) CALL errore('io_init','closing dftdata file',ABS(ierr))
           !
      CASE ( 'pw_export' )
           !
           suffix_dft_data = ".export/index.xml"
           dirname  = TRIM(work_dir) // '/' // TRIM(prefix) // '.export/'
           filename = TRIM( dirname) // "index.xml"
           !
           CALL qexpt_openfile( filename, "read", IERR=ierr )
           IF ( ierr/=0) CALL errore('io_init','opening dftdata file',ABS(ierr))
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
           IF ( ierr/=0) CALL errore('io_init','closing dftdata file',ABS(ierr))
           !
      CASE DEFAULT
           !
           CALL errore('io_init','invalid dftdata_fmt = '//TRIM(dftdata_fmt),1)
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
   SUBROUTINE io_name(data,filename,lpostfix,lpath)
   !**********************************************************
      IMPLICIT NONE
      CHARACTER(*),       INTENT(in)  :: data
      CHARACTER(*),       INTENT(out) :: filename
      LOGICAL, OPTIONAL,  INTENT(in)  :: lpostfix, lpath   
      LOGICAL             :: lpostfix_, lpath_
      CHARACTER(nstrx)    :: path_, postfix_, suffix_
      INTEGER             :: length

      !
      ! DEFAULT
      lpostfix_ = .TRUE.
      lpath_ = .TRUE.
      IF ( PRESENT(lpostfix) ) lpostfix_ = lpostfix
      IF ( PRESENT(lpath) )     lpath_ = lpath
      
      !
      ! setting the base name
      path_ = " " 
      postfix_ = " " 
      IF ( lpath_ )  path_ = TRIM(work_dir)
      IF ( lpostfix_ ) postfix_ = TRIM(postfix)


      !
      ! add the / if needed
      length = LEN_TRIM( path_ )
      IF ( length /= 0 ) THEN
         IF ( path_(length:length) /= "/"  ) path_ = TRIM(path_)//"/"
      ENDIF

          

      SELECT CASE( TRIM(data) )
      CASE ( "dft_data" ) 
           suffix_ = TRIM(suffix_dft_data)
      CASE ( "space" ) 
           suffix_ = TRIM(suffix_space)
      CASE ( "overlap_projection" ) 
           suffix_ = TRIM(suffix_ovp)
      CASE ( "wannier" ) 
           suffix_ = TRIM(suffix_wannier)
      CASE ( "hamiltonian" )
           suffix_ = TRIM(suffix_hamiltonian)
      CASE ( "save" )
           suffix_ = TRIM(suffix_save)
      CASE ( "log" )
           suffix_ = TRIM(suffix_log)
      CASE DEFAULT
           CALL errore('io_name','Unknown DATA type in input',1)
      END SELECT

      filename = TRIM(path_)//TRIM(prefix)//TRIM(postfix_)//TRIM(suffix_)

  END SUBROUTINE io_name
   

!**********************************************************
   SUBROUTINE io_read_data(unit,name,prefix_,postfix_,work_dir_,title_,found)
   !**********************************************************
      IMPLICIT NONE
      INTEGER,         INTENT(in)   :: unit
      CHARACTER(*),    INTENT(in)   :: name
      LOGICAL,         INTENT(out)  :: found
      CHARACTER(*),    INTENT(out)  :: prefix_, postfix_, work_dir_, title_ 

      CHARACTER(12)                 :: sub_name='io_read_data'
      CHARACTER(nstrx)              :: attr
      INTEGER                       :: ierr

      CALL iotk_scan_empty(unit,name,FOUND=found,ATTR=attr,IERR=ierr)
      IF ( .NOT. found ) RETURN
      IF ( ierr > 0 ) CALL errore(sub_name,'Wrong format in tag '//TRIM(name),ierr)
      found = .TRUE.

      CALL iotk_scan_attr(attr,'prefix',prefix_,IERR=ierr)
         IF (ierr /= 0) CALL errore(sub_name,'Wrong input format in PREFIX',ABS(ierr))
      CALL iotk_scan_attr(attr,'postfix',postfix_,IERR=ierr)
         IF (ierr /= 0) CALL errore(sub_name,'Wrong input format in POSTFIX',ABS(ierr))
      CALL iotk_scan_attr(attr,'work_dir',work_dir_,IERR=ierr)
         IF (ierr /= 0) CALL errore(sub_name,'Wrong input format in WORK_DIR',ABS(ierr))
      CALL iotk_scan_attr(attr,'title',title_,IERR=ierr)
         IF (ierr /= 0) CALL errore(sub_name,'Wrong input format in TITLE',ABS(ierr))

   END SUBROUTINE io_read_data


!**********************************************************
   SUBROUTINE io_write_data(unit,name,prefix_,postfix_,work_dir_,title_)
   !**********************************************************
      IMPLICIT NONE
      INTEGER,         INTENT(in)   :: unit
      CHARACTER(*),    INTENT(in)   :: name
      CHARACTER(*),    INTENT(in)   :: prefix_, postfix_, work_dir_, title_ 

      CHARACTER(nstrx)              :: attr

      CALL iotk_write_attr(attr,"prefix",TRIM(prefix_),FIRST=.TRUE.)
      CALL iotk_write_attr(attr,"postfix",TRIM(postfix_),FIRST=.TRUE.)
      CALL iotk_write_attr(attr,"work_dir",TRIM(work_dir_))
      CALL iotk_write_attr(attr,"title",TRIM(title_))
      CALL iotk_write_empty(unit,name,ATTR=attr)

   END SUBROUTINE io_write_data



END MODULE io_module

