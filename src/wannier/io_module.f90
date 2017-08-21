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
   !
   USE kinds,                   ONLY : dbl
   USE parameters,              ONLY : nstrx
   USE io_global_module,        ONLY : stdout, stdin, stderr, ionode, ionode_id, &
                                       io_global_start, io_global_getionode
   USE mp_global,               ONLY : nproc, mpime
   USE iotk_module
   !
   IMPLICIT NONE
   PRIVATE
   SAVE
   !
   ! Contains basic data concerning IO
   ! 
   ! contains:
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
       sgm_unit = 24,              &! self-energy unit
       aux_unit = 30,              &! auxiliary units
      aux1_unit = 31,              &! 
      aux2_unit = 32,              &! 
      aux3_unit = 33,              &! 
      aux4_unit = 34,              &! 
      aux5_unit = 35,              &! 
      save_unit = 60                ! restart file unit


   CHARACTER( 6), PARAMETER   ::  suffix_space=".space"
   CHARACTER( 4), PARAMETER   ::  suffix_ovp=".ovp"
   CHARACTER( 7), PARAMETER   ::  suffix_transl=".transl"
   CHARACTER( 4), PARAMETER   ::  suffix_wannier=".wan"
   CHARACTER( 4), PARAMETER   ::  suffix_hamiltonian=".ham"
   CHARACTER( 5), PARAMETER   ::  suffix_save=".save"
   CHARACTER( 4), PARAMETER   ::  suffix_log=".log"
   CHARACTER( 4), PARAMETER   ::  suffix_sgm=".sgm"
   CHARACTER( 3), PARAMETER   ::  suffix_gf=".gf"
   CHARACTER(nstrx)           ::  suffix_qe_data=" "
   CHARACTER(12), PARAMETER   ::  suffix_etsf_io_data="_WFK-etsf.nc"
   REAL,          PARAMETER   ::  etsf_io_version_min=2.1
   
   CHARACTER(nstrx)           :: prefix
   CHARACTER(nstrx)           :: postfix
   CHARACTER(nstrx)           :: work_dir
   CHARACTER(nstrx)           :: title
   CHARACTER(nstrx)           :: pseudo_dir
         
   CHARACTER(nstrx)           :: dftdata_fmt = ' '
   CHARACTER(nstrx)           :: dftdata_fmt_version
   CHARACTER(nstrx)           :: dftdata_file = ' '
   CHARACTER(nstrx)           :: wantdata_fmt
   CHARACTER(nstrx)           :: wantdata_form
   LOGICAL                    :: wantdata_binary
   !
   CHARACTER(nstrx)           :: datafile_sgm = ' '
   CHARACTER(nstrx)           :: datafile_qp = ' '

   LOGICAL                    :: alloc = .FALSE.
!
! end delcarations
!

   INTERFACE
      SUBROUTINE io_init( need_wfc )
         IMPLICIT NONE
         LOGICAL, OPTIONAL, INTENT(IN) :: need_wfc
      END SUBROUTINE
      SUBROUTINE io_open_dftdata( lserial )
         IMPLICIT NONE
         LOGICAL,     INTENT(IN) :: lserial
      END SUBROUTINE
      SUBROUTINE io_close_dftdata( lserial )
         IMPLICIT NONE
         LOGICAL,     INTENT(IN) :: lserial
      END SUBROUTINE
   END INTERFACE

   PUBLIC ::  io_global_start, io_global_getionode
   PUBLIC ::  mpime, ionode, ionode_id

   PUBLIC ::  stderr, stdin, stdout 
   PUBLIC ::  dftdata_fmt, dftdata_fmt_version, dftdata_file
   PUBLIC ::  datafile_sgm, datafile_qp
   PUBLIC ::  wantdata_fmt, wantdata_form, wantdata_binary
   PUBLIC ::  dft_unit, pseudo_unit 
   PUBLIC ::  ovp_unit, space_unit, wan_unit, ham_unit, sgm_unit 
   PUBLIC ::  aux_unit, aux1_unit, aux2_unit, aux3_unit, aux4_unit, aux5_unit
   PUBLIC ::  save_unit, log_unit
   PUBLIC ::  etsf_io_version_min
   !
   PUBLIC ::  suffix_space, suffix_ovp, suffix_transl, suffix_wannier
   PUBLIC ::  suffix_hamiltonian, suffix_save, suffix_log, suffix_sgm, suffix_gf
   PUBLIC ::  suffix_qe_data, suffix_etsf_io_data
   !
   PUBLIC ::  prefix, postfix, work_dir, title, pseudo_dir
   PUBLIC ::  alloc
   PUBLIC ::  io_init
   PUBLIC ::  io_name
   PUBLIC ::  io_set_nd_nmbr
   PUBLIC ::  io_open_dftdata, io_close_dftdata
   PUBLIC ::  io_read_data
   PUBLIC ::  io_write_data

   CONTAINS


!**********************************************************
   SUBROUTINE io_name( data_type, filename, lpostfix, postfix_loc, lbody, body, lpath, lproc)
   !**********************************************************
   !
   IMPLICIT NONE
      !
      CHARACTER(*),            INTENT(IN)  :: data_type
      CHARACTER(*),            INTENT(OUT) :: filename
      CHARACTER(*),  OPTIONAL, INTENT(IN)  :: body, postfix_loc
      LOGICAL,       OPTIONAL, INTENT(IN)  :: lpostfix, lbody, lpath, lproc   
      !
      CHARACTER(7)        :: subname="io_name"
      LOGICAL             :: lpostfix_, lbody_, lpath_, lproc_
      CHARACTER(nstrx)    :: path_, prefix_, body_, postfix_, suffix_, proc_
      INTEGER             :: length

      !
      ! DEFAULT
      lpostfix_   = .TRUE.
      lpath_      = .TRUE.
      lproc_      = .TRUE.
      lbody_      = .FALSE.
      !
      IF ( PRESENT(lbody) )       lbody_ = lbody
      IF ( PRESENT(body) )         body_ = TRIM(body)
      IF ( PRESENT(body) )        lbody_ = .TRUE.
      IF ( PRESENT(lpostfix) ) lpostfix_ = lpostfix
      IF ( PRESENT(lpath) )       lpath_ = lpath
      IF ( PRESENT(lproc) )       lproc_ = lproc


      !
      ! setting the base name
      path_    = " " 
      body_    = " "
      postfix_ = " " 
      IF ( lpath_ )           path_ = TRIM(work_dir)
      IF ( lpostfix_ )     postfix_ = TRIM(postfix)
      IF ( lpostfix_ .AND. PRESENT( postfix_loc) ) &    
                           postfix_ = TRIM(postfix_loc)


      !
      ! add the / if needed
      length = LEN_TRIM( path_ )
      IF ( length /= 0 ) THEN
         IF ( path_(length:length) /= "/"  ) path_ = TRIM(path_)//"/"
      ENDIF

      !
      ! set data for parallelism
      proc_ = " "
      !
      IF ( nproc > 1 .AND. lproc_ ) THEN
         CALL io_set_nd_nmbr( proc_, mpime, nproc )
         proc_ = "."//TRIM(proc_)
      ENDIF

      !
      ! redefine prefix
      prefix_ = TRIM(prefix)
          

      SELECT CASE( TRIM(data_type) )
      CASE ( "dft_data" ) 
           !
           SELECT CASE ( TRIM(dftdata_fmt) ) 
           CASE ( 'qexml', 'pw_export' )
               !
               suffix_  = TRIM(suffix_qe_data)
               postfix_ = " "
               !
           CASE ( 'etsf_io' ) 
               !
               suffix_  = TRIM(suffix_etsf_io_data)
               postfix_ = " "
               !
           CASE ( 'crystal', 'wannier90', 'internal' ) 
               !
               path_    = " "
               prefix_  = " " 
               postfix_ = " "
               suffix_  = TRIM( dftdata_file )
               !
           CASE DEFAULT
               CALL errore(subname,'invalid DFTDATA_FMT = '//TRIM(dftdata_fmt),1)
           END SELECT
           !
      CASE ( "space" ) 
           !
           suffix_ = TRIM(suffix_space)
           !
      CASE ( "overlap_projection" ) 
           !
           suffix_ = TRIM(suffix_ovp)
           !
      CASE ( "translations" ) 
           !
           suffix_ = TRIM(suffix_transl)
           !
      CASE ( "wannier" ) 
           !
           suffix_ = TRIM(suffix_wannier)
           !
      CASE ( "hamiltonian", "ham" )
           !
           suffix_ = TRIM(suffix_hamiltonian) 
           !
      CASE ( "save" )
           !
           suffix_ = TRIM(suffix_save) // TRIM(proc_)
           !
      CASE ( "log" )
           !
           suffix_ = TRIM(suffix_log) // TRIM(proc_)
           IF ( lbody_ ) body_ = "debug"
           !
      CASE ( "conductance", "cond" )
           !
           body_   = "cond" 
           suffix_ = ".dat"
           !
      CASE ( "doscond" )
           !
           body_   = "doscond" 
           suffix_ = ".dat"
           !
      CASE ( "eigenchannels", "eigchn" )
           !
           body_   = "eigchn" 
           suffix_ = ".dat"
           !
      CASE ( "dos" )
           !
           body_   = "dos" 
           suffix_ = ".dat"
           !
      CASE ( "sgm" )
           !
           body_   = "sgm" 
           IF ( PRESENT(body) )  body_ = TRIM(body)
           suffix_ = TRIM(suffix_sgm) // TRIM(proc_)
           !
      CASE ( "gf" )
           !
           body_   = "greenf" 
           IF ( PRESENT(body) )  body_ = TRIM(body)
           suffix_ = TRIM(suffix_gf) // TRIM(proc_)
           !
      CASE ( "free" )
           !
           body_   = "" 
           IF ( PRESENT(body) )  body_ = TRIM(body)
           suffix_ = TRIM(proc_)
           !
      CASE DEFAULT
           CALL errore('io_name','Unknown DATA type in input: '//TRIM(data_type),1)
      END SELECT

      filename = TRIM(path_)//TRIM(prefix_)//TRIM(body_)//TRIM(postfix_)//TRIM(suffix_)

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
      !
      CALL iotk_scan_attr(attr,'postfix',postfix_,IERR=ierr)
      IF (ierr /= 0) CALL errore(sub_name,'Wrong input format in POSTFIX',ABS(ierr))
      !
      CALL iotk_scan_attr(attr,'work_dir',work_dir_,IERR=ierr)
      IF (ierr /= 0) CALL errore(sub_name,'Wrong input format in WORK_DIR',ABS(ierr))
      !
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

!
! Copyright (C) 2001-2008 Quantum-Espresso group
! This file is distributed under the terms of the
! GNU General Public License. See the file `License'
! in the root directory of the present distribution,
! or http://www.gnu.org/copyleft/gpl.txt .
!

!**********************************************************
   SUBROUTINE io_set_nd_nmbr( nd_nmbr, node_number, nproc_image )
   !**********************************************************
     !
     IMPLICIT NONE
     !
     CHARACTER(LEN=6), INTENT(OUT) :: nd_nmbr
     INTEGER, INTENT(IN) :: node_number
     INTEGER, INTENT(IN) :: nproc_image
     !
     INTEGER :: nmax, nleft, nfact, n
     !
     nd_nmbr = '      '
     nmax = INT ( LOG10 ( nproc_image + 1.0D-8 ) )
     !
     ! nmax+1=number of digits of nproc_image (number of processors)
     ! 1.0D-8 protects from rounding error if nproc_image is a power of 10
     !
     IF ( nmax+1 > LEN (nd_nmbr) ) &
        CALL errore ( "io_set_nd_nmbr", 'insufficient size for nd_nmbr', nmax)
     IF ( nmax < 0) &
        CALL errore ( "io_set_nd_nmbr", 'incorrect value for nproc_image', nmax)
     !
     nleft = node_number
     !
     DO n = nmax, 0, -1
        !
        ! decompose node_number (index of this process) into powers of 10:
        !    node_number = i*10^nmax+j*10^(nmax-1)+k*10^(nmax-2)...
        ! i,j,k,... can be equal to 0
        !
        nfact = INT ( nleft/10**n )
        IF ( nfact > 9 ) CALL errore ( "io_set_nd_nmbr", 'internal error', 1 )
        nleft = nleft - nfact*10**n
        !
        WRITE( nd_nmbr(nmax-n+1:nmax-n+1), '(I1)' ) nfact
        !
     END DO
     !
     IF ( nleft > 0 ) CALL errore ( "io_set_nd_nmbr", 'internal error', 2 )
     !
     RETURN
     !
  END SUBROUTINE io_set_nd_nmbr
  !
END MODULE io_module

