!
! Copyright (C) 2004 WanT Group
!
! This file is distributed under the terms of the
! GNU General Public License. See the file `License'
! in the root directory of the present distribution,
! or http://www.gnu.org/copyleft/gpl.txt .
!
! <INFO>
!*********************************************
   MODULE io_module
!*********************************************
   USE parameters, ONLY : nstrx
   USE iotk_module
   IMPLICIT NONE
   PRIVATE
   SAVE

!
! Contains basic data concerning IO
! 
! contains:
! SUBROUTINE  ioname(data,filename[,lpostfix][,lpath])
! SUBROUTINE  read_iodata(unit,name,prefix,postfix,work_dir,title,found)
! SUBROUTINE  write_iodata(unit,name,prefix,postfix,work_dir,title)
!
! DATA in ioname routine should be:
! 
! * 'dft_data'
! * 'export'
! * 'space'
! * 'wannier'
! * 'hamiltonian'
! * 'save'
!
! </INFO>
!
   INTEGER, PARAMETER         ::   &
          stdin = 5,               &! std input unit
         stdout = 6,               &! std output unit
       dft_unit = 10,              &! input file (DFT data) unit
    pseudo_unit = 11,              &! input pseudopotential data unit
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


   CHARACTER(4), PARAMETER    ::  suffix_dft_data=".dft"
   CHARACTER(17),PARAMETER    ::  suffix_export=".export/index.xml"
   CHARACTER(7), PARAMETER    ::  suffix_space=".space"
   CHARACTER(4), PARAMETER    ::  suffix_ovp=".ovp"
   CHARACTER(4), PARAMETER    ::  suffix_wannier=".wan"
   CHARACTER(4), PARAMETER    ::  suffix_hamiltonian=".ham"
   CHARACTER(5), PARAMETER    ::  suffix_save=".save"
   
   CHARACTER(nstrx)           :: prefix
   CHARACTER(nstrx)           :: postfix
   CHARACTER(nstrx)           :: work_dir
   CHARACTER(nstrx)           :: title
   CHARACTER(nstrx)           :: pseudo_dir
         
   INTEGER :: ionode_id = 0
   LOGICAL :: ionode = .TRUE.
   LOGICAL :: first = .TRUE.

!
! end delcarations
!


   PUBLIC ::  io_global_start, io_global_getionode
   PUBLIC ::  ionode, ionode_id

   PUBLIC ::  stdin, stdout 
   PUBLIC ::  dft_unit, pseudo_unit 
   PUBLIC ::  ovp_unit, space_unit, wan_unit, ham_unit 
   PUBLIC ::  aux_unit, aux1_unit, aux2_unit, aux3_unit, aux4_unit
   PUBLIC ::  save_unit

   PUBLIC ::  prefix, postfix, work_dir, title, pseudo_dir
   PUBLIC ::  ioname
   PUBLIC ::  read_iodata
   PUBLIC ::  write_iodata


   CONTAINS

!
! subroutines
!

!**********************************************************
   SUBROUTINE ioname(data,filename,lpostfix,lpath)
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
      CASE ( "export" ) 
           suffix_ = TRIM(suffix_export)
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
      CASE DEFAULT
           CALL errore('ioname','Unknown DATA type in input',1)
      END SELECT

      filename = TRIM(path_)//TRIM(prefix)//TRIM(postfix_)//TRIM(suffix_)

  END SUBROUTINE ioname
   

!**********************************************************
   SUBROUTINE read_iodata(unit,name,prefix_,postfix_,work_dir_,title_,found)
   !**********************************************************
      IMPLICIT NONE
      INTEGER,         INTENT(in)   :: unit
      CHARACTER(*),    INTENT(in)   :: name
      LOGICAL,         INTENT(out)  :: found
      CHARACTER(*),    INTENT(out)  :: prefix_, postfix_, work_dir_, title_ 

      CHARACTER(11)                 :: sub_name='read_iodata'
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

   END SUBROUTINE read_iodata


!**********************************************************
   SUBROUTINE write_iodata(unit,name,prefix_,postfix_,work_dir_,title_)
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

   END SUBROUTINE write_iodata

!
! Copyright (C) 2002 FPMD & PWSCF group
! This file is distributed under the terms of the
! GNU General Public License. See the file `License'
! in the root directory of the present distribution,
! or http://www.gnu.org/copyleft/gpl.txt .
!
     !
     !-----------------------------------------------------------------------
     SUBROUTINE io_global_start( mpime, ionode_set )
       !-----------------------------------------------------------------------
       !
       IMPLICIT NONE
       !
       INTEGER, INTENT(IN) :: mpime, ionode_set
       !
       !
       IF ( mpime == ionode_set ) THEN
          ionode = .TRUE.
       ELSE
          ionode = .FALSE.
       END IF
       !
       ionode_id = ionode_set
       first = .FALSE.
       !
       RETURN
       !
     END SUBROUTINE io_global_start
     !
     !
     !-----------------------------------------------------------------------
     SUBROUTINE io_global_getionode( ionode_out, ionode_id_out )
       !-----------------------------------------------------------------------
       !
       IMPLICIT NONE
       !
       LOGICAL, INTENT(OUT) :: ionode_out
       INTEGER, INTENT(OUT) :: ionode_id_out
       !
       !
       IF ( first ) &
          CALL errore( ' get_ionode ', ' ionode not yet defined ', 1 )
       ionode_out = ionode
       ionode_id_out = ionode_id
       !
       RETURN
       !
     END SUBROUTINE io_global_getionode

END MODULE io_module

