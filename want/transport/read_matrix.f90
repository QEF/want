!
!      Copyright (C) 2005 WanT Group
!
!      This file is distributed under the terms of the
!      GNU General Public License. See the file `License'
!      in the root directory of the present distribution,
!      or http://www.gnu.org/copyleft/gpl.txt .
!
!***************************************************************************
   SUBROUTINE read_matrix( filename, ispin, transport_dir, opr )
   !***************************************************************************
   !
   ! Read the required matrix block from FILEIN.
   ! The dimensions and all the related data are given through OPR.
   !
   USE kinds 
   USE parameters,           ONLY : nstrx
   USE constants,            ONLY : CZERO, CONE
   USE files_module,         ONLY : file_open, file_close
   USE io_module,            ONLY : stdin, aux_unit, ionode, ionode_id
   USE log_module,           ONLY : log_push, log_pop
   USE timing_module,        ONLY : timing
   USE mp,                   ONLY : mp_bcast
   USE T_kpoints_module,     ONLY : kpoints_alloc => alloc, nrtot_par, vr_par, nr_par
   USE T_operator_blc_module
   USE iotk_module
   USE parser_module
   !                                    
   IMPLICIT NONE

   ! 
   ! input variables
   !
   CHARACTER(*),         INTENT(IN)    :: filename
   INTEGER,              INTENT(IN)    :: ispin
   INTEGER,              INTENT(IN)    :: transport_dir
   TYPE( operator_blc ), INTENT(INOUT) :: opr

   !
   ! local variables
   !
   CHARACTER(11)             :: subname = 'read_matrix'
   INTEGER                   :: dim1, dim2
   INTEGER                   :: i, j, ierr
   !
   INTEGER                   :: ldimwann, nrtot, nspin, ir, ir_par
   INTEGER,      ALLOCATABLE :: ivr(:,:)
   COMPLEX(dbl), ALLOCATABLE :: A_loc(:,:), S_loc(:,:)
   COMPLEX(dbl), ALLOCATABLE :: A(:,:,:), S(:,:,:)
   CHARACTER(nstrx)          :: attr, str
   !
   LOGICAL                   :: found, ivr_from_input, lhave_ovp
   INTEGER                   :: ind, ivr_aux(3), ivr_input, nr_aux(3)
   INTEGER                   :: ncols, nrows
   CHARACTER(nstrx)          :: cols, rows
   CHARACTER(nstrx)          :: filein 

   !
   ! end of declarations
   !

!
!----------------------------------------
! main Body
!----------------------------------------
!
   CALL timing( subname, OPR='start' )
   CALL log_push( subname )

   !
   ! some checks
   !
   IF ( .NOT. kpoints_alloc ) CALL errore(subname, 'kpoints not alloc', 1 )
   IF ( .NOT. opr%alloc )     CALL errore(subname, 'opr not alloc',2)

   !
   ! parse tag (read from stdin)
   !
   attr = TRIM( opr%tag )
   !
   CALL iotk_scan_attr(attr, 'filein', filein, FOUND=found, IERR=ierr)
   IF (ierr/=0) CALL errore(subname, 'searching for file', ABS(ierr) )
   IF( .NOT. found ) filein = TRIM(filename)
   !
   CALL iotk_scan_attr(attr, 'cols', cols, FOUND=found, IERR=ierr)
   IF (ierr/=0) CALL errore(subname, 'searching for cols', ABS(ierr) )
   IF( .NOT. found ) cols = 'all'
   CALL change_case( cols, 'lower')
   !
   CALL iotk_scan_attr(attr, 'rows', rows, FOUND=found, IERR=ierr)
   IF (ierr/=0) CALL errore(subname, 'searching for rows', ABS(ierr) )
   IF( .NOT. found ) rows = 'all'
   CALL change_case( rows, 'lower')
   !
   CALL iotk_scan_attr(attr, 'ivr', ivr_input, FOUND=ivr_from_input, IERR=ierr)
   IF (ierr/=0) CALL errore(subname, 'searching for ivr', ABS(ierr) )

   !
   ! parse the obtained data
   !
   dim1 = opr%dim1
   dim2 = opr%dim2

   !
   ! deal with rows or cols = "all"
   !
   IF ( TRIM(rows) == "all" ) rows="1-"//TRIM( int2char(dim1) )
   IF ( TRIM(cols) == "all" ) cols="1-"//TRIM( int2char(dim2) )

   !
   ! get the number of required rows and cols
   CALL parser_replica( rows, nrows, IERR=ierr)
   IF ( ierr/=0 ) CALL errore(subname,'wrong FMT in rows string I',ABS(ierr))
   !
   CALL parser_replica( cols, ncols, IERR=ierr)
   IF ( ierr/=0 ) CALL errore(subname,'wrong FMT in cols string I',ABS(ierr))
   ! 
   IF ( nrows /= dim1 ) CALL errore(subname,'invalid number of rows',3)
   IF ( ncols /= dim2 ) CALL errore(subname,'invalid number of cols',3)
   !
   ! get the actual indexes for rows and cols
   CALL parser_replica( rows, nrows, opr%irows, IERR=ierr)
   IF ( ierr/=0 ) CALL errore(subname,'wrong FMT in rows string II',ABS(ierr))
   !
   CALL parser_replica( cols, ncols, opr%icols, IERR=ierr)
   IF ( ierr/=0 ) CALL errore(subname,'wrong FMT in cols string II',ABS(ierr))
   !
   ! simple check
   IF ( ANY( opr%irows(:) <=0 ) ) CALL errore(subname,'invalid irows(:) I',10) 
   IF ( ANY( opr%icols(:) <=0 ) ) CALL errore(subname,'invalid icols(:) I',10) 


!
! reading from iotk-formatted .ham file (internal datafmt)
!
   IF ( ionode ) THEN
       !
       CALL file_open( aux_unit, TRIM(filein), PATH="/HAMILTONIAN/",  &
                       ACTION="read", IERR=ierr )
       IF (ierr/=0) CALL errore(subname, 'opening '//TRIM(filein), ABS(ierr) )
       !
       CALL iotk_scan_empty(aux_unit, "DATA", ATTR=attr, IERR=ierr)
       IF (ierr/=0) CALL errore(subname, 'searching DATA', ABS(ierr) )
       !
       CALL iotk_scan_attr(attr,"dimwann",ldimwann, IERR=ierr)
       IF (ierr/=0) CALL errore(subname, 'searching dimwann', ABS(ierr) )
       !
       CALL iotk_scan_attr(attr,"nspin",nspin, FOUND=found, IERR=ierr)
       IF (ierr > 0) CALL errore(subname, 'searching nspin', ABS(ierr) )
       !
       IF ( .NOT. found ) nspin = 1
       !
       CALL iotk_scan_attr(attr,"nrtot",nrtot, IERR=ierr)
       IF (ierr/=0) CALL errore(subname, 'searching nrtot', ABS(ierr) )
       !
       CALL iotk_scan_attr(attr,"nr",nr_aux, IERR=ierr)
       IF (ierr/=0) CALL errore(subname, 'searching nr', ABS(ierr) )
       !
       !
       CALL iotk_scan_attr(attr,"have_overlap",lhave_ovp, FOUND=found, IERR=ierr)
       IF (ierr > 0) CALL errore(subname, 'searching have_overlap', ABS(ierr) )
       !
       IF ( .NOT. found ) lhave_ovp = .FALSE.
       !
   ENDIF
   !
   CALL mp_bcast( ldimwann,     ionode_id )
   CALL mp_bcast( nspin,        ionode_id )
   CALL mp_bcast( nrtot,        ionode_id )
   CALL mp_bcast( nr_aux,       ionode_id )
   CALL mp_bcast( lhave_ovp,    ionode_id )
   !
   opr%nrtot = nrtot


   !
   ! some checks
   !
   IF ( ldimwann <=0 )  CALL errore(subname, 'invalid dimwann', ABS(ierr))
   IF ( nrtot <=0 )     CALL errore(subname, 'invalid nrtot', ABS(ierr))
   IF ( nspin == 2 .AND. ispin == 0 ) CALL errore(subname,'unspecified ispin', 71)
   !
   i = 0
   DO j= 1, 3
       !
       IF ( transport_dir /= j ) THEN
          i = i+1
          IF ( nr_aux(j) /= nr_par(i) ) CALL errore(subname, 'invalid nr', j)
       ENDIF
       !
   ENDDO
   !
   IF ( ANY( opr%icols(:) > ldimwann ) ) CALL errore(subname, 'invalid icols(:) II', 11)
   IF ( ANY( opr%irows(:) > ldimwann ) ) CALL errore(subname, 'invalid irows(:) II', 11)

   !
   ALLOCATE( ivr(3,nrtot), STAT=ierr )
   IF (ierr/=0) CALL errore(subname, 'allocating ivr', ABS(ierr) )
   !
   ALLOCATE( A_loc(ldimwann,ldimwann), STAT=ierr )
   IF (ierr/=0) CALL errore(subname, 'allocating A_loc', ABS(ierr) )
   ALLOCATE( S_loc(ldimwann,ldimwann), STAT=ierr )
   IF (ierr/=0) CALL errore(subname, 'allocating S_loc', ABS(ierr) )
   !
   ALLOCATE( A(dim1,dim2,nrtot_par), STAT=ierr )
   IF (ierr/=0) CALL errore(subname, 'allocating A', ABS(ierr) )
   ALLOCATE( S(dim1,dim2,nrtot_par), STAT=ierr )
   IF (ierr/=0) CALL errore(subname, 'allocating S', ABS(ierr) )


   IF ( ionode ) THEN
       !
       CALL iotk_scan_dat(aux_unit, "IVR", ivr, IERR=ierr)
       IF (ierr/=0) CALL errore(subname, 'searching indxws', ABS(ierr) )

       !
       ! select the required spin component, if the case
       !
       IF ( nspin == 2 ) THEN
           !
           CALL iotk_scan_begin(aux_unit, "SPIN"//TRIM(iotk_index(ispin)), IERR=ierr)
           IF (ierr/=0) CALL errore(subname, 'searching SPIN'//TRIM(iotk_index(ispin)), ABS(ierr) )
           !
       ENDIF
        
       !
       ! get the desired R indexes
       !
       CALL iotk_scan_begin(aux_unit, "RHAM", IERR=ierr)
       IF (ierr/=0) CALL errore(subname, 'searching RHAM', ABS(ierr) )
       !
   ENDIF
   !
   CALL mp_bcast( ivr,    ionode_id )


   R_loop: &
   DO ir_par = 1, nrtot_par


      !
      ! set the 3D corresponding R vector
      !
      j = 0
      DO i=1,3
          !         
          IF ( i == transport_dir ) THEN
              !
              ! set ivr_aux(i) = 0 , 1 depending on the
              ! required matrix (detected from opr%blc_name)
              !
              SELECT CASE( TRIM(opr%blc_name) )
              !
              CASE( "block_00C", "block_00R", "block_00L", "block_C", "block_emb" )
                  ivr_aux(i) = 0
              CASE( "block_01R", "block_01L", "block_LC", "block_CR" )
                  ivr_aux(i) = 1
              CASE DEFAULT
                  CALL errore(subname, 'invalid label = '//TRIM(opr%blc_name), 1009 )
              END SELECT
              !
              ! if ivr is from input, overwrite the default choice
              !
              IF ( ivr_from_input ) ivr_aux(i) = ivr_input
              !
          ELSE
              !
              ! set the 2D parallel indexes
              !
              j = j + 1
              ivr_aux(i) = NINT( vr_par( j, ir_par) )
              !
          ENDIF
          !
      ENDDO

      !
      ! search the 3D index corresponding to ivr_aux
      !
      found = .FALSE.
      !
      DO ir = 1, nrtot
          ! 
          IF ( ALL( ivr(:,ir) == ivr_aux(:) ) )  THEN
              !
              found = .TRUE.
              ind   = ir 
              EXIT 
              !
          ENDIF
          !
      ENDDO
      !
      IF ( .NOT. found ) CALL errore(subname, '3D R-vector not found', ir_par )


      !
      ! read the 3D R matrix corresponding to index
      !
      str = "VR"//TRIM(iotk_index(ind))
      !
      IF ( ionode ) THEN
          !
          CALL iotk_scan_dat( aux_unit, str, A_loc, IERR=ierr)
          IF (ierr/=0) CALL errore(subname, 'searching '//TRIM(str), ABS(ierr) )
          !
          IF ( lhave_ovp ) THEN
              !
              str = "OVERLAP"//TRIM(iotk_index(ind))
              !
              CALL iotk_scan_dat( aux_unit, str, S_loc, IERR=ierr)
              IF (ierr/=0) CALL errore(subname, 'searching '//TRIM(str), ABS(ierr) )
              !
          ELSE
              !
              ! set the default for overlaps
              !
              SELECT CASE( TRIM(opr%blc_name) )
              !
              CASE( "block_00C", "block_00R", "block_00L", "block_C", "block_emb" )
                  !
                  S_loc(:,:) = CZERO
                  !
                  IF ( ALL (ivr_aux(:) == 0 ) ) THEN
                      !
                      DO i = 1, ldimwann
                         S_loc(i,i) = CONE
                      ENDDO   
                      !
                  ENDIF
                  !
              CASE( "block_01R", "block_01L", "block_LC", "block_CR" )
                  !
                  ! This case could be joined to the previous
                  S_loc(:,:) = CZERO
                  !
              CASE DEFAULT
                  CALL errore(subname, 'invalid label = '//TRIM(opr%blc_name), 1010 )
              END SELECT
              !
          ENDIF
          !
      ENDIF
      !
      CALL mp_bcast(  A_loc,    ionode_id )
      CALL mp_bcast(  S_loc,    ionode_id )


      !
      ! cut the total hamiltonian according to the required rows and cols
      !
      DO j=1,dim2
      DO i=1,dim1
          !
          A(i, j, ir_par) = A_loc( opr%irows(i), opr%icols(j) )
          S(i, j, ir_par) = S_loc( opr%irows(i), opr%icols(j) )
          !
      ENDDO
      ENDDO

   ENDDO R_loop

   !
   ! finalize read-in
   !
   IF ( ionode ) THEN
       !
       CALL iotk_scan_end(aux_unit, "RHAM", IERR=ierr)
       IF (ierr/=0) CALL errore(subname, 'searching end of RHAM', ABS(ierr) )
       !
       IF ( nspin == 2 ) THEN
           !
           CALL iotk_scan_end(aux_unit, "SPIN"//TRIM(iotk_index(ispin)), IERR=ierr)
           IF (ierr/=0) CALL errore(subname, 'searching end of SPIN' &
                                             //TRIM(iotk_index(ispin)), ABS(ierr) )
           !
       ENDIF
       !
       CALL file_close( aux_unit, PATH="/HAMILTONIAN/", ACTION="read", IERR=ierr )
       IF (ierr/=0) CALL errore(subname, 'closing '//TRIM(filein), ABS(ierr) )
       !
   ENDIF


   !
   ! perform the 2D FFT in the plane orthogonal to transport dir
   !
   CALL fourier_par( opr%H, dim1, dim2, A, dim1, dim2)
   CALL fourier_par( opr%S, dim1, dim2, S, dim1, dim2)



!
! cleaning local workspace
!
   DEALLOCATE( ivr, STAT=ierr)
   IF (ierr/=0) CALL errore(subname, 'deallocating ivr', ABS(ierr) )
   !
   DEALLOCATE( A_loc, S_loc, STAT=ierr)
   IF (ierr/=0) CALL errore(subname, 'deallocating A_loc, S_loc', ABS(ierr) )
   !
   DEALLOCATE( A, S, STAT=ierr)
   IF (ierr/=0) CALL errore(subname, 'deallocating A, S', ABS(ierr) )

   CALL timing( subname, OPR='stop' )
   CALL log_pop( subname )
   !
END SUBROUTINE read_matrix

