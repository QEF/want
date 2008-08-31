!
!      Copyright (C) 2005 WanT Group
!
!      This file is distributed under the terms of the
!      GNU General Public License. See the file `License'
!      in the root directory of the present distribution,
!      or http://www.gnu.org/copyleft/gpl.txt .
!
!***************************************************************************
   SUBROUTINE read_matrix( filename, ispin, label, dim1, dim2, A, lda1, lda2, &
                           have_overlap, S, lds1, lds2 )
   !***************************************************************************
   !
   ! First, the routine reads from stdin a namelist called MATRIX_DATA
   ! inside a NAME xml-iotk block, giving all the information related to the 
   ! required matrix A. Then the matrix is finally read from the related datafile.
   !
   ! The routine check for the presence of overlaps and read them if the case.
   !
   ! NOTE: some data about cols and rows, are saved to be used for correlation
   !       self-energies. This is a fragile implementation to be made safer. 
   !
   !
   USE kinds 
   USE parameters,           ONLY : nstrx
   USE constants,            ONLY : CZERO, CONE
   USE files_module,         ONLY : file_open, file_close, file_delete
   USE io_module,            ONLY : stdin, aux_unit
   USE iotk_module
   USE timing_module
   USE log_module,           ONLY : log_push, log_pop
   USE parser_module
   USE T_control_module,     ONLY : transport_dir, use_correlation
   USE T_kpoints_module,     ONLY : kpoints_alloc => alloc, nrtot_par, vr_par, nr_par
   USE T_correlation_module, ONLY : icols_corr => icols, irows_corr => irows,  &
                                    ncols_corr => ncols, nrows_corr => nrows
   IMPLICIT NONE

   ! 
   ! input variables
   !
   CHARACTER(*), INTENT(IN)  :: filename, label 
   INTEGER,      INTENT(IN)  :: ispin
   INTEGER,      INTENT(IN)  :: dim1, dim2, lda1, lda2
   INTEGER,      INTENT(IN)  :: lds1, lds2
   LOGICAL,      INTENT(OUT) :: have_overlap
   COMPLEX(dbl), INTENT(OUT) :: A(lda1,lda2,nrtot_par)
   COMPLEX(dbl), INTENT(OUT) :: S(lds1,lds2,nrtot_par)

   !
   ! local variables
   !
   CHARACTER(11)             :: subname = 'read_matrix'
   INTEGER                   :: i, j, ierr
   !
   INTEGER                   :: ldimwann, nrtot, nspin, ir, ir_par
   INTEGER,      ALLOCATABLE :: ivr(:,:)
   COMPLEX(dbl), ALLOCATABLE :: A_loc(:,:), S_loc(:,:)
   CHARACTER(nstrx)          :: attr, str
   !
   LOGICAL                   :: found
   INTEGER                   :: index, ivr_aux(3), nr_aux(3)
   INTEGER                   :: ncols, nrows
   INTEGER,      ALLOCATABLE :: icols(:), irows(:)
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
   IF ( dim1 > lda1 ) CALL errore(subname, 'invalid dim1', 1 )
   IF ( dim2 > lda2 ) CALL errore(subname, 'invalid dim2', 1 )
   IF ( dim1 > lds1 ) CALL errore(subname, 'invalid dim1', 2 )
   IF ( dim2 > lds2 ) CALL errore(subname, 'invalid dim2', 2 )

   !
   ! read data from STDIN
   !
   CALL iotk_scan_empty(stdin, TRIM(label), ATTR=attr, IERR=ierr)
   IF (ierr/=0) CALL errore(subname, 'searching for '//TRIM(label), ABS(ierr) )
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
! parse the obtained data
!
   !
   ! deal with rows or cols = "all"
   !
   IF ( TRIM(rows) == "all" ) rows="1-"//TRIM( int2char(dim1))
   IF ( TRIM(cols) == "all" ) cols="1-"//TRIM( int2char(dim2))
   
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
   ALLOCATE( irows(nrows), STAT=ierr )
   IF (ierr/=0) CALL errore(subname, 'allocating irows', ABS(ierr) )
   !
   ALLOCATE( icols(ncols), STAT=ierr )
   IF (ierr/=0) CALL errore(subname, 'allocating icols', ABS(ierr) )
   !
   ! get the actual indexes for rows and cols
   CALL parser_replica( rows, nrows, irows, IERR=ierr)
   IF ( ierr/=0 ) CALL errore(subname,'wrong FMT in rows string II',ABS(ierr))
   !
   CALL parser_replica( cols, ncols, icols, IERR=ierr)
   IF ( ierr/=0 ) CALL errore(subname,'wrong FMT in cols string II',ABS(ierr))
   !
   ! simple check
   DO i=1,nrows
       IF ( irows(i) <=0 ) CALL errore(subname,'invalid irows(i)',i) 
   ENDDO
   !
   DO i=1,ncols
       IF ( icols(i) <=0 ) CALL errore(subname,'invalid icols(i)',i) 
   ENDDO

   !
   ! save cols and rows data of H00_C for correlation self-energies
   ! this statement is quite fragile and should be improved
   !
   IF ( use_correlation ) THEN
      !
      IF ( TRIM(label) == 'H00_C') THEN 
           !
           ncols_corr = ncols
           nrows_corr = nrows
           !
           ALLOCATE( icols_corr(ncols_corr), STAT=ierr )
           IF (ierr/=0) CALL errore(subname,'allocating icols_corr',ABS(ierr))
           !
           ALLOCATE( irows_corr(nrows_corr), STAT=ierr )
           IF (ierr/=0) CALL errore(subname,'allocating irows_corr',ABS(ierr))
           !
           icols_corr(:) = icols(:)
           irows_corr(:) = irows(:)
      ENDIF
      !
   ENDIF


!
! reading from iotk-formatted .ham file (internal datafmt)
!
   CALL file_open( aux_unit, TRIM(filein), PATH="/HAMILTONIAN/", ACTION="read", IERR=ierr )
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
   CALL iotk_scan_attr(attr,"have_overlap",have_overlap, FOUND=found, IERR=ierr)
   IF (ierr > 0) CALL errore(subname, 'searching have_overlap', ABS(ierr) )
   !
   IF ( .NOT. found ) have_overlap = .FALSE.

   IF ( ldimwann <=0 )                CALL errore(subname, 'invalid dimwann', ABS(ierr))
   IF ( nrtot <=0 )                   CALL errore(subname, 'invalid nrtot', ABS(ierr))
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
   DO i=1,ncols
      IF ( icols(i) > ldimwann ) CALL errore(subname, 'invalid icols(i)', i)
   ENDDO
   !
   DO i=1,nrows
      IF ( irows(i) > ldimwann ) CALL errore(subname, 'invalid irows(i)', i)
   ENDDO

   !
   ALLOCATE( ivr(3,nrtot), STAT=ierr )
   IF (ierr/=0) CALL errore(subname, 'allocating ivr', ABS(ierr) )
   !
   ALLOCATE( A_loc(ldimwann,ldimwann), STAT=ierr )
   IF (ierr/=0) CALL errore(subname, 'allocating A_loc', ABS(ierr) )
   !
   ALLOCATE( S_loc(ldimwann,ldimwann), STAT=ierr )
   IF (ierr/=0) CALL errore(subname, 'allocating S_loc', ABS(ierr) )


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
              ! required matrix (from the label input variable)
              !
              SELECT CASE( TRIM(label) )
              !
              CASE( "H00_C", "H00_R", "H00_L" )
                  ivr_aux(i) = 0
              CASE( "H01_R", "H01_L", "H_LC", "H_CR" )
                  ivr_aux(i) = 1
              CASE DEFAULT
                  CALL errore(subname, 'invalid label = '//TRIM(label), 1009 )
              END SELECT
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
      DO ir = 1, nrtot
          ! 
          IF ( ALL( ivr(:,ir) == ivr_aux(:) ) )  THEN
              !
              found = .TRUE.
              index = ir 
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
      str = "VR"//TRIM(iotk_index(index))
      !
      CALL iotk_scan_dat( aux_unit, str, A_loc, IERR=ierr)
      IF (ierr/=0) CALL errore(subname, 'searching '//TRIM(str), ABS(ierr) )
      !
      IF ( have_overlap ) THEN
          !
          str = "OVERLAP"//TRIM(iotk_index(index))
          !
          CALL iotk_scan_dat( aux_unit, str, S_loc, IERR=ierr)
          IF (ierr/=0) CALL errore(subname, 'searching '//TRIM(str), ABS(ierr) )
          !
      ELSE
          !
          ! set the default for overlaps
          !
          SELECT CASE( TRIM(label) )
          !
          CASE( "H00_C", "H00_R", "H00_L" )
              !
              S_loc(:,:) = CZERO
              !
              DO i = 1, ldimwann
                 S_loc(i,i) = CONE
              ENDDO   
              !
          CASE( "H01_R", "H01_L", "H_LC", "H_CR" )
              !
              S_loc(:,:) = CZERO
              !
          CASE DEFAULT
              CALL errore(subname, 'invalid label = '//TRIM(label), 1010 )
          END SELECT
          !
      ENDIF

      !
      ! cut the total hamiltonian according to the required rows and cols
      !
      DO j=1,ncols
      DO i=1,nrows
          !
          A(i, j, ir_par) = A_loc( irows(i), icols(j) )
          S(i, j, ir_par) = S_loc( irows(i), icols(j) )
          !
      ENDDO
      ENDDO

   ENDDO R_loop


   CALL iotk_scan_end(aux_unit, "RHAM", IERR=ierr)
   IF (ierr/=0) CALL errore(subname, 'searching end of RHAM', ABS(ierr) )
   !
   IF ( nspin == 2 ) THEN
       !
       CALL iotk_scan_end(aux_unit, "SPIN"//TRIM(iotk_index(ispin)), IERR=ierr)
       IF (ierr/=0) CALL errore(subname, 'searching end of SPIN'//TRIM(iotk_index(ispin)), ABS(ierr) )
       !
   ENDIF
   !
   CALL file_close( aux_unit, PATH="/HAMILTONIAN/", ACTION="read", IERR=ierr )
   IF (ierr/=0) CALL errore(subname, 'closing '//TRIM(filein), ABS(ierr) )


!
! cleaning local workspace
!
   DEALLOCATE( ivr, STAT=ierr)
   IF (ierr/=0) CALL errore(subname, 'deallocating ivr', ABS(ierr) )
   !
   DEALLOCATE( A_loc, S_loc, STAT=ierr)
   IF (ierr/=0) CALL errore(subname, 'deallocating A_loc, S_loc', ABS(ierr) )
   !
   DEALLOCATE( icols, irows, STAT=ierr)
   IF (ierr/=0) CALL errore(subname, 'deallocating icols, irows', ABS(ierr) )

   CALL timing( subname, OPR='stop' )
   CALL log_pop( subname )
   !
END SUBROUTINE read_matrix

