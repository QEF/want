!
!      Copyright (C) 2005 WanT Group
!
!      This file is distributed under the terms of the
!      GNU General Public License. See the file `License'
!      in the root directory of the present distribution,
!      or http://www.gnu.org/copyleft/gpl.txt .
!
!***************************************************************************
   SUBROUTINE read_matrix( file, name, dim1, dim2, a, lda1, lda2 )
   !***************************************************************************
   !
   ! First, the routine reads from stdin a namelist called MATRIX_DATA
   ! inside a NAME xml-iotk block, giving all the information related to the 
   ! required matrix A. Then the matrix is finally read from the wannier datafile.
   !
   ! NOTE: some data about cols and rows, are saved to be used for correlation
   !       self-energies. This is a fragile implementation to be made safer. 
   !
   !
   USE KINDS
   USE parameters,       ONLY : nstrx
   USE files_module,     ONLY : file_open, file_close, file_delete
   USE io_module,        ONLY : stdin, aux_unit
   USE iotk_module
   USE timing_module
   USE parser_module
   USE T_control_module, ONLY : transport_dir, use_correlation
   USE T_kpoints_module, ONLY : kpoints_alloc => alloc, nrtot_par, vr_par, nr_par
   USE T_correlation_module, ONLY : icols_corr => icols, irows_corr => irows,  &
                                    ncols_corr => ncols, nrows_corr => nrows
   IMPLICIT NONE

   ! 
   ! input variables
   !
   CHARACTER(*), INTENT(in)  :: file, name 
   INTEGER,      INTENT(in)  :: dim1, dim2, lda1, lda2
   COMPLEX(dbl), INTENT(out) :: a(lda1,lda2,nrtot_par)

   !
   ! local variables
   !
   INTEGER   :: i, j, ierr
   !
   INTEGER                   :: ldimwann, nrtot, ir, ir_par
   INTEGER,      ALLOCATABLE :: ivr(:,:)
   COMPLEX(dbl), ALLOCATABLE :: lmatrix(:,:)
   CHARACTER(nstrx)          :: attr
   !
   LOGICAL                   :: found
   INTEGER                   :: index, ivr_aux(3), nr_aux(3)
   INTEGER                   :: ncols, nrows
   INTEGER,      ALLOCATABLE :: icols(:), irows(:)
   CHARACTER(nstrx)          :: cols, rows

   !
   ! end of declarations
   !

!
!----------------------------------------
! main Body
!----------------------------------------
!
   CALL timing( 'read_matrix', OPR='start' )

   !
   ! some checks
   !
   IF ( .NOT. kpoints_alloc ) CALL errore('read_matrix', 'kpoints not alloc', 1 )
   IF ( dim1 > lda1 ) CALL errore('read_matrix', 'invalid dim1', 1 )
   IF ( dim2 > lda2 ) CALL errore('read_matrix', 'invalid dim2', 2 )

   CALL iotk_scan_empty(stdin, TRIM(name), ATTR=attr, IERR=ierr)
   IF (ierr/=0) CALL errore('read_matrix', 'searching for '//TRIM(name), ABS(ierr) )
   !
   CALL iotk_scan_attr(attr, 'cols', cols, FOUND=found, IERR=ierr)
   IF (ierr/=0) CALL errore('read_matrix', 'searching for cols', ABS(ierr) )
   IF( .NOT. found ) cols = 'all'
   CALL change_case( cols, 'lower')
   !
   CALL iotk_scan_attr(attr, 'rows', rows, FOUND=found, IERR=ierr)
   IF (ierr/=0) CALL errore('read_matrix', 'searching for rows', ABS(ierr) )
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
   IF ( ierr/=0 ) CALL errore('read_matrix','wrong FMT in rows string I',ABS(ierr))
   CALL parser_replica( cols, ncols, IERR=ierr)
   IF ( ierr/=0 ) CALL errore('read_matrix','wrong FMT in cols string I',ABS(ierr))
   ! 
   IF ( nrows /= dim1 ) CALL errore('read_matrix','invalid number of rows',3)
   IF ( ncols /= dim2 ) CALL errore('read_matrix','invalid number of cols',3)
   !
   ALLOCATE( irows(nrows), STAT=ierr )
      IF (ierr/=0) CALL errore('read_matrix', 'allocating irows', ABS(ierr) )
   ALLOCATE( icols(ncols), STAT=ierr )
      IF (ierr/=0) CALL errore('read_matrix', 'allocating icols', ABS(ierr) )
   !
   ! get the actual indexes for rows and cols
   CALL parser_replica( rows, nrows, irows, IERR=ierr)
   IF ( ierr/=0 ) CALL errore('read_matrix','wrong FMT in rows string II',ABS(ierr))
   CALL parser_replica( cols, ncols, icols, IERR=ierr)
   IF ( ierr/=0 ) CALL errore('read_matrix','wrong FMT in cols string II',ABS(ierr))
   !
   ! simple check
   DO i=1,nrows
       IF ( irows(i) <=0 ) CALL errore('read_matrix','invalid irows(i)',i) 
   ENDDO
   DO i=1,ncols
       IF ( icols(i) <=0 ) CALL errore('read_matrix','invalid icols(i)',i) 
   ENDDO

   !
   ! save cols and rows data of H00_C for correlation self-energies
   ! this statement is quite fragile and should be improved
   !
   IF ( use_correlation ) THEN
      IF ( TRIM(name) == 'H00_C') THEN 
           !
           ncols_corr = ncols
           nrows_corr = nrows
           ALLOCATE( icols_corr(ncols_corr), STAT=ierr )
               IF (ierr/=0) CALL errore('read_matrix','allocating icols_corr',ABS(ierr))
           ALLOCATE( irows_corr(nrows_corr), STAT=ierr )
               IF (ierr/=0) CALL errore('read_matrix','allocating irows_corr',ABS(ierr))

           icols_corr(:) = icols(:)
           irows_corr(:) = irows(:)
      ENDIF
   ENDIF


!
! reading form iotk-formatted .ham file produced by wannier
!
   CALL file_open( aux_unit, TRIM(file), PATH="/HAMILTONIAN/", &
                   ACTION="read", FORM="formatted" )
   !
   CALL iotk_scan_empty(aux_unit, "DATA", ATTR=attr, IERR=ierr)
      IF (ierr/=0) CALL errore('read_matrix', 'searching DATA', ABS(ierr) )
   CALL iotk_scan_attr(attr,"dimwann",ldimwann, IERR=ierr)
      IF (ierr/=0) CALL errore('read_matrix', 'searching dimwann', ABS(ierr) )
   CALL iotk_scan_attr(attr,"nr",nr_aux, IERR=ierr)
      IF (ierr/=0) CALL errore('read_matrix', 'searching nr', ABS(ierr) )
   CALL iotk_scan_attr(attr,"nrtot",nrtot, IERR=ierr)
      IF (ierr/=0) CALL errore('read_matrix', 'searching nrtot', ABS(ierr) )

   IF (ldimwann <=0 ) CALL errore('read_matrix', 'invalid dimwann', ABS(ierr))
   IF (nrtot <=0 ) CALL errore('read_matrix', 'invalid nrtot', ABS(ierr))
   !
   i = 0
   DO j= 1, 3
      IF ( transport_dir /= j ) THEN
         i = i+1
         IF ( nr_aux(j) /= nr_par(i) ) CALL errore('read_matrix', 'invalid nr', j)
      ENDIF
   ENDDO

   !
   DO i=1,ncols
      IF ( icols(i) > ldimwann ) CALL errore('read_matrix', 'invalid icols(i)', i)
   ENDDO
   DO i=1,nrows
      IF ( irows(i) > ldimwann ) CALL errore('read_matrix', 'invalid irows(i)', i)
   ENDDO

   !
   ALLOCATE( ivr(3,nrtot), STAT=ierr )
      IF (ierr/=0) CALL errore('read_matrix', 'allocating ivr', ABS(ierr) )
   ALLOCATE( lmatrix(ldimwann,ldimwann), STAT=ierr )
      IF (ierr/=0) CALL errore('read_matrix', 'allocating lmatrix', ABS(ierr) )

   CALL iotk_scan_dat(aux_unit, "IVR", ivr, IERR=ierr)
      IF (ierr/=0) CALL errore('read_matrix', 'searching indxws', ABS(ierr) )

   !
   ! get the desired R indexes
   !
   CALL iotk_scan_begin(aux_unit, "RHAM", IERR=ierr)
   IF (ierr/=0) CALL errore('read_matrix', 'searching RHAM', ABS(ierr) )

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
            ! required matrix (from the name input variable)
            !
            SELECT CASE( TRIM(name) )
            CASE( "H00_C", "H00_R", "H00_L" )
                ivr_aux(i) = 0
            CASE( "H01_R", "H01_L", "H_LC", "H_CR" )
                ivr_aux(i) = 1
            CASE DEFAULT
                CALL errore('read_matrix', 'invalid name = '//TRIM(name), ABS(ierr) )
            END SELECT
            !
         ELSE
            !
            ! set the 2D parallel indexes
            !
            j = j + 1
            ivr_aux(i) = NINT( vr_par( j, ir_par) )
         ENDIF
      ENDDO

      !
      ! search the 3D index corresponding to ivr_aux
      !
      found = .FALSE.
      DO ir = 1, nrtot
          ! 
          IF ( ALL( ivr(:,ir) == ivr_aux(:) ) )  THEN
               found = .TRUE.
               index = ir 
               EXIT 
          ENDIF
      ENDDO
      !
      IF ( .NOT. found ) CALL errore('read_matrix', '3D R-vector not found', ir_par )


      !
      ! read the 3D R matrix corresponding to index
      !
      CALL iotk_scan_dat( aux_unit, "VR"//TRIM(iotk_index(index)), lmatrix, IERR=ierr)
      IF (ierr/=0) &
         CALL errore('read_matrix', 'searching VR'//TRIM(iotk_index(index)), ABS(ierr) )

      !
      ! cut the total hamiltonian according to the required rows and cols
      !
      DO j=1,ncols
      DO i=1,nrows
         a(i, j, ir_par) = lmatrix( irows(i), icols(j) )
      ENDDO
      ENDDO

      !
      ! this is a debug purpose statement
      ! to be used to check differences arising due to non real wannier
      ! hamiltonians
      !
#ifdef __DEBUG_REAL_MATRIX
      a(1:dim1, 1:dim2, ir_par) = CMPLX( REAL(a(1:dim1, 1:dim2, ir_par)), dbl )
#endif

   ENDDO R_loop


   CALL iotk_scan_end(aux_unit, "RHAM", IERR=ierr)
   IF (ierr/=0) CALL errore('read_matrix', 'searching end of RHAM', ABS(ierr) )

   CALL file_close( aux_unit, PATH="/HAMILTONIAN/", ACTION="read" )


!
! cleaning local workspace
!
   DEALLOCATE( ivr, STAT=ierr)
      IF (ierr/=0) CALL errore('read_matrix', 'deallocating ivr', ABS(ierr) )
   DEALLOCATE( lmatrix, STAT=ierr)
      IF (ierr/=0) CALL errore('read_matrix', 'deallocating lmatrix', ABS(ierr) )
   DEALLOCATE( icols, irows, STAT=ierr)
      IF (ierr/=0) CALL errore('read_matrix', 'deallocating icols, irows', ABS(ierr) )

   CALL timing( 'read_matrix', OPR='stop' )
END SUBROUTINE read_matrix

