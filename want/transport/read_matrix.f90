!
!      Copyright (C) 2005 WanT Group
!
!      This file is distributed under the terms of the
!      GNU General Public License. See the file `License'
!      in the root directory of the present distribution,
!      or http://www.gnu.org/copyleft/gpl.txt .
!
!***************************************************************************
   SUBROUTINE read_matrix( unit, name, dim1, dim2, a )
   !***************************************************************************
   !
   ! First the routine reads from UNIT (usually stdin) a namelist called MATRIX_DATA
   ! inside a NAME xml-iotk block, giving all the information related to the 
   ! required matrix A. Then the matrix is finally read from the wannier datafile.
   !
   !
   USE KINDS
   USE parameters,       ONLY : nstrx
   USE files_module,     ONLY : file_open, file_close, file_delete
   USE io_module,        ONLY : aux_unit
   USE iotk_module
   USE parser_module
   IMPLICIT NONE

   ! 
   ! input variables
   !
   INTEGER,      INTENT(in)  :: unit
   CHARACTER(*), INTENT(in)  :: name 
   INTEGER,      INTENT(in)  :: dim1, dim2
   COMPLEX(dbl), INTENT(out) :: a(dim1,dim2)

   !
   ! local variables
   !
   REAL(dbl) :: aux1, aux2
   INTEGER   :: nw1, nw2
   INTEGER   :: i, j, ierr
   !
   INTEGER                   :: ldimwann, lnkpts, lnws, iws
   INTEGER,      ALLOCATABLE :: indxws(:,:), degen(:)
   REAL(dbl),    ALLOCATABLE :: vkpt(:,:)
   COMPLEX(dbl), ALLOCATABLE :: lmatrix(:,:)
   CHARACTER(nstrx)          :: attr
   !
   INTEGER                   :: idir(3)
   INTEGER                   :: ncols, nrows
   INTEGER,      ALLOCATABLE :: icols(:), irows(:)
   CHARACTER(nstrx) :: filename, cols, rows
   INTEGER                   :: direction
   !
   NAMELIST / MATRIX_DATA / filename, cols, rows, direction


   !
   ! end of declarations
   !

!
!----------------------------------------
! main Body
!----------------------------------------
!

   CALL iotk_scan_begin(unit, TRIM(name), IERR=ierr)
      IF (ierr/=0) CALL errore('read_matrix', 'searching for '//TRIM(name), ABS(ierr) )

   !
   ! setting defaults
   !
   filename=" "
   cols=" "
   rows=" "
   direction=0
   !
   READ(unit, MATRIX_DATA, IOSTAT=ierr )
      IF (ierr/=0) CALL errore('read_matrix', 'reading namelist in '//TRIM(name), ABS(ierr) )

   CALL iotk_scan_end(unit, TRIM(name), IERR=ierr)
      IF (ierr/=0) CALL errore('read_matrix', 'searching end for '//TRIM(name), ABS(ierr) )

!
! parse the obtained data
!
   
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
   ! get the idir vector from DIRECTION
   ! direction = 0  ->  R = 0 0 0
   ! direction = 1  ->  R = 1 0 0
   ! direction = 2  ->  R = 0 1 0
   ! direction = 3  ->  R = 0 0 1
   !
   IF ( direction < 0 .OR. direction > 3 ) &
        CALL errore('read_matrix','invalid direction',ABS(direction)+1)
   idir(:) = 0
   IF ( direction /= 0 ) idir ( direction ) = 1
   

!
! reading form iotk-formatted .ham file produced by wannier
!
   CALL file_open( aux_unit, TRIM(filename), PATH="/HAMILTONIAN/", &
                   ACTION="read", FORM="formatted" )
   !
   CALL iotk_scan_empty(aux_unit, "DATA", ATTR=attr, IERR=ierr)
      IF (ierr/=0) CALL errore('read_matrix', 'searching DATA', ABS(ierr) )
   CALL iotk_scan_attr(attr,"dimwann",ldimwann, IERR=ierr)
      IF (ierr/=0) CALL errore('read_matrix', 'searching dimwann', ABS(ierr) )
   CALL iotk_scan_attr(attr,"nkpts",lnkpts, IERR=ierr)
      IF (ierr/=0) CALL errore('read_matrix', 'searching nkpts', ABS(ierr) )
   CALL iotk_scan_attr(attr,"nws",lnws, IERR=ierr)
      IF (ierr/=0) CALL errore('read_matrix', 'searching nws', ABS(ierr) )

   IF (ldimwann <=0 ) CALL errore('read_matrix', 'invalid dimwann', ABS(ierr))
   IF (lnkpts <=0 ) CALL errore('read_matrix', 'invalid nkpts', ABS(ierr))
   IF (lnws <=0 ) CALL errore('read_matrix', 'invalid nws', ABS(ierr))
   !
   DO i=1,ncols
      IF ( icols(i) > ldimwann ) CALL errore('read_matrix', 'invalid icols(i)', i)
   ENDDO
   DO i=1,nrows
      IF ( irows(i) > ldimwann ) CALL errore('read_matrix', 'invalid irows(i)', i)
   ENDDO

   !
   ALLOCATE( indxws(3,lnws), STAT=ierr )
      IF (ierr/=0) CALL errore('read_matrix', 'allocating indxws', ABS(ierr) )
   ALLOCATE( degen(lnws), STAT=ierr )
      IF (ierr/=0) CALL errore('read_matrix', 'allocating degen', ABS(ierr) )
   ALLOCATE( vkpt(3,lnkpts), STAT=ierr )
      IF (ierr/=0) CALL errore('read_matrix', 'allocating vkpt', ABS(ierr) )
   ALLOCATE( lmatrix(ldimwann,ldimwann), STAT=ierr )
      IF (ierr/=0) CALL errore('read_matrix', 'allocating lmatrix', ABS(ierr) )

   CALL iotk_scan_dat(aux_unit, "VKPT", vkpt, IERR=ierr)
      IF (ierr/=0) CALL errore('read_matrix', 'searching vkpt', ABS(ierr) )
   CALL iotk_scan_dat(aux_unit, "INDXWS", indxws, IERR=ierr)
      IF (ierr/=0) CALL errore('read_matrix', 'searching indxws', ABS(ierr) )
   CALL iotk_scan_dat(aux_unit, "DEGEN", degen, IERR=ierr)
      IF (ierr/=0) CALL errore('read_matrix', 'searching degen', ABS(ierr) )

   !
   ! get the desired R index
   !
   iws = 0
   R_loop: &
   DO i = 1, lnws
      IF ( indxws(1,i) == idir(1) .AND. &
           indxws(2,i) == idir(2) .AND. &
           indxws(3,i) == idir(3)   ) THEN 
         iws = i
         EXIT R_loop
      ENDIF
   ENDDO R_loop
   IF ( iws == 0 ) CALL errore('read_matrix', 'searching iws', 4)


!
! searching the required hamiltonian matrix
!
   CALL iotk_scan_begin(aux_unit, "RHAM", IERR=ierr)
      IF (ierr/=0) CALL errore('read_matrix', 'searching RHAM', ABS(ierr) )
   CALL iotk_scan_dat( aux_unit, "WS"//TRIM(iotk_index(iws)), lmatrix, IERR=ierr)
      IF (ierr/=0) &
         CALL errore('read_matrix', 'searching WS'//TRIM(iotk_index(iws)), ABS(ierr) )
   CALL iotk_scan_end(aux_unit, "RHAM", IERR=ierr)
      IF (ierr/=0) CALL errore('read_matrix', 'searching end of RHAM', ABS(ierr) )


   CALL file_close( aux_unit, PATH="/HAMILTONIAN/", ACTION="read" )


!
! cut the total hamiltonian according to the required rows and cols
!
  DO j=1,ncols
  DO i=1,nrows
      a(i,j) = lmatrix( irows(i), icols(j) )
  ENDDO
  ENDDO

!
! this is a debug purpose statement
!
#ifdef __DEBUG_REAL_MATRIX
  a(:,:) = CMPLX( REAL(a(:,:)) )
#endif


!
! cleaning local workspace
!
   DEALLOCATE( indxws, degen, STAT=ierr)
      IF (ierr/=0) CALL errore('read_matrix', 'deallocating indxws, degen', ABS(ierr) )
   DEALLOCATE( vkpt, STAT=ierr)
      IF (ierr/=0) CALL errore('read_matrix', 'deallocating vkpt', ABS(ierr) )
   DEALLOCATE( lmatrix, STAT=ierr)
      IF (ierr/=0) CALL errore('read_matrix', 'deallocating lmatrix', ABS(ierr) )
   DEALLOCATE( icols, irows, STAT=ierr)
      IF (ierr/=0) CALL errore('read_matrix', 'deallocating icols, irows', ABS(ierr) )

END SUBROUTINE read_matrix

