!
! Copyright (C) 2004 Andrea Ferretti
!
! This file is distributed under the terms of the
! GNU General Public License. See the file `License\'
! in the root directory of the present distribution,
! or http://www.gnu.org/copyleft/gpl.txt .
!

!*********************************************
   MODULE ggrids_module
!*********************************************
   USE kinds, ONLY : dbl
   USE parameters, ONLY : nstrx
   USE windows_module, ONLY : nkpts
   USE lattice_module, ONLY : lattice_alloc => alloc, bvec, tpiba 
   USE iotk_module
   USE timing_module, ONLY : timing
   USE parser_module, ONLY : change_case
   USE converters_module, ONLY : cry2cart
   IMPLICIT NONE
   PRIVATE
   SAVE

! This module handles data referring the G vector Grids
! used in PW representation
!
! routines in this module:
! SUBROUTINE ggrids_allocate()
! SUBROUTINE ggrids_deallocate()
! SUBROUTINE ggrids_read_ext(unit)
! SUBROUTINE ggrids_gv_indexes( igv, igsort, npwk, nr1, nr2, nr3, ninvpw, nindpw )

!
! declarations of common variables
!   

   INTEGER                   :: npw              ! number of G vects for the density
   INTEGER                   :: nr(3)            ! dimension of the FFT space grid
   !
   REAL(dbl)                 :: ecutwfc          ! energy cutoff for wfc (Ry)
   REAL(dbl)                 :: ecutrho          ! energy cutoff for the density (Ry)
   INTEGER,      ALLOCATABLE :: igv(:,:)         ! G vect cry comp (density), DIM: 3*npw
   REAL(dbl),    ALLOCATABLE :: g(:,:)           ! G vect cart comp (density) (tpiba)
   REAL(dbl),    ALLOCATABLE :: gg(:)            ! moduli of the above vectors (tpiba**2)

   LOGICAL :: alloc = .FALSE.

!
! end of declarations
!

   PUBLIC :: npw, nr 
   PUBLIC :: ecutwfc, ecutrho
   PUBLIC :: igv, g, gg
   PUBLIC :: alloc

   PUBLIC :: ggrids_allocate, ggrids_deallocate
   PUBLIC :: ggrids_read_ext
   PUBLIC :: ggrids_gv_indexes

CONTAINS

!
! subroutines
!

!**********************************************************
   SUBROUTINE ggrids_allocate()
   !**********************************************************
   IMPLICIT NONE
       CHARACTER(18)      :: subname="ggrids_allocate"
       INTEGER            :: ierr 

       IF ( npw <= 0 ) CALL errore(subname,'npw <= 0',ABS(npw)+1)
       IF ( nkpts <= 0 )  CALL errore(subname,'nkpts <= 0',ABS(nkpts)+1)

       ALLOCATE( igv(3,npw), STAT=ierr )
          IF (ierr/=0) CALL errore(subname,'allocating igv',3*npw)
       ALLOCATE( g(3,npw), STAT=ierr )
          IF (ierr/=0) CALL errore(subname,'allocating g',3*npw)
       ALLOCATE( gg(npw), STAT=ierr )
          IF (ierr/=0) CALL errore(subname,'allocating gg',npw)
       alloc = .TRUE.
      
   END SUBROUTINE ggrids_allocate


!**********************************************************
   SUBROUTINE ggrids_deallocate()
   !**********************************************************
   IMPLICIT NONE
       CHARACTER(20)      :: subname="ggrids_deallocate"
       INTEGER            :: ierr

       IF ( ALLOCATED(igv) ) THEN
            DEALLOCATE(igv, STAT=ierr)
            IF (ierr/=0)  CALL errore(subname,' deallocating igv ',ABS(ierr))
       ENDIF
       IF ( ALLOCATED(g) ) THEN
            DEALLOCATE(g, STAT=ierr)
            IF (ierr/=0)  CALL errore(subname,' deallocating g ',ABS(ierr))
       ENDIF
       IF ( ALLOCATED(gg) ) THEN
            DEALLOCATE(gg, STAT=ierr)
            IF (ierr/=0)  CALL errore(subname,' deallocating gg ',ABS(ierr))
       ENDIF
       alloc = .FALSE.

   END SUBROUTINE ggrids_deallocate


!*********************************************************
   SUBROUTINE ggrids_read_ext(unit)
   !*********************************************************
   IMPLICIT NONE
       INTEGER,           INTENT(in) :: unit
       CHARACTER(nstrx)   :: attr
       CHARACTER(nstrx)   :: str
       CHARACTER(15)      :: subname="ggrids_read_ext"
       INTEGER            :: i, ierr

       CALL timing(subname,OPR='start')
       !
       ! ... Various parameters
       !
       CALL iotk_scan_begin(unit,'Other_parameters',IERR=ierr)
       IF (ierr/=0)  CALL errore(subname,'Unable to find Other_parameters',ABS(ierr))
       !
       CALL iotk_scan_empty(unit,"Cutoff",ATTR=attr,IERR=ierr)
       IF (ierr/=0) CALL errore(subname,'unable to find Cutoff',ABS(ierr))
       CALL iotk_scan_attr(attr,"wfc",ecutwfc,IERR=ierr)
       IF (ierr/=0) CALL errore(subname,'unable to find ecutwfc',ABS(ierr))
       CALL iotk_scan_attr(attr,"rho",ecutrho,IERR=ierr)
       IF (ierr/=0) CALL errore(subname,'unable to find ecutrho',ABS(ierr))
       CALL iotk_scan_attr(attr,"units",str,IERR=ierr)
       IF (ierr/=0) CALL errore(subname,'unable to find units',ABS(ierr))
       CALL change_case(str,'UPPER')
       IF ( TRIM(str) /= 'RYDBERG' .AND. TRIM(str) /= 'RY' .AND. TRIM(str) /= 'RYD') &
            CALL errore(subname,'Cutoff units not in Rydberg',3)

       CALL iotk_scan_empty(unit,"Space_grid",ATTR=attr,IERR=ierr)
       IF (ierr/=0) CALL errore(subname,'unable to find Space_grid',ABS(ierr))
       CALL iotk_scan_attr(attr,"nr1",nr(1),IERR=ierr)
       IF (ierr/=0) CALL errore(subname,'unable to find nr1',ABS(ierr))
       CALL iotk_scan_attr(attr,"nr2",nr(2),IERR=ierr)
       IF (ierr/=0) CALL errore(subname,'unable to find nr2',ABS(ierr))
       CALL iotk_scan_attr(attr,"nr3",nr(3),IERR=ierr)
       IF (ierr/=0) CALL errore(subname,'unable to find nr3',ABS(ierr))
       !
       CALL iotk_scan_end(unit,'Other_parameters',IERR=ierr)
       IF (ierr/=0)  CALL errore(subname,'Unable to end tag Other_parameters',ABS(ierr))

       !
       ! ... Main G grid (density)
       !
       CALL iotk_scan_begin(unit,'Main_grid',ATTR=attr,IERR=ierr)
       IF (ierr/=0)  CALL errore(subname,'Unable to find Main_grid',ABS(ierr))
       !
       CALL iotk_scan_attr(attr,"npw",npw,IERR=ierr)
       IF (ierr/=0) CALL errore(subname,'unable to find npw',ABS(ierr))

       CALL ggrids_allocate() 

       CALL iotk_scan_dat(unit,"g",igv(:,:),IERR=ierr)
       IF (ierr/=0) CALL errore(subname,'unable to find igv',ABS(ierr))

       !
       CALL iotk_scan_end(unit,'Main_grid',IERR=ierr)
       IF (ierr/=0)  CALL errore(subname,'Unable to end tag Main_grid',ABS(ierr))

       !
       ! ... init, units are in tpiba (2pi/alat) according to Espresso units
       !
       IF ( .NOT. lattice_alloc ) CALL errore(subname,'Lattice quantities not allocated',4)
       g(:,:) = REAL( igv(:,:) )
       CALL cry2cart(g, bvec / tpiba )
       DO i=1,SIZE(igv,2)
           gg(i) = g(1,i)**2 + g(2,i)**2 + g(3,i)**2
       ENDDO

       CALL timing(subname,OPR='stop')
       RETURN
   END SUBROUTINE ggrids_read_ext


!**********************************************************
   SUBROUTINE ggrids_gv_indexes( igv, igsort, npwk, nr1, nr2, nr3, gk2fft, fft2gk )
   !**********************************************************
    IMPLICIT NONE
    INTEGER, INTENT(IN) :: igv(:,:)
    INTEGER, INTENT(IN) :: igsort(:)
    INTEGER, INTENT(IN) :: npwk, nr1, nr2, nr3
    INTEGER, OPTIONAL, INTENT(OUT) :: fft2gk(0:)
    INTEGER, OPTIONAL, INTENT(OUT) :: gk2fft(:)
    INTEGER :: igk, np, nx, ny, nz, npoint

    IF ( PRESENT(gk2fft) ) gk2fft = 0
    IF ( PRESENT(fft2gk) ) fft2gk = 0
    DO np = 1, npwk

      igk = igsort( np )

      IF ( igv(1,igk) >= 0 ) nx = igv(1,igk) + 1
      IF ( igv(1,igk) <  0 ) nx = igv(1,igk) + 1 + nr1
      IF ( igv(2,igk) >= 0 ) ny = igv(2,igk) + 1
      IF ( igv(2,igk) <  0 ) ny = igv(2,igk) + 1 + nr2
      IF ( igv(3,igk) >= 0 ) nz = igv(3,igk) + 1
      IF ( igv(3,igk) <  0 ) nz = igv(3,igk) + 1 + nr3       
   
! The following indexes are incoherent with FFT
!      nx = igv(1,igk)
!      IF ( nx < 1   ) nx = nx + nr1
!      IF ( nx > nr1 ) nx = nx - nr1
!      !
!      ny = igv(2,igk)
!      IF ( ny < 1   ) ny = ny + nr2
!      IF ( ny > nr2 ) ny = ny - nr2
!      !
!      nz = igv(3,igk)
!      IF ( nz < 1   ) nz = nz + nr3
!      IF ( nz > nr3 ) nz = nz - nr3
!      !

      npoint = nx + (ny-1)*nr1 + (nz-1)*nr1*nr2

      IF( PRESENT( gk2fft ) ) gk2fft(np) = npoint  ! index
      IF( PRESENT( fft2gk ) ) fft2gk(npoint) = np  ! index

    ENDDO

    RETURN
  END SUBROUTINE ggrids_gv_indexes

END MODULE ggrids_module

