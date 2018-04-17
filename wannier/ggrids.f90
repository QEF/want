!
! Copyright (C) 2004 WanT Group
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
! SUBROUTINE ggrids_gk_indexes( igv, igsort, npwk, nr1, nr2, nr3, gk2fft, fft2gk )
! SUBROUTINE ggrids_gv_indexes( igv, ngm, nr1, nr2, nr3, gv2fft, fft2gv )

!
! declarations of common variables
!   

   INTEGER                   :: npw_rho          ! number of G vects for the density
   INTEGER                   :: nfft(3)          ! dimension of the FFT space grid
   !
   REAL(dbl)                 :: ecutwfc          ! energy cutoff for wfc (Ry)
   REAL(dbl)                 :: ecutrho          ! energy cutoff for the density (Ry)
   INTEGER,      ALLOCATABLE :: igv(:,:)         ! G vect cry comp (density), DIM: 3*npw_rho
   REAL(dbl),    ALLOCATABLE :: g(:,:)           ! G vect cart comp (density) (tpiba)
   REAL(dbl),    ALLOCATABLE :: gg(:)            ! moduli of the above vectors (tpiba**2)

   LOGICAL :: alloc = .FALSE.

!
! end of declarations
!

   PUBLIC :: npw_rho, nfft
   PUBLIC :: ecutwfc, ecutrho
   PUBLIC :: igv, g, gg
   PUBLIC :: alloc

   PUBLIC :: ggrids_allocate, ggrids_deallocate
   PUBLIC :: ggrids_read_ext
   PUBLIC :: ggrids_gk_indexes, ggrids_gv_indexes

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

       IF ( npw_rho <= 0 ) CALL errore(subname,'npw_rho <= 0',ABS(npw_rho)+1)
       IF ( nkpts <= 0 )  CALL errore(subname,'nkpts <= 0',ABS(nkpts)+1)

       ALLOCATE( igv(3,npw_rho), STAT=ierr )
          IF (ierr/=0) CALL errore(subname,'allocating igv',3*npw_rho)
       ALLOCATE( g(3,npw_rho), STAT=ierr )
          IF (ierr/=0) CALL errore(subname,'allocating g',3*npw_rho)
       ALLOCATE( gg(npw_rho), STAT=ierr )
          IF (ierr/=0) CALL errore(subname,'allocating gg',npw_rho)
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
       REAL(dbl)          :: tmp(3,3)
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
       CALL iotk_scan_attr(attr,"nr1",nfft(1),IERR=ierr)
       IF (ierr/=0) CALL errore(subname,'unable to find nr1',ABS(ierr))
       CALL iotk_scan_attr(attr,"nr2",nfft(2),IERR=ierr)
       IF (ierr/=0) CALL errore(subname,'unable to find nr2',ABS(ierr))
       CALL iotk_scan_attr(attr,"nr3",nfft(3),IERR=ierr)
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
       CALL iotk_scan_attr(attr,"npw",npw_rho,IERR=ierr)
       IF (ierr/=0) CALL errore(subname,'unable to find npw_rho',ABS(ierr))

       CALL ggrids_allocate() 

       !
       ! this is the call giving problems with INTEL compiler
       ! maybe e question in the internal management of memory
       !
       CALL iotk_scan_dat(unit,"g",igv(:,:),IERR=ierr)
       IF (ierr/=0) CALL errore(subname,'unable to find igv',ABS(ierr))

       !
       CALL iotk_scan_end(unit,'Main_grid',IERR=ierr)
       IF (ierr/=0)  CALL errore(subname,'Unable to end tag Main_grid',ABS(ierr))

       !
       ! ... init, units are in tpiba (2pi/alat) according to Espresso units
       !
       IF ( .NOT. lattice_alloc ) CALL errore(subname,'Lattice quantities not allocated',4)
       g(:,:) = REAL( igv(:,:), dbl )
       tmp(:,:) =  bvec(:,:) / tpiba
       CALL cry2cart(g, tmp )
       DO i=1,SIZE(igv,2)
           gg(i) = g(1,i)**2 + g(2,i)**2 + g(3,i)**2
       ENDDO

       CALL timing(subname,OPR='stop')
       RETURN
   END SUBROUTINE ggrids_read_ext


!**********************************************************
   SUBROUTINE ggrids_gk_indexes( igv, igsort, npwk, nr1, nr2, nr3, gk2fft, fft2gk )
   !**********************************************************
   !
   ! Set the direct and inverse map between IGSORT and FFT grid, i.e. between
   ! wfc (at a given kpt) and FFT.
   !
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
   
      npoint = nx + (ny-1)*nr1 + (nz-1)*nr1*nr2

      IF( PRESENT( gk2fft ) ) gk2fft(np) = npoint  ! index
      IF( PRESENT( fft2gk ) ) fft2gk(npoint) = np  ! index

   ENDDO
END SUBROUTINE ggrids_gk_indexes


!**********************************************************
   SUBROUTINE ggrids_gv_indexes( igv, ngm, nr1, nr2, nr3, gv2fft, fft2gv )
   !**********************************************************
   IMPLICIT NONE
   INTEGER, INTENT(IN) :: igv(:,:)
   INTEGER, INTENT(IN) :: ngm, nr1, nr2, nr3
   INTEGER, OPTIONAL, INTENT(OUT) :: fft2gv(0:)
   INTEGER, OPTIONAL, INTENT(OUT) :: gv2fft(:)
   INTEGER :: ig, nx, ny, nz, npoint

   IF ( PRESENT(gv2fft) ) gv2fft = 0
   IF ( PRESENT(fft2gv) ) fft2gv = 0

   DO ig = 1, ngm

      IF ( igv(1,ig) >= 0 ) nx = igv(1,ig) + 1
      IF ( igv(1,ig) <  0 ) nx = igv(1,ig) + 1 + nr1
      IF ( igv(2,ig) >= 0 ) ny = igv(2,ig) + 1
      IF ( igv(2,ig) <  0 ) ny = igv(2,ig) + 1 + nr2
      IF ( igv(3,ig) >= 0 ) nz = igv(3,ig) + 1
      IF ( igv(3,ig) <  0 ) nz = igv(3,ig) + 1 + nr3       
   
      npoint = nx + (ny-1)*nr1 + (nz-1)*nr1*nr2

      IF( PRESENT( gv2fft ) ) gv2fft(ig) = npoint  ! index
      IF( PRESENT( fft2gv ) ) fft2gv(npoint) = ig  ! index

    ENDDO
END SUBROUTINE ggrids_gv_indexes

END MODULE ggrids_module

