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
   !
   USE kinds,             ONLY : dbl
   USE constants,         ONLY : TWO, RYD
   USE parameters,        ONLY : nstrx
   USE windows_module,    ONLY : nkpts
   USE lattice_module,    ONLY : lattice_alloc => alloc, bvec, tpiba 
   USE timing_module,     ONLY : timing
   USE log_module,        ONLY : log_push, log_pop
   USE parser_module,     ONLY : change_case
   USE converters_module, ONLY : cry2cart
   USE qexml_module
   USE qexpt_module
   IMPLICIT NONE
   PRIVATE
   SAVE

! This module handles data referring the G vector Grids
! used in PW representation
!
! routines in this module:
! SUBROUTINE ggrids_allocate()
! SUBROUTINE ggrids_deallocate()
! SUBROUTINE ggrids_read_ext( filefmt )
! SUBROUTINE ggrids_summary( ounit )
! SUBROUTINE ggrids_gk_indexes( igv, igsort, npwk, nr1, nr2, nr3, gk2fft, fft2gk )
! SUBROUTINE ggrids_gv_indexes( igv, ngm, nr1, nr2, nr3, gv2fft, fft2gv )

!
! declarations of common variables
!   

   INTEGER                   :: npw_rho          ! number of G vects for the density
   INTEGER                   :: nfft(3)          ! dimension of the FFT space grid
   !
   LOGICAL                   :: have_smooth_rhogrid   ! if .true. we have a smooth grid
   INTEGER                   :: npws_rho         ! number of G vects in the smooth density grid
   INTEGER                   :: nffts(3)         ! dimension of the smooth (USPP only) FFT grid
   !                                             ! when dealing with NCPP, nfft and nffts are equal 
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

   PUBLIC :: npw_rho,  nfft
   PUBLIC :: npws_rho, nffts, have_smooth_rhogrid
   PUBLIC :: ecutwfc, ecutrho
   PUBLIC :: igv, g, gg
   PUBLIC :: alloc

   PUBLIC :: ggrids_allocate, ggrids_deallocate
   PUBLIC :: ggrids_read_ext
   PUBLIC :: ggrids_summary
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

       CALL log_push( subname )
       !
       IF ( npw_rho <= 0 ) CALL errore(subname,'npw_rho <= 0',ABS(npw_rho)+1)
       IF ( nkpts <= 0 )  CALL errore(subname,'nkpts <= 0',ABS(nkpts)+1)

       ALLOCATE( igv(3,npw_rho), STAT=ierr )
       IF (ierr/=0) CALL errore(subname,'allocating igv',3*npw_rho)
       !
       ALLOCATE( g(3,npw_rho), STAT=ierr )
       IF (ierr/=0) CALL errore(subname,'allocating g',3*npw_rho)
       !
       ALLOCATE( gg(npw_rho), STAT=ierr )
       IF (ierr/=0) CALL errore(subname,'allocating gg',npw_rho)
       alloc = .TRUE.
       ! 
       CALL log_pop ( subname )
       !
   END SUBROUTINE ggrids_allocate


!**********************************************************
   SUBROUTINE ggrids_deallocate()
   !**********************************************************
   IMPLICIT NONE
       CHARACTER(20)      :: subname="ggrids_deallocate"
       INTEGER            :: ierr

       CALL log_push ( subname )
       !
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
       !
       CALL log_pop ( subname )
       !
   END SUBROUTINE ggrids_deallocate


!*********************************************************
   SUBROUTINE ggrids_read_ext( filefmt )
   !*********************************************************
   IMPLICIT NONE
       CHARACTER(*), INTENT(IN) :: filefmt      
       ! 
       REAL(dbl)          :: tmp(3,3)
       CHARACTER(nstrx)   :: str
       CHARACTER(15)      :: subname="ggrids_read_ext"
       INTEGER            :: i, ierr

       !
       CALL timing ( subname,OPR='start')
       CALL log_push ( subname )
       !
       !
       IF ( alloc ) CALL ggrids_deallocate()
       !
       !
       SELECT CASE ( TRIM(filefmt) )
       !
       CASE ( 'qexml' )
            !
            CALL qexml_read_planewaves( ECUTWFC=ecutwfc, ECUTRHO=ecutrho, CUTOFF_UNITS=str, &
                                        NR1=nfft(1),   NR2=nfft(2),   NR3=nfft(3),          &
                                        NR1S=nffts(1), NR2S=nffts(2), NR3S=nffts(3),        &
                                        NGM=npw_rho,   NGMS=npws_rho, IERR=ierr )
            !
       CASE ( 'pw_export' )
            !
            CALL qexpt_read_planewaves( ECUTWFC=ecutwfc, ECUTRHO=ecutrho, CUTOFF_UNITS=str, &
                                        NR1=nfft(1), NR2=nfft(2), NR3=nfft(3),              &
                                        NGM=npw_rho, IERR=ierr )
            !
            ! because data related to the smooth grid is missing in the pw_export dataset
            ! we set the two grids to be equal
            !
            nffts(1:3) = nfft(1:3)
            npws_rho   = npw_rho
            !
       CASE DEFAULT
            !
            CALL errore(subname,'invalid filefmt = '//TRIM(filefmt), 1)
       END SELECT
       !
       IF ( ierr/=0) CALL errore(subname,'getting bands dimensions',ABS(ierr))
       
       !
       ! for future reference
       !
       have_smooth_rhogrid = .FALSE.
       IF ( ANY( nffts(:) < nfft(:)) ) have_smooth_rhogrid = .TRUE.

       !
       ! ... allocaing ggrids
       !
       CALL ggrids_allocate( )

       !
       ! read massive data
       !
       SELECT CASE ( TRIM(filefmt) )
       !
       CASE ( 'qexml' )
            !
            CALL qexml_read_planewaves( IGV = igv, IERR = ierr )
            !
       CASE ( 'pw_export' )
            !
            CALL qexpt_read_planewaves( IGV = igv, IERR = ierr )
            !
       CASE DEFAULT
            !
            CALL errore(subname,'invalid filefmt = '//TRIM(filefmt), 2)
       END SELECT
       !
       IF ( ierr/=0) CALL errore(subname,'getting massive data',ABS(ierr))
       
       !
       ! check energy units (move them to Ryd)
       !
       CALL change_case(str,'lower')
       !
       SELECT CASE ( TRIM(str) )
       CASE ( 'rydberg', 'ryd', 'ry' )
           !
           ! doing nothing
           !
       CASE ( 'hartree', 'ha')
           !
           ecutwfc = ecutwfc * TWO 
           ecutrho = ecutrho * TWO 
           !
       CASE ( 'elettronvolt', 'elettron-volt', 'ev')
           !
           ecutwfc = ecutwfc / RYD
           ecutrho = ecutrho / RYD
           !
       CASE DEFAULT
           CALL errore(subname,'Wrong units in Energies',5)
       END SELECT


       !
       ! ... move units in tpiba (2pi/alat) according to Espresso units
       !
       IF ( .NOT. lattice_alloc ) CALL errore(subname,'Lattice quantities not allocated',4)

       !
       ! compute g and gg in units of tpiba and tpiba2
       !
       g(:,:) = REAL( igv(:,:), dbl )
       !
       tmp(:,:) =  bvec(:,:) / tpiba
       CALL cry2cart(g, tmp )
       !
       DO i=1,SIZE(igv,2)
           gg(i) = g(1,i)**2 + g(2,i)**2 + g(3,i)**2
       ENDDO
       !
       !
       CALL timing ( subname,OPR='stop')
       CALL log_pop ( subname )
       !
       ! 
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

   CALL log_push( 'ggrids_gk_indexes' )
   !
   IF ( PRESENT(gk2fft) ) gk2fft = 0
   IF ( PRESENT(fft2gk) ) fft2gk = 0

   DO np = 1, npwk
      igk = igsort( np )

      nx = -100000
      ny = -100000
      nz = -100000
      !
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
   !
   CALL log_pop( 'ggrids_gk_indexes' )
   !
END SUBROUTINE ggrids_gk_indexes


!**********************************************************
   SUBROUTINE ggrids_gv_indexes( igvl, ngm, nr1, nr2, nr3, gv2fft, fft2gv )
   !**********************************************************
   !
   ! Set the direct and inverse map between IGV and FFT grid, i.e. between
   ! rho and FFT.
   !
   IMPLICIT NONE
   INTEGER, INTENT(IN) :: igvl(3,ngm)
   INTEGER, INTENT(IN) :: ngm, nr1, nr2, nr3
   INTEGER, OPTIONAL, INTENT(OUT) :: fft2gv(0:)
   INTEGER, OPTIONAL, INTENT(OUT) :: gv2fft(:)
   INTEGER :: ig, nx, ny, nz, npoint

   CALL log_push( 'ggrids_gv_indexes' )
   !
   IF ( PRESENT(gv2fft) ) gv2fft = 0
   IF ( PRESENT(fft2gv) ) fft2gv = 0

   DO ig = 1, ngm

      nx = -100000
      ny = -100000
      nz = -100000
      !
      IF ( igvl(1,ig) >= 0 ) nx = igvl(1,ig) + 1
      IF ( igvl(1,ig) <  0 ) nx = igvl(1,ig) + 1 + nr1
      IF ( igvl(2,ig) >= 0 ) ny = igvl(2,ig) + 1
      IF ( igvl(2,ig) <  0 ) ny = igvl(2,ig) + 1 + nr2
      IF ( igvl(3,ig) >= 0 ) nz = igvl(3,ig) + 1
      IF ( igvl(3,ig) <  0 ) nz = igvl(3,ig) + 1 + nr3       
   
      npoint = nx + (ny-1)*nr1 + (nz-1)*nr1*nr2

      IF( PRESENT( gv2fft ) ) gv2fft(ig) = npoint  ! index
      IF( PRESENT( fft2gv ) ) fft2gv(npoint) = ig  ! index

   ENDDO
   !
   CALL log_pop( 'ggrids_gv_indexes' )
   !
END SUBROUTINE ggrids_gv_indexes


!**********************************************************
   SUBROUTINE ggrids_summary( iunit )
   !**********************************************************
   !
   ! Writes summary of the main quantities and dimensions in the
   ! module
   !
   IMPLICIT NONE
   INTEGER, INTENT(IN) :: iunit
   !
   !
   WRITE(iunit, "(/,6x,'    Energy cut-off for rho  =  ',5x,F7.2,' (Ry)' )") ecutrho
   WRITE(iunit, "(  6x,'Total number of PW for rho  =  ',i9)") npw_rho
   !
   IF ( have_smooth_rhogrid ) &
      WRITE(iunit, "(6x,'              (smooth grid) =  ',i9)") npws_rho
   !
   WRITE(iunit, "(6x,'  FFT grid components (rho) =  ( ', 3i5,' )' )") nfft(:)
   !
   IF ( have_smooth_rhogrid ) &
      WRITE(iunit, "(6x,'              (smooth grid) =  ( ', 3i5,' )' )") nffts(:)
   !
   !
END SUBROUTINE ggrids_summary
   

END MODULE ggrids_module

