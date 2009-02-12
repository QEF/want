!
! Copyright (C) 2004 WanT Group
!
! This file is distributed under the terms of the
! GNU General Public License. See the file `License\'
! in the root directory of the present distribution,
! or http://www.gnu.org/copyleft/gpl.txt .
!

!*********************************************
   MODULE wfc_data_module
!*********************************************
   !
   USE kinds,             ONLY : dbl
   USE constants,         ONLY : CZERO, ZERO, TWO
   USE parameters,        ONLY : nstrx
   USE windows_module,    ONLY : nbnd, dimwinx, imin, imax, ispin, nspin
   USE kpoints_module,    ONLY : kpoints_alloc, nkpts_g
   USE lattice_module,    ONLY : lattice_alloc => alloc, bvec, tpiba
   USE timing_module,     ONLY : timing
   USE log_module,        ONLY : log_push, log_pop
   USE io_global_module,  ONLY : ionode, ionode_id
   USE ggrids_module,     ONLY : ggrids_alloc => alloc, ecutwfc, ecutrho
   USE mp,                ONLY : mp_bcast
   USE wfc_info_module
   !
   USE qexml_module
   USE qexpt_module
   !
#ifdef __ETSF_IO
   USE etsf_io
   USE etsf_io_tools
   USE etsf_io_data_module
#endif
   !
   IMPLICIT NONE
   PRIVATE
   SAVE

! This module handles data referring PW representation
! of wave-functions, including the grids
!
! routines in this module:
! SUBROUTINE wfc_data_deallocate()
! SUBROUTINE wfc_data_grids_read( filefmt )
! SUBROUTINE wfc_data_grids_summary( iunit )
! SUBROUTINE wfc_data_kread(unit,ik_g,label,wfc,lwfc[,iband_min][,iband_max])
!

!
! declarations of common variables
!   

   INTEGER                   :: npwkx            ! max number of G vects over kpts +1 
                                                 ! the last position is used in overlap
   INTEGER,      ALLOCATABLE :: npwk(:)          ! number of G for each kpt, DIM: nkpts_g
   INTEGER,      ALLOCATABLE :: igsort(:,:)      ! G map between the global IGV array and
                                                 ! the local ordering for each kpt
                                                 ! DIM: npwkx, nkpts_g

!
! ... Bloch eigenvectors
!
   COMPLEX(dbl), ALLOCATABLE :: evc(:,:)         ! wfc data; store wfc related to one or more
                                                 ! kpts: current impl store wfc for k and b
                                                 ! DIM: npwx, nvect
   TYPE(wfc_info)            :: evc_info         ! the descriptof of the wfcs in EVC

   !
   LOGICAL :: alloc = .FALSE.

!
! end of declarations
!

   PUBLIC :: npwkx
   PUBLIC :: npwk
   PUBLIC :: igsort
   PUBLIC :: evc
   PUBLIC :: evc_info
   PUBLIC :: ecutwfc
   PUBLIC :: alloc

   PUBLIC :: wfc_data_deallocate
   PUBLIC :: wfc_data_memusage
   PUBLIC :: wfc_data_grids_read 
   PUBLIC :: wfc_data_grids_summary
   PUBLIC :: wfc_data_kread

CONTAINS

!
! subroutines
!

!**********************************************************
   SUBROUTINE wfc_data_grids_read( filefmt )
   !**********************************************************
   IMPLICIT NONE
       CHARACTER(*), INTENT(IN) :: filefmt
       !
       CHARACTER(19)        :: subname="wfc_data_grids_read"
       INTEGER              :: ik_g, ierr 
       !
#ifdef __ETSF_IO
       INTEGER              :: nfft(3), ig, npw_rho
       REAL(dbl)            :: gcutm, b1(3), b2(3), b3(3)
       INTEGER, ALLOCATABLE :: gvectors_aux(:,:)
       INTEGER, ALLOCATABLE :: gmap(:,:,:)
       !
       TYPE(etsf_basisdata) :: basisdata       
       INTEGER,          ALLOCATABLE, TARGET :: number_of_coefficients(:)
       INTEGER,          ALLOCATABLE, TARGET :: reduced_coordinates_of_plane_waves(:,:,:)
#endif


       CALL timing ( subname, OPR="START")
       CALL log_push( subname )

       IF ( .NOT. lattice_alloc ) CALL errore(subname,'lattice not alloc',10)
       IF ( .NOT. kpoints_alloc ) CALL errore(subname,'kpoints not alloc',10)
       IF ( .NOT. ggrids_alloc )  CALL errore(subname,'ggrids not alloc',10)

       !
       !
       ! few checks
       !
       IF ( dimwinx <= 0 )   CALL errore(subname,'dimwinx <= 0', ABS(dimwinx)+1)
       IF ( nkpts_g <= 0 )   CALL errore(subname,'nkpts_g <= 0', ABS(nkpts_g)+1)
       IF ( nbnd <= 0 )      CALL errore(subname,'nbnd <= 0',    ABS(nbnd)+1)
       !
       ALLOCATE( npwk(nkpts_g), STAT=ierr )
       IF (ierr/=0) CALL errore(subname,'allocating npwk',ABS(ierr))

       !
       !
       ! ... reads dimensions for grids
       !     npwkx is obtained from the loop and not read because of 
       !     problems with the formats and thier back readability
       !
       npwk(1:nkpts_g) = 0
       !
       IF ( ionode ) THEN
           !
           SELECT CASE ( TRIM(filefmt) )
           !
           CASE ( 'qexml' )
                !
                DO ik_g = 1, nkpts_g
                    !
                    CALL qexml_read_gk( ik_g, NPWK=npwk(ik_g), IERR=ierr )
                    IF ( ierr/=0) CALL errore(subname,'QEXML: getting npwk',ik_g)
                    !
                ENDDO
                !
           CASE ( 'pw_export' )
                !
                DO ik_g = 1, nkpts_g
                    !
                    CALL qexpt_read_gk( ik_g, NPWK=npwk(ik_g), IERR=ierr )
                    IF ( ierr/=0) CALL errore(subname,'QEXPT: getting npwk',ik_g)
                    !
                ENDDO
                !
           CASE ( 'etsf_io' )
                !
#ifdef __ETSF_IO
                !
                ALLOCATE( number_of_coefficients(dims%number_of_kpoints) )
                !
                basisdata%number_of_coefficients   => number_of_coefficients
                !
                call etsf_io_basisdata_get(ncid, basisdata, lstat, error_data)
                IF ( .NOT. lstat) CALL etsf_error(error_data,subname,'ETSF_IO: getting npwk',10)
                !
                basisdata%number_of_coefficients   => null()
                !
                DO ik_g = 1, nkpts_g
                    !
                    npwk( ik_g ) = number_of_coefficients(ik_g)
                    !
                ENDDO
                !
                DEALLOCATE( number_of_coefficients )
                !
#else
                CALL errore(subname,'ETSF_IO not configured',10)
#endif
                !
           CASE DEFAULT
                !
                CALL errore(subname,'invalid filefmt = '//TRIM(filefmt), 1)
           END SELECT
           !
       ENDIF
       !
       CALL mp_bcast( npwk,   ionode_id )
       !
       npwkx = MAXVAL( npwk( 1: nkpts_g ) )

       !
       ! Allocations
       !

       IF ( npwkx <= 0 )     CALL errore(subname,'npwkx <= 0',   ABS(npwkx)+1)
       !
       ! WARNING: nasty redefinition to have one component of the array free
       !
       npwkx = npwkx + 1
       !
       ! igsort is allocated globally since it is not-at-all memory consuming
       !
       ALLOCATE( igsort(npwkx,nkpts_g), STAT=ierr )
       IF (ierr/=0) CALL errore(subname,'allocating igsort',ABS(ierr))

       !
       ! init
       igsort(:,:) = 0

       !
       ! read massive data
       !
       IF ( ionode ) THEN
           !
           SELECT CASE ( TRIM(filefmt) )
           !
           CASE ( 'qexml' )
                !
                DO ik_g = 1, nkpts_g
                    !
                    CALL qexml_read_gk( ik_g, INDEX=igsort( 1:npwk(ik_g), ik_g), IERR=ierr )
                    IF ( ierr/=0) CALL errore(subname,'getting igsort',ik_g )
                    !
                ENDDO
                !
           CASE ( 'pw_export' )
                !
                DO ik_g = 1, nkpts_g
                    !
                    CALL qexpt_read_gk( ik_g, INDEX=igsort( 1:npwk(ik_g), ik_g), IERR=ierr )
                    IF ( ierr/=0) CALL errore(subname,'getting igsort',ik_g)
                    !
                ENDDO
                !
           CASE( 'etsf_io' )
                !
#ifdef __ETSF_IO
                !
                ALLOCATE( reduced_coordinates_of_plane_waves( dims%number_of_reduced_dimensions, & 
                                                              dims%max_number_of_coefficients,   &
                                                              dims%number_of_kpoints), STAT=ierr )
                IF ( ierr/=0 ) CALL errore(subname,'allocating reduced_coordinates_of_plane_waves',10)
                !
                basisdata%reduced_coordinates_of_plane_waves%data3d =>  &
                                              reduced_coordinates_of_plane_waves(:,:,:)
                !
                CALL etsf_io_basisdata_get(ncid, basisdata, lstat, error_data)
                IF(.NOT.lstat) CALL etsf_error(error_data,subname,'ETSF_IO: reading plane-waves',10)
                !
                basisdata%reduced_coordinates_of_plane_waves%data3d => null()

                !
                ! we have to feel igksort with the map between the PWs of each
                ! kpt and the G-grid of the density.
                ! To do this, we regenerate locally the main G-grid use the map
                ! given by gglobal
                !
                
                ! aux data
                gcutm   = ecutrho / tpiba**2
                !
                b1 = bvec(:,1) / tpiba
                b2 = bvec(:,2) / tpiba
                b3 = bvec(:,3) / tpiba
                !
                nfft(1) = dims%number_of_grid_points_vector1
                nfft(2) = dims%number_of_grid_points_vector2
                nfft(3) = dims%number_of_grid_points_vector3
                !
                ALLOCATE( gvectors_aux(3, PRODUCT(nfft)), STAT=ierr )
                IF ( ierr/=0 ) CALL errore(subname,'allocating gvectors_aux',ABS(ierr))
                !
                ALLOCATE( gmap(-nfft(1):nfft(1), -nfft(2):nfft(2), -nfft(3):nfft(3)), STAT=ierr)
                IF ( ierr/=0 ) CALL errore(subname,'allocating gmap',ABS(ierr))
                !
                CALL gglobal( npw_rho, gvectors_aux, gmap, b1, b2, b3, &
                              nfft(1), nfft(2), nfft(3), gcutm, .FALSE. )
                !
                DO ik_g = 1, nkpts_g
                    !
                    DO ig = 1, npwk( ik_g )
                        !
                        igsort( ig, ik_g) = gmap( reduced_coordinates_of_plane_waves(1,ig,ik_g), &
                                                  reduced_coordinates_of_plane_waves(2,ig,ik_g), &
                                                  reduced_coordinates_of_plane_waves(3,ig,ik_g)  )
                        !
                    ENDDO
                    !
                ENDDO


                DEALLOCATE( reduced_coordinates_of_plane_waves, STAT=ierr )
                IF ( ierr/=0 ) CALL errore(subname,'deallocating reduced_coordinates_of_plane_waves',10)
                !
                DEALLOCATE( gvectors_aux, gmap, STAT=ierr)
                IF ( ierr/=0 ) CALL errore(subname,'deallocating gvectors_aux, gmap',ABS(ierr))
                !
#endif
                !
           CASE DEFAULT
                !
                CALL errore(subname,'invalid filefmt = '//TRIM(filefmt), 1)
           END SELECT
           !
       ENDIF
       !
       CALL mp_bcast( igsort,    ionode_id )
       !
       alloc = .TRUE.
       !
       CALL timing ( subname, OPR="STOP")
       CALL log_pop( subname )
       !
   END SUBROUTINE wfc_data_grids_read


!**********************************************************
   SUBROUTINE wfc_data_deallocate()
   !**********************************************************
   IMPLICIT NONE
       CHARACTER(19)      :: subname="wfc_data_deallocate"
       INTEGER            :: ierr

       CALL log_push( subname )
       !
       IF ( ALLOCATED(npwk) ) THEN
            DEALLOCATE(npwk, STAT=ierr)
            IF (ierr/=0)  CALL errore(subname,' deallocating npwk ',ABS(ierr))
       ENDIF
       IF ( ALLOCATED(igsort) ) THEN
            DEALLOCATE(igsort, STAT=ierr)
            IF (ierr/=0)  CALL errore(subname,' deallocating igsort ',ABS(ierr))
       ENDIF
       IF ( ALLOCATED(evc) ) THEN
            DEALLOCATE(evc, STAT=ierr)
            IF (ierr/=0)  CALL errore(subname,' deallocating evc ',ABS(ierr))
       ENDIF
       IF ( evc_info%alloc ) CALL wfc_info_deallocate(evc_info)
       !
       alloc = .FALSE.
       !
       CALL log_pop( subname )
       !
   END SUBROUTINE wfc_data_deallocate


!**********************************************************
   REAL(dbl) FUNCTION wfc_data_memusage()
   !**********************************************************
   IMPLICIT NONE
       !   
       REAL(dbl) :: cost
       !
       cost = ZERO
       IF ( ALLOCATED(npwk) )   cost = cost + REAL(SIZE(npwk))   *  4.0_dbl
       IF ( ALLOCATED(igsort) ) cost = cost + REAL(SIZE(igsort)) *  4.0_dbl
       IF ( ALLOCATED(evc) )    cost = cost + REAL(SIZE(evc))    * 16.0_dbl
       !   
       wfc_data_memusage = cost / 1000000.0_dbl
       !   
   END FUNCTION wfc_data_memusage


!*********************************************************
   SUBROUTINE wfc_data_kread( filefmt, ik_g, label, wfc, lwfc, iband_min, iband_max )
   !*********************************************************
   !
   ! read wfc of a selected kpt from file to the type LWFC. 
   ! the object may contain other wfc and free slots are used to
   ! store the new ones.
   ! Using IBAND_MIN = IBAND_MAX it can be used to read one wfc
   ! at the time
   !
   ! in the parallel case, ik_g is a global index 
   !
   IMPLICIT NONE
       INTEGER,           INTENT(IN)    :: ik_g
       CHARACTER(*),      INTENT(IN)    :: label, filefmt
       COMPLEX(dbl),      INTENT(INOUT) :: wfc(:,:)
       TYPE(wfc_info),    INTENT(INOUT) :: lwfc
       INTEGER, OPTIONAL, INTENT(IN)    :: iband_min, iband_max

       CHARACTER(14)        :: subname="wfc_data_kread"
       INTEGER              :: ib, ibs, ibe, lindex, ierr
       !
#ifdef __ETSF_IO
       INTEGER              :: npw
       TYPE(etsf_main)      :: main
       TYPE(etsf_basisdata) :: basisdata
       !
       INTEGER,          ALLOCATABLE, TARGET :: number_of_coefficients(:)
       DOUBLE PRECISION, ALLOCATABLE, TARGET :: coefficients_of_wavefunctions(:,:,:)
#endif

       !
       CALL timing ( subname,OPR='start')
       CALL log_push( subname )

       !
       ! lwfc is supposed to be already allocated
       IF ( .NOT. lwfc%alloc ) CALL errore(subname,'lwfc not yet allocated',1)
       IF ( ik_g <= 0 .OR. ik_g > nkpts_g ) CALL errore(subname,'invalid ik_g',2)

       ibs = imin(ik_g)
       ibe = imax(ik_g)
       IF ( PRESENT(iband_min) ) ibs = iband_min
       IF ( PRESENT(iband_max) ) ibe = iband_max

       IF ( ibs < imin(ik_g) ) CALL errore(subname,'Invalid iband_min',2)
       IF ( ibe > imax(ik_g) ) CALL errore(subname,'Invalid iband_max',3)

       !
       ! setting the wfc descriptor
       !
       CALL wfc_info_add(npwk(ik_g), ibs, ik_g, TRIM(label), lwfc, INDEX=lindex )
       !
       DO ib = ibs+1, ibe
            !
            CALL wfc_info_add(npwk(ik_g), ib, ik_g, TRIM(label), lwfc )
            !
       ENDDO
  
       !
       ! ... actual reading
       !
       SELECT CASE ( TRIM(filefmt) )
       !
       CASE ( 'qexml' )
            !
            IF ( nspin == 2 ) THEN
               !
               CALL qexml_read_wfc( ibs, ibe, ik_g, ISPIN= ispin, IGK=igsort(:,ik_g), &
                                    WF=wfc(:, lindex: ), IERR=ierr )
               !
            ELSE
               !
               CALL qexml_read_wfc( ibs, ibe, ik_g, IGK=igsort(:,ik_g), &
                                    WF=wfc(:, lindex: ), IERR=ierr )
               !
            ENDIF
            !
       CASE ( 'pw_export' )
            !
            CALL qexpt_read_wfc( ibs, ibe, ik_g, ispin, IGK=igsort(:,ik_g), &
                                 WF=wfc(:, lindex: ), IERR=ierr )
            !
       CASE ( 'etsf_io' )
            !
#ifdef __ETSF_IO
            !
            !
            IF ( dims%number_of_spins/= 1 .AND. dims%number_of_spinor_components/=1)   &
            CALL errore(subname,'too many spin and spinor components',1)
            !
            IF ( dims%number_of_spinor_components /= 1 ) &
               CALL errore(subname,'spinorial wfcs not implemented',10)
            !
            ALLOCATE( coefficients_of_wavefunctions(dims%real_or_complex_coefficients, &
                                                    dims%max_number_of_coefficients,   & 
                                                    dims%max_number_of_states), STAT=ierr)
            IF (ierr/=0 ) CALL errore(subname,'allocating coeff_of_wfcs',ABS(ierr))
            !
            ALLOCATE( number_of_coefficients(dims%number_of_kpoints), STAT=ierr )
            IF (ierr/=0 ) CALL errore(subname,'allocating numb_of_coeff',ABS(ierr))
            !
            !
            basisdata%number_of_coefficients           => number_of_coefficients 
            !
            CALL etsf_io_basisdata_get(ncid, basisdata, lstat, error_data)
            IF(.NOT.lstat) CALL etsf_error(error_data,subname,'ETSF_IO: reading basisdata',ABS(ierr))
            !
            basisdata%number_of_coefficients           => null()
            !
            !
            main%wfs_coeff__kpoint_access = ik_g
            main%wfs_coeff__spin_access   = ispin
            main%coefficients_of_wavefunctions%data3D => coefficients_of_wavefunctions
            !
            CALL etsf_io_main_get(ncid, main, lstat, error_data)
            IF(.NOT.lstat) CALL etsf_error(error_data,subname,'ETSF_IO: reading wfcs',ABS(ierr))
            !
            main%coefficients_of_wavefunctions%data3D => null()
            !
            npw = number_of_coefficients( ik_g )
            !
            wfc( 1 : npw, lindex : lindex+ibe-ibs ) = &
                    CMPLX( coefficients_of_wavefunctions(1, 1:npw, ibs:ibe ), &
                           coefficients_of_wavefunctions(2, 1:npw, ibs:ibe ), dbl )
            !
            DEALLOCATE( coefficients_of_wavefunctions, STAT=ierr )
            IF (ierr/=0 ) CALL errore(subname,'deallocating coeff_of_wfcs',ABS(ierr))
            DEALLOCATE( number_of_coefficients, STAT=ierr )
            IF (ierr/=0 ) CALL errore(subname,'deallocating numb_of_coeff',ABS(ierr))
            !
#else
            CALL errore(subname,'ETSF_IO not configure', 10)
#endif
            !
       CASE DEFAULT
            !
            CALL errore(subname,'invalid filefmt = '//TRIM(filefmt), 1)
            !
       END SELECT
       !
       IF ( ierr/=0) CALL errore(subname,'reading wfcs',ABS(ierr))
       !
       !
       CALL timing(subname,OPR='stop')
       CALL log_pop( subname )
       ! 
   END SUBROUTINE wfc_data_kread


!**********************************************************
   SUBROUTINE wfc_data_grids_summary( iunit )
   !**********************************************************
   !
   ! Writes summary of the main quantities and dimensions in the
   ! module
   !
   IMPLICIT NONE
       INTEGER, INTENT(IN) :: iunit
       !
       IF ( ionode ) THEN
           !
           WRITE(iunit, "(/,6x,'    Energy cut-off for wfcs =  ',5x,F7.2,' (Ry)' )") ecutwfc
           !
           ! we subtract 1 because of the internal redefinition of the parameter npwkx
           ! wrt the one used in DFT (see wfc_data_grids_read)
           !
           WRITE(iunit, "(  6x,'  Max number of PW for wfc  =  ',i9)") npwkx -1
           !!!WRITE(iunit, "(  6x,'Total number of PW for wfcs =  ',i9)") MAXVAL(igsort(:,:))
           !
           WRITE(iunit, "()" )
           !
       ENDIF
       !
   END SUBROUTINE wfc_data_grids_summary

END MODULE wfc_data_module

