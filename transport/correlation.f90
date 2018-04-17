!
! Copyright (C) 2005 WanT Group
!
! This file is distributed under the terms of the
! GNU General Public License. See the file `License\'
! in the root directory of the present distribution,
! or http://www.gnu.org/copyleft/gpl.txt .
!
!*********************************************
   MODULE T_correlation_module
!*********************************************
   USE kinds,                ONLY : dbl
   USE constants,            ONLY : EPS_m4
   USE parameters,           ONLY : nstrx
   USE T_egrid_module,       ONLY : ne, egrid, egrid_alloc => alloc
   USE T_kpoints_module,     ONLY : nkpts_par, nrtot_par, vr_par, kpoints_alloc => alloc
   USE T_hamiltonian_module, ONLY : dimL, dimC, dimR, dimx
   USE T_control_module,     ONLY : transport_dir
   USE timing_module,        ONLY : timing
   USE iotk_module
   IMPLICIT NONE
   PRIVATE 
   SAVE

!
! Contains correlation self-energy data
! 
    INTEGER                   :: dimC_file, nrtot
    INTEGER                   :: ncols, nrows
    INTEGER, ALLOCATABLE      :: icols(:), irows(:)
    !
    INTEGER, ALLOCATABLE      :: ivr_corr(:,:)
    COMPLEX(dbl), ALLOCATABLE :: sgm_corr(:,:,:)
    !
    LOGICAL :: alloc = .FALSE.


!
! end delcarations
!

   PUBLIC :: dimL, dimC, dimR, dimx     
   PUBLIC :: nkpts_par
   !
   PUBLIC :: ncols, nrows
   PUBLIC :: icols, irows
   PUBLIC :: ivr_corr
   PUBLIC :: sgm_corr
   !
   PUBLIC :: alloc
   !
   PUBLIC :: correlation_allocate
   PUBLIC :: correlation_deallocate
   PUBLIC :: correlation_init
   PUBLIC :: correlation_sgmread


CONTAINS

!
! subroutines
!

!**********************************************************
   SUBROUTINE correlation_allocate()
   !**********************************************************
   IMPLICIT NONE
      CHARACTER(20)  :: subname="correlation_allocate"
      INTEGER        :: ierr

      IF ( alloc )               CALL errore(subname,'already allocated', 1 )
      IF ( .NOT. kpoints_alloc ) CALL errore(subname,'kpoints not alloc', 1 )
      IF ( dimC <= 0 )           CALL errore(subname,'invalid dimC', 1 )
      IF ( nkpts_par <= 0 )      CALL errore(subname,'invalid nkpts_par', 1 )

      !
      ! allocation       
      !
      ALLOCATE ( sgm_corr(dimC,dimC,nkpts_par), STAT=ierr )
           IF( ierr /=0 ) CALL errore(subname, 'allocating sgm_corr', ABS(ierr) )
      alloc = .TRUE.

   END SUBROUTINE correlation_allocate


!**********************************************************
   SUBROUTINE correlation_init(unit)
   !**********************************************************
   !
   ! opne the sigma file and allocate the main workspace
   !
   IMPLICIT NONE
      INTEGER, INTENT(in):: unit
      CHARACTER(16)    :: subname="correlation_init"
      CHARACTER(nstrx) :: attr
      REAL(dbl), ALLOCATABLE :: grid_file(:)
      LOGICAL          :: opened
      INTEGER          :: nomega, ie, ierr

      IF ( .NOT. egrid_alloc )   CALL errore(subname,'egrid not alloc', 1 )


      INQUIRE( unit, OPENED=opened )
      IF ( .NOT. opened ) CALL errore(subname, 'unit not opened', 3 )
      !
      ! get main data and check them
      !
      CALL iotk_scan_empty(unit, "DATA", ATTR=attr, IERR=ierr)
      IF (ierr/=0) CALL errore(subname,'searching DATA',ABS(ierr))
      !
      CALL iotk_scan_attr(attr,"dimwann",dimC_file, IERR=ierr)
      IF (ierr/=0) CALL errore(subname,'searching dimwann',ABS(ierr))
      !
      CALL iotk_scan_attr(attr,"nrtot",nrtot, IERR=ierr)
      IF (ierr/=0) CALL errore(subname,'searching nrtot',ABS(ierr))
      !
      CALL iotk_scan_attr(attr,"nomega",nomega, IERR=ierr)
      IF (ierr/=0) CALL errore(subname,'searching nomega',ABS(ierr))
      !
      CALL iotk_scan_dat(unit, "VR", ivr_corr, IERR=ierr)
      IF (ierr/=0) CALL errore(subname,'searching VR',ABS(ierr))
      !
      ALLOCATE( grid_file(nomega), STAT=ierr )
      IF (ierr/=0) CALL errore(subname,'allocating grid',ABS(ierr))
      CALL iotk_scan_dat(unit, "GRID", grid_file, IERR=ierr)
      IF (ierr/=0) CALL errore(subname,'searching GRID',ABS(ierr))
      ! 
      IF ( dimC_file > dimC) CALL errore(subname,'invalid dimC_file',3)
      IF ( nrtot <= 0 ) CALL errore(subname,'invalid nrtot',3)
      !
      ALLOCATE ( ivr_corr(3,nrtot), STAT=ierr )
           IF( ierr /=0 ) CALL errore(subname, 'allocating ivr_corr', ABS(ierr) )
      !
      IF ( nomega /= ne) CALL errore(subname,'invalid nomega',3)
      DO ie=1, ne
          IF ( ABS(egrid(ie)-grid_file(ie)) > EPS_m4 ) &
             CALL errore(subname,'invalid grid',ie)
      ENDDO
      !
      DEALLOCATE( grid_file, STAT=ierr )
      IF (ierr/=0) CALL errore(subname,'deallocating grid',ABS(ierr))
      
   END SUBROUTINE correlation_init


!**********************************************************
   SUBROUTINE correlation_deallocate()
   !**********************************************************
   IMPLICIT NONE
      CHARACTER(22)      :: subname="correlation_deallocate"
      INTEGER :: ierr

      IF ( .NOT. alloc ) RETURN

      IF ( ALLOCATED( sgm_corr ) ) THEN
         DEALLOCATE ( sgm_corr, STAT=ierr )
         IF( ierr /=0 ) CALL errore(subname,'deallocating sgm_corr', ABS(ierr) )
      ENDIF
      IF ( ALLOCATED( ivr_corr ) ) THEN
         DEALLOCATE ( ivr_corr, STAT=ierr )
         IF( ierr /=0 ) CALL errore(subname,'deallocating ivr_corr', ABS(ierr) )
      ENDIF
      IF ( ALLOCATED( icols ) ) THEN
         DEALLOCATE ( icols, STAT=ierr )
         IF( ierr /=0 ) CALL errore(subname,'deallocating icols', ABS(ierr) )
      ENDIF
      IF ( ALLOCATED( irows ) ) THEN
         DEALLOCATE ( irows, STAT=ierr )
         IF( ierr /=0 ) CALL errore(subname,'deallocating irows', ABS(ierr) )
      ENDIF
      alloc = .FALSE.   

   END SUBROUTINE correlation_deallocate


!**********************************************************
   SUBROUTINE correlation_sgmread(unit, ie, opr )
   !**********************************************************
   IMPLICIT NONE
      INTEGER,       INTENT(in)  :: unit, ie
      COMPLEX(dbl),  INTENT(out) :: opr(:,:,:)
      !
      CHARACTER(19)              :: subname="correlation_sgmread"
      COMPLEX(dbl), ALLOCATABLE  :: aux(:,:), aux_small(:,:,:)
      LOGICAL                    :: opened, found
      INTEGER                    :: index, ivr_aux(3)
      INTEGER                    :: i, j, ir, ir_par, ierr


      CALL timing( 'correlation_sgmread', OPR='start' )
 
      IF ( SIZE(opr,1) /= dimC ) CALL errore(subname,'invalid dim',1)
      IF ( SIZE(opr,2) /= dimC ) CALL errore(subname,'invalid dim',2)
      IF ( SIZE(opr,3) /= nkpts_par ) CALL errore(subname,'invalid dim',3)

      INQUIRE( unit, OPENED=opened )
      IF ( .NOT. opened ) CALL errore(subname,'unit is closed',4)

      !
      ! scan for the desired frequency
      !
      CALL iotk_scan_begin(unit, "OPR"//TRIM(iotk_index(ie)), IERR=ierr)
      IF (ierr/=0) CALL errore(subname,'searching for OPR',ie)

      !
      ! allocate auxiliary quantities
      !
      ALLOCATE( aux(dimC_file, dimC_file), STAT=ierr )
      IF ( ierr/=0 ) CALL errore(subname, 'allocating aux', ABS(ierr))
      ALLOCATE( aux_small(dimC, dimC, nrtot_par), STAT=ierr )
      IF ( ierr/=0 ) CALL errore(subname, 'allocating aux_small', ABS(ierr))
      !

      R_loop: &
      DO ir_par = 1,nrtot_par

	 !
         ! set the indexes
         !
         j = 0
         DO i=1,3
            IF ( i == transport_dir ) THEN
               ivr_aux(i) = 0
            ELSE
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
            IF ( ALL( ivr_corr(:,ir) == ivr_aux(:) ) )  THEN
                 found = .TRUE.
                 index = ir
                 EXIT
            ENDIF
         ENDDO
         !
         IF ( .NOT. found ) CALL errore(subname, '3D R-vector not found', ir_par )


         !
         ! read the 3D R matrix corresponding to index
         !
         CALL iotk_scan_dat( unit, "VR"//TRIM(iotk_index(index)), aux, IERR=ierr)
         IF (ierr/=0) &
            CALL errore(subname, 'searching VR'//TRIM(iotk_index(index)), ABS(ierr) )

         !
         ! cut the operator (aux) according to the required rows and cols
         !
         DO j=1,ncols
         DO i=1,nrows
            aux_small(i, j, ir_par) = aux( irows(i), icols(j) )
         ENDDO
         ENDDO

      ENDDO R_loop

      CALL iotk_scan_end(unit, "OPR"//TRIM(iotk_index(ie)), IERR=ierr)
      IF (ierr/=0) CALL errore(subname,'searching end for OPR',ie)

      !
      ! finally compute 2D fourier transform
      !
      CALL fourier_par (opr, dimC, dimC, aux_small, dimC, dimC)

      !
      ! cleaning
      !
      DEALLOCATE( aux, aux_small, STAT=ierr )
      IF ( ierr/=0 ) CALL errore(subname, 'deallocating aux, aux_small', ABS(ierr))

      CALL timing( 'correlation_sgmread', OPR='stop' )
   END SUBROUTINE correlation_sgmread
    

END MODULE T_correlation_module

