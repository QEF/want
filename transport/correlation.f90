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
   !
   USE kinds,                ONLY : dbl
   USE constants,            ONLY : EPS_m4
   USE parameters,           ONLY : nstrx
   USE io_module,            ONLY : ionode, ionode_id, stdout, sgm_unit
   USE mp,                   ONLY : mp_bcast
   USE parser_module,        ONLY : change_case
   USE operator_module,      ONLY : operator_read_aux
   USE T_egrid_module,       ONLY : de, ne, egrid, emin, emax, egrid_alloc => alloc
   USE T_kpoints_module,     ONLY : nkpts_par, nrtot_par, vr_par, kpoints_alloc => alloc
   USE T_hamiltonian_module, ONLY : dimC
   USE T_control_module,     ONLY : transport_dir
   USE timing_module,        ONLY : timing
   USE log_module,           ONLY : log_push, log_pop
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

   PUBLIC :: dimC
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
      
      CALL log_push( 'correlation_allocate' )

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
      CALL log_pop( 'correlation_allocate' )

   END SUBROUTINE correlation_allocate


!**********************************************************
   SUBROUTINE correlation_init(unit)
   !**********************************************************
   !
   ! open the sigma file and allocate the main workspace
   ! energy grid is read from file
   !
   IMPLICIT NONE
      INTEGER, INTENT(in):: unit
      CHARACTER(16)      :: subname="correlation_init"
      CHARACTER(nstrx)   :: analyticity
      LOGICAL            :: lopen, ldynam
      INTEGER            :: ierr

      CALL log_push( 'correlation_init' )
      IF ( egrid_alloc )   CALL errore(subname,'egrid already allocated', 1 )


      INQUIRE( unit, OPENED=lopen )
      IF ( .NOT. lopen ) CALL errore(subname, 'unit not opened', 3 )
      !
      ! get main data and check them
      !
      IF ( ionode ) THEN
         !
         CALL operator_read_aux( sgm_unit, DIMWANN=dimC_file, NR=nrtot, DYNAMICAL=ldynam, &
                                 NOMEGA=ne, ANALYTICITY=analyticity, IERR=ierr )
         !
         IF ( ierr/=0 ) CALL errore(subname,'reading DIMWANN--ANALYTICITY', ABS(ierr))
         !
      ENDIF
      !
      CALL mp_bcast( dimC_file,    ionode_id )
      CALL mp_bcast( nrtot,        ionode_id )
      CALL mp_bcast( ldynam,       ionode_id )
      CALL mp_bcast( ne,           ionode_id )
      CALL mp_bcast( analyticity,  ionode_id )
      !
      !
      IF ( dimC_file > dimC) CALL errore(subname,'invalid dimC_file',3)
      IF ( nrtot <= 0 )      CALL errore(subname,'invalid nrtot',3)
      IF ( ne <= 0 )         CALL errore(subname,'invalid ne',3)
      IF ( .NOT. ldynam )    CALL errore(subname,'sgm is static', 10 )
      !
      CALL change_case( analyticity, 'lower' )
      IF ( TRIM(analyticity) /= 'retarded' ) CALL errore(subname,'sgm is not retarded', 10 )
      !
      !
      ALLOCATE( egrid(ne), STAT=ierr )
      IF (ierr/=0) CALL errore(subname,'allocating egrid',ABS(ierr))
      !
      ALLOCATE ( ivr_corr(3,nrtot), STAT=ierr )
      IF( ierr /=0 ) CALL errore(subname, 'allocating ivr_corr', ABS(ierr) )
      !
      !
      IF ( ionode ) THEN
         !
         CALL operator_read_aux( sgm_unit, GRID=egrid, IVR=ivr_corr, IERR=ierr )
         IF (ierr/=0) CALL errore(subname,'reading GRID, IVR',ABS(ierr))
         !
         CALL warning( stdout, "energy egrid is forced from SGM datafile" )
         WRITE( stdout, "()")
         !
      ENDIF
      !
      CALL mp_bcast( egrid,         ionode_id )
      CALL mp_bcast( ivr_corr,     ionode_id )
      ! 
      !
      ! set further data about the energy grid
      !
      emin = egrid(1)
      emax = egrid(ne)
      de   = ( emax - emin ) / REAL( ne -1, dbl )  
      egrid_alloc = .TRUE.
      !
      !
      CALL log_pop( 'correlation_init' )
      ! 
   END SUBROUTINE correlation_init


!**********************************************************
   SUBROUTINE correlation_deallocate()
   !**********************************************************
   IMPLICIT NONE
      CHARACTER(22)      :: subname="correlation_deallocate"
      INTEGER :: ierr

      CALL log_push( 'correlation_deallocate' )

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

      CALL log_pop( 'correlation_deallocate' )

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
      CALL log_push( 'correlation_sgmread' )
 
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
      CALL log_pop( 'correlation_sgmread' )
   END SUBROUTINE correlation_sgmread
    

END MODULE T_correlation_module

