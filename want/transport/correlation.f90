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
   USE operator_module,      ONLY : operator_read_aux, operator_read_data
   USE T_egrid_module,       ONLY : de, ne, egrid, emin, emax, egrid_alloc => alloc
   USE T_kpoints_module,     ONLY : nkpts_par, nrtot_par, vr_par, kpoints_alloc => alloc
   USE T_hamiltonian_module, ONLY : dimC
   USE T_control_module,     ONLY : transport_dir
   USE timing_module,        ONLY : timing
   USE log_module,           ONLY : log_push, log_pop
   !
   IMPLICIT NONE
   PRIVATE 
   SAVE

!
! Contains correlation self-energy data
! 
    INTEGER                   :: dimC_corr
    INTEGER                   :: nrtot_corr
    REAL(dbl)                 :: shift_corr
    LOGICAL                   :: lhave_corr   = .FALSE.
    LOGICAL                   :: ldynam_corr  = .FALSE.
    !
    INTEGER                   :: ncols_corr, nrows_corr
    INTEGER, ALLOCATABLE      :: icols_corr(:), irows_corr(:)
    !
    INTEGER, ALLOCATABLE      :: ivr_corr(:,:)
    COMPLEX(dbl), ALLOCATABLE :: sgm_corr(:,:,:)
    !
    LOGICAL :: alloc = .FALSE.
    LOGICAL ::  init = .FALSE.


!
! end delcarations
!

   PUBLIC :: dimC
   PUBLIC :: nkpts_par
   !
   PUBLIC :: dimC_corr
   PUBLIC :: nrtot_corr
   PUBLIC :: shift_corr
   PUBLIC :: lhave_corr, ldynam_corr
   !
   PUBLIC :: ncols_corr, nrows_corr
   PUBLIC :: icols_corr, irows_corr
   !
   PUBLIC :: ivr_corr
   PUBLIC :: sgm_corr
   !
   PUBLIC :: alloc
   PUBLIC :: init
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
      LOGICAL            :: lopen
      INTEGER            :: ne_corr, ierr

      CALL log_push( 'correlation_init' )
      IF ( egrid_alloc )   CALL errore(subname,'egrid already allocated', 1 )


      INQUIRE( unit, OPENED=lopen )
      IF ( .NOT. lopen ) CALL errore(subname, 'unit not opened', 3 )
      !
      ! get main data and check them
      !
      IF ( ionode ) THEN
         !
         CALL operator_read_aux( sgm_unit, DIMWANN=dimC_corr, NR=nrtot_corr, &
                                 DYNAMICAL=ldynam_corr, &
                                 NOMEGA=ne_corr, ANALYTICITY=analyticity, IERR=ierr )
         !
         IF ( ierr/=0 ) CALL errore(subname,'reading DIMWANN--ANALYTICITY', ABS(ierr))
         !
      ENDIF
      !
      CALL mp_bcast( dimC_corr,    ionode_id )
      CALL mp_bcast( nrtot_corr,   ionode_id )
      CALL mp_bcast( ldynam_corr,  ionode_id )
      CALL mp_bcast( ne_corr,      ionode_id )
      CALL mp_bcast( analyticity,  ionode_id )
      !
      !
      IF ( dimC_corr > dimC)               CALL errore(subname,'invalid dimC_corr',3)
      IF ( nrtot_corr <= 0 )               CALL errore(subname,'invalid nrtot_corr',3)
      IF ( ne_corr <= 0 .AND. ldynam_corr) CALL errore(subname,'invalid ne_corr',3)
      !
      CALL change_case( analyticity, 'lower' )
      IF ( TRIM(analyticity) /= 'retarded' .AND. ldynam_corr) &
                CALL errore(subname,'invalid analyticity = '//TRIM(analyticity),1)
      !
      !
      ALLOCATE ( ivr_corr(3,nrtot_corr), STAT=ierr )
      IF( ierr /=0 ) CALL errore(subname, 'allocating ivr_corr', ABS(ierr) )
      !
      IF ( ldynam_corr ) THEN
          !
          ne = ne_corr
          !
          ALLOCATE( egrid(ne), STAT=ierr )
          IF (ierr/=0) CALL errore(subname,'allocating egrid',ABS(ierr))
          !
      ENDIF
      !
      !
      IF ( ionode ) THEN
          !
          IF ( ldynam_corr ) THEN
              !
              CALL operator_read_aux( sgm_unit, GRID=egrid, IVR=ivr_corr, IERR=ierr )
              IF (ierr/=0) CALL errore(subname,'reading GRID, IVR',ABS(ierr))
              !
          ELSE
              !
              CALL operator_read_aux( sgm_unit, IVR=ivr_corr, IERR=ierr )
              IF (ierr/=0) CALL errore(subname,'reading IVR',ABS(ierr))
              !
          ENDIF
          !
      ENDIF
      !
      IF ( ldynam_corr ) CALL mp_bcast( egrid, ionode_id )
      CALL mp_bcast( ivr_corr, ionode_id )

      ! 
      !
      ! set further data about the energy grid
      !
      IF ( ldynam_corr ) THEN
          !
          CALL warning( stdout, "energy egrid is forced from SGM datafile" )
          WRITE( stdout, "()")
          !
          emin = egrid(1)
          emax = egrid(ne)
          de   = ( emax - emin ) / REAL( ne -1, dbl )  
          egrid_alloc = .TRUE.
          !
      ENDIF
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
      IF ( ALLOCATED( icols_corr ) ) THEN
         DEALLOCATE ( icols_corr, STAT=ierr )
         IF( ierr /=0 ) CALL errore(subname,'deallocating icols_corr', ABS(ierr) )
      ENDIF
      IF ( ALLOCATED( irows_corr ) ) THEN
         DEALLOCATE ( irows_corr, STAT=ierr )
         IF( ierr /=0 ) CALL errore(subname,'deallocating irows_corr', ABS(ierr) )
      ENDIF
      alloc = .FALSE.   

      CALL log_pop( 'correlation_deallocate' )

   END SUBROUTINE correlation_deallocate


!**********************************************************
   SUBROUTINE correlation_sgmread( iun, opr, ie )
   !**********************************************************
   IMPLICIT NONE
      INTEGER,            INTENT(IN)  :: iun
      COMPLEX(dbl),       INTENT(OUT) :: opr(:,:,:)
      INTEGER, OPTIONAL,  INTENT(IN)  :: ie
      !
      CHARACTER(19)              :: subname="correlation_sgmread"
      COMPLEX(dbl), ALLOCATABLE  :: caux(:,:,:), caux_small(:,:,:)
      LOGICAL                    :: lfound
      INTEGER                    :: ind, ivr_aux(3)
      INTEGER                    :: i, j, ir, ir_par, ierr


      CALL timing( subname, OPR='start' )
      CALL log_push( subname )

      IF ( .NOT. init ) CALL errore(subname,'correlation mod not init',71)
 
      IF ( SIZE(opr,1) < dimC )      CALL errore(subname,'invalid dim',1)
      IF ( SIZE(opr,2) < dimC )      CALL errore(subname,'invalid dim',2)
      IF ( SIZE(opr,3) < nkpts_par ) CALL errore(subname,'invalid dim',3)


      !
      ! allocate auxiliary quantities
      !
      ALLOCATE( caux(dimC_corr, dimC_corr, nrtot_corr), STAT=ierr )
      IF ( ierr/=0 ) CALL errore(subname, 'allocating caux', ABS(ierr))
      !
      ALLOCATE( caux_small(dimC, dimC, nrtot_par), STAT=ierr )
      IF ( ierr/=0 ) CALL errore(subname, 'allocating caux_small', ABS(ierr))

      !
      ! get the required data
      !
      IF ( PRESENT( ie ) ) THEN
          !
          CALL operator_read_data( iun, ie, R_OPR=caux, IERR=ierr )
          !
      ELSE
          !
          CALL operator_read_data( iun, R_OPR=caux, IERR=ierr )
          !
      ENDIF
      !
      IF ( ierr/=0 ) CALL errore(subname, 'reading data from file', ABS(ierr))


      !
      ! get the required matrix elements
      !
      R_loop: &
      DO ir_par = 1,nrtot_par

          !
          ! set the indexes
          !
          j = 0
          DO i=1,3
              !
              IF ( i == transport_dir ) THEN
                 ivr_aux(i) = 0
              ELSE
                 j = j + 1
                 ivr_aux(i) = NINT( vr_par( j, ir_par) )
              ENDIF
              !
          ENDDO
      
          !
          ! search the 3D index corresponding to ivr_aux
          !
          lfound = .FALSE.
          DO ir = 1, nrtot_corr
              !
              IF ( ALL( ivr_corr(:,ir) == ivr_aux(:) ) )  THEN
                  lfound = .TRUE.
                  ind = ir
                  EXIT
              ENDIF
              !
          ENDDO
          !
          IF ( .NOT. lfound ) CALL errore(subname, '3D R-vector not found', ir_par )


          !
          ! cut the operator (caux) 
          ! according to the required rows and cols
          !
          DO j=1,ncols_corr
          DO i=1,nrows_corr
              !
              caux_small(i, j, ir_par) = caux( irows_corr(i), icols_corr(j), ind )
              !
          ENDDO
          ENDDO
          !
          !
      ENDDO R_loop


      !
      ! Compute the 2D fourier transform
      !
      CALL fourier_par (opr, dimC, dimC, caux_small, dimC, dimC)

      !
      ! local cleaning
      !
      DEALLOCATE( caux, caux_small, STAT=ierr )
      IF ( ierr/=0 ) CALL errore(subname, 'deallocating caux, caux_small', ABS(ierr))

      CALL timing( subname, OPR='stop' )
      CALL log_pop( subname )
      !
   END SUBROUTINE correlation_sgmread
    

END MODULE T_correlation_module

