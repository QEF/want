!
! Copyright (C) 2005 WanT Group
!
! This file is distributed under the terms of the
! GNU General Public License. See the file `License\'
! in the root directory of the present distribution,
! or http://www.gnu.org/copyleft/gpl.txt .
!
!*********************************************
   MODULE T_kpoints_module
   !*********************************************
   !
   USE kinds,           ONLY : dbl
   USE constants,       ONLY : EPS_m6, ZERO, ONE, TPI
   USE log_module,      ONLY : log_push, log_pop
   !
   IMPLICIT NONE
   PRIVATE 
   SAVE

!
! Contains parallel kpt-and R-vectors data
! all vectors (vkpt_par, vr_par) in crystal units
! 
    LOGICAL                   :: use_symm        ! whether to use kpt_symm
    INTEGER                   :: nkpts_par       ! number of parallel  kpts
    INTEGER                   :: nk_par(2)       ! 2D kpt mesh generator
    INTEGER                   :: s_par(2)        ! 2D shifts for kpt mesh generation
    REAL(dbl),    ALLOCATABLE :: vkpt_par(:,:)   ! 2D kpt-vectors
    REAL(dbl),    ALLOCATABLE :: wk_par(:)       ! weights of the 2D kpts
    !
    INTEGER                   :: nrtot_par       ! number of 2D R-vectors
    INTEGER                   :: nr_par(2)       ! 2D R-vect mesh geenrator
    REAL(dbl),    ALLOCATABLE :: vr_par(:,:)     ! 2D R-vectors
    REAL(dbl),    ALLOCATABLE :: wr_par(:)       ! weights of the 2D R-vects
    !
    COMPLEX(dbl), ALLOCATABLE :: table_par(:,:)  ! coefficients for the 2D FFT
                                                 ! R -> k  : table(ir,ik) = e^{i k*R}
    !
    !
    LOGICAL :: alloc = .FALSE.


!
! end delcarations
!

   PUBLIC :: use_symm
   PUBLIC :: nrtot_par, nr_par, vr_par, wr_par
   PUBLIC :: nkpts_par, nk_par, s_par, vkpt_par, wk_par
   PUBLIC :: table_par
   !
   PUBLIC :: alloc
   !
   PUBLIC :: kpoints_init
   PUBLIC :: kpoints_deallocate


CONTAINS

!
! subroutines
!


!**********************************************************
   SUBROUTINE kpoints_init()
   !**********************************************************
   !
   USE parameters,       ONLY : nstrx
   USE files_module,     ONLY : file_open, file_close
   USE io_module,        ONLY : aux_unit
   USE iotk_module
   USE T_control_module, ONLY : datafile_C, transport_dir 
   !
   IMPLICIT NONE

      CHARACTER(12)      :: subname="kpoints_init"
      CHARACTER(nstrx)   :: attr
      INTEGER            :: nr_(3), nrtot_, nkpts_par_x
      INTEGER            :: ir, ik, i, j, l, ierr
      LOGICAL            :: lfound, lequiv
      REAL(dbl)          :: arg, vaux(3)
 
      CALL log_push( 'kpoints_init' )

      !
      ! read data from datafile_C (ham file)
      !
      CALL file_open(aux_unit,TRIM(datafile_C),PATH="/",ACTION="read", IERR=ierr)
      IF (ierr/=0) CALL errore(subname, 'opening '//TRIM(datafile_C), ABS(ierr) )
      !
      CALL iotk_scan_begin( aux_unit, "HAMILTONIAN", IERR=ierr )
      IF (ierr/=0) CALL errore(subname, 'searching for HAMILTONIAN', ABS(ierr) )
      !
      CALL iotk_scan_empty( aux_unit, 'DATA', ATTR=attr, IERR=ierr) 
      IF (ierr/=0) CALL errore(subname, 'searching for DATA', ABS(ierr) )
!      CALL iotk_scan_attr(attr,'nk',nk_,IERR=ierr) 
!      IF (ierr/=0) CALL errore(subname, 'searching for nk', ABS(ierr) )
      CALL iotk_scan_attr(attr,'nrtot',nrtot_,IERR=ierr) 
      IF (ierr/=0) CALL errore(subname, 'searching for nrtot', ABS(ierr) )
      CALL iotk_scan_attr(attr,'nr',nr_,IERR=ierr) 
      IF (ierr/=0) CALL errore(subname, 'searching for nr', ABS(ierr) )
      !
      CALL iotk_scan_end( aux_unit, "HAMILTONIAN", IERR=ierr )
      IF (ierr/=0) CALL errore(subname, 'searching for end HAMILTONIAN', ABS(ierr) )
      !
      CALL file_close(aux_unit,PATH="/",ACTION="read", IERR=ierr)
      IF (ierr/=0) CALL errore(subname, 'closing '//TRIM(datafile_C), ABS(ierr) )
  
      !
      ! set the 2D Rmesh orthogonal to transport direction
      ! given by transport_dir
      !
      IF ( transport_dir < 1 .OR. transport_dir > 3 )  &
           CALL errore(subname, 'invalid transport_dir', ABS(transport_dir)+1 )
  
      i = 0
      DO j=1,3
         IF ( transport_dir /= j ) THEN  
             i = i+1
             nr_par(i) = nr_(j)
         ENDIF
      ENDDO
      !
      nrtot_par = PRODUCT(nr_par)

      !
      ! set the dimensions of the kpt_par mesh
      ! if nk_par /= 0 they are from input. otherwise use nr_par
      !
      IF ( nk_par(1) == 0 ) nk_par(1) = nr_par(1)
      IF ( nk_par(2) == 0 ) nk_par(2) = nr_par(2)
      !
      IF ( ANY( nk_par(:) < 1 ) ) CALL errore(subname,'nk mesh too small',71)
      !
      ! the total number of kpts will re-defined below
      ! due to symmetrization
      !
      nkpts_par_x = PRODUCT(nk_par)
  
      !
      ! allocations
      !
      ALLOCATE( vr_par(2,nrtot_par), wr_par(nrtot_par), STAT=ierr)
      IF( ierr /=0 ) CALL errore(subname, 'allocating vr_par, wr_par',ABS(ierr))
      !
      ALLOCATE( vkpt_par(2,nkpts_par_x), wk_par(nkpts_par_x) , STAT=ierr)
      IF( ierr /=0 ) CALL errore(subname, 'allocating vkpt_par, wk_par',ABS(ierr))
  
      !
      ! set the prallel grids (vr & kpt) in crystal units
      !
  
      !
      ! first the R grid
      ir = 0
      DO j = 1, nr_par(2)
      DO i = 1, nr_par(1)
          !
          ir = ir + 1
          !
          vr_par(1,ir) = REAL( i - ( nr_par(1)+1)/2, dbl )
          vr_par(2,ir) = REAL( j - ( nr_par(2)+1)/2, dbl )
          !
          wr_par( ir ) = ONE
          !
      ENDDO
      ENDDO
  
      !
      ! then the kpt grid
      !
      vkpt_par( :, :) = ZERO
      wk_par( : )     = ZERO
      !
      ik = 0
      !
      DO j = 1, nk_par(2)
      DO i = 1, nk_par(1)
          !
          !
          vaux(1) = REAL( i-1 -( nk_par(1) )/2, dbl ) / REAL( nk_par(1), dbl ) + &
                    REAL( s_par(1), dbl ) / REAL( 2* nk_par(1), dbl )
          vaux(2) = REAL( j-1 -( nk_par(2) )/2, dbl ) / REAL( nk_par(2), dbl ) + &
                    REAL( s_par(2), dbl ) / REAL( 2* nk_par(2), dbl )
          !
          ! check whether the new kpt is equivalent
          ! to one of the formers
          !
          lfound = .FALSE.
          DO l = 1, ik
              !
              IF ( .NOT. use_symm ) EXIT
              !
              lequiv = kpoints_equivalent( vaux(:), vkpt_par(:,l) )
              !
              IF ( lequiv ) THEN
                 !
                 lfound = .TRUE.
                 EXIT
                 !
              ENDIF
              !
          ENDDO
          !
          !
          IF ( lfound ) THEN
              !
              ! equivalent kpt found, increase weight
              wk_par( l ) = wk_par( l ) + ONE
              !
          ELSE
              !
              ik = ik + 1
              vkpt_par(:,ik) = vaux(:)
              wk_par( ik )   = ONE
              !
          ENDIF
          !
      ENDDO
      ENDDO
      !
      wk_par ( : ) = wk_par ( : ) / REAL(nkpts_par_x, dbl)
      nkpts_par    = ik 

      ALLOCATE( table_par(nrtot_par,nkpts_par) , STAT=ierr)
      IF( ierr /=0 ) CALL errore(subname, 'allocating table_par, wk_par',ABS(ierr))

      !
      ! setting the 2D Fourier transform phases
      !
      DO ik = 1, nkpts_par
      DO ir = 1, nrtot_par
          !
          arg = TPI * DOT_PRODUCT( vkpt_par(:,ik), vr_par(:,ir) )
          table_par(ir,ik) = CMPLX( COS(arg), SIN(arg), dbl )
          !
      ENDDO
      ENDDO

      alloc = .TRUE.
      CALL log_pop( 'kpoints_init' )
      !
   END SUBROUTINE kpoints_init


!**********************************************************
   SUBROUTINE kpoints_deallocate()
   !**********************************************************
   IMPLICIT NONE
      CHARACTER(18)      :: subname="kpoints_deallocate"
      INTEGER :: ierr
      CALL log_push( 'kpoints_deallocate' )

      IF ( ALLOCATED( vr_par)  ) THEN
          DEALLOCATE( vr_par, STAT=ierr)
          IF( ierr /=0 ) CALL errore(subname, 'deallocating vr_par',ABS(ierr))
      ENDIF
      IF ( ALLOCATED( wr_par)  ) THEN
          DEALLOCATE( wr_par, STAT=ierr)
          IF( ierr /=0 ) CALL errore(subname, 'deallocating wr_par',ABS(ierr))
      ENDIF
      IF ( ALLOCATED( vkpt_par)  ) THEN
          DEALLOCATE( vkpt_par, STAT=ierr)
          IF( ierr /=0 ) CALL errore(subname, 'deallocating vkpt_par',ABS(ierr))
      ENDIF
      IF ( ALLOCATED( wk_par)  ) THEN
          DEALLOCATE( wk_par, STAT=ierr)
          IF( ierr /=0 ) CALL errore(subname, 'deallocating wk_par',ABS(ierr))
      ENDIF
      IF ( ALLOCATED( table_par )  ) THEN
          DEALLOCATE( table_par, STAT=ierr)
          IF( ierr /=0 ) CALL errore(subname, 'deallocating table_par',ABS(ierr))
      ENDIF

      alloc = .FALSE.
      CALL log_pop( 'kpoints_deallocate' )
   END SUBROUTINE kpoints_deallocate

   
!**********************************************************
   LOGICAL FUNCTION kpoints_equivalent( v1, v2 )
   !**********************************************************
   !
   ! this routine check whether the input 2D kpts are equivalent
   ! by symmetry
   ! At the moment, only time-reversal symmetry is implemented ( k <==> -k)
   ! spatial symmetries maybe added later on if needed
   !
   IMPLICIT NONE
      REAL( dbl )   :: v1(2), v2(2)   
      INTEGER       :: i
      !       
      kpoints_equivalent = .TRUE.
      !
      DO i = 1, 2
         !
         ! Time-Rev symmetry
         IF ( ABS( MOD( v1(i)+v2(i), ONE ) ) > EPS_m6 ) kpoints_equivalent = .FALSE.
         !
      ENDDO
      !
      RETURN
      !
   END FUNCTION kpoints_equivalent


END MODULE T_kpoints_module

