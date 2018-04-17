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
   USE kinds,           ONLY : dbl
   IMPLICIT NONE
   PRIVATE 
   SAVE

!
! Contains parallel kpt-and R-vectors data
! all vectors (vkpt_par, vr_par) in crystal units
! 
    INTEGER                   :: nkpts_par       ! number of paralle  kpts
    INTEGER                   :: nk_par(2)       ! 2D kpt mesh generator
    REAL(dbl),    ALLOCATABLE :: vkpt_par(:,:)   ! 2D kpt-vectors
    REAL(dbl),    ALLOCATABLE :: wk_par(:)       ! weights of the 2D kpts
    !
    INTEGER                   :: nrtot_par       ! number of 2D R-vectors
    INTEGER                   :: nr_par(2)       ! 2D R-vect mesh geenrator
    REAL(dbl),    ALLOCATABLE :: vr_par(:,:)     ! 2D kpt-vectors
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

   PUBLIC :: nrtot_par, nr_par, vr_par, wr_par
   PUBLIC :: nkpts_par, nk_par, vkpt_par, wk_par
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
   USE parameters,       ONLY : nstrx
   USE constants,        ONLY : ONE, TPI
   USE files_module,     ONLY : file_open, file_close
   USE io_module,        ONLY : aux_unit
   USE iotk_module
   USE T_control_module, ONLY : datafile_C, transport_dir 
   IMPLICIT NONE

      CHARACTER(12)      :: subname="kpoints_init"
      CHARACTER(nstrx)   :: attr
      INTEGER            :: nr_(3), nk_(3), nrtot_
      INTEGER            :: ir, ik, i, j, ierr
      REAL(dbl)          :: arg
 

      !
      ! read data from datafile_C (wannier ham file)
      !
      CALL file_open(aux_unit,TRIM(datafile_C),PATH="/",ACTION="read",FORM="formatted")
      !
      CALL iotk_scan_begin( aux_unit, "HAMILTONIAN", IERR=ierr )
      IF (ierr/=0) CALL errore(subname, 'searching for HAMILTONIAN', ABS(ierr) )
      !
      CALL iotk_scan_empty( aux_unit, 'DATA', ATTR=attr, IERR=ierr) 
      IF (ierr/=0) CALL errore(subname, 'searching for DATA', ABS(ierr) )
      CALL iotk_scan_attr(attr,'nk',nk_,IERR=ierr) 
      IF (ierr/=0) CALL errore(subname, 'searching for nk', ABS(ierr) )
      CALL iotk_scan_attr(attr,'nrtot',nrtot_,IERR=ierr) 
      IF (ierr/=0) CALL errore(subname, 'searching for nrtot', ABS(ierr) )
      CALL iotk_scan_attr(attr,'nr',nr_,IERR=ierr) 
      IF (ierr/=0) CALL errore(subname, 'searching for nr', ABS(ierr) )
      !
      CALL iotk_scan_end( aux_unit, "HAMILTONIAN", IERR=ierr )
      IF (ierr/=0) CALL errore(subname, 'searching for end HAMILTONIAN', ABS(ierr) )
      !
      CALL file_close(aux_unit,PATH="/",ACTION="read")
  
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
      !
      nk_par(1:2) = nr_par(1:2)
      nkpts_par = PRODUCT(nk_par)
  
      !
      ! allocations
      !
      ALLOCATE( vr_par(2,nrtot_par), wr_par(nrtot_par), STAT=ierr)
      IF( ierr /=0 ) CALL errore(subname, 'allocating vr_par, wr_par',ABS(ierr))
      ALLOCATE( vkpt_par(2,nkpts_par), wk_par(nkpts_par) , STAT=ierr)
      IF( ierr /=0 ) CALL errore(subname, 'allocating vkpt_par, wk_par',ABS(ierr))
      ALLOCATE( table_par(nrtot_par,nkpts_par) , STAT=ierr)
      IF( ierr /=0 ) CALL errore(subname, 'allocating table_par, wk_par',ABS(ierr))
      alloc = .TRUE.
  
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
      ENDDO
      ENDDO
  
      !
      ! then the kpt grid
      ik = 0
      DO j = 1, nk_par(2)
      DO i = 1, nk_par(1)
           !
           ik = ik + 1
           !
           vkpt_par(1,ik) = REAL( i - ( nk_par(1)+1)/2, dbl ) / REAL( nk_par(1), dbl )
           vkpt_par(2,ik) = REAL( j - ( nk_par(2)+1)/2, dbl ) / REAL( nk_par(2), dbl )
           !
           wk_par( ik ) = ONE/REAL(nkpts_par, dbl)
      ENDDO
      ENDDO

      !
      ! setting the 2D Fourier transform phases
      !
      DO ik = 1, nkpts_par
      DO ir = 1, nrtot_par
           arg = TPI * DOT_PRODUCT( vkpt_par(:,ik), vr_par(:,ir) )
           table_par(ir,ik) = CMPLX( COS(arg), SIN(arg), dbl )
      ENDDO
      ENDDO

   END SUBROUTINE kpoints_init


!**********************************************************
   SUBROUTINE kpoints_deallocate()
   !**********************************************************
   IMPLICIT NONE
      CHARACTER(18)      :: subname="kpoints_deallocate"
      INTEGER :: ierr

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
   END SUBROUTINE kpoints_deallocate

END MODULE T_kpoints_module

