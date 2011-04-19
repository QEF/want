!
! Copyright (C) 2005 WanT Group
!
! This file is distributed under the terms of the
! GNU General Public License. See the file `License\'
! in the root directory of the present distribution,
! or http://www.gnu.org/copyleft/gpl.txt .
!
!*********************************************
   MODULE T_workspace_module
   !*********************************************
   !
   USE kinds,                ONLY : dbl
   USE constants,            ONLY : ZERO
   USE T_hamiltonian_module, ONLY : dimL, dimC, dimR
   USE T_kpoints_module,     ONLY : nkpts_par, nrtot_par, kpoints_alloc => alloc
   USE T_control_module,     ONLY : write_lead_sgm, write_gf
   !
   IMPLICIT NONE
   PRIVATE 
   SAVE

   !
   ! Contains workspace used through transport calcs
   ! 
   COMPLEX(dbl), ALLOCATABLE :: totL(:,:)
   COMPLEX(dbl), ALLOCATABLE :: totR(:,:)
   COMPLEX(dbl), ALLOCATABLE :: tottL(:,:)
   COMPLEX(dbl), ALLOCATABLE :: tottR(:,:)
   COMPLEX(dbl), ALLOCATABLE :: tot(:,:)
   COMPLEX(dbl), ALLOCATABLE :: tott(:,:)
   COMPLEX(dbl), ALLOCATABLE :: gamma_R(:,:)
   COMPLEX(dbl), ALLOCATABLE :: gamma_L(:,:)
   COMPLEX(dbl), ALLOCATABLE :: sgm_L(:,:,:)
   COMPLEX(dbl), ALLOCATABLE :: sgm_R(:,:,:)
   COMPLEX(dbl), ALLOCATABLE :: rsgm_L(:,:,:)
   COMPLEX(dbl), ALLOCATABLE :: rsgm_R(:,:,:)
   COMPLEX(dbl), ALLOCATABLE :: gL(:,:)
   COMPLEX(dbl), ALLOCATABLE :: gR(:,:)
   COMPLEX(dbl), ALLOCATABLE :: gC(:,:)
   !
   COMPLEX(dbl), ALLOCATABLE :: rgC(:,:,:)
   COMPLEX(dbl), ALLOCATABLE :: kgC(:,:,:)
   !
   LOGICAL :: alloc = .FALSE.


!
! end delcarations
!

   PUBLIC :: dimL, dimR, dimC     
   !
   !
   PUBLIC :: totL, tottL
   PUBLIC :: totR, tottR
   !
   PUBLIC :: gR, gL, gC
   PUBLIC :: gamma_R, gamma_L
   PUBLIC :: sgm_L,  sgm_R
   PUBLIC :: rsgm_L, rsgm_R
   PUBLIC :: rgC, kgC
   !
   PUBLIC :: workspace_allocate
   PUBLIC :: workspace_deallocate
   PUBLIC :: workspace_memusage
   PUBLIC :: alloc

CONTAINS

!
! subroutines
!

!**********************************************************
   SUBROUTINE workspace_allocate()
   !**********************************************************
   IMPLICIT NONE
      CHARACTER(18)      :: subname="workspace_allocate"
      INTEGER  :: ierr

      IF ( alloc )       CALL errore(subname,'already allocated', 1 )
      IF ( .NOT. kpoints_alloc ) &
                         CALL errore(subname,'kpoints modukle not alloc', 2 )
      IF ( dimL <= 0 )   CALL errore(subname,'invalid dimL', 3 )
      IF ( dimR <= 0 )   CALL errore(subname,'invalid dimR', 3 )
      IF ( dimC <= 0 )   CALL errore(subname,'invalid dimC', 3 )
      !
      ALLOCATE ( totL(dimL,dimL), STAT=ierr )
      IF( ierr /=0 ) CALL errore(subname,'allocating totL', ABS(ierr) )
      ALLOCATE ( totR(dimR,dimR), STAT=ierr )
      IF( ierr /=0 ) CALL errore(subname,'allocating totR', ABS(ierr) )
      ALLOCATE ( tottL(dimL,dimL), STAT=ierr )
      IF( ierr /=0 ) CALL errore(subname,'allocating tottL', ABS(ierr) )
      ALLOCATE ( tottR(dimR,dimR), STAT=ierr )
      IF( ierr /=0 ) CALL errore(subname,'allocating tottR', ABS(ierr) )
      ! 
      ALLOCATE ( sgm_L(dimC,dimC,nkpts_par), STAT=ierr )
      IF( ierr /=0 ) CALL errore(subname,'allocating sgm_L', ABS(ierr) )
      ALLOCATE ( sgm_R(dimC,dimC,nkpts_par), STAT=ierr )
      IF( ierr /=0 ) CALL errore(subname,'allocating sgm_R', ABS(ierr) )
      !
      IF ( write_lead_sgm ) THEN
          !
          ALLOCATE ( rsgm_L(dimC,dimC,nrtot_par), STAT=ierr )
          IF( ierr /=0 ) CALL errore(subname,'allocating rsgm_L', ABS(ierr) )
          ALLOCATE ( rsgm_R(dimC,dimC,nrtot_par), STAT=ierr )
          IF( ierr /=0 ) CALL errore(subname,'allocating rsgm_R', ABS(ierr) )
          !
      ENDIF
      !
      IF ( write_gf ) THEN
          !
          ALLOCATE ( rgC(dimC,dimC,nrtot_par), STAT=ierr )
          IF( ierr /=0 ) CALL errore(subname,'allocating rgC', ABS(ierr) )
          ALLOCATE ( kgC(dimC,dimC,nkpts_par), STAT=ierr )
          IF( ierr /=0 ) CALL errore(subname,'allocating kgC', ABS(ierr) )
          !
      ENDIF
      !
      ALLOCATE ( gamma_R(dimC,dimC), STAT=ierr )
      IF( ierr /=0 ) CALL errore(subname,'allocating gamma_R', ABS(ierr) )
      ALLOCATE ( gamma_L(dimC,dimC), STAT=ierr )
      IF( ierr /=0 ) CALL errore(subname,'allocating gamma_L', ABS(ierr) )
      !
      ALLOCATE ( gL(dimL,dimL), STAT=ierr )
      IF( ierr /=0 ) CALL errore(subname,'allocating gL', ABS(ierr) )
      ALLOCATE ( gR(dimR,dimR), STAT=ierr )
      IF( ierr /=0 ) CALL errore(subname,'allocating gR', ABS(ierr) )
      ALLOCATE ( gC(dimC,dimC), STAT=ierr )
      IF( ierr /=0 ) CALL errore(subname,'allocating gC', ABS(ierr) )
      !
      alloc = .TRUE.
      !
   END SUBROUTINE workspace_allocate


!**********************************************************
   SUBROUTINE workspace_deallocate()
   !**********************************************************
   IMPLICIT NONE
      CHARACTER(20)      :: subname="workspace_deallocate"
      INTEGER :: ierr

      IF ( .NOT. alloc ) RETURN
      !
      DEALLOCATE ( totL, tottL, STAT=ierr )
      IF( ierr /=0 ) CALL errore(subname,'deallocating totL, tottL', ABS(ierr) )
      DEALLOCATE ( totR, tottR, STAT=ierr )
      IF( ierr /=0 ) CALL errore(subname,'deallocating totR, tottR', ABS(ierr) )
   
      DEALLOCATE ( sgm_L, sgm_R, STAT=ierr )
      IF( ierr /=0 ) CALL errore(subname,'deallocating sgm_L, sgm_R ', ABS(ierr) )
      !
      IF ( write_lead_sgm ) THEN
          !
          DEALLOCATE ( rsgm_L, rsgm_R, STAT=ierr )
          IF( ierr /=0 ) CALL errore(subname,'deallocating rsgm_L, rsgm_R ', ABS(ierr) )
          !
      ENDIF
      !
      IF ( write_gf ) THEN
          !
          DEALLOCATE ( rgC, STAT=ierr )
          IF( ierr /=0 ) CALL errore(subname,'deallocating rgC', ABS(ierr) )
          DEALLOCATE ( kgC, STAT=ierr )
          IF( ierr /=0 ) CALL errore(subname,'deallocating kgC', ABS(ierr) )
          !
      ENDIF
      !
      DEALLOCATE ( gamma_R, gamma_L, STAT=ierr )
      IF( ierr /=0 ) CALL errore(subname,'deallocating gamma_R, gamma_L ', ABS(ierr) )
      DEALLOCATE ( gR, gL, STAT=ierr )
      IF( ierr /=0 ) CALL errore(subname,'deallocating gR, gL ', ABS(ierr) )
      DEALLOCATE ( gC, STAT=ierr )
      IF( ierr /=0 ) CALL errore(subname,'deallocating gC ', ABS(ierr) )
      !
      alloc = .FALSE.   
      !
   END SUBROUTINE workspace_deallocate


!**********************************************************
   REAL(dbl) FUNCTION workspace_memusage()
   !**********************************************************
   IMPLICIT NONE
       !
       REAL(dbl) :: cost
       !
       cost = ZERO
       IF ( ALLOCATED(totL) )     cost = cost + REAL(SIZE(totL))       * 16.0_dbl
       IF ( ALLOCATED(totR) )     cost = cost + REAL(SIZE(totR))       * 16.0_dbl
       IF ( ALLOCATED(tottL) )    cost = cost + REAL(SIZE(tottL))      * 16.0_dbl
       IF ( ALLOCATED(tottR) )    cost = cost + REAL(SIZE(tottR))      * 16.0_dbl
       IF ( ALLOCATED(gamma_R) )  cost = cost + REAL(SIZE(gamma_R))    * 16.0_dbl
       IF ( ALLOCATED(gamma_L) )  cost = cost + REAL(SIZE(gamma_L))    * 16.0_dbl
       !
       IF ( ALLOCATED(sgm_L) )    cost = cost + REAL(SIZE(sgm_L))      * 16.0_dbl
       IF ( ALLOCATED(sgm_R) )    cost = cost + REAL(SIZE(sgm_R))      * 16.0_dbl
       IF ( ALLOCATED(rsgm_L) )   cost = cost + REAL(SIZE(rsgm_L))     * 16.0_dbl
       IF ( ALLOCATED(rsgm_R) )   cost = cost + REAL(SIZE(rsgm_R))     * 16.0_dbl
       !
       IF ( ALLOCATED(gL) )       cost = cost + REAL(SIZE(gL))         * 16.0_dbl
       IF ( ALLOCATED(gR) )       cost = cost + REAL(SIZE(gR))         * 16.0_dbl
       IF ( ALLOCATED(gC) )       cost = cost + REAL(SIZE(gC))         * 16.0_dbl
       IF ( ALLOCATED(rgC) )      cost = cost + REAL(SIZE(rgC))        * 16.0_dbl
       IF ( ALLOCATED(kgC) )      cost = cost + REAL(SIZE(kgC))        * 16.0_dbl
       !
       workspace_memusage = cost / 1000000.0_dbl
       !
   END FUNCTION workspace_memusage


END MODULE T_workspace_module

