!
! Copyright (C) 2009 WanT Group
!
! This file is distributed under the terms of the
! GNU General Public License. See the file `License\'
! in the root directory of the present distribution,
! or http://www.gnu.org/copyleft/gpl.txt .
!
!*********************************************
   MODULE E_workspace_module
   !*********************************************
   !
   USE kinds,                 ONLY : dbl
   USE constants,             ONLY : ZERO
   !
   USE T_kpoints_module,      ONLY : nkpts_par, nrtot_par, kpoints_alloc => alloc
   USE E_hamiltonian_module,  ONLY : dimT, dimE, dimB
   !
   IMPLICIT NONE
   PRIVATE 
   SAVE

   !
   ! Contains workspace used through transport calcs
   ! 
   COMPLEX(dbl), ALLOCATABLE :: sgm_B(:,:,:)
   COMPLEX(dbl), ALLOCATABLE :: rsgm_B(:,:,:)
   COMPLEX(dbl), ALLOCATABLE :: gT(:,:)
   COMPLEX(dbl), ALLOCATABLE :: gB(:,:)
   COMPLEX(dbl), ALLOCATABLE :: gE(:,:)
   !
   LOGICAL :: alloc = .FALSE.


!
! end delcarations
!

   PUBLIC :: dimT, dimE
   !
   PUBLIC :: gT, gB, gE
   PUBLIC :: sgm_B
   PUBLIC :: rsgm_B
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
      IF ( dimT <= 0 )   CALL errore(subname,'invalid dimT', 3 )
      IF ( dimE <= 0 )   CALL errore(subname,'invalid dimE', 3 )
      !
      ALLOCATE ( sgm_B(dimE,dimE,nkpts_par), STAT=ierr )
      IF( ierr /=0 ) CALL errore(subname,'allocating sgm_B', ABS(ierr) )
      ALLOCATE ( rsgm_B(dimE,dimE,nrtot_par), STAT=ierr )
      IF( ierr /=0 ) CALL errore(subname,'allocating rsgm_B', ABS(ierr) )
      !
      ALLOCATE ( gT(dimT,dimT), STAT=ierr )
      IF( ierr /=0 ) CALL errore(subname,'allocating gT', ABS(ierr) )
      ALLOCATE ( gB(dimB,dimB), STAT=ierr )
      IF( ierr /=0 ) CALL errore(subname,'allocating gB', ABS(ierr) )
      ALLOCATE ( gE(dimE,dimE), STAT=ierr )
      IF( ierr /=0 ) CALL errore(subname,'allocating gE', ABS(ierr) )
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
      DEALLOCATE ( sgm_B, rsgm_B, STAT=ierr )
      IF( ierr /=0 ) CALL errore(subname,'deallocating sgm_B, rsgm_B', ABS(ierr) )
      !
      DEALLOCATE ( gT, gB, gE, STAT=ierr )
      IF( ierr /=0 ) CALL errore(subname,'deallocating gT, gB, gE', ABS(ierr) )
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
       IF ( ALLOCATED(sgm_B) )    cost = cost + REAL(SIZE(sgm_B))      * 16.0_dbl
       IF ( ALLOCATED(rsgm_B) )   cost = cost + REAL(SIZE(rsgm_B))     * 16.0_dbl
       IF ( ALLOCATED(gT) )       cost = cost + REAL(SIZE(gT))         * 16.0_dbl
       IF ( ALLOCATED(gB) )       cost = cost + REAL(SIZE(gB))         * 16.0_dbl
       IF ( ALLOCATED(gE) )       cost = cost + REAL(SIZE(gE))         * 16.0_dbl
       !
       workspace_memusage = cost / 1000000.0_dbl
       !
   END FUNCTION workspace_memusage



END MODULE E_workspace_module

