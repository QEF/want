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
   USE kinds,                ONLY : dbl
   USE T_hamiltonian_module, ONLY : dimL, dimC, dimR
   IMPLICIT NONE
   PRIVATE 
   SAVE
!
! Contains workspace used through transport calcs
! 
   ! 
   !
   COMPLEX(dbl), ALLOCATABLE :: aux00_L(:,:), aux01_L(:,:)
   COMPLEX(dbl), ALLOCATABLE :: aux00_R(:,:), aux01_R(:,:)
   COMPLEX(dbl), ALLOCATABLE :: aux00_C(:,:)
   COMPLEX(dbl), ALLOCATABLE :: aux_LC(:,:), aux_CL(:,:)
   COMPLEX(dbl), ALLOCATABLE :: aux_CR(:,:), aux_RC(:,:)
   !
   COMPLEX(dbl), ALLOCATABLE :: totL(:,:)
   COMPLEX(dbl), ALLOCATABLE :: totR(:,:)
   COMPLEX(dbl), ALLOCATABLE :: tottL(:,:)
   COMPLEX(dbl), ALLOCATABLE :: tottR(:,:)
   COMPLEX(dbl), ALLOCATABLE :: gamma_R(:,:)
   COMPLEX(dbl), ALLOCATABLE :: gamma_L(:,:)
   COMPLEX(dbl), ALLOCATABLE :: sgm_L(:,:)
   COMPLEX(dbl), ALLOCATABLE :: sgm_R(:,:)
   COMPLEX(dbl), ALLOCATABLE :: gL(:,:)
   COMPLEX(dbl), ALLOCATABLE :: gR(:,:)
   COMPLEX(dbl), ALLOCATABLE :: gC(:,:)
   !
   LOGICAL :: alloc = .FALSE.


!
! end delcarations
!

   PUBLIC :: dimL, dimR, dimC     
   !
   PUBLIC :: aux00_L, aux01_L
   PUBLIC :: aux00_R, aux01_R
   PUBLIC :: aux00_C
   PUBLIC :: aux_LC, aux_CL
   PUBLIC :: aux_CR, aux_RC
   !
   PUBLIC :: totL, tottL
   PUBLIC :: totR, tottR
   !
   PUBLIC :: gR, gL, gC
   PUBLIC :: gamma_R, gamma_L
   PUBLIC :: sgm_L, sgm_R
   !
   PUBLIC :: workspace_allocate
   PUBLIC :: workspace_deallocate
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
      IF ( dimL <= 0 )   CALL errore(subname,'invalid dimL', 1 )
      IF ( dimR <= 0 )   CALL errore(subname,'invalid dimR', 1 )
      IF ( dimC <= 0 )   CALL errore(subname,'invalid dimC', 1 )

      ALLOCATE ( aux00_L(dimL,dimL), STAT=ierr )
         IF( ierr /=0 ) CALL errore(subname,'allocating aux00_L ', ABS(ierr) )
      ALLOCATE ( aux01_L(dimL,dimL), STAT=ierr )
         IF( ierr /=0 ) CALL errore(subname,'allocating aux01_L', ABS(ierr) )
      ALLOCATE ( aux00_R(dimR,dimR), STAT=ierr )
         IF( ierr /=0 ) CALL errore(subname,'allocating aux00_R', ABS(ierr) )
      ALLOCATE ( aux01_R(dimR,dimR), STAT=ierr )
         IF( ierr /=0 ) CALL errore(subname,'allocating aux01_R', ABS(ierr) )
      ALLOCATE ( aux00_C(dimC,dimC), STAT=ierr )
         IF( ierr /=0 ) CALL errore(subname,'allocating aux00_C', ABS(ierr) )
      ALLOCATE ( aux_LC(dimL,dimC), STAT=ierr )
         IF( ierr /=0 ) CALL errore(subname,'allocating aux_LC', ABS(ierr) )
      ALLOCATE ( aux_CR(dimC,dimR), STAT=ierr )
         IF( ierr /=0 ) CALL errore(subname,'allocating aux_CR', ABS(ierr) )
      ALLOCATE ( aux_CL(dimC,dimL), STAT=ierr )
         IF( ierr /=0 ) CALL errore(subname,'allocating aux_CL', ABS(ierr) )
      ALLOCATE ( aux_RC(dimR,dimC), STAT=ierr )
         IF( ierr /=0 ) CALL errore(subname,'allocating aux_RC', ABS(ierr) )
   
      ALLOCATE ( totL(dimL,dimL), STAT=ierr )
         IF( ierr /=0 ) CALL errore(subname,'allocating totL', ABS(ierr) )
      ALLOCATE ( totR(dimR,dimR), STAT=ierr )
         IF( ierr /=0 ) CALL errore(subname,'allocating totR', ABS(ierr) )
      ALLOCATE ( tottL(dimL,dimL), STAT=ierr )
         IF( ierr /=0 ) CALL errore(subname,'allocating tottL', ABS(ierr) )
      ALLOCATE ( tottR(dimR,dimR), STAT=ierr )
         IF( ierr /=0 ) CALL errore(subname,'allocating tottR', ABS(ierr) )
   
      ALLOCATE ( sgm_L(dimC,dimC), STAT=ierr )
         IF( ierr /=0 ) CALL errore(subname,'allocating sgm_L', ABS(ierr) )
      ALLOCATE ( sgm_R(dimC,dimC), STAT=ierr )
         IF( ierr /=0 ) CALL errore(subname,'allocating sgm_R', ABS(ierr) )
      ALLOCATE ( gamma_R(dimC,dimC), STAT=ierr )
         IF( ierr /=0 ) CALL errore(subname,'allocating gamma_R', ABS(ierr) )
      ALLOCATE ( gamma_L(dimC,dimC), STAT=ierr )
         IF( ierr /=0 ) CALL errore(subname,'allocating gamma_L', ABS(ierr) )

      ALLOCATE ( gL(dimL,dimL), STAT=ierr )
         IF( ierr /=0 ) CALL errore(subname,'allocating gL', ABS(ierr) )
      ALLOCATE ( gR(dimR,dimR), STAT=ierr )
         IF( ierr /=0 ) CALL errore(subname,'allocating gR', ABS(ierr) )
      ALLOCATE ( gC(dimC,dimC), STAT=ierr )
         IF( ierr /=0 ) CALL errore(subname,'allocating gC', ABS(ierr) )

      alloc = .TRUE.
   END SUBROUTINE workspace_allocate


!**********************************************************
   SUBROUTINE workspace_deallocate()
   !**********************************************************
   IMPLICIT NONE
      CHARACTER(20)      :: subname="workspace_deallocate"
      INTEGER :: ierr

      IF ( .NOT. alloc ) RETURN

      DEALLOCATE ( aux00_L, aux01_L, STAT=ierr )
         IF( ierr /=0 ) CALL errore(subname,'deallocating aux00_L, aux01_L', ABS(ierr) )
      DEALLOCATE ( aux00_R, aux01_R, STAT=ierr )
         IF( ierr /=0 ) CALL errore(subname,'deallocating aux00_R, aux01_R', ABS(ierr) )
      DEALLOCATE ( aux00_C, STAT=ierr )
         IF( ierr /=0 ) CALL errore(subname,'deallocating aux00_C', ABS(ierr) )
      DEALLOCATE ( aux_LC, aux_CL, STAT=ierr )
         IF( ierr /=0 ) CALL errore(subname,'deallocating aux_LC, aux_CL', ABS(ierr) )
      DEALLOCATE ( aux_CR, aux_RC, STAT=ierr)
         IF( ierr /=0 ) CALL errore(subname,'deallocating aux_CR, aux_RC', ABS(ierr) )
   
      DEALLOCATE ( totL, tottL, STAT=ierr )
         IF( ierr /=0 ) CALL errore(subname,'deallocating totL, tottL', ABS(ierr) )
      DEALLOCATE ( totR, tottR, STAT=ierr )
         IF( ierr /=0 ) CALL errore(subname,'deallocating totR, tottR', ABS(ierr) )
   
      DEALLOCATE ( sgm_L, sgm_R, STAT=ierr )
         IF( ierr /=0 ) CALL errore(subname,'deallocating sgm_L, sgm_R ', ABS(ierr) )
      DEALLOCATE ( gamma_R, gamma_L, STAT=ierr )
         IF( ierr /=0 ) CALL errore(subname,'deallocating gamma_R, gamma_L ', ABS(ierr) )
      DEALLOCATE ( gR, gL, STAT=ierr )
         IF( ierr /=0 ) CALL errore(subname,'deallocating gR, gL ', ABS(ierr) )
      DEALLOCATE ( gC, STAT=ierr )
         IF( ierr /=0 ) CALL errore(subname,'deallocating gC ', ABS(ierr) )

      alloc = .FALSE.   
   END SUBROUTINE workspace_deallocate

END MODULE T_workspace_module

