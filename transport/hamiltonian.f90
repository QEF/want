!
! Copyright (C) 2005 WanT Group
!
! This file is distributed under the terms of the
! GNU General Public License. See the file `License\'
! in the root directory of the present distribution,
! or http://www.gnu.org/copyleft/gpl.txt .
!
!*********************************************
   MODULE T_hamiltonian_module
!*********************************************
   USE kinds,           ONLY : dbl
   USE parameters,      ONLY : nstrx
   USE T_kpoints_module,ONLY : nkpts_par
   IMPLICIT NONE
   PRIVATE 
   SAVE
!
! Contains transport hamiltonian data
! 
    ! 
    ! WF numbers in lead L,R and conductor C 
    INTEGER                   :: dimL  
    INTEGER                   :: dimC       
    INTEGER                   :: dimR       
    INTEGER                   :: dimx      ! MAX ( dimL, dimC, dimR )       
    !
    COMPLEX(dbl), ALLOCATABLE :: h00_L(:,:,:), h01_L(:,:,:)
    COMPLEX(dbl), ALLOCATABLE :: h00_R(:,:,:), h01_R(:,:,:)
    COMPLEX(dbl), ALLOCATABLE :: h00_C(:,:,:)
    COMPLEX(dbl), ALLOCATABLE :: h_LC(:,:,:)
    COMPLEX(dbl), ALLOCATABLE :: h_CR(:,:,:)
    !
    COMPLEX(dbl), ALLOCATABLE :: s00_L(:,:,:), s01_L(:,:,:)
    COMPLEX(dbl), ALLOCATABLE :: s00_R(:,:,:), s01_R(:,:,:)
    COMPLEX(dbl), ALLOCATABLE :: s00_C(:,:,:)
    COMPLEX(dbl), ALLOCATABLE :: s_LC(:,:,:)
    COMPLEX(dbl), ALLOCATABLE :: s_CR(:,:,:)
    !
    LOGICAL :: alloc = .FALSE.


!
! end delcarations
!

   PUBLIC :: dimL, dimC, dimR, dimx     
   PUBLIC :: nkpts_par
   !
   PUBLIC :: h00_L, h01_L
   PUBLIC :: h00_R, h01_R
   PUBLIC :: h00_C
   PUBLIC :: h_LC
   PUBLIC :: h_CR
   !
   PUBLIC :: s00_L, s01_L
   PUBLIC :: s00_R, s01_R
   PUBLIC :: s00_C
   PUBLIC :: s_LC
   PUBLIC :: s_CR
   !
   PUBLIC :: alloc
   !
   PUBLIC :: hamiltonian_allocate
   PUBLIC :: hamiltonian_deallocate


CONTAINS

!
! subroutines
!

!**********************************************************
   SUBROUTINE hamiltonian_allocate()
   !**********************************************************
   IMPLICIT NONE
      CHARACTER(20)      :: subname="hamiltonian_allocate"
      INTEGER  :: ierr

      IF ( alloc )       CALL errore(subname,'already allocated', 1 )
      IF ( dimL <= 0 )   CALL errore(subname,'invalid dimL', 1 )
      IF ( dimR <= 0 )   CALL errore(subname,'invalid dimR', 1 )
      IF ( dimC <= 0 )   CALL errore(subname,'invalid dimC', 1 )
      IF ( nkpts_par <= 0 )   CALL errore(subname,'invalid nkpts_par', 1 )

      !
      ! h allocation       
      !
      ALLOCATE ( h00_L(dimL,dimL,nkpts_par), STAT=ierr )
           IF( ierr /=0 ) CALL errore(subname, ' allocating h00_L', 1 )
      ALLOCATE ( h01_L(dimL,dimL,nkpts_par), STAT=ierr )
           IF( ierr /=0 ) CALL errore(subname, ' allocating h01_L', 1 )
      ALLOCATE ( h00_R(dimR,dimR,nkpts_par), STAT=ierr )
           IF( ierr /=0 ) CALL errore(subname, ' allocating h00_R', 1 )
      ALLOCATE ( h01_R(dimR,dimR,nkpts_par), STAT=ierr )
           IF( ierr /=0 ) CALL errore(subname, ' allocating h01_R', 1 )
      ALLOCATE ( h00_C(dimC,dimC,nkpts_par), STAT=ierr )
           IF( ierr /=0 ) CALL errore(subname, ' allocating h00_C', 1 )
      ALLOCATE ( h_LC(dimL,dimC,nkpts_par), STAT=ierr )
           IF( ierr /=0 ) CALL errore(subname, ' allocating h_LC', 1 )
      ALLOCATE ( h_CR(dimC,dimR,nkpts_par), STAT=ierr )
           IF( ierr /=0 ) CALL errore(subname, ' allocating h_CR', 1 )

      !
      ! s allocation
      !
      ALLOCATE ( s00_L(dimL,dimL,nkpts_par), STAT=ierr )
           IF( ierr /=0 ) CALL errore(subname, ' allocating s00_L ', 1 )
      ALLOCATE ( s01_L(dimL,dimL,nkpts_par), STAT=ierr )
           IF( ierr /=0 ) CALL errore(subname, ' allocating s01_L ', 1 )
      ALLOCATE ( s00_R(dimR,dimR,nkpts_par), STAT=ierr )
           IF( ierr /=0 ) CALL errore(subname, ' allocating s00_R ', 1 )
      ALLOCATE ( s01_R(dimR,dimR,nkpts_par), STAT=ierr )
           IF( ierr /=0 ) CALL errore(subname, ' allocating s01_R ', 1 )
      ALLOCATE ( s00_C(dimC,dimC,nkpts_par), STAT=ierr )
           IF( ierr /=0 ) CALL errore(subname, ' allocating s00_C ', 1 )
      ALLOCATE ( s_LC(dimL,dimC,nkpts_par), STAT=ierr )
           IF( ierr /=0 ) CALL errore(subname, ' allocating s_LC ', 1 )
      ALLOCATE ( s_CR(dimC,dimR,nkpts_par), STAT=ierr )
           IF( ierr /=0 ) CALL errore(subname, ' allocating s_CR ', 1 )

      alloc = .TRUE.
   END SUBROUTINE hamiltonian_allocate


!**********************************************************
   SUBROUTINE hamiltonian_deallocate()
   !**********************************************************
   IMPLICIT NONE
      CHARACTER(22)      :: subname="hamiltonian_deallocate"
      INTEGER :: ierr

      IF ( .NOT. alloc ) RETURN

      DEALLOCATE ( h00_L, h01_L, STAT=ierr )
           IF( ierr /=0 ) CALL errore(subname, 'deallocating h00_L, h01_L', 1 )
      DEALLOCATE ( h00_R, h01_R, STAT=ierr )
           IF( ierr /=0 ) CALL errore(subname, 'deallocating h00_R, h01_R', 1 )
      DEALLOCATE ( h00_C, STAT=ierr )
           IF( ierr /=0 ) CALL errore(subname, 'deallocating h00_C', 1 )
      DEALLOCATE ( h_LC, STAT=ierr )
           IF( ierr /=0 ) CALL errore(subname, 'deallocating h_LC', 1 )
      DEALLOCATE ( h_CR, STAT=ierr )
           IF( ierr /=0 ) CALL errore(subname, 'deallocating hac_cb', 1 )

      DEALLOCATE ( s00_L, s01_L, STAT=ierr )
           IF( ierr /=0 ) CALL errore(subname, 'deallocating s00_L, s01_L', 1 )
      DEALLOCATE ( s00_R, s01_R, STAT=ierr )
           IF( ierr /=0 ) CALL errore(subname, 'deallocating s00_R, s01_R', 1 )
      DEALLOCATE ( s00_C, STAT=ierr )
           IF( ierr /=0 ) CALL errore(subname, 'deallocating s00_C', 1 )
      DEALLOCATE ( s_LC, STAT=ierr )
           IF( ierr /=0 ) CALL errore(subname, 'deallocating s_LC', 1 )
      DEALLOCATE ( s_CR, STAT=ierr )
           IF( ierr /=0 ) CALL errore(subname, 'deallocating s_CR', 1 )
          
      alloc = .FALSE.   

   END SUBROUTINE hamiltonian_deallocate

END MODULE T_hamiltonian_module

