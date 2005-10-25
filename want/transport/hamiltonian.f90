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
   IMPLICIT NONE
   PRIVATE 
   SAVE
!
! Contains transport hamiltonian data
! 
    INTEGER :: dimA       ! WF number in lead A
    INTEGER :: dimB       ! WF number in lead B
    INTEGER :: dimC       ! WF number in the central conductor region
    !
    COMPLEX(dbl), ALLOCATABLE :: h00_a(:,:), h01_a(:,:)
    COMPLEX(dbl), ALLOCATABLE :: h00_b(:,:), h01_b(:,:)
    COMPLEX(dbl), ALLOCATABLE :: h00_c(:,:)
    COMPLEX(dbl), ALLOCATABLE :: hci_ac(:,:)
    COMPLEX(dbl), ALLOCATABLE :: hci_cb(:,:)
    !
    COMPLEX(dbl), ALLOCATABLE :: s00_a(:,:), s01_a(:,:)
    COMPLEX(dbl), ALLOCATABLE :: s00_b(:,:), s01_b(:,:)
    COMPLEX(dbl), ALLOCATABLE :: s00_c(:,:)
    COMPLEX(dbl), ALLOCATABLE :: sci_ac(:,:)
    COMPLEX(dbl), ALLOCATABLE :: sci_cb(:,:)
    !
    CHARACTER(nstrx) :: sgmfile 
    !
    LOGICAL :: alloc = .FALSE.


!
! end delcarations
!

   PUBLIC :: dimA     
   PUBLIC :: dimB      
   PUBLIC :: dimC       
   !
   PUBLIC :: h00_a, h01_a
   PUBLIC :: h00_b, h01_b
   PUBLIC :: h00_c
   PUBLIC :: hci_ac
   PUBLIC :: hci_cb
   !
   PUBLIC :: s00_a, s01_a
   PUBLIC :: s00_b, s01_b
   PUBLIC :: s00_c
   PUBLIC :: sci_ac
   PUBLIC :: sci_cb
   !
   PUBLIC :: sgmfile
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
      IF ( dimA <= 0 )   CALL errore(subname,'invalid dimA', 1 )
      IF ( dimB <= 0 )   CALL errore(subname,'invalid dimB', 1 )
      IF ( dimC <= 0 )   CALL errore(subname,'invalid dimC', 1 )

      !
      ! h allocation       
      !
      ALLOCATE ( h00_a(dimA,dimA), STAT=ierr )
           IF( ierr /=0 ) CALL errore(subname, ' allocating h00_a ', 1 )
      ALLOCATE ( h01_a(dimA,dimA), STAT=ierr )
           IF( ierr /=0 ) CALL errore(subname, ' allocating h01_a ', 1 )
      ALLOCATE ( h00_b(dimB,dimB), STAT=ierr )
           IF( ierr /=0 ) CALL errore(subname, ' allocating h00_b ', 1 )
      ALLOCATE ( h01_b(dimB,dimB), STAT=ierr )
           IF( ierr /=0 ) CALL errore(subname, ' allocating h01_b ', 1 )
      ALLOCATE ( h00_c(dimC,dimC), STAT=ierr )
           IF( ierr /=0 ) CALL errore(subname, ' allocating h00_c ', 1 )
      ALLOCATE ( hci_ac(dimA,dimC), STAT=ierr )
           IF( ierr /=0 ) CALL errore(subname, ' allocating hci_ac ', 1 )
      ALLOCATE ( hci_cb(dimC,dimB), STAT=ierr )
           IF( ierr /=0 ) CALL errore(subname, ' allocating hac_cb ', 1 )

      !
      ! s allocation
      !
      ALLOCATE ( s00_a(dimA,dimA), STAT=ierr )
           IF( ierr /=0 ) CALL errore(subname, ' allocating s00_a ', 1 )
      ALLOCATE ( s01_a(dimA,dimA), STAT=ierr )
           IF( ierr /=0 ) CALL errore(subname, ' allocating s01_a ', 1 )
      ALLOCATE ( s00_b(dimB,dimB), STAT=ierr )
           IF( ierr /=0 ) CALL errore(subname, ' allocating s00_b ', 1 )
      ALLOCATE ( s01_b(dimB,dimB), STAT=ierr )
           IF( ierr /=0 ) CALL errore(subname, ' allocating s01_b ', 1 )
      ALLOCATE ( s00_c(dimC,dimC), STAT=ierr )
           IF( ierr /=0 ) CALL errore(subname, ' allocating s00_c ', 1 )
      ALLOCATE ( sci_ac(dimA,dimC), STAT=ierr )
           IF( ierr /=0 ) CALL errore(subname, ' allocating sci_ac ', 1 )
      ALLOCATE ( sci_cb(dimC,dimB), STAT=ierr )
           IF( ierr /=0 ) CALL errore(subname, ' allocating sci_cb ', 1 )

       alloc = .TRUE.
   END SUBROUTINE hamiltonian_allocate


!**********************************************************
   SUBROUTINE hamiltonian_deallocate()
   !**********************************************************
   IMPLICIT NONE
      CHARACTER(22)      :: subname="hamiltonian_deallocate"
      INTEGER :: ierr

      IF ( .NOT. alloc ) RETURN

      DEALLOCATE ( h00_a, h01_a, STAT=ierr )
           IF( ierr /=0 ) CALL errore(subname, ' deallocating h00_a, h01_a ', 1 )
      DEALLOCATE ( h00_b, h01_b, STAT=ierr )
           IF( ierr /=0 ) CALL errore(subname, ' deallocating h00_b, h01_b ', 1 )
      DEALLOCATE ( h00_c, STAT=ierr )
           IF( ierr /=0 ) CALL errore(subname, ' deallocating h00_c ', 1 )
      DEALLOCATE ( hci_ac, STAT=ierr )
           IF( ierr /=0 ) CALL errore(subname, ' deallocating hci_ac', 1 )
      DEALLOCATE ( hci_cb, STAT=ierr )
           IF( ierr /=0 ) CALL errore(subname, ' deallocating hac_cb', 1 )

      DEALLOCATE ( s00_a, s01_a, STAT=ierr )
           IF( ierr /=0 ) CALL errore(subname, ' deallocating s00_a, s01_a ', 1 )
      DEALLOCATE ( s00_b, s01_b, STAT=ierr )
           IF( ierr /=0 ) CALL errore(subname, ' deallocating s00_b, s01_b ', 1 )
      DEALLOCATE ( s00_c, STAT=ierr )
           IF( ierr /=0 ) CALL errore(subname, ' deallocating s00_c ', 1 )
      DEALLOCATE ( sci_ac, STAT=ierr )
           IF( ierr /=0 ) CALL errore(subname, ' deallocating sci_ac', 1 )
      DEALLOCATE ( sci_cb, STAT=ierr )
           IF( ierr /=0 ) CALL errore(subname, ' deallocating sci_cb', 1 )
          
      alloc = .FALSE.   

   END SUBROUTINE hamiltonian_deallocate


END MODULE T_hamiltonian_module

