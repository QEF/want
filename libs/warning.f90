!
! Copyright (C) 2005 WanT Group
! This file is distributed under the terms of the
! GNU General Public License. See the file `License'
! in the root directory of the present distribution,
! or http://www.gnu.org/copyleft/gpl.txt .
!
! based on the error.f90 file from Quantum-Espresso
!
!*****************************************
   SUBROUTINE warning(msg)
   !*****************************************
   !
   !  this routine a warning message
   !  INPUT: msg       the messageto be printed
   !
   IMPLICIT NONE

   !
   ! input variables
   !
   CHARACTER(LEN=*), INTENT(in)  :: msg

   !
   ! local variables
   !
   INTEGER :: ip, nproc, mpime

!
!----------------------------------------
! main Body
!----------------------------------------
!
#if defined __MPI
      CALL mpi_comm_size(mpi_comm_world,nproc,ierr)
      CALL mpi_comm_rank(mpi_comm_world,mpime,ierr)
#else
      mpime = 0
      nproc = 1
#endif

!
! print the warning message
!
      DO ip = 0, nproc-1
          IF( mpime == 0 ) WRITE (6,"(2x,'WARNING: ',a)") TRIM(msg) 
      ENDDO

END SUBROUTINE warning
