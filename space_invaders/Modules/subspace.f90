!
! Copyright (C) 2004 Andrea Ferretti
!
! This file is distributed under the terms of the
! GNU General Public License. See the file `License\'
! in the root directory of the present distribution,
! or http://www.gnu.org/copyleft/gpl.txt .
!

!*********************************************
   MODULE subspace_module
!*********************************************
   USE kinds, ONLY : dbl
   USE windows_module, ONLY : mxdbnd, nkpts
   USE iotk_module
   IMPLICIT NONE
   PRIVATE
   SAVE

! This module handles data referring to the definition of 
! the subspace spanned by the Wannier functions which will
! be computed
!
! routines in this module:
! SUBROUTINE subspace_allocate()
! SUBROUTINE subspace_deallocate()
! SUBROUTINE subspace_write(unit)
! SUBROUTINE subspace_read(unit)

!
! declarations of common variables
!   

   !
   ! ... the hamiltonian in the final subspace
   REAL(dbl),    POINTER       :: s_eig(:,:)         ! the eigenvalues in the new subspace
   COMPLEX(dbl), POINTER       :: ham(:,:,:)         ! the hamiltonian in the subspace
                                                     ! mxdbnd, mxdbnd, nkpts
   !
   ! ... rotations defining the chosen subspace
   COMPLEX(dbl), POINTER       :: lamp(:,:,:)        ! mxdbnd, mxdbnd, nkpts
   COMPLEX(dbl), POINTER       :: camp(:,:,:)        ! equal
   COMPLEX(dbl), POINTER       :: eamp(:,:,:)        ! equal
   !
   ! NOTE: the second dimension should be DIMWANN instead of MXDBND but, 
   !       for coherence with the old notation about frozen states 
   !       (the total number of states is DIMWANN + DIMFROZ_max) matrixes are
   !       overallocated.
   !
   COMPLEX(dbl), POINTER       :: mtrx_in(:,:,:)   ! equal
   COMPLEX(dbl), POINTER       :: mtrx_out(:,:,:)  ! equal
   

!
! end of declarations
!

   PUBLIC :: ham, s_eig
   PUBLIC :: lamp, camp, eamp 
   PUBLIC :: mtrx_in, mtrx_out

CONTAINS

!
! subroutines
!

!**********************************************************
   SUBROUTINE subspace_allocate()
   !**********************************************************
   IMPLICIT NONE
       CHARACTER(17)      :: subname="subspace_allocate"
       INTEGER            :: ierr 
      
       IF ( mxdbnd <= 0 .OR. nkpts <= 0 ) &
           CALL errore(subname,' Invalid MXDBND or NKPTS ',1)

       ALLOCATE( s_eig(mxdbnd,nkpts), STAT=ierr )
           IF ( ierr/=0 ) CALL errore(subname,' S_EIG already allocated ',mxdbnd*nkpts)
       ALLOCATE( ham(mxdbnd,mxdbnd,nkpts), STAT = ierr )
           IF( ierr /=0 ) &
           CALL errore(' disentangle ', ' allocating ham ', mxdbnd*mxdbnd*nkpts )

END MODULE subspace_module
