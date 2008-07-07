!
! Copyright (C) 2004 WanT Group
!
! This file is distributed under the terms of the
! GNU General Public License. See the file `License\'
! in the root directory of the present distribution,
! or http://www.gnu.org/copyleft/gpl.txt .
!
!*********************************************
   MODULE want_interfaces_module
!*********************************************
   !
   ! contains explicit and inplicit interfaces to some WanT internal routines 
   !
   USE kinds
   USE summary_module,         ONLY : summary
   USE want_dftread_module,    ONLY : want_dftread
   USE want_init_module,       ONLY : want_init
   USE postproc_init_module,   ONLY : postproc_init
   !
   IMPLICIT NONE
   PRIVATE

!
! interfaces in this module
!
! *   summary
! *   want_dftread
! *   want_init
! *   postproc_init
!

   PUBLIC :: summary 
   PUBLIC :: want_dftread
   PUBLIC :: want_init
   PUBLIC :: postproc_init
             
!
! locally-defined interfaces
!

!
! left here as a template
!
!   INTERFACE postproc_init
!      !
!      SUBROUTINE postproc_init_x ( windows, subspace, hamiltonian, wannier )
!         LOGICAL, OPTIONAL, INTENT(in) :: windows
!         LOGICAL, OPTIONAL, INTENT(in) :: subspace
!         LOGICAL, OPTIONAL, INTENT(in) :: hamiltonian
!         LOGICAL, OPTIONAL, INTENT(in) :: wannier
!      END SUBROUTINE postproc_init_x
!      !
!   END INTERFACE

END MODULE want_interfaces_module


