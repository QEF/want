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
   ! contains explicit interfaces for some WanT internal routines 
   !
   USE kinds
   IMPLICIT NONE
   PRIVATE

!
! interfaces in this module
!
! *   summary
! *   want_dftread
! *   want_init
!

   PUBLIC :: summary 
   PUBLIC :: want_dftread
   PUBLIC :: want_init
             
!
! defined interfcaes
!

   INTERFACE summary
      !
      SUBROUTINE summary_x ( iunit, input, lattice, ions, windows, symmetry, &
                             kpoints, bshells, pseudo)
         INTEGER,           INTENT(in) :: iunit
         LOGICAL, OPTIONAL, INTENT(in) :: input   
         LOGICAL, OPTIONAL, INTENT(in) :: lattice 
         LOGICAL, OPTIONAL, INTENT(in) :: ions   
         LOGICAL, OPTIONAL, INTENT(in) :: windows
         LOGICAL, OPTIONAL, INTENT(in) :: symmetry
         LOGICAL, OPTIONAL, INTENT(in) :: kpoints   
         LOGICAL, OPTIONAL, INTENT(in) :: bshells   
         LOGICAL, OPTIONAL, INTENT(in) :: pseudo   
      END SUBROUTINE summary_x
      !
   END INTERFACE


   INTERFACE want_init
      !
      SUBROUTINE want_init_x (input, lattice, ions, windows, &
                              kpoints, bshells, pseudo)
         LOGICAL, OPTIONAL, INTENT(in) :: input
         LOGICAL, OPTIONAL, INTENT(in) :: lattice
         LOGICAL, OPTIONAL, INTENT(in) :: ions
         LOGICAL, OPTIONAL, INTENT(in) :: windows
         LOGICAL, OPTIONAL, INTENT(in) :: kpoints
         LOGICAL, OPTIONAL, INTENT(in) :: bshells
         LOGICAL, OPTIONAL, INTENT(in) :: pseudo
      END SUBROUTINE want_init_x
      !
   END INTERFACE


   INTERFACE want_dftread
      !
      SUBROUTINE want_dftread_x (lattice, ions, windows, symmetry, kpoints, pseudo, need_wfc)
         LOGICAL, OPTIONAL, INTENT(in) :: lattice
         LOGICAL, OPTIONAL, INTENT(in) :: ions
         LOGICAL, OPTIONAL, INTENT(in) :: windows
         LOGICAL, OPTIONAL, INTENT(in) :: symmetry
         LOGICAL, OPTIONAL, INTENT(in) :: kpoints
         LOGICAL, OPTIONAL, INTENT(in) :: pseudo
         LOGICAL, OPTIONAL, INTENT(in) :: need_wfc
      END SUBROUTINE want_dftread_x
      !
   END INTERFACE

END MODULE want_interfaces_module


