!
! Copyright (C) 2004-2007 Quantum-Espresso group
! This file is distributed under the terms of the
! GNU General Public License. See the file `License'
! in the root directory of the present distribution,
! or http://www.gnu.org/copyleft/gpl.txt .
!
!--------------------------------------------------------------------------
!
MODULE atom_module
  !
  ! ... The variables needed to describe the atoms and related quantities
  !
  USE radial_grids_module, ONLY : radial_grid_type, deallocate_radial_grid
  !
  SAVE
  !
  type(radial_grid_type), allocatable, target :: &
       rgrid(:)                ! the information on atomic radial grids.
                               ! NB: some of the subsequent data are therefore redundant 
                               ! and will be eliminated in due course asap
  INTEGER, ALLOCATABLE :: &
       msh(:)                  ! the point at rcut
  !
CONTAINS

  !-----------------------------------------------------------------------
  SUBROUTINE atom_deallocate()
    !-----------------------------------------------------------------------
    !   
    IMPLICIT NONE
    INTEGER :: i
    !   
    IF ( ALLOCATED( rgrid ) ) THEN
        !
        DO i = 1, SIZE( rgrid )
            CALL deallocate_radial_grid( rgrid(i) )
        ENDDO
        !
        DEALLOCATE( rgrid )
        !
    ENDIF
    !
    IF ( ALLOCATED( msh ) ) DEALLOCATE( msh)
    !
  END SUBROUTINE atom_deallocate

END MODULE atom_module

