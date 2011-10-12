!
! Copyright (C) 2011 WanT Group
!
! This file is distributed under the terms of the
! GNU General Public License. See the file `License'
! in the root directory of the present distribution,
! or http://www.gnu.org/copyleft/gpl.txt .

!
!----------------------------------
   MODULE bond_module
!----------------------------------
   !
   ! contains basic dafinitions to define bonds
   !
   USE kinds,     ONLY : dbl
   USE constants, ONLY : EPS_m6
   IMPLICIT NONE
   !
   PRIVATE
   SAVE
   !
   !
   TYPE bond_type
      !
      INTEGER     :: atom1
      INTEGER     :: atom2
      INTEGER     :: cell1(3)
      INTEGER     :: cell2(3)
      INTEGER     :: itype
      REAL(dbl)   :: r2
      REAL(dbl)   :: midcoord(3)
      REAL(dbl)   :: pair_coord(6)
      !
      TYPE( bond_type ), POINTER :: next
      !
   END TYPE bond_type
   !
   ! 
   INTERFACE OPERATOR (==)
       MODULE PROCEDURE bond_type_compare
   END INTERFACE
   !
   PUBLIC :: bond_type
   PUBLIC :: OPERATOR(==)
   !
CONTAINS
   !
!------------------------------------------------------
   LOGICAL FUNCTION bond_type_compare( bond1, bond2)
   !------------------------------------------------------
   IMPLICIT NONE
      !
      TYPE( bond_type ), INTENT(IN) :: bond1, bond2
      LOGICAL :: ldirect, linverse
      !
      bond_type_compare = .FALSE.
      !  
      !
      ! check direct equality
      !
      ldirect  = .FALSE.
      !
      IF ( bond1%atom1  ==  bond2%atom1  .AND.   &
           bond1%atom2  ==  bond2%atom2  .AND.   &
           ALL ( bond1%cell1(:) - bond1%cell2(:) == bond2%cell1(:) - bond2%cell2(:) ) .AND. &
           ABS( bond1%r2 - bond2%r2 ) < EPS_m6 ) &
           ldirect  = .TRUE.
      !
      !
      ! check inverse equality
      !
      linverse = .FALSE.
      !
      IF ( bond1%atom1  ==  bond2%atom2  .AND.   &
           bond1%atom2  ==  bond2%atom1  .AND.   &
           ALL ( bond1%cell1(:) - bond1%cell2(:) == bond2%cell2(:) - bond2%cell1(:) ) .AND. &
           ABS( bond1%r2 - bond2%r2 ) < EPS_m6 ) &
           linverse  = .TRUE.
      !
      !
      bond_type_compare = ldirect .OR. linverse
      !
   END FUNCTION bond_type_compare
   !
END MODULE

 
