!
! Copyright (C) 2001-2003 PWSCF group
! This file is distributed under the terms of the
! GNU General Public License. See the file `License'
! in the root directory of the present distribution,
! or http://www.gnu.org/copyleft/gpl.txt .
!
!----------------------------------------------------------------------------
!
MODULE becmod
  USE kinds, ONLY :  dbl
  !
  SAVE
  ! variables containing <beta|psi>
  COMPLEX(KIND=dbl), ALLOCATABLE ::  &
       becp (:,:,:)            !  <beta|psi>, indeces: ibeta, ibnd, ikpt
  
END MODULE becmod


