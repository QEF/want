!
! Copyright (C) 2005 WanT Group
!
! This file is distributed under the terms of the
! GNU General Public License. See the file `License\'
! in the root directory of the present distribution,
! or http://www.gnu.org/copyleft/gpl.txt .
!
!*********************************************
   MODULE T_control_module
!*********************************************
   USE kinds,      ONLY : dbl
   USE parameters, ONLY : nstrx
   IMPLICIT NONE
   PRIVATE 
   SAVE
!
! Contains GLOBAL CONTROL variables for transport calculations
! 
   
   CHARACTER(nstrx)          :: calculation_type
   CHARACTER(nstrx)          :: conduct_formula
   !
   LOGICAL                   :: use_overlap
   LOGICAL                   :: use_correlation
   !
   INTEGER                   :: niterx
   !
   REAL(dbl)                 :: bias

!
! end delcarations
!

   PUBLIC :: calculation_type
   PUBLIC :: conduct_formula
   !
   PUBLIC :: use_overlap
   PUBLIC :: use_correlation
   !
   PUBLIC :: niterx
   !
   PUBLIC :: bias

END MODULE T_control_module

