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
   !
   USE kinds,      ONLY : dbl
   USE parameters, ONLY : nstrx
   !
   IMPLICIT NONE
   PRIVATE 
   SAVE
!
! Contains GLOBAL CONTROL variables for transport calculations
! 
   
   CHARACTER(nstrx)          :: calculation_type
   CHARACTER(nstrx)          :: conduct_formula
   !
   CHARACTER(nstrx)          :: datafile_L, datafile_C, datafile_R
   CHARACTER(nstrx)          :: datafile_sgm
   !
   INTEGER                   :: transport_dir
   INTEGER                   :: debug_level
   !
   LOGICAL                   :: do_eigenchannels = .FALSE.
   !
   LOGICAL                   :: use_overlap = .FALSE.
   LOGICAL                   :: use_debug_mode
   LOGICAL                   :: write_kdata
   !
   INTEGER                   :: niterx
   INTEGER                   :: nfailx
   INTEGER                   :: nfail = 0
   !
   INTEGER                   :: nprint
   !
   REAL(dbl)                 :: bias

!
! end delcarations
!

   PUBLIC :: calculation_type
   PUBLIC :: conduct_formula
   PUBLIC :: datafile_L, datafile_C, datafile_R
   PUBLIC :: datafile_sgm
   !
   PUBLIC :: transport_dir
   PUBLIC :: debug_level
   !
   PUBLIC :: do_eigenchannels
   PUBLIC :: use_overlap
   PUBLIC :: use_debug_mode
   PUBLIC :: write_kdata
   !
   PUBLIC :: niterx
   PUBLIC :: nfailx
   PUBLIC :: nfail
   !
   PUBLIC :: nprint
   !
   PUBLIC :: bias

END MODULE T_control_module

