!
! Copyright (C) 2005 Andrea Ferretti
!
! This file is distributed under the terms of the
! GNU General Public License. See the file `License\'
! in the root directory of the present distribution,
! or http://www.gnu.org/copyleft/gpl.txt .
!
!*********************************************
   MODULE control_module
!*********************************************
   USE kinds,      ONLY : dbl
   USE parameters, ONLY : nstrx
   IMPLICIT NONE
   PRIVATE 
   SAVE
!
! Contains GLOBAL CONTROL variables
! Many other variables governing the flow of the WanT code
! will be probably added.
! 
   
   CHARACTER(nstrx)          :: verbosity
   CHARACTER(nstrx)          :: trial_mode
   CHARACTER(nstrx)          :: ordering_mode

   INTEGER                   :: nprint
   INTEGER                   :: iphase
   REAL(dbl)                 :: unitary_thr

   LOGICAL                   :: do_pseudo

 
!
! end delcarations
!

   PUBLIC :: verbosity
   PUBLIC :: trial_mode
   PUBLIC :: ordering_mode
   PUBLIC :: nprint
   PUBLIC :: iphase
   PUBLIC :: unitary_thr

   PUBLIC :: do_pseudo


END MODULE control_module

