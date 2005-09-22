!
! Copyright (C) 2005 WanT Group
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
   CHARACTER(nstrx)          :: restart_mode
   CHARACTER(nstrx)          :: subspace_init
   CHARACTER(nstrx)          :: localization_init
   CHARACTER(nstrx)          :: ordering_mode

   INTEGER                   :: nprint_dis
   INTEGER                   :: nsave_dis
   INTEGER                   :: nprint_wan
   INTEGER                   :: nsave_wan
   REAL(dbl)                 :: unitary_thr

   LOGICAL                   :: use_pseudo 
   LOGICAL                   :: use_uspp
   LOGICAL                   :: use_atomwfc 
   LOGICAL                   :: use_blimit 

   LOGICAL                   :: do_overlaps
   LOGICAL                   :: do_projections
   LOGICAL                   :: do_condmin

   LOGICAL                   :: read_pseudo 
   LOGICAL                   :: read_overlaps
   LOGICAL                   :: read_projections
   LOGICAL                   :: read_subspace
   LOGICAL                   :: read_unitary
 
!
! end delcarations
!

   PUBLIC :: verbosity
   PUBLIC :: restart_mode
   PUBLIC :: ordering_mode
   PUBLIC :: subspace_init
   PUBLIC :: localization_init
   PUBLIC :: nprint_dis
   PUBLIC :: nsave_dis
   PUBLIC :: nprint_wan
   PUBLIC :: nsave_wan
   PUBLIC :: unitary_thr

   PUBLIC :: use_pseudo
   PUBLIC :: use_atomwfc
   PUBLIC :: use_uspp
   PUBLIC :: use_blimit

   PUBLIC :: do_overlaps
   PUBLIC :: do_projections
   PUBLIC :: do_condmin

   PUBLIC :: read_pseudo
   PUBLIC :: read_overlaps
   PUBLIC :: read_projections
   PUBLIC :: read_subspace
   PUBLIC :: read_unitary


END MODULE control_module

