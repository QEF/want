!
! Copyright (C) 2005 WanT Group
!
! This file is distributed under the terms of the
! GNU General Public License. See the file `License\'
! in the root directory of the present distribution,
! or http://www.gnu.org/copyleft/gpl.txt .
!
!*********************************************
   MODULE E_control_module
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
   
   CHARACTER(nstrx)          :: datafile_tot
   CHARACTER(nstrx)          :: datafile_emb
   CHARACTER(nstrx)          :: datafile_sgm
   CHARACTER(nstrx)          :: datafile_sgm_emb
   !
   LOGICAL                   :: write_embed_sgm
   LOGICAL                   :: do_orthoovp
   !
   INTEGER                   :: transport_dir
   INTEGER                   :: debug_level
   LOGICAL                   :: use_debug_mode
   !
   INTEGER                   :: nprint

!
! end delcarations
!

   PUBLIC :: datafile_tot
   PUBLIC :: datafile_emb
   PUBLIC :: datafile_sgm
   PUBLIC :: datafile_sgm_emb
   !
   PUBLIC :: write_embed_sgm
   !
   PUBLIC :: transport_dir
   PUBLIC :: debug_level
   PUBLIC :: use_debug_mode
   PUBLIC :: do_orthoovp
   !
   PUBLIC :: nprint

END MODULE E_control_module

