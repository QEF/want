! 
! Copyright (C) 2009 WanT Group
! 
! This file is distributed under the terms of the 
! GNU General Public License. See the file `License' 
! in the root directory of the present distribution, 
! or http://www.gnu.org/copyleft/gpl.txt . 
! 
!********************************************
   MODULE E_input_module
   !********************************************
   !
   USE kinds,      ONLY : dbl
   USE parameters, ONLY : nstrx
   USE io_module,  ONLY : stdin, log_unit, io_name
   USE log_module, ONLY : log_init, log_push
   USE constants,  ONLY : ZERO
   !
   IMPLICIT NONE
   PRIVATE
!
! This module handles the reading of input data
!
! routines in this module:
! SUBROUTINE input_manager()
! SUBROUTINE setup_control()
! SUBROUTINE setup_hamiltonian()
! SUBROUTINE setup_kpoints()
! SUBROUTINE setup_egrid()
! 


   PUBLIC :: input_manager


CONTAINS

!
! subroutines
!


!**********************************************************
   SUBROUTINE input_manager()
   !**********************************************************
      !
      USE E_input_parameters_module,  ONLY : read_namelist_input
      !
      IMPLICIT NONE

      !
      ! attach input from file if the case
      !
      CALL input_from_file ( stdin )

      !
      ! reading and checking namelists
      !
      CALL read_namelist_input(stdin)

      !
      ! scattering data in their own modules
      !
      CALL setup_control()
      CALL setup_io()
      CALL setup_egrid()
      CALL setup_hamiltonian()
      CALL setup_correlation()
      CALL setup_kpoints()

   END SUBROUTINE input_manager


!**********************************************************
   SUBROUTINE setup_control()
   !**********************************************************
      USE E_control_module,         ONLY : nprint,            &
                                           transport_dir,     &
                                           datafile_C,        &
                                           datafile_emb,      &
                                           datafile_sgm,      &
                                           datafile_sgm_emb,  &
                                           debug_level,       &
                                           use_debug_mode
                                            
      USE E_input_parameters_module,ONLY : nprint_            => nprint, &
                                           transport_dir_     => transport_dir, &
                                           datafile_C_        => datafile_C, &
                                           datafile_emb_      => datafile_emb, &
                                           datafile_sgm_      => datafile_sgm, &
                                           datafile_sgm_emb_  => datafile_sgm_emb, &
                                           debug_level_       => debug_level

      IMPLICIT NONE

      nprint              = nprint_
      datafile_C          = datafile_C_
      datafile_emb        = datafile_emb_
      datafile_sgm        = datafile_sgm_
      datafile_sgm_emb    = datafile_sgm_emb_
      transport_dir       = transport_dir_
      debug_level         = debug_level_

      use_debug_mode = .FALSE.
      IF ( debug_level_ > 0 )  use_debug_mode  = .TRUE.

   END SUBROUTINE setup_control
      

!**********************************************************
   SUBROUTINE setup_io()
   !**********************************************************
      USE io_module,                ONLY : work_dir, &
                                           prefix,   &
                                           postfix
      USE E_control_module,         ONLY : debug_level, &
                                           use_debug_mode
      USE E_input_parameters_module,ONLY : work_dir_          => work_dir, &
                                           prefix_            => prefix,   &
                                           postfix_           => postfix,  &
                                           debug_level_       => debug_level
      IMPLICIT NONE
      !
      CHARACTER( nstrx ) :: logfile

      work_dir            = work_dir_
      prefix              = prefix_
      postfix             = postfix_

      !
      ! this part is replicated for the sake of robustness
      !
      debug_level         = debug_level_
      use_debug_mode      = .FALSE.
      !
      IF ( debug_level_ > 0 )  use_debug_mode  = .TRUE.

      CALL io_name( "log", logfile, LBODY=.TRUE. )
      !
      CALL log_init( log_unit, use_debug_mode, logfile, debug_level)
      CALL log_push("main")

   END SUBROUTINE setup_io


!**********************************************************
   SUBROUTINE setup_egrid()
   !**********************************************************
      USE T_egrid_module,           ONLY : ne,           &
                                           emin, emax   
      USE E_input_parameters_module,ONLY : ne_     => ne,   &
                                           emin_   => emin, &
                                           emax_   => emax 
      IMPLICIT NONE

      ne     = ne_
      emin   = emin_
      emax   = emax_

   END SUBROUTINE setup_egrid


!**********************************************************
   SUBROUTINE setup_smearing()
   !**********************************************************
      USE T_smearing_module,        ONLY : delta,         &
                                           smearing_type, &
                                           delta_ratio, xmax

      USE E_input_parameters_module,ONLY : delta_         => delta,         &
                                           smearing_type_ => smearing_type, &
                                           delta_ratio_   => delta_ratio,   &
                                           xmax_          => xmax

      IMPLICIT NONE

      delta         = delta_
      smearing_type = smearing_type_
      delta_ratio   = delta_ratio_
      xmax          = xmax_

   END SUBROUTINE setup_smearing


!**********************************************************
   SUBROUTINE setup_hamiltonian()
   !**********************************************************
      USE E_hamiltonian_module, ONLY :     dimC,    & 
                                           dim_emb, &
                                           ispin,   &
                                           shift_C
      USE E_input_parameters_module,ONLY : dimC_     => dimC,    &
                                           dim_emb_  => dim_emb, &
                                           ispin_    => ispin,   &
                                           shift_C_  => shift_C
      IMPLICIT NONE
      ! 
      dimC            = dimC_
      dim_emb         = dim_emb_
      !
      ispin           = ispin_
      shift_C         = shift_C_
      !
   END SUBROUTINE setup_hamiltonian


!**********************************************************
   SUBROUTINE setup_kpoints()
   !**********************************************************
      USE T_kpoints_module,         ONLY :     nk_par, s_par, use_symm
      USE E_input_parameters_module,ONLY :     nk, s, use_symm_ => use_symm
      IMPLICIT NONE
      
      nk_par(1:2)  = nk(1:2)
      s_par(1:2)   = s(1:2)
      use_symm     = use_symm_

   END SUBROUTINE setup_kpoints


!**********************************************************
   SUBROUTINE setup_correlation()
   !**********************************************************
      USE E_correlation_module,     ONLY :     lhave_corr
      USE E_input_parameters_module,ONLY :     datafile_sgm_ => datafile_sgm
      IMPLICIT NONE
    
      lhave_corr     = LEN_TRIM( datafile_sgm_ ) /= 0

   END SUBROUTINE setup_correlation

END MODULE E_input_module

