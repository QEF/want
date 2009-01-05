! 
! Copyright (C) 2005 WanT Group
! 
! This file is distributed under the terms of the 
! GNU General Public License. See the file `License' 
! in the root directory of the present distribution, 
! or http://www.gnu.org/copyleft/gpl.txt . 
! 
!********************************************
   MODULE T_input_module
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
! SUBROUTINE setup_smearing()
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
      USE T_input_parameters_module,  ONLY : read_namelist_input_conductor
      !
      IMPLICIT NONE

      !
      ! attach input from file if the case
      !
      CALL input_from_file ( stdin )

      !
      ! reading and checking namelists
      !
      CALL read_namelist_input_conductor(stdin)

      !
      ! scattering data in their own modules
      !
      CALL setup_control()
      CALL setup_io()
      CALL setup_egrid()
      CALL setup_smearing()
      CALL setup_hamiltonian()
      CALL setup_correlation()
      CALL setup_kpoints()


   END SUBROUTINE input_manager


!**********************************************************
   SUBROUTINE setup_control()
   !**********************************************************
      USE T_control_module,         ONLY : calculation_type,  &
                                           conduct_formula,   &
                                           niterx,            &
                                           nprint,            &
                                           bias,              &
                                           transport_dir,     &
                                           datafile_L,        &
                                           datafile_C,        &
                                           datafile_R,        &
                                           datafile_sgm,      &
                                           write_kdata,       &
                                           debug_level,       &
                                           use_debug_mode,    &
                                           nfailx
                                            
      USE T_input_parameters_module,ONLY : calculation_type_  => calculation_type, &
                                           conduct_formula_   => conduct_formula, &
                                           niterx_            => niterx, &
                                           nprint_            => nprint, &
                                           bias_              => bias, &
                                           transport_dir_     => transport_dir, &
                                           datafile_L_        => datafile_L, &
                                           datafile_C_        => datafile_C, &
                                           datafile_R_        => datafile_R, &
                                           datafile_sgm_      => datafile_sgm, &
                                           write_kdata_       => write_kdata, &
                                           debug_level_       => debug_level, &
                                           nfailx_            => nfailx

      IMPLICIT NONE

      calculation_type    = calculation_type_
      conduct_formula     = conduct_formula_
      niterx              = niterx_
      nprint              = nprint_
      bias                = bias_
      datafile_L          = datafile_L_
      datafile_C          = datafile_C_
      datafile_R          = datafile_R_
      datafile_sgm        = datafile_sgm_
      transport_dir       = transport_dir_
      write_kdata         = write_kdata_
      debug_level         = debug_level_
      nfailx              = nfailx_

      use_debug_mode = .FALSE.
      IF ( debug_level_ > 0 )  use_debug_mode  = .TRUE.

   END SUBROUTINE setup_control
      

!**********************************************************
   SUBROUTINE setup_io()
   !**********************************************************
      USE io_module,                ONLY : work_dir, &
                                           prefix,   &
                                           postfix
      USE T_control_module,         ONLY : debug_level, &
                                           use_debug_mode
      USE T_input_parameters_module,ONLY : work_dir_          => work_dir, &
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
      USE T_input_parameters_module,ONLY : ne_     => ne,   &
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

      USE T_input_parameters_module,ONLY : delta_         => delta,         &
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
      USE T_hamiltonian_module, ONLY :     dimL,    &
                                           dimR,    &
                                           dimC,    & 
                                           dimx,    & 
                                           ispin,   &
                                           shift_L, &
                                           shift_C, &
                                           shift_R, &
                                           shift_corr
      USE T_input_parameters_module,ONLY : dimL_     => dimL,    &
                                           dimR_     => dimR,    &
                                           dimC_     => dimC,    &
                                           ispin_    => ispin,   &
                                           shift_L_  => shift_L, &
                                           shift_C_  => shift_C, &
                                           shift_R_  => shift_R, &
                                        shift_corr_  => shift_corr
      IMPLICIT NONE
      ! 
      dimL            = dimL_
      dimR            = dimR_
      dimC            = dimC_
      dimx            = MAX( dimL, dimR, dimC)
      !
      ispin           = ispin_
      shift_L         = shift_L_
      shift_C         = shift_C_
      shift_R         = shift_R_
      shift_corr      = shift_corr_
      !
   END SUBROUTINE setup_hamiltonian


!**********************************************************
   SUBROUTINE setup_kpoints()
   !**********************************************************
      USE T_kpoints_module,         ONLY :     nk_par, s_par, use_symm
      USE T_input_parameters_module,ONLY :     nk, s, use_symm_ => use_symm
      IMPLICIT NONE
      
      nk_par(1:2)  = nk(1:2)
      s_par(1:2)   = s(1:2)
      use_symm     = use_symm_

   END SUBROUTINE setup_kpoints


!**********************************************************
   SUBROUTINE setup_correlation()
   !**********************************************************
      USE T_correlation_module,     ONLY :     lhave_corr
      USE T_input_parameters_module,ONLY :     datafile_sgm_ => datafile_sgm
      IMPLICIT NONE
    
      lhave_corr     = LEN_TRIM( datafile_sgm_ ) /= 0

   END SUBROUTINE setup_correlation

END MODULE T_input_module

