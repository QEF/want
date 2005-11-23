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
   USE kinds, ONLY : dbl
   USE io_module, ONLY : stdin, stdout
   USE constants, ONLY : ZERO
   IMPLICIT NONE
   PRIVATE
!
! This module handles the reading of input data
!
! routines in this module:
! SUBROUTINE input_manager()
! SUBROUTINE setup_control()
! SUBROUTINE setup_hamiltonian()
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
      USE T_input_parameters_module,  ONLY : read_namelist_input_conductor
      IMPLICIT NONE

      !
      ! reading and checking namelists
      !
      CALL read_namelist_input_conductor(stdin)

      !
      ! scattering data in their own modules
      !
      CALL setup_control()
      CALL setup_egrid()
      CALL setup_hamiltonian()

      !
      ! reading further input data
      !

   END SUBROUTINE input_manager


!**********************************************************
   SUBROUTINE setup_control()
   !**********************************************************
      USE T_control_module,         ONLY : calculation_type,  &
                                           conduct_formula,   &
                                           use_overlap,       &
                                           use_correlation,   &
                                           niterx,            &
                                           nprint,            &
                                           bias,              &
                                           transport_dir,     &
                                           datafile_L,        &
                                           datafile_C,        &
                                           datafile_R,        &
                                           datafile_sgm
      USE T_input_parameters_module,ONLY : calculation_type_  => calculation_type, &
                                           conduct_formula_   => conduct_formula, &
                                           use_overlap_       => use_overlap, &
                                           use_correlation_   => use_correlation, &
                                           niterx_            => niterx, &
                                           nprint_            => nprint, &
                                           bias_              => bias, &
                                           transport_dir_     => transport_dir, &
                                           datafile_L_        => datafile_L, &
                                           datafile_C_        => datafile_C, &
                                           datafile_R_        => datafile_R, &
                                           datafile_sgm_      => datafile_sgm

      IMPLICIT NONE

      calculation_type    = calculation_type_
      conduct_formula     = conduct_formula_
      use_overlap         = use_overlap_
      use_correlation     = use_correlation_
      niterx              = niterx_
      nprint              = nprint_
      bias                = bias_
      datafile_sgm        = datafile_sgm_
      datafile_L          = datafile_L_
      datafile_C          = datafile_C_
      datafile_R          = datafile_R_
      transport_dir       = transport_dir_

   END SUBROUTINE setup_control
      

!**********************************************************
   SUBROUTINE setup_egrid()
   !**********************************************************
      USE T_egrid_module,           ONLY : ne,           &
                                           emin, emax,   &
                                           delta
      USE T_input_parameters_module,ONLY : ne_     => ne,   &
                                           emin_   => emin, &
                                           emax_   => emax, &
                                           delta_  => delta
      IMPLICIT NONE

      ne     = ne_
      emin   = emin_
      emax   = emax_
      delta  = delta_

   END SUBROUTINE setup_egrid


!**********************************************************
   SUBROUTINE setup_hamiltonian()
   !**********************************************************
      USE T_hamiltonian_module, ONLY :     dimL,    &
                                           dimR,    &
                                           dimC,    & 
                                           dimx
      USE T_input_parameters_module,ONLY : dimL_     => dimL, &
                                           dimR_     => dimR, &
                                           dimC_     => dimC
      IMPLICIT NONE
      
      dimL            = dimL_
      dimR            = dimR_
      dimC            = dimC_
      dimx            = MAX ( dimL, dimC, dimR )

   END SUBROUTINE setup_hamiltonian

END MODULE T_input_module

