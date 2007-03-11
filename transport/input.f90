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
   USE io_module,  ONLY : stdin, ionode
   USE log_module, ONLY : log_push, log_pop
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
      USE T_input_parameters_module,  ONLY : read_namelist_input_conductor
      IMPLICIT NONE
      !
      INTEGER :: ierr

      !
      ! attach input from file if the case
      !
      
      CALL log_push( 'input_manager' )

      IF ( ionode ) THEN
          CALL input_from_file ( stdin, ierr )
          IF ( ierr /= 0 )  CALL errore('input_manager','error in input from file',ABS(ierr))
      ENDIF

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
      CALL setup_kpoints()

      !
      ! reading further input data
      !

      CALL log_pop( 'input_manager' )

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
                                           datafile_sgm,      &
                                           write_kdata 
      USE T_input_parameters_module,ONLY : calculation_type_  => calculation_type, &
                                           conduct_formula_   => conduct_formula, &
                                           use_overlap_       => use_overlap, &
                                           niterx_            => niterx, &
                                           nprint_            => nprint, &
                                           bias_              => bias, &
                                           transport_dir_     => transport_dir, &
                                           datafile_L_        => datafile_L, &
                                           datafile_C_        => datafile_C, &
                                           datafile_R_        => datafile_R, &
                                           datafile_sgm_      => datafile_sgm, &
                                           write_kdata_       => write_kdata

      IMPLICIT NONE

      calculation_type    = calculation_type_
      conduct_formula     = conduct_formula_
      use_overlap         = use_overlap_
      niterx              = niterx_
      nprint              = nprint_
      bias                = bias_
      datafile_L          = datafile_L_
      datafile_C          = datafile_C_
      datafile_R          = datafile_R_
      datafile_sgm        = datafile_sgm_
      use_correlation     = LEN_TRIM( datafile_sgm_ ) /= 0
      transport_dir       = transport_dir_
      write_kdata         = write_kdata_

   END SUBROUTINE setup_control
      

!**********************************************************
   SUBROUTINE setup_io()
   !**********************************************************
      USE io_module,                ONLY : work_dir, &
                                           prefix, &
                                           postfix
      USE T_input_parameters_module,ONLY : work_dir_          => work_dir, &
                                           prefix_            => prefix, &
                                           postfix_           => postfix
      IMPLICIT NONE

      work_dir            = work_dir_
      prefix              = prefix_
      postfix             = postfix_

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


!**********************************************************
   SUBROUTINE setup_kpoints()
   !**********************************************************
      USE T_kpoints_module,         ONLY :     nk_par
      USE T_input_parameters_module,ONLY :     nk
      IMPLICIT NONE
      
      nk_par(1:2)  = nk(1:2)

   END SUBROUTINE setup_kpoints

END MODULE T_input_module

