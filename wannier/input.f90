! 
! Copyright (C) 2005 Andrea Ferretti
! 
! This file is distributed under the terms of the 
! GNU General Public License. See the file `License' 
! in the root directory of the present distribution, 
! or http://www.gnu.org/copyleft/gpl.txt . 
! 
!********************************************
   MODULE input_module
!********************************************
   USE kinds, ONLY : dbl
   USE io_module, ONLY : stdin, stdout
   IMPLICIT NONE
   PRIVATE
!
! This module handles the reading of input data
!
! routines in this module:
! SUBROUTINE input_manager()
! SUBROUTINE setup_control()
! SUBROUTINE setup_io()
! SUBROUTINE setup_windows()
! SUBROUTINE setup_kpoints()
! SUBROUTINE setup_subspace()
! SUBROUTINE setup_localization()
! 


   PUBLIC :: input_manager


CONTAINS

!
! subroutines
!


!**********************************************************
   SUBROUTINE input_manager()
   !**********************************************************
      USE input_parameters_module,  ONLY : read_namelist_control, &
                                           read_namelist_subspace, &
                                           read_namelist_localization
      USE trial_center_data_module, ONLY : trial, trial_center_data_allocate
      USE input_base_module,        ONLY : read_cards, &
                                           card_wannier_centers
      IMPLICIT NONE

      !
      ! reading and checking namelists
      !
      CALL read_namelist_control(stdin)
      CALL read_namelist_subspace(stdin)
      CALL read_namelist_localization(stdin)

      !
      ! scattering data in their own modules
      !
      CALL setup_control()
      CALL setup_io()
      CALL setup_windows()
      CALL setup_kpoints()
      CALL setup_subspace()
      CALL setup_localization()

      !
      ! reading cards
      !
      CALL trial_center_data_allocate()
      CALL read_cards(stdin)

   END SUBROUTINE input_manager


!**********************************************************
   SUBROUTINE setup_control()
   !**********************************************************
      USE control_module,           ONLY : verbosity,         &
                                           nprint,            &
                                           unitary_thr,       &
                                           do_pseudo,         &
                                           ordering_mode,     &
                                           iphase,            &
                                           trial_mode
      USE input_parameters_module,  ONLY : verbosity_       => verbosity, &
                                           nprint_          => nprint,    &
                                           unitary_thr_     => unitary_thr, &
                                           trial_mode_      => trial_mode, &
                                           ordering_mode_   => ordering_mode, &
                                           iphase_          => iphase, &
                                           assume_ncpp_     => assume_ncpp
      IMPLICIT NONE
      verbosity = verbosity_
      nprint = nprint_
      unitary_thr = unitary_thr_
      trial_mode = trial_mode_
      ordering_mode = ordering_mode_
      iphase = iphase_
      do_pseudo = .NOT. assume_ncpp_

   END SUBROUTINE setup_control
      

!**********************************************************
   SUBROUTINE setup_io()
   !**********************************************************
      USE io_module,                ONLY : prefix, postfix, work_dir, title
      USE input_parameters_module,  ONLY : prefix_    => prefix,       &
                                           postfix_   => postfix,      &
                                           work_dir_  => work_dir,     &
                                           title_     => title
      IMPLICIT NONE
      prefix   = prefix_
      postfix  = postfix_
      work_dir = work_dir_
      title    = title_
   END SUBROUTINE setup_io


!**********************************************************
   SUBROUTINE setup_windows()
   !**********************************************************
      USE windows_module,           ONLY : win_min,      &
                                           win_max,      &
                                           froz_min,     &
                                           froz_max,     &
                                           spin_component
      USE input_parameters_module,  ONLY : win_min_   => win_min,      &
                                           win_max_   => win_max,      &
                                           froz_min_  => froz_min,     &
                                           froz_max_  => froz_max,     &
                                           spin_component_ => spin_component
      IMPLICIT NONE
      win_min    = win_min_
      win_max    = win_max_
      froz_min   = froz_min_
      froz_max   = froz_max_
      spin_component = spin_component_
   END SUBROUTINE setup_windows


!**********************************************************
   SUBROUTINE setup_kpoints()
   !**********************************************************
      USE kpoints_module,           ONLY : nshells, nwhich
      USE input_parameters_module,  ONLY : nshells_   => nshells,      &
                                           nwhich_    => nwhich
      IMPLICIT NONE
      nshells  = nshells_
      nwhich   = nwhich_
   END SUBROUTINE setup_kpoints


!**********************************************************
   SUBROUTINE setup_subspace()
   !**********************************************************
      USE subspace_module,          ONLY : dimwann,      &
                                           alpha_dis,    &
                                           maxiter_dis,  &
                                           disentangle_thr
      USE input_parameters_module,  ONLY : dimwann_         => dimwann,    &
                                           alpha_dis_       => alpha_dis,  &
                                           maxiter_dis_     => maxiter_dis,  &
                                           disentangle_thr_ => disentangle_thr
      IMPLICIT NONE
      dimwann      = dimwann_
      alpha_dis    = alpha_dis_
      maxiter_dis  = maxiter_dis_
      disentangle_thr  = disentangle_thr_
   END SUBROUTINE setup_subspace

 
!**********************************************************
   SUBROUTINE setup_localization()
   !**********************************************************
      USE localization_module,      ONLY : wannier_thr,  &
                                           alpha0_wan,   &
                                           alpha1_wan,   &
                                           maxiter0_wan, &
                                           maxiter1_wan, &
                                           ncg
      USE input_parameters_module,  ONLY : wannier_thr_     => wannier_thr, &
                                           alpha0_wan_      => alpha0_wan,  &
                                           alpha1_wan_      => alpha1_wan,  &
                                           maxiter0_wan_    => maxiter0_wan,  &
                                           maxiter1_wan_    => maxiter1_wan,  &
                                           ncg_             => ncg,  &
                                           iphase_          => iphase
      IMPLICIT NONE
      wannier_thr    =  wannier_thr_
      alpha0_wan     =  alpha0_wan_
      alpha1_wan     =  alpha1_wan_
      maxiter0_wan   =  maxiter0_wan_
      maxiter1_wan   =  maxiter1_wan_
      ncg            =  ncg_
   END SUBROUTINE setup_localization


END MODULE input_module

