! 
! Copyright (C) 2005 WanT Group
! 
! This file is distributed under the terms of the 
! GNU General Public License. See the file `License' 
! in the root directory of the present distribution, 
! or http://www.gnu.org/copyleft/gpl.txt . 
! 
!********************************************
   MODULE input_module
   !********************************************
   !
   USE kinds,     ONLY : dbl
   USE constants, ONLY : ZERO
   !
   IMPLICIT NONE
   PRIVATE
!
! This module handles the reading of input data
!
! routines in this module:
! SUBROUTINE setup_control()
! SUBROUTINE setup_io()
! SUBROUTINE setup_windows()
! SUBROUTINE setup_subspace()
! SUBROUTINE setup_localization()
! 


   PUBLIC :: setup_control
   PUBLIC :: setup_io
   PUBLIC :: setup_windows
   PUBLIC :: setup_subspace
   PUBLIC :: setup_localization


CONTAINS

!
! subroutines
!

!**********************************************************
   SUBROUTINE setup_control()
   !**********************************************************
      USE control_module,           ONLY : verbosity,         &
                                           unitary_thr,       &
                                           restart_mode,      &
                                           ordering_mode,     &
                                           do_condmin,        &
                                           do_overlaps,       &
                                           do_projections,    &
                                           do_collect_wf,     &
                                           do_ordering,       &
                                           use_pseudo,        &
                                           use_symmetry,      &
                                           use_timerev,       &
                                           use_debug_mode,    &
                                           debug_level,       &
                                           read_pseudo,       &
                                           read_overlaps,     &
                                           read_projections,  &
                                           read_subspace,     &
                                           read_unitary,      &
                                           read_symmetry,     &
                                           nprint_dis,        &
                                           nprint_wan,        &
                                           nsave_dis,         &
                                           nsave_wan,         &
                                           nwfc_buffer,       &
                                           nkb_buffer,        &
                                           use_blimit,        &
                                           subspace_init,     &
                                           localization_init 
      USE input_parameters_module,  ONLY : verbosity_       => verbosity, &
                                           restart_mode_    => restart_mode, &
                                           subspace_init_   => subspace_init, &
                                           localization_init_  => localization_init, &
                                           nprint_dis_      => nprint_dis, &
                                           nprint_wan_      => nprint_wan, &
                                           nsave_dis_       => nsave_dis, &
                                           nsave_wan_       => nsave_wan, &
                                           nwfc_buffer_     => nwfc_buffer, &
                                           nkb_buffer_      => nkb_buffer, &
                                           overlaps_        => overlaps,  &
                                           projections_     => projections, &
                                           unitary_thr_     => unitary_thr, &
                                           a_condmin_       => a_condmin, &
                                           ordering_mode_   => ordering_mode, &
                                           collect_wf_      => collect_wf, &
                                           use_blimit_      => use_blimit, &
                                           use_symmetry_    => use_symmetry, &
                                           use_timerev_     => use_symmetry, &
                                           debug_level_     => debug_level, &
                                           assume_ncpp_     => assume_ncpp

      IMPLICIT NONE

      verbosity        = verbosity_
      unitary_thr      = unitary_thr_
      ordering_mode    = ordering_mode_
      nprint_dis       = nprint_dis_
      nsave_dis        = nsave_dis_
      nprint_wan       = nprint_wan_
      nsave_wan        = nsave_wan_
      nwfc_buffer      = nwfc_buffer_
      nkb_buffer       = nkb_buffer_
      use_blimit       = use_blimit_
      use_pseudo       = .NOT. assume_ncpp_
      read_pseudo      = .NOT. assume_ncpp_
      use_symmetry     = use_symmetry_
      read_symmetry    = use_symmetry_
      use_timerev      = use_timerev_

      debug_level      = debug_level_
      use_debug_mode   = .FALSE.
      IF ( debug_level_ > 0 )  use_debug_mode   = .TRUE.

      do_condmin = .TRUE.
      IF ( a_condmin_ <= ZERO ) do_condmin = .FALSE.

      do_ordering   = .NOT. TRIM( ordering_mode ) == "none"

      do_collect_wf = collect_wf_ 

      subspace_init = subspace_init_
      localization_init = localization_init_

      restart_mode = restart_mode_
      SELECT CASE ( TRIM(restart_mode_) )
      CASE ( "from_scratch" )
      CASE ( "restart" )
           overlaps_ = "from_file"
           projections_ = "from_file"
           subspace_init = "from_file"
           localization_init = "from_file"
      CASE DEFAULT
           CALL errore('setup_control', &
                       'Invalid value for restart_mode = '//TRIM(restart_mode_),1)
      END SELECT

      IF ( TRIM( subspace_init_ ) == "from_file" ) read_subspace = .TRUE.
      IF ( TRIM( localization_init_ ) == "from_file" ) read_unitary = .TRUE.

      !
      SELECT CASE( TRIM(overlaps_) )
      CASE ( "from_scratch"  )
           read_overlaps = .FALSE.
           do_overlaps = .TRUE.
      CASE ( "from_file"  )
           read_overlaps = .TRUE.
           do_overlaps = .FALSE.
      CASE DEFAULT
           CALL errore('setup_control', &
                       'Invalid value for overlaps = '//TRIM(overlaps_),1)
      END SELECT
      !
      SELECT CASE( TRIM(projections_) )
      CASE ( "from_scratch"  )
           read_projections = .FALSE.
           do_projections = .TRUE.
      CASE ( "from_file"  )
           read_projections = .TRUE.
           do_projections = .FALSE.
      CASE DEFAULT
           CALL errore('setup_control', &
                       'Invalid value for projections = '//TRIM(projections_),2)
      END SELECT

   END SUBROUTINE setup_control
      

!**********************************************************
   SUBROUTINE setup_io()
   !**********************************************************
      USE io_module,                ONLY : prefix, postfix, work_dir, title, &
                                           dftdata_fmt, wantdata_fmt,        &
                                           io_init
      USE input_parameters_module,  ONLY : prefix_       => prefix,       &
                                           wantdata_fmt_ => wantdata_fmt, &
                                           postfix_      => postfix,      &
                                           work_dir_     => work_dir,     &
                                           title_        => title,        &
                                           dftdata_fmt_  => dftdata_fmt
      IMPLICIT NONE
      prefix   = prefix_
      postfix  = postfix_
      work_dir = work_dir_
      title    = title_
      dftdata_fmt  = TRIM(dftdata_fmt_)
      wantdata_fmt = TRIM(wantdata_fmt_)

      !
      ! init io
      !
      CALL io_init ( )

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
      !
   END SUBROUTINE setup_windows


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
      !
   END SUBROUTINE setup_subspace

 
!**********************************************************
   SUBROUTINE setup_localization()
   !**********************************************************
      USE localization_module,      ONLY : wannier_thr,  &
                                           alpha0_wan,   &
                                           alpha1_wan,   &
                                           maxiter0_wan, &
                                           maxiter1_wan, &
                                           a_condmin,    &
                                           dump_condmin, &
                                           niter_condmin,&
                                           xcell,        &
                                           ncg
      USE input_parameters_module,  ONLY : wannier_thr_     => wannier_thr, &
                                           alpha0_wan_      => alpha0_wan,  &
                                           alpha1_wan_      => alpha1_wan,  &
                                           maxiter0_wan_    => maxiter0_wan,  &
                                           maxiter1_wan_    => maxiter1_wan,  &
                                           niter_condmin_   => niter_condmin,  &
                                           a_condmin_       => a_condmin,  &
                                           dump_condmin_    => dump_condmin,  &
                                           xcell_           => xcell,  &
                                           ncg_             => ncg
      IMPLICIT NONE
      wannier_thr    =  wannier_thr_
      alpha0_wan     =  alpha0_wan_
      alpha1_wan     =  alpha1_wan_
      maxiter0_wan   =  maxiter0_wan_
      maxiter1_wan   =  maxiter1_wan_
      ncg            =  ncg_
      niter_condmin  =  niter_condmin_
      IF ( niter_condmin_ <= 0 ) niter_condmin = maxiter0_wan_ + maxiter1_wan_
      dump_condmin   =  dump_condmin_
      a_condmin      =  a_condmin_
      xcell(1:3)     =  xcell_(1:3)               
     
   END SUBROUTINE setup_localization


END MODULE input_module

