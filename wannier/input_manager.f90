! 
! Copyright (C) 2005 WanT Group
! 
! This file is distributed under the terms of the 
! GNU General Public License. See the file `License' 
! in the root directory of the present distribution, 
! or http://www.gnu.org/copyleft/gpl.txt . 
! 
!**********************************************************
   SUBROUTINE input_manager()
   !**********************************************************
   !
   ! manage all the operations to deal with input from stdin
   !
   USE io_module,                ONLY : stdin
   USE input_module,             ONLY : setup_control, &
                                        setup_io, & 
                                        setup_windows, & 
                                        setup_subspace, & 
                                        setup_localization
   USE input_parameters_module,  ONLY : read_namelist_control, &
                                        read_namelist_subspace, &
                                        read_namelist_localization
   USE trial_center_data_module, ONLY : trial_center_data_allocate
   USE input_base_module,        ONLY : read_cards, card_wannier_centers
   !
   IMPLICIT NONE

   !
   ! attach input from file if the case
   !
   CALL input_from_file ( stdin )

   !
   ! reading and checking namelists
   !
   CALL read_namelist_control( stdin )
   CALL read_namelist_subspace( stdin )
   CALL read_namelist_localization( stdin )

   !
   ! scattering data in their own modules
   !
   CALL setup_control()
   CALL setup_io()
   CALL setup_windows()
   CALL setup_subspace()
   CALL setup_localization()

   !
   ! reading cards
   !
   CALL trial_center_data_allocate()
   !
   CALL read_cards( stdin )

END SUBROUTINE input_manager

