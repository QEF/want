! 
! Copyright (C) 2005 WanT Group
! 
! This file is distributed under the terms of the 
! GNU General Public License. See the file `License' 
! in the root directory of the present distribution, 
! or http://www.gnu.org/copyleft/gpl.txt . 
! 
!********************************************
   MODULE input_parameters_module
!********************************************
   USE kinds, ONLY : dbl
   USE parameters, ONLY : nstrx, nnx
   USE parser_module, ONLY : change_case
   IMPLICIT NONE
   PRIVATE
   SAVE
!
! This module contains the definitions of the parameters in the
! input file and of thier default values (when any). 
! These data are then exported to the input module where the
! main routine control the IO and after that exported to the
! final modules where internal data are stored.
!
! Here are also the routine reading and checking each NAMELIST
!
! routines in this module:
! SUBROUTINE  read_namelist_control(unit)
! SUBROUTINE  read_namelist_subspace(unit)
! SUBROUTINE  read_namelist_localization(unit)
!

!
! ... declarations

!
!======================================== 
! CONTROL Namelist parameters
!======================================== 

   CHARACTER(nstrx) :: title = "Wannier Transport Calculation"
       ! the title of the calculation

   CHARACTER(nstrx) :: prefix = "WanT"
       ! specifies the prefix for the names of all the output and input (data) files
       ! INPUT  files:   "prefix".aaa
       ! OUTPUT files:   "prefix""postfix".bbb

   CHARACTER(nstrx) :: postfix = " "
       ! specifies the second part of the names of the output files
   
   CHARACTER(nstrx) :: work_dir = "./"
       ! the directory in which produced data files are written 

   CHARACTER(nstrx) :: verbosity = 'medium'
       ! ( 'low' | 'medium' | 'high' )
       ! the level of details of the code output

   CHARACTER(nstrx) :: verbosity_allowed(3)
   DATA verbosity_allowed / 'low',  'medium', 'high' /
       ! the allowed values for verbosity

   LOGICAL :: assume_ncpp = .FALSE.
       ! if .TRUE. this variable avoids the reading of pseudopotential files
       ! assuming the DFT calculation has been performed within norm-conserving
       ! pseudopotentials (for whom no knowledge of them is required in the WanT calc)

   REAL(dbl) :: unitary_thr = 1.0d-8  
       ! threshold for the check of matrix unitariery

   INTEGER :: nprint = 10  
       ! each nprint iterations in wannier minimizations write to stdout


   NAMELIST / CONTROL /  title, prefix, postfix, work_dir, verbosity, &
                         assume_ncpp, unitary_thr, nprint


   PUBLIC :: title, prefix, postfix, work_dir
   PUBLIC :: verbosity, assume_ncpp, unitary_thr, nprint
   PUBLIC :: CONTROL


!======================================== 
! SUBSPACE Namelist parameters
!======================================== 

   INTEGER :: dimwann = 0
       ! the number of wannier functions, i.e. the dimension of the wannier subspace

   REAL(dbl) :: win_min = 0.0_dbl
       ! the lower bound of the energy window for the determination of the 
       ! Wannier subspace
     
   REAL(dbl) :: win_max = 0.0_dbl
       ! the upper bound of the above defined energy window

   REAL(dbl) :: froz_min = -20000000.0_dbl
       ! the lower bound of a second energy window which "frozes" the
       ! inner states to be part of the subspace.

   REAL(dbl) :: froz_max = -10000000.0_dbl
       ! the upper bound of the frozen energy window

   REAL(dbl) :: alpha_dis = 0.5_dbl
       ! the mixing parameter for iterative minimization in diesentangle procedure

   INTEGER :: maxiter_dis = 1000
       ! maximum number of iterations during the disentangle procedure

   REAL(dbl) :: disentangle_thr = 1.0d-8  
       ! threshold for convergence of the iterative disentangle procedure

   CHARACTER(nstrx) :: trial_mode = "center_projections"
       ! ( 'lower_states' | 'upper_states' | 'center_projections' )
       ! Determine how the trial subspace is chosen
       ! 'lower_states' : the lower DIMWANN bands from DFT calculation are
       !                  used to define the subspace
       ! 'upper_states' : the upper DIMWANN bands from DFT calculation are
       !                  used to define the subspace
       ! 'center_projections' : a subspace is extracted from the DFT bands
       !                  by means of a projections on the given WANNIER_TRIAL_CENTERS
       !                  (see the section TRIAL_CENTERS)
   CHARACTER(nstrx) :: trial_mode_allowed(3)
   DATA trial_mode_allowed / 'lower_states', 'upper_states', 'center_projections' /

   CHARACTER(10)    :: spin_component = 'none'
       ! ( 'up' | 'down' | 'none' )
       ! define whether the calculation is spin polarized and if
       ! the case which spin component is to be treated 

   CHARACTER(nstrx) :: spin_component_allowed(3)
   DATA spin_component_allowed / 'up',  'down', 'none' /
       ! the allowed values for spin_component


   NAMELIST / SUBSPACE / dimwann, win_min, win_max, froz_min, froz_max, spin_component, &
                         alpha_dis, maxiter_dis, disentangle_thr, trial_mode


   PUBLIC :: dimwann, win_min, win_max, froz_min, froz_max, spin_component
   PUBLIC :: alpha_dis, maxiter_dis, disentangle_thr, trial_mode
   PUBLIC :: SUBSPACE
   

!======================================== 
! LOCALIZATION Namelist parameters
!======================================== 
   
   REAL(dbl) :: wannier_thr = 1d-6      
       ! threshold for convergence of the iterative wannier minimization

   REAL(dbl) :: alpha0_wan = 0.5_dbl
       ! mixing parameter during the first CG part of the wannier minimization

   REAL(dbl) :: alpha1_wan = 0.5_dbl
       ! mixing parameter during the second part of the wannier minimization

   INTEGER :: maxiter0_wan = 500
       ! maximum number of iterations for the first minim part

   INTEGER :: maxiter1_wan = 500
       ! maximum number of iterations for the second minim part

   INTEGER :: ncg = 3
       ! each ncg iterations in the second iteration part, do a CG minimization

   INTEGER :: iphase = 1
       ! obsolete variable

   INTEGER :: nshells = 0
       ! the number of kpt nearest-neighbour shells used in the calculations
       ! Hopefully will be removed very soon

   INTEGER :: nwhich(nnx)
       ! the indexes of the chosen shells
       ! as above, hopefully it will be removed very soon

   CHARACTER(nstrx) :: ordering_mode = "none"
       ! ( "spatial" | "complete" | "none" ) 
       ! after the minimization WF's maybe ordered for simplicity purposes
       ! following these three schemes
       ! "spatial"  : orders by means of the spatial position of the centers
       !              (distance from the origin)
       ! "complete" : centers which are close to each other are ordered for
       !              increasing spreads
       ! "none"     : disable the ordering
       !
   CHARACTER(nstrx) :: ordering_mode_allowed(3)    
   DATA ordering_mode_allowed / "spatial", "complete", "none" /


   NAMELIST / LOCALIZATION / wannier_thr, alpha0_wan, alpha1_wan, maxiter0_wan, &
     maxiter1_wan, ncg, iphase, nshells, nwhich, ordering_mode, nshells, nwhich


   PUBLIC :: wannier_thr, alpha0_wan, alpha1_wan, maxiter0_wan, maxiter1_wan
   PUBLIC :: ncg, iphase, nshells, nwhich, ordering_mode
   PUBLIC :: LOCALIZATION


!
! ... subroutines
!

   PUBLIC :: read_namelist_control
   PUBLIC :: read_namelist_subspace
   PUBLIC :: read_namelist_localization

CONTAINS

!**********************************************************
   SUBROUTINE read_namelist_control(unit)
   !**********************************************************
   !
   ! reads CONTROL namelist
   !
   IMPLICIT NONE
      INTEGER, INTENT(in)   :: unit

      CHARACTER(21) :: subname='read_namelist_control'
      LOGICAL :: allowed
      INTEGER :: i, ios, ierr

      READ(unit, CONTROL, IOSTAT=ios )
         IF (ios/=0) CALL errore(subname,'reading CONTROL namelist',ABS(ios))

      !
      ! ... checking parameters
      !
      IF ( unitary_thr <= 0 ) CALL errore(subname, ' unitary_thr must be positive ', 1 )
      IF ( nprint <= 0 ) CALL errore(subname, ' nprint must be non-negative ', -nprint+1 )

      CALL change_case(verbosity,'lower')
      allowed=.FALSE.
      DO i=1,SIZE(verbosity_allowed)
          IF ( TRIM(verbosity) == verbosity_allowed(i) ) allowed=.TRUE. 
      ENDDO
      IF (.NOT. allowed) CALL errore(subname,'Invalid verbosity "'//TRIM(verbosity)//'"',10)

   END SUBROUTINE read_namelist_control


!**********************************************************
   SUBROUTINE read_namelist_subspace(unit)
   !**********************************************************
   !
   ! reads SUBSPACE namelist
   !
   IMPLICIT NONE
      INTEGER, INTENT(in)   :: unit

      CHARACTER(22) :: subname='read__namelist_subspace'
      LOGICAL :: allowed
      INTEGER :: i, ios, ierr

      READ(unit, SUBSPACE, IOSTAT=ios )
         IF (ios/=0) CALL errore(subname,'reading SUBSPACE namelist',ABS(ios))

      !
      ! ... checking parameters
      !
      IF ( win_max <= win_min ) CALL errore(subname, 'win_max <= win_min ', 1 )
      IF ( froz_max <= froz_min ) CALL errore(subname, 'win_max <= win_min ', 1 )
      IF ( dimwann <= 0 ) CALL errore(subname, 'dimwann should be positive ', -dimwann+1 ) 
      IF ( alpha_dis <= 0.0 ) CALL errore(subname, 'alpha_dis should be positive ', 1 ) 
      IF ( alpha_dis > 1.0 ) CALL errore(subname, 'alpha_dis should <=1.0 ', 1 ) 
      IF ( maxiter_dis <= 0 ) CALL errore(subname, 'maxiter_dis should be >0',-maxiter_dis+1)
      IF ( disentangle_thr <= 0.0 ) CALL errore(subname, 'disentangle_thr should be > 0',1)

      CALL change_case(trial_mode,'lower')
      allowed=.FALSE.
      DO i=1,SIZE(trial_mode_allowed)
          IF ( TRIM(trial_mode) == trial_mode_allowed(i) ) allowed=.TRUE. 
      ENDDO
      IF (.NOT. allowed) CALL errore(subname,'Invalid trial_mode "'//TRIM(trial_mode)//'"',2)

      CALL change_case(spin_component,'lower')
      allowed=.FALSE.
      DO i=1,SIZE(spin_component_allowed)
          IF ( TRIM(spin_component) == spin_component_allowed(i) ) allowed=.TRUE. 
      ENDDO
      IF (.NOT. allowed) CALL errore(subname, &
             'Invalid spin_component "'//TRIM(spin_component)//'"',10)

   END SUBROUTINE read_namelist_subspace


!**********************************************************
   SUBROUTINE read_namelist_localization(unit)
   !**********************************************************
   !
   ! reads LOCALIZATION namelist
   !
   IMPLICIT NONE
      INTEGER, INTENT(in)   :: unit

      CHARACTER(26) :: subname='read_namelist_localization'
      LOGICAL :: allowed
      INTEGER :: i, ios, ierr

      READ(unit, LOCALIZATION, IOSTAT=ios )
         IF (ios/=0) CALL errore(subname,'reading LOCALIZATION namelist',ABS(ios))

      !
      ! ... checking parameters
      !
      IF ( wannier_thr <= 0.0 ) CALL errore(subname, 'wannier_thr should be > 0.0 ', 1 )
      IF ( alpha0_wan <= 0.0 ) CALL errore(subname, 'alpha0_wan should be positive ', 1 ) 
      IF ( alpha0_wan > 1.0 ) CALL errore(subname, 'alpha0_wan should <=1.0 ', 1 ) 
      IF ( alpha1_wan <= 0.0 ) CALL errore(subname, 'alpha1_wan should be positive ', 1 ) 
      IF ( alpha1_wan > 1.0 ) CALL errore(subname, 'alpha1_wan should <=1.0 ', 1 ) 
      IF ( maxiter0_wan <= 0 ) CALL errore(subname, 'maxiter0_wan should be >0',1)
      IF ( maxiter1_wan <= 0 ) CALL errore(subname, 'maxiter1_wan should be >0',1)
      IF ( ncg <= 0 ) CALL errore(subname, 'ncg should be >0',1)
      IF ( iphase /=1 ) CALL errore(subname, 'iphase MUST be 1',ABS(iphase-1))
      IF ( nshells <= 0 ) CALL errore(subname, 'nshells should be > 0',1)
      IF ( nshells > nnx ) CALL errore(subname, 'nshells should be < nnx',nnx)
      IF ( ANY( nwhich(1:nshells) <= 0 ) ) CALL errore(subname, 'nwhich should be >= 1',2)

      CALL change_case(ordering_mode,'lower')
      allowed=.FALSE.
      DO i=1,SIZE(ordering_mode_allowed)
          IF ( TRIM(ordering_mode) == ordering_mode_allowed(i) ) allowed=.TRUE. 
      ENDDO
      IF (.NOT. allowed) &
           CALL errore(subname,'Invalid ordering_mode "'//TRIM(ordering_mode)//'"',30)

   END SUBROUTINE read_namelist_localization

END MODULE input_parameters_module

