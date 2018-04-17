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
   USE parameters, ONLY : nstrx
   USE parser_module, ONLY : change_case
   IMPLICIT NONE
   PRIVATE
   SAVE
!
! This module contains the definitions of the parameters in the
! input file and of thier default values (when any). 
! These data are then exported to the input module where the
! main routine controls the IO and after that exports to the
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

   CHARACTER(nstrx) :: restart_mode = 'from_scratch'
       ! ( 'from_scratch' | 'restart' )
       ! define whether to restart a previous calculation; at the moment it implies
       ! OVERLAPS="from_file", PROJECTIONS="from_file", SUBSPACE="from_file", and 
       ! UNITARY="from_file"

   CHARACTER(nstrx) :: restart_mode_allowed(2)
   DATA restart_mode_allowed / 'from_scratch',  'restart' /
       ! the allowed values for restart_mode

   CHARACTER(nstrx) :: verbosity = 'medium'
       ! ( 'low' | 'medium' | 'high' )
       ! the level of details of the code output

   CHARACTER(nstrx) :: verbosity_allowed(3)
   DATA verbosity_allowed / 'low',  'medium', 'high' /
       ! the allowed values for verbosity

   CHARACTER(nstrx) :: overlaps = 'from_scratch'
       ! ( 'from_scratch' | 'from_file' )
       ! wheter overlaps are calculated or read from an existing file

   CHARACTER(nstrx) :: overlaps_allowed(2)
   DATA overlaps_allowed / 'from_scratch',  'from_file' /
       ! the allowed values for overlaps

   CHARACTER(nstrx) :: projections = 'from_scratch'
       ! ( 'from_scratch' | 'from_file' )
       ! wheter projections are calculated or read from an existing file

   CHARACTER(nstrx) :: projections_allowed(2)
   DATA projections_allowed / 'from_scratch',  'from_file' /
       ! the allowed values for projections

   LOGICAL :: assume_ncpp = .FALSE.
       ! if .TRUE. this variable avoids the reading of pseudopotential files
       ! assuming the DFT calculation has been performed within norm-conserving
       ! pseudopotentials (for whom no knowledge of them is required in the WanT calc)

   REAL(dbl) :: unitary_thr = 1.0d-6  
       ! threshold for the check of matrix unitariery

   NAMELIST / CONTROL /  title, prefix, postfix, restart_mode, work_dir, verbosity, &
                         overlaps, projections, assume_ncpp, unitary_thr


   PUBLIC :: title, prefix, postfix, work_dir
   PUBLIC :: overlaps, projections, restart_mode
   PUBLIC :: verbosity, assume_ncpp, unitary_thr
   PUBLIC :: CONTROL


!======================================== 
! SUBSPACE Namelist parameters
!======================================== 

   INTEGER :: dimwann = 0
       ! the number of wannier functions, i.e. the dimension of the wannier subspace

   REAL(dbl) :: win_min =  -20000000.0_dbl
       ! the lower bound of the energy window for the determination of the 
       ! Wannier subspace
     
   REAL(dbl) :: win_max =   20000000.0_dbl
       ! the upper bound of the above defined energy window

   REAL(dbl) :: froz_min = -60000000.0_dbl
       ! the lower bound of a second energy window which "frozes" the
       ! inner states to be part of the subspace.

   REAL(dbl) :: froz_max = -50000000.0_dbl
       ! the upper bound of the frozen energy window

   REAL(dbl) :: alpha_dis = 0.5_dbl
       ! the mixing parameter for iterative minimization in disentangle procedure

   INTEGER :: maxiter_dis = 1000
       ! maximum number of iterations during the disentangle procedure

   INTEGER :: nprint_dis = 10  
       ! every nprint iterations in disentangle minimizations write to stdout

   INTEGER :: nsave_dis = 10  
       ! every nsave iterations in disentangle minimizations write subspace to disk

   REAL(dbl) :: disentangle_thr = 1.0d-8  
       ! threshold for convergence of the iterative disentangle procedure

   CHARACTER(nstrx) :: subspace_init = "center_projections"
       ! ( 'randomized' | 'lower_states' | 'upper_states' | 
       !   'center_projections' | 'from_file' )
       !
       ! Determine how the trial subspace is chosen
       ! 'randomized'   : random lamp matrix
       ! 'lower_states' : the lower DIMWANN bands from DFT calculation are
       !                  used to define the subspace
       ! 'upper_states' : the upper DIMWANN bands from DFT calculation are
       !                  used to define the subspace
       ! 'center_projections' : a subspace is extracted from the DFT bands
       !                  by means of a projections on the given WANNIER_TRIAL_CENTERS
       !                  (see the section TRIAL_CENTERS)
       ! 'from_file'    : read from an existing file (default for restart)

   CHARACTER(nstrx) :: subspace_init_allowed(5)
   DATA subspace_init_allowed / 'randomized', 'lower_states', 'upper_states', &
                                'center_projections', 'from_file' /

   CHARACTER(10)    :: spin_component = 'none'
       ! ( 'up' | 'down' | 'none' )
       ! define whether the calculation is spin polarized and if
       ! the case which spin component is to be treated 

   CHARACTER(nstrx) :: spin_component_allowed(3)
   DATA spin_component_allowed / 'up',  'down', 'none' /
       ! the allowed values for spin_component

   LOGICAL :: use_blimit = .FALSE.
       ! if .TRUE. sets b = 0 in the calculation of overlap augmentation

   NAMELIST / SUBSPACE / dimwann, win_min, win_max, froz_min, froz_max, spin_component, &
                         alpha_dis, maxiter_dis, disentangle_thr, nprint_dis, nsave_dis, &
                         subspace_init, use_blimit


   PUBLIC :: dimwann, win_min, win_max, froz_min, froz_max, spin_component, use_blimit
   PUBLIC :: alpha_dis, maxiter_dis, nprint_dis, nsave_dis, disentangle_thr, subspace_init
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

   REAL(dbl) :: a_condmin = 0.0_dbl
       ! amplitude of the auxiliary part of the functional driving the
       ! conditioned minimization

   REAL(dbl) :: dump_condmin = 0.0_dbl
       ! dumping factor for condmin swhitching off

   INTEGER :: niter_condmin = 0
       ! number of conditioned iterations in wannier

   INTEGER :: maxiter0_wan = 500
       ! maximum number of iterations for the first minim part

   INTEGER :: maxiter1_wan = 500
       ! maximum number of iterations for the second minim part

   INTEGER :: ncg = 3
       ! each ncg iterations in the second iteration part, do a CG minimization

   INTEGER :: nprint_wan = 10  
       ! each nprint_wan iterations in wannier minimizations write to stdout

   INTEGER :: nsave_wan = 10  
       ! each nsave_wan iterations in wannier minimizations save data to disk

   CHARACTER(nstrx) :: localization_init = "center_projections"
       ! ( 'randomized' | 'no_guess' | 'center_projections' | 'from_file' )
       !
       ! Determine how the wannier localization is started
       ! 'no_guess'            : Cu's are set equal to the identity
       ! 'randomized'          : Cu's are unitary random matrixes
       ! 'center_projections'  : a subspace is extracted from the DFT bands
       !                  by means of a projections on the given WANNIER_TRIAL_CENTERS
       !                  (see the section TRIAL_CENTERS)
       ! 'from_file'           : read from an existing file (default for restart)

   CHARACTER(nstrx) :: localization_init_allowed(4)
   DATA localization_init_allowed / 'no_guess', 'randomized', 'center_projections', &
                                 'from_file' /

   CHARACTER(nstrx) :: ordering_mode = "none"
       ! ( "none" | "spatial" | "spread" | "complete" ) 
       ! after the minimization WF's maybe ordered for simplicity purposes
       ! following these three schemes
       ! "none"     : disable the ordering
       ! "spatial"  : ordering based on WF center positions (distance from the origin)
       ! "spread"   : ordering based on WF increasing spreads
       ! "complete" : SPATIAL + SPREAD for WF with the same centers
       !
   CHARACTER(nstrx) :: ordering_mode_allowed(4)    
   DATA ordering_mode_allowed / "none", "spatial", "spread", "complete" /


   NAMELIST / LOCALIZATION / wannier_thr, alpha0_wan, alpha1_wan, maxiter0_wan, &
     maxiter1_wan, nprint_wan, nsave_wan, ncg, localization_init, &
     ordering_mode, a_condmin, niter_condmin, dump_condmin


   PUBLIC :: wannier_thr, alpha0_wan, alpha1_wan, maxiter0_wan, maxiter1_wan
   PUBLIC :: nprint_wan, nsave_wan, ncg, localization_init
   PUBLIC :: ordering_mode, a_condmin, niter_condmin, dump_condmin
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
      INTEGER :: i, ios

      READ(unit, CONTROL, IOSTAT=ios )
         IF (ios/=0) CALL errore(subname,'reading CONTROL namelist',ABS(ios))

      !
      ! ... checking parameters
      !
      IF ( unitary_thr <= 0 ) CALL errore(subname, ' unitary_thr must be positive ', 1 )

      CALL change_case(restart_mode,'lower')
      allowed=.FALSE.
      DO i=1,SIZE(restart_mode_allowed)
          IF ( TRIM(restart_mode) == restart_mode_allowed(i) ) allowed=.TRUE. 
      ENDDO
      IF (.NOT. allowed) &
          CALL errore(subname,'Invalid restart_mode "'//TRIM(restart_mode)//'"',10)

      CALL change_case(verbosity,'lower')
      allowed=.FALSE.
      DO i=1,SIZE(verbosity_allowed)
          IF ( TRIM(verbosity) == verbosity_allowed(i) ) allowed=.TRUE. 
      ENDDO
      IF (.NOT. allowed) CALL errore(subname,'Invalid verbosity "'//TRIM(verbosity)//'"',10)

      CALL change_case(overlaps,'lower')
      allowed=.FALSE.
      DO i=1,SIZE(overlaps_allowed)
          IF ( TRIM(overlaps) == overlaps_allowed(i) ) allowed=.TRUE. 
      ENDDO
      IF (.NOT. allowed) CALL errore(subname,'Invalid overlaps "'//TRIM(overlaps)//'"',11)

      CALL change_case(projections,'lower')
      allowed=.FALSE.
      DO i=1,SIZE(projections_allowed)
          IF ( TRIM(projections) == projections_allowed(i) ) allowed=.TRUE. 
      ENDDO
      IF (.NOT. allowed) &
          CALL errore(subname,'Invalid projections "'//TRIM(projections)//'"',12)

   END SUBROUTINE read_namelist_control


!**********************************************************
   SUBROUTINE read_namelist_subspace(unit)
   !**********************************************************
   !
   ! reads SUBSPACE namelist
   !
   IMPLICIT NONE
      INTEGER, INTENT(in)   :: unit

      CHARACTER(22) :: subname='read_namelist_subspace'
      LOGICAL :: allowed
      INTEGER :: i, ios

      READ(unit, SUBSPACE, IOSTAT=ios )
         IF (ios/=0) CALL errore(subname,'reading SUBSPACE namelist',ABS(ios))

      !
      ! ... checking parameters
      !
      IF ( win_max <= win_min ) CALL errore(subname, 'win_max <= win_min ', 1 )
      IF ( froz_max <= froz_min ) CALL errore(subname, 'win_max <= win_min ', 1 )
      IF ( dimwann <= 0 ) CALL errore(subname, 'dimwann should be positive ', -dimwann+1 ) 
      IF ( alpha_dis <= 0.0 ) CALL errore(subname, 'alpha_dis should be positive ', 1 ) 
      IF ( alpha_dis > 1.0) CALL errore(subname, 'alpha_dis should <=1.0 ', 1 ) 
      IF ( maxiter_dis < 0) CALL errore(subname, 'maxiter_dis should be >= 0',-maxiter_dis+1)
      IF ( disentangle_thr <= 0.0 ) CALL errore(subname, 'disentangle_thr should be > 0',1)
      IF ( nprint_dis <= 0) CALL errore(subname, ' nprint_dis must be > 0 ', -nprint_dis+1 )
      IF ( nsave_dis <= 0 ) CALL errore(subname, ' nsave_dis must be > 0 ', -nsave_dis+1 )

      CALL change_case(subspace_init,'lower')
      allowed=.FALSE.
      DO i=1,SIZE(subspace_init_allowed)
          IF ( TRIM(subspace_init) == subspace_init_allowed(i) ) allowed=.TRUE. 
      ENDDO
      IF (.NOT. allowed) &
         CALL errore(subname,'Invalid subspace_init "'//TRIM(subspace_init)//'"',2)

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
      INTEGER :: i, ios

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
      IF ( maxiter0_wan < 0 ) CALL errore(subname, 'maxiter0_wan should be >= 0',1)
      IF ( maxiter1_wan < 0 ) CALL errore(subname, 'maxiter1_wan should be >= 0',1)
      IF ( niter_condmin < 0 ) CALL errore(subname, 'niter_condmin should be >= 0',1)
      IF ( dump_condmin < 0.0 ) CALL errore(subname, 'dump_condmin should be >= 0.0',1)
      IF ( nprint_wan <= 0 ) CALL errore(subname, ' nprint_wan must be > 0 ', -nprint_wan+1 )
      IF ( nsave_wan <= 0 ) CALL errore(subname, ' nsave_wan must be > 0 ', -nsave_wan+1 )
      IF ( ncg <= 0 ) CALL errore(subname, 'ncg should be >0',1)

      CALL change_case(localization_init,'lower')
      allowed=.FALSE.
      DO i=1,SIZE(localization_init_allowed)
          IF ( TRIM(localization_init) == localization_init_allowed(i) ) allowed=.TRUE. 
      ENDDO
      IF (.NOT. allowed) &
         CALL errore(subname,'Invalid localization_init "'//TRIM(localization_init)//'"',2)

      CALL change_case(ordering_mode,'lower')
      allowed=.FALSE.
      DO i=1,SIZE(ordering_mode_allowed)
          IF ( TRIM(ordering_mode) == ordering_mode_allowed(i) ) allowed=.TRUE. 
      ENDDO
      IF (.NOT. allowed) &
           CALL errore(subname,'Invalid ordering_mode "'//TRIM(ordering_mode)//'"',30)

   END SUBROUTINE read_namelist_localization

END MODULE input_parameters_module

