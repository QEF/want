! 
! Copyright (C) 2005 WanT Group
! 
! This file is distributed under the terms of the 
! GNU General Public License. See the file `License' 
! in the root directory of the present distribution, 
! or http://www.gnu.org/copyleft/gpl.txt . 
! 
!********************************************
   MODULE T_input_parameters_module
!********************************************
   !
   USE kinds,         ONLY : dbl
   USE constants,     ONLY : ZERO, EPS_m1, EPS_m2, EPS_m5, EPS_m4, EPS_m3 
   USE parameters,    ONLY : nstrx
   USE parser_module, ONLY : change_case
   USE io_module,     ONLY : ionode, ionode_id
   USE log_module,    ONLY : log_push, log_pop
   USE mp,            ONLY : mp_bcast
   !
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
! The T character in front of the module name stands there
! in order to distinguish from wannier modules.
!
! Here are also the routine reading and checking the NAMELIST
!
! routines in this module:
! SUBROUTINE  read_namelist_input_conductor(unit)
!

!
! ... declarations

!
!======================================== 
! INPUT_CONDUCTOR Namelist parameters
!======================================== 

   INTEGER :: dimL = 0
       ! WF number in lead L

   INTEGER :: dimR = 0
       ! WF number in lead R

   INTEGER :: dimC = 0
       ! WF number in the central conductor region

   INTEGER :: transport_dir = 0
       ! transport direction 

   CHARACTER(nstrx) :: calculation_type = 'conductor'
       ! ( 'conductor' | 'bulk' )
       ! wheter full conductor or bulk calculation is performed

   CHARACTER(nstrx) :: calculation_type_allowed(2)
   DATA calculation_type_allowed / 'conductor',  'bulk' /
       ! the allowed values for calculation_type

   CHARACTER(nstrx) :: conduct_formula = 'landauer'
       ! ( 'landauer' | 'generalized' )
       ! wheter use landauer of correlation corrected formula

   CHARACTER(nstrx) :: conduct_formula_allowed(2)
   DATA conduct_formula_allowed / 'landauer',  'generalized' /
       ! the allowed values for conduct_formula

   INTEGER :: ne = 1000  
       ! the dimension of the energy grid

   REAL(dbl) :: emin = -10.0
       ! lower bound of the energy range

   REAL(dbl) :: emax =  10.0
       ! upper bound of the energy range

   REAL(dbl) :: delta =  EPS_m5
       ! i\delta broadening of green functions

   CHARACTER(30) :: smearing_type = 'lorentzian'
       ! type of smearing technique

   CHARACTER(30) :: smearing_type_allowed(8)
   DATA smearing_type_allowed / 'lorentzian',  'gaussian', 'fermi-dirac', 'fd',         &
                                'methfessel-paxton', 'mp', 'marzari-vanderbilt', 'mv' /
       ! the allowed values for smearing_type
       
   REAL(dbl) :: delta_ratio =  5.0_dbl * EPS_m3
       ! ratio between ddelta (used for convolution with the pole) and
       ! smearing delta

   REAL(dbl) :: xmax = 25.0    
        ! grid extrema  (-xmax, xmax)

   REAL(dbl) :: bias =  ZERO
       ! effective bias between the leads
       ! not fully implemented at the moment

   INTEGER :: nk(2) =  0
       ! dimension of the 2D kpt mesh on which the input 
       ! Hamiltonian will be interpolated

   INTEGER :: s(2) =  0
       ! shifts for the generation of the 2D mesh of kpts
 
   LOGICAL :: use_symm = .FALSE.  ! this is a tmp default
       ! whether to use symmetry to reduce the numberof kpts
       ! only Time-Rev is implemented at the moment

   INTEGER :: nprint = 20 
       ! every nprint energy step write to stdout

   INTEGER :: niterx = 200
       ! max number of iterations in the calculation of
       ! lead self-energies

   INTEGER :: nfailx = 5
       ! max allowed number of failures during lead-sgm calculation

   LOGICAL :: write_kdata = .FALSE.
       ! write kpoint-resolved dos an transmittance to output

   LOGICAL :: write_lead_sgm = .FALSE.
       ! write the computed lead self-energies to disk.

   LOGICAL :: do_eigenchannels = .FALSE.
       ! compute eigenchannels

   CHARACTER(nstrx) :: work_dir   = './'
       ! working directory where to write datafiles

   CHARACTER(nstrx) :: prefix     = ' '
       ! prefix used for the names of files

   CHARACTER(nstrx) :: postfix     = ' '
       ! postfix used for the names of files

   CHARACTER(nstrx) :: datafile_L = ' '
       ! the name of the file containing L lead data

   CHARACTER(nstrx) :: datafile_C = ' '
       ! the name of the file containing central conductor data

   CHARACTER(nstrx) :: datafile_R = ' '
       ! the name of the file containing R lead data

   CHARACTER(nstrx) :: datafile_sgm = ' '
       ! the name of the file containing correlation self-energy
       ! If a valid file is provided, correlation is taken into account

   REAL(dbl) :: shift_L = 0.0
       ! global energy shift [eV] to be applied to the matrix elements
       ! of the left lead (H00_L, H01_L)

   REAL(dbl) :: shift_C = 0.0
       ! global energy shift [eV] to be applied to the matrix elements
       ! of the conductor region (H00_C, H_LC, H_CR)
       
   REAL(dbl) :: shift_R = 0.0
       ! global energy shift [eV] to be applied to the matrix elements
       ! of the right lead (H00_R, H01_R)

   REAL(dbl) :: shift_corr = 0.0
       ! global energy shift [eV] to be applied to the matrix elements
       ! of the correlation self-energy operator.

   INTEGER :: debug_level = 0
       ! level of debug report; values <= 0 switch the debug_mode off

   INTEGER :: ispin = 0
       ! define which spin component has to be used in the calculation.
       ! This variable is intended to exist temporarily until a full
       ! treatment of the spin degrees of freedom is not implemented.
       ! Currently it is used only within the interface with CRYSTAL06.

   NAMELIST / INPUT_CONDUCTOR / dimL, dimC, dimR, calculation_type,             &
                 conduct_formula, niterx, ne, emin, emax, nprint, delta, bias,  &
                 datafile_L, datafile_C, datafile_R, datafile_sgm,              &
                 transport_dir, smearing_type, do_eigenchannels,                &
                 delta_ratio, xmax, nk, s, use_symm, debug_level,               &
                 work_dir, prefix, postfix, write_kdata, write_lead_sgm, ispin, &
                 shift_L, shift_C, shift_R, shift_corr, nfailx 


   PUBLIC :: dimL, dimC, dimR, calculation_type, conduct_formula, niterx, smearing_type
   PUBLIC :: ne, emin, emax, nprint, delta, bias, delta_ratio, xmax 
   PUBLIC :: datafile_sgm, datafile_L, datafile_C, datafile_R, transport_dir    
   PUBLIC :: nk, s, use_symm, debug_level
   PUBLIC :: work_dir, prefix, postfix, ispin, write_kdata, write_lead_sgm, do_eigenchannels
   PUBLIC :: shift_L, shift_C, shift_R, shift_corr, nfailx
   PUBLIC :: INPUT_CONDUCTOR


!
! ... subroutines
!

   PUBLIC :: read_namelist_input_conductor

CONTAINS

!**********************************************************
   SUBROUTINE read_namelist_input_conductor(unit)
   !**********************************************************
   !
   ! reads INPUT_CONDUCTOR namelist
   !
   IMPLICIT NONE
      INTEGER, INTENT(in)   :: unit

      CHARACTER(29) :: subname='read_namelist_input_conductor'
      LOGICAL :: allowed, exists
      INTEGER :: i, ios

      CALL log_push( 'read_namelist_input_conductor' )

      IF ( ionode ) THEN
         !
         READ(unit, INPUT_CONDUCTOR, IOSTAT=ios )
         IF (ios/=0) CALL errore(subname,'reading INPUT_CONDUCTOR namelist',ABS(ios))
         !
      ENDIF

      !
      ! variable bcasting
      !
      CALL mp_bcast( dimL,               ionode_id)      
      CALL mp_bcast( dimC,               ionode_id)      
      CALL mp_bcast( dimR,               ionode_id)      
      CALL mp_bcast( transport_dir,      ionode_id)      
      CALL mp_bcast( calculation_type,   ionode_id)      
      CALL mp_bcast( conduct_formula,    ionode_id)      
      CALL mp_bcast( ne,                 ionode_id)      
      CALL mp_bcast( emin,               ionode_id)      
      CALL mp_bcast( emax,               ionode_id)      
      CALL mp_bcast( delta,              ionode_id)      
      CALL mp_bcast( smearing_type,      ionode_id)      
      CALL mp_bcast( delta_ratio,        ionode_id)      
      CALL mp_bcast( xmax,               ionode_id)      
      CALL mp_bcast( bias,               ionode_id)      
      CALL mp_bcast( nprint,             ionode_id)      
      CALL mp_bcast( niterx,             ionode_id)      
      CALL mp_bcast( write_kdata,        ionode_id)      
      CALL mp_bcast( write_lead_sgm,     ionode_id)      
      CALL mp_bcast( nk,                 ionode_id)      
      CALL mp_bcast( s,                  ionode_id)      
      CALL mp_bcast( use_symm,           ionode_id)      
      CALL mp_bcast( debug_level,        ionode_id)      
      CALL mp_bcast( do_eigenchannels,   ionode_id)      
      CALL mp_bcast( ispin,              ionode_id)      
      CALL mp_bcast( work_dir,           ionode_id)      
      CALL mp_bcast( prefix,             ionode_id)      
      CALL mp_bcast( postfix,            ionode_id)      
      CALL mp_bcast( datafile_L,         ionode_id)      
      CALL mp_bcast( datafile_C,         ionode_id)      
      CALL mp_bcast( datafile_R,         ionode_id)      
      CALL mp_bcast( datafile_sgm,       ionode_id)      
      CALL mp_bcast( shift_L,            ionode_id)      
      CALL mp_bcast( shift_C,            ionode_id)      
      CALL mp_bcast( shift_R,            ionode_id)      
      CALL mp_bcast( shift_corr,         ionode_id)      
      CALL mp_bcast( nfailx,             ionode_id)      

      !
      ! ... checking parameters
      !
      IF ( transport_dir < 1 .OR. transport_dir > 3) &
           CALL errore(subname,'Invalid transport_dir',1)

      IF ( dimC <= 0) CALL errore(subname,'Invalid dimC',1)

      IF ( LEN_TRIM(datafile_C) == 0 ) &
           CALL errore(subname,'datafile_C unspecified',1)

      INQUIRE( FILE=datafile_C, EXIST=exists )
      IF ( .NOT. exists ) CALL errore(subname,'unable to find '//TRIM(datafile_C),1)
      !
      IF ( emax <= emin ) CALL errore(subname,'Invalid EMIN EMAX',1)
      IF ( ne <= 1 ) CALL errore(subname,'Invalid NE',1)
      IF ( niterx <= 0 ) CALL errore(subname,'Invalid NITERX',1)
      IF ( nprint <= 0) CALL errore(subname, ' nprint must be > 0 ', -nprint+1 )
      IF ( delta < ZERO ) CALL errore(subname,'Invalid DELTA',1)

      IF ( delta > 3.0_dbl* EPS_m1 ) CALL errore(subname,'DELTA too large',1)

      CALL change_case(calculation_type,'lower')
      allowed=.FALSE.
      DO i=1,SIZE(calculation_type_allowed)
          IF ( TRIM(calculation_type) == calculation_type_allowed(i) ) allowed=.TRUE. 
      ENDDO
      IF (.NOT. allowed) &
          CALL errore(subname,'Invalid calculation_type ='//TRIM(calculation_type),10)

      CALL change_case(conduct_formula,'lower')
      allowed=.FALSE.
      DO i=1,SIZE(conduct_formula_allowed)
          IF ( TRIM(conduct_formula) == conduct_formula_allowed(i) ) allowed=.TRUE. 
      ENDDO
      IF (.NOT. allowed) &
          CALL errore(subname,'Invalid conduct_formula ='//TRIM(conduct_formula),10)

      CALL change_case(smearing_type,'lower')
      allowed=.FALSE.
      DO i=1,SIZE(smearing_type_allowed)
          IF ( TRIM(smearing_type) == smearing_type_allowed(i) ) allowed=.TRUE. 
      ENDDO
      IF (.NOT. allowed) &
          CALL errore(subname,'Invalid smearing_type ='//TRIM(smearing_type),10)

      IF ( TRIM(calculation_type) == 'conductor' ) THEN
           IF ( dimL <= 0) CALL errore(subname,'Invalid dimL',1)
           IF ( dimR <= 0) CALL errore(subname,'Invalid dimR',1)
           !
           IF ( LEN_TRIM(datafile_L) == 0 ) &
                CALL errore(subname,'datafile_L unspecified',1)
           IF ( LEN_TRIM(datafile_R) == 0 ) &
                CALL errore(subname,'datafile_R unspecified',1)

           !
           INQUIRE( FILE=datafile_L, EXIST=exists )
           IF ( .NOT. exists ) CALL errore(subname,'unable to find '//TRIM(datafile_L),1)
           ! 
           INQUIRE( FILE=datafile_R, EXIST=exists )
           IF ( .NOT. exists ) CALL errore(subname,'unable to find '//TRIM(datafile_R),1)
           !
      ELSE
           !
           ! bulk case
           !
           IF ( dimL /= 0) CALL errore(subname,'dimL should not be specified',1)
           IF ( dimR /= 0) CALL errore(subname,'dimR should not be specified',1)
           dimL = dimC
           dimR = dimC
           IF ( LEN_TRIM(datafile_L) /= 0 ) &
                CALL errore(subname,'datafile_L should not be specified',1)
           IF ( LEN_TRIM(datafile_R) /= 0 ) &
                CALL errore(subname,'datafile_R should not be specified',1)
      ENDIF

      IF ( ANY( nk(:) < 0 ) ) CALL errore(subname,'invalid nk', 10 )
      IF ( ANY( s(:)  < 0 .OR.  s(:) > 1 ) ) CALL errore(subname,'invalid s', 10 )

      IF ( xmax < 10.0_dbl )      CALL errore(subname,'xmax too small',1)
      IF ( delta_ratio < ZERO )   CALL errore(subname,'delta_ratio is negative',1)
      IF ( delta_ratio > EPS_m1 ) CALL errore(subname,'delta_ratio too large',1)

      IF ( TRIM(conduct_formula) /= 'landauer' .AND. LEN_TRIM (datafile_sgm) == 0 ) &
           CALL errore(subname,'invalid conduct formula',1)

      IF ( ispin < 0 ) CALL errore(subname, 'ispin too small', 1) 
      IF ( ispin > 2 ) CALL errore(subname, 'ispin too large', 2) 

      CALL log_pop( 'read_namelist_input_conductor' )

   END SUBROUTINE read_namelist_input_conductor


END MODULE T_input_parameters_module

