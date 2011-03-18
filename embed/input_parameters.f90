! 
! Copyright (C) 2005 WanT Group
! 
! This file is distributed under the terms of the 
! GNU General Public License. See the file `License' 
! in the root directory of the present distribution, 
! or http://www.gnu.org/copyleft/gpl.txt . 
! 
!********************************************
   MODULE E_input_parameters_module
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
! The E character in front of the module name stands there
! in order to distinguish from wannier/transport modules.
!

!
! ... declarations

!
!======================================== 
! INPUT Namelist parameters
!======================================== 

   INTEGER :: dim_emb = 0
       ! WF number the reduced subspace

   INTEGER :: dim_tot = 0
       ! WF number in the complete region

   INTEGER :: ne = 1000  
       ! the dimension of the energy grid

   INTEGER :: ne_buffer = 1
       ! dimension of the energy buffering for correlation sgm

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

   INTEGER :: transport_dir = 3
       ! the direction that we want to avoid to use... 

   INTEGER :: nk(2) =  0
       ! dimension of the 2D kpt mesh on which the input 
       ! Hamiltonian will be interpolated

   INTEGER :: s(2) =  0
       ! shifts for the generation of the 2D mesh of kpts
 
   LOGICAL :: use_symm = .FALSE.  
       ! whether to use symmetry to reduce the numberof kpts
       ! only Time-Rev is implemented at the moment

   LOGICAL :: write_embed_sgm = .TRUE.
       ! write the computed embed self-energy to disk.
       ! ACTUNG: at the moment it needs use_symm=.FALSE.

   INTEGER :: nprint = 20 
       ! every nprint energy step write to stdout

   CHARACTER(nstrx) :: work_dir   = './'
       ! working directory where to write datafiles

   CHARACTER(nstrx) :: prefix     = ' '
       ! prefix used for the names of files

   CHARACTER(nstrx) :: postfix     = ' '
       ! postfix used for the names of files

   CHARACTER(nstrx) :: datafile_tot = ' '
       ! the name of the file containing central conductor data

   CHARACTER(nstrx) :: datafile_emb = ' '
       ! the name of the file containing the reduced hamiltonian data

   CHARACTER(nstrx) :: datafile_sgm = ' '
       ! the name of the file containing correlation self-energy
       ! If a valid file is provided, correlation is taken into account

   CHARACTER(nstrx) :: datafile_sgm_emb = ' '
       ! the name of the file containing the embedding sgm

   REAL(dbl) :: shift_tot = 0.0
       ! global energy shift [eV] to be applied to the matrix elements
       ! of the hamiltonian
       
   INTEGER :: debug_level = 0
       ! level of debug report; values <= 0 switch the debug_mode off

   INTEGER :: ispin = 0
       ! define which spin component has to be used in the calculation.
       ! This variable is intended to exist temporarily until a full
       ! treatment of the spin degrees of freedom is not implemented.
       ! Currently it is used only within the interface with CRYSTAL06.


   NAMELIST / INPUT / dim_tot, dim_emb, ne, ne_buffer, emin, emax, nprint,   &
                 datafile_tot, datafile_emb, datafile_sgm, datafile_sgm_emb, &
                 use_symm, write_embed_sgm, &
                 delta, smearing_type, delta_ratio, xmax,                    &
                 transport_dir, nk, s, debug_level,                          &
                 work_dir, prefix, postfix, shift_tot, ispin

   PUBLIC :: dim_tot, dim_emb, ne, ne_buffer, emin, emax, nprint
   PUBLIC :: datafile_tot, datafile_emb, datafile_sgm, datafile_sgm_emb
   PUBLIC :: use_symm, write_embed_sgm
   PUBLIC :: transport_dir, nk, s, debug_level
   PUBLIC :: delta, smearing_type, delta_ratio, xmax
   PUBLIC :: work_dir, prefix, postfix, shift_tot, ispin
   PUBLIC :: INPUT


!
! ... subroutines
!

   PUBLIC :: read_namelist_input

CONTAINS

!**********************************************************
   SUBROUTINE read_namelist_input(unit)
   !**********************************************************
   !
   ! reads INPUT namelist
   !
   IMPLICIT NONE
      INTEGER, INTENT(in)   :: unit

      CHARACTER(19) :: subname='read_namelist_input'
      LOGICAL :: allowed, exists
      INTEGER :: i, ios

      CALL log_push( 'read_namelist_input' )

      IF ( ionode ) THEN
         !
         READ(unit, INPUT, IOSTAT=ios )
         IF (ios/=0) CALL errore(subname,'reading INPUT namelist',ABS(ios))
         !
      ENDIF

      !
      ! variable bcasting
      !
      CALL mp_bcast( dim_tot,            ionode_id)      
      CALL mp_bcast( dim_emb,            ionode_id)      
      CALL mp_bcast( transport_dir,      ionode_id)      
      CALL mp_bcast( ne,                 ionode_id)      
      CALL mp_bcast( ne_buffer,          ionode_id)      
      CALL mp_bcast( emin,               ionode_id)      
      CALL mp_bcast( emax,               ionode_id)      
      CALL mp_bcast( delta,              ionode_id)
      CALL mp_bcast( smearing_type,      ionode_id)
      CALL mp_bcast( delta_ratio,        ionode_id)
      CALL mp_bcast( xmax,               ionode_id)
      CALL mp_bcast( nprint,             ionode_id)      
      CALL mp_bcast( nk,                 ionode_id)      
      CALL mp_bcast( s,                  ionode_id)      
      CALL mp_bcast( use_symm,           ionode_id)      
      CALL mp_bcast( write_embed_sgm,    ionode_id)      
      CALL mp_bcast( debug_level,        ionode_id)      
      CALL mp_bcast( work_dir,           ionode_id)      
      CALL mp_bcast( prefix,             ionode_id)      
      CALL mp_bcast( postfix,            ionode_id)      
      CALL mp_bcast( datafile_tot,       ionode_id)      
      CALL mp_bcast( datafile_emb,       ionode_id)      
      CALL mp_bcast( datafile_sgm,       ionode_id)      
      CALL mp_bcast( datafile_sgm_emb,   ionode_id)      
      CALL mp_bcast( shift_tot,          ionode_id)      
      CALL mp_bcast( ispin,              ionode_id)      

      !
      ! ... checking parameters
      !
      IF ( transport_dir < 1 .OR. transport_dir > 3) &
           CALL errore(subname,'Invalid transport_dir',1)

      IF ( dim_tot <= 0) CALL errore(subname,'Invalid dim_tot',1)

      IF ( LEN_TRIM(datafile_tot) == 0 ) &
           CALL errore(subname,'datafile_tot unspecified',1)
      IF ( LEN_TRIM(datafile_emb) == 0 ) &
           CALL errore(subname,'datafile_emb unspecified',1)
      IF ( LEN_TRIM(datafile_sgm_emb) == 0 ) &
           CALL errore(subname,'datafile_sgm_emb unspecified',1)

      INQUIRE( FILE=datafile_tot, EXIST=exists )
      IF ( .NOT. exists ) CALL errore(subname,'unable to find '//TRIM(datafile_tot),1)
      !
      IF ( emax <= emin )    CALL errore(subname,'Invalid EMIN EMAX',1)
      IF ( ne <= 1 )         CALL errore(subname,'Invalid NE',1)
      IF ( ne_buffer <= 1 )  CALL errore(subname,'Invalid NE_BUFFER',1)

      IF ( delta < ZERO ) CALL errore(subname,'Invalid DELTA',1)
      IF ( delta > 3.0_dbl* EPS_m1 ) CALL errore(subname,'DELTA too large',1)

      CALL change_case(smearing_type,'lower')
      allowed=.FALSE.
      DO i=1,SIZE(smearing_type_allowed)
          IF ( TRIM(smearing_type) == smearing_type_allowed(i) ) allowed=.TRUE.
      ENDDO
      IF (.NOT. allowed) &
          CALL errore(subname,'Invalid smearing_type ='//TRIM(smearing_type),10)

      IF ( dim_emb <= 0)        CALL errore(subname,'Invalid dim_emb',1)
      IF ( dim_emb > dim_tot)   CALL errore(subname,'Invalid dim_emb > dim_tot',1)

      IF ( ANY( nk(:) < 0 ) ) CALL errore(subname,'invalid nk', 10 )
      IF ( ANY( s(:)  < 0 .OR.  s(:) > 1 ) ) CALL errore(subname,'invalid s', 10 )

      IF ( ispin < 0 ) CALL errore(subname, 'ispin too small', 1)
      IF ( ispin > 2 ) CALL errore(subname, 'ispin too large', 2)

      IF ( write_embed_sgm .AND. use_symm ) &
          CALL errore(subname,'use_symm and write_sgm not implemented',1)

      CALL log_pop( 'read_namelist_input' )

   END SUBROUTINE read_namelist_input

END MODULE E_input_parameters_module

