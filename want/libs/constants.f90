!
! Copyright (C) 2002 FPMD group
! This file is distributed under the terms of the
! GNU General Public License. See the file `License'
! in the root directory of the present distribution,
! or http://www.gnu.org/copyleft/gpl.txt .
!

      MODULE constants

        USE kinds
  !
  !    The constants needed everywhere
  !
        IMPLICIT NONE
        SAVE

! ...   Numerical constants
        
        REAL(dbl), PARAMETER ::    ZERO = 0.0_dbl
        REAL(dbl), PARAMETER ::     ONE = 1.0_dbl
        REAL(dbl), PARAMETER ::     TWO = 2.0_dbl
        REAL(dbl), PARAMETER ::   THREE = 3.0_dbl
        COMPLEX(dbl), PARAMETER:: CZERO = (0.0_dbl, 0.0_dbl)
        COMPLEX(dbl), PARAMETER::  CONE = (1.0_dbl, 0.0_dbl)
        COMPLEX(dbl), PARAMETER::    CI = (0.0_dbl, 1.0_dbl)

        REAL(dbl), PARAMETER ::      PI = 3.14159265358979323846_dbl
        REAL(dbl), PARAMETER ::     TPI = 2.0_dbl * 3.14159265358979323846_dbl
        REAL(dbl), PARAMETER ::     FPI = 4.0_dbl * 3.14159265358979323846_dbl
        REAL(dbl), PARAMETER ::   SQRT2 = 1.41421356237309504880_dbl
        REAL(dbl), PARAMETER ::  SQRTPI = 1.77245385090551602729_dbl
        REAL(dbl), PARAMETER :: SQRTPM1 = 1.0_dbl / SQRTPI

! ...   System related constants
        REAL(dbl) :: ALAT, TPIBA, TPIBA2

! ...   Physical constants
        REAL(dbl), PARAMETER :: K_BOLTZMAN_SI    = 1.38066D-23       ! J K^-1 
        REAL(dbl), PARAMETER :: K_BOLTZMAN_AU    = 3.1667D-6         ! Hartree K^-1 
        REAL(dbl), PARAMETER :: K_BOLTZMAN_M1_AU = 315795.26D0       ! Hartree^-1 K 
        REAL(dbl), PARAMETER :: FACTEM           = 315795.26D0       ! Hartree^-1 K 

! ...   Physical constants defining the Atomic Units System
        REAL(dbl), PARAMETER :: BOHR_RADIUS_SI   = 0.529177D-10      ! m
        REAL(dbl), PARAMETER :: BOHR_RADIUS_CM   = 0.529177D-8       ! cm
        REAL(dbl), PARAMETER :: BOHR_RADIUS_ANGS = 0.529177D0        ! angstrom
        REAL(dbl), PARAMETER :: ELECTRONMASS_SI  = 9.10953D-31       ! Kg
        REAL(dbl), PARAMETER :: ELECTRONMASS_UMA = 5.4858D-4         ! uma

! ...   Units conversion factors
        REAL(dbl), PARAMETER :: ELECTRONVOLT_SI  = 1.6021892D-19     ! J  
        REAL(dbl), PARAMETER :: UMA_SI           = 1.66057D-27       ! Kg
        REAL(dbl), PARAMETER :: ANGSTROM_AU      = 1.889727D0        ! au
        REAL(dbl), PARAMETER :: AU_TO_OHMCMM1    = 46000.0D0         ! (ohm cm)^-1
        REAL(dbl), PARAMETER :: AU_KB            = 294210.0D0        ! Kbar
        REAL(dbl), PARAMETER :: KB_AU            = 1.0D0/294210.0D0  ! au
        REAL(dbl), PARAMETER :: AU               = 27.211652d0       ! eV
        REAL(dbl), PARAMETER :: RY               = 13.605826d0       ! eV 
        REAL(dbl), PARAMETER :: SCMASS           = 1822.89D0         ! uma to au
        REAL(dbl), PARAMETER :: UMA_AU           = 1822.89D0         ! au
        REAL(dbl), PARAMETER :: AU_TERAHERTZ     = 2.418D-5          ! THz
        REAL(dbl), PARAMETER :: TERAHERTZ        = 2.418D-5          ! from au to THz
        REAL(dbl), PARAMETER :: AU_SEC           = 2.4189D-17        ! sec
       

        REAL(dbl), PARAMETER :: rhothr = 1.0e-5_dbl ! tolerance
        REAL(dbl), PARAMETER :: gsmall = 1.0d-12

        REAL(dbl), PARAMETER :: e2 = 2.d0           ! the square of the electron charge
        REAL(dbl), PARAMETER :: degspin = 2.d0      ! the number of spins per level
        REAL(dbl), PARAMETER :: rytoev=13.6058d0    ! conversion from Ry to eV

        !  mass conversion: a.m.u to a.u. (Ry)
        REAL(dbl), PARAMETER :: amconv= 1.66042d-24/9.1095d-28*0.5d0 
        !  pressure conversion from Ry/(a.u)^3 to K
        REAL(dbl), PARAMETER :: uakbar= 147105.d0


      CONTAINS

        SUBROUTINE constants_setup(CELLDM)
           REAL(dbl), INTENT(IN) :: celldm
           If(celldm.le.0.0d0) THEN
             CALL errore(' constants_setup ', ' zero or negative CELLDM ',1)
           END IF
           alat   = celldm
           tpiba  = tpi / celldm       ! scaling constant: 2*pi/alat
           tpiba2 = tpiba ** 2
           RETURN
        END SUBROUTINE constants_setup

      END MODULE constants
