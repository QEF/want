!
! Copyright (C) 2001-2004 PWSCF group
! This file is distributed under the terms of the
! GNU General Public License. See the file `License'
! in the root directory of the present distribution,
! or http://www.gnu.org/copyleft/gpl.txt .
!
! From file pwcom.f90
!--------------------------------------------------------------------------
!
MODULE pseud_module
  !
  ! ... The variables describing pseudopotentials in analytical form
  !  
  USE kinds,      ONLY : DP => dbl
  USE parameters, ONLY : npsx
  !
  SAVE
  !
  REAL(KIND=DP) :: &
       cc(2,npsx),            &! the coefficients of the erf functions
       alpc(2,npsx),          &! the alpha of the erf functions
       zp(npsx),              &! the charge of the pseudopotential
       aps(6,0:3,npsx),       &! the a_l coefficient
       alps(3,0:3,npsx)        ! the b_l coefficient
  REAL(KIND=DP) :: &
       a_nlcc(npsx),         &! nonlinear core correction coefficients:
       b_nlcc(npsx),         &! rho_c(r) = (a_c + b_c*r^2) exp(-alpha_c*r^2)
       alpha_nlcc(npsx)       ! 
  INTEGER :: &
       nlc(npsx),             &! number of erf functions
       nnl(npsx),             &! number of the gaussian functions
       lmax(npsx),            &! maximum angular momentum of the pseudopot
       lloc(npsx)              ! angular momentum of the part taken as local
  !
END MODULE pseud_module
!
!
MODULE us_module
  !
  ! ... These parameters are needed with the US pseudopotentials
  !  
  USE kinds,      ONLY : DP => dbl
  !
  SAVE
  !
  INTEGER :: &
       nqxq,             &! size of interpolation table
       nqx                ! number of interpolation points
  REAL(KIND=DP), PARAMETER:: &
       dq = 0.01_DP       ! space between points in the pseudopotential tab.
  REAL(KIND=DP), ALLOCATABLE :: &
       qrad(:,:,:,:),         &! radial FT of Q functions
       tab(:,:,:),            &! interpolation table for PPs
       tab_at(:,:,:)           ! interpolation table for atomic wfc
  LOGICAL :: &
       okvan                   ! if .TRUE. at least one pseudo is Vanderbilt
  !
END MODULE us_module
!
!
MODULE spin_orb_module
  
  USE kinds, ONLY: DP => dbl
  USE parameters, ONLY : lmaxx
  
  SAVE

  LOGICAL :: &
      lspinorb, domag    ! if .TRUE. this is a spin-orbit calculation

  COMPLEX (kind=dp) :: rot_ylm(2*lmaxx+1,2*lmaxx+1)  ! transform real
                         ! spherical harmonics into complex ones
  COMPLEX (kind=dp), ALLOCATABLE :: fcoef(:,:,:,:,:) ! function needed to
                         ! account for spinors.
END MODULE spin_orb_module

