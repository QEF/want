!
! Copyright (C) 2009 WanT Group
! 
! This file is distributed under the terms of the
! GNU General Public License. See the file `License'
! in the root directory of the present distribution,
! or http://www.gnu.org/copyleft/gpl.txt .
!
!*********************************************************
   MODULE etsf_io_data_module
   !*********************************************************
   !
   USE kinds,     ONLY : dbl
   !
#ifdef __ETSF_IO
   USE etsf_io
#endif
   !
   IMPLICIT NONE
   SAVE
!
! This module contains data specific to ETSF_IO fmt
! (used eg by Abinit)
!
! The whole interface to ETSF_IO has been done with
! the contribution of Conor Hogan
!

   !
   ! general data used to readin
   !
   LOGICAL                      :: lstat
   INTEGER                      :: ncid
   !
#ifdef __ETSF_IO
   !
   TYPE(etsf_io_low_error)      :: error_data
   TYPE(etsf_groups_flags)      :: flags
   
   !
   ! dimensions, as defined by ETSF specifications
   !
   TYPE(etsf_dims)              :: dims

#endif

END MODULE etsf_io_data_module
