# 1 "iotk_dat_interf.spp"
! Input/Output Tool Kit (IOTK)
! Copyright (C) 2004,2005 Giovanni Bussi
!
! This library is free software; you can redistribute it and/or
! modify it under the terms of the GNU Lesser General Public
! License as published by the Free Software Foundation; either
! version 2.1 of the License, or (at your option) any later version.
!
! This library is distributed in the hope that it will be useful,
! but WITHOUT ANY WARRANTY; without even the implied warranty of
! MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
! Lesser General Public License for more details.
!
! You should have received a copy of the GNU Lesser General Public
! License along with this library; if not, write to the Free Software
! Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

!------------------------------------------------------------------------------!
! Inclusion of configuration file
#include "iotk_config.h"
!------------------------------------------------------------------------------!

# 2 "../include/iotk_auxmacros.spp"
#ifndef __IOTK_AUXMACROS
#define __IOTK_AUXMACROS

! The macros are defined with -D option or inside iotk_config.h
! The default values are set here
! Maximum rank of an array
#ifndef __IOTK_MAXRANK
#  define __IOTK_MAXRANK 7
#endif
! Minimum value used in iotk_free_unit
#ifndef __IOTK_UNITMIN
#  define __IOTK_UNITMIN 90000
#endif
! Maximum value used in iotk_free_unit
#ifndef __IOTK_UNITMAX
#  define __IOTK_UNITMAX 99999
#endif
! Unit for errors
#ifndef __IOTK_ERROR_UNIT
#  define __IOTK_ERROR_UNIT 0
#endif
! Kind for header in binary files
#ifndef __IOTK_HEADER_KIND
#  define __IOTK_HEADER_KIND selected_int_kind(8)
#endif
! Character (or eventually string) for newline
! It may be adjusted for particular systems
! Unix    achar(10)
! Mac-OS  achar(13)
! Windows ? (now it should be a single byte)
#ifndef __IOTK_NEWLINE
#  define __IOTK_NEWLINE achar(10)
#endif
! Character for EOS
#ifndef __IOTK_EOS
#  define __IOTK_EOS achar(0)
#endif
! These are the default kinds, which depend on the options used
! during the library compilation
! Only default characters are implemented
#define __IOTK_CHARACTER1 iotk_defkind_character
! For logical, integer and real types, the c precompiler
! looks for defined kinds. If no kind is found, the default
! is used as __IOTK_type1
# 50 "../include/iotk_auxmacros.spp"
#ifndef __IOTK_LOGICAL1
# 50 "../include/iotk_auxmacros.spp"
#ifndef __IOTK_LOGICAL2
# 50 "../include/iotk_auxmacros.spp"
#ifndef __IOTK_LOGICAL3
# 50 "../include/iotk_auxmacros.spp"
#ifndef __IOTK_LOGICAL4
# 52 "../include/iotk_auxmacros.spp"
#define __IOTK_LOGICAL1 iotk_defkind_LOGICAL
# 55 "../include/iotk_auxmacros.spp"
#endif
# 55 "../include/iotk_auxmacros.spp"
#endif
# 55 "../include/iotk_auxmacros.spp"
#endif
# 55 "../include/iotk_auxmacros.spp"
#endif
# 50 "../include/iotk_auxmacros.spp"
#ifndef __IOTK_INTEGER1
# 50 "../include/iotk_auxmacros.spp"
#ifndef __IOTK_INTEGER2
# 50 "../include/iotk_auxmacros.spp"
#ifndef __IOTK_INTEGER3
# 50 "../include/iotk_auxmacros.spp"
#ifndef __IOTK_INTEGER4
# 52 "../include/iotk_auxmacros.spp"
#define __IOTK_INTEGER1 iotk_defkind_INTEGER
# 55 "../include/iotk_auxmacros.spp"
#endif
# 55 "../include/iotk_auxmacros.spp"
#endif
# 55 "../include/iotk_auxmacros.spp"
#endif
# 55 "../include/iotk_auxmacros.spp"
#endif
# 50 "../include/iotk_auxmacros.spp"
#ifndef __IOTK_REAL1
# 50 "../include/iotk_auxmacros.spp"
#ifndef __IOTK_REAL2
# 50 "../include/iotk_auxmacros.spp"
#ifndef __IOTK_REAL3
# 50 "../include/iotk_auxmacros.spp"
#ifndef __IOTK_REAL4
# 52 "../include/iotk_auxmacros.spp"
#define __IOTK_REAL1 iotk_defkind_REAL
# 55 "../include/iotk_auxmacros.spp"
#endif
# 55 "../include/iotk_auxmacros.spp"
#endif
# 55 "../include/iotk_auxmacros.spp"
#endif
# 55 "../include/iotk_auxmacros.spp"
#endif
# 58 "../include/iotk_auxmacros.spp"

! Some useful check follow
#if __IOTK_MAXRANK > 7
#  error
#endif
#if __IOTK_MAXRANK < 1
#  error
#endif
# 68 "../include/iotk_auxmacros.spp"
#ifdef __IOTK_LOGICAL5
#  error
#endif
# 68 "../include/iotk_auxmacros.spp"
#ifdef __IOTK_LOGICAL6
#  error
#endif
# 68 "../include/iotk_auxmacros.spp"
#ifdef __IOTK_LOGICAL7
#  error
#endif
# 68 "../include/iotk_auxmacros.spp"
#ifdef __IOTK_LOGICAL8
#  error
#endif
# 68 "../include/iotk_auxmacros.spp"
#ifdef __IOTK_LOGICAL9
#  error
#endif
# 68 "../include/iotk_auxmacros.spp"
#ifdef __IOTK_LOGICAL10
#  error
#endif
# 68 "../include/iotk_auxmacros.spp"
#ifdef __IOTK_INTEGER5
#  error
#endif
# 68 "../include/iotk_auxmacros.spp"
#ifdef __IOTK_INTEGER6
#  error
#endif
# 68 "../include/iotk_auxmacros.spp"
#ifdef __IOTK_INTEGER7
#  error
#endif
# 68 "../include/iotk_auxmacros.spp"
#ifdef __IOTK_INTEGER8
#  error
#endif
# 68 "../include/iotk_auxmacros.spp"
#ifdef __IOTK_INTEGER9
#  error
#endif
# 68 "../include/iotk_auxmacros.spp"
#ifdef __IOTK_INTEGER10
#  error
#endif
# 68 "../include/iotk_auxmacros.spp"
#ifdef __IOTK_REAL5
#  error
#endif
# 68 "../include/iotk_auxmacros.spp"
#ifdef __IOTK_REAL6
#  error
#endif
# 68 "../include/iotk_auxmacros.spp"
#ifdef __IOTK_REAL7
#  error
#endif
# 68 "../include/iotk_auxmacros.spp"
#ifdef __IOTK_REAL8
#  error
#endif
# 68 "../include/iotk_auxmacros.spp"
#ifdef __IOTK_REAL9
#  error
#endif
# 68 "../include/iotk_auxmacros.spp"
#ifdef __IOTK_REAL10
#  error
#endif
# 73 "../include/iotk_auxmacros.spp"
#endif

! Complex are treated indentically to reals
! These lines map the definitions.
# 78 "../include/iotk_auxmacros.spp"
#ifdef __IOTK_REAL1
#  define __IOTK_COMPLEX1 __IOTK_REAL1
#else
#  undef __IOTK_COMPLEX1
#endif
# 78 "../include/iotk_auxmacros.spp"
#ifdef __IOTK_REAL2
#  define __IOTK_COMPLEX2 __IOTK_REAL2
#else
#  undef __IOTK_COMPLEX2
#endif
# 78 "../include/iotk_auxmacros.spp"
#ifdef __IOTK_REAL3
#  define __IOTK_COMPLEX3 __IOTK_REAL3
#else
#  undef __IOTK_COMPLEX3
#endif
# 78 "../include/iotk_auxmacros.spp"
#ifdef __IOTK_REAL4
#  define __IOTK_COMPLEX4 __IOTK_REAL4
#else
#  undef __IOTK_COMPLEX4
#endif
# 84 "../include/iotk_auxmacros.spp"


# 30 "iotk_dat_interf.spp"

module iotk_dat_interf
implicit none
private

public :: iotk_write_dat
public :: iotk_scan_dat
public :: iotk_scan_dat_aux


interface iotk_write_dat
# 44 "iotk_dat_interf.spp"
#ifdef __IOTK_LOGICAL1
# 46 "iotk_dat_interf.spp"
#if 0 <= __IOTK_MAXRANK
subroutine iotk_write_dat_LOGICAL1_0(unit,name,dat,dummy,fmt,ierr)
  use iotk_base
  implicit none
  integer,                intent(in)  :: unit
  character(*),           intent(in)  :: name
# 53 "iotk_dat_interf.spp"
  LOGICAL (kind=__IOTK_LOGICAL1),        intent(in)  :: dat 
  type(iotk_dummytype), optional      :: dummy
  character(*), optional, intent(in)  :: fmt
  integer,      optional, intent(out) :: ierr
end subroutine iotk_write_dat_LOGICAL1_0
#endif
# 46 "iotk_dat_interf.spp"
#if 1 <= __IOTK_MAXRANK
subroutine iotk_write_dat_LOGICAL1_1(unit,name,dat,dummy,fmt,ierr)
  use iotk_base
  implicit none
  integer,                intent(in)  :: unit
  character(*),           intent(in)  :: name
# 53 "iotk_dat_interf.spp"
  LOGICAL (kind=__IOTK_LOGICAL1),        intent(in)  :: dat (:)
  type(iotk_dummytype), optional      :: dummy
  character(*), optional, intent(in)  :: fmt
  integer,      optional, intent(out) :: ierr
end subroutine iotk_write_dat_LOGICAL1_1
#endif
# 46 "iotk_dat_interf.spp"
#if 2 <= __IOTK_MAXRANK
subroutine iotk_write_dat_LOGICAL1_2(unit,name,dat,dummy,fmt,ierr)
  use iotk_base
  implicit none
  integer,                intent(in)  :: unit
  character(*),           intent(in)  :: name
# 53 "iotk_dat_interf.spp"
  LOGICAL (kind=__IOTK_LOGICAL1),        intent(in)  :: dat (:,:)
  type(iotk_dummytype), optional      :: dummy
  character(*), optional, intent(in)  :: fmt
  integer,      optional, intent(out) :: ierr
end subroutine iotk_write_dat_LOGICAL1_2
#endif
# 46 "iotk_dat_interf.spp"
#if 3 <= __IOTK_MAXRANK
subroutine iotk_write_dat_LOGICAL1_3(unit,name,dat,dummy,fmt,ierr)
  use iotk_base
  implicit none
  integer,                intent(in)  :: unit
  character(*),           intent(in)  :: name
# 53 "iotk_dat_interf.spp"
  LOGICAL (kind=__IOTK_LOGICAL1),        intent(in)  :: dat (:,:,:)
  type(iotk_dummytype), optional      :: dummy
  character(*), optional, intent(in)  :: fmt
  integer,      optional, intent(out) :: ierr
end subroutine iotk_write_dat_LOGICAL1_3
#endif
# 46 "iotk_dat_interf.spp"
#if 4 <= __IOTK_MAXRANK
subroutine iotk_write_dat_LOGICAL1_4(unit,name,dat,dummy,fmt,ierr)
  use iotk_base
  implicit none
  integer,                intent(in)  :: unit
  character(*),           intent(in)  :: name
# 53 "iotk_dat_interf.spp"
  LOGICAL (kind=__IOTK_LOGICAL1),        intent(in)  :: dat (:,:,:,:)
  type(iotk_dummytype), optional      :: dummy
  character(*), optional, intent(in)  :: fmt
  integer,      optional, intent(out) :: ierr
end subroutine iotk_write_dat_LOGICAL1_4
#endif
# 46 "iotk_dat_interf.spp"
#if 5 <= __IOTK_MAXRANK
subroutine iotk_write_dat_LOGICAL1_5(unit,name,dat,dummy,fmt,ierr)
  use iotk_base
  implicit none
  integer,                intent(in)  :: unit
  character(*),           intent(in)  :: name
# 53 "iotk_dat_interf.spp"
  LOGICAL (kind=__IOTK_LOGICAL1),        intent(in)  :: dat (:,:,:,:,:)
  type(iotk_dummytype), optional      :: dummy
  character(*), optional, intent(in)  :: fmt
  integer,      optional, intent(out) :: ierr
end subroutine iotk_write_dat_LOGICAL1_5
#endif
# 46 "iotk_dat_interf.spp"
#if 6 <= __IOTK_MAXRANK
subroutine iotk_write_dat_LOGICAL1_6(unit,name,dat,dummy,fmt,ierr)
  use iotk_base
  implicit none
  integer,                intent(in)  :: unit
  character(*),           intent(in)  :: name
# 53 "iotk_dat_interf.spp"
  LOGICAL (kind=__IOTK_LOGICAL1),        intent(in)  :: dat (:,:,:,:,:,:)
  type(iotk_dummytype), optional      :: dummy
  character(*), optional, intent(in)  :: fmt
  integer,      optional, intent(out) :: ierr
end subroutine iotk_write_dat_LOGICAL1_6
#endif
# 46 "iotk_dat_interf.spp"
#if 7 <= __IOTK_MAXRANK
subroutine iotk_write_dat_LOGICAL1_7(unit,name,dat,dummy,fmt,ierr)
  use iotk_base
  implicit none
  integer,                intent(in)  :: unit
  character(*),           intent(in)  :: name
# 53 "iotk_dat_interf.spp"
  LOGICAL (kind=__IOTK_LOGICAL1),        intent(in)  :: dat (:,:,:,:,:,:,:)
  type(iotk_dummytype), optional      :: dummy
  character(*), optional, intent(in)  :: fmt
  integer,      optional, intent(out) :: ierr
end subroutine iotk_write_dat_LOGICAL1_7
#endif
# 60 "iotk_dat_interf.spp"
#endif
# 44 "iotk_dat_interf.spp"
#ifdef __IOTK_LOGICAL2
# 46 "iotk_dat_interf.spp"
#if 0 <= __IOTK_MAXRANK
subroutine iotk_write_dat_LOGICAL2_0(unit,name,dat,dummy,fmt,ierr)
  use iotk_base
  implicit none
  integer,                intent(in)  :: unit
  character(*),           intent(in)  :: name
# 53 "iotk_dat_interf.spp"
  LOGICAL (kind=__IOTK_LOGICAL2),        intent(in)  :: dat 
  type(iotk_dummytype), optional      :: dummy
  character(*), optional, intent(in)  :: fmt
  integer,      optional, intent(out) :: ierr
end subroutine iotk_write_dat_LOGICAL2_0
#endif
# 46 "iotk_dat_interf.spp"
#if 1 <= __IOTK_MAXRANK
subroutine iotk_write_dat_LOGICAL2_1(unit,name,dat,dummy,fmt,ierr)
  use iotk_base
  implicit none
  integer,                intent(in)  :: unit
  character(*),           intent(in)  :: name
# 53 "iotk_dat_interf.spp"
  LOGICAL (kind=__IOTK_LOGICAL2),        intent(in)  :: dat (:)
  type(iotk_dummytype), optional      :: dummy
  character(*), optional, intent(in)  :: fmt
  integer,      optional, intent(out) :: ierr
end subroutine iotk_write_dat_LOGICAL2_1
#endif
# 46 "iotk_dat_interf.spp"
#if 2 <= __IOTK_MAXRANK
subroutine iotk_write_dat_LOGICAL2_2(unit,name,dat,dummy,fmt,ierr)
  use iotk_base
  implicit none
  integer,                intent(in)  :: unit
  character(*),           intent(in)  :: name
# 53 "iotk_dat_interf.spp"
  LOGICAL (kind=__IOTK_LOGICAL2),        intent(in)  :: dat (:,:)
  type(iotk_dummytype), optional      :: dummy
  character(*), optional, intent(in)  :: fmt
  integer,      optional, intent(out) :: ierr
end subroutine iotk_write_dat_LOGICAL2_2
#endif
# 46 "iotk_dat_interf.spp"
#if 3 <= __IOTK_MAXRANK
subroutine iotk_write_dat_LOGICAL2_3(unit,name,dat,dummy,fmt,ierr)
  use iotk_base
  implicit none
  integer,                intent(in)  :: unit
  character(*),           intent(in)  :: name
# 53 "iotk_dat_interf.spp"
  LOGICAL (kind=__IOTK_LOGICAL2),        intent(in)  :: dat (:,:,:)
  type(iotk_dummytype), optional      :: dummy
  character(*), optional, intent(in)  :: fmt
  integer,      optional, intent(out) :: ierr
end subroutine iotk_write_dat_LOGICAL2_3
#endif
# 46 "iotk_dat_interf.spp"
#if 4 <= __IOTK_MAXRANK
subroutine iotk_write_dat_LOGICAL2_4(unit,name,dat,dummy,fmt,ierr)
  use iotk_base
  implicit none
  integer,                intent(in)  :: unit
  character(*),           intent(in)  :: name
# 53 "iotk_dat_interf.spp"
  LOGICAL (kind=__IOTK_LOGICAL2),        intent(in)  :: dat (:,:,:,:)
  type(iotk_dummytype), optional      :: dummy
  character(*), optional, intent(in)  :: fmt
  integer,      optional, intent(out) :: ierr
end subroutine iotk_write_dat_LOGICAL2_4
#endif
# 46 "iotk_dat_interf.spp"
#if 5 <= __IOTK_MAXRANK
subroutine iotk_write_dat_LOGICAL2_5(unit,name,dat,dummy,fmt,ierr)
  use iotk_base
  implicit none
  integer,                intent(in)  :: unit
  character(*),           intent(in)  :: name
# 53 "iotk_dat_interf.spp"
  LOGICAL (kind=__IOTK_LOGICAL2),        intent(in)  :: dat (:,:,:,:,:)
  type(iotk_dummytype), optional      :: dummy
  character(*), optional, intent(in)  :: fmt
  integer,      optional, intent(out) :: ierr
end subroutine iotk_write_dat_LOGICAL2_5
#endif
# 46 "iotk_dat_interf.spp"
#if 6 <= __IOTK_MAXRANK
subroutine iotk_write_dat_LOGICAL2_6(unit,name,dat,dummy,fmt,ierr)
  use iotk_base
  implicit none
  integer,                intent(in)  :: unit
  character(*),           intent(in)  :: name
# 53 "iotk_dat_interf.spp"
  LOGICAL (kind=__IOTK_LOGICAL2),        intent(in)  :: dat (:,:,:,:,:,:)
  type(iotk_dummytype), optional      :: dummy
  character(*), optional, intent(in)  :: fmt
  integer,      optional, intent(out) :: ierr
end subroutine iotk_write_dat_LOGICAL2_6
#endif
# 46 "iotk_dat_interf.spp"
#if 7 <= __IOTK_MAXRANK
subroutine iotk_write_dat_LOGICAL2_7(unit,name,dat,dummy,fmt,ierr)
  use iotk_base
  implicit none
  integer,                intent(in)  :: unit
  character(*),           intent(in)  :: name
# 53 "iotk_dat_interf.spp"
  LOGICAL (kind=__IOTK_LOGICAL2),        intent(in)  :: dat (:,:,:,:,:,:,:)
  type(iotk_dummytype), optional      :: dummy
  character(*), optional, intent(in)  :: fmt
  integer,      optional, intent(out) :: ierr
end subroutine iotk_write_dat_LOGICAL2_7
#endif
# 60 "iotk_dat_interf.spp"
#endif
# 44 "iotk_dat_interf.spp"
#ifdef __IOTK_LOGICAL3
# 46 "iotk_dat_interf.spp"
#if 0 <= __IOTK_MAXRANK
subroutine iotk_write_dat_LOGICAL3_0(unit,name,dat,dummy,fmt,ierr)
  use iotk_base
  implicit none
  integer,                intent(in)  :: unit
  character(*),           intent(in)  :: name
# 53 "iotk_dat_interf.spp"
  LOGICAL (kind=__IOTK_LOGICAL3),        intent(in)  :: dat 
  type(iotk_dummytype), optional      :: dummy
  character(*), optional, intent(in)  :: fmt
  integer,      optional, intent(out) :: ierr
end subroutine iotk_write_dat_LOGICAL3_0
#endif
# 46 "iotk_dat_interf.spp"
#if 1 <= __IOTK_MAXRANK
subroutine iotk_write_dat_LOGICAL3_1(unit,name,dat,dummy,fmt,ierr)
  use iotk_base
  implicit none
  integer,                intent(in)  :: unit
  character(*),           intent(in)  :: name
# 53 "iotk_dat_interf.spp"
  LOGICAL (kind=__IOTK_LOGICAL3),        intent(in)  :: dat (:)
  type(iotk_dummytype), optional      :: dummy
  character(*), optional, intent(in)  :: fmt
  integer,      optional, intent(out) :: ierr
end subroutine iotk_write_dat_LOGICAL3_1
#endif
# 46 "iotk_dat_interf.spp"
#if 2 <= __IOTK_MAXRANK
subroutine iotk_write_dat_LOGICAL3_2(unit,name,dat,dummy,fmt,ierr)
  use iotk_base
  implicit none
  integer,                intent(in)  :: unit
  character(*),           intent(in)  :: name
# 53 "iotk_dat_interf.spp"
  LOGICAL (kind=__IOTK_LOGICAL3),        intent(in)  :: dat (:,:)
  type(iotk_dummytype), optional      :: dummy
  character(*), optional, intent(in)  :: fmt
  integer,      optional, intent(out) :: ierr
end subroutine iotk_write_dat_LOGICAL3_2
#endif
# 46 "iotk_dat_interf.spp"
#if 3 <= __IOTK_MAXRANK
subroutine iotk_write_dat_LOGICAL3_3(unit,name,dat,dummy,fmt,ierr)
  use iotk_base
  implicit none
  integer,                intent(in)  :: unit
  character(*),           intent(in)  :: name
# 53 "iotk_dat_interf.spp"
  LOGICAL (kind=__IOTK_LOGICAL3),        intent(in)  :: dat (:,:,:)
  type(iotk_dummytype), optional      :: dummy
  character(*), optional, intent(in)  :: fmt
  integer,      optional, intent(out) :: ierr
end subroutine iotk_write_dat_LOGICAL3_3
#endif
# 46 "iotk_dat_interf.spp"
#if 4 <= __IOTK_MAXRANK
subroutine iotk_write_dat_LOGICAL3_4(unit,name,dat,dummy,fmt,ierr)
  use iotk_base
  implicit none
  integer,                intent(in)  :: unit
  character(*),           intent(in)  :: name
# 53 "iotk_dat_interf.spp"
  LOGICAL (kind=__IOTK_LOGICAL3),        intent(in)  :: dat (:,:,:,:)
  type(iotk_dummytype), optional      :: dummy
  character(*), optional, intent(in)  :: fmt
  integer,      optional, intent(out) :: ierr
end subroutine iotk_write_dat_LOGICAL3_4
#endif
# 46 "iotk_dat_interf.spp"
#if 5 <= __IOTK_MAXRANK
subroutine iotk_write_dat_LOGICAL3_5(unit,name,dat,dummy,fmt,ierr)
  use iotk_base
  implicit none
  integer,                intent(in)  :: unit
  character(*),           intent(in)  :: name
# 53 "iotk_dat_interf.spp"
  LOGICAL (kind=__IOTK_LOGICAL3),        intent(in)  :: dat (:,:,:,:,:)
  type(iotk_dummytype), optional      :: dummy
  character(*), optional, intent(in)  :: fmt
  integer,      optional, intent(out) :: ierr
end subroutine iotk_write_dat_LOGICAL3_5
#endif
# 46 "iotk_dat_interf.spp"
#if 6 <= __IOTK_MAXRANK
subroutine iotk_write_dat_LOGICAL3_6(unit,name,dat,dummy,fmt,ierr)
  use iotk_base
  implicit none
  integer,                intent(in)  :: unit
  character(*),           intent(in)  :: name
# 53 "iotk_dat_interf.spp"
  LOGICAL (kind=__IOTK_LOGICAL3),        intent(in)  :: dat (:,:,:,:,:,:)
  type(iotk_dummytype), optional      :: dummy
  character(*), optional, intent(in)  :: fmt
  integer,      optional, intent(out) :: ierr
end subroutine iotk_write_dat_LOGICAL3_6
#endif
# 46 "iotk_dat_interf.spp"
#if 7 <= __IOTK_MAXRANK
subroutine iotk_write_dat_LOGICAL3_7(unit,name,dat,dummy,fmt,ierr)
  use iotk_base
  implicit none
  integer,                intent(in)  :: unit
  character(*),           intent(in)  :: name
# 53 "iotk_dat_interf.spp"
  LOGICAL (kind=__IOTK_LOGICAL3),        intent(in)  :: dat (:,:,:,:,:,:,:)
  type(iotk_dummytype), optional      :: dummy
  character(*), optional, intent(in)  :: fmt
  integer,      optional, intent(out) :: ierr
end subroutine iotk_write_dat_LOGICAL3_7
#endif
# 60 "iotk_dat_interf.spp"
#endif
# 44 "iotk_dat_interf.spp"
#ifdef __IOTK_LOGICAL4
# 46 "iotk_dat_interf.spp"
#if 0 <= __IOTK_MAXRANK
subroutine iotk_write_dat_LOGICAL4_0(unit,name,dat,dummy,fmt,ierr)
  use iotk_base
  implicit none
  integer,                intent(in)  :: unit
  character(*),           intent(in)  :: name
# 53 "iotk_dat_interf.spp"
  LOGICAL (kind=__IOTK_LOGICAL4),        intent(in)  :: dat 
  type(iotk_dummytype), optional      :: dummy
  character(*), optional, intent(in)  :: fmt
  integer,      optional, intent(out) :: ierr
end subroutine iotk_write_dat_LOGICAL4_0
#endif
# 46 "iotk_dat_interf.spp"
#if 1 <= __IOTK_MAXRANK
subroutine iotk_write_dat_LOGICAL4_1(unit,name,dat,dummy,fmt,ierr)
  use iotk_base
  implicit none
  integer,                intent(in)  :: unit
  character(*),           intent(in)  :: name
# 53 "iotk_dat_interf.spp"
  LOGICAL (kind=__IOTK_LOGICAL4),        intent(in)  :: dat (:)
  type(iotk_dummytype), optional      :: dummy
  character(*), optional, intent(in)  :: fmt
  integer,      optional, intent(out) :: ierr
end subroutine iotk_write_dat_LOGICAL4_1
#endif
# 46 "iotk_dat_interf.spp"
#if 2 <= __IOTK_MAXRANK
subroutine iotk_write_dat_LOGICAL4_2(unit,name,dat,dummy,fmt,ierr)
  use iotk_base
  implicit none
  integer,                intent(in)  :: unit
  character(*),           intent(in)  :: name
# 53 "iotk_dat_interf.spp"
  LOGICAL (kind=__IOTK_LOGICAL4),        intent(in)  :: dat (:,:)
  type(iotk_dummytype), optional      :: dummy
  character(*), optional, intent(in)  :: fmt
  integer,      optional, intent(out) :: ierr
end subroutine iotk_write_dat_LOGICAL4_2
#endif
# 46 "iotk_dat_interf.spp"
#if 3 <= __IOTK_MAXRANK
subroutine iotk_write_dat_LOGICAL4_3(unit,name,dat,dummy,fmt,ierr)
  use iotk_base
  implicit none
  integer,                intent(in)  :: unit
  character(*),           intent(in)  :: name
# 53 "iotk_dat_interf.spp"
  LOGICAL (kind=__IOTK_LOGICAL4),        intent(in)  :: dat (:,:,:)
  type(iotk_dummytype), optional      :: dummy
  character(*), optional, intent(in)  :: fmt
  integer,      optional, intent(out) :: ierr
end subroutine iotk_write_dat_LOGICAL4_3
#endif
# 46 "iotk_dat_interf.spp"
#if 4 <= __IOTK_MAXRANK
subroutine iotk_write_dat_LOGICAL4_4(unit,name,dat,dummy,fmt,ierr)
  use iotk_base
  implicit none
  integer,                intent(in)  :: unit
  character(*),           intent(in)  :: name
# 53 "iotk_dat_interf.spp"
  LOGICAL (kind=__IOTK_LOGICAL4),        intent(in)  :: dat (:,:,:,:)
  type(iotk_dummytype), optional      :: dummy
  character(*), optional, intent(in)  :: fmt
  integer,      optional, intent(out) :: ierr
end subroutine iotk_write_dat_LOGICAL4_4
#endif
# 46 "iotk_dat_interf.spp"
#if 5 <= __IOTK_MAXRANK
subroutine iotk_write_dat_LOGICAL4_5(unit,name,dat,dummy,fmt,ierr)
  use iotk_base
  implicit none
  integer,                intent(in)  :: unit
  character(*),           intent(in)  :: name
# 53 "iotk_dat_interf.spp"
  LOGICAL (kind=__IOTK_LOGICAL4),        intent(in)  :: dat (:,:,:,:,:)
  type(iotk_dummytype), optional      :: dummy
  character(*), optional, intent(in)  :: fmt
  integer,      optional, intent(out) :: ierr
end subroutine iotk_write_dat_LOGICAL4_5
#endif
# 46 "iotk_dat_interf.spp"
#if 6 <= __IOTK_MAXRANK
subroutine iotk_write_dat_LOGICAL4_6(unit,name,dat,dummy,fmt,ierr)
  use iotk_base
  implicit none
  integer,                intent(in)  :: unit
  character(*),           intent(in)  :: name
# 53 "iotk_dat_interf.spp"
  LOGICAL (kind=__IOTK_LOGICAL4),        intent(in)  :: dat (:,:,:,:,:,:)
  type(iotk_dummytype), optional      :: dummy
  character(*), optional, intent(in)  :: fmt
  integer,      optional, intent(out) :: ierr
end subroutine iotk_write_dat_LOGICAL4_6
#endif
# 46 "iotk_dat_interf.spp"
#if 7 <= __IOTK_MAXRANK
subroutine iotk_write_dat_LOGICAL4_7(unit,name,dat,dummy,fmt,ierr)
  use iotk_base
  implicit none
  integer,                intent(in)  :: unit
  character(*),           intent(in)  :: name
# 53 "iotk_dat_interf.spp"
  LOGICAL (kind=__IOTK_LOGICAL4),        intent(in)  :: dat (:,:,:,:,:,:,:)
  type(iotk_dummytype), optional      :: dummy
  character(*), optional, intent(in)  :: fmt
  integer,      optional, intent(out) :: ierr
end subroutine iotk_write_dat_LOGICAL4_7
#endif
# 60 "iotk_dat_interf.spp"
#endif
# 44 "iotk_dat_interf.spp"
#ifdef __IOTK_INTEGER1
# 46 "iotk_dat_interf.spp"
#if 0 <= __IOTK_MAXRANK
subroutine iotk_write_dat_INTEGER1_0(unit,name,dat,dummy,fmt,ierr)
  use iotk_base
  implicit none
  integer,                intent(in)  :: unit
  character(*),           intent(in)  :: name
# 53 "iotk_dat_interf.spp"
  INTEGER (kind=__IOTK_INTEGER1),        intent(in)  :: dat 
  type(iotk_dummytype), optional      :: dummy
  character(*), optional, intent(in)  :: fmt
  integer,      optional, intent(out) :: ierr
end subroutine iotk_write_dat_INTEGER1_0
#endif
# 46 "iotk_dat_interf.spp"
#if 1 <= __IOTK_MAXRANK
subroutine iotk_write_dat_INTEGER1_1(unit,name,dat,dummy,fmt,ierr)
  use iotk_base
  implicit none
  integer,                intent(in)  :: unit
  character(*),           intent(in)  :: name
# 53 "iotk_dat_interf.spp"
  INTEGER (kind=__IOTK_INTEGER1),        intent(in)  :: dat (:)
  type(iotk_dummytype), optional      :: dummy
  character(*), optional, intent(in)  :: fmt
  integer,      optional, intent(out) :: ierr
end subroutine iotk_write_dat_INTEGER1_1
#endif
# 46 "iotk_dat_interf.spp"
#if 2 <= __IOTK_MAXRANK
subroutine iotk_write_dat_INTEGER1_2(unit,name,dat,dummy,fmt,ierr)
  use iotk_base
  implicit none
  integer,                intent(in)  :: unit
  character(*),           intent(in)  :: name
# 53 "iotk_dat_interf.spp"
  INTEGER (kind=__IOTK_INTEGER1),        intent(in)  :: dat (:,:)
  type(iotk_dummytype), optional      :: dummy
  character(*), optional, intent(in)  :: fmt
  integer,      optional, intent(out) :: ierr
end subroutine iotk_write_dat_INTEGER1_2
#endif
# 46 "iotk_dat_interf.spp"
#if 3 <= __IOTK_MAXRANK
subroutine iotk_write_dat_INTEGER1_3(unit,name,dat,dummy,fmt,ierr)
  use iotk_base
  implicit none
  integer,                intent(in)  :: unit
  character(*),           intent(in)  :: name
# 53 "iotk_dat_interf.spp"
  INTEGER (kind=__IOTK_INTEGER1),        intent(in)  :: dat (:,:,:)
  type(iotk_dummytype), optional      :: dummy
  character(*), optional, intent(in)  :: fmt
  integer,      optional, intent(out) :: ierr
end subroutine iotk_write_dat_INTEGER1_3
#endif
# 46 "iotk_dat_interf.spp"
#if 4 <= __IOTK_MAXRANK
subroutine iotk_write_dat_INTEGER1_4(unit,name,dat,dummy,fmt,ierr)
  use iotk_base
  implicit none
  integer,                intent(in)  :: unit
  character(*),           intent(in)  :: name
# 53 "iotk_dat_interf.spp"
  INTEGER (kind=__IOTK_INTEGER1),        intent(in)  :: dat (:,:,:,:)
  type(iotk_dummytype), optional      :: dummy
  character(*), optional, intent(in)  :: fmt
  integer,      optional, intent(out) :: ierr
end subroutine iotk_write_dat_INTEGER1_4
#endif
# 46 "iotk_dat_interf.spp"
#if 5 <= __IOTK_MAXRANK
subroutine iotk_write_dat_INTEGER1_5(unit,name,dat,dummy,fmt,ierr)
  use iotk_base
  implicit none
  integer,                intent(in)  :: unit
  character(*),           intent(in)  :: name
# 53 "iotk_dat_interf.spp"
  INTEGER (kind=__IOTK_INTEGER1),        intent(in)  :: dat (:,:,:,:,:)
  type(iotk_dummytype), optional      :: dummy
  character(*), optional, intent(in)  :: fmt
  integer,      optional, intent(out) :: ierr
end subroutine iotk_write_dat_INTEGER1_5
#endif
# 46 "iotk_dat_interf.spp"
#if 6 <= __IOTK_MAXRANK
subroutine iotk_write_dat_INTEGER1_6(unit,name,dat,dummy,fmt,ierr)
  use iotk_base
  implicit none
  integer,                intent(in)  :: unit
  character(*),           intent(in)  :: name
# 53 "iotk_dat_interf.spp"
  INTEGER (kind=__IOTK_INTEGER1),        intent(in)  :: dat (:,:,:,:,:,:)
  type(iotk_dummytype), optional      :: dummy
  character(*), optional, intent(in)  :: fmt
  integer,      optional, intent(out) :: ierr
end subroutine iotk_write_dat_INTEGER1_6
#endif
# 46 "iotk_dat_interf.spp"
#if 7 <= __IOTK_MAXRANK
subroutine iotk_write_dat_INTEGER1_7(unit,name,dat,dummy,fmt,ierr)
  use iotk_base
  implicit none
  integer,                intent(in)  :: unit
  character(*),           intent(in)  :: name
# 53 "iotk_dat_interf.spp"
  INTEGER (kind=__IOTK_INTEGER1),        intent(in)  :: dat (:,:,:,:,:,:,:)
  type(iotk_dummytype), optional      :: dummy
  character(*), optional, intent(in)  :: fmt
  integer,      optional, intent(out) :: ierr
end subroutine iotk_write_dat_INTEGER1_7
#endif
# 60 "iotk_dat_interf.spp"
#endif
# 44 "iotk_dat_interf.spp"
#ifdef __IOTK_INTEGER2
# 46 "iotk_dat_interf.spp"
#if 0 <= __IOTK_MAXRANK
subroutine iotk_write_dat_INTEGER2_0(unit,name,dat,dummy,fmt,ierr)
  use iotk_base
  implicit none
  integer,                intent(in)  :: unit
  character(*),           intent(in)  :: name
# 53 "iotk_dat_interf.spp"
  INTEGER (kind=__IOTK_INTEGER2),        intent(in)  :: dat 
  type(iotk_dummytype), optional      :: dummy
  character(*), optional, intent(in)  :: fmt
  integer,      optional, intent(out) :: ierr
end subroutine iotk_write_dat_INTEGER2_0
#endif
# 46 "iotk_dat_interf.spp"
#if 1 <= __IOTK_MAXRANK
subroutine iotk_write_dat_INTEGER2_1(unit,name,dat,dummy,fmt,ierr)
  use iotk_base
  implicit none
  integer,                intent(in)  :: unit
  character(*),           intent(in)  :: name
# 53 "iotk_dat_interf.spp"
  INTEGER (kind=__IOTK_INTEGER2),        intent(in)  :: dat (:)
  type(iotk_dummytype), optional      :: dummy
  character(*), optional, intent(in)  :: fmt
  integer,      optional, intent(out) :: ierr
end subroutine iotk_write_dat_INTEGER2_1
#endif
# 46 "iotk_dat_interf.spp"
#if 2 <= __IOTK_MAXRANK
subroutine iotk_write_dat_INTEGER2_2(unit,name,dat,dummy,fmt,ierr)
  use iotk_base
  implicit none
  integer,                intent(in)  :: unit
  character(*),           intent(in)  :: name
# 53 "iotk_dat_interf.spp"
  INTEGER (kind=__IOTK_INTEGER2),        intent(in)  :: dat (:,:)
  type(iotk_dummytype), optional      :: dummy
  character(*), optional, intent(in)  :: fmt
  integer,      optional, intent(out) :: ierr
end subroutine iotk_write_dat_INTEGER2_2
#endif
# 46 "iotk_dat_interf.spp"
#if 3 <= __IOTK_MAXRANK
subroutine iotk_write_dat_INTEGER2_3(unit,name,dat,dummy,fmt,ierr)
  use iotk_base
  implicit none
  integer,                intent(in)  :: unit
  character(*),           intent(in)  :: name
# 53 "iotk_dat_interf.spp"
  INTEGER (kind=__IOTK_INTEGER2),        intent(in)  :: dat (:,:,:)
  type(iotk_dummytype), optional      :: dummy
  character(*), optional, intent(in)  :: fmt
  integer,      optional, intent(out) :: ierr
end subroutine iotk_write_dat_INTEGER2_3
#endif
# 46 "iotk_dat_interf.spp"
#if 4 <= __IOTK_MAXRANK
subroutine iotk_write_dat_INTEGER2_4(unit,name,dat,dummy,fmt,ierr)
  use iotk_base
  implicit none
  integer,                intent(in)  :: unit
  character(*),           intent(in)  :: name
# 53 "iotk_dat_interf.spp"
  INTEGER (kind=__IOTK_INTEGER2),        intent(in)  :: dat (:,:,:,:)
  type(iotk_dummytype), optional      :: dummy
  character(*), optional, intent(in)  :: fmt
  integer,      optional, intent(out) :: ierr
end subroutine iotk_write_dat_INTEGER2_4
#endif
# 46 "iotk_dat_interf.spp"
#if 5 <= __IOTK_MAXRANK
subroutine iotk_write_dat_INTEGER2_5(unit,name,dat,dummy,fmt,ierr)
  use iotk_base
  implicit none
  integer,                intent(in)  :: unit
  character(*),           intent(in)  :: name
# 53 "iotk_dat_interf.spp"
  INTEGER (kind=__IOTK_INTEGER2),        intent(in)  :: dat (:,:,:,:,:)
  type(iotk_dummytype), optional      :: dummy
  character(*), optional, intent(in)  :: fmt
  integer,      optional, intent(out) :: ierr
end subroutine iotk_write_dat_INTEGER2_5
#endif
# 46 "iotk_dat_interf.spp"
#if 6 <= __IOTK_MAXRANK
subroutine iotk_write_dat_INTEGER2_6(unit,name,dat,dummy,fmt,ierr)
  use iotk_base
  implicit none
  integer,                intent(in)  :: unit
  character(*),           intent(in)  :: name
# 53 "iotk_dat_interf.spp"
  INTEGER (kind=__IOTK_INTEGER2),        intent(in)  :: dat (:,:,:,:,:,:)
  type(iotk_dummytype), optional      :: dummy
  character(*), optional, intent(in)  :: fmt
  integer,      optional, intent(out) :: ierr
end subroutine iotk_write_dat_INTEGER2_6
#endif
# 46 "iotk_dat_interf.spp"
#if 7 <= __IOTK_MAXRANK
subroutine iotk_write_dat_INTEGER2_7(unit,name,dat,dummy,fmt,ierr)
  use iotk_base
  implicit none
  integer,                intent(in)  :: unit
  character(*),           intent(in)  :: name
# 53 "iotk_dat_interf.spp"
  INTEGER (kind=__IOTK_INTEGER2),        intent(in)  :: dat (:,:,:,:,:,:,:)
  type(iotk_dummytype), optional      :: dummy
  character(*), optional, intent(in)  :: fmt
  integer,      optional, intent(out) :: ierr
end subroutine iotk_write_dat_INTEGER2_7
#endif
# 60 "iotk_dat_interf.spp"
#endif
# 44 "iotk_dat_interf.spp"
#ifdef __IOTK_INTEGER3
# 46 "iotk_dat_interf.spp"
#if 0 <= __IOTK_MAXRANK
subroutine iotk_write_dat_INTEGER3_0(unit,name,dat,dummy,fmt,ierr)
  use iotk_base
  implicit none
  integer,                intent(in)  :: unit
  character(*),           intent(in)  :: name
# 53 "iotk_dat_interf.spp"
  INTEGER (kind=__IOTK_INTEGER3),        intent(in)  :: dat 
  type(iotk_dummytype), optional      :: dummy
  character(*), optional, intent(in)  :: fmt
  integer,      optional, intent(out) :: ierr
end subroutine iotk_write_dat_INTEGER3_0
#endif
# 46 "iotk_dat_interf.spp"
#if 1 <= __IOTK_MAXRANK
subroutine iotk_write_dat_INTEGER3_1(unit,name,dat,dummy,fmt,ierr)
  use iotk_base
  implicit none
  integer,                intent(in)  :: unit
  character(*),           intent(in)  :: name
# 53 "iotk_dat_interf.spp"
  INTEGER (kind=__IOTK_INTEGER3),        intent(in)  :: dat (:)
  type(iotk_dummytype), optional      :: dummy
  character(*), optional, intent(in)  :: fmt
  integer,      optional, intent(out) :: ierr
end subroutine iotk_write_dat_INTEGER3_1
#endif
# 46 "iotk_dat_interf.spp"
#if 2 <= __IOTK_MAXRANK
subroutine iotk_write_dat_INTEGER3_2(unit,name,dat,dummy,fmt,ierr)
  use iotk_base
  implicit none
  integer,                intent(in)  :: unit
  character(*),           intent(in)  :: name
# 53 "iotk_dat_interf.spp"
  INTEGER (kind=__IOTK_INTEGER3),        intent(in)  :: dat (:,:)
  type(iotk_dummytype), optional      :: dummy
  character(*), optional, intent(in)  :: fmt
  integer,      optional, intent(out) :: ierr
end subroutine iotk_write_dat_INTEGER3_2
#endif
# 46 "iotk_dat_interf.spp"
#if 3 <= __IOTK_MAXRANK
subroutine iotk_write_dat_INTEGER3_3(unit,name,dat,dummy,fmt,ierr)
  use iotk_base
  implicit none
  integer,                intent(in)  :: unit
  character(*),           intent(in)  :: name
# 53 "iotk_dat_interf.spp"
  INTEGER (kind=__IOTK_INTEGER3),        intent(in)  :: dat (:,:,:)
  type(iotk_dummytype), optional      :: dummy
  character(*), optional, intent(in)  :: fmt
  integer,      optional, intent(out) :: ierr
end subroutine iotk_write_dat_INTEGER3_3
#endif
# 46 "iotk_dat_interf.spp"
#if 4 <= __IOTK_MAXRANK
subroutine iotk_write_dat_INTEGER3_4(unit,name,dat,dummy,fmt,ierr)
  use iotk_base
  implicit none
  integer,                intent(in)  :: unit
  character(*),           intent(in)  :: name
# 53 "iotk_dat_interf.spp"
  INTEGER (kind=__IOTK_INTEGER3),        intent(in)  :: dat (:,:,:,:)
  type(iotk_dummytype), optional      :: dummy
  character(*), optional, intent(in)  :: fmt
  integer,      optional, intent(out) :: ierr
end subroutine iotk_write_dat_INTEGER3_4
#endif
# 46 "iotk_dat_interf.spp"
#if 5 <= __IOTK_MAXRANK
subroutine iotk_write_dat_INTEGER3_5(unit,name,dat,dummy,fmt,ierr)
  use iotk_base
  implicit none
  integer,                intent(in)  :: unit
  character(*),           intent(in)  :: name
# 53 "iotk_dat_interf.spp"
  INTEGER (kind=__IOTK_INTEGER3),        intent(in)  :: dat (:,:,:,:,:)
  type(iotk_dummytype), optional      :: dummy
  character(*), optional, intent(in)  :: fmt
  integer,      optional, intent(out) :: ierr
end subroutine iotk_write_dat_INTEGER3_5
#endif
# 46 "iotk_dat_interf.spp"
#if 6 <= __IOTK_MAXRANK
subroutine iotk_write_dat_INTEGER3_6(unit,name,dat,dummy,fmt,ierr)
  use iotk_base
  implicit none
  integer,                intent(in)  :: unit
  character(*),           intent(in)  :: name
# 53 "iotk_dat_interf.spp"
  INTEGER (kind=__IOTK_INTEGER3),        intent(in)  :: dat (:,:,:,:,:,:)
  type(iotk_dummytype), optional      :: dummy
  character(*), optional, intent(in)  :: fmt
  integer,      optional, intent(out) :: ierr
end subroutine iotk_write_dat_INTEGER3_6
#endif
# 46 "iotk_dat_interf.spp"
#if 7 <= __IOTK_MAXRANK
subroutine iotk_write_dat_INTEGER3_7(unit,name,dat,dummy,fmt,ierr)
  use iotk_base
  implicit none
  integer,                intent(in)  :: unit
  character(*),           intent(in)  :: name
# 53 "iotk_dat_interf.spp"
  INTEGER (kind=__IOTK_INTEGER3),        intent(in)  :: dat (:,:,:,:,:,:,:)
  type(iotk_dummytype), optional      :: dummy
  character(*), optional, intent(in)  :: fmt
  integer,      optional, intent(out) :: ierr
end subroutine iotk_write_dat_INTEGER3_7
#endif
# 60 "iotk_dat_interf.spp"
#endif
# 44 "iotk_dat_interf.spp"
#ifdef __IOTK_INTEGER4
# 46 "iotk_dat_interf.spp"
#if 0 <= __IOTK_MAXRANK
subroutine iotk_write_dat_INTEGER4_0(unit,name,dat,dummy,fmt,ierr)
  use iotk_base
  implicit none
  integer,                intent(in)  :: unit
  character(*),           intent(in)  :: name
# 53 "iotk_dat_interf.spp"
  INTEGER (kind=__IOTK_INTEGER4),        intent(in)  :: dat 
  type(iotk_dummytype), optional      :: dummy
  character(*), optional, intent(in)  :: fmt
  integer,      optional, intent(out) :: ierr
end subroutine iotk_write_dat_INTEGER4_0
#endif
# 46 "iotk_dat_interf.spp"
#if 1 <= __IOTK_MAXRANK
subroutine iotk_write_dat_INTEGER4_1(unit,name,dat,dummy,fmt,ierr)
  use iotk_base
  implicit none
  integer,                intent(in)  :: unit
  character(*),           intent(in)  :: name
# 53 "iotk_dat_interf.spp"
  INTEGER (kind=__IOTK_INTEGER4),        intent(in)  :: dat (:)
  type(iotk_dummytype), optional      :: dummy
  character(*), optional, intent(in)  :: fmt
  integer,      optional, intent(out) :: ierr
end subroutine iotk_write_dat_INTEGER4_1
#endif
# 46 "iotk_dat_interf.spp"
#if 2 <= __IOTK_MAXRANK
subroutine iotk_write_dat_INTEGER4_2(unit,name,dat,dummy,fmt,ierr)
  use iotk_base
  implicit none
  integer,                intent(in)  :: unit
  character(*),           intent(in)  :: name
# 53 "iotk_dat_interf.spp"
  INTEGER (kind=__IOTK_INTEGER4),        intent(in)  :: dat (:,:)
  type(iotk_dummytype), optional      :: dummy
  character(*), optional, intent(in)  :: fmt
  integer,      optional, intent(out) :: ierr
end subroutine iotk_write_dat_INTEGER4_2
#endif
# 46 "iotk_dat_interf.spp"
#if 3 <= __IOTK_MAXRANK
subroutine iotk_write_dat_INTEGER4_3(unit,name,dat,dummy,fmt,ierr)
  use iotk_base
  implicit none
  integer,                intent(in)  :: unit
  character(*),           intent(in)  :: name
# 53 "iotk_dat_interf.spp"
  INTEGER (kind=__IOTK_INTEGER4),        intent(in)  :: dat (:,:,:)
  type(iotk_dummytype), optional      :: dummy
  character(*), optional, intent(in)  :: fmt
  integer,      optional, intent(out) :: ierr
end subroutine iotk_write_dat_INTEGER4_3
#endif
# 46 "iotk_dat_interf.spp"
#if 4 <= __IOTK_MAXRANK
subroutine iotk_write_dat_INTEGER4_4(unit,name,dat,dummy,fmt,ierr)
  use iotk_base
  implicit none
  integer,                intent(in)  :: unit
  character(*),           intent(in)  :: name
# 53 "iotk_dat_interf.spp"
  INTEGER (kind=__IOTK_INTEGER4),        intent(in)  :: dat (:,:,:,:)
  type(iotk_dummytype), optional      :: dummy
  character(*), optional, intent(in)  :: fmt
  integer,      optional, intent(out) :: ierr
end subroutine iotk_write_dat_INTEGER4_4
#endif
# 46 "iotk_dat_interf.spp"
#if 5 <= __IOTK_MAXRANK
subroutine iotk_write_dat_INTEGER4_5(unit,name,dat,dummy,fmt,ierr)
  use iotk_base
  implicit none
  integer,                intent(in)  :: unit
  character(*),           intent(in)  :: name
# 53 "iotk_dat_interf.spp"
  INTEGER (kind=__IOTK_INTEGER4),        intent(in)  :: dat (:,:,:,:,:)
  type(iotk_dummytype), optional      :: dummy
  character(*), optional, intent(in)  :: fmt
  integer,      optional, intent(out) :: ierr
end subroutine iotk_write_dat_INTEGER4_5
#endif
# 46 "iotk_dat_interf.spp"
#if 6 <= __IOTK_MAXRANK
subroutine iotk_write_dat_INTEGER4_6(unit,name,dat,dummy,fmt,ierr)
  use iotk_base
  implicit none
  integer,                intent(in)  :: unit
  character(*),           intent(in)  :: name
# 53 "iotk_dat_interf.spp"
  INTEGER (kind=__IOTK_INTEGER4),        intent(in)  :: dat (:,:,:,:,:,:)
  type(iotk_dummytype), optional      :: dummy
  character(*), optional, intent(in)  :: fmt
  integer,      optional, intent(out) :: ierr
end subroutine iotk_write_dat_INTEGER4_6
#endif
# 46 "iotk_dat_interf.spp"
#if 7 <= __IOTK_MAXRANK
subroutine iotk_write_dat_INTEGER4_7(unit,name,dat,dummy,fmt,ierr)
  use iotk_base
  implicit none
  integer,                intent(in)  :: unit
  character(*),           intent(in)  :: name
# 53 "iotk_dat_interf.spp"
  INTEGER (kind=__IOTK_INTEGER4),        intent(in)  :: dat (:,:,:,:,:,:,:)
  type(iotk_dummytype), optional      :: dummy
  character(*), optional, intent(in)  :: fmt
  integer,      optional, intent(out) :: ierr
end subroutine iotk_write_dat_INTEGER4_7
#endif
# 60 "iotk_dat_interf.spp"
#endif
# 44 "iotk_dat_interf.spp"
#ifdef __IOTK_REAL1
# 46 "iotk_dat_interf.spp"
#if 0 <= __IOTK_MAXRANK
subroutine iotk_write_dat_REAL1_0(unit,name,dat,dummy,fmt,ierr)
  use iotk_base
  implicit none
  integer,                intent(in)  :: unit
  character(*),           intent(in)  :: name
# 53 "iotk_dat_interf.spp"
  REAL (kind=__IOTK_REAL1),        intent(in)  :: dat 
  type(iotk_dummytype), optional      :: dummy
  character(*), optional, intent(in)  :: fmt
  integer,      optional, intent(out) :: ierr
end subroutine iotk_write_dat_REAL1_0
#endif
# 46 "iotk_dat_interf.spp"
#if 1 <= __IOTK_MAXRANK
subroutine iotk_write_dat_REAL1_1(unit,name,dat,dummy,fmt,ierr)
  use iotk_base
  implicit none
  integer,                intent(in)  :: unit
  character(*),           intent(in)  :: name
# 53 "iotk_dat_interf.spp"
  REAL (kind=__IOTK_REAL1),        intent(in)  :: dat (:)
  type(iotk_dummytype), optional      :: dummy
  character(*), optional, intent(in)  :: fmt
  integer,      optional, intent(out) :: ierr
end subroutine iotk_write_dat_REAL1_1
#endif
# 46 "iotk_dat_interf.spp"
#if 2 <= __IOTK_MAXRANK
subroutine iotk_write_dat_REAL1_2(unit,name,dat,dummy,fmt,ierr)
  use iotk_base
  implicit none
  integer,                intent(in)  :: unit
  character(*),           intent(in)  :: name
# 53 "iotk_dat_interf.spp"
  REAL (kind=__IOTK_REAL1),        intent(in)  :: dat (:,:)
  type(iotk_dummytype), optional      :: dummy
  character(*), optional, intent(in)  :: fmt
  integer,      optional, intent(out) :: ierr
end subroutine iotk_write_dat_REAL1_2
#endif
# 46 "iotk_dat_interf.spp"
#if 3 <= __IOTK_MAXRANK
subroutine iotk_write_dat_REAL1_3(unit,name,dat,dummy,fmt,ierr)
  use iotk_base
  implicit none
  integer,                intent(in)  :: unit
  character(*),           intent(in)  :: name
# 53 "iotk_dat_interf.spp"
  REAL (kind=__IOTK_REAL1),        intent(in)  :: dat (:,:,:)
  type(iotk_dummytype), optional      :: dummy
  character(*), optional, intent(in)  :: fmt
  integer,      optional, intent(out) :: ierr
end subroutine iotk_write_dat_REAL1_3
#endif
# 46 "iotk_dat_interf.spp"
#if 4 <= __IOTK_MAXRANK
subroutine iotk_write_dat_REAL1_4(unit,name,dat,dummy,fmt,ierr)
  use iotk_base
  implicit none
  integer,                intent(in)  :: unit
  character(*),           intent(in)  :: name
# 53 "iotk_dat_interf.spp"
  REAL (kind=__IOTK_REAL1),        intent(in)  :: dat (:,:,:,:)
  type(iotk_dummytype), optional      :: dummy
  character(*), optional, intent(in)  :: fmt
  integer,      optional, intent(out) :: ierr
end subroutine iotk_write_dat_REAL1_4
#endif
# 46 "iotk_dat_interf.spp"
#if 5 <= __IOTK_MAXRANK
subroutine iotk_write_dat_REAL1_5(unit,name,dat,dummy,fmt,ierr)
  use iotk_base
  implicit none
  integer,                intent(in)  :: unit
  character(*),           intent(in)  :: name
# 53 "iotk_dat_interf.spp"
  REAL (kind=__IOTK_REAL1),        intent(in)  :: dat (:,:,:,:,:)
  type(iotk_dummytype), optional      :: dummy
  character(*), optional, intent(in)  :: fmt
  integer,      optional, intent(out) :: ierr
end subroutine iotk_write_dat_REAL1_5
#endif
# 46 "iotk_dat_interf.spp"
#if 6 <= __IOTK_MAXRANK
subroutine iotk_write_dat_REAL1_6(unit,name,dat,dummy,fmt,ierr)
  use iotk_base
  implicit none
  integer,                intent(in)  :: unit
  character(*),           intent(in)  :: name
# 53 "iotk_dat_interf.spp"
  REAL (kind=__IOTK_REAL1),        intent(in)  :: dat (:,:,:,:,:,:)
  type(iotk_dummytype), optional      :: dummy
  character(*), optional, intent(in)  :: fmt
  integer,      optional, intent(out) :: ierr
end subroutine iotk_write_dat_REAL1_6
#endif
# 46 "iotk_dat_interf.spp"
#if 7 <= __IOTK_MAXRANK
subroutine iotk_write_dat_REAL1_7(unit,name,dat,dummy,fmt,ierr)
  use iotk_base
  implicit none
  integer,                intent(in)  :: unit
  character(*),           intent(in)  :: name
# 53 "iotk_dat_interf.spp"
  REAL (kind=__IOTK_REAL1),        intent(in)  :: dat (:,:,:,:,:,:,:)
  type(iotk_dummytype), optional      :: dummy
  character(*), optional, intent(in)  :: fmt
  integer,      optional, intent(out) :: ierr
end subroutine iotk_write_dat_REAL1_7
#endif
# 60 "iotk_dat_interf.spp"
#endif
# 44 "iotk_dat_interf.spp"
#ifdef __IOTK_REAL2
# 46 "iotk_dat_interf.spp"
#if 0 <= __IOTK_MAXRANK
subroutine iotk_write_dat_REAL2_0(unit,name,dat,dummy,fmt,ierr)
  use iotk_base
  implicit none
  integer,                intent(in)  :: unit
  character(*),           intent(in)  :: name
# 53 "iotk_dat_interf.spp"
  REAL (kind=__IOTK_REAL2),        intent(in)  :: dat 
  type(iotk_dummytype), optional      :: dummy
  character(*), optional, intent(in)  :: fmt
  integer,      optional, intent(out) :: ierr
end subroutine iotk_write_dat_REAL2_0
#endif
# 46 "iotk_dat_interf.spp"
#if 1 <= __IOTK_MAXRANK
subroutine iotk_write_dat_REAL2_1(unit,name,dat,dummy,fmt,ierr)
  use iotk_base
  implicit none
  integer,                intent(in)  :: unit
  character(*),           intent(in)  :: name
# 53 "iotk_dat_interf.spp"
  REAL (kind=__IOTK_REAL2),        intent(in)  :: dat (:)
  type(iotk_dummytype), optional      :: dummy
  character(*), optional, intent(in)  :: fmt
  integer,      optional, intent(out) :: ierr
end subroutine iotk_write_dat_REAL2_1
#endif
# 46 "iotk_dat_interf.spp"
#if 2 <= __IOTK_MAXRANK
subroutine iotk_write_dat_REAL2_2(unit,name,dat,dummy,fmt,ierr)
  use iotk_base
  implicit none
  integer,                intent(in)  :: unit
  character(*),           intent(in)  :: name
# 53 "iotk_dat_interf.spp"
  REAL (kind=__IOTK_REAL2),        intent(in)  :: dat (:,:)
  type(iotk_dummytype), optional      :: dummy
  character(*), optional, intent(in)  :: fmt
  integer,      optional, intent(out) :: ierr
end subroutine iotk_write_dat_REAL2_2
#endif
# 46 "iotk_dat_interf.spp"
#if 3 <= __IOTK_MAXRANK
subroutine iotk_write_dat_REAL2_3(unit,name,dat,dummy,fmt,ierr)
  use iotk_base
  implicit none
  integer,                intent(in)  :: unit
  character(*),           intent(in)  :: name
# 53 "iotk_dat_interf.spp"
  REAL (kind=__IOTK_REAL2),        intent(in)  :: dat (:,:,:)
  type(iotk_dummytype), optional      :: dummy
  character(*), optional, intent(in)  :: fmt
  integer,      optional, intent(out) :: ierr
end subroutine iotk_write_dat_REAL2_3
#endif
# 46 "iotk_dat_interf.spp"
#if 4 <= __IOTK_MAXRANK
subroutine iotk_write_dat_REAL2_4(unit,name,dat,dummy,fmt,ierr)
  use iotk_base
  implicit none
  integer,                intent(in)  :: unit
  character(*),           intent(in)  :: name
# 53 "iotk_dat_interf.spp"
  REAL (kind=__IOTK_REAL2),        intent(in)  :: dat (:,:,:,:)
  type(iotk_dummytype), optional      :: dummy
  character(*), optional, intent(in)  :: fmt
  integer,      optional, intent(out) :: ierr
end subroutine iotk_write_dat_REAL2_4
#endif
# 46 "iotk_dat_interf.spp"
#if 5 <= __IOTK_MAXRANK
subroutine iotk_write_dat_REAL2_5(unit,name,dat,dummy,fmt,ierr)
  use iotk_base
  implicit none
  integer,                intent(in)  :: unit
  character(*),           intent(in)  :: name
# 53 "iotk_dat_interf.spp"
  REAL (kind=__IOTK_REAL2),        intent(in)  :: dat (:,:,:,:,:)
  type(iotk_dummytype), optional      :: dummy
  character(*), optional, intent(in)  :: fmt
  integer,      optional, intent(out) :: ierr
end subroutine iotk_write_dat_REAL2_5
#endif
# 46 "iotk_dat_interf.spp"
#if 6 <= __IOTK_MAXRANK
subroutine iotk_write_dat_REAL2_6(unit,name,dat,dummy,fmt,ierr)
  use iotk_base
  implicit none
  integer,                intent(in)  :: unit
  character(*),           intent(in)  :: name
# 53 "iotk_dat_interf.spp"
  REAL (kind=__IOTK_REAL2),        intent(in)  :: dat (:,:,:,:,:,:)
  type(iotk_dummytype), optional      :: dummy
  character(*), optional, intent(in)  :: fmt
  integer,      optional, intent(out) :: ierr
end subroutine iotk_write_dat_REAL2_6
#endif
# 46 "iotk_dat_interf.spp"
#if 7 <= __IOTK_MAXRANK
subroutine iotk_write_dat_REAL2_7(unit,name,dat,dummy,fmt,ierr)
  use iotk_base
  implicit none
  integer,                intent(in)  :: unit
  character(*),           intent(in)  :: name
# 53 "iotk_dat_interf.spp"
  REAL (kind=__IOTK_REAL2),        intent(in)  :: dat (:,:,:,:,:,:,:)
  type(iotk_dummytype), optional      :: dummy
  character(*), optional, intent(in)  :: fmt
  integer,      optional, intent(out) :: ierr
end subroutine iotk_write_dat_REAL2_7
#endif
# 60 "iotk_dat_interf.spp"
#endif
# 44 "iotk_dat_interf.spp"
#ifdef __IOTK_REAL3
# 46 "iotk_dat_interf.spp"
#if 0 <= __IOTK_MAXRANK
subroutine iotk_write_dat_REAL3_0(unit,name,dat,dummy,fmt,ierr)
  use iotk_base
  implicit none
  integer,                intent(in)  :: unit
  character(*),           intent(in)  :: name
# 53 "iotk_dat_interf.spp"
  REAL (kind=__IOTK_REAL3),        intent(in)  :: dat 
  type(iotk_dummytype), optional      :: dummy
  character(*), optional, intent(in)  :: fmt
  integer,      optional, intent(out) :: ierr
end subroutine iotk_write_dat_REAL3_0
#endif
# 46 "iotk_dat_interf.spp"
#if 1 <= __IOTK_MAXRANK
subroutine iotk_write_dat_REAL3_1(unit,name,dat,dummy,fmt,ierr)
  use iotk_base
  implicit none
  integer,                intent(in)  :: unit
  character(*),           intent(in)  :: name
# 53 "iotk_dat_interf.spp"
  REAL (kind=__IOTK_REAL3),        intent(in)  :: dat (:)
  type(iotk_dummytype), optional      :: dummy
  character(*), optional, intent(in)  :: fmt
  integer,      optional, intent(out) :: ierr
end subroutine iotk_write_dat_REAL3_1
#endif
# 46 "iotk_dat_interf.spp"
#if 2 <= __IOTK_MAXRANK
subroutine iotk_write_dat_REAL3_2(unit,name,dat,dummy,fmt,ierr)
  use iotk_base
  implicit none
  integer,                intent(in)  :: unit
  character(*),           intent(in)  :: name
# 53 "iotk_dat_interf.spp"
  REAL (kind=__IOTK_REAL3),        intent(in)  :: dat (:,:)
  type(iotk_dummytype), optional      :: dummy
  character(*), optional, intent(in)  :: fmt
  integer,      optional, intent(out) :: ierr
end subroutine iotk_write_dat_REAL3_2
#endif
# 46 "iotk_dat_interf.spp"
#if 3 <= __IOTK_MAXRANK
subroutine iotk_write_dat_REAL3_3(unit,name,dat,dummy,fmt,ierr)
  use iotk_base
  implicit none
  integer,                intent(in)  :: unit
  character(*),           intent(in)  :: name
# 53 "iotk_dat_interf.spp"
  REAL (kind=__IOTK_REAL3),        intent(in)  :: dat (:,:,:)
  type(iotk_dummytype), optional      :: dummy
  character(*), optional, intent(in)  :: fmt
  integer,      optional, intent(out) :: ierr
end subroutine iotk_write_dat_REAL3_3
#endif
# 46 "iotk_dat_interf.spp"
#if 4 <= __IOTK_MAXRANK
subroutine iotk_write_dat_REAL3_4(unit,name,dat,dummy,fmt,ierr)
  use iotk_base
  implicit none
  integer,                intent(in)  :: unit
  character(*),           intent(in)  :: name
# 53 "iotk_dat_interf.spp"
  REAL (kind=__IOTK_REAL3),        intent(in)  :: dat (:,:,:,:)
  type(iotk_dummytype), optional      :: dummy
  character(*), optional, intent(in)  :: fmt
  integer,      optional, intent(out) :: ierr
end subroutine iotk_write_dat_REAL3_4
#endif
# 46 "iotk_dat_interf.spp"
#if 5 <= __IOTK_MAXRANK
subroutine iotk_write_dat_REAL3_5(unit,name,dat,dummy,fmt,ierr)
  use iotk_base
  implicit none
  integer,                intent(in)  :: unit
  character(*),           intent(in)  :: name
# 53 "iotk_dat_interf.spp"
  REAL (kind=__IOTK_REAL3),        intent(in)  :: dat (:,:,:,:,:)
  type(iotk_dummytype), optional      :: dummy
  character(*), optional, intent(in)  :: fmt
  integer,      optional, intent(out) :: ierr
end subroutine iotk_write_dat_REAL3_5
#endif
# 46 "iotk_dat_interf.spp"
#if 6 <= __IOTK_MAXRANK
subroutine iotk_write_dat_REAL3_6(unit,name,dat,dummy,fmt,ierr)
  use iotk_base
  implicit none
  integer,                intent(in)  :: unit
  character(*),           intent(in)  :: name
# 53 "iotk_dat_interf.spp"
  REAL (kind=__IOTK_REAL3),        intent(in)  :: dat (:,:,:,:,:,:)
  type(iotk_dummytype), optional      :: dummy
  character(*), optional, intent(in)  :: fmt
  integer,      optional, intent(out) :: ierr
end subroutine iotk_write_dat_REAL3_6
#endif
# 46 "iotk_dat_interf.spp"
#if 7 <= __IOTK_MAXRANK
subroutine iotk_write_dat_REAL3_7(unit,name,dat,dummy,fmt,ierr)
  use iotk_base
  implicit none
  integer,                intent(in)  :: unit
  character(*),           intent(in)  :: name
# 53 "iotk_dat_interf.spp"
  REAL (kind=__IOTK_REAL3),        intent(in)  :: dat (:,:,:,:,:,:,:)
  type(iotk_dummytype), optional      :: dummy
  character(*), optional, intent(in)  :: fmt
  integer,      optional, intent(out) :: ierr
end subroutine iotk_write_dat_REAL3_7
#endif
# 60 "iotk_dat_interf.spp"
#endif
# 44 "iotk_dat_interf.spp"
#ifdef __IOTK_REAL4
# 46 "iotk_dat_interf.spp"
#if 0 <= __IOTK_MAXRANK
subroutine iotk_write_dat_REAL4_0(unit,name,dat,dummy,fmt,ierr)
  use iotk_base
  implicit none
  integer,                intent(in)  :: unit
  character(*),           intent(in)  :: name
# 53 "iotk_dat_interf.spp"
  REAL (kind=__IOTK_REAL4),        intent(in)  :: dat 
  type(iotk_dummytype), optional      :: dummy
  character(*), optional, intent(in)  :: fmt
  integer,      optional, intent(out) :: ierr
end subroutine iotk_write_dat_REAL4_0
#endif
# 46 "iotk_dat_interf.spp"
#if 1 <= __IOTK_MAXRANK
subroutine iotk_write_dat_REAL4_1(unit,name,dat,dummy,fmt,ierr)
  use iotk_base
  implicit none
  integer,                intent(in)  :: unit
  character(*),           intent(in)  :: name
# 53 "iotk_dat_interf.spp"
  REAL (kind=__IOTK_REAL4),        intent(in)  :: dat (:)
  type(iotk_dummytype), optional      :: dummy
  character(*), optional, intent(in)  :: fmt
  integer,      optional, intent(out) :: ierr
end subroutine iotk_write_dat_REAL4_1
#endif
# 46 "iotk_dat_interf.spp"
#if 2 <= __IOTK_MAXRANK
subroutine iotk_write_dat_REAL4_2(unit,name,dat,dummy,fmt,ierr)
  use iotk_base
  implicit none
  integer,                intent(in)  :: unit
  character(*),           intent(in)  :: name
# 53 "iotk_dat_interf.spp"
  REAL (kind=__IOTK_REAL4),        intent(in)  :: dat (:,:)
  type(iotk_dummytype), optional      :: dummy
  character(*), optional, intent(in)  :: fmt
  integer,      optional, intent(out) :: ierr
end subroutine iotk_write_dat_REAL4_2
#endif
# 46 "iotk_dat_interf.spp"
#if 3 <= __IOTK_MAXRANK
subroutine iotk_write_dat_REAL4_3(unit,name,dat,dummy,fmt,ierr)
  use iotk_base
  implicit none
  integer,                intent(in)  :: unit
  character(*),           intent(in)  :: name
# 53 "iotk_dat_interf.spp"
  REAL (kind=__IOTK_REAL4),        intent(in)  :: dat (:,:,:)
  type(iotk_dummytype), optional      :: dummy
  character(*), optional, intent(in)  :: fmt
  integer,      optional, intent(out) :: ierr
end subroutine iotk_write_dat_REAL4_3
#endif
# 46 "iotk_dat_interf.spp"
#if 4 <= __IOTK_MAXRANK
subroutine iotk_write_dat_REAL4_4(unit,name,dat,dummy,fmt,ierr)
  use iotk_base
  implicit none
  integer,                intent(in)  :: unit
  character(*),           intent(in)  :: name
# 53 "iotk_dat_interf.spp"
  REAL (kind=__IOTK_REAL4),        intent(in)  :: dat (:,:,:,:)
  type(iotk_dummytype), optional      :: dummy
  character(*), optional, intent(in)  :: fmt
  integer,      optional, intent(out) :: ierr
end subroutine iotk_write_dat_REAL4_4
#endif
# 46 "iotk_dat_interf.spp"
#if 5 <= __IOTK_MAXRANK
subroutine iotk_write_dat_REAL4_5(unit,name,dat,dummy,fmt,ierr)
  use iotk_base
  implicit none
  integer,                intent(in)  :: unit
  character(*),           intent(in)  :: name
# 53 "iotk_dat_interf.spp"
  REAL (kind=__IOTK_REAL4),        intent(in)  :: dat (:,:,:,:,:)
  type(iotk_dummytype), optional      :: dummy
  character(*), optional, intent(in)  :: fmt
  integer,      optional, intent(out) :: ierr
end subroutine iotk_write_dat_REAL4_5
#endif
# 46 "iotk_dat_interf.spp"
#if 6 <= __IOTK_MAXRANK
subroutine iotk_write_dat_REAL4_6(unit,name,dat,dummy,fmt,ierr)
  use iotk_base
  implicit none
  integer,                intent(in)  :: unit
  character(*),           intent(in)  :: name
# 53 "iotk_dat_interf.spp"
  REAL (kind=__IOTK_REAL4),        intent(in)  :: dat (:,:,:,:,:,:)
  type(iotk_dummytype), optional      :: dummy
  character(*), optional, intent(in)  :: fmt
  integer,      optional, intent(out) :: ierr
end subroutine iotk_write_dat_REAL4_6
#endif
# 46 "iotk_dat_interf.spp"
#if 7 <= __IOTK_MAXRANK
subroutine iotk_write_dat_REAL4_7(unit,name,dat,dummy,fmt,ierr)
  use iotk_base
  implicit none
  integer,                intent(in)  :: unit
  character(*),           intent(in)  :: name
# 53 "iotk_dat_interf.spp"
  REAL (kind=__IOTK_REAL4),        intent(in)  :: dat (:,:,:,:,:,:,:)
  type(iotk_dummytype), optional      :: dummy
  character(*), optional, intent(in)  :: fmt
  integer,      optional, intent(out) :: ierr
end subroutine iotk_write_dat_REAL4_7
#endif
# 60 "iotk_dat_interf.spp"
#endif
# 44 "iotk_dat_interf.spp"
#ifdef __IOTK_COMPLEX1
# 46 "iotk_dat_interf.spp"
#if 0 <= __IOTK_MAXRANK
subroutine iotk_write_dat_COMPLEX1_0(unit,name,dat,dummy,fmt,ierr)
  use iotk_base
  implicit none
  integer,                intent(in)  :: unit
  character(*),           intent(in)  :: name
# 53 "iotk_dat_interf.spp"
  COMPLEX (kind=__IOTK_COMPLEX1),        intent(in)  :: dat 
  type(iotk_dummytype), optional      :: dummy
  character(*), optional, intent(in)  :: fmt
  integer,      optional, intent(out) :: ierr
end subroutine iotk_write_dat_COMPLEX1_0
#endif
# 46 "iotk_dat_interf.spp"
#if 1 <= __IOTK_MAXRANK
subroutine iotk_write_dat_COMPLEX1_1(unit,name,dat,dummy,fmt,ierr)
  use iotk_base
  implicit none
  integer,                intent(in)  :: unit
  character(*),           intent(in)  :: name
# 53 "iotk_dat_interf.spp"
  COMPLEX (kind=__IOTK_COMPLEX1),        intent(in)  :: dat (:)
  type(iotk_dummytype), optional      :: dummy
  character(*), optional, intent(in)  :: fmt
  integer,      optional, intent(out) :: ierr
end subroutine iotk_write_dat_COMPLEX1_1
#endif
# 46 "iotk_dat_interf.spp"
#if 2 <= __IOTK_MAXRANK
subroutine iotk_write_dat_COMPLEX1_2(unit,name,dat,dummy,fmt,ierr)
  use iotk_base
  implicit none
  integer,                intent(in)  :: unit
  character(*),           intent(in)  :: name
# 53 "iotk_dat_interf.spp"
  COMPLEX (kind=__IOTK_COMPLEX1),        intent(in)  :: dat (:,:)
  type(iotk_dummytype), optional      :: dummy
  character(*), optional, intent(in)  :: fmt
  integer,      optional, intent(out) :: ierr
end subroutine iotk_write_dat_COMPLEX1_2
#endif
# 46 "iotk_dat_interf.spp"
#if 3 <= __IOTK_MAXRANK
subroutine iotk_write_dat_COMPLEX1_3(unit,name,dat,dummy,fmt,ierr)
  use iotk_base
  implicit none
  integer,                intent(in)  :: unit
  character(*),           intent(in)  :: name
# 53 "iotk_dat_interf.spp"
  COMPLEX (kind=__IOTK_COMPLEX1),        intent(in)  :: dat (:,:,:)
  type(iotk_dummytype), optional      :: dummy
  character(*), optional, intent(in)  :: fmt
  integer,      optional, intent(out) :: ierr
end subroutine iotk_write_dat_COMPLEX1_3
#endif
# 46 "iotk_dat_interf.spp"
#if 4 <= __IOTK_MAXRANK
subroutine iotk_write_dat_COMPLEX1_4(unit,name,dat,dummy,fmt,ierr)
  use iotk_base
  implicit none
  integer,                intent(in)  :: unit
  character(*),           intent(in)  :: name
# 53 "iotk_dat_interf.spp"
  COMPLEX (kind=__IOTK_COMPLEX1),        intent(in)  :: dat (:,:,:,:)
  type(iotk_dummytype), optional      :: dummy
  character(*), optional, intent(in)  :: fmt
  integer,      optional, intent(out) :: ierr
end subroutine iotk_write_dat_COMPLEX1_4
#endif
# 46 "iotk_dat_interf.spp"
#if 5 <= __IOTK_MAXRANK
subroutine iotk_write_dat_COMPLEX1_5(unit,name,dat,dummy,fmt,ierr)
  use iotk_base
  implicit none
  integer,                intent(in)  :: unit
  character(*),           intent(in)  :: name
# 53 "iotk_dat_interf.spp"
  COMPLEX (kind=__IOTK_COMPLEX1),        intent(in)  :: dat (:,:,:,:,:)
  type(iotk_dummytype), optional      :: dummy
  character(*), optional, intent(in)  :: fmt
  integer,      optional, intent(out) :: ierr
end subroutine iotk_write_dat_COMPLEX1_5
#endif
# 46 "iotk_dat_interf.spp"
#if 6 <= __IOTK_MAXRANK
subroutine iotk_write_dat_COMPLEX1_6(unit,name,dat,dummy,fmt,ierr)
  use iotk_base
  implicit none
  integer,                intent(in)  :: unit
  character(*),           intent(in)  :: name
# 53 "iotk_dat_interf.spp"
  COMPLEX (kind=__IOTK_COMPLEX1),        intent(in)  :: dat (:,:,:,:,:,:)
  type(iotk_dummytype), optional      :: dummy
  character(*), optional, intent(in)  :: fmt
  integer,      optional, intent(out) :: ierr
end subroutine iotk_write_dat_COMPLEX1_6
#endif
# 46 "iotk_dat_interf.spp"
#if 7 <= __IOTK_MAXRANK
subroutine iotk_write_dat_COMPLEX1_7(unit,name,dat,dummy,fmt,ierr)
  use iotk_base
  implicit none
  integer,                intent(in)  :: unit
  character(*),           intent(in)  :: name
# 53 "iotk_dat_interf.spp"
  COMPLEX (kind=__IOTK_COMPLEX1),        intent(in)  :: dat (:,:,:,:,:,:,:)
  type(iotk_dummytype), optional      :: dummy
  character(*), optional, intent(in)  :: fmt
  integer,      optional, intent(out) :: ierr
end subroutine iotk_write_dat_COMPLEX1_7
#endif
# 60 "iotk_dat_interf.spp"
#endif
# 44 "iotk_dat_interf.spp"
#ifdef __IOTK_COMPLEX2
# 46 "iotk_dat_interf.spp"
#if 0 <= __IOTK_MAXRANK
subroutine iotk_write_dat_COMPLEX2_0(unit,name,dat,dummy,fmt,ierr)
  use iotk_base
  implicit none
  integer,                intent(in)  :: unit
  character(*),           intent(in)  :: name
# 53 "iotk_dat_interf.spp"
  COMPLEX (kind=__IOTK_COMPLEX2),        intent(in)  :: dat 
  type(iotk_dummytype), optional      :: dummy
  character(*), optional, intent(in)  :: fmt
  integer,      optional, intent(out) :: ierr
end subroutine iotk_write_dat_COMPLEX2_0
#endif
# 46 "iotk_dat_interf.spp"
#if 1 <= __IOTK_MAXRANK
subroutine iotk_write_dat_COMPLEX2_1(unit,name,dat,dummy,fmt,ierr)
  use iotk_base
  implicit none
  integer,                intent(in)  :: unit
  character(*),           intent(in)  :: name
# 53 "iotk_dat_interf.spp"
  COMPLEX (kind=__IOTK_COMPLEX2),        intent(in)  :: dat (:)
  type(iotk_dummytype), optional      :: dummy
  character(*), optional, intent(in)  :: fmt
  integer,      optional, intent(out) :: ierr
end subroutine iotk_write_dat_COMPLEX2_1
#endif
# 46 "iotk_dat_interf.spp"
#if 2 <= __IOTK_MAXRANK
subroutine iotk_write_dat_COMPLEX2_2(unit,name,dat,dummy,fmt,ierr)
  use iotk_base
  implicit none
  integer,                intent(in)  :: unit
  character(*),           intent(in)  :: name
# 53 "iotk_dat_interf.spp"
  COMPLEX (kind=__IOTK_COMPLEX2),        intent(in)  :: dat (:,:)
  type(iotk_dummytype), optional      :: dummy
  character(*), optional, intent(in)  :: fmt
  integer,      optional, intent(out) :: ierr
end subroutine iotk_write_dat_COMPLEX2_2
#endif
# 46 "iotk_dat_interf.spp"
#if 3 <= __IOTK_MAXRANK
subroutine iotk_write_dat_COMPLEX2_3(unit,name,dat,dummy,fmt,ierr)
  use iotk_base
  implicit none
  integer,                intent(in)  :: unit
  character(*),           intent(in)  :: name
# 53 "iotk_dat_interf.spp"
  COMPLEX (kind=__IOTK_COMPLEX2),        intent(in)  :: dat (:,:,:)
  type(iotk_dummytype), optional      :: dummy
  character(*), optional, intent(in)  :: fmt
  integer,      optional, intent(out) :: ierr
end subroutine iotk_write_dat_COMPLEX2_3
#endif
# 46 "iotk_dat_interf.spp"
#if 4 <= __IOTK_MAXRANK
subroutine iotk_write_dat_COMPLEX2_4(unit,name,dat,dummy,fmt,ierr)
  use iotk_base
  implicit none
  integer,                intent(in)  :: unit
  character(*),           intent(in)  :: name
# 53 "iotk_dat_interf.spp"
  COMPLEX (kind=__IOTK_COMPLEX2),        intent(in)  :: dat (:,:,:,:)
  type(iotk_dummytype), optional      :: dummy
  character(*), optional, intent(in)  :: fmt
  integer,      optional, intent(out) :: ierr
end subroutine iotk_write_dat_COMPLEX2_4
#endif
# 46 "iotk_dat_interf.spp"
#if 5 <= __IOTK_MAXRANK
subroutine iotk_write_dat_COMPLEX2_5(unit,name,dat,dummy,fmt,ierr)
  use iotk_base
  implicit none
  integer,                intent(in)  :: unit
  character(*),           intent(in)  :: name
# 53 "iotk_dat_interf.spp"
  COMPLEX (kind=__IOTK_COMPLEX2),        intent(in)  :: dat (:,:,:,:,:)
  type(iotk_dummytype), optional      :: dummy
  character(*), optional, intent(in)  :: fmt
  integer,      optional, intent(out) :: ierr
end subroutine iotk_write_dat_COMPLEX2_5
#endif
# 46 "iotk_dat_interf.spp"
#if 6 <= __IOTK_MAXRANK
subroutine iotk_write_dat_COMPLEX2_6(unit,name,dat,dummy,fmt,ierr)
  use iotk_base
  implicit none
  integer,                intent(in)  :: unit
  character(*),           intent(in)  :: name
# 53 "iotk_dat_interf.spp"
  COMPLEX (kind=__IOTK_COMPLEX2),        intent(in)  :: dat (:,:,:,:,:,:)
  type(iotk_dummytype), optional      :: dummy
  character(*), optional, intent(in)  :: fmt
  integer,      optional, intent(out) :: ierr
end subroutine iotk_write_dat_COMPLEX2_6
#endif
# 46 "iotk_dat_interf.spp"
#if 7 <= __IOTK_MAXRANK
subroutine iotk_write_dat_COMPLEX2_7(unit,name,dat,dummy,fmt,ierr)
  use iotk_base
  implicit none
  integer,                intent(in)  :: unit
  character(*),           intent(in)  :: name
# 53 "iotk_dat_interf.spp"
  COMPLEX (kind=__IOTK_COMPLEX2),        intent(in)  :: dat (:,:,:,:,:,:,:)
  type(iotk_dummytype), optional      :: dummy
  character(*), optional, intent(in)  :: fmt
  integer,      optional, intent(out) :: ierr
end subroutine iotk_write_dat_COMPLEX2_7
#endif
# 60 "iotk_dat_interf.spp"
#endif
# 44 "iotk_dat_interf.spp"
#ifdef __IOTK_COMPLEX3
# 46 "iotk_dat_interf.spp"
#if 0 <= __IOTK_MAXRANK
subroutine iotk_write_dat_COMPLEX3_0(unit,name,dat,dummy,fmt,ierr)
  use iotk_base
  implicit none
  integer,                intent(in)  :: unit
  character(*),           intent(in)  :: name
# 53 "iotk_dat_interf.spp"
  COMPLEX (kind=__IOTK_COMPLEX3),        intent(in)  :: dat 
  type(iotk_dummytype), optional      :: dummy
  character(*), optional, intent(in)  :: fmt
  integer,      optional, intent(out) :: ierr
end subroutine iotk_write_dat_COMPLEX3_0
#endif
# 46 "iotk_dat_interf.spp"
#if 1 <= __IOTK_MAXRANK
subroutine iotk_write_dat_COMPLEX3_1(unit,name,dat,dummy,fmt,ierr)
  use iotk_base
  implicit none
  integer,                intent(in)  :: unit
  character(*),           intent(in)  :: name
# 53 "iotk_dat_interf.spp"
  COMPLEX (kind=__IOTK_COMPLEX3),        intent(in)  :: dat (:)
  type(iotk_dummytype), optional      :: dummy
  character(*), optional, intent(in)  :: fmt
  integer,      optional, intent(out) :: ierr
end subroutine iotk_write_dat_COMPLEX3_1
#endif
# 46 "iotk_dat_interf.spp"
#if 2 <= __IOTK_MAXRANK
subroutine iotk_write_dat_COMPLEX3_2(unit,name,dat,dummy,fmt,ierr)
  use iotk_base
  implicit none
  integer,                intent(in)  :: unit
  character(*),           intent(in)  :: name
# 53 "iotk_dat_interf.spp"
  COMPLEX (kind=__IOTK_COMPLEX3),        intent(in)  :: dat (:,:)
  type(iotk_dummytype), optional      :: dummy
  character(*), optional, intent(in)  :: fmt
  integer,      optional, intent(out) :: ierr
end subroutine iotk_write_dat_COMPLEX3_2
#endif
# 46 "iotk_dat_interf.spp"
#if 3 <= __IOTK_MAXRANK
subroutine iotk_write_dat_COMPLEX3_3(unit,name,dat,dummy,fmt,ierr)
  use iotk_base
  implicit none
  integer,                intent(in)  :: unit
  character(*),           intent(in)  :: name
# 53 "iotk_dat_interf.spp"
  COMPLEX (kind=__IOTK_COMPLEX3),        intent(in)  :: dat (:,:,:)
  type(iotk_dummytype), optional      :: dummy
  character(*), optional, intent(in)  :: fmt
  integer,      optional, intent(out) :: ierr
end subroutine iotk_write_dat_COMPLEX3_3
#endif
# 46 "iotk_dat_interf.spp"
#if 4 <= __IOTK_MAXRANK
subroutine iotk_write_dat_COMPLEX3_4(unit,name,dat,dummy,fmt,ierr)
  use iotk_base
  implicit none
  integer,                intent(in)  :: unit
  character(*),           intent(in)  :: name
# 53 "iotk_dat_interf.spp"
  COMPLEX (kind=__IOTK_COMPLEX3),        intent(in)  :: dat (:,:,:,:)
  type(iotk_dummytype), optional      :: dummy
  character(*), optional, intent(in)  :: fmt
  integer,      optional, intent(out) :: ierr
end subroutine iotk_write_dat_COMPLEX3_4
#endif
# 46 "iotk_dat_interf.spp"
#if 5 <= __IOTK_MAXRANK
subroutine iotk_write_dat_COMPLEX3_5(unit,name,dat,dummy,fmt,ierr)
  use iotk_base
  implicit none
  integer,                intent(in)  :: unit
  character(*),           intent(in)  :: name
# 53 "iotk_dat_interf.spp"
  COMPLEX (kind=__IOTK_COMPLEX3),        intent(in)  :: dat (:,:,:,:,:)
  type(iotk_dummytype), optional      :: dummy
  character(*), optional, intent(in)  :: fmt
  integer,      optional, intent(out) :: ierr
end subroutine iotk_write_dat_COMPLEX3_5
#endif
# 46 "iotk_dat_interf.spp"
#if 6 <= __IOTK_MAXRANK
subroutine iotk_write_dat_COMPLEX3_6(unit,name,dat,dummy,fmt,ierr)
  use iotk_base
  implicit none
  integer,                intent(in)  :: unit
  character(*),           intent(in)  :: name
# 53 "iotk_dat_interf.spp"
  COMPLEX (kind=__IOTK_COMPLEX3),        intent(in)  :: dat (:,:,:,:,:,:)
  type(iotk_dummytype), optional      :: dummy
  character(*), optional, intent(in)  :: fmt
  integer,      optional, intent(out) :: ierr
end subroutine iotk_write_dat_COMPLEX3_6
#endif
# 46 "iotk_dat_interf.spp"
#if 7 <= __IOTK_MAXRANK
subroutine iotk_write_dat_COMPLEX3_7(unit,name,dat,dummy,fmt,ierr)
  use iotk_base
  implicit none
  integer,                intent(in)  :: unit
  character(*),           intent(in)  :: name
# 53 "iotk_dat_interf.spp"
  COMPLEX (kind=__IOTK_COMPLEX3),        intent(in)  :: dat (:,:,:,:,:,:,:)
  type(iotk_dummytype), optional      :: dummy
  character(*), optional, intent(in)  :: fmt
  integer,      optional, intent(out) :: ierr
end subroutine iotk_write_dat_COMPLEX3_7
#endif
# 60 "iotk_dat_interf.spp"
#endif
# 44 "iotk_dat_interf.spp"
#ifdef __IOTK_COMPLEX4
# 46 "iotk_dat_interf.spp"
#if 0 <= __IOTK_MAXRANK
subroutine iotk_write_dat_COMPLEX4_0(unit,name,dat,dummy,fmt,ierr)
  use iotk_base
  implicit none
  integer,                intent(in)  :: unit
  character(*),           intent(in)  :: name
# 53 "iotk_dat_interf.spp"
  COMPLEX (kind=__IOTK_COMPLEX4),        intent(in)  :: dat 
  type(iotk_dummytype), optional      :: dummy
  character(*), optional, intent(in)  :: fmt
  integer,      optional, intent(out) :: ierr
end subroutine iotk_write_dat_COMPLEX4_0
#endif
# 46 "iotk_dat_interf.spp"
#if 1 <= __IOTK_MAXRANK
subroutine iotk_write_dat_COMPLEX4_1(unit,name,dat,dummy,fmt,ierr)
  use iotk_base
  implicit none
  integer,                intent(in)  :: unit
  character(*),           intent(in)  :: name
# 53 "iotk_dat_interf.spp"
  COMPLEX (kind=__IOTK_COMPLEX4),        intent(in)  :: dat (:)
  type(iotk_dummytype), optional      :: dummy
  character(*), optional, intent(in)  :: fmt
  integer,      optional, intent(out) :: ierr
end subroutine iotk_write_dat_COMPLEX4_1
#endif
# 46 "iotk_dat_interf.spp"
#if 2 <= __IOTK_MAXRANK
subroutine iotk_write_dat_COMPLEX4_2(unit,name,dat,dummy,fmt,ierr)
  use iotk_base
  implicit none
  integer,                intent(in)  :: unit
  character(*),           intent(in)  :: name
# 53 "iotk_dat_interf.spp"
  COMPLEX (kind=__IOTK_COMPLEX4),        intent(in)  :: dat (:,:)
  type(iotk_dummytype), optional      :: dummy
  character(*), optional, intent(in)  :: fmt
  integer,      optional, intent(out) :: ierr
end subroutine iotk_write_dat_COMPLEX4_2
#endif
# 46 "iotk_dat_interf.spp"
#if 3 <= __IOTK_MAXRANK
subroutine iotk_write_dat_COMPLEX4_3(unit,name,dat,dummy,fmt,ierr)
  use iotk_base
  implicit none
  integer,                intent(in)  :: unit
  character(*),           intent(in)  :: name
# 53 "iotk_dat_interf.spp"
  COMPLEX (kind=__IOTK_COMPLEX4),        intent(in)  :: dat (:,:,:)
  type(iotk_dummytype), optional      :: dummy
  character(*), optional, intent(in)  :: fmt
  integer,      optional, intent(out) :: ierr
end subroutine iotk_write_dat_COMPLEX4_3
#endif
# 46 "iotk_dat_interf.spp"
#if 4 <= __IOTK_MAXRANK
subroutine iotk_write_dat_COMPLEX4_4(unit,name,dat,dummy,fmt,ierr)
  use iotk_base
  implicit none
  integer,                intent(in)  :: unit
  character(*),           intent(in)  :: name
# 53 "iotk_dat_interf.spp"
  COMPLEX (kind=__IOTK_COMPLEX4),        intent(in)  :: dat (:,:,:,:)
  type(iotk_dummytype), optional      :: dummy
  character(*), optional, intent(in)  :: fmt
  integer,      optional, intent(out) :: ierr
end subroutine iotk_write_dat_COMPLEX4_4
#endif
# 46 "iotk_dat_interf.spp"
#if 5 <= __IOTK_MAXRANK
subroutine iotk_write_dat_COMPLEX4_5(unit,name,dat,dummy,fmt,ierr)
  use iotk_base
  implicit none
  integer,                intent(in)  :: unit
  character(*),           intent(in)  :: name
# 53 "iotk_dat_interf.spp"
  COMPLEX (kind=__IOTK_COMPLEX4),        intent(in)  :: dat (:,:,:,:,:)
  type(iotk_dummytype), optional      :: dummy
  character(*), optional, intent(in)  :: fmt
  integer,      optional, intent(out) :: ierr
end subroutine iotk_write_dat_COMPLEX4_5
#endif
# 46 "iotk_dat_interf.spp"
#if 6 <= __IOTK_MAXRANK
subroutine iotk_write_dat_COMPLEX4_6(unit,name,dat,dummy,fmt,ierr)
  use iotk_base
  implicit none
  integer,                intent(in)  :: unit
  character(*),           intent(in)  :: name
# 53 "iotk_dat_interf.spp"
  COMPLEX (kind=__IOTK_COMPLEX4),        intent(in)  :: dat (:,:,:,:,:,:)
  type(iotk_dummytype), optional      :: dummy
  character(*), optional, intent(in)  :: fmt
  integer,      optional, intent(out) :: ierr
end subroutine iotk_write_dat_COMPLEX4_6
#endif
# 46 "iotk_dat_interf.spp"
#if 7 <= __IOTK_MAXRANK
subroutine iotk_write_dat_COMPLEX4_7(unit,name,dat,dummy,fmt,ierr)
  use iotk_base
  implicit none
  integer,                intent(in)  :: unit
  character(*),           intent(in)  :: name
# 53 "iotk_dat_interf.spp"
  COMPLEX (kind=__IOTK_COMPLEX4),        intent(in)  :: dat (:,:,:,:,:,:,:)
  type(iotk_dummytype), optional      :: dummy
  character(*), optional, intent(in)  :: fmt
  integer,      optional, intent(out) :: ierr
end subroutine iotk_write_dat_COMPLEX4_7
#endif
# 60 "iotk_dat_interf.spp"
#endif
# 44 "iotk_dat_interf.spp"
#ifdef __IOTK_CHARACTER1
# 46 "iotk_dat_interf.spp"
#if 0 <= __IOTK_MAXRANK
subroutine iotk_write_dat_CHARACTER1_0(unit,name,dat,dummy,fmt,ierr)
  use iotk_base
  implicit none
  integer,                intent(in)  :: unit
  character(*),           intent(in)  :: name
# 53 "iotk_dat_interf.spp"
  CHARACTER (kind=__IOTK_CHARACTER1,len=*),        intent(in)  :: dat 
  type(iotk_dummytype), optional      :: dummy
  character(*), optional, intent(in)  :: fmt
  integer,      optional, intent(out) :: ierr
end subroutine iotk_write_dat_CHARACTER1_0
#endif
# 46 "iotk_dat_interf.spp"
#if 1 <= __IOTK_MAXRANK
subroutine iotk_write_dat_CHARACTER1_1(unit,name,dat,dummy,fmt,ierr)
  use iotk_base
  implicit none
  integer,                intent(in)  :: unit
  character(*),           intent(in)  :: name
# 53 "iotk_dat_interf.spp"
  CHARACTER (kind=__IOTK_CHARACTER1,len=*),        intent(in)  :: dat (:)
  type(iotk_dummytype), optional      :: dummy
  character(*), optional, intent(in)  :: fmt
  integer,      optional, intent(out) :: ierr
end subroutine iotk_write_dat_CHARACTER1_1
#endif
# 46 "iotk_dat_interf.spp"
#if 2 <= __IOTK_MAXRANK
subroutine iotk_write_dat_CHARACTER1_2(unit,name,dat,dummy,fmt,ierr)
  use iotk_base
  implicit none
  integer,                intent(in)  :: unit
  character(*),           intent(in)  :: name
# 53 "iotk_dat_interf.spp"
  CHARACTER (kind=__IOTK_CHARACTER1,len=*),        intent(in)  :: dat (:,:)
  type(iotk_dummytype), optional      :: dummy
  character(*), optional, intent(in)  :: fmt
  integer,      optional, intent(out) :: ierr
end subroutine iotk_write_dat_CHARACTER1_2
#endif
# 46 "iotk_dat_interf.spp"
#if 3 <= __IOTK_MAXRANK
subroutine iotk_write_dat_CHARACTER1_3(unit,name,dat,dummy,fmt,ierr)
  use iotk_base
  implicit none
  integer,                intent(in)  :: unit
  character(*),           intent(in)  :: name
# 53 "iotk_dat_interf.spp"
  CHARACTER (kind=__IOTK_CHARACTER1,len=*),        intent(in)  :: dat (:,:,:)
  type(iotk_dummytype), optional      :: dummy
  character(*), optional, intent(in)  :: fmt
  integer,      optional, intent(out) :: ierr
end subroutine iotk_write_dat_CHARACTER1_3
#endif
# 46 "iotk_dat_interf.spp"
#if 4 <= __IOTK_MAXRANK
subroutine iotk_write_dat_CHARACTER1_4(unit,name,dat,dummy,fmt,ierr)
  use iotk_base
  implicit none
  integer,                intent(in)  :: unit
  character(*),           intent(in)  :: name
# 53 "iotk_dat_interf.spp"
  CHARACTER (kind=__IOTK_CHARACTER1,len=*),        intent(in)  :: dat (:,:,:,:)
  type(iotk_dummytype), optional      :: dummy
  character(*), optional, intent(in)  :: fmt
  integer,      optional, intent(out) :: ierr
end subroutine iotk_write_dat_CHARACTER1_4
#endif
# 46 "iotk_dat_interf.spp"
#if 5 <= __IOTK_MAXRANK
subroutine iotk_write_dat_CHARACTER1_5(unit,name,dat,dummy,fmt,ierr)
  use iotk_base
  implicit none
  integer,                intent(in)  :: unit
  character(*),           intent(in)  :: name
# 53 "iotk_dat_interf.spp"
  CHARACTER (kind=__IOTK_CHARACTER1,len=*),        intent(in)  :: dat (:,:,:,:,:)
  type(iotk_dummytype), optional      :: dummy
  character(*), optional, intent(in)  :: fmt
  integer,      optional, intent(out) :: ierr
end subroutine iotk_write_dat_CHARACTER1_5
#endif
# 46 "iotk_dat_interf.spp"
#if 6 <= __IOTK_MAXRANK
subroutine iotk_write_dat_CHARACTER1_6(unit,name,dat,dummy,fmt,ierr)
  use iotk_base
  implicit none
  integer,                intent(in)  :: unit
  character(*),           intent(in)  :: name
# 53 "iotk_dat_interf.spp"
  CHARACTER (kind=__IOTK_CHARACTER1,len=*),        intent(in)  :: dat (:,:,:,:,:,:)
  type(iotk_dummytype), optional      :: dummy
  character(*), optional, intent(in)  :: fmt
  integer,      optional, intent(out) :: ierr
end subroutine iotk_write_dat_CHARACTER1_6
#endif
# 46 "iotk_dat_interf.spp"
#if 7 <= __IOTK_MAXRANK
subroutine iotk_write_dat_CHARACTER1_7(unit,name,dat,dummy,fmt,ierr)
  use iotk_base
  implicit none
  integer,                intent(in)  :: unit
  character(*),           intent(in)  :: name
# 53 "iotk_dat_interf.spp"
  CHARACTER (kind=__IOTK_CHARACTER1,len=*),        intent(in)  :: dat (:,:,:,:,:,:,:)
  type(iotk_dummytype), optional      :: dummy
  character(*), optional, intent(in)  :: fmt
  integer,      optional, intent(out) :: ierr
end subroutine iotk_write_dat_CHARACTER1_7
#endif
# 60 "iotk_dat_interf.spp"
#endif
# 64 "iotk_dat_interf.spp"
end interface

interface iotk_scan_dat
# 70 "iotk_dat_interf.spp"
#ifdef __IOTK_LOGICAL1
# 72 "iotk_dat_interf.spp"
#if 0 <= __IOTK_MAXRANK
subroutine iotk_scan_dat_LOGICAL1_0(unit,name,dat,dummy,found,default,ierr)
  use iotk_base
  implicit none
  integer,                   intent(in)  :: unit
  character(*),              intent(in)  :: name
# 79 "iotk_dat_interf.spp"
  LOGICAL (kind=__IOTK_LOGICAL1),           intent(out) :: dat 
  type(iotk_dummytype), optional      :: dummy
  logical,         optional, intent(out) :: found
  LOGICAL (kind=__IOTK_LOGICAL1), optional, intent(in)  :: default 
  integer,         optional, intent(out) :: ierr
end subroutine iotk_scan_dat_LOGICAL1_0
#endif
# 72 "iotk_dat_interf.spp"
#if 1 <= __IOTK_MAXRANK
subroutine iotk_scan_dat_LOGICAL1_1(unit,name,dat,dummy,found,default,ierr)
  use iotk_base
  implicit none
  integer,                   intent(in)  :: unit
  character(*),              intent(in)  :: name
# 79 "iotk_dat_interf.spp"
  LOGICAL (kind=__IOTK_LOGICAL1),           intent(out) :: dat (:)
  type(iotk_dummytype), optional      :: dummy
  logical,         optional, intent(out) :: found
  LOGICAL (kind=__IOTK_LOGICAL1), optional, intent(in)  :: default (:)
  integer,         optional, intent(out) :: ierr
end subroutine iotk_scan_dat_LOGICAL1_1
#endif
# 72 "iotk_dat_interf.spp"
#if 2 <= __IOTK_MAXRANK
subroutine iotk_scan_dat_LOGICAL1_2(unit,name,dat,dummy,found,default,ierr)
  use iotk_base
  implicit none
  integer,                   intent(in)  :: unit
  character(*),              intent(in)  :: name
# 79 "iotk_dat_interf.spp"
  LOGICAL (kind=__IOTK_LOGICAL1),           intent(out) :: dat (:,:)
  type(iotk_dummytype), optional      :: dummy
  logical,         optional, intent(out) :: found
  LOGICAL (kind=__IOTK_LOGICAL1), optional, intent(in)  :: default (:,:)
  integer,         optional, intent(out) :: ierr
end subroutine iotk_scan_dat_LOGICAL1_2
#endif
# 72 "iotk_dat_interf.spp"
#if 3 <= __IOTK_MAXRANK
subroutine iotk_scan_dat_LOGICAL1_3(unit,name,dat,dummy,found,default,ierr)
  use iotk_base
  implicit none
  integer,                   intent(in)  :: unit
  character(*),              intent(in)  :: name
# 79 "iotk_dat_interf.spp"
  LOGICAL (kind=__IOTK_LOGICAL1),           intent(out) :: dat (:,:,:)
  type(iotk_dummytype), optional      :: dummy
  logical,         optional, intent(out) :: found
  LOGICAL (kind=__IOTK_LOGICAL1), optional, intent(in)  :: default (:,:,:)
  integer,         optional, intent(out) :: ierr
end subroutine iotk_scan_dat_LOGICAL1_3
#endif
# 72 "iotk_dat_interf.spp"
#if 4 <= __IOTK_MAXRANK
subroutine iotk_scan_dat_LOGICAL1_4(unit,name,dat,dummy,found,default,ierr)
  use iotk_base
  implicit none
  integer,                   intent(in)  :: unit
  character(*),              intent(in)  :: name
# 79 "iotk_dat_interf.spp"
  LOGICAL (kind=__IOTK_LOGICAL1),           intent(out) :: dat (:,:,:,:)
  type(iotk_dummytype), optional      :: dummy
  logical,         optional, intent(out) :: found
  LOGICAL (kind=__IOTK_LOGICAL1), optional, intent(in)  :: default (:,:,:,:)
  integer,         optional, intent(out) :: ierr
end subroutine iotk_scan_dat_LOGICAL1_4
#endif
# 72 "iotk_dat_interf.spp"
#if 5 <= __IOTK_MAXRANK
subroutine iotk_scan_dat_LOGICAL1_5(unit,name,dat,dummy,found,default,ierr)
  use iotk_base
  implicit none
  integer,                   intent(in)  :: unit
  character(*),              intent(in)  :: name
# 79 "iotk_dat_interf.spp"
  LOGICAL (kind=__IOTK_LOGICAL1),           intent(out) :: dat (:,:,:,:,:)
  type(iotk_dummytype), optional      :: dummy
  logical,         optional, intent(out) :: found
  LOGICAL (kind=__IOTK_LOGICAL1), optional, intent(in)  :: default (:,:,:,:,:)
  integer,         optional, intent(out) :: ierr
end subroutine iotk_scan_dat_LOGICAL1_5
#endif
# 72 "iotk_dat_interf.spp"
#if 6 <= __IOTK_MAXRANK
subroutine iotk_scan_dat_LOGICAL1_6(unit,name,dat,dummy,found,default,ierr)
  use iotk_base
  implicit none
  integer,                   intent(in)  :: unit
  character(*),              intent(in)  :: name
# 79 "iotk_dat_interf.spp"
  LOGICAL (kind=__IOTK_LOGICAL1),           intent(out) :: dat (:,:,:,:,:,:)
  type(iotk_dummytype), optional      :: dummy
  logical,         optional, intent(out) :: found
  LOGICAL (kind=__IOTK_LOGICAL1), optional, intent(in)  :: default (:,:,:,:,:,:)
  integer,         optional, intent(out) :: ierr
end subroutine iotk_scan_dat_LOGICAL1_6
#endif
# 72 "iotk_dat_interf.spp"
#if 7 <= __IOTK_MAXRANK
subroutine iotk_scan_dat_LOGICAL1_7(unit,name,dat,dummy,found,default,ierr)
  use iotk_base
  implicit none
  integer,                   intent(in)  :: unit
  character(*),              intent(in)  :: name
# 79 "iotk_dat_interf.spp"
  LOGICAL (kind=__IOTK_LOGICAL1),           intent(out) :: dat (:,:,:,:,:,:,:)
  type(iotk_dummytype), optional      :: dummy
  logical,         optional, intent(out) :: found
  LOGICAL (kind=__IOTK_LOGICAL1), optional, intent(in)  :: default (:,:,:,:,:,:,:)
  integer,         optional, intent(out) :: ierr
end subroutine iotk_scan_dat_LOGICAL1_7
#endif
# 87 "iotk_dat_interf.spp"
#endif
# 70 "iotk_dat_interf.spp"
#ifdef __IOTK_LOGICAL2
# 72 "iotk_dat_interf.spp"
#if 0 <= __IOTK_MAXRANK
subroutine iotk_scan_dat_LOGICAL2_0(unit,name,dat,dummy,found,default,ierr)
  use iotk_base
  implicit none
  integer,                   intent(in)  :: unit
  character(*),              intent(in)  :: name
# 79 "iotk_dat_interf.spp"
  LOGICAL (kind=__IOTK_LOGICAL2),           intent(out) :: dat 
  type(iotk_dummytype), optional      :: dummy
  logical,         optional, intent(out) :: found
  LOGICAL (kind=__IOTK_LOGICAL2), optional, intent(in)  :: default 
  integer,         optional, intent(out) :: ierr
end subroutine iotk_scan_dat_LOGICAL2_0
#endif
# 72 "iotk_dat_interf.spp"
#if 1 <= __IOTK_MAXRANK
subroutine iotk_scan_dat_LOGICAL2_1(unit,name,dat,dummy,found,default,ierr)
  use iotk_base
  implicit none
  integer,                   intent(in)  :: unit
  character(*),              intent(in)  :: name
# 79 "iotk_dat_interf.spp"
  LOGICAL (kind=__IOTK_LOGICAL2),           intent(out) :: dat (:)
  type(iotk_dummytype), optional      :: dummy
  logical,         optional, intent(out) :: found
  LOGICAL (kind=__IOTK_LOGICAL2), optional, intent(in)  :: default (:)
  integer,         optional, intent(out) :: ierr
end subroutine iotk_scan_dat_LOGICAL2_1
#endif
# 72 "iotk_dat_interf.spp"
#if 2 <= __IOTK_MAXRANK
subroutine iotk_scan_dat_LOGICAL2_2(unit,name,dat,dummy,found,default,ierr)
  use iotk_base
  implicit none
  integer,                   intent(in)  :: unit
  character(*),              intent(in)  :: name
# 79 "iotk_dat_interf.spp"
  LOGICAL (kind=__IOTK_LOGICAL2),           intent(out) :: dat (:,:)
  type(iotk_dummytype), optional      :: dummy
  logical,         optional, intent(out) :: found
  LOGICAL (kind=__IOTK_LOGICAL2), optional, intent(in)  :: default (:,:)
  integer,         optional, intent(out) :: ierr
end subroutine iotk_scan_dat_LOGICAL2_2
#endif
# 72 "iotk_dat_interf.spp"
#if 3 <= __IOTK_MAXRANK
subroutine iotk_scan_dat_LOGICAL2_3(unit,name,dat,dummy,found,default,ierr)
  use iotk_base
  implicit none
  integer,                   intent(in)  :: unit
  character(*),              intent(in)  :: name
# 79 "iotk_dat_interf.spp"
  LOGICAL (kind=__IOTK_LOGICAL2),           intent(out) :: dat (:,:,:)
  type(iotk_dummytype), optional      :: dummy
  logical,         optional, intent(out) :: found
  LOGICAL (kind=__IOTK_LOGICAL2), optional, intent(in)  :: default (:,:,:)
  integer,         optional, intent(out) :: ierr
end subroutine iotk_scan_dat_LOGICAL2_3
#endif
# 72 "iotk_dat_interf.spp"
#if 4 <= __IOTK_MAXRANK
subroutine iotk_scan_dat_LOGICAL2_4(unit,name,dat,dummy,found,default,ierr)
  use iotk_base
  implicit none
  integer,                   intent(in)  :: unit
  character(*),              intent(in)  :: name
# 79 "iotk_dat_interf.spp"
  LOGICAL (kind=__IOTK_LOGICAL2),           intent(out) :: dat (:,:,:,:)
  type(iotk_dummytype), optional      :: dummy
  logical,         optional, intent(out) :: found
  LOGICAL (kind=__IOTK_LOGICAL2), optional, intent(in)  :: default (:,:,:,:)
  integer,         optional, intent(out) :: ierr
end subroutine iotk_scan_dat_LOGICAL2_4
#endif
# 72 "iotk_dat_interf.spp"
#if 5 <= __IOTK_MAXRANK
subroutine iotk_scan_dat_LOGICAL2_5(unit,name,dat,dummy,found,default,ierr)
  use iotk_base
  implicit none
  integer,                   intent(in)  :: unit
  character(*),              intent(in)  :: name
# 79 "iotk_dat_interf.spp"
  LOGICAL (kind=__IOTK_LOGICAL2),           intent(out) :: dat (:,:,:,:,:)
  type(iotk_dummytype), optional      :: dummy
  logical,         optional, intent(out) :: found
  LOGICAL (kind=__IOTK_LOGICAL2), optional, intent(in)  :: default (:,:,:,:,:)
  integer,         optional, intent(out) :: ierr
end subroutine iotk_scan_dat_LOGICAL2_5
#endif
# 72 "iotk_dat_interf.spp"
#if 6 <= __IOTK_MAXRANK
subroutine iotk_scan_dat_LOGICAL2_6(unit,name,dat,dummy,found,default,ierr)
  use iotk_base
  implicit none
  integer,                   intent(in)  :: unit
  character(*),              intent(in)  :: name
# 79 "iotk_dat_interf.spp"
  LOGICAL (kind=__IOTK_LOGICAL2),           intent(out) :: dat (:,:,:,:,:,:)
  type(iotk_dummytype), optional      :: dummy
  logical,         optional, intent(out) :: found
  LOGICAL (kind=__IOTK_LOGICAL2), optional, intent(in)  :: default (:,:,:,:,:,:)
  integer,         optional, intent(out) :: ierr
end subroutine iotk_scan_dat_LOGICAL2_6
#endif
# 72 "iotk_dat_interf.spp"
#if 7 <= __IOTK_MAXRANK
subroutine iotk_scan_dat_LOGICAL2_7(unit,name,dat,dummy,found,default,ierr)
  use iotk_base
  implicit none
  integer,                   intent(in)  :: unit
  character(*),              intent(in)  :: name
# 79 "iotk_dat_interf.spp"
  LOGICAL (kind=__IOTK_LOGICAL2),           intent(out) :: dat (:,:,:,:,:,:,:)
  type(iotk_dummytype), optional      :: dummy
  logical,         optional, intent(out) :: found
  LOGICAL (kind=__IOTK_LOGICAL2), optional, intent(in)  :: default (:,:,:,:,:,:,:)
  integer,         optional, intent(out) :: ierr
end subroutine iotk_scan_dat_LOGICAL2_7
#endif
# 87 "iotk_dat_interf.spp"
#endif
# 70 "iotk_dat_interf.spp"
#ifdef __IOTK_LOGICAL3
# 72 "iotk_dat_interf.spp"
#if 0 <= __IOTK_MAXRANK
subroutine iotk_scan_dat_LOGICAL3_0(unit,name,dat,dummy,found,default,ierr)
  use iotk_base
  implicit none
  integer,                   intent(in)  :: unit
  character(*),              intent(in)  :: name
# 79 "iotk_dat_interf.spp"
  LOGICAL (kind=__IOTK_LOGICAL3),           intent(out) :: dat 
  type(iotk_dummytype), optional      :: dummy
  logical,         optional, intent(out) :: found
  LOGICAL (kind=__IOTK_LOGICAL3), optional, intent(in)  :: default 
  integer,         optional, intent(out) :: ierr
end subroutine iotk_scan_dat_LOGICAL3_0
#endif
# 72 "iotk_dat_interf.spp"
#if 1 <= __IOTK_MAXRANK
subroutine iotk_scan_dat_LOGICAL3_1(unit,name,dat,dummy,found,default,ierr)
  use iotk_base
  implicit none
  integer,                   intent(in)  :: unit
  character(*),              intent(in)  :: name
# 79 "iotk_dat_interf.spp"
  LOGICAL (kind=__IOTK_LOGICAL3),           intent(out) :: dat (:)
  type(iotk_dummytype), optional      :: dummy
  logical,         optional, intent(out) :: found
  LOGICAL (kind=__IOTK_LOGICAL3), optional, intent(in)  :: default (:)
  integer,         optional, intent(out) :: ierr
end subroutine iotk_scan_dat_LOGICAL3_1
#endif
# 72 "iotk_dat_interf.spp"
#if 2 <= __IOTK_MAXRANK
subroutine iotk_scan_dat_LOGICAL3_2(unit,name,dat,dummy,found,default,ierr)
  use iotk_base
  implicit none
  integer,                   intent(in)  :: unit
  character(*),              intent(in)  :: name
# 79 "iotk_dat_interf.spp"
  LOGICAL (kind=__IOTK_LOGICAL3),           intent(out) :: dat (:,:)
  type(iotk_dummytype), optional      :: dummy
  logical,         optional, intent(out) :: found
  LOGICAL (kind=__IOTK_LOGICAL3), optional, intent(in)  :: default (:,:)
  integer,         optional, intent(out) :: ierr
end subroutine iotk_scan_dat_LOGICAL3_2
#endif
# 72 "iotk_dat_interf.spp"
#if 3 <= __IOTK_MAXRANK
subroutine iotk_scan_dat_LOGICAL3_3(unit,name,dat,dummy,found,default,ierr)
  use iotk_base
  implicit none
  integer,                   intent(in)  :: unit
  character(*),              intent(in)  :: name
# 79 "iotk_dat_interf.spp"
  LOGICAL (kind=__IOTK_LOGICAL3),           intent(out) :: dat (:,:,:)
  type(iotk_dummytype), optional      :: dummy
  logical,         optional, intent(out) :: found
  LOGICAL (kind=__IOTK_LOGICAL3), optional, intent(in)  :: default (:,:,:)
  integer,         optional, intent(out) :: ierr
end subroutine iotk_scan_dat_LOGICAL3_3
#endif
# 72 "iotk_dat_interf.spp"
#if 4 <= __IOTK_MAXRANK
subroutine iotk_scan_dat_LOGICAL3_4(unit,name,dat,dummy,found,default,ierr)
  use iotk_base
  implicit none
  integer,                   intent(in)  :: unit
  character(*),              intent(in)  :: name
# 79 "iotk_dat_interf.spp"
  LOGICAL (kind=__IOTK_LOGICAL3),           intent(out) :: dat (:,:,:,:)
  type(iotk_dummytype), optional      :: dummy
  logical,         optional, intent(out) :: found
  LOGICAL (kind=__IOTK_LOGICAL3), optional, intent(in)  :: default (:,:,:,:)
  integer,         optional, intent(out) :: ierr
end subroutine iotk_scan_dat_LOGICAL3_4
#endif
# 72 "iotk_dat_interf.spp"
#if 5 <= __IOTK_MAXRANK
subroutine iotk_scan_dat_LOGICAL3_5(unit,name,dat,dummy,found,default,ierr)
  use iotk_base
  implicit none
  integer,                   intent(in)  :: unit
  character(*),              intent(in)  :: name
# 79 "iotk_dat_interf.spp"
  LOGICAL (kind=__IOTK_LOGICAL3),           intent(out) :: dat (:,:,:,:,:)
  type(iotk_dummytype), optional      :: dummy
  logical,         optional, intent(out) :: found
  LOGICAL (kind=__IOTK_LOGICAL3), optional, intent(in)  :: default (:,:,:,:,:)
  integer,         optional, intent(out) :: ierr
end subroutine iotk_scan_dat_LOGICAL3_5
#endif
# 72 "iotk_dat_interf.spp"
#if 6 <= __IOTK_MAXRANK
subroutine iotk_scan_dat_LOGICAL3_6(unit,name,dat,dummy,found,default,ierr)
  use iotk_base
  implicit none
  integer,                   intent(in)  :: unit
  character(*),              intent(in)  :: name
# 79 "iotk_dat_interf.spp"
  LOGICAL (kind=__IOTK_LOGICAL3),           intent(out) :: dat (:,:,:,:,:,:)
  type(iotk_dummytype), optional      :: dummy
  logical,         optional, intent(out) :: found
  LOGICAL (kind=__IOTK_LOGICAL3), optional, intent(in)  :: default (:,:,:,:,:,:)
  integer,         optional, intent(out) :: ierr
end subroutine iotk_scan_dat_LOGICAL3_6
#endif
# 72 "iotk_dat_interf.spp"
#if 7 <= __IOTK_MAXRANK
subroutine iotk_scan_dat_LOGICAL3_7(unit,name,dat,dummy,found,default,ierr)
  use iotk_base
  implicit none
  integer,                   intent(in)  :: unit
  character(*),              intent(in)  :: name
# 79 "iotk_dat_interf.spp"
  LOGICAL (kind=__IOTK_LOGICAL3),           intent(out) :: dat (:,:,:,:,:,:,:)
  type(iotk_dummytype), optional      :: dummy
  logical,         optional, intent(out) :: found
  LOGICAL (kind=__IOTK_LOGICAL3), optional, intent(in)  :: default (:,:,:,:,:,:,:)
  integer,         optional, intent(out) :: ierr
end subroutine iotk_scan_dat_LOGICAL3_7
#endif
# 87 "iotk_dat_interf.spp"
#endif
# 70 "iotk_dat_interf.spp"
#ifdef __IOTK_LOGICAL4
# 72 "iotk_dat_interf.spp"
#if 0 <= __IOTK_MAXRANK
subroutine iotk_scan_dat_LOGICAL4_0(unit,name,dat,dummy,found,default,ierr)
  use iotk_base
  implicit none
  integer,                   intent(in)  :: unit
  character(*),              intent(in)  :: name
# 79 "iotk_dat_interf.spp"
  LOGICAL (kind=__IOTK_LOGICAL4),           intent(out) :: dat 
  type(iotk_dummytype), optional      :: dummy
  logical,         optional, intent(out) :: found
  LOGICAL (kind=__IOTK_LOGICAL4), optional, intent(in)  :: default 
  integer,         optional, intent(out) :: ierr
end subroutine iotk_scan_dat_LOGICAL4_0
#endif
# 72 "iotk_dat_interf.spp"
#if 1 <= __IOTK_MAXRANK
subroutine iotk_scan_dat_LOGICAL4_1(unit,name,dat,dummy,found,default,ierr)
  use iotk_base
  implicit none
  integer,                   intent(in)  :: unit
  character(*),              intent(in)  :: name
# 79 "iotk_dat_interf.spp"
  LOGICAL (kind=__IOTK_LOGICAL4),           intent(out) :: dat (:)
  type(iotk_dummytype), optional      :: dummy
  logical,         optional, intent(out) :: found
  LOGICAL (kind=__IOTK_LOGICAL4), optional, intent(in)  :: default (:)
  integer,         optional, intent(out) :: ierr
end subroutine iotk_scan_dat_LOGICAL4_1
#endif
# 72 "iotk_dat_interf.spp"
#if 2 <= __IOTK_MAXRANK
subroutine iotk_scan_dat_LOGICAL4_2(unit,name,dat,dummy,found,default,ierr)
  use iotk_base
  implicit none
  integer,                   intent(in)  :: unit
  character(*),              intent(in)  :: name
# 79 "iotk_dat_interf.spp"
  LOGICAL (kind=__IOTK_LOGICAL4),           intent(out) :: dat (:,:)
  type(iotk_dummytype), optional      :: dummy
  logical,         optional, intent(out) :: found
  LOGICAL (kind=__IOTK_LOGICAL4), optional, intent(in)  :: default (:,:)
  integer,         optional, intent(out) :: ierr
end subroutine iotk_scan_dat_LOGICAL4_2
#endif
# 72 "iotk_dat_interf.spp"
#if 3 <= __IOTK_MAXRANK
subroutine iotk_scan_dat_LOGICAL4_3(unit,name,dat,dummy,found,default,ierr)
  use iotk_base
  implicit none
  integer,                   intent(in)  :: unit
  character(*),              intent(in)  :: name
# 79 "iotk_dat_interf.spp"
  LOGICAL (kind=__IOTK_LOGICAL4),           intent(out) :: dat (:,:,:)
  type(iotk_dummytype), optional      :: dummy
  logical,         optional, intent(out) :: found
  LOGICAL (kind=__IOTK_LOGICAL4), optional, intent(in)  :: default (:,:,:)
  integer,         optional, intent(out) :: ierr
end subroutine iotk_scan_dat_LOGICAL4_3
#endif
# 72 "iotk_dat_interf.spp"
#if 4 <= __IOTK_MAXRANK
subroutine iotk_scan_dat_LOGICAL4_4(unit,name,dat,dummy,found,default,ierr)
  use iotk_base
  implicit none
  integer,                   intent(in)  :: unit
  character(*),              intent(in)  :: name
# 79 "iotk_dat_interf.spp"
  LOGICAL (kind=__IOTK_LOGICAL4),           intent(out) :: dat (:,:,:,:)
  type(iotk_dummytype), optional      :: dummy
  logical,         optional, intent(out) :: found
  LOGICAL (kind=__IOTK_LOGICAL4), optional, intent(in)  :: default (:,:,:,:)
  integer,         optional, intent(out) :: ierr
end subroutine iotk_scan_dat_LOGICAL4_4
#endif
# 72 "iotk_dat_interf.spp"
#if 5 <= __IOTK_MAXRANK
subroutine iotk_scan_dat_LOGICAL4_5(unit,name,dat,dummy,found,default,ierr)
  use iotk_base
  implicit none
  integer,                   intent(in)  :: unit
  character(*),              intent(in)  :: name
# 79 "iotk_dat_interf.spp"
  LOGICAL (kind=__IOTK_LOGICAL4),           intent(out) :: dat (:,:,:,:,:)
  type(iotk_dummytype), optional      :: dummy
  logical,         optional, intent(out) :: found
  LOGICAL (kind=__IOTK_LOGICAL4), optional, intent(in)  :: default (:,:,:,:,:)
  integer,         optional, intent(out) :: ierr
end subroutine iotk_scan_dat_LOGICAL4_5
#endif
# 72 "iotk_dat_interf.spp"
#if 6 <= __IOTK_MAXRANK
subroutine iotk_scan_dat_LOGICAL4_6(unit,name,dat,dummy,found,default,ierr)
  use iotk_base
  implicit none
  integer,                   intent(in)  :: unit
  character(*),              intent(in)  :: name
# 79 "iotk_dat_interf.spp"
  LOGICAL (kind=__IOTK_LOGICAL4),           intent(out) :: dat (:,:,:,:,:,:)
  type(iotk_dummytype), optional      :: dummy
  logical,         optional, intent(out) :: found
  LOGICAL (kind=__IOTK_LOGICAL4), optional, intent(in)  :: default (:,:,:,:,:,:)
  integer,         optional, intent(out) :: ierr
end subroutine iotk_scan_dat_LOGICAL4_6
#endif
# 72 "iotk_dat_interf.spp"
#if 7 <= __IOTK_MAXRANK
subroutine iotk_scan_dat_LOGICAL4_7(unit,name,dat,dummy,found,default,ierr)
  use iotk_base
  implicit none
  integer,                   intent(in)  :: unit
  character(*),              intent(in)  :: name
# 79 "iotk_dat_interf.spp"
  LOGICAL (kind=__IOTK_LOGICAL4),           intent(out) :: dat (:,:,:,:,:,:,:)
  type(iotk_dummytype), optional      :: dummy
  logical,         optional, intent(out) :: found
  LOGICAL (kind=__IOTK_LOGICAL4), optional, intent(in)  :: default (:,:,:,:,:,:,:)
  integer,         optional, intent(out) :: ierr
end subroutine iotk_scan_dat_LOGICAL4_7
#endif
# 87 "iotk_dat_interf.spp"
#endif
# 70 "iotk_dat_interf.spp"
#ifdef __IOTK_INTEGER1
# 72 "iotk_dat_interf.spp"
#if 0 <= __IOTK_MAXRANK
subroutine iotk_scan_dat_INTEGER1_0(unit,name,dat,dummy,found,default,ierr)
  use iotk_base
  implicit none
  integer,                   intent(in)  :: unit
  character(*),              intent(in)  :: name
# 79 "iotk_dat_interf.spp"
  INTEGER (kind=__IOTK_INTEGER1),           intent(out) :: dat 
  type(iotk_dummytype), optional      :: dummy
  logical,         optional, intent(out) :: found
  INTEGER (kind=__IOTK_INTEGER1), optional, intent(in)  :: default 
  integer,         optional, intent(out) :: ierr
end subroutine iotk_scan_dat_INTEGER1_0
#endif
# 72 "iotk_dat_interf.spp"
#if 1 <= __IOTK_MAXRANK
subroutine iotk_scan_dat_INTEGER1_1(unit,name,dat,dummy,found,default,ierr)
  use iotk_base
  implicit none
  integer,                   intent(in)  :: unit
  character(*),              intent(in)  :: name
# 79 "iotk_dat_interf.spp"
  INTEGER (kind=__IOTK_INTEGER1),           intent(out) :: dat (:)
  type(iotk_dummytype), optional      :: dummy
  logical,         optional, intent(out) :: found
  INTEGER (kind=__IOTK_INTEGER1), optional, intent(in)  :: default (:)
  integer,         optional, intent(out) :: ierr
end subroutine iotk_scan_dat_INTEGER1_1
#endif
# 72 "iotk_dat_interf.spp"
#if 2 <= __IOTK_MAXRANK
subroutine iotk_scan_dat_INTEGER1_2(unit,name,dat,dummy,found,default,ierr)
  use iotk_base
  implicit none
  integer,                   intent(in)  :: unit
  character(*),              intent(in)  :: name
# 79 "iotk_dat_interf.spp"
  INTEGER (kind=__IOTK_INTEGER1),           intent(out) :: dat (:,:)
  type(iotk_dummytype), optional      :: dummy
  logical,         optional, intent(out) :: found
  INTEGER (kind=__IOTK_INTEGER1), optional, intent(in)  :: default (:,:)
  integer,         optional, intent(out) :: ierr
end subroutine iotk_scan_dat_INTEGER1_2
#endif
# 72 "iotk_dat_interf.spp"
#if 3 <= __IOTK_MAXRANK
subroutine iotk_scan_dat_INTEGER1_3(unit,name,dat,dummy,found,default,ierr)
  use iotk_base
  implicit none
  integer,                   intent(in)  :: unit
  character(*),              intent(in)  :: name
# 79 "iotk_dat_interf.spp"
  INTEGER (kind=__IOTK_INTEGER1),           intent(out) :: dat (:,:,:)
  type(iotk_dummytype), optional      :: dummy
  logical,         optional, intent(out) :: found
  INTEGER (kind=__IOTK_INTEGER1), optional, intent(in)  :: default (:,:,:)
  integer,         optional, intent(out) :: ierr
end subroutine iotk_scan_dat_INTEGER1_3
#endif
# 72 "iotk_dat_interf.spp"
#if 4 <= __IOTK_MAXRANK
subroutine iotk_scan_dat_INTEGER1_4(unit,name,dat,dummy,found,default,ierr)
  use iotk_base
  implicit none
  integer,                   intent(in)  :: unit
  character(*),              intent(in)  :: name
# 79 "iotk_dat_interf.spp"
  INTEGER (kind=__IOTK_INTEGER1),           intent(out) :: dat (:,:,:,:)
  type(iotk_dummytype), optional      :: dummy
  logical,         optional, intent(out) :: found
  INTEGER (kind=__IOTK_INTEGER1), optional, intent(in)  :: default (:,:,:,:)
  integer,         optional, intent(out) :: ierr
end subroutine iotk_scan_dat_INTEGER1_4
#endif
# 72 "iotk_dat_interf.spp"
#if 5 <= __IOTK_MAXRANK
subroutine iotk_scan_dat_INTEGER1_5(unit,name,dat,dummy,found,default,ierr)
  use iotk_base
  implicit none
  integer,                   intent(in)  :: unit
  character(*),              intent(in)  :: name
# 79 "iotk_dat_interf.spp"
  INTEGER (kind=__IOTK_INTEGER1),           intent(out) :: dat (:,:,:,:,:)
  type(iotk_dummytype), optional      :: dummy
  logical,         optional, intent(out) :: found
  INTEGER (kind=__IOTK_INTEGER1), optional, intent(in)  :: default (:,:,:,:,:)
  integer,         optional, intent(out) :: ierr
end subroutine iotk_scan_dat_INTEGER1_5
#endif
# 72 "iotk_dat_interf.spp"
#if 6 <= __IOTK_MAXRANK
subroutine iotk_scan_dat_INTEGER1_6(unit,name,dat,dummy,found,default,ierr)
  use iotk_base
  implicit none
  integer,                   intent(in)  :: unit
  character(*),              intent(in)  :: name
# 79 "iotk_dat_interf.spp"
  INTEGER (kind=__IOTK_INTEGER1),           intent(out) :: dat (:,:,:,:,:,:)
  type(iotk_dummytype), optional      :: dummy
  logical,         optional, intent(out) :: found
  INTEGER (kind=__IOTK_INTEGER1), optional, intent(in)  :: default (:,:,:,:,:,:)
  integer,         optional, intent(out) :: ierr
end subroutine iotk_scan_dat_INTEGER1_6
#endif
# 72 "iotk_dat_interf.spp"
#if 7 <= __IOTK_MAXRANK
subroutine iotk_scan_dat_INTEGER1_7(unit,name,dat,dummy,found,default,ierr)
  use iotk_base
  implicit none
  integer,                   intent(in)  :: unit
  character(*),              intent(in)  :: name
# 79 "iotk_dat_interf.spp"
  INTEGER (kind=__IOTK_INTEGER1),           intent(out) :: dat (:,:,:,:,:,:,:)
  type(iotk_dummytype), optional      :: dummy
  logical,         optional, intent(out) :: found
  INTEGER (kind=__IOTK_INTEGER1), optional, intent(in)  :: default (:,:,:,:,:,:,:)
  integer,         optional, intent(out) :: ierr
end subroutine iotk_scan_dat_INTEGER1_7
#endif
# 87 "iotk_dat_interf.spp"
#endif
# 70 "iotk_dat_interf.spp"
#ifdef __IOTK_INTEGER2
# 72 "iotk_dat_interf.spp"
#if 0 <= __IOTK_MAXRANK
subroutine iotk_scan_dat_INTEGER2_0(unit,name,dat,dummy,found,default,ierr)
  use iotk_base
  implicit none
  integer,                   intent(in)  :: unit
  character(*),              intent(in)  :: name
# 79 "iotk_dat_interf.spp"
  INTEGER (kind=__IOTK_INTEGER2),           intent(out) :: dat 
  type(iotk_dummytype), optional      :: dummy
  logical,         optional, intent(out) :: found
  INTEGER (kind=__IOTK_INTEGER2), optional, intent(in)  :: default 
  integer,         optional, intent(out) :: ierr
end subroutine iotk_scan_dat_INTEGER2_0
#endif
# 72 "iotk_dat_interf.spp"
#if 1 <= __IOTK_MAXRANK
subroutine iotk_scan_dat_INTEGER2_1(unit,name,dat,dummy,found,default,ierr)
  use iotk_base
  implicit none
  integer,                   intent(in)  :: unit
  character(*),              intent(in)  :: name
# 79 "iotk_dat_interf.spp"
  INTEGER (kind=__IOTK_INTEGER2),           intent(out) :: dat (:)
  type(iotk_dummytype), optional      :: dummy
  logical,         optional, intent(out) :: found
  INTEGER (kind=__IOTK_INTEGER2), optional, intent(in)  :: default (:)
  integer,         optional, intent(out) :: ierr
end subroutine iotk_scan_dat_INTEGER2_1
#endif
# 72 "iotk_dat_interf.spp"
#if 2 <= __IOTK_MAXRANK
subroutine iotk_scan_dat_INTEGER2_2(unit,name,dat,dummy,found,default,ierr)
  use iotk_base
  implicit none
  integer,                   intent(in)  :: unit
  character(*),              intent(in)  :: name
# 79 "iotk_dat_interf.spp"
  INTEGER (kind=__IOTK_INTEGER2),           intent(out) :: dat (:,:)
  type(iotk_dummytype), optional      :: dummy
  logical,         optional, intent(out) :: found
  INTEGER (kind=__IOTK_INTEGER2), optional, intent(in)  :: default (:,:)
  integer,         optional, intent(out) :: ierr
end subroutine iotk_scan_dat_INTEGER2_2
#endif
# 72 "iotk_dat_interf.spp"
#if 3 <= __IOTK_MAXRANK
subroutine iotk_scan_dat_INTEGER2_3(unit,name,dat,dummy,found,default,ierr)
  use iotk_base
  implicit none
  integer,                   intent(in)  :: unit
  character(*),              intent(in)  :: name
# 79 "iotk_dat_interf.spp"
  INTEGER (kind=__IOTK_INTEGER2),           intent(out) :: dat (:,:,:)
  type(iotk_dummytype), optional      :: dummy
  logical,         optional, intent(out) :: found
  INTEGER (kind=__IOTK_INTEGER2), optional, intent(in)  :: default (:,:,:)
  integer,         optional, intent(out) :: ierr
end subroutine iotk_scan_dat_INTEGER2_3
#endif
# 72 "iotk_dat_interf.spp"
#if 4 <= __IOTK_MAXRANK
subroutine iotk_scan_dat_INTEGER2_4(unit,name,dat,dummy,found,default,ierr)
  use iotk_base
  implicit none
  integer,                   intent(in)  :: unit
  character(*),              intent(in)  :: name
# 79 "iotk_dat_interf.spp"
  INTEGER (kind=__IOTK_INTEGER2),           intent(out) :: dat (:,:,:,:)
  type(iotk_dummytype), optional      :: dummy
  logical,         optional, intent(out) :: found
  INTEGER (kind=__IOTK_INTEGER2), optional, intent(in)  :: default (:,:,:,:)
  integer,         optional, intent(out) :: ierr
end subroutine iotk_scan_dat_INTEGER2_4
#endif
# 72 "iotk_dat_interf.spp"
#if 5 <= __IOTK_MAXRANK
subroutine iotk_scan_dat_INTEGER2_5(unit,name,dat,dummy,found,default,ierr)
  use iotk_base
  implicit none
  integer,                   intent(in)  :: unit
  character(*),              intent(in)  :: name
# 79 "iotk_dat_interf.spp"
  INTEGER (kind=__IOTK_INTEGER2),           intent(out) :: dat (:,:,:,:,:)
  type(iotk_dummytype), optional      :: dummy
  logical,         optional, intent(out) :: found
  INTEGER (kind=__IOTK_INTEGER2), optional, intent(in)  :: default (:,:,:,:,:)
  integer,         optional, intent(out) :: ierr
end subroutine iotk_scan_dat_INTEGER2_5
#endif
# 72 "iotk_dat_interf.spp"
#if 6 <= __IOTK_MAXRANK
subroutine iotk_scan_dat_INTEGER2_6(unit,name,dat,dummy,found,default,ierr)
  use iotk_base
  implicit none
  integer,                   intent(in)  :: unit
  character(*),              intent(in)  :: name
# 79 "iotk_dat_interf.spp"
  INTEGER (kind=__IOTK_INTEGER2),           intent(out) :: dat (:,:,:,:,:,:)
  type(iotk_dummytype), optional      :: dummy
  logical,         optional, intent(out) :: found
  INTEGER (kind=__IOTK_INTEGER2), optional, intent(in)  :: default (:,:,:,:,:,:)
  integer,         optional, intent(out) :: ierr
end subroutine iotk_scan_dat_INTEGER2_6
#endif
# 72 "iotk_dat_interf.spp"
#if 7 <= __IOTK_MAXRANK
subroutine iotk_scan_dat_INTEGER2_7(unit,name,dat,dummy,found,default,ierr)
  use iotk_base
  implicit none
  integer,                   intent(in)  :: unit
  character(*),              intent(in)  :: name
# 79 "iotk_dat_interf.spp"
  INTEGER (kind=__IOTK_INTEGER2),           intent(out) :: dat (:,:,:,:,:,:,:)
  type(iotk_dummytype), optional      :: dummy
  logical,         optional, intent(out) :: found
  INTEGER (kind=__IOTK_INTEGER2), optional, intent(in)  :: default (:,:,:,:,:,:,:)
  integer,         optional, intent(out) :: ierr
end subroutine iotk_scan_dat_INTEGER2_7
#endif
# 87 "iotk_dat_interf.spp"
#endif
# 70 "iotk_dat_interf.spp"
#ifdef __IOTK_INTEGER3
# 72 "iotk_dat_interf.spp"
#if 0 <= __IOTK_MAXRANK
subroutine iotk_scan_dat_INTEGER3_0(unit,name,dat,dummy,found,default,ierr)
  use iotk_base
  implicit none
  integer,                   intent(in)  :: unit
  character(*),              intent(in)  :: name
# 79 "iotk_dat_interf.spp"
  INTEGER (kind=__IOTK_INTEGER3),           intent(out) :: dat 
  type(iotk_dummytype), optional      :: dummy
  logical,         optional, intent(out) :: found
  INTEGER (kind=__IOTK_INTEGER3), optional, intent(in)  :: default 
  integer,         optional, intent(out) :: ierr
end subroutine iotk_scan_dat_INTEGER3_0
#endif
# 72 "iotk_dat_interf.spp"
#if 1 <= __IOTK_MAXRANK
subroutine iotk_scan_dat_INTEGER3_1(unit,name,dat,dummy,found,default,ierr)
  use iotk_base
  implicit none
  integer,                   intent(in)  :: unit
  character(*),              intent(in)  :: name
# 79 "iotk_dat_interf.spp"
  INTEGER (kind=__IOTK_INTEGER3),           intent(out) :: dat (:)
  type(iotk_dummytype), optional      :: dummy
  logical,         optional, intent(out) :: found
  INTEGER (kind=__IOTK_INTEGER3), optional, intent(in)  :: default (:)
  integer,         optional, intent(out) :: ierr
end subroutine iotk_scan_dat_INTEGER3_1
#endif
# 72 "iotk_dat_interf.spp"
#if 2 <= __IOTK_MAXRANK
subroutine iotk_scan_dat_INTEGER3_2(unit,name,dat,dummy,found,default,ierr)
  use iotk_base
  implicit none
  integer,                   intent(in)  :: unit
  character(*),              intent(in)  :: name
# 79 "iotk_dat_interf.spp"
  INTEGER (kind=__IOTK_INTEGER3),           intent(out) :: dat (:,:)
  type(iotk_dummytype), optional      :: dummy
  logical,         optional, intent(out) :: found
  INTEGER (kind=__IOTK_INTEGER3), optional, intent(in)  :: default (:,:)
  integer,         optional, intent(out) :: ierr
end subroutine iotk_scan_dat_INTEGER3_2
#endif
# 72 "iotk_dat_interf.spp"
#if 3 <= __IOTK_MAXRANK
subroutine iotk_scan_dat_INTEGER3_3(unit,name,dat,dummy,found,default,ierr)
  use iotk_base
  implicit none
  integer,                   intent(in)  :: unit
  character(*),              intent(in)  :: name
# 79 "iotk_dat_interf.spp"
  INTEGER (kind=__IOTK_INTEGER3),           intent(out) :: dat (:,:,:)
  type(iotk_dummytype), optional      :: dummy
  logical,         optional, intent(out) :: found
  INTEGER (kind=__IOTK_INTEGER3), optional, intent(in)  :: default (:,:,:)
  integer,         optional, intent(out) :: ierr
end subroutine iotk_scan_dat_INTEGER3_3
#endif
# 72 "iotk_dat_interf.spp"
#if 4 <= __IOTK_MAXRANK
subroutine iotk_scan_dat_INTEGER3_4(unit,name,dat,dummy,found,default,ierr)
  use iotk_base
  implicit none
  integer,                   intent(in)  :: unit
  character(*),              intent(in)  :: name
# 79 "iotk_dat_interf.spp"
  INTEGER (kind=__IOTK_INTEGER3),           intent(out) :: dat (:,:,:,:)
  type(iotk_dummytype), optional      :: dummy
  logical,         optional, intent(out) :: found
  INTEGER (kind=__IOTK_INTEGER3), optional, intent(in)  :: default (:,:,:,:)
  integer,         optional, intent(out) :: ierr
end subroutine iotk_scan_dat_INTEGER3_4
#endif
# 72 "iotk_dat_interf.spp"
#if 5 <= __IOTK_MAXRANK
subroutine iotk_scan_dat_INTEGER3_5(unit,name,dat,dummy,found,default,ierr)
  use iotk_base
  implicit none
  integer,                   intent(in)  :: unit
  character(*),              intent(in)  :: name
# 79 "iotk_dat_interf.spp"
  INTEGER (kind=__IOTK_INTEGER3),           intent(out) :: dat (:,:,:,:,:)
  type(iotk_dummytype), optional      :: dummy
  logical,         optional, intent(out) :: found
  INTEGER (kind=__IOTK_INTEGER3), optional, intent(in)  :: default (:,:,:,:,:)
  integer,         optional, intent(out) :: ierr
end subroutine iotk_scan_dat_INTEGER3_5
#endif
# 72 "iotk_dat_interf.spp"
#if 6 <= __IOTK_MAXRANK
subroutine iotk_scan_dat_INTEGER3_6(unit,name,dat,dummy,found,default,ierr)
  use iotk_base
  implicit none
  integer,                   intent(in)  :: unit
  character(*),              intent(in)  :: name
# 79 "iotk_dat_interf.spp"
  INTEGER (kind=__IOTK_INTEGER3),           intent(out) :: dat (:,:,:,:,:,:)
  type(iotk_dummytype), optional      :: dummy
  logical,         optional, intent(out) :: found
  INTEGER (kind=__IOTK_INTEGER3), optional, intent(in)  :: default (:,:,:,:,:,:)
  integer,         optional, intent(out) :: ierr
end subroutine iotk_scan_dat_INTEGER3_6
#endif
# 72 "iotk_dat_interf.spp"
#if 7 <= __IOTK_MAXRANK
subroutine iotk_scan_dat_INTEGER3_7(unit,name,dat,dummy,found,default,ierr)
  use iotk_base
  implicit none
  integer,                   intent(in)  :: unit
  character(*),              intent(in)  :: name
# 79 "iotk_dat_interf.spp"
  INTEGER (kind=__IOTK_INTEGER3),           intent(out) :: dat (:,:,:,:,:,:,:)
  type(iotk_dummytype), optional      :: dummy
  logical,         optional, intent(out) :: found
  INTEGER (kind=__IOTK_INTEGER3), optional, intent(in)  :: default (:,:,:,:,:,:,:)
  integer,         optional, intent(out) :: ierr
end subroutine iotk_scan_dat_INTEGER3_7
#endif
# 87 "iotk_dat_interf.spp"
#endif
# 70 "iotk_dat_interf.spp"
#ifdef __IOTK_INTEGER4
# 72 "iotk_dat_interf.spp"
#if 0 <= __IOTK_MAXRANK
subroutine iotk_scan_dat_INTEGER4_0(unit,name,dat,dummy,found,default,ierr)
  use iotk_base
  implicit none
  integer,                   intent(in)  :: unit
  character(*),              intent(in)  :: name
# 79 "iotk_dat_interf.spp"
  INTEGER (kind=__IOTK_INTEGER4),           intent(out) :: dat 
  type(iotk_dummytype), optional      :: dummy
  logical,         optional, intent(out) :: found
  INTEGER (kind=__IOTK_INTEGER4), optional, intent(in)  :: default 
  integer,         optional, intent(out) :: ierr
end subroutine iotk_scan_dat_INTEGER4_0
#endif
# 72 "iotk_dat_interf.spp"
#if 1 <= __IOTK_MAXRANK
subroutine iotk_scan_dat_INTEGER4_1(unit,name,dat,dummy,found,default,ierr)
  use iotk_base
  implicit none
  integer,                   intent(in)  :: unit
  character(*),              intent(in)  :: name
# 79 "iotk_dat_interf.spp"
  INTEGER (kind=__IOTK_INTEGER4),           intent(out) :: dat (:)
  type(iotk_dummytype), optional      :: dummy
  logical,         optional, intent(out) :: found
  INTEGER (kind=__IOTK_INTEGER4), optional, intent(in)  :: default (:)
  integer,         optional, intent(out) :: ierr
end subroutine iotk_scan_dat_INTEGER4_1
#endif
# 72 "iotk_dat_interf.spp"
#if 2 <= __IOTK_MAXRANK
subroutine iotk_scan_dat_INTEGER4_2(unit,name,dat,dummy,found,default,ierr)
  use iotk_base
  implicit none
  integer,                   intent(in)  :: unit
  character(*),              intent(in)  :: name
# 79 "iotk_dat_interf.spp"
  INTEGER (kind=__IOTK_INTEGER4),           intent(out) :: dat (:,:)
  type(iotk_dummytype), optional      :: dummy
  logical,         optional, intent(out) :: found
  INTEGER (kind=__IOTK_INTEGER4), optional, intent(in)  :: default (:,:)
  integer,         optional, intent(out) :: ierr
end subroutine iotk_scan_dat_INTEGER4_2
#endif
# 72 "iotk_dat_interf.spp"
#if 3 <= __IOTK_MAXRANK
subroutine iotk_scan_dat_INTEGER4_3(unit,name,dat,dummy,found,default,ierr)
  use iotk_base
  implicit none
  integer,                   intent(in)  :: unit
  character(*),              intent(in)  :: name
# 79 "iotk_dat_interf.spp"
  INTEGER (kind=__IOTK_INTEGER4),           intent(out) :: dat (:,:,:)
  type(iotk_dummytype), optional      :: dummy
  logical,         optional, intent(out) :: found
  INTEGER (kind=__IOTK_INTEGER4), optional, intent(in)  :: default (:,:,:)
  integer,         optional, intent(out) :: ierr
end subroutine iotk_scan_dat_INTEGER4_3
#endif
# 72 "iotk_dat_interf.spp"
#if 4 <= __IOTK_MAXRANK
subroutine iotk_scan_dat_INTEGER4_4(unit,name,dat,dummy,found,default,ierr)
  use iotk_base
  implicit none
  integer,                   intent(in)  :: unit
  character(*),              intent(in)  :: name
# 79 "iotk_dat_interf.spp"
  INTEGER (kind=__IOTK_INTEGER4),           intent(out) :: dat (:,:,:,:)
  type(iotk_dummytype), optional      :: dummy
  logical,         optional, intent(out) :: found
  INTEGER (kind=__IOTK_INTEGER4), optional, intent(in)  :: default (:,:,:,:)
  integer,         optional, intent(out) :: ierr
end subroutine iotk_scan_dat_INTEGER4_4
#endif
# 72 "iotk_dat_interf.spp"
#if 5 <= __IOTK_MAXRANK
subroutine iotk_scan_dat_INTEGER4_5(unit,name,dat,dummy,found,default,ierr)
  use iotk_base
  implicit none
  integer,                   intent(in)  :: unit
  character(*),              intent(in)  :: name
# 79 "iotk_dat_interf.spp"
  INTEGER (kind=__IOTK_INTEGER4),           intent(out) :: dat (:,:,:,:,:)
  type(iotk_dummytype), optional      :: dummy
  logical,         optional, intent(out) :: found
  INTEGER (kind=__IOTK_INTEGER4), optional, intent(in)  :: default (:,:,:,:,:)
  integer,         optional, intent(out) :: ierr
end subroutine iotk_scan_dat_INTEGER4_5
#endif
# 72 "iotk_dat_interf.spp"
#if 6 <= __IOTK_MAXRANK
subroutine iotk_scan_dat_INTEGER4_6(unit,name,dat,dummy,found,default,ierr)
  use iotk_base
  implicit none
  integer,                   intent(in)  :: unit
  character(*),              intent(in)  :: name
# 79 "iotk_dat_interf.spp"
  INTEGER (kind=__IOTK_INTEGER4),           intent(out) :: dat (:,:,:,:,:,:)
  type(iotk_dummytype), optional      :: dummy
  logical,         optional, intent(out) :: found
  INTEGER (kind=__IOTK_INTEGER4), optional, intent(in)  :: default (:,:,:,:,:,:)
  integer,         optional, intent(out) :: ierr
end subroutine iotk_scan_dat_INTEGER4_6
#endif
# 72 "iotk_dat_interf.spp"
#if 7 <= __IOTK_MAXRANK
subroutine iotk_scan_dat_INTEGER4_7(unit,name,dat,dummy,found,default,ierr)
  use iotk_base
  implicit none
  integer,                   intent(in)  :: unit
  character(*),              intent(in)  :: name
# 79 "iotk_dat_interf.spp"
  INTEGER (kind=__IOTK_INTEGER4),           intent(out) :: dat (:,:,:,:,:,:,:)
  type(iotk_dummytype), optional      :: dummy
  logical,         optional, intent(out) :: found
  INTEGER (kind=__IOTK_INTEGER4), optional, intent(in)  :: default (:,:,:,:,:,:,:)
  integer,         optional, intent(out) :: ierr
end subroutine iotk_scan_dat_INTEGER4_7
#endif
# 87 "iotk_dat_interf.spp"
#endif
# 70 "iotk_dat_interf.spp"
#ifdef __IOTK_REAL1
# 72 "iotk_dat_interf.spp"
#if 0 <= __IOTK_MAXRANK
subroutine iotk_scan_dat_REAL1_0(unit,name,dat,dummy,found,default,ierr)
  use iotk_base
  implicit none
  integer,                   intent(in)  :: unit
  character(*),              intent(in)  :: name
# 79 "iotk_dat_interf.spp"
  REAL (kind=__IOTK_REAL1),           intent(out) :: dat 
  type(iotk_dummytype), optional      :: dummy
  logical,         optional, intent(out) :: found
  REAL (kind=__IOTK_REAL1), optional, intent(in)  :: default 
  integer,         optional, intent(out) :: ierr
end subroutine iotk_scan_dat_REAL1_0
#endif
# 72 "iotk_dat_interf.spp"
#if 1 <= __IOTK_MAXRANK
subroutine iotk_scan_dat_REAL1_1(unit,name,dat,dummy,found,default,ierr)
  use iotk_base
  implicit none
  integer,                   intent(in)  :: unit
  character(*),              intent(in)  :: name
# 79 "iotk_dat_interf.spp"
  REAL (kind=__IOTK_REAL1),           intent(out) :: dat (:)
  type(iotk_dummytype), optional      :: dummy
  logical,         optional, intent(out) :: found
  REAL (kind=__IOTK_REAL1), optional, intent(in)  :: default (:)
  integer,         optional, intent(out) :: ierr
end subroutine iotk_scan_dat_REAL1_1
#endif
# 72 "iotk_dat_interf.spp"
#if 2 <= __IOTK_MAXRANK
subroutine iotk_scan_dat_REAL1_2(unit,name,dat,dummy,found,default,ierr)
  use iotk_base
  implicit none
  integer,                   intent(in)  :: unit
  character(*),              intent(in)  :: name
# 79 "iotk_dat_interf.spp"
  REAL (kind=__IOTK_REAL1),           intent(out) :: dat (:,:)
  type(iotk_dummytype), optional      :: dummy
  logical,         optional, intent(out) :: found
  REAL (kind=__IOTK_REAL1), optional, intent(in)  :: default (:,:)
  integer,         optional, intent(out) :: ierr
end subroutine iotk_scan_dat_REAL1_2
#endif
# 72 "iotk_dat_interf.spp"
#if 3 <= __IOTK_MAXRANK
subroutine iotk_scan_dat_REAL1_3(unit,name,dat,dummy,found,default,ierr)
  use iotk_base
  implicit none
  integer,                   intent(in)  :: unit
  character(*),              intent(in)  :: name
# 79 "iotk_dat_interf.spp"
  REAL (kind=__IOTK_REAL1),           intent(out) :: dat (:,:,:)
  type(iotk_dummytype), optional      :: dummy
  logical,         optional, intent(out) :: found
  REAL (kind=__IOTK_REAL1), optional, intent(in)  :: default (:,:,:)
  integer,         optional, intent(out) :: ierr
end subroutine iotk_scan_dat_REAL1_3
#endif
# 72 "iotk_dat_interf.spp"
#if 4 <= __IOTK_MAXRANK
subroutine iotk_scan_dat_REAL1_4(unit,name,dat,dummy,found,default,ierr)
  use iotk_base
  implicit none
  integer,                   intent(in)  :: unit
  character(*),              intent(in)  :: name
# 79 "iotk_dat_interf.spp"
  REAL (kind=__IOTK_REAL1),           intent(out) :: dat (:,:,:,:)
  type(iotk_dummytype), optional      :: dummy
  logical,         optional, intent(out) :: found
  REAL (kind=__IOTK_REAL1), optional, intent(in)  :: default (:,:,:,:)
  integer,         optional, intent(out) :: ierr
end subroutine iotk_scan_dat_REAL1_4
#endif
# 72 "iotk_dat_interf.spp"
#if 5 <= __IOTK_MAXRANK
subroutine iotk_scan_dat_REAL1_5(unit,name,dat,dummy,found,default,ierr)
  use iotk_base
  implicit none
  integer,                   intent(in)  :: unit
  character(*),              intent(in)  :: name
# 79 "iotk_dat_interf.spp"
  REAL (kind=__IOTK_REAL1),           intent(out) :: dat (:,:,:,:,:)
  type(iotk_dummytype), optional      :: dummy
  logical,         optional, intent(out) :: found
  REAL (kind=__IOTK_REAL1), optional, intent(in)  :: default (:,:,:,:,:)
  integer,         optional, intent(out) :: ierr
end subroutine iotk_scan_dat_REAL1_5
#endif
# 72 "iotk_dat_interf.spp"
#if 6 <= __IOTK_MAXRANK
subroutine iotk_scan_dat_REAL1_6(unit,name,dat,dummy,found,default,ierr)
  use iotk_base
  implicit none
  integer,                   intent(in)  :: unit
  character(*),              intent(in)  :: name
# 79 "iotk_dat_interf.spp"
  REAL (kind=__IOTK_REAL1),           intent(out) :: dat (:,:,:,:,:,:)
  type(iotk_dummytype), optional      :: dummy
  logical,         optional, intent(out) :: found
  REAL (kind=__IOTK_REAL1), optional, intent(in)  :: default (:,:,:,:,:,:)
  integer,         optional, intent(out) :: ierr
end subroutine iotk_scan_dat_REAL1_6
#endif
# 72 "iotk_dat_interf.spp"
#if 7 <= __IOTK_MAXRANK
subroutine iotk_scan_dat_REAL1_7(unit,name,dat,dummy,found,default,ierr)
  use iotk_base
  implicit none
  integer,                   intent(in)  :: unit
  character(*),              intent(in)  :: name
# 79 "iotk_dat_interf.spp"
  REAL (kind=__IOTK_REAL1),           intent(out) :: dat (:,:,:,:,:,:,:)
  type(iotk_dummytype), optional      :: dummy
  logical,         optional, intent(out) :: found
  REAL (kind=__IOTK_REAL1), optional, intent(in)  :: default (:,:,:,:,:,:,:)
  integer,         optional, intent(out) :: ierr
end subroutine iotk_scan_dat_REAL1_7
#endif
# 87 "iotk_dat_interf.spp"
#endif
# 70 "iotk_dat_interf.spp"
#ifdef __IOTK_REAL2
# 72 "iotk_dat_interf.spp"
#if 0 <= __IOTK_MAXRANK
subroutine iotk_scan_dat_REAL2_0(unit,name,dat,dummy,found,default,ierr)
  use iotk_base
  implicit none
  integer,                   intent(in)  :: unit
  character(*),              intent(in)  :: name
# 79 "iotk_dat_interf.spp"
  REAL (kind=__IOTK_REAL2),           intent(out) :: dat 
  type(iotk_dummytype), optional      :: dummy
  logical,         optional, intent(out) :: found
  REAL (kind=__IOTK_REAL2), optional, intent(in)  :: default 
  integer,         optional, intent(out) :: ierr
end subroutine iotk_scan_dat_REAL2_0
#endif
# 72 "iotk_dat_interf.spp"
#if 1 <= __IOTK_MAXRANK
subroutine iotk_scan_dat_REAL2_1(unit,name,dat,dummy,found,default,ierr)
  use iotk_base
  implicit none
  integer,                   intent(in)  :: unit
  character(*),              intent(in)  :: name
# 79 "iotk_dat_interf.spp"
  REAL (kind=__IOTK_REAL2),           intent(out) :: dat (:)
  type(iotk_dummytype), optional      :: dummy
  logical,         optional, intent(out) :: found
  REAL (kind=__IOTK_REAL2), optional, intent(in)  :: default (:)
  integer,         optional, intent(out) :: ierr
end subroutine iotk_scan_dat_REAL2_1
#endif
# 72 "iotk_dat_interf.spp"
#if 2 <= __IOTK_MAXRANK
subroutine iotk_scan_dat_REAL2_2(unit,name,dat,dummy,found,default,ierr)
  use iotk_base
  implicit none
  integer,                   intent(in)  :: unit
  character(*),              intent(in)  :: name
# 79 "iotk_dat_interf.spp"
  REAL (kind=__IOTK_REAL2),           intent(out) :: dat (:,:)
  type(iotk_dummytype), optional      :: dummy
  logical,         optional, intent(out) :: found
  REAL (kind=__IOTK_REAL2), optional, intent(in)  :: default (:,:)
  integer,         optional, intent(out) :: ierr
end subroutine iotk_scan_dat_REAL2_2
#endif
# 72 "iotk_dat_interf.spp"
#if 3 <= __IOTK_MAXRANK
subroutine iotk_scan_dat_REAL2_3(unit,name,dat,dummy,found,default,ierr)
  use iotk_base
  implicit none
  integer,                   intent(in)  :: unit
  character(*),              intent(in)  :: name
# 79 "iotk_dat_interf.spp"
  REAL (kind=__IOTK_REAL2),           intent(out) :: dat (:,:,:)
  type(iotk_dummytype), optional      :: dummy
  logical,         optional, intent(out) :: found
  REAL (kind=__IOTK_REAL2), optional, intent(in)  :: default (:,:,:)
  integer,         optional, intent(out) :: ierr
end subroutine iotk_scan_dat_REAL2_3
#endif
# 72 "iotk_dat_interf.spp"
#if 4 <= __IOTK_MAXRANK
subroutine iotk_scan_dat_REAL2_4(unit,name,dat,dummy,found,default,ierr)
  use iotk_base
  implicit none
  integer,                   intent(in)  :: unit
  character(*),              intent(in)  :: name
# 79 "iotk_dat_interf.spp"
  REAL (kind=__IOTK_REAL2),           intent(out) :: dat (:,:,:,:)
  type(iotk_dummytype), optional      :: dummy
  logical,         optional, intent(out) :: found
  REAL (kind=__IOTK_REAL2), optional, intent(in)  :: default (:,:,:,:)
  integer,         optional, intent(out) :: ierr
end subroutine iotk_scan_dat_REAL2_4
#endif
# 72 "iotk_dat_interf.spp"
#if 5 <= __IOTK_MAXRANK
subroutine iotk_scan_dat_REAL2_5(unit,name,dat,dummy,found,default,ierr)
  use iotk_base
  implicit none
  integer,                   intent(in)  :: unit
  character(*),              intent(in)  :: name
# 79 "iotk_dat_interf.spp"
  REAL (kind=__IOTK_REAL2),           intent(out) :: dat (:,:,:,:,:)
  type(iotk_dummytype), optional      :: dummy
  logical,         optional, intent(out) :: found
  REAL (kind=__IOTK_REAL2), optional, intent(in)  :: default (:,:,:,:,:)
  integer,         optional, intent(out) :: ierr
end subroutine iotk_scan_dat_REAL2_5
#endif
# 72 "iotk_dat_interf.spp"
#if 6 <= __IOTK_MAXRANK
subroutine iotk_scan_dat_REAL2_6(unit,name,dat,dummy,found,default,ierr)
  use iotk_base
  implicit none
  integer,                   intent(in)  :: unit
  character(*),              intent(in)  :: name
# 79 "iotk_dat_interf.spp"
  REAL (kind=__IOTK_REAL2),           intent(out) :: dat (:,:,:,:,:,:)
  type(iotk_dummytype), optional      :: dummy
  logical,         optional, intent(out) :: found
  REAL (kind=__IOTK_REAL2), optional, intent(in)  :: default (:,:,:,:,:,:)
  integer,         optional, intent(out) :: ierr
end subroutine iotk_scan_dat_REAL2_6
#endif
# 72 "iotk_dat_interf.spp"
#if 7 <= __IOTK_MAXRANK
subroutine iotk_scan_dat_REAL2_7(unit,name,dat,dummy,found,default,ierr)
  use iotk_base
  implicit none
  integer,                   intent(in)  :: unit
  character(*),              intent(in)  :: name
# 79 "iotk_dat_interf.spp"
  REAL (kind=__IOTK_REAL2),           intent(out) :: dat (:,:,:,:,:,:,:)
  type(iotk_dummytype), optional      :: dummy
  logical,         optional, intent(out) :: found
  REAL (kind=__IOTK_REAL2), optional, intent(in)  :: default (:,:,:,:,:,:,:)
  integer,         optional, intent(out) :: ierr
end subroutine iotk_scan_dat_REAL2_7
#endif
# 87 "iotk_dat_interf.spp"
#endif
# 70 "iotk_dat_interf.spp"
#ifdef __IOTK_REAL3
# 72 "iotk_dat_interf.spp"
#if 0 <= __IOTK_MAXRANK
subroutine iotk_scan_dat_REAL3_0(unit,name,dat,dummy,found,default,ierr)
  use iotk_base
  implicit none
  integer,                   intent(in)  :: unit
  character(*),              intent(in)  :: name
# 79 "iotk_dat_interf.spp"
  REAL (kind=__IOTK_REAL3),           intent(out) :: dat 
  type(iotk_dummytype), optional      :: dummy
  logical,         optional, intent(out) :: found
  REAL (kind=__IOTK_REAL3), optional, intent(in)  :: default 
  integer,         optional, intent(out) :: ierr
end subroutine iotk_scan_dat_REAL3_0
#endif
# 72 "iotk_dat_interf.spp"
#if 1 <= __IOTK_MAXRANK
subroutine iotk_scan_dat_REAL3_1(unit,name,dat,dummy,found,default,ierr)
  use iotk_base
  implicit none
  integer,                   intent(in)  :: unit
  character(*),              intent(in)  :: name
# 79 "iotk_dat_interf.spp"
  REAL (kind=__IOTK_REAL3),           intent(out) :: dat (:)
  type(iotk_dummytype), optional      :: dummy
  logical,         optional, intent(out) :: found
  REAL (kind=__IOTK_REAL3), optional, intent(in)  :: default (:)
  integer,         optional, intent(out) :: ierr
end subroutine iotk_scan_dat_REAL3_1
#endif
# 72 "iotk_dat_interf.spp"
#if 2 <= __IOTK_MAXRANK
subroutine iotk_scan_dat_REAL3_2(unit,name,dat,dummy,found,default,ierr)
  use iotk_base
  implicit none
  integer,                   intent(in)  :: unit
  character(*),              intent(in)  :: name
# 79 "iotk_dat_interf.spp"
  REAL (kind=__IOTK_REAL3),           intent(out) :: dat (:,:)
  type(iotk_dummytype), optional      :: dummy
  logical,         optional, intent(out) :: found
  REAL (kind=__IOTK_REAL3), optional, intent(in)  :: default (:,:)
  integer,         optional, intent(out) :: ierr
end subroutine iotk_scan_dat_REAL3_2
#endif
# 72 "iotk_dat_interf.spp"
#if 3 <= __IOTK_MAXRANK
subroutine iotk_scan_dat_REAL3_3(unit,name,dat,dummy,found,default,ierr)
  use iotk_base
  implicit none
  integer,                   intent(in)  :: unit
  character(*),              intent(in)  :: name
# 79 "iotk_dat_interf.spp"
  REAL (kind=__IOTK_REAL3),           intent(out) :: dat (:,:,:)
  type(iotk_dummytype), optional      :: dummy
  logical,         optional, intent(out) :: found
  REAL (kind=__IOTK_REAL3), optional, intent(in)  :: default (:,:,:)
  integer,         optional, intent(out) :: ierr
end subroutine iotk_scan_dat_REAL3_3
#endif
# 72 "iotk_dat_interf.spp"
#if 4 <= __IOTK_MAXRANK
subroutine iotk_scan_dat_REAL3_4(unit,name,dat,dummy,found,default,ierr)
  use iotk_base
  implicit none
  integer,                   intent(in)  :: unit
  character(*),              intent(in)  :: name
# 79 "iotk_dat_interf.spp"
  REAL (kind=__IOTK_REAL3),           intent(out) :: dat (:,:,:,:)
  type(iotk_dummytype), optional      :: dummy
  logical,         optional, intent(out) :: found
  REAL (kind=__IOTK_REAL3), optional, intent(in)  :: default (:,:,:,:)
  integer,         optional, intent(out) :: ierr
end subroutine iotk_scan_dat_REAL3_4
#endif
# 72 "iotk_dat_interf.spp"
#if 5 <= __IOTK_MAXRANK
subroutine iotk_scan_dat_REAL3_5(unit,name,dat,dummy,found,default,ierr)
  use iotk_base
  implicit none
  integer,                   intent(in)  :: unit
  character(*),              intent(in)  :: name
# 79 "iotk_dat_interf.spp"
  REAL (kind=__IOTK_REAL3),           intent(out) :: dat (:,:,:,:,:)
  type(iotk_dummytype), optional      :: dummy
  logical,         optional, intent(out) :: found
  REAL (kind=__IOTK_REAL3), optional, intent(in)  :: default (:,:,:,:,:)
  integer,         optional, intent(out) :: ierr
end subroutine iotk_scan_dat_REAL3_5
#endif
# 72 "iotk_dat_interf.spp"
#if 6 <= __IOTK_MAXRANK
subroutine iotk_scan_dat_REAL3_6(unit,name,dat,dummy,found,default,ierr)
  use iotk_base
  implicit none
  integer,                   intent(in)  :: unit
  character(*),              intent(in)  :: name
# 79 "iotk_dat_interf.spp"
  REAL (kind=__IOTK_REAL3),           intent(out) :: dat (:,:,:,:,:,:)
  type(iotk_dummytype), optional      :: dummy
  logical,         optional, intent(out) :: found
  REAL (kind=__IOTK_REAL3), optional, intent(in)  :: default (:,:,:,:,:,:)
  integer,         optional, intent(out) :: ierr
end subroutine iotk_scan_dat_REAL3_6
#endif
# 72 "iotk_dat_interf.spp"
#if 7 <= __IOTK_MAXRANK
subroutine iotk_scan_dat_REAL3_7(unit,name,dat,dummy,found,default,ierr)
  use iotk_base
  implicit none
  integer,                   intent(in)  :: unit
  character(*),              intent(in)  :: name
# 79 "iotk_dat_interf.spp"
  REAL (kind=__IOTK_REAL3),           intent(out) :: dat (:,:,:,:,:,:,:)
  type(iotk_dummytype), optional      :: dummy
  logical,         optional, intent(out) :: found
  REAL (kind=__IOTK_REAL3), optional, intent(in)  :: default (:,:,:,:,:,:,:)
  integer,         optional, intent(out) :: ierr
end subroutine iotk_scan_dat_REAL3_7
#endif
# 87 "iotk_dat_interf.spp"
#endif
# 70 "iotk_dat_interf.spp"
#ifdef __IOTK_REAL4
# 72 "iotk_dat_interf.spp"
#if 0 <= __IOTK_MAXRANK
subroutine iotk_scan_dat_REAL4_0(unit,name,dat,dummy,found,default,ierr)
  use iotk_base
  implicit none
  integer,                   intent(in)  :: unit
  character(*),              intent(in)  :: name
# 79 "iotk_dat_interf.spp"
  REAL (kind=__IOTK_REAL4),           intent(out) :: dat 
  type(iotk_dummytype), optional      :: dummy
  logical,         optional, intent(out) :: found
  REAL (kind=__IOTK_REAL4), optional, intent(in)  :: default 
  integer,         optional, intent(out) :: ierr
end subroutine iotk_scan_dat_REAL4_0
#endif
# 72 "iotk_dat_interf.spp"
#if 1 <= __IOTK_MAXRANK
subroutine iotk_scan_dat_REAL4_1(unit,name,dat,dummy,found,default,ierr)
  use iotk_base
  implicit none
  integer,                   intent(in)  :: unit
  character(*),              intent(in)  :: name
# 79 "iotk_dat_interf.spp"
  REAL (kind=__IOTK_REAL4),           intent(out) :: dat (:)
  type(iotk_dummytype), optional      :: dummy
  logical,         optional, intent(out) :: found
  REAL (kind=__IOTK_REAL4), optional, intent(in)  :: default (:)
  integer,         optional, intent(out) :: ierr
end subroutine iotk_scan_dat_REAL4_1
#endif
# 72 "iotk_dat_interf.spp"
#if 2 <= __IOTK_MAXRANK
subroutine iotk_scan_dat_REAL4_2(unit,name,dat,dummy,found,default,ierr)
  use iotk_base
  implicit none
  integer,                   intent(in)  :: unit
  character(*),              intent(in)  :: name
# 79 "iotk_dat_interf.spp"
  REAL (kind=__IOTK_REAL4),           intent(out) :: dat (:,:)
  type(iotk_dummytype), optional      :: dummy
  logical,         optional, intent(out) :: found
  REAL (kind=__IOTK_REAL4), optional, intent(in)  :: default (:,:)
  integer,         optional, intent(out) :: ierr
end subroutine iotk_scan_dat_REAL4_2
#endif
# 72 "iotk_dat_interf.spp"
#if 3 <= __IOTK_MAXRANK
subroutine iotk_scan_dat_REAL4_3(unit,name,dat,dummy,found,default,ierr)
  use iotk_base
  implicit none
  integer,                   intent(in)  :: unit
  character(*),              intent(in)  :: name
# 79 "iotk_dat_interf.spp"
  REAL (kind=__IOTK_REAL4),           intent(out) :: dat (:,:,:)
  type(iotk_dummytype), optional      :: dummy
  logical,         optional, intent(out) :: found
  REAL (kind=__IOTK_REAL4), optional, intent(in)  :: default (:,:,:)
  integer,         optional, intent(out) :: ierr
end subroutine iotk_scan_dat_REAL4_3
#endif
# 72 "iotk_dat_interf.spp"
#if 4 <= __IOTK_MAXRANK
subroutine iotk_scan_dat_REAL4_4(unit,name,dat,dummy,found,default,ierr)
  use iotk_base
  implicit none
  integer,                   intent(in)  :: unit
  character(*),              intent(in)  :: name
# 79 "iotk_dat_interf.spp"
  REAL (kind=__IOTK_REAL4),           intent(out) :: dat (:,:,:,:)
  type(iotk_dummytype), optional      :: dummy
  logical,         optional, intent(out) :: found
  REAL (kind=__IOTK_REAL4), optional, intent(in)  :: default (:,:,:,:)
  integer,         optional, intent(out) :: ierr
end subroutine iotk_scan_dat_REAL4_4
#endif
# 72 "iotk_dat_interf.spp"
#if 5 <= __IOTK_MAXRANK
subroutine iotk_scan_dat_REAL4_5(unit,name,dat,dummy,found,default,ierr)
  use iotk_base
  implicit none
  integer,                   intent(in)  :: unit
  character(*),              intent(in)  :: name
# 79 "iotk_dat_interf.spp"
  REAL (kind=__IOTK_REAL4),           intent(out) :: dat (:,:,:,:,:)
  type(iotk_dummytype), optional      :: dummy
  logical,         optional, intent(out) :: found
  REAL (kind=__IOTK_REAL4), optional, intent(in)  :: default (:,:,:,:,:)
  integer,         optional, intent(out) :: ierr
end subroutine iotk_scan_dat_REAL4_5
#endif
# 72 "iotk_dat_interf.spp"
#if 6 <= __IOTK_MAXRANK
subroutine iotk_scan_dat_REAL4_6(unit,name,dat,dummy,found,default,ierr)
  use iotk_base
  implicit none
  integer,                   intent(in)  :: unit
  character(*),              intent(in)  :: name
# 79 "iotk_dat_interf.spp"
  REAL (kind=__IOTK_REAL4),           intent(out) :: dat (:,:,:,:,:,:)
  type(iotk_dummytype), optional      :: dummy
  logical,         optional, intent(out) :: found
  REAL (kind=__IOTK_REAL4), optional, intent(in)  :: default (:,:,:,:,:,:)
  integer,         optional, intent(out) :: ierr
end subroutine iotk_scan_dat_REAL4_6
#endif
# 72 "iotk_dat_interf.spp"
#if 7 <= __IOTK_MAXRANK
subroutine iotk_scan_dat_REAL4_7(unit,name,dat,dummy,found,default,ierr)
  use iotk_base
  implicit none
  integer,                   intent(in)  :: unit
  character(*),              intent(in)  :: name
# 79 "iotk_dat_interf.spp"
  REAL (kind=__IOTK_REAL4),           intent(out) :: dat (:,:,:,:,:,:,:)
  type(iotk_dummytype), optional      :: dummy
  logical,         optional, intent(out) :: found
  REAL (kind=__IOTK_REAL4), optional, intent(in)  :: default (:,:,:,:,:,:,:)
  integer,         optional, intent(out) :: ierr
end subroutine iotk_scan_dat_REAL4_7
#endif
# 87 "iotk_dat_interf.spp"
#endif
# 70 "iotk_dat_interf.spp"
#ifdef __IOTK_COMPLEX1
# 72 "iotk_dat_interf.spp"
#if 0 <= __IOTK_MAXRANK
subroutine iotk_scan_dat_COMPLEX1_0(unit,name,dat,dummy,found,default,ierr)
  use iotk_base
  implicit none
  integer,                   intent(in)  :: unit
  character(*),              intent(in)  :: name
# 79 "iotk_dat_interf.spp"
  COMPLEX (kind=__IOTK_COMPLEX1),           intent(out) :: dat 
  type(iotk_dummytype), optional      :: dummy
  logical,         optional, intent(out) :: found
  COMPLEX (kind=__IOTK_COMPLEX1), optional, intent(in)  :: default 
  integer,         optional, intent(out) :: ierr
end subroutine iotk_scan_dat_COMPLEX1_0
#endif
# 72 "iotk_dat_interf.spp"
#if 1 <= __IOTK_MAXRANK
subroutine iotk_scan_dat_COMPLEX1_1(unit,name,dat,dummy,found,default,ierr)
  use iotk_base
  implicit none
  integer,                   intent(in)  :: unit
  character(*),              intent(in)  :: name
# 79 "iotk_dat_interf.spp"
  COMPLEX (kind=__IOTK_COMPLEX1),           intent(out) :: dat (:)
  type(iotk_dummytype), optional      :: dummy
  logical,         optional, intent(out) :: found
  COMPLEX (kind=__IOTK_COMPLEX1), optional, intent(in)  :: default (:)
  integer,         optional, intent(out) :: ierr
end subroutine iotk_scan_dat_COMPLEX1_1
#endif
# 72 "iotk_dat_interf.spp"
#if 2 <= __IOTK_MAXRANK
subroutine iotk_scan_dat_COMPLEX1_2(unit,name,dat,dummy,found,default,ierr)
  use iotk_base
  implicit none
  integer,                   intent(in)  :: unit
  character(*),              intent(in)  :: name
# 79 "iotk_dat_interf.spp"
  COMPLEX (kind=__IOTK_COMPLEX1),           intent(out) :: dat (:,:)
  type(iotk_dummytype), optional      :: dummy
  logical,         optional, intent(out) :: found
  COMPLEX (kind=__IOTK_COMPLEX1), optional, intent(in)  :: default (:,:)
  integer,         optional, intent(out) :: ierr
end subroutine iotk_scan_dat_COMPLEX1_2
#endif
# 72 "iotk_dat_interf.spp"
#if 3 <= __IOTK_MAXRANK
subroutine iotk_scan_dat_COMPLEX1_3(unit,name,dat,dummy,found,default,ierr)
  use iotk_base
  implicit none
  integer,                   intent(in)  :: unit
  character(*),              intent(in)  :: name
# 79 "iotk_dat_interf.spp"
  COMPLEX (kind=__IOTK_COMPLEX1),           intent(out) :: dat (:,:,:)
  type(iotk_dummytype), optional      :: dummy
  logical,         optional, intent(out) :: found
  COMPLEX (kind=__IOTK_COMPLEX1), optional, intent(in)  :: default (:,:,:)
  integer,         optional, intent(out) :: ierr
end subroutine iotk_scan_dat_COMPLEX1_3
#endif
# 72 "iotk_dat_interf.spp"
#if 4 <= __IOTK_MAXRANK
subroutine iotk_scan_dat_COMPLEX1_4(unit,name,dat,dummy,found,default,ierr)
  use iotk_base
  implicit none
  integer,                   intent(in)  :: unit
  character(*),              intent(in)  :: name
# 79 "iotk_dat_interf.spp"
  COMPLEX (kind=__IOTK_COMPLEX1),           intent(out) :: dat (:,:,:,:)
  type(iotk_dummytype), optional      :: dummy
  logical,         optional, intent(out) :: found
  COMPLEX (kind=__IOTK_COMPLEX1), optional, intent(in)  :: default (:,:,:,:)
  integer,         optional, intent(out) :: ierr
end subroutine iotk_scan_dat_COMPLEX1_4
#endif
# 72 "iotk_dat_interf.spp"
#if 5 <= __IOTK_MAXRANK
subroutine iotk_scan_dat_COMPLEX1_5(unit,name,dat,dummy,found,default,ierr)
  use iotk_base
  implicit none
  integer,                   intent(in)  :: unit
  character(*),              intent(in)  :: name
# 79 "iotk_dat_interf.spp"
  COMPLEX (kind=__IOTK_COMPLEX1),           intent(out) :: dat (:,:,:,:,:)
  type(iotk_dummytype), optional      :: dummy
  logical,         optional, intent(out) :: found
  COMPLEX (kind=__IOTK_COMPLEX1), optional, intent(in)  :: default (:,:,:,:,:)
  integer,         optional, intent(out) :: ierr
end subroutine iotk_scan_dat_COMPLEX1_5
#endif
# 72 "iotk_dat_interf.spp"
#if 6 <= __IOTK_MAXRANK
subroutine iotk_scan_dat_COMPLEX1_6(unit,name,dat,dummy,found,default,ierr)
  use iotk_base
  implicit none
  integer,                   intent(in)  :: unit
  character(*),              intent(in)  :: name
# 79 "iotk_dat_interf.spp"
  COMPLEX (kind=__IOTK_COMPLEX1),           intent(out) :: dat (:,:,:,:,:,:)
  type(iotk_dummytype), optional      :: dummy
  logical,         optional, intent(out) :: found
  COMPLEX (kind=__IOTK_COMPLEX1), optional, intent(in)  :: default (:,:,:,:,:,:)
  integer,         optional, intent(out) :: ierr
end subroutine iotk_scan_dat_COMPLEX1_6
#endif
# 72 "iotk_dat_interf.spp"
#if 7 <= __IOTK_MAXRANK
subroutine iotk_scan_dat_COMPLEX1_7(unit,name,dat,dummy,found,default,ierr)
  use iotk_base
  implicit none
  integer,                   intent(in)  :: unit
  character(*),              intent(in)  :: name
# 79 "iotk_dat_interf.spp"
  COMPLEX (kind=__IOTK_COMPLEX1),           intent(out) :: dat (:,:,:,:,:,:,:)
  type(iotk_dummytype), optional      :: dummy
  logical,         optional, intent(out) :: found
  COMPLEX (kind=__IOTK_COMPLEX1), optional, intent(in)  :: default (:,:,:,:,:,:,:)
  integer,         optional, intent(out) :: ierr
end subroutine iotk_scan_dat_COMPLEX1_7
#endif
# 87 "iotk_dat_interf.spp"
#endif
# 70 "iotk_dat_interf.spp"
#ifdef __IOTK_COMPLEX2
# 72 "iotk_dat_interf.spp"
#if 0 <= __IOTK_MAXRANK
subroutine iotk_scan_dat_COMPLEX2_0(unit,name,dat,dummy,found,default,ierr)
  use iotk_base
  implicit none
  integer,                   intent(in)  :: unit
  character(*),              intent(in)  :: name
# 79 "iotk_dat_interf.spp"
  COMPLEX (kind=__IOTK_COMPLEX2),           intent(out) :: dat 
  type(iotk_dummytype), optional      :: dummy
  logical,         optional, intent(out) :: found
  COMPLEX (kind=__IOTK_COMPLEX2), optional, intent(in)  :: default 
  integer,         optional, intent(out) :: ierr
end subroutine iotk_scan_dat_COMPLEX2_0
#endif
# 72 "iotk_dat_interf.spp"
#if 1 <= __IOTK_MAXRANK
subroutine iotk_scan_dat_COMPLEX2_1(unit,name,dat,dummy,found,default,ierr)
  use iotk_base
  implicit none
  integer,                   intent(in)  :: unit
  character(*),              intent(in)  :: name
# 79 "iotk_dat_interf.spp"
  COMPLEX (kind=__IOTK_COMPLEX2),           intent(out) :: dat (:)
  type(iotk_dummytype), optional      :: dummy
  logical,         optional, intent(out) :: found
  COMPLEX (kind=__IOTK_COMPLEX2), optional, intent(in)  :: default (:)
  integer,         optional, intent(out) :: ierr
end subroutine iotk_scan_dat_COMPLEX2_1
#endif
# 72 "iotk_dat_interf.spp"
#if 2 <= __IOTK_MAXRANK
subroutine iotk_scan_dat_COMPLEX2_2(unit,name,dat,dummy,found,default,ierr)
  use iotk_base
  implicit none
  integer,                   intent(in)  :: unit
  character(*),              intent(in)  :: name
# 79 "iotk_dat_interf.spp"
  COMPLEX (kind=__IOTK_COMPLEX2),           intent(out) :: dat (:,:)
  type(iotk_dummytype), optional      :: dummy
  logical,         optional, intent(out) :: found
  COMPLEX (kind=__IOTK_COMPLEX2), optional, intent(in)  :: default (:,:)
  integer,         optional, intent(out) :: ierr
end subroutine iotk_scan_dat_COMPLEX2_2
#endif
# 72 "iotk_dat_interf.spp"
#if 3 <= __IOTK_MAXRANK
subroutine iotk_scan_dat_COMPLEX2_3(unit,name,dat,dummy,found,default,ierr)
  use iotk_base
  implicit none
  integer,                   intent(in)  :: unit
  character(*),              intent(in)  :: name
# 79 "iotk_dat_interf.spp"
  COMPLEX (kind=__IOTK_COMPLEX2),           intent(out) :: dat (:,:,:)
  type(iotk_dummytype), optional      :: dummy
  logical,         optional, intent(out) :: found
  COMPLEX (kind=__IOTK_COMPLEX2), optional, intent(in)  :: default (:,:,:)
  integer,         optional, intent(out) :: ierr
end subroutine iotk_scan_dat_COMPLEX2_3
#endif
# 72 "iotk_dat_interf.spp"
#if 4 <= __IOTK_MAXRANK
subroutine iotk_scan_dat_COMPLEX2_4(unit,name,dat,dummy,found,default,ierr)
  use iotk_base
  implicit none
  integer,                   intent(in)  :: unit
  character(*),              intent(in)  :: name
# 79 "iotk_dat_interf.spp"
  COMPLEX (kind=__IOTK_COMPLEX2),           intent(out) :: dat (:,:,:,:)
  type(iotk_dummytype), optional      :: dummy
  logical,         optional, intent(out) :: found
  COMPLEX (kind=__IOTK_COMPLEX2), optional, intent(in)  :: default (:,:,:,:)
  integer,         optional, intent(out) :: ierr
end subroutine iotk_scan_dat_COMPLEX2_4
#endif
# 72 "iotk_dat_interf.spp"
#if 5 <= __IOTK_MAXRANK
subroutine iotk_scan_dat_COMPLEX2_5(unit,name,dat,dummy,found,default,ierr)
  use iotk_base
  implicit none
  integer,                   intent(in)  :: unit
  character(*),              intent(in)  :: name
# 79 "iotk_dat_interf.spp"
  COMPLEX (kind=__IOTK_COMPLEX2),           intent(out) :: dat (:,:,:,:,:)
  type(iotk_dummytype), optional      :: dummy
  logical,         optional, intent(out) :: found
  COMPLEX (kind=__IOTK_COMPLEX2), optional, intent(in)  :: default (:,:,:,:,:)
  integer,         optional, intent(out) :: ierr
end subroutine iotk_scan_dat_COMPLEX2_5
#endif
# 72 "iotk_dat_interf.spp"
#if 6 <= __IOTK_MAXRANK
subroutine iotk_scan_dat_COMPLEX2_6(unit,name,dat,dummy,found,default,ierr)
  use iotk_base
  implicit none
  integer,                   intent(in)  :: unit
  character(*),              intent(in)  :: name
# 79 "iotk_dat_interf.spp"
  COMPLEX (kind=__IOTK_COMPLEX2),           intent(out) :: dat (:,:,:,:,:,:)
  type(iotk_dummytype), optional      :: dummy
  logical,         optional, intent(out) :: found
  COMPLEX (kind=__IOTK_COMPLEX2), optional, intent(in)  :: default (:,:,:,:,:,:)
  integer,         optional, intent(out) :: ierr
end subroutine iotk_scan_dat_COMPLEX2_6
#endif
# 72 "iotk_dat_interf.spp"
#if 7 <= __IOTK_MAXRANK
subroutine iotk_scan_dat_COMPLEX2_7(unit,name,dat,dummy,found,default,ierr)
  use iotk_base
  implicit none
  integer,                   intent(in)  :: unit
  character(*),              intent(in)  :: name
# 79 "iotk_dat_interf.spp"
  COMPLEX (kind=__IOTK_COMPLEX2),           intent(out) :: dat (:,:,:,:,:,:,:)
  type(iotk_dummytype), optional      :: dummy
  logical,         optional, intent(out) :: found
  COMPLEX (kind=__IOTK_COMPLEX2), optional, intent(in)  :: default (:,:,:,:,:,:,:)
  integer,         optional, intent(out) :: ierr
end subroutine iotk_scan_dat_COMPLEX2_7
#endif
# 87 "iotk_dat_interf.spp"
#endif
# 70 "iotk_dat_interf.spp"
#ifdef __IOTK_COMPLEX3
# 72 "iotk_dat_interf.spp"
#if 0 <= __IOTK_MAXRANK
subroutine iotk_scan_dat_COMPLEX3_0(unit,name,dat,dummy,found,default,ierr)
  use iotk_base
  implicit none
  integer,                   intent(in)  :: unit
  character(*),              intent(in)  :: name
# 79 "iotk_dat_interf.spp"
  COMPLEX (kind=__IOTK_COMPLEX3),           intent(out) :: dat 
  type(iotk_dummytype), optional      :: dummy
  logical,         optional, intent(out) :: found
  COMPLEX (kind=__IOTK_COMPLEX3), optional, intent(in)  :: default 
  integer,         optional, intent(out) :: ierr
end subroutine iotk_scan_dat_COMPLEX3_0
#endif
# 72 "iotk_dat_interf.spp"
#if 1 <= __IOTK_MAXRANK
subroutine iotk_scan_dat_COMPLEX3_1(unit,name,dat,dummy,found,default,ierr)
  use iotk_base
  implicit none
  integer,                   intent(in)  :: unit
  character(*),              intent(in)  :: name
# 79 "iotk_dat_interf.spp"
  COMPLEX (kind=__IOTK_COMPLEX3),           intent(out) :: dat (:)
  type(iotk_dummytype), optional      :: dummy
  logical,         optional, intent(out) :: found
  COMPLEX (kind=__IOTK_COMPLEX3), optional, intent(in)  :: default (:)
  integer,         optional, intent(out) :: ierr
end subroutine iotk_scan_dat_COMPLEX3_1
#endif
# 72 "iotk_dat_interf.spp"
#if 2 <= __IOTK_MAXRANK
subroutine iotk_scan_dat_COMPLEX3_2(unit,name,dat,dummy,found,default,ierr)
  use iotk_base
  implicit none
  integer,                   intent(in)  :: unit
  character(*),              intent(in)  :: name
# 79 "iotk_dat_interf.spp"
  COMPLEX (kind=__IOTK_COMPLEX3),           intent(out) :: dat (:,:)
  type(iotk_dummytype), optional      :: dummy
  logical,         optional, intent(out) :: found
  COMPLEX (kind=__IOTK_COMPLEX3), optional, intent(in)  :: default (:,:)
  integer,         optional, intent(out) :: ierr
end subroutine iotk_scan_dat_COMPLEX3_2
#endif
# 72 "iotk_dat_interf.spp"
#if 3 <= __IOTK_MAXRANK
subroutine iotk_scan_dat_COMPLEX3_3(unit,name,dat,dummy,found,default,ierr)
  use iotk_base
  implicit none
  integer,                   intent(in)  :: unit
  character(*),              intent(in)  :: name
# 79 "iotk_dat_interf.spp"
  COMPLEX (kind=__IOTK_COMPLEX3),           intent(out) :: dat (:,:,:)
  type(iotk_dummytype), optional      :: dummy
  logical,         optional, intent(out) :: found
  COMPLEX (kind=__IOTK_COMPLEX3), optional, intent(in)  :: default (:,:,:)
  integer,         optional, intent(out) :: ierr
end subroutine iotk_scan_dat_COMPLEX3_3
#endif
# 72 "iotk_dat_interf.spp"
#if 4 <= __IOTK_MAXRANK
subroutine iotk_scan_dat_COMPLEX3_4(unit,name,dat,dummy,found,default,ierr)
  use iotk_base
  implicit none
  integer,                   intent(in)  :: unit
  character(*),              intent(in)  :: name
# 79 "iotk_dat_interf.spp"
  COMPLEX (kind=__IOTK_COMPLEX3),           intent(out) :: dat (:,:,:,:)
  type(iotk_dummytype), optional      :: dummy
  logical,         optional, intent(out) :: found
  COMPLEX (kind=__IOTK_COMPLEX3), optional, intent(in)  :: default (:,:,:,:)
  integer,         optional, intent(out) :: ierr
end subroutine iotk_scan_dat_COMPLEX3_4
#endif
# 72 "iotk_dat_interf.spp"
#if 5 <= __IOTK_MAXRANK
subroutine iotk_scan_dat_COMPLEX3_5(unit,name,dat,dummy,found,default,ierr)
  use iotk_base
  implicit none
  integer,                   intent(in)  :: unit
  character(*),              intent(in)  :: name
# 79 "iotk_dat_interf.spp"
  COMPLEX (kind=__IOTK_COMPLEX3),           intent(out) :: dat (:,:,:,:,:)
  type(iotk_dummytype), optional      :: dummy
  logical,         optional, intent(out) :: found
  COMPLEX (kind=__IOTK_COMPLEX3), optional, intent(in)  :: default (:,:,:,:,:)
  integer,         optional, intent(out) :: ierr
end subroutine iotk_scan_dat_COMPLEX3_5
#endif
# 72 "iotk_dat_interf.spp"
#if 6 <= __IOTK_MAXRANK
subroutine iotk_scan_dat_COMPLEX3_6(unit,name,dat,dummy,found,default,ierr)
  use iotk_base
  implicit none
  integer,                   intent(in)  :: unit
  character(*),              intent(in)  :: name
# 79 "iotk_dat_interf.spp"
  COMPLEX (kind=__IOTK_COMPLEX3),           intent(out) :: dat (:,:,:,:,:,:)
  type(iotk_dummytype), optional      :: dummy
  logical,         optional, intent(out) :: found
  COMPLEX (kind=__IOTK_COMPLEX3), optional, intent(in)  :: default (:,:,:,:,:,:)
  integer,         optional, intent(out) :: ierr
end subroutine iotk_scan_dat_COMPLEX3_6
#endif
# 72 "iotk_dat_interf.spp"
#if 7 <= __IOTK_MAXRANK
subroutine iotk_scan_dat_COMPLEX3_7(unit,name,dat,dummy,found,default,ierr)
  use iotk_base
  implicit none
  integer,                   intent(in)  :: unit
  character(*),              intent(in)  :: name
# 79 "iotk_dat_interf.spp"
  COMPLEX (kind=__IOTK_COMPLEX3),           intent(out) :: dat (:,:,:,:,:,:,:)
  type(iotk_dummytype), optional      :: dummy
  logical,         optional, intent(out) :: found
  COMPLEX (kind=__IOTK_COMPLEX3), optional, intent(in)  :: default (:,:,:,:,:,:,:)
  integer,         optional, intent(out) :: ierr
end subroutine iotk_scan_dat_COMPLEX3_7
#endif
# 87 "iotk_dat_interf.spp"
#endif
# 70 "iotk_dat_interf.spp"
#ifdef __IOTK_COMPLEX4
# 72 "iotk_dat_interf.spp"
#if 0 <= __IOTK_MAXRANK
subroutine iotk_scan_dat_COMPLEX4_0(unit,name,dat,dummy,found,default,ierr)
  use iotk_base
  implicit none
  integer,                   intent(in)  :: unit
  character(*),              intent(in)  :: name
# 79 "iotk_dat_interf.spp"
  COMPLEX (kind=__IOTK_COMPLEX4),           intent(out) :: dat 
  type(iotk_dummytype), optional      :: dummy
  logical,         optional, intent(out) :: found
  COMPLEX (kind=__IOTK_COMPLEX4), optional, intent(in)  :: default 
  integer,         optional, intent(out) :: ierr
end subroutine iotk_scan_dat_COMPLEX4_0
#endif
# 72 "iotk_dat_interf.spp"
#if 1 <= __IOTK_MAXRANK
subroutine iotk_scan_dat_COMPLEX4_1(unit,name,dat,dummy,found,default,ierr)
  use iotk_base
  implicit none
  integer,                   intent(in)  :: unit
  character(*),              intent(in)  :: name
# 79 "iotk_dat_interf.spp"
  COMPLEX (kind=__IOTK_COMPLEX4),           intent(out) :: dat (:)
  type(iotk_dummytype), optional      :: dummy
  logical,         optional, intent(out) :: found
  COMPLEX (kind=__IOTK_COMPLEX4), optional, intent(in)  :: default (:)
  integer,         optional, intent(out) :: ierr
end subroutine iotk_scan_dat_COMPLEX4_1
#endif
# 72 "iotk_dat_interf.spp"
#if 2 <= __IOTK_MAXRANK
subroutine iotk_scan_dat_COMPLEX4_2(unit,name,dat,dummy,found,default,ierr)
  use iotk_base
  implicit none
  integer,                   intent(in)  :: unit
  character(*),              intent(in)  :: name
# 79 "iotk_dat_interf.spp"
  COMPLEX (kind=__IOTK_COMPLEX4),           intent(out) :: dat (:,:)
  type(iotk_dummytype), optional      :: dummy
  logical,         optional, intent(out) :: found
  COMPLEX (kind=__IOTK_COMPLEX4), optional, intent(in)  :: default (:,:)
  integer,         optional, intent(out) :: ierr
end subroutine iotk_scan_dat_COMPLEX4_2
#endif
# 72 "iotk_dat_interf.spp"
#if 3 <= __IOTK_MAXRANK
subroutine iotk_scan_dat_COMPLEX4_3(unit,name,dat,dummy,found,default,ierr)
  use iotk_base
  implicit none
  integer,                   intent(in)  :: unit
  character(*),              intent(in)  :: name
# 79 "iotk_dat_interf.spp"
  COMPLEX (kind=__IOTK_COMPLEX4),           intent(out) :: dat (:,:,:)
  type(iotk_dummytype), optional      :: dummy
  logical,         optional, intent(out) :: found
  COMPLEX (kind=__IOTK_COMPLEX4), optional, intent(in)  :: default (:,:,:)
  integer,         optional, intent(out) :: ierr
end subroutine iotk_scan_dat_COMPLEX4_3
#endif
# 72 "iotk_dat_interf.spp"
#if 4 <= __IOTK_MAXRANK
subroutine iotk_scan_dat_COMPLEX4_4(unit,name,dat,dummy,found,default,ierr)
  use iotk_base
  implicit none
  integer,                   intent(in)  :: unit
  character(*),              intent(in)  :: name
# 79 "iotk_dat_interf.spp"
  COMPLEX (kind=__IOTK_COMPLEX4),           intent(out) :: dat (:,:,:,:)
  type(iotk_dummytype), optional      :: dummy
  logical,         optional, intent(out) :: found
  COMPLEX (kind=__IOTK_COMPLEX4), optional, intent(in)  :: default (:,:,:,:)
  integer,         optional, intent(out) :: ierr
end subroutine iotk_scan_dat_COMPLEX4_4
#endif
# 72 "iotk_dat_interf.spp"
#if 5 <= __IOTK_MAXRANK
subroutine iotk_scan_dat_COMPLEX4_5(unit,name,dat,dummy,found,default,ierr)
  use iotk_base
  implicit none
  integer,                   intent(in)  :: unit
  character(*),              intent(in)  :: name
# 79 "iotk_dat_interf.spp"
  COMPLEX (kind=__IOTK_COMPLEX4),           intent(out) :: dat (:,:,:,:,:)
  type(iotk_dummytype), optional      :: dummy
  logical,         optional, intent(out) :: found
  COMPLEX (kind=__IOTK_COMPLEX4), optional, intent(in)  :: default (:,:,:,:,:)
  integer,         optional, intent(out) :: ierr
end subroutine iotk_scan_dat_COMPLEX4_5
#endif
# 72 "iotk_dat_interf.spp"
#if 6 <= __IOTK_MAXRANK
subroutine iotk_scan_dat_COMPLEX4_6(unit,name,dat,dummy,found,default,ierr)
  use iotk_base
  implicit none
  integer,                   intent(in)  :: unit
  character(*),              intent(in)  :: name
# 79 "iotk_dat_interf.spp"
  COMPLEX (kind=__IOTK_COMPLEX4),           intent(out) :: dat (:,:,:,:,:,:)
  type(iotk_dummytype), optional      :: dummy
  logical,         optional, intent(out) :: found
  COMPLEX (kind=__IOTK_COMPLEX4), optional, intent(in)  :: default (:,:,:,:,:,:)
  integer,         optional, intent(out) :: ierr
end subroutine iotk_scan_dat_COMPLEX4_6
#endif
# 72 "iotk_dat_interf.spp"
#if 7 <= __IOTK_MAXRANK
subroutine iotk_scan_dat_COMPLEX4_7(unit,name,dat,dummy,found,default,ierr)
  use iotk_base
  implicit none
  integer,                   intent(in)  :: unit
  character(*),              intent(in)  :: name
# 79 "iotk_dat_interf.spp"
  COMPLEX (kind=__IOTK_COMPLEX4),           intent(out) :: dat (:,:,:,:,:,:,:)
  type(iotk_dummytype), optional      :: dummy
  logical,         optional, intent(out) :: found
  COMPLEX (kind=__IOTK_COMPLEX4), optional, intent(in)  :: default (:,:,:,:,:,:,:)
  integer,         optional, intent(out) :: ierr
end subroutine iotk_scan_dat_COMPLEX4_7
#endif
# 87 "iotk_dat_interf.spp"
#endif
# 70 "iotk_dat_interf.spp"
#ifdef __IOTK_CHARACTER1
# 72 "iotk_dat_interf.spp"
#if 0 <= __IOTK_MAXRANK
subroutine iotk_scan_dat_CHARACTER1_0(unit,name,dat,dummy,found,default,ierr)
  use iotk_base
  implicit none
  integer,                   intent(in)  :: unit
  character(*),              intent(in)  :: name
# 79 "iotk_dat_interf.spp"
  CHARACTER (kind=__IOTK_CHARACTER1,len=*),           intent(out) :: dat 
  type(iotk_dummytype), optional      :: dummy
  logical,         optional, intent(out) :: found
  CHARACTER (kind=__IOTK_CHARACTER1,len=*), optional, intent(in)  :: default 
  integer,         optional, intent(out) :: ierr
end subroutine iotk_scan_dat_CHARACTER1_0
#endif
# 72 "iotk_dat_interf.spp"
#if 1 <= __IOTK_MAXRANK
subroutine iotk_scan_dat_CHARACTER1_1(unit,name,dat,dummy,found,default,ierr)
  use iotk_base
  implicit none
  integer,                   intent(in)  :: unit
  character(*),              intent(in)  :: name
# 79 "iotk_dat_interf.spp"
  CHARACTER (kind=__IOTK_CHARACTER1,len=*),           intent(out) :: dat (:)
  type(iotk_dummytype), optional      :: dummy
  logical,         optional, intent(out) :: found
  CHARACTER (kind=__IOTK_CHARACTER1,len=*), optional, intent(in)  :: default (:)
  integer,         optional, intent(out) :: ierr
end subroutine iotk_scan_dat_CHARACTER1_1
#endif
# 72 "iotk_dat_interf.spp"
#if 2 <= __IOTK_MAXRANK
subroutine iotk_scan_dat_CHARACTER1_2(unit,name,dat,dummy,found,default,ierr)
  use iotk_base
  implicit none
  integer,                   intent(in)  :: unit
  character(*),              intent(in)  :: name
# 79 "iotk_dat_interf.spp"
  CHARACTER (kind=__IOTK_CHARACTER1,len=*),           intent(out) :: dat (:,:)
  type(iotk_dummytype), optional      :: dummy
  logical,         optional, intent(out) :: found
  CHARACTER (kind=__IOTK_CHARACTER1,len=*), optional, intent(in)  :: default (:,:)
  integer,         optional, intent(out) :: ierr
end subroutine iotk_scan_dat_CHARACTER1_2
#endif
# 72 "iotk_dat_interf.spp"
#if 3 <= __IOTK_MAXRANK
subroutine iotk_scan_dat_CHARACTER1_3(unit,name,dat,dummy,found,default,ierr)
  use iotk_base
  implicit none
  integer,                   intent(in)  :: unit
  character(*),              intent(in)  :: name
# 79 "iotk_dat_interf.spp"
  CHARACTER (kind=__IOTK_CHARACTER1,len=*),           intent(out) :: dat (:,:,:)
  type(iotk_dummytype), optional      :: dummy
  logical,         optional, intent(out) :: found
  CHARACTER (kind=__IOTK_CHARACTER1,len=*), optional, intent(in)  :: default (:,:,:)
  integer,         optional, intent(out) :: ierr
end subroutine iotk_scan_dat_CHARACTER1_3
#endif
# 72 "iotk_dat_interf.spp"
#if 4 <= __IOTK_MAXRANK
subroutine iotk_scan_dat_CHARACTER1_4(unit,name,dat,dummy,found,default,ierr)
  use iotk_base
  implicit none
  integer,                   intent(in)  :: unit
  character(*),              intent(in)  :: name
# 79 "iotk_dat_interf.spp"
  CHARACTER (kind=__IOTK_CHARACTER1,len=*),           intent(out) :: dat (:,:,:,:)
  type(iotk_dummytype), optional      :: dummy
  logical,         optional, intent(out) :: found
  CHARACTER (kind=__IOTK_CHARACTER1,len=*), optional, intent(in)  :: default (:,:,:,:)
  integer,         optional, intent(out) :: ierr
end subroutine iotk_scan_dat_CHARACTER1_4
#endif
# 72 "iotk_dat_interf.spp"
#if 5 <= __IOTK_MAXRANK
subroutine iotk_scan_dat_CHARACTER1_5(unit,name,dat,dummy,found,default,ierr)
  use iotk_base
  implicit none
  integer,                   intent(in)  :: unit
  character(*),              intent(in)  :: name
# 79 "iotk_dat_interf.spp"
  CHARACTER (kind=__IOTK_CHARACTER1,len=*),           intent(out) :: dat (:,:,:,:,:)
  type(iotk_dummytype), optional      :: dummy
  logical,         optional, intent(out) :: found
  CHARACTER (kind=__IOTK_CHARACTER1,len=*), optional, intent(in)  :: default (:,:,:,:,:)
  integer,         optional, intent(out) :: ierr
end subroutine iotk_scan_dat_CHARACTER1_5
#endif
# 72 "iotk_dat_interf.spp"
#if 6 <= __IOTK_MAXRANK
subroutine iotk_scan_dat_CHARACTER1_6(unit,name,dat,dummy,found,default,ierr)
  use iotk_base
  implicit none
  integer,                   intent(in)  :: unit
  character(*),              intent(in)  :: name
# 79 "iotk_dat_interf.spp"
  CHARACTER (kind=__IOTK_CHARACTER1,len=*),           intent(out) :: dat (:,:,:,:,:,:)
  type(iotk_dummytype), optional      :: dummy
  logical,         optional, intent(out) :: found
  CHARACTER (kind=__IOTK_CHARACTER1,len=*), optional, intent(in)  :: default (:,:,:,:,:,:)
  integer,         optional, intent(out) :: ierr
end subroutine iotk_scan_dat_CHARACTER1_6
#endif
# 72 "iotk_dat_interf.spp"
#if 7 <= __IOTK_MAXRANK
subroutine iotk_scan_dat_CHARACTER1_7(unit,name,dat,dummy,found,default,ierr)
  use iotk_base
  implicit none
  integer,                   intent(in)  :: unit
  character(*),              intent(in)  :: name
# 79 "iotk_dat_interf.spp"
  CHARACTER (kind=__IOTK_CHARACTER1,len=*),           intent(out) :: dat (:,:,:,:,:,:,:)
  type(iotk_dummytype), optional      :: dummy
  logical,         optional, intent(out) :: found
  CHARACTER (kind=__IOTK_CHARACTER1,len=*), optional, intent(in)  :: default (:,:,:,:,:,:,:)
  integer,         optional, intent(out) :: ierr
end subroutine iotk_scan_dat_CHARACTER1_7
#endif
# 87 "iotk_dat_interf.spp"
#endif
# 91 "iotk_dat_interf.spp"
end interface

interface iotk_scan_dat_aux
# 97 "iotk_dat_interf.spp"
#ifdef __IOTK_LOGICAL1
subroutine iotk_scan_dat_aux_LOGICAL1(unit,dat,rkind,rlen,fmt,ierr)
  use iotk_base
  implicit none
  integer,         intent(in)  :: unit
# 103 "iotk_dat_interf.spp"
  LOGICAL (kind=__IOTK_LOGICAL1), intent(out) :: dat(:)
  integer,         intent(in)  :: rkind
  integer,         intent(in)  :: rlen
  character(*),    intent(in)  :: fmt
  integer,         intent(out) :: ierr
end subroutine iotk_scan_dat_aux_LOGICAL1
#endif
# 97 "iotk_dat_interf.spp"
#ifdef __IOTK_LOGICAL2
subroutine iotk_scan_dat_aux_LOGICAL2(unit,dat,rkind,rlen,fmt,ierr)
  use iotk_base
  implicit none
  integer,         intent(in)  :: unit
# 103 "iotk_dat_interf.spp"
  LOGICAL (kind=__IOTK_LOGICAL2), intent(out) :: dat(:)
  integer,         intent(in)  :: rkind
  integer,         intent(in)  :: rlen
  character(*),    intent(in)  :: fmt
  integer,         intent(out) :: ierr
end subroutine iotk_scan_dat_aux_LOGICAL2
#endif
# 97 "iotk_dat_interf.spp"
#ifdef __IOTK_LOGICAL3
subroutine iotk_scan_dat_aux_LOGICAL3(unit,dat,rkind,rlen,fmt,ierr)
  use iotk_base
  implicit none
  integer,         intent(in)  :: unit
# 103 "iotk_dat_interf.spp"
  LOGICAL (kind=__IOTK_LOGICAL3), intent(out) :: dat(:)
  integer,         intent(in)  :: rkind
  integer,         intent(in)  :: rlen
  character(*),    intent(in)  :: fmt
  integer,         intent(out) :: ierr
end subroutine iotk_scan_dat_aux_LOGICAL3
#endif
# 97 "iotk_dat_interf.spp"
#ifdef __IOTK_LOGICAL4
subroutine iotk_scan_dat_aux_LOGICAL4(unit,dat,rkind,rlen,fmt,ierr)
  use iotk_base
  implicit none
  integer,         intent(in)  :: unit
# 103 "iotk_dat_interf.spp"
  LOGICAL (kind=__IOTK_LOGICAL4), intent(out) :: dat(:)
  integer,         intent(in)  :: rkind
  integer,         intent(in)  :: rlen
  character(*),    intent(in)  :: fmt
  integer,         intent(out) :: ierr
end subroutine iotk_scan_dat_aux_LOGICAL4
#endif
# 97 "iotk_dat_interf.spp"
#ifdef __IOTK_INTEGER1
subroutine iotk_scan_dat_aux_INTEGER1(unit,dat,rkind,rlen,fmt,ierr)
  use iotk_base
  implicit none
  integer,         intent(in)  :: unit
# 103 "iotk_dat_interf.spp"
  INTEGER (kind=__IOTK_INTEGER1), intent(out) :: dat(:)
  integer,         intent(in)  :: rkind
  integer,         intent(in)  :: rlen
  character(*),    intent(in)  :: fmt
  integer,         intent(out) :: ierr
end subroutine iotk_scan_dat_aux_INTEGER1
#endif
# 97 "iotk_dat_interf.spp"
#ifdef __IOTK_INTEGER2
subroutine iotk_scan_dat_aux_INTEGER2(unit,dat,rkind,rlen,fmt,ierr)
  use iotk_base
  implicit none
  integer,         intent(in)  :: unit
# 103 "iotk_dat_interf.spp"
  INTEGER (kind=__IOTK_INTEGER2), intent(out) :: dat(:)
  integer,         intent(in)  :: rkind
  integer,         intent(in)  :: rlen
  character(*),    intent(in)  :: fmt
  integer,         intent(out) :: ierr
end subroutine iotk_scan_dat_aux_INTEGER2
#endif
# 97 "iotk_dat_interf.spp"
#ifdef __IOTK_INTEGER3
subroutine iotk_scan_dat_aux_INTEGER3(unit,dat,rkind,rlen,fmt,ierr)
  use iotk_base
  implicit none
  integer,         intent(in)  :: unit
# 103 "iotk_dat_interf.spp"
  INTEGER (kind=__IOTK_INTEGER3), intent(out) :: dat(:)
  integer,         intent(in)  :: rkind
  integer,         intent(in)  :: rlen
  character(*),    intent(in)  :: fmt
  integer,         intent(out) :: ierr
end subroutine iotk_scan_dat_aux_INTEGER3
#endif
# 97 "iotk_dat_interf.spp"
#ifdef __IOTK_INTEGER4
subroutine iotk_scan_dat_aux_INTEGER4(unit,dat,rkind,rlen,fmt,ierr)
  use iotk_base
  implicit none
  integer,         intent(in)  :: unit
# 103 "iotk_dat_interf.spp"
  INTEGER (kind=__IOTK_INTEGER4), intent(out) :: dat(:)
  integer,         intent(in)  :: rkind
  integer,         intent(in)  :: rlen
  character(*),    intent(in)  :: fmt
  integer,         intent(out) :: ierr
end subroutine iotk_scan_dat_aux_INTEGER4
#endif
# 97 "iotk_dat_interf.spp"
#ifdef __IOTK_REAL1
subroutine iotk_scan_dat_aux_REAL1(unit,dat,rkind,rlen,fmt,ierr)
  use iotk_base
  implicit none
  integer,         intent(in)  :: unit
# 103 "iotk_dat_interf.spp"
  REAL (kind=__IOTK_REAL1), intent(out) :: dat(:)
  integer,         intent(in)  :: rkind
  integer,         intent(in)  :: rlen
  character(*),    intent(in)  :: fmt
  integer,         intent(out) :: ierr
end subroutine iotk_scan_dat_aux_REAL1
#endif
# 97 "iotk_dat_interf.spp"
#ifdef __IOTK_REAL2
subroutine iotk_scan_dat_aux_REAL2(unit,dat,rkind,rlen,fmt,ierr)
  use iotk_base
  implicit none
  integer,         intent(in)  :: unit
# 103 "iotk_dat_interf.spp"
  REAL (kind=__IOTK_REAL2), intent(out) :: dat(:)
  integer,         intent(in)  :: rkind
  integer,         intent(in)  :: rlen
  character(*),    intent(in)  :: fmt
  integer,         intent(out) :: ierr
end subroutine iotk_scan_dat_aux_REAL2
#endif
# 97 "iotk_dat_interf.spp"
#ifdef __IOTK_REAL3
subroutine iotk_scan_dat_aux_REAL3(unit,dat,rkind,rlen,fmt,ierr)
  use iotk_base
  implicit none
  integer,         intent(in)  :: unit
# 103 "iotk_dat_interf.spp"
  REAL (kind=__IOTK_REAL3), intent(out) :: dat(:)
  integer,         intent(in)  :: rkind
  integer,         intent(in)  :: rlen
  character(*),    intent(in)  :: fmt
  integer,         intent(out) :: ierr
end subroutine iotk_scan_dat_aux_REAL3
#endif
# 97 "iotk_dat_interf.spp"
#ifdef __IOTK_REAL4
subroutine iotk_scan_dat_aux_REAL4(unit,dat,rkind,rlen,fmt,ierr)
  use iotk_base
  implicit none
  integer,         intent(in)  :: unit
# 103 "iotk_dat_interf.spp"
  REAL (kind=__IOTK_REAL4), intent(out) :: dat(:)
  integer,         intent(in)  :: rkind
  integer,         intent(in)  :: rlen
  character(*),    intent(in)  :: fmt
  integer,         intent(out) :: ierr
end subroutine iotk_scan_dat_aux_REAL4
#endif
# 97 "iotk_dat_interf.spp"
#ifdef __IOTK_COMPLEX1
subroutine iotk_scan_dat_aux_COMPLEX1(unit,dat,rkind,rlen,fmt,ierr)
  use iotk_base
  implicit none
  integer,         intent(in)  :: unit
# 103 "iotk_dat_interf.spp"
  COMPLEX (kind=__IOTK_COMPLEX1), intent(out) :: dat(:)
  integer,         intent(in)  :: rkind
  integer,         intent(in)  :: rlen
  character(*),    intent(in)  :: fmt
  integer,         intent(out) :: ierr
end subroutine iotk_scan_dat_aux_COMPLEX1
#endif
# 97 "iotk_dat_interf.spp"
#ifdef __IOTK_COMPLEX2
subroutine iotk_scan_dat_aux_COMPLEX2(unit,dat,rkind,rlen,fmt,ierr)
  use iotk_base
  implicit none
  integer,         intent(in)  :: unit
# 103 "iotk_dat_interf.spp"
  COMPLEX (kind=__IOTK_COMPLEX2), intent(out) :: dat(:)
  integer,         intent(in)  :: rkind
  integer,         intent(in)  :: rlen
  character(*),    intent(in)  :: fmt
  integer,         intent(out) :: ierr
end subroutine iotk_scan_dat_aux_COMPLEX2
#endif
# 97 "iotk_dat_interf.spp"
#ifdef __IOTK_COMPLEX3
subroutine iotk_scan_dat_aux_COMPLEX3(unit,dat,rkind,rlen,fmt,ierr)
  use iotk_base
  implicit none
  integer,         intent(in)  :: unit
# 103 "iotk_dat_interf.spp"
  COMPLEX (kind=__IOTK_COMPLEX3), intent(out) :: dat(:)
  integer,         intent(in)  :: rkind
  integer,         intent(in)  :: rlen
  character(*),    intent(in)  :: fmt
  integer,         intent(out) :: ierr
end subroutine iotk_scan_dat_aux_COMPLEX3
#endif
# 97 "iotk_dat_interf.spp"
#ifdef __IOTK_COMPLEX4
subroutine iotk_scan_dat_aux_COMPLEX4(unit,dat,rkind,rlen,fmt,ierr)
  use iotk_base
  implicit none
  integer,         intent(in)  :: unit
# 103 "iotk_dat_interf.spp"
  COMPLEX (kind=__IOTK_COMPLEX4), intent(out) :: dat(:)
  integer,         intent(in)  :: rkind
  integer,         intent(in)  :: rlen
  character(*),    intent(in)  :: fmt
  integer,         intent(out) :: ierr
end subroutine iotk_scan_dat_aux_COMPLEX4
#endif
# 97 "iotk_dat_interf.spp"
#ifdef __IOTK_CHARACTER1
subroutine iotk_scan_dat_aux_CHARACTER1(unit,dat,rkind,rlen,fmt,ierr)
  use iotk_base
  implicit none
  integer,         intent(in)  :: unit
# 103 "iotk_dat_interf.spp"
  CHARACTER (kind=__IOTK_CHARACTER1,len=*), intent(out) :: dat(:)
  integer,         intent(in)  :: rkind
  integer,         intent(in)  :: rlen
  character(*),    intent(in)  :: fmt
  integer,         intent(out) :: ierr
end subroutine iotk_scan_dat_aux_CHARACTER1
#endif
# 113 "iotk_dat_interf.spp"
end interface

end module iotk_dat_interf
