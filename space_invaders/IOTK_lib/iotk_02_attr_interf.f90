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
# 46 "../include/iotk_auxmacros.spp"
#ifndef __IOTK_LOGICAL1
# 46 "../include/iotk_auxmacros.spp"
#ifndef __IOTK_LOGICAL2
# 46 "../include/iotk_auxmacros.spp"
#ifndef __IOTK_LOGICAL3
# 46 "../include/iotk_auxmacros.spp"
#ifndef __IOTK_LOGICAL4
# 48 "../include/iotk_auxmacros.spp"
#define __IOTK_LOGICAL1 iotk_defkind_LOGICAL
# 51 "../include/iotk_auxmacros.spp"
#endif
# 51 "../include/iotk_auxmacros.spp"
#endif
# 51 "../include/iotk_auxmacros.spp"
#endif
# 51 "../include/iotk_auxmacros.spp"
#endif
# 46 "../include/iotk_auxmacros.spp"
#ifndef __IOTK_INTEGER1
# 46 "../include/iotk_auxmacros.spp"
#ifndef __IOTK_INTEGER2
# 46 "../include/iotk_auxmacros.spp"
#ifndef __IOTK_INTEGER3
# 46 "../include/iotk_auxmacros.spp"
#ifndef __IOTK_INTEGER4
# 48 "../include/iotk_auxmacros.spp"
#define __IOTK_INTEGER1 iotk_defkind_INTEGER
# 51 "../include/iotk_auxmacros.spp"
#endif
# 51 "../include/iotk_auxmacros.spp"
#endif
# 51 "../include/iotk_auxmacros.spp"
#endif
# 51 "../include/iotk_auxmacros.spp"
#endif
# 46 "../include/iotk_auxmacros.spp"
#ifndef __IOTK_REAL1
# 46 "../include/iotk_auxmacros.spp"
#ifndef __IOTK_REAL2
# 46 "../include/iotk_auxmacros.spp"
#ifndef __IOTK_REAL3
# 46 "../include/iotk_auxmacros.spp"
#ifndef __IOTK_REAL4
# 48 "../include/iotk_auxmacros.spp"
#define __IOTK_REAL1 iotk_defkind_REAL
# 51 "../include/iotk_auxmacros.spp"
#endif
# 51 "../include/iotk_auxmacros.spp"
#endif
# 51 "../include/iotk_auxmacros.spp"
#endif
# 51 "../include/iotk_auxmacros.spp"
#endif
# 54 "../include/iotk_auxmacros.spp"
! Complex are treated indentically to reals
! These lines map the definitions.
# 57 "../include/iotk_auxmacros.spp"
#ifdef __IOTK_REAL1
#define __IOTK_COMPLEX1 __IOTK_REAL1
#else
#undef __IOTK_COMPLEX1
#endif
# 57 "../include/iotk_auxmacros.spp"
#ifdef __IOTK_REAL2
#define __IOTK_COMPLEX2 __IOTK_REAL2
#else
#undef __IOTK_COMPLEX2
#endif
# 57 "../include/iotk_auxmacros.spp"
#ifdef __IOTK_REAL3
#define __IOTK_COMPLEX3 __IOTK_REAL3
#else
#undef __IOTK_COMPLEX3
#endif
# 57 "../include/iotk_auxmacros.spp"
#ifdef __IOTK_REAL4
#define __IOTK_COMPLEX4 __IOTK_REAL4
#else
#undef __IOTK_COMPLEX4
#endif
# 63 "../include/iotk_auxmacros.spp"
! If the binary format is not defined, use *
#ifndef __IOTK_BINARY_FORMAT
#define __IOTK_BINARY_FORMAT "*"
#endif

! Some check 
#if __IOTK_MAXRANK > 7
#  error
#endif
#if __IOTK_MAXRANK < 1
#  error
#endif

#endif

# 30 "iotk_attr_interf.spp"

module iotk_attr_interf
implicit none
private

public :: iotk_read
public :: iotk_write
public :: iotk_write_attr
public :: iotk_scan_attr


interface iotk_read
# 45 "iotk_attr_interf.spp"
#ifdef __IOTK_LOGICAL1
subroutine iotk_read_LOGICAL1(val,string,index,ierr)
  use iotk_base
  implicit none
  LOGICAL(kind=__IOTK_LOGICAL1), intent(inout) :: val(:)
  character(len=*),                    intent(in)    :: string
  integer,                             intent(inout) :: index
  integer,                             intent(out) :: ierr
end subroutine iotk_read_LOGICAL1
#endif
# 45 "iotk_attr_interf.spp"
#ifdef __IOTK_LOGICAL2
subroutine iotk_read_LOGICAL2(val,string,index,ierr)
  use iotk_base
  implicit none
  LOGICAL(kind=__IOTK_LOGICAL2), intent(inout) :: val(:)
  character(len=*),                    intent(in)    :: string
  integer,                             intent(inout) :: index
  integer,                             intent(out) :: ierr
end subroutine iotk_read_LOGICAL2
#endif
# 45 "iotk_attr_interf.spp"
#ifdef __IOTK_LOGICAL3
subroutine iotk_read_LOGICAL3(val,string,index,ierr)
  use iotk_base
  implicit none
  LOGICAL(kind=__IOTK_LOGICAL3), intent(inout) :: val(:)
  character(len=*),                    intent(in)    :: string
  integer,                             intent(inout) :: index
  integer,                             intent(out) :: ierr
end subroutine iotk_read_LOGICAL3
#endif
# 45 "iotk_attr_interf.spp"
#ifdef __IOTK_LOGICAL4
subroutine iotk_read_LOGICAL4(val,string,index,ierr)
  use iotk_base
  implicit none
  LOGICAL(kind=__IOTK_LOGICAL4), intent(inout) :: val(:)
  character(len=*),                    intent(in)    :: string
  integer,                             intent(inout) :: index
  integer,                             intent(out) :: ierr
end subroutine iotk_read_LOGICAL4
#endif
# 45 "iotk_attr_interf.spp"
#ifdef __IOTK_INTEGER1
subroutine iotk_read_INTEGER1(val,string,index,ierr)
  use iotk_base
  implicit none
  INTEGER(kind=__IOTK_INTEGER1), intent(inout) :: val(:)
  character(len=*),                    intent(in)    :: string
  integer,                             intent(inout) :: index
  integer,                             intent(out) :: ierr
end subroutine iotk_read_INTEGER1
#endif
# 45 "iotk_attr_interf.spp"
#ifdef __IOTK_INTEGER2
subroutine iotk_read_INTEGER2(val,string,index,ierr)
  use iotk_base
  implicit none
  INTEGER(kind=__IOTK_INTEGER2), intent(inout) :: val(:)
  character(len=*),                    intent(in)    :: string
  integer,                             intent(inout) :: index
  integer,                             intent(out) :: ierr
end subroutine iotk_read_INTEGER2
#endif
# 45 "iotk_attr_interf.spp"
#ifdef __IOTK_INTEGER3
subroutine iotk_read_INTEGER3(val,string,index,ierr)
  use iotk_base
  implicit none
  INTEGER(kind=__IOTK_INTEGER3), intent(inout) :: val(:)
  character(len=*),                    intent(in)    :: string
  integer,                             intent(inout) :: index
  integer,                             intent(out) :: ierr
end subroutine iotk_read_INTEGER3
#endif
# 45 "iotk_attr_interf.spp"
#ifdef __IOTK_INTEGER4
subroutine iotk_read_INTEGER4(val,string,index,ierr)
  use iotk_base
  implicit none
  INTEGER(kind=__IOTK_INTEGER4), intent(inout) :: val(:)
  character(len=*),                    intent(in)    :: string
  integer,                             intent(inout) :: index
  integer,                             intent(out) :: ierr
end subroutine iotk_read_INTEGER4
#endif
# 45 "iotk_attr_interf.spp"
#ifdef __IOTK_REAL1
subroutine iotk_read_REAL1(val,string,index,ierr)
  use iotk_base
  implicit none
  REAL(kind=__IOTK_REAL1), intent(inout) :: val(:)
  character(len=*),                    intent(in)    :: string
  integer,                             intent(inout) :: index
  integer,                             intent(out) :: ierr
end subroutine iotk_read_REAL1
#endif
# 45 "iotk_attr_interf.spp"
#ifdef __IOTK_REAL2
subroutine iotk_read_REAL2(val,string,index,ierr)
  use iotk_base
  implicit none
  REAL(kind=__IOTK_REAL2), intent(inout) :: val(:)
  character(len=*),                    intent(in)    :: string
  integer,                             intent(inout) :: index
  integer,                             intent(out) :: ierr
end subroutine iotk_read_REAL2
#endif
# 45 "iotk_attr_interf.spp"
#ifdef __IOTK_REAL3
subroutine iotk_read_REAL3(val,string,index,ierr)
  use iotk_base
  implicit none
  REAL(kind=__IOTK_REAL3), intent(inout) :: val(:)
  character(len=*),                    intent(in)    :: string
  integer,                             intent(inout) :: index
  integer,                             intent(out) :: ierr
end subroutine iotk_read_REAL3
#endif
# 45 "iotk_attr_interf.spp"
#ifdef __IOTK_REAL4
subroutine iotk_read_REAL4(val,string,index,ierr)
  use iotk_base
  implicit none
  REAL(kind=__IOTK_REAL4), intent(inout) :: val(:)
  character(len=*),                    intent(in)    :: string
  integer,                             intent(inout) :: index
  integer,                             intent(out) :: ierr
end subroutine iotk_read_REAL4
#endif
# 45 "iotk_attr_interf.spp"
#ifdef __IOTK_COMPLEX1
subroutine iotk_read_COMPLEX1(val,string,index,ierr)
  use iotk_base
  implicit none
  COMPLEX(kind=__IOTK_COMPLEX1), intent(inout) :: val(:)
  character(len=*),                    intent(in)    :: string
  integer,                             intent(inout) :: index
  integer,                             intent(out) :: ierr
end subroutine iotk_read_COMPLEX1
#endif
# 45 "iotk_attr_interf.spp"
#ifdef __IOTK_COMPLEX2
subroutine iotk_read_COMPLEX2(val,string,index,ierr)
  use iotk_base
  implicit none
  COMPLEX(kind=__IOTK_COMPLEX2), intent(inout) :: val(:)
  character(len=*),                    intent(in)    :: string
  integer,                             intent(inout) :: index
  integer,                             intent(out) :: ierr
end subroutine iotk_read_COMPLEX2
#endif
# 45 "iotk_attr_interf.spp"
#ifdef __IOTK_COMPLEX3
subroutine iotk_read_COMPLEX3(val,string,index,ierr)
  use iotk_base
  implicit none
  COMPLEX(kind=__IOTK_COMPLEX3), intent(inout) :: val(:)
  character(len=*),                    intent(in)    :: string
  integer,                             intent(inout) :: index
  integer,                             intent(out) :: ierr
end subroutine iotk_read_COMPLEX3
#endif
# 45 "iotk_attr_interf.spp"
#ifdef __IOTK_COMPLEX4
subroutine iotk_read_COMPLEX4(val,string,index,ierr)
  use iotk_base
  implicit none
  COMPLEX(kind=__IOTK_COMPLEX4), intent(inout) :: val(:)
  character(len=*),                    intent(in)    :: string
  integer,                             intent(inout) :: index
  integer,                             intent(out) :: ierr
end subroutine iotk_read_COMPLEX4
#endif
# 58 "iotk_attr_interf.spp"
end interface

interface iotk_write
# 64 "iotk_attr_interf.spp"
#ifdef __IOTK_LOGICAL1
subroutine iotk_write_LOGICAL1(val,string,ierr)
  use iotk_base
  implicit none
  LOGICAL(kind=__IOTK_LOGICAL1), intent(in)  :: val(:)
  character(len=*),                    intent(out) :: string
  integer,                             intent(out) :: ierr
end subroutine iotk_write_LOGICAL1
#endif
# 64 "iotk_attr_interf.spp"
#ifdef __IOTK_LOGICAL2
subroutine iotk_write_LOGICAL2(val,string,ierr)
  use iotk_base
  implicit none
  LOGICAL(kind=__IOTK_LOGICAL2), intent(in)  :: val(:)
  character(len=*),                    intent(out) :: string
  integer,                             intent(out) :: ierr
end subroutine iotk_write_LOGICAL2
#endif
# 64 "iotk_attr_interf.spp"
#ifdef __IOTK_LOGICAL3
subroutine iotk_write_LOGICAL3(val,string,ierr)
  use iotk_base
  implicit none
  LOGICAL(kind=__IOTK_LOGICAL3), intent(in)  :: val(:)
  character(len=*),                    intent(out) :: string
  integer,                             intent(out) :: ierr
end subroutine iotk_write_LOGICAL3
#endif
# 64 "iotk_attr_interf.spp"
#ifdef __IOTK_LOGICAL4
subroutine iotk_write_LOGICAL4(val,string,ierr)
  use iotk_base
  implicit none
  LOGICAL(kind=__IOTK_LOGICAL4), intent(in)  :: val(:)
  character(len=*),                    intent(out) :: string
  integer,                             intent(out) :: ierr
end subroutine iotk_write_LOGICAL4
#endif
# 64 "iotk_attr_interf.spp"
#ifdef __IOTK_INTEGER1
subroutine iotk_write_INTEGER1(val,string,ierr)
  use iotk_base
  implicit none
  INTEGER(kind=__IOTK_INTEGER1), intent(in)  :: val(:)
  character(len=*),                    intent(out) :: string
  integer,                             intent(out) :: ierr
end subroutine iotk_write_INTEGER1
#endif
# 64 "iotk_attr_interf.spp"
#ifdef __IOTK_INTEGER2
subroutine iotk_write_INTEGER2(val,string,ierr)
  use iotk_base
  implicit none
  INTEGER(kind=__IOTK_INTEGER2), intent(in)  :: val(:)
  character(len=*),                    intent(out) :: string
  integer,                             intent(out) :: ierr
end subroutine iotk_write_INTEGER2
#endif
# 64 "iotk_attr_interf.spp"
#ifdef __IOTK_INTEGER3
subroutine iotk_write_INTEGER3(val,string,ierr)
  use iotk_base
  implicit none
  INTEGER(kind=__IOTK_INTEGER3), intent(in)  :: val(:)
  character(len=*),                    intent(out) :: string
  integer,                             intent(out) :: ierr
end subroutine iotk_write_INTEGER3
#endif
# 64 "iotk_attr_interf.spp"
#ifdef __IOTK_INTEGER4
subroutine iotk_write_INTEGER4(val,string,ierr)
  use iotk_base
  implicit none
  INTEGER(kind=__IOTK_INTEGER4), intent(in)  :: val(:)
  character(len=*),                    intent(out) :: string
  integer,                             intent(out) :: ierr
end subroutine iotk_write_INTEGER4
#endif
# 64 "iotk_attr_interf.spp"
#ifdef __IOTK_REAL1
subroutine iotk_write_REAL1(val,string,ierr)
  use iotk_base
  implicit none
  REAL(kind=__IOTK_REAL1), intent(in)  :: val(:)
  character(len=*),                    intent(out) :: string
  integer,                             intent(out) :: ierr
end subroutine iotk_write_REAL1
#endif
# 64 "iotk_attr_interf.spp"
#ifdef __IOTK_REAL2
subroutine iotk_write_REAL2(val,string,ierr)
  use iotk_base
  implicit none
  REAL(kind=__IOTK_REAL2), intent(in)  :: val(:)
  character(len=*),                    intent(out) :: string
  integer,                             intent(out) :: ierr
end subroutine iotk_write_REAL2
#endif
# 64 "iotk_attr_interf.spp"
#ifdef __IOTK_REAL3
subroutine iotk_write_REAL3(val,string,ierr)
  use iotk_base
  implicit none
  REAL(kind=__IOTK_REAL3), intent(in)  :: val(:)
  character(len=*),                    intent(out) :: string
  integer,                             intent(out) :: ierr
end subroutine iotk_write_REAL3
#endif
# 64 "iotk_attr_interf.spp"
#ifdef __IOTK_REAL4
subroutine iotk_write_REAL4(val,string,ierr)
  use iotk_base
  implicit none
  REAL(kind=__IOTK_REAL4), intent(in)  :: val(:)
  character(len=*),                    intent(out) :: string
  integer,                             intent(out) :: ierr
end subroutine iotk_write_REAL4
#endif
# 64 "iotk_attr_interf.spp"
#ifdef __IOTK_COMPLEX1
subroutine iotk_write_COMPLEX1(val,string,ierr)
  use iotk_base
  implicit none
  COMPLEX(kind=__IOTK_COMPLEX1), intent(in)  :: val(:)
  character(len=*),                    intent(out) :: string
  integer,                             intent(out) :: ierr
end subroutine iotk_write_COMPLEX1
#endif
# 64 "iotk_attr_interf.spp"
#ifdef __IOTK_COMPLEX2
subroutine iotk_write_COMPLEX2(val,string,ierr)
  use iotk_base
  implicit none
  COMPLEX(kind=__IOTK_COMPLEX2), intent(in)  :: val(:)
  character(len=*),                    intent(out) :: string
  integer,                             intent(out) :: ierr
end subroutine iotk_write_COMPLEX2
#endif
# 64 "iotk_attr_interf.spp"
#ifdef __IOTK_COMPLEX3
subroutine iotk_write_COMPLEX3(val,string,ierr)
  use iotk_base
  implicit none
  COMPLEX(kind=__IOTK_COMPLEX3), intent(in)  :: val(:)
  character(len=*),                    intent(out) :: string
  integer,                             intent(out) :: ierr
end subroutine iotk_write_COMPLEX3
#endif
# 64 "iotk_attr_interf.spp"
#ifdef __IOTK_COMPLEX4
subroutine iotk_write_COMPLEX4(val,string,ierr)
  use iotk_base
  implicit none
  COMPLEX(kind=__IOTK_COMPLEX4), intent(in)  :: val(:)
  character(len=*),                    intent(out) :: string
  integer,                             intent(out) :: ierr
end subroutine iotk_write_COMPLEX4
#endif
# 76 "iotk_attr_interf.spp"
end interface

interface iotk_write_attr
# 82 "iotk_attr_interf.spp"
#ifdef __IOTK_LOGICAL1
# 85 "iotk_attr_interf.spp"
#if 0 <= __IOTK_MAXRANK
subroutine iotk_write_attr_LOGICAL1_0(attr,name,val,first,ierr)
  use iotk_base
  implicit none
  character(*), intent(inout) :: attr
  character(*), intent(in)    :: name
# 92 "iotk_attr_interf.spp"
  LOGICAL(kind=__IOTK_LOGICAL1), intent(in)  :: val 
  logical, optional, intent(in)  :: first
  integer, optional, intent(out) :: ierr
end subroutine iotk_write_attr_LOGICAL1_0
#endif
# 85 "iotk_attr_interf.spp"
#if 1 <= __IOTK_MAXRANK
subroutine iotk_write_attr_LOGICAL1_1(attr,name,val,first,ierr)
  use iotk_base
  implicit none
  character(*), intent(inout) :: attr
  character(*), intent(in)    :: name
# 92 "iotk_attr_interf.spp"
  LOGICAL(kind=__IOTK_LOGICAL1), intent(in)  :: val (:)
  logical, optional, intent(in)  :: first
  integer, optional, intent(out) :: ierr
end subroutine iotk_write_attr_LOGICAL1_1
#endif
# 85 "iotk_attr_interf.spp"
#if 2 <= __IOTK_MAXRANK
subroutine iotk_write_attr_LOGICAL1_2(attr,name,val,first,ierr)
  use iotk_base
  implicit none
  character(*), intent(inout) :: attr
  character(*), intent(in)    :: name
# 92 "iotk_attr_interf.spp"
  LOGICAL(kind=__IOTK_LOGICAL1), intent(in)  :: val (:,:)
  logical, optional, intent(in)  :: first
  integer, optional, intent(out) :: ierr
end subroutine iotk_write_attr_LOGICAL1_2
#endif
# 85 "iotk_attr_interf.spp"
#if 3 <= __IOTK_MAXRANK
subroutine iotk_write_attr_LOGICAL1_3(attr,name,val,first,ierr)
  use iotk_base
  implicit none
  character(*), intent(inout) :: attr
  character(*), intent(in)    :: name
# 92 "iotk_attr_interf.spp"
  LOGICAL(kind=__IOTK_LOGICAL1), intent(in)  :: val (:,:,:)
  logical, optional, intent(in)  :: first
  integer, optional, intent(out) :: ierr
end subroutine iotk_write_attr_LOGICAL1_3
#endif
# 85 "iotk_attr_interf.spp"
#if 4 <= __IOTK_MAXRANK
subroutine iotk_write_attr_LOGICAL1_4(attr,name,val,first,ierr)
  use iotk_base
  implicit none
  character(*), intent(inout) :: attr
  character(*), intent(in)    :: name
# 92 "iotk_attr_interf.spp"
  LOGICAL(kind=__IOTK_LOGICAL1), intent(in)  :: val (:,:,:,:)
  logical, optional, intent(in)  :: first
  integer, optional, intent(out) :: ierr
end subroutine iotk_write_attr_LOGICAL1_4
#endif
# 85 "iotk_attr_interf.spp"
#if 5 <= __IOTK_MAXRANK
subroutine iotk_write_attr_LOGICAL1_5(attr,name,val,first,ierr)
  use iotk_base
  implicit none
  character(*), intent(inout) :: attr
  character(*), intent(in)    :: name
# 92 "iotk_attr_interf.spp"
  LOGICAL(kind=__IOTK_LOGICAL1), intent(in)  :: val (:,:,:,:,:)
  logical, optional, intent(in)  :: first
  integer, optional, intent(out) :: ierr
end subroutine iotk_write_attr_LOGICAL1_5
#endif
# 85 "iotk_attr_interf.spp"
#if 6 <= __IOTK_MAXRANK
subroutine iotk_write_attr_LOGICAL1_6(attr,name,val,first,ierr)
  use iotk_base
  implicit none
  character(*), intent(inout) :: attr
  character(*), intent(in)    :: name
# 92 "iotk_attr_interf.spp"
  LOGICAL(kind=__IOTK_LOGICAL1), intent(in)  :: val (:,:,:,:,:,:)
  logical, optional, intent(in)  :: first
  integer, optional, intent(out) :: ierr
end subroutine iotk_write_attr_LOGICAL1_6
#endif
# 85 "iotk_attr_interf.spp"
#if 7 <= __IOTK_MAXRANK
subroutine iotk_write_attr_LOGICAL1_7(attr,name,val,first,ierr)
  use iotk_base
  implicit none
  character(*), intent(inout) :: attr
  character(*), intent(in)    :: name
# 92 "iotk_attr_interf.spp"
  LOGICAL(kind=__IOTK_LOGICAL1), intent(in)  :: val (:,:,:,:,:,:,:)
  logical, optional, intent(in)  :: first
  integer, optional, intent(out) :: ierr
end subroutine iotk_write_attr_LOGICAL1_7
#endif
# 99 "iotk_attr_interf.spp"
#endif
# 82 "iotk_attr_interf.spp"
#ifdef __IOTK_LOGICAL2
# 85 "iotk_attr_interf.spp"
#if 0 <= __IOTK_MAXRANK
subroutine iotk_write_attr_LOGICAL2_0(attr,name,val,first,ierr)
  use iotk_base
  implicit none
  character(*), intent(inout) :: attr
  character(*), intent(in)    :: name
# 92 "iotk_attr_interf.spp"
  LOGICAL(kind=__IOTK_LOGICAL2), intent(in)  :: val 
  logical, optional, intent(in)  :: first
  integer, optional, intent(out) :: ierr
end subroutine iotk_write_attr_LOGICAL2_0
#endif
# 85 "iotk_attr_interf.spp"
#if 1 <= __IOTK_MAXRANK
subroutine iotk_write_attr_LOGICAL2_1(attr,name,val,first,ierr)
  use iotk_base
  implicit none
  character(*), intent(inout) :: attr
  character(*), intent(in)    :: name
# 92 "iotk_attr_interf.spp"
  LOGICAL(kind=__IOTK_LOGICAL2), intent(in)  :: val (:)
  logical, optional, intent(in)  :: first
  integer, optional, intent(out) :: ierr
end subroutine iotk_write_attr_LOGICAL2_1
#endif
# 85 "iotk_attr_interf.spp"
#if 2 <= __IOTK_MAXRANK
subroutine iotk_write_attr_LOGICAL2_2(attr,name,val,first,ierr)
  use iotk_base
  implicit none
  character(*), intent(inout) :: attr
  character(*), intent(in)    :: name
# 92 "iotk_attr_interf.spp"
  LOGICAL(kind=__IOTK_LOGICAL2), intent(in)  :: val (:,:)
  logical, optional, intent(in)  :: first
  integer, optional, intent(out) :: ierr
end subroutine iotk_write_attr_LOGICAL2_2
#endif
# 85 "iotk_attr_interf.spp"
#if 3 <= __IOTK_MAXRANK
subroutine iotk_write_attr_LOGICAL2_3(attr,name,val,first,ierr)
  use iotk_base
  implicit none
  character(*), intent(inout) :: attr
  character(*), intent(in)    :: name
# 92 "iotk_attr_interf.spp"
  LOGICAL(kind=__IOTK_LOGICAL2), intent(in)  :: val (:,:,:)
  logical, optional, intent(in)  :: first
  integer, optional, intent(out) :: ierr
end subroutine iotk_write_attr_LOGICAL2_3
#endif
# 85 "iotk_attr_interf.spp"
#if 4 <= __IOTK_MAXRANK
subroutine iotk_write_attr_LOGICAL2_4(attr,name,val,first,ierr)
  use iotk_base
  implicit none
  character(*), intent(inout) :: attr
  character(*), intent(in)    :: name
# 92 "iotk_attr_interf.spp"
  LOGICAL(kind=__IOTK_LOGICAL2), intent(in)  :: val (:,:,:,:)
  logical, optional, intent(in)  :: first
  integer, optional, intent(out) :: ierr
end subroutine iotk_write_attr_LOGICAL2_4
#endif
# 85 "iotk_attr_interf.spp"
#if 5 <= __IOTK_MAXRANK
subroutine iotk_write_attr_LOGICAL2_5(attr,name,val,first,ierr)
  use iotk_base
  implicit none
  character(*), intent(inout) :: attr
  character(*), intent(in)    :: name
# 92 "iotk_attr_interf.spp"
  LOGICAL(kind=__IOTK_LOGICAL2), intent(in)  :: val (:,:,:,:,:)
  logical, optional, intent(in)  :: first
  integer, optional, intent(out) :: ierr
end subroutine iotk_write_attr_LOGICAL2_5
#endif
# 85 "iotk_attr_interf.spp"
#if 6 <= __IOTK_MAXRANK
subroutine iotk_write_attr_LOGICAL2_6(attr,name,val,first,ierr)
  use iotk_base
  implicit none
  character(*), intent(inout) :: attr
  character(*), intent(in)    :: name
# 92 "iotk_attr_interf.spp"
  LOGICAL(kind=__IOTK_LOGICAL2), intent(in)  :: val (:,:,:,:,:,:)
  logical, optional, intent(in)  :: first
  integer, optional, intent(out) :: ierr
end subroutine iotk_write_attr_LOGICAL2_6
#endif
# 85 "iotk_attr_interf.spp"
#if 7 <= __IOTK_MAXRANK
subroutine iotk_write_attr_LOGICAL2_7(attr,name,val,first,ierr)
  use iotk_base
  implicit none
  character(*), intent(inout) :: attr
  character(*), intent(in)    :: name
# 92 "iotk_attr_interf.spp"
  LOGICAL(kind=__IOTK_LOGICAL2), intent(in)  :: val (:,:,:,:,:,:,:)
  logical, optional, intent(in)  :: first
  integer, optional, intent(out) :: ierr
end subroutine iotk_write_attr_LOGICAL2_7
#endif
# 99 "iotk_attr_interf.spp"
#endif
# 82 "iotk_attr_interf.spp"
#ifdef __IOTK_LOGICAL3
# 85 "iotk_attr_interf.spp"
#if 0 <= __IOTK_MAXRANK
subroutine iotk_write_attr_LOGICAL3_0(attr,name,val,first,ierr)
  use iotk_base
  implicit none
  character(*), intent(inout) :: attr
  character(*), intent(in)    :: name
# 92 "iotk_attr_interf.spp"
  LOGICAL(kind=__IOTK_LOGICAL3), intent(in)  :: val 
  logical, optional, intent(in)  :: first
  integer, optional, intent(out) :: ierr
end subroutine iotk_write_attr_LOGICAL3_0
#endif
# 85 "iotk_attr_interf.spp"
#if 1 <= __IOTK_MAXRANK
subroutine iotk_write_attr_LOGICAL3_1(attr,name,val,first,ierr)
  use iotk_base
  implicit none
  character(*), intent(inout) :: attr
  character(*), intent(in)    :: name
# 92 "iotk_attr_interf.spp"
  LOGICAL(kind=__IOTK_LOGICAL3), intent(in)  :: val (:)
  logical, optional, intent(in)  :: first
  integer, optional, intent(out) :: ierr
end subroutine iotk_write_attr_LOGICAL3_1
#endif
# 85 "iotk_attr_interf.spp"
#if 2 <= __IOTK_MAXRANK
subroutine iotk_write_attr_LOGICAL3_2(attr,name,val,first,ierr)
  use iotk_base
  implicit none
  character(*), intent(inout) :: attr
  character(*), intent(in)    :: name
# 92 "iotk_attr_interf.spp"
  LOGICAL(kind=__IOTK_LOGICAL3), intent(in)  :: val (:,:)
  logical, optional, intent(in)  :: first
  integer, optional, intent(out) :: ierr
end subroutine iotk_write_attr_LOGICAL3_2
#endif
# 85 "iotk_attr_interf.spp"
#if 3 <= __IOTK_MAXRANK
subroutine iotk_write_attr_LOGICAL3_3(attr,name,val,first,ierr)
  use iotk_base
  implicit none
  character(*), intent(inout) :: attr
  character(*), intent(in)    :: name
# 92 "iotk_attr_interf.spp"
  LOGICAL(kind=__IOTK_LOGICAL3), intent(in)  :: val (:,:,:)
  logical, optional, intent(in)  :: first
  integer, optional, intent(out) :: ierr
end subroutine iotk_write_attr_LOGICAL3_3
#endif
# 85 "iotk_attr_interf.spp"
#if 4 <= __IOTK_MAXRANK
subroutine iotk_write_attr_LOGICAL3_4(attr,name,val,first,ierr)
  use iotk_base
  implicit none
  character(*), intent(inout) :: attr
  character(*), intent(in)    :: name
# 92 "iotk_attr_interf.spp"
  LOGICAL(kind=__IOTK_LOGICAL3), intent(in)  :: val (:,:,:,:)
  logical, optional, intent(in)  :: first
  integer, optional, intent(out) :: ierr
end subroutine iotk_write_attr_LOGICAL3_4
#endif
# 85 "iotk_attr_interf.spp"
#if 5 <= __IOTK_MAXRANK
subroutine iotk_write_attr_LOGICAL3_5(attr,name,val,first,ierr)
  use iotk_base
  implicit none
  character(*), intent(inout) :: attr
  character(*), intent(in)    :: name
# 92 "iotk_attr_interf.spp"
  LOGICAL(kind=__IOTK_LOGICAL3), intent(in)  :: val (:,:,:,:,:)
  logical, optional, intent(in)  :: first
  integer, optional, intent(out) :: ierr
end subroutine iotk_write_attr_LOGICAL3_5
#endif
# 85 "iotk_attr_interf.spp"
#if 6 <= __IOTK_MAXRANK
subroutine iotk_write_attr_LOGICAL3_6(attr,name,val,first,ierr)
  use iotk_base
  implicit none
  character(*), intent(inout) :: attr
  character(*), intent(in)    :: name
# 92 "iotk_attr_interf.spp"
  LOGICAL(kind=__IOTK_LOGICAL3), intent(in)  :: val (:,:,:,:,:,:)
  logical, optional, intent(in)  :: first
  integer, optional, intent(out) :: ierr
end subroutine iotk_write_attr_LOGICAL3_6
#endif
# 85 "iotk_attr_interf.spp"
#if 7 <= __IOTK_MAXRANK
subroutine iotk_write_attr_LOGICAL3_7(attr,name,val,first,ierr)
  use iotk_base
  implicit none
  character(*), intent(inout) :: attr
  character(*), intent(in)    :: name
# 92 "iotk_attr_interf.spp"
  LOGICAL(kind=__IOTK_LOGICAL3), intent(in)  :: val (:,:,:,:,:,:,:)
  logical, optional, intent(in)  :: first
  integer, optional, intent(out) :: ierr
end subroutine iotk_write_attr_LOGICAL3_7
#endif
# 99 "iotk_attr_interf.spp"
#endif
# 82 "iotk_attr_interf.spp"
#ifdef __IOTK_LOGICAL4
# 85 "iotk_attr_interf.spp"
#if 0 <= __IOTK_MAXRANK
subroutine iotk_write_attr_LOGICAL4_0(attr,name,val,first,ierr)
  use iotk_base
  implicit none
  character(*), intent(inout) :: attr
  character(*), intent(in)    :: name
# 92 "iotk_attr_interf.spp"
  LOGICAL(kind=__IOTK_LOGICAL4), intent(in)  :: val 
  logical, optional, intent(in)  :: first
  integer, optional, intent(out) :: ierr
end subroutine iotk_write_attr_LOGICAL4_0
#endif
# 85 "iotk_attr_interf.spp"
#if 1 <= __IOTK_MAXRANK
subroutine iotk_write_attr_LOGICAL4_1(attr,name,val,first,ierr)
  use iotk_base
  implicit none
  character(*), intent(inout) :: attr
  character(*), intent(in)    :: name
# 92 "iotk_attr_interf.spp"
  LOGICAL(kind=__IOTK_LOGICAL4), intent(in)  :: val (:)
  logical, optional, intent(in)  :: first
  integer, optional, intent(out) :: ierr
end subroutine iotk_write_attr_LOGICAL4_1
#endif
# 85 "iotk_attr_interf.spp"
#if 2 <= __IOTK_MAXRANK
subroutine iotk_write_attr_LOGICAL4_2(attr,name,val,first,ierr)
  use iotk_base
  implicit none
  character(*), intent(inout) :: attr
  character(*), intent(in)    :: name
# 92 "iotk_attr_interf.spp"
  LOGICAL(kind=__IOTK_LOGICAL4), intent(in)  :: val (:,:)
  logical, optional, intent(in)  :: first
  integer, optional, intent(out) :: ierr
end subroutine iotk_write_attr_LOGICAL4_2
#endif
# 85 "iotk_attr_interf.spp"
#if 3 <= __IOTK_MAXRANK
subroutine iotk_write_attr_LOGICAL4_3(attr,name,val,first,ierr)
  use iotk_base
  implicit none
  character(*), intent(inout) :: attr
  character(*), intent(in)    :: name
# 92 "iotk_attr_interf.spp"
  LOGICAL(kind=__IOTK_LOGICAL4), intent(in)  :: val (:,:,:)
  logical, optional, intent(in)  :: first
  integer, optional, intent(out) :: ierr
end subroutine iotk_write_attr_LOGICAL4_3
#endif
# 85 "iotk_attr_interf.spp"
#if 4 <= __IOTK_MAXRANK
subroutine iotk_write_attr_LOGICAL4_4(attr,name,val,first,ierr)
  use iotk_base
  implicit none
  character(*), intent(inout) :: attr
  character(*), intent(in)    :: name
# 92 "iotk_attr_interf.spp"
  LOGICAL(kind=__IOTK_LOGICAL4), intent(in)  :: val (:,:,:,:)
  logical, optional, intent(in)  :: first
  integer, optional, intent(out) :: ierr
end subroutine iotk_write_attr_LOGICAL4_4
#endif
# 85 "iotk_attr_interf.spp"
#if 5 <= __IOTK_MAXRANK
subroutine iotk_write_attr_LOGICAL4_5(attr,name,val,first,ierr)
  use iotk_base
  implicit none
  character(*), intent(inout) :: attr
  character(*), intent(in)    :: name
# 92 "iotk_attr_interf.spp"
  LOGICAL(kind=__IOTK_LOGICAL4), intent(in)  :: val (:,:,:,:,:)
  logical, optional, intent(in)  :: first
  integer, optional, intent(out) :: ierr
end subroutine iotk_write_attr_LOGICAL4_5
#endif
# 85 "iotk_attr_interf.spp"
#if 6 <= __IOTK_MAXRANK
subroutine iotk_write_attr_LOGICAL4_6(attr,name,val,first,ierr)
  use iotk_base
  implicit none
  character(*), intent(inout) :: attr
  character(*), intent(in)    :: name
# 92 "iotk_attr_interf.spp"
  LOGICAL(kind=__IOTK_LOGICAL4), intent(in)  :: val (:,:,:,:,:,:)
  logical, optional, intent(in)  :: first
  integer, optional, intent(out) :: ierr
end subroutine iotk_write_attr_LOGICAL4_6
#endif
# 85 "iotk_attr_interf.spp"
#if 7 <= __IOTK_MAXRANK
subroutine iotk_write_attr_LOGICAL4_7(attr,name,val,first,ierr)
  use iotk_base
  implicit none
  character(*), intent(inout) :: attr
  character(*), intent(in)    :: name
# 92 "iotk_attr_interf.spp"
  LOGICAL(kind=__IOTK_LOGICAL4), intent(in)  :: val (:,:,:,:,:,:,:)
  logical, optional, intent(in)  :: first
  integer, optional, intent(out) :: ierr
end subroutine iotk_write_attr_LOGICAL4_7
#endif
# 99 "iotk_attr_interf.spp"
#endif
# 82 "iotk_attr_interf.spp"
#ifdef __IOTK_INTEGER1
# 85 "iotk_attr_interf.spp"
#if 0 <= __IOTK_MAXRANK
subroutine iotk_write_attr_INTEGER1_0(attr,name,val,first,ierr)
  use iotk_base
  implicit none
  character(*), intent(inout) :: attr
  character(*), intent(in)    :: name
# 92 "iotk_attr_interf.spp"
  INTEGER(kind=__IOTK_INTEGER1), intent(in)  :: val 
  logical, optional, intent(in)  :: first
  integer, optional, intent(out) :: ierr
end subroutine iotk_write_attr_INTEGER1_0
#endif
# 85 "iotk_attr_interf.spp"
#if 1 <= __IOTK_MAXRANK
subroutine iotk_write_attr_INTEGER1_1(attr,name,val,first,ierr)
  use iotk_base
  implicit none
  character(*), intent(inout) :: attr
  character(*), intent(in)    :: name
# 92 "iotk_attr_interf.spp"
  INTEGER(kind=__IOTK_INTEGER1), intent(in)  :: val (:)
  logical, optional, intent(in)  :: first
  integer, optional, intent(out) :: ierr
end subroutine iotk_write_attr_INTEGER1_1
#endif
# 85 "iotk_attr_interf.spp"
#if 2 <= __IOTK_MAXRANK
subroutine iotk_write_attr_INTEGER1_2(attr,name,val,first,ierr)
  use iotk_base
  implicit none
  character(*), intent(inout) :: attr
  character(*), intent(in)    :: name
# 92 "iotk_attr_interf.spp"
  INTEGER(kind=__IOTK_INTEGER1), intent(in)  :: val (:,:)
  logical, optional, intent(in)  :: first
  integer, optional, intent(out) :: ierr
end subroutine iotk_write_attr_INTEGER1_2
#endif
# 85 "iotk_attr_interf.spp"
#if 3 <= __IOTK_MAXRANK
subroutine iotk_write_attr_INTEGER1_3(attr,name,val,first,ierr)
  use iotk_base
  implicit none
  character(*), intent(inout) :: attr
  character(*), intent(in)    :: name
# 92 "iotk_attr_interf.spp"
  INTEGER(kind=__IOTK_INTEGER1), intent(in)  :: val (:,:,:)
  logical, optional, intent(in)  :: first
  integer, optional, intent(out) :: ierr
end subroutine iotk_write_attr_INTEGER1_3
#endif
# 85 "iotk_attr_interf.spp"
#if 4 <= __IOTK_MAXRANK
subroutine iotk_write_attr_INTEGER1_4(attr,name,val,first,ierr)
  use iotk_base
  implicit none
  character(*), intent(inout) :: attr
  character(*), intent(in)    :: name
# 92 "iotk_attr_interf.spp"
  INTEGER(kind=__IOTK_INTEGER1), intent(in)  :: val (:,:,:,:)
  logical, optional, intent(in)  :: first
  integer, optional, intent(out) :: ierr
end subroutine iotk_write_attr_INTEGER1_4
#endif
# 85 "iotk_attr_interf.spp"
#if 5 <= __IOTK_MAXRANK
subroutine iotk_write_attr_INTEGER1_5(attr,name,val,first,ierr)
  use iotk_base
  implicit none
  character(*), intent(inout) :: attr
  character(*), intent(in)    :: name
# 92 "iotk_attr_interf.spp"
  INTEGER(kind=__IOTK_INTEGER1), intent(in)  :: val (:,:,:,:,:)
  logical, optional, intent(in)  :: first
  integer, optional, intent(out) :: ierr
end subroutine iotk_write_attr_INTEGER1_5
#endif
# 85 "iotk_attr_interf.spp"
#if 6 <= __IOTK_MAXRANK
subroutine iotk_write_attr_INTEGER1_6(attr,name,val,first,ierr)
  use iotk_base
  implicit none
  character(*), intent(inout) :: attr
  character(*), intent(in)    :: name
# 92 "iotk_attr_interf.spp"
  INTEGER(kind=__IOTK_INTEGER1), intent(in)  :: val (:,:,:,:,:,:)
  logical, optional, intent(in)  :: first
  integer, optional, intent(out) :: ierr
end subroutine iotk_write_attr_INTEGER1_6
#endif
# 85 "iotk_attr_interf.spp"
#if 7 <= __IOTK_MAXRANK
subroutine iotk_write_attr_INTEGER1_7(attr,name,val,first,ierr)
  use iotk_base
  implicit none
  character(*), intent(inout) :: attr
  character(*), intent(in)    :: name
# 92 "iotk_attr_interf.spp"
  INTEGER(kind=__IOTK_INTEGER1), intent(in)  :: val (:,:,:,:,:,:,:)
  logical, optional, intent(in)  :: first
  integer, optional, intent(out) :: ierr
end subroutine iotk_write_attr_INTEGER1_7
#endif
# 99 "iotk_attr_interf.spp"
#endif
# 82 "iotk_attr_interf.spp"
#ifdef __IOTK_INTEGER2
# 85 "iotk_attr_interf.spp"
#if 0 <= __IOTK_MAXRANK
subroutine iotk_write_attr_INTEGER2_0(attr,name,val,first,ierr)
  use iotk_base
  implicit none
  character(*), intent(inout) :: attr
  character(*), intent(in)    :: name
# 92 "iotk_attr_interf.spp"
  INTEGER(kind=__IOTK_INTEGER2), intent(in)  :: val 
  logical, optional, intent(in)  :: first
  integer, optional, intent(out) :: ierr
end subroutine iotk_write_attr_INTEGER2_0
#endif
# 85 "iotk_attr_interf.spp"
#if 1 <= __IOTK_MAXRANK
subroutine iotk_write_attr_INTEGER2_1(attr,name,val,first,ierr)
  use iotk_base
  implicit none
  character(*), intent(inout) :: attr
  character(*), intent(in)    :: name
# 92 "iotk_attr_interf.spp"
  INTEGER(kind=__IOTK_INTEGER2), intent(in)  :: val (:)
  logical, optional, intent(in)  :: first
  integer, optional, intent(out) :: ierr
end subroutine iotk_write_attr_INTEGER2_1
#endif
# 85 "iotk_attr_interf.spp"
#if 2 <= __IOTK_MAXRANK
subroutine iotk_write_attr_INTEGER2_2(attr,name,val,first,ierr)
  use iotk_base
  implicit none
  character(*), intent(inout) :: attr
  character(*), intent(in)    :: name
# 92 "iotk_attr_interf.spp"
  INTEGER(kind=__IOTK_INTEGER2), intent(in)  :: val (:,:)
  logical, optional, intent(in)  :: first
  integer, optional, intent(out) :: ierr
end subroutine iotk_write_attr_INTEGER2_2
#endif
# 85 "iotk_attr_interf.spp"
#if 3 <= __IOTK_MAXRANK
subroutine iotk_write_attr_INTEGER2_3(attr,name,val,first,ierr)
  use iotk_base
  implicit none
  character(*), intent(inout) :: attr
  character(*), intent(in)    :: name
# 92 "iotk_attr_interf.spp"
  INTEGER(kind=__IOTK_INTEGER2), intent(in)  :: val (:,:,:)
  logical, optional, intent(in)  :: first
  integer, optional, intent(out) :: ierr
end subroutine iotk_write_attr_INTEGER2_3
#endif
# 85 "iotk_attr_interf.spp"
#if 4 <= __IOTK_MAXRANK
subroutine iotk_write_attr_INTEGER2_4(attr,name,val,first,ierr)
  use iotk_base
  implicit none
  character(*), intent(inout) :: attr
  character(*), intent(in)    :: name
# 92 "iotk_attr_interf.spp"
  INTEGER(kind=__IOTK_INTEGER2), intent(in)  :: val (:,:,:,:)
  logical, optional, intent(in)  :: first
  integer, optional, intent(out) :: ierr
end subroutine iotk_write_attr_INTEGER2_4
#endif
# 85 "iotk_attr_interf.spp"
#if 5 <= __IOTK_MAXRANK
subroutine iotk_write_attr_INTEGER2_5(attr,name,val,first,ierr)
  use iotk_base
  implicit none
  character(*), intent(inout) :: attr
  character(*), intent(in)    :: name
# 92 "iotk_attr_interf.spp"
  INTEGER(kind=__IOTK_INTEGER2), intent(in)  :: val (:,:,:,:,:)
  logical, optional, intent(in)  :: first
  integer, optional, intent(out) :: ierr
end subroutine iotk_write_attr_INTEGER2_5
#endif
# 85 "iotk_attr_interf.spp"
#if 6 <= __IOTK_MAXRANK
subroutine iotk_write_attr_INTEGER2_6(attr,name,val,first,ierr)
  use iotk_base
  implicit none
  character(*), intent(inout) :: attr
  character(*), intent(in)    :: name
# 92 "iotk_attr_interf.spp"
  INTEGER(kind=__IOTK_INTEGER2), intent(in)  :: val (:,:,:,:,:,:)
  logical, optional, intent(in)  :: first
  integer, optional, intent(out) :: ierr
end subroutine iotk_write_attr_INTEGER2_6
#endif
# 85 "iotk_attr_interf.spp"
#if 7 <= __IOTK_MAXRANK
subroutine iotk_write_attr_INTEGER2_7(attr,name,val,first,ierr)
  use iotk_base
  implicit none
  character(*), intent(inout) :: attr
  character(*), intent(in)    :: name
# 92 "iotk_attr_interf.spp"
  INTEGER(kind=__IOTK_INTEGER2), intent(in)  :: val (:,:,:,:,:,:,:)
  logical, optional, intent(in)  :: first
  integer, optional, intent(out) :: ierr
end subroutine iotk_write_attr_INTEGER2_7
#endif
# 99 "iotk_attr_interf.spp"
#endif
# 82 "iotk_attr_interf.spp"
#ifdef __IOTK_INTEGER3
# 85 "iotk_attr_interf.spp"
#if 0 <= __IOTK_MAXRANK
subroutine iotk_write_attr_INTEGER3_0(attr,name,val,first,ierr)
  use iotk_base
  implicit none
  character(*), intent(inout) :: attr
  character(*), intent(in)    :: name
# 92 "iotk_attr_interf.spp"
  INTEGER(kind=__IOTK_INTEGER3), intent(in)  :: val 
  logical, optional, intent(in)  :: first
  integer, optional, intent(out) :: ierr
end subroutine iotk_write_attr_INTEGER3_0
#endif
# 85 "iotk_attr_interf.spp"
#if 1 <= __IOTK_MAXRANK
subroutine iotk_write_attr_INTEGER3_1(attr,name,val,first,ierr)
  use iotk_base
  implicit none
  character(*), intent(inout) :: attr
  character(*), intent(in)    :: name
# 92 "iotk_attr_interf.spp"
  INTEGER(kind=__IOTK_INTEGER3), intent(in)  :: val (:)
  logical, optional, intent(in)  :: first
  integer, optional, intent(out) :: ierr
end subroutine iotk_write_attr_INTEGER3_1
#endif
# 85 "iotk_attr_interf.spp"
#if 2 <= __IOTK_MAXRANK
subroutine iotk_write_attr_INTEGER3_2(attr,name,val,first,ierr)
  use iotk_base
  implicit none
  character(*), intent(inout) :: attr
  character(*), intent(in)    :: name
# 92 "iotk_attr_interf.spp"
  INTEGER(kind=__IOTK_INTEGER3), intent(in)  :: val (:,:)
  logical, optional, intent(in)  :: first
  integer, optional, intent(out) :: ierr
end subroutine iotk_write_attr_INTEGER3_2
#endif
# 85 "iotk_attr_interf.spp"
#if 3 <= __IOTK_MAXRANK
subroutine iotk_write_attr_INTEGER3_3(attr,name,val,first,ierr)
  use iotk_base
  implicit none
  character(*), intent(inout) :: attr
  character(*), intent(in)    :: name
# 92 "iotk_attr_interf.spp"
  INTEGER(kind=__IOTK_INTEGER3), intent(in)  :: val (:,:,:)
  logical, optional, intent(in)  :: first
  integer, optional, intent(out) :: ierr
end subroutine iotk_write_attr_INTEGER3_3
#endif
# 85 "iotk_attr_interf.spp"
#if 4 <= __IOTK_MAXRANK
subroutine iotk_write_attr_INTEGER3_4(attr,name,val,first,ierr)
  use iotk_base
  implicit none
  character(*), intent(inout) :: attr
  character(*), intent(in)    :: name
# 92 "iotk_attr_interf.spp"
  INTEGER(kind=__IOTK_INTEGER3), intent(in)  :: val (:,:,:,:)
  logical, optional, intent(in)  :: first
  integer, optional, intent(out) :: ierr
end subroutine iotk_write_attr_INTEGER3_4
#endif
# 85 "iotk_attr_interf.spp"
#if 5 <= __IOTK_MAXRANK
subroutine iotk_write_attr_INTEGER3_5(attr,name,val,first,ierr)
  use iotk_base
  implicit none
  character(*), intent(inout) :: attr
  character(*), intent(in)    :: name
# 92 "iotk_attr_interf.spp"
  INTEGER(kind=__IOTK_INTEGER3), intent(in)  :: val (:,:,:,:,:)
  logical, optional, intent(in)  :: first
  integer, optional, intent(out) :: ierr
end subroutine iotk_write_attr_INTEGER3_5
#endif
# 85 "iotk_attr_interf.spp"
#if 6 <= __IOTK_MAXRANK
subroutine iotk_write_attr_INTEGER3_6(attr,name,val,first,ierr)
  use iotk_base
  implicit none
  character(*), intent(inout) :: attr
  character(*), intent(in)    :: name
# 92 "iotk_attr_interf.spp"
  INTEGER(kind=__IOTK_INTEGER3), intent(in)  :: val (:,:,:,:,:,:)
  logical, optional, intent(in)  :: first
  integer, optional, intent(out) :: ierr
end subroutine iotk_write_attr_INTEGER3_6
#endif
# 85 "iotk_attr_interf.spp"
#if 7 <= __IOTK_MAXRANK
subroutine iotk_write_attr_INTEGER3_7(attr,name,val,first,ierr)
  use iotk_base
  implicit none
  character(*), intent(inout) :: attr
  character(*), intent(in)    :: name
# 92 "iotk_attr_interf.spp"
  INTEGER(kind=__IOTK_INTEGER3), intent(in)  :: val (:,:,:,:,:,:,:)
  logical, optional, intent(in)  :: first
  integer, optional, intent(out) :: ierr
end subroutine iotk_write_attr_INTEGER3_7
#endif
# 99 "iotk_attr_interf.spp"
#endif
# 82 "iotk_attr_interf.spp"
#ifdef __IOTK_INTEGER4
# 85 "iotk_attr_interf.spp"
#if 0 <= __IOTK_MAXRANK
subroutine iotk_write_attr_INTEGER4_0(attr,name,val,first,ierr)
  use iotk_base
  implicit none
  character(*), intent(inout) :: attr
  character(*), intent(in)    :: name
# 92 "iotk_attr_interf.spp"
  INTEGER(kind=__IOTK_INTEGER4), intent(in)  :: val 
  logical, optional, intent(in)  :: first
  integer, optional, intent(out) :: ierr
end subroutine iotk_write_attr_INTEGER4_0
#endif
# 85 "iotk_attr_interf.spp"
#if 1 <= __IOTK_MAXRANK
subroutine iotk_write_attr_INTEGER4_1(attr,name,val,first,ierr)
  use iotk_base
  implicit none
  character(*), intent(inout) :: attr
  character(*), intent(in)    :: name
# 92 "iotk_attr_interf.spp"
  INTEGER(kind=__IOTK_INTEGER4), intent(in)  :: val (:)
  logical, optional, intent(in)  :: first
  integer, optional, intent(out) :: ierr
end subroutine iotk_write_attr_INTEGER4_1
#endif
# 85 "iotk_attr_interf.spp"
#if 2 <= __IOTK_MAXRANK
subroutine iotk_write_attr_INTEGER4_2(attr,name,val,first,ierr)
  use iotk_base
  implicit none
  character(*), intent(inout) :: attr
  character(*), intent(in)    :: name
# 92 "iotk_attr_interf.spp"
  INTEGER(kind=__IOTK_INTEGER4), intent(in)  :: val (:,:)
  logical, optional, intent(in)  :: first
  integer, optional, intent(out) :: ierr
end subroutine iotk_write_attr_INTEGER4_2
#endif
# 85 "iotk_attr_interf.spp"
#if 3 <= __IOTK_MAXRANK
subroutine iotk_write_attr_INTEGER4_3(attr,name,val,first,ierr)
  use iotk_base
  implicit none
  character(*), intent(inout) :: attr
  character(*), intent(in)    :: name
# 92 "iotk_attr_interf.spp"
  INTEGER(kind=__IOTK_INTEGER4), intent(in)  :: val (:,:,:)
  logical, optional, intent(in)  :: first
  integer, optional, intent(out) :: ierr
end subroutine iotk_write_attr_INTEGER4_3
#endif
# 85 "iotk_attr_interf.spp"
#if 4 <= __IOTK_MAXRANK
subroutine iotk_write_attr_INTEGER4_4(attr,name,val,first,ierr)
  use iotk_base
  implicit none
  character(*), intent(inout) :: attr
  character(*), intent(in)    :: name
# 92 "iotk_attr_interf.spp"
  INTEGER(kind=__IOTK_INTEGER4), intent(in)  :: val (:,:,:,:)
  logical, optional, intent(in)  :: first
  integer, optional, intent(out) :: ierr
end subroutine iotk_write_attr_INTEGER4_4
#endif
# 85 "iotk_attr_interf.spp"
#if 5 <= __IOTK_MAXRANK
subroutine iotk_write_attr_INTEGER4_5(attr,name,val,first,ierr)
  use iotk_base
  implicit none
  character(*), intent(inout) :: attr
  character(*), intent(in)    :: name
# 92 "iotk_attr_interf.spp"
  INTEGER(kind=__IOTK_INTEGER4), intent(in)  :: val (:,:,:,:,:)
  logical, optional, intent(in)  :: first
  integer, optional, intent(out) :: ierr
end subroutine iotk_write_attr_INTEGER4_5
#endif
# 85 "iotk_attr_interf.spp"
#if 6 <= __IOTK_MAXRANK
subroutine iotk_write_attr_INTEGER4_6(attr,name,val,first,ierr)
  use iotk_base
  implicit none
  character(*), intent(inout) :: attr
  character(*), intent(in)    :: name
# 92 "iotk_attr_interf.spp"
  INTEGER(kind=__IOTK_INTEGER4), intent(in)  :: val (:,:,:,:,:,:)
  logical, optional, intent(in)  :: first
  integer, optional, intent(out) :: ierr
end subroutine iotk_write_attr_INTEGER4_6
#endif
# 85 "iotk_attr_interf.spp"
#if 7 <= __IOTK_MAXRANK
subroutine iotk_write_attr_INTEGER4_7(attr,name,val,first,ierr)
  use iotk_base
  implicit none
  character(*), intent(inout) :: attr
  character(*), intent(in)    :: name
# 92 "iotk_attr_interf.spp"
  INTEGER(kind=__IOTK_INTEGER4), intent(in)  :: val (:,:,:,:,:,:,:)
  logical, optional, intent(in)  :: first
  integer, optional, intent(out) :: ierr
end subroutine iotk_write_attr_INTEGER4_7
#endif
# 99 "iotk_attr_interf.spp"
#endif
# 82 "iotk_attr_interf.spp"
#ifdef __IOTK_REAL1
# 85 "iotk_attr_interf.spp"
#if 0 <= __IOTK_MAXRANK
subroutine iotk_write_attr_REAL1_0(attr,name,val,first,ierr)
  use iotk_base
  implicit none
  character(*), intent(inout) :: attr
  character(*), intent(in)    :: name
# 92 "iotk_attr_interf.spp"
  REAL(kind=__IOTK_REAL1), intent(in)  :: val 
  logical, optional, intent(in)  :: first
  integer, optional, intent(out) :: ierr
end subroutine iotk_write_attr_REAL1_0
#endif
# 85 "iotk_attr_interf.spp"
#if 1 <= __IOTK_MAXRANK
subroutine iotk_write_attr_REAL1_1(attr,name,val,first,ierr)
  use iotk_base
  implicit none
  character(*), intent(inout) :: attr
  character(*), intent(in)    :: name
# 92 "iotk_attr_interf.spp"
  REAL(kind=__IOTK_REAL1), intent(in)  :: val (:)
  logical, optional, intent(in)  :: first
  integer, optional, intent(out) :: ierr
end subroutine iotk_write_attr_REAL1_1
#endif
# 85 "iotk_attr_interf.spp"
#if 2 <= __IOTK_MAXRANK
subroutine iotk_write_attr_REAL1_2(attr,name,val,first,ierr)
  use iotk_base
  implicit none
  character(*), intent(inout) :: attr
  character(*), intent(in)    :: name
# 92 "iotk_attr_interf.spp"
  REAL(kind=__IOTK_REAL1), intent(in)  :: val (:,:)
  logical, optional, intent(in)  :: first
  integer, optional, intent(out) :: ierr
end subroutine iotk_write_attr_REAL1_2
#endif
# 85 "iotk_attr_interf.spp"
#if 3 <= __IOTK_MAXRANK
subroutine iotk_write_attr_REAL1_3(attr,name,val,first,ierr)
  use iotk_base
  implicit none
  character(*), intent(inout) :: attr
  character(*), intent(in)    :: name
# 92 "iotk_attr_interf.spp"
  REAL(kind=__IOTK_REAL1), intent(in)  :: val (:,:,:)
  logical, optional, intent(in)  :: first
  integer, optional, intent(out) :: ierr
end subroutine iotk_write_attr_REAL1_3
#endif
# 85 "iotk_attr_interf.spp"
#if 4 <= __IOTK_MAXRANK
subroutine iotk_write_attr_REAL1_4(attr,name,val,first,ierr)
  use iotk_base
  implicit none
  character(*), intent(inout) :: attr
  character(*), intent(in)    :: name
# 92 "iotk_attr_interf.spp"
  REAL(kind=__IOTK_REAL1), intent(in)  :: val (:,:,:,:)
  logical, optional, intent(in)  :: first
  integer, optional, intent(out) :: ierr
end subroutine iotk_write_attr_REAL1_4
#endif
# 85 "iotk_attr_interf.spp"
#if 5 <= __IOTK_MAXRANK
subroutine iotk_write_attr_REAL1_5(attr,name,val,first,ierr)
  use iotk_base
  implicit none
  character(*), intent(inout) :: attr
  character(*), intent(in)    :: name
# 92 "iotk_attr_interf.spp"
  REAL(kind=__IOTK_REAL1), intent(in)  :: val (:,:,:,:,:)
  logical, optional, intent(in)  :: first
  integer, optional, intent(out) :: ierr
end subroutine iotk_write_attr_REAL1_5
#endif
# 85 "iotk_attr_interf.spp"
#if 6 <= __IOTK_MAXRANK
subroutine iotk_write_attr_REAL1_6(attr,name,val,first,ierr)
  use iotk_base
  implicit none
  character(*), intent(inout) :: attr
  character(*), intent(in)    :: name
# 92 "iotk_attr_interf.spp"
  REAL(kind=__IOTK_REAL1), intent(in)  :: val (:,:,:,:,:,:)
  logical, optional, intent(in)  :: first
  integer, optional, intent(out) :: ierr
end subroutine iotk_write_attr_REAL1_6
#endif
# 85 "iotk_attr_interf.spp"
#if 7 <= __IOTK_MAXRANK
subroutine iotk_write_attr_REAL1_7(attr,name,val,first,ierr)
  use iotk_base
  implicit none
  character(*), intent(inout) :: attr
  character(*), intent(in)    :: name
# 92 "iotk_attr_interf.spp"
  REAL(kind=__IOTK_REAL1), intent(in)  :: val (:,:,:,:,:,:,:)
  logical, optional, intent(in)  :: first
  integer, optional, intent(out) :: ierr
end subroutine iotk_write_attr_REAL1_7
#endif
# 99 "iotk_attr_interf.spp"
#endif
# 82 "iotk_attr_interf.spp"
#ifdef __IOTK_REAL2
# 85 "iotk_attr_interf.spp"
#if 0 <= __IOTK_MAXRANK
subroutine iotk_write_attr_REAL2_0(attr,name,val,first,ierr)
  use iotk_base
  implicit none
  character(*), intent(inout) :: attr
  character(*), intent(in)    :: name
# 92 "iotk_attr_interf.spp"
  REAL(kind=__IOTK_REAL2), intent(in)  :: val 
  logical, optional, intent(in)  :: first
  integer, optional, intent(out) :: ierr
end subroutine iotk_write_attr_REAL2_0
#endif
# 85 "iotk_attr_interf.spp"
#if 1 <= __IOTK_MAXRANK
subroutine iotk_write_attr_REAL2_1(attr,name,val,first,ierr)
  use iotk_base
  implicit none
  character(*), intent(inout) :: attr
  character(*), intent(in)    :: name
# 92 "iotk_attr_interf.spp"
  REAL(kind=__IOTK_REAL2), intent(in)  :: val (:)
  logical, optional, intent(in)  :: first
  integer, optional, intent(out) :: ierr
end subroutine iotk_write_attr_REAL2_1
#endif
# 85 "iotk_attr_interf.spp"
#if 2 <= __IOTK_MAXRANK
subroutine iotk_write_attr_REAL2_2(attr,name,val,first,ierr)
  use iotk_base
  implicit none
  character(*), intent(inout) :: attr
  character(*), intent(in)    :: name
# 92 "iotk_attr_interf.spp"
  REAL(kind=__IOTK_REAL2), intent(in)  :: val (:,:)
  logical, optional, intent(in)  :: first
  integer, optional, intent(out) :: ierr
end subroutine iotk_write_attr_REAL2_2
#endif
# 85 "iotk_attr_interf.spp"
#if 3 <= __IOTK_MAXRANK
subroutine iotk_write_attr_REAL2_3(attr,name,val,first,ierr)
  use iotk_base
  implicit none
  character(*), intent(inout) :: attr
  character(*), intent(in)    :: name
# 92 "iotk_attr_interf.spp"
  REAL(kind=__IOTK_REAL2), intent(in)  :: val (:,:,:)
  logical, optional, intent(in)  :: first
  integer, optional, intent(out) :: ierr
end subroutine iotk_write_attr_REAL2_3
#endif
# 85 "iotk_attr_interf.spp"
#if 4 <= __IOTK_MAXRANK
subroutine iotk_write_attr_REAL2_4(attr,name,val,first,ierr)
  use iotk_base
  implicit none
  character(*), intent(inout) :: attr
  character(*), intent(in)    :: name
# 92 "iotk_attr_interf.spp"
  REAL(kind=__IOTK_REAL2), intent(in)  :: val (:,:,:,:)
  logical, optional, intent(in)  :: first
  integer, optional, intent(out) :: ierr
end subroutine iotk_write_attr_REAL2_4
#endif
# 85 "iotk_attr_interf.spp"
#if 5 <= __IOTK_MAXRANK
subroutine iotk_write_attr_REAL2_5(attr,name,val,first,ierr)
  use iotk_base
  implicit none
  character(*), intent(inout) :: attr
  character(*), intent(in)    :: name
# 92 "iotk_attr_interf.spp"
  REAL(kind=__IOTK_REAL2), intent(in)  :: val (:,:,:,:,:)
  logical, optional, intent(in)  :: first
  integer, optional, intent(out) :: ierr
end subroutine iotk_write_attr_REAL2_5
#endif
# 85 "iotk_attr_interf.spp"
#if 6 <= __IOTK_MAXRANK
subroutine iotk_write_attr_REAL2_6(attr,name,val,first,ierr)
  use iotk_base
  implicit none
  character(*), intent(inout) :: attr
  character(*), intent(in)    :: name
# 92 "iotk_attr_interf.spp"
  REAL(kind=__IOTK_REAL2), intent(in)  :: val (:,:,:,:,:,:)
  logical, optional, intent(in)  :: first
  integer, optional, intent(out) :: ierr
end subroutine iotk_write_attr_REAL2_6
#endif
# 85 "iotk_attr_interf.spp"
#if 7 <= __IOTK_MAXRANK
subroutine iotk_write_attr_REAL2_7(attr,name,val,first,ierr)
  use iotk_base
  implicit none
  character(*), intent(inout) :: attr
  character(*), intent(in)    :: name
# 92 "iotk_attr_interf.spp"
  REAL(kind=__IOTK_REAL2), intent(in)  :: val (:,:,:,:,:,:,:)
  logical, optional, intent(in)  :: first
  integer, optional, intent(out) :: ierr
end subroutine iotk_write_attr_REAL2_7
#endif
# 99 "iotk_attr_interf.spp"
#endif
# 82 "iotk_attr_interf.spp"
#ifdef __IOTK_REAL3
# 85 "iotk_attr_interf.spp"
#if 0 <= __IOTK_MAXRANK
subroutine iotk_write_attr_REAL3_0(attr,name,val,first,ierr)
  use iotk_base
  implicit none
  character(*), intent(inout) :: attr
  character(*), intent(in)    :: name
# 92 "iotk_attr_interf.spp"
  REAL(kind=__IOTK_REAL3), intent(in)  :: val 
  logical, optional, intent(in)  :: first
  integer, optional, intent(out) :: ierr
end subroutine iotk_write_attr_REAL3_0
#endif
# 85 "iotk_attr_interf.spp"
#if 1 <= __IOTK_MAXRANK
subroutine iotk_write_attr_REAL3_1(attr,name,val,first,ierr)
  use iotk_base
  implicit none
  character(*), intent(inout) :: attr
  character(*), intent(in)    :: name
# 92 "iotk_attr_interf.spp"
  REAL(kind=__IOTK_REAL3), intent(in)  :: val (:)
  logical, optional, intent(in)  :: first
  integer, optional, intent(out) :: ierr
end subroutine iotk_write_attr_REAL3_1
#endif
# 85 "iotk_attr_interf.spp"
#if 2 <= __IOTK_MAXRANK
subroutine iotk_write_attr_REAL3_2(attr,name,val,first,ierr)
  use iotk_base
  implicit none
  character(*), intent(inout) :: attr
  character(*), intent(in)    :: name
# 92 "iotk_attr_interf.spp"
  REAL(kind=__IOTK_REAL3), intent(in)  :: val (:,:)
  logical, optional, intent(in)  :: first
  integer, optional, intent(out) :: ierr
end subroutine iotk_write_attr_REAL3_2
#endif
# 85 "iotk_attr_interf.spp"
#if 3 <= __IOTK_MAXRANK
subroutine iotk_write_attr_REAL3_3(attr,name,val,first,ierr)
  use iotk_base
  implicit none
  character(*), intent(inout) :: attr
  character(*), intent(in)    :: name
# 92 "iotk_attr_interf.spp"
  REAL(kind=__IOTK_REAL3), intent(in)  :: val (:,:,:)
  logical, optional, intent(in)  :: first
  integer, optional, intent(out) :: ierr
end subroutine iotk_write_attr_REAL3_3
#endif
# 85 "iotk_attr_interf.spp"
#if 4 <= __IOTK_MAXRANK
subroutine iotk_write_attr_REAL3_4(attr,name,val,first,ierr)
  use iotk_base
  implicit none
  character(*), intent(inout) :: attr
  character(*), intent(in)    :: name
# 92 "iotk_attr_interf.spp"
  REAL(kind=__IOTK_REAL3), intent(in)  :: val (:,:,:,:)
  logical, optional, intent(in)  :: first
  integer, optional, intent(out) :: ierr
end subroutine iotk_write_attr_REAL3_4
#endif
# 85 "iotk_attr_interf.spp"
#if 5 <= __IOTK_MAXRANK
subroutine iotk_write_attr_REAL3_5(attr,name,val,first,ierr)
  use iotk_base
  implicit none
  character(*), intent(inout) :: attr
  character(*), intent(in)    :: name
# 92 "iotk_attr_interf.spp"
  REAL(kind=__IOTK_REAL3), intent(in)  :: val (:,:,:,:,:)
  logical, optional, intent(in)  :: first
  integer, optional, intent(out) :: ierr
end subroutine iotk_write_attr_REAL3_5
#endif
# 85 "iotk_attr_interf.spp"
#if 6 <= __IOTK_MAXRANK
subroutine iotk_write_attr_REAL3_6(attr,name,val,first,ierr)
  use iotk_base
  implicit none
  character(*), intent(inout) :: attr
  character(*), intent(in)    :: name
# 92 "iotk_attr_interf.spp"
  REAL(kind=__IOTK_REAL3), intent(in)  :: val (:,:,:,:,:,:)
  logical, optional, intent(in)  :: first
  integer, optional, intent(out) :: ierr
end subroutine iotk_write_attr_REAL3_6
#endif
# 85 "iotk_attr_interf.spp"
#if 7 <= __IOTK_MAXRANK
subroutine iotk_write_attr_REAL3_7(attr,name,val,first,ierr)
  use iotk_base
  implicit none
  character(*), intent(inout) :: attr
  character(*), intent(in)    :: name
# 92 "iotk_attr_interf.spp"
  REAL(kind=__IOTK_REAL3), intent(in)  :: val (:,:,:,:,:,:,:)
  logical, optional, intent(in)  :: first
  integer, optional, intent(out) :: ierr
end subroutine iotk_write_attr_REAL3_7
#endif
# 99 "iotk_attr_interf.spp"
#endif
# 82 "iotk_attr_interf.spp"
#ifdef __IOTK_REAL4
# 85 "iotk_attr_interf.spp"
#if 0 <= __IOTK_MAXRANK
subroutine iotk_write_attr_REAL4_0(attr,name,val,first,ierr)
  use iotk_base
  implicit none
  character(*), intent(inout) :: attr
  character(*), intent(in)    :: name
# 92 "iotk_attr_interf.spp"
  REAL(kind=__IOTK_REAL4), intent(in)  :: val 
  logical, optional, intent(in)  :: first
  integer, optional, intent(out) :: ierr
end subroutine iotk_write_attr_REAL4_0
#endif
# 85 "iotk_attr_interf.spp"
#if 1 <= __IOTK_MAXRANK
subroutine iotk_write_attr_REAL4_1(attr,name,val,first,ierr)
  use iotk_base
  implicit none
  character(*), intent(inout) :: attr
  character(*), intent(in)    :: name
# 92 "iotk_attr_interf.spp"
  REAL(kind=__IOTK_REAL4), intent(in)  :: val (:)
  logical, optional, intent(in)  :: first
  integer, optional, intent(out) :: ierr
end subroutine iotk_write_attr_REAL4_1
#endif
# 85 "iotk_attr_interf.spp"
#if 2 <= __IOTK_MAXRANK
subroutine iotk_write_attr_REAL4_2(attr,name,val,first,ierr)
  use iotk_base
  implicit none
  character(*), intent(inout) :: attr
  character(*), intent(in)    :: name
# 92 "iotk_attr_interf.spp"
  REAL(kind=__IOTK_REAL4), intent(in)  :: val (:,:)
  logical, optional, intent(in)  :: first
  integer, optional, intent(out) :: ierr
end subroutine iotk_write_attr_REAL4_2
#endif
# 85 "iotk_attr_interf.spp"
#if 3 <= __IOTK_MAXRANK
subroutine iotk_write_attr_REAL4_3(attr,name,val,first,ierr)
  use iotk_base
  implicit none
  character(*), intent(inout) :: attr
  character(*), intent(in)    :: name
# 92 "iotk_attr_interf.spp"
  REAL(kind=__IOTK_REAL4), intent(in)  :: val (:,:,:)
  logical, optional, intent(in)  :: first
  integer, optional, intent(out) :: ierr
end subroutine iotk_write_attr_REAL4_3
#endif
# 85 "iotk_attr_interf.spp"
#if 4 <= __IOTK_MAXRANK
subroutine iotk_write_attr_REAL4_4(attr,name,val,first,ierr)
  use iotk_base
  implicit none
  character(*), intent(inout) :: attr
  character(*), intent(in)    :: name
# 92 "iotk_attr_interf.spp"
  REAL(kind=__IOTK_REAL4), intent(in)  :: val (:,:,:,:)
  logical, optional, intent(in)  :: first
  integer, optional, intent(out) :: ierr
end subroutine iotk_write_attr_REAL4_4
#endif
# 85 "iotk_attr_interf.spp"
#if 5 <= __IOTK_MAXRANK
subroutine iotk_write_attr_REAL4_5(attr,name,val,first,ierr)
  use iotk_base
  implicit none
  character(*), intent(inout) :: attr
  character(*), intent(in)    :: name
# 92 "iotk_attr_interf.spp"
  REAL(kind=__IOTK_REAL4), intent(in)  :: val (:,:,:,:,:)
  logical, optional, intent(in)  :: first
  integer, optional, intent(out) :: ierr
end subroutine iotk_write_attr_REAL4_5
#endif
# 85 "iotk_attr_interf.spp"
#if 6 <= __IOTK_MAXRANK
subroutine iotk_write_attr_REAL4_6(attr,name,val,first,ierr)
  use iotk_base
  implicit none
  character(*), intent(inout) :: attr
  character(*), intent(in)    :: name
# 92 "iotk_attr_interf.spp"
  REAL(kind=__IOTK_REAL4), intent(in)  :: val (:,:,:,:,:,:)
  logical, optional, intent(in)  :: first
  integer, optional, intent(out) :: ierr
end subroutine iotk_write_attr_REAL4_6
#endif
# 85 "iotk_attr_interf.spp"
#if 7 <= __IOTK_MAXRANK
subroutine iotk_write_attr_REAL4_7(attr,name,val,first,ierr)
  use iotk_base
  implicit none
  character(*), intent(inout) :: attr
  character(*), intent(in)    :: name
# 92 "iotk_attr_interf.spp"
  REAL(kind=__IOTK_REAL4), intent(in)  :: val (:,:,:,:,:,:,:)
  logical, optional, intent(in)  :: first
  integer, optional, intent(out) :: ierr
end subroutine iotk_write_attr_REAL4_7
#endif
# 99 "iotk_attr_interf.spp"
#endif
# 82 "iotk_attr_interf.spp"
#ifdef __IOTK_COMPLEX1
# 85 "iotk_attr_interf.spp"
#if 0 <= __IOTK_MAXRANK
subroutine iotk_write_attr_COMPLEX1_0(attr,name,val,first,ierr)
  use iotk_base
  implicit none
  character(*), intent(inout) :: attr
  character(*), intent(in)    :: name
# 92 "iotk_attr_interf.spp"
  COMPLEX(kind=__IOTK_COMPLEX1), intent(in)  :: val 
  logical, optional, intent(in)  :: first
  integer, optional, intent(out) :: ierr
end subroutine iotk_write_attr_COMPLEX1_0
#endif
# 85 "iotk_attr_interf.spp"
#if 1 <= __IOTK_MAXRANK
subroutine iotk_write_attr_COMPLEX1_1(attr,name,val,first,ierr)
  use iotk_base
  implicit none
  character(*), intent(inout) :: attr
  character(*), intent(in)    :: name
# 92 "iotk_attr_interf.spp"
  COMPLEX(kind=__IOTK_COMPLEX1), intent(in)  :: val (:)
  logical, optional, intent(in)  :: first
  integer, optional, intent(out) :: ierr
end subroutine iotk_write_attr_COMPLEX1_1
#endif
# 85 "iotk_attr_interf.spp"
#if 2 <= __IOTK_MAXRANK
subroutine iotk_write_attr_COMPLEX1_2(attr,name,val,first,ierr)
  use iotk_base
  implicit none
  character(*), intent(inout) :: attr
  character(*), intent(in)    :: name
# 92 "iotk_attr_interf.spp"
  COMPLEX(kind=__IOTK_COMPLEX1), intent(in)  :: val (:,:)
  logical, optional, intent(in)  :: first
  integer, optional, intent(out) :: ierr
end subroutine iotk_write_attr_COMPLEX1_2
#endif
# 85 "iotk_attr_interf.spp"
#if 3 <= __IOTK_MAXRANK
subroutine iotk_write_attr_COMPLEX1_3(attr,name,val,first,ierr)
  use iotk_base
  implicit none
  character(*), intent(inout) :: attr
  character(*), intent(in)    :: name
# 92 "iotk_attr_interf.spp"
  COMPLEX(kind=__IOTK_COMPLEX1), intent(in)  :: val (:,:,:)
  logical, optional, intent(in)  :: first
  integer, optional, intent(out) :: ierr
end subroutine iotk_write_attr_COMPLEX1_3
#endif
# 85 "iotk_attr_interf.spp"
#if 4 <= __IOTK_MAXRANK
subroutine iotk_write_attr_COMPLEX1_4(attr,name,val,first,ierr)
  use iotk_base
  implicit none
  character(*), intent(inout) :: attr
  character(*), intent(in)    :: name
# 92 "iotk_attr_interf.spp"
  COMPLEX(kind=__IOTK_COMPLEX1), intent(in)  :: val (:,:,:,:)
  logical, optional, intent(in)  :: first
  integer, optional, intent(out) :: ierr
end subroutine iotk_write_attr_COMPLEX1_4
#endif
# 85 "iotk_attr_interf.spp"
#if 5 <= __IOTK_MAXRANK
subroutine iotk_write_attr_COMPLEX1_5(attr,name,val,first,ierr)
  use iotk_base
  implicit none
  character(*), intent(inout) :: attr
  character(*), intent(in)    :: name
# 92 "iotk_attr_interf.spp"
  COMPLEX(kind=__IOTK_COMPLEX1), intent(in)  :: val (:,:,:,:,:)
  logical, optional, intent(in)  :: first
  integer, optional, intent(out) :: ierr
end subroutine iotk_write_attr_COMPLEX1_5
#endif
# 85 "iotk_attr_interf.spp"
#if 6 <= __IOTK_MAXRANK
subroutine iotk_write_attr_COMPLEX1_6(attr,name,val,first,ierr)
  use iotk_base
  implicit none
  character(*), intent(inout) :: attr
  character(*), intent(in)    :: name
# 92 "iotk_attr_interf.spp"
  COMPLEX(kind=__IOTK_COMPLEX1), intent(in)  :: val (:,:,:,:,:,:)
  logical, optional, intent(in)  :: first
  integer, optional, intent(out) :: ierr
end subroutine iotk_write_attr_COMPLEX1_6
#endif
# 85 "iotk_attr_interf.spp"
#if 7 <= __IOTK_MAXRANK
subroutine iotk_write_attr_COMPLEX1_7(attr,name,val,first,ierr)
  use iotk_base
  implicit none
  character(*), intent(inout) :: attr
  character(*), intent(in)    :: name
# 92 "iotk_attr_interf.spp"
  COMPLEX(kind=__IOTK_COMPLEX1), intent(in)  :: val (:,:,:,:,:,:,:)
  logical, optional, intent(in)  :: first
  integer, optional, intent(out) :: ierr
end subroutine iotk_write_attr_COMPLEX1_7
#endif
# 99 "iotk_attr_interf.spp"
#endif
# 82 "iotk_attr_interf.spp"
#ifdef __IOTK_COMPLEX2
# 85 "iotk_attr_interf.spp"
#if 0 <= __IOTK_MAXRANK
subroutine iotk_write_attr_COMPLEX2_0(attr,name,val,first,ierr)
  use iotk_base
  implicit none
  character(*), intent(inout) :: attr
  character(*), intent(in)    :: name
# 92 "iotk_attr_interf.spp"
  COMPLEX(kind=__IOTK_COMPLEX2), intent(in)  :: val 
  logical, optional, intent(in)  :: first
  integer, optional, intent(out) :: ierr
end subroutine iotk_write_attr_COMPLEX2_0
#endif
# 85 "iotk_attr_interf.spp"
#if 1 <= __IOTK_MAXRANK
subroutine iotk_write_attr_COMPLEX2_1(attr,name,val,first,ierr)
  use iotk_base
  implicit none
  character(*), intent(inout) :: attr
  character(*), intent(in)    :: name
# 92 "iotk_attr_interf.spp"
  COMPLEX(kind=__IOTK_COMPLEX2), intent(in)  :: val (:)
  logical, optional, intent(in)  :: first
  integer, optional, intent(out) :: ierr
end subroutine iotk_write_attr_COMPLEX2_1
#endif
# 85 "iotk_attr_interf.spp"
#if 2 <= __IOTK_MAXRANK
subroutine iotk_write_attr_COMPLEX2_2(attr,name,val,first,ierr)
  use iotk_base
  implicit none
  character(*), intent(inout) :: attr
  character(*), intent(in)    :: name
# 92 "iotk_attr_interf.spp"
  COMPLEX(kind=__IOTK_COMPLEX2), intent(in)  :: val (:,:)
  logical, optional, intent(in)  :: first
  integer, optional, intent(out) :: ierr
end subroutine iotk_write_attr_COMPLEX2_2
#endif
# 85 "iotk_attr_interf.spp"
#if 3 <= __IOTK_MAXRANK
subroutine iotk_write_attr_COMPLEX2_3(attr,name,val,first,ierr)
  use iotk_base
  implicit none
  character(*), intent(inout) :: attr
  character(*), intent(in)    :: name
# 92 "iotk_attr_interf.spp"
  COMPLEX(kind=__IOTK_COMPLEX2), intent(in)  :: val (:,:,:)
  logical, optional, intent(in)  :: first
  integer, optional, intent(out) :: ierr
end subroutine iotk_write_attr_COMPLEX2_3
#endif
# 85 "iotk_attr_interf.spp"
#if 4 <= __IOTK_MAXRANK
subroutine iotk_write_attr_COMPLEX2_4(attr,name,val,first,ierr)
  use iotk_base
  implicit none
  character(*), intent(inout) :: attr
  character(*), intent(in)    :: name
# 92 "iotk_attr_interf.spp"
  COMPLEX(kind=__IOTK_COMPLEX2), intent(in)  :: val (:,:,:,:)
  logical, optional, intent(in)  :: first
  integer, optional, intent(out) :: ierr
end subroutine iotk_write_attr_COMPLEX2_4
#endif
# 85 "iotk_attr_interf.spp"
#if 5 <= __IOTK_MAXRANK
subroutine iotk_write_attr_COMPLEX2_5(attr,name,val,first,ierr)
  use iotk_base
  implicit none
  character(*), intent(inout) :: attr
  character(*), intent(in)    :: name
# 92 "iotk_attr_interf.spp"
  COMPLEX(kind=__IOTK_COMPLEX2), intent(in)  :: val (:,:,:,:,:)
  logical, optional, intent(in)  :: first
  integer, optional, intent(out) :: ierr
end subroutine iotk_write_attr_COMPLEX2_5
#endif
# 85 "iotk_attr_interf.spp"
#if 6 <= __IOTK_MAXRANK
subroutine iotk_write_attr_COMPLEX2_6(attr,name,val,first,ierr)
  use iotk_base
  implicit none
  character(*), intent(inout) :: attr
  character(*), intent(in)    :: name
# 92 "iotk_attr_interf.spp"
  COMPLEX(kind=__IOTK_COMPLEX2), intent(in)  :: val (:,:,:,:,:,:)
  logical, optional, intent(in)  :: first
  integer, optional, intent(out) :: ierr
end subroutine iotk_write_attr_COMPLEX2_6
#endif
# 85 "iotk_attr_interf.spp"
#if 7 <= __IOTK_MAXRANK
subroutine iotk_write_attr_COMPLEX2_7(attr,name,val,first,ierr)
  use iotk_base
  implicit none
  character(*), intent(inout) :: attr
  character(*), intent(in)    :: name
# 92 "iotk_attr_interf.spp"
  COMPLEX(kind=__IOTK_COMPLEX2), intent(in)  :: val (:,:,:,:,:,:,:)
  logical, optional, intent(in)  :: first
  integer, optional, intent(out) :: ierr
end subroutine iotk_write_attr_COMPLEX2_7
#endif
# 99 "iotk_attr_interf.spp"
#endif
# 82 "iotk_attr_interf.spp"
#ifdef __IOTK_COMPLEX3
# 85 "iotk_attr_interf.spp"
#if 0 <= __IOTK_MAXRANK
subroutine iotk_write_attr_COMPLEX3_0(attr,name,val,first,ierr)
  use iotk_base
  implicit none
  character(*), intent(inout) :: attr
  character(*), intent(in)    :: name
# 92 "iotk_attr_interf.spp"
  COMPLEX(kind=__IOTK_COMPLEX3), intent(in)  :: val 
  logical, optional, intent(in)  :: first
  integer, optional, intent(out) :: ierr
end subroutine iotk_write_attr_COMPLEX3_0
#endif
# 85 "iotk_attr_interf.spp"
#if 1 <= __IOTK_MAXRANK
subroutine iotk_write_attr_COMPLEX3_1(attr,name,val,first,ierr)
  use iotk_base
  implicit none
  character(*), intent(inout) :: attr
  character(*), intent(in)    :: name
# 92 "iotk_attr_interf.spp"
  COMPLEX(kind=__IOTK_COMPLEX3), intent(in)  :: val (:)
  logical, optional, intent(in)  :: first
  integer, optional, intent(out) :: ierr
end subroutine iotk_write_attr_COMPLEX3_1
#endif
# 85 "iotk_attr_interf.spp"
#if 2 <= __IOTK_MAXRANK
subroutine iotk_write_attr_COMPLEX3_2(attr,name,val,first,ierr)
  use iotk_base
  implicit none
  character(*), intent(inout) :: attr
  character(*), intent(in)    :: name
# 92 "iotk_attr_interf.spp"
  COMPLEX(kind=__IOTK_COMPLEX3), intent(in)  :: val (:,:)
  logical, optional, intent(in)  :: first
  integer, optional, intent(out) :: ierr
end subroutine iotk_write_attr_COMPLEX3_2
#endif
# 85 "iotk_attr_interf.spp"
#if 3 <= __IOTK_MAXRANK
subroutine iotk_write_attr_COMPLEX3_3(attr,name,val,first,ierr)
  use iotk_base
  implicit none
  character(*), intent(inout) :: attr
  character(*), intent(in)    :: name
# 92 "iotk_attr_interf.spp"
  COMPLEX(kind=__IOTK_COMPLEX3), intent(in)  :: val (:,:,:)
  logical, optional, intent(in)  :: first
  integer, optional, intent(out) :: ierr
end subroutine iotk_write_attr_COMPLEX3_3
#endif
# 85 "iotk_attr_interf.spp"
#if 4 <= __IOTK_MAXRANK
subroutine iotk_write_attr_COMPLEX3_4(attr,name,val,first,ierr)
  use iotk_base
  implicit none
  character(*), intent(inout) :: attr
  character(*), intent(in)    :: name
# 92 "iotk_attr_interf.spp"
  COMPLEX(kind=__IOTK_COMPLEX3), intent(in)  :: val (:,:,:,:)
  logical, optional, intent(in)  :: first
  integer, optional, intent(out) :: ierr
end subroutine iotk_write_attr_COMPLEX3_4
#endif
# 85 "iotk_attr_interf.spp"
#if 5 <= __IOTK_MAXRANK
subroutine iotk_write_attr_COMPLEX3_5(attr,name,val,first,ierr)
  use iotk_base
  implicit none
  character(*), intent(inout) :: attr
  character(*), intent(in)    :: name
# 92 "iotk_attr_interf.spp"
  COMPLEX(kind=__IOTK_COMPLEX3), intent(in)  :: val (:,:,:,:,:)
  logical, optional, intent(in)  :: first
  integer, optional, intent(out) :: ierr
end subroutine iotk_write_attr_COMPLEX3_5
#endif
# 85 "iotk_attr_interf.spp"
#if 6 <= __IOTK_MAXRANK
subroutine iotk_write_attr_COMPLEX3_6(attr,name,val,first,ierr)
  use iotk_base
  implicit none
  character(*), intent(inout) :: attr
  character(*), intent(in)    :: name
# 92 "iotk_attr_interf.spp"
  COMPLEX(kind=__IOTK_COMPLEX3), intent(in)  :: val (:,:,:,:,:,:)
  logical, optional, intent(in)  :: first
  integer, optional, intent(out) :: ierr
end subroutine iotk_write_attr_COMPLEX3_6
#endif
# 85 "iotk_attr_interf.spp"
#if 7 <= __IOTK_MAXRANK
subroutine iotk_write_attr_COMPLEX3_7(attr,name,val,first,ierr)
  use iotk_base
  implicit none
  character(*), intent(inout) :: attr
  character(*), intent(in)    :: name
# 92 "iotk_attr_interf.spp"
  COMPLEX(kind=__IOTK_COMPLEX3), intent(in)  :: val (:,:,:,:,:,:,:)
  logical, optional, intent(in)  :: first
  integer, optional, intent(out) :: ierr
end subroutine iotk_write_attr_COMPLEX3_7
#endif
# 99 "iotk_attr_interf.spp"
#endif
# 82 "iotk_attr_interf.spp"
#ifdef __IOTK_COMPLEX4
# 85 "iotk_attr_interf.spp"
#if 0 <= __IOTK_MAXRANK
subroutine iotk_write_attr_COMPLEX4_0(attr,name,val,first,ierr)
  use iotk_base
  implicit none
  character(*), intent(inout) :: attr
  character(*), intent(in)    :: name
# 92 "iotk_attr_interf.spp"
  COMPLEX(kind=__IOTK_COMPLEX4), intent(in)  :: val 
  logical, optional, intent(in)  :: first
  integer, optional, intent(out) :: ierr
end subroutine iotk_write_attr_COMPLEX4_0
#endif
# 85 "iotk_attr_interf.spp"
#if 1 <= __IOTK_MAXRANK
subroutine iotk_write_attr_COMPLEX4_1(attr,name,val,first,ierr)
  use iotk_base
  implicit none
  character(*), intent(inout) :: attr
  character(*), intent(in)    :: name
# 92 "iotk_attr_interf.spp"
  COMPLEX(kind=__IOTK_COMPLEX4), intent(in)  :: val (:)
  logical, optional, intent(in)  :: first
  integer, optional, intent(out) :: ierr
end subroutine iotk_write_attr_COMPLEX4_1
#endif
# 85 "iotk_attr_interf.spp"
#if 2 <= __IOTK_MAXRANK
subroutine iotk_write_attr_COMPLEX4_2(attr,name,val,first,ierr)
  use iotk_base
  implicit none
  character(*), intent(inout) :: attr
  character(*), intent(in)    :: name
# 92 "iotk_attr_interf.spp"
  COMPLEX(kind=__IOTK_COMPLEX4), intent(in)  :: val (:,:)
  logical, optional, intent(in)  :: first
  integer, optional, intent(out) :: ierr
end subroutine iotk_write_attr_COMPLEX4_2
#endif
# 85 "iotk_attr_interf.spp"
#if 3 <= __IOTK_MAXRANK
subroutine iotk_write_attr_COMPLEX4_3(attr,name,val,first,ierr)
  use iotk_base
  implicit none
  character(*), intent(inout) :: attr
  character(*), intent(in)    :: name
# 92 "iotk_attr_interf.spp"
  COMPLEX(kind=__IOTK_COMPLEX4), intent(in)  :: val (:,:,:)
  logical, optional, intent(in)  :: first
  integer, optional, intent(out) :: ierr
end subroutine iotk_write_attr_COMPLEX4_3
#endif
# 85 "iotk_attr_interf.spp"
#if 4 <= __IOTK_MAXRANK
subroutine iotk_write_attr_COMPLEX4_4(attr,name,val,first,ierr)
  use iotk_base
  implicit none
  character(*), intent(inout) :: attr
  character(*), intent(in)    :: name
# 92 "iotk_attr_interf.spp"
  COMPLEX(kind=__IOTK_COMPLEX4), intent(in)  :: val (:,:,:,:)
  logical, optional, intent(in)  :: first
  integer, optional, intent(out) :: ierr
end subroutine iotk_write_attr_COMPLEX4_4
#endif
# 85 "iotk_attr_interf.spp"
#if 5 <= __IOTK_MAXRANK
subroutine iotk_write_attr_COMPLEX4_5(attr,name,val,first,ierr)
  use iotk_base
  implicit none
  character(*), intent(inout) :: attr
  character(*), intent(in)    :: name
# 92 "iotk_attr_interf.spp"
  COMPLEX(kind=__IOTK_COMPLEX4), intent(in)  :: val (:,:,:,:,:)
  logical, optional, intent(in)  :: first
  integer, optional, intent(out) :: ierr
end subroutine iotk_write_attr_COMPLEX4_5
#endif
# 85 "iotk_attr_interf.spp"
#if 6 <= __IOTK_MAXRANK
subroutine iotk_write_attr_COMPLEX4_6(attr,name,val,first,ierr)
  use iotk_base
  implicit none
  character(*), intent(inout) :: attr
  character(*), intent(in)    :: name
# 92 "iotk_attr_interf.spp"
  COMPLEX(kind=__IOTK_COMPLEX4), intent(in)  :: val (:,:,:,:,:,:)
  logical, optional, intent(in)  :: first
  integer, optional, intent(out) :: ierr
end subroutine iotk_write_attr_COMPLEX4_6
#endif
# 85 "iotk_attr_interf.spp"
#if 7 <= __IOTK_MAXRANK
subroutine iotk_write_attr_COMPLEX4_7(attr,name,val,first,ierr)
  use iotk_base
  implicit none
  character(*), intent(inout) :: attr
  character(*), intent(in)    :: name
# 92 "iotk_attr_interf.spp"
  COMPLEX(kind=__IOTK_COMPLEX4), intent(in)  :: val (:,:,:,:,:,:,:)
  logical, optional, intent(in)  :: first
  integer, optional, intent(out) :: ierr
end subroutine iotk_write_attr_COMPLEX4_7
#endif
# 99 "iotk_attr_interf.spp"
#endif
# 82 "iotk_attr_interf.spp"
#ifdef __IOTK_CHARACTER1
# 85 "iotk_attr_interf.spp"
#if 0 <= __IOTK_MAXRANK
subroutine iotk_write_attr_CHARACTER1_0(attr,name,val,first,ierr)
  use iotk_base
  implicit none
  character(*), intent(inout) :: attr
  character(*), intent(in)    :: name
# 92 "iotk_attr_interf.spp"
  CHARACTER(kind=__IOTK_CHARACTER1,len=*), intent(in)  :: val 
  logical, optional, intent(in)  :: first
  integer, optional, intent(out) :: ierr
end subroutine iotk_write_attr_CHARACTER1_0
#endif
# 99 "iotk_attr_interf.spp"
#endif
# 103 "iotk_attr_interf.spp"
end interface

interface iotk_scan_attr
# 109 "iotk_attr_interf.spp"
#ifdef __IOTK_LOGICAL1
# 112 "iotk_attr_interf.spp"
#if 0 <= __IOTK_MAXRANK
subroutine iotk_scan_attr_LOGICAL1_0(attr,name,val,found,default,eos,ierr)
  use iotk_base
  implicit none
  character(*),             intent(in)  :: attr
  character(*),             intent(in)  :: name
# 119 "iotk_attr_interf.spp"
  LOGICAL(kind=__IOTK_LOGICAL1),           intent(out) :: val 
  logical,        optional, intent(out) :: found
  LOGICAL(kind=__IOTK_LOGICAL1), optional, intent(in)  :: default 
  logical,        optional, intent(in)  :: eos
  integer,        optional, intent(out) :: ierr
end subroutine iotk_scan_attr_LOGICAL1_0
#endif
# 112 "iotk_attr_interf.spp"
#if 1 <= __IOTK_MAXRANK
subroutine iotk_scan_attr_LOGICAL1_1(attr,name,val,found,default,eos,ierr)
  use iotk_base
  implicit none
  character(*),             intent(in)  :: attr
  character(*),             intent(in)  :: name
# 119 "iotk_attr_interf.spp"
  LOGICAL(kind=__IOTK_LOGICAL1),           intent(out) :: val (:)
  logical,        optional, intent(out) :: found
  LOGICAL(kind=__IOTK_LOGICAL1), optional, intent(in)  :: default (:)
  logical,        optional, intent(in)  :: eos
  integer,        optional, intent(out) :: ierr
end subroutine iotk_scan_attr_LOGICAL1_1
#endif
# 112 "iotk_attr_interf.spp"
#if 2 <= __IOTK_MAXRANK
subroutine iotk_scan_attr_LOGICAL1_2(attr,name,val,found,default,eos,ierr)
  use iotk_base
  implicit none
  character(*),             intent(in)  :: attr
  character(*),             intent(in)  :: name
# 119 "iotk_attr_interf.spp"
  LOGICAL(kind=__IOTK_LOGICAL1),           intent(out) :: val (:,:)
  logical,        optional, intent(out) :: found
  LOGICAL(kind=__IOTK_LOGICAL1), optional, intent(in)  :: default (:,:)
  logical,        optional, intent(in)  :: eos
  integer,        optional, intent(out) :: ierr
end subroutine iotk_scan_attr_LOGICAL1_2
#endif
# 112 "iotk_attr_interf.spp"
#if 3 <= __IOTK_MAXRANK
subroutine iotk_scan_attr_LOGICAL1_3(attr,name,val,found,default,eos,ierr)
  use iotk_base
  implicit none
  character(*),             intent(in)  :: attr
  character(*),             intent(in)  :: name
# 119 "iotk_attr_interf.spp"
  LOGICAL(kind=__IOTK_LOGICAL1),           intent(out) :: val (:,:,:)
  logical,        optional, intent(out) :: found
  LOGICAL(kind=__IOTK_LOGICAL1), optional, intent(in)  :: default (:,:,:)
  logical,        optional, intent(in)  :: eos
  integer,        optional, intent(out) :: ierr
end subroutine iotk_scan_attr_LOGICAL1_3
#endif
# 112 "iotk_attr_interf.spp"
#if 4 <= __IOTK_MAXRANK
subroutine iotk_scan_attr_LOGICAL1_4(attr,name,val,found,default,eos,ierr)
  use iotk_base
  implicit none
  character(*),             intent(in)  :: attr
  character(*),             intent(in)  :: name
# 119 "iotk_attr_interf.spp"
  LOGICAL(kind=__IOTK_LOGICAL1),           intent(out) :: val (:,:,:,:)
  logical,        optional, intent(out) :: found
  LOGICAL(kind=__IOTK_LOGICAL1), optional, intent(in)  :: default (:,:,:,:)
  logical,        optional, intent(in)  :: eos
  integer,        optional, intent(out) :: ierr
end subroutine iotk_scan_attr_LOGICAL1_4
#endif
# 112 "iotk_attr_interf.spp"
#if 5 <= __IOTK_MAXRANK
subroutine iotk_scan_attr_LOGICAL1_5(attr,name,val,found,default,eos,ierr)
  use iotk_base
  implicit none
  character(*),             intent(in)  :: attr
  character(*),             intent(in)  :: name
# 119 "iotk_attr_interf.spp"
  LOGICAL(kind=__IOTK_LOGICAL1),           intent(out) :: val (:,:,:,:,:)
  logical,        optional, intent(out) :: found
  LOGICAL(kind=__IOTK_LOGICAL1), optional, intent(in)  :: default (:,:,:,:,:)
  logical,        optional, intent(in)  :: eos
  integer,        optional, intent(out) :: ierr
end subroutine iotk_scan_attr_LOGICAL1_5
#endif
# 112 "iotk_attr_interf.spp"
#if 6 <= __IOTK_MAXRANK
subroutine iotk_scan_attr_LOGICAL1_6(attr,name,val,found,default,eos,ierr)
  use iotk_base
  implicit none
  character(*),             intent(in)  :: attr
  character(*),             intent(in)  :: name
# 119 "iotk_attr_interf.spp"
  LOGICAL(kind=__IOTK_LOGICAL1),           intent(out) :: val (:,:,:,:,:,:)
  logical,        optional, intent(out) :: found
  LOGICAL(kind=__IOTK_LOGICAL1), optional, intent(in)  :: default (:,:,:,:,:,:)
  logical,        optional, intent(in)  :: eos
  integer,        optional, intent(out) :: ierr
end subroutine iotk_scan_attr_LOGICAL1_6
#endif
# 112 "iotk_attr_interf.spp"
#if 7 <= __IOTK_MAXRANK
subroutine iotk_scan_attr_LOGICAL1_7(attr,name,val,found,default,eos,ierr)
  use iotk_base
  implicit none
  character(*),             intent(in)  :: attr
  character(*),             intent(in)  :: name
# 119 "iotk_attr_interf.spp"
  LOGICAL(kind=__IOTK_LOGICAL1),           intent(out) :: val (:,:,:,:,:,:,:)
  logical,        optional, intent(out) :: found
  LOGICAL(kind=__IOTK_LOGICAL1), optional, intent(in)  :: default (:,:,:,:,:,:,:)
  logical,        optional, intent(in)  :: eos
  integer,        optional, intent(out) :: ierr
end subroutine iotk_scan_attr_LOGICAL1_7
#endif
# 128 "iotk_attr_interf.spp"
#endif
# 109 "iotk_attr_interf.spp"
#ifdef __IOTK_LOGICAL2
# 112 "iotk_attr_interf.spp"
#if 0 <= __IOTK_MAXRANK
subroutine iotk_scan_attr_LOGICAL2_0(attr,name,val,found,default,eos,ierr)
  use iotk_base
  implicit none
  character(*),             intent(in)  :: attr
  character(*),             intent(in)  :: name
# 119 "iotk_attr_interf.spp"
  LOGICAL(kind=__IOTK_LOGICAL2),           intent(out) :: val 
  logical,        optional, intent(out) :: found
  LOGICAL(kind=__IOTK_LOGICAL2), optional, intent(in)  :: default 
  logical,        optional, intent(in)  :: eos
  integer,        optional, intent(out) :: ierr
end subroutine iotk_scan_attr_LOGICAL2_0
#endif
# 112 "iotk_attr_interf.spp"
#if 1 <= __IOTK_MAXRANK
subroutine iotk_scan_attr_LOGICAL2_1(attr,name,val,found,default,eos,ierr)
  use iotk_base
  implicit none
  character(*),             intent(in)  :: attr
  character(*),             intent(in)  :: name
# 119 "iotk_attr_interf.spp"
  LOGICAL(kind=__IOTK_LOGICAL2),           intent(out) :: val (:)
  logical,        optional, intent(out) :: found
  LOGICAL(kind=__IOTK_LOGICAL2), optional, intent(in)  :: default (:)
  logical,        optional, intent(in)  :: eos
  integer,        optional, intent(out) :: ierr
end subroutine iotk_scan_attr_LOGICAL2_1
#endif
# 112 "iotk_attr_interf.spp"
#if 2 <= __IOTK_MAXRANK
subroutine iotk_scan_attr_LOGICAL2_2(attr,name,val,found,default,eos,ierr)
  use iotk_base
  implicit none
  character(*),             intent(in)  :: attr
  character(*),             intent(in)  :: name
# 119 "iotk_attr_interf.spp"
  LOGICAL(kind=__IOTK_LOGICAL2),           intent(out) :: val (:,:)
  logical,        optional, intent(out) :: found
  LOGICAL(kind=__IOTK_LOGICAL2), optional, intent(in)  :: default (:,:)
  logical,        optional, intent(in)  :: eos
  integer,        optional, intent(out) :: ierr
end subroutine iotk_scan_attr_LOGICAL2_2
#endif
# 112 "iotk_attr_interf.spp"
#if 3 <= __IOTK_MAXRANK
subroutine iotk_scan_attr_LOGICAL2_3(attr,name,val,found,default,eos,ierr)
  use iotk_base
  implicit none
  character(*),             intent(in)  :: attr
  character(*),             intent(in)  :: name
# 119 "iotk_attr_interf.spp"
  LOGICAL(kind=__IOTK_LOGICAL2),           intent(out) :: val (:,:,:)
  logical,        optional, intent(out) :: found
  LOGICAL(kind=__IOTK_LOGICAL2), optional, intent(in)  :: default (:,:,:)
  logical,        optional, intent(in)  :: eos
  integer,        optional, intent(out) :: ierr
end subroutine iotk_scan_attr_LOGICAL2_3
#endif
# 112 "iotk_attr_interf.spp"
#if 4 <= __IOTK_MAXRANK
subroutine iotk_scan_attr_LOGICAL2_4(attr,name,val,found,default,eos,ierr)
  use iotk_base
  implicit none
  character(*),             intent(in)  :: attr
  character(*),             intent(in)  :: name
# 119 "iotk_attr_interf.spp"
  LOGICAL(kind=__IOTK_LOGICAL2),           intent(out) :: val (:,:,:,:)
  logical,        optional, intent(out) :: found
  LOGICAL(kind=__IOTK_LOGICAL2), optional, intent(in)  :: default (:,:,:,:)
  logical,        optional, intent(in)  :: eos
  integer,        optional, intent(out) :: ierr
end subroutine iotk_scan_attr_LOGICAL2_4
#endif
# 112 "iotk_attr_interf.spp"
#if 5 <= __IOTK_MAXRANK
subroutine iotk_scan_attr_LOGICAL2_5(attr,name,val,found,default,eos,ierr)
  use iotk_base
  implicit none
  character(*),             intent(in)  :: attr
  character(*),             intent(in)  :: name
# 119 "iotk_attr_interf.spp"
  LOGICAL(kind=__IOTK_LOGICAL2),           intent(out) :: val (:,:,:,:,:)
  logical,        optional, intent(out) :: found
  LOGICAL(kind=__IOTK_LOGICAL2), optional, intent(in)  :: default (:,:,:,:,:)
  logical,        optional, intent(in)  :: eos
  integer,        optional, intent(out) :: ierr
end subroutine iotk_scan_attr_LOGICAL2_5
#endif
# 112 "iotk_attr_interf.spp"
#if 6 <= __IOTK_MAXRANK
subroutine iotk_scan_attr_LOGICAL2_6(attr,name,val,found,default,eos,ierr)
  use iotk_base
  implicit none
  character(*),             intent(in)  :: attr
  character(*),             intent(in)  :: name
# 119 "iotk_attr_interf.spp"
  LOGICAL(kind=__IOTK_LOGICAL2),           intent(out) :: val (:,:,:,:,:,:)
  logical,        optional, intent(out) :: found
  LOGICAL(kind=__IOTK_LOGICAL2), optional, intent(in)  :: default (:,:,:,:,:,:)
  logical,        optional, intent(in)  :: eos
  integer,        optional, intent(out) :: ierr
end subroutine iotk_scan_attr_LOGICAL2_6
#endif
# 112 "iotk_attr_interf.spp"
#if 7 <= __IOTK_MAXRANK
subroutine iotk_scan_attr_LOGICAL2_7(attr,name,val,found,default,eos,ierr)
  use iotk_base
  implicit none
  character(*),             intent(in)  :: attr
  character(*),             intent(in)  :: name
# 119 "iotk_attr_interf.spp"
  LOGICAL(kind=__IOTK_LOGICAL2),           intent(out) :: val (:,:,:,:,:,:,:)
  logical,        optional, intent(out) :: found
  LOGICAL(kind=__IOTK_LOGICAL2), optional, intent(in)  :: default (:,:,:,:,:,:,:)
  logical,        optional, intent(in)  :: eos
  integer,        optional, intent(out) :: ierr
end subroutine iotk_scan_attr_LOGICAL2_7
#endif
# 128 "iotk_attr_interf.spp"
#endif
# 109 "iotk_attr_interf.spp"
#ifdef __IOTK_LOGICAL3
# 112 "iotk_attr_interf.spp"
#if 0 <= __IOTK_MAXRANK
subroutine iotk_scan_attr_LOGICAL3_0(attr,name,val,found,default,eos,ierr)
  use iotk_base
  implicit none
  character(*),             intent(in)  :: attr
  character(*),             intent(in)  :: name
# 119 "iotk_attr_interf.spp"
  LOGICAL(kind=__IOTK_LOGICAL3),           intent(out) :: val 
  logical,        optional, intent(out) :: found
  LOGICAL(kind=__IOTK_LOGICAL3), optional, intent(in)  :: default 
  logical,        optional, intent(in)  :: eos
  integer,        optional, intent(out) :: ierr
end subroutine iotk_scan_attr_LOGICAL3_0
#endif
# 112 "iotk_attr_interf.spp"
#if 1 <= __IOTK_MAXRANK
subroutine iotk_scan_attr_LOGICAL3_1(attr,name,val,found,default,eos,ierr)
  use iotk_base
  implicit none
  character(*),             intent(in)  :: attr
  character(*),             intent(in)  :: name
# 119 "iotk_attr_interf.spp"
  LOGICAL(kind=__IOTK_LOGICAL3),           intent(out) :: val (:)
  logical,        optional, intent(out) :: found
  LOGICAL(kind=__IOTK_LOGICAL3), optional, intent(in)  :: default (:)
  logical,        optional, intent(in)  :: eos
  integer,        optional, intent(out) :: ierr
end subroutine iotk_scan_attr_LOGICAL3_1
#endif
# 112 "iotk_attr_interf.spp"
#if 2 <= __IOTK_MAXRANK
subroutine iotk_scan_attr_LOGICAL3_2(attr,name,val,found,default,eos,ierr)
  use iotk_base
  implicit none
  character(*),             intent(in)  :: attr
  character(*),             intent(in)  :: name
# 119 "iotk_attr_interf.spp"
  LOGICAL(kind=__IOTK_LOGICAL3),           intent(out) :: val (:,:)
  logical,        optional, intent(out) :: found
  LOGICAL(kind=__IOTK_LOGICAL3), optional, intent(in)  :: default (:,:)
  logical,        optional, intent(in)  :: eos
  integer,        optional, intent(out) :: ierr
end subroutine iotk_scan_attr_LOGICAL3_2
#endif
# 112 "iotk_attr_interf.spp"
#if 3 <= __IOTK_MAXRANK
subroutine iotk_scan_attr_LOGICAL3_3(attr,name,val,found,default,eos,ierr)
  use iotk_base
  implicit none
  character(*),             intent(in)  :: attr
  character(*),             intent(in)  :: name
# 119 "iotk_attr_interf.spp"
  LOGICAL(kind=__IOTK_LOGICAL3),           intent(out) :: val (:,:,:)
  logical,        optional, intent(out) :: found
  LOGICAL(kind=__IOTK_LOGICAL3), optional, intent(in)  :: default (:,:,:)
  logical,        optional, intent(in)  :: eos
  integer,        optional, intent(out) :: ierr
end subroutine iotk_scan_attr_LOGICAL3_3
#endif
# 112 "iotk_attr_interf.spp"
#if 4 <= __IOTK_MAXRANK
subroutine iotk_scan_attr_LOGICAL3_4(attr,name,val,found,default,eos,ierr)
  use iotk_base
  implicit none
  character(*),             intent(in)  :: attr
  character(*),             intent(in)  :: name
# 119 "iotk_attr_interf.spp"
  LOGICAL(kind=__IOTK_LOGICAL3),           intent(out) :: val (:,:,:,:)
  logical,        optional, intent(out) :: found
  LOGICAL(kind=__IOTK_LOGICAL3), optional, intent(in)  :: default (:,:,:,:)
  logical,        optional, intent(in)  :: eos
  integer,        optional, intent(out) :: ierr
end subroutine iotk_scan_attr_LOGICAL3_4
#endif
# 112 "iotk_attr_interf.spp"
#if 5 <= __IOTK_MAXRANK
subroutine iotk_scan_attr_LOGICAL3_5(attr,name,val,found,default,eos,ierr)
  use iotk_base
  implicit none
  character(*),             intent(in)  :: attr
  character(*),             intent(in)  :: name
# 119 "iotk_attr_interf.spp"
  LOGICAL(kind=__IOTK_LOGICAL3),           intent(out) :: val (:,:,:,:,:)
  logical,        optional, intent(out) :: found
  LOGICAL(kind=__IOTK_LOGICAL3), optional, intent(in)  :: default (:,:,:,:,:)
  logical,        optional, intent(in)  :: eos
  integer,        optional, intent(out) :: ierr
end subroutine iotk_scan_attr_LOGICAL3_5
#endif
# 112 "iotk_attr_interf.spp"
#if 6 <= __IOTK_MAXRANK
subroutine iotk_scan_attr_LOGICAL3_6(attr,name,val,found,default,eos,ierr)
  use iotk_base
  implicit none
  character(*),             intent(in)  :: attr
  character(*),             intent(in)  :: name
# 119 "iotk_attr_interf.spp"
  LOGICAL(kind=__IOTK_LOGICAL3),           intent(out) :: val (:,:,:,:,:,:)
  logical,        optional, intent(out) :: found
  LOGICAL(kind=__IOTK_LOGICAL3), optional, intent(in)  :: default (:,:,:,:,:,:)
  logical,        optional, intent(in)  :: eos
  integer,        optional, intent(out) :: ierr
end subroutine iotk_scan_attr_LOGICAL3_6
#endif
# 112 "iotk_attr_interf.spp"
#if 7 <= __IOTK_MAXRANK
subroutine iotk_scan_attr_LOGICAL3_7(attr,name,val,found,default,eos,ierr)
  use iotk_base
  implicit none
  character(*),             intent(in)  :: attr
  character(*),             intent(in)  :: name
# 119 "iotk_attr_interf.spp"
  LOGICAL(kind=__IOTK_LOGICAL3),           intent(out) :: val (:,:,:,:,:,:,:)
  logical,        optional, intent(out) :: found
  LOGICAL(kind=__IOTK_LOGICAL3), optional, intent(in)  :: default (:,:,:,:,:,:,:)
  logical,        optional, intent(in)  :: eos
  integer,        optional, intent(out) :: ierr
end subroutine iotk_scan_attr_LOGICAL3_7
#endif
# 128 "iotk_attr_interf.spp"
#endif
# 109 "iotk_attr_interf.spp"
#ifdef __IOTK_LOGICAL4
# 112 "iotk_attr_interf.spp"
#if 0 <= __IOTK_MAXRANK
subroutine iotk_scan_attr_LOGICAL4_0(attr,name,val,found,default,eos,ierr)
  use iotk_base
  implicit none
  character(*),             intent(in)  :: attr
  character(*),             intent(in)  :: name
# 119 "iotk_attr_interf.spp"
  LOGICAL(kind=__IOTK_LOGICAL4),           intent(out) :: val 
  logical,        optional, intent(out) :: found
  LOGICAL(kind=__IOTK_LOGICAL4), optional, intent(in)  :: default 
  logical,        optional, intent(in)  :: eos
  integer,        optional, intent(out) :: ierr
end subroutine iotk_scan_attr_LOGICAL4_0
#endif
# 112 "iotk_attr_interf.spp"
#if 1 <= __IOTK_MAXRANK
subroutine iotk_scan_attr_LOGICAL4_1(attr,name,val,found,default,eos,ierr)
  use iotk_base
  implicit none
  character(*),             intent(in)  :: attr
  character(*),             intent(in)  :: name
# 119 "iotk_attr_interf.spp"
  LOGICAL(kind=__IOTK_LOGICAL4),           intent(out) :: val (:)
  logical,        optional, intent(out) :: found
  LOGICAL(kind=__IOTK_LOGICAL4), optional, intent(in)  :: default (:)
  logical,        optional, intent(in)  :: eos
  integer,        optional, intent(out) :: ierr
end subroutine iotk_scan_attr_LOGICAL4_1
#endif
# 112 "iotk_attr_interf.spp"
#if 2 <= __IOTK_MAXRANK
subroutine iotk_scan_attr_LOGICAL4_2(attr,name,val,found,default,eos,ierr)
  use iotk_base
  implicit none
  character(*),             intent(in)  :: attr
  character(*),             intent(in)  :: name
# 119 "iotk_attr_interf.spp"
  LOGICAL(kind=__IOTK_LOGICAL4),           intent(out) :: val (:,:)
  logical,        optional, intent(out) :: found
  LOGICAL(kind=__IOTK_LOGICAL4), optional, intent(in)  :: default (:,:)
  logical,        optional, intent(in)  :: eos
  integer,        optional, intent(out) :: ierr
end subroutine iotk_scan_attr_LOGICAL4_2
#endif
# 112 "iotk_attr_interf.spp"
#if 3 <= __IOTK_MAXRANK
subroutine iotk_scan_attr_LOGICAL4_3(attr,name,val,found,default,eos,ierr)
  use iotk_base
  implicit none
  character(*),             intent(in)  :: attr
  character(*),             intent(in)  :: name
# 119 "iotk_attr_interf.spp"
  LOGICAL(kind=__IOTK_LOGICAL4),           intent(out) :: val (:,:,:)
  logical,        optional, intent(out) :: found
  LOGICAL(kind=__IOTK_LOGICAL4), optional, intent(in)  :: default (:,:,:)
  logical,        optional, intent(in)  :: eos
  integer,        optional, intent(out) :: ierr
end subroutine iotk_scan_attr_LOGICAL4_3
#endif
# 112 "iotk_attr_interf.spp"
#if 4 <= __IOTK_MAXRANK
subroutine iotk_scan_attr_LOGICAL4_4(attr,name,val,found,default,eos,ierr)
  use iotk_base
  implicit none
  character(*),             intent(in)  :: attr
  character(*),             intent(in)  :: name
# 119 "iotk_attr_interf.spp"
  LOGICAL(kind=__IOTK_LOGICAL4),           intent(out) :: val (:,:,:,:)
  logical,        optional, intent(out) :: found
  LOGICAL(kind=__IOTK_LOGICAL4), optional, intent(in)  :: default (:,:,:,:)
  logical,        optional, intent(in)  :: eos
  integer,        optional, intent(out) :: ierr
end subroutine iotk_scan_attr_LOGICAL4_4
#endif
# 112 "iotk_attr_interf.spp"
#if 5 <= __IOTK_MAXRANK
subroutine iotk_scan_attr_LOGICAL4_5(attr,name,val,found,default,eos,ierr)
  use iotk_base
  implicit none
  character(*),             intent(in)  :: attr
  character(*),             intent(in)  :: name
# 119 "iotk_attr_interf.spp"
  LOGICAL(kind=__IOTK_LOGICAL4),           intent(out) :: val (:,:,:,:,:)
  logical,        optional, intent(out) :: found
  LOGICAL(kind=__IOTK_LOGICAL4), optional, intent(in)  :: default (:,:,:,:,:)
  logical,        optional, intent(in)  :: eos
  integer,        optional, intent(out) :: ierr
end subroutine iotk_scan_attr_LOGICAL4_5
#endif
# 112 "iotk_attr_interf.spp"
#if 6 <= __IOTK_MAXRANK
subroutine iotk_scan_attr_LOGICAL4_6(attr,name,val,found,default,eos,ierr)
  use iotk_base
  implicit none
  character(*),             intent(in)  :: attr
  character(*),             intent(in)  :: name
# 119 "iotk_attr_interf.spp"
  LOGICAL(kind=__IOTK_LOGICAL4),           intent(out) :: val (:,:,:,:,:,:)
  logical,        optional, intent(out) :: found
  LOGICAL(kind=__IOTK_LOGICAL4), optional, intent(in)  :: default (:,:,:,:,:,:)
  logical,        optional, intent(in)  :: eos
  integer,        optional, intent(out) :: ierr
end subroutine iotk_scan_attr_LOGICAL4_6
#endif
# 112 "iotk_attr_interf.spp"
#if 7 <= __IOTK_MAXRANK
subroutine iotk_scan_attr_LOGICAL4_7(attr,name,val,found,default,eos,ierr)
  use iotk_base
  implicit none
  character(*),             intent(in)  :: attr
  character(*),             intent(in)  :: name
# 119 "iotk_attr_interf.spp"
  LOGICAL(kind=__IOTK_LOGICAL4),           intent(out) :: val (:,:,:,:,:,:,:)
  logical,        optional, intent(out) :: found
  LOGICAL(kind=__IOTK_LOGICAL4), optional, intent(in)  :: default (:,:,:,:,:,:,:)
  logical,        optional, intent(in)  :: eos
  integer,        optional, intent(out) :: ierr
end subroutine iotk_scan_attr_LOGICAL4_7
#endif
# 128 "iotk_attr_interf.spp"
#endif
# 109 "iotk_attr_interf.spp"
#ifdef __IOTK_INTEGER1
# 112 "iotk_attr_interf.spp"
#if 0 <= __IOTK_MAXRANK
subroutine iotk_scan_attr_INTEGER1_0(attr,name,val,found,default,eos,ierr)
  use iotk_base
  implicit none
  character(*),             intent(in)  :: attr
  character(*),             intent(in)  :: name
# 119 "iotk_attr_interf.spp"
  INTEGER(kind=__IOTK_INTEGER1),           intent(out) :: val 
  logical,        optional, intent(out) :: found
  INTEGER(kind=__IOTK_INTEGER1), optional, intent(in)  :: default 
  logical,        optional, intent(in)  :: eos
  integer,        optional, intent(out) :: ierr
end subroutine iotk_scan_attr_INTEGER1_0
#endif
# 112 "iotk_attr_interf.spp"
#if 1 <= __IOTK_MAXRANK
subroutine iotk_scan_attr_INTEGER1_1(attr,name,val,found,default,eos,ierr)
  use iotk_base
  implicit none
  character(*),             intent(in)  :: attr
  character(*),             intent(in)  :: name
# 119 "iotk_attr_interf.spp"
  INTEGER(kind=__IOTK_INTEGER1),           intent(out) :: val (:)
  logical,        optional, intent(out) :: found
  INTEGER(kind=__IOTK_INTEGER1), optional, intent(in)  :: default (:)
  logical,        optional, intent(in)  :: eos
  integer,        optional, intent(out) :: ierr
end subroutine iotk_scan_attr_INTEGER1_1
#endif
# 112 "iotk_attr_interf.spp"
#if 2 <= __IOTK_MAXRANK
subroutine iotk_scan_attr_INTEGER1_2(attr,name,val,found,default,eos,ierr)
  use iotk_base
  implicit none
  character(*),             intent(in)  :: attr
  character(*),             intent(in)  :: name
# 119 "iotk_attr_interf.spp"
  INTEGER(kind=__IOTK_INTEGER1),           intent(out) :: val (:,:)
  logical,        optional, intent(out) :: found
  INTEGER(kind=__IOTK_INTEGER1), optional, intent(in)  :: default (:,:)
  logical,        optional, intent(in)  :: eos
  integer,        optional, intent(out) :: ierr
end subroutine iotk_scan_attr_INTEGER1_2
#endif
# 112 "iotk_attr_interf.spp"
#if 3 <= __IOTK_MAXRANK
subroutine iotk_scan_attr_INTEGER1_3(attr,name,val,found,default,eos,ierr)
  use iotk_base
  implicit none
  character(*),             intent(in)  :: attr
  character(*),             intent(in)  :: name
# 119 "iotk_attr_interf.spp"
  INTEGER(kind=__IOTK_INTEGER1),           intent(out) :: val (:,:,:)
  logical,        optional, intent(out) :: found
  INTEGER(kind=__IOTK_INTEGER1), optional, intent(in)  :: default (:,:,:)
  logical,        optional, intent(in)  :: eos
  integer,        optional, intent(out) :: ierr
end subroutine iotk_scan_attr_INTEGER1_3
#endif
# 112 "iotk_attr_interf.spp"
#if 4 <= __IOTK_MAXRANK
subroutine iotk_scan_attr_INTEGER1_4(attr,name,val,found,default,eos,ierr)
  use iotk_base
  implicit none
  character(*),             intent(in)  :: attr
  character(*),             intent(in)  :: name
# 119 "iotk_attr_interf.spp"
  INTEGER(kind=__IOTK_INTEGER1),           intent(out) :: val (:,:,:,:)
  logical,        optional, intent(out) :: found
  INTEGER(kind=__IOTK_INTEGER1), optional, intent(in)  :: default (:,:,:,:)
  logical,        optional, intent(in)  :: eos
  integer,        optional, intent(out) :: ierr
end subroutine iotk_scan_attr_INTEGER1_4
#endif
# 112 "iotk_attr_interf.spp"
#if 5 <= __IOTK_MAXRANK
subroutine iotk_scan_attr_INTEGER1_5(attr,name,val,found,default,eos,ierr)
  use iotk_base
  implicit none
  character(*),             intent(in)  :: attr
  character(*),             intent(in)  :: name
# 119 "iotk_attr_interf.spp"
  INTEGER(kind=__IOTK_INTEGER1),           intent(out) :: val (:,:,:,:,:)
  logical,        optional, intent(out) :: found
  INTEGER(kind=__IOTK_INTEGER1), optional, intent(in)  :: default (:,:,:,:,:)
  logical,        optional, intent(in)  :: eos
  integer,        optional, intent(out) :: ierr
end subroutine iotk_scan_attr_INTEGER1_5
#endif
# 112 "iotk_attr_interf.spp"
#if 6 <= __IOTK_MAXRANK
subroutine iotk_scan_attr_INTEGER1_6(attr,name,val,found,default,eos,ierr)
  use iotk_base
  implicit none
  character(*),             intent(in)  :: attr
  character(*),             intent(in)  :: name
# 119 "iotk_attr_interf.spp"
  INTEGER(kind=__IOTK_INTEGER1),           intent(out) :: val (:,:,:,:,:,:)
  logical,        optional, intent(out) :: found
  INTEGER(kind=__IOTK_INTEGER1), optional, intent(in)  :: default (:,:,:,:,:,:)
  logical,        optional, intent(in)  :: eos
  integer,        optional, intent(out) :: ierr
end subroutine iotk_scan_attr_INTEGER1_6
#endif
# 112 "iotk_attr_interf.spp"
#if 7 <= __IOTK_MAXRANK
subroutine iotk_scan_attr_INTEGER1_7(attr,name,val,found,default,eos,ierr)
  use iotk_base
  implicit none
  character(*),             intent(in)  :: attr
  character(*),             intent(in)  :: name
# 119 "iotk_attr_interf.spp"
  INTEGER(kind=__IOTK_INTEGER1),           intent(out) :: val (:,:,:,:,:,:,:)
  logical,        optional, intent(out) :: found
  INTEGER(kind=__IOTK_INTEGER1), optional, intent(in)  :: default (:,:,:,:,:,:,:)
  logical,        optional, intent(in)  :: eos
  integer,        optional, intent(out) :: ierr
end subroutine iotk_scan_attr_INTEGER1_7
#endif
# 128 "iotk_attr_interf.spp"
#endif
# 109 "iotk_attr_interf.spp"
#ifdef __IOTK_INTEGER2
# 112 "iotk_attr_interf.spp"
#if 0 <= __IOTK_MAXRANK
subroutine iotk_scan_attr_INTEGER2_0(attr,name,val,found,default,eos,ierr)
  use iotk_base
  implicit none
  character(*),             intent(in)  :: attr
  character(*),             intent(in)  :: name
# 119 "iotk_attr_interf.spp"
  INTEGER(kind=__IOTK_INTEGER2),           intent(out) :: val 
  logical,        optional, intent(out) :: found
  INTEGER(kind=__IOTK_INTEGER2), optional, intent(in)  :: default 
  logical,        optional, intent(in)  :: eos
  integer,        optional, intent(out) :: ierr
end subroutine iotk_scan_attr_INTEGER2_0
#endif
# 112 "iotk_attr_interf.spp"
#if 1 <= __IOTK_MAXRANK
subroutine iotk_scan_attr_INTEGER2_1(attr,name,val,found,default,eos,ierr)
  use iotk_base
  implicit none
  character(*),             intent(in)  :: attr
  character(*),             intent(in)  :: name
# 119 "iotk_attr_interf.spp"
  INTEGER(kind=__IOTK_INTEGER2),           intent(out) :: val (:)
  logical,        optional, intent(out) :: found
  INTEGER(kind=__IOTK_INTEGER2), optional, intent(in)  :: default (:)
  logical,        optional, intent(in)  :: eos
  integer,        optional, intent(out) :: ierr
end subroutine iotk_scan_attr_INTEGER2_1
#endif
# 112 "iotk_attr_interf.spp"
#if 2 <= __IOTK_MAXRANK
subroutine iotk_scan_attr_INTEGER2_2(attr,name,val,found,default,eos,ierr)
  use iotk_base
  implicit none
  character(*),             intent(in)  :: attr
  character(*),             intent(in)  :: name
# 119 "iotk_attr_interf.spp"
  INTEGER(kind=__IOTK_INTEGER2),           intent(out) :: val (:,:)
  logical,        optional, intent(out) :: found
  INTEGER(kind=__IOTK_INTEGER2), optional, intent(in)  :: default (:,:)
  logical,        optional, intent(in)  :: eos
  integer,        optional, intent(out) :: ierr
end subroutine iotk_scan_attr_INTEGER2_2
#endif
# 112 "iotk_attr_interf.spp"
#if 3 <= __IOTK_MAXRANK
subroutine iotk_scan_attr_INTEGER2_3(attr,name,val,found,default,eos,ierr)
  use iotk_base
  implicit none
  character(*),             intent(in)  :: attr
  character(*),             intent(in)  :: name
# 119 "iotk_attr_interf.spp"
  INTEGER(kind=__IOTK_INTEGER2),           intent(out) :: val (:,:,:)
  logical,        optional, intent(out) :: found
  INTEGER(kind=__IOTK_INTEGER2), optional, intent(in)  :: default (:,:,:)
  logical,        optional, intent(in)  :: eos
  integer,        optional, intent(out) :: ierr
end subroutine iotk_scan_attr_INTEGER2_3
#endif
# 112 "iotk_attr_interf.spp"
#if 4 <= __IOTK_MAXRANK
subroutine iotk_scan_attr_INTEGER2_4(attr,name,val,found,default,eos,ierr)
  use iotk_base
  implicit none
  character(*),             intent(in)  :: attr
  character(*),             intent(in)  :: name
# 119 "iotk_attr_interf.spp"
  INTEGER(kind=__IOTK_INTEGER2),           intent(out) :: val (:,:,:,:)
  logical,        optional, intent(out) :: found
  INTEGER(kind=__IOTK_INTEGER2), optional, intent(in)  :: default (:,:,:,:)
  logical,        optional, intent(in)  :: eos
  integer,        optional, intent(out) :: ierr
end subroutine iotk_scan_attr_INTEGER2_4
#endif
# 112 "iotk_attr_interf.spp"
#if 5 <= __IOTK_MAXRANK
subroutine iotk_scan_attr_INTEGER2_5(attr,name,val,found,default,eos,ierr)
  use iotk_base
  implicit none
  character(*),             intent(in)  :: attr
  character(*),             intent(in)  :: name
# 119 "iotk_attr_interf.spp"
  INTEGER(kind=__IOTK_INTEGER2),           intent(out) :: val (:,:,:,:,:)
  logical,        optional, intent(out) :: found
  INTEGER(kind=__IOTK_INTEGER2), optional, intent(in)  :: default (:,:,:,:,:)
  logical,        optional, intent(in)  :: eos
  integer,        optional, intent(out) :: ierr
end subroutine iotk_scan_attr_INTEGER2_5
#endif
# 112 "iotk_attr_interf.spp"
#if 6 <= __IOTK_MAXRANK
subroutine iotk_scan_attr_INTEGER2_6(attr,name,val,found,default,eos,ierr)
  use iotk_base
  implicit none
  character(*),             intent(in)  :: attr
  character(*),             intent(in)  :: name
# 119 "iotk_attr_interf.spp"
  INTEGER(kind=__IOTK_INTEGER2),           intent(out) :: val (:,:,:,:,:,:)
  logical,        optional, intent(out) :: found
  INTEGER(kind=__IOTK_INTEGER2), optional, intent(in)  :: default (:,:,:,:,:,:)
  logical,        optional, intent(in)  :: eos
  integer,        optional, intent(out) :: ierr
end subroutine iotk_scan_attr_INTEGER2_6
#endif
# 112 "iotk_attr_interf.spp"
#if 7 <= __IOTK_MAXRANK
subroutine iotk_scan_attr_INTEGER2_7(attr,name,val,found,default,eos,ierr)
  use iotk_base
  implicit none
  character(*),             intent(in)  :: attr
  character(*),             intent(in)  :: name
# 119 "iotk_attr_interf.spp"
  INTEGER(kind=__IOTK_INTEGER2),           intent(out) :: val (:,:,:,:,:,:,:)
  logical,        optional, intent(out) :: found
  INTEGER(kind=__IOTK_INTEGER2), optional, intent(in)  :: default (:,:,:,:,:,:,:)
  logical,        optional, intent(in)  :: eos
  integer,        optional, intent(out) :: ierr
end subroutine iotk_scan_attr_INTEGER2_7
#endif
# 128 "iotk_attr_interf.spp"
#endif
# 109 "iotk_attr_interf.spp"
#ifdef __IOTK_INTEGER3
# 112 "iotk_attr_interf.spp"
#if 0 <= __IOTK_MAXRANK
subroutine iotk_scan_attr_INTEGER3_0(attr,name,val,found,default,eos,ierr)
  use iotk_base
  implicit none
  character(*),             intent(in)  :: attr
  character(*),             intent(in)  :: name
# 119 "iotk_attr_interf.spp"
  INTEGER(kind=__IOTK_INTEGER3),           intent(out) :: val 
  logical,        optional, intent(out) :: found
  INTEGER(kind=__IOTK_INTEGER3), optional, intent(in)  :: default 
  logical,        optional, intent(in)  :: eos
  integer,        optional, intent(out) :: ierr
end subroutine iotk_scan_attr_INTEGER3_0
#endif
# 112 "iotk_attr_interf.spp"
#if 1 <= __IOTK_MAXRANK
subroutine iotk_scan_attr_INTEGER3_1(attr,name,val,found,default,eos,ierr)
  use iotk_base
  implicit none
  character(*),             intent(in)  :: attr
  character(*),             intent(in)  :: name
# 119 "iotk_attr_interf.spp"
  INTEGER(kind=__IOTK_INTEGER3),           intent(out) :: val (:)
  logical,        optional, intent(out) :: found
  INTEGER(kind=__IOTK_INTEGER3), optional, intent(in)  :: default (:)
  logical,        optional, intent(in)  :: eos
  integer,        optional, intent(out) :: ierr
end subroutine iotk_scan_attr_INTEGER3_1
#endif
# 112 "iotk_attr_interf.spp"
#if 2 <= __IOTK_MAXRANK
subroutine iotk_scan_attr_INTEGER3_2(attr,name,val,found,default,eos,ierr)
  use iotk_base
  implicit none
  character(*),             intent(in)  :: attr
  character(*),             intent(in)  :: name
# 119 "iotk_attr_interf.spp"
  INTEGER(kind=__IOTK_INTEGER3),           intent(out) :: val (:,:)
  logical,        optional, intent(out) :: found
  INTEGER(kind=__IOTK_INTEGER3), optional, intent(in)  :: default (:,:)
  logical,        optional, intent(in)  :: eos
  integer,        optional, intent(out) :: ierr
end subroutine iotk_scan_attr_INTEGER3_2
#endif
# 112 "iotk_attr_interf.spp"
#if 3 <= __IOTK_MAXRANK
subroutine iotk_scan_attr_INTEGER3_3(attr,name,val,found,default,eos,ierr)
  use iotk_base
  implicit none
  character(*),             intent(in)  :: attr
  character(*),             intent(in)  :: name
# 119 "iotk_attr_interf.spp"
  INTEGER(kind=__IOTK_INTEGER3),           intent(out) :: val (:,:,:)
  logical,        optional, intent(out) :: found
  INTEGER(kind=__IOTK_INTEGER3), optional, intent(in)  :: default (:,:,:)
  logical,        optional, intent(in)  :: eos
  integer,        optional, intent(out) :: ierr
end subroutine iotk_scan_attr_INTEGER3_3
#endif
# 112 "iotk_attr_interf.spp"
#if 4 <= __IOTK_MAXRANK
subroutine iotk_scan_attr_INTEGER3_4(attr,name,val,found,default,eos,ierr)
  use iotk_base
  implicit none
  character(*),             intent(in)  :: attr
  character(*),             intent(in)  :: name
# 119 "iotk_attr_interf.spp"
  INTEGER(kind=__IOTK_INTEGER3),           intent(out) :: val (:,:,:,:)
  logical,        optional, intent(out) :: found
  INTEGER(kind=__IOTK_INTEGER3), optional, intent(in)  :: default (:,:,:,:)
  logical,        optional, intent(in)  :: eos
  integer,        optional, intent(out) :: ierr
end subroutine iotk_scan_attr_INTEGER3_4
#endif
# 112 "iotk_attr_interf.spp"
#if 5 <= __IOTK_MAXRANK
subroutine iotk_scan_attr_INTEGER3_5(attr,name,val,found,default,eos,ierr)
  use iotk_base
  implicit none
  character(*),             intent(in)  :: attr
  character(*),             intent(in)  :: name
# 119 "iotk_attr_interf.spp"
  INTEGER(kind=__IOTK_INTEGER3),           intent(out) :: val (:,:,:,:,:)
  logical,        optional, intent(out) :: found
  INTEGER(kind=__IOTK_INTEGER3), optional, intent(in)  :: default (:,:,:,:,:)
  logical,        optional, intent(in)  :: eos
  integer,        optional, intent(out) :: ierr
end subroutine iotk_scan_attr_INTEGER3_5
#endif
# 112 "iotk_attr_interf.spp"
#if 6 <= __IOTK_MAXRANK
subroutine iotk_scan_attr_INTEGER3_6(attr,name,val,found,default,eos,ierr)
  use iotk_base
  implicit none
  character(*),             intent(in)  :: attr
  character(*),             intent(in)  :: name
# 119 "iotk_attr_interf.spp"
  INTEGER(kind=__IOTK_INTEGER3),           intent(out) :: val (:,:,:,:,:,:)
  logical,        optional, intent(out) :: found
  INTEGER(kind=__IOTK_INTEGER3), optional, intent(in)  :: default (:,:,:,:,:,:)
  logical,        optional, intent(in)  :: eos
  integer,        optional, intent(out) :: ierr
end subroutine iotk_scan_attr_INTEGER3_6
#endif
# 112 "iotk_attr_interf.spp"
#if 7 <= __IOTK_MAXRANK
subroutine iotk_scan_attr_INTEGER3_7(attr,name,val,found,default,eos,ierr)
  use iotk_base
  implicit none
  character(*),             intent(in)  :: attr
  character(*),             intent(in)  :: name
# 119 "iotk_attr_interf.spp"
  INTEGER(kind=__IOTK_INTEGER3),           intent(out) :: val (:,:,:,:,:,:,:)
  logical,        optional, intent(out) :: found
  INTEGER(kind=__IOTK_INTEGER3), optional, intent(in)  :: default (:,:,:,:,:,:,:)
  logical,        optional, intent(in)  :: eos
  integer,        optional, intent(out) :: ierr
end subroutine iotk_scan_attr_INTEGER3_7
#endif
# 128 "iotk_attr_interf.spp"
#endif
# 109 "iotk_attr_interf.spp"
#ifdef __IOTK_INTEGER4
# 112 "iotk_attr_interf.spp"
#if 0 <= __IOTK_MAXRANK
subroutine iotk_scan_attr_INTEGER4_0(attr,name,val,found,default,eos,ierr)
  use iotk_base
  implicit none
  character(*),             intent(in)  :: attr
  character(*),             intent(in)  :: name
# 119 "iotk_attr_interf.spp"
  INTEGER(kind=__IOTK_INTEGER4),           intent(out) :: val 
  logical,        optional, intent(out) :: found
  INTEGER(kind=__IOTK_INTEGER4), optional, intent(in)  :: default 
  logical,        optional, intent(in)  :: eos
  integer,        optional, intent(out) :: ierr
end subroutine iotk_scan_attr_INTEGER4_0
#endif
# 112 "iotk_attr_interf.spp"
#if 1 <= __IOTK_MAXRANK
subroutine iotk_scan_attr_INTEGER4_1(attr,name,val,found,default,eos,ierr)
  use iotk_base
  implicit none
  character(*),             intent(in)  :: attr
  character(*),             intent(in)  :: name
# 119 "iotk_attr_interf.spp"
  INTEGER(kind=__IOTK_INTEGER4),           intent(out) :: val (:)
  logical,        optional, intent(out) :: found
  INTEGER(kind=__IOTK_INTEGER4), optional, intent(in)  :: default (:)
  logical,        optional, intent(in)  :: eos
  integer,        optional, intent(out) :: ierr
end subroutine iotk_scan_attr_INTEGER4_1
#endif
# 112 "iotk_attr_interf.spp"
#if 2 <= __IOTK_MAXRANK
subroutine iotk_scan_attr_INTEGER4_2(attr,name,val,found,default,eos,ierr)
  use iotk_base
  implicit none
  character(*),             intent(in)  :: attr
  character(*),             intent(in)  :: name
# 119 "iotk_attr_interf.spp"
  INTEGER(kind=__IOTK_INTEGER4),           intent(out) :: val (:,:)
  logical,        optional, intent(out) :: found
  INTEGER(kind=__IOTK_INTEGER4), optional, intent(in)  :: default (:,:)
  logical,        optional, intent(in)  :: eos
  integer,        optional, intent(out) :: ierr
end subroutine iotk_scan_attr_INTEGER4_2
#endif
# 112 "iotk_attr_interf.spp"
#if 3 <= __IOTK_MAXRANK
subroutine iotk_scan_attr_INTEGER4_3(attr,name,val,found,default,eos,ierr)
  use iotk_base
  implicit none
  character(*),             intent(in)  :: attr
  character(*),             intent(in)  :: name
# 119 "iotk_attr_interf.spp"
  INTEGER(kind=__IOTK_INTEGER4),           intent(out) :: val (:,:,:)
  logical,        optional, intent(out) :: found
  INTEGER(kind=__IOTK_INTEGER4), optional, intent(in)  :: default (:,:,:)
  logical,        optional, intent(in)  :: eos
  integer,        optional, intent(out) :: ierr
end subroutine iotk_scan_attr_INTEGER4_3
#endif
# 112 "iotk_attr_interf.spp"
#if 4 <= __IOTK_MAXRANK
subroutine iotk_scan_attr_INTEGER4_4(attr,name,val,found,default,eos,ierr)
  use iotk_base
  implicit none
  character(*),             intent(in)  :: attr
  character(*),             intent(in)  :: name
# 119 "iotk_attr_interf.spp"
  INTEGER(kind=__IOTK_INTEGER4),           intent(out) :: val (:,:,:,:)
  logical,        optional, intent(out) :: found
  INTEGER(kind=__IOTK_INTEGER4), optional, intent(in)  :: default (:,:,:,:)
  logical,        optional, intent(in)  :: eos
  integer,        optional, intent(out) :: ierr
end subroutine iotk_scan_attr_INTEGER4_4
#endif
# 112 "iotk_attr_interf.spp"
#if 5 <= __IOTK_MAXRANK
subroutine iotk_scan_attr_INTEGER4_5(attr,name,val,found,default,eos,ierr)
  use iotk_base
  implicit none
  character(*),             intent(in)  :: attr
  character(*),             intent(in)  :: name
# 119 "iotk_attr_interf.spp"
  INTEGER(kind=__IOTK_INTEGER4),           intent(out) :: val (:,:,:,:,:)
  logical,        optional, intent(out) :: found
  INTEGER(kind=__IOTK_INTEGER4), optional, intent(in)  :: default (:,:,:,:,:)
  logical,        optional, intent(in)  :: eos
  integer,        optional, intent(out) :: ierr
end subroutine iotk_scan_attr_INTEGER4_5
#endif
# 112 "iotk_attr_interf.spp"
#if 6 <= __IOTK_MAXRANK
subroutine iotk_scan_attr_INTEGER4_6(attr,name,val,found,default,eos,ierr)
  use iotk_base
  implicit none
  character(*),             intent(in)  :: attr
  character(*),             intent(in)  :: name
# 119 "iotk_attr_interf.spp"
  INTEGER(kind=__IOTK_INTEGER4),           intent(out) :: val (:,:,:,:,:,:)
  logical,        optional, intent(out) :: found
  INTEGER(kind=__IOTK_INTEGER4), optional, intent(in)  :: default (:,:,:,:,:,:)
  logical,        optional, intent(in)  :: eos
  integer,        optional, intent(out) :: ierr
end subroutine iotk_scan_attr_INTEGER4_6
#endif
# 112 "iotk_attr_interf.spp"
#if 7 <= __IOTK_MAXRANK
subroutine iotk_scan_attr_INTEGER4_7(attr,name,val,found,default,eos,ierr)
  use iotk_base
  implicit none
  character(*),             intent(in)  :: attr
  character(*),             intent(in)  :: name
# 119 "iotk_attr_interf.spp"
  INTEGER(kind=__IOTK_INTEGER4),           intent(out) :: val (:,:,:,:,:,:,:)
  logical,        optional, intent(out) :: found
  INTEGER(kind=__IOTK_INTEGER4), optional, intent(in)  :: default (:,:,:,:,:,:,:)
  logical,        optional, intent(in)  :: eos
  integer,        optional, intent(out) :: ierr
end subroutine iotk_scan_attr_INTEGER4_7
#endif
# 128 "iotk_attr_interf.spp"
#endif
# 109 "iotk_attr_interf.spp"
#ifdef __IOTK_REAL1
# 112 "iotk_attr_interf.spp"
#if 0 <= __IOTK_MAXRANK
subroutine iotk_scan_attr_REAL1_0(attr,name,val,found,default,eos,ierr)
  use iotk_base
  implicit none
  character(*),             intent(in)  :: attr
  character(*),             intent(in)  :: name
# 119 "iotk_attr_interf.spp"
  REAL(kind=__IOTK_REAL1),           intent(out) :: val 
  logical,        optional, intent(out) :: found
  REAL(kind=__IOTK_REAL1), optional, intent(in)  :: default 
  logical,        optional, intent(in)  :: eos
  integer,        optional, intent(out) :: ierr
end subroutine iotk_scan_attr_REAL1_0
#endif
# 112 "iotk_attr_interf.spp"
#if 1 <= __IOTK_MAXRANK
subroutine iotk_scan_attr_REAL1_1(attr,name,val,found,default,eos,ierr)
  use iotk_base
  implicit none
  character(*),             intent(in)  :: attr
  character(*),             intent(in)  :: name
# 119 "iotk_attr_interf.spp"
  REAL(kind=__IOTK_REAL1),           intent(out) :: val (:)
  logical,        optional, intent(out) :: found
  REAL(kind=__IOTK_REAL1), optional, intent(in)  :: default (:)
  logical,        optional, intent(in)  :: eos
  integer,        optional, intent(out) :: ierr
end subroutine iotk_scan_attr_REAL1_1
#endif
# 112 "iotk_attr_interf.spp"
#if 2 <= __IOTK_MAXRANK
subroutine iotk_scan_attr_REAL1_2(attr,name,val,found,default,eos,ierr)
  use iotk_base
  implicit none
  character(*),             intent(in)  :: attr
  character(*),             intent(in)  :: name
# 119 "iotk_attr_interf.spp"
  REAL(kind=__IOTK_REAL1),           intent(out) :: val (:,:)
  logical,        optional, intent(out) :: found
  REAL(kind=__IOTK_REAL1), optional, intent(in)  :: default (:,:)
  logical,        optional, intent(in)  :: eos
  integer,        optional, intent(out) :: ierr
end subroutine iotk_scan_attr_REAL1_2
#endif
# 112 "iotk_attr_interf.spp"
#if 3 <= __IOTK_MAXRANK
subroutine iotk_scan_attr_REAL1_3(attr,name,val,found,default,eos,ierr)
  use iotk_base
  implicit none
  character(*),             intent(in)  :: attr
  character(*),             intent(in)  :: name
# 119 "iotk_attr_interf.spp"
  REAL(kind=__IOTK_REAL1),           intent(out) :: val (:,:,:)
  logical,        optional, intent(out) :: found
  REAL(kind=__IOTK_REAL1), optional, intent(in)  :: default (:,:,:)
  logical,        optional, intent(in)  :: eos
  integer,        optional, intent(out) :: ierr
end subroutine iotk_scan_attr_REAL1_3
#endif
# 112 "iotk_attr_interf.spp"
#if 4 <= __IOTK_MAXRANK
subroutine iotk_scan_attr_REAL1_4(attr,name,val,found,default,eos,ierr)
  use iotk_base
  implicit none
  character(*),             intent(in)  :: attr
  character(*),             intent(in)  :: name
# 119 "iotk_attr_interf.spp"
  REAL(kind=__IOTK_REAL1),           intent(out) :: val (:,:,:,:)
  logical,        optional, intent(out) :: found
  REAL(kind=__IOTK_REAL1), optional, intent(in)  :: default (:,:,:,:)
  logical,        optional, intent(in)  :: eos
  integer,        optional, intent(out) :: ierr
end subroutine iotk_scan_attr_REAL1_4
#endif
# 112 "iotk_attr_interf.spp"
#if 5 <= __IOTK_MAXRANK
subroutine iotk_scan_attr_REAL1_5(attr,name,val,found,default,eos,ierr)
  use iotk_base
  implicit none
  character(*),             intent(in)  :: attr
  character(*),             intent(in)  :: name
# 119 "iotk_attr_interf.spp"
  REAL(kind=__IOTK_REAL1),           intent(out) :: val (:,:,:,:,:)
  logical,        optional, intent(out) :: found
  REAL(kind=__IOTK_REAL1), optional, intent(in)  :: default (:,:,:,:,:)
  logical,        optional, intent(in)  :: eos
  integer,        optional, intent(out) :: ierr
end subroutine iotk_scan_attr_REAL1_5
#endif
# 112 "iotk_attr_interf.spp"
#if 6 <= __IOTK_MAXRANK
subroutine iotk_scan_attr_REAL1_6(attr,name,val,found,default,eos,ierr)
  use iotk_base
  implicit none
  character(*),             intent(in)  :: attr
  character(*),             intent(in)  :: name
# 119 "iotk_attr_interf.spp"
  REAL(kind=__IOTK_REAL1),           intent(out) :: val (:,:,:,:,:,:)
  logical,        optional, intent(out) :: found
  REAL(kind=__IOTK_REAL1), optional, intent(in)  :: default (:,:,:,:,:,:)
  logical,        optional, intent(in)  :: eos
  integer,        optional, intent(out) :: ierr
end subroutine iotk_scan_attr_REAL1_6
#endif
# 112 "iotk_attr_interf.spp"
#if 7 <= __IOTK_MAXRANK
subroutine iotk_scan_attr_REAL1_7(attr,name,val,found,default,eos,ierr)
  use iotk_base
  implicit none
  character(*),             intent(in)  :: attr
  character(*),             intent(in)  :: name
# 119 "iotk_attr_interf.spp"
  REAL(kind=__IOTK_REAL1),           intent(out) :: val (:,:,:,:,:,:,:)
  logical,        optional, intent(out) :: found
  REAL(kind=__IOTK_REAL1), optional, intent(in)  :: default (:,:,:,:,:,:,:)
  logical,        optional, intent(in)  :: eos
  integer,        optional, intent(out) :: ierr
end subroutine iotk_scan_attr_REAL1_7
#endif
# 128 "iotk_attr_interf.spp"
#endif
# 109 "iotk_attr_interf.spp"
#ifdef __IOTK_REAL2
# 112 "iotk_attr_interf.spp"
#if 0 <= __IOTK_MAXRANK
subroutine iotk_scan_attr_REAL2_0(attr,name,val,found,default,eos,ierr)
  use iotk_base
  implicit none
  character(*),             intent(in)  :: attr
  character(*),             intent(in)  :: name
# 119 "iotk_attr_interf.spp"
  REAL(kind=__IOTK_REAL2),           intent(out) :: val 
  logical,        optional, intent(out) :: found
  REAL(kind=__IOTK_REAL2), optional, intent(in)  :: default 
  logical,        optional, intent(in)  :: eos
  integer,        optional, intent(out) :: ierr
end subroutine iotk_scan_attr_REAL2_0
#endif
# 112 "iotk_attr_interf.spp"
#if 1 <= __IOTK_MAXRANK
subroutine iotk_scan_attr_REAL2_1(attr,name,val,found,default,eos,ierr)
  use iotk_base
  implicit none
  character(*),             intent(in)  :: attr
  character(*),             intent(in)  :: name
# 119 "iotk_attr_interf.spp"
  REAL(kind=__IOTK_REAL2),           intent(out) :: val (:)
  logical,        optional, intent(out) :: found
  REAL(kind=__IOTK_REAL2), optional, intent(in)  :: default (:)
  logical,        optional, intent(in)  :: eos
  integer,        optional, intent(out) :: ierr
end subroutine iotk_scan_attr_REAL2_1
#endif
# 112 "iotk_attr_interf.spp"
#if 2 <= __IOTK_MAXRANK
subroutine iotk_scan_attr_REAL2_2(attr,name,val,found,default,eos,ierr)
  use iotk_base
  implicit none
  character(*),             intent(in)  :: attr
  character(*),             intent(in)  :: name
# 119 "iotk_attr_interf.spp"
  REAL(kind=__IOTK_REAL2),           intent(out) :: val (:,:)
  logical,        optional, intent(out) :: found
  REAL(kind=__IOTK_REAL2), optional, intent(in)  :: default (:,:)
  logical,        optional, intent(in)  :: eos
  integer,        optional, intent(out) :: ierr
end subroutine iotk_scan_attr_REAL2_2
#endif
# 112 "iotk_attr_interf.spp"
#if 3 <= __IOTK_MAXRANK
subroutine iotk_scan_attr_REAL2_3(attr,name,val,found,default,eos,ierr)
  use iotk_base
  implicit none
  character(*),             intent(in)  :: attr
  character(*),             intent(in)  :: name
# 119 "iotk_attr_interf.spp"
  REAL(kind=__IOTK_REAL2),           intent(out) :: val (:,:,:)
  logical,        optional, intent(out) :: found
  REAL(kind=__IOTK_REAL2), optional, intent(in)  :: default (:,:,:)
  logical,        optional, intent(in)  :: eos
  integer,        optional, intent(out) :: ierr
end subroutine iotk_scan_attr_REAL2_3
#endif
# 112 "iotk_attr_interf.spp"
#if 4 <= __IOTK_MAXRANK
subroutine iotk_scan_attr_REAL2_4(attr,name,val,found,default,eos,ierr)
  use iotk_base
  implicit none
  character(*),             intent(in)  :: attr
  character(*),             intent(in)  :: name
# 119 "iotk_attr_interf.spp"
  REAL(kind=__IOTK_REAL2),           intent(out) :: val (:,:,:,:)
  logical,        optional, intent(out) :: found
  REAL(kind=__IOTK_REAL2), optional, intent(in)  :: default (:,:,:,:)
  logical,        optional, intent(in)  :: eos
  integer,        optional, intent(out) :: ierr
end subroutine iotk_scan_attr_REAL2_4
#endif
# 112 "iotk_attr_interf.spp"
#if 5 <= __IOTK_MAXRANK
subroutine iotk_scan_attr_REAL2_5(attr,name,val,found,default,eos,ierr)
  use iotk_base
  implicit none
  character(*),             intent(in)  :: attr
  character(*),             intent(in)  :: name
# 119 "iotk_attr_interf.spp"
  REAL(kind=__IOTK_REAL2),           intent(out) :: val (:,:,:,:,:)
  logical,        optional, intent(out) :: found
  REAL(kind=__IOTK_REAL2), optional, intent(in)  :: default (:,:,:,:,:)
  logical,        optional, intent(in)  :: eos
  integer,        optional, intent(out) :: ierr
end subroutine iotk_scan_attr_REAL2_5
#endif
# 112 "iotk_attr_interf.spp"
#if 6 <= __IOTK_MAXRANK
subroutine iotk_scan_attr_REAL2_6(attr,name,val,found,default,eos,ierr)
  use iotk_base
  implicit none
  character(*),             intent(in)  :: attr
  character(*),             intent(in)  :: name
# 119 "iotk_attr_interf.spp"
  REAL(kind=__IOTK_REAL2),           intent(out) :: val (:,:,:,:,:,:)
  logical,        optional, intent(out) :: found
  REAL(kind=__IOTK_REAL2), optional, intent(in)  :: default (:,:,:,:,:,:)
  logical,        optional, intent(in)  :: eos
  integer,        optional, intent(out) :: ierr
end subroutine iotk_scan_attr_REAL2_6
#endif
# 112 "iotk_attr_interf.spp"
#if 7 <= __IOTK_MAXRANK
subroutine iotk_scan_attr_REAL2_7(attr,name,val,found,default,eos,ierr)
  use iotk_base
  implicit none
  character(*),             intent(in)  :: attr
  character(*),             intent(in)  :: name
# 119 "iotk_attr_interf.spp"
  REAL(kind=__IOTK_REAL2),           intent(out) :: val (:,:,:,:,:,:,:)
  logical,        optional, intent(out) :: found
  REAL(kind=__IOTK_REAL2), optional, intent(in)  :: default (:,:,:,:,:,:,:)
  logical,        optional, intent(in)  :: eos
  integer,        optional, intent(out) :: ierr
end subroutine iotk_scan_attr_REAL2_7
#endif
# 128 "iotk_attr_interf.spp"
#endif
# 109 "iotk_attr_interf.spp"
#ifdef __IOTK_REAL3
# 112 "iotk_attr_interf.spp"
#if 0 <= __IOTK_MAXRANK
subroutine iotk_scan_attr_REAL3_0(attr,name,val,found,default,eos,ierr)
  use iotk_base
  implicit none
  character(*),             intent(in)  :: attr
  character(*),             intent(in)  :: name
# 119 "iotk_attr_interf.spp"
  REAL(kind=__IOTK_REAL3),           intent(out) :: val 
  logical,        optional, intent(out) :: found
  REAL(kind=__IOTK_REAL3), optional, intent(in)  :: default 
  logical,        optional, intent(in)  :: eos
  integer,        optional, intent(out) :: ierr
end subroutine iotk_scan_attr_REAL3_0
#endif
# 112 "iotk_attr_interf.spp"
#if 1 <= __IOTK_MAXRANK
subroutine iotk_scan_attr_REAL3_1(attr,name,val,found,default,eos,ierr)
  use iotk_base
  implicit none
  character(*),             intent(in)  :: attr
  character(*),             intent(in)  :: name
# 119 "iotk_attr_interf.spp"
  REAL(kind=__IOTK_REAL3),           intent(out) :: val (:)
  logical,        optional, intent(out) :: found
  REAL(kind=__IOTK_REAL3), optional, intent(in)  :: default (:)
  logical,        optional, intent(in)  :: eos
  integer,        optional, intent(out) :: ierr
end subroutine iotk_scan_attr_REAL3_1
#endif
# 112 "iotk_attr_interf.spp"
#if 2 <= __IOTK_MAXRANK
subroutine iotk_scan_attr_REAL3_2(attr,name,val,found,default,eos,ierr)
  use iotk_base
  implicit none
  character(*),             intent(in)  :: attr
  character(*),             intent(in)  :: name
# 119 "iotk_attr_interf.spp"
  REAL(kind=__IOTK_REAL3),           intent(out) :: val (:,:)
  logical,        optional, intent(out) :: found
  REAL(kind=__IOTK_REAL3), optional, intent(in)  :: default (:,:)
  logical,        optional, intent(in)  :: eos
  integer,        optional, intent(out) :: ierr
end subroutine iotk_scan_attr_REAL3_2
#endif
# 112 "iotk_attr_interf.spp"
#if 3 <= __IOTK_MAXRANK
subroutine iotk_scan_attr_REAL3_3(attr,name,val,found,default,eos,ierr)
  use iotk_base
  implicit none
  character(*),             intent(in)  :: attr
  character(*),             intent(in)  :: name
# 119 "iotk_attr_interf.spp"
  REAL(kind=__IOTK_REAL3),           intent(out) :: val (:,:,:)
  logical,        optional, intent(out) :: found
  REAL(kind=__IOTK_REAL3), optional, intent(in)  :: default (:,:,:)
  logical,        optional, intent(in)  :: eos
  integer,        optional, intent(out) :: ierr
end subroutine iotk_scan_attr_REAL3_3
#endif
# 112 "iotk_attr_interf.spp"
#if 4 <= __IOTK_MAXRANK
subroutine iotk_scan_attr_REAL3_4(attr,name,val,found,default,eos,ierr)
  use iotk_base
  implicit none
  character(*),             intent(in)  :: attr
  character(*),             intent(in)  :: name
# 119 "iotk_attr_interf.spp"
  REAL(kind=__IOTK_REAL3),           intent(out) :: val (:,:,:,:)
  logical,        optional, intent(out) :: found
  REAL(kind=__IOTK_REAL3), optional, intent(in)  :: default (:,:,:,:)
  logical,        optional, intent(in)  :: eos
  integer,        optional, intent(out) :: ierr
end subroutine iotk_scan_attr_REAL3_4
#endif
# 112 "iotk_attr_interf.spp"
#if 5 <= __IOTK_MAXRANK
subroutine iotk_scan_attr_REAL3_5(attr,name,val,found,default,eos,ierr)
  use iotk_base
  implicit none
  character(*),             intent(in)  :: attr
  character(*),             intent(in)  :: name
# 119 "iotk_attr_interf.spp"
  REAL(kind=__IOTK_REAL3),           intent(out) :: val (:,:,:,:,:)
  logical,        optional, intent(out) :: found
  REAL(kind=__IOTK_REAL3), optional, intent(in)  :: default (:,:,:,:,:)
  logical,        optional, intent(in)  :: eos
  integer,        optional, intent(out) :: ierr
end subroutine iotk_scan_attr_REAL3_5
#endif
# 112 "iotk_attr_interf.spp"
#if 6 <= __IOTK_MAXRANK
subroutine iotk_scan_attr_REAL3_6(attr,name,val,found,default,eos,ierr)
  use iotk_base
  implicit none
  character(*),             intent(in)  :: attr
  character(*),             intent(in)  :: name
# 119 "iotk_attr_interf.spp"
  REAL(kind=__IOTK_REAL3),           intent(out) :: val (:,:,:,:,:,:)
  logical,        optional, intent(out) :: found
  REAL(kind=__IOTK_REAL3), optional, intent(in)  :: default (:,:,:,:,:,:)
  logical,        optional, intent(in)  :: eos
  integer,        optional, intent(out) :: ierr
end subroutine iotk_scan_attr_REAL3_6
#endif
# 112 "iotk_attr_interf.spp"
#if 7 <= __IOTK_MAXRANK
subroutine iotk_scan_attr_REAL3_7(attr,name,val,found,default,eos,ierr)
  use iotk_base
  implicit none
  character(*),             intent(in)  :: attr
  character(*),             intent(in)  :: name
# 119 "iotk_attr_interf.spp"
  REAL(kind=__IOTK_REAL3),           intent(out) :: val (:,:,:,:,:,:,:)
  logical,        optional, intent(out) :: found
  REAL(kind=__IOTK_REAL3), optional, intent(in)  :: default (:,:,:,:,:,:,:)
  logical,        optional, intent(in)  :: eos
  integer,        optional, intent(out) :: ierr
end subroutine iotk_scan_attr_REAL3_7
#endif
# 128 "iotk_attr_interf.spp"
#endif
# 109 "iotk_attr_interf.spp"
#ifdef __IOTK_REAL4
# 112 "iotk_attr_interf.spp"
#if 0 <= __IOTK_MAXRANK
subroutine iotk_scan_attr_REAL4_0(attr,name,val,found,default,eos,ierr)
  use iotk_base
  implicit none
  character(*),             intent(in)  :: attr
  character(*),             intent(in)  :: name
# 119 "iotk_attr_interf.spp"
  REAL(kind=__IOTK_REAL4),           intent(out) :: val 
  logical,        optional, intent(out) :: found
  REAL(kind=__IOTK_REAL4), optional, intent(in)  :: default 
  logical,        optional, intent(in)  :: eos
  integer,        optional, intent(out) :: ierr
end subroutine iotk_scan_attr_REAL4_0
#endif
# 112 "iotk_attr_interf.spp"
#if 1 <= __IOTK_MAXRANK
subroutine iotk_scan_attr_REAL4_1(attr,name,val,found,default,eos,ierr)
  use iotk_base
  implicit none
  character(*),             intent(in)  :: attr
  character(*),             intent(in)  :: name
# 119 "iotk_attr_interf.spp"
  REAL(kind=__IOTK_REAL4),           intent(out) :: val (:)
  logical,        optional, intent(out) :: found
  REAL(kind=__IOTK_REAL4), optional, intent(in)  :: default (:)
  logical,        optional, intent(in)  :: eos
  integer,        optional, intent(out) :: ierr
end subroutine iotk_scan_attr_REAL4_1
#endif
# 112 "iotk_attr_interf.spp"
#if 2 <= __IOTK_MAXRANK
subroutine iotk_scan_attr_REAL4_2(attr,name,val,found,default,eos,ierr)
  use iotk_base
  implicit none
  character(*),             intent(in)  :: attr
  character(*),             intent(in)  :: name
# 119 "iotk_attr_interf.spp"
  REAL(kind=__IOTK_REAL4),           intent(out) :: val (:,:)
  logical,        optional, intent(out) :: found
  REAL(kind=__IOTK_REAL4), optional, intent(in)  :: default (:,:)
  logical,        optional, intent(in)  :: eos
  integer,        optional, intent(out) :: ierr
end subroutine iotk_scan_attr_REAL4_2
#endif
# 112 "iotk_attr_interf.spp"
#if 3 <= __IOTK_MAXRANK
subroutine iotk_scan_attr_REAL4_3(attr,name,val,found,default,eos,ierr)
  use iotk_base
  implicit none
  character(*),             intent(in)  :: attr
  character(*),             intent(in)  :: name
# 119 "iotk_attr_interf.spp"
  REAL(kind=__IOTK_REAL4),           intent(out) :: val (:,:,:)
  logical,        optional, intent(out) :: found
  REAL(kind=__IOTK_REAL4), optional, intent(in)  :: default (:,:,:)
  logical,        optional, intent(in)  :: eos
  integer,        optional, intent(out) :: ierr
end subroutine iotk_scan_attr_REAL4_3
#endif
# 112 "iotk_attr_interf.spp"
#if 4 <= __IOTK_MAXRANK
subroutine iotk_scan_attr_REAL4_4(attr,name,val,found,default,eos,ierr)
  use iotk_base
  implicit none
  character(*),             intent(in)  :: attr
  character(*),             intent(in)  :: name
# 119 "iotk_attr_interf.spp"
  REAL(kind=__IOTK_REAL4),           intent(out) :: val (:,:,:,:)
  logical,        optional, intent(out) :: found
  REAL(kind=__IOTK_REAL4), optional, intent(in)  :: default (:,:,:,:)
  logical,        optional, intent(in)  :: eos
  integer,        optional, intent(out) :: ierr
end subroutine iotk_scan_attr_REAL4_4
#endif
# 112 "iotk_attr_interf.spp"
#if 5 <= __IOTK_MAXRANK
subroutine iotk_scan_attr_REAL4_5(attr,name,val,found,default,eos,ierr)
  use iotk_base
  implicit none
  character(*),             intent(in)  :: attr
  character(*),             intent(in)  :: name
# 119 "iotk_attr_interf.spp"
  REAL(kind=__IOTK_REAL4),           intent(out) :: val (:,:,:,:,:)
  logical,        optional, intent(out) :: found
  REAL(kind=__IOTK_REAL4), optional, intent(in)  :: default (:,:,:,:,:)
  logical,        optional, intent(in)  :: eos
  integer,        optional, intent(out) :: ierr
end subroutine iotk_scan_attr_REAL4_5
#endif
# 112 "iotk_attr_interf.spp"
#if 6 <= __IOTK_MAXRANK
subroutine iotk_scan_attr_REAL4_6(attr,name,val,found,default,eos,ierr)
  use iotk_base
  implicit none
  character(*),             intent(in)  :: attr
  character(*),             intent(in)  :: name
# 119 "iotk_attr_interf.spp"
  REAL(kind=__IOTK_REAL4),           intent(out) :: val (:,:,:,:,:,:)
  logical,        optional, intent(out) :: found
  REAL(kind=__IOTK_REAL4), optional, intent(in)  :: default (:,:,:,:,:,:)
  logical,        optional, intent(in)  :: eos
  integer,        optional, intent(out) :: ierr
end subroutine iotk_scan_attr_REAL4_6
#endif
# 112 "iotk_attr_interf.spp"
#if 7 <= __IOTK_MAXRANK
subroutine iotk_scan_attr_REAL4_7(attr,name,val,found,default,eos,ierr)
  use iotk_base
  implicit none
  character(*),             intent(in)  :: attr
  character(*),             intent(in)  :: name
# 119 "iotk_attr_interf.spp"
  REAL(kind=__IOTK_REAL4),           intent(out) :: val (:,:,:,:,:,:,:)
  logical,        optional, intent(out) :: found
  REAL(kind=__IOTK_REAL4), optional, intent(in)  :: default (:,:,:,:,:,:,:)
  logical,        optional, intent(in)  :: eos
  integer,        optional, intent(out) :: ierr
end subroutine iotk_scan_attr_REAL4_7
#endif
# 128 "iotk_attr_interf.spp"
#endif
# 109 "iotk_attr_interf.spp"
#ifdef __IOTK_COMPLEX1
# 112 "iotk_attr_interf.spp"
#if 0 <= __IOTK_MAXRANK
subroutine iotk_scan_attr_COMPLEX1_0(attr,name,val,found,default,eos,ierr)
  use iotk_base
  implicit none
  character(*),             intent(in)  :: attr
  character(*),             intent(in)  :: name
# 119 "iotk_attr_interf.spp"
  COMPLEX(kind=__IOTK_COMPLEX1),           intent(out) :: val 
  logical,        optional, intent(out) :: found
  COMPLEX(kind=__IOTK_COMPLEX1), optional, intent(in)  :: default 
  logical,        optional, intent(in)  :: eos
  integer,        optional, intent(out) :: ierr
end subroutine iotk_scan_attr_COMPLEX1_0
#endif
# 112 "iotk_attr_interf.spp"
#if 1 <= __IOTK_MAXRANK
subroutine iotk_scan_attr_COMPLEX1_1(attr,name,val,found,default,eos,ierr)
  use iotk_base
  implicit none
  character(*),             intent(in)  :: attr
  character(*),             intent(in)  :: name
# 119 "iotk_attr_interf.spp"
  COMPLEX(kind=__IOTK_COMPLEX1),           intent(out) :: val (:)
  logical,        optional, intent(out) :: found
  COMPLEX(kind=__IOTK_COMPLEX1), optional, intent(in)  :: default (:)
  logical,        optional, intent(in)  :: eos
  integer,        optional, intent(out) :: ierr
end subroutine iotk_scan_attr_COMPLEX1_1
#endif
# 112 "iotk_attr_interf.spp"
#if 2 <= __IOTK_MAXRANK
subroutine iotk_scan_attr_COMPLEX1_2(attr,name,val,found,default,eos,ierr)
  use iotk_base
  implicit none
  character(*),             intent(in)  :: attr
  character(*),             intent(in)  :: name
# 119 "iotk_attr_interf.spp"
  COMPLEX(kind=__IOTK_COMPLEX1),           intent(out) :: val (:,:)
  logical,        optional, intent(out) :: found
  COMPLEX(kind=__IOTK_COMPLEX1), optional, intent(in)  :: default (:,:)
  logical,        optional, intent(in)  :: eos
  integer,        optional, intent(out) :: ierr
end subroutine iotk_scan_attr_COMPLEX1_2
#endif
# 112 "iotk_attr_interf.spp"
#if 3 <= __IOTK_MAXRANK
subroutine iotk_scan_attr_COMPLEX1_3(attr,name,val,found,default,eos,ierr)
  use iotk_base
  implicit none
  character(*),             intent(in)  :: attr
  character(*),             intent(in)  :: name
# 119 "iotk_attr_interf.spp"
  COMPLEX(kind=__IOTK_COMPLEX1),           intent(out) :: val (:,:,:)
  logical,        optional, intent(out) :: found
  COMPLEX(kind=__IOTK_COMPLEX1), optional, intent(in)  :: default (:,:,:)
  logical,        optional, intent(in)  :: eos
  integer,        optional, intent(out) :: ierr
end subroutine iotk_scan_attr_COMPLEX1_3
#endif
# 112 "iotk_attr_interf.spp"
#if 4 <= __IOTK_MAXRANK
subroutine iotk_scan_attr_COMPLEX1_4(attr,name,val,found,default,eos,ierr)
  use iotk_base
  implicit none
  character(*),             intent(in)  :: attr
  character(*),             intent(in)  :: name
# 119 "iotk_attr_interf.spp"
  COMPLEX(kind=__IOTK_COMPLEX1),           intent(out) :: val (:,:,:,:)
  logical,        optional, intent(out) :: found
  COMPLEX(kind=__IOTK_COMPLEX1), optional, intent(in)  :: default (:,:,:,:)
  logical,        optional, intent(in)  :: eos
  integer,        optional, intent(out) :: ierr
end subroutine iotk_scan_attr_COMPLEX1_4
#endif
# 112 "iotk_attr_interf.spp"
#if 5 <= __IOTK_MAXRANK
subroutine iotk_scan_attr_COMPLEX1_5(attr,name,val,found,default,eos,ierr)
  use iotk_base
  implicit none
  character(*),             intent(in)  :: attr
  character(*),             intent(in)  :: name
# 119 "iotk_attr_interf.spp"
  COMPLEX(kind=__IOTK_COMPLEX1),           intent(out) :: val (:,:,:,:,:)
  logical,        optional, intent(out) :: found
  COMPLEX(kind=__IOTK_COMPLEX1), optional, intent(in)  :: default (:,:,:,:,:)
  logical,        optional, intent(in)  :: eos
  integer,        optional, intent(out) :: ierr
end subroutine iotk_scan_attr_COMPLEX1_5
#endif
# 112 "iotk_attr_interf.spp"
#if 6 <= __IOTK_MAXRANK
subroutine iotk_scan_attr_COMPLEX1_6(attr,name,val,found,default,eos,ierr)
  use iotk_base
  implicit none
  character(*),             intent(in)  :: attr
  character(*),             intent(in)  :: name
# 119 "iotk_attr_interf.spp"
  COMPLEX(kind=__IOTK_COMPLEX1),           intent(out) :: val (:,:,:,:,:,:)
  logical,        optional, intent(out) :: found
  COMPLEX(kind=__IOTK_COMPLEX1), optional, intent(in)  :: default (:,:,:,:,:,:)
  logical,        optional, intent(in)  :: eos
  integer,        optional, intent(out) :: ierr
end subroutine iotk_scan_attr_COMPLEX1_6
#endif
# 112 "iotk_attr_interf.spp"
#if 7 <= __IOTK_MAXRANK
subroutine iotk_scan_attr_COMPLEX1_7(attr,name,val,found,default,eos,ierr)
  use iotk_base
  implicit none
  character(*),             intent(in)  :: attr
  character(*),             intent(in)  :: name
# 119 "iotk_attr_interf.spp"
  COMPLEX(kind=__IOTK_COMPLEX1),           intent(out) :: val (:,:,:,:,:,:,:)
  logical,        optional, intent(out) :: found
  COMPLEX(kind=__IOTK_COMPLEX1), optional, intent(in)  :: default (:,:,:,:,:,:,:)
  logical,        optional, intent(in)  :: eos
  integer,        optional, intent(out) :: ierr
end subroutine iotk_scan_attr_COMPLEX1_7
#endif
# 128 "iotk_attr_interf.spp"
#endif
# 109 "iotk_attr_interf.spp"
#ifdef __IOTK_COMPLEX2
# 112 "iotk_attr_interf.spp"
#if 0 <= __IOTK_MAXRANK
subroutine iotk_scan_attr_COMPLEX2_0(attr,name,val,found,default,eos,ierr)
  use iotk_base
  implicit none
  character(*),             intent(in)  :: attr
  character(*),             intent(in)  :: name
# 119 "iotk_attr_interf.spp"
  COMPLEX(kind=__IOTK_COMPLEX2),           intent(out) :: val 
  logical,        optional, intent(out) :: found
  COMPLEX(kind=__IOTK_COMPLEX2), optional, intent(in)  :: default 
  logical,        optional, intent(in)  :: eos
  integer,        optional, intent(out) :: ierr
end subroutine iotk_scan_attr_COMPLEX2_0
#endif
# 112 "iotk_attr_interf.spp"
#if 1 <= __IOTK_MAXRANK
subroutine iotk_scan_attr_COMPLEX2_1(attr,name,val,found,default,eos,ierr)
  use iotk_base
  implicit none
  character(*),             intent(in)  :: attr
  character(*),             intent(in)  :: name
# 119 "iotk_attr_interf.spp"
  COMPLEX(kind=__IOTK_COMPLEX2),           intent(out) :: val (:)
  logical,        optional, intent(out) :: found
  COMPLEX(kind=__IOTK_COMPLEX2), optional, intent(in)  :: default (:)
  logical,        optional, intent(in)  :: eos
  integer,        optional, intent(out) :: ierr
end subroutine iotk_scan_attr_COMPLEX2_1
#endif
# 112 "iotk_attr_interf.spp"
#if 2 <= __IOTK_MAXRANK
subroutine iotk_scan_attr_COMPLEX2_2(attr,name,val,found,default,eos,ierr)
  use iotk_base
  implicit none
  character(*),             intent(in)  :: attr
  character(*),             intent(in)  :: name
# 119 "iotk_attr_interf.spp"
  COMPLEX(kind=__IOTK_COMPLEX2),           intent(out) :: val (:,:)
  logical,        optional, intent(out) :: found
  COMPLEX(kind=__IOTK_COMPLEX2), optional, intent(in)  :: default (:,:)
  logical,        optional, intent(in)  :: eos
  integer,        optional, intent(out) :: ierr
end subroutine iotk_scan_attr_COMPLEX2_2
#endif
# 112 "iotk_attr_interf.spp"
#if 3 <= __IOTK_MAXRANK
subroutine iotk_scan_attr_COMPLEX2_3(attr,name,val,found,default,eos,ierr)
  use iotk_base
  implicit none
  character(*),             intent(in)  :: attr
  character(*),             intent(in)  :: name
# 119 "iotk_attr_interf.spp"
  COMPLEX(kind=__IOTK_COMPLEX2),           intent(out) :: val (:,:,:)
  logical,        optional, intent(out) :: found
  COMPLEX(kind=__IOTK_COMPLEX2), optional, intent(in)  :: default (:,:,:)
  logical,        optional, intent(in)  :: eos
  integer,        optional, intent(out) :: ierr
end subroutine iotk_scan_attr_COMPLEX2_3
#endif
# 112 "iotk_attr_interf.spp"
#if 4 <= __IOTK_MAXRANK
subroutine iotk_scan_attr_COMPLEX2_4(attr,name,val,found,default,eos,ierr)
  use iotk_base
  implicit none
  character(*),             intent(in)  :: attr
  character(*),             intent(in)  :: name
# 119 "iotk_attr_interf.spp"
  COMPLEX(kind=__IOTK_COMPLEX2),           intent(out) :: val (:,:,:,:)
  logical,        optional, intent(out) :: found
  COMPLEX(kind=__IOTK_COMPLEX2), optional, intent(in)  :: default (:,:,:,:)
  logical,        optional, intent(in)  :: eos
  integer,        optional, intent(out) :: ierr
end subroutine iotk_scan_attr_COMPLEX2_4
#endif
# 112 "iotk_attr_interf.spp"
#if 5 <= __IOTK_MAXRANK
subroutine iotk_scan_attr_COMPLEX2_5(attr,name,val,found,default,eos,ierr)
  use iotk_base
  implicit none
  character(*),             intent(in)  :: attr
  character(*),             intent(in)  :: name
# 119 "iotk_attr_interf.spp"
  COMPLEX(kind=__IOTK_COMPLEX2),           intent(out) :: val (:,:,:,:,:)
  logical,        optional, intent(out) :: found
  COMPLEX(kind=__IOTK_COMPLEX2), optional, intent(in)  :: default (:,:,:,:,:)
  logical,        optional, intent(in)  :: eos
  integer,        optional, intent(out) :: ierr
end subroutine iotk_scan_attr_COMPLEX2_5
#endif
# 112 "iotk_attr_interf.spp"
#if 6 <= __IOTK_MAXRANK
subroutine iotk_scan_attr_COMPLEX2_6(attr,name,val,found,default,eos,ierr)
  use iotk_base
  implicit none
  character(*),             intent(in)  :: attr
  character(*),             intent(in)  :: name
# 119 "iotk_attr_interf.spp"
  COMPLEX(kind=__IOTK_COMPLEX2),           intent(out) :: val (:,:,:,:,:,:)
  logical,        optional, intent(out) :: found
  COMPLEX(kind=__IOTK_COMPLEX2), optional, intent(in)  :: default (:,:,:,:,:,:)
  logical,        optional, intent(in)  :: eos
  integer,        optional, intent(out) :: ierr
end subroutine iotk_scan_attr_COMPLEX2_6
#endif
# 112 "iotk_attr_interf.spp"
#if 7 <= __IOTK_MAXRANK
subroutine iotk_scan_attr_COMPLEX2_7(attr,name,val,found,default,eos,ierr)
  use iotk_base
  implicit none
  character(*),             intent(in)  :: attr
  character(*),             intent(in)  :: name
# 119 "iotk_attr_interf.spp"
  COMPLEX(kind=__IOTK_COMPLEX2),           intent(out) :: val (:,:,:,:,:,:,:)
  logical,        optional, intent(out) :: found
  COMPLEX(kind=__IOTK_COMPLEX2), optional, intent(in)  :: default (:,:,:,:,:,:,:)
  logical,        optional, intent(in)  :: eos
  integer,        optional, intent(out) :: ierr
end subroutine iotk_scan_attr_COMPLEX2_7
#endif
# 128 "iotk_attr_interf.spp"
#endif
# 109 "iotk_attr_interf.spp"
#ifdef __IOTK_COMPLEX3
# 112 "iotk_attr_interf.spp"
#if 0 <= __IOTK_MAXRANK
subroutine iotk_scan_attr_COMPLEX3_0(attr,name,val,found,default,eos,ierr)
  use iotk_base
  implicit none
  character(*),             intent(in)  :: attr
  character(*),             intent(in)  :: name
# 119 "iotk_attr_interf.spp"
  COMPLEX(kind=__IOTK_COMPLEX3),           intent(out) :: val 
  logical,        optional, intent(out) :: found
  COMPLEX(kind=__IOTK_COMPLEX3), optional, intent(in)  :: default 
  logical,        optional, intent(in)  :: eos
  integer,        optional, intent(out) :: ierr
end subroutine iotk_scan_attr_COMPLEX3_0
#endif
# 112 "iotk_attr_interf.spp"
#if 1 <= __IOTK_MAXRANK
subroutine iotk_scan_attr_COMPLEX3_1(attr,name,val,found,default,eos,ierr)
  use iotk_base
  implicit none
  character(*),             intent(in)  :: attr
  character(*),             intent(in)  :: name
# 119 "iotk_attr_interf.spp"
  COMPLEX(kind=__IOTK_COMPLEX3),           intent(out) :: val (:)
  logical,        optional, intent(out) :: found
  COMPLEX(kind=__IOTK_COMPLEX3), optional, intent(in)  :: default (:)
  logical,        optional, intent(in)  :: eos
  integer,        optional, intent(out) :: ierr
end subroutine iotk_scan_attr_COMPLEX3_1
#endif
# 112 "iotk_attr_interf.spp"
#if 2 <= __IOTK_MAXRANK
subroutine iotk_scan_attr_COMPLEX3_2(attr,name,val,found,default,eos,ierr)
  use iotk_base
  implicit none
  character(*),             intent(in)  :: attr
  character(*),             intent(in)  :: name
# 119 "iotk_attr_interf.spp"
  COMPLEX(kind=__IOTK_COMPLEX3),           intent(out) :: val (:,:)
  logical,        optional, intent(out) :: found
  COMPLEX(kind=__IOTK_COMPLEX3), optional, intent(in)  :: default (:,:)
  logical,        optional, intent(in)  :: eos
  integer,        optional, intent(out) :: ierr
end subroutine iotk_scan_attr_COMPLEX3_2
#endif
# 112 "iotk_attr_interf.spp"
#if 3 <= __IOTK_MAXRANK
subroutine iotk_scan_attr_COMPLEX3_3(attr,name,val,found,default,eos,ierr)
  use iotk_base
  implicit none
  character(*),             intent(in)  :: attr
  character(*),             intent(in)  :: name
# 119 "iotk_attr_interf.spp"
  COMPLEX(kind=__IOTK_COMPLEX3),           intent(out) :: val (:,:,:)
  logical,        optional, intent(out) :: found
  COMPLEX(kind=__IOTK_COMPLEX3), optional, intent(in)  :: default (:,:,:)
  logical,        optional, intent(in)  :: eos
  integer,        optional, intent(out) :: ierr
end subroutine iotk_scan_attr_COMPLEX3_3
#endif
# 112 "iotk_attr_interf.spp"
#if 4 <= __IOTK_MAXRANK
subroutine iotk_scan_attr_COMPLEX3_4(attr,name,val,found,default,eos,ierr)
  use iotk_base
  implicit none
  character(*),             intent(in)  :: attr
  character(*),             intent(in)  :: name
# 119 "iotk_attr_interf.spp"
  COMPLEX(kind=__IOTK_COMPLEX3),           intent(out) :: val (:,:,:,:)
  logical,        optional, intent(out) :: found
  COMPLEX(kind=__IOTK_COMPLEX3), optional, intent(in)  :: default (:,:,:,:)
  logical,        optional, intent(in)  :: eos
  integer,        optional, intent(out) :: ierr
end subroutine iotk_scan_attr_COMPLEX3_4
#endif
# 112 "iotk_attr_interf.spp"
#if 5 <= __IOTK_MAXRANK
subroutine iotk_scan_attr_COMPLEX3_5(attr,name,val,found,default,eos,ierr)
  use iotk_base
  implicit none
  character(*),             intent(in)  :: attr
  character(*),             intent(in)  :: name
# 119 "iotk_attr_interf.spp"
  COMPLEX(kind=__IOTK_COMPLEX3),           intent(out) :: val (:,:,:,:,:)
  logical,        optional, intent(out) :: found
  COMPLEX(kind=__IOTK_COMPLEX3), optional, intent(in)  :: default (:,:,:,:,:)
  logical,        optional, intent(in)  :: eos
  integer,        optional, intent(out) :: ierr
end subroutine iotk_scan_attr_COMPLEX3_5
#endif
# 112 "iotk_attr_interf.spp"
#if 6 <= __IOTK_MAXRANK
subroutine iotk_scan_attr_COMPLEX3_6(attr,name,val,found,default,eos,ierr)
  use iotk_base
  implicit none
  character(*),             intent(in)  :: attr
  character(*),             intent(in)  :: name
# 119 "iotk_attr_interf.spp"
  COMPLEX(kind=__IOTK_COMPLEX3),           intent(out) :: val (:,:,:,:,:,:)
  logical,        optional, intent(out) :: found
  COMPLEX(kind=__IOTK_COMPLEX3), optional, intent(in)  :: default (:,:,:,:,:,:)
  logical,        optional, intent(in)  :: eos
  integer,        optional, intent(out) :: ierr
end subroutine iotk_scan_attr_COMPLEX3_6
#endif
# 112 "iotk_attr_interf.spp"
#if 7 <= __IOTK_MAXRANK
subroutine iotk_scan_attr_COMPLEX3_7(attr,name,val,found,default,eos,ierr)
  use iotk_base
  implicit none
  character(*),             intent(in)  :: attr
  character(*),             intent(in)  :: name
# 119 "iotk_attr_interf.spp"
  COMPLEX(kind=__IOTK_COMPLEX3),           intent(out) :: val (:,:,:,:,:,:,:)
  logical,        optional, intent(out) :: found
  COMPLEX(kind=__IOTK_COMPLEX3), optional, intent(in)  :: default (:,:,:,:,:,:,:)
  logical,        optional, intent(in)  :: eos
  integer,        optional, intent(out) :: ierr
end subroutine iotk_scan_attr_COMPLEX3_7
#endif
# 128 "iotk_attr_interf.spp"
#endif
# 109 "iotk_attr_interf.spp"
#ifdef __IOTK_COMPLEX4
# 112 "iotk_attr_interf.spp"
#if 0 <= __IOTK_MAXRANK
subroutine iotk_scan_attr_COMPLEX4_0(attr,name,val,found,default,eos,ierr)
  use iotk_base
  implicit none
  character(*),             intent(in)  :: attr
  character(*),             intent(in)  :: name
# 119 "iotk_attr_interf.spp"
  COMPLEX(kind=__IOTK_COMPLEX4),           intent(out) :: val 
  logical,        optional, intent(out) :: found
  COMPLEX(kind=__IOTK_COMPLEX4), optional, intent(in)  :: default 
  logical,        optional, intent(in)  :: eos
  integer,        optional, intent(out) :: ierr
end subroutine iotk_scan_attr_COMPLEX4_0
#endif
# 112 "iotk_attr_interf.spp"
#if 1 <= __IOTK_MAXRANK
subroutine iotk_scan_attr_COMPLEX4_1(attr,name,val,found,default,eos,ierr)
  use iotk_base
  implicit none
  character(*),             intent(in)  :: attr
  character(*),             intent(in)  :: name
# 119 "iotk_attr_interf.spp"
  COMPLEX(kind=__IOTK_COMPLEX4),           intent(out) :: val (:)
  logical,        optional, intent(out) :: found
  COMPLEX(kind=__IOTK_COMPLEX4), optional, intent(in)  :: default (:)
  logical,        optional, intent(in)  :: eos
  integer,        optional, intent(out) :: ierr
end subroutine iotk_scan_attr_COMPLEX4_1
#endif
# 112 "iotk_attr_interf.spp"
#if 2 <= __IOTK_MAXRANK
subroutine iotk_scan_attr_COMPLEX4_2(attr,name,val,found,default,eos,ierr)
  use iotk_base
  implicit none
  character(*),             intent(in)  :: attr
  character(*),             intent(in)  :: name
# 119 "iotk_attr_interf.spp"
  COMPLEX(kind=__IOTK_COMPLEX4),           intent(out) :: val (:,:)
  logical,        optional, intent(out) :: found
  COMPLEX(kind=__IOTK_COMPLEX4), optional, intent(in)  :: default (:,:)
  logical,        optional, intent(in)  :: eos
  integer,        optional, intent(out) :: ierr
end subroutine iotk_scan_attr_COMPLEX4_2
#endif
# 112 "iotk_attr_interf.spp"
#if 3 <= __IOTK_MAXRANK
subroutine iotk_scan_attr_COMPLEX4_3(attr,name,val,found,default,eos,ierr)
  use iotk_base
  implicit none
  character(*),             intent(in)  :: attr
  character(*),             intent(in)  :: name
# 119 "iotk_attr_interf.spp"
  COMPLEX(kind=__IOTK_COMPLEX4),           intent(out) :: val (:,:,:)
  logical,        optional, intent(out) :: found
  COMPLEX(kind=__IOTK_COMPLEX4), optional, intent(in)  :: default (:,:,:)
  logical,        optional, intent(in)  :: eos
  integer,        optional, intent(out) :: ierr
end subroutine iotk_scan_attr_COMPLEX4_3
#endif
# 112 "iotk_attr_interf.spp"
#if 4 <= __IOTK_MAXRANK
subroutine iotk_scan_attr_COMPLEX4_4(attr,name,val,found,default,eos,ierr)
  use iotk_base
  implicit none
  character(*),             intent(in)  :: attr
  character(*),             intent(in)  :: name
# 119 "iotk_attr_interf.spp"
  COMPLEX(kind=__IOTK_COMPLEX4),           intent(out) :: val (:,:,:,:)
  logical,        optional, intent(out) :: found
  COMPLEX(kind=__IOTK_COMPLEX4), optional, intent(in)  :: default (:,:,:,:)
  logical,        optional, intent(in)  :: eos
  integer,        optional, intent(out) :: ierr
end subroutine iotk_scan_attr_COMPLEX4_4
#endif
# 112 "iotk_attr_interf.spp"
#if 5 <= __IOTK_MAXRANK
subroutine iotk_scan_attr_COMPLEX4_5(attr,name,val,found,default,eos,ierr)
  use iotk_base
  implicit none
  character(*),             intent(in)  :: attr
  character(*),             intent(in)  :: name
# 119 "iotk_attr_interf.spp"
  COMPLEX(kind=__IOTK_COMPLEX4),           intent(out) :: val (:,:,:,:,:)
  logical,        optional, intent(out) :: found
  COMPLEX(kind=__IOTK_COMPLEX4), optional, intent(in)  :: default (:,:,:,:,:)
  logical,        optional, intent(in)  :: eos
  integer,        optional, intent(out) :: ierr
end subroutine iotk_scan_attr_COMPLEX4_5
#endif
# 112 "iotk_attr_interf.spp"
#if 6 <= __IOTK_MAXRANK
subroutine iotk_scan_attr_COMPLEX4_6(attr,name,val,found,default,eos,ierr)
  use iotk_base
  implicit none
  character(*),             intent(in)  :: attr
  character(*),             intent(in)  :: name
# 119 "iotk_attr_interf.spp"
  COMPLEX(kind=__IOTK_COMPLEX4),           intent(out) :: val (:,:,:,:,:,:)
  logical,        optional, intent(out) :: found
  COMPLEX(kind=__IOTK_COMPLEX4), optional, intent(in)  :: default (:,:,:,:,:,:)
  logical,        optional, intent(in)  :: eos
  integer,        optional, intent(out) :: ierr
end subroutine iotk_scan_attr_COMPLEX4_6
#endif
# 112 "iotk_attr_interf.spp"
#if 7 <= __IOTK_MAXRANK
subroutine iotk_scan_attr_COMPLEX4_7(attr,name,val,found,default,eos,ierr)
  use iotk_base
  implicit none
  character(*),             intent(in)  :: attr
  character(*),             intent(in)  :: name
# 119 "iotk_attr_interf.spp"
  COMPLEX(kind=__IOTK_COMPLEX4),           intent(out) :: val (:,:,:,:,:,:,:)
  logical,        optional, intent(out) :: found
  COMPLEX(kind=__IOTK_COMPLEX4), optional, intent(in)  :: default (:,:,:,:,:,:,:)
  logical,        optional, intent(in)  :: eos
  integer,        optional, intent(out) :: ierr
end subroutine iotk_scan_attr_COMPLEX4_7
#endif
# 128 "iotk_attr_interf.spp"
#endif
# 109 "iotk_attr_interf.spp"
#ifdef __IOTK_CHARACTER1
# 112 "iotk_attr_interf.spp"
#if 0 <= __IOTK_MAXRANK
subroutine iotk_scan_attr_CHARACTER1_0(attr,name,val,found,default,eos,ierr)
  use iotk_base
  implicit none
  character(*),             intent(in)  :: attr
  character(*),             intent(in)  :: name
# 119 "iotk_attr_interf.spp"
  CHARACTER(kind=__IOTK_CHARACTER1,len=*),           intent(out) :: val 
  logical,        optional, intent(out) :: found
  CHARACTER(kind=__IOTK_CHARACTER1,len=*), optional, intent(in)  :: default 
  logical,        optional, intent(in)  :: eos
  integer,        optional, intent(out) :: ierr
end subroutine iotk_scan_attr_CHARACTER1_0
#endif
# 128 "iotk_attr_interf.spp"
#endif
# 132 "iotk_attr_interf.spp"
end interface

end module iotk_attr_interf
