# 1 "iotk_str_interf.spp"
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


# 30 "iotk_str_interf.spp"

module iotk_str_interf
use iotk_base
implicit none
private

public :: iotk_escape
public :: iotk_deescape
public :: iotk_strcpy
public :: iotk_strcat
public :: iotk_strlen
public :: iotk_strpad
public :: iotk_strscan
public :: iotk_strcomp
public :: iotk_strtrim
public :: iotk_strlen_trim
public :: iotk_toupper
public :: iotk_tolower

interface iotk_toupper
function iotk_toupper_x(str)
  implicit none
  character(len=*), intent(in) :: str
  character(len=len(str))      :: iotk_toupper_x
end function iotk_toupper_x
end interface

interface iotk_tolower
function iotk_tolower_x(str)
  implicit none
  character(len=*), intent(in) :: str
  character(len=len(str))      :: iotk_tolower_x
end function
end interface

interface iotk_escape
subroutine iotk_escape_x(to,from)
  implicit none
  character(len=*), intent(in) :: from
  character(len=*), intent(out):: to
end subroutine
end interface

interface iotk_deescape
subroutine iotk_deescape_x(to,from,quot,apos)
  implicit none
  character(len=*), intent(in)  :: from
  character(len=*), intent(out) :: to
  logical, optional, intent(in) :: quot,apos
end subroutine iotk_deescape_x
end interface

interface iotk_strscan
function iotk_strscan_x(string,set,back)
  implicit none
  character(len=*),  intent(in) :: string
  character(len=*),  intent(in) :: set
  logical, optional, intent(in) :: back
  integer                       :: iotk_strscan_x
end function iotk_strscan_x
end interface


interface iotk_strtrim
function iotk_strtrim_x(str)
  implicit none
  character(len=*), intent(in) :: str
  character(len=len(str))      :: iotk_strtrim_x
end function iotk_strtrim_x 
end interface

interface iotk_strlen_trim
function iotk_strlen_trim_x(str)
  implicit none
  character(len=*), intent(in) :: str
  integer                      :: iotk_strlen_trim_x
end function iotk_strlen_trim_x
end interface

interface iotk_strlen
function iotk_strlen_x(str)
  implicit none
  character(len=*), intent(in) :: str
  integer :: iotk_strlen_x
end function iotk_strlen_x
end interface

interface iotk_strpad
function iotk_strpad_x(str)
  implicit none
  character(len=*), intent(in) :: str
  character(len=len(str))      :: iotk_strpad_x
end function iotk_strpad_x
end interface

interface iotk_strcpy
subroutine iotk_strcpy_x(to,from,ierr)
  implicit none
  character(len=*), intent(out):: to
  character(len=*), intent(in) :: from
  integer,          intent(out):: ierr
end subroutine iotk_strcpy_x
end interface

interface iotk_strcat
subroutine iotk_strcat_x(to,from,ierr)
  implicit none
  character(len=*), intent(inout):: to
  character(len=*), intent(in) :: from
  integer,          intent(out):: ierr
end subroutine iotk_strcat_x
end interface

interface iotk_strcomp
function iotk_strcomp_x(str1,str2)
  implicit none
  logical :: iotk_strcomp_x
  character(len=*), intent(in) :: str1,str2
end function iotk_strcomp_x
end interface

end module iotk_str_interf
