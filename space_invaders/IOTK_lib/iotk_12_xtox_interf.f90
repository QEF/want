# 1 "iotk_xtox_interf.spp"
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


# 30 "iotk_xtox_interf.spp"

module iotk_xtox_interf
use iotk_base
implicit none
private

public :: iotk_atol
public :: iotk_ltoa
public :: iotk_atoi
public :: iotk_itoa

interface iotk_atol
function iotk_atol_x(a,check)
  character(len=*),  intent(in)  :: a
  logical, optional, intent(out) :: check
  logical                        :: iotk_atol_x
end function iotk_atol_x
end interface

interface iotk_atoi
# 51 "iotk_xtox_interf.spp"
#ifdef __IOTK_INTEGER1
subroutine iotk_atoi1(i,a,check)
  use iotk_base
  implicit none
  integer(kind=__IOTK_INTEGER1), intent(out) :: i
  character(len=*),                    intent(in)  :: a
  logical, optional,                   intent(out) :: check
end subroutine iotk_atoi1
#endif
# 51 "iotk_xtox_interf.spp"
#ifdef __IOTK_INTEGER2
subroutine iotk_atoi2(i,a,check)
  use iotk_base
  implicit none
  integer(kind=__IOTK_INTEGER2), intent(out) :: i
  character(len=*),                    intent(in)  :: a
  logical, optional,                   intent(out) :: check
end subroutine iotk_atoi2
#endif
# 51 "iotk_xtox_interf.spp"
#ifdef __IOTK_INTEGER3
subroutine iotk_atoi3(i,a,check)
  use iotk_base
  implicit none
  integer(kind=__IOTK_INTEGER3), intent(out) :: i
  character(len=*),                    intent(in)  :: a
  logical, optional,                   intent(out) :: check
end subroutine iotk_atoi3
#endif
# 51 "iotk_xtox_interf.spp"
#ifdef __IOTK_INTEGER4
subroutine iotk_atoi4(i,a,check)
  use iotk_base
  implicit none
  integer(kind=__IOTK_INTEGER4), intent(out) :: i
  character(len=*),                    intent(in)  :: a
  logical, optional,                   intent(out) :: check
end subroutine iotk_atoi4
#endif
# 61 "iotk_xtox_interf.spp"
end interface

interface iotk_itoa
# 65 "iotk_xtox_interf.spp"
#ifdef __IOTK_INTEGER1
function iotk_itoa1(i,length)
  use iotk_base
  implicit none
  integer(kind=__IOTK_INTEGER1), intent(in)  :: i
  integer, optional, intent(out)                 :: length
  character(len=range(i)+2)                      :: iotk_itoa1
end function iotk_itoa1
#endif
# 65 "iotk_xtox_interf.spp"
#ifdef __IOTK_INTEGER2
function iotk_itoa2(i,length)
  use iotk_base
  implicit none
  integer(kind=__IOTK_INTEGER2), intent(in)  :: i
  integer, optional, intent(out)                 :: length
  character(len=range(i)+2)                      :: iotk_itoa2
end function iotk_itoa2
#endif
# 65 "iotk_xtox_interf.spp"
#ifdef __IOTK_INTEGER3
function iotk_itoa3(i,length)
  use iotk_base
  implicit none
  integer(kind=__IOTK_INTEGER3), intent(in)  :: i
  integer, optional, intent(out)                 :: length
  character(len=range(i)+2)                      :: iotk_itoa3
end function iotk_itoa3
#endif
# 65 "iotk_xtox_interf.spp"
#ifdef __IOTK_INTEGER4
function iotk_itoa4(i,length)
  use iotk_base
  implicit none
  integer(kind=__IOTK_INTEGER4), intent(in)  :: i
  integer, optional, intent(out)                 :: length
  character(len=range(i)+2)                      :: iotk_itoa4
end function iotk_itoa4
#endif
# 75 "iotk_xtox_interf.spp"
end interface

interface iotk_ltoa
# 79 "iotk_xtox_interf.spp"
#ifdef __IOTK_LOGICAL1
function iotk_ltoa1(l)
  use iotk_base
  implicit none
  logical(kind=__IOTK_LOGICAL1), intent(in) :: l
  character                                     :: iotk_ltoa1
end function iotk_ltoa1
#endif
# 79 "iotk_xtox_interf.spp"
#ifdef __IOTK_LOGICAL2
function iotk_ltoa2(l)
  use iotk_base
  implicit none
  logical(kind=__IOTK_LOGICAL2), intent(in) :: l
  character                                     :: iotk_ltoa2
end function iotk_ltoa2
#endif
# 79 "iotk_xtox_interf.spp"
#ifdef __IOTK_LOGICAL3
function iotk_ltoa3(l)
  use iotk_base
  implicit none
  logical(kind=__IOTK_LOGICAL3), intent(in) :: l
  character                                     :: iotk_ltoa3
end function iotk_ltoa3
#endif
# 79 "iotk_xtox_interf.spp"
#ifdef __IOTK_LOGICAL4
function iotk_ltoa4(l)
  use iotk_base
  implicit none
  logical(kind=__IOTK_LOGICAL4), intent(in) :: l
  character                                     :: iotk_ltoa4
end function iotk_ltoa4
#endif
# 88 "iotk_xtox_interf.spp"
end interface

end module iotk_xtox_interf
