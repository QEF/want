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

# 30 "iotk_fmt.spp"

# 33 "iotk_fmt.spp"

function iotk_basefmt_x(type,ikind,ilen)
  use iotk_base
  use iotk_xtox_interf
  use iotk_misc_interf
  implicit none
  character(100)           :: iotk_basefmt_x
  integer,      intent(in) :: ikind,ilen
  character(*), intent(in) :: type
  integer :: nexp,exp,ndig,baselen
  logical, save :: first_call = .true.
# 45 "iotk_fmt.spp"
#ifdef __IOTK_INTEGER1
  integer (__IOTK_INTEGER1) :: example_INTEGER1
  character(46), save :: save_basefmt_integer1 = ""
#endif
#ifdef __IOTK_REAL1
  real (__IOTK_REAL1) :: example_REAL1
  character(46), save :: save_basefmt_real1 = ""
#endif
# 45 "iotk_fmt.spp"
#ifdef __IOTK_INTEGER2
  integer (__IOTK_INTEGER2) :: example_INTEGER2
  character(46), save :: save_basefmt_integer2 = ""
#endif
#ifdef __IOTK_REAL2
  real (__IOTK_REAL2) :: example_REAL2
  character(46), save :: save_basefmt_real2 = ""
#endif
# 45 "iotk_fmt.spp"
#ifdef __IOTK_INTEGER3
  integer (__IOTK_INTEGER3) :: example_INTEGER3
  character(46), save :: save_basefmt_integer3 = ""
#endif
#ifdef __IOTK_REAL3
  real (__IOTK_REAL3) :: example_REAL3
  character(46), save :: save_basefmt_real3 = ""
#endif
# 45 "iotk_fmt.spp"
#ifdef __IOTK_INTEGER4
  integer (__IOTK_INTEGER4) :: example_INTEGER4
  character(46), save :: save_basefmt_integer4 = ""
#endif
#ifdef __IOTK_REAL4
  real (__IOTK_REAL4) :: example_REAL4
  character(46), save :: save_basefmt_real4 = ""
#endif
# 54 "iotk_fmt.spp"
  if(first_call) then
# 56 "iotk_fmt.spp"
#ifdef __IOTK_INTEGER1
    baselen = range(example_INTEGER1) + 1
    save_basefmt_integer1 = "(i"//trim(iotk_itoa(baselen))//")"
#endif
#ifdef __IOTK_REAL1
    ndig = precision(example_REAL1)+1
    exp = range(example_REAL1)+1
    nexp = 1
    do
      if(exp < 10) exit
      exp = exp / 10
      nexp = nexp + 1
    end do
    baselen = nexp+ndig-1+5
    save_basefmt_real1 = "(ES"//trim(iotk_itoa(baselen))//"." &
                //trim(iotk_itoa(ndig-1))//"E"//trim(iotk_itoa(nexp))//")"

#endif
# 56 "iotk_fmt.spp"
#ifdef __IOTK_INTEGER2
    baselen = range(example_INTEGER2) + 1
    save_basefmt_integer2 = "(i"//trim(iotk_itoa(baselen))//")"
#endif
#ifdef __IOTK_REAL2
    ndig = precision(example_REAL2)+1
    exp = range(example_REAL2)+1
    nexp = 1
    do
      if(exp < 10) exit
      exp = exp / 10
      nexp = nexp + 1
    end do
    baselen = nexp+ndig-1+5
    save_basefmt_real2 = "(ES"//trim(iotk_itoa(baselen))//"." &
                //trim(iotk_itoa(ndig-1))//"E"//trim(iotk_itoa(nexp))//")"

#endif
# 56 "iotk_fmt.spp"
#ifdef __IOTK_INTEGER3
    baselen = range(example_INTEGER3) + 1
    save_basefmt_integer3 = "(i"//trim(iotk_itoa(baselen))//")"
#endif
#ifdef __IOTK_REAL3
    ndig = precision(example_REAL3)+1
    exp = range(example_REAL3)+1
    nexp = 1
    do
      if(exp < 10) exit
      exp = exp / 10
      nexp = nexp + 1
    end do
    baselen = nexp+ndig-1+5
    save_basefmt_real3 = "(ES"//trim(iotk_itoa(baselen))//"." &
                //trim(iotk_itoa(ndig-1))//"E"//trim(iotk_itoa(nexp))//")"

#endif
# 56 "iotk_fmt.spp"
#ifdef __IOTK_INTEGER4
    baselen = range(example_INTEGER4) + 1
    save_basefmt_integer4 = "(i"//trim(iotk_itoa(baselen))//")"
#endif
#ifdef __IOTK_REAL4
    ndig = precision(example_REAL4)+1
    exp = range(example_REAL4)+1
    nexp = 1
    do
      if(exp < 10) exit
      exp = exp / 10
      nexp = nexp + 1
    end do
    baselen = nexp+ndig-1+5
    save_basefmt_real4 = "(ES"//trim(iotk_itoa(baselen))//"." &
                //trim(iotk_itoa(ndig-1))//"E"//trim(iotk_itoa(nexp))//")"

#endif
# 75 "iotk_fmt.spp"
    first_call = .false.
  end if
  select case(type)
  case("LOGICAL")
    iotk_basefmt_x = "(l1)"
  case("INTEGER")
    select case(ikind)
# 83 "iotk_fmt.spp"
#ifdef __IOTK_INTEGER1
    case(__IOTK_INTEGER1)
      iotk_basefmt_x = save_basefmt_integer1
#endif
# 83 "iotk_fmt.spp"
#ifdef __IOTK_INTEGER2
    case(__IOTK_INTEGER2)
      iotk_basefmt_x = save_basefmt_integer2
#endif
# 83 "iotk_fmt.spp"
#ifdef __IOTK_INTEGER3
    case(__IOTK_INTEGER3)
      iotk_basefmt_x = save_basefmt_integer3
#endif
# 83 "iotk_fmt.spp"
#ifdef __IOTK_INTEGER4
    case(__IOTK_INTEGER4)
      iotk_basefmt_x = save_basefmt_integer4
#endif
# 88 "iotk_fmt.spp"
    end select
  case("REAL")
    select case(ikind)
# 92 "iotk_fmt.spp"
#ifdef __IOTK_REAL1
    case(__IOTK_REAL1)
      iotk_basefmt_x = save_basefmt_real1
#endif
# 92 "iotk_fmt.spp"
#ifdef __IOTK_REAL2
    case(__IOTK_REAL2)
      iotk_basefmt_x = save_basefmt_real2
#endif
# 92 "iotk_fmt.spp"
#ifdef __IOTK_REAL3
    case(__IOTK_REAL3)
      iotk_basefmt_x = save_basefmt_real3
#endif
# 92 "iotk_fmt.spp"
#ifdef __IOTK_REAL4
    case(__IOTK_REAL4)
      iotk_basefmt_x = save_basefmt_real4
#endif
# 97 "iotk_fmt.spp"
    end select
  case("COMPLEX")
    select case(ikind)
# 101 "iotk_fmt.spp"
#ifdef __IOTK_REAL1
    case(__IOTK_REAL1)
      iotk_basefmt_x = "("//trim(save_basefmt_real1)//",',',"//trim(save_basefmt_real1)//")"
#endif
# 101 "iotk_fmt.spp"
#ifdef __IOTK_REAL2
    case(__IOTK_REAL2)
      iotk_basefmt_x = "("//trim(save_basefmt_real2)//",',',"//trim(save_basefmt_real2)//")"
#endif
# 101 "iotk_fmt.spp"
#ifdef __IOTK_REAL3
    case(__IOTK_REAL3)
      iotk_basefmt_x = "("//trim(save_basefmt_real3)//",',',"//trim(save_basefmt_real3)//")"
#endif
# 101 "iotk_fmt.spp"
#ifdef __IOTK_REAL4
    case(__IOTK_REAL4)
      iotk_basefmt_x = "("//trim(save_basefmt_real4)//",',',"//trim(save_basefmt_real4)//")"
#endif
# 106 "iotk_fmt.spp"
    end select
  case("CHARACTER")
    if(ilen>=0) then
      iotk_basefmt_x = "(a"//trim(iotk_itoa(ilen))//")"
    else
      iotk_basefmt_x = "(a)"
    end if
  end select
end function iotk_basefmt_x

function iotk_wfmt_x(type,ikind,isize,ilen)
  use iotk_base
  use iotk_xtox_interf
  use iotk_fmt_interf
  use iotk_misc_interf
  implicit none
  integer,       intent(in)  :: ikind
  character(*),  intent(in)  :: type
  integer,       intent(in)  :: isize
  integer,       intent(in)  :: ilen
  character(150)             :: iotk_wfmt_x
  if(isize==1) then
    iotk_wfmt_x = "("//trim(iotk_basefmt(type,ikind,ilen))//")"
  else
    iotk_wfmt_x = "("//trim(iotk_itoa(isize))//"("//trim(iotk_basefmt(type,ikind,ilen)) &
                //",:,','))"
  end if
!write(0,*) "FMT:"//trim(iotk_wfmt_x)
end function iotk_wfmt_x
