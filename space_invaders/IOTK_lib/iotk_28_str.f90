# 1 "iotk_str.spp"
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


# 30 "iotk_str.spp"

# 33 "iotk_str.spp"

function iotk_toupper_x(str)
  use iotk_base
  use iotk_misc_interf
  implicit none
  character(len=*), intent(in) :: str
  character(len=len(str))      :: iotk_toupper_x
  integer :: i,pos
  do i = 1,len(str)
    if(str(i:i)==iotk_eos) exit
    pos=scan(lowalphabet,str(i:i))
    if(pos==0) then
      iotk_toupper_x(i:i) = str(i:i)
    else
      iotk_toupper_x(i:i) = upalphabet(pos:pos)
    end if
  end do
  if(i<=len(iotk_toupper_x)) iotk_toupper_x(i:i) = iotk_eos
end function iotk_toupper_x

function iotk_tolower_x(str)
  use iotk_base
  use iotk_misc_interf
  implicit none
  character(len=*), intent(in) :: str
  character(len=len(str))      :: iotk_tolower_x
  integer :: i,pos
  do i = 1,len(str)
    if(str(i:i)==iotk_eos) exit
    pos=scan(upalphabet,str(i:i))
    if(pos==0) then
      iotk_tolower_x(i:i) = str(i:i)
    else
      iotk_tolower_x(i:i) = lowalphabet(pos:pos)
    end if
  end do
  if(i<=len(iotk_tolower_x)) iotk_tolower_x(i:i) = iotk_eos
end function iotk_tolower_x

subroutine iotk_escape_x(to,from)
  use iotk_base
  use iotk_misc_interf
  use iotk_str_interf
  implicit none
  character(len=*), intent(in) :: from
  character(len=*), intent(out):: to
  integer :: pos,pos1,semic,fromlen
  pos = 1
  pos1 = 1
  fromlen = iotk_strlen(from)
  do  
    if(pos>fromlen) exit
    if(from(pos:pos)=="&" .and. pos/=fromlen) then
      semic = scan(from(pos+1:fromlen),";")
      if(semic<=1) to(pos1:pos1)="&"
      select case(from(pos+1:pos+semic-1))
      case("amp")
        to(pos1:pos1)="&"
      case("lt")
        to(pos1:pos1)="<"
      case("gt")
        to(pos1:pos1)=">"
      case("quot")
        to(pos1:pos1)='"'
      case("apos")
        to(pos1:pos1)="'"
      case default
        to(pos1:pos1+semic) = from(pos:pos+semic)
        pos1 = pos1 + semic
      end select
      pos = pos + semic
    else
      to(pos1:pos1)=from(pos:pos)
    end if
    pos = pos + 1
    pos1 = pos1 + 1
    if(pos1>len(to)) exit
  end do  
  if(pos1<=len(to)) to(pos1:pos1)=iotk_eos
end subroutine iotk_escape_x

subroutine iotk_deescape_x(to,from,quot,apos)
  use iotk_base
  use iotk_misc_interf
  use iotk_str_interf
  implicit none
  character(len=*), intent(in)  :: from
  character(len=*), intent(out) :: to
  logical, optional, intent(in) :: quot,apos
  logical :: lquot,lapos
  integer :: pos,pos1
  lquot=.false.
  lapos=.false.
  if(present(quot)) lquot = quot
  if(present(apos)) lapos = apos
  pos = 1
  pos1 = 1
  do
    if(pos>len(from) .or. pos1>len(to)) exit ! i due test devono essere separati
    if(from(pos:pos)==iotk_eos) exit
    select case(from(pos:pos))
    case("&")
      if(pos1+4<=len(to)) to(pos1:pos1+4)="&amp;"
      pos1=pos1+4
    case("<")
      if(pos1+3<=len(to)) to(pos1:pos1+3)="&lt;"
      pos1=pos1+3
    case(">")
      if(pos1+3<=len(to)) to(pos1:pos1+3)="&gt;"
      pos1=pos1+3
    case('"')
      if(lquot) then
        if(pos1+5<=len(to)) to(pos1:pos1+5)="&quot;"
        pos1=pos1+5
      else
        to(pos1:pos1) = from(pos:pos)
      end if
    case("'")
      if(lapos) then
        if(pos1+5<=len(to)) to(pos1:pos1+5)="&apos;"
        pos1=pos1+5
      else
        to(pos1:pos1) = from(pos:pos)
      end if
    case default
      to(pos1:pos1) = from(pos:pos)
    end select
    pos = pos + 1
    pos1 = pos1 + 1
  end do
  if(pos1<=len(to)) to(pos1:pos1)=iotk_eos
end subroutine iotk_deescape_x

function iotk_strtrim_x(str)
  use iotk_base
  use iotk_str_interf
  use iotk_misc_interf
  implicit none
  character(len=*), intent(in) :: str
  character(len=len(str))      :: iotk_strtrim_x
  integer :: lentrim
  lentrim = len_trim(str(1:iotk_strlen(str)))
  iotk_strtrim_x(1:lentrim) = str(1:lentrim)
  if(lentrim<len(iotk_strtrim_x)) iotk_strtrim_x(lentrim+1:lentrim+1) = iotk_eos
end function iotk_strtrim_x

function iotk_strlen_trim_x(str)
  use iotk_base
  use iotk_str_interf
  use iotk_misc_interf
  implicit none
  character(len=*), intent(in) :: str
  integer                      :: iotk_strlen_trim_x
  iotk_strlen_trim_x = len_trim(str(1:iotk_strlen(str)))
end function iotk_strlen_trim_x

function iotk_strscan_x(string,set,back)
  use iotk_misc_interf
  use iotk_str_interf
  implicit none
  character(len=*),  intent(in) :: string
  character(len=*),  intent(in) :: set
  logical, optional, intent(in) :: back
  integer                       :: iotk_strscan_x
  logical :: backl
  backl = .false.
  if(present(back)) backl=back
  iotk_strscan_x = scan(string(1:iotk_strlen(string)),set(1:iotk_strlen(set)),backl)
end function iotk_strscan_x

function iotk_strlen_x(str)
  use iotk_base
  implicit none
  character(len=*), intent(in) :: str
  integer :: iotk_strlen_x
  integer :: pos
  pos = scan(str,iotk_eos) - 1
  if(pos>=0) then
    iotk_strlen_x = pos
  else
    iotk_strlen_x = len(str)
  end if
end function iotk_strlen_x

function iotk_strpad_x(str)
  use iotk_base
  use iotk_misc_interf
  use iotk_str_interf
  implicit none
  character(len=*), intent(in) :: str
  character(len=len(str))      :: iotk_strpad_x
  integer :: strlen
  strlen = iotk_strlen(str)
  iotk_strpad_x(1:strlen) = str(1:strlen)
  if(strlen<len(iotk_strpad_x)) iotk_strpad_x(strlen+1:) = " "
end function iotk_strpad_x

# 231 "iotk_str.spp"
subroutine iotk_strcpy_x(to,from,ierr)
  use iotk_base
  use iotk_error_interf
  use iotk_misc_interf
  implicit none
  character(len=*), intent(out):: to
  character(len=*), intent(in) :: from
  integer,          intent(out):: ierr
  integer :: i,fromlen
  ierr = 0
  do i=1,min(len(from),len(to))
    if(from(i:i)==iotk_eos) exit
    to(i:i)=from(i:i)
  end do
  if(i>len(to) .and. i<=len(from)) then
    if(from(i:i)/=iotk_eos) then
      call iotk_error_issue(ierr,"iotk_strcpy",__FILE__,__LINE__)
# 247 "iotk_str.spp"
call iotk_error_msg(ierr,"CVS Revision: 1.7 ")
      return
    end if
  end if
  if(i<=len(to)) to(i:i) = iotk_eos
end subroutine iotk_strcpy_x

# 255 "iotk_str.spp"
subroutine iotk_strcat_x(to,from,ierr)
  use iotk_base
  use iotk_error_interf
  use iotk_str_interf
  use iotk_misc_interf
  implicit none
  character(len=*), intent(inout):: to
  character(len=*), intent(in) :: from
  integer,          intent(out):: ierr
  integer :: tolen,fromlen
  ierr = 0
  tolen = iotk_strlen(to)
  fromlen = iotk_strlen(from)
  if(tolen+fromlen>len(to)) then
    call iotk_error_issue(ierr,"iotk_strcat",__FILE__,__LINE__)
# 269 "iotk_str.spp"
call iotk_error_msg(ierr,"CVS Revision: 1.7 ")
  end if
  if(ierr/=0) return
  to(tolen+1:tolen+fromlen) = from(1:fromlen)
  if(tolen+fromlen+1<=len(to)) to(tolen+fromlen+1:tolen+fromlen+1)=iotk_eos
end subroutine iotk_strcat_x

# 277 "iotk_str.spp"
function iotk_strcomp_x(str1,str2)
  use iotk_base
  implicit none
  logical :: iotk_strcomp_x
  character(len=*), intent(in) :: str1,str2
  integer :: i
  iotk_strcomp_x = .false.
  do i=1,min(len(str1),len(str2))
    if(str1(i:i)/=str2(i:i)) return
    if(str1(i:i)==iotk_eos) exit
  end do
  if(i>len(str1)) then
    if(i<=len(str2)) then
      if(str2(i:i)/=iotk_eos) return
    end if
  else if(i>len(str2)) then
    if(i<=len(str1)) then
      if(str1(i:i)/=iotk_eos) return
    end if
  end if
  iotk_strcomp_x = .true.
end function iotk_strcomp_x
