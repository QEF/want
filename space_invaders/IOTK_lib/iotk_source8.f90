! Input/Output Tool Kit (IOTK)
! Copyright (C) 2004 Giovanni Bussi
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

# 30 "iotk_xtox.spp"

# 33 "iotk_xtox.spp"

# 35 "iotk_xtox.spp"
function iotk_atol_x(a,check)
  use iotk_base
  use iotk_interface
  implicit none
  character(len=*),  intent(in)  :: a
  logical, optional, intent(out) :: check
  logical :: iotk_atol_x
  integer :: i
  iotk_atol_x = .false.
  if(present(check)) check = .false.
  if(len(a)==0) return
  do i = 1 , len(a)
    if(a(i:i)/=" " .and. a(i:i)/=".") exit
  end do
  if(i>len(a)) return
  if(present(check)) check = .true.
  if(a(i:i)=="T" .or. a(i:i)=="t") then
    iotk_atol_x = .true.
  else if(a(i:i)=="F" .or. a(i:i)=="f") then
    iotk_atol_x = .false.
  else
    if(present(check)) check = .false.
  end if
end function iotk_atol_x

# 61 "iotk_xtox.spp"
#ifdef __IOTK_INTEGER1
subroutine iotk_atoi1(i,a,check)
  use iotk_base
  use iotk_interface
  implicit none
  character(len=*),                  intent(in)  :: a
  logical, optional,                 intent(out) :: check
  integer(kind=__IOTK_INTEGER1), intent(out) :: i
  logical :: minus
  integer :: pos,index,ii
! AGGIUNGERE DEI CHECK SULL'OVERFLOW
  minus = .false.
  i = 0
  if(present(check)) check = .false.
  if(len(a)==0) return
  do ii = 1 , len(a)
    if(a(ii:ii)/=" ") exit
  end do
  if(ii>len(a)) return
  if(a(ii:ii)=="-") then
    minus = .true.
    ii = ii + 1
  else if(a(ii:ii)=="+") then
    ii = ii + 1
  end if
  if(ii>len(a)) return
  pos = ii
  do ii=pos,len(a)
    index = iachar(a(ii:ii)) - iachar("0")
    if(index<0 .or. index>9) exit
    i = i*10 + index
  end do
  if(minus) i = - i
  if(present(check)) then
    pos = ii
    do ii=pos,len(a)
      if(a(ii:ii)/=" ") return
    end do
    check = .true.
  end if
end subroutine iotk_atoi1
#endif
# 61 "iotk_xtox.spp"
#ifdef __IOTK_INTEGER2
subroutine iotk_atoi2(i,a,check)
  use iotk_base
  use iotk_interface
  implicit none
  character(len=*),                  intent(in)  :: a
  logical, optional,                 intent(out) :: check
  integer(kind=__IOTK_INTEGER2), intent(out) :: i
  logical :: minus
  integer :: pos,index,ii
! AGGIUNGERE DEI CHECK SULL'OVERFLOW
  minus = .false.
  i = 0
  if(present(check)) check = .false.
  if(len(a)==0) return
  do ii = 1 , len(a)
    if(a(ii:ii)/=" ") exit
  end do
  if(ii>len(a)) return
  if(a(ii:ii)=="-") then
    minus = .true.
    ii = ii + 1
  else if(a(ii:ii)=="+") then
    ii = ii + 1
  end if
  if(ii>len(a)) return
  pos = ii
  do ii=pos,len(a)
    index = iachar(a(ii:ii)) - iachar("0")
    if(index<0 .or. index>9) exit
    i = i*10 + index
  end do
  if(minus) i = - i
  if(present(check)) then
    pos = ii
    do ii=pos,len(a)
      if(a(ii:ii)/=" ") return
    end do
    check = .true.
  end if
end subroutine iotk_atoi2
#endif
# 61 "iotk_xtox.spp"
#ifdef __IOTK_INTEGER3
subroutine iotk_atoi3(i,a,check)
  use iotk_base
  use iotk_interface
  implicit none
  character(len=*),                  intent(in)  :: a
  logical, optional,                 intent(out) :: check
  integer(kind=__IOTK_INTEGER3), intent(out) :: i
  logical :: minus
  integer :: pos,index,ii
! AGGIUNGERE DEI CHECK SULL'OVERFLOW
  minus = .false.
  i = 0
  if(present(check)) check = .false.
  if(len(a)==0) return
  do ii = 1 , len(a)
    if(a(ii:ii)/=" ") exit
  end do
  if(ii>len(a)) return
  if(a(ii:ii)=="-") then
    minus = .true.
    ii = ii + 1
  else if(a(ii:ii)=="+") then
    ii = ii + 1
  end if
  if(ii>len(a)) return
  pos = ii
  do ii=pos,len(a)
    index = iachar(a(ii:ii)) - iachar("0")
    if(index<0 .or. index>9) exit
    i = i*10 + index
  end do
  if(minus) i = - i
  if(present(check)) then
    pos = ii
    do ii=pos,len(a)
      if(a(ii:ii)/=" ") return
    end do
    check = .true.
  end if
end subroutine iotk_atoi3
#endif
# 61 "iotk_xtox.spp"
#ifdef __IOTK_INTEGER4
subroutine iotk_atoi4(i,a,check)
  use iotk_base
  use iotk_interface
  implicit none
  character(len=*),                  intent(in)  :: a
  logical, optional,                 intent(out) :: check
  integer(kind=__IOTK_INTEGER4), intent(out) :: i
  logical :: minus
  integer :: pos,index,ii
! AGGIUNGERE DEI CHECK SULL'OVERFLOW
  minus = .false.
  i = 0
  if(present(check)) check = .false.
  if(len(a)==0) return
  do ii = 1 , len(a)
    if(a(ii:ii)/=" ") exit
  end do
  if(ii>len(a)) return
  if(a(ii:ii)=="-") then
    minus = .true.
    ii = ii + 1
  else if(a(ii:ii)=="+") then
    ii = ii + 1
  end if
  if(ii>len(a)) return
  pos = ii
  do ii=pos,len(a)
    index = iachar(a(ii:ii)) - iachar("0")
    if(index<0 .or. index>9) exit
    i = i*10 + index
  end do
  if(minus) i = - i
  if(present(check)) then
    pos = ii
    do ii=pos,len(a)
      if(a(ii:ii)/=" ") return
    end do
    check = .true.
  end if
end subroutine iotk_atoi4
#endif
# 104 "iotk_xtox.spp"

# 106 "iotk_xtox.spp"
#ifdef __IOTK_INTEGER1
function iotk_itoa1(i,length)
  use iotk_base
  use iotk_interface
  implicit none
  integer(kind=__IOTK_INTEGER1), intent(in)  :: i
  integer, optional, intent(out)                 :: length
  character(len=range(i)+2) :: iotk_itoa1
  integer(kind=__IOTK_INTEGER1) :: itmp
  integer :: pos,pos1
  character(len=range(i)+2) :: tmp
  itmp = abs(i)
  do pos = 1 , len(tmp)
    tmp(pos:pos) = achar( modulo(itmp,int(10,kind(itmp))) + iachar("0") )
    itmp = itmp/10
    if(itmp==0) exit
    if(pos==len(tmp)) exit
  end do
  if(i<0) then
    tmp(pos+1:pos+1)="-"
    pos = pos + 1
  end if
  do pos1=1,pos
    iotk_itoa1(pos1:pos1) = tmp(pos-pos1+1:pos-pos1+1)
  end do
  if(present(length)) length = pos
  do pos1=pos+1,len(iotk_itoa1)
    iotk_itoa1(pos1:pos1) = " "
  end do
end function iotk_itoa1
#endif
# 106 "iotk_xtox.spp"
#ifdef __IOTK_INTEGER2
function iotk_itoa2(i,length)
  use iotk_base
  use iotk_interface
  implicit none
  integer(kind=__IOTK_INTEGER2), intent(in)  :: i
  integer, optional, intent(out)                 :: length
  character(len=range(i)+2) :: iotk_itoa2
  integer(kind=__IOTK_INTEGER2) :: itmp
  integer :: pos,pos1
  character(len=range(i)+2) :: tmp
  itmp = abs(i)
  do pos = 1 , len(tmp)
    tmp(pos:pos) = achar( modulo(itmp,int(10,kind(itmp))) + iachar("0") )
    itmp = itmp/10
    if(itmp==0) exit
    if(pos==len(tmp)) exit
  end do
  if(i<0) then
    tmp(pos+1:pos+1)="-"
    pos = pos + 1
  end if
  do pos1=1,pos
    iotk_itoa2(pos1:pos1) = tmp(pos-pos1+1:pos-pos1+1)
  end do
  if(present(length)) length = pos
  do pos1=pos+1,len(iotk_itoa2)
    iotk_itoa2(pos1:pos1) = " "
  end do
end function iotk_itoa2
#endif
# 106 "iotk_xtox.spp"
#ifdef __IOTK_INTEGER3
function iotk_itoa3(i,length)
  use iotk_base
  use iotk_interface
  implicit none
  integer(kind=__IOTK_INTEGER3), intent(in)  :: i
  integer, optional, intent(out)                 :: length
  character(len=range(i)+2) :: iotk_itoa3
  integer(kind=__IOTK_INTEGER3) :: itmp
  integer :: pos,pos1
  character(len=range(i)+2) :: tmp
  itmp = abs(i)
  do pos = 1 , len(tmp)
    tmp(pos:pos) = achar( modulo(itmp,int(10,kind(itmp))) + iachar("0") )
    itmp = itmp/10
    if(itmp==0) exit
    if(pos==len(tmp)) exit
  end do
  if(i<0) then
    tmp(pos+1:pos+1)="-"
    pos = pos + 1
  end if
  do pos1=1,pos
    iotk_itoa3(pos1:pos1) = tmp(pos-pos1+1:pos-pos1+1)
  end do
  if(present(length)) length = pos
  do pos1=pos+1,len(iotk_itoa3)
    iotk_itoa3(pos1:pos1) = " "
  end do
end function iotk_itoa3
#endif
# 106 "iotk_xtox.spp"
#ifdef __IOTK_INTEGER4
function iotk_itoa4(i,length)
  use iotk_base
  use iotk_interface
  implicit none
  integer(kind=__IOTK_INTEGER4), intent(in)  :: i
  integer, optional, intent(out)                 :: length
  character(len=range(i)+2) :: iotk_itoa4
  integer(kind=__IOTK_INTEGER4) :: itmp
  integer :: pos,pos1
  character(len=range(i)+2) :: tmp
  itmp = abs(i)
  do pos = 1 , len(tmp)
    tmp(pos:pos) = achar( modulo(itmp,int(10,kind(itmp))) + iachar("0") )
    itmp = itmp/10
    if(itmp==0) exit
    if(pos==len(tmp)) exit
  end do
  if(i<0) then
    tmp(pos+1:pos+1)="-"
    pos = pos + 1
  end if
  do pos1=1,pos
    iotk_itoa4(pos1:pos1) = tmp(pos-pos1+1:pos-pos1+1)
  end do
  if(present(length)) length = pos
  do pos1=pos+1,len(iotk_itoa4)
    iotk_itoa4(pos1:pos1) = " "
  end do
end function iotk_itoa4
#endif
# 138 "iotk_xtox.spp"

# 140 "iotk_xtox.spp"
#ifdef __IOTK_LOGICAL1
function iotk_ltoa1(l)
  use iotk_base
  use iotk_interface
  implicit none
  logical(kind=__IOTK_LOGICAL1), intent(in) :: l
  character                                     :: iotk_ltoa1
  if(l) then
    iotk_ltoa1 = "T"
  else
    iotk_ltoa1 = "F"
  end if
end function iotk_ltoa1
#endif
# 140 "iotk_xtox.spp"
#ifdef __IOTK_LOGICAL2
function iotk_ltoa2(l)
  use iotk_base
  use iotk_interface
  implicit none
  logical(kind=__IOTK_LOGICAL2), intent(in) :: l
  character                                     :: iotk_ltoa2
  if(l) then
    iotk_ltoa2 = "T"
  else
    iotk_ltoa2 = "F"
  end if
end function iotk_ltoa2
#endif
# 140 "iotk_xtox.spp"
#ifdef __IOTK_LOGICAL3
function iotk_ltoa3(l)
  use iotk_base
  use iotk_interface
  implicit none
  logical(kind=__IOTK_LOGICAL3), intent(in) :: l
  character                                     :: iotk_ltoa3
  if(l) then
    iotk_ltoa3 = "T"
  else
    iotk_ltoa3 = "F"
  end if
end function iotk_ltoa3
#endif
# 140 "iotk_xtox.spp"
#ifdef __IOTK_LOGICAL4
function iotk_ltoa4(l)
  use iotk_base
  use iotk_interface
  implicit none
  logical(kind=__IOTK_LOGICAL4), intent(in) :: l
  character                                     :: iotk_ltoa4
  if(l) then
    iotk_ltoa4 = "T"
  else
    iotk_ltoa4 = "F"
  end if
end function iotk_ltoa4
#endif
