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

# 30 "iotk_unit.spp"

# 33 "iotk_unit.spp"

# 35 "iotk_unit.spp"
subroutine iotk_free_unit_x(unit,ierr)
  use iotk_base
  use iotk_error_interf
  implicit none
! This subroutine sets 'unit' to the number of
! an I/O unit which is free (i.e. not already opened).
! The search is carried out starting from unit
! 'unitmin' in a range of 'nsearch' units.
! The starting unit for the search is increased at each
! call, so that a number of subsequent ask can be done
! obtaining different units.
  integer,           intent(out) :: unit
  integer, optional, intent(out) :: ierr
  integer, save :: offset = 0
  logical       :: opened,exist
  integer       :: isearch,nsearch,unitmin
  integer       :: ierrl
  integer       :: iostat
  iostat = 0
  unitmin = iotk_unitmin
  nsearch = iotk_unitmax - iotk_unitmin + 1
  ierrl = 0 
  do isearch=0,nsearch-1
    unit = modulo(isearch+offset,nsearch) + unitmin
    inquire(unit=unit,opened=opened,exist=exist,iostat=iostat)
    if((.not.opened .and. exist) .or. iostat/=0) exit
  end do
  if(iostat/=0) then
    call iotk_error_issue(ierrl,"iotk_free_unit",__FILE__,__LINE__)
# 63 "iotk_unit.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.7 ")
# 63 "iotk_unit.spp"
call iotk_error_msg(ierrl,'Error inquiring')
# 63 "iotk_unit.spp"
call iotk_error_write(ierrl,"unit",unit)
# 63 "iotk_unit.spp"
call iotk_error_write(ierrl,"iostat",iostat)
    goto 1
  end if 
  if(isearch>=nsearch) then
    call iotk_error_issue(ierrl,"iotk_free_unit",__FILE__,__LINE__)
# 67 "iotk_unit.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.7 ")
# 67 "iotk_unit.spp"
call iotk_error_msg(ierrl,'There are no units left')
    goto 1
  end if 
1 continue
  offset = modulo(unit - unitmin + 1,nsearch)
  if(present(ierr)) then 
    ierr = ierrl
  else
    if(ierrl/=0) call iotk_error_handler(ierrl)
  end if
end subroutine iotk_free_unit_x

# 80 "iotk_unit.spp"
function iotk_phys_unit_x(unit) result(result)
  use iotk_base
  use iotk_unit_interf
  implicit none
  integer, intent(in) :: unit
  integer :: result
  integer :: ierrl
  type(iotk_unit), pointer :: this
  ierrl = 0
  result = unit
  if(.not. iotk_units_init) then
    iotk_units_init = .true.
    nullify(iotk_units)
  end if
  call iotk_unit_get(unit,pointer=this)
  if(.not.associated(this)) return
  do
    if(.not. associated(this%son)) exit
    this => this%son
  end do
  result = this%unit
end function iotk_phys_unit_x

# 104 "iotk_unit.spp"
subroutine iotk_unit_print_x(unit)
  use iotk_base
  use iotk_str_interf
  implicit none 
  integer, intent(in) :: unit
  type (iotk_unit), pointer :: this
  this => iotk_units
  write(unit,"(a)") "IOTK units"
  do
    if(.not. associated(this)) exit
    write(unit,"(a,i8)") "Unit :",this%unit
    write(unit,"(a,a,a,i8)") "Root :",this%root(1:iotk_strlen_trim(this%root)),"Level:",this%level
    if(associated(this%son)) then
      write(unit,"(a,i8)") "Son :",this%son%unit
    end if
    if(associated(this%parent)) then
      write(unit,"(a,i8)") "Parent :",this%parent%unit
    end if
    this => this%next
  end do
  write(unit,"(a)") "end IOTK units"
end subroutine iotk_unit_print_x

# 128 "iotk_unit.spp"
subroutine iotk_unit_add_x(unit,this,ierr)
  use iotk_base
  use iotk_error_interf
  implicit none
  integer,      intent(in)  :: unit
  type (iotk_unit), pointer :: this
  integer,      intent(out) :: ierr
  ierr = 0
  if(.not. iotk_units_init) then
    iotk_units_init = .true.
    nullify(iotk_units)
  end if 
  this => iotk_units
  do
    if(.not.associated(this)) exit
    if(this%unit == unit) then
      call iotk_error_issue(ierr,"iotk_unit_add",__FILE__,__LINE__)
# 144 "iotk_unit.spp"
call iotk_error_msg(ierr,"CVS Revision: 1.7 ")
# 144 "iotk_unit.spp"
call iotk_error_msg(ierr,'unit')
      return
    end if
    this => this%next
  end do
  allocate(this)
  this%unit         = unit
  this%root         = ""
  this%skip_root    = .false.
  this%raw          = .false.
  this%level        = 0
  this%close_at_end = .false.
  this%next  => iotk_units
  nullify(this%son)
  nullify(this%parent)
  iotk_units => this
end subroutine iotk_unit_add_x

# 163 "iotk_unit.spp"
subroutine iotk_inquire_x(unit,binary,ierr)
  use iotk_base
  use iotk_error_interf
  use iotk_str_interf
  use iotk_misc_interf
  implicit none
  integer,           intent(in)  :: unit
  logical,           intent(out) :: binary
  integer,           intent(out) :: ierr
  character(50) :: form,access,pad,blank
  logical :: opened
  integer :: iostat
  iostat = 0
  ierr = 0
  inquire(unit=unit,form=form,iostat=iostat,access=access,pad=pad,blank=blank,opened=opened)
  if(iostat/=0) then
    call iotk_error_issue(ierr,"iotk_inquire",__FILE__,__LINE__)
# 179 "iotk_unit.spp"
call iotk_error_msg(ierr,"CVS Revision: 1.7 ")
# 179 "iotk_unit.spp"
call iotk_error_msg(ierr,'Error inquiring')
    return
  end if
  if(opened .and. iotk_toupper(form)=="UNFORMATTED") then
    binary = .true.
  else
    binary = .false.
  end if
  if(opened .and. iotk_toupper(access)/="SEQUENTIAL") then
    call iotk_error_issue(ierr,"iotk_inquire",__FILE__,__LINE__)
# 188 "iotk_unit.spp"
call iotk_error_msg(ierr,"CVS Revision: 1.7 ")
    return
  end if
  if(.not. binary) then
    if(opened .and. iotk_toupper(blank)/="NULL") then
      call iotk_error_issue(ierr,"iotk_inquire",__FILE__,__LINE__)
# 193 "iotk_unit.spp"
call iotk_error_msg(ierr,"CVS Revision: 1.7 ")
      return
    end if
    if(opened .and. iotk_toupper(pad)  /="YES") then
      call iotk_error_issue(ierr,"iotk_inquire",__FILE__,__LINE__)
# 197 "iotk_unit.spp"
call iotk_error_msg(ierr,"CVS Revision: 1.7 ")
      return
    end if
  end if
end subroutine iotk_inquire_x

# 204 "iotk_unit.spp"
subroutine iotk_unit_del_x(unit,ierr)
  use iotk_base
  use iotk_error_interf
  use iotk_misc_interf
  implicit none
  integer, intent(in)  :: unit
  integer, intent(out) :: ierr
  type (iotk_unit), pointer :: this,prev
  ierr = 0
  if(.not. iotk_units_init) then
    iotk_units_init = .true.
    nullify(iotk_units)
  end if
  nullify(prev)
  this => iotk_units
  do
    if(.not.associated(this)) then
      call iotk_error_issue(ierr,"iotk_unit_del",__FILE__,__LINE__)
# 221 "iotk_unit.spp"
call iotk_error_msg(ierr,"CVS Revision: 1.7 ")
      return
    end if
    if(this%unit == unit) exit
    prev => this
    this => this%next
  end do
  if(associated(this%son)) then
    call iotk_error_issue(ierr,"iotk_unit_del",__FILE__,__LINE__)
# 229 "iotk_unit.spp"
call iotk_error_msg(ierr,"CVS Revision: 1.7 ")
    return
  end if
  if(associated(this%parent)) then
  end if
  if(associated(this%parent)) nullify(this%parent%son)
  if(associated(prev)) then
    prev%next  => this%next
  else
    iotk_units => this%next
  end if
  deallocate(this)
end subroutine iotk_unit_del_x

# 244 "iotk_unit.spp"
subroutine iotk_unit_parent_x(parent,son,ierr)
  use iotk_base
  use iotk_error_interf
  use iotk_misc_interf
  use iotk_unit_interf
  implicit none
  integer, intent(in) :: parent,son
  integer, intent(out) :: ierr
  type(iotk_unit), pointer :: this_parent,this_son
  ierr = 0
  call iotk_unit_get(parent,pointer=this_parent)
  if(.not.associated(this_parent)) then
    call iotk_error_issue(ierr,"iotk_unit_parent",__FILE__,__LINE__)
# 256 "iotk_unit.spp"
call iotk_error_msg(ierr,"CVS Revision: 1.7 ")
    return
  end if
  call iotk_unit_get(son,pointer=this_son)
  if(.not.associated(this_son)) then
    call iotk_error_issue(ierr,"iotk_unit_parent",__FILE__,__LINE__)
# 261 "iotk_unit.spp"
call iotk_error_msg(ierr,"CVS Revision: 1.7 ")
    return
  end if
  if(associated(this_parent%son)) then
    call iotk_error_issue(ierr,"iotk_unit_parent",__FILE__,__LINE__)
# 265 "iotk_unit.spp"
call iotk_error_msg(ierr,"CVS Revision: 1.7 ")
    return
  end if
  if(associated(this_son%parent)) then
    call iotk_error_issue(ierr,"iotk_unit_parent",__FILE__,__LINE__)
# 269 "iotk_unit.spp"
call iotk_error_msg(ierr,"CVS Revision: 1.7 ")
    return
  end if
  this_parent%son => this_son
  this_son%parent => this_parent
end subroutine iotk_unit_parent_x

# 277 "iotk_unit.spp"
subroutine iotk_unit_get_x(unit,pointer)
  use iotk_base
  use iotk_misc_interf
  implicit none
  integer,                intent(in)  :: unit
  type(iotk_unit), optional, pointer :: pointer
  type (iotk_unit), pointer :: this
  if(present(pointer)) nullify(pointer)
  if(.not. iotk_units_init) then
    iotk_units_init = .true.
    nullify(iotk_units)
  end if
  this => iotk_units
  do
    if(.not.associated(this)) exit
    if(this%unit == unit) exit
    this => this%next
  end do
  if(associated(this)) then
    if(present(pointer)) pointer => this
  end if
end subroutine iotk_unit_get_x
