# 1 "iotk_unit.spp"
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

# 28 "iotk_unit.spp"
#include "iotk_auxmacros.h"
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
call iotk_error_msg(ierrl,"CVS Revision: 1.10 ")
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
call iotk_error_msg(ierrl,"CVS Revision: 1.10 ")
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
    write(unit,"(a,l8)") "Raw  :",this%raw
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

# 129 "iotk_unit.spp"
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
# 145 "iotk_unit.spp"
call iotk_error_msg(ierr,"CVS Revision: 1.10 ")
# 145 "iotk_unit.spp"
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

# 164 "iotk_unit.spp"
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
# 180 "iotk_unit.spp"
call iotk_error_msg(ierr,"CVS Revision: 1.10 ")
# 180 "iotk_unit.spp"
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
# 189 "iotk_unit.spp"
call iotk_error_msg(ierr,"CVS Revision: 1.10 ")
    return
  end if
  if(.not. binary) then
    if(opened .and. iotk_toupper(blank)/="NULL") then
      call iotk_error_issue(ierr,"iotk_inquire",__FILE__,__LINE__)
# 194 "iotk_unit.spp"
call iotk_error_msg(ierr,"CVS Revision: 1.10 ")
      return
    end if
    if(opened .and. iotk_toupper(pad)  /="YES") then
      call iotk_error_issue(ierr,"iotk_inquire",__FILE__,__LINE__)
# 198 "iotk_unit.spp"
call iotk_error_msg(ierr,"CVS Revision: 1.10 ")
      return
    end if
  end if
end subroutine iotk_inquire_x

# 205 "iotk_unit.spp"
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
# 222 "iotk_unit.spp"
call iotk_error_msg(ierr,"CVS Revision: 1.10 ")
      return
    end if
    if(this%unit == unit) exit
    prev => this
    this => this%next
  end do
  if(associated(this%son)) then
    call iotk_error_issue(ierr,"iotk_unit_del",__FILE__,__LINE__)
# 230 "iotk_unit.spp"
call iotk_error_msg(ierr,"CVS Revision: 1.10 ")
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

# 245 "iotk_unit.spp"
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
# 257 "iotk_unit.spp"
call iotk_error_msg(ierr,"CVS Revision: 1.10 ")
    return
  end if
  call iotk_unit_get(son,pointer=this_son)
  if(.not.associated(this_son)) then
    call iotk_error_issue(ierr,"iotk_unit_parent",__FILE__,__LINE__)
# 262 "iotk_unit.spp"
call iotk_error_msg(ierr,"CVS Revision: 1.10 ")
    return
  end if
  if(associated(this_parent%son)) then
    call iotk_error_issue(ierr,"iotk_unit_parent",__FILE__,__LINE__)
# 266 "iotk_unit.spp"
call iotk_error_msg(ierr,"CVS Revision: 1.10 ")
    return
  end if
  if(associated(this_son%parent)) then
    call iotk_error_issue(ierr,"iotk_unit_parent",__FILE__,__LINE__)
# 270 "iotk_unit.spp"
call iotk_error_msg(ierr,"CVS Revision: 1.10 ")
    return
  end if
  this_parent%son => this_son
  this_son%parent => this_parent
end subroutine iotk_unit_parent_x

# 278 "iotk_unit.spp"
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
