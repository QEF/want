# 1 "iotk_write.spp"
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

# 28 "iotk_write.spp"
#include "iotk_auxmacros.h"
# 30 "iotk_write.spp"

# 33 "iotk_write.spp"

# 35 "iotk_write.spp"
subroutine iotk_write_begin_x(unit,name,attr,dummy,ierr)
  use iotk_base
  use iotk_error_interf
  use iotk_misc_interf
  use iotk_write_interf
  use iotk_str_interf
  use iotk_unit_interf
  implicit none
  integer,                intent(in)  :: unit
  character(*),           intent(in)  :: name
  character(*), optional, intent(in)  :: attr
  type(iotk_dummytype), optional      :: dummy
  integer,      optional, intent(out) :: ierr
  character(iotk_taglenx) :: tag
  character(iotk_attlenx) :: attrl
  character(iotk_fillenx) :: oldfile
  type(iotk_unit), pointer :: this_unit
  integer :: indent
  logical :: binary
  integer :: ierrl,lunit,link_unit,iostat
  ierrl = 0
  iostat = 0
  lunit = iotk_phys_unit(unit)
  ierrl=0
  indent=0
  call iotk_unit_get(lunit,pointer=this_unit)
  if(associated(this_unit)) then
    if(this_unit%raw) goto 1
  end if
  if(.not.iotk_check_name(name)) then
    call iotk_error_issue(ierrl,"iotk_write_begin",__FILE__,__LINE__)
# 65 "iotk_write.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
# 65 "iotk_write.spp"
call iotk_error_msg(ierrl,'Wrong tag name')
# 65 "iotk_write.spp"
call iotk_error_write(ierrl,"name",name)
    goto 1
  end if
  attrl(1:1)=iotk_eos
  if(present(attr)) then
    call iotk_strcpy(attrl,attr,ierrl)
    if(ierrl/=0) then
      call iotk_error_issue(ierrl,"iotk_write_begin",__FILE__,__LINE__)
# 72 "iotk_write.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
      goto 1
    end if
  end if
  call iotk_strcpy(tag,iotk_strtrim(name),ierr=ierrl)
  if(ierrl/=0) then
    call iotk_error_issue(ierrl,"iotk_write_begin",__FILE__,__LINE__)
# 78 "iotk_write.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
    goto 1
  end if
  call iotk_strcat(tag,attrl,ierr=ierrl)
  if(ierrl/=0) then
    call iotk_error_issue(ierrl,"iotk_write_begin",__FILE__,__LINE__)
# 83 "iotk_write.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
    goto 1
  end if
  call iotk_inquire(lunit,binary=binary,ierr=ierrl)
  if(ierrl/=0) then
    call iotk_error_issue(ierrl,"iotk_write_begin",__FILE__,__LINE__)
# 88 "iotk_write.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
    goto 1
  end if
  if(associated(this_unit)) indent = iotk_indent*(this_unit%level+1)
  call iotk_write_tag(lunit,1,tag,binary,indent,ierrl)
  if(ierrl/=0) then
    call iotk_error_issue(ierrl,"iotk_write_begin",__FILE__,__LINE__)
# 94 "iotk_write.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
# 94 "iotk_write.spp"
call iotk_error_msg(ierrl,'Error writing tag')
# 94 "iotk_write.spp"
call iotk_error_write(ierrl,"name",name)
    goto 1
  end if
1 continue
  if(ierrl==0 .and. associated(this_unit)) then
    this_unit%level = this_unit%level + 1
  end if
  if(present(ierr)) then
    ierr = ierrl
  else
    if(ierrl/=0) call iotk_error_handler(ierrl)
  end if
end subroutine iotk_write_begin_x
    
# 109 "iotk_write.spp"
subroutine iotk_write_end_x(unit,name,dummy,ierr)
  use iotk_base
  use iotk_error_interf
  use iotk_files_interf
  use iotk_write_interf
  use iotk_misc_interf
  use iotk_str_interf
  use iotk_unit_interf
  implicit none
  integer,                intent(in)  :: unit
  character(*),           intent(in)  :: name
  type(iotk_dummytype), optional      :: dummy
  integer,      optional, intent(out) :: ierr
  character(iotk_taglenx) :: tag
  logical :: binary
  integer :: ierrl,lunit,indent
  type(iotk_unit), pointer :: this_unit
  ierrl = 0
  lunit = iotk_phys_unit(unit)
  ierrl=0
  indent=0
  call iotk_unit_get(lunit,pointer=this_unit)
  if(.not.iotk_check_name(name)) then
    call iotk_error_issue(ierrl,"iotk_write_end",__FILE__,__LINE__)
# 132 "iotk_write.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
# 132 "iotk_write.spp"
call iotk_error_msg(ierrl,'Wrong tag name')
# 132 "iotk_write.spp"
call iotk_error_write(ierrl,"name",iotk_strtrim(name))
    goto 1
  end if
  call iotk_strcpy(tag,iotk_strtrim(name),ierr=ierrl)
  if(ierrl/=0) then
    call iotk_error_issue(ierrl,"iotk_write_end",__FILE__,__LINE__)
# 137 "iotk_write.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
    goto 1
  end if
  if(associated(this_unit)) then
    if(this_unit%raw) goto 2
  end if
  call iotk_inquire(lunit,binary=binary,ierr=ierrl)
  if(ierrl/=0) then
    call iotk_error_issue(ierrl,"iotk_write_end",__FILE__,__LINE__)
# 145 "iotk_write.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
    goto 1
  end if
  if(associated(this_unit)) indent = iotk_indent * this_unit%level
  call iotk_write_tag(lunit,2,tag,binary,indent,ierrl)
  if(ierrl/=0) then
    call iotk_error_issue(ierrl,"iotk_write_end",__FILE__,__LINE__)
# 151 "iotk_write.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
    goto 1
  end if
2 continue
  if(ierrl==0 .and. associated(this_unit)) then
    this_unit%level = this_unit%level - 1
  end if
  if(associated(this_unit) .and. unit/=lunit) then
    if(associated(this_unit%parent) .and. this_unit%level == -1 .and. this_unit%skip_root) then
      call iotk_close_write(lunit,ierr=ierrl)
      if(ierrl/=0) then
        call iotk_error_issue(ierrl,"iotk_write_end",__FILE__,__LINE__)
# 162 "iotk_write.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
        goto 1
      end if
      lunit = iotk_phys_unit(unit)
      call iotk_unit_get(lunit,pointer=this_unit)
      if(.not.associated(this_unit)) then
        call iotk_error_issue(ierrl,"iotk_write_end",__FILE__,__LINE__)
# 168 "iotk_write.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
        goto 1
      end if
    end if
  end if
1 continue
  if(present(ierr)) then
    ierr = ierrl
  else
    if(ierrl/=0) call iotk_error_handler(ierrl)
  end if
end subroutine iotk_write_end_x
    
# 182 "iotk_write.spp"
subroutine iotk_write_pi_x(unit,name,attr,dummy,ierr)
  use iotk_base
  use iotk_error_interf
  use iotk_write_interf
  use iotk_misc_interf
  use iotk_str_interf
  use iotk_unit_interf
  implicit none
  integer,                intent(in)  :: unit
  character(*),           intent(in)  :: name
  character(*), optional, intent(in)  :: attr
  type(iotk_dummytype), optional      :: dummy
  integer,      optional, intent(out) :: ierr
  character(iotk_taglenx) :: tag
  character(iotk_attlenx) :: attrl
  logical :: binary
  integer :: ierrl,lunit,indent
  type(iotk_unit), pointer :: this_unit
  ierrl = 0
  lunit = iotk_phys_unit(unit)
  ierrl=0
  indent=0
  call iotk_unit_get(lunit,pointer=this_unit)
  if(associated(this_unit)) then
    if(this_unit%raw) goto 1
  end if
  if(.not.iotk_check_name(name)) then
    call iotk_error_issue(ierrl,"iotk_write_pi",__FILE__,__LINE__)
# 209 "iotk_write.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
# 209 "iotk_write.spp"
call iotk_error_msg(ierrl,'Wrong tag name')
# 209 "iotk_write.spp"
call iotk_error_write(ierrl,"name",iotk_strtrim(name))
    goto 1
  end if
  attrl(1:1)=iotk_eos
  if(present(attr)) then
    call iotk_strcpy(attrl,attr,ierrl)
    if(ierrl/=0) then
      call iotk_error_issue(ierrl,"iotk_write_pi",__FILE__,__LINE__)
# 216 "iotk_write.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
      goto 1
    end if
  end if
  call iotk_strcpy(tag,iotk_strtrim(name),ierr=ierrl)
  if(ierrl/=0) then
    call iotk_error_issue(ierrl,"iotk_write_pi",__FILE__,__LINE__)
# 222 "iotk_write.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
    goto 1
  end if
  call iotk_strcat(tag,attrl,ierr=ierrl)
  if(ierrl/=0) then
    call iotk_error_issue(ierrl,"iotk_write_pi",__FILE__,__LINE__)
# 227 "iotk_write.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
    goto 1 
  end if
  call iotk_inquire(lunit,binary=binary,ierr=ierrl)
  if(ierrl/=0) then
    call iotk_error_issue(ierrl,"iotk_write_pi",__FILE__,__LINE__)
# 232 "iotk_write.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
    goto 1
  end if
  if(associated(this_unit)) indent = iotk_indent*(this_unit%level+1)
  call iotk_write_tag(lunit,5,tag,binary,indent,ierrl)
  if(ierrl/=0) then
    call iotk_error_issue(ierrl,"iotk_write_pi",__FILE__,__LINE__)
# 238 "iotk_write.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
    goto 1
  end if
1 continue
  if(present(ierr)) then
    ierr = ierrl
  else
    if(ierrl/=0) call iotk_error_handler(ierrl)
  end if
end subroutine iotk_write_pi_x

# 250 "iotk_write.spp"
subroutine iotk_write_comment_x(unit,text,dummy,ierr)
  use iotk_base
  use iotk_error_interf
  use iotk_write_interf
  use iotk_misc_interf
  use iotk_str_interf
  use iotk_unit_interf
  implicit none
  integer,                intent(in)  :: unit
  character(*),           intent(in)  :: text
  type(iotk_dummytype), optional      :: dummy
  integer,      optional, intent(out) :: ierr
  integer :: ierrl,lunit
  integer :: taglen,indent
  logical :: binary
  character(iotk_taglenx) :: tag
  type(iotk_unit), pointer :: this
  ierrl = 0
  lunit = iotk_phys_unit(unit)
  ierrl=0
  indent = 0
  call iotk_unit_get(lunit,pointer=this)
  if(associated(this)) then
    if(this%raw) goto 1
  end if
  call iotk_deescape(tag,text)
  call iotk_inquire(lunit,binary=binary,ierr=ierrl)
  if(ierrl/=0) then
    call iotk_error_issue(ierrl,"iotk_write_comment",__FILE__,__LINE__)
# 278 "iotk_write.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
    goto 1
  end if
  if(associated(this)) indent = iotk_indent*(this%level+1)
  call iotk_write_tag(lunit,4,tag,binary,indent,ierrl)
  if(ierrl/=0) then
    call iotk_error_issue(ierrl,"iotk_write_comment",__FILE__,__LINE__)
# 284 "iotk_write.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
    goto 1
  end if
1 continue
  if(present(ierr)) then
    ierr = ierrl
  else
    if(ierrl/=0) call iotk_error_handler(ierrl)
  end if
end subroutine iotk_write_comment_x

# 296 "iotk_write.spp"
subroutine iotk_write_empty_x(unit,name,attr,dummy,ierr)
  use iotk_base
  use iotk_error_interf
  use iotk_write_interf
  use iotk_misc_interf
  use iotk_str_interf
  use iotk_unit_interf
  implicit none
  integer,                intent(in)  :: unit
  character(*),           intent(in)  :: name
  character(*), optional, intent(in)  :: attr
  type(iotk_dummytype), optional      :: dummy
  integer,      optional, intent(out) :: ierr
  character(iotk_taglenx) :: tag
  character(iotk_attlenx) :: attrl
  type(iotk_unit), pointer :: this_unit
  logical :: binary
  integer :: ierrl,lunit,indent
  indent = 0
  ierrl = 0
  lunit = iotk_phys_unit(unit)
  call iotk_unit_get(lunit,pointer=this_unit)
  if(associated(this_unit)) then
    if(this_unit%raw) goto 1
  end if
  if(.not.iotk_check_name(name)) then
    call iotk_error_issue(ierrl,"iotk_write_empty",__FILE__,__LINE__)
# 322 "iotk_write.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
# 322 "iotk_write.spp"
call iotk_error_msg(ierrl,'Wrong tag name')
# 322 "iotk_write.spp"
call iotk_error_write(ierrl,"name",trim(name))
    goto 1
  end if
  attrl(1:1)=iotk_eos
  if(present(attr)) then
    call iotk_strcpy(attrl,attr,ierrl)
    if(ierrl/=0) then
      call iotk_error_issue(ierrl,"iotk_write_empty",__FILE__,__LINE__)
# 329 "iotk_write.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
      goto 1
    end if
  end if
  call iotk_strcpy(tag,iotk_strtrim(name),ierr=ierrl)
  if(ierrl/=0) then
    call iotk_error_issue(ierrl,"iotk_write_empty",__FILE__,__LINE__)
# 335 "iotk_write.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
    goto 1
  end if
  call iotk_strcat(tag,attrl,ierr=ierrl)
  if(ierrl/=0) then
    call iotk_error_issue(ierrl,"iotk_write_empty",__FILE__,__LINE__)
# 340 "iotk_write.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
    goto 1
  end if
  call iotk_inquire(lunit,binary=binary,ierr=ierrl)
  if(ierrl/=0) then
    call iotk_error_issue(ierrl,"iotk_write_empty",__FILE__,__LINE__)
# 345 "iotk_write.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
    goto 1
  end if
  if(associated(this_unit)) indent = iotk_indent*(this_unit%level+1)
  call iotk_write_tag(lunit,3,tag,binary,indent,ierrl)
  if(ierrl/=0) then
    call iotk_error_issue(ierrl,"iotk_write_empty",__FILE__,__LINE__)
# 351 "iotk_write.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
    goto 1
  end if
1 continue
  if(present(ierr)) then
    ierr = ierrl
  else
    if(ierrl/=0) call iotk_error_handler(ierrl)
  end if
end subroutine iotk_write_empty_x

# 363 "iotk_write.spp"
subroutine iotk_write_tag_x(unit,control,tag,binary,indent,ierr)
  use iotk_base
  use iotk_error_interf
  use iotk_misc_interf
  use iotk_str_interf
  implicit none
  integer,                   intent(in)  :: unit
  integer,                   intent(in)  :: control
  character(iotk_taglenx),   intent(in)  :: tag
  logical,                   intent(in)  :: binary
  integer,                   intent(in)  :: indent
  integer,                   intent(out) :: ierr
  integer(iotk_header_kind) :: header,header2
  integer :: taglen,taglenp
  integer :: iostat,pos1,pos2
  integer :: lindent
  character(iotk_maxindent), parameter :: indentstr=""
  character(4) :: begin,end
  lindent = min(len(indentstr),indent)
  iostat = 0
  ierr = 0
  taglen = iotk_strlen(tag)
  select case(control)
  case(1)
    begin = "<"
    end   = ">"
  case(2)
    begin = "</"
    end   = ">"
  case(3)
    begin = "<"
    end   = "/>"
  case(4) 
    begin = "<!--"
    end   = "-->"
  case(5)
    begin = "<?"
    end   = "?>"
  end select
  if(binary) then
    taglenp = taglen + len_trim(begin) + len_trim(end) + 2 + lindent
    header  = control + taglenp*(iotk_ncontrol+1)
    header2 = 128     + taglenp*(iotk_ncontrol+1)
! taglenp e' la lunghezza TOTALE (inclusi delimitatori e newlines)
    write(unit,iostat=iostat) header
    if(iostat/=0) then
      call iotk_error_issue(ierr,"iotk_write_tag",__FILE__,__LINE__)
# 409 "iotk_write.spp"
call iotk_error_msg(ierr,"CVS Revision: 1.17 ")
# 409 "iotk_write.spp"
call iotk_error_msg(ierr,'error writing the header record')
# 409 "iotk_write.spp"
call iotk_error_write(ierr,"iostat",iostat)
    end if
    write(unit,iostat=iostat) header2,iotk_newline//indentstr(1:lindent)// &
                         trim(begin)//tag(1:taglen)//trim(end)//iotk_newline
  else
    pos1=0
    write(unit,"(a)",iostat=iostat,advance="no") indentstr(1:lindent)//trim(begin)
    do
      if(pos1+iotk_linlen >= taglen ) then
        pos2 = taglen+1
      else
        pos2 = pos1 + scan(tag(pos1+1:pos1+iotk_linlen)," ",back=.true.)
        if(pos2<=pos1) then
          pos2 = pos1+iotk_linlen + scan(tag(pos1+iotk_linlen+1:taglen)," ")
          if(pos2<=pos1+iotk_linlen) pos2=taglen+1
        end if
      end if
      write(unit,"(a)",iostat=iostat,advance="no") tag(pos1+1:pos2-1)
      pos1=pos2
      if(pos1>taglen) exit
      write(unit,*,iostat=iostat)
    end do
    write(unit,"(a)",iostat=iostat) trim(end)
  end if
  if(iostat/=0) then
    call iotk_error_issue(ierr,"iotk_write_tag",__FILE__,__LINE__)
# 434 "iotk_write.spp"
call iotk_error_msg(ierr,"CVS Revision: 1.17 ")
# 434 "iotk_write.spp"
call iotk_error_msg(ierr,'error writing')
# 434 "iotk_write.spp"
call iotk_error_write(ierr,"iostat",iostat)
  end if
end subroutine iotk_write_tag_x
