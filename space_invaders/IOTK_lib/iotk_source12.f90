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

# 30 "iotk_write.spp"

# 33 "iotk_write.spp"

# 35 "iotk_write.spp"
subroutine iotk_write_begin_x(unit,name,attr,ierr)
  use iotk_base
  use iotk_interface
  implicit none
  integer,                intent(in)  :: unit
  character(*),           intent(in)  :: name
  character(*), optional, intent(in)  :: attr
  integer,      optional, intent(out) :: ierr
  character(iotk_taglenx) :: tag
  character(iotk_attlenx) :: attrl
  character(iotk_fillenx) :: oldfile
  type(iotk_unit), pointer :: this_unit
  logical :: binary,raw
  integer :: ierrl,lunit,link_unit,iostat
  ierrl = 0
  iostat = 0
  lunit = iotk_phys_unit(unit)
  ierrl=0
  call iotk_unit_get(lunit,raw=raw,pointer=this_unit)
  if(raw) goto 1
  if(.not.iotk_check_name(name)) then
    call iotk_error_issue(ierrl,"iotk_write_begin",__FILE__,__LINE__)
# 56 "iotk_write.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.1 ")
# 56 "iotk_write.spp"
call iotk_error_msg(ierrl,'Wrong tag name')
# 56 "iotk_write.spp"
call iotk_error_write(ierrl,"name",name)
    goto 1
  end if
  attrl(1:1)=iotk_eos
  if(present(attr)) then
    call iotk_strcpy(attrl,attr,ierrl)
    if(ierrl/=0) then
      call iotk_error_issue(ierrl,"iotk_write_begin",__FILE__,__LINE__)
# 63 "iotk_write.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.1 ")
      goto 1
    end if
  end if
  call iotk_strcpy(tag,iotk_strtrim(name),ierr=ierrl)
  if(ierrl/=0) then
    call iotk_error_issue(ierrl,"iotk_write_begin",__FILE__,__LINE__)
# 69 "iotk_write.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.1 ")
    goto 1
  end if
  call iotk_strcat(tag,attrl,ierr=ierrl)
  if(ierrl/=0) then
    call iotk_error_issue(ierrl,"iotk_write_begin",__FILE__,__LINE__)
# 74 "iotk_write.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.1 ")
    goto 1
  end if
  call iotk_inquire(lunit,binary=binary,ierr=ierrl)
  if(ierrl/=0) then
    call iotk_error_issue(ierrl,"iotk_write_begin",__FILE__,__LINE__)
# 79 "iotk_write.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.1 ")
    goto 1
  end if
  call iotk_write_tag(lunit,1,tag,binary,ierrl)
  if(ierrl/=0) then
    call iotk_error_issue(ierrl,"iotk_write_begin",__FILE__,__LINE__)
# 84 "iotk_write.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.1 ")
# 84 "iotk_write.spp"
call iotk_error_msg(ierrl,'Error writing tag')
# 84 "iotk_write.spp"
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
    
# 99 "iotk_write.spp"
subroutine iotk_write_end_x(unit,name,ierr)
  use iotk_base
  use iotk_interface
  implicit none
  integer,                intent(in)  :: unit
  character(*),           intent(in)  :: name
  integer,      optional, intent(out) :: ierr
  character(iotk_taglenx) :: tag
  logical :: binary,raw
  integer :: ierrl,lunit
  type(iotk_unit), pointer :: this_unit
  ierrl = 0
  lunit = iotk_phys_unit(unit)
  ierrl=0
  call iotk_unit_get(lunit,pointer=this_unit,raw=raw)
  if(.not.iotk_check_name(name)) then
    call iotk_error_issue(ierrl,"iotk_write_end",__FILE__,__LINE__)
# 115 "iotk_write.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.1 ")
# 115 "iotk_write.spp"
call iotk_error_msg(ierrl,'Wrong tag name')
# 115 "iotk_write.spp"
call iotk_error_write(ierrl,"name",iotk_strtrim(name))
    goto 1
  end if
  call iotk_strcpy(tag,iotk_strtrim(name),ierr=ierrl)
  if(ierrl/=0) then
    call iotk_error_issue(ierrl,"iotk_write_end",__FILE__,__LINE__)
# 120 "iotk_write.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.1 ")
    goto 1
  end if
  if(raw) goto 2
  call iotk_inquire(lunit,binary=binary,ierr=ierrl)
  if(ierrl/=0) then
    call iotk_error_issue(ierrl,"iotk_write_end",__FILE__,__LINE__)
# 126 "iotk_write.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.1 ")
    goto 1
  end if
  call iotk_write_tag(lunit,2,tag,binary,ierrl)
  if(ierrl/=0) then
    call iotk_error_issue(ierrl,"iotk_write_end",__FILE__,__LINE__)
# 131 "iotk_write.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.1 ")
    goto 1
  end if
2 continue
  if(ierrl==0 .and. associated(this_unit)) then
    this_unit%level = this_unit%level - 1
  end if
  if(associated(this_unit) .and. unit/=lunit) then
    if(associated(this_unit%parent) .and. this_unit%level == 0 .and. this_unit%skip_root) then
      call iotk_close_write(lunit,ierr=ierrl)
      if(ierrl/=0) then
        call iotk_error_issue(ierrl,"iotk_write_end",__FILE__,__LINE__)
# 142 "iotk_write.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.1 ")
        goto 1
      end if
      lunit = iotk_phys_unit(unit)
      call iotk_unit_get(lunit,raw=raw,pointer=this_unit)
      if(.not.associated(this_unit)) then
        call iotk_error_issue(ierrl,"iotk_write_end",__FILE__,__LINE__)
# 148 "iotk_write.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.1 ")
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
    
# 162 "iotk_write.spp"
subroutine iotk_write_pi_x(unit,name,attr,ierr)
  use iotk_base
  use iotk_interface
  implicit none
  integer,                intent(in)  :: unit
  character(*),           intent(in)  :: name
  character(*), optional, intent(in)  :: attr
  integer,      optional, intent(out) :: ierr
  character(iotk_taglenx) :: tag
  character(iotk_attlenx) :: attrl
  logical :: binary,raw
  integer :: ierrl,lunit
  ierrl = 0
  lunit = iotk_phys_unit(unit)
  ierrl=0
  call iotk_unit_get(lunit,raw=raw)
  if(raw) goto 1
  if(.not.iotk_check_name(name)) then
    call iotk_error_issue(ierrl,"iotk_write_pi",__FILE__,__LINE__)
# 180 "iotk_write.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.1 ")
# 180 "iotk_write.spp"
call iotk_error_msg(ierrl,'Wrong tag name')
# 180 "iotk_write.spp"
call iotk_error_write(ierrl,"name",iotk_strtrim(name))
    goto 1
  end if
  attrl(1:1)=iotk_eos
  if(present(attr)) then
    call iotk_strcpy(attrl,attr,ierrl)
    if(ierrl/=0) then
      call iotk_error_issue(ierrl,"iotk_write_pi",__FILE__,__LINE__)
# 187 "iotk_write.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.1 ")
      goto 1
    end if
  end if
  call iotk_strcpy(tag,iotk_strtrim(name),ierr=ierrl)
  if(ierrl/=0) then
    call iotk_error_issue(ierrl,"iotk_write_pi",__FILE__,__LINE__)
# 193 "iotk_write.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.1 ")
    goto 1
  end if
  call iotk_strcat(tag,attrl,ierr=ierrl)
  if(ierrl/=0) then
    call iotk_error_issue(ierrl,"iotk_write_pi",__FILE__,__LINE__)
# 198 "iotk_write.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.1 ")
    goto 1 
  end if
  call iotk_inquire(lunit,binary=binary,ierr=ierrl)
  if(ierrl/=0) then
    call iotk_error_issue(ierrl,"iotk_write_pi",__FILE__,__LINE__)
# 203 "iotk_write.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.1 ")
    goto 1
  end if
  call iotk_write_tag(lunit,5,tag,binary,ierrl)
  if(ierrl/=0) then
    call iotk_error_issue(ierrl,"iotk_write_pi",__FILE__,__LINE__)
# 208 "iotk_write.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.1 ")
    goto 1
  end if
1 continue
  if(present(ierr)) then
    ierr = ierrl
  else
    if(ierrl/=0) call iotk_error_handler(ierrl)
  end if
end subroutine iotk_write_pi_x

# 220 "iotk_write.spp"
subroutine iotk_write_comment_x(unit,text,ierr)
  use iotk_base
  use iotk_interface
  implicit none
  integer,                intent(in)  :: unit
  character(*),           intent(in)  :: text
  integer,      optional, intent(out) :: ierr
  integer :: ierrl,lunit
  integer :: taglen
  logical :: binary,raw
  character(iotk_taglenx) :: tag
  ierrl = 0
  lunit = iotk_phys_unit(unit)
  ierrl=0
  call iotk_unit_get(lunit,raw=raw)
  if(raw) goto 1
  call iotk_deescape(tag,text)
  call iotk_inquire(lunit,binary=binary,ierr=ierrl)
  if(ierrl/=0) then
    call iotk_error_issue(ierrl,"iotk_write_comment",__FILE__,__LINE__)
# 239 "iotk_write.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.1 ")
    goto 1
  end if
  call iotk_write_tag(lunit,4,tag,binary,ierrl)
  if(ierrl/=0) then
    call iotk_error_issue(ierrl,"iotk_write_comment",__FILE__,__LINE__)
# 244 "iotk_write.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.1 ")
    goto 1
  end if
1 continue
  if(present(ierr)) then
    ierr = ierrl
  else
    if(ierrl/=0) call iotk_error_handler(ierrl)
  end if
end subroutine iotk_write_comment_x

# 256 "iotk_write.spp"
subroutine iotk_write_empty_x(unit,name,attr,ierr)
  use iotk_base
  use iotk_interface
  integer,                intent(in)  :: unit
  character(*),           intent(in)  :: name
  character(*), optional, intent(in)  :: attr
  integer,      optional, intent(out) :: ierr
  character(iotk_taglenx) :: tag
  character(iotk_attlenx) :: attrl
  logical :: binary,raw
  integer :: ierrl,lunit
  ierrl = 0
  lunit = iotk_phys_unit(unit)
  call iotk_unit_get(lunit,raw=raw)
  if(raw) goto 1
  if(.not.iotk_check_name(name)) then
    call iotk_error_issue(ierrl,"iotk_write_empty",__FILE__,__LINE__)
# 272 "iotk_write.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.1 ")
# 272 "iotk_write.spp"
call iotk_error_msg(ierrl,'Wrong tag name')
# 272 "iotk_write.spp"
call iotk_error_write(ierrl,"name",trim(name))
    goto 1
  end if
  attrl(1:1)=iotk_eos
  if(present(attr)) then
    call iotk_strcpy(attrl,attr,ierrl)
    if(ierrl/=0) then
      call iotk_error_issue(ierrl,"iotk_write_empty",__FILE__,__LINE__)
# 279 "iotk_write.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.1 ")
      goto 1
    end if
  end if
  call iotk_strcpy(tag,iotk_strtrim(name),ierr=ierrl)
  if(ierrl/=0) then
    call iotk_error_issue(ierrl,"iotk_write_empty",__FILE__,__LINE__)
# 285 "iotk_write.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.1 ")
    goto 1
  end if
  call iotk_strcat(tag,attrl,ierr=ierrl)
  if(ierrl/=0) then
    call iotk_error_issue(ierrl,"iotk_write_empty",__FILE__,__LINE__)
# 290 "iotk_write.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.1 ")
    goto 1
  end if
  call iotk_inquire(lunit,binary=binary,ierr=ierrl)
  if(ierrl/=0) then
    call iotk_error_issue(ierrl,"iotk_write_empty",__FILE__,__LINE__)
# 295 "iotk_write.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.1 ")
    goto 1
  end if
  call iotk_write_tag(lunit,3,tag,binary,ierrl)
  if(ierrl/=0) then
    call iotk_error_issue(ierrl,"iotk_write_empty",__FILE__,__LINE__)
# 300 "iotk_write.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.1 ")
    goto 1
  end if
1 continue
  if(present(ierr)) then
    ierr = ierrl
  else
    if(ierrl/=0) call iotk_error_handler(ierrl)
  end if
end subroutine iotk_write_empty_x

# 312 "iotk_write.spp"
subroutine iotk_write_tag_x(unit,control,tag,binary,ierr)
  use iotk_base
  use iotk_interface
  implicit none
  integer,                   intent(in)  :: unit
  integer,                   intent(in)  :: control
  character(iotk_taglenx),   intent(in)  :: tag
  logical,                   intent(in)  :: binary
  integer,                   intent(out) :: ierr
  integer(iotk_header_kind) :: header,header2
  integer :: taglen
  integer :: iostat,pos1,pos2
  character(4) :: begin,end
  iostat = 0
  ierr = 0
  taglen = iotk_strlen(tag)
  if(binary) then
    header  = control + taglen*(iotk_ncontrol+1)
    header2 = 128     + taglen*(iotk_ncontrol+1)
    write(unit,iostat=iostat) header
    if(iostat/=0) then
      call iotk_error_issue(ierr,"iotk_write_tag",__FILE__,__LINE__)
# 333 "iotk_write.spp"
call iotk_error_msg(ierr,"CVS Revision: 1.1 ")
# 333 "iotk_write.spp"
call iotk_error_msg(ierr,'error writing the header record')
# 333 "iotk_write.spp"
call iotk_error_write(ierr,"iostat",iostat)
    end if
    write(unit,iostat=iostat) header2,tag(1:taglen)
  else
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
    pos1=0
    write(unit,"(a)",iostat=iostat,advance="no") trim(begin)
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
# 374 "iotk_write.spp"
call iotk_error_msg(ierr,"CVS Revision: 1.1 ")
# 374 "iotk_write.spp"
call iotk_error_msg(ierr,'error writing')
# 374 "iotk_write.spp"
call iotk_error_write(ierr,"iostat",iostat)
  end if
end subroutine iotk_write_tag_x
