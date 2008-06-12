# 48 "iotk_attr.spp"
! Input/Output Tool Kit (IOTK)
! Copyright (C) 2004-2006 Giovanni Bussi
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
# 65 "iotk_attr.spp"


!------------------------------------------------------------------------------!
! Inclusion of configuration file
#include "iotk_config.h"
!------------------------------------------------------------------------------!

# 74 "iotk_attr.spp"
#include "iotk_auxmacros.h"
# 76 "iotk_attr.spp"

# 78 "iotk_attr.spp"

#ifdef __IOTK_LOGICAL1
#if 0 <= __IOTK_MAXRANK

# 83 "iotk_attr.spp"
! This is needed as a workaround for bugged pack 
subroutine iotk_private_pack_LOGICAL1(out,in,n,l)
    use iotk_base
    implicit none
    integer,                                    intent(in)  :: n,l
# 92 "iotk_attr.spp"
    LOGICAL (kind=iotk_LOGICAL1), intent(out) :: out(n)
    LOGICAL (kind=iotk_LOGICAL1), intent(in)  :: in(n)
# 95 "iotk_attr.spp"
    out = in
end subroutine iotk_private_pack_LOGICAL1

# 100 "iotk_attr.spp"
subroutine iotk_write_LOGICAL1(val,string,ierr)
  use iotk_base
  use iotk_error_interf
  use iotk_xtox_interf
  use iotk_fmt_interf
  use iotk_str_interf
  use iotk_misc_interf
  implicit none
  LOGICAL(kind=iotk_LOGICAL1), intent(in) :: val(:)
#ifdef __IOTK_WORKAROUND6
  character(len=*)              :: string
#else
  character(len=*), intent(out) :: string
#endif
  integer, intent(out) :: ierr
# 118 "iotk_attr.spp"
  integer :: index,iostat
  ierr = 0
  iostat = 0 
  string(1:1) = iotk_eos
  if(size(val)==0) return
  if(len(string)==0) then
    call iotk_error_issue(ierr,"iotk_write",__FILE__,__LINE__)
# 124 "iotk_attr.spp"
call iotk_error_msg(ierr,"CVS Revision: 1.20 ")
    return
  end if
  do index=1,size(val)
# 129 "iotk_attr.spp"
    call iotk_strcat(string,iotk_ltoa(val(index))//" ",ierr)
    if(ierr/=0) then
      call iotk_error_issue(ierr,"iotk_write",__FILE__,__LINE__)
# 131 "iotk_attr.spp"
call iotk_error_msg(ierr,"CVS Revision: 1.20 ")
      return
    end if
# 152 "iotk_attr.spp"
  end do
! the last blank is deleted
  string(iotk_strlen(string):iotk_strlen(string)) = iotk_eos
end subroutine iotk_write_LOGICAL1
# 158 "iotk_attr.spp"

# 162 "iotk_attr.spp"
subroutine iotk_read_LOGICAL1(val,string,index,ierr)
  use iotk_base
  use iotk_error_interf
  use iotk_xtox_interf
  use iotk_misc_interf
  implicit none
  LOGICAL(kind=iotk_LOGICAL1), intent(inout) :: val(:)
  character(len=*), intent(in) :: string
  integer, intent(inout) :: index
  integer, intent(out) :: ierr
# 173 "iotk_attr.spp"
  logical :: check
# 175 "iotk_attr.spp"
  integer :: pos,pos1,iostat
  integer :: maxindex
# 181 "iotk_attr.spp"
  pos = 0
  pos1= 0
  ierr = 0
  iostat = 0
# 188 "iotk_attr.spp"
    maxindex = size(val)
# 190 "iotk_attr.spp"
! for the moment, commas are considered as blanks
  do
    pos = verify(string(pos1+1:)," ,")+pos1
    if(pos==pos1) exit
    pos = pos - 1
    pos1 = scan(string(pos+1:)," ,")+pos
    if(pos1==pos) pos1 = len(string) + 1
!READ string(pos+1:pos1-1)
    index = index+1
    if(index>maxindex) then
      call iotk_error_issue(ierr,"iotk_read",__FILE__,__LINE__)
# 200 "iotk_attr.spp"
call iotk_error_msg(ierr,"CVS Revision: 1.20 ")
# 200 "iotk_attr.spp"
call iotk_error_msg(ierr,'Too many data')
    end if
# 205 "iotk_attr.spp"
    val(index) = iotk_atol(string(pos+1:pos1-1),check=check)
# 218 "iotk_attr.spp"
    if(.not.check) then
      call iotk_error_issue(ierr,"iotk_read",__FILE__,__LINE__)
# 219 "iotk_attr.spp"
call iotk_error_msg(ierr,"CVS Revision: 1.20 ")
# 219 "iotk_attr.spp"
call iotk_error_msg(ierr,'Wrong string')
# 219 "iotk_attr.spp"
call iotk_error_write(ierr,"string",string(pos+1:pos1-1))
      return
    end if
# 228 "iotk_attr.spp"
    if(pos1>=len(string)) exit
  end do
end subroutine iotk_read_LOGICAL1
# 233 "iotk_attr.spp"

# 236 "iotk_attr.spp"
subroutine iotk_write_attr_LOGICAL1_0(attr,name,val,dummy,first,newline,ierr)
  use iotk_base
  use iotk_error_interf
  use iotk_attr_interf
  use iotk_str_interf
  use iotk_misc_interf
  implicit none
  character(*), intent(inout) :: attr
  character(*), intent(in)    :: name
  LOGICAL(kind=iotk_LOGICAL1), intent(in)  :: val 
  type(iotk_dummytype), optional :: dummy
  logical, optional, intent(in)  :: first
  logical, optional, intent(in)  :: newline
  integer, optional, intent(out) :: ierr
  integer :: ierrl
  integer :: attlen
  integer :: vallen
  integer :: namlen
  character :: delim
# 258 "iotk_attr.spp"
  character(iotk_vallenx) :: tmpval
  logical :: nl
  if(present(newline)) then
    nl = newline
  else
    nl = .false.
  endif
  ierrl = 0
  if(present(first)) then
    if(first) attr(1:1) = iotk_eos
  end if
  attlen = iotk_strlen_trim(attr)
  namlen = iotk_strlen_trim(name)
  if(.not.iotk_check_name(name)) then
    call iotk_error_issue(ierrl,"iotk_write_attr",__FILE__,__LINE__)
# 272 "iotk_attr.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.20 ")
# 272 "iotk_attr.spp"
call iotk_error_msg(ierrl,'Wrong tag name')
# 272 "iotk_attr.spp"
call iotk_error_write(ierrl,"name",name(1:namlen))
    goto 1
  end if
# 289 "iotk_attr.spp"
  delim = '"'
# 291 "iotk_attr.spp"
  call iotk_write((/val/),tmpval,ierrl)
# 295 "iotk_attr.spp"
  if(ierrl/=0) then
    call iotk_error_issue(ierrl,"iotk_write_attr",__FILE__,__LINE__)
# 296 "iotk_attr.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.20 ")
    goto 1
  end if
# 300 "iotk_attr.spp"
  vallen = iotk_strlen(tmpval)
  if(attlen+vallen+namlen+5>len(attr)) then
    call iotk_error_issue(ierrl,"iotk_write_attr",__FILE__,__LINE__)
# 302 "iotk_attr.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.20 ")
# 302 "iotk_attr.spp"
call iotk_error_msg(ierrl,'Attribute dummy argument is too short')
    goto 1
  end if
  if(.not. nl) then
    attr(attlen+1:attlen+vallen+namlen+5) = " "//name(1:namlen)//"="//delim//tmpval(1:vallen)//delim//iotk_eos
  else
    attr(attlen+1:attlen+vallen+namlen+len(iotk_newline)+5) &
       = iotk_newline//" "//name(1:namlen)//"="//delim//tmpval(1:vallen)//delim//iotk_eos
  endif
1 continue
  if(present(ierr)) then
    ierr = ierrl
  else
    if(ierrl/=0) call iotk_error_handler(ierrl)
  end if
end subroutine iotk_write_attr_LOGICAL1_0

# 320 "iotk_attr.spp"
subroutine iotk_scan_attr_LOGICAL1_0(attr,name,val,dummy,found,default,eos,ierr)
  use iotk_base
  use iotk_error_interf
  use iotk_attr_interf, only : iotk_read
  use iotk_str_interf
  use iotk_misc_interf
  implicit none
  character(*),             intent(in)  :: attr
  character(*),             intent(in)  :: name
#ifdef __IOTK_WORKAROUND6
  LOGICAL(kind=iotk_LOGICAL1)                        :: val 
#else
  LOGICAL(kind=iotk_LOGICAL1), intent(out)           :: val 
#endif
  type(iotk_dummytype), optional :: dummy
  logical,        optional, intent(out) :: found
  LOGICAL(kind=iotk_LOGICAL1), optional, intent(in)  :: default 
  logical,        optional, intent(in)  :: eos
  integer,        optional, intent(out) :: ierr
  integer :: ierrl
  integer :: attlen,pos,equal,namlen
  character :: delim
  logical :: foundl
  character(iotk_vallenx) :: valc
# 351 "iotk_attr.spp"
  integer :: index
  LOGICAL(kind=iotk_LOGICAL1), allocatable :: tmpval (:)
# 354 "iotk_attr.spp"
  ierrl = 0
  attlen=iotk_strlen(attr)
  namlen=iotk_strlen_trim(name)
  foundl = .false.
  equal = 0
  do
    pos = verify(attr(equal+1:attlen)," ")
    if(pos<=0) exit
    equal = equal + pos
    pos = scan(attr(equal+1:attlen),"=")
    if(pos<=0) then
      call iotk_error_issue(ierrl,"iotk_scan_attr",__FILE__,__LINE__)
# 365 "iotk_attr.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.20 ")
# 365 "iotk_attr.spp"
call iotk_error_msg(ierrl,'')
# 365 "iotk_attr.spp"
call iotk_error_write(ierrl,"attr",attr(equal+1:attlen))
      goto 1
    end if
    equal = equal + pos
    if(trim(attr(equal-pos:equal-1))==name(1:namlen)) foundl = .true.
    pos = verify(attr(equal+1:attlen)," ")
    if(pos<=0) then
      call iotk_error_issue(ierrl,"iotk_scan_attr",__FILE__,__LINE__)
# 372 "iotk_attr.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.20 ")
      goto 1
    end if
    equal = equal + pos
    delim = attr(equal:equal)
    if(delim/="'" .and. delim/='"') then
      call iotk_error_issue(ierrl,"iotk_scan_attr",__FILE__,__LINE__)
# 378 "iotk_attr.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.20 ")
      goto 1
    end if
    pos = scan(attr(equal+1:attlen),delim)
    if(pos<=0) then
      call iotk_error_issue(ierrl,"iotk_scan_attr",__FILE__,__LINE__)
# 383 "iotk_attr.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.20 ")
      goto 1
    end if
    if(foundl) exit
    equal = equal + pos
  end do
  if(foundl) then
    call iotk_strcpy(valc,attr(equal+1:equal+pos-1),ierrl)
    if(ierrl/=0) then
      call iotk_error_issue(ierrl,"iotk_scan_attr",__FILE__,__LINE__)
# 392 "iotk_attr.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.20 ")
      goto 1
    end if
  else
    goto 1
  end if
# 413 "iotk_attr.spp"
  allocate(tmpval(1))
  index = 0
  call iotk_str_clean(valc)
  call iotk_read(tmpval,valc(1:iotk_strlen(valc)),index,ierrl)
  if(ierrl/=0) then
    call iotk_error_issue(ierrl,"iotk_scan_attr",__FILE__,__LINE__)
# 418 "iotk_attr.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.20 ")
    goto 1
  end if
# 424 "iotk_attr.spp"
  if(index/=1) then
# 426 "iotk_attr.spp"
    call iotk_error_issue(ierrl,"iotk_scan_attr",__FILE__,__LINE__)
# 426 "iotk_attr.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.20 ")
# 426 "iotk_attr.spp"
call iotk_error_msg(ierrl,'Attribute size does not match')
# 426 "iotk_attr.spp"
call iotk_error_write(ierrl,"attr",valc)
# 426 "iotk_attr.spp"
call iotk_error_write(ierrl,"size",size(tmpval))
    goto 1
  end if
# 430 "iotk_attr.spp"
  val = tmpval(1)
# 434 "iotk_attr.spp"
  deallocate(tmpval)
# 436 "iotk_attr.spp"
1 continue
  if(ierrl/=0) foundl=.false.
  if(present(found)) found = foundl
  if(ierrl==0 .and. .not. present(found) .and. .not. present(default) .and. .not. foundl) then
    call iotk_error_issue(ierrl,"iotk_scan_attr",__FILE__,__LINE__)
# 440 "iotk_attr.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.20 ")
# 440 "iotk_attr.spp"
call iotk_error_msg(ierrl,'Attribute not found')
# 440 "iotk_attr.spp"
call iotk_error_write(ierrl,"name",name)
    ierrl = - ierrl
  end if
  if(present(default) .and. .not. foundl) then
# 453 "iotk_attr.spp"
    val = default
# 455 "iotk_attr.spp"
  end if
  if(present(ierr)) then
    ierr = ierrl
  else
    if(ierrl/=0) call iotk_error_handler(ierrl)
  end if
end subroutine iotk_scan_attr_LOGICAL1_0
# 463 "iotk_attr.spp"

#endif
#endif

subroutine iotk_attr_dummy_LOGICAL1_0
  write(0,*)
end subroutine iotk_attr_dummy_LOGICAL1_0

# 45 "iotk_attr.spp"

# 65 "iotk_attr.spp"


!------------------------------------------------------------------------------!
! Inclusion of configuration file
#include "iotk_config.h"
!------------------------------------------------------------------------------!

# 74 "iotk_attr.spp"
#include "iotk_auxmacros.h"
# 76 "iotk_attr.spp"

# 78 "iotk_attr.spp"

#ifdef __IOTK_LOGICAL1
#if 1 <= __IOTK_MAXRANK

# 158 "iotk_attr.spp"

# 233 "iotk_attr.spp"

# 236 "iotk_attr.spp"
subroutine iotk_write_attr_LOGICAL1_1(attr,name,val,dummy,first,newline,ierr)
  use iotk_base
  use iotk_error_interf
  use iotk_attr_interf
  use iotk_str_interf
  use iotk_misc_interf
  implicit none
  character(*), intent(inout) :: attr
  character(*), intent(in)    :: name
  LOGICAL(kind=iotk_LOGICAL1), intent(in)  :: val (:)
  type(iotk_dummytype), optional :: dummy
  logical, optional, intent(in)  :: first
  logical, optional, intent(in)  :: newline
  integer, optional, intent(out) :: ierr
  integer :: ierrl
  integer :: attlen
  integer :: vallen
  integer :: namlen
  character :: delim
# 258 "iotk_attr.spp"
  character(iotk_vallenx) :: tmpval
  logical :: nl
  if(present(newline)) then
    nl = newline
  else
    nl = .false.
  endif
  ierrl = 0
  if(present(first)) then
    if(first) attr(1:1) = iotk_eos
  end if
  attlen = iotk_strlen_trim(attr)
  namlen = iotk_strlen_trim(name)
  if(.not.iotk_check_name(name)) then
    call iotk_error_issue(ierrl,"iotk_write_attr",__FILE__,__LINE__)
# 272 "iotk_attr.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.20 ")
# 272 "iotk_attr.spp"
call iotk_error_msg(ierrl,'Wrong tag name')
# 272 "iotk_attr.spp"
call iotk_error_write(ierrl,"name",name(1:namlen))
    goto 1
  end if
# 289 "iotk_attr.spp"
  delim = '"'
# 293 "iotk_attr.spp"
  call iotk_write(pack(val,mask=.true.),tmpval,ierrl)
# 295 "iotk_attr.spp"
  if(ierrl/=0) then
    call iotk_error_issue(ierrl,"iotk_write_attr",__FILE__,__LINE__)
# 296 "iotk_attr.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.20 ")
    goto 1
  end if
# 300 "iotk_attr.spp"
  vallen = iotk_strlen(tmpval)
  if(attlen+vallen+namlen+5>len(attr)) then
    call iotk_error_issue(ierrl,"iotk_write_attr",__FILE__,__LINE__)
# 302 "iotk_attr.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.20 ")
# 302 "iotk_attr.spp"
call iotk_error_msg(ierrl,'Attribute dummy argument is too short')
    goto 1
  end if
  if(.not. nl) then
    attr(attlen+1:attlen+vallen+namlen+5) = " "//name(1:namlen)//"="//delim//tmpval(1:vallen)//delim//iotk_eos
  else
    attr(attlen+1:attlen+vallen+namlen+len(iotk_newline)+5) &
       = iotk_newline//" "//name(1:namlen)//"="//delim//tmpval(1:vallen)//delim//iotk_eos
  endif
1 continue
  if(present(ierr)) then
    ierr = ierrl
  else
    if(ierrl/=0) call iotk_error_handler(ierrl)
  end if
end subroutine iotk_write_attr_LOGICAL1_1

# 320 "iotk_attr.spp"
subroutine iotk_scan_attr_LOGICAL1_1(attr,name,val,dummy,found,default,eos,ierr)
  use iotk_base
  use iotk_error_interf
  use iotk_attr_interf, only : iotk_read
  use iotk_str_interf
  use iotk_misc_interf
  implicit none
  character(*),             intent(in)  :: attr
  character(*),             intent(in)  :: name
#ifdef __IOTK_WORKAROUND6
  LOGICAL(kind=iotk_LOGICAL1)                        :: val (:)
#else
  LOGICAL(kind=iotk_LOGICAL1), intent(out)           :: val (:)
#endif
  type(iotk_dummytype), optional :: dummy
  logical,        optional, intent(out) :: found
  LOGICAL(kind=iotk_LOGICAL1), optional, intent(in)  :: default (:)
  logical,        optional, intent(in)  :: eos
  integer,        optional, intent(out) :: ierr
  integer :: ierrl
  integer :: attlen,pos,equal,namlen
  character :: delim
  logical :: foundl
  character(iotk_vallenx) :: valc
# 351 "iotk_attr.spp"
  integer :: index
  LOGICAL(kind=iotk_LOGICAL1), allocatable :: tmpval (:)
# 354 "iotk_attr.spp"
  ierrl = 0
  attlen=iotk_strlen(attr)
  namlen=iotk_strlen_trim(name)
  foundl = .false.
  equal = 0
  do
    pos = verify(attr(equal+1:attlen)," ")
    if(pos<=0) exit
    equal = equal + pos
    pos = scan(attr(equal+1:attlen),"=")
    if(pos<=0) then
      call iotk_error_issue(ierrl,"iotk_scan_attr",__FILE__,__LINE__)
# 365 "iotk_attr.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.20 ")
# 365 "iotk_attr.spp"
call iotk_error_msg(ierrl,'')
# 365 "iotk_attr.spp"
call iotk_error_write(ierrl,"attr",attr(equal+1:attlen))
      goto 1
    end if
    equal = equal + pos
    if(trim(attr(equal-pos:equal-1))==name(1:namlen)) foundl = .true.
    pos = verify(attr(equal+1:attlen)," ")
    if(pos<=0) then
      call iotk_error_issue(ierrl,"iotk_scan_attr",__FILE__,__LINE__)
# 372 "iotk_attr.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.20 ")
      goto 1
    end if
    equal = equal + pos
    delim = attr(equal:equal)
    if(delim/="'" .and. delim/='"') then
      call iotk_error_issue(ierrl,"iotk_scan_attr",__FILE__,__LINE__)
# 378 "iotk_attr.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.20 ")
      goto 1
    end if
    pos = scan(attr(equal+1:attlen),delim)
    if(pos<=0) then
      call iotk_error_issue(ierrl,"iotk_scan_attr",__FILE__,__LINE__)
# 383 "iotk_attr.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.20 ")
      goto 1
    end if
    if(foundl) exit
    equal = equal + pos
  end do
  if(foundl) then
    call iotk_strcpy(valc,attr(equal+1:equal+pos-1),ierrl)
    if(ierrl/=0) then
      call iotk_error_issue(ierrl,"iotk_scan_attr",__FILE__,__LINE__)
# 392 "iotk_attr.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.20 ")
      goto 1
    end if
  else
    goto 1
  end if
# 413 "iotk_attr.spp"
  allocate(tmpval(size(val)))
  index = 0
  call iotk_str_clean(valc)
  call iotk_read(tmpval,valc(1:iotk_strlen(valc)),index,ierrl)
  if(ierrl/=0) then
    call iotk_error_issue(ierrl,"iotk_scan_attr",__FILE__,__LINE__)
# 418 "iotk_attr.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.20 ")
    goto 1
  end if
# 424 "iotk_attr.spp"
  if(index/=size(val)) then
# 426 "iotk_attr.spp"
    call iotk_error_issue(ierrl,"iotk_scan_attr",__FILE__,__LINE__)
# 426 "iotk_attr.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.20 ")
# 426 "iotk_attr.spp"
call iotk_error_msg(ierrl,'Attribute size does not match')
# 426 "iotk_attr.spp"
call iotk_error_write(ierrl,"attr",valc)
# 426 "iotk_attr.spp"
call iotk_error_write(ierrl,"size",size(tmpval))
    goto 1
  end if
# 432 "iotk_attr.spp"
  val = reshape (source=tmpval,shape=shape(val))
# 434 "iotk_attr.spp"
  deallocate(tmpval)
# 436 "iotk_attr.spp"
1 continue
  if(ierrl/=0) foundl=.false.
  if(present(found)) found = foundl
  if(ierrl==0 .and. .not. present(found) .and. .not. present(default) .and. .not. foundl) then
    call iotk_error_issue(ierrl,"iotk_scan_attr",__FILE__,__LINE__)
# 440 "iotk_attr.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.20 ")
# 440 "iotk_attr.spp"
call iotk_error_msg(ierrl,'Attribute not found')
# 440 "iotk_attr.spp"
call iotk_error_write(ierrl,"name",name)
    ierrl = - ierrl
  end if
  if(present(default) .and. .not. foundl) then
# 453 "iotk_attr.spp"
    val = default
# 455 "iotk_attr.spp"
  end if
  if(present(ierr)) then
    ierr = ierrl
  else
    if(ierrl/=0) call iotk_error_handler(ierrl)
  end if
end subroutine iotk_scan_attr_LOGICAL1_1
# 463 "iotk_attr.spp"

#endif
#endif

subroutine iotk_attr_dummy_LOGICAL1_1
  write(0,*)
end subroutine iotk_attr_dummy_LOGICAL1_1

# 45 "iotk_attr.spp"

# 65 "iotk_attr.spp"


!------------------------------------------------------------------------------!
! Inclusion of configuration file
#include "iotk_config.h"
!------------------------------------------------------------------------------!

# 74 "iotk_attr.spp"
#include "iotk_auxmacros.h"
# 76 "iotk_attr.spp"

# 78 "iotk_attr.spp"

#ifdef __IOTK_LOGICAL1
#if 2 <= __IOTK_MAXRANK

# 158 "iotk_attr.spp"

# 233 "iotk_attr.spp"

# 236 "iotk_attr.spp"
subroutine iotk_write_attr_LOGICAL1_2(attr,name,val,dummy,first,newline,ierr)
  use iotk_base
  use iotk_error_interf
  use iotk_attr_interf
  use iotk_str_interf
  use iotk_misc_interf
  implicit none
  character(*), intent(inout) :: attr
  character(*), intent(in)    :: name
  LOGICAL(kind=iotk_LOGICAL1), intent(in)  :: val (:,:)
  type(iotk_dummytype), optional :: dummy
  logical, optional, intent(in)  :: first
  logical, optional, intent(in)  :: newline
  integer, optional, intent(out) :: ierr
  integer :: ierrl
  integer :: attlen
  integer :: vallen
  integer :: namlen
  character :: delim
# 258 "iotk_attr.spp"
  character(iotk_vallenx) :: tmpval
  logical :: nl
  if(present(newline)) then
    nl = newline
  else
    nl = .false.
  endif
  ierrl = 0
  if(present(first)) then
    if(first) attr(1:1) = iotk_eos
  end if
  attlen = iotk_strlen_trim(attr)
  namlen = iotk_strlen_trim(name)
  if(.not.iotk_check_name(name)) then
    call iotk_error_issue(ierrl,"iotk_write_attr",__FILE__,__LINE__)
# 272 "iotk_attr.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.20 ")
# 272 "iotk_attr.spp"
call iotk_error_msg(ierrl,'Wrong tag name')
# 272 "iotk_attr.spp"
call iotk_error_write(ierrl,"name",name(1:namlen))
    goto 1
  end if
# 289 "iotk_attr.spp"
  delim = '"'
# 293 "iotk_attr.spp"
  call iotk_write(pack(val,mask=.true.),tmpval,ierrl)
# 295 "iotk_attr.spp"
  if(ierrl/=0) then
    call iotk_error_issue(ierrl,"iotk_write_attr",__FILE__,__LINE__)
# 296 "iotk_attr.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.20 ")
    goto 1
  end if
# 300 "iotk_attr.spp"
  vallen = iotk_strlen(tmpval)
  if(attlen+vallen+namlen+5>len(attr)) then
    call iotk_error_issue(ierrl,"iotk_write_attr",__FILE__,__LINE__)
# 302 "iotk_attr.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.20 ")
# 302 "iotk_attr.spp"
call iotk_error_msg(ierrl,'Attribute dummy argument is too short')
    goto 1
  end if
  if(.not. nl) then
    attr(attlen+1:attlen+vallen+namlen+5) = " "//name(1:namlen)//"="//delim//tmpval(1:vallen)//delim//iotk_eos
  else
    attr(attlen+1:attlen+vallen+namlen+len(iotk_newline)+5) &
       = iotk_newline//" "//name(1:namlen)//"="//delim//tmpval(1:vallen)//delim//iotk_eos
  endif
1 continue
  if(present(ierr)) then
    ierr = ierrl
  else
    if(ierrl/=0) call iotk_error_handler(ierrl)
  end if
end subroutine iotk_write_attr_LOGICAL1_2

# 320 "iotk_attr.spp"
subroutine iotk_scan_attr_LOGICAL1_2(attr,name,val,dummy,found,default,eos,ierr)
  use iotk_base
  use iotk_error_interf
  use iotk_attr_interf, only : iotk_read
  use iotk_str_interf
  use iotk_misc_interf
  implicit none
  character(*),             intent(in)  :: attr
  character(*),             intent(in)  :: name
#ifdef __IOTK_WORKAROUND6
  LOGICAL(kind=iotk_LOGICAL1)                        :: val (:,:)
#else
  LOGICAL(kind=iotk_LOGICAL1), intent(out)           :: val (:,:)
#endif
  type(iotk_dummytype), optional :: dummy
  logical,        optional, intent(out) :: found
  LOGICAL(kind=iotk_LOGICAL1), optional, intent(in)  :: default (:,:)
  logical,        optional, intent(in)  :: eos
  integer,        optional, intent(out) :: ierr
  integer :: ierrl
  integer :: attlen,pos,equal,namlen
  character :: delim
  logical :: foundl
  character(iotk_vallenx) :: valc
# 351 "iotk_attr.spp"
  integer :: index
  LOGICAL(kind=iotk_LOGICAL1), allocatable :: tmpval (:)
# 354 "iotk_attr.spp"
  ierrl = 0
  attlen=iotk_strlen(attr)
  namlen=iotk_strlen_trim(name)
  foundl = .false.
  equal = 0
  do
    pos = verify(attr(equal+1:attlen)," ")
    if(pos<=0) exit
    equal = equal + pos
    pos = scan(attr(equal+1:attlen),"=")
    if(pos<=0) then
      call iotk_error_issue(ierrl,"iotk_scan_attr",__FILE__,__LINE__)
# 365 "iotk_attr.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.20 ")
# 365 "iotk_attr.spp"
call iotk_error_msg(ierrl,'')
# 365 "iotk_attr.spp"
call iotk_error_write(ierrl,"attr",attr(equal+1:attlen))
      goto 1
    end if
    equal = equal + pos
    if(trim(attr(equal-pos:equal-1))==name(1:namlen)) foundl = .true.
    pos = verify(attr(equal+1:attlen)," ")
    if(pos<=0) then
      call iotk_error_issue(ierrl,"iotk_scan_attr",__FILE__,__LINE__)
# 372 "iotk_attr.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.20 ")
      goto 1
    end if
    equal = equal + pos
    delim = attr(equal:equal)
    if(delim/="'" .and. delim/='"') then
      call iotk_error_issue(ierrl,"iotk_scan_attr",__FILE__,__LINE__)
# 378 "iotk_attr.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.20 ")
      goto 1
    end if
    pos = scan(attr(equal+1:attlen),delim)
    if(pos<=0) then
      call iotk_error_issue(ierrl,"iotk_scan_attr",__FILE__,__LINE__)
# 383 "iotk_attr.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.20 ")
      goto 1
    end if
    if(foundl) exit
    equal = equal + pos
  end do
  if(foundl) then
    call iotk_strcpy(valc,attr(equal+1:equal+pos-1),ierrl)
    if(ierrl/=0) then
      call iotk_error_issue(ierrl,"iotk_scan_attr",__FILE__,__LINE__)
# 392 "iotk_attr.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.20 ")
      goto 1
    end if
  else
    goto 1
  end if
# 413 "iotk_attr.spp"
  allocate(tmpval(size(val)))
  index = 0
  call iotk_str_clean(valc)
  call iotk_read(tmpval,valc(1:iotk_strlen(valc)),index,ierrl)
  if(ierrl/=0) then
    call iotk_error_issue(ierrl,"iotk_scan_attr",__FILE__,__LINE__)
# 418 "iotk_attr.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.20 ")
    goto 1
  end if
# 424 "iotk_attr.spp"
  if(index/=size(val)) then
# 426 "iotk_attr.spp"
    call iotk_error_issue(ierrl,"iotk_scan_attr",__FILE__,__LINE__)
# 426 "iotk_attr.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.20 ")
# 426 "iotk_attr.spp"
call iotk_error_msg(ierrl,'Attribute size does not match')
# 426 "iotk_attr.spp"
call iotk_error_write(ierrl,"attr",valc)
# 426 "iotk_attr.spp"
call iotk_error_write(ierrl,"size",size(tmpval))
    goto 1
  end if
# 432 "iotk_attr.spp"
  val = reshape (source=tmpval,shape=shape(val))
# 434 "iotk_attr.spp"
  deallocate(tmpval)
# 436 "iotk_attr.spp"
1 continue
  if(ierrl/=0) foundl=.false.
  if(present(found)) found = foundl
  if(ierrl==0 .and. .not. present(found) .and. .not. present(default) .and. .not. foundl) then
    call iotk_error_issue(ierrl,"iotk_scan_attr",__FILE__,__LINE__)
# 440 "iotk_attr.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.20 ")
# 440 "iotk_attr.spp"
call iotk_error_msg(ierrl,'Attribute not found')
# 440 "iotk_attr.spp"
call iotk_error_write(ierrl,"name",name)
    ierrl = - ierrl
  end if
  if(present(default) .and. .not. foundl) then
# 453 "iotk_attr.spp"
    val = default
# 455 "iotk_attr.spp"
  end if
  if(present(ierr)) then
    ierr = ierrl
  else
    if(ierrl/=0) call iotk_error_handler(ierrl)
  end if
end subroutine iotk_scan_attr_LOGICAL1_2
# 463 "iotk_attr.spp"

#endif
#endif

subroutine iotk_attr_dummy_LOGICAL1_2
  write(0,*)
end subroutine iotk_attr_dummy_LOGICAL1_2

# 45 "iotk_attr.spp"

# 48 "iotk_attr.spp"
! Input/Output Tool Kit (IOTK)
! Copyright (C) 2004-2006 Giovanni Bussi
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
# 65 "iotk_attr.spp"


!------------------------------------------------------------------------------!
! Inclusion of configuration file
#include "iotk_config.h"
!------------------------------------------------------------------------------!

# 74 "iotk_attr.spp"
#include "iotk_auxmacros.h"
# 76 "iotk_attr.spp"

# 78 "iotk_attr.spp"

#ifdef __IOTK_LOGICAL1
#if 3 <= __IOTK_MAXRANK

# 158 "iotk_attr.spp"

# 233 "iotk_attr.spp"

# 236 "iotk_attr.spp"
subroutine iotk_write_attr_LOGICAL1_3(attr,name,val,dummy,first,newline,ierr)
  use iotk_base
  use iotk_error_interf
  use iotk_attr_interf
  use iotk_str_interf
  use iotk_misc_interf
  implicit none
  character(*), intent(inout) :: attr
  character(*), intent(in)    :: name
  LOGICAL(kind=iotk_LOGICAL1), intent(in)  :: val (:,:,:)
  type(iotk_dummytype), optional :: dummy
  logical, optional, intent(in)  :: first
  logical, optional, intent(in)  :: newline
  integer, optional, intent(out) :: ierr
  integer :: ierrl
  integer :: attlen
  integer :: vallen
  integer :: namlen
  character :: delim
# 258 "iotk_attr.spp"
  character(iotk_vallenx) :: tmpval
  logical :: nl
  if(present(newline)) then
    nl = newline
  else
    nl = .false.
  endif
  ierrl = 0
  if(present(first)) then
    if(first) attr(1:1) = iotk_eos
  end if
  attlen = iotk_strlen_trim(attr)
  namlen = iotk_strlen_trim(name)
  if(.not.iotk_check_name(name)) then
    call iotk_error_issue(ierrl,"iotk_write_attr",__FILE__,__LINE__)
# 272 "iotk_attr.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.20 ")
# 272 "iotk_attr.spp"
call iotk_error_msg(ierrl,'Wrong tag name')
# 272 "iotk_attr.spp"
call iotk_error_write(ierrl,"name",name(1:namlen))
    goto 1
  end if
# 289 "iotk_attr.spp"
  delim = '"'
# 293 "iotk_attr.spp"
  call iotk_write(pack(val,mask=.true.),tmpval,ierrl)
# 295 "iotk_attr.spp"
  if(ierrl/=0) then
    call iotk_error_issue(ierrl,"iotk_write_attr",__FILE__,__LINE__)
# 296 "iotk_attr.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.20 ")
    goto 1
  end if
# 300 "iotk_attr.spp"
  vallen = iotk_strlen(tmpval)
  if(attlen+vallen+namlen+5>len(attr)) then
    call iotk_error_issue(ierrl,"iotk_write_attr",__FILE__,__LINE__)
# 302 "iotk_attr.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.20 ")
# 302 "iotk_attr.spp"
call iotk_error_msg(ierrl,'Attribute dummy argument is too short')
    goto 1
  end if
  if(.not. nl) then
    attr(attlen+1:attlen+vallen+namlen+5) = " "//name(1:namlen)//"="//delim//tmpval(1:vallen)//delim//iotk_eos
  else
    attr(attlen+1:attlen+vallen+namlen+len(iotk_newline)+5) &
       = iotk_newline//" "//name(1:namlen)//"="//delim//tmpval(1:vallen)//delim//iotk_eos
  endif
1 continue
  if(present(ierr)) then
    ierr = ierrl
  else
    if(ierrl/=0) call iotk_error_handler(ierrl)
  end if
end subroutine iotk_write_attr_LOGICAL1_3

# 320 "iotk_attr.spp"
subroutine iotk_scan_attr_LOGICAL1_3(attr,name,val,dummy,found,default,eos,ierr)
  use iotk_base
  use iotk_error_interf
  use iotk_attr_interf, only : iotk_read
  use iotk_str_interf
  use iotk_misc_interf
  implicit none
  character(*),             intent(in)  :: attr
  character(*),             intent(in)  :: name
#ifdef __IOTK_WORKAROUND6
  LOGICAL(kind=iotk_LOGICAL1)                        :: val (:,:,:)
#else
  LOGICAL(kind=iotk_LOGICAL1), intent(out)           :: val (:,:,:)
#endif
  type(iotk_dummytype), optional :: dummy
  logical,        optional, intent(out) :: found
  LOGICAL(kind=iotk_LOGICAL1), optional, intent(in)  :: default (:,:,:)
  logical,        optional, intent(in)  :: eos
  integer,        optional, intent(out) :: ierr
  integer :: ierrl
  integer :: attlen,pos,equal,namlen
  character :: delim
  logical :: foundl
  character(iotk_vallenx) :: valc
# 351 "iotk_attr.spp"
  integer :: index
  LOGICAL(kind=iotk_LOGICAL1), allocatable :: tmpval (:)
# 354 "iotk_attr.spp"
  ierrl = 0
  attlen=iotk_strlen(attr)
  namlen=iotk_strlen_trim(name)
  foundl = .false.
  equal = 0
  do
    pos = verify(attr(equal+1:attlen)," ")
    if(pos<=0) exit
    equal = equal + pos
    pos = scan(attr(equal+1:attlen),"=")
    if(pos<=0) then
      call iotk_error_issue(ierrl,"iotk_scan_attr",__FILE__,__LINE__)
# 365 "iotk_attr.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.20 ")
# 365 "iotk_attr.spp"
call iotk_error_msg(ierrl,'')
# 365 "iotk_attr.spp"
call iotk_error_write(ierrl,"attr",attr(equal+1:attlen))
      goto 1
    end if
    equal = equal + pos
    if(trim(attr(equal-pos:equal-1))==name(1:namlen)) foundl = .true.
    pos = verify(attr(equal+1:attlen)," ")
    if(pos<=0) then
      call iotk_error_issue(ierrl,"iotk_scan_attr",__FILE__,__LINE__)
# 372 "iotk_attr.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.20 ")
      goto 1
    end if
    equal = equal + pos
    delim = attr(equal:equal)
    if(delim/="'" .and. delim/='"') then
      call iotk_error_issue(ierrl,"iotk_scan_attr",__FILE__,__LINE__)
# 378 "iotk_attr.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.20 ")
      goto 1
    end if
    pos = scan(attr(equal+1:attlen),delim)
    if(pos<=0) then
      call iotk_error_issue(ierrl,"iotk_scan_attr",__FILE__,__LINE__)
# 383 "iotk_attr.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.20 ")
      goto 1
    end if
    if(foundl) exit
    equal = equal + pos
  end do
  if(foundl) then
    call iotk_strcpy(valc,attr(equal+1:equal+pos-1),ierrl)
    if(ierrl/=0) then
      call iotk_error_issue(ierrl,"iotk_scan_attr",__FILE__,__LINE__)
# 392 "iotk_attr.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.20 ")
      goto 1
    end if
  else
    goto 1
  end if
# 413 "iotk_attr.spp"
  allocate(tmpval(size(val)))
  index = 0
  call iotk_str_clean(valc)
  call iotk_read(tmpval,valc(1:iotk_strlen(valc)),index,ierrl)
  if(ierrl/=0) then
    call iotk_error_issue(ierrl,"iotk_scan_attr",__FILE__,__LINE__)
# 418 "iotk_attr.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.20 ")
    goto 1
  end if
# 424 "iotk_attr.spp"
  if(index/=size(val)) then
# 426 "iotk_attr.spp"
    call iotk_error_issue(ierrl,"iotk_scan_attr",__FILE__,__LINE__)
# 426 "iotk_attr.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.20 ")
# 426 "iotk_attr.spp"
call iotk_error_msg(ierrl,'Attribute size does not match')
# 426 "iotk_attr.spp"
call iotk_error_write(ierrl,"attr",valc)
# 426 "iotk_attr.spp"
call iotk_error_write(ierrl,"size",size(tmpval))
    goto 1
  end if
# 432 "iotk_attr.spp"
  val = reshape (source=tmpval,shape=shape(val))
# 434 "iotk_attr.spp"
  deallocate(tmpval)
# 436 "iotk_attr.spp"
1 continue
  if(ierrl/=0) foundl=.false.
  if(present(found)) found = foundl
  if(ierrl==0 .and. .not. present(found) .and. .not. present(default) .and. .not. foundl) then
    call iotk_error_issue(ierrl,"iotk_scan_attr",__FILE__,__LINE__)
# 440 "iotk_attr.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.20 ")
# 440 "iotk_attr.spp"
call iotk_error_msg(ierrl,'Attribute not found')
# 440 "iotk_attr.spp"
call iotk_error_write(ierrl,"name",name)
    ierrl = - ierrl
  end if
  if(present(default) .and. .not. foundl) then
# 453 "iotk_attr.spp"
    val = default
# 455 "iotk_attr.spp"
  end if
  if(present(ierr)) then
    ierr = ierrl
  else
    if(ierrl/=0) call iotk_error_handler(ierrl)
  end if
end subroutine iotk_scan_attr_LOGICAL1_3
# 463 "iotk_attr.spp"

#endif
#endif

subroutine iotk_attr_dummy_LOGICAL1_3
  write(0,*)
end subroutine iotk_attr_dummy_LOGICAL1_3

# 45 "iotk_attr.spp"

# 65 "iotk_attr.spp"


!------------------------------------------------------------------------------!
! Inclusion of configuration file
#include "iotk_config.h"
!------------------------------------------------------------------------------!

# 74 "iotk_attr.spp"
#include "iotk_auxmacros.h"
# 76 "iotk_attr.spp"

# 78 "iotk_attr.spp"

#ifdef __IOTK_LOGICAL1
#if 4 <= __IOTK_MAXRANK

# 158 "iotk_attr.spp"

# 233 "iotk_attr.spp"

# 236 "iotk_attr.spp"
subroutine iotk_write_attr_LOGICAL1_4(attr,name,val,dummy,first,newline,ierr)
  use iotk_base
  use iotk_error_interf
  use iotk_attr_interf
  use iotk_str_interf
  use iotk_misc_interf
  implicit none
  character(*), intent(inout) :: attr
  character(*), intent(in)    :: name
  LOGICAL(kind=iotk_LOGICAL1), intent(in)  :: val (:,:,:,:)
  type(iotk_dummytype), optional :: dummy
  logical, optional, intent(in)  :: first
  logical, optional, intent(in)  :: newline
  integer, optional, intent(out) :: ierr
  integer :: ierrl
  integer :: attlen
  integer :: vallen
  integer :: namlen
  character :: delim
# 258 "iotk_attr.spp"
  character(iotk_vallenx) :: tmpval
  logical :: nl
  if(present(newline)) then
    nl = newline
  else
    nl = .false.
  endif
  ierrl = 0
  if(present(first)) then
    if(first) attr(1:1) = iotk_eos
  end if
  attlen = iotk_strlen_trim(attr)
  namlen = iotk_strlen_trim(name)
  if(.not.iotk_check_name(name)) then
    call iotk_error_issue(ierrl,"iotk_write_attr",__FILE__,__LINE__)
# 272 "iotk_attr.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.20 ")
# 272 "iotk_attr.spp"
call iotk_error_msg(ierrl,'Wrong tag name')
# 272 "iotk_attr.spp"
call iotk_error_write(ierrl,"name",name(1:namlen))
    goto 1
  end if
# 289 "iotk_attr.spp"
  delim = '"'
# 293 "iotk_attr.spp"
  call iotk_write(pack(val,mask=.true.),tmpval,ierrl)
# 295 "iotk_attr.spp"
  if(ierrl/=0) then
    call iotk_error_issue(ierrl,"iotk_write_attr",__FILE__,__LINE__)
# 296 "iotk_attr.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.20 ")
    goto 1
  end if
# 300 "iotk_attr.spp"
  vallen = iotk_strlen(tmpval)
  if(attlen+vallen+namlen+5>len(attr)) then
    call iotk_error_issue(ierrl,"iotk_write_attr",__FILE__,__LINE__)
# 302 "iotk_attr.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.20 ")
# 302 "iotk_attr.spp"
call iotk_error_msg(ierrl,'Attribute dummy argument is too short')
    goto 1
  end if
  if(.not. nl) then
    attr(attlen+1:attlen+vallen+namlen+5) = " "//name(1:namlen)//"="//delim//tmpval(1:vallen)//delim//iotk_eos
  else
    attr(attlen+1:attlen+vallen+namlen+len(iotk_newline)+5) &
       = iotk_newline//" "//name(1:namlen)//"="//delim//tmpval(1:vallen)//delim//iotk_eos
  endif
1 continue
  if(present(ierr)) then
    ierr = ierrl
  else
    if(ierrl/=0) call iotk_error_handler(ierrl)
  end if
end subroutine iotk_write_attr_LOGICAL1_4

# 320 "iotk_attr.spp"
subroutine iotk_scan_attr_LOGICAL1_4(attr,name,val,dummy,found,default,eos,ierr)
  use iotk_base
  use iotk_error_interf
  use iotk_attr_interf, only : iotk_read
  use iotk_str_interf
  use iotk_misc_interf
  implicit none
  character(*),             intent(in)  :: attr
  character(*),             intent(in)  :: name
#ifdef __IOTK_WORKAROUND6
  LOGICAL(kind=iotk_LOGICAL1)                        :: val (:,:,:,:)
#else
  LOGICAL(kind=iotk_LOGICAL1), intent(out)           :: val (:,:,:,:)
#endif
  type(iotk_dummytype), optional :: dummy
  logical,        optional, intent(out) :: found
  LOGICAL(kind=iotk_LOGICAL1), optional, intent(in)  :: default (:,:,:,:)
  logical,        optional, intent(in)  :: eos
  integer,        optional, intent(out) :: ierr
  integer :: ierrl
  integer :: attlen,pos,equal,namlen
  character :: delim
  logical :: foundl
  character(iotk_vallenx) :: valc
# 351 "iotk_attr.spp"
  integer :: index
  LOGICAL(kind=iotk_LOGICAL1), allocatable :: tmpval (:)
# 354 "iotk_attr.spp"
  ierrl = 0
  attlen=iotk_strlen(attr)
  namlen=iotk_strlen_trim(name)
  foundl = .false.
  equal = 0
  do
    pos = verify(attr(equal+1:attlen)," ")
    if(pos<=0) exit
    equal = equal + pos
    pos = scan(attr(equal+1:attlen),"=")
    if(pos<=0) then
      call iotk_error_issue(ierrl,"iotk_scan_attr",__FILE__,__LINE__)
# 365 "iotk_attr.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.20 ")
# 365 "iotk_attr.spp"
call iotk_error_msg(ierrl,'')
# 365 "iotk_attr.spp"
call iotk_error_write(ierrl,"attr",attr(equal+1:attlen))
      goto 1
    end if
    equal = equal + pos
    if(trim(attr(equal-pos:equal-1))==name(1:namlen)) foundl = .true.
    pos = verify(attr(equal+1:attlen)," ")
    if(pos<=0) then
      call iotk_error_issue(ierrl,"iotk_scan_attr",__FILE__,__LINE__)
# 372 "iotk_attr.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.20 ")
      goto 1
    end if
    equal = equal + pos
    delim = attr(equal:equal)
    if(delim/="'" .and. delim/='"') then
      call iotk_error_issue(ierrl,"iotk_scan_attr",__FILE__,__LINE__)
# 378 "iotk_attr.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.20 ")
      goto 1
    end if
    pos = scan(attr(equal+1:attlen),delim)
    if(pos<=0) then
      call iotk_error_issue(ierrl,"iotk_scan_attr",__FILE__,__LINE__)
# 383 "iotk_attr.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.20 ")
      goto 1
    end if
    if(foundl) exit
    equal = equal + pos
  end do
  if(foundl) then
    call iotk_strcpy(valc,attr(equal+1:equal+pos-1),ierrl)
    if(ierrl/=0) then
      call iotk_error_issue(ierrl,"iotk_scan_attr",__FILE__,__LINE__)
# 392 "iotk_attr.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.20 ")
      goto 1
    end if
  else
    goto 1
  end if
# 413 "iotk_attr.spp"
  allocate(tmpval(size(val)))
  index = 0
  call iotk_str_clean(valc)
  call iotk_read(tmpval,valc(1:iotk_strlen(valc)),index,ierrl)
  if(ierrl/=0) then
    call iotk_error_issue(ierrl,"iotk_scan_attr",__FILE__,__LINE__)
# 418 "iotk_attr.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.20 ")
    goto 1
  end if
# 424 "iotk_attr.spp"
  if(index/=size(val)) then
# 426 "iotk_attr.spp"
    call iotk_error_issue(ierrl,"iotk_scan_attr",__FILE__,__LINE__)
# 426 "iotk_attr.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.20 ")
# 426 "iotk_attr.spp"
call iotk_error_msg(ierrl,'Attribute size does not match')
# 426 "iotk_attr.spp"
call iotk_error_write(ierrl,"attr",valc)
# 426 "iotk_attr.spp"
call iotk_error_write(ierrl,"size",size(tmpval))
    goto 1
  end if
# 432 "iotk_attr.spp"
  val = reshape (source=tmpval,shape=shape(val))
# 434 "iotk_attr.spp"
  deallocate(tmpval)
# 436 "iotk_attr.spp"
1 continue
  if(ierrl/=0) foundl=.false.
  if(present(found)) found = foundl
  if(ierrl==0 .and. .not. present(found) .and. .not. present(default) .and. .not. foundl) then
    call iotk_error_issue(ierrl,"iotk_scan_attr",__FILE__,__LINE__)
# 440 "iotk_attr.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.20 ")
# 440 "iotk_attr.spp"
call iotk_error_msg(ierrl,'Attribute not found')
# 440 "iotk_attr.spp"
call iotk_error_write(ierrl,"name",name)
    ierrl = - ierrl
  end if
  if(present(default) .and. .not. foundl) then
# 453 "iotk_attr.spp"
    val = default
# 455 "iotk_attr.spp"
  end if
  if(present(ierr)) then
    ierr = ierrl
  else
    if(ierrl/=0) call iotk_error_handler(ierrl)
  end if
end subroutine iotk_scan_attr_LOGICAL1_4
# 463 "iotk_attr.spp"

#endif
#endif

subroutine iotk_attr_dummy_LOGICAL1_4
  write(0,*)
end subroutine iotk_attr_dummy_LOGICAL1_4

# 45 "iotk_attr.spp"

# 65 "iotk_attr.spp"


!------------------------------------------------------------------------------!
! Inclusion of configuration file
#include "iotk_config.h"
!------------------------------------------------------------------------------!

# 74 "iotk_attr.spp"
#include "iotk_auxmacros.h"
# 76 "iotk_attr.spp"

# 78 "iotk_attr.spp"

#ifdef __IOTK_LOGICAL1
#if 5 <= __IOTK_MAXRANK

# 158 "iotk_attr.spp"

# 233 "iotk_attr.spp"

# 236 "iotk_attr.spp"
subroutine iotk_write_attr_LOGICAL1_5(attr,name,val,dummy,first,newline,ierr)
  use iotk_base
  use iotk_error_interf
  use iotk_attr_interf
  use iotk_str_interf
  use iotk_misc_interf
  implicit none
  character(*), intent(inout) :: attr
  character(*), intent(in)    :: name
  LOGICAL(kind=iotk_LOGICAL1), intent(in)  :: val (:,:,:,:,:)
  type(iotk_dummytype), optional :: dummy
  logical, optional, intent(in)  :: first
  logical, optional, intent(in)  :: newline
  integer, optional, intent(out) :: ierr
  integer :: ierrl
  integer :: attlen
  integer :: vallen
  integer :: namlen
  character :: delim
# 258 "iotk_attr.spp"
  character(iotk_vallenx) :: tmpval
  logical :: nl
  if(present(newline)) then
    nl = newline
  else
    nl = .false.
  endif
  ierrl = 0
  if(present(first)) then
    if(first) attr(1:1) = iotk_eos
  end if
  attlen = iotk_strlen_trim(attr)
  namlen = iotk_strlen_trim(name)
  if(.not.iotk_check_name(name)) then
    call iotk_error_issue(ierrl,"iotk_write_attr",__FILE__,__LINE__)
# 272 "iotk_attr.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.20 ")
# 272 "iotk_attr.spp"
call iotk_error_msg(ierrl,'Wrong tag name')
# 272 "iotk_attr.spp"
call iotk_error_write(ierrl,"name",name(1:namlen))
    goto 1
  end if
# 289 "iotk_attr.spp"
  delim = '"'
# 293 "iotk_attr.spp"
  call iotk_write(pack(val,mask=.true.),tmpval,ierrl)
# 295 "iotk_attr.spp"
  if(ierrl/=0) then
    call iotk_error_issue(ierrl,"iotk_write_attr",__FILE__,__LINE__)
# 296 "iotk_attr.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.20 ")
    goto 1
  end if
# 300 "iotk_attr.spp"
  vallen = iotk_strlen(tmpval)
  if(attlen+vallen+namlen+5>len(attr)) then
    call iotk_error_issue(ierrl,"iotk_write_attr",__FILE__,__LINE__)
# 302 "iotk_attr.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.20 ")
# 302 "iotk_attr.spp"
call iotk_error_msg(ierrl,'Attribute dummy argument is too short')
    goto 1
  end if
  if(.not. nl) then
    attr(attlen+1:attlen+vallen+namlen+5) = " "//name(1:namlen)//"="//delim//tmpval(1:vallen)//delim//iotk_eos
  else
    attr(attlen+1:attlen+vallen+namlen+len(iotk_newline)+5) &
       = iotk_newline//" "//name(1:namlen)//"="//delim//tmpval(1:vallen)//delim//iotk_eos
  endif
1 continue
  if(present(ierr)) then
    ierr = ierrl
  else
    if(ierrl/=0) call iotk_error_handler(ierrl)
  end if
end subroutine iotk_write_attr_LOGICAL1_5

# 320 "iotk_attr.spp"
subroutine iotk_scan_attr_LOGICAL1_5(attr,name,val,dummy,found,default,eos,ierr)
  use iotk_base
  use iotk_error_interf
  use iotk_attr_interf, only : iotk_read
  use iotk_str_interf
  use iotk_misc_interf
  implicit none
  character(*),             intent(in)  :: attr
  character(*),             intent(in)  :: name
#ifdef __IOTK_WORKAROUND6
  LOGICAL(kind=iotk_LOGICAL1)                        :: val (:,:,:,:,:)
#else
  LOGICAL(kind=iotk_LOGICAL1), intent(out)           :: val (:,:,:,:,:)
#endif
  type(iotk_dummytype), optional :: dummy
  logical,        optional, intent(out) :: found
  LOGICAL(kind=iotk_LOGICAL1), optional, intent(in)  :: default (:,:,:,:,:)
  logical,        optional, intent(in)  :: eos
  integer,        optional, intent(out) :: ierr
  integer :: ierrl
  integer :: attlen,pos,equal,namlen
  character :: delim
  logical :: foundl
  character(iotk_vallenx) :: valc
# 351 "iotk_attr.spp"
  integer :: index
  LOGICAL(kind=iotk_LOGICAL1), allocatable :: tmpval (:)
# 354 "iotk_attr.spp"
  ierrl = 0
  attlen=iotk_strlen(attr)
  namlen=iotk_strlen_trim(name)
  foundl = .false.
  equal = 0
  do
    pos = verify(attr(equal+1:attlen)," ")
    if(pos<=0) exit
    equal = equal + pos
    pos = scan(attr(equal+1:attlen),"=")
    if(pos<=0) then
      call iotk_error_issue(ierrl,"iotk_scan_attr",__FILE__,__LINE__)
# 365 "iotk_attr.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.20 ")
# 365 "iotk_attr.spp"
call iotk_error_msg(ierrl,'')
# 365 "iotk_attr.spp"
call iotk_error_write(ierrl,"attr",attr(equal+1:attlen))
      goto 1
    end if
    equal = equal + pos
    if(trim(attr(equal-pos:equal-1))==name(1:namlen)) foundl = .true.
    pos = verify(attr(equal+1:attlen)," ")
    if(pos<=0) then
      call iotk_error_issue(ierrl,"iotk_scan_attr",__FILE__,__LINE__)
# 372 "iotk_attr.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.20 ")
      goto 1
    end if
    equal = equal + pos
    delim = attr(equal:equal)
    if(delim/="'" .and. delim/='"') then
      call iotk_error_issue(ierrl,"iotk_scan_attr",__FILE__,__LINE__)
# 378 "iotk_attr.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.20 ")
      goto 1
    end if
    pos = scan(attr(equal+1:attlen),delim)
    if(pos<=0) then
      call iotk_error_issue(ierrl,"iotk_scan_attr",__FILE__,__LINE__)
# 383 "iotk_attr.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.20 ")
      goto 1
    end if
    if(foundl) exit
    equal = equal + pos
  end do
  if(foundl) then
    call iotk_strcpy(valc,attr(equal+1:equal+pos-1),ierrl)
    if(ierrl/=0) then
      call iotk_error_issue(ierrl,"iotk_scan_attr",__FILE__,__LINE__)
# 392 "iotk_attr.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.20 ")
      goto 1
    end if
  else
    goto 1
  end if
# 413 "iotk_attr.spp"
  allocate(tmpval(size(val)))
  index = 0
  call iotk_str_clean(valc)
  call iotk_read(tmpval,valc(1:iotk_strlen(valc)),index,ierrl)
  if(ierrl/=0) then
    call iotk_error_issue(ierrl,"iotk_scan_attr",__FILE__,__LINE__)
# 418 "iotk_attr.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.20 ")
    goto 1
  end if
# 424 "iotk_attr.spp"
  if(index/=size(val)) then
# 426 "iotk_attr.spp"
    call iotk_error_issue(ierrl,"iotk_scan_attr",__FILE__,__LINE__)
# 426 "iotk_attr.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.20 ")
# 426 "iotk_attr.spp"
call iotk_error_msg(ierrl,'Attribute size does not match')
# 426 "iotk_attr.spp"
call iotk_error_write(ierrl,"attr",valc)
# 426 "iotk_attr.spp"
call iotk_error_write(ierrl,"size",size(tmpval))
    goto 1
  end if
# 432 "iotk_attr.spp"
  val = reshape (source=tmpval,shape=shape(val))
# 434 "iotk_attr.spp"
  deallocate(tmpval)
# 436 "iotk_attr.spp"
1 continue
  if(ierrl/=0) foundl=.false.
  if(present(found)) found = foundl
  if(ierrl==0 .and. .not. present(found) .and. .not. present(default) .and. .not. foundl) then
    call iotk_error_issue(ierrl,"iotk_scan_attr",__FILE__,__LINE__)
# 440 "iotk_attr.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.20 ")
# 440 "iotk_attr.spp"
call iotk_error_msg(ierrl,'Attribute not found')
# 440 "iotk_attr.spp"
call iotk_error_write(ierrl,"name",name)
    ierrl = - ierrl
  end if
  if(present(default) .and. .not. foundl) then
# 453 "iotk_attr.spp"
    val = default
# 455 "iotk_attr.spp"
  end if
  if(present(ierr)) then
    ierr = ierrl
  else
    if(ierrl/=0) call iotk_error_handler(ierrl)
  end if
end subroutine iotk_scan_attr_LOGICAL1_5
# 463 "iotk_attr.spp"

#endif
#endif

subroutine iotk_attr_dummy_LOGICAL1_5
  write(0,*)
end subroutine iotk_attr_dummy_LOGICAL1_5

# 45 "iotk_attr.spp"

# 48 "iotk_attr.spp"
! Input/Output Tool Kit (IOTK)
! Copyright (C) 2004-2006 Giovanni Bussi
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
# 65 "iotk_attr.spp"


!------------------------------------------------------------------------------!
! Inclusion of configuration file
#include "iotk_config.h"
!------------------------------------------------------------------------------!

# 74 "iotk_attr.spp"
#include "iotk_auxmacros.h"
# 76 "iotk_attr.spp"

# 78 "iotk_attr.spp"

#ifdef __IOTK_LOGICAL1
#if 6 <= __IOTK_MAXRANK

# 158 "iotk_attr.spp"

# 233 "iotk_attr.spp"

# 236 "iotk_attr.spp"
subroutine iotk_write_attr_LOGICAL1_6(attr,name,val,dummy,first,newline,ierr)
  use iotk_base
  use iotk_error_interf
  use iotk_attr_interf
  use iotk_str_interf
  use iotk_misc_interf
  implicit none
  character(*), intent(inout) :: attr
  character(*), intent(in)    :: name
  LOGICAL(kind=iotk_LOGICAL1), intent(in)  :: val (:,:,:,:,:,:)
  type(iotk_dummytype), optional :: dummy
  logical, optional, intent(in)  :: first
  logical, optional, intent(in)  :: newline
  integer, optional, intent(out) :: ierr
  integer :: ierrl
  integer :: attlen
  integer :: vallen
  integer :: namlen
  character :: delim
# 258 "iotk_attr.spp"
  character(iotk_vallenx) :: tmpval
  logical :: nl
  if(present(newline)) then
    nl = newline
  else
    nl = .false.
  endif
  ierrl = 0
  if(present(first)) then
    if(first) attr(1:1) = iotk_eos
  end if
  attlen = iotk_strlen_trim(attr)
  namlen = iotk_strlen_trim(name)
  if(.not.iotk_check_name(name)) then
    call iotk_error_issue(ierrl,"iotk_write_attr",__FILE__,__LINE__)
# 272 "iotk_attr.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.20 ")
# 272 "iotk_attr.spp"
call iotk_error_msg(ierrl,'Wrong tag name')
# 272 "iotk_attr.spp"
call iotk_error_write(ierrl,"name",name(1:namlen))
    goto 1
  end if
# 289 "iotk_attr.spp"
  delim = '"'
# 293 "iotk_attr.spp"
  call iotk_write(pack(val,mask=.true.),tmpval,ierrl)
# 295 "iotk_attr.spp"
  if(ierrl/=0) then
    call iotk_error_issue(ierrl,"iotk_write_attr",__FILE__,__LINE__)
# 296 "iotk_attr.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.20 ")
    goto 1
  end if
# 300 "iotk_attr.spp"
  vallen = iotk_strlen(tmpval)
  if(attlen+vallen+namlen+5>len(attr)) then
    call iotk_error_issue(ierrl,"iotk_write_attr",__FILE__,__LINE__)
# 302 "iotk_attr.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.20 ")
# 302 "iotk_attr.spp"
call iotk_error_msg(ierrl,'Attribute dummy argument is too short')
    goto 1
  end if
  if(.not. nl) then
    attr(attlen+1:attlen+vallen+namlen+5) = " "//name(1:namlen)//"="//delim//tmpval(1:vallen)//delim//iotk_eos
  else
    attr(attlen+1:attlen+vallen+namlen+len(iotk_newline)+5) &
       = iotk_newline//" "//name(1:namlen)//"="//delim//tmpval(1:vallen)//delim//iotk_eos
  endif
1 continue
  if(present(ierr)) then
    ierr = ierrl
  else
    if(ierrl/=0) call iotk_error_handler(ierrl)
  end if
end subroutine iotk_write_attr_LOGICAL1_6

# 320 "iotk_attr.spp"
subroutine iotk_scan_attr_LOGICAL1_6(attr,name,val,dummy,found,default,eos,ierr)
  use iotk_base
  use iotk_error_interf
  use iotk_attr_interf, only : iotk_read
  use iotk_str_interf
  use iotk_misc_interf
  implicit none
  character(*),             intent(in)  :: attr
  character(*),             intent(in)  :: name
#ifdef __IOTK_WORKAROUND6
  LOGICAL(kind=iotk_LOGICAL1)                        :: val (:,:,:,:,:,:)
#else
  LOGICAL(kind=iotk_LOGICAL1), intent(out)           :: val (:,:,:,:,:,:)
#endif
  type(iotk_dummytype), optional :: dummy
  logical,        optional, intent(out) :: found
  LOGICAL(kind=iotk_LOGICAL1), optional, intent(in)  :: default (:,:,:,:,:,:)
  logical,        optional, intent(in)  :: eos
  integer,        optional, intent(out) :: ierr
  integer :: ierrl
  integer :: attlen,pos,equal,namlen
  character :: delim
  logical :: foundl
  character(iotk_vallenx) :: valc
# 351 "iotk_attr.spp"
  integer :: index
  LOGICAL(kind=iotk_LOGICAL1), allocatable :: tmpval (:)
# 354 "iotk_attr.spp"
  ierrl = 0
  attlen=iotk_strlen(attr)
  namlen=iotk_strlen_trim(name)
  foundl = .false.
  equal = 0
  do
    pos = verify(attr(equal+1:attlen)," ")
    if(pos<=0) exit
    equal = equal + pos
    pos = scan(attr(equal+1:attlen),"=")
    if(pos<=0) then
      call iotk_error_issue(ierrl,"iotk_scan_attr",__FILE__,__LINE__)
# 365 "iotk_attr.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.20 ")
# 365 "iotk_attr.spp"
call iotk_error_msg(ierrl,'')
# 365 "iotk_attr.spp"
call iotk_error_write(ierrl,"attr",attr(equal+1:attlen))
      goto 1
    end if
    equal = equal + pos
    if(trim(attr(equal-pos:equal-1))==name(1:namlen)) foundl = .true.
    pos = verify(attr(equal+1:attlen)," ")
    if(pos<=0) then
      call iotk_error_issue(ierrl,"iotk_scan_attr",__FILE__,__LINE__)
# 372 "iotk_attr.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.20 ")
      goto 1
    end if
    equal = equal + pos
    delim = attr(equal:equal)
    if(delim/="'" .and. delim/='"') then
      call iotk_error_issue(ierrl,"iotk_scan_attr",__FILE__,__LINE__)
# 378 "iotk_attr.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.20 ")
      goto 1
    end if
    pos = scan(attr(equal+1:attlen),delim)
    if(pos<=0) then
      call iotk_error_issue(ierrl,"iotk_scan_attr",__FILE__,__LINE__)
# 383 "iotk_attr.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.20 ")
      goto 1
    end if
    if(foundl) exit
    equal = equal + pos
  end do
  if(foundl) then
    call iotk_strcpy(valc,attr(equal+1:equal+pos-1),ierrl)
    if(ierrl/=0) then
      call iotk_error_issue(ierrl,"iotk_scan_attr",__FILE__,__LINE__)
# 392 "iotk_attr.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.20 ")
      goto 1
    end if
  else
    goto 1
  end if
# 413 "iotk_attr.spp"
  allocate(tmpval(size(val)))
  index = 0
  call iotk_str_clean(valc)
  call iotk_read(tmpval,valc(1:iotk_strlen(valc)),index,ierrl)
  if(ierrl/=0) then
    call iotk_error_issue(ierrl,"iotk_scan_attr",__FILE__,__LINE__)
# 418 "iotk_attr.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.20 ")
    goto 1
  end if
# 424 "iotk_attr.spp"
  if(index/=size(val)) then
# 426 "iotk_attr.spp"
    call iotk_error_issue(ierrl,"iotk_scan_attr",__FILE__,__LINE__)
# 426 "iotk_attr.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.20 ")
# 426 "iotk_attr.spp"
call iotk_error_msg(ierrl,'Attribute size does not match')
# 426 "iotk_attr.spp"
call iotk_error_write(ierrl,"attr",valc)
# 426 "iotk_attr.spp"
call iotk_error_write(ierrl,"size",size(tmpval))
    goto 1
  end if
# 432 "iotk_attr.spp"
  val = reshape (source=tmpval,shape=shape(val))
# 434 "iotk_attr.spp"
  deallocate(tmpval)
# 436 "iotk_attr.spp"
1 continue
  if(ierrl/=0) foundl=.false.
  if(present(found)) found = foundl
  if(ierrl==0 .and. .not. present(found) .and. .not. present(default) .and. .not. foundl) then
    call iotk_error_issue(ierrl,"iotk_scan_attr",__FILE__,__LINE__)
# 440 "iotk_attr.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.20 ")
# 440 "iotk_attr.spp"
call iotk_error_msg(ierrl,'Attribute not found')
# 440 "iotk_attr.spp"
call iotk_error_write(ierrl,"name",name)
    ierrl = - ierrl
  end if
  if(present(default) .and. .not. foundl) then
# 453 "iotk_attr.spp"
    val = default
# 455 "iotk_attr.spp"
  end if
  if(present(ierr)) then
    ierr = ierrl
  else
    if(ierrl/=0) call iotk_error_handler(ierrl)
  end if
end subroutine iotk_scan_attr_LOGICAL1_6
# 463 "iotk_attr.spp"

#endif
#endif

subroutine iotk_attr_dummy_LOGICAL1_6
  write(0,*)
end subroutine iotk_attr_dummy_LOGICAL1_6

# 45 "iotk_attr.spp"

# 65 "iotk_attr.spp"


!------------------------------------------------------------------------------!
! Inclusion of configuration file
#include "iotk_config.h"
!------------------------------------------------------------------------------!

# 74 "iotk_attr.spp"
#include "iotk_auxmacros.h"
# 76 "iotk_attr.spp"

# 78 "iotk_attr.spp"

#ifdef __IOTK_LOGICAL1
#if 7 <= __IOTK_MAXRANK

# 158 "iotk_attr.spp"

# 233 "iotk_attr.spp"

# 236 "iotk_attr.spp"
subroutine iotk_write_attr_LOGICAL1_7(attr,name,val,dummy,first,newline,ierr)
  use iotk_base
  use iotk_error_interf
  use iotk_attr_interf
  use iotk_str_interf
  use iotk_misc_interf
  implicit none
  character(*), intent(inout) :: attr
  character(*), intent(in)    :: name
  LOGICAL(kind=iotk_LOGICAL1), intent(in)  :: val (:,:,:,:,:,:,:)
  type(iotk_dummytype), optional :: dummy
  logical, optional, intent(in)  :: first
  logical, optional, intent(in)  :: newline
  integer, optional, intent(out) :: ierr
  integer :: ierrl
  integer :: attlen
  integer :: vallen
  integer :: namlen
  character :: delim
# 258 "iotk_attr.spp"
  character(iotk_vallenx) :: tmpval
  logical :: nl
  if(present(newline)) then
    nl = newline
  else
    nl = .false.
  endif
  ierrl = 0
  if(present(first)) then
    if(first) attr(1:1) = iotk_eos
  end if
  attlen = iotk_strlen_trim(attr)
  namlen = iotk_strlen_trim(name)
  if(.not.iotk_check_name(name)) then
    call iotk_error_issue(ierrl,"iotk_write_attr",__FILE__,__LINE__)
# 272 "iotk_attr.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.20 ")
# 272 "iotk_attr.spp"
call iotk_error_msg(ierrl,'Wrong tag name')
# 272 "iotk_attr.spp"
call iotk_error_write(ierrl,"name",name(1:namlen))
    goto 1
  end if
# 289 "iotk_attr.spp"
  delim = '"'
# 293 "iotk_attr.spp"
  call iotk_write(pack(val,mask=.true.),tmpval,ierrl)
# 295 "iotk_attr.spp"
  if(ierrl/=0) then
    call iotk_error_issue(ierrl,"iotk_write_attr",__FILE__,__LINE__)
# 296 "iotk_attr.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.20 ")
    goto 1
  end if
# 300 "iotk_attr.spp"
  vallen = iotk_strlen(tmpval)
  if(attlen+vallen+namlen+5>len(attr)) then
    call iotk_error_issue(ierrl,"iotk_write_attr",__FILE__,__LINE__)
# 302 "iotk_attr.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.20 ")
# 302 "iotk_attr.spp"
call iotk_error_msg(ierrl,'Attribute dummy argument is too short')
    goto 1
  end if
  if(.not. nl) then
    attr(attlen+1:attlen+vallen+namlen+5) = " "//name(1:namlen)//"="//delim//tmpval(1:vallen)//delim//iotk_eos
  else
    attr(attlen+1:attlen+vallen+namlen+len(iotk_newline)+5) &
       = iotk_newline//" "//name(1:namlen)//"="//delim//tmpval(1:vallen)//delim//iotk_eos
  endif
1 continue
  if(present(ierr)) then
    ierr = ierrl
  else
    if(ierrl/=0) call iotk_error_handler(ierrl)
  end if
end subroutine iotk_write_attr_LOGICAL1_7

# 320 "iotk_attr.spp"
subroutine iotk_scan_attr_LOGICAL1_7(attr,name,val,dummy,found,default,eos,ierr)
  use iotk_base
  use iotk_error_interf
  use iotk_attr_interf, only : iotk_read
  use iotk_str_interf
  use iotk_misc_interf
  implicit none
  character(*),             intent(in)  :: attr
  character(*),             intent(in)  :: name
#ifdef __IOTK_WORKAROUND6
  LOGICAL(kind=iotk_LOGICAL1)                        :: val (:,:,:,:,:,:,:)
#else
  LOGICAL(kind=iotk_LOGICAL1), intent(out)           :: val (:,:,:,:,:,:,:)
#endif
  type(iotk_dummytype), optional :: dummy
  logical,        optional, intent(out) :: found
  LOGICAL(kind=iotk_LOGICAL1), optional, intent(in)  :: default (:,:,:,:,:,:,:)
  logical,        optional, intent(in)  :: eos
  integer,        optional, intent(out) :: ierr
  integer :: ierrl
  integer :: attlen,pos,equal,namlen
  character :: delim
  logical :: foundl
  character(iotk_vallenx) :: valc
# 351 "iotk_attr.spp"
  integer :: index
  LOGICAL(kind=iotk_LOGICAL1), allocatable :: tmpval (:)
# 354 "iotk_attr.spp"
  ierrl = 0
  attlen=iotk_strlen(attr)
  namlen=iotk_strlen_trim(name)
  foundl = .false.
  equal = 0
  do
    pos = verify(attr(equal+1:attlen)," ")
    if(pos<=0) exit
    equal = equal + pos
    pos = scan(attr(equal+1:attlen),"=")
    if(pos<=0) then
      call iotk_error_issue(ierrl,"iotk_scan_attr",__FILE__,__LINE__)
# 365 "iotk_attr.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.20 ")
# 365 "iotk_attr.spp"
call iotk_error_msg(ierrl,'')
# 365 "iotk_attr.spp"
call iotk_error_write(ierrl,"attr",attr(equal+1:attlen))
      goto 1
    end if
    equal = equal + pos
    if(trim(attr(equal-pos:equal-1))==name(1:namlen)) foundl = .true.
    pos = verify(attr(equal+1:attlen)," ")
    if(pos<=0) then
      call iotk_error_issue(ierrl,"iotk_scan_attr",__FILE__,__LINE__)
# 372 "iotk_attr.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.20 ")
      goto 1
    end if
    equal = equal + pos
    delim = attr(equal:equal)
    if(delim/="'" .and. delim/='"') then
      call iotk_error_issue(ierrl,"iotk_scan_attr",__FILE__,__LINE__)
# 378 "iotk_attr.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.20 ")
      goto 1
    end if
    pos = scan(attr(equal+1:attlen),delim)
    if(pos<=0) then
      call iotk_error_issue(ierrl,"iotk_scan_attr",__FILE__,__LINE__)
# 383 "iotk_attr.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.20 ")
      goto 1
    end if
    if(foundl) exit
    equal = equal + pos
  end do
  if(foundl) then
    call iotk_strcpy(valc,attr(equal+1:equal+pos-1),ierrl)
    if(ierrl/=0) then
      call iotk_error_issue(ierrl,"iotk_scan_attr",__FILE__,__LINE__)
# 392 "iotk_attr.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.20 ")
      goto 1
    end if
  else
    goto 1
  end if
# 413 "iotk_attr.spp"
  allocate(tmpval(size(val)))
  index = 0
  call iotk_str_clean(valc)
  call iotk_read(tmpval,valc(1:iotk_strlen(valc)),index,ierrl)
  if(ierrl/=0) then
    call iotk_error_issue(ierrl,"iotk_scan_attr",__FILE__,__LINE__)
# 418 "iotk_attr.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.20 ")
    goto 1
  end if
# 424 "iotk_attr.spp"
  if(index/=size(val)) then
# 426 "iotk_attr.spp"
    call iotk_error_issue(ierrl,"iotk_scan_attr",__FILE__,__LINE__)
# 426 "iotk_attr.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.20 ")
# 426 "iotk_attr.spp"
call iotk_error_msg(ierrl,'Attribute size does not match')
# 426 "iotk_attr.spp"
call iotk_error_write(ierrl,"attr",valc)
# 426 "iotk_attr.spp"
call iotk_error_write(ierrl,"size",size(tmpval))
    goto 1
  end if
# 432 "iotk_attr.spp"
  val = reshape (source=tmpval,shape=shape(val))
# 434 "iotk_attr.spp"
  deallocate(tmpval)
# 436 "iotk_attr.spp"
1 continue
  if(ierrl/=0) foundl=.false.
  if(present(found)) found = foundl
  if(ierrl==0 .and. .not. present(found) .and. .not. present(default) .and. .not. foundl) then
    call iotk_error_issue(ierrl,"iotk_scan_attr",__FILE__,__LINE__)
# 440 "iotk_attr.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.20 ")
# 440 "iotk_attr.spp"
call iotk_error_msg(ierrl,'Attribute not found')
# 440 "iotk_attr.spp"
call iotk_error_write(ierrl,"name",name)
    ierrl = - ierrl
  end if
  if(present(default) .and. .not. foundl) then
# 453 "iotk_attr.spp"
    val = default
# 455 "iotk_attr.spp"
  end if
  if(present(ierr)) then
    ierr = ierrl
  else
    if(ierrl/=0) call iotk_error_handler(ierrl)
  end if
end subroutine iotk_scan_attr_LOGICAL1_7
# 463 "iotk_attr.spp"

#endif
#endif

subroutine iotk_attr_dummy_LOGICAL1_7
  write(0,*)
end subroutine iotk_attr_dummy_LOGICAL1_7

# 45 "iotk_attr.spp"

# 48 "iotk_attr.spp"
! Input/Output Tool Kit (IOTK)
! Copyright (C) 2004-2006 Giovanni Bussi
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
# 65 "iotk_attr.spp"


!------------------------------------------------------------------------------!
! Inclusion of configuration file
#include "iotk_config.h"
!------------------------------------------------------------------------------!

# 74 "iotk_attr.spp"
#include "iotk_auxmacros.h"
# 76 "iotk_attr.spp"

# 78 "iotk_attr.spp"

#ifdef __IOTK_LOGICAL2
#if 0 <= __IOTK_MAXRANK

# 83 "iotk_attr.spp"
! This is needed as a workaround for bugged pack 
subroutine iotk_private_pack_LOGICAL2(out,in,n,l)
    use iotk_base
    implicit none
    integer,                                    intent(in)  :: n,l
# 92 "iotk_attr.spp"
    LOGICAL (kind=iotk_LOGICAL2), intent(out) :: out(n)
    LOGICAL (kind=iotk_LOGICAL2), intent(in)  :: in(n)
# 95 "iotk_attr.spp"
    out = in
end subroutine iotk_private_pack_LOGICAL2

# 100 "iotk_attr.spp"
subroutine iotk_write_LOGICAL2(val,string,ierr)
  use iotk_base
  use iotk_error_interf
  use iotk_xtox_interf
  use iotk_fmt_interf
  use iotk_str_interf
  use iotk_misc_interf
  implicit none
  LOGICAL(kind=iotk_LOGICAL2), intent(in) :: val(:)
#ifdef __IOTK_WORKAROUND6
  character(len=*)              :: string
#else
  character(len=*), intent(out) :: string
#endif
  integer, intent(out) :: ierr
# 118 "iotk_attr.spp"
  integer :: index,iostat
  ierr = 0
  iostat = 0 
  string(1:1) = iotk_eos
  if(size(val)==0) return
  if(len(string)==0) then
    call iotk_error_issue(ierr,"iotk_write",__FILE__,__LINE__)
# 124 "iotk_attr.spp"
call iotk_error_msg(ierr,"CVS Revision: 1.20 ")
    return
  end if
  do index=1,size(val)
# 129 "iotk_attr.spp"
    call iotk_strcat(string,iotk_ltoa(val(index))//" ",ierr)
    if(ierr/=0) then
      call iotk_error_issue(ierr,"iotk_write",__FILE__,__LINE__)
# 131 "iotk_attr.spp"
call iotk_error_msg(ierr,"CVS Revision: 1.20 ")
      return
    end if
# 152 "iotk_attr.spp"
  end do
! the last blank is deleted
  string(iotk_strlen(string):iotk_strlen(string)) = iotk_eos
end subroutine iotk_write_LOGICAL2
# 158 "iotk_attr.spp"

# 162 "iotk_attr.spp"
subroutine iotk_read_LOGICAL2(val,string,index,ierr)
  use iotk_base
  use iotk_error_interf
  use iotk_xtox_interf
  use iotk_misc_interf
  implicit none
  LOGICAL(kind=iotk_LOGICAL2), intent(inout) :: val(:)
  character(len=*), intent(in) :: string
  integer, intent(inout) :: index
  integer, intent(out) :: ierr
# 173 "iotk_attr.spp"
  logical :: check
# 175 "iotk_attr.spp"
  integer :: pos,pos1,iostat
  integer :: maxindex
# 181 "iotk_attr.spp"
  pos = 0
  pos1= 0
  ierr = 0
  iostat = 0
# 188 "iotk_attr.spp"
    maxindex = size(val)
# 190 "iotk_attr.spp"
! for the moment, commas are considered as blanks
  do
    pos = verify(string(pos1+1:)," ,")+pos1
    if(pos==pos1) exit
    pos = pos - 1
    pos1 = scan(string(pos+1:)," ,")+pos
    if(pos1==pos) pos1 = len(string) + 1
!READ string(pos+1:pos1-1)
    index = index+1
    if(index>maxindex) then
      call iotk_error_issue(ierr,"iotk_read",__FILE__,__LINE__)
# 200 "iotk_attr.spp"
call iotk_error_msg(ierr,"CVS Revision: 1.20 ")
# 200 "iotk_attr.spp"
call iotk_error_msg(ierr,'Too many data')
    end if
# 205 "iotk_attr.spp"
    val(index) = iotk_atol(string(pos+1:pos1-1),check=check)
# 218 "iotk_attr.spp"
    if(.not.check) then
      call iotk_error_issue(ierr,"iotk_read",__FILE__,__LINE__)
# 219 "iotk_attr.spp"
call iotk_error_msg(ierr,"CVS Revision: 1.20 ")
# 219 "iotk_attr.spp"
call iotk_error_msg(ierr,'Wrong string')
# 219 "iotk_attr.spp"
call iotk_error_write(ierr,"string",string(pos+1:pos1-1))
      return
    end if
# 228 "iotk_attr.spp"
    if(pos1>=len(string)) exit
  end do
end subroutine iotk_read_LOGICAL2
# 233 "iotk_attr.spp"

# 236 "iotk_attr.spp"
subroutine iotk_write_attr_LOGICAL2_0(attr,name,val,dummy,first,newline,ierr)
  use iotk_base
  use iotk_error_interf
  use iotk_attr_interf
  use iotk_str_interf
  use iotk_misc_interf
  implicit none
  character(*), intent(inout) :: attr
  character(*), intent(in)    :: name
  LOGICAL(kind=iotk_LOGICAL2), intent(in)  :: val 
  type(iotk_dummytype), optional :: dummy
  logical, optional, intent(in)  :: first
  logical, optional, intent(in)  :: newline
  integer, optional, intent(out) :: ierr
  integer :: ierrl
  integer :: attlen
  integer :: vallen
  integer :: namlen
  character :: delim
# 258 "iotk_attr.spp"
  character(iotk_vallenx) :: tmpval
  logical :: nl
  if(present(newline)) then
    nl = newline
  else
    nl = .false.
  endif
  ierrl = 0
  if(present(first)) then
    if(first) attr(1:1) = iotk_eos
  end if
  attlen = iotk_strlen_trim(attr)
  namlen = iotk_strlen_trim(name)
  if(.not.iotk_check_name(name)) then
    call iotk_error_issue(ierrl,"iotk_write_attr",__FILE__,__LINE__)
# 272 "iotk_attr.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.20 ")
# 272 "iotk_attr.spp"
call iotk_error_msg(ierrl,'Wrong tag name')
# 272 "iotk_attr.spp"
call iotk_error_write(ierrl,"name",name(1:namlen))
    goto 1
  end if
# 289 "iotk_attr.spp"
  delim = '"'
# 291 "iotk_attr.spp"
  call iotk_write((/val/),tmpval,ierrl)
# 295 "iotk_attr.spp"
  if(ierrl/=0) then
    call iotk_error_issue(ierrl,"iotk_write_attr",__FILE__,__LINE__)
# 296 "iotk_attr.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.20 ")
    goto 1
  end if
# 300 "iotk_attr.spp"
  vallen = iotk_strlen(tmpval)
  if(attlen+vallen+namlen+5>len(attr)) then
    call iotk_error_issue(ierrl,"iotk_write_attr",__FILE__,__LINE__)
# 302 "iotk_attr.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.20 ")
# 302 "iotk_attr.spp"
call iotk_error_msg(ierrl,'Attribute dummy argument is too short')
    goto 1
  end if
  if(.not. nl) then
    attr(attlen+1:attlen+vallen+namlen+5) = " "//name(1:namlen)//"="//delim//tmpval(1:vallen)//delim//iotk_eos
  else
    attr(attlen+1:attlen+vallen+namlen+len(iotk_newline)+5) &
       = iotk_newline//" "//name(1:namlen)//"="//delim//tmpval(1:vallen)//delim//iotk_eos
  endif
1 continue
  if(present(ierr)) then
    ierr = ierrl
  else
    if(ierrl/=0) call iotk_error_handler(ierrl)
  end if
end subroutine iotk_write_attr_LOGICAL2_0

# 320 "iotk_attr.spp"
subroutine iotk_scan_attr_LOGICAL2_0(attr,name,val,dummy,found,default,eos,ierr)
  use iotk_base
  use iotk_error_interf
  use iotk_attr_interf, only : iotk_read
  use iotk_str_interf
  use iotk_misc_interf
  implicit none
  character(*),             intent(in)  :: attr
  character(*),             intent(in)  :: name
#ifdef __IOTK_WORKAROUND6
  LOGICAL(kind=iotk_LOGICAL2)                        :: val 
#else
  LOGICAL(kind=iotk_LOGICAL2), intent(out)           :: val 
#endif
  type(iotk_dummytype), optional :: dummy
  logical,        optional, intent(out) :: found
  LOGICAL(kind=iotk_LOGICAL2), optional, intent(in)  :: default 
  logical,        optional, intent(in)  :: eos
  integer,        optional, intent(out) :: ierr
  integer :: ierrl
  integer :: attlen,pos,equal,namlen
  character :: delim
  logical :: foundl
  character(iotk_vallenx) :: valc
# 351 "iotk_attr.spp"
  integer :: index
  LOGICAL(kind=iotk_LOGICAL2), allocatable :: tmpval (:)
# 354 "iotk_attr.spp"
  ierrl = 0
  attlen=iotk_strlen(attr)
  namlen=iotk_strlen_trim(name)
  foundl = .false.
  equal = 0
  do
    pos = verify(attr(equal+1:attlen)," ")
    if(pos<=0) exit
    equal = equal + pos
    pos = scan(attr(equal+1:attlen),"=")
    if(pos<=0) then
      call iotk_error_issue(ierrl,"iotk_scan_attr",__FILE__,__LINE__)
# 365 "iotk_attr.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.20 ")
# 365 "iotk_attr.spp"
call iotk_error_msg(ierrl,'')
# 365 "iotk_attr.spp"
call iotk_error_write(ierrl,"attr",attr(equal+1:attlen))
      goto 1
    end if
    equal = equal + pos
    if(trim(attr(equal-pos:equal-1))==name(1:namlen)) foundl = .true.
    pos = verify(attr(equal+1:attlen)," ")
    if(pos<=0) then
      call iotk_error_issue(ierrl,"iotk_scan_attr",__FILE__,__LINE__)
# 372 "iotk_attr.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.20 ")
      goto 1
    end if
    equal = equal + pos
    delim = attr(equal:equal)
    if(delim/="'" .and. delim/='"') then
      call iotk_error_issue(ierrl,"iotk_scan_attr",__FILE__,__LINE__)
# 378 "iotk_attr.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.20 ")
      goto 1
    end if
    pos = scan(attr(equal+1:attlen),delim)
    if(pos<=0) then
      call iotk_error_issue(ierrl,"iotk_scan_attr",__FILE__,__LINE__)
# 383 "iotk_attr.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.20 ")
      goto 1
    end if
    if(foundl) exit
    equal = equal + pos
  end do
  if(foundl) then
    call iotk_strcpy(valc,attr(equal+1:equal+pos-1),ierrl)
    if(ierrl/=0) then
      call iotk_error_issue(ierrl,"iotk_scan_attr",__FILE__,__LINE__)
# 392 "iotk_attr.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.20 ")
      goto 1
    end if
  else
    goto 1
  end if
# 413 "iotk_attr.spp"
  allocate(tmpval(1))
  index = 0
  call iotk_str_clean(valc)
  call iotk_read(tmpval,valc(1:iotk_strlen(valc)),index,ierrl)
  if(ierrl/=0) then
    call iotk_error_issue(ierrl,"iotk_scan_attr",__FILE__,__LINE__)
# 418 "iotk_attr.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.20 ")
    goto 1
  end if
# 424 "iotk_attr.spp"
  if(index/=1) then
# 426 "iotk_attr.spp"
    call iotk_error_issue(ierrl,"iotk_scan_attr",__FILE__,__LINE__)
# 426 "iotk_attr.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.20 ")
# 426 "iotk_attr.spp"
call iotk_error_msg(ierrl,'Attribute size does not match')
# 426 "iotk_attr.spp"
call iotk_error_write(ierrl,"attr",valc)
# 426 "iotk_attr.spp"
call iotk_error_write(ierrl,"size",size(tmpval))
    goto 1
  end if
# 430 "iotk_attr.spp"
  val = tmpval(1)
# 434 "iotk_attr.spp"
  deallocate(tmpval)
# 436 "iotk_attr.spp"
1 continue
  if(ierrl/=0) foundl=.false.
  if(present(found)) found = foundl
  if(ierrl==0 .and. .not. present(found) .and. .not. present(default) .and. .not. foundl) then
    call iotk_error_issue(ierrl,"iotk_scan_attr",__FILE__,__LINE__)
# 440 "iotk_attr.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.20 ")
# 440 "iotk_attr.spp"
call iotk_error_msg(ierrl,'Attribute not found')
# 440 "iotk_attr.spp"
call iotk_error_write(ierrl,"name",name)
    ierrl = - ierrl
  end if
  if(present(default) .and. .not. foundl) then
# 453 "iotk_attr.spp"
    val = default
# 455 "iotk_attr.spp"
  end if
  if(present(ierr)) then
    ierr = ierrl
  else
    if(ierrl/=0) call iotk_error_handler(ierrl)
  end if
end subroutine iotk_scan_attr_LOGICAL2_0
# 463 "iotk_attr.spp"

#endif
#endif

subroutine iotk_attr_dummy_LOGICAL2_0
  write(0,*)
end subroutine iotk_attr_dummy_LOGICAL2_0

# 45 "iotk_attr.spp"

# 65 "iotk_attr.spp"


!------------------------------------------------------------------------------!
! Inclusion of configuration file
#include "iotk_config.h"
!------------------------------------------------------------------------------!

# 74 "iotk_attr.spp"
#include "iotk_auxmacros.h"
# 76 "iotk_attr.spp"

# 78 "iotk_attr.spp"

#ifdef __IOTK_LOGICAL2
#if 1 <= __IOTK_MAXRANK

# 158 "iotk_attr.spp"

# 233 "iotk_attr.spp"

# 236 "iotk_attr.spp"
subroutine iotk_write_attr_LOGICAL2_1(attr,name,val,dummy,first,newline,ierr)
  use iotk_base
  use iotk_error_interf
  use iotk_attr_interf
  use iotk_str_interf
  use iotk_misc_interf
  implicit none
  character(*), intent(inout) :: attr
  character(*), intent(in)    :: name
  LOGICAL(kind=iotk_LOGICAL2), intent(in)  :: val (:)
  type(iotk_dummytype), optional :: dummy
  logical, optional, intent(in)  :: first
  logical, optional, intent(in)  :: newline
  integer, optional, intent(out) :: ierr
  integer :: ierrl
  integer :: attlen
  integer :: vallen
  integer :: namlen
  character :: delim
# 258 "iotk_attr.spp"
  character(iotk_vallenx) :: tmpval
  logical :: nl
  if(present(newline)) then
    nl = newline
  else
    nl = .false.
  endif
  ierrl = 0
  if(present(first)) then
    if(first) attr(1:1) = iotk_eos
  end if
  attlen = iotk_strlen_trim(attr)
  namlen = iotk_strlen_trim(name)
  if(.not.iotk_check_name(name)) then
    call iotk_error_issue(ierrl,"iotk_write_attr",__FILE__,__LINE__)
# 272 "iotk_attr.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.20 ")
# 272 "iotk_attr.spp"
call iotk_error_msg(ierrl,'Wrong tag name')
# 272 "iotk_attr.spp"
call iotk_error_write(ierrl,"name",name(1:namlen))
    goto 1
  end if
# 289 "iotk_attr.spp"
  delim = '"'
# 293 "iotk_attr.spp"
  call iotk_write(pack(val,mask=.true.),tmpval,ierrl)
# 295 "iotk_attr.spp"
  if(ierrl/=0) then
    call iotk_error_issue(ierrl,"iotk_write_attr",__FILE__,__LINE__)
# 296 "iotk_attr.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.20 ")
    goto 1
  end if
# 300 "iotk_attr.spp"
  vallen = iotk_strlen(tmpval)
  if(attlen+vallen+namlen+5>len(attr)) then
    call iotk_error_issue(ierrl,"iotk_write_attr",__FILE__,__LINE__)
# 302 "iotk_attr.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.20 ")
# 302 "iotk_attr.spp"
call iotk_error_msg(ierrl,'Attribute dummy argument is too short')
    goto 1
  end if
  if(.not. nl) then
    attr(attlen+1:attlen+vallen+namlen+5) = " "//name(1:namlen)//"="//delim//tmpval(1:vallen)//delim//iotk_eos
  else
    attr(attlen+1:attlen+vallen+namlen+len(iotk_newline)+5) &
       = iotk_newline//" "//name(1:namlen)//"="//delim//tmpval(1:vallen)//delim//iotk_eos
  endif
1 continue
  if(present(ierr)) then
    ierr = ierrl
  else
    if(ierrl/=0) call iotk_error_handler(ierrl)
  end if
end subroutine iotk_write_attr_LOGICAL2_1

# 320 "iotk_attr.spp"
subroutine iotk_scan_attr_LOGICAL2_1(attr,name,val,dummy,found,default,eos,ierr)
  use iotk_base
  use iotk_error_interf
  use iotk_attr_interf, only : iotk_read
  use iotk_str_interf
  use iotk_misc_interf
  implicit none
  character(*),             intent(in)  :: attr
  character(*),             intent(in)  :: name
#ifdef __IOTK_WORKAROUND6
  LOGICAL(kind=iotk_LOGICAL2)                        :: val (:)
#else
  LOGICAL(kind=iotk_LOGICAL2), intent(out)           :: val (:)
#endif
  type(iotk_dummytype), optional :: dummy
  logical,        optional, intent(out) :: found
  LOGICAL(kind=iotk_LOGICAL2), optional, intent(in)  :: default (:)
  logical,        optional, intent(in)  :: eos
  integer,        optional, intent(out) :: ierr
  integer :: ierrl
  integer :: attlen,pos,equal,namlen
  character :: delim
  logical :: foundl
  character(iotk_vallenx) :: valc
# 351 "iotk_attr.spp"
  integer :: index
  LOGICAL(kind=iotk_LOGICAL2), allocatable :: tmpval (:)
# 354 "iotk_attr.spp"
  ierrl = 0
  attlen=iotk_strlen(attr)
  namlen=iotk_strlen_trim(name)
  foundl = .false.
  equal = 0
  do
    pos = verify(attr(equal+1:attlen)," ")
    if(pos<=0) exit
    equal = equal + pos
    pos = scan(attr(equal+1:attlen),"=")
    if(pos<=0) then
      call iotk_error_issue(ierrl,"iotk_scan_attr",__FILE__,__LINE__)
# 365 "iotk_attr.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.20 ")
# 365 "iotk_attr.spp"
call iotk_error_msg(ierrl,'')
# 365 "iotk_attr.spp"
call iotk_error_write(ierrl,"attr",attr(equal+1:attlen))
      goto 1
    end if
    equal = equal + pos
    if(trim(attr(equal-pos:equal-1))==name(1:namlen)) foundl = .true.
    pos = verify(attr(equal+1:attlen)," ")
    if(pos<=0) then
      call iotk_error_issue(ierrl,"iotk_scan_attr",__FILE__,__LINE__)
# 372 "iotk_attr.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.20 ")
      goto 1
    end if
    equal = equal + pos
    delim = attr(equal:equal)
    if(delim/="'" .and. delim/='"') then
      call iotk_error_issue(ierrl,"iotk_scan_attr",__FILE__,__LINE__)
# 378 "iotk_attr.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.20 ")
      goto 1
    end if
    pos = scan(attr(equal+1:attlen),delim)
    if(pos<=0) then
      call iotk_error_issue(ierrl,"iotk_scan_attr",__FILE__,__LINE__)
# 383 "iotk_attr.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.20 ")
      goto 1
    end if
    if(foundl) exit
    equal = equal + pos
  end do
  if(foundl) then
    call iotk_strcpy(valc,attr(equal+1:equal+pos-1),ierrl)
    if(ierrl/=0) then
      call iotk_error_issue(ierrl,"iotk_scan_attr",__FILE__,__LINE__)
# 392 "iotk_attr.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.20 ")
      goto 1
    end if
  else
    goto 1
  end if
# 413 "iotk_attr.spp"
  allocate(tmpval(size(val)))
  index = 0
  call iotk_str_clean(valc)
  call iotk_read(tmpval,valc(1:iotk_strlen(valc)),index,ierrl)
  if(ierrl/=0) then
    call iotk_error_issue(ierrl,"iotk_scan_attr",__FILE__,__LINE__)
# 418 "iotk_attr.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.20 ")
    goto 1
  end if
# 424 "iotk_attr.spp"
  if(index/=size(val)) then
# 426 "iotk_attr.spp"
    call iotk_error_issue(ierrl,"iotk_scan_attr",__FILE__,__LINE__)
# 426 "iotk_attr.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.20 ")
# 426 "iotk_attr.spp"
call iotk_error_msg(ierrl,'Attribute size does not match')
# 426 "iotk_attr.spp"
call iotk_error_write(ierrl,"attr",valc)
# 426 "iotk_attr.spp"
call iotk_error_write(ierrl,"size",size(tmpval))
    goto 1
  end if
# 432 "iotk_attr.spp"
  val = reshape (source=tmpval,shape=shape(val))
# 434 "iotk_attr.spp"
  deallocate(tmpval)
# 436 "iotk_attr.spp"
1 continue
  if(ierrl/=0) foundl=.false.
  if(present(found)) found = foundl
  if(ierrl==0 .and. .not. present(found) .and. .not. present(default) .and. .not. foundl) then
    call iotk_error_issue(ierrl,"iotk_scan_attr",__FILE__,__LINE__)
# 440 "iotk_attr.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.20 ")
# 440 "iotk_attr.spp"
call iotk_error_msg(ierrl,'Attribute not found')
# 440 "iotk_attr.spp"
call iotk_error_write(ierrl,"name",name)
    ierrl = - ierrl
  end if
  if(present(default) .and. .not. foundl) then
# 453 "iotk_attr.spp"
    val = default
# 455 "iotk_attr.spp"
  end if
  if(present(ierr)) then
    ierr = ierrl
  else
    if(ierrl/=0) call iotk_error_handler(ierrl)
  end if
end subroutine iotk_scan_attr_LOGICAL2_1
# 463 "iotk_attr.spp"

#endif
#endif

subroutine iotk_attr_dummy_LOGICAL2_1
  write(0,*)
end subroutine iotk_attr_dummy_LOGICAL2_1

# 45 "iotk_attr.spp"

# 65 "iotk_attr.spp"


!------------------------------------------------------------------------------!
! Inclusion of configuration file
#include "iotk_config.h"
!------------------------------------------------------------------------------!

# 74 "iotk_attr.spp"
#include "iotk_auxmacros.h"
# 76 "iotk_attr.spp"

# 78 "iotk_attr.spp"

#ifdef __IOTK_LOGICAL2
#if 2 <= __IOTK_MAXRANK

# 158 "iotk_attr.spp"

# 233 "iotk_attr.spp"

# 236 "iotk_attr.spp"
subroutine iotk_write_attr_LOGICAL2_2(attr,name,val,dummy,first,newline,ierr)
  use iotk_base
  use iotk_error_interf
  use iotk_attr_interf
  use iotk_str_interf
  use iotk_misc_interf
  implicit none
  character(*), intent(inout) :: attr
  character(*), intent(in)    :: name
  LOGICAL(kind=iotk_LOGICAL2), intent(in)  :: val (:,:)
  type(iotk_dummytype), optional :: dummy
  logical, optional, intent(in)  :: first
  logical, optional, intent(in)  :: newline
  integer, optional, intent(out) :: ierr
  integer :: ierrl
  integer :: attlen
  integer :: vallen
  integer :: namlen
  character :: delim
# 258 "iotk_attr.spp"
  character(iotk_vallenx) :: tmpval
  logical :: nl
  if(present(newline)) then
    nl = newline
  else
    nl = .false.
  endif
  ierrl = 0
  if(present(first)) then
    if(first) attr(1:1) = iotk_eos
  end if
  attlen = iotk_strlen_trim(attr)
  namlen = iotk_strlen_trim(name)
  if(.not.iotk_check_name(name)) then
    call iotk_error_issue(ierrl,"iotk_write_attr",__FILE__,__LINE__)
# 272 "iotk_attr.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.20 ")
# 272 "iotk_attr.spp"
call iotk_error_msg(ierrl,'Wrong tag name')
# 272 "iotk_attr.spp"
call iotk_error_write(ierrl,"name",name(1:namlen))
    goto 1
  end if
# 289 "iotk_attr.spp"
  delim = '"'
# 293 "iotk_attr.spp"
  call iotk_write(pack(val,mask=.true.),tmpval,ierrl)
# 295 "iotk_attr.spp"
  if(ierrl/=0) then
    call iotk_error_issue(ierrl,"iotk_write_attr",__FILE__,__LINE__)
# 296 "iotk_attr.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.20 ")
    goto 1
  end if
# 300 "iotk_attr.spp"
  vallen = iotk_strlen(tmpval)
  if(attlen+vallen+namlen+5>len(attr)) then
    call iotk_error_issue(ierrl,"iotk_write_attr",__FILE__,__LINE__)
# 302 "iotk_attr.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.20 ")
# 302 "iotk_attr.spp"
call iotk_error_msg(ierrl,'Attribute dummy argument is too short')
    goto 1
  end if
  if(.not. nl) then
    attr(attlen+1:attlen+vallen+namlen+5) = " "//name(1:namlen)//"="//delim//tmpval(1:vallen)//delim//iotk_eos
  else
    attr(attlen+1:attlen+vallen+namlen+len(iotk_newline)+5) &
       = iotk_newline//" "//name(1:namlen)//"="//delim//tmpval(1:vallen)//delim//iotk_eos
  endif
1 continue
  if(present(ierr)) then
    ierr = ierrl
  else
    if(ierrl/=0) call iotk_error_handler(ierrl)
  end if
end subroutine iotk_write_attr_LOGICAL2_2

# 320 "iotk_attr.spp"
subroutine iotk_scan_attr_LOGICAL2_2(attr,name,val,dummy,found,default,eos,ierr)
  use iotk_base
  use iotk_error_interf
  use iotk_attr_interf, only : iotk_read
  use iotk_str_interf
  use iotk_misc_interf
  implicit none
  character(*),             intent(in)  :: attr
  character(*),             intent(in)  :: name
#ifdef __IOTK_WORKAROUND6
  LOGICAL(kind=iotk_LOGICAL2)                        :: val (:,:)
#else
  LOGICAL(kind=iotk_LOGICAL2), intent(out)           :: val (:,:)
#endif
  type(iotk_dummytype), optional :: dummy
  logical,        optional, intent(out) :: found
  LOGICAL(kind=iotk_LOGICAL2), optional, intent(in)  :: default (:,:)
  logical,        optional, intent(in)  :: eos
  integer,        optional, intent(out) :: ierr
  integer :: ierrl
  integer :: attlen,pos,equal,namlen
  character :: delim
  logical :: foundl
  character(iotk_vallenx) :: valc
# 351 "iotk_attr.spp"
  integer :: index
  LOGICAL(kind=iotk_LOGICAL2), allocatable :: tmpval (:)
# 354 "iotk_attr.spp"
  ierrl = 0
  attlen=iotk_strlen(attr)
  namlen=iotk_strlen_trim(name)
  foundl = .false.
  equal = 0
  do
    pos = verify(attr(equal+1:attlen)," ")
    if(pos<=0) exit
    equal = equal + pos
    pos = scan(attr(equal+1:attlen),"=")
    if(pos<=0) then
      call iotk_error_issue(ierrl,"iotk_scan_attr",__FILE__,__LINE__)
# 365 "iotk_attr.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.20 ")
# 365 "iotk_attr.spp"
call iotk_error_msg(ierrl,'')
# 365 "iotk_attr.spp"
call iotk_error_write(ierrl,"attr",attr(equal+1:attlen))
      goto 1
    end if
    equal = equal + pos
    if(trim(attr(equal-pos:equal-1))==name(1:namlen)) foundl = .true.
    pos = verify(attr(equal+1:attlen)," ")
    if(pos<=0) then
      call iotk_error_issue(ierrl,"iotk_scan_attr",__FILE__,__LINE__)
# 372 "iotk_attr.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.20 ")
      goto 1
    end if
    equal = equal + pos
    delim = attr(equal:equal)
    if(delim/="'" .and. delim/='"') then
      call iotk_error_issue(ierrl,"iotk_scan_attr",__FILE__,__LINE__)
# 378 "iotk_attr.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.20 ")
      goto 1
    end if
    pos = scan(attr(equal+1:attlen),delim)
    if(pos<=0) then
      call iotk_error_issue(ierrl,"iotk_scan_attr",__FILE__,__LINE__)
# 383 "iotk_attr.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.20 ")
      goto 1
    end if
    if(foundl) exit
    equal = equal + pos
  end do
  if(foundl) then
    call iotk_strcpy(valc,attr(equal+1:equal+pos-1),ierrl)
    if(ierrl/=0) then
      call iotk_error_issue(ierrl,"iotk_scan_attr",__FILE__,__LINE__)
# 392 "iotk_attr.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.20 ")
      goto 1
    end if
  else
    goto 1
  end if
# 413 "iotk_attr.spp"
  allocate(tmpval(size(val)))
  index = 0
  call iotk_str_clean(valc)
  call iotk_read(tmpval,valc(1:iotk_strlen(valc)),index,ierrl)
  if(ierrl/=0) then
    call iotk_error_issue(ierrl,"iotk_scan_attr",__FILE__,__LINE__)
# 418 "iotk_attr.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.20 ")
    goto 1
  end if
# 424 "iotk_attr.spp"
  if(index/=size(val)) then
# 426 "iotk_attr.spp"
    call iotk_error_issue(ierrl,"iotk_scan_attr",__FILE__,__LINE__)
# 426 "iotk_attr.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.20 ")
# 426 "iotk_attr.spp"
call iotk_error_msg(ierrl,'Attribute size does not match')
# 426 "iotk_attr.spp"
call iotk_error_write(ierrl,"attr",valc)
# 426 "iotk_attr.spp"
call iotk_error_write(ierrl,"size",size(tmpval))
    goto 1
  end if
# 432 "iotk_attr.spp"
  val = reshape (source=tmpval,shape=shape(val))
# 434 "iotk_attr.spp"
  deallocate(tmpval)
# 436 "iotk_attr.spp"
1 continue
  if(ierrl/=0) foundl=.false.
  if(present(found)) found = foundl
  if(ierrl==0 .and. .not. present(found) .and. .not. present(default) .and. .not. foundl) then
    call iotk_error_issue(ierrl,"iotk_scan_attr",__FILE__,__LINE__)
# 440 "iotk_attr.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.20 ")
# 440 "iotk_attr.spp"
call iotk_error_msg(ierrl,'Attribute not found')
# 440 "iotk_attr.spp"
call iotk_error_write(ierrl,"name",name)
    ierrl = - ierrl
  end if
  if(present(default) .and. .not. foundl) then
# 453 "iotk_attr.spp"
    val = default
# 455 "iotk_attr.spp"
  end if
  if(present(ierr)) then
    ierr = ierrl
  else
    if(ierrl/=0) call iotk_error_handler(ierrl)
  end if
end subroutine iotk_scan_attr_LOGICAL2_2
# 463 "iotk_attr.spp"

#endif
#endif

subroutine iotk_attr_dummy_LOGICAL2_2
  write(0,*)
end subroutine iotk_attr_dummy_LOGICAL2_2

# 45 "iotk_attr.spp"

# 48 "iotk_attr.spp"
! Input/Output Tool Kit (IOTK)
! Copyright (C) 2004-2006 Giovanni Bussi
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
# 65 "iotk_attr.spp"


!------------------------------------------------------------------------------!
! Inclusion of configuration file
#include "iotk_config.h"
!------------------------------------------------------------------------------!

# 74 "iotk_attr.spp"
#include "iotk_auxmacros.h"
# 76 "iotk_attr.spp"

# 78 "iotk_attr.spp"

#ifdef __IOTK_LOGICAL2
#if 3 <= __IOTK_MAXRANK

# 158 "iotk_attr.spp"

# 233 "iotk_attr.spp"

# 236 "iotk_attr.spp"
subroutine iotk_write_attr_LOGICAL2_3(attr,name,val,dummy,first,newline,ierr)
  use iotk_base
  use iotk_error_interf
  use iotk_attr_interf
  use iotk_str_interf
  use iotk_misc_interf
  implicit none
  character(*), intent(inout) :: attr
  character(*), intent(in)    :: name
  LOGICAL(kind=iotk_LOGICAL2), intent(in)  :: val (:,:,:)
  type(iotk_dummytype), optional :: dummy
  logical, optional, intent(in)  :: first
  logical, optional, intent(in)  :: newline
  integer, optional, intent(out) :: ierr
  integer :: ierrl
  integer :: attlen
  integer :: vallen
  integer :: namlen
  character :: delim
# 258 "iotk_attr.spp"
  character(iotk_vallenx) :: tmpval
  logical :: nl
  if(present(newline)) then
    nl = newline
  else
    nl = .false.
  endif
  ierrl = 0
  if(present(first)) then
    if(first) attr(1:1) = iotk_eos
  end if
  attlen = iotk_strlen_trim(attr)
  namlen = iotk_strlen_trim(name)
  if(.not.iotk_check_name(name)) then
    call iotk_error_issue(ierrl,"iotk_write_attr",__FILE__,__LINE__)
# 272 "iotk_attr.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.20 ")
# 272 "iotk_attr.spp"
call iotk_error_msg(ierrl,'Wrong tag name')
# 272 "iotk_attr.spp"
call iotk_error_write(ierrl,"name",name(1:namlen))
    goto 1
  end if
# 289 "iotk_attr.spp"
  delim = '"'
# 293 "iotk_attr.spp"
  call iotk_write(pack(val,mask=.true.),tmpval,ierrl)
# 295 "iotk_attr.spp"
  if(ierrl/=0) then
    call iotk_error_issue(ierrl,"iotk_write_attr",__FILE__,__LINE__)
# 296 "iotk_attr.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.20 ")
    goto 1
  end if
# 300 "iotk_attr.spp"
  vallen = iotk_strlen(tmpval)
  if(attlen+vallen+namlen+5>len(attr)) then
    call iotk_error_issue(ierrl,"iotk_write_attr",__FILE__,__LINE__)
# 302 "iotk_attr.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.20 ")
# 302 "iotk_attr.spp"
call iotk_error_msg(ierrl,'Attribute dummy argument is too short')
    goto 1
  end if
  if(.not. nl) then
    attr(attlen+1:attlen+vallen+namlen+5) = " "//name(1:namlen)//"="//delim//tmpval(1:vallen)//delim//iotk_eos
  else
    attr(attlen+1:attlen+vallen+namlen+len(iotk_newline)+5) &
       = iotk_newline//" "//name(1:namlen)//"="//delim//tmpval(1:vallen)//delim//iotk_eos
  endif
1 continue
  if(present(ierr)) then
    ierr = ierrl
  else
    if(ierrl/=0) call iotk_error_handler(ierrl)
  end if
end subroutine iotk_write_attr_LOGICAL2_3

# 320 "iotk_attr.spp"
subroutine iotk_scan_attr_LOGICAL2_3(attr,name,val,dummy,found,default,eos,ierr)
  use iotk_base
  use iotk_error_interf
  use iotk_attr_interf, only : iotk_read
  use iotk_str_interf
  use iotk_misc_interf
  implicit none
  character(*),             intent(in)  :: attr
  character(*),             intent(in)  :: name
#ifdef __IOTK_WORKAROUND6
  LOGICAL(kind=iotk_LOGICAL2)                        :: val (:,:,:)
#else
  LOGICAL(kind=iotk_LOGICAL2), intent(out)           :: val (:,:,:)
#endif
  type(iotk_dummytype), optional :: dummy
  logical,        optional, intent(out) :: found
  LOGICAL(kind=iotk_LOGICAL2), optional, intent(in)  :: default (:,:,:)
  logical,        optional, intent(in)  :: eos
  integer,        optional, intent(out) :: ierr
  integer :: ierrl
  integer :: attlen,pos,equal,namlen
  character :: delim
  logical :: foundl
  character(iotk_vallenx) :: valc
# 351 "iotk_attr.spp"
  integer :: index
  LOGICAL(kind=iotk_LOGICAL2), allocatable :: tmpval (:)
# 354 "iotk_attr.spp"
  ierrl = 0
  attlen=iotk_strlen(attr)
  namlen=iotk_strlen_trim(name)
  foundl = .false.
  equal = 0
  do
    pos = verify(attr(equal+1:attlen)," ")
    if(pos<=0) exit
    equal = equal + pos
    pos = scan(attr(equal+1:attlen),"=")
    if(pos<=0) then
      call iotk_error_issue(ierrl,"iotk_scan_attr",__FILE__,__LINE__)
# 365 "iotk_attr.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.20 ")
# 365 "iotk_attr.spp"
call iotk_error_msg(ierrl,'')
# 365 "iotk_attr.spp"
call iotk_error_write(ierrl,"attr",attr(equal+1:attlen))
      goto 1
    end if
    equal = equal + pos
    if(trim(attr(equal-pos:equal-1))==name(1:namlen)) foundl = .true.
    pos = verify(attr(equal+1:attlen)," ")
    if(pos<=0) then
      call iotk_error_issue(ierrl,"iotk_scan_attr",__FILE__,__LINE__)
# 372 "iotk_attr.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.20 ")
      goto 1
    end if
    equal = equal + pos
    delim = attr(equal:equal)
    if(delim/="'" .and. delim/='"') then
      call iotk_error_issue(ierrl,"iotk_scan_attr",__FILE__,__LINE__)
# 378 "iotk_attr.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.20 ")
      goto 1
    end if
    pos = scan(attr(equal+1:attlen),delim)
    if(pos<=0) then
      call iotk_error_issue(ierrl,"iotk_scan_attr",__FILE__,__LINE__)
# 383 "iotk_attr.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.20 ")
      goto 1
    end if
    if(foundl) exit
    equal = equal + pos
  end do
  if(foundl) then
    call iotk_strcpy(valc,attr(equal+1:equal+pos-1),ierrl)
    if(ierrl/=0) then
      call iotk_error_issue(ierrl,"iotk_scan_attr",__FILE__,__LINE__)
# 392 "iotk_attr.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.20 ")
      goto 1
    end if
  else
    goto 1
  end if
# 413 "iotk_attr.spp"
  allocate(tmpval(size(val)))
  index = 0
  call iotk_str_clean(valc)
  call iotk_read(tmpval,valc(1:iotk_strlen(valc)),index,ierrl)
  if(ierrl/=0) then
    call iotk_error_issue(ierrl,"iotk_scan_attr",__FILE__,__LINE__)
# 418 "iotk_attr.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.20 ")
    goto 1
  end if
# 424 "iotk_attr.spp"
  if(index/=size(val)) then
# 426 "iotk_attr.spp"
    call iotk_error_issue(ierrl,"iotk_scan_attr",__FILE__,__LINE__)
# 426 "iotk_attr.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.20 ")
# 426 "iotk_attr.spp"
call iotk_error_msg(ierrl,'Attribute size does not match')
# 426 "iotk_attr.spp"
call iotk_error_write(ierrl,"attr",valc)
# 426 "iotk_attr.spp"
call iotk_error_write(ierrl,"size",size(tmpval))
    goto 1
  end if
# 432 "iotk_attr.spp"
  val = reshape (source=tmpval,shape=shape(val))
# 434 "iotk_attr.spp"
  deallocate(tmpval)
# 436 "iotk_attr.spp"
1 continue
  if(ierrl/=0) foundl=.false.
  if(present(found)) found = foundl
  if(ierrl==0 .and. .not. present(found) .and. .not. present(default) .and. .not. foundl) then
    call iotk_error_issue(ierrl,"iotk_scan_attr",__FILE__,__LINE__)
# 440 "iotk_attr.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.20 ")
# 440 "iotk_attr.spp"
call iotk_error_msg(ierrl,'Attribute not found')
# 440 "iotk_attr.spp"
call iotk_error_write(ierrl,"name",name)
    ierrl = - ierrl
  end if
  if(present(default) .and. .not. foundl) then
# 453 "iotk_attr.spp"
    val = default
# 455 "iotk_attr.spp"
  end if
  if(present(ierr)) then
    ierr = ierrl
  else
    if(ierrl/=0) call iotk_error_handler(ierrl)
  end if
end subroutine iotk_scan_attr_LOGICAL2_3
# 463 "iotk_attr.spp"

#endif
#endif

subroutine iotk_attr_dummy_LOGICAL2_3
  write(0,*)
end subroutine iotk_attr_dummy_LOGICAL2_3

# 45 "iotk_attr.spp"

# 65 "iotk_attr.spp"


!------------------------------------------------------------------------------!
! Inclusion of configuration file
#include "iotk_config.h"
!------------------------------------------------------------------------------!

# 74 "iotk_attr.spp"
#include "iotk_auxmacros.h"
# 76 "iotk_attr.spp"

# 78 "iotk_attr.spp"

#ifdef __IOTK_LOGICAL2
#if 4 <= __IOTK_MAXRANK

# 158 "iotk_attr.spp"

# 233 "iotk_attr.spp"

# 236 "iotk_attr.spp"
subroutine iotk_write_attr_LOGICAL2_4(attr,name,val,dummy,first,newline,ierr)
  use iotk_base
  use iotk_error_interf
  use iotk_attr_interf
  use iotk_str_interf
  use iotk_misc_interf
  implicit none
  character(*), intent(inout) :: attr
  character(*), intent(in)    :: name
  LOGICAL(kind=iotk_LOGICAL2), intent(in)  :: val (:,:,:,:)
  type(iotk_dummytype), optional :: dummy
  logical, optional, intent(in)  :: first
  logical, optional, intent(in)  :: newline
  integer, optional, intent(out) :: ierr
  integer :: ierrl
  integer :: attlen
  integer :: vallen
  integer :: namlen
  character :: delim
# 258 "iotk_attr.spp"
  character(iotk_vallenx) :: tmpval
  logical :: nl
  if(present(newline)) then
    nl = newline
  else
    nl = .false.
  endif
  ierrl = 0
  if(present(first)) then
    if(first) attr(1:1) = iotk_eos
  end if
  attlen = iotk_strlen_trim(attr)
  namlen = iotk_strlen_trim(name)
  if(.not.iotk_check_name(name)) then
    call iotk_error_issue(ierrl,"iotk_write_attr",__FILE__,__LINE__)
# 272 "iotk_attr.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.20 ")
# 272 "iotk_attr.spp"
call iotk_error_msg(ierrl,'Wrong tag name')
# 272 "iotk_attr.spp"
call iotk_error_write(ierrl,"name",name(1:namlen))
    goto 1
  end if
# 289 "iotk_attr.spp"
  delim = '"'
# 293 "iotk_attr.spp"
  call iotk_write(pack(val,mask=.true.),tmpval,ierrl)
# 295 "iotk_attr.spp"
  if(ierrl/=0) then
    call iotk_error_issue(ierrl,"iotk_write_attr",__FILE__,__LINE__)
# 296 "iotk_attr.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.20 ")
    goto 1
  end if
# 300 "iotk_attr.spp"
  vallen = iotk_strlen(tmpval)
  if(attlen+vallen+namlen+5>len(attr)) then
    call iotk_error_issue(ierrl,"iotk_write_attr",__FILE__,__LINE__)
# 302 "iotk_attr.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.20 ")
# 302 "iotk_attr.spp"
call iotk_error_msg(ierrl,'Attribute dummy argument is too short')
    goto 1
  end if
  if(.not. nl) then
    attr(attlen+1:attlen+vallen+namlen+5) = " "//name(1:namlen)//"="//delim//tmpval(1:vallen)//delim//iotk_eos
  else
    attr(attlen+1:attlen+vallen+namlen+len(iotk_newline)+5) &
       = iotk_newline//" "//name(1:namlen)//"="//delim//tmpval(1:vallen)//delim//iotk_eos
  endif
1 continue
  if(present(ierr)) then
    ierr = ierrl
  else
    if(ierrl/=0) call iotk_error_handler(ierrl)
  end if
end subroutine iotk_write_attr_LOGICAL2_4

# 320 "iotk_attr.spp"
subroutine iotk_scan_attr_LOGICAL2_4(attr,name,val,dummy,found,default,eos,ierr)
  use iotk_base
  use iotk_error_interf
  use iotk_attr_interf, only : iotk_read
  use iotk_str_interf
  use iotk_misc_interf
  implicit none
  character(*),             intent(in)  :: attr
  character(*),             intent(in)  :: name
#ifdef __IOTK_WORKAROUND6
  LOGICAL(kind=iotk_LOGICAL2)                        :: val (:,:,:,:)
#else
  LOGICAL(kind=iotk_LOGICAL2), intent(out)           :: val (:,:,:,:)
#endif
  type(iotk_dummytype), optional :: dummy
  logical,        optional, intent(out) :: found
  LOGICAL(kind=iotk_LOGICAL2), optional, intent(in)  :: default (:,:,:,:)
  logical,        optional, intent(in)  :: eos
  integer,        optional, intent(out) :: ierr
  integer :: ierrl
  integer :: attlen,pos,equal,namlen
  character :: delim
  logical :: foundl
  character(iotk_vallenx) :: valc
# 351 "iotk_attr.spp"
  integer :: index
  LOGICAL(kind=iotk_LOGICAL2), allocatable :: tmpval (:)
# 354 "iotk_attr.spp"
  ierrl = 0
  attlen=iotk_strlen(attr)
  namlen=iotk_strlen_trim(name)
  foundl = .false.
  equal = 0
  do
    pos = verify(attr(equal+1:attlen)," ")
    if(pos<=0) exit
    equal = equal + pos
    pos = scan(attr(equal+1:attlen),"=")
    if(pos<=0) then
      call iotk_error_issue(ierrl,"iotk_scan_attr",__FILE__,__LINE__)
# 365 "iotk_attr.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.20 ")
# 365 "iotk_attr.spp"
call iotk_error_msg(ierrl,'')
# 365 "iotk_attr.spp"
call iotk_error_write(ierrl,"attr",attr(equal+1:attlen))
      goto 1
    end if
    equal = equal + pos
    if(trim(attr(equal-pos:equal-1))==name(1:namlen)) foundl = .true.
    pos = verify(attr(equal+1:attlen)," ")
    if(pos<=0) then
      call iotk_error_issue(ierrl,"iotk_scan_attr",__FILE__,__LINE__)
# 372 "iotk_attr.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.20 ")
      goto 1
    end if
    equal = equal + pos
    delim = attr(equal:equal)
    if(delim/="'" .and. delim/='"') then
      call iotk_error_issue(ierrl,"iotk_scan_attr",__FILE__,__LINE__)
# 378 "iotk_attr.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.20 ")
      goto 1
    end if
    pos = scan(attr(equal+1:attlen),delim)
    if(pos<=0) then
      call iotk_error_issue(ierrl,"iotk_scan_attr",__FILE__,__LINE__)
# 383 "iotk_attr.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.20 ")
      goto 1
    end if
    if(foundl) exit
    equal = equal + pos
  end do
  if(foundl) then
    call iotk_strcpy(valc,attr(equal+1:equal+pos-1),ierrl)
    if(ierrl/=0) then
      call iotk_error_issue(ierrl,"iotk_scan_attr",__FILE__,__LINE__)
# 392 "iotk_attr.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.20 ")
      goto 1
    end if
  else
    goto 1
  end if
# 413 "iotk_attr.spp"
  allocate(tmpval(size(val)))
  index = 0
  call iotk_str_clean(valc)
  call iotk_read(tmpval,valc(1:iotk_strlen(valc)),index,ierrl)
  if(ierrl/=0) then
    call iotk_error_issue(ierrl,"iotk_scan_attr",__FILE__,__LINE__)
# 418 "iotk_attr.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.20 ")
    goto 1
  end if
# 424 "iotk_attr.spp"
  if(index/=size(val)) then
# 426 "iotk_attr.spp"
    call iotk_error_issue(ierrl,"iotk_scan_attr",__FILE__,__LINE__)
# 426 "iotk_attr.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.20 ")
# 426 "iotk_attr.spp"
call iotk_error_msg(ierrl,'Attribute size does not match')
# 426 "iotk_attr.spp"
call iotk_error_write(ierrl,"attr",valc)
# 426 "iotk_attr.spp"
call iotk_error_write(ierrl,"size",size(tmpval))
    goto 1
  end if
# 432 "iotk_attr.spp"
  val = reshape (source=tmpval,shape=shape(val))
# 434 "iotk_attr.spp"
  deallocate(tmpval)
# 436 "iotk_attr.spp"
1 continue
  if(ierrl/=0) foundl=.false.
  if(present(found)) found = foundl
  if(ierrl==0 .and. .not. present(found) .and. .not. present(default) .and. .not. foundl) then
    call iotk_error_issue(ierrl,"iotk_scan_attr",__FILE__,__LINE__)
# 440 "iotk_attr.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.20 ")
# 440 "iotk_attr.spp"
call iotk_error_msg(ierrl,'Attribute not found')
# 440 "iotk_attr.spp"
call iotk_error_write(ierrl,"name",name)
    ierrl = - ierrl
  end if
  if(present(default) .and. .not. foundl) then
# 453 "iotk_attr.spp"
    val = default
# 455 "iotk_attr.spp"
  end if
  if(present(ierr)) then
    ierr = ierrl
  else
    if(ierrl/=0) call iotk_error_handler(ierrl)
  end if
end subroutine iotk_scan_attr_LOGICAL2_4
# 463 "iotk_attr.spp"

#endif
#endif

subroutine iotk_attr_dummy_LOGICAL2_4
  write(0,*)
end subroutine iotk_attr_dummy_LOGICAL2_4

# 45 "iotk_attr.spp"

# 65 "iotk_attr.spp"


!------------------------------------------------------------------------------!
! Inclusion of configuration file
#include "iotk_config.h"
!------------------------------------------------------------------------------!

# 74 "iotk_attr.spp"
#include "iotk_auxmacros.h"
# 76 "iotk_attr.spp"

# 78 "iotk_attr.spp"

#ifdef __IOTK_LOGICAL2
#if 5 <= __IOTK_MAXRANK

# 158 "iotk_attr.spp"

# 233 "iotk_attr.spp"

# 236 "iotk_attr.spp"
subroutine iotk_write_attr_LOGICAL2_5(attr,name,val,dummy,first,newline,ierr)
  use iotk_base
  use iotk_error_interf
  use iotk_attr_interf
  use iotk_str_interf
  use iotk_misc_interf
  implicit none
  character(*), intent(inout) :: attr
  character(*), intent(in)    :: name
  LOGICAL(kind=iotk_LOGICAL2), intent(in)  :: val (:,:,:,:,:)
  type(iotk_dummytype), optional :: dummy
  logical, optional, intent(in)  :: first
  logical, optional, intent(in)  :: newline
  integer, optional, intent(out) :: ierr
  integer :: ierrl
  integer :: attlen
  integer :: vallen
  integer :: namlen
  character :: delim
# 258 "iotk_attr.spp"
  character(iotk_vallenx) :: tmpval
  logical :: nl
  if(present(newline)) then
    nl = newline
  else
    nl = .false.
  endif
  ierrl = 0
  if(present(first)) then
    if(first) attr(1:1) = iotk_eos
  end if
  attlen = iotk_strlen_trim(attr)
  namlen = iotk_strlen_trim(name)
  if(.not.iotk_check_name(name)) then
    call iotk_error_issue(ierrl,"iotk_write_attr",__FILE__,__LINE__)
# 272 "iotk_attr.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.20 ")
# 272 "iotk_attr.spp"
call iotk_error_msg(ierrl,'Wrong tag name')
# 272 "iotk_attr.spp"
call iotk_error_write(ierrl,"name",name(1:namlen))
    goto 1
  end if
# 289 "iotk_attr.spp"
  delim = '"'
# 293 "iotk_attr.spp"
  call iotk_write(pack(val,mask=.true.),tmpval,ierrl)
# 295 "iotk_attr.spp"
  if(ierrl/=0) then
    call iotk_error_issue(ierrl,"iotk_write_attr",__FILE__,__LINE__)
# 296 "iotk_attr.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.20 ")
    goto 1
  end if
# 300 "iotk_attr.spp"
  vallen = iotk_strlen(tmpval)
  if(attlen+vallen+namlen+5>len(attr)) then
    call iotk_error_issue(ierrl,"iotk_write_attr",__FILE__,__LINE__)
# 302 "iotk_attr.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.20 ")
# 302 "iotk_attr.spp"
call iotk_error_msg(ierrl,'Attribute dummy argument is too short')
    goto 1
  end if
  if(.not. nl) then
    attr(attlen+1:attlen+vallen+namlen+5) = " "//name(1:namlen)//"="//delim//tmpval(1:vallen)//delim//iotk_eos
  else
    attr(attlen+1:attlen+vallen+namlen+len(iotk_newline)+5) &
       = iotk_newline//" "//name(1:namlen)//"="//delim//tmpval(1:vallen)//delim//iotk_eos
  endif
1 continue
  if(present(ierr)) then
    ierr = ierrl
  else
    if(ierrl/=0) call iotk_error_handler(ierrl)
  end if
end subroutine iotk_write_attr_LOGICAL2_5

# 320 "iotk_attr.spp"
subroutine iotk_scan_attr_LOGICAL2_5(attr,name,val,dummy,found,default,eos,ierr)
  use iotk_base
  use iotk_error_interf
  use iotk_attr_interf, only : iotk_read
  use iotk_str_interf
  use iotk_misc_interf
  implicit none
  character(*),             intent(in)  :: attr
  character(*),             intent(in)  :: name
#ifdef __IOTK_WORKAROUND6
  LOGICAL(kind=iotk_LOGICAL2)                        :: val (:,:,:,:,:)
#else
  LOGICAL(kind=iotk_LOGICAL2), intent(out)           :: val (:,:,:,:,:)
#endif
  type(iotk_dummytype), optional :: dummy
  logical,        optional, intent(out) :: found
  LOGICAL(kind=iotk_LOGICAL2), optional, intent(in)  :: default (:,:,:,:,:)
  logical,        optional, intent(in)  :: eos
  integer,        optional, intent(out) :: ierr
  integer :: ierrl
  integer :: attlen,pos,equal,namlen
  character :: delim
  logical :: foundl
  character(iotk_vallenx) :: valc
# 351 "iotk_attr.spp"
  integer :: index
  LOGICAL(kind=iotk_LOGICAL2), allocatable :: tmpval (:)
# 354 "iotk_attr.spp"
  ierrl = 0
  attlen=iotk_strlen(attr)
  namlen=iotk_strlen_trim(name)
  foundl = .false.
  equal = 0
  do
    pos = verify(attr(equal+1:attlen)," ")
    if(pos<=0) exit
    equal = equal + pos
    pos = scan(attr(equal+1:attlen),"=")
    if(pos<=0) then
      call iotk_error_issue(ierrl,"iotk_scan_attr",__FILE__,__LINE__)
# 365 "iotk_attr.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.20 ")
# 365 "iotk_attr.spp"
call iotk_error_msg(ierrl,'')
# 365 "iotk_attr.spp"
call iotk_error_write(ierrl,"attr",attr(equal+1:attlen))
      goto 1
    end if
    equal = equal + pos
    if(trim(attr(equal-pos:equal-1))==name(1:namlen)) foundl = .true.
    pos = verify(attr(equal+1:attlen)," ")
    if(pos<=0) then
      call iotk_error_issue(ierrl,"iotk_scan_attr",__FILE__,__LINE__)
# 372 "iotk_attr.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.20 ")
      goto 1
    end if
    equal = equal + pos
    delim = attr(equal:equal)
    if(delim/="'" .and. delim/='"') then
      call iotk_error_issue(ierrl,"iotk_scan_attr",__FILE__,__LINE__)
# 378 "iotk_attr.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.20 ")
      goto 1
    end if
    pos = scan(attr(equal+1:attlen),delim)
    if(pos<=0) then
      call iotk_error_issue(ierrl,"iotk_scan_attr",__FILE__,__LINE__)
# 383 "iotk_attr.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.20 ")
      goto 1
    end if
    if(foundl) exit
    equal = equal + pos
  end do
  if(foundl) then
    call iotk_strcpy(valc,attr(equal+1:equal+pos-1),ierrl)
    if(ierrl/=0) then
      call iotk_error_issue(ierrl,"iotk_scan_attr",__FILE__,__LINE__)
# 392 "iotk_attr.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.20 ")
      goto 1
    end if
  else
    goto 1
  end if
# 413 "iotk_attr.spp"
  allocate(tmpval(size(val)))
  index = 0
  call iotk_str_clean(valc)
  call iotk_read(tmpval,valc(1:iotk_strlen(valc)),index,ierrl)
  if(ierrl/=0) then
    call iotk_error_issue(ierrl,"iotk_scan_attr",__FILE__,__LINE__)
# 418 "iotk_attr.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.20 ")
    goto 1
  end if
# 424 "iotk_attr.spp"
  if(index/=size(val)) then
# 426 "iotk_attr.spp"
    call iotk_error_issue(ierrl,"iotk_scan_attr",__FILE__,__LINE__)
# 426 "iotk_attr.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.20 ")
# 426 "iotk_attr.spp"
call iotk_error_msg(ierrl,'Attribute size does not match')
# 426 "iotk_attr.spp"
call iotk_error_write(ierrl,"attr",valc)
# 426 "iotk_attr.spp"
call iotk_error_write(ierrl,"size",size(tmpval))
    goto 1
  end if
# 432 "iotk_attr.spp"
  val = reshape (source=tmpval,shape=shape(val))
# 434 "iotk_attr.spp"
  deallocate(tmpval)
# 436 "iotk_attr.spp"
1 continue
  if(ierrl/=0) foundl=.false.
  if(present(found)) found = foundl
  if(ierrl==0 .and. .not. present(found) .and. .not. present(default) .and. .not. foundl) then
    call iotk_error_issue(ierrl,"iotk_scan_attr",__FILE__,__LINE__)
# 440 "iotk_attr.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.20 ")
# 440 "iotk_attr.spp"
call iotk_error_msg(ierrl,'Attribute not found')
# 440 "iotk_attr.spp"
call iotk_error_write(ierrl,"name",name)
    ierrl = - ierrl
  end if
  if(present(default) .and. .not. foundl) then
# 453 "iotk_attr.spp"
    val = default
# 455 "iotk_attr.spp"
  end if
  if(present(ierr)) then
    ierr = ierrl
  else
    if(ierrl/=0) call iotk_error_handler(ierrl)
  end if
end subroutine iotk_scan_attr_LOGICAL2_5
# 463 "iotk_attr.spp"

#endif
#endif

subroutine iotk_attr_dummy_LOGICAL2_5
  write(0,*)
end subroutine iotk_attr_dummy_LOGICAL2_5

# 45 "iotk_attr.spp"

# 48 "iotk_attr.spp"
! Input/Output Tool Kit (IOTK)
! Copyright (C) 2004-2006 Giovanni Bussi
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
# 65 "iotk_attr.spp"


!------------------------------------------------------------------------------!
! Inclusion of configuration file
#include "iotk_config.h"
!------------------------------------------------------------------------------!

# 74 "iotk_attr.spp"
#include "iotk_auxmacros.h"
# 76 "iotk_attr.spp"

# 78 "iotk_attr.spp"

#ifdef __IOTK_LOGICAL2
#if 6 <= __IOTK_MAXRANK

# 158 "iotk_attr.spp"

# 233 "iotk_attr.spp"

# 236 "iotk_attr.spp"
subroutine iotk_write_attr_LOGICAL2_6(attr,name,val,dummy,first,newline,ierr)
  use iotk_base
  use iotk_error_interf
  use iotk_attr_interf
  use iotk_str_interf
  use iotk_misc_interf
  implicit none
  character(*), intent(inout) :: attr
  character(*), intent(in)    :: name
  LOGICAL(kind=iotk_LOGICAL2), intent(in)  :: val (:,:,:,:,:,:)
  type(iotk_dummytype), optional :: dummy
  logical, optional, intent(in)  :: first
  logical, optional, intent(in)  :: newline
  integer, optional, intent(out) :: ierr
  integer :: ierrl
  integer :: attlen
  integer :: vallen
  integer :: namlen
  character :: delim
# 258 "iotk_attr.spp"
  character(iotk_vallenx) :: tmpval
  logical :: nl
  if(present(newline)) then
    nl = newline
  else
    nl = .false.
  endif
  ierrl = 0
  if(present(first)) then
    if(first) attr(1:1) = iotk_eos
  end if
  attlen = iotk_strlen_trim(attr)
  namlen = iotk_strlen_trim(name)
  if(.not.iotk_check_name(name)) then
    call iotk_error_issue(ierrl,"iotk_write_attr",__FILE__,__LINE__)
# 272 "iotk_attr.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.20 ")
# 272 "iotk_attr.spp"
call iotk_error_msg(ierrl,'Wrong tag name')
# 272 "iotk_attr.spp"
call iotk_error_write(ierrl,"name",name(1:namlen))
    goto 1
  end if
# 289 "iotk_attr.spp"
  delim = '"'
# 293 "iotk_attr.spp"
  call iotk_write(pack(val,mask=.true.),tmpval,ierrl)
# 295 "iotk_attr.spp"
  if(ierrl/=0) then
    call iotk_error_issue(ierrl,"iotk_write_attr",__FILE__,__LINE__)
# 296 "iotk_attr.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.20 ")
    goto 1
  end if
# 300 "iotk_attr.spp"
  vallen = iotk_strlen(tmpval)
  if(attlen+vallen+namlen+5>len(attr)) then
    call iotk_error_issue(ierrl,"iotk_write_attr",__FILE__,__LINE__)
# 302 "iotk_attr.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.20 ")
# 302 "iotk_attr.spp"
call iotk_error_msg(ierrl,'Attribute dummy argument is too short')
    goto 1
  end if
  if(.not. nl) then
    attr(attlen+1:attlen+vallen+namlen+5) = " "//name(1:namlen)//"="//delim//tmpval(1:vallen)//delim//iotk_eos
  else
    attr(attlen+1:attlen+vallen+namlen+len(iotk_newline)+5) &
       = iotk_newline//" "//name(1:namlen)//"="//delim//tmpval(1:vallen)//delim//iotk_eos
  endif
1 continue
  if(present(ierr)) then
    ierr = ierrl
  else
    if(ierrl/=0) call iotk_error_handler(ierrl)
  end if
end subroutine iotk_write_attr_LOGICAL2_6

# 320 "iotk_attr.spp"
subroutine iotk_scan_attr_LOGICAL2_6(attr,name,val,dummy,found,default,eos,ierr)
  use iotk_base
  use iotk_error_interf
  use iotk_attr_interf, only : iotk_read
  use iotk_str_interf
  use iotk_misc_interf
  implicit none
  character(*),             intent(in)  :: attr
  character(*),             intent(in)  :: name
#ifdef __IOTK_WORKAROUND6
  LOGICAL(kind=iotk_LOGICAL2)                        :: val (:,:,:,:,:,:)
#else
  LOGICAL(kind=iotk_LOGICAL2), intent(out)           :: val (:,:,:,:,:,:)
#endif
  type(iotk_dummytype), optional :: dummy
  logical,        optional, intent(out) :: found
  LOGICAL(kind=iotk_LOGICAL2), optional, intent(in)  :: default (:,:,:,:,:,:)
  logical,        optional, intent(in)  :: eos
  integer,        optional, intent(out) :: ierr
  integer :: ierrl
  integer :: attlen,pos,equal,namlen
  character :: delim
  logical :: foundl
  character(iotk_vallenx) :: valc
# 351 "iotk_attr.spp"
  integer :: index
  LOGICAL(kind=iotk_LOGICAL2), allocatable :: tmpval (:)
# 354 "iotk_attr.spp"
  ierrl = 0
  attlen=iotk_strlen(attr)
  namlen=iotk_strlen_trim(name)
  foundl = .false.
  equal = 0
  do
    pos = verify(attr(equal+1:attlen)," ")
    if(pos<=0) exit
    equal = equal + pos
    pos = scan(attr(equal+1:attlen),"=")
    if(pos<=0) then
      call iotk_error_issue(ierrl,"iotk_scan_attr",__FILE__,__LINE__)
# 365 "iotk_attr.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.20 ")
# 365 "iotk_attr.spp"
call iotk_error_msg(ierrl,'')
# 365 "iotk_attr.spp"
call iotk_error_write(ierrl,"attr",attr(equal+1:attlen))
      goto 1
    end if
    equal = equal + pos
    if(trim(attr(equal-pos:equal-1))==name(1:namlen)) foundl = .true.
    pos = verify(attr(equal+1:attlen)," ")
    if(pos<=0) then
      call iotk_error_issue(ierrl,"iotk_scan_attr",__FILE__,__LINE__)
# 372 "iotk_attr.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.20 ")
      goto 1
    end if
    equal = equal + pos
    delim = attr(equal:equal)
    if(delim/="'" .and. delim/='"') then
      call iotk_error_issue(ierrl,"iotk_scan_attr",__FILE__,__LINE__)
# 378 "iotk_attr.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.20 ")
      goto 1
    end if
    pos = scan(attr(equal+1:attlen),delim)
    if(pos<=0) then
      call iotk_error_issue(ierrl,"iotk_scan_attr",__FILE__,__LINE__)
# 383 "iotk_attr.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.20 ")
      goto 1
    end if
    if(foundl) exit
    equal = equal + pos
  end do
  if(foundl) then
    call iotk_strcpy(valc,attr(equal+1:equal+pos-1),ierrl)
    if(ierrl/=0) then
      call iotk_error_issue(ierrl,"iotk_scan_attr",__FILE__,__LINE__)
# 392 "iotk_attr.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.20 ")
      goto 1
    end if
  else
    goto 1
  end if
# 413 "iotk_attr.spp"
  allocate(tmpval(size(val)))
  index = 0
  call iotk_str_clean(valc)
  call iotk_read(tmpval,valc(1:iotk_strlen(valc)),index,ierrl)
  if(ierrl/=0) then
    call iotk_error_issue(ierrl,"iotk_scan_attr",__FILE__,__LINE__)
# 418 "iotk_attr.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.20 ")
    goto 1
  end if
# 424 "iotk_attr.spp"
  if(index/=size(val)) then
# 426 "iotk_attr.spp"
    call iotk_error_issue(ierrl,"iotk_scan_attr",__FILE__,__LINE__)
# 426 "iotk_attr.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.20 ")
# 426 "iotk_attr.spp"
call iotk_error_msg(ierrl,'Attribute size does not match')
# 426 "iotk_attr.spp"
call iotk_error_write(ierrl,"attr",valc)
# 426 "iotk_attr.spp"
call iotk_error_write(ierrl,"size",size(tmpval))
    goto 1
  end if
# 432 "iotk_attr.spp"
  val = reshape (source=tmpval,shape=shape(val))
# 434 "iotk_attr.spp"
  deallocate(tmpval)
# 436 "iotk_attr.spp"
1 continue
  if(ierrl/=0) foundl=.false.
  if(present(found)) found = foundl
  if(ierrl==0 .and. .not. present(found) .and. .not. present(default) .and. .not. foundl) then
    call iotk_error_issue(ierrl,"iotk_scan_attr",__FILE__,__LINE__)
# 440 "iotk_attr.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.20 ")
# 440 "iotk_attr.spp"
call iotk_error_msg(ierrl,'Attribute not found')
# 440 "iotk_attr.spp"
call iotk_error_write(ierrl,"name",name)
    ierrl = - ierrl
  end if
  if(present(default) .and. .not. foundl) then
# 453 "iotk_attr.spp"
    val = default
# 455 "iotk_attr.spp"
  end if
  if(present(ierr)) then
    ierr = ierrl
  else
    if(ierrl/=0) call iotk_error_handler(ierrl)
  end if
end subroutine iotk_scan_attr_LOGICAL2_6
# 463 "iotk_attr.spp"

#endif
#endif

subroutine iotk_attr_dummy_LOGICAL2_6
  write(0,*)
end subroutine iotk_attr_dummy_LOGICAL2_6

# 45 "iotk_attr.spp"

# 65 "iotk_attr.spp"


!------------------------------------------------------------------------------!
! Inclusion of configuration file
#include "iotk_config.h"
!------------------------------------------------------------------------------!

# 74 "iotk_attr.spp"
#include "iotk_auxmacros.h"
# 76 "iotk_attr.spp"

# 78 "iotk_attr.spp"

#ifdef __IOTK_LOGICAL2
#if 7 <= __IOTK_MAXRANK

# 158 "iotk_attr.spp"

# 233 "iotk_attr.spp"

# 236 "iotk_attr.spp"
subroutine iotk_write_attr_LOGICAL2_7(attr,name,val,dummy,first,newline,ierr)
  use iotk_base
  use iotk_error_interf
  use iotk_attr_interf
  use iotk_str_interf
  use iotk_misc_interf
  implicit none
  character(*), intent(inout) :: attr
  character(*), intent(in)    :: name
  LOGICAL(kind=iotk_LOGICAL2), intent(in)  :: val (:,:,:,:,:,:,:)
  type(iotk_dummytype), optional :: dummy
  logical, optional, intent(in)  :: first
  logical, optional, intent(in)  :: newline
  integer, optional, intent(out) :: ierr
  integer :: ierrl
  integer :: attlen
  integer :: vallen
  integer :: namlen
  character :: delim
# 258 "iotk_attr.spp"
  character(iotk_vallenx) :: tmpval
  logical :: nl
  if(present(newline)) then
    nl = newline
  else
    nl = .false.
  endif
  ierrl = 0
  if(present(first)) then
    if(first) attr(1:1) = iotk_eos
  end if
  attlen = iotk_strlen_trim(attr)
  namlen = iotk_strlen_trim(name)
  if(.not.iotk_check_name(name)) then
    call iotk_error_issue(ierrl,"iotk_write_attr",__FILE__,__LINE__)
# 272 "iotk_attr.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.20 ")
# 272 "iotk_attr.spp"
call iotk_error_msg(ierrl,'Wrong tag name')
# 272 "iotk_attr.spp"
call iotk_error_write(ierrl,"name",name(1:namlen))
    goto 1
  end if
# 289 "iotk_attr.spp"
  delim = '"'
# 293 "iotk_attr.spp"
  call iotk_write(pack(val,mask=.true.),tmpval,ierrl)
# 295 "iotk_attr.spp"
  if(ierrl/=0) then
    call iotk_error_issue(ierrl,"iotk_write_attr",__FILE__,__LINE__)
# 296 "iotk_attr.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.20 ")
    goto 1
  end if
# 300 "iotk_attr.spp"
  vallen = iotk_strlen(tmpval)
  if(attlen+vallen+namlen+5>len(attr)) then
    call iotk_error_issue(ierrl,"iotk_write_attr",__FILE__,__LINE__)
# 302 "iotk_attr.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.20 ")
# 302 "iotk_attr.spp"
call iotk_error_msg(ierrl,'Attribute dummy argument is too short')
    goto 1
  end if
  if(.not. nl) then
    attr(attlen+1:attlen+vallen+namlen+5) = " "//name(1:namlen)//"="//delim//tmpval(1:vallen)//delim//iotk_eos
  else
    attr(attlen+1:attlen+vallen+namlen+len(iotk_newline)+5) &
       = iotk_newline//" "//name(1:namlen)//"="//delim//tmpval(1:vallen)//delim//iotk_eos
  endif
1 continue
  if(present(ierr)) then
    ierr = ierrl
  else
    if(ierrl/=0) call iotk_error_handler(ierrl)
  end if
end subroutine iotk_write_attr_LOGICAL2_7

# 320 "iotk_attr.spp"
subroutine iotk_scan_attr_LOGICAL2_7(attr,name,val,dummy,found,default,eos,ierr)
  use iotk_base
  use iotk_error_interf
  use iotk_attr_interf, only : iotk_read
  use iotk_str_interf
  use iotk_misc_interf
  implicit none
  character(*),             intent(in)  :: attr
  character(*),             intent(in)  :: name
#ifdef __IOTK_WORKAROUND6
  LOGICAL(kind=iotk_LOGICAL2)                        :: val (:,:,:,:,:,:,:)
#else
  LOGICAL(kind=iotk_LOGICAL2), intent(out)           :: val (:,:,:,:,:,:,:)
#endif
  type(iotk_dummytype), optional :: dummy
  logical,        optional, intent(out) :: found
  LOGICAL(kind=iotk_LOGICAL2), optional, intent(in)  :: default (:,:,:,:,:,:,:)
  logical,        optional, intent(in)  :: eos
  integer,        optional, intent(out) :: ierr
  integer :: ierrl
  integer :: attlen,pos,equal,namlen
  character :: delim
  logical :: foundl
  character(iotk_vallenx) :: valc
# 351 "iotk_attr.spp"
  integer :: index
  LOGICAL(kind=iotk_LOGICAL2), allocatable :: tmpval (:)
# 354 "iotk_attr.spp"
  ierrl = 0
  attlen=iotk_strlen(attr)
  namlen=iotk_strlen_trim(name)
  foundl = .false.
  equal = 0
  do
    pos = verify(attr(equal+1:attlen)," ")
    if(pos<=0) exit
    equal = equal + pos
    pos = scan(attr(equal+1:attlen),"=")
    if(pos<=0) then
      call iotk_error_issue(ierrl,"iotk_scan_attr",__FILE__,__LINE__)
# 365 "iotk_attr.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.20 ")
# 365 "iotk_attr.spp"
call iotk_error_msg(ierrl,'')
# 365 "iotk_attr.spp"
call iotk_error_write(ierrl,"attr",attr(equal+1:attlen))
      goto 1
    end if
    equal = equal + pos
    if(trim(attr(equal-pos:equal-1))==name(1:namlen)) foundl = .true.
    pos = verify(attr(equal+1:attlen)," ")
    if(pos<=0) then
      call iotk_error_issue(ierrl,"iotk_scan_attr",__FILE__,__LINE__)
# 372 "iotk_attr.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.20 ")
      goto 1
    end if
    equal = equal + pos
    delim = attr(equal:equal)
    if(delim/="'" .and. delim/='"') then
      call iotk_error_issue(ierrl,"iotk_scan_attr",__FILE__,__LINE__)
# 378 "iotk_attr.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.20 ")
      goto 1
    end if
    pos = scan(attr(equal+1:attlen),delim)
    if(pos<=0) then
      call iotk_error_issue(ierrl,"iotk_scan_attr",__FILE__,__LINE__)
# 383 "iotk_attr.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.20 ")
      goto 1
    end if
    if(foundl) exit
    equal = equal + pos
  end do
  if(foundl) then
    call iotk_strcpy(valc,attr(equal+1:equal+pos-1),ierrl)
    if(ierrl/=0) then
      call iotk_error_issue(ierrl,"iotk_scan_attr",__FILE__,__LINE__)
# 392 "iotk_attr.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.20 ")
      goto 1
    end if
  else
    goto 1
  end if
# 413 "iotk_attr.spp"
  allocate(tmpval(size(val)))
  index = 0
  call iotk_str_clean(valc)
  call iotk_read(tmpval,valc(1:iotk_strlen(valc)),index,ierrl)
  if(ierrl/=0) then
    call iotk_error_issue(ierrl,"iotk_scan_attr",__FILE__,__LINE__)
# 418 "iotk_attr.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.20 ")
    goto 1
  end if
# 424 "iotk_attr.spp"
  if(index/=size(val)) then
# 426 "iotk_attr.spp"
    call iotk_error_issue(ierrl,"iotk_scan_attr",__FILE__,__LINE__)
# 426 "iotk_attr.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.20 ")
# 426 "iotk_attr.spp"
call iotk_error_msg(ierrl,'Attribute size does not match')
# 426 "iotk_attr.spp"
call iotk_error_write(ierrl,"attr",valc)
# 426 "iotk_attr.spp"
call iotk_error_write(ierrl,"size",size(tmpval))
    goto 1
  end if
# 432 "iotk_attr.spp"
  val = reshape (source=tmpval,shape=shape(val))
# 434 "iotk_attr.spp"
  deallocate(tmpval)
# 436 "iotk_attr.spp"
1 continue
  if(ierrl/=0) foundl=.false.
  if(present(found)) found = foundl
  if(ierrl==0 .and. .not. present(found) .and. .not. present(default) .and. .not. foundl) then
    call iotk_error_issue(ierrl,"iotk_scan_attr",__FILE__,__LINE__)
# 440 "iotk_attr.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.20 ")
# 440 "iotk_attr.spp"
call iotk_error_msg(ierrl,'Attribute not found')
# 440 "iotk_attr.spp"
call iotk_error_write(ierrl,"name",name)
    ierrl = - ierrl
  end if
  if(present(default) .and. .not. foundl) then
# 453 "iotk_attr.spp"
    val = default
# 455 "iotk_attr.spp"
  end if
  if(present(ierr)) then
    ierr = ierrl
  else
    if(ierrl/=0) call iotk_error_handler(ierrl)
  end if
end subroutine iotk_scan_attr_LOGICAL2_7
# 463 "iotk_attr.spp"

#endif
#endif

subroutine iotk_attr_dummy_LOGICAL2_7
  write(0,*)
end subroutine iotk_attr_dummy_LOGICAL2_7

# 45 "iotk_attr.spp"

# 48 "iotk_attr.spp"
! Input/Output Tool Kit (IOTK)
! Copyright (C) 2004-2006 Giovanni Bussi
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
# 65 "iotk_attr.spp"


!------------------------------------------------------------------------------!
! Inclusion of configuration file
#include "iotk_config.h"
!------------------------------------------------------------------------------!

# 74 "iotk_attr.spp"
#include "iotk_auxmacros.h"
# 76 "iotk_attr.spp"

# 78 "iotk_attr.spp"

#ifdef __IOTK_LOGICAL3
#if 0 <= __IOTK_MAXRANK

# 83 "iotk_attr.spp"
! This is needed as a workaround for bugged pack 
subroutine iotk_private_pack_LOGICAL3(out,in,n,l)
    use iotk_base
    implicit none
    integer,                                    intent(in)  :: n,l
# 92 "iotk_attr.spp"
    LOGICAL (kind=iotk_LOGICAL3), intent(out) :: out(n)
    LOGICAL (kind=iotk_LOGICAL3), intent(in)  :: in(n)
# 95 "iotk_attr.spp"
    out = in
end subroutine iotk_private_pack_LOGICAL3

# 100 "iotk_attr.spp"
subroutine iotk_write_LOGICAL3(val,string,ierr)
  use iotk_base
  use iotk_error_interf
  use iotk_xtox_interf
  use iotk_fmt_interf
  use iotk_str_interf
  use iotk_misc_interf
  implicit none
  LOGICAL(kind=iotk_LOGICAL3), intent(in) :: val(:)
#ifdef __IOTK_WORKAROUND6
  character(len=*)              :: string
#else
  character(len=*), intent(out) :: string
#endif
  integer, intent(out) :: ierr
# 118 "iotk_attr.spp"
  integer :: index,iostat
  ierr = 0
  iostat = 0 
  string(1:1) = iotk_eos
  if(size(val)==0) return
  if(len(string)==0) then
    call iotk_error_issue(ierr,"iotk_write",__FILE__,__LINE__)
# 124 "iotk_attr.spp"
call iotk_error_msg(ierr,"CVS Revision: 1.20 ")
    return
  end if
  do index=1,size(val)
# 129 "iotk_attr.spp"
    call iotk_strcat(string,iotk_ltoa(val(index))//" ",ierr)
    if(ierr/=0) then
      call iotk_error_issue(ierr,"iotk_write",__FILE__,__LINE__)
# 131 "iotk_attr.spp"
call iotk_error_msg(ierr,"CVS Revision: 1.20 ")
      return
    end if
# 152 "iotk_attr.spp"
  end do
! the last blank is deleted
  string(iotk_strlen(string):iotk_strlen(string)) = iotk_eos
end subroutine iotk_write_LOGICAL3
# 158 "iotk_attr.spp"

# 162 "iotk_attr.spp"
subroutine iotk_read_LOGICAL3(val,string,index,ierr)
  use iotk_base
  use iotk_error_interf
  use iotk_xtox_interf
  use iotk_misc_interf
  implicit none
  LOGICAL(kind=iotk_LOGICAL3), intent(inout) :: val(:)
  character(len=*), intent(in) :: string
  integer, intent(inout) :: index
  integer, intent(out) :: ierr
# 173 "iotk_attr.spp"
  logical :: check
# 175 "iotk_attr.spp"
  integer :: pos,pos1,iostat
  integer :: maxindex
# 181 "iotk_attr.spp"
  pos = 0
  pos1= 0
  ierr = 0
  iostat = 0
# 188 "iotk_attr.spp"
    maxindex = size(val)
# 190 "iotk_attr.spp"
! for the moment, commas are considered as blanks
  do
    pos = verify(string(pos1+1:)," ,")+pos1
    if(pos==pos1) exit
    pos = pos - 1
    pos1 = scan(string(pos+1:)," ,")+pos
    if(pos1==pos) pos1 = len(string) + 1
!READ string(pos+1:pos1-1)
    index = index+1
    if(index>maxindex) then
      call iotk_error_issue(ierr,"iotk_read",__FILE__,__LINE__)
# 200 "iotk_attr.spp"
call iotk_error_msg(ierr,"CVS Revision: 1.20 ")
# 200 "iotk_attr.spp"
call iotk_error_msg(ierr,'Too many data')
    end if
# 205 "iotk_attr.spp"
    val(index) = iotk_atol(string(pos+1:pos1-1),check=check)
# 218 "iotk_attr.spp"
    if(.not.check) then
      call iotk_error_issue(ierr,"iotk_read",__FILE__,__LINE__)
# 219 "iotk_attr.spp"
call iotk_error_msg(ierr,"CVS Revision: 1.20 ")
# 219 "iotk_attr.spp"
call iotk_error_msg(ierr,'Wrong string')
# 219 "iotk_attr.spp"
call iotk_error_write(ierr,"string",string(pos+1:pos1-1))
      return
    end if
# 228 "iotk_attr.spp"
    if(pos1>=len(string)) exit
  end do
end subroutine iotk_read_LOGICAL3
# 233 "iotk_attr.spp"

# 236 "iotk_attr.spp"
subroutine iotk_write_attr_LOGICAL3_0(attr,name,val,dummy,first,newline,ierr)
  use iotk_base
  use iotk_error_interf
  use iotk_attr_interf
  use iotk_str_interf
  use iotk_misc_interf
  implicit none
  character(*), intent(inout) :: attr
  character(*), intent(in)    :: name
  LOGICAL(kind=iotk_LOGICAL3), intent(in)  :: val 
  type(iotk_dummytype), optional :: dummy
  logical, optional, intent(in)  :: first
  logical, optional, intent(in)  :: newline
  integer, optional, intent(out) :: ierr
  integer :: ierrl
  integer :: attlen
  integer :: vallen
  integer :: namlen
  character :: delim
# 258 "iotk_attr.spp"
  character(iotk_vallenx) :: tmpval
  logical :: nl
  if(present(newline)) then
    nl = newline
  else
    nl = .false.
  endif
  ierrl = 0
  if(present(first)) then
    if(first) attr(1:1) = iotk_eos
  end if
  attlen = iotk_strlen_trim(attr)
  namlen = iotk_strlen_trim(name)
  if(.not.iotk_check_name(name)) then
    call iotk_error_issue(ierrl,"iotk_write_attr",__FILE__,__LINE__)
# 272 "iotk_attr.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.20 ")
# 272 "iotk_attr.spp"
call iotk_error_msg(ierrl,'Wrong tag name')
# 272 "iotk_attr.spp"
call iotk_error_write(ierrl,"name",name(1:namlen))
    goto 1
  end if
# 289 "iotk_attr.spp"
  delim = '"'
# 291 "iotk_attr.spp"
  call iotk_write((/val/),tmpval,ierrl)
# 295 "iotk_attr.spp"
  if(ierrl/=0) then
    call iotk_error_issue(ierrl,"iotk_write_attr",__FILE__,__LINE__)
# 296 "iotk_attr.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.20 ")
    goto 1
  end if
# 300 "iotk_attr.spp"
  vallen = iotk_strlen(tmpval)
  if(attlen+vallen+namlen+5>len(attr)) then
    call iotk_error_issue(ierrl,"iotk_write_attr",__FILE__,__LINE__)
# 302 "iotk_attr.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.20 ")
# 302 "iotk_attr.spp"
call iotk_error_msg(ierrl,'Attribute dummy argument is too short')
    goto 1
  end if
  if(.not. nl) then
    attr(attlen+1:attlen+vallen+namlen+5) = " "//name(1:namlen)//"="//delim//tmpval(1:vallen)//delim//iotk_eos
  else
    attr(attlen+1:attlen+vallen+namlen+len(iotk_newline)+5) &
       = iotk_newline//" "//name(1:namlen)//"="//delim//tmpval(1:vallen)//delim//iotk_eos
  endif
1 continue
  if(present(ierr)) then
    ierr = ierrl
  else
    if(ierrl/=0) call iotk_error_handler(ierrl)
  end if
end subroutine iotk_write_attr_LOGICAL3_0

# 320 "iotk_attr.spp"
subroutine iotk_scan_attr_LOGICAL3_0(attr,name,val,dummy,found,default,eos,ierr)
  use iotk_base
  use iotk_error_interf
  use iotk_attr_interf, only : iotk_read
  use iotk_str_interf
  use iotk_misc_interf
  implicit none
  character(*),             intent(in)  :: attr
  character(*),             intent(in)  :: name
#ifdef __IOTK_WORKAROUND6
  LOGICAL(kind=iotk_LOGICAL3)                        :: val 
#else
  LOGICAL(kind=iotk_LOGICAL3), intent(out)           :: val 
#endif
  type(iotk_dummytype), optional :: dummy
  logical,        optional, intent(out) :: found
  LOGICAL(kind=iotk_LOGICAL3), optional, intent(in)  :: default 
  logical,        optional, intent(in)  :: eos
  integer,        optional, intent(out) :: ierr
  integer :: ierrl
  integer :: attlen,pos,equal,namlen
  character :: delim
  logical :: foundl
  character(iotk_vallenx) :: valc
# 351 "iotk_attr.spp"
  integer :: index
  LOGICAL(kind=iotk_LOGICAL3), allocatable :: tmpval (:)
# 354 "iotk_attr.spp"
  ierrl = 0
  attlen=iotk_strlen(attr)
  namlen=iotk_strlen_trim(name)
  foundl = .false.
  equal = 0
  do
    pos = verify(attr(equal+1:attlen)," ")
    if(pos<=0) exit
    equal = equal + pos
    pos = scan(attr(equal+1:attlen),"=")
    if(pos<=0) then
      call iotk_error_issue(ierrl,"iotk_scan_attr",__FILE__,__LINE__)
# 365 "iotk_attr.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.20 ")
# 365 "iotk_attr.spp"
call iotk_error_msg(ierrl,'')
# 365 "iotk_attr.spp"
call iotk_error_write(ierrl,"attr",attr(equal+1:attlen))
      goto 1
    end if
    equal = equal + pos
    if(trim(attr(equal-pos:equal-1))==name(1:namlen)) foundl = .true.
    pos = verify(attr(equal+1:attlen)," ")
    if(pos<=0) then
      call iotk_error_issue(ierrl,"iotk_scan_attr",__FILE__,__LINE__)
# 372 "iotk_attr.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.20 ")
      goto 1
    end if
    equal = equal + pos
    delim = attr(equal:equal)
    if(delim/="'" .and. delim/='"') then
      call iotk_error_issue(ierrl,"iotk_scan_attr",__FILE__,__LINE__)
# 378 "iotk_attr.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.20 ")
      goto 1
    end if
    pos = scan(attr(equal+1:attlen),delim)
    if(pos<=0) then
      call iotk_error_issue(ierrl,"iotk_scan_attr",__FILE__,__LINE__)
# 383 "iotk_attr.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.20 ")
      goto 1
    end if
    if(foundl) exit
    equal = equal + pos
  end do
  if(foundl) then
    call iotk_strcpy(valc,attr(equal+1:equal+pos-1),ierrl)
    if(ierrl/=0) then
      call iotk_error_issue(ierrl,"iotk_scan_attr",__FILE__,__LINE__)
# 392 "iotk_attr.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.20 ")
      goto 1
    end if
  else
    goto 1
  end if
# 413 "iotk_attr.spp"
  allocate(tmpval(1))
  index = 0
  call iotk_str_clean(valc)
  call iotk_read(tmpval,valc(1:iotk_strlen(valc)),index,ierrl)
  if(ierrl/=0) then
    call iotk_error_issue(ierrl,"iotk_scan_attr",__FILE__,__LINE__)
# 418 "iotk_attr.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.20 ")
    goto 1
  end if
# 424 "iotk_attr.spp"
  if(index/=1) then
# 426 "iotk_attr.spp"
    call iotk_error_issue(ierrl,"iotk_scan_attr",__FILE__,__LINE__)
# 426 "iotk_attr.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.20 ")
# 426 "iotk_attr.spp"
call iotk_error_msg(ierrl,'Attribute size does not match')
# 426 "iotk_attr.spp"
call iotk_error_write(ierrl,"attr",valc)
# 426 "iotk_attr.spp"
call iotk_error_write(ierrl,"size",size(tmpval))
    goto 1
  end if
# 430 "iotk_attr.spp"
  val = tmpval(1)
# 434 "iotk_attr.spp"
  deallocate(tmpval)
# 436 "iotk_attr.spp"
1 continue
  if(ierrl/=0) foundl=.false.
  if(present(found)) found = foundl
  if(ierrl==0 .and. .not. present(found) .and. .not. present(default) .and. .not. foundl) then
    call iotk_error_issue(ierrl,"iotk_scan_attr",__FILE__,__LINE__)
# 440 "iotk_attr.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.20 ")
# 440 "iotk_attr.spp"
call iotk_error_msg(ierrl,'Attribute not found')
# 440 "iotk_attr.spp"
call iotk_error_write(ierrl,"name",name)
    ierrl = - ierrl
  end if
  if(present(default) .and. .not. foundl) then
# 453 "iotk_attr.spp"
    val = default
# 455 "iotk_attr.spp"
  end if
  if(present(ierr)) then
    ierr = ierrl
  else
    if(ierrl/=0) call iotk_error_handler(ierrl)
  end if
end subroutine iotk_scan_attr_LOGICAL3_0
# 463 "iotk_attr.spp"

#endif
#endif

subroutine iotk_attr_dummy_LOGICAL3_0
  write(0,*)
end subroutine iotk_attr_dummy_LOGICAL3_0

# 45 "iotk_attr.spp"

# 65 "iotk_attr.spp"


!------------------------------------------------------------------------------!
! Inclusion of configuration file
#include "iotk_config.h"
!------------------------------------------------------------------------------!

# 74 "iotk_attr.spp"
#include "iotk_auxmacros.h"
# 76 "iotk_attr.spp"

# 78 "iotk_attr.spp"

#ifdef __IOTK_LOGICAL3
#if 1 <= __IOTK_MAXRANK

# 158 "iotk_attr.spp"

# 233 "iotk_attr.spp"

# 236 "iotk_attr.spp"
subroutine iotk_write_attr_LOGICAL3_1(attr,name,val,dummy,first,newline,ierr)
  use iotk_base
  use iotk_error_interf
  use iotk_attr_interf
  use iotk_str_interf
  use iotk_misc_interf
  implicit none
  character(*), intent(inout) :: attr
  character(*), intent(in)    :: name
  LOGICAL(kind=iotk_LOGICAL3), intent(in)  :: val (:)
  type(iotk_dummytype), optional :: dummy
  logical, optional, intent(in)  :: first
  logical, optional, intent(in)  :: newline
  integer, optional, intent(out) :: ierr
  integer :: ierrl
  integer :: attlen
  integer :: vallen
  integer :: namlen
  character :: delim
# 258 "iotk_attr.spp"
  character(iotk_vallenx) :: tmpval
  logical :: nl
  if(present(newline)) then
    nl = newline
  else
    nl = .false.
  endif
  ierrl = 0
  if(present(first)) then
    if(first) attr(1:1) = iotk_eos
  end if
  attlen = iotk_strlen_trim(attr)
  namlen = iotk_strlen_trim(name)
  if(.not.iotk_check_name(name)) then
    call iotk_error_issue(ierrl,"iotk_write_attr",__FILE__,__LINE__)
# 272 "iotk_attr.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.20 ")
# 272 "iotk_attr.spp"
call iotk_error_msg(ierrl,'Wrong tag name')
# 272 "iotk_attr.spp"
call iotk_error_write(ierrl,"name",name(1:namlen))
    goto 1
  end if
# 289 "iotk_attr.spp"
  delim = '"'
# 293 "iotk_attr.spp"
  call iotk_write(pack(val,mask=.true.),tmpval,ierrl)
# 295 "iotk_attr.spp"
  if(ierrl/=0) then
    call iotk_error_issue(ierrl,"iotk_write_attr",__FILE__,__LINE__)
# 296 "iotk_attr.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.20 ")
    goto 1
  end if
# 300 "iotk_attr.spp"
  vallen = iotk_strlen(tmpval)
  if(attlen+vallen+namlen+5>len(attr)) then
    call iotk_error_issue(ierrl,"iotk_write_attr",__FILE__,__LINE__)
# 302 "iotk_attr.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.20 ")
# 302 "iotk_attr.spp"
call iotk_error_msg(ierrl,'Attribute dummy argument is too short')
    goto 1
  end if
  if(.not. nl) then
    attr(attlen+1:attlen+vallen+namlen+5) = " "//name(1:namlen)//"="//delim//tmpval(1:vallen)//delim//iotk_eos
  else
    attr(attlen+1:attlen+vallen+namlen+len(iotk_newline)+5) &
       = iotk_newline//" "//name(1:namlen)//"="//delim//tmpval(1:vallen)//delim//iotk_eos
  endif
1 continue
  if(present(ierr)) then
    ierr = ierrl
  else
    if(ierrl/=0) call iotk_error_handler(ierrl)
  end if
end subroutine iotk_write_attr_LOGICAL3_1

# 320 "iotk_attr.spp"
subroutine iotk_scan_attr_LOGICAL3_1(attr,name,val,dummy,found,default,eos,ierr)
  use iotk_base
  use iotk_error_interf
  use iotk_attr_interf, only : iotk_read
  use iotk_str_interf
  use iotk_misc_interf
  implicit none
  character(*),             intent(in)  :: attr
  character(*),             intent(in)  :: name
#ifdef __IOTK_WORKAROUND6
  LOGICAL(kind=iotk_LOGICAL3)                        :: val (:)
#else
  LOGICAL(kind=iotk_LOGICAL3), intent(out)           :: val (:)
#endif
  type(iotk_dummytype), optional :: dummy
  logical,        optional, intent(out) :: found
  LOGICAL(kind=iotk_LOGICAL3), optional, intent(in)  :: default (:)
  logical,        optional, intent(in)  :: eos
  integer,        optional, intent(out) :: ierr
  integer :: ierrl
  integer :: attlen,pos,equal,namlen
  character :: delim
  logical :: foundl
  character(iotk_vallenx) :: valc
# 351 "iotk_attr.spp"
  integer :: index
  LOGICAL(kind=iotk_LOGICAL3), allocatable :: tmpval (:)
# 354 "iotk_attr.spp"
  ierrl = 0
  attlen=iotk_strlen(attr)
  namlen=iotk_strlen_trim(name)
  foundl = .false.
  equal = 0
  do
    pos = verify(attr(equal+1:attlen)," ")
    if(pos<=0) exit
    equal = equal + pos
    pos = scan(attr(equal+1:attlen),"=")
    if(pos<=0) then
      call iotk_error_issue(ierrl,"iotk_scan_attr",__FILE__,__LINE__)
# 365 "iotk_attr.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.20 ")
# 365 "iotk_attr.spp"
call iotk_error_msg(ierrl,'')
# 365 "iotk_attr.spp"
call iotk_error_write(ierrl,"attr",attr(equal+1:attlen))
      goto 1
    end if
    equal = equal + pos
    if(trim(attr(equal-pos:equal-1))==name(1:namlen)) foundl = .true.
    pos = verify(attr(equal+1:attlen)," ")
    if(pos<=0) then
      call iotk_error_issue(ierrl,"iotk_scan_attr",__FILE__,__LINE__)
# 372 "iotk_attr.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.20 ")
      goto 1
    end if
    equal = equal + pos
    delim = attr(equal:equal)
    if(delim/="'" .and. delim/='"') then
      call iotk_error_issue(ierrl,"iotk_scan_attr",__FILE__,__LINE__)
# 378 "iotk_attr.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.20 ")
      goto 1
    end if
    pos = scan(attr(equal+1:attlen),delim)
    if(pos<=0) then
      call iotk_error_issue(ierrl,"iotk_scan_attr",__FILE__,__LINE__)
# 383 "iotk_attr.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.20 ")
      goto 1
    end if
    if(foundl) exit
    equal = equal + pos
  end do
  if(foundl) then
    call iotk_strcpy(valc,attr(equal+1:equal+pos-1),ierrl)
    if(ierrl/=0) then
      call iotk_error_issue(ierrl,"iotk_scan_attr",__FILE__,__LINE__)
# 392 "iotk_attr.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.20 ")
      goto 1
    end if
  else
    goto 1
  end if
# 413 "iotk_attr.spp"
  allocate(tmpval(size(val)))
  index = 0
  call iotk_str_clean(valc)
  call iotk_read(tmpval,valc(1:iotk_strlen(valc)),index,ierrl)
  if(ierrl/=0) then
    call iotk_error_issue(ierrl,"iotk_scan_attr",__FILE__,__LINE__)
# 418 "iotk_attr.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.20 ")
    goto 1
  end if
# 424 "iotk_attr.spp"
  if(index/=size(val)) then
# 426 "iotk_attr.spp"
    call iotk_error_issue(ierrl,"iotk_scan_attr",__FILE__,__LINE__)
# 426 "iotk_attr.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.20 ")
# 426 "iotk_attr.spp"
call iotk_error_msg(ierrl,'Attribute size does not match')
# 426 "iotk_attr.spp"
call iotk_error_write(ierrl,"attr",valc)
# 426 "iotk_attr.spp"
call iotk_error_write(ierrl,"size",size(tmpval))
    goto 1
  end if
# 432 "iotk_attr.spp"
  val = reshape (source=tmpval,shape=shape(val))
# 434 "iotk_attr.spp"
  deallocate(tmpval)
# 436 "iotk_attr.spp"
1 continue
  if(ierrl/=0) foundl=.false.
  if(present(found)) found = foundl
  if(ierrl==0 .and. .not. present(found) .and. .not. present(default) .and. .not. foundl) then
    call iotk_error_issue(ierrl,"iotk_scan_attr",__FILE__,__LINE__)
# 440 "iotk_attr.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.20 ")
# 440 "iotk_attr.spp"
call iotk_error_msg(ierrl,'Attribute not found')
# 440 "iotk_attr.spp"
call iotk_error_write(ierrl,"name",name)
    ierrl = - ierrl
  end if
  if(present(default) .and. .not. foundl) then
# 453 "iotk_attr.spp"
    val = default
# 455 "iotk_attr.spp"
  end if
  if(present(ierr)) then
    ierr = ierrl
  else
    if(ierrl/=0) call iotk_error_handler(ierrl)
  end if
end subroutine iotk_scan_attr_LOGICAL3_1
# 463 "iotk_attr.spp"

#endif
#endif

subroutine iotk_attr_dummy_LOGICAL3_1
  write(0,*)
end subroutine iotk_attr_dummy_LOGICAL3_1

# 45 "iotk_attr.spp"

# 65 "iotk_attr.spp"


!------------------------------------------------------------------------------!
! Inclusion of configuration file
#include "iotk_config.h"
!------------------------------------------------------------------------------!

# 74 "iotk_attr.spp"
#include "iotk_auxmacros.h"
# 76 "iotk_attr.spp"

# 78 "iotk_attr.spp"

#ifdef __IOTK_LOGICAL3
#if 2 <= __IOTK_MAXRANK

# 158 "iotk_attr.spp"

# 233 "iotk_attr.spp"

# 236 "iotk_attr.spp"
subroutine iotk_write_attr_LOGICAL3_2(attr,name,val,dummy,first,newline,ierr)
  use iotk_base
  use iotk_error_interf
  use iotk_attr_interf
  use iotk_str_interf
  use iotk_misc_interf
  implicit none
  character(*), intent(inout) :: attr
  character(*), intent(in)    :: name
  LOGICAL(kind=iotk_LOGICAL3), intent(in)  :: val (:,:)
  type(iotk_dummytype), optional :: dummy
  logical, optional, intent(in)  :: first
  logical, optional, intent(in)  :: newline
  integer, optional, intent(out) :: ierr
  integer :: ierrl
  integer :: attlen
  integer :: vallen
  integer :: namlen
  character :: delim
# 258 "iotk_attr.spp"
  character(iotk_vallenx) :: tmpval
  logical :: nl
  if(present(newline)) then
    nl = newline
  else
    nl = .false.
  endif
  ierrl = 0
  if(present(first)) then
    if(first) attr(1:1) = iotk_eos
  end if
  attlen = iotk_strlen_trim(attr)
  namlen = iotk_strlen_trim(name)
  if(.not.iotk_check_name(name)) then
    call iotk_error_issue(ierrl,"iotk_write_attr",__FILE__,__LINE__)
# 272 "iotk_attr.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.20 ")
# 272 "iotk_attr.spp"
call iotk_error_msg(ierrl,'Wrong tag name')
# 272 "iotk_attr.spp"
call iotk_error_write(ierrl,"name",name(1:namlen))
    goto 1
  end if
# 289 "iotk_attr.spp"
  delim = '"'
# 293 "iotk_attr.spp"
  call iotk_write(pack(val,mask=.true.),tmpval,ierrl)
# 295 "iotk_attr.spp"
  if(ierrl/=0) then
    call iotk_error_issue(ierrl,"iotk_write_attr",__FILE__,__LINE__)
# 296 "iotk_attr.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.20 ")
    goto 1
  end if
# 300 "iotk_attr.spp"
  vallen = iotk_strlen(tmpval)
  if(attlen+vallen+namlen+5>len(attr)) then
    call iotk_error_issue(ierrl,"iotk_write_attr",__FILE__,__LINE__)
# 302 "iotk_attr.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.20 ")
# 302 "iotk_attr.spp"
call iotk_error_msg(ierrl,'Attribute dummy argument is too short')
    goto 1
  end if
  if(.not. nl) then
    attr(attlen+1:attlen+vallen+namlen+5) = " "//name(1:namlen)//"="//delim//tmpval(1:vallen)//delim//iotk_eos
  else
    attr(attlen+1:attlen+vallen+namlen+len(iotk_newline)+5) &
       = iotk_newline//" "//name(1:namlen)//"="//delim//tmpval(1:vallen)//delim//iotk_eos
  endif
1 continue
  if(present(ierr)) then
    ierr = ierrl
  else
    if(ierrl/=0) call iotk_error_handler(ierrl)
  end if
end subroutine iotk_write_attr_LOGICAL3_2

# 320 "iotk_attr.spp"
subroutine iotk_scan_attr_LOGICAL3_2(attr,name,val,dummy,found,default,eos,ierr)
  use iotk_base
  use iotk_error_interf
  use iotk_attr_interf, only : iotk_read
  use iotk_str_interf
  use iotk_misc_interf
  implicit none
  character(*),             intent(in)  :: attr
  character(*),             intent(in)  :: name
#ifdef __IOTK_WORKAROUND6
  LOGICAL(kind=iotk_LOGICAL3)                        :: val (:,:)
#else
  LOGICAL(kind=iotk_LOGICAL3), intent(out)           :: val (:,:)
#endif
  type(iotk_dummytype), optional :: dummy
  logical,        optional, intent(out) :: found
  LOGICAL(kind=iotk_LOGICAL3), optional, intent(in)  :: default (:,:)
  logical,        optional, intent(in)  :: eos
  integer,        optional, intent(out) :: ierr
  integer :: ierrl
  integer :: attlen,pos,equal,namlen
  character :: delim
  logical :: foundl
  character(iotk_vallenx) :: valc
# 351 "iotk_attr.spp"
  integer :: index
  LOGICAL(kind=iotk_LOGICAL3), allocatable :: tmpval (:)
# 354 "iotk_attr.spp"
  ierrl = 0
  attlen=iotk_strlen(attr)
  namlen=iotk_strlen_trim(name)
  foundl = .false.
  equal = 0
  do
    pos = verify(attr(equal+1:attlen)," ")
    if(pos<=0) exit
    equal = equal + pos
    pos = scan(attr(equal+1:attlen),"=")
    if(pos<=0) then
      call iotk_error_issue(ierrl,"iotk_scan_attr",__FILE__,__LINE__)
# 365 "iotk_attr.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.20 ")
# 365 "iotk_attr.spp"
call iotk_error_msg(ierrl,'')
# 365 "iotk_attr.spp"
call iotk_error_write(ierrl,"attr",attr(equal+1:attlen))
      goto 1
    end if
    equal = equal + pos
    if(trim(attr(equal-pos:equal-1))==name(1:namlen)) foundl = .true.
    pos = verify(attr(equal+1:attlen)," ")
    if(pos<=0) then
      call iotk_error_issue(ierrl,"iotk_scan_attr",__FILE__,__LINE__)
# 372 "iotk_attr.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.20 ")
      goto 1
    end if
    equal = equal + pos
    delim = attr(equal:equal)
    if(delim/="'" .and. delim/='"') then
      call iotk_error_issue(ierrl,"iotk_scan_attr",__FILE__,__LINE__)
# 378 "iotk_attr.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.20 ")
      goto 1
    end if
    pos = scan(attr(equal+1:attlen),delim)
    if(pos<=0) then
      call iotk_error_issue(ierrl,"iotk_scan_attr",__FILE__,__LINE__)
# 383 "iotk_attr.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.20 ")
      goto 1
    end if
    if(foundl) exit
    equal = equal + pos
  end do
  if(foundl) then
    call iotk_strcpy(valc,attr(equal+1:equal+pos-1),ierrl)
    if(ierrl/=0) then
      call iotk_error_issue(ierrl,"iotk_scan_attr",__FILE__,__LINE__)
# 392 "iotk_attr.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.20 ")
      goto 1
    end if
  else
    goto 1
  end if
# 413 "iotk_attr.spp"
  allocate(tmpval(size(val)))
  index = 0
  call iotk_str_clean(valc)
  call iotk_read(tmpval,valc(1:iotk_strlen(valc)),index,ierrl)
  if(ierrl/=0) then
    call iotk_error_issue(ierrl,"iotk_scan_attr",__FILE__,__LINE__)
# 418 "iotk_attr.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.20 ")
    goto 1
  end if
# 424 "iotk_attr.spp"
  if(index/=size(val)) then
# 426 "iotk_attr.spp"
    call iotk_error_issue(ierrl,"iotk_scan_attr",__FILE__,__LINE__)
# 426 "iotk_attr.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.20 ")
# 426 "iotk_attr.spp"
call iotk_error_msg(ierrl,'Attribute size does not match')
# 426 "iotk_attr.spp"
call iotk_error_write(ierrl,"attr",valc)
# 426 "iotk_attr.spp"
call iotk_error_write(ierrl,"size",size(tmpval))
    goto 1
  end if
# 432 "iotk_attr.spp"
  val = reshape (source=tmpval,shape=shape(val))
# 434 "iotk_attr.spp"
  deallocate(tmpval)
# 436 "iotk_attr.spp"
1 continue
  if(ierrl/=0) foundl=.false.
  if(present(found)) found = foundl
  if(ierrl==0 .and. .not. present(found) .and. .not. present(default) .and. .not. foundl) then
    call iotk_error_issue(ierrl,"iotk_scan_attr",__FILE__,__LINE__)
# 440 "iotk_attr.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.20 ")
# 440 "iotk_attr.spp"
call iotk_error_msg(ierrl,'Attribute not found')
# 440 "iotk_attr.spp"
call iotk_error_write(ierrl,"name",name)
    ierrl = - ierrl
  end if
  if(present(default) .and. .not. foundl) then
# 453 "iotk_attr.spp"
    val = default
# 455 "iotk_attr.spp"
  end if
  if(present(ierr)) then
    ierr = ierrl
  else
    if(ierrl/=0) call iotk_error_handler(ierrl)
  end if
end subroutine iotk_scan_attr_LOGICAL3_2
# 463 "iotk_attr.spp"

#endif
#endif

subroutine iotk_attr_dummy_LOGICAL3_2
  write(0,*)
end subroutine iotk_attr_dummy_LOGICAL3_2

# 45 "iotk_attr.spp"

# 48 "iotk_attr.spp"
! Input/Output Tool Kit (IOTK)
! Copyright (C) 2004-2006 Giovanni Bussi
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
# 65 "iotk_attr.spp"


!------------------------------------------------------------------------------!
! Inclusion of configuration file
#include "iotk_config.h"
!------------------------------------------------------------------------------!

# 74 "iotk_attr.spp"
#include "iotk_auxmacros.h"
# 76 "iotk_attr.spp"

# 78 "iotk_attr.spp"

#ifdef __IOTK_LOGICAL3
#if 3 <= __IOTK_MAXRANK

# 158 "iotk_attr.spp"

# 233 "iotk_attr.spp"

# 236 "iotk_attr.spp"
subroutine iotk_write_attr_LOGICAL3_3(attr,name,val,dummy,first,newline,ierr)
  use iotk_base
  use iotk_error_interf
  use iotk_attr_interf
  use iotk_str_interf
  use iotk_misc_interf
  implicit none
  character(*), intent(inout) :: attr
  character(*), intent(in)    :: name
  LOGICAL(kind=iotk_LOGICAL3), intent(in)  :: val (:,:,:)
  type(iotk_dummytype), optional :: dummy
  logical, optional, intent(in)  :: first
  logical, optional, intent(in)  :: newline
  integer, optional, intent(out) :: ierr
  integer :: ierrl
  integer :: attlen
  integer :: vallen
  integer :: namlen
  character :: delim
# 258 "iotk_attr.spp"
  character(iotk_vallenx) :: tmpval
  logical :: nl
  if(present(newline)) then
    nl = newline
  else
    nl = .false.
  endif
  ierrl = 0
  if(present(first)) then
    if(first) attr(1:1) = iotk_eos
  end if
  attlen = iotk_strlen_trim(attr)
  namlen = iotk_strlen_trim(name)
  if(.not.iotk_check_name(name)) then
    call iotk_error_issue(ierrl,"iotk_write_attr",__FILE__,__LINE__)
# 272 "iotk_attr.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.20 ")
# 272 "iotk_attr.spp"
call iotk_error_msg(ierrl,'Wrong tag name')
# 272 "iotk_attr.spp"
call iotk_error_write(ierrl,"name",name(1:namlen))
    goto 1
  end if
# 289 "iotk_attr.spp"
  delim = '"'
# 293 "iotk_attr.spp"
  call iotk_write(pack(val,mask=.true.),tmpval,ierrl)
# 295 "iotk_attr.spp"
  if(ierrl/=0) then
    call iotk_error_issue(ierrl,"iotk_write_attr",__FILE__,__LINE__)
# 296 "iotk_attr.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.20 ")
    goto 1
  end if
# 300 "iotk_attr.spp"
  vallen = iotk_strlen(tmpval)
  if(attlen+vallen+namlen+5>len(attr)) then
    call iotk_error_issue(ierrl,"iotk_write_attr",__FILE__,__LINE__)
# 302 "iotk_attr.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.20 ")
# 302 "iotk_attr.spp"
call iotk_error_msg(ierrl,'Attribute dummy argument is too short')
    goto 1
  end if
  if(.not. nl) then
    attr(attlen+1:attlen+vallen+namlen+5) = " "//name(1:namlen)//"="//delim//tmpval(1:vallen)//delim//iotk_eos
  else
    attr(attlen+1:attlen+vallen+namlen+len(iotk_newline)+5) &
       = iotk_newline//" "//name(1:namlen)//"="//delim//tmpval(1:vallen)//delim//iotk_eos
  endif
1 continue
  if(present(ierr)) then
    ierr = ierrl
  else
    if(ierrl/=0) call iotk_error_handler(ierrl)
  end if
end subroutine iotk_write_attr_LOGICAL3_3

# 320 "iotk_attr.spp"
subroutine iotk_scan_attr_LOGICAL3_3(attr,name,val,dummy,found,default,eos,ierr)
  use iotk_base
  use iotk_error_interf
  use iotk_attr_interf, only : iotk_read
  use iotk_str_interf
  use iotk_misc_interf
  implicit none
  character(*),             intent(in)  :: attr
  character(*),             intent(in)  :: name
#ifdef __IOTK_WORKAROUND6
  LOGICAL(kind=iotk_LOGICAL3)                        :: val (:,:,:)
#else
  LOGICAL(kind=iotk_LOGICAL3), intent(out)           :: val (:,:,:)
#endif
  type(iotk_dummytype), optional :: dummy
  logical,        optional, intent(out) :: found
  LOGICAL(kind=iotk_LOGICAL3), optional, intent(in)  :: default (:,:,:)
  logical,        optional, intent(in)  :: eos
  integer,        optional, intent(out) :: ierr
  integer :: ierrl
  integer :: attlen,pos,equal,namlen
  character :: delim
  logical :: foundl
  character(iotk_vallenx) :: valc
# 351 "iotk_attr.spp"
  integer :: index
  LOGICAL(kind=iotk_LOGICAL3), allocatable :: tmpval (:)
# 354 "iotk_attr.spp"
  ierrl = 0
  attlen=iotk_strlen(attr)
  namlen=iotk_strlen_trim(name)
  foundl = .false.
  equal = 0
  do
    pos = verify(attr(equal+1:attlen)," ")
    if(pos<=0) exit
    equal = equal + pos
    pos = scan(attr(equal+1:attlen),"=")
    if(pos<=0) then
      call iotk_error_issue(ierrl,"iotk_scan_attr",__FILE__,__LINE__)
# 365 "iotk_attr.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.20 ")
# 365 "iotk_attr.spp"
call iotk_error_msg(ierrl,'')
# 365 "iotk_attr.spp"
call iotk_error_write(ierrl,"attr",attr(equal+1:attlen))
      goto 1
    end if
    equal = equal + pos
    if(trim(attr(equal-pos:equal-1))==name(1:namlen)) foundl = .true.
    pos = verify(attr(equal+1:attlen)," ")
    if(pos<=0) then
      call iotk_error_issue(ierrl,"iotk_scan_attr",__FILE__,__LINE__)
# 372 "iotk_attr.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.20 ")
      goto 1
    end if
    equal = equal + pos
    delim = attr(equal:equal)
    if(delim/="'" .and. delim/='"') then
      call iotk_error_issue(ierrl,"iotk_scan_attr",__FILE__,__LINE__)
# 378 "iotk_attr.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.20 ")
      goto 1
    end if
    pos = scan(attr(equal+1:attlen),delim)
    if(pos<=0) then
      call iotk_error_issue(ierrl,"iotk_scan_attr",__FILE__,__LINE__)
# 383 "iotk_attr.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.20 ")
      goto 1
    end if
    if(foundl) exit
    equal = equal + pos
  end do
  if(foundl) then
    call iotk_strcpy(valc,attr(equal+1:equal+pos-1),ierrl)
    if(ierrl/=0) then
      call iotk_error_issue(ierrl,"iotk_scan_attr",__FILE__,__LINE__)
# 392 "iotk_attr.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.20 ")
      goto 1
    end if
  else
    goto 1
  end if
# 413 "iotk_attr.spp"
  allocate(tmpval(size(val)))
  index = 0
  call iotk_str_clean(valc)
  call iotk_read(tmpval,valc(1:iotk_strlen(valc)),index,ierrl)
  if(ierrl/=0) then
    call iotk_error_issue(ierrl,"iotk_scan_attr",__FILE__,__LINE__)
# 418 "iotk_attr.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.20 ")
    goto 1
  end if
# 424 "iotk_attr.spp"
  if(index/=size(val)) then
# 426 "iotk_attr.spp"
    call iotk_error_issue(ierrl,"iotk_scan_attr",__FILE__,__LINE__)
# 426 "iotk_attr.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.20 ")
# 426 "iotk_attr.spp"
call iotk_error_msg(ierrl,'Attribute size does not match')
# 426 "iotk_attr.spp"
call iotk_error_write(ierrl,"attr",valc)
# 426 "iotk_attr.spp"
call iotk_error_write(ierrl,"size",size(tmpval))
    goto 1
  end if
# 432 "iotk_attr.spp"
  val = reshape (source=tmpval,shape=shape(val))
# 434 "iotk_attr.spp"
  deallocate(tmpval)
# 436 "iotk_attr.spp"
1 continue
  if(ierrl/=0) foundl=.false.
  if(present(found)) found = foundl
  if(ierrl==0 .and. .not. present(found) .and. .not. present(default) .and. .not. foundl) then
    call iotk_error_issue(ierrl,"iotk_scan_attr",__FILE__,__LINE__)
# 440 "iotk_attr.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.20 ")
# 440 "iotk_attr.spp"
call iotk_error_msg(ierrl,'Attribute not found')
# 440 "iotk_attr.spp"
call iotk_error_write(ierrl,"name",name)
    ierrl = - ierrl
  end if
  if(present(default) .and. .not. foundl) then
# 453 "iotk_attr.spp"
    val = default
# 455 "iotk_attr.spp"
  end if
  if(present(ierr)) then
    ierr = ierrl
  else
    if(ierrl/=0) call iotk_error_handler(ierrl)
  end if
end subroutine iotk_scan_attr_LOGICAL3_3
# 463 "iotk_attr.spp"

#endif
#endif

subroutine iotk_attr_dummy_LOGICAL3_3
  write(0,*)
end subroutine iotk_attr_dummy_LOGICAL3_3

# 45 "iotk_attr.spp"

# 65 "iotk_attr.spp"


!------------------------------------------------------------------------------!
! Inclusion of configuration file
#include "iotk_config.h"
!------------------------------------------------------------------------------!

# 74 "iotk_attr.spp"
#include "iotk_auxmacros.h"
# 76 "iotk_attr.spp"

# 78 "iotk_attr.spp"

#ifdef __IOTK_LOGICAL3
#if 4 <= __IOTK_MAXRANK

# 158 "iotk_attr.spp"

# 233 "iotk_attr.spp"

# 236 "iotk_attr.spp"
subroutine iotk_write_attr_LOGICAL3_4(attr,name,val,dummy,first,newline,ierr)
  use iotk_base
  use iotk_error_interf
  use iotk_attr_interf
  use iotk_str_interf
  use iotk_misc_interf
  implicit none
  character(*), intent(inout) :: attr
  character(*), intent(in)    :: name
  LOGICAL(kind=iotk_LOGICAL3), intent(in)  :: val (:,:,:,:)
  type(iotk_dummytype), optional :: dummy
  logical, optional, intent(in)  :: first
  logical, optional, intent(in)  :: newline
  integer, optional, intent(out) :: ierr
  integer :: ierrl
  integer :: attlen
  integer :: vallen
  integer :: namlen
  character :: delim
# 258 "iotk_attr.spp"
  character(iotk_vallenx) :: tmpval
  logical :: nl
  if(present(newline)) then
    nl = newline
  else
    nl = .false.
  endif
  ierrl = 0
  if(present(first)) then
    if(first) attr(1:1) = iotk_eos
  end if
  attlen = iotk_strlen_trim(attr)
  namlen = iotk_strlen_trim(name)
  if(.not.iotk_check_name(name)) then
    call iotk_error_issue(ierrl,"iotk_write_attr",__FILE__,__LINE__)
# 272 "iotk_attr.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.20 ")
# 272 "iotk_attr.spp"
call iotk_error_msg(ierrl,'Wrong tag name')
# 272 "iotk_attr.spp"
call iotk_error_write(ierrl,"name",name(1:namlen))
    goto 1
  end if
# 289 "iotk_attr.spp"
  delim = '"'
# 293 "iotk_attr.spp"
  call iotk_write(pack(val,mask=.true.),tmpval,ierrl)
# 295 "iotk_attr.spp"
  if(ierrl/=0) then
    call iotk_error_issue(ierrl,"iotk_write_attr",__FILE__,__LINE__)
# 296 "iotk_attr.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.20 ")
    goto 1
  end if
# 300 "iotk_attr.spp"
  vallen = iotk_strlen(tmpval)
  if(attlen+vallen+namlen+5>len(attr)) then
    call iotk_error_issue(ierrl,"iotk_write_attr",__FILE__,__LINE__)
# 302 "iotk_attr.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.20 ")
# 302 "iotk_attr.spp"
call iotk_error_msg(ierrl,'Attribute dummy argument is too short')
    goto 1
  end if
  if(.not. nl) then
    attr(attlen+1:attlen+vallen+namlen+5) = " "//name(1:namlen)//"="//delim//tmpval(1:vallen)//delim//iotk_eos
  else
    attr(attlen+1:attlen+vallen+namlen+len(iotk_newline)+5) &
       = iotk_newline//" "//name(1:namlen)//"="//delim//tmpval(1:vallen)//delim//iotk_eos
  endif
1 continue
  if(present(ierr)) then
    ierr = ierrl
  else
    if(ierrl/=0) call iotk_error_handler(ierrl)
  end if
end subroutine iotk_write_attr_LOGICAL3_4

# 320 "iotk_attr.spp"
subroutine iotk_scan_attr_LOGICAL3_4(attr,name,val,dummy,found,default,eos,ierr)
  use iotk_base
  use iotk_error_interf
  use iotk_attr_interf, only : iotk_read
  use iotk_str_interf
  use iotk_misc_interf
  implicit none
  character(*),             intent(in)  :: attr
  character(*),             intent(in)  :: name
#ifdef __IOTK_WORKAROUND6
  LOGICAL(kind=iotk_LOGICAL3)                        :: val (:,:,:,:)
#else
  LOGICAL(kind=iotk_LOGICAL3), intent(out)           :: val (:,:,:,:)
#endif
  type(iotk_dummytype), optional :: dummy
  logical,        optional, intent(out) :: found
  LOGICAL(kind=iotk_LOGICAL3), optional, intent(in)  :: default (:,:,:,:)
  logical,        optional, intent(in)  :: eos
  integer,        optional, intent(out) :: ierr
  integer :: ierrl
  integer :: attlen,pos,equal,namlen
  character :: delim
  logical :: foundl
  character(iotk_vallenx) :: valc
# 351 "iotk_attr.spp"
  integer :: index
  LOGICAL(kind=iotk_LOGICAL3), allocatable :: tmpval (:)
# 354 "iotk_attr.spp"
  ierrl = 0
  attlen=iotk_strlen(attr)
  namlen=iotk_strlen_trim(name)
  foundl = .false.
  equal = 0
  do
    pos = verify(attr(equal+1:attlen)," ")
    if(pos<=0) exit
    equal = equal + pos
    pos = scan(attr(equal+1:attlen),"=")
    if(pos<=0) then
      call iotk_error_issue(ierrl,"iotk_scan_attr",__FILE__,__LINE__)
# 365 "iotk_attr.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.20 ")
# 365 "iotk_attr.spp"
call iotk_error_msg(ierrl,'')
# 365 "iotk_attr.spp"
call iotk_error_write(ierrl,"attr",attr(equal+1:attlen))
      goto 1
    end if
    equal = equal + pos
    if(trim(attr(equal-pos:equal-1))==name(1:namlen)) foundl = .true.
    pos = verify(attr(equal+1:attlen)," ")
    if(pos<=0) then
      call iotk_error_issue(ierrl,"iotk_scan_attr",__FILE__,__LINE__)
# 372 "iotk_attr.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.20 ")
      goto 1
    end if
    equal = equal + pos
    delim = attr(equal:equal)
    if(delim/="'" .and. delim/='"') then
      call iotk_error_issue(ierrl,"iotk_scan_attr",__FILE__,__LINE__)
# 378 "iotk_attr.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.20 ")
      goto 1
    end if
    pos = scan(attr(equal+1:attlen),delim)
    if(pos<=0) then
      call iotk_error_issue(ierrl,"iotk_scan_attr",__FILE__,__LINE__)
# 383 "iotk_attr.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.20 ")
      goto 1
    end if
    if(foundl) exit
    equal = equal + pos
  end do
  if(foundl) then
    call iotk_strcpy(valc,attr(equal+1:equal+pos-1),ierrl)
    if(ierrl/=0) then
      call iotk_error_issue(ierrl,"iotk_scan_attr",__FILE__,__LINE__)
# 392 "iotk_attr.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.20 ")
      goto 1
    end if
  else
    goto 1
  end if
# 413 "iotk_attr.spp"
  allocate(tmpval(size(val)))
  index = 0
  call iotk_str_clean(valc)
  call iotk_read(tmpval,valc(1:iotk_strlen(valc)),index,ierrl)
  if(ierrl/=0) then
    call iotk_error_issue(ierrl,"iotk_scan_attr",__FILE__,__LINE__)
# 418 "iotk_attr.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.20 ")
    goto 1
  end if
# 424 "iotk_attr.spp"
  if(index/=size(val)) then
# 426 "iotk_attr.spp"
    call iotk_error_issue(ierrl,"iotk_scan_attr",__FILE__,__LINE__)
# 426 "iotk_attr.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.20 ")
# 426 "iotk_attr.spp"
call iotk_error_msg(ierrl,'Attribute size does not match')
# 426 "iotk_attr.spp"
call iotk_error_write(ierrl,"attr",valc)
# 426 "iotk_attr.spp"
call iotk_error_write(ierrl,"size",size(tmpval))
    goto 1
  end if
# 432 "iotk_attr.spp"
  val = reshape (source=tmpval,shape=shape(val))
# 434 "iotk_attr.spp"
  deallocate(tmpval)
# 436 "iotk_attr.spp"
1 continue
  if(ierrl/=0) foundl=.false.
  if(present(found)) found = foundl
  if(ierrl==0 .and. .not. present(found) .and. .not. present(default) .and. .not. foundl) then
    call iotk_error_issue(ierrl,"iotk_scan_attr",__FILE__,__LINE__)
# 440 "iotk_attr.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.20 ")
# 440 "iotk_attr.spp"
call iotk_error_msg(ierrl,'Attribute not found')
# 440 "iotk_attr.spp"
call iotk_error_write(ierrl,"name",name)
    ierrl = - ierrl
  end if
  if(present(default) .and. .not. foundl) then
# 453 "iotk_attr.spp"
    val = default
# 455 "iotk_attr.spp"
  end if
  if(present(ierr)) then
    ierr = ierrl
  else
    if(ierrl/=0) call iotk_error_handler(ierrl)
  end if
end subroutine iotk_scan_attr_LOGICAL3_4
# 463 "iotk_attr.spp"

#endif
#endif

subroutine iotk_attr_dummy_LOGICAL3_4
  write(0,*)
end subroutine iotk_attr_dummy_LOGICAL3_4

# 45 "iotk_attr.spp"

# 65 "iotk_attr.spp"


!------------------------------------------------------------------------------!
! Inclusion of configuration file
#include "iotk_config.h"
!------------------------------------------------------------------------------!

# 74 "iotk_attr.spp"
#include "iotk_auxmacros.h"
# 76 "iotk_attr.spp"

# 78 "iotk_attr.spp"

#ifdef __IOTK_LOGICAL3
#if 5 <= __IOTK_MAXRANK

# 158 "iotk_attr.spp"

# 233 "iotk_attr.spp"

# 236 "iotk_attr.spp"
subroutine iotk_write_attr_LOGICAL3_5(attr,name,val,dummy,first,newline,ierr)
  use iotk_base
  use iotk_error_interf
  use iotk_attr_interf
  use iotk_str_interf
  use iotk_misc_interf
  implicit none
  character(*), intent(inout) :: attr
  character(*), intent(in)    :: name
  LOGICAL(kind=iotk_LOGICAL3), intent(in)  :: val (:,:,:,:,:)
  type(iotk_dummytype), optional :: dummy
  logical, optional, intent(in)  :: first
  logical, optional, intent(in)  :: newline
  integer, optional, intent(out) :: ierr
  integer :: ierrl
  integer :: attlen
  integer :: vallen
  integer :: namlen
  character :: delim
# 258 "iotk_attr.spp"
  character(iotk_vallenx) :: tmpval
  logical :: nl
  if(present(newline)) then
    nl = newline
  else
    nl = .false.
  endif
  ierrl = 0
  if(present(first)) then
    if(first) attr(1:1) = iotk_eos
  end if
  attlen = iotk_strlen_trim(attr)
  namlen = iotk_strlen_trim(name)
  if(.not.iotk_check_name(name)) then
    call iotk_error_issue(ierrl,"iotk_write_attr",__FILE__,__LINE__)
# 272 "iotk_attr.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.20 ")
# 272 "iotk_attr.spp"
call iotk_error_msg(ierrl,'Wrong tag name')
# 272 "iotk_attr.spp"
call iotk_error_write(ierrl,"name",name(1:namlen))
    goto 1
  end if
# 289 "iotk_attr.spp"
  delim = '"'
# 293 "iotk_attr.spp"
  call iotk_write(pack(val,mask=.true.),tmpval,ierrl)
# 295 "iotk_attr.spp"
  if(ierrl/=0) then
    call iotk_error_issue(ierrl,"iotk_write_attr",__FILE__,__LINE__)
# 296 "iotk_attr.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.20 ")
    goto 1
  end if
# 300 "iotk_attr.spp"
  vallen = iotk_strlen(tmpval)
  if(attlen+vallen+namlen+5>len(attr)) then
    call iotk_error_issue(ierrl,"iotk_write_attr",__FILE__,__LINE__)
# 302 "iotk_attr.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.20 ")
# 302 "iotk_attr.spp"
call iotk_error_msg(ierrl,'Attribute dummy argument is too short')
    goto 1
  end if
  if(.not. nl) then
    attr(attlen+1:attlen+vallen+namlen+5) = " "//name(1:namlen)//"="//delim//tmpval(1:vallen)//delim//iotk_eos
  else
    attr(attlen+1:attlen+vallen+namlen+len(iotk_newline)+5) &
       = iotk_newline//" "//name(1:namlen)//"="//delim//tmpval(1:vallen)//delim//iotk_eos
  endif
1 continue
  if(present(ierr)) then
    ierr = ierrl
  else
    if(ierrl/=0) call iotk_error_handler(ierrl)
  end if
end subroutine iotk_write_attr_LOGICAL3_5

# 320 "iotk_attr.spp"
subroutine iotk_scan_attr_LOGICAL3_5(attr,name,val,dummy,found,default,eos,ierr)
  use iotk_base
  use iotk_error_interf
  use iotk_attr_interf, only : iotk_read
  use iotk_str_interf
  use iotk_misc_interf
  implicit none
  character(*),             intent(in)  :: attr
  character(*),             intent(in)  :: name
#ifdef __IOTK_WORKAROUND6
  LOGICAL(kind=iotk_LOGICAL3)                        :: val (:,:,:,:,:)
#else
  LOGICAL(kind=iotk_LOGICAL3), intent(out)           :: val (:,:,:,:,:)
#endif
  type(iotk_dummytype), optional :: dummy
  logical,        optional, intent(out) :: found
  LOGICAL(kind=iotk_LOGICAL3), optional, intent(in)  :: default (:,:,:,:,:)
  logical,        optional, intent(in)  :: eos
  integer,        optional, intent(out) :: ierr
  integer :: ierrl
  integer :: attlen,pos,equal,namlen
  character :: delim
  logical :: foundl
  character(iotk_vallenx) :: valc
# 351 "iotk_attr.spp"
  integer :: index
  LOGICAL(kind=iotk_LOGICAL3), allocatable :: tmpval (:)
# 354 "iotk_attr.spp"
  ierrl = 0
  attlen=iotk_strlen(attr)
  namlen=iotk_strlen_trim(name)
  foundl = .false.
  equal = 0
  do
    pos = verify(attr(equal+1:attlen)," ")
    if(pos<=0) exit
    equal = equal + pos
    pos = scan(attr(equal+1:attlen),"=")
    if(pos<=0) then
      call iotk_error_issue(ierrl,"iotk_scan_attr",__FILE__,__LINE__)
# 365 "iotk_attr.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.20 ")
# 365 "iotk_attr.spp"
call iotk_error_msg(ierrl,'')
# 365 "iotk_attr.spp"
call iotk_error_write(ierrl,"attr",attr(equal+1:attlen))
      goto 1
    end if
    equal = equal + pos
    if(trim(attr(equal-pos:equal-1))==name(1:namlen)) foundl = .true.
    pos = verify(attr(equal+1:attlen)," ")
    if(pos<=0) then
      call iotk_error_issue(ierrl,"iotk_scan_attr",__FILE__,__LINE__)
# 372 "iotk_attr.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.20 ")
      goto 1
    end if
    equal = equal + pos
    delim = attr(equal:equal)
    if(delim/="'" .and. delim/='"') then
      call iotk_error_issue(ierrl,"iotk_scan_attr",__FILE__,__LINE__)
# 378 "iotk_attr.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.20 ")
      goto 1
    end if
    pos = scan(attr(equal+1:attlen),delim)
    if(pos<=0) then
      call iotk_error_issue(ierrl,"iotk_scan_attr",__FILE__,__LINE__)
# 383 "iotk_attr.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.20 ")
      goto 1
    end if
    if(foundl) exit
    equal = equal + pos
  end do
  if(foundl) then
    call iotk_strcpy(valc,attr(equal+1:equal+pos-1),ierrl)
    if(ierrl/=0) then
      call iotk_error_issue(ierrl,"iotk_scan_attr",__FILE__,__LINE__)
# 392 "iotk_attr.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.20 ")
      goto 1
    end if
  else
    goto 1
  end if
# 413 "iotk_attr.spp"
  allocate(tmpval(size(val)))
  index = 0
  call iotk_str_clean(valc)
  call iotk_read(tmpval,valc(1:iotk_strlen(valc)),index,ierrl)
  if(ierrl/=0) then
    call iotk_error_issue(ierrl,"iotk_scan_attr",__FILE__,__LINE__)
# 418 "iotk_attr.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.20 ")
    goto 1
  end if
# 424 "iotk_attr.spp"
  if(index/=size(val)) then
# 426 "iotk_attr.spp"
    call iotk_error_issue(ierrl,"iotk_scan_attr",__FILE__,__LINE__)
# 426 "iotk_attr.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.20 ")
# 426 "iotk_attr.spp"
call iotk_error_msg(ierrl,'Attribute size does not match')
# 426 "iotk_attr.spp"
call iotk_error_write(ierrl,"attr",valc)
# 426 "iotk_attr.spp"
call iotk_error_write(ierrl,"size",size(tmpval))
    goto 1
  end if
# 432 "iotk_attr.spp"
  val = reshape (source=tmpval,shape=shape(val))
# 434 "iotk_attr.spp"
  deallocate(tmpval)
# 436 "iotk_attr.spp"
1 continue
  if(ierrl/=0) foundl=.false.
  if(present(found)) found = foundl
  if(ierrl==0 .and. .not. present(found) .and. .not. present(default) .and. .not. foundl) then
    call iotk_error_issue(ierrl,"iotk_scan_attr",__FILE__,__LINE__)
# 440 "iotk_attr.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.20 ")
# 440 "iotk_attr.spp"
call iotk_error_msg(ierrl,'Attribute not found')
# 440 "iotk_attr.spp"
call iotk_error_write(ierrl,"name",name)
    ierrl = - ierrl
  end if
  if(present(default) .and. .not. foundl) then
# 453 "iotk_attr.spp"
    val = default
# 455 "iotk_attr.spp"
  end if
  if(present(ierr)) then
    ierr = ierrl
  else
    if(ierrl/=0) call iotk_error_handler(ierrl)
  end if
end subroutine iotk_scan_attr_LOGICAL3_5
# 463 "iotk_attr.spp"

#endif
#endif

subroutine iotk_attr_dummy_LOGICAL3_5
  write(0,*)
end subroutine iotk_attr_dummy_LOGICAL3_5

# 45 "iotk_attr.spp"

# 48 "iotk_attr.spp"
! Input/Output Tool Kit (IOTK)
! Copyright (C) 2004-2006 Giovanni Bussi
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
# 65 "iotk_attr.spp"


!------------------------------------------------------------------------------!
! Inclusion of configuration file
#include "iotk_config.h"
!------------------------------------------------------------------------------!

# 74 "iotk_attr.spp"
#include "iotk_auxmacros.h"
# 76 "iotk_attr.spp"

# 78 "iotk_attr.spp"

#ifdef __IOTK_LOGICAL3
#if 6 <= __IOTK_MAXRANK

# 158 "iotk_attr.spp"

# 233 "iotk_attr.spp"

# 236 "iotk_attr.spp"
subroutine iotk_write_attr_LOGICAL3_6(attr,name,val,dummy,first,newline,ierr)
  use iotk_base
  use iotk_error_interf
  use iotk_attr_interf
  use iotk_str_interf
  use iotk_misc_interf
  implicit none
  character(*), intent(inout) :: attr
  character(*), intent(in)    :: name
  LOGICAL(kind=iotk_LOGICAL3), intent(in)  :: val (:,:,:,:,:,:)
  type(iotk_dummytype), optional :: dummy
  logical, optional, intent(in)  :: first
  logical, optional, intent(in)  :: newline
  integer, optional, intent(out) :: ierr
  integer :: ierrl
  integer :: attlen
  integer :: vallen
  integer :: namlen
  character :: delim
# 258 "iotk_attr.spp"
  character(iotk_vallenx) :: tmpval
  logical :: nl
  if(present(newline)) then
    nl = newline
  else
    nl = .false.
  endif
  ierrl = 0
  if(present(first)) then
    if(first) attr(1:1) = iotk_eos
  end if
  attlen = iotk_strlen_trim(attr)
  namlen = iotk_strlen_trim(name)
  if(.not.iotk_check_name(name)) then
    call iotk_error_issue(ierrl,"iotk_write_attr",__FILE__,__LINE__)
# 272 "iotk_attr.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.20 ")
# 272 "iotk_attr.spp"
call iotk_error_msg(ierrl,'Wrong tag name')
# 272 "iotk_attr.spp"
call iotk_error_write(ierrl,"name",name(1:namlen))
    goto 1
  end if
# 289 "iotk_attr.spp"
  delim = '"'
# 293 "iotk_attr.spp"
  call iotk_write(pack(val,mask=.true.),tmpval,ierrl)
# 295 "iotk_attr.spp"
  if(ierrl/=0) then
    call iotk_error_issue(ierrl,"iotk_write_attr",__FILE__,__LINE__)
# 296 "iotk_attr.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.20 ")
    goto 1
  end if
# 300 "iotk_attr.spp"
  vallen = iotk_strlen(tmpval)
  if(attlen+vallen+namlen+5>len(attr)) then
    call iotk_error_issue(ierrl,"iotk_write_attr",__FILE__,__LINE__)
# 302 "iotk_attr.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.20 ")
# 302 "iotk_attr.spp"
call iotk_error_msg(ierrl,'Attribute dummy argument is too short')
    goto 1
  end if
  if(.not. nl) then
    attr(attlen+1:attlen+vallen+namlen+5) = " "//name(1:namlen)//"="//delim//tmpval(1:vallen)//delim//iotk_eos
  else
    attr(attlen+1:attlen+vallen+namlen+len(iotk_newline)+5) &
       = iotk_newline//" "//name(1:namlen)//"="//delim//tmpval(1:vallen)//delim//iotk_eos
  endif
1 continue
  if(present(ierr)) then
    ierr = ierrl
  else
    if(ierrl/=0) call iotk_error_handler(ierrl)
  end if
end subroutine iotk_write_attr_LOGICAL3_6

# 320 "iotk_attr.spp"
subroutine iotk_scan_attr_LOGICAL3_6(attr,name,val,dummy,found,default,eos,ierr)
  use iotk_base
  use iotk_error_interf
  use iotk_attr_interf, only : iotk_read
  use iotk_str_interf
  use iotk_misc_interf
  implicit none
  character(*),             intent(in)  :: attr
  character(*),             intent(in)  :: name
#ifdef __IOTK_WORKAROUND6
  LOGICAL(kind=iotk_LOGICAL3)                        :: val (:,:,:,:,:,:)
#else
  LOGICAL(kind=iotk_LOGICAL3), intent(out)           :: val (:,:,:,:,:,:)
#endif
  type(iotk_dummytype), optional :: dummy
  logical,        optional, intent(out) :: found
  LOGICAL(kind=iotk_LOGICAL3), optional, intent(in)  :: default (:,:,:,:,:,:)
  logical,        optional, intent(in)  :: eos
  integer,        optional, intent(out) :: ierr
  integer :: ierrl
  integer :: attlen,pos,equal,namlen
  character :: delim
  logical :: foundl
  character(iotk_vallenx) :: valc
# 351 "iotk_attr.spp"
  integer :: index
  LOGICAL(kind=iotk_LOGICAL3), allocatable :: tmpval (:)
# 354 "iotk_attr.spp"
  ierrl = 0
  attlen=iotk_strlen(attr)
  namlen=iotk_strlen_trim(name)
  foundl = .false.
  equal = 0
  do
    pos = verify(attr(equal+1:attlen)," ")
    if(pos<=0) exit
    equal = equal + pos
    pos = scan(attr(equal+1:attlen),"=")
    if(pos<=0) then
      call iotk_error_issue(ierrl,"iotk_scan_attr",__FILE__,__LINE__)
# 365 "iotk_attr.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.20 ")
# 365 "iotk_attr.spp"
call iotk_error_msg(ierrl,'')
# 365 "iotk_attr.spp"
call iotk_error_write(ierrl,"attr",attr(equal+1:attlen))
      goto 1
    end if
    equal = equal + pos
    if(trim(attr(equal-pos:equal-1))==name(1:namlen)) foundl = .true.
    pos = verify(attr(equal+1:attlen)," ")
    if(pos<=0) then
      call iotk_error_issue(ierrl,"iotk_scan_attr",__FILE__,__LINE__)
# 372 "iotk_attr.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.20 ")
      goto 1
    end if
    equal = equal + pos
    delim = attr(equal:equal)
    if(delim/="'" .and. delim/='"') then
      call iotk_error_issue(ierrl,"iotk_scan_attr",__FILE__,__LINE__)
# 378 "iotk_attr.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.20 ")
      goto 1
    end if
    pos = scan(attr(equal+1:attlen),delim)
    if(pos<=0) then
      call iotk_error_issue(ierrl,"iotk_scan_attr",__FILE__,__LINE__)
# 383 "iotk_attr.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.20 ")
      goto 1
    end if
    if(foundl) exit
    equal = equal + pos
  end do
  if(foundl) then
    call iotk_strcpy(valc,attr(equal+1:equal+pos-1),ierrl)
    if(ierrl/=0) then
      call iotk_error_issue(ierrl,"iotk_scan_attr",__FILE__,__LINE__)
# 392 "iotk_attr.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.20 ")
      goto 1
    end if
  else
    goto 1
  end if
# 413 "iotk_attr.spp"
  allocate(tmpval(size(val)))
  index = 0
  call iotk_str_clean(valc)
  call iotk_read(tmpval,valc(1:iotk_strlen(valc)),index,ierrl)
  if(ierrl/=0) then
    call iotk_error_issue(ierrl,"iotk_scan_attr",__FILE__,__LINE__)
# 418 "iotk_attr.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.20 ")
    goto 1
  end if
# 424 "iotk_attr.spp"
  if(index/=size(val)) then
# 426 "iotk_attr.spp"
    call iotk_error_issue(ierrl,"iotk_scan_attr",__FILE__,__LINE__)
# 426 "iotk_attr.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.20 ")
# 426 "iotk_attr.spp"
call iotk_error_msg(ierrl,'Attribute size does not match')
# 426 "iotk_attr.spp"
call iotk_error_write(ierrl,"attr",valc)
# 426 "iotk_attr.spp"
call iotk_error_write(ierrl,"size",size(tmpval))
    goto 1
  end if
# 432 "iotk_attr.spp"
  val = reshape (source=tmpval,shape=shape(val))
# 434 "iotk_attr.spp"
  deallocate(tmpval)
# 436 "iotk_attr.spp"
1 continue
  if(ierrl/=0) foundl=.false.
  if(present(found)) found = foundl
  if(ierrl==0 .and. .not. present(found) .and. .not. present(default) .and. .not. foundl) then
    call iotk_error_issue(ierrl,"iotk_scan_attr",__FILE__,__LINE__)
# 440 "iotk_attr.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.20 ")
# 440 "iotk_attr.spp"
call iotk_error_msg(ierrl,'Attribute not found')
# 440 "iotk_attr.spp"
call iotk_error_write(ierrl,"name",name)
    ierrl = - ierrl
  end if
  if(present(default) .and. .not. foundl) then
# 453 "iotk_attr.spp"
    val = default
# 455 "iotk_attr.spp"
  end if
  if(present(ierr)) then
    ierr = ierrl
  else
    if(ierrl/=0) call iotk_error_handler(ierrl)
  end if
end subroutine iotk_scan_attr_LOGICAL3_6
# 463 "iotk_attr.spp"

#endif
#endif

subroutine iotk_attr_dummy_LOGICAL3_6
  write(0,*)
end subroutine iotk_attr_dummy_LOGICAL3_6

# 45 "iotk_attr.spp"

# 65 "iotk_attr.spp"


!------------------------------------------------------------------------------!
! Inclusion of configuration file
#include "iotk_config.h"
!------------------------------------------------------------------------------!

# 74 "iotk_attr.spp"
#include "iotk_auxmacros.h"
# 76 "iotk_attr.spp"

# 78 "iotk_attr.spp"

#ifdef __IOTK_LOGICAL3
#if 7 <= __IOTK_MAXRANK

# 158 "iotk_attr.spp"

# 233 "iotk_attr.spp"

# 236 "iotk_attr.spp"
subroutine iotk_write_attr_LOGICAL3_7(attr,name,val,dummy,first,newline,ierr)
  use iotk_base
  use iotk_error_interf
  use iotk_attr_interf
  use iotk_str_interf
  use iotk_misc_interf
  implicit none
  character(*), intent(inout) :: attr
  character(*), intent(in)    :: name
  LOGICAL(kind=iotk_LOGICAL3), intent(in)  :: val (:,:,:,:,:,:,:)
  type(iotk_dummytype), optional :: dummy
  logical, optional, intent(in)  :: first
  logical, optional, intent(in)  :: newline
  integer, optional, intent(out) :: ierr
  integer :: ierrl
  integer :: attlen
  integer :: vallen
  integer :: namlen
  character :: delim
# 258 "iotk_attr.spp"
  character(iotk_vallenx) :: tmpval
  logical :: nl
  if(present(newline)) then
    nl = newline
  else
    nl = .false.
  endif
  ierrl = 0
  if(present(first)) then
    if(first) attr(1:1) = iotk_eos
  end if
  attlen = iotk_strlen_trim(attr)
  namlen = iotk_strlen_trim(name)
  if(.not.iotk_check_name(name)) then
    call iotk_error_issue(ierrl,"iotk_write_attr",__FILE__,__LINE__)
# 272 "iotk_attr.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.20 ")
# 272 "iotk_attr.spp"
call iotk_error_msg(ierrl,'Wrong tag name')
# 272 "iotk_attr.spp"
call iotk_error_write(ierrl,"name",name(1:namlen))
    goto 1
  end if
# 289 "iotk_attr.spp"
  delim = '"'
# 293 "iotk_attr.spp"
  call iotk_write(pack(val,mask=.true.),tmpval,ierrl)
# 295 "iotk_attr.spp"
  if(ierrl/=0) then
    call iotk_error_issue(ierrl,"iotk_write_attr",__FILE__,__LINE__)
# 296 "iotk_attr.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.20 ")
    goto 1
  end if
# 300 "iotk_attr.spp"
  vallen = iotk_strlen(tmpval)
  if(attlen+vallen+namlen+5>len(attr)) then
    call iotk_error_issue(ierrl,"iotk_write_attr",__FILE__,__LINE__)
# 302 "iotk_attr.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.20 ")
# 302 "iotk_attr.spp"
call iotk_error_msg(ierrl,'Attribute dummy argument is too short')
    goto 1
  end if
  if(.not. nl) then
    attr(attlen+1:attlen+vallen+namlen+5) = " "//name(1:namlen)//"="//delim//tmpval(1:vallen)//delim//iotk_eos
  else
    attr(attlen+1:attlen+vallen+namlen+len(iotk_newline)+5) &
       = iotk_newline//" "//name(1:namlen)//"="//delim//tmpval(1:vallen)//delim//iotk_eos
  endif
1 continue
  if(present(ierr)) then
    ierr = ierrl
  else
    if(ierrl/=0) call iotk_error_handler(ierrl)
  end if
end subroutine iotk_write_attr_LOGICAL3_7

# 320 "iotk_attr.spp"
subroutine iotk_scan_attr_LOGICAL3_7(attr,name,val,dummy,found,default,eos,ierr)
  use iotk_base
  use iotk_error_interf
  use iotk_attr_interf, only : iotk_read
  use iotk_str_interf
  use iotk_misc_interf
  implicit none
  character(*),             intent(in)  :: attr
  character(*),             intent(in)  :: name
#ifdef __IOTK_WORKAROUND6
  LOGICAL(kind=iotk_LOGICAL3)                        :: val (:,:,:,:,:,:,:)
#else
  LOGICAL(kind=iotk_LOGICAL3), intent(out)           :: val (:,:,:,:,:,:,:)
#endif
  type(iotk_dummytype), optional :: dummy
  logical,        optional, intent(out) :: found
  LOGICAL(kind=iotk_LOGICAL3), optional, intent(in)  :: default (:,:,:,:,:,:,:)
  logical,        optional, intent(in)  :: eos
  integer,        optional, intent(out) :: ierr
  integer :: ierrl
  integer :: attlen,pos,equal,namlen
  character :: delim
  logical :: foundl
  character(iotk_vallenx) :: valc
# 351 "iotk_attr.spp"
  integer :: index
  LOGICAL(kind=iotk_LOGICAL3), allocatable :: tmpval (:)
# 354 "iotk_attr.spp"
  ierrl = 0
  attlen=iotk_strlen(attr)
  namlen=iotk_strlen_trim(name)
  foundl = .false.
  equal = 0
  do
    pos = verify(attr(equal+1:attlen)," ")
    if(pos<=0) exit
    equal = equal + pos
    pos = scan(attr(equal+1:attlen),"=")
    if(pos<=0) then
      call iotk_error_issue(ierrl,"iotk_scan_attr",__FILE__,__LINE__)
# 365 "iotk_attr.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.20 ")
# 365 "iotk_attr.spp"
call iotk_error_msg(ierrl,'')
# 365 "iotk_attr.spp"
call iotk_error_write(ierrl,"attr",attr(equal+1:attlen))
      goto 1
    end if
    equal = equal + pos
    if(trim(attr(equal-pos:equal-1))==name(1:namlen)) foundl = .true.
    pos = verify(attr(equal+1:attlen)," ")
    if(pos<=0) then
      call iotk_error_issue(ierrl,"iotk_scan_attr",__FILE__,__LINE__)
# 372 "iotk_attr.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.20 ")
      goto 1
    end if
    equal = equal + pos
    delim = attr(equal:equal)
    if(delim/="'" .and. delim/='"') then
      call iotk_error_issue(ierrl,"iotk_scan_attr",__FILE__,__LINE__)
# 378 "iotk_attr.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.20 ")
      goto 1
    end if
    pos = scan(attr(equal+1:attlen),delim)
    if(pos<=0) then
      call iotk_error_issue(ierrl,"iotk_scan_attr",__FILE__,__LINE__)
# 383 "iotk_attr.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.20 ")
      goto 1
    end if
    if(foundl) exit
    equal = equal + pos
  end do
  if(foundl) then
    call iotk_strcpy(valc,attr(equal+1:equal+pos-1),ierrl)
    if(ierrl/=0) then
      call iotk_error_issue(ierrl,"iotk_scan_attr",__FILE__,__LINE__)
# 392 "iotk_attr.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.20 ")
      goto 1
    end if
  else
    goto 1
  end if
# 413 "iotk_attr.spp"
  allocate(tmpval(size(val)))
  index = 0
  call iotk_str_clean(valc)
  call iotk_read(tmpval,valc(1:iotk_strlen(valc)),index,ierrl)
  if(ierrl/=0) then
    call iotk_error_issue(ierrl,"iotk_scan_attr",__FILE__,__LINE__)
# 418 "iotk_attr.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.20 ")
    goto 1
  end if
# 424 "iotk_attr.spp"
  if(index/=size(val)) then
# 426 "iotk_attr.spp"
    call iotk_error_issue(ierrl,"iotk_scan_attr",__FILE__,__LINE__)
# 426 "iotk_attr.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.20 ")
# 426 "iotk_attr.spp"
call iotk_error_msg(ierrl,'Attribute size does not match')
# 426 "iotk_attr.spp"
call iotk_error_write(ierrl,"attr",valc)
# 426 "iotk_attr.spp"
call iotk_error_write(ierrl,"size",size(tmpval))
    goto 1
  end if
# 432 "iotk_attr.spp"
  val = reshape (source=tmpval,shape=shape(val))
# 434 "iotk_attr.spp"
  deallocate(tmpval)
# 436 "iotk_attr.spp"
1 continue
  if(ierrl/=0) foundl=.false.
  if(present(found)) found = foundl
  if(ierrl==0 .and. .not. present(found) .and. .not. present(default) .and. .not. foundl) then
    call iotk_error_issue(ierrl,"iotk_scan_attr",__FILE__,__LINE__)
# 440 "iotk_attr.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.20 ")
# 440 "iotk_attr.spp"
call iotk_error_msg(ierrl,'Attribute not found')
# 440 "iotk_attr.spp"
call iotk_error_write(ierrl,"name",name)
    ierrl = - ierrl
  end if
  if(present(default) .and. .not. foundl) then
# 453 "iotk_attr.spp"
    val = default
# 455 "iotk_attr.spp"
  end if
  if(present(ierr)) then
    ierr = ierrl
  else
    if(ierrl/=0) call iotk_error_handler(ierrl)
  end if
end subroutine iotk_scan_attr_LOGICAL3_7
# 463 "iotk_attr.spp"

#endif
#endif

subroutine iotk_attr_dummy_LOGICAL3_7
  write(0,*)
end subroutine iotk_attr_dummy_LOGICAL3_7

# 45 "iotk_attr.spp"

# 48 "iotk_attr.spp"
! Input/Output Tool Kit (IOTK)
! Copyright (C) 2004-2006 Giovanni Bussi
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
# 65 "iotk_attr.spp"


!------------------------------------------------------------------------------!
! Inclusion of configuration file
#include "iotk_config.h"
!------------------------------------------------------------------------------!

# 74 "iotk_attr.spp"
#include "iotk_auxmacros.h"
# 76 "iotk_attr.spp"

# 78 "iotk_attr.spp"

#ifdef __IOTK_LOGICAL4
#if 0 <= __IOTK_MAXRANK

# 83 "iotk_attr.spp"
! This is needed as a workaround for bugged pack 
subroutine iotk_private_pack_LOGICAL4(out,in,n,l)
    use iotk_base
    implicit none
    integer,                                    intent(in)  :: n,l
# 92 "iotk_attr.spp"
    LOGICAL (kind=iotk_LOGICAL4), intent(out) :: out(n)
    LOGICAL (kind=iotk_LOGICAL4), intent(in)  :: in(n)
# 95 "iotk_attr.spp"
    out = in
end subroutine iotk_private_pack_LOGICAL4

# 100 "iotk_attr.spp"
subroutine iotk_write_LOGICAL4(val,string,ierr)
  use iotk_base
  use iotk_error_interf
  use iotk_xtox_interf
  use iotk_fmt_interf
  use iotk_str_interf
  use iotk_misc_interf
  implicit none
  LOGICAL(kind=iotk_LOGICAL4), intent(in) :: val(:)
#ifdef __IOTK_WORKAROUND6
  character(len=*)              :: string
#else
  character(len=*), intent(out) :: string
#endif
  integer, intent(out) :: ierr
# 118 "iotk_attr.spp"
  integer :: index,iostat
  ierr = 0
  iostat = 0 
  string(1:1) = iotk_eos
  if(size(val)==0) return
  if(len(string)==0) then
    call iotk_error_issue(ierr,"iotk_write",__FILE__,__LINE__)
# 124 "iotk_attr.spp"
call iotk_error_msg(ierr,"CVS Revision: 1.20 ")
    return
  end if
  do index=1,size(val)
# 129 "iotk_attr.spp"
    call iotk_strcat(string,iotk_ltoa(val(index))//" ",ierr)
    if(ierr/=0) then
      call iotk_error_issue(ierr,"iotk_write",__FILE__,__LINE__)
# 131 "iotk_attr.spp"
call iotk_error_msg(ierr,"CVS Revision: 1.20 ")
      return
    end if
# 152 "iotk_attr.spp"
  end do
! the last blank is deleted
  string(iotk_strlen(string):iotk_strlen(string)) = iotk_eos
end subroutine iotk_write_LOGICAL4
# 158 "iotk_attr.spp"

# 162 "iotk_attr.spp"
subroutine iotk_read_LOGICAL4(val,string,index,ierr)
  use iotk_base
  use iotk_error_interf
  use iotk_xtox_interf
  use iotk_misc_interf
  implicit none
  LOGICAL(kind=iotk_LOGICAL4), intent(inout) :: val(:)
  character(len=*), intent(in) :: string
  integer, intent(inout) :: index
  integer, intent(out) :: ierr
# 173 "iotk_attr.spp"
  logical :: check
# 175 "iotk_attr.spp"
  integer :: pos,pos1,iostat
  integer :: maxindex
# 181 "iotk_attr.spp"
  pos = 0
  pos1= 0
  ierr = 0
  iostat = 0
# 188 "iotk_attr.spp"
    maxindex = size(val)
# 190 "iotk_attr.spp"
! for the moment, commas are considered as blanks
  do
    pos = verify(string(pos1+1:)," ,")+pos1
    if(pos==pos1) exit
    pos = pos - 1
    pos1 = scan(string(pos+1:)," ,")+pos
    if(pos1==pos) pos1 = len(string) + 1
!READ string(pos+1:pos1-1)
    index = index+1
    if(index>maxindex) then
      call iotk_error_issue(ierr,"iotk_read",__FILE__,__LINE__)
# 200 "iotk_attr.spp"
call iotk_error_msg(ierr,"CVS Revision: 1.20 ")
# 200 "iotk_attr.spp"
call iotk_error_msg(ierr,'Too many data')
    end if
# 205 "iotk_attr.spp"
    val(index) = iotk_atol(string(pos+1:pos1-1),check=check)
# 218 "iotk_attr.spp"
    if(.not.check) then
      call iotk_error_issue(ierr,"iotk_read",__FILE__,__LINE__)
# 219 "iotk_attr.spp"
call iotk_error_msg(ierr,"CVS Revision: 1.20 ")
# 219 "iotk_attr.spp"
call iotk_error_msg(ierr,'Wrong string')
# 219 "iotk_attr.spp"
call iotk_error_write(ierr,"string",string(pos+1:pos1-1))
      return
    end if
# 228 "iotk_attr.spp"
    if(pos1>=len(string)) exit
  end do
end subroutine iotk_read_LOGICAL4
# 233 "iotk_attr.spp"

# 236 "iotk_attr.spp"
subroutine iotk_write_attr_LOGICAL4_0(attr,name,val,dummy,first,newline,ierr)
  use iotk_base
  use iotk_error_interf
  use iotk_attr_interf
  use iotk_str_interf
  use iotk_misc_interf
  implicit none
  character(*), intent(inout) :: attr
  character(*), intent(in)    :: name
  LOGICAL(kind=iotk_LOGICAL4), intent(in)  :: val 
  type(iotk_dummytype), optional :: dummy
  logical, optional, intent(in)  :: first
  logical, optional, intent(in)  :: newline
  integer, optional, intent(out) :: ierr
  integer :: ierrl
  integer :: attlen
  integer :: vallen
  integer :: namlen
  character :: delim
# 258 "iotk_attr.spp"
  character(iotk_vallenx) :: tmpval
  logical :: nl
  if(present(newline)) then
    nl = newline
  else
    nl = .false.
  endif
  ierrl = 0
  if(present(first)) then
    if(first) attr(1:1) = iotk_eos
  end if
  attlen = iotk_strlen_trim(attr)
  namlen = iotk_strlen_trim(name)
  if(.not.iotk_check_name(name)) then
    call iotk_error_issue(ierrl,"iotk_write_attr",__FILE__,__LINE__)
# 272 "iotk_attr.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.20 ")
# 272 "iotk_attr.spp"
call iotk_error_msg(ierrl,'Wrong tag name')
# 272 "iotk_attr.spp"
call iotk_error_write(ierrl,"name",name(1:namlen))
    goto 1
  end if
# 289 "iotk_attr.spp"
  delim = '"'
# 291 "iotk_attr.spp"
  call iotk_write((/val/),tmpval,ierrl)
# 295 "iotk_attr.spp"
  if(ierrl/=0) then
    call iotk_error_issue(ierrl,"iotk_write_attr",__FILE__,__LINE__)
# 296 "iotk_attr.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.20 ")
    goto 1
  end if
# 300 "iotk_attr.spp"
  vallen = iotk_strlen(tmpval)
  if(attlen+vallen+namlen+5>len(attr)) then
    call iotk_error_issue(ierrl,"iotk_write_attr",__FILE__,__LINE__)
# 302 "iotk_attr.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.20 ")
# 302 "iotk_attr.spp"
call iotk_error_msg(ierrl,'Attribute dummy argument is too short')
    goto 1
  end if
  if(.not. nl) then
    attr(attlen+1:attlen+vallen+namlen+5) = " "//name(1:namlen)//"="//delim//tmpval(1:vallen)//delim//iotk_eos
  else
    attr(attlen+1:attlen+vallen+namlen+len(iotk_newline)+5) &
       = iotk_newline//" "//name(1:namlen)//"="//delim//tmpval(1:vallen)//delim//iotk_eos
  endif
1 continue
  if(present(ierr)) then
    ierr = ierrl
  else
    if(ierrl/=0) call iotk_error_handler(ierrl)
  end if
end subroutine iotk_write_attr_LOGICAL4_0

# 320 "iotk_attr.spp"
subroutine iotk_scan_attr_LOGICAL4_0(attr,name,val,dummy,found,default,eos,ierr)
  use iotk_base
  use iotk_error_interf
  use iotk_attr_interf, only : iotk_read
  use iotk_str_interf
  use iotk_misc_interf
  implicit none
  character(*),             intent(in)  :: attr
  character(*),             intent(in)  :: name
#ifdef __IOTK_WORKAROUND6
  LOGICAL(kind=iotk_LOGICAL4)                        :: val 
#else
  LOGICAL(kind=iotk_LOGICAL4), intent(out)           :: val 
#endif
  type(iotk_dummytype), optional :: dummy
  logical,        optional, intent(out) :: found
  LOGICAL(kind=iotk_LOGICAL4), optional, intent(in)  :: default 
  logical,        optional, intent(in)  :: eos
  integer,        optional, intent(out) :: ierr
  integer :: ierrl
  integer :: attlen,pos,equal,namlen
  character :: delim
  logical :: foundl
  character(iotk_vallenx) :: valc
# 351 "iotk_attr.spp"
  integer :: index
  LOGICAL(kind=iotk_LOGICAL4), allocatable :: tmpval (:)
# 354 "iotk_attr.spp"
  ierrl = 0
  attlen=iotk_strlen(attr)
  namlen=iotk_strlen_trim(name)
  foundl = .false.
  equal = 0
  do
    pos = verify(attr(equal+1:attlen)," ")
    if(pos<=0) exit
    equal = equal + pos
    pos = scan(attr(equal+1:attlen),"=")
    if(pos<=0) then
      call iotk_error_issue(ierrl,"iotk_scan_attr",__FILE__,__LINE__)
# 365 "iotk_attr.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.20 ")
# 365 "iotk_attr.spp"
call iotk_error_msg(ierrl,'')
# 365 "iotk_attr.spp"
call iotk_error_write(ierrl,"attr",attr(equal+1:attlen))
      goto 1
    end if
    equal = equal + pos
    if(trim(attr(equal-pos:equal-1))==name(1:namlen)) foundl = .true.
    pos = verify(attr(equal+1:attlen)," ")
    if(pos<=0) then
      call iotk_error_issue(ierrl,"iotk_scan_attr",__FILE__,__LINE__)
# 372 "iotk_attr.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.20 ")
      goto 1
    end if
    equal = equal + pos
    delim = attr(equal:equal)
    if(delim/="'" .and. delim/='"') then
      call iotk_error_issue(ierrl,"iotk_scan_attr",__FILE__,__LINE__)
# 378 "iotk_attr.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.20 ")
      goto 1
    end if
    pos = scan(attr(equal+1:attlen),delim)
    if(pos<=0) then
      call iotk_error_issue(ierrl,"iotk_scan_attr",__FILE__,__LINE__)
# 383 "iotk_attr.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.20 ")
      goto 1
    end if
    if(foundl) exit
    equal = equal + pos
  end do
  if(foundl) then
    call iotk_strcpy(valc,attr(equal+1:equal+pos-1),ierrl)
    if(ierrl/=0) then
      call iotk_error_issue(ierrl,"iotk_scan_attr",__FILE__,__LINE__)
# 392 "iotk_attr.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.20 ")
      goto 1
    end if
  else
    goto 1
  end if
# 413 "iotk_attr.spp"
  allocate(tmpval(1))
  index = 0
  call iotk_str_clean(valc)
  call iotk_read(tmpval,valc(1:iotk_strlen(valc)),index,ierrl)
  if(ierrl/=0) then
    call iotk_error_issue(ierrl,"iotk_scan_attr",__FILE__,__LINE__)
# 418 "iotk_attr.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.20 ")
    goto 1
  end if
# 424 "iotk_attr.spp"
  if(index/=1) then
# 426 "iotk_attr.spp"
    call iotk_error_issue(ierrl,"iotk_scan_attr",__FILE__,__LINE__)
# 426 "iotk_attr.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.20 ")
# 426 "iotk_attr.spp"
call iotk_error_msg(ierrl,'Attribute size does not match')
# 426 "iotk_attr.spp"
call iotk_error_write(ierrl,"attr",valc)
# 426 "iotk_attr.spp"
call iotk_error_write(ierrl,"size",size(tmpval))
    goto 1
  end if
# 430 "iotk_attr.spp"
  val = tmpval(1)
# 434 "iotk_attr.spp"
  deallocate(tmpval)
# 436 "iotk_attr.spp"
1 continue
  if(ierrl/=0) foundl=.false.
  if(present(found)) found = foundl
  if(ierrl==0 .and. .not. present(found) .and. .not. present(default) .and. .not. foundl) then
    call iotk_error_issue(ierrl,"iotk_scan_attr",__FILE__,__LINE__)
# 440 "iotk_attr.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.20 ")
# 440 "iotk_attr.spp"
call iotk_error_msg(ierrl,'Attribute not found')
# 440 "iotk_attr.spp"
call iotk_error_write(ierrl,"name",name)
    ierrl = - ierrl
  end if
  if(present(default) .and. .not. foundl) then
# 453 "iotk_attr.spp"
    val = default
# 455 "iotk_attr.spp"
  end if
  if(present(ierr)) then
    ierr = ierrl
  else
    if(ierrl/=0) call iotk_error_handler(ierrl)
  end if
end subroutine iotk_scan_attr_LOGICAL4_0
# 463 "iotk_attr.spp"

#endif
#endif

subroutine iotk_attr_dummy_LOGICAL4_0
  write(0,*)
end subroutine iotk_attr_dummy_LOGICAL4_0

# 45 "iotk_attr.spp"

# 65 "iotk_attr.spp"


!------------------------------------------------------------------------------!
! Inclusion of configuration file
#include "iotk_config.h"
!------------------------------------------------------------------------------!

# 74 "iotk_attr.spp"
#include "iotk_auxmacros.h"
# 76 "iotk_attr.spp"

# 78 "iotk_attr.spp"

#ifdef __IOTK_LOGICAL4
#if 1 <= __IOTK_MAXRANK

# 158 "iotk_attr.spp"

# 233 "iotk_attr.spp"

# 236 "iotk_attr.spp"
subroutine iotk_write_attr_LOGICAL4_1(attr,name,val,dummy,first,newline,ierr)
  use iotk_base
  use iotk_error_interf
  use iotk_attr_interf
  use iotk_str_interf
  use iotk_misc_interf
  implicit none
  character(*), intent(inout) :: attr
  character(*), intent(in)    :: name
  LOGICAL(kind=iotk_LOGICAL4), intent(in)  :: val (:)
  type(iotk_dummytype), optional :: dummy
  logical, optional, intent(in)  :: first
  logical, optional, intent(in)  :: newline
  integer, optional, intent(out) :: ierr
  integer :: ierrl
  integer :: attlen
  integer :: vallen
  integer :: namlen
  character :: delim
# 258 "iotk_attr.spp"
  character(iotk_vallenx) :: tmpval
  logical :: nl
  if(present(newline)) then
    nl = newline
  else
    nl = .false.
  endif
  ierrl = 0
  if(present(first)) then
    if(first) attr(1:1) = iotk_eos
  end if
  attlen = iotk_strlen_trim(attr)
  namlen = iotk_strlen_trim(name)
  if(.not.iotk_check_name(name)) then
    call iotk_error_issue(ierrl,"iotk_write_attr",__FILE__,__LINE__)
# 272 "iotk_attr.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.20 ")
# 272 "iotk_attr.spp"
call iotk_error_msg(ierrl,'Wrong tag name')
# 272 "iotk_attr.spp"
call iotk_error_write(ierrl,"name",name(1:namlen))
    goto 1
  end if
# 289 "iotk_attr.spp"
  delim = '"'
# 293 "iotk_attr.spp"
  call iotk_write(pack(val,mask=.true.),tmpval,ierrl)
# 295 "iotk_attr.spp"
  if(ierrl/=0) then
    call iotk_error_issue(ierrl,"iotk_write_attr",__FILE__,__LINE__)
# 296 "iotk_attr.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.20 ")
    goto 1
  end if
# 300 "iotk_attr.spp"
  vallen = iotk_strlen(tmpval)
  if(attlen+vallen+namlen+5>len(attr)) then
    call iotk_error_issue(ierrl,"iotk_write_attr",__FILE__,__LINE__)
# 302 "iotk_attr.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.20 ")
# 302 "iotk_attr.spp"
call iotk_error_msg(ierrl,'Attribute dummy argument is too short')
    goto 1
  end if
  if(.not. nl) then
    attr(attlen+1:attlen+vallen+namlen+5) = " "//name(1:namlen)//"="//delim//tmpval(1:vallen)//delim//iotk_eos
  else
    attr(attlen+1:attlen+vallen+namlen+len(iotk_newline)+5) &
       = iotk_newline//" "//name(1:namlen)//"="//delim//tmpval(1:vallen)//delim//iotk_eos
  endif
1 continue
  if(present(ierr)) then
    ierr = ierrl
  else
    if(ierrl/=0) call iotk_error_handler(ierrl)
  end if
end subroutine iotk_write_attr_LOGICAL4_1

# 320 "iotk_attr.spp"
subroutine iotk_scan_attr_LOGICAL4_1(attr,name,val,dummy,found,default,eos,ierr)
  use iotk_base
  use iotk_error_interf
  use iotk_attr_interf, only : iotk_read
  use iotk_str_interf
  use iotk_misc_interf
  implicit none
  character(*),             intent(in)  :: attr
  character(*),             intent(in)  :: name
#ifdef __IOTK_WORKAROUND6
  LOGICAL(kind=iotk_LOGICAL4)                        :: val (:)
#else
  LOGICAL(kind=iotk_LOGICAL4), intent(out)           :: val (:)
#endif
  type(iotk_dummytype), optional :: dummy
  logical,        optional, intent(out) :: found
  LOGICAL(kind=iotk_LOGICAL4), optional, intent(in)  :: default (:)
  logical,        optional, intent(in)  :: eos
  integer,        optional, intent(out) :: ierr
  integer :: ierrl
  integer :: attlen,pos,equal,namlen
  character :: delim
  logical :: foundl
  character(iotk_vallenx) :: valc
# 351 "iotk_attr.spp"
  integer :: index
  LOGICAL(kind=iotk_LOGICAL4), allocatable :: tmpval (:)
# 354 "iotk_attr.spp"
  ierrl = 0
  attlen=iotk_strlen(attr)
  namlen=iotk_strlen_trim(name)
  foundl = .false.
  equal = 0
  do
    pos = verify(attr(equal+1:attlen)," ")
    if(pos<=0) exit
    equal = equal + pos
    pos = scan(attr(equal+1:attlen),"=")
    if(pos<=0) then
      call iotk_error_issue(ierrl,"iotk_scan_attr",__FILE__,__LINE__)
# 365 "iotk_attr.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.20 ")
# 365 "iotk_attr.spp"
call iotk_error_msg(ierrl,'')
# 365 "iotk_attr.spp"
call iotk_error_write(ierrl,"attr",attr(equal+1:attlen))
      goto 1
    end if
    equal = equal + pos
    if(trim(attr(equal-pos:equal-1))==name(1:namlen)) foundl = .true.
    pos = verify(attr(equal+1:attlen)," ")
    if(pos<=0) then
      call iotk_error_issue(ierrl,"iotk_scan_attr",__FILE__,__LINE__)
# 372 "iotk_attr.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.20 ")
      goto 1
    end if
    equal = equal + pos
    delim = attr(equal:equal)
    if(delim/="'" .and. delim/='"') then
      call iotk_error_issue(ierrl,"iotk_scan_attr",__FILE__,__LINE__)
# 378 "iotk_attr.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.20 ")
      goto 1
    end if
    pos = scan(attr(equal+1:attlen),delim)
    if(pos<=0) then
      call iotk_error_issue(ierrl,"iotk_scan_attr",__FILE__,__LINE__)
# 383 "iotk_attr.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.20 ")
      goto 1
    end if
    if(foundl) exit
    equal = equal + pos
  end do
  if(foundl) then
    call iotk_strcpy(valc,attr(equal+1:equal+pos-1),ierrl)
    if(ierrl/=0) then
      call iotk_error_issue(ierrl,"iotk_scan_attr",__FILE__,__LINE__)
# 392 "iotk_attr.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.20 ")
      goto 1
    end if
  else
    goto 1
  end if
# 413 "iotk_attr.spp"
  allocate(tmpval(size(val)))
  index = 0
  call iotk_str_clean(valc)
  call iotk_read(tmpval,valc(1:iotk_strlen(valc)),index,ierrl)
  if(ierrl/=0) then
    call iotk_error_issue(ierrl,"iotk_scan_attr",__FILE__,__LINE__)
# 418 "iotk_attr.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.20 ")
    goto 1
  end if
# 424 "iotk_attr.spp"
  if(index/=size(val)) then
# 426 "iotk_attr.spp"
    call iotk_error_issue(ierrl,"iotk_scan_attr",__FILE__,__LINE__)
# 426 "iotk_attr.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.20 ")
# 426 "iotk_attr.spp"
call iotk_error_msg(ierrl,'Attribute size does not match')
# 426 "iotk_attr.spp"
call iotk_error_write(ierrl,"attr",valc)
# 426 "iotk_attr.spp"
call iotk_error_write(ierrl,"size",size(tmpval))
    goto 1
  end if
# 432 "iotk_attr.spp"
  val = reshape (source=tmpval,shape=shape(val))
# 434 "iotk_attr.spp"
  deallocate(tmpval)
# 436 "iotk_attr.spp"
1 continue
  if(ierrl/=0) foundl=.false.
  if(present(found)) found = foundl
  if(ierrl==0 .and. .not. present(found) .and. .not. present(default) .and. .not. foundl) then
    call iotk_error_issue(ierrl,"iotk_scan_attr",__FILE__,__LINE__)
# 440 "iotk_attr.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.20 ")
# 440 "iotk_attr.spp"
call iotk_error_msg(ierrl,'Attribute not found')
# 440 "iotk_attr.spp"
call iotk_error_write(ierrl,"name",name)
    ierrl = - ierrl
  end if
  if(present(default) .and. .not. foundl) then
# 453 "iotk_attr.spp"
    val = default
# 455 "iotk_attr.spp"
  end if
  if(present(ierr)) then
    ierr = ierrl
  else
    if(ierrl/=0) call iotk_error_handler(ierrl)
  end if
end subroutine iotk_scan_attr_LOGICAL4_1
# 463 "iotk_attr.spp"

#endif
#endif

subroutine iotk_attr_dummy_LOGICAL4_1
  write(0,*)
end subroutine iotk_attr_dummy_LOGICAL4_1

# 45 "iotk_attr.spp"

# 65 "iotk_attr.spp"


!------------------------------------------------------------------------------!
! Inclusion of configuration file
#include "iotk_config.h"
!------------------------------------------------------------------------------!

# 74 "iotk_attr.spp"
#include "iotk_auxmacros.h"
# 76 "iotk_attr.spp"

# 78 "iotk_attr.spp"

#ifdef __IOTK_LOGICAL4
#if 2 <= __IOTK_MAXRANK

# 158 "iotk_attr.spp"

# 233 "iotk_attr.spp"

# 236 "iotk_attr.spp"
subroutine iotk_write_attr_LOGICAL4_2(attr,name,val,dummy,first,newline,ierr)
  use iotk_base
  use iotk_error_interf
  use iotk_attr_interf
  use iotk_str_interf
  use iotk_misc_interf
  implicit none
  character(*), intent(inout) :: attr
  character(*), intent(in)    :: name
  LOGICAL(kind=iotk_LOGICAL4), intent(in)  :: val (:,:)
  type(iotk_dummytype), optional :: dummy
  logical, optional, intent(in)  :: first
  logical, optional, intent(in)  :: newline
  integer, optional, intent(out) :: ierr
  integer :: ierrl
  integer :: attlen
  integer :: vallen
  integer :: namlen
  character :: delim
# 258 "iotk_attr.spp"
  character(iotk_vallenx) :: tmpval
  logical :: nl
  if(present(newline)) then
    nl = newline
  else
    nl = .false.
  endif
  ierrl = 0
  if(present(first)) then
    if(first) attr(1:1) = iotk_eos
  end if
  attlen = iotk_strlen_trim(attr)
  namlen = iotk_strlen_trim(name)
  if(.not.iotk_check_name(name)) then
    call iotk_error_issue(ierrl,"iotk_write_attr",__FILE__,__LINE__)
# 272 "iotk_attr.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.20 ")
# 272 "iotk_attr.spp"
call iotk_error_msg(ierrl,'Wrong tag name')
# 272 "iotk_attr.spp"
call iotk_error_write(ierrl,"name",name(1:namlen))
    goto 1
  end if
# 289 "iotk_attr.spp"
  delim = '"'
# 293 "iotk_attr.spp"
  call iotk_write(pack(val,mask=.true.),tmpval,ierrl)
# 295 "iotk_attr.spp"
  if(ierrl/=0) then
    call iotk_error_issue(ierrl,"iotk_write_attr",__FILE__,__LINE__)
# 296 "iotk_attr.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.20 ")
    goto 1
  end if
# 300 "iotk_attr.spp"
  vallen = iotk_strlen(tmpval)
  if(attlen+vallen+namlen+5>len(attr)) then
    call iotk_error_issue(ierrl,"iotk_write_attr",__FILE__,__LINE__)
# 302 "iotk_attr.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.20 ")
# 302 "iotk_attr.spp"
call iotk_error_msg(ierrl,'Attribute dummy argument is too short')
    goto 1
  end if
  if(.not. nl) then
    attr(attlen+1:attlen+vallen+namlen+5) = " "//name(1:namlen)//"="//delim//tmpval(1:vallen)//delim//iotk_eos
  else
    attr(attlen+1:attlen+vallen+namlen+len(iotk_newline)+5) &
       = iotk_newline//" "//name(1:namlen)//"="//delim//tmpval(1:vallen)//delim//iotk_eos
  endif
1 continue
  if(present(ierr)) then
    ierr = ierrl
  else
    if(ierrl/=0) call iotk_error_handler(ierrl)
  end if
end subroutine iotk_write_attr_LOGICAL4_2

# 320 "iotk_attr.spp"
subroutine iotk_scan_attr_LOGICAL4_2(attr,name,val,dummy,found,default,eos,ierr)
  use iotk_base
  use iotk_error_interf
  use iotk_attr_interf, only : iotk_read
  use iotk_str_interf
  use iotk_misc_interf
  implicit none
  character(*),             intent(in)  :: attr
  character(*),             intent(in)  :: name
#ifdef __IOTK_WORKAROUND6
  LOGICAL(kind=iotk_LOGICAL4)                        :: val (:,:)
#else
  LOGICAL(kind=iotk_LOGICAL4), intent(out)           :: val (:,:)
#endif
  type(iotk_dummytype), optional :: dummy
  logical,        optional, intent(out) :: found
  LOGICAL(kind=iotk_LOGICAL4), optional, intent(in)  :: default (:,:)
  logical,        optional, intent(in)  :: eos
  integer,        optional, intent(out) :: ierr
  integer :: ierrl
  integer :: attlen,pos,equal,namlen
  character :: delim
  logical :: foundl
  character(iotk_vallenx) :: valc
# 351 "iotk_attr.spp"
  integer :: index
  LOGICAL(kind=iotk_LOGICAL4), allocatable :: tmpval (:)
# 354 "iotk_attr.spp"
  ierrl = 0
  attlen=iotk_strlen(attr)
  namlen=iotk_strlen_trim(name)
  foundl = .false.
  equal = 0
  do
    pos = verify(attr(equal+1:attlen)," ")
    if(pos<=0) exit
    equal = equal + pos
    pos = scan(attr(equal+1:attlen),"=")
    if(pos<=0) then
      call iotk_error_issue(ierrl,"iotk_scan_attr",__FILE__,__LINE__)
# 365 "iotk_attr.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.20 ")
# 365 "iotk_attr.spp"
call iotk_error_msg(ierrl,'')
# 365 "iotk_attr.spp"
call iotk_error_write(ierrl,"attr",attr(equal+1:attlen))
      goto 1
    end if
    equal = equal + pos
    if(trim(attr(equal-pos:equal-1))==name(1:namlen)) foundl = .true.
    pos = verify(attr(equal+1:attlen)," ")
    if(pos<=0) then
      call iotk_error_issue(ierrl,"iotk_scan_attr",__FILE__,__LINE__)
# 372 "iotk_attr.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.20 ")
      goto 1
    end if
    equal = equal + pos
    delim = attr(equal:equal)
    if(delim/="'" .and. delim/='"') then
      call iotk_error_issue(ierrl,"iotk_scan_attr",__FILE__,__LINE__)
# 378 "iotk_attr.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.20 ")
      goto 1
    end if
    pos = scan(attr(equal+1:attlen),delim)
    if(pos<=0) then
      call iotk_error_issue(ierrl,"iotk_scan_attr",__FILE__,__LINE__)
# 383 "iotk_attr.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.20 ")
      goto 1
    end if
    if(foundl) exit
    equal = equal + pos
  end do
  if(foundl) then
    call iotk_strcpy(valc,attr(equal+1:equal+pos-1),ierrl)
    if(ierrl/=0) then
      call iotk_error_issue(ierrl,"iotk_scan_attr",__FILE__,__LINE__)
# 392 "iotk_attr.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.20 ")
      goto 1
    end if
  else
    goto 1
  end if
# 413 "iotk_attr.spp"
  allocate(tmpval(size(val)))
  index = 0
  call iotk_str_clean(valc)
  call iotk_read(tmpval,valc(1:iotk_strlen(valc)),index,ierrl)
  if(ierrl/=0) then
    call iotk_error_issue(ierrl,"iotk_scan_attr",__FILE__,__LINE__)
# 418 "iotk_attr.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.20 ")
    goto 1
  end if
# 424 "iotk_attr.spp"
  if(index/=size(val)) then
# 426 "iotk_attr.spp"
    call iotk_error_issue(ierrl,"iotk_scan_attr",__FILE__,__LINE__)
# 426 "iotk_attr.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.20 ")
# 426 "iotk_attr.spp"
call iotk_error_msg(ierrl,'Attribute size does not match')
# 426 "iotk_attr.spp"
call iotk_error_write(ierrl,"attr",valc)
# 426 "iotk_attr.spp"
call iotk_error_write(ierrl,"size",size(tmpval))
    goto 1
  end if
# 432 "iotk_attr.spp"
  val = reshape (source=tmpval,shape=shape(val))
# 434 "iotk_attr.spp"
  deallocate(tmpval)
# 436 "iotk_attr.spp"
1 continue
  if(ierrl/=0) foundl=.false.
  if(present(found)) found = foundl
  if(ierrl==0 .and. .not. present(found) .and. .not. present(default) .and. .not. foundl) then
    call iotk_error_issue(ierrl,"iotk_scan_attr",__FILE__,__LINE__)
# 440 "iotk_attr.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.20 ")
# 440 "iotk_attr.spp"
call iotk_error_msg(ierrl,'Attribute not found')
# 440 "iotk_attr.spp"
call iotk_error_write(ierrl,"name",name)
    ierrl = - ierrl
  end if
  if(present(default) .and. .not. foundl) then
# 453 "iotk_attr.spp"
    val = default
# 455 "iotk_attr.spp"
  end if
  if(present(ierr)) then
    ierr = ierrl
  else
    if(ierrl/=0) call iotk_error_handler(ierrl)
  end if
end subroutine iotk_scan_attr_LOGICAL4_2
# 463 "iotk_attr.spp"

#endif
#endif

subroutine iotk_attr_dummy_LOGICAL4_2
  write(0,*)
end subroutine iotk_attr_dummy_LOGICAL4_2

# 45 "iotk_attr.spp"

# 48 "iotk_attr.spp"
! Input/Output Tool Kit (IOTK)
! Copyright (C) 2004-2006 Giovanni Bussi
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
# 65 "iotk_attr.spp"


!------------------------------------------------------------------------------!
! Inclusion of configuration file
#include "iotk_config.h"
!------------------------------------------------------------------------------!

# 74 "iotk_attr.spp"
#include "iotk_auxmacros.h"
# 76 "iotk_attr.spp"

# 78 "iotk_attr.spp"

#ifdef __IOTK_LOGICAL4
#if 3 <= __IOTK_MAXRANK

# 158 "iotk_attr.spp"

# 233 "iotk_attr.spp"

# 236 "iotk_attr.spp"
subroutine iotk_write_attr_LOGICAL4_3(attr,name,val,dummy,first,newline,ierr)
  use iotk_base
  use iotk_error_interf
  use iotk_attr_interf
  use iotk_str_interf
  use iotk_misc_interf
  implicit none
  character(*), intent(inout) :: attr
  character(*), intent(in)    :: name
  LOGICAL(kind=iotk_LOGICAL4), intent(in)  :: val (:,:,:)
  type(iotk_dummytype), optional :: dummy
  logical, optional, intent(in)  :: first
  logical, optional, intent(in)  :: newline
  integer, optional, intent(out) :: ierr
  integer :: ierrl
  integer :: attlen
  integer :: vallen
  integer :: namlen
  character :: delim
# 258 "iotk_attr.spp"
  character(iotk_vallenx) :: tmpval
  logical :: nl
  if(present(newline)) then
    nl = newline
  else
    nl = .false.
  endif
  ierrl = 0
  if(present(first)) then
    if(first) attr(1:1) = iotk_eos
  end if
  attlen = iotk_strlen_trim(attr)
  namlen = iotk_strlen_trim(name)
  if(.not.iotk_check_name(name)) then
    call iotk_error_issue(ierrl,"iotk_write_attr",__FILE__,__LINE__)
# 272 "iotk_attr.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.20 ")
# 272 "iotk_attr.spp"
call iotk_error_msg(ierrl,'Wrong tag name')
# 272 "iotk_attr.spp"
call iotk_error_write(ierrl,"name",name(1:namlen))
    goto 1
  end if
# 289 "iotk_attr.spp"
  delim = '"'
# 293 "iotk_attr.spp"
  call iotk_write(pack(val,mask=.true.),tmpval,ierrl)
# 295 "iotk_attr.spp"
  if(ierrl/=0) then
    call iotk_error_issue(ierrl,"iotk_write_attr",__FILE__,__LINE__)
# 296 "iotk_attr.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.20 ")
    goto 1
  end if
# 300 "iotk_attr.spp"
  vallen = iotk_strlen(tmpval)
  if(attlen+vallen+namlen+5>len(attr)) then
    call iotk_error_issue(ierrl,"iotk_write_attr",__FILE__,__LINE__)
# 302 "iotk_attr.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.20 ")
# 302 "iotk_attr.spp"
call iotk_error_msg(ierrl,'Attribute dummy argument is too short')
    goto 1
  end if
  if(.not. nl) then
    attr(attlen+1:attlen+vallen+namlen+5) = " "//name(1:namlen)//"="//delim//tmpval(1:vallen)//delim//iotk_eos
  else
    attr(attlen+1:attlen+vallen+namlen+len(iotk_newline)+5) &
       = iotk_newline//" "//name(1:namlen)//"="//delim//tmpval(1:vallen)//delim//iotk_eos
  endif
1 continue
  if(present(ierr)) then
    ierr = ierrl
  else
    if(ierrl/=0) call iotk_error_handler(ierrl)
  end if
end subroutine iotk_write_attr_LOGICAL4_3

# 320 "iotk_attr.spp"
subroutine iotk_scan_attr_LOGICAL4_3(attr,name,val,dummy,found,default,eos,ierr)
  use iotk_base
  use iotk_error_interf
  use iotk_attr_interf, only : iotk_read
  use iotk_str_interf
  use iotk_misc_interf
  implicit none
  character(*),             intent(in)  :: attr
  character(*),             intent(in)  :: name
#ifdef __IOTK_WORKAROUND6
  LOGICAL(kind=iotk_LOGICAL4)                        :: val (:,:,:)
#else
  LOGICAL(kind=iotk_LOGICAL4), intent(out)           :: val (:,:,:)
#endif
  type(iotk_dummytype), optional :: dummy
  logical,        optional, intent(out) :: found
  LOGICAL(kind=iotk_LOGICAL4), optional, intent(in)  :: default (:,:,:)
  logical,        optional, intent(in)  :: eos
  integer,        optional, intent(out) :: ierr
  integer :: ierrl
  integer :: attlen,pos,equal,namlen
  character :: delim
  logical :: foundl
  character(iotk_vallenx) :: valc
# 351 "iotk_attr.spp"
  integer :: index
  LOGICAL(kind=iotk_LOGICAL4), allocatable :: tmpval (:)
# 354 "iotk_attr.spp"
  ierrl = 0
  attlen=iotk_strlen(attr)
  namlen=iotk_strlen_trim(name)
  foundl = .false.
  equal = 0
  do
    pos = verify(attr(equal+1:attlen)," ")
    if(pos<=0) exit
    equal = equal + pos
    pos = scan(attr(equal+1:attlen),"=")
    if(pos<=0) then
      call iotk_error_issue(ierrl,"iotk_scan_attr",__FILE__,__LINE__)
# 365 "iotk_attr.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.20 ")
# 365 "iotk_attr.spp"
call iotk_error_msg(ierrl,'')
# 365 "iotk_attr.spp"
call iotk_error_write(ierrl,"attr",attr(equal+1:attlen))
      goto 1
    end if
    equal = equal + pos
    if(trim(attr(equal-pos:equal-1))==name(1:namlen)) foundl = .true.
    pos = verify(attr(equal+1:attlen)," ")
    if(pos<=0) then
      call iotk_error_issue(ierrl,"iotk_scan_attr",__FILE__,__LINE__)
# 372 "iotk_attr.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.20 ")
      goto 1
    end if
    equal = equal + pos
    delim = attr(equal:equal)
    if(delim/="'" .and. delim/='"') then
      call iotk_error_issue(ierrl,"iotk_scan_attr",__FILE__,__LINE__)
# 378 "iotk_attr.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.20 ")
      goto 1
    end if
    pos = scan(attr(equal+1:attlen),delim)
    if(pos<=0) then
      call iotk_error_issue(ierrl,"iotk_scan_attr",__FILE__,__LINE__)
# 383 "iotk_attr.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.20 ")
      goto 1
    end if
    if(foundl) exit
    equal = equal + pos
  end do
  if(foundl) then
    call iotk_strcpy(valc,attr(equal+1:equal+pos-1),ierrl)
    if(ierrl/=0) then
      call iotk_error_issue(ierrl,"iotk_scan_attr",__FILE__,__LINE__)
# 392 "iotk_attr.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.20 ")
      goto 1
    end if
  else
    goto 1
  end if
# 413 "iotk_attr.spp"
  allocate(tmpval(size(val)))
  index = 0
  call iotk_str_clean(valc)
  call iotk_read(tmpval,valc(1:iotk_strlen(valc)),index,ierrl)
  if(ierrl/=0) then
    call iotk_error_issue(ierrl,"iotk_scan_attr",__FILE__,__LINE__)
# 418 "iotk_attr.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.20 ")
    goto 1
  end if
# 424 "iotk_attr.spp"
  if(index/=size(val)) then
# 426 "iotk_attr.spp"
    call iotk_error_issue(ierrl,"iotk_scan_attr",__FILE__,__LINE__)
# 426 "iotk_attr.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.20 ")
# 426 "iotk_attr.spp"
call iotk_error_msg(ierrl,'Attribute size does not match')
# 426 "iotk_attr.spp"
call iotk_error_write(ierrl,"attr",valc)
# 426 "iotk_attr.spp"
call iotk_error_write(ierrl,"size",size(tmpval))
    goto 1
  end if
# 432 "iotk_attr.spp"
  val = reshape (source=tmpval,shape=shape(val))
# 434 "iotk_attr.spp"
  deallocate(tmpval)
# 436 "iotk_attr.spp"
1 continue
  if(ierrl/=0) foundl=.false.
  if(present(found)) found = foundl
  if(ierrl==0 .and. .not. present(found) .and. .not. present(default) .and. .not. foundl) then
    call iotk_error_issue(ierrl,"iotk_scan_attr",__FILE__,__LINE__)
# 440 "iotk_attr.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.20 ")
# 440 "iotk_attr.spp"
call iotk_error_msg(ierrl,'Attribute not found')
# 440 "iotk_attr.spp"
call iotk_error_write(ierrl,"name",name)
    ierrl = - ierrl
  end if
  if(present(default) .and. .not. foundl) then
# 453 "iotk_attr.spp"
    val = default
# 455 "iotk_attr.spp"
  end if
  if(present(ierr)) then
    ierr = ierrl
  else
    if(ierrl/=0) call iotk_error_handler(ierrl)
  end if
end subroutine iotk_scan_attr_LOGICAL4_3
# 463 "iotk_attr.spp"

#endif
#endif

subroutine iotk_attr_dummy_LOGICAL4_3
  write(0,*)
end subroutine iotk_attr_dummy_LOGICAL4_3

# 45 "iotk_attr.spp"

# 65 "iotk_attr.spp"


!------------------------------------------------------------------------------!
! Inclusion of configuration file
#include "iotk_config.h"
!------------------------------------------------------------------------------!

# 74 "iotk_attr.spp"
#include "iotk_auxmacros.h"
# 76 "iotk_attr.spp"

# 78 "iotk_attr.spp"

#ifdef __IOTK_LOGICAL4
#if 4 <= __IOTK_MAXRANK

# 158 "iotk_attr.spp"

# 233 "iotk_attr.spp"

# 236 "iotk_attr.spp"
subroutine iotk_write_attr_LOGICAL4_4(attr,name,val,dummy,first,newline,ierr)
  use iotk_base
  use iotk_error_interf
  use iotk_attr_interf
  use iotk_str_interf
  use iotk_misc_interf
  implicit none
  character(*), intent(inout) :: attr
  character(*), intent(in)    :: name
  LOGICAL(kind=iotk_LOGICAL4), intent(in)  :: val (:,:,:,:)
  type(iotk_dummytype), optional :: dummy
  logical, optional, intent(in)  :: first
  logical, optional, intent(in)  :: newline
  integer, optional, intent(out) :: ierr
  integer :: ierrl
  integer :: attlen
  integer :: vallen
  integer :: namlen
  character :: delim
# 258 "iotk_attr.spp"
  character(iotk_vallenx) :: tmpval
  logical :: nl
  if(present(newline)) then
    nl = newline
  else
    nl = .false.
  endif
  ierrl = 0
  if(present(first)) then
    if(first) attr(1:1) = iotk_eos
  end if
  attlen = iotk_strlen_trim(attr)
  namlen = iotk_strlen_trim(name)
  if(.not.iotk_check_name(name)) then
    call iotk_error_issue(ierrl,"iotk_write_attr",__FILE__,__LINE__)
# 272 "iotk_attr.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.20 ")
# 272 "iotk_attr.spp"
call iotk_error_msg(ierrl,'Wrong tag name')
# 272 "iotk_attr.spp"
call iotk_error_write(ierrl,"name",name(1:namlen))
    goto 1
  end if
# 289 "iotk_attr.spp"
  delim = '"'
# 293 "iotk_attr.spp"
  call iotk_write(pack(val,mask=.true.),tmpval,ierrl)
# 295 "iotk_attr.spp"
  if(ierrl/=0) then
    call iotk_error_issue(ierrl,"iotk_write_attr",__FILE__,__LINE__)
# 296 "iotk_attr.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.20 ")
    goto 1
  end if
# 300 "iotk_attr.spp"
  vallen = iotk_strlen(tmpval)
  if(attlen+vallen+namlen+5>len(attr)) then
    call iotk_error_issue(ierrl,"iotk_write_attr",__FILE__,__LINE__)
# 302 "iotk_attr.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.20 ")
# 302 "iotk_attr.spp"
call iotk_error_msg(ierrl,'Attribute dummy argument is too short')
    goto 1
  end if
  if(.not. nl) then
    attr(attlen+1:attlen+vallen+namlen+5) = " "//name(1:namlen)//"="//delim//tmpval(1:vallen)//delim//iotk_eos
  else
    attr(attlen+1:attlen+vallen+namlen+len(iotk_newline)+5) &
       = iotk_newline//" "//name(1:namlen)//"="//delim//tmpval(1:vallen)//delim//iotk_eos
  endif
1 continue
  if(present(ierr)) then
    ierr = ierrl
  else
    if(ierrl/=0) call iotk_error_handler(ierrl)
  end if
end subroutine iotk_write_attr_LOGICAL4_4

# 320 "iotk_attr.spp"
subroutine iotk_scan_attr_LOGICAL4_4(attr,name,val,dummy,found,default,eos,ierr)
  use iotk_base
  use iotk_error_interf
  use iotk_attr_interf, only : iotk_read
  use iotk_str_interf
  use iotk_misc_interf
  implicit none
  character(*),             intent(in)  :: attr
  character(*),             intent(in)  :: name
#ifdef __IOTK_WORKAROUND6
  LOGICAL(kind=iotk_LOGICAL4)                        :: val (:,:,:,:)
#else
  LOGICAL(kind=iotk_LOGICAL4), intent(out)           :: val (:,:,:,:)
#endif
  type(iotk_dummytype), optional :: dummy
  logical,        optional, intent(out) :: found
  LOGICAL(kind=iotk_LOGICAL4), optional, intent(in)  :: default (:,:,:,:)
  logical,        optional, intent(in)  :: eos
  integer,        optional, intent(out) :: ierr
  integer :: ierrl
  integer :: attlen,pos,equal,namlen
  character :: delim
  logical :: foundl
  character(iotk_vallenx) :: valc
# 351 "iotk_attr.spp"
  integer :: index
  LOGICAL(kind=iotk_LOGICAL4), allocatable :: tmpval (:)
# 354 "iotk_attr.spp"
  ierrl = 0
  attlen=iotk_strlen(attr)
  namlen=iotk_strlen_trim(name)
  foundl = .false.
  equal = 0
  do
    pos = verify(attr(equal+1:attlen)," ")
    if(pos<=0) exit
    equal = equal + pos
    pos = scan(attr(equal+1:attlen),"=")
    if(pos<=0) then
      call iotk_error_issue(ierrl,"iotk_scan_attr",__FILE__,__LINE__)
# 365 "iotk_attr.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.20 ")
# 365 "iotk_attr.spp"
call iotk_error_msg(ierrl,'')
# 365 "iotk_attr.spp"
call iotk_error_write(ierrl,"attr",attr(equal+1:attlen))
      goto 1
    end if
    equal = equal + pos
    if(trim(attr(equal-pos:equal-1))==name(1:namlen)) foundl = .true.
    pos = verify(attr(equal+1:attlen)," ")
    if(pos<=0) then
      call iotk_error_issue(ierrl,"iotk_scan_attr",__FILE__,__LINE__)
# 372 "iotk_attr.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.20 ")
      goto 1
    end if
    equal = equal + pos
    delim = attr(equal:equal)
    if(delim/="'" .and. delim/='"') then
      call iotk_error_issue(ierrl,"iotk_scan_attr",__FILE__,__LINE__)
# 378 "iotk_attr.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.20 ")
      goto 1
    end if
    pos = scan(attr(equal+1:attlen),delim)
    if(pos<=0) then
      call iotk_error_issue(ierrl,"iotk_scan_attr",__FILE__,__LINE__)
# 383 "iotk_attr.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.20 ")
      goto 1
    end if
    if(foundl) exit
    equal = equal + pos
  end do
  if(foundl) then
    call iotk_strcpy(valc,attr(equal+1:equal+pos-1),ierrl)
    if(ierrl/=0) then
      call iotk_error_issue(ierrl,"iotk_scan_attr",__FILE__,__LINE__)
# 392 "iotk_attr.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.20 ")
      goto 1
    end if
  else
    goto 1
  end if
# 413 "iotk_attr.spp"
  allocate(tmpval(size(val)))
  index = 0
  call iotk_str_clean(valc)
  call iotk_read(tmpval,valc(1:iotk_strlen(valc)),index,ierrl)
  if(ierrl/=0) then
    call iotk_error_issue(ierrl,"iotk_scan_attr",__FILE__,__LINE__)
# 418 "iotk_attr.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.20 ")
    goto 1
  end if
# 424 "iotk_attr.spp"
  if(index/=size(val)) then
# 426 "iotk_attr.spp"
    call iotk_error_issue(ierrl,"iotk_scan_attr",__FILE__,__LINE__)
# 426 "iotk_attr.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.20 ")
# 426 "iotk_attr.spp"
call iotk_error_msg(ierrl,'Attribute size does not match')
# 426 "iotk_attr.spp"
call iotk_error_write(ierrl,"attr",valc)
# 426 "iotk_attr.spp"
call iotk_error_write(ierrl,"size",size(tmpval))
    goto 1
  end if
# 432 "iotk_attr.spp"
  val = reshape (source=tmpval,shape=shape(val))
# 434 "iotk_attr.spp"
  deallocate(tmpval)
# 436 "iotk_attr.spp"
1 continue
  if(ierrl/=0) foundl=.false.
  if(present(found)) found = foundl
  if(ierrl==0 .and. .not. present(found) .and. .not. present(default) .and. .not. foundl) then
    call iotk_error_issue(ierrl,"iotk_scan_attr",__FILE__,__LINE__)
# 440 "iotk_attr.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.20 ")
# 440 "iotk_attr.spp"
call iotk_error_msg(ierrl,'Attribute not found')
# 440 "iotk_attr.spp"
call iotk_error_write(ierrl,"name",name)
    ierrl = - ierrl
  end if
  if(present(default) .and. .not. foundl) then
# 453 "iotk_attr.spp"
    val = default
# 455 "iotk_attr.spp"
  end if
  if(present(ierr)) then
    ierr = ierrl
  else
    if(ierrl/=0) call iotk_error_handler(ierrl)
  end if
end subroutine iotk_scan_attr_LOGICAL4_4
# 463 "iotk_attr.spp"

#endif
#endif

subroutine iotk_attr_dummy_LOGICAL4_4
  write(0,*)
end subroutine iotk_attr_dummy_LOGICAL4_4

# 45 "iotk_attr.spp"

# 65 "iotk_attr.spp"


!------------------------------------------------------------------------------!
! Inclusion of configuration file
#include "iotk_config.h"
!------------------------------------------------------------------------------!

# 74 "iotk_attr.spp"
#include "iotk_auxmacros.h"
# 76 "iotk_attr.spp"

# 78 "iotk_attr.spp"

#ifdef __IOTK_LOGICAL4
#if 5 <= __IOTK_MAXRANK

# 158 "iotk_attr.spp"

# 233 "iotk_attr.spp"

# 236 "iotk_attr.spp"
subroutine iotk_write_attr_LOGICAL4_5(attr,name,val,dummy,first,newline,ierr)
  use iotk_base
  use iotk_error_interf
  use iotk_attr_interf
  use iotk_str_interf
  use iotk_misc_interf
  implicit none
  character(*), intent(inout) :: attr
  character(*), intent(in)    :: name
  LOGICAL(kind=iotk_LOGICAL4), intent(in)  :: val (:,:,:,:,:)
  type(iotk_dummytype), optional :: dummy
  logical, optional, intent(in)  :: first
  logical, optional, intent(in)  :: newline
  integer, optional, intent(out) :: ierr
  integer :: ierrl
  integer :: attlen
  integer :: vallen
  integer :: namlen
  character :: delim
# 258 "iotk_attr.spp"
  character(iotk_vallenx) :: tmpval
  logical :: nl
  if(present(newline)) then
    nl = newline
  else
    nl = .false.
  endif
  ierrl = 0
  if(present(first)) then
    if(first) attr(1:1) = iotk_eos
  end if
  attlen = iotk_strlen_trim(attr)
  namlen = iotk_strlen_trim(name)
  if(.not.iotk_check_name(name)) then
    call iotk_error_issue(ierrl,"iotk_write_attr",__FILE__,__LINE__)
# 272 "iotk_attr.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.20 ")
# 272 "iotk_attr.spp"
call iotk_error_msg(ierrl,'Wrong tag name')
# 272 "iotk_attr.spp"
call iotk_error_write(ierrl,"name",name(1:namlen))
    goto 1
  end if
# 289 "iotk_attr.spp"
  delim = '"'
# 293 "iotk_attr.spp"
  call iotk_write(pack(val,mask=.true.),tmpval,ierrl)
# 295 "iotk_attr.spp"
  if(ierrl/=0) then
    call iotk_error_issue(ierrl,"iotk_write_attr",__FILE__,__LINE__)
# 296 "iotk_attr.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.20 ")
    goto 1
  end if
# 300 "iotk_attr.spp"
  vallen = iotk_strlen(tmpval)
  if(attlen+vallen+namlen+5>len(attr)) then
    call iotk_error_issue(ierrl,"iotk_write_attr",__FILE__,__LINE__)
# 302 "iotk_attr.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.20 ")
# 302 "iotk_attr.spp"
call iotk_error_msg(ierrl,'Attribute dummy argument is too short')
    goto 1
  end if
  if(.not. nl) then
    attr(attlen+1:attlen+vallen+namlen+5) = " "//name(1:namlen)//"="//delim//tmpval(1:vallen)//delim//iotk_eos
  else
    attr(attlen+1:attlen+vallen+namlen+len(iotk_newline)+5) &
       = iotk_newline//" "//name(1:namlen)//"="//delim//tmpval(1:vallen)//delim//iotk_eos
  endif
1 continue
  if(present(ierr)) then
    ierr = ierrl
  else
    if(ierrl/=0) call iotk_error_handler(ierrl)
  end if
end subroutine iotk_write_attr_LOGICAL4_5

# 320 "iotk_attr.spp"
subroutine iotk_scan_attr_LOGICAL4_5(attr,name,val,dummy,found,default,eos,ierr)
  use iotk_base
  use iotk_error_interf
  use iotk_attr_interf, only : iotk_read
  use iotk_str_interf
  use iotk_misc_interf
  implicit none
  character(*),             intent(in)  :: attr
  character(*),             intent(in)  :: name
#ifdef __IOTK_WORKAROUND6
  LOGICAL(kind=iotk_LOGICAL4)                        :: val (:,:,:,:,:)
#else
  LOGICAL(kind=iotk_LOGICAL4), intent(out)           :: val (:,:,:,:,:)
#endif
  type(iotk_dummytype), optional :: dummy
  logical,        optional, intent(out) :: found
  LOGICAL(kind=iotk_LOGICAL4), optional, intent(in)  :: default (:,:,:,:,:)
  logical,        optional, intent(in)  :: eos
  integer,        optional, intent(out) :: ierr
  integer :: ierrl
  integer :: attlen,pos,equal,namlen
  character :: delim
  logical :: foundl
  character(iotk_vallenx) :: valc
# 351 "iotk_attr.spp"
  integer :: index
  LOGICAL(kind=iotk_LOGICAL4), allocatable :: tmpval (:)
# 354 "iotk_attr.spp"
  ierrl = 0
  attlen=iotk_strlen(attr)
  namlen=iotk_strlen_trim(name)
  foundl = .false.
  equal = 0
  do
    pos = verify(attr(equal+1:attlen)," ")
    if(pos<=0) exit
    equal = equal + pos
    pos = scan(attr(equal+1:attlen),"=")
    if(pos<=0) then
      call iotk_error_issue(ierrl,"iotk_scan_attr",__FILE__,__LINE__)
# 365 "iotk_attr.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.20 ")
# 365 "iotk_attr.spp"
call iotk_error_msg(ierrl,'')
# 365 "iotk_attr.spp"
call iotk_error_write(ierrl,"attr",attr(equal+1:attlen))
      goto 1
    end if
    equal = equal + pos
    if(trim(attr(equal-pos:equal-1))==name(1:namlen)) foundl = .true.
    pos = verify(attr(equal+1:attlen)," ")
    if(pos<=0) then
      call iotk_error_issue(ierrl,"iotk_scan_attr",__FILE__,__LINE__)
# 372 "iotk_attr.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.20 ")
      goto 1
    end if
    equal = equal + pos
    delim = attr(equal:equal)
    if(delim/="'" .and. delim/='"') then
      call iotk_error_issue(ierrl,"iotk_scan_attr",__FILE__,__LINE__)
# 378 "iotk_attr.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.20 ")
      goto 1
    end if
    pos = scan(attr(equal+1:attlen),delim)
    if(pos<=0) then
      call iotk_error_issue(ierrl,"iotk_scan_attr",__FILE__,__LINE__)
# 383 "iotk_attr.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.20 ")
      goto 1
    end if
    if(foundl) exit
    equal = equal + pos
  end do
  if(foundl) then
    call iotk_strcpy(valc,attr(equal+1:equal+pos-1),ierrl)
    if(ierrl/=0) then
      call iotk_error_issue(ierrl,"iotk_scan_attr",__FILE__,__LINE__)
# 392 "iotk_attr.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.20 ")
      goto 1
    end if
  else
    goto 1
  end if
# 413 "iotk_attr.spp"
  allocate(tmpval(size(val)))
  index = 0
  call iotk_str_clean(valc)
  call iotk_read(tmpval,valc(1:iotk_strlen(valc)),index,ierrl)
  if(ierrl/=0) then
    call iotk_error_issue(ierrl,"iotk_scan_attr",__FILE__,__LINE__)
# 418 "iotk_attr.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.20 ")
    goto 1
  end if
# 424 "iotk_attr.spp"
  if(index/=size(val)) then
# 426 "iotk_attr.spp"
    call iotk_error_issue(ierrl,"iotk_scan_attr",__FILE__,__LINE__)
# 426 "iotk_attr.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.20 ")
# 426 "iotk_attr.spp"
call iotk_error_msg(ierrl,'Attribute size does not match')
# 426 "iotk_attr.spp"
call iotk_error_write(ierrl,"attr",valc)
# 426 "iotk_attr.spp"
call iotk_error_write(ierrl,"size",size(tmpval))
    goto 1
  end if
# 432 "iotk_attr.spp"
  val = reshape (source=tmpval,shape=shape(val))
# 434 "iotk_attr.spp"
  deallocate(tmpval)
# 436 "iotk_attr.spp"
1 continue
  if(ierrl/=0) foundl=.false.
  if(present(found)) found = foundl
  if(ierrl==0 .and. .not. present(found) .and. .not. present(default) .and. .not. foundl) then
    call iotk_error_issue(ierrl,"iotk_scan_attr",__FILE__,__LINE__)
# 440 "iotk_attr.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.20 ")
# 440 "iotk_attr.spp"
call iotk_error_msg(ierrl,'Attribute not found')
# 440 "iotk_attr.spp"
call iotk_error_write(ierrl,"name",name)
    ierrl = - ierrl
  end if
  if(present(default) .and. .not. foundl) then
# 453 "iotk_attr.spp"
    val = default
# 455 "iotk_attr.spp"
  end if
  if(present(ierr)) then
    ierr = ierrl
  else
    if(ierrl/=0) call iotk_error_handler(ierrl)
  end if
end subroutine iotk_scan_attr_LOGICAL4_5
# 463 "iotk_attr.spp"

#endif
#endif

subroutine iotk_attr_dummy_LOGICAL4_5
  write(0,*)
end subroutine iotk_attr_dummy_LOGICAL4_5

# 45 "iotk_attr.spp"

# 48 "iotk_attr.spp"
! Input/Output Tool Kit (IOTK)
! Copyright (C) 2004-2006 Giovanni Bussi
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
# 65 "iotk_attr.spp"


!------------------------------------------------------------------------------!
! Inclusion of configuration file
#include "iotk_config.h"
!------------------------------------------------------------------------------!

# 74 "iotk_attr.spp"
#include "iotk_auxmacros.h"
# 76 "iotk_attr.spp"

# 78 "iotk_attr.spp"

#ifdef __IOTK_LOGICAL4
#if 6 <= __IOTK_MAXRANK

# 158 "iotk_attr.spp"

# 233 "iotk_attr.spp"

# 236 "iotk_attr.spp"
subroutine iotk_write_attr_LOGICAL4_6(attr,name,val,dummy,first,newline,ierr)
  use iotk_base
  use iotk_error_interf
  use iotk_attr_interf
  use iotk_str_interf
  use iotk_misc_interf
  implicit none
  character(*), intent(inout) :: attr
  character(*), intent(in)    :: name
  LOGICAL(kind=iotk_LOGICAL4), intent(in)  :: val (:,:,:,:,:,:)
  type(iotk_dummytype), optional :: dummy
  logical, optional, intent(in)  :: first
  logical, optional, intent(in)  :: newline
  integer, optional, intent(out) :: ierr
  integer :: ierrl
  integer :: attlen
  integer :: vallen
  integer :: namlen
  character :: delim
# 258 "iotk_attr.spp"
  character(iotk_vallenx) :: tmpval
  logical :: nl
  if(present(newline)) then
    nl = newline
  else
    nl = .false.
  endif
  ierrl = 0
  if(present(first)) then
    if(first) attr(1:1) = iotk_eos
  end if
  attlen = iotk_strlen_trim(attr)
  namlen = iotk_strlen_trim(name)
  if(.not.iotk_check_name(name)) then
    call iotk_error_issue(ierrl,"iotk_write_attr",__FILE__,__LINE__)
# 272 "iotk_attr.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.20 ")
# 272 "iotk_attr.spp"
call iotk_error_msg(ierrl,'Wrong tag name')
# 272 "iotk_attr.spp"
call iotk_error_write(ierrl,"name",name(1:namlen))
    goto 1
  end if
# 289 "iotk_attr.spp"
  delim = '"'
# 293 "iotk_attr.spp"
  call iotk_write(pack(val,mask=.true.),tmpval,ierrl)
# 295 "iotk_attr.spp"
  if(ierrl/=0) then
    call iotk_error_issue(ierrl,"iotk_write_attr",__FILE__,__LINE__)
# 296 "iotk_attr.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.20 ")
    goto 1
  end if
# 300 "iotk_attr.spp"
  vallen = iotk_strlen(tmpval)
  if(attlen+vallen+namlen+5>len(attr)) then
    call iotk_error_issue(ierrl,"iotk_write_attr",__FILE__,__LINE__)
# 302 "iotk_attr.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.20 ")
# 302 "iotk_attr.spp"
call iotk_error_msg(ierrl,'Attribute dummy argument is too short')
    goto 1
  end if
  if(.not. nl) then
    attr(attlen+1:attlen+vallen+namlen+5) = " "//name(1:namlen)//"="//delim//tmpval(1:vallen)//delim//iotk_eos
  else
    attr(attlen+1:attlen+vallen+namlen+len(iotk_newline)+5) &
       = iotk_newline//" "//name(1:namlen)//"="//delim//tmpval(1:vallen)//delim//iotk_eos
  endif
1 continue
  if(present(ierr)) then
    ierr = ierrl
  else
    if(ierrl/=0) call iotk_error_handler(ierrl)
  end if
end subroutine iotk_write_attr_LOGICAL4_6

# 320 "iotk_attr.spp"
subroutine iotk_scan_attr_LOGICAL4_6(attr,name,val,dummy,found,default,eos,ierr)
  use iotk_base
  use iotk_error_interf
  use iotk_attr_interf, only : iotk_read
  use iotk_str_interf
  use iotk_misc_interf
  implicit none
  character(*),             intent(in)  :: attr
  character(*),             intent(in)  :: name
#ifdef __IOTK_WORKAROUND6
  LOGICAL(kind=iotk_LOGICAL4)                        :: val (:,:,:,:,:,:)
#else
  LOGICAL(kind=iotk_LOGICAL4), intent(out)           :: val (:,:,:,:,:,:)
#endif
  type(iotk_dummytype), optional :: dummy
  logical,        optional, intent(out) :: found
  LOGICAL(kind=iotk_LOGICAL4), optional, intent(in)  :: default (:,:,:,:,:,:)
  logical,        optional, intent(in)  :: eos
  integer,        optional, intent(out) :: ierr
  integer :: ierrl
  integer :: attlen,pos,equal,namlen
  character :: delim
  logical :: foundl
  character(iotk_vallenx) :: valc
# 351 "iotk_attr.spp"
  integer :: index
  LOGICAL(kind=iotk_LOGICAL4), allocatable :: tmpval (:)
# 354 "iotk_attr.spp"
  ierrl = 0
  attlen=iotk_strlen(attr)
  namlen=iotk_strlen_trim(name)
  foundl = .false.
  equal = 0
  do
    pos = verify(attr(equal+1:attlen)," ")
    if(pos<=0) exit
    equal = equal + pos
    pos = scan(attr(equal+1:attlen),"=")
    if(pos<=0) then
      call iotk_error_issue(ierrl,"iotk_scan_attr",__FILE__,__LINE__)
# 365 "iotk_attr.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.20 ")
# 365 "iotk_attr.spp"
call iotk_error_msg(ierrl,'')
# 365 "iotk_attr.spp"
call iotk_error_write(ierrl,"attr",attr(equal+1:attlen))
      goto 1
    end if
    equal = equal + pos
    if(trim(attr(equal-pos:equal-1))==name(1:namlen)) foundl = .true.
    pos = verify(attr(equal+1:attlen)," ")
    if(pos<=0) then
      call iotk_error_issue(ierrl,"iotk_scan_attr",__FILE__,__LINE__)
# 372 "iotk_attr.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.20 ")
      goto 1
    end if
    equal = equal + pos
    delim = attr(equal:equal)
    if(delim/="'" .and. delim/='"') then
      call iotk_error_issue(ierrl,"iotk_scan_attr",__FILE__,__LINE__)
# 378 "iotk_attr.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.20 ")
      goto 1
    end if
    pos = scan(attr(equal+1:attlen),delim)
    if(pos<=0) then
      call iotk_error_issue(ierrl,"iotk_scan_attr",__FILE__,__LINE__)
# 383 "iotk_attr.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.20 ")
      goto 1
    end if
    if(foundl) exit
    equal = equal + pos
  end do
  if(foundl) then
    call iotk_strcpy(valc,attr(equal+1:equal+pos-1),ierrl)
    if(ierrl/=0) then
      call iotk_error_issue(ierrl,"iotk_scan_attr",__FILE__,__LINE__)
# 392 "iotk_attr.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.20 ")
      goto 1
    end if
  else
    goto 1
  end if
# 413 "iotk_attr.spp"
  allocate(tmpval(size(val)))
  index = 0
  call iotk_str_clean(valc)
  call iotk_read(tmpval,valc(1:iotk_strlen(valc)),index,ierrl)
  if(ierrl/=0) then
    call iotk_error_issue(ierrl,"iotk_scan_attr",__FILE__,__LINE__)
# 418 "iotk_attr.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.20 ")
    goto 1
  end if
# 424 "iotk_attr.spp"
  if(index/=size(val)) then
# 426 "iotk_attr.spp"
    call iotk_error_issue(ierrl,"iotk_scan_attr",__FILE__,__LINE__)
# 426 "iotk_attr.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.20 ")
# 426 "iotk_attr.spp"
call iotk_error_msg(ierrl,'Attribute size does not match')
# 426 "iotk_attr.spp"
call iotk_error_write(ierrl,"attr",valc)
# 426 "iotk_attr.spp"
call iotk_error_write(ierrl,"size",size(tmpval))
    goto 1
  end if
# 432 "iotk_attr.spp"
  val = reshape (source=tmpval,shape=shape(val))
# 434 "iotk_attr.spp"
  deallocate(tmpval)
# 436 "iotk_attr.spp"
1 continue
  if(ierrl/=0) foundl=.false.
  if(present(found)) found = foundl
  if(ierrl==0 .and. .not. present(found) .and. .not. present(default) .and. .not. foundl) then
    call iotk_error_issue(ierrl,"iotk_scan_attr",__FILE__,__LINE__)
# 440 "iotk_attr.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.20 ")
# 440 "iotk_attr.spp"
call iotk_error_msg(ierrl,'Attribute not found')
# 440 "iotk_attr.spp"
call iotk_error_write(ierrl,"name",name)
    ierrl = - ierrl
  end if
  if(present(default) .and. .not. foundl) then
# 453 "iotk_attr.spp"
    val = default
# 455 "iotk_attr.spp"
  end if
  if(present(ierr)) then
    ierr = ierrl
  else
    if(ierrl/=0) call iotk_error_handler(ierrl)
  end if
end subroutine iotk_scan_attr_LOGICAL4_6
# 463 "iotk_attr.spp"

#endif
#endif

subroutine iotk_attr_dummy_LOGICAL4_6
  write(0,*)
end subroutine iotk_attr_dummy_LOGICAL4_6

# 45 "iotk_attr.spp"

# 65 "iotk_attr.spp"


!------------------------------------------------------------------------------!
! Inclusion of configuration file
#include "iotk_config.h"
!------------------------------------------------------------------------------!

# 74 "iotk_attr.spp"
#include "iotk_auxmacros.h"
# 76 "iotk_attr.spp"

# 78 "iotk_attr.spp"

#ifdef __IOTK_LOGICAL4
#if 7 <= __IOTK_MAXRANK

# 158 "iotk_attr.spp"

# 233 "iotk_attr.spp"

# 236 "iotk_attr.spp"
subroutine iotk_write_attr_LOGICAL4_7(attr,name,val,dummy,first,newline,ierr)
  use iotk_base
  use iotk_error_interf
  use iotk_attr_interf
  use iotk_str_interf
  use iotk_misc_interf
  implicit none
  character(*), intent(inout) :: attr
  character(*), intent(in)    :: name
  LOGICAL(kind=iotk_LOGICAL4), intent(in)  :: val (:,:,:,:,:,:,:)
  type(iotk_dummytype), optional :: dummy
  logical, optional, intent(in)  :: first
  logical, optional, intent(in)  :: newline
  integer, optional, intent(out) :: ierr
  integer :: ierrl
  integer :: attlen
  integer :: vallen
  integer :: namlen
  character :: delim
# 258 "iotk_attr.spp"
  character(iotk_vallenx) :: tmpval
  logical :: nl
  if(present(newline)) then
    nl = newline
  else
    nl = .false.
  endif
  ierrl = 0
  if(present(first)) then
    if(first) attr(1:1) = iotk_eos
  end if
  attlen = iotk_strlen_trim(attr)
  namlen = iotk_strlen_trim(name)
  if(.not.iotk_check_name(name)) then
    call iotk_error_issue(ierrl,"iotk_write_attr",__FILE__,__LINE__)
# 272 "iotk_attr.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.20 ")
# 272 "iotk_attr.spp"
call iotk_error_msg(ierrl,'Wrong tag name')
# 272 "iotk_attr.spp"
call iotk_error_write(ierrl,"name",name(1:namlen))
    goto 1
  end if
# 289 "iotk_attr.spp"
  delim = '"'
# 293 "iotk_attr.spp"
  call iotk_write(pack(val,mask=.true.),tmpval,ierrl)
# 295 "iotk_attr.spp"
  if(ierrl/=0) then
    call iotk_error_issue(ierrl,"iotk_write_attr",__FILE__,__LINE__)
# 296 "iotk_attr.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.20 ")
    goto 1
  end if
# 300 "iotk_attr.spp"
  vallen = iotk_strlen(tmpval)
  if(attlen+vallen+namlen+5>len(attr)) then
    call iotk_error_issue(ierrl,"iotk_write_attr",__FILE__,__LINE__)
# 302 "iotk_attr.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.20 ")
# 302 "iotk_attr.spp"
call iotk_error_msg(ierrl,'Attribute dummy argument is too short')
    goto 1
  end if
  if(.not. nl) then
    attr(attlen+1:attlen+vallen+namlen+5) = " "//name(1:namlen)//"="//delim//tmpval(1:vallen)//delim//iotk_eos
  else
    attr(attlen+1:attlen+vallen+namlen+len(iotk_newline)+5) &
       = iotk_newline//" "//name(1:namlen)//"="//delim//tmpval(1:vallen)//delim//iotk_eos
  endif
1 continue
  if(present(ierr)) then
    ierr = ierrl
  else
    if(ierrl/=0) call iotk_error_handler(ierrl)
  end if
end subroutine iotk_write_attr_LOGICAL4_7

# 320 "iotk_attr.spp"
subroutine iotk_scan_attr_LOGICAL4_7(attr,name,val,dummy,found,default,eos,ierr)
  use iotk_base
  use iotk_error_interf
  use iotk_attr_interf, only : iotk_read
  use iotk_str_interf
  use iotk_misc_interf
  implicit none
  character(*),             intent(in)  :: attr
  character(*),             intent(in)  :: name
#ifdef __IOTK_WORKAROUND6
  LOGICAL(kind=iotk_LOGICAL4)                        :: val (:,:,:,:,:,:,:)
#else
  LOGICAL(kind=iotk_LOGICAL4), intent(out)           :: val (:,:,:,:,:,:,:)
#endif
  type(iotk_dummytype), optional :: dummy
  logical,        optional, intent(out) :: found
  LOGICAL(kind=iotk_LOGICAL4), optional, intent(in)  :: default (:,:,:,:,:,:,:)
  logical,        optional, intent(in)  :: eos
  integer,        optional, intent(out) :: ierr
  integer :: ierrl
  integer :: attlen,pos,equal,namlen
  character :: delim
  logical :: foundl
  character(iotk_vallenx) :: valc
# 351 "iotk_attr.spp"
  integer :: index
  LOGICAL(kind=iotk_LOGICAL4), allocatable :: tmpval (:)
# 354 "iotk_attr.spp"
  ierrl = 0
  attlen=iotk_strlen(attr)
  namlen=iotk_strlen_trim(name)
  foundl = .false.
  equal = 0
  do
    pos = verify(attr(equal+1:attlen)," ")
    if(pos<=0) exit
    equal = equal + pos
    pos = scan(attr(equal+1:attlen),"=")
    if(pos<=0) then
      call iotk_error_issue(ierrl,"iotk_scan_attr",__FILE__,__LINE__)
# 365 "iotk_attr.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.20 ")
# 365 "iotk_attr.spp"
call iotk_error_msg(ierrl,'')
# 365 "iotk_attr.spp"
call iotk_error_write(ierrl,"attr",attr(equal+1:attlen))
      goto 1
    end if
    equal = equal + pos
    if(trim(attr(equal-pos:equal-1))==name(1:namlen)) foundl = .true.
    pos = verify(attr(equal+1:attlen)," ")
    if(pos<=0) then
      call iotk_error_issue(ierrl,"iotk_scan_attr",__FILE__,__LINE__)
# 372 "iotk_attr.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.20 ")
      goto 1
    end if
    equal = equal + pos
    delim = attr(equal:equal)
    if(delim/="'" .and. delim/='"') then
      call iotk_error_issue(ierrl,"iotk_scan_attr",__FILE__,__LINE__)
# 378 "iotk_attr.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.20 ")
      goto 1
    end if
    pos = scan(attr(equal+1:attlen),delim)
    if(pos<=0) then
      call iotk_error_issue(ierrl,"iotk_scan_attr",__FILE__,__LINE__)
# 383 "iotk_attr.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.20 ")
      goto 1
    end if
    if(foundl) exit
    equal = equal + pos
  end do
  if(foundl) then
    call iotk_strcpy(valc,attr(equal+1:equal+pos-1),ierrl)
    if(ierrl/=0) then
      call iotk_error_issue(ierrl,"iotk_scan_attr",__FILE__,__LINE__)
# 392 "iotk_attr.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.20 ")
      goto 1
    end if
  else
    goto 1
  end if
# 413 "iotk_attr.spp"
  allocate(tmpval(size(val)))
  index = 0
  call iotk_str_clean(valc)
  call iotk_read(tmpval,valc(1:iotk_strlen(valc)),index,ierrl)
  if(ierrl/=0) then
    call iotk_error_issue(ierrl,"iotk_scan_attr",__FILE__,__LINE__)
# 418 "iotk_attr.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.20 ")
    goto 1
  end if
# 424 "iotk_attr.spp"
  if(index/=size(val)) then
# 426 "iotk_attr.spp"
    call iotk_error_issue(ierrl,"iotk_scan_attr",__FILE__,__LINE__)
# 426 "iotk_attr.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.20 ")
# 426 "iotk_attr.spp"
call iotk_error_msg(ierrl,'Attribute size does not match')
# 426 "iotk_attr.spp"
call iotk_error_write(ierrl,"attr",valc)
# 426 "iotk_attr.spp"
call iotk_error_write(ierrl,"size",size(tmpval))
    goto 1
  end if
# 432 "iotk_attr.spp"
  val = reshape (source=tmpval,shape=shape(val))
# 434 "iotk_attr.spp"
  deallocate(tmpval)
# 436 "iotk_attr.spp"
1 continue
  if(ierrl/=0) foundl=.false.
  if(present(found)) found = foundl
  if(ierrl==0 .and. .not. present(found) .and. .not. present(default) .and. .not. foundl) then
    call iotk_error_issue(ierrl,"iotk_scan_attr",__FILE__,__LINE__)
# 440 "iotk_attr.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.20 ")
# 440 "iotk_attr.spp"
call iotk_error_msg(ierrl,'Attribute not found')
# 440 "iotk_attr.spp"
call iotk_error_write(ierrl,"name",name)
    ierrl = - ierrl
  end if
  if(present(default) .and. .not. foundl) then
# 453 "iotk_attr.spp"
    val = default
# 455 "iotk_attr.spp"
  end if
  if(present(ierr)) then
    ierr = ierrl
  else
    if(ierrl/=0) call iotk_error_handler(ierrl)
  end if
end subroutine iotk_scan_attr_LOGICAL4_7
# 463 "iotk_attr.spp"

#endif
#endif

subroutine iotk_attr_dummy_LOGICAL4_7
  write(0,*)
end subroutine iotk_attr_dummy_LOGICAL4_7

# 45 "iotk_attr.spp"

