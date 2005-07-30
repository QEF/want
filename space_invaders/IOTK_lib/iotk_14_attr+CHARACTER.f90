# 48 "iotk_attr.spp"
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
# 65 "iotk_attr.spp"


!------------------------------------------------------------------------------!
! Inclusion of configuration file
#include "iotk_config.h"
!------------------------------------------------------------------------------!

# 74 "iotk_attr.spp"
#include "iotk_auxmacros.h"
# 76 "iotk_attr.spp"

# 78 "iotk_attr.spp"

#ifdef __IOTK_CHARACTER1
#if 0 <= __IOTK_MAXRANK

# 83 "iotk_attr.spp"
! This is needed as a workaround for bugged pack 
subroutine iotk_private_pack_CHARACTER1(out,in,n,l)
    use iotk_base
    implicit none
    integer,                                    intent(in)  :: n,l
# 89 "iotk_attr.spp"
    CHARACTER (kind=__IOTK_CHARACTER1,len=l), intent(out) :: out(n)
    CHARACTER (kind=__IOTK_CHARACTER1,len=l), intent(in)  :: in(n)
# 95 "iotk_attr.spp"
    out = in
end subroutine iotk_private_pack_CHARACTER1

# 156 "iotk_attr.spp"

# 229 "iotk_attr.spp"

# 232 "iotk_attr.spp"
subroutine iotk_write_attr_CHARACTER1_0(attr,name,val,dummy,first,ierr)
  use iotk_base
  use iotk_error_interf
  use iotk_attr_interf
  use iotk_str_interf
  use iotk_misc_interf
  implicit none
  character(*), intent(inout) :: attr
  character(*), intent(in)    :: name
  CHARACTER(kind=__IOTK_CHARACTER1,len=*), intent(in)  :: val 
  type(iotk_dummytype), optional :: dummy
  logical, optional, intent(in)  :: first
  integer, optional, intent(out) :: ierr
  integer :: ierrl
  integer :: attlen
  integer :: vallen
  integer :: namlen
  integer :: iostat
  character :: delim
# 252 "iotk_attr.spp"
  logical :: lquot,lapos
# 254 "iotk_attr.spp"
  character(iotk_vallenx) :: tmpval
  ierrl = 0
  iostat = 0
  if(present(first)) then
    if(first) attr(1:1) = iotk_eos
  end if
  if(.not.iotk_check_name(name)) then
    call iotk_error_issue(ierrl,"iotk_write_attr",__FILE__,__LINE__)
# 261 "iotk_attr.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.10 ")
# 261 "iotk_attr.spp"
call iotk_error_msg(ierrl,'Wrong tag name')
# 261 "iotk_attr.spp"
call iotk_error_write(ierrl,"name",trim(name))
    goto 1
  end if
  attlen = iotk_strlen(attr)
  if(attlen==len(attr)) attlen = len_trim(attr)
  namlen = len_trim(name)
# 268 "iotk_attr.spp"
  lquot=iotk_strscan(val,'"')>0
  lapos=iotk_strscan(val,"'")>0
  if(.not.lquot) then
    delim='"'
    call iotk_deescape(tmpval,val)
  else if(.not.lapos) then
    delim="'"
    call iotk_deescape(tmpval,val)
  else
    delim='"'
    call iotk_deescape(tmpval,val,quot=.true.,apos=.true.)
  end if
# 292 "iotk_attr.spp"
  vallen = iotk_strlen(tmpval)
  if(attlen+vallen+namlen+5>len(attr)) then
    call iotk_error_issue(ierrl,"iotk_write_attr",__FILE__,__LINE__)
# 294 "iotk_attr.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.10 ")
# 294 "iotk_attr.spp"
call iotk_error_msg(ierrl,'Attribute dummy argument is too short')
    goto 1
  end if
  attr(attlen+1:attlen+vallen+namlen+5) = " "//trim(name)//"="//delim//tmpval(1:vallen)//delim//iotk_eos
1 continue
  if(present(ierr)) then
    ierr = ierrl
  else
    if(ierrl/=0) call iotk_error_handler(ierrl)
  end if
end subroutine iotk_write_attr_CHARACTER1_0

# 307 "iotk_attr.spp"
subroutine iotk_scan_attr_CHARACTER1_0(attr,name,val,dummy,found,default,eos,ierr)
  use iotk_base
  use iotk_error_interf
  use iotk_attr_interf, only : iotk_read
  use iotk_str_interf
  use iotk_misc_interf
  implicit none
  character(*),             intent(in)  :: attr
  character(*),             intent(in)  :: name
#ifdef __IOTK_WORKAROUND6
  CHARACTER(kind=__IOTK_CHARACTER1,len=*)                        :: val 
#else
  CHARACTER(kind=__IOTK_CHARACTER1,len=*), intent(out)           :: val 
#endif
  type(iotk_dummytype), optional :: dummy
  logical,        optional, intent(out) :: found
  CHARACTER(kind=__IOTK_CHARACTER1,len=*), optional, intent(in)  :: default 
  logical,        optional, intent(in)  :: eos
  integer,        optional, intent(out) :: ierr
  integer :: ierrl
  integer :: attlen,pos,equal
  character :: delim
  logical :: foundl
  character(iotk_vallenx) :: valc
# 332 "iotk_attr.spp"
  character(iotk_vallenx) :: valctmp
  integer :: vallen,defaultlen
  logical :: leos
# 339 "iotk_attr.spp"
  ierrl = 0
  attlen=iotk_strlen(attr)
  foundl = .false.
  equal = 0
  do
    pos = verify(attr(equal+1:attlen)," ")
    if(pos<=0) exit
    equal = equal + pos
    pos = scan(attr(equal+1:attlen),"=")
    if(pos<=0) then
      call iotk_error_issue(ierrl,"iotk_scan_attr",__FILE__,__LINE__)
# 349 "iotk_attr.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.10 ")
# 349 "iotk_attr.spp"
call iotk_error_msg(ierrl,'')
# 349 "iotk_attr.spp"
call iotk_error_write(ierrl,"attr",attr(equal+1:attlen))
      goto 1
    end if
    equal = equal + pos
    if(trim(attr(equal-pos:equal-1))==trim(name)) foundl = .true.
    pos = verify(attr(equal+1:attlen)," ")
    if(pos<=0) then
      call iotk_error_issue(ierrl,"iotk_scan_attr",__FILE__,__LINE__)
# 356 "iotk_attr.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.10 ")
      goto 1
    end if
    equal = equal + pos
    delim = attr(equal:equal)
    if(delim/="'" .and. delim/='"') then
      call iotk_error_issue(ierrl,"iotk_scan_attr",__FILE__,__LINE__)
# 362 "iotk_attr.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.10 ")
      goto 1
    end if
    pos = scan(attr(equal+1:attlen),delim)
    if(pos<=0) then
      call iotk_error_issue(ierrl,"iotk_scan_attr",__FILE__,__LINE__)
# 367 "iotk_attr.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.10 ")
      goto 1
    end if
    if(foundl) exit
    equal = equal + pos
  end do
  if(foundl) then
    call iotk_strcpy(valc,attr(equal+1:equal+pos-1),ierrl)
    if(ierrl/=0) then
      call iotk_error_issue(ierrl,"iotk_scan_attr",__FILE__,__LINE__)
# 376 "iotk_attr.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.10 ")
      goto 1
    end if
  else
    goto 1
  end if
# 383 "iotk_attr.spp"
  call iotk_escape(valctmp,valc)
  vallen = iotk_strlen(valctmp)
  if(len(val) < vallen) then
    call iotk_error_issue(ierrl,"iotk_scan_attr",__FILE__,__LINE__)
# 386 "iotk_attr.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.10 ")
    goto 1
  end if
  leos=.false.
  if(present(eos)) leos=eos
  val(1:vallen) = valctmp(1:vallen)
  if(len(val) > vallen) then
    val(vallen+1:vallen+1) = iotk_eos
    if(.not.leos) then
      val(vallen+1:)=" "
    end if
  end if
# 422 "iotk_attr.spp"
1 continue
  if(ierrl/=0) foundl=.false.
  if(present(found)) found = foundl
  if(ierrl==0 .and. .not. present(found) .and. .not. present(default) .and. .not. foundl) then
    call iotk_error_issue(ierrl,"iotk_scan_attr",__FILE__,__LINE__)
# 426 "iotk_attr.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.10 ")
# 426 "iotk_attr.spp"
call iotk_error_msg(ierrl,'Attribute not found')
# 426 "iotk_attr.spp"
call iotk_error_write(ierrl,"name",name)
    ierrl = - ierrl
  end if
  if(present(default) .and. .not. foundl) then
# 431 "iotk_attr.spp"
    if(leos) then
      defaultlen = min(iotk_strlen(default),len(val))
      val(1:defaultlen) = default(1:defaultlen)
      if(defaultlen<len(val)) val(defaultlen+1:defaultlen+1)=iotk_eos
    else
      val = default
    end if
# 441 "iotk_attr.spp"
  end if
  if(present(ierr)) then
    ierr = ierrl
  else
    if(ierrl/=0) call iotk_error_handler(ierrl)
  end if
end subroutine iotk_scan_attr_CHARACTER1_0
# 449 "iotk_attr.spp"

#endif
#endif

subroutine iotk_attr_dummy_CHARACTER1_0
  write(0,*)
end subroutine iotk_attr_dummy_CHARACTER1_0

