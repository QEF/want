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

#ifdef __IOTK_REAL1
#if 0 <= __IOTK_MAXRANK

# 83 "iotk_attr.spp"
! This is needed as a workaround for bugged pack 
subroutine iotk_private_pack_REAL1(out,in,n,l)
    use iotk_base
    implicit none
    integer,                                    intent(in)  :: n,l
# 92 "iotk_attr.spp"
    REAL (kind=__IOTK_REAL1), intent(out) :: out(n)
    REAL (kind=__IOTK_REAL1), intent(in)  :: in(n)
# 95 "iotk_attr.spp"
    out = in
end subroutine iotk_private_pack_REAL1

# 100 "iotk_attr.spp"
subroutine iotk_write_REAL1(val,string,ierr)
  use iotk_base
  use iotk_error_interf
  use iotk_xtox_interf
  use iotk_fmt_interf
  use iotk_str_interf
  use iotk_misc_interf
  implicit none
  REAL(kind=__IOTK_REAL1), intent(in) :: val(:)
#ifdef __IOTK_WORKAROUND6
  character(len=*)              :: string
#else
  character(len=*), intent(out) :: string
#endif
  integer, intent(out) :: ierr
# 116 "iotk_attr.spp"
  character(len=100) :: tmpval
# 118 "iotk_attr.spp"
  integer :: index,iostat
  ierr = 0
  iostat = 0 
  string(1:1) = iotk_eos
  if(size(val)==0) return
  if(len(string)==0) then
    call iotk_error_issue(ierr,"iotk_write",__FILE__,__LINE__)
# 124 "iotk_attr.spp"
call iotk_error_msg(ierr,"CVS Revision: 1.15 ")
    return
  end if
  do index=1,size(val)
# 141 "iotk_attr.spp"
    write(tmpval,trim(iotk_wfmt("REAL",kind(val),1,-1," ")),iostat=iostat) val(index)
    if(iostat/=0) then
      call iotk_error_issue(ierr,"iotk_write",__FILE__,__LINE__)
# 143 "iotk_attr.spp"
call iotk_error_msg(ierr,"CVS Revision: 1.15 ")
# 143 "iotk_attr.spp"
call iotk_error_msg(ierr,' ')
# 143 "iotk_attr.spp"
call iotk_error_write(ierr,"iostat",iostat)
      return
    end if
    call iotk_strcat(string,trim(adjustl(tmpval))//" ",ierr)
    if(ierr/=0) then
      call iotk_error_issue(ierr,"iotk_write",__FILE__,__LINE__)
# 148 "iotk_attr.spp"
call iotk_error_msg(ierr,"CVS Revision: 1.15 ")
      return
    end if
# 152 "iotk_attr.spp"
  end do
! taglio l'ultimo spazio
  string(iotk_strlen(string):iotk_strlen(string)) = iotk_eos
end subroutine iotk_write_REAL1
# 158 "iotk_attr.spp"

# 162 "iotk_attr.spp"
subroutine iotk_read_REAL1(val,string,index,ierr)
  use iotk_base
  use iotk_error_interf
  use iotk_xtox_interf
  use iotk_misc_interf
  implicit none
  REAL(kind=__IOTK_REAL1), intent(inout) :: val(:)
  character(len=*), intent(in) :: string
  integer, intent(inout) :: index
  integer, intent(out) :: ierr
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
! PER ORA CONSIDERA LE VIRGOLE COME SPAZII
  do
    pos = verify(string(pos1+1:)," ,")+pos1
    if(pos==pos1) exit
    pos = pos - 1
    pos1 = scan(string(pos+1:)," ,")+pos
    if(pos1==pos) pos1 = len(string) + 1
!LEGGI string(pos+1:pos1-1)
    index = index+1
    if(index>maxindex) then
      call iotk_error_issue(ierr,"iotk_read",__FILE__,__LINE__)
# 200 "iotk_attr.spp"
call iotk_error_msg(ierr,"CVS Revision: 1.15 ")
# 200 "iotk_attr.spp"
call iotk_error_msg(ierr,'Too many data')
    end if
# 207 "iotk_attr.spp"
    read(string(pos+1:pos1-1),"(G100.95)",iostat=iostat) val(index)
# 223 "iotk_attr.spp"
    if(iostat/=0) then
      call iotk_error_issue(ierr,"iotk_read",__FILE__,__LINE__)
# 224 "iotk_attr.spp"
call iotk_error_msg(ierr,"CVS Revision: 1.15 ")
# 224 "iotk_attr.spp"
call iotk_error_msg(ierr,'Error reading from string')
# 224 "iotk_attr.spp"
call iotk_error_write(ierr,"string",string(pos+1:pos1-1))
# 224 "iotk_attr.spp"
call iotk_error_write(ierr,"iostat",iostat)
      return
    end if
# 228 "iotk_attr.spp"
    if(pos1>=len(string)) exit
  end do
end subroutine iotk_read_REAL1
# 233 "iotk_attr.spp"

# 236 "iotk_attr.spp"
subroutine iotk_write_attr_REAL1_0(attr,name,val,dummy,first,ierr)
  use iotk_base
  use iotk_error_interf
  use iotk_attr_interf
  use iotk_str_interf
  use iotk_misc_interf
  implicit none
  character(*), intent(inout) :: attr
  character(*), intent(in)    :: name
  REAL(kind=__IOTK_REAL1), intent(in)  :: val 
  type(iotk_dummytype), optional :: dummy
  logical, optional, intent(in)  :: first
  integer, optional, intent(out) :: ierr
  integer :: ierrl
  integer :: attlen
  integer :: vallen
  integer :: namlen
  character :: delim
# 257 "iotk_attr.spp"
  character(iotk_vallenx) :: tmpval
  ierrl = 0
  if(present(first)) then
    if(first) attr(1:1) = iotk_eos
  end if
  attlen = iotk_strlen_trim(attr)
  namlen = iotk_strlen_trim(name)
  if(.not.iotk_check_name(name)) then
    call iotk_error_issue(ierrl,"iotk_write_attr",__FILE__,__LINE__)
# 265 "iotk_attr.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.15 ")
# 265 "iotk_attr.spp"
call iotk_error_msg(ierrl,'Wrong tag name')
# 265 "iotk_attr.spp"
call iotk_error_write(ierrl,"name",name(1:namlen))
    goto 1
  end if
# 282 "iotk_attr.spp"
  delim = '"'
# 284 "iotk_attr.spp"
  call iotk_write((/val/),tmpval,ierrl)
# 288 "iotk_attr.spp"
  if(ierrl/=0) then
    call iotk_error_issue(ierrl,"iotk_write_attr",__FILE__,__LINE__)
# 289 "iotk_attr.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.15 ")
    goto 1
  end if
# 293 "iotk_attr.spp"
  vallen = iotk_strlen(tmpval)
  if(attlen+vallen+namlen+5>len(attr)) then
    call iotk_error_issue(ierrl,"iotk_write_attr",__FILE__,__LINE__)
# 295 "iotk_attr.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.15 ")
# 295 "iotk_attr.spp"
call iotk_error_msg(ierrl,'Attribute dummy argument is too short')
    goto 1
  end if
  attr(attlen+1:attlen+vallen+namlen+5) = " "//name(1:namlen)//"="//delim//tmpval(1:vallen)//delim//iotk_eos
1 continue
  if(present(ierr)) then
    ierr = ierrl
  else
    if(ierrl/=0) call iotk_error_handler(ierrl)
  end if
end subroutine iotk_write_attr_REAL1_0

# 308 "iotk_attr.spp"
subroutine iotk_scan_attr_REAL1_0(attr,name,val,dummy,found,default,eos,ierr)
  use iotk_base
  use iotk_error_interf
  use iotk_attr_interf, only : iotk_read
  use iotk_str_interf
  use iotk_misc_interf
  implicit none
  character(*),             intent(in)  :: attr
  character(*),             intent(in)  :: name
#ifdef __IOTK_WORKAROUND6
  REAL(kind=__IOTK_REAL1)                        :: val 
#else
  REAL(kind=__IOTK_REAL1), intent(out)           :: val 
#endif
  type(iotk_dummytype), optional :: dummy
  logical,        optional, intent(out) :: found
  REAL(kind=__IOTK_REAL1), optional, intent(in)  :: default 
  logical,        optional, intent(in)  :: eos
  integer,        optional, intent(out) :: ierr
  integer :: ierrl
  integer :: attlen,pos,equal,namlen
  character :: delim
  logical :: foundl
  character(iotk_vallenx) :: valc
# 337 "iotk_attr.spp"
  integer :: index
  REAL(kind=__IOTK_REAL1), allocatable :: tmpval (:)
# 340 "iotk_attr.spp"
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
# 351 "iotk_attr.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.15 ")
# 351 "iotk_attr.spp"
call iotk_error_msg(ierrl,'')
# 351 "iotk_attr.spp"
call iotk_error_write(ierrl,"attr",attr(equal+1:attlen))
      goto 1
    end if
    equal = equal + pos
    if(trim(attr(equal-pos:equal-1))==name(1:namlen)) foundl = .true.
    pos = verify(attr(equal+1:attlen)," ")
    if(pos<=0) then
      call iotk_error_issue(ierrl,"iotk_scan_attr",__FILE__,__LINE__)
# 358 "iotk_attr.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.15 ")
      goto 1
    end if
    equal = equal + pos
    delim = attr(equal:equal)
    if(delim/="'" .and. delim/='"') then
      call iotk_error_issue(ierrl,"iotk_scan_attr",__FILE__,__LINE__)
# 364 "iotk_attr.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.15 ")
      goto 1
    end if
    pos = scan(attr(equal+1:attlen),delim)
    if(pos<=0) then
      call iotk_error_issue(ierrl,"iotk_scan_attr",__FILE__,__LINE__)
# 369 "iotk_attr.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.15 ")
      goto 1
    end if
    if(foundl) exit
    equal = equal + pos
  end do
  if(foundl) then
    call iotk_strcpy(valc,attr(equal+1:equal+pos-1),ierrl)
    if(ierrl/=0) then
      call iotk_error_issue(ierrl,"iotk_scan_attr",__FILE__,__LINE__)
# 378 "iotk_attr.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.15 ")
      goto 1
    end if
  else
    goto 1
  end if
# 401 "iotk_attr.spp"
  allocate(tmpval(1))
  index = 0
  call iotk_str_clean(valc)
  call iotk_read(tmpval,valc(1:iotk_strlen(valc)),index,ierrl)
  if(ierrl/=0) then
    call iotk_error_issue(ierrl,"iotk_scan_attr",__FILE__,__LINE__)
# 406 "iotk_attr.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.15 ")
    goto 1
  end if
# 412 "iotk_attr.spp"
  if(index/=1) then
# 414 "iotk_attr.spp"
    call iotk_error_issue(ierrl,"iotk_scan_attr",__FILE__,__LINE__)
# 414 "iotk_attr.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.15 ")
# 414 "iotk_attr.spp"
call iotk_error_msg(ierrl,'Attribute size does not match')
# 414 "iotk_attr.spp"
call iotk_error_write(ierrl,"attr",valc)
# 414 "iotk_attr.spp"
call iotk_error_write(ierrl,"size",size(tmpval))
    goto 1
  end if
# 418 "iotk_attr.spp"
  val = tmpval(1)
# 422 "iotk_attr.spp"
  deallocate(tmpval)
# 424 "iotk_attr.spp"
1 continue
  if(ierrl/=0) foundl=.false.
  if(present(found)) found = foundl
  if(ierrl==0 .and. .not. present(found) .and. .not. present(default) .and. .not. foundl) then
    call iotk_error_issue(ierrl,"iotk_scan_attr",__FILE__,__LINE__)
# 428 "iotk_attr.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.15 ")
# 428 "iotk_attr.spp"
call iotk_error_msg(ierrl,'Attribute not found')
# 428 "iotk_attr.spp"
call iotk_error_write(ierrl,"name",name)
    ierrl = - ierrl
  end if
  if(present(default) .and. .not. foundl) then
# 441 "iotk_attr.spp"
    val = default
# 443 "iotk_attr.spp"
  end if
  if(present(ierr)) then
    ierr = ierrl
  else
    if(ierrl/=0) call iotk_error_handler(ierrl)
  end if
end subroutine iotk_scan_attr_REAL1_0
# 451 "iotk_attr.spp"

#endif
#endif

subroutine iotk_attr_dummy_REAL1_0
  write(0,*)
end subroutine iotk_attr_dummy_REAL1_0

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

#ifdef __IOTK_REAL1
#if 1 <= __IOTK_MAXRANK

# 158 "iotk_attr.spp"

# 233 "iotk_attr.spp"

# 236 "iotk_attr.spp"
subroutine iotk_write_attr_REAL1_1(attr,name,val,dummy,first,ierr)
  use iotk_base
  use iotk_error_interf
  use iotk_attr_interf
  use iotk_str_interf
  use iotk_misc_interf
  implicit none
  character(*), intent(inout) :: attr
  character(*), intent(in)    :: name
  REAL(kind=__IOTK_REAL1), intent(in)  :: val (:)
  type(iotk_dummytype), optional :: dummy
  logical, optional, intent(in)  :: first
  integer, optional, intent(out) :: ierr
  integer :: ierrl
  integer :: attlen
  integer :: vallen
  integer :: namlen
  character :: delim
# 257 "iotk_attr.spp"
  character(iotk_vallenx) :: tmpval
  ierrl = 0
  if(present(first)) then
    if(first) attr(1:1) = iotk_eos
  end if
  attlen = iotk_strlen_trim(attr)
  namlen = iotk_strlen_trim(name)
  if(.not.iotk_check_name(name)) then
    call iotk_error_issue(ierrl,"iotk_write_attr",__FILE__,__LINE__)
# 265 "iotk_attr.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.15 ")
# 265 "iotk_attr.spp"
call iotk_error_msg(ierrl,'Wrong tag name')
# 265 "iotk_attr.spp"
call iotk_error_write(ierrl,"name",name(1:namlen))
    goto 1
  end if
# 282 "iotk_attr.spp"
  delim = '"'
# 286 "iotk_attr.spp"
  call iotk_write(pack(val,mask=.true.),tmpval,ierrl)
# 288 "iotk_attr.spp"
  if(ierrl/=0) then
    call iotk_error_issue(ierrl,"iotk_write_attr",__FILE__,__LINE__)
# 289 "iotk_attr.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.15 ")
    goto 1
  end if
# 293 "iotk_attr.spp"
  vallen = iotk_strlen(tmpval)
  if(attlen+vallen+namlen+5>len(attr)) then
    call iotk_error_issue(ierrl,"iotk_write_attr",__FILE__,__LINE__)
# 295 "iotk_attr.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.15 ")
# 295 "iotk_attr.spp"
call iotk_error_msg(ierrl,'Attribute dummy argument is too short')
    goto 1
  end if
  attr(attlen+1:attlen+vallen+namlen+5) = " "//name(1:namlen)//"="//delim//tmpval(1:vallen)//delim//iotk_eos
1 continue
  if(present(ierr)) then
    ierr = ierrl
  else
    if(ierrl/=0) call iotk_error_handler(ierrl)
  end if
end subroutine iotk_write_attr_REAL1_1

# 308 "iotk_attr.spp"
subroutine iotk_scan_attr_REAL1_1(attr,name,val,dummy,found,default,eos,ierr)
  use iotk_base
  use iotk_error_interf
  use iotk_attr_interf, only : iotk_read
  use iotk_str_interf
  use iotk_misc_interf
  implicit none
  character(*),             intent(in)  :: attr
  character(*),             intent(in)  :: name
#ifdef __IOTK_WORKAROUND6
  REAL(kind=__IOTK_REAL1)                        :: val (:)
#else
  REAL(kind=__IOTK_REAL1), intent(out)           :: val (:)
#endif
  type(iotk_dummytype), optional :: dummy
  logical,        optional, intent(out) :: found
  REAL(kind=__IOTK_REAL1), optional, intent(in)  :: default (:)
  logical,        optional, intent(in)  :: eos
  integer,        optional, intent(out) :: ierr
  integer :: ierrl
  integer :: attlen,pos,equal,namlen
  character :: delim
  logical :: foundl
  character(iotk_vallenx) :: valc
# 337 "iotk_attr.spp"
  integer :: index
  REAL(kind=__IOTK_REAL1), allocatable :: tmpval (:)
# 340 "iotk_attr.spp"
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
# 351 "iotk_attr.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.15 ")
# 351 "iotk_attr.spp"
call iotk_error_msg(ierrl,'')
# 351 "iotk_attr.spp"
call iotk_error_write(ierrl,"attr",attr(equal+1:attlen))
      goto 1
    end if
    equal = equal + pos
    if(trim(attr(equal-pos:equal-1))==name(1:namlen)) foundl = .true.
    pos = verify(attr(equal+1:attlen)," ")
    if(pos<=0) then
      call iotk_error_issue(ierrl,"iotk_scan_attr",__FILE__,__LINE__)
# 358 "iotk_attr.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.15 ")
      goto 1
    end if
    equal = equal + pos
    delim = attr(equal:equal)
    if(delim/="'" .and. delim/='"') then
      call iotk_error_issue(ierrl,"iotk_scan_attr",__FILE__,__LINE__)
# 364 "iotk_attr.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.15 ")
      goto 1
    end if
    pos = scan(attr(equal+1:attlen),delim)
    if(pos<=0) then
      call iotk_error_issue(ierrl,"iotk_scan_attr",__FILE__,__LINE__)
# 369 "iotk_attr.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.15 ")
      goto 1
    end if
    if(foundl) exit
    equal = equal + pos
  end do
  if(foundl) then
    call iotk_strcpy(valc,attr(equal+1:equal+pos-1),ierrl)
    if(ierrl/=0) then
      call iotk_error_issue(ierrl,"iotk_scan_attr",__FILE__,__LINE__)
# 378 "iotk_attr.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.15 ")
      goto 1
    end if
  else
    goto 1
  end if
# 401 "iotk_attr.spp"
  allocate(tmpval(size(val)))
  index = 0
  call iotk_str_clean(valc)
  call iotk_read(tmpval,valc(1:iotk_strlen(valc)),index,ierrl)
  if(ierrl/=0) then
    call iotk_error_issue(ierrl,"iotk_scan_attr",__FILE__,__LINE__)
# 406 "iotk_attr.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.15 ")
    goto 1
  end if
# 412 "iotk_attr.spp"
  if(index/=size(val)) then
# 414 "iotk_attr.spp"
    call iotk_error_issue(ierrl,"iotk_scan_attr",__FILE__,__LINE__)
# 414 "iotk_attr.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.15 ")
# 414 "iotk_attr.spp"
call iotk_error_msg(ierrl,'Attribute size does not match')
# 414 "iotk_attr.spp"
call iotk_error_write(ierrl,"attr",valc)
# 414 "iotk_attr.spp"
call iotk_error_write(ierrl,"size",size(tmpval))
    goto 1
  end if
# 420 "iotk_attr.spp"
  val = reshape (source=tmpval,shape=shape(val))
# 422 "iotk_attr.spp"
  deallocate(tmpval)
# 424 "iotk_attr.spp"
1 continue
  if(ierrl/=0) foundl=.false.
  if(present(found)) found = foundl
  if(ierrl==0 .and. .not. present(found) .and. .not. present(default) .and. .not. foundl) then
    call iotk_error_issue(ierrl,"iotk_scan_attr",__FILE__,__LINE__)
# 428 "iotk_attr.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.15 ")
# 428 "iotk_attr.spp"
call iotk_error_msg(ierrl,'Attribute not found')
# 428 "iotk_attr.spp"
call iotk_error_write(ierrl,"name",name)
    ierrl = - ierrl
  end if
  if(present(default) .and. .not. foundl) then
# 441 "iotk_attr.spp"
    val = default
# 443 "iotk_attr.spp"
  end if
  if(present(ierr)) then
    ierr = ierrl
  else
    if(ierrl/=0) call iotk_error_handler(ierrl)
  end if
end subroutine iotk_scan_attr_REAL1_1
# 451 "iotk_attr.spp"

#endif
#endif

subroutine iotk_attr_dummy_REAL1_1
  write(0,*)
end subroutine iotk_attr_dummy_REAL1_1

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

#ifdef __IOTK_REAL1
#if 2 <= __IOTK_MAXRANK

# 158 "iotk_attr.spp"

# 233 "iotk_attr.spp"

# 236 "iotk_attr.spp"
subroutine iotk_write_attr_REAL1_2(attr,name,val,dummy,first,ierr)
  use iotk_base
  use iotk_error_interf
  use iotk_attr_interf
  use iotk_str_interf
  use iotk_misc_interf
  implicit none
  character(*), intent(inout) :: attr
  character(*), intent(in)    :: name
  REAL(kind=__IOTK_REAL1), intent(in)  :: val (:,:)
  type(iotk_dummytype), optional :: dummy
  logical, optional, intent(in)  :: first
  integer, optional, intent(out) :: ierr
  integer :: ierrl
  integer :: attlen
  integer :: vallen
  integer :: namlen
  character :: delim
# 257 "iotk_attr.spp"
  character(iotk_vallenx) :: tmpval
  ierrl = 0
  if(present(first)) then
    if(first) attr(1:1) = iotk_eos
  end if
  attlen = iotk_strlen_trim(attr)
  namlen = iotk_strlen_trim(name)
  if(.not.iotk_check_name(name)) then
    call iotk_error_issue(ierrl,"iotk_write_attr",__FILE__,__LINE__)
# 265 "iotk_attr.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.15 ")
# 265 "iotk_attr.spp"
call iotk_error_msg(ierrl,'Wrong tag name')
# 265 "iotk_attr.spp"
call iotk_error_write(ierrl,"name",name(1:namlen))
    goto 1
  end if
# 282 "iotk_attr.spp"
  delim = '"'
# 286 "iotk_attr.spp"
  call iotk_write(pack(val,mask=.true.),tmpval,ierrl)
# 288 "iotk_attr.spp"
  if(ierrl/=0) then
    call iotk_error_issue(ierrl,"iotk_write_attr",__FILE__,__LINE__)
# 289 "iotk_attr.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.15 ")
    goto 1
  end if
# 293 "iotk_attr.spp"
  vallen = iotk_strlen(tmpval)
  if(attlen+vallen+namlen+5>len(attr)) then
    call iotk_error_issue(ierrl,"iotk_write_attr",__FILE__,__LINE__)
# 295 "iotk_attr.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.15 ")
# 295 "iotk_attr.spp"
call iotk_error_msg(ierrl,'Attribute dummy argument is too short')
    goto 1
  end if
  attr(attlen+1:attlen+vallen+namlen+5) = " "//name(1:namlen)//"="//delim//tmpval(1:vallen)//delim//iotk_eos
1 continue
  if(present(ierr)) then
    ierr = ierrl
  else
    if(ierrl/=0) call iotk_error_handler(ierrl)
  end if
end subroutine iotk_write_attr_REAL1_2

# 308 "iotk_attr.spp"
subroutine iotk_scan_attr_REAL1_2(attr,name,val,dummy,found,default,eos,ierr)
  use iotk_base
  use iotk_error_interf
  use iotk_attr_interf, only : iotk_read
  use iotk_str_interf
  use iotk_misc_interf
  implicit none
  character(*),             intent(in)  :: attr
  character(*),             intent(in)  :: name
#ifdef __IOTK_WORKAROUND6
  REAL(kind=__IOTK_REAL1)                        :: val (:,:)
#else
  REAL(kind=__IOTK_REAL1), intent(out)           :: val (:,:)
#endif
  type(iotk_dummytype), optional :: dummy
  logical,        optional, intent(out) :: found
  REAL(kind=__IOTK_REAL1), optional, intent(in)  :: default (:,:)
  logical,        optional, intent(in)  :: eos
  integer,        optional, intent(out) :: ierr
  integer :: ierrl
  integer :: attlen,pos,equal,namlen
  character :: delim
  logical :: foundl
  character(iotk_vallenx) :: valc
# 337 "iotk_attr.spp"
  integer :: index
  REAL(kind=__IOTK_REAL1), allocatable :: tmpval (:)
# 340 "iotk_attr.spp"
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
# 351 "iotk_attr.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.15 ")
# 351 "iotk_attr.spp"
call iotk_error_msg(ierrl,'')
# 351 "iotk_attr.spp"
call iotk_error_write(ierrl,"attr",attr(equal+1:attlen))
      goto 1
    end if
    equal = equal + pos
    if(trim(attr(equal-pos:equal-1))==name(1:namlen)) foundl = .true.
    pos = verify(attr(equal+1:attlen)," ")
    if(pos<=0) then
      call iotk_error_issue(ierrl,"iotk_scan_attr",__FILE__,__LINE__)
# 358 "iotk_attr.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.15 ")
      goto 1
    end if
    equal = equal + pos
    delim = attr(equal:equal)
    if(delim/="'" .and. delim/='"') then
      call iotk_error_issue(ierrl,"iotk_scan_attr",__FILE__,__LINE__)
# 364 "iotk_attr.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.15 ")
      goto 1
    end if
    pos = scan(attr(equal+1:attlen),delim)
    if(pos<=0) then
      call iotk_error_issue(ierrl,"iotk_scan_attr",__FILE__,__LINE__)
# 369 "iotk_attr.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.15 ")
      goto 1
    end if
    if(foundl) exit
    equal = equal + pos
  end do
  if(foundl) then
    call iotk_strcpy(valc,attr(equal+1:equal+pos-1),ierrl)
    if(ierrl/=0) then
      call iotk_error_issue(ierrl,"iotk_scan_attr",__FILE__,__LINE__)
# 378 "iotk_attr.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.15 ")
      goto 1
    end if
  else
    goto 1
  end if
# 401 "iotk_attr.spp"
  allocate(tmpval(size(val)))
  index = 0
  call iotk_str_clean(valc)
  call iotk_read(tmpval,valc(1:iotk_strlen(valc)),index,ierrl)
  if(ierrl/=0) then
    call iotk_error_issue(ierrl,"iotk_scan_attr",__FILE__,__LINE__)
# 406 "iotk_attr.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.15 ")
    goto 1
  end if
# 412 "iotk_attr.spp"
  if(index/=size(val)) then
# 414 "iotk_attr.spp"
    call iotk_error_issue(ierrl,"iotk_scan_attr",__FILE__,__LINE__)
# 414 "iotk_attr.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.15 ")
# 414 "iotk_attr.spp"
call iotk_error_msg(ierrl,'Attribute size does not match')
# 414 "iotk_attr.spp"
call iotk_error_write(ierrl,"attr",valc)
# 414 "iotk_attr.spp"
call iotk_error_write(ierrl,"size",size(tmpval))
    goto 1
  end if
# 420 "iotk_attr.spp"
  val = reshape (source=tmpval,shape=shape(val))
# 422 "iotk_attr.spp"
  deallocate(tmpval)
# 424 "iotk_attr.spp"
1 continue
  if(ierrl/=0) foundl=.false.
  if(present(found)) found = foundl
  if(ierrl==0 .and. .not. present(found) .and. .not. present(default) .and. .not. foundl) then
    call iotk_error_issue(ierrl,"iotk_scan_attr",__FILE__,__LINE__)
# 428 "iotk_attr.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.15 ")
# 428 "iotk_attr.spp"
call iotk_error_msg(ierrl,'Attribute not found')
# 428 "iotk_attr.spp"
call iotk_error_write(ierrl,"name",name)
    ierrl = - ierrl
  end if
  if(present(default) .and. .not. foundl) then
# 441 "iotk_attr.spp"
    val = default
# 443 "iotk_attr.spp"
  end if
  if(present(ierr)) then
    ierr = ierrl
  else
    if(ierrl/=0) call iotk_error_handler(ierrl)
  end if
end subroutine iotk_scan_attr_REAL1_2
# 451 "iotk_attr.spp"

#endif
#endif

subroutine iotk_attr_dummy_REAL1_2
  write(0,*)
end subroutine iotk_attr_dummy_REAL1_2

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

#ifdef __IOTK_REAL1
#if 3 <= __IOTK_MAXRANK

# 158 "iotk_attr.spp"

# 233 "iotk_attr.spp"

# 236 "iotk_attr.spp"
subroutine iotk_write_attr_REAL1_3(attr,name,val,dummy,first,ierr)
  use iotk_base
  use iotk_error_interf
  use iotk_attr_interf
  use iotk_str_interf
  use iotk_misc_interf
  implicit none
  character(*), intent(inout) :: attr
  character(*), intent(in)    :: name
  REAL(kind=__IOTK_REAL1), intent(in)  :: val (:,:,:)
  type(iotk_dummytype), optional :: dummy
  logical, optional, intent(in)  :: first
  integer, optional, intent(out) :: ierr
  integer :: ierrl
  integer :: attlen
  integer :: vallen
  integer :: namlen
  character :: delim
# 257 "iotk_attr.spp"
  character(iotk_vallenx) :: tmpval
  ierrl = 0
  if(present(first)) then
    if(first) attr(1:1) = iotk_eos
  end if
  attlen = iotk_strlen_trim(attr)
  namlen = iotk_strlen_trim(name)
  if(.not.iotk_check_name(name)) then
    call iotk_error_issue(ierrl,"iotk_write_attr",__FILE__,__LINE__)
# 265 "iotk_attr.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.15 ")
# 265 "iotk_attr.spp"
call iotk_error_msg(ierrl,'Wrong tag name')
# 265 "iotk_attr.spp"
call iotk_error_write(ierrl,"name",name(1:namlen))
    goto 1
  end if
# 282 "iotk_attr.spp"
  delim = '"'
# 286 "iotk_attr.spp"
  call iotk_write(pack(val,mask=.true.),tmpval,ierrl)
# 288 "iotk_attr.spp"
  if(ierrl/=0) then
    call iotk_error_issue(ierrl,"iotk_write_attr",__FILE__,__LINE__)
# 289 "iotk_attr.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.15 ")
    goto 1
  end if
# 293 "iotk_attr.spp"
  vallen = iotk_strlen(tmpval)
  if(attlen+vallen+namlen+5>len(attr)) then
    call iotk_error_issue(ierrl,"iotk_write_attr",__FILE__,__LINE__)
# 295 "iotk_attr.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.15 ")
# 295 "iotk_attr.spp"
call iotk_error_msg(ierrl,'Attribute dummy argument is too short')
    goto 1
  end if
  attr(attlen+1:attlen+vallen+namlen+5) = " "//name(1:namlen)//"="//delim//tmpval(1:vallen)//delim//iotk_eos
1 continue
  if(present(ierr)) then
    ierr = ierrl
  else
    if(ierrl/=0) call iotk_error_handler(ierrl)
  end if
end subroutine iotk_write_attr_REAL1_3

# 308 "iotk_attr.spp"
subroutine iotk_scan_attr_REAL1_3(attr,name,val,dummy,found,default,eos,ierr)
  use iotk_base
  use iotk_error_interf
  use iotk_attr_interf, only : iotk_read
  use iotk_str_interf
  use iotk_misc_interf
  implicit none
  character(*),             intent(in)  :: attr
  character(*),             intent(in)  :: name
#ifdef __IOTK_WORKAROUND6
  REAL(kind=__IOTK_REAL1)                        :: val (:,:,:)
#else
  REAL(kind=__IOTK_REAL1), intent(out)           :: val (:,:,:)
#endif
  type(iotk_dummytype), optional :: dummy
  logical,        optional, intent(out) :: found
  REAL(kind=__IOTK_REAL1), optional, intent(in)  :: default (:,:,:)
  logical,        optional, intent(in)  :: eos
  integer,        optional, intent(out) :: ierr
  integer :: ierrl
  integer :: attlen,pos,equal,namlen
  character :: delim
  logical :: foundl
  character(iotk_vallenx) :: valc
# 337 "iotk_attr.spp"
  integer :: index
  REAL(kind=__IOTK_REAL1), allocatable :: tmpval (:)
# 340 "iotk_attr.spp"
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
# 351 "iotk_attr.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.15 ")
# 351 "iotk_attr.spp"
call iotk_error_msg(ierrl,'')
# 351 "iotk_attr.spp"
call iotk_error_write(ierrl,"attr",attr(equal+1:attlen))
      goto 1
    end if
    equal = equal + pos
    if(trim(attr(equal-pos:equal-1))==name(1:namlen)) foundl = .true.
    pos = verify(attr(equal+1:attlen)," ")
    if(pos<=0) then
      call iotk_error_issue(ierrl,"iotk_scan_attr",__FILE__,__LINE__)
# 358 "iotk_attr.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.15 ")
      goto 1
    end if
    equal = equal + pos
    delim = attr(equal:equal)
    if(delim/="'" .and. delim/='"') then
      call iotk_error_issue(ierrl,"iotk_scan_attr",__FILE__,__LINE__)
# 364 "iotk_attr.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.15 ")
      goto 1
    end if
    pos = scan(attr(equal+1:attlen),delim)
    if(pos<=0) then
      call iotk_error_issue(ierrl,"iotk_scan_attr",__FILE__,__LINE__)
# 369 "iotk_attr.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.15 ")
      goto 1
    end if
    if(foundl) exit
    equal = equal + pos
  end do
  if(foundl) then
    call iotk_strcpy(valc,attr(equal+1:equal+pos-1),ierrl)
    if(ierrl/=0) then
      call iotk_error_issue(ierrl,"iotk_scan_attr",__FILE__,__LINE__)
# 378 "iotk_attr.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.15 ")
      goto 1
    end if
  else
    goto 1
  end if
# 401 "iotk_attr.spp"
  allocate(tmpval(size(val)))
  index = 0
  call iotk_str_clean(valc)
  call iotk_read(tmpval,valc(1:iotk_strlen(valc)),index,ierrl)
  if(ierrl/=0) then
    call iotk_error_issue(ierrl,"iotk_scan_attr",__FILE__,__LINE__)
# 406 "iotk_attr.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.15 ")
    goto 1
  end if
# 412 "iotk_attr.spp"
  if(index/=size(val)) then
# 414 "iotk_attr.spp"
    call iotk_error_issue(ierrl,"iotk_scan_attr",__FILE__,__LINE__)
# 414 "iotk_attr.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.15 ")
# 414 "iotk_attr.spp"
call iotk_error_msg(ierrl,'Attribute size does not match')
# 414 "iotk_attr.spp"
call iotk_error_write(ierrl,"attr",valc)
# 414 "iotk_attr.spp"
call iotk_error_write(ierrl,"size",size(tmpval))
    goto 1
  end if
# 420 "iotk_attr.spp"
  val = reshape (source=tmpval,shape=shape(val))
# 422 "iotk_attr.spp"
  deallocate(tmpval)
# 424 "iotk_attr.spp"
1 continue
  if(ierrl/=0) foundl=.false.
  if(present(found)) found = foundl
  if(ierrl==0 .and. .not. present(found) .and. .not. present(default) .and. .not. foundl) then
    call iotk_error_issue(ierrl,"iotk_scan_attr",__FILE__,__LINE__)
# 428 "iotk_attr.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.15 ")
# 428 "iotk_attr.spp"
call iotk_error_msg(ierrl,'Attribute not found')
# 428 "iotk_attr.spp"
call iotk_error_write(ierrl,"name",name)
    ierrl = - ierrl
  end if
  if(present(default) .and. .not. foundl) then
# 441 "iotk_attr.spp"
    val = default
# 443 "iotk_attr.spp"
  end if
  if(present(ierr)) then
    ierr = ierrl
  else
    if(ierrl/=0) call iotk_error_handler(ierrl)
  end if
end subroutine iotk_scan_attr_REAL1_3
# 451 "iotk_attr.spp"

#endif
#endif

subroutine iotk_attr_dummy_REAL1_3
  write(0,*)
end subroutine iotk_attr_dummy_REAL1_3

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

#ifdef __IOTK_REAL1
#if 4 <= __IOTK_MAXRANK

# 158 "iotk_attr.spp"

# 233 "iotk_attr.spp"

# 236 "iotk_attr.spp"
subroutine iotk_write_attr_REAL1_4(attr,name,val,dummy,first,ierr)
  use iotk_base
  use iotk_error_interf
  use iotk_attr_interf
  use iotk_str_interf
  use iotk_misc_interf
  implicit none
  character(*), intent(inout) :: attr
  character(*), intent(in)    :: name
  REAL(kind=__IOTK_REAL1), intent(in)  :: val (:,:,:,:)
  type(iotk_dummytype), optional :: dummy
  logical, optional, intent(in)  :: first
  integer, optional, intent(out) :: ierr
  integer :: ierrl
  integer :: attlen
  integer :: vallen
  integer :: namlen
  character :: delim
# 257 "iotk_attr.spp"
  character(iotk_vallenx) :: tmpval
  ierrl = 0
  if(present(first)) then
    if(first) attr(1:1) = iotk_eos
  end if
  attlen = iotk_strlen_trim(attr)
  namlen = iotk_strlen_trim(name)
  if(.not.iotk_check_name(name)) then
    call iotk_error_issue(ierrl,"iotk_write_attr",__FILE__,__LINE__)
# 265 "iotk_attr.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.15 ")
# 265 "iotk_attr.spp"
call iotk_error_msg(ierrl,'Wrong tag name')
# 265 "iotk_attr.spp"
call iotk_error_write(ierrl,"name",name(1:namlen))
    goto 1
  end if
# 282 "iotk_attr.spp"
  delim = '"'
# 286 "iotk_attr.spp"
  call iotk_write(pack(val,mask=.true.),tmpval,ierrl)
# 288 "iotk_attr.spp"
  if(ierrl/=0) then
    call iotk_error_issue(ierrl,"iotk_write_attr",__FILE__,__LINE__)
# 289 "iotk_attr.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.15 ")
    goto 1
  end if
# 293 "iotk_attr.spp"
  vallen = iotk_strlen(tmpval)
  if(attlen+vallen+namlen+5>len(attr)) then
    call iotk_error_issue(ierrl,"iotk_write_attr",__FILE__,__LINE__)
# 295 "iotk_attr.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.15 ")
# 295 "iotk_attr.spp"
call iotk_error_msg(ierrl,'Attribute dummy argument is too short')
    goto 1
  end if
  attr(attlen+1:attlen+vallen+namlen+5) = " "//name(1:namlen)//"="//delim//tmpval(1:vallen)//delim//iotk_eos
1 continue
  if(present(ierr)) then
    ierr = ierrl
  else
    if(ierrl/=0) call iotk_error_handler(ierrl)
  end if
end subroutine iotk_write_attr_REAL1_4

# 308 "iotk_attr.spp"
subroutine iotk_scan_attr_REAL1_4(attr,name,val,dummy,found,default,eos,ierr)
  use iotk_base
  use iotk_error_interf
  use iotk_attr_interf, only : iotk_read
  use iotk_str_interf
  use iotk_misc_interf
  implicit none
  character(*),             intent(in)  :: attr
  character(*),             intent(in)  :: name
#ifdef __IOTK_WORKAROUND6
  REAL(kind=__IOTK_REAL1)                        :: val (:,:,:,:)
#else
  REAL(kind=__IOTK_REAL1), intent(out)           :: val (:,:,:,:)
#endif
  type(iotk_dummytype), optional :: dummy
  logical,        optional, intent(out) :: found
  REAL(kind=__IOTK_REAL1), optional, intent(in)  :: default (:,:,:,:)
  logical,        optional, intent(in)  :: eos
  integer,        optional, intent(out) :: ierr
  integer :: ierrl
  integer :: attlen,pos,equal,namlen
  character :: delim
  logical :: foundl
  character(iotk_vallenx) :: valc
# 337 "iotk_attr.spp"
  integer :: index
  REAL(kind=__IOTK_REAL1), allocatable :: tmpval (:)
# 340 "iotk_attr.spp"
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
# 351 "iotk_attr.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.15 ")
# 351 "iotk_attr.spp"
call iotk_error_msg(ierrl,'')
# 351 "iotk_attr.spp"
call iotk_error_write(ierrl,"attr",attr(equal+1:attlen))
      goto 1
    end if
    equal = equal + pos
    if(trim(attr(equal-pos:equal-1))==name(1:namlen)) foundl = .true.
    pos = verify(attr(equal+1:attlen)," ")
    if(pos<=0) then
      call iotk_error_issue(ierrl,"iotk_scan_attr",__FILE__,__LINE__)
# 358 "iotk_attr.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.15 ")
      goto 1
    end if
    equal = equal + pos
    delim = attr(equal:equal)
    if(delim/="'" .and. delim/='"') then
      call iotk_error_issue(ierrl,"iotk_scan_attr",__FILE__,__LINE__)
# 364 "iotk_attr.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.15 ")
      goto 1
    end if
    pos = scan(attr(equal+1:attlen),delim)
    if(pos<=0) then
      call iotk_error_issue(ierrl,"iotk_scan_attr",__FILE__,__LINE__)
# 369 "iotk_attr.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.15 ")
      goto 1
    end if
    if(foundl) exit
    equal = equal + pos
  end do
  if(foundl) then
    call iotk_strcpy(valc,attr(equal+1:equal+pos-1),ierrl)
    if(ierrl/=0) then
      call iotk_error_issue(ierrl,"iotk_scan_attr",__FILE__,__LINE__)
# 378 "iotk_attr.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.15 ")
      goto 1
    end if
  else
    goto 1
  end if
# 401 "iotk_attr.spp"
  allocate(tmpval(size(val)))
  index = 0
  call iotk_str_clean(valc)
  call iotk_read(tmpval,valc(1:iotk_strlen(valc)),index,ierrl)
  if(ierrl/=0) then
    call iotk_error_issue(ierrl,"iotk_scan_attr",__FILE__,__LINE__)
# 406 "iotk_attr.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.15 ")
    goto 1
  end if
# 412 "iotk_attr.spp"
  if(index/=size(val)) then
# 414 "iotk_attr.spp"
    call iotk_error_issue(ierrl,"iotk_scan_attr",__FILE__,__LINE__)
# 414 "iotk_attr.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.15 ")
# 414 "iotk_attr.spp"
call iotk_error_msg(ierrl,'Attribute size does not match')
# 414 "iotk_attr.spp"
call iotk_error_write(ierrl,"attr",valc)
# 414 "iotk_attr.spp"
call iotk_error_write(ierrl,"size",size(tmpval))
    goto 1
  end if
# 420 "iotk_attr.spp"
  val = reshape (source=tmpval,shape=shape(val))
# 422 "iotk_attr.spp"
  deallocate(tmpval)
# 424 "iotk_attr.spp"
1 continue
  if(ierrl/=0) foundl=.false.
  if(present(found)) found = foundl
  if(ierrl==0 .and. .not. present(found) .and. .not. present(default) .and. .not. foundl) then
    call iotk_error_issue(ierrl,"iotk_scan_attr",__FILE__,__LINE__)
# 428 "iotk_attr.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.15 ")
# 428 "iotk_attr.spp"
call iotk_error_msg(ierrl,'Attribute not found')
# 428 "iotk_attr.spp"
call iotk_error_write(ierrl,"name",name)
    ierrl = - ierrl
  end if
  if(present(default) .and. .not. foundl) then
# 441 "iotk_attr.spp"
    val = default
# 443 "iotk_attr.spp"
  end if
  if(present(ierr)) then
    ierr = ierrl
  else
    if(ierrl/=0) call iotk_error_handler(ierrl)
  end if
end subroutine iotk_scan_attr_REAL1_4
# 451 "iotk_attr.spp"

#endif
#endif

subroutine iotk_attr_dummy_REAL1_4
  write(0,*)
end subroutine iotk_attr_dummy_REAL1_4

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

#ifdef __IOTK_REAL1
#if 5 <= __IOTK_MAXRANK

# 158 "iotk_attr.spp"

# 233 "iotk_attr.spp"

# 236 "iotk_attr.spp"
subroutine iotk_write_attr_REAL1_5(attr,name,val,dummy,first,ierr)
  use iotk_base
  use iotk_error_interf
  use iotk_attr_interf
  use iotk_str_interf
  use iotk_misc_interf
  implicit none
  character(*), intent(inout) :: attr
  character(*), intent(in)    :: name
  REAL(kind=__IOTK_REAL1), intent(in)  :: val (:,:,:,:,:)
  type(iotk_dummytype), optional :: dummy
  logical, optional, intent(in)  :: first
  integer, optional, intent(out) :: ierr
  integer :: ierrl
  integer :: attlen
  integer :: vallen
  integer :: namlen
  character :: delim
# 257 "iotk_attr.spp"
  character(iotk_vallenx) :: tmpval
  ierrl = 0
  if(present(first)) then
    if(first) attr(1:1) = iotk_eos
  end if
  attlen = iotk_strlen_trim(attr)
  namlen = iotk_strlen_trim(name)
  if(.not.iotk_check_name(name)) then
    call iotk_error_issue(ierrl,"iotk_write_attr",__FILE__,__LINE__)
# 265 "iotk_attr.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.15 ")
# 265 "iotk_attr.spp"
call iotk_error_msg(ierrl,'Wrong tag name')
# 265 "iotk_attr.spp"
call iotk_error_write(ierrl,"name",name(1:namlen))
    goto 1
  end if
# 282 "iotk_attr.spp"
  delim = '"'
# 286 "iotk_attr.spp"
  call iotk_write(pack(val,mask=.true.),tmpval,ierrl)
# 288 "iotk_attr.spp"
  if(ierrl/=0) then
    call iotk_error_issue(ierrl,"iotk_write_attr",__FILE__,__LINE__)
# 289 "iotk_attr.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.15 ")
    goto 1
  end if
# 293 "iotk_attr.spp"
  vallen = iotk_strlen(tmpval)
  if(attlen+vallen+namlen+5>len(attr)) then
    call iotk_error_issue(ierrl,"iotk_write_attr",__FILE__,__LINE__)
# 295 "iotk_attr.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.15 ")
# 295 "iotk_attr.spp"
call iotk_error_msg(ierrl,'Attribute dummy argument is too short')
    goto 1
  end if
  attr(attlen+1:attlen+vallen+namlen+5) = " "//name(1:namlen)//"="//delim//tmpval(1:vallen)//delim//iotk_eos
1 continue
  if(present(ierr)) then
    ierr = ierrl
  else
    if(ierrl/=0) call iotk_error_handler(ierrl)
  end if
end subroutine iotk_write_attr_REAL1_5

# 308 "iotk_attr.spp"
subroutine iotk_scan_attr_REAL1_5(attr,name,val,dummy,found,default,eos,ierr)
  use iotk_base
  use iotk_error_interf
  use iotk_attr_interf, only : iotk_read
  use iotk_str_interf
  use iotk_misc_interf
  implicit none
  character(*),             intent(in)  :: attr
  character(*),             intent(in)  :: name
#ifdef __IOTK_WORKAROUND6
  REAL(kind=__IOTK_REAL1)                        :: val (:,:,:,:,:)
#else
  REAL(kind=__IOTK_REAL1), intent(out)           :: val (:,:,:,:,:)
#endif
  type(iotk_dummytype), optional :: dummy
  logical,        optional, intent(out) :: found
  REAL(kind=__IOTK_REAL1), optional, intent(in)  :: default (:,:,:,:,:)
  logical,        optional, intent(in)  :: eos
  integer,        optional, intent(out) :: ierr
  integer :: ierrl
  integer :: attlen,pos,equal,namlen
  character :: delim
  logical :: foundl
  character(iotk_vallenx) :: valc
# 337 "iotk_attr.spp"
  integer :: index
  REAL(kind=__IOTK_REAL1), allocatable :: tmpval (:)
# 340 "iotk_attr.spp"
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
# 351 "iotk_attr.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.15 ")
# 351 "iotk_attr.spp"
call iotk_error_msg(ierrl,'')
# 351 "iotk_attr.spp"
call iotk_error_write(ierrl,"attr",attr(equal+1:attlen))
      goto 1
    end if
    equal = equal + pos
    if(trim(attr(equal-pos:equal-1))==name(1:namlen)) foundl = .true.
    pos = verify(attr(equal+1:attlen)," ")
    if(pos<=0) then
      call iotk_error_issue(ierrl,"iotk_scan_attr",__FILE__,__LINE__)
# 358 "iotk_attr.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.15 ")
      goto 1
    end if
    equal = equal + pos
    delim = attr(equal:equal)
    if(delim/="'" .and. delim/='"') then
      call iotk_error_issue(ierrl,"iotk_scan_attr",__FILE__,__LINE__)
# 364 "iotk_attr.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.15 ")
      goto 1
    end if
    pos = scan(attr(equal+1:attlen),delim)
    if(pos<=0) then
      call iotk_error_issue(ierrl,"iotk_scan_attr",__FILE__,__LINE__)
# 369 "iotk_attr.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.15 ")
      goto 1
    end if
    if(foundl) exit
    equal = equal + pos
  end do
  if(foundl) then
    call iotk_strcpy(valc,attr(equal+1:equal+pos-1),ierrl)
    if(ierrl/=0) then
      call iotk_error_issue(ierrl,"iotk_scan_attr",__FILE__,__LINE__)
# 378 "iotk_attr.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.15 ")
      goto 1
    end if
  else
    goto 1
  end if
# 401 "iotk_attr.spp"
  allocate(tmpval(size(val)))
  index = 0
  call iotk_str_clean(valc)
  call iotk_read(tmpval,valc(1:iotk_strlen(valc)),index,ierrl)
  if(ierrl/=0) then
    call iotk_error_issue(ierrl,"iotk_scan_attr",__FILE__,__LINE__)
# 406 "iotk_attr.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.15 ")
    goto 1
  end if
# 412 "iotk_attr.spp"
  if(index/=size(val)) then
# 414 "iotk_attr.spp"
    call iotk_error_issue(ierrl,"iotk_scan_attr",__FILE__,__LINE__)
# 414 "iotk_attr.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.15 ")
# 414 "iotk_attr.spp"
call iotk_error_msg(ierrl,'Attribute size does not match')
# 414 "iotk_attr.spp"
call iotk_error_write(ierrl,"attr",valc)
# 414 "iotk_attr.spp"
call iotk_error_write(ierrl,"size",size(tmpval))
    goto 1
  end if
# 420 "iotk_attr.spp"
  val = reshape (source=tmpval,shape=shape(val))
# 422 "iotk_attr.spp"
  deallocate(tmpval)
# 424 "iotk_attr.spp"
1 continue
  if(ierrl/=0) foundl=.false.
  if(present(found)) found = foundl
  if(ierrl==0 .and. .not. present(found) .and. .not. present(default) .and. .not. foundl) then
    call iotk_error_issue(ierrl,"iotk_scan_attr",__FILE__,__LINE__)
# 428 "iotk_attr.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.15 ")
# 428 "iotk_attr.spp"
call iotk_error_msg(ierrl,'Attribute not found')
# 428 "iotk_attr.spp"
call iotk_error_write(ierrl,"name",name)
    ierrl = - ierrl
  end if
  if(present(default) .and. .not. foundl) then
# 441 "iotk_attr.spp"
    val = default
# 443 "iotk_attr.spp"
  end if
  if(present(ierr)) then
    ierr = ierrl
  else
    if(ierrl/=0) call iotk_error_handler(ierrl)
  end if
end subroutine iotk_scan_attr_REAL1_5
# 451 "iotk_attr.spp"

#endif
#endif

subroutine iotk_attr_dummy_REAL1_5
  write(0,*)
end subroutine iotk_attr_dummy_REAL1_5

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

#ifdef __IOTK_REAL1
#if 6 <= __IOTK_MAXRANK

# 158 "iotk_attr.spp"

# 233 "iotk_attr.spp"

# 236 "iotk_attr.spp"
subroutine iotk_write_attr_REAL1_6(attr,name,val,dummy,first,ierr)
  use iotk_base
  use iotk_error_interf
  use iotk_attr_interf
  use iotk_str_interf
  use iotk_misc_interf
  implicit none
  character(*), intent(inout) :: attr
  character(*), intent(in)    :: name
  REAL(kind=__IOTK_REAL1), intent(in)  :: val (:,:,:,:,:,:)
  type(iotk_dummytype), optional :: dummy
  logical, optional, intent(in)  :: first
  integer, optional, intent(out) :: ierr
  integer :: ierrl
  integer :: attlen
  integer :: vallen
  integer :: namlen
  character :: delim
# 257 "iotk_attr.spp"
  character(iotk_vallenx) :: tmpval
  ierrl = 0
  if(present(first)) then
    if(first) attr(1:1) = iotk_eos
  end if
  attlen = iotk_strlen_trim(attr)
  namlen = iotk_strlen_trim(name)
  if(.not.iotk_check_name(name)) then
    call iotk_error_issue(ierrl,"iotk_write_attr",__FILE__,__LINE__)
# 265 "iotk_attr.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.15 ")
# 265 "iotk_attr.spp"
call iotk_error_msg(ierrl,'Wrong tag name')
# 265 "iotk_attr.spp"
call iotk_error_write(ierrl,"name",name(1:namlen))
    goto 1
  end if
# 282 "iotk_attr.spp"
  delim = '"'
# 286 "iotk_attr.spp"
  call iotk_write(pack(val,mask=.true.),tmpval,ierrl)
# 288 "iotk_attr.spp"
  if(ierrl/=0) then
    call iotk_error_issue(ierrl,"iotk_write_attr",__FILE__,__LINE__)
# 289 "iotk_attr.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.15 ")
    goto 1
  end if
# 293 "iotk_attr.spp"
  vallen = iotk_strlen(tmpval)
  if(attlen+vallen+namlen+5>len(attr)) then
    call iotk_error_issue(ierrl,"iotk_write_attr",__FILE__,__LINE__)
# 295 "iotk_attr.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.15 ")
# 295 "iotk_attr.spp"
call iotk_error_msg(ierrl,'Attribute dummy argument is too short')
    goto 1
  end if
  attr(attlen+1:attlen+vallen+namlen+5) = " "//name(1:namlen)//"="//delim//tmpval(1:vallen)//delim//iotk_eos
1 continue
  if(present(ierr)) then
    ierr = ierrl
  else
    if(ierrl/=0) call iotk_error_handler(ierrl)
  end if
end subroutine iotk_write_attr_REAL1_6

# 308 "iotk_attr.spp"
subroutine iotk_scan_attr_REAL1_6(attr,name,val,dummy,found,default,eos,ierr)
  use iotk_base
  use iotk_error_interf
  use iotk_attr_interf, only : iotk_read
  use iotk_str_interf
  use iotk_misc_interf
  implicit none
  character(*),             intent(in)  :: attr
  character(*),             intent(in)  :: name
#ifdef __IOTK_WORKAROUND6
  REAL(kind=__IOTK_REAL1)                        :: val (:,:,:,:,:,:)
#else
  REAL(kind=__IOTK_REAL1), intent(out)           :: val (:,:,:,:,:,:)
#endif
  type(iotk_dummytype), optional :: dummy
  logical,        optional, intent(out) :: found
  REAL(kind=__IOTK_REAL1), optional, intent(in)  :: default (:,:,:,:,:,:)
  logical,        optional, intent(in)  :: eos
  integer,        optional, intent(out) :: ierr
  integer :: ierrl
  integer :: attlen,pos,equal,namlen
  character :: delim
  logical :: foundl
  character(iotk_vallenx) :: valc
# 337 "iotk_attr.spp"
  integer :: index
  REAL(kind=__IOTK_REAL1), allocatable :: tmpval (:)
# 340 "iotk_attr.spp"
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
# 351 "iotk_attr.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.15 ")
# 351 "iotk_attr.spp"
call iotk_error_msg(ierrl,'')
# 351 "iotk_attr.spp"
call iotk_error_write(ierrl,"attr",attr(equal+1:attlen))
      goto 1
    end if
    equal = equal + pos
    if(trim(attr(equal-pos:equal-1))==name(1:namlen)) foundl = .true.
    pos = verify(attr(equal+1:attlen)," ")
    if(pos<=0) then
      call iotk_error_issue(ierrl,"iotk_scan_attr",__FILE__,__LINE__)
# 358 "iotk_attr.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.15 ")
      goto 1
    end if
    equal = equal + pos
    delim = attr(equal:equal)
    if(delim/="'" .and. delim/='"') then
      call iotk_error_issue(ierrl,"iotk_scan_attr",__FILE__,__LINE__)
# 364 "iotk_attr.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.15 ")
      goto 1
    end if
    pos = scan(attr(equal+1:attlen),delim)
    if(pos<=0) then
      call iotk_error_issue(ierrl,"iotk_scan_attr",__FILE__,__LINE__)
# 369 "iotk_attr.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.15 ")
      goto 1
    end if
    if(foundl) exit
    equal = equal + pos
  end do
  if(foundl) then
    call iotk_strcpy(valc,attr(equal+1:equal+pos-1),ierrl)
    if(ierrl/=0) then
      call iotk_error_issue(ierrl,"iotk_scan_attr",__FILE__,__LINE__)
# 378 "iotk_attr.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.15 ")
      goto 1
    end if
  else
    goto 1
  end if
# 401 "iotk_attr.spp"
  allocate(tmpval(size(val)))
  index = 0
  call iotk_str_clean(valc)
  call iotk_read(tmpval,valc(1:iotk_strlen(valc)),index,ierrl)
  if(ierrl/=0) then
    call iotk_error_issue(ierrl,"iotk_scan_attr",__FILE__,__LINE__)
# 406 "iotk_attr.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.15 ")
    goto 1
  end if
# 412 "iotk_attr.spp"
  if(index/=size(val)) then
# 414 "iotk_attr.spp"
    call iotk_error_issue(ierrl,"iotk_scan_attr",__FILE__,__LINE__)
# 414 "iotk_attr.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.15 ")
# 414 "iotk_attr.spp"
call iotk_error_msg(ierrl,'Attribute size does not match')
# 414 "iotk_attr.spp"
call iotk_error_write(ierrl,"attr",valc)
# 414 "iotk_attr.spp"
call iotk_error_write(ierrl,"size",size(tmpval))
    goto 1
  end if
# 420 "iotk_attr.spp"
  val = reshape (source=tmpval,shape=shape(val))
# 422 "iotk_attr.spp"
  deallocate(tmpval)
# 424 "iotk_attr.spp"
1 continue
  if(ierrl/=0) foundl=.false.
  if(present(found)) found = foundl
  if(ierrl==0 .and. .not. present(found) .and. .not. present(default) .and. .not. foundl) then
    call iotk_error_issue(ierrl,"iotk_scan_attr",__FILE__,__LINE__)
# 428 "iotk_attr.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.15 ")
# 428 "iotk_attr.spp"
call iotk_error_msg(ierrl,'Attribute not found')
# 428 "iotk_attr.spp"
call iotk_error_write(ierrl,"name",name)
    ierrl = - ierrl
  end if
  if(present(default) .and. .not. foundl) then
# 441 "iotk_attr.spp"
    val = default
# 443 "iotk_attr.spp"
  end if
  if(present(ierr)) then
    ierr = ierrl
  else
    if(ierrl/=0) call iotk_error_handler(ierrl)
  end if
end subroutine iotk_scan_attr_REAL1_6
# 451 "iotk_attr.spp"

#endif
#endif

subroutine iotk_attr_dummy_REAL1_6
  write(0,*)
end subroutine iotk_attr_dummy_REAL1_6

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

#ifdef __IOTK_REAL1
#if 7 <= __IOTK_MAXRANK

# 158 "iotk_attr.spp"

# 233 "iotk_attr.spp"

# 236 "iotk_attr.spp"
subroutine iotk_write_attr_REAL1_7(attr,name,val,dummy,first,ierr)
  use iotk_base
  use iotk_error_interf
  use iotk_attr_interf
  use iotk_str_interf
  use iotk_misc_interf
  implicit none
  character(*), intent(inout) :: attr
  character(*), intent(in)    :: name
  REAL(kind=__IOTK_REAL1), intent(in)  :: val (:,:,:,:,:,:,:)
  type(iotk_dummytype), optional :: dummy
  logical, optional, intent(in)  :: first
  integer, optional, intent(out) :: ierr
  integer :: ierrl
  integer :: attlen
  integer :: vallen
  integer :: namlen
  character :: delim
# 257 "iotk_attr.spp"
  character(iotk_vallenx) :: tmpval
  ierrl = 0
  if(present(first)) then
    if(first) attr(1:1) = iotk_eos
  end if
  attlen = iotk_strlen_trim(attr)
  namlen = iotk_strlen_trim(name)
  if(.not.iotk_check_name(name)) then
    call iotk_error_issue(ierrl,"iotk_write_attr",__FILE__,__LINE__)
# 265 "iotk_attr.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.15 ")
# 265 "iotk_attr.spp"
call iotk_error_msg(ierrl,'Wrong tag name')
# 265 "iotk_attr.spp"
call iotk_error_write(ierrl,"name",name(1:namlen))
    goto 1
  end if
# 282 "iotk_attr.spp"
  delim = '"'
# 286 "iotk_attr.spp"
  call iotk_write(pack(val,mask=.true.),tmpval,ierrl)
# 288 "iotk_attr.spp"
  if(ierrl/=0) then
    call iotk_error_issue(ierrl,"iotk_write_attr",__FILE__,__LINE__)
# 289 "iotk_attr.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.15 ")
    goto 1
  end if
# 293 "iotk_attr.spp"
  vallen = iotk_strlen(tmpval)
  if(attlen+vallen+namlen+5>len(attr)) then
    call iotk_error_issue(ierrl,"iotk_write_attr",__FILE__,__LINE__)
# 295 "iotk_attr.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.15 ")
# 295 "iotk_attr.spp"
call iotk_error_msg(ierrl,'Attribute dummy argument is too short')
    goto 1
  end if
  attr(attlen+1:attlen+vallen+namlen+5) = " "//name(1:namlen)//"="//delim//tmpval(1:vallen)//delim//iotk_eos
1 continue
  if(present(ierr)) then
    ierr = ierrl
  else
    if(ierrl/=0) call iotk_error_handler(ierrl)
  end if
end subroutine iotk_write_attr_REAL1_7

# 308 "iotk_attr.spp"
subroutine iotk_scan_attr_REAL1_7(attr,name,val,dummy,found,default,eos,ierr)
  use iotk_base
  use iotk_error_interf
  use iotk_attr_interf, only : iotk_read
  use iotk_str_interf
  use iotk_misc_interf
  implicit none
  character(*),             intent(in)  :: attr
  character(*),             intent(in)  :: name
#ifdef __IOTK_WORKAROUND6
  REAL(kind=__IOTK_REAL1)                        :: val (:,:,:,:,:,:,:)
#else
  REAL(kind=__IOTK_REAL1), intent(out)           :: val (:,:,:,:,:,:,:)
#endif
  type(iotk_dummytype), optional :: dummy
  logical,        optional, intent(out) :: found
  REAL(kind=__IOTK_REAL1), optional, intent(in)  :: default (:,:,:,:,:,:,:)
  logical,        optional, intent(in)  :: eos
  integer,        optional, intent(out) :: ierr
  integer :: ierrl
  integer :: attlen,pos,equal,namlen
  character :: delim
  logical :: foundl
  character(iotk_vallenx) :: valc
# 337 "iotk_attr.spp"
  integer :: index
  REAL(kind=__IOTK_REAL1), allocatable :: tmpval (:)
# 340 "iotk_attr.spp"
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
# 351 "iotk_attr.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.15 ")
# 351 "iotk_attr.spp"
call iotk_error_msg(ierrl,'')
# 351 "iotk_attr.spp"
call iotk_error_write(ierrl,"attr",attr(equal+1:attlen))
      goto 1
    end if
    equal = equal + pos
    if(trim(attr(equal-pos:equal-1))==name(1:namlen)) foundl = .true.
    pos = verify(attr(equal+1:attlen)," ")
    if(pos<=0) then
      call iotk_error_issue(ierrl,"iotk_scan_attr",__FILE__,__LINE__)
# 358 "iotk_attr.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.15 ")
      goto 1
    end if
    equal = equal + pos
    delim = attr(equal:equal)
    if(delim/="'" .and. delim/='"') then
      call iotk_error_issue(ierrl,"iotk_scan_attr",__FILE__,__LINE__)
# 364 "iotk_attr.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.15 ")
      goto 1
    end if
    pos = scan(attr(equal+1:attlen),delim)
    if(pos<=0) then
      call iotk_error_issue(ierrl,"iotk_scan_attr",__FILE__,__LINE__)
# 369 "iotk_attr.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.15 ")
      goto 1
    end if
    if(foundl) exit
    equal = equal + pos
  end do
  if(foundl) then
    call iotk_strcpy(valc,attr(equal+1:equal+pos-1),ierrl)
    if(ierrl/=0) then
      call iotk_error_issue(ierrl,"iotk_scan_attr",__FILE__,__LINE__)
# 378 "iotk_attr.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.15 ")
      goto 1
    end if
  else
    goto 1
  end if
# 401 "iotk_attr.spp"
  allocate(tmpval(size(val)))
  index = 0
  call iotk_str_clean(valc)
  call iotk_read(tmpval,valc(1:iotk_strlen(valc)),index,ierrl)
  if(ierrl/=0) then
    call iotk_error_issue(ierrl,"iotk_scan_attr",__FILE__,__LINE__)
# 406 "iotk_attr.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.15 ")
    goto 1
  end if
# 412 "iotk_attr.spp"
  if(index/=size(val)) then
# 414 "iotk_attr.spp"
    call iotk_error_issue(ierrl,"iotk_scan_attr",__FILE__,__LINE__)
# 414 "iotk_attr.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.15 ")
# 414 "iotk_attr.spp"
call iotk_error_msg(ierrl,'Attribute size does not match')
# 414 "iotk_attr.spp"
call iotk_error_write(ierrl,"attr",valc)
# 414 "iotk_attr.spp"
call iotk_error_write(ierrl,"size",size(tmpval))
    goto 1
  end if
# 420 "iotk_attr.spp"
  val = reshape (source=tmpval,shape=shape(val))
# 422 "iotk_attr.spp"
  deallocate(tmpval)
# 424 "iotk_attr.spp"
1 continue
  if(ierrl/=0) foundl=.false.
  if(present(found)) found = foundl
  if(ierrl==0 .and. .not. present(found) .and. .not. present(default) .and. .not. foundl) then
    call iotk_error_issue(ierrl,"iotk_scan_attr",__FILE__,__LINE__)
# 428 "iotk_attr.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.15 ")
# 428 "iotk_attr.spp"
call iotk_error_msg(ierrl,'Attribute not found')
# 428 "iotk_attr.spp"
call iotk_error_write(ierrl,"name",name)
    ierrl = - ierrl
  end if
  if(present(default) .and. .not. foundl) then
# 441 "iotk_attr.spp"
    val = default
# 443 "iotk_attr.spp"
  end if
  if(present(ierr)) then
    ierr = ierrl
  else
    if(ierrl/=0) call iotk_error_handler(ierrl)
  end if
end subroutine iotk_scan_attr_REAL1_7
# 451 "iotk_attr.spp"

#endif
#endif

subroutine iotk_attr_dummy_REAL1_7
  write(0,*)
end subroutine iotk_attr_dummy_REAL1_7

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

#ifdef __IOTK_REAL2
#if 0 <= __IOTK_MAXRANK

# 83 "iotk_attr.spp"
! This is needed as a workaround for bugged pack 
subroutine iotk_private_pack_REAL2(out,in,n,l)
    use iotk_base
    implicit none
    integer,                                    intent(in)  :: n,l
# 92 "iotk_attr.spp"
    REAL (kind=__IOTK_REAL2), intent(out) :: out(n)
    REAL (kind=__IOTK_REAL2), intent(in)  :: in(n)
# 95 "iotk_attr.spp"
    out = in
end subroutine iotk_private_pack_REAL2

# 100 "iotk_attr.spp"
subroutine iotk_write_REAL2(val,string,ierr)
  use iotk_base
  use iotk_error_interf
  use iotk_xtox_interf
  use iotk_fmt_interf
  use iotk_str_interf
  use iotk_misc_interf
  implicit none
  REAL(kind=__IOTK_REAL2), intent(in) :: val(:)
#ifdef __IOTK_WORKAROUND6
  character(len=*)              :: string
#else
  character(len=*), intent(out) :: string
#endif
  integer, intent(out) :: ierr
# 116 "iotk_attr.spp"
  character(len=100) :: tmpval
# 118 "iotk_attr.spp"
  integer :: index,iostat
  ierr = 0
  iostat = 0 
  string(1:1) = iotk_eos
  if(size(val)==0) return
  if(len(string)==0) then
    call iotk_error_issue(ierr,"iotk_write",__FILE__,__LINE__)
# 124 "iotk_attr.spp"
call iotk_error_msg(ierr,"CVS Revision: 1.15 ")
    return
  end if
  do index=1,size(val)
# 141 "iotk_attr.spp"
    write(tmpval,trim(iotk_wfmt("REAL",kind(val),1,-1," ")),iostat=iostat) val(index)
    if(iostat/=0) then
      call iotk_error_issue(ierr,"iotk_write",__FILE__,__LINE__)
# 143 "iotk_attr.spp"
call iotk_error_msg(ierr,"CVS Revision: 1.15 ")
# 143 "iotk_attr.spp"
call iotk_error_msg(ierr,' ')
# 143 "iotk_attr.spp"
call iotk_error_write(ierr,"iostat",iostat)
      return
    end if
    call iotk_strcat(string,trim(adjustl(tmpval))//" ",ierr)
    if(ierr/=0) then
      call iotk_error_issue(ierr,"iotk_write",__FILE__,__LINE__)
# 148 "iotk_attr.spp"
call iotk_error_msg(ierr,"CVS Revision: 1.15 ")
      return
    end if
# 152 "iotk_attr.spp"
  end do
! taglio l'ultimo spazio
  string(iotk_strlen(string):iotk_strlen(string)) = iotk_eos
end subroutine iotk_write_REAL2
# 158 "iotk_attr.spp"

# 162 "iotk_attr.spp"
subroutine iotk_read_REAL2(val,string,index,ierr)
  use iotk_base
  use iotk_error_interf
  use iotk_xtox_interf
  use iotk_misc_interf
  implicit none
  REAL(kind=__IOTK_REAL2), intent(inout) :: val(:)
  character(len=*), intent(in) :: string
  integer, intent(inout) :: index
  integer, intent(out) :: ierr
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
! PER ORA CONSIDERA LE VIRGOLE COME SPAZII
  do
    pos = verify(string(pos1+1:)," ,")+pos1
    if(pos==pos1) exit
    pos = pos - 1
    pos1 = scan(string(pos+1:)," ,")+pos
    if(pos1==pos) pos1 = len(string) + 1
!LEGGI string(pos+1:pos1-1)
    index = index+1
    if(index>maxindex) then
      call iotk_error_issue(ierr,"iotk_read",__FILE__,__LINE__)
# 200 "iotk_attr.spp"
call iotk_error_msg(ierr,"CVS Revision: 1.15 ")
# 200 "iotk_attr.spp"
call iotk_error_msg(ierr,'Too many data')
    end if
# 207 "iotk_attr.spp"
    read(string(pos+1:pos1-1),"(G100.95)",iostat=iostat) val(index)
# 223 "iotk_attr.spp"
    if(iostat/=0) then
      call iotk_error_issue(ierr,"iotk_read",__FILE__,__LINE__)
# 224 "iotk_attr.spp"
call iotk_error_msg(ierr,"CVS Revision: 1.15 ")
# 224 "iotk_attr.spp"
call iotk_error_msg(ierr,'Error reading from string')
# 224 "iotk_attr.spp"
call iotk_error_write(ierr,"string",string(pos+1:pos1-1))
# 224 "iotk_attr.spp"
call iotk_error_write(ierr,"iostat",iostat)
      return
    end if
# 228 "iotk_attr.spp"
    if(pos1>=len(string)) exit
  end do
end subroutine iotk_read_REAL2
# 233 "iotk_attr.spp"

# 236 "iotk_attr.spp"
subroutine iotk_write_attr_REAL2_0(attr,name,val,dummy,first,ierr)
  use iotk_base
  use iotk_error_interf
  use iotk_attr_interf
  use iotk_str_interf
  use iotk_misc_interf
  implicit none
  character(*), intent(inout) :: attr
  character(*), intent(in)    :: name
  REAL(kind=__IOTK_REAL2), intent(in)  :: val 
  type(iotk_dummytype), optional :: dummy
  logical, optional, intent(in)  :: first
  integer, optional, intent(out) :: ierr
  integer :: ierrl
  integer :: attlen
  integer :: vallen
  integer :: namlen
  character :: delim
# 257 "iotk_attr.spp"
  character(iotk_vallenx) :: tmpval
  ierrl = 0
  if(present(first)) then
    if(first) attr(1:1) = iotk_eos
  end if
  attlen = iotk_strlen_trim(attr)
  namlen = iotk_strlen_trim(name)
  if(.not.iotk_check_name(name)) then
    call iotk_error_issue(ierrl,"iotk_write_attr",__FILE__,__LINE__)
# 265 "iotk_attr.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.15 ")
# 265 "iotk_attr.spp"
call iotk_error_msg(ierrl,'Wrong tag name')
# 265 "iotk_attr.spp"
call iotk_error_write(ierrl,"name",name(1:namlen))
    goto 1
  end if
# 282 "iotk_attr.spp"
  delim = '"'
# 284 "iotk_attr.spp"
  call iotk_write((/val/),tmpval,ierrl)
# 288 "iotk_attr.spp"
  if(ierrl/=0) then
    call iotk_error_issue(ierrl,"iotk_write_attr",__FILE__,__LINE__)
# 289 "iotk_attr.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.15 ")
    goto 1
  end if
# 293 "iotk_attr.spp"
  vallen = iotk_strlen(tmpval)
  if(attlen+vallen+namlen+5>len(attr)) then
    call iotk_error_issue(ierrl,"iotk_write_attr",__FILE__,__LINE__)
# 295 "iotk_attr.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.15 ")
# 295 "iotk_attr.spp"
call iotk_error_msg(ierrl,'Attribute dummy argument is too short')
    goto 1
  end if
  attr(attlen+1:attlen+vallen+namlen+5) = " "//name(1:namlen)//"="//delim//tmpval(1:vallen)//delim//iotk_eos
1 continue
  if(present(ierr)) then
    ierr = ierrl
  else
    if(ierrl/=0) call iotk_error_handler(ierrl)
  end if
end subroutine iotk_write_attr_REAL2_0

# 308 "iotk_attr.spp"
subroutine iotk_scan_attr_REAL2_0(attr,name,val,dummy,found,default,eos,ierr)
  use iotk_base
  use iotk_error_interf
  use iotk_attr_interf, only : iotk_read
  use iotk_str_interf
  use iotk_misc_interf
  implicit none
  character(*),             intent(in)  :: attr
  character(*),             intent(in)  :: name
#ifdef __IOTK_WORKAROUND6
  REAL(kind=__IOTK_REAL2)                        :: val 
#else
  REAL(kind=__IOTK_REAL2), intent(out)           :: val 
#endif
  type(iotk_dummytype), optional :: dummy
  logical,        optional, intent(out) :: found
  REAL(kind=__IOTK_REAL2), optional, intent(in)  :: default 
  logical,        optional, intent(in)  :: eos
  integer,        optional, intent(out) :: ierr
  integer :: ierrl
  integer :: attlen,pos,equal,namlen
  character :: delim
  logical :: foundl
  character(iotk_vallenx) :: valc
# 337 "iotk_attr.spp"
  integer :: index
  REAL(kind=__IOTK_REAL2), allocatable :: tmpval (:)
# 340 "iotk_attr.spp"
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
# 351 "iotk_attr.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.15 ")
# 351 "iotk_attr.spp"
call iotk_error_msg(ierrl,'')
# 351 "iotk_attr.spp"
call iotk_error_write(ierrl,"attr",attr(equal+1:attlen))
      goto 1
    end if
    equal = equal + pos
    if(trim(attr(equal-pos:equal-1))==name(1:namlen)) foundl = .true.
    pos = verify(attr(equal+1:attlen)," ")
    if(pos<=0) then
      call iotk_error_issue(ierrl,"iotk_scan_attr",__FILE__,__LINE__)
# 358 "iotk_attr.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.15 ")
      goto 1
    end if
    equal = equal + pos
    delim = attr(equal:equal)
    if(delim/="'" .and. delim/='"') then
      call iotk_error_issue(ierrl,"iotk_scan_attr",__FILE__,__LINE__)
# 364 "iotk_attr.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.15 ")
      goto 1
    end if
    pos = scan(attr(equal+1:attlen),delim)
    if(pos<=0) then
      call iotk_error_issue(ierrl,"iotk_scan_attr",__FILE__,__LINE__)
# 369 "iotk_attr.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.15 ")
      goto 1
    end if
    if(foundl) exit
    equal = equal + pos
  end do
  if(foundl) then
    call iotk_strcpy(valc,attr(equal+1:equal+pos-1),ierrl)
    if(ierrl/=0) then
      call iotk_error_issue(ierrl,"iotk_scan_attr",__FILE__,__LINE__)
# 378 "iotk_attr.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.15 ")
      goto 1
    end if
  else
    goto 1
  end if
# 401 "iotk_attr.spp"
  allocate(tmpval(1))
  index = 0
  call iotk_str_clean(valc)
  call iotk_read(tmpval,valc(1:iotk_strlen(valc)),index,ierrl)
  if(ierrl/=0) then
    call iotk_error_issue(ierrl,"iotk_scan_attr",__FILE__,__LINE__)
# 406 "iotk_attr.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.15 ")
    goto 1
  end if
# 412 "iotk_attr.spp"
  if(index/=1) then
# 414 "iotk_attr.spp"
    call iotk_error_issue(ierrl,"iotk_scan_attr",__FILE__,__LINE__)
# 414 "iotk_attr.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.15 ")
# 414 "iotk_attr.spp"
call iotk_error_msg(ierrl,'Attribute size does not match')
# 414 "iotk_attr.spp"
call iotk_error_write(ierrl,"attr",valc)
# 414 "iotk_attr.spp"
call iotk_error_write(ierrl,"size",size(tmpval))
    goto 1
  end if
# 418 "iotk_attr.spp"
  val = tmpval(1)
# 422 "iotk_attr.spp"
  deallocate(tmpval)
# 424 "iotk_attr.spp"
1 continue
  if(ierrl/=0) foundl=.false.
  if(present(found)) found = foundl
  if(ierrl==0 .and. .not. present(found) .and. .not. present(default) .and. .not. foundl) then
    call iotk_error_issue(ierrl,"iotk_scan_attr",__FILE__,__LINE__)
# 428 "iotk_attr.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.15 ")
# 428 "iotk_attr.spp"
call iotk_error_msg(ierrl,'Attribute not found')
# 428 "iotk_attr.spp"
call iotk_error_write(ierrl,"name",name)
    ierrl = - ierrl
  end if
  if(present(default) .and. .not. foundl) then
# 441 "iotk_attr.spp"
    val = default
# 443 "iotk_attr.spp"
  end if
  if(present(ierr)) then
    ierr = ierrl
  else
    if(ierrl/=0) call iotk_error_handler(ierrl)
  end if
end subroutine iotk_scan_attr_REAL2_0
# 451 "iotk_attr.spp"

#endif
#endif

subroutine iotk_attr_dummy_REAL2_0
  write(0,*)
end subroutine iotk_attr_dummy_REAL2_0

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

#ifdef __IOTK_REAL2
#if 1 <= __IOTK_MAXRANK

# 158 "iotk_attr.spp"

# 233 "iotk_attr.spp"

# 236 "iotk_attr.spp"
subroutine iotk_write_attr_REAL2_1(attr,name,val,dummy,first,ierr)
  use iotk_base
  use iotk_error_interf
  use iotk_attr_interf
  use iotk_str_interf
  use iotk_misc_interf
  implicit none
  character(*), intent(inout) :: attr
  character(*), intent(in)    :: name
  REAL(kind=__IOTK_REAL2), intent(in)  :: val (:)
  type(iotk_dummytype), optional :: dummy
  logical, optional, intent(in)  :: first
  integer, optional, intent(out) :: ierr
  integer :: ierrl
  integer :: attlen
  integer :: vallen
  integer :: namlen
  character :: delim
# 257 "iotk_attr.spp"
  character(iotk_vallenx) :: tmpval
  ierrl = 0
  if(present(first)) then
    if(first) attr(1:1) = iotk_eos
  end if
  attlen = iotk_strlen_trim(attr)
  namlen = iotk_strlen_trim(name)
  if(.not.iotk_check_name(name)) then
    call iotk_error_issue(ierrl,"iotk_write_attr",__FILE__,__LINE__)
# 265 "iotk_attr.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.15 ")
# 265 "iotk_attr.spp"
call iotk_error_msg(ierrl,'Wrong tag name')
# 265 "iotk_attr.spp"
call iotk_error_write(ierrl,"name",name(1:namlen))
    goto 1
  end if
# 282 "iotk_attr.spp"
  delim = '"'
# 286 "iotk_attr.spp"
  call iotk_write(pack(val,mask=.true.),tmpval,ierrl)
# 288 "iotk_attr.spp"
  if(ierrl/=0) then
    call iotk_error_issue(ierrl,"iotk_write_attr",__FILE__,__LINE__)
# 289 "iotk_attr.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.15 ")
    goto 1
  end if
# 293 "iotk_attr.spp"
  vallen = iotk_strlen(tmpval)
  if(attlen+vallen+namlen+5>len(attr)) then
    call iotk_error_issue(ierrl,"iotk_write_attr",__FILE__,__LINE__)
# 295 "iotk_attr.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.15 ")
# 295 "iotk_attr.spp"
call iotk_error_msg(ierrl,'Attribute dummy argument is too short')
    goto 1
  end if
  attr(attlen+1:attlen+vallen+namlen+5) = " "//name(1:namlen)//"="//delim//tmpval(1:vallen)//delim//iotk_eos
1 continue
  if(present(ierr)) then
    ierr = ierrl
  else
    if(ierrl/=0) call iotk_error_handler(ierrl)
  end if
end subroutine iotk_write_attr_REAL2_1

# 308 "iotk_attr.spp"
subroutine iotk_scan_attr_REAL2_1(attr,name,val,dummy,found,default,eos,ierr)
  use iotk_base
  use iotk_error_interf
  use iotk_attr_interf, only : iotk_read
  use iotk_str_interf
  use iotk_misc_interf
  implicit none
  character(*),             intent(in)  :: attr
  character(*),             intent(in)  :: name
#ifdef __IOTK_WORKAROUND6
  REAL(kind=__IOTK_REAL2)                        :: val (:)
#else
  REAL(kind=__IOTK_REAL2), intent(out)           :: val (:)
#endif
  type(iotk_dummytype), optional :: dummy
  logical,        optional, intent(out) :: found
  REAL(kind=__IOTK_REAL2), optional, intent(in)  :: default (:)
  logical,        optional, intent(in)  :: eos
  integer,        optional, intent(out) :: ierr
  integer :: ierrl
  integer :: attlen,pos,equal,namlen
  character :: delim
  logical :: foundl
  character(iotk_vallenx) :: valc
# 337 "iotk_attr.spp"
  integer :: index
  REAL(kind=__IOTK_REAL2), allocatable :: tmpval (:)
# 340 "iotk_attr.spp"
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
# 351 "iotk_attr.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.15 ")
# 351 "iotk_attr.spp"
call iotk_error_msg(ierrl,'')
# 351 "iotk_attr.spp"
call iotk_error_write(ierrl,"attr",attr(equal+1:attlen))
      goto 1
    end if
    equal = equal + pos
    if(trim(attr(equal-pos:equal-1))==name(1:namlen)) foundl = .true.
    pos = verify(attr(equal+1:attlen)," ")
    if(pos<=0) then
      call iotk_error_issue(ierrl,"iotk_scan_attr",__FILE__,__LINE__)
# 358 "iotk_attr.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.15 ")
      goto 1
    end if
    equal = equal + pos
    delim = attr(equal:equal)
    if(delim/="'" .and. delim/='"') then
      call iotk_error_issue(ierrl,"iotk_scan_attr",__FILE__,__LINE__)
# 364 "iotk_attr.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.15 ")
      goto 1
    end if
    pos = scan(attr(equal+1:attlen),delim)
    if(pos<=0) then
      call iotk_error_issue(ierrl,"iotk_scan_attr",__FILE__,__LINE__)
# 369 "iotk_attr.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.15 ")
      goto 1
    end if
    if(foundl) exit
    equal = equal + pos
  end do
  if(foundl) then
    call iotk_strcpy(valc,attr(equal+1:equal+pos-1),ierrl)
    if(ierrl/=0) then
      call iotk_error_issue(ierrl,"iotk_scan_attr",__FILE__,__LINE__)
# 378 "iotk_attr.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.15 ")
      goto 1
    end if
  else
    goto 1
  end if
# 401 "iotk_attr.spp"
  allocate(tmpval(size(val)))
  index = 0
  call iotk_str_clean(valc)
  call iotk_read(tmpval,valc(1:iotk_strlen(valc)),index,ierrl)
  if(ierrl/=0) then
    call iotk_error_issue(ierrl,"iotk_scan_attr",__FILE__,__LINE__)
# 406 "iotk_attr.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.15 ")
    goto 1
  end if
# 412 "iotk_attr.spp"
  if(index/=size(val)) then
# 414 "iotk_attr.spp"
    call iotk_error_issue(ierrl,"iotk_scan_attr",__FILE__,__LINE__)
# 414 "iotk_attr.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.15 ")
# 414 "iotk_attr.spp"
call iotk_error_msg(ierrl,'Attribute size does not match')
# 414 "iotk_attr.spp"
call iotk_error_write(ierrl,"attr",valc)
# 414 "iotk_attr.spp"
call iotk_error_write(ierrl,"size",size(tmpval))
    goto 1
  end if
# 420 "iotk_attr.spp"
  val = reshape (source=tmpval,shape=shape(val))
# 422 "iotk_attr.spp"
  deallocate(tmpval)
# 424 "iotk_attr.spp"
1 continue
  if(ierrl/=0) foundl=.false.
  if(present(found)) found = foundl
  if(ierrl==0 .and. .not. present(found) .and. .not. present(default) .and. .not. foundl) then
    call iotk_error_issue(ierrl,"iotk_scan_attr",__FILE__,__LINE__)
# 428 "iotk_attr.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.15 ")
# 428 "iotk_attr.spp"
call iotk_error_msg(ierrl,'Attribute not found')
# 428 "iotk_attr.spp"
call iotk_error_write(ierrl,"name",name)
    ierrl = - ierrl
  end if
  if(present(default) .and. .not. foundl) then
# 441 "iotk_attr.spp"
    val = default
# 443 "iotk_attr.spp"
  end if
  if(present(ierr)) then
    ierr = ierrl
  else
    if(ierrl/=0) call iotk_error_handler(ierrl)
  end if
end subroutine iotk_scan_attr_REAL2_1
# 451 "iotk_attr.spp"

#endif
#endif

subroutine iotk_attr_dummy_REAL2_1
  write(0,*)
end subroutine iotk_attr_dummy_REAL2_1

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

#ifdef __IOTK_REAL2
#if 2 <= __IOTK_MAXRANK

# 158 "iotk_attr.spp"

# 233 "iotk_attr.spp"

# 236 "iotk_attr.spp"
subroutine iotk_write_attr_REAL2_2(attr,name,val,dummy,first,ierr)
  use iotk_base
  use iotk_error_interf
  use iotk_attr_interf
  use iotk_str_interf
  use iotk_misc_interf
  implicit none
  character(*), intent(inout) :: attr
  character(*), intent(in)    :: name
  REAL(kind=__IOTK_REAL2), intent(in)  :: val (:,:)
  type(iotk_dummytype), optional :: dummy
  logical, optional, intent(in)  :: first
  integer, optional, intent(out) :: ierr
  integer :: ierrl
  integer :: attlen
  integer :: vallen
  integer :: namlen
  character :: delim
# 257 "iotk_attr.spp"
  character(iotk_vallenx) :: tmpval
  ierrl = 0
  if(present(first)) then
    if(first) attr(1:1) = iotk_eos
  end if
  attlen = iotk_strlen_trim(attr)
  namlen = iotk_strlen_trim(name)
  if(.not.iotk_check_name(name)) then
    call iotk_error_issue(ierrl,"iotk_write_attr",__FILE__,__LINE__)
# 265 "iotk_attr.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.15 ")
# 265 "iotk_attr.spp"
call iotk_error_msg(ierrl,'Wrong tag name')
# 265 "iotk_attr.spp"
call iotk_error_write(ierrl,"name",name(1:namlen))
    goto 1
  end if
# 282 "iotk_attr.spp"
  delim = '"'
# 286 "iotk_attr.spp"
  call iotk_write(pack(val,mask=.true.),tmpval,ierrl)
# 288 "iotk_attr.spp"
  if(ierrl/=0) then
    call iotk_error_issue(ierrl,"iotk_write_attr",__FILE__,__LINE__)
# 289 "iotk_attr.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.15 ")
    goto 1
  end if
# 293 "iotk_attr.spp"
  vallen = iotk_strlen(tmpval)
  if(attlen+vallen+namlen+5>len(attr)) then
    call iotk_error_issue(ierrl,"iotk_write_attr",__FILE__,__LINE__)
# 295 "iotk_attr.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.15 ")
# 295 "iotk_attr.spp"
call iotk_error_msg(ierrl,'Attribute dummy argument is too short')
    goto 1
  end if
  attr(attlen+1:attlen+vallen+namlen+5) = " "//name(1:namlen)//"="//delim//tmpval(1:vallen)//delim//iotk_eos
1 continue
  if(present(ierr)) then
    ierr = ierrl
  else
    if(ierrl/=0) call iotk_error_handler(ierrl)
  end if
end subroutine iotk_write_attr_REAL2_2

# 308 "iotk_attr.spp"
subroutine iotk_scan_attr_REAL2_2(attr,name,val,dummy,found,default,eos,ierr)
  use iotk_base
  use iotk_error_interf
  use iotk_attr_interf, only : iotk_read
  use iotk_str_interf
  use iotk_misc_interf
  implicit none
  character(*),             intent(in)  :: attr
  character(*),             intent(in)  :: name
#ifdef __IOTK_WORKAROUND6
  REAL(kind=__IOTK_REAL2)                        :: val (:,:)
#else
  REAL(kind=__IOTK_REAL2), intent(out)           :: val (:,:)
#endif
  type(iotk_dummytype), optional :: dummy
  logical,        optional, intent(out) :: found
  REAL(kind=__IOTK_REAL2), optional, intent(in)  :: default (:,:)
  logical,        optional, intent(in)  :: eos
  integer,        optional, intent(out) :: ierr
  integer :: ierrl
  integer :: attlen,pos,equal,namlen
  character :: delim
  logical :: foundl
  character(iotk_vallenx) :: valc
# 337 "iotk_attr.spp"
  integer :: index
  REAL(kind=__IOTK_REAL2), allocatable :: tmpval (:)
# 340 "iotk_attr.spp"
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
# 351 "iotk_attr.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.15 ")
# 351 "iotk_attr.spp"
call iotk_error_msg(ierrl,'')
# 351 "iotk_attr.spp"
call iotk_error_write(ierrl,"attr",attr(equal+1:attlen))
      goto 1
    end if
    equal = equal + pos
    if(trim(attr(equal-pos:equal-1))==name(1:namlen)) foundl = .true.
    pos = verify(attr(equal+1:attlen)," ")
    if(pos<=0) then
      call iotk_error_issue(ierrl,"iotk_scan_attr",__FILE__,__LINE__)
# 358 "iotk_attr.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.15 ")
      goto 1
    end if
    equal = equal + pos
    delim = attr(equal:equal)
    if(delim/="'" .and. delim/='"') then
      call iotk_error_issue(ierrl,"iotk_scan_attr",__FILE__,__LINE__)
# 364 "iotk_attr.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.15 ")
      goto 1
    end if
    pos = scan(attr(equal+1:attlen),delim)
    if(pos<=0) then
      call iotk_error_issue(ierrl,"iotk_scan_attr",__FILE__,__LINE__)
# 369 "iotk_attr.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.15 ")
      goto 1
    end if
    if(foundl) exit
    equal = equal + pos
  end do
  if(foundl) then
    call iotk_strcpy(valc,attr(equal+1:equal+pos-1),ierrl)
    if(ierrl/=0) then
      call iotk_error_issue(ierrl,"iotk_scan_attr",__FILE__,__LINE__)
# 378 "iotk_attr.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.15 ")
      goto 1
    end if
  else
    goto 1
  end if
# 401 "iotk_attr.spp"
  allocate(tmpval(size(val)))
  index = 0
  call iotk_str_clean(valc)
  call iotk_read(tmpval,valc(1:iotk_strlen(valc)),index,ierrl)
  if(ierrl/=0) then
    call iotk_error_issue(ierrl,"iotk_scan_attr",__FILE__,__LINE__)
# 406 "iotk_attr.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.15 ")
    goto 1
  end if
# 412 "iotk_attr.spp"
  if(index/=size(val)) then
# 414 "iotk_attr.spp"
    call iotk_error_issue(ierrl,"iotk_scan_attr",__FILE__,__LINE__)
# 414 "iotk_attr.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.15 ")
# 414 "iotk_attr.spp"
call iotk_error_msg(ierrl,'Attribute size does not match')
# 414 "iotk_attr.spp"
call iotk_error_write(ierrl,"attr",valc)
# 414 "iotk_attr.spp"
call iotk_error_write(ierrl,"size",size(tmpval))
    goto 1
  end if
# 420 "iotk_attr.spp"
  val = reshape (source=tmpval,shape=shape(val))
# 422 "iotk_attr.spp"
  deallocate(tmpval)
# 424 "iotk_attr.spp"
1 continue
  if(ierrl/=0) foundl=.false.
  if(present(found)) found = foundl
  if(ierrl==0 .and. .not. present(found) .and. .not. present(default) .and. .not. foundl) then
    call iotk_error_issue(ierrl,"iotk_scan_attr",__FILE__,__LINE__)
# 428 "iotk_attr.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.15 ")
# 428 "iotk_attr.spp"
call iotk_error_msg(ierrl,'Attribute not found')
# 428 "iotk_attr.spp"
call iotk_error_write(ierrl,"name",name)
    ierrl = - ierrl
  end if
  if(present(default) .and. .not. foundl) then
# 441 "iotk_attr.spp"
    val = default
# 443 "iotk_attr.spp"
  end if
  if(present(ierr)) then
    ierr = ierrl
  else
    if(ierrl/=0) call iotk_error_handler(ierrl)
  end if
end subroutine iotk_scan_attr_REAL2_2
# 451 "iotk_attr.spp"

#endif
#endif

subroutine iotk_attr_dummy_REAL2_2
  write(0,*)
end subroutine iotk_attr_dummy_REAL2_2

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

#ifdef __IOTK_REAL2
#if 3 <= __IOTK_MAXRANK

# 158 "iotk_attr.spp"

# 233 "iotk_attr.spp"

# 236 "iotk_attr.spp"
subroutine iotk_write_attr_REAL2_3(attr,name,val,dummy,first,ierr)
  use iotk_base
  use iotk_error_interf
  use iotk_attr_interf
  use iotk_str_interf
  use iotk_misc_interf
  implicit none
  character(*), intent(inout) :: attr
  character(*), intent(in)    :: name
  REAL(kind=__IOTK_REAL2), intent(in)  :: val (:,:,:)
  type(iotk_dummytype), optional :: dummy
  logical, optional, intent(in)  :: first
  integer, optional, intent(out) :: ierr
  integer :: ierrl
  integer :: attlen
  integer :: vallen
  integer :: namlen
  character :: delim
# 257 "iotk_attr.spp"
  character(iotk_vallenx) :: tmpval
  ierrl = 0
  if(present(first)) then
    if(first) attr(1:1) = iotk_eos
  end if
  attlen = iotk_strlen_trim(attr)
  namlen = iotk_strlen_trim(name)
  if(.not.iotk_check_name(name)) then
    call iotk_error_issue(ierrl,"iotk_write_attr",__FILE__,__LINE__)
# 265 "iotk_attr.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.15 ")
# 265 "iotk_attr.spp"
call iotk_error_msg(ierrl,'Wrong tag name')
# 265 "iotk_attr.spp"
call iotk_error_write(ierrl,"name",name(1:namlen))
    goto 1
  end if
# 282 "iotk_attr.spp"
  delim = '"'
# 286 "iotk_attr.spp"
  call iotk_write(pack(val,mask=.true.),tmpval,ierrl)
# 288 "iotk_attr.spp"
  if(ierrl/=0) then
    call iotk_error_issue(ierrl,"iotk_write_attr",__FILE__,__LINE__)
# 289 "iotk_attr.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.15 ")
    goto 1
  end if
# 293 "iotk_attr.spp"
  vallen = iotk_strlen(tmpval)
  if(attlen+vallen+namlen+5>len(attr)) then
    call iotk_error_issue(ierrl,"iotk_write_attr",__FILE__,__LINE__)
# 295 "iotk_attr.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.15 ")
# 295 "iotk_attr.spp"
call iotk_error_msg(ierrl,'Attribute dummy argument is too short')
    goto 1
  end if
  attr(attlen+1:attlen+vallen+namlen+5) = " "//name(1:namlen)//"="//delim//tmpval(1:vallen)//delim//iotk_eos
1 continue
  if(present(ierr)) then
    ierr = ierrl
  else
    if(ierrl/=0) call iotk_error_handler(ierrl)
  end if
end subroutine iotk_write_attr_REAL2_3

# 308 "iotk_attr.spp"
subroutine iotk_scan_attr_REAL2_3(attr,name,val,dummy,found,default,eos,ierr)
  use iotk_base
  use iotk_error_interf
  use iotk_attr_interf, only : iotk_read
  use iotk_str_interf
  use iotk_misc_interf
  implicit none
  character(*),             intent(in)  :: attr
  character(*),             intent(in)  :: name
#ifdef __IOTK_WORKAROUND6
  REAL(kind=__IOTK_REAL2)                        :: val (:,:,:)
#else
  REAL(kind=__IOTK_REAL2), intent(out)           :: val (:,:,:)
#endif
  type(iotk_dummytype), optional :: dummy
  logical,        optional, intent(out) :: found
  REAL(kind=__IOTK_REAL2), optional, intent(in)  :: default (:,:,:)
  logical,        optional, intent(in)  :: eos
  integer,        optional, intent(out) :: ierr
  integer :: ierrl
  integer :: attlen,pos,equal,namlen
  character :: delim
  logical :: foundl
  character(iotk_vallenx) :: valc
# 337 "iotk_attr.spp"
  integer :: index
  REAL(kind=__IOTK_REAL2), allocatable :: tmpval (:)
# 340 "iotk_attr.spp"
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
# 351 "iotk_attr.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.15 ")
# 351 "iotk_attr.spp"
call iotk_error_msg(ierrl,'')
# 351 "iotk_attr.spp"
call iotk_error_write(ierrl,"attr",attr(equal+1:attlen))
      goto 1
    end if
    equal = equal + pos
    if(trim(attr(equal-pos:equal-1))==name(1:namlen)) foundl = .true.
    pos = verify(attr(equal+1:attlen)," ")
    if(pos<=0) then
      call iotk_error_issue(ierrl,"iotk_scan_attr",__FILE__,__LINE__)
# 358 "iotk_attr.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.15 ")
      goto 1
    end if
    equal = equal + pos
    delim = attr(equal:equal)
    if(delim/="'" .and. delim/='"') then
      call iotk_error_issue(ierrl,"iotk_scan_attr",__FILE__,__LINE__)
# 364 "iotk_attr.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.15 ")
      goto 1
    end if
    pos = scan(attr(equal+1:attlen),delim)
    if(pos<=0) then
      call iotk_error_issue(ierrl,"iotk_scan_attr",__FILE__,__LINE__)
# 369 "iotk_attr.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.15 ")
      goto 1
    end if
    if(foundl) exit
    equal = equal + pos
  end do
  if(foundl) then
    call iotk_strcpy(valc,attr(equal+1:equal+pos-1),ierrl)
    if(ierrl/=0) then
      call iotk_error_issue(ierrl,"iotk_scan_attr",__FILE__,__LINE__)
# 378 "iotk_attr.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.15 ")
      goto 1
    end if
  else
    goto 1
  end if
# 401 "iotk_attr.spp"
  allocate(tmpval(size(val)))
  index = 0
  call iotk_str_clean(valc)
  call iotk_read(tmpval,valc(1:iotk_strlen(valc)),index,ierrl)
  if(ierrl/=0) then
    call iotk_error_issue(ierrl,"iotk_scan_attr",__FILE__,__LINE__)
# 406 "iotk_attr.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.15 ")
    goto 1
  end if
# 412 "iotk_attr.spp"
  if(index/=size(val)) then
# 414 "iotk_attr.spp"
    call iotk_error_issue(ierrl,"iotk_scan_attr",__FILE__,__LINE__)
# 414 "iotk_attr.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.15 ")
# 414 "iotk_attr.spp"
call iotk_error_msg(ierrl,'Attribute size does not match')
# 414 "iotk_attr.spp"
call iotk_error_write(ierrl,"attr",valc)
# 414 "iotk_attr.spp"
call iotk_error_write(ierrl,"size",size(tmpval))
    goto 1
  end if
# 420 "iotk_attr.spp"
  val = reshape (source=tmpval,shape=shape(val))
# 422 "iotk_attr.spp"
  deallocate(tmpval)
# 424 "iotk_attr.spp"
1 continue
  if(ierrl/=0) foundl=.false.
  if(present(found)) found = foundl
  if(ierrl==0 .and. .not. present(found) .and. .not. present(default) .and. .not. foundl) then
    call iotk_error_issue(ierrl,"iotk_scan_attr",__FILE__,__LINE__)
# 428 "iotk_attr.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.15 ")
# 428 "iotk_attr.spp"
call iotk_error_msg(ierrl,'Attribute not found')
# 428 "iotk_attr.spp"
call iotk_error_write(ierrl,"name",name)
    ierrl = - ierrl
  end if
  if(present(default) .and. .not. foundl) then
# 441 "iotk_attr.spp"
    val = default
# 443 "iotk_attr.spp"
  end if
  if(present(ierr)) then
    ierr = ierrl
  else
    if(ierrl/=0) call iotk_error_handler(ierrl)
  end if
end subroutine iotk_scan_attr_REAL2_3
# 451 "iotk_attr.spp"

#endif
#endif

subroutine iotk_attr_dummy_REAL2_3
  write(0,*)
end subroutine iotk_attr_dummy_REAL2_3

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

#ifdef __IOTK_REAL2
#if 4 <= __IOTK_MAXRANK

# 158 "iotk_attr.spp"

# 233 "iotk_attr.spp"

# 236 "iotk_attr.spp"
subroutine iotk_write_attr_REAL2_4(attr,name,val,dummy,first,ierr)
  use iotk_base
  use iotk_error_interf
  use iotk_attr_interf
  use iotk_str_interf
  use iotk_misc_interf
  implicit none
  character(*), intent(inout) :: attr
  character(*), intent(in)    :: name
  REAL(kind=__IOTK_REAL2), intent(in)  :: val (:,:,:,:)
  type(iotk_dummytype), optional :: dummy
  logical, optional, intent(in)  :: first
  integer, optional, intent(out) :: ierr
  integer :: ierrl
  integer :: attlen
  integer :: vallen
  integer :: namlen
  character :: delim
# 257 "iotk_attr.spp"
  character(iotk_vallenx) :: tmpval
  ierrl = 0
  if(present(first)) then
    if(first) attr(1:1) = iotk_eos
  end if
  attlen = iotk_strlen_trim(attr)
  namlen = iotk_strlen_trim(name)
  if(.not.iotk_check_name(name)) then
    call iotk_error_issue(ierrl,"iotk_write_attr",__FILE__,__LINE__)
# 265 "iotk_attr.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.15 ")
# 265 "iotk_attr.spp"
call iotk_error_msg(ierrl,'Wrong tag name')
# 265 "iotk_attr.spp"
call iotk_error_write(ierrl,"name",name(1:namlen))
    goto 1
  end if
# 282 "iotk_attr.spp"
  delim = '"'
# 286 "iotk_attr.spp"
  call iotk_write(pack(val,mask=.true.),tmpval,ierrl)
# 288 "iotk_attr.spp"
  if(ierrl/=0) then
    call iotk_error_issue(ierrl,"iotk_write_attr",__FILE__,__LINE__)
# 289 "iotk_attr.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.15 ")
    goto 1
  end if
# 293 "iotk_attr.spp"
  vallen = iotk_strlen(tmpval)
  if(attlen+vallen+namlen+5>len(attr)) then
    call iotk_error_issue(ierrl,"iotk_write_attr",__FILE__,__LINE__)
# 295 "iotk_attr.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.15 ")
# 295 "iotk_attr.spp"
call iotk_error_msg(ierrl,'Attribute dummy argument is too short')
    goto 1
  end if
  attr(attlen+1:attlen+vallen+namlen+5) = " "//name(1:namlen)//"="//delim//tmpval(1:vallen)//delim//iotk_eos
1 continue
  if(present(ierr)) then
    ierr = ierrl
  else
    if(ierrl/=0) call iotk_error_handler(ierrl)
  end if
end subroutine iotk_write_attr_REAL2_4

# 308 "iotk_attr.spp"
subroutine iotk_scan_attr_REAL2_4(attr,name,val,dummy,found,default,eos,ierr)
  use iotk_base
  use iotk_error_interf
  use iotk_attr_interf, only : iotk_read
  use iotk_str_interf
  use iotk_misc_interf
  implicit none
  character(*),             intent(in)  :: attr
  character(*),             intent(in)  :: name
#ifdef __IOTK_WORKAROUND6
  REAL(kind=__IOTK_REAL2)                        :: val (:,:,:,:)
#else
  REAL(kind=__IOTK_REAL2), intent(out)           :: val (:,:,:,:)
#endif
  type(iotk_dummytype), optional :: dummy
  logical,        optional, intent(out) :: found
  REAL(kind=__IOTK_REAL2), optional, intent(in)  :: default (:,:,:,:)
  logical,        optional, intent(in)  :: eos
  integer,        optional, intent(out) :: ierr
  integer :: ierrl
  integer :: attlen,pos,equal,namlen
  character :: delim
  logical :: foundl
  character(iotk_vallenx) :: valc
# 337 "iotk_attr.spp"
  integer :: index
  REAL(kind=__IOTK_REAL2), allocatable :: tmpval (:)
# 340 "iotk_attr.spp"
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
# 351 "iotk_attr.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.15 ")
# 351 "iotk_attr.spp"
call iotk_error_msg(ierrl,'')
# 351 "iotk_attr.spp"
call iotk_error_write(ierrl,"attr",attr(equal+1:attlen))
      goto 1
    end if
    equal = equal + pos
    if(trim(attr(equal-pos:equal-1))==name(1:namlen)) foundl = .true.
    pos = verify(attr(equal+1:attlen)," ")
    if(pos<=0) then
      call iotk_error_issue(ierrl,"iotk_scan_attr",__FILE__,__LINE__)
# 358 "iotk_attr.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.15 ")
      goto 1
    end if
    equal = equal + pos
    delim = attr(equal:equal)
    if(delim/="'" .and. delim/='"') then
      call iotk_error_issue(ierrl,"iotk_scan_attr",__FILE__,__LINE__)
# 364 "iotk_attr.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.15 ")
      goto 1
    end if
    pos = scan(attr(equal+1:attlen),delim)
    if(pos<=0) then
      call iotk_error_issue(ierrl,"iotk_scan_attr",__FILE__,__LINE__)
# 369 "iotk_attr.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.15 ")
      goto 1
    end if
    if(foundl) exit
    equal = equal + pos
  end do
  if(foundl) then
    call iotk_strcpy(valc,attr(equal+1:equal+pos-1),ierrl)
    if(ierrl/=0) then
      call iotk_error_issue(ierrl,"iotk_scan_attr",__FILE__,__LINE__)
# 378 "iotk_attr.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.15 ")
      goto 1
    end if
  else
    goto 1
  end if
# 401 "iotk_attr.spp"
  allocate(tmpval(size(val)))
  index = 0
  call iotk_str_clean(valc)
  call iotk_read(tmpval,valc(1:iotk_strlen(valc)),index,ierrl)
  if(ierrl/=0) then
    call iotk_error_issue(ierrl,"iotk_scan_attr",__FILE__,__LINE__)
# 406 "iotk_attr.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.15 ")
    goto 1
  end if
# 412 "iotk_attr.spp"
  if(index/=size(val)) then
# 414 "iotk_attr.spp"
    call iotk_error_issue(ierrl,"iotk_scan_attr",__FILE__,__LINE__)
# 414 "iotk_attr.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.15 ")
# 414 "iotk_attr.spp"
call iotk_error_msg(ierrl,'Attribute size does not match')
# 414 "iotk_attr.spp"
call iotk_error_write(ierrl,"attr",valc)
# 414 "iotk_attr.spp"
call iotk_error_write(ierrl,"size",size(tmpval))
    goto 1
  end if
# 420 "iotk_attr.spp"
  val = reshape (source=tmpval,shape=shape(val))
# 422 "iotk_attr.spp"
  deallocate(tmpval)
# 424 "iotk_attr.spp"
1 continue
  if(ierrl/=0) foundl=.false.
  if(present(found)) found = foundl
  if(ierrl==0 .and. .not. present(found) .and. .not. present(default) .and. .not. foundl) then
    call iotk_error_issue(ierrl,"iotk_scan_attr",__FILE__,__LINE__)
# 428 "iotk_attr.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.15 ")
# 428 "iotk_attr.spp"
call iotk_error_msg(ierrl,'Attribute not found')
# 428 "iotk_attr.spp"
call iotk_error_write(ierrl,"name",name)
    ierrl = - ierrl
  end if
  if(present(default) .and. .not. foundl) then
# 441 "iotk_attr.spp"
    val = default
# 443 "iotk_attr.spp"
  end if
  if(present(ierr)) then
    ierr = ierrl
  else
    if(ierrl/=0) call iotk_error_handler(ierrl)
  end if
end subroutine iotk_scan_attr_REAL2_4
# 451 "iotk_attr.spp"

#endif
#endif

subroutine iotk_attr_dummy_REAL2_4
  write(0,*)
end subroutine iotk_attr_dummy_REAL2_4

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

#ifdef __IOTK_REAL2
#if 5 <= __IOTK_MAXRANK

# 158 "iotk_attr.spp"

# 233 "iotk_attr.spp"

# 236 "iotk_attr.spp"
subroutine iotk_write_attr_REAL2_5(attr,name,val,dummy,first,ierr)
  use iotk_base
  use iotk_error_interf
  use iotk_attr_interf
  use iotk_str_interf
  use iotk_misc_interf
  implicit none
  character(*), intent(inout) :: attr
  character(*), intent(in)    :: name
  REAL(kind=__IOTK_REAL2), intent(in)  :: val (:,:,:,:,:)
  type(iotk_dummytype), optional :: dummy
  logical, optional, intent(in)  :: first
  integer, optional, intent(out) :: ierr
  integer :: ierrl
  integer :: attlen
  integer :: vallen
  integer :: namlen
  character :: delim
# 257 "iotk_attr.spp"
  character(iotk_vallenx) :: tmpval
  ierrl = 0
  if(present(first)) then
    if(first) attr(1:1) = iotk_eos
  end if
  attlen = iotk_strlen_trim(attr)
  namlen = iotk_strlen_trim(name)
  if(.not.iotk_check_name(name)) then
    call iotk_error_issue(ierrl,"iotk_write_attr",__FILE__,__LINE__)
# 265 "iotk_attr.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.15 ")
# 265 "iotk_attr.spp"
call iotk_error_msg(ierrl,'Wrong tag name')
# 265 "iotk_attr.spp"
call iotk_error_write(ierrl,"name",name(1:namlen))
    goto 1
  end if
# 282 "iotk_attr.spp"
  delim = '"'
# 286 "iotk_attr.spp"
  call iotk_write(pack(val,mask=.true.),tmpval,ierrl)
# 288 "iotk_attr.spp"
  if(ierrl/=0) then
    call iotk_error_issue(ierrl,"iotk_write_attr",__FILE__,__LINE__)
# 289 "iotk_attr.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.15 ")
    goto 1
  end if
# 293 "iotk_attr.spp"
  vallen = iotk_strlen(tmpval)
  if(attlen+vallen+namlen+5>len(attr)) then
    call iotk_error_issue(ierrl,"iotk_write_attr",__FILE__,__LINE__)
# 295 "iotk_attr.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.15 ")
# 295 "iotk_attr.spp"
call iotk_error_msg(ierrl,'Attribute dummy argument is too short')
    goto 1
  end if
  attr(attlen+1:attlen+vallen+namlen+5) = " "//name(1:namlen)//"="//delim//tmpval(1:vallen)//delim//iotk_eos
1 continue
  if(present(ierr)) then
    ierr = ierrl
  else
    if(ierrl/=0) call iotk_error_handler(ierrl)
  end if
end subroutine iotk_write_attr_REAL2_5

# 308 "iotk_attr.spp"
subroutine iotk_scan_attr_REAL2_5(attr,name,val,dummy,found,default,eos,ierr)
  use iotk_base
  use iotk_error_interf
  use iotk_attr_interf, only : iotk_read
  use iotk_str_interf
  use iotk_misc_interf
  implicit none
  character(*),             intent(in)  :: attr
  character(*),             intent(in)  :: name
#ifdef __IOTK_WORKAROUND6
  REAL(kind=__IOTK_REAL2)                        :: val (:,:,:,:,:)
#else
  REAL(kind=__IOTK_REAL2), intent(out)           :: val (:,:,:,:,:)
#endif
  type(iotk_dummytype), optional :: dummy
  logical,        optional, intent(out) :: found
  REAL(kind=__IOTK_REAL2), optional, intent(in)  :: default (:,:,:,:,:)
  logical,        optional, intent(in)  :: eos
  integer,        optional, intent(out) :: ierr
  integer :: ierrl
  integer :: attlen,pos,equal,namlen
  character :: delim
  logical :: foundl
  character(iotk_vallenx) :: valc
# 337 "iotk_attr.spp"
  integer :: index
  REAL(kind=__IOTK_REAL2), allocatable :: tmpval (:)
# 340 "iotk_attr.spp"
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
# 351 "iotk_attr.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.15 ")
# 351 "iotk_attr.spp"
call iotk_error_msg(ierrl,'')
# 351 "iotk_attr.spp"
call iotk_error_write(ierrl,"attr",attr(equal+1:attlen))
      goto 1
    end if
    equal = equal + pos
    if(trim(attr(equal-pos:equal-1))==name(1:namlen)) foundl = .true.
    pos = verify(attr(equal+1:attlen)," ")
    if(pos<=0) then
      call iotk_error_issue(ierrl,"iotk_scan_attr",__FILE__,__LINE__)
# 358 "iotk_attr.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.15 ")
      goto 1
    end if
    equal = equal + pos
    delim = attr(equal:equal)
    if(delim/="'" .and. delim/='"') then
      call iotk_error_issue(ierrl,"iotk_scan_attr",__FILE__,__LINE__)
# 364 "iotk_attr.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.15 ")
      goto 1
    end if
    pos = scan(attr(equal+1:attlen),delim)
    if(pos<=0) then
      call iotk_error_issue(ierrl,"iotk_scan_attr",__FILE__,__LINE__)
# 369 "iotk_attr.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.15 ")
      goto 1
    end if
    if(foundl) exit
    equal = equal + pos
  end do
  if(foundl) then
    call iotk_strcpy(valc,attr(equal+1:equal+pos-1),ierrl)
    if(ierrl/=0) then
      call iotk_error_issue(ierrl,"iotk_scan_attr",__FILE__,__LINE__)
# 378 "iotk_attr.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.15 ")
      goto 1
    end if
  else
    goto 1
  end if
# 401 "iotk_attr.spp"
  allocate(tmpval(size(val)))
  index = 0
  call iotk_str_clean(valc)
  call iotk_read(tmpval,valc(1:iotk_strlen(valc)),index,ierrl)
  if(ierrl/=0) then
    call iotk_error_issue(ierrl,"iotk_scan_attr",__FILE__,__LINE__)
# 406 "iotk_attr.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.15 ")
    goto 1
  end if
# 412 "iotk_attr.spp"
  if(index/=size(val)) then
# 414 "iotk_attr.spp"
    call iotk_error_issue(ierrl,"iotk_scan_attr",__FILE__,__LINE__)
# 414 "iotk_attr.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.15 ")
# 414 "iotk_attr.spp"
call iotk_error_msg(ierrl,'Attribute size does not match')
# 414 "iotk_attr.spp"
call iotk_error_write(ierrl,"attr",valc)
# 414 "iotk_attr.spp"
call iotk_error_write(ierrl,"size",size(tmpval))
    goto 1
  end if
# 420 "iotk_attr.spp"
  val = reshape (source=tmpval,shape=shape(val))
# 422 "iotk_attr.spp"
  deallocate(tmpval)
# 424 "iotk_attr.spp"
1 continue
  if(ierrl/=0) foundl=.false.
  if(present(found)) found = foundl
  if(ierrl==0 .and. .not. present(found) .and. .not. present(default) .and. .not. foundl) then
    call iotk_error_issue(ierrl,"iotk_scan_attr",__FILE__,__LINE__)
# 428 "iotk_attr.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.15 ")
# 428 "iotk_attr.spp"
call iotk_error_msg(ierrl,'Attribute not found')
# 428 "iotk_attr.spp"
call iotk_error_write(ierrl,"name",name)
    ierrl = - ierrl
  end if
  if(present(default) .and. .not. foundl) then
# 441 "iotk_attr.spp"
    val = default
# 443 "iotk_attr.spp"
  end if
  if(present(ierr)) then
    ierr = ierrl
  else
    if(ierrl/=0) call iotk_error_handler(ierrl)
  end if
end subroutine iotk_scan_attr_REAL2_5
# 451 "iotk_attr.spp"

#endif
#endif

subroutine iotk_attr_dummy_REAL2_5
  write(0,*)
end subroutine iotk_attr_dummy_REAL2_5

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

#ifdef __IOTK_REAL2
#if 6 <= __IOTK_MAXRANK

# 158 "iotk_attr.spp"

# 233 "iotk_attr.spp"

# 236 "iotk_attr.spp"
subroutine iotk_write_attr_REAL2_6(attr,name,val,dummy,first,ierr)
  use iotk_base
  use iotk_error_interf
  use iotk_attr_interf
  use iotk_str_interf
  use iotk_misc_interf
  implicit none
  character(*), intent(inout) :: attr
  character(*), intent(in)    :: name
  REAL(kind=__IOTK_REAL2), intent(in)  :: val (:,:,:,:,:,:)
  type(iotk_dummytype), optional :: dummy
  logical, optional, intent(in)  :: first
  integer, optional, intent(out) :: ierr
  integer :: ierrl
  integer :: attlen
  integer :: vallen
  integer :: namlen
  character :: delim
# 257 "iotk_attr.spp"
  character(iotk_vallenx) :: tmpval
  ierrl = 0
  if(present(first)) then
    if(first) attr(1:1) = iotk_eos
  end if
  attlen = iotk_strlen_trim(attr)
  namlen = iotk_strlen_trim(name)
  if(.not.iotk_check_name(name)) then
    call iotk_error_issue(ierrl,"iotk_write_attr",__FILE__,__LINE__)
# 265 "iotk_attr.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.15 ")
# 265 "iotk_attr.spp"
call iotk_error_msg(ierrl,'Wrong tag name')
# 265 "iotk_attr.spp"
call iotk_error_write(ierrl,"name",name(1:namlen))
    goto 1
  end if
# 282 "iotk_attr.spp"
  delim = '"'
# 286 "iotk_attr.spp"
  call iotk_write(pack(val,mask=.true.),tmpval,ierrl)
# 288 "iotk_attr.spp"
  if(ierrl/=0) then
    call iotk_error_issue(ierrl,"iotk_write_attr",__FILE__,__LINE__)
# 289 "iotk_attr.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.15 ")
    goto 1
  end if
# 293 "iotk_attr.spp"
  vallen = iotk_strlen(tmpval)
  if(attlen+vallen+namlen+5>len(attr)) then
    call iotk_error_issue(ierrl,"iotk_write_attr",__FILE__,__LINE__)
# 295 "iotk_attr.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.15 ")
# 295 "iotk_attr.spp"
call iotk_error_msg(ierrl,'Attribute dummy argument is too short')
    goto 1
  end if
  attr(attlen+1:attlen+vallen+namlen+5) = " "//name(1:namlen)//"="//delim//tmpval(1:vallen)//delim//iotk_eos
1 continue
  if(present(ierr)) then
    ierr = ierrl
  else
    if(ierrl/=0) call iotk_error_handler(ierrl)
  end if
end subroutine iotk_write_attr_REAL2_6

# 308 "iotk_attr.spp"
subroutine iotk_scan_attr_REAL2_6(attr,name,val,dummy,found,default,eos,ierr)
  use iotk_base
  use iotk_error_interf
  use iotk_attr_interf, only : iotk_read
  use iotk_str_interf
  use iotk_misc_interf
  implicit none
  character(*),             intent(in)  :: attr
  character(*),             intent(in)  :: name
#ifdef __IOTK_WORKAROUND6
  REAL(kind=__IOTK_REAL2)                        :: val (:,:,:,:,:,:)
#else
  REAL(kind=__IOTK_REAL2), intent(out)           :: val (:,:,:,:,:,:)
#endif
  type(iotk_dummytype), optional :: dummy
  logical,        optional, intent(out) :: found
  REAL(kind=__IOTK_REAL2), optional, intent(in)  :: default (:,:,:,:,:,:)
  logical,        optional, intent(in)  :: eos
  integer,        optional, intent(out) :: ierr
  integer :: ierrl
  integer :: attlen,pos,equal,namlen
  character :: delim
  logical :: foundl
  character(iotk_vallenx) :: valc
# 337 "iotk_attr.spp"
  integer :: index
  REAL(kind=__IOTK_REAL2), allocatable :: tmpval (:)
# 340 "iotk_attr.spp"
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
# 351 "iotk_attr.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.15 ")
# 351 "iotk_attr.spp"
call iotk_error_msg(ierrl,'')
# 351 "iotk_attr.spp"
call iotk_error_write(ierrl,"attr",attr(equal+1:attlen))
      goto 1
    end if
    equal = equal + pos
    if(trim(attr(equal-pos:equal-1))==name(1:namlen)) foundl = .true.
    pos = verify(attr(equal+1:attlen)," ")
    if(pos<=0) then
      call iotk_error_issue(ierrl,"iotk_scan_attr",__FILE__,__LINE__)
# 358 "iotk_attr.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.15 ")
      goto 1
    end if
    equal = equal + pos
    delim = attr(equal:equal)
    if(delim/="'" .and. delim/='"') then
      call iotk_error_issue(ierrl,"iotk_scan_attr",__FILE__,__LINE__)
# 364 "iotk_attr.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.15 ")
      goto 1
    end if
    pos = scan(attr(equal+1:attlen),delim)
    if(pos<=0) then
      call iotk_error_issue(ierrl,"iotk_scan_attr",__FILE__,__LINE__)
# 369 "iotk_attr.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.15 ")
      goto 1
    end if
    if(foundl) exit
    equal = equal + pos
  end do
  if(foundl) then
    call iotk_strcpy(valc,attr(equal+1:equal+pos-1),ierrl)
    if(ierrl/=0) then
      call iotk_error_issue(ierrl,"iotk_scan_attr",__FILE__,__LINE__)
# 378 "iotk_attr.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.15 ")
      goto 1
    end if
  else
    goto 1
  end if
# 401 "iotk_attr.spp"
  allocate(tmpval(size(val)))
  index = 0
  call iotk_str_clean(valc)
  call iotk_read(tmpval,valc(1:iotk_strlen(valc)),index,ierrl)
  if(ierrl/=0) then
    call iotk_error_issue(ierrl,"iotk_scan_attr",__FILE__,__LINE__)
# 406 "iotk_attr.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.15 ")
    goto 1
  end if
# 412 "iotk_attr.spp"
  if(index/=size(val)) then
# 414 "iotk_attr.spp"
    call iotk_error_issue(ierrl,"iotk_scan_attr",__FILE__,__LINE__)
# 414 "iotk_attr.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.15 ")
# 414 "iotk_attr.spp"
call iotk_error_msg(ierrl,'Attribute size does not match')
# 414 "iotk_attr.spp"
call iotk_error_write(ierrl,"attr",valc)
# 414 "iotk_attr.spp"
call iotk_error_write(ierrl,"size",size(tmpval))
    goto 1
  end if
# 420 "iotk_attr.spp"
  val = reshape (source=tmpval,shape=shape(val))
# 422 "iotk_attr.spp"
  deallocate(tmpval)
# 424 "iotk_attr.spp"
1 continue
  if(ierrl/=0) foundl=.false.
  if(present(found)) found = foundl
  if(ierrl==0 .and. .not. present(found) .and. .not. present(default) .and. .not. foundl) then
    call iotk_error_issue(ierrl,"iotk_scan_attr",__FILE__,__LINE__)
# 428 "iotk_attr.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.15 ")
# 428 "iotk_attr.spp"
call iotk_error_msg(ierrl,'Attribute not found')
# 428 "iotk_attr.spp"
call iotk_error_write(ierrl,"name",name)
    ierrl = - ierrl
  end if
  if(present(default) .and. .not. foundl) then
# 441 "iotk_attr.spp"
    val = default
# 443 "iotk_attr.spp"
  end if
  if(present(ierr)) then
    ierr = ierrl
  else
    if(ierrl/=0) call iotk_error_handler(ierrl)
  end if
end subroutine iotk_scan_attr_REAL2_6
# 451 "iotk_attr.spp"

#endif
#endif

subroutine iotk_attr_dummy_REAL2_6
  write(0,*)
end subroutine iotk_attr_dummy_REAL2_6

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

#ifdef __IOTK_REAL2
#if 7 <= __IOTK_MAXRANK

# 158 "iotk_attr.spp"

# 233 "iotk_attr.spp"

# 236 "iotk_attr.spp"
subroutine iotk_write_attr_REAL2_7(attr,name,val,dummy,first,ierr)
  use iotk_base
  use iotk_error_interf
  use iotk_attr_interf
  use iotk_str_interf
  use iotk_misc_interf
  implicit none
  character(*), intent(inout) :: attr
  character(*), intent(in)    :: name
  REAL(kind=__IOTK_REAL2), intent(in)  :: val (:,:,:,:,:,:,:)
  type(iotk_dummytype), optional :: dummy
  logical, optional, intent(in)  :: first
  integer, optional, intent(out) :: ierr
  integer :: ierrl
  integer :: attlen
  integer :: vallen
  integer :: namlen
  character :: delim
# 257 "iotk_attr.spp"
  character(iotk_vallenx) :: tmpval
  ierrl = 0
  if(present(first)) then
    if(first) attr(1:1) = iotk_eos
  end if
  attlen = iotk_strlen_trim(attr)
  namlen = iotk_strlen_trim(name)
  if(.not.iotk_check_name(name)) then
    call iotk_error_issue(ierrl,"iotk_write_attr",__FILE__,__LINE__)
# 265 "iotk_attr.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.15 ")
# 265 "iotk_attr.spp"
call iotk_error_msg(ierrl,'Wrong tag name')
# 265 "iotk_attr.spp"
call iotk_error_write(ierrl,"name",name(1:namlen))
    goto 1
  end if
# 282 "iotk_attr.spp"
  delim = '"'
# 286 "iotk_attr.spp"
  call iotk_write(pack(val,mask=.true.),tmpval,ierrl)
# 288 "iotk_attr.spp"
  if(ierrl/=0) then
    call iotk_error_issue(ierrl,"iotk_write_attr",__FILE__,__LINE__)
# 289 "iotk_attr.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.15 ")
    goto 1
  end if
# 293 "iotk_attr.spp"
  vallen = iotk_strlen(tmpval)
  if(attlen+vallen+namlen+5>len(attr)) then
    call iotk_error_issue(ierrl,"iotk_write_attr",__FILE__,__LINE__)
# 295 "iotk_attr.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.15 ")
# 295 "iotk_attr.spp"
call iotk_error_msg(ierrl,'Attribute dummy argument is too short')
    goto 1
  end if
  attr(attlen+1:attlen+vallen+namlen+5) = " "//name(1:namlen)//"="//delim//tmpval(1:vallen)//delim//iotk_eos
1 continue
  if(present(ierr)) then
    ierr = ierrl
  else
    if(ierrl/=0) call iotk_error_handler(ierrl)
  end if
end subroutine iotk_write_attr_REAL2_7

# 308 "iotk_attr.spp"
subroutine iotk_scan_attr_REAL2_7(attr,name,val,dummy,found,default,eos,ierr)
  use iotk_base
  use iotk_error_interf
  use iotk_attr_interf, only : iotk_read
  use iotk_str_interf
  use iotk_misc_interf
  implicit none
  character(*),             intent(in)  :: attr
  character(*),             intent(in)  :: name
#ifdef __IOTK_WORKAROUND6
  REAL(kind=__IOTK_REAL2)                        :: val (:,:,:,:,:,:,:)
#else
  REAL(kind=__IOTK_REAL2), intent(out)           :: val (:,:,:,:,:,:,:)
#endif
  type(iotk_dummytype), optional :: dummy
  logical,        optional, intent(out) :: found
  REAL(kind=__IOTK_REAL2), optional, intent(in)  :: default (:,:,:,:,:,:,:)
  logical,        optional, intent(in)  :: eos
  integer,        optional, intent(out) :: ierr
  integer :: ierrl
  integer :: attlen,pos,equal,namlen
  character :: delim
  logical :: foundl
  character(iotk_vallenx) :: valc
# 337 "iotk_attr.spp"
  integer :: index
  REAL(kind=__IOTK_REAL2), allocatable :: tmpval (:)
# 340 "iotk_attr.spp"
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
# 351 "iotk_attr.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.15 ")
# 351 "iotk_attr.spp"
call iotk_error_msg(ierrl,'')
# 351 "iotk_attr.spp"
call iotk_error_write(ierrl,"attr",attr(equal+1:attlen))
      goto 1
    end if
    equal = equal + pos
    if(trim(attr(equal-pos:equal-1))==name(1:namlen)) foundl = .true.
    pos = verify(attr(equal+1:attlen)," ")
    if(pos<=0) then
      call iotk_error_issue(ierrl,"iotk_scan_attr",__FILE__,__LINE__)
# 358 "iotk_attr.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.15 ")
      goto 1
    end if
    equal = equal + pos
    delim = attr(equal:equal)
    if(delim/="'" .and. delim/='"') then
      call iotk_error_issue(ierrl,"iotk_scan_attr",__FILE__,__LINE__)
# 364 "iotk_attr.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.15 ")
      goto 1
    end if
    pos = scan(attr(equal+1:attlen),delim)
    if(pos<=0) then
      call iotk_error_issue(ierrl,"iotk_scan_attr",__FILE__,__LINE__)
# 369 "iotk_attr.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.15 ")
      goto 1
    end if
    if(foundl) exit
    equal = equal + pos
  end do
  if(foundl) then
    call iotk_strcpy(valc,attr(equal+1:equal+pos-1),ierrl)
    if(ierrl/=0) then
      call iotk_error_issue(ierrl,"iotk_scan_attr",__FILE__,__LINE__)
# 378 "iotk_attr.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.15 ")
      goto 1
    end if
  else
    goto 1
  end if
# 401 "iotk_attr.spp"
  allocate(tmpval(size(val)))
  index = 0
  call iotk_str_clean(valc)
  call iotk_read(tmpval,valc(1:iotk_strlen(valc)),index,ierrl)
  if(ierrl/=0) then
    call iotk_error_issue(ierrl,"iotk_scan_attr",__FILE__,__LINE__)
# 406 "iotk_attr.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.15 ")
    goto 1
  end if
# 412 "iotk_attr.spp"
  if(index/=size(val)) then
# 414 "iotk_attr.spp"
    call iotk_error_issue(ierrl,"iotk_scan_attr",__FILE__,__LINE__)
# 414 "iotk_attr.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.15 ")
# 414 "iotk_attr.spp"
call iotk_error_msg(ierrl,'Attribute size does not match')
# 414 "iotk_attr.spp"
call iotk_error_write(ierrl,"attr",valc)
# 414 "iotk_attr.spp"
call iotk_error_write(ierrl,"size",size(tmpval))
    goto 1
  end if
# 420 "iotk_attr.spp"
  val = reshape (source=tmpval,shape=shape(val))
# 422 "iotk_attr.spp"
  deallocate(tmpval)
# 424 "iotk_attr.spp"
1 continue
  if(ierrl/=0) foundl=.false.
  if(present(found)) found = foundl
  if(ierrl==0 .and. .not. present(found) .and. .not. present(default) .and. .not. foundl) then
    call iotk_error_issue(ierrl,"iotk_scan_attr",__FILE__,__LINE__)
# 428 "iotk_attr.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.15 ")
# 428 "iotk_attr.spp"
call iotk_error_msg(ierrl,'Attribute not found')
# 428 "iotk_attr.spp"
call iotk_error_write(ierrl,"name",name)
    ierrl = - ierrl
  end if
  if(present(default) .and. .not. foundl) then
# 441 "iotk_attr.spp"
    val = default
# 443 "iotk_attr.spp"
  end if
  if(present(ierr)) then
    ierr = ierrl
  else
    if(ierrl/=0) call iotk_error_handler(ierrl)
  end if
end subroutine iotk_scan_attr_REAL2_7
# 451 "iotk_attr.spp"

#endif
#endif

subroutine iotk_attr_dummy_REAL2_7
  write(0,*)
end subroutine iotk_attr_dummy_REAL2_7

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

#ifdef __IOTK_REAL3
#if 0 <= __IOTK_MAXRANK

# 83 "iotk_attr.spp"
! This is needed as a workaround for bugged pack 
subroutine iotk_private_pack_REAL3(out,in,n,l)
    use iotk_base
    implicit none
    integer,                                    intent(in)  :: n,l
# 92 "iotk_attr.spp"
    REAL (kind=__IOTK_REAL3), intent(out) :: out(n)
    REAL (kind=__IOTK_REAL3), intent(in)  :: in(n)
# 95 "iotk_attr.spp"
    out = in
end subroutine iotk_private_pack_REAL3

# 100 "iotk_attr.spp"
subroutine iotk_write_REAL3(val,string,ierr)
  use iotk_base
  use iotk_error_interf
  use iotk_xtox_interf
  use iotk_fmt_interf
  use iotk_str_interf
  use iotk_misc_interf
  implicit none
  REAL(kind=__IOTK_REAL3), intent(in) :: val(:)
#ifdef __IOTK_WORKAROUND6
  character(len=*)              :: string
#else
  character(len=*), intent(out) :: string
#endif
  integer, intent(out) :: ierr
# 116 "iotk_attr.spp"
  character(len=100) :: tmpval
# 118 "iotk_attr.spp"
  integer :: index,iostat
  ierr = 0
  iostat = 0 
  string(1:1) = iotk_eos
  if(size(val)==0) return
  if(len(string)==0) then
    call iotk_error_issue(ierr,"iotk_write",__FILE__,__LINE__)
# 124 "iotk_attr.spp"
call iotk_error_msg(ierr,"CVS Revision: 1.15 ")
    return
  end if
  do index=1,size(val)
# 141 "iotk_attr.spp"
    write(tmpval,trim(iotk_wfmt("REAL",kind(val),1,-1," ")),iostat=iostat) val(index)
    if(iostat/=0) then
      call iotk_error_issue(ierr,"iotk_write",__FILE__,__LINE__)
# 143 "iotk_attr.spp"
call iotk_error_msg(ierr,"CVS Revision: 1.15 ")
# 143 "iotk_attr.spp"
call iotk_error_msg(ierr,' ')
# 143 "iotk_attr.spp"
call iotk_error_write(ierr,"iostat",iostat)
      return
    end if
    call iotk_strcat(string,trim(adjustl(tmpval))//" ",ierr)
    if(ierr/=0) then
      call iotk_error_issue(ierr,"iotk_write",__FILE__,__LINE__)
# 148 "iotk_attr.spp"
call iotk_error_msg(ierr,"CVS Revision: 1.15 ")
      return
    end if
# 152 "iotk_attr.spp"
  end do
! taglio l'ultimo spazio
  string(iotk_strlen(string):iotk_strlen(string)) = iotk_eos
end subroutine iotk_write_REAL3
# 158 "iotk_attr.spp"

# 162 "iotk_attr.spp"
subroutine iotk_read_REAL3(val,string,index,ierr)
  use iotk_base
  use iotk_error_interf
  use iotk_xtox_interf
  use iotk_misc_interf
  implicit none
  REAL(kind=__IOTK_REAL3), intent(inout) :: val(:)
  character(len=*), intent(in) :: string
  integer, intent(inout) :: index
  integer, intent(out) :: ierr
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
! PER ORA CONSIDERA LE VIRGOLE COME SPAZII
  do
    pos = verify(string(pos1+1:)," ,")+pos1
    if(pos==pos1) exit
    pos = pos - 1
    pos1 = scan(string(pos+1:)," ,")+pos
    if(pos1==pos) pos1 = len(string) + 1
!LEGGI string(pos+1:pos1-1)
    index = index+1
    if(index>maxindex) then
      call iotk_error_issue(ierr,"iotk_read",__FILE__,__LINE__)
# 200 "iotk_attr.spp"
call iotk_error_msg(ierr,"CVS Revision: 1.15 ")
# 200 "iotk_attr.spp"
call iotk_error_msg(ierr,'Too many data')
    end if
# 207 "iotk_attr.spp"
    read(string(pos+1:pos1-1),"(G100.95)",iostat=iostat) val(index)
# 223 "iotk_attr.spp"
    if(iostat/=0) then
      call iotk_error_issue(ierr,"iotk_read",__FILE__,__LINE__)
# 224 "iotk_attr.spp"
call iotk_error_msg(ierr,"CVS Revision: 1.15 ")
# 224 "iotk_attr.spp"
call iotk_error_msg(ierr,'Error reading from string')
# 224 "iotk_attr.spp"
call iotk_error_write(ierr,"string",string(pos+1:pos1-1))
# 224 "iotk_attr.spp"
call iotk_error_write(ierr,"iostat",iostat)
      return
    end if
# 228 "iotk_attr.spp"
    if(pos1>=len(string)) exit
  end do
end subroutine iotk_read_REAL3
# 233 "iotk_attr.spp"

# 236 "iotk_attr.spp"
subroutine iotk_write_attr_REAL3_0(attr,name,val,dummy,first,ierr)
  use iotk_base
  use iotk_error_interf
  use iotk_attr_interf
  use iotk_str_interf
  use iotk_misc_interf
  implicit none
  character(*), intent(inout) :: attr
  character(*), intent(in)    :: name
  REAL(kind=__IOTK_REAL3), intent(in)  :: val 
  type(iotk_dummytype), optional :: dummy
  logical, optional, intent(in)  :: first
  integer, optional, intent(out) :: ierr
  integer :: ierrl
  integer :: attlen
  integer :: vallen
  integer :: namlen
  character :: delim
# 257 "iotk_attr.spp"
  character(iotk_vallenx) :: tmpval
  ierrl = 0
  if(present(first)) then
    if(first) attr(1:1) = iotk_eos
  end if
  attlen = iotk_strlen_trim(attr)
  namlen = iotk_strlen_trim(name)
  if(.not.iotk_check_name(name)) then
    call iotk_error_issue(ierrl,"iotk_write_attr",__FILE__,__LINE__)
# 265 "iotk_attr.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.15 ")
# 265 "iotk_attr.spp"
call iotk_error_msg(ierrl,'Wrong tag name')
# 265 "iotk_attr.spp"
call iotk_error_write(ierrl,"name",name(1:namlen))
    goto 1
  end if
# 282 "iotk_attr.spp"
  delim = '"'
# 284 "iotk_attr.spp"
  call iotk_write((/val/),tmpval,ierrl)
# 288 "iotk_attr.spp"
  if(ierrl/=0) then
    call iotk_error_issue(ierrl,"iotk_write_attr",__FILE__,__LINE__)
# 289 "iotk_attr.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.15 ")
    goto 1
  end if
# 293 "iotk_attr.spp"
  vallen = iotk_strlen(tmpval)
  if(attlen+vallen+namlen+5>len(attr)) then
    call iotk_error_issue(ierrl,"iotk_write_attr",__FILE__,__LINE__)
# 295 "iotk_attr.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.15 ")
# 295 "iotk_attr.spp"
call iotk_error_msg(ierrl,'Attribute dummy argument is too short')
    goto 1
  end if
  attr(attlen+1:attlen+vallen+namlen+5) = " "//name(1:namlen)//"="//delim//tmpval(1:vallen)//delim//iotk_eos
1 continue
  if(present(ierr)) then
    ierr = ierrl
  else
    if(ierrl/=0) call iotk_error_handler(ierrl)
  end if
end subroutine iotk_write_attr_REAL3_0

# 308 "iotk_attr.spp"
subroutine iotk_scan_attr_REAL3_0(attr,name,val,dummy,found,default,eos,ierr)
  use iotk_base
  use iotk_error_interf
  use iotk_attr_interf, only : iotk_read
  use iotk_str_interf
  use iotk_misc_interf
  implicit none
  character(*),             intent(in)  :: attr
  character(*),             intent(in)  :: name
#ifdef __IOTK_WORKAROUND6
  REAL(kind=__IOTK_REAL3)                        :: val 
#else
  REAL(kind=__IOTK_REAL3), intent(out)           :: val 
#endif
  type(iotk_dummytype), optional :: dummy
  logical,        optional, intent(out) :: found
  REAL(kind=__IOTK_REAL3), optional, intent(in)  :: default 
  logical,        optional, intent(in)  :: eos
  integer,        optional, intent(out) :: ierr
  integer :: ierrl
  integer :: attlen,pos,equal,namlen
  character :: delim
  logical :: foundl
  character(iotk_vallenx) :: valc
# 337 "iotk_attr.spp"
  integer :: index
  REAL(kind=__IOTK_REAL3), allocatable :: tmpval (:)
# 340 "iotk_attr.spp"
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
# 351 "iotk_attr.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.15 ")
# 351 "iotk_attr.spp"
call iotk_error_msg(ierrl,'')
# 351 "iotk_attr.spp"
call iotk_error_write(ierrl,"attr",attr(equal+1:attlen))
      goto 1
    end if
    equal = equal + pos
    if(trim(attr(equal-pos:equal-1))==name(1:namlen)) foundl = .true.
    pos = verify(attr(equal+1:attlen)," ")
    if(pos<=0) then
      call iotk_error_issue(ierrl,"iotk_scan_attr",__FILE__,__LINE__)
# 358 "iotk_attr.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.15 ")
      goto 1
    end if
    equal = equal + pos
    delim = attr(equal:equal)
    if(delim/="'" .and. delim/='"') then
      call iotk_error_issue(ierrl,"iotk_scan_attr",__FILE__,__LINE__)
# 364 "iotk_attr.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.15 ")
      goto 1
    end if
    pos = scan(attr(equal+1:attlen),delim)
    if(pos<=0) then
      call iotk_error_issue(ierrl,"iotk_scan_attr",__FILE__,__LINE__)
# 369 "iotk_attr.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.15 ")
      goto 1
    end if
    if(foundl) exit
    equal = equal + pos
  end do
  if(foundl) then
    call iotk_strcpy(valc,attr(equal+1:equal+pos-1),ierrl)
    if(ierrl/=0) then
      call iotk_error_issue(ierrl,"iotk_scan_attr",__FILE__,__LINE__)
# 378 "iotk_attr.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.15 ")
      goto 1
    end if
  else
    goto 1
  end if
# 401 "iotk_attr.spp"
  allocate(tmpval(1))
  index = 0
  call iotk_str_clean(valc)
  call iotk_read(tmpval,valc(1:iotk_strlen(valc)),index,ierrl)
  if(ierrl/=0) then
    call iotk_error_issue(ierrl,"iotk_scan_attr",__FILE__,__LINE__)
# 406 "iotk_attr.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.15 ")
    goto 1
  end if
# 412 "iotk_attr.spp"
  if(index/=1) then
# 414 "iotk_attr.spp"
    call iotk_error_issue(ierrl,"iotk_scan_attr",__FILE__,__LINE__)
# 414 "iotk_attr.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.15 ")
# 414 "iotk_attr.spp"
call iotk_error_msg(ierrl,'Attribute size does not match')
# 414 "iotk_attr.spp"
call iotk_error_write(ierrl,"attr",valc)
# 414 "iotk_attr.spp"
call iotk_error_write(ierrl,"size",size(tmpval))
    goto 1
  end if
# 418 "iotk_attr.spp"
  val = tmpval(1)
# 422 "iotk_attr.spp"
  deallocate(tmpval)
# 424 "iotk_attr.spp"
1 continue
  if(ierrl/=0) foundl=.false.
  if(present(found)) found = foundl
  if(ierrl==0 .and. .not. present(found) .and. .not. present(default) .and. .not. foundl) then
    call iotk_error_issue(ierrl,"iotk_scan_attr",__FILE__,__LINE__)
# 428 "iotk_attr.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.15 ")
# 428 "iotk_attr.spp"
call iotk_error_msg(ierrl,'Attribute not found')
# 428 "iotk_attr.spp"
call iotk_error_write(ierrl,"name",name)
    ierrl = - ierrl
  end if
  if(present(default) .and. .not. foundl) then
# 441 "iotk_attr.spp"
    val = default
# 443 "iotk_attr.spp"
  end if
  if(present(ierr)) then
    ierr = ierrl
  else
    if(ierrl/=0) call iotk_error_handler(ierrl)
  end if
end subroutine iotk_scan_attr_REAL3_0
# 451 "iotk_attr.spp"

#endif
#endif

subroutine iotk_attr_dummy_REAL3_0
  write(0,*)
end subroutine iotk_attr_dummy_REAL3_0

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

#ifdef __IOTK_REAL3
#if 1 <= __IOTK_MAXRANK

# 158 "iotk_attr.spp"

# 233 "iotk_attr.spp"

# 236 "iotk_attr.spp"
subroutine iotk_write_attr_REAL3_1(attr,name,val,dummy,first,ierr)
  use iotk_base
  use iotk_error_interf
  use iotk_attr_interf
  use iotk_str_interf
  use iotk_misc_interf
  implicit none
  character(*), intent(inout) :: attr
  character(*), intent(in)    :: name
  REAL(kind=__IOTK_REAL3), intent(in)  :: val (:)
  type(iotk_dummytype), optional :: dummy
  logical, optional, intent(in)  :: first
  integer, optional, intent(out) :: ierr
  integer :: ierrl
  integer :: attlen
  integer :: vallen
  integer :: namlen
  character :: delim
# 257 "iotk_attr.spp"
  character(iotk_vallenx) :: tmpval
  ierrl = 0
  if(present(first)) then
    if(first) attr(1:1) = iotk_eos
  end if
  attlen = iotk_strlen_trim(attr)
  namlen = iotk_strlen_trim(name)
  if(.not.iotk_check_name(name)) then
    call iotk_error_issue(ierrl,"iotk_write_attr",__FILE__,__LINE__)
# 265 "iotk_attr.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.15 ")
# 265 "iotk_attr.spp"
call iotk_error_msg(ierrl,'Wrong tag name')
# 265 "iotk_attr.spp"
call iotk_error_write(ierrl,"name",name(1:namlen))
    goto 1
  end if
# 282 "iotk_attr.spp"
  delim = '"'
# 286 "iotk_attr.spp"
  call iotk_write(pack(val,mask=.true.),tmpval,ierrl)
# 288 "iotk_attr.spp"
  if(ierrl/=0) then
    call iotk_error_issue(ierrl,"iotk_write_attr",__FILE__,__LINE__)
# 289 "iotk_attr.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.15 ")
    goto 1
  end if
# 293 "iotk_attr.spp"
  vallen = iotk_strlen(tmpval)
  if(attlen+vallen+namlen+5>len(attr)) then
    call iotk_error_issue(ierrl,"iotk_write_attr",__FILE__,__LINE__)
# 295 "iotk_attr.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.15 ")
# 295 "iotk_attr.spp"
call iotk_error_msg(ierrl,'Attribute dummy argument is too short')
    goto 1
  end if
  attr(attlen+1:attlen+vallen+namlen+5) = " "//name(1:namlen)//"="//delim//tmpval(1:vallen)//delim//iotk_eos
1 continue
  if(present(ierr)) then
    ierr = ierrl
  else
    if(ierrl/=0) call iotk_error_handler(ierrl)
  end if
end subroutine iotk_write_attr_REAL3_1

# 308 "iotk_attr.spp"
subroutine iotk_scan_attr_REAL3_1(attr,name,val,dummy,found,default,eos,ierr)
  use iotk_base
  use iotk_error_interf
  use iotk_attr_interf, only : iotk_read
  use iotk_str_interf
  use iotk_misc_interf
  implicit none
  character(*),             intent(in)  :: attr
  character(*),             intent(in)  :: name
#ifdef __IOTK_WORKAROUND6
  REAL(kind=__IOTK_REAL3)                        :: val (:)
#else
  REAL(kind=__IOTK_REAL3), intent(out)           :: val (:)
#endif
  type(iotk_dummytype), optional :: dummy
  logical,        optional, intent(out) :: found
  REAL(kind=__IOTK_REAL3), optional, intent(in)  :: default (:)
  logical,        optional, intent(in)  :: eos
  integer,        optional, intent(out) :: ierr
  integer :: ierrl
  integer :: attlen,pos,equal,namlen
  character :: delim
  logical :: foundl
  character(iotk_vallenx) :: valc
# 337 "iotk_attr.spp"
  integer :: index
  REAL(kind=__IOTK_REAL3), allocatable :: tmpval (:)
# 340 "iotk_attr.spp"
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
# 351 "iotk_attr.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.15 ")
# 351 "iotk_attr.spp"
call iotk_error_msg(ierrl,'')
# 351 "iotk_attr.spp"
call iotk_error_write(ierrl,"attr",attr(equal+1:attlen))
      goto 1
    end if
    equal = equal + pos
    if(trim(attr(equal-pos:equal-1))==name(1:namlen)) foundl = .true.
    pos = verify(attr(equal+1:attlen)," ")
    if(pos<=0) then
      call iotk_error_issue(ierrl,"iotk_scan_attr",__FILE__,__LINE__)
# 358 "iotk_attr.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.15 ")
      goto 1
    end if
    equal = equal + pos
    delim = attr(equal:equal)
    if(delim/="'" .and. delim/='"') then
      call iotk_error_issue(ierrl,"iotk_scan_attr",__FILE__,__LINE__)
# 364 "iotk_attr.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.15 ")
      goto 1
    end if
    pos = scan(attr(equal+1:attlen),delim)
    if(pos<=0) then
      call iotk_error_issue(ierrl,"iotk_scan_attr",__FILE__,__LINE__)
# 369 "iotk_attr.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.15 ")
      goto 1
    end if
    if(foundl) exit
    equal = equal + pos
  end do
  if(foundl) then
    call iotk_strcpy(valc,attr(equal+1:equal+pos-1),ierrl)
    if(ierrl/=0) then
      call iotk_error_issue(ierrl,"iotk_scan_attr",__FILE__,__LINE__)
# 378 "iotk_attr.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.15 ")
      goto 1
    end if
  else
    goto 1
  end if
# 401 "iotk_attr.spp"
  allocate(tmpval(size(val)))
  index = 0
  call iotk_str_clean(valc)
  call iotk_read(tmpval,valc(1:iotk_strlen(valc)),index,ierrl)
  if(ierrl/=0) then
    call iotk_error_issue(ierrl,"iotk_scan_attr",__FILE__,__LINE__)
# 406 "iotk_attr.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.15 ")
    goto 1
  end if
# 412 "iotk_attr.spp"
  if(index/=size(val)) then
# 414 "iotk_attr.spp"
    call iotk_error_issue(ierrl,"iotk_scan_attr",__FILE__,__LINE__)
# 414 "iotk_attr.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.15 ")
# 414 "iotk_attr.spp"
call iotk_error_msg(ierrl,'Attribute size does not match')
# 414 "iotk_attr.spp"
call iotk_error_write(ierrl,"attr",valc)
# 414 "iotk_attr.spp"
call iotk_error_write(ierrl,"size",size(tmpval))
    goto 1
  end if
# 420 "iotk_attr.spp"
  val = reshape (source=tmpval,shape=shape(val))
# 422 "iotk_attr.spp"
  deallocate(tmpval)
# 424 "iotk_attr.spp"
1 continue
  if(ierrl/=0) foundl=.false.
  if(present(found)) found = foundl
  if(ierrl==0 .and. .not. present(found) .and. .not. present(default) .and. .not. foundl) then
    call iotk_error_issue(ierrl,"iotk_scan_attr",__FILE__,__LINE__)
# 428 "iotk_attr.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.15 ")
# 428 "iotk_attr.spp"
call iotk_error_msg(ierrl,'Attribute not found')
# 428 "iotk_attr.spp"
call iotk_error_write(ierrl,"name",name)
    ierrl = - ierrl
  end if
  if(present(default) .and. .not. foundl) then
# 441 "iotk_attr.spp"
    val = default
# 443 "iotk_attr.spp"
  end if
  if(present(ierr)) then
    ierr = ierrl
  else
    if(ierrl/=0) call iotk_error_handler(ierrl)
  end if
end subroutine iotk_scan_attr_REAL3_1
# 451 "iotk_attr.spp"

#endif
#endif

subroutine iotk_attr_dummy_REAL3_1
  write(0,*)
end subroutine iotk_attr_dummy_REAL3_1

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

#ifdef __IOTK_REAL3
#if 2 <= __IOTK_MAXRANK

# 158 "iotk_attr.spp"

# 233 "iotk_attr.spp"

# 236 "iotk_attr.spp"
subroutine iotk_write_attr_REAL3_2(attr,name,val,dummy,first,ierr)
  use iotk_base
  use iotk_error_interf
  use iotk_attr_interf
  use iotk_str_interf
  use iotk_misc_interf
  implicit none
  character(*), intent(inout) :: attr
  character(*), intent(in)    :: name
  REAL(kind=__IOTK_REAL3), intent(in)  :: val (:,:)
  type(iotk_dummytype), optional :: dummy
  logical, optional, intent(in)  :: first
  integer, optional, intent(out) :: ierr
  integer :: ierrl
  integer :: attlen
  integer :: vallen
  integer :: namlen
  character :: delim
# 257 "iotk_attr.spp"
  character(iotk_vallenx) :: tmpval
  ierrl = 0
  if(present(first)) then
    if(first) attr(1:1) = iotk_eos
  end if
  attlen = iotk_strlen_trim(attr)
  namlen = iotk_strlen_trim(name)
  if(.not.iotk_check_name(name)) then
    call iotk_error_issue(ierrl,"iotk_write_attr",__FILE__,__LINE__)
# 265 "iotk_attr.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.15 ")
# 265 "iotk_attr.spp"
call iotk_error_msg(ierrl,'Wrong tag name')
# 265 "iotk_attr.spp"
call iotk_error_write(ierrl,"name",name(1:namlen))
    goto 1
  end if
# 282 "iotk_attr.spp"
  delim = '"'
# 286 "iotk_attr.spp"
  call iotk_write(pack(val,mask=.true.),tmpval,ierrl)
# 288 "iotk_attr.spp"
  if(ierrl/=0) then
    call iotk_error_issue(ierrl,"iotk_write_attr",__FILE__,__LINE__)
# 289 "iotk_attr.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.15 ")
    goto 1
  end if
# 293 "iotk_attr.spp"
  vallen = iotk_strlen(tmpval)
  if(attlen+vallen+namlen+5>len(attr)) then
    call iotk_error_issue(ierrl,"iotk_write_attr",__FILE__,__LINE__)
# 295 "iotk_attr.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.15 ")
# 295 "iotk_attr.spp"
call iotk_error_msg(ierrl,'Attribute dummy argument is too short')
    goto 1
  end if
  attr(attlen+1:attlen+vallen+namlen+5) = " "//name(1:namlen)//"="//delim//tmpval(1:vallen)//delim//iotk_eos
1 continue
  if(present(ierr)) then
    ierr = ierrl
  else
    if(ierrl/=0) call iotk_error_handler(ierrl)
  end if
end subroutine iotk_write_attr_REAL3_2

# 308 "iotk_attr.spp"
subroutine iotk_scan_attr_REAL3_2(attr,name,val,dummy,found,default,eos,ierr)
  use iotk_base
  use iotk_error_interf
  use iotk_attr_interf, only : iotk_read
  use iotk_str_interf
  use iotk_misc_interf
  implicit none
  character(*),             intent(in)  :: attr
  character(*),             intent(in)  :: name
#ifdef __IOTK_WORKAROUND6
  REAL(kind=__IOTK_REAL3)                        :: val (:,:)
#else
  REAL(kind=__IOTK_REAL3), intent(out)           :: val (:,:)
#endif
  type(iotk_dummytype), optional :: dummy
  logical,        optional, intent(out) :: found
  REAL(kind=__IOTK_REAL3), optional, intent(in)  :: default (:,:)
  logical,        optional, intent(in)  :: eos
  integer,        optional, intent(out) :: ierr
  integer :: ierrl
  integer :: attlen,pos,equal,namlen
  character :: delim
  logical :: foundl
  character(iotk_vallenx) :: valc
# 337 "iotk_attr.spp"
  integer :: index
  REAL(kind=__IOTK_REAL3), allocatable :: tmpval (:)
# 340 "iotk_attr.spp"
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
# 351 "iotk_attr.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.15 ")
# 351 "iotk_attr.spp"
call iotk_error_msg(ierrl,'')
# 351 "iotk_attr.spp"
call iotk_error_write(ierrl,"attr",attr(equal+1:attlen))
      goto 1
    end if
    equal = equal + pos
    if(trim(attr(equal-pos:equal-1))==name(1:namlen)) foundl = .true.
    pos = verify(attr(equal+1:attlen)," ")
    if(pos<=0) then
      call iotk_error_issue(ierrl,"iotk_scan_attr",__FILE__,__LINE__)
# 358 "iotk_attr.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.15 ")
      goto 1
    end if
    equal = equal + pos
    delim = attr(equal:equal)
    if(delim/="'" .and. delim/='"') then
      call iotk_error_issue(ierrl,"iotk_scan_attr",__FILE__,__LINE__)
# 364 "iotk_attr.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.15 ")
      goto 1
    end if
    pos = scan(attr(equal+1:attlen),delim)
    if(pos<=0) then
      call iotk_error_issue(ierrl,"iotk_scan_attr",__FILE__,__LINE__)
# 369 "iotk_attr.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.15 ")
      goto 1
    end if
    if(foundl) exit
    equal = equal + pos
  end do
  if(foundl) then
    call iotk_strcpy(valc,attr(equal+1:equal+pos-1),ierrl)
    if(ierrl/=0) then
      call iotk_error_issue(ierrl,"iotk_scan_attr",__FILE__,__LINE__)
# 378 "iotk_attr.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.15 ")
      goto 1
    end if
  else
    goto 1
  end if
# 401 "iotk_attr.spp"
  allocate(tmpval(size(val)))
  index = 0
  call iotk_str_clean(valc)
  call iotk_read(tmpval,valc(1:iotk_strlen(valc)),index,ierrl)
  if(ierrl/=0) then
    call iotk_error_issue(ierrl,"iotk_scan_attr",__FILE__,__LINE__)
# 406 "iotk_attr.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.15 ")
    goto 1
  end if
# 412 "iotk_attr.spp"
  if(index/=size(val)) then
# 414 "iotk_attr.spp"
    call iotk_error_issue(ierrl,"iotk_scan_attr",__FILE__,__LINE__)
# 414 "iotk_attr.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.15 ")
# 414 "iotk_attr.spp"
call iotk_error_msg(ierrl,'Attribute size does not match')
# 414 "iotk_attr.spp"
call iotk_error_write(ierrl,"attr",valc)
# 414 "iotk_attr.spp"
call iotk_error_write(ierrl,"size",size(tmpval))
    goto 1
  end if
# 420 "iotk_attr.spp"
  val = reshape (source=tmpval,shape=shape(val))
# 422 "iotk_attr.spp"
  deallocate(tmpval)
# 424 "iotk_attr.spp"
1 continue
  if(ierrl/=0) foundl=.false.
  if(present(found)) found = foundl
  if(ierrl==0 .and. .not. present(found) .and. .not. present(default) .and. .not. foundl) then
    call iotk_error_issue(ierrl,"iotk_scan_attr",__FILE__,__LINE__)
# 428 "iotk_attr.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.15 ")
# 428 "iotk_attr.spp"
call iotk_error_msg(ierrl,'Attribute not found')
# 428 "iotk_attr.spp"
call iotk_error_write(ierrl,"name",name)
    ierrl = - ierrl
  end if
  if(present(default) .and. .not. foundl) then
# 441 "iotk_attr.spp"
    val = default
# 443 "iotk_attr.spp"
  end if
  if(present(ierr)) then
    ierr = ierrl
  else
    if(ierrl/=0) call iotk_error_handler(ierrl)
  end if
end subroutine iotk_scan_attr_REAL3_2
# 451 "iotk_attr.spp"

#endif
#endif

subroutine iotk_attr_dummy_REAL3_2
  write(0,*)
end subroutine iotk_attr_dummy_REAL3_2

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

#ifdef __IOTK_REAL3
#if 3 <= __IOTK_MAXRANK

# 158 "iotk_attr.spp"

# 233 "iotk_attr.spp"

# 236 "iotk_attr.spp"
subroutine iotk_write_attr_REAL3_3(attr,name,val,dummy,first,ierr)
  use iotk_base
  use iotk_error_interf
  use iotk_attr_interf
  use iotk_str_interf
  use iotk_misc_interf
  implicit none
  character(*), intent(inout) :: attr
  character(*), intent(in)    :: name
  REAL(kind=__IOTK_REAL3), intent(in)  :: val (:,:,:)
  type(iotk_dummytype), optional :: dummy
  logical, optional, intent(in)  :: first
  integer, optional, intent(out) :: ierr
  integer :: ierrl
  integer :: attlen
  integer :: vallen
  integer :: namlen
  character :: delim
# 257 "iotk_attr.spp"
  character(iotk_vallenx) :: tmpval
  ierrl = 0
  if(present(first)) then
    if(first) attr(1:1) = iotk_eos
  end if
  attlen = iotk_strlen_trim(attr)
  namlen = iotk_strlen_trim(name)
  if(.not.iotk_check_name(name)) then
    call iotk_error_issue(ierrl,"iotk_write_attr",__FILE__,__LINE__)
# 265 "iotk_attr.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.15 ")
# 265 "iotk_attr.spp"
call iotk_error_msg(ierrl,'Wrong tag name')
# 265 "iotk_attr.spp"
call iotk_error_write(ierrl,"name",name(1:namlen))
    goto 1
  end if
# 282 "iotk_attr.spp"
  delim = '"'
# 286 "iotk_attr.spp"
  call iotk_write(pack(val,mask=.true.),tmpval,ierrl)
# 288 "iotk_attr.spp"
  if(ierrl/=0) then
    call iotk_error_issue(ierrl,"iotk_write_attr",__FILE__,__LINE__)
# 289 "iotk_attr.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.15 ")
    goto 1
  end if
# 293 "iotk_attr.spp"
  vallen = iotk_strlen(tmpval)
  if(attlen+vallen+namlen+5>len(attr)) then
    call iotk_error_issue(ierrl,"iotk_write_attr",__FILE__,__LINE__)
# 295 "iotk_attr.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.15 ")
# 295 "iotk_attr.spp"
call iotk_error_msg(ierrl,'Attribute dummy argument is too short')
    goto 1
  end if
  attr(attlen+1:attlen+vallen+namlen+5) = " "//name(1:namlen)//"="//delim//tmpval(1:vallen)//delim//iotk_eos
1 continue
  if(present(ierr)) then
    ierr = ierrl
  else
    if(ierrl/=0) call iotk_error_handler(ierrl)
  end if
end subroutine iotk_write_attr_REAL3_3

# 308 "iotk_attr.spp"
subroutine iotk_scan_attr_REAL3_3(attr,name,val,dummy,found,default,eos,ierr)
  use iotk_base
  use iotk_error_interf
  use iotk_attr_interf, only : iotk_read
  use iotk_str_interf
  use iotk_misc_interf
  implicit none
  character(*),             intent(in)  :: attr
  character(*),             intent(in)  :: name
#ifdef __IOTK_WORKAROUND6
  REAL(kind=__IOTK_REAL3)                        :: val (:,:,:)
#else
  REAL(kind=__IOTK_REAL3), intent(out)           :: val (:,:,:)
#endif
  type(iotk_dummytype), optional :: dummy
  logical,        optional, intent(out) :: found
  REAL(kind=__IOTK_REAL3), optional, intent(in)  :: default (:,:,:)
  logical,        optional, intent(in)  :: eos
  integer,        optional, intent(out) :: ierr
  integer :: ierrl
  integer :: attlen,pos,equal,namlen
  character :: delim
  logical :: foundl
  character(iotk_vallenx) :: valc
# 337 "iotk_attr.spp"
  integer :: index
  REAL(kind=__IOTK_REAL3), allocatable :: tmpval (:)
# 340 "iotk_attr.spp"
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
# 351 "iotk_attr.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.15 ")
# 351 "iotk_attr.spp"
call iotk_error_msg(ierrl,'')
# 351 "iotk_attr.spp"
call iotk_error_write(ierrl,"attr",attr(equal+1:attlen))
      goto 1
    end if
    equal = equal + pos
    if(trim(attr(equal-pos:equal-1))==name(1:namlen)) foundl = .true.
    pos = verify(attr(equal+1:attlen)," ")
    if(pos<=0) then
      call iotk_error_issue(ierrl,"iotk_scan_attr",__FILE__,__LINE__)
# 358 "iotk_attr.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.15 ")
      goto 1
    end if
    equal = equal + pos
    delim = attr(equal:equal)
    if(delim/="'" .and. delim/='"') then
      call iotk_error_issue(ierrl,"iotk_scan_attr",__FILE__,__LINE__)
# 364 "iotk_attr.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.15 ")
      goto 1
    end if
    pos = scan(attr(equal+1:attlen),delim)
    if(pos<=0) then
      call iotk_error_issue(ierrl,"iotk_scan_attr",__FILE__,__LINE__)
# 369 "iotk_attr.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.15 ")
      goto 1
    end if
    if(foundl) exit
    equal = equal + pos
  end do
  if(foundl) then
    call iotk_strcpy(valc,attr(equal+1:equal+pos-1),ierrl)
    if(ierrl/=0) then
      call iotk_error_issue(ierrl,"iotk_scan_attr",__FILE__,__LINE__)
# 378 "iotk_attr.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.15 ")
      goto 1
    end if
  else
    goto 1
  end if
# 401 "iotk_attr.spp"
  allocate(tmpval(size(val)))
  index = 0
  call iotk_str_clean(valc)
  call iotk_read(tmpval,valc(1:iotk_strlen(valc)),index,ierrl)
  if(ierrl/=0) then
    call iotk_error_issue(ierrl,"iotk_scan_attr",__FILE__,__LINE__)
# 406 "iotk_attr.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.15 ")
    goto 1
  end if
# 412 "iotk_attr.spp"
  if(index/=size(val)) then
# 414 "iotk_attr.spp"
    call iotk_error_issue(ierrl,"iotk_scan_attr",__FILE__,__LINE__)
# 414 "iotk_attr.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.15 ")
# 414 "iotk_attr.spp"
call iotk_error_msg(ierrl,'Attribute size does not match')
# 414 "iotk_attr.spp"
call iotk_error_write(ierrl,"attr",valc)
# 414 "iotk_attr.spp"
call iotk_error_write(ierrl,"size",size(tmpval))
    goto 1
  end if
# 420 "iotk_attr.spp"
  val = reshape (source=tmpval,shape=shape(val))
# 422 "iotk_attr.spp"
  deallocate(tmpval)
# 424 "iotk_attr.spp"
1 continue
  if(ierrl/=0) foundl=.false.
  if(present(found)) found = foundl
  if(ierrl==0 .and. .not. present(found) .and. .not. present(default) .and. .not. foundl) then
    call iotk_error_issue(ierrl,"iotk_scan_attr",__FILE__,__LINE__)
# 428 "iotk_attr.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.15 ")
# 428 "iotk_attr.spp"
call iotk_error_msg(ierrl,'Attribute not found')
# 428 "iotk_attr.spp"
call iotk_error_write(ierrl,"name",name)
    ierrl = - ierrl
  end if
  if(present(default) .and. .not. foundl) then
# 441 "iotk_attr.spp"
    val = default
# 443 "iotk_attr.spp"
  end if
  if(present(ierr)) then
    ierr = ierrl
  else
    if(ierrl/=0) call iotk_error_handler(ierrl)
  end if
end subroutine iotk_scan_attr_REAL3_3
# 451 "iotk_attr.spp"

#endif
#endif

subroutine iotk_attr_dummy_REAL3_3
  write(0,*)
end subroutine iotk_attr_dummy_REAL3_3

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

#ifdef __IOTK_REAL3
#if 4 <= __IOTK_MAXRANK

# 158 "iotk_attr.spp"

# 233 "iotk_attr.spp"

# 236 "iotk_attr.spp"
subroutine iotk_write_attr_REAL3_4(attr,name,val,dummy,first,ierr)
  use iotk_base
  use iotk_error_interf
  use iotk_attr_interf
  use iotk_str_interf
  use iotk_misc_interf
  implicit none
  character(*), intent(inout) :: attr
  character(*), intent(in)    :: name
  REAL(kind=__IOTK_REAL3), intent(in)  :: val (:,:,:,:)
  type(iotk_dummytype), optional :: dummy
  logical, optional, intent(in)  :: first
  integer, optional, intent(out) :: ierr
  integer :: ierrl
  integer :: attlen
  integer :: vallen
  integer :: namlen
  character :: delim
# 257 "iotk_attr.spp"
  character(iotk_vallenx) :: tmpval
  ierrl = 0
  if(present(first)) then
    if(first) attr(1:1) = iotk_eos
  end if
  attlen = iotk_strlen_trim(attr)
  namlen = iotk_strlen_trim(name)
  if(.not.iotk_check_name(name)) then
    call iotk_error_issue(ierrl,"iotk_write_attr",__FILE__,__LINE__)
# 265 "iotk_attr.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.15 ")
# 265 "iotk_attr.spp"
call iotk_error_msg(ierrl,'Wrong tag name')
# 265 "iotk_attr.spp"
call iotk_error_write(ierrl,"name",name(1:namlen))
    goto 1
  end if
# 282 "iotk_attr.spp"
  delim = '"'
# 286 "iotk_attr.spp"
  call iotk_write(pack(val,mask=.true.),tmpval,ierrl)
# 288 "iotk_attr.spp"
  if(ierrl/=0) then
    call iotk_error_issue(ierrl,"iotk_write_attr",__FILE__,__LINE__)
# 289 "iotk_attr.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.15 ")
    goto 1
  end if
# 293 "iotk_attr.spp"
  vallen = iotk_strlen(tmpval)
  if(attlen+vallen+namlen+5>len(attr)) then
    call iotk_error_issue(ierrl,"iotk_write_attr",__FILE__,__LINE__)
# 295 "iotk_attr.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.15 ")
# 295 "iotk_attr.spp"
call iotk_error_msg(ierrl,'Attribute dummy argument is too short')
    goto 1
  end if
  attr(attlen+1:attlen+vallen+namlen+5) = " "//name(1:namlen)//"="//delim//tmpval(1:vallen)//delim//iotk_eos
1 continue
  if(present(ierr)) then
    ierr = ierrl
  else
    if(ierrl/=0) call iotk_error_handler(ierrl)
  end if
end subroutine iotk_write_attr_REAL3_4

# 308 "iotk_attr.spp"
subroutine iotk_scan_attr_REAL3_4(attr,name,val,dummy,found,default,eos,ierr)
  use iotk_base
  use iotk_error_interf
  use iotk_attr_interf, only : iotk_read
  use iotk_str_interf
  use iotk_misc_interf
  implicit none
  character(*),             intent(in)  :: attr
  character(*),             intent(in)  :: name
#ifdef __IOTK_WORKAROUND6
  REAL(kind=__IOTK_REAL3)                        :: val (:,:,:,:)
#else
  REAL(kind=__IOTK_REAL3), intent(out)           :: val (:,:,:,:)
#endif
  type(iotk_dummytype), optional :: dummy
  logical,        optional, intent(out) :: found
  REAL(kind=__IOTK_REAL3), optional, intent(in)  :: default (:,:,:,:)
  logical,        optional, intent(in)  :: eos
  integer,        optional, intent(out) :: ierr
  integer :: ierrl
  integer :: attlen,pos,equal,namlen
  character :: delim
  logical :: foundl
  character(iotk_vallenx) :: valc
# 337 "iotk_attr.spp"
  integer :: index
  REAL(kind=__IOTK_REAL3), allocatable :: tmpval (:)
# 340 "iotk_attr.spp"
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
# 351 "iotk_attr.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.15 ")
# 351 "iotk_attr.spp"
call iotk_error_msg(ierrl,'')
# 351 "iotk_attr.spp"
call iotk_error_write(ierrl,"attr",attr(equal+1:attlen))
      goto 1
    end if
    equal = equal + pos
    if(trim(attr(equal-pos:equal-1))==name(1:namlen)) foundl = .true.
    pos = verify(attr(equal+1:attlen)," ")
    if(pos<=0) then
      call iotk_error_issue(ierrl,"iotk_scan_attr",__FILE__,__LINE__)
# 358 "iotk_attr.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.15 ")
      goto 1
    end if
    equal = equal + pos
    delim = attr(equal:equal)
    if(delim/="'" .and. delim/='"') then
      call iotk_error_issue(ierrl,"iotk_scan_attr",__FILE__,__LINE__)
# 364 "iotk_attr.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.15 ")
      goto 1
    end if
    pos = scan(attr(equal+1:attlen),delim)
    if(pos<=0) then
      call iotk_error_issue(ierrl,"iotk_scan_attr",__FILE__,__LINE__)
# 369 "iotk_attr.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.15 ")
      goto 1
    end if
    if(foundl) exit
    equal = equal + pos
  end do
  if(foundl) then
    call iotk_strcpy(valc,attr(equal+1:equal+pos-1),ierrl)
    if(ierrl/=0) then
      call iotk_error_issue(ierrl,"iotk_scan_attr",__FILE__,__LINE__)
# 378 "iotk_attr.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.15 ")
      goto 1
    end if
  else
    goto 1
  end if
# 401 "iotk_attr.spp"
  allocate(tmpval(size(val)))
  index = 0
  call iotk_str_clean(valc)
  call iotk_read(tmpval,valc(1:iotk_strlen(valc)),index,ierrl)
  if(ierrl/=0) then
    call iotk_error_issue(ierrl,"iotk_scan_attr",__FILE__,__LINE__)
# 406 "iotk_attr.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.15 ")
    goto 1
  end if
# 412 "iotk_attr.spp"
  if(index/=size(val)) then
# 414 "iotk_attr.spp"
    call iotk_error_issue(ierrl,"iotk_scan_attr",__FILE__,__LINE__)
# 414 "iotk_attr.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.15 ")
# 414 "iotk_attr.spp"
call iotk_error_msg(ierrl,'Attribute size does not match')
# 414 "iotk_attr.spp"
call iotk_error_write(ierrl,"attr",valc)
# 414 "iotk_attr.spp"
call iotk_error_write(ierrl,"size",size(tmpval))
    goto 1
  end if
# 420 "iotk_attr.spp"
  val = reshape (source=tmpval,shape=shape(val))
# 422 "iotk_attr.spp"
  deallocate(tmpval)
# 424 "iotk_attr.spp"
1 continue
  if(ierrl/=0) foundl=.false.
  if(present(found)) found = foundl
  if(ierrl==0 .and. .not. present(found) .and. .not. present(default) .and. .not. foundl) then
    call iotk_error_issue(ierrl,"iotk_scan_attr",__FILE__,__LINE__)
# 428 "iotk_attr.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.15 ")
# 428 "iotk_attr.spp"
call iotk_error_msg(ierrl,'Attribute not found')
# 428 "iotk_attr.spp"
call iotk_error_write(ierrl,"name",name)
    ierrl = - ierrl
  end if
  if(present(default) .and. .not. foundl) then
# 441 "iotk_attr.spp"
    val = default
# 443 "iotk_attr.spp"
  end if
  if(present(ierr)) then
    ierr = ierrl
  else
    if(ierrl/=0) call iotk_error_handler(ierrl)
  end if
end subroutine iotk_scan_attr_REAL3_4
# 451 "iotk_attr.spp"

#endif
#endif

subroutine iotk_attr_dummy_REAL3_4
  write(0,*)
end subroutine iotk_attr_dummy_REAL3_4

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

#ifdef __IOTK_REAL3
#if 5 <= __IOTK_MAXRANK

# 158 "iotk_attr.spp"

# 233 "iotk_attr.spp"

# 236 "iotk_attr.spp"
subroutine iotk_write_attr_REAL3_5(attr,name,val,dummy,first,ierr)
  use iotk_base
  use iotk_error_interf
  use iotk_attr_interf
  use iotk_str_interf
  use iotk_misc_interf
  implicit none
  character(*), intent(inout) :: attr
  character(*), intent(in)    :: name
  REAL(kind=__IOTK_REAL3), intent(in)  :: val (:,:,:,:,:)
  type(iotk_dummytype), optional :: dummy
  logical, optional, intent(in)  :: first
  integer, optional, intent(out) :: ierr
  integer :: ierrl
  integer :: attlen
  integer :: vallen
  integer :: namlen
  character :: delim
# 257 "iotk_attr.spp"
  character(iotk_vallenx) :: tmpval
  ierrl = 0
  if(present(first)) then
    if(first) attr(1:1) = iotk_eos
  end if
  attlen = iotk_strlen_trim(attr)
  namlen = iotk_strlen_trim(name)
  if(.not.iotk_check_name(name)) then
    call iotk_error_issue(ierrl,"iotk_write_attr",__FILE__,__LINE__)
# 265 "iotk_attr.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.15 ")
# 265 "iotk_attr.spp"
call iotk_error_msg(ierrl,'Wrong tag name')
# 265 "iotk_attr.spp"
call iotk_error_write(ierrl,"name",name(1:namlen))
    goto 1
  end if
# 282 "iotk_attr.spp"
  delim = '"'
# 286 "iotk_attr.spp"
  call iotk_write(pack(val,mask=.true.),tmpval,ierrl)
# 288 "iotk_attr.spp"
  if(ierrl/=0) then
    call iotk_error_issue(ierrl,"iotk_write_attr",__FILE__,__LINE__)
# 289 "iotk_attr.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.15 ")
    goto 1
  end if
# 293 "iotk_attr.spp"
  vallen = iotk_strlen(tmpval)
  if(attlen+vallen+namlen+5>len(attr)) then
    call iotk_error_issue(ierrl,"iotk_write_attr",__FILE__,__LINE__)
# 295 "iotk_attr.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.15 ")
# 295 "iotk_attr.spp"
call iotk_error_msg(ierrl,'Attribute dummy argument is too short')
    goto 1
  end if
  attr(attlen+1:attlen+vallen+namlen+5) = " "//name(1:namlen)//"="//delim//tmpval(1:vallen)//delim//iotk_eos
1 continue
  if(present(ierr)) then
    ierr = ierrl
  else
    if(ierrl/=0) call iotk_error_handler(ierrl)
  end if
end subroutine iotk_write_attr_REAL3_5

# 308 "iotk_attr.spp"
subroutine iotk_scan_attr_REAL3_5(attr,name,val,dummy,found,default,eos,ierr)
  use iotk_base
  use iotk_error_interf
  use iotk_attr_interf, only : iotk_read
  use iotk_str_interf
  use iotk_misc_interf
  implicit none
  character(*),             intent(in)  :: attr
  character(*),             intent(in)  :: name
#ifdef __IOTK_WORKAROUND6
  REAL(kind=__IOTK_REAL3)                        :: val (:,:,:,:,:)
#else
  REAL(kind=__IOTK_REAL3), intent(out)           :: val (:,:,:,:,:)
#endif
  type(iotk_dummytype), optional :: dummy
  logical,        optional, intent(out) :: found
  REAL(kind=__IOTK_REAL3), optional, intent(in)  :: default (:,:,:,:,:)
  logical,        optional, intent(in)  :: eos
  integer,        optional, intent(out) :: ierr
  integer :: ierrl
  integer :: attlen,pos,equal,namlen
  character :: delim
  logical :: foundl
  character(iotk_vallenx) :: valc
# 337 "iotk_attr.spp"
  integer :: index
  REAL(kind=__IOTK_REAL3), allocatable :: tmpval (:)
# 340 "iotk_attr.spp"
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
# 351 "iotk_attr.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.15 ")
# 351 "iotk_attr.spp"
call iotk_error_msg(ierrl,'')
# 351 "iotk_attr.spp"
call iotk_error_write(ierrl,"attr",attr(equal+1:attlen))
      goto 1
    end if
    equal = equal + pos
    if(trim(attr(equal-pos:equal-1))==name(1:namlen)) foundl = .true.
    pos = verify(attr(equal+1:attlen)," ")
    if(pos<=0) then
      call iotk_error_issue(ierrl,"iotk_scan_attr",__FILE__,__LINE__)
# 358 "iotk_attr.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.15 ")
      goto 1
    end if
    equal = equal + pos
    delim = attr(equal:equal)
    if(delim/="'" .and. delim/='"') then
      call iotk_error_issue(ierrl,"iotk_scan_attr",__FILE__,__LINE__)
# 364 "iotk_attr.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.15 ")
      goto 1
    end if
    pos = scan(attr(equal+1:attlen),delim)
    if(pos<=0) then
      call iotk_error_issue(ierrl,"iotk_scan_attr",__FILE__,__LINE__)
# 369 "iotk_attr.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.15 ")
      goto 1
    end if
    if(foundl) exit
    equal = equal + pos
  end do
  if(foundl) then
    call iotk_strcpy(valc,attr(equal+1:equal+pos-1),ierrl)
    if(ierrl/=0) then
      call iotk_error_issue(ierrl,"iotk_scan_attr",__FILE__,__LINE__)
# 378 "iotk_attr.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.15 ")
      goto 1
    end if
  else
    goto 1
  end if
# 401 "iotk_attr.spp"
  allocate(tmpval(size(val)))
  index = 0
  call iotk_str_clean(valc)
  call iotk_read(tmpval,valc(1:iotk_strlen(valc)),index,ierrl)
  if(ierrl/=0) then
    call iotk_error_issue(ierrl,"iotk_scan_attr",__FILE__,__LINE__)
# 406 "iotk_attr.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.15 ")
    goto 1
  end if
# 412 "iotk_attr.spp"
  if(index/=size(val)) then
# 414 "iotk_attr.spp"
    call iotk_error_issue(ierrl,"iotk_scan_attr",__FILE__,__LINE__)
# 414 "iotk_attr.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.15 ")
# 414 "iotk_attr.spp"
call iotk_error_msg(ierrl,'Attribute size does not match')
# 414 "iotk_attr.spp"
call iotk_error_write(ierrl,"attr",valc)
# 414 "iotk_attr.spp"
call iotk_error_write(ierrl,"size",size(tmpval))
    goto 1
  end if
# 420 "iotk_attr.spp"
  val = reshape (source=tmpval,shape=shape(val))
# 422 "iotk_attr.spp"
  deallocate(tmpval)
# 424 "iotk_attr.spp"
1 continue
  if(ierrl/=0) foundl=.false.
  if(present(found)) found = foundl
  if(ierrl==0 .and. .not. present(found) .and. .not. present(default) .and. .not. foundl) then
    call iotk_error_issue(ierrl,"iotk_scan_attr",__FILE__,__LINE__)
# 428 "iotk_attr.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.15 ")
# 428 "iotk_attr.spp"
call iotk_error_msg(ierrl,'Attribute not found')
# 428 "iotk_attr.spp"
call iotk_error_write(ierrl,"name",name)
    ierrl = - ierrl
  end if
  if(present(default) .and. .not. foundl) then
# 441 "iotk_attr.spp"
    val = default
# 443 "iotk_attr.spp"
  end if
  if(present(ierr)) then
    ierr = ierrl
  else
    if(ierrl/=0) call iotk_error_handler(ierrl)
  end if
end subroutine iotk_scan_attr_REAL3_5
# 451 "iotk_attr.spp"

#endif
#endif

subroutine iotk_attr_dummy_REAL3_5
  write(0,*)
end subroutine iotk_attr_dummy_REAL3_5

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

#ifdef __IOTK_REAL3
#if 6 <= __IOTK_MAXRANK

# 158 "iotk_attr.spp"

# 233 "iotk_attr.spp"

# 236 "iotk_attr.spp"
subroutine iotk_write_attr_REAL3_6(attr,name,val,dummy,first,ierr)
  use iotk_base
  use iotk_error_interf
  use iotk_attr_interf
  use iotk_str_interf
  use iotk_misc_interf
  implicit none
  character(*), intent(inout) :: attr
  character(*), intent(in)    :: name
  REAL(kind=__IOTK_REAL3), intent(in)  :: val (:,:,:,:,:,:)
  type(iotk_dummytype), optional :: dummy
  logical, optional, intent(in)  :: first
  integer, optional, intent(out) :: ierr
  integer :: ierrl
  integer :: attlen
  integer :: vallen
  integer :: namlen
  character :: delim
# 257 "iotk_attr.spp"
  character(iotk_vallenx) :: tmpval
  ierrl = 0
  if(present(first)) then
    if(first) attr(1:1) = iotk_eos
  end if
  attlen = iotk_strlen_trim(attr)
  namlen = iotk_strlen_trim(name)
  if(.not.iotk_check_name(name)) then
    call iotk_error_issue(ierrl,"iotk_write_attr",__FILE__,__LINE__)
# 265 "iotk_attr.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.15 ")
# 265 "iotk_attr.spp"
call iotk_error_msg(ierrl,'Wrong tag name')
# 265 "iotk_attr.spp"
call iotk_error_write(ierrl,"name",name(1:namlen))
    goto 1
  end if
# 282 "iotk_attr.spp"
  delim = '"'
# 286 "iotk_attr.spp"
  call iotk_write(pack(val,mask=.true.),tmpval,ierrl)
# 288 "iotk_attr.spp"
  if(ierrl/=0) then
    call iotk_error_issue(ierrl,"iotk_write_attr",__FILE__,__LINE__)
# 289 "iotk_attr.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.15 ")
    goto 1
  end if
# 293 "iotk_attr.spp"
  vallen = iotk_strlen(tmpval)
  if(attlen+vallen+namlen+5>len(attr)) then
    call iotk_error_issue(ierrl,"iotk_write_attr",__FILE__,__LINE__)
# 295 "iotk_attr.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.15 ")
# 295 "iotk_attr.spp"
call iotk_error_msg(ierrl,'Attribute dummy argument is too short')
    goto 1
  end if
  attr(attlen+1:attlen+vallen+namlen+5) = " "//name(1:namlen)//"="//delim//tmpval(1:vallen)//delim//iotk_eos
1 continue
  if(present(ierr)) then
    ierr = ierrl
  else
    if(ierrl/=0) call iotk_error_handler(ierrl)
  end if
end subroutine iotk_write_attr_REAL3_6

# 308 "iotk_attr.spp"
subroutine iotk_scan_attr_REAL3_6(attr,name,val,dummy,found,default,eos,ierr)
  use iotk_base
  use iotk_error_interf
  use iotk_attr_interf, only : iotk_read
  use iotk_str_interf
  use iotk_misc_interf
  implicit none
  character(*),             intent(in)  :: attr
  character(*),             intent(in)  :: name
#ifdef __IOTK_WORKAROUND6
  REAL(kind=__IOTK_REAL3)                        :: val (:,:,:,:,:,:)
#else
  REAL(kind=__IOTK_REAL3), intent(out)           :: val (:,:,:,:,:,:)
#endif
  type(iotk_dummytype), optional :: dummy
  logical,        optional, intent(out) :: found
  REAL(kind=__IOTK_REAL3), optional, intent(in)  :: default (:,:,:,:,:,:)
  logical,        optional, intent(in)  :: eos
  integer,        optional, intent(out) :: ierr
  integer :: ierrl
  integer :: attlen,pos,equal,namlen
  character :: delim
  logical :: foundl
  character(iotk_vallenx) :: valc
# 337 "iotk_attr.spp"
  integer :: index
  REAL(kind=__IOTK_REAL3), allocatable :: tmpval (:)
# 340 "iotk_attr.spp"
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
# 351 "iotk_attr.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.15 ")
# 351 "iotk_attr.spp"
call iotk_error_msg(ierrl,'')
# 351 "iotk_attr.spp"
call iotk_error_write(ierrl,"attr",attr(equal+1:attlen))
      goto 1
    end if
    equal = equal + pos
    if(trim(attr(equal-pos:equal-1))==name(1:namlen)) foundl = .true.
    pos = verify(attr(equal+1:attlen)," ")
    if(pos<=0) then
      call iotk_error_issue(ierrl,"iotk_scan_attr",__FILE__,__LINE__)
# 358 "iotk_attr.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.15 ")
      goto 1
    end if
    equal = equal + pos
    delim = attr(equal:equal)
    if(delim/="'" .and. delim/='"') then
      call iotk_error_issue(ierrl,"iotk_scan_attr",__FILE__,__LINE__)
# 364 "iotk_attr.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.15 ")
      goto 1
    end if
    pos = scan(attr(equal+1:attlen),delim)
    if(pos<=0) then
      call iotk_error_issue(ierrl,"iotk_scan_attr",__FILE__,__LINE__)
# 369 "iotk_attr.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.15 ")
      goto 1
    end if
    if(foundl) exit
    equal = equal + pos
  end do
  if(foundl) then
    call iotk_strcpy(valc,attr(equal+1:equal+pos-1),ierrl)
    if(ierrl/=0) then
      call iotk_error_issue(ierrl,"iotk_scan_attr",__FILE__,__LINE__)
# 378 "iotk_attr.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.15 ")
      goto 1
    end if
  else
    goto 1
  end if
# 401 "iotk_attr.spp"
  allocate(tmpval(size(val)))
  index = 0
  call iotk_str_clean(valc)
  call iotk_read(tmpval,valc(1:iotk_strlen(valc)),index,ierrl)
  if(ierrl/=0) then
    call iotk_error_issue(ierrl,"iotk_scan_attr",__FILE__,__LINE__)
# 406 "iotk_attr.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.15 ")
    goto 1
  end if
# 412 "iotk_attr.spp"
  if(index/=size(val)) then
# 414 "iotk_attr.spp"
    call iotk_error_issue(ierrl,"iotk_scan_attr",__FILE__,__LINE__)
# 414 "iotk_attr.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.15 ")
# 414 "iotk_attr.spp"
call iotk_error_msg(ierrl,'Attribute size does not match')
# 414 "iotk_attr.spp"
call iotk_error_write(ierrl,"attr",valc)
# 414 "iotk_attr.spp"
call iotk_error_write(ierrl,"size",size(tmpval))
    goto 1
  end if
# 420 "iotk_attr.spp"
  val = reshape (source=tmpval,shape=shape(val))
# 422 "iotk_attr.spp"
  deallocate(tmpval)
# 424 "iotk_attr.spp"
1 continue
  if(ierrl/=0) foundl=.false.
  if(present(found)) found = foundl
  if(ierrl==0 .and. .not. present(found) .and. .not. present(default) .and. .not. foundl) then
    call iotk_error_issue(ierrl,"iotk_scan_attr",__FILE__,__LINE__)
# 428 "iotk_attr.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.15 ")
# 428 "iotk_attr.spp"
call iotk_error_msg(ierrl,'Attribute not found')
# 428 "iotk_attr.spp"
call iotk_error_write(ierrl,"name",name)
    ierrl = - ierrl
  end if
  if(present(default) .and. .not. foundl) then
# 441 "iotk_attr.spp"
    val = default
# 443 "iotk_attr.spp"
  end if
  if(present(ierr)) then
    ierr = ierrl
  else
    if(ierrl/=0) call iotk_error_handler(ierrl)
  end if
end subroutine iotk_scan_attr_REAL3_6
# 451 "iotk_attr.spp"

#endif
#endif

subroutine iotk_attr_dummy_REAL3_6
  write(0,*)
end subroutine iotk_attr_dummy_REAL3_6

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

#ifdef __IOTK_REAL3
#if 7 <= __IOTK_MAXRANK

# 158 "iotk_attr.spp"

# 233 "iotk_attr.spp"

# 236 "iotk_attr.spp"
subroutine iotk_write_attr_REAL3_7(attr,name,val,dummy,first,ierr)
  use iotk_base
  use iotk_error_interf
  use iotk_attr_interf
  use iotk_str_interf
  use iotk_misc_interf
  implicit none
  character(*), intent(inout) :: attr
  character(*), intent(in)    :: name
  REAL(kind=__IOTK_REAL3), intent(in)  :: val (:,:,:,:,:,:,:)
  type(iotk_dummytype), optional :: dummy
  logical, optional, intent(in)  :: first
  integer, optional, intent(out) :: ierr
  integer :: ierrl
  integer :: attlen
  integer :: vallen
  integer :: namlen
  character :: delim
# 257 "iotk_attr.spp"
  character(iotk_vallenx) :: tmpval
  ierrl = 0
  if(present(first)) then
    if(first) attr(1:1) = iotk_eos
  end if
  attlen = iotk_strlen_trim(attr)
  namlen = iotk_strlen_trim(name)
  if(.not.iotk_check_name(name)) then
    call iotk_error_issue(ierrl,"iotk_write_attr",__FILE__,__LINE__)
# 265 "iotk_attr.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.15 ")
# 265 "iotk_attr.spp"
call iotk_error_msg(ierrl,'Wrong tag name')
# 265 "iotk_attr.spp"
call iotk_error_write(ierrl,"name",name(1:namlen))
    goto 1
  end if
# 282 "iotk_attr.spp"
  delim = '"'
# 286 "iotk_attr.spp"
  call iotk_write(pack(val,mask=.true.),tmpval,ierrl)
# 288 "iotk_attr.spp"
  if(ierrl/=0) then
    call iotk_error_issue(ierrl,"iotk_write_attr",__FILE__,__LINE__)
# 289 "iotk_attr.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.15 ")
    goto 1
  end if
# 293 "iotk_attr.spp"
  vallen = iotk_strlen(tmpval)
  if(attlen+vallen+namlen+5>len(attr)) then
    call iotk_error_issue(ierrl,"iotk_write_attr",__FILE__,__LINE__)
# 295 "iotk_attr.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.15 ")
# 295 "iotk_attr.spp"
call iotk_error_msg(ierrl,'Attribute dummy argument is too short')
    goto 1
  end if
  attr(attlen+1:attlen+vallen+namlen+5) = " "//name(1:namlen)//"="//delim//tmpval(1:vallen)//delim//iotk_eos
1 continue
  if(present(ierr)) then
    ierr = ierrl
  else
    if(ierrl/=0) call iotk_error_handler(ierrl)
  end if
end subroutine iotk_write_attr_REAL3_7

# 308 "iotk_attr.spp"
subroutine iotk_scan_attr_REAL3_7(attr,name,val,dummy,found,default,eos,ierr)
  use iotk_base
  use iotk_error_interf
  use iotk_attr_interf, only : iotk_read
  use iotk_str_interf
  use iotk_misc_interf
  implicit none
  character(*),             intent(in)  :: attr
  character(*),             intent(in)  :: name
#ifdef __IOTK_WORKAROUND6
  REAL(kind=__IOTK_REAL3)                        :: val (:,:,:,:,:,:,:)
#else
  REAL(kind=__IOTK_REAL3), intent(out)           :: val (:,:,:,:,:,:,:)
#endif
  type(iotk_dummytype), optional :: dummy
  logical,        optional, intent(out) :: found
  REAL(kind=__IOTK_REAL3), optional, intent(in)  :: default (:,:,:,:,:,:,:)
  logical,        optional, intent(in)  :: eos
  integer,        optional, intent(out) :: ierr
  integer :: ierrl
  integer :: attlen,pos,equal,namlen
  character :: delim
  logical :: foundl
  character(iotk_vallenx) :: valc
# 337 "iotk_attr.spp"
  integer :: index
  REAL(kind=__IOTK_REAL3), allocatable :: tmpval (:)
# 340 "iotk_attr.spp"
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
# 351 "iotk_attr.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.15 ")
# 351 "iotk_attr.spp"
call iotk_error_msg(ierrl,'')
# 351 "iotk_attr.spp"
call iotk_error_write(ierrl,"attr",attr(equal+1:attlen))
      goto 1
    end if
    equal = equal + pos
    if(trim(attr(equal-pos:equal-1))==name(1:namlen)) foundl = .true.
    pos = verify(attr(equal+1:attlen)," ")
    if(pos<=0) then
      call iotk_error_issue(ierrl,"iotk_scan_attr",__FILE__,__LINE__)
# 358 "iotk_attr.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.15 ")
      goto 1
    end if
    equal = equal + pos
    delim = attr(equal:equal)
    if(delim/="'" .and. delim/='"') then
      call iotk_error_issue(ierrl,"iotk_scan_attr",__FILE__,__LINE__)
# 364 "iotk_attr.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.15 ")
      goto 1
    end if
    pos = scan(attr(equal+1:attlen),delim)
    if(pos<=0) then
      call iotk_error_issue(ierrl,"iotk_scan_attr",__FILE__,__LINE__)
# 369 "iotk_attr.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.15 ")
      goto 1
    end if
    if(foundl) exit
    equal = equal + pos
  end do
  if(foundl) then
    call iotk_strcpy(valc,attr(equal+1:equal+pos-1),ierrl)
    if(ierrl/=0) then
      call iotk_error_issue(ierrl,"iotk_scan_attr",__FILE__,__LINE__)
# 378 "iotk_attr.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.15 ")
      goto 1
    end if
  else
    goto 1
  end if
# 401 "iotk_attr.spp"
  allocate(tmpval(size(val)))
  index = 0
  call iotk_str_clean(valc)
  call iotk_read(tmpval,valc(1:iotk_strlen(valc)),index,ierrl)
  if(ierrl/=0) then
    call iotk_error_issue(ierrl,"iotk_scan_attr",__FILE__,__LINE__)
# 406 "iotk_attr.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.15 ")
    goto 1
  end if
# 412 "iotk_attr.spp"
  if(index/=size(val)) then
# 414 "iotk_attr.spp"
    call iotk_error_issue(ierrl,"iotk_scan_attr",__FILE__,__LINE__)
# 414 "iotk_attr.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.15 ")
# 414 "iotk_attr.spp"
call iotk_error_msg(ierrl,'Attribute size does not match')
# 414 "iotk_attr.spp"
call iotk_error_write(ierrl,"attr",valc)
# 414 "iotk_attr.spp"
call iotk_error_write(ierrl,"size",size(tmpval))
    goto 1
  end if
# 420 "iotk_attr.spp"
  val = reshape (source=tmpval,shape=shape(val))
# 422 "iotk_attr.spp"
  deallocate(tmpval)
# 424 "iotk_attr.spp"
1 continue
  if(ierrl/=0) foundl=.false.
  if(present(found)) found = foundl
  if(ierrl==0 .and. .not. present(found) .and. .not. present(default) .and. .not. foundl) then
    call iotk_error_issue(ierrl,"iotk_scan_attr",__FILE__,__LINE__)
# 428 "iotk_attr.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.15 ")
# 428 "iotk_attr.spp"
call iotk_error_msg(ierrl,'Attribute not found')
# 428 "iotk_attr.spp"
call iotk_error_write(ierrl,"name",name)
    ierrl = - ierrl
  end if
  if(present(default) .and. .not. foundl) then
# 441 "iotk_attr.spp"
    val = default
# 443 "iotk_attr.spp"
  end if
  if(present(ierr)) then
    ierr = ierrl
  else
    if(ierrl/=0) call iotk_error_handler(ierrl)
  end if
end subroutine iotk_scan_attr_REAL3_7
# 451 "iotk_attr.spp"

#endif
#endif

subroutine iotk_attr_dummy_REAL3_7
  write(0,*)
end subroutine iotk_attr_dummy_REAL3_7

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

#ifdef __IOTK_REAL4
#if 0 <= __IOTK_MAXRANK

# 83 "iotk_attr.spp"
! This is needed as a workaround for bugged pack 
subroutine iotk_private_pack_REAL4(out,in,n,l)
    use iotk_base
    implicit none
    integer,                                    intent(in)  :: n,l
# 92 "iotk_attr.spp"
    REAL (kind=__IOTK_REAL4), intent(out) :: out(n)
    REAL (kind=__IOTK_REAL4), intent(in)  :: in(n)
# 95 "iotk_attr.spp"
    out = in
end subroutine iotk_private_pack_REAL4

# 100 "iotk_attr.spp"
subroutine iotk_write_REAL4(val,string,ierr)
  use iotk_base
  use iotk_error_interf
  use iotk_xtox_interf
  use iotk_fmt_interf
  use iotk_str_interf
  use iotk_misc_interf
  implicit none
  REAL(kind=__IOTK_REAL4), intent(in) :: val(:)
#ifdef __IOTK_WORKAROUND6
  character(len=*)              :: string
#else
  character(len=*), intent(out) :: string
#endif
  integer, intent(out) :: ierr
# 116 "iotk_attr.spp"
  character(len=100) :: tmpval
# 118 "iotk_attr.spp"
  integer :: index,iostat
  ierr = 0
  iostat = 0 
  string(1:1) = iotk_eos
  if(size(val)==0) return
  if(len(string)==0) then
    call iotk_error_issue(ierr,"iotk_write",__FILE__,__LINE__)
# 124 "iotk_attr.spp"
call iotk_error_msg(ierr,"CVS Revision: 1.15 ")
    return
  end if
  do index=1,size(val)
# 141 "iotk_attr.spp"
    write(tmpval,trim(iotk_wfmt("REAL",kind(val),1,-1," ")),iostat=iostat) val(index)
    if(iostat/=0) then
      call iotk_error_issue(ierr,"iotk_write",__FILE__,__LINE__)
# 143 "iotk_attr.spp"
call iotk_error_msg(ierr,"CVS Revision: 1.15 ")
# 143 "iotk_attr.spp"
call iotk_error_msg(ierr,' ')
# 143 "iotk_attr.spp"
call iotk_error_write(ierr,"iostat",iostat)
      return
    end if
    call iotk_strcat(string,trim(adjustl(tmpval))//" ",ierr)
    if(ierr/=0) then
      call iotk_error_issue(ierr,"iotk_write",__FILE__,__LINE__)
# 148 "iotk_attr.spp"
call iotk_error_msg(ierr,"CVS Revision: 1.15 ")
      return
    end if
# 152 "iotk_attr.spp"
  end do
! taglio l'ultimo spazio
  string(iotk_strlen(string):iotk_strlen(string)) = iotk_eos
end subroutine iotk_write_REAL4
# 158 "iotk_attr.spp"

# 162 "iotk_attr.spp"
subroutine iotk_read_REAL4(val,string,index,ierr)
  use iotk_base
  use iotk_error_interf
  use iotk_xtox_interf
  use iotk_misc_interf
  implicit none
  REAL(kind=__IOTK_REAL4), intent(inout) :: val(:)
  character(len=*), intent(in) :: string
  integer, intent(inout) :: index
  integer, intent(out) :: ierr
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
! PER ORA CONSIDERA LE VIRGOLE COME SPAZII
  do
    pos = verify(string(pos1+1:)," ,")+pos1
    if(pos==pos1) exit
    pos = pos - 1
    pos1 = scan(string(pos+1:)," ,")+pos
    if(pos1==pos) pos1 = len(string) + 1
!LEGGI string(pos+1:pos1-1)
    index = index+1
    if(index>maxindex) then
      call iotk_error_issue(ierr,"iotk_read",__FILE__,__LINE__)
# 200 "iotk_attr.spp"
call iotk_error_msg(ierr,"CVS Revision: 1.15 ")
# 200 "iotk_attr.spp"
call iotk_error_msg(ierr,'Too many data')
    end if
# 207 "iotk_attr.spp"
    read(string(pos+1:pos1-1),"(G100.95)",iostat=iostat) val(index)
# 223 "iotk_attr.spp"
    if(iostat/=0) then
      call iotk_error_issue(ierr,"iotk_read",__FILE__,__LINE__)
# 224 "iotk_attr.spp"
call iotk_error_msg(ierr,"CVS Revision: 1.15 ")
# 224 "iotk_attr.spp"
call iotk_error_msg(ierr,'Error reading from string')
# 224 "iotk_attr.spp"
call iotk_error_write(ierr,"string",string(pos+1:pos1-1))
# 224 "iotk_attr.spp"
call iotk_error_write(ierr,"iostat",iostat)
      return
    end if
# 228 "iotk_attr.spp"
    if(pos1>=len(string)) exit
  end do
end subroutine iotk_read_REAL4
# 233 "iotk_attr.spp"

# 236 "iotk_attr.spp"
subroutine iotk_write_attr_REAL4_0(attr,name,val,dummy,first,ierr)
  use iotk_base
  use iotk_error_interf
  use iotk_attr_interf
  use iotk_str_interf
  use iotk_misc_interf
  implicit none
  character(*), intent(inout) :: attr
  character(*), intent(in)    :: name
  REAL(kind=__IOTK_REAL4), intent(in)  :: val 
  type(iotk_dummytype), optional :: dummy
  logical, optional, intent(in)  :: first
  integer, optional, intent(out) :: ierr
  integer :: ierrl
  integer :: attlen
  integer :: vallen
  integer :: namlen
  character :: delim
# 257 "iotk_attr.spp"
  character(iotk_vallenx) :: tmpval
  ierrl = 0
  if(present(first)) then
    if(first) attr(1:1) = iotk_eos
  end if
  attlen = iotk_strlen_trim(attr)
  namlen = iotk_strlen_trim(name)
  if(.not.iotk_check_name(name)) then
    call iotk_error_issue(ierrl,"iotk_write_attr",__FILE__,__LINE__)
# 265 "iotk_attr.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.15 ")
# 265 "iotk_attr.spp"
call iotk_error_msg(ierrl,'Wrong tag name')
# 265 "iotk_attr.spp"
call iotk_error_write(ierrl,"name",name(1:namlen))
    goto 1
  end if
# 282 "iotk_attr.spp"
  delim = '"'
# 284 "iotk_attr.spp"
  call iotk_write((/val/),tmpval,ierrl)
# 288 "iotk_attr.spp"
  if(ierrl/=0) then
    call iotk_error_issue(ierrl,"iotk_write_attr",__FILE__,__LINE__)
# 289 "iotk_attr.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.15 ")
    goto 1
  end if
# 293 "iotk_attr.spp"
  vallen = iotk_strlen(tmpval)
  if(attlen+vallen+namlen+5>len(attr)) then
    call iotk_error_issue(ierrl,"iotk_write_attr",__FILE__,__LINE__)
# 295 "iotk_attr.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.15 ")
# 295 "iotk_attr.spp"
call iotk_error_msg(ierrl,'Attribute dummy argument is too short')
    goto 1
  end if
  attr(attlen+1:attlen+vallen+namlen+5) = " "//name(1:namlen)//"="//delim//tmpval(1:vallen)//delim//iotk_eos
1 continue
  if(present(ierr)) then
    ierr = ierrl
  else
    if(ierrl/=0) call iotk_error_handler(ierrl)
  end if
end subroutine iotk_write_attr_REAL4_0

# 308 "iotk_attr.spp"
subroutine iotk_scan_attr_REAL4_0(attr,name,val,dummy,found,default,eos,ierr)
  use iotk_base
  use iotk_error_interf
  use iotk_attr_interf, only : iotk_read
  use iotk_str_interf
  use iotk_misc_interf
  implicit none
  character(*),             intent(in)  :: attr
  character(*),             intent(in)  :: name
#ifdef __IOTK_WORKAROUND6
  REAL(kind=__IOTK_REAL4)                        :: val 
#else
  REAL(kind=__IOTK_REAL4), intent(out)           :: val 
#endif
  type(iotk_dummytype), optional :: dummy
  logical,        optional, intent(out) :: found
  REAL(kind=__IOTK_REAL4), optional, intent(in)  :: default 
  logical,        optional, intent(in)  :: eos
  integer,        optional, intent(out) :: ierr
  integer :: ierrl
  integer :: attlen,pos,equal,namlen
  character :: delim
  logical :: foundl
  character(iotk_vallenx) :: valc
# 337 "iotk_attr.spp"
  integer :: index
  REAL(kind=__IOTK_REAL4), allocatable :: tmpval (:)
# 340 "iotk_attr.spp"
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
# 351 "iotk_attr.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.15 ")
# 351 "iotk_attr.spp"
call iotk_error_msg(ierrl,'')
# 351 "iotk_attr.spp"
call iotk_error_write(ierrl,"attr",attr(equal+1:attlen))
      goto 1
    end if
    equal = equal + pos
    if(trim(attr(equal-pos:equal-1))==name(1:namlen)) foundl = .true.
    pos = verify(attr(equal+1:attlen)," ")
    if(pos<=0) then
      call iotk_error_issue(ierrl,"iotk_scan_attr",__FILE__,__LINE__)
# 358 "iotk_attr.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.15 ")
      goto 1
    end if
    equal = equal + pos
    delim = attr(equal:equal)
    if(delim/="'" .and. delim/='"') then
      call iotk_error_issue(ierrl,"iotk_scan_attr",__FILE__,__LINE__)
# 364 "iotk_attr.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.15 ")
      goto 1
    end if
    pos = scan(attr(equal+1:attlen),delim)
    if(pos<=0) then
      call iotk_error_issue(ierrl,"iotk_scan_attr",__FILE__,__LINE__)
# 369 "iotk_attr.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.15 ")
      goto 1
    end if
    if(foundl) exit
    equal = equal + pos
  end do
  if(foundl) then
    call iotk_strcpy(valc,attr(equal+1:equal+pos-1),ierrl)
    if(ierrl/=0) then
      call iotk_error_issue(ierrl,"iotk_scan_attr",__FILE__,__LINE__)
# 378 "iotk_attr.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.15 ")
      goto 1
    end if
  else
    goto 1
  end if
# 401 "iotk_attr.spp"
  allocate(tmpval(1))
  index = 0
  call iotk_str_clean(valc)
  call iotk_read(tmpval,valc(1:iotk_strlen(valc)),index,ierrl)
  if(ierrl/=0) then
    call iotk_error_issue(ierrl,"iotk_scan_attr",__FILE__,__LINE__)
# 406 "iotk_attr.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.15 ")
    goto 1
  end if
# 412 "iotk_attr.spp"
  if(index/=1) then
# 414 "iotk_attr.spp"
    call iotk_error_issue(ierrl,"iotk_scan_attr",__FILE__,__LINE__)
# 414 "iotk_attr.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.15 ")
# 414 "iotk_attr.spp"
call iotk_error_msg(ierrl,'Attribute size does not match')
# 414 "iotk_attr.spp"
call iotk_error_write(ierrl,"attr",valc)
# 414 "iotk_attr.spp"
call iotk_error_write(ierrl,"size",size(tmpval))
    goto 1
  end if
# 418 "iotk_attr.spp"
  val = tmpval(1)
# 422 "iotk_attr.spp"
  deallocate(tmpval)
# 424 "iotk_attr.spp"
1 continue
  if(ierrl/=0) foundl=.false.
  if(present(found)) found = foundl
  if(ierrl==0 .and. .not. present(found) .and. .not. present(default) .and. .not. foundl) then
    call iotk_error_issue(ierrl,"iotk_scan_attr",__FILE__,__LINE__)
# 428 "iotk_attr.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.15 ")
# 428 "iotk_attr.spp"
call iotk_error_msg(ierrl,'Attribute not found')
# 428 "iotk_attr.spp"
call iotk_error_write(ierrl,"name",name)
    ierrl = - ierrl
  end if
  if(present(default) .and. .not. foundl) then
# 441 "iotk_attr.spp"
    val = default
# 443 "iotk_attr.spp"
  end if
  if(present(ierr)) then
    ierr = ierrl
  else
    if(ierrl/=0) call iotk_error_handler(ierrl)
  end if
end subroutine iotk_scan_attr_REAL4_0
# 451 "iotk_attr.spp"

#endif
#endif

subroutine iotk_attr_dummy_REAL4_0
  write(0,*)
end subroutine iotk_attr_dummy_REAL4_0

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

#ifdef __IOTK_REAL4
#if 1 <= __IOTK_MAXRANK

# 158 "iotk_attr.spp"

# 233 "iotk_attr.spp"

# 236 "iotk_attr.spp"
subroutine iotk_write_attr_REAL4_1(attr,name,val,dummy,first,ierr)
  use iotk_base
  use iotk_error_interf
  use iotk_attr_interf
  use iotk_str_interf
  use iotk_misc_interf
  implicit none
  character(*), intent(inout) :: attr
  character(*), intent(in)    :: name
  REAL(kind=__IOTK_REAL4), intent(in)  :: val (:)
  type(iotk_dummytype), optional :: dummy
  logical, optional, intent(in)  :: first
  integer, optional, intent(out) :: ierr
  integer :: ierrl
  integer :: attlen
  integer :: vallen
  integer :: namlen
  character :: delim
# 257 "iotk_attr.spp"
  character(iotk_vallenx) :: tmpval
  ierrl = 0
  if(present(first)) then
    if(first) attr(1:1) = iotk_eos
  end if
  attlen = iotk_strlen_trim(attr)
  namlen = iotk_strlen_trim(name)
  if(.not.iotk_check_name(name)) then
    call iotk_error_issue(ierrl,"iotk_write_attr",__FILE__,__LINE__)
# 265 "iotk_attr.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.15 ")
# 265 "iotk_attr.spp"
call iotk_error_msg(ierrl,'Wrong tag name')
# 265 "iotk_attr.spp"
call iotk_error_write(ierrl,"name",name(1:namlen))
    goto 1
  end if
# 282 "iotk_attr.spp"
  delim = '"'
# 286 "iotk_attr.spp"
  call iotk_write(pack(val,mask=.true.),tmpval,ierrl)
# 288 "iotk_attr.spp"
  if(ierrl/=0) then
    call iotk_error_issue(ierrl,"iotk_write_attr",__FILE__,__LINE__)
# 289 "iotk_attr.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.15 ")
    goto 1
  end if
# 293 "iotk_attr.spp"
  vallen = iotk_strlen(tmpval)
  if(attlen+vallen+namlen+5>len(attr)) then
    call iotk_error_issue(ierrl,"iotk_write_attr",__FILE__,__LINE__)
# 295 "iotk_attr.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.15 ")
# 295 "iotk_attr.spp"
call iotk_error_msg(ierrl,'Attribute dummy argument is too short')
    goto 1
  end if
  attr(attlen+1:attlen+vallen+namlen+5) = " "//name(1:namlen)//"="//delim//tmpval(1:vallen)//delim//iotk_eos
1 continue
  if(present(ierr)) then
    ierr = ierrl
  else
    if(ierrl/=0) call iotk_error_handler(ierrl)
  end if
end subroutine iotk_write_attr_REAL4_1

# 308 "iotk_attr.spp"
subroutine iotk_scan_attr_REAL4_1(attr,name,val,dummy,found,default,eos,ierr)
  use iotk_base
  use iotk_error_interf
  use iotk_attr_interf, only : iotk_read
  use iotk_str_interf
  use iotk_misc_interf
  implicit none
  character(*),             intent(in)  :: attr
  character(*),             intent(in)  :: name
#ifdef __IOTK_WORKAROUND6
  REAL(kind=__IOTK_REAL4)                        :: val (:)
#else
  REAL(kind=__IOTK_REAL4), intent(out)           :: val (:)
#endif
  type(iotk_dummytype), optional :: dummy
  logical,        optional, intent(out) :: found
  REAL(kind=__IOTK_REAL4), optional, intent(in)  :: default (:)
  logical,        optional, intent(in)  :: eos
  integer,        optional, intent(out) :: ierr
  integer :: ierrl
  integer :: attlen,pos,equal,namlen
  character :: delim
  logical :: foundl
  character(iotk_vallenx) :: valc
# 337 "iotk_attr.spp"
  integer :: index
  REAL(kind=__IOTK_REAL4), allocatable :: tmpval (:)
# 340 "iotk_attr.spp"
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
# 351 "iotk_attr.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.15 ")
# 351 "iotk_attr.spp"
call iotk_error_msg(ierrl,'')
# 351 "iotk_attr.spp"
call iotk_error_write(ierrl,"attr",attr(equal+1:attlen))
      goto 1
    end if
    equal = equal + pos
    if(trim(attr(equal-pos:equal-1))==name(1:namlen)) foundl = .true.
    pos = verify(attr(equal+1:attlen)," ")
    if(pos<=0) then
      call iotk_error_issue(ierrl,"iotk_scan_attr",__FILE__,__LINE__)
# 358 "iotk_attr.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.15 ")
      goto 1
    end if
    equal = equal + pos
    delim = attr(equal:equal)
    if(delim/="'" .and. delim/='"') then
      call iotk_error_issue(ierrl,"iotk_scan_attr",__FILE__,__LINE__)
# 364 "iotk_attr.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.15 ")
      goto 1
    end if
    pos = scan(attr(equal+1:attlen),delim)
    if(pos<=0) then
      call iotk_error_issue(ierrl,"iotk_scan_attr",__FILE__,__LINE__)
# 369 "iotk_attr.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.15 ")
      goto 1
    end if
    if(foundl) exit
    equal = equal + pos
  end do
  if(foundl) then
    call iotk_strcpy(valc,attr(equal+1:equal+pos-1),ierrl)
    if(ierrl/=0) then
      call iotk_error_issue(ierrl,"iotk_scan_attr",__FILE__,__LINE__)
# 378 "iotk_attr.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.15 ")
      goto 1
    end if
  else
    goto 1
  end if
# 401 "iotk_attr.spp"
  allocate(tmpval(size(val)))
  index = 0
  call iotk_str_clean(valc)
  call iotk_read(tmpval,valc(1:iotk_strlen(valc)),index,ierrl)
  if(ierrl/=0) then
    call iotk_error_issue(ierrl,"iotk_scan_attr",__FILE__,__LINE__)
# 406 "iotk_attr.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.15 ")
    goto 1
  end if
# 412 "iotk_attr.spp"
  if(index/=size(val)) then
# 414 "iotk_attr.spp"
    call iotk_error_issue(ierrl,"iotk_scan_attr",__FILE__,__LINE__)
# 414 "iotk_attr.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.15 ")
# 414 "iotk_attr.spp"
call iotk_error_msg(ierrl,'Attribute size does not match')
# 414 "iotk_attr.spp"
call iotk_error_write(ierrl,"attr",valc)
# 414 "iotk_attr.spp"
call iotk_error_write(ierrl,"size",size(tmpval))
    goto 1
  end if
# 420 "iotk_attr.spp"
  val = reshape (source=tmpval,shape=shape(val))
# 422 "iotk_attr.spp"
  deallocate(tmpval)
# 424 "iotk_attr.spp"
1 continue
  if(ierrl/=0) foundl=.false.
  if(present(found)) found = foundl
  if(ierrl==0 .and. .not. present(found) .and. .not. present(default) .and. .not. foundl) then
    call iotk_error_issue(ierrl,"iotk_scan_attr",__FILE__,__LINE__)
# 428 "iotk_attr.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.15 ")
# 428 "iotk_attr.spp"
call iotk_error_msg(ierrl,'Attribute not found')
# 428 "iotk_attr.spp"
call iotk_error_write(ierrl,"name",name)
    ierrl = - ierrl
  end if
  if(present(default) .and. .not. foundl) then
# 441 "iotk_attr.spp"
    val = default
# 443 "iotk_attr.spp"
  end if
  if(present(ierr)) then
    ierr = ierrl
  else
    if(ierrl/=0) call iotk_error_handler(ierrl)
  end if
end subroutine iotk_scan_attr_REAL4_1
# 451 "iotk_attr.spp"

#endif
#endif

subroutine iotk_attr_dummy_REAL4_1
  write(0,*)
end subroutine iotk_attr_dummy_REAL4_1

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

#ifdef __IOTK_REAL4
#if 2 <= __IOTK_MAXRANK

# 158 "iotk_attr.spp"

# 233 "iotk_attr.spp"

# 236 "iotk_attr.spp"
subroutine iotk_write_attr_REAL4_2(attr,name,val,dummy,first,ierr)
  use iotk_base
  use iotk_error_interf
  use iotk_attr_interf
  use iotk_str_interf
  use iotk_misc_interf
  implicit none
  character(*), intent(inout) :: attr
  character(*), intent(in)    :: name
  REAL(kind=__IOTK_REAL4), intent(in)  :: val (:,:)
  type(iotk_dummytype), optional :: dummy
  logical, optional, intent(in)  :: first
  integer, optional, intent(out) :: ierr
  integer :: ierrl
  integer :: attlen
  integer :: vallen
  integer :: namlen
  character :: delim
# 257 "iotk_attr.spp"
  character(iotk_vallenx) :: tmpval
  ierrl = 0
  if(present(first)) then
    if(first) attr(1:1) = iotk_eos
  end if
  attlen = iotk_strlen_trim(attr)
  namlen = iotk_strlen_trim(name)
  if(.not.iotk_check_name(name)) then
    call iotk_error_issue(ierrl,"iotk_write_attr",__FILE__,__LINE__)
# 265 "iotk_attr.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.15 ")
# 265 "iotk_attr.spp"
call iotk_error_msg(ierrl,'Wrong tag name')
# 265 "iotk_attr.spp"
call iotk_error_write(ierrl,"name",name(1:namlen))
    goto 1
  end if
# 282 "iotk_attr.spp"
  delim = '"'
# 286 "iotk_attr.spp"
  call iotk_write(pack(val,mask=.true.),tmpval,ierrl)
# 288 "iotk_attr.spp"
  if(ierrl/=0) then
    call iotk_error_issue(ierrl,"iotk_write_attr",__FILE__,__LINE__)
# 289 "iotk_attr.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.15 ")
    goto 1
  end if
# 293 "iotk_attr.spp"
  vallen = iotk_strlen(tmpval)
  if(attlen+vallen+namlen+5>len(attr)) then
    call iotk_error_issue(ierrl,"iotk_write_attr",__FILE__,__LINE__)
# 295 "iotk_attr.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.15 ")
# 295 "iotk_attr.spp"
call iotk_error_msg(ierrl,'Attribute dummy argument is too short')
    goto 1
  end if
  attr(attlen+1:attlen+vallen+namlen+5) = " "//name(1:namlen)//"="//delim//tmpval(1:vallen)//delim//iotk_eos
1 continue
  if(present(ierr)) then
    ierr = ierrl
  else
    if(ierrl/=0) call iotk_error_handler(ierrl)
  end if
end subroutine iotk_write_attr_REAL4_2

# 308 "iotk_attr.spp"
subroutine iotk_scan_attr_REAL4_2(attr,name,val,dummy,found,default,eos,ierr)
  use iotk_base
  use iotk_error_interf
  use iotk_attr_interf, only : iotk_read
  use iotk_str_interf
  use iotk_misc_interf
  implicit none
  character(*),             intent(in)  :: attr
  character(*),             intent(in)  :: name
#ifdef __IOTK_WORKAROUND6
  REAL(kind=__IOTK_REAL4)                        :: val (:,:)
#else
  REAL(kind=__IOTK_REAL4), intent(out)           :: val (:,:)
#endif
  type(iotk_dummytype), optional :: dummy
  logical,        optional, intent(out) :: found
  REAL(kind=__IOTK_REAL4), optional, intent(in)  :: default (:,:)
  logical,        optional, intent(in)  :: eos
  integer,        optional, intent(out) :: ierr
  integer :: ierrl
  integer :: attlen,pos,equal,namlen
  character :: delim
  logical :: foundl
  character(iotk_vallenx) :: valc
# 337 "iotk_attr.spp"
  integer :: index
  REAL(kind=__IOTK_REAL4), allocatable :: tmpval (:)
# 340 "iotk_attr.spp"
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
# 351 "iotk_attr.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.15 ")
# 351 "iotk_attr.spp"
call iotk_error_msg(ierrl,'')
# 351 "iotk_attr.spp"
call iotk_error_write(ierrl,"attr",attr(equal+1:attlen))
      goto 1
    end if
    equal = equal + pos
    if(trim(attr(equal-pos:equal-1))==name(1:namlen)) foundl = .true.
    pos = verify(attr(equal+1:attlen)," ")
    if(pos<=0) then
      call iotk_error_issue(ierrl,"iotk_scan_attr",__FILE__,__LINE__)
# 358 "iotk_attr.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.15 ")
      goto 1
    end if
    equal = equal + pos
    delim = attr(equal:equal)
    if(delim/="'" .and. delim/='"') then
      call iotk_error_issue(ierrl,"iotk_scan_attr",__FILE__,__LINE__)
# 364 "iotk_attr.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.15 ")
      goto 1
    end if
    pos = scan(attr(equal+1:attlen),delim)
    if(pos<=0) then
      call iotk_error_issue(ierrl,"iotk_scan_attr",__FILE__,__LINE__)
# 369 "iotk_attr.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.15 ")
      goto 1
    end if
    if(foundl) exit
    equal = equal + pos
  end do
  if(foundl) then
    call iotk_strcpy(valc,attr(equal+1:equal+pos-1),ierrl)
    if(ierrl/=0) then
      call iotk_error_issue(ierrl,"iotk_scan_attr",__FILE__,__LINE__)
# 378 "iotk_attr.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.15 ")
      goto 1
    end if
  else
    goto 1
  end if
# 401 "iotk_attr.spp"
  allocate(tmpval(size(val)))
  index = 0
  call iotk_str_clean(valc)
  call iotk_read(tmpval,valc(1:iotk_strlen(valc)),index,ierrl)
  if(ierrl/=0) then
    call iotk_error_issue(ierrl,"iotk_scan_attr",__FILE__,__LINE__)
# 406 "iotk_attr.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.15 ")
    goto 1
  end if
# 412 "iotk_attr.spp"
  if(index/=size(val)) then
# 414 "iotk_attr.spp"
    call iotk_error_issue(ierrl,"iotk_scan_attr",__FILE__,__LINE__)
# 414 "iotk_attr.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.15 ")
# 414 "iotk_attr.spp"
call iotk_error_msg(ierrl,'Attribute size does not match')
# 414 "iotk_attr.spp"
call iotk_error_write(ierrl,"attr",valc)
# 414 "iotk_attr.spp"
call iotk_error_write(ierrl,"size",size(tmpval))
    goto 1
  end if
# 420 "iotk_attr.spp"
  val = reshape (source=tmpval,shape=shape(val))
# 422 "iotk_attr.spp"
  deallocate(tmpval)
# 424 "iotk_attr.spp"
1 continue
  if(ierrl/=0) foundl=.false.
  if(present(found)) found = foundl
  if(ierrl==0 .and. .not. present(found) .and. .not. present(default) .and. .not. foundl) then
    call iotk_error_issue(ierrl,"iotk_scan_attr",__FILE__,__LINE__)
# 428 "iotk_attr.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.15 ")
# 428 "iotk_attr.spp"
call iotk_error_msg(ierrl,'Attribute not found')
# 428 "iotk_attr.spp"
call iotk_error_write(ierrl,"name",name)
    ierrl = - ierrl
  end if
  if(present(default) .and. .not. foundl) then
# 441 "iotk_attr.spp"
    val = default
# 443 "iotk_attr.spp"
  end if
  if(present(ierr)) then
    ierr = ierrl
  else
    if(ierrl/=0) call iotk_error_handler(ierrl)
  end if
end subroutine iotk_scan_attr_REAL4_2
# 451 "iotk_attr.spp"

#endif
#endif

subroutine iotk_attr_dummy_REAL4_2
  write(0,*)
end subroutine iotk_attr_dummy_REAL4_2

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

#ifdef __IOTK_REAL4
#if 3 <= __IOTK_MAXRANK

# 158 "iotk_attr.spp"

# 233 "iotk_attr.spp"

# 236 "iotk_attr.spp"
subroutine iotk_write_attr_REAL4_3(attr,name,val,dummy,first,ierr)
  use iotk_base
  use iotk_error_interf
  use iotk_attr_interf
  use iotk_str_interf
  use iotk_misc_interf
  implicit none
  character(*), intent(inout) :: attr
  character(*), intent(in)    :: name
  REAL(kind=__IOTK_REAL4), intent(in)  :: val (:,:,:)
  type(iotk_dummytype), optional :: dummy
  logical, optional, intent(in)  :: first
  integer, optional, intent(out) :: ierr
  integer :: ierrl
  integer :: attlen
  integer :: vallen
  integer :: namlen
  character :: delim
# 257 "iotk_attr.spp"
  character(iotk_vallenx) :: tmpval
  ierrl = 0
  if(present(first)) then
    if(first) attr(1:1) = iotk_eos
  end if
  attlen = iotk_strlen_trim(attr)
  namlen = iotk_strlen_trim(name)
  if(.not.iotk_check_name(name)) then
    call iotk_error_issue(ierrl,"iotk_write_attr",__FILE__,__LINE__)
# 265 "iotk_attr.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.15 ")
# 265 "iotk_attr.spp"
call iotk_error_msg(ierrl,'Wrong tag name')
# 265 "iotk_attr.spp"
call iotk_error_write(ierrl,"name",name(1:namlen))
    goto 1
  end if
# 282 "iotk_attr.spp"
  delim = '"'
# 286 "iotk_attr.spp"
  call iotk_write(pack(val,mask=.true.),tmpval,ierrl)
# 288 "iotk_attr.spp"
  if(ierrl/=0) then
    call iotk_error_issue(ierrl,"iotk_write_attr",__FILE__,__LINE__)
# 289 "iotk_attr.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.15 ")
    goto 1
  end if
# 293 "iotk_attr.spp"
  vallen = iotk_strlen(tmpval)
  if(attlen+vallen+namlen+5>len(attr)) then
    call iotk_error_issue(ierrl,"iotk_write_attr",__FILE__,__LINE__)
# 295 "iotk_attr.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.15 ")
# 295 "iotk_attr.spp"
call iotk_error_msg(ierrl,'Attribute dummy argument is too short')
    goto 1
  end if
  attr(attlen+1:attlen+vallen+namlen+5) = " "//name(1:namlen)//"="//delim//tmpval(1:vallen)//delim//iotk_eos
1 continue
  if(present(ierr)) then
    ierr = ierrl
  else
    if(ierrl/=0) call iotk_error_handler(ierrl)
  end if
end subroutine iotk_write_attr_REAL4_3

# 308 "iotk_attr.spp"
subroutine iotk_scan_attr_REAL4_3(attr,name,val,dummy,found,default,eos,ierr)
  use iotk_base
  use iotk_error_interf
  use iotk_attr_interf, only : iotk_read
  use iotk_str_interf
  use iotk_misc_interf
  implicit none
  character(*),             intent(in)  :: attr
  character(*),             intent(in)  :: name
#ifdef __IOTK_WORKAROUND6
  REAL(kind=__IOTK_REAL4)                        :: val (:,:,:)
#else
  REAL(kind=__IOTK_REAL4), intent(out)           :: val (:,:,:)
#endif
  type(iotk_dummytype), optional :: dummy
  logical,        optional, intent(out) :: found
  REAL(kind=__IOTK_REAL4), optional, intent(in)  :: default (:,:,:)
  logical,        optional, intent(in)  :: eos
  integer,        optional, intent(out) :: ierr
  integer :: ierrl
  integer :: attlen,pos,equal,namlen
  character :: delim
  logical :: foundl
  character(iotk_vallenx) :: valc
# 337 "iotk_attr.spp"
  integer :: index
  REAL(kind=__IOTK_REAL4), allocatable :: tmpval (:)
# 340 "iotk_attr.spp"
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
# 351 "iotk_attr.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.15 ")
# 351 "iotk_attr.spp"
call iotk_error_msg(ierrl,'')
# 351 "iotk_attr.spp"
call iotk_error_write(ierrl,"attr",attr(equal+1:attlen))
      goto 1
    end if
    equal = equal + pos
    if(trim(attr(equal-pos:equal-1))==name(1:namlen)) foundl = .true.
    pos = verify(attr(equal+1:attlen)," ")
    if(pos<=0) then
      call iotk_error_issue(ierrl,"iotk_scan_attr",__FILE__,__LINE__)
# 358 "iotk_attr.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.15 ")
      goto 1
    end if
    equal = equal + pos
    delim = attr(equal:equal)
    if(delim/="'" .and. delim/='"') then
      call iotk_error_issue(ierrl,"iotk_scan_attr",__FILE__,__LINE__)
# 364 "iotk_attr.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.15 ")
      goto 1
    end if
    pos = scan(attr(equal+1:attlen),delim)
    if(pos<=0) then
      call iotk_error_issue(ierrl,"iotk_scan_attr",__FILE__,__LINE__)
# 369 "iotk_attr.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.15 ")
      goto 1
    end if
    if(foundl) exit
    equal = equal + pos
  end do
  if(foundl) then
    call iotk_strcpy(valc,attr(equal+1:equal+pos-1),ierrl)
    if(ierrl/=0) then
      call iotk_error_issue(ierrl,"iotk_scan_attr",__FILE__,__LINE__)
# 378 "iotk_attr.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.15 ")
      goto 1
    end if
  else
    goto 1
  end if
# 401 "iotk_attr.spp"
  allocate(tmpval(size(val)))
  index = 0
  call iotk_str_clean(valc)
  call iotk_read(tmpval,valc(1:iotk_strlen(valc)),index,ierrl)
  if(ierrl/=0) then
    call iotk_error_issue(ierrl,"iotk_scan_attr",__FILE__,__LINE__)
# 406 "iotk_attr.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.15 ")
    goto 1
  end if
# 412 "iotk_attr.spp"
  if(index/=size(val)) then
# 414 "iotk_attr.spp"
    call iotk_error_issue(ierrl,"iotk_scan_attr",__FILE__,__LINE__)
# 414 "iotk_attr.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.15 ")
# 414 "iotk_attr.spp"
call iotk_error_msg(ierrl,'Attribute size does not match')
# 414 "iotk_attr.spp"
call iotk_error_write(ierrl,"attr",valc)
# 414 "iotk_attr.spp"
call iotk_error_write(ierrl,"size",size(tmpval))
    goto 1
  end if
# 420 "iotk_attr.spp"
  val = reshape (source=tmpval,shape=shape(val))
# 422 "iotk_attr.spp"
  deallocate(tmpval)
# 424 "iotk_attr.spp"
1 continue
  if(ierrl/=0) foundl=.false.
  if(present(found)) found = foundl
  if(ierrl==0 .and. .not. present(found) .and. .not. present(default) .and. .not. foundl) then
    call iotk_error_issue(ierrl,"iotk_scan_attr",__FILE__,__LINE__)
# 428 "iotk_attr.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.15 ")
# 428 "iotk_attr.spp"
call iotk_error_msg(ierrl,'Attribute not found')
# 428 "iotk_attr.spp"
call iotk_error_write(ierrl,"name",name)
    ierrl = - ierrl
  end if
  if(present(default) .and. .not. foundl) then
# 441 "iotk_attr.spp"
    val = default
# 443 "iotk_attr.spp"
  end if
  if(present(ierr)) then
    ierr = ierrl
  else
    if(ierrl/=0) call iotk_error_handler(ierrl)
  end if
end subroutine iotk_scan_attr_REAL4_3
# 451 "iotk_attr.spp"

#endif
#endif

subroutine iotk_attr_dummy_REAL4_3
  write(0,*)
end subroutine iotk_attr_dummy_REAL4_3

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

#ifdef __IOTK_REAL4
#if 4 <= __IOTK_MAXRANK

# 158 "iotk_attr.spp"

# 233 "iotk_attr.spp"

# 236 "iotk_attr.spp"
subroutine iotk_write_attr_REAL4_4(attr,name,val,dummy,first,ierr)
  use iotk_base
  use iotk_error_interf
  use iotk_attr_interf
  use iotk_str_interf
  use iotk_misc_interf
  implicit none
  character(*), intent(inout) :: attr
  character(*), intent(in)    :: name
  REAL(kind=__IOTK_REAL4), intent(in)  :: val (:,:,:,:)
  type(iotk_dummytype), optional :: dummy
  logical, optional, intent(in)  :: first
  integer, optional, intent(out) :: ierr
  integer :: ierrl
  integer :: attlen
  integer :: vallen
  integer :: namlen
  character :: delim
# 257 "iotk_attr.spp"
  character(iotk_vallenx) :: tmpval
  ierrl = 0
  if(present(first)) then
    if(first) attr(1:1) = iotk_eos
  end if
  attlen = iotk_strlen_trim(attr)
  namlen = iotk_strlen_trim(name)
  if(.not.iotk_check_name(name)) then
    call iotk_error_issue(ierrl,"iotk_write_attr",__FILE__,__LINE__)
# 265 "iotk_attr.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.15 ")
# 265 "iotk_attr.spp"
call iotk_error_msg(ierrl,'Wrong tag name')
# 265 "iotk_attr.spp"
call iotk_error_write(ierrl,"name",name(1:namlen))
    goto 1
  end if
# 282 "iotk_attr.spp"
  delim = '"'
# 286 "iotk_attr.spp"
  call iotk_write(pack(val,mask=.true.),tmpval,ierrl)
# 288 "iotk_attr.spp"
  if(ierrl/=0) then
    call iotk_error_issue(ierrl,"iotk_write_attr",__FILE__,__LINE__)
# 289 "iotk_attr.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.15 ")
    goto 1
  end if
# 293 "iotk_attr.spp"
  vallen = iotk_strlen(tmpval)
  if(attlen+vallen+namlen+5>len(attr)) then
    call iotk_error_issue(ierrl,"iotk_write_attr",__FILE__,__LINE__)
# 295 "iotk_attr.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.15 ")
# 295 "iotk_attr.spp"
call iotk_error_msg(ierrl,'Attribute dummy argument is too short')
    goto 1
  end if
  attr(attlen+1:attlen+vallen+namlen+5) = " "//name(1:namlen)//"="//delim//tmpval(1:vallen)//delim//iotk_eos
1 continue
  if(present(ierr)) then
    ierr = ierrl
  else
    if(ierrl/=0) call iotk_error_handler(ierrl)
  end if
end subroutine iotk_write_attr_REAL4_4

# 308 "iotk_attr.spp"
subroutine iotk_scan_attr_REAL4_4(attr,name,val,dummy,found,default,eos,ierr)
  use iotk_base
  use iotk_error_interf
  use iotk_attr_interf, only : iotk_read
  use iotk_str_interf
  use iotk_misc_interf
  implicit none
  character(*),             intent(in)  :: attr
  character(*),             intent(in)  :: name
#ifdef __IOTK_WORKAROUND6
  REAL(kind=__IOTK_REAL4)                        :: val (:,:,:,:)
#else
  REAL(kind=__IOTK_REAL4), intent(out)           :: val (:,:,:,:)
#endif
  type(iotk_dummytype), optional :: dummy
  logical,        optional, intent(out) :: found
  REAL(kind=__IOTK_REAL4), optional, intent(in)  :: default (:,:,:,:)
  logical,        optional, intent(in)  :: eos
  integer,        optional, intent(out) :: ierr
  integer :: ierrl
  integer :: attlen,pos,equal,namlen
  character :: delim
  logical :: foundl
  character(iotk_vallenx) :: valc
# 337 "iotk_attr.spp"
  integer :: index
  REAL(kind=__IOTK_REAL4), allocatable :: tmpval (:)
# 340 "iotk_attr.spp"
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
# 351 "iotk_attr.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.15 ")
# 351 "iotk_attr.spp"
call iotk_error_msg(ierrl,'')
# 351 "iotk_attr.spp"
call iotk_error_write(ierrl,"attr",attr(equal+1:attlen))
      goto 1
    end if
    equal = equal + pos
    if(trim(attr(equal-pos:equal-1))==name(1:namlen)) foundl = .true.
    pos = verify(attr(equal+1:attlen)," ")
    if(pos<=0) then
      call iotk_error_issue(ierrl,"iotk_scan_attr",__FILE__,__LINE__)
# 358 "iotk_attr.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.15 ")
      goto 1
    end if
    equal = equal + pos
    delim = attr(equal:equal)
    if(delim/="'" .and. delim/='"') then
      call iotk_error_issue(ierrl,"iotk_scan_attr",__FILE__,__LINE__)
# 364 "iotk_attr.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.15 ")
      goto 1
    end if
    pos = scan(attr(equal+1:attlen),delim)
    if(pos<=0) then
      call iotk_error_issue(ierrl,"iotk_scan_attr",__FILE__,__LINE__)
# 369 "iotk_attr.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.15 ")
      goto 1
    end if
    if(foundl) exit
    equal = equal + pos
  end do
  if(foundl) then
    call iotk_strcpy(valc,attr(equal+1:equal+pos-1),ierrl)
    if(ierrl/=0) then
      call iotk_error_issue(ierrl,"iotk_scan_attr",__FILE__,__LINE__)
# 378 "iotk_attr.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.15 ")
      goto 1
    end if
  else
    goto 1
  end if
# 401 "iotk_attr.spp"
  allocate(tmpval(size(val)))
  index = 0
  call iotk_str_clean(valc)
  call iotk_read(tmpval,valc(1:iotk_strlen(valc)),index,ierrl)
  if(ierrl/=0) then
    call iotk_error_issue(ierrl,"iotk_scan_attr",__FILE__,__LINE__)
# 406 "iotk_attr.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.15 ")
    goto 1
  end if
# 412 "iotk_attr.spp"
  if(index/=size(val)) then
# 414 "iotk_attr.spp"
    call iotk_error_issue(ierrl,"iotk_scan_attr",__FILE__,__LINE__)
# 414 "iotk_attr.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.15 ")
# 414 "iotk_attr.spp"
call iotk_error_msg(ierrl,'Attribute size does not match')
# 414 "iotk_attr.spp"
call iotk_error_write(ierrl,"attr",valc)
# 414 "iotk_attr.spp"
call iotk_error_write(ierrl,"size",size(tmpval))
    goto 1
  end if
# 420 "iotk_attr.spp"
  val = reshape (source=tmpval,shape=shape(val))
# 422 "iotk_attr.spp"
  deallocate(tmpval)
# 424 "iotk_attr.spp"
1 continue
  if(ierrl/=0) foundl=.false.
  if(present(found)) found = foundl
  if(ierrl==0 .and. .not. present(found) .and. .not. present(default) .and. .not. foundl) then
    call iotk_error_issue(ierrl,"iotk_scan_attr",__FILE__,__LINE__)
# 428 "iotk_attr.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.15 ")
# 428 "iotk_attr.spp"
call iotk_error_msg(ierrl,'Attribute not found')
# 428 "iotk_attr.spp"
call iotk_error_write(ierrl,"name",name)
    ierrl = - ierrl
  end if
  if(present(default) .and. .not. foundl) then
# 441 "iotk_attr.spp"
    val = default
# 443 "iotk_attr.spp"
  end if
  if(present(ierr)) then
    ierr = ierrl
  else
    if(ierrl/=0) call iotk_error_handler(ierrl)
  end if
end subroutine iotk_scan_attr_REAL4_4
# 451 "iotk_attr.spp"

#endif
#endif

subroutine iotk_attr_dummy_REAL4_4
  write(0,*)
end subroutine iotk_attr_dummy_REAL4_4

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

#ifdef __IOTK_REAL4
#if 5 <= __IOTK_MAXRANK

# 158 "iotk_attr.spp"

# 233 "iotk_attr.spp"

# 236 "iotk_attr.spp"
subroutine iotk_write_attr_REAL4_5(attr,name,val,dummy,first,ierr)
  use iotk_base
  use iotk_error_interf
  use iotk_attr_interf
  use iotk_str_interf
  use iotk_misc_interf
  implicit none
  character(*), intent(inout) :: attr
  character(*), intent(in)    :: name
  REAL(kind=__IOTK_REAL4), intent(in)  :: val (:,:,:,:,:)
  type(iotk_dummytype), optional :: dummy
  logical, optional, intent(in)  :: first
  integer, optional, intent(out) :: ierr
  integer :: ierrl
  integer :: attlen
  integer :: vallen
  integer :: namlen
  character :: delim
# 257 "iotk_attr.spp"
  character(iotk_vallenx) :: tmpval
  ierrl = 0
  if(present(first)) then
    if(first) attr(1:1) = iotk_eos
  end if
  attlen = iotk_strlen_trim(attr)
  namlen = iotk_strlen_trim(name)
  if(.not.iotk_check_name(name)) then
    call iotk_error_issue(ierrl,"iotk_write_attr",__FILE__,__LINE__)
# 265 "iotk_attr.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.15 ")
# 265 "iotk_attr.spp"
call iotk_error_msg(ierrl,'Wrong tag name')
# 265 "iotk_attr.spp"
call iotk_error_write(ierrl,"name",name(1:namlen))
    goto 1
  end if
# 282 "iotk_attr.spp"
  delim = '"'
# 286 "iotk_attr.spp"
  call iotk_write(pack(val,mask=.true.),tmpval,ierrl)
# 288 "iotk_attr.spp"
  if(ierrl/=0) then
    call iotk_error_issue(ierrl,"iotk_write_attr",__FILE__,__LINE__)
# 289 "iotk_attr.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.15 ")
    goto 1
  end if
# 293 "iotk_attr.spp"
  vallen = iotk_strlen(tmpval)
  if(attlen+vallen+namlen+5>len(attr)) then
    call iotk_error_issue(ierrl,"iotk_write_attr",__FILE__,__LINE__)
# 295 "iotk_attr.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.15 ")
# 295 "iotk_attr.spp"
call iotk_error_msg(ierrl,'Attribute dummy argument is too short')
    goto 1
  end if
  attr(attlen+1:attlen+vallen+namlen+5) = " "//name(1:namlen)//"="//delim//tmpval(1:vallen)//delim//iotk_eos
1 continue
  if(present(ierr)) then
    ierr = ierrl
  else
    if(ierrl/=0) call iotk_error_handler(ierrl)
  end if
end subroutine iotk_write_attr_REAL4_5

# 308 "iotk_attr.spp"
subroutine iotk_scan_attr_REAL4_5(attr,name,val,dummy,found,default,eos,ierr)
  use iotk_base
  use iotk_error_interf
  use iotk_attr_interf, only : iotk_read
  use iotk_str_interf
  use iotk_misc_interf
  implicit none
  character(*),             intent(in)  :: attr
  character(*),             intent(in)  :: name
#ifdef __IOTK_WORKAROUND6
  REAL(kind=__IOTK_REAL4)                        :: val (:,:,:,:,:)
#else
  REAL(kind=__IOTK_REAL4), intent(out)           :: val (:,:,:,:,:)
#endif
  type(iotk_dummytype), optional :: dummy
  logical,        optional, intent(out) :: found
  REAL(kind=__IOTK_REAL4), optional, intent(in)  :: default (:,:,:,:,:)
  logical,        optional, intent(in)  :: eos
  integer,        optional, intent(out) :: ierr
  integer :: ierrl
  integer :: attlen,pos,equal,namlen
  character :: delim
  logical :: foundl
  character(iotk_vallenx) :: valc
# 337 "iotk_attr.spp"
  integer :: index
  REAL(kind=__IOTK_REAL4), allocatable :: tmpval (:)
# 340 "iotk_attr.spp"
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
# 351 "iotk_attr.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.15 ")
# 351 "iotk_attr.spp"
call iotk_error_msg(ierrl,'')
# 351 "iotk_attr.spp"
call iotk_error_write(ierrl,"attr",attr(equal+1:attlen))
      goto 1
    end if
    equal = equal + pos
    if(trim(attr(equal-pos:equal-1))==name(1:namlen)) foundl = .true.
    pos = verify(attr(equal+1:attlen)," ")
    if(pos<=0) then
      call iotk_error_issue(ierrl,"iotk_scan_attr",__FILE__,__LINE__)
# 358 "iotk_attr.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.15 ")
      goto 1
    end if
    equal = equal + pos
    delim = attr(equal:equal)
    if(delim/="'" .and. delim/='"') then
      call iotk_error_issue(ierrl,"iotk_scan_attr",__FILE__,__LINE__)
# 364 "iotk_attr.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.15 ")
      goto 1
    end if
    pos = scan(attr(equal+1:attlen),delim)
    if(pos<=0) then
      call iotk_error_issue(ierrl,"iotk_scan_attr",__FILE__,__LINE__)
# 369 "iotk_attr.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.15 ")
      goto 1
    end if
    if(foundl) exit
    equal = equal + pos
  end do
  if(foundl) then
    call iotk_strcpy(valc,attr(equal+1:equal+pos-1),ierrl)
    if(ierrl/=0) then
      call iotk_error_issue(ierrl,"iotk_scan_attr",__FILE__,__LINE__)
# 378 "iotk_attr.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.15 ")
      goto 1
    end if
  else
    goto 1
  end if
# 401 "iotk_attr.spp"
  allocate(tmpval(size(val)))
  index = 0
  call iotk_str_clean(valc)
  call iotk_read(tmpval,valc(1:iotk_strlen(valc)),index,ierrl)
  if(ierrl/=0) then
    call iotk_error_issue(ierrl,"iotk_scan_attr",__FILE__,__LINE__)
# 406 "iotk_attr.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.15 ")
    goto 1
  end if
# 412 "iotk_attr.spp"
  if(index/=size(val)) then
# 414 "iotk_attr.spp"
    call iotk_error_issue(ierrl,"iotk_scan_attr",__FILE__,__LINE__)
# 414 "iotk_attr.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.15 ")
# 414 "iotk_attr.spp"
call iotk_error_msg(ierrl,'Attribute size does not match')
# 414 "iotk_attr.spp"
call iotk_error_write(ierrl,"attr",valc)
# 414 "iotk_attr.spp"
call iotk_error_write(ierrl,"size",size(tmpval))
    goto 1
  end if
# 420 "iotk_attr.spp"
  val = reshape (source=tmpval,shape=shape(val))
# 422 "iotk_attr.spp"
  deallocate(tmpval)
# 424 "iotk_attr.spp"
1 continue
  if(ierrl/=0) foundl=.false.
  if(present(found)) found = foundl
  if(ierrl==0 .and. .not. present(found) .and. .not. present(default) .and. .not. foundl) then
    call iotk_error_issue(ierrl,"iotk_scan_attr",__FILE__,__LINE__)
# 428 "iotk_attr.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.15 ")
# 428 "iotk_attr.spp"
call iotk_error_msg(ierrl,'Attribute not found')
# 428 "iotk_attr.spp"
call iotk_error_write(ierrl,"name",name)
    ierrl = - ierrl
  end if
  if(present(default) .and. .not. foundl) then
# 441 "iotk_attr.spp"
    val = default
# 443 "iotk_attr.spp"
  end if
  if(present(ierr)) then
    ierr = ierrl
  else
    if(ierrl/=0) call iotk_error_handler(ierrl)
  end if
end subroutine iotk_scan_attr_REAL4_5
# 451 "iotk_attr.spp"

#endif
#endif

subroutine iotk_attr_dummy_REAL4_5
  write(0,*)
end subroutine iotk_attr_dummy_REAL4_5

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

#ifdef __IOTK_REAL4
#if 6 <= __IOTK_MAXRANK

# 158 "iotk_attr.spp"

# 233 "iotk_attr.spp"

# 236 "iotk_attr.spp"
subroutine iotk_write_attr_REAL4_6(attr,name,val,dummy,first,ierr)
  use iotk_base
  use iotk_error_interf
  use iotk_attr_interf
  use iotk_str_interf
  use iotk_misc_interf
  implicit none
  character(*), intent(inout) :: attr
  character(*), intent(in)    :: name
  REAL(kind=__IOTK_REAL4), intent(in)  :: val (:,:,:,:,:,:)
  type(iotk_dummytype), optional :: dummy
  logical, optional, intent(in)  :: first
  integer, optional, intent(out) :: ierr
  integer :: ierrl
  integer :: attlen
  integer :: vallen
  integer :: namlen
  character :: delim
# 257 "iotk_attr.spp"
  character(iotk_vallenx) :: tmpval
  ierrl = 0
  if(present(first)) then
    if(first) attr(1:1) = iotk_eos
  end if
  attlen = iotk_strlen_trim(attr)
  namlen = iotk_strlen_trim(name)
  if(.not.iotk_check_name(name)) then
    call iotk_error_issue(ierrl,"iotk_write_attr",__FILE__,__LINE__)
# 265 "iotk_attr.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.15 ")
# 265 "iotk_attr.spp"
call iotk_error_msg(ierrl,'Wrong tag name')
# 265 "iotk_attr.spp"
call iotk_error_write(ierrl,"name",name(1:namlen))
    goto 1
  end if
# 282 "iotk_attr.spp"
  delim = '"'
# 286 "iotk_attr.spp"
  call iotk_write(pack(val,mask=.true.),tmpval,ierrl)
# 288 "iotk_attr.spp"
  if(ierrl/=0) then
    call iotk_error_issue(ierrl,"iotk_write_attr",__FILE__,__LINE__)
# 289 "iotk_attr.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.15 ")
    goto 1
  end if
# 293 "iotk_attr.spp"
  vallen = iotk_strlen(tmpval)
  if(attlen+vallen+namlen+5>len(attr)) then
    call iotk_error_issue(ierrl,"iotk_write_attr",__FILE__,__LINE__)
# 295 "iotk_attr.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.15 ")
# 295 "iotk_attr.spp"
call iotk_error_msg(ierrl,'Attribute dummy argument is too short')
    goto 1
  end if
  attr(attlen+1:attlen+vallen+namlen+5) = " "//name(1:namlen)//"="//delim//tmpval(1:vallen)//delim//iotk_eos
1 continue
  if(present(ierr)) then
    ierr = ierrl
  else
    if(ierrl/=0) call iotk_error_handler(ierrl)
  end if
end subroutine iotk_write_attr_REAL4_6

# 308 "iotk_attr.spp"
subroutine iotk_scan_attr_REAL4_6(attr,name,val,dummy,found,default,eos,ierr)
  use iotk_base
  use iotk_error_interf
  use iotk_attr_interf, only : iotk_read
  use iotk_str_interf
  use iotk_misc_interf
  implicit none
  character(*),             intent(in)  :: attr
  character(*),             intent(in)  :: name
#ifdef __IOTK_WORKAROUND6
  REAL(kind=__IOTK_REAL4)                        :: val (:,:,:,:,:,:)
#else
  REAL(kind=__IOTK_REAL4), intent(out)           :: val (:,:,:,:,:,:)
#endif
  type(iotk_dummytype), optional :: dummy
  logical,        optional, intent(out) :: found
  REAL(kind=__IOTK_REAL4), optional, intent(in)  :: default (:,:,:,:,:,:)
  logical,        optional, intent(in)  :: eos
  integer,        optional, intent(out) :: ierr
  integer :: ierrl
  integer :: attlen,pos,equal,namlen
  character :: delim
  logical :: foundl
  character(iotk_vallenx) :: valc
# 337 "iotk_attr.spp"
  integer :: index
  REAL(kind=__IOTK_REAL4), allocatable :: tmpval (:)
# 340 "iotk_attr.spp"
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
# 351 "iotk_attr.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.15 ")
# 351 "iotk_attr.spp"
call iotk_error_msg(ierrl,'')
# 351 "iotk_attr.spp"
call iotk_error_write(ierrl,"attr",attr(equal+1:attlen))
      goto 1
    end if
    equal = equal + pos
    if(trim(attr(equal-pos:equal-1))==name(1:namlen)) foundl = .true.
    pos = verify(attr(equal+1:attlen)," ")
    if(pos<=0) then
      call iotk_error_issue(ierrl,"iotk_scan_attr",__FILE__,__LINE__)
# 358 "iotk_attr.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.15 ")
      goto 1
    end if
    equal = equal + pos
    delim = attr(equal:equal)
    if(delim/="'" .and. delim/='"') then
      call iotk_error_issue(ierrl,"iotk_scan_attr",__FILE__,__LINE__)
# 364 "iotk_attr.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.15 ")
      goto 1
    end if
    pos = scan(attr(equal+1:attlen),delim)
    if(pos<=0) then
      call iotk_error_issue(ierrl,"iotk_scan_attr",__FILE__,__LINE__)
# 369 "iotk_attr.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.15 ")
      goto 1
    end if
    if(foundl) exit
    equal = equal + pos
  end do
  if(foundl) then
    call iotk_strcpy(valc,attr(equal+1:equal+pos-1),ierrl)
    if(ierrl/=0) then
      call iotk_error_issue(ierrl,"iotk_scan_attr",__FILE__,__LINE__)
# 378 "iotk_attr.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.15 ")
      goto 1
    end if
  else
    goto 1
  end if
# 401 "iotk_attr.spp"
  allocate(tmpval(size(val)))
  index = 0
  call iotk_str_clean(valc)
  call iotk_read(tmpval,valc(1:iotk_strlen(valc)),index,ierrl)
  if(ierrl/=0) then
    call iotk_error_issue(ierrl,"iotk_scan_attr",__FILE__,__LINE__)
# 406 "iotk_attr.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.15 ")
    goto 1
  end if
# 412 "iotk_attr.spp"
  if(index/=size(val)) then
# 414 "iotk_attr.spp"
    call iotk_error_issue(ierrl,"iotk_scan_attr",__FILE__,__LINE__)
# 414 "iotk_attr.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.15 ")
# 414 "iotk_attr.spp"
call iotk_error_msg(ierrl,'Attribute size does not match')
# 414 "iotk_attr.spp"
call iotk_error_write(ierrl,"attr",valc)
# 414 "iotk_attr.spp"
call iotk_error_write(ierrl,"size",size(tmpval))
    goto 1
  end if
# 420 "iotk_attr.spp"
  val = reshape (source=tmpval,shape=shape(val))
# 422 "iotk_attr.spp"
  deallocate(tmpval)
# 424 "iotk_attr.spp"
1 continue
  if(ierrl/=0) foundl=.false.
  if(present(found)) found = foundl
  if(ierrl==0 .and. .not. present(found) .and. .not. present(default) .and. .not. foundl) then
    call iotk_error_issue(ierrl,"iotk_scan_attr",__FILE__,__LINE__)
# 428 "iotk_attr.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.15 ")
# 428 "iotk_attr.spp"
call iotk_error_msg(ierrl,'Attribute not found')
# 428 "iotk_attr.spp"
call iotk_error_write(ierrl,"name",name)
    ierrl = - ierrl
  end if
  if(present(default) .and. .not. foundl) then
# 441 "iotk_attr.spp"
    val = default
# 443 "iotk_attr.spp"
  end if
  if(present(ierr)) then
    ierr = ierrl
  else
    if(ierrl/=0) call iotk_error_handler(ierrl)
  end if
end subroutine iotk_scan_attr_REAL4_6
# 451 "iotk_attr.spp"

#endif
#endif

subroutine iotk_attr_dummy_REAL4_6
  write(0,*)
end subroutine iotk_attr_dummy_REAL4_6

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

#ifdef __IOTK_REAL4
#if 7 <= __IOTK_MAXRANK

# 158 "iotk_attr.spp"

# 233 "iotk_attr.spp"

# 236 "iotk_attr.spp"
subroutine iotk_write_attr_REAL4_7(attr,name,val,dummy,first,ierr)
  use iotk_base
  use iotk_error_interf
  use iotk_attr_interf
  use iotk_str_interf
  use iotk_misc_interf
  implicit none
  character(*), intent(inout) :: attr
  character(*), intent(in)    :: name
  REAL(kind=__IOTK_REAL4), intent(in)  :: val (:,:,:,:,:,:,:)
  type(iotk_dummytype), optional :: dummy
  logical, optional, intent(in)  :: first
  integer, optional, intent(out) :: ierr
  integer :: ierrl
  integer :: attlen
  integer :: vallen
  integer :: namlen
  character :: delim
# 257 "iotk_attr.spp"
  character(iotk_vallenx) :: tmpval
  ierrl = 0
  if(present(first)) then
    if(first) attr(1:1) = iotk_eos
  end if
  attlen = iotk_strlen_trim(attr)
  namlen = iotk_strlen_trim(name)
  if(.not.iotk_check_name(name)) then
    call iotk_error_issue(ierrl,"iotk_write_attr",__FILE__,__LINE__)
# 265 "iotk_attr.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.15 ")
# 265 "iotk_attr.spp"
call iotk_error_msg(ierrl,'Wrong tag name')
# 265 "iotk_attr.spp"
call iotk_error_write(ierrl,"name",name(1:namlen))
    goto 1
  end if
# 282 "iotk_attr.spp"
  delim = '"'
# 286 "iotk_attr.spp"
  call iotk_write(pack(val,mask=.true.),tmpval,ierrl)
# 288 "iotk_attr.spp"
  if(ierrl/=0) then
    call iotk_error_issue(ierrl,"iotk_write_attr",__FILE__,__LINE__)
# 289 "iotk_attr.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.15 ")
    goto 1
  end if
# 293 "iotk_attr.spp"
  vallen = iotk_strlen(tmpval)
  if(attlen+vallen+namlen+5>len(attr)) then
    call iotk_error_issue(ierrl,"iotk_write_attr",__FILE__,__LINE__)
# 295 "iotk_attr.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.15 ")
# 295 "iotk_attr.spp"
call iotk_error_msg(ierrl,'Attribute dummy argument is too short')
    goto 1
  end if
  attr(attlen+1:attlen+vallen+namlen+5) = " "//name(1:namlen)//"="//delim//tmpval(1:vallen)//delim//iotk_eos
1 continue
  if(present(ierr)) then
    ierr = ierrl
  else
    if(ierrl/=0) call iotk_error_handler(ierrl)
  end if
end subroutine iotk_write_attr_REAL4_7

# 308 "iotk_attr.spp"
subroutine iotk_scan_attr_REAL4_7(attr,name,val,dummy,found,default,eos,ierr)
  use iotk_base
  use iotk_error_interf
  use iotk_attr_interf, only : iotk_read
  use iotk_str_interf
  use iotk_misc_interf
  implicit none
  character(*),             intent(in)  :: attr
  character(*),             intent(in)  :: name
#ifdef __IOTK_WORKAROUND6
  REAL(kind=__IOTK_REAL4)                        :: val (:,:,:,:,:,:,:)
#else
  REAL(kind=__IOTK_REAL4), intent(out)           :: val (:,:,:,:,:,:,:)
#endif
  type(iotk_dummytype), optional :: dummy
  logical,        optional, intent(out) :: found
  REAL(kind=__IOTK_REAL4), optional, intent(in)  :: default (:,:,:,:,:,:,:)
  logical,        optional, intent(in)  :: eos
  integer,        optional, intent(out) :: ierr
  integer :: ierrl
  integer :: attlen,pos,equal,namlen
  character :: delim
  logical :: foundl
  character(iotk_vallenx) :: valc
# 337 "iotk_attr.spp"
  integer :: index
  REAL(kind=__IOTK_REAL4), allocatable :: tmpval (:)
# 340 "iotk_attr.spp"
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
# 351 "iotk_attr.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.15 ")
# 351 "iotk_attr.spp"
call iotk_error_msg(ierrl,'')
# 351 "iotk_attr.spp"
call iotk_error_write(ierrl,"attr",attr(equal+1:attlen))
      goto 1
    end if
    equal = equal + pos
    if(trim(attr(equal-pos:equal-1))==name(1:namlen)) foundl = .true.
    pos = verify(attr(equal+1:attlen)," ")
    if(pos<=0) then
      call iotk_error_issue(ierrl,"iotk_scan_attr",__FILE__,__LINE__)
# 358 "iotk_attr.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.15 ")
      goto 1
    end if
    equal = equal + pos
    delim = attr(equal:equal)
    if(delim/="'" .and. delim/='"') then
      call iotk_error_issue(ierrl,"iotk_scan_attr",__FILE__,__LINE__)
# 364 "iotk_attr.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.15 ")
      goto 1
    end if
    pos = scan(attr(equal+1:attlen),delim)
    if(pos<=0) then
      call iotk_error_issue(ierrl,"iotk_scan_attr",__FILE__,__LINE__)
# 369 "iotk_attr.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.15 ")
      goto 1
    end if
    if(foundl) exit
    equal = equal + pos
  end do
  if(foundl) then
    call iotk_strcpy(valc,attr(equal+1:equal+pos-1),ierrl)
    if(ierrl/=0) then
      call iotk_error_issue(ierrl,"iotk_scan_attr",__FILE__,__LINE__)
# 378 "iotk_attr.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.15 ")
      goto 1
    end if
  else
    goto 1
  end if
# 401 "iotk_attr.spp"
  allocate(tmpval(size(val)))
  index = 0
  call iotk_str_clean(valc)
  call iotk_read(tmpval,valc(1:iotk_strlen(valc)),index,ierrl)
  if(ierrl/=0) then
    call iotk_error_issue(ierrl,"iotk_scan_attr",__FILE__,__LINE__)
# 406 "iotk_attr.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.15 ")
    goto 1
  end if
# 412 "iotk_attr.spp"
  if(index/=size(val)) then
# 414 "iotk_attr.spp"
    call iotk_error_issue(ierrl,"iotk_scan_attr",__FILE__,__LINE__)
# 414 "iotk_attr.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.15 ")
# 414 "iotk_attr.spp"
call iotk_error_msg(ierrl,'Attribute size does not match')
# 414 "iotk_attr.spp"
call iotk_error_write(ierrl,"attr",valc)
# 414 "iotk_attr.spp"
call iotk_error_write(ierrl,"size",size(tmpval))
    goto 1
  end if
# 420 "iotk_attr.spp"
  val = reshape (source=tmpval,shape=shape(val))
# 422 "iotk_attr.spp"
  deallocate(tmpval)
# 424 "iotk_attr.spp"
1 continue
  if(ierrl/=0) foundl=.false.
  if(present(found)) found = foundl
  if(ierrl==0 .and. .not. present(found) .and. .not. present(default) .and. .not. foundl) then
    call iotk_error_issue(ierrl,"iotk_scan_attr",__FILE__,__LINE__)
# 428 "iotk_attr.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.15 ")
# 428 "iotk_attr.spp"
call iotk_error_msg(ierrl,'Attribute not found')
# 428 "iotk_attr.spp"
call iotk_error_write(ierrl,"name",name)
    ierrl = - ierrl
  end if
  if(present(default) .and. .not. foundl) then
# 441 "iotk_attr.spp"
    val = default
# 443 "iotk_attr.spp"
  end if
  if(present(ierr)) then
    ierr = ierrl
  else
    if(ierrl/=0) call iotk_error_handler(ierrl)
  end if
end subroutine iotk_scan_attr_REAL4_7
# 451 "iotk_attr.spp"

#endif
#endif

subroutine iotk_attr_dummy_REAL4_7
  write(0,*)
end subroutine iotk_attr_dummy_REAL4_7

# 45 "iotk_attr.spp"

