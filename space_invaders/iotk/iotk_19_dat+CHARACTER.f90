# 48 "iotk_dat.spp"
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
# 65 "iotk_dat.spp"


!------------------------------------------------------------------------------!
! Inclusion of configuration file
#include "iotk_config.h"
!------------------------------------------------------------------------------!

# 74 "iotk_dat.spp"
#include "iotk_auxmacros.h"
# 76 "iotk_dat.spp"

# 78 "iotk_dat.spp"

#ifdef __IOTK_CHARACTER1
#if 0 <= __IOTK_MAXRANK
# 82 "iotk_dat.spp"
subroutine iotk_write_dat_CHARACTER1_0(unit,name,dat,dummy,attr,columns,sep,fmt,ierr)
  use iotk_base
  use iotk_error_interf
  use iotk_attr_interf, only : iotk_write_attr
  use iotk_write_interf
  use iotk_fmt_interf
  use iotk_str_interf
  use iotk_unit_interf
  use iotk_misc_interf
  implicit none
  integer,                intent(in)  :: unit
  character(*),           intent(in)  :: name
  CHARACTER (kind=__IOTK_CHARACTER1,len=*), intent(in)  :: dat  
  type(iotk_dummytype), optional      :: dummy
  character(len=*), optional, intent(in)  :: attr
  integer,          optional, intent(in)  :: columns
  character(len=*), optional, intent(in)  :: sep
  character(len=*), optional, intent(in)  :: fmt
  integer,      optional, intent(out) :: ierr
  integer :: ierrl,lunit,iostat
  logical :: binary,raw
  integer :: lcolumns
  integer(iotk_header_kind), parameter :: idummy=0
  character(100) :: lsep
  character(300) :: usefmt,usefmt1
  character(iotk_attlenx) :: lattr
  character(iotk_attlenx) :: attr_tmp
  type (iotk_unit), pointer :: this
# 111 "iotk_dat.spp"
  CHARACTER (kind=__IOTK_CHARACTER1,len=len(dat)),allocatable :: dattmp(:)
  character(len=iotk_linlenx) :: linetmp
# 116 "iotk_dat.spp"
  integer :: itmp
  ierrl = 0
  iostat = 0
  lcolumns = 1
  lsep(1:2) = " "//iotk_eos
  if(present(columns)) lcolumns = columns
  if(present(sep)) then
    call iotk_strcpy(lsep,sep,ierrl)
    if(ierrl/=0) then
      call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 125 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.19 ")
      goto 1
    end if
  end if
  lunit = iotk_phys_unit(unit)
  call iotk_unit_get(lunit,pointer=this)
  raw = .false.
  if(associated(this)) then
    raw = this%raw
  end if
  call iotk_inquire(lunit,binary=binary,ierr=ierrl)
  if(ierrl/=0) then
    call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 137 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.19 ")
    goto 1
  end if
  call iotk_strcpy(usefmt,"!",ierrl)
  if(ierrl/=0) then
    call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 142 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.19 ")
    goto 1
  end if
  if(present(fmt) .and. .not. raw) call iotk_strcpy(usefmt,iotk_strtrim(fmt),ierrl)
  if(ierrl/=0) then
    call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 147 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.19 ")
    goto 1
  end if
  if(iotk_strscan(usefmt,"<>&")/=0) then
    call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 151 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.19 ")
# 151 "iotk_dat.spp"
call iotk_error_msg(ierrl,'Special characters (<>&) found in fmt string')
# 151 "iotk_dat.spp"
call iotk_error_write(ierrl,"unit",unit)
# 151 "iotk_dat.spp"
call iotk_error_write(ierrl,"name",trim(name))
# 151 "iotk_dat.spp"
call iotk_error_write(ierrl,"fmt",trim(fmt))
    goto 1
  end if
  call iotk_write_attr(lattr,"type",iotk_tolower("CHARACTER"),first=.true.,ierr=ierrl)
  if(ierrl/=0) then
    call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 156 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.19 ")
    goto 1
  end if
  call iotk_write_attr(lattr,"size",1,ierr=ierrl)
  if(ierrl/=0) then
    call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 161 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.19 ")
    goto 1
  end if
# 165 "iotk_dat.spp"
  call iotk_write_attr(lattr,"len",len(dat),ierr=ierrl)
  if(ierrl/=0) then
    call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 167 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.19 ")
    goto 1
  end if
# 179 "iotk_dat.spp"
  if(.not.iotk_strcomp(usefmt,"!")) call iotk_write_attr(lattr,"fmt",iotk_strtrim(usefmt),ierr=ierrl)
  if(ierrl/=0) then
    call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 181 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.19 ")
    goto 1
  end if
  if(lcolumns/=1) call iotk_write_attr(lattr,"columns",lcolumns,ierr=ierrl)
  if(ierrl/=0) then
    call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 186 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.19 ")
    goto 1
  end if
  if(present(attr)) then
    attr_tmp(1:1)=iotk_eos
    call iotk_strcpy(attr_tmp,attr,ierr=ierrl)
    if(ierrl/=0) then
      call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 193 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.19 ")
      goto 1
    end if
    call iotk_delete_attr(attr_tmp,"type",ierrl)
    if(ierrl/=0) then
      call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 198 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.19 ")
      goto 1
    end if
    call iotk_delete_attr(attr_tmp,"kind",ierrl)
    if(ierrl/=0) then
      call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 203 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.19 ")
      goto 1
    end if
    call iotk_delete_attr(attr_tmp,"size",ierrl)
    if(ierrl/=0) then
      call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 208 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.19 ")
      goto 1
    end if
    call iotk_delete_attr(attr_tmp,"fmt",ierrl)
    if(ierrl/=0) then
      call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 213 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.19 ")
      goto 1
    end if
    call iotk_delete_attr(attr_tmp,"columns",ierrl)
    if(ierrl/=0) then
      call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 218 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.19 ")
      goto 1
    end if
    call iotk_delete_attr(attr_tmp,"len",ierrl)
    if(ierrl/=0) then
      call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 223 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.19 ")
      goto 1
    end if
    if(iotk_strlen_trim(attr_tmp)>0) call iotk_strcat(lattr,iotk_strtrim(attr_tmp),ierr=ierrl)
  end if
  if(ierrl/=0) then
    call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 229 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.19 ")
    goto 1
  end if
  call iotk_write_begin(unit,name,lattr,ierr=ierrl)
  if(ierrl/=0) then
    call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 234 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.19 ")
    goto 1
  end if

  allocate(dattmp(1))
# 240 "iotk_dat.spp"
     dattmp(1) = dat
# 252 "iotk_dat.spp"

  if(binary) then
    if(raw) then
      write(lunit,iostat=iostat) (dattmp(itmp),itmp=1,size(dattmp))
      if(iostat/=0) then
        call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 257 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.19 ")
        goto 1
      end if
    else
      write(lunit,iostat=iostat) idummy,(dattmp(itmp),itmp=1,size(dattmp))
      if(iostat/=0) then
        call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 263 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.19 ")
        goto 1
      end if
    end if
  else
    if(raw) then
# 270 "iotk_dat.spp"
      write(lunit,"(a)",iostat=iostat) (dattmp(itmp),itmp=1,size(dattmp))
# 274 "iotk_dat.spp"
      if(iostat/=0) then
        call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 275 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.19 ")
        goto 1
      end if
    else if(iotk_strcomp(usefmt,"*")) then
      write(lunit,*,iostat=iostat) (dattmp(itmp),itmp=1,size(dattmp))
      if(iostat/=0) then
        call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 281 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.19 ")
        goto 1
      end if
    else if(iotk_strcomp(usefmt,"!")) then
# 286 "iotk_dat.spp"
     do itmp = 1 , size(dattmp)
       call iotk_deescape(linetmp,dattmp(itmp))
       write(lunit,"(a)",iostat=iostat) linetmp(1:iotk_strlen(linetmp))
       if(iostat/=0) then
        call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 290 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.19 ")
        goto 1
        end if
     end do
# 301 "iotk_dat.spp"
    else
      write(lunit,fmt=usefmt(1:iotk_strlen(usefmt)),iostat=iostat) (dattmp(itmp),itmp=1,size(dattmp))
      if(iostat/=0) then
        call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 304 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.19 ")
        goto 1
      end if
    end if
  end if
  call iotk_write_end(unit,name,ierr=ierrl)
  if(ierrl/=0) then
    call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 311 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.19 ")
    goto 1
  end if
1 continue
  if(allocated(dattmp)) deallocate(dattmp)
  if(present(ierr)) then
    ierr = ierrl
  else
    if(ierrl/=0) call iotk_error_handler(ierrl)
  end if
end subroutine iotk_write_dat_CHARACTER1_0


# 629 "iotk_dat.spp"

# 631 "iotk_dat.spp"
subroutine iotk_scan_dat_CHARACTER1_0(unit,name,dat,dummy,attr,found,default,ierr)
  use iotk_base
  use iotk_error_interf
  use iotk_dat_interf, only: iotk_scan_dat_aux
  use iotk_scan_interf
  use iotk_str_interf
  use iotk_misc_interf
  implicit none
  integer,                   intent(in)  :: unit
  character(*),              intent(in)  :: name
#ifdef __IOTK_WORKAROUND6
  CHARACTER (kind=__IOTK_CHARACTER1,len=*)                        :: dat 
#else
  CHARACTER (kind=__IOTK_CHARACTER1,len=*),           intent(out) :: dat 
#endif
  type(iotk_dummytype), optional         :: dummy
#ifdef __IOTK_WORKAROUND6
  character(len=*),optional              :: attr
#else
  character(len=*),optional, intent(out) :: attr
#endif
  logical,         optional, intent(out) :: found
  CHARACTER (kind=__IOTK_CHARACTER1,len=*), optional, intent(in)  :: default 
  integer,         optional, intent(out) :: ierr
# 656 "iotk_dat.spp"
  CHARACTER (kind=__IOTK_CHARACTER1,len=len(dat)), allocatable :: tmpdat(:)
# 660 "iotk_dat.spp"
  integer :: ierrl,ierrl2
  integer :: rkind,rsize,rlen
  character(iotk_vallenx) :: rtype
  character(iotk_vallenx) :: fmt
  character(iotk_attlenx) :: lattr
  integer :: columns
  logical :: inside,foundl
  inside = .false.
  ierrl = 0
  ierrl2 = 0
  foundl=.false.
  call iotk_scan_begin(unit,name,lattr,found=foundl,ierr=ierrl)
  if(.not. foundl) goto 1
  foundl = .true.
  inside = .true.
  if(present(attr)) call iotk_strcpy(attr,lattr,ierrl)
  if(ierrl/=0) then
    call iotk_error_issue(ierrl,"iotk_scan_dat",__FILE__,__LINE__)
# 677 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.19 ")
    goto 1
  end if
  call iotk_parse_dat(lattr,rtype,rkind,rsize,rlen,fmt,columns,ierrl)
! Note that columns is not effectively used
  if(ierrl/=0) goto 1
  if(.not. (iotk_strcomp(rtype,iotk_eos) .or. iotk_strcomp(rtype,"CHARACTER") ) ) then
    call iotk_error_issue(ierrl,"iotk_scan_dat",__FILE__,__LINE__)
# 684 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.19 ")
# 684 "iotk_dat.spp"
call iotk_error_msg(ierrl,' ')
# 684 "iotk_dat.spp"
call iotk_error_write(ierrl,"rtype",rtype(1:iotk_strlen(rtype)))
# 684 "iotk_dat.spp"
call iotk_error_write(ierrl,"type","CHARACTER")
    goto 1
  end if
  if(.not. (rsize==-1 .or. rsize==1) ) then
    call iotk_error_issue(ierrl,"iotk_scan_dat",__FILE__,__LINE__)
# 688 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.19 ")
    goto 1
  end if
  if(rkind==-1) rkind = kind(dat)
# 693 "iotk_dat.spp"
  if(rlen ==-1) rlen  = len(dat)
# 695 "iotk_dat.spp"

  allocate(tmpdat(1))
  call iotk_scan_dat_aux(unit,tmpdat,rkind,rlen,fmt(1:iotk_strlen(fmt)),ierrl)
# 699 "iotk_dat.spp"
     dat = tmpdat(1)
# 711 "iotk_dat.spp"
  deallocate(tmpdat)
1 continue
  if(inside) then
    call iotk_scan_end(unit,name,ierr=ierrl2)
    if(ierrl2/=0) then
      call iotk_error_clear(ierrl)
      ierrl=ierrl2
    end if
  end if
  if(ierrl/=0) foundl=.false.
  if(present(found)) found = foundl
  if(ierrl==0 .and. .not. present(found) .and. .not. present(default) .and. .not. foundl) then
    call iotk_error_issue(ierrl,"iotk_scan_dat",__FILE__,__LINE__)
# 723 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.19 ")
# 723 "iotk_dat.spp"
call iotk_error_msg(ierrl,'Dat not found')
# 723 "iotk_dat.spp"
call iotk_error_write(ierrl,"name",name)
    ierrl = - ierrl
  end if 
  if(present(default) .and. .not. foundl) then
    dat=default
  end if
  if(present(ierr)) then
    ierr = ierrl
  else
    if(ierrl>0 .or. (.not.present(found) .and. .not.present(default))) call iotk_error_handler(ierrl)
  end if
end subroutine iotk_scan_dat_CHARACTER1_0


#endif
#endif

subroutine iotk_dat_dummy_CHARACTER1_0
  write(0,*)
end subroutine iotk_dat_dummy_CHARACTER1_0


# 45 "iotk_dat.spp"

# 65 "iotk_dat.spp"


!------------------------------------------------------------------------------!
! Inclusion of configuration file
#include "iotk_config.h"
!------------------------------------------------------------------------------!

# 74 "iotk_dat.spp"
#include "iotk_auxmacros.h"
# 76 "iotk_dat.spp"

# 78 "iotk_dat.spp"

#ifdef __IOTK_CHARACTER1
#if 1 <= __IOTK_MAXRANK
# 82 "iotk_dat.spp"
subroutine iotk_write_dat_CHARACTER1_1(unit,name,dat,dummy,attr,columns,sep,fmt,ierr)
  use iotk_base
  use iotk_error_interf
  use iotk_attr_interf, only : iotk_write_attr
  use iotk_write_interf
  use iotk_fmt_interf
  use iotk_str_interf
  use iotk_unit_interf
  use iotk_misc_interf
  implicit none
  integer,                intent(in)  :: unit
  character(*),           intent(in)  :: name
  CHARACTER (kind=__IOTK_CHARACTER1,len=*), intent(in)  :: dat (:) 
  type(iotk_dummytype), optional      :: dummy
  character(len=*), optional, intent(in)  :: attr
  integer,          optional, intent(in)  :: columns
  character(len=*), optional, intent(in)  :: sep
  character(len=*), optional, intent(in)  :: fmt
  integer,      optional, intent(out) :: ierr
  integer :: ierrl,lunit,iostat
  logical :: binary,raw
  integer :: lcolumns
  integer(iotk_header_kind), parameter :: idummy=0
  character(100) :: lsep
  character(300) :: usefmt,usefmt1
  character(iotk_attlenx) :: lattr
  character(iotk_attlenx) :: attr_tmp
  type (iotk_unit), pointer :: this
# 111 "iotk_dat.spp"
  CHARACTER (kind=__IOTK_CHARACTER1,len=len(dat)),allocatable :: dattmp(:)
  character(len=iotk_linlenx) :: linetmp
# 116 "iotk_dat.spp"
  integer :: itmp
  ierrl = 0
  iostat = 0
  lcolumns = 1
  lsep(1:2) = " "//iotk_eos
  if(present(columns)) lcolumns = columns
  if(present(sep)) then
    call iotk_strcpy(lsep,sep,ierrl)
    if(ierrl/=0) then
      call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 125 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.19 ")
      goto 1
    end if
  end if
  lunit = iotk_phys_unit(unit)
  call iotk_unit_get(lunit,pointer=this)
  raw = .false.
  if(associated(this)) then
    raw = this%raw
  end if
  call iotk_inquire(lunit,binary=binary,ierr=ierrl)
  if(ierrl/=0) then
    call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 137 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.19 ")
    goto 1
  end if
  call iotk_strcpy(usefmt,"!",ierrl)
  if(ierrl/=0) then
    call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 142 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.19 ")
    goto 1
  end if
  if(present(fmt) .and. .not. raw) call iotk_strcpy(usefmt,iotk_strtrim(fmt),ierrl)
  if(ierrl/=0) then
    call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 147 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.19 ")
    goto 1
  end if
  if(iotk_strscan(usefmt,"<>&")/=0) then
    call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 151 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.19 ")
# 151 "iotk_dat.spp"
call iotk_error_msg(ierrl,'Special characters (<>&) found in fmt string')
# 151 "iotk_dat.spp"
call iotk_error_write(ierrl,"unit",unit)
# 151 "iotk_dat.spp"
call iotk_error_write(ierrl,"name",trim(name))
# 151 "iotk_dat.spp"
call iotk_error_write(ierrl,"fmt",trim(fmt))
    goto 1
  end if
  call iotk_write_attr(lattr,"type",iotk_tolower("CHARACTER"),first=.true.,ierr=ierrl)
  if(ierrl/=0) then
    call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 156 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.19 ")
    goto 1
  end if
  call iotk_write_attr(lattr,"size",size(dat),ierr=ierrl)
  if(ierrl/=0) then
    call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 161 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.19 ")
    goto 1
  end if
# 165 "iotk_dat.spp"
  call iotk_write_attr(lattr,"len",len(dat),ierr=ierrl)
  if(ierrl/=0) then
    call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 167 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.19 ")
    goto 1
  end if
# 179 "iotk_dat.spp"
  if(.not.iotk_strcomp(usefmt,"!")) call iotk_write_attr(lattr,"fmt",iotk_strtrim(usefmt),ierr=ierrl)
  if(ierrl/=0) then
    call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 181 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.19 ")
    goto 1
  end if
  if(lcolumns/=1) call iotk_write_attr(lattr,"columns",lcolumns,ierr=ierrl)
  if(ierrl/=0) then
    call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 186 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.19 ")
    goto 1
  end if
  if(present(attr)) then
    attr_tmp(1:1)=iotk_eos
    call iotk_strcpy(attr_tmp,attr,ierr=ierrl)
    if(ierrl/=0) then
      call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 193 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.19 ")
      goto 1
    end if
    call iotk_delete_attr(attr_tmp,"type",ierrl)
    if(ierrl/=0) then
      call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 198 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.19 ")
      goto 1
    end if
    call iotk_delete_attr(attr_tmp,"kind",ierrl)
    if(ierrl/=0) then
      call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 203 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.19 ")
      goto 1
    end if
    call iotk_delete_attr(attr_tmp,"size",ierrl)
    if(ierrl/=0) then
      call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 208 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.19 ")
      goto 1
    end if
    call iotk_delete_attr(attr_tmp,"fmt",ierrl)
    if(ierrl/=0) then
      call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 213 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.19 ")
      goto 1
    end if
    call iotk_delete_attr(attr_tmp,"columns",ierrl)
    if(ierrl/=0) then
      call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 218 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.19 ")
      goto 1
    end if
    call iotk_delete_attr(attr_tmp,"len",ierrl)
    if(ierrl/=0) then
      call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 223 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.19 ")
      goto 1
    end if
    if(iotk_strlen_trim(attr_tmp)>0) call iotk_strcat(lattr,iotk_strtrim(attr_tmp),ierr=ierrl)
  end if
  if(ierrl/=0) then
    call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 229 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.19 ")
    goto 1
  end if
  call iotk_write_begin(unit,name,lattr,ierr=ierrl)
  if(ierrl/=0) then
    call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 234 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.19 ")
    goto 1
  end if

  allocate(dattmp(size(dat)))
# 242 "iotk_dat.spp"
#if defined(__IOTK_WORKAROUND3) || defined(__IOTK_WORKAROUND4)
# 244 "iotk_dat.spp"
     call iotk_private_pack_CHARACTER1(dattmp,dat,size(dattmp),len(dattmp))
# 248 "iotk_dat.spp"
#else
     dattmp = pack(dat,mask=.true.)
#endif
# 252 "iotk_dat.spp"

  if(binary) then
    if(raw) then
      write(lunit,iostat=iostat) (dattmp(itmp),itmp=1,size(dattmp))
      if(iostat/=0) then
        call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 257 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.19 ")
        goto 1
      end if
    else
      write(lunit,iostat=iostat) idummy,(dattmp(itmp),itmp=1,size(dattmp))
      if(iostat/=0) then
        call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 263 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.19 ")
        goto 1
      end if
    end if
  else
    if(raw) then
# 270 "iotk_dat.spp"
      write(lunit,"(a)",iostat=iostat) (dattmp(itmp),itmp=1,size(dattmp))
# 274 "iotk_dat.spp"
      if(iostat/=0) then
        call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 275 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.19 ")
        goto 1
      end if
    else if(iotk_strcomp(usefmt,"*")) then
      write(lunit,*,iostat=iostat) (dattmp(itmp),itmp=1,size(dattmp))
      if(iostat/=0) then
        call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 281 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.19 ")
        goto 1
      end if
    else if(iotk_strcomp(usefmt,"!")) then
# 286 "iotk_dat.spp"
     do itmp = 1 , size(dattmp)
       call iotk_deescape(linetmp,dattmp(itmp))
       write(lunit,"(a)",iostat=iostat) linetmp(1:iotk_strlen(linetmp))
       if(iostat/=0) then
        call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 290 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.19 ")
        goto 1
        end if
     end do
# 301 "iotk_dat.spp"
    else
      write(lunit,fmt=usefmt(1:iotk_strlen(usefmt)),iostat=iostat) (dattmp(itmp),itmp=1,size(dattmp))
      if(iostat/=0) then
        call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 304 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.19 ")
        goto 1
      end if
    end if
  end if
  call iotk_write_end(unit,name,ierr=ierrl)
  if(ierrl/=0) then
    call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 311 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.19 ")
    goto 1
  end if
1 continue
  if(allocated(dattmp)) deallocate(dattmp)
  if(present(ierr)) then
    ierr = ierrl
  else
    if(ierrl/=0) call iotk_error_handler(ierrl)
  end if
end subroutine iotk_write_dat_CHARACTER1_1


# 326 "iotk_dat.spp"
recursive subroutine iotk_scan_dat_aux_CHARACTER1(unit,dat,rkind,rlen,fmt,ierr)
  use iotk_base
  use iotk_error_interf
  use iotk_attr_interf, only: iotk_read
  use iotk_scan_interf
  use iotk_str_interf
  use iotk_unit_interf
  use iotk_misc_interf
  implicit none
  integer,         intent(in)  :: unit
#ifdef __IOTK_WORKAROUND6
  CHARACTER (kind=__IOTK_CHARACTER1,len=*)                        :: dat (:)
#else
  CHARACTER (kind=__IOTK_CHARACTER1,len=*),           intent(out) :: dat (:)
#endif
  integer,         intent(in)  :: rkind
  integer,         intent(in)  :: rlen
  character(*),    intent(in)  :: fmt
  integer,         intent(out) :: ierr
  integer(iotk_header_kind) :: idummy
  logical :: raw,binary
  integer :: lunit
  integer :: i,index,length,nexttag,iostat,altlength
  type(iotk_unit), pointer :: this
  character(len=iotk_linlenx) :: line,altline
# 352 "iotk_dat.spp"
  CHARACTER (kind=kind(dat), len=rlen) :: dattmp(ubound(dat,1))
# 362 "iotk_dat.spp"
  lunit = iotk_phys_unit(unit)
  ierr = 0
  iostat = 0
  idummy = 0
  call iotk_unit_get(lunit,pointer=this)
  raw = .false.
  if(associated(this)) then
    raw = this%raw
  end if
  call iotk_inquire(unit=lunit,binary=binary,ierr=ierr)
  if(ierr/=0) then
    call iotk_error_issue(ierr,"iotk_scan_dat_aux",__FILE__,__LINE__)
# 373 "iotk_dat.spp"
call iotk_error_msg(ierr,"CVS Revision: 1.19 ")
    return
  end if
# 377 "iotk_dat.spp"
  if(binary) then
    if(raw) then
#ifdef __IOTK_WORKAROUND3
      read(lunit,iostat=iostat) ( dattmp(i), i=1,ubound(dat,1) )
#else
      read(lunit,iostat=iostat) dattmp
#endif
      if(iostat/=0) then
        call iotk_error_issue(ierr,"iotk_scan_dat_aux",__FILE__,__LINE__)
# 385 "iotk_dat.spp"
call iotk_error_msg(ierr,"CVS Revision: 1.19 ")
# 385 "iotk_dat.spp"
call iotk_error_msg(ierr,' ')
# 385 "iotk_dat.spp"
call iotk_error_write(ierr,"iostat",iostat)
        return
      end if
    else
#ifdef __IOTK_WORKAROUND3
      read(lunit,iostat=iostat) idummy, ( dattmp(i), i=1,ubound(dat,1) )
#else
      read(lunit,iostat=iostat) idummy, dattmp
#endif
      if(iostat/=0) then
        call iotk_error_issue(ierr,"iotk_scan_dat_aux",__FILE__,__LINE__)
# 395 "iotk_dat.spp"
call iotk_error_msg(ierr,"CVS Revision: 1.19 ")
# 395 "iotk_dat.spp"
call iotk_error_msg(ierr,' ')
# 395 "iotk_dat.spp"
call iotk_error_write(ierr,"iostat",iostat)
        return
      end if
    end if
  else
    if(raw) then  
#ifdef __IOTK_WORKAROUND3
      read(lunit,"(a)",iostat=iostat) ( dattmp(i), i=1,ubound(dat,1) )
#else
      read(lunit,"(a)",iostat=iostat) dattmp
#endif
      if(iostat/=0) then
        call iotk_error_issue(ierr,"iotk_scan_dat_aux",__FILE__,__LINE__)
# 407 "iotk_dat.spp"
call iotk_error_msg(ierr,"CVS Revision: 1.19 ")
# 407 "iotk_dat.spp"
call iotk_error_msg(ierr,' ')
# 407 "iotk_dat.spp"
call iotk_error_write(ierr,"iostat",iostat)
        return
      end if
    else if(iotk_strcomp(fmt,"*")) then
#ifdef __IOTK_WORKAROUND3
      read(lunit,fmt=*,  iostat=iostat) ( dattmp(i), i=1,ubound(dat,1) )
#else
      read(lunit,fmt=*,  iostat=iostat) dattmp
#endif
      if(iostat/=0) then
        call iotk_error_issue(ierr,"iotk_scan_dat_aux",__FILE__,__LINE__)
# 417 "iotk_dat.spp"
call iotk_error_msg(ierr,"CVS Revision: 1.19 ")
# 417 "iotk_dat.spp"
call iotk_error_msg(ierr,' ')
# 417 "iotk_dat.spp"
call iotk_error_write(ierr,"iostat",iostat)
        return
      end if
    else if(iotk_strcomp(fmt,"!")) then
      index = 0
      iostat = 0
      do
        call iotk_getline(lunit,line,length,ierr)
        if(ierr/=0) then
          call iotk_error_issue(ierr,"iotk_scan_dat_aux",__FILE__,__LINE__)
# 426 "iotk_dat.spp"
call iotk_error_msg(ierr,"CVS Revision: 1.19 ")
          return
        end if
        nexttag = scan(line(1:length),"<")
        if(nexttag==0) then
          nexttag = length + 1
        else
! AGGIUSTA LA POSIZIONE SE C'E' UNA TAG SU QUESTA LINEA
! E' UN PO' CASERECCIO MA FUNZIONA
          backspace(lunit,iostat=iostat)
          if(iostat/=0) then
            call iotk_error_issue(ierr,"iotk_scan_dat_aux",__FILE__,__LINE__)
# 437 "iotk_dat.spp"
call iotk_error_msg(ierr,"CVS Revision: 1.19 ")
# 437 "iotk_dat.spp"
call iotk_error_msg(ierr,' ')
# 437 "iotk_dat.spp"
call iotk_error_write(ierr,"iostat",iostat)
            return
          end if
          call iotk_getline(lunit,altline,altlength,ierr)
          if(ierr/=0) then
            call iotk_error_issue(ierr,"iotk_scan_dat_aux",__FILE__,__LINE__)
# 442 "iotk_dat.spp"
call iotk_error_msg(ierr,"CVS Revision: 1.19 ")
            return
          end if
          backspace(lunit,iostat=iostat)
          if(iostat/=0) then
            call iotk_error_issue(ierr,"iotk_scan_dat_aux",__FILE__,__LINE__)
# 447 "iotk_dat.spp"
call iotk_error_msg(ierr,"CVS Revision: 1.19 ")
# 447 "iotk_dat.spp"
call iotk_error_msg(ierr,' ')
# 447 "iotk_dat.spp"
call iotk_error_write(ierr,"iostat",iostat)
            return
          end if
          read(lunit,"(a)",advance="no",iostat=iostat) altline(1:nexttag-1 + altlength - length)
          if(iostat/=0) then
            call iotk_error_issue(ierr,"iotk_scan_dat_aux",__FILE__,__LINE__)
# 452 "iotk_dat.spp"
call iotk_error_msg(ierr,"CVS Revision: 1.19 ")
# 452 "iotk_dat.spp"
call iotk_error_msg(ierr,' ')
# 452 "iotk_dat.spp"
call iotk_error_write(ierr,"iostat",iostat)
            return
          end if
        end if
        index = index + 1
        call iotk_escape(to=dattmp(index),from=line(1:nexttag - 1))
        if(iotk_strlen(dattmp(index)) < len(dattmp)) dattmp(index)(iotk_strlen(dattmp(index))+1:) = " "
        if(index == size(dat)) exit
        if(nexttag/=length + 1) then
          call iotk_error_issue(ierr,"iotk_scan_dat_aux",__FILE__,__LINE__)
# 461 "iotk_dat.spp"
call iotk_error_msg(ierr,"CVS Revision: 1.19 ")
# 461 "iotk_dat.spp"
call iotk_error_msg(ierr,'Missing dat')
          return
        end if
      end do
    else
#ifdef __IOTK_WORKAROUND3
      read(lunit,fmt=fmt(1:iotk_strlen(fmt)),iostat=iostat) ( dattmp(i), i=1,ubound(dat,1) )
#else
      read(lunit,fmt=fmt(1:iotk_strlen(fmt)),iostat=iostat) dattmp
#endif
      if(iostat/=0) then
        call iotk_error_issue(ierr,"iotk_scan_dat_aux",__FILE__,__LINE__)
# 472 "iotk_dat.spp"
call iotk_error_msg(ierr,"CVS Revision: 1.19 ")
# 472 "iotk_dat.spp"
call iotk_error_msg(ierr,' ')
# 472 "iotk_dat.spp"
call iotk_error_write(ierr,"iostat",iostat)
        return
      end if
    end if
  end if
  if(len(dattmp) <= len(dat)) then
    dat (:) = dattmp (:)
  else
    dat (:) = dattmp (:) (1:len(dat))
  end if
# 623 "iotk_dat.spp"
  if(idummy/=0) then
    call iotk_error_issue(ierr,"iotk_scan_dat_aux",__FILE__,__LINE__)
# 624 "iotk_dat.spp"
call iotk_error_msg(ierr,"CVS Revision: 1.19 ")
    return
  end if
end subroutine iotk_scan_dat_aux_CHARACTER1
# 629 "iotk_dat.spp"

# 631 "iotk_dat.spp"
subroutine iotk_scan_dat_CHARACTER1_1(unit,name,dat,dummy,attr,found,default,ierr)
  use iotk_base
  use iotk_error_interf
  use iotk_dat_interf, only: iotk_scan_dat_aux
  use iotk_scan_interf
  use iotk_str_interf
  use iotk_misc_interf
  implicit none
  integer,                   intent(in)  :: unit
  character(*),              intent(in)  :: name
#ifdef __IOTK_WORKAROUND6
  CHARACTER (kind=__IOTK_CHARACTER1,len=*)                        :: dat (:)
#else
  CHARACTER (kind=__IOTK_CHARACTER1,len=*),           intent(out) :: dat (:)
#endif
  type(iotk_dummytype), optional         :: dummy
#ifdef __IOTK_WORKAROUND6
  character(len=*),optional              :: attr
#else
  character(len=*),optional, intent(out) :: attr
#endif
  logical,         optional, intent(out) :: found
  CHARACTER (kind=__IOTK_CHARACTER1,len=*), optional, intent(in)  :: default (:)
  integer,         optional, intent(out) :: ierr
# 656 "iotk_dat.spp"
  CHARACTER (kind=__IOTK_CHARACTER1,len=len(dat)), allocatable :: tmpdat(:)
# 660 "iotk_dat.spp"
  integer :: ierrl,ierrl2
  integer :: rkind,rsize,rlen
  character(iotk_vallenx) :: rtype
  character(iotk_vallenx) :: fmt
  character(iotk_attlenx) :: lattr
  integer :: columns
  logical :: inside,foundl
  inside = .false.
  ierrl = 0
  ierrl2 = 0
  foundl=.false.
  call iotk_scan_begin(unit,name,lattr,found=foundl,ierr=ierrl)
  if(.not. foundl) goto 1
  foundl = .true.
  inside = .true.
  if(present(attr)) call iotk_strcpy(attr,lattr,ierrl)
  if(ierrl/=0) then
    call iotk_error_issue(ierrl,"iotk_scan_dat",__FILE__,__LINE__)
# 677 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.19 ")
    goto 1
  end if
  call iotk_parse_dat(lattr,rtype,rkind,rsize,rlen,fmt,columns,ierrl)
! Note that columns is not effectively used
  if(ierrl/=0) goto 1
  if(.not. (iotk_strcomp(rtype,iotk_eos) .or. iotk_strcomp(rtype,"CHARACTER") ) ) then
    call iotk_error_issue(ierrl,"iotk_scan_dat",__FILE__,__LINE__)
# 684 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.19 ")
# 684 "iotk_dat.spp"
call iotk_error_msg(ierrl,' ')
# 684 "iotk_dat.spp"
call iotk_error_write(ierrl,"rtype",rtype(1:iotk_strlen(rtype)))
# 684 "iotk_dat.spp"
call iotk_error_write(ierrl,"type","CHARACTER")
    goto 1
  end if
  if(.not. (rsize==-1 .or. rsize==size(dat)) ) then
    call iotk_error_issue(ierrl,"iotk_scan_dat",__FILE__,__LINE__)
# 688 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.19 ")
    goto 1
  end if
  if(rkind==-1) rkind = kind(dat)
# 693 "iotk_dat.spp"
  if(rlen ==-1) rlen  = len(dat)
# 695 "iotk_dat.spp"

  allocate(tmpdat(size(dat)))
  call iotk_scan_dat_aux(unit,tmpdat,rkind,rlen,fmt(1:iotk_strlen(fmt)),ierrl)
# 701 "iotk_dat.spp"
#if defined(__IOTK_WORKAROUND3) || defined(__IOTK_WORKAROUND4)
# 703 "iotk_dat.spp"
     call iotk_private_pack_CHARACTER1(dat,tmpdat,size(tmpdat),len(tmpdat))
# 707 "iotk_dat.spp"
#else
     dat = reshape(tmpdat,shape(dat))
#endif
# 711 "iotk_dat.spp"
  deallocate(tmpdat)
1 continue
  if(inside) then
    call iotk_scan_end(unit,name,ierr=ierrl2)
    if(ierrl2/=0) then
      call iotk_error_clear(ierrl)
      ierrl=ierrl2
    end if
  end if
  if(ierrl/=0) foundl=.false.
  if(present(found)) found = foundl
  if(ierrl==0 .and. .not. present(found) .and. .not. present(default) .and. .not. foundl) then
    call iotk_error_issue(ierrl,"iotk_scan_dat",__FILE__,__LINE__)
# 723 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.19 ")
# 723 "iotk_dat.spp"
call iotk_error_msg(ierrl,'Dat not found')
# 723 "iotk_dat.spp"
call iotk_error_write(ierrl,"name",name)
    ierrl = - ierrl
  end if 
  if(present(default) .and. .not. foundl) then
    dat=default
  end if
  if(present(ierr)) then
    ierr = ierrl
  else
    if(ierrl>0 .or. (.not.present(found) .and. .not.present(default))) call iotk_error_handler(ierrl)
  end if
end subroutine iotk_scan_dat_CHARACTER1_1


#endif
#endif

subroutine iotk_dat_dummy_CHARACTER1_1
  write(0,*)
end subroutine iotk_dat_dummy_CHARACTER1_1


# 45 "iotk_dat.spp"

# 65 "iotk_dat.spp"


!------------------------------------------------------------------------------!
! Inclusion of configuration file
#include "iotk_config.h"
!------------------------------------------------------------------------------!

# 74 "iotk_dat.spp"
#include "iotk_auxmacros.h"
# 76 "iotk_dat.spp"

# 78 "iotk_dat.spp"

#ifdef __IOTK_CHARACTER1
#if 2 <= __IOTK_MAXRANK
# 82 "iotk_dat.spp"
subroutine iotk_write_dat_CHARACTER1_2(unit,name,dat,dummy,attr,columns,sep,fmt,ierr)
  use iotk_base
  use iotk_error_interf
  use iotk_attr_interf, only : iotk_write_attr
  use iotk_write_interf
  use iotk_fmt_interf
  use iotk_str_interf
  use iotk_unit_interf
  use iotk_misc_interf
  implicit none
  integer,                intent(in)  :: unit
  character(*),           intent(in)  :: name
  CHARACTER (kind=__IOTK_CHARACTER1,len=*), intent(in)  :: dat (:,:) 
  type(iotk_dummytype), optional      :: dummy
  character(len=*), optional, intent(in)  :: attr
  integer,          optional, intent(in)  :: columns
  character(len=*), optional, intent(in)  :: sep
  character(len=*), optional, intent(in)  :: fmt
  integer,      optional, intent(out) :: ierr
  integer :: ierrl,lunit,iostat
  logical :: binary,raw
  integer :: lcolumns
  integer(iotk_header_kind), parameter :: idummy=0
  character(100) :: lsep
  character(300) :: usefmt,usefmt1
  character(iotk_attlenx) :: lattr
  character(iotk_attlenx) :: attr_tmp
  type (iotk_unit), pointer :: this
# 111 "iotk_dat.spp"
  CHARACTER (kind=__IOTK_CHARACTER1,len=len(dat)),allocatable :: dattmp(:)
  character(len=iotk_linlenx) :: linetmp
# 116 "iotk_dat.spp"
  integer :: itmp
  ierrl = 0
  iostat = 0
  lcolumns = 1
  lsep(1:2) = " "//iotk_eos
  if(present(columns)) lcolumns = columns
  if(present(sep)) then
    call iotk_strcpy(lsep,sep,ierrl)
    if(ierrl/=0) then
      call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 125 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.19 ")
      goto 1
    end if
  end if
  lunit = iotk_phys_unit(unit)
  call iotk_unit_get(lunit,pointer=this)
  raw = .false.
  if(associated(this)) then
    raw = this%raw
  end if
  call iotk_inquire(lunit,binary=binary,ierr=ierrl)
  if(ierrl/=0) then
    call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 137 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.19 ")
    goto 1
  end if
  call iotk_strcpy(usefmt,"!",ierrl)
  if(ierrl/=0) then
    call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 142 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.19 ")
    goto 1
  end if
  if(present(fmt) .and. .not. raw) call iotk_strcpy(usefmt,iotk_strtrim(fmt),ierrl)
  if(ierrl/=0) then
    call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 147 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.19 ")
    goto 1
  end if
  if(iotk_strscan(usefmt,"<>&")/=0) then
    call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 151 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.19 ")
# 151 "iotk_dat.spp"
call iotk_error_msg(ierrl,'Special characters (<>&) found in fmt string')
# 151 "iotk_dat.spp"
call iotk_error_write(ierrl,"unit",unit)
# 151 "iotk_dat.spp"
call iotk_error_write(ierrl,"name",trim(name))
# 151 "iotk_dat.spp"
call iotk_error_write(ierrl,"fmt",trim(fmt))
    goto 1
  end if
  call iotk_write_attr(lattr,"type",iotk_tolower("CHARACTER"),first=.true.,ierr=ierrl)
  if(ierrl/=0) then
    call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 156 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.19 ")
    goto 1
  end if
  call iotk_write_attr(lattr,"size",size(dat),ierr=ierrl)
  if(ierrl/=0) then
    call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 161 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.19 ")
    goto 1
  end if
# 165 "iotk_dat.spp"
  call iotk_write_attr(lattr,"len",len(dat),ierr=ierrl)
  if(ierrl/=0) then
    call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 167 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.19 ")
    goto 1
  end if
# 179 "iotk_dat.spp"
  if(.not.iotk_strcomp(usefmt,"!")) call iotk_write_attr(lattr,"fmt",iotk_strtrim(usefmt),ierr=ierrl)
  if(ierrl/=0) then
    call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 181 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.19 ")
    goto 1
  end if
  if(lcolumns/=1) call iotk_write_attr(lattr,"columns",lcolumns,ierr=ierrl)
  if(ierrl/=0) then
    call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 186 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.19 ")
    goto 1
  end if
  if(present(attr)) then
    attr_tmp(1:1)=iotk_eos
    call iotk_strcpy(attr_tmp,attr,ierr=ierrl)
    if(ierrl/=0) then
      call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 193 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.19 ")
      goto 1
    end if
    call iotk_delete_attr(attr_tmp,"type",ierrl)
    if(ierrl/=0) then
      call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 198 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.19 ")
      goto 1
    end if
    call iotk_delete_attr(attr_tmp,"kind",ierrl)
    if(ierrl/=0) then
      call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 203 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.19 ")
      goto 1
    end if
    call iotk_delete_attr(attr_tmp,"size",ierrl)
    if(ierrl/=0) then
      call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 208 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.19 ")
      goto 1
    end if
    call iotk_delete_attr(attr_tmp,"fmt",ierrl)
    if(ierrl/=0) then
      call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 213 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.19 ")
      goto 1
    end if
    call iotk_delete_attr(attr_tmp,"columns",ierrl)
    if(ierrl/=0) then
      call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 218 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.19 ")
      goto 1
    end if
    call iotk_delete_attr(attr_tmp,"len",ierrl)
    if(ierrl/=0) then
      call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 223 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.19 ")
      goto 1
    end if
    if(iotk_strlen_trim(attr_tmp)>0) call iotk_strcat(lattr,iotk_strtrim(attr_tmp),ierr=ierrl)
  end if
  if(ierrl/=0) then
    call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 229 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.19 ")
    goto 1
  end if
  call iotk_write_begin(unit,name,lattr,ierr=ierrl)
  if(ierrl/=0) then
    call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 234 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.19 ")
    goto 1
  end if

  allocate(dattmp(size(dat)))
# 242 "iotk_dat.spp"
#if defined(__IOTK_WORKAROUND3) || defined(__IOTK_WORKAROUND4)
# 244 "iotk_dat.spp"
     call iotk_private_pack_CHARACTER1(dattmp,dat,size(dattmp),len(dattmp))
# 248 "iotk_dat.spp"
#else
     dattmp = pack(dat,mask=.true.)
#endif
# 252 "iotk_dat.spp"

  if(binary) then
    if(raw) then
      write(lunit,iostat=iostat) (dattmp(itmp),itmp=1,size(dattmp))
      if(iostat/=0) then
        call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 257 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.19 ")
        goto 1
      end if
    else
      write(lunit,iostat=iostat) idummy,(dattmp(itmp),itmp=1,size(dattmp))
      if(iostat/=0) then
        call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 263 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.19 ")
        goto 1
      end if
    end if
  else
    if(raw) then
# 270 "iotk_dat.spp"
      write(lunit,"(a)",iostat=iostat) (dattmp(itmp),itmp=1,size(dattmp))
# 274 "iotk_dat.spp"
      if(iostat/=0) then
        call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 275 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.19 ")
        goto 1
      end if
    else if(iotk_strcomp(usefmt,"*")) then
      write(lunit,*,iostat=iostat) (dattmp(itmp),itmp=1,size(dattmp))
      if(iostat/=0) then
        call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 281 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.19 ")
        goto 1
      end if
    else if(iotk_strcomp(usefmt,"!")) then
# 286 "iotk_dat.spp"
     do itmp = 1 , size(dattmp)
       call iotk_deescape(linetmp,dattmp(itmp))
       write(lunit,"(a)",iostat=iostat) linetmp(1:iotk_strlen(linetmp))
       if(iostat/=0) then
        call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 290 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.19 ")
        goto 1
        end if
     end do
# 301 "iotk_dat.spp"
    else
      write(lunit,fmt=usefmt(1:iotk_strlen(usefmt)),iostat=iostat) (dattmp(itmp),itmp=1,size(dattmp))
      if(iostat/=0) then
        call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 304 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.19 ")
        goto 1
      end if
    end if
  end if
  call iotk_write_end(unit,name,ierr=ierrl)
  if(ierrl/=0) then
    call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 311 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.19 ")
    goto 1
  end if
1 continue
  if(allocated(dattmp)) deallocate(dattmp)
  if(present(ierr)) then
    ierr = ierrl
  else
    if(ierrl/=0) call iotk_error_handler(ierrl)
  end if
end subroutine iotk_write_dat_CHARACTER1_2


# 629 "iotk_dat.spp"

# 631 "iotk_dat.spp"
subroutine iotk_scan_dat_CHARACTER1_2(unit,name,dat,dummy,attr,found,default,ierr)
  use iotk_base
  use iotk_error_interf
  use iotk_dat_interf, only: iotk_scan_dat_aux
  use iotk_scan_interf
  use iotk_str_interf
  use iotk_misc_interf
  implicit none
  integer,                   intent(in)  :: unit
  character(*),              intent(in)  :: name
#ifdef __IOTK_WORKAROUND6
  CHARACTER (kind=__IOTK_CHARACTER1,len=*)                        :: dat (:,:)
#else
  CHARACTER (kind=__IOTK_CHARACTER1,len=*),           intent(out) :: dat (:,:)
#endif
  type(iotk_dummytype), optional         :: dummy
#ifdef __IOTK_WORKAROUND6
  character(len=*),optional              :: attr
#else
  character(len=*),optional, intent(out) :: attr
#endif
  logical,         optional, intent(out) :: found
  CHARACTER (kind=__IOTK_CHARACTER1,len=*), optional, intent(in)  :: default (:,:)
  integer,         optional, intent(out) :: ierr
# 656 "iotk_dat.spp"
  CHARACTER (kind=__IOTK_CHARACTER1,len=len(dat)), allocatable :: tmpdat(:)
# 660 "iotk_dat.spp"
  integer :: ierrl,ierrl2
  integer :: rkind,rsize,rlen
  character(iotk_vallenx) :: rtype
  character(iotk_vallenx) :: fmt
  character(iotk_attlenx) :: lattr
  integer :: columns
  logical :: inside,foundl
  inside = .false.
  ierrl = 0
  ierrl2 = 0
  foundl=.false.
  call iotk_scan_begin(unit,name,lattr,found=foundl,ierr=ierrl)
  if(.not. foundl) goto 1
  foundl = .true.
  inside = .true.
  if(present(attr)) call iotk_strcpy(attr,lattr,ierrl)
  if(ierrl/=0) then
    call iotk_error_issue(ierrl,"iotk_scan_dat",__FILE__,__LINE__)
# 677 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.19 ")
    goto 1
  end if
  call iotk_parse_dat(lattr,rtype,rkind,rsize,rlen,fmt,columns,ierrl)
! Note that columns is not effectively used
  if(ierrl/=0) goto 1
  if(.not. (iotk_strcomp(rtype,iotk_eos) .or. iotk_strcomp(rtype,"CHARACTER") ) ) then
    call iotk_error_issue(ierrl,"iotk_scan_dat",__FILE__,__LINE__)
# 684 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.19 ")
# 684 "iotk_dat.spp"
call iotk_error_msg(ierrl,' ')
# 684 "iotk_dat.spp"
call iotk_error_write(ierrl,"rtype",rtype(1:iotk_strlen(rtype)))
# 684 "iotk_dat.spp"
call iotk_error_write(ierrl,"type","CHARACTER")
    goto 1
  end if
  if(.not. (rsize==-1 .or. rsize==size(dat)) ) then
    call iotk_error_issue(ierrl,"iotk_scan_dat",__FILE__,__LINE__)
# 688 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.19 ")
    goto 1
  end if
  if(rkind==-1) rkind = kind(dat)
# 693 "iotk_dat.spp"
  if(rlen ==-1) rlen  = len(dat)
# 695 "iotk_dat.spp"

  allocate(tmpdat(size(dat)))
  call iotk_scan_dat_aux(unit,tmpdat,rkind,rlen,fmt(1:iotk_strlen(fmt)),ierrl)
# 701 "iotk_dat.spp"
#if defined(__IOTK_WORKAROUND3) || defined(__IOTK_WORKAROUND4)
# 703 "iotk_dat.spp"
     call iotk_private_pack_CHARACTER1(dat,tmpdat,size(tmpdat),len(tmpdat))
# 707 "iotk_dat.spp"
#else
     dat = reshape(tmpdat,shape(dat))
#endif
# 711 "iotk_dat.spp"
  deallocate(tmpdat)
1 continue
  if(inside) then
    call iotk_scan_end(unit,name,ierr=ierrl2)
    if(ierrl2/=0) then
      call iotk_error_clear(ierrl)
      ierrl=ierrl2
    end if
  end if
  if(ierrl/=0) foundl=.false.
  if(present(found)) found = foundl
  if(ierrl==0 .and. .not. present(found) .and. .not. present(default) .and. .not. foundl) then
    call iotk_error_issue(ierrl,"iotk_scan_dat",__FILE__,__LINE__)
# 723 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.19 ")
# 723 "iotk_dat.spp"
call iotk_error_msg(ierrl,'Dat not found')
# 723 "iotk_dat.spp"
call iotk_error_write(ierrl,"name",name)
    ierrl = - ierrl
  end if 
  if(present(default) .and. .not. foundl) then
    dat=default
  end if
  if(present(ierr)) then
    ierr = ierrl
  else
    if(ierrl>0 .or. (.not.present(found) .and. .not.present(default))) call iotk_error_handler(ierrl)
  end if
end subroutine iotk_scan_dat_CHARACTER1_2


#endif
#endif

subroutine iotk_dat_dummy_CHARACTER1_2
  write(0,*)
end subroutine iotk_dat_dummy_CHARACTER1_2


# 45 "iotk_dat.spp"

# 48 "iotk_dat.spp"
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
# 65 "iotk_dat.spp"


!------------------------------------------------------------------------------!
! Inclusion of configuration file
#include "iotk_config.h"
!------------------------------------------------------------------------------!

# 74 "iotk_dat.spp"
#include "iotk_auxmacros.h"
# 76 "iotk_dat.spp"

# 78 "iotk_dat.spp"

#ifdef __IOTK_CHARACTER1
#if 3 <= __IOTK_MAXRANK
# 82 "iotk_dat.spp"
subroutine iotk_write_dat_CHARACTER1_3(unit,name,dat,dummy,attr,columns,sep,fmt,ierr)
  use iotk_base
  use iotk_error_interf
  use iotk_attr_interf, only : iotk_write_attr
  use iotk_write_interf
  use iotk_fmt_interf
  use iotk_str_interf
  use iotk_unit_interf
  use iotk_misc_interf
  implicit none
  integer,                intent(in)  :: unit
  character(*),           intent(in)  :: name
  CHARACTER (kind=__IOTK_CHARACTER1,len=*), intent(in)  :: dat (:,:,:) 
  type(iotk_dummytype), optional      :: dummy
  character(len=*), optional, intent(in)  :: attr
  integer,          optional, intent(in)  :: columns
  character(len=*), optional, intent(in)  :: sep
  character(len=*), optional, intent(in)  :: fmt
  integer,      optional, intent(out) :: ierr
  integer :: ierrl,lunit,iostat
  logical :: binary,raw
  integer :: lcolumns
  integer(iotk_header_kind), parameter :: idummy=0
  character(100) :: lsep
  character(300) :: usefmt,usefmt1
  character(iotk_attlenx) :: lattr
  character(iotk_attlenx) :: attr_tmp
  type (iotk_unit), pointer :: this
# 111 "iotk_dat.spp"
  CHARACTER (kind=__IOTK_CHARACTER1,len=len(dat)),allocatable :: dattmp(:)
  character(len=iotk_linlenx) :: linetmp
# 116 "iotk_dat.spp"
  integer :: itmp
  ierrl = 0
  iostat = 0
  lcolumns = 1
  lsep(1:2) = " "//iotk_eos
  if(present(columns)) lcolumns = columns
  if(present(sep)) then
    call iotk_strcpy(lsep,sep,ierrl)
    if(ierrl/=0) then
      call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 125 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.19 ")
      goto 1
    end if
  end if
  lunit = iotk_phys_unit(unit)
  call iotk_unit_get(lunit,pointer=this)
  raw = .false.
  if(associated(this)) then
    raw = this%raw
  end if
  call iotk_inquire(lunit,binary=binary,ierr=ierrl)
  if(ierrl/=0) then
    call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 137 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.19 ")
    goto 1
  end if
  call iotk_strcpy(usefmt,"!",ierrl)
  if(ierrl/=0) then
    call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 142 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.19 ")
    goto 1
  end if
  if(present(fmt) .and. .not. raw) call iotk_strcpy(usefmt,iotk_strtrim(fmt),ierrl)
  if(ierrl/=0) then
    call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 147 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.19 ")
    goto 1
  end if
  if(iotk_strscan(usefmt,"<>&")/=0) then
    call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 151 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.19 ")
# 151 "iotk_dat.spp"
call iotk_error_msg(ierrl,'Special characters (<>&) found in fmt string')
# 151 "iotk_dat.spp"
call iotk_error_write(ierrl,"unit",unit)
# 151 "iotk_dat.spp"
call iotk_error_write(ierrl,"name",trim(name))
# 151 "iotk_dat.spp"
call iotk_error_write(ierrl,"fmt",trim(fmt))
    goto 1
  end if
  call iotk_write_attr(lattr,"type",iotk_tolower("CHARACTER"),first=.true.,ierr=ierrl)
  if(ierrl/=0) then
    call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 156 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.19 ")
    goto 1
  end if
  call iotk_write_attr(lattr,"size",size(dat),ierr=ierrl)
  if(ierrl/=0) then
    call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 161 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.19 ")
    goto 1
  end if
# 165 "iotk_dat.spp"
  call iotk_write_attr(lattr,"len",len(dat),ierr=ierrl)
  if(ierrl/=0) then
    call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 167 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.19 ")
    goto 1
  end if
# 179 "iotk_dat.spp"
  if(.not.iotk_strcomp(usefmt,"!")) call iotk_write_attr(lattr,"fmt",iotk_strtrim(usefmt),ierr=ierrl)
  if(ierrl/=0) then
    call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 181 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.19 ")
    goto 1
  end if
  if(lcolumns/=1) call iotk_write_attr(lattr,"columns",lcolumns,ierr=ierrl)
  if(ierrl/=0) then
    call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 186 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.19 ")
    goto 1
  end if
  if(present(attr)) then
    attr_tmp(1:1)=iotk_eos
    call iotk_strcpy(attr_tmp,attr,ierr=ierrl)
    if(ierrl/=0) then
      call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 193 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.19 ")
      goto 1
    end if
    call iotk_delete_attr(attr_tmp,"type",ierrl)
    if(ierrl/=0) then
      call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 198 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.19 ")
      goto 1
    end if
    call iotk_delete_attr(attr_tmp,"kind",ierrl)
    if(ierrl/=0) then
      call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 203 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.19 ")
      goto 1
    end if
    call iotk_delete_attr(attr_tmp,"size",ierrl)
    if(ierrl/=0) then
      call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 208 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.19 ")
      goto 1
    end if
    call iotk_delete_attr(attr_tmp,"fmt",ierrl)
    if(ierrl/=0) then
      call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 213 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.19 ")
      goto 1
    end if
    call iotk_delete_attr(attr_tmp,"columns",ierrl)
    if(ierrl/=0) then
      call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 218 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.19 ")
      goto 1
    end if
    call iotk_delete_attr(attr_tmp,"len",ierrl)
    if(ierrl/=0) then
      call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 223 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.19 ")
      goto 1
    end if
    if(iotk_strlen_trim(attr_tmp)>0) call iotk_strcat(lattr,iotk_strtrim(attr_tmp),ierr=ierrl)
  end if
  if(ierrl/=0) then
    call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 229 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.19 ")
    goto 1
  end if
  call iotk_write_begin(unit,name,lattr,ierr=ierrl)
  if(ierrl/=0) then
    call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 234 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.19 ")
    goto 1
  end if

  allocate(dattmp(size(dat)))
# 242 "iotk_dat.spp"
#if defined(__IOTK_WORKAROUND3) || defined(__IOTK_WORKAROUND4)
# 244 "iotk_dat.spp"
     call iotk_private_pack_CHARACTER1(dattmp,dat,size(dattmp),len(dattmp))
# 248 "iotk_dat.spp"
#else
     dattmp = pack(dat,mask=.true.)
#endif
# 252 "iotk_dat.spp"

  if(binary) then
    if(raw) then
      write(lunit,iostat=iostat) (dattmp(itmp),itmp=1,size(dattmp))
      if(iostat/=0) then
        call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 257 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.19 ")
        goto 1
      end if
    else
      write(lunit,iostat=iostat) idummy,(dattmp(itmp),itmp=1,size(dattmp))
      if(iostat/=0) then
        call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 263 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.19 ")
        goto 1
      end if
    end if
  else
    if(raw) then
# 270 "iotk_dat.spp"
      write(lunit,"(a)",iostat=iostat) (dattmp(itmp),itmp=1,size(dattmp))
# 274 "iotk_dat.spp"
      if(iostat/=0) then
        call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 275 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.19 ")
        goto 1
      end if
    else if(iotk_strcomp(usefmt,"*")) then
      write(lunit,*,iostat=iostat) (dattmp(itmp),itmp=1,size(dattmp))
      if(iostat/=0) then
        call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 281 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.19 ")
        goto 1
      end if
    else if(iotk_strcomp(usefmt,"!")) then
# 286 "iotk_dat.spp"
     do itmp = 1 , size(dattmp)
       call iotk_deescape(linetmp,dattmp(itmp))
       write(lunit,"(a)",iostat=iostat) linetmp(1:iotk_strlen(linetmp))
       if(iostat/=0) then
        call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 290 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.19 ")
        goto 1
        end if
     end do
# 301 "iotk_dat.spp"
    else
      write(lunit,fmt=usefmt(1:iotk_strlen(usefmt)),iostat=iostat) (dattmp(itmp),itmp=1,size(dattmp))
      if(iostat/=0) then
        call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 304 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.19 ")
        goto 1
      end if
    end if
  end if
  call iotk_write_end(unit,name,ierr=ierrl)
  if(ierrl/=0) then
    call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 311 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.19 ")
    goto 1
  end if
1 continue
  if(allocated(dattmp)) deallocate(dattmp)
  if(present(ierr)) then
    ierr = ierrl
  else
    if(ierrl/=0) call iotk_error_handler(ierrl)
  end if
end subroutine iotk_write_dat_CHARACTER1_3


# 629 "iotk_dat.spp"

# 631 "iotk_dat.spp"
subroutine iotk_scan_dat_CHARACTER1_3(unit,name,dat,dummy,attr,found,default,ierr)
  use iotk_base
  use iotk_error_interf
  use iotk_dat_interf, only: iotk_scan_dat_aux
  use iotk_scan_interf
  use iotk_str_interf
  use iotk_misc_interf
  implicit none
  integer,                   intent(in)  :: unit
  character(*),              intent(in)  :: name
#ifdef __IOTK_WORKAROUND6
  CHARACTER (kind=__IOTK_CHARACTER1,len=*)                        :: dat (:,:,:)
#else
  CHARACTER (kind=__IOTK_CHARACTER1,len=*),           intent(out) :: dat (:,:,:)
#endif
  type(iotk_dummytype), optional         :: dummy
#ifdef __IOTK_WORKAROUND6
  character(len=*),optional              :: attr
#else
  character(len=*),optional, intent(out) :: attr
#endif
  logical,         optional, intent(out) :: found
  CHARACTER (kind=__IOTK_CHARACTER1,len=*), optional, intent(in)  :: default (:,:,:)
  integer,         optional, intent(out) :: ierr
# 656 "iotk_dat.spp"
  CHARACTER (kind=__IOTK_CHARACTER1,len=len(dat)), allocatable :: tmpdat(:)
# 660 "iotk_dat.spp"
  integer :: ierrl,ierrl2
  integer :: rkind,rsize,rlen
  character(iotk_vallenx) :: rtype
  character(iotk_vallenx) :: fmt
  character(iotk_attlenx) :: lattr
  integer :: columns
  logical :: inside,foundl
  inside = .false.
  ierrl = 0
  ierrl2 = 0
  foundl=.false.
  call iotk_scan_begin(unit,name,lattr,found=foundl,ierr=ierrl)
  if(.not. foundl) goto 1
  foundl = .true.
  inside = .true.
  if(present(attr)) call iotk_strcpy(attr,lattr,ierrl)
  if(ierrl/=0) then
    call iotk_error_issue(ierrl,"iotk_scan_dat",__FILE__,__LINE__)
# 677 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.19 ")
    goto 1
  end if
  call iotk_parse_dat(lattr,rtype,rkind,rsize,rlen,fmt,columns,ierrl)
! Note that columns is not effectively used
  if(ierrl/=0) goto 1
  if(.not. (iotk_strcomp(rtype,iotk_eos) .or. iotk_strcomp(rtype,"CHARACTER") ) ) then
    call iotk_error_issue(ierrl,"iotk_scan_dat",__FILE__,__LINE__)
# 684 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.19 ")
# 684 "iotk_dat.spp"
call iotk_error_msg(ierrl,' ')
# 684 "iotk_dat.spp"
call iotk_error_write(ierrl,"rtype",rtype(1:iotk_strlen(rtype)))
# 684 "iotk_dat.spp"
call iotk_error_write(ierrl,"type","CHARACTER")
    goto 1
  end if
  if(.not. (rsize==-1 .or. rsize==size(dat)) ) then
    call iotk_error_issue(ierrl,"iotk_scan_dat",__FILE__,__LINE__)
# 688 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.19 ")
    goto 1
  end if
  if(rkind==-1) rkind = kind(dat)
# 693 "iotk_dat.spp"
  if(rlen ==-1) rlen  = len(dat)
# 695 "iotk_dat.spp"

  allocate(tmpdat(size(dat)))
  call iotk_scan_dat_aux(unit,tmpdat,rkind,rlen,fmt(1:iotk_strlen(fmt)),ierrl)
# 701 "iotk_dat.spp"
#if defined(__IOTK_WORKAROUND3) || defined(__IOTK_WORKAROUND4)
# 703 "iotk_dat.spp"
     call iotk_private_pack_CHARACTER1(dat,tmpdat,size(tmpdat),len(tmpdat))
# 707 "iotk_dat.spp"
#else
     dat = reshape(tmpdat,shape(dat))
#endif
# 711 "iotk_dat.spp"
  deallocate(tmpdat)
1 continue
  if(inside) then
    call iotk_scan_end(unit,name,ierr=ierrl2)
    if(ierrl2/=0) then
      call iotk_error_clear(ierrl)
      ierrl=ierrl2
    end if
  end if
  if(ierrl/=0) foundl=.false.
  if(present(found)) found = foundl
  if(ierrl==0 .and. .not. present(found) .and. .not. present(default) .and. .not. foundl) then
    call iotk_error_issue(ierrl,"iotk_scan_dat",__FILE__,__LINE__)
# 723 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.19 ")
# 723 "iotk_dat.spp"
call iotk_error_msg(ierrl,'Dat not found')
# 723 "iotk_dat.spp"
call iotk_error_write(ierrl,"name",name)
    ierrl = - ierrl
  end if 
  if(present(default) .and. .not. foundl) then
    dat=default
  end if
  if(present(ierr)) then
    ierr = ierrl
  else
    if(ierrl>0 .or. (.not.present(found) .and. .not.present(default))) call iotk_error_handler(ierrl)
  end if
end subroutine iotk_scan_dat_CHARACTER1_3


#endif
#endif

subroutine iotk_dat_dummy_CHARACTER1_3
  write(0,*)
end subroutine iotk_dat_dummy_CHARACTER1_3


# 45 "iotk_dat.spp"

# 65 "iotk_dat.spp"


!------------------------------------------------------------------------------!
! Inclusion of configuration file
#include "iotk_config.h"
!------------------------------------------------------------------------------!

# 74 "iotk_dat.spp"
#include "iotk_auxmacros.h"
# 76 "iotk_dat.spp"

# 78 "iotk_dat.spp"

#ifdef __IOTK_CHARACTER1
#if 4 <= __IOTK_MAXRANK
# 82 "iotk_dat.spp"
subroutine iotk_write_dat_CHARACTER1_4(unit,name,dat,dummy,attr,columns,sep,fmt,ierr)
  use iotk_base
  use iotk_error_interf
  use iotk_attr_interf, only : iotk_write_attr
  use iotk_write_interf
  use iotk_fmt_interf
  use iotk_str_interf
  use iotk_unit_interf
  use iotk_misc_interf
  implicit none
  integer,                intent(in)  :: unit
  character(*),           intent(in)  :: name
  CHARACTER (kind=__IOTK_CHARACTER1,len=*), intent(in)  :: dat (:,:,:,:) 
  type(iotk_dummytype), optional      :: dummy
  character(len=*), optional, intent(in)  :: attr
  integer,          optional, intent(in)  :: columns
  character(len=*), optional, intent(in)  :: sep
  character(len=*), optional, intent(in)  :: fmt
  integer,      optional, intent(out) :: ierr
  integer :: ierrl,lunit,iostat
  logical :: binary,raw
  integer :: lcolumns
  integer(iotk_header_kind), parameter :: idummy=0
  character(100) :: lsep
  character(300) :: usefmt,usefmt1
  character(iotk_attlenx) :: lattr
  character(iotk_attlenx) :: attr_tmp
  type (iotk_unit), pointer :: this
# 111 "iotk_dat.spp"
  CHARACTER (kind=__IOTK_CHARACTER1,len=len(dat)),allocatable :: dattmp(:)
  character(len=iotk_linlenx) :: linetmp
# 116 "iotk_dat.spp"
  integer :: itmp
  ierrl = 0
  iostat = 0
  lcolumns = 1
  lsep(1:2) = " "//iotk_eos
  if(present(columns)) lcolumns = columns
  if(present(sep)) then
    call iotk_strcpy(lsep,sep,ierrl)
    if(ierrl/=0) then
      call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 125 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.19 ")
      goto 1
    end if
  end if
  lunit = iotk_phys_unit(unit)
  call iotk_unit_get(lunit,pointer=this)
  raw = .false.
  if(associated(this)) then
    raw = this%raw
  end if
  call iotk_inquire(lunit,binary=binary,ierr=ierrl)
  if(ierrl/=0) then
    call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 137 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.19 ")
    goto 1
  end if
  call iotk_strcpy(usefmt,"!",ierrl)
  if(ierrl/=0) then
    call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 142 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.19 ")
    goto 1
  end if
  if(present(fmt) .and. .not. raw) call iotk_strcpy(usefmt,iotk_strtrim(fmt),ierrl)
  if(ierrl/=0) then
    call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 147 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.19 ")
    goto 1
  end if
  if(iotk_strscan(usefmt,"<>&")/=0) then
    call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 151 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.19 ")
# 151 "iotk_dat.spp"
call iotk_error_msg(ierrl,'Special characters (<>&) found in fmt string')
# 151 "iotk_dat.spp"
call iotk_error_write(ierrl,"unit",unit)
# 151 "iotk_dat.spp"
call iotk_error_write(ierrl,"name",trim(name))
# 151 "iotk_dat.spp"
call iotk_error_write(ierrl,"fmt",trim(fmt))
    goto 1
  end if
  call iotk_write_attr(lattr,"type",iotk_tolower("CHARACTER"),first=.true.,ierr=ierrl)
  if(ierrl/=0) then
    call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 156 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.19 ")
    goto 1
  end if
  call iotk_write_attr(lattr,"size",size(dat),ierr=ierrl)
  if(ierrl/=0) then
    call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 161 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.19 ")
    goto 1
  end if
# 165 "iotk_dat.spp"
  call iotk_write_attr(lattr,"len",len(dat),ierr=ierrl)
  if(ierrl/=0) then
    call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 167 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.19 ")
    goto 1
  end if
# 179 "iotk_dat.spp"
  if(.not.iotk_strcomp(usefmt,"!")) call iotk_write_attr(lattr,"fmt",iotk_strtrim(usefmt),ierr=ierrl)
  if(ierrl/=0) then
    call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 181 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.19 ")
    goto 1
  end if
  if(lcolumns/=1) call iotk_write_attr(lattr,"columns",lcolumns,ierr=ierrl)
  if(ierrl/=0) then
    call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 186 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.19 ")
    goto 1
  end if
  if(present(attr)) then
    attr_tmp(1:1)=iotk_eos
    call iotk_strcpy(attr_tmp,attr,ierr=ierrl)
    if(ierrl/=0) then
      call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 193 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.19 ")
      goto 1
    end if
    call iotk_delete_attr(attr_tmp,"type",ierrl)
    if(ierrl/=0) then
      call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 198 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.19 ")
      goto 1
    end if
    call iotk_delete_attr(attr_tmp,"kind",ierrl)
    if(ierrl/=0) then
      call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 203 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.19 ")
      goto 1
    end if
    call iotk_delete_attr(attr_tmp,"size",ierrl)
    if(ierrl/=0) then
      call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 208 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.19 ")
      goto 1
    end if
    call iotk_delete_attr(attr_tmp,"fmt",ierrl)
    if(ierrl/=0) then
      call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 213 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.19 ")
      goto 1
    end if
    call iotk_delete_attr(attr_tmp,"columns",ierrl)
    if(ierrl/=0) then
      call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 218 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.19 ")
      goto 1
    end if
    call iotk_delete_attr(attr_tmp,"len",ierrl)
    if(ierrl/=0) then
      call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 223 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.19 ")
      goto 1
    end if
    if(iotk_strlen_trim(attr_tmp)>0) call iotk_strcat(lattr,iotk_strtrim(attr_tmp),ierr=ierrl)
  end if
  if(ierrl/=0) then
    call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 229 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.19 ")
    goto 1
  end if
  call iotk_write_begin(unit,name,lattr,ierr=ierrl)
  if(ierrl/=0) then
    call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 234 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.19 ")
    goto 1
  end if

  allocate(dattmp(size(dat)))
# 242 "iotk_dat.spp"
#if defined(__IOTK_WORKAROUND3) || defined(__IOTK_WORKAROUND4)
# 244 "iotk_dat.spp"
     call iotk_private_pack_CHARACTER1(dattmp,dat,size(dattmp),len(dattmp))
# 248 "iotk_dat.spp"
#else
     dattmp = pack(dat,mask=.true.)
#endif
# 252 "iotk_dat.spp"

  if(binary) then
    if(raw) then
      write(lunit,iostat=iostat) (dattmp(itmp),itmp=1,size(dattmp))
      if(iostat/=0) then
        call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 257 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.19 ")
        goto 1
      end if
    else
      write(lunit,iostat=iostat) idummy,(dattmp(itmp),itmp=1,size(dattmp))
      if(iostat/=0) then
        call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 263 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.19 ")
        goto 1
      end if
    end if
  else
    if(raw) then
# 270 "iotk_dat.spp"
      write(lunit,"(a)",iostat=iostat) (dattmp(itmp),itmp=1,size(dattmp))
# 274 "iotk_dat.spp"
      if(iostat/=0) then
        call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 275 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.19 ")
        goto 1
      end if
    else if(iotk_strcomp(usefmt,"*")) then
      write(lunit,*,iostat=iostat) (dattmp(itmp),itmp=1,size(dattmp))
      if(iostat/=0) then
        call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 281 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.19 ")
        goto 1
      end if
    else if(iotk_strcomp(usefmt,"!")) then
# 286 "iotk_dat.spp"
     do itmp = 1 , size(dattmp)
       call iotk_deescape(linetmp,dattmp(itmp))
       write(lunit,"(a)",iostat=iostat) linetmp(1:iotk_strlen(linetmp))
       if(iostat/=0) then
        call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 290 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.19 ")
        goto 1
        end if
     end do
# 301 "iotk_dat.spp"
    else
      write(lunit,fmt=usefmt(1:iotk_strlen(usefmt)),iostat=iostat) (dattmp(itmp),itmp=1,size(dattmp))
      if(iostat/=0) then
        call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 304 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.19 ")
        goto 1
      end if
    end if
  end if
  call iotk_write_end(unit,name,ierr=ierrl)
  if(ierrl/=0) then
    call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 311 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.19 ")
    goto 1
  end if
1 continue
  if(allocated(dattmp)) deallocate(dattmp)
  if(present(ierr)) then
    ierr = ierrl
  else
    if(ierrl/=0) call iotk_error_handler(ierrl)
  end if
end subroutine iotk_write_dat_CHARACTER1_4


# 629 "iotk_dat.spp"

# 631 "iotk_dat.spp"
subroutine iotk_scan_dat_CHARACTER1_4(unit,name,dat,dummy,attr,found,default,ierr)
  use iotk_base
  use iotk_error_interf
  use iotk_dat_interf, only: iotk_scan_dat_aux
  use iotk_scan_interf
  use iotk_str_interf
  use iotk_misc_interf
  implicit none
  integer,                   intent(in)  :: unit
  character(*),              intent(in)  :: name
#ifdef __IOTK_WORKAROUND6
  CHARACTER (kind=__IOTK_CHARACTER1,len=*)                        :: dat (:,:,:,:)
#else
  CHARACTER (kind=__IOTK_CHARACTER1,len=*),           intent(out) :: dat (:,:,:,:)
#endif
  type(iotk_dummytype), optional         :: dummy
#ifdef __IOTK_WORKAROUND6
  character(len=*),optional              :: attr
#else
  character(len=*),optional, intent(out) :: attr
#endif
  logical,         optional, intent(out) :: found
  CHARACTER (kind=__IOTK_CHARACTER1,len=*), optional, intent(in)  :: default (:,:,:,:)
  integer,         optional, intent(out) :: ierr
# 656 "iotk_dat.spp"
  CHARACTER (kind=__IOTK_CHARACTER1,len=len(dat)), allocatable :: tmpdat(:)
# 660 "iotk_dat.spp"
  integer :: ierrl,ierrl2
  integer :: rkind,rsize,rlen
  character(iotk_vallenx) :: rtype
  character(iotk_vallenx) :: fmt
  character(iotk_attlenx) :: lattr
  integer :: columns
  logical :: inside,foundl
  inside = .false.
  ierrl = 0
  ierrl2 = 0
  foundl=.false.
  call iotk_scan_begin(unit,name,lattr,found=foundl,ierr=ierrl)
  if(.not. foundl) goto 1
  foundl = .true.
  inside = .true.
  if(present(attr)) call iotk_strcpy(attr,lattr,ierrl)
  if(ierrl/=0) then
    call iotk_error_issue(ierrl,"iotk_scan_dat",__FILE__,__LINE__)
# 677 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.19 ")
    goto 1
  end if
  call iotk_parse_dat(lattr,rtype,rkind,rsize,rlen,fmt,columns,ierrl)
! Note that columns is not effectively used
  if(ierrl/=0) goto 1
  if(.not. (iotk_strcomp(rtype,iotk_eos) .or. iotk_strcomp(rtype,"CHARACTER") ) ) then
    call iotk_error_issue(ierrl,"iotk_scan_dat",__FILE__,__LINE__)
# 684 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.19 ")
# 684 "iotk_dat.spp"
call iotk_error_msg(ierrl,' ')
# 684 "iotk_dat.spp"
call iotk_error_write(ierrl,"rtype",rtype(1:iotk_strlen(rtype)))
# 684 "iotk_dat.spp"
call iotk_error_write(ierrl,"type","CHARACTER")
    goto 1
  end if
  if(.not. (rsize==-1 .or. rsize==size(dat)) ) then
    call iotk_error_issue(ierrl,"iotk_scan_dat",__FILE__,__LINE__)
# 688 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.19 ")
    goto 1
  end if
  if(rkind==-1) rkind = kind(dat)
# 693 "iotk_dat.spp"
  if(rlen ==-1) rlen  = len(dat)
# 695 "iotk_dat.spp"

  allocate(tmpdat(size(dat)))
  call iotk_scan_dat_aux(unit,tmpdat,rkind,rlen,fmt(1:iotk_strlen(fmt)),ierrl)
# 701 "iotk_dat.spp"
#if defined(__IOTK_WORKAROUND3) || defined(__IOTK_WORKAROUND4)
# 703 "iotk_dat.spp"
     call iotk_private_pack_CHARACTER1(dat,tmpdat,size(tmpdat),len(tmpdat))
# 707 "iotk_dat.spp"
#else
     dat = reshape(tmpdat,shape(dat))
#endif
# 711 "iotk_dat.spp"
  deallocate(tmpdat)
1 continue
  if(inside) then
    call iotk_scan_end(unit,name,ierr=ierrl2)
    if(ierrl2/=0) then
      call iotk_error_clear(ierrl)
      ierrl=ierrl2
    end if
  end if
  if(ierrl/=0) foundl=.false.
  if(present(found)) found = foundl
  if(ierrl==0 .and. .not. present(found) .and. .not. present(default) .and. .not. foundl) then
    call iotk_error_issue(ierrl,"iotk_scan_dat",__FILE__,__LINE__)
# 723 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.19 ")
# 723 "iotk_dat.spp"
call iotk_error_msg(ierrl,'Dat not found')
# 723 "iotk_dat.spp"
call iotk_error_write(ierrl,"name",name)
    ierrl = - ierrl
  end if 
  if(present(default) .and. .not. foundl) then
    dat=default
  end if
  if(present(ierr)) then
    ierr = ierrl
  else
    if(ierrl>0 .or. (.not.present(found) .and. .not.present(default))) call iotk_error_handler(ierrl)
  end if
end subroutine iotk_scan_dat_CHARACTER1_4


#endif
#endif

subroutine iotk_dat_dummy_CHARACTER1_4
  write(0,*)
end subroutine iotk_dat_dummy_CHARACTER1_4


# 45 "iotk_dat.spp"

# 65 "iotk_dat.spp"


!------------------------------------------------------------------------------!
! Inclusion of configuration file
#include "iotk_config.h"
!------------------------------------------------------------------------------!

# 74 "iotk_dat.spp"
#include "iotk_auxmacros.h"
# 76 "iotk_dat.spp"

# 78 "iotk_dat.spp"

#ifdef __IOTK_CHARACTER1
#if 5 <= __IOTK_MAXRANK
# 82 "iotk_dat.spp"
subroutine iotk_write_dat_CHARACTER1_5(unit,name,dat,dummy,attr,columns,sep,fmt,ierr)
  use iotk_base
  use iotk_error_interf
  use iotk_attr_interf, only : iotk_write_attr
  use iotk_write_interf
  use iotk_fmt_interf
  use iotk_str_interf
  use iotk_unit_interf
  use iotk_misc_interf
  implicit none
  integer,                intent(in)  :: unit
  character(*),           intent(in)  :: name
  CHARACTER (kind=__IOTK_CHARACTER1,len=*), intent(in)  :: dat (:,:,:,:,:) 
  type(iotk_dummytype), optional      :: dummy
  character(len=*), optional, intent(in)  :: attr
  integer,          optional, intent(in)  :: columns
  character(len=*), optional, intent(in)  :: sep
  character(len=*), optional, intent(in)  :: fmt
  integer,      optional, intent(out) :: ierr
  integer :: ierrl,lunit,iostat
  logical :: binary,raw
  integer :: lcolumns
  integer(iotk_header_kind), parameter :: idummy=0
  character(100) :: lsep
  character(300) :: usefmt,usefmt1
  character(iotk_attlenx) :: lattr
  character(iotk_attlenx) :: attr_tmp
  type (iotk_unit), pointer :: this
# 111 "iotk_dat.spp"
  CHARACTER (kind=__IOTK_CHARACTER1,len=len(dat)),allocatable :: dattmp(:)
  character(len=iotk_linlenx) :: linetmp
# 116 "iotk_dat.spp"
  integer :: itmp
  ierrl = 0
  iostat = 0
  lcolumns = 1
  lsep(1:2) = " "//iotk_eos
  if(present(columns)) lcolumns = columns
  if(present(sep)) then
    call iotk_strcpy(lsep,sep,ierrl)
    if(ierrl/=0) then
      call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 125 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.19 ")
      goto 1
    end if
  end if
  lunit = iotk_phys_unit(unit)
  call iotk_unit_get(lunit,pointer=this)
  raw = .false.
  if(associated(this)) then
    raw = this%raw
  end if
  call iotk_inquire(lunit,binary=binary,ierr=ierrl)
  if(ierrl/=0) then
    call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 137 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.19 ")
    goto 1
  end if
  call iotk_strcpy(usefmt,"!",ierrl)
  if(ierrl/=0) then
    call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 142 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.19 ")
    goto 1
  end if
  if(present(fmt) .and. .not. raw) call iotk_strcpy(usefmt,iotk_strtrim(fmt),ierrl)
  if(ierrl/=0) then
    call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 147 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.19 ")
    goto 1
  end if
  if(iotk_strscan(usefmt,"<>&")/=0) then
    call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 151 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.19 ")
# 151 "iotk_dat.spp"
call iotk_error_msg(ierrl,'Special characters (<>&) found in fmt string')
# 151 "iotk_dat.spp"
call iotk_error_write(ierrl,"unit",unit)
# 151 "iotk_dat.spp"
call iotk_error_write(ierrl,"name",trim(name))
# 151 "iotk_dat.spp"
call iotk_error_write(ierrl,"fmt",trim(fmt))
    goto 1
  end if
  call iotk_write_attr(lattr,"type",iotk_tolower("CHARACTER"),first=.true.,ierr=ierrl)
  if(ierrl/=0) then
    call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 156 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.19 ")
    goto 1
  end if
  call iotk_write_attr(lattr,"size",size(dat),ierr=ierrl)
  if(ierrl/=0) then
    call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 161 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.19 ")
    goto 1
  end if
# 165 "iotk_dat.spp"
  call iotk_write_attr(lattr,"len",len(dat),ierr=ierrl)
  if(ierrl/=0) then
    call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 167 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.19 ")
    goto 1
  end if
# 179 "iotk_dat.spp"
  if(.not.iotk_strcomp(usefmt,"!")) call iotk_write_attr(lattr,"fmt",iotk_strtrim(usefmt),ierr=ierrl)
  if(ierrl/=0) then
    call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 181 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.19 ")
    goto 1
  end if
  if(lcolumns/=1) call iotk_write_attr(lattr,"columns",lcolumns,ierr=ierrl)
  if(ierrl/=0) then
    call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 186 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.19 ")
    goto 1
  end if
  if(present(attr)) then
    attr_tmp(1:1)=iotk_eos
    call iotk_strcpy(attr_tmp,attr,ierr=ierrl)
    if(ierrl/=0) then
      call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 193 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.19 ")
      goto 1
    end if
    call iotk_delete_attr(attr_tmp,"type",ierrl)
    if(ierrl/=0) then
      call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 198 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.19 ")
      goto 1
    end if
    call iotk_delete_attr(attr_tmp,"kind",ierrl)
    if(ierrl/=0) then
      call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 203 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.19 ")
      goto 1
    end if
    call iotk_delete_attr(attr_tmp,"size",ierrl)
    if(ierrl/=0) then
      call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 208 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.19 ")
      goto 1
    end if
    call iotk_delete_attr(attr_tmp,"fmt",ierrl)
    if(ierrl/=0) then
      call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 213 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.19 ")
      goto 1
    end if
    call iotk_delete_attr(attr_tmp,"columns",ierrl)
    if(ierrl/=0) then
      call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 218 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.19 ")
      goto 1
    end if
    call iotk_delete_attr(attr_tmp,"len",ierrl)
    if(ierrl/=0) then
      call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 223 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.19 ")
      goto 1
    end if
    if(iotk_strlen_trim(attr_tmp)>0) call iotk_strcat(lattr,iotk_strtrim(attr_tmp),ierr=ierrl)
  end if
  if(ierrl/=0) then
    call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 229 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.19 ")
    goto 1
  end if
  call iotk_write_begin(unit,name,lattr,ierr=ierrl)
  if(ierrl/=0) then
    call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 234 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.19 ")
    goto 1
  end if

  allocate(dattmp(size(dat)))
# 242 "iotk_dat.spp"
#if defined(__IOTK_WORKAROUND3) || defined(__IOTK_WORKAROUND4)
# 244 "iotk_dat.spp"
     call iotk_private_pack_CHARACTER1(dattmp,dat,size(dattmp),len(dattmp))
# 248 "iotk_dat.spp"
#else
     dattmp = pack(dat,mask=.true.)
#endif
# 252 "iotk_dat.spp"

  if(binary) then
    if(raw) then
      write(lunit,iostat=iostat) (dattmp(itmp),itmp=1,size(dattmp))
      if(iostat/=0) then
        call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 257 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.19 ")
        goto 1
      end if
    else
      write(lunit,iostat=iostat) idummy,(dattmp(itmp),itmp=1,size(dattmp))
      if(iostat/=0) then
        call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 263 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.19 ")
        goto 1
      end if
    end if
  else
    if(raw) then
# 270 "iotk_dat.spp"
      write(lunit,"(a)",iostat=iostat) (dattmp(itmp),itmp=1,size(dattmp))
# 274 "iotk_dat.spp"
      if(iostat/=0) then
        call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 275 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.19 ")
        goto 1
      end if
    else if(iotk_strcomp(usefmt,"*")) then
      write(lunit,*,iostat=iostat) (dattmp(itmp),itmp=1,size(dattmp))
      if(iostat/=0) then
        call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 281 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.19 ")
        goto 1
      end if
    else if(iotk_strcomp(usefmt,"!")) then
# 286 "iotk_dat.spp"
     do itmp = 1 , size(dattmp)
       call iotk_deescape(linetmp,dattmp(itmp))
       write(lunit,"(a)",iostat=iostat) linetmp(1:iotk_strlen(linetmp))
       if(iostat/=0) then
        call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 290 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.19 ")
        goto 1
        end if
     end do
# 301 "iotk_dat.spp"
    else
      write(lunit,fmt=usefmt(1:iotk_strlen(usefmt)),iostat=iostat) (dattmp(itmp),itmp=1,size(dattmp))
      if(iostat/=0) then
        call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 304 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.19 ")
        goto 1
      end if
    end if
  end if
  call iotk_write_end(unit,name,ierr=ierrl)
  if(ierrl/=0) then
    call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 311 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.19 ")
    goto 1
  end if
1 continue
  if(allocated(dattmp)) deallocate(dattmp)
  if(present(ierr)) then
    ierr = ierrl
  else
    if(ierrl/=0) call iotk_error_handler(ierrl)
  end if
end subroutine iotk_write_dat_CHARACTER1_5


# 629 "iotk_dat.spp"

# 631 "iotk_dat.spp"
subroutine iotk_scan_dat_CHARACTER1_5(unit,name,dat,dummy,attr,found,default,ierr)
  use iotk_base
  use iotk_error_interf
  use iotk_dat_interf, only: iotk_scan_dat_aux
  use iotk_scan_interf
  use iotk_str_interf
  use iotk_misc_interf
  implicit none
  integer,                   intent(in)  :: unit
  character(*),              intent(in)  :: name
#ifdef __IOTK_WORKAROUND6
  CHARACTER (kind=__IOTK_CHARACTER1,len=*)                        :: dat (:,:,:,:,:)
#else
  CHARACTER (kind=__IOTK_CHARACTER1,len=*),           intent(out) :: dat (:,:,:,:,:)
#endif
  type(iotk_dummytype), optional         :: dummy
#ifdef __IOTK_WORKAROUND6
  character(len=*),optional              :: attr
#else
  character(len=*),optional, intent(out) :: attr
#endif
  logical,         optional, intent(out) :: found
  CHARACTER (kind=__IOTK_CHARACTER1,len=*), optional, intent(in)  :: default (:,:,:,:,:)
  integer,         optional, intent(out) :: ierr
# 656 "iotk_dat.spp"
  CHARACTER (kind=__IOTK_CHARACTER1,len=len(dat)), allocatable :: tmpdat(:)
# 660 "iotk_dat.spp"
  integer :: ierrl,ierrl2
  integer :: rkind,rsize,rlen
  character(iotk_vallenx) :: rtype
  character(iotk_vallenx) :: fmt
  character(iotk_attlenx) :: lattr
  integer :: columns
  logical :: inside,foundl
  inside = .false.
  ierrl = 0
  ierrl2 = 0
  foundl=.false.
  call iotk_scan_begin(unit,name,lattr,found=foundl,ierr=ierrl)
  if(.not. foundl) goto 1
  foundl = .true.
  inside = .true.
  if(present(attr)) call iotk_strcpy(attr,lattr,ierrl)
  if(ierrl/=0) then
    call iotk_error_issue(ierrl,"iotk_scan_dat",__FILE__,__LINE__)
# 677 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.19 ")
    goto 1
  end if
  call iotk_parse_dat(lattr,rtype,rkind,rsize,rlen,fmt,columns,ierrl)
! Note that columns is not effectively used
  if(ierrl/=0) goto 1
  if(.not. (iotk_strcomp(rtype,iotk_eos) .or. iotk_strcomp(rtype,"CHARACTER") ) ) then
    call iotk_error_issue(ierrl,"iotk_scan_dat",__FILE__,__LINE__)
# 684 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.19 ")
# 684 "iotk_dat.spp"
call iotk_error_msg(ierrl,' ')
# 684 "iotk_dat.spp"
call iotk_error_write(ierrl,"rtype",rtype(1:iotk_strlen(rtype)))
# 684 "iotk_dat.spp"
call iotk_error_write(ierrl,"type","CHARACTER")
    goto 1
  end if
  if(.not. (rsize==-1 .or. rsize==size(dat)) ) then
    call iotk_error_issue(ierrl,"iotk_scan_dat",__FILE__,__LINE__)
# 688 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.19 ")
    goto 1
  end if
  if(rkind==-1) rkind = kind(dat)
# 693 "iotk_dat.spp"
  if(rlen ==-1) rlen  = len(dat)
# 695 "iotk_dat.spp"

  allocate(tmpdat(size(dat)))
  call iotk_scan_dat_aux(unit,tmpdat,rkind,rlen,fmt(1:iotk_strlen(fmt)),ierrl)
# 701 "iotk_dat.spp"
#if defined(__IOTK_WORKAROUND3) || defined(__IOTK_WORKAROUND4)
# 703 "iotk_dat.spp"
     call iotk_private_pack_CHARACTER1(dat,tmpdat,size(tmpdat),len(tmpdat))
# 707 "iotk_dat.spp"
#else
     dat = reshape(tmpdat,shape(dat))
#endif
# 711 "iotk_dat.spp"
  deallocate(tmpdat)
1 continue
  if(inside) then
    call iotk_scan_end(unit,name,ierr=ierrl2)
    if(ierrl2/=0) then
      call iotk_error_clear(ierrl)
      ierrl=ierrl2
    end if
  end if
  if(ierrl/=0) foundl=.false.
  if(present(found)) found = foundl
  if(ierrl==0 .and. .not. present(found) .and. .not. present(default) .and. .not. foundl) then
    call iotk_error_issue(ierrl,"iotk_scan_dat",__FILE__,__LINE__)
# 723 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.19 ")
# 723 "iotk_dat.spp"
call iotk_error_msg(ierrl,'Dat not found')
# 723 "iotk_dat.spp"
call iotk_error_write(ierrl,"name",name)
    ierrl = - ierrl
  end if 
  if(present(default) .and. .not. foundl) then
    dat=default
  end if
  if(present(ierr)) then
    ierr = ierrl
  else
    if(ierrl>0 .or. (.not.present(found) .and. .not.present(default))) call iotk_error_handler(ierrl)
  end if
end subroutine iotk_scan_dat_CHARACTER1_5


#endif
#endif

subroutine iotk_dat_dummy_CHARACTER1_5
  write(0,*)
end subroutine iotk_dat_dummy_CHARACTER1_5


# 45 "iotk_dat.spp"

# 48 "iotk_dat.spp"
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
# 65 "iotk_dat.spp"


!------------------------------------------------------------------------------!
! Inclusion of configuration file
#include "iotk_config.h"
!------------------------------------------------------------------------------!

# 74 "iotk_dat.spp"
#include "iotk_auxmacros.h"
# 76 "iotk_dat.spp"

# 78 "iotk_dat.spp"

#ifdef __IOTK_CHARACTER1
#if 6 <= __IOTK_MAXRANK
# 82 "iotk_dat.spp"
subroutine iotk_write_dat_CHARACTER1_6(unit,name,dat,dummy,attr,columns,sep,fmt,ierr)
  use iotk_base
  use iotk_error_interf
  use iotk_attr_interf, only : iotk_write_attr
  use iotk_write_interf
  use iotk_fmt_interf
  use iotk_str_interf
  use iotk_unit_interf
  use iotk_misc_interf
  implicit none
  integer,                intent(in)  :: unit
  character(*),           intent(in)  :: name
  CHARACTER (kind=__IOTK_CHARACTER1,len=*), intent(in)  :: dat (:,:,:,:,:,:) 
  type(iotk_dummytype), optional      :: dummy
  character(len=*), optional, intent(in)  :: attr
  integer,          optional, intent(in)  :: columns
  character(len=*), optional, intent(in)  :: sep
  character(len=*), optional, intent(in)  :: fmt
  integer,      optional, intent(out) :: ierr
  integer :: ierrl,lunit,iostat
  logical :: binary,raw
  integer :: lcolumns
  integer(iotk_header_kind), parameter :: idummy=0
  character(100) :: lsep
  character(300) :: usefmt,usefmt1
  character(iotk_attlenx) :: lattr
  character(iotk_attlenx) :: attr_tmp
  type (iotk_unit), pointer :: this
# 111 "iotk_dat.spp"
  CHARACTER (kind=__IOTK_CHARACTER1,len=len(dat)),allocatable :: dattmp(:)
  character(len=iotk_linlenx) :: linetmp
# 116 "iotk_dat.spp"
  integer :: itmp
  ierrl = 0
  iostat = 0
  lcolumns = 1
  lsep(1:2) = " "//iotk_eos
  if(present(columns)) lcolumns = columns
  if(present(sep)) then
    call iotk_strcpy(lsep,sep,ierrl)
    if(ierrl/=0) then
      call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 125 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.19 ")
      goto 1
    end if
  end if
  lunit = iotk_phys_unit(unit)
  call iotk_unit_get(lunit,pointer=this)
  raw = .false.
  if(associated(this)) then
    raw = this%raw
  end if
  call iotk_inquire(lunit,binary=binary,ierr=ierrl)
  if(ierrl/=0) then
    call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 137 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.19 ")
    goto 1
  end if
  call iotk_strcpy(usefmt,"!",ierrl)
  if(ierrl/=0) then
    call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 142 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.19 ")
    goto 1
  end if
  if(present(fmt) .and. .not. raw) call iotk_strcpy(usefmt,iotk_strtrim(fmt),ierrl)
  if(ierrl/=0) then
    call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 147 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.19 ")
    goto 1
  end if
  if(iotk_strscan(usefmt,"<>&")/=0) then
    call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 151 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.19 ")
# 151 "iotk_dat.spp"
call iotk_error_msg(ierrl,'Special characters (<>&) found in fmt string')
# 151 "iotk_dat.spp"
call iotk_error_write(ierrl,"unit",unit)
# 151 "iotk_dat.spp"
call iotk_error_write(ierrl,"name",trim(name))
# 151 "iotk_dat.spp"
call iotk_error_write(ierrl,"fmt",trim(fmt))
    goto 1
  end if
  call iotk_write_attr(lattr,"type",iotk_tolower("CHARACTER"),first=.true.,ierr=ierrl)
  if(ierrl/=0) then
    call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 156 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.19 ")
    goto 1
  end if
  call iotk_write_attr(lattr,"size",size(dat),ierr=ierrl)
  if(ierrl/=0) then
    call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 161 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.19 ")
    goto 1
  end if
# 165 "iotk_dat.spp"
  call iotk_write_attr(lattr,"len",len(dat),ierr=ierrl)
  if(ierrl/=0) then
    call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 167 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.19 ")
    goto 1
  end if
# 179 "iotk_dat.spp"
  if(.not.iotk_strcomp(usefmt,"!")) call iotk_write_attr(lattr,"fmt",iotk_strtrim(usefmt),ierr=ierrl)
  if(ierrl/=0) then
    call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 181 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.19 ")
    goto 1
  end if
  if(lcolumns/=1) call iotk_write_attr(lattr,"columns",lcolumns,ierr=ierrl)
  if(ierrl/=0) then
    call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 186 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.19 ")
    goto 1
  end if
  if(present(attr)) then
    attr_tmp(1:1)=iotk_eos
    call iotk_strcpy(attr_tmp,attr,ierr=ierrl)
    if(ierrl/=0) then
      call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 193 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.19 ")
      goto 1
    end if
    call iotk_delete_attr(attr_tmp,"type",ierrl)
    if(ierrl/=0) then
      call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 198 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.19 ")
      goto 1
    end if
    call iotk_delete_attr(attr_tmp,"kind",ierrl)
    if(ierrl/=0) then
      call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 203 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.19 ")
      goto 1
    end if
    call iotk_delete_attr(attr_tmp,"size",ierrl)
    if(ierrl/=0) then
      call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 208 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.19 ")
      goto 1
    end if
    call iotk_delete_attr(attr_tmp,"fmt",ierrl)
    if(ierrl/=0) then
      call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 213 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.19 ")
      goto 1
    end if
    call iotk_delete_attr(attr_tmp,"columns",ierrl)
    if(ierrl/=0) then
      call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 218 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.19 ")
      goto 1
    end if
    call iotk_delete_attr(attr_tmp,"len",ierrl)
    if(ierrl/=0) then
      call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 223 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.19 ")
      goto 1
    end if
    if(iotk_strlen_trim(attr_tmp)>0) call iotk_strcat(lattr,iotk_strtrim(attr_tmp),ierr=ierrl)
  end if
  if(ierrl/=0) then
    call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 229 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.19 ")
    goto 1
  end if
  call iotk_write_begin(unit,name,lattr,ierr=ierrl)
  if(ierrl/=0) then
    call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 234 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.19 ")
    goto 1
  end if

  allocate(dattmp(size(dat)))
# 242 "iotk_dat.spp"
#if defined(__IOTK_WORKAROUND3) || defined(__IOTK_WORKAROUND4)
# 244 "iotk_dat.spp"
     call iotk_private_pack_CHARACTER1(dattmp,dat,size(dattmp),len(dattmp))
# 248 "iotk_dat.spp"
#else
     dattmp = pack(dat,mask=.true.)
#endif
# 252 "iotk_dat.spp"

  if(binary) then
    if(raw) then
      write(lunit,iostat=iostat) (dattmp(itmp),itmp=1,size(dattmp))
      if(iostat/=0) then
        call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 257 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.19 ")
        goto 1
      end if
    else
      write(lunit,iostat=iostat) idummy,(dattmp(itmp),itmp=1,size(dattmp))
      if(iostat/=0) then
        call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 263 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.19 ")
        goto 1
      end if
    end if
  else
    if(raw) then
# 270 "iotk_dat.spp"
      write(lunit,"(a)",iostat=iostat) (dattmp(itmp),itmp=1,size(dattmp))
# 274 "iotk_dat.spp"
      if(iostat/=0) then
        call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 275 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.19 ")
        goto 1
      end if
    else if(iotk_strcomp(usefmt,"*")) then
      write(lunit,*,iostat=iostat) (dattmp(itmp),itmp=1,size(dattmp))
      if(iostat/=0) then
        call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 281 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.19 ")
        goto 1
      end if
    else if(iotk_strcomp(usefmt,"!")) then
# 286 "iotk_dat.spp"
     do itmp = 1 , size(dattmp)
       call iotk_deescape(linetmp,dattmp(itmp))
       write(lunit,"(a)",iostat=iostat) linetmp(1:iotk_strlen(linetmp))
       if(iostat/=0) then
        call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 290 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.19 ")
        goto 1
        end if
     end do
# 301 "iotk_dat.spp"
    else
      write(lunit,fmt=usefmt(1:iotk_strlen(usefmt)),iostat=iostat) (dattmp(itmp),itmp=1,size(dattmp))
      if(iostat/=0) then
        call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 304 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.19 ")
        goto 1
      end if
    end if
  end if
  call iotk_write_end(unit,name,ierr=ierrl)
  if(ierrl/=0) then
    call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 311 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.19 ")
    goto 1
  end if
1 continue
  if(allocated(dattmp)) deallocate(dattmp)
  if(present(ierr)) then
    ierr = ierrl
  else
    if(ierrl/=0) call iotk_error_handler(ierrl)
  end if
end subroutine iotk_write_dat_CHARACTER1_6


# 629 "iotk_dat.spp"

# 631 "iotk_dat.spp"
subroutine iotk_scan_dat_CHARACTER1_6(unit,name,dat,dummy,attr,found,default,ierr)
  use iotk_base
  use iotk_error_interf
  use iotk_dat_interf, only: iotk_scan_dat_aux
  use iotk_scan_interf
  use iotk_str_interf
  use iotk_misc_interf
  implicit none
  integer,                   intent(in)  :: unit
  character(*),              intent(in)  :: name
#ifdef __IOTK_WORKAROUND6
  CHARACTER (kind=__IOTK_CHARACTER1,len=*)                        :: dat (:,:,:,:,:,:)
#else
  CHARACTER (kind=__IOTK_CHARACTER1,len=*),           intent(out) :: dat (:,:,:,:,:,:)
#endif
  type(iotk_dummytype), optional         :: dummy
#ifdef __IOTK_WORKAROUND6
  character(len=*),optional              :: attr
#else
  character(len=*),optional, intent(out) :: attr
#endif
  logical,         optional, intent(out) :: found
  CHARACTER (kind=__IOTK_CHARACTER1,len=*), optional, intent(in)  :: default (:,:,:,:,:,:)
  integer,         optional, intent(out) :: ierr
# 656 "iotk_dat.spp"
  CHARACTER (kind=__IOTK_CHARACTER1,len=len(dat)), allocatable :: tmpdat(:)
# 660 "iotk_dat.spp"
  integer :: ierrl,ierrl2
  integer :: rkind,rsize,rlen
  character(iotk_vallenx) :: rtype
  character(iotk_vallenx) :: fmt
  character(iotk_attlenx) :: lattr
  integer :: columns
  logical :: inside,foundl
  inside = .false.
  ierrl = 0
  ierrl2 = 0
  foundl=.false.
  call iotk_scan_begin(unit,name,lattr,found=foundl,ierr=ierrl)
  if(.not. foundl) goto 1
  foundl = .true.
  inside = .true.
  if(present(attr)) call iotk_strcpy(attr,lattr,ierrl)
  if(ierrl/=0) then
    call iotk_error_issue(ierrl,"iotk_scan_dat",__FILE__,__LINE__)
# 677 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.19 ")
    goto 1
  end if
  call iotk_parse_dat(lattr,rtype,rkind,rsize,rlen,fmt,columns,ierrl)
! Note that columns is not effectively used
  if(ierrl/=0) goto 1
  if(.not. (iotk_strcomp(rtype,iotk_eos) .or. iotk_strcomp(rtype,"CHARACTER") ) ) then
    call iotk_error_issue(ierrl,"iotk_scan_dat",__FILE__,__LINE__)
# 684 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.19 ")
# 684 "iotk_dat.spp"
call iotk_error_msg(ierrl,' ')
# 684 "iotk_dat.spp"
call iotk_error_write(ierrl,"rtype",rtype(1:iotk_strlen(rtype)))
# 684 "iotk_dat.spp"
call iotk_error_write(ierrl,"type","CHARACTER")
    goto 1
  end if
  if(.not. (rsize==-1 .or. rsize==size(dat)) ) then
    call iotk_error_issue(ierrl,"iotk_scan_dat",__FILE__,__LINE__)
# 688 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.19 ")
    goto 1
  end if
  if(rkind==-1) rkind = kind(dat)
# 693 "iotk_dat.spp"
  if(rlen ==-1) rlen  = len(dat)
# 695 "iotk_dat.spp"

  allocate(tmpdat(size(dat)))
  call iotk_scan_dat_aux(unit,tmpdat,rkind,rlen,fmt(1:iotk_strlen(fmt)),ierrl)
# 701 "iotk_dat.spp"
#if defined(__IOTK_WORKAROUND3) || defined(__IOTK_WORKAROUND4)
# 703 "iotk_dat.spp"
     call iotk_private_pack_CHARACTER1(dat,tmpdat,size(tmpdat),len(tmpdat))
# 707 "iotk_dat.spp"
#else
     dat = reshape(tmpdat,shape(dat))
#endif
# 711 "iotk_dat.spp"
  deallocate(tmpdat)
1 continue
  if(inside) then
    call iotk_scan_end(unit,name,ierr=ierrl2)
    if(ierrl2/=0) then
      call iotk_error_clear(ierrl)
      ierrl=ierrl2
    end if
  end if
  if(ierrl/=0) foundl=.false.
  if(present(found)) found = foundl
  if(ierrl==0 .and. .not. present(found) .and. .not. present(default) .and. .not. foundl) then
    call iotk_error_issue(ierrl,"iotk_scan_dat",__FILE__,__LINE__)
# 723 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.19 ")
# 723 "iotk_dat.spp"
call iotk_error_msg(ierrl,'Dat not found')
# 723 "iotk_dat.spp"
call iotk_error_write(ierrl,"name",name)
    ierrl = - ierrl
  end if 
  if(present(default) .and. .not. foundl) then
    dat=default
  end if
  if(present(ierr)) then
    ierr = ierrl
  else
    if(ierrl>0 .or. (.not.present(found) .and. .not.present(default))) call iotk_error_handler(ierrl)
  end if
end subroutine iotk_scan_dat_CHARACTER1_6


#endif
#endif

subroutine iotk_dat_dummy_CHARACTER1_6
  write(0,*)
end subroutine iotk_dat_dummy_CHARACTER1_6


# 45 "iotk_dat.spp"

# 65 "iotk_dat.spp"


!------------------------------------------------------------------------------!
! Inclusion of configuration file
#include "iotk_config.h"
!------------------------------------------------------------------------------!

# 74 "iotk_dat.spp"
#include "iotk_auxmacros.h"
# 76 "iotk_dat.spp"

# 78 "iotk_dat.spp"

#ifdef __IOTK_CHARACTER1
#if 7 <= __IOTK_MAXRANK
# 82 "iotk_dat.spp"
subroutine iotk_write_dat_CHARACTER1_7(unit,name,dat,dummy,attr,columns,sep,fmt,ierr)
  use iotk_base
  use iotk_error_interf
  use iotk_attr_interf, only : iotk_write_attr
  use iotk_write_interf
  use iotk_fmt_interf
  use iotk_str_interf
  use iotk_unit_interf
  use iotk_misc_interf
  implicit none
  integer,                intent(in)  :: unit
  character(*),           intent(in)  :: name
  CHARACTER (kind=__IOTK_CHARACTER1,len=*), intent(in)  :: dat (:,:,:,:,:,:,:) 
  type(iotk_dummytype), optional      :: dummy
  character(len=*), optional, intent(in)  :: attr
  integer,          optional, intent(in)  :: columns
  character(len=*), optional, intent(in)  :: sep
  character(len=*), optional, intent(in)  :: fmt
  integer,      optional, intent(out) :: ierr
  integer :: ierrl,lunit,iostat
  logical :: binary,raw
  integer :: lcolumns
  integer(iotk_header_kind), parameter :: idummy=0
  character(100) :: lsep
  character(300) :: usefmt,usefmt1
  character(iotk_attlenx) :: lattr
  character(iotk_attlenx) :: attr_tmp
  type (iotk_unit), pointer :: this
# 111 "iotk_dat.spp"
  CHARACTER (kind=__IOTK_CHARACTER1,len=len(dat)),allocatable :: dattmp(:)
  character(len=iotk_linlenx) :: linetmp
# 116 "iotk_dat.spp"
  integer :: itmp
  ierrl = 0
  iostat = 0
  lcolumns = 1
  lsep(1:2) = " "//iotk_eos
  if(present(columns)) lcolumns = columns
  if(present(sep)) then
    call iotk_strcpy(lsep,sep,ierrl)
    if(ierrl/=0) then
      call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 125 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.19 ")
      goto 1
    end if
  end if
  lunit = iotk_phys_unit(unit)
  call iotk_unit_get(lunit,pointer=this)
  raw = .false.
  if(associated(this)) then
    raw = this%raw
  end if
  call iotk_inquire(lunit,binary=binary,ierr=ierrl)
  if(ierrl/=0) then
    call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 137 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.19 ")
    goto 1
  end if
  call iotk_strcpy(usefmt,"!",ierrl)
  if(ierrl/=0) then
    call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 142 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.19 ")
    goto 1
  end if
  if(present(fmt) .and. .not. raw) call iotk_strcpy(usefmt,iotk_strtrim(fmt),ierrl)
  if(ierrl/=0) then
    call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 147 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.19 ")
    goto 1
  end if
  if(iotk_strscan(usefmt,"<>&")/=0) then
    call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 151 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.19 ")
# 151 "iotk_dat.spp"
call iotk_error_msg(ierrl,'Special characters (<>&) found in fmt string')
# 151 "iotk_dat.spp"
call iotk_error_write(ierrl,"unit",unit)
# 151 "iotk_dat.spp"
call iotk_error_write(ierrl,"name",trim(name))
# 151 "iotk_dat.spp"
call iotk_error_write(ierrl,"fmt",trim(fmt))
    goto 1
  end if
  call iotk_write_attr(lattr,"type",iotk_tolower("CHARACTER"),first=.true.,ierr=ierrl)
  if(ierrl/=0) then
    call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 156 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.19 ")
    goto 1
  end if
  call iotk_write_attr(lattr,"size",size(dat),ierr=ierrl)
  if(ierrl/=0) then
    call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 161 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.19 ")
    goto 1
  end if
# 165 "iotk_dat.spp"
  call iotk_write_attr(lattr,"len",len(dat),ierr=ierrl)
  if(ierrl/=0) then
    call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 167 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.19 ")
    goto 1
  end if
# 179 "iotk_dat.spp"
  if(.not.iotk_strcomp(usefmt,"!")) call iotk_write_attr(lattr,"fmt",iotk_strtrim(usefmt),ierr=ierrl)
  if(ierrl/=0) then
    call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 181 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.19 ")
    goto 1
  end if
  if(lcolumns/=1) call iotk_write_attr(lattr,"columns",lcolumns,ierr=ierrl)
  if(ierrl/=0) then
    call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 186 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.19 ")
    goto 1
  end if
  if(present(attr)) then
    attr_tmp(1:1)=iotk_eos
    call iotk_strcpy(attr_tmp,attr,ierr=ierrl)
    if(ierrl/=0) then
      call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 193 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.19 ")
      goto 1
    end if
    call iotk_delete_attr(attr_tmp,"type",ierrl)
    if(ierrl/=0) then
      call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 198 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.19 ")
      goto 1
    end if
    call iotk_delete_attr(attr_tmp,"kind",ierrl)
    if(ierrl/=0) then
      call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 203 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.19 ")
      goto 1
    end if
    call iotk_delete_attr(attr_tmp,"size",ierrl)
    if(ierrl/=0) then
      call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 208 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.19 ")
      goto 1
    end if
    call iotk_delete_attr(attr_tmp,"fmt",ierrl)
    if(ierrl/=0) then
      call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 213 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.19 ")
      goto 1
    end if
    call iotk_delete_attr(attr_tmp,"columns",ierrl)
    if(ierrl/=0) then
      call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 218 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.19 ")
      goto 1
    end if
    call iotk_delete_attr(attr_tmp,"len",ierrl)
    if(ierrl/=0) then
      call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 223 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.19 ")
      goto 1
    end if
    if(iotk_strlen_trim(attr_tmp)>0) call iotk_strcat(lattr,iotk_strtrim(attr_tmp),ierr=ierrl)
  end if
  if(ierrl/=0) then
    call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 229 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.19 ")
    goto 1
  end if
  call iotk_write_begin(unit,name,lattr,ierr=ierrl)
  if(ierrl/=0) then
    call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 234 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.19 ")
    goto 1
  end if

  allocate(dattmp(size(dat)))
# 242 "iotk_dat.spp"
#if defined(__IOTK_WORKAROUND3) || defined(__IOTK_WORKAROUND4)
# 244 "iotk_dat.spp"
     call iotk_private_pack_CHARACTER1(dattmp,dat,size(dattmp),len(dattmp))
# 248 "iotk_dat.spp"
#else
     dattmp = pack(dat,mask=.true.)
#endif
# 252 "iotk_dat.spp"

  if(binary) then
    if(raw) then
      write(lunit,iostat=iostat) (dattmp(itmp),itmp=1,size(dattmp))
      if(iostat/=0) then
        call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 257 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.19 ")
        goto 1
      end if
    else
      write(lunit,iostat=iostat) idummy,(dattmp(itmp),itmp=1,size(dattmp))
      if(iostat/=0) then
        call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 263 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.19 ")
        goto 1
      end if
    end if
  else
    if(raw) then
# 270 "iotk_dat.spp"
      write(lunit,"(a)",iostat=iostat) (dattmp(itmp),itmp=1,size(dattmp))
# 274 "iotk_dat.spp"
      if(iostat/=0) then
        call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 275 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.19 ")
        goto 1
      end if
    else if(iotk_strcomp(usefmt,"*")) then
      write(lunit,*,iostat=iostat) (dattmp(itmp),itmp=1,size(dattmp))
      if(iostat/=0) then
        call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 281 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.19 ")
        goto 1
      end if
    else if(iotk_strcomp(usefmt,"!")) then
# 286 "iotk_dat.spp"
     do itmp = 1 , size(dattmp)
       call iotk_deescape(linetmp,dattmp(itmp))
       write(lunit,"(a)",iostat=iostat) linetmp(1:iotk_strlen(linetmp))
       if(iostat/=0) then
        call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 290 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.19 ")
        goto 1
        end if
     end do
# 301 "iotk_dat.spp"
    else
      write(lunit,fmt=usefmt(1:iotk_strlen(usefmt)),iostat=iostat) (dattmp(itmp),itmp=1,size(dattmp))
      if(iostat/=0) then
        call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 304 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.19 ")
        goto 1
      end if
    end if
  end if
  call iotk_write_end(unit,name,ierr=ierrl)
  if(ierrl/=0) then
    call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 311 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.19 ")
    goto 1
  end if
1 continue
  if(allocated(dattmp)) deallocate(dattmp)
  if(present(ierr)) then
    ierr = ierrl
  else
    if(ierrl/=0) call iotk_error_handler(ierrl)
  end if
end subroutine iotk_write_dat_CHARACTER1_7


# 629 "iotk_dat.spp"

# 631 "iotk_dat.spp"
subroutine iotk_scan_dat_CHARACTER1_7(unit,name,dat,dummy,attr,found,default,ierr)
  use iotk_base
  use iotk_error_interf
  use iotk_dat_interf, only: iotk_scan_dat_aux
  use iotk_scan_interf
  use iotk_str_interf
  use iotk_misc_interf
  implicit none
  integer,                   intent(in)  :: unit
  character(*),              intent(in)  :: name
#ifdef __IOTK_WORKAROUND6
  CHARACTER (kind=__IOTK_CHARACTER1,len=*)                        :: dat (:,:,:,:,:,:,:)
#else
  CHARACTER (kind=__IOTK_CHARACTER1,len=*),           intent(out) :: dat (:,:,:,:,:,:,:)
#endif
  type(iotk_dummytype), optional         :: dummy
#ifdef __IOTK_WORKAROUND6
  character(len=*),optional              :: attr
#else
  character(len=*),optional, intent(out) :: attr
#endif
  logical,         optional, intent(out) :: found
  CHARACTER (kind=__IOTK_CHARACTER1,len=*), optional, intent(in)  :: default (:,:,:,:,:,:,:)
  integer,         optional, intent(out) :: ierr
# 656 "iotk_dat.spp"
  CHARACTER (kind=__IOTK_CHARACTER1,len=len(dat)), allocatable :: tmpdat(:)
# 660 "iotk_dat.spp"
  integer :: ierrl,ierrl2
  integer :: rkind,rsize,rlen
  character(iotk_vallenx) :: rtype
  character(iotk_vallenx) :: fmt
  character(iotk_attlenx) :: lattr
  integer :: columns
  logical :: inside,foundl
  inside = .false.
  ierrl = 0
  ierrl2 = 0
  foundl=.false.
  call iotk_scan_begin(unit,name,lattr,found=foundl,ierr=ierrl)
  if(.not. foundl) goto 1
  foundl = .true.
  inside = .true.
  if(present(attr)) call iotk_strcpy(attr,lattr,ierrl)
  if(ierrl/=0) then
    call iotk_error_issue(ierrl,"iotk_scan_dat",__FILE__,__LINE__)
# 677 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.19 ")
    goto 1
  end if
  call iotk_parse_dat(lattr,rtype,rkind,rsize,rlen,fmt,columns,ierrl)
! Note that columns is not effectively used
  if(ierrl/=0) goto 1
  if(.not. (iotk_strcomp(rtype,iotk_eos) .or. iotk_strcomp(rtype,"CHARACTER") ) ) then
    call iotk_error_issue(ierrl,"iotk_scan_dat",__FILE__,__LINE__)
# 684 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.19 ")
# 684 "iotk_dat.spp"
call iotk_error_msg(ierrl,' ')
# 684 "iotk_dat.spp"
call iotk_error_write(ierrl,"rtype",rtype(1:iotk_strlen(rtype)))
# 684 "iotk_dat.spp"
call iotk_error_write(ierrl,"type","CHARACTER")
    goto 1
  end if
  if(.not. (rsize==-1 .or. rsize==size(dat)) ) then
    call iotk_error_issue(ierrl,"iotk_scan_dat",__FILE__,__LINE__)
# 688 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.19 ")
    goto 1
  end if
  if(rkind==-1) rkind = kind(dat)
# 693 "iotk_dat.spp"
  if(rlen ==-1) rlen  = len(dat)
# 695 "iotk_dat.spp"

  allocate(tmpdat(size(dat)))
  call iotk_scan_dat_aux(unit,tmpdat,rkind,rlen,fmt(1:iotk_strlen(fmt)),ierrl)
# 701 "iotk_dat.spp"
#if defined(__IOTK_WORKAROUND3) || defined(__IOTK_WORKAROUND4)
# 703 "iotk_dat.spp"
     call iotk_private_pack_CHARACTER1(dat,tmpdat,size(tmpdat),len(tmpdat))
# 707 "iotk_dat.spp"
#else
     dat = reshape(tmpdat,shape(dat))
#endif
# 711 "iotk_dat.spp"
  deallocate(tmpdat)
1 continue
  if(inside) then
    call iotk_scan_end(unit,name,ierr=ierrl2)
    if(ierrl2/=0) then
      call iotk_error_clear(ierrl)
      ierrl=ierrl2
    end if
  end if
  if(ierrl/=0) foundl=.false.
  if(present(found)) found = foundl
  if(ierrl==0 .and. .not. present(found) .and. .not. present(default) .and. .not. foundl) then
    call iotk_error_issue(ierrl,"iotk_scan_dat",__FILE__,__LINE__)
# 723 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.19 ")
# 723 "iotk_dat.spp"
call iotk_error_msg(ierrl,'Dat not found')
# 723 "iotk_dat.spp"
call iotk_error_write(ierrl,"name",name)
    ierrl = - ierrl
  end if 
  if(present(default) .and. .not. foundl) then
    dat=default
  end if
  if(present(ierr)) then
    ierr = ierrl
  else
    if(ierrl>0 .or. (.not.present(found) .and. .not.present(default))) call iotk_error_handler(ierrl)
  end if
end subroutine iotk_scan_dat_CHARACTER1_7


#endif
#endif

subroutine iotk_dat_dummy_CHARACTER1_7
  write(0,*)
end subroutine iotk_dat_dummy_CHARACTER1_7


