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

#ifdef __IOTK_INTEGER1
#if 0 <= __IOTK_MAXRANK
# 82 "iotk_dat.spp"
subroutine iotk_write_dat_INTEGER1_0(unit,name,dat,dummy,attr,columns,sep,fmt,ierr)
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
  INTEGER (kind=__IOTK_INTEGER1), intent(in)  :: dat  
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
# 114 "iotk_dat.spp"
  INTEGER (kind=__IOTK_INTEGER1),allocatable :: dattmp(:)
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
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
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
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
    goto 1
  end if
  call iotk_strcpy(usefmt,"!",ierrl)
  if(ierrl/=0) then
    call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 142 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
    goto 1
  end if
  if(present(fmt) .and. .not. raw) call iotk_strcpy(usefmt,iotk_strtrim(fmt),ierrl)
  if(ierrl/=0) then
    call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 147 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
    goto 1
  end if
  if(iotk_strscan(usefmt,"<>&")/=0) then
    call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 151 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
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
  call iotk_write_attr(lattr,"type",iotk_tolower("INTEGER"),first=.true.,ierr=ierrl)
  if(ierrl/=0) then
    call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 156 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
    goto 1
  end if
  call iotk_write_attr(lattr,"size",1,ierr=ierrl)
  if(ierrl/=0) then
    call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 161 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
    goto 1
  end if
# 171 "iotk_dat.spp"
  if(binary) then
    call iotk_write_attr(lattr,"kind",kind(dat),ierr=ierrl)
    if(ierrl/=0) then
      call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 174 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
      goto 1
    end if
  end if
# 179 "iotk_dat.spp"
  if(.not.iotk_strcomp(usefmt,"!")) call iotk_write_attr(lattr,"fmt",iotk_strtrim(usefmt),ierr=ierrl)
  if(ierrl/=0) then
    call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 181 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
    goto 1
  end if
  if(lcolumns/=1) call iotk_write_attr(lattr,"columns",lcolumns,ierr=ierrl)
  if(ierrl/=0) then
    call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 186 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
    goto 1
  end if
  if(present(attr)) then
    attr_tmp(1:1)=iotk_eos
    call iotk_strcpy(attr_tmp,attr,ierr=ierrl)
    if(ierrl/=0) then
      call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 193 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
      goto 1
    end if
    call iotk_delete_attr(attr_tmp,"type",ierrl)
    if(ierrl/=0) then
      call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 198 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
      goto 1
    end if
    call iotk_delete_attr(attr_tmp,"kind",ierrl)
    if(ierrl/=0) then
      call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 203 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
      goto 1
    end if
    call iotk_delete_attr(attr_tmp,"size",ierrl)
    if(ierrl/=0) then
      call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 208 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
      goto 1
    end if
    call iotk_delete_attr(attr_tmp,"fmt",ierrl)
    if(ierrl/=0) then
      call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 213 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
      goto 1
    end if
    call iotk_delete_attr(attr_tmp,"columns",ierrl)
    if(ierrl/=0) then
      call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 218 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
      goto 1
    end if
    call iotk_delete_attr(attr_tmp,"len",ierrl)
    if(ierrl/=0) then
      call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 223 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
      goto 1
    end if
    if(iotk_strlen_trim(attr_tmp)>0) call iotk_strcat(lattr,iotk_strtrim(attr_tmp),ierr=ierrl)
  end if
  if(ierrl/=0) then
    call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 229 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
    goto 1
  end if
  call iotk_write_begin(unit,name,lattr,ierr=ierrl)
  if(ierrl/=0) then
    call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 234 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
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
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
        goto 1
      end if
    else
      write(lunit,iostat=iostat) idummy,(dattmp(itmp),itmp=1,size(dattmp))
      if(iostat/=0) then
        call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 263 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
        goto 1
      end if
    end if
  else
    if(raw) then
# 272 "iotk_dat.spp"
      write(lunit,*,iostat=iostat) (dattmp(itmp),itmp=1,size(dattmp))
# 274 "iotk_dat.spp"
      if(iostat/=0) then
        call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 275 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
        goto 1
      end if
    else if(iotk_strcomp(usefmt,"*")) then
      write(lunit,*,iostat=iostat) (dattmp(itmp),itmp=1,size(dattmp))
      if(iostat/=0) then
        call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 281 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
        goto 1
      end if
    else if(iotk_strcomp(usefmt,"!")) then
# 295 "iotk_dat.spp"
     write(lunit,fmt=trim(iotk_wfmt("INTEGER",kind(dattmp),lcolumns,-1,lsep)),iostat=iostat) (dattmp(itmp),itmp=1,size(dattmp))
     if(iostat/=0) then
      call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 297 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
      goto 1
     end if
# 301 "iotk_dat.spp"
    else
      write(lunit,fmt=usefmt(1:iotk_strlen(usefmt)),iostat=iostat) (dattmp(itmp),itmp=1,size(dattmp))
      if(iostat/=0) then
        call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 304 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
        goto 1
      end if
    end if
  end if
  call iotk_write_end(unit,name,ierr=ierrl)
  if(ierrl/=0) then
    call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 311 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
    goto 1
  end if
1 continue
  if(allocated(dattmp)) deallocate(dattmp)
  if(present(ierr)) then
    ierr = ierrl
  else
    if(ierrl/=0) call iotk_error_handler(ierrl)
  end if
end subroutine iotk_write_dat_INTEGER1_0


# 589 "iotk_dat.spp"

# 591 "iotk_dat.spp"
subroutine iotk_scan_dat_INTEGER1_0(unit,name,dat,dummy,attr,found,default,ierr)
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
  INTEGER (kind=__IOTK_INTEGER1)                        :: dat 
#else
  INTEGER (kind=__IOTK_INTEGER1),           intent(out) :: dat 
#endif
  type(iotk_dummytype), optional         :: dummy
#ifdef __IOTK_WORKAROUND6
  character(len=*),optional              :: attr
#else
  character(len=*),optional, intent(out) :: attr
#endif
  logical,         optional, intent(out) :: found
  INTEGER (kind=__IOTK_INTEGER1), optional, intent(in)  :: default 
  integer,         optional, intent(out) :: ierr
# 618 "iotk_dat.spp"
  INTEGER (kind=__IOTK_INTEGER1),              allocatable :: tmpdat(:)
# 620 "iotk_dat.spp"
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
# 637 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
    goto 1
  end if
  call iotk_parse_dat(lattr,rtype,rkind,rsize,rlen,fmt,columns,ierrl)
! Note that columns is not effectively used
  if(ierrl/=0) goto 1
  if(.not. (iotk_strcomp(rtype,iotk_eos) .or. iotk_strcomp(rtype,"INTEGER") ) ) then
    call iotk_error_issue(ierrl,"iotk_scan_dat",__FILE__,__LINE__)
# 644 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
# 644 "iotk_dat.spp"
call iotk_error_msg(ierrl,' ')
# 644 "iotk_dat.spp"
call iotk_error_write(ierrl,"rtype",rtype(1:iotk_strlen(rtype)))
# 644 "iotk_dat.spp"
call iotk_error_write(ierrl,"type","INTEGER")
    goto 1
  end if
  if(.not. (rsize==-1 .or. rsize==1) ) then
    call iotk_error_issue(ierrl,"iotk_scan_dat",__FILE__,__LINE__)
# 648 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
    goto 1
  end if
  if(rkind==-1) rkind = kind(dat)
# 655 "iotk_dat.spp"

  allocate(tmpdat(1))
  call iotk_scan_dat_aux(unit,tmpdat,rkind,rlen,fmt(1:iotk_strlen(fmt)),ierrl)
# 659 "iotk_dat.spp"
        dat = tmpdat(1)
# 671 "iotk_dat.spp"
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
# 683 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
# 683 "iotk_dat.spp"
call iotk_error_msg(ierrl,'Dat not found')
# 683 "iotk_dat.spp"
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
end subroutine iotk_scan_dat_INTEGER1_0


#endif
#endif

subroutine iotk_dat_dummy_INTEGER1_0
  write(0,*)
end subroutine iotk_dat_dummy_INTEGER1_0


# 727 "iotk_dat.spp"


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

#ifdef __IOTK_INTEGER1
#if 1 <= __IOTK_MAXRANK
# 82 "iotk_dat.spp"
subroutine iotk_write_dat_INTEGER1_1(unit,name,dat,dummy,attr,columns,sep,fmt,ierr)
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
  INTEGER (kind=__IOTK_INTEGER1), intent(in)  :: dat (:) 
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
# 114 "iotk_dat.spp"
  INTEGER (kind=__IOTK_INTEGER1),allocatable :: dattmp(:)
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
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
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
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
    goto 1
  end if
  call iotk_strcpy(usefmt,"!",ierrl)
  if(ierrl/=0) then
    call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 142 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
    goto 1
  end if
  if(present(fmt) .and. .not. raw) call iotk_strcpy(usefmt,iotk_strtrim(fmt),ierrl)
  if(ierrl/=0) then
    call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 147 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
    goto 1
  end if
  if(iotk_strscan(usefmt,"<>&")/=0) then
    call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 151 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
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
  call iotk_write_attr(lattr,"type",iotk_tolower("INTEGER"),first=.true.,ierr=ierrl)
  if(ierrl/=0) then
    call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 156 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
    goto 1
  end if
  call iotk_write_attr(lattr,"size",size(dat),ierr=ierrl)
  if(ierrl/=0) then
    call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 161 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
    goto 1
  end if
# 171 "iotk_dat.spp"
  if(binary) then
    call iotk_write_attr(lattr,"kind",kind(dat),ierr=ierrl)
    if(ierrl/=0) then
      call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 174 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
      goto 1
    end if
  end if
# 179 "iotk_dat.spp"
  if(.not.iotk_strcomp(usefmt,"!")) call iotk_write_attr(lattr,"fmt",iotk_strtrim(usefmt),ierr=ierrl)
  if(ierrl/=0) then
    call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 181 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
    goto 1
  end if
  if(lcolumns/=1) call iotk_write_attr(lattr,"columns",lcolumns,ierr=ierrl)
  if(ierrl/=0) then
    call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 186 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
    goto 1
  end if
  if(present(attr)) then
    attr_tmp(1:1)=iotk_eos
    call iotk_strcpy(attr_tmp,attr,ierr=ierrl)
    if(ierrl/=0) then
      call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 193 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
      goto 1
    end if
    call iotk_delete_attr(attr_tmp,"type",ierrl)
    if(ierrl/=0) then
      call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 198 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
      goto 1
    end if
    call iotk_delete_attr(attr_tmp,"kind",ierrl)
    if(ierrl/=0) then
      call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 203 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
      goto 1
    end if
    call iotk_delete_attr(attr_tmp,"size",ierrl)
    if(ierrl/=0) then
      call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 208 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
      goto 1
    end if
    call iotk_delete_attr(attr_tmp,"fmt",ierrl)
    if(ierrl/=0) then
      call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 213 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
      goto 1
    end if
    call iotk_delete_attr(attr_tmp,"columns",ierrl)
    if(ierrl/=0) then
      call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 218 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
      goto 1
    end if
    call iotk_delete_attr(attr_tmp,"len",ierrl)
    if(ierrl/=0) then
      call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 223 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
      goto 1
    end if
    if(iotk_strlen_trim(attr_tmp)>0) call iotk_strcat(lattr,iotk_strtrim(attr_tmp),ierr=ierrl)
  end if
  if(ierrl/=0) then
    call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 229 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
    goto 1
  end if
  call iotk_write_begin(unit,name,lattr,ierr=ierrl)
  if(ierrl/=0) then
    call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 234 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
    goto 1
  end if

  allocate(dattmp(size(dat)))
# 242 "iotk_dat.spp"
#if defined(__IOTK_WORKAROUND3) || defined(__IOTK_WORKAROUND4)
# 246 "iotk_dat.spp"
     call iotk_private_pack_INTEGER1(dattmp,dat,size(dattmp),1)
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
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
        goto 1
      end if
    else
      write(lunit,iostat=iostat) idummy,(dattmp(itmp),itmp=1,size(dattmp))
      if(iostat/=0) then
        call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 263 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
        goto 1
      end if
    end if
  else
    if(raw) then
# 272 "iotk_dat.spp"
      write(lunit,*,iostat=iostat) (dattmp(itmp),itmp=1,size(dattmp))
# 274 "iotk_dat.spp"
      if(iostat/=0) then
        call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 275 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
        goto 1
      end if
    else if(iotk_strcomp(usefmt,"*")) then
      write(lunit,*,iostat=iostat) (dattmp(itmp),itmp=1,size(dattmp))
      if(iostat/=0) then
        call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 281 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
        goto 1
      end if
    else if(iotk_strcomp(usefmt,"!")) then
# 295 "iotk_dat.spp"
     write(lunit,fmt=trim(iotk_wfmt("INTEGER",kind(dattmp),lcolumns,-1,lsep)),iostat=iostat) (dattmp(itmp),itmp=1,size(dattmp))
     if(iostat/=0) then
      call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 297 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
      goto 1
     end if
# 301 "iotk_dat.spp"
    else
      write(lunit,fmt=usefmt(1:iotk_strlen(usefmt)),iostat=iostat) (dattmp(itmp),itmp=1,size(dattmp))
      if(iostat/=0) then
        call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 304 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
        goto 1
      end if
    end if
  end if
  call iotk_write_end(unit,name,ierr=ierrl)
  if(ierrl/=0) then
    call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 311 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
    goto 1
  end if
1 continue
  if(allocated(dattmp)) deallocate(dattmp)
  if(present(ierr)) then
    ierr = ierrl
  else
    if(ierrl/=0) call iotk_error_handler(ierrl)
  end if
end subroutine iotk_write_dat_INTEGER1_1


# 326 "iotk_dat.spp"
recursive subroutine iotk_scan_dat_aux_INTEGER1(unit,dat,rkind,rlen,fmt,ierr)
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
  INTEGER (kind=__IOTK_INTEGER1)                        :: dat (:)
#else
  INTEGER (kind=__IOTK_INTEGER1),           intent(out) :: dat (:)
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
# 356 "iotk_dat.spp"
#ifdef __IOTK_INTEGER2
  INTEGER (__IOTK_INTEGER2), allocatable :: dat2 (:)
#endif
# 356 "iotk_dat.spp"
#ifdef __IOTK_INTEGER3
  INTEGER (__IOTK_INTEGER3), allocatable :: dat3 (:)
#endif
# 356 "iotk_dat.spp"
#ifdef __IOTK_INTEGER4
  INTEGER (__IOTK_INTEGER4), allocatable :: dat4 (:)
#endif
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
call iotk_error_msg(ierr,"CVS Revision: 1.17 ")
    return
  end if
# 463 "iotk_dat.spp"
  if(binary) then
    select case(rkind)
    case(kind(dat))
      if(raw) then
        read(lunit,iostat=iostat) ( dat(i), i=1,ubound(dat,1) )
        if(iostat/=0) then
          call iotk_error_issue(ierr,"iotk_scan_dat_aux",__FILE__,__LINE__)
# 469 "iotk_dat.spp"
call iotk_error_msg(ierr,"CVS Revision: 1.17 ")
# 469 "iotk_dat.spp"
call iotk_error_msg(ierr,' ')
# 469 "iotk_dat.spp"
call iotk_error_write(ierr,"iostat",iostat)
          return
        end if
      else
        read(lunit,iostat=iostat) idummy, ( dat(i), i=1,ubound(dat,1) )
        if(iostat/=0) then
          call iotk_error_issue(ierr,"iotk_scan_dat_aux",__FILE__,__LINE__)
# 475 "iotk_dat.spp"
call iotk_error_msg(ierr,"CVS Revision: 1.17 ")
# 475 "iotk_dat.spp"
call iotk_error_msg(ierr,' ')
# 475 "iotk_dat.spp"
call iotk_error_write(ierr,"iostat",iostat)
          return
        end if
      end if
# 481 "iotk_dat.spp"
#ifdef __IOTK_INTEGER2
    case(kind(dat2))
      ! Giusto per scrupolo. Se e' raw non ci sono info sul kind, quindi questa linea e' irraggiungibile
      if(raw) then
        call iotk_error_issue(ierr,"iotk_scan_dat_aux",__FILE__,__LINE__)
# 485 "iotk_dat.spp"
call iotk_error_msg(ierr,"CVS Revision: 1.17 ")
        return
      end if
      allocate(dat2(ubound(dat,1)))
      read(lunit,iostat=iostat) idummy,( dat2(i), i=1,ubound(dat2,1) )
      if(iostat/=0) then
        call iotk_error_issue(ierr,"iotk_scan_dat_aux",__FILE__,__LINE__)
# 491 "iotk_dat.spp"
call iotk_error_msg(ierr,"CVS Revision: 1.17 ")
# 491 "iotk_dat.spp"
call iotk_error_msg(ierr,' ')
# 491 "iotk_dat.spp"
call iotk_error_write(ierr,"iostat",iostat)
        return
      end if
# 501 "iotk_dat.spp"
      dat = dat2
# 503 "iotk_dat.spp"
      deallocate(dat2)
#endif
# 481 "iotk_dat.spp"
#ifdef __IOTK_INTEGER3
    case(kind(dat3))
      ! Giusto per scrupolo. Se e' raw non ci sono info sul kind, quindi questa linea e' irraggiungibile
      if(raw) then
        call iotk_error_issue(ierr,"iotk_scan_dat_aux",__FILE__,__LINE__)
# 485 "iotk_dat.spp"
call iotk_error_msg(ierr,"CVS Revision: 1.17 ")
        return
      end if
      allocate(dat3(ubound(dat,1)))
      read(lunit,iostat=iostat) idummy,( dat3(i), i=1,ubound(dat3,1) )
      if(iostat/=0) then
        call iotk_error_issue(ierr,"iotk_scan_dat_aux",__FILE__,__LINE__)
# 491 "iotk_dat.spp"
call iotk_error_msg(ierr,"CVS Revision: 1.17 ")
# 491 "iotk_dat.spp"
call iotk_error_msg(ierr,' ')
# 491 "iotk_dat.spp"
call iotk_error_write(ierr,"iostat",iostat)
        return
      end if
# 501 "iotk_dat.spp"
      dat = dat3
# 503 "iotk_dat.spp"
      deallocate(dat3)
#endif
# 481 "iotk_dat.spp"
#ifdef __IOTK_INTEGER4
    case(kind(dat4))
      ! Giusto per scrupolo. Se e' raw non ci sono info sul kind, quindi questa linea e' irraggiungibile
      if(raw) then
        call iotk_error_issue(ierr,"iotk_scan_dat_aux",__FILE__,__LINE__)
# 485 "iotk_dat.spp"
call iotk_error_msg(ierr,"CVS Revision: 1.17 ")
        return
      end if
      allocate(dat4(ubound(dat,1)))
      read(lunit,iostat=iostat) idummy,( dat4(i), i=1,ubound(dat4,1) )
      if(iostat/=0) then
        call iotk_error_issue(ierr,"iotk_scan_dat_aux",__FILE__,__LINE__)
# 491 "iotk_dat.spp"
call iotk_error_msg(ierr,"CVS Revision: 1.17 ")
# 491 "iotk_dat.spp"
call iotk_error_msg(ierr,' ')
# 491 "iotk_dat.spp"
call iotk_error_write(ierr,"iostat",iostat)
        return
      end if
# 501 "iotk_dat.spp"
      dat = dat4
# 503 "iotk_dat.spp"
      deallocate(dat4)
#endif
# 507 "iotk_dat.spp"
    case default
      call iotk_error_issue(ierr,"iotk_scan_dat_aux",__FILE__,__LINE__)
# 508 "iotk_dat.spp"
call iotk_error_msg(ierr,"CVS Revision: 1.17 ")
# 508 "iotk_dat.spp"
call iotk_error_msg(ierr,'Kind incompatibility')
# 508 "iotk_dat.spp"
call iotk_error_write(ierr,"kind",rkind)
    end select
  else
    if(raw) then
      read(lunit,fmt=*,iostat=iostat) ( dat(i), i=1,ubound(dat,1) )
      if(iostat/=0) then
        call iotk_error_issue(ierr,"iotk_scan_dat_aux",__FILE__,__LINE__)
# 514 "iotk_dat.spp"
call iotk_error_msg(ierr,"CVS Revision: 1.17 ")
# 514 "iotk_dat.spp"
call iotk_error_msg(ierr,' ')
# 514 "iotk_dat.spp"
call iotk_error_write(ierr,"iostat",iostat)
        return
      end if
    else if(iotk_strcomp(fmt,"*")) then
      read(lunit,fmt=*,iostat=iostat) ( dat(i), i=1,ubound(dat,1) )
      if(iostat/=0) then
        call iotk_error_issue(ierr,"iotk_scan_dat_aux",__FILE__,__LINE__)
# 520 "iotk_dat.spp"
call iotk_error_msg(ierr,"CVS Revision: 1.17 ")
# 520 "iotk_dat.spp"
call iotk_error_msg(ierr,' ')
# 520 "iotk_dat.spp"
call iotk_error_write(ierr,"iostat",iostat)
        return
      end if
    else if(iotk_strcomp(fmt,"!")) then
      index = 0
      do
        call iotk_getline(lunit,line,length,ierr)
        if(ierr/=0) then
          call iotk_error_issue(ierr,"iotk_scan_dat_aux",__FILE__,__LINE__)
# 528 "iotk_dat.spp"
call iotk_error_msg(ierr,"CVS Revision: 1.17 ")
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
# 539 "iotk_dat.spp"
call iotk_error_msg(ierr,"CVS Revision: 1.17 ")
# 539 "iotk_dat.spp"
call iotk_error_msg(ierr,' ')
# 539 "iotk_dat.spp"
call iotk_error_write(ierr,"iostat",iostat)
            return
          end if
          call iotk_getline(lunit,altline,altlength,ierr)
          if(ierr/=0) then
            call iotk_error_issue(ierr,"iotk_scan_dat_aux",__FILE__,__LINE__)
# 544 "iotk_dat.spp"
call iotk_error_msg(ierr,"CVS Revision: 1.17 ")
            return
          end if
          backspace(lunit,iostat=iostat)
          if(iostat/=0) then
            call iotk_error_issue(ierr,"iotk_scan_dat_aux",__FILE__,__LINE__)
# 549 "iotk_dat.spp"
call iotk_error_msg(ierr,"CVS Revision: 1.17 ")
# 549 "iotk_dat.spp"
call iotk_error_msg(ierr,' ')
# 549 "iotk_dat.spp"
call iotk_error_write(ierr,"iostat",iostat)
            return
          end if
          read(lunit,"(a)",advance="no",iostat=iostat) altline(1:nexttag-1 + altlength - length)
          if(iostat/=0) then
            call iotk_error_issue(ierr,"iotk_scan_dat_aux",__FILE__,__LINE__)
# 554 "iotk_dat.spp"
call iotk_error_msg(ierr,"CVS Revision: 1.17 ")
# 554 "iotk_dat.spp"
call iotk_error_msg(ierr,' ')
# 554 "iotk_dat.spp"
call iotk_error_write(ierr,"iostat",iostat)
            return
          end if
        end if
        call iotk_str_clean(line(1:nexttag - 1))
        call iotk_read(dat,line(1:nexttag - 1),index,ierr)
        if(ierr/=0) then
          call iotk_error_issue(ierr,"iotk_scan_dat_aux",__FILE__,__LINE__)
# 561 "iotk_dat.spp"
call iotk_error_msg(ierr,"CVS Revision: 1.17 ")
          return
        end if
# 567 "iotk_dat.spp"
        if(index == size(dat)) exit
# 569 "iotk_dat.spp"
        if(nexttag/=length + 1) then
          call iotk_error_issue(ierr,"iotk_scan_dat_aux",__FILE__,__LINE__)
# 570 "iotk_dat.spp"
call iotk_error_msg(ierr,"CVS Revision: 1.17 ")
          return
        end if
      end do
    else
      read(lunit,fmt=fmt(1:iotk_strlen(fmt)),iostat=iostat) ( dat(i), i=1,ubound(dat,1) )
      if(iostat/=0) then
        call iotk_error_issue(ierr,"iotk_scan_dat_aux",__FILE__,__LINE__)
# 577 "iotk_dat.spp"
call iotk_error_msg(ierr,"CVS Revision: 1.17 ")
# 577 "iotk_dat.spp"
call iotk_error_msg(ierr,' ')
# 577 "iotk_dat.spp"
call iotk_error_write(ierr,"iostat",iostat)
        return
      end if
    end if
  end if
# 583 "iotk_dat.spp"
  if(idummy/=0) then
    call iotk_error_issue(ierr,"iotk_scan_dat_aux",__FILE__,__LINE__)
# 584 "iotk_dat.spp"
call iotk_error_msg(ierr,"CVS Revision: 1.17 ")
    return
  end if
end subroutine iotk_scan_dat_aux_INTEGER1
# 589 "iotk_dat.spp"

# 591 "iotk_dat.spp"
subroutine iotk_scan_dat_INTEGER1_1(unit,name,dat,dummy,attr,found,default,ierr)
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
  INTEGER (kind=__IOTK_INTEGER1)                        :: dat (:)
#else
  INTEGER (kind=__IOTK_INTEGER1),           intent(out) :: dat (:)
#endif
  type(iotk_dummytype), optional         :: dummy
#ifdef __IOTK_WORKAROUND6
  character(len=*),optional              :: attr
#else
  character(len=*),optional, intent(out) :: attr
#endif
  logical,         optional, intent(out) :: found
  INTEGER (kind=__IOTK_INTEGER1), optional, intent(in)  :: default (:)
  integer,         optional, intent(out) :: ierr
# 618 "iotk_dat.spp"
  INTEGER (kind=__IOTK_INTEGER1),              allocatable :: tmpdat(:)
# 620 "iotk_dat.spp"
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
# 637 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
    goto 1
  end if
  call iotk_parse_dat(lattr,rtype,rkind,rsize,rlen,fmt,columns,ierrl)
! Note that columns is not effectively used
  if(ierrl/=0) goto 1
  if(.not. (iotk_strcomp(rtype,iotk_eos) .or. iotk_strcomp(rtype,"INTEGER") ) ) then
    call iotk_error_issue(ierrl,"iotk_scan_dat",__FILE__,__LINE__)
# 644 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
# 644 "iotk_dat.spp"
call iotk_error_msg(ierrl,' ')
# 644 "iotk_dat.spp"
call iotk_error_write(ierrl,"rtype",rtype(1:iotk_strlen(rtype)))
# 644 "iotk_dat.spp"
call iotk_error_write(ierrl,"type","INTEGER")
    goto 1
  end if
  if(.not. (rsize==-1 .or. rsize==size(dat)) ) then
    call iotk_error_issue(ierrl,"iotk_scan_dat",__FILE__,__LINE__)
# 648 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
    goto 1
  end if
  if(rkind==-1) rkind = kind(dat)
# 655 "iotk_dat.spp"

  allocate(tmpdat(size(dat)))
  call iotk_scan_dat_aux(unit,tmpdat,rkind,rlen,fmt(1:iotk_strlen(fmt)),ierrl)
# 662 "iotk_dat.spp"
#ifdef __IOTK_WORKAROUND7
        call iotk_reshape_INTEGER1( size(dat),dat,tmpdat )
#else
        dat = reshape(tmpdat,shape(dat))
#endif
# 671 "iotk_dat.spp"
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
# 683 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
# 683 "iotk_dat.spp"
call iotk_error_msg(ierrl,'Dat not found')
# 683 "iotk_dat.spp"
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
end subroutine iotk_scan_dat_INTEGER1_1


#endif
#endif

subroutine iotk_dat_dummy_INTEGER1_1
  write(0,*)
end subroutine iotk_dat_dummy_INTEGER1_1


# 706 "iotk_dat.spp"
!
! the following workaround is introduced for INTEL compiler < 9.0 
! having problem in treating INTEGER arrays with large dimensions
! within implicit loops
!
#ifdef __IOTK_WORKAROUND7

subroutine iotk_reshape_INTEGER1(n,dat_out,dat_in)
  implicit none
  integer, intent(in) :: n
  INTEGER (kind=__IOTK_INTEGER1), intent(in)  :: dat_in(n)
  INTEGER (kind=__IOTK_INTEGER1), intent(out) :: dat_out(n)
  integer :: i
  do i=1,n
     dat_out(i) = dat_in(i)
  enddo
end subroutine iotk_reshape_INTEGER1

#endif

# 727 "iotk_dat.spp"


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

#ifdef __IOTK_INTEGER1
#if 2 <= __IOTK_MAXRANK
# 82 "iotk_dat.spp"
subroutine iotk_write_dat_INTEGER1_2(unit,name,dat,dummy,attr,columns,sep,fmt,ierr)
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
  INTEGER (kind=__IOTK_INTEGER1), intent(in)  :: dat (:,:) 
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
# 114 "iotk_dat.spp"
  INTEGER (kind=__IOTK_INTEGER1),allocatable :: dattmp(:)
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
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
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
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
    goto 1
  end if
  call iotk_strcpy(usefmt,"!",ierrl)
  if(ierrl/=0) then
    call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 142 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
    goto 1
  end if
  if(present(fmt) .and. .not. raw) call iotk_strcpy(usefmt,iotk_strtrim(fmt),ierrl)
  if(ierrl/=0) then
    call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 147 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
    goto 1
  end if
  if(iotk_strscan(usefmt,"<>&")/=0) then
    call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 151 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
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
  call iotk_write_attr(lattr,"type",iotk_tolower("INTEGER"),first=.true.,ierr=ierrl)
  if(ierrl/=0) then
    call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 156 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
    goto 1
  end if
  call iotk_write_attr(lattr,"size",size(dat),ierr=ierrl)
  if(ierrl/=0) then
    call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 161 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
    goto 1
  end if
# 171 "iotk_dat.spp"
  if(binary) then
    call iotk_write_attr(lattr,"kind",kind(dat),ierr=ierrl)
    if(ierrl/=0) then
      call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 174 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
      goto 1
    end if
  end if
# 179 "iotk_dat.spp"
  if(.not.iotk_strcomp(usefmt,"!")) call iotk_write_attr(lattr,"fmt",iotk_strtrim(usefmt),ierr=ierrl)
  if(ierrl/=0) then
    call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 181 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
    goto 1
  end if
  if(lcolumns/=1) call iotk_write_attr(lattr,"columns",lcolumns,ierr=ierrl)
  if(ierrl/=0) then
    call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 186 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
    goto 1
  end if
  if(present(attr)) then
    attr_tmp(1:1)=iotk_eos
    call iotk_strcpy(attr_tmp,attr,ierr=ierrl)
    if(ierrl/=0) then
      call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 193 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
      goto 1
    end if
    call iotk_delete_attr(attr_tmp,"type",ierrl)
    if(ierrl/=0) then
      call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 198 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
      goto 1
    end if
    call iotk_delete_attr(attr_tmp,"kind",ierrl)
    if(ierrl/=0) then
      call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 203 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
      goto 1
    end if
    call iotk_delete_attr(attr_tmp,"size",ierrl)
    if(ierrl/=0) then
      call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 208 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
      goto 1
    end if
    call iotk_delete_attr(attr_tmp,"fmt",ierrl)
    if(ierrl/=0) then
      call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 213 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
      goto 1
    end if
    call iotk_delete_attr(attr_tmp,"columns",ierrl)
    if(ierrl/=0) then
      call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 218 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
      goto 1
    end if
    call iotk_delete_attr(attr_tmp,"len",ierrl)
    if(ierrl/=0) then
      call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 223 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
      goto 1
    end if
    if(iotk_strlen_trim(attr_tmp)>0) call iotk_strcat(lattr,iotk_strtrim(attr_tmp),ierr=ierrl)
  end if
  if(ierrl/=0) then
    call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 229 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
    goto 1
  end if
  call iotk_write_begin(unit,name,lattr,ierr=ierrl)
  if(ierrl/=0) then
    call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 234 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
    goto 1
  end if

  allocate(dattmp(size(dat)))
# 242 "iotk_dat.spp"
#if defined(__IOTK_WORKAROUND3) || defined(__IOTK_WORKAROUND4)
# 246 "iotk_dat.spp"
     call iotk_private_pack_INTEGER1(dattmp,dat,size(dattmp),1)
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
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
        goto 1
      end if
    else
      write(lunit,iostat=iostat) idummy,(dattmp(itmp),itmp=1,size(dattmp))
      if(iostat/=0) then
        call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 263 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
        goto 1
      end if
    end if
  else
    if(raw) then
# 272 "iotk_dat.spp"
      write(lunit,*,iostat=iostat) (dattmp(itmp),itmp=1,size(dattmp))
# 274 "iotk_dat.spp"
      if(iostat/=0) then
        call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 275 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
        goto 1
      end if
    else if(iotk_strcomp(usefmt,"*")) then
      write(lunit,*,iostat=iostat) (dattmp(itmp),itmp=1,size(dattmp))
      if(iostat/=0) then
        call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 281 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
        goto 1
      end if
    else if(iotk_strcomp(usefmt,"!")) then
# 295 "iotk_dat.spp"
     write(lunit,fmt=trim(iotk_wfmt("INTEGER",kind(dattmp),lcolumns,-1,lsep)),iostat=iostat) (dattmp(itmp),itmp=1,size(dattmp))
     if(iostat/=0) then
      call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 297 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
      goto 1
     end if
# 301 "iotk_dat.spp"
    else
      write(lunit,fmt=usefmt(1:iotk_strlen(usefmt)),iostat=iostat) (dattmp(itmp),itmp=1,size(dattmp))
      if(iostat/=0) then
        call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 304 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
        goto 1
      end if
    end if
  end if
  call iotk_write_end(unit,name,ierr=ierrl)
  if(ierrl/=0) then
    call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 311 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
    goto 1
  end if
1 continue
  if(allocated(dattmp)) deallocate(dattmp)
  if(present(ierr)) then
    ierr = ierrl
  else
    if(ierrl/=0) call iotk_error_handler(ierrl)
  end if
end subroutine iotk_write_dat_INTEGER1_2


# 589 "iotk_dat.spp"

# 591 "iotk_dat.spp"
subroutine iotk_scan_dat_INTEGER1_2(unit,name,dat,dummy,attr,found,default,ierr)
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
  INTEGER (kind=__IOTK_INTEGER1)                        :: dat (:,:)
#else
  INTEGER (kind=__IOTK_INTEGER1),           intent(out) :: dat (:,:)
#endif
  type(iotk_dummytype), optional         :: dummy
#ifdef __IOTK_WORKAROUND6
  character(len=*),optional              :: attr
#else
  character(len=*),optional, intent(out) :: attr
#endif
  logical,         optional, intent(out) :: found
  INTEGER (kind=__IOTK_INTEGER1), optional, intent(in)  :: default (:,:)
  integer,         optional, intent(out) :: ierr
# 618 "iotk_dat.spp"
  INTEGER (kind=__IOTK_INTEGER1),              allocatable :: tmpdat(:)
# 620 "iotk_dat.spp"
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
# 637 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
    goto 1
  end if
  call iotk_parse_dat(lattr,rtype,rkind,rsize,rlen,fmt,columns,ierrl)
! Note that columns is not effectively used
  if(ierrl/=0) goto 1
  if(.not. (iotk_strcomp(rtype,iotk_eos) .or. iotk_strcomp(rtype,"INTEGER") ) ) then
    call iotk_error_issue(ierrl,"iotk_scan_dat",__FILE__,__LINE__)
# 644 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
# 644 "iotk_dat.spp"
call iotk_error_msg(ierrl,' ')
# 644 "iotk_dat.spp"
call iotk_error_write(ierrl,"rtype",rtype(1:iotk_strlen(rtype)))
# 644 "iotk_dat.spp"
call iotk_error_write(ierrl,"type","INTEGER")
    goto 1
  end if
  if(.not. (rsize==-1 .or. rsize==size(dat)) ) then
    call iotk_error_issue(ierrl,"iotk_scan_dat",__FILE__,__LINE__)
# 648 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
    goto 1
  end if
  if(rkind==-1) rkind = kind(dat)
# 655 "iotk_dat.spp"

  allocate(tmpdat(size(dat)))
  call iotk_scan_dat_aux(unit,tmpdat,rkind,rlen,fmt(1:iotk_strlen(fmt)),ierrl)
# 662 "iotk_dat.spp"
#ifdef __IOTK_WORKAROUND7
        call iotk_reshape_INTEGER1( size(dat),dat,tmpdat )
#else
        dat = reshape(tmpdat,shape(dat))
#endif
# 671 "iotk_dat.spp"
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
# 683 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
# 683 "iotk_dat.spp"
call iotk_error_msg(ierrl,'Dat not found')
# 683 "iotk_dat.spp"
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
end subroutine iotk_scan_dat_INTEGER1_2


#endif
#endif

subroutine iotk_dat_dummy_INTEGER1_2
  write(0,*)
end subroutine iotk_dat_dummy_INTEGER1_2


# 727 "iotk_dat.spp"


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

#ifdef __IOTK_INTEGER1
#if 3 <= __IOTK_MAXRANK
# 82 "iotk_dat.spp"
subroutine iotk_write_dat_INTEGER1_3(unit,name,dat,dummy,attr,columns,sep,fmt,ierr)
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
  INTEGER (kind=__IOTK_INTEGER1), intent(in)  :: dat (:,:,:) 
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
# 114 "iotk_dat.spp"
  INTEGER (kind=__IOTK_INTEGER1),allocatable :: dattmp(:)
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
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
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
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
    goto 1
  end if
  call iotk_strcpy(usefmt,"!",ierrl)
  if(ierrl/=0) then
    call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 142 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
    goto 1
  end if
  if(present(fmt) .and. .not. raw) call iotk_strcpy(usefmt,iotk_strtrim(fmt),ierrl)
  if(ierrl/=0) then
    call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 147 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
    goto 1
  end if
  if(iotk_strscan(usefmt,"<>&")/=0) then
    call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 151 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
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
  call iotk_write_attr(lattr,"type",iotk_tolower("INTEGER"),first=.true.,ierr=ierrl)
  if(ierrl/=0) then
    call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 156 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
    goto 1
  end if
  call iotk_write_attr(lattr,"size",size(dat),ierr=ierrl)
  if(ierrl/=0) then
    call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 161 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
    goto 1
  end if
# 171 "iotk_dat.spp"
  if(binary) then
    call iotk_write_attr(lattr,"kind",kind(dat),ierr=ierrl)
    if(ierrl/=0) then
      call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 174 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
      goto 1
    end if
  end if
# 179 "iotk_dat.spp"
  if(.not.iotk_strcomp(usefmt,"!")) call iotk_write_attr(lattr,"fmt",iotk_strtrim(usefmt),ierr=ierrl)
  if(ierrl/=0) then
    call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 181 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
    goto 1
  end if
  if(lcolumns/=1) call iotk_write_attr(lattr,"columns",lcolumns,ierr=ierrl)
  if(ierrl/=0) then
    call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 186 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
    goto 1
  end if
  if(present(attr)) then
    attr_tmp(1:1)=iotk_eos
    call iotk_strcpy(attr_tmp,attr,ierr=ierrl)
    if(ierrl/=0) then
      call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 193 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
      goto 1
    end if
    call iotk_delete_attr(attr_tmp,"type",ierrl)
    if(ierrl/=0) then
      call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 198 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
      goto 1
    end if
    call iotk_delete_attr(attr_tmp,"kind",ierrl)
    if(ierrl/=0) then
      call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 203 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
      goto 1
    end if
    call iotk_delete_attr(attr_tmp,"size",ierrl)
    if(ierrl/=0) then
      call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 208 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
      goto 1
    end if
    call iotk_delete_attr(attr_tmp,"fmt",ierrl)
    if(ierrl/=0) then
      call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 213 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
      goto 1
    end if
    call iotk_delete_attr(attr_tmp,"columns",ierrl)
    if(ierrl/=0) then
      call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 218 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
      goto 1
    end if
    call iotk_delete_attr(attr_tmp,"len",ierrl)
    if(ierrl/=0) then
      call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 223 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
      goto 1
    end if
    if(iotk_strlen_trim(attr_tmp)>0) call iotk_strcat(lattr,iotk_strtrim(attr_tmp),ierr=ierrl)
  end if
  if(ierrl/=0) then
    call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 229 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
    goto 1
  end if
  call iotk_write_begin(unit,name,lattr,ierr=ierrl)
  if(ierrl/=0) then
    call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 234 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
    goto 1
  end if

  allocate(dattmp(size(dat)))
# 242 "iotk_dat.spp"
#if defined(__IOTK_WORKAROUND3) || defined(__IOTK_WORKAROUND4)
# 246 "iotk_dat.spp"
     call iotk_private_pack_INTEGER1(dattmp,dat,size(dattmp),1)
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
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
        goto 1
      end if
    else
      write(lunit,iostat=iostat) idummy,(dattmp(itmp),itmp=1,size(dattmp))
      if(iostat/=0) then
        call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 263 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
        goto 1
      end if
    end if
  else
    if(raw) then
# 272 "iotk_dat.spp"
      write(lunit,*,iostat=iostat) (dattmp(itmp),itmp=1,size(dattmp))
# 274 "iotk_dat.spp"
      if(iostat/=0) then
        call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 275 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
        goto 1
      end if
    else if(iotk_strcomp(usefmt,"*")) then
      write(lunit,*,iostat=iostat) (dattmp(itmp),itmp=1,size(dattmp))
      if(iostat/=0) then
        call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 281 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
        goto 1
      end if
    else if(iotk_strcomp(usefmt,"!")) then
# 295 "iotk_dat.spp"
     write(lunit,fmt=trim(iotk_wfmt("INTEGER",kind(dattmp),lcolumns,-1,lsep)),iostat=iostat) (dattmp(itmp),itmp=1,size(dattmp))
     if(iostat/=0) then
      call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 297 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
      goto 1
     end if
# 301 "iotk_dat.spp"
    else
      write(lunit,fmt=usefmt(1:iotk_strlen(usefmt)),iostat=iostat) (dattmp(itmp),itmp=1,size(dattmp))
      if(iostat/=0) then
        call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 304 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
        goto 1
      end if
    end if
  end if
  call iotk_write_end(unit,name,ierr=ierrl)
  if(ierrl/=0) then
    call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 311 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
    goto 1
  end if
1 continue
  if(allocated(dattmp)) deallocate(dattmp)
  if(present(ierr)) then
    ierr = ierrl
  else
    if(ierrl/=0) call iotk_error_handler(ierrl)
  end if
end subroutine iotk_write_dat_INTEGER1_3


# 589 "iotk_dat.spp"

# 591 "iotk_dat.spp"
subroutine iotk_scan_dat_INTEGER1_3(unit,name,dat,dummy,attr,found,default,ierr)
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
  INTEGER (kind=__IOTK_INTEGER1)                        :: dat (:,:,:)
#else
  INTEGER (kind=__IOTK_INTEGER1),           intent(out) :: dat (:,:,:)
#endif
  type(iotk_dummytype), optional         :: dummy
#ifdef __IOTK_WORKAROUND6
  character(len=*),optional              :: attr
#else
  character(len=*),optional, intent(out) :: attr
#endif
  logical,         optional, intent(out) :: found
  INTEGER (kind=__IOTK_INTEGER1), optional, intent(in)  :: default (:,:,:)
  integer,         optional, intent(out) :: ierr
# 618 "iotk_dat.spp"
  INTEGER (kind=__IOTK_INTEGER1),              allocatable :: tmpdat(:)
# 620 "iotk_dat.spp"
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
# 637 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
    goto 1
  end if
  call iotk_parse_dat(lattr,rtype,rkind,rsize,rlen,fmt,columns,ierrl)
! Note that columns is not effectively used
  if(ierrl/=0) goto 1
  if(.not. (iotk_strcomp(rtype,iotk_eos) .or. iotk_strcomp(rtype,"INTEGER") ) ) then
    call iotk_error_issue(ierrl,"iotk_scan_dat",__FILE__,__LINE__)
# 644 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
# 644 "iotk_dat.spp"
call iotk_error_msg(ierrl,' ')
# 644 "iotk_dat.spp"
call iotk_error_write(ierrl,"rtype",rtype(1:iotk_strlen(rtype)))
# 644 "iotk_dat.spp"
call iotk_error_write(ierrl,"type","INTEGER")
    goto 1
  end if
  if(.not. (rsize==-1 .or. rsize==size(dat)) ) then
    call iotk_error_issue(ierrl,"iotk_scan_dat",__FILE__,__LINE__)
# 648 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
    goto 1
  end if
  if(rkind==-1) rkind = kind(dat)
# 655 "iotk_dat.spp"

  allocate(tmpdat(size(dat)))
  call iotk_scan_dat_aux(unit,tmpdat,rkind,rlen,fmt(1:iotk_strlen(fmt)),ierrl)
# 662 "iotk_dat.spp"
#ifdef __IOTK_WORKAROUND7
        call iotk_reshape_INTEGER1( size(dat),dat,tmpdat )
#else
        dat = reshape(tmpdat,shape(dat))
#endif
# 671 "iotk_dat.spp"
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
# 683 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
# 683 "iotk_dat.spp"
call iotk_error_msg(ierrl,'Dat not found')
# 683 "iotk_dat.spp"
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
end subroutine iotk_scan_dat_INTEGER1_3


#endif
#endif

subroutine iotk_dat_dummy_INTEGER1_3
  write(0,*)
end subroutine iotk_dat_dummy_INTEGER1_3


# 727 "iotk_dat.spp"


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

#ifdef __IOTK_INTEGER1
#if 4 <= __IOTK_MAXRANK
# 82 "iotk_dat.spp"
subroutine iotk_write_dat_INTEGER1_4(unit,name,dat,dummy,attr,columns,sep,fmt,ierr)
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
  INTEGER (kind=__IOTK_INTEGER1), intent(in)  :: dat (:,:,:,:) 
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
# 114 "iotk_dat.spp"
  INTEGER (kind=__IOTK_INTEGER1),allocatable :: dattmp(:)
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
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
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
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
    goto 1
  end if
  call iotk_strcpy(usefmt,"!",ierrl)
  if(ierrl/=0) then
    call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 142 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
    goto 1
  end if
  if(present(fmt) .and. .not. raw) call iotk_strcpy(usefmt,iotk_strtrim(fmt),ierrl)
  if(ierrl/=0) then
    call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 147 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
    goto 1
  end if
  if(iotk_strscan(usefmt,"<>&")/=0) then
    call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 151 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
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
  call iotk_write_attr(lattr,"type",iotk_tolower("INTEGER"),first=.true.,ierr=ierrl)
  if(ierrl/=0) then
    call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 156 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
    goto 1
  end if
  call iotk_write_attr(lattr,"size",size(dat),ierr=ierrl)
  if(ierrl/=0) then
    call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 161 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
    goto 1
  end if
# 171 "iotk_dat.spp"
  if(binary) then
    call iotk_write_attr(lattr,"kind",kind(dat),ierr=ierrl)
    if(ierrl/=0) then
      call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 174 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
      goto 1
    end if
  end if
# 179 "iotk_dat.spp"
  if(.not.iotk_strcomp(usefmt,"!")) call iotk_write_attr(lattr,"fmt",iotk_strtrim(usefmt),ierr=ierrl)
  if(ierrl/=0) then
    call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 181 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
    goto 1
  end if
  if(lcolumns/=1) call iotk_write_attr(lattr,"columns",lcolumns,ierr=ierrl)
  if(ierrl/=0) then
    call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 186 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
    goto 1
  end if
  if(present(attr)) then
    attr_tmp(1:1)=iotk_eos
    call iotk_strcpy(attr_tmp,attr,ierr=ierrl)
    if(ierrl/=0) then
      call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 193 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
      goto 1
    end if
    call iotk_delete_attr(attr_tmp,"type",ierrl)
    if(ierrl/=0) then
      call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 198 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
      goto 1
    end if
    call iotk_delete_attr(attr_tmp,"kind",ierrl)
    if(ierrl/=0) then
      call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 203 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
      goto 1
    end if
    call iotk_delete_attr(attr_tmp,"size",ierrl)
    if(ierrl/=0) then
      call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 208 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
      goto 1
    end if
    call iotk_delete_attr(attr_tmp,"fmt",ierrl)
    if(ierrl/=0) then
      call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 213 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
      goto 1
    end if
    call iotk_delete_attr(attr_tmp,"columns",ierrl)
    if(ierrl/=0) then
      call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 218 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
      goto 1
    end if
    call iotk_delete_attr(attr_tmp,"len",ierrl)
    if(ierrl/=0) then
      call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 223 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
      goto 1
    end if
    if(iotk_strlen_trim(attr_tmp)>0) call iotk_strcat(lattr,iotk_strtrim(attr_tmp),ierr=ierrl)
  end if
  if(ierrl/=0) then
    call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 229 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
    goto 1
  end if
  call iotk_write_begin(unit,name,lattr,ierr=ierrl)
  if(ierrl/=0) then
    call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 234 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
    goto 1
  end if

  allocate(dattmp(size(dat)))
# 242 "iotk_dat.spp"
#if defined(__IOTK_WORKAROUND3) || defined(__IOTK_WORKAROUND4)
# 246 "iotk_dat.spp"
     call iotk_private_pack_INTEGER1(dattmp,dat,size(dattmp),1)
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
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
        goto 1
      end if
    else
      write(lunit,iostat=iostat) idummy,(dattmp(itmp),itmp=1,size(dattmp))
      if(iostat/=0) then
        call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 263 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
        goto 1
      end if
    end if
  else
    if(raw) then
# 272 "iotk_dat.spp"
      write(lunit,*,iostat=iostat) (dattmp(itmp),itmp=1,size(dattmp))
# 274 "iotk_dat.spp"
      if(iostat/=0) then
        call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 275 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
        goto 1
      end if
    else if(iotk_strcomp(usefmt,"*")) then
      write(lunit,*,iostat=iostat) (dattmp(itmp),itmp=1,size(dattmp))
      if(iostat/=0) then
        call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 281 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
        goto 1
      end if
    else if(iotk_strcomp(usefmt,"!")) then
# 295 "iotk_dat.spp"
     write(lunit,fmt=trim(iotk_wfmt("INTEGER",kind(dattmp),lcolumns,-1,lsep)),iostat=iostat) (dattmp(itmp),itmp=1,size(dattmp))
     if(iostat/=0) then
      call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 297 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
      goto 1
     end if
# 301 "iotk_dat.spp"
    else
      write(lunit,fmt=usefmt(1:iotk_strlen(usefmt)),iostat=iostat) (dattmp(itmp),itmp=1,size(dattmp))
      if(iostat/=0) then
        call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 304 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
        goto 1
      end if
    end if
  end if
  call iotk_write_end(unit,name,ierr=ierrl)
  if(ierrl/=0) then
    call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 311 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
    goto 1
  end if
1 continue
  if(allocated(dattmp)) deallocate(dattmp)
  if(present(ierr)) then
    ierr = ierrl
  else
    if(ierrl/=0) call iotk_error_handler(ierrl)
  end if
end subroutine iotk_write_dat_INTEGER1_4


# 589 "iotk_dat.spp"

# 591 "iotk_dat.spp"
subroutine iotk_scan_dat_INTEGER1_4(unit,name,dat,dummy,attr,found,default,ierr)
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
  INTEGER (kind=__IOTK_INTEGER1)                        :: dat (:,:,:,:)
#else
  INTEGER (kind=__IOTK_INTEGER1),           intent(out) :: dat (:,:,:,:)
#endif
  type(iotk_dummytype), optional         :: dummy
#ifdef __IOTK_WORKAROUND6
  character(len=*),optional              :: attr
#else
  character(len=*),optional, intent(out) :: attr
#endif
  logical,         optional, intent(out) :: found
  INTEGER (kind=__IOTK_INTEGER1), optional, intent(in)  :: default (:,:,:,:)
  integer,         optional, intent(out) :: ierr
# 618 "iotk_dat.spp"
  INTEGER (kind=__IOTK_INTEGER1),              allocatable :: tmpdat(:)
# 620 "iotk_dat.spp"
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
# 637 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
    goto 1
  end if
  call iotk_parse_dat(lattr,rtype,rkind,rsize,rlen,fmt,columns,ierrl)
! Note that columns is not effectively used
  if(ierrl/=0) goto 1
  if(.not. (iotk_strcomp(rtype,iotk_eos) .or. iotk_strcomp(rtype,"INTEGER") ) ) then
    call iotk_error_issue(ierrl,"iotk_scan_dat",__FILE__,__LINE__)
# 644 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
# 644 "iotk_dat.spp"
call iotk_error_msg(ierrl,' ')
# 644 "iotk_dat.spp"
call iotk_error_write(ierrl,"rtype",rtype(1:iotk_strlen(rtype)))
# 644 "iotk_dat.spp"
call iotk_error_write(ierrl,"type","INTEGER")
    goto 1
  end if
  if(.not. (rsize==-1 .or. rsize==size(dat)) ) then
    call iotk_error_issue(ierrl,"iotk_scan_dat",__FILE__,__LINE__)
# 648 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
    goto 1
  end if
  if(rkind==-1) rkind = kind(dat)
# 655 "iotk_dat.spp"

  allocate(tmpdat(size(dat)))
  call iotk_scan_dat_aux(unit,tmpdat,rkind,rlen,fmt(1:iotk_strlen(fmt)),ierrl)
# 662 "iotk_dat.spp"
#ifdef __IOTK_WORKAROUND7
        call iotk_reshape_INTEGER1( size(dat),dat,tmpdat )
#else
        dat = reshape(tmpdat,shape(dat))
#endif
# 671 "iotk_dat.spp"
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
# 683 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
# 683 "iotk_dat.spp"
call iotk_error_msg(ierrl,'Dat not found')
# 683 "iotk_dat.spp"
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
end subroutine iotk_scan_dat_INTEGER1_4


#endif
#endif

subroutine iotk_dat_dummy_INTEGER1_4
  write(0,*)
end subroutine iotk_dat_dummy_INTEGER1_4


# 727 "iotk_dat.spp"


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

#ifdef __IOTK_INTEGER1
#if 5 <= __IOTK_MAXRANK
# 82 "iotk_dat.spp"
subroutine iotk_write_dat_INTEGER1_5(unit,name,dat,dummy,attr,columns,sep,fmt,ierr)
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
  INTEGER (kind=__IOTK_INTEGER1), intent(in)  :: dat (:,:,:,:,:) 
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
# 114 "iotk_dat.spp"
  INTEGER (kind=__IOTK_INTEGER1),allocatable :: dattmp(:)
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
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
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
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
    goto 1
  end if
  call iotk_strcpy(usefmt,"!",ierrl)
  if(ierrl/=0) then
    call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 142 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
    goto 1
  end if
  if(present(fmt) .and. .not. raw) call iotk_strcpy(usefmt,iotk_strtrim(fmt),ierrl)
  if(ierrl/=0) then
    call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 147 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
    goto 1
  end if
  if(iotk_strscan(usefmt,"<>&")/=0) then
    call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 151 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
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
  call iotk_write_attr(lattr,"type",iotk_tolower("INTEGER"),first=.true.,ierr=ierrl)
  if(ierrl/=0) then
    call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 156 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
    goto 1
  end if
  call iotk_write_attr(lattr,"size",size(dat),ierr=ierrl)
  if(ierrl/=0) then
    call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 161 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
    goto 1
  end if
# 171 "iotk_dat.spp"
  if(binary) then
    call iotk_write_attr(lattr,"kind",kind(dat),ierr=ierrl)
    if(ierrl/=0) then
      call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 174 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
      goto 1
    end if
  end if
# 179 "iotk_dat.spp"
  if(.not.iotk_strcomp(usefmt,"!")) call iotk_write_attr(lattr,"fmt",iotk_strtrim(usefmt),ierr=ierrl)
  if(ierrl/=0) then
    call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 181 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
    goto 1
  end if
  if(lcolumns/=1) call iotk_write_attr(lattr,"columns",lcolumns,ierr=ierrl)
  if(ierrl/=0) then
    call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 186 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
    goto 1
  end if
  if(present(attr)) then
    attr_tmp(1:1)=iotk_eos
    call iotk_strcpy(attr_tmp,attr,ierr=ierrl)
    if(ierrl/=0) then
      call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 193 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
      goto 1
    end if
    call iotk_delete_attr(attr_tmp,"type",ierrl)
    if(ierrl/=0) then
      call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 198 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
      goto 1
    end if
    call iotk_delete_attr(attr_tmp,"kind",ierrl)
    if(ierrl/=0) then
      call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 203 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
      goto 1
    end if
    call iotk_delete_attr(attr_tmp,"size",ierrl)
    if(ierrl/=0) then
      call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 208 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
      goto 1
    end if
    call iotk_delete_attr(attr_tmp,"fmt",ierrl)
    if(ierrl/=0) then
      call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 213 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
      goto 1
    end if
    call iotk_delete_attr(attr_tmp,"columns",ierrl)
    if(ierrl/=0) then
      call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 218 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
      goto 1
    end if
    call iotk_delete_attr(attr_tmp,"len",ierrl)
    if(ierrl/=0) then
      call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 223 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
      goto 1
    end if
    if(iotk_strlen_trim(attr_tmp)>0) call iotk_strcat(lattr,iotk_strtrim(attr_tmp),ierr=ierrl)
  end if
  if(ierrl/=0) then
    call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 229 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
    goto 1
  end if
  call iotk_write_begin(unit,name,lattr,ierr=ierrl)
  if(ierrl/=0) then
    call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 234 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
    goto 1
  end if

  allocate(dattmp(size(dat)))
# 242 "iotk_dat.spp"
#if defined(__IOTK_WORKAROUND3) || defined(__IOTK_WORKAROUND4)
# 246 "iotk_dat.spp"
     call iotk_private_pack_INTEGER1(dattmp,dat,size(dattmp),1)
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
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
        goto 1
      end if
    else
      write(lunit,iostat=iostat) idummy,(dattmp(itmp),itmp=1,size(dattmp))
      if(iostat/=0) then
        call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 263 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
        goto 1
      end if
    end if
  else
    if(raw) then
# 272 "iotk_dat.spp"
      write(lunit,*,iostat=iostat) (dattmp(itmp),itmp=1,size(dattmp))
# 274 "iotk_dat.spp"
      if(iostat/=0) then
        call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 275 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
        goto 1
      end if
    else if(iotk_strcomp(usefmt,"*")) then
      write(lunit,*,iostat=iostat) (dattmp(itmp),itmp=1,size(dattmp))
      if(iostat/=0) then
        call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 281 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
        goto 1
      end if
    else if(iotk_strcomp(usefmt,"!")) then
# 295 "iotk_dat.spp"
     write(lunit,fmt=trim(iotk_wfmt("INTEGER",kind(dattmp),lcolumns,-1,lsep)),iostat=iostat) (dattmp(itmp),itmp=1,size(dattmp))
     if(iostat/=0) then
      call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 297 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
      goto 1
     end if
# 301 "iotk_dat.spp"
    else
      write(lunit,fmt=usefmt(1:iotk_strlen(usefmt)),iostat=iostat) (dattmp(itmp),itmp=1,size(dattmp))
      if(iostat/=0) then
        call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 304 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
        goto 1
      end if
    end if
  end if
  call iotk_write_end(unit,name,ierr=ierrl)
  if(ierrl/=0) then
    call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 311 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
    goto 1
  end if
1 continue
  if(allocated(dattmp)) deallocate(dattmp)
  if(present(ierr)) then
    ierr = ierrl
  else
    if(ierrl/=0) call iotk_error_handler(ierrl)
  end if
end subroutine iotk_write_dat_INTEGER1_5


# 589 "iotk_dat.spp"

# 591 "iotk_dat.spp"
subroutine iotk_scan_dat_INTEGER1_5(unit,name,dat,dummy,attr,found,default,ierr)
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
  INTEGER (kind=__IOTK_INTEGER1)                        :: dat (:,:,:,:,:)
#else
  INTEGER (kind=__IOTK_INTEGER1),           intent(out) :: dat (:,:,:,:,:)
#endif
  type(iotk_dummytype), optional         :: dummy
#ifdef __IOTK_WORKAROUND6
  character(len=*),optional              :: attr
#else
  character(len=*),optional, intent(out) :: attr
#endif
  logical,         optional, intent(out) :: found
  INTEGER (kind=__IOTK_INTEGER1), optional, intent(in)  :: default (:,:,:,:,:)
  integer,         optional, intent(out) :: ierr
# 618 "iotk_dat.spp"
  INTEGER (kind=__IOTK_INTEGER1),              allocatable :: tmpdat(:)
# 620 "iotk_dat.spp"
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
# 637 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
    goto 1
  end if
  call iotk_parse_dat(lattr,rtype,rkind,rsize,rlen,fmt,columns,ierrl)
! Note that columns is not effectively used
  if(ierrl/=0) goto 1
  if(.not. (iotk_strcomp(rtype,iotk_eos) .or. iotk_strcomp(rtype,"INTEGER") ) ) then
    call iotk_error_issue(ierrl,"iotk_scan_dat",__FILE__,__LINE__)
# 644 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
# 644 "iotk_dat.spp"
call iotk_error_msg(ierrl,' ')
# 644 "iotk_dat.spp"
call iotk_error_write(ierrl,"rtype",rtype(1:iotk_strlen(rtype)))
# 644 "iotk_dat.spp"
call iotk_error_write(ierrl,"type","INTEGER")
    goto 1
  end if
  if(.not. (rsize==-1 .or. rsize==size(dat)) ) then
    call iotk_error_issue(ierrl,"iotk_scan_dat",__FILE__,__LINE__)
# 648 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
    goto 1
  end if
  if(rkind==-1) rkind = kind(dat)
# 655 "iotk_dat.spp"

  allocate(tmpdat(size(dat)))
  call iotk_scan_dat_aux(unit,tmpdat,rkind,rlen,fmt(1:iotk_strlen(fmt)),ierrl)
# 662 "iotk_dat.spp"
#ifdef __IOTK_WORKAROUND7
        call iotk_reshape_INTEGER1( size(dat),dat,tmpdat )
#else
        dat = reshape(tmpdat,shape(dat))
#endif
# 671 "iotk_dat.spp"
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
# 683 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
# 683 "iotk_dat.spp"
call iotk_error_msg(ierrl,'Dat not found')
# 683 "iotk_dat.spp"
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
end subroutine iotk_scan_dat_INTEGER1_5


#endif
#endif

subroutine iotk_dat_dummy_INTEGER1_5
  write(0,*)
end subroutine iotk_dat_dummy_INTEGER1_5


# 727 "iotk_dat.spp"


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

#ifdef __IOTK_INTEGER1
#if 6 <= __IOTK_MAXRANK
# 82 "iotk_dat.spp"
subroutine iotk_write_dat_INTEGER1_6(unit,name,dat,dummy,attr,columns,sep,fmt,ierr)
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
  INTEGER (kind=__IOTK_INTEGER1), intent(in)  :: dat (:,:,:,:,:,:) 
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
# 114 "iotk_dat.spp"
  INTEGER (kind=__IOTK_INTEGER1),allocatable :: dattmp(:)
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
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
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
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
    goto 1
  end if
  call iotk_strcpy(usefmt,"!",ierrl)
  if(ierrl/=0) then
    call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 142 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
    goto 1
  end if
  if(present(fmt) .and. .not. raw) call iotk_strcpy(usefmt,iotk_strtrim(fmt),ierrl)
  if(ierrl/=0) then
    call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 147 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
    goto 1
  end if
  if(iotk_strscan(usefmt,"<>&")/=0) then
    call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 151 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
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
  call iotk_write_attr(lattr,"type",iotk_tolower("INTEGER"),first=.true.,ierr=ierrl)
  if(ierrl/=0) then
    call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 156 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
    goto 1
  end if
  call iotk_write_attr(lattr,"size",size(dat),ierr=ierrl)
  if(ierrl/=0) then
    call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 161 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
    goto 1
  end if
# 171 "iotk_dat.spp"
  if(binary) then
    call iotk_write_attr(lattr,"kind",kind(dat),ierr=ierrl)
    if(ierrl/=0) then
      call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 174 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
      goto 1
    end if
  end if
# 179 "iotk_dat.spp"
  if(.not.iotk_strcomp(usefmt,"!")) call iotk_write_attr(lattr,"fmt",iotk_strtrim(usefmt),ierr=ierrl)
  if(ierrl/=0) then
    call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 181 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
    goto 1
  end if
  if(lcolumns/=1) call iotk_write_attr(lattr,"columns",lcolumns,ierr=ierrl)
  if(ierrl/=0) then
    call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 186 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
    goto 1
  end if
  if(present(attr)) then
    attr_tmp(1:1)=iotk_eos
    call iotk_strcpy(attr_tmp,attr,ierr=ierrl)
    if(ierrl/=0) then
      call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 193 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
      goto 1
    end if
    call iotk_delete_attr(attr_tmp,"type",ierrl)
    if(ierrl/=0) then
      call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 198 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
      goto 1
    end if
    call iotk_delete_attr(attr_tmp,"kind",ierrl)
    if(ierrl/=0) then
      call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 203 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
      goto 1
    end if
    call iotk_delete_attr(attr_tmp,"size",ierrl)
    if(ierrl/=0) then
      call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 208 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
      goto 1
    end if
    call iotk_delete_attr(attr_tmp,"fmt",ierrl)
    if(ierrl/=0) then
      call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 213 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
      goto 1
    end if
    call iotk_delete_attr(attr_tmp,"columns",ierrl)
    if(ierrl/=0) then
      call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 218 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
      goto 1
    end if
    call iotk_delete_attr(attr_tmp,"len",ierrl)
    if(ierrl/=0) then
      call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 223 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
      goto 1
    end if
    if(iotk_strlen_trim(attr_tmp)>0) call iotk_strcat(lattr,iotk_strtrim(attr_tmp),ierr=ierrl)
  end if
  if(ierrl/=0) then
    call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 229 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
    goto 1
  end if
  call iotk_write_begin(unit,name,lattr,ierr=ierrl)
  if(ierrl/=0) then
    call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 234 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
    goto 1
  end if

  allocate(dattmp(size(dat)))
# 242 "iotk_dat.spp"
#if defined(__IOTK_WORKAROUND3) || defined(__IOTK_WORKAROUND4)
# 246 "iotk_dat.spp"
     call iotk_private_pack_INTEGER1(dattmp,dat,size(dattmp),1)
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
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
        goto 1
      end if
    else
      write(lunit,iostat=iostat) idummy,(dattmp(itmp),itmp=1,size(dattmp))
      if(iostat/=0) then
        call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 263 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
        goto 1
      end if
    end if
  else
    if(raw) then
# 272 "iotk_dat.spp"
      write(lunit,*,iostat=iostat) (dattmp(itmp),itmp=1,size(dattmp))
# 274 "iotk_dat.spp"
      if(iostat/=0) then
        call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 275 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
        goto 1
      end if
    else if(iotk_strcomp(usefmt,"*")) then
      write(lunit,*,iostat=iostat) (dattmp(itmp),itmp=1,size(dattmp))
      if(iostat/=0) then
        call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 281 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
        goto 1
      end if
    else if(iotk_strcomp(usefmt,"!")) then
# 295 "iotk_dat.spp"
     write(lunit,fmt=trim(iotk_wfmt("INTEGER",kind(dattmp),lcolumns,-1,lsep)),iostat=iostat) (dattmp(itmp),itmp=1,size(dattmp))
     if(iostat/=0) then
      call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 297 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
      goto 1
     end if
# 301 "iotk_dat.spp"
    else
      write(lunit,fmt=usefmt(1:iotk_strlen(usefmt)),iostat=iostat) (dattmp(itmp),itmp=1,size(dattmp))
      if(iostat/=0) then
        call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 304 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
        goto 1
      end if
    end if
  end if
  call iotk_write_end(unit,name,ierr=ierrl)
  if(ierrl/=0) then
    call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 311 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
    goto 1
  end if
1 continue
  if(allocated(dattmp)) deallocate(dattmp)
  if(present(ierr)) then
    ierr = ierrl
  else
    if(ierrl/=0) call iotk_error_handler(ierrl)
  end if
end subroutine iotk_write_dat_INTEGER1_6


# 589 "iotk_dat.spp"

# 591 "iotk_dat.spp"
subroutine iotk_scan_dat_INTEGER1_6(unit,name,dat,dummy,attr,found,default,ierr)
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
  INTEGER (kind=__IOTK_INTEGER1)                        :: dat (:,:,:,:,:,:)
#else
  INTEGER (kind=__IOTK_INTEGER1),           intent(out) :: dat (:,:,:,:,:,:)
#endif
  type(iotk_dummytype), optional         :: dummy
#ifdef __IOTK_WORKAROUND6
  character(len=*),optional              :: attr
#else
  character(len=*),optional, intent(out) :: attr
#endif
  logical,         optional, intent(out) :: found
  INTEGER (kind=__IOTK_INTEGER1), optional, intent(in)  :: default (:,:,:,:,:,:)
  integer,         optional, intent(out) :: ierr
# 618 "iotk_dat.spp"
  INTEGER (kind=__IOTK_INTEGER1),              allocatable :: tmpdat(:)
# 620 "iotk_dat.spp"
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
# 637 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
    goto 1
  end if
  call iotk_parse_dat(lattr,rtype,rkind,rsize,rlen,fmt,columns,ierrl)
! Note that columns is not effectively used
  if(ierrl/=0) goto 1
  if(.not. (iotk_strcomp(rtype,iotk_eos) .or. iotk_strcomp(rtype,"INTEGER") ) ) then
    call iotk_error_issue(ierrl,"iotk_scan_dat",__FILE__,__LINE__)
# 644 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
# 644 "iotk_dat.spp"
call iotk_error_msg(ierrl,' ')
# 644 "iotk_dat.spp"
call iotk_error_write(ierrl,"rtype",rtype(1:iotk_strlen(rtype)))
# 644 "iotk_dat.spp"
call iotk_error_write(ierrl,"type","INTEGER")
    goto 1
  end if
  if(.not. (rsize==-1 .or. rsize==size(dat)) ) then
    call iotk_error_issue(ierrl,"iotk_scan_dat",__FILE__,__LINE__)
# 648 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
    goto 1
  end if
  if(rkind==-1) rkind = kind(dat)
# 655 "iotk_dat.spp"

  allocate(tmpdat(size(dat)))
  call iotk_scan_dat_aux(unit,tmpdat,rkind,rlen,fmt(1:iotk_strlen(fmt)),ierrl)
# 662 "iotk_dat.spp"
#ifdef __IOTK_WORKAROUND7
        call iotk_reshape_INTEGER1( size(dat),dat,tmpdat )
#else
        dat = reshape(tmpdat,shape(dat))
#endif
# 671 "iotk_dat.spp"
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
# 683 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
# 683 "iotk_dat.spp"
call iotk_error_msg(ierrl,'Dat not found')
# 683 "iotk_dat.spp"
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
end subroutine iotk_scan_dat_INTEGER1_6


#endif
#endif

subroutine iotk_dat_dummy_INTEGER1_6
  write(0,*)
end subroutine iotk_dat_dummy_INTEGER1_6


# 727 "iotk_dat.spp"


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

#ifdef __IOTK_INTEGER1
#if 7 <= __IOTK_MAXRANK
# 82 "iotk_dat.spp"
subroutine iotk_write_dat_INTEGER1_7(unit,name,dat,dummy,attr,columns,sep,fmt,ierr)
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
  INTEGER (kind=__IOTK_INTEGER1), intent(in)  :: dat (:,:,:,:,:,:,:) 
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
# 114 "iotk_dat.spp"
  INTEGER (kind=__IOTK_INTEGER1),allocatable :: dattmp(:)
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
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
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
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
    goto 1
  end if
  call iotk_strcpy(usefmt,"!",ierrl)
  if(ierrl/=0) then
    call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 142 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
    goto 1
  end if
  if(present(fmt) .and. .not. raw) call iotk_strcpy(usefmt,iotk_strtrim(fmt),ierrl)
  if(ierrl/=0) then
    call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 147 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
    goto 1
  end if
  if(iotk_strscan(usefmt,"<>&")/=0) then
    call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 151 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
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
  call iotk_write_attr(lattr,"type",iotk_tolower("INTEGER"),first=.true.,ierr=ierrl)
  if(ierrl/=0) then
    call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 156 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
    goto 1
  end if
  call iotk_write_attr(lattr,"size",size(dat),ierr=ierrl)
  if(ierrl/=0) then
    call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 161 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
    goto 1
  end if
# 171 "iotk_dat.spp"
  if(binary) then
    call iotk_write_attr(lattr,"kind",kind(dat),ierr=ierrl)
    if(ierrl/=0) then
      call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 174 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
      goto 1
    end if
  end if
# 179 "iotk_dat.spp"
  if(.not.iotk_strcomp(usefmt,"!")) call iotk_write_attr(lattr,"fmt",iotk_strtrim(usefmt),ierr=ierrl)
  if(ierrl/=0) then
    call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 181 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
    goto 1
  end if
  if(lcolumns/=1) call iotk_write_attr(lattr,"columns",lcolumns,ierr=ierrl)
  if(ierrl/=0) then
    call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 186 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
    goto 1
  end if
  if(present(attr)) then
    attr_tmp(1:1)=iotk_eos
    call iotk_strcpy(attr_tmp,attr,ierr=ierrl)
    if(ierrl/=0) then
      call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 193 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
      goto 1
    end if
    call iotk_delete_attr(attr_tmp,"type",ierrl)
    if(ierrl/=0) then
      call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 198 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
      goto 1
    end if
    call iotk_delete_attr(attr_tmp,"kind",ierrl)
    if(ierrl/=0) then
      call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 203 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
      goto 1
    end if
    call iotk_delete_attr(attr_tmp,"size",ierrl)
    if(ierrl/=0) then
      call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 208 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
      goto 1
    end if
    call iotk_delete_attr(attr_tmp,"fmt",ierrl)
    if(ierrl/=0) then
      call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 213 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
      goto 1
    end if
    call iotk_delete_attr(attr_tmp,"columns",ierrl)
    if(ierrl/=0) then
      call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 218 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
      goto 1
    end if
    call iotk_delete_attr(attr_tmp,"len",ierrl)
    if(ierrl/=0) then
      call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 223 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
      goto 1
    end if
    if(iotk_strlen_trim(attr_tmp)>0) call iotk_strcat(lattr,iotk_strtrim(attr_tmp),ierr=ierrl)
  end if
  if(ierrl/=0) then
    call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 229 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
    goto 1
  end if
  call iotk_write_begin(unit,name,lattr,ierr=ierrl)
  if(ierrl/=0) then
    call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 234 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
    goto 1
  end if

  allocate(dattmp(size(dat)))
# 242 "iotk_dat.spp"
#if defined(__IOTK_WORKAROUND3) || defined(__IOTK_WORKAROUND4)
# 246 "iotk_dat.spp"
     call iotk_private_pack_INTEGER1(dattmp,dat,size(dattmp),1)
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
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
        goto 1
      end if
    else
      write(lunit,iostat=iostat) idummy,(dattmp(itmp),itmp=1,size(dattmp))
      if(iostat/=0) then
        call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 263 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
        goto 1
      end if
    end if
  else
    if(raw) then
# 272 "iotk_dat.spp"
      write(lunit,*,iostat=iostat) (dattmp(itmp),itmp=1,size(dattmp))
# 274 "iotk_dat.spp"
      if(iostat/=0) then
        call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 275 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
        goto 1
      end if
    else if(iotk_strcomp(usefmt,"*")) then
      write(lunit,*,iostat=iostat) (dattmp(itmp),itmp=1,size(dattmp))
      if(iostat/=0) then
        call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 281 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
        goto 1
      end if
    else if(iotk_strcomp(usefmt,"!")) then
# 295 "iotk_dat.spp"
     write(lunit,fmt=trim(iotk_wfmt("INTEGER",kind(dattmp),lcolumns,-1,lsep)),iostat=iostat) (dattmp(itmp),itmp=1,size(dattmp))
     if(iostat/=0) then
      call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 297 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
      goto 1
     end if
# 301 "iotk_dat.spp"
    else
      write(lunit,fmt=usefmt(1:iotk_strlen(usefmt)),iostat=iostat) (dattmp(itmp),itmp=1,size(dattmp))
      if(iostat/=0) then
        call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 304 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
        goto 1
      end if
    end if
  end if
  call iotk_write_end(unit,name,ierr=ierrl)
  if(ierrl/=0) then
    call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 311 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
    goto 1
  end if
1 continue
  if(allocated(dattmp)) deallocate(dattmp)
  if(present(ierr)) then
    ierr = ierrl
  else
    if(ierrl/=0) call iotk_error_handler(ierrl)
  end if
end subroutine iotk_write_dat_INTEGER1_7


# 589 "iotk_dat.spp"

# 591 "iotk_dat.spp"
subroutine iotk_scan_dat_INTEGER1_7(unit,name,dat,dummy,attr,found,default,ierr)
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
  INTEGER (kind=__IOTK_INTEGER1)                        :: dat (:,:,:,:,:,:,:)
#else
  INTEGER (kind=__IOTK_INTEGER1),           intent(out) :: dat (:,:,:,:,:,:,:)
#endif
  type(iotk_dummytype), optional         :: dummy
#ifdef __IOTK_WORKAROUND6
  character(len=*),optional              :: attr
#else
  character(len=*),optional, intent(out) :: attr
#endif
  logical,         optional, intent(out) :: found
  INTEGER (kind=__IOTK_INTEGER1), optional, intent(in)  :: default (:,:,:,:,:,:,:)
  integer,         optional, intent(out) :: ierr
# 618 "iotk_dat.spp"
  INTEGER (kind=__IOTK_INTEGER1),              allocatable :: tmpdat(:)
# 620 "iotk_dat.spp"
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
# 637 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
    goto 1
  end if
  call iotk_parse_dat(lattr,rtype,rkind,rsize,rlen,fmt,columns,ierrl)
! Note that columns is not effectively used
  if(ierrl/=0) goto 1
  if(.not. (iotk_strcomp(rtype,iotk_eos) .or. iotk_strcomp(rtype,"INTEGER") ) ) then
    call iotk_error_issue(ierrl,"iotk_scan_dat",__FILE__,__LINE__)
# 644 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
# 644 "iotk_dat.spp"
call iotk_error_msg(ierrl,' ')
# 644 "iotk_dat.spp"
call iotk_error_write(ierrl,"rtype",rtype(1:iotk_strlen(rtype)))
# 644 "iotk_dat.spp"
call iotk_error_write(ierrl,"type","INTEGER")
    goto 1
  end if
  if(.not. (rsize==-1 .or. rsize==size(dat)) ) then
    call iotk_error_issue(ierrl,"iotk_scan_dat",__FILE__,__LINE__)
# 648 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
    goto 1
  end if
  if(rkind==-1) rkind = kind(dat)
# 655 "iotk_dat.spp"

  allocate(tmpdat(size(dat)))
  call iotk_scan_dat_aux(unit,tmpdat,rkind,rlen,fmt(1:iotk_strlen(fmt)),ierrl)
# 662 "iotk_dat.spp"
#ifdef __IOTK_WORKAROUND7
        call iotk_reshape_INTEGER1( size(dat),dat,tmpdat )
#else
        dat = reshape(tmpdat,shape(dat))
#endif
# 671 "iotk_dat.spp"
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
# 683 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
# 683 "iotk_dat.spp"
call iotk_error_msg(ierrl,'Dat not found')
# 683 "iotk_dat.spp"
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
end subroutine iotk_scan_dat_INTEGER1_7


#endif
#endif

subroutine iotk_dat_dummy_INTEGER1_7
  write(0,*)
end subroutine iotk_dat_dummy_INTEGER1_7


# 727 "iotk_dat.spp"


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

#ifdef __IOTK_INTEGER2
#if 0 <= __IOTK_MAXRANK
# 82 "iotk_dat.spp"
subroutine iotk_write_dat_INTEGER2_0(unit,name,dat,dummy,attr,columns,sep,fmt,ierr)
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
  INTEGER (kind=__IOTK_INTEGER2), intent(in)  :: dat  
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
# 114 "iotk_dat.spp"
  INTEGER (kind=__IOTK_INTEGER2),allocatable :: dattmp(:)
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
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
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
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
    goto 1
  end if
  call iotk_strcpy(usefmt,"!",ierrl)
  if(ierrl/=0) then
    call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 142 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
    goto 1
  end if
  if(present(fmt) .and. .not. raw) call iotk_strcpy(usefmt,iotk_strtrim(fmt),ierrl)
  if(ierrl/=0) then
    call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 147 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
    goto 1
  end if
  if(iotk_strscan(usefmt,"<>&")/=0) then
    call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 151 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
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
  call iotk_write_attr(lattr,"type",iotk_tolower("INTEGER"),first=.true.,ierr=ierrl)
  if(ierrl/=0) then
    call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 156 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
    goto 1
  end if
  call iotk_write_attr(lattr,"size",1,ierr=ierrl)
  if(ierrl/=0) then
    call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 161 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
    goto 1
  end if
# 171 "iotk_dat.spp"
  if(binary) then
    call iotk_write_attr(lattr,"kind",kind(dat),ierr=ierrl)
    if(ierrl/=0) then
      call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 174 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
      goto 1
    end if
  end if
# 179 "iotk_dat.spp"
  if(.not.iotk_strcomp(usefmt,"!")) call iotk_write_attr(lattr,"fmt",iotk_strtrim(usefmt),ierr=ierrl)
  if(ierrl/=0) then
    call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 181 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
    goto 1
  end if
  if(lcolumns/=1) call iotk_write_attr(lattr,"columns",lcolumns,ierr=ierrl)
  if(ierrl/=0) then
    call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 186 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
    goto 1
  end if
  if(present(attr)) then
    attr_tmp(1:1)=iotk_eos
    call iotk_strcpy(attr_tmp,attr,ierr=ierrl)
    if(ierrl/=0) then
      call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 193 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
      goto 1
    end if
    call iotk_delete_attr(attr_tmp,"type",ierrl)
    if(ierrl/=0) then
      call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 198 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
      goto 1
    end if
    call iotk_delete_attr(attr_tmp,"kind",ierrl)
    if(ierrl/=0) then
      call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 203 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
      goto 1
    end if
    call iotk_delete_attr(attr_tmp,"size",ierrl)
    if(ierrl/=0) then
      call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 208 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
      goto 1
    end if
    call iotk_delete_attr(attr_tmp,"fmt",ierrl)
    if(ierrl/=0) then
      call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 213 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
      goto 1
    end if
    call iotk_delete_attr(attr_tmp,"columns",ierrl)
    if(ierrl/=0) then
      call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 218 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
      goto 1
    end if
    call iotk_delete_attr(attr_tmp,"len",ierrl)
    if(ierrl/=0) then
      call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 223 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
      goto 1
    end if
    if(iotk_strlen_trim(attr_tmp)>0) call iotk_strcat(lattr,iotk_strtrim(attr_tmp),ierr=ierrl)
  end if
  if(ierrl/=0) then
    call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 229 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
    goto 1
  end if
  call iotk_write_begin(unit,name,lattr,ierr=ierrl)
  if(ierrl/=0) then
    call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 234 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
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
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
        goto 1
      end if
    else
      write(lunit,iostat=iostat) idummy,(dattmp(itmp),itmp=1,size(dattmp))
      if(iostat/=0) then
        call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 263 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
        goto 1
      end if
    end if
  else
    if(raw) then
# 272 "iotk_dat.spp"
      write(lunit,*,iostat=iostat) (dattmp(itmp),itmp=1,size(dattmp))
# 274 "iotk_dat.spp"
      if(iostat/=0) then
        call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 275 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
        goto 1
      end if
    else if(iotk_strcomp(usefmt,"*")) then
      write(lunit,*,iostat=iostat) (dattmp(itmp),itmp=1,size(dattmp))
      if(iostat/=0) then
        call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 281 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
        goto 1
      end if
    else if(iotk_strcomp(usefmt,"!")) then
# 295 "iotk_dat.spp"
     write(lunit,fmt=trim(iotk_wfmt("INTEGER",kind(dattmp),lcolumns,-1,lsep)),iostat=iostat) (dattmp(itmp),itmp=1,size(dattmp))
     if(iostat/=0) then
      call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 297 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
      goto 1
     end if
# 301 "iotk_dat.spp"
    else
      write(lunit,fmt=usefmt(1:iotk_strlen(usefmt)),iostat=iostat) (dattmp(itmp),itmp=1,size(dattmp))
      if(iostat/=0) then
        call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 304 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
        goto 1
      end if
    end if
  end if
  call iotk_write_end(unit,name,ierr=ierrl)
  if(ierrl/=0) then
    call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 311 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
    goto 1
  end if
1 continue
  if(allocated(dattmp)) deallocate(dattmp)
  if(present(ierr)) then
    ierr = ierrl
  else
    if(ierrl/=0) call iotk_error_handler(ierrl)
  end if
end subroutine iotk_write_dat_INTEGER2_0


# 589 "iotk_dat.spp"

# 591 "iotk_dat.spp"
subroutine iotk_scan_dat_INTEGER2_0(unit,name,dat,dummy,attr,found,default,ierr)
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
  INTEGER (kind=__IOTK_INTEGER2)                        :: dat 
#else
  INTEGER (kind=__IOTK_INTEGER2),           intent(out) :: dat 
#endif
  type(iotk_dummytype), optional         :: dummy
#ifdef __IOTK_WORKAROUND6
  character(len=*),optional              :: attr
#else
  character(len=*),optional, intent(out) :: attr
#endif
  logical,         optional, intent(out) :: found
  INTEGER (kind=__IOTK_INTEGER2), optional, intent(in)  :: default 
  integer,         optional, intent(out) :: ierr
# 618 "iotk_dat.spp"
  INTEGER (kind=__IOTK_INTEGER2),              allocatable :: tmpdat(:)
# 620 "iotk_dat.spp"
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
# 637 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
    goto 1
  end if
  call iotk_parse_dat(lattr,rtype,rkind,rsize,rlen,fmt,columns,ierrl)
! Note that columns is not effectively used
  if(ierrl/=0) goto 1
  if(.not. (iotk_strcomp(rtype,iotk_eos) .or. iotk_strcomp(rtype,"INTEGER") ) ) then
    call iotk_error_issue(ierrl,"iotk_scan_dat",__FILE__,__LINE__)
# 644 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
# 644 "iotk_dat.spp"
call iotk_error_msg(ierrl,' ')
# 644 "iotk_dat.spp"
call iotk_error_write(ierrl,"rtype",rtype(1:iotk_strlen(rtype)))
# 644 "iotk_dat.spp"
call iotk_error_write(ierrl,"type","INTEGER")
    goto 1
  end if
  if(.not. (rsize==-1 .or. rsize==1) ) then
    call iotk_error_issue(ierrl,"iotk_scan_dat",__FILE__,__LINE__)
# 648 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
    goto 1
  end if
  if(rkind==-1) rkind = kind(dat)
# 655 "iotk_dat.spp"

  allocate(tmpdat(1))
  call iotk_scan_dat_aux(unit,tmpdat,rkind,rlen,fmt(1:iotk_strlen(fmt)),ierrl)
# 659 "iotk_dat.spp"
        dat = tmpdat(1)
# 671 "iotk_dat.spp"
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
# 683 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
# 683 "iotk_dat.spp"
call iotk_error_msg(ierrl,'Dat not found')
# 683 "iotk_dat.spp"
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
end subroutine iotk_scan_dat_INTEGER2_0


#endif
#endif

subroutine iotk_dat_dummy_INTEGER2_0
  write(0,*)
end subroutine iotk_dat_dummy_INTEGER2_0


# 727 "iotk_dat.spp"


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

#ifdef __IOTK_INTEGER2
#if 1 <= __IOTK_MAXRANK
# 82 "iotk_dat.spp"
subroutine iotk_write_dat_INTEGER2_1(unit,name,dat,dummy,attr,columns,sep,fmt,ierr)
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
  INTEGER (kind=__IOTK_INTEGER2), intent(in)  :: dat (:) 
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
# 114 "iotk_dat.spp"
  INTEGER (kind=__IOTK_INTEGER2),allocatable :: dattmp(:)
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
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
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
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
    goto 1
  end if
  call iotk_strcpy(usefmt,"!",ierrl)
  if(ierrl/=0) then
    call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 142 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
    goto 1
  end if
  if(present(fmt) .and. .not. raw) call iotk_strcpy(usefmt,iotk_strtrim(fmt),ierrl)
  if(ierrl/=0) then
    call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 147 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
    goto 1
  end if
  if(iotk_strscan(usefmt,"<>&")/=0) then
    call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 151 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
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
  call iotk_write_attr(lattr,"type",iotk_tolower("INTEGER"),first=.true.,ierr=ierrl)
  if(ierrl/=0) then
    call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 156 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
    goto 1
  end if
  call iotk_write_attr(lattr,"size",size(dat),ierr=ierrl)
  if(ierrl/=0) then
    call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 161 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
    goto 1
  end if
# 171 "iotk_dat.spp"
  if(binary) then
    call iotk_write_attr(lattr,"kind",kind(dat),ierr=ierrl)
    if(ierrl/=0) then
      call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 174 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
      goto 1
    end if
  end if
# 179 "iotk_dat.spp"
  if(.not.iotk_strcomp(usefmt,"!")) call iotk_write_attr(lattr,"fmt",iotk_strtrim(usefmt),ierr=ierrl)
  if(ierrl/=0) then
    call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 181 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
    goto 1
  end if
  if(lcolumns/=1) call iotk_write_attr(lattr,"columns",lcolumns,ierr=ierrl)
  if(ierrl/=0) then
    call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 186 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
    goto 1
  end if
  if(present(attr)) then
    attr_tmp(1:1)=iotk_eos
    call iotk_strcpy(attr_tmp,attr,ierr=ierrl)
    if(ierrl/=0) then
      call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 193 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
      goto 1
    end if
    call iotk_delete_attr(attr_tmp,"type",ierrl)
    if(ierrl/=0) then
      call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 198 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
      goto 1
    end if
    call iotk_delete_attr(attr_tmp,"kind",ierrl)
    if(ierrl/=0) then
      call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 203 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
      goto 1
    end if
    call iotk_delete_attr(attr_tmp,"size",ierrl)
    if(ierrl/=0) then
      call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 208 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
      goto 1
    end if
    call iotk_delete_attr(attr_tmp,"fmt",ierrl)
    if(ierrl/=0) then
      call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 213 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
      goto 1
    end if
    call iotk_delete_attr(attr_tmp,"columns",ierrl)
    if(ierrl/=0) then
      call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 218 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
      goto 1
    end if
    call iotk_delete_attr(attr_tmp,"len",ierrl)
    if(ierrl/=0) then
      call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 223 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
      goto 1
    end if
    if(iotk_strlen_trim(attr_tmp)>0) call iotk_strcat(lattr,iotk_strtrim(attr_tmp),ierr=ierrl)
  end if
  if(ierrl/=0) then
    call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 229 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
    goto 1
  end if
  call iotk_write_begin(unit,name,lattr,ierr=ierrl)
  if(ierrl/=0) then
    call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 234 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
    goto 1
  end if

  allocate(dattmp(size(dat)))
# 242 "iotk_dat.spp"
#if defined(__IOTK_WORKAROUND3) || defined(__IOTK_WORKAROUND4)
# 246 "iotk_dat.spp"
     call iotk_private_pack_INTEGER2(dattmp,dat,size(dattmp),1)
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
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
        goto 1
      end if
    else
      write(lunit,iostat=iostat) idummy,(dattmp(itmp),itmp=1,size(dattmp))
      if(iostat/=0) then
        call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 263 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
        goto 1
      end if
    end if
  else
    if(raw) then
# 272 "iotk_dat.spp"
      write(lunit,*,iostat=iostat) (dattmp(itmp),itmp=1,size(dattmp))
# 274 "iotk_dat.spp"
      if(iostat/=0) then
        call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 275 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
        goto 1
      end if
    else if(iotk_strcomp(usefmt,"*")) then
      write(lunit,*,iostat=iostat) (dattmp(itmp),itmp=1,size(dattmp))
      if(iostat/=0) then
        call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 281 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
        goto 1
      end if
    else if(iotk_strcomp(usefmt,"!")) then
# 295 "iotk_dat.spp"
     write(lunit,fmt=trim(iotk_wfmt("INTEGER",kind(dattmp),lcolumns,-1,lsep)),iostat=iostat) (dattmp(itmp),itmp=1,size(dattmp))
     if(iostat/=0) then
      call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 297 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
      goto 1
     end if
# 301 "iotk_dat.spp"
    else
      write(lunit,fmt=usefmt(1:iotk_strlen(usefmt)),iostat=iostat) (dattmp(itmp),itmp=1,size(dattmp))
      if(iostat/=0) then
        call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 304 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
        goto 1
      end if
    end if
  end if
  call iotk_write_end(unit,name,ierr=ierrl)
  if(ierrl/=0) then
    call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 311 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
    goto 1
  end if
1 continue
  if(allocated(dattmp)) deallocate(dattmp)
  if(present(ierr)) then
    ierr = ierrl
  else
    if(ierrl/=0) call iotk_error_handler(ierrl)
  end if
end subroutine iotk_write_dat_INTEGER2_1


# 326 "iotk_dat.spp"
recursive subroutine iotk_scan_dat_aux_INTEGER2(unit,dat,rkind,rlen,fmt,ierr)
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
  INTEGER (kind=__IOTK_INTEGER2)                        :: dat (:)
#else
  INTEGER (kind=__IOTK_INTEGER2),           intent(out) :: dat (:)
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
# 356 "iotk_dat.spp"
#ifdef __IOTK_INTEGER1
  INTEGER (__IOTK_INTEGER1), allocatable :: dat1 (:)
#endif
# 356 "iotk_dat.spp"
#ifdef __IOTK_INTEGER3
  INTEGER (__IOTK_INTEGER3), allocatable :: dat3 (:)
#endif
# 356 "iotk_dat.spp"
#ifdef __IOTK_INTEGER4
  INTEGER (__IOTK_INTEGER4), allocatable :: dat4 (:)
#endif
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
call iotk_error_msg(ierr,"CVS Revision: 1.17 ")
    return
  end if
# 463 "iotk_dat.spp"
  if(binary) then
    select case(rkind)
    case(kind(dat))
      if(raw) then
        read(lunit,iostat=iostat) ( dat(i), i=1,ubound(dat,1) )
        if(iostat/=0) then
          call iotk_error_issue(ierr,"iotk_scan_dat_aux",__FILE__,__LINE__)
# 469 "iotk_dat.spp"
call iotk_error_msg(ierr,"CVS Revision: 1.17 ")
# 469 "iotk_dat.spp"
call iotk_error_msg(ierr,' ')
# 469 "iotk_dat.spp"
call iotk_error_write(ierr,"iostat",iostat)
          return
        end if
      else
        read(lunit,iostat=iostat) idummy, ( dat(i), i=1,ubound(dat,1) )
        if(iostat/=0) then
          call iotk_error_issue(ierr,"iotk_scan_dat_aux",__FILE__,__LINE__)
# 475 "iotk_dat.spp"
call iotk_error_msg(ierr,"CVS Revision: 1.17 ")
# 475 "iotk_dat.spp"
call iotk_error_msg(ierr,' ')
# 475 "iotk_dat.spp"
call iotk_error_write(ierr,"iostat",iostat)
          return
        end if
      end if
# 481 "iotk_dat.spp"
#ifdef __IOTK_INTEGER1
    case(kind(dat1))
      ! Giusto per scrupolo. Se e' raw non ci sono info sul kind, quindi questa linea e' irraggiungibile
      if(raw) then
        call iotk_error_issue(ierr,"iotk_scan_dat_aux",__FILE__,__LINE__)
# 485 "iotk_dat.spp"
call iotk_error_msg(ierr,"CVS Revision: 1.17 ")
        return
      end if
      allocate(dat1(ubound(dat,1)))
      read(lunit,iostat=iostat) idummy,( dat1(i), i=1,ubound(dat1,1) )
      if(iostat/=0) then
        call iotk_error_issue(ierr,"iotk_scan_dat_aux",__FILE__,__LINE__)
# 491 "iotk_dat.spp"
call iotk_error_msg(ierr,"CVS Revision: 1.17 ")
# 491 "iotk_dat.spp"
call iotk_error_msg(ierr,' ')
# 491 "iotk_dat.spp"
call iotk_error_write(ierr,"iostat",iostat)
        return
      end if
# 501 "iotk_dat.spp"
      dat = dat1
# 503 "iotk_dat.spp"
      deallocate(dat1)
#endif
# 481 "iotk_dat.spp"
#ifdef __IOTK_INTEGER3
    case(kind(dat3))
      ! Giusto per scrupolo. Se e' raw non ci sono info sul kind, quindi questa linea e' irraggiungibile
      if(raw) then
        call iotk_error_issue(ierr,"iotk_scan_dat_aux",__FILE__,__LINE__)
# 485 "iotk_dat.spp"
call iotk_error_msg(ierr,"CVS Revision: 1.17 ")
        return
      end if
      allocate(dat3(ubound(dat,1)))
      read(lunit,iostat=iostat) idummy,( dat3(i), i=1,ubound(dat3,1) )
      if(iostat/=0) then
        call iotk_error_issue(ierr,"iotk_scan_dat_aux",__FILE__,__LINE__)
# 491 "iotk_dat.spp"
call iotk_error_msg(ierr,"CVS Revision: 1.17 ")
# 491 "iotk_dat.spp"
call iotk_error_msg(ierr,' ')
# 491 "iotk_dat.spp"
call iotk_error_write(ierr,"iostat",iostat)
        return
      end if
# 501 "iotk_dat.spp"
      dat = dat3
# 503 "iotk_dat.spp"
      deallocate(dat3)
#endif
# 481 "iotk_dat.spp"
#ifdef __IOTK_INTEGER4
    case(kind(dat4))
      ! Giusto per scrupolo. Se e' raw non ci sono info sul kind, quindi questa linea e' irraggiungibile
      if(raw) then
        call iotk_error_issue(ierr,"iotk_scan_dat_aux",__FILE__,__LINE__)
# 485 "iotk_dat.spp"
call iotk_error_msg(ierr,"CVS Revision: 1.17 ")
        return
      end if
      allocate(dat4(ubound(dat,1)))
      read(lunit,iostat=iostat) idummy,( dat4(i), i=1,ubound(dat4,1) )
      if(iostat/=0) then
        call iotk_error_issue(ierr,"iotk_scan_dat_aux",__FILE__,__LINE__)
# 491 "iotk_dat.spp"
call iotk_error_msg(ierr,"CVS Revision: 1.17 ")
# 491 "iotk_dat.spp"
call iotk_error_msg(ierr,' ')
# 491 "iotk_dat.spp"
call iotk_error_write(ierr,"iostat",iostat)
        return
      end if
# 501 "iotk_dat.spp"
      dat = dat4
# 503 "iotk_dat.spp"
      deallocate(dat4)
#endif
# 507 "iotk_dat.spp"
    case default
      call iotk_error_issue(ierr,"iotk_scan_dat_aux",__FILE__,__LINE__)
# 508 "iotk_dat.spp"
call iotk_error_msg(ierr,"CVS Revision: 1.17 ")
# 508 "iotk_dat.spp"
call iotk_error_msg(ierr,'Kind incompatibility')
# 508 "iotk_dat.spp"
call iotk_error_write(ierr,"kind",rkind)
    end select
  else
    if(raw) then
      read(lunit,fmt=*,iostat=iostat) ( dat(i), i=1,ubound(dat,1) )
      if(iostat/=0) then
        call iotk_error_issue(ierr,"iotk_scan_dat_aux",__FILE__,__LINE__)
# 514 "iotk_dat.spp"
call iotk_error_msg(ierr,"CVS Revision: 1.17 ")
# 514 "iotk_dat.spp"
call iotk_error_msg(ierr,' ')
# 514 "iotk_dat.spp"
call iotk_error_write(ierr,"iostat",iostat)
        return
      end if
    else if(iotk_strcomp(fmt,"*")) then
      read(lunit,fmt=*,iostat=iostat) ( dat(i), i=1,ubound(dat,1) )
      if(iostat/=0) then
        call iotk_error_issue(ierr,"iotk_scan_dat_aux",__FILE__,__LINE__)
# 520 "iotk_dat.spp"
call iotk_error_msg(ierr,"CVS Revision: 1.17 ")
# 520 "iotk_dat.spp"
call iotk_error_msg(ierr,' ')
# 520 "iotk_dat.spp"
call iotk_error_write(ierr,"iostat",iostat)
        return
      end if
    else if(iotk_strcomp(fmt,"!")) then
      index = 0
      do
        call iotk_getline(lunit,line,length,ierr)
        if(ierr/=0) then
          call iotk_error_issue(ierr,"iotk_scan_dat_aux",__FILE__,__LINE__)
# 528 "iotk_dat.spp"
call iotk_error_msg(ierr,"CVS Revision: 1.17 ")
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
# 539 "iotk_dat.spp"
call iotk_error_msg(ierr,"CVS Revision: 1.17 ")
# 539 "iotk_dat.spp"
call iotk_error_msg(ierr,' ')
# 539 "iotk_dat.spp"
call iotk_error_write(ierr,"iostat",iostat)
            return
          end if
          call iotk_getline(lunit,altline,altlength,ierr)
          if(ierr/=0) then
            call iotk_error_issue(ierr,"iotk_scan_dat_aux",__FILE__,__LINE__)
# 544 "iotk_dat.spp"
call iotk_error_msg(ierr,"CVS Revision: 1.17 ")
            return
          end if
          backspace(lunit,iostat=iostat)
          if(iostat/=0) then
            call iotk_error_issue(ierr,"iotk_scan_dat_aux",__FILE__,__LINE__)
# 549 "iotk_dat.spp"
call iotk_error_msg(ierr,"CVS Revision: 1.17 ")
# 549 "iotk_dat.spp"
call iotk_error_msg(ierr,' ')
# 549 "iotk_dat.spp"
call iotk_error_write(ierr,"iostat",iostat)
            return
          end if
          read(lunit,"(a)",advance="no",iostat=iostat) altline(1:nexttag-1 + altlength - length)
          if(iostat/=0) then
            call iotk_error_issue(ierr,"iotk_scan_dat_aux",__FILE__,__LINE__)
# 554 "iotk_dat.spp"
call iotk_error_msg(ierr,"CVS Revision: 1.17 ")
# 554 "iotk_dat.spp"
call iotk_error_msg(ierr,' ')
# 554 "iotk_dat.spp"
call iotk_error_write(ierr,"iostat",iostat)
            return
          end if
        end if
        call iotk_str_clean(line(1:nexttag - 1))
        call iotk_read(dat,line(1:nexttag - 1),index,ierr)
        if(ierr/=0) then
          call iotk_error_issue(ierr,"iotk_scan_dat_aux",__FILE__,__LINE__)
# 561 "iotk_dat.spp"
call iotk_error_msg(ierr,"CVS Revision: 1.17 ")
          return
        end if
# 567 "iotk_dat.spp"
        if(index == size(dat)) exit
# 569 "iotk_dat.spp"
        if(nexttag/=length + 1) then
          call iotk_error_issue(ierr,"iotk_scan_dat_aux",__FILE__,__LINE__)
# 570 "iotk_dat.spp"
call iotk_error_msg(ierr,"CVS Revision: 1.17 ")
          return
        end if
      end do
    else
      read(lunit,fmt=fmt(1:iotk_strlen(fmt)),iostat=iostat) ( dat(i), i=1,ubound(dat,1) )
      if(iostat/=0) then
        call iotk_error_issue(ierr,"iotk_scan_dat_aux",__FILE__,__LINE__)
# 577 "iotk_dat.spp"
call iotk_error_msg(ierr,"CVS Revision: 1.17 ")
# 577 "iotk_dat.spp"
call iotk_error_msg(ierr,' ')
# 577 "iotk_dat.spp"
call iotk_error_write(ierr,"iostat",iostat)
        return
      end if
    end if
  end if
# 583 "iotk_dat.spp"
  if(idummy/=0) then
    call iotk_error_issue(ierr,"iotk_scan_dat_aux",__FILE__,__LINE__)
# 584 "iotk_dat.spp"
call iotk_error_msg(ierr,"CVS Revision: 1.17 ")
    return
  end if
end subroutine iotk_scan_dat_aux_INTEGER2
# 589 "iotk_dat.spp"

# 591 "iotk_dat.spp"
subroutine iotk_scan_dat_INTEGER2_1(unit,name,dat,dummy,attr,found,default,ierr)
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
  INTEGER (kind=__IOTK_INTEGER2)                        :: dat (:)
#else
  INTEGER (kind=__IOTK_INTEGER2),           intent(out) :: dat (:)
#endif
  type(iotk_dummytype), optional         :: dummy
#ifdef __IOTK_WORKAROUND6
  character(len=*),optional              :: attr
#else
  character(len=*),optional, intent(out) :: attr
#endif
  logical,         optional, intent(out) :: found
  INTEGER (kind=__IOTK_INTEGER2), optional, intent(in)  :: default (:)
  integer,         optional, intent(out) :: ierr
# 618 "iotk_dat.spp"
  INTEGER (kind=__IOTK_INTEGER2),              allocatable :: tmpdat(:)
# 620 "iotk_dat.spp"
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
# 637 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
    goto 1
  end if
  call iotk_parse_dat(lattr,rtype,rkind,rsize,rlen,fmt,columns,ierrl)
! Note that columns is not effectively used
  if(ierrl/=0) goto 1
  if(.not. (iotk_strcomp(rtype,iotk_eos) .or. iotk_strcomp(rtype,"INTEGER") ) ) then
    call iotk_error_issue(ierrl,"iotk_scan_dat",__FILE__,__LINE__)
# 644 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
# 644 "iotk_dat.spp"
call iotk_error_msg(ierrl,' ')
# 644 "iotk_dat.spp"
call iotk_error_write(ierrl,"rtype",rtype(1:iotk_strlen(rtype)))
# 644 "iotk_dat.spp"
call iotk_error_write(ierrl,"type","INTEGER")
    goto 1
  end if
  if(.not. (rsize==-1 .or. rsize==size(dat)) ) then
    call iotk_error_issue(ierrl,"iotk_scan_dat",__FILE__,__LINE__)
# 648 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
    goto 1
  end if
  if(rkind==-1) rkind = kind(dat)
# 655 "iotk_dat.spp"

  allocate(tmpdat(size(dat)))
  call iotk_scan_dat_aux(unit,tmpdat,rkind,rlen,fmt(1:iotk_strlen(fmt)),ierrl)
# 662 "iotk_dat.spp"
#ifdef __IOTK_WORKAROUND7
        call iotk_reshape_INTEGER2( size(dat),dat,tmpdat )
#else
        dat = reshape(tmpdat,shape(dat))
#endif
# 671 "iotk_dat.spp"
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
# 683 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
# 683 "iotk_dat.spp"
call iotk_error_msg(ierrl,'Dat not found')
# 683 "iotk_dat.spp"
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
end subroutine iotk_scan_dat_INTEGER2_1


#endif
#endif

subroutine iotk_dat_dummy_INTEGER2_1
  write(0,*)
end subroutine iotk_dat_dummy_INTEGER2_1


# 706 "iotk_dat.spp"
!
! the following workaround is introduced for INTEL compiler < 9.0 
! having problem in treating INTEGER arrays with large dimensions
! within implicit loops
!
#ifdef __IOTK_WORKAROUND7

subroutine iotk_reshape_INTEGER2(n,dat_out,dat_in)
  implicit none
  integer, intent(in) :: n
  INTEGER (kind=__IOTK_INTEGER2), intent(in)  :: dat_in(n)
  INTEGER (kind=__IOTK_INTEGER2), intent(out) :: dat_out(n)
  integer :: i
  do i=1,n
     dat_out(i) = dat_in(i)
  enddo
end subroutine iotk_reshape_INTEGER2

#endif

# 727 "iotk_dat.spp"


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

#ifdef __IOTK_INTEGER2
#if 2 <= __IOTK_MAXRANK
# 82 "iotk_dat.spp"
subroutine iotk_write_dat_INTEGER2_2(unit,name,dat,dummy,attr,columns,sep,fmt,ierr)
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
  INTEGER (kind=__IOTK_INTEGER2), intent(in)  :: dat (:,:) 
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
# 114 "iotk_dat.spp"
  INTEGER (kind=__IOTK_INTEGER2),allocatable :: dattmp(:)
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
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
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
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
    goto 1
  end if
  call iotk_strcpy(usefmt,"!",ierrl)
  if(ierrl/=0) then
    call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 142 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
    goto 1
  end if
  if(present(fmt) .and. .not. raw) call iotk_strcpy(usefmt,iotk_strtrim(fmt),ierrl)
  if(ierrl/=0) then
    call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 147 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
    goto 1
  end if
  if(iotk_strscan(usefmt,"<>&")/=0) then
    call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 151 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
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
  call iotk_write_attr(lattr,"type",iotk_tolower("INTEGER"),first=.true.,ierr=ierrl)
  if(ierrl/=0) then
    call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 156 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
    goto 1
  end if
  call iotk_write_attr(lattr,"size",size(dat),ierr=ierrl)
  if(ierrl/=0) then
    call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 161 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
    goto 1
  end if
# 171 "iotk_dat.spp"
  if(binary) then
    call iotk_write_attr(lattr,"kind",kind(dat),ierr=ierrl)
    if(ierrl/=0) then
      call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 174 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
      goto 1
    end if
  end if
# 179 "iotk_dat.spp"
  if(.not.iotk_strcomp(usefmt,"!")) call iotk_write_attr(lattr,"fmt",iotk_strtrim(usefmt),ierr=ierrl)
  if(ierrl/=0) then
    call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 181 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
    goto 1
  end if
  if(lcolumns/=1) call iotk_write_attr(lattr,"columns",lcolumns,ierr=ierrl)
  if(ierrl/=0) then
    call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 186 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
    goto 1
  end if
  if(present(attr)) then
    attr_tmp(1:1)=iotk_eos
    call iotk_strcpy(attr_tmp,attr,ierr=ierrl)
    if(ierrl/=0) then
      call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 193 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
      goto 1
    end if
    call iotk_delete_attr(attr_tmp,"type",ierrl)
    if(ierrl/=0) then
      call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 198 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
      goto 1
    end if
    call iotk_delete_attr(attr_tmp,"kind",ierrl)
    if(ierrl/=0) then
      call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 203 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
      goto 1
    end if
    call iotk_delete_attr(attr_tmp,"size",ierrl)
    if(ierrl/=0) then
      call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 208 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
      goto 1
    end if
    call iotk_delete_attr(attr_tmp,"fmt",ierrl)
    if(ierrl/=0) then
      call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 213 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
      goto 1
    end if
    call iotk_delete_attr(attr_tmp,"columns",ierrl)
    if(ierrl/=0) then
      call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 218 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
      goto 1
    end if
    call iotk_delete_attr(attr_tmp,"len",ierrl)
    if(ierrl/=0) then
      call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 223 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
      goto 1
    end if
    if(iotk_strlen_trim(attr_tmp)>0) call iotk_strcat(lattr,iotk_strtrim(attr_tmp),ierr=ierrl)
  end if
  if(ierrl/=0) then
    call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 229 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
    goto 1
  end if
  call iotk_write_begin(unit,name,lattr,ierr=ierrl)
  if(ierrl/=0) then
    call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 234 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
    goto 1
  end if

  allocate(dattmp(size(dat)))
# 242 "iotk_dat.spp"
#if defined(__IOTK_WORKAROUND3) || defined(__IOTK_WORKAROUND4)
# 246 "iotk_dat.spp"
     call iotk_private_pack_INTEGER2(dattmp,dat,size(dattmp),1)
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
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
        goto 1
      end if
    else
      write(lunit,iostat=iostat) idummy,(dattmp(itmp),itmp=1,size(dattmp))
      if(iostat/=0) then
        call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 263 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
        goto 1
      end if
    end if
  else
    if(raw) then
# 272 "iotk_dat.spp"
      write(lunit,*,iostat=iostat) (dattmp(itmp),itmp=1,size(dattmp))
# 274 "iotk_dat.spp"
      if(iostat/=0) then
        call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 275 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
        goto 1
      end if
    else if(iotk_strcomp(usefmt,"*")) then
      write(lunit,*,iostat=iostat) (dattmp(itmp),itmp=1,size(dattmp))
      if(iostat/=0) then
        call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 281 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
        goto 1
      end if
    else if(iotk_strcomp(usefmt,"!")) then
# 295 "iotk_dat.spp"
     write(lunit,fmt=trim(iotk_wfmt("INTEGER",kind(dattmp),lcolumns,-1,lsep)),iostat=iostat) (dattmp(itmp),itmp=1,size(dattmp))
     if(iostat/=0) then
      call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 297 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
      goto 1
     end if
# 301 "iotk_dat.spp"
    else
      write(lunit,fmt=usefmt(1:iotk_strlen(usefmt)),iostat=iostat) (dattmp(itmp),itmp=1,size(dattmp))
      if(iostat/=0) then
        call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 304 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
        goto 1
      end if
    end if
  end if
  call iotk_write_end(unit,name,ierr=ierrl)
  if(ierrl/=0) then
    call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 311 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
    goto 1
  end if
1 continue
  if(allocated(dattmp)) deallocate(dattmp)
  if(present(ierr)) then
    ierr = ierrl
  else
    if(ierrl/=0) call iotk_error_handler(ierrl)
  end if
end subroutine iotk_write_dat_INTEGER2_2


# 589 "iotk_dat.spp"

# 591 "iotk_dat.spp"
subroutine iotk_scan_dat_INTEGER2_2(unit,name,dat,dummy,attr,found,default,ierr)
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
  INTEGER (kind=__IOTK_INTEGER2)                        :: dat (:,:)
#else
  INTEGER (kind=__IOTK_INTEGER2),           intent(out) :: dat (:,:)
#endif
  type(iotk_dummytype), optional         :: dummy
#ifdef __IOTK_WORKAROUND6
  character(len=*),optional              :: attr
#else
  character(len=*),optional, intent(out) :: attr
#endif
  logical,         optional, intent(out) :: found
  INTEGER (kind=__IOTK_INTEGER2), optional, intent(in)  :: default (:,:)
  integer,         optional, intent(out) :: ierr
# 618 "iotk_dat.spp"
  INTEGER (kind=__IOTK_INTEGER2),              allocatable :: tmpdat(:)
# 620 "iotk_dat.spp"
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
# 637 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
    goto 1
  end if
  call iotk_parse_dat(lattr,rtype,rkind,rsize,rlen,fmt,columns,ierrl)
! Note that columns is not effectively used
  if(ierrl/=0) goto 1
  if(.not. (iotk_strcomp(rtype,iotk_eos) .or. iotk_strcomp(rtype,"INTEGER") ) ) then
    call iotk_error_issue(ierrl,"iotk_scan_dat",__FILE__,__LINE__)
# 644 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
# 644 "iotk_dat.spp"
call iotk_error_msg(ierrl,' ')
# 644 "iotk_dat.spp"
call iotk_error_write(ierrl,"rtype",rtype(1:iotk_strlen(rtype)))
# 644 "iotk_dat.spp"
call iotk_error_write(ierrl,"type","INTEGER")
    goto 1
  end if
  if(.not. (rsize==-1 .or. rsize==size(dat)) ) then
    call iotk_error_issue(ierrl,"iotk_scan_dat",__FILE__,__LINE__)
# 648 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
    goto 1
  end if
  if(rkind==-1) rkind = kind(dat)
# 655 "iotk_dat.spp"

  allocate(tmpdat(size(dat)))
  call iotk_scan_dat_aux(unit,tmpdat,rkind,rlen,fmt(1:iotk_strlen(fmt)),ierrl)
# 662 "iotk_dat.spp"
#ifdef __IOTK_WORKAROUND7
        call iotk_reshape_INTEGER2( size(dat),dat,tmpdat )
#else
        dat = reshape(tmpdat,shape(dat))
#endif
# 671 "iotk_dat.spp"
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
# 683 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
# 683 "iotk_dat.spp"
call iotk_error_msg(ierrl,'Dat not found')
# 683 "iotk_dat.spp"
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
end subroutine iotk_scan_dat_INTEGER2_2


#endif
#endif

subroutine iotk_dat_dummy_INTEGER2_2
  write(0,*)
end subroutine iotk_dat_dummy_INTEGER2_2


# 727 "iotk_dat.spp"


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

#ifdef __IOTK_INTEGER2
#if 3 <= __IOTK_MAXRANK
# 82 "iotk_dat.spp"
subroutine iotk_write_dat_INTEGER2_3(unit,name,dat,dummy,attr,columns,sep,fmt,ierr)
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
  INTEGER (kind=__IOTK_INTEGER2), intent(in)  :: dat (:,:,:) 
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
# 114 "iotk_dat.spp"
  INTEGER (kind=__IOTK_INTEGER2),allocatable :: dattmp(:)
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
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
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
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
    goto 1
  end if
  call iotk_strcpy(usefmt,"!",ierrl)
  if(ierrl/=0) then
    call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 142 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
    goto 1
  end if
  if(present(fmt) .and. .not. raw) call iotk_strcpy(usefmt,iotk_strtrim(fmt),ierrl)
  if(ierrl/=0) then
    call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 147 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
    goto 1
  end if
  if(iotk_strscan(usefmt,"<>&")/=0) then
    call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 151 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
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
  call iotk_write_attr(lattr,"type",iotk_tolower("INTEGER"),first=.true.,ierr=ierrl)
  if(ierrl/=0) then
    call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 156 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
    goto 1
  end if
  call iotk_write_attr(lattr,"size",size(dat),ierr=ierrl)
  if(ierrl/=0) then
    call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 161 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
    goto 1
  end if
# 171 "iotk_dat.spp"
  if(binary) then
    call iotk_write_attr(lattr,"kind",kind(dat),ierr=ierrl)
    if(ierrl/=0) then
      call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 174 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
      goto 1
    end if
  end if
# 179 "iotk_dat.spp"
  if(.not.iotk_strcomp(usefmt,"!")) call iotk_write_attr(lattr,"fmt",iotk_strtrim(usefmt),ierr=ierrl)
  if(ierrl/=0) then
    call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 181 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
    goto 1
  end if
  if(lcolumns/=1) call iotk_write_attr(lattr,"columns",lcolumns,ierr=ierrl)
  if(ierrl/=0) then
    call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 186 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
    goto 1
  end if
  if(present(attr)) then
    attr_tmp(1:1)=iotk_eos
    call iotk_strcpy(attr_tmp,attr,ierr=ierrl)
    if(ierrl/=0) then
      call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 193 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
      goto 1
    end if
    call iotk_delete_attr(attr_tmp,"type",ierrl)
    if(ierrl/=0) then
      call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 198 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
      goto 1
    end if
    call iotk_delete_attr(attr_tmp,"kind",ierrl)
    if(ierrl/=0) then
      call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 203 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
      goto 1
    end if
    call iotk_delete_attr(attr_tmp,"size",ierrl)
    if(ierrl/=0) then
      call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 208 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
      goto 1
    end if
    call iotk_delete_attr(attr_tmp,"fmt",ierrl)
    if(ierrl/=0) then
      call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 213 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
      goto 1
    end if
    call iotk_delete_attr(attr_tmp,"columns",ierrl)
    if(ierrl/=0) then
      call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 218 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
      goto 1
    end if
    call iotk_delete_attr(attr_tmp,"len",ierrl)
    if(ierrl/=0) then
      call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 223 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
      goto 1
    end if
    if(iotk_strlen_trim(attr_tmp)>0) call iotk_strcat(lattr,iotk_strtrim(attr_tmp),ierr=ierrl)
  end if
  if(ierrl/=0) then
    call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 229 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
    goto 1
  end if
  call iotk_write_begin(unit,name,lattr,ierr=ierrl)
  if(ierrl/=0) then
    call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 234 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
    goto 1
  end if

  allocate(dattmp(size(dat)))
# 242 "iotk_dat.spp"
#if defined(__IOTK_WORKAROUND3) || defined(__IOTK_WORKAROUND4)
# 246 "iotk_dat.spp"
     call iotk_private_pack_INTEGER2(dattmp,dat,size(dattmp),1)
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
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
        goto 1
      end if
    else
      write(lunit,iostat=iostat) idummy,(dattmp(itmp),itmp=1,size(dattmp))
      if(iostat/=0) then
        call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 263 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
        goto 1
      end if
    end if
  else
    if(raw) then
# 272 "iotk_dat.spp"
      write(lunit,*,iostat=iostat) (dattmp(itmp),itmp=1,size(dattmp))
# 274 "iotk_dat.spp"
      if(iostat/=0) then
        call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 275 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
        goto 1
      end if
    else if(iotk_strcomp(usefmt,"*")) then
      write(lunit,*,iostat=iostat) (dattmp(itmp),itmp=1,size(dattmp))
      if(iostat/=0) then
        call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 281 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
        goto 1
      end if
    else if(iotk_strcomp(usefmt,"!")) then
# 295 "iotk_dat.spp"
     write(lunit,fmt=trim(iotk_wfmt("INTEGER",kind(dattmp),lcolumns,-1,lsep)),iostat=iostat) (dattmp(itmp),itmp=1,size(dattmp))
     if(iostat/=0) then
      call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 297 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
      goto 1
     end if
# 301 "iotk_dat.spp"
    else
      write(lunit,fmt=usefmt(1:iotk_strlen(usefmt)),iostat=iostat) (dattmp(itmp),itmp=1,size(dattmp))
      if(iostat/=0) then
        call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 304 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
        goto 1
      end if
    end if
  end if
  call iotk_write_end(unit,name,ierr=ierrl)
  if(ierrl/=0) then
    call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 311 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
    goto 1
  end if
1 continue
  if(allocated(dattmp)) deallocate(dattmp)
  if(present(ierr)) then
    ierr = ierrl
  else
    if(ierrl/=0) call iotk_error_handler(ierrl)
  end if
end subroutine iotk_write_dat_INTEGER2_3


# 589 "iotk_dat.spp"

# 591 "iotk_dat.spp"
subroutine iotk_scan_dat_INTEGER2_3(unit,name,dat,dummy,attr,found,default,ierr)
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
  INTEGER (kind=__IOTK_INTEGER2)                        :: dat (:,:,:)
#else
  INTEGER (kind=__IOTK_INTEGER2),           intent(out) :: dat (:,:,:)
#endif
  type(iotk_dummytype), optional         :: dummy
#ifdef __IOTK_WORKAROUND6
  character(len=*),optional              :: attr
#else
  character(len=*),optional, intent(out) :: attr
#endif
  logical,         optional, intent(out) :: found
  INTEGER (kind=__IOTK_INTEGER2), optional, intent(in)  :: default (:,:,:)
  integer,         optional, intent(out) :: ierr
# 618 "iotk_dat.spp"
  INTEGER (kind=__IOTK_INTEGER2),              allocatable :: tmpdat(:)
# 620 "iotk_dat.spp"
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
# 637 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
    goto 1
  end if
  call iotk_parse_dat(lattr,rtype,rkind,rsize,rlen,fmt,columns,ierrl)
! Note that columns is not effectively used
  if(ierrl/=0) goto 1
  if(.not. (iotk_strcomp(rtype,iotk_eos) .or. iotk_strcomp(rtype,"INTEGER") ) ) then
    call iotk_error_issue(ierrl,"iotk_scan_dat",__FILE__,__LINE__)
# 644 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
# 644 "iotk_dat.spp"
call iotk_error_msg(ierrl,' ')
# 644 "iotk_dat.spp"
call iotk_error_write(ierrl,"rtype",rtype(1:iotk_strlen(rtype)))
# 644 "iotk_dat.spp"
call iotk_error_write(ierrl,"type","INTEGER")
    goto 1
  end if
  if(.not. (rsize==-1 .or. rsize==size(dat)) ) then
    call iotk_error_issue(ierrl,"iotk_scan_dat",__FILE__,__LINE__)
# 648 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
    goto 1
  end if
  if(rkind==-1) rkind = kind(dat)
# 655 "iotk_dat.spp"

  allocate(tmpdat(size(dat)))
  call iotk_scan_dat_aux(unit,tmpdat,rkind,rlen,fmt(1:iotk_strlen(fmt)),ierrl)
# 662 "iotk_dat.spp"
#ifdef __IOTK_WORKAROUND7
        call iotk_reshape_INTEGER2( size(dat),dat,tmpdat )
#else
        dat = reshape(tmpdat,shape(dat))
#endif
# 671 "iotk_dat.spp"
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
# 683 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
# 683 "iotk_dat.spp"
call iotk_error_msg(ierrl,'Dat not found')
# 683 "iotk_dat.spp"
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
end subroutine iotk_scan_dat_INTEGER2_3


#endif
#endif

subroutine iotk_dat_dummy_INTEGER2_3
  write(0,*)
end subroutine iotk_dat_dummy_INTEGER2_3


# 727 "iotk_dat.spp"


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

#ifdef __IOTK_INTEGER2
#if 4 <= __IOTK_MAXRANK
# 82 "iotk_dat.spp"
subroutine iotk_write_dat_INTEGER2_4(unit,name,dat,dummy,attr,columns,sep,fmt,ierr)
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
  INTEGER (kind=__IOTK_INTEGER2), intent(in)  :: dat (:,:,:,:) 
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
# 114 "iotk_dat.spp"
  INTEGER (kind=__IOTK_INTEGER2),allocatable :: dattmp(:)
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
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
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
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
    goto 1
  end if
  call iotk_strcpy(usefmt,"!",ierrl)
  if(ierrl/=0) then
    call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 142 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
    goto 1
  end if
  if(present(fmt) .and. .not. raw) call iotk_strcpy(usefmt,iotk_strtrim(fmt),ierrl)
  if(ierrl/=0) then
    call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 147 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
    goto 1
  end if
  if(iotk_strscan(usefmt,"<>&")/=0) then
    call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 151 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
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
  call iotk_write_attr(lattr,"type",iotk_tolower("INTEGER"),first=.true.,ierr=ierrl)
  if(ierrl/=0) then
    call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 156 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
    goto 1
  end if
  call iotk_write_attr(lattr,"size",size(dat),ierr=ierrl)
  if(ierrl/=0) then
    call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 161 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
    goto 1
  end if
# 171 "iotk_dat.spp"
  if(binary) then
    call iotk_write_attr(lattr,"kind",kind(dat),ierr=ierrl)
    if(ierrl/=0) then
      call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 174 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
      goto 1
    end if
  end if
# 179 "iotk_dat.spp"
  if(.not.iotk_strcomp(usefmt,"!")) call iotk_write_attr(lattr,"fmt",iotk_strtrim(usefmt),ierr=ierrl)
  if(ierrl/=0) then
    call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 181 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
    goto 1
  end if
  if(lcolumns/=1) call iotk_write_attr(lattr,"columns",lcolumns,ierr=ierrl)
  if(ierrl/=0) then
    call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 186 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
    goto 1
  end if
  if(present(attr)) then
    attr_tmp(1:1)=iotk_eos
    call iotk_strcpy(attr_tmp,attr,ierr=ierrl)
    if(ierrl/=0) then
      call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 193 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
      goto 1
    end if
    call iotk_delete_attr(attr_tmp,"type",ierrl)
    if(ierrl/=0) then
      call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 198 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
      goto 1
    end if
    call iotk_delete_attr(attr_tmp,"kind",ierrl)
    if(ierrl/=0) then
      call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 203 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
      goto 1
    end if
    call iotk_delete_attr(attr_tmp,"size",ierrl)
    if(ierrl/=0) then
      call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 208 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
      goto 1
    end if
    call iotk_delete_attr(attr_tmp,"fmt",ierrl)
    if(ierrl/=0) then
      call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 213 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
      goto 1
    end if
    call iotk_delete_attr(attr_tmp,"columns",ierrl)
    if(ierrl/=0) then
      call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 218 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
      goto 1
    end if
    call iotk_delete_attr(attr_tmp,"len",ierrl)
    if(ierrl/=0) then
      call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 223 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
      goto 1
    end if
    if(iotk_strlen_trim(attr_tmp)>0) call iotk_strcat(lattr,iotk_strtrim(attr_tmp),ierr=ierrl)
  end if
  if(ierrl/=0) then
    call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 229 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
    goto 1
  end if
  call iotk_write_begin(unit,name,lattr,ierr=ierrl)
  if(ierrl/=0) then
    call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 234 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
    goto 1
  end if

  allocate(dattmp(size(dat)))
# 242 "iotk_dat.spp"
#if defined(__IOTK_WORKAROUND3) || defined(__IOTK_WORKAROUND4)
# 246 "iotk_dat.spp"
     call iotk_private_pack_INTEGER2(dattmp,dat,size(dattmp),1)
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
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
        goto 1
      end if
    else
      write(lunit,iostat=iostat) idummy,(dattmp(itmp),itmp=1,size(dattmp))
      if(iostat/=0) then
        call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 263 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
        goto 1
      end if
    end if
  else
    if(raw) then
# 272 "iotk_dat.spp"
      write(lunit,*,iostat=iostat) (dattmp(itmp),itmp=1,size(dattmp))
# 274 "iotk_dat.spp"
      if(iostat/=0) then
        call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 275 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
        goto 1
      end if
    else if(iotk_strcomp(usefmt,"*")) then
      write(lunit,*,iostat=iostat) (dattmp(itmp),itmp=1,size(dattmp))
      if(iostat/=0) then
        call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 281 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
        goto 1
      end if
    else if(iotk_strcomp(usefmt,"!")) then
# 295 "iotk_dat.spp"
     write(lunit,fmt=trim(iotk_wfmt("INTEGER",kind(dattmp),lcolumns,-1,lsep)),iostat=iostat) (dattmp(itmp),itmp=1,size(dattmp))
     if(iostat/=0) then
      call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 297 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
      goto 1
     end if
# 301 "iotk_dat.spp"
    else
      write(lunit,fmt=usefmt(1:iotk_strlen(usefmt)),iostat=iostat) (dattmp(itmp),itmp=1,size(dattmp))
      if(iostat/=0) then
        call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 304 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
        goto 1
      end if
    end if
  end if
  call iotk_write_end(unit,name,ierr=ierrl)
  if(ierrl/=0) then
    call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 311 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
    goto 1
  end if
1 continue
  if(allocated(dattmp)) deallocate(dattmp)
  if(present(ierr)) then
    ierr = ierrl
  else
    if(ierrl/=0) call iotk_error_handler(ierrl)
  end if
end subroutine iotk_write_dat_INTEGER2_4


# 589 "iotk_dat.spp"

# 591 "iotk_dat.spp"
subroutine iotk_scan_dat_INTEGER2_4(unit,name,dat,dummy,attr,found,default,ierr)
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
  INTEGER (kind=__IOTK_INTEGER2)                        :: dat (:,:,:,:)
#else
  INTEGER (kind=__IOTK_INTEGER2),           intent(out) :: dat (:,:,:,:)
#endif
  type(iotk_dummytype), optional         :: dummy
#ifdef __IOTK_WORKAROUND6
  character(len=*),optional              :: attr
#else
  character(len=*),optional, intent(out) :: attr
#endif
  logical,         optional, intent(out) :: found
  INTEGER (kind=__IOTK_INTEGER2), optional, intent(in)  :: default (:,:,:,:)
  integer,         optional, intent(out) :: ierr
# 618 "iotk_dat.spp"
  INTEGER (kind=__IOTK_INTEGER2),              allocatable :: tmpdat(:)
# 620 "iotk_dat.spp"
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
# 637 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
    goto 1
  end if
  call iotk_parse_dat(lattr,rtype,rkind,rsize,rlen,fmt,columns,ierrl)
! Note that columns is not effectively used
  if(ierrl/=0) goto 1
  if(.not. (iotk_strcomp(rtype,iotk_eos) .or. iotk_strcomp(rtype,"INTEGER") ) ) then
    call iotk_error_issue(ierrl,"iotk_scan_dat",__FILE__,__LINE__)
# 644 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
# 644 "iotk_dat.spp"
call iotk_error_msg(ierrl,' ')
# 644 "iotk_dat.spp"
call iotk_error_write(ierrl,"rtype",rtype(1:iotk_strlen(rtype)))
# 644 "iotk_dat.spp"
call iotk_error_write(ierrl,"type","INTEGER")
    goto 1
  end if
  if(.not. (rsize==-1 .or. rsize==size(dat)) ) then
    call iotk_error_issue(ierrl,"iotk_scan_dat",__FILE__,__LINE__)
# 648 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
    goto 1
  end if
  if(rkind==-1) rkind = kind(dat)
# 655 "iotk_dat.spp"

  allocate(tmpdat(size(dat)))
  call iotk_scan_dat_aux(unit,tmpdat,rkind,rlen,fmt(1:iotk_strlen(fmt)),ierrl)
# 662 "iotk_dat.spp"
#ifdef __IOTK_WORKAROUND7
        call iotk_reshape_INTEGER2( size(dat),dat,tmpdat )
#else
        dat = reshape(tmpdat,shape(dat))
#endif
# 671 "iotk_dat.spp"
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
# 683 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
# 683 "iotk_dat.spp"
call iotk_error_msg(ierrl,'Dat not found')
# 683 "iotk_dat.spp"
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
end subroutine iotk_scan_dat_INTEGER2_4


#endif
#endif

subroutine iotk_dat_dummy_INTEGER2_4
  write(0,*)
end subroutine iotk_dat_dummy_INTEGER2_4


# 727 "iotk_dat.spp"


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

#ifdef __IOTK_INTEGER2
#if 5 <= __IOTK_MAXRANK
# 82 "iotk_dat.spp"
subroutine iotk_write_dat_INTEGER2_5(unit,name,dat,dummy,attr,columns,sep,fmt,ierr)
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
  INTEGER (kind=__IOTK_INTEGER2), intent(in)  :: dat (:,:,:,:,:) 
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
# 114 "iotk_dat.spp"
  INTEGER (kind=__IOTK_INTEGER2),allocatable :: dattmp(:)
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
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
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
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
    goto 1
  end if
  call iotk_strcpy(usefmt,"!",ierrl)
  if(ierrl/=0) then
    call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 142 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
    goto 1
  end if
  if(present(fmt) .and. .not. raw) call iotk_strcpy(usefmt,iotk_strtrim(fmt),ierrl)
  if(ierrl/=0) then
    call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 147 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
    goto 1
  end if
  if(iotk_strscan(usefmt,"<>&")/=0) then
    call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 151 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
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
  call iotk_write_attr(lattr,"type",iotk_tolower("INTEGER"),first=.true.,ierr=ierrl)
  if(ierrl/=0) then
    call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 156 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
    goto 1
  end if
  call iotk_write_attr(lattr,"size",size(dat),ierr=ierrl)
  if(ierrl/=0) then
    call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 161 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
    goto 1
  end if
# 171 "iotk_dat.spp"
  if(binary) then
    call iotk_write_attr(lattr,"kind",kind(dat),ierr=ierrl)
    if(ierrl/=0) then
      call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 174 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
      goto 1
    end if
  end if
# 179 "iotk_dat.spp"
  if(.not.iotk_strcomp(usefmt,"!")) call iotk_write_attr(lattr,"fmt",iotk_strtrim(usefmt),ierr=ierrl)
  if(ierrl/=0) then
    call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 181 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
    goto 1
  end if
  if(lcolumns/=1) call iotk_write_attr(lattr,"columns",lcolumns,ierr=ierrl)
  if(ierrl/=0) then
    call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 186 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
    goto 1
  end if
  if(present(attr)) then
    attr_tmp(1:1)=iotk_eos
    call iotk_strcpy(attr_tmp,attr,ierr=ierrl)
    if(ierrl/=0) then
      call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 193 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
      goto 1
    end if
    call iotk_delete_attr(attr_tmp,"type",ierrl)
    if(ierrl/=0) then
      call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 198 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
      goto 1
    end if
    call iotk_delete_attr(attr_tmp,"kind",ierrl)
    if(ierrl/=0) then
      call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 203 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
      goto 1
    end if
    call iotk_delete_attr(attr_tmp,"size",ierrl)
    if(ierrl/=0) then
      call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 208 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
      goto 1
    end if
    call iotk_delete_attr(attr_tmp,"fmt",ierrl)
    if(ierrl/=0) then
      call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 213 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
      goto 1
    end if
    call iotk_delete_attr(attr_tmp,"columns",ierrl)
    if(ierrl/=0) then
      call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 218 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
      goto 1
    end if
    call iotk_delete_attr(attr_tmp,"len",ierrl)
    if(ierrl/=0) then
      call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 223 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
      goto 1
    end if
    if(iotk_strlen_trim(attr_tmp)>0) call iotk_strcat(lattr,iotk_strtrim(attr_tmp),ierr=ierrl)
  end if
  if(ierrl/=0) then
    call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 229 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
    goto 1
  end if
  call iotk_write_begin(unit,name,lattr,ierr=ierrl)
  if(ierrl/=0) then
    call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 234 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
    goto 1
  end if

  allocate(dattmp(size(dat)))
# 242 "iotk_dat.spp"
#if defined(__IOTK_WORKAROUND3) || defined(__IOTK_WORKAROUND4)
# 246 "iotk_dat.spp"
     call iotk_private_pack_INTEGER2(dattmp,dat,size(dattmp),1)
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
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
        goto 1
      end if
    else
      write(lunit,iostat=iostat) idummy,(dattmp(itmp),itmp=1,size(dattmp))
      if(iostat/=0) then
        call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 263 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
        goto 1
      end if
    end if
  else
    if(raw) then
# 272 "iotk_dat.spp"
      write(lunit,*,iostat=iostat) (dattmp(itmp),itmp=1,size(dattmp))
# 274 "iotk_dat.spp"
      if(iostat/=0) then
        call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 275 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
        goto 1
      end if
    else if(iotk_strcomp(usefmt,"*")) then
      write(lunit,*,iostat=iostat) (dattmp(itmp),itmp=1,size(dattmp))
      if(iostat/=0) then
        call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 281 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
        goto 1
      end if
    else if(iotk_strcomp(usefmt,"!")) then
# 295 "iotk_dat.spp"
     write(lunit,fmt=trim(iotk_wfmt("INTEGER",kind(dattmp),lcolumns,-1,lsep)),iostat=iostat) (dattmp(itmp),itmp=1,size(dattmp))
     if(iostat/=0) then
      call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 297 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
      goto 1
     end if
# 301 "iotk_dat.spp"
    else
      write(lunit,fmt=usefmt(1:iotk_strlen(usefmt)),iostat=iostat) (dattmp(itmp),itmp=1,size(dattmp))
      if(iostat/=0) then
        call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 304 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
        goto 1
      end if
    end if
  end if
  call iotk_write_end(unit,name,ierr=ierrl)
  if(ierrl/=0) then
    call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 311 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
    goto 1
  end if
1 continue
  if(allocated(dattmp)) deallocate(dattmp)
  if(present(ierr)) then
    ierr = ierrl
  else
    if(ierrl/=0) call iotk_error_handler(ierrl)
  end if
end subroutine iotk_write_dat_INTEGER2_5


# 589 "iotk_dat.spp"

# 591 "iotk_dat.spp"
subroutine iotk_scan_dat_INTEGER2_5(unit,name,dat,dummy,attr,found,default,ierr)
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
  INTEGER (kind=__IOTK_INTEGER2)                        :: dat (:,:,:,:,:)
#else
  INTEGER (kind=__IOTK_INTEGER2),           intent(out) :: dat (:,:,:,:,:)
#endif
  type(iotk_dummytype), optional         :: dummy
#ifdef __IOTK_WORKAROUND6
  character(len=*),optional              :: attr
#else
  character(len=*),optional, intent(out) :: attr
#endif
  logical,         optional, intent(out) :: found
  INTEGER (kind=__IOTK_INTEGER2), optional, intent(in)  :: default (:,:,:,:,:)
  integer,         optional, intent(out) :: ierr
# 618 "iotk_dat.spp"
  INTEGER (kind=__IOTK_INTEGER2),              allocatable :: tmpdat(:)
# 620 "iotk_dat.spp"
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
# 637 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
    goto 1
  end if
  call iotk_parse_dat(lattr,rtype,rkind,rsize,rlen,fmt,columns,ierrl)
! Note that columns is not effectively used
  if(ierrl/=0) goto 1
  if(.not. (iotk_strcomp(rtype,iotk_eos) .or. iotk_strcomp(rtype,"INTEGER") ) ) then
    call iotk_error_issue(ierrl,"iotk_scan_dat",__FILE__,__LINE__)
# 644 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
# 644 "iotk_dat.spp"
call iotk_error_msg(ierrl,' ')
# 644 "iotk_dat.spp"
call iotk_error_write(ierrl,"rtype",rtype(1:iotk_strlen(rtype)))
# 644 "iotk_dat.spp"
call iotk_error_write(ierrl,"type","INTEGER")
    goto 1
  end if
  if(.not. (rsize==-1 .or. rsize==size(dat)) ) then
    call iotk_error_issue(ierrl,"iotk_scan_dat",__FILE__,__LINE__)
# 648 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
    goto 1
  end if
  if(rkind==-1) rkind = kind(dat)
# 655 "iotk_dat.spp"

  allocate(tmpdat(size(dat)))
  call iotk_scan_dat_aux(unit,tmpdat,rkind,rlen,fmt(1:iotk_strlen(fmt)),ierrl)
# 662 "iotk_dat.spp"
#ifdef __IOTK_WORKAROUND7
        call iotk_reshape_INTEGER2( size(dat),dat,tmpdat )
#else
        dat = reshape(tmpdat,shape(dat))
#endif
# 671 "iotk_dat.spp"
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
# 683 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
# 683 "iotk_dat.spp"
call iotk_error_msg(ierrl,'Dat not found')
# 683 "iotk_dat.spp"
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
end subroutine iotk_scan_dat_INTEGER2_5


#endif
#endif

subroutine iotk_dat_dummy_INTEGER2_5
  write(0,*)
end subroutine iotk_dat_dummy_INTEGER2_5


# 727 "iotk_dat.spp"


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

#ifdef __IOTK_INTEGER2
#if 6 <= __IOTK_MAXRANK
# 82 "iotk_dat.spp"
subroutine iotk_write_dat_INTEGER2_6(unit,name,dat,dummy,attr,columns,sep,fmt,ierr)
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
  INTEGER (kind=__IOTK_INTEGER2), intent(in)  :: dat (:,:,:,:,:,:) 
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
# 114 "iotk_dat.spp"
  INTEGER (kind=__IOTK_INTEGER2),allocatable :: dattmp(:)
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
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
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
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
    goto 1
  end if
  call iotk_strcpy(usefmt,"!",ierrl)
  if(ierrl/=0) then
    call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 142 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
    goto 1
  end if
  if(present(fmt) .and. .not. raw) call iotk_strcpy(usefmt,iotk_strtrim(fmt),ierrl)
  if(ierrl/=0) then
    call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 147 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
    goto 1
  end if
  if(iotk_strscan(usefmt,"<>&")/=0) then
    call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 151 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
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
  call iotk_write_attr(lattr,"type",iotk_tolower("INTEGER"),first=.true.,ierr=ierrl)
  if(ierrl/=0) then
    call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 156 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
    goto 1
  end if
  call iotk_write_attr(lattr,"size",size(dat),ierr=ierrl)
  if(ierrl/=0) then
    call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 161 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
    goto 1
  end if
# 171 "iotk_dat.spp"
  if(binary) then
    call iotk_write_attr(lattr,"kind",kind(dat),ierr=ierrl)
    if(ierrl/=0) then
      call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 174 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
      goto 1
    end if
  end if
# 179 "iotk_dat.spp"
  if(.not.iotk_strcomp(usefmt,"!")) call iotk_write_attr(lattr,"fmt",iotk_strtrim(usefmt),ierr=ierrl)
  if(ierrl/=0) then
    call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 181 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
    goto 1
  end if
  if(lcolumns/=1) call iotk_write_attr(lattr,"columns",lcolumns,ierr=ierrl)
  if(ierrl/=0) then
    call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 186 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
    goto 1
  end if
  if(present(attr)) then
    attr_tmp(1:1)=iotk_eos
    call iotk_strcpy(attr_tmp,attr,ierr=ierrl)
    if(ierrl/=0) then
      call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 193 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
      goto 1
    end if
    call iotk_delete_attr(attr_tmp,"type",ierrl)
    if(ierrl/=0) then
      call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 198 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
      goto 1
    end if
    call iotk_delete_attr(attr_tmp,"kind",ierrl)
    if(ierrl/=0) then
      call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 203 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
      goto 1
    end if
    call iotk_delete_attr(attr_tmp,"size",ierrl)
    if(ierrl/=0) then
      call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 208 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
      goto 1
    end if
    call iotk_delete_attr(attr_tmp,"fmt",ierrl)
    if(ierrl/=0) then
      call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 213 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
      goto 1
    end if
    call iotk_delete_attr(attr_tmp,"columns",ierrl)
    if(ierrl/=0) then
      call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 218 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
      goto 1
    end if
    call iotk_delete_attr(attr_tmp,"len",ierrl)
    if(ierrl/=0) then
      call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 223 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
      goto 1
    end if
    if(iotk_strlen_trim(attr_tmp)>0) call iotk_strcat(lattr,iotk_strtrim(attr_tmp),ierr=ierrl)
  end if
  if(ierrl/=0) then
    call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 229 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
    goto 1
  end if
  call iotk_write_begin(unit,name,lattr,ierr=ierrl)
  if(ierrl/=0) then
    call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 234 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
    goto 1
  end if

  allocate(dattmp(size(dat)))
# 242 "iotk_dat.spp"
#if defined(__IOTK_WORKAROUND3) || defined(__IOTK_WORKAROUND4)
# 246 "iotk_dat.spp"
     call iotk_private_pack_INTEGER2(dattmp,dat,size(dattmp),1)
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
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
        goto 1
      end if
    else
      write(lunit,iostat=iostat) idummy,(dattmp(itmp),itmp=1,size(dattmp))
      if(iostat/=0) then
        call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 263 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
        goto 1
      end if
    end if
  else
    if(raw) then
# 272 "iotk_dat.spp"
      write(lunit,*,iostat=iostat) (dattmp(itmp),itmp=1,size(dattmp))
# 274 "iotk_dat.spp"
      if(iostat/=0) then
        call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 275 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
        goto 1
      end if
    else if(iotk_strcomp(usefmt,"*")) then
      write(lunit,*,iostat=iostat) (dattmp(itmp),itmp=1,size(dattmp))
      if(iostat/=0) then
        call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 281 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
        goto 1
      end if
    else if(iotk_strcomp(usefmt,"!")) then
# 295 "iotk_dat.spp"
     write(lunit,fmt=trim(iotk_wfmt("INTEGER",kind(dattmp),lcolumns,-1,lsep)),iostat=iostat) (dattmp(itmp),itmp=1,size(dattmp))
     if(iostat/=0) then
      call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 297 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
      goto 1
     end if
# 301 "iotk_dat.spp"
    else
      write(lunit,fmt=usefmt(1:iotk_strlen(usefmt)),iostat=iostat) (dattmp(itmp),itmp=1,size(dattmp))
      if(iostat/=0) then
        call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 304 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
        goto 1
      end if
    end if
  end if
  call iotk_write_end(unit,name,ierr=ierrl)
  if(ierrl/=0) then
    call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 311 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
    goto 1
  end if
1 continue
  if(allocated(dattmp)) deallocate(dattmp)
  if(present(ierr)) then
    ierr = ierrl
  else
    if(ierrl/=0) call iotk_error_handler(ierrl)
  end if
end subroutine iotk_write_dat_INTEGER2_6


# 589 "iotk_dat.spp"

# 591 "iotk_dat.spp"
subroutine iotk_scan_dat_INTEGER2_6(unit,name,dat,dummy,attr,found,default,ierr)
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
  INTEGER (kind=__IOTK_INTEGER2)                        :: dat (:,:,:,:,:,:)
#else
  INTEGER (kind=__IOTK_INTEGER2),           intent(out) :: dat (:,:,:,:,:,:)
#endif
  type(iotk_dummytype), optional         :: dummy
#ifdef __IOTK_WORKAROUND6
  character(len=*),optional              :: attr
#else
  character(len=*),optional, intent(out) :: attr
#endif
  logical,         optional, intent(out) :: found
  INTEGER (kind=__IOTK_INTEGER2), optional, intent(in)  :: default (:,:,:,:,:,:)
  integer,         optional, intent(out) :: ierr
# 618 "iotk_dat.spp"
  INTEGER (kind=__IOTK_INTEGER2),              allocatable :: tmpdat(:)
# 620 "iotk_dat.spp"
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
# 637 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
    goto 1
  end if
  call iotk_parse_dat(lattr,rtype,rkind,rsize,rlen,fmt,columns,ierrl)
! Note that columns is not effectively used
  if(ierrl/=0) goto 1
  if(.not. (iotk_strcomp(rtype,iotk_eos) .or. iotk_strcomp(rtype,"INTEGER") ) ) then
    call iotk_error_issue(ierrl,"iotk_scan_dat",__FILE__,__LINE__)
# 644 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
# 644 "iotk_dat.spp"
call iotk_error_msg(ierrl,' ')
# 644 "iotk_dat.spp"
call iotk_error_write(ierrl,"rtype",rtype(1:iotk_strlen(rtype)))
# 644 "iotk_dat.spp"
call iotk_error_write(ierrl,"type","INTEGER")
    goto 1
  end if
  if(.not. (rsize==-1 .or. rsize==size(dat)) ) then
    call iotk_error_issue(ierrl,"iotk_scan_dat",__FILE__,__LINE__)
# 648 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
    goto 1
  end if
  if(rkind==-1) rkind = kind(dat)
# 655 "iotk_dat.spp"

  allocate(tmpdat(size(dat)))
  call iotk_scan_dat_aux(unit,tmpdat,rkind,rlen,fmt(1:iotk_strlen(fmt)),ierrl)
# 662 "iotk_dat.spp"
#ifdef __IOTK_WORKAROUND7
        call iotk_reshape_INTEGER2( size(dat),dat,tmpdat )
#else
        dat = reshape(tmpdat,shape(dat))
#endif
# 671 "iotk_dat.spp"
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
# 683 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
# 683 "iotk_dat.spp"
call iotk_error_msg(ierrl,'Dat not found')
# 683 "iotk_dat.spp"
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
end subroutine iotk_scan_dat_INTEGER2_6


#endif
#endif

subroutine iotk_dat_dummy_INTEGER2_6
  write(0,*)
end subroutine iotk_dat_dummy_INTEGER2_6


# 727 "iotk_dat.spp"


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

#ifdef __IOTK_INTEGER2
#if 7 <= __IOTK_MAXRANK
# 82 "iotk_dat.spp"
subroutine iotk_write_dat_INTEGER2_7(unit,name,dat,dummy,attr,columns,sep,fmt,ierr)
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
  INTEGER (kind=__IOTK_INTEGER2), intent(in)  :: dat (:,:,:,:,:,:,:) 
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
# 114 "iotk_dat.spp"
  INTEGER (kind=__IOTK_INTEGER2),allocatable :: dattmp(:)
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
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
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
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
    goto 1
  end if
  call iotk_strcpy(usefmt,"!",ierrl)
  if(ierrl/=0) then
    call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 142 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
    goto 1
  end if
  if(present(fmt) .and. .not. raw) call iotk_strcpy(usefmt,iotk_strtrim(fmt),ierrl)
  if(ierrl/=0) then
    call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 147 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
    goto 1
  end if
  if(iotk_strscan(usefmt,"<>&")/=0) then
    call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 151 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
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
  call iotk_write_attr(lattr,"type",iotk_tolower("INTEGER"),first=.true.,ierr=ierrl)
  if(ierrl/=0) then
    call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 156 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
    goto 1
  end if
  call iotk_write_attr(lattr,"size",size(dat),ierr=ierrl)
  if(ierrl/=0) then
    call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 161 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
    goto 1
  end if
# 171 "iotk_dat.spp"
  if(binary) then
    call iotk_write_attr(lattr,"kind",kind(dat),ierr=ierrl)
    if(ierrl/=0) then
      call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 174 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
      goto 1
    end if
  end if
# 179 "iotk_dat.spp"
  if(.not.iotk_strcomp(usefmt,"!")) call iotk_write_attr(lattr,"fmt",iotk_strtrim(usefmt),ierr=ierrl)
  if(ierrl/=0) then
    call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 181 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
    goto 1
  end if
  if(lcolumns/=1) call iotk_write_attr(lattr,"columns",lcolumns,ierr=ierrl)
  if(ierrl/=0) then
    call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 186 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
    goto 1
  end if
  if(present(attr)) then
    attr_tmp(1:1)=iotk_eos
    call iotk_strcpy(attr_tmp,attr,ierr=ierrl)
    if(ierrl/=0) then
      call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 193 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
      goto 1
    end if
    call iotk_delete_attr(attr_tmp,"type",ierrl)
    if(ierrl/=0) then
      call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 198 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
      goto 1
    end if
    call iotk_delete_attr(attr_tmp,"kind",ierrl)
    if(ierrl/=0) then
      call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 203 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
      goto 1
    end if
    call iotk_delete_attr(attr_tmp,"size",ierrl)
    if(ierrl/=0) then
      call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 208 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
      goto 1
    end if
    call iotk_delete_attr(attr_tmp,"fmt",ierrl)
    if(ierrl/=0) then
      call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 213 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
      goto 1
    end if
    call iotk_delete_attr(attr_tmp,"columns",ierrl)
    if(ierrl/=0) then
      call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 218 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
      goto 1
    end if
    call iotk_delete_attr(attr_tmp,"len",ierrl)
    if(ierrl/=0) then
      call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 223 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
      goto 1
    end if
    if(iotk_strlen_trim(attr_tmp)>0) call iotk_strcat(lattr,iotk_strtrim(attr_tmp),ierr=ierrl)
  end if
  if(ierrl/=0) then
    call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 229 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
    goto 1
  end if
  call iotk_write_begin(unit,name,lattr,ierr=ierrl)
  if(ierrl/=0) then
    call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 234 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
    goto 1
  end if

  allocate(dattmp(size(dat)))
# 242 "iotk_dat.spp"
#if defined(__IOTK_WORKAROUND3) || defined(__IOTK_WORKAROUND4)
# 246 "iotk_dat.spp"
     call iotk_private_pack_INTEGER2(dattmp,dat,size(dattmp),1)
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
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
        goto 1
      end if
    else
      write(lunit,iostat=iostat) idummy,(dattmp(itmp),itmp=1,size(dattmp))
      if(iostat/=0) then
        call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 263 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
        goto 1
      end if
    end if
  else
    if(raw) then
# 272 "iotk_dat.spp"
      write(lunit,*,iostat=iostat) (dattmp(itmp),itmp=1,size(dattmp))
# 274 "iotk_dat.spp"
      if(iostat/=0) then
        call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 275 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
        goto 1
      end if
    else if(iotk_strcomp(usefmt,"*")) then
      write(lunit,*,iostat=iostat) (dattmp(itmp),itmp=1,size(dattmp))
      if(iostat/=0) then
        call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 281 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
        goto 1
      end if
    else if(iotk_strcomp(usefmt,"!")) then
# 295 "iotk_dat.spp"
     write(lunit,fmt=trim(iotk_wfmt("INTEGER",kind(dattmp),lcolumns,-1,lsep)),iostat=iostat) (dattmp(itmp),itmp=1,size(dattmp))
     if(iostat/=0) then
      call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 297 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
      goto 1
     end if
# 301 "iotk_dat.spp"
    else
      write(lunit,fmt=usefmt(1:iotk_strlen(usefmt)),iostat=iostat) (dattmp(itmp),itmp=1,size(dattmp))
      if(iostat/=0) then
        call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 304 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
        goto 1
      end if
    end if
  end if
  call iotk_write_end(unit,name,ierr=ierrl)
  if(ierrl/=0) then
    call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 311 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
    goto 1
  end if
1 continue
  if(allocated(dattmp)) deallocate(dattmp)
  if(present(ierr)) then
    ierr = ierrl
  else
    if(ierrl/=0) call iotk_error_handler(ierrl)
  end if
end subroutine iotk_write_dat_INTEGER2_7


# 589 "iotk_dat.spp"

# 591 "iotk_dat.spp"
subroutine iotk_scan_dat_INTEGER2_7(unit,name,dat,dummy,attr,found,default,ierr)
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
  INTEGER (kind=__IOTK_INTEGER2)                        :: dat (:,:,:,:,:,:,:)
#else
  INTEGER (kind=__IOTK_INTEGER2),           intent(out) :: dat (:,:,:,:,:,:,:)
#endif
  type(iotk_dummytype), optional         :: dummy
#ifdef __IOTK_WORKAROUND6
  character(len=*),optional              :: attr
#else
  character(len=*),optional, intent(out) :: attr
#endif
  logical,         optional, intent(out) :: found
  INTEGER (kind=__IOTK_INTEGER2), optional, intent(in)  :: default (:,:,:,:,:,:,:)
  integer,         optional, intent(out) :: ierr
# 618 "iotk_dat.spp"
  INTEGER (kind=__IOTK_INTEGER2),              allocatable :: tmpdat(:)
# 620 "iotk_dat.spp"
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
# 637 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
    goto 1
  end if
  call iotk_parse_dat(lattr,rtype,rkind,rsize,rlen,fmt,columns,ierrl)
! Note that columns is not effectively used
  if(ierrl/=0) goto 1
  if(.not. (iotk_strcomp(rtype,iotk_eos) .or. iotk_strcomp(rtype,"INTEGER") ) ) then
    call iotk_error_issue(ierrl,"iotk_scan_dat",__FILE__,__LINE__)
# 644 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
# 644 "iotk_dat.spp"
call iotk_error_msg(ierrl,' ')
# 644 "iotk_dat.spp"
call iotk_error_write(ierrl,"rtype",rtype(1:iotk_strlen(rtype)))
# 644 "iotk_dat.spp"
call iotk_error_write(ierrl,"type","INTEGER")
    goto 1
  end if
  if(.not. (rsize==-1 .or. rsize==size(dat)) ) then
    call iotk_error_issue(ierrl,"iotk_scan_dat",__FILE__,__LINE__)
# 648 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
    goto 1
  end if
  if(rkind==-1) rkind = kind(dat)
# 655 "iotk_dat.spp"

  allocate(tmpdat(size(dat)))
  call iotk_scan_dat_aux(unit,tmpdat,rkind,rlen,fmt(1:iotk_strlen(fmt)),ierrl)
# 662 "iotk_dat.spp"
#ifdef __IOTK_WORKAROUND7
        call iotk_reshape_INTEGER2( size(dat),dat,tmpdat )
#else
        dat = reshape(tmpdat,shape(dat))
#endif
# 671 "iotk_dat.spp"
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
# 683 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
# 683 "iotk_dat.spp"
call iotk_error_msg(ierrl,'Dat not found')
# 683 "iotk_dat.spp"
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
end subroutine iotk_scan_dat_INTEGER2_7


#endif
#endif

subroutine iotk_dat_dummy_INTEGER2_7
  write(0,*)
end subroutine iotk_dat_dummy_INTEGER2_7


# 727 "iotk_dat.spp"


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

#ifdef __IOTK_INTEGER3
#if 0 <= __IOTK_MAXRANK
# 82 "iotk_dat.spp"
subroutine iotk_write_dat_INTEGER3_0(unit,name,dat,dummy,attr,columns,sep,fmt,ierr)
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
  INTEGER (kind=__IOTK_INTEGER3), intent(in)  :: dat  
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
# 114 "iotk_dat.spp"
  INTEGER (kind=__IOTK_INTEGER3),allocatable :: dattmp(:)
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
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
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
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
    goto 1
  end if
  call iotk_strcpy(usefmt,"!",ierrl)
  if(ierrl/=0) then
    call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 142 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
    goto 1
  end if
  if(present(fmt) .and. .not. raw) call iotk_strcpy(usefmt,iotk_strtrim(fmt),ierrl)
  if(ierrl/=0) then
    call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 147 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
    goto 1
  end if
  if(iotk_strscan(usefmt,"<>&")/=0) then
    call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 151 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
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
  call iotk_write_attr(lattr,"type",iotk_tolower("INTEGER"),first=.true.,ierr=ierrl)
  if(ierrl/=0) then
    call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 156 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
    goto 1
  end if
  call iotk_write_attr(lattr,"size",1,ierr=ierrl)
  if(ierrl/=0) then
    call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 161 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
    goto 1
  end if
# 171 "iotk_dat.spp"
  if(binary) then
    call iotk_write_attr(lattr,"kind",kind(dat),ierr=ierrl)
    if(ierrl/=0) then
      call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 174 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
      goto 1
    end if
  end if
# 179 "iotk_dat.spp"
  if(.not.iotk_strcomp(usefmt,"!")) call iotk_write_attr(lattr,"fmt",iotk_strtrim(usefmt),ierr=ierrl)
  if(ierrl/=0) then
    call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 181 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
    goto 1
  end if
  if(lcolumns/=1) call iotk_write_attr(lattr,"columns",lcolumns,ierr=ierrl)
  if(ierrl/=0) then
    call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 186 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
    goto 1
  end if
  if(present(attr)) then
    attr_tmp(1:1)=iotk_eos
    call iotk_strcpy(attr_tmp,attr,ierr=ierrl)
    if(ierrl/=0) then
      call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 193 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
      goto 1
    end if
    call iotk_delete_attr(attr_tmp,"type",ierrl)
    if(ierrl/=0) then
      call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 198 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
      goto 1
    end if
    call iotk_delete_attr(attr_tmp,"kind",ierrl)
    if(ierrl/=0) then
      call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 203 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
      goto 1
    end if
    call iotk_delete_attr(attr_tmp,"size",ierrl)
    if(ierrl/=0) then
      call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 208 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
      goto 1
    end if
    call iotk_delete_attr(attr_tmp,"fmt",ierrl)
    if(ierrl/=0) then
      call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 213 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
      goto 1
    end if
    call iotk_delete_attr(attr_tmp,"columns",ierrl)
    if(ierrl/=0) then
      call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 218 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
      goto 1
    end if
    call iotk_delete_attr(attr_tmp,"len",ierrl)
    if(ierrl/=0) then
      call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 223 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
      goto 1
    end if
    if(iotk_strlen_trim(attr_tmp)>0) call iotk_strcat(lattr,iotk_strtrim(attr_tmp),ierr=ierrl)
  end if
  if(ierrl/=0) then
    call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 229 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
    goto 1
  end if
  call iotk_write_begin(unit,name,lattr,ierr=ierrl)
  if(ierrl/=0) then
    call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 234 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
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
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
        goto 1
      end if
    else
      write(lunit,iostat=iostat) idummy,(dattmp(itmp),itmp=1,size(dattmp))
      if(iostat/=0) then
        call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 263 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
        goto 1
      end if
    end if
  else
    if(raw) then
# 272 "iotk_dat.spp"
      write(lunit,*,iostat=iostat) (dattmp(itmp),itmp=1,size(dattmp))
# 274 "iotk_dat.spp"
      if(iostat/=0) then
        call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 275 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
        goto 1
      end if
    else if(iotk_strcomp(usefmt,"*")) then
      write(lunit,*,iostat=iostat) (dattmp(itmp),itmp=1,size(dattmp))
      if(iostat/=0) then
        call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 281 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
        goto 1
      end if
    else if(iotk_strcomp(usefmt,"!")) then
# 295 "iotk_dat.spp"
     write(lunit,fmt=trim(iotk_wfmt("INTEGER",kind(dattmp),lcolumns,-1,lsep)),iostat=iostat) (dattmp(itmp),itmp=1,size(dattmp))
     if(iostat/=0) then
      call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 297 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
      goto 1
     end if
# 301 "iotk_dat.spp"
    else
      write(lunit,fmt=usefmt(1:iotk_strlen(usefmt)),iostat=iostat) (dattmp(itmp),itmp=1,size(dattmp))
      if(iostat/=0) then
        call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 304 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
        goto 1
      end if
    end if
  end if
  call iotk_write_end(unit,name,ierr=ierrl)
  if(ierrl/=0) then
    call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 311 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
    goto 1
  end if
1 continue
  if(allocated(dattmp)) deallocate(dattmp)
  if(present(ierr)) then
    ierr = ierrl
  else
    if(ierrl/=0) call iotk_error_handler(ierrl)
  end if
end subroutine iotk_write_dat_INTEGER3_0


# 589 "iotk_dat.spp"

# 591 "iotk_dat.spp"
subroutine iotk_scan_dat_INTEGER3_0(unit,name,dat,dummy,attr,found,default,ierr)
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
  INTEGER (kind=__IOTK_INTEGER3)                        :: dat 
#else
  INTEGER (kind=__IOTK_INTEGER3),           intent(out) :: dat 
#endif
  type(iotk_dummytype), optional         :: dummy
#ifdef __IOTK_WORKAROUND6
  character(len=*),optional              :: attr
#else
  character(len=*),optional, intent(out) :: attr
#endif
  logical,         optional, intent(out) :: found
  INTEGER (kind=__IOTK_INTEGER3), optional, intent(in)  :: default 
  integer,         optional, intent(out) :: ierr
# 618 "iotk_dat.spp"
  INTEGER (kind=__IOTK_INTEGER3),              allocatable :: tmpdat(:)
# 620 "iotk_dat.spp"
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
# 637 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
    goto 1
  end if
  call iotk_parse_dat(lattr,rtype,rkind,rsize,rlen,fmt,columns,ierrl)
! Note that columns is not effectively used
  if(ierrl/=0) goto 1
  if(.not. (iotk_strcomp(rtype,iotk_eos) .or. iotk_strcomp(rtype,"INTEGER") ) ) then
    call iotk_error_issue(ierrl,"iotk_scan_dat",__FILE__,__LINE__)
# 644 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
# 644 "iotk_dat.spp"
call iotk_error_msg(ierrl,' ')
# 644 "iotk_dat.spp"
call iotk_error_write(ierrl,"rtype",rtype(1:iotk_strlen(rtype)))
# 644 "iotk_dat.spp"
call iotk_error_write(ierrl,"type","INTEGER")
    goto 1
  end if
  if(.not. (rsize==-1 .or. rsize==1) ) then
    call iotk_error_issue(ierrl,"iotk_scan_dat",__FILE__,__LINE__)
# 648 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
    goto 1
  end if
  if(rkind==-1) rkind = kind(dat)
# 655 "iotk_dat.spp"

  allocate(tmpdat(1))
  call iotk_scan_dat_aux(unit,tmpdat,rkind,rlen,fmt(1:iotk_strlen(fmt)),ierrl)
# 659 "iotk_dat.spp"
        dat = tmpdat(1)
# 671 "iotk_dat.spp"
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
# 683 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
# 683 "iotk_dat.spp"
call iotk_error_msg(ierrl,'Dat not found')
# 683 "iotk_dat.spp"
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
end subroutine iotk_scan_dat_INTEGER3_0


#endif
#endif

subroutine iotk_dat_dummy_INTEGER3_0
  write(0,*)
end subroutine iotk_dat_dummy_INTEGER3_0


# 727 "iotk_dat.spp"


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

#ifdef __IOTK_INTEGER3
#if 1 <= __IOTK_MAXRANK
# 82 "iotk_dat.spp"
subroutine iotk_write_dat_INTEGER3_1(unit,name,dat,dummy,attr,columns,sep,fmt,ierr)
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
  INTEGER (kind=__IOTK_INTEGER3), intent(in)  :: dat (:) 
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
# 114 "iotk_dat.spp"
  INTEGER (kind=__IOTK_INTEGER3),allocatable :: dattmp(:)
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
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
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
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
    goto 1
  end if
  call iotk_strcpy(usefmt,"!",ierrl)
  if(ierrl/=0) then
    call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 142 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
    goto 1
  end if
  if(present(fmt) .and. .not. raw) call iotk_strcpy(usefmt,iotk_strtrim(fmt),ierrl)
  if(ierrl/=0) then
    call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 147 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
    goto 1
  end if
  if(iotk_strscan(usefmt,"<>&")/=0) then
    call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 151 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
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
  call iotk_write_attr(lattr,"type",iotk_tolower("INTEGER"),first=.true.,ierr=ierrl)
  if(ierrl/=0) then
    call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 156 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
    goto 1
  end if
  call iotk_write_attr(lattr,"size",size(dat),ierr=ierrl)
  if(ierrl/=0) then
    call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 161 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
    goto 1
  end if
# 171 "iotk_dat.spp"
  if(binary) then
    call iotk_write_attr(lattr,"kind",kind(dat),ierr=ierrl)
    if(ierrl/=0) then
      call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 174 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
      goto 1
    end if
  end if
# 179 "iotk_dat.spp"
  if(.not.iotk_strcomp(usefmt,"!")) call iotk_write_attr(lattr,"fmt",iotk_strtrim(usefmt),ierr=ierrl)
  if(ierrl/=0) then
    call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 181 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
    goto 1
  end if
  if(lcolumns/=1) call iotk_write_attr(lattr,"columns",lcolumns,ierr=ierrl)
  if(ierrl/=0) then
    call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 186 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
    goto 1
  end if
  if(present(attr)) then
    attr_tmp(1:1)=iotk_eos
    call iotk_strcpy(attr_tmp,attr,ierr=ierrl)
    if(ierrl/=0) then
      call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 193 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
      goto 1
    end if
    call iotk_delete_attr(attr_tmp,"type",ierrl)
    if(ierrl/=0) then
      call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 198 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
      goto 1
    end if
    call iotk_delete_attr(attr_tmp,"kind",ierrl)
    if(ierrl/=0) then
      call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 203 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
      goto 1
    end if
    call iotk_delete_attr(attr_tmp,"size",ierrl)
    if(ierrl/=0) then
      call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 208 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
      goto 1
    end if
    call iotk_delete_attr(attr_tmp,"fmt",ierrl)
    if(ierrl/=0) then
      call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 213 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
      goto 1
    end if
    call iotk_delete_attr(attr_tmp,"columns",ierrl)
    if(ierrl/=0) then
      call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 218 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
      goto 1
    end if
    call iotk_delete_attr(attr_tmp,"len",ierrl)
    if(ierrl/=0) then
      call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 223 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
      goto 1
    end if
    if(iotk_strlen_trim(attr_tmp)>0) call iotk_strcat(lattr,iotk_strtrim(attr_tmp),ierr=ierrl)
  end if
  if(ierrl/=0) then
    call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 229 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
    goto 1
  end if
  call iotk_write_begin(unit,name,lattr,ierr=ierrl)
  if(ierrl/=0) then
    call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 234 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
    goto 1
  end if

  allocate(dattmp(size(dat)))
# 242 "iotk_dat.spp"
#if defined(__IOTK_WORKAROUND3) || defined(__IOTK_WORKAROUND4)
# 246 "iotk_dat.spp"
     call iotk_private_pack_INTEGER3(dattmp,dat,size(dattmp),1)
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
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
        goto 1
      end if
    else
      write(lunit,iostat=iostat) idummy,(dattmp(itmp),itmp=1,size(dattmp))
      if(iostat/=0) then
        call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 263 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
        goto 1
      end if
    end if
  else
    if(raw) then
# 272 "iotk_dat.spp"
      write(lunit,*,iostat=iostat) (dattmp(itmp),itmp=1,size(dattmp))
# 274 "iotk_dat.spp"
      if(iostat/=0) then
        call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 275 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
        goto 1
      end if
    else if(iotk_strcomp(usefmt,"*")) then
      write(lunit,*,iostat=iostat) (dattmp(itmp),itmp=1,size(dattmp))
      if(iostat/=0) then
        call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 281 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
        goto 1
      end if
    else if(iotk_strcomp(usefmt,"!")) then
# 295 "iotk_dat.spp"
     write(lunit,fmt=trim(iotk_wfmt("INTEGER",kind(dattmp),lcolumns,-1,lsep)),iostat=iostat) (dattmp(itmp),itmp=1,size(dattmp))
     if(iostat/=0) then
      call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 297 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
      goto 1
     end if
# 301 "iotk_dat.spp"
    else
      write(lunit,fmt=usefmt(1:iotk_strlen(usefmt)),iostat=iostat) (dattmp(itmp),itmp=1,size(dattmp))
      if(iostat/=0) then
        call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 304 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
        goto 1
      end if
    end if
  end if
  call iotk_write_end(unit,name,ierr=ierrl)
  if(ierrl/=0) then
    call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 311 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
    goto 1
  end if
1 continue
  if(allocated(dattmp)) deallocate(dattmp)
  if(present(ierr)) then
    ierr = ierrl
  else
    if(ierrl/=0) call iotk_error_handler(ierrl)
  end if
end subroutine iotk_write_dat_INTEGER3_1


# 326 "iotk_dat.spp"
recursive subroutine iotk_scan_dat_aux_INTEGER3(unit,dat,rkind,rlen,fmt,ierr)
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
  INTEGER (kind=__IOTK_INTEGER3)                        :: dat (:)
#else
  INTEGER (kind=__IOTK_INTEGER3),           intent(out) :: dat (:)
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
# 356 "iotk_dat.spp"
#ifdef __IOTK_INTEGER1
  INTEGER (__IOTK_INTEGER1), allocatable :: dat1 (:)
#endif
# 356 "iotk_dat.spp"
#ifdef __IOTK_INTEGER2
  INTEGER (__IOTK_INTEGER2), allocatable :: dat2 (:)
#endif
# 356 "iotk_dat.spp"
#ifdef __IOTK_INTEGER4
  INTEGER (__IOTK_INTEGER4), allocatable :: dat4 (:)
#endif
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
call iotk_error_msg(ierr,"CVS Revision: 1.17 ")
    return
  end if
# 463 "iotk_dat.spp"
  if(binary) then
    select case(rkind)
    case(kind(dat))
      if(raw) then
        read(lunit,iostat=iostat) ( dat(i), i=1,ubound(dat,1) )
        if(iostat/=0) then
          call iotk_error_issue(ierr,"iotk_scan_dat_aux",__FILE__,__LINE__)
# 469 "iotk_dat.spp"
call iotk_error_msg(ierr,"CVS Revision: 1.17 ")
# 469 "iotk_dat.spp"
call iotk_error_msg(ierr,' ')
# 469 "iotk_dat.spp"
call iotk_error_write(ierr,"iostat",iostat)
          return
        end if
      else
        read(lunit,iostat=iostat) idummy, ( dat(i), i=1,ubound(dat,1) )
        if(iostat/=0) then
          call iotk_error_issue(ierr,"iotk_scan_dat_aux",__FILE__,__LINE__)
# 475 "iotk_dat.spp"
call iotk_error_msg(ierr,"CVS Revision: 1.17 ")
# 475 "iotk_dat.spp"
call iotk_error_msg(ierr,' ')
# 475 "iotk_dat.spp"
call iotk_error_write(ierr,"iostat",iostat)
          return
        end if
      end if
# 481 "iotk_dat.spp"
#ifdef __IOTK_INTEGER1
    case(kind(dat1))
      ! Giusto per scrupolo. Se e' raw non ci sono info sul kind, quindi questa linea e' irraggiungibile
      if(raw) then
        call iotk_error_issue(ierr,"iotk_scan_dat_aux",__FILE__,__LINE__)
# 485 "iotk_dat.spp"
call iotk_error_msg(ierr,"CVS Revision: 1.17 ")
        return
      end if
      allocate(dat1(ubound(dat,1)))
      read(lunit,iostat=iostat) idummy,( dat1(i), i=1,ubound(dat1,1) )
      if(iostat/=0) then
        call iotk_error_issue(ierr,"iotk_scan_dat_aux",__FILE__,__LINE__)
# 491 "iotk_dat.spp"
call iotk_error_msg(ierr,"CVS Revision: 1.17 ")
# 491 "iotk_dat.spp"
call iotk_error_msg(ierr,' ')
# 491 "iotk_dat.spp"
call iotk_error_write(ierr,"iostat",iostat)
        return
      end if
# 501 "iotk_dat.spp"
      dat = dat1
# 503 "iotk_dat.spp"
      deallocate(dat1)
#endif
# 481 "iotk_dat.spp"
#ifdef __IOTK_INTEGER2
    case(kind(dat2))
      ! Giusto per scrupolo. Se e' raw non ci sono info sul kind, quindi questa linea e' irraggiungibile
      if(raw) then
        call iotk_error_issue(ierr,"iotk_scan_dat_aux",__FILE__,__LINE__)
# 485 "iotk_dat.spp"
call iotk_error_msg(ierr,"CVS Revision: 1.17 ")
        return
      end if
      allocate(dat2(ubound(dat,1)))
      read(lunit,iostat=iostat) idummy,( dat2(i), i=1,ubound(dat2,1) )
      if(iostat/=0) then
        call iotk_error_issue(ierr,"iotk_scan_dat_aux",__FILE__,__LINE__)
# 491 "iotk_dat.spp"
call iotk_error_msg(ierr,"CVS Revision: 1.17 ")
# 491 "iotk_dat.spp"
call iotk_error_msg(ierr,' ')
# 491 "iotk_dat.spp"
call iotk_error_write(ierr,"iostat",iostat)
        return
      end if
# 501 "iotk_dat.spp"
      dat = dat2
# 503 "iotk_dat.spp"
      deallocate(dat2)
#endif
# 481 "iotk_dat.spp"
#ifdef __IOTK_INTEGER4
    case(kind(dat4))
      ! Giusto per scrupolo. Se e' raw non ci sono info sul kind, quindi questa linea e' irraggiungibile
      if(raw) then
        call iotk_error_issue(ierr,"iotk_scan_dat_aux",__FILE__,__LINE__)
# 485 "iotk_dat.spp"
call iotk_error_msg(ierr,"CVS Revision: 1.17 ")
        return
      end if
      allocate(dat4(ubound(dat,1)))
      read(lunit,iostat=iostat) idummy,( dat4(i), i=1,ubound(dat4,1) )
      if(iostat/=0) then
        call iotk_error_issue(ierr,"iotk_scan_dat_aux",__FILE__,__LINE__)
# 491 "iotk_dat.spp"
call iotk_error_msg(ierr,"CVS Revision: 1.17 ")
# 491 "iotk_dat.spp"
call iotk_error_msg(ierr,' ')
# 491 "iotk_dat.spp"
call iotk_error_write(ierr,"iostat",iostat)
        return
      end if
# 501 "iotk_dat.spp"
      dat = dat4
# 503 "iotk_dat.spp"
      deallocate(dat4)
#endif
# 507 "iotk_dat.spp"
    case default
      call iotk_error_issue(ierr,"iotk_scan_dat_aux",__FILE__,__LINE__)
# 508 "iotk_dat.spp"
call iotk_error_msg(ierr,"CVS Revision: 1.17 ")
# 508 "iotk_dat.spp"
call iotk_error_msg(ierr,'Kind incompatibility')
# 508 "iotk_dat.spp"
call iotk_error_write(ierr,"kind",rkind)
    end select
  else
    if(raw) then
      read(lunit,fmt=*,iostat=iostat) ( dat(i), i=1,ubound(dat,1) )
      if(iostat/=0) then
        call iotk_error_issue(ierr,"iotk_scan_dat_aux",__FILE__,__LINE__)
# 514 "iotk_dat.spp"
call iotk_error_msg(ierr,"CVS Revision: 1.17 ")
# 514 "iotk_dat.spp"
call iotk_error_msg(ierr,' ')
# 514 "iotk_dat.spp"
call iotk_error_write(ierr,"iostat",iostat)
        return
      end if
    else if(iotk_strcomp(fmt,"*")) then
      read(lunit,fmt=*,iostat=iostat) ( dat(i), i=1,ubound(dat,1) )
      if(iostat/=0) then
        call iotk_error_issue(ierr,"iotk_scan_dat_aux",__FILE__,__LINE__)
# 520 "iotk_dat.spp"
call iotk_error_msg(ierr,"CVS Revision: 1.17 ")
# 520 "iotk_dat.spp"
call iotk_error_msg(ierr,' ')
# 520 "iotk_dat.spp"
call iotk_error_write(ierr,"iostat",iostat)
        return
      end if
    else if(iotk_strcomp(fmt,"!")) then
      index = 0
      do
        call iotk_getline(lunit,line,length,ierr)
        if(ierr/=0) then
          call iotk_error_issue(ierr,"iotk_scan_dat_aux",__FILE__,__LINE__)
# 528 "iotk_dat.spp"
call iotk_error_msg(ierr,"CVS Revision: 1.17 ")
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
# 539 "iotk_dat.spp"
call iotk_error_msg(ierr,"CVS Revision: 1.17 ")
# 539 "iotk_dat.spp"
call iotk_error_msg(ierr,' ')
# 539 "iotk_dat.spp"
call iotk_error_write(ierr,"iostat",iostat)
            return
          end if
          call iotk_getline(lunit,altline,altlength,ierr)
          if(ierr/=0) then
            call iotk_error_issue(ierr,"iotk_scan_dat_aux",__FILE__,__LINE__)
# 544 "iotk_dat.spp"
call iotk_error_msg(ierr,"CVS Revision: 1.17 ")
            return
          end if
          backspace(lunit,iostat=iostat)
          if(iostat/=0) then
            call iotk_error_issue(ierr,"iotk_scan_dat_aux",__FILE__,__LINE__)
# 549 "iotk_dat.spp"
call iotk_error_msg(ierr,"CVS Revision: 1.17 ")
# 549 "iotk_dat.spp"
call iotk_error_msg(ierr,' ')
# 549 "iotk_dat.spp"
call iotk_error_write(ierr,"iostat",iostat)
            return
          end if
          read(lunit,"(a)",advance="no",iostat=iostat) altline(1:nexttag-1 + altlength - length)
          if(iostat/=0) then
            call iotk_error_issue(ierr,"iotk_scan_dat_aux",__FILE__,__LINE__)
# 554 "iotk_dat.spp"
call iotk_error_msg(ierr,"CVS Revision: 1.17 ")
# 554 "iotk_dat.spp"
call iotk_error_msg(ierr,' ')
# 554 "iotk_dat.spp"
call iotk_error_write(ierr,"iostat",iostat)
            return
          end if
        end if
        call iotk_str_clean(line(1:nexttag - 1))
        call iotk_read(dat,line(1:nexttag - 1),index,ierr)
        if(ierr/=0) then
          call iotk_error_issue(ierr,"iotk_scan_dat_aux",__FILE__,__LINE__)
# 561 "iotk_dat.spp"
call iotk_error_msg(ierr,"CVS Revision: 1.17 ")
          return
        end if
# 567 "iotk_dat.spp"
        if(index == size(dat)) exit
# 569 "iotk_dat.spp"
        if(nexttag/=length + 1) then
          call iotk_error_issue(ierr,"iotk_scan_dat_aux",__FILE__,__LINE__)
# 570 "iotk_dat.spp"
call iotk_error_msg(ierr,"CVS Revision: 1.17 ")
          return
        end if
      end do
    else
      read(lunit,fmt=fmt(1:iotk_strlen(fmt)),iostat=iostat) ( dat(i), i=1,ubound(dat,1) )
      if(iostat/=0) then
        call iotk_error_issue(ierr,"iotk_scan_dat_aux",__FILE__,__LINE__)
# 577 "iotk_dat.spp"
call iotk_error_msg(ierr,"CVS Revision: 1.17 ")
# 577 "iotk_dat.spp"
call iotk_error_msg(ierr,' ')
# 577 "iotk_dat.spp"
call iotk_error_write(ierr,"iostat",iostat)
        return
      end if
    end if
  end if
# 583 "iotk_dat.spp"
  if(idummy/=0) then
    call iotk_error_issue(ierr,"iotk_scan_dat_aux",__FILE__,__LINE__)
# 584 "iotk_dat.spp"
call iotk_error_msg(ierr,"CVS Revision: 1.17 ")
    return
  end if
end subroutine iotk_scan_dat_aux_INTEGER3
# 589 "iotk_dat.spp"

# 591 "iotk_dat.spp"
subroutine iotk_scan_dat_INTEGER3_1(unit,name,dat,dummy,attr,found,default,ierr)
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
  INTEGER (kind=__IOTK_INTEGER3)                        :: dat (:)
#else
  INTEGER (kind=__IOTK_INTEGER3),           intent(out) :: dat (:)
#endif
  type(iotk_dummytype), optional         :: dummy
#ifdef __IOTK_WORKAROUND6
  character(len=*),optional              :: attr
#else
  character(len=*),optional, intent(out) :: attr
#endif
  logical,         optional, intent(out) :: found
  INTEGER (kind=__IOTK_INTEGER3), optional, intent(in)  :: default (:)
  integer,         optional, intent(out) :: ierr
# 618 "iotk_dat.spp"
  INTEGER (kind=__IOTK_INTEGER3),              allocatable :: tmpdat(:)
# 620 "iotk_dat.spp"
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
# 637 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
    goto 1
  end if
  call iotk_parse_dat(lattr,rtype,rkind,rsize,rlen,fmt,columns,ierrl)
! Note that columns is not effectively used
  if(ierrl/=0) goto 1
  if(.not. (iotk_strcomp(rtype,iotk_eos) .or. iotk_strcomp(rtype,"INTEGER") ) ) then
    call iotk_error_issue(ierrl,"iotk_scan_dat",__FILE__,__LINE__)
# 644 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
# 644 "iotk_dat.spp"
call iotk_error_msg(ierrl,' ')
# 644 "iotk_dat.spp"
call iotk_error_write(ierrl,"rtype",rtype(1:iotk_strlen(rtype)))
# 644 "iotk_dat.spp"
call iotk_error_write(ierrl,"type","INTEGER")
    goto 1
  end if
  if(.not. (rsize==-1 .or. rsize==size(dat)) ) then
    call iotk_error_issue(ierrl,"iotk_scan_dat",__FILE__,__LINE__)
# 648 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
    goto 1
  end if
  if(rkind==-1) rkind = kind(dat)
# 655 "iotk_dat.spp"

  allocate(tmpdat(size(dat)))
  call iotk_scan_dat_aux(unit,tmpdat,rkind,rlen,fmt(1:iotk_strlen(fmt)),ierrl)
# 662 "iotk_dat.spp"
#ifdef __IOTK_WORKAROUND7
        call iotk_reshape_INTEGER3( size(dat),dat,tmpdat )
#else
        dat = reshape(tmpdat,shape(dat))
#endif
# 671 "iotk_dat.spp"
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
# 683 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
# 683 "iotk_dat.spp"
call iotk_error_msg(ierrl,'Dat not found')
# 683 "iotk_dat.spp"
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
end subroutine iotk_scan_dat_INTEGER3_1


#endif
#endif

subroutine iotk_dat_dummy_INTEGER3_1
  write(0,*)
end subroutine iotk_dat_dummy_INTEGER3_1


# 706 "iotk_dat.spp"
!
! the following workaround is introduced for INTEL compiler < 9.0 
! having problem in treating INTEGER arrays with large dimensions
! within implicit loops
!
#ifdef __IOTK_WORKAROUND7

subroutine iotk_reshape_INTEGER3(n,dat_out,dat_in)
  implicit none
  integer, intent(in) :: n
  INTEGER (kind=__IOTK_INTEGER3), intent(in)  :: dat_in(n)
  INTEGER (kind=__IOTK_INTEGER3), intent(out) :: dat_out(n)
  integer :: i
  do i=1,n
     dat_out(i) = dat_in(i)
  enddo
end subroutine iotk_reshape_INTEGER3

#endif

# 727 "iotk_dat.spp"


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

#ifdef __IOTK_INTEGER3
#if 2 <= __IOTK_MAXRANK
# 82 "iotk_dat.spp"
subroutine iotk_write_dat_INTEGER3_2(unit,name,dat,dummy,attr,columns,sep,fmt,ierr)
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
  INTEGER (kind=__IOTK_INTEGER3), intent(in)  :: dat (:,:) 
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
# 114 "iotk_dat.spp"
  INTEGER (kind=__IOTK_INTEGER3),allocatable :: dattmp(:)
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
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
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
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
    goto 1
  end if
  call iotk_strcpy(usefmt,"!",ierrl)
  if(ierrl/=0) then
    call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 142 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
    goto 1
  end if
  if(present(fmt) .and. .not. raw) call iotk_strcpy(usefmt,iotk_strtrim(fmt),ierrl)
  if(ierrl/=0) then
    call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 147 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
    goto 1
  end if
  if(iotk_strscan(usefmt,"<>&")/=0) then
    call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 151 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
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
  call iotk_write_attr(lattr,"type",iotk_tolower("INTEGER"),first=.true.,ierr=ierrl)
  if(ierrl/=0) then
    call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 156 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
    goto 1
  end if
  call iotk_write_attr(lattr,"size",size(dat),ierr=ierrl)
  if(ierrl/=0) then
    call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 161 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
    goto 1
  end if
# 171 "iotk_dat.spp"
  if(binary) then
    call iotk_write_attr(lattr,"kind",kind(dat),ierr=ierrl)
    if(ierrl/=0) then
      call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 174 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
      goto 1
    end if
  end if
# 179 "iotk_dat.spp"
  if(.not.iotk_strcomp(usefmt,"!")) call iotk_write_attr(lattr,"fmt",iotk_strtrim(usefmt),ierr=ierrl)
  if(ierrl/=0) then
    call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 181 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
    goto 1
  end if
  if(lcolumns/=1) call iotk_write_attr(lattr,"columns",lcolumns,ierr=ierrl)
  if(ierrl/=0) then
    call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 186 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
    goto 1
  end if
  if(present(attr)) then
    attr_tmp(1:1)=iotk_eos
    call iotk_strcpy(attr_tmp,attr,ierr=ierrl)
    if(ierrl/=0) then
      call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 193 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
      goto 1
    end if
    call iotk_delete_attr(attr_tmp,"type",ierrl)
    if(ierrl/=0) then
      call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 198 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
      goto 1
    end if
    call iotk_delete_attr(attr_tmp,"kind",ierrl)
    if(ierrl/=0) then
      call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 203 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
      goto 1
    end if
    call iotk_delete_attr(attr_tmp,"size",ierrl)
    if(ierrl/=0) then
      call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 208 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
      goto 1
    end if
    call iotk_delete_attr(attr_tmp,"fmt",ierrl)
    if(ierrl/=0) then
      call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 213 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
      goto 1
    end if
    call iotk_delete_attr(attr_tmp,"columns",ierrl)
    if(ierrl/=0) then
      call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 218 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
      goto 1
    end if
    call iotk_delete_attr(attr_tmp,"len",ierrl)
    if(ierrl/=0) then
      call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 223 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
      goto 1
    end if
    if(iotk_strlen_trim(attr_tmp)>0) call iotk_strcat(lattr,iotk_strtrim(attr_tmp),ierr=ierrl)
  end if
  if(ierrl/=0) then
    call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 229 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
    goto 1
  end if
  call iotk_write_begin(unit,name,lattr,ierr=ierrl)
  if(ierrl/=0) then
    call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 234 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
    goto 1
  end if

  allocate(dattmp(size(dat)))
# 242 "iotk_dat.spp"
#if defined(__IOTK_WORKAROUND3) || defined(__IOTK_WORKAROUND4)
# 246 "iotk_dat.spp"
     call iotk_private_pack_INTEGER3(dattmp,dat,size(dattmp),1)
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
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
        goto 1
      end if
    else
      write(lunit,iostat=iostat) idummy,(dattmp(itmp),itmp=1,size(dattmp))
      if(iostat/=0) then
        call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 263 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
        goto 1
      end if
    end if
  else
    if(raw) then
# 272 "iotk_dat.spp"
      write(lunit,*,iostat=iostat) (dattmp(itmp),itmp=1,size(dattmp))
# 274 "iotk_dat.spp"
      if(iostat/=0) then
        call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 275 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
        goto 1
      end if
    else if(iotk_strcomp(usefmt,"*")) then
      write(lunit,*,iostat=iostat) (dattmp(itmp),itmp=1,size(dattmp))
      if(iostat/=0) then
        call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 281 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
        goto 1
      end if
    else if(iotk_strcomp(usefmt,"!")) then
# 295 "iotk_dat.spp"
     write(lunit,fmt=trim(iotk_wfmt("INTEGER",kind(dattmp),lcolumns,-1,lsep)),iostat=iostat) (dattmp(itmp),itmp=1,size(dattmp))
     if(iostat/=0) then
      call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 297 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
      goto 1
     end if
# 301 "iotk_dat.spp"
    else
      write(lunit,fmt=usefmt(1:iotk_strlen(usefmt)),iostat=iostat) (dattmp(itmp),itmp=1,size(dattmp))
      if(iostat/=0) then
        call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 304 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
        goto 1
      end if
    end if
  end if
  call iotk_write_end(unit,name,ierr=ierrl)
  if(ierrl/=0) then
    call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 311 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
    goto 1
  end if
1 continue
  if(allocated(dattmp)) deallocate(dattmp)
  if(present(ierr)) then
    ierr = ierrl
  else
    if(ierrl/=0) call iotk_error_handler(ierrl)
  end if
end subroutine iotk_write_dat_INTEGER3_2


# 589 "iotk_dat.spp"

# 591 "iotk_dat.spp"
subroutine iotk_scan_dat_INTEGER3_2(unit,name,dat,dummy,attr,found,default,ierr)
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
  INTEGER (kind=__IOTK_INTEGER3)                        :: dat (:,:)
#else
  INTEGER (kind=__IOTK_INTEGER3),           intent(out) :: dat (:,:)
#endif
  type(iotk_dummytype), optional         :: dummy
#ifdef __IOTK_WORKAROUND6
  character(len=*),optional              :: attr
#else
  character(len=*),optional, intent(out) :: attr
#endif
  logical,         optional, intent(out) :: found
  INTEGER (kind=__IOTK_INTEGER3), optional, intent(in)  :: default (:,:)
  integer,         optional, intent(out) :: ierr
# 618 "iotk_dat.spp"
  INTEGER (kind=__IOTK_INTEGER3),              allocatable :: tmpdat(:)
# 620 "iotk_dat.spp"
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
# 637 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
    goto 1
  end if
  call iotk_parse_dat(lattr,rtype,rkind,rsize,rlen,fmt,columns,ierrl)
! Note that columns is not effectively used
  if(ierrl/=0) goto 1
  if(.not. (iotk_strcomp(rtype,iotk_eos) .or. iotk_strcomp(rtype,"INTEGER") ) ) then
    call iotk_error_issue(ierrl,"iotk_scan_dat",__FILE__,__LINE__)
# 644 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
# 644 "iotk_dat.spp"
call iotk_error_msg(ierrl,' ')
# 644 "iotk_dat.spp"
call iotk_error_write(ierrl,"rtype",rtype(1:iotk_strlen(rtype)))
# 644 "iotk_dat.spp"
call iotk_error_write(ierrl,"type","INTEGER")
    goto 1
  end if
  if(.not. (rsize==-1 .or. rsize==size(dat)) ) then
    call iotk_error_issue(ierrl,"iotk_scan_dat",__FILE__,__LINE__)
# 648 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
    goto 1
  end if
  if(rkind==-1) rkind = kind(dat)
# 655 "iotk_dat.spp"

  allocate(tmpdat(size(dat)))
  call iotk_scan_dat_aux(unit,tmpdat,rkind,rlen,fmt(1:iotk_strlen(fmt)),ierrl)
# 662 "iotk_dat.spp"
#ifdef __IOTK_WORKAROUND7
        call iotk_reshape_INTEGER3( size(dat),dat,tmpdat )
#else
        dat = reshape(tmpdat,shape(dat))
#endif
# 671 "iotk_dat.spp"
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
# 683 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
# 683 "iotk_dat.spp"
call iotk_error_msg(ierrl,'Dat not found')
# 683 "iotk_dat.spp"
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
end subroutine iotk_scan_dat_INTEGER3_2


#endif
#endif

subroutine iotk_dat_dummy_INTEGER3_2
  write(0,*)
end subroutine iotk_dat_dummy_INTEGER3_2


# 727 "iotk_dat.spp"


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

#ifdef __IOTK_INTEGER3
#if 3 <= __IOTK_MAXRANK
# 82 "iotk_dat.spp"
subroutine iotk_write_dat_INTEGER3_3(unit,name,dat,dummy,attr,columns,sep,fmt,ierr)
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
  INTEGER (kind=__IOTK_INTEGER3), intent(in)  :: dat (:,:,:) 
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
# 114 "iotk_dat.spp"
  INTEGER (kind=__IOTK_INTEGER3),allocatable :: dattmp(:)
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
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
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
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
    goto 1
  end if
  call iotk_strcpy(usefmt,"!",ierrl)
  if(ierrl/=0) then
    call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 142 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
    goto 1
  end if
  if(present(fmt) .and. .not. raw) call iotk_strcpy(usefmt,iotk_strtrim(fmt),ierrl)
  if(ierrl/=0) then
    call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 147 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
    goto 1
  end if
  if(iotk_strscan(usefmt,"<>&")/=0) then
    call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 151 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
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
  call iotk_write_attr(lattr,"type",iotk_tolower("INTEGER"),first=.true.,ierr=ierrl)
  if(ierrl/=0) then
    call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 156 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
    goto 1
  end if
  call iotk_write_attr(lattr,"size",size(dat),ierr=ierrl)
  if(ierrl/=0) then
    call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 161 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
    goto 1
  end if
# 171 "iotk_dat.spp"
  if(binary) then
    call iotk_write_attr(lattr,"kind",kind(dat),ierr=ierrl)
    if(ierrl/=0) then
      call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 174 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
      goto 1
    end if
  end if
# 179 "iotk_dat.spp"
  if(.not.iotk_strcomp(usefmt,"!")) call iotk_write_attr(lattr,"fmt",iotk_strtrim(usefmt),ierr=ierrl)
  if(ierrl/=0) then
    call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 181 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
    goto 1
  end if
  if(lcolumns/=1) call iotk_write_attr(lattr,"columns",lcolumns,ierr=ierrl)
  if(ierrl/=0) then
    call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 186 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
    goto 1
  end if
  if(present(attr)) then
    attr_tmp(1:1)=iotk_eos
    call iotk_strcpy(attr_tmp,attr,ierr=ierrl)
    if(ierrl/=0) then
      call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 193 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
      goto 1
    end if
    call iotk_delete_attr(attr_tmp,"type",ierrl)
    if(ierrl/=0) then
      call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 198 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
      goto 1
    end if
    call iotk_delete_attr(attr_tmp,"kind",ierrl)
    if(ierrl/=0) then
      call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 203 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
      goto 1
    end if
    call iotk_delete_attr(attr_tmp,"size",ierrl)
    if(ierrl/=0) then
      call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 208 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
      goto 1
    end if
    call iotk_delete_attr(attr_tmp,"fmt",ierrl)
    if(ierrl/=0) then
      call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 213 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
      goto 1
    end if
    call iotk_delete_attr(attr_tmp,"columns",ierrl)
    if(ierrl/=0) then
      call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 218 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
      goto 1
    end if
    call iotk_delete_attr(attr_tmp,"len",ierrl)
    if(ierrl/=0) then
      call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 223 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
      goto 1
    end if
    if(iotk_strlen_trim(attr_tmp)>0) call iotk_strcat(lattr,iotk_strtrim(attr_tmp),ierr=ierrl)
  end if
  if(ierrl/=0) then
    call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 229 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
    goto 1
  end if
  call iotk_write_begin(unit,name,lattr,ierr=ierrl)
  if(ierrl/=0) then
    call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 234 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
    goto 1
  end if

  allocate(dattmp(size(dat)))
# 242 "iotk_dat.spp"
#if defined(__IOTK_WORKAROUND3) || defined(__IOTK_WORKAROUND4)
# 246 "iotk_dat.spp"
     call iotk_private_pack_INTEGER3(dattmp,dat,size(dattmp),1)
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
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
        goto 1
      end if
    else
      write(lunit,iostat=iostat) idummy,(dattmp(itmp),itmp=1,size(dattmp))
      if(iostat/=0) then
        call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 263 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
        goto 1
      end if
    end if
  else
    if(raw) then
# 272 "iotk_dat.spp"
      write(lunit,*,iostat=iostat) (dattmp(itmp),itmp=1,size(dattmp))
# 274 "iotk_dat.spp"
      if(iostat/=0) then
        call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 275 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
        goto 1
      end if
    else if(iotk_strcomp(usefmt,"*")) then
      write(lunit,*,iostat=iostat) (dattmp(itmp),itmp=1,size(dattmp))
      if(iostat/=0) then
        call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 281 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
        goto 1
      end if
    else if(iotk_strcomp(usefmt,"!")) then
# 295 "iotk_dat.spp"
     write(lunit,fmt=trim(iotk_wfmt("INTEGER",kind(dattmp),lcolumns,-1,lsep)),iostat=iostat) (dattmp(itmp),itmp=1,size(dattmp))
     if(iostat/=0) then
      call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 297 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
      goto 1
     end if
# 301 "iotk_dat.spp"
    else
      write(lunit,fmt=usefmt(1:iotk_strlen(usefmt)),iostat=iostat) (dattmp(itmp),itmp=1,size(dattmp))
      if(iostat/=0) then
        call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 304 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
        goto 1
      end if
    end if
  end if
  call iotk_write_end(unit,name,ierr=ierrl)
  if(ierrl/=0) then
    call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 311 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
    goto 1
  end if
1 continue
  if(allocated(dattmp)) deallocate(dattmp)
  if(present(ierr)) then
    ierr = ierrl
  else
    if(ierrl/=0) call iotk_error_handler(ierrl)
  end if
end subroutine iotk_write_dat_INTEGER3_3


# 589 "iotk_dat.spp"

# 591 "iotk_dat.spp"
subroutine iotk_scan_dat_INTEGER3_3(unit,name,dat,dummy,attr,found,default,ierr)
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
  INTEGER (kind=__IOTK_INTEGER3)                        :: dat (:,:,:)
#else
  INTEGER (kind=__IOTK_INTEGER3),           intent(out) :: dat (:,:,:)
#endif
  type(iotk_dummytype), optional         :: dummy
#ifdef __IOTK_WORKAROUND6
  character(len=*),optional              :: attr
#else
  character(len=*),optional, intent(out) :: attr
#endif
  logical,         optional, intent(out) :: found
  INTEGER (kind=__IOTK_INTEGER3), optional, intent(in)  :: default (:,:,:)
  integer,         optional, intent(out) :: ierr
# 618 "iotk_dat.spp"
  INTEGER (kind=__IOTK_INTEGER3),              allocatable :: tmpdat(:)
# 620 "iotk_dat.spp"
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
# 637 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
    goto 1
  end if
  call iotk_parse_dat(lattr,rtype,rkind,rsize,rlen,fmt,columns,ierrl)
! Note that columns is not effectively used
  if(ierrl/=0) goto 1
  if(.not. (iotk_strcomp(rtype,iotk_eos) .or. iotk_strcomp(rtype,"INTEGER") ) ) then
    call iotk_error_issue(ierrl,"iotk_scan_dat",__FILE__,__LINE__)
# 644 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
# 644 "iotk_dat.spp"
call iotk_error_msg(ierrl,' ')
# 644 "iotk_dat.spp"
call iotk_error_write(ierrl,"rtype",rtype(1:iotk_strlen(rtype)))
# 644 "iotk_dat.spp"
call iotk_error_write(ierrl,"type","INTEGER")
    goto 1
  end if
  if(.not. (rsize==-1 .or. rsize==size(dat)) ) then
    call iotk_error_issue(ierrl,"iotk_scan_dat",__FILE__,__LINE__)
# 648 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
    goto 1
  end if
  if(rkind==-1) rkind = kind(dat)
# 655 "iotk_dat.spp"

  allocate(tmpdat(size(dat)))
  call iotk_scan_dat_aux(unit,tmpdat,rkind,rlen,fmt(1:iotk_strlen(fmt)),ierrl)
# 662 "iotk_dat.spp"
#ifdef __IOTK_WORKAROUND7
        call iotk_reshape_INTEGER3( size(dat),dat,tmpdat )
#else
        dat = reshape(tmpdat,shape(dat))
#endif
# 671 "iotk_dat.spp"
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
# 683 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
# 683 "iotk_dat.spp"
call iotk_error_msg(ierrl,'Dat not found')
# 683 "iotk_dat.spp"
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
end subroutine iotk_scan_dat_INTEGER3_3


#endif
#endif

subroutine iotk_dat_dummy_INTEGER3_3
  write(0,*)
end subroutine iotk_dat_dummy_INTEGER3_3


# 727 "iotk_dat.spp"


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

#ifdef __IOTK_INTEGER3
#if 4 <= __IOTK_MAXRANK
# 82 "iotk_dat.spp"
subroutine iotk_write_dat_INTEGER3_4(unit,name,dat,dummy,attr,columns,sep,fmt,ierr)
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
  INTEGER (kind=__IOTK_INTEGER3), intent(in)  :: dat (:,:,:,:) 
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
# 114 "iotk_dat.spp"
  INTEGER (kind=__IOTK_INTEGER3),allocatable :: dattmp(:)
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
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
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
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
    goto 1
  end if
  call iotk_strcpy(usefmt,"!",ierrl)
  if(ierrl/=0) then
    call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 142 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
    goto 1
  end if
  if(present(fmt) .and. .not. raw) call iotk_strcpy(usefmt,iotk_strtrim(fmt),ierrl)
  if(ierrl/=0) then
    call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 147 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
    goto 1
  end if
  if(iotk_strscan(usefmt,"<>&")/=0) then
    call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 151 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
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
  call iotk_write_attr(lattr,"type",iotk_tolower("INTEGER"),first=.true.,ierr=ierrl)
  if(ierrl/=0) then
    call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 156 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
    goto 1
  end if
  call iotk_write_attr(lattr,"size",size(dat),ierr=ierrl)
  if(ierrl/=0) then
    call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 161 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
    goto 1
  end if
# 171 "iotk_dat.spp"
  if(binary) then
    call iotk_write_attr(lattr,"kind",kind(dat),ierr=ierrl)
    if(ierrl/=0) then
      call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 174 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
      goto 1
    end if
  end if
# 179 "iotk_dat.spp"
  if(.not.iotk_strcomp(usefmt,"!")) call iotk_write_attr(lattr,"fmt",iotk_strtrim(usefmt),ierr=ierrl)
  if(ierrl/=0) then
    call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 181 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
    goto 1
  end if
  if(lcolumns/=1) call iotk_write_attr(lattr,"columns",lcolumns,ierr=ierrl)
  if(ierrl/=0) then
    call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 186 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
    goto 1
  end if
  if(present(attr)) then
    attr_tmp(1:1)=iotk_eos
    call iotk_strcpy(attr_tmp,attr,ierr=ierrl)
    if(ierrl/=0) then
      call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 193 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
      goto 1
    end if
    call iotk_delete_attr(attr_tmp,"type",ierrl)
    if(ierrl/=0) then
      call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 198 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
      goto 1
    end if
    call iotk_delete_attr(attr_tmp,"kind",ierrl)
    if(ierrl/=0) then
      call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 203 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
      goto 1
    end if
    call iotk_delete_attr(attr_tmp,"size",ierrl)
    if(ierrl/=0) then
      call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 208 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
      goto 1
    end if
    call iotk_delete_attr(attr_tmp,"fmt",ierrl)
    if(ierrl/=0) then
      call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 213 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
      goto 1
    end if
    call iotk_delete_attr(attr_tmp,"columns",ierrl)
    if(ierrl/=0) then
      call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 218 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
      goto 1
    end if
    call iotk_delete_attr(attr_tmp,"len",ierrl)
    if(ierrl/=0) then
      call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 223 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
      goto 1
    end if
    if(iotk_strlen_trim(attr_tmp)>0) call iotk_strcat(lattr,iotk_strtrim(attr_tmp),ierr=ierrl)
  end if
  if(ierrl/=0) then
    call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 229 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
    goto 1
  end if
  call iotk_write_begin(unit,name,lattr,ierr=ierrl)
  if(ierrl/=0) then
    call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 234 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
    goto 1
  end if

  allocate(dattmp(size(dat)))
# 242 "iotk_dat.spp"
#if defined(__IOTK_WORKAROUND3) || defined(__IOTK_WORKAROUND4)
# 246 "iotk_dat.spp"
     call iotk_private_pack_INTEGER3(dattmp,dat,size(dattmp),1)
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
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
        goto 1
      end if
    else
      write(lunit,iostat=iostat) idummy,(dattmp(itmp),itmp=1,size(dattmp))
      if(iostat/=0) then
        call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 263 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
        goto 1
      end if
    end if
  else
    if(raw) then
# 272 "iotk_dat.spp"
      write(lunit,*,iostat=iostat) (dattmp(itmp),itmp=1,size(dattmp))
# 274 "iotk_dat.spp"
      if(iostat/=0) then
        call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 275 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
        goto 1
      end if
    else if(iotk_strcomp(usefmt,"*")) then
      write(lunit,*,iostat=iostat) (dattmp(itmp),itmp=1,size(dattmp))
      if(iostat/=0) then
        call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 281 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
        goto 1
      end if
    else if(iotk_strcomp(usefmt,"!")) then
# 295 "iotk_dat.spp"
     write(lunit,fmt=trim(iotk_wfmt("INTEGER",kind(dattmp),lcolumns,-1,lsep)),iostat=iostat) (dattmp(itmp),itmp=1,size(dattmp))
     if(iostat/=0) then
      call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 297 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
      goto 1
     end if
# 301 "iotk_dat.spp"
    else
      write(lunit,fmt=usefmt(1:iotk_strlen(usefmt)),iostat=iostat) (dattmp(itmp),itmp=1,size(dattmp))
      if(iostat/=0) then
        call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 304 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
        goto 1
      end if
    end if
  end if
  call iotk_write_end(unit,name,ierr=ierrl)
  if(ierrl/=0) then
    call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 311 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
    goto 1
  end if
1 continue
  if(allocated(dattmp)) deallocate(dattmp)
  if(present(ierr)) then
    ierr = ierrl
  else
    if(ierrl/=0) call iotk_error_handler(ierrl)
  end if
end subroutine iotk_write_dat_INTEGER3_4


# 589 "iotk_dat.spp"

# 591 "iotk_dat.spp"
subroutine iotk_scan_dat_INTEGER3_4(unit,name,dat,dummy,attr,found,default,ierr)
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
  INTEGER (kind=__IOTK_INTEGER3)                        :: dat (:,:,:,:)
#else
  INTEGER (kind=__IOTK_INTEGER3),           intent(out) :: dat (:,:,:,:)
#endif
  type(iotk_dummytype), optional         :: dummy
#ifdef __IOTK_WORKAROUND6
  character(len=*),optional              :: attr
#else
  character(len=*),optional, intent(out) :: attr
#endif
  logical,         optional, intent(out) :: found
  INTEGER (kind=__IOTK_INTEGER3), optional, intent(in)  :: default (:,:,:,:)
  integer,         optional, intent(out) :: ierr
# 618 "iotk_dat.spp"
  INTEGER (kind=__IOTK_INTEGER3),              allocatable :: tmpdat(:)
# 620 "iotk_dat.spp"
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
# 637 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
    goto 1
  end if
  call iotk_parse_dat(lattr,rtype,rkind,rsize,rlen,fmt,columns,ierrl)
! Note that columns is not effectively used
  if(ierrl/=0) goto 1
  if(.not. (iotk_strcomp(rtype,iotk_eos) .or. iotk_strcomp(rtype,"INTEGER") ) ) then
    call iotk_error_issue(ierrl,"iotk_scan_dat",__FILE__,__LINE__)
# 644 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
# 644 "iotk_dat.spp"
call iotk_error_msg(ierrl,' ')
# 644 "iotk_dat.spp"
call iotk_error_write(ierrl,"rtype",rtype(1:iotk_strlen(rtype)))
# 644 "iotk_dat.spp"
call iotk_error_write(ierrl,"type","INTEGER")
    goto 1
  end if
  if(.not. (rsize==-1 .or. rsize==size(dat)) ) then
    call iotk_error_issue(ierrl,"iotk_scan_dat",__FILE__,__LINE__)
# 648 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
    goto 1
  end if
  if(rkind==-1) rkind = kind(dat)
# 655 "iotk_dat.spp"

  allocate(tmpdat(size(dat)))
  call iotk_scan_dat_aux(unit,tmpdat,rkind,rlen,fmt(1:iotk_strlen(fmt)),ierrl)
# 662 "iotk_dat.spp"
#ifdef __IOTK_WORKAROUND7
        call iotk_reshape_INTEGER3( size(dat),dat,tmpdat )
#else
        dat = reshape(tmpdat,shape(dat))
#endif
# 671 "iotk_dat.spp"
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
# 683 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
# 683 "iotk_dat.spp"
call iotk_error_msg(ierrl,'Dat not found')
# 683 "iotk_dat.spp"
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
end subroutine iotk_scan_dat_INTEGER3_4


#endif
#endif

subroutine iotk_dat_dummy_INTEGER3_4
  write(0,*)
end subroutine iotk_dat_dummy_INTEGER3_4


# 727 "iotk_dat.spp"


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

#ifdef __IOTK_INTEGER3
#if 5 <= __IOTK_MAXRANK
# 82 "iotk_dat.spp"
subroutine iotk_write_dat_INTEGER3_5(unit,name,dat,dummy,attr,columns,sep,fmt,ierr)
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
  INTEGER (kind=__IOTK_INTEGER3), intent(in)  :: dat (:,:,:,:,:) 
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
# 114 "iotk_dat.spp"
  INTEGER (kind=__IOTK_INTEGER3),allocatable :: dattmp(:)
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
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
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
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
    goto 1
  end if
  call iotk_strcpy(usefmt,"!",ierrl)
  if(ierrl/=0) then
    call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 142 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
    goto 1
  end if
  if(present(fmt) .and. .not. raw) call iotk_strcpy(usefmt,iotk_strtrim(fmt),ierrl)
  if(ierrl/=0) then
    call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 147 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
    goto 1
  end if
  if(iotk_strscan(usefmt,"<>&")/=0) then
    call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 151 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
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
  call iotk_write_attr(lattr,"type",iotk_tolower("INTEGER"),first=.true.,ierr=ierrl)
  if(ierrl/=0) then
    call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 156 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
    goto 1
  end if
  call iotk_write_attr(lattr,"size",size(dat),ierr=ierrl)
  if(ierrl/=0) then
    call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 161 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
    goto 1
  end if
# 171 "iotk_dat.spp"
  if(binary) then
    call iotk_write_attr(lattr,"kind",kind(dat),ierr=ierrl)
    if(ierrl/=0) then
      call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 174 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
      goto 1
    end if
  end if
# 179 "iotk_dat.spp"
  if(.not.iotk_strcomp(usefmt,"!")) call iotk_write_attr(lattr,"fmt",iotk_strtrim(usefmt),ierr=ierrl)
  if(ierrl/=0) then
    call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 181 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
    goto 1
  end if
  if(lcolumns/=1) call iotk_write_attr(lattr,"columns",lcolumns,ierr=ierrl)
  if(ierrl/=0) then
    call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 186 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
    goto 1
  end if
  if(present(attr)) then
    attr_tmp(1:1)=iotk_eos
    call iotk_strcpy(attr_tmp,attr,ierr=ierrl)
    if(ierrl/=0) then
      call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 193 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
      goto 1
    end if
    call iotk_delete_attr(attr_tmp,"type",ierrl)
    if(ierrl/=0) then
      call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 198 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
      goto 1
    end if
    call iotk_delete_attr(attr_tmp,"kind",ierrl)
    if(ierrl/=0) then
      call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 203 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
      goto 1
    end if
    call iotk_delete_attr(attr_tmp,"size",ierrl)
    if(ierrl/=0) then
      call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 208 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
      goto 1
    end if
    call iotk_delete_attr(attr_tmp,"fmt",ierrl)
    if(ierrl/=0) then
      call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 213 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
      goto 1
    end if
    call iotk_delete_attr(attr_tmp,"columns",ierrl)
    if(ierrl/=0) then
      call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 218 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
      goto 1
    end if
    call iotk_delete_attr(attr_tmp,"len",ierrl)
    if(ierrl/=0) then
      call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 223 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
      goto 1
    end if
    if(iotk_strlen_trim(attr_tmp)>0) call iotk_strcat(lattr,iotk_strtrim(attr_tmp),ierr=ierrl)
  end if
  if(ierrl/=0) then
    call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 229 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
    goto 1
  end if
  call iotk_write_begin(unit,name,lattr,ierr=ierrl)
  if(ierrl/=0) then
    call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 234 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
    goto 1
  end if

  allocate(dattmp(size(dat)))
# 242 "iotk_dat.spp"
#if defined(__IOTK_WORKAROUND3) || defined(__IOTK_WORKAROUND4)
# 246 "iotk_dat.spp"
     call iotk_private_pack_INTEGER3(dattmp,dat,size(dattmp),1)
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
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
        goto 1
      end if
    else
      write(lunit,iostat=iostat) idummy,(dattmp(itmp),itmp=1,size(dattmp))
      if(iostat/=0) then
        call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 263 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
        goto 1
      end if
    end if
  else
    if(raw) then
# 272 "iotk_dat.spp"
      write(lunit,*,iostat=iostat) (dattmp(itmp),itmp=1,size(dattmp))
# 274 "iotk_dat.spp"
      if(iostat/=0) then
        call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 275 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
        goto 1
      end if
    else if(iotk_strcomp(usefmt,"*")) then
      write(lunit,*,iostat=iostat) (dattmp(itmp),itmp=1,size(dattmp))
      if(iostat/=0) then
        call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 281 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
        goto 1
      end if
    else if(iotk_strcomp(usefmt,"!")) then
# 295 "iotk_dat.spp"
     write(lunit,fmt=trim(iotk_wfmt("INTEGER",kind(dattmp),lcolumns,-1,lsep)),iostat=iostat) (dattmp(itmp),itmp=1,size(dattmp))
     if(iostat/=0) then
      call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 297 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
      goto 1
     end if
# 301 "iotk_dat.spp"
    else
      write(lunit,fmt=usefmt(1:iotk_strlen(usefmt)),iostat=iostat) (dattmp(itmp),itmp=1,size(dattmp))
      if(iostat/=0) then
        call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 304 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
        goto 1
      end if
    end if
  end if
  call iotk_write_end(unit,name,ierr=ierrl)
  if(ierrl/=0) then
    call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 311 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
    goto 1
  end if
1 continue
  if(allocated(dattmp)) deallocate(dattmp)
  if(present(ierr)) then
    ierr = ierrl
  else
    if(ierrl/=0) call iotk_error_handler(ierrl)
  end if
end subroutine iotk_write_dat_INTEGER3_5


# 589 "iotk_dat.spp"

# 591 "iotk_dat.spp"
subroutine iotk_scan_dat_INTEGER3_5(unit,name,dat,dummy,attr,found,default,ierr)
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
  INTEGER (kind=__IOTK_INTEGER3)                        :: dat (:,:,:,:,:)
#else
  INTEGER (kind=__IOTK_INTEGER3),           intent(out) :: dat (:,:,:,:,:)
#endif
  type(iotk_dummytype), optional         :: dummy
#ifdef __IOTK_WORKAROUND6
  character(len=*),optional              :: attr
#else
  character(len=*),optional, intent(out) :: attr
#endif
  logical,         optional, intent(out) :: found
  INTEGER (kind=__IOTK_INTEGER3), optional, intent(in)  :: default (:,:,:,:,:)
  integer,         optional, intent(out) :: ierr
# 618 "iotk_dat.spp"
  INTEGER (kind=__IOTK_INTEGER3),              allocatable :: tmpdat(:)
# 620 "iotk_dat.spp"
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
# 637 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
    goto 1
  end if
  call iotk_parse_dat(lattr,rtype,rkind,rsize,rlen,fmt,columns,ierrl)
! Note that columns is not effectively used
  if(ierrl/=0) goto 1
  if(.not. (iotk_strcomp(rtype,iotk_eos) .or. iotk_strcomp(rtype,"INTEGER") ) ) then
    call iotk_error_issue(ierrl,"iotk_scan_dat",__FILE__,__LINE__)
# 644 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
# 644 "iotk_dat.spp"
call iotk_error_msg(ierrl,' ')
# 644 "iotk_dat.spp"
call iotk_error_write(ierrl,"rtype",rtype(1:iotk_strlen(rtype)))
# 644 "iotk_dat.spp"
call iotk_error_write(ierrl,"type","INTEGER")
    goto 1
  end if
  if(.not. (rsize==-1 .or. rsize==size(dat)) ) then
    call iotk_error_issue(ierrl,"iotk_scan_dat",__FILE__,__LINE__)
# 648 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
    goto 1
  end if
  if(rkind==-1) rkind = kind(dat)
# 655 "iotk_dat.spp"

  allocate(tmpdat(size(dat)))
  call iotk_scan_dat_aux(unit,tmpdat,rkind,rlen,fmt(1:iotk_strlen(fmt)),ierrl)
# 662 "iotk_dat.spp"
#ifdef __IOTK_WORKAROUND7
        call iotk_reshape_INTEGER3( size(dat),dat,tmpdat )
#else
        dat = reshape(tmpdat,shape(dat))
#endif
# 671 "iotk_dat.spp"
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
# 683 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
# 683 "iotk_dat.spp"
call iotk_error_msg(ierrl,'Dat not found')
# 683 "iotk_dat.spp"
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
end subroutine iotk_scan_dat_INTEGER3_5


#endif
#endif

subroutine iotk_dat_dummy_INTEGER3_5
  write(0,*)
end subroutine iotk_dat_dummy_INTEGER3_5


# 727 "iotk_dat.spp"


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

#ifdef __IOTK_INTEGER3
#if 6 <= __IOTK_MAXRANK
# 82 "iotk_dat.spp"
subroutine iotk_write_dat_INTEGER3_6(unit,name,dat,dummy,attr,columns,sep,fmt,ierr)
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
  INTEGER (kind=__IOTK_INTEGER3), intent(in)  :: dat (:,:,:,:,:,:) 
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
# 114 "iotk_dat.spp"
  INTEGER (kind=__IOTK_INTEGER3),allocatable :: dattmp(:)
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
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
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
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
    goto 1
  end if
  call iotk_strcpy(usefmt,"!",ierrl)
  if(ierrl/=0) then
    call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 142 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
    goto 1
  end if
  if(present(fmt) .and. .not. raw) call iotk_strcpy(usefmt,iotk_strtrim(fmt),ierrl)
  if(ierrl/=0) then
    call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 147 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
    goto 1
  end if
  if(iotk_strscan(usefmt,"<>&")/=0) then
    call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 151 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
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
  call iotk_write_attr(lattr,"type",iotk_tolower("INTEGER"),first=.true.,ierr=ierrl)
  if(ierrl/=0) then
    call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 156 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
    goto 1
  end if
  call iotk_write_attr(lattr,"size",size(dat),ierr=ierrl)
  if(ierrl/=0) then
    call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 161 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
    goto 1
  end if
# 171 "iotk_dat.spp"
  if(binary) then
    call iotk_write_attr(lattr,"kind",kind(dat),ierr=ierrl)
    if(ierrl/=0) then
      call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 174 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
      goto 1
    end if
  end if
# 179 "iotk_dat.spp"
  if(.not.iotk_strcomp(usefmt,"!")) call iotk_write_attr(lattr,"fmt",iotk_strtrim(usefmt),ierr=ierrl)
  if(ierrl/=0) then
    call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 181 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
    goto 1
  end if
  if(lcolumns/=1) call iotk_write_attr(lattr,"columns",lcolumns,ierr=ierrl)
  if(ierrl/=0) then
    call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 186 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
    goto 1
  end if
  if(present(attr)) then
    attr_tmp(1:1)=iotk_eos
    call iotk_strcpy(attr_tmp,attr,ierr=ierrl)
    if(ierrl/=0) then
      call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 193 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
      goto 1
    end if
    call iotk_delete_attr(attr_tmp,"type",ierrl)
    if(ierrl/=0) then
      call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 198 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
      goto 1
    end if
    call iotk_delete_attr(attr_tmp,"kind",ierrl)
    if(ierrl/=0) then
      call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 203 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
      goto 1
    end if
    call iotk_delete_attr(attr_tmp,"size",ierrl)
    if(ierrl/=0) then
      call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 208 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
      goto 1
    end if
    call iotk_delete_attr(attr_tmp,"fmt",ierrl)
    if(ierrl/=0) then
      call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 213 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
      goto 1
    end if
    call iotk_delete_attr(attr_tmp,"columns",ierrl)
    if(ierrl/=0) then
      call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 218 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
      goto 1
    end if
    call iotk_delete_attr(attr_tmp,"len",ierrl)
    if(ierrl/=0) then
      call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 223 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
      goto 1
    end if
    if(iotk_strlen_trim(attr_tmp)>0) call iotk_strcat(lattr,iotk_strtrim(attr_tmp),ierr=ierrl)
  end if
  if(ierrl/=0) then
    call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 229 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
    goto 1
  end if
  call iotk_write_begin(unit,name,lattr,ierr=ierrl)
  if(ierrl/=0) then
    call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 234 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
    goto 1
  end if

  allocate(dattmp(size(dat)))
# 242 "iotk_dat.spp"
#if defined(__IOTK_WORKAROUND3) || defined(__IOTK_WORKAROUND4)
# 246 "iotk_dat.spp"
     call iotk_private_pack_INTEGER3(dattmp,dat,size(dattmp),1)
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
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
        goto 1
      end if
    else
      write(lunit,iostat=iostat) idummy,(dattmp(itmp),itmp=1,size(dattmp))
      if(iostat/=0) then
        call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 263 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
        goto 1
      end if
    end if
  else
    if(raw) then
# 272 "iotk_dat.spp"
      write(lunit,*,iostat=iostat) (dattmp(itmp),itmp=1,size(dattmp))
# 274 "iotk_dat.spp"
      if(iostat/=0) then
        call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 275 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
        goto 1
      end if
    else if(iotk_strcomp(usefmt,"*")) then
      write(lunit,*,iostat=iostat) (dattmp(itmp),itmp=1,size(dattmp))
      if(iostat/=0) then
        call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 281 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
        goto 1
      end if
    else if(iotk_strcomp(usefmt,"!")) then
# 295 "iotk_dat.spp"
     write(lunit,fmt=trim(iotk_wfmt("INTEGER",kind(dattmp),lcolumns,-1,lsep)),iostat=iostat) (dattmp(itmp),itmp=1,size(dattmp))
     if(iostat/=0) then
      call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 297 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
      goto 1
     end if
# 301 "iotk_dat.spp"
    else
      write(lunit,fmt=usefmt(1:iotk_strlen(usefmt)),iostat=iostat) (dattmp(itmp),itmp=1,size(dattmp))
      if(iostat/=0) then
        call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 304 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
        goto 1
      end if
    end if
  end if
  call iotk_write_end(unit,name,ierr=ierrl)
  if(ierrl/=0) then
    call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 311 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
    goto 1
  end if
1 continue
  if(allocated(dattmp)) deallocate(dattmp)
  if(present(ierr)) then
    ierr = ierrl
  else
    if(ierrl/=0) call iotk_error_handler(ierrl)
  end if
end subroutine iotk_write_dat_INTEGER3_6


# 589 "iotk_dat.spp"

# 591 "iotk_dat.spp"
subroutine iotk_scan_dat_INTEGER3_6(unit,name,dat,dummy,attr,found,default,ierr)
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
  INTEGER (kind=__IOTK_INTEGER3)                        :: dat (:,:,:,:,:,:)
#else
  INTEGER (kind=__IOTK_INTEGER3),           intent(out) :: dat (:,:,:,:,:,:)
#endif
  type(iotk_dummytype), optional         :: dummy
#ifdef __IOTK_WORKAROUND6
  character(len=*),optional              :: attr
#else
  character(len=*),optional, intent(out) :: attr
#endif
  logical,         optional, intent(out) :: found
  INTEGER (kind=__IOTK_INTEGER3), optional, intent(in)  :: default (:,:,:,:,:,:)
  integer,         optional, intent(out) :: ierr
# 618 "iotk_dat.spp"
  INTEGER (kind=__IOTK_INTEGER3),              allocatable :: tmpdat(:)
# 620 "iotk_dat.spp"
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
# 637 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
    goto 1
  end if
  call iotk_parse_dat(lattr,rtype,rkind,rsize,rlen,fmt,columns,ierrl)
! Note that columns is not effectively used
  if(ierrl/=0) goto 1
  if(.not. (iotk_strcomp(rtype,iotk_eos) .or. iotk_strcomp(rtype,"INTEGER") ) ) then
    call iotk_error_issue(ierrl,"iotk_scan_dat",__FILE__,__LINE__)
# 644 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
# 644 "iotk_dat.spp"
call iotk_error_msg(ierrl,' ')
# 644 "iotk_dat.spp"
call iotk_error_write(ierrl,"rtype",rtype(1:iotk_strlen(rtype)))
# 644 "iotk_dat.spp"
call iotk_error_write(ierrl,"type","INTEGER")
    goto 1
  end if
  if(.not. (rsize==-1 .or. rsize==size(dat)) ) then
    call iotk_error_issue(ierrl,"iotk_scan_dat",__FILE__,__LINE__)
# 648 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
    goto 1
  end if
  if(rkind==-1) rkind = kind(dat)
# 655 "iotk_dat.spp"

  allocate(tmpdat(size(dat)))
  call iotk_scan_dat_aux(unit,tmpdat,rkind,rlen,fmt(1:iotk_strlen(fmt)),ierrl)
# 662 "iotk_dat.spp"
#ifdef __IOTK_WORKAROUND7
        call iotk_reshape_INTEGER3( size(dat),dat,tmpdat )
#else
        dat = reshape(tmpdat,shape(dat))
#endif
# 671 "iotk_dat.spp"
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
# 683 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
# 683 "iotk_dat.spp"
call iotk_error_msg(ierrl,'Dat not found')
# 683 "iotk_dat.spp"
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
end subroutine iotk_scan_dat_INTEGER3_6


#endif
#endif

subroutine iotk_dat_dummy_INTEGER3_6
  write(0,*)
end subroutine iotk_dat_dummy_INTEGER3_6


# 727 "iotk_dat.spp"


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

#ifdef __IOTK_INTEGER3
#if 7 <= __IOTK_MAXRANK
# 82 "iotk_dat.spp"
subroutine iotk_write_dat_INTEGER3_7(unit,name,dat,dummy,attr,columns,sep,fmt,ierr)
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
  INTEGER (kind=__IOTK_INTEGER3), intent(in)  :: dat (:,:,:,:,:,:,:) 
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
# 114 "iotk_dat.spp"
  INTEGER (kind=__IOTK_INTEGER3),allocatable :: dattmp(:)
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
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
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
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
    goto 1
  end if
  call iotk_strcpy(usefmt,"!",ierrl)
  if(ierrl/=0) then
    call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 142 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
    goto 1
  end if
  if(present(fmt) .and. .not. raw) call iotk_strcpy(usefmt,iotk_strtrim(fmt),ierrl)
  if(ierrl/=0) then
    call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 147 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
    goto 1
  end if
  if(iotk_strscan(usefmt,"<>&")/=0) then
    call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 151 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
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
  call iotk_write_attr(lattr,"type",iotk_tolower("INTEGER"),first=.true.,ierr=ierrl)
  if(ierrl/=0) then
    call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 156 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
    goto 1
  end if
  call iotk_write_attr(lattr,"size",size(dat),ierr=ierrl)
  if(ierrl/=0) then
    call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 161 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
    goto 1
  end if
# 171 "iotk_dat.spp"
  if(binary) then
    call iotk_write_attr(lattr,"kind",kind(dat),ierr=ierrl)
    if(ierrl/=0) then
      call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 174 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
      goto 1
    end if
  end if
# 179 "iotk_dat.spp"
  if(.not.iotk_strcomp(usefmt,"!")) call iotk_write_attr(lattr,"fmt",iotk_strtrim(usefmt),ierr=ierrl)
  if(ierrl/=0) then
    call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 181 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
    goto 1
  end if
  if(lcolumns/=1) call iotk_write_attr(lattr,"columns",lcolumns,ierr=ierrl)
  if(ierrl/=0) then
    call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 186 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
    goto 1
  end if
  if(present(attr)) then
    attr_tmp(1:1)=iotk_eos
    call iotk_strcpy(attr_tmp,attr,ierr=ierrl)
    if(ierrl/=0) then
      call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 193 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
      goto 1
    end if
    call iotk_delete_attr(attr_tmp,"type",ierrl)
    if(ierrl/=0) then
      call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 198 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
      goto 1
    end if
    call iotk_delete_attr(attr_tmp,"kind",ierrl)
    if(ierrl/=0) then
      call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 203 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
      goto 1
    end if
    call iotk_delete_attr(attr_tmp,"size",ierrl)
    if(ierrl/=0) then
      call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 208 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
      goto 1
    end if
    call iotk_delete_attr(attr_tmp,"fmt",ierrl)
    if(ierrl/=0) then
      call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 213 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
      goto 1
    end if
    call iotk_delete_attr(attr_tmp,"columns",ierrl)
    if(ierrl/=0) then
      call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 218 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
      goto 1
    end if
    call iotk_delete_attr(attr_tmp,"len",ierrl)
    if(ierrl/=0) then
      call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 223 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
      goto 1
    end if
    if(iotk_strlen_trim(attr_tmp)>0) call iotk_strcat(lattr,iotk_strtrim(attr_tmp),ierr=ierrl)
  end if
  if(ierrl/=0) then
    call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 229 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
    goto 1
  end if
  call iotk_write_begin(unit,name,lattr,ierr=ierrl)
  if(ierrl/=0) then
    call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 234 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
    goto 1
  end if

  allocate(dattmp(size(dat)))
# 242 "iotk_dat.spp"
#if defined(__IOTK_WORKAROUND3) || defined(__IOTK_WORKAROUND4)
# 246 "iotk_dat.spp"
     call iotk_private_pack_INTEGER3(dattmp,dat,size(dattmp),1)
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
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
        goto 1
      end if
    else
      write(lunit,iostat=iostat) idummy,(dattmp(itmp),itmp=1,size(dattmp))
      if(iostat/=0) then
        call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 263 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
        goto 1
      end if
    end if
  else
    if(raw) then
# 272 "iotk_dat.spp"
      write(lunit,*,iostat=iostat) (dattmp(itmp),itmp=1,size(dattmp))
# 274 "iotk_dat.spp"
      if(iostat/=0) then
        call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 275 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
        goto 1
      end if
    else if(iotk_strcomp(usefmt,"*")) then
      write(lunit,*,iostat=iostat) (dattmp(itmp),itmp=1,size(dattmp))
      if(iostat/=0) then
        call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 281 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
        goto 1
      end if
    else if(iotk_strcomp(usefmt,"!")) then
# 295 "iotk_dat.spp"
     write(lunit,fmt=trim(iotk_wfmt("INTEGER",kind(dattmp),lcolumns,-1,lsep)),iostat=iostat) (dattmp(itmp),itmp=1,size(dattmp))
     if(iostat/=0) then
      call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 297 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
      goto 1
     end if
# 301 "iotk_dat.spp"
    else
      write(lunit,fmt=usefmt(1:iotk_strlen(usefmt)),iostat=iostat) (dattmp(itmp),itmp=1,size(dattmp))
      if(iostat/=0) then
        call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 304 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
        goto 1
      end if
    end if
  end if
  call iotk_write_end(unit,name,ierr=ierrl)
  if(ierrl/=0) then
    call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 311 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
    goto 1
  end if
1 continue
  if(allocated(dattmp)) deallocate(dattmp)
  if(present(ierr)) then
    ierr = ierrl
  else
    if(ierrl/=0) call iotk_error_handler(ierrl)
  end if
end subroutine iotk_write_dat_INTEGER3_7


# 589 "iotk_dat.spp"

# 591 "iotk_dat.spp"
subroutine iotk_scan_dat_INTEGER3_7(unit,name,dat,dummy,attr,found,default,ierr)
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
  INTEGER (kind=__IOTK_INTEGER3)                        :: dat (:,:,:,:,:,:,:)
#else
  INTEGER (kind=__IOTK_INTEGER3),           intent(out) :: dat (:,:,:,:,:,:,:)
#endif
  type(iotk_dummytype), optional         :: dummy
#ifdef __IOTK_WORKAROUND6
  character(len=*),optional              :: attr
#else
  character(len=*),optional, intent(out) :: attr
#endif
  logical,         optional, intent(out) :: found
  INTEGER (kind=__IOTK_INTEGER3), optional, intent(in)  :: default (:,:,:,:,:,:,:)
  integer,         optional, intent(out) :: ierr
# 618 "iotk_dat.spp"
  INTEGER (kind=__IOTK_INTEGER3),              allocatable :: tmpdat(:)
# 620 "iotk_dat.spp"
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
# 637 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
    goto 1
  end if
  call iotk_parse_dat(lattr,rtype,rkind,rsize,rlen,fmt,columns,ierrl)
! Note that columns is not effectively used
  if(ierrl/=0) goto 1
  if(.not. (iotk_strcomp(rtype,iotk_eos) .or. iotk_strcomp(rtype,"INTEGER") ) ) then
    call iotk_error_issue(ierrl,"iotk_scan_dat",__FILE__,__LINE__)
# 644 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
# 644 "iotk_dat.spp"
call iotk_error_msg(ierrl,' ')
# 644 "iotk_dat.spp"
call iotk_error_write(ierrl,"rtype",rtype(1:iotk_strlen(rtype)))
# 644 "iotk_dat.spp"
call iotk_error_write(ierrl,"type","INTEGER")
    goto 1
  end if
  if(.not. (rsize==-1 .or. rsize==size(dat)) ) then
    call iotk_error_issue(ierrl,"iotk_scan_dat",__FILE__,__LINE__)
# 648 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
    goto 1
  end if
  if(rkind==-1) rkind = kind(dat)
# 655 "iotk_dat.spp"

  allocate(tmpdat(size(dat)))
  call iotk_scan_dat_aux(unit,tmpdat,rkind,rlen,fmt(1:iotk_strlen(fmt)),ierrl)
# 662 "iotk_dat.spp"
#ifdef __IOTK_WORKAROUND7
        call iotk_reshape_INTEGER3( size(dat),dat,tmpdat )
#else
        dat = reshape(tmpdat,shape(dat))
#endif
# 671 "iotk_dat.spp"
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
# 683 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
# 683 "iotk_dat.spp"
call iotk_error_msg(ierrl,'Dat not found')
# 683 "iotk_dat.spp"
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
end subroutine iotk_scan_dat_INTEGER3_7


#endif
#endif

subroutine iotk_dat_dummy_INTEGER3_7
  write(0,*)
end subroutine iotk_dat_dummy_INTEGER3_7


# 727 "iotk_dat.spp"


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

#ifdef __IOTK_INTEGER4
#if 0 <= __IOTK_MAXRANK
# 82 "iotk_dat.spp"
subroutine iotk_write_dat_INTEGER4_0(unit,name,dat,dummy,attr,columns,sep,fmt,ierr)
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
  INTEGER (kind=__IOTK_INTEGER4), intent(in)  :: dat  
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
# 114 "iotk_dat.spp"
  INTEGER (kind=__IOTK_INTEGER4),allocatable :: dattmp(:)
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
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
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
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
    goto 1
  end if
  call iotk_strcpy(usefmt,"!",ierrl)
  if(ierrl/=0) then
    call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 142 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
    goto 1
  end if
  if(present(fmt) .and. .not. raw) call iotk_strcpy(usefmt,iotk_strtrim(fmt),ierrl)
  if(ierrl/=0) then
    call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 147 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
    goto 1
  end if
  if(iotk_strscan(usefmt,"<>&")/=0) then
    call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 151 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
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
  call iotk_write_attr(lattr,"type",iotk_tolower("INTEGER"),first=.true.,ierr=ierrl)
  if(ierrl/=0) then
    call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 156 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
    goto 1
  end if
  call iotk_write_attr(lattr,"size",1,ierr=ierrl)
  if(ierrl/=0) then
    call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 161 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
    goto 1
  end if
# 171 "iotk_dat.spp"
  if(binary) then
    call iotk_write_attr(lattr,"kind",kind(dat),ierr=ierrl)
    if(ierrl/=0) then
      call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 174 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
      goto 1
    end if
  end if
# 179 "iotk_dat.spp"
  if(.not.iotk_strcomp(usefmt,"!")) call iotk_write_attr(lattr,"fmt",iotk_strtrim(usefmt),ierr=ierrl)
  if(ierrl/=0) then
    call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 181 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
    goto 1
  end if
  if(lcolumns/=1) call iotk_write_attr(lattr,"columns",lcolumns,ierr=ierrl)
  if(ierrl/=0) then
    call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 186 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
    goto 1
  end if
  if(present(attr)) then
    attr_tmp(1:1)=iotk_eos
    call iotk_strcpy(attr_tmp,attr,ierr=ierrl)
    if(ierrl/=0) then
      call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 193 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
      goto 1
    end if
    call iotk_delete_attr(attr_tmp,"type",ierrl)
    if(ierrl/=0) then
      call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 198 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
      goto 1
    end if
    call iotk_delete_attr(attr_tmp,"kind",ierrl)
    if(ierrl/=0) then
      call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 203 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
      goto 1
    end if
    call iotk_delete_attr(attr_tmp,"size",ierrl)
    if(ierrl/=0) then
      call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 208 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
      goto 1
    end if
    call iotk_delete_attr(attr_tmp,"fmt",ierrl)
    if(ierrl/=0) then
      call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 213 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
      goto 1
    end if
    call iotk_delete_attr(attr_tmp,"columns",ierrl)
    if(ierrl/=0) then
      call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 218 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
      goto 1
    end if
    call iotk_delete_attr(attr_tmp,"len",ierrl)
    if(ierrl/=0) then
      call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 223 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
      goto 1
    end if
    if(iotk_strlen_trim(attr_tmp)>0) call iotk_strcat(lattr,iotk_strtrim(attr_tmp),ierr=ierrl)
  end if
  if(ierrl/=0) then
    call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 229 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
    goto 1
  end if
  call iotk_write_begin(unit,name,lattr,ierr=ierrl)
  if(ierrl/=0) then
    call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 234 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
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
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
        goto 1
      end if
    else
      write(lunit,iostat=iostat) idummy,(dattmp(itmp),itmp=1,size(dattmp))
      if(iostat/=0) then
        call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 263 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
        goto 1
      end if
    end if
  else
    if(raw) then
# 272 "iotk_dat.spp"
      write(lunit,*,iostat=iostat) (dattmp(itmp),itmp=1,size(dattmp))
# 274 "iotk_dat.spp"
      if(iostat/=0) then
        call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 275 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
        goto 1
      end if
    else if(iotk_strcomp(usefmt,"*")) then
      write(lunit,*,iostat=iostat) (dattmp(itmp),itmp=1,size(dattmp))
      if(iostat/=0) then
        call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 281 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
        goto 1
      end if
    else if(iotk_strcomp(usefmt,"!")) then
# 295 "iotk_dat.spp"
     write(lunit,fmt=trim(iotk_wfmt("INTEGER",kind(dattmp),lcolumns,-1,lsep)),iostat=iostat) (dattmp(itmp),itmp=1,size(dattmp))
     if(iostat/=0) then
      call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 297 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
      goto 1
     end if
# 301 "iotk_dat.spp"
    else
      write(lunit,fmt=usefmt(1:iotk_strlen(usefmt)),iostat=iostat) (dattmp(itmp),itmp=1,size(dattmp))
      if(iostat/=0) then
        call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 304 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
        goto 1
      end if
    end if
  end if
  call iotk_write_end(unit,name,ierr=ierrl)
  if(ierrl/=0) then
    call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 311 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
    goto 1
  end if
1 continue
  if(allocated(dattmp)) deallocate(dattmp)
  if(present(ierr)) then
    ierr = ierrl
  else
    if(ierrl/=0) call iotk_error_handler(ierrl)
  end if
end subroutine iotk_write_dat_INTEGER4_0


# 589 "iotk_dat.spp"

# 591 "iotk_dat.spp"
subroutine iotk_scan_dat_INTEGER4_0(unit,name,dat,dummy,attr,found,default,ierr)
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
  INTEGER (kind=__IOTK_INTEGER4)                        :: dat 
#else
  INTEGER (kind=__IOTK_INTEGER4),           intent(out) :: dat 
#endif
  type(iotk_dummytype), optional         :: dummy
#ifdef __IOTK_WORKAROUND6
  character(len=*),optional              :: attr
#else
  character(len=*),optional, intent(out) :: attr
#endif
  logical,         optional, intent(out) :: found
  INTEGER (kind=__IOTK_INTEGER4), optional, intent(in)  :: default 
  integer,         optional, intent(out) :: ierr
# 618 "iotk_dat.spp"
  INTEGER (kind=__IOTK_INTEGER4),              allocatable :: tmpdat(:)
# 620 "iotk_dat.spp"
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
# 637 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
    goto 1
  end if
  call iotk_parse_dat(lattr,rtype,rkind,rsize,rlen,fmt,columns,ierrl)
! Note that columns is not effectively used
  if(ierrl/=0) goto 1
  if(.not. (iotk_strcomp(rtype,iotk_eos) .or. iotk_strcomp(rtype,"INTEGER") ) ) then
    call iotk_error_issue(ierrl,"iotk_scan_dat",__FILE__,__LINE__)
# 644 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
# 644 "iotk_dat.spp"
call iotk_error_msg(ierrl,' ')
# 644 "iotk_dat.spp"
call iotk_error_write(ierrl,"rtype",rtype(1:iotk_strlen(rtype)))
# 644 "iotk_dat.spp"
call iotk_error_write(ierrl,"type","INTEGER")
    goto 1
  end if
  if(.not. (rsize==-1 .or. rsize==1) ) then
    call iotk_error_issue(ierrl,"iotk_scan_dat",__FILE__,__LINE__)
# 648 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
    goto 1
  end if
  if(rkind==-1) rkind = kind(dat)
# 655 "iotk_dat.spp"

  allocate(tmpdat(1))
  call iotk_scan_dat_aux(unit,tmpdat,rkind,rlen,fmt(1:iotk_strlen(fmt)),ierrl)
# 659 "iotk_dat.spp"
        dat = tmpdat(1)
# 671 "iotk_dat.spp"
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
# 683 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
# 683 "iotk_dat.spp"
call iotk_error_msg(ierrl,'Dat not found')
# 683 "iotk_dat.spp"
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
end subroutine iotk_scan_dat_INTEGER4_0


#endif
#endif

subroutine iotk_dat_dummy_INTEGER4_0
  write(0,*)
end subroutine iotk_dat_dummy_INTEGER4_0


# 727 "iotk_dat.spp"


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

#ifdef __IOTK_INTEGER4
#if 1 <= __IOTK_MAXRANK
# 82 "iotk_dat.spp"
subroutine iotk_write_dat_INTEGER4_1(unit,name,dat,dummy,attr,columns,sep,fmt,ierr)
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
  INTEGER (kind=__IOTK_INTEGER4), intent(in)  :: dat (:) 
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
# 114 "iotk_dat.spp"
  INTEGER (kind=__IOTK_INTEGER4),allocatable :: dattmp(:)
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
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
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
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
    goto 1
  end if
  call iotk_strcpy(usefmt,"!",ierrl)
  if(ierrl/=0) then
    call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 142 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
    goto 1
  end if
  if(present(fmt) .and. .not. raw) call iotk_strcpy(usefmt,iotk_strtrim(fmt),ierrl)
  if(ierrl/=0) then
    call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 147 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
    goto 1
  end if
  if(iotk_strscan(usefmt,"<>&")/=0) then
    call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 151 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
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
  call iotk_write_attr(lattr,"type",iotk_tolower("INTEGER"),first=.true.,ierr=ierrl)
  if(ierrl/=0) then
    call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 156 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
    goto 1
  end if
  call iotk_write_attr(lattr,"size",size(dat),ierr=ierrl)
  if(ierrl/=0) then
    call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 161 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
    goto 1
  end if
# 171 "iotk_dat.spp"
  if(binary) then
    call iotk_write_attr(lattr,"kind",kind(dat),ierr=ierrl)
    if(ierrl/=0) then
      call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 174 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
      goto 1
    end if
  end if
# 179 "iotk_dat.spp"
  if(.not.iotk_strcomp(usefmt,"!")) call iotk_write_attr(lattr,"fmt",iotk_strtrim(usefmt),ierr=ierrl)
  if(ierrl/=0) then
    call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 181 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
    goto 1
  end if
  if(lcolumns/=1) call iotk_write_attr(lattr,"columns",lcolumns,ierr=ierrl)
  if(ierrl/=0) then
    call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 186 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
    goto 1
  end if
  if(present(attr)) then
    attr_tmp(1:1)=iotk_eos
    call iotk_strcpy(attr_tmp,attr,ierr=ierrl)
    if(ierrl/=0) then
      call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 193 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
      goto 1
    end if
    call iotk_delete_attr(attr_tmp,"type",ierrl)
    if(ierrl/=0) then
      call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 198 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
      goto 1
    end if
    call iotk_delete_attr(attr_tmp,"kind",ierrl)
    if(ierrl/=0) then
      call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 203 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
      goto 1
    end if
    call iotk_delete_attr(attr_tmp,"size",ierrl)
    if(ierrl/=0) then
      call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 208 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
      goto 1
    end if
    call iotk_delete_attr(attr_tmp,"fmt",ierrl)
    if(ierrl/=0) then
      call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 213 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
      goto 1
    end if
    call iotk_delete_attr(attr_tmp,"columns",ierrl)
    if(ierrl/=0) then
      call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 218 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
      goto 1
    end if
    call iotk_delete_attr(attr_tmp,"len",ierrl)
    if(ierrl/=0) then
      call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 223 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
      goto 1
    end if
    if(iotk_strlen_trim(attr_tmp)>0) call iotk_strcat(lattr,iotk_strtrim(attr_tmp),ierr=ierrl)
  end if
  if(ierrl/=0) then
    call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 229 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
    goto 1
  end if
  call iotk_write_begin(unit,name,lattr,ierr=ierrl)
  if(ierrl/=0) then
    call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 234 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
    goto 1
  end if

  allocate(dattmp(size(dat)))
# 242 "iotk_dat.spp"
#if defined(__IOTK_WORKAROUND3) || defined(__IOTK_WORKAROUND4)
# 246 "iotk_dat.spp"
     call iotk_private_pack_INTEGER4(dattmp,dat,size(dattmp),1)
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
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
        goto 1
      end if
    else
      write(lunit,iostat=iostat) idummy,(dattmp(itmp),itmp=1,size(dattmp))
      if(iostat/=0) then
        call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 263 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
        goto 1
      end if
    end if
  else
    if(raw) then
# 272 "iotk_dat.spp"
      write(lunit,*,iostat=iostat) (dattmp(itmp),itmp=1,size(dattmp))
# 274 "iotk_dat.spp"
      if(iostat/=0) then
        call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 275 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
        goto 1
      end if
    else if(iotk_strcomp(usefmt,"*")) then
      write(lunit,*,iostat=iostat) (dattmp(itmp),itmp=1,size(dattmp))
      if(iostat/=0) then
        call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 281 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
        goto 1
      end if
    else if(iotk_strcomp(usefmt,"!")) then
# 295 "iotk_dat.spp"
     write(lunit,fmt=trim(iotk_wfmt("INTEGER",kind(dattmp),lcolumns,-1,lsep)),iostat=iostat) (dattmp(itmp),itmp=1,size(dattmp))
     if(iostat/=0) then
      call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 297 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
      goto 1
     end if
# 301 "iotk_dat.spp"
    else
      write(lunit,fmt=usefmt(1:iotk_strlen(usefmt)),iostat=iostat) (dattmp(itmp),itmp=1,size(dattmp))
      if(iostat/=0) then
        call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 304 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
        goto 1
      end if
    end if
  end if
  call iotk_write_end(unit,name,ierr=ierrl)
  if(ierrl/=0) then
    call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 311 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
    goto 1
  end if
1 continue
  if(allocated(dattmp)) deallocate(dattmp)
  if(present(ierr)) then
    ierr = ierrl
  else
    if(ierrl/=0) call iotk_error_handler(ierrl)
  end if
end subroutine iotk_write_dat_INTEGER4_1


# 326 "iotk_dat.spp"
recursive subroutine iotk_scan_dat_aux_INTEGER4(unit,dat,rkind,rlen,fmt,ierr)
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
  INTEGER (kind=__IOTK_INTEGER4)                        :: dat (:)
#else
  INTEGER (kind=__IOTK_INTEGER4),           intent(out) :: dat (:)
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
# 356 "iotk_dat.spp"
#ifdef __IOTK_INTEGER1
  INTEGER (__IOTK_INTEGER1), allocatable :: dat1 (:)
#endif
# 356 "iotk_dat.spp"
#ifdef __IOTK_INTEGER2
  INTEGER (__IOTK_INTEGER2), allocatable :: dat2 (:)
#endif
# 356 "iotk_dat.spp"
#ifdef __IOTK_INTEGER3
  INTEGER (__IOTK_INTEGER3), allocatable :: dat3 (:)
#endif
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
call iotk_error_msg(ierr,"CVS Revision: 1.17 ")
    return
  end if
# 463 "iotk_dat.spp"
  if(binary) then
    select case(rkind)
    case(kind(dat))
      if(raw) then
        read(lunit,iostat=iostat) ( dat(i), i=1,ubound(dat,1) )
        if(iostat/=0) then
          call iotk_error_issue(ierr,"iotk_scan_dat_aux",__FILE__,__LINE__)
# 469 "iotk_dat.spp"
call iotk_error_msg(ierr,"CVS Revision: 1.17 ")
# 469 "iotk_dat.spp"
call iotk_error_msg(ierr,' ')
# 469 "iotk_dat.spp"
call iotk_error_write(ierr,"iostat",iostat)
          return
        end if
      else
        read(lunit,iostat=iostat) idummy, ( dat(i), i=1,ubound(dat,1) )
        if(iostat/=0) then
          call iotk_error_issue(ierr,"iotk_scan_dat_aux",__FILE__,__LINE__)
# 475 "iotk_dat.spp"
call iotk_error_msg(ierr,"CVS Revision: 1.17 ")
# 475 "iotk_dat.spp"
call iotk_error_msg(ierr,' ')
# 475 "iotk_dat.spp"
call iotk_error_write(ierr,"iostat",iostat)
          return
        end if
      end if
# 481 "iotk_dat.spp"
#ifdef __IOTK_INTEGER1
    case(kind(dat1))
      ! Giusto per scrupolo. Se e' raw non ci sono info sul kind, quindi questa linea e' irraggiungibile
      if(raw) then
        call iotk_error_issue(ierr,"iotk_scan_dat_aux",__FILE__,__LINE__)
# 485 "iotk_dat.spp"
call iotk_error_msg(ierr,"CVS Revision: 1.17 ")
        return
      end if
      allocate(dat1(ubound(dat,1)))
      read(lunit,iostat=iostat) idummy,( dat1(i), i=1,ubound(dat1,1) )
      if(iostat/=0) then
        call iotk_error_issue(ierr,"iotk_scan_dat_aux",__FILE__,__LINE__)
# 491 "iotk_dat.spp"
call iotk_error_msg(ierr,"CVS Revision: 1.17 ")
# 491 "iotk_dat.spp"
call iotk_error_msg(ierr,' ')
# 491 "iotk_dat.spp"
call iotk_error_write(ierr,"iostat",iostat)
        return
      end if
# 501 "iotk_dat.spp"
      dat = dat1
# 503 "iotk_dat.spp"
      deallocate(dat1)
#endif
# 481 "iotk_dat.spp"
#ifdef __IOTK_INTEGER2
    case(kind(dat2))
      ! Giusto per scrupolo. Se e' raw non ci sono info sul kind, quindi questa linea e' irraggiungibile
      if(raw) then
        call iotk_error_issue(ierr,"iotk_scan_dat_aux",__FILE__,__LINE__)
# 485 "iotk_dat.spp"
call iotk_error_msg(ierr,"CVS Revision: 1.17 ")
        return
      end if
      allocate(dat2(ubound(dat,1)))
      read(lunit,iostat=iostat) idummy,( dat2(i), i=1,ubound(dat2,1) )
      if(iostat/=0) then
        call iotk_error_issue(ierr,"iotk_scan_dat_aux",__FILE__,__LINE__)
# 491 "iotk_dat.spp"
call iotk_error_msg(ierr,"CVS Revision: 1.17 ")
# 491 "iotk_dat.spp"
call iotk_error_msg(ierr,' ')
# 491 "iotk_dat.spp"
call iotk_error_write(ierr,"iostat",iostat)
        return
      end if
# 501 "iotk_dat.spp"
      dat = dat2
# 503 "iotk_dat.spp"
      deallocate(dat2)
#endif
# 481 "iotk_dat.spp"
#ifdef __IOTK_INTEGER3
    case(kind(dat3))
      ! Giusto per scrupolo. Se e' raw non ci sono info sul kind, quindi questa linea e' irraggiungibile
      if(raw) then
        call iotk_error_issue(ierr,"iotk_scan_dat_aux",__FILE__,__LINE__)
# 485 "iotk_dat.spp"
call iotk_error_msg(ierr,"CVS Revision: 1.17 ")
        return
      end if
      allocate(dat3(ubound(dat,1)))
      read(lunit,iostat=iostat) idummy,( dat3(i), i=1,ubound(dat3,1) )
      if(iostat/=0) then
        call iotk_error_issue(ierr,"iotk_scan_dat_aux",__FILE__,__LINE__)
# 491 "iotk_dat.spp"
call iotk_error_msg(ierr,"CVS Revision: 1.17 ")
# 491 "iotk_dat.spp"
call iotk_error_msg(ierr,' ')
# 491 "iotk_dat.spp"
call iotk_error_write(ierr,"iostat",iostat)
        return
      end if
# 501 "iotk_dat.spp"
      dat = dat3
# 503 "iotk_dat.spp"
      deallocate(dat3)
#endif
# 507 "iotk_dat.spp"
    case default
      call iotk_error_issue(ierr,"iotk_scan_dat_aux",__FILE__,__LINE__)
# 508 "iotk_dat.spp"
call iotk_error_msg(ierr,"CVS Revision: 1.17 ")
# 508 "iotk_dat.spp"
call iotk_error_msg(ierr,'Kind incompatibility')
# 508 "iotk_dat.spp"
call iotk_error_write(ierr,"kind",rkind)
    end select
  else
    if(raw) then
      read(lunit,fmt=*,iostat=iostat) ( dat(i), i=1,ubound(dat,1) )
      if(iostat/=0) then
        call iotk_error_issue(ierr,"iotk_scan_dat_aux",__FILE__,__LINE__)
# 514 "iotk_dat.spp"
call iotk_error_msg(ierr,"CVS Revision: 1.17 ")
# 514 "iotk_dat.spp"
call iotk_error_msg(ierr,' ')
# 514 "iotk_dat.spp"
call iotk_error_write(ierr,"iostat",iostat)
        return
      end if
    else if(iotk_strcomp(fmt,"*")) then
      read(lunit,fmt=*,iostat=iostat) ( dat(i), i=1,ubound(dat,1) )
      if(iostat/=0) then
        call iotk_error_issue(ierr,"iotk_scan_dat_aux",__FILE__,__LINE__)
# 520 "iotk_dat.spp"
call iotk_error_msg(ierr,"CVS Revision: 1.17 ")
# 520 "iotk_dat.spp"
call iotk_error_msg(ierr,' ')
# 520 "iotk_dat.spp"
call iotk_error_write(ierr,"iostat",iostat)
        return
      end if
    else if(iotk_strcomp(fmt,"!")) then
      index = 0
      do
        call iotk_getline(lunit,line,length,ierr)
        if(ierr/=0) then
          call iotk_error_issue(ierr,"iotk_scan_dat_aux",__FILE__,__LINE__)
# 528 "iotk_dat.spp"
call iotk_error_msg(ierr,"CVS Revision: 1.17 ")
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
# 539 "iotk_dat.spp"
call iotk_error_msg(ierr,"CVS Revision: 1.17 ")
# 539 "iotk_dat.spp"
call iotk_error_msg(ierr,' ')
# 539 "iotk_dat.spp"
call iotk_error_write(ierr,"iostat",iostat)
            return
          end if
          call iotk_getline(lunit,altline,altlength,ierr)
          if(ierr/=0) then
            call iotk_error_issue(ierr,"iotk_scan_dat_aux",__FILE__,__LINE__)
# 544 "iotk_dat.spp"
call iotk_error_msg(ierr,"CVS Revision: 1.17 ")
            return
          end if
          backspace(lunit,iostat=iostat)
          if(iostat/=0) then
            call iotk_error_issue(ierr,"iotk_scan_dat_aux",__FILE__,__LINE__)
# 549 "iotk_dat.spp"
call iotk_error_msg(ierr,"CVS Revision: 1.17 ")
# 549 "iotk_dat.spp"
call iotk_error_msg(ierr,' ')
# 549 "iotk_dat.spp"
call iotk_error_write(ierr,"iostat",iostat)
            return
          end if
          read(lunit,"(a)",advance="no",iostat=iostat) altline(1:nexttag-1 + altlength - length)
          if(iostat/=0) then
            call iotk_error_issue(ierr,"iotk_scan_dat_aux",__FILE__,__LINE__)
# 554 "iotk_dat.spp"
call iotk_error_msg(ierr,"CVS Revision: 1.17 ")
# 554 "iotk_dat.spp"
call iotk_error_msg(ierr,' ')
# 554 "iotk_dat.spp"
call iotk_error_write(ierr,"iostat",iostat)
            return
          end if
        end if
        call iotk_str_clean(line(1:nexttag - 1))
        call iotk_read(dat,line(1:nexttag - 1),index,ierr)
        if(ierr/=0) then
          call iotk_error_issue(ierr,"iotk_scan_dat_aux",__FILE__,__LINE__)
# 561 "iotk_dat.spp"
call iotk_error_msg(ierr,"CVS Revision: 1.17 ")
          return
        end if
# 567 "iotk_dat.spp"
        if(index == size(dat)) exit
# 569 "iotk_dat.spp"
        if(nexttag/=length + 1) then
          call iotk_error_issue(ierr,"iotk_scan_dat_aux",__FILE__,__LINE__)
# 570 "iotk_dat.spp"
call iotk_error_msg(ierr,"CVS Revision: 1.17 ")
          return
        end if
      end do
    else
      read(lunit,fmt=fmt(1:iotk_strlen(fmt)),iostat=iostat) ( dat(i), i=1,ubound(dat,1) )
      if(iostat/=0) then
        call iotk_error_issue(ierr,"iotk_scan_dat_aux",__FILE__,__LINE__)
# 577 "iotk_dat.spp"
call iotk_error_msg(ierr,"CVS Revision: 1.17 ")
# 577 "iotk_dat.spp"
call iotk_error_msg(ierr,' ')
# 577 "iotk_dat.spp"
call iotk_error_write(ierr,"iostat",iostat)
        return
      end if
    end if
  end if
# 583 "iotk_dat.spp"
  if(idummy/=0) then
    call iotk_error_issue(ierr,"iotk_scan_dat_aux",__FILE__,__LINE__)
# 584 "iotk_dat.spp"
call iotk_error_msg(ierr,"CVS Revision: 1.17 ")
    return
  end if
end subroutine iotk_scan_dat_aux_INTEGER4
# 589 "iotk_dat.spp"

# 591 "iotk_dat.spp"
subroutine iotk_scan_dat_INTEGER4_1(unit,name,dat,dummy,attr,found,default,ierr)
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
  INTEGER (kind=__IOTK_INTEGER4)                        :: dat (:)
#else
  INTEGER (kind=__IOTK_INTEGER4),           intent(out) :: dat (:)
#endif
  type(iotk_dummytype), optional         :: dummy
#ifdef __IOTK_WORKAROUND6
  character(len=*),optional              :: attr
#else
  character(len=*),optional, intent(out) :: attr
#endif
  logical,         optional, intent(out) :: found
  INTEGER (kind=__IOTK_INTEGER4), optional, intent(in)  :: default (:)
  integer,         optional, intent(out) :: ierr
# 618 "iotk_dat.spp"
  INTEGER (kind=__IOTK_INTEGER4),              allocatable :: tmpdat(:)
# 620 "iotk_dat.spp"
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
# 637 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
    goto 1
  end if
  call iotk_parse_dat(lattr,rtype,rkind,rsize,rlen,fmt,columns,ierrl)
! Note that columns is not effectively used
  if(ierrl/=0) goto 1
  if(.not. (iotk_strcomp(rtype,iotk_eos) .or. iotk_strcomp(rtype,"INTEGER") ) ) then
    call iotk_error_issue(ierrl,"iotk_scan_dat",__FILE__,__LINE__)
# 644 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
# 644 "iotk_dat.spp"
call iotk_error_msg(ierrl,' ')
# 644 "iotk_dat.spp"
call iotk_error_write(ierrl,"rtype",rtype(1:iotk_strlen(rtype)))
# 644 "iotk_dat.spp"
call iotk_error_write(ierrl,"type","INTEGER")
    goto 1
  end if
  if(.not. (rsize==-1 .or. rsize==size(dat)) ) then
    call iotk_error_issue(ierrl,"iotk_scan_dat",__FILE__,__LINE__)
# 648 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
    goto 1
  end if
  if(rkind==-1) rkind = kind(dat)
# 655 "iotk_dat.spp"

  allocate(tmpdat(size(dat)))
  call iotk_scan_dat_aux(unit,tmpdat,rkind,rlen,fmt(1:iotk_strlen(fmt)),ierrl)
# 662 "iotk_dat.spp"
#ifdef __IOTK_WORKAROUND7
        call iotk_reshape_INTEGER4( size(dat),dat,tmpdat )
#else
        dat = reshape(tmpdat,shape(dat))
#endif
# 671 "iotk_dat.spp"
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
# 683 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
# 683 "iotk_dat.spp"
call iotk_error_msg(ierrl,'Dat not found')
# 683 "iotk_dat.spp"
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
end subroutine iotk_scan_dat_INTEGER4_1


#endif
#endif

subroutine iotk_dat_dummy_INTEGER4_1
  write(0,*)
end subroutine iotk_dat_dummy_INTEGER4_1


# 706 "iotk_dat.spp"
!
! the following workaround is introduced for INTEL compiler < 9.0 
! having problem in treating INTEGER arrays with large dimensions
! within implicit loops
!
#ifdef __IOTK_WORKAROUND7

subroutine iotk_reshape_INTEGER4(n,dat_out,dat_in)
  implicit none
  integer, intent(in) :: n
  INTEGER (kind=__IOTK_INTEGER4), intent(in)  :: dat_in(n)
  INTEGER (kind=__IOTK_INTEGER4), intent(out) :: dat_out(n)
  integer :: i
  do i=1,n
     dat_out(i) = dat_in(i)
  enddo
end subroutine iotk_reshape_INTEGER4

#endif

# 727 "iotk_dat.spp"


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

#ifdef __IOTK_INTEGER4
#if 2 <= __IOTK_MAXRANK
# 82 "iotk_dat.spp"
subroutine iotk_write_dat_INTEGER4_2(unit,name,dat,dummy,attr,columns,sep,fmt,ierr)
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
  INTEGER (kind=__IOTK_INTEGER4), intent(in)  :: dat (:,:) 
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
# 114 "iotk_dat.spp"
  INTEGER (kind=__IOTK_INTEGER4),allocatable :: dattmp(:)
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
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
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
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
    goto 1
  end if
  call iotk_strcpy(usefmt,"!",ierrl)
  if(ierrl/=0) then
    call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 142 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
    goto 1
  end if
  if(present(fmt) .and. .not. raw) call iotk_strcpy(usefmt,iotk_strtrim(fmt),ierrl)
  if(ierrl/=0) then
    call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 147 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
    goto 1
  end if
  if(iotk_strscan(usefmt,"<>&")/=0) then
    call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 151 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
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
  call iotk_write_attr(lattr,"type",iotk_tolower("INTEGER"),first=.true.,ierr=ierrl)
  if(ierrl/=0) then
    call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 156 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
    goto 1
  end if
  call iotk_write_attr(lattr,"size",size(dat),ierr=ierrl)
  if(ierrl/=0) then
    call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 161 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
    goto 1
  end if
# 171 "iotk_dat.spp"
  if(binary) then
    call iotk_write_attr(lattr,"kind",kind(dat),ierr=ierrl)
    if(ierrl/=0) then
      call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 174 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
      goto 1
    end if
  end if
# 179 "iotk_dat.spp"
  if(.not.iotk_strcomp(usefmt,"!")) call iotk_write_attr(lattr,"fmt",iotk_strtrim(usefmt),ierr=ierrl)
  if(ierrl/=0) then
    call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 181 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
    goto 1
  end if
  if(lcolumns/=1) call iotk_write_attr(lattr,"columns",lcolumns,ierr=ierrl)
  if(ierrl/=0) then
    call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 186 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
    goto 1
  end if
  if(present(attr)) then
    attr_tmp(1:1)=iotk_eos
    call iotk_strcpy(attr_tmp,attr,ierr=ierrl)
    if(ierrl/=0) then
      call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 193 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
      goto 1
    end if
    call iotk_delete_attr(attr_tmp,"type",ierrl)
    if(ierrl/=0) then
      call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 198 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
      goto 1
    end if
    call iotk_delete_attr(attr_tmp,"kind",ierrl)
    if(ierrl/=0) then
      call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 203 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
      goto 1
    end if
    call iotk_delete_attr(attr_tmp,"size",ierrl)
    if(ierrl/=0) then
      call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 208 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
      goto 1
    end if
    call iotk_delete_attr(attr_tmp,"fmt",ierrl)
    if(ierrl/=0) then
      call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 213 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
      goto 1
    end if
    call iotk_delete_attr(attr_tmp,"columns",ierrl)
    if(ierrl/=0) then
      call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 218 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
      goto 1
    end if
    call iotk_delete_attr(attr_tmp,"len",ierrl)
    if(ierrl/=0) then
      call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 223 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
      goto 1
    end if
    if(iotk_strlen_trim(attr_tmp)>0) call iotk_strcat(lattr,iotk_strtrim(attr_tmp),ierr=ierrl)
  end if
  if(ierrl/=0) then
    call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 229 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
    goto 1
  end if
  call iotk_write_begin(unit,name,lattr,ierr=ierrl)
  if(ierrl/=0) then
    call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 234 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
    goto 1
  end if

  allocate(dattmp(size(dat)))
# 242 "iotk_dat.spp"
#if defined(__IOTK_WORKAROUND3) || defined(__IOTK_WORKAROUND4)
# 246 "iotk_dat.spp"
     call iotk_private_pack_INTEGER4(dattmp,dat,size(dattmp),1)
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
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
        goto 1
      end if
    else
      write(lunit,iostat=iostat) idummy,(dattmp(itmp),itmp=1,size(dattmp))
      if(iostat/=0) then
        call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 263 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
        goto 1
      end if
    end if
  else
    if(raw) then
# 272 "iotk_dat.spp"
      write(lunit,*,iostat=iostat) (dattmp(itmp),itmp=1,size(dattmp))
# 274 "iotk_dat.spp"
      if(iostat/=0) then
        call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 275 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
        goto 1
      end if
    else if(iotk_strcomp(usefmt,"*")) then
      write(lunit,*,iostat=iostat) (dattmp(itmp),itmp=1,size(dattmp))
      if(iostat/=0) then
        call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 281 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
        goto 1
      end if
    else if(iotk_strcomp(usefmt,"!")) then
# 295 "iotk_dat.spp"
     write(lunit,fmt=trim(iotk_wfmt("INTEGER",kind(dattmp),lcolumns,-1,lsep)),iostat=iostat) (dattmp(itmp),itmp=1,size(dattmp))
     if(iostat/=0) then
      call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 297 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
      goto 1
     end if
# 301 "iotk_dat.spp"
    else
      write(lunit,fmt=usefmt(1:iotk_strlen(usefmt)),iostat=iostat) (dattmp(itmp),itmp=1,size(dattmp))
      if(iostat/=0) then
        call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 304 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
        goto 1
      end if
    end if
  end if
  call iotk_write_end(unit,name,ierr=ierrl)
  if(ierrl/=0) then
    call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 311 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
    goto 1
  end if
1 continue
  if(allocated(dattmp)) deallocate(dattmp)
  if(present(ierr)) then
    ierr = ierrl
  else
    if(ierrl/=0) call iotk_error_handler(ierrl)
  end if
end subroutine iotk_write_dat_INTEGER4_2


# 589 "iotk_dat.spp"

# 591 "iotk_dat.spp"
subroutine iotk_scan_dat_INTEGER4_2(unit,name,dat,dummy,attr,found,default,ierr)
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
  INTEGER (kind=__IOTK_INTEGER4)                        :: dat (:,:)
#else
  INTEGER (kind=__IOTK_INTEGER4),           intent(out) :: dat (:,:)
#endif
  type(iotk_dummytype), optional         :: dummy
#ifdef __IOTK_WORKAROUND6
  character(len=*),optional              :: attr
#else
  character(len=*),optional, intent(out) :: attr
#endif
  logical,         optional, intent(out) :: found
  INTEGER (kind=__IOTK_INTEGER4), optional, intent(in)  :: default (:,:)
  integer,         optional, intent(out) :: ierr
# 618 "iotk_dat.spp"
  INTEGER (kind=__IOTK_INTEGER4),              allocatable :: tmpdat(:)
# 620 "iotk_dat.spp"
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
# 637 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
    goto 1
  end if
  call iotk_parse_dat(lattr,rtype,rkind,rsize,rlen,fmt,columns,ierrl)
! Note that columns is not effectively used
  if(ierrl/=0) goto 1
  if(.not. (iotk_strcomp(rtype,iotk_eos) .or. iotk_strcomp(rtype,"INTEGER") ) ) then
    call iotk_error_issue(ierrl,"iotk_scan_dat",__FILE__,__LINE__)
# 644 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
# 644 "iotk_dat.spp"
call iotk_error_msg(ierrl,' ')
# 644 "iotk_dat.spp"
call iotk_error_write(ierrl,"rtype",rtype(1:iotk_strlen(rtype)))
# 644 "iotk_dat.spp"
call iotk_error_write(ierrl,"type","INTEGER")
    goto 1
  end if
  if(.not. (rsize==-1 .or. rsize==size(dat)) ) then
    call iotk_error_issue(ierrl,"iotk_scan_dat",__FILE__,__LINE__)
# 648 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
    goto 1
  end if
  if(rkind==-1) rkind = kind(dat)
# 655 "iotk_dat.spp"

  allocate(tmpdat(size(dat)))
  call iotk_scan_dat_aux(unit,tmpdat,rkind,rlen,fmt(1:iotk_strlen(fmt)),ierrl)
# 662 "iotk_dat.spp"
#ifdef __IOTK_WORKAROUND7
        call iotk_reshape_INTEGER4( size(dat),dat,tmpdat )
#else
        dat = reshape(tmpdat,shape(dat))
#endif
# 671 "iotk_dat.spp"
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
# 683 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
# 683 "iotk_dat.spp"
call iotk_error_msg(ierrl,'Dat not found')
# 683 "iotk_dat.spp"
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
end subroutine iotk_scan_dat_INTEGER4_2


#endif
#endif

subroutine iotk_dat_dummy_INTEGER4_2
  write(0,*)
end subroutine iotk_dat_dummy_INTEGER4_2


# 727 "iotk_dat.spp"


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

#ifdef __IOTK_INTEGER4
#if 3 <= __IOTK_MAXRANK
# 82 "iotk_dat.spp"
subroutine iotk_write_dat_INTEGER4_3(unit,name,dat,dummy,attr,columns,sep,fmt,ierr)
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
  INTEGER (kind=__IOTK_INTEGER4), intent(in)  :: dat (:,:,:) 
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
# 114 "iotk_dat.spp"
  INTEGER (kind=__IOTK_INTEGER4),allocatable :: dattmp(:)
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
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
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
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
    goto 1
  end if
  call iotk_strcpy(usefmt,"!",ierrl)
  if(ierrl/=0) then
    call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 142 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
    goto 1
  end if
  if(present(fmt) .and. .not. raw) call iotk_strcpy(usefmt,iotk_strtrim(fmt),ierrl)
  if(ierrl/=0) then
    call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 147 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
    goto 1
  end if
  if(iotk_strscan(usefmt,"<>&")/=0) then
    call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 151 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
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
  call iotk_write_attr(lattr,"type",iotk_tolower("INTEGER"),first=.true.,ierr=ierrl)
  if(ierrl/=0) then
    call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 156 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
    goto 1
  end if
  call iotk_write_attr(lattr,"size",size(dat),ierr=ierrl)
  if(ierrl/=0) then
    call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 161 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
    goto 1
  end if
# 171 "iotk_dat.spp"
  if(binary) then
    call iotk_write_attr(lattr,"kind",kind(dat),ierr=ierrl)
    if(ierrl/=0) then
      call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 174 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
      goto 1
    end if
  end if
# 179 "iotk_dat.spp"
  if(.not.iotk_strcomp(usefmt,"!")) call iotk_write_attr(lattr,"fmt",iotk_strtrim(usefmt),ierr=ierrl)
  if(ierrl/=0) then
    call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 181 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
    goto 1
  end if
  if(lcolumns/=1) call iotk_write_attr(lattr,"columns",lcolumns,ierr=ierrl)
  if(ierrl/=0) then
    call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 186 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
    goto 1
  end if
  if(present(attr)) then
    attr_tmp(1:1)=iotk_eos
    call iotk_strcpy(attr_tmp,attr,ierr=ierrl)
    if(ierrl/=0) then
      call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 193 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
      goto 1
    end if
    call iotk_delete_attr(attr_tmp,"type",ierrl)
    if(ierrl/=0) then
      call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 198 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
      goto 1
    end if
    call iotk_delete_attr(attr_tmp,"kind",ierrl)
    if(ierrl/=0) then
      call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 203 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
      goto 1
    end if
    call iotk_delete_attr(attr_tmp,"size",ierrl)
    if(ierrl/=0) then
      call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 208 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
      goto 1
    end if
    call iotk_delete_attr(attr_tmp,"fmt",ierrl)
    if(ierrl/=0) then
      call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 213 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
      goto 1
    end if
    call iotk_delete_attr(attr_tmp,"columns",ierrl)
    if(ierrl/=0) then
      call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 218 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
      goto 1
    end if
    call iotk_delete_attr(attr_tmp,"len",ierrl)
    if(ierrl/=0) then
      call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 223 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
      goto 1
    end if
    if(iotk_strlen_trim(attr_tmp)>0) call iotk_strcat(lattr,iotk_strtrim(attr_tmp),ierr=ierrl)
  end if
  if(ierrl/=0) then
    call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 229 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
    goto 1
  end if
  call iotk_write_begin(unit,name,lattr,ierr=ierrl)
  if(ierrl/=0) then
    call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 234 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
    goto 1
  end if

  allocate(dattmp(size(dat)))
# 242 "iotk_dat.spp"
#if defined(__IOTK_WORKAROUND3) || defined(__IOTK_WORKAROUND4)
# 246 "iotk_dat.spp"
     call iotk_private_pack_INTEGER4(dattmp,dat,size(dattmp),1)
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
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
        goto 1
      end if
    else
      write(lunit,iostat=iostat) idummy,(dattmp(itmp),itmp=1,size(dattmp))
      if(iostat/=0) then
        call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 263 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
        goto 1
      end if
    end if
  else
    if(raw) then
# 272 "iotk_dat.spp"
      write(lunit,*,iostat=iostat) (dattmp(itmp),itmp=1,size(dattmp))
# 274 "iotk_dat.spp"
      if(iostat/=0) then
        call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 275 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
        goto 1
      end if
    else if(iotk_strcomp(usefmt,"*")) then
      write(lunit,*,iostat=iostat) (dattmp(itmp),itmp=1,size(dattmp))
      if(iostat/=0) then
        call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 281 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
        goto 1
      end if
    else if(iotk_strcomp(usefmt,"!")) then
# 295 "iotk_dat.spp"
     write(lunit,fmt=trim(iotk_wfmt("INTEGER",kind(dattmp),lcolumns,-1,lsep)),iostat=iostat) (dattmp(itmp),itmp=1,size(dattmp))
     if(iostat/=0) then
      call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 297 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
      goto 1
     end if
# 301 "iotk_dat.spp"
    else
      write(lunit,fmt=usefmt(1:iotk_strlen(usefmt)),iostat=iostat) (dattmp(itmp),itmp=1,size(dattmp))
      if(iostat/=0) then
        call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 304 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
        goto 1
      end if
    end if
  end if
  call iotk_write_end(unit,name,ierr=ierrl)
  if(ierrl/=0) then
    call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 311 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
    goto 1
  end if
1 continue
  if(allocated(dattmp)) deallocate(dattmp)
  if(present(ierr)) then
    ierr = ierrl
  else
    if(ierrl/=0) call iotk_error_handler(ierrl)
  end if
end subroutine iotk_write_dat_INTEGER4_3


# 589 "iotk_dat.spp"

# 591 "iotk_dat.spp"
subroutine iotk_scan_dat_INTEGER4_3(unit,name,dat,dummy,attr,found,default,ierr)
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
  INTEGER (kind=__IOTK_INTEGER4)                        :: dat (:,:,:)
#else
  INTEGER (kind=__IOTK_INTEGER4),           intent(out) :: dat (:,:,:)
#endif
  type(iotk_dummytype), optional         :: dummy
#ifdef __IOTK_WORKAROUND6
  character(len=*),optional              :: attr
#else
  character(len=*),optional, intent(out) :: attr
#endif
  logical,         optional, intent(out) :: found
  INTEGER (kind=__IOTK_INTEGER4), optional, intent(in)  :: default (:,:,:)
  integer,         optional, intent(out) :: ierr
# 618 "iotk_dat.spp"
  INTEGER (kind=__IOTK_INTEGER4),              allocatable :: tmpdat(:)
# 620 "iotk_dat.spp"
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
# 637 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
    goto 1
  end if
  call iotk_parse_dat(lattr,rtype,rkind,rsize,rlen,fmt,columns,ierrl)
! Note that columns is not effectively used
  if(ierrl/=0) goto 1
  if(.not. (iotk_strcomp(rtype,iotk_eos) .or. iotk_strcomp(rtype,"INTEGER") ) ) then
    call iotk_error_issue(ierrl,"iotk_scan_dat",__FILE__,__LINE__)
# 644 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
# 644 "iotk_dat.spp"
call iotk_error_msg(ierrl,' ')
# 644 "iotk_dat.spp"
call iotk_error_write(ierrl,"rtype",rtype(1:iotk_strlen(rtype)))
# 644 "iotk_dat.spp"
call iotk_error_write(ierrl,"type","INTEGER")
    goto 1
  end if
  if(.not. (rsize==-1 .or. rsize==size(dat)) ) then
    call iotk_error_issue(ierrl,"iotk_scan_dat",__FILE__,__LINE__)
# 648 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
    goto 1
  end if
  if(rkind==-1) rkind = kind(dat)
# 655 "iotk_dat.spp"

  allocate(tmpdat(size(dat)))
  call iotk_scan_dat_aux(unit,tmpdat,rkind,rlen,fmt(1:iotk_strlen(fmt)),ierrl)
# 662 "iotk_dat.spp"
#ifdef __IOTK_WORKAROUND7
        call iotk_reshape_INTEGER4( size(dat),dat,tmpdat )
#else
        dat = reshape(tmpdat,shape(dat))
#endif
# 671 "iotk_dat.spp"
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
# 683 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
# 683 "iotk_dat.spp"
call iotk_error_msg(ierrl,'Dat not found')
# 683 "iotk_dat.spp"
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
end subroutine iotk_scan_dat_INTEGER4_3


#endif
#endif

subroutine iotk_dat_dummy_INTEGER4_3
  write(0,*)
end subroutine iotk_dat_dummy_INTEGER4_3


# 727 "iotk_dat.spp"


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

#ifdef __IOTK_INTEGER4
#if 4 <= __IOTK_MAXRANK
# 82 "iotk_dat.spp"
subroutine iotk_write_dat_INTEGER4_4(unit,name,dat,dummy,attr,columns,sep,fmt,ierr)
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
  INTEGER (kind=__IOTK_INTEGER4), intent(in)  :: dat (:,:,:,:) 
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
# 114 "iotk_dat.spp"
  INTEGER (kind=__IOTK_INTEGER4),allocatable :: dattmp(:)
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
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
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
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
    goto 1
  end if
  call iotk_strcpy(usefmt,"!",ierrl)
  if(ierrl/=0) then
    call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 142 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
    goto 1
  end if
  if(present(fmt) .and. .not. raw) call iotk_strcpy(usefmt,iotk_strtrim(fmt),ierrl)
  if(ierrl/=0) then
    call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 147 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
    goto 1
  end if
  if(iotk_strscan(usefmt,"<>&")/=0) then
    call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 151 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
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
  call iotk_write_attr(lattr,"type",iotk_tolower("INTEGER"),first=.true.,ierr=ierrl)
  if(ierrl/=0) then
    call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 156 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
    goto 1
  end if
  call iotk_write_attr(lattr,"size",size(dat),ierr=ierrl)
  if(ierrl/=0) then
    call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 161 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
    goto 1
  end if
# 171 "iotk_dat.spp"
  if(binary) then
    call iotk_write_attr(lattr,"kind",kind(dat),ierr=ierrl)
    if(ierrl/=0) then
      call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 174 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
      goto 1
    end if
  end if
# 179 "iotk_dat.spp"
  if(.not.iotk_strcomp(usefmt,"!")) call iotk_write_attr(lattr,"fmt",iotk_strtrim(usefmt),ierr=ierrl)
  if(ierrl/=0) then
    call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 181 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
    goto 1
  end if
  if(lcolumns/=1) call iotk_write_attr(lattr,"columns",lcolumns,ierr=ierrl)
  if(ierrl/=0) then
    call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 186 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
    goto 1
  end if
  if(present(attr)) then
    attr_tmp(1:1)=iotk_eos
    call iotk_strcpy(attr_tmp,attr,ierr=ierrl)
    if(ierrl/=0) then
      call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 193 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
      goto 1
    end if
    call iotk_delete_attr(attr_tmp,"type",ierrl)
    if(ierrl/=0) then
      call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 198 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
      goto 1
    end if
    call iotk_delete_attr(attr_tmp,"kind",ierrl)
    if(ierrl/=0) then
      call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 203 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
      goto 1
    end if
    call iotk_delete_attr(attr_tmp,"size",ierrl)
    if(ierrl/=0) then
      call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 208 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
      goto 1
    end if
    call iotk_delete_attr(attr_tmp,"fmt",ierrl)
    if(ierrl/=0) then
      call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 213 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
      goto 1
    end if
    call iotk_delete_attr(attr_tmp,"columns",ierrl)
    if(ierrl/=0) then
      call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 218 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
      goto 1
    end if
    call iotk_delete_attr(attr_tmp,"len",ierrl)
    if(ierrl/=0) then
      call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 223 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
      goto 1
    end if
    if(iotk_strlen_trim(attr_tmp)>0) call iotk_strcat(lattr,iotk_strtrim(attr_tmp),ierr=ierrl)
  end if
  if(ierrl/=0) then
    call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 229 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
    goto 1
  end if
  call iotk_write_begin(unit,name,lattr,ierr=ierrl)
  if(ierrl/=0) then
    call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 234 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
    goto 1
  end if

  allocate(dattmp(size(dat)))
# 242 "iotk_dat.spp"
#if defined(__IOTK_WORKAROUND3) || defined(__IOTK_WORKAROUND4)
# 246 "iotk_dat.spp"
     call iotk_private_pack_INTEGER4(dattmp,dat,size(dattmp),1)
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
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
        goto 1
      end if
    else
      write(lunit,iostat=iostat) idummy,(dattmp(itmp),itmp=1,size(dattmp))
      if(iostat/=0) then
        call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 263 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
        goto 1
      end if
    end if
  else
    if(raw) then
# 272 "iotk_dat.spp"
      write(lunit,*,iostat=iostat) (dattmp(itmp),itmp=1,size(dattmp))
# 274 "iotk_dat.spp"
      if(iostat/=0) then
        call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 275 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
        goto 1
      end if
    else if(iotk_strcomp(usefmt,"*")) then
      write(lunit,*,iostat=iostat) (dattmp(itmp),itmp=1,size(dattmp))
      if(iostat/=0) then
        call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 281 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
        goto 1
      end if
    else if(iotk_strcomp(usefmt,"!")) then
# 295 "iotk_dat.spp"
     write(lunit,fmt=trim(iotk_wfmt("INTEGER",kind(dattmp),lcolumns,-1,lsep)),iostat=iostat) (dattmp(itmp),itmp=1,size(dattmp))
     if(iostat/=0) then
      call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 297 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
      goto 1
     end if
# 301 "iotk_dat.spp"
    else
      write(lunit,fmt=usefmt(1:iotk_strlen(usefmt)),iostat=iostat) (dattmp(itmp),itmp=1,size(dattmp))
      if(iostat/=0) then
        call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 304 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
        goto 1
      end if
    end if
  end if
  call iotk_write_end(unit,name,ierr=ierrl)
  if(ierrl/=0) then
    call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 311 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
    goto 1
  end if
1 continue
  if(allocated(dattmp)) deallocate(dattmp)
  if(present(ierr)) then
    ierr = ierrl
  else
    if(ierrl/=0) call iotk_error_handler(ierrl)
  end if
end subroutine iotk_write_dat_INTEGER4_4


# 589 "iotk_dat.spp"

# 591 "iotk_dat.spp"
subroutine iotk_scan_dat_INTEGER4_4(unit,name,dat,dummy,attr,found,default,ierr)
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
  INTEGER (kind=__IOTK_INTEGER4)                        :: dat (:,:,:,:)
#else
  INTEGER (kind=__IOTK_INTEGER4),           intent(out) :: dat (:,:,:,:)
#endif
  type(iotk_dummytype), optional         :: dummy
#ifdef __IOTK_WORKAROUND6
  character(len=*),optional              :: attr
#else
  character(len=*),optional, intent(out) :: attr
#endif
  logical,         optional, intent(out) :: found
  INTEGER (kind=__IOTK_INTEGER4), optional, intent(in)  :: default (:,:,:,:)
  integer,         optional, intent(out) :: ierr
# 618 "iotk_dat.spp"
  INTEGER (kind=__IOTK_INTEGER4),              allocatable :: tmpdat(:)
# 620 "iotk_dat.spp"
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
# 637 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
    goto 1
  end if
  call iotk_parse_dat(lattr,rtype,rkind,rsize,rlen,fmt,columns,ierrl)
! Note that columns is not effectively used
  if(ierrl/=0) goto 1
  if(.not. (iotk_strcomp(rtype,iotk_eos) .or. iotk_strcomp(rtype,"INTEGER") ) ) then
    call iotk_error_issue(ierrl,"iotk_scan_dat",__FILE__,__LINE__)
# 644 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
# 644 "iotk_dat.spp"
call iotk_error_msg(ierrl,' ')
# 644 "iotk_dat.spp"
call iotk_error_write(ierrl,"rtype",rtype(1:iotk_strlen(rtype)))
# 644 "iotk_dat.spp"
call iotk_error_write(ierrl,"type","INTEGER")
    goto 1
  end if
  if(.not. (rsize==-1 .or. rsize==size(dat)) ) then
    call iotk_error_issue(ierrl,"iotk_scan_dat",__FILE__,__LINE__)
# 648 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
    goto 1
  end if
  if(rkind==-1) rkind = kind(dat)
# 655 "iotk_dat.spp"

  allocate(tmpdat(size(dat)))
  call iotk_scan_dat_aux(unit,tmpdat,rkind,rlen,fmt(1:iotk_strlen(fmt)),ierrl)
# 662 "iotk_dat.spp"
#ifdef __IOTK_WORKAROUND7
        call iotk_reshape_INTEGER4( size(dat),dat,tmpdat )
#else
        dat = reshape(tmpdat,shape(dat))
#endif
# 671 "iotk_dat.spp"
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
# 683 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
# 683 "iotk_dat.spp"
call iotk_error_msg(ierrl,'Dat not found')
# 683 "iotk_dat.spp"
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
end subroutine iotk_scan_dat_INTEGER4_4


#endif
#endif

subroutine iotk_dat_dummy_INTEGER4_4
  write(0,*)
end subroutine iotk_dat_dummy_INTEGER4_4


# 727 "iotk_dat.spp"


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

#ifdef __IOTK_INTEGER4
#if 5 <= __IOTK_MAXRANK
# 82 "iotk_dat.spp"
subroutine iotk_write_dat_INTEGER4_5(unit,name,dat,dummy,attr,columns,sep,fmt,ierr)
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
  INTEGER (kind=__IOTK_INTEGER4), intent(in)  :: dat (:,:,:,:,:) 
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
# 114 "iotk_dat.spp"
  INTEGER (kind=__IOTK_INTEGER4),allocatable :: dattmp(:)
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
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
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
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
    goto 1
  end if
  call iotk_strcpy(usefmt,"!",ierrl)
  if(ierrl/=0) then
    call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 142 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
    goto 1
  end if
  if(present(fmt) .and. .not. raw) call iotk_strcpy(usefmt,iotk_strtrim(fmt),ierrl)
  if(ierrl/=0) then
    call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 147 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
    goto 1
  end if
  if(iotk_strscan(usefmt,"<>&")/=0) then
    call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 151 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
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
  call iotk_write_attr(lattr,"type",iotk_tolower("INTEGER"),first=.true.,ierr=ierrl)
  if(ierrl/=0) then
    call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 156 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
    goto 1
  end if
  call iotk_write_attr(lattr,"size",size(dat),ierr=ierrl)
  if(ierrl/=0) then
    call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 161 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
    goto 1
  end if
# 171 "iotk_dat.spp"
  if(binary) then
    call iotk_write_attr(lattr,"kind",kind(dat),ierr=ierrl)
    if(ierrl/=0) then
      call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 174 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
      goto 1
    end if
  end if
# 179 "iotk_dat.spp"
  if(.not.iotk_strcomp(usefmt,"!")) call iotk_write_attr(lattr,"fmt",iotk_strtrim(usefmt),ierr=ierrl)
  if(ierrl/=0) then
    call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 181 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
    goto 1
  end if
  if(lcolumns/=1) call iotk_write_attr(lattr,"columns",lcolumns,ierr=ierrl)
  if(ierrl/=0) then
    call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 186 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
    goto 1
  end if
  if(present(attr)) then
    attr_tmp(1:1)=iotk_eos
    call iotk_strcpy(attr_tmp,attr,ierr=ierrl)
    if(ierrl/=0) then
      call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 193 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
      goto 1
    end if
    call iotk_delete_attr(attr_tmp,"type",ierrl)
    if(ierrl/=0) then
      call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 198 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
      goto 1
    end if
    call iotk_delete_attr(attr_tmp,"kind",ierrl)
    if(ierrl/=0) then
      call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 203 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
      goto 1
    end if
    call iotk_delete_attr(attr_tmp,"size",ierrl)
    if(ierrl/=0) then
      call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 208 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
      goto 1
    end if
    call iotk_delete_attr(attr_tmp,"fmt",ierrl)
    if(ierrl/=0) then
      call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 213 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
      goto 1
    end if
    call iotk_delete_attr(attr_tmp,"columns",ierrl)
    if(ierrl/=0) then
      call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 218 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
      goto 1
    end if
    call iotk_delete_attr(attr_tmp,"len",ierrl)
    if(ierrl/=0) then
      call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 223 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
      goto 1
    end if
    if(iotk_strlen_trim(attr_tmp)>0) call iotk_strcat(lattr,iotk_strtrim(attr_tmp),ierr=ierrl)
  end if
  if(ierrl/=0) then
    call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 229 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
    goto 1
  end if
  call iotk_write_begin(unit,name,lattr,ierr=ierrl)
  if(ierrl/=0) then
    call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 234 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
    goto 1
  end if

  allocate(dattmp(size(dat)))
# 242 "iotk_dat.spp"
#if defined(__IOTK_WORKAROUND3) || defined(__IOTK_WORKAROUND4)
# 246 "iotk_dat.spp"
     call iotk_private_pack_INTEGER4(dattmp,dat,size(dattmp),1)
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
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
        goto 1
      end if
    else
      write(lunit,iostat=iostat) idummy,(dattmp(itmp),itmp=1,size(dattmp))
      if(iostat/=0) then
        call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 263 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
        goto 1
      end if
    end if
  else
    if(raw) then
# 272 "iotk_dat.spp"
      write(lunit,*,iostat=iostat) (dattmp(itmp),itmp=1,size(dattmp))
# 274 "iotk_dat.spp"
      if(iostat/=0) then
        call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 275 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
        goto 1
      end if
    else if(iotk_strcomp(usefmt,"*")) then
      write(lunit,*,iostat=iostat) (dattmp(itmp),itmp=1,size(dattmp))
      if(iostat/=0) then
        call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 281 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
        goto 1
      end if
    else if(iotk_strcomp(usefmt,"!")) then
# 295 "iotk_dat.spp"
     write(lunit,fmt=trim(iotk_wfmt("INTEGER",kind(dattmp),lcolumns,-1,lsep)),iostat=iostat) (dattmp(itmp),itmp=1,size(dattmp))
     if(iostat/=0) then
      call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 297 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
      goto 1
     end if
# 301 "iotk_dat.spp"
    else
      write(lunit,fmt=usefmt(1:iotk_strlen(usefmt)),iostat=iostat) (dattmp(itmp),itmp=1,size(dattmp))
      if(iostat/=0) then
        call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 304 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
        goto 1
      end if
    end if
  end if
  call iotk_write_end(unit,name,ierr=ierrl)
  if(ierrl/=0) then
    call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 311 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
    goto 1
  end if
1 continue
  if(allocated(dattmp)) deallocate(dattmp)
  if(present(ierr)) then
    ierr = ierrl
  else
    if(ierrl/=0) call iotk_error_handler(ierrl)
  end if
end subroutine iotk_write_dat_INTEGER4_5


# 589 "iotk_dat.spp"

# 591 "iotk_dat.spp"
subroutine iotk_scan_dat_INTEGER4_5(unit,name,dat,dummy,attr,found,default,ierr)
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
  INTEGER (kind=__IOTK_INTEGER4)                        :: dat (:,:,:,:,:)
#else
  INTEGER (kind=__IOTK_INTEGER4),           intent(out) :: dat (:,:,:,:,:)
#endif
  type(iotk_dummytype), optional         :: dummy
#ifdef __IOTK_WORKAROUND6
  character(len=*),optional              :: attr
#else
  character(len=*),optional, intent(out) :: attr
#endif
  logical,         optional, intent(out) :: found
  INTEGER (kind=__IOTK_INTEGER4), optional, intent(in)  :: default (:,:,:,:,:)
  integer,         optional, intent(out) :: ierr
# 618 "iotk_dat.spp"
  INTEGER (kind=__IOTK_INTEGER4),              allocatable :: tmpdat(:)
# 620 "iotk_dat.spp"
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
# 637 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
    goto 1
  end if
  call iotk_parse_dat(lattr,rtype,rkind,rsize,rlen,fmt,columns,ierrl)
! Note that columns is not effectively used
  if(ierrl/=0) goto 1
  if(.not. (iotk_strcomp(rtype,iotk_eos) .or. iotk_strcomp(rtype,"INTEGER") ) ) then
    call iotk_error_issue(ierrl,"iotk_scan_dat",__FILE__,__LINE__)
# 644 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
# 644 "iotk_dat.spp"
call iotk_error_msg(ierrl,' ')
# 644 "iotk_dat.spp"
call iotk_error_write(ierrl,"rtype",rtype(1:iotk_strlen(rtype)))
# 644 "iotk_dat.spp"
call iotk_error_write(ierrl,"type","INTEGER")
    goto 1
  end if
  if(.not. (rsize==-1 .or. rsize==size(dat)) ) then
    call iotk_error_issue(ierrl,"iotk_scan_dat",__FILE__,__LINE__)
# 648 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
    goto 1
  end if
  if(rkind==-1) rkind = kind(dat)
# 655 "iotk_dat.spp"

  allocate(tmpdat(size(dat)))
  call iotk_scan_dat_aux(unit,tmpdat,rkind,rlen,fmt(1:iotk_strlen(fmt)),ierrl)
# 662 "iotk_dat.spp"
#ifdef __IOTK_WORKAROUND7
        call iotk_reshape_INTEGER4( size(dat),dat,tmpdat )
#else
        dat = reshape(tmpdat,shape(dat))
#endif
# 671 "iotk_dat.spp"
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
# 683 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
# 683 "iotk_dat.spp"
call iotk_error_msg(ierrl,'Dat not found')
# 683 "iotk_dat.spp"
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
end subroutine iotk_scan_dat_INTEGER4_5


#endif
#endif

subroutine iotk_dat_dummy_INTEGER4_5
  write(0,*)
end subroutine iotk_dat_dummy_INTEGER4_5


# 727 "iotk_dat.spp"


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

#ifdef __IOTK_INTEGER4
#if 6 <= __IOTK_MAXRANK
# 82 "iotk_dat.spp"
subroutine iotk_write_dat_INTEGER4_6(unit,name,dat,dummy,attr,columns,sep,fmt,ierr)
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
  INTEGER (kind=__IOTK_INTEGER4), intent(in)  :: dat (:,:,:,:,:,:) 
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
# 114 "iotk_dat.spp"
  INTEGER (kind=__IOTK_INTEGER4),allocatable :: dattmp(:)
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
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
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
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
    goto 1
  end if
  call iotk_strcpy(usefmt,"!",ierrl)
  if(ierrl/=0) then
    call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 142 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
    goto 1
  end if
  if(present(fmt) .and. .not. raw) call iotk_strcpy(usefmt,iotk_strtrim(fmt),ierrl)
  if(ierrl/=0) then
    call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 147 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
    goto 1
  end if
  if(iotk_strscan(usefmt,"<>&")/=0) then
    call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 151 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
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
  call iotk_write_attr(lattr,"type",iotk_tolower("INTEGER"),first=.true.,ierr=ierrl)
  if(ierrl/=0) then
    call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 156 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
    goto 1
  end if
  call iotk_write_attr(lattr,"size",size(dat),ierr=ierrl)
  if(ierrl/=0) then
    call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 161 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
    goto 1
  end if
# 171 "iotk_dat.spp"
  if(binary) then
    call iotk_write_attr(lattr,"kind",kind(dat),ierr=ierrl)
    if(ierrl/=0) then
      call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 174 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
      goto 1
    end if
  end if
# 179 "iotk_dat.spp"
  if(.not.iotk_strcomp(usefmt,"!")) call iotk_write_attr(lattr,"fmt",iotk_strtrim(usefmt),ierr=ierrl)
  if(ierrl/=0) then
    call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 181 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
    goto 1
  end if
  if(lcolumns/=1) call iotk_write_attr(lattr,"columns",lcolumns,ierr=ierrl)
  if(ierrl/=0) then
    call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 186 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
    goto 1
  end if
  if(present(attr)) then
    attr_tmp(1:1)=iotk_eos
    call iotk_strcpy(attr_tmp,attr,ierr=ierrl)
    if(ierrl/=0) then
      call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 193 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
      goto 1
    end if
    call iotk_delete_attr(attr_tmp,"type",ierrl)
    if(ierrl/=0) then
      call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 198 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
      goto 1
    end if
    call iotk_delete_attr(attr_tmp,"kind",ierrl)
    if(ierrl/=0) then
      call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 203 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
      goto 1
    end if
    call iotk_delete_attr(attr_tmp,"size",ierrl)
    if(ierrl/=0) then
      call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 208 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
      goto 1
    end if
    call iotk_delete_attr(attr_tmp,"fmt",ierrl)
    if(ierrl/=0) then
      call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 213 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
      goto 1
    end if
    call iotk_delete_attr(attr_tmp,"columns",ierrl)
    if(ierrl/=0) then
      call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 218 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
      goto 1
    end if
    call iotk_delete_attr(attr_tmp,"len",ierrl)
    if(ierrl/=0) then
      call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 223 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
      goto 1
    end if
    if(iotk_strlen_trim(attr_tmp)>0) call iotk_strcat(lattr,iotk_strtrim(attr_tmp),ierr=ierrl)
  end if
  if(ierrl/=0) then
    call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 229 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
    goto 1
  end if
  call iotk_write_begin(unit,name,lattr,ierr=ierrl)
  if(ierrl/=0) then
    call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 234 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
    goto 1
  end if

  allocate(dattmp(size(dat)))
# 242 "iotk_dat.spp"
#if defined(__IOTK_WORKAROUND3) || defined(__IOTK_WORKAROUND4)
# 246 "iotk_dat.spp"
     call iotk_private_pack_INTEGER4(dattmp,dat,size(dattmp),1)
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
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
        goto 1
      end if
    else
      write(lunit,iostat=iostat) idummy,(dattmp(itmp),itmp=1,size(dattmp))
      if(iostat/=0) then
        call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 263 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
        goto 1
      end if
    end if
  else
    if(raw) then
# 272 "iotk_dat.spp"
      write(lunit,*,iostat=iostat) (dattmp(itmp),itmp=1,size(dattmp))
# 274 "iotk_dat.spp"
      if(iostat/=0) then
        call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 275 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
        goto 1
      end if
    else if(iotk_strcomp(usefmt,"*")) then
      write(lunit,*,iostat=iostat) (dattmp(itmp),itmp=1,size(dattmp))
      if(iostat/=0) then
        call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 281 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
        goto 1
      end if
    else if(iotk_strcomp(usefmt,"!")) then
# 295 "iotk_dat.spp"
     write(lunit,fmt=trim(iotk_wfmt("INTEGER",kind(dattmp),lcolumns,-1,lsep)),iostat=iostat) (dattmp(itmp),itmp=1,size(dattmp))
     if(iostat/=0) then
      call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 297 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
      goto 1
     end if
# 301 "iotk_dat.spp"
    else
      write(lunit,fmt=usefmt(1:iotk_strlen(usefmt)),iostat=iostat) (dattmp(itmp),itmp=1,size(dattmp))
      if(iostat/=0) then
        call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 304 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
        goto 1
      end if
    end if
  end if
  call iotk_write_end(unit,name,ierr=ierrl)
  if(ierrl/=0) then
    call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 311 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
    goto 1
  end if
1 continue
  if(allocated(dattmp)) deallocate(dattmp)
  if(present(ierr)) then
    ierr = ierrl
  else
    if(ierrl/=0) call iotk_error_handler(ierrl)
  end if
end subroutine iotk_write_dat_INTEGER4_6


# 589 "iotk_dat.spp"

# 591 "iotk_dat.spp"
subroutine iotk_scan_dat_INTEGER4_6(unit,name,dat,dummy,attr,found,default,ierr)
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
  INTEGER (kind=__IOTK_INTEGER4)                        :: dat (:,:,:,:,:,:)
#else
  INTEGER (kind=__IOTK_INTEGER4),           intent(out) :: dat (:,:,:,:,:,:)
#endif
  type(iotk_dummytype), optional         :: dummy
#ifdef __IOTK_WORKAROUND6
  character(len=*),optional              :: attr
#else
  character(len=*),optional, intent(out) :: attr
#endif
  logical,         optional, intent(out) :: found
  INTEGER (kind=__IOTK_INTEGER4), optional, intent(in)  :: default (:,:,:,:,:,:)
  integer,         optional, intent(out) :: ierr
# 618 "iotk_dat.spp"
  INTEGER (kind=__IOTK_INTEGER4),              allocatable :: tmpdat(:)
# 620 "iotk_dat.spp"
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
# 637 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
    goto 1
  end if
  call iotk_parse_dat(lattr,rtype,rkind,rsize,rlen,fmt,columns,ierrl)
! Note that columns is not effectively used
  if(ierrl/=0) goto 1
  if(.not. (iotk_strcomp(rtype,iotk_eos) .or. iotk_strcomp(rtype,"INTEGER") ) ) then
    call iotk_error_issue(ierrl,"iotk_scan_dat",__FILE__,__LINE__)
# 644 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
# 644 "iotk_dat.spp"
call iotk_error_msg(ierrl,' ')
# 644 "iotk_dat.spp"
call iotk_error_write(ierrl,"rtype",rtype(1:iotk_strlen(rtype)))
# 644 "iotk_dat.spp"
call iotk_error_write(ierrl,"type","INTEGER")
    goto 1
  end if
  if(.not. (rsize==-1 .or. rsize==size(dat)) ) then
    call iotk_error_issue(ierrl,"iotk_scan_dat",__FILE__,__LINE__)
# 648 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
    goto 1
  end if
  if(rkind==-1) rkind = kind(dat)
# 655 "iotk_dat.spp"

  allocate(tmpdat(size(dat)))
  call iotk_scan_dat_aux(unit,tmpdat,rkind,rlen,fmt(1:iotk_strlen(fmt)),ierrl)
# 662 "iotk_dat.spp"
#ifdef __IOTK_WORKAROUND7
        call iotk_reshape_INTEGER4( size(dat),dat,tmpdat )
#else
        dat = reshape(tmpdat,shape(dat))
#endif
# 671 "iotk_dat.spp"
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
# 683 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
# 683 "iotk_dat.spp"
call iotk_error_msg(ierrl,'Dat not found')
# 683 "iotk_dat.spp"
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
end subroutine iotk_scan_dat_INTEGER4_6


#endif
#endif

subroutine iotk_dat_dummy_INTEGER4_6
  write(0,*)
end subroutine iotk_dat_dummy_INTEGER4_6


# 727 "iotk_dat.spp"


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

#ifdef __IOTK_INTEGER4
#if 7 <= __IOTK_MAXRANK
# 82 "iotk_dat.spp"
subroutine iotk_write_dat_INTEGER4_7(unit,name,dat,dummy,attr,columns,sep,fmt,ierr)
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
  INTEGER (kind=__IOTK_INTEGER4), intent(in)  :: dat (:,:,:,:,:,:,:) 
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
# 114 "iotk_dat.spp"
  INTEGER (kind=__IOTK_INTEGER4),allocatable :: dattmp(:)
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
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
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
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
    goto 1
  end if
  call iotk_strcpy(usefmt,"!",ierrl)
  if(ierrl/=0) then
    call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 142 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
    goto 1
  end if
  if(present(fmt) .and. .not. raw) call iotk_strcpy(usefmt,iotk_strtrim(fmt),ierrl)
  if(ierrl/=0) then
    call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 147 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
    goto 1
  end if
  if(iotk_strscan(usefmt,"<>&")/=0) then
    call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 151 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
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
  call iotk_write_attr(lattr,"type",iotk_tolower("INTEGER"),first=.true.,ierr=ierrl)
  if(ierrl/=0) then
    call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 156 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
    goto 1
  end if
  call iotk_write_attr(lattr,"size",size(dat),ierr=ierrl)
  if(ierrl/=0) then
    call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 161 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
    goto 1
  end if
# 171 "iotk_dat.spp"
  if(binary) then
    call iotk_write_attr(lattr,"kind",kind(dat),ierr=ierrl)
    if(ierrl/=0) then
      call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 174 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
      goto 1
    end if
  end if
# 179 "iotk_dat.spp"
  if(.not.iotk_strcomp(usefmt,"!")) call iotk_write_attr(lattr,"fmt",iotk_strtrim(usefmt),ierr=ierrl)
  if(ierrl/=0) then
    call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 181 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
    goto 1
  end if
  if(lcolumns/=1) call iotk_write_attr(lattr,"columns",lcolumns,ierr=ierrl)
  if(ierrl/=0) then
    call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 186 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
    goto 1
  end if
  if(present(attr)) then
    attr_tmp(1:1)=iotk_eos
    call iotk_strcpy(attr_tmp,attr,ierr=ierrl)
    if(ierrl/=0) then
      call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 193 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
      goto 1
    end if
    call iotk_delete_attr(attr_tmp,"type",ierrl)
    if(ierrl/=0) then
      call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 198 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
      goto 1
    end if
    call iotk_delete_attr(attr_tmp,"kind",ierrl)
    if(ierrl/=0) then
      call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 203 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
      goto 1
    end if
    call iotk_delete_attr(attr_tmp,"size",ierrl)
    if(ierrl/=0) then
      call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 208 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
      goto 1
    end if
    call iotk_delete_attr(attr_tmp,"fmt",ierrl)
    if(ierrl/=0) then
      call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 213 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
      goto 1
    end if
    call iotk_delete_attr(attr_tmp,"columns",ierrl)
    if(ierrl/=0) then
      call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 218 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
      goto 1
    end if
    call iotk_delete_attr(attr_tmp,"len",ierrl)
    if(ierrl/=0) then
      call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 223 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
      goto 1
    end if
    if(iotk_strlen_trim(attr_tmp)>0) call iotk_strcat(lattr,iotk_strtrim(attr_tmp),ierr=ierrl)
  end if
  if(ierrl/=0) then
    call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 229 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
    goto 1
  end if
  call iotk_write_begin(unit,name,lattr,ierr=ierrl)
  if(ierrl/=0) then
    call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 234 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
    goto 1
  end if

  allocate(dattmp(size(dat)))
# 242 "iotk_dat.spp"
#if defined(__IOTK_WORKAROUND3) || defined(__IOTK_WORKAROUND4)
# 246 "iotk_dat.spp"
     call iotk_private_pack_INTEGER4(dattmp,dat,size(dattmp),1)
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
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
        goto 1
      end if
    else
      write(lunit,iostat=iostat) idummy,(dattmp(itmp),itmp=1,size(dattmp))
      if(iostat/=0) then
        call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 263 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
        goto 1
      end if
    end if
  else
    if(raw) then
# 272 "iotk_dat.spp"
      write(lunit,*,iostat=iostat) (dattmp(itmp),itmp=1,size(dattmp))
# 274 "iotk_dat.spp"
      if(iostat/=0) then
        call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 275 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
        goto 1
      end if
    else if(iotk_strcomp(usefmt,"*")) then
      write(lunit,*,iostat=iostat) (dattmp(itmp),itmp=1,size(dattmp))
      if(iostat/=0) then
        call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 281 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
        goto 1
      end if
    else if(iotk_strcomp(usefmt,"!")) then
# 295 "iotk_dat.spp"
     write(lunit,fmt=trim(iotk_wfmt("INTEGER",kind(dattmp),lcolumns,-1,lsep)),iostat=iostat) (dattmp(itmp),itmp=1,size(dattmp))
     if(iostat/=0) then
      call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 297 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
      goto 1
     end if
# 301 "iotk_dat.spp"
    else
      write(lunit,fmt=usefmt(1:iotk_strlen(usefmt)),iostat=iostat) (dattmp(itmp),itmp=1,size(dattmp))
      if(iostat/=0) then
        call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 304 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
        goto 1
      end if
    end if
  end if
  call iotk_write_end(unit,name,ierr=ierrl)
  if(ierrl/=0) then
    call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 311 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
    goto 1
  end if
1 continue
  if(allocated(dattmp)) deallocate(dattmp)
  if(present(ierr)) then
    ierr = ierrl
  else
    if(ierrl/=0) call iotk_error_handler(ierrl)
  end if
end subroutine iotk_write_dat_INTEGER4_7


# 589 "iotk_dat.spp"

# 591 "iotk_dat.spp"
subroutine iotk_scan_dat_INTEGER4_7(unit,name,dat,dummy,attr,found,default,ierr)
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
  INTEGER (kind=__IOTK_INTEGER4)                        :: dat (:,:,:,:,:,:,:)
#else
  INTEGER (kind=__IOTK_INTEGER4),           intent(out) :: dat (:,:,:,:,:,:,:)
#endif
  type(iotk_dummytype), optional         :: dummy
#ifdef __IOTK_WORKAROUND6
  character(len=*),optional              :: attr
#else
  character(len=*),optional, intent(out) :: attr
#endif
  logical,         optional, intent(out) :: found
  INTEGER (kind=__IOTK_INTEGER4), optional, intent(in)  :: default (:,:,:,:,:,:,:)
  integer,         optional, intent(out) :: ierr
# 618 "iotk_dat.spp"
  INTEGER (kind=__IOTK_INTEGER4),              allocatable :: tmpdat(:)
# 620 "iotk_dat.spp"
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
# 637 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
    goto 1
  end if
  call iotk_parse_dat(lattr,rtype,rkind,rsize,rlen,fmt,columns,ierrl)
! Note that columns is not effectively used
  if(ierrl/=0) goto 1
  if(.not. (iotk_strcomp(rtype,iotk_eos) .or. iotk_strcomp(rtype,"INTEGER") ) ) then
    call iotk_error_issue(ierrl,"iotk_scan_dat",__FILE__,__LINE__)
# 644 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
# 644 "iotk_dat.spp"
call iotk_error_msg(ierrl,' ')
# 644 "iotk_dat.spp"
call iotk_error_write(ierrl,"rtype",rtype(1:iotk_strlen(rtype)))
# 644 "iotk_dat.spp"
call iotk_error_write(ierrl,"type","INTEGER")
    goto 1
  end if
  if(.not. (rsize==-1 .or. rsize==size(dat)) ) then
    call iotk_error_issue(ierrl,"iotk_scan_dat",__FILE__,__LINE__)
# 648 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
    goto 1
  end if
  if(rkind==-1) rkind = kind(dat)
# 655 "iotk_dat.spp"

  allocate(tmpdat(size(dat)))
  call iotk_scan_dat_aux(unit,tmpdat,rkind,rlen,fmt(1:iotk_strlen(fmt)),ierrl)
# 662 "iotk_dat.spp"
#ifdef __IOTK_WORKAROUND7
        call iotk_reshape_INTEGER4( size(dat),dat,tmpdat )
#else
        dat = reshape(tmpdat,shape(dat))
#endif
# 671 "iotk_dat.spp"
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
# 683 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.17 ")
# 683 "iotk_dat.spp"
call iotk_error_msg(ierrl,'Dat not found')
# 683 "iotk_dat.spp"
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
end subroutine iotk_scan_dat_INTEGER4_7


#endif
#endif

subroutine iotk_dat_dummy_INTEGER4_7
  write(0,*)
end subroutine iotk_dat_dummy_INTEGER4_7


# 727 "iotk_dat.spp"


# 45 "iotk_dat.spp"

