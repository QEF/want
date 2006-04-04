# 48 "iotk_dat.spp"
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
# 65 "iotk_dat.spp"


!------------------------------------------------------------------------------!
! Inclusion of configuration file
#include "iotk_config.h"
!------------------------------------------------------------------------------!

# 74 "iotk_dat.spp"
#include "iotk_auxmacros.h"
# 76 "iotk_dat.spp"

# 78 "iotk_dat.spp"

#ifdef __IOTK_REAL1
#if 0 <= __IOTK_MAXRANK
# 82 "iotk_dat.spp"
subroutine iotk_write_dat_REAL1_0(unit,name,dat,dummy,attr,columns,sep,fmt,ierr)
  use iotk_base
  use iotk_error_interf
  use iotk_attr_interf, only : iotk_write_attr
  use iotk_write_interf
  use iotk_fmt_interf
  use iotk_str_interf
  use iotk_unit_interf
  use iotk_misc_interf
  implicit none
  integer, parameter :: this_kind = iotk_REAL1
  integer,                                    intent(in)  :: unit
  character(len=*),                           intent(in)  :: name
  REAL (kind=this_kind),           intent(in)  :: dat  
  type(iotk_dummytype),             optional              :: dummy
  character(len=*),                 optional, intent(in)  :: attr
  integer,                          optional, intent(in)  :: columns
  character(len=*),                 optional, intent(in)  :: sep
  character(len=*),                 optional, intent(in)  :: fmt
  integer,                          optional, intent(out) :: ierr
  integer :: ierrl,lunit,iostat
  logical :: binary,raw,stream
  integer :: lcolumns
  integer(iotk_header_kind), parameter :: idummy=0
  character(100) :: lsep
  character(300) :: usefmt
  character(iotk_attlenx) :: lattr
  character(iotk_attlenx) :: attr_tmp
  type (iotk_unit), pointer :: this
# 115 "iotk_dat.spp"
  REAL (kind=this_kind),allocatable :: dattmp(:)
# 117 "iotk_dat.spp"
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
# 126 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
      goto 1
    end if
  end if
  lunit = iotk_phys_unit(unit)
  call iotk_unit_get(lunit,pointer=this)
  raw = .false.
  if(associated(this)) then
    raw = this%raw
  end if
  call iotk_inquire(lunit,binary,stream,ierrl)
  if(ierrl/=0) then
    call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 138 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
    goto 1
  end if
  call iotk_strcpy(usefmt,"!",ierrl)
  if(ierrl/=0) then
    call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 143 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
    goto 1
  end if
  if(present(fmt) .and. .not. raw) call iotk_strcpy(usefmt,iotk_strtrim(fmt),ierrl)
  if(ierrl/=0) then
    call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 148 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
    goto 1
  end if
  if(iotk_strscan(usefmt,"<>&")/=0) then
    call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 152 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
# 152 "iotk_dat.spp"
call iotk_error_msg(ierrl,'Special characters (<>&) found in fmt string')
# 152 "iotk_dat.spp"
call iotk_error_write(ierrl,"unit",unit)
# 152 "iotk_dat.spp"
call iotk_error_write(ierrl,"name",trim(name))
# 152 "iotk_dat.spp"
call iotk_error_write(ierrl,"fmt",trim(fmt))
    goto 1
  end if
  call iotk_write_attr(lattr,"type",iotk_tolower("REAL"),first=.true.,ierr=ierrl)
  if(ierrl/=0) then
    call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 157 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
    goto 1
  end if
  call iotk_write_attr(lattr,"size",1,ierr=ierrl)
  if(ierrl/=0) then
    call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 162 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
    goto 1
  end if
# 172 "iotk_dat.spp"
  if(binary) then
    call iotk_write_attr(lattr,"kind",kind(dat),ierr=ierrl)
    if(ierrl/=0) then
      call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 175 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
      goto 1
    end if
  end if
# 180 "iotk_dat.spp"
  if(.not.iotk_strcomp(usefmt,"!")) call iotk_write_attr(lattr,"fmt",iotk_strtrim(usefmt),ierr=ierrl)
  if(ierrl/=0) then
    call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 182 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
    goto 1
  end if
  if(lcolumns/=1) call iotk_write_attr(lattr,"columns",lcolumns,ierr=ierrl)
  if(ierrl/=0) then
    call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 187 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
    goto 1
  end if
  if(present(attr)) then
    attr_tmp(1:1)=iotk_eos
    call iotk_strcpy(attr_tmp,attr,ierr=ierrl)
    if(ierrl/=0) then
      call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 194 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
      goto 1
    end if
    call iotk_delete_attr(attr_tmp,"type",ierrl)
    if(ierrl/=0) then
      call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 199 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
      goto 1
    end if
    call iotk_delete_attr(attr_tmp,"kind",ierrl)
    if(ierrl/=0) then
      call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 204 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
      goto 1
    end if
    call iotk_delete_attr(attr_tmp,"size",ierrl)
    if(ierrl/=0) then
      call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 209 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
      goto 1
    end if
    call iotk_delete_attr(attr_tmp,"fmt",ierrl)
    if(ierrl/=0) then
      call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 214 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
      goto 1
    end if
    call iotk_delete_attr(attr_tmp,"columns",ierrl)
    if(ierrl/=0) then
      call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 219 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
      goto 1
    end if
    call iotk_delete_attr(attr_tmp,"len",ierrl)
    if(ierrl/=0) then
      call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 224 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
      goto 1
    end if
    if(iotk_strlen_trim(attr_tmp)>0) call iotk_strcat(lattr,iotk_strtrim(attr_tmp),ierr=ierrl)
  end if
  if(ierrl/=0) then
    call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 230 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
    goto 1
  end if
  call iotk_write_begin(unit,name,lattr,ierr=ierrl)
  if(ierrl/=0) then
    call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 235 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
    goto 1
  end if

  allocate(dattmp(1))
# 241 "iotk_dat.spp"
     dattmp(1) = dat
# 253 "iotk_dat.spp"

  if(binary) then
    if(raw) then
      write(lunit,iostat=iostat) (dattmp(itmp),itmp=1,size(dattmp))
      if(iostat/=0) then
        call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 258 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
        goto 1
      end if
    else
      write(lunit,iostat=iostat) idummy,(dattmp(itmp),itmp=1,size(dattmp))
      if(iostat/=0) then
        call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 264 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
        goto 1
      end if
    end if
  else
    if(raw) then
# 273 "iotk_dat.spp"
      write(lunit,*,iostat=iostat) (dattmp(itmp),itmp=1,size(dattmp))
# 275 "iotk_dat.spp"
      if(iostat/=0) then
        call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 276 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
        goto 1
      end if
    else if(iotk_strcomp(usefmt,"*")) then
      write(lunit,*,iostat=iostat) (dattmp(itmp),itmp=1,size(dattmp))
      if(iostat/=0) then
        call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 282 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
        goto 1
      end if
    else if(iotk_strcomp(usefmt,"!")) then
# 296 "iotk_dat.spp"
     write(lunit,fmt=trim(iotk_wfmt("REAL",kind(dattmp),lcolumns,-1,lsep)),iostat=iostat) (dattmp(itmp),itmp=1,size(dattmp))
     if(iostat/=0) then
      call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 298 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
      goto 1
     end if
# 302 "iotk_dat.spp"
    else
      write(lunit,fmt=usefmt(1:iotk_strlen(usefmt)),iostat=iostat) (dattmp(itmp),itmp=1,size(dattmp))
      if(iostat/=0) then
        call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 305 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
        goto 1
      end if
    end if
  end if
  call iotk_write_end(unit,name,ierr=ierrl)
  if(ierrl/=0) then
    call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 312 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
    goto 1
  end if
1 continue
  if(allocated(dattmp)) deallocate(dattmp)
  if(present(ierr)) then
    ierr = ierrl
  else
    if(ierrl/=0) call iotk_error_handler(ierrl)
  end if
end subroutine iotk_write_dat_REAL1_0


# 668 "iotk_dat.spp"

# 670 "iotk_dat.spp"
subroutine iotk_scan_dat_REAL1_0(unit,name,dat,dummy,attr,found,default,ierr)
  use iotk_base
  use iotk_error_interf
  use iotk_dat_interf, only: iotk_scan_dat_aux
  use iotk_scan_interf
  use iotk_str_interf
  use iotk_misc_interf
  implicit none
  integer, parameter :: this_kind = iotk_REAL1
  integer,                                   intent(in)  :: unit
  character(len=*),                          intent(in)  :: name
#ifdef __IOTK_WORKAROUND6
  REAL(kind=this_kind)                        :: dat 
#else
  REAL(kind=this_kind),           intent(out) :: dat 
#endif
  type(iotk_dummytype),            optional              :: dummy
#ifdef __IOTK_WORKAROUND6
  character(len=*),                optional              :: attr
#else
  character(len=*),                optional, intent(out) :: attr
#endif
  logical,                         optional, intent(out) :: found
  REAL(kind=this_kind), optional, intent(in)  :: default 
  integer,                         optional, intent(out) :: ierr
# 698 "iotk_dat.spp"
  REAL (kind=this_kind),              allocatable :: tmpdat(:)
# 700 "iotk_dat.spp"
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
# 717 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
    goto 1
  end if
  call iotk_parse_dat(lattr,rtype,rkind,rsize,rlen,fmt,columns,ierrl)
! Note that columns is not effectively used
  if(ierrl/=0) goto 1
  if(.not. (iotk_strcomp(rtype,iotk_eos) .or. iotk_strcomp(rtype,"REAL") ) ) then
    call iotk_error_issue(ierrl,"iotk_scan_dat",__FILE__,__LINE__)
# 724 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
# 724 "iotk_dat.spp"
call iotk_error_msg(ierrl,' ')
# 724 "iotk_dat.spp"
call iotk_error_write(ierrl,"rtype",rtype(1:iotk_strlen(rtype)))
# 724 "iotk_dat.spp"
call iotk_error_write(ierrl,"type","REAL")
    goto 1
  end if
  if(.not. (rsize==-1 .or. rsize==1) ) then
    call iotk_error_issue(ierrl,"iotk_scan_dat",__FILE__,__LINE__)
# 728 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
    goto 1
  end if
  if(rkind==-1) rkind = kind(dat)
# 735 "iotk_dat.spp"

  allocate(tmpdat(1))
  call iotk_scan_dat_aux(unit,tmpdat,rkind,rlen,fmt(1:iotk_strlen(fmt)),ierrl)
  if(ierrl/=0) then
    call iotk_error_issue(ierrl,"iotk_scan_dat",__FILE__,__LINE__)
# 739 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
# 739 "iotk_dat.spp"
call iotk_error_msg(ierrl,'Error reading data')
# 739 "iotk_dat.spp"
call iotk_error_write(ierrl,"name",name)
# 739 "iotk_dat.spp"
call iotk_error_write(ierrl,"rkind",rkind)
# 739 "iotk_dat.spp"
call iotk_error_write(ierrl,"rlen",rlen)
    goto 1
  end if
# 743 "iotk_dat.spp"
     dat = tmpdat(1)
# 755 "iotk_dat.spp"
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
# 767 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
# 767 "iotk_dat.spp"
call iotk_error_msg(ierrl,'Dat not found')
# 767 "iotk_dat.spp"
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
end subroutine iotk_scan_dat_REAL1_0


#endif
#endif

subroutine iotk_dat_dummy_REAL1_0
  write(0,*)
end subroutine iotk_dat_dummy_REAL1_0


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

#ifdef __IOTK_REAL1
#if 1 <= __IOTK_MAXRANK
# 82 "iotk_dat.spp"
subroutine iotk_write_dat_REAL1_1(unit,name,dat,dummy,attr,columns,sep,fmt,ierr)
  use iotk_base
  use iotk_error_interf
  use iotk_attr_interf, only : iotk_write_attr
  use iotk_write_interf
  use iotk_fmt_interf
  use iotk_str_interf
  use iotk_unit_interf
  use iotk_misc_interf
  implicit none
  integer, parameter :: this_kind = iotk_REAL1
  integer,                                    intent(in)  :: unit
  character(len=*),                           intent(in)  :: name
  REAL (kind=this_kind),           intent(in)  :: dat (:) 
  type(iotk_dummytype),             optional              :: dummy
  character(len=*),                 optional, intent(in)  :: attr
  integer,                          optional, intent(in)  :: columns
  character(len=*),                 optional, intent(in)  :: sep
  character(len=*),                 optional, intent(in)  :: fmt
  integer,                          optional, intent(out) :: ierr
  integer :: ierrl,lunit,iostat
  logical :: binary,raw,stream
  integer :: lcolumns
  integer(iotk_header_kind), parameter :: idummy=0
  character(100) :: lsep
  character(300) :: usefmt
  character(iotk_attlenx) :: lattr
  character(iotk_attlenx) :: attr_tmp
  type (iotk_unit), pointer :: this
# 115 "iotk_dat.spp"
  REAL (kind=this_kind),allocatable :: dattmp(:)
# 117 "iotk_dat.spp"
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
# 126 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
      goto 1
    end if
  end if
  lunit = iotk_phys_unit(unit)
  call iotk_unit_get(lunit,pointer=this)
  raw = .false.
  if(associated(this)) then
    raw = this%raw
  end if
  call iotk_inquire(lunit,binary,stream,ierrl)
  if(ierrl/=0) then
    call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 138 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
    goto 1
  end if
  call iotk_strcpy(usefmt,"!",ierrl)
  if(ierrl/=0) then
    call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 143 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
    goto 1
  end if
  if(present(fmt) .and. .not. raw) call iotk_strcpy(usefmt,iotk_strtrim(fmt),ierrl)
  if(ierrl/=0) then
    call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 148 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
    goto 1
  end if
  if(iotk_strscan(usefmt,"<>&")/=0) then
    call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 152 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
# 152 "iotk_dat.spp"
call iotk_error_msg(ierrl,'Special characters (<>&) found in fmt string')
# 152 "iotk_dat.spp"
call iotk_error_write(ierrl,"unit",unit)
# 152 "iotk_dat.spp"
call iotk_error_write(ierrl,"name",trim(name))
# 152 "iotk_dat.spp"
call iotk_error_write(ierrl,"fmt",trim(fmt))
    goto 1
  end if
  call iotk_write_attr(lattr,"type",iotk_tolower("REAL"),first=.true.,ierr=ierrl)
  if(ierrl/=0) then
    call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 157 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
    goto 1
  end if
  call iotk_write_attr(lattr,"size",size(dat),ierr=ierrl)
  if(ierrl/=0) then
    call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 162 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
    goto 1
  end if
# 172 "iotk_dat.spp"
  if(binary) then
    call iotk_write_attr(lattr,"kind",kind(dat),ierr=ierrl)
    if(ierrl/=0) then
      call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 175 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
      goto 1
    end if
  end if
# 180 "iotk_dat.spp"
  if(.not.iotk_strcomp(usefmt,"!")) call iotk_write_attr(lattr,"fmt",iotk_strtrim(usefmt),ierr=ierrl)
  if(ierrl/=0) then
    call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 182 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
    goto 1
  end if
  if(lcolumns/=1) call iotk_write_attr(lattr,"columns",lcolumns,ierr=ierrl)
  if(ierrl/=0) then
    call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 187 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
    goto 1
  end if
  if(present(attr)) then
    attr_tmp(1:1)=iotk_eos
    call iotk_strcpy(attr_tmp,attr,ierr=ierrl)
    if(ierrl/=0) then
      call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 194 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
      goto 1
    end if
    call iotk_delete_attr(attr_tmp,"type",ierrl)
    if(ierrl/=0) then
      call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 199 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
      goto 1
    end if
    call iotk_delete_attr(attr_tmp,"kind",ierrl)
    if(ierrl/=0) then
      call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 204 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
      goto 1
    end if
    call iotk_delete_attr(attr_tmp,"size",ierrl)
    if(ierrl/=0) then
      call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 209 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
      goto 1
    end if
    call iotk_delete_attr(attr_tmp,"fmt",ierrl)
    if(ierrl/=0) then
      call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 214 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
      goto 1
    end if
    call iotk_delete_attr(attr_tmp,"columns",ierrl)
    if(ierrl/=0) then
      call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 219 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
      goto 1
    end if
    call iotk_delete_attr(attr_tmp,"len",ierrl)
    if(ierrl/=0) then
      call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 224 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
      goto 1
    end if
    if(iotk_strlen_trim(attr_tmp)>0) call iotk_strcat(lattr,iotk_strtrim(attr_tmp),ierr=ierrl)
  end if
  if(ierrl/=0) then
    call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 230 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
    goto 1
  end if
  call iotk_write_begin(unit,name,lattr,ierr=ierrl)
  if(ierrl/=0) then
    call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 235 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
    goto 1
  end if

  allocate(dattmp(size(dat)))
# 243 "iotk_dat.spp"
#if defined(__IOTK_WORKAROUND3) || defined(__IOTK_WORKAROUND4)
# 247 "iotk_dat.spp"
     call iotk_private_pack_REAL1(dattmp,dat,size(dattmp),1)
# 249 "iotk_dat.spp"
#else
     dattmp = pack(dat,mask=.true.)
#endif
# 253 "iotk_dat.spp"

  if(binary) then
    if(raw) then
      write(lunit,iostat=iostat) (dattmp(itmp),itmp=1,size(dattmp))
      if(iostat/=0) then
        call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 258 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
        goto 1
      end if
    else
      write(lunit,iostat=iostat) idummy,(dattmp(itmp),itmp=1,size(dattmp))
      if(iostat/=0) then
        call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 264 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
        goto 1
      end if
    end if
  else
    if(raw) then
# 273 "iotk_dat.spp"
      write(lunit,*,iostat=iostat) (dattmp(itmp),itmp=1,size(dattmp))
# 275 "iotk_dat.spp"
      if(iostat/=0) then
        call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 276 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
        goto 1
      end if
    else if(iotk_strcomp(usefmt,"*")) then
      write(lunit,*,iostat=iostat) (dattmp(itmp),itmp=1,size(dattmp))
      if(iostat/=0) then
        call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 282 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
        goto 1
      end if
    else if(iotk_strcomp(usefmt,"!")) then
# 296 "iotk_dat.spp"
     write(lunit,fmt=trim(iotk_wfmt("REAL",kind(dattmp),lcolumns,-1,lsep)),iostat=iostat) (dattmp(itmp),itmp=1,size(dattmp))
     if(iostat/=0) then
      call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 298 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
      goto 1
     end if
# 302 "iotk_dat.spp"
    else
      write(lunit,fmt=usefmt(1:iotk_strlen(usefmt)),iostat=iostat) (dattmp(itmp),itmp=1,size(dattmp))
      if(iostat/=0) then
        call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 305 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
        goto 1
      end if
    end if
  end if
  call iotk_write_end(unit,name,ierr=ierrl)
  if(ierrl/=0) then
    call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 312 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
    goto 1
  end if
1 continue
  if(allocated(dattmp)) deallocate(dattmp)
  if(present(ierr)) then
    ierr = ierrl
  else
    if(ierrl/=0) call iotk_error_handler(ierrl)
  end if
end subroutine iotk_write_dat_REAL1_1


# 327 "iotk_dat.spp"
recursive subroutine iotk_scan_dat_aux_REAL1(unit,dat,rkind,rlen,fmt,ierr)
  use iotk_base
  use iotk_error_interf
  use iotk_attr_interf, only: iotk_read
  use iotk_scan_interf
  use iotk_str_interf
  use iotk_unit_interf
  use iotk_misc_interf
  use iotk_stream_interf
  implicit none
  integer, parameter :: this_kind = iotk_REAL1
  integer,                         intent(in)  :: unit
#ifdef __IOTK_WORKAROUND6
  REAL(kind=this_kind)              :: dat (:)
#else
  REAL(kind=this_kind), intent(out) :: dat (:)
#endif
  integer,                         intent(in)  :: rkind
  integer,                         intent(in)  :: rlen
  character(len=*),                intent(in)  :: fmt
  integer,                         intent(out) :: ierr
  integer(iotk_header_kind) :: idummy
  logical :: raw,binary,stream
  integer :: lunit
# 352 "iotk_dat.spp"
  integer :: i
# 354 "iotk_dat.spp"
#ifdef __IOTK_WORKAROUND3
  integer :: j
#endif
  integer :: index,length,nexttag,iostat,altlength
  type(iotk_unit), pointer :: this
  character(len=iotk_linlenx) :: line,altline
# 365 "iotk_dat.spp"
#ifdef __IOTK_REAL2
  REAL (kind=iotk_REAL2), allocatable :: dat2 (:)
#endif
# 365 "iotk_dat.spp"
#ifdef __IOTK_REAL3
  REAL (kind=iotk_REAL3), allocatable :: dat3 (:)
#endif
# 365 "iotk_dat.spp"
#ifdef __IOTK_REAL4
  REAL (kind=iotk_REAL4), allocatable :: dat4 (:)
#endif
# 371 "iotk_dat.spp"
  lunit = iotk_phys_unit(unit)
  ierr = 0
  iostat = 0
  idummy = 0
  call iotk_unit_get(lunit,pointer=this)
  raw = .false.
  if(associated(this)) then
    raw = this%raw
  end if
  call iotk_inquire(lunit,binary,stream,ierr)
  if(ierr/=0) then
    call iotk_error_issue(ierr,"iotk_scan_dat_aux",__FILE__,__LINE__)
# 382 "iotk_dat.spp"
call iotk_error_msg(ierr,"CVS Revision: 1.25 ")
    return
  end if
# 500 "iotk_dat.spp"
  if(binary) then
    select case(rkind)
    case(kind(dat))
      if(raw) then
#ifdef __IOTK_WORKAROUND3
        read(lunit,iostat=iostat) ( dat(j), j=1,ubound(dat,1) )
#else
        read(lunit,iostat=iostat) dat
#endif
        if(iostat/=0) then
          call iotk_error_issue(ierr,"iotk_scan_dat_aux",__FILE__,__LINE__)
# 510 "iotk_dat.spp"
call iotk_error_msg(ierr,"CVS Revision: 1.25 ")
# 510 "iotk_dat.spp"
call iotk_error_msg(ierr,' ')
# 510 "iotk_dat.spp"
call iotk_error_write(ierr,"iostat",iostat)
          return
        end if
      else
        if(stream) then
          call iotk_stream_read(lunit,idummy,dat,ierr=ierr)
          if(ierr/=0) then
            call iotk_error_issue(ierr,"iotk_scan_dat_aux",__FILE__,__LINE__)
# 517 "iotk_dat.spp"
call iotk_error_msg(ierr,"CVS Revision: 1.25 ")
            return
          end if
        else
#ifdef __IOTK_WORKAROUND3
          read(lunit,iostat=iostat) idummy, ( dat(j), j=1,ubound(dat,1) )
#else
          read(lunit,iostat=iostat) idummy, dat
#endif
        end if
        if(iostat/=0) then
          call iotk_error_issue(ierr,"iotk_scan_dat_aux",__FILE__,__LINE__)
# 528 "iotk_dat.spp"
call iotk_error_msg(ierr,"CVS Revision: 1.25 ")
# 528 "iotk_dat.spp"
call iotk_error_msg(ierr,' ')
# 528 "iotk_dat.spp"
call iotk_error_write(ierr,"iostat",iostat)
          return
        end if
      end if
# 534 "iotk_dat.spp"
#ifdef __IOTK_REAL2
    case(kind(dat2))
      ! Giusto per scrupolo. Se e' raw non ci sono info sul kind, quindi questa linea e' irraggiungibile
      if(raw) then
        call iotk_error_issue(ierr,"iotk_scan_dat_aux",__FILE__,__LINE__)
# 538 "iotk_dat.spp"
call iotk_error_msg(ierr,"CVS Revision: 1.25 ")
        return
      end if
      allocate(dat2(ubound(dat,1)))
      if(stream) then
        call iotk_stream_read(lunit,idummy,dat2,ierr=ierr)
        if(ierr/=0) then
          deallocate(dat2)
          call iotk_error_issue(ierr,"iotk_scan_dat_aux",__FILE__,__LINE__)
# 546 "iotk_dat.spp"
call iotk_error_msg(ierr,"CVS Revision: 1.25 ")
# 546 "iotk_dat.spp"
call iotk_error_msg(ierr,' ')
# 546 "iotk_dat.spp"
call iotk_error_write(ierr,"iostat",iostat)
          return
        end if
      else
        read(lunit,iostat=iostat) idummy,( dat2(i), i=1,ubound(dat2,1) )
        if(iostat/=0) then
          deallocate(dat2)
          call iotk_error_issue(ierr,"iotk_scan_dat_aux",__FILE__,__LINE__)
# 553 "iotk_dat.spp"
call iotk_error_msg(ierr,"CVS Revision: 1.25 ")
# 553 "iotk_dat.spp"
call iotk_error_msg(ierr,' ')
# 553 "iotk_dat.spp"
call iotk_error_write(ierr,"iostat",iostat)
          return
        end if
      end if
# 566 "iotk_dat.spp"
      dat = real(dat2,kind=kind(dat))
# 570 "iotk_dat.spp"
      deallocate(dat2)
#endif
# 534 "iotk_dat.spp"
#ifdef __IOTK_REAL3
    case(kind(dat3))
      ! Giusto per scrupolo. Se e' raw non ci sono info sul kind, quindi questa linea e' irraggiungibile
      if(raw) then
        call iotk_error_issue(ierr,"iotk_scan_dat_aux",__FILE__,__LINE__)
# 538 "iotk_dat.spp"
call iotk_error_msg(ierr,"CVS Revision: 1.25 ")
        return
      end if
      allocate(dat3(ubound(dat,1)))
      if(stream) then
        call iotk_stream_read(lunit,idummy,dat3,ierr=ierr)
        if(ierr/=0) then
          deallocate(dat3)
          call iotk_error_issue(ierr,"iotk_scan_dat_aux",__FILE__,__LINE__)
# 546 "iotk_dat.spp"
call iotk_error_msg(ierr,"CVS Revision: 1.25 ")
# 546 "iotk_dat.spp"
call iotk_error_msg(ierr,' ')
# 546 "iotk_dat.spp"
call iotk_error_write(ierr,"iostat",iostat)
          return
        end if
      else
        read(lunit,iostat=iostat) idummy,( dat3(i), i=1,ubound(dat3,1) )
        if(iostat/=0) then
          deallocate(dat3)
          call iotk_error_issue(ierr,"iotk_scan_dat_aux",__FILE__,__LINE__)
# 553 "iotk_dat.spp"
call iotk_error_msg(ierr,"CVS Revision: 1.25 ")
# 553 "iotk_dat.spp"
call iotk_error_msg(ierr,' ')
# 553 "iotk_dat.spp"
call iotk_error_write(ierr,"iostat",iostat)
          return
        end if
      end if
# 566 "iotk_dat.spp"
      dat = real(dat3,kind=kind(dat))
# 570 "iotk_dat.spp"
      deallocate(dat3)
#endif
# 534 "iotk_dat.spp"
#ifdef __IOTK_REAL4
    case(kind(dat4))
      ! Giusto per scrupolo. Se e' raw non ci sono info sul kind, quindi questa linea e' irraggiungibile
      if(raw) then
        call iotk_error_issue(ierr,"iotk_scan_dat_aux",__FILE__,__LINE__)
# 538 "iotk_dat.spp"
call iotk_error_msg(ierr,"CVS Revision: 1.25 ")
        return
      end if
      allocate(dat4(ubound(dat,1)))
      if(stream) then
        call iotk_stream_read(lunit,idummy,dat4,ierr=ierr)
        if(ierr/=0) then
          deallocate(dat4)
          call iotk_error_issue(ierr,"iotk_scan_dat_aux",__FILE__,__LINE__)
# 546 "iotk_dat.spp"
call iotk_error_msg(ierr,"CVS Revision: 1.25 ")
# 546 "iotk_dat.spp"
call iotk_error_msg(ierr,' ')
# 546 "iotk_dat.spp"
call iotk_error_write(ierr,"iostat",iostat)
          return
        end if
      else
        read(lunit,iostat=iostat) idummy,( dat4(i), i=1,ubound(dat4,1) )
        if(iostat/=0) then
          deallocate(dat4)
          call iotk_error_issue(ierr,"iotk_scan_dat_aux",__FILE__,__LINE__)
# 553 "iotk_dat.spp"
call iotk_error_msg(ierr,"CVS Revision: 1.25 ")
# 553 "iotk_dat.spp"
call iotk_error_msg(ierr,' ')
# 553 "iotk_dat.spp"
call iotk_error_write(ierr,"iostat",iostat)
          return
        end if
      end if
# 566 "iotk_dat.spp"
      dat = real(dat4,kind=kind(dat))
# 570 "iotk_dat.spp"
      deallocate(dat4)
#endif
# 574 "iotk_dat.spp"
    case default
      call iotk_error_issue(ierr,"iotk_scan_dat_aux",__FILE__,__LINE__)
# 575 "iotk_dat.spp"
call iotk_error_msg(ierr,"CVS Revision: 1.25 ")
# 575 "iotk_dat.spp"
call iotk_error_msg(ierr,'Kind incompatibility')
# 575 "iotk_dat.spp"
call iotk_error_write(ierr,"kind",rkind)
    end select
  else
    if(raw) then
#ifdef __IOTK_WORKAROUND3
      read(lunit,fmt=*,iostat=iostat) ( dat(j), j=1,ubound(dat,1) )
#else
      read(lunit,fmt=*,iostat=iostat) dat
#endif
      if(iostat/=0) then
        call iotk_error_issue(ierr,"iotk_scan_dat_aux",__FILE__,__LINE__)
# 585 "iotk_dat.spp"
call iotk_error_msg(ierr,"CVS Revision: 1.25 ")
# 585 "iotk_dat.spp"
call iotk_error_msg(ierr,' ')
# 585 "iotk_dat.spp"
call iotk_error_write(ierr,"iostat",iostat)
        return
      end if
    else if(iotk_strcomp(fmt,"*")) then
#ifdef __IOTK_WORKAROUND3
      read(lunit,fmt=*,iostat=iostat) ( dat(j), j=1,ubound(dat,1) )
#else
      read(lunit,fmt=*,iostat=iostat) dat
#endif
      if(iostat/=0) then
        call iotk_error_issue(ierr,"iotk_scan_dat_aux",__FILE__,__LINE__)
# 595 "iotk_dat.spp"
call iotk_error_msg(ierr,"CVS Revision: 1.25 ")
# 595 "iotk_dat.spp"
call iotk_error_msg(ierr,' ')
# 595 "iotk_dat.spp"
call iotk_error_write(ierr,"iostat",iostat)
        return
      end if
    else if(iotk_strcomp(fmt,"!")) then
      index = 0
      do
        call iotk_getline(lunit,line,length,ierr)
        if(ierr/=0) then
          call iotk_error_issue(ierr,"iotk_scan_dat_aux",__FILE__,__LINE__)
# 603 "iotk_dat.spp"
call iotk_error_msg(ierr,"CVS Revision: 1.25 ")
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
# 614 "iotk_dat.spp"
call iotk_error_msg(ierr,"CVS Revision: 1.25 ")
# 614 "iotk_dat.spp"
call iotk_error_msg(ierr,' ')
# 614 "iotk_dat.spp"
call iotk_error_write(ierr,"iostat",iostat)
            return
          end if
          call iotk_getline(lunit,altline,altlength,ierr)
          if(ierr/=0) then
            call iotk_error_issue(ierr,"iotk_scan_dat_aux",__FILE__,__LINE__)
# 619 "iotk_dat.spp"
call iotk_error_msg(ierr,"CVS Revision: 1.25 ")
            return
          end if
          backspace(lunit,iostat=iostat)
          if(iostat/=0) then
            call iotk_error_issue(ierr,"iotk_scan_dat_aux",__FILE__,__LINE__)
# 624 "iotk_dat.spp"
call iotk_error_msg(ierr,"CVS Revision: 1.25 ")
# 624 "iotk_dat.spp"
call iotk_error_msg(ierr,' ')
# 624 "iotk_dat.spp"
call iotk_error_write(ierr,"iostat",iostat)
            return
          end if
          read(lunit,"(a)",advance="no",iostat=iostat) altline(1:nexttag-1 + altlength - length)
          if(iostat/=0) then
            call iotk_error_issue(ierr,"iotk_scan_dat_aux",__FILE__,__LINE__)
# 629 "iotk_dat.spp"
call iotk_error_msg(ierr,"CVS Revision: 1.25 ")
# 629 "iotk_dat.spp"
call iotk_error_msg(ierr,' ')
# 629 "iotk_dat.spp"
call iotk_error_write(ierr,"iostat",iostat)
            return
          end if
        end if
        call iotk_str_clean(line(1:nexttag - 1))
        call iotk_read(dat,line(1:nexttag - 1),index,ierr)
        if(ierr/=0) then
          call iotk_error_issue(ierr,"iotk_scan_dat_aux",__FILE__,__LINE__)
# 636 "iotk_dat.spp"
call iotk_error_msg(ierr,"CVS Revision: 1.25 ")
# 636 "iotk_dat.spp"
call iotk_error_msg(ierr,'Error reading REAL data')
          return
        end if
# 642 "iotk_dat.spp"
        if(index == size(dat)) exit
# 644 "iotk_dat.spp"
        if(nexttag/=length + 1) then
          call iotk_error_issue(ierr,"iotk_scan_dat_aux",__FILE__,__LINE__)
# 645 "iotk_dat.spp"
call iotk_error_msg(ierr,"CVS Revision: 1.25 ")
          return
        end if
      end do
    else
#ifdef __IOTK_WORKAROUND3
      read(lunit,fmt=fmt(1:iotk_strlen(fmt)),iostat=iostat) ( dat(j), j=1,ubound(dat,1) )
#else
      read(lunit,fmt=fmt(1:iotk_strlen(fmt)),iostat=iostat) dat
#endif
      if(iostat/=0) then
        call iotk_error_issue(ierr,"iotk_scan_dat_aux",__FILE__,__LINE__)
# 656 "iotk_dat.spp"
call iotk_error_msg(ierr,"CVS Revision: 1.25 ")
# 656 "iotk_dat.spp"
call iotk_error_msg(ierr,' ')
# 656 "iotk_dat.spp"
call iotk_error_write(ierr,"iostat",iostat)
        return
      end if
    end if
  end if
# 662 "iotk_dat.spp"
  if(idummy/=0) then
    call iotk_error_issue(ierr,"iotk_scan_dat_aux",__FILE__,__LINE__)
# 663 "iotk_dat.spp"
call iotk_error_msg(ierr,"CVS Revision: 1.25 ")
    return
  end if
end subroutine iotk_scan_dat_aux_REAL1
# 668 "iotk_dat.spp"

# 670 "iotk_dat.spp"
subroutine iotk_scan_dat_REAL1_1(unit,name,dat,dummy,attr,found,default,ierr)
  use iotk_base
  use iotk_error_interf
  use iotk_dat_interf, only: iotk_scan_dat_aux
  use iotk_scan_interf
  use iotk_str_interf
  use iotk_misc_interf
  implicit none
  integer, parameter :: this_kind = iotk_REAL1
  integer,                                   intent(in)  :: unit
  character(len=*),                          intent(in)  :: name
#ifdef __IOTK_WORKAROUND6
  REAL(kind=this_kind)                        :: dat (:)
#else
  REAL(kind=this_kind),           intent(out) :: dat (:)
#endif
  type(iotk_dummytype),            optional              :: dummy
#ifdef __IOTK_WORKAROUND6
  character(len=*),                optional              :: attr
#else
  character(len=*),                optional, intent(out) :: attr
#endif
  logical,                         optional, intent(out) :: found
  REAL(kind=this_kind), optional, intent(in)  :: default (:)
  integer,                         optional, intent(out) :: ierr
# 698 "iotk_dat.spp"
  REAL (kind=this_kind),              allocatable :: tmpdat(:)
# 700 "iotk_dat.spp"
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
# 717 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
    goto 1
  end if
  call iotk_parse_dat(lattr,rtype,rkind,rsize,rlen,fmt,columns,ierrl)
! Note that columns is not effectively used
  if(ierrl/=0) goto 1
  if(.not. (iotk_strcomp(rtype,iotk_eos) .or. iotk_strcomp(rtype,"REAL") ) ) then
    call iotk_error_issue(ierrl,"iotk_scan_dat",__FILE__,__LINE__)
# 724 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
# 724 "iotk_dat.spp"
call iotk_error_msg(ierrl,' ')
# 724 "iotk_dat.spp"
call iotk_error_write(ierrl,"rtype",rtype(1:iotk_strlen(rtype)))
# 724 "iotk_dat.spp"
call iotk_error_write(ierrl,"type","REAL")
    goto 1
  end if
  if(.not. (rsize==-1 .or. rsize==size(dat)) ) then
    call iotk_error_issue(ierrl,"iotk_scan_dat",__FILE__,__LINE__)
# 728 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
    goto 1
  end if
  if(rkind==-1) rkind = kind(dat)
# 735 "iotk_dat.spp"

  allocate(tmpdat(size(dat)))
  call iotk_scan_dat_aux(unit,tmpdat,rkind,rlen,fmt(1:iotk_strlen(fmt)),ierrl)
  if(ierrl/=0) then
    call iotk_error_issue(ierrl,"iotk_scan_dat",__FILE__,__LINE__)
# 739 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
# 739 "iotk_dat.spp"
call iotk_error_msg(ierrl,'Error reading data')
# 739 "iotk_dat.spp"
call iotk_error_write(ierrl,"name",name)
# 739 "iotk_dat.spp"
call iotk_error_write(ierrl,"rkind",rkind)
# 739 "iotk_dat.spp"
call iotk_error_write(ierrl,"rlen",rlen)
    goto 1
  end if
# 745 "iotk_dat.spp"
#if defined(__IOTK_WORKAROUND3) || defined(__IOTK_WORKAROUND4)
# 749 "iotk_dat.spp"
     call iotk_private_pack_REAL1(dat,tmpdat,size(tmpdat),1)
# 751 "iotk_dat.spp"
#else
     dat = reshape(tmpdat,shape(dat))
#endif
# 755 "iotk_dat.spp"
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
# 767 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
# 767 "iotk_dat.spp"
call iotk_error_msg(ierrl,'Dat not found')
# 767 "iotk_dat.spp"
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
end subroutine iotk_scan_dat_REAL1_1


#endif
#endif

subroutine iotk_dat_dummy_REAL1_1
  write(0,*)
end subroutine iotk_dat_dummy_REAL1_1


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

#ifdef __IOTK_REAL1
#if 2 <= __IOTK_MAXRANK
# 82 "iotk_dat.spp"
subroutine iotk_write_dat_REAL1_2(unit,name,dat,dummy,attr,columns,sep,fmt,ierr)
  use iotk_base
  use iotk_error_interf
  use iotk_attr_interf, only : iotk_write_attr
  use iotk_write_interf
  use iotk_fmt_interf
  use iotk_str_interf
  use iotk_unit_interf
  use iotk_misc_interf
  implicit none
  integer, parameter :: this_kind = iotk_REAL1
  integer,                                    intent(in)  :: unit
  character(len=*),                           intent(in)  :: name
  REAL (kind=this_kind),           intent(in)  :: dat (:,:) 
  type(iotk_dummytype),             optional              :: dummy
  character(len=*),                 optional, intent(in)  :: attr
  integer,                          optional, intent(in)  :: columns
  character(len=*),                 optional, intent(in)  :: sep
  character(len=*),                 optional, intent(in)  :: fmt
  integer,                          optional, intent(out) :: ierr
  integer :: ierrl,lunit,iostat
  logical :: binary,raw,stream
  integer :: lcolumns
  integer(iotk_header_kind), parameter :: idummy=0
  character(100) :: lsep
  character(300) :: usefmt
  character(iotk_attlenx) :: lattr
  character(iotk_attlenx) :: attr_tmp
  type (iotk_unit), pointer :: this
# 115 "iotk_dat.spp"
  REAL (kind=this_kind),allocatable :: dattmp(:)
# 117 "iotk_dat.spp"
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
# 126 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
      goto 1
    end if
  end if
  lunit = iotk_phys_unit(unit)
  call iotk_unit_get(lunit,pointer=this)
  raw = .false.
  if(associated(this)) then
    raw = this%raw
  end if
  call iotk_inquire(lunit,binary,stream,ierrl)
  if(ierrl/=0) then
    call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 138 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
    goto 1
  end if
  call iotk_strcpy(usefmt,"!",ierrl)
  if(ierrl/=0) then
    call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 143 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
    goto 1
  end if
  if(present(fmt) .and. .not. raw) call iotk_strcpy(usefmt,iotk_strtrim(fmt),ierrl)
  if(ierrl/=0) then
    call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 148 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
    goto 1
  end if
  if(iotk_strscan(usefmt,"<>&")/=0) then
    call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 152 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
# 152 "iotk_dat.spp"
call iotk_error_msg(ierrl,'Special characters (<>&) found in fmt string')
# 152 "iotk_dat.spp"
call iotk_error_write(ierrl,"unit",unit)
# 152 "iotk_dat.spp"
call iotk_error_write(ierrl,"name",trim(name))
# 152 "iotk_dat.spp"
call iotk_error_write(ierrl,"fmt",trim(fmt))
    goto 1
  end if
  call iotk_write_attr(lattr,"type",iotk_tolower("REAL"),first=.true.,ierr=ierrl)
  if(ierrl/=0) then
    call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 157 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
    goto 1
  end if
  call iotk_write_attr(lattr,"size",size(dat),ierr=ierrl)
  if(ierrl/=0) then
    call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 162 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
    goto 1
  end if
# 172 "iotk_dat.spp"
  if(binary) then
    call iotk_write_attr(lattr,"kind",kind(dat),ierr=ierrl)
    if(ierrl/=0) then
      call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 175 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
      goto 1
    end if
  end if
# 180 "iotk_dat.spp"
  if(.not.iotk_strcomp(usefmt,"!")) call iotk_write_attr(lattr,"fmt",iotk_strtrim(usefmt),ierr=ierrl)
  if(ierrl/=0) then
    call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 182 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
    goto 1
  end if
  if(lcolumns/=1) call iotk_write_attr(lattr,"columns",lcolumns,ierr=ierrl)
  if(ierrl/=0) then
    call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 187 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
    goto 1
  end if
  if(present(attr)) then
    attr_tmp(1:1)=iotk_eos
    call iotk_strcpy(attr_tmp,attr,ierr=ierrl)
    if(ierrl/=0) then
      call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 194 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
      goto 1
    end if
    call iotk_delete_attr(attr_tmp,"type",ierrl)
    if(ierrl/=0) then
      call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 199 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
      goto 1
    end if
    call iotk_delete_attr(attr_tmp,"kind",ierrl)
    if(ierrl/=0) then
      call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 204 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
      goto 1
    end if
    call iotk_delete_attr(attr_tmp,"size",ierrl)
    if(ierrl/=0) then
      call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 209 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
      goto 1
    end if
    call iotk_delete_attr(attr_tmp,"fmt",ierrl)
    if(ierrl/=0) then
      call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 214 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
      goto 1
    end if
    call iotk_delete_attr(attr_tmp,"columns",ierrl)
    if(ierrl/=0) then
      call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 219 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
      goto 1
    end if
    call iotk_delete_attr(attr_tmp,"len",ierrl)
    if(ierrl/=0) then
      call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 224 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
      goto 1
    end if
    if(iotk_strlen_trim(attr_tmp)>0) call iotk_strcat(lattr,iotk_strtrim(attr_tmp),ierr=ierrl)
  end if
  if(ierrl/=0) then
    call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 230 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
    goto 1
  end if
  call iotk_write_begin(unit,name,lattr,ierr=ierrl)
  if(ierrl/=0) then
    call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 235 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
    goto 1
  end if

  allocate(dattmp(size(dat)))
# 243 "iotk_dat.spp"
#if defined(__IOTK_WORKAROUND3) || defined(__IOTK_WORKAROUND4)
# 247 "iotk_dat.spp"
     call iotk_private_pack_REAL1(dattmp,dat,size(dattmp),1)
# 249 "iotk_dat.spp"
#else
     dattmp = pack(dat,mask=.true.)
#endif
# 253 "iotk_dat.spp"

  if(binary) then
    if(raw) then
      write(lunit,iostat=iostat) (dattmp(itmp),itmp=1,size(dattmp))
      if(iostat/=0) then
        call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 258 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
        goto 1
      end if
    else
      write(lunit,iostat=iostat) idummy,(dattmp(itmp),itmp=1,size(dattmp))
      if(iostat/=0) then
        call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 264 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
        goto 1
      end if
    end if
  else
    if(raw) then
# 273 "iotk_dat.spp"
      write(lunit,*,iostat=iostat) (dattmp(itmp),itmp=1,size(dattmp))
# 275 "iotk_dat.spp"
      if(iostat/=0) then
        call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 276 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
        goto 1
      end if
    else if(iotk_strcomp(usefmt,"*")) then
      write(lunit,*,iostat=iostat) (dattmp(itmp),itmp=1,size(dattmp))
      if(iostat/=0) then
        call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 282 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
        goto 1
      end if
    else if(iotk_strcomp(usefmt,"!")) then
# 296 "iotk_dat.spp"
     write(lunit,fmt=trim(iotk_wfmt("REAL",kind(dattmp),lcolumns,-1,lsep)),iostat=iostat) (dattmp(itmp),itmp=1,size(dattmp))
     if(iostat/=0) then
      call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 298 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
      goto 1
     end if
# 302 "iotk_dat.spp"
    else
      write(lunit,fmt=usefmt(1:iotk_strlen(usefmt)),iostat=iostat) (dattmp(itmp),itmp=1,size(dattmp))
      if(iostat/=0) then
        call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 305 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
        goto 1
      end if
    end if
  end if
  call iotk_write_end(unit,name,ierr=ierrl)
  if(ierrl/=0) then
    call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 312 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
    goto 1
  end if
1 continue
  if(allocated(dattmp)) deallocate(dattmp)
  if(present(ierr)) then
    ierr = ierrl
  else
    if(ierrl/=0) call iotk_error_handler(ierrl)
  end if
end subroutine iotk_write_dat_REAL1_2


# 668 "iotk_dat.spp"

# 670 "iotk_dat.spp"
subroutine iotk_scan_dat_REAL1_2(unit,name,dat,dummy,attr,found,default,ierr)
  use iotk_base
  use iotk_error_interf
  use iotk_dat_interf, only: iotk_scan_dat_aux
  use iotk_scan_interf
  use iotk_str_interf
  use iotk_misc_interf
  implicit none
  integer, parameter :: this_kind = iotk_REAL1
  integer,                                   intent(in)  :: unit
  character(len=*),                          intent(in)  :: name
#ifdef __IOTK_WORKAROUND6
  REAL(kind=this_kind)                        :: dat (:,:)
#else
  REAL(kind=this_kind),           intent(out) :: dat (:,:)
#endif
  type(iotk_dummytype),            optional              :: dummy
#ifdef __IOTK_WORKAROUND6
  character(len=*),                optional              :: attr
#else
  character(len=*),                optional, intent(out) :: attr
#endif
  logical,                         optional, intent(out) :: found
  REAL(kind=this_kind), optional, intent(in)  :: default (:,:)
  integer,                         optional, intent(out) :: ierr
# 698 "iotk_dat.spp"
  REAL (kind=this_kind),              allocatable :: tmpdat(:)
# 700 "iotk_dat.spp"
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
# 717 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
    goto 1
  end if
  call iotk_parse_dat(lattr,rtype,rkind,rsize,rlen,fmt,columns,ierrl)
! Note that columns is not effectively used
  if(ierrl/=0) goto 1
  if(.not. (iotk_strcomp(rtype,iotk_eos) .or. iotk_strcomp(rtype,"REAL") ) ) then
    call iotk_error_issue(ierrl,"iotk_scan_dat",__FILE__,__LINE__)
# 724 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
# 724 "iotk_dat.spp"
call iotk_error_msg(ierrl,' ')
# 724 "iotk_dat.spp"
call iotk_error_write(ierrl,"rtype",rtype(1:iotk_strlen(rtype)))
# 724 "iotk_dat.spp"
call iotk_error_write(ierrl,"type","REAL")
    goto 1
  end if
  if(.not. (rsize==-1 .or. rsize==size(dat)) ) then
    call iotk_error_issue(ierrl,"iotk_scan_dat",__FILE__,__LINE__)
# 728 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
    goto 1
  end if
  if(rkind==-1) rkind = kind(dat)
# 735 "iotk_dat.spp"

  allocate(tmpdat(size(dat)))
  call iotk_scan_dat_aux(unit,tmpdat,rkind,rlen,fmt(1:iotk_strlen(fmt)),ierrl)
  if(ierrl/=0) then
    call iotk_error_issue(ierrl,"iotk_scan_dat",__FILE__,__LINE__)
# 739 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
# 739 "iotk_dat.spp"
call iotk_error_msg(ierrl,'Error reading data')
# 739 "iotk_dat.spp"
call iotk_error_write(ierrl,"name",name)
# 739 "iotk_dat.spp"
call iotk_error_write(ierrl,"rkind",rkind)
# 739 "iotk_dat.spp"
call iotk_error_write(ierrl,"rlen",rlen)
    goto 1
  end if
# 745 "iotk_dat.spp"
#if defined(__IOTK_WORKAROUND3) || defined(__IOTK_WORKAROUND4)
# 749 "iotk_dat.spp"
     call iotk_private_pack_REAL1(dat,tmpdat,size(tmpdat),1)
# 751 "iotk_dat.spp"
#else
     dat = reshape(tmpdat,shape(dat))
#endif
# 755 "iotk_dat.spp"
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
# 767 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
# 767 "iotk_dat.spp"
call iotk_error_msg(ierrl,'Dat not found')
# 767 "iotk_dat.spp"
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
end subroutine iotk_scan_dat_REAL1_2


#endif
#endif

subroutine iotk_dat_dummy_REAL1_2
  write(0,*)
end subroutine iotk_dat_dummy_REAL1_2


# 45 "iotk_dat.spp"

# 48 "iotk_dat.spp"
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
# 65 "iotk_dat.spp"


!------------------------------------------------------------------------------!
! Inclusion of configuration file
#include "iotk_config.h"
!------------------------------------------------------------------------------!

# 74 "iotk_dat.spp"
#include "iotk_auxmacros.h"
# 76 "iotk_dat.spp"

# 78 "iotk_dat.spp"

#ifdef __IOTK_REAL1
#if 3 <= __IOTK_MAXRANK
# 82 "iotk_dat.spp"
subroutine iotk_write_dat_REAL1_3(unit,name,dat,dummy,attr,columns,sep,fmt,ierr)
  use iotk_base
  use iotk_error_interf
  use iotk_attr_interf, only : iotk_write_attr
  use iotk_write_interf
  use iotk_fmt_interf
  use iotk_str_interf
  use iotk_unit_interf
  use iotk_misc_interf
  implicit none
  integer, parameter :: this_kind = iotk_REAL1
  integer,                                    intent(in)  :: unit
  character(len=*),                           intent(in)  :: name
  REAL (kind=this_kind),           intent(in)  :: dat (:,:,:) 
  type(iotk_dummytype),             optional              :: dummy
  character(len=*),                 optional, intent(in)  :: attr
  integer,                          optional, intent(in)  :: columns
  character(len=*),                 optional, intent(in)  :: sep
  character(len=*),                 optional, intent(in)  :: fmt
  integer,                          optional, intent(out) :: ierr
  integer :: ierrl,lunit,iostat
  logical :: binary,raw,stream
  integer :: lcolumns
  integer(iotk_header_kind), parameter :: idummy=0
  character(100) :: lsep
  character(300) :: usefmt
  character(iotk_attlenx) :: lattr
  character(iotk_attlenx) :: attr_tmp
  type (iotk_unit), pointer :: this
# 115 "iotk_dat.spp"
  REAL (kind=this_kind),allocatable :: dattmp(:)
# 117 "iotk_dat.spp"
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
# 126 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
      goto 1
    end if
  end if
  lunit = iotk_phys_unit(unit)
  call iotk_unit_get(lunit,pointer=this)
  raw = .false.
  if(associated(this)) then
    raw = this%raw
  end if
  call iotk_inquire(lunit,binary,stream,ierrl)
  if(ierrl/=0) then
    call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 138 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
    goto 1
  end if
  call iotk_strcpy(usefmt,"!",ierrl)
  if(ierrl/=0) then
    call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 143 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
    goto 1
  end if
  if(present(fmt) .and. .not. raw) call iotk_strcpy(usefmt,iotk_strtrim(fmt),ierrl)
  if(ierrl/=0) then
    call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 148 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
    goto 1
  end if
  if(iotk_strscan(usefmt,"<>&")/=0) then
    call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 152 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
# 152 "iotk_dat.spp"
call iotk_error_msg(ierrl,'Special characters (<>&) found in fmt string')
# 152 "iotk_dat.spp"
call iotk_error_write(ierrl,"unit",unit)
# 152 "iotk_dat.spp"
call iotk_error_write(ierrl,"name",trim(name))
# 152 "iotk_dat.spp"
call iotk_error_write(ierrl,"fmt",trim(fmt))
    goto 1
  end if
  call iotk_write_attr(lattr,"type",iotk_tolower("REAL"),first=.true.,ierr=ierrl)
  if(ierrl/=0) then
    call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 157 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
    goto 1
  end if
  call iotk_write_attr(lattr,"size",size(dat),ierr=ierrl)
  if(ierrl/=0) then
    call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 162 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
    goto 1
  end if
# 172 "iotk_dat.spp"
  if(binary) then
    call iotk_write_attr(lattr,"kind",kind(dat),ierr=ierrl)
    if(ierrl/=0) then
      call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 175 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
      goto 1
    end if
  end if
# 180 "iotk_dat.spp"
  if(.not.iotk_strcomp(usefmt,"!")) call iotk_write_attr(lattr,"fmt",iotk_strtrim(usefmt),ierr=ierrl)
  if(ierrl/=0) then
    call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 182 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
    goto 1
  end if
  if(lcolumns/=1) call iotk_write_attr(lattr,"columns",lcolumns,ierr=ierrl)
  if(ierrl/=0) then
    call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 187 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
    goto 1
  end if
  if(present(attr)) then
    attr_tmp(1:1)=iotk_eos
    call iotk_strcpy(attr_tmp,attr,ierr=ierrl)
    if(ierrl/=0) then
      call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 194 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
      goto 1
    end if
    call iotk_delete_attr(attr_tmp,"type",ierrl)
    if(ierrl/=0) then
      call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 199 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
      goto 1
    end if
    call iotk_delete_attr(attr_tmp,"kind",ierrl)
    if(ierrl/=0) then
      call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 204 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
      goto 1
    end if
    call iotk_delete_attr(attr_tmp,"size",ierrl)
    if(ierrl/=0) then
      call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 209 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
      goto 1
    end if
    call iotk_delete_attr(attr_tmp,"fmt",ierrl)
    if(ierrl/=0) then
      call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 214 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
      goto 1
    end if
    call iotk_delete_attr(attr_tmp,"columns",ierrl)
    if(ierrl/=0) then
      call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 219 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
      goto 1
    end if
    call iotk_delete_attr(attr_tmp,"len",ierrl)
    if(ierrl/=0) then
      call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 224 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
      goto 1
    end if
    if(iotk_strlen_trim(attr_tmp)>0) call iotk_strcat(lattr,iotk_strtrim(attr_tmp),ierr=ierrl)
  end if
  if(ierrl/=0) then
    call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 230 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
    goto 1
  end if
  call iotk_write_begin(unit,name,lattr,ierr=ierrl)
  if(ierrl/=0) then
    call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 235 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
    goto 1
  end if

  allocate(dattmp(size(dat)))
# 243 "iotk_dat.spp"
#if defined(__IOTK_WORKAROUND3) || defined(__IOTK_WORKAROUND4)
# 247 "iotk_dat.spp"
     call iotk_private_pack_REAL1(dattmp,dat,size(dattmp),1)
# 249 "iotk_dat.spp"
#else
     dattmp = pack(dat,mask=.true.)
#endif
# 253 "iotk_dat.spp"

  if(binary) then
    if(raw) then
      write(lunit,iostat=iostat) (dattmp(itmp),itmp=1,size(dattmp))
      if(iostat/=0) then
        call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 258 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
        goto 1
      end if
    else
      write(lunit,iostat=iostat) idummy,(dattmp(itmp),itmp=1,size(dattmp))
      if(iostat/=0) then
        call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 264 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
        goto 1
      end if
    end if
  else
    if(raw) then
# 273 "iotk_dat.spp"
      write(lunit,*,iostat=iostat) (dattmp(itmp),itmp=1,size(dattmp))
# 275 "iotk_dat.spp"
      if(iostat/=0) then
        call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 276 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
        goto 1
      end if
    else if(iotk_strcomp(usefmt,"*")) then
      write(lunit,*,iostat=iostat) (dattmp(itmp),itmp=1,size(dattmp))
      if(iostat/=0) then
        call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 282 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
        goto 1
      end if
    else if(iotk_strcomp(usefmt,"!")) then
# 296 "iotk_dat.spp"
     write(lunit,fmt=trim(iotk_wfmt("REAL",kind(dattmp),lcolumns,-1,lsep)),iostat=iostat) (dattmp(itmp),itmp=1,size(dattmp))
     if(iostat/=0) then
      call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 298 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
      goto 1
     end if
# 302 "iotk_dat.spp"
    else
      write(lunit,fmt=usefmt(1:iotk_strlen(usefmt)),iostat=iostat) (dattmp(itmp),itmp=1,size(dattmp))
      if(iostat/=0) then
        call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 305 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
        goto 1
      end if
    end if
  end if
  call iotk_write_end(unit,name,ierr=ierrl)
  if(ierrl/=0) then
    call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 312 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
    goto 1
  end if
1 continue
  if(allocated(dattmp)) deallocate(dattmp)
  if(present(ierr)) then
    ierr = ierrl
  else
    if(ierrl/=0) call iotk_error_handler(ierrl)
  end if
end subroutine iotk_write_dat_REAL1_3


# 668 "iotk_dat.spp"

# 670 "iotk_dat.spp"
subroutine iotk_scan_dat_REAL1_3(unit,name,dat,dummy,attr,found,default,ierr)
  use iotk_base
  use iotk_error_interf
  use iotk_dat_interf, only: iotk_scan_dat_aux
  use iotk_scan_interf
  use iotk_str_interf
  use iotk_misc_interf
  implicit none
  integer, parameter :: this_kind = iotk_REAL1
  integer,                                   intent(in)  :: unit
  character(len=*),                          intent(in)  :: name
#ifdef __IOTK_WORKAROUND6
  REAL(kind=this_kind)                        :: dat (:,:,:)
#else
  REAL(kind=this_kind),           intent(out) :: dat (:,:,:)
#endif
  type(iotk_dummytype),            optional              :: dummy
#ifdef __IOTK_WORKAROUND6
  character(len=*),                optional              :: attr
#else
  character(len=*),                optional, intent(out) :: attr
#endif
  logical,                         optional, intent(out) :: found
  REAL(kind=this_kind), optional, intent(in)  :: default (:,:,:)
  integer,                         optional, intent(out) :: ierr
# 698 "iotk_dat.spp"
  REAL (kind=this_kind),              allocatable :: tmpdat(:)
# 700 "iotk_dat.spp"
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
# 717 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
    goto 1
  end if
  call iotk_parse_dat(lattr,rtype,rkind,rsize,rlen,fmt,columns,ierrl)
! Note that columns is not effectively used
  if(ierrl/=0) goto 1
  if(.not. (iotk_strcomp(rtype,iotk_eos) .or. iotk_strcomp(rtype,"REAL") ) ) then
    call iotk_error_issue(ierrl,"iotk_scan_dat",__FILE__,__LINE__)
# 724 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
# 724 "iotk_dat.spp"
call iotk_error_msg(ierrl,' ')
# 724 "iotk_dat.spp"
call iotk_error_write(ierrl,"rtype",rtype(1:iotk_strlen(rtype)))
# 724 "iotk_dat.spp"
call iotk_error_write(ierrl,"type","REAL")
    goto 1
  end if
  if(.not. (rsize==-1 .or. rsize==size(dat)) ) then
    call iotk_error_issue(ierrl,"iotk_scan_dat",__FILE__,__LINE__)
# 728 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
    goto 1
  end if
  if(rkind==-1) rkind = kind(dat)
# 735 "iotk_dat.spp"

  allocate(tmpdat(size(dat)))
  call iotk_scan_dat_aux(unit,tmpdat,rkind,rlen,fmt(1:iotk_strlen(fmt)),ierrl)
  if(ierrl/=0) then
    call iotk_error_issue(ierrl,"iotk_scan_dat",__FILE__,__LINE__)
# 739 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
# 739 "iotk_dat.spp"
call iotk_error_msg(ierrl,'Error reading data')
# 739 "iotk_dat.spp"
call iotk_error_write(ierrl,"name",name)
# 739 "iotk_dat.spp"
call iotk_error_write(ierrl,"rkind",rkind)
# 739 "iotk_dat.spp"
call iotk_error_write(ierrl,"rlen",rlen)
    goto 1
  end if
# 745 "iotk_dat.spp"
#if defined(__IOTK_WORKAROUND3) || defined(__IOTK_WORKAROUND4)
# 749 "iotk_dat.spp"
     call iotk_private_pack_REAL1(dat,tmpdat,size(tmpdat),1)
# 751 "iotk_dat.spp"
#else
     dat = reshape(tmpdat,shape(dat))
#endif
# 755 "iotk_dat.spp"
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
# 767 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
# 767 "iotk_dat.spp"
call iotk_error_msg(ierrl,'Dat not found')
# 767 "iotk_dat.spp"
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
end subroutine iotk_scan_dat_REAL1_3


#endif
#endif

subroutine iotk_dat_dummy_REAL1_3
  write(0,*)
end subroutine iotk_dat_dummy_REAL1_3


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

#ifdef __IOTK_REAL1
#if 4 <= __IOTK_MAXRANK
# 82 "iotk_dat.spp"
subroutine iotk_write_dat_REAL1_4(unit,name,dat,dummy,attr,columns,sep,fmt,ierr)
  use iotk_base
  use iotk_error_interf
  use iotk_attr_interf, only : iotk_write_attr
  use iotk_write_interf
  use iotk_fmt_interf
  use iotk_str_interf
  use iotk_unit_interf
  use iotk_misc_interf
  implicit none
  integer, parameter :: this_kind = iotk_REAL1
  integer,                                    intent(in)  :: unit
  character(len=*),                           intent(in)  :: name
  REAL (kind=this_kind),           intent(in)  :: dat (:,:,:,:) 
  type(iotk_dummytype),             optional              :: dummy
  character(len=*),                 optional, intent(in)  :: attr
  integer,                          optional, intent(in)  :: columns
  character(len=*),                 optional, intent(in)  :: sep
  character(len=*),                 optional, intent(in)  :: fmt
  integer,                          optional, intent(out) :: ierr
  integer :: ierrl,lunit,iostat
  logical :: binary,raw,stream
  integer :: lcolumns
  integer(iotk_header_kind), parameter :: idummy=0
  character(100) :: lsep
  character(300) :: usefmt
  character(iotk_attlenx) :: lattr
  character(iotk_attlenx) :: attr_tmp
  type (iotk_unit), pointer :: this
# 115 "iotk_dat.spp"
  REAL (kind=this_kind),allocatable :: dattmp(:)
# 117 "iotk_dat.spp"
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
# 126 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
      goto 1
    end if
  end if
  lunit = iotk_phys_unit(unit)
  call iotk_unit_get(lunit,pointer=this)
  raw = .false.
  if(associated(this)) then
    raw = this%raw
  end if
  call iotk_inquire(lunit,binary,stream,ierrl)
  if(ierrl/=0) then
    call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 138 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
    goto 1
  end if
  call iotk_strcpy(usefmt,"!",ierrl)
  if(ierrl/=0) then
    call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 143 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
    goto 1
  end if
  if(present(fmt) .and. .not. raw) call iotk_strcpy(usefmt,iotk_strtrim(fmt),ierrl)
  if(ierrl/=0) then
    call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 148 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
    goto 1
  end if
  if(iotk_strscan(usefmt,"<>&")/=0) then
    call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 152 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
# 152 "iotk_dat.spp"
call iotk_error_msg(ierrl,'Special characters (<>&) found in fmt string')
# 152 "iotk_dat.spp"
call iotk_error_write(ierrl,"unit",unit)
# 152 "iotk_dat.spp"
call iotk_error_write(ierrl,"name",trim(name))
# 152 "iotk_dat.spp"
call iotk_error_write(ierrl,"fmt",trim(fmt))
    goto 1
  end if
  call iotk_write_attr(lattr,"type",iotk_tolower("REAL"),first=.true.,ierr=ierrl)
  if(ierrl/=0) then
    call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 157 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
    goto 1
  end if
  call iotk_write_attr(lattr,"size",size(dat),ierr=ierrl)
  if(ierrl/=0) then
    call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 162 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
    goto 1
  end if
# 172 "iotk_dat.spp"
  if(binary) then
    call iotk_write_attr(lattr,"kind",kind(dat),ierr=ierrl)
    if(ierrl/=0) then
      call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 175 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
      goto 1
    end if
  end if
# 180 "iotk_dat.spp"
  if(.not.iotk_strcomp(usefmt,"!")) call iotk_write_attr(lattr,"fmt",iotk_strtrim(usefmt),ierr=ierrl)
  if(ierrl/=0) then
    call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 182 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
    goto 1
  end if
  if(lcolumns/=1) call iotk_write_attr(lattr,"columns",lcolumns,ierr=ierrl)
  if(ierrl/=0) then
    call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 187 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
    goto 1
  end if
  if(present(attr)) then
    attr_tmp(1:1)=iotk_eos
    call iotk_strcpy(attr_tmp,attr,ierr=ierrl)
    if(ierrl/=0) then
      call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 194 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
      goto 1
    end if
    call iotk_delete_attr(attr_tmp,"type",ierrl)
    if(ierrl/=0) then
      call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 199 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
      goto 1
    end if
    call iotk_delete_attr(attr_tmp,"kind",ierrl)
    if(ierrl/=0) then
      call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 204 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
      goto 1
    end if
    call iotk_delete_attr(attr_tmp,"size",ierrl)
    if(ierrl/=0) then
      call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 209 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
      goto 1
    end if
    call iotk_delete_attr(attr_tmp,"fmt",ierrl)
    if(ierrl/=0) then
      call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 214 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
      goto 1
    end if
    call iotk_delete_attr(attr_tmp,"columns",ierrl)
    if(ierrl/=0) then
      call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 219 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
      goto 1
    end if
    call iotk_delete_attr(attr_tmp,"len",ierrl)
    if(ierrl/=0) then
      call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 224 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
      goto 1
    end if
    if(iotk_strlen_trim(attr_tmp)>0) call iotk_strcat(lattr,iotk_strtrim(attr_tmp),ierr=ierrl)
  end if
  if(ierrl/=0) then
    call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 230 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
    goto 1
  end if
  call iotk_write_begin(unit,name,lattr,ierr=ierrl)
  if(ierrl/=0) then
    call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 235 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
    goto 1
  end if

  allocate(dattmp(size(dat)))
# 243 "iotk_dat.spp"
#if defined(__IOTK_WORKAROUND3) || defined(__IOTK_WORKAROUND4)
# 247 "iotk_dat.spp"
     call iotk_private_pack_REAL1(dattmp,dat,size(dattmp),1)
# 249 "iotk_dat.spp"
#else
     dattmp = pack(dat,mask=.true.)
#endif
# 253 "iotk_dat.spp"

  if(binary) then
    if(raw) then
      write(lunit,iostat=iostat) (dattmp(itmp),itmp=1,size(dattmp))
      if(iostat/=0) then
        call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 258 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
        goto 1
      end if
    else
      write(lunit,iostat=iostat) idummy,(dattmp(itmp),itmp=1,size(dattmp))
      if(iostat/=0) then
        call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 264 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
        goto 1
      end if
    end if
  else
    if(raw) then
# 273 "iotk_dat.spp"
      write(lunit,*,iostat=iostat) (dattmp(itmp),itmp=1,size(dattmp))
# 275 "iotk_dat.spp"
      if(iostat/=0) then
        call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 276 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
        goto 1
      end if
    else if(iotk_strcomp(usefmt,"*")) then
      write(lunit,*,iostat=iostat) (dattmp(itmp),itmp=1,size(dattmp))
      if(iostat/=0) then
        call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 282 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
        goto 1
      end if
    else if(iotk_strcomp(usefmt,"!")) then
# 296 "iotk_dat.spp"
     write(lunit,fmt=trim(iotk_wfmt("REAL",kind(dattmp),lcolumns,-1,lsep)),iostat=iostat) (dattmp(itmp),itmp=1,size(dattmp))
     if(iostat/=0) then
      call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 298 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
      goto 1
     end if
# 302 "iotk_dat.spp"
    else
      write(lunit,fmt=usefmt(1:iotk_strlen(usefmt)),iostat=iostat) (dattmp(itmp),itmp=1,size(dattmp))
      if(iostat/=0) then
        call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 305 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
        goto 1
      end if
    end if
  end if
  call iotk_write_end(unit,name,ierr=ierrl)
  if(ierrl/=0) then
    call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 312 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
    goto 1
  end if
1 continue
  if(allocated(dattmp)) deallocate(dattmp)
  if(present(ierr)) then
    ierr = ierrl
  else
    if(ierrl/=0) call iotk_error_handler(ierrl)
  end if
end subroutine iotk_write_dat_REAL1_4


# 668 "iotk_dat.spp"

# 670 "iotk_dat.spp"
subroutine iotk_scan_dat_REAL1_4(unit,name,dat,dummy,attr,found,default,ierr)
  use iotk_base
  use iotk_error_interf
  use iotk_dat_interf, only: iotk_scan_dat_aux
  use iotk_scan_interf
  use iotk_str_interf
  use iotk_misc_interf
  implicit none
  integer, parameter :: this_kind = iotk_REAL1
  integer,                                   intent(in)  :: unit
  character(len=*),                          intent(in)  :: name
#ifdef __IOTK_WORKAROUND6
  REAL(kind=this_kind)                        :: dat (:,:,:,:)
#else
  REAL(kind=this_kind),           intent(out) :: dat (:,:,:,:)
#endif
  type(iotk_dummytype),            optional              :: dummy
#ifdef __IOTK_WORKAROUND6
  character(len=*),                optional              :: attr
#else
  character(len=*),                optional, intent(out) :: attr
#endif
  logical,                         optional, intent(out) :: found
  REAL(kind=this_kind), optional, intent(in)  :: default (:,:,:,:)
  integer,                         optional, intent(out) :: ierr
# 698 "iotk_dat.spp"
  REAL (kind=this_kind),              allocatable :: tmpdat(:)
# 700 "iotk_dat.spp"
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
# 717 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
    goto 1
  end if
  call iotk_parse_dat(lattr,rtype,rkind,rsize,rlen,fmt,columns,ierrl)
! Note that columns is not effectively used
  if(ierrl/=0) goto 1
  if(.not. (iotk_strcomp(rtype,iotk_eos) .or. iotk_strcomp(rtype,"REAL") ) ) then
    call iotk_error_issue(ierrl,"iotk_scan_dat",__FILE__,__LINE__)
# 724 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
# 724 "iotk_dat.spp"
call iotk_error_msg(ierrl,' ')
# 724 "iotk_dat.spp"
call iotk_error_write(ierrl,"rtype",rtype(1:iotk_strlen(rtype)))
# 724 "iotk_dat.spp"
call iotk_error_write(ierrl,"type","REAL")
    goto 1
  end if
  if(.not. (rsize==-1 .or. rsize==size(dat)) ) then
    call iotk_error_issue(ierrl,"iotk_scan_dat",__FILE__,__LINE__)
# 728 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
    goto 1
  end if
  if(rkind==-1) rkind = kind(dat)
# 735 "iotk_dat.spp"

  allocate(tmpdat(size(dat)))
  call iotk_scan_dat_aux(unit,tmpdat,rkind,rlen,fmt(1:iotk_strlen(fmt)),ierrl)
  if(ierrl/=0) then
    call iotk_error_issue(ierrl,"iotk_scan_dat",__FILE__,__LINE__)
# 739 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
# 739 "iotk_dat.spp"
call iotk_error_msg(ierrl,'Error reading data')
# 739 "iotk_dat.spp"
call iotk_error_write(ierrl,"name",name)
# 739 "iotk_dat.spp"
call iotk_error_write(ierrl,"rkind",rkind)
# 739 "iotk_dat.spp"
call iotk_error_write(ierrl,"rlen",rlen)
    goto 1
  end if
# 745 "iotk_dat.spp"
#if defined(__IOTK_WORKAROUND3) || defined(__IOTK_WORKAROUND4)
# 749 "iotk_dat.spp"
     call iotk_private_pack_REAL1(dat,tmpdat,size(tmpdat),1)
# 751 "iotk_dat.spp"
#else
     dat = reshape(tmpdat,shape(dat))
#endif
# 755 "iotk_dat.spp"
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
# 767 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
# 767 "iotk_dat.spp"
call iotk_error_msg(ierrl,'Dat not found')
# 767 "iotk_dat.spp"
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
end subroutine iotk_scan_dat_REAL1_4


#endif
#endif

subroutine iotk_dat_dummy_REAL1_4
  write(0,*)
end subroutine iotk_dat_dummy_REAL1_4


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

#ifdef __IOTK_REAL1
#if 5 <= __IOTK_MAXRANK
# 82 "iotk_dat.spp"
subroutine iotk_write_dat_REAL1_5(unit,name,dat,dummy,attr,columns,sep,fmt,ierr)
  use iotk_base
  use iotk_error_interf
  use iotk_attr_interf, only : iotk_write_attr
  use iotk_write_interf
  use iotk_fmt_interf
  use iotk_str_interf
  use iotk_unit_interf
  use iotk_misc_interf
  implicit none
  integer, parameter :: this_kind = iotk_REAL1
  integer,                                    intent(in)  :: unit
  character(len=*),                           intent(in)  :: name
  REAL (kind=this_kind),           intent(in)  :: dat (:,:,:,:,:) 
  type(iotk_dummytype),             optional              :: dummy
  character(len=*),                 optional, intent(in)  :: attr
  integer,                          optional, intent(in)  :: columns
  character(len=*),                 optional, intent(in)  :: sep
  character(len=*),                 optional, intent(in)  :: fmt
  integer,                          optional, intent(out) :: ierr
  integer :: ierrl,lunit,iostat
  logical :: binary,raw,stream
  integer :: lcolumns
  integer(iotk_header_kind), parameter :: idummy=0
  character(100) :: lsep
  character(300) :: usefmt
  character(iotk_attlenx) :: lattr
  character(iotk_attlenx) :: attr_tmp
  type (iotk_unit), pointer :: this
# 115 "iotk_dat.spp"
  REAL (kind=this_kind),allocatable :: dattmp(:)
# 117 "iotk_dat.spp"
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
# 126 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
      goto 1
    end if
  end if
  lunit = iotk_phys_unit(unit)
  call iotk_unit_get(lunit,pointer=this)
  raw = .false.
  if(associated(this)) then
    raw = this%raw
  end if
  call iotk_inquire(lunit,binary,stream,ierrl)
  if(ierrl/=0) then
    call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 138 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
    goto 1
  end if
  call iotk_strcpy(usefmt,"!",ierrl)
  if(ierrl/=0) then
    call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 143 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
    goto 1
  end if
  if(present(fmt) .and. .not. raw) call iotk_strcpy(usefmt,iotk_strtrim(fmt),ierrl)
  if(ierrl/=0) then
    call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 148 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
    goto 1
  end if
  if(iotk_strscan(usefmt,"<>&")/=0) then
    call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 152 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
# 152 "iotk_dat.spp"
call iotk_error_msg(ierrl,'Special characters (<>&) found in fmt string')
# 152 "iotk_dat.spp"
call iotk_error_write(ierrl,"unit",unit)
# 152 "iotk_dat.spp"
call iotk_error_write(ierrl,"name",trim(name))
# 152 "iotk_dat.spp"
call iotk_error_write(ierrl,"fmt",trim(fmt))
    goto 1
  end if
  call iotk_write_attr(lattr,"type",iotk_tolower("REAL"),first=.true.,ierr=ierrl)
  if(ierrl/=0) then
    call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 157 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
    goto 1
  end if
  call iotk_write_attr(lattr,"size",size(dat),ierr=ierrl)
  if(ierrl/=0) then
    call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 162 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
    goto 1
  end if
# 172 "iotk_dat.spp"
  if(binary) then
    call iotk_write_attr(lattr,"kind",kind(dat),ierr=ierrl)
    if(ierrl/=0) then
      call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 175 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
      goto 1
    end if
  end if
# 180 "iotk_dat.spp"
  if(.not.iotk_strcomp(usefmt,"!")) call iotk_write_attr(lattr,"fmt",iotk_strtrim(usefmt),ierr=ierrl)
  if(ierrl/=0) then
    call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 182 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
    goto 1
  end if
  if(lcolumns/=1) call iotk_write_attr(lattr,"columns",lcolumns,ierr=ierrl)
  if(ierrl/=0) then
    call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 187 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
    goto 1
  end if
  if(present(attr)) then
    attr_tmp(1:1)=iotk_eos
    call iotk_strcpy(attr_tmp,attr,ierr=ierrl)
    if(ierrl/=0) then
      call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 194 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
      goto 1
    end if
    call iotk_delete_attr(attr_tmp,"type",ierrl)
    if(ierrl/=0) then
      call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 199 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
      goto 1
    end if
    call iotk_delete_attr(attr_tmp,"kind",ierrl)
    if(ierrl/=0) then
      call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 204 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
      goto 1
    end if
    call iotk_delete_attr(attr_tmp,"size",ierrl)
    if(ierrl/=0) then
      call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 209 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
      goto 1
    end if
    call iotk_delete_attr(attr_tmp,"fmt",ierrl)
    if(ierrl/=0) then
      call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 214 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
      goto 1
    end if
    call iotk_delete_attr(attr_tmp,"columns",ierrl)
    if(ierrl/=0) then
      call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 219 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
      goto 1
    end if
    call iotk_delete_attr(attr_tmp,"len",ierrl)
    if(ierrl/=0) then
      call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 224 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
      goto 1
    end if
    if(iotk_strlen_trim(attr_tmp)>0) call iotk_strcat(lattr,iotk_strtrim(attr_tmp),ierr=ierrl)
  end if
  if(ierrl/=0) then
    call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 230 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
    goto 1
  end if
  call iotk_write_begin(unit,name,lattr,ierr=ierrl)
  if(ierrl/=0) then
    call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 235 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
    goto 1
  end if

  allocate(dattmp(size(dat)))
# 243 "iotk_dat.spp"
#if defined(__IOTK_WORKAROUND3) || defined(__IOTK_WORKAROUND4)
# 247 "iotk_dat.spp"
     call iotk_private_pack_REAL1(dattmp,dat,size(dattmp),1)
# 249 "iotk_dat.spp"
#else
     dattmp = pack(dat,mask=.true.)
#endif
# 253 "iotk_dat.spp"

  if(binary) then
    if(raw) then
      write(lunit,iostat=iostat) (dattmp(itmp),itmp=1,size(dattmp))
      if(iostat/=0) then
        call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 258 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
        goto 1
      end if
    else
      write(lunit,iostat=iostat) idummy,(dattmp(itmp),itmp=1,size(dattmp))
      if(iostat/=0) then
        call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 264 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
        goto 1
      end if
    end if
  else
    if(raw) then
# 273 "iotk_dat.spp"
      write(lunit,*,iostat=iostat) (dattmp(itmp),itmp=1,size(dattmp))
# 275 "iotk_dat.spp"
      if(iostat/=0) then
        call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 276 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
        goto 1
      end if
    else if(iotk_strcomp(usefmt,"*")) then
      write(lunit,*,iostat=iostat) (dattmp(itmp),itmp=1,size(dattmp))
      if(iostat/=0) then
        call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 282 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
        goto 1
      end if
    else if(iotk_strcomp(usefmt,"!")) then
# 296 "iotk_dat.spp"
     write(lunit,fmt=trim(iotk_wfmt("REAL",kind(dattmp),lcolumns,-1,lsep)),iostat=iostat) (dattmp(itmp),itmp=1,size(dattmp))
     if(iostat/=0) then
      call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 298 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
      goto 1
     end if
# 302 "iotk_dat.spp"
    else
      write(lunit,fmt=usefmt(1:iotk_strlen(usefmt)),iostat=iostat) (dattmp(itmp),itmp=1,size(dattmp))
      if(iostat/=0) then
        call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 305 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
        goto 1
      end if
    end if
  end if
  call iotk_write_end(unit,name,ierr=ierrl)
  if(ierrl/=0) then
    call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 312 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
    goto 1
  end if
1 continue
  if(allocated(dattmp)) deallocate(dattmp)
  if(present(ierr)) then
    ierr = ierrl
  else
    if(ierrl/=0) call iotk_error_handler(ierrl)
  end if
end subroutine iotk_write_dat_REAL1_5


# 668 "iotk_dat.spp"

# 670 "iotk_dat.spp"
subroutine iotk_scan_dat_REAL1_5(unit,name,dat,dummy,attr,found,default,ierr)
  use iotk_base
  use iotk_error_interf
  use iotk_dat_interf, only: iotk_scan_dat_aux
  use iotk_scan_interf
  use iotk_str_interf
  use iotk_misc_interf
  implicit none
  integer, parameter :: this_kind = iotk_REAL1
  integer,                                   intent(in)  :: unit
  character(len=*),                          intent(in)  :: name
#ifdef __IOTK_WORKAROUND6
  REAL(kind=this_kind)                        :: dat (:,:,:,:,:)
#else
  REAL(kind=this_kind),           intent(out) :: dat (:,:,:,:,:)
#endif
  type(iotk_dummytype),            optional              :: dummy
#ifdef __IOTK_WORKAROUND6
  character(len=*),                optional              :: attr
#else
  character(len=*),                optional, intent(out) :: attr
#endif
  logical,                         optional, intent(out) :: found
  REAL(kind=this_kind), optional, intent(in)  :: default (:,:,:,:,:)
  integer,                         optional, intent(out) :: ierr
# 698 "iotk_dat.spp"
  REAL (kind=this_kind),              allocatable :: tmpdat(:)
# 700 "iotk_dat.spp"
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
# 717 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
    goto 1
  end if
  call iotk_parse_dat(lattr,rtype,rkind,rsize,rlen,fmt,columns,ierrl)
! Note that columns is not effectively used
  if(ierrl/=0) goto 1
  if(.not. (iotk_strcomp(rtype,iotk_eos) .or. iotk_strcomp(rtype,"REAL") ) ) then
    call iotk_error_issue(ierrl,"iotk_scan_dat",__FILE__,__LINE__)
# 724 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
# 724 "iotk_dat.spp"
call iotk_error_msg(ierrl,' ')
# 724 "iotk_dat.spp"
call iotk_error_write(ierrl,"rtype",rtype(1:iotk_strlen(rtype)))
# 724 "iotk_dat.spp"
call iotk_error_write(ierrl,"type","REAL")
    goto 1
  end if
  if(.not. (rsize==-1 .or. rsize==size(dat)) ) then
    call iotk_error_issue(ierrl,"iotk_scan_dat",__FILE__,__LINE__)
# 728 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
    goto 1
  end if
  if(rkind==-1) rkind = kind(dat)
# 735 "iotk_dat.spp"

  allocate(tmpdat(size(dat)))
  call iotk_scan_dat_aux(unit,tmpdat,rkind,rlen,fmt(1:iotk_strlen(fmt)),ierrl)
  if(ierrl/=0) then
    call iotk_error_issue(ierrl,"iotk_scan_dat",__FILE__,__LINE__)
# 739 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
# 739 "iotk_dat.spp"
call iotk_error_msg(ierrl,'Error reading data')
# 739 "iotk_dat.spp"
call iotk_error_write(ierrl,"name",name)
# 739 "iotk_dat.spp"
call iotk_error_write(ierrl,"rkind",rkind)
# 739 "iotk_dat.spp"
call iotk_error_write(ierrl,"rlen",rlen)
    goto 1
  end if
# 745 "iotk_dat.spp"
#if defined(__IOTK_WORKAROUND3) || defined(__IOTK_WORKAROUND4)
# 749 "iotk_dat.spp"
     call iotk_private_pack_REAL1(dat,tmpdat,size(tmpdat),1)
# 751 "iotk_dat.spp"
#else
     dat = reshape(tmpdat,shape(dat))
#endif
# 755 "iotk_dat.spp"
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
# 767 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
# 767 "iotk_dat.spp"
call iotk_error_msg(ierrl,'Dat not found')
# 767 "iotk_dat.spp"
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
end subroutine iotk_scan_dat_REAL1_5


#endif
#endif

subroutine iotk_dat_dummy_REAL1_5
  write(0,*)
end subroutine iotk_dat_dummy_REAL1_5


# 45 "iotk_dat.spp"

# 48 "iotk_dat.spp"
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
# 65 "iotk_dat.spp"


!------------------------------------------------------------------------------!
! Inclusion of configuration file
#include "iotk_config.h"
!------------------------------------------------------------------------------!

# 74 "iotk_dat.spp"
#include "iotk_auxmacros.h"
# 76 "iotk_dat.spp"

# 78 "iotk_dat.spp"

#ifdef __IOTK_REAL1
#if 6 <= __IOTK_MAXRANK
# 82 "iotk_dat.spp"
subroutine iotk_write_dat_REAL1_6(unit,name,dat,dummy,attr,columns,sep,fmt,ierr)
  use iotk_base
  use iotk_error_interf
  use iotk_attr_interf, only : iotk_write_attr
  use iotk_write_interf
  use iotk_fmt_interf
  use iotk_str_interf
  use iotk_unit_interf
  use iotk_misc_interf
  implicit none
  integer, parameter :: this_kind = iotk_REAL1
  integer,                                    intent(in)  :: unit
  character(len=*),                           intent(in)  :: name
  REAL (kind=this_kind),           intent(in)  :: dat (:,:,:,:,:,:) 
  type(iotk_dummytype),             optional              :: dummy
  character(len=*),                 optional, intent(in)  :: attr
  integer,                          optional, intent(in)  :: columns
  character(len=*),                 optional, intent(in)  :: sep
  character(len=*),                 optional, intent(in)  :: fmt
  integer,                          optional, intent(out) :: ierr
  integer :: ierrl,lunit,iostat
  logical :: binary,raw,stream
  integer :: lcolumns
  integer(iotk_header_kind), parameter :: idummy=0
  character(100) :: lsep
  character(300) :: usefmt
  character(iotk_attlenx) :: lattr
  character(iotk_attlenx) :: attr_tmp
  type (iotk_unit), pointer :: this
# 115 "iotk_dat.spp"
  REAL (kind=this_kind),allocatable :: dattmp(:)
# 117 "iotk_dat.spp"
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
# 126 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
      goto 1
    end if
  end if
  lunit = iotk_phys_unit(unit)
  call iotk_unit_get(lunit,pointer=this)
  raw = .false.
  if(associated(this)) then
    raw = this%raw
  end if
  call iotk_inquire(lunit,binary,stream,ierrl)
  if(ierrl/=0) then
    call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 138 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
    goto 1
  end if
  call iotk_strcpy(usefmt,"!",ierrl)
  if(ierrl/=0) then
    call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 143 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
    goto 1
  end if
  if(present(fmt) .and. .not. raw) call iotk_strcpy(usefmt,iotk_strtrim(fmt),ierrl)
  if(ierrl/=0) then
    call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 148 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
    goto 1
  end if
  if(iotk_strscan(usefmt,"<>&")/=0) then
    call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 152 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
# 152 "iotk_dat.spp"
call iotk_error_msg(ierrl,'Special characters (<>&) found in fmt string')
# 152 "iotk_dat.spp"
call iotk_error_write(ierrl,"unit",unit)
# 152 "iotk_dat.spp"
call iotk_error_write(ierrl,"name",trim(name))
# 152 "iotk_dat.spp"
call iotk_error_write(ierrl,"fmt",trim(fmt))
    goto 1
  end if
  call iotk_write_attr(lattr,"type",iotk_tolower("REAL"),first=.true.,ierr=ierrl)
  if(ierrl/=0) then
    call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 157 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
    goto 1
  end if
  call iotk_write_attr(lattr,"size",size(dat),ierr=ierrl)
  if(ierrl/=0) then
    call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 162 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
    goto 1
  end if
# 172 "iotk_dat.spp"
  if(binary) then
    call iotk_write_attr(lattr,"kind",kind(dat),ierr=ierrl)
    if(ierrl/=0) then
      call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 175 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
      goto 1
    end if
  end if
# 180 "iotk_dat.spp"
  if(.not.iotk_strcomp(usefmt,"!")) call iotk_write_attr(lattr,"fmt",iotk_strtrim(usefmt),ierr=ierrl)
  if(ierrl/=0) then
    call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 182 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
    goto 1
  end if
  if(lcolumns/=1) call iotk_write_attr(lattr,"columns",lcolumns,ierr=ierrl)
  if(ierrl/=0) then
    call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 187 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
    goto 1
  end if
  if(present(attr)) then
    attr_tmp(1:1)=iotk_eos
    call iotk_strcpy(attr_tmp,attr,ierr=ierrl)
    if(ierrl/=0) then
      call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 194 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
      goto 1
    end if
    call iotk_delete_attr(attr_tmp,"type",ierrl)
    if(ierrl/=0) then
      call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 199 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
      goto 1
    end if
    call iotk_delete_attr(attr_tmp,"kind",ierrl)
    if(ierrl/=0) then
      call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 204 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
      goto 1
    end if
    call iotk_delete_attr(attr_tmp,"size",ierrl)
    if(ierrl/=0) then
      call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 209 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
      goto 1
    end if
    call iotk_delete_attr(attr_tmp,"fmt",ierrl)
    if(ierrl/=0) then
      call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 214 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
      goto 1
    end if
    call iotk_delete_attr(attr_tmp,"columns",ierrl)
    if(ierrl/=0) then
      call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 219 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
      goto 1
    end if
    call iotk_delete_attr(attr_tmp,"len",ierrl)
    if(ierrl/=0) then
      call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 224 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
      goto 1
    end if
    if(iotk_strlen_trim(attr_tmp)>0) call iotk_strcat(lattr,iotk_strtrim(attr_tmp),ierr=ierrl)
  end if
  if(ierrl/=0) then
    call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 230 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
    goto 1
  end if
  call iotk_write_begin(unit,name,lattr,ierr=ierrl)
  if(ierrl/=0) then
    call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 235 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
    goto 1
  end if

  allocate(dattmp(size(dat)))
# 243 "iotk_dat.spp"
#if defined(__IOTK_WORKAROUND3) || defined(__IOTK_WORKAROUND4)
# 247 "iotk_dat.spp"
     call iotk_private_pack_REAL1(dattmp,dat,size(dattmp),1)
# 249 "iotk_dat.spp"
#else
     dattmp = pack(dat,mask=.true.)
#endif
# 253 "iotk_dat.spp"

  if(binary) then
    if(raw) then
      write(lunit,iostat=iostat) (dattmp(itmp),itmp=1,size(dattmp))
      if(iostat/=0) then
        call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 258 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
        goto 1
      end if
    else
      write(lunit,iostat=iostat) idummy,(dattmp(itmp),itmp=1,size(dattmp))
      if(iostat/=0) then
        call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 264 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
        goto 1
      end if
    end if
  else
    if(raw) then
# 273 "iotk_dat.spp"
      write(lunit,*,iostat=iostat) (dattmp(itmp),itmp=1,size(dattmp))
# 275 "iotk_dat.spp"
      if(iostat/=0) then
        call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 276 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
        goto 1
      end if
    else if(iotk_strcomp(usefmt,"*")) then
      write(lunit,*,iostat=iostat) (dattmp(itmp),itmp=1,size(dattmp))
      if(iostat/=0) then
        call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 282 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
        goto 1
      end if
    else if(iotk_strcomp(usefmt,"!")) then
# 296 "iotk_dat.spp"
     write(lunit,fmt=trim(iotk_wfmt("REAL",kind(dattmp),lcolumns,-1,lsep)),iostat=iostat) (dattmp(itmp),itmp=1,size(dattmp))
     if(iostat/=0) then
      call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 298 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
      goto 1
     end if
# 302 "iotk_dat.spp"
    else
      write(lunit,fmt=usefmt(1:iotk_strlen(usefmt)),iostat=iostat) (dattmp(itmp),itmp=1,size(dattmp))
      if(iostat/=0) then
        call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 305 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
        goto 1
      end if
    end if
  end if
  call iotk_write_end(unit,name,ierr=ierrl)
  if(ierrl/=0) then
    call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 312 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
    goto 1
  end if
1 continue
  if(allocated(dattmp)) deallocate(dattmp)
  if(present(ierr)) then
    ierr = ierrl
  else
    if(ierrl/=0) call iotk_error_handler(ierrl)
  end if
end subroutine iotk_write_dat_REAL1_6


# 668 "iotk_dat.spp"

# 670 "iotk_dat.spp"
subroutine iotk_scan_dat_REAL1_6(unit,name,dat,dummy,attr,found,default,ierr)
  use iotk_base
  use iotk_error_interf
  use iotk_dat_interf, only: iotk_scan_dat_aux
  use iotk_scan_interf
  use iotk_str_interf
  use iotk_misc_interf
  implicit none
  integer, parameter :: this_kind = iotk_REAL1
  integer,                                   intent(in)  :: unit
  character(len=*),                          intent(in)  :: name
#ifdef __IOTK_WORKAROUND6
  REAL(kind=this_kind)                        :: dat (:,:,:,:,:,:)
#else
  REAL(kind=this_kind),           intent(out) :: dat (:,:,:,:,:,:)
#endif
  type(iotk_dummytype),            optional              :: dummy
#ifdef __IOTK_WORKAROUND6
  character(len=*),                optional              :: attr
#else
  character(len=*),                optional, intent(out) :: attr
#endif
  logical,                         optional, intent(out) :: found
  REAL(kind=this_kind), optional, intent(in)  :: default (:,:,:,:,:,:)
  integer,                         optional, intent(out) :: ierr
# 698 "iotk_dat.spp"
  REAL (kind=this_kind),              allocatable :: tmpdat(:)
# 700 "iotk_dat.spp"
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
# 717 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
    goto 1
  end if
  call iotk_parse_dat(lattr,rtype,rkind,rsize,rlen,fmt,columns,ierrl)
! Note that columns is not effectively used
  if(ierrl/=0) goto 1
  if(.not. (iotk_strcomp(rtype,iotk_eos) .or. iotk_strcomp(rtype,"REAL") ) ) then
    call iotk_error_issue(ierrl,"iotk_scan_dat",__FILE__,__LINE__)
# 724 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
# 724 "iotk_dat.spp"
call iotk_error_msg(ierrl,' ')
# 724 "iotk_dat.spp"
call iotk_error_write(ierrl,"rtype",rtype(1:iotk_strlen(rtype)))
# 724 "iotk_dat.spp"
call iotk_error_write(ierrl,"type","REAL")
    goto 1
  end if
  if(.not. (rsize==-1 .or. rsize==size(dat)) ) then
    call iotk_error_issue(ierrl,"iotk_scan_dat",__FILE__,__LINE__)
# 728 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
    goto 1
  end if
  if(rkind==-1) rkind = kind(dat)
# 735 "iotk_dat.spp"

  allocate(tmpdat(size(dat)))
  call iotk_scan_dat_aux(unit,tmpdat,rkind,rlen,fmt(1:iotk_strlen(fmt)),ierrl)
  if(ierrl/=0) then
    call iotk_error_issue(ierrl,"iotk_scan_dat",__FILE__,__LINE__)
# 739 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
# 739 "iotk_dat.spp"
call iotk_error_msg(ierrl,'Error reading data')
# 739 "iotk_dat.spp"
call iotk_error_write(ierrl,"name",name)
# 739 "iotk_dat.spp"
call iotk_error_write(ierrl,"rkind",rkind)
# 739 "iotk_dat.spp"
call iotk_error_write(ierrl,"rlen",rlen)
    goto 1
  end if
# 745 "iotk_dat.spp"
#if defined(__IOTK_WORKAROUND3) || defined(__IOTK_WORKAROUND4)
# 749 "iotk_dat.spp"
     call iotk_private_pack_REAL1(dat,tmpdat,size(tmpdat),1)
# 751 "iotk_dat.spp"
#else
     dat = reshape(tmpdat,shape(dat))
#endif
# 755 "iotk_dat.spp"
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
# 767 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
# 767 "iotk_dat.spp"
call iotk_error_msg(ierrl,'Dat not found')
# 767 "iotk_dat.spp"
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
end subroutine iotk_scan_dat_REAL1_6


#endif
#endif

subroutine iotk_dat_dummy_REAL1_6
  write(0,*)
end subroutine iotk_dat_dummy_REAL1_6


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

#ifdef __IOTK_REAL1
#if 7 <= __IOTK_MAXRANK
# 82 "iotk_dat.spp"
subroutine iotk_write_dat_REAL1_7(unit,name,dat,dummy,attr,columns,sep,fmt,ierr)
  use iotk_base
  use iotk_error_interf
  use iotk_attr_interf, only : iotk_write_attr
  use iotk_write_interf
  use iotk_fmt_interf
  use iotk_str_interf
  use iotk_unit_interf
  use iotk_misc_interf
  implicit none
  integer, parameter :: this_kind = iotk_REAL1
  integer,                                    intent(in)  :: unit
  character(len=*),                           intent(in)  :: name
  REAL (kind=this_kind),           intent(in)  :: dat (:,:,:,:,:,:,:) 
  type(iotk_dummytype),             optional              :: dummy
  character(len=*),                 optional, intent(in)  :: attr
  integer,                          optional, intent(in)  :: columns
  character(len=*),                 optional, intent(in)  :: sep
  character(len=*),                 optional, intent(in)  :: fmt
  integer,                          optional, intent(out) :: ierr
  integer :: ierrl,lunit,iostat
  logical :: binary,raw,stream
  integer :: lcolumns
  integer(iotk_header_kind), parameter :: idummy=0
  character(100) :: lsep
  character(300) :: usefmt
  character(iotk_attlenx) :: lattr
  character(iotk_attlenx) :: attr_tmp
  type (iotk_unit), pointer :: this
# 115 "iotk_dat.spp"
  REAL (kind=this_kind),allocatable :: dattmp(:)
# 117 "iotk_dat.spp"
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
# 126 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
      goto 1
    end if
  end if
  lunit = iotk_phys_unit(unit)
  call iotk_unit_get(lunit,pointer=this)
  raw = .false.
  if(associated(this)) then
    raw = this%raw
  end if
  call iotk_inquire(lunit,binary,stream,ierrl)
  if(ierrl/=0) then
    call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 138 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
    goto 1
  end if
  call iotk_strcpy(usefmt,"!",ierrl)
  if(ierrl/=0) then
    call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 143 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
    goto 1
  end if
  if(present(fmt) .and. .not. raw) call iotk_strcpy(usefmt,iotk_strtrim(fmt),ierrl)
  if(ierrl/=0) then
    call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 148 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
    goto 1
  end if
  if(iotk_strscan(usefmt,"<>&")/=0) then
    call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 152 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
# 152 "iotk_dat.spp"
call iotk_error_msg(ierrl,'Special characters (<>&) found in fmt string')
# 152 "iotk_dat.spp"
call iotk_error_write(ierrl,"unit",unit)
# 152 "iotk_dat.spp"
call iotk_error_write(ierrl,"name",trim(name))
# 152 "iotk_dat.spp"
call iotk_error_write(ierrl,"fmt",trim(fmt))
    goto 1
  end if
  call iotk_write_attr(lattr,"type",iotk_tolower("REAL"),first=.true.,ierr=ierrl)
  if(ierrl/=0) then
    call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 157 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
    goto 1
  end if
  call iotk_write_attr(lattr,"size",size(dat),ierr=ierrl)
  if(ierrl/=0) then
    call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 162 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
    goto 1
  end if
# 172 "iotk_dat.spp"
  if(binary) then
    call iotk_write_attr(lattr,"kind",kind(dat),ierr=ierrl)
    if(ierrl/=0) then
      call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 175 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
      goto 1
    end if
  end if
# 180 "iotk_dat.spp"
  if(.not.iotk_strcomp(usefmt,"!")) call iotk_write_attr(lattr,"fmt",iotk_strtrim(usefmt),ierr=ierrl)
  if(ierrl/=0) then
    call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 182 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
    goto 1
  end if
  if(lcolumns/=1) call iotk_write_attr(lattr,"columns",lcolumns,ierr=ierrl)
  if(ierrl/=0) then
    call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 187 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
    goto 1
  end if
  if(present(attr)) then
    attr_tmp(1:1)=iotk_eos
    call iotk_strcpy(attr_tmp,attr,ierr=ierrl)
    if(ierrl/=0) then
      call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 194 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
      goto 1
    end if
    call iotk_delete_attr(attr_tmp,"type",ierrl)
    if(ierrl/=0) then
      call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 199 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
      goto 1
    end if
    call iotk_delete_attr(attr_tmp,"kind",ierrl)
    if(ierrl/=0) then
      call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 204 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
      goto 1
    end if
    call iotk_delete_attr(attr_tmp,"size",ierrl)
    if(ierrl/=0) then
      call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 209 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
      goto 1
    end if
    call iotk_delete_attr(attr_tmp,"fmt",ierrl)
    if(ierrl/=0) then
      call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 214 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
      goto 1
    end if
    call iotk_delete_attr(attr_tmp,"columns",ierrl)
    if(ierrl/=0) then
      call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 219 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
      goto 1
    end if
    call iotk_delete_attr(attr_tmp,"len",ierrl)
    if(ierrl/=0) then
      call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 224 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
      goto 1
    end if
    if(iotk_strlen_trim(attr_tmp)>0) call iotk_strcat(lattr,iotk_strtrim(attr_tmp),ierr=ierrl)
  end if
  if(ierrl/=0) then
    call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 230 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
    goto 1
  end if
  call iotk_write_begin(unit,name,lattr,ierr=ierrl)
  if(ierrl/=0) then
    call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 235 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
    goto 1
  end if

  allocate(dattmp(size(dat)))
# 243 "iotk_dat.spp"
#if defined(__IOTK_WORKAROUND3) || defined(__IOTK_WORKAROUND4)
# 247 "iotk_dat.spp"
     call iotk_private_pack_REAL1(dattmp,dat,size(dattmp),1)
# 249 "iotk_dat.spp"
#else
     dattmp = pack(dat,mask=.true.)
#endif
# 253 "iotk_dat.spp"

  if(binary) then
    if(raw) then
      write(lunit,iostat=iostat) (dattmp(itmp),itmp=1,size(dattmp))
      if(iostat/=0) then
        call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 258 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
        goto 1
      end if
    else
      write(lunit,iostat=iostat) idummy,(dattmp(itmp),itmp=1,size(dattmp))
      if(iostat/=0) then
        call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 264 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
        goto 1
      end if
    end if
  else
    if(raw) then
# 273 "iotk_dat.spp"
      write(lunit,*,iostat=iostat) (dattmp(itmp),itmp=1,size(dattmp))
# 275 "iotk_dat.spp"
      if(iostat/=0) then
        call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 276 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
        goto 1
      end if
    else if(iotk_strcomp(usefmt,"*")) then
      write(lunit,*,iostat=iostat) (dattmp(itmp),itmp=1,size(dattmp))
      if(iostat/=0) then
        call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 282 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
        goto 1
      end if
    else if(iotk_strcomp(usefmt,"!")) then
# 296 "iotk_dat.spp"
     write(lunit,fmt=trim(iotk_wfmt("REAL",kind(dattmp),lcolumns,-1,lsep)),iostat=iostat) (dattmp(itmp),itmp=1,size(dattmp))
     if(iostat/=0) then
      call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 298 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
      goto 1
     end if
# 302 "iotk_dat.spp"
    else
      write(lunit,fmt=usefmt(1:iotk_strlen(usefmt)),iostat=iostat) (dattmp(itmp),itmp=1,size(dattmp))
      if(iostat/=0) then
        call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 305 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
        goto 1
      end if
    end if
  end if
  call iotk_write_end(unit,name,ierr=ierrl)
  if(ierrl/=0) then
    call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 312 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
    goto 1
  end if
1 continue
  if(allocated(dattmp)) deallocate(dattmp)
  if(present(ierr)) then
    ierr = ierrl
  else
    if(ierrl/=0) call iotk_error_handler(ierrl)
  end if
end subroutine iotk_write_dat_REAL1_7


# 668 "iotk_dat.spp"

# 670 "iotk_dat.spp"
subroutine iotk_scan_dat_REAL1_7(unit,name,dat,dummy,attr,found,default,ierr)
  use iotk_base
  use iotk_error_interf
  use iotk_dat_interf, only: iotk_scan_dat_aux
  use iotk_scan_interf
  use iotk_str_interf
  use iotk_misc_interf
  implicit none
  integer, parameter :: this_kind = iotk_REAL1
  integer,                                   intent(in)  :: unit
  character(len=*),                          intent(in)  :: name
#ifdef __IOTK_WORKAROUND6
  REAL(kind=this_kind)                        :: dat (:,:,:,:,:,:,:)
#else
  REAL(kind=this_kind),           intent(out) :: dat (:,:,:,:,:,:,:)
#endif
  type(iotk_dummytype),            optional              :: dummy
#ifdef __IOTK_WORKAROUND6
  character(len=*),                optional              :: attr
#else
  character(len=*),                optional, intent(out) :: attr
#endif
  logical,                         optional, intent(out) :: found
  REAL(kind=this_kind), optional, intent(in)  :: default (:,:,:,:,:,:,:)
  integer,                         optional, intent(out) :: ierr
# 698 "iotk_dat.spp"
  REAL (kind=this_kind),              allocatable :: tmpdat(:)
# 700 "iotk_dat.spp"
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
# 717 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
    goto 1
  end if
  call iotk_parse_dat(lattr,rtype,rkind,rsize,rlen,fmt,columns,ierrl)
! Note that columns is not effectively used
  if(ierrl/=0) goto 1
  if(.not. (iotk_strcomp(rtype,iotk_eos) .or. iotk_strcomp(rtype,"REAL") ) ) then
    call iotk_error_issue(ierrl,"iotk_scan_dat",__FILE__,__LINE__)
# 724 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
# 724 "iotk_dat.spp"
call iotk_error_msg(ierrl,' ')
# 724 "iotk_dat.spp"
call iotk_error_write(ierrl,"rtype",rtype(1:iotk_strlen(rtype)))
# 724 "iotk_dat.spp"
call iotk_error_write(ierrl,"type","REAL")
    goto 1
  end if
  if(.not. (rsize==-1 .or. rsize==size(dat)) ) then
    call iotk_error_issue(ierrl,"iotk_scan_dat",__FILE__,__LINE__)
# 728 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
    goto 1
  end if
  if(rkind==-1) rkind = kind(dat)
# 735 "iotk_dat.spp"

  allocate(tmpdat(size(dat)))
  call iotk_scan_dat_aux(unit,tmpdat,rkind,rlen,fmt(1:iotk_strlen(fmt)),ierrl)
  if(ierrl/=0) then
    call iotk_error_issue(ierrl,"iotk_scan_dat",__FILE__,__LINE__)
# 739 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
# 739 "iotk_dat.spp"
call iotk_error_msg(ierrl,'Error reading data')
# 739 "iotk_dat.spp"
call iotk_error_write(ierrl,"name",name)
# 739 "iotk_dat.spp"
call iotk_error_write(ierrl,"rkind",rkind)
# 739 "iotk_dat.spp"
call iotk_error_write(ierrl,"rlen",rlen)
    goto 1
  end if
# 745 "iotk_dat.spp"
#if defined(__IOTK_WORKAROUND3) || defined(__IOTK_WORKAROUND4)
# 749 "iotk_dat.spp"
     call iotk_private_pack_REAL1(dat,tmpdat,size(tmpdat),1)
# 751 "iotk_dat.spp"
#else
     dat = reshape(tmpdat,shape(dat))
#endif
# 755 "iotk_dat.spp"
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
# 767 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
# 767 "iotk_dat.spp"
call iotk_error_msg(ierrl,'Dat not found')
# 767 "iotk_dat.spp"
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
end subroutine iotk_scan_dat_REAL1_7


#endif
#endif

subroutine iotk_dat_dummy_REAL1_7
  write(0,*)
end subroutine iotk_dat_dummy_REAL1_7


# 45 "iotk_dat.spp"

# 48 "iotk_dat.spp"
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
# 65 "iotk_dat.spp"


!------------------------------------------------------------------------------!
! Inclusion of configuration file
#include "iotk_config.h"
!------------------------------------------------------------------------------!

# 74 "iotk_dat.spp"
#include "iotk_auxmacros.h"
# 76 "iotk_dat.spp"

# 78 "iotk_dat.spp"

#ifdef __IOTK_REAL2
#if 0 <= __IOTK_MAXRANK
# 82 "iotk_dat.spp"
subroutine iotk_write_dat_REAL2_0(unit,name,dat,dummy,attr,columns,sep,fmt,ierr)
  use iotk_base
  use iotk_error_interf
  use iotk_attr_interf, only : iotk_write_attr
  use iotk_write_interf
  use iotk_fmt_interf
  use iotk_str_interf
  use iotk_unit_interf
  use iotk_misc_interf
  implicit none
  integer, parameter :: this_kind = iotk_REAL2
  integer,                                    intent(in)  :: unit
  character(len=*),                           intent(in)  :: name
  REAL (kind=this_kind),           intent(in)  :: dat  
  type(iotk_dummytype),             optional              :: dummy
  character(len=*),                 optional, intent(in)  :: attr
  integer,                          optional, intent(in)  :: columns
  character(len=*),                 optional, intent(in)  :: sep
  character(len=*),                 optional, intent(in)  :: fmt
  integer,                          optional, intent(out) :: ierr
  integer :: ierrl,lunit,iostat
  logical :: binary,raw,stream
  integer :: lcolumns
  integer(iotk_header_kind), parameter :: idummy=0
  character(100) :: lsep
  character(300) :: usefmt
  character(iotk_attlenx) :: lattr
  character(iotk_attlenx) :: attr_tmp
  type (iotk_unit), pointer :: this
# 115 "iotk_dat.spp"
  REAL (kind=this_kind),allocatable :: dattmp(:)
# 117 "iotk_dat.spp"
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
# 126 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
      goto 1
    end if
  end if
  lunit = iotk_phys_unit(unit)
  call iotk_unit_get(lunit,pointer=this)
  raw = .false.
  if(associated(this)) then
    raw = this%raw
  end if
  call iotk_inquire(lunit,binary,stream,ierrl)
  if(ierrl/=0) then
    call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 138 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
    goto 1
  end if
  call iotk_strcpy(usefmt,"!",ierrl)
  if(ierrl/=0) then
    call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 143 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
    goto 1
  end if
  if(present(fmt) .and. .not. raw) call iotk_strcpy(usefmt,iotk_strtrim(fmt),ierrl)
  if(ierrl/=0) then
    call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 148 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
    goto 1
  end if
  if(iotk_strscan(usefmt,"<>&")/=0) then
    call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 152 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
# 152 "iotk_dat.spp"
call iotk_error_msg(ierrl,'Special characters (<>&) found in fmt string')
# 152 "iotk_dat.spp"
call iotk_error_write(ierrl,"unit",unit)
# 152 "iotk_dat.spp"
call iotk_error_write(ierrl,"name",trim(name))
# 152 "iotk_dat.spp"
call iotk_error_write(ierrl,"fmt",trim(fmt))
    goto 1
  end if
  call iotk_write_attr(lattr,"type",iotk_tolower("REAL"),first=.true.,ierr=ierrl)
  if(ierrl/=0) then
    call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 157 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
    goto 1
  end if
  call iotk_write_attr(lattr,"size",1,ierr=ierrl)
  if(ierrl/=0) then
    call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 162 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
    goto 1
  end if
# 172 "iotk_dat.spp"
  if(binary) then
    call iotk_write_attr(lattr,"kind",kind(dat),ierr=ierrl)
    if(ierrl/=0) then
      call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 175 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
      goto 1
    end if
  end if
# 180 "iotk_dat.spp"
  if(.not.iotk_strcomp(usefmt,"!")) call iotk_write_attr(lattr,"fmt",iotk_strtrim(usefmt),ierr=ierrl)
  if(ierrl/=0) then
    call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 182 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
    goto 1
  end if
  if(lcolumns/=1) call iotk_write_attr(lattr,"columns",lcolumns,ierr=ierrl)
  if(ierrl/=0) then
    call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 187 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
    goto 1
  end if
  if(present(attr)) then
    attr_tmp(1:1)=iotk_eos
    call iotk_strcpy(attr_tmp,attr,ierr=ierrl)
    if(ierrl/=0) then
      call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 194 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
      goto 1
    end if
    call iotk_delete_attr(attr_tmp,"type",ierrl)
    if(ierrl/=0) then
      call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 199 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
      goto 1
    end if
    call iotk_delete_attr(attr_tmp,"kind",ierrl)
    if(ierrl/=0) then
      call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 204 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
      goto 1
    end if
    call iotk_delete_attr(attr_tmp,"size",ierrl)
    if(ierrl/=0) then
      call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 209 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
      goto 1
    end if
    call iotk_delete_attr(attr_tmp,"fmt",ierrl)
    if(ierrl/=0) then
      call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 214 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
      goto 1
    end if
    call iotk_delete_attr(attr_tmp,"columns",ierrl)
    if(ierrl/=0) then
      call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 219 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
      goto 1
    end if
    call iotk_delete_attr(attr_tmp,"len",ierrl)
    if(ierrl/=0) then
      call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 224 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
      goto 1
    end if
    if(iotk_strlen_trim(attr_tmp)>0) call iotk_strcat(lattr,iotk_strtrim(attr_tmp),ierr=ierrl)
  end if
  if(ierrl/=0) then
    call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 230 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
    goto 1
  end if
  call iotk_write_begin(unit,name,lattr,ierr=ierrl)
  if(ierrl/=0) then
    call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 235 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
    goto 1
  end if

  allocate(dattmp(1))
# 241 "iotk_dat.spp"
     dattmp(1) = dat
# 253 "iotk_dat.spp"

  if(binary) then
    if(raw) then
      write(lunit,iostat=iostat) (dattmp(itmp),itmp=1,size(dattmp))
      if(iostat/=0) then
        call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 258 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
        goto 1
      end if
    else
      write(lunit,iostat=iostat) idummy,(dattmp(itmp),itmp=1,size(dattmp))
      if(iostat/=0) then
        call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 264 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
        goto 1
      end if
    end if
  else
    if(raw) then
# 273 "iotk_dat.spp"
      write(lunit,*,iostat=iostat) (dattmp(itmp),itmp=1,size(dattmp))
# 275 "iotk_dat.spp"
      if(iostat/=0) then
        call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 276 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
        goto 1
      end if
    else if(iotk_strcomp(usefmt,"*")) then
      write(lunit,*,iostat=iostat) (dattmp(itmp),itmp=1,size(dattmp))
      if(iostat/=0) then
        call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 282 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
        goto 1
      end if
    else if(iotk_strcomp(usefmt,"!")) then
# 296 "iotk_dat.spp"
     write(lunit,fmt=trim(iotk_wfmt("REAL",kind(dattmp),lcolumns,-1,lsep)),iostat=iostat) (dattmp(itmp),itmp=1,size(dattmp))
     if(iostat/=0) then
      call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 298 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
      goto 1
     end if
# 302 "iotk_dat.spp"
    else
      write(lunit,fmt=usefmt(1:iotk_strlen(usefmt)),iostat=iostat) (dattmp(itmp),itmp=1,size(dattmp))
      if(iostat/=0) then
        call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 305 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
        goto 1
      end if
    end if
  end if
  call iotk_write_end(unit,name,ierr=ierrl)
  if(ierrl/=0) then
    call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 312 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
    goto 1
  end if
1 continue
  if(allocated(dattmp)) deallocate(dattmp)
  if(present(ierr)) then
    ierr = ierrl
  else
    if(ierrl/=0) call iotk_error_handler(ierrl)
  end if
end subroutine iotk_write_dat_REAL2_0


# 668 "iotk_dat.spp"

# 670 "iotk_dat.spp"
subroutine iotk_scan_dat_REAL2_0(unit,name,dat,dummy,attr,found,default,ierr)
  use iotk_base
  use iotk_error_interf
  use iotk_dat_interf, only: iotk_scan_dat_aux
  use iotk_scan_interf
  use iotk_str_interf
  use iotk_misc_interf
  implicit none
  integer, parameter :: this_kind = iotk_REAL2
  integer,                                   intent(in)  :: unit
  character(len=*),                          intent(in)  :: name
#ifdef __IOTK_WORKAROUND6
  REAL(kind=this_kind)                        :: dat 
#else
  REAL(kind=this_kind),           intent(out) :: dat 
#endif
  type(iotk_dummytype),            optional              :: dummy
#ifdef __IOTK_WORKAROUND6
  character(len=*),                optional              :: attr
#else
  character(len=*),                optional, intent(out) :: attr
#endif
  logical,                         optional, intent(out) :: found
  REAL(kind=this_kind), optional, intent(in)  :: default 
  integer,                         optional, intent(out) :: ierr
# 698 "iotk_dat.spp"
  REAL (kind=this_kind),              allocatable :: tmpdat(:)
# 700 "iotk_dat.spp"
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
# 717 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
    goto 1
  end if
  call iotk_parse_dat(lattr,rtype,rkind,rsize,rlen,fmt,columns,ierrl)
! Note that columns is not effectively used
  if(ierrl/=0) goto 1
  if(.not. (iotk_strcomp(rtype,iotk_eos) .or. iotk_strcomp(rtype,"REAL") ) ) then
    call iotk_error_issue(ierrl,"iotk_scan_dat",__FILE__,__LINE__)
# 724 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
# 724 "iotk_dat.spp"
call iotk_error_msg(ierrl,' ')
# 724 "iotk_dat.spp"
call iotk_error_write(ierrl,"rtype",rtype(1:iotk_strlen(rtype)))
# 724 "iotk_dat.spp"
call iotk_error_write(ierrl,"type","REAL")
    goto 1
  end if
  if(.not. (rsize==-1 .or. rsize==1) ) then
    call iotk_error_issue(ierrl,"iotk_scan_dat",__FILE__,__LINE__)
# 728 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
    goto 1
  end if
  if(rkind==-1) rkind = kind(dat)
# 735 "iotk_dat.spp"

  allocate(tmpdat(1))
  call iotk_scan_dat_aux(unit,tmpdat,rkind,rlen,fmt(1:iotk_strlen(fmt)),ierrl)
  if(ierrl/=0) then
    call iotk_error_issue(ierrl,"iotk_scan_dat",__FILE__,__LINE__)
# 739 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
# 739 "iotk_dat.spp"
call iotk_error_msg(ierrl,'Error reading data')
# 739 "iotk_dat.spp"
call iotk_error_write(ierrl,"name",name)
# 739 "iotk_dat.spp"
call iotk_error_write(ierrl,"rkind",rkind)
# 739 "iotk_dat.spp"
call iotk_error_write(ierrl,"rlen",rlen)
    goto 1
  end if
# 743 "iotk_dat.spp"
     dat = tmpdat(1)
# 755 "iotk_dat.spp"
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
# 767 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
# 767 "iotk_dat.spp"
call iotk_error_msg(ierrl,'Dat not found')
# 767 "iotk_dat.spp"
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
end subroutine iotk_scan_dat_REAL2_0


#endif
#endif

subroutine iotk_dat_dummy_REAL2_0
  write(0,*)
end subroutine iotk_dat_dummy_REAL2_0


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

#ifdef __IOTK_REAL2
#if 1 <= __IOTK_MAXRANK
# 82 "iotk_dat.spp"
subroutine iotk_write_dat_REAL2_1(unit,name,dat,dummy,attr,columns,sep,fmt,ierr)
  use iotk_base
  use iotk_error_interf
  use iotk_attr_interf, only : iotk_write_attr
  use iotk_write_interf
  use iotk_fmt_interf
  use iotk_str_interf
  use iotk_unit_interf
  use iotk_misc_interf
  implicit none
  integer, parameter :: this_kind = iotk_REAL2
  integer,                                    intent(in)  :: unit
  character(len=*),                           intent(in)  :: name
  REAL (kind=this_kind),           intent(in)  :: dat (:) 
  type(iotk_dummytype),             optional              :: dummy
  character(len=*),                 optional, intent(in)  :: attr
  integer,                          optional, intent(in)  :: columns
  character(len=*),                 optional, intent(in)  :: sep
  character(len=*),                 optional, intent(in)  :: fmt
  integer,                          optional, intent(out) :: ierr
  integer :: ierrl,lunit,iostat
  logical :: binary,raw,stream
  integer :: lcolumns
  integer(iotk_header_kind), parameter :: idummy=0
  character(100) :: lsep
  character(300) :: usefmt
  character(iotk_attlenx) :: lattr
  character(iotk_attlenx) :: attr_tmp
  type (iotk_unit), pointer :: this
# 115 "iotk_dat.spp"
  REAL (kind=this_kind),allocatable :: dattmp(:)
# 117 "iotk_dat.spp"
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
# 126 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
      goto 1
    end if
  end if
  lunit = iotk_phys_unit(unit)
  call iotk_unit_get(lunit,pointer=this)
  raw = .false.
  if(associated(this)) then
    raw = this%raw
  end if
  call iotk_inquire(lunit,binary,stream,ierrl)
  if(ierrl/=0) then
    call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 138 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
    goto 1
  end if
  call iotk_strcpy(usefmt,"!",ierrl)
  if(ierrl/=0) then
    call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 143 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
    goto 1
  end if
  if(present(fmt) .and. .not. raw) call iotk_strcpy(usefmt,iotk_strtrim(fmt),ierrl)
  if(ierrl/=0) then
    call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 148 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
    goto 1
  end if
  if(iotk_strscan(usefmt,"<>&")/=0) then
    call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 152 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
# 152 "iotk_dat.spp"
call iotk_error_msg(ierrl,'Special characters (<>&) found in fmt string')
# 152 "iotk_dat.spp"
call iotk_error_write(ierrl,"unit",unit)
# 152 "iotk_dat.spp"
call iotk_error_write(ierrl,"name",trim(name))
# 152 "iotk_dat.spp"
call iotk_error_write(ierrl,"fmt",trim(fmt))
    goto 1
  end if
  call iotk_write_attr(lattr,"type",iotk_tolower("REAL"),first=.true.,ierr=ierrl)
  if(ierrl/=0) then
    call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 157 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
    goto 1
  end if
  call iotk_write_attr(lattr,"size",size(dat),ierr=ierrl)
  if(ierrl/=0) then
    call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 162 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
    goto 1
  end if
# 172 "iotk_dat.spp"
  if(binary) then
    call iotk_write_attr(lattr,"kind",kind(dat),ierr=ierrl)
    if(ierrl/=0) then
      call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 175 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
      goto 1
    end if
  end if
# 180 "iotk_dat.spp"
  if(.not.iotk_strcomp(usefmt,"!")) call iotk_write_attr(lattr,"fmt",iotk_strtrim(usefmt),ierr=ierrl)
  if(ierrl/=0) then
    call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 182 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
    goto 1
  end if
  if(lcolumns/=1) call iotk_write_attr(lattr,"columns",lcolumns,ierr=ierrl)
  if(ierrl/=0) then
    call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 187 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
    goto 1
  end if
  if(present(attr)) then
    attr_tmp(1:1)=iotk_eos
    call iotk_strcpy(attr_tmp,attr,ierr=ierrl)
    if(ierrl/=0) then
      call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 194 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
      goto 1
    end if
    call iotk_delete_attr(attr_tmp,"type",ierrl)
    if(ierrl/=0) then
      call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 199 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
      goto 1
    end if
    call iotk_delete_attr(attr_tmp,"kind",ierrl)
    if(ierrl/=0) then
      call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 204 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
      goto 1
    end if
    call iotk_delete_attr(attr_tmp,"size",ierrl)
    if(ierrl/=0) then
      call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 209 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
      goto 1
    end if
    call iotk_delete_attr(attr_tmp,"fmt",ierrl)
    if(ierrl/=0) then
      call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 214 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
      goto 1
    end if
    call iotk_delete_attr(attr_tmp,"columns",ierrl)
    if(ierrl/=0) then
      call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 219 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
      goto 1
    end if
    call iotk_delete_attr(attr_tmp,"len",ierrl)
    if(ierrl/=0) then
      call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 224 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
      goto 1
    end if
    if(iotk_strlen_trim(attr_tmp)>0) call iotk_strcat(lattr,iotk_strtrim(attr_tmp),ierr=ierrl)
  end if
  if(ierrl/=0) then
    call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 230 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
    goto 1
  end if
  call iotk_write_begin(unit,name,lattr,ierr=ierrl)
  if(ierrl/=0) then
    call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 235 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
    goto 1
  end if

  allocate(dattmp(size(dat)))
# 243 "iotk_dat.spp"
#if defined(__IOTK_WORKAROUND3) || defined(__IOTK_WORKAROUND4)
# 247 "iotk_dat.spp"
     call iotk_private_pack_REAL2(dattmp,dat,size(dattmp),1)
# 249 "iotk_dat.spp"
#else
     dattmp = pack(dat,mask=.true.)
#endif
# 253 "iotk_dat.spp"

  if(binary) then
    if(raw) then
      write(lunit,iostat=iostat) (dattmp(itmp),itmp=1,size(dattmp))
      if(iostat/=0) then
        call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 258 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
        goto 1
      end if
    else
      write(lunit,iostat=iostat) idummy,(dattmp(itmp),itmp=1,size(dattmp))
      if(iostat/=0) then
        call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 264 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
        goto 1
      end if
    end if
  else
    if(raw) then
# 273 "iotk_dat.spp"
      write(lunit,*,iostat=iostat) (dattmp(itmp),itmp=1,size(dattmp))
# 275 "iotk_dat.spp"
      if(iostat/=0) then
        call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 276 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
        goto 1
      end if
    else if(iotk_strcomp(usefmt,"*")) then
      write(lunit,*,iostat=iostat) (dattmp(itmp),itmp=1,size(dattmp))
      if(iostat/=0) then
        call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 282 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
        goto 1
      end if
    else if(iotk_strcomp(usefmt,"!")) then
# 296 "iotk_dat.spp"
     write(lunit,fmt=trim(iotk_wfmt("REAL",kind(dattmp),lcolumns,-1,lsep)),iostat=iostat) (dattmp(itmp),itmp=1,size(dattmp))
     if(iostat/=0) then
      call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 298 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
      goto 1
     end if
# 302 "iotk_dat.spp"
    else
      write(lunit,fmt=usefmt(1:iotk_strlen(usefmt)),iostat=iostat) (dattmp(itmp),itmp=1,size(dattmp))
      if(iostat/=0) then
        call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 305 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
        goto 1
      end if
    end if
  end if
  call iotk_write_end(unit,name,ierr=ierrl)
  if(ierrl/=0) then
    call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 312 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
    goto 1
  end if
1 continue
  if(allocated(dattmp)) deallocate(dattmp)
  if(present(ierr)) then
    ierr = ierrl
  else
    if(ierrl/=0) call iotk_error_handler(ierrl)
  end if
end subroutine iotk_write_dat_REAL2_1


# 327 "iotk_dat.spp"
recursive subroutine iotk_scan_dat_aux_REAL2(unit,dat,rkind,rlen,fmt,ierr)
  use iotk_base
  use iotk_error_interf
  use iotk_attr_interf, only: iotk_read
  use iotk_scan_interf
  use iotk_str_interf
  use iotk_unit_interf
  use iotk_misc_interf
  use iotk_stream_interf
  implicit none
  integer, parameter :: this_kind = iotk_REAL2
  integer,                         intent(in)  :: unit
#ifdef __IOTK_WORKAROUND6
  REAL(kind=this_kind)              :: dat (:)
#else
  REAL(kind=this_kind), intent(out) :: dat (:)
#endif
  integer,                         intent(in)  :: rkind
  integer,                         intent(in)  :: rlen
  character(len=*),                intent(in)  :: fmt
  integer,                         intent(out) :: ierr
  integer(iotk_header_kind) :: idummy
  logical :: raw,binary,stream
  integer :: lunit
# 352 "iotk_dat.spp"
  integer :: i
# 354 "iotk_dat.spp"
#ifdef __IOTK_WORKAROUND3
  integer :: j
#endif
  integer :: index,length,nexttag,iostat,altlength
  type(iotk_unit), pointer :: this
  character(len=iotk_linlenx) :: line,altline
# 365 "iotk_dat.spp"
#ifdef __IOTK_REAL1
  REAL (kind=iotk_REAL1), allocatable :: dat1 (:)
#endif
# 365 "iotk_dat.spp"
#ifdef __IOTK_REAL3
  REAL (kind=iotk_REAL3), allocatable :: dat3 (:)
#endif
# 365 "iotk_dat.spp"
#ifdef __IOTK_REAL4
  REAL (kind=iotk_REAL4), allocatable :: dat4 (:)
#endif
# 371 "iotk_dat.spp"
  lunit = iotk_phys_unit(unit)
  ierr = 0
  iostat = 0
  idummy = 0
  call iotk_unit_get(lunit,pointer=this)
  raw = .false.
  if(associated(this)) then
    raw = this%raw
  end if
  call iotk_inquire(lunit,binary,stream,ierr)
  if(ierr/=0) then
    call iotk_error_issue(ierr,"iotk_scan_dat_aux",__FILE__,__LINE__)
# 382 "iotk_dat.spp"
call iotk_error_msg(ierr,"CVS Revision: 1.25 ")
    return
  end if
# 500 "iotk_dat.spp"
  if(binary) then
    select case(rkind)
    case(kind(dat))
      if(raw) then
#ifdef __IOTK_WORKAROUND3
        read(lunit,iostat=iostat) ( dat(j), j=1,ubound(dat,1) )
#else
        read(lunit,iostat=iostat) dat
#endif
        if(iostat/=0) then
          call iotk_error_issue(ierr,"iotk_scan_dat_aux",__FILE__,__LINE__)
# 510 "iotk_dat.spp"
call iotk_error_msg(ierr,"CVS Revision: 1.25 ")
# 510 "iotk_dat.spp"
call iotk_error_msg(ierr,' ')
# 510 "iotk_dat.spp"
call iotk_error_write(ierr,"iostat",iostat)
          return
        end if
      else
        if(stream) then
          call iotk_stream_read(lunit,idummy,dat,ierr=ierr)
          if(ierr/=0) then
            call iotk_error_issue(ierr,"iotk_scan_dat_aux",__FILE__,__LINE__)
# 517 "iotk_dat.spp"
call iotk_error_msg(ierr,"CVS Revision: 1.25 ")
            return
          end if
        else
#ifdef __IOTK_WORKAROUND3
          read(lunit,iostat=iostat) idummy, ( dat(j), j=1,ubound(dat,1) )
#else
          read(lunit,iostat=iostat) idummy, dat
#endif
        end if
        if(iostat/=0) then
          call iotk_error_issue(ierr,"iotk_scan_dat_aux",__FILE__,__LINE__)
# 528 "iotk_dat.spp"
call iotk_error_msg(ierr,"CVS Revision: 1.25 ")
# 528 "iotk_dat.spp"
call iotk_error_msg(ierr,' ')
# 528 "iotk_dat.spp"
call iotk_error_write(ierr,"iostat",iostat)
          return
        end if
      end if
# 534 "iotk_dat.spp"
#ifdef __IOTK_REAL1
    case(kind(dat1))
      ! Giusto per scrupolo. Se e' raw non ci sono info sul kind, quindi questa linea e' irraggiungibile
      if(raw) then
        call iotk_error_issue(ierr,"iotk_scan_dat_aux",__FILE__,__LINE__)
# 538 "iotk_dat.spp"
call iotk_error_msg(ierr,"CVS Revision: 1.25 ")
        return
      end if
      allocate(dat1(ubound(dat,1)))
      if(stream) then
        call iotk_stream_read(lunit,idummy,dat1,ierr=ierr)
        if(ierr/=0) then
          deallocate(dat1)
          call iotk_error_issue(ierr,"iotk_scan_dat_aux",__FILE__,__LINE__)
# 546 "iotk_dat.spp"
call iotk_error_msg(ierr,"CVS Revision: 1.25 ")
# 546 "iotk_dat.spp"
call iotk_error_msg(ierr,' ')
# 546 "iotk_dat.spp"
call iotk_error_write(ierr,"iostat",iostat)
          return
        end if
      else
        read(lunit,iostat=iostat) idummy,( dat1(i), i=1,ubound(dat1,1) )
        if(iostat/=0) then
          deallocate(dat1)
          call iotk_error_issue(ierr,"iotk_scan_dat_aux",__FILE__,__LINE__)
# 553 "iotk_dat.spp"
call iotk_error_msg(ierr,"CVS Revision: 1.25 ")
# 553 "iotk_dat.spp"
call iotk_error_msg(ierr,' ')
# 553 "iotk_dat.spp"
call iotk_error_write(ierr,"iostat",iostat)
          return
        end if
      end if
# 566 "iotk_dat.spp"
      dat = real(dat1,kind=kind(dat))
# 570 "iotk_dat.spp"
      deallocate(dat1)
#endif
# 534 "iotk_dat.spp"
#ifdef __IOTK_REAL3
    case(kind(dat3))
      ! Giusto per scrupolo. Se e' raw non ci sono info sul kind, quindi questa linea e' irraggiungibile
      if(raw) then
        call iotk_error_issue(ierr,"iotk_scan_dat_aux",__FILE__,__LINE__)
# 538 "iotk_dat.spp"
call iotk_error_msg(ierr,"CVS Revision: 1.25 ")
        return
      end if
      allocate(dat3(ubound(dat,1)))
      if(stream) then
        call iotk_stream_read(lunit,idummy,dat3,ierr=ierr)
        if(ierr/=0) then
          deallocate(dat3)
          call iotk_error_issue(ierr,"iotk_scan_dat_aux",__FILE__,__LINE__)
# 546 "iotk_dat.spp"
call iotk_error_msg(ierr,"CVS Revision: 1.25 ")
# 546 "iotk_dat.spp"
call iotk_error_msg(ierr,' ')
# 546 "iotk_dat.spp"
call iotk_error_write(ierr,"iostat",iostat)
          return
        end if
      else
        read(lunit,iostat=iostat) idummy,( dat3(i), i=1,ubound(dat3,1) )
        if(iostat/=0) then
          deallocate(dat3)
          call iotk_error_issue(ierr,"iotk_scan_dat_aux",__FILE__,__LINE__)
# 553 "iotk_dat.spp"
call iotk_error_msg(ierr,"CVS Revision: 1.25 ")
# 553 "iotk_dat.spp"
call iotk_error_msg(ierr,' ')
# 553 "iotk_dat.spp"
call iotk_error_write(ierr,"iostat",iostat)
          return
        end if
      end if
# 566 "iotk_dat.spp"
      dat = real(dat3,kind=kind(dat))
# 570 "iotk_dat.spp"
      deallocate(dat3)
#endif
# 534 "iotk_dat.spp"
#ifdef __IOTK_REAL4
    case(kind(dat4))
      ! Giusto per scrupolo. Se e' raw non ci sono info sul kind, quindi questa linea e' irraggiungibile
      if(raw) then
        call iotk_error_issue(ierr,"iotk_scan_dat_aux",__FILE__,__LINE__)
# 538 "iotk_dat.spp"
call iotk_error_msg(ierr,"CVS Revision: 1.25 ")
        return
      end if
      allocate(dat4(ubound(dat,1)))
      if(stream) then
        call iotk_stream_read(lunit,idummy,dat4,ierr=ierr)
        if(ierr/=0) then
          deallocate(dat4)
          call iotk_error_issue(ierr,"iotk_scan_dat_aux",__FILE__,__LINE__)
# 546 "iotk_dat.spp"
call iotk_error_msg(ierr,"CVS Revision: 1.25 ")
# 546 "iotk_dat.spp"
call iotk_error_msg(ierr,' ')
# 546 "iotk_dat.spp"
call iotk_error_write(ierr,"iostat",iostat)
          return
        end if
      else
        read(lunit,iostat=iostat) idummy,( dat4(i), i=1,ubound(dat4,1) )
        if(iostat/=0) then
          deallocate(dat4)
          call iotk_error_issue(ierr,"iotk_scan_dat_aux",__FILE__,__LINE__)
# 553 "iotk_dat.spp"
call iotk_error_msg(ierr,"CVS Revision: 1.25 ")
# 553 "iotk_dat.spp"
call iotk_error_msg(ierr,' ')
# 553 "iotk_dat.spp"
call iotk_error_write(ierr,"iostat",iostat)
          return
        end if
      end if
# 566 "iotk_dat.spp"
      dat = real(dat4,kind=kind(dat))
# 570 "iotk_dat.spp"
      deallocate(dat4)
#endif
# 574 "iotk_dat.spp"
    case default
      call iotk_error_issue(ierr,"iotk_scan_dat_aux",__FILE__,__LINE__)
# 575 "iotk_dat.spp"
call iotk_error_msg(ierr,"CVS Revision: 1.25 ")
# 575 "iotk_dat.spp"
call iotk_error_msg(ierr,'Kind incompatibility')
# 575 "iotk_dat.spp"
call iotk_error_write(ierr,"kind",rkind)
    end select
  else
    if(raw) then
#ifdef __IOTK_WORKAROUND3
      read(lunit,fmt=*,iostat=iostat) ( dat(j), j=1,ubound(dat,1) )
#else
      read(lunit,fmt=*,iostat=iostat) dat
#endif
      if(iostat/=0) then
        call iotk_error_issue(ierr,"iotk_scan_dat_aux",__FILE__,__LINE__)
# 585 "iotk_dat.spp"
call iotk_error_msg(ierr,"CVS Revision: 1.25 ")
# 585 "iotk_dat.spp"
call iotk_error_msg(ierr,' ')
# 585 "iotk_dat.spp"
call iotk_error_write(ierr,"iostat",iostat)
        return
      end if
    else if(iotk_strcomp(fmt,"*")) then
#ifdef __IOTK_WORKAROUND3
      read(lunit,fmt=*,iostat=iostat) ( dat(j), j=1,ubound(dat,1) )
#else
      read(lunit,fmt=*,iostat=iostat) dat
#endif
      if(iostat/=0) then
        call iotk_error_issue(ierr,"iotk_scan_dat_aux",__FILE__,__LINE__)
# 595 "iotk_dat.spp"
call iotk_error_msg(ierr,"CVS Revision: 1.25 ")
# 595 "iotk_dat.spp"
call iotk_error_msg(ierr,' ')
# 595 "iotk_dat.spp"
call iotk_error_write(ierr,"iostat",iostat)
        return
      end if
    else if(iotk_strcomp(fmt,"!")) then
      index = 0
      do
        call iotk_getline(lunit,line,length,ierr)
        if(ierr/=0) then
          call iotk_error_issue(ierr,"iotk_scan_dat_aux",__FILE__,__LINE__)
# 603 "iotk_dat.spp"
call iotk_error_msg(ierr,"CVS Revision: 1.25 ")
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
# 614 "iotk_dat.spp"
call iotk_error_msg(ierr,"CVS Revision: 1.25 ")
# 614 "iotk_dat.spp"
call iotk_error_msg(ierr,' ')
# 614 "iotk_dat.spp"
call iotk_error_write(ierr,"iostat",iostat)
            return
          end if
          call iotk_getline(lunit,altline,altlength,ierr)
          if(ierr/=0) then
            call iotk_error_issue(ierr,"iotk_scan_dat_aux",__FILE__,__LINE__)
# 619 "iotk_dat.spp"
call iotk_error_msg(ierr,"CVS Revision: 1.25 ")
            return
          end if
          backspace(lunit,iostat=iostat)
          if(iostat/=0) then
            call iotk_error_issue(ierr,"iotk_scan_dat_aux",__FILE__,__LINE__)
# 624 "iotk_dat.spp"
call iotk_error_msg(ierr,"CVS Revision: 1.25 ")
# 624 "iotk_dat.spp"
call iotk_error_msg(ierr,' ')
# 624 "iotk_dat.spp"
call iotk_error_write(ierr,"iostat",iostat)
            return
          end if
          read(lunit,"(a)",advance="no",iostat=iostat) altline(1:nexttag-1 + altlength - length)
          if(iostat/=0) then
            call iotk_error_issue(ierr,"iotk_scan_dat_aux",__FILE__,__LINE__)
# 629 "iotk_dat.spp"
call iotk_error_msg(ierr,"CVS Revision: 1.25 ")
# 629 "iotk_dat.spp"
call iotk_error_msg(ierr,' ')
# 629 "iotk_dat.spp"
call iotk_error_write(ierr,"iostat",iostat)
            return
          end if
        end if
        call iotk_str_clean(line(1:nexttag - 1))
        call iotk_read(dat,line(1:nexttag - 1),index,ierr)
        if(ierr/=0) then
          call iotk_error_issue(ierr,"iotk_scan_dat_aux",__FILE__,__LINE__)
# 636 "iotk_dat.spp"
call iotk_error_msg(ierr,"CVS Revision: 1.25 ")
# 636 "iotk_dat.spp"
call iotk_error_msg(ierr,'Error reading REAL data')
          return
        end if
# 642 "iotk_dat.spp"
        if(index == size(dat)) exit
# 644 "iotk_dat.spp"
        if(nexttag/=length + 1) then
          call iotk_error_issue(ierr,"iotk_scan_dat_aux",__FILE__,__LINE__)
# 645 "iotk_dat.spp"
call iotk_error_msg(ierr,"CVS Revision: 1.25 ")
          return
        end if
      end do
    else
#ifdef __IOTK_WORKAROUND3
      read(lunit,fmt=fmt(1:iotk_strlen(fmt)),iostat=iostat) ( dat(j), j=1,ubound(dat,1) )
#else
      read(lunit,fmt=fmt(1:iotk_strlen(fmt)),iostat=iostat) dat
#endif
      if(iostat/=0) then
        call iotk_error_issue(ierr,"iotk_scan_dat_aux",__FILE__,__LINE__)
# 656 "iotk_dat.spp"
call iotk_error_msg(ierr,"CVS Revision: 1.25 ")
# 656 "iotk_dat.spp"
call iotk_error_msg(ierr,' ')
# 656 "iotk_dat.spp"
call iotk_error_write(ierr,"iostat",iostat)
        return
      end if
    end if
  end if
# 662 "iotk_dat.spp"
  if(idummy/=0) then
    call iotk_error_issue(ierr,"iotk_scan_dat_aux",__FILE__,__LINE__)
# 663 "iotk_dat.spp"
call iotk_error_msg(ierr,"CVS Revision: 1.25 ")
    return
  end if
end subroutine iotk_scan_dat_aux_REAL2
# 668 "iotk_dat.spp"

# 670 "iotk_dat.spp"
subroutine iotk_scan_dat_REAL2_1(unit,name,dat,dummy,attr,found,default,ierr)
  use iotk_base
  use iotk_error_interf
  use iotk_dat_interf, only: iotk_scan_dat_aux
  use iotk_scan_interf
  use iotk_str_interf
  use iotk_misc_interf
  implicit none
  integer, parameter :: this_kind = iotk_REAL2
  integer,                                   intent(in)  :: unit
  character(len=*),                          intent(in)  :: name
#ifdef __IOTK_WORKAROUND6
  REAL(kind=this_kind)                        :: dat (:)
#else
  REAL(kind=this_kind),           intent(out) :: dat (:)
#endif
  type(iotk_dummytype),            optional              :: dummy
#ifdef __IOTK_WORKAROUND6
  character(len=*),                optional              :: attr
#else
  character(len=*),                optional, intent(out) :: attr
#endif
  logical,                         optional, intent(out) :: found
  REAL(kind=this_kind), optional, intent(in)  :: default (:)
  integer,                         optional, intent(out) :: ierr
# 698 "iotk_dat.spp"
  REAL (kind=this_kind),              allocatable :: tmpdat(:)
# 700 "iotk_dat.spp"
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
# 717 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
    goto 1
  end if
  call iotk_parse_dat(lattr,rtype,rkind,rsize,rlen,fmt,columns,ierrl)
! Note that columns is not effectively used
  if(ierrl/=0) goto 1
  if(.not. (iotk_strcomp(rtype,iotk_eos) .or. iotk_strcomp(rtype,"REAL") ) ) then
    call iotk_error_issue(ierrl,"iotk_scan_dat",__FILE__,__LINE__)
# 724 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
# 724 "iotk_dat.spp"
call iotk_error_msg(ierrl,' ')
# 724 "iotk_dat.spp"
call iotk_error_write(ierrl,"rtype",rtype(1:iotk_strlen(rtype)))
# 724 "iotk_dat.spp"
call iotk_error_write(ierrl,"type","REAL")
    goto 1
  end if
  if(.not. (rsize==-1 .or. rsize==size(dat)) ) then
    call iotk_error_issue(ierrl,"iotk_scan_dat",__FILE__,__LINE__)
# 728 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
    goto 1
  end if
  if(rkind==-1) rkind = kind(dat)
# 735 "iotk_dat.spp"

  allocate(tmpdat(size(dat)))
  call iotk_scan_dat_aux(unit,tmpdat,rkind,rlen,fmt(1:iotk_strlen(fmt)),ierrl)
  if(ierrl/=0) then
    call iotk_error_issue(ierrl,"iotk_scan_dat",__FILE__,__LINE__)
# 739 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
# 739 "iotk_dat.spp"
call iotk_error_msg(ierrl,'Error reading data')
# 739 "iotk_dat.spp"
call iotk_error_write(ierrl,"name",name)
# 739 "iotk_dat.spp"
call iotk_error_write(ierrl,"rkind",rkind)
# 739 "iotk_dat.spp"
call iotk_error_write(ierrl,"rlen",rlen)
    goto 1
  end if
# 745 "iotk_dat.spp"
#if defined(__IOTK_WORKAROUND3) || defined(__IOTK_WORKAROUND4)
# 749 "iotk_dat.spp"
     call iotk_private_pack_REAL2(dat,tmpdat,size(tmpdat),1)
# 751 "iotk_dat.spp"
#else
     dat = reshape(tmpdat,shape(dat))
#endif
# 755 "iotk_dat.spp"
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
# 767 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
# 767 "iotk_dat.spp"
call iotk_error_msg(ierrl,'Dat not found')
# 767 "iotk_dat.spp"
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
end subroutine iotk_scan_dat_REAL2_1


#endif
#endif

subroutine iotk_dat_dummy_REAL2_1
  write(0,*)
end subroutine iotk_dat_dummy_REAL2_1


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

#ifdef __IOTK_REAL2
#if 2 <= __IOTK_MAXRANK
# 82 "iotk_dat.spp"
subroutine iotk_write_dat_REAL2_2(unit,name,dat,dummy,attr,columns,sep,fmt,ierr)
  use iotk_base
  use iotk_error_interf
  use iotk_attr_interf, only : iotk_write_attr
  use iotk_write_interf
  use iotk_fmt_interf
  use iotk_str_interf
  use iotk_unit_interf
  use iotk_misc_interf
  implicit none
  integer, parameter :: this_kind = iotk_REAL2
  integer,                                    intent(in)  :: unit
  character(len=*),                           intent(in)  :: name
  REAL (kind=this_kind),           intent(in)  :: dat (:,:) 
  type(iotk_dummytype),             optional              :: dummy
  character(len=*),                 optional, intent(in)  :: attr
  integer,                          optional, intent(in)  :: columns
  character(len=*),                 optional, intent(in)  :: sep
  character(len=*),                 optional, intent(in)  :: fmt
  integer,                          optional, intent(out) :: ierr
  integer :: ierrl,lunit,iostat
  logical :: binary,raw,stream
  integer :: lcolumns
  integer(iotk_header_kind), parameter :: idummy=0
  character(100) :: lsep
  character(300) :: usefmt
  character(iotk_attlenx) :: lattr
  character(iotk_attlenx) :: attr_tmp
  type (iotk_unit), pointer :: this
# 115 "iotk_dat.spp"
  REAL (kind=this_kind),allocatable :: dattmp(:)
# 117 "iotk_dat.spp"
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
# 126 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
      goto 1
    end if
  end if
  lunit = iotk_phys_unit(unit)
  call iotk_unit_get(lunit,pointer=this)
  raw = .false.
  if(associated(this)) then
    raw = this%raw
  end if
  call iotk_inquire(lunit,binary,stream,ierrl)
  if(ierrl/=0) then
    call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 138 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
    goto 1
  end if
  call iotk_strcpy(usefmt,"!",ierrl)
  if(ierrl/=0) then
    call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 143 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
    goto 1
  end if
  if(present(fmt) .and. .not. raw) call iotk_strcpy(usefmt,iotk_strtrim(fmt),ierrl)
  if(ierrl/=0) then
    call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 148 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
    goto 1
  end if
  if(iotk_strscan(usefmt,"<>&")/=0) then
    call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 152 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
# 152 "iotk_dat.spp"
call iotk_error_msg(ierrl,'Special characters (<>&) found in fmt string')
# 152 "iotk_dat.spp"
call iotk_error_write(ierrl,"unit",unit)
# 152 "iotk_dat.spp"
call iotk_error_write(ierrl,"name",trim(name))
# 152 "iotk_dat.spp"
call iotk_error_write(ierrl,"fmt",trim(fmt))
    goto 1
  end if
  call iotk_write_attr(lattr,"type",iotk_tolower("REAL"),first=.true.,ierr=ierrl)
  if(ierrl/=0) then
    call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 157 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
    goto 1
  end if
  call iotk_write_attr(lattr,"size",size(dat),ierr=ierrl)
  if(ierrl/=0) then
    call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 162 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
    goto 1
  end if
# 172 "iotk_dat.spp"
  if(binary) then
    call iotk_write_attr(lattr,"kind",kind(dat),ierr=ierrl)
    if(ierrl/=0) then
      call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 175 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
      goto 1
    end if
  end if
# 180 "iotk_dat.spp"
  if(.not.iotk_strcomp(usefmt,"!")) call iotk_write_attr(lattr,"fmt",iotk_strtrim(usefmt),ierr=ierrl)
  if(ierrl/=0) then
    call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 182 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
    goto 1
  end if
  if(lcolumns/=1) call iotk_write_attr(lattr,"columns",lcolumns,ierr=ierrl)
  if(ierrl/=0) then
    call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 187 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
    goto 1
  end if
  if(present(attr)) then
    attr_tmp(1:1)=iotk_eos
    call iotk_strcpy(attr_tmp,attr,ierr=ierrl)
    if(ierrl/=0) then
      call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 194 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
      goto 1
    end if
    call iotk_delete_attr(attr_tmp,"type",ierrl)
    if(ierrl/=0) then
      call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 199 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
      goto 1
    end if
    call iotk_delete_attr(attr_tmp,"kind",ierrl)
    if(ierrl/=0) then
      call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 204 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
      goto 1
    end if
    call iotk_delete_attr(attr_tmp,"size",ierrl)
    if(ierrl/=0) then
      call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 209 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
      goto 1
    end if
    call iotk_delete_attr(attr_tmp,"fmt",ierrl)
    if(ierrl/=0) then
      call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 214 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
      goto 1
    end if
    call iotk_delete_attr(attr_tmp,"columns",ierrl)
    if(ierrl/=0) then
      call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 219 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
      goto 1
    end if
    call iotk_delete_attr(attr_tmp,"len",ierrl)
    if(ierrl/=0) then
      call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 224 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
      goto 1
    end if
    if(iotk_strlen_trim(attr_tmp)>0) call iotk_strcat(lattr,iotk_strtrim(attr_tmp),ierr=ierrl)
  end if
  if(ierrl/=0) then
    call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 230 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
    goto 1
  end if
  call iotk_write_begin(unit,name,lattr,ierr=ierrl)
  if(ierrl/=0) then
    call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 235 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
    goto 1
  end if

  allocate(dattmp(size(dat)))
# 243 "iotk_dat.spp"
#if defined(__IOTK_WORKAROUND3) || defined(__IOTK_WORKAROUND4)
# 247 "iotk_dat.spp"
     call iotk_private_pack_REAL2(dattmp,dat,size(dattmp),1)
# 249 "iotk_dat.spp"
#else
     dattmp = pack(dat,mask=.true.)
#endif
# 253 "iotk_dat.spp"

  if(binary) then
    if(raw) then
      write(lunit,iostat=iostat) (dattmp(itmp),itmp=1,size(dattmp))
      if(iostat/=0) then
        call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 258 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
        goto 1
      end if
    else
      write(lunit,iostat=iostat) idummy,(dattmp(itmp),itmp=1,size(dattmp))
      if(iostat/=0) then
        call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 264 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
        goto 1
      end if
    end if
  else
    if(raw) then
# 273 "iotk_dat.spp"
      write(lunit,*,iostat=iostat) (dattmp(itmp),itmp=1,size(dattmp))
# 275 "iotk_dat.spp"
      if(iostat/=0) then
        call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 276 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
        goto 1
      end if
    else if(iotk_strcomp(usefmt,"*")) then
      write(lunit,*,iostat=iostat) (dattmp(itmp),itmp=1,size(dattmp))
      if(iostat/=0) then
        call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 282 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
        goto 1
      end if
    else if(iotk_strcomp(usefmt,"!")) then
# 296 "iotk_dat.spp"
     write(lunit,fmt=trim(iotk_wfmt("REAL",kind(dattmp),lcolumns,-1,lsep)),iostat=iostat) (dattmp(itmp),itmp=1,size(dattmp))
     if(iostat/=0) then
      call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 298 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
      goto 1
     end if
# 302 "iotk_dat.spp"
    else
      write(lunit,fmt=usefmt(1:iotk_strlen(usefmt)),iostat=iostat) (dattmp(itmp),itmp=1,size(dattmp))
      if(iostat/=0) then
        call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 305 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
        goto 1
      end if
    end if
  end if
  call iotk_write_end(unit,name,ierr=ierrl)
  if(ierrl/=0) then
    call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 312 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
    goto 1
  end if
1 continue
  if(allocated(dattmp)) deallocate(dattmp)
  if(present(ierr)) then
    ierr = ierrl
  else
    if(ierrl/=0) call iotk_error_handler(ierrl)
  end if
end subroutine iotk_write_dat_REAL2_2


# 668 "iotk_dat.spp"

# 670 "iotk_dat.spp"
subroutine iotk_scan_dat_REAL2_2(unit,name,dat,dummy,attr,found,default,ierr)
  use iotk_base
  use iotk_error_interf
  use iotk_dat_interf, only: iotk_scan_dat_aux
  use iotk_scan_interf
  use iotk_str_interf
  use iotk_misc_interf
  implicit none
  integer, parameter :: this_kind = iotk_REAL2
  integer,                                   intent(in)  :: unit
  character(len=*),                          intent(in)  :: name
#ifdef __IOTK_WORKAROUND6
  REAL(kind=this_kind)                        :: dat (:,:)
#else
  REAL(kind=this_kind),           intent(out) :: dat (:,:)
#endif
  type(iotk_dummytype),            optional              :: dummy
#ifdef __IOTK_WORKAROUND6
  character(len=*),                optional              :: attr
#else
  character(len=*),                optional, intent(out) :: attr
#endif
  logical,                         optional, intent(out) :: found
  REAL(kind=this_kind), optional, intent(in)  :: default (:,:)
  integer,                         optional, intent(out) :: ierr
# 698 "iotk_dat.spp"
  REAL (kind=this_kind),              allocatable :: tmpdat(:)
# 700 "iotk_dat.spp"
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
# 717 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
    goto 1
  end if
  call iotk_parse_dat(lattr,rtype,rkind,rsize,rlen,fmt,columns,ierrl)
! Note that columns is not effectively used
  if(ierrl/=0) goto 1
  if(.not. (iotk_strcomp(rtype,iotk_eos) .or. iotk_strcomp(rtype,"REAL") ) ) then
    call iotk_error_issue(ierrl,"iotk_scan_dat",__FILE__,__LINE__)
# 724 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
# 724 "iotk_dat.spp"
call iotk_error_msg(ierrl,' ')
# 724 "iotk_dat.spp"
call iotk_error_write(ierrl,"rtype",rtype(1:iotk_strlen(rtype)))
# 724 "iotk_dat.spp"
call iotk_error_write(ierrl,"type","REAL")
    goto 1
  end if
  if(.not. (rsize==-1 .or. rsize==size(dat)) ) then
    call iotk_error_issue(ierrl,"iotk_scan_dat",__FILE__,__LINE__)
# 728 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
    goto 1
  end if
  if(rkind==-1) rkind = kind(dat)
# 735 "iotk_dat.spp"

  allocate(tmpdat(size(dat)))
  call iotk_scan_dat_aux(unit,tmpdat,rkind,rlen,fmt(1:iotk_strlen(fmt)),ierrl)
  if(ierrl/=0) then
    call iotk_error_issue(ierrl,"iotk_scan_dat",__FILE__,__LINE__)
# 739 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
# 739 "iotk_dat.spp"
call iotk_error_msg(ierrl,'Error reading data')
# 739 "iotk_dat.spp"
call iotk_error_write(ierrl,"name",name)
# 739 "iotk_dat.spp"
call iotk_error_write(ierrl,"rkind",rkind)
# 739 "iotk_dat.spp"
call iotk_error_write(ierrl,"rlen",rlen)
    goto 1
  end if
# 745 "iotk_dat.spp"
#if defined(__IOTK_WORKAROUND3) || defined(__IOTK_WORKAROUND4)
# 749 "iotk_dat.spp"
     call iotk_private_pack_REAL2(dat,tmpdat,size(tmpdat),1)
# 751 "iotk_dat.spp"
#else
     dat = reshape(tmpdat,shape(dat))
#endif
# 755 "iotk_dat.spp"
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
# 767 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
# 767 "iotk_dat.spp"
call iotk_error_msg(ierrl,'Dat not found')
# 767 "iotk_dat.spp"
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
end subroutine iotk_scan_dat_REAL2_2


#endif
#endif

subroutine iotk_dat_dummy_REAL2_2
  write(0,*)
end subroutine iotk_dat_dummy_REAL2_2


# 45 "iotk_dat.spp"

# 48 "iotk_dat.spp"
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
# 65 "iotk_dat.spp"


!------------------------------------------------------------------------------!
! Inclusion of configuration file
#include "iotk_config.h"
!------------------------------------------------------------------------------!

# 74 "iotk_dat.spp"
#include "iotk_auxmacros.h"
# 76 "iotk_dat.spp"

# 78 "iotk_dat.spp"

#ifdef __IOTK_REAL2
#if 3 <= __IOTK_MAXRANK
# 82 "iotk_dat.spp"
subroutine iotk_write_dat_REAL2_3(unit,name,dat,dummy,attr,columns,sep,fmt,ierr)
  use iotk_base
  use iotk_error_interf
  use iotk_attr_interf, only : iotk_write_attr
  use iotk_write_interf
  use iotk_fmt_interf
  use iotk_str_interf
  use iotk_unit_interf
  use iotk_misc_interf
  implicit none
  integer, parameter :: this_kind = iotk_REAL2
  integer,                                    intent(in)  :: unit
  character(len=*),                           intent(in)  :: name
  REAL (kind=this_kind),           intent(in)  :: dat (:,:,:) 
  type(iotk_dummytype),             optional              :: dummy
  character(len=*),                 optional, intent(in)  :: attr
  integer,                          optional, intent(in)  :: columns
  character(len=*),                 optional, intent(in)  :: sep
  character(len=*),                 optional, intent(in)  :: fmt
  integer,                          optional, intent(out) :: ierr
  integer :: ierrl,lunit,iostat
  logical :: binary,raw,stream
  integer :: lcolumns
  integer(iotk_header_kind), parameter :: idummy=0
  character(100) :: lsep
  character(300) :: usefmt
  character(iotk_attlenx) :: lattr
  character(iotk_attlenx) :: attr_tmp
  type (iotk_unit), pointer :: this
# 115 "iotk_dat.spp"
  REAL (kind=this_kind),allocatable :: dattmp(:)
# 117 "iotk_dat.spp"
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
# 126 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
      goto 1
    end if
  end if
  lunit = iotk_phys_unit(unit)
  call iotk_unit_get(lunit,pointer=this)
  raw = .false.
  if(associated(this)) then
    raw = this%raw
  end if
  call iotk_inquire(lunit,binary,stream,ierrl)
  if(ierrl/=0) then
    call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 138 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
    goto 1
  end if
  call iotk_strcpy(usefmt,"!",ierrl)
  if(ierrl/=0) then
    call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 143 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
    goto 1
  end if
  if(present(fmt) .and. .not. raw) call iotk_strcpy(usefmt,iotk_strtrim(fmt),ierrl)
  if(ierrl/=0) then
    call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 148 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
    goto 1
  end if
  if(iotk_strscan(usefmt,"<>&")/=0) then
    call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 152 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
# 152 "iotk_dat.spp"
call iotk_error_msg(ierrl,'Special characters (<>&) found in fmt string')
# 152 "iotk_dat.spp"
call iotk_error_write(ierrl,"unit",unit)
# 152 "iotk_dat.spp"
call iotk_error_write(ierrl,"name",trim(name))
# 152 "iotk_dat.spp"
call iotk_error_write(ierrl,"fmt",trim(fmt))
    goto 1
  end if
  call iotk_write_attr(lattr,"type",iotk_tolower("REAL"),first=.true.,ierr=ierrl)
  if(ierrl/=0) then
    call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 157 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
    goto 1
  end if
  call iotk_write_attr(lattr,"size",size(dat),ierr=ierrl)
  if(ierrl/=0) then
    call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 162 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
    goto 1
  end if
# 172 "iotk_dat.spp"
  if(binary) then
    call iotk_write_attr(lattr,"kind",kind(dat),ierr=ierrl)
    if(ierrl/=0) then
      call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 175 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
      goto 1
    end if
  end if
# 180 "iotk_dat.spp"
  if(.not.iotk_strcomp(usefmt,"!")) call iotk_write_attr(lattr,"fmt",iotk_strtrim(usefmt),ierr=ierrl)
  if(ierrl/=0) then
    call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 182 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
    goto 1
  end if
  if(lcolumns/=1) call iotk_write_attr(lattr,"columns",lcolumns,ierr=ierrl)
  if(ierrl/=0) then
    call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 187 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
    goto 1
  end if
  if(present(attr)) then
    attr_tmp(1:1)=iotk_eos
    call iotk_strcpy(attr_tmp,attr,ierr=ierrl)
    if(ierrl/=0) then
      call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 194 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
      goto 1
    end if
    call iotk_delete_attr(attr_tmp,"type",ierrl)
    if(ierrl/=0) then
      call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 199 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
      goto 1
    end if
    call iotk_delete_attr(attr_tmp,"kind",ierrl)
    if(ierrl/=0) then
      call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 204 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
      goto 1
    end if
    call iotk_delete_attr(attr_tmp,"size",ierrl)
    if(ierrl/=0) then
      call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 209 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
      goto 1
    end if
    call iotk_delete_attr(attr_tmp,"fmt",ierrl)
    if(ierrl/=0) then
      call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 214 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
      goto 1
    end if
    call iotk_delete_attr(attr_tmp,"columns",ierrl)
    if(ierrl/=0) then
      call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 219 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
      goto 1
    end if
    call iotk_delete_attr(attr_tmp,"len",ierrl)
    if(ierrl/=0) then
      call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 224 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
      goto 1
    end if
    if(iotk_strlen_trim(attr_tmp)>0) call iotk_strcat(lattr,iotk_strtrim(attr_tmp),ierr=ierrl)
  end if
  if(ierrl/=0) then
    call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 230 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
    goto 1
  end if
  call iotk_write_begin(unit,name,lattr,ierr=ierrl)
  if(ierrl/=0) then
    call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 235 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
    goto 1
  end if

  allocate(dattmp(size(dat)))
# 243 "iotk_dat.spp"
#if defined(__IOTK_WORKAROUND3) || defined(__IOTK_WORKAROUND4)
# 247 "iotk_dat.spp"
     call iotk_private_pack_REAL2(dattmp,dat,size(dattmp),1)
# 249 "iotk_dat.spp"
#else
     dattmp = pack(dat,mask=.true.)
#endif
# 253 "iotk_dat.spp"

  if(binary) then
    if(raw) then
      write(lunit,iostat=iostat) (dattmp(itmp),itmp=1,size(dattmp))
      if(iostat/=0) then
        call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 258 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
        goto 1
      end if
    else
      write(lunit,iostat=iostat) idummy,(dattmp(itmp),itmp=1,size(dattmp))
      if(iostat/=0) then
        call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 264 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
        goto 1
      end if
    end if
  else
    if(raw) then
# 273 "iotk_dat.spp"
      write(lunit,*,iostat=iostat) (dattmp(itmp),itmp=1,size(dattmp))
# 275 "iotk_dat.spp"
      if(iostat/=0) then
        call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 276 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
        goto 1
      end if
    else if(iotk_strcomp(usefmt,"*")) then
      write(lunit,*,iostat=iostat) (dattmp(itmp),itmp=1,size(dattmp))
      if(iostat/=0) then
        call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 282 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
        goto 1
      end if
    else if(iotk_strcomp(usefmt,"!")) then
# 296 "iotk_dat.spp"
     write(lunit,fmt=trim(iotk_wfmt("REAL",kind(dattmp),lcolumns,-1,lsep)),iostat=iostat) (dattmp(itmp),itmp=1,size(dattmp))
     if(iostat/=0) then
      call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 298 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
      goto 1
     end if
# 302 "iotk_dat.spp"
    else
      write(lunit,fmt=usefmt(1:iotk_strlen(usefmt)),iostat=iostat) (dattmp(itmp),itmp=1,size(dattmp))
      if(iostat/=0) then
        call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 305 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
        goto 1
      end if
    end if
  end if
  call iotk_write_end(unit,name,ierr=ierrl)
  if(ierrl/=0) then
    call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 312 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
    goto 1
  end if
1 continue
  if(allocated(dattmp)) deallocate(dattmp)
  if(present(ierr)) then
    ierr = ierrl
  else
    if(ierrl/=0) call iotk_error_handler(ierrl)
  end if
end subroutine iotk_write_dat_REAL2_3


# 668 "iotk_dat.spp"

# 670 "iotk_dat.spp"
subroutine iotk_scan_dat_REAL2_3(unit,name,dat,dummy,attr,found,default,ierr)
  use iotk_base
  use iotk_error_interf
  use iotk_dat_interf, only: iotk_scan_dat_aux
  use iotk_scan_interf
  use iotk_str_interf
  use iotk_misc_interf
  implicit none
  integer, parameter :: this_kind = iotk_REAL2
  integer,                                   intent(in)  :: unit
  character(len=*),                          intent(in)  :: name
#ifdef __IOTK_WORKAROUND6
  REAL(kind=this_kind)                        :: dat (:,:,:)
#else
  REAL(kind=this_kind),           intent(out) :: dat (:,:,:)
#endif
  type(iotk_dummytype),            optional              :: dummy
#ifdef __IOTK_WORKAROUND6
  character(len=*),                optional              :: attr
#else
  character(len=*),                optional, intent(out) :: attr
#endif
  logical,                         optional, intent(out) :: found
  REAL(kind=this_kind), optional, intent(in)  :: default (:,:,:)
  integer,                         optional, intent(out) :: ierr
# 698 "iotk_dat.spp"
  REAL (kind=this_kind),              allocatable :: tmpdat(:)
# 700 "iotk_dat.spp"
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
# 717 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
    goto 1
  end if
  call iotk_parse_dat(lattr,rtype,rkind,rsize,rlen,fmt,columns,ierrl)
! Note that columns is not effectively used
  if(ierrl/=0) goto 1
  if(.not. (iotk_strcomp(rtype,iotk_eos) .or. iotk_strcomp(rtype,"REAL") ) ) then
    call iotk_error_issue(ierrl,"iotk_scan_dat",__FILE__,__LINE__)
# 724 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
# 724 "iotk_dat.spp"
call iotk_error_msg(ierrl,' ')
# 724 "iotk_dat.spp"
call iotk_error_write(ierrl,"rtype",rtype(1:iotk_strlen(rtype)))
# 724 "iotk_dat.spp"
call iotk_error_write(ierrl,"type","REAL")
    goto 1
  end if
  if(.not. (rsize==-1 .or. rsize==size(dat)) ) then
    call iotk_error_issue(ierrl,"iotk_scan_dat",__FILE__,__LINE__)
# 728 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
    goto 1
  end if
  if(rkind==-1) rkind = kind(dat)
# 735 "iotk_dat.spp"

  allocate(tmpdat(size(dat)))
  call iotk_scan_dat_aux(unit,tmpdat,rkind,rlen,fmt(1:iotk_strlen(fmt)),ierrl)
  if(ierrl/=0) then
    call iotk_error_issue(ierrl,"iotk_scan_dat",__FILE__,__LINE__)
# 739 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
# 739 "iotk_dat.spp"
call iotk_error_msg(ierrl,'Error reading data')
# 739 "iotk_dat.spp"
call iotk_error_write(ierrl,"name",name)
# 739 "iotk_dat.spp"
call iotk_error_write(ierrl,"rkind",rkind)
# 739 "iotk_dat.spp"
call iotk_error_write(ierrl,"rlen",rlen)
    goto 1
  end if
# 745 "iotk_dat.spp"
#if defined(__IOTK_WORKAROUND3) || defined(__IOTK_WORKAROUND4)
# 749 "iotk_dat.spp"
     call iotk_private_pack_REAL2(dat,tmpdat,size(tmpdat),1)
# 751 "iotk_dat.spp"
#else
     dat = reshape(tmpdat,shape(dat))
#endif
# 755 "iotk_dat.spp"
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
# 767 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
# 767 "iotk_dat.spp"
call iotk_error_msg(ierrl,'Dat not found')
# 767 "iotk_dat.spp"
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
end subroutine iotk_scan_dat_REAL2_3


#endif
#endif

subroutine iotk_dat_dummy_REAL2_3
  write(0,*)
end subroutine iotk_dat_dummy_REAL2_3


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

#ifdef __IOTK_REAL2
#if 4 <= __IOTK_MAXRANK
# 82 "iotk_dat.spp"
subroutine iotk_write_dat_REAL2_4(unit,name,dat,dummy,attr,columns,sep,fmt,ierr)
  use iotk_base
  use iotk_error_interf
  use iotk_attr_interf, only : iotk_write_attr
  use iotk_write_interf
  use iotk_fmt_interf
  use iotk_str_interf
  use iotk_unit_interf
  use iotk_misc_interf
  implicit none
  integer, parameter :: this_kind = iotk_REAL2
  integer,                                    intent(in)  :: unit
  character(len=*),                           intent(in)  :: name
  REAL (kind=this_kind),           intent(in)  :: dat (:,:,:,:) 
  type(iotk_dummytype),             optional              :: dummy
  character(len=*),                 optional, intent(in)  :: attr
  integer,                          optional, intent(in)  :: columns
  character(len=*),                 optional, intent(in)  :: sep
  character(len=*),                 optional, intent(in)  :: fmt
  integer,                          optional, intent(out) :: ierr
  integer :: ierrl,lunit,iostat
  logical :: binary,raw,stream
  integer :: lcolumns
  integer(iotk_header_kind), parameter :: idummy=0
  character(100) :: lsep
  character(300) :: usefmt
  character(iotk_attlenx) :: lattr
  character(iotk_attlenx) :: attr_tmp
  type (iotk_unit), pointer :: this
# 115 "iotk_dat.spp"
  REAL (kind=this_kind),allocatable :: dattmp(:)
# 117 "iotk_dat.spp"
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
# 126 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
      goto 1
    end if
  end if
  lunit = iotk_phys_unit(unit)
  call iotk_unit_get(lunit,pointer=this)
  raw = .false.
  if(associated(this)) then
    raw = this%raw
  end if
  call iotk_inquire(lunit,binary,stream,ierrl)
  if(ierrl/=0) then
    call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 138 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
    goto 1
  end if
  call iotk_strcpy(usefmt,"!",ierrl)
  if(ierrl/=0) then
    call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 143 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
    goto 1
  end if
  if(present(fmt) .and. .not. raw) call iotk_strcpy(usefmt,iotk_strtrim(fmt),ierrl)
  if(ierrl/=0) then
    call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 148 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
    goto 1
  end if
  if(iotk_strscan(usefmt,"<>&")/=0) then
    call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 152 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
# 152 "iotk_dat.spp"
call iotk_error_msg(ierrl,'Special characters (<>&) found in fmt string')
# 152 "iotk_dat.spp"
call iotk_error_write(ierrl,"unit",unit)
# 152 "iotk_dat.spp"
call iotk_error_write(ierrl,"name",trim(name))
# 152 "iotk_dat.spp"
call iotk_error_write(ierrl,"fmt",trim(fmt))
    goto 1
  end if
  call iotk_write_attr(lattr,"type",iotk_tolower("REAL"),first=.true.,ierr=ierrl)
  if(ierrl/=0) then
    call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 157 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
    goto 1
  end if
  call iotk_write_attr(lattr,"size",size(dat),ierr=ierrl)
  if(ierrl/=0) then
    call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 162 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
    goto 1
  end if
# 172 "iotk_dat.spp"
  if(binary) then
    call iotk_write_attr(lattr,"kind",kind(dat),ierr=ierrl)
    if(ierrl/=0) then
      call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 175 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
      goto 1
    end if
  end if
# 180 "iotk_dat.spp"
  if(.not.iotk_strcomp(usefmt,"!")) call iotk_write_attr(lattr,"fmt",iotk_strtrim(usefmt),ierr=ierrl)
  if(ierrl/=0) then
    call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 182 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
    goto 1
  end if
  if(lcolumns/=1) call iotk_write_attr(lattr,"columns",lcolumns,ierr=ierrl)
  if(ierrl/=0) then
    call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 187 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
    goto 1
  end if
  if(present(attr)) then
    attr_tmp(1:1)=iotk_eos
    call iotk_strcpy(attr_tmp,attr,ierr=ierrl)
    if(ierrl/=0) then
      call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 194 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
      goto 1
    end if
    call iotk_delete_attr(attr_tmp,"type",ierrl)
    if(ierrl/=0) then
      call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 199 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
      goto 1
    end if
    call iotk_delete_attr(attr_tmp,"kind",ierrl)
    if(ierrl/=0) then
      call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 204 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
      goto 1
    end if
    call iotk_delete_attr(attr_tmp,"size",ierrl)
    if(ierrl/=0) then
      call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 209 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
      goto 1
    end if
    call iotk_delete_attr(attr_tmp,"fmt",ierrl)
    if(ierrl/=0) then
      call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 214 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
      goto 1
    end if
    call iotk_delete_attr(attr_tmp,"columns",ierrl)
    if(ierrl/=0) then
      call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 219 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
      goto 1
    end if
    call iotk_delete_attr(attr_tmp,"len",ierrl)
    if(ierrl/=0) then
      call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 224 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
      goto 1
    end if
    if(iotk_strlen_trim(attr_tmp)>0) call iotk_strcat(lattr,iotk_strtrim(attr_tmp),ierr=ierrl)
  end if
  if(ierrl/=0) then
    call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 230 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
    goto 1
  end if
  call iotk_write_begin(unit,name,lattr,ierr=ierrl)
  if(ierrl/=0) then
    call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 235 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
    goto 1
  end if

  allocate(dattmp(size(dat)))
# 243 "iotk_dat.spp"
#if defined(__IOTK_WORKAROUND3) || defined(__IOTK_WORKAROUND4)
# 247 "iotk_dat.spp"
     call iotk_private_pack_REAL2(dattmp,dat,size(dattmp),1)
# 249 "iotk_dat.spp"
#else
     dattmp = pack(dat,mask=.true.)
#endif
# 253 "iotk_dat.spp"

  if(binary) then
    if(raw) then
      write(lunit,iostat=iostat) (dattmp(itmp),itmp=1,size(dattmp))
      if(iostat/=0) then
        call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 258 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
        goto 1
      end if
    else
      write(lunit,iostat=iostat) idummy,(dattmp(itmp),itmp=1,size(dattmp))
      if(iostat/=0) then
        call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 264 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
        goto 1
      end if
    end if
  else
    if(raw) then
# 273 "iotk_dat.spp"
      write(lunit,*,iostat=iostat) (dattmp(itmp),itmp=1,size(dattmp))
# 275 "iotk_dat.spp"
      if(iostat/=0) then
        call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 276 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
        goto 1
      end if
    else if(iotk_strcomp(usefmt,"*")) then
      write(lunit,*,iostat=iostat) (dattmp(itmp),itmp=1,size(dattmp))
      if(iostat/=0) then
        call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 282 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
        goto 1
      end if
    else if(iotk_strcomp(usefmt,"!")) then
# 296 "iotk_dat.spp"
     write(lunit,fmt=trim(iotk_wfmt("REAL",kind(dattmp),lcolumns,-1,lsep)),iostat=iostat) (dattmp(itmp),itmp=1,size(dattmp))
     if(iostat/=0) then
      call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 298 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
      goto 1
     end if
# 302 "iotk_dat.spp"
    else
      write(lunit,fmt=usefmt(1:iotk_strlen(usefmt)),iostat=iostat) (dattmp(itmp),itmp=1,size(dattmp))
      if(iostat/=0) then
        call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 305 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
        goto 1
      end if
    end if
  end if
  call iotk_write_end(unit,name,ierr=ierrl)
  if(ierrl/=0) then
    call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 312 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
    goto 1
  end if
1 continue
  if(allocated(dattmp)) deallocate(dattmp)
  if(present(ierr)) then
    ierr = ierrl
  else
    if(ierrl/=0) call iotk_error_handler(ierrl)
  end if
end subroutine iotk_write_dat_REAL2_4


# 668 "iotk_dat.spp"

# 670 "iotk_dat.spp"
subroutine iotk_scan_dat_REAL2_4(unit,name,dat,dummy,attr,found,default,ierr)
  use iotk_base
  use iotk_error_interf
  use iotk_dat_interf, only: iotk_scan_dat_aux
  use iotk_scan_interf
  use iotk_str_interf
  use iotk_misc_interf
  implicit none
  integer, parameter :: this_kind = iotk_REAL2
  integer,                                   intent(in)  :: unit
  character(len=*),                          intent(in)  :: name
#ifdef __IOTK_WORKAROUND6
  REAL(kind=this_kind)                        :: dat (:,:,:,:)
#else
  REAL(kind=this_kind),           intent(out) :: dat (:,:,:,:)
#endif
  type(iotk_dummytype),            optional              :: dummy
#ifdef __IOTK_WORKAROUND6
  character(len=*),                optional              :: attr
#else
  character(len=*),                optional, intent(out) :: attr
#endif
  logical,                         optional, intent(out) :: found
  REAL(kind=this_kind), optional, intent(in)  :: default (:,:,:,:)
  integer,                         optional, intent(out) :: ierr
# 698 "iotk_dat.spp"
  REAL (kind=this_kind),              allocatable :: tmpdat(:)
# 700 "iotk_dat.spp"
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
# 717 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
    goto 1
  end if
  call iotk_parse_dat(lattr,rtype,rkind,rsize,rlen,fmt,columns,ierrl)
! Note that columns is not effectively used
  if(ierrl/=0) goto 1
  if(.not. (iotk_strcomp(rtype,iotk_eos) .or. iotk_strcomp(rtype,"REAL") ) ) then
    call iotk_error_issue(ierrl,"iotk_scan_dat",__FILE__,__LINE__)
# 724 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
# 724 "iotk_dat.spp"
call iotk_error_msg(ierrl,' ')
# 724 "iotk_dat.spp"
call iotk_error_write(ierrl,"rtype",rtype(1:iotk_strlen(rtype)))
# 724 "iotk_dat.spp"
call iotk_error_write(ierrl,"type","REAL")
    goto 1
  end if
  if(.not. (rsize==-1 .or. rsize==size(dat)) ) then
    call iotk_error_issue(ierrl,"iotk_scan_dat",__FILE__,__LINE__)
# 728 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
    goto 1
  end if
  if(rkind==-1) rkind = kind(dat)
# 735 "iotk_dat.spp"

  allocate(tmpdat(size(dat)))
  call iotk_scan_dat_aux(unit,tmpdat,rkind,rlen,fmt(1:iotk_strlen(fmt)),ierrl)
  if(ierrl/=0) then
    call iotk_error_issue(ierrl,"iotk_scan_dat",__FILE__,__LINE__)
# 739 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
# 739 "iotk_dat.spp"
call iotk_error_msg(ierrl,'Error reading data')
# 739 "iotk_dat.spp"
call iotk_error_write(ierrl,"name",name)
# 739 "iotk_dat.spp"
call iotk_error_write(ierrl,"rkind",rkind)
# 739 "iotk_dat.spp"
call iotk_error_write(ierrl,"rlen",rlen)
    goto 1
  end if
# 745 "iotk_dat.spp"
#if defined(__IOTK_WORKAROUND3) || defined(__IOTK_WORKAROUND4)
# 749 "iotk_dat.spp"
     call iotk_private_pack_REAL2(dat,tmpdat,size(tmpdat),1)
# 751 "iotk_dat.spp"
#else
     dat = reshape(tmpdat,shape(dat))
#endif
# 755 "iotk_dat.spp"
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
# 767 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
# 767 "iotk_dat.spp"
call iotk_error_msg(ierrl,'Dat not found')
# 767 "iotk_dat.spp"
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
end subroutine iotk_scan_dat_REAL2_4


#endif
#endif

subroutine iotk_dat_dummy_REAL2_4
  write(0,*)
end subroutine iotk_dat_dummy_REAL2_4


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

#ifdef __IOTK_REAL2
#if 5 <= __IOTK_MAXRANK
# 82 "iotk_dat.spp"
subroutine iotk_write_dat_REAL2_5(unit,name,dat,dummy,attr,columns,sep,fmt,ierr)
  use iotk_base
  use iotk_error_interf
  use iotk_attr_interf, only : iotk_write_attr
  use iotk_write_interf
  use iotk_fmt_interf
  use iotk_str_interf
  use iotk_unit_interf
  use iotk_misc_interf
  implicit none
  integer, parameter :: this_kind = iotk_REAL2
  integer,                                    intent(in)  :: unit
  character(len=*),                           intent(in)  :: name
  REAL (kind=this_kind),           intent(in)  :: dat (:,:,:,:,:) 
  type(iotk_dummytype),             optional              :: dummy
  character(len=*),                 optional, intent(in)  :: attr
  integer,                          optional, intent(in)  :: columns
  character(len=*),                 optional, intent(in)  :: sep
  character(len=*),                 optional, intent(in)  :: fmt
  integer,                          optional, intent(out) :: ierr
  integer :: ierrl,lunit,iostat
  logical :: binary,raw,stream
  integer :: lcolumns
  integer(iotk_header_kind), parameter :: idummy=0
  character(100) :: lsep
  character(300) :: usefmt
  character(iotk_attlenx) :: lattr
  character(iotk_attlenx) :: attr_tmp
  type (iotk_unit), pointer :: this
# 115 "iotk_dat.spp"
  REAL (kind=this_kind),allocatable :: dattmp(:)
# 117 "iotk_dat.spp"
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
# 126 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
      goto 1
    end if
  end if
  lunit = iotk_phys_unit(unit)
  call iotk_unit_get(lunit,pointer=this)
  raw = .false.
  if(associated(this)) then
    raw = this%raw
  end if
  call iotk_inquire(lunit,binary,stream,ierrl)
  if(ierrl/=0) then
    call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 138 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
    goto 1
  end if
  call iotk_strcpy(usefmt,"!",ierrl)
  if(ierrl/=0) then
    call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 143 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
    goto 1
  end if
  if(present(fmt) .and. .not. raw) call iotk_strcpy(usefmt,iotk_strtrim(fmt),ierrl)
  if(ierrl/=0) then
    call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 148 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
    goto 1
  end if
  if(iotk_strscan(usefmt,"<>&")/=0) then
    call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 152 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
# 152 "iotk_dat.spp"
call iotk_error_msg(ierrl,'Special characters (<>&) found in fmt string')
# 152 "iotk_dat.spp"
call iotk_error_write(ierrl,"unit",unit)
# 152 "iotk_dat.spp"
call iotk_error_write(ierrl,"name",trim(name))
# 152 "iotk_dat.spp"
call iotk_error_write(ierrl,"fmt",trim(fmt))
    goto 1
  end if
  call iotk_write_attr(lattr,"type",iotk_tolower("REAL"),first=.true.,ierr=ierrl)
  if(ierrl/=0) then
    call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 157 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
    goto 1
  end if
  call iotk_write_attr(lattr,"size",size(dat),ierr=ierrl)
  if(ierrl/=0) then
    call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 162 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
    goto 1
  end if
# 172 "iotk_dat.spp"
  if(binary) then
    call iotk_write_attr(lattr,"kind",kind(dat),ierr=ierrl)
    if(ierrl/=0) then
      call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 175 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
      goto 1
    end if
  end if
# 180 "iotk_dat.spp"
  if(.not.iotk_strcomp(usefmt,"!")) call iotk_write_attr(lattr,"fmt",iotk_strtrim(usefmt),ierr=ierrl)
  if(ierrl/=0) then
    call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 182 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
    goto 1
  end if
  if(lcolumns/=1) call iotk_write_attr(lattr,"columns",lcolumns,ierr=ierrl)
  if(ierrl/=0) then
    call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 187 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
    goto 1
  end if
  if(present(attr)) then
    attr_tmp(1:1)=iotk_eos
    call iotk_strcpy(attr_tmp,attr,ierr=ierrl)
    if(ierrl/=0) then
      call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 194 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
      goto 1
    end if
    call iotk_delete_attr(attr_tmp,"type",ierrl)
    if(ierrl/=0) then
      call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 199 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
      goto 1
    end if
    call iotk_delete_attr(attr_tmp,"kind",ierrl)
    if(ierrl/=0) then
      call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 204 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
      goto 1
    end if
    call iotk_delete_attr(attr_tmp,"size",ierrl)
    if(ierrl/=0) then
      call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 209 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
      goto 1
    end if
    call iotk_delete_attr(attr_tmp,"fmt",ierrl)
    if(ierrl/=0) then
      call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 214 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
      goto 1
    end if
    call iotk_delete_attr(attr_tmp,"columns",ierrl)
    if(ierrl/=0) then
      call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 219 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
      goto 1
    end if
    call iotk_delete_attr(attr_tmp,"len",ierrl)
    if(ierrl/=0) then
      call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 224 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
      goto 1
    end if
    if(iotk_strlen_trim(attr_tmp)>0) call iotk_strcat(lattr,iotk_strtrim(attr_tmp),ierr=ierrl)
  end if
  if(ierrl/=0) then
    call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 230 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
    goto 1
  end if
  call iotk_write_begin(unit,name,lattr,ierr=ierrl)
  if(ierrl/=0) then
    call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 235 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
    goto 1
  end if

  allocate(dattmp(size(dat)))
# 243 "iotk_dat.spp"
#if defined(__IOTK_WORKAROUND3) || defined(__IOTK_WORKAROUND4)
# 247 "iotk_dat.spp"
     call iotk_private_pack_REAL2(dattmp,dat,size(dattmp),1)
# 249 "iotk_dat.spp"
#else
     dattmp = pack(dat,mask=.true.)
#endif
# 253 "iotk_dat.spp"

  if(binary) then
    if(raw) then
      write(lunit,iostat=iostat) (dattmp(itmp),itmp=1,size(dattmp))
      if(iostat/=0) then
        call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 258 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
        goto 1
      end if
    else
      write(lunit,iostat=iostat) idummy,(dattmp(itmp),itmp=1,size(dattmp))
      if(iostat/=0) then
        call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 264 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
        goto 1
      end if
    end if
  else
    if(raw) then
# 273 "iotk_dat.spp"
      write(lunit,*,iostat=iostat) (dattmp(itmp),itmp=1,size(dattmp))
# 275 "iotk_dat.spp"
      if(iostat/=0) then
        call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 276 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
        goto 1
      end if
    else if(iotk_strcomp(usefmt,"*")) then
      write(lunit,*,iostat=iostat) (dattmp(itmp),itmp=1,size(dattmp))
      if(iostat/=0) then
        call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 282 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
        goto 1
      end if
    else if(iotk_strcomp(usefmt,"!")) then
# 296 "iotk_dat.spp"
     write(lunit,fmt=trim(iotk_wfmt("REAL",kind(dattmp),lcolumns,-1,lsep)),iostat=iostat) (dattmp(itmp),itmp=1,size(dattmp))
     if(iostat/=0) then
      call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 298 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
      goto 1
     end if
# 302 "iotk_dat.spp"
    else
      write(lunit,fmt=usefmt(1:iotk_strlen(usefmt)),iostat=iostat) (dattmp(itmp),itmp=1,size(dattmp))
      if(iostat/=0) then
        call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 305 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
        goto 1
      end if
    end if
  end if
  call iotk_write_end(unit,name,ierr=ierrl)
  if(ierrl/=0) then
    call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 312 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
    goto 1
  end if
1 continue
  if(allocated(dattmp)) deallocate(dattmp)
  if(present(ierr)) then
    ierr = ierrl
  else
    if(ierrl/=0) call iotk_error_handler(ierrl)
  end if
end subroutine iotk_write_dat_REAL2_5


# 668 "iotk_dat.spp"

# 670 "iotk_dat.spp"
subroutine iotk_scan_dat_REAL2_5(unit,name,dat,dummy,attr,found,default,ierr)
  use iotk_base
  use iotk_error_interf
  use iotk_dat_interf, only: iotk_scan_dat_aux
  use iotk_scan_interf
  use iotk_str_interf
  use iotk_misc_interf
  implicit none
  integer, parameter :: this_kind = iotk_REAL2
  integer,                                   intent(in)  :: unit
  character(len=*),                          intent(in)  :: name
#ifdef __IOTK_WORKAROUND6
  REAL(kind=this_kind)                        :: dat (:,:,:,:,:)
#else
  REAL(kind=this_kind),           intent(out) :: dat (:,:,:,:,:)
#endif
  type(iotk_dummytype),            optional              :: dummy
#ifdef __IOTK_WORKAROUND6
  character(len=*),                optional              :: attr
#else
  character(len=*),                optional, intent(out) :: attr
#endif
  logical,                         optional, intent(out) :: found
  REAL(kind=this_kind), optional, intent(in)  :: default (:,:,:,:,:)
  integer,                         optional, intent(out) :: ierr
# 698 "iotk_dat.spp"
  REAL (kind=this_kind),              allocatable :: tmpdat(:)
# 700 "iotk_dat.spp"
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
# 717 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
    goto 1
  end if
  call iotk_parse_dat(lattr,rtype,rkind,rsize,rlen,fmt,columns,ierrl)
! Note that columns is not effectively used
  if(ierrl/=0) goto 1
  if(.not. (iotk_strcomp(rtype,iotk_eos) .or. iotk_strcomp(rtype,"REAL") ) ) then
    call iotk_error_issue(ierrl,"iotk_scan_dat",__FILE__,__LINE__)
# 724 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
# 724 "iotk_dat.spp"
call iotk_error_msg(ierrl,' ')
# 724 "iotk_dat.spp"
call iotk_error_write(ierrl,"rtype",rtype(1:iotk_strlen(rtype)))
# 724 "iotk_dat.spp"
call iotk_error_write(ierrl,"type","REAL")
    goto 1
  end if
  if(.not. (rsize==-1 .or. rsize==size(dat)) ) then
    call iotk_error_issue(ierrl,"iotk_scan_dat",__FILE__,__LINE__)
# 728 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
    goto 1
  end if
  if(rkind==-1) rkind = kind(dat)
# 735 "iotk_dat.spp"

  allocate(tmpdat(size(dat)))
  call iotk_scan_dat_aux(unit,tmpdat,rkind,rlen,fmt(1:iotk_strlen(fmt)),ierrl)
  if(ierrl/=0) then
    call iotk_error_issue(ierrl,"iotk_scan_dat",__FILE__,__LINE__)
# 739 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
# 739 "iotk_dat.spp"
call iotk_error_msg(ierrl,'Error reading data')
# 739 "iotk_dat.spp"
call iotk_error_write(ierrl,"name",name)
# 739 "iotk_dat.spp"
call iotk_error_write(ierrl,"rkind",rkind)
# 739 "iotk_dat.spp"
call iotk_error_write(ierrl,"rlen",rlen)
    goto 1
  end if
# 745 "iotk_dat.spp"
#if defined(__IOTK_WORKAROUND3) || defined(__IOTK_WORKAROUND4)
# 749 "iotk_dat.spp"
     call iotk_private_pack_REAL2(dat,tmpdat,size(tmpdat),1)
# 751 "iotk_dat.spp"
#else
     dat = reshape(tmpdat,shape(dat))
#endif
# 755 "iotk_dat.spp"
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
# 767 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
# 767 "iotk_dat.spp"
call iotk_error_msg(ierrl,'Dat not found')
# 767 "iotk_dat.spp"
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
end subroutine iotk_scan_dat_REAL2_5


#endif
#endif

subroutine iotk_dat_dummy_REAL2_5
  write(0,*)
end subroutine iotk_dat_dummy_REAL2_5


# 45 "iotk_dat.spp"

# 48 "iotk_dat.spp"
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
# 65 "iotk_dat.spp"


!------------------------------------------------------------------------------!
! Inclusion of configuration file
#include "iotk_config.h"
!------------------------------------------------------------------------------!

# 74 "iotk_dat.spp"
#include "iotk_auxmacros.h"
# 76 "iotk_dat.spp"

# 78 "iotk_dat.spp"

#ifdef __IOTK_REAL2
#if 6 <= __IOTK_MAXRANK
# 82 "iotk_dat.spp"
subroutine iotk_write_dat_REAL2_6(unit,name,dat,dummy,attr,columns,sep,fmt,ierr)
  use iotk_base
  use iotk_error_interf
  use iotk_attr_interf, only : iotk_write_attr
  use iotk_write_interf
  use iotk_fmt_interf
  use iotk_str_interf
  use iotk_unit_interf
  use iotk_misc_interf
  implicit none
  integer, parameter :: this_kind = iotk_REAL2
  integer,                                    intent(in)  :: unit
  character(len=*),                           intent(in)  :: name
  REAL (kind=this_kind),           intent(in)  :: dat (:,:,:,:,:,:) 
  type(iotk_dummytype),             optional              :: dummy
  character(len=*),                 optional, intent(in)  :: attr
  integer,                          optional, intent(in)  :: columns
  character(len=*),                 optional, intent(in)  :: sep
  character(len=*),                 optional, intent(in)  :: fmt
  integer,                          optional, intent(out) :: ierr
  integer :: ierrl,lunit,iostat
  logical :: binary,raw,stream
  integer :: lcolumns
  integer(iotk_header_kind), parameter :: idummy=0
  character(100) :: lsep
  character(300) :: usefmt
  character(iotk_attlenx) :: lattr
  character(iotk_attlenx) :: attr_tmp
  type (iotk_unit), pointer :: this
# 115 "iotk_dat.spp"
  REAL (kind=this_kind),allocatable :: dattmp(:)
# 117 "iotk_dat.spp"
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
# 126 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
      goto 1
    end if
  end if
  lunit = iotk_phys_unit(unit)
  call iotk_unit_get(lunit,pointer=this)
  raw = .false.
  if(associated(this)) then
    raw = this%raw
  end if
  call iotk_inquire(lunit,binary,stream,ierrl)
  if(ierrl/=0) then
    call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 138 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
    goto 1
  end if
  call iotk_strcpy(usefmt,"!",ierrl)
  if(ierrl/=0) then
    call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 143 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
    goto 1
  end if
  if(present(fmt) .and. .not. raw) call iotk_strcpy(usefmt,iotk_strtrim(fmt),ierrl)
  if(ierrl/=0) then
    call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 148 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
    goto 1
  end if
  if(iotk_strscan(usefmt,"<>&")/=0) then
    call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 152 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
# 152 "iotk_dat.spp"
call iotk_error_msg(ierrl,'Special characters (<>&) found in fmt string')
# 152 "iotk_dat.spp"
call iotk_error_write(ierrl,"unit",unit)
# 152 "iotk_dat.spp"
call iotk_error_write(ierrl,"name",trim(name))
# 152 "iotk_dat.spp"
call iotk_error_write(ierrl,"fmt",trim(fmt))
    goto 1
  end if
  call iotk_write_attr(lattr,"type",iotk_tolower("REAL"),first=.true.,ierr=ierrl)
  if(ierrl/=0) then
    call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 157 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
    goto 1
  end if
  call iotk_write_attr(lattr,"size",size(dat),ierr=ierrl)
  if(ierrl/=0) then
    call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 162 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
    goto 1
  end if
# 172 "iotk_dat.spp"
  if(binary) then
    call iotk_write_attr(lattr,"kind",kind(dat),ierr=ierrl)
    if(ierrl/=0) then
      call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 175 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
      goto 1
    end if
  end if
# 180 "iotk_dat.spp"
  if(.not.iotk_strcomp(usefmt,"!")) call iotk_write_attr(lattr,"fmt",iotk_strtrim(usefmt),ierr=ierrl)
  if(ierrl/=0) then
    call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 182 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
    goto 1
  end if
  if(lcolumns/=1) call iotk_write_attr(lattr,"columns",lcolumns,ierr=ierrl)
  if(ierrl/=0) then
    call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 187 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
    goto 1
  end if
  if(present(attr)) then
    attr_tmp(1:1)=iotk_eos
    call iotk_strcpy(attr_tmp,attr,ierr=ierrl)
    if(ierrl/=0) then
      call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 194 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
      goto 1
    end if
    call iotk_delete_attr(attr_tmp,"type",ierrl)
    if(ierrl/=0) then
      call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 199 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
      goto 1
    end if
    call iotk_delete_attr(attr_tmp,"kind",ierrl)
    if(ierrl/=0) then
      call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 204 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
      goto 1
    end if
    call iotk_delete_attr(attr_tmp,"size",ierrl)
    if(ierrl/=0) then
      call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 209 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
      goto 1
    end if
    call iotk_delete_attr(attr_tmp,"fmt",ierrl)
    if(ierrl/=0) then
      call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 214 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
      goto 1
    end if
    call iotk_delete_attr(attr_tmp,"columns",ierrl)
    if(ierrl/=0) then
      call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 219 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
      goto 1
    end if
    call iotk_delete_attr(attr_tmp,"len",ierrl)
    if(ierrl/=0) then
      call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 224 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
      goto 1
    end if
    if(iotk_strlen_trim(attr_tmp)>0) call iotk_strcat(lattr,iotk_strtrim(attr_tmp),ierr=ierrl)
  end if
  if(ierrl/=0) then
    call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 230 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
    goto 1
  end if
  call iotk_write_begin(unit,name,lattr,ierr=ierrl)
  if(ierrl/=0) then
    call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 235 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
    goto 1
  end if

  allocate(dattmp(size(dat)))
# 243 "iotk_dat.spp"
#if defined(__IOTK_WORKAROUND3) || defined(__IOTK_WORKAROUND4)
# 247 "iotk_dat.spp"
     call iotk_private_pack_REAL2(dattmp,dat,size(dattmp),1)
# 249 "iotk_dat.spp"
#else
     dattmp = pack(dat,mask=.true.)
#endif
# 253 "iotk_dat.spp"

  if(binary) then
    if(raw) then
      write(lunit,iostat=iostat) (dattmp(itmp),itmp=1,size(dattmp))
      if(iostat/=0) then
        call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 258 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
        goto 1
      end if
    else
      write(lunit,iostat=iostat) idummy,(dattmp(itmp),itmp=1,size(dattmp))
      if(iostat/=0) then
        call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 264 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
        goto 1
      end if
    end if
  else
    if(raw) then
# 273 "iotk_dat.spp"
      write(lunit,*,iostat=iostat) (dattmp(itmp),itmp=1,size(dattmp))
# 275 "iotk_dat.spp"
      if(iostat/=0) then
        call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 276 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
        goto 1
      end if
    else if(iotk_strcomp(usefmt,"*")) then
      write(lunit,*,iostat=iostat) (dattmp(itmp),itmp=1,size(dattmp))
      if(iostat/=0) then
        call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 282 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
        goto 1
      end if
    else if(iotk_strcomp(usefmt,"!")) then
# 296 "iotk_dat.spp"
     write(lunit,fmt=trim(iotk_wfmt("REAL",kind(dattmp),lcolumns,-1,lsep)),iostat=iostat) (dattmp(itmp),itmp=1,size(dattmp))
     if(iostat/=0) then
      call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 298 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
      goto 1
     end if
# 302 "iotk_dat.spp"
    else
      write(lunit,fmt=usefmt(1:iotk_strlen(usefmt)),iostat=iostat) (dattmp(itmp),itmp=1,size(dattmp))
      if(iostat/=0) then
        call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 305 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
        goto 1
      end if
    end if
  end if
  call iotk_write_end(unit,name,ierr=ierrl)
  if(ierrl/=0) then
    call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 312 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
    goto 1
  end if
1 continue
  if(allocated(dattmp)) deallocate(dattmp)
  if(present(ierr)) then
    ierr = ierrl
  else
    if(ierrl/=0) call iotk_error_handler(ierrl)
  end if
end subroutine iotk_write_dat_REAL2_6


# 668 "iotk_dat.spp"

# 670 "iotk_dat.spp"
subroutine iotk_scan_dat_REAL2_6(unit,name,dat,dummy,attr,found,default,ierr)
  use iotk_base
  use iotk_error_interf
  use iotk_dat_interf, only: iotk_scan_dat_aux
  use iotk_scan_interf
  use iotk_str_interf
  use iotk_misc_interf
  implicit none
  integer, parameter :: this_kind = iotk_REAL2
  integer,                                   intent(in)  :: unit
  character(len=*),                          intent(in)  :: name
#ifdef __IOTK_WORKAROUND6
  REAL(kind=this_kind)                        :: dat (:,:,:,:,:,:)
#else
  REAL(kind=this_kind),           intent(out) :: dat (:,:,:,:,:,:)
#endif
  type(iotk_dummytype),            optional              :: dummy
#ifdef __IOTK_WORKAROUND6
  character(len=*),                optional              :: attr
#else
  character(len=*),                optional, intent(out) :: attr
#endif
  logical,                         optional, intent(out) :: found
  REAL(kind=this_kind), optional, intent(in)  :: default (:,:,:,:,:,:)
  integer,                         optional, intent(out) :: ierr
# 698 "iotk_dat.spp"
  REAL (kind=this_kind),              allocatable :: tmpdat(:)
# 700 "iotk_dat.spp"
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
# 717 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
    goto 1
  end if
  call iotk_parse_dat(lattr,rtype,rkind,rsize,rlen,fmt,columns,ierrl)
! Note that columns is not effectively used
  if(ierrl/=0) goto 1
  if(.not. (iotk_strcomp(rtype,iotk_eos) .or. iotk_strcomp(rtype,"REAL") ) ) then
    call iotk_error_issue(ierrl,"iotk_scan_dat",__FILE__,__LINE__)
# 724 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
# 724 "iotk_dat.spp"
call iotk_error_msg(ierrl,' ')
# 724 "iotk_dat.spp"
call iotk_error_write(ierrl,"rtype",rtype(1:iotk_strlen(rtype)))
# 724 "iotk_dat.spp"
call iotk_error_write(ierrl,"type","REAL")
    goto 1
  end if
  if(.not. (rsize==-1 .or. rsize==size(dat)) ) then
    call iotk_error_issue(ierrl,"iotk_scan_dat",__FILE__,__LINE__)
# 728 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
    goto 1
  end if
  if(rkind==-1) rkind = kind(dat)
# 735 "iotk_dat.spp"

  allocate(tmpdat(size(dat)))
  call iotk_scan_dat_aux(unit,tmpdat,rkind,rlen,fmt(1:iotk_strlen(fmt)),ierrl)
  if(ierrl/=0) then
    call iotk_error_issue(ierrl,"iotk_scan_dat",__FILE__,__LINE__)
# 739 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
# 739 "iotk_dat.spp"
call iotk_error_msg(ierrl,'Error reading data')
# 739 "iotk_dat.spp"
call iotk_error_write(ierrl,"name",name)
# 739 "iotk_dat.spp"
call iotk_error_write(ierrl,"rkind",rkind)
# 739 "iotk_dat.spp"
call iotk_error_write(ierrl,"rlen",rlen)
    goto 1
  end if
# 745 "iotk_dat.spp"
#if defined(__IOTK_WORKAROUND3) || defined(__IOTK_WORKAROUND4)
# 749 "iotk_dat.spp"
     call iotk_private_pack_REAL2(dat,tmpdat,size(tmpdat),1)
# 751 "iotk_dat.spp"
#else
     dat = reshape(tmpdat,shape(dat))
#endif
# 755 "iotk_dat.spp"
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
# 767 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
# 767 "iotk_dat.spp"
call iotk_error_msg(ierrl,'Dat not found')
# 767 "iotk_dat.spp"
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
end subroutine iotk_scan_dat_REAL2_6


#endif
#endif

subroutine iotk_dat_dummy_REAL2_6
  write(0,*)
end subroutine iotk_dat_dummy_REAL2_6


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

#ifdef __IOTK_REAL2
#if 7 <= __IOTK_MAXRANK
# 82 "iotk_dat.spp"
subroutine iotk_write_dat_REAL2_7(unit,name,dat,dummy,attr,columns,sep,fmt,ierr)
  use iotk_base
  use iotk_error_interf
  use iotk_attr_interf, only : iotk_write_attr
  use iotk_write_interf
  use iotk_fmt_interf
  use iotk_str_interf
  use iotk_unit_interf
  use iotk_misc_interf
  implicit none
  integer, parameter :: this_kind = iotk_REAL2
  integer,                                    intent(in)  :: unit
  character(len=*),                           intent(in)  :: name
  REAL (kind=this_kind),           intent(in)  :: dat (:,:,:,:,:,:,:) 
  type(iotk_dummytype),             optional              :: dummy
  character(len=*),                 optional, intent(in)  :: attr
  integer,                          optional, intent(in)  :: columns
  character(len=*),                 optional, intent(in)  :: sep
  character(len=*),                 optional, intent(in)  :: fmt
  integer,                          optional, intent(out) :: ierr
  integer :: ierrl,lunit,iostat
  logical :: binary,raw,stream
  integer :: lcolumns
  integer(iotk_header_kind), parameter :: idummy=0
  character(100) :: lsep
  character(300) :: usefmt
  character(iotk_attlenx) :: lattr
  character(iotk_attlenx) :: attr_tmp
  type (iotk_unit), pointer :: this
# 115 "iotk_dat.spp"
  REAL (kind=this_kind),allocatable :: dattmp(:)
# 117 "iotk_dat.spp"
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
# 126 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
      goto 1
    end if
  end if
  lunit = iotk_phys_unit(unit)
  call iotk_unit_get(lunit,pointer=this)
  raw = .false.
  if(associated(this)) then
    raw = this%raw
  end if
  call iotk_inquire(lunit,binary,stream,ierrl)
  if(ierrl/=0) then
    call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 138 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
    goto 1
  end if
  call iotk_strcpy(usefmt,"!",ierrl)
  if(ierrl/=0) then
    call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 143 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
    goto 1
  end if
  if(present(fmt) .and. .not. raw) call iotk_strcpy(usefmt,iotk_strtrim(fmt),ierrl)
  if(ierrl/=0) then
    call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 148 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
    goto 1
  end if
  if(iotk_strscan(usefmt,"<>&")/=0) then
    call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 152 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
# 152 "iotk_dat.spp"
call iotk_error_msg(ierrl,'Special characters (<>&) found in fmt string')
# 152 "iotk_dat.spp"
call iotk_error_write(ierrl,"unit",unit)
# 152 "iotk_dat.spp"
call iotk_error_write(ierrl,"name",trim(name))
# 152 "iotk_dat.spp"
call iotk_error_write(ierrl,"fmt",trim(fmt))
    goto 1
  end if
  call iotk_write_attr(lattr,"type",iotk_tolower("REAL"),first=.true.,ierr=ierrl)
  if(ierrl/=0) then
    call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 157 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
    goto 1
  end if
  call iotk_write_attr(lattr,"size",size(dat),ierr=ierrl)
  if(ierrl/=0) then
    call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 162 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
    goto 1
  end if
# 172 "iotk_dat.spp"
  if(binary) then
    call iotk_write_attr(lattr,"kind",kind(dat),ierr=ierrl)
    if(ierrl/=0) then
      call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 175 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
      goto 1
    end if
  end if
# 180 "iotk_dat.spp"
  if(.not.iotk_strcomp(usefmt,"!")) call iotk_write_attr(lattr,"fmt",iotk_strtrim(usefmt),ierr=ierrl)
  if(ierrl/=0) then
    call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 182 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
    goto 1
  end if
  if(lcolumns/=1) call iotk_write_attr(lattr,"columns",lcolumns,ierr=ierrl)
  if(ierrl/=0) then
    call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 187 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
    goto 1
  end if
  if(present(attr)) then
    attr_tmp(1:1)=iotk_eos
    call iotk_strcpy(attr_tmp,attr,ierr=ierrl)
    if(ierrl/=0) then
      call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 194 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
      goto 1
    end if
    call iotk_delete_attr(attr_tmp,"type",ierrl)
    if(ierrl/=0) then
      call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 199 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
      goto 1
    end if
    call iotk_delete_attr(attr_tmp,"kind",ierrl)
    if(ierrl/=0) then
      call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 204 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
      goto 1
    end if
    call iotk_delete_attr(attr_tmp,"size",ierrl)
    if(ierrl/=0) then
      call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 209 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
      goto 1
    end if
    call iotk_delete_attr(attr_tmp,"fmt",ierrl)
    if(ierrl/=0) then
      call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 214 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
      goto 1
    end if
    call iotk_delete_attr(attr_tmp,"columns",ierrl)
    if(ierrl/=0) then
      call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 219 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
      goto 1
    end if
    call iotk_delete_attr(attr_tmp,"len",ierrl)
    if(ierrl/=0) then
      call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 224 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
      goto 1
    end if
    if(iotk_strlen_trim(attr_tmp)>0) call iotk_strcat(lattr,iotk_strtrim(attr_tmp),ierr=ierrl)
  end if
  if(ierrl/=0) then
    call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 230 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
    goto 1
  end if
  call iotk_write_begin(unit,name,lattr,ierr=ierrl)
  if(ierrl/=0) then
    call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 235 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
    goto 1
  end if

  allocate(dattmp(size(dat)))
# 243 "iotk_dat.spp"
#if defined(__IOTK_WORKAROUND3) || defined(__IOTK_WORKAROUND4)
# 247 "iotk_dat.spp"
     call iotk_private_pack_REAL2(dattmp,dat,size(dattmp),1)
# 249 "iotk_dat.spp"
#else
     dattmp = pack(dat,mask=.true.)
#endif
# 253 "iotk_dat.spp"

  if(binary) then
    if(raw) then
      write(lunit,iostat=iostat) (dattmp(itmp),itmp=1,size(dattmp))
      if(iostat/=0) then
        call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 258 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
        goto 1
      end if
    else
      write(lunit,iostat=iostat) idummy,(dattmp(itmp),itmp=1,size(dattmp))
      if(iostat/=0) then
        call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 264 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
        goto 1
      end if
    end if
  else
    if(raw) then
# 273 "iotk_dat.spp"
      write(lunit,*,iostat=iostat) (dattmp(itmp),itmp=1,size(dattmp))
# 275 "iotk_dat.spp"
      if(iostat/=0) then
        call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 276 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
        goto 1
      end if
    else if(iotk_strcomp(usefmt,"*")) then
      write(lunit,*,iostat=iostat) (dattmp(itmp),itmp=1,size(dattmp))
      if(iostat/=0) then
        call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 282 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
        goto 1
      end if
    else if(iotk_strcomp(usefmt,"!")) then
# 296 "iotk_dat.spp"
     write(lunit,fmt=trim(iotk_wfmt("REAL",kind(dattmp),lcolumns,-1,lsep)),iostat=iostat) (dattmp(itmp),itmp=1,size(dattmp))
     if(iostat/=0) then
      call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 298 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
      goto 1
     end if
# 302 "iotk_dat.spp"
    else
      write(lunit,fmt=usefmt(1:iotk_strlen(usefmt)),iostat=iostat) (dattmp(itmp),itmp=1,size(dattmp))
      if(iostat/=0) then
        call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 305 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
        goto 1
      end if
    end if
  end if
  call iotk_write_end(unit,name,ierr=ierrl)
  if(ierrl/=0) then
    call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 312 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
    goto 1
  end if
1 continue
  if(allocated(dattmp)) deallocate(dattmp)
  if(present(ierr)) then
    ierr = ierrl
  else
    if(ierrl/=0) call iotk_error_handler(ierrl)
  end if
end subroutine iotk_write_dat_REAL2_7


# 668 "iotk_dat.spp"

# 670 "iotk_dat.spp"
subroutine iotk_scan_dat_REAL2_7(unit,name,dat,dummy,attr,found,default,ierr)
  use iotk_base
  use iotk_error_interf
  use iotk_dat_interf, only: iotk_scan_dat_aux
  use iotk_scan_interf
  use iotk_str_interf
  use iotk_misc_interf
  implicit none
  integer, parameter :: this_kind = iotk_REAL2
  integer,                                   intent(in)  :: unit
  character(len=*),                          intent(in)  :: name
#ifdef __IOTK_WORKAROUND6
  REAL(kind=this_kind)                        :: dat (:,:,:,:,:,:,:)
#else
  REAL(kind=this_kind),           intent(out) :: dat (:,:,:,:,:,:,:)
#endif
  type(iotk_dummytype),            optional              :: dummy
#ifdef __IOTK_WORKAROUND6
  character(len=*),                optional              :: attr
#else
  character(len=*),                optional, intent(out) :: attr
#endif
  logical,                         optional, intent(out) :: found
  REAL(kind=this_kind), optional, intent(in)  :: default (:,:,:,:,:,:,:)
  integer,                         optional, intent(out) :: ierr
# 698 "iotk_dat.spp"
  REAL (kind=this_kind),              allocatable :: tmpdat(:)
# 700 "iotk_dat.spp"
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
# 717 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
    goto 1
  end if
  call iotk_parse_dat(lattr,rtype,rkind,rsize,rlen,fmt,columns,ierrl)
! Note that columns is not effectively used
  if(ierrl/=0) goto 1
  if(.not. (iotk_strcomp(rtype,iotk_eos) .or. iotk_strcomp(rtype,"REAL") ) ) then
    call iotk_error_issue(ierrl,"iotk_scan_dat",__FILE__,__LINE__)
# 724 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
# 724 "iotk_dat.spp"
call iotk_error_msg(ierrl,' ')
# 724 "iotk_dat.spp"
call iotk_error_write(ierrl,"rtype",rtype(1:iotk_strlen(rtype)))
# 724 "iotk_dat.spp"
call iotk_error_write(ierrl,"type","REAL")
    goto 1
  end if
  if(.not. (rsize==-1 .or. rsize==size(dat)) ) then
    call iotk_error_issue(ierrl,"iotk_scan_dat",__FILE__,__LINE__)
# 728 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
    goto 1
  end if
  if(rkind==-1) rkind = kind(dat)
# 735 "iotk_dat.spp"

  allocate(tmpdat(size(dat)))
  call iotk_scan_dat_aux(unit,tmpdat,rkind,rlen,fmt(1:iotk_strlen(fmt)),ierrl)
  if(ierrl/=0) then
    call iotk_error_issue(ierrl,"iotk_scan_dat",__FILE__,__LINE__)
# 739 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
# 739 "iotk_dat.spp"
call iotk_error_msg(ierrl,'Error reading data')
# 739 "iotk_dat.spp"
call iotk_error_write(ierrl,"name",name)
# 739 "iotk_dat.spp"
call iotk_error_write(ierrl,"rkind",rkind)
# 739 "iotk_dat.spp"
call iotk_error_write(ierrl,"rlen",rlen)
    goto 1
  end if
# 745 "iotk_dat.spp"
#if defined(__IOTK_WORKAROUND3) || defined(__IOTK_WORKAROUND4)
# 749 "iotk_dat.spp"
     call iotk_private_pack_REAL2(dat,tmpdat,size(tmpdat),1)
# 751 "iotk_dat.spp"
#else
     dat = reshape(tmpdat,shape(dat))
#endif
# 755 "iotk_dat.spp"
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
# 767 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
# 767 "iotk_dat.spp"
call iotk_error_msg(ierrl,'Dat not found')
# 767 "iotk_dat.spp"
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
end subroutine iotk_scan_dat_REAL2_7


#endif
#endif

subroutine iotk_dat_dummy_REAL2_7
  write(0,*)
end subroutine iotk_dat_dummy_REAL2_7


# 45 "iotk_dat.spp"

# 48 "iotk_dat.spp"
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
# 65 "iotk_dat.spp"


!------------------------------------------------------------------------------!
! Inclusion of configuration file
#include "iotk_config.h"
!------------------------------------------------------------------------------!

# 74 "iotk_dat.spp"
#include "iotk_auxmacros.h"
# 76 "iotk_dat.spp"

# 78 "iotk_dat.spp"

#ifdef __IOTK_REAL3
#if 0 <= __IOTK_MAXRANK
# 82 "iotk_dat.spp"
subroutine iotk_write_dat_REAL3_0(unit,name,dat,dummy,attr,columns,sep,fmt,ierr)
  use iotk_base
  use iotk_error_interf
  use iotk_attr_interf, only : iotk_write_attr
  use iotk_write_interf
  use iotk_fmt_interf
  use iotk_str_interf
  use iotk_unit_interf
  use iotk_misc_interf
  implicit none
  integer, parameter :: this_kind = iotk_REAL3
  integer,                                    intent(in)  :: unit
  character(len=*),                           intent(in)  :: name
  REAL (kind=this_kind),           intent(in)  :: dat  
  type(iotk_dummytype),             optional              :: dummy
  character(len=*),                 optional, intent(in)  :: attr
  integer,                          optional, intent(in)  :: columns
  character(len=*),                 optional, intent(in)  :: sep
  character(len=*),                 optional, intent(in)  :: fmt
  integer,                          optional, intent(out) :: ierr
  integer :: ierrl,lunit,iostat
  logical :: binary,raw,stream
  integer :: lcolumns
  integer(iotk_header_kind), parameter :: idummy=0
  character(100) :: lsep
  character(300) :: usefmt
  character(iotk_attlenx) :: lattr
  character(iotk_attlenx) :: attr_tmp
  type (iotk_unit), pointer :: this
# 115 "iotk_dat.spp"
  REAL (kind=this_kind),allocatable :: dattmp(:)
# 117 "iotk_dat.spp"
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
# 126 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
      goto 1
    end if
  end if
  lunit = iotk_phys_unit(unit)
  call iotk_unit_get(lunit,pointer=this)
  raw = .false.
  if(associated(this)) then
    raw = this%raw
  end if
  call iotk_inquire(lunit,binary,stream,ierrl)
  if(ierrl/=0) then
    call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 138 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
    goto 1
  end if
  call iotk_strcpy(usefmt,"!",ierrl)
  if(ierrl/=0) then
    call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 143 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
    goto 1
  end if
  if(present(fmt) .and. .not. raw) call iotk_strcpy(usefmt,iotk_strtrim(fmt),ierrl)
  if(ierrl/=0) then
    call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 148 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
    goto 1
  end if
  if(iotk_strscan(usefmt,"<>&")/=0) then
    call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 152 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
# 152 "iotk_dat.spp"
call iotk_error_msg(ierrl,'Special characters (<>&) found in fmt string')
# 152 "iotk_dat.spp"
call iotk_error_write(ierrl,"unit",unit)
# 152 "iotk_dat.spp"
call iotk_error_write(ierrl,"name",trim(name))
# 152 "iotk_dat.spp"
call iotk_error_write(ierrl,"fmt",trim(fmt))
    goto 1
  end if
  call iotk_write_attr(lattr,"type",iotk_tolower("REAL"),first=.true.,ierr=ierrl)
  if(ierrl/=0) then
    call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 157 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
    goto 1
  end if
  call iotk_write_attr(lattr,"size",1,ierr=ierrl)
  if(ierrl/=0) then
    call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 162 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
    goto 1
  end if
# 172 "iotk_dat.spp"
  if(binary) then
    call iotk_write_attr(lattr,"kind",kind(dat),ierr=ierrl)
    if(ierrl/=0) then
      call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 175 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
      goto 1
    end if
  end if
# 180 "iotk_dat.spp"
  if(.not.iotk_strcomp(usefmt,"!")) call iotk_write_attr(lattr,"fmt",iotk_strtrim(usefmt),ierr=ierrl)
  if(ierrl/=0) then
    call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 182 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
    goto 1
  end if
  if(lcolumns/=1) call iotk_write_attr(lattr,"columns",lcolumns,ierr=ierrl)
  if(ierrl/=0) then
    call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 187 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
    goto 1
  end if
  if(present(attr)) then
    attr_tmp(1:1)=iotk_eos
    call iotk_strcpy(attr_tmp,attr,ierr=ierrl)
    if(ierrl/=0) then
      call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 194 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
      goto 1
    end if
    call iotk_delete_attr(attr_tmp,"type",ierrl)
    if(ierrl/=0) then
      call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 199 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
      goto 1
    end if
    call iotk_delete_attr(attr_tmp,"kind",ierrl)
    if(ierrl/=0) then
      call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 204 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
      goto 1
    end if
    call iotk_delete_attr(attr_tmp,"size",ierrl)
    if(ierrl/=0) then
      call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 209 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
      goto 1
    end if
    call iotk_delete_attr(attr_tmp,"fmt",ierrl)
    if(ierrl/=0) then
      call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 214 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
      goto 1
    end if
    call iotk_delete_attr(attr_tmp,"columns",ierrl)
    if(ierrl/=0) then
      call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 219 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
      goto 1
    end if
    call iotk_delete_attr(attr_tmp,"len",ierrl)
    if(ierrl/=0) then
      call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 224 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
      goto 1
    end if
    if(iotk_strlen_trim(attr_tmp)>0) call iotk_strcat(lattr,iotk_strtrim(attr_tmp),ierr=ierrl)
  end if
  if(ierrl/=0) then
    call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 230 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
    goto 1
  end if
  call iotk_write_begin(unit,name,lattr,ierr=ierrl)
  if(ierrl/=0) then
    call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 235 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
    goto 1
  end if

  allocate(dattmp(1))
# 241 "iotk_dat.spp"
     dattmp(1) = dat
# 253 "iotk_dat.spp"

  if(binary) then
    if(raw) then
      write(lunit,iostat=iostat) (dattmp(itmp),itmp=1,size(dattmp))
      if(iostat/=0) then
        call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 258 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
        goto 1
      end if
    else
      write(lunit,iostat=iostat) idummy,(dattmp(itmp),itmp=1,size(dattmp))
      if(iostat/=0) then
        call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 264 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
        goto 1
      end if
    end if
  else
    if(raw) then
# 273 "iotk_dat.spp"
      write(lunit,*,iostat=iostat) (dattmp(itmp),itmp=1,size(dattmp))
# 275 "iotk_dat.spp"
      if(iostat/=0) then
        call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 276 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
        goto 1
      end if
    else if(iotk_strcomp(usefmt,"*")) then
      write(lunit,*,iostat=iostat) (dattmp(itmp),itmp=1,size(dattmp))
      if(iostat/=0) then
        call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 282 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
        goto 1
      end if
    else if(iotk_strcomp(usefmt,"!")) then
# 296 "iotk_dat.spp"
     write(lunit,fmt=trim(iotk_wfmt("REAL",kind(dattmp),lcolumns,-1,lsep)),iostat=iostat) (dattmp(itmp),itmp=1,size(dattmp))
     if(iostat/=0) then
      call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 298 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
      goto 1
     end if
# 302 "iotk_dat.spp"
    else
      write(lunit,fmt=usefmt(1:iotk_strlen(usefmt)),iostat=iostat) (dattmp(itmp),itmp=1,size(dattmp))
      if(iostat/=0) then
        call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 305 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
        goto 1
      end if
    end if
  end if
  call iotk_write_end(unit,name,ierr=ierrl)
  if(ierrl/=0) then
    call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 312 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
    goto 1
  end if
1 continue
  if(allocated(dattmp)) deallocate(dattmp)
  if(present(ierr)) then
    ierr = ierrl
  else
    if(ierrl/=0) call iotk_error_handler(ierrl)
  end if
end subroutine iotk_write_dat_REAL3_0


# 668 "iotk_dat.spp"

# 670 "iotk_dat.spp"
subroutine iotk_scan_dat_REAL3_0(unit,name,dat,dummy,attr,found,default,ierr)
  use iotk_base
  use iotk_error_interf
  use iotk_dat_interf, only: iotk_scan_dat_aux
  use iotk_scan_interf
  use iotk_str_interf
  use iotk_misc_interf
  implicit none
  integer, parameter :: this_kind = iotk_REAL3
  integer,                                   intent(in)  :: unit
  character(len=*),                          intent(in)  :: name
#ifdef __IOTK_WORKAROUND6
  REAL(kind=this_kind)                        :: dat 
#else
  REAL(kind=this_kind),           intent(out) :: dat 
#endif
  type(iotk_dummytype),            optional              :: dummy
#ifdef __IOTK_WORKAROUND6
  character(len=*),                optional              :: attr
#else
  character(len=*),                optional, intent(out) :: attr
#endif
  logical,                         optional, intent(out) :: found
  REAL(kind=this_kind), optional, intent(in)  :: default 
  integer,                         optional, intent(out) :: ierr
# 698 "iotk_dat.spp"
  REAL (kind=this_kind),              allocatable :: tmpdat(:)
# 700 "iotk_dat.spp"
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
# 717 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
    goto 1
  end if
  call iotk_parse_dat(lattr,rtype,rkind,rsize,rlen,fmt,columns,ierrl)
! Note that columns is not effectively used
  if(ierrl/=0) goto 1
  if(.not. (iotk_strcomp(rtype,iotk_eos) .or. iotk_strcomp(rtype,"REAL") ) ) then
    call iotk_error_issue(ierrl,"iotk_scan_dat",__FILE__,__LINE__)
# 724 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
# 724 "iotk_dat.spp"
call iotk_error_msg(ierrl,' ')
# 724 "iotk_dat.spp"
call iotk_error_write(ierrl,"rtype",rtype(1:iotk_strlen(rtype)))
# 724 "iotk_dat.spp"
call iotk_error_write(ierrl,"type","REAL")
    goto 1
  end if
  if(.not. (rsize==-1 .or. rsize==1) ) then
    call iotk_error_issue(ierrl,"iotk_scan_dat",__FILE__,__LINE__)
# 728 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
    goto 1
  end if
  if(rkind==-1) rkind = kind(dat)
# 735 "iotk_dat.spp"

  allocate(tmpdat(1))
  call iotk_scan_dat_aux(unit,tmpdat,rkind,rlen,fmt(1:iotk_strlen(fmt)),ierrl)
  if(ierrl/=0) then
    call iotk_error_issue(ierrl,"iotk_scan_dat",__FILE__,__LINE__)
# 739 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
# 739 "iotk_dat.spp"
call iotk_error_msg(ierrl,'Error reading data')
# 739 "iotk_dat.spp"
call iotk_error_write(ierrl,"name",name)
# 739 "iotk_dat.spp"
call iotk_error_write(ierrl,"rkind",rkind)
# 739 "iotk_dat.spp"
call iotk_error_write(ierrl,"rlen",rlen)
    goto 1
  end if
# 743 "iotk_dat.spp"
     dat = tmpdat(1)
# 755 "iotk_dat.spp"
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
# 767 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
# 767 "iotk_dat.spp"
call iotk_error_msg(ierrl,'Dat not found')
# 767 "iotk_dat.spp"
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
end subroutine iotk_scan_dat_REAL3_0


#endif
#endif

subroutine iotk_dat_dummy_REAL3_0
  write(0,*)
end subroutine iotk_dat_dummy_REAL3_0


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

#ifdef __IOTK_REAL3
#if 1 <= __IOTK_MAXRANK
# 82 "iotk_dat.spp"
subroutine iotk_write_dat_REAL3_1(unit,name,dat,dummy,attr,columns,sep,fmt,ierr)
  use iotk_base
  use iotk_error_interf
  use iotk_attr_interf, only : iotk_write_attr
  use iotk_write_interf
  use iotk_fmt_interf
  use iotk_str_interf
  use iotk_unit_interf
  use iotk_misc_interf
  implicit none
  integer, parameter :: this_kind = iotk_REAL3
  integer,                                    intent(in)  :: unit
  character(len=*),                           intent(in)  :: name
  REAL (kind=this_kind),           intent(in)  :: dat (:) 
  type(iotk_dummytype),             optional              :: dummy
  character(len=*),                 optional, intent(in)  :: attr
  integer,                          optional, intent(in)  :: columns
  character(len=*),                 optional, intent(in)  :: sep
  character(len=*),                 optional, intent(in)  :: fmt
  integer,                          optional, intent(out) :: ierr
  integer :: ierrl,lunit,iostat
  logical :: binary,raw,stream
  integer :: lcolumns
  integer(iotk_header_kind), parameter :: idummy=0
  character(100) :: lsep
  character(300) :: usefmt
  character(iotk_attlenx) :: lattr
  character(iotk_attlenx) :: attr_tmp
  type (iotk_unit), pointer :: this
# 115 "iotk_dat.spp"
  REAL (kind=this_kind),allocatable :: dattmp(:)
# 117 "iotk_dat.spp"
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
# 126 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
      goto 1
    end if
  end if
  lunit = iotk_phys_unit(unit)
  call iotk_unit_get(lunit,pointer=this)
  raw = .false.
  if(associated(this)) then
    raw = this%raw
  end if
  call iotk_inquire(lunit,binary,stream,ierrl)
  if(ierrl/=0) then
    call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 138 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
    goto 1
  end if
  call iotk_strcpy(usefmt,"!",ierrl)
  if(ierrl/=0) then
    call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 143 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
    goto 1
  end if
  if(present(fmt) .and. .not. raw) call iotk_strcpy(usefmt,iotk_strtrim(fmt),ierrl)
  if(ierrl/=0) then
    call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 148 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
    goto 1
  end if
  if(iotk_strscan(usefmt,"<>&")/=0) then
    call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 152 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
# 152 "iotk_dat.spp"
call iotk_error_msg(ierrl,'Special characters (<>&) found in fmt string')
# 152 "iotk_dat.spp"
call iotk_error_write(ierrl,"unit",unit)
# 152 "iotk_dat.spp"
call iotk_error_write(ierrl,"name",trim(name))
# 152 "iotk_dat.spp"
call iotk_error_write(ierrl,"fmt",trim(fmt))
    goto 1
  end if
  call iotk_write_attr(lattr,"type",iotk_tolower("REAL"),first=.true.,ierr=ierrl)
  if(ierrl/=0) then
    call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 157 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
    goto 1
  end if
  call iotk_write_attr(lattr,"size",size(dat),ierr=ierrl)
  if(ierrl/=0) then
    call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 162 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
    goto 1
  end if
# 172 "iotk_dat.spp"
  if(binary) then
    call iotk_write_attr(lattr,"kind",kind(dat),ierr=ierrl)
    if(ierrl/=0) then
      call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 175 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
      goto 1
    end if
  end if
# 180 "iotk_dat.spp"
  if(.not.iotk_strcomp(usefmt,"!")) call iotk_write_attr(lattr,"fmt",iotk_strtrim(usefmt),ierr=ierrl)
  if(ierrl/=0) then
    call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 182 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
    goto 1
  end if
  if(lcolumns/=1) call iotk_write_attr(lattr,"columns",lcolumns,ierr=ierrl)
  if(ierrl/=0) then
    call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 187 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
    goto 1
  end if
  if(present(attr)) then
    attr_tmp(1:1)=iotk_eos
    call iotk_strcpy(attr_tmp,attr,ierr=ierrl)
    if(ierrl/=0) then
      call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 194 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
      goto 1
    end if
    call iotk_delete_attr(attr_tmp,"type",ierrl)
    if(ierrl/=0) then
      call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 199 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
      goto 1
    end if
    call iotk_delete_attr(attr_tmp,"kind",ierrl)
    if(ierrl/=0) then
      call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 204 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
      goto 1
    end if
    call iotk_delete_attr(attr_tmp,"size",ierrl)
    if(ierrl/=0) then
      call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 209 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
      goto 1
    end if
    call iotk_delete_attr(attr_tmp,"fmt",ierrl)
    if(ierrl/=0) then
      call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 214 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
      goto 1
    end if
    call iotk_delete_attr(attr_tmp,"columns",ierrl)
    if(ierrl/=0) then
      call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 219 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
      goto 1
    end if
    call iotk_delete_attr(attr_tmp,"len",ierrl)
    if(ierrl/=0) then
      call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 224 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
      goto 1
    end if
    if(iotk_strlen_trim(attr_tmp)>0) call iotk_strcat(lattr,iotk_strtrim(attr_tmp),ierr=ierrl)
  end if
  if(ierrl/=0) then
    call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 230 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
    goto 1
  end if
  call iotk_write_begin(unit,name,lattr,ierr=ierrl)
  if(ierrl/=0) then
    call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 235 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
    goto 1
  end if

  allocate(dattmp(size(dat)))
# 243 "iotk_dat.spp"
#if defined(__IOTK_WORKAROUND3) || defined(__IOTK_WORKAROUND4)
# 247 "iotk_dat.spp"
     call iotk_private_pack_REAL3(dattmp,dat,size(dattmp),1)
# 249 "iotk_dat.spp"
#else
     dattmp = pack(dat,mask=.true.)
#endif
# 253 "iotk_dat.spp"

  if(binary) then
    if(raw) then
      write(lunit,iostat=iostat) (dattmp(itmp),itmp=1,size(dattmp))
      if(iostat/=0) then
        call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 258 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
        goto 1
      end if
    else
      write(lunit,iostat=iostat) idummy,(dattmp(itmp),itmp=1,size(dattmp))
      if(iostat/=0) then
        call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 264 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
        goto 1
      end if
    end if
  else
    if(raw) then
# 273 "iotk_dat.spp"
      write(lunit,*,iostat=iostat) (dattmp(itmp),itmp=1,size(dattmp))
# 275 "iotk_dat.spp"
      if(iostat/=0) then
        call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 276 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
        goto 1
      end if
    else if(iotk_strcomp(usefmt,"*")) then
      write(lunit,*,iostat=iostat) (dattmp(itmp),itmp=1,size(dattmp))
      if(iostat/=0) then
        call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 282 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
        goto 1
      end if
    else if(iotk_strcomp(usefmt,"!")) then
# 296 "iotk_dat.spp"
     write(lunit,fmt=trim(iotk_wfmt("REAL",kind(dattmp),lcolumns,-1,lsep)),iostat=iostat) (dattmp(itmp),itmp=1,size(dattmp))
     if(iostat/=0) then
      call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 298 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
      goto 1
     end if
# 302 "iotk_dat.spp"
    else
      write(lunit,fmt=usefmt(1:iotk_strlen(usefmt)),iostat=iostat) (dattmp(itmp),itmp=1,size(dattmp))
      if(iostat/=0) then
        call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 305 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
        goto 1
      end if
    end if
  end if
  call iotk_write_end(unit,name,ierr=ierrl)
  if(ierrl/=0) then
    call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 312 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
    goto 1
  end if
1 continue
  if(allocated(dattmp)) deallocate(dattmp)
  if(present(ierr)) then
    ierr = ierrl
  else
    if(ierrl/=0) call iotk_error_handler(ierrl)
  end if
end subroutine iotk_write_dat_REAL3_1


# 327 "iotk_dat.spp"
recursive subroutine iotk_scan_dat_aux_REAL3(unit,dat,rkind,rlen,fmt,ierr)
  use iotk_base
  use iotk_error_interf
  use iotk_attr_interf, only: iotk_read
  use iotk_scan_interf
  use iotk_str_interf
  use iotk_unit_interf
  use iotk_misc_interf
  use iotk_stream_interf
  implicit none
  integer, parameter :: this_kind = iotk_REAL3
  integer,                         intent(in)  :: unit
#ifdef __IOTK_WORKAROUND6
  REAL(kind=this_kind)              :: dat (:)
#else
  REAL(kind=this_kind), intent(out) :: dat (:)
#endif
  integer,                         intent(in)  :: rkind
  integer,                         intent(in)  :: rlen
  character(len=*),                intent(in)  :: fmt
  integer,                         intent(out) :: ierr
  integer(iotk_header_kind) :: idummy
  logical :: raw,binary,stream
  integer :: lunit
# 352 "iotk_dat.spp"
  integer :: i
# 354 "iotk_dat.spp"
#ifdef __IOTK_WORKAROUND3
  integer :: j
#endif
  integer :: index,length,nexttag,iostat,altlength
  type(iotk_unit), pointer :: this
  character(len=iotk_linlenx) :: line,altline
# 365 "iotk_dat.spp"
#ifdef __IOTK_REAL1
  REAL (kind=iotk_REAL1), allocatable :: dat1 (:)
#endif
# 365 "iotk_dat.spp"
#ifdef __IOTK_REAL2
  REAL (kind=iotk_REAL2), allocatable :: dat2 (:)
#endif
# 365 "iotk_dat.spp"
#ifdef __IOTK_REAL4
  REAL (kind=iotk_REAL4), allocatable :: dat4 (:)
#endif
# 371 "iotk_dat.spp"
  lunit = iotk_phys_unit(unit)
  ierr = 0
  iostat = 0
  idummy = 0
  call iotk_unit_get(lunit,pointer=this)
  raw = .false.
  if(associated(this)) then
    raw = this%raw
  end if
  call iotk_inquire(lunit,binary,stream,ierr)
  if(ierr/=0) then
    call iotk_error_issue(ierr,"iotk_scan_dat_aux",__FILE__,__LINE__)
# 382 "iotk_dat.spp"
call iotk_error_msg(ierr,"CVS Revision: 1.25 ")
    return
  end if
# 500 "iotk_dat.spp"
  if(binary) then
    select case(rkind)
    case(kind(dat))
      if(raw) then
#ifdef __IOTK_WORKAROUND3
        read(lunit,iostat=iostat) ( dat(j), j=1,ubound(dat,1) )
#else
        read(lunit,iostat=iostat) dat
#endif
        if(iostat/=0) then
          call iotk_error_issue(ierr,"iotk_scan_dat_aux",__FILE__,__LINE__)
# 510 "iotk_dat.spp"
call iotk_error_msg(ierr,"CVS Revision: 1.25 ")
# 510 "iotk_dat.spp"
call iotk_error_msg(ierr,' ')
# 510 "iotk_dat.spp"
call iotk_error_write(ierr,"iostat",iostat)
          return
        end if
      else
        if(stream) then
          call iotk_stream_read(lunit,idummy,dat,ierr=ierr)
          if(ierr/=0) then
            call iotk_error_issue(ierr,"iotk_scan_dat_aux",__FILE__,__LINE__)
# 517 "iotk_dat.spp"
call iotk_error_msg(ierr,"CVS Revision: 1.25 ")
            return
          end if
        else
#ifdef __IOTK_WORKAROUND3
          read(lunit,iostat=iostat) idummy, ( dat(j), j=1,ubound(dat,1) )
#else
          read(lunit,iostat=iostat) idummy, dat
#endif
        end if
        if(iostat/=0) then
          call iotk_error_issue(ierr,"iotk_scan_dat_aux",__FILE__,__LINE__)
# 528 "iotk_dat.spp"
call iotk_error_msg(ierr,"CVS Revision: 1.25 ")
# 528 "iotk_dat.spp"
call iotk_error_msg(ierr,' ')
# 528 "iotk_dat.spp"
call iotk_error_write(ierr,"iostat",iostat)
          return
        end if
      end if
# 534 "iotk_dat.spp"
#ifdef __IOTK_REAL1
    case(kind(dat1))
      ! Giusto per scrupolo. Se e' raw non ci sono info sul kind, quindi questa linea e' irraggiungibile
      if(raw) then
        call iotk_error_issue(ierr,"iotk_scan_dat_aux",__FILE__,__LINE__)
# 538 "iotk_dat.spp"
call iotk_error_msg(ierr,"CVS Revision: 1.25 ")
        return
      end if
      allocate(dat1(ubound(dat,1)))
      if(stream) then
        call iotk_stream_read(lunit,idummy,dat1,ierr=ierr)
        if(ierr/=0) then
          deallocate(dat1)
          call iotk_error_issue(ierr,"iotk_scan_dat_aux",__FILE__,__LINE__)
# 546 "iotk_dat.spp"
call iotk_error_msg(ierr,"CVS Revision: 1.25 ")
# 546 "iotk_dat.spp"
call iotk_error_msg(ierr,' ')
# 546 "iotk_dat.spp"
call iotk_error_write(ierr,"iostat",iostat)
          return
        end if
      else
        read(lunit,iostat=iostat) idummy,( dat1(i), i=1,ubound(dat1,1) )
        if(iostat/=0) then
          deallocate(dat1)
          call iotk_error_issue(ierr,"iotk_scan_dat_aux",__FILE__,__LINE__)
# 553 "iotk_dat.spp"
call iotk_error_msg(ierr,"CVS Revision: 1.25 ")
# 553 "iotk_dat.spp"
call iotk_error_msg(ierr,' ')
# 553 "iotk_dat.spp"
call iotk_error_write(ierr,"iostat",iostat)
          return
        end if
      end if
# 566 "iotk_dat.spp"
      dat = real(dat1,kind=kind(dat))
# 570 "iotk_dat.spp"
      deallocate(dat1)
#endif
# 534 "iotk_dat.spp"
#ifdef __IOTK_REAL2
    case(kind(dat2))
      ! Giusto per scrupolo. Se e' raw non ci sono info sul kind, quindi questa linea e' irraggiungibile
      if(raw) then
        call iotk_error_issue(ierr,"iotk_scan_dat_aux",__FILE__,__LINE__)
# 538 "iotk_dat.spp"
call iotk_error_msg(ierr,"CVS Revision: 1.25 ")
        return
      end if
      allocate(dat2(ubound(dat,1)))
      if(stream) then
        call iotk_stream_read(lunit,idummy,dat2,ierr=ierr)
        if(ierr/=0) then
          deallocate(dat2)
          call iotk_error_issue(ierr,"iotk_scan_dat_aux",__FILE__,__LINE__)
# 546 "iotk_dat.spp"
call iotk_error_msg(ierr,"CVS Revision: 1.25 ")
# 546 "iotk_dat.spp"
call iotk_error_msg(ierr,' ')
# 546 "iotk_dat.spp"
call iotk_error_write(ierr,"iostat",iostat)
          return
        end if
      else
        read(lunit,iostat=iostat) idummy,( dat2(i), i=1,ubound(dat2,1) )
        if(iostat/=0) then
          deallocate(dat2)
          call iotk_error_issue(ierr,"iotk_scan_dat_aux",__FILE__,__LINE__)
# 553 "iotk_dat.spp"
call iotk_error_msg(ierr,"CVS Revision: 1.25 ")
# 553 "iotk_dat.spp"
call iotk_error_msg(ierr,' ')
# 553 "iotk_dat.spp"
call iotk_error_write(ierr,"iostat",iostat)
          return
        end if
      end if
# 566 "iotk_dat.spp"
      dat = real(dat2,kind=kind(dat))
# 570 "iotk_dat.spp"
      deallocate(dat2)
#endif
# 534 "iotk_dat.spp"
#ifdef __IOTK_REAL4
    case(kind(dat4))
      ! Giusto per scrupolo. Se e' raw non ci sono info sul kind, quindi questa linea e' irraggiungibile
      if(raw) then
        call iotk_error_issue(ierr,"iotk_scan_dat_aux",__FILE__,__LINE__)
# 538 "iotk_dat.spp"
call iotk_error_msg(ierr,"CVS Revision: 1.25 ")
        return
      end if
      allocate(dat4(ubound(dat,1)))
      if(stream) then
        call iotk_stream_read(lunit,idummy,dat4,ierr=ierr)
        if(ierr/=0) then
          deallocate(dat4)
          call iotk_error_issue(ierr,"iotk_scan_dat_aux",__FILE__,__LINE__)
# 546 "iotk_dat.spp"
call iotk_error_msg(ierr,"CVS Revision: 1.25 ")
# 546 "iotk_dat.spp"
call iotk_error_msg(ierr,' ')
# 546 "iotk_dat.spp"
call iotk_error_write(ierr,"iostat",iostat)
          return
        end if
      else
        read(lunit,iostat=iostat) idummy,( dat4(i), i=1,ubound(dat4,1) )
        if(iostat/=0) then
          deallocate(dat4)
          call iotk_error_issue(ierr,"iotk_scan_dat_aux",__FILE__,__LINE__)
# 553 "iotk_dat.spp"
call iotk_error_msg(ierr,"CVS Revision: 1.25 ")
# 553 "iotk_dat.spp"
call iotk_error_msg(ierr,' ')
# 553 "iotk_dat.spp"
call iotk_error_write(ierr,"iostat",iostat)
          return
        end if
      end if
# 566 "iotk_dat.spp"
      dat = real(dat4,kind=kind(dat))
# 570 "iotk_dat.spp"
      deallocate(dat4)
#endif
# 574 "iotk_dat.spp"
    case default
      call iotk_error_issue(ierr,"iotk_scan_dat_aux",__FILE__,__LINE__)
# 575 "iotk_dat.spp"
call iotk_error_msg(ierr,"CVS Revision: 1.25 ")
# 575 "iotk_dat.spp"
call iotk_error_msg(ierr,'Kind incompatibility')
# 575 "iotk_dat.spp"
call iotk_error_write(ierr,"kind",rkind)
    end select
  else
    if(raw) then
#ifdef __IOTK_WORKAROUND3
      read(lunit,fmt=*,iostat=iostat) ( dat(j), j=1,ubound(dat,1) )
#else
      read(lunit,fmt=*,iostat=iostat) dat
#endif
      if(iostat/=0) then
        call iotk_error_issue(ierr,"iotk_scan_dat_aux",__FILE__,__LINE__)
# 585 "iotk_dat.spp"
call iotk_error_msg(ierr,"CVS Revision: 1.25 ")
# 585 "iotk_dat.spp"
call iotk_error_msg(ierr,' ')
# 585 "iotk_dat.spp"
call iotk_error_write(ierr,"iostat",iostat)
        return
      end if
    else if(iotk_strcomp(fmt,"*")) then
#ifdef __IOTK_WORKAROUND3
      read(lunit,fmt=*,iostat=iostat) ( dat(j), j=1,ubound(dat,1) )
#else
      read(lunit,fmt=*,iostat=iostat) dat
#endif
      if(iostat/=0) then
        call iotk_error_issue(ierr,"iotk_scan_dat_aux",__FILE__,__LINE__)
# 595 "iotk_dat.spp"
call iotk_error_msg(ierr,"CVS Revision: 1.25 ")
# 595 "iotk_dat.spp"
call iotk_error_msg(ierr,' ')
# 595 "iotk_dat.spp"
call iotk_error_write(ierr,"iostat",iostat)
        return
      end if
    else if(iotk_strcomp(fmt,"!")) then
      index = 0
      do
        call iotk_getline(lunit,line,length,ierr)
        if(ierr/=0) then
          call iotk_error_issue(ierr,"iotk_scan_dat_aux",__FILE__,__LINE__)
# 603 "iotk_dat.spp"
call iotk_error_msg(ierr,"CVS Revision: 1.25 ")
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
# 614 "iotk_dat.spp"
call iotk_error_msg(ierr,"CVS Revision: 1.25 ")
# 614 "iotk_dat.spp"
call iotk_error_msg(ierr,' ')
# 614 "iotk_dat.spp"
call iotk_error_write(ierr,"iostat",iostat)
            return
          end if
          call iotk_getline(lunit,altline,altlength,ierr)
          if(ierr/=0) then
            call iotk_error_issue(ierr,"iotk_scan_dat_aux",__FILE__,__LINE__)
# 619 "iotk_dat.spp"
call iotk_error_msg(ierr,"CVS Revision: 1.25 ")
            return
          end if
          backspace(lunit,iostat=iostat)
          if(iostat/=0) then
            call iotk_error_issue(ierr,"iotk_scan_dat_aux",__FILE__,__LINE__)
# 624 "iotk_dat.spp"
call iotk_error_msg(ierr,"CVS Revision: 1.25 ")
# 624 "iotk_dat.spp"
call iotk_error_msg(ierr,' ')
# 624 "iotk_dat.spp"
call iotk_error_write(ierr,"iostat",iostat)
            return
          end if
          read(lunit,"(a)",advance="no",iostat=iostat) altline(1:nexttag-1 + altlength - length)
          if(iostat/=0) then
            call iotk_error_issue(ierr,"iotk_scan_dat_aux",__FILE__,__LINE__)
# 629 "iotk_dat.spp"
call iotk_error_msg(ierr,"CVS Revision: 1.25 ")
# 629 "iotk_dat.spp"
call iotk_error_msg(ierr,' ')
# 629 "iotk_dat.spp"
call iotk_error_write(ierr,"iostat",iostat)
            return
          end if
        end if
        call iotk_str_clean(line(1:nexttag - 1))
        call iotk_read(dat,line(1:nexttag - 1),index,ierr)
        if(ierr/=0) then
          call iotk_error_issue(ierr,"iotk_scan_dat_aux",__FILE__,__LINE__)
# 636 "iotk_dat.spp"
call iotk_error_msg(ierr,"CVS Revision: 1.25 ")
# 636 "iotk_dat.spp"
call iotk_error_msg(ierr,'Error reading REAL data')
          return
        end if
# 642 "iotk_dat.spp"
        if(index == size(dat)) exit
# 644 "iotk_dat.spp"
        if(nexttag/=length + 1) then
          call iotk_error_issue(ierr,"iotk_scan_dat_aux",__FILE__,__LINE__)
# 645 "iotk_dat.spp"
call iotk_error_msg(ierr,"CVS Revision: 1.25 ")
          return
        end if
      end do
    else
#ifdef __IOTK_WORKAROUND3
      read(lunit,fmt=fmt(1:iotk_strlen(fmt)),iostat=iostat) ( dat(j), j=1,ubound(dat,1) )
#else
      read(lunit,fmt=fmt(1:iotk_strlen(fmt)),iostat=iostat) dat
#endif
      if(iostat/=0) then
        call iotk_error_issue(ierr,"iotk_scan_dat_aux",__FILE__,__LINE__)
# 656 "iotk_dat.spp"
call iotk_error_msg(ierr,"CVS Revision: 1.25 ")
# 656 "iotk_dat.spp"
call iotk_error_msg(ierr,' ')
# 656 "iotk_dat.spp"
call iotk_error_write(ierr,"iostat",iostat)
        return
      end if
    end if
  end if
# 662 "iotk_dat.spp"
  if(idummy/=0) then
    call iotk_error_issue(ierr,"iotk_scan_dat_aux",__FILE__,__LINE__)
# 663 "iotk_dat.spp"
call iotk_error_msg(ierr,"CVS Revision: 1.25 ")
    return
  end if
end subroutine iotk_scan_dat_aux_REAL3
# 668 "iotk_dat.spp"

# 670 "iotk_dat.spp"
subroutine iotk_scan_dat_REAL3_1(unit,name,dat,dummy,attr,found,default,ierr)
  use iotk_base
  use iotk_error_interf
  use iotk_dat_interf, only: iotk_scan_dat_aux
  use iotk_scan_interf
  use iotk_str_interf
  use iotk_misc_interf
  implicit none
  integer, parameter :: this_kind = iotk_REAL3
  integer,                                   intent(in)  :: unit
  character(len=*),                          intent(in)  :: name
#ifdef __IOTK_WORKAROUND6
  REAL(kind=this_kind)                        :: dat (:)
#else
  REAL(kind=this_kind),           intent(out) :: dat (:)
#endif
  type(iotk_dummytype),            optional              :: dummy
#ifdef __IOTK_WORKAROUND6
  character(len=*),                optional              :: attr
#else
  character(len=*),                optional, intent(out) :: attr
#endif
  logical,                         optional, intent(out) :: found
  REAL(kind=this_kind), optional, intent(in)  :: default (:)
  integer,                         optional, intent(out) :: ierr
# 698 "iotk_dat.spp"
  REAL (kind=this_kind),              allocatable :: tmpdat(:)
# 700 "iotk_dat.spp"
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
# 717 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
    goto 1
  end if
  call iotk_parse_dat(lattr,rtype,rkind,rsize,rlen,fmt,columns,ierrl)
! Note that columns is not effectively used
  if(ierrl/=0) goto 1
  if(.not. (iotk_strcomp(rtype,iotk_eos) .or. iotk_strcomp(rtype,"REAL") ) ) then
    call iotk_error_issue(ierrl,"iotk_scan_dat",__FILE__,__LINE__)
# 724 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
# 724 "iotk_dat.spp"
call iotk_error_msg(ierrl,' ')
# 724 "iotk_dat.spp"
call iotk_error_write(ierrl,"rtype",rtype(1:iotk_strlen(rtype)))
# 724 "iotk_dat.spp"
call iotk_error_write(ierrl,"type","REAL")
    goto 1
  end if
  if(.not. (rsize==-1 .or. rsize==size(dat)) ) then
    call iotk_error_issue(ierrl,"iotk_scan_dat",__FILE__,__LINE__)
# 728 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
    goto 1
  end if
  if(rkind==-1) rkind = kind(dat)
# 735 "iotk_dat.spp"

  allocate(tmpdat(size(dat)))
  call iotk_scan_dat_aux(unit,tmpdat,rkind,rlen,fmt(1:iotk_strlen(fmt)),ierrl)
  if(ierrl/=0) then
    call iotk_error_issue(ierrl,"iotk_scan_dat",__FILE__,__LINE__)
# 739 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
# 739 "iotk_dat.spp"
call iotk_error_msg(ierrl,'Error reading data')
# 739 "iotk_dat.spp"
call iotk_error_write(ierrl,"name",name)
# 739 "iotk_dat.spp"
call iotk_error_write(ierrl,"rkind",rkind)
# 739 "iotk_dat.spp"
call iotk_error_write(ierrl,"rlen",rlen)
    goto 1
  end if
# 745 "iotk_dat.spp"
#if defined(__IOTK_WORKAROUND3) || defined(__IOTK_WORKAROUND4)
# 749 "iotk_dat.spp"
     call iotk_private_pack_REAL3(dat,tmpdat,size(tmpdat),1)
# 751 "iotk_dat.spp"
#else
     dat = reshape(tmpdat,shape(dat))
#endif
# 755 "iotk_dat.spp"
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
# 767 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
# 767 "iotk_dat.spp"
call iotk_error_msg(ierrl,'Dat not found')
# 767 "iotk_dat.spp"
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
end subroutine iotk_scan_dat_REAL3_1


#endif
#endif

subroutine iotk_dat_dummy_REAL3_1
  write(0,*)
end subroutine iotk_dat_dummy_REAL3_1


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

#ifdef __IOTK_REAL3
#if 2 <= __IOTK_MAXRANK
# 82 "iotk_dat.spp"
subroutine iotk_write_dat_REAL3_2(unit,name,dat,dummy,attr,columns,sep,fmt,ierr)
  use iotk_base
  use iotk_error_interf
  use iotk_attr_interf, only : iotk_write_attr
  use iotk_write_interf
  use iotk_fmt_interf
  use iotk_str_interf
  use iotk_unit_interf
  use iotk_misc_interf
  implicit none
  integer, parameter :: this_kind = iotk_REAL3
  integer,                                    intent(in)  :: unit
  character(len=*),                           intent(in)  :: name
  REAL (kind=this_kind),           intent(in)  :: dat (:,:) 
  type(iotk_dummytype),             optional              :: dummy
  character(len=*),                 optional, intent(in)  :: attr
  integer,                          optional, intent(in)  :: columns
  character(len=*),                 optional, intent(in)  :: sep
  character(len=*),                 optional, intent(in)  :: fmt
  integer,                          optional, intent(out) :: ierr
  integer :: ierrl,lunit,iostat
  logical :: binary,raw,stream
  integer :: lcolumns
  integer(iotk_header_kind), parameter :: idummy=0
  character(100) :: lsep
  character(300) :: usefmt
  character(iotk_attlenx) :: lattr
  character(iotk_attlenx) :: attr_tmp
  type (iotk_unit), pointer :: this
# 115 "iotk_dat.spp"
  REAL (kind=this_kind),allocatable :: dattmp(:)
# 117 "iotk_dat.spp"
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
# 126 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
      goto 1
    end if
  end if
  lunit = iotk_phys_unit(unit)
  call iotk_unit_get(lunit,pointer=this)
  raw = .false.
  if(associated(this)) then
    raw = this%raw
  end if
  call iotk_inquire(lunit,binary,stream,ierrl)
  if(ierrl/=0) then
    call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 138 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
    goto 1
  end if
  call iotk_strcpy(usefmt,"!",ierrl)
  if(ierrl/=0) then
    call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 143 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
    goto 1
  end if
  if(present(fmt) .and. .not. raw) call iotk_strcpy(usefmt,iotk_strtrim(fmt),ierrl)
  if(ierrl/=0) then
    call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 148 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
    goto 1
  end if
  if(iotk_strscan(usefmt,"<>&")/=0) then
    call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 152 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
# 152 "iotk_dat.spp"
call iotk_error_msg(ierrl,'Special characters (<>&) found in fmt string')
# 152 "iotk_dat.spp"
call iotk_error_write(ierrl,"unit",unit)
# 152 "iotk_dat.spp"
call iotk_error_write(ierrl,"name",trim(name))
# 152 "iotk_dat.spp"
call iotk_error_write(ierrl,"fmt",trim(fmt))
    goto 1
  end if
  call iotk_write_attr(lattr,"type",iotk_tolower("REAL"),first=.true.,ierr=ierrl)
  if(ierrl/=0) then
    call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 157 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
    goto 1
  end if
  call iotk_write_attr(lattr,"size",size(dat),ierr=ierrl)
  if(ierrl/=0) then
    call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 162 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
    goto 1
  end if
# 172 "iotk_dat.spp"
  if(binary) then
    call iotk_write_attr(lattr,"kind",kind(dat),ierr=ierrl)
    if(ierrl/=0) then
      call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 175 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
      goto 1
    end if
  end if
# 180 "iotk_dat.spp"
  if(.not.iotk_strcomp(usefmt,"!")) call iotk_write_attr(lattr,"fmt",iotk_strtrim(usefmt),ierr=ierrl)
  if(ierrl/=0) then
    call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 182 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
    goto 1
  end if
  if(lcolumns/=1) call iotk_write_attr(lattr,"columns",lcolumns,ierr=ierrl)
  if(ierrl/=0) then
    call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 187 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
    goto 1
  end if
  if(present(attr)) then
    attr_tmp(1:1)=iotk_eos
    call iotk_strcpy(attr_tmp,attr,ierr=ierrl)
    if(ierrl/=0) then
      call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 194 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
      goto 1
    end if
    call iotk_delete_attr(attr_tmp,"type",ierrl)
    if(ierrl/=0) then
      call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 199 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
      goto 1
    end if
    call iotk_delete_attr(attr_tmp,"kind",ierrl)
    if(ierrl/=0) then
      call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 204 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
      goto 1
    end if
    call iotk_delete_attr(attr_tmp,"size",ierrl)
    if(ierrl/=0) then
      call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 209 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
      goto 1
    end if
    call iotk_delete_attr(attr_tmp,"fmt",ierrl)
    if(ierrl/=0) then
      call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 214 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
      goto 1
    end if
    call iotk_delete_attr(attr_tmp,"columns",ierrl)
    if(ierrl/=0) then
      call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 219 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
      goto 1
    end if
    call iotk_delete_attr(attr_tmp,"len",ierrl)
    if(ierrl/=0) then
      call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 224 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
      goto 1
    end if
    if(iotk_strlen_trim(attr_tmp)>0) call iotk_strcat(lattr,iotk_strtrim(attr_tmp),ierr=ierrl)
  end if
  if(ierrl/=0) then
    call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 230 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
    goto 1
  end if
  call iotk_write_begin(unit,name,lattr,ierr=ierrl)
  if(ierrl/=0) then
    call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 235 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
    goto 1
  end if

  allocate(dattmp(size(dat)))
# 243 "iotk_dat.spp"
#if defined(__IOTK_WORKAROUND3) || defined(__IOTK_WORKAROUND4)
# 247 "iotk_dat.spp"
     call iotk_private_pack_REAL3(dattmp,dat,size(dattmp),1)
# 249 "iotk_dat.spp"
#else
     dattmp = pack(dat,mask=.true.)
#endif
# 253 "iotk_dat.spp"

  if(binary) then
    if(raw) then
      write(lunit,iostat=iostat) (dattmp(itmp),itmp=1,size(dattmp))
      if(iostat/=0) then
        call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 258 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
        goto 1
      end if
    else
      write(lunit,iostat=iostat) idummy,(dattmp(itmp),itmp=1,size(dattmp))
      if(iostat/=0) then
        call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 264 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
        goto 1
      end if
    end if
  else
    if(raw) then
# 273 "iotk_dat.spp"
      write(lunit,*,iostat=iostat) (dattmp(itmp),itmp=1,size(dattmp))
# 275 "iotk_dat.spp"
      if(iostat/=0) then
        call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 276 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
        goto 1
      end if
    else if(iotk_strcomp(usefmt,"*")) then
      write(lunit,*,iostat=iostat) (dattmp(itmp),itmp=1,size(dattmp))
      if(iostat/=0) then
        call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 282 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
        goto 1
      end if
    else if(iotk_strcomp(usefmt,"!")) then
# 296 "iotk_dat.spp"
     write(lunit,fmt=trim(iotk_wfmt("REAL",kind(dattmp),lcolumns,-1,lsep)),iostat=iostat) (dattmp(itmp),itmp=1,size(dattmp))
     if(iostat/=0) then
      call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 298 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
      goto 1
     end if
# 302 "iotk_dat.spp"
    else
      write(lunit,fmt=usefmt(1:iotk_strlen(usefmt)),iostat=iostat) (dattmp(itmp),itmp=1,size(dattmp))
      if(iostat/=0) then
        call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 305 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
        goto 1
      end if
    end if
  end if
  call iotk_write_end(unit,name,ierr=ierrl)
  if(ierrl/=0) then
    call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 312 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
    goto 1
  end if
1 continue
  if(allocated(dattmp)) deallocate(dattmp)
  if(present(ierr)) then
    ierr = ierrl
  else
    if(ierrl/=0) call iotk_error_handler(ierrl)
  end if
end subroutine iotk_write_dat_REAL3_2


# 668 "iotk_dat.spp"

# 670 "iotk_dat.spp"
subroutine iotk_scan_dat_REAL3_2(unit,name,dat,dummy,attr,found,default,ierr)
  use iotk_base
  use iotk_error_interf
  use iotk_dat_interf, only: iotk_scan_dat_aux
  use iotk_scan_interf
  use iotk_str_interf
  use iotk_misc_interf
  implicit none
  integer, parameter :: this_kind = iotk_REAL3
  integer,                                   intent(in)  :: unit
  character(len=*),                          intent(in)  :: name
#ifdef __IOTK_WORKAROUND6
  REAL(kind=this_kind)                        :: dat (:,:)
#else
  REAL(kind=this_kind),           intent(out) :: dat (:,:)
#endif
  type(iotk_dummytype),            optional              :: dummy
#ifdef __IOTK_WORKAROUND6
  character(len=*),                optional              :: attr
#else
  character(len=*),                optional, intent(out) :: attr
#endif
  logical,                         optional, intent(out) :: found
  REAL(kind=this_kind), optional, intent(in)  :: default (:,:)
  integer,                         optional, intent(out) :: ierr
# 698 "iotk_dat.spp"
  REAL (kind=this_kind),              allocatable :: tmpdat(:)
# 700 "iotk_dat.spp"
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
# 717 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
    goto 1
  end if
  call iotk_parse_dat(lattr,rtype,rkind,rsize,rlen,fmt,columns,ierrl)
! Note that columns is not effectively used
  if(ierrl/=0) goto 1
  if(.not. (iotk_strcomp(rtype,iotk_eos) .or. iotk_strcomp(rtype,"REAL") ) ) then
    call iotk_error_issue(ierrl,"iotk_scan_dat",__FILE__,__LINE__)
# 724 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
# 724 "iotk_dat.spp"
call iotk_error_msg(ierrl,' ')
# 724 "iotk_dat.spp"
call iotk_error_write(ierrl,"rtype",rtype(1:iotk_strlen(rtype)))
# 724 "iotk_dat.spp"
call iotk_error_write(ierrl,"type","REAL")
    goto 1
  end if
  if(.not. (rsize==-1 .or. rsize==size(dat)) ) then
    call iotk_error_issue(ierrl,"iotk_scan_dat",__FILE__,__LINE__)
# 728 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
    goto 1
  end if
  if(rkind==-1) rkind = kind(dat)
# 735 "iotk_dat.spp"

  allocate(tmpdat(size(dat)))
  call iotk_scan_dat_aux(unit,tmpdat,rkind,rlen,fmt(1:iotk_strlen(fmt)),ierrl)
  if(ierrl/=0) then
    call iotk_error_issue(ierrl,"iotk_scan_dat",__FILE__,__LINE__)
# 739 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
# 739 "iotk_dat.spp"
call iotk_error_msg(ierrl,'Error reading data')
# 739 "iotk_dat.spp"
call iotk_error_write(ierrl,"name",name)
# 739 "iotk_dat.spp"
call iotk_error_write(ierrl,"rkind",rkind)
# 739 "iotk_dat.spp"
call iotk_error_write(ierrl,"rlen",rlen)
    goto 1
  end if
# 745 "iotk_dat.spp"
#if defined(__IOTK_WORKAROUND3) || defined(__IOTK_WORKAROUND4)
# 749 "iotk_dat.spp"
     call iotk_private_pack_REAL3(dat,tmpdat,size(tmpdat),1)
# 751 "iotk_dat.spp"
#else
     dat = reshape(tmpdat,shape(dat))
#endif
# 755 "iotk_dat.spp"
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
# 767 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
# 767 "iotk_dat.spp"
call iotk_error_msg(ierrl,'Dat not found')
# 767 "iotk_dat.spp"
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
end subroutine iotk_scan_dat_REAL3_2


#endif
#endif

subroutine iotk_dat_dummy_REAL3_2
  write(0,*)
end subroutine iotk_dat_dummy_REAL3_2


# 45 "iotk_dat.spp"

# 48 "iotk_dat.spp"
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
# 65 "iotk_dat.spp"


!------------------------------------------------------------------------------!
! Inclusion of configuration file
#include "iotk_config.h"
!------------------------------------------------------------------------------!

# 74 "iotk_dat.spp"
#include "iotk_auxmacros.h"
# 76 "iotk_dat.spp"

# 78 "iotk_dat.spp"

#ifdef __IOTK_REAL3
#if 3 <= __IOTK_MAXRANK
# 82 "iotk_dat.spp"
subroutine iotk_write_dat_REAL3_3(unit,name,dat,dummy,attr,columns,sep,fmt,ierr)
  use iotk_base
  use iotk_error_interf
  use iotk_attr_interf, only : iotk_write_attr
  use iotk_write_interf
  use iotk_fmt_interf
  use iotk_str_interf
  use iotk_unit_interf
  use iotk_misc_interf
  implicit none
  integer, parameter :: this_kind = iotk_REAL3
  integer,                                    intent(in)  :: unit
  character(len=*),                           intent(in)  :: name
  REAL (kind=this_kind),           intent(in)  :: dat (:,:,:) 
  type(iotk_dummytype),             optional              :: dummy
  character(len=*),                 optional, intent(in)  :: attr
  integer,                          optional, intent(in)  :: columns
  character(len=*),                 optional, intent(in)  :: sep
  character(len=*),                 optional, intent(in)  :: fmt
  integer,                          optional, intent(out) :: ierr
  integer :: ierrl,lunit,iostat
  logical :: binary,raw,stream
  integer :: lcolumns
  integer(iotk_header_kind), parameter :: idummy=0
  character(100) :: lsep
  character(300) :: usefmt
  character(iotk_attlenx) :: lattr
  character(iotk_attlenx) :: attr_tmp
  type (iotk_unit), pointer :: this
# 115 "iotk_dat.spp"
  REAL (kind=this_kind),allocatable :: dattmp(:)
# 117 "iotk_dat.spp"
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
# 126 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
      goto 1
    end if
  end if
  lunit = iotk_phys_unit(unit)
  call iotk_unit_get(lunit,pointer=this)
  raw = .false.
  if(associated(this)) then
    raw = this%raw
  end if
  call iotk_inquire(lunit,binary,stream,ierrl)
  if(ierrl/=0) then
    call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 138 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
    goto 1
  end if
  call iotk_strcpy(usefmt,"!",ierrl)
  if(ierrl/=0) then
    call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 143 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
    goto 1
  end if
  if(present(fmt) .and. .not. raw) call iotk_strcpy(usefmt,iotk_strtrim(fmt),ierrl)
  if(ierrl/=0) then
    call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 148 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
    goto 1
  end if
  if(iotk_strscan(usefmt,"<>&")/=0) then
    call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 152 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
# 152 "iotk_dat.spp"
call iotk_error_msg(ierrl,'Special characters (<>&) found in fmt string')
# 152 "iotk_dat.spp"
call iotk_error_write(ierrl,"unit",unit)
# 152 "iotk_dat.spp"
call iotk_error_write(ierrl,"name",trim(name))
# 152 "iotk_dat.spp"
call iotk_error_write(ierrl,"fmt",trim(fmt))
    goto 1
  end if
  call iotk_write_attr(lattr,"type",iotk_tolower("REAL"),first=.true.,ierr=ierrl)
  if(ierrl/=0) then
    call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 157 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
    goto 1
  end if
  call iotk_write_attr(lattr,"size",size(dat),ierr=ierrl)
  if(ierrl/=0) then
    call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 162 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
    goto 1
  end if
# 172 "iotk_dat.spp"
  if(binary) then
    call iotk_write_attr(lattr,"kind",kind(dat),ierr=ierrl)
    if(ierrl/=0) then
      call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 175 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
      goto 1
    end if
  end if
# 180 "iotk_dat.spp"
  if(.not.iotk_strcomp(usefmt,"!")) call iotk_write_attr(lattr,"fmt",iotk_strtrim(usefmt),ierr=ierrl)
  if(ierrl/=0) then
    call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 182 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
    goto 1
  end if
  if(lcolumns/=1) call iotk_write_attr(lattr,"columns",lcolumns,ierr=ierrl)
  if(ierrl/=0) then
    call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 187 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
    goto 1
  end if
  if(present(attr)) then
    attr_tmp(1:1)=iotk_eos
    call iotk_strcpy(attr_tmp,attr,ierr=ierrl)
    if(ierrl/=0) then
      call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 194 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
      goto 1
    end if
    call iotk_delete_attr(attr_tmp,"type",ierrl)
    if(ierrl/=0) then
      call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 199 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
      goto 1
    end if
    call iotk_delete_attr(attr_tmp,"kind",ierrl)
    if(ierrl/=0) then
      call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 204 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
      goto 1
    end if
    call iotk_delete_attr(attr_tmp,"size",ierrl)
    if(ierrl/=0) then
      call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 209 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
      goto 1
    end if
    call iotk_delete_attr(attr_tmp,"fmt",ierrl)
    if(ierrl/=0) then
      call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 214 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
      goto 1
    end if
    call iotk_delete_attr(attr_tmp,"columns",ierrl)
    if(ierrl/=0) then
      call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 219 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
      goto 1
    end if
    call iotk_delete_attr(attr_tmp,"len",ierrl)
    if(ierrl/=0) then
      call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 224 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
      goto 1
    end if
    if(iotk_strlen_trim(attr_tmp)>0) call iotk_strcat(lattr,iotk_strtrim(attr_tmp),ierr=ierrl)
  end if
  if(ierrl/=0) then
    call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 230 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
    goto 1
  end if
  call iotk_write_begin(unit,name,lattr,ierr=ierrl)
  if(ierrl/=0) then
    call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 235 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
    goto 1
  end if

  allocate(dattmp(size(dat)))
# 243 "iotk_dat.spp"
#if defined(__IOTK_WORKAROUND3) || defined(__IOTK_WORKAROUND4)
# 247 "iotk_dat.spp"
     call iotk_private_pack_REAL3(dattmp,dat,size(dattmp),1)
# 249 "iotk_dat.spp"
#else
     dattmp = pack(dat,mask=.true.)
#endif
# 253 "iotk_dat.spp"

  if(binary) then
    if(raw) then
      write(lunit,iostat=iostat) (dattmp(itmp),itmp=1,size(dattmp))
      if(iostat/=0) then
        call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 258 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
        goto 1
      end if
    else
      write(lunit,iostat=iostat) idummy,(dattmp(itmp),itmp=1,size(dattmp))
      if(iostat/=0) then
        call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 264 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
        goto 1
      end if
    end if
  else
    if(raw) then
# 273 "iotk_dat.spp"
      write(lunit,*,iostat=iostat) (dattmp(itmp),itmp=1,size(dattmp))
# 275 "iotk_dat.spp"
      if(iostat/=0) then
        call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 276 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
        goto 1
      end if
    else if(iotk_strcomp(usefmt,"*")) then
      write(lunit,*,iostat=iostat) (dattmp(itmp),itmp=1,size(dattmp))
      if(iostat/=0) then
        call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 282 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
        goto 1
      end if
    else if(iotk_strcomp(usefmt,"!")) then
# 296 "iotk_dat.spp"
     write(lunit,fmt=trim(iotk_wfmt("REAL",kind(dattmp),lcolumns,-1,lsep)),iostat=iostat) (dattmp(itmp),itmp=1,size(dattmp))
     if(iostat/=0) then
      call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 298 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
      goto 1
     end if
# 302 "iotk_dat.spp"
    else
      write(lunit,fmt=usefmt(1:iotk_strlen(usefmt)),iostat=iostat) (dattmp(itmp),itmp=1,size(dattmp))
      if(iostat/=0) then
        call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 305 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
        goto 1
      end if
    end if
  end if
  call iotk_write_end(unit,name,ierr=ierrl)
  if(ierrl/=0) then
    call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 312 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
    goto 1
  end if
1 continue
  if(allocated(dattmp)) deallocate(dattmp)
  if(present(ierr)) then
    ierr = ierrl
  else
    if(ierrl/=0) call iotk_error_handler(ierrl)
  end if
end subroutine iotk_write_dat_REAL3_3


# 668 "iotk_dat.spp"

# 670 "iotk_dat.spp"
subroutine iotk_scan_dat_REAL3_3(unit,name,dat,dummy,attr,found,default,ierr)
  use iotk_base
  use iotk_error_interf
  use iotk_dat_interf, only: iotk_scan_dat_aux
  use iotk_scan_interf
  use iotk_str_interf
  use iotk_misc_interf
  implicit none
  integer, parameter :: this_kind = iotk_REAL3
  integer,                                   intent(in)  :: unit
  character(len=*),                          intent(in)  :: name
#ifdef __IOTK_WORKAROUND6
  REAL(kind=this_kind)                        :: dat (:,:,:)
#else
  REAL(kind=this_kind),           intent(out) :: dat (:,:,:)
#endif
  type(iotk_dummytype),            optional              :: dummy
#ifdef __IOTK_WORKAROUND6
  character(len=*),                optional              :: attr
#else
  character(len=*),                optional, intent(out) :: attr
#endif
  logical,                         optional, intent(out) :: found
  REAL(kind=this_kind), optional, intent(in)  :: default (:,:,:)
  integer,                         optional, intent(out) :: ierr
# 698 "iotk_dat.spp"
  REAL (kind=this_kind),              allocatable :: tmpdat(:)
# 700 "iotk_dat.spp"
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
# 717 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
    goto 1
  end if
  call iotk_parse_dat(lattr,rtype,rkind,rsize,rlen,fmt,columns,ierrl)
! Note that columns is not effectively used
  if(ierrl/=0) goto 1
  if(.not. (iotk_strcomp(rtype,iotk_eos) .or. iotk_strcomp(rtype,"REAL") ) ) then
    call iotk_error_issue(ierrl,"iotk_scan_dat",__FILE__,__LINE__)
# 724 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
# 724 "iotk_dat.spp"
call iotk_error_msg(ierrl,' ')
# 724 "iotk_dat.spp"
call iotk_error_write(ierrl,"rtype",rtype(1:iotk_strlen(rtype)))
# 724 "iotk_dat.spp"
call iotk_error_write(ierrl,"type","REAL")
    goto 1
  end if
  if(.not. (rsize==-1 .or. rsize==size(dat)) ) then
    call iotk_error_issue(ierrl,"iotk_scan_dat",__FILE__,__LINE__)
# 728 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
    goto 1
  end if
  if(rkind==-1) rkind = kind(dat)
# 735 "iotk_dat.spp"

  allocate(tmpdat(size(dat)))
  call iotk_scan_dat_aux(unit,tmpdat,rkind,rlen,fmt(1:iotk_strlen(fmt)),ierrl)
  if(ierrl/=0) then
    call iotk_error_issue(ierrl,"iotk_scan_dat",__FILE__,__LINE__)
# 739 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
# 739 "iotk_dat.spp"
call iotk_error_msg(ierrl,'Error reading data')
# 739 "iotk_dat.spp"
call iotk_error_write(ierrl,"name",name)
# 739 "iotk_dat.spp"
call iotk_error_write(ierrl,"rkind",rkind)
# 739 "iotk_dat.spp"
call iotk_error_write(ierrl,"rlen",rlen)
    goto 1
  end if
# 745 "iotk_dat.spp"
#if defined(__IOTK_WORKAROUND3) || defined(__IOTK_WORKAROUND4)
# 749 "iotk_dat.spp"
     call iotk_private_pack_REAL3(dat,tmpdat,size(tmpdat),1)
# 751 "iotk_dat.spp"
#else
     dat = reshape(tmpdat,shape(dat))
#endif
# 755 "iotk_dat.spp"
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
# 767 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
# 767 "iotk_dat.spp"
call iotk_error_msg(ierrl,'Dat not found')
# 767 "iotk_dat.spp"
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
end subroutine iotk_scan_dat_REAL3_3


#endif
#endif

subroutine iotk_dat_dummy_REAL3_3
  write(0,*)
end subroutine iotk_dat_dummy_REAL3_3


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

#ifdef __IOTK_REAL3
#if 4 <= __IOTK_MAXRANK
# 82 "iotk_dat.spp"
subroutine iotk_write_dat_REAL3_4(unit,name,dat,dummy,attr,columns,sep,fmt,ierr)
  use iotk_base
  use iotk_error_interf
  use iotk_attr_interf, only : iotk_write_attr
  use iotk_write_interf
  use iotk_fmt_interf
  use iotk_str_interf
  use iotk_unit_interf
  use iotk_misc_interf
  implicit none
  integer, parameter :: this_kind = iotk_REAL3
  integer,                                    intent(in)  :: unit
  character(len=*),                           intent(in)  :: name
  REAL (kind=this_kind),           intent(in)  :: dat (:,:,:,:) 
  type(iotk_dummytype),             optional              :: dummy
  character(len=*),                 optional, intent(in)  :: attr
  integer,                          optional, intent(in)  :: columns
  character(len=*),                 optional, intent(in)  :: sep
  character(len=*),                 optional, intent(in)  :: fmt
  integer,                          optional, intent(out) :: ierr
  integer :: ierrl,lunit,iostat
  logical :: binary,raw,stream
  integer :: lcolumns
  integer(iotk_header_kind), parameter :: idummy=0
  character(100) :: lsep
  character(300) :: usefmt
  character(iotk_attlenx) :: lattr
  character(iotk_attlenx) :: attr_tmp
  type (iotk_unit), pointer :: this
# 115 "iotk_dat.spp"
  REAL (kind=this_kind),allocatable :: dattmp(:)
# 117 "iotk_dat.spp"
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
# 126 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
      goto 1
    end if
  end if
  lunit = iotk_phys_unit(unit)
  call iotk_unit_get(lunit,pointer=this)
  raw = .false.
  if(associated(this)) then
    raw = this%raw
  end if
  call iotk_inquire(lunit,binary,stream,ierrl)
  if(ierrl/=0) then
    call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 138 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
    goto 1
  end if
  call iotk_strcpy(usefmt,"!",ierrl)
  if(ierrl/=0) then
    call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 143 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
    goto 1
  end if
  if(present(fmt) .and. .not. raw) call iotk_strcpy(usefmt,iotk_strtrim(fmt),ierrl)
  if(ierrl/=0) then
    call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 148 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
    goto 1
  end if
  if(iotk_strscan(usefmt,"<>&")/=0) then
    call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 152 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
# 152 "iotk_dat.spp"
call iotk_error_msg(ierrl,'Special characters (<>&) found in fmt string')
# 152 "iotk_dat.spp"
call iotk_error_write(ierrl,"unit",unit)
# 152 "iotk_dat.spp"
call iotk_error_write(ierrl,"name",trim(name))
# 152 "iotk_dat.spp"
call iotk_error_write(ierrl,"fmt",trim(fmt))
    goto 1
  end if
  call iotk_write_attr(lattr,"type",iotk_tolower("REAL"),first=.true.,ierr=ierrl)
  if(ierrl/=0) then
    call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 157 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
    goto 1
  end if
  call iotk_write_attr(lattr,"size",size(dat),ierr=ierrl)
  if(ierrl/=0) then
    call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 162 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
    goto 1
  end if
# 172 "iotk_dat.spp"
  if(binary) then
    call iotk_write_attr(lattr,"kind",kind(dat),ierr=ierrl)
    if(ierrl/=0) then
      call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 175 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
      goto 1
    end if
  end if
# 180 "iotk_dat.spp"
  if(.not.iotk_strcomp(usefmt,"!")) call iotk_write_attr(lattr,"fmt",iotk_strtrim(usefmt),ierr=ierrl)
  if(ierrl/=0) then
    call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 182 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
    goto 1
  end if
  if(lcolumns/=1) call iotk_write_attr(lattr,"columns",lcolumns,ierr=ierrl)
  if(ierrl/=0) then
    call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 187 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
    goto 1
  end if
  if(present(attr)) then
    attr_tmp(1:1)=iotk_eos
    call iotk_strcpy(attr_tmp,attr,ierr=ierrl)
    if(ierrl/=0) then
      call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 194 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
      goto 1
    end if
    call iotk_delete_attr(attr_tmp,"type",ierrl)
    if(ierrl/=0) then
      call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 199 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
      goto 1
    end if
    call iotk_delete_attr(attr_tmp,"kind",ierrl)
    if(ierrl/=0) then
      call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 204 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
      goto 1
    end if
    call iotk_delete_attr(attr_tmp,"size",ierrl)
    if(ierrl/=0) then
      call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 209 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
      goto 1
    end if
    call iotk_delete_attr(attr_tmp,"fmt",ierrl)
    if(ierrl/=0) then
      call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 214 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
      goto 1
    end if
    call iotk_delete_attr(attr_tmp,"columns",ierrl)
    if(ierrl/=0) then
      call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 219 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
      goto 1
    end if
    call iotk_delete_attr(attr_tmp,"len",ierrl)
    if(ierrl/=0) then
      call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 224 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
      goto 1
    end if
    if(iotk_strlen_trim(attr_tmp)>0) call iotk_strcat(lattr,iotk_strtrim(attr_tmp),ierr=ierrl)
  end if
  if(ierrl/=0) then
    call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 230 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
    goto 1
  end if
  call iotk_write_begin(unit,name,lattr,ierr=ierrl)
  if(ierrl/=0) then
    call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 235 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
    goto 1
  end if

  allocate(dattmp(size(dat)))
# 243 "iotk_dat.spp"
#if defined(__IOTK_WORKAROUND3) || defined(__IOTK_WORKAROUND4)
# 247 "iotk_dat.spp"
     call iotk_private_pack_REAL3(dattmp,dat,size(dattmp),1)
# 249 "iotk_dat.spp"
#else
     dattmp = pack(dat,mask=.true.)
#endif
# 253 "iotk_dat.spp"

  if(binary) then
    if(raw) then
      write(lunit,iostat=iostat) (dattmp(itmp),itmp=1,size(dattmp))
      if(iostat/=0) then
        call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 258 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
        goto 1
      end if
    else
      write(lunit,iostat=iostat) idummy,(dattmp(itmp),itmp=1,size(dattmp))
      if(iostat/=0) then
        call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 264 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
        goto 1
      end if
    end if
  else
    if(raw) then
# 273 "iotk_dat.spp"
      write(lunit,*,iostat=iostat) (dattmp(itmp),itmp=1,size(dattmp))
# 275 "iotk_dat.spp"
      if(iostat/=0) then
        call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 276 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
        goto 1
      end if
    else if(iotk_strcomp(usefmt,"*")) then
      write(lunit,*,iostat=iostat) (dattmp(itmp),itmp=1,size(dattmp))
      if(iostat/=0) then
        call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 282 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
        goto 1
      end if
    else if(iotk_strcomp(usefmt,"!")) then
# 296 "iotk_dat.spp"
     write(lunit,fmt=trim(iotk_wfmt("REAL",kind(dattmp),lcolumns,-1,lsep)),iostat=iostat) (dattmp(itmp),itmp=1,size(dattmp))
     if(iostat/=0) then
      call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 298 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
      goto 1
     end if
# 302 "iotk_dat.spp"
    else
      write(lunit,fmt=usefmt(1:iotk_strlen(usefmt)),iostat=iostat) (dattmp(itmp),itmp=1,size(dattmp))
      if(iostat/=0) then
        call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 305 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
        goto 1
      end if
    end if
  end if
  call iotk_write_end(unit,name,ierr=ierrl)
  if(ierrl/=0) then
    call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 312 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
    goto 1
  end if
1 continue
  if(allocated(dattmp)) deallocate(dattmp)
  if(present(ierr)) then
    ierr = ierrl
  else
    if(ierrl/=0) call iotk_error_handler(ierrl)
  end if
end subroutine iotk_write_dat_REAL3_4


# 668 "iotk_dat.spp"

# 670 "iotk_dat.spp"
subroutine iotk_scan_dat_REAL3_4(unit,name,dat,dummy,attr,found,default,ierr)
  use iotk_base
  use iotk_error_interf
  use iotk_dat_interf, only: iotk_scan_dat_aux
  use iotk_scan_interf
  use iotk_str_interf
  use iotk_misc_interf
  implicit none
  integer, parameter :: this_kind = iotk_REAL3
  integer,                                   intent(in)  :: unit
  character(len=*),                          intent(in)  :: name
#ifdef __IOTK_WORKAROUND6
  REAL(kind=this_kind)                        :: dat (:,:,:,:)
#else
  REAL(kind=this_kind),           intent(out) :: dat (:,:,:,:)
#endif
  type(iotk_dummytype),            optional              :: dummy
#ifdef __IOTK_WORKAROUND6
  character(len=*),                optional              :: attr
#else
  character(len=*),                optional, intent(out) :: attr
#endif
  logical,                         optional, intent(out) :: found
  REAL(kind=this_kind), optional, intent(in)  :: default (:,:,:,:)
  integer,                         optional, intent(out) :: ierr
# 698 "iotk_dat.spp"
  REAL (kind=this_kind),              allocatable :: tmpdat(:)
# 700 "iotk_dat.spp"
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
# 717 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
    goto 1
  end if
  call iotk_parse_dat(lattr,rtype,rkind,rsize,rlen,fmt,columns,ierrl)
! Note that columns is not effectively used
  if(ierrl/=0) goto 1
  if(.not. (iotk_strcomp(rtype,iotk_eos) .or. iotk_strcomp(rtype,"REAL") ) ) then
    call iotk_error_issue(ierrl,"iotk_scan_dat",__FILE__,__LINE__)
# 724 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
# 724 "iotk_dat.spp"
call iotk_error_msg(ierrl,' ')
# 724 "iotk_dat.spp"
call iotk_error_write(ierrl,"rtype",rtype(1:iotk_strlen(rtype)))
# 724 "iotk_dat.spp"
call iotk_error_write(ierrl,"type","REAL")
    goto 1
  end if
  if(.not. (rsize==-1 .or. rsize==size(dat)) ) then
    call iotk_error_issue(ierrl,"iotk_scan_dat",__FILE__,__LINE__)
# 728 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
    goto 1
  end if
  if(rkind==-1) rkind = kind(dat)
# 735 "iotk_dat.spp"

  allocate(tmpdat(size(dat)))
  call iotk_scan_dat_aux(unit,tmpdat,rkind,rlen,fmt(1:iotk_strlen(fmt)),ierrl)
  if(ierrl/=0) then
    call iotk_error_issue(ierrl,"iotk_scan_dat",__FILE__,__LINE__)
# 739 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
# 739 "iotk_dat.spp"
call iotk_error_msg(ierrl,'Error reading data')
# 739 "iotk_dat.spp"
call iotk_error_write(ierrl,"name",name)
# 739 "iotk_dat.spp"
call iotk_error_write(ierrl,"rkind",rkind)
# 739 "iotk_dat.spp"
call iotk_error_write(ierrl,"rlen",rlen)
    goto 1
  end if
# 745 "iotk_dat.spp"
#if defined(__IOTK_WORKAROUND3) || defined(__IOTK_WORKAROUND4)
# 749 "iotk_dat.spp"
     call iotk_private_pack_REAL3(dat,tmpdat,size(tmpdat),1)
# 751 "iotk_dat.spp"
#else
     dat = reshape(tmpdat,shape(dat))
#endif
# 755 "iotk_dat.spp"
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
# 767 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
# 767 "iotk_dat.spp"
call iotk_error_msg(ierrl,'Dat not found')
# 767 "iotk_dat.spp"
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
end subroutine iotk_scan_dat_REAL3_4


#endif
#endif

subroutine iotk_dat_dummy_REAL3_4
  write(0,*)
end subroutine iotk_dat_dummy_REAL3_4


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

#ifdef __IOTK_REAL3
#if 5 <= __IOTK_MAXRANK
# 82 "iotk_dat.spp"
subroutine iotk_write_dat_REAL3_5(unit,name,dat,dummy,attr,columns,sep,fmt,ierr)
  use iotk_base
  use iotk_error_interf
  use iotk_attr_interf, only : iotk_write_attr
  use iotk_write_interf
  use iotk_fmt_interf
  use iotk_str_interf
  use iotk_unit_interf
  use iotk_misc_interf
  implicit none
  integer, parameter :: this_kind = iotk_REAL3
  integer,                                    intent(in)  :: unit
  character(len=*),                           intent(in)  :: name
  REAL (kind=this_kind),           intent(in)  :: dat (:,:,:,:,:) 
  type(iotk_dummytype),             optional              :: dummy
  character(len=*),                 optional, intent(in)  :: attr
  integer,                          optional, intent(in)  :: columns
  character(len=*),                 optional, intent(in)  :: sep
  character(len=*),                 optional, intent(in)  :: fmt
  integer,                          optional, intent(out) :: ierr
  integer :: ierrl,lunit,iostat
  logical :: binary,raw,stream
  integer :: lcolumns
  integer(iotk_header_kind), parameter :: idummy=0
  character(100) :: lsep
  character(300) :: usefmt
  character(iotk_attlenx) :: lattr
  character(iotk_attlenx) :: attr_tmp
  type (iotk_unit), pointer :: this
# 115 "iotk_dat.spp"
  REAL (kind=this_kind),allocatable :: dattmp(:)
# 117 "iotk_dat.spp"
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
# 126 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
      goto 1
    end if
  end if
  lunit = iotk_phys_unit(unit)
  call iotk_unit_get(lunit,pointer=this)
  raw = .false.
  if(associated(this)) then
    raw = this%raw
  end if
  call iotk_inquire(lunit,binary,stream,ierrl)
  if(ierrl/=0) then
    call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 138 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
    goto 1
  end if
  call iotk_strcpy(usefmt,"!",ierrl)
  if(ierrl/=0) then
    call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 143 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
    goto 1
  end if
  if(present(fmt) .and. .not. raw) call iotk_strcpy(usefmt,iotk_strtrim(fmt),ierrl)
  if(ierrl/=0) then
    call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 148 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
    goto 1
  end if
  if(iotk_strscan(usefmt,"<>&")/=0) then
    call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 152 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
# 152 "iotk_dat.spp"
call iotk_error_msg(ierrl,'Special characters (<>&) found in fmt string')
# 152 "iotk_dat.spp"
call iotk_error_write(ierrl,"unit",unit)
# 152 "iotk_dat.spp"
call iotk_error_write(ierrl,"name",trim(name))
# 152 "iotk_dat.spp"
call iotk_error_write(ierrl,"fmt",trim(fmt))
    goto 1
  end if
  call iotk_write_attr(lattr,"type",iotk_tolower("REAL"),first=.true.,ierr=ierrl)
  if(ierrl/=0) then
    call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 157 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
    goto 1
  end if
  call iotk_write_attr(lattr,"size",size(dat),ierr=ierrl)
  if(ierrl/=0) then
    call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 162 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
    goto 1
  end if
# 172 "iotk_dat.spp"
  if(binary) then
    call iotk_write_attr(lattr,"kind",kind(dat),ierr=ierrl)
    if(ierrl/=0) then
      call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 175 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
      goto 1
    end if
  end if
# 180 "iotk_dat.spp"
  if(.not.iotk_strcomp(usefmt,"!")) call iotk_write_attr(lattr,"fmt",iotk_strtrim(usefmt),ierr=ierrl)
  if(ierrl/=0) then
    call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 182 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
    goto 1
  end if
  if(lcolumns/=1) call iotk_write_attr(lattr,"columns",lcolumns,ierr=ierrl)
  if(ierrl/=0) then
    call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 187 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
    goto 1
  end if
  if(present(attr)) then
    attr_tmp(1:1)=iotk_eos
    call iotk_strcpy(attr_tmp,attr,ierr=ierrl)
    if(ierrl/=0) then
      call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 194 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
      goto 1
    end if
    call iotk_delete_attr(attr_tmp,"type",ierrl)
    if(ierrl/=0) then
      call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 199 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
      goto 1
    end if
    call iotk_delete_attr(attr_tmp,"kind",ierrl)
    if(ierrl/=0) then
      call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 204 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
      goto 1
    end if
    call iotk_delete_attr(attr_tmp,"size",ierrl)
    if(ierrl/=0) then
      call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 209 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
      goto 1
    end if
    call iotk_delete_attr(attr_tmp,"fmt",ierrl)
    if(ierrl/=0) then
      call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 214 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
      goto 1
    end if
    call iotk_delete_attr(attr_tmp,"columns",ierrl)
    if(ierrl/=0) then
      call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 219 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
      goto 1
    end if
    call iotk_delete_attr(attr_tmp,"len",ierrl)
    if(ierrl/=0) then
      call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 224 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
      goto 1
    end if
    if(iotk_strlen_trim(attr_tmp)>0) call iotk_strcat(lattr,iotk_strtrim(attr_tmp),ierr=ierrl)
  end if
  if(ierrl/=0) then
    call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 230 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
    goto 1
  end if
  call iotk_write_begin(unit,name,lattr,ierr=ierrl)
  if(ierrl/=0) then
    call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 235 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
    goto 1
  end if

  allocate(dattmp(size(dat)))
# 243 "iotk_dat.spp"
#if defined(__IOTK_WORKAROUND3) || defined(__IOTK_WORKAROUND4)
# 247 "iotk_dat.spp"
     call iotk_private_pack_REAL3(dattmp,dat,size(dattmp),1)
# 249 "iotk_dat.spp"
#else
     dattmp = pack(dat,mask=.true.)
#endif
# 253 "iotk_dat.spp"

  if(binary) then
    if(raw) then
      write(lunit,iostat=iostat) (dattmp(itmp),itmp=1,size(dattmp))
      if(iostat/=0) then
        call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 258 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
        goto 1
      end if
    else
      write(lunit,iostat=iostat) idummy,(dattmp(itmp),itmp=1,size(dattmp))
      if(iostat/=0) then
        call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 264 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
        goto 1
      end if
    end if
  else
    if(raw) then
# 273 "iotk_dat.spp"
      write(lunit,*,iostat=iostat) (dattmp(itmp),itmp=1,size(dattmp))
# 275 "iotk_dat.spp"
      if(iostat/=0) then
        call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 276 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
        goto 1
      end if
    else if(iotk_strcomp(usefmt,"*")) then
      write(lunit,*,iostat=iostat) (dattmp(itmp),itmp=1,size(dattmp))
      if(iostat/=0) then
        call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 282 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
        goto 1
      end if
    else if(iotk_strcomp(usefmt,"!")) then
# 296 "iotk_dat.spp"
     write(lunit,fmt=trim(iotk_wfmt("REAL",kind(dattmp),lcolumns,-1,lsep)),iostat=iostat) (dattmp(itmp),itmp=1,size(dattmp))
     if(iostat/=0) then
      call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 298 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
      goto 1
     end if
# 302 "iotk_dat.spp"
    else
      write(lunit,fmt=usefmt(1:iotk_strlen(usefmt)),iostat=iostat) (dattmp(itmp),itmp=1,size(dattmp))
      if(iostat/=0) then
        call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 305 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
        goto 1
      end if
    end if
  end if
  call iotk_write_end(unit,name,ierr=ierrl)
  if(ierrl/=0) then
    call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 312 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
    goto 1
  end if
1 continue
  if(allocated(dattmp)) deallocate(dattmp)
  if(present(ierr)) then
    ierr = ierrl
  else
    if(ierrl/=0) call iotk_error_handler(ierrl)
  end if
end subroutine iotk_write_dat_REAL3_5


# 668 "iotk_dat.spp"

# 670 "iotk_dat.spp"
subroutine iotk_scan_dat_REAL3_5(unit,name,dat,dummy,attr,found,default,ierr)
  use iotk_base
  use iotk_error_interf
  use iotk_dat_interf, only: iotk_scan_dat_aux
  use iotk_scan_interf
  use iotk_str_interf
  use iotk_misc_interf
  implicit none
  integer, parameter :: this_kind = iotk_REAL3
  integer,                                   intent(in)  :: unit
  character(len=*),                          intent(in)  :: name
#ifdef __IOTK_WORKAROUND6
  REAL(kind=this_kind)                        :: dat (:,:,:,:,:)
#else
  REAL(kind=this_kind),           intent(out) :: dat (:,:,:,:,:)
#endif
  type(iotk_dummytype),            optional              :: dummy
#ifdef __IOTK_WORKAROUND6
  character(len=*),                optional              :: attr
#else
  character(len=*),                optional, intent(out) :: attr
#endif
  logical,                         optional, intent(out) :: found
  REAL(kind=this_kind), optional, intent(in)  :: default (:,:,:,:,:)
  integer,                         optional, intent(out) :: ierr
# 698 "iotk_dat.spp"
  REAL (kind=this_kind),              allocatable :: tmpdat(:)
# 700 "iotk_dat.spp"
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
# 717 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
    goto 1
  end if
  call iotk_parse_dat(lattr,rtype,rkind,rsize,rlen,fmt,columns,ierrl)
! Note that columns is not effectively used
  if(ierrl/=0) goto 1
  if(.not. (iotk_strcomp(rtype,iotk_eos) .or. iotk_strcomp(rtype,"REAL") ) ) then
    call iotk_error_issue(ierrl,"iotk_scan_dat",__FILE__,__LINE__)
# 724 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
# 724 "iotk_dat.spp"
call iotk_error_msg(ierrl,' ')
# 724 "iotk_dat.spp"
call iotk_error_write(ierrl,"rtype",rtype(1:iotk_strlen(rtype)))
# 724 "iotk_dat.spp"
call iotk_error_write(ierrl,"type","REAL")
    goto 1
  end if
  if(.not. (rsize==-1 .or. rsize==size(dat)) ) then
    call iotk_error_issue(ierrl,"iotk_scan_dat",__FILE__,__LINE__)
# 728 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
    goto 1
  end if
  if(rkind==-1) rkind = kind(dat)
# 735 "iotk_dat.spp"

  allocate(tmpdat(size(dat)))
  call iotk_scan_dat_aux(unit,tmpdat,rkind,rlen,fmt(1:iotk_strlen(fmt)),ierrl)
  if(ierrl/=0) then
    call iotk_error_issue(ierrl,"iotk_scan_dat",__FILE__,__LINE__)
# 739 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
# 739 "iotk_dat.spp"
call iotk_error_msg(ierrl,'Error reading data')
# 739 "iotk_dat.spp"
call iotk_error_write(ierrl,"name",name)
# 739 "iotk_dat.spp"
call iotk_error_write(ierrl,"rkind",rkind)
# 739 "iotk_dat.spp"
call iotk_error_write(ierrl,"rlen",rlen)
    goto 1
  end if
# 745 "iotk_dat.spp"
#if defined(__IOTK_WORKAROUND3) || defined(__IOTK_WORKAROUND4)
# 749 "iotk_dat.spp"
     call iotk_private_pack_REAL3(dat,tmpdat,size(tmpdat),1)
# 751 "iotk_dat.spp"
#else
     dat = reshape(tmpdat,shape(dat))
#endif
# 755 "iotk_dat.spp"
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
# 767 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
# 767 "iotk_dat.spp"
call iotk_error_msg(ierrl,'Dat not found')
# 767 "iotk_dat.spp"
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
end subroutine iotk_scan_dat_REAL3_5


#endif
#endif

subroutine iotk_dat_dummy_REAL3_5
  write(0,*)
end subroutine iotk_dat_dummy_REAL3_5


# 45 "iotk_dat.spp"

# 48 "iotk_dat.spp"
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
# 65 "iotk_dat.spp"


!------------------------------------------------------------------------------!
! Inclusion of configuration file
#include "iotk_config.h"
!------------------------------------------------------------------------------!

# 74 "iotk_dat.spp"
#include "iotk_auxmacros.h"
# 76 "iotk_dat.spp"

# 78 "iotk_dat.spp"

#ifdef __IOTK_REAL3
#if 6 <= __IOTK_MAXRANK
# 82 "iotk_dat.spp"
subroutine iotk_write_dat_REAL3_6(unit,name,dat,dummy,attr,columns,sep,fmt,ierr)
  use iotk_base
  use iotk_error_interf
  use iotk_attr_interf, only : iotk_write_attr
  use iotk_write_interf
  use iotk_fmt_interf
  use iotk_str_interf
  use iotk_unit_interf
  use iotk_misc_interf
  implicit none
  integer, parameter :: this_kind = iotk_REAL3
  integer,                                    intent(in)  :: unit
  character(len=*),                           intent(in)  :: name
  REAL (kind=this_kind),           intent(in)  :: dat (:,:,:,:,:,:) 
  type(iotk_dummytype),             optional              :: dummy
  character(len=*),                 optional, intent(in)  :: attr
  integer,                          optional, intent(in)  :: columns
  character(len=*),                 optional, intent(in)  :: sep
  character(len=*),                 optional, intent(in)  :: fmt
  integer,                          optional, intent(out) :: ierr
  integer :: ierrl,lunit,iostat
  logical :: binary,raw,stream
  integer :: lcolumns
  integer(iotk_header_kind), parameter :: idummy=0
  character(100) :: lsep
  character(300) :: usefmt
  character(iotk_attlenx) :: lattr
  character(iotk_attlenx) :: attr_tmp
  type (iotk_unit), pointer :: this
# 115 "iotk_dat.spp"
  REAL (kind=this_kind),allocatable :: dattmp(:)
# 117 "iotk_dat.spp"
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
# 126 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
      goto 1
    end if
  end if
  lunit = iotk_phys_unit(unit)
  call iotk_unit_get(lunit,pointer=this)
  raw = .false.
  if(associated(this)) then
    raw = this%raw
  end if
  call iotk_inquire(lunit,binary,stream,ierrl)
  if(ierrl/=0) then
    call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 138 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
    goto 1
  end if
  call iotk_strcpy(usefmt,"!",ierrl)
  if(ierrl/=0) then
    call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 143 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
    goto 1
  end if
  if(present(fmt) .and. .not. raw) call iotk_strcpy(usefmt,iotk_strtrim(fmt),ierrl)
  if(ierrl/=0) then
    call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 148 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
    goto 1
  end if
  if(iotk_strscan(usefmt,"<>&")/=0) then
    call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 152 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
# 152 "iotk_dat.spp"
call iotk_error_msg(ierrl,'Special characters (<>&) found in fmt string')
# 152 "iotk_dat.spp"
call iotk_error_write(ierrl,"unit",unit)
# 152 "iotk_dat.spp"
call iotk_error_write(ierrl,"name",trim(name))
# 152 "iotk_dat.spp"
call iotk_error_write(ierrl,"fmt",trim(fmt))
    goto 1
  end if
  call iotk_write_attr(lattr,"type",iotk_tolower("REAL"),first=.true.,ierr=ierrl)
  if(ierrl/=0) then
    call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 157 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
    goto 1
  end if
  call iotk_write_attr(lattr,"size",size(dat),ierr=ierrl)
  if(ierrl/=0) then
    call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 162 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
    goto 1
  end if
# 172 "iotk_dat.spp"
  if(binary) then
    call iotk_write_attr(lattr,"kind",kind(dat),ierr=ierrl)
    if(ierrl/=0) then
      call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 175 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
      goto 1
    end if
  end if
# 180 "iotk_dat.spp"
  if(.not.iotk_strcomp(usefmt,"!")) call iotk_write_attr(lattr,"fmt",iotk_strtrim(usefmt),ierr=ierrl)
  if(ierrl/=0) then
    call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 182 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
    goto 1
  end if
  if(lcolumns/=1) call iotk_write_attr(lattr,"columns",lcolumns,ierr=ierrl)
  if(ierrl/=0) then
    call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 187 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
    goto 1
  end if
  if(present(attr)) then
    attr_tmp(1:1)=iotk_eos
    call iotk_strcpy(attr_tmp,attr,ierr=ierrl)
    if(ierrl/=0) then
      call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 194 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
      goto 1
    end if
    call iotk_delete_attr(attr_tmp,"type",ierrl)
    if(ierrl/=0) then
      call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 199 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
      goto 1
    end if
    call iotk_delete_attr(attr_tmp,"kind",ierrl)
    if(ierrl/=0) then
      call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 204 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
      goto 1
    end if
    call iotk_delete_attr(attr_tmp,"size",ierrl)
    if(ierrl/=0) then
      call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 209 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
      goto 1
    end if
    call iotk_delete_attr(attr_tmp,"fmt",ierrl)
    if(ierrl/=0) then
      call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 214 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
      goto 1
    end if
    call iotk_delete_attr(attr_tmp,"columns",ierrl)
    if(ierrl/=0) then
      call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 219 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
      goto 1
    end if
    call iotk_delete_attr(attr_tmp,"len",ierrl)
    if(ierrl/=0) then
      call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 224 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
      goto 1
    end if
    if(iotk_strlen_trim(attr_tmp)>0) call iotk_strcat(lattr,iotk_strtrim(attr_tmp),ierr=ierrl)
  end if
  if(ierrl/=0) then
    call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 230 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
    goto 1
  end if
  call iotk_write_begin(unit,name,lattr,ierr=ierrl)
  if(ierrl/=0) then
    call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 235 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
    goto 1
  end if

  allocate(dattmp(size(dat)))
# 243 "iotk_dat.spp"
#if defined(__IOTK_WORKAROUND3) || defined(__IOTK_WORKAROUND4)
# 247 "iotk_dat.spp"
     call iotk_private_pack_REAL3(dattmp,dat,size(dattmp),1)
# 249 "iotk_dat.spp"
#else
     dattmp = pack(dat,mask=.true.)
#endif
# 253 "iotk_dat.spp"

  if(binary) then
    if(raw) then
      write(lunit,iostat=iostat) (dattmp(itmp),itmp=1,size(dattmp))
      if(iostat/=0) then
        call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 258 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
        goto 1
      end if
    else
      write(lunit,iostat=iostat) idummy,(dattmp(itmp),itmp=1,size(dattmp))
      if(iostat/=0) then
        call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 264 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
        goto 1
      end if
    end if
  else
    if(raw) then
# 273 "iotk_dat.spp"
      write(lunit,*,iostat=iostat) (dattmp(itmp),itmp=1,size(dattmp))
# 275 "iotk_dat.spp"
      if(iostat/=0) then
        call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 276 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
        goto 1
      end if
    else if(iotk_strcomp(usefmt,"*")) then
      write(lunit,*,iostat=iostat) (dattmp(itmp),itmp=1,size(dattmp))
      if(iostat/=0) then
        call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 282 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
        goto 1
      end if
    else if(iotk_strcomp(usefmt,"!")) then
# 296 "iotk_dat.spp"
     write(lunit,fmt=trim(iotk_wfmt("REAL",kind(dattmp),lcolumns,-1,lsep)),iostat=iostat) (dattmp(itmp),itmp=1,size(dattmp))
     if(iostat/=0) then
      call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 298 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
      goto 1
     end if
# 302 "iotk_dat.spp"
    else
      write(lunit,fmt=usefmt(1:iotk_strlen(usefmt)),iostat=iostat) (dattmp(itmp),itmp=1,size(dattmp))
      if(iostat/=0) then
        call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 305 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
        goto 1
      end if
    end if
  end if
  call iotk_write_end(unit,name,ierr=ierrl)
  if(ierrl/=0) then
    call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 312 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
    goto 1
  end if
1 continue
  if(allocated(dattmp)) deallocate(dattmp)
  if(present(ierr)) then
    ierr = ierrl
  else
    if(ierrl/=0) call iotk_error_handler(ierrl)
  end if
end subroutine iotk_write_dat_REAL3_6


# 668 "iotk_dat.spp"

# 670 "iotk_dat.spp"
subroutine iotk_scan_dat_REAL3_6(unit,name,dat,dummy,attr,found,default,ierr)
  use iotk_base
  use iotk_error_interf
  use iotk_dat_interf, only: iotk_scan_dat_aux
  use iotk_scan_interf
  use iotk_str_interf
  use iotk_misc_interf
  implicit none
  integer, parameter :: this_kind = iotk_REAL3
  integer,                                   intent(in)  :: unit
  character(len=*),                          intent(in)  :: name
#ifdef __IOTK_WORKAROUND6
  REAL(kind=this_kind)                        :: dat (:,:,:,:,:,:)
#else
  REAL(kind=this_kind),           intent(out) :: dat (:,:,:,:,:,:)
#endif
  type(iotk_dummytype),            optional              :: dummy
#ifdef __IOTK_WORKAROUND6
  character(len=*),                optional              :: attr
#else
  character(len=*),                optional, intent(out) :: attr
#endif
  logical,                         optional, intent(out) :: found
  REAL(kind=this_kind), optional, intent(in)  :: default (:,:,:,:,:,:)
  integer,                         optional, intent(out) :: ierr
# 698 "iotk_dat.spp"
  REAL (kind=this_kind),              allocatable :: tmpdat(:)
# 700 "iotk_dat.spp"
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
# 717 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
    goto 1
  end if
  call iotk_parse_dat(lattr,rtype,rkind,rsize,rlen,fmt,columns,ierrl)
! Note that columns is not effectively used
  if(ierrl/=0) goto 1
  if(.not. (iotk_strcomp(rtype,iotk_eos) .or. iotk_strcomp(rtype,"REAL") ) ) then
    call iotk_error_issue(ierrl,"iotk_scan_dat",__FILE__,__LINE__)
# 724 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
# 724 "iotk_dat.spp"
call iotk_error_msg(ierrl,' ')
# 724 "iotk_dat.spp"
call iotk_error_write(ierrl,"rtype",rtype(1:iotk_strlen(rtype)))
# 724 "iotk_dat.spp"
call iotk_error_write(ierrl,"type","REAL")
    goto 1
  end if
  if(.not. (rsize==-1 .or. rsize==size(dat)) ) then
    call iotk_error_issue(ierrl,"iotk_scan_dat",__FILE__,__LINE__)
# 728 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
    goto 1
  end if
  if(rkind==-1) rkind = kind(dat)
# 735 "iotk_dat.spp"

  allocate(tmpdat(size(dat)))
  call iotk_scan_dat_aux(unit,tmpdat,rkind,rlen,fmt(1:iotk_strlen(fmt)),ierrl)
  if(ierrl/=0) then
    call iotk_error_issue(ierrl,"iotk_scan_dat",__FILE__,__LINE__)
# 739 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
# 739 "iotk_dat.spp"
call iotk_error_msg(ierrl,'Error reading data')
# 739 "iotk_dat.spp"
call iotk_error_write(ierrl,"name",name)
# 739 "iotk_dat.spp"
call iotk_error_write(ierrl,"rkind",rkind)
# 739 "iotk_dat.spp"
call iotk_error_write(ierrl,"rlen",rlen)
    goto 1
  end if
# 745 "iotk_dat.spp"
#if defined(__IOTK_WORKAROUND3) || defined(__IOTK_WORKAROUND4)
# 749 "iotk_dat.spp"
     call iotk_private_pack_REAL3(dat,tmpdat,size(tmpdat),1)
# 751 "iotk_dat.spp"
#else
     dat = reshape(tmpdat,shape(dat))
#endif
# 755 "iotk_dat.spp"
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
# 767 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
# 767 "iotk_dat.spp"
call iotk_error_msg(ierrl,'Dat not found')
# 767 "iotk_dat.spp"
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
end subroutine iotk_scan_dat_REAL3_6


#endif
#endif

subroutine iotk_dat_dummy_REAL3_6
  write(0,*)
end subroutine iotk_dat_dummy_REAL3_6


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

#ifdef __IOTK_REAL3
#if 7 <= __IOTK_MAXRANK
# 82 "iotk_dat.spp"
subroutine iotk_write_dat_REAL3_7(unit,name,dat,dummy,attr,columns,sep,fmt,ierr)
  use iotk_base
  use iotk_error_interf
  use iotk_attr_interf, only : iotk_write_attr
  use iotk_write_interf
  use iotk_fmt_interf
  use iotk_str_interf
  use iotk_unit_interf
  use iotk_misc_interf
  implicit none
  integer, parameter :: this_kind = iotk_REAL3
  integer,                                    intent(in)  :: unit
  character(len=*),                           intent(in)  :: name
  REAL (kind=this_kind),           intent(in)  :: dat (:,:,:,:,:,:,:) 
  type(iotk_dummytype),             optional              :: dummy
  character(len=*),                 optional, intent(in)  :: attr
  integer,                          optional, intent(in)  :: columns
  character(len=*),                 optional, intent(in)  :: sep
  character(len=*),                 optional, intent(in)  :: fmt
  integer,                          optional, intent(out) :: ierr
  integer :: ierrl,lunit,iostat
  logical :: binary,raw,stream
  integer :: lcolumns
  integer(iotk_header_kind), parameter :: idummy=0
  character(100) :: lsep
  character(300) :: usefmt
  character(iotk_attlenx) :: lattr
  character(iotk_attlenx) :: attr_tmp
  type (iotk_unit), pointer :: this
# 115 "iotk_dat.spp"
  REAL (kind=this_kind),allocatable :: dattmp(:)
# 117 "iotk_dat.spp"
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
# 126 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
      goto 1
    end if
  end if
  lunit = iotk_phys_unit(unit)
  call iotk_unit_get(lunit,pointer=this)
  raw = .false.
  if(associated(this)) then
    raw = this%raw
  end if
  call iotk_inquire(lunit,binary,stream,ierrl)
  if(ierrl/=0) then
    call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 138 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
    goto 1
  end if
  call iotk_strcpy(usefmt,"!",ierrl)
  if(ierrl/=0) then
    call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 143 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
    goto 1
  end if
  if(present(fmt) .and. .not. raw) call iotk_strcpy(usefmt,iotk_strtrim(fmt),ierrl)
  if(ierrl/=0) then
    call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 148 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
    goto 1
  end if
  if(iotk_strscan(usefmt,"<>&")/=0) then
    call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 152 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
# 152 "iotk_dat.spp"
call iotk_error_msg(ierrl,'Special characters (<>&) found in fmt string')
# 152 "iotk_dat.spp"
call iotk_error_write(ierrl,"unit",unit)
# 152 "iotk_dat.spp"
call iotk_error_write(ierrl,"name",trim(name))
# 152 "iotk_dat.spp"
call iotk_error_write(ierrl,"fmt",trim(fmt))
    goto 1
  end if
  call iotk_write_attr(lattr,"type",iotk_tolower("REAL"),first=.true.,ierr=ierrl)
  if(ierrl/=0) then
    call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 157 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
    goto 1
  end if
  call iotk_write_attr(lattr,"size",size(dat),ierr=ierrl)
  if(ierrl/=0) then
    call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 162 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
    goto 1
  end if
# 172 "iotk_dat.spp"
  if(binary) then
    call iotk_write_attr(lattr,"kind",kind(dat),ierr=ierrl)
    if(ierrl/=0) then
      call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 175 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
      goto 1
    end if
  end if
# 180 "iotk_dat.spp"
  if(.not.iotk_strcomp(usefmt,"!")) call iotk_write_attr(lattr,"fmt",iotk_strtrim(usefmt),ierr=ierrl)
  if(ierrl/=0) then
    call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 182 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
    goto 1
  end if
  if(lcolumns/=1) call iotk_write_attr(lattr,"columns",lcolumns,ierr=ierrl)
  if(ierrl/=0) then
    call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 187 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
    goto 1
  end if
  if(present(attr)) then
    attr_tmp(1:1)=iotk_eos
    call iotk_strcpy(attr_tmp,attr,ierr=ierrl)
    if(ierrl/=0) then
      call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 194 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
      goto 1
    end if
    call iotk_delete_attr(attr_tmp,"type",ierrl)
    if(ierrl/=0) then
      call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 199 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
      goto 1
    end if
    call iotk_delete_attr(attr_tmp,"kind",ierrl)
    if(ierrl/=0) then
      call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 204 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
      goto 1
    end if
    call iotk_delete_attr(attr_tmp,"size",ierrl)
    if(ierrl/=0) then
      call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 209 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
      goto 1
    end if
    call iotk_delete_attr(attr_tmp,"fmt",ierrl)
    if(ierrl/=0) then
      call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 214 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
      goto 1
    end if
    call iotk_delete_attr(attr_tmp,"columns",ierrl)
    if(ierrl/=0) then
      call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 219 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
      goto 1
    end if
    call iotk_delete_attr(attr_tmp,"len",ierrl)
    if(ierrl/=0) then
      call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 224 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
      goto 1
    end if
    if(iotk_strlen_trim(attr_tmp)>0) call iotk_strcat(lattr,iotk_strtrim(attr_tmp),ierr=ierrl)
  end if
  if(ierrl/=0) then
    call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 230 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
    goto 1
  end if
  call iotk_write_begin(unit,name,lattr,ierr=ierrl)
  if(ierrl/=0) then
    call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 235 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
    goto 1
  end if

  allocate(dattmp(size(dat)))
# 243 "iotk_dat.spp"
#if defined(__IOTK_WORKAROUND3) || defined(__IOTK_WORKAROUND4)
# 247 "iotk_dat.spp"
     call iotk_private_pack_REAL3(dattmp,dat,size(dattmp),1)
# 249 "iotk_dat.spp"
#else
     dattmp = pack(dat,mask=.true.)
#endif
# 253 "iotk_dat.spp"

  if(binary) then
    if(raw) then
      write(lunit,iostat=iostat) (dattmp(itmp),itmp=1,size(dattmp))
      if(iostat/=0) then
        call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 258 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
        goto 1
      end if
    else
      write(lunit,iostat=iostat) idummy,(dattmp(itmp),itmp=1,size(dattmp))
      if(iostat/=0) then
        call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 264 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
        goto 1
      end if
    end if
  else
    if(raw) then
# 273 "iotk_dat.spp"
      write(lunit,*,iostat=iostat) (dattmp(itmp),itmp=1,size(dattmp))
# 275 "iotk_dat.spp"
      if(iostat/=0) then
        call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 276 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
        goto 1
      end if
    else if(iotk_strcomp(usefmt,"*")) then
      write(lunit,*,iostat=iostat) (dattmp(itmp),itmp=1,size(dattmp))
      if(iostat/=0) then
        call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 282 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
        goto 1
      end if
    else if(iotk_strcomp(usefmt,"!")) then
# 296 "iotk_dat.spp"
     write(lunit,fmt=trim(iotk_wfmt("REAL",kind(dattmp),lcolumns,-1,lsep)),iostat=iostat) (dattmp(itmp),itmp=1,size(dattmp))
     if(iostat/=0) then
      call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 298 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
      goto 1
     end if
# 302 "iotk_dat.spp"
    else
      write(lunit,fmt=usefmt(1:iotk_strlen(usefmt)),iostat=iostat) (dattmp(itmp),itmp=1,size(dattmp))
      if(iostat/=0) then
        call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 305 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
        goto 1
      end if
    end if
  end if
  call iotk_write_end(unit,name,ierr=ierrl)
  if(ierrl/=0) then
    call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 312 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
    goto 1
  end if
1 continue
  if(allocated(dattmp)) deallocate(dattmp)
  if(present(ierr)) then
    ierr = ierrl
  else
    if(ierrl/=0) call iotk_error_handler(ierrl)
  end if
end subroutine iotk_write_dat_REAL3_7


# 668 "iotk_dat.spp"

# 670 "iotk_dat.spp"
subroutine iotk_scan_dat_REAL3_7(unit,name,dat,dummy,attr,found,default,ierr)
  use iotk_base
  use iotk_error_interf
  use iotk_dat_interf, only: iotk_scan_dat_aux
  use iotk_scan_interf
  use iotk_str_interf
  use iotk_misc_interf
  implicit none
  integer, parameter :: this_kind = iotk_REAL3
  integer,                                   intent(in)  :: unit
  character(len=*),                          intent(in)  :: name
#ifdef __IOTK_WORKAROUND6
  REAL(kind=this_kind)                        :: dat (:,:,:,:,:,:,:)
#else
  REAL(kind=this_kind),           intent(out) :: dat (:,:,:,:,:,:,:)
#endif
  type(iotk_dummytype),            optional              :: dummy
#ifdef __IOTK_WORKAROUND6
  character(len=*),                optional              :: attr
#else
  character(len=*),                optional, intent(out) :: attr
#endif
  logical,                         optional, intent(out) :: found
  REAL(kind=this_kind), optional, intent(in)  :: default (:,:,:,:,:,:,:)
  integer,                         optional, intent(out) :: ierr
# 698 "iotk_dat.spp"
  REAL (kind=this_kind),              allocatable :: tmpdat(:)
# 700 "iotk_dat.spp"
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
# 717 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
    goto 1
  end if
  call iotk_parse_dat(lattr,rtype,rkind,rsize,rlen,fmt,columns,ierrl)
! Note that columns is not effectively used
  if(ierrl/=0) goto 1
  if(.not. (iotk_strcomp(rtype,iotk_eos) .or. iotk_strcomp(rtype,"REAL") ) ) then
    call iotk_error_issue(ierrl,"iotk_scan_dat",__FILE__,__LINE__)
# 724 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
# 724 "iotk_dat.spp"
call iotk_error_msg(ierrl,' ')
# 724 "iotk_dat.spp"
call iotk_error_write(ierrl,"rtype",rtype(1:iotk_strlen(rtype)))
# 724 "iotk_dat.spp"
call iotk_error_write(ierrl,"type","REAL")
    goto 1
  end if
  if(.not. (rsize==-1 .or. rsize==size(dat)) ) then
    call iotk_error_issue(ierrl,"iotk_scan_dat",__FILE__,__LINE__)
# 728 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
    goto 1
  end if
  if(rkind==-1) rkind = kind(dat)
# 735 "iotk_dat.spp"

  allocate(tmpdat(size(dat)))
  call iotk_scan_dat_aux(unit,tmpdat,rkind,rlen,fmt(1:iotk_strlen(fmt)),ierrl)
  if(ierrl/=0) then
    call iotk_error_issue(ierrl,"iotk_scan_dat",__FILE__,__LINE__)
# 739 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
# 739 "iotk_dat.spp"
call iotk_error_msg(ierrl,'Error reading data')
# 739 "iotk_dat.spp"
call iotk_error_write(ierrl,"name",name)
# 739 "iotk_dat.spp"
call iotk_error_write(ierrl,"rkind",rkind)
# 739 "iotk_dat.spp"
call iotk_error_write(ierrl,"rlen",rlen)
    goto 1
  end if
# 745 "iotk_dat.spp"
#if defined(__IOTK_WORKAROUND3) || defined(__IOTK_WORKAROUND4)
# 749 "iotk_dat.spp"
     call iotk_private_pack_REAL3(dat,tmpdat,size(tmpdat),1)
# 751 "iotk_dat.spp"
#else
     dat = reshape(tmpdat,shape(dat))
#endif
# 755 "iotk_dat.spp"
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
# 767 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
# 767 "iotk_dat.spp"
call iotk_error_msg(ierrl,'Dat not found')
# 767 "iotk_dat.spp"
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
end subroutine iotk_scan_dat_REAL3_7


#endif
#endif

subroutine iotk_dat_dummy_REAL3_7
  write(0,*)
end subroutine iotk_dat_dummy_REAL3_7


# 45 "iotk_dat.spp"

# 48 "iotk_dat.spp"
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
# 65 "iotk_dat.spp"


!------------------------------------------------------------------------------!
! Inclusion of configuration file
#include "iotk_config.h"
!------------------------------------------------------------------------------!

# 74 "iotk_dat.spp"
#include "iotk_auxmacros.h"
# 76 "iotk_dat.spp"

# 78 "iotk_dat.spp"

#ifdef __IOTK_REAL4
#if 0 <= __IOTK_MAXRANK
# 82 "iotk_dat.spp"
subroutine iotk_write_dat_REAL4_0(unit,name,dat,dummy,attr,columns,sep,fmt,ierr)
  use iotk_base
  use iotk_error_interf
  use iotk_attr_interf, only : iotk_write_attr
  use iotk_write_interf
  use iotk_fmt_interf
  use iotk_str_interf
  use iotk_unit_interf
  use iotk_misc_interf
  implicit none
  integer, parameter :: this_kind = iotk_REAL4
  integer,                                    intent(in)  :: unit
  character(len=*),                           intent(in)  :: name
  REAL (kind=this_kind),           intent(in)  :: dat  
  type(iotk_dummytype),             optional              :: dummy
  character(len=*),                 optional, intent(in)  :: attr
  integer,                          optional, intent(in)  :: columns
  character(len=*),                 optional, intent(in)  :: sep
  character(len=*),                 optional, intent(in)  :: fmt
  integer,                          optional, intent(out) :: ierr
  integer :: ierrl,lunit,iostat
  logical :: binary,raw,stream
  integer :: lcolumns
  integer(iotk_header_kind), parameter :: idummy=0
  character(100) :: lsep
  character(300) :: usefmt
  character(iotk_attlenx) :: lattr
  character(iotk_attlenx) :: attr_tmp
  type (iotk_unit), pointer :: this
# 115 "iotk_dat.spp"
  REAL (kind=this_kind),allocatable :: dattmp(:)
# 117 "iotk_dat.spp"
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
# 126 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
      goto 1
    end if
  end if
  lunit = iotk_phys_unit(unit)
  call iotk_unit_get(lunit,pointer=this)
  raw = .false.
  if(associated(this)) then
    raw = this%raw
  end if
  call iotk_inquire(lunit,binary,stream,ierrl)
  if(ierrl/=0) then
    call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 138 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
    goto 1
  end if
  call iotk_strcpy(usefmt,"!",ierrl)
  if(ierrl/=0) then
    call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 143 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
    goto 1
  end if
  if(present(fmt) .and. .not. raw) call iotk_strcpy(usefmt,iotk_strtrim(fmt),ierrl)
  if(ierrl/=0) then
    call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 148 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
    goto 1
  end if
  if(iotk_strscan(usefmt,"<>&")/=0) then
    call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 152 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
# 152 "iotk_dat.spp"
call iotk_error_msg(ierrl,'Special characters (<>&) found in fmt string')
# 152 "iotk_dat.spp"
call iotk_error_write(ierrl,"unit",unit)
# 152 "iotk_dat.spp"
call iotk_error_write(ierrl,"name",trim(name))
# 152 "iotk_dat.spp"
call iotk_error_write(ierrl,"fmt",trim(fmt))
    goto 1
  end if
  call iotk_write_attr(lattr,"type",iotk_tolower("REAL"),first=.true.,ierr=ierrl)
  if(ierrl/=0) then
    call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 157 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
    goto 1
  end if
  call iotk_write_attr(lattr,"size",1,ierr=ierrl)
  if(ierrl/=0) then
    call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 162 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
    goto 1
  end if
# 172 "iotk_dat.spp"
  if(binary) then
    call iotk_write_attr(lattr,"kind",kind(dat),ierr=ierrl)
    if(ierrl/=0) then
      call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 175 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
      goto 1
    end if
  end if
# 180 "iotk_dat.spp"
  if(.not.iotk_strcomp(usefmt,"!")) call iotk_write_attr(lattr,"fmt",iotk_strtrim(usefmt),ierr=ierrl)
  if(ierrl/=0) then
    call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 182 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
    goto 1
  end if
  if(lcolumns/=1) call iotk_write_attr(lattr,"columns",lcolumns,ierr=ierrl)
  if(ierrl/=0) then
    call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 187 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
    goto 1
  end if
  if(present(attr)) then
    attr_tmp(1:1)=iotk_eos
    call iotk_strcpy(attr_tmp,attr,ierr=ierrl)
    if(ierrl/=0) then
      call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 194 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
      goto 1
    end if
    call iotk_delete_attr(attr_tmp,"type",ierrl)
    if(ierrl/=0) then
      call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 199 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
      goto 1
    end if
    call iotk_delete_attr(attr_tmp,"kind",ierrl)
    if(ierrl/=0) then
      call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 204 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
      goto 1
    end if
    call iotk_delete_attr(attr_tmp,"size",ierrl)
    if(ierrl/=0) then
      call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 209 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
      goto 1
    end if
    call iotk_delete_attr(attr_tmp,"fmt",ierrl)
    if(ierrl/=0) then
      call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 214 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
      goto 1
    end if
    call iotk_delete_attr(attr_tmp,"columns",ierrl)
    if(ierrl/=0) then
      call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 219 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
      goto 1
    end if
    call iotk_delete_attr(attr_tmp,"len",ierrl)
    if(ierrl/=0) then
      call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 224 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
      goto 1
    end if
    if(iotk_strlen_trim(attr_tmp)>0) call iotk_strcat(lattr,iotk_strtrim(attr_tmp),ierr=ierrl)
  end if
  if(ierrl/=0) then
    call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 230 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
    goto 1
  end if
  call iotk_write_begin(unit,name,lattr,ierr=ierrl)
  if(ierrl/=0) then
    call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 235 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
    goto 1
  end if

  allocate(dattmp(1))
# 241 "iotk_dat.spp"
     dattmp(1) = dat
# 253 "iotk_dat.spp"

  if(binary) then
    if(raw) then
      write(lunit,iostat=iostat) (dattmp(itmp),itmp=1,size(dattmp))
      if(iostat/=0) then
        call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 258 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
        goto 1
      end if
    else
      write(lunit,iostat=iostat) idummy,(dattmp(itmp),itmp=1,size(dattmp))
      if(iostat/=0) then
        call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 264 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
        goto 1
      end if
    end if
  else
    if(raw) then
# 273 "iotk_dat.spp"
      write(lunit,*,iostat=iostat) (dattmp(itmp),itmp=1,size(dattmp))
# 275 "iotk_dat.spp"
      if(iostat/=0) then
        call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 276 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
        goto 1
      end if
    else if(iotk_strcomp(usefmt,"*")) then
      write(lunit,*,iostat=iostat) (dattmp(itmp),itmp=1,size(dattmp))
      if(iostat/=0) then
        call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 282 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
        goto 1
      end if
    else if(iotk_strcomp(usefmt,"!")) then
# 296 "iotk_dat.spp"
     write(lunit,fmt=trim(iotk_wfmt("REAL",kind(dattmp),lcolumns,-1,lsep)),iostat=iostat) (dattmp(itmp),itmp=1,size(dattmp))
     if(iostat/=0) then
      call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 298 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
      goto 1
     end if
# 302 "iotk_dat.spp"
    else
      write(lunit,fmt=usefmt(1:iotk_strlen(usefmt)),iostat=iostat) (dattmp(itmp),itmp=1,size(dattmp))
      if(iostat/=0) then
        call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 305 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
        goto 1
      end if
    end if
  end if
  call iotk_write_end(unit,name,ierr=ierrl)
  if(ierrl/=0) then
    call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 312 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
    goto 1
  end if
1 continue
  if(allocated(dattmp)) deallocate(dattmp)
  if(present(ierr)) then
    ierr = ierrl
  else
    if(ierrl/=0) call iotk_error_handler(ierrl)
  end if
end subroutine iotk_write_dat_REAL4_0


# 668 "iotk_dat.spp"

# 670 "iotk_dat.spp"
subroutine iotk_scan_dat_REAL4_0(unit,name,dat,dummy,attr,found,default,ierr)
  use iotk_base
  use iotk_error_interf
  use iotk_dat_interf, only: iotk_scan_dat_aux
  use iotk_scan_interf
  use iotk_str_interf
  use iotk_misc_interf
  implicit none
  integer, parameter :: this_kind = iotk_REAL4
  integer,                                   intent(in)  :: unit
  character(len=*),                          intent(in)  :: name
#ifdef __IOTK_WORKAROUND6
  REAL(kind=this_kind)                        :: dat 
#else
  REAL(kind=this_kind),           intent(out) :: dat 
#endif
  type(iotk_dummytype),            optional              :: dummy
#ifdef __IOTK_WORKAROUND6
  character(len=*),                optional              :: attr
#else
  character(len=*),                optional, intent(out) :: attr
#endif
  logical,                         optional, intent(out) :: found
  REAL(kind=this_kind), optional, intent(in)  :: default 
  integer,                         optional, intent(out) :: ierr
# 698 "iotk_dat.spp"
  REAL (kind=this_kind),              allocatable :: tmpdat(:)
# 700 "iotk_dat.spp"
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
# 717 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
    goto 1
  end if
  call iotk_parse_dat(lattr,rtype,rkind,rsize,rlen,fmt,columns,ierrl)
! Note that columns is not effectively used
  if(ierrl/=0) goto 1
  if(.not. (iotk_strcomp(rtype,iotk_eos) .or. iotk_strcomp(rtype,"REAL") ) ) then
    call iotk_error_issue(ierrl,"iotk_scan_dat",__FILE__,__LINE__)
# 724 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
# 724 "iotk_dat.spp"
call iotk_error_msg(ierrl,' ')
# 724 "iotk_dat.spp"
call iotk_error_write(ierrl,"rtype",rtype(1:iotk_strlen(rtype)))
# 724 "iotk_dat.spp"
call iotk_error_write(ierrl,"type","REAL")
    goto 1
  end if
  if(.not. (rsize==-1 .or. rsize==1) ) then
    call iotk_error_issue(ierrl,"iotk_scan_dat",__FILE__,__LINE__)
# 728 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
    goto 1
  end if
  if(rkind==-1) rkind = kind(dat)
# 735 "iotk_dat.spp"

  allocate(tmpdat(1))
  call iotk_scan_dat_aux(unit,tmpdat,rkind,rlen,fmt(1:iotk_strlen(fmt)),ierrl)
  if(ierrl/=0) then
    call iotk_error_issue(ierrl,"iotk_scan_dat",__FILE__,__LINE__)
# 739 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
# 739 "iotk_dat.spp"
call iotk_error_msg(ierrl,'Error reading data')
# 739 "iotk_dat.spp"
call iotk_error_write(ierrl,"name",name)
# 739 "iotk_dat.spp"
call iotk_error_write(ierrl,"rkind",rkind)
# 739 "iotk_dat.spp"
call iotk_error_write(ierrl,"rlen",rlen)
    goto 1
  end if
# 743 "iotk_dat.spp"
     dat = tmpdat(1)
# 755 "iotk_dat.spp"
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
# 767 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
# 767 "iotk_dat.spp"
call iotk_error_msg(ierrl,'Dat not found')
# 767 "iotk_dat.spp"
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
end subroutine iotk_scan_dat_REAL4_0


#endif
#endif

subroutine iotk_dat_dummy_REAL4_0
  write(0,*)
end subroutine iotk_dat_dummy_REAL4_0


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

#ifdef __IOTK_REAL4
#if 1 <= __IOTK_MAXRANK
# 82 "iotk_dat.spp"
subroutine iotk_write_dat_REAL4_1(unit,name,dat,dummy,attr,columns,sep,fmt,ierr)
  use iotk_base
  use iotk_error_interf
  use iotk_attr_interf, only : iotk_write_attr
  use iotk_write_interf
  use iotk_fmt_interf
  use iotk_str_interf
  use iotk_unit_interf
  use iotk_misc_interf
  implicit none
  integer, parameter :: this_kind = iotk_REAL4
  integer,                                    intent(in)  :: unit
  character(len=*),                           intent(in)  :: name
  REAL (kind=this_kind),           intent(in)  :: dat (:) 
  type(iotk_dummytype),             optional              :: dummy
  character(len=*),                 optional, intent(in)  :: attr
  integer,                          optional, intent(in)  :: columns
  character(len=*),                 optional, intent(in)  :: sep
  character(len=*),                 optional, intent(in)  :: fmt
  integer,                          optional, intent(out) :: ierr
  integer :: ierrl,lunit,iostat
  logical :: binary,raw,stream
  integer :: lcolumns
  integer(iotk_header_kind), parameter :: idummy=0
  character(100) :: lsep
  character(300) :: usefmt
  character(iotk_attlenx) :: lattr
  character(iotk_attlenx) :: attr_tmp
  type (iotk_unit), pointer :: this
# 115 "iotk_dat.spp"
  REAL (kind=this_kind),allocatable :: dattmp(:)
# 117 "iotk_dat.spp"
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
# 126 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
      goto 1
    end if
  end if
  lunit = iotk_phys_unit(unit)
  call iotk_unit_get(lunit,pointer=this)
  raw = .false.
  if(associated(this)) then
    raw = this%raw
  end if
  call iotk_inquire(lunit,binary,stream,ierrl)
  if(ierrl/=0) then
    call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 138 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
    goto 1
  end if
  call iotk_strcpy(usefmt,"!",ierrl)
  if(ierrl/=0) then
    call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 143 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
    goto 1
  end if
  if(present(fmt) .and. .not. raw) call iotk_strcpy(usefmt,iotk_strtrim(fmt),ierrl)
  if(ierrl/=0) then
    call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 148 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
    goto 1
  end if
  if(iotk_strscan(usefmt,"<>&")/=0) then
    call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 152 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
# 152 "iotk_dat.spp"
call iotk_error_msg(ierrl,'Special characters (<>&) found in fmt string')
# 152 "iotk_dat.spp"
call iotk_error_write(ierrl,"unit",unit)
# 152 "iotk_dat.spp"
call iotk_error_write(ierrl,"name",trim(name))
# 152 "iotk_dat.spp"
call iotk_error_write(ierrl,"fmt",trim(fmt))
    goto 1
  end if
  call iotk_write_attr(lattr,"type",iotk_tolower("REAL"),first=.true.,ierr=ierrl)
  if(ierrl/=0) then
    call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 157 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
    goto 1
  end if
  call iotk_write_attr(lattr,"size",size(dat),ierr=ierrl)
  if(ierrl/=0) then
    call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 162 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
    goto 1
  end if
# 172 "iotk_dat.spp"
  if(binary) then
    call iotk_write_attr(lattr,"kind",kind(dat),ierr=ierrl)
    if(ierrl/=0) then
      call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 175 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
      goto 1
    end if
  end if
# 180 "iotk_dat.spp"
  if(.not.iotk_strcomp(usefmt,"!")) call iotk_write_attr(lattr,"fmt",iotk_strtrim(usefmt),ierr=ierrl)
  if(ierrl/=0) then
    call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 182 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
    goto 1
  end if
  if(lcolumns/=1) call iotk_write_attr(lattr,"columns",lcolumns,ierr=ierrl)
  if(ierrl/=0) then
    call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 187 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
    goto 1
  end if
  if(present(attr)) then
    attr_tmp(1:1)=iotk_eos
    call iotk_strcpy(attr_tmp,attr,ierr=ierrl)
    if(ierrl/=0) then
      call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 194 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
      goto 1
    end if
    call iotk_delete_attr(attr_tmp,"type",ierrl)
    if(ierrl/=0) then
      call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 199 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
      goto 1
    end if
    call iotk_delete_attr(attr_tmp,"kind",ierrl)
    if(ierrl/=0) then
      call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 204 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
      goto 1
    end if
    call iotk_delete_attr(attr_tmp,"size",ierrl)
    if(ierrl/=0) then
      call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 209 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
      goto 1
    end if
    call iotk_delete_attr(attr_tmp,"fmt",ierrl)
    if(ierrl/=0) then
      call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 214 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
      goto 1
    end if
    call iotk_delete_attr(attr_tmp,"columns",ierrl)
    if(ierrl/=0) then
      call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 219 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
      goto 1
    end if
    call iotk_delete_attr(attr_tmp,"len",ierrl)
    if(ierrl/=0) then
      call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 224 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
      goto 1
    end if
    if(iotk_strlen_trim(attr_tmp)>0) call iotk_strcat(lattr,iotk_strtrim(attr_tmp),ierr=ierrl)
  end if
  if(ierrl/=0) then
    call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 230 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
    goto 1
  end if
  call iotk_write_begin(unit,name,lattr,ierr=ierrl)
  if(ierrl/=0) then
    call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 235 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
    goto 1
  end if

  allocate(dattmp(size(dat)))
# 243 "iotk_dat.spp"
#if defined(__IOTK_WORKAROUND3) || defined(__IOTK_WORKAROUND4)
# 247 "iotk_dat.spp"
     call iotk_private_pack_REAL4(dattmp,dat,size(dattmp),1)
# 249 "iotk_dat.spp"
#else
     dattmp = pack(dat,mask=.true.)
#endif
# 253 "iotk_dat.spp"

  if(binary) then
    if(raw) then
      write(lunit,iostat=iostat) (dattmp(itmp),itmp=1,size(dattmp))
      if(iostat/=0) then
        call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 258 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
        goto 1
      end if
    else
      write(lunit,iostat=iostat) idummy,(dattmp(itmp),itmp=1,size(dattmp))
      if(iostat/=0) then
        call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 264 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
        goto 1
      end if
    end if
  else
    if(raw) then
# 273 "iotk_dat.spp"
      write(lunit,*,iostat=iostat) (dattmp(itmp),itmp=1,size(dattmp))
# 275 "iotk_dat.spp"
      if(iostat/=0) then
        call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 276 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
        goto 1
      end if
    else if(iotk_strcomp(usefmt,"*")) then
      write(lunit,*,iostat=iostat) (dattmp(itmp),itmp=1,size(dattmp))
      if(iostat/=0) then
        call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 282 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
        goto 1
      end if
    else if(iotk_strcomp(usefmt,"!")) then
# 296 "iotk_dat.spp"
     write(lunit,fmt=trim(iotk_wfmt("REAL",kind(dattmp),lcolumns,-1,lsep)),iostat=iostat) (dattmp(itmp),itmp=1,size(dattmp))
     if(iostat/=0) then
      call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 298 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
      goto 1
     end if
# 302 "iotk_dat.spp"
    else
      write(lunit,fmt=usefmt(1:iotk_strlen(usefmt)),iostat=iostat) (dattmp(itmp),itmp=1,size(dattmp))
      if(iostat/=0) then
        call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 305 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
        goto 1
      end if
    end if
  end if
  call iotk_write_end(unit,name,ierr=ierrl)
  if(ierrl/=0) then
    call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 312 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
    goto 1
  end if
1 continue
  if(allocated(dattmp)) deallocate(dattmp)
  if(present(ierr)) then
    ierr = ierrl
  else
    if(ierrl/=0) call iotk_error_handler(ierrl)
  end if
end subroutine iotk_write_dat_REAL4_1


# 327 "iotk_dat.spp"
recursive subroutine iotk_scan_dat_aux_REAL4(unit,dat,rkind,rlen,fmt,ierr)
  use iotk_base
  use iotk_error_interf
  use iotk_attr_interf, only: iotk_read
  use iotk_scan_interf
  use iotk_str_interf
  use iotk_unit_interf
  use iotk_misc_interf
  use iotk_stream_interf
  implicit none
  integer, parameter :: this_kind = iotk_REAL4
  integer,                         intent(in)  :: unit
#ifdef __IOTK_WORKAROUND6
  REAL(kind=this_kind)              :: dat (:)
#else
  REAL(kind=this_kind), intent(out) :: dat (:)
#endif
  integer,                         intent(in)  :: rkind
  integer,                         intent(in)  :: rlen
  character(len=*),                intent(in)  :: fmt
  integer,                         intent(out) :: ierr
  integer(iotk_header_kind) :: idummy
  logical :: raw,binary,stream
  integer :: lunit
# 352 "iotk_dat.spp"
  integer :: i
# 354 "iotk_dat.spp"
#ifdef __IOTK_WORKAROUND3
  integer :: j
#endif
  integer :: index,length,nexttag,iostat,altlength
  type(iotk_unit), pointer :: this
  character(len=iotk_linlenx) :: line,altline
# 365 "iotk_dat.spp"
#ifdef __IOTK_REAL1
  REAL (kind=iotk_REAL1), allocatable :: dat1 (:)
#endif
# 365 "iotk_dat.spp"
#ifdef __IOTK_REAL2
  REAL (kind=iotk_REAL2), allocatable :: dat2 (:)
#endif
# 365 "iotk_dat.spp"
#ifdef __IOTK_REAL3
  REAL (kind=iotk_REAL3), allocatable :: dat3 (:)
#endif
# 371 "iotk_dat.spp"
  lunit = iotk_phys_unit(unit)
  ierr = 0
  iostat = 0
  idummy = 0
  call iotk_unit_get(lunit,pointer=this)
  raw = .false.
  if(associated(this)) then
    raw = this%raw
  end if
  call iotk_inquire(lunit,binary,stream,ierr)
  if(ierr/=0) then
    call iotk_error_issue(ierr,"iotk_scan_dat_aux",__FILE__,__LINE__)
# 382 "iotk_dat.spp"
call iotk_error_msg(ierr,"CVS Revision: 1.25 ")
    return
  end if
# 500 "iotk_dat.spp"
  if(binary) then
    select case(rkind)
    case(kind(dat))
      if(raw) then
#ifdef __IOTK_WORKAROUND3
        read(lunit,iostat=iostat) ( dat(j), j=1,ubound(dat,1) )
#else
        read(lunit,iostat=iostat) dat
#endif
        if(iostat/=0) then
          call iotk_error_issue(ierr,"iotk_scan_dat_aux",__FILE__,__LINE__)
# 510 "iotk_dat.spp"
call iotk_error_msg(ierr,"CVS Revision: 1.25 ")
# 510 "iotk_dat.spp"
call iotk_error_msg(ierr,' ')
# 510 "iotk_dat.spp"
call iotk_error_write(ierr,"iostat",iostat)
          return
        end if
      else
        if(stream) then
          call iotk_stream_read(lunit,idummy,dat,ierr=ierr)
          if(ierr/=0) then
            call iotk_error_issue(ierr,"iotk_scan_dat_aux",__FILE__,__LINE__)
# 517 "iotk_dat.spp"
call iotk_error_msg(ierr,"CVS Revision: 1.25 ")
            return
          end if
        else
#ifdef __IOTK_WORKAROUND3
          read(lunit,iostat=iostat) idummy, ( dat(j), j=1,ubound(dat,1) )
#else
          read(lunit,iostat=iostat) idummy, dat
#endif
        end if
        if(iostat/=0) then
          call iotk_error_issue(ierr,"iotk_scan_dat_aux",__FILE__,__LINE__)
# 528 "iotk_dat.spp"
call iotk_error_msg(ierr,"CVS Revision: 1.25 ")
# 528 "iotk_dat.spp"
call iotk_error_msg(ierr,' ')
# 528 "iotk_dat.spp"
call iotk_error_write(ierr,"iostat",iostat)
          return
        end if
      end if
# 534 "iotk_dat.spp"
#ifdef __IOTK_REAL1
    case(kind(dat1))
      ! Giusto per scrupolo. Se e' raw non ci sono info sul kind, quindi questa linea e' irraggiungibile
      if(raw) then
        call iotk_error_issue(ierr,"iotk_scan_dat_aux",__FILE__,__LINE__)
# 538 "iotk_dat.spp"
call iotk_error_msg(ierr,"CVS Revision: 1.25 ")
        return
      end if
      allocate(dat1(ubound(dat,1)))
      if(stream) then
        call iotk_stream_read(lunit,idummy,dat1,ierr=ierr)
        if(ierr/=0) then
          deallocate(dat1)
          call iotk_error_issue(ierr,"iotk_scan_dat_aux",__FILE__,__LINE__)
# 546 "iotk_dat.spp"
call iotk_error_msg(ierr,"CVS Revision: 1.25 ")
# 546 "iotk_dat.spp"
call iotk_error_msg(ierr,' ')
# 546 "iotk_dat.spp"
call iotk_error_write(ierr,"iostat",iostat)
          return
        end if
      else
        read(lunit,iostat=iostat) idummy,( dat1(i), i=1,ubound(dat1,1) )
        if(iostat/=0) then
          deallocate(dat1)
          call iotk_error_issue(ierr,"iotk_scan_dat_aux",__FILE__,__LINE__)
# 553 "iotk_dat.spp"
call iotk_error_msg(ierr,"CVS Revision: 1.25 ")
# 553 "iotk_dat.spp"
call iotk_error_msg(ierr,' ')
# 553 "iotk_dat.spp"
call iotk_error_write(ierr,"iostat",iostat)
          return
        end if
      end if
# 566 "iotk_dat.spp"
      dat = real(dat1,kind=kind(dat))
# 570 "iotk_dat.spp"
      deallocate(dat1)
#endif
# 534 "iotk_dat.spp"
#ifdef __IOTK_REAL2
    case(kind(dat2))
      ! Giusto per scrupolo. Se e' raw non ci sono info sul kind, quindi questa linea e' irraggiungibile
      if(raw) then
        call iotk_error_issue(ierr,"iotk_scan_dat_aux",__FILE__,__LINE__)
# 538 "iotk_dat.spp"
call iotk_error_msg(ierr,"CVS Revision: 1.25 ")
        return
      end if
      allocate(dat2(ubound(dat,1)))
      if(stream) then
        call iotk_stream_read(lunit,idummy,dat2,ierr=ierr)
        if(ierr/=0) then
          deallocate(dat2)
          call iotk_error_issue(ierr,"iotk_scan_dat_aux",__FILE__,__LINE__)
# 546 "iotk_dat.spp"
call iotk_error_msg(ierr,"CVS Revision: 1.25 ")
# 546 "iotk_dat.spp"
call iotk_error_msg(ierr,' ')
# 546 "iotk_dat.spp"
call iotk_error_write(ierr,"iostat",iostat)
          return
        end if
      else
        read(lunit,iostat=iostat) idummy,( dat2(i), i=1,ubound(dat2,1) )
        if(iostat/=0) then
          deallocate(dat2)
          call iotk_error_issue(ierr,"iotk_scan_dat_aux",__FILE__,__LINE__)
# 553 "iotk_dat.spp"
call iotk_error_msg(ierr,"CVS Revision: 1.25 ")
# 553 "iotk_dat.spp"
call iotk_error_msg(ierr,' ')
# 553 "iotk_dat.spp"
call iotk_error_write(ierr,"iostat",iostat)
          return
        end if
      end if
# 566 "iotk_dat.spp"
      dat = real(dat2,kind=kind(dat))
# 570 "iotk_dat.spp"
      deallocate(dat2)
#endif
# 534 "iotk_dat.spp"
#ifdef __IOTK_REAL3
    case(kind(dat3))
      ! Giusto per scrupolo. Se e' raw non ci sono info sul kind, quindi questa linea e' irraggiungibile
      if(raw) then
        call iotk_error_issue(ierr,"iotk_scan_dat_aux",__FILE__,__LINE__)
# 538 "iotk_dat.spp"
call iotk_error_msg(ierr,"CVS Revision: 1.25 ")
        return
      end if
      allocate(dat3(ubound(dat,1)))
      if(stream) then
        call iotk_stream_read(lunit,idummy,dat3,ierr=ierr)
        if(ierr/=0) then
          deallocate(dat3)
          call iotk_error_issue(ierr,"iotk_scan_dat_aux",__FILE__,__LINE__)
# 546 "iotk_dat.spp"
call iotk_error_msg(ierr,"CVS Revision: 1.25 ")
# 546 "iotk_dat.spp"
call iotk_error_msg(ierr,' ')
# 546 "iotk_dat.spp"
call iotk_error_write(ierr,"iostat",iostat)
          return
        end if
      else
        read(lunit,iostat=iostat) idummy,( dat3(i), i=1,ubound(dat3,1) )
        if(iostat/=0) then
          deallocate(dat3)
          call iotk_error_issue(ierr,"iotk_scan_dat_aux",__FILE__,__LINE__)
# 553 "iotk_dat.spp"
call iotk_error_msg(ierr,"CVS Revision: 1.25 ")
# 553 "iotk_dat.spp"
call iotk_error_msg(ierr,' ')
# 553 "iotk_dat.spp"
call iotk_error_write(ierr,"iostat",iostat)
          return
        end if
      end if
# 566 "iotk_dat.spp"
      dat = real(dat3,kind=kind(dat))
# 570 "iotk_dat.spp"
      deallocate(dat3)
#endif
# 574 "iotk_dat.spp"
    case default
      call iotk_error_issue(ierr,"iotk_scan_dat_aux",__FILE__,__LINE__)
# 575 "iotk_dat.spp"
call iotk_error_msg(ierr,"CVS Revision: 1.25 ")
# 575 "iotk_dat.spp"
call iotk_error_msg(ierr,'Kind incompatibility')
# 575 "iotk_dat.spp"
call iotk_error_write(ierr,"kind",rkind)
    end select
  else
    if(raw) then
#ifdef __IOTK_WORKAROUND3
      read(lunit,fmt=*,iostat=iostat) ( dat(j), j=1,ubound(dat,1) )
#else
      read(lunit,fmt=*,iostat=iostat) dat
#endif
      if(iostat/=0) then
        call iotk_error_issue(ierr,"iotk_scan_dat_aux",__FILE__,__LINE__)
# 585 "iotk_dat.spp"
call iotk_error_msg(ierr,"CVS Revision: 1.25 ")
# 585 "iotk_dat.spp"
call iotk_error_msg(ierr,' ')
# 585 "iotk_dat.spp"
call iotk_error_write(ierr,"iostat",iostat)
        return
      end if
    else if(iotk_strcomp(fmt,"*")) then
#ifdef __IOTK_WORKAROUND3
      read(lunit,fmt=*,iostat=iostat) ( dat(j), j=1,ubound(dat,1) )
#else
      read(lunit,fmt=*,iostat=iostat) dat
#endif
      if(iostat/=0) then
        call iotk_error_issue(ierr,"iotk_scan_dat_aux",__FILE__,__LINE__)
# 595 "iotk_dat.spp"
call iotk_error_msg(ierr,"CVS Revision: 1.25 ")
# 595 "iotk_dat.spp"
call iotk_error_msg(ierr,' ')
# 595 "iotk_dat.spp"
call iotk_error_write(ierr,"iostat",iostat)
        return
      end if
    else if(iotk_strcomp(fmt,"!")) then
      index = 0
      do
        call iotk_getline(lunit,line,length,ierr)
        if(ierr/=0) then
          call iotk_error_issue(ierr,"iotk_scan_dat_aux",__FILE__,__LINE__)
# 603 "iotk_dat.spp"
call iotk_error_msg(ierr,"CVS Revision: 1.25 ")
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
# 614 "iotk_dat.spp"
call iotk_error_msg(ierr,"CVS Revision: 1.25 ")
# 614 "iotk_dat.spp"
call iotk_error_msg(ierr,' ')
# 614 "iotk_dat.spp"
call iotk_error_write(ierr,"iostat",iostat)
            return
          end if
          call iotk_getline(lunit,altline,altlength,ierr)
          if(ierr/=0) then
            call iotk_error_issue(ierr,"iotk_scan_dat_aux",__FILE__,__LINE__)
# 619 "iotk_dat.spp"
call iotk_error_msg(ierr,"CVS Revision: 1.25 ")
            return
          end if
          backspace(lunit,iostat=iostat)
          if(iostat/=0) then
            call iotk_error_issue(ierr,"iotk_scan_dat_aux",__FILE__,__LINE__)
# 624 "iotk_dat.spp"
call iotk_error_msg(ierr,"CVS Revision: 1.25 ")
# 624 "iotk_dat.spp"
call iotk_error_msg(ierr,' ')
# 624 "iotk_dat.spp"
call iotk_error_write(ierr,"iostat",iostat)
            return
          end if
          read(lunit,"(a)",advance="no",iostat=iostat) altline(1:nexttag-1 + altlength - length)
          if(iostat/=0) then
            call iotk_error_issue(ierr,"iotk_scan_dat_aux",__FILE__,__LINE__)
# 629 "iotk_dat.spp"
call iotk_error_msg(ierr,"CVS Revision: 1.25 ")
# 629 "iotk_dat.spp"
call iotk_error_msg(ierr,' ')
# 629 "iotk_dat.spp"
call iotk_error_write(ierr,"iostat",iostat)
            return
          end if
        end if
        call iotk_str_clean(line(1:nexttag - 1))
        call iotk_read(dat,line(1:nexttag - 1),index,ierr)
        if(ierr/=0) then
          call iotk_error_issue(ierr,"iotk_scan_dat_aux",__FILE__,__LINE__)
# 636 "iotk_dat.spp"
call iotk_error_msg(ierr,"CVS Revision: 1.25 ")
# 636 "iotk_dat.spp"
call iotk_error_msg(ierr,'Error reading REAL data')
          return
        end if
# 642 "iotk_dat.spp"
        if(index == size(dat)) exit
# 644 "iotk_dat.spp"
        if(nexttag/=length + 1) then
          call iotk_error_issue(ierr,"iotk_scan_dat_aux",__FILE__,__LINE__)
# 645 "iotk_dat.spp"
call iotk_error_msg(ierr,"CVS Revision: 1.25 ")
          return
        end if
      end do
    else
#ifdef __IOTK_WORKAROUND3
      read(lunit,fmt=fmt(1:iotk_strlen(fmt)),iostat=iostat) ( dat(j), j=1,ubound(dat,1) )
#else
      read(lunit,fmt=fmt(1:iotk_strlen(fmt)),iostat=iostat) dat
#endif
      if(iostat/=0) then
        call iotk_error_issue(ierr,"iotk_scan_dat_aux",__FILE__,__LINE__)
# 656 "iotk_dat.spp"
call iotk_error_msg(ierr,"CVS Revision: 1.25 ")
# 656 "iotk_dat.spp"
call iotk_error_msg(ierr,' ')
# 656 "iotk_dat.spp"
call iotk_error_write(ierr,"iostat",iostat)
        return
      end if
    end if
  end if
# 662 "iotk_dat.spp"
  if(idummy/=0) then
    call iotk_error_issue(ierr,"iotk_scan_dat_aux",__FILE__,__LINE__)
# 663 "iotk_dat.spp"
call iotk_error_msg(ierr,"CVS Revision: 1.25 ")
    return
  end if
end subroutine iotk_scan_dat_aux_REAL4
# 668 "iotk_dat.spp"

# 670 "iotk_dat.spp"
subroutine iotk_scan_dat_REAL4_1(unit,name,dat,dummy,attr,found,default,ierr)
  use iotk_base
  use iotk_error_interf
  use iotk_dat_interf, only: iotk_scan_dat_aux
  use iotk_scan_interf
  use iotk_str_interf
  use iotk_misc_interf
  implicit none
  integer, parameter :: this_kind = iotk_REAL4
  integer,                                   intent(in)  :: unit
  character(len=*),                          intent(in)  :: name
#ifdef __IOTK_WORKAROUND6
  REAL(kind=this_kind)                        :: dat (:)
#else
  REAL(kind=this_kind),           intent(out) :: dat (:)
#endif
  type(iotk_dummytype),            optional              :: dummy
#ifdef __IOTK_WORKAROUND6
  character(len=*),                optional              :: attr
#else
  character(len=*),                optional, intent(out) :: attr
#endif
  logical,                         optional, intent(out) :: found
  REAL(kind=this_kind), optional, intent(in)  :: default (:)
  integer,                         optional, intent(out) :: ierr
# 698 "iotk_dat.spp"
  REAL (kind=this_kind),              allocatable :: tmpdat(:)
# 700 "iotk_dat.spp"
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
# 717 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
    goto 1
  end if
  call iotk_parse_dat(lattr,rtype,rkind,rsize,rlen,fmt,columns,ierrl)
! Note that columns is not effectively used
  if(ierrl/=0) goto 1
  if(.not. (iotk_strcomp(rtype,iotk_eos) .or. iotk_strcomp(rtype,"REAL") ) ) then
    call iotk_error_issue(ierrl,"iotk_scan_dat",__FILE__,__LINE__)
# 724 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
# 724 "iotk_dat.spp"
call iotk_error_msg(ierrl,' ')
# 724 "iotk_dat.spp"
call iotk_error_write(ierrl,"rtype",rtype(1:iotk_strlen(rtype)))
# 724 "iotk_dat.spp"
call iotk_error_write(ierrl,"type","REAL")
    goto 1
  end if
  if(.not. (rsize==-1 .or. rsize==size(dat)) ) then
    call iotk_error_issue(ierrl,"iotk_scan_dat",__FILE__,__LINE__)
# 728 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
    goto 1
  end if
  if(rkind==-1) rkind = kind(dat)
# 735 "iotk_dat.spp"

  allocate(tmpdat(size(dat)))
  call iotk_scan_dat_aux(unit,tmpdat,rkind,rlen,fmt(1:iotk_strlen(fmt)),ierrl)
  if(ierrl/=0) then
    call iotk_error_issue(ierrl,"iotk_scan_dat",__FILE__,__LINE__)
# 739 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
# 739 "iotk_dat.spp"
call iotk_error_msg(ierrl,'Error reading data')
# 739 "iotk_dat.spp"
call iotk_error_write(ierrl,"name",name)
# 739 "iotk_dat.spp"
call iotk_error_write(ierrl,"rkind",rkind)
# 739 "iotk_dat.spp"
call iotk_error_write(ierrl,"rlen",rlen)
    goto 1
  end if
# 745 "iotk_dat.spp"
#if defined(__IOTK_WORKAROUND3) || defined(__IOTK_WORKAROUND4)
# 749 "iotk_dat.spp"
     call iotk_private_pack_REAL4(dat,tmpdat,size(tmpdat),1)
# 751 "iotk_dat.spp"
#else
     dat = reshape(tmpdat,shape(dat))
#endif
# 755 "iotk_dat.spp"
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
# 767 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
# 767 "iotk_dat.spp"
call iotk_error_msg(ierrl,'Dat not found')
# 767 "iotk_dat.spp"
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
end subroutine iotk_scan_dat_REAL4_1


#endif
#endif

subroutine iotk_dat_dummy_REAL4_1
  write(0,*)
end subroutine iotk_dat_dummy_REAL4_1


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

#ifdef __IOTK_REAL4
#if 2 <= __IOTK_MAXRANK
# 82 "iotk_dat.spp"
subroutine iotk_write_dat_REAL4_2(unit,name,dat,dummy,attr,columns,sep,fmt,ierr)
  use iotk_base
  use iotk_error_interf
  use iotk_attr_interf, only : iotk_write_attr
  use iotk_write_interf
  use iotk_fmt_interf
  use iotk_str_interf
  use iotk_unit_interf
  use iotk_misc_interf
  implicit none
  integer, parameter :: this_kind = iotk_REAL4
  integer,                                    intent(in)  :: unit
  character(len=*),                           intent(in)  :: name
  REAL (kind=this_kind),           intent(in)  :: dat (:,:) 
  type(iotk_dummytype),             optional              :: dummy
  character(len=*),                 optional, intent(in)  :: attr
  integer,                          optional, intent(in)  :: columns
  character(len=*),                 optional, intent(in)  :: sep
  character(len=*),                 optional, intent(in)  :: fmt
  integer,                          optional, intent(out) :: ierr
  integer :: ierrl,lunit,iostat
  logical :: binary,raw,stream
  integer :: lcolumns
  integer(iotk_header_kind), parameter :: idummy=0
  character(100) :: lsep
  character(300) :: usefmt
  character(iotk_attlenx) :: lattr
  character(iotk_attlenx) :: attr_tmp
  type (iotk_unit), pointer :: this
# 115 "iotk_dat.spp"
  REAL (kind=this_kind),allocatable :: dattmp(:)
# 117 "iotk_dat.spp"
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
# 126 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
      goto 1
    end if
  end if
  lunit = iotk_phys_unit(unit)
  call iotk_unit_get(lunit,pointer=this)
  raw = .false.
  if(associated(this)) then
    raw = this%raw
  end if
  call iotk_inquire(lunit,binary,stream,ierrl)
  if(ierrl/=0) then
    call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 138 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
    goto 1
  end if
  call iotk_strcpy(usefmt,"!",ierrl)
  if(ierrl/=0) then
    call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 143 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
    goto 1
  end if
  if(present(fmt) .and. .not. raw) call iotk_strcpy(usefmt,iotk_strtrim(fmt),ierrl)
  if(ierrl/=0) then
    call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 148 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
    goto 1
  end if
  if(iotk_strscan(usefmt,"<>&")/=0) then
    call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 152 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
# 152 "iotk_dat.spp"
call iotk_error_msg(ierrl,'Special characters (<>&) found in fmt string')
# 152 "iotk_dat.spp"
call iotk_error_write(ierrl,"unit",unit)
# 152 "iotk_dat.spp"
call iotk_error_write(ierrl,"name",trim(name))
# 152 "iotk_dat.spp"
call iotk_error_write(ierrl,"fmt",trim(fmt))
    goto 1
  end if
  call iotk_write_attr(lattr,"type",iotk_tolower("REAL"),first=.true.,ierr=ierrl)
  if(ierrl/=0) then
    call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 157 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
    goto 1
  end if
  call iotk_write_attr(lattr,"size",size(dat),ierr=ierrl)
  if(ierrl/=0) then
    call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 162 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
    goto 1
  end if
# 172 "iotk_dat.spp"
  if(binary) then
    call iotk_write_attr(lattr,"kind",kind(dat),ierr=ierrl)
    if(ierrl/=0) then
      call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 175 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
      goto 1
    end if
  end if
# 180 "iotk_dat.spp"
  if(.not.iotk_strcomp(usefmt,"!")) call iotk_write_attr(lattr,"fmt",iotk_strtrim(usefmt),ierr=ierrl)
  if(ierrl/=0) then
    call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 182 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
    goto 1
  end if
  if(lcolumns/=1) call iotk_write_attr(lattr,"columns",lcolumns,ierr=ierrl)
  if(ierrl/=0) then
    call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 187 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
    goto 1
  end if
  if(present(attr)) then
    attr_tmp(1:1)=iotk_eos
    call iotk_strcpy(attr_tmp,attr,ierr=ierrl)
    if(ierrl/=0) then
      call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 194 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
      goto 1
    end if
    call iotk_delete_attr(attr_tmp,"type",ierrl)
    if(ierrl/=0) then
      call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 199 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
      goto 1
    end if
    call iotk_delete_attr(attr_tmp,"kind",ierrl)
    if(ierrl/=0) then
      call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 204 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
      goto 1
    end if
    call iotk_delete_attr(attr_tmp,"size",ierrl)
    if(ierrl/=0) then
      call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 209 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
      goto 1
    end if
    call iotk_delete_attr(attr_tmp,"fmt",ierrl)
    if(ierrl/=0) then
      call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 214 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
      goto 1
    end if
    call iotk_delete_attr(attr_tmp,"columns",ierrl)
    if(ierrl/=0) then
      call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 219 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
      goto 1
    end if
    call iotk_delete_attr(attr_tmp,"len",ierrl)
    if(ierrl/=0) then
      call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 224 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
      goto 1
    end if
    if(iotk_strlen_trim(attr_tmp)>0) call iotk_strcat(lattr,iotk_strtrim(attr_tmp),ierr=ierrl)
  end if
  if(ierrl/=0) then
    call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 230 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
    goto 1
  end if
  call iotk_write_begin(unit,name,lattr,ierr=ierrl)
  if(ierrl/=0) then
    call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 235 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
    goto 1
  end if

  allocate(dattmp(size(dat)))
# 243 "iotk_dat.spp"
#if defined(__IOTK_WORKAROUND3) || defined(__IOTK_WORKAROUND4)
# 247 "iotk_dat.spp"
     call iotk_private_pack_REAL4(dattmp,dat,size(dattmp),1)
# 249 "iotk_dat.spp"
#else
     dattmp = pack(dat,mask=.true.)
#endif
# 253 "iotk_dat.spp"

  if(binary) then
    if(raw) then
      write(lunit,iostat=iostat) (dattmp(itmp),itmp=1,size(dattmp))
      if(iostat/=0) then
        call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 258 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
        goto 1
      end if
    else
      write(lunit,iostat=iostat) idummy,(dattmp(itmp),itmp=1,size(dattmp))
      if(iostat/=0) then
        call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 264 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
        goto 1
      end if
    end if
  else
    if(raw) then
# 273 "iotk_dat.spp"
      write(lunit,*,iostat=iostat) (dattmp(itmp),itmp=1,size(dattmp))
# 275 "iotk_dat.spp"
      if(iostat/=0) then
        call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 276 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
        goto 1
      end if
    else if(iotk_strcomp(usefmt,"*")) then
      write(lunit,*,iostat=iostat) (dattmp(itmp),itmp=1,size(dattmp))
      if(iostat/=0) then
        call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 282 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
        goto 1
      end if
    else if(iotk_strcomp(usefmt,"!")) then
# 296 "iotk_dat.spp"
     write(lunit,fmt=trim(iotk_wfmt("REAL",kind(dattmp),lcolumns,-1,lsep)),iostat=iostat) (dattmp(itmp),itmp=1,size(dattmp))
     if(iostat/=0) then
      call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 298 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
      goto 1
     end if
# 302 "iotk_dat.spp"
    else
      write(lunit,fmt=usefmt(1:iotk_strlen(usefmt)),iostat=iostat) (dattmp(itmp),itmp=1,size(dattmp))
      if(iostat/=0) then
        call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 305 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
        goto 1
      end if
    end if
  end if
  call iotk_write_end(unit,name,ierr=ierrl)
  if(ierrl/=0) then
    call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 312 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
    goto 1
  end if
1 continue
  if(allocated(dattmp)) deallocate(dattmp)
  if(present(ierr)) then
    ierr = ierrl
  else
    if(ierrl/=0) call iotk_error_handler(ierrl)
  end if
end subroutine iotk_write_dat_REAL4_2


# 668 "iotk_dat.spp"

# 670 "iotk_dat.spp"
subroutine iotk_scan_dat_REAL4_2(unit,name,dat,dummy,attr,found,default,ierr)
  use iotk_base
  use iotk_error_interf
  use iotk_dat_interf, only: iotk_scan_dat_aux
  use iotk_scan_interf
  use iotk_str_interf
  use iotk_misc_interf
  implicit none
  integer, parameter :: this_kind = iotk_REAL4
  integer,                                   intent(in)  :: unit
  character(len=*),                          intent(in)  :: name
#ifdef __IOTK_WORKAROUND6
  REAL(kind=this_kind)                        :: dat (:,:)
#else
  REAL(kind=this_kind),           intent(out) :: dat (:,:)
#endif
  type(iotk_dummytype),            optional              :: dummy
#ifdef __IOTK_WORKAROUND6
  character(len=*),                optional              :: attr
#else
  character(len=*),                optional, intent(out) :: attr
#endif
  logical,                         optional, intent(out) :: found
  REAL(kind=this_kind), optional, intent(in)  :: default (:,:)
  integer,                         optional, intent(out) :: ierr
# 698 "iotk_dat.spp"
  REAL (kind=this_kind),              allocatable :: tmpdat(:)
# 700 "iotk_dat.spp"
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
# 717 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
    goto 1
  end if
  call iotk_parse_dat(lattr,rtype,rkind,rsize,rlen,fmt,columns,ierrl)
! Note that columns is not effectively used
  if(ierrl/=0) goto 1
  if(.not. (iotk_strcomp(rtype,iotk_eos) .or. iotk_strcomp(rtype,"REAL") ) ) then
    call iotk_error_issue(ierrl,"iotk_scan_dat",__FILE__,__LINE__)
# 724 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
# 724 "iotk_dat.spp"
call iotk_error_msg(ierrl,' ')
# 724 "iotk_dat.spp"
call iotk_error_write(ierrl,"rtype",rtype(1:iotk_strlen(rtype)))
# 724 "iotk_dat.spp"
call iotk_error_write(ierrl,"type","REAL")
    goto 1
  end if
  if(.not. (rsize==-1 .or. rsize==size(dat)) ) then
    call iotk_error_issue(ierrl,"iotk_scan_dat",__FILE__,__LINE__)
# 728 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
    goto 1
  end if
  if(rkind==-1) rkind = kind(dat)
# 735 "iotk_dat.spp"

  allocate(tmpdat(size(dat)))
  call iotk_scan_dat_aux(unit,tmpdat,rkind,rlen,fmt(1:iotk_strlen(fmt)),ierrl)
  if(ierrl/=0) then
    call iotk_error_issue(ierrl,"iotk_scan_dat",__FILE__,__LINE__)
# 739 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
# 739 "iotk_dat.spp"
call iotk_error_msg(ierrl,'Error reading data')
# 739 "iotk_dat.spp"
call iotk_error_write(ierrl,"name",name)
# 739 "iotk_dat.spp"
call iotk_error_write(ierrl,"rkind",rkind)
# 739 "iotk_dat.spp"
call iotk_error_write(ierrl,"rlen",rlen)
    goto 1
  end if
# 745 "iotk_dat.spp"
#if defined(__IOTK_WORKAROUND3) || defined(__IOTK_WORKAROUND4)
# 749 "iotk_dat.spp"
     call iotk_private_pack_REAL4(dat,tmpdat,size(tmpdat),1)
# 751 "iotk_dat.spp"
#else
     dat = reshape(tmpdat,shape(dat))
#endif
# 755 "iotk_dat.spp"
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
# 767 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
# 767 "iotk_dat.spp"
call iotk_error_msg(ierrl,'Dat not found')
# 767 "iotk_dat.spp"
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
end subroutine iotk_scan_dat_REAL4_2


#endif
#endif

subroutine iotk_dat_dummy_REAL4_2
  write(0,*)
end subroutine iotk_dat_dummy_REAL4_2


# 45 "iotk_dat.spp"

# 48 "iotk_dat.spp"
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
# 65 "iotk_dat.spp"


!------------------------------------------------------------------------------!
! Inclusion of configuration file
#include "iotk_config.h"
!------------------------------------------------------------------------------!

# 74 "iotk_dat.spp"
#include "iotk_auxmacros.h"
# 76 "iotk_dat.spp"

# 78 "iotk_dat.spp"

#ifdef __IOTK_REAL4
#if 3 <= __IOTK_MAXRANK
# 82 "iotk_dat.spp"
subroutine iotk_write_dat_REAL4_3(unit,name,dat,dummy,attr,columns,sep,fmt,ierr)
  use iotk_base
  use iotk_error_interf
  use iotk_attr_interf, only : iotk_write_attr
  use iotk_write_interf
  use iotk_fmt_interf
  use iotk_str_interf
  use iotk_unit_interf
  use iotk_misc_interf
  implicit none
  integer, parameter :: this_kind = iotk_REAL4
  integer,                                    intent(in)  :: unit
  character(len=*),                           intent(in)  :: name
  REAL (kind=this_kind),           intent(in)  :: dat (:,:,:) 
  type(iotk_dummytype),             optional              :: dummy
  character(len=*),                 optional, intent(in)  :: attr
  integer,                          optional, intent(in)  :: columns
  character(len=*),                 optional, intent(in)  :: sep
  character(len=*),                 optional, intent(in)  :: fmt
  integer,                          optional, intent(out) :: ierr
  integer :: ierrl,lunit,iostat
  logical :: binary,raw,stream
  integer :: lcolumns
  integer(iotk_header_kind), parameter :: idummy=0
  character(100) :: lsep
  character(300) :: usefmt
  character(iotk_attlenx) :: lattr
  character(iotk_attlenx) :: attr_tmp
  type (iotk_unit), pointer :: this
# 115 "iotk_dat.spp"
  REAL (kind=this_kind),allocatable :: dattmp(:)
# 117 "iotk_dat.spp"
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
# 126 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
      goto 1
    end if
  end if
  lunit = iotk_phys_unit(unit)
  call iotk_unit_get(lunit,pointer=this)
  raw = .false.
  if(associated(this)) then
    raw = this%raw
  end if
  call iotk_inquire(lunit,binary,stream,ierrl)
  if(ierrl/=0) then
    call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 138 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
    goto 1
  end if
  call iotk_strcpy(usefmt,"!",ierrl)
  if(ierrl/=0) then
    call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 143 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
    goto 1
  end if
  if(present(fmt) .and. .not. raw) call iotk_strcpy(usefmt,iotk_strtrim(fmt),ierrl)
  if(ierrl/=0) then
    call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 148 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
    goto 1
  end if
  if(iotk_strscan(usefmt,"<>&")/=0) then
    call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 152 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
# 152 "iotk_dat.spp"
call iotk_error_msg(ierrl,'Special characters (<>&) found in fmt string')
# 152 "iotk_dat.spp"
call iotk_error_write(ierrl,"unit",unit)
# 152 "iotk_dat.spp"
call iotk_error_write(ierrl,"name",trim(name))
# 152 "iotk_dat.spp"
call iotk_error_write(ierrl,"fmt",trim(fmt))
    goto 1
  end if
  call iotk_write_attr(lattr,"type",iotk_tolower("REAL"),first=.true.,ierr=ierrl)
  if(ierrl/=0) then
    call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 157 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
    goto 1
  end if
  call iotk_write_attr(lattr,"size",size(dat),ierr=ierrl)
  if(ierrl/=0) then
    call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 162 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
    goto 1
  end if
# 172 "iotk_dat.spp"
  if(binary) then
    call iotk_write_attr(lattr,"kind",kind(dat),ierr=ierrl)
    if(ierrl/=0) then
      call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 175 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
      goto 1
    end if
  end if
# 180 "iotk_dat.spp"
  if(.not.iotk_strcomp(usefmt,"!")) call iotk_write_attr(lattr,"fmt",iotk_strtrim(usefmt),ierr=ierrl)
  if(ierrl/=0) then
    call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 182 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
    goto 1
  end if
  if(lcolumns/=1) call iotk_write_attr(lattr,"columns",lcolumns,ierr=ierrl)
  if(ierrl/=0) then
    call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 187 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
    goto 1
  end if
  if(present(attr)) then
    attr_tmp(1:1)=iotk_eos
    call iotk_strcpy(attr_tmp,attr,ierr=ierrl)
    if(ierrl/=0) then
      call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 194 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
      goto 1
    end if
    call iotk_delete_attr(attr_tmp,"type",ierrl)
    if(ierrl/=0) then
      call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 199 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
      goto 1
    end if
    call iotk_delete_attr(attr_tmp,"kind",ierrl)
    if(ierrl/=0) then
      call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 204 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
      goto 1
    end if
    call iotk_delete_attr(attr_tmp,"size",ierrl)
    if(ierrl/=0) then
      call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 209 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
      goto 1
    end if
    call iotk_delete_attr(attr_tmp,"fmt",ierrl)
    if(ierrl/=0) then
      call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 214 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
      goto 1
    end if
    call iotk_delete_attr(attr_tmp,"columns",ierrl)
    if(ierrl/=0) then
      call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 219 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
      goto 1
    end if
    call iotk_delete_attr(attr_tmp,"len",ierrl)
    if(ierrl/=0) then
      call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 224 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
      goto 1
    end if
    if(iotk_strlen_trim(attr_tmp)>0) call iotk_strcat(lattr,iotk_strtrim(attr_tmp),ierr=ierrl)
  end if
  if(ierrl/=0) then
    call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 230 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
    goto 1
  end if
  call iotk_write_begin(unit,name,lattr,ierr=ierrl)
  if(ierrl/=0) then
    call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 235 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
    goto 1
  end if

  allocate(dattmp(size(dat)))
# 243 "iotk_dat.spp"
#if defined(__IOTK_WORKAROUND3) || defined(__IOTK_WORKAROUND4)
# 247 "iotk_dat.spp"
     call iotk_private_pack_REAL4(dattmp,dat,size(dattmp),1)
# 249 "iotk_dat.spp"
#else
     dattmp = pack(dat,mask=.true.)
#endif
# 253 "iotk_dat.spp"

  if(binary) then
    if(raw) then
      write(lunit,iostat=iostat) (dattmp(itmp),itmp=1,size(dattmp))
      if(iostat/=0) then
        call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 258 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
        goto 1
      end if
    else
      write(lunit,iostat=iostat) idummy,(dattmp(itmp),itmp=1,size(dattmp))
      if(iostat/=0) then
        call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 264 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
        goto 1
      end if
    end if
  else
    if(raw) then
# 273 "iotk_dat.spp"
      write(lunit,*,iostat=iostat) (dattmp(itmp),itmp=1,size(dattmp))
# 275 "iotk_dat.spp"
      if(iostat/=0) then
        call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 276 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
        goto 1
      end if
    else if(iotk_strcomp(usefmt,"*")) then
      write(lunit,*,iostat=iostat) (dattmp(itmp),itmp=1,size(dattmp))
      if(iostat/=0) then
        call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 282 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
        goto 1
      end if
    else if(iotk_strcomp(usefmt,"!")) then
# 296 "iotk_dat.spp"
     write(lunit,fmt=trim(iotk_wfmt("REAL",kind(dattmp),lcolumns,-1,lsep)),iostat=iostat) (dattmp(itmp),itmp=1,size(dattmp))
     if(iostat/=0) then
      call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 298 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
      goto 1
     end if
# 302 "iotk_dat.spp"
    else
      write(lunit,fmt=usefmt(1:iotk_strlen(usefmt)),iostat=iostat) (dattmp(itmp),itmp=1,size(dattmp))
      if(iostat/=0) then
        call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 305 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
        goto 1
      end if
    end if
  end if
  call iotk_write_end(unit,name,ierr=ierrl)
  if(ierrl/=0) then
    call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 312 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
    goto 1
  end if
1 continue
  if(allocated(dattmp)) deallocate(dattmp)
  if(present(ierr)) then
    ierr = ierrl
  else
    if(ierrl/=0) call iotk_error_handler(ierrl)
  end if
end subroutine iotk_write_dat_REAL4_3


# 668 "iotk_dat.spp"

# 670 "iotk_dat.spp"
subroutine iotk_scan_dat_REAL4_3(unit,name,dat,dummy,attr,found,default,ierr)
  use iotk_base
  use iotk_error_interf
  use iotk_dat_interf, only: iotk_scan_dat_aux
  use iotk_scan_interf
  use iotk_str_interf
  use iotk_misc_interf
  implicit none
  integer, parameter :: this_kind = iotk_REAL4
  integer,                                   intent(in)  :: unit
  character(len=*),                          intent(in)  :: name
#ifdef __IOTK_WORKAROUND6
  REAL(kind=this_kind)                        :: dat (:,:,:)
#else
  REAL(kind=this_kind),           intent(out) :: dat (:,:,:)
#endif
  type(iotk_dummytype),            optional              :: dummy
#ifdef __IOTK_WORKAROUND6
  character(len=*),                optional              :: attr
#else
  character(len=*),                optional, intent(out) :: attr
#endif
  logical,                         optional, intent(out) :: found
  REAL(kind=this_kind), optional, intent(in)  :: default (:,:,:)
  integer,                         optional, intent(out) :: ierr
# 698 "iotk_dat.spp"
  REAL (kind=this_kind),              allocatable :: tmpdat(:)
# 700 "iotk_dat.spp"
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
# 717 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
    goto 1
  end if
  call iotk_parse_dat(lattr,rtype,rkind,rsize,rlen,fmt,columns,ierrl)
! Note that columns is not effectively used
  if(ierrl/=0) goto 1
  if(.not. (iotk_strcomp(rtype,iotk_eos) .or. iotk_strcomp(rtype,"REAL") ) ) then
    call iotk_error_issue(ierrl,"iotk_scan_dat",__FILE__,__LINE__)
# 724 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
# 724 "iotk_dat.spp"
call iotk_error_msg(ierrl,' ')
# 724 "iotk_dat.spp"
call iotk_error_write(ierrl,"rtype",rtype(1:iotk_strlen(rtype)))
# 724 "iotk_dat.spp"
call iotk_error_write(ierrl,"type","REAL")
    goto 1
  end if
  if(.not. (rsize==-1 .or. rsize==size(dat)) ) then
    call iotk_error_issue(ierrl,"iotk_scan_dat",__FILE__,__LINE__)
# 728 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
    goto 1
  end if
  if(rkind==-1) rkind = kind(dat)
# 735 "iotk_dat.spp"

  allocate(tmpdat(size(dat)))
  call iotk_scan_dat_aux(unit,tmpdat,rkind,rlen,fmt(1:iotk_strlen(fmt)),ierrl)
  if(ierrl/=0) then
    call iotk_error_issue(ierrl,"iotk_scan_dat",__FILE__,__LINE__)
# 739 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
# 739 "iotk_dat.spp"
call iotk_error_msg(ierrl,'Error reading data')
# 739 "iotk_dat.spp"
call iotk_error_write(ierrl,"name",name)
# 739 "iotk_dat.spp"
call iotk_error_write(ierrl,"rkind",rkind)
# 739 "iotk_dat.spp"
call iotk_error_write(ierrl,"rlen",rlen)
    goto 1
  end if
# 745 "iotk_dat.spp"
#if defined(__IOTK_WORKAROUND3) || defined(__IOTK_WORKAROUND4)
# 749 "iotk_dat.spp"
     call iotk_private_pack_REAL4(dat,tmpdat,size(tmpdat),1)
# 751 "iotk_dat.spp"
#else
     dat = reshape(tmpdat,shape(dat))
#endif
# 755 "iotk_dat.spp"
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
# 767 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
# 767 "iotk_dat.spp"
call iotk_error_msg(ierrl,'Dat not found')
# 767 "iotk_dat.spp"
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
end subroutine iotk_scan_dat_REAL4_3


#endif
#endif

subroutine iotk_dat_dummy_REAL4_3
  write(0,*)
end subroutine iotk_dat_dummy_REAL4_3


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

#ifdef __IOTK_REAL4
#if 4 <= __IOTK_MAXRANK
# 82 "iotk_dat.spp"
subroutine iotk_write_dat_REAL4_4(unit,name,dat,dummy,attr,columns,sep,fmt,ierr)
  use iotk_base
  use iotk_error_interf
  use iotk_attr_interf, only : iotk_write_attr
  use iotk_write_interf
  use iotk_fmt_interf
  use iotk_str_interf
  use iotk_unit_interf
  use iotk_misc_interf
  implicit none
  integer, parameter :: this_kind = iotk_REAL4
  integer,                                    intent(in)  :: unit
  character(len=*),                           intent(in)  :: name
  REAL (kind=this_kind),           intent(in)  :: dat (:,:,:,:) 
  type(iotk_dummytype),             optional              :: dummy
  character(len=*),                 optional, intent(in)  :: attr
  integer,                          optional, intent(in)  :: columns
  character(len=*),                 optional, intent(in)  :: sep
  character(len=*),                 optional, intent(in)  :: fmt
  integer,                          optional, intent(out) :: ierr
  integer :: ierrl,lunit,iostat
  logical :: binary,raw,stream
  integer :: lcolumns
  integer(iotk_header_kind), parameter :: idummy=0
  character(100) :: lsep
  character(300) :: usefmt
  character(iotk_attlenx) :: lattr
  character(iotk_attlenx) :: attr_tmp
  type (iotk_unit), pointer :: this
# 115 "iotk_dat.spp"
  REAL (kind=this_kind),allocatable :: dattmp(:)
# 117 "iotk_dat.spp"
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
# 126 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
      goto 1
    end if
  end if
  lunit = iotk_phys_unit(unit)
  call iotk_unit_get(lunit,pointer=this)
  raw = .false.
  if(associated(this)) then
    raw = this%raw
  end if
  call iotk_inquire(lunit,binary,stream,ierrl)
  if(ierrl/=0) then
    call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 138 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
    goto 1
  end if
  call iotk_strcpy(usefmt,"!",ierrl)
  if(ierrl/=0) then
    call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 143 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
    goto 1
  end if
  if(present(fmt) .and. .not. raw) call iotk_strcpy(usefmt,iotk_strtrim(fmt),ierrl)
  if(ierrl/=0) then
    call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 148 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
    goto 1
  end if
  if(iotk_strscan(usefmt,"<>&")/=0) then
    call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 152 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
# 152 "iotk_dat.spp"
call iotk_error_msg(ierrl,'Special characters (<>&) found in fmt string')
# 152 "iotk_dat.spp"
call iotk_error_write(ierrl,"unit",unit)
# 152 "iotk_dat.spp"
call iotk_error_write(ierrl,"name",trim(name))
# 152 "iotk_dat.spp"
call iotk_error_write(ierrl,"fmt",trim(fmt))
    goto 1
  end if
  call iotk_write_attr(lattr,"type",iotk_tolower("REAL"),first=.true.,ierr=ierrl)
  if(ierrl/=0) then
    call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 157 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
    goto 1
  end if
  call iotk_write_attr(lattr,"size",size(dat),ierr=ierrl)
  if(ierrl/=0) then
    call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 162 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
    goto 1
  end if
# 172 "iotk_dat.spp"
  if(binary) then
    call iotk_write_attr(lattr,"kind",kind(dat),ierr=ierrl)
    if(ierrl/=0) then
      call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 175 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
      goto 1
    end if
  end if
# 180 "iotk_dat.spp"
  if(.not.iotk_strcomp(usefmt,"!")) call iotk_write_attr(lattr,"fmt",iotk_strtrim(usefmt),ierr=ierrl)
  if(ierrl/=0) then
    call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 182 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
    goto 1
  end if
  if(lcolumns/=1) call iotk_write_attr(lattr,"columns",lcolumns,ierr=ierrl)
  if(ierrl/=0) then
    call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 187 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
    goto 1
  end if
  if(present(attr)) then
    attr_tmp(1:1)=iotk_eos
    call iotk_strcpy(attr_tmp,attr,ierr=ierrl)
    if(ierrl/=0) then
      call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 194 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
      goto 1
    end if
    call iotk_delete_attr(attr_tmp,"type",ierrl)
    if(ierrl/=0) then
      call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 199 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
      goto 1
    end if
    call iotk_delete_attr(attr_tmp,"kind",ierrl)
    if(ierrl/=0) then
      call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 204 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
      goto 1
    end if
    call iotk_delete_attr(attr_tmp,"size",ierrl)
    if(ierrl/=0) then
      call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 209 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
      goto 1
    end if
    call iotk_delete_attr(attr_tmp,"fmt",ierrl)
    if(ierrl/=0) then
      call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 214 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
      goto 1
    end if
    call iotk_delete_attr(attr_tmp,"columns",ierrl)
    if(ierrl/=0) then
      call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 219 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
      goto 1
    end if
    call iotk_delete_attr(attr_tmp,"len",ierrl)
    if(ierrl/=0) then
      call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 224 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
      goto 1
    end if
    if(iotk_strlen_trim(attr_tmp)>0) call iotk_strcat(lattr,iotk_strtrim(attr_tmp),ierr=ierrl)
  end if
  if(ierrl/=0) then
    call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 230 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
    goto 1
  end if
  call iotk_write_begin(unit,name,lattr,ierr=ierrl)
  if(ierrl/=0) then
    call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 235 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
    goto 1
  end if

  allocate(dattmp(size(dat)))
# 243 "iotk_dat.spp"
#if defined(__IOTK_WORKAROUND3) || defined(__IOTK_WORKAROUND4)
# 247 "iotk_dat.spp"
     call iotk_private_pack_REAL4(dattmp,dat,size(dattmp),1)
# 249 "iotk_dat.spp"
#else
     dattmp = pack(dat,mask=.true.)
#endif
# 253 "iotk_dat.spp"

  if(binary) then
    if(raw) then
      write(lunit,iostat=iostat) (dattmp(itmp),itmp=1,size(dattmp))
      if(iostat/=0) then
        call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 258 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
        goto 1
      end if
    else
      write(lunit,iostat=iostat) idummy,(dattmp(itmp),itmp=1,size(dattmp))
      if(iostat/=0) then
        call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 264 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
        goto 1
      end if
    end if
  else
    if(raw) then
# 273 "iotk_dat.spp"
      write(lunit,*,iostat=iostat) (dattmp(itmp),itmp=1,size(dattmp))
# 275 "iotk_dat.spp"
      if(iostat/=0) then
        call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 276 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
        goto 1
      end if
    else if(iotk_strcomp(usefmt,"*")) then
      write(lunit,*,iostat=iostat) (dattmp(itmp),itmp=1,size(dattmp))
      if(iostat/=0) then
        call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 282 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
        goto 1
      end if
    else if(iotk_strcomp(usefmt,"!")) then
# 296 "iotk_dat.spp"
     write(lunit,fmt=trim(iotk_wfmt("REAL",kind(dattmp),lcolumns,-1,lsep)),iostat=iostat) (dattmp(itmp),itmp=1,size(dattmp))
     if(iostat/=0) then
      call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 298 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
      goto 1
     end if
# 302 "iotk_dat.spp"
    else
      write(lunit,fmt=usefmt(1:iotk_strlen(usefmt)),iostat=iostat) (dattmp(itmp),itmp=1,size(dattmp))
      if(iostat/=0) then
        call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 305 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
        goto 1
      end if
    end if
  end if
  call iotk_write_end(unit,name,ierr=ierrl)
  if(ierrl/=0) then
    call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 312 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
    goto 1
  end if
1 continue
  if(allocated(dattmp)) deallocate(dattmp)
  if(present(ierr)) then
    ierr = ierrl
  else
    if(ierrl/=0) call iotk_error_handler(ierrl)
  end if
end subroutine iotk_write_dat_REAL4_4


# 668 "iotk_dat.spp"

# 670 "iotk_dat.spp"
subroutine iotk_scan_dat_REAL4_4(unit,name,dat,dummy,attr,found,default,ierr)
  use iotk_base
  use iotk_error_interf
  use iotk_dat_interf, only: iotk_scan_dat_aux
  use iotk_scan_interf
  use iotk_str_interf
  use iotk_misc_interf
  implicit none
  integer, parameter :: this_kind = iotk_REAL4
  integer,                                   intent(in)  :: unit
  character(len=*),                          intent(in)  :: name
#ifdef __IOTK_WORKAROUND6
  REAL(kind=this_kind)                        :: dat (:,:,:,:)
#else
  REAL(kind=this_kind),           intent(out) :: dat (:,:,:,:)
#endif
  type(iotk_dummytype),            optional              :: dummy
#ifdef __IOTK_WORKAROUND6
  character(len=*),                optional              :: attr
#else
  character(len=*),                optional, intent(out) :: attr
#endif
  logical,                         optional, intent(out) :: found
  REAL(kind=this_kind), optional, intent(in)  :: default (:,:,:,:)
  integer,                         optional, intent(out) :: ierr
# 698 "iotk_dat.spp"
  REAL (kind=this_kind),              allocatable :: tmpdat(:)
# 700 "iotk_dat.spp"
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
# 717 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
    goto 1
  end if
  call iotk_parse_dat(lattr,rtype,rkind,rsize,rlen,fmt,columns,ierrl)
! Note that columns is not effectively used
  if(ierrl/=0) goto 1
  if(.not. (iotk_strcomp(rtype,iotk_eos) .or. iotk_strcomp(rtype,"REAL") ) ) then
    call iotk_error_issue(ierrl,"iotk_scan_dat",__FILE__,__LINE__)
# 724 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
# 724 "iotk_dat.spp"
call iotk_error_msg(ierrl,' ')
# 724 "iotk_dat.spp"
call iotk_error_write(ierrl,"rtype",rtype(1:iotk_strlen(rtype)))
# 724 "iotk_dat.spp"
call iotk_error_write(ierrl,"type","REAL")
    goto 1
  end if
  if(.not. (rsize==-1 .or. rsize==size(dat)) ) then
    call iotk_error_issue(ierrl,"iotk_scan_dat",__FILE__,__LINE__)
# 728 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
    goto 1
  end if
  if(rkind==-1) rkind = kind(dat)
# 735 "iotk_dat.spp"

  allocate(tmpdat(size(dat)))
  call iotk_scan_dat_aux(unit,tmpdat,rkind,rlen,fmt(1:iotk_strlen(fmt)),ierrl)
  if(ierrl/=0) then
    call iotk_error_issue(ierrl,"iotk_scan_dat",__FILE__,__LINE__)
# 739 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
# 739 "iotk_dat.spp"
call iotk_error_msg(ierrl,'Error reading data')
# 739 "iotk_dat.spp"
call iotk_error_write(ierrl,"name",name)
# 739 "iotk_dat.spp"
call iotk_error_write(ierrl,"rkind",rkind)
# 739 "iotk_dat.spp"
call iotk_error_write(ierrl,"rlen",rlen)
    goto 1
  end if
# 745 "iotk_dat.spp"
#if defined(__IOTK_WORKAROUND3) || defined(__IOTK_WORKAROUND4)
# 749 "iotk_dat.spp"
     call iotk_private_pack_REAL4(dat,tmpdat,size(tmpdat),1)
# 751 "iotk_dat.spp"
#else
     dat = reshape(tmpdat,shape(dat))
#endif
# 755 "iotk_dat.spp"
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
# 767 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
# 767 "iotk_dat.spp"
call iotk_error_msg(ierrl,'Dat not found')
# 767 "iotk_dat.spp"
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
end subroutine iotk_scan_dat_REAL4_4


#endif
#endif

subroutine iotk_dat_dummy_REAL4_4
  write(0,*)
end subroutine iotk_dat_dummy_REAL4_4


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

#ifdef __IOTK_REAL4
#if 5 <= __IOTK_MAXRANK
# 82 "iotk_dat.spp"
subroutine iotk_write_dat_REAL4_5(unit,name,dat,dummy,attr,columns,sep,fmt,ierr)
  use iotk_base
  use iotk_error_interf
  use iotk_attr_interf, only : iotk_write_attr
  use iotk_write_interf
  use iotk_fmt_interf
  use iotk_str_interf
  use iotk_unit_interf
  use iotk_misc_interf
  implicit none
  integer, parameter :: this_kind = iotk_REAL4
  integer,                                    intent(in)  :: unit
  character(len=*),                           intent(in)  :: name
  REAL (kind=this_kind),           intent(in)  :: dat (:,:,:,:,:) 
  type(iotk_dummytype),             optional              :: dummy
  character(len=*),                 optional, intent(in)  :: attr
  integer,                          optional, intent(in)  :: columns
  character(len=*),                 optional, intent(in)  :: sep
  character(len=*),                 optional, intent(in)  :: fmt
  integer,                          optional, intent(out) :: ierr
  integer :: ierrl,lunit,iostat
  logical :: binary,raw,stream
  integer :: lcolumns
  integer(iotk_header_kind), parameter :: idummy=0
  character(100) :: lsep
  character(300) :: usefmt
  character(iotk_attlenx) :: lattr
  character(iotk_attlenx) :: attr_tmp
  type (iotk_unit), pointer :: this
# 115 "iotk_dat.spp"
  REAL (kind=this_kind),allocatable :: dattmp(:)
# 117 "iotk_dat.spp"
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
# 126 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
      goto 1
    end if
  end if
  lunit = iotk_phys_unit(unit)
  call iotk_unit_get(lunit,pointer=this)
  raw = .false.
  if(associated(this)) then
    raw = this%raw
  end if
  call iotk_inquire(lunit,binary,stream,ierrl)
  if(ierrl/=0) then
    call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 138 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
    goto 1
  end if
  call iotk_strcpy(usefmt,"!",ierrl)
  if(ierrl/=0) then
    call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 143 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
    goto 1
  end if
  if(present(fmt) .and. .not. raw) call iotk_strcpy(usefmt,iotk_strtrim(fmt),ierrl)
  if(ierrl/=0) then
    call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 148 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
    goto 1
  end if
  if(iotk_strscan(usefmt,"<>&")/=0) then
    call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 152 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
# 152 "iotk_dat.spp"
call iotk_error_msg(ierrl,'Special characters (<>&) found in fmt string')
# 152 "iotk_dat.spp"
call iotk_error_write(ierrl,"unit",unit)
# 152 "iotk_dat.spp"
call iotk_error_write(ierrl,"name",trim(name))
# 152 "iotk_dat.spp"
call iotk_error_write(ierrl,"fmt",trim(fmt))
    goto 1
  end if
  call iotk_write_attr(lattr,"type",iotk_tolower("REAL"),first=.true.,ierr=ierrl)
  if(ierrl/=0) then
    call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 157 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
    goto 1
  end if
  call iotk_write_attr(lattr,"size",size(dat),ierr=ierrl)
  if(ierrl/=0) then
    call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 162 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
    goto 1
  end if
# 172 "iotk_dat.spp"
  if(binary) then
    call iotk_write_attr(lattr,"kind",kind(dat),ierr=ierrl)
    if(ierrl/=0) then
      call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 175 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
      goto 1
    end if
  end if
# 180 "iotk_dat.spp"
  if(.not.iotk_strcomp(usefmt,"!")) call iotk_write_attr(lattr,"fmt",iotk_strtrim(usefmt),ierr=ierrl)
  if(ierrl/=0) then
    call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 182 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
    goto 1
  end if
  if(lcolumns/=1) call iotk_write_attr(lattr,"columns",lcolumns,ierr=ierrl)
  if(ierrl/=0) then
    call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 187 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
    goto 1
  end if
  if(present(attr)) then
    attr_tmp(1:1)=iotk_eos
    call iotk_strcpy(attr_tmp,attr,ierr=ierrl)
    if(ierrl/=0) then
      call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 194 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
      goto 1
    end if
    call iotk_delete_attr(attr_tmp,"type",ierrl)
    if(ierrl/=0) then
      call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 199 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
      goto 1
    end if
    call iotk_delete_attr(attr_tmp,"kind",ierrl)
    if(ierrl/=0) then
      call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 204 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
      goto 1
    end if
    call iotk_delete_attr(attr_tmp,"size",ierrl)
    if(ierrl/=0) then
      call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 209 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
      goto 1
    end if
    call iotk_delete_attr(attr_tmp,"fmt",ierrl)
    if(ierrl/=0) then
      call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 214 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
      goto 1
    end if
    call iotk_delete_attr(attr_tmp,"columns",ierrl)
    if(ierrl/=0) then
      call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 219 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
      goto 1
    end if
    call iotk_delete_attr(attr_tmp,"len",ierrl)
    if(ierrl/=0) then
      call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 224 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
      goto 1
    end if
    if(iotk_strlen_trim(attr_tmp)>0) call iotk_strcat(lattr,iotk_strtrim(attr_tmp),ierr=ierrl)
  end if
  if(ierrl/=0) then
    call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 230 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
    goto 1
  end if
  call iotk_write_begin(unit,name,lattr,ierr=ierrl)
  if(ierrl/=0) then
    call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 235 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
    goto 1
  end if

  allocate(dattmp(size(dat)))
# 243 "iotk_dat.spp"
#if defined(__IOTK_WORKAROUND3) || defined(__IOTK_WORKAROUND4)
# 247 "iotk_dat.spp"
     call iotk_private_pack_REAL4(dattmp,dat,size(dattmp),1)
# 249 "iotk_dat.spp"
#else
     dattmp = pack(dat,mask=.true.)
#endif
# 253 "iotk_dat.spp"

  if(binary) then
    if(raw) then
      write(lunit,iostat=iostat) (dattmp(itmp),itmp=1,size(dattmp))
      if(iostat/=0) then
        call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 258 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
        goto 1
      end if
    else
      write(lunit,iostat=iostat) idummy,(dattmp(itmp),itmp=1,size(dattmp))
      if(iostat/=0) then
        call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 264 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
        goto 1
      end if
    end if
  else
    if(raw) then
# 273 "iotk_dat.spp"
      write(lunit,*,iostat=iostat) (dattmp(itmp),itmp=1,size(dattmp))
# 275 "iotk_dat.spp"
      if(iostat/=0) then
        call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 276 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
        goto 1
      end if
    else if(iotk_strcomp(usefmt,"*")) then
      write(lunit,*,iostat=iostat) (dattmp(itmp),itmp=1,size(dattmp))
      if(iostat/=0) then
        call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 282 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
        goto 1
      end if
    else if(iotk_strcomp(usefmt,"!")) then
# 296 "iotk_dat.spp"
     write(lunit,fmt=trim(iotk_wfmt("REAL",kind(dattmp),lcolumns,-1,lsep)),iostat=iostat) (dattmp(itmp),itmp=1,size(dattmp))
     if(iostat/=0) then
      call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 298 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
      goto 1
     end if
# 302 "iotk_dat.spp"
    else
      write(lunit,fmt=usefmt(1:iotk_strlen(usefmt)),iostat=iostat) (dattmp(itmp),itmp=1,size(dattmp))
      if(iostat/=0) then
        call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 305 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
        goto 1
      end if
    end if
  end if
  call iotk_write_end(unit,name,ierr=ierrl)
  if(ierrl/=0) then
    call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 312 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
    goto 1
  end if
1 continue
  if(allocated(dattmp)) deallocate(dattmp)
  if(present(ierr)) then
    ierr = ierrl
  else
    if(ierrl/=0) call iotk_error_handler(ierrl)
  end if
end subroutine iotk_write_dat_REAL4_5


# 668 "iotk_dat.spp"

# 670 "iotk_dat.spp"
subroutine iotk_scan_dat_REAL4_5(unit,name,dat,dummy,attr,found,default,ierr)
  use iotk_base
  use iotk_error_interf
  use iotk_dat_interf, only: iotk_scan_dat_aux
  use iotk_scan_interf
  use iotk_str_interf
  use iotk_misc_interf
  implicit none
  integer, parameter :: this_kind = iotk_REAL4
  integer,                                   intent(in)  :: unit
  character(len=*),                          intent(in)  :: name
#ifdef __IOTK_WORKAROUND6
  REAL(kind=this_kind)                        :: dat (:,:,:,:,:)
#else
  REAL(kind=this_kind),           intent(out) :: dat (:,:,:,:,:)
#endif
  type(iotk_dummytype),            optional              :: dummy
#ifdef __IOTK_WORKAROUND6
  character(len=*),                optional              :: attr
#else
  character(len=*),                optional, intent(out) :: attr
#endif
  logical,                         optional, intent(out) :: found
  REAL(kind=this_kind), optional, intent(in)  :: default (:,:,:,:,:)
  integer,                         optional, intent(out) :: ierr
# 698 "iotk_dat.spp"
  REAL (kind=this_kind),              allocatable :: tmpdat(:)
# 700 "iotk_dat.spp"
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
# 717 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
    goto 1
  end if
  call iotk_parse_dat(lattr,rtype,rkind,rsize,rlen,fmt,columns,ierrl)
! Note that columns is not effectively used
  if(ierrl/=0) goto 1
  if(.not. (iotk_strcomp(rtype,iotk_eos) .or. iotk_strcomp(rtype,"REAL") ) ) then
    call iotk_error_issue(ierrl,"iotk_scan_dat",__FILE__,__LINE__)
# 724 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
# 724 "iotk_dat.spp"
call iotk_error_msg(ierrl,' ')
# 724 "iotk_dat.spp"
call iotk_error_write(ierrl,"rtype",rtype(1:iotk_strlen(rtype)))
# 724 "iotk_dat.spp"
call iotk_error_write(ierrl,"type","REAL")
    goto 1
  end if
  if(.not. (rsize==-1 .or. rsize==size(dat)) ) then
    call iotk_error_issue(ierrl,"iotk_scan_dat",__FILE__,__LINE__)
# 728 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
    goto 1
  end if
  if(rkind==-1) rkind = kind(dat)
# 735 "iotk_dat.spp"

  allocate(tmpdat(size(dat)))
  call iotk_scan_dat_aux(unit,tmpdat,rkind,rlen,fmt(1:iotk_strlen(fmt)),ierrl)
  if(ierrl/=0) then
    call iotk_error_issue(ierrl,"iotk_scan_dat",__FILE__,__LINE__)
# 739 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
# 739 "iotk_dat.spp"
call iotk_error_msg(ierrl,'Error reading data')
# 739 "iotk_dat.spp"
call iotk_error_write(ierrl,"name",name)
# 739 "iotk_dat.spp"
call iotk_error_write(ierrl,"rkind",rkind)
# 739 "iotk_dat.spp"
call iotk_error_write(ierrl,"rlen",rlen)
    goto 1
  end if
# 745 "iotk_dat.spp"
#if defined(__IOTK_WORKAROUND3) || defined(__IOTK_WORKAROUND4)
# 749 "iotk_dat.spp"
     call iotk_private_pack_REAL4(dat,tmpdat,size(tmpdat),1)
# 751 "iotk_dat.spp"
#else
     dat = reshape(tmpdat,shape(dat))
#endif
# 755 "iotk_dat.spp"
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
# 767 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
# 767 "iotk_dat.spp"
call iotk_error_msg(ierrl,'Dat not found')
# 767 "iotk_dat.spp"
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
end subroutine iotk_scan_dat_REAL4_5


#endif
#endif

subroutine iotk_dat_dummy_REAL4_5
  write(0,*)
end subroutine iotk_dat_dummy_REAL4_5


# 45 "iotk_dat.spp"

# 48 "iotk_dat.spp"
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
# 65 "iotk_dat.spp"


!------------------------------------------------------------------------------!
! Inclusion of configuration file
#include "iotk_config.h"
!------------------------------------------------------------------------------!

# 74 "iotk_dat.spp"
#include "iotk_auxmacros.h"
# 76 "iotk_dat.spp"

# 78 "iotk_dat.spp"

#ifdef __IOTK_REAL4
#if 6 <= __IOTK_MAXRANK
# 82 "iotk_dat.spp"
subroutine iotk_write_dat_REAL4_6(unit,name,dat,dummy,attr,columns,sep,fmt,ierr)
  use iotk_base
  use iotk_error_interf
  use iotk_attr_interf, only : iotk_write_attr
  use iotk_write_interf
  use iotk_fmt_interf
  use iotk_str_interf
  use iotk_unit_interf
  use iotk_misc_interf
  implicit none
  integer, parameter :: this_kind = iotk_REAL4
  integer,                                    intent(in)  :: unit
  character(len=*),                           intent(in)  :: name
  REAL (kind=this_kind),           intent(in)  :: dat (:,:,:,:,:,:) 
  type(iotk_dummytype),             optional              :: dummy
  character(len=*),                 optional, intent(in)  :: attr
  integer,                          optional, intent(in)  :: columns
  character(len=*),                 optional, intent(in)  :: sep
  character(len=*),                 optional, intent(in)  :: fmt
  integer,                          optional, intent(out) :: ierr
  integer :: ierrl,lunit,iostat
  logical :: binary,raw,stream
  integer :: lcolumns
  integer(iotk_header_kind), parameter :: idummy=0
  character(100) :: lsep
  character(300) :: usefmt
  character(iotk_attlenx) :: lattr
  character(iotk_attlenx) :: attr_tmp
  type (iotk_unit), pointer :: this
# 115 "iotk_dat.spp"
  REAL (kind=this_kind),allocatable :: dattmp(:)
# 117 "iotk_dat.spp"
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
# 126 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
      goto 1
    end if
  end if
  lunit = iotk_phys_unit(unit)
  call iotk_unit_get(lunit,pointer=this)
  raw = .false.
  if(associated(this)) then
    raw = this%raw
  end if
  call iotk_inquire(lunit,binary,stream,ierrl)
  if(ierrl/=0) then
    call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 138 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
    goto 1
  end if
  call iotk_strcpy(usefmt,"!",ierrl)
  if(ierrl/=0) then
    call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 143 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
    goto 1
  end if
  if(present(fmt) .and. .not. raw) call iotk_strcpy(usefmt,iotk_strtrim(fmt),ierrl)
  if(ierrl/=0) then
    call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 148 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
    goto 1
  end if
  if(iotk_strscan(usefmt,"<>&")/=0) then
    call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 152 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
# 152 "iotk_dat.spp"
call iotk_error_msg(ierrl,'Special characters (<>&) found in fmt string')
# 152 "iotk_dat.spp"
call iotk_error_write(ierrl,"unit",unit)
# 152 "iotk_dat.spp"
call iotk_error_write(ierrl,"name",trim(name))
# 152 "iotk_dat.spp"
call iotk_error_write(ierrl,"fmt",trim(fmt))
    goto 1
  end if
  call iotk_write_attr(lattr,"type",iotk_tolower("REAL"),first=.true.,ierr=ierrl)
  if(ierrl/=0) then
    call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 157 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
    goto 1
  end if
  call iotk_write_attr(lattr,"size",size(dat),ierr=ierrl)
  if(ierrl/=0) then
    call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 162 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
    goto 1
  end if
# 172 "iotk_dat.spp"
  if(binary) then
    call iotk_write_attr(lattr,"kind",kind(dat),ierr=ierrl)
    if(ierrl/=0) then
      call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 175 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
      goto 1
    end if
  end if
# 180 "iotk_dat.spp"
  if(.not.iotk_strcomp(usefmt,"!")) call iotk_write_attr(lattr,"fmt",iotk_strtrim(usefmt),ierr=ierrl)
  if(ierrl/=0) then
    call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 182 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
    goto 1
  end if
  if(lcolumns/=1) call iotk_write_attr(lattr,"columns",lcolumns,ierr=ierrl)
  if(ierrl/=0) then
    call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 187 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
    goto 1
  end if
  if(present(attr)) then
    attr_tmp(1:1)=iotk_eos
    call iotk_strcpy(attr_tmp,attr,ierr=ierrl)
    if(ierrl/=0) then
      call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 194 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
      goto 1
    end if
    call iotk_delete_attr(attr_tmp,"type",ierrl)
    if(ierrl/=0) then
      call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 199 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
      goto 1
    end if
    call iotk_delete_attr(attr_tmp,"kind",ierrl)
    if(ierrl/=0) then
      call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 204 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
      goto 1
    end if
    call iotk_delete_attr(attr_tmp,"size",ierrl)
    if(ierrl/=0) then
      call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 209 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
      goto 1
    end if
    call iotk_delete_attr(attr_tmp,"fmt",ierrl)
    if(ierrl/=0) then
      call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 214 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
      goto 1
    end if
    call iotk_delete_attr(attr_tmp,"columns",ierrl)
    if(ierrl/=0) then
      call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 219 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
      goto 1
    end if
    call iotk_delete_attr(attr_tmp,"len",ierrl)
    if(ierrl/=0) then
      call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 224 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
      goto 1
    end if
    if(iotk_strlen_trim(attr_tmp)>0) call iotk_strcat(lattr,iotk_strtrim(attr_tmp),ierr=ierrl)
  end if
  if(ierrl/=0) then
    call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 230 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
    goto 1
  end if
  call iotk_write_begin(unit,name,lattr,ierr=ierrl)
  if(ierrl/=0) then
    call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 235 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
    goto 1
  end if

  allocate(dattmp(size(dat)))
# 243 "iotk_dat.spp"
#if defined(__IOTK_WORKAROUND3) || defined(__IOTK_WORKAROUND4)
# 247 "iotk_dat.spp"
     call iotk_private_pack_REAL4(dattmp,dat,size(dattmp),1)
# 249 "iotk_dat.spp"
#else
     dattmp = pack(dat,mask=.true.)
#endif
# 253 "iotk_dat.spp"

  if(binary) then
    if(raw) then
      write(lunit,iostat=iostat) (dattmp(itmp),itmp=1,size(dattmp))
      if(iostat/=0) then
        call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 258 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
        goto 1
      end if
    else
      write(lunit,iostat=iostat) idummy,(dattmp(itmp),itmp=1,size(dattmp))
      if(iostat/=0) then
        call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 264 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
        goto 1
      end if
    end if
  else
    if(raw) then
# 273 "iotk_dat.spp"
      write(lunit,*,iostat=iostat) (dattmp(itmp),itmp=1,size(dattmp))
# 275 "iotk_dat.spp"
      if(iostat/=0) then
        call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 276 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
        goto 1
      end if
    else if(iotk_strcomp(usefmt,"*")) then
      write(lunit,*,iostat=iostat) (dattmp(itmp),itmp=1,size(dattmp))
      if(iostat/=0) then
        call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 282 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
        goto 1
      end if
    else if(iotk_strcomp(usefmt,"!")) then
# 296 "iotk_dat.spp"
     write(lunit,fmt=trim(iotk_wfmt("REAL",kind(dattmp),lcolumns,-1,lsep)),iostat=iostat) (dattmp(itmp),itmp=1,size(dattmp))
     if(iostat/=0) then
      call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 298 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
      goto 1
     end if
# 302 "iotk_dat.spp"
    else
      write(lunit,fmt=usefmt(1:iotk_strlen(usefmt)),iostat=iostat) (dattmp(itmp),itmp=1,size(dattmp))
      if(iostat/=0) then
        call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 305 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
        goto 1
      end if
    end if
  end if
  call iotk_write_end(unit,name,ierr=ierrl)
  if(ierrl/=0) then
    call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 312 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
    goto 1
  end if
1 continue
  if(allocated(dattmp)) deallocate(dattmp)
  if(present(ierr)) then
    ierr = ierrl
  else
    if(ierrl/=0) call iotk_error_handler(ierrl)
  end if
end subroutine iotk_write_dat_REAL4_6


# 668 "iotk_dat.spp"

# 670 "iotk_dat.spp"
subroutine iotk_scan_dat_REAL4_6(unit,name,dat,dummy,attr,found,default,ierr)
  use iotk_base
  use iotk_error_interf
  use iotk_dat_interf, only: iotk_scan_dat_aux
  use iotk_scan_interf
  use iotk_str_interf
  use iotk_misc_interf
  implicit none
  integer, parameter :: this_kind = iotk_REAL4
  integer,                                   intent(in)  :: unit
  character(len=*),                          intent(in)  :: name
#ifdef __IOTK_WORKAROUND6
  REAL(kind=this_kind)                        :: dat (:,:,:,:,:,:)
#else
  REAL(kind=this_kind),           intent(out) :: dat (:,:,:,:,:,:)
#endif
  type(iotk_dummytype),            optional              :: dummy
#ifdef __IOTK_WORKAROUND6
  character(len=*),                optional              :: attr
#else
  character(len=*),                optional, intent(out) :: attr
#endif
  logical,                         optional, intent(out) :: found
  REAL(kind=this_kind), optional, intent(in)  :: default (:,:,:,:,:,:)
  integer,                         optional, intent(out) :: ierr
# 698 "iotk_dat.spp"
  REAL (kind=this_kind),              allocatable :: tmpdat(:)
# 700 "iotk_dat.spp"
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
# 717 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
    goto 1
  end if
  call iotk_parse_dat(lattr,rtype,rkind,rsize,rlen,fmt,columns,ierrl)
! Note that columns is not effectively used
  if(ierrl/=0) goto 1
  if(.not. (iotk_strcomp(rtype,iotk_eos) .or. iotk_strcomp(rtype,"REAL") ) ) then
    call iotk_error_issue(ierrl,"iotk_scan_dat",__FILE__,__LINE__)
# 724 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
# 724 "iotk_dat.spp"
call iotk_error_msg(ierrl,' ')
# 724 "iotk_dat.spp"
call iotk_error_write(ierrl,"rtype",rtype(1:iotk_strlen(rtype)))
# 724 "iotk_dat.spp"
call iotk_error_write(ierrl,"type","REAL")
    goto 1
  end if
  if(.not. (rsize==-1 .or. rsize==size(dat)) ) then
    call iotk_error_issue(ierrl,"iotk_scan_dat",__FILE__,__LINE__)
# 728 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
    goto 1
  end if
  if(rkind==-1) rkind = kind(dat)
# 735 "iotk_dat.spp"

  allocate(tmpdat(size(dat)))
  call iotk_scan_dat_aux(unit,tmpdat,rkind,rlen,fmt(1:iotk_strlen(fmt)),ierrl)
  if(ierrl/=0) then
    call iotk_error_issue(ierrl,"iotk_scan_dat",__FILE__,__LINE__)
# 739 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
# 739 "iotk_dat.spp"
call iotk_error_msg(ierrl,'Error reading data')
# 739 "iotk_dat.spp"
call iotk_error_write(ierrl,"name",name)
# 739 "iotk_dat.spp"
call iotk_error_write(ierrl,"rkind",rkind)
# 739 "iotk_dat.spp"
call iotk_error_write(ierrl,"rlen",rlen)
    goto 1
  end if
# 745 "iotk_dat.spp"
#if defined(__IOTK_WORKAROUND3) || defined(__IOTK_WORKAROUND4)
# 749 "iotk_dat.spp"
     call iotk_private_pack_REAL4(dat,tmpdat,size(tmpdat),1)
# 751 "iotk_dat.spp"
#else
     dat = reshape(tmpdat,shape(dat))
#endif
# 755 "iotk_dat.spp"
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
# 767 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
# 767 "iotk_dat.spp"
call iotk_error_msg(ierrl,'Dat not found')
# 767 "iotk_dat.spp"
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
end subroutine iotk_scan_dat_REAL4_6


#endif
#endif

subroutine iotk_dat_dummy_REAL4_6
  write(0,*)
end subroutine iotk_dat_dummy_REAL4_6


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

#ifdef __IOTK_REAL4
#if 7 <= __IOTK_MAXRANK
# 82 "iotk_dat.spp"
subroutine iotk_write_dat_REAL4_7(unit,name,dat,dummy,attr,columns,sep,fmt,ierr)
  use iotk_base
  use iotk_error_interf
  use iotk_attr_interf, only : iotk_write_attr
  use iotk_write_interf
  use iotk_fmt_interf
  use iotk_str_interf
  use iotk_unit_interf
  use iotk_misc_interf
  implicit none
  integer, parameter :: this_kind = iotk_REAL4
  integer,                                    intent(in)  :: unit
  character(len=*),                           intent(in)  :: name
  REAL (kind=this_kind),           intent(in)  :: dat (:,:,:,:,:,:,:) 
  type(iotk_dummytype),             optional              :: dummy
  character(len=*),                 optional, intent(in)  :: attr
  integer,                          optional, intent(in)  :: columns
  character(len=*),                 optional, intent(in)  :: sep
  character(len=*),                 optional, intent(in)  :: fmt
  integer,                          optional, intent(out) :: ierr
  integer :: ierrl,lunit,iostat
  logical :: binary,raw,stream
  integer :: lcolumns
  integer(iotk_header_kind), parameter :: idummy=0
  character(100) :: lsep
  character(300) :: usefmt
  character(iotk_attlenx) :: lattr
  character(iotk_attlenx) :: attr_tmp
  type (iotk_unit), pointer :: this
# 115 "iotk_dat.spp"
  REAL (kind=this_kind),allocatable :: dattmp(:)
# 117 "iotk_dat.spp"
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
# 126 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
      goto 1
    end if
  end if
  lunit = iotk_phys_unit(unit)
  call iotk_unit_get(lunit,pointer=this)
  raw = .false.
  if(associated(this)) then
    raw = this%raw
  end if
  call iotk_inquire(lunit,binary,stream,ierrl)
  if(ierrl/=0) then
    call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 138 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
    goto 1
  end if
  call iotk_strcpy(usefmt,"!",ierrl)
  if(ierrl/=0) then
    call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 143 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
    goto 1
  end if
  if(present(fmt) .and. .not. raw) call iotk_strcpy(usefmt,iotk_strtrim(fmt),ierrl)
  if(ierrl/=0) then
    call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 148 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
    goto 1
  end if
  if(iotk_strscan(usefmt,"<>&")/=0) then
    call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 152 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
# 152 "iotk_dat.spp"
call iotk_error_msg(ierrl,'Special characters (<>&) found in fmt string')
# 152 "iotk_dat.spp"
call iotk_error_write(ierrl,"unit",unit)
# 152 "iotk_dat.spp"
call iotk_error_write(ierrl,"name",trim(name))
# 152 "iotk_dat.spp"
call iotk_error_write(ierrl,"fmt",trim(fmt))
    goto 1
  end if
  call iotk_write_attr(lattr,"type",iotk_tolower("REAL"),first=.true.,ierr=ierrl)
  if(ierrl/=0) then
    call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 157 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
    goto 1
  end if
  call iotk_write_attr(lattr,"size",size(dat),ierr=ierrl)
  if(ierrl/=0) then
    call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 162 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
    goto 1
  end if
# 172 "iotk_dat.spp"
  if(binary) then
    call iotk_write_attr(lattr,"kind",kind(dat),ierr=ierrl)
    if(ierrl/=0) then
      call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 175 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
      goto 1
    end if
  end if
# 180 "iotk_dat.spp"
  if(.not.iotk_strcomp(usefmt,"!")) call iotk_write_attr(lattr,"fmt",iotk_strtrim(usefmt),ierr=ierrl)
  if(ierrl/=0) then
    call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 182 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
    goto 1
  end if
  if(lcolumns/=1) call iotk_write_attr(lattr,"columns",lcolumns,ierr=ierrl)
  if(ierrl/=0) then
    call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 187 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
    goto 1
  end if
  if(present(attr)) then
    attr_tmp(1:1)=iotk_eos
    call iotk_strcpy(attr_tmp,attr,ierr=ierrl)
    if(ierrl/=0) then
      call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 194 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
      goto 1
    end if
    call iotk_delete_attr(attr_tmp,"type",ierrl)
    if(ierrl/=0) then
      call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 199 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
      goto 1
    end if
    call iotk_delete_attr(attr_tmp,"kind",ierrl)
    if(ierrl/=0) then
      call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 204 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
      goto 1
    end if
    call iotk_delete_attr(attr_tmp,"size",ierrl)
    if(ierrl/=0) then
      call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 209 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
      goto 1
    end if
    call iotk_delete_attr(attr_tmp,"fmt",ierrl)
    if(ierrl/=0) then
      call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 214 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
      goto 1
    end if
    call iotk_delete_attr(attr_tmp,"columns",ierrl)
    if(ierrl/=0) then
      call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 219 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
      goto 1
    end if
    call iotk_delete_attr(attr_tmp,"len",ierrl)
    if(ierrl/=0) then
      call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 224 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
      goto 1
    end if
    if(iotk_strlen_trim(attr_tmp)>0) call iotk_strcat(lattr,iotk_strtrim(attr_tmp),ierr=ierrl)
  end if
  if(ierrl/=0) then
    call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 230 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
    goto 1
  end if
  call iotk_write_begin(unit,name,lattr,ierr=ierrl)
  if(ierrl/=0) then
    call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 235 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
    goto 1
  end if

  allocate(dattmp(size(dat)))
# 243 "iotk_dat.spp"
#if defined(__IOTK_WORKAROUND3) || defined(__IOTK_WORKAROUND4)
# 247 "iotk_dat.spp"
     call iotk_private_pack_REAL4(dattmp,dat,size(dattmp),1)
# 249 "iotk_dat.spp"
#else
     dattmp = pack(dat,mask=.true.)
#endif
# 253 "iotk_dat.spp"

  if(binary) then
    if(raw) then
      write(lunit,iostat=iostat) (dattmp(itmp),itmp=1,size(dattmp))
      if(iostat/=0) then
        call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 258 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
        goto 1
      end if
    else
      write(lunit,iostat=iostat) idummy,(dattmp(itmp),itmp=1,size(dattmp))
      if(iostat/=0) then
        call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 264 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
        goto 1
      end if
    end if
  else
    if(raw) then
# 273 "iotk_dat.spp"
      write(lunit,*,iostat=iostat) (dattmp(itmp),itmp=1,size(dattmp))
# 275 "iotk_dat.spp"
      if(iostat/=0) then
        call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 276 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
        goto 1
      end if
    else if(iotk_strcomp(usefmt,"*")) then
      write(lunit,*,iostat=iostat) (dattmp(itmp),itmp=1,size(dattmp))
      if(iostat/=0) then
        call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 282 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
        goto 1
      end if
    else if(iotk_strcomp(usefmt,"!")) then
# 296 "iotk_dat.spp"
     write(lunit,fmt=trim(iotk_wfmt("REAL",kind(dattmp),lcolumns,-1,lsep)),iostat=iostat) (dattmp(itmp),itmp=1,size(dattmp))
     if(iostat/=0) then
      call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 298 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
      goto 1
     end if
# 302 "iotk_dat.spp"
    else
      write(lunit,fmt=usefmt(1:iotk_strlen(usefmt)),iostat=iostat) (dattmp(itmp),itmp=1,size(dattmp))
      if(iostat/=0) then
        call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 305 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
        goto 1
      end if
    end if
  end if
  call iotk_write_end(unit,name,ierr=ierrl)
  if(ierrl/=0) then
    call iotk_error_issue(ierrl,"iotk_write_dat",__FILE__,__LINE__)
# 312 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
    goto 1
  end if
1 continue
  if(allocated(dattmp)) deallocate(dattmp)
  if(present(ierr)) then
    ierr = ierrl
  else
    if(ierrl/=0) call iotk_error_handler(ierrl)
  end if
end subroutine iotk_write_dat_REAL4_7


# 668 "iotk_dat.spp"

# 670 "iotk_dat.spp"
subroutine iotk_scan_dat_REAL4_7(unit,name,dat,dummy,attr,found,default,ierr)
  use iotk_base
  use iotk_error_interf
  use iotk_dat_interf, only: iotk_scan_dat_aux
  use iotk_scan_interf
  use iotk_str_interf
  use iotk_misc_interf
  implicit none
  integer, parameter :: this_kind = iotk_REAL4
  integer,                                   intent(in)  :: unit
  character(len=*),                          intent(in)  :: name
#ifdef __IOTK_WORKAROUND6
  REAL(kind=this_kind)                        :: dat (:,:,:,:,:,:,:)
#else
  REAL(kind=this_kind),           intent(out) :: dat (:,:,:,:,:,:,:)
#endif
  type(iotk_dummytype),            optional              :: dummy
#ifdef __IOTK_WORKAROUND6
  character(len=*),                optional              :: attr
#else
  character(len=*),                optional, intent(out) :: attr
#endif
  logical,                         optional, intent(out) :: found
  REAL(kind=this_kind), optional, intent(in)  :: default (:,:,:,:,:,:,:)
  integer,                         optional, intent(out) :: ierr
# 698 "iotk_dat.spp"
  REAL (kind=this_kind),              allocatable :: tmpdat(:)
# 700 "iotk_dat.spp"
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
# 717 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
    goto 1
  end if
  call iotk_parse_dat(lattr,rtype,rkind,rsize,rlen,fmt,columns,ierrl)
! Note that columns is not effectively used
  if(ierrl/=0) goto 1
  if(.not. (iotk_strcomp(rtype,iotk_eos) .or. iotk_strcomp(rtype,"REAL") ) ) then
    call iotk_error_issue(ierrl,"iotk_scan_dat",__FILE__,__LINE__)
# 724 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
# 724 "iotk_dat.spp"
call iotk_error_msg(ierrl,' ')
# 724 "iotk_dat.spp"
call iotk_error_write(ierrl,"rtype",rtype(1:iotk_strlen(rtype)))
# 724 "iotk_dat.spp"
call iotk_error_write(ierrl,"type","REAL")
    goto 1
  end if
  if(.not. (rsize==-1 .or. rsize==size(dat)) ) then
    call iotk_error_issue(ierrl,"iotk_scan_dat",__FILE__,__LINE__)
# 728 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
    goto 1
  end if
  if(rkind==-1) rkind = kind(dat)
# 735 "iotk_dat.spp"

  allocate(tmpdat(size(dat)))
  call iotk_scan_dat_aux(unit,tmpdat,rkind,rlen,fmt(1:iotk_strlen(fmt)),ierrl)
  if(ierrl/=0) then
    call iotk_error_issue(ierrl,"iotk_scan_dat",__FILE__,__LINE__)
# 739 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
# 739 "iotk_dat.spp"
call iotk_error_msg(ierrl,'Error reading data')
# 739 "iotk_dat.spp"
call iotk_error_write(ierrl,"name",name)
# 739 "iotk_dat.spp"
call iotk_error_write(ierrl,"rkind",rkind)
# 739 "iotk_dat.spp"
call iotk_error_write(ierrl,"rlen",rlen)
    goto 1
  end if
# 745 "iotk_dat.spp"
#if defined(__IOTK_WORKAROUND3) || defined(__IOTK_WORKAROUND4)
# 749 "iotk_dat.spp"
     call iotk_private_pack_REAL4(dat,tmpdat,size(tmpdat),1)
# 751 "iotk_dat.spp"
#else
     dat = reshape(tmpdat,shape(dat))
#endif
# 755 "iotk_dat.spp"
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
# 767 "iotk_dat.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.25 ")
# 767 "iotk_dat.spp"
call iotk_error_msg(ierrl,'Dat not found')
# 767 "iotk_dat.spp"
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
end subroutine iotk_scan_dat_REAL4_7


#endif
#endif

subroutine iotk_dat_dummy_REAL4_7
  write(0,*)
end subroutine iotk_dat_dummy_REAL4_7


# 45 "iotk_dat.spp"

