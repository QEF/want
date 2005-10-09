# 1 "iotk_misc.spp"
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

# 28 "iotk_misc.spp"
#include "iotk_auxmacros.h"
# 30 "iotk_misc.spp"

# 33 "iotk_misc.spp"

# 35 "iotk_misc.spp"
subroutine iotk_copy_tag_x(source,dest,dummy,maxsize,ierr)
  use iotk_base
  use iotk_error_interf
  use iotk_attr_interf
  use iotk_scan_interf
  use iotk_write_interf
  use iotk_str_interf
  use iotk_unit_interf
  use iotk_misc_interf
  implicit none
  integer,           intent(in)  :: source
  integer,           intent(in)  :: dest
  type(iotk_dummytype),optional  :: dummy
  integer, optional, intent(in)  :: maxsize
  integer, optional, intent(out) :: ierr
  logical :: source_binary,dest_binary
  integer :: ierrl,control,maxsizel
  character(iotk_taglenx) :: tag
  character(iotk_namlenx) :: name
  character(iotk_attlenx) :: attr
  character(iotk_vallenx) :: type
  type(iotk_unit), pointer :: this
  integer :: taglen           
  logical :: finish
  ierrl = 0         
  maxsizel = -1     
  if(present(maxsize)) maxsizel = maxsize 
  call iotk_inquire(source,binary=source_binary,ierr=ierrl)
  if(ierrl/=0) then
    call iotk_error_issue(ierrl,"iotk_copy_tag",__FILE__,__LINE__)
# 64 "iotk_misc.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.22 ")
    goto 1
  end if
  call iotk_inquire(dest  ,binary=dest_binary,  ierr=ierrl)
  if(ierrl/=0) then
    call iotk_error_issue(ierrl,"iotk_copy_tag",__FILE__,__LINE__)
# 69 "iotk_misc.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.22 ")            
    goto 1
  end if
  call iotk_unit_get(source,pointer=this)
  if(.not.associated(this)) then
    call iotk_error_issue(ierrl,"iotk_copy_tag",__FILE__,__LINE__)
# 74 "iotk_misc.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.22 ")
# 74 "iotk_misc.spp"
call iotk_error_msg(ierrl,'unit')  
    goto 1
  end if
  do
    call iotk_scan_tag(source,+1,control,tag,source_binary,ierrl)
    if(ierrl/=0) then
      call iotk_error_issue(ierrl,"iotk_copy_tag",__FILE__,__LINE__)
# 80 "iotk_misc.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.22 ")
      goto 1
    end if
    if(control/=4) then ! SKIP FOR COMMENTS
      call iotk_tag_parse(tag,name,attr,ierrl)
      if(ierrl/=0) then
        call iotk_error_issue(ierrl,"iotk_copy_tag",__FILE__,__LINE__)
# 86 "iotk_misc.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.22 ")
        goto 1
      end if
    end if
    if(iotk_strcomp(name,this%root)) then
      call iotk_scan_tag(source,-1,control,tag,source_binary,ierrl)
      if(ierrl/=0) then
        call iotk_error_issue(ierrl,"iotk_copy_tag",__FILE__,__LINE__)
# 93 "iotk_misc.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.22 ")
        goto 1
      end if
      return
    end if
    select case(control)
    case(1)
      call iotk_scan_attr(attr,"type",type,ierr=ierrl,eos=.true.,default=" ")
      if(ierrl/=0) then
        call iotk_error_issue(ierrl,"iotk_copy_tag",__FILE__,__LINE__)
# 102 "iotk_misc.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.22 ")
        goto 1
      end if
      if((iotk_strcomp(type,"real") .or. iotk_strcomp(type,"integer") .or. iotk_strcomp(type,"logical") &
                                    .or. iotk_strcomp(type,"character") .or. iotk_strcomp(type,"complex")) .and. control==1) then
        call iotk_copy_dat(source,dest,source_binary,dest_binary,name,attr,maxsize=maxsizel,ierr=ierrl)
        if(ierrl/=0) then
          call iotk_error_issue(ierrl,"iotk_copy_tag",__FILE__,__LINE__)
# 109 "iotk_misc.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.22 ")
          goto 1
        end if
        call iotk_scan_tag(source,+1,control,tag,source_binary,ierrl)
      else
        call iotk_write_begin(dest,name,attr,ierr=ierrl)
      end if
    case(2)
      call iotk_write_end(dest,name,ierr=ierrl)
    case(3)
      call iotk_write_empty(dest,name,attr,ierr=ierrl)
    case(4)
      call iotk_write_comment(dest,tag,ierr=ierrl)
    case(5)
      call iotk_write_pi(dest,name,attr,ierr=ierrl)
    end select
    if(ierrl/=0) then
      call iotk_error_issue(ierrl,"iotk_copy_tag",__FILE__,__LINE__)
# 126 "iotk_misc.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.22 ")
      goto 1
    end if
  end do
1 continue
  if(present(ierr)) then
    ierr = ierrl
  else
    if(ierrl/=0) call iotk_error_handler(ierrl)
  end if
end subroutine iotk_copy_tag_x

# 139 "iotk_misc.spp"
subroutine iotk_parse_dat_x(attr,type,ikind,isize,ilen,fmt,columns,ierr)
  use iotk_base
  use iotk_error_interf
  use iotk_attr_interf
  use iotk_scan_interf
  use iotk_str_interf
  use iotk_misc_interf
  implicit none
  character(len=*), intent(in)  :: attr
#ifdef __WORKAROUND6
  character(len=*)              :: type
#else
  character(len=*), intent(out) :: type
#endif
  integer,          intent(out) :: ikind
  integer,          intent(out) :: isize
  integer,          intent(out) :: ilen
#ifdef __WORKAROUND6
  character(len=*)              :: fmt
#else
  character(len=*), intent(out) :: fmt
#endif
  integer,          intent(out) :: columns
  integer,          intent(out) :: ierr
  character(iotk_vallenx) :: typename
  integer :: typelen
  ierr = 0
  call iotk_scan_attr(attr,"type",typename,ierr=ierr,eos=.true.,default=iotk_eos)
  if(ierr/=0) then
    call iotk_error_issue(ierr,"iotk_parse_dat",__FILE__,__LINE__)
# 168 "iotk_misc.spp"
call iotk_error_msg(ierr,"CVS Revision: 1.22 ")
    return
  end if
  typelen = iotk_strlen(typename)
  type = iotk_toupper(typename)
  call iotk_scan_attr(attr,"kind",ikind,ierr=ierr,default=-1)
  if(ierr/=0) then
    call iotk_error_issue(ierr,"iotk_parse_dat",__FILE__,__LINE__)
# 175 "iotk_misc.spp"
call iotk_error_msg(ierr,"CVS Revision: 1.22 ")
    return
  end if
  call iotk_scan_attr(attr,"size",isize,ierr=ierr,default=-1)
  if(ierr/=0) then
    call iotk_error_issue(ierr,"iotk_parse_dat",__FILE__,__LINE__)
# 180 "iotk_misc.spp"
call iotk_error_msg(ierr,"CVS Revision: 1.22 ")
    return
  end if
  call iotk_scan_attr(attr,"len", ilen, ierr=ierr,default=-1)
  if(ierr/=0) then
    call iotk_error_issue(ierr,"iotk_parse_dat",__FILE__,__LINE__)
# 185 "iotk_misc.spp"
call iotk_error_msg(ierr,"CVS Revision: 1.22 ")
    return
  end if
  call iotk_scan_attr(attr,"fmt", fmt, ierr=ierr,eos=.true.,default="!"//iotk_eos)
  if(ierr/=0) then
    call iotk_error_issue(ierr,"iotk_parse_dat",__FILE__,__LINE__)
# 190 "iotk_misc.spp"
call iotk_error_msg(ierr,"CVS Revision: 1.22 ")
    return
  end if
  call iotk_scan_attr(attr,"columns",columns,ierr=ierr,default=1)
  if(ierr/=0) then
    call iotk_error_issue(ierr,"iotk_parse_dat",__FILE__,__LINE__)
# 195 "iotk_misc.spp"
call iotk_error_msg(ierr,"CVS Revision: 1.22 ")
    return
  end if
end subroutine iotk_parse_dat_x

# 201 "iotk_misc.spp"
subroutine iotk_set_options_x(dummy,unitmin,unitmax,getline_buffer,error_warn_overflow,ierr)
  use iotk_base
  use iotk_error_interf
  use iotk_misc_interf
  implicit none
  type(iotk_dummytype),optional  :: dummy
  integer, optional, intent(in) :: unitmin
  integer, optional, intent(in) :: unitmax
  integer, optional, intent(in) :: getline_buffer
  logical, optional, intent(in) :: error_warn_overflow
  integer, optional, intent(out):: ierr
  integer :: ierrl
  ierrl = 0
  if(present(error_warn_overflow)) then
    iotk_error_warn_overflow = error_warn_overflow
  end if
  if(present(unitmin)) then
    if(unitmin<0) then
      call iotk_error_issue(ierrl,"iotk_set_options",__FILE__,__LINE__)
# 219 "iotk_misc.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.22 ")
      goto 1 
    end if
    iotk_unitmin = unitmin 
  end if
  if(present(unitmax)) then
    if(unitmax<iotk_unitmin) then
      call iotk_error_issue(ierrl,"iotk_set_options",__FILE__,__LINE__)
# 226 "iotk_misc.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.22 ")
      goto 1
    end if
    iotk_unitmax = unitmax
  end if
  if(present(getline_buffer)) then
    if(getline_buffer<1) then
      call iotk_error_issue(ierrl,"iotk_set_options",__FILE__,__LINE__)
# 233 "iotk_misc.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.22 ")
      goto 1
    end if
    iotk_getline_buffer = getline_buffer
  end if
1 continue 
  if(present(ierr)) then
    ierr = ierrl
  else
    if(ierrl/=0) call iotk_error_handler(ierrl)
  end if
end subroutine iotk_set_options_x

# 247 "iotk_misc.spp"
subroutine iotk_get_options_x(dummy,unitmin,unitmax,getline_buffer,error_warn_overflow)
  use iotk_base
  use iotk_misc_interf
  implicit none
  type(iotk_dummytype),optional  :: dummy
  integer, optional, intent(out):: unitmin
  integer, optional, intent(out):: unitmax
  integer, optional, intent(out):: getline_buffer
  logical, optional, intent(out):: error_warn_overflow
  if(present(unitmin)) unitmin = iotk_unitmin
  if(present(unitmax)) unitmax = iotk_unitmax
  if(present(unitmax)) getline_buffer = iotk_getline_buffer
  if(present(error_warn_overflow)) error_warn_overflow = iotk_error_warn_overflow
end subroutine iotk_get_options_x

# 263 "iotk_misc.spp"
subroutine iotk_print_kinds_x
  use iotk_base
  use iotk_misc_interf
  use iotk_xtox_interf
  implicit none
  character(100) :: string
  write(*,"(a,i5)") "Maximum rank            : ", iotk_maxrank
  write(*,"(a,i5)") "Maximum rank hard limit : ", iotk_maxrank
# 272 "iotk_misc.spp"
#ifdef __IOTK_LOGICAL1
  string = "logical(kind="//trim(iotk_itoa(__IOTK_LOGICAL1))//")"
  write(*,"(a)") trim(string)
#endif
# 272 "iotk_misc.spp"
#ifdef __IOTK_LOGICAL2
  string = "logical(kind="//trim(iotk_itoa(__IOTK_LOGICAL2))//")"
  write(*,"(a)") trim(string)
#endif
# 272 "iotk_misc.spp"
#ifdef __IOTK_LOGICAL3
  string = "logical(kind="//trim(iotk_itoa(__IOTK_LOGICAL3))//")"
  write(*,"(a)") trim(string)
#endif
# 272 "iotk_misc.spp"
#ifdef __IOTK_LOGICAL4
  string = "logical(kind="//trim(iotk_itoa(__IOTK_LOGICAL4))//")"
  write(*,"(a)") trim(string)
#endif
# 278 "iotk_misc.spp"
#ifdef __IOTK_INTEGER1
  string = "integer(kind="//trim(iotk_itoa(__IOTK_INTEGER1))//")"
  write(*,"(a)") trim(string)
#endif
# 278 "iotk_misc.spp"
#ifdef __IOTK_INTEGER2
  string = "integer(kind="//trim(iotk_itoa(__IOTK_INTEGER2))//")"
  write(*,"(a)") trim(string)
#endif
# 278 "iotk_misc.spp"
#ifdef __IOTK_INTEGER3
  string = "integer(kind="//trim(iotk_itoa(__IOTK_INTEGER3))//")"
  write(*,"(a)") trim(string)
#endif
# 278 "iotk_misc.spp"
#ifdef __IOTK_INTEGER4
  string = "integer(kind="//trim(iotk_itoa(__IOTK_INTEGER4))//")"
  write(*,"(a)") trim(string)
#endif
# 284 "iotk_misc.spp"
#ifdef __IOTK_REAL1
  string = "real(kind="//trim(iotk_itoa(__IOTK_REAL1))//")"
  write(*,"(a)") trim(string)
#endif
# 284 "iotk_misc.spp"
#ifdef __IOTK_REAL2
  string = "real(kind="//trim(iotk_itoa(__IOTK_REAL2))//")"
  write(*,"(a)") trim(string)
#endif
# 284 "iotk_misc.spp"
#ifdef __IOTK_REAL3
  string = "real(kind="//trim(iotk_itoa(__IOTK_REAL3))//")"
  write(*,"(a)") trim(string)
#endif
# 284 "iotk_misc.spp"
#ifdef __IOTK_REAL4
  string = "real(kind="//trim(iotk_itoa(__IOTK_REAL4))//")"
  write(*,"(a)") trim(string)
#endif
# 289 "iotk_misc.spp"
  string = "character(kind="//trim(iotk_itoa(__IOTK_CHARACTER1))//")"
  write(*,"(a)") trim(string)
end subroutine iotk_print_kinds_x


# 295 "iotk_misc.spp"
subroutine iotk_copy_dat_aux_x(source,dest,source_binary,dest_binary,name,type,ikind,isize, &
                               ilen,fmt,columns,attr,ierr)
  use iotk_base
  use iotk_error_interf
  use iotk_dat_interf
  use iotk_scan_interf
  use iotk_str_interf
  use iotk_misc_interf
  implicit none
  integer,      intent(in)  :: source
  integer,      intent(in)  :: dest
  logical,      intent(in)  :: source_binary
  logical,      intent(in)  :: dest_binary
  character(*), intent(in)  :: name
  character(*), intent(in)  :: type
  integer,      intent(in)  :: ikind
  integer,      intent(in)  :: isize
  integer,      intent(in)  :: ilen
  character(*), intent(in)  :: fmt
  integer,      intent(in)  :: columns
  character(*), intent(in)  :: attr
  integer,      intent(out) :: ierr
  
  integer :: tmpkind
# 322 "iotk_misc.spp"
#ifdef __IOTK_LOGICAL1
# 326 "iotk_misc.spp"
  LOGICAL (kind=__IOTK_LOGICAL1), allocatable :: dat_LOGICAL1 (:)
# 328 "iotk_misc.spp"
#endif
# 322 "iotk_misc.spp"
#ifdef __IOTK_LOGICAL2
# 326 "iotk_misc.spp"
  LOGICAL (kind=__IOTK_LOGICAL2), allocatable :: dat_LOGICAL2 (:)
# 328 "iotk_misc.spp"
#endif
# 322 "iotk_misc.spp"
#ifdef __IOTK_LOGICAL3
# 326 "iotk_misc.spp"
  LOGICAL (kind=__IOTK_LOGICAL3), allocatable :: dat_LOGICAL3 (:)
# 328 "iotk_misc.spp"
#endif
# 322 "iotk_misc.spp"
#ifdef __IOTK_LOGICAL4
# 326 "iotk_misc.spp"
  LOGICAL (kind=__IOTK_LOGICAL4), allocatable :: dat_LOGICAL4 (:)
# 328 "iotk_misc.spp"
#endif
# 322 "iotk_misc.spp"
#ifdef __IOTK_INTEGER1
# 326 "iotk_misc.spp"
  INTEGER (kind=__IOTK_INTEGER1), allocatable :: dat_INTEGER1 (:)
# 328 "iotk_misc.spp"
#endif
# 322 "iotk_misc.spp"
#ifdef __IOTK_INTEGER2
# 326 "iotk_misc.spp"
  INTEGER (kind=__IOTK_INTEGER2), allocatable :: dat_INTEGER2 (:)
# 328 "iotk_misc.spp"
#endif
# 322 "iotk_misc.spp"
#ifdef __IOTK_INTEGER3
# 326 "iotk_misc.spp"
  INTEGER (kind=__IOTK_INTEGER3), allocatable :: dat_INTEGER3 (:)
# 328 "iotk_misc.spp"
#endif
# 322 "iotk_misc.spp"
#ifdef __IOTK_INTEGER4
# 326 "iotk_misc.spp"
  INTEGER (kind=__IOTK_INTEGER4), allocatable :: dat_INTEGER4 (:)
# 328 "iotk_misc.spp"
#endif
# 322 "iotk_misc.spp"
#ifdef __IOTK_REAL1
# 326 "iotk_misc.spp"
  REAL (kind=__IOTK_REAL1), allocatable :: dat_REAL1 (:)
# 328 "iotk_misc.spp"
#endif
# 322 "iotk_misc.spp"
#ifdef __IOTK_REAL2
# 326 "iotk_misc.spp"
  REAL (kind=__IOTK_REAL2), allocatable :: dat_REAL2 (:)
# 328 "iotk_misc.spp"
#endif
# 322 "iotk_misc.spp"
#ifdef __IOTK_REAL3
# 326 "iotk_misc.spp"
  REAL (kind=__IOTK_REAL3), allocatable :: dat_REAL3 (:)
# 328 "iotk_misc.spp"
#endif
# 322 "iotk_misc.spp"
#ifdef __IOTK_REAL4
# 326 "iotk_misc.spp"
  REAL (kind=__IOTK_REAL4), allocatable :: dat_REAL4 (:)
# 328 "iotk_misc.spp"
#endif
# 322 "iotk_misc.spp"
#ifdef __IOTK_COMPLEX1
# 326 "iotk_misc.spp"
  COMPLEX (kind=__IOTK_COMPLEX1), allocatable :: dat_COMPLEX1 (:)
# 328 "iotk_misc.spp"
#endif
# 322 "iotk_misc.spp"
#ifdef __IOTK_COMPLEX2
# 326 "iotk_misc.spp"
  COMPLEX (kind=__IOTK_COMPLEX2), allocatable :: dat_COMPLEX2 (:)
# 328 "iotk_misc.spp"
#endif
# 322 "iotk_misc.spp"
#ifdef __IOTK_COMPLEX3
# 326 "iotk_misc.spp"
  COMPLEX (kind=__IOTK_COMPLEX3), allocatable :: dat_COMPLEX3 (:)
# 328 "iotk_misc.spp"
#endif
# 322 "iotk_misc.spp"
#ifdef __IOTK_COMPLEX4
# 326 "iotk_misc.spp"
  COMPLEX (kind=__IOTK_COMPLEX4), allocatable :: dat_COMPLEX4 (:)
# 328 "iotk_misc.spp"
#endif
# 322 "iotk_misc.spp"
#ifdef __IOTK_CHARACTER1
# 324 "iotk_misc.spp"
  CHARACTER (kind=__IOTK_CHARACTER1,len=ilen), allocatable :: dat_CHARACTER1 (:)
# 328 "iotk_misc.spp"
#endif
# 332 "iotk_misc.spp"

! la regola e' semplice
! SE SOURCE E' BINARIO: usa il kind di source
! SE SOURCE E' TESTUALE: use il kind di default se e' definito
!                        altrimenti usa il primo kind disponibile
! ad ogni modo, il kind e' calcolato run-time, dunque in futuro lo si potrebbe
! chiedere all'utente
  ierr=0
  select case(type(1:iotk_strlen(type)))
# 342 "iotk_misc.spp"
  case("LOGICAL")
# 346 "iotk_misc.spp"
    if(source_binary) then
      tmpkind=ikind
    else
      tmpkind=0
# 351 "iotk_misc.spp"
#ifdef __IOTK_LOGICAL1
      if(tmpkind==0) tmpkind=__IOTK_LOGICAL1
      if(__IOTK_LOGICAL1 == iotk_defkind_LOGICAL) then
        tmpkind=iotk_defkind_LOGICAL
      end if
#endif
# 351 "iotk_misc.spp"
#ifdef __IOTK_LOGICAL2
      if(tmpkind==0) tmpkind=__IOTK_LOGICAL2
      if(__IOTK_LOGICAL2 == iotk_defkind_LOGICAL) then
        tmpkind=iotk_defkind_LOGICAL
      end if
#endif
# 351 "iotk_misc.spp"
#ifdef __IOTK_LOGICAL3
      if(tmpkind==0) tmpkind=__IOTK_LOGICAL3
      if(__IOTK_LOGICAL3 == iotk_defkind_LOGICAL) then
        tmpkind=iotk_defkind_LOGICAL
      end if
#endif
# 351 "iotk_misc.spp"
#ifdef __IOTK_LOGICAL4
      if(tmpkind==0) tmpkind=__IOTK_LOGICAL4
      if(__IOTK_LOGICAL4 == iotk_defkind_LOGICAL) then
        tmpkind=iotk_defkind_LOGICAL
      end if
#endif
# 358 "iotk_misc.spp"
    end if
# 360 "iotk_misc.spp"
    select case(tmpkind)
# 363 "iotk_misc.spp"
#ifdef __IOTK_LOGICAL1
    case(__IOTK_LOGICAL1)
      allocate(dat_LOGICAL1(isize))
      call iotk_scan_dat_aux(source,dat_LOGICAL1,ikind,ilen,fmt,ierr)
      if(ierr==0) call iotk_write_dat(dest,name,dat_LOGICAL1,attr=attr,ierr=ierr,fmt=fmt,columns=columns)
      deallocate(dat_LOGICAL1)
#endif
# 363 "iotk_misc.spp"
#ifdef __IOTK_LOGICAL2
    case(__IOTK_LOGICAL2)
      allocate(dat_LOGICAL2(isize))
      call iotk_scan_dat_aux(source,dat_LOGICAL2,ikind,ilen,fmt,ierr)
      if(ierr==0) call iotk_write_dat(dest,name,dat_LOGICAL2,attr=attr,ierr=ierr,fmt=fmt,columns=columns)
      deallocate(dat_LOGICAL2)
#endif
# 363 "iotk_misc.spp"
#ifdef __IOTK_LOGICAL3
    case(__IOTK_LOGICAL3)
      allocate(dat_LOGICAL3(isize))
      call iotk_scan_dat_aux(source,dat_LOGICAL3,ikind,ilen,fmt,ierr)
      if(ierr==0) call iotk_write_dat(dest,name,dat_LOGICAL3,attr=attr,ierr=ierr,fmt=fmt,columns=columns)
      deallocate(dat_LOGICAL3)
#endif
# 363 "iotk_misc.spp"
#ifdef __IOTK_LOGICAL4
    case(__IOTK_LOGICAL4)
      allocate(dat_LOGICAL4(isize))
      call iotk_scan_dat_aux(source,dat_LOGICAL4,ikind,ilen,fmt,ierr)
      if(ierr==0) call iotk_write_dat(dest,name,dat_LOGICAL4,attr=attr,ierr=ierr,fmt=fmt,columns=columns)
      deallocate(dat_LOGICAL4)
#endif
# 372 "iotk_misc.spp"
    case default
      call iotk_error_issue(ierr,"iotk_copy_dat_aux",__FILE__,__LINE__)
# 373 "iotk_misc.spp"
call iotk_error_msg(ierr,"CVS Revision: 1.22 ")
# 373 "iotk_misc.spp"
call iotk_error_msg(ierr,'internal error')
    end select
# 342 "iotk_misc.spp"
  case("INTEGER")
# 346 "iotk_misc.spp"
    if(source_binary) then
      tmpkind=ikind
    else
      tmpkind=0
# 351 "iotk_misc.spp"
#ifdef __IOTK_INTEGER1
      if(tmpkind==0) tmpkind=__IOTK_INTEGER1
      if(__IOTK_INTEGER1 == iotk_defkind_INTEGER) then
        tmpkind=iotk_defkind_INTEGER
      end if
#endif
# 351 "iotk_misc.spp"
#ifdef __IOTK_INTEGER2
      if(tmpkind==0) tmpkind=__IOTK_INTEGER2
      if(__IOTK_INTEGER2 == iotk_defkind_INTEGER) then
        tmpkind=iotk_defkind_INTEGER
      end if
#endif
# 351 "iotk_misc.spp"
#ifdef __IOTK_INTEGER3
      if(tmpkind==0) tmpkind=__IOTK_INTEGER3
      if(__IOTK_INTEGER3 == iotk_defkind_INTEGER) then
        tmpkind=iotk_defkind_INTEGER
      end if
#endif
# 351 "iotk_misc.spp"
#ifdef __IOTK_INTEGER4
      if(tmpkind==0) tmpkind=__IOTK_INTEGER4
      if(__IOTK_INTEGER4 == iotk_defkind_INTEGER) then
        tmpkind=iotk_defkind_INTEGER
      end if
#endif
# 358 "iotk_misc.spp"
    end if
# 360 "iotk_misc.spp"
    select case(tmpkind)
# 363 "iotk_misc.spp"
#ifdef __IOTK_INTEGER1
    case(__IOTK_INTEGER1)
      allocate(dat_INTEGER1(isize))
      call iotk_scan_dat_aux(source,dat_INTEGER1,ikind,ilen,fmt,ierr)
      if(ierr==0) call iotk_write_dat(dest,name,dat_INTEGER1,attr=attr,ierr=ierr,fmt=fmt,columns=columns)
      deallocate(dat_INTEGER1)
#endif
# 363 "iotk_misc.spp"
#ifdef __IOTK_INTEGER2
    case(__IOTK_INTEGER2)
      allocate(dat_INTEGER2(isize))
      call iotk_scan_dat_aux(source,dat_INTEGER2,ikind,ilen,fmt,ierr)
      if(ierr==0) call iotk_write_dat(dest,name,dat_INTEGER2,attr=attr,ierr=ierr,fmt=fmt,columns=columns)
      deallocate(dat_INTEGER2)
#endif
# 363 "iotk_misc.spp"
#ifdef __IOTK_INTEGER3
    case(__IOTK_INTEGER3)
      allocate(dat_INTEGER3(isize))
      call iotk_scan_dat_aux(source,dat_INTEGER3,ikind,ilen,fmt,ierr)
      if(ierr==0) call iotk_write_dat(dest,name,dat_INTEGER3,attr=attr,ierr=ierr,fmt=fmt,columns=columns)
      deallocate(dat_INTEGER3)
#endif
# 363 "iotk_misc.spp"
#ifdef __IOTK_INTEGER4
    case(__IOTK_INTEGER4)
      allocate(dat_INTEGER4(isize))
      call iotk_scan_dat_aux(source,dat_INTEGER4,ikind,ilen,fmt,ierr)
      if(ierr==0) call iotk_write_dat(dest,name,dat_INTEGER4,attr=attr,ierr=ierr,fmt=fmt,columns=columns)
      deallocate(dat_INTEGER4)
#endif
# 372 "iotk_misc.spp"
    case default
      call iotk_error_issue(ierr,"iotk_copy_dat_aux",__FILE__,__LINE__)
# 373 "iotk_misc.spp"
call iotk_error_msg(ierr,"CVS Revision: 1.22 ")
# 373 "iotk_misc.spp"
call iotk_error_msg(ierr,'internal error')
    end select
# 342 "iotk_misc.spp"
  case("REAL")
# 346 "iotk_misc.spp"
    if(source_binary) then
      tmpkind=ikind
    else
      tmpkind=0
# 351 "iotk_misc.spp"
#ifdef __IOTK_REAL1
      if(tmpkind==0) tmpkind=__IOTK_REAL1
      if(__IOTK_REAL1 == iotk_defkind_REAL) then
        tmpkind=iotk_defkind_REAL
      end if
#endif
# 351 "iotk_misc.spp"
#ifdef __IOTK_REAL2
      if(tmpkind==0) tmpkind=__IOTK_REAL2
      if(__IOTK_REAL2 == iotk_defkind_REAL) then
        tmpkind=iotk_defkind_REAL
      end if
#endif
# 351 "iotk_misc.spp"
#ifdef __IOTK_REAL3
      if(tmpkind==0) tmpkind=__IOTK_REAL3
      if(__IOTK_REAL3 == iotk_defkind_REAL) then
        tmpkind=iotk_defkind_REAL
      end if
#endif
# 351 "iotk_misc.spp"
#ifdef __IOTK_REAL4
      if(tmpkind==0) tmpkind=__IOTK_REAL4
      if(__IOTK_REAL4 == iotk_defkind_REAL) then
        tmpkind=iotk_defkind_REAL
      end if
#endif
# 358 "iotk_misc.spp"
    end if
# 360 "iotk_misc.spp"
    select case(tmpkind)
# 363 "iotk_misc.spp"
#ifdef __IOTK_REAL1
    case(__IOTK_REAL1)
      allocate(dat_REAL1(isize))
      call iotk_scan_dat_aux(source,dat_REAL1,ikind,ilen,fmt,ierr)
      if(ierr==0) call iotk_write_dat(dest,name,dat_REAL1,attr=attr,ierr=ierr,fmt=fmt,columns=columns)
      deallocate(dat_REAL1)
#endif
# 363 "iotk_misc.spp"
#ifdef __IOTK_REAL2
    case(__IOTK_REAL2)
      allocate(dat_REAL2(isize))
      call iotk_scan_dat_aux(source,dat_REAL2,ikind,ilen,fmt,ierr)
      if(ierr==0) call iotk_write_dat(dest,name,dat_REAL2,attr=attr,ierr=ierr,fmt=fmt,columns=columns)
      deallocate(dat_REAL2)
#endif
# 363 "iotk_misc.spp"
#ifdef __IOTK_REAL3
    case(__IOTK_REAL3)
      allocate(dat_REAL3(isize))
      call iotk_scan_dat_aux(source,dat_REAL3,ikind,ilen,fmt,ierr)
      if(ierr==0) call iotk_write_dat(dest,name,dat_REAL3,attr=attr,ierr=ierr,fmt=fmt,columns=columns)
      deallocate(dat_REAL3)
#endif
# 363 "iotk_misc.spp"
#ifdef __IOTK_REAL4
    case(__IOTK_REAL4)
      allocate(dat_REAL4(isize))
      call iotk_scan_dat_aux(source,dat_REAL4,ikind,ilen,fmt,ierr)
      if(ierr==0) call iotk_write_dat(dest,name,dat_REAL4,attr=attr,ierr=ierr,fmt=fmt,columns=columns)
      deallocate(dat_REAL4)
#endif
# 372 "iotk_misc.spp"
    case default
      call iotk_error_issue(ierr,"iotk_copy_dat_aux",__FILE__,__LINE__)
# 373 "iotk_misc.spp"
call iotk_error_msg(ierr,"CVS Revision: 1.22 ")
# 373 "iotk_misc.spp"
call iotk_error_msg(ierr,'internal error')
    end select
# 342 "iotk_misc.spp"
  case("COMPLEX")
# 346 "iotk_misc.spp"
    if(source_binary) then
      tmpkind=ikind
    else
      tmpkind=0
# 351 "iotk_misc.spp"
#ifdef __IOTK_COMPLEX1
      if(tmpkind==0) tmpkind=__IOTK_COMPLEX1
      if(__IOTK_COMPLEX1 == iotk_defkind_COMPLEX) then
        tmpkind=iotk_defkind_COMPLEX
      end if
#endif
# 351 "iotk_misc.spp"
#ifdef __IOTK_COMPLEX2
      if(tmpkind==0) tmpkind=__IOTK_COMPLEX2
      if(__IOTK_COMPLEX2 == iotk_defkind_COMPLEX) then
        tmpkind=iotk_defkind_COMPLEX
      end if
#endif
# 351 "iotk_misc.spp"
#ifdef __IOTK_COMPLEX3
      if(tmpkind==0) tmpkind=__IOTK_COMPLEX3
      if(__IOTK_COMPLEX3 == iotk_defkind_COMPLEX) then
        tmpkind=iotk_defkind_COMPLEX
      end if
#endif
# 351 "iotk_misc.spp"
#ifdef __IOTK_COMPLEX4
      if(tmpkind==0) tmpkind=__IOTK_COMPLEX4
      if(__IOTK_COMPLEX4 == iotk_defkind_COMPLEX) then
        tmpkind=iotk_defkind_COMPLEX
      end if
#endif
# 358 "iotk_misc.spp"
    end if
# 360 "iotk_misc.spp"
    select case(tmpkind)
# 363 "iotk_misc.spp"
#ifdef __IOTK_COMPLEX1
    case(__IOTK_COMPLEX1)
      allocate(dat_COMPLEX1(isize))
      call iotk_scan_dat_aux(source,dat_COMPLEX1,ikind,ilen,fmt,ierr)
      if(ierr==0) call iotk_write_dat(dest,name,dat_COMPLEX1,attr=attr,ierr=ierr,fmt=fmt,columns=columns)
      deallocate(dat_COMPLEX1)
#endif
# 363 "iotk_misc.spp"
#ifdef __IOTK_COMPLEX2
    case(__IOTK_COMPLEX2)
      allocate(dat_COMPLEX2(isize))
      call iotk_scan_dat_aux(source,dat_COMPLEX2,ikind,ilen,fmt,ierr)
      if(ierr==0) call iotk_write_dat(dest,name,dat_COMPLEX2,attr=attr,ierr=ierr,fmt=fmt,columns=columns)
      deallocate(dat_COMPLEX2)
#endif
# 363 "iotk_misc.spp"
#ifdef __IOTK_COMPLEX3
    case(__IOTK_COMPLEX3)
      allocate(dat_COMPLEX3(isize))
      call iotk_scan_dat_aux(source,dat_COMPLEX3,ikind,ilen,fmt,ierr)
      if(ierr==0) call iotk_write_dat(dest,name,dat_COMPLEX3,attr=attr,ierr=ierr,fmt=fmt,columns=columns)
      deallocate(dat_COMPLEX3)
#endif
# 363 "iotk_misc.spp"
#ifdef __IOTK_COMPLEX4
    case(__IOTK_COMPLEX4)
      allocate(dat_COMPLEX4(isize))
      call iotk_scan_dat_aux(source,dat_COMPLEX4,ikind,ilen,fmt,ierr)
      if(ierr==0) call iotk_write_dat(dest,name,dat_COMPLEX4,attr=attr,ierr=ierr,fmt=fmt,columns=columns)
      deallocate(dat_COMPLEX4)
#endif
# 372 "iotk_misc.spp"
    case default
      call iotk_error_issue(ierr,"iotk_copy_dat_aux",__FILE__,__LINE__)
# 373 "iotk_misc.spp"
call iotk_error_msg(ierr,"CVS Revision: 1.22 ")
# 373 "iotk_misc.spp"
call iotk_error_msg(ierr,'internal error')
    end select
# 342 "iotk_misc.spp"
  case("CHARACTER")
# 344 "iotk_misc.spp"
    tmpkind=iotk_defkind_CHARACTER
# 360 "iotk_misc.spp"
    select case(tmpkind)
# 363 "iotk_misc.spp"
#ifdef __IOTK_CHARACTER1
    case(__IOTK_CHARACTER1)
      allocate(dat_CHARACTER1(isize))
      call iotk_scan_dat_aux(source,dat_CHARACTER1,ikind,ilen,fmt,ierr)
      if(ierr==0) call iotk_write_dat(dest,name,dat_CHARACTER1,attr=attr,ierr=ierr,fmt=fmt,columns=columns)
      deallocate(dat_CHARACTER1)
#endif
# 372 "iotk_misc.spp"
    case default
      call iotk_error_issue(ierr,"iotk_copy_dat_aux",__FILE__,__LINE__)
# 373 "iotk_misc.spp"
call iotk_error_msg(ierr,"CVS Revision: 1.22 ")
# 373 "iotk_misc.spp"
call iotk_error_msg(ierr,'internal error')
    end select
# 376 "iotk_misc.spp"
  case default
    call iotk_error_issue(ierr,"iotk_copy_dat_aux",__FILE__,__LINE__)
# 377 "iotk_misc.spp"
call iotk_error_msg(ierr,"CVS Revision: 1.22 ")
# 377 "iotk_misc.spp"
call iotk_error_msg(ierr,'internal error')
  end select
  
end subroutine iotk_copy_dat_aux_x


# 384 "iotk_misc.spp"
subroutine iotk_copy_dat_x(source,dest,source_binary,dest_binary,name,attr,maxsize,ierr)
  use iotk_base
  use iotk_error_interf
  use iotk_attr_interf
  use iotk_write_interf
  use iotk_str_interf
  use iotk_misc_interf
  implicit none
  integer,      intent(in)  :: source
  integer,      intent(in)  :: dest
  logical,      intent(in)  :: source_binary
  logical,      intent(in)  :: dest_binary
  character(*), intent(in)  :: name
  character(*), intent(in)  :: attr
  integer,      intent(in)  :: maxsize
  integer,      intent(out) :: ierr
  character(9) :: type
  integer :: ikind,isize,ilen,columns
  character(iotk_vallenx) :: fmt
  character(iotk_attlenx) :: attr1
  ierr = 0
  call iotk_parse_dat(attr,type,ikind,isize,ilen,fmt,columns,ierr)
  if(ierr/=0) then
    call iotk_error_issue(ierr,"iotk_copy_dat",__FILE__,__LINE__)
# 407 "iotk_misc.spp"
call iotk_error_msg(ierr,"CVS Revision: 1.22 ")
    return
  end if
  if(iotk_strcomp(type,iotk_eos)) then
    call iotk_error_issue(ierr,"iotk_copy_dat",__FILE__,__LINE__)
# 411 "iotk_misc.spp"
call iotk_error_msg(ierr,"CVS Revision: 1.22 ")
    return
  end if
  if(isize==-1) then
    call iotk_error_issue(ierr,"iotk_copy_dat",__FILE__,__LINE__)
# 415 "iotk_misc.spp"
call iotk_error_msg(ierr,"CVS Revision: 1.22 ")
    return
  end if
  if(ilen==-1 .and. iotk_strcomp(type,"CHARACTER")) then
    call iotk_error_issue(ierr,"iotk_copy_dat",__FILE__,__LINE__)
# 419 "iotk_misc.spp"
call iotk_error_msg(ierr,"CVS Revision: 1.22 ")
    return
  end if
  if(isize<=maxsize .or. maxsize==-1 .or. dest_binary) then
    call iotk_copy_dat_aux(source,dest,source_binary,dest_binary,name,type,ikind,isize, &
                           ilen,fmt,columns,attr,ierr)
    if(ierr/=0) then
      call iotk_error_issue(ierr,"iotk_copy_dat",__FILE__,__LINE__)
# 426 "iotk_misc.spp"
call iotk_error_msg(ierr,"CVS Revision: 1.22 ")
      return
    end if  
  else    
    call iotk_strcpy(attr1,attr,ierr=ierr)
    if(ierr/=0) then
      call iotk_error_issue(ierr,"iotk_copy_dat",__FILE__,__LINE__)
# 432 "iotk_misc.spp"
call iotk_error_msg(ierr,"CVS Revision: 1.22 ")
      return
    end if
    call iotk_write_attr (attr1,"trunc",.true.,ierr=ierr)
    if(ierr/=0) then
      call iotk_error_issue(ierr,"iotk_copy_dat",__FILE__,__LINE__)
# 437 "iotk_misc.spp"
call iotk_error_msg(ierr,"CVS Revision: 1.22 ")
      return
    end if
    call iotk_write_empty(dest,name,attr=attr1,ierr=ierr)
    if(ierr/=0) then
      call iotk_error_issue(ierr,"iotk_copy_dat",__FILE__,__LINE__)
# 442 "iotk_misc.spp"
call iotk_error_msg(ierr,"CVS Revision: 1.22 ")
      return
    end if
  end if
end subroutine iotk_copy_dat_x

# 449 "iotk_misc.spp"
subroutine iotk_check_iotk_attr_x(unit,attr,ierr)
  use iotk_base
  use iotk_error_interf
  use iotk_attr_interf
  use iotk_scan_interf
  use iotk_str_interf
  use iotk_xtox_interf
  use iotk_unit_interf
  use iotk_misc_interf
  implicit none
  integer,                 intent(in)  :: unit
  character(iotk_attlenx), intent(in)  :: attr
  integer,                 intent(out) :: ierr
  character(iotk_vallenx) :: version,file_version
  logical :: binary,rbinary,check,found
  integer :: pos1,pos2,attlen,itmp_major,itmp_minor
  ierr = 0
  call iotk_scan_attr(attr,"file_version",file_version,eos=.true.,ierr=ierr,found=found)
  if(ierr/=0) then
    call iotk_error_issue(ierr,"iotk_check_iotk_attr",__FILE__,__LINE__)
# 468 "iotk_misc.spp"
call iotk_error_msg(ierr,"CVS Revision: 1.22 ")
    return
  end if
  if(found) then
    attlen = iotk_strlen(file_version)
    pos1   = iotk_strscan(file_version,".")
    if(pos1<=1 .or. pos1>=attlen) then
      call iotk_error_issue(ierr,"iotk_check_iotk_attr",__FILE__,__LINE__)
# 475 "iotk_misc.spp"
call iotk_error_msg(ierr,"CVS Revision: 1.22 ")
# 475 "iotk_misc.spp"
call iotk_error_msg(ierr,'Problems reading file version')
# 475 "iotk_misc.spp"
call iotk_error_write(ierr,"file_version",file_version)
# 475 "iotk_misc.spp"
call iotk_error_write(ierr,"attlen",attlen)
# 475 "iotk_misc.spp"
call iotk_error_write(ierr,"pos1",pos1)
      return
    end if
    pos2   = pos1 + verify(file_version(pos1+1:attlen),numbers)
    if(pos2==pos1+1) then
      call iotk_error_issue(ierr,"iotk_check_iotk_attr",__FILE__,__LINE__)
# 480 "iotk_misc.spp"
call iotk_error_msg(ierr,"CVS Revision: 1.22 ")
# 480 "iotk_misc.spp"
call iotk_error_msg(ierr,'Problems reading file version')
# 480 "iotk_misc.spp"
call iotk_error_write(ierr,"file_version",file_version)
# 480 "iotk_misc.spp"
call iotk_error_write(ierr,"attlen",attlen)
# 480 "iotk_misc.spp"
call iotk_error_write(ierr,"pos1",pos1)
# 480 "iotk_misc.spp"
call iotk_error_write(ierr,"pos2",pos2)
      return
    end if
    if(pos2==pos1) pos2 = attlen+1
    call iotk_atoi(itmp_major,file_version(1:pos1-1),check)
    if(.not.check) then
      call iotk_error_issue(ierr,"iotk_check_iotk_attr",__FILE__,__LINE__)
# 486 "iotk_misc.spp"
call iotk_error_msg(ierr,"CVS Revision: 1.22 ")
# 486 "iotk_misc.spp"
call iotk_error_msg(ierr,'Problems reading file version')
# 486 "iotk_misc.spp"
call iotk_error_write(ierr,"file_version",file_version)
      return
    end if
    call iotk_atoi(itmp_minor,file_version(pos1+1:pos2-1),check)
    if(.not.check) then
      call iotk_error_issue(ierr,"iotk_check_iotk_attr",__FILE__,__LINE__)
# 491 "iotk_misc.spp"
call iotk_error_msg(ierr,"CVS Revision: 1.22 ")
# 491 "iotk_misc.spp"
call iotk_error_msg(ierr,'Problems reading file version')
# 491 "iotk_misc.spp"
call iotk_error_write(ierr,"file_version",file_version)
      return
    end if
    if(itmp_major > iotk_file_version_major .or. &
      (itmp_major==iotk_file_version_major .and. itmp_minor > iotk_file_version_minor) ) then
      call iotk_error_issue(ierr,"iotk_check_iotk_attr",__FILE__,__LINE__)
# 496 "iotk_misc.spp"
call iotk_error_msg(ierr,"CVS Revision: 1.22 ")
# 496 "iotk_misc.spp"
call iotk_error_msg(ierr,'File version is newer than internal version')
# 496 "iotk_misc.spp"
call iotk_error_write(ierr,"file_version",file_version)
# 496 "iotk_misc.spp"
call iotk_error_write(ierr,"internal_version",iotk_file_version)
      return
    end if
  end if
  call iotk_scan_attr(attr,"binary",rbinary,ierr=ierr,found=found)
  if(ierr/=0) then
    call iotk_error_issue(ierr,"iotk_check_iotk_attr",__FILE__,__LINE__)
# 502 "iotk_misc.spp"
call iotk_error_msg(ierr,"CVS Revision: 1.22 ")
    return
  end if
  if(found) then
    call iotk_inquire(unit,binary,ierr=ierr)
    if(ierr/=0) then
      call iotk_error_issue(ierr,"iotk_check_iotk_attr",__FILE__,__LINE__)
# 508 "iotk_misc.spp"
call iotk_error_msg(ierr,"CVS Revision: 1.22 ")
      return
    end if
    if(rbinary .neqv. binary) then
      call iotk_error_issue(ierr,"iotk_check_iotk_attr",__FILE__,__LINE__)
# 512 "iotk_misc.spp"
call iotk_error_msg(ierr,"CVS Revision: 1.22 ")
      return
    end if
  end if
end subroutine iotk_check_iotk_attr_x

# 519 "iotk_misc.spp"
function iotk_index_scal(index)
  use iotk_base
  use iotk_xtox_interf
  use iotk_misc_interf
  integer,           intent(in) :: index
  character(len=range(index)+3) :: iotk_index_scal
  iotk_index_scal="."//iotk_itoa(index)
end function iotk_index_scal
  
# 529 "iotk_misc.spp"
function iotk_index_vec(index)
  use iotk_base
  use iotk_xtox_interf
  use iotk_misc_interf
  implicit none
  integer,                         intent(in) :: index(:)
  character(len=(range(index)+3)*size(index)) :: iotk_index_vec
  integer :: length,i
  length = 0
  iotk_index_vec = " "
  do i = 1,size(index)
    iotk_index_vec(length+1:length+1+(range(index)+3)) = "."//iotk_itoa(index(i))
    length = len_trim(iotk_index_vec)
  end do
end function iotk_index_vec


# 547 "iotk_misc.spp"
subroutine iotk_tag_parse_x(tag,name,attr,ierr)
  use iotk_base
  use iotk_error_interf
  use iotk_misc_interf
  use iotk_str_interf
  implicit none
  character(iotk_taglenx), intent(in)  :: tag
  character(iotk_namlenx), intent(out) :: name
  character(iotk_attlenx), intent(out) :: attr
  integer,                 intent(out) :: ierr
  integer :: pos,lenatt,lentag
  ierr = 0
  lentag=iotk_strlen(tag)
  if(verify(tag(1:1),iotk_namcharfirst)/=0) then
    call iotk_error_issue(ierr,"iotk_tag_parse",__FILE__,__LINE__)
# 561 "iotk_misc.spp"
call iotk_error_msg(ierr,"CVS Revision: 1.22 ")
# 561 "iotk_misc.spp"
call iotk_error_msg(ierr,'Wrong syntax in tag')
    call iotk_error_write(ierr,"tag",tag(1:lentag))
    return
  end if
  pos = scan(tag(1:lentag)," ")
  if(pos==0) pos=lentag+1
  if(pos>len(name)+1) then
    call iotk_error_issue(ierr,"iotk_tag_parse",__FILE__,__LINE__)
# 568 "iotk_misc.spp"
call iotk_error_msg(ierr,"CVS Revision: 1.22 ")
# 568 "iotk_misc.spp"
call iotk_error_msg(ierr,'Tag name too long')
    return
   end if
  name = tag(1:pos-1)
  if(pos<=len(name)) name(pos:pos) = iotk_eos
  lenatt = len_trim(tag(pos:lentag))
  if(lenatt>iotk_attlenx) then
    call iotk_error_issue(ierr,"iotk_tag_parse",__FILE__,__LINE__)
# 575 "iotk_misc.spp"
call iotk_error_msg(ierr,"CVS Revision: 1.22 ")
# 575 "iotk_misc.spp"
call iotk_error_msg(ierr,'Attribute string too long')
    return
  end if
  if(lenatt>0) then
    attr(1:lenatt) = tag(pos:pos+lenatt-1)
    if(lenatt+1<=len(attr)) attr(lenatt+1:lenatt+1)=iotk_eos
  else
    attr(1:1)=iotk_eos
  end if
end subroutine iotk_tag_parse_x

# 587 "iotk_misc.spp"
function iotk_complete_filepath_x(newfile,oldfile)
  use iotk_base
  use iotk_misc_interf
  implicit none
  character(len=*), intent(in) :: newfile
  character(len=*), intent(in) :: oldfile
  character(len=len(newfile)+len(oldfile)) :: iotk_complete_filepath_x
  character(len=len(oldfile)) :: prefix
  integer :: pos
  if(newfile(1:1)=="/") then
    iotk_complete_filepath_x = newfile
  else
    pos = scan(oldfile,"/",back=.true.)
    prefix = " "
    if(pos>0) prefix = oldfile(1:pos)
    iotk_complete_filepath_x = trim(prefix)//trim(newfile)
  end if
end function iotk_complete_filepath_x

# 607 "iotk_misc.spp"
function iotk_check_name_x(name)
  use iotk_base
  use iotk_misc_interf
  use iotk_str_interf
  implicit none
  character(len=*), intent(in) :: name
  logical                      :: iotk_check_name_x
! Checks a single name
  integer :: len_name
  iotk_check_name_x = .true.
  len_name = iotk_strlen_trim(name)
  if(len_name>iotk_namlenx) iotk_check_name_x = .false.
  if(verify(name(1:1),iotk_namcharfirst)/=0) iotk_check_name_x = .false.
  if(len_name>1) then
    if(verify(name(2:len_name),iotk_namchar)/=0) iotk_check_name_x = .false.
  end if
end function iotk_check_name_x

# 626 "iotk_misc.spp"
subroutine iotk_delete_attr_x(attr,name,ierr)
  use iotk_base
  use iotk_str_interf
  use iotk_error_interf
  implicit none
  character(len=*), intent(inout) :: attr
  character(len=*), intent(in)    :: name
  integer,          intent(out)   :: ierr
  integer :: i,attlen,pos,equal,begin
  logical :: foundl
  character :: delim
  ierr = 0
  attlen=iotk_strlen(attr)
  foundl = .false.
  equal = 0
  do
    pos = verify(attr(equal+1:attlen)," ")
    if(pos<=0) exit
    equal = equal + pos
    pos = scan(attr(equal+1:attlen),"=")
    if(pos<=0) then
      call iotk_error_issue(ierr,"iotk_delete_attr",__FILE__,__LINE__)
# 647 "iotk_misc.spp"
call iotk_error_msg(ierr,"CVS Revision: 1.22 ")
# 647 "iotk_misc.spp"
call iotk_error_msg(ierr,'')
# 647 "iotk_misc.spp"
call iotk_error_write(ierr,"attr",attr)
# 647 "iotk_misc.spp"
call iotk_error_write(ierr,"equal",equal)
      return
    end if
    if(trim(attr(equal:equal+pos-1))==trim(name)) foundl = .true.
    begin = equal
    equal = equal + pos
    pos   = verify(attr(equal+1:attlen)," ")
    if(pos<=0) then
      call iotk_error_issue(ierr,"iotk_delete_attr",__FILE__,__LINE__)
# 655 "iotk_misc.spp"
call iotk_error_msg(ierr,"CVS Revision: 1.22 ")
      return
    end if
    equal = equal + pos
    delim = attr(equal:equal)
    if(delim/="'" .and. delim/='"') then
      call iotk_error_issue(ierr,"iotk_delete_attr",__FILE__,__LINE__)
# 661 "iotk_misc.spp"
call iotk_error_msg(ierr,"CVS Revision: 1.22 ")
# 661 "iotk_misc.spp"
call iotk_error_msg(ierr,'delim')
      return
    end if
    pos = scan(attr(equal+1:attlen),delim)
    if(pos<=0) then
      call iotk_error_issue(ierr,"iotk_delete_attr",__FILE__,__LINE__)
# 666 "iotk_misc.spp"
call iotk_error_msg(ierr,"CVS Revision: 1.22 ")
      return
    end if
    equal = equal + pos
    if(foundl) exit
  end do
  if(foundl) then
    if(equal<attlen) then
      pos = verify(attr(equal+1:attlen)," ")
      if(pos==0) then
        equal=attlen
      else
        equal=equal+pos-1
      end if
    end if
    if(equal<attlen) then
      attr(begin:begin+attlen-equal) = attr(equal+1:attlen)//iotk_eos
    else
      attr(begin:begin)=iotk_eos
    end if
  end if
end subroutine iotk_delete_attr_x

