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

# 30 "iotk_external.spp"

# 33 "iotk_external.spp"

# 35 "iotk_external.spp"
subroutine iotk_copy_tag_x(source,dest,maxsize,ierr)
  use iotk_base
  use iotk_interface
  implicit none
  integer,           intent(in)  :: source
  integer,           intent(in)  :: dest
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
# 57 "iotk_external.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.16 ")
    goto 1
  end if
  call iotk_inquire(dest  ,binary=dest_binary,  ierr=ierrl)
  if(ierrl/=0) then
    call iotk_error_issue(ierrl,"iotk_copy_tag",__FILE__,__LINE__)
# 62 "iotk_external.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.16 ")            
    goto 1
  end if
  call iotk_unit_get(source,pointer=this)
  if(.not.associated(this)) then
    call iotk_error_issue(ierrl,"iotk_copy_tag",__FILE__,__LINE__)
# 67 "iotk_external.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.16 ")
# 67 "iotk_external.spp"
call iotk_error_msg(ierrl,'unit')  
    goto 1
  end if
  do
    call iotk_scan_tag(source,+1,control,tag,source_binary,ierrl)
    if(ierrl/=0) then
      call iotk_error_issue(ierrl,"iotk_copy_tag",__FILE__,__LINE__)
# 73 "iotk_external.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.16 ")
      goto 1
    end if
    call iotk_tag_parse(tag,name,attr,ierrl)
    if(ierrl/=0) then
      call iotk_error_issue(ierrl,"iotk_copy_tag",__FILE__,__LINE__)
# 78 "iotk_external.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.16 ")
      goto 1
    end if
    if(iotk_strcomp(name,this%root)) then
      call iotk_scan_tag(source,-1,control,tag,source_binary,ierrl)
      if(ierrl/=0) then
        call iotk_error_issue(ierrl,"iotk_copy_tag",__FILE__,__LINE__)
# 84 "iotk_external.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.16 ")
        goto 1
      end if
      return
    end if
    call iotk_scan_attr(attr,"type",type,ierr=ierrl,eos=.true.,default=" ")
    if(ierrl<0) call iotk_error_clear(ierrl)
    if(ierrl/=0) then
      call iotk_error_issue(ierrl,"iotk_copy_tag",__FILE__,__LINE__)
# 92 "iotk_external.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.16 ")
      goto 1
    end if
    if((iotk_strcomp(type,"real") .or. iotk_strcomp(type,"integer") .or. iotk_strcomp(type,"logical") &
                                  .or. iotk_strcomp(type,"character") .or. iotk_strcomp(type,"complex")) .and. control==1) then
      call iotk_copy_dat(source,dest,source_binary,dest_binary,name,attr,maxsize=maxsizel,ierr=ierrl)
      if(ierrl/=0) then
        call iotk_error_issue(ierrl,"iotk_copy_tag",__FILE__,__LINE__)
# 99 "iotk_external.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.16 ")
        goto 1
      end if
      call iotk_scan_tag(source,+1,control,tag,source_binary,ierrl)
      if(ierrl/=0) then
        call iotk_error_issue(ierrl,"iotk_copy_tag",__FILE__,__LINE__)
# 104 "iotk_external.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.16 ")
        goto 1
      end if
    else
      call iotk_write_tag(dest,control,tag,dest_binary,ierrl)
      if(ierrl/=0) then
        call iotk_error_issue(ierrl,"iotk_copy_tag",__FILE__,__LINE__)
# 110 "iotk_external.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.16 ")
        goto 1
      end if
    end if
  end do
1 continue
  if(present(ierr)) then
    ierr = ierrl
  else
    if(ierrl/=0) call iotk_error_handler(ierrl)
  end if
end subroutine iotk_copy_tag_x

# 124 "iotk_external.spp"
subroutine iotk_parse_dat_x(attr,type,ikind,isize,ilen,fmt,ierr)
  use iotk_base
  use iotk_interface
  implicit none
  character(*), intent(in)  :: attr
  character(*), intent(out) :: type
  integer,      intent(out) :: ikind
  integer,      intent(out) :: isize
  integer,      intent(out) :: ilen
  character(*), intent(out) :: fmt
  integer,      intent(out) :: ierr
  character(iotk_vallenx) :: typename
  integer :: typelen
  ierr = 0
  call iotk_scan_attr(attr,"type",typename,ierr=ierr,eos=.true.,default=iotk_eos)
  if(ierr<0) call iotk_error_clear(ierr)
  if(ierr/=0) then
    call iotk_error_issue(ierr,"iotk_parse_dat",__FILE__,__LINE__)
# 141 "iotk_external.spp"
call iotk_error_msg(ierr,"CVS Revision: 1.16 ")
    return
  end if
  typelen = iotk_strlen(typename)
  type = iotk_toupper(typename)
  call iotk_scan_attr(attr,"kind",ikind,ierr=ierr,default=-1)
  if(ierr<0) call iotk_error_clear(ierr)
  if(ierr/=0) then
    call iotk_error_issue(ierr,"iotk_parse_dat",__FILE__,__LINE__)
# 149 "iotk_external.spp"
call iotk_error_msg(ierr,"CVS Revision: 1.16 ")
    return
  end if
  call iotk_scan_attr(attr,"size",isize,ierr=ierr,default=-1)
  if(ierr<0) call iotk_error_clear(ierr)
  if(ierr/=0) then
    call iotk_error_issue(ierr,"iotk_parse_dat",__FILE__,__LINE__)
# 155 "iotk_external.spp"
call iotk_error_msg(ierr,"CVS Revision: 1.16 ")
    return
  end if
  call iotk_scan_attr(attr,"len", ilen, ierr=ierr,default=-1)
  if(ierr<0) call iotk_error_clear(ierr)
  if(ierr/=0) then
    call iotk_error_issue(ierr,"iotk_parse_dat",__FILE__,__LINE__)
# 161 "iotk_external.spp"
call iotk_error_msg(ierr,"CVS Revision: 1.16 ")
    return
  end if
  call iotk_scan_attr(attr,"fmt", fmt, ierr=ierr,eos=.true.,default="!"//iotk_eos)
  if(ierr<0) call iotk_error_clear(ierr)
  if(ierr/=0) then
    call iotk_error_issue(ierr,"iotk_parse_dat",__FILE__,__LINE__)
# 167 "iotk_external.spp"
call iotk_error_msg(ierr,"CVS Revision: 1.16 ")
    return
  end if
end subroutine iotk_parse_dat_x

# 173 "iotk_external.spp"
subroutine iotk_set_options_x(unitmin,unitmax,getline_buffer,error_warn_overflow,ierr)
  use iotk_base
  use iotk_interface
  implicit none
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
# 189 "iotk_external.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.16 ")
      goto 1 
    end if
    iotk_unitmin = unitmin 
  end if
  if(present(unitmax)) then
    if(unitmax<iotk_unitmin) then
      call iotk_error_issue(ierrl,"iotk_set_options",__FILE__,__LINE__)
# 196 "iotk_external.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.16 ")
      goto 1
    end if
    iotk_unitmax = unitmax
  end if
  if(present(getline_buffer)) then
    if(getline_buffer<1) then
      call iotk_error_issue(ierrl,"iotk_set_options",__FILE__,__LINE__)
# 203 "iotk_external.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.16 ")
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

# 217 "iotk_external.spp"
subroutine iotk_get_options_x(unitmin,unitmax,getline_buffer,error_warn_overflow)
  use iotk_base
  use iotk_interface
  implicit none
  integer, optional, intent(out):: unitmin
  integer, optional, intent(out):: unitmax
  integer, optional, intent(out):: getline_buffer
  logical, optional, intent(out):: error_warn_overflow
  if(present(unitmin)) unitmin = iotk_unitmin
  if(present(unitmax)) unitmax = iotk_unitmax
  if(present(unitmax)) getline_buffer = iotk_getline_buffer
  if(present(error_warn_overflow)) error_warn_overflow = iotk_error_warn_overflow
end subroutine iotk_get_options_x

# 232 "iotk_external.spp"
subroutine iotk_print_kinds_x
  use iotk_base
  use iotk_interface
  implicit none
  character(100) :: string
# 238 "iotk_external.spp"
#ifdef __IOTK_LOGICAL1
  string = "logical(kind="//trim(iotk_itoa(__IOTK_LOGICAL1))//")"
  write(*,"(a)") trim(string)
#endif
# 238 "iotk_external.spp"
#ifdef __IOTK_LOGICAL2
  string = "logical(kind="//trim(iotk_itoa(__IOTK_LOGICAL2))//")"
  write(*,"(a)") trim(string)
#endif
# 238 "iotk_external.spp"
#ifdef __IOTK_LOGICAL3
  string = "logical(kind="//trim(iotk_itoa(__IOTK_LOGICAL3))//")"
  write(*,"(a)") trim(string)
#endif
# 238 "iotk_external.spp"
#ifdef __IOTK_LOGICAL4
  string = "logical(kind="//trim(iotk_itoa(__IOTK_LOGICAL4))//")"
  write(*,"(a)") trim(string)
#endif
# 244 "iotk_external.spp"
#ifdef __IOTK_INTEGER1
  string = "integer(kind="//trim(iotk_itoa(__IOTK_INTEGER1))//")"
  write(*,"(a)") trim(string)
#endif
# 244 "iotk_external.spp"
#ifdef __IOTK_INTEGER2
  string = "integer(kind="//trim(iotk_itoa(__IOTK_INTEGER2))//")"
  write(*,"(a)") trim(string)
#endif
# 244 "iotk_external.spp"
#ifdef __IOTK_INTEGER3
  string = "integer(kind="//trim(iotk_itoa(__IOTK_INTEGER3))//")"
  write(*,"(a)") trim(string)
#endif
# 244 "iotk_external.spp"
#ifdef __IOTK_INTEGER4
  string = "integer(kind="//trim(iotk_itoa(__IOTK_INTEGER4))//")"
  write(*,"(a)") trim(string)
#endif
# 250 "iotk_external.spp"
#ifdef __IOTK_REAL1
  string = "real(kind="//trim(iotk_itoa(__IOTK_REAL1))//")"
  write(*,"(a)") trim(string)
#endif
# 250 "iotk_external.spp"
#ifdef __IOTK_REAL2
  string = "real(kind="//trim(iotk_itoa(__IOTK_REAL2))//")"
  write(*,"(a)") trim(string)
#endif
# 250 "iotk_external.spp"
#ifdef __IOTK_REAL3
  string = "real(kind="//trim(iotk_itoa(__IOTK_REAL3))//")"
  write(*,"(a)") trim(string)
#endif
# 250 "iotk_external.spp"
#ifdef __IOTK_REAL4
  string = "real(kind="//trim(iotk_itoa(__IOTK_REAL4))//")"
  write(*,"(a)") trim(string)
#endif
# 255 "iotk_external.spp"
  string = "character(kind="//trim(iotk_itoa(__IOTK_CHARACTER1))//")"
  write(*,"(a)") trim(string)
end subroutine iotk_print_kinds_x


# 261 "iotk_external.spp"
subroutine iotk_copy_dat_aux_x(source,dest,source_binary,dest_binary,name,type,ikind,isize,ilen,fmt,ierr)
  use iotk_base
  use iotk_interface
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
  integer,      intent(out) :: ierr
  
  logical  (kind=iotk_defkind_LOGICAL),           allocatable :: dat_lo(:)
  integer  (kind=iotk_defkind_INTEGER),           allocatable :: dat_in(:)
  real     (kind=iotk_defkind_REAL),              allocatable :: dat_re(:)
  complex  (kind=iotk_defkind_COMPLEX),           allocatable :: dat_co(:)
  character(kind=iotk_defkind_CHARACTER,len=ilen), allocatable :: dat_ch(:)
  
  ierr = 0
  select case(type(1:iotk_strlen(type)))
  case("LOGICAL")
    allocate(dat_lo(isize))
    call iotk_scan_dat_aux(source,dat_lo,ikind,ilen,fmt,ierr)
    if(ierr/=0) then
      call iotk_error_issue(ierr,"iotk_copy_dat_aux",__FILE__,__LINE__)
# 289 "iotk_external.spp"
call iotk_error_msg(ierr,"CVS Revision: 1.16 ")
      return
    end if
    call iotk_write_dat(dest,name,dat_lo,ierr=ierr,fmt=fmt)
    if(ierr/=0) then
      call iotk_error_issue(ierr,"iotk_copy_dat_aux",__FILE__,__LINE__)
# 294 "iotk_external.spp"
call iotk_error_msg(ierr,"CVS Revision: 1.16 ")
      return
    end if
    deallocate(dat_lo)
  case("INTEGER")
    allocate(dat_in(isize))
    call iotk_scan_dat_aux(source,dat_in,ikind,ilen,fmt,ierr)
    if(ierr/=0) then
      call iotk_error_issue(ierr,"iotk_copy_dat_aux",__FILE__,__LINE__)
# 302 "iotk_external.spp"
call iotk_error_msg(ierr,"CVS Revision: 1.16 ")
      return
    end if
    call iotk_write_dat(dest,name,dat_in,ierr=ierr,fmt=fmt)
    if(ierr/=0) then
      call iotk_error_issue(ierr,"iotk_copy_dat_aux",__FILE__,__LINE__)
# 307 "iotk_external.spp"
call iotk_error_msg(ierr,"CVS Revision: 1.16 ")
      return
    end if
    deallocate(dat_in)
  case("REAL")
    allocate(dat_re(isize))
    call iotk_scan_dat_aux(source,dat_re,ikind,ilen,fmt,ierr)
    if(ierr/=0) then
      call iotk_error_issue(ierr,"iotk_copy_dat_aux",__FILE__,__LINE__)
# 315 "iotk_external.spp"
call iotk_error_msg(ierr,"CVS Revision: 1.16 ")
      return
    end if
    call iotk_write_dat(dest,name,dat_re,ierr=ierr,fmt=fmt)
    if(ierr/=0) then
      call iotk_error_issue(ierr,"iotk_copy_dat_aux",__FILE__,__LINE__)
# 320 "iotk_external.spp"
call iotk_error_msg(ierr,"CVS Revision: 1.16 ")
      return
    end if
    deallocate(dat_re)
  case("COMPLEX")
    allocate(dat_co(isize))
    call iotk_scan_dat_aux(source,dat_co,ikind,ilen,fmt,ierr)
    if(ierr/=0) then
      call iotk_error_issue(ierr,"iotk_copy_dat_aux",__FILE__,__LINE__)
# 328 "iotk_external.spp"
call iotk_error_msg(ierr,"CVS Revision: 1.16 ")
      return
    end if
    call iotk_write_dat(dest,name,dat_co,ierr=ierr,fmt=fmt)
    if(ierr/=0) then
      call iotk_error_issue(ierr,"iotk_copy_dat_aux",__FILE__,__LINE__)
# 333 "iotk_external.spp"
call iotk_error_msg(ierr,"CVS Revision: 1.16 ")
      return
    end if
    deallocate(dat_co)
  case("CHARACTER")
    allocate(dat_ch(isize))
    call iotk_scan_dat_aux(source,dat_ch,ikind,ilen,fmt,ierr)
    if(ierr/=0) then
      call iotk_error_issue(ierr,"iotk_copy_dat_aux",__FILE__,__LINE__)
# 341 "iotk_external.spp"
call iotk_error_msg(ierr,"CVS Revision: 1.16 ")
      return
    end if
    call iotk_write_dat(dest,name,dat_ch,ierr=ierr,fmt=fmt)
    if(ierr/=0) then
      call iotk_error_issue(ierr,"iotk_copy_dat_aux",__FILE__,__LINE__)
# 346 "iotk_external.spp"
call iotk_error_msg(ierr,"CVS Revision: 1.16 ")
      return
    end if
    deallocate(dat_ch)
  end select
end subroutine iotk_copy_dat_aux_x


# 355 "iotk_external.spp"
subroutine iotk_copy_dat_x(source,dest,source_binary,dest_binary,name,attr,maxsize,ierr)
  use iotk_base
  use iotk_interface
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
  integer :: ikind,isize,ilen
  character(iotk_vallenx) :: fmt
  character(iotk_attlenx) :: attr1
  ierr = 0
  call iotk_parse_dat(attr,type,ikind,isize,ilen,fmt,ierr)
  if(ierr/=0) then
    call iotk_error_issue(ierr,"iotk_copy_dat",__FILE__,__LINE__)
# 374 "iotk_external.spp"
call iotk_error_msg(ierr,"CVS Revision: 1.16 ")
    return
  end if
  if(iotk_strcomp(type,iotk_eos)) then
    call iotk_error_issue(ierr,"iotk_copy_dat",__FILE__,__LINE__)
# 378 "iotk_external.spp"
call iotk_error_msg(ierr,"CVS Revision: 1.16 ")
    return
  end if
  if(isize==-1) then
    call iotk_error_issue(ierr,"iotk_copy_dat",__FILE__,__LINE__)
# 382 "iotk_external.spp"
call iotk_error_msg(ierr,"CVS Revision: 1.16 ")
    return
  end if
  if(ilen==-1 .and. iotk_strcomp(type,"CHARACTER")) then
    call iotk_error_issue(ierr,"iotk_copy_dat",__FILE__,__LINE__)
# 386 "iotk_external.spp"
call iotk_error_msg(ierr,"CVS Revision: 1.16 ")
    return
  end if
  if(isize<=maxsize .or. maxsize==-1 .or. dest_binary) then
    call iotk_copy_dat_aux(source,dest,source_binary,dest_binary,name,type,ikind,isize,ilen,fmt,ierr)
    if(ierr/=0) then
      call iotk_error_issue(ierr,"iotk_copy_dat",__FILE__,__LINE__)
# 392 "iotk_external.spp"
call iotk_error_msg(ierr,"CVS Revision: 1.16 ")
      return
    end if  
  else    
    call iotk_strcpy(attr1,attr,ierr=ierr)
    if(ierr/=0) then
      call iotk_error_issue(ierr,"iotk_copy_dat",__FILE__,__LINE__)
# 398 "iotk_external.spp"
call iotk_error_msg(ierr,"CVS Revision: 1.16 ")
      return
    end if
    call iotk_write_attr (attr1,"trunc",.true.,ierr=ierr)
    if(ierr/=0) then
      call iotk_error_issue(ierr,"iotk_copy_dat",__FILE__,__LINE__)
# 403 "iotk_external.spp"
call iotk_error_msg(ierr,"CVS Revision: 1.16 ")
      return
    end if
    call iotk_write_empty(dest,name,attr=attr1,ierr=ierr)
    if(ierr/=0) then
      call iotk_error_issue(ierr,"iotk_copy_dat",__FILE__,__LINE__)
# 408 "iotk_external.spp"
call iotk_error_msg(ierr,"CVS Revision: 1.16 ")
      return
    end if
  end if
end subroutine iotk_copy_dat_x

# 415 "iotk_external.spp"
subroutine iotk_check_iotk_attr_x(unit,attr,ierr)
  use iotk_base
  use iotk_interface
  implicit none
  integer,                 intent(in)  :: unit
  character(iotk_attlenx), intent(in)  :: attr
  integer,                 intent(out) :: ierr
  character(iotk_vallenx) :: version,binary_format,file_version
  logical :: binary,rbinary,check
  integer :: pos1,pos2,attlen,itmp_major,itmp_minor
  ierr = 0
  call iotk_scan_attr(attr,"file_version",file_version,eos=.true.,ierr=ierr)
  if(ierr>0) then
    call iotk_error_issue(ierr,"iotk_check_iotk_attr",__FILE__,__LINE__)
# 428 "iotk_external.spp"
call iotk_error_msg(ierr,"CVS Revision: 1.16 ")
    return
  else if(ierr<0) then
    call iotk_error_clear(ierr)
  else
    attlen = iotk_strlen(file_version)
    pos1   = iotk_strscan(file_version,".")
    if(pos1<=1 .or. pos1>=attlen) then
      call iotk_error_issue(ierr,"iotk_check_iotk_attr",__FILE__,__LINE__)
# 436 "iotk_external.spp"
call iotk_error_msg(ierr,"CVS Revision: 1.16 ")
# 436 "iotk_external.spp"
call iotk_error_msg(ierr,'Problems reading file version')
# 436 "iotk_external.spp"
call iotk_error_write(ierr,"file_version",file_version(1:attlen))
# 436 "iotk_external.spp"
call iotk_error_write(ierr,"attlen",attlen)
# 436 "iotk_external.spp"
call iotk_error_write(ierr,"pos1",pos1)
      return
    end if
    pos2   = pos1 + verify(file_version(pos1+1:attlen),numbers)
    if(pos2==pos1+1) then
      call iotk_error_issue(ierr,"iotk_check_iotk_attr",__FILE__,__LINE__)
# 441 "iotk_external.spp"
call iotk_error_msg(ierr,"CVS Revision: 1.16 ")
# 441 "iotk_external.spp"
call iotk_error_msg(ierr,'Problems reading file version')
# 441 "iotk_external.spp"
call iotk_error_write(ierr,"file_version",file_version(1:attlen))
# 441 "iotk_external.spp"
call iotk_error_write(ierr,"attlen",attlen)
# 441 "iotk_external.spp"
call iotk_error_write(ierr,"pos1",pos1)
# 441 "iotk_external.spp"
call iotk_error_write(ierr,"pos2",pos2)
      return
    end if
    if(pos2==pos1) pos2 = attlen+1
    call iotk_atoi(itmp_major,file_version(1:pos1-1),check)
    if(.not.check) then
      call iotk_error_issue(ierr,"iotk_check_iotk_attr",__FILE__,__LINE__)
# 447 "iotk_external.spp"
call iotk_error_msg(ierr,"CVS Revision: 1.16 ")
# 447 "iotk_external.spp"
call iotk_error_msg(ierr,'Problems reading file version')
# 447 "iotk_external.spp"
call iotk_error_write(ierr,"file_version",file_version(1:attlen))
      return
    end if
    call iotk_atoi(itmp_minor,file_version(pos1+1:pos2-1),check)
    if(.not.check) then
      call iotk_error_issue(ierr,"iotk_check_iotk_attr",__FILE__,__LINE__)
# 452 "iotk_external.spp"
call iotk_error_msg(ierr,"CVS Revision: 1.16 ")
# 452 "iotk_external.spp"
call iotk_error_msg(ierr,'Problems reading file version')
# 452 "iotk_external.spp"
call iotk_error_write(ierr,"file_version",file_version(1:attlen))
      return
    end if
    if(itmp_major > iotk_file_version_major .or. &
      (itmp_major==iotk_file_version_major .and. itmp_minor > iotk_file_version_minor) ) then
      call iotk_error_issue(ierr,"iotk_check_iotk_attr",__FILE__,__LINE__)
# 457 "iotk_external.spp"
call iotk_error_msg(ierr,"CVS Revision: 1.16 ")
# 457 "iotk_external.spp"
call iotk_error_msg(ierr,'File version is newer than internal version')
# 457 "iotk_external.spp"
call iotk_error_write(ierr,"file_version",file_version(1:attlen))
# 457 "iotk_external.spp"
call iotk_error_write(ierr,"internal_version",iotk_file_version)
      return
    end if
  end if
  call iotk_scan_attr(attr,"binary",rbinary,ierr=ierr)
  if(ierr>0) then
    call iotk_error_issue(ierr,"iotk_check_iotk_attr",__FILE__,__LINE__)
# 463 "iotk_external.spp"
call iotk_error_msg(ierr,"CVS Revision: 1.16 ")
    return
  else if(ierr<0) then
    call iotk_error_clear(ierr)
  else
    call iotk_inquire(unit,binary,ierr=ierr)
    if(ierr/=0) then
      call iotk_error_issue(ierr,"iotk_check_iotk_attr",__FILE__,__LINE__)
# 470 "iotk_external.spp"
call iotk_error_msg(ierr,"CVS Revision: 1.16 ")
      return
    end if
    if(rbinary .neqv. binary) then
      call iotk_error_issue(ierr,"iotk_check_iotk_attr",__FILE__,__LINE__)
# 474 "iotk_external.spp"
call iotk_error_msg(ierr,"CVS Revision: 1.16 ")
      return
    end if
  end if
  call iotk_scan_attr(attr,"binary_format",binary_format,ierr=ierr)
  if(ierr>0) then
    call iotk_error_issue(ierr,"iotk_check_iotk_attr",__FILE__,__LINE__)
# 480 "iotk_external.spp"
call iotk_error_msg(ierr,"CVS Revision: 1.16 ")
    return
  else if(ierr<0) then
    call iotk_error_clear(ierr)
  else
    if(binary_format/=iotk_binary_format .and. binary_format/="*" .and. iotk_binary_format/="*") then
      call iotk_error_issue(ierr,"iotk_check_iotk_attr",__FILE__,__LINE__)
# 486 "iotk_external.spp"
call iotk_error_msg(ierr,"CVS Revision: 1.16 ")
# 486 "iotk_external.spp"
call iotk_error_msg(ierr,'Binary format is not compatible')
      return
    end if
  end if
end subroutine iotk_check_iotk_attr_x

# 493 "iotk_external.spp"
function iotk_index_scal(index)
  use iotk_base
  use iotk_interface
  integer,           intent(in) :: index
  character(len=range(index)+3) :: iotk_index_scal
  iotk_index_scal="."//iotk_itoa(index)
end function iotk_index_scal
  
# 502 "iotk_external.spp"
function iotk_index_vec(index)
  use iotk_base
  use iotk_interface
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


# 519 "iotk_external.spp"
subroutine iotk_tag_parse_x(tag,name,attr,ierr)
  use iotk_base
  use iotk_interface
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
# 531 "iotk_external.spp"
call iotk_error_msg(ierr,"CVS Revision: 1.16 ")
# 531 "iotk_external.spp"
call iotk_error_msg(ierr,'Wrong syntax in tag')
    call iotk_error_write(ierr,"tag",tag(1:lentag))
    return
  end if
  pos = scan(tag(1:lentag)," ")
  if(pos==0) pos=lentag+1
  if(pos>len(name)+1) then
    call iotk_error_issue(ierr,"iotk_tag_parse",__FILE__,__LINE__)
# 538 "iotk_external.spp"
call iotk_error_msg(ierr,"CVS Revision: 1.16 ")
# 538 "iotk_external.spp"
call iotk_error_msg(ierr,'Tag name too long')
    return
   end if
  name = tag(1:pos-1)
  if(pos<=len(name)) name(pos:pos) = iotk_eos
  lenatt = len_trim(tag(pos:lentag))
  if(lenatt>iotk_attlenx) then
    call iotk_error_issue(ierr,"iotk_tag_parse",__FILE__,__LINE__)
# 545 "iotk_external.spp"
call iotk_error_msg(ierr,"CVS Revision: 1.16 ")
# 545 "iotk_external.spp"
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

# 557 "iotk_external.spp"
function iotk_complete_filepath_x(newfile,oldfile)
  use iotk_base
  use iotk_interface
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

# 577 "iotk_external.spp"
function iotk_check_name_x(name)
  use iotk_base
  use iotk_interface
  implicit none
  character(len=*), intent(in) :: name
  logical                      :: iotk_check_name_x
! Checks a single name
  integer :: len_name
  iotk_check_name_x = .true.
  len_name = len_trim(name)
  if(len_name>iotk_namlenx) iotk_check_name_x = .false.
  if(verify(name(1:1),iotk_namcharfirst)/=0) iotk_check_name_x = .false.
  if(len_name>1) then
    if(verify(name(2:len_name),iotk_namchar)/=0) iotk_check_name_x = .false.
  end if
end function iotk_check_name_x
