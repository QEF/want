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

# 30 "iotk_misc.spp"

# 33 "iotk_misc.spp"

# 35 "iotk_misc.spp"
subroutine iotk_copy_tag_x(source,dest,maxsize,ierr)
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
# 63 "iotk_misc.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.13 ")
    goto 1
  end if
  call iotk_inquire(dest  ,binary=dest_binary,  ierr=ierrl)
  if(ierrl/=0) then
    call iotk_error_issue(ierrl,"iotk_copy_tag",__FILE__,__LINE__)
# 68 "iotk_misc.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.13 ")            
    goto 1
  end if
  call iotk_unit_get(source,pointer=this)
  if(.not.associated(this)) then
    call iotk_error_issue(ierrl,"iotk_copy_tag",__FILE__,__LINE__)
# 73 "iotk_misc.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.13 ")
# 73 "iotk_misc.spp"
call iotk_error_msg(ierrl,'unit')  
    goto 1
  end if
  do
    call iotk_scan_tag(source,+1,control,tag,source_binary,ierrl)
    if(ierrl/=0) then
      call iotk_error_issue(ierrl,"iotk_copy_tag",__FILE__,__LINE__)
# 79 "iotk_misc.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.13 ")
      goto 1
    end if
    if(control/=4) then ! SKIP FOR COMMENTS
      call iotk_tag_parse(tag,name,attr,ierrl)
      if(ierrl/=0) then
        call iotk_error_issue(ierrl,"iotk_copy_tag",__FILE__,__LINE__)
# 85 "iotk_misc.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.13 ")
        goto 1
      end if
    end if
    if(iotk_strcomp(name,this%root)) then
      call iotk_scan_tag(source,-1,control,tag,source_binary,ierrl)
      if(ierrl/=0) then
        call iotk_error_issue(ierrl,"iotk_copy_tag",__FILE__,__LINE__)
# 92 "iotk_misc.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.13 ")
        goto 1
      end if
      return
    end if
    select case(control)
    case(1)
      call iotk_scan_attr(attr,"type",type,ierr=ierrl,eos=.true.,default=" ")
      if(ierrl/=0) then
        call iotk_error_issue(ierrl,"iotk_copy_tag",__FILE__,__LINE__)
# 101 "iotk_misc.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.13 ")
        goto 1
      end if
      if((iotk_strcomp(type,"real") .or. iotk_strcomp(type,"integer") .or. iotk_strcomp(type,"logical") &
                                    .or. iotk_strcomp(type,"character") .or. iotk_strcomp(type,"complex")) .and. control==1) then
        call iotk_copy_dat(source,dest,source_binary,dest_binary,name,attr,maxsize=maxsizel,ierr=ierrl)
        if(ierrl/=0) then
          call iotk_error_issue(ierrl,"iotk_copy_tag",__FILE__,__LINE__)
# 108 "iotk_misc.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.13 ")
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
# 125 "iotk_misc.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.13 ")
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

# 138 "iotk_misc.spp"
subroutine iotk_parse_dat_x(attr,type,ikind,isize,ilen,fmt,ierr)
  use iotk_base
  use iotk_error_interf
  use iotk_attr_interf
  use iotk_scan_interf
  use iotk_str_interf
  use iotk_misc_interf
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
  if(ierr/=0) then
    call iotk_error_issue(ierr,"iotk_parse_dat",__FILE__,__LINE__)
# 158 "iotk_misc.spp"
call iotk_error_msg(ierr,"CVS Revision: 1.13 ")
    return
  end if
  typelen = iotk_strlen(typename)
  type = iotk_toupper(typename)
  call iotk_scan_attr(attr,"kind",ikind,ierr=ierr,default=-1)
  if(ierr/=0) then
    call iotk_error_issue(ierr,"iotk_parse_dat",__FILE__,__LINE__)
# 165 "iotk_misc.spp"
call iotk_error_msg(ierr,"CVS Revision: 1.13 ")
    return
  end if
  call iotk_scan_attr(attr,"size",isize,ierr=ierr,default=-1)
  if(ierr/=0) then
    call iotk_error_issue(ierr,"iotk_parse_dat",__FILE__,__LINE__)
# 170 "iotk_misc.spp"
call iotk_error_msg(ierr,"CVS Revision: 1.13 ")
    return
  end if
  call iotk_scan_attr(attr,"len", ilen, ierr=ierr,default=-1)
  if(ierr/=0) then
    call iotk_error_issue(ierr,"iotk_parse_dat",__FILE__,__LINE__)
# 175 "iotk_misc.spp"
call iotk_error_msg(ierr,"CVS Revision: 1.13 ")
    return
  end if
  call iotk_scan_attr(attr,"fmt", fmt, ierr=ierr,eos=.true.,default="!"//iotk_eos)
  if(ierr/=0) then
    call iotk_error_issue(ierr,"iotk_parse_dat",__FILE__,__LINE__)
# 180 "iotk_misc.spp"
call iotk_error_msg(ierr,"CVS Revision: 1.13 ")
    return
  end if
end subroutine iotk_parse_dat_x

# 186 "iotk_misc.spp"
subroutine iotk_set_options_x(unitmin,unitmax,getline_buffer,error_warn_overflow,ierr)
  use iotk_base
  use iotk_error_interf
  use iotk_misc_interf
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
# 203 "iotk_misc.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.13 ")
      goto 1 
    end if
    iotk_unitmin = unitmin 
  end if
  if(present(unitmax)) then
    if(unitmax<iotk_unitmin) then
      call iotk_error_issue(ierrl,"iotk_set_options",__FILE__,__LINE__)
# 210 "iotk_misc.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.13 ")
      goto 1
    end if
    iotk_unitmax = unitmax
  end if
  if(present(getline_buffer)) then
    if(getline_buffer<1) then
      call iotk_error_issue(ierrl,"iotk_set_options",__FILE__,__LINE__)
# 217 "iotk_misc.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.13 ")
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

# 231 "iotk_misc.spp"
subroutine iotk_get_options_x(unitmin,unitmax,getline_buffer,error_warn_overflow)
  use iotk_base
  use iotk_misc_interf
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

# 246 "iotk_misc.spp"
subroutine iotk_print_kinds_x
  use iotk_base
  use iotk_misc_interf
  use iotk_xtox_interf
  implicit none
  character(100) :: string
# 253 "iotk_misc.spp"
#ifdef __IOTK_LOGICAL1
  string = "logical(kind="//trim(iotk_itoa(__IOTK_LOGICAL1))//")"
  write(*,"(a)") trim(string)
#endif
# 253 "iotk_misc.spp"
#ifdef __IOTK_LOGICAL2
  string = "logical(kind="//trim(iotk_itoa(__IOTK_LOGICAL2))//")"
  write(*,"(a)") trim(string)
#endif
# 253 "iotk_misc.spp"
#ifdef __IOTK_LOGICAL3
  string = "logical(kind="//trim(iotk_itoa(__IOTK_LOGICAL3))//")"
  write(*,"(a)") trim(string)
#endif
# 253 "iotk_misc.spp"
#ifdef __IOTK_LOGICAL4
  string = "logical(kind="//trim(iotk_itoa(__IOTK_LOGICAL4))//")"
  write(*,"(a)") trim(string)
#endif
# 259 "iotk_misc.spp"
#ifdef __IOTK_INTEGER1
  string = "integer(kind="//trim(iotk_itoa(__IOTK_INTEGER1))//")"
  write(*,"(a)") trim(string)
#endif
# 259 "iotk_misc.spp"
#ifdef __IOTK_INTEGER2
  string = "integer(kind="//trim(iotk_itoa(__IOTK_INTEGER2))//")"
  write(*,"(a)") trim(string)
#endif
# 259 "iotk_misc.spp"
#ifdef __IOTK_INTEGER3
  string = "integer(kind="//trim(iotk_itoa(__IOTK_INTEGER3))//")"
  write(*,"(a)") trim(string)
#endif
# 259 "iotk_misc.spp"
#ifdef __IOTK_INTEGER4
  string = "integer(kind="//trim(iotk_itoa(__IOTK_INTEGER4))//")"
  write(*,"(a)") trim(string)
#endif
# 265 "iotk_misc.spp"
#ifdef __IOTK_REAL1
  string = "real(kind="//trim(iotk_itoa(__IOTK_REAL1))//")"
  write(*,"(a)") trim(string)
#endif
# 265 "iotk_misc.spp"
#ifdef __IOTK_REAL2
  string = "real(kind="//trim(iotk_itoa(__IOTK_REAL2))//")"
  write(*,"(a)") trim(string)
#endif
# 265 "iotk_misc.spp"
#ifdef __IOTK_REAL3
  string = "real(kind="//trim(iotk_itoa(__IOTK_REAL3))//")"
  write(*,"(a)") trim(string)
#endif
# 265 "iotk_misc.spp"
#ifdef __IOTK_REAL4
  string = "real(kind="//trim(iotk_itoa(__IOTK_REAL4))//")"
  write(*,"(a)") trim(string)
#endif
# 270 "iotk_misc.spp"
  string = "character(kind="//trim(iotk_itoa(__IOTK_CHARACTER1))//")"
  write(*,"(a)") trim(string)
end subroutine iotk_print_kinds_x


# 276 "iotk_misc.spp"
subroutine iotk_copy_dat_aux_x(source,dest,source_binary,dest_binary,name,type,ikind,isize,ilen,fmt,ierr)
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
  integer,      intent(out) :: ierr
  
  integer :: tmpkind
# 300 "iotk_misc.spp"
#ifdef __IOTK_LOGICAL1
# 304 "iotk_misc.spp"
  LOGICAL (kind=__IOTK_LOGICAL1), allocatable :: dat_LOGICAL1 (:)
# 306 "iotk_misc.spp"
#endif
# 300 "iotk_misc.spp"
#ifdef __IOTK_LOGICAL2
# 304 "iotk_misc.spp"
  LOGICAL (kind=__IOTK_LOGICAL2), allocatable :: dat_LOGICAL2 (:)
# 306 "iotk_misc.spp"
#endif
# 300 "iotk_misc.spp"
#ifdef __IOTK_LOGICAL3
# 304 "iotk_misc.spp"
  LOGICAL (kind=__IOTK_LOGICAL3), allocatable :: dat_LOGICAL3 (:)
# 306 "iotk_misc.spp"
#endif
# 300 "iotk_misc.spp"
#ifdef __IOTK_LOGICAL4
# 304 "iotk_misc.spp"
  LOGICAL (kind=__IOTK_LOGICAL4), allocatable :: dat_LOGICAL4 (:)
# 306 "iotk_misc.spp"
#endif
# 300 "iotk_misc.spp"
#ifdef __IOTK_INTEGER1
# 304 "iotk_misc.spp"
  INTEGER (kind=__IOTK_INTEGER1), allocatable :: dat_INTEGER1 (:)
# 306 "iotk_misc.spp"
#endif
# 300 "iotk_misc.spp"
#ifdef __IOTK_INTEGER2
# 304 "iotk_misc.spp"
  INTEGER (kind=__IOTK_INTEGER2), allocatable :: dat_INTEGER2 (:)
# 306 "iotk_misc.spp"
#endif
# 300 "iotk_misc.spp"
#ifdef __IOTK_INTEGER3
# 304 "iotk_misc.spp"
  INTEGER (kind=__IOTK_INTEGER3), allocatable :: dat_INTEGER3 (:)
# 306 "iotk_misc.spp"
#endif
# 300 "iotk_misc.spp"
#ifdef __IOTK_INTEGER4
# 304 "iotk_misc.spp"
  INTEGER (kind=__IOTK_INTEGER4), allocatable :: dat_INTEGER4 (:)
# 306 "iotk_misc.spp"
#endif
# 300 "iotk_misc.spp"
#ifdef __IOTK_REAL1
# 304 "iotk_misc.spp"
  REAL (kind=__IOTK_REAL1), allocatable :: dat_REAL1 (:)
# 306 "iotk_misc.spp"
#endif
# 300 "iotk_misc.spp"
#ifdef __IOTK_REAL2
# 304 "iotk_misc.spp"
  REAL (kind=__IOTK_REAL2), allocatable :: dat_REAL2 (:)
# 306 "iotk_misc.spp"
#endif
# 300 "iotk_misc.spp"
#ifdef __IOTK_REAL3
# 304 "iotk_misc.spp"
  REAL (kind=__IOTK_REAL3), allocatable :: dat_REAL3 (:)
# 306 "iotk_misc.spp"
#endif
# 300 "iotk_misc.spp"
#ifdef __IOTK_REAL4
# 304 "iotk_misc.spp"
  REAL (kind=__IOTK_REAL4), allocatable :: dat_REAL4 (:)
# 306 "iotk_misc.spp"
#endif
# 300 "iotk_misc.spp"
#ifdef __IOTK_COMPLEX1
# 304 "iotk_misc.spp"
  COMPLEX (kind=__IOTK_COMPLEX1), allocatable :: dat_COMPLEX1 (:)
# 306 "iotk_misc.spp"
#endif
# 300 "iotk_misc.spp"
#ifdef __IOTK_COMPLEX2
# 304 "iotk_misc.spp"
  COMPLEX (kind=__IOTK_COMPLEX2), allocatable :: dat_COMPLEX2 (:)
# 306 "iotk_misc.spp"
#endif
# 300 "iotk_misc.spp"
#ifdef __IOTK_COMPLEX3
# 304 "iotk_misc.spp"
  COMPLEX (kind=__IOTK_COMPLEX3), allocatable :: dat_COMPLEX3 (:)
# 306 "iotk_misc.spp"
#endif
# 300 "iotk_misc.spp"
#ifdef __IOTK_COMPLEX4
# 304 "iotk_misc.spp"
  COMPLEX (kind=__IOTK_COMPLEX4), allocatable :: dat_COMPLEX4 (:)
# 306 "iotk_misc.spp"
#endif
# 300 "iotk_misc.spp"
#ifdef __IOTK_CHARACTER1
# 302 "iotk_misc.spp"
  CHARACTER (kind=__IOTK_CHARACTER1,len=ilen), allocatable :: dat_CHARACTER1 (:)
# 306 "iotk_misc.spp"
#endif
# 310 "iotk_misc.spp"

! la regola e' semplice
! SE SOURCE E' BINARIO: usa il kind di source
! SE SOURCE E' TESTUALE: use il kind di default se e' definito
!                        altrimenti usa il primo kind disponibile
! ad ogni modo, il kind e' calcolato run-time, dunque in futuro lo si potrebbe
! chiedere all'utente
  ierr=0
  select case(type(1:iotk_strlen(type)))
# 320 "iotk_misc.spp"
  case("LOGICAL")
# 324 "iotk_misc.spp"
    if(source_binary) then
      tmpkind=ikind
    else
      tmpkind=0
# 329 "iotk_misc.spp"
#ifdef __IOTK_LOGICAL1
      if(tmpkind==0) tmpkind=__IOTK_LOGICAL1
      if(__IOTK_LOGICAL1 == iotk_defkind_LOGICAL) then
        tmpkind=iotk_defkind_LOGICAL
      end if
#endif
# 329 "iotk_misc.spp"
#ifdef __IOTK_LOGICAL2
      if(tmpkind==0) tmpkind=__IOTK_LOGICAL2
      if(__IOTK_LOGICAL2 == iotk_defkind_LOGICAL) then
        tmpkind=iotk_defkind_LOGICAL
      end if
#endif
# 329 "iotk_misc.spp"
#ifdef __IOTK_LOGICAL3
      if(tmpkind==0) tmpkind=__IOTK_LOGICAL3
      if(__IOTK_LOGICAL3 == iotk_defkind_LOGICAL) then
        tmpkind=iotk_defkind_LOGICAL
      end if
#endif
# 329 "iotk_misc.spp"
#ifdef __IOTK_LOGICAL4
      if(tmpkind==0) tmpkind=__IOTK_LOGICAL4
      if(__IOTK_LOGICAL4 == iotk_defkind_LOGICAL) then
        tmpkind=iotk_defkind_LOGICAL
      end if
#endif
# 336 "iotk_misc.spp"
    end if
# 338 "iotk_misc.spp"
    select case(tmpkind)
# 341 "iotk_misc.spp"
#ifdef __IOTK_LOGICAL1
    case(__IOTK_LOGICAL1)
      allocate(dat_LOGICAL1(isize))
      call iotk_scan_dat_aux(source,dat_LOGICAL1,ikind,ilen,fmt,ierr)
      if(ierr==0) call iotk_write_dat(dest,name,dat_LOGICAL1,ierr=ierr,fmt=fmt)
      deallocate(dat_LOGICAL1)
#endif
# 341 "iotk_misc.spp"
#ifdef __IOTK_LOGICAL2
    case(__IOTK_LOGICAL2)
      allocate(dat_LOGICAL2(isize))
      call iotk_scan_dat_aux(source,dat_LOGICAL2,ikind,ilen,fmt,ierr)
      if(ierr==0) call iotk_write_dat(dest,name,dat_LOGICAL2,ierr=ierr,fmt=fmt)
      deallocate(dat_LOGICAL2)
#endif
# 341 "iotk_misc.spp"
#ifdef __IOTK_LOGICAL3
    case(__IOTK_LOGICAL3)
      allocate(dat_LOGICAL3(isize))
      call iotk_scan_dat_aux(source,dat_LOGICAL3,ikind,ilen,fmt,ierr)
      if(ierr==0) call iotk_write_dat(dest,name,dat_LOGICAL3,ierr=ierr,fmt=fmt)
      deallocate(dat_LOGICAL3)
#endif
# 341 "iotk_misc.spp"
#ifdef __IOTK_LOGICAL4
    case(__IOTK_LOGICAL4)
      allocate(dat_LOGICAL4(isize))
      call iotk_scan_dat_aux(source,dat_LOGICAL4,ikind,ilen,fmt,ierr)
      if(ierr==0) call iotk_write_dat(dest,name,dat_LOGICAL4,ierr=ierr,fmt=fmt)
      deallocate(dat_LOGICAL4)
#endif
# 350 "iotk_misc.spp"
    case default
      call iotk_error_issue(ierr,"iotk_copy_dat_aux",__FILE__,__LINE__)
# 351 "iotk_misc.spp"
call iotk_error_msg(ierr,"CVS Revision: 1.13 ")
# 351 "iotk_misc.spp"
call iotk_error_msg(ierr,'internal error')
    end select
# 320 "iotk_misc.spp"
  case("INTEGER")
# 324 "iotk_misc.spp"
    if(source_binary) then
      tmpkind=ikind
    else
      tmpkind=0
# 329 "iotk_misc.spp"
#ifdef __IOTK_INTEGER1
      if(tmpkind==0) tmpkind=__IOTK_INTEGER1
      if(__IOTK_INTEGER1 == iotk_defkind_INTEGER) then
        tmpkind=iotk_defkind_INTEGER
      end if
#endif
# 329 "iotk_misc.spp"
#ifdef __IOTK_INTEGER2
      if(tmpkind==0) tmpkind=__IOTK_INTEGER2
      if(__IOTK_INTEGER2 == iotk_defkind_INTEGER) then
        tmpkind=iotk_defkind_INTEGER
      end if
#endif
# 329 "iotk_misc.spp"
#ifdef __IOTK_INTEGER3
      if(tmpkind==0) tmpkind=__IOTK_INTEGER3
      if(__IOTK_INTEGER3 == iotk_defkind_INTEGER) then
        tmpkind=iotk_defkind_INTEGER
      end if
#endif
# 329 "iotk_misc.spp"
#ifdef __IOTK_INTEGER4
      if(tmpkind==0) tmpkind=__IOTK_INTEGER4
      if(__IOTK_INTEGER4 == iotk_defkind_INTEGER) then
        tmpkind=iotk_defkind_INTEGER
      end if
#endif
# 336 "iotk_misc.spp"
    end if
# 338 "iotk_misc.spp"
    select case(tmpkind)
# 341 "iotk_misc.spp"
#ifdef __IOTK_INTEGER1
    case(__IOTK_INTEGER1)
      allocate(dat_INTEGER1(isize))
      call iotk_scan_dat_aux(source,dat_INTEGER1,ikind,ilen,fmt,ierr)
      if(ierr==0) call iotk_write_dat(dest,name,dat_INTEGER1,ierr=ierr,fmt=fmt)
      deallocate(dat_INTEGER1)
#endif
# 341 "iotk_misc.spp"
#ifdef __IOTK_INTEGER2
    case(__IOTK_INTEGER2)
      allocate(dat_INTEGER2(isize))
      call iotk_scan_dat_aux(source,dat_INTEGER2,ikind,ilen,fmt,ierr)
      if(ierr==0) call iotk_write_dat(dest,name,dat_INTEGER2,ierr=ierr,fmt=fmt)
      deallocate(dat_INTEGER2)
#endif
# 341 "iotk_misc.spp"
#ifdef __IOTK_INTEGER3
    case(__IOTK_INTEGER3)
      allocate(dat_INTEGER3(isize))
      call iotk_scan_dat_aux(source,dat_INTEGER3,ikind,ilen,fmt,ierr)
      if(ierr==0) call iotk_write_dat(dest,name,dat_INTEGER3,ierr=ierr,fmt=fmt)
      deallocate(dat_INTEGER3)
#endif
# 341 "iotk_misc.spp"
#ifdef __IOTK_INTEGER4
    case(__IOTK_INTEGER4)
      allocate(dat_INTEGER4(isize))
      call iotk_scan_dat_aux(source,dat_INTEGER4,ikind,ilen,fmt,ierr)
      if(ierr==0) call iotk_write_dat(dest,name,dat_INTEGER4,ierr=ierr,fmt=fmt)
      deallocate(dat_INTEGER4)
#endif
# 350 "iotk_misc.spp"
    case default
      call iotk_error_issue(ierr,"iotk_copy_dat_aux",__FILE__,__LINE__)
# 351 "iotk_misc.spp"
call iotk_error_msg(ierr,"CVS Revision: 1.13 ")
# 351 "iotk_misc.spp"
call iotk_error_msg(ierr,'internal error')
    end select
# 320 "iotk_misc.spp"
  case("REAL")
# 324 "iotk_misc.spp"
    if(source_binary) then
      tmpkind=ikind
    else
      tmpkind=0
# 329 "iotk_misc.spp"
#ifdef __IOTK_REAL1
      if(tmpkind==0) tmpkind=__IOTK_REAL1
      if(__IOTK_REAL1 == iotk_defkind_REAL) then
        tmpkind=iotk_defkind_REAL
      end if
#endif
# 329 "iotk_misc.spp"
#ifdef __IOTK_REAL2
      if(tmpkind==0) tmpkind=__IOTK_REAL2
      if(__IOTK_REAL2 == iotk_defkind_REAL) then
        tmpkind=iotk_defkind_REAL
      end if
#endif
# 329 "iotk_misc.spp"
#ifdef __IOTK_REAL3
      if(tmpkind==0) tmpkind=__IOTK_REAL3
      if(__IOTK_REAL3 == iotk_defkind_REAL) then
        tmpkind=iotk_defkind_REAL
      end if
#endif
# 329 "iotk_misc.spp"
#ifdef __IOTK_REAL4
      if(tmpkind==0) tmpkind=__IOTK_REAL4
      if(__IOTK_REAL4 == iotk_defkind_REAL) then
        tmpkind=iotk_defkind_REAL
      end if
#endif
# 336 "iotk_misc.spp"
    end if
# 338 "iotk_misc.spp"
    select case(tmpkind)
# 341 "iotk_misc.spp"
#ifdef __IOTK_REAL1
    case(__IOTK_REAL1)
      allocate(dat_REAL1(isize))
      call iotk_scan_dat_aux(source,dat_REAL1,ikind,ilen,fmt,ierr)
      if(ierr==0) call iotk_write_dat(dest,name,dat_REAL1,ierr=ierr,fmt=fmt)
      deallocate(dat_REAL1)
#endif
# 341 "iotk_misc.spp"
#ifdef __IOTK_REAL2
    case(__IOTK_REAL2)
      allocate(dat_REAL2(isize))
      call iotk_scan_dat_aux(source,dat_REAL2,ikind,ilen,fmt,ierr)
      if(ierr==0) call iotk_write_dat(dest,name,dat_REAL2,ierr=ierr,fmt=fmt)
      deallocate(dat_REAL2)
#endif
# 341 "iotk_misc.spp"
#ifdef __IOTK_REAL3
    case(__IOTK_REAL3)
      allocate(dat_REAL3(isize))
      call iotk_scan_dat_aux(source,dat_REAL3,ikind,ilen,fmt,ierr)
      if(ierr==0) call iotk_write_dat(dest,name,dat_REAL3,ierr=ierr,fmt=fmt)
      deallocate(dat_REAL3)
#endif
# 341 "iotk_misc.spp"
#ifdef __IOTK_REAL4
    case(__IOTK_REAL4)
      allocate(dat_REAL4(isize))
      call iotk_scan_dat_aux(source,dat_REAL4,ikind,ilen,fmt,ierr)
      if(ierr==0) call iotk_write_dat(dest,name,dat_REAL4,ierr=ierr,fmt=fmt)
      deallocate(dat_REAL4)
#endif
# 350 "iotk_misc.spp"
    case default
      call iotk_error_issue(ierr,"iotk_copy_dat_aux",__FILE__,__LINE__)
# 351 "iotk_misc.spp"
call iotk_error_msg(ierr,"CVS Revision: 1.13 ")
# 351 "iotk_misc.spp"
call iotk_error_msg(ierr,'internal error')
    end select
# 320 "iotk_misc.spp"
  case("COMPLEX")
# 324 "iotk_misc.spp"
    if(source_binary) then
      tmpkind=ikind
    else
      tmpkind=0
# 329 "iotk_misc.spp"
#ifdef __IOTK_COMPLEX1
      if(tmpkind==0) tmpkind=__IOTK_COMPLEX1
      if(__IOTK_COMPLEX1 == iotk_defkind_COMPLEX) then
        tmpkind=iotk_defkind_COMPLEX
      end if
#endif
# 329 "iotk_misc.spp"
#ifdef __IOTK_COMPLEX2
      if(tmpkind==0) tmpkind=__IOTK_COMPLEX2
      if(__IOTK_COMPLEX2 == iotk_defkind_COMPLEX) then
        tmpkind=iotk_defkind_COMPLEX
      end if
#endif
# 329 "iotk_misc.spp"
#ifdef __IOTK_COMPLEX3
      if(tmpkind==0) tmpkind=__IOTK_COMPLEX3
      if(__IOTK_COMPLEX3 == iotk_defkind_COMPLEX) then
        tmpkind=iotk_defkind_COMPLEX
      end if
#endif
# 329 "iotk_misc.spp"
#ifdef __IOTK_COMPLEX4
      if(tmpkind==0) tmpkind=__IOTK_COMPLEX4
      if(__IOTK_COMPLEX4 == iotk_defkind_COMPLEX) then
        tmpkind=iotk_defkind_COMPLEX
      end if
#endif
# 336 "iotk_misc.spp"
    end if
# 338 "iotk_misc.spp"
    select case(tmpkind)
# 341 "iotk_misc.spp"
#ifdef __IOTK_COMPLEX1
    case(__IOTK_COMPLEX1)
      allocate(dat_COMPLEX1(isize))
      call iotk_scan_dat_aux(source,dat_COMPLEX1,ikind,ilen,fmt,ierr)
      if(ierr==0) call iotk_write_dat(dest,name,dat_COMPLEX1,ierr=ierr,fmt=fmt)
      deallocate(dat_COMPLEX1)
#endif
# 341 "iotk_misc.spp"
#ifdef __IOTK_COMPLEX2
    case(__IOTK_COMPLEX2)
      allocate(dat_COMPLEX2(isize))
      call iotk_scan_dat_aux(source,dat_COMPLEX2,ikind,ilen,fmt,ierr)
      if(ierr==0) call iotk_write_dat(dest,name,dat_COMPLEX2,ierr=ierr,fmt=fmt)
      deallocate(dat_COMPLEX2)
#endif
# 341 "iotk_misc.spp"
#ifdef __IOTK_COMPLEX3
    case(__IOTK_COMPLEX3)
      allocate(dat_COMPLEX3(isize))
      call iotk_scan_dat_aux(source,dat_COMPLEX3,ikind,ilen,fmt,ierr)
      if(ierr==0) call iotk_write_dat(dest,name,dat_COMPLEX3,ierr=ierr,fmt=fmt)
      deallocate(dat_COMPLEX3)
#endif
# 341 "iotk_misc.spp"
#ifdef __IOTK_COMPLEX4
    case(__IOTK_COMPLEX4)
      allocate(dat_COMPLEX4(isize))
      call iotk_scan_dat_aux(source,dat_COMPLEX4,ikind,ilen,fmt,ierr)
      if(ierr==0) call iotk_write_dat(dest,name,dat_COMPLEX4,ierr=ierr,fmt=fmt)
      deallocate(dat_COMPLEX4)
#endif
# 350 "iotk_misc.spp"
    case default
      call iotk_error_issue(ierr,"iotk_copy_dat_aux",__FILE__,__LINE__)
# 351 "iotk_misc.spp"
call iotk_error_msg(ierr,"CVS Revision: 1.13 ")
# 351 "iotk_misc.spp"
call iotk_error_msg(ierr,'internal error')
    end select
# 320 "iotk_misc.spp"
  case("CHARACTER")
# 322 "iotk_misc.spp"
    tmpkind=iotk_defkind_CHARACTER
# 338 "iotk_misc.spp"
    select case(tmpkind)
# 341 "iotk_misc.spp"
#ifdef __IOTK_CHARACTER1
    case(__IOTK_CHARACTER1)
      allocate(dat_CHARACTER1(isize))
      call iotk_scan_dat_aux(source,dat_CHARACTER1,ikind,ilen,fmt,ierr)
      if(ierr==0) call iotk_write_dat(dest,name,dat_CHARACTER1,ierr=ierr,fmt=fmt)
      deallocate(dat_CHARACTER1)
#endif
# 350 "iotk_misc.spp"
    case default
      call iotk_error_issue(ierr,"iotk_copy_dat_aux",__FILE__,__LINE__)
# 351 "iotk_misc.spp"
call iotk_error_msg(ierr,"CVS Revision: 1.13 ")
# 351 "iotk_misc.spp"
call iotk_error_msg(ierr,'internal error')
    end select
# 354 "iotk_misc.spp"
  case default
    call iotk_error_issue(ierr,"iotk_copy_dat_aux",__FILE__,__LINE__)
# 355 "iotk_misc.spp"
call iotk_error_msg(ierr,"CVS Revision: 1.13 ")
# 355 "iotk_misc.spp"
call iotk_error_msg(ierr,'internal error')
  end select
  
end subroutine iotk_copy_dat_aux_x


# 362 "iotk_misc.spp"
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
  integer :: ikind,isize,ilen
  character(iotk_vallenx) :: fmt
  character(iotk_attlenx) :: attr1
  ierr = 0
  call iotk_parse_dat(attr,type,ikind,isize,ilen,fmt,ierr)
  if(ierr/=0) then
    call iotk_error_issue(ierr,"iotk_copy_dat",__FILE__,__LINE__)
# 385 "iotk_misc.spp"
call iotk_error_msg(ierr,"CVS Revision: 1.13 ")
    return
  end if
  if(iotk_strcomp(type,iotk_eos)) then
    call iotk_error_issue(ierr,"iotk_copy_dat",__FILE__,__LINE__)
# 389 "iotk_misc.spp"
call iotk_error_msg(ierr,"CVS Revision: 1.13 ")
    return
  end if
  if(isize==-1) then
    call iotk_error_issue(ierr,"iotk_copy_dat",__FILE__,__LINE__)
# 393 "iotk_misc.spp"
call iotk_error_msg(ierr,"CVS Revision: 1.13 ")
    return
  end if
  if(ilen==-1 .and. iotk_strcomp(type,"CHARACTER")) then
    call iotk_error_issue(ierr,"iotk_copy_dat",__FILE__,__LINE__)
# 397 "iotk_misc.spp"
call iotk_error_msg(ierr,"CVS Revision: 1.13 ")
    return
  end if
  if(isize<=maxsize .or. maxsize==-1 .or. dest_binary) then
    call iotk_copy_dat_aux(source,dest,source_binary,dest_binary,name,type,ikind,isize,ilen,fmt,ierr)
    if(ierr/=0) then
      call iotk_error_issue(ierr,"iotk_copy_dat",__FILE__,__LINE__)
# 403 "iotk_misc.spp"
call iotk_error_msg(ierr,"CVS Revision: 1.13 ")
      return
    end if  
  else    
    call iotk_strcpy(attr1,attr,ierr=ierr)
    if(ierr/=0) then
      call iotk_error_issue(ierr,"iotk_copy_dat",__FILE__,__LINE__)
# 409 "iotk_misc.spp"
call iotk_error_msg(ierr,"CVS Revision: 1.13 ")
      return
    end if
    call iotk_write_attr (attr1,"trunc",.true.,ierr=ierr)
    if(ierr/=0) then
      call iotk_error_issue(ierr,"iotk_copy_dat",__FILE__,__LINE__)
# 414 "iotk_misc.spp"
call iotk_error_msg(ierr,"CVS Revision: 1.13 ")
      return
    end if
    call iotk_write_empty(dest,name,attr=attr1,ierr=ierr)
    if(ierr/=0) then
      call iotk_error_issue(ierr,"iotk_copy_dat",__FILE__,__LINE__)
# 419 "iotk_misc.spp"
call iotk_error_msg(ierr,"CVS Revision: 1.13 ")
      return
    end if
  end if
end subroutine iotk_copy_dat_x

# 426 "iotk_misc.spp"
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
# 445 "iotk_misc.spp"
call iotk_error_msg(ierr,"CVS Revision: 1.13 ")
    return
  end if
  if(found) then
    attlen = iotk_strlen(file_version)
    pos1   = iotk_strscan(file_version,".")
    if(pos1<=1 .or. pos1>=attlen) then
      call iotk_error_issue(ierr,"iotk_check_iotk_attr",__FILE__,__LINE__)
# 452 "iotk_misc.spp"
call iotk_error_msg(ierr,"CVS Revision: 1.13 ")
# 452 "iotk_misc.spp"
call iotk_error_msg(ierr,'Problems reading file version')
# 452 "iotk_misc.spp"
call iotk_error_write(ierr,"file_version",file_version)
# 452 "iotk_misc.spp"
call iotk_error_write(ierr,"attlen",attlen)
# 452 "iotk_misc.spp"
call iotk_error_write(ierr,"pos1",pos1)
      return
    end if
    pos2   = pos1 + verify(file_version(pos1+1:attlen),numbers)
    if(pos2==pos1+1) then
      call iotk_error_issue(ierr,"iotk_check_iotk_attr",__FILE__,__LINE__)
# 457 "iotk_misc.spp"
call iotk_error_msg(ierr,"CVS Revision: 1.13 ")
# 457 "iotk_misc.spp"
call iotk_error_msg(ierr,'Problems reading file version')
# 457 "iotk_misc.spp"
call iotk_error_write(ierr,"file_version",file_version)
# 457 "iotk_misc.spp"
call iotk_error_write(ierr,"attlen",attlen)
# 457 "iotk_misc.spp"
call iotk_error_write(ierr,"pos1",pos1)
# 457 "iotk_misc.spp"
call iotk_error_write(ierr,"pos2",pos2)
      return
    end if
    if(pos2==pos1) pos2 = attlen+1
    call iotk_atoi(itmp_major,file_version(1:pos1-1),check)
    if(.not.check) then
      call iotk_error_issue(ierr,"iotk_check_iotk_attr",__FILE__,__LINE__)
# 463 "iotk_misc.spp"
call iotk_error_msg(ierr,"CVS Revision: 1.13 ")
# 463 "iotk_misc.spp"
call iotk_error_msg(ierr,'Problems reading file version')
# 463 "iotk_misc.spp"
call iotk_error_write(ierr,"file_version",file_version)
      return
    end if
    call iotk_atoi(itmp_minor,file_version(pos1+1:pos2-1),check)
    if(.not.check) then
      call iotk_error_issue(ierr,"iotk_check_iotk_attr",__FILE__,__LINE__)
# 468 "iotk_misc.spp"
call iotk_error_msg(ierr,"CVS Revision: 1.13 ")
# 468 "iotk_misc.spp"
call iotk_error_msg(ierr,'Problems reading file version')
# 468 "iotk_misc.spp"
call iotk_error_write(ierr,"file_version",file_version)
      return
    end if
    if(itmp_major > iotk_file_version_major .or. &
      (itmp_major==iotk_file_version_major .and. itmp_minor > iotk_file_version_minor) ) then
      call iotk_error_issue(ierr,"iotk_check_iotk_attr",__FILE__,__LINE__)
# 473 "iotk_misc.spp"
call iotk_error_msg(ierr,"CVS Revision: 1.13 ")
# 473 "iotk_misc.spp"
call iotk_error_msg(ierr,'File version is newer than internal version')
# 473 "iotk_misc.spp"
call iotk_error_write(ierr,"file_version",file_version)
# 473 "iotk_misc.spp"
call iotk_error_write(ierr,"internal_version",iotk_file_version)
      return
    end if
  end if
  call iotk_scan_attr(attr,"binary",rbinary,ierr=ierr,found=found)
  if(ierr/=0) then
    call iotk_error_issue(ierr,"iotk_check_iotk_attr",__FILE__,__LINE__)
# 479 "iotk_misc.spp"
call iotk_error_msg(ierr,"CVS Revision: 1.13 ")
    return
  end if
  if(found) then
    call iotk_inquire(unit,binary,ierr=ierr)
    if(ierr/=0) then
      call iotk_error_issue(ierr,"iotk_check_iotk_attr",__FILE__,__LINE__)
# 485 "iotk_misc.spp"
call iotk_error_msg(ierr,"CVS Revision: 1.13 ")
      return
    end if
    if(rbinary .neqv. binary) then
      call iotk_error_issue(ierr,"iotk_check_iotk_attr",__FILE__,__LINE__)
# 489 "iotk_misc.spp"
call iotk_error_msg(ierr,"CVS Revision: 1.13 ")
      return
    end if
  end if
end subroutine iotk_check_iotk_attr_x

# 496 "iotk_misc.spp"
function iotk_index_scal(index)
  use iotk_base
  use iotk_xtox_interf
  use iotk_misc_interf
  integer,           intent(in) :: index
  character(len=range(index)+3) :: iotk_index_scal
  iotk_index_scal="."//iotk_itoa(index)
end function iotk_index_scal
  
# 506 "iotk_misc.spp"
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


# 524 "iotk_misc.spp"
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
# 538 "iotk_misc.spp"
call iotk_error_msg(ierr,"CVS Revision: 1.13 ")
# 538 "iotk_misc.spp"
call iotk_error_msg(ierr,'Wrong syntax in tag')
    call iotk_error_write(ierr,"tag",tag(1:lentag))
    return
  end if
  pos = scan(tag(1:lentag)," ")
  if(pos==0) pos=lentag+1
  if(pos>len(name)+1) then
    call iotk_error_issue(ierr,"iotk_tag_parse",__FILE__,__LINE__)
# 545 "iotk_misc.spp"
call iotk_error_msg(ierr,"CVS Revision: 1.13 ")
# 545 "iotk_misc.spp"
call iotk_error_msg(ierr,'Tag name too long')
    return
   end if
  name = tag(1:pos-1)
  if(pos<=len(name)) name(pos:pos) = iotk_eos
  lenatt = len_trim(tag(pos:lentag))
  if(lenatt>iotk_attlenx) then
    call iotk_error_issue(ierr,"iotk_tag_parse",__FILE__,__LINE__)
# 552 "iotk_misc.spp"
call iotk_error_msg(ierr,"CVS Revision: 1.13 ")
# 552 "iotk_misc.spp"
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

# 564 "iotk_misc.spp"
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

# 584 "iotk_misc.spp"
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
