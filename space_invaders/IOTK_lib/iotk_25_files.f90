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

# 30 "iotk_files.spp"

# 33 "iotk_files.spp"

# 35 "iotk_files.spp"
subroutine iotk_copyfile_x(source,dest,source_unit,dest_unit,ierr)
  use iotk_base
  use iotk_error_interf
  use iotk_scan_interf
  use iotk_str_interf
  use iotk_unit_interf
  use iotk_misc_interf
  implicit none
  character(len=*), optional, intent(in) :: source
  character(len=*), optional, intent(in) :: dest
  integer,          optional, intent(in) :: source_unit
  integer,          optional, intent(in) :: dest_unit
  integer,          optional, intent(out):: ierr
  integer :: ierrl,unit1,unit2
  integer :: iostat,length
  character(len=iotk_linlenx) :: line
  iostat = 0
  ierrl  = 0
  if(present(source) .eqv. present(source_unit)) then
    call iotk_error_issue(ierrl,"iotk_copyfile_x",__FILE__,__LINE__)
# 54 "iotk_files.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.11 ")
# 54 "iotk_files.spp"
call iotk_error_msg(ierrl,'Use exactly one between source and source_unit')
    goto 1
  end if
  if(present(dest)   .eqv. present(dest_unit)) then
    call iotk_error_issue(ierrl,"iotk_copyfile_x",__FILE__,__LINE__)
# 58 "iotk_files.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.11 ")
# 58 "iotk_files.spp"
call iotk_error_msg(ierrl,'Use exactly one between dest and dest_unit')
    goto 1
  end if
  if(present(source)) then
    call iotk_free_unit(unit1,ierr=ierrl)
    if(ierrl/=0) then
      call iotk_error_issue(ierrl,"iotk_copyfile_x",__FILE__,__LINE__)
# 64 "iotk_files.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.11 ")
# 64 "iotk_files.spp"
call iotk_error_msg(ierrl,'Error searching for a free unit')
      goto 1
    end if
    open(unit1,file=trim(iotk_strpad(source)),iostat=iostat)
    if(iostat/=0) then
      call iotk_error_issue(ierrl,"iotk_copyfile_x",__FILE__,__LINE__)
# 69 "iotk_files.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.11 ")
# 69 "iotk_files.spp"
call iotk_error_msg(ierrl,'messaggio')
# 69 "iotk_files.spp"
call iotk_error_write(ierrl,"sourcefile",trim(iotk_strpad(source)))
# 69 "iotk_files.spp"
call iotk_error_write(ierrl,"sourceunit",unit1)
# 69 "iotk_files.spp"
call iotk_error_write(ierrl,"iostat",iostat)
      goto 1
    end if
  else
    unit1=source_unit
  end if
  if(present(dest)) then
    call iotk_free_unit(unit2,ierr=ierrl)
    if(ierrl/=0) then
      call iotk_error_issue(ierrl,"iotk_copyfile_x",__FILE__,__LINE__)
# 78 "iotk_files.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.11 ")
      goto 1
    end if
    open(unit2,file=trim(iotk_strpad(dest)),iostat=iostat)
    if(iostat/=0) then
      call iotk_error_issue(ierrl,"iotk_copyfile_x",__FILE__,__LINE__)
# 83 "iotk_files.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.11 ")
# 83 "iotk_files.spp"
call iotk_error_msg(ierrl,'Error opening destination file')
# 83 "iotk_files.spp"
call iotk_error_write(ierrl,"destfile",trim(iotk_strpad(dest)))
# 83 "iotk_files.spp"
call iotk_error_write(ierrl,"destunit",unit2)
# 83 "iotk_files.spp"
call iotk_error_write(ierrl,"iostat",iostat)
      goto 1
    end if
  else
    unit2=dest_unit
  end if
  do
    call iotk_getline(unit1,line,length,ierrl)
    if(ierrl/=0) then
      call iotk_error_scan(ierrl,"iostat",iostat)
      if(iostat<0) then
        call iotk_error_clear(ierrl)
        exit
      end if
      call iotk_error_issue(ierrl,"iotk_copyfile_x",__FILE__,__LINE__)
# 97 "iotk_files.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.11 ")
# 97 "iotk_files.spp"
call iotk_error_msg(ierrl,'Error reading source file')
# 97 "iotk_files.spp"
call iotk_error_write(ierrl,"sourceunit",unit1)
      goto 1
    end if
    write(unit2,"(a)",iostat=iostat) line(1:length)
    if(iostat/=0) then
       call iotk_error_issue(ierrl,"iotk_copyfile_x",__FILE__,__LINE__)
# 102 "iotk_files.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.11 ")
# 102 "iotk_files.spp"
call iotk_error_msg(ierrl,'Error writing destination file')
# 102 "iotk_files.spp"
call iotk_error_write(ierrl,"destunit",unit2)
# 102 "iotk_files.spp"
call iotk_error_write(ierrl,"iostat",iostat)
       goto 1
    end if 
  end do
  iostat=0
  if(present(source)) then
    close(unit1,iostat=iostat)
    if(iostat/=0) then
       call iotk_error_issue(ierrl,"iotk_copyfile_x",__FILE__,__LINE__)
# 110 "iotk_files.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.11 ")
# 110 "iotk_files.spp"
call iotk_error_msg(ierrl,'Error closing source file')
# 110 "iotk_files.spp"
call iotk_error_write(ierrl,"sourcefile",trim(iotk_strpad(source)))
# 110 "iotk_files.spp"
call iotk_error_write(ierrl,"sourceunit",unit1)
# 110 "iotk_files.spp"
call iotk_error_write(ierrl,"iostat",iostat)
       goto 1
    end if 
  end if
  if(present(dest)) then
    close(unit2,iostat=iostat)
    if(iostat/=0) then
       call iotk_error_issue(ierrl,"iotk_copyfile_x",__FILE__,__LINE__)
# 117 "iotk_files.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.11 ")
# 117 "iotk_files.spp"
call iotk_error_msg(ierrl,'Error closing destination file')
# 117 "iotk_files.spp"
call iotk_error_write(ierrl,"destfile",trim(iotk_strpad(dest)))
# 117 "iotk_files.spp"
call iotk_error_write(ierrl,"destunit",unit2)
# 117 "iotk_files.spp"
call iotk_error_write(ierrl,"iostat",iostat)
       goto 1
    end if
  end if
1 continue
  if(present(ierr)) then
    ierr = ierrl
  else
    if(ierrl/=0) call iotk_error_handler(ierrl)
  end if
end subroutine iotk_copyfile_x

# 130 "iotk_files.spp"
subroutine iotk_link_x(unit,name,file,binary,raw,create,ierr)
  use iotk_base
  use iotk_error_interf
  use iotk_attr_interf
  use iotk_files_interf
  use iotk_str_interf
  use iotk_write_interf
  use iotk_misc_interf
  use iotk_unit_interf
  implicit none
  integer,                intent(in)  :: unit
  character(*),           intent(in)  :: name
  character(*),           intent(in)  :: file
  logical,      optional, intent(in)  :: binary
  logical,      optional, intent(in)  :: raw
  logical,      optional, intent(in)  :: create
  integer,      optional, intent(out) :: ierr
  logical :: lbinary,lraw,lcreate
  integer :: ierrl,iostat
  integer :: lunit,link_unit
  type(iotk_unit), pointer :: this_unit
  character(iotk_attlenx) :: attr
  character(iotk_fillenx) :: oldfile
  ierrl  = 0
  iostat = 0
  lbinary=.false.
  lraw   =.false.
  lcreate=.false.
  if(present(binary)) lbinary = binary
  if(present(raw))    lraw = raw
  if(present(create)) lcreate = create
  lunit = iotk_phys_unit(unit)
  call iotk_unit_get(lunit,pointer=this_unit)
  if(.not.associated(this_unit)) then
    call iotk_error_issue(ierrl,"iotk_link",__FILE__,__LINE__)
# 164 "iotk_files.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.11 ")
# 164 "iotk_files.spp"
call iotk_error_msg(ierrl,'Links do not apply to units which are not explicitly connected')
    goto 1
  end if
  call iotk_write_attr(attr,"iotk_link",iotk_strtrim(file),ierr=ierrl,first=.true.)
  if(ierrl/=0) then
    call iotk_error_issue(ierrl,"iotk_link",__FILE__,__LINE__)
# 169 "iotk_files.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.11 ")
    goto 1
  end if
  if(lraw) then
    if(lbinary) then
      call iotk_write_attr(attr,"iotk_binary",lbinary,ierr=ierrl)
      if(ierrl/=0) then
        call iotk_error_issue(ierrl,"iotk_link",__FILE__,__LINE__)
# 176 "iotk_files.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.11 ")
        goto 1
      end if
    end if
    call iotk_write_attr(attr,"iotk_raw",lraw,ierr=ierrl)
    if(ierrl/=0) then
      call iotk_error_issue(ierrl,"iotk_link",__FILE__,__LINE__)
# 182 "iotk_files.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.11 ")
      goto 1
    end if
  end if
  call iotk_write_begin(unit,name,attr,ierr=ierrl)
  if(ierrl/=0) then
    call iotk_error_issue(ierrl,"iotk_link",__FILE__,__LINE__)
# 188 "iotk_files.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.11 ")
    goto 1
  end if
  call iotk_write_comment(unit,"This is a link to the file indicated in the iotk_link attribute",ierr=ierrl)
  if(ierrl/=0) then
    call iotk_error_issue(ierrl,"iotk_link",__FILE__,__LINE__)
# 193 "iotk_files.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.11 ")
    goto 1
  end if
  call iotk_write_end  (unit,name,ierr=ierrl)
  if(ierrl/=0) then
    call iotk_error_issue(ierrl,"iotk_link",__FILE__,__LINE__)
# 198 "iotk_files.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.11 ")
    goto 1
  end if
  if(lcreate) then
    call iotk_free_unit(link_unit,ierrl)
    if(ierrl/=0) then
      call iotk_error_issue(ierrl,"iotk_link",__FILE__,__LINE__)
# 204 "iotk_files.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.11 ")
      goto 1
    end if
    inquire(unit=lunit,name=oldfile,iostat=iostat)
    if(iostat/=0) then
      call iotk_error_issue(ierrl,"iotk_link",__FILE__,__LINE__)
# 209 "iotk_files.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.11 ")
# 209 "iotk_files.spp"
call iotk_error_msg(ierrl,'Error inquiring')
# 209 "iotk_files.spp"
call iotk_error_write(ierrl,"unit",lunit)
# 209 "iotk_files.spp"
call iotk_error_write(ierrl,"file",trim(oldfile))
# 209 "iotk_files.spp"
call iotk_error_write(ierrl,"iostat",iostat)
      goto 1
    end if
    call iotk_open_write(link_unit,file=iotk_complete_filepath(file,trim(oldfile)), &
                                 binary=lbinary,raw=lraw,skip_root=.true.,ierr=ierrl)
    if(ierrl/=0) then
      call iotk_error_issue(ierrl,"iotk_link",__FILE__,__LINE__)
# 215 "iotk_files.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.11 ")
      goto 1
    end if
    call iotk_unit_parent(parent=lunit,son=link_unit,ierr=ierrl)
    if(ierrl/=0) then
      call iotk_error_issue(ierrl,"iotk_link",__FILE__,__LINE__)
# 220 "iotk_files.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.11 ")
      goto 1
    end if
  end if
1 continue
  if(present(ierr)) then
    ierr = ierrl
  else
    if(ierrl/=0) call iotk_error_handler(ierrl)
  end if
end subroutine iotk_link_x

# 233 "iotk_files.spp"
subroutine iotk_open_write_x(unit,file,attr,binary,new,raw,root,skip_root,skip_head,ierr)
  use iotk_base
  use iotk_error_interf
  use iotk_attr_interf
  use iotk_str_interf
  use iotk_write_interf
  use iotk_unit_interf
  use iotk_misc_interf
  implicit none
  integer,                intent(in)  :: unit
  character(*), optional, intent(in)  :: file
  character(*), optional, intent(in)  :: attr
  logical,      optional, intent(in)  :: binary
  logical,      optional, intent(in)  :: new
  logical,      optional, intent(in)  :: raw
  character(*), optional, intent(in)  :: root
  logical,      optional, intent(in)  :: skip_root
  logical,      optional, intent(in)  :: skip_head
  integer,      optional, intent(out) :: ierr
! Opens a file properly
  integer :: iostat
  character(50) :: status,form
  character(iotk_namlenx) :: lroot
  character(iotk_attlenx) :: lattr
  integer :: ierrl
  logical :: lbinary,lraw,lnew,lskip_root,lskip_head
  type (iotk_unit), pointer :: this
  ierrl = 0
  iostat = 0
  lroot = "Root"
  lraw = .false.
  lnew = .false.
  lbinary = .false.
  lskip_root = .false.
  lskip_head = .false.
  if(present(root)) lroot = root
  if(present(raw)) lraw=raw
  if(present(binary)) lbinary = binary
  if(present(new)) lnew = new
  if(present(skip_root)) lskip_root = skip_root
  if(lskip_root) lroot=""
  if(present(skip_head)) lskip_head = skip_head
  if(present(file)) then
    form = "formatted"
    if(lbinary) form = "unformatted"
    status = "unknown"
    if(lnew) status = "new"
    open(unit=unit,file=file,status=status,form=form,position="rewind",iostat=iostat,action="write")
    if(iostat/=0) then
      call iotk_error_issue(ierrl,"iotk_open_write",__FILE__,__LINE__)
# 282 "iotk_files.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.11 ")
# 282 "iotk_files.spp"
call iotk_error_msg(ierrl,'Error opening file')
# 282 "iotk_files.spp"
call iotk_error_write(ierrl,"unit",unit)
# 282 "iotk_files.spp"
call iotk_error_write(ierrl,"file",file)
# 282 "iotk_files.spp"
call iotk_error_write(ierrl,"binary",lbinary)
# 282 "iotk_files.spp"
call iotk_error_write(ierrl,"new",lnew)
# 282 "iotk_files.spp"
call iotk_error_write(ierrl,"iostat",iostat)
      goto 1
    end if
  else
    call iotk_inquire(unit,binary=lbinary,ierr=ierrl)
    if(ierrl/=0) then
      call iotk_error_issue(ierrl,"iotk_open_write",__FILE__,__LINE__)
# 288 "iotk_files.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.11 ")
      goto 1
    end if
  end if
  if(.not.lraw) then
    if(.not.lskip_head) then
      if(.not. lbinary) then
        write(unit,"(a)",iostat=iostat) '<?xml version="1.0"?>'
        if(iostat/=0) then
          call iotk_error_issue(ierrl,"iotk_open_write",__FILE__,__LINE__)
# 297 "iotk_files.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.11 ")
# 297 "iotk_files.spp"
call iotk_error_msg(ierrl,'Error writing XML tag')
# 297 "iotk_files.spp"
call iotk_error_write(ierrl,"unit",unit)
# 297 "iotk_files.spp"
call iotk_error_write(ierrl,"iostat",iostat)
          goto 1
        end if
      end if
      call iotk_write_attr(lattr,"version",trim(iotk_version),first=.true.,ierr=ierrl)
      if(ierrl/=0) then
        call iotk_error_issue(ierrl,"iotk_open_write",__FILE__,__LINE__)
# 303 "iotk_files.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.11 ")
# 303 "iotk_files.spp"
call iotk_error_msg(ierrl,'Error writing version attribute')
        goto 1
      end if
      call iotk_write_pi(unit,"iotk",lattr,ierr=ierrl)
      if(ierrl/=0) then
        call iotk_error_issue(ierrl,"iotk_open_write",__FILE__,__LINE__)
# 308 "iotk_files.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.11 ")
# 308 "iotk_files.spp"
call iotk_error_msg(ierrl,'Error writing version tag')
        goto 1
      end if
      call iotk_write_attr(lattr,"file_version",trim(iotk_file_version),first=.true.,ierr=ierrl)
      if(ierrl/=0) then
        call iotk_error_issue(ierrl,"iotk_open_write",__FILE__,__LINE__)
# 313 "iotk_files.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.11 ")
# 313 "iotk_files.spp"
call iotk_error_msg(ierrl,'Error writing file_version attribute')
        goto 1
      end if
      call iotk_write_pi(unit,"iotk",lattr,ierr=ierrl)
      if(ierrl/=0) then
        call iotk_error_issue(ierrl,"iotk_open_write",__FILE__,__LINE__)
# 318 "iotk_files.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.11 ")
# 318 "iotk_files.spp"
call iotk_error_msg(ierrl,'Error writing version tag')
        goto 1
      end if
      call iotk_write_attr(lattr,"binary",lbinary,first=.true.,ierr=ierrl)
      if(ierrl/=0) then
        call iotk_error_issue(ierrl,"iotk_open_write",__FILE__,__LINE__)
# 323 "iotk_files.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.11 ")
# 323 "iotk_files.spp"
call iotk_error_msg(ierrl,'Error writing binary attribute')
        goto 1
      end if
      call iotk_write_pi(unit,"iotk",lattr,ierr=ierrl)
      if(ierrl/=0) then
        call iotk_error_issue(ierrl,"iotk_open_write",__FILE__,__LINE__)
# 328 "iotk_files.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.11 ")
# 328 "iotk_files.spp"
call iotk_error_msg(ierrl,'Error writing binary tag')
        goto 1
      end if
    end if
    if(.not.lskip_root) then
      lattr(1:1) = iotk_eos
      if(present(attr)) then
        call iotk_strcpy(lattr,attr,ierr=ierrl)
        if(ierrl/=0) then
          call iotk_error_issue(ierrl,"iotk_open_write",__FILE__,__LINE__)
# 337 "iotk_files.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.11 ")
# 337 "iotk_files.spp"
call iotk_error_msg(ierrl,'Error writing attributes from the root tag')
          goto 1
        end if
      end if
      call iotk_write_begin(unit,lroot,attr=lattr,ierr=ierrl)
      if(ierrl/=0) then
        call iotk_error_issue(ierrl,"iotk_open_write",__FILE__,__LINE__)
# 343 "iotk_files.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.11 ")
# 343 "iotk_files.spp"
call iotk_error_msg(ierrl,'Error writing the root tag')
        goto 1
      end if
    end if
  end if
  call iotk_unit_add(unit,this,ierr=ierrl)
  if(ierrl/=0) then
    call iotk_error_issue(ierrl,"iotk_open_write",__FILE__,__LINE__)
# 350 "iotk_files.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.11 ")
# 350 "iotk_files.spp"
call iotk_error_msg(ierrl,'Error adding the unit to the list')
    goto 1
  end if
  this%root=lroot
  this%raw=lraw
  this%close_at_end=present(file)
  this%skip_root=lskip_root
  if(lskip_root) this%level = -1
1 continue
  if(present(ierr)) then
    ierr = ierrl
  else
    if(ierrl/=0) call iotk_error_handler(ierrl)
  end if
end subroutine iotk_open_write_x

# 367 "iotk_files.spp"
recursive subroutine iotk_close_write_x(unit,ierr)
  use iotk_base
  use iotk_error_interf
  use iotk_write_interf
  use iotk_unit_interf
  use iotk_misc_interf
  implicit none
  integer,                intent(in)  :: unit
  integer,      optional, intent(out) :: ierr
! Closes a file properly
  logical :: binary
  integer :: ierrl,iostat
  type(iotk_unit), pointer :: this
  nullify(this)
  ierrl = 0
  iostat = 0
  call iotk_unit_get(unit,pointer=this)
  if(.not.associated(this)) then
    call iotk_error_issue(ierrl,"iotk_close_write",__FILE__,__LINE__)
# 385 "iotk_files.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.11 ")
    goto 1
  end if
  call iotk_inquire(unit,binary,ierrl)
  if(ierrl/=0) then
    call iotk_error_issue(ierrl,"iotk_close_write",__FILE__,__LINE__)
# 390 "iotk_files.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.11 ")
    goto 1
  end if
  if(.not.this%raw) then
    if(.not.this%skip_root) then
      call iotk_write_end(unit,this%root,ierr=ierrl)
      if(ierrl/=0) then
        call iotk_error_issue(ierrl,"iotk_close_write",__FILE__,__LINE__)
# 397 "iotk_files.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.11 ")
        goto 1
      end if
    end if
  end if
  if(this%close_at_end) then
    if(.not.binary) then
      write(unit,*,iostat=iostat)
      if(iostat/=0) then
        call iotk_error_issue(ierrl,"iotk_close_write",__FILE__,__LINE__)
# 406 "iotk_files.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.11 ")
# 406 "iotk_files.spp"
call iotk_error_msg(ierrl,'unit')
# 406 "iotk_files.spp"
call iotk_error_write(ierrl,"iostat",iostat)
        goto 1
      end if
    end if
    close(unit,iostat=iostat)
    if(iostat/=0) then
      call iotk_error_issue(ierrl,"iotk_close_write",__FILE__,__LINE__)
# 412 "iotk_files.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.11 ")
# 412 "iotk_files.spp"
call iotk_error_msg(ierrl,'unit')
# 412 "iotk_files.spp"
call iotk_error_write(ierrl,"iostat",iostat)
      goto 1
    end if
  end if
  call iotk_unit_del(unit,ierr=ierrl)
  if(ierrl/=0) then
    call iotk_error_issue(ierrl,"iotk_close_write",__FILE__,__LINE__)
# 418 "iotk_files.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.11 ")
    goto 1
  end if
1 continue
  if(present(ierr)) then
    ierr = ierrl
  else
    if(ierrl/=0) call iotk_error_handler(ierrl)
  end if
end subroutine iotk_close_write_x


# 431 "iotk_files.spp"
subroutine iotk_open_read_x(unit,file,attr,binary,raw,root,ierr)
  use iotk_base
  use iotk_error_interf
  use iotk_str_interf
  use iotk_attr_interf
  use iotk_scan_interf
  use iotk_unit_interf
  use iotk_misc_interf
  use iotk_files_interf
  implicit none
  integer,                intent(in)  :: unit
  character(*), optional, intent(in)  :: file
  logical,      optional, intent(in)  :: binary
  logical,      optional, intent(in)  :: raw
  character(*), optional, intent(out) :: attr
  character(*), optional, intent(out) :: root
  integer,      optional, intent(out) :: ierr
  character(50)           :: status,form
  character(iotk_attlenx) :: lattr
  character(iotk_taglenx) :: tag
  character(iotk_namlenx) :: lroot
  type(iotk_unit),pointer :: this
  integer                 :: ierrl,control,iostat
  logical                 :: lbinary,lraw
  ierrl = 0
  iostat = 0
  lbinary=.false.
  lraw=.false.
  lroot = " "
  lattr(1:1) = iotk_eos
  if(present(raw)) lraw=raw
  if(present(file)) then
    if(present(binary)) lbinary = binary
    if(.not.lbinary .and. .not. lraw) call iotk_magic(file,lbinary)
    form = "formatted"
    if(lbinary) form = "unformatted"
    open(unit=unit,file=trim(file(1:iotk_strlen(file))),status="old",form=form,position="rewind",iostat=iostat,action="read")
    if(iostat/=0) then
      call iotk_error_issue(ierrl,"iotk_open_read",__FILE__,__LINE__)
# 469 "iotk_files.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.11 ")
# 469 "iotk_files.spp"
call iotk_error_msg(ierrl,'unit')
# 469 "iotk_files.spp"
call iotk_error_write(ierrl,"file",file)
# 469 "iotk_files.spp"
call iotk_error_write(ierrl,"binary",lbinary)
# 469 "iotk_files.spp"
call iotk_error_write(ierrl,"iostat",iostat)
      goto 1
    end if
  else
    call iotk_inquire(unit,binary=lbinary,ierr=ierrl)
    if(ierrl/=0) then
      call iotk_error_issue(ierrl,"iotk_open_read",__FILE__,__LINE__)
# 475 "iotk_files.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.11 ")
      goto 1
    end if
  end if
  if(.not.lraw) then
    do
      call iotk_scan_tag(unit,+1,control,tag,lbinary,ierrl)
      if(ierrl/=0) then
        call iotk_error_issue(ierrl,"iotk_open_read",__FILE__,__LINE__)
# 483 "iotk_files.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.11 ")
        goto 1
      end if
      select case(control)
      case(1)
        call iotk_tag_parse(tag,lroot,lattr,ierrl)
        if(ierrl/=0) then
          call iotk_error_issue(ierrl,"iotk_open_read",__FILE__,__LINE__)
# 490 "iotk_files.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.11 ")
          goto 1
        end if
        exit
      case(2:3)
        call iotk_error_issue(ierrl,"iotk_open_read",__FILE__,__LINE__)
# 495 "iotk_files.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.11 ")
# 495 "iotk_files.spp"
call iotk_error_msg(ierrl,'End or empty tag at the beginning of a file')
# 495 "iotk_files.spp"
call iotk_error_write(ierrl,"unit",unit)
# 495 "iotk_files.spp"
call iotk_error_write(ierrl,"file",trim(file(1:iotk_strlen(file))))
# 495 "iotk_files.spp"
call iotk_error_write(ierrl,"binary",lbinary)
# 495 "iotk_files.spp"
call iotk_error_write(ierrl,"iostat",iostat)
        goto 1
      case(5)
        call iotk_tag_parse(tag,lroot,lattr,ierrl)
        if(ierrl/=0) then
          call iotk_error_issue(ierrl,"iotk_open_read",__FILE__,__LINE__)
# 500 "iotk_files.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.11 ")
          goto 1
        end if
        if(iotk_strcomp(lroot,"iotk")) then
          call iotk_check_iotk_attr(unit,lattr,ierrl)
          if(ierrl/=0) then
            call iotk_error_issue(ierrl,"iotk_open_read",__FILE__,__LINE__)
# 506 "iotk_files.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.11 ")
            goto 1
          end if
        end if
      end select
    end do
  end if
  if(present(root)) root = lroot
  if(present(attr)) call iotk_strcpy(attr,lattr,ierrl)
  if(ierrl/=0) then
    call iotk_error_issue(ierrl,"iotk_open_read",__FILE__,__LINE__)
# 516 "iotk_files.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.11 ")
    goto 1
  end if
  call iotk_unit_add(unit,this,ierr=ierrl)
  if(ierrl/=0) then
    call iotk_error_issue(ierrl,"iotk_open_read",__FILE__,__LINE__)
# 521 "iotk_files.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.11 ")
    goto 1
  end if
  this%root=lroot
  this%raw=lraw
  this%close_at_end=present(file)
  this%skip_root=.false.
1 continue
  if(present(ierr)) then
    ierr = ierrl
  else
    if(ierrl/=0) call iotk_error_handler(ierrl)
  end if
end subroutine iotk_open_read_x

# 537 "iotk_files.spp"
subroutine iotk_close_read_x(unit,ierr) 
  use iotk_base
  use iotk_error_interf
  use iotk_scan_interf
  use iotk_unit_interf
  use iotk_misc_interf
  implicit none
  integer,                intent(in)  :: unit
  integer,      optional, intent(out) :: ierr
  integer                 :: ierrl
  integer :: iostat
  type(iotk_unit), pointer :: this
  character(iotk_namlenx) :: root
  logical :: raw
  logical :: close_at_end
  ierrl = 0
  iostat = 0
  call iotk_unit_get(unit,pointer=this)
  if(.not.associated(this)) then
    call iotk_error_issue(ierrl,"iotk_close_read",__FILE__,__LINE__)
# 556 "iotk_files.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.11 ")
    goto 1
  end if
  root = this%root
  close_at_end = this%close_at_end
  raw = this%raw
  call iotk_unit_del(unit,ierr=ierrl)
  if(ierrl/=0) then
    call iotk_error_issue(ierrl,"iotk_close_read",__FILE__,__LINE__)
# 564 "iotk_files.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.11 ")
    goto 1
  end if
  if(.not.raw) then      
    call iotk_scan_end(unit,root,ierr=ierrl)
    if(ierrl/=0) then
      call iotk_error_issue(ierrl,"iotk_close_read",__FILE__,__LINE__)
# 570 "iotk_files.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.11 ")
      goto 1
    end if
  end if
  if(close_at_end) then
    close(unit,iostat=iostat)
    if(iostat/=0) then
      call iotk_error_issue(ierrl,"iotk_close_read",__FILE__,__LINE__)
# 577 "iotk_files.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.11 ")
# 577 "iotk_files.spp"
call iotk_error_msg(ierrl,'unit')
# 577 "iotk_files.spp"
call iotk_error_write(ierrl,"iostat",iostat)
      goto 1
    end if
  end if
1 continue
  if(present(ierr)) then
    ierr = ierrl
  else
    if(ierrl/=0) call iotk_error_handler(ierrl)
  end if
end subroutine iotk_close_read_x

subroutine iotk_magic_x(file,binary)
  use iotk_base
  use iotk_str_interf
  use iotk_error_interf
  use iotk_scan_interf
  use iotk_misc_interf
  use iotk_unit_interf
  use iotk_attr_interf
  character(len=*), intent(in) :: file
  logical,          intent(out):: binary
  integer :: iostat,unit,control,ierrl
  logical :: found,opened
  character(len=iotk_taglenx) :: tag
  character(len=iotk_namlenx) :: name
  character(len=iotk_attlenx) :: attr
  binary=.false.
  call iotk_free_unit(unit)
  open(unit=unit,file=trim(file(1:iotk_strlen(file))),status="old",form="unformatted", &
       position="rewind",iostat=iostat,action="read")
  if(iostat/=0) goto 1
  do
    call iotk_scan_tag(unit,+1,control,tag,.true.,ierrl)
    if(ierrl/=0) goto 1
    if(control==1) then
      exit
    else if(control==5) then
      call iotk_tag_parse(tag,name,attr,ierrl)
      if(iotk_strcomp(name,"iotk")) then
        call iotk_scan_attr(attr,"binary",binary,found=found,ierr=ierrl)
        if(ierrl/=0) goto 1
        if(found) goto 1
      end if
    end if
  end do
1 continue
  if(ierrl/=0) call iotk_error_clear(ierrl)
  inquire(unit=unit,opened=opened)
  if(opened) close(unit,iostat=iostat)
end subroutine iotk_magic_x
