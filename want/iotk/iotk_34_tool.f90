# 1 "iotk_tool.spp"
! Input/Output Tool Kit (IOTK)
! Copyright (C) 2006 Giovanni Bussi
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

# 28 "iotk_tool.spp"
#include "iotk_auxmacros.h"
# 30 "iotk_tool.spp"

# 33 "iotk_tool.spp"

# 35 "iotk_tool.spp"
subroutine iotk_tool_x(args)
  use iotk_base
  use iotk_error_interf
  use iotk_str_interf
  use iotk_tool_interf
  use iotk_xtox_interf
  use iotk_misc_interf
  implicit none
  character(len=*), intent(in) :: args(:)
  integer :: iarg,ierrl
  character(iotk_linlenx) :: arg
  logical :: print_help_options,print_help_commands,print_help_basic,print_copyright,print_version
  logical :: check
  integer :: linlen,indent,maxindent
  ierrl = 0
  iarg = 1

  print_version = .false.
  print_help_options  = .false.
  print_help_commands = .false.
  print_help_basic = .false.
  print_copyright = .false.

  if(size(args)==0) then
    print_help_basic = .true.
  end if

  do iarg = 1 , size(args)
    arg = args(iarg)
    if(iotk_strcomp(arg(1:1),"-")) then
! options here
      if(iotk_strcomp(arg,"--help") .or. iotk_strcomp(arg,"-H")) then
        print_help_basic = .true.
        exit
      else if(iotk_strcomp(arg,"--version")) then
        print_version = .true.
        exit
      else if(iotk_strcomp(arg,"--copyright")) then
        print_copyright = .true.
        exit
      else if(iotk_strcomp(arg,"--help-options")) then
        print_help_options = .true.
        exit
      else if(iotk_strcomp(arg,"--help-commands")) then
        print_help_commands = .true.
        exit
      else if(arg(1:13)=="--set-linlen=") then
        call iotk_atoi(linlen,arg(14:iotk_strlen(arg)),check=check)
        if(.not.check) then
          call iotk_error_issue(ierrl,"iotk_tool",__FILE__,__LINE__)
# 84 "iotk_tool.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.14 ")
# 84 "iotk_tool.spp"
call iotk_error_msg(ierrl,'')
          goto 1
        end if
        call iotk_set(linlen=linlen,ierr=ierrl)
        if(ierrl/=0) then
          call iotk_error_issue(ierrl,"iotk_tool",__FILE__,__LINE__)
# 89 "iotk_tool.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.14 ")
# 89 "iotk_tool.spp"
call iotk_error_msg(ierrl,'')
          goto 1
        end if
      else if(arg(1:13)=="--set-indent=") then
        call iotk_atoi(indent,arg(14:iotk_strlen(arg)),check=check)
        if(.not.check) then
          call iotk_error_issue(ierrl,"iotk_tool",__FILE__,__LINE__)
# 95 "iotk_tool.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.14 ")
# 95 "iotk_tool.spp"
call iotk_error_msg(ierrl,'')
          goto 1
        end if
        call iotk_set(indent=indent,ierr=ierrl)
        if(ierrl/=0) then
          call iotk_error_issue(ierrl,"iotk_tool",__FILE__,__LINE__)
# 100 "iotk_tool.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.14 ")
# 100 "iotk_tool.spp"
call iotk_error_msg(ierrl,'')
          goto 1
        end if
      else if(arg(1:16)=="--set-maxindent=") then
        call iotk_atoi(maxindent,arg(17:iotk_strlen(arg)),check=check)
        if(.not.check) then
          call iotk_error_issue(ierrl,"iotk_tool",__FILE__,__LINE__)
# 106 "iotk_tool.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.14 ")
# 106 "iotk_tool.spp"
call iotk_error_msg(ierrl,'')
          goto 1
        end if
        call iotk_set(maxindent=maxindent,ierr=ierrl)
        if(ierrl/=0) then
          call iotk_error_issue(ierrl,"iotk_tool",__FILE__,__LINE__)
# 111 "iotk_tool.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.14 ")
# 111 "iotk_tool.spp"
call iotk_error_msg(ierrl,'')
          goto 1
        end if
      else
        write(iotk_error_unit,"(a)") "unrecognized option `"//arg(1:iotk_strlen(arg))//"'"
        print_help_basic = .true.
        exit
      end if
    else
! commands here
      if(iotk_strcomp(arg,"convert")) then
        call iotk_tool_convert(args(iarg+1:),ierrl)
        if(ierrl/=0) then
          call iotk_error_issue(ierrl,"iotk_tool",__FILE__,__LINE__)
# 124 "iotk_tool.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.14 ")
# 124 "iotk_tool.spp"
call iotk_error_msg(ierrl,'Error converting file')
          goto 1
        end if
      else if(iotk_strcomp(arg,"dump")) then
        call iotk_tool_dump(args(iarg+1:),ierrl)
        if(ierrl/=0) then
          call iotk_error_issue(ierrl,"iotk_tool",__FILE__,__LINE__)
# 130 "iotk_tool.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.14 ")
# 130 "iotk_tool.spp"
call iotk_error_msg(ierrl,'Error converting file')
          goto 1
        end if
      else if(iotk_strcomp(arg,"info")) then
        call iotk_tool_info(args(iarg+1:),ierrl)
        if(ierrl/=0) then
          call iotk_error_issue(ierrl,"iotk_tool",__FILE__,__LINE__)
# 136 "iotk_tool.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.14 ")
# 136 "iotk_tool.spp"
call iotk_error_msg(ierrl,'Error converting file')
          goto 1
        end if
      else
        write(iotk_error_unit,"(a)") "Unknown command: `"//arg(1:iotk_strlen(arg))//"'"
        write(iotk_error_unit,"(a)") ""
        print_help_commands = .true.
      end if
      exit
    end if
  end do

  if(print_help_basic) then
    write(iotk_error_unit,"(a)") "Usage: iotk [iotk-options] command [command-options-and-arguments]"
    write(iotk_error_unit,"(a)") "  where iotk-options are ..."
    write(iotk_error_unit,"(a)") "    (specify --help-options for a list of options)"
    write(iotk_error_unit,"(a)") "  where command is convert, dump, etc."
    write(iotk_error_unit,"(a)") "    (specify --help-commands for a list of commands)"
    write(iotk_error_unit,"(a)") "  where command-options-and-arguments depend on the specific command"
    write(iotk_error_unit,"(a)") "    (specify a command followed by --help for command-specific help)"
    write(iotk_error_unit,"(a)") "  Specify --help to receive this message"
  end if

  if(print_help_commands) then
    write(iotk_error_unit,"(a)") "IOTK commands are:"
    write(iotk_error_unit,"(a)") "  convert    to convert a file"
    write(iotk_error_unit,"(a)") "  dump       to dump a file"
    write(iotk_error_unit,"(a)") "  info       to obtain informations about how iotk was compiled"
  end if

  if(print_help_options) then
    write(iotk_error_unit,"(a)") "IOTK options are:"
    write(iotk_error_unit,"(a)") "  --copyright        print copyright informations"
    write(iotk_error_unit,"(a)") "  --version          print version informations"
    write(iotk_error_unit,"(a)") "  --help             print a short, generic help"
    write(iotk_error_unit,"(a)") "  --help-options     print a list of options (this list)"
    write(iotk_error_unit,"(a)") "  --help-commands    print a list of commands"
    write(iotk_error_unit,"(a)") "  --set-linlen=N     to set the length of an output line"
    write(iotk_error_unit,"(a)") "  --set-indent=N     to set the number of spaces for an indent level"
    write(iotk_error_unit,"(a)") "  --set-maxindent=N  to set the maximum number of spaces when indenting"
  end if

  if(print_version) then
    write(*,"(a)") "IOTK (Input/Output Tool Kit) version: "//trim(iotk_version)
  end if

  if(print_copyright) then
    write(iotk_error_unit,"(a)") "Input/Output Tool Kit (IOTK)"
    write(iotk_error_unit,"(a)") "Copyright (C) 2004-2006 Giovanni Bussi"
    write(iotk_error_unit,"(a)") ""
    write(iotk_error_unit,"(a)") "This library is free software; you can redistribute it and/or"
    write(iotk_error_unit,"(a)") "modify it under the terms of the GNU Lesser General Public"
    write(iotk_error_unit,"(a)") "License as published by the Free Software Foundation; either"
    write(iotk_error_unit,"(a)") "version 2.1 of the License, or (at your option) any later version."
    write(iotk_error_unit,"(a)") ""
    write(iotk_error_unit,"(a)") "This library is distributed in the hope that it will be useful,"
    write(iotk_error_unit,"(a)") "but WITHOUT ANY WARRANTY; without even the implied warranty of"
    write(iotk_error_unit,"(a)") "MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU"
    write(iotk_error_unit,"(a)") "Lesser General Public License for more details."
    write(iotk_error_unit,"(a)") ""
    write(iotk_error_unit,"(a)") "You should have received a copy of the GNU Lesser General Public"
    write(iotk_error_unit,"(a)") "License along with this library; if not, write to the Free Software"
    write(iotk_error_unit,"(a)") "Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA"
  end if

1 continue
  if(ierrl/=0) call iotk_error_handler(ierrl)

end subroutine iotk_tool_x

# 207 "iotk_tool.spp"
subroutine iotk_tool_convert_x(args,ierr)
  use iotk_base
  use iotk_error_interf
  use iotk_str_interf
  use iotk_misc_interf
  use iotk_files_interf
  implicit none
  character(len=*),           intent(in)  :: args(:)
  integer,          optional, intent(out) :: ierr
  integer :: iarg,ierrl,outfile_len
  character(len=iotk_fillenx) :: infile,outfile
  logical :: binary
  character(len=iotk_attlenx) :: attr
  character(len=iotk_taglenx) :: root
  integer :: maxsize
  logical :: autofmt
  infile=""
  outfile=""
  binary=.true.
  maxsize=-1
  ierrl = 0
  autofmt = .true.
  do iarg = 1 , size(args)
    if(iotk_strcomp(args(iarg)(1:1),"-")) then
      if(iotk_strcomp(args(iarg),"--help")) then
        write(iotk_error_unit,"(a)") "Usage: iotk convert [OPTIONS] infile outfile"
        write(iotk_error_unit,"(a)") "Options:"
        write(iotk_error_unit,"(a)") "  --mode=X  set the output file to be X, where X can be"
        write(iotk_error_unit,"(a)") "            'textual', 'binary' or 'auto'."
        write(iotk_error_unit,"(a)") "  -b        equivalent to --mode=binary"
        write(iotk_error_unit,"(a)") "  -t        equivalent to --mode=textual"
        write(iotk_error_unit,"(a)") "This command converts a iotk data file into another iotk data file."
        write(iotk_error_unit,"(a)") "The infile can be textual or binary, and its format is automatically detected."
        write(iotk_error_unit,"(a)") "The outfile can be textual or binary depending on the --mode option."
        write(iotk_error_unit,"(a)") "If the mode is 'auto', the decision is driven by outfile extension,"
        write(iotk_error_unit,"(a)") "i.e. a file matching *.txt of *.xml will be considered textual, otherwise binary"
        goto 1
      else if(iotk_strcomp(args(iarg),"-b") .or. iotk_strcomp(args(iarg),"--mode=binary")) then
        binary = .true.
        autofmt = .false.
      else if(iotk_strcomp(args(iarg),"-t") .or. iotk_strcomp(args(iarg),"--mode=textual")) then
        binary = .false.
        autofmt = .false.
      else if(iotk_strcomp(args(iarg),"--mode=auto")) then
        binary = .true.
        autofmt = .true.
      else
        call iotk_error_issue(ierrl,"iotk_tool_convert",__FILE__,__LINE__)
# 254 "iotk_tool.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.14 ")
# 254 "iotk_tool.spp"
call iotk_error_msg(ierrl,'Unknown option')
        goto 1
      end if
    else
      if(infile=="") then
        call iotk_strcpy(infile,args(iarg),ierrl)
        if(ierrl/=0) then
          call iotk_error_issue(ierrl,"iotk_tool_convert",__FILE__,__LINE__)
# 261 "iotk_tool.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.14 ")
# 261 "iotk_tool.spp"
call iotk_error_msg(ierrl,'File name too long')
          goto 1
        end if
      else if(outfile=="") then
        call iotk_strcpy(outfile,args(iarg),ierrl)
        if(ierrl/=0) then
          call iotk_error_issue(ierrl,"iotk_tool_convert",__FILE__,__LINE__)
# 267 "iotk_tool.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.14 ")
# 267 "iotk_tool.spp"
call iotk_error_msg(ierrl,'File name too long')
          goto 1
        end if
      else
        call iotk_error_issue(ierrl,"iotk_tool_convert",__FILE__,__LINE__)
# 271 "iotk_tool.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.14 ")
# 271 "iotk_tool.spp"
call iotk_error_msg(ierrl,'Three files. What do you mean?')
        goto 1
      end if
    end if
  end do
  if(outfile=="") then
    call iotk_error_issue(ierrl,"iotk_tool_convert",__FILE__,__LINE__)
# 277 "iotk_tool.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.14 ")
# 277 "iotk_tool.spp"
call iotk_error_msg(ierrl,'Convert: bad usage')
    goto 1
  end if

  outfile_len = iotk_strlen(outfile)
  if(outfile_len>3) then
    select case(outfile(outfile_len-3:outfile_len))
    case(".xml")
      binary = .false.
    case(".txt")
      binary = .false.
    case default
      binary = .true.
    end select
  end if

  call iotk_open_read(60,infile,root=root,attr=attr)
  call iotk_open_write(61,outfile,binary=binary,root=root,attr=attr)
  call iotk_copy_tag(60,61,maxsize=-1)
  call iotk_close_write(61)
  call iotk_close_read(60)

1 continue
  if(present(ierr)) then
    ierr = ierrl
  else
    if(ierrl/=0) call iotk_error_handler(ierrl)
  end if
end subroutine iotk_tool_convert_x


# 309 "iotk_tool.spp"
subroutine iotk_tool_dump_x(args,ierr)
  use iotk_base
  use iotk_error_interf
  use iotk_str_interf
  use iotk_misc_interf
  use iotk_files_interf
  implicit none
  character(len=*),           intent(in)  :: args(:)
  integer,          optional, intent(out) :: ierr
  integer :: iarg,ierrl
  character(len=iotk_fillenx) :: infile
  character(len=iotk_attlenx) :: attr
  character(len=iotk_taglenx) :: root
  integer :: maxsize
  infile=""
  maxsize=-1
  ierrl = 0
  do iarg = 1 , size(args)
    if(iotk_strcomp(args(iarg)(1:1),"-")) then
      if(iotk_strcomp(args(iarg),"--help")) then
        write(iotk_error_unit,"(a)") "Usage: iotk dump file"
        write(iotk_error_unit,"(a)") "This command dumps a iotk data file on standard out."
        write(iotk_error_unit,"(a)") "The file can be textual or binary, and its format is automatically detected."
        goto 1
      else
        call iotk_error_issue(ierrl,"iotk_tool_dump",__FILE__,__LINE__)
# 334 "iotk_tool.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.14 ")
# 334 "iotk_tool.spp"
call iotk_error_msg(ierrl,'Unknown option')
        goto 1
      end if
    else
      if(infile=="") then
        call iotk_strcpy(infile,args(iarg),ierrl)
        if(ierrl/=0) then
          call iotk_error_issue(ierrl,"iotk_tool_dump",__FILE__,__LINE__)
# 341 "iotk_tool.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.14 ")
# 341 "iotk_tool.spp"
call iotk_error_msg(ierrl,'File name too long')
          goto 1
        end if
      else
        call iotk_error_issue(ierrl,"iotk_tool_dump",__FILE__,__LINE__)
# 345 "iotk_tool.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.14 ")
# 345 "iotk_tool.spp"
call iotk_error_msg(ierrl,'Two files. What do you mean?')
        goto 1
      end if
    end if
  end do

  call iotk_open_read(60, trim(infile),root=root,attr=attr)
  call iotk_open_write(iotk_output_unit,root=root,attr=attr)
  call iotk_copy_tag(60,iotk_output_unit,maxsize=-1)
  call iotk_close_write(iotk_output_unit)
  call iotk_close_read(60)

1 continue
  if(present(ierr)) then
    ierr = ierrl
  else
    if(ierrl/=0) call iotk_error_handler(ierrl)
  end if
end subroutine iotk_tool_dump_x

subroutine iotk_tool_info_x(args,ierr)
  use iotk_base
  use iotk_misc_interf
  use iotk_xtox_interf
  use iotk_error_interf
  implicit none
  character(len=*),           intent(in)  :: args(:)
  integer,          optional, intent(out) :: ierr
  integer :: ierrl
  ierrl = 0
  write(*,"(a)") "IOTK (Input/Output Tool Kit) version: "//trim(iotk_version)
  write(*,"(a)") "Limits:"
  write(*,"(a)") "  maximum rank (soft limit): "//trim(iotk_itoa(iotk_maxrank))
  write(*,"(a)") "  maximum rank (hard limit): "//trim(iotk_itoa(iotk_maxrank_hard))
  write(*,"(a)") "Special kinds:"
  write(*,"(a)") "  headers in binary files are integer(kind="//trim(iotk_itoa(iotk_header_kind))//")"
  write(*,"(a)") "  default integers are integer(kind="//trim(iotk_itoa(iotk_integer_defkind))//")"
  write(*,"(a)") "  default logicals are logical(kind="//trim(iotk_itoa(iotk_logical_defkind))//")"
  write(*,"(a)") "  default characters are character(kind="//trim(iotk_itoa(iotk_character_defkind))//")"
  write(*,"(a)") "Kinds configured for i/o operations:"
# 386 "iotk_tool.spp"
#ifdef __IOTK_LOGICAL1
  write(*,"(a)") "  logical(kind="//trim(iotk_itoa(iotk_logical1))//")"
#endif
# 386 "iotk_tool.spp"
#ifdef __IOTK_LOGICAL2
  write(*,"(a)") "  logical(kind="//trim(iotk_itoa(iotk_logical2))//")"
#endif
# 386 "iotk_tool.spp"
#ifdef __IOTK_LOGICAL3
  write(*,"(a)") "  logical(kind="//trim(iotk_itoa(iotk_logical3))//")"
#endif
# 386 "iotk_tool.spp"
#ifdef __IOTK_LOGICAL4
  write(*,"(a)") "  logical(kind="//trim(iotk_itoa(iotk_logical4))//")"
#endif
# 391 "iotk_tool.spp"
#ifdef __IOTK_INTEGER1
  write(*,"(a)") "  integer(kind="//trim(iotk_itoa(iotk_integer1))//")"
#endif
# 391 "iotk_tool.spp"
#ifdef __IOTK_INTEGER2
  write(*,"(a)") "  integer(kind="//trim(iotk_itoa(iotk_integer2))//")"
#endif
# 391 "iotk_tool.spp"
#ifdef __IOTK_INTEGER3
  write(*,"(a)") "  integer(kind="//trim(iotk_itoa(iotk_integer3))//")"
#endif
# 391 "iotk_tool.spp"
#ifdef __IOTK_INTEGER4
  write(*,"(a)") "  integer(kind="//trim(iotk_itoa(iotk_integer4))//")"
#endif
# 396 "iotk_tool.spp"
#ifdef __IOTK_REAL1
  write(*,"(a)") "  real(kind="//trim(iotk_itoa(iotk_real1))//")"
#endif
# 396 "iotk_tool.spp"
#ifdef __IOTK_REAL2
  write(*,"(a)") "  real(kind="//trim(iotk_itoa(iotk_real2))//")"
#endif
# 396 "iotk_tool.spp"
#ifdef __IOTK_REAL3
  write(*,"(a)") "  real(kind="//trim(iotk_itoa(iotk_real3))//")"
#endif
# 396 "iotk_tool.spp"
#ifdef __IOTK_REAL4
  write(*,"(a)") "  real(kind="//trim(iotk_itoa(iotk_real4))//")"
#endif
# 401 "iotk_tool.spp"
#ifdef __IOTK_REAL1
  write(*,"(a)") "  complex(kind="//trim(iotk_itoa(iotk_real1))//")"
#endif
# 401 "iotk_tool.spp"
#ifdef __IOTK_REAL2
  write(*,"(a)") "  complex(kind="//trim(iotk_itoa(iotk_real2))//")"
#endif
# 401 "iotk_tool.spp"
#ifdef __IOTK_REAL3
  write(*,"(a)") "  complex(kind="//trim(iotk_itoa(iotk_real3))//")"
#endif
# 401 "iotk_tool.spp"
#ifdef __IOTK_REAL4
  write(*,"(a)") "  complex(kind="//trim(iotk_itoa(iotk_real4))//")"
#endif
# 405 "iotk_tool.spp"
  write(*,"(a)") "  character(kind="//trim(iotk_itoa(iotk_character1))//")"

1 continue
  if(present(ierr)) then
    ierr = ierrl
  else
    if(ierrl/=0) call iotk_error_handler(ierrl)
  end if
end subroutine iotk_tool_info_x

