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
# 33 "iotk_tool.spp"

# 36 "iotk_tool.spp"

# 38 "iotk_tool.spp"
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
      else if(iotk_strcomp(arg,"--do-nothing")) then
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
# 89 "iotk_tool.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.18 ")
# 89 "iotk_tool.spp"
call iotk_error_msg(ierrl,'')
          goto 1
        end if
        call iotk_set(linlen=linlen,ierr=ierrl)
        if(ierrl/=0) then
          call iotk_error_issue(ierrl,"iotk_tool",__FILE__,__LINE__)
# 94 "iotk_tool.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.18 ")
# 94 "iotk_tool.spp"
call iotk_error_msg(ierrl,'')
          goto 1
        end if
      else if(arg(1:13)=="--set-indent=") then
        call iotk_atoi(indent,arg(14:iotk_strlen(arg)),check=check)
        if(.not.check) then
          call iotk_error_issue(ierrl,"iotk_tool",__FILE__,__LINE__)
# 100 "iotk_tool.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.18 ")
# 100 "iotk_tool.spp"
call iotk_error_msg(ierrl,'')
          goto 1
        end if
        call iotk_set(indent=indent,ierr=ierrl)
        if(ierrl/=0) then
          call iotk_error_issue(ierrl,"iotk_tool",__FILE__,__LINE__)
# 105 "iotk_tool.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.18 ")
# 105 "iotk_tool.spp"
call iotk_error_msg(ierrl,'')
          goto 1
        end if
      else if(arg(1:16)=="--set-maxindent=") then
        call iotk_atoi(maxindent,arg(17:iotk_strlen(arg)),check=check)
        if(.not.check) then
          call iotk_error_issue(ierrl,"iotk_tool",__FILE__,__LINE__)
# 111 "iotk_tool.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.18 ")
# 111 "iotk_tool.spp"
call iotk_error_msg(ierrl,'')
          goto 1
        end if
        call iotk_set(maxindent=maxindent,ierr=ierrl)
        if(ierrl/=0) then
          call iotk_error_issue(ierrl,"iotk_tool",__FILE__,__LINE__)
# 116 "iotk_tool.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.18 ")
# 116 "iotk_tool.spp"
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
# 129 "iotk_tool.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.18 ")
# 129 "iotk_tool.spp"
call iotk_error_msg(ierrl,'Error converting file')
          goto 1
        end if
      else if(iotk_strcomp(arg,"dump")) then
        call iotk_tool_dump(args(iarg+1:),ierrl)
        if(ierrl/=0) then
          call iotk_error_issue(ierrl,"iotk_tool",__FILE__,__LINE__)
# 135 "iotk_tool.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.18 ")
# 135 "iotk_tool.spp"
call iotk_error_msg(ierrl,'Error dumping file')
          goto 1
        end if
      else if(iotk_strcomp(arg,"info")) then
        call iotk_tool_info(args(iarg+1:),ierrl)
        if(ierrl/=0) then
          call iotk_error_issue(ierrl,"iotk_tool",__FILE__,__LINE__)
# 141 "iotk_tool.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.18 ")
# 141 "iotk_tool.spp"
call iotk_error_msg(ierrl,'Error')
          goto 1
        end if
      else if(iotk_strcomp(arg,"man")) then
        call iotk_tool_man(args(iarg+1:),ierrl)
        if(ierrl/=0) then
          call iotk_error_issue(ierrl,"iotk_tool",__FILE__,__LINE__)
# 147 "iotk_tool.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.18 ")
# 147 "iotk_tool.spp"
call iotk_error_msg(ierrl,'Error')
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
    write(iotk_error_unit,"(a)") "  man        to print manual pages"
  end if

  if(print_help_options) then
    write(iotk_error_unit,"(a)") "IOTK options are:"
    write(iotk_error_unit,"(a)") "  --iotk-exe EXE     set the full path of iotk.x executable (first option)"
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
    write(*,"(a)") "Input/Output Tool Kit (IOTK) version: "//trim(iotk_version)
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

# 220 "iotk_tool.spp"
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
# 267 "iotk_tool.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.18 ")
# 267 "iotk_tool.spp"
call iotk_error_msg(ierrl,'Unknown option')
        goto 1
      end if
    else
      if(infile=="") then
        call iotk_strcpy(infile,args(iarg),ierrl)
        if(ierrl/=0) then
          call iotk_error_issue(ierrl,"iotk_tool_convert",__FILE__,__LINE__)
# 274 "iotk_tool.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.18 ")
# 274 "iotk_tool.spp"
call iotk_error_msg(ierrl,'File name too long')
          goto 1
        end if
      else if(outfile=="") then
        call iotk_strcpy(outfile,args(iarg),ierrl)
        if(ierrl/=0) then
          call iotk_error_issue(ierrl,"iotk_tool_convert",__FILE__,__LINE__)
# 280 "iotk_tool.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.18 ")
# 280 "iotk_tool.spp"
call iotk_error_msg(ierrl,'File name too long')
          goto 1
        end if
      else
        call iotk_error_issue(ierrl,"iotk_tool_convert",__FILE__,__LINE__)
# 284 "iotk_tool.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.18 ")
# 284 "iotk_tool.spp"
call iotk_error_msg(ierrl,'Three files. What do you mean?')
        goto 1
      end if
    end if
  end do
  if(outfile=="") then
    call iotk_error_issue(ierrl,"iotk_tool_convert",__FILE__,__LINE__)
# 290 "iotk_tool.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.18 ")
# 290 "iotk_tool.spp"
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


# 322 "iotk_tool.spp"
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
# 347 "iotk_tool.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.18 ")
# 347 "iotk_tool.spp"
call iotk_error_msg(ierrl,'Unknown option')
        goto 1
      end if
    else
      if(infile=="") then
        call iotk_strcpy(infile,args(iarg),ierrl)
        if(ierrl/=0) then
          call iotk_error_issue(ierrl,"iotk_tool_dump",__FILE__,__LINE__)
# 354 "iotk_tool.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.18 ")
# 354 "iotk_tool.spp"
call iotk_error_msg(ierrl,'File name too long')
          goto 1
        end if
      else
        call iotk_error_issue(ierrl,"iotk_tool_dump",__FILE__,__LINE__)
# 358 "iotk_tool.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.18 ")
# 358 "iotk_tool.spp"
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
# 399 "iotk_tool.spp"
#ifdef __IOTK_LOGICAL1
  write(*,"(a)") "  logical(kind="//trim(iotk_itoa(iotk_logical1))//")"
#endif
# 399 "iotk_tool.spp"
#ifdef __IOTK_LOGICAL2
  write(*,"(a)") "  logical(kind="//trim(iotk_itoa(iotk_logical2))//")"
#endif
# 399 "iotk_tool.spp"
#ifdef __IOTK_LOGICAL3
  write(*,"(a)") "  logical(kind="//trim(iotk_itoa(iotk_logical3))//")"
#endif
# 399 "iotk_tool.spp"
#ifdef __IOTK_LOGICAL4
  write(*,"(a)") "  logical(kind="//trim(iotk_itoa(iotk_logical4))//")"
#endif
# 404 "iotk_tool.spp"
#ifdef __IOTK_INTEGER1
  write(*,"(a)") "  integer(kind="//trim(iotk_itoa(iotk_integer1))//")"
#endif
# 404 "iotk_tool.spp"
#ifdef __IOTK_INTEGER2
  write(*,"(a)") "  integer(kind="//trim(iotk_itoa(iotk_integer2))//")"
#endif
# 404 "iotk_tool.spp"
#ifdef __IOTK_INTEGER3
  write(*,"(a)") "  integer(kind="//trim(iotk_itoa(iotk_integer3))//")"
#endif
# 404 "iotk_tool.spp"
#ifdef __IOTK_INTEGER4
  write(*,"(a)") "  integer(kind="//trim(iotk_itoa(iotk_integer4))//")"
#endif
# 409 "iotk_tool.spp"
#ifdef __IOTK_REAL1
  write(*,"(a)") "  real(kind="//trim(iotk_itoa(iotk_real1))//")"
#endif
# 409 "iotk_tool.spp"
#ifdef __IOTK_REAL2
  write(*,"(a)") "  real(kind="//trim(iotk_itoa(iotk_real2))//")"
#endif
# 409 "iotk_tool.spp"
#ifdef __IOTK_REAL3
  write(*,"(a)") "  real(kind="//trim(iotk_itoa(iotk_real3))//")"
#endif
# 409 "iotk_tool.spp"
#ifdef __IOTK_REAL4
  write(*,"(a)") "  real(kind="//trim(iotk_itoa(iotk_real4))//")"
#endif
# 414 "iotk_tool.spp"
#ifdef __IOTK_REAL1
  write(*,"(a)") "  complex(kind="//trim(iotk_itoa(iotk_real1))//")"
#endif
# 414 "iotk_tool.spp"
#ifdef __IOTK_REAL2
  write(*,"(a)") "  complex(kind="//trim(iotk_itoa(iotk_real2))//")"
#endif
# 414 "iotk_tool.spp"
#ifdef __IOTK_REAL3
  write(*,"(a)") "  complex(kind="//trim(iotk_itoa(iotk_real3))//")"
#endif
# 414 "iotk_tool.spp"
#ifdef __IOTK_REAL4
  write(*,"(a)") "  complex(kind="//trim(iotk_itoa(iotk_real4))//")"
#endif
# 418 "iotk_tool.spp"
  write(*,"(a)") "  character(kind="//trim(iotk_itoa(iotk_character1))//")"

1 continue
  if(present(ierr)) then
    ierr = ierrl
  else
    if(ierrl/=0) call iotk_error_handler(ierrl)
  end if
end subroutine iotk_tool_info_x

subroutine iotk_tool_man_x(args,ierr)
  use iotk_base
  use iotk_misc_interf
  use iotk_xtox_interf
  use iotk_error_interf
  use iotk_str_interf
  implicit none
  character(len=*),           intent(in)  :: args(:)
  integer,          optional, intent(out) :: ierr
  character(len=iotk_linlenx) :: keyword
  integer :: ierrl,iarg
  logical :: printme,printlist

  ierrl = 0
  printme = .false.
  printlist = .false.
  keyword(1:1) = iotk_eos

  do iarg = 1 , size(args)
    if(iotk_strcomp(args(iarg)(1:1),"-")) then
      if(iotk_strcomp(args(iarg),"--help")) then
        write(iotk_error_unit,"(a)") "Usage: iotk man [keyword]"
        write(iotk_error_unit,"(a)") "This command prints on stdout the page of the built-in manual associated with the keyword."
        write(iotk_error_unit,"(a)") "If the keyword is not given a list of all the available keywords will be printed."
        goto 1
      else
        call iotk_error_issue(ierrl,"iotk_tool_dump",__FILE__,__LINE__)
# 454 "iotk_tool.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.18 ")
# 454 "iotk_tool.spp"
call iotk_error_msg(ierrl,'Unknown option')
        goto 1
      end if
    else
      if(iotk_strcomp(keyword,"")) then
        call iotk_strcpy(keyword,args(iarg),ierrl)
        if(ierrl/=0) then
          call iotk_error_issue(ierrl,"iotk_tool_dump",__FILE__,__LINE__)
# 461 "iotk_tool.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.18 ")
          goto 1
        end if
      else
        call iotk_error_issue(ierrl,"iotk_tool_dump",__FILE__,__LINE__)
# 465 "iotk_tool.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.18 ")
# 465 "iotk_tool.spp"
call iotk_error_msg(ierrl,'Two keywords. What do you mean?')
        goto 1
      end if
    end if
  end do

  if(iotk_strcomp(keyword,"")) then
    write(iotk_output_unit,"(a)") "List of available pages:"
    printlist = .true.
  end if

  if(printlist) write(iotk_output_unit,"(a)") &
# 476 "iotk_tool.spp"
" intro"
# 476 "iotk_tool.spp"
printme=.false.
# 476 "iotk_tool.spp"
if(iotk_strcomp(keyword,"all")) printme=.true.
# 476 "iotk_tool.spp"
if(iotk_strcomp(keyword,'intro')) printme=.true.
# 476 "iotk_tool.spp"
if(printme) write(iotk_output_unit,"(a)") &
# 476 "iotk_tool.spp"
""
# 476 "iotk_tool.spp"
if(printme) write(iotk_output_unit,"(a)") &
# 476 "iotk_tool.spp"
"IOTK: INTRODUCTION"
# 476 "iotk_tool.spp"
if(printme) write(iotk_output_unit,"(a)") &
# 476 "iotk_tool.spp"
""
# 476 "iotk_tool.spp"
if(printme) write(iotk_output_unit,"(a)") &
# 476 "iotk_tool.spp"
"The input/output tool kit (IOTK) is a FORTRAN90 library intended"
# 476 "iotk_tool.spp"
if(printme) write(iotk_output_unit,"(a)") &
# 476 "iotk_tool.spp"
"to provide an easy access to tagged files formatted using some specific rule."
# 476 "iotk_tool.spp"
if(printme) write(iotk_output_unit,"(a)") &
# 476 "iotk_tool.spp"
"In this context, a tagged file is a file containing tags and data."
# 476 "iotk_tool.spp"
if(printme) write(iotk_output_unit,"(a)") &
# 476 "iotk_tool.spp"
"Tagged files can be textual, in which case a XML-like format is used,"
# 476 "iotk_tool.spp"
if(printme) write(iotk_output_unit,"(a)") &
# 476 "iotk_tool.spp"
"or binary, in which case a special format is used."
# 476 "iotk_tool.spp"
if(printme) write(iotk_output_unit,"(a)") &
# 476 "iotk_tool.spp"
"Note that IOTK is not an XML parser, but it can be used as a writer/parser"
# 476 "iotk_tool.spp"
if(printme) write(iotk_output_unit,"(a)") &
# 476 "iotk_tool.spp"
"for a limited subset of the XML language."
# 476 "iotk_tool.spp"
if(printme) write(iotk_output_unit,"(a)") &
# 476 "iotk_tool.spp"
""
# 476 "iotk_tool.spp"
if(printme) write(iotk_output_unit,"(a)") &
# 476 "iotk_tool.spp"
"To use the IOTK library from a FORTRAN90 source, the user should"
# 476 "iotk_tool.spp"
if(printme) write(iotk_output_unit,"(a)") &
# 476 "iotk_tool.spp"
"use the module 'iotk_module'."
# 476 "iotk_tool.spp"
if(printme) write(iotk_output_unit,"(a)") &
# 476 "iotk_tool.spp"
"To minimize the possibility of name clashes, all public names exported"
# 476 "iotk_tool.spp"
if(printme) write(iotk_output_unit,"(a)") &
# 476 "iotk_tool.spp"
"from this module has the "//'"'//&
# 476 "iotk_tool.spp"
"iotk_"//'"'//&
# 476 "iotk_tool.spp"
" prefix."
# 476 "iotk_tool.spp"
if(printme) write(iotk_output_unit,"(a)") &
# 476 "iotk_tool.spp"
"Communication between user and library is based on"
# 476 "iotk_tool.spp"
if(printme) write(iotk_output_unit,"(a)") &
# 476 "iotk_tool.spp"
"integers, characters and logicals of the default kind (notice that"
# 476 "iotk_tool.spp"
if(printme) write(iotk_output_unit,"(a)") &
# 476 "iotk_tool.spp"
"these kinds can be changed using proper compiler options, so that"
# 476 "iotk_tool.spp"
if(printme) write(iotk_output_unit,"(a)") &
# 476 "iotk_tool.spp"
"the actual kinds depend on how the library was compiled on your machine)."
# 476 "iotk_tool.spp"
if(printme) write(iotk_output_unit,"(a)") &
# 476 "iotk_tool.spp"
"However, the library can handle formatted input/output for"
# 476 "iotk_tool.spp"
if(printme) write(iotk_output_unit,"(a)") &
# 476 "iotk_tool.spp"
"all intrinsic datatypes, kinds and ranks if properly configured."
# 476 "iotk_tool.spp"
if(printme) write(iotk_output_unit,"(a)") &
# 476 "iotk_tool.spp"
"This is obtained interfacing procedures which acts on all kinds,"
# 476 "iotk_tool.spp"
if(printme) write(iotk_output_unit,"(a)") &
# 476 "iotk_tool.spp"
"types and (in almost all cases) ranks. Thus, a single generic"
# 476 "iotk_tool.spp"
if(printme) write(iotk_output_unit,"(a)") &
# 476 "iotk_tool.spp"
"name has to be remembered for each subroutine, and the compiler will"
# 476 "iotk_tool.spp"
if(printme) write(iotk_output_unit,"(a)") &
# 476 "iotk_tool.spp"
"link the correct one dependening on type, kind and rank of the arguments."
# 476 "iotk_tool.spp"
if(printme) write(iotk_output_unit,"(a)") &
# 476 "iotk_tool.spp"
"Backward API compatibility will be mantained (as long as it is possible)"
# 476 "iotk_tool.spp"
if(printme) write(iotk_output_unit,"(a)") &
# 476 "iotk_tool.spp"
"in future versions."
# 476 "iotk_tool.spp"
if(printme) write(iotk_output_unit,"(a)") &
# 476 "iotk_tool.spp"
"Backward file compatibility will be mantained (as long as it is possible) in"
# 476 "iotk_tool.spp"
if(printme) write(iotk_output_unit,"(a)") &
# 476 "iotk_tool.spp"
"future versions."
# 476 "iotk_tool.spp"
if(printme) write(iotk_output_unit,"(a)") &
# 476 "iotk_tool.spp"
"The library writes on files informations about the version of the library."
# 476 "iotk_tool.spp"
if(printme) write(iotk_output_unit,"(a)") &
# 476 "iotk_tool.spp"
"It also writes informations about the version of the file format (file_version)."
# 476 "iotk_tool.spp"
if(printme) write(iotk_output_unit,"(a)") &
# 476 "iotk_tool.spp"
"The later has to be older or equal to the format supported in the actual library."
# 476 "iotk_tool.spp"
if(printme) write(iotk_output_unit,"(a)") &
# 476 "iotk_tool.spp"
""
# 476 "iotk_tool.spp"
if(printlist) write(iotk_output_unit,"(a)") &
# 476 "iotk_tool.spp"
" error_handling"
# 476 "iotk_tool.spp"
printme=.false.
# 476 "iotk_tool.spp"
if(iotk_strcomp(keyword,"all")) printme=.true.
# 476 "iotk_tool.spp"
if(iotk_strcomp(keyword,'error_handling')) printme=.true.
# 476 "iotk_tool.spp"
if(printme) write(iotk_output_unit,"(a)") &
# 476 "iotk_tool.spp"
""
# 476 "iotk_tool.spp"
if(printme) write(iotk_output_unit,"(a)") &
# 476 "iotk_tool.spp"
"IOTK: ERROR HANDLING"
# 476 "iotk_tool.spp"
if(printme) write(iotk_output_unit,"(a)") &
# 476 "iotk_tool.spp"
"The way iotk handles error is sophisticated and allows for a trace back"
# 476 "iotk_tool.spp"
if(printme) write(iotk_output_unit,"(a)") &
# 476 "iotk_tool.spp"
"of the error condition inside the library."
# 476 "iotk_tool.spp"
if(printme) write(iotk_output_unit,"(a)") &
# 476 "iotk_tool.spp"
"Every iotk routines which possibly leads to an error condition has an optional"
# 476 "iotk_tool.spp"
if(printme) write(iotk_output_unit,"(a)") &
# 476 "iotk_tool.spp"
"intent(out) integer argument ierr. The returned value is conventionally"
# 476 "iotk_tool.spp"
if(printme) write(iotk_output_unit,"(a)") &
# 476 "iotk_tool.spp"
"0 when the routine returns correctly, and different from 0 when the routines"
# 476 "iotk_tool.spp"
if(printme) write(iotk_output_unit,"(a)") &
# 476 "iotk_tool.spp"
"raise an error. The value is effectively a handler for a more complex"
# 476 "iotk_tool.spp"
if(printme) write(iotk_output_unit,"(a)") &
# 476 "iotk_tool.spp"
"object containing the error message. When an error is raised in a low-level"
# 476 "iotk_tool.spp"
if(printme) write(iotk_output_unit,"(a)") &
# 476 "iotk_tool.spp"
"iotk routine, a message is written on the error object. Any intermediate routine"
# 476 "iotk_tool.spp"
if(printme) write(iotk_output_unit,"(a)") &
# 476 "iotk_tool.spp"
"can add other messages to the error object, at least the number of the line in"
# 476 "iotk_tool.spp"
if(printme) write(iotk_output_unit,"(a)") &
# 476 "iotk_tool.spp"
"the source file. In this way, the error message contains a complete trace of"
# 476 "iotk_tool.spp"
if(printme) write(iotk_output_unit,"(a)") &
# 476 "iotk_tool.spp"
"the error plus some additional information."
# 476 "iotk_tool.spp"
if(printme) write(iotk_output_unit,"(a)") &
# 476 "iotk_tool.spp"
"At any point in the chain the messages can be exctracted from the error object."
# 476 "iotk_tool.spp"
if(printme) write(iotk_output_unit,"(a)") &
# 476 "iotk_tool.spp"
"At some point in the chain the error is really handled, usually by writing the"
# 476 "iotk_tool.spp"
if(printme) write(iotk_output_unit,"(a)") &
# 476 "iotk_tool.spp"
"message on the appropriate unit and aborting the execution."
# 476 "iotk_tool.spp"
if(printme) write(iotk_output_unit,"(a)") &
# 476 "iotk_tool.spp"
""
# 476 "iotk_tool.spp"
if(printme) write(iotk_output_unit,"(a)") &
# 476 "iotk_tool.spp"
"Scanning routines (iotk_scan_*) have an optional logical argument "//'"'//&
# 476 "iotk_tool.spp"
"found"//'"'//&
# 476 "iotk_tool.spp"
""
# 476 "iotk_tool.spp"
if(printme) write(iotk_output_unit,"(a)") &
# 476 "iotk_tool.spp"
"which returns true or false. When scanning for data, also a "//'"'//&
# 476 "iotk_tool.spp"
"default"//'"'//&
# 476 "iotk_tool.spp"
" argument"
# 476 "iotk_tool.spp"
if(printme) write(iotk_output_unit,"(a)") &
# 476 "iotk_tool.spp"
"can be used. If one of these two argument is present, the searched"
# 476 "iotk_tool.spp"
if(printme) write(iotk_output_unit,"(a)") &
# 476 "iotk_tool.spp"
"object is considered as an optional object. Otherwise, it is considered as a needed object."
# 476 "iotk_tool.spp"
if(printme) write(iotk_output_unit,"(a)") &
# 476 "iotk_tool.spp"
""
# 476 "iotk_tool.spp"
if(printme) write(iotk_output_unit,"(a)") &
# 476 "iotk_tool.spp"
"If the ierr optional argument is absent, the error handling is leaved to the iotk library."
# 476 "iotk_tool.spp"
if(printme) write(iotk_output_unit,"(a)") &
# 476 "iotk_tool.spp"
"In this case, if a needed object is not present, the library handles the error with a"
# 476 "iotk_tool.spp"
if(printme) write(iotk_output_unit,"(a)") &
# 476 "iotk_tool.spp"
"forced stop."
# 476 "iotk_tool.spp"
if(printme) write(iotk_output_unit,"(a)") &
# 476 "iotk_tool.spp"
""
# 476 "iotk_tool.spp"
if(printme) write(iotk_output_unit,"(a)") &
# 476 "iotk_tool.spp"
"If the ierr optional argument is present, it returns an error code."
# 476 "iotk_tool.spp"
if(printme) write(iotk_output_unit,"(a)") &
# 476 "iotk_tool.spp"
"ierr = 0 means that no error has occurred"
# 476 "iotk_tool.spp"
if(printme) write(iotk_output_unit,"(a)") &
# 476 "iotk_tool.spp"
"ierr > 0 means that an error has occurred probably related to file corruption"
# 476 "iotk_tool.spp"
if(printme) write(iotk_output_unit,"(a)") &
# 476 "iotk_tool.spp"
"ierr < 0 means that the item that was searched for has not been found"
# 476 "iotk_tool.spp"
if(printme) write(iotk_output_unit,"(a)") &
# 476 "iotk_tool.spp"
"(it is possible only for scanning routines and only if the"
# 476 "iotk_tool.spp"
if(printme) write(iotk_output_unit,"(a)") &
# 476 "iotk_tool.spp"
"found and the default keywords are both missing, i.e. only for no-optional objetcs)"
# 476 "iotk_tool.spp"
if(printme) write(iotk_output_unit,"(a)") &
# 476 "iotk_tool.spp"
"In scanning routines, if the argument "//'"'//&
# 476 "iotk_tool.spp"
"found"//'"'//&
# 476 "iotk_tool.spp"
" is present it returns .true."
# 476 "iotk_tool.spp"
if(printme) write(iotk_output_unit,"(a)") &
# 476 "iotk_tool.spp"
"if the item has been found, .false. otherwise."
# 476 "iotk_tool.spp"
if(printme) write(iotk_output_unit,"(a)") &
# 476 "iotk_tool.spp"
"If a library routine returns an ierr /= 0 it is STRONGLY RECOMMENDED to"
# 476 "iotk_tool.spp"
if(printme) write(iotk_output_unit,"(a)") &
# 476 "iotk_tool.spp"
"clear that error with "//'"'//&
# 476 "iotk_tool.spp"
"call iotk_error_clear(ierr)"//'"'//&
# 476 "iotk_tool.spp"
" before proceeding."
# 476 "iotk_tool.spp"
if(printme) write(iotk_output_unit,"(a)") &
# 476 "iotk_tool.spp"
"Thus, the final recipe is:"
# 476 "iotk_tool.spp"
if(printme) write(iotk_output_unit,"(a)") &
# 476 "iotk_tool.spp"
"* if you want to handle errors, always use the 'ierr' optional argument."
# 476 "iotk_tool.spp"
if(printme) write(iotk_output_unit,"(a)") &
# 476 "iotk_tool.spp"
"looking at the sign, you will discern between lacking data and file corruption."
# 476 "iotk_tool.spp"
if(printme) write(iotk_output_unit,"(a)") &
# 476 "iotk_tool.spp"
"with iotk_error_print you can obtain a description of the error."
# 476 "iotk_tool.spp"
if(printme) write(iotk_output_unit,"(a)") &
# 476 "iotk_tool.spp"
"* if you want to leave the error handling to the library, don't use"
# 476 "iotk_tool.spp"
if(printme) write(iotk_output_unit,"(a)") &
# 476 "iotk_tool.spp"
"the 'ierr' optional argument."
# 476 "iotk_tool.spp"
if(printme) write(iotk_output_unit,"(a)") &
# 476 "iotk_tool.spp"
"- if the object you are searching is optional, use 'found' or 'default' optional arguments."
# 476 "iotk_tool.spp"
if(printme) write(iotk_output_unit,"(a)") &
# 476 "iotk_tool.spp"
"- if the object you are searching is non-optional, don't use 'found' nor 'default' optional arguments."
# 476 "iotk_tool.spp"
if(printme) write(iotk_output_unit,"(a)") &
# 476 "iotk_tool.spp"
""
# 476 "iotk_tool.spp"
if(printlist) write(iotk_output_unit,"(a)") &
# 476 "iotk_tool.spp"
" binary_and_textual_files"
# 476 "iotk_tool.spp"
printme=.false.
# 476 "iotk_tool.spp"
if(iotk_strcomp(keyword,"all")) printme=.true.
# 476 "iotk_tool.spp"
if(iotk_strcomp(keyword,'binary_and_textual_files')) printme=.true.
# 476 "iotk_tool.spp"
if(printme) write(iotk_output_unit,"(a)") &
# 476 "iotk_tool.spp"
""
# 476 "iotk_tool.spp"
if(printme) write(iotk_output_unit,"(a)") &
# 476 "iotk_tool.spp"
"IOTK: BINARY AND TEXTUAL FILES"
# 476 "iotk_tool.spp"
if(printme) write(iotk_output_unit,"(a)") &
# 476 "iotk_tool.spp"
"Units can be opened on textual or binary files."
# 476 "iotk_tool.spp"
if(printme) write(iotk_output_unit,"(a)") &
# 476 "iotk_tool.spp"
"The word 'binary' is used instead of the fortran 'unformatted' since"
# 476 "iotk_tool.spp"
if(printme) write(iotk_output_unit,"(a)") &
# 476 "iotk_tool.spp"
"using this libray also binary files have a degree of formattation."
# 476 "iotk_tool.spp"
if(printme) write(iotk_output_unit,"(a)") &
# 476 "iotk_tool.spp"
"After a unit has been opened, the library automatically detects"
# 476 "iotk_tool.spp"
if(printme) write(iotk_output_unit,"(a)") &
# 476 "iotk_tool.spp"
"its format through an INQUIRE and acts consequently."
# 476 "iotk_tool.spp"
if(printme) write(iotk_output_unit,"(a)") &
# 476 "iotk_tool.spp"
"Note that the iotk routines check for necessary properties of an opened unit"
# 476 "iotk_tool.spp"
if(printme) write(iotk_output_unit,"(a)") &
# 476 "iotk_tool.spp"
"access="//'"'//&
# 476 "iotk_tool.spp"
"sequential"//'"'//&
# 476 "iotk_tool.spp"
""
# 476 "iotk_tool.spp"
if(printme) write(iotk_output_unit,"(a)") &
# 476 "iotk_tool.spp"
"blank ="//'"'//&
# 476 "iotk_tool.spp"
"null"//'"'//&
# 476 "iotk_tool.spp"
" (only textual i/o)"
# 476 "iotk_tool.spp"
if(printme) write(iotk_output_unit,"(a)") &
# 476 "iotk_tool.spp"
"pad   ="//'"'//&
# 476 "iotk_tool.spp"
"yes"//'"'//&
# 476 "iotk_tool.spp"
"  (only textual i/o)"
# 476 "iotk_tool.spp"
if(printme) write(iotk_output_unit,"(a)") &
# 476 "iotk_tool.spp"
"Moreover, a textual or binary unit can be designed as raw."
# 476 "iotk_tool.spp"
if(printme) write(iotk_output_unit,"(a)") &
# 476 "iotk_tool.spp"
"In that case, no tags are placed on the file and everything"
# 476 "iotk_tool.spp"
if(printme) write(iotk_output_unit,"(a)") &
# 476 "iotk_tool.spp"
"has to be read and written in the same order."
# 476 "iotk_tool.spp"
if(printme) write(iotk_output_unit,"(a)") &
# 476 "iotk_tool.spp"
"This feature is provided for compatibility reasons but it should be"
# 476 "iotk_tool.spp"
if(printme) write(iotk_output_unit,"(a)") &
# 476 "iotk_tool.spp"
"used as few as possible."
# 476 "iotk_tool.spp"
if(printme) write(iotk_output_unit,"(a)") &
# 476 "iotk_tool.spp"
""
# 476 "iotk_tool.spp"
if(printlist) write(iotk_output_unit,"(a)") &
# 476 "iotk_tool.spp"
" optional_arguments"
# 476 "iotk_tool.spp"
printme=.false.
# 476 "iotk_tool.spp"
if(iotk_strcomp(keyword,"all")) printme=.true.
# 476 "iotk_tool.spp"
if(iotk_strcomp(keyword,'optional_arguments')) printme=.true.
# 476 "iotk_tool.spp"
if(printme) write(iotk_output_unit,"(a)") &
# 476 "iotk_tool.spp"
""
# 476 "iotk_tool.spp"
if(printme) write(iotk_output_unit,"(a)") &
# 476 "iotk_tool.spp"
"IOTK: OPTIONAL ARGUMENTS"
# 476 "iotk_tool.spp"
if(printme) write(iotk_output_unit,"(a)") &
# 476 "iotk_tool.spp"
""
# 476 "iotk_tool.spp"
if(printme) write(iotk_output_unit,"(a)") &
# 476 "iotk_tool.spp"
"Most iotk routines accept optional arguments."
# 476 "iotk_tool.spp"
if(printme) write(iotk_output_unit,"(a)") &
# 476 "iotk_tool.spp"
"The calling routine will not compile if the names of the"
# 476 "iotk_tool.spp"
if(printme) write(iotk_output_unit,"(a)") &
# 476 "iotk_tool.spp"
"arguments are not indicated.  For instance, use"
# 476 "iotk_tool.spp"
if(printme) write(iotk_output_unit,"(a)") &
# 476 "iotk_tool.spp"
"call iotk_scan_dat(10,"//'"'//&
# 476 "iotk_tool.spp"
"pippo"//'"'//&
# 476 "iotk_tool.spp"
",aa(:),ierr=ii)"
# 476 "iotk_tool.spp"
if(printme) write(iotk_output_unit,"(a)") &
# 476 "iotk_tool.spp"
"and NOT:"
# 476 "iotk_tool.spp"
if(printme) write(iotk_output_unit,"(a)") &
# 476 "iotk_tool.spp"
"call iotk_scan_dat(10,"//'"'//&
# 476 "iotk_tool.spp"
"pippo"//'"'//&
# 476 "iotk_tool.spp"
",aa(:),ii)"
# 476 "iotk_tool.spp"
if(printme) write(iotk_output_unit,"(a)") &
# 476 "iotk_tool.spp"
"The only exeption is the attr argument, for which the name can be"
# 476 "iotk_tool.spp"
if(printme) write(iotk_output_unit,"(a)") &
# 476 "iotk_tool.spp"
"omitted if it is placed as the first of the optional arguments."
# 476 "iotk_tool.spp"
if(printme) write(iotk_output_unit,"(a)") &
# 476 "iotk_tool.spp"
"In any case, it is better to always explicitly label optional arguments."
# 476 "iotk_tool.spp"
if(printme) write(iotk_output_unit,"(a)") &
# 476 "iotk_tool.spp"
""
# 476 "iotk_tool.spp"
if(printlist) write(iotk_output_unit,"(a)") &
# 476 "iotk_tool.spp"
" basic_writing_routines iotk_write_begin iotk_write_end iotk_write_empty iotk_write_pi iotk_write_comment"
# 476 "iotk_tool.spp"
printme=.false.
# 476 "iotk_tool.spp"
if(iotk_strcomp(keyword,"all")) printme=.true.
# 476 "iotk_tool.spp"
if(iotk_strcomp(keyword,'basic_writing_routines')) printme=.true.
# 476 "iotk_tool.spp"
if(iotk_strcomp(keyword,'iotk_write_begin')) printme=.true.
# 476 "iotk_tool.spp"
if(iotk_strcomp(keyword,'iotk_write_end')) printme=.true.
# 476 "iotk_tool.spp"
if(iotk_strcomp(keyword,'iotk_write_empty')) printme=.true.
# 476 "iotk_tool.spp"
if(iotk_strcomp(keyword,'iotk_write_pi')) printme=.true.
# 476 "iotk_tool.spp"
if(iotk_strcomp(keyword,'iotk_write_comment')) printme=.true.
# 476 "iotk_tool.spp"
if(printme) write(iotk_output_unit,"(a)") &
# 476 "iotk_tool.spp"
""
# 476 "iotk_tool.spp"
if(printme) write(iotk_output_unit,"(a)") &
# 476 "iotk_tool.spp"
"IOTK: BASIC WRITING ROUTINES"
# 476 "iotk_tool.spp"
if(printme) write(iotk_output_unit,"(a)") &
# 476 "iotk_tool.spp"
"iotk_write_begin  (unit,name[,attr][,ierr])"
# 476 "iotk_tool.spp"
if(printme) write(iotk_output_unit,"(a)") &
# 476 "iotk_tool.spp"
"iotk_write_end    (unit,name[,ierr])"
# 476 "iotk_tool.spp"
if(printme) write(iotk_output_unit,"(a)") &
# 476 "iotk_tool.spp"
"iotk_write_empty  (unit,name[,attr][,ierr])"
# 476 "iotk_tool.spp"
if(printme) write(iotk_output_unit,"(a)") &
# 476 "iotk_tool.spp"
"iotk_write_pi     (unit,name[,attr][,ierr])"
# 476 "iotk_tool.spp"
if(printme) write(iotk_output_unit,"(a)") &
# 476 "iotk_tool.spp"
"iotk_write_comment(unit,text[,ierr])"
# 476 "iotk_tool.spp"
if(printme) write(iotk_output_unit,"(a)") &
# 476 "iotk_tool.spp"
"integer,          intent(in) :: unit"
# 476 "iotk_tool.spp"
if(printme) write(iotk_output_unit,"(a)") &
# 476 "iotk_tool.spp"
"character(len=*), intent(in) :: name"
# 476 "iotk_tool.spp"
if(printme) write(iotk_output_unit,"(a)") &
# 476 "iotk_tool.spp"
"character(len=*), intent(in) :: text"
# 476 "iotk_tool.spp"
if(printme) write(iotk_output_unit,"(a)") &
# 476 "iotk_tool.spp"
"character(len=*), intent(in) :: attr"
# 476 "iotk_tool.spp"
if(printme) write(iotk_output_unit,"(a)") &
# 476 "iotk_tool.spp"
"integer,          intent(out):: ierr ! see error_handling page"
# 476 "iotk_tool.spp"
if(printme) write(iotk_output_unit,"(a)") &
# 476 "iotk_tool.spp"
"These routines write a tag named 'name' on fortran unit 'unit'."
# 476 "iotk_tool.spp"
if(printme) write(iotk_output_unit,"(a)") &
# 476 "iotk_tool.spp"
"The type of the tag is determined from the name of the routine:"
# 476 "iotk_tool.spp"
if(printme) write(iotk_output_unit,"(a)") &
# 476 "iotk_tool.spp"
"iotk_write_begin   => <name attr>"
# 476 "iotk_tool.spp"
if(printme) write(iotk_output_unit,"(a)") &
# 476 "iotk_tool.spp"
"iotk_write_end     => </name>"
# 476 "iotk_tool.spp"
if(printme) write(iotk_output_unit,"(a)") &
# 476 "iotk_tool.spp"
"iotk_write_empty   => <name attr/>"
# 476 "iotk_tool.spp"
if(printme) write(iotk_output_unit,"(a)") &
# 476 "iotk_tool.spp"
"iotk_write_pi      => <?name attr?>"
# 476 "iotk_tool.spp"
if(printme) write(iotk_output_unit,"(a)") &
# 476 "iotk_tool.spp"
"iotk_write_comment => <!--text-->"
# 476 "iotk_tool.spp"
if(printme) write(iotk_output_unit,"(a)") &
# 476 "iotk_tool.spp"
"An optional attribute string can be supplied in 'attr'"
# 476 "iotk_tool.spp"
if(printme) write(iotk_output_unit,"(a)") &
# 476 "iotk_tool.spp"
"In end tags, no attribute is allowed."
# 476 "iotk_tool.spp"
if(printme) write(iotk_output_unit,"(a)") &
# 476 "iotk_tool.spp"
"To build the attribute string, use iotk_write_attr."
# 476 "iotk_tool.spp"
if(printme) write(iotk_output_unit,"(a)") &
# 476 "iotk_tool.spp"
"DON'T TRY TO MANIPULATE THE ATTRIBUTE STRING DIRECTLY!"
# 476 "iotk_tool.spp"
if(printme) write(iotk_output_unit,"(a)") &
# 476 "iotk_tool.spp"
""
# 476 "iotk_tool.spp"
if(printlist) write(iotk_output_unit,"(a)") &
# 476 "iotk_tool.spp"
" basic_scanning_routines iotk_scan_begin iotk_scan_end iotk_scan_empty iotk_scan_pi"
# 476 "iotk_tool.spp"
printme=.false.
# 476 "iotk_tool.spp"
if(iotk_strcomp(keyword,"all")) printme=.true.
# 476 "iotk_tool.spp"
if(iotk_strcomp(keyword,'basic_scanning_routines')) printme=.true.
# 476 "iotk_tool.spp"
if(iotk_strcomp(keyword,'iotk_scan_begin')) printme=.true.
# 476 "iotk_tool.spp"
if(iotk_strcomp(keyword,'iotk_scan_end')) printme=.true.
# 476 "iotk_tool.spp"
if(iotk_strcomp(keyword,'iotk_scan_empty')) printme=.true.
# 476 "iotk_tool.spp"
if(iotk_strcomp(keyword,'iotk_scan_pi')) printme=.true.
# 476 "iotk_tool.spp"
if(printme) write(iotk_output_unit,"(a)") &
# 476 "iotk_tool.spp"
""
# 476 "iotk_tool.spp"
if(printme) write(iotk_output_unit,"(a)") &
# 476 "iotk_tool.spp"
"IOTK: BASIC SCANNING ROUTINES"
# 476 "iotk_tool.spp"
if(printme) write(iotk_output_unit,"(a)") &
# 476 "iotk_tool.spp"
"iotk_scan_begin(unit,name[,attr][,found][,ierr])"
# 476 "iotk_tool.spp"
if(printme) write(iotk_output_unit,"(a)") &
# 476 "iotk_tool.spp"
"iotk_scan_end  (unit,name[,found][,ierr])"
# 476 "iotk_tool.spp"
if(printme) write(iotk_output_unit,"(a)") &
# 476 "iotk_tool.spp"
"iotk_scan_empty(unit,name[,attr][,found][,ierr])"
# 476 "iotk_tool.spp"
if(printme) write(iotk_output_unit,"(a)") &
# 476 "iotk_tool.spp"
"iotk_scan_pi   (unit,name[,attr][,found][,ierr])"
# 476 "iotk_tool.spp"
if(printme) write(iotk_output_unit,"(a)") &
# 476 "iotk_tool.spp"
"integer,          intent(in) :: unit"
# 476 "iotk_tool.spp"
if(printme) write(iotk_output_unit,"(a)") &
# 476 "iotk_tool.spp"
"character(len=*), intent(in) :: name  ! len less or equal iotk_namlenx"
# 476 "iotk_tool.spp"
if(printme) write(iotk_output_unit,"(a)") &
# 476 "iotk_tool.spp"
"character(len=*), intent(out):: attr  ! len possibily equal iotk_attlenx"
# 476 "iotk_tool.spp"
if(printme) write(iotk_output_unit,"(a)") &
# 476 "iotk_tool.spp"
"logical,          intent(out):: found ! see error_handling page"
# 476 "iotk_tool.spp"
if(printme) write(iotk_output_unit,"(a)") &
# 476 "iotk_tool.spp"
"integer,          intent(out):: ierr  ! see error_handling page"
# 476 "iotk_tool.spp"
if(printme) write(iotk_output_unit,"(a)") &
# 476 "iotk_tool.spp"
"These routines scan for a tag named 'name' on fortran unit 'unit'."
# 476 "iotk_tool.spp"
if(printme) write(iotk_output_unit,"(a)") &
# 476 "iotk_tool.spp"
"The type of the tag is determined from the name of the routine:"
# 476 "iotk_tool.spp"
if(printme) write(iotk_output_unit,"(a)") &
# 476 "iotk_tool.spp"
"iotk_scan_begin => <name attr>"
# 476 "iotk_tool.spp"
if(printme) write(iotk_output_unit,"(a)") &
# 476 "iotk_tool.spp"
"iotk_scan_end   => </name>"
# 476 "iotk_tool.spp"
if(printme) write(iotk_output_unit,"(a)") &
# 476 "iotk_tool.spp"
"iotk_scan_empty => <name attr/>"
# 476 "iotk_tool.spp"
if(printme) write(iotk_output_unit,"(a)") &
# 476 "iotk_tool.spp"
"iotk_scan_pi    => <?name attr?>"
# 476 "iotk_tool.spp"
if(printme) write(iotk_output_unit,"(a)") &
# 476 "iotk_tool.spp"
"These routines (except for iotk_scan_end) also fills the"
# 476 "iotk_tool.spp"
if(printme) write(iotk_output_unit,"(a)") &
# 476 "iotk_tool.spp"
"attr string, which can be subsequently decoded with iotk_scan_attr."
# 476 "iotk_tool.spp"
if(printme) write(iotk_output_unit,"(a)") &
# 476 "iotk_tool.spp"
"DON'T TRY TO MANIPULATE THE ATTRIBUTE STRING DIRECTLY!"
# 476 "iotk_tool.spp"
if(printme) write(iotk_output_unit,"(a)") &
# 476 "iotk_tool.spp"
""
# 476 "iotk_tool.spp"
if(printlist) write(iotk_output_unit,"(a)") &
# 476 "iotk_tool.spp"
" writing_attributes iotk_write_attr"
# 476 "iotk_tool.spp"
printme=.false.
# 476 "iotk_tool.spp"
if(iotk_strcomp(keyword,"all")) printme=.true.
# 476 "iotk_tool.spp"
if(iotk_strcomp(keyword,'writing_attributes')) printme=.true.
# 476 "iotk_tool.spp"
if(iotk_strcomp(keyword,'iotk_write_attr')) printme=.true.
# 476 "iotk_tool.spp"
if(printme) write(iotk_output_unit,"(a)") &
# 476 "iotk_tool.spp"
""
# 476 "iotk_tool.spp"
if(printme) write(iotk_output_unit,"(a)") &
# 476 "iotk_tool.spp"
"IOTK: WRITING ATTRIBUTES"
# 476 "iotk_tool.spp"
if(printme) write(iotk_output_unit,"(a)") &
# 476 "iotk_tool.spp"
"iotk_write_attr (attr,name,val[,first][,ierr])"
# 476 "iotk_tool.spp"
if(printme) write(iotk_output_unit,"(a)") &
# 476 "iotk_tool.spp"
"character(len=*), intent(out):: attr  ! len less or equal iotk_namlenx"
# 476 "iotk_tool.spp"
if(printme) write(iotk_output_unit,"(a)") &
# 476 "iotk_tool.spp"
"character(len=*), intent(in) :: name  ! len less or equal iotk_attlenx"
# 476 "iotk_tool.spp"
if(printme) write(iotk_output_unit,"(a)") &
# 476 "iotk_tool.spp"
"TYPE(KIND),       intent(in) :: val   ! any type, any kind, any rank [but only scalars for character]"
# 476 "iotk_tool.spp"
if(printme) write(iotk_output_unit,"(a)") &
# 476 "iotk_tool.spp"
"logical,          intent(in) :: first"
# 476 "iotk_tool.spp"
if(printme) write(iotk_output_unit,"(a)") &
# 476 "iotk_tool.spp"
"integer,          intent(out):: ierr"
# 476 "iotk_tool.spp"
if(printme) write(iotk_output_unit,"(a)") &
# 476 "iotk_tool.spp"
"This routine adds one attribute to the 'attr' string."
# 476 "iotk_tool.spp"
if(printme) write(iotk_output_unit,"(a)") &
# 476 "iotk_tool.spp"
"To clean the string (for the first attribute) use first=.true."
# 476 "iotk_tool.spp"
if(printme) write(iotk_output_unit,"(a)") &
# 476 "iotk_tool.spp"
"Example:"
# 476 "iotk_tool.spp"
if(printme) write(iotk_output_unit,"(a)") &
# 476 "iotk_tool.spp"
"call iotk_write_attr(attr,"//'"'//&
# 476 "iotk_tool.spp"
"pippo"//'"'//&
# 476 "iotk_tool.spp"
",1,first=.true.)"
# 476 "iotk_tool.spp"
if(printme) write(iotk_output_unit,"(a)") &
# 476 "iotk_tool.spp"
"call iotk_write_attr(attr,"//'"'//&
# 476 "iotk_tool.spp"
"paperino"//'"'//&
# 476 "iotk_tool.spp"
",2)"
# 476 "iotk_tool.spp"
if(printme) write(iotk_output_unit,"(a)") &
# 476 "iotk_tool.spp"
"call iotk_write_attr(attr,"//'"'//&
# 476 "iotk_tool.spp"
"pluto"//'"'//&
# 476 "iotk_tool.spp"
",3)"
# 476 "iotk_tool.spp"
if(printme) write(iotk_output_unit,"(a)") &
# 476 "iotk_tool.spp"
"This is equivalent to attr="//'"'//&
# 476 "iotk_tool.spp"
""//'"'//&
# 476 "iotk_tool.spp"
" before the call, but more efficient."
# 476 "iotk_tool.spp"
if(printme) write(iotk_output_unit,"(a)") &
# 476 "iotk_tool.spp"
"The attribute is added in the form name="//'"'//&
# 476 "iotk_tool.spp"
"value"//'"'//&
# 476 "iotk_tool.spp"
","
# 476 "iotk_tool.spp"
if(printme) write(iotk_output_unit,"(a)") &
# 476 "iotk_tool.spp"
"where "//'"'//&
# 476 "iotk_tool.spp"
"value"//'"'//&
# 476 "iotk_tool.spp"
" is a string containing a textual representation"
# 476 "iotk_tool.spp"
if(printme) write(iotk_output_unit,"(a)") &
# 476 "iotk_tool.spp"
"of the val variable."
# 476 "iotk_tool.spp"
if(printme) write(iotk_output_unit,"(a)") &
# 476 "iotk_tool.spp"
"If one of <>&"//'"'//&
# 476 "iotk_tool.spp"
"' appears in val, it is automatically escaped."
# 476 "iotk_tool.spp"
if(printme) write(iotk_output_unit,"(a)") &
# 476 "iotk_tool.spp"
""
# 476 "iotk_tool.spp"
if(printlist) write(iotk_output_unit,"(a)") &
# 476 "iotk_tool.spp"
" scanning_attributes iotk_scan_attr"
# 476 "iotk_tool.spp"
printme=.false.
# 476 "iotk_tool.spp"
if(iotk_strcomp(keyword,"all")) printme=.true.
# 476 "iotk_tool.spp"
if(iotk_strcomp(keyword,'scanning_attributes')) printme=.true.
# 476 "iotk_tool.spp"
if(iotk_strcomp(keyword,'iotk_scan_attr')) printme=.true.
# 476 "iotk_tool.spp"
if(printme) write(iotk_output_unit,"(a)") &
# 476 "iotk_tool.spp"
""
# 476 "iotk_tool.spp"
if(printme) write(iotk_output_unit,"(a)") &
# 476 "iotk_tool.spp"
"IOTK: SCANNING ATTRIBUTES"
# 476 "iotk_tool.spp"
if(printme) write(iotk_output_unit,"(a)") &
# 476 "iotk_tool.spp"
"iotk_scan_attr  (attr,name,val[,found][,default][,eos][,ierr])"
# 476 "iotk_tool.spp"
if(printme) write(iotk_output_unit,"(a)") &
# 476 "iotk_tool.spp"
"character(len=*), intent(in) :: attr    ! len possibily equal iotk_attlenx"
# 476 "iotk_tool.spp"
if(printme) write(iotk_output_unit,"(a)") &
# 476 "iotk_tool.spp"
"character(len=*), intent(in) :: name    ! len less or equal iotk_namlenx"
# 476 "iotk_tool.spp"
if(printme) write(iotk_output_unit,"(a)") &
# 476 "iotk_tool.spp"
"TYPE(KIND),       intent(out):: val     ! any type, any kind, any rank [but only scalars for character]"
# 476 "iotk_tool.spp"
if(printme) write(iotk_output_unit,"(a)") &
# 476 "iotk_tool.spp"
"logical,          intent(out):: found   ! see error_handling page"
# 476 "iotk_tool.spp"
if(printme) write(iotk_output_unit,"(a)") &
# 476 "iotk_tool.spp"
"TYPE(KIND),       intent(in) :: default ! same type, kind and rank as val"
# 476 "iotk_tool.spp"
if(printme) write(iotk_output_unit,"(a)") &
# 476 "iotk_tool.spp"
"logical,          intent(in) :: eos"
# 476 "iotk_tool.spp"
if(printme) write(iotk_output_unit,"(a)") &
# 476 "iotk_tool.spp"
"integer,          intent(out):: ierr    ! see error_handling page"
# 476 "iotk_tool.spp"
if(printme) write(iotk_output_unit,"(a)") &
# 476 "iotk_tool.spp"
"This routine scans for one attribute named 'name' from the 'attr' string."
# 476 "iotk_tool.spp"
if(printme) write(iotk_output_unit,"(a)") &
# 476 "iotk_tool.spp"
"If the attribute is found, it is read to variable 'val'."
# 476 "iotk_tool.spp"
if(printme) write(iotk_output_unit,"(a)") &
# 476 "iotk_tool.spp"
"If it is not found and default is present, default is copied onto val."
# 476 "iotk_tool.spp"
if(printme) write(iotk_output_unit,"(a)") &
# 476 "iotk_tool.spp"
"If TYPE is character and eos is present and true,"
# 476 "iotk_tool.spp"
if(printme) write(iotk_output_unit,"(a)") &
# 476 "iotk_tool.spp"
"an end-of-string terminator will be attached at the end of the read string,"
# 476 "iotk_tool.spp"
if(printme) write(iotk_output_unit,"(a)") &
# 476 "iotk_tool.spp"
"and the following bytes will not be touched. This is faster, but requires"
# 476 "iotk_tool.spp"
if(printme) write(iotk_output_unit,"(a)") &
# 476 "iotk_tool.spp"
"the user to take care directly of the end-of-string. Thus, it is discouraged."
# 476 "iotk_tool.spp"
if(printme) write(iotk_output_unit,"(a)") &
# 476 "iotk_tool.spp"
"The attribute can be delimited with "//'"'//&
# 476 "iotk_tool.spp"
""//'"'//&
# 476 "iotk_tool.spp"
" or with ''"
# 476 "iotk_tool.spp"
if(printme) write(iotk_output_unit,"(a)") &
# 476 "iotk_tool.spp"
""
# 476 "iotk_tool.spp"
if(printlist) write(iotk_output_unit,"(a)") &
# 476 "iotk_tool.spp"
" writing_data iotk_write_dat"
# 476 "iotk_tool.spp"
printme=.false.
# 476 "iotk_tool.spp"
if(iotk_strcomp(keyword,"all")) printme=.true.
# 476 "iotk_tool.spp"
if(iotk_strcomp(keyword,'writing_data')) printme=.true.
# 476 "iotk_tool.spp"
if(iotk_strcomp(keyword,'iotk_write_dat')) printme=.true.
# 476 "iotk_tool.spp"
if(printme) write(iotk_output_unit,"(a)") &
# 476 "iotk_tool.spp"
""
# 476 "iotk_tool.spp"
if(printme) write(iotk_output_unit,"(a)") &
# 476 "iotk_tool.spp"
"IOTK: WRITING DATA"
# 476 "iotk_tool.spp"
if(printme) write(iotk_output_unit,"(a)") &
# 476 "iotk_tool.spp"
"iotk_write_dat  (unit,name,dat[,fmt][,columns][,ierr])"
# 476 "iotk_tool.spp"
if(printme) write(iotk_output_unit,"(a)") &
# 476 "iotk_tool.spp"
"integer,          intent(in) :: unit"
# 476 "iotk_tool.spp"
if(printme) write(iotk_output_unit,"(a)") &
# 476 "iotk_tool.spp"
"character(len=*), intent(in) :: name    ! len less or equal iotk_namlenx"
# 476 "iotk_tool.spp"
if(printme) write(iotk_output_unit,"(a)") &
# 476 "iotk_tool.spp"
"TYPE(KIND),       intent(in) :: dat     ! any type, any kind, any rank"
# 476 "iotk_tool.spp"
if(printme) write(iotk_output_unit,"(a)") &
# 476 "iotk_tool.spp"
"character(len=*), intent(in) :: fmt"
# 476 "iotk_tool.spp"
if(printme) write(iotk_output_unit,"(a)") &
# 476 "iotk_tool.spp"
"integer,          intent(in) :: columns"
# 476 "iotk_tool.spp"
if(printme) write(iotk_output_unit,"(a)") &
# 476 "iotk_tool.spp"
"integer,          intent(out):: ierr    ! see error_handling page"
# 476 "iotk_tool.spp"
if(printme) write(iotk_output_unit,"(a)") &
# 476 "iotk_tool.spp"
"This routines write a data object, that is a self-described"
# 476 "iotk_tool.spp"
if(printme) write(iotk_output_unit,"(a)") &
# 476 "iotk_tool.spp"
"object containg fortran data."
# 476 "iotk_tool.spp"
if(printme) write(iotk_output_unit,"(a)") &
# 476 "iotk_tool.spp"
"A single data object has the following form"
# 476 "iotk_tool.spp"
if(printme) write(iotk_output_unit,"(a)") &
# 476 "iotk_tool.spp"
"<name type="//'"'//&
# 476 "iotk_tool.spp"
"TYPE"//'"'//&
# 476 "iotk_tool.spp"
" kind="//'"'//&
# 476 "iotk_tool.spp"
"KIND"//'"'//&
# 476 "iotk_tool.spp"
" size="//'"'//&
# 476 "iotk_tool.spp"
"SIZE"//'"'//&
# 476 "iotk_tool.spp"
" columns="//'"'//&
# 476 "iotk_tool.spp"
"COLUMNS"//'"'//&
# 476 "iotk_tool.spp"
" len="//'"'//&
# 476 "iotk_tool.spp"
"LEN"//'"'//&
# 476 "iotk_tool.spp"
" fmt="//'"'//&
# 476 "iotk_tool.spp"
"FMT"//'"'//&
# 476 "iotk_tool.spp"
">"
# 476 "iotk_tool.spp"
if(printme) write(iotk_output_unit,"(a)") &
# 476 "iotk_tool.spp"
".. DATA ..."
# 476 "iotk_tool.spp"
if(printme) write(iotk_output_unit,"(a)") &
# 476 "iotk_tool.spp"
"</name>"
# 476 "iotk_tool.spp"
if(printme) write(iotk_output_unit,"(a)") &
# 476 "iotk_tool.spp"
"where"
# 476 "iotk_tool.spp"
if(printme) write(iotk_output_unit,"(a)") &
# 476 "iotk_tool.spp"
"TYPE    is the intrinsic type (logical,integer,real,complex or character),"
# 476 "iotk_tool.spp"
if(printme) write(iotk_output_unit,"(a)") &
# 476 "iotk_tool.spp"
"KIND    is the data kind (stored in binary files only)"
# 476 "iotk_tool.spp"
if(printme) write(iotk_output_unit,"(a)") &
# 476 "iotk_tool.spp"
"SIZE    is the array size (shape informations are not stored)"
# 476 "iotk_tool.spp"
if(printme) write(iotk_output_unit,"(a)") &
# 476 "iotk_tool.spp"
"COLUMNS is the number of data per line"
# 476 "iotk_tool.spp"
if(printme) write(iotk_output_unit,"(a)") &
# 476 "iotk_tool.spp"
"LEN     is the string length"
# 476 "iotk_tool.spp"
if(printme) write(iotk_output_unit,"(a)") &
# 476 "iotk_tool.spp"
"FMT     is a fortran format string used to write data"
# 476 "iotk_tool.spp"
if(printme) write(iotk_output_unit,"(a)") &
# 476 "iotk_tool.spp"
"If the optional 'fmt' is not passed, default format ('columns' element per line)"
# 476 "iotk_tool.spp"
if(printme) write(iotk_output_unit,"(a)") &
# 476 "iotk_tool.spp"
"is used and the fmt attribute is not written. Otherwise, the string"
# 476 "iotk_tool.spp"
if(printme) write(iotk_output_unit,"(a)") &
# 476 "iotk_tool.spp"
"fmt is used as a FORTRAN format specifierfor the write statement. In this"
# 476 "iotk_tool.spp"
if(printme) write(iotk_output_unit,"(a)") &
# 476 "iotk_tool.spp"
"case it is also written on the file (and used for reading the data back)."
# 476 "iotk_tool.spp"
if(printme) write(iotk_output_unit,"(a)") &
# 476 "iotk_tool.spp"
"fmt="//'"'//&
# 476 "iotk_tool.spp"
"*"//'"'//&
# 476 "iotk_tool.spp"
" can be used and correspond to the "//'"'//&
# 476 "iotk_tool.spp"
"write(unit,*)"//'"'//&
# 476 "iotk_tool.spp"
" statement."
# 476 "iotk_tool.spp"
if(printme) write(iotk_output_unit,"(a)") &
# 476 "iotk_tool.spp"
"If the optional 'columns' is not passed, it is assumed to be 1 and"
# 476 "iotk_tool.spp"
if(printme) write(iotk_output_unit,"(a)") &
# 476 "iotk_tool.spp"
"the columns attribute is not written. Note that this attribute is completely"
# 476 "iotk_tool.spp"
if(printme) write(iotk_output_unit,"(a)") &
# 476 "iotk_tool.spp"
"ininfluent when reading."
# 476 "iotk_tool.spp"
if(printme) write(iotk_output_unit,"(a)") &
# 476 "iotk_tool.spp"
"columns and fmt arguments are incompatible."
# 476 "iotk_tool.spp"
if(printme) write(iotk_output_unit,"(a)") &
# 476 "iotk_tool.spp"
"For complex data, one element is a couple of comma separated real numbers."
# 476 "iotk_tool.spp"
if(printme) write(iotk_output_unit,"(a)") &
# 476 "iotk_tool.spp"
"If one of <>& appears in dat, it is escaped."
# 476 "iotk_tool.spp"
if(printme) write(iotk_output_unit,"(a)") &
# 476 "iotk_tool.spp"
""
# 476 "iotk_tool.spp"
if(printlist) write(iotk_output_unit,"(a)") &
# 476 "iotk_tool.spp"
" scanning_data iotk_scan_dat"
# 476 "iotk_tool.spp"
printme=.false.
# 476 "iotk_tool.spp"
if(iotk_strcomp(keyword,"all")) printme=.true.
# 476 "iotk_tool.spp"
if(iotk_strcomp(keyword,'scanning_data')) printme=.true.
# 476 "iotk_tool.spp"
if(iotk_strcomp(keyword,'iotk_scan_dat')) printme=.true.
# 476 "iotk_tool.spp"
if(printme) write(iotk_output_unit,"(a)") &
# 476 "iotk_tool.spp"
""
# 476 "iotk_tool.spp"
if(printme) write(iotk_output_unit,"(a)") &
# 476 "iotk_tool.spp"
"IOTK: SCANNING DATA"
# 476 "iotk_tool.spp"
if(printme) write(iotk_output_unit,"(a)") &
# 476 "iotk_tool.spp"
"iotk_scan_dat  (unit,name,dat[,found][,default][,ierr])"
# 476 "iotk_tool.spp"
if(printme) write(iotk_output_unit,"(a)") &
# 476 "iotk_tool.spp"
"integer,          intent(in) :: unit"
# 476 "iotk_tool.spp"
if(printme) write(iotk_output_unit,"(a)") &
# 476 "iotk_tool.spp"
"character(len=*), intent(in) :: name    ! len less or equal iotk_namlenx"
# 476 "iotk_tool.spp"
if(printme) write(iotk_output_unit,"(a)") &
# 476 "iotk_tool.spp"
"TYPE(KIND),       intent(out):: dat     ! any type, any kind, any rank"
# 476 "iotk_tool.spp"
if(printme) write(iotk_output_unit,"(a)") &
# 476 "iotk_tool.spp"
"logical,          intent(out):: found   ! see error_handling page"
# 476 "iotk_tool.spp"
if(printme) write(iotk_output_unit,"(a)") &
# 476 "iotk_tool.spp"
"TYPE(KIND),       intent(in) :: default ! same type, kind and rank as dat"
# 476 "iotk_tool.spp"
if(printme) write(iotk_output_unit,"(a)") &
# 476 "iotk_tool.spp"
"integer,          intent(out):: ierr    ! see error_handling page"
# 476 "iotk_tool.spp"
if(printme) write(iotk_output_unit,"(a)") &
# 476 "iotk_tool.spp"
"A data object written with iotk_write_dat is read."
# 476 "iotk_tool.spp"
if(printme) write(iotk_output_unit,"(a)") &
# 476 "iotk_tool.spp"
"If it is not found and default is present, default is copied onto dat."
# 476 "iotk_tool.spp"
if(printme) write(iotk_output_unit,"(a)") &
# 476 "iotk_tool.spp"
"If a keyword is absent in the file, the value is deduced from the"
# 476 "iotk_tool.spp"
if(printme) write(iotk_output_unit,"(a)") &
# 476 "iotk_tool.spp"
"dat argument and no check is performed. This allows to write"
# 476 "iotk_tool.spp"
if(printme) write(iotk_output_unit,"(a)") &
# 476 "iotk_tool.spp"
"rapidly by hand data objects. For instance"
# 476 "iotk_tool.spp"
if(printme) write(iotk_output_unit,"(a)") &
# 476 "iotk_tool.spp"
"<datum> 1.0 </datum>"
# 476 "iotk_tool.spp"
if(printme) write(iotk_output_unit,"(a)") &
# 476 "iotk_tool.spp"
"can be read with"
# 476 "iotk_tool.spp"
if(printme) write(iotk_output_unit,"(a)") &
# 476 "iotk_tool.spp"
"real :: val"
# 476 "iotk_tool.spp"
if(printme) write(iotk_output_unit,"(a)") &
# 476 "iotk_tool.spp"
"call iotk_scan_dat(unit,"//'"'//&
# 476 "iotk_tool.spp"
"datum"//'"'//&
# 476 "iotk_tool.spp"
",val)"
# 476 "iotk_tool.spp"
if(printme) write(iotk_output_unit,"(a)") &
# 476 "iotk_tool.spp"
"If fmt is not present on file, the default format is used."
# 476 "iotk_tool.spp"
if(printme) write(iotk_output_unit,"(a)") &
# 476 "iotk_tool.spp"
"Types and sizes are checked."
# 476 "iotk_tool.spp"
if(printme) write(iotk_output_unit,"(a)") &
# 476 "iotk_tool.spp"
"Different kinds (for binary i/o) are automatically converted."
# 476 "iotk_tool.spp"
if(printme) write(iotk_output_unit,"(a)") &
# 476 "iotk_tool.spp"
"Length (for characters) are not checked. If strings on files"
# 476 "iotk_tool.spp"
if(printme) write(iotk_output_unit,"(a)") &
# 476 "iotk_tool.spp"
"are longer then len(dat), only the first characters are read; if strings"
# 476 "iotk_tool.spp"
if(printme) write(iotk_output_unit,"(a)") &
# 476 "iotk_tool.spp"
"on files are shorter, dat is padded with blanks."
# 476 "iotk_tool.spp"
if(printme) write(iotk_output_unit,"(a)") &
# 476 "iotk_tool.spp"
""
# 476 "iotk_tool.spp"
if(printlist) write(iotk_output_unit,"(a)") &
# 476 "iotk_tool.spp"
" opening_files iotk_open_write iotk_open_read"
# 476 "iotk_tool.spp"
printme=.false.
# 476 "iotk_tool.spp"
if(iotk_strcomp(keyword,"all")) printme=.true.
# 476 "iotk_tool.spp"
if(iotk_strcomp(keyword,'opening_files')) printme=.true.
# 476 "iotk_tool.spp"
if(iotk_strcomp(keyword,'iotk_open_write')) printme=.true.
# 476 "iotk_tool.spp"
if(iotk_strcomp(keyword,'iotk_open_read')) printme=.true.
# 476 "iotk_tool.spp"
if(printme) write(iotk_output_unit,"(a)") &
# 476 "iotk_tool.spp"
""
# 476 "iotk_tool.spp"
if(printme) write(iotk_output_unit,"(a)") &
# 476 "iotk_tool.spp"
"IOTK: OPENING FILES"
# 476 "iotk_tool.spp"
if(printme) write(iotk_output_unit,"(a)") &
# 476 "iotk_tool.spp"
""
# 476 "iotk_tool.spp"
if(printme) write(iotk_output_unit,"(a)") &
# 476 "iotk_tool.spp"
"iotk_open_write(unit[,file][,attr][,binary][,raw][,new][,root][,ierr])"
# 476 "iotk_tool.spp"
if(printme) write(iotk_output_unit,"(a)") &
# 476 "iotk_tool.spp"
"integer,          intent(in)  :: unit"
# 476 "iotk_tool.spp"
if(printme) write(iotk_output_unit,"(a)") &
# 476 "iotk_tool.spp"
"character(len=*), intent(in)  :: file"
# 476 "iotk_tool.spp"
if(printme) write(iotk_output_unit,"(a)") &
# 476 "iotk_tool.spp"
"character(len=*), intent(in)  :: attr"
# 476 "iotk_tool.spp"
if(printme) write(iotk_output_unit,"(a)") &
# 476 "iotk_tool.spp"
"logical,          intent(in)  :: binary"
# 476 "iotk_tool.spp"
if(printme) write(iotk_output_unit,"(a)") &
# 476 "iotk_tool.spp"
"logical,          intent(in)  :: new"
# 476 "iotk_tool.spp"
if(printme) write(iotk_output_unit,"(a)") &
# 476 "iotk_tool.spp"
"logical,          intent(in)  :: raw"
# 476 "iotk_tool.spp"
if(printme) write(iotk_output_unit,"(a)") &
# 476 "iotk_tool.spp"
"character(len=*), intent(in)  :: root   ! len less or equal iotk_namlenx"
# 476 "iotk_tool.spp"
if(printme) write(iotk_output_unit,"(a)") &
# 476 "iotk_tool.spp"
"integer,          intent(out) :: ierr   ! see error_handling page"
# 476 "iotk_tool.spp"
if(printme) write(iotk_output_unit,"(a)") &
# 476 "iotk_tool.spp"
"If file is present, this routines opens file 'file' on"
# 476 "iotk_tool.spp"
if(printme) write(iotk_output_unit,"(a)") &
# 476 "iotk_tool.spp"
"unit 'unit' with the proper options."
# 476 "iotk_tool.spp"
if(printme) write(iotk_output_unit,"(a)") &
# 476 "iotk_tool.spp"
"If binary is present and true, the file is binary."
# 476 "iotk_tool.spp"
if(printme) write(iotk_output_unit,"(a)") &
# 476 "iotk_tool.spp"
"If new is present and true, the file must not exist already."
# 476 "iotk_tool.spp"
if(printme) write(iotk_output_unit,"(a)") &
# 476 "iotk_tool.spp"
"If raw is present and true, the file is considered as a raw data file"
# 476 "iotk_tool.spp"
if(printme) write(iotk_output_unit,"(a)") &
# 476 "iotk_tool.spp"
"(use of raw data files is discouraged)."
# 476 "iotk_tool.spp"
if(printme) write(iotk_output_unit,"(a)") &
# 476 "iotk_tool.spp"
"If file is not present, unit is assumed to be already connected."
# 476 "iotk_tool.spp"
if(printme) write(iotk_output_unit,"(a)") &
# 476 "iotk_tool.spp"
"If root is present, it is used as the name of the root begin/end tag."
# 476 "iotk_tool.spp"
if(printme) write(iotk_output_unit,"(a)") &
# 476 "iotk_tool.spp"
"If it is absent, the default "//'"'//&
# 476 "iotk_tool.spp"
"Root"//'"'//&
# 476 "iotk_tool.spp"
" is used."
# 476 "iotk_tool.spp"
if(printme) write(iotk_output_unit,"(a)") &
# 476 "iotk_tool.spp"
"An optional attribute string can be supplied in 'attr', and will be used"
# 476 "iotk_tool.spp"
if(printme) write(iotk_output_unit,"(a)") &
# 476 "iotk_tool.spp"
"as an attribute list for the begin root tag."
# 476 "iotk_tool.spp"
if(printme) write(iotk_output_unit,"(a)") &
# 476 "iotk_tool.spp"
"Also informations about iotk version and binary format are written as"
# 476 "iotk_tool.spp"
if(printme) write(iotk_output_unit,"(a)") &
# 476 "iotk_tool.spp"
"pi informations."
# 476 "iotk_tool.spp"
if(printme) write(iotk_output_unit,"(a)") &
# 476 "iotk_tool.spp"
""
# 476 "iotk_tool.spp"
if(printme) write(iotk_output_unit,"(a)") &
# 476 "iotk_tool.spp"
"iotk_open_read(unit[,file][,attr][,binary][,raw][,root][,ierr])"
# 476 "iotk_tool.spp"
if(printme) write(iotk_output_unit,"(a)") &
# 476 "iotk_tool.spp"
"integer,          intent(in)  :: unit"
# 476 "iotk_tool.spp"
if(printme) write(iotk_output_unit,"(a)") &
# 476 "iotk_tool.spp"
"character(len=*), intent(in)  :: file"
# 476 "iotk_tool.spp"
if(printme) write(iotk_output_unit,"(a)") &
# 476 "iotk_tool.spp"
"character(len=*), intent(out) :: attr"
# 476 "iotk_tool.spp"
if(printme) write(iotk_output_unit,"(a)") &
# 476 "iotk_tool.spp"
"logical,          intent(in)  :: binary"
# 476 "iotk_tool.spp"
if(printme) write(iotk_output_unit,"(a)") &
# 476 "iotk_tool.spp"
"logical,          intent(in)  :: raw"
# 476 "iotk_tool.spp"
if(printme) write(iotk_output_unit,"(a)") &
# 476 "iotk_tool.spp"
"character(len=*), intent(out) :: root   ! len possibly equal iotk_namlenx"
# 476 "iotk_tool.spp"
if(printme) write(iotk_output_unit,"(a)") &
# 476 "iotk_tool.spp"
"integer,          intent(out) :: ierr   ! see error_handling page"
# 476 "iotk_tool.spp"
if(printme) write(iotk_output_unit,"(a)") &
# 476 "iotk_tool.spp"
"If file is present, this routines opens file 'file' on"
# 476 "iotk_tool.spp"
if(printme) write(iotk_output_unit,"(a)") &
# 476 "iotk_tool.spp"
"unit 'unit' with the proper options."
# 476 "iotk_tool.spp"
if(printme) write(iotk_output_unit,"(a)") &
# 476 "iotk_tool.spp"
"If binary is present and true, the file is binary."
# 476 "iotk_tool.spp"
if(printme) write(iotk_output_unit,"(a)") &
# 476 "iotk_tool.spp"
"If raw is present and true, the file is considered as a raw data file"
# 476 "iotk_tool.spp"
if(printme) write(iotk_output_unit,"(a)") &
# 476 "iotk_tool.spp"
"(use of raw data files is discouraged)."
# 476 "iotk_tool.spp"
if(printme) write(iotk_output_unit,"(a)") &
# 476 "iotk_tool.spp"
"If file is not present, unit is assumed to be already connected."
# 476 "iotk_tool.spp"
if(printme) write(iotk_output_unit,"(a)") &
# 476 "iotk_tool.spp"
"If root is present, the name of root in file is read onto that variable."
# 476 "iotk_tool.spp"
if(printme) write(iotk_output_unit,"(a)") &
# 476 "iotk_tool.spp"
"If attr is present, the attributes of the root tag are read onto that variable,"
# 476 "iotk_tool.spp"
if(printme) write(iotk_output_unit,"(a)") &
# 476 "iotk_tool.spp"
"which can be subsequently decoded with iotk_scan_attr."
# 476 "iotk_tool.spp"
if(printme) write(iotk_output_unit,"(a)") &
# 476 "iotk_tool.spp"
"DON'T TRY TO MANIPULATE THE ATTRIBUTE STRING DIRECTLY!"
# 476 "iotk_tool.spp"
if(printme) write(iotk_output_unit,"(a)") &
# 476 "iotk_tool.spp"
""
# 476 "iotk_tool.spp"
if(printlist) write(iotk_output_unit,"(a)") &
# 476 "iotk_tool.spp"
" closing_files iotk_close_write iotk_close_read"
# 476 "iotk_tool.spp"
printme=.false.
# 476 "iotk_tool.spp"
if(iotk_strcomp(keyword,"all")) printme=.true.
# 476 "iotk_tool.spp"
if(iotk_strcomp(keyword,'closing_files')) printme=.true.
# 476 "iotk_tool.spp"
if(iotk_strcomp(keyword,'iotk_close_write')) printme=.true.
# 476 "iotk_tool.spp"
if(iotk_strcomp(keyword,'iotk_close_read')) printme=.true.
# 476 "iotk_tool.spp"
if(printme) write(iotk_output_unit,"(a)") &
# 476 "iotk_tool.spp"
""
# 476 "iotk_tool.spp"
if(printme) write(iotk_output_unit,"(a)") &
# 476 "iotk_tool.spp"
"IOTK: CLOSING FILES"
# 476 "iotk_tool.spp"
if(printme) write(iotk_output_unit,"(a)") &
# 476 "iotk_tool.spp"
""
# 476 "iotk_tool.spp"
if(printme) write(iotk_output_unit,"(a)") &
# 476 "iotk_tool.spp"
"iotk_close_write(unit[,ierr])"
# 476 "iotk_tool.spp"
if(printme) write(iotk_output_unit,"(a)") &
# 476 "iotk_tool.spp"
"iotk_close_read(unit[,ierr])"
# 476 "iotk_tool.spp"
if(printme) write(iotk_output_unit,"(a)") &
# 476 "iotk_tool.spp"
"integer,      intent(in)  :: unit"
# 476 "iotk_tool.spp"
if(printme) write(iotk_output_unit,"(a)") &
# 476 "iotk_tool.spp"
"integer,      intent(out) :: ierr ! see error_handling page"
# 476 "iotk_tool.spp"
if(printme) write(iotk_output_unit,"(a)") &
# 476 "iotk_tool.spp"
"This routines close a file opened with iotk_open_*"
# 476 "iotk_tool.spp"
if(printme) write(iotk_output_unit,"(a)") &
# 476 "iotk_tool.spp"
"Note that if the units were already connected before iotk_open_*, they"
# 476 "iotk_tool.spp"
if(printme) write(iotk_output_unit,"(a)") &
# 476 "iotk_tool.spp"
"are left connected here."
# 476 "iotk_tool.spp"
if(printme) write(iotk_output_unit,"(a)") &
# 476 "iotk_tool.spp"
""
# 476 "iotk_tool.spp"
if(printlist) write(iotk_output_unit,"(a)") &
# 476 "iotk_tool.spp"
" multiple_files iotk_link"
# 476 "iotk_tool.spp"
printme=.false.
# 476 "iotk_tool.spp"
if(iotk_strcomp(keyword,"all")) printme=.true.
# 476 "iotk_tool.spp"
if(iotk_strcomp(keyword,'multiple_files')) printme=.true.
# 476 "iotk_tool.spp"
if(iotk_strcomp(keyword,'iotk_link')) printme=.true.
# 476 "iotk_tool.spp"
if(printme) write(iotk_output_unit,"(a)") &
# 476 "iotk_tool.spp"
""
# 476 "iotk_tool.spp"
if(printme) write(iotk_output_unit,"(a)") &
# 476 "iotk_tool.spp"
"IOTK: MULTIPLE FILES"
# 476 "iotk_tool.spp"
if(printme) write(iotk_output_unit,"(a)") &
# 476 "iotk_tool.spp"
""
# 476 "iotk_tool.spp"
if(printme) write(iotk_output_unit,"(a)") &
# 476 "iotk_tool.spp"
"When reading, if a begin tag with an attribute iotk_link="//'"'//&
# 476 "iotk_tool.spp"
"FILENAME"//'"'//&
# 476 "iotk_tool.spp"
" is found,"
# 476 "iotk_tool.spp"
if(printme) write(iotk_output_unit,"(a)") &
# 476 "iotk_tool.spp"
"file FILENAME is mounted in its place"
# 476 "iotk_tool.spp"
if(printme) write(iotk_output_unit,"(a)") &
# 476 "iotk_tool.spp"
"If FILENAME begins with a "//'"'//&
# 476 "iotk_tool.spp"
"/"//'"'//&
# 476 "iotk_tool.spp"
", the path is absolute, otherwise it is relative"
# 476 "iotk_tool.spp"
if(printme) write(iotk_output_unit,"(a)") &
# 476 "iotk_tool.spp"
"to the original file."
# 476 "iotk_tool.spp"
if(printme) write(iotk_output_unit,"(a)") &
# 476 "iotk_tool.spp"
"Note that the mounting is completely transparent for users, which can access"
# 476 "iotk_tool.spp"
if(printme) write(iotk_output_unit,"(a)") &
# 476 "iotk_tool.spp"
"the new file using the old unit. However, if the user wants to access"
# 476 "iotk_tool.spp"
if(printme) write(iotk_output_unit,"(a)") &
# 476 "iotk_tool.spp"
"directly the new file, iotk_physical_unit should be used."
# 476 "iotk_tool.spp"
if(printme) write(iotk_output_unit,"(a)") &
# 476 "iotk_tool.spp"
""
# 476 "iotk_tool.spp"
if(printme) write(iotk_output_unit,"(a)") &
# 476 "iotk_tool.spp"
"When writing, the user can switch a logical unit to a different file using"
# 476 "iotk_tool.spp"
if(printme) write(iotk_output_unit,"(a)") &
# 476 "iotk_tool.spp"
"the following routine"
# 476 "iotk_tool.spp"
if(printme) write(iotk_output_unit,"(a)") &
# 476 "iotk_tool.spp"
""
# 476 "iotk_tool.spp"
if(printme) write(iotk_output_unit,"(a)") &
# 476 "iotk_tool.spp"
"iotk_link(unit,name,file,dummy[,binary][,raw][,create][,ierr])"
# 476 "iotk_tool.spp"
if(printme) write(iotk_output_unit,"(a)") &
# 476 "iotk_tool.spp"
"integer,          intent(in)  :: unit"
# 476 "iotk_tool.spp"
if(printme) write(iotk_output_unit,"(a)") &
# 476 "iotk_tool.spp"
"character(len=*), intent(in)  :: name"
# 476 "iotk_tool.spp"
if(printme) write(iotk_output_unit,"(a)") &
# 476 "iotk_tool.spp"
"character(len=*), intent(in)  :: file"
# 476 "iotk_tool.spp"
if(printme) write(iotk_output_unit,"(a)") &
# 476 "iotk_tool.spp"
"logical,          intent(in)  :: binary"
# 476 "iotk_tool.spp"
if(printme) write(iotk_output_unit,"(a)") &
# 476 "iotk_tool.spp"
"logical,          intent(in)  :: raw"
# 476 "iotk_tool.spp"
if(printme) write(iotk_output_unit,"(a)") &
# 476 "iotk_tool.spp"
"logical,          intent(in)  :: create"
# 476 "iotk_tool.spp"
if(printme) write(iotk_output_unit,"(a)") &
# 476 "iotk_tool.spp"
"integer,          intent(out) :: ierr"
# 476 "iotk_tool.spp"
if(printme) write(iotk_output_unit,"(a)") &
# 476 "iotk_tool.spp"
"name is the name of the tag which represents the link."
# 476 "iotk_tool.spp"
if(printme) write(iotk_output_unit,"(a)") &
# 476 "iotk_tool.spp"
"file is the name of the new file"
# 476 "iotk_tool.spp"
if(printme) write(iotk_output_unit,"(a)") &
# 476 "iotk_tool.spp"
"if binary is present and true, the new file will be binary"
# 476 "iotk_tool.spp"
if(printme) write(iotk_output_unit,"(a)") &
# 476 "iotk_tool.spp"
"if raw is present and true, the new file will be raw"
# 476 "iotk_tool.spp"
if(printme) write(iotk_output_unit,"(a)") &
# 476 "iotk_tool.spp"
"if create is present and true, the new file is actually created"
# 476 "iotk_tool.spp"
if(printme) write(iotk_output_unit,"(a)") &
# 476 "iotk_tool.spp"
"and the next write statement will act on this new file automatically."
# 476 "iotk_tool.spp"
if(printme) write(iotk_output_unit,"(a)") &
# 476 "iotk_tool.spp"
"Otherwise, only the symbolic link is created."
# 476 "iotk_tool.spp"
if(printme) write(iotk_output_unit,"(a)") &
# 476 "iotk_tool.spp"
""
# 476 "iotk_tool.spp"
if(printlist) write(iotk_output_unit,"(a)") &
# 476 "iotk_tool.spp"
" utilities"
# 476 "iotk_tool.spp"
printme=.false.
# 476 "iotk_tool.spp"
if(iotk_strcomp(keyword,"all")) printme=.true.
# 476 "iotk_tool.spp"
if(iotk_strcomp(keyword,'utilities')) printme=.true.
# 476 "iotk_tool.spp"
if(printme) write(iotk_output_unit,"(a)") &
# 476 "iotk_tool.spp"
""
# 476 "iotk_tool.spp"
if(printme) write(iotk_output_unit,"(a)") &
# 476 "iotk_tool.spp"
"IOTK: OTHER UTILITIES"
# 476 "iotk_tool.spp"
if(printme) write(iotk_output_unit,"(a)") &
# 476 "iotk_tool.spp"
""
# 476 "iotk_tool.spp"
if(printme) write(iotk_output_unit,"(a)") &
# 476 "iotk_tool.spp"
"Here a number of additional routines/parameters available"
# 476 "iotk_tool.spp"
if(printme) write(iotk_output_unit,"(a)") &
# 476 "iotk_tool.spp"
"from the iotk_module is listed"
# 476 "iotk_tool.spp"
if(printme) write(iotk_output_unit,"(a)") &
# 476 "iotk_tool.spp"
""
# 476 "iotk_tool.spp"
if(printme) write(iotk_output_unit,"(a)") &
# 476 "iotk_tool.spp"
"character(len=*) :: iotk_index (index)"
# 476 "iotk_tool.spp"
if(printme) write(iotk_output_unit,"(a)") &
# 476 "iotk_tool.spp"
"integer, intent(in) :: index ! scalar or rank 1"
# 476 "iotk_tool.spp"
if(printme) write(iotk_output_unit,"(a)") &
# 476 "iotk_tool.spp"
"Returns a string representing the index in an array."
# 476 "iotk_tool.spp"
if(printme) write(iotk_output_unit,"(a)") &
# 476 "iotk_tool.spp"
"Example: index = (/1,2,3/) => iotk_index = "//'"'//&
# 476 "iotk_tool.spp"
".1.2.3"//'"'//&
# 476 "iotk_tool.spp"
""
# 476 "iotk_tool.spp"
if(printme) write(iotk_output_unit,"(a)") &
# 476 "iotk_tool.spp"
"The correct way for writing an array of derived types is"
# 476 "iotk_tool.spp"
if(printme) write(iotk_output_unit,"(a)") &
# 476 "iotk_tool.spp"
"to build the names as follows"
# 476 "iotk_tool.spp"
if(printme) write(iotk_output_unit,"(a)") &
# 476 "iotk_tool.spp"
"! ONE-DIMENSIONAL ARRAY"
# 476 "iotk_tool.spp"
if(printme) write(iotk_output_unit,"(a)") &
# 476 "iotk_tool.spp"
"do i = 1 , n"
# 476 "iotk_tool.spp"
if(printme) write(iotk_output_unit,"(a)") &
# 476 "iotk_tool.spp"
"call iotk_write_begin(unit,"//'"'//&
# 476 "iotk_tool.spp"
"dummy"//'"'//&
# 476 "iotk_tool.spp"
"//iotk_index(i))"
# 476 "iotk_tool.spp"
if(printme) write(iotk_output_unit,"(a)") &
# 476 "iotk_tool.spp"
"! WRITE THE OBJECT HERE"
# 476 "iotk_tool.spp"
if(printme) write(iotk_output_unit,"(a)") &
# 476 "iotk_tool.spp"
"call iotk_write_end  (unit,"//'"'//&
# 476 "iotk_tool.spp"
"dummy"//'"'//&
# 476 "iotk_tool.spp"
"//iotk_index(i))"
# 476 "iotk_tool.spp"
if(printme) write(iotk_output_unit,"(a)") &
# 476 "iotk_tool.spp"
"end do"
# 476 "iotk_tool.spp"
if(printme) write(iotk_output_unit,"(a)") &
# 476 "iotk_tool.spp"
"do i = 1 , n"
# 476 "iotk_tool.spp"
if(printme) write(iotk_output_unit,"(a)") &
# 476 "iotk_tool.spp"
"do j = 1 , m"
# 476 "iotk_tool.spp"
if(printme) write(iotk_output_unit,"(a)") &
# 476 "iotk_tool.spp"
"! NOTE THE ORDER OF INDEXES, THE FASTER IS THE LAST"
# 476 "iotk_tool.spp"
if(printme) write(iotk_output_unit,"(a)") &
# 476 "iotk_tool.spp"
"call iotk_write_begin(unit,"//'"'//&
# 476 "iotk_tool.spp"
"dummy"//'"'//&
# 476 "iotk_tool.spp"
"//iotk_index((/i,j/)))"
# 476 "iotk_tool.spp"
if(printme) write(iotk_output_unit,"(a)") &
# 476 "iotk_tool.spp"
"! WRITE THE OBJECT HERE"
# 476 "iotk_tool.spp"
if(printme) write(iotk_output_unit,"(a)") &
# 476 "iotk_tool.spp"
"call iotk_write_end  (unit,"//'"'//&
# 476 "iotk_tool.spp"
"dummy"//'"'//&
# 476 "iotk_tool.spp"
"//iotk_index((/i,j/)))"
# 476 "iotk_tool.spp"
if(printme) write(iotk_output_unit,"(a)") &
# 476 "iotk_tool.spp"
"end do"
# 476 "iotk_tool.spp"
if(printme) write(iotk_output_unit,"(a)") &
# 476 "iotk_tool.spp"
"end do"
# 476 "iotk_tool.spp"
if(printme) write(iotk_output_unit,"(a)") &
# 476 "iotk_tool.spp"
""
# 476 "iotk_tool.spp"
if(printme) write(iotk_output_unit,"(a)") &
# 476 "iotk_tool.spp"
"iotk_free_unit(unit[,ierr])"
# 476 "iotk_tool.spp"
if(printme) write(iotk_output_unit,"(a)") &
# 476 "iotk_tool.spp"
"integer, intent(out) :: unit"
# 476 "iotk_tool.spp"
if(printme) write(iotk_output_unit,"(a)") &
# 476 "iotk_tool.spp"
"integer, intent(out) :: ierr"
# 476 "iotk_tool.spp"
if(printme) write(iotk_output_unit,"(a)") &
# 476 "iotk_tool.spp"
"This routine returns the number of a free FORTRAN unit."
# 476 "iotk_tool.spp"
if(printme) write(iotk_output_unit,"(a)") &
# 476 "iotk_tool.spp"
""
# 476 "iotk_tool.spp"
if(printme) write(iotk_output_unit,"(a)") &
# 476 "iotk_tool.spp"
"character(len=*) :: iotk_version"
# 476 "iotk_tool.spp"
if(printme) write(iotk_output_unit,"(a)") &
# 476 "iotk_tool.spp"
"version string of iotk"
# 476 "iotk_tool.spp"
if(printme) write(iotk_output_unit,"(a)") &
# 476 "iotk_tool.spp"
""
# 476 "iotk_tool.spp"
if(printme) write(iotk_output_unit,"(a)") &
# 476 "iotk_tool.spp"
"character :: iotk_newline"
# 476 "iotk_tool.spp"
if(printme) write(iotk_output_unit,"(a)") &
# 476 "iotk_tool.spp"
"newline sequence"
# 476 "iotk_tool.spp"
if(printme) write(iotk_output_unit,"(a)") &
# 476 "iotk_tool.spp"
""
# 476 "iotk_tool.spp"
if(printme) write(iotk_output_unit,"(a)") &
# 476 "iotk_tool.spp"
"character :: iotk_eos"
# 476 "iotk_tool.spp"
if(printme) write(iotk_output_unit,"(a)") &
# 476 "iotk_tool.spp"
"end-of-string character"
# 476 "iotk_tool.spp"
if(printme) write(iotk_output_unit,"(a)") &
# 476 "iotk_tool.spp"
""
# 476 "iotk_tool.spp"
if(printme) write(iotk_output_unit,"(a)") &
# 476 "iotk_tool.spp"
"integer :: iotk_taglenx"
# 476 "iotk_tool.spp"
if(printme) write(iotk_output_unit,"(a)") &
# 476 "iotk_tool.spp"
"max length of a tag"
# 476 "iotk_tool.spp"
if(printme) write(iotk_output_unit,"(a)") &
# 476 "iotk_tool.spp"
""
# 476 "iotk_tool.spp"
if(printme) write(iotk_output_unit,"(a)") &
# 476 "iotk_tool.spp"
"integer :: iotk_namlenx"
# 476 "iotk_tool.spp"
if(printme) write(iotk_output_unit,"(a)") &
# 476 "iotk_tool.spp"
"max length of a tag or attribute name"
# 476 "iotk_tool.spp"
if(printme) write(iotk_output_unit,"(a)") &
# 476 "iotk_tool.spp"
""
# 476 "iotk_tool.spp"
if(printme) write(iotk_output_unit,"(a)") &
# 476 "iotk_tool.spp"
"integer :: iotk_attlenx"
# 476 "iotk_tool.spp"
if(printme) write(iotk_output_unit,"(a)") &
# 476 "iotk_tool.spp"
"max length of the attribute string"
# 476 "iotk_tool.spp"
if(printme) write(iotk_output_unit,"(a)") &
# 476 "iotk_tool.spp"
""
# 476 "iotk_tool.spp"
if(printme) write(iotk_output_unit,"(a)") &
# 476 "iotk_tool.spp"
"integer :: iotk_vallenx"
# 476 "iotk_tool.spp"
if(printme) write(iotk_output_unit,"(a)") &
# 476 "iotk_tool.spp"
"max length of the value of an attribute"
# 476 "iotk_tool.spp"
if(printme) write(iotk_output_unit,"(a)") &
# 476 "iotk_tool.spp"
""
# 476 "iotk_tool.spp"
if(printme) write(iotk_output_unit,"(a)") &
# 476 "iotk_tool.spp"
"integer :: iotk_linlenx"
# 476 "iotk_tool.spp"
if(printme) write(iotk_output_unit,"(a)") &
# 476 "iotk_tool.spp"
"max length of a line in textual files"
# 476 "iotk_tool.spp"
if(printme) write(iotk_output_unit,"(a)") &
# 476 "iotk_tool.spp"
""
# 476 "iotk_tool.spp"
if(printme) write(iotk_output_unit,"(a)") &
# 476 "iotk_tool.spp"
"integer :: iotk_fillenx"
# 476 "iotk_tool.spp"
if(printme) write(iotk_output_unit,"(a)") &
# 476 "iotk_tool.spp"
"max length of a file name"
# 476 "iotk_tool.spp"
if(printme) write(iotk_output_unit,"(a)") &
# 476 "iotk_tool.spp"
""
# 476 "iotk_tool.spp"
if(printme) write(iotk_output_unit,"(a)") &
# 476 "iotk_tool.spp"
"integer :: iotk_header_kind"
# 476 "iotk_tool.spp"
if(printme) write(iotk_output_unit,"(a)") &
# 476 "iotk_tool.spp"
"integer kind of headers in binary files"
# 476 "iotk_tool.spp"
if(printme) write(iotk_output_unit,"(a)") &
# 476 "iotk_tool.spp"
""
# 476 "iotk_tool.spp"
if(printme) write(iotk_output_unit,"(a)") &
# 476 "iotk_tool.spp"
""

  1 continue
  if(present(ierr)) then
    ierr = ierrl
  else
    if(ierrl/=0) call iotk_error_handler(ierrl)
  end if
end subroutine iotk_tool_man_x


