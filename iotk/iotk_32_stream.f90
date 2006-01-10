# 1 "iotk_stream.spp"
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

!------------------------------------------------------------------------------!
! Inclusion of configuration file
#include "iotk_config.h"
!------------------------------------------------------------------------------!

# 28 "iotk_stream.spp"
#include "iotk_auxmacros.h"
# 30 "iotk_stream.spp"

# 33 "iotk_stream.spp"

subroutine iotk_stream_read_x(unit,header,setpos,getpos,ierr)
  use iotk_base
  use iotk_stream_interf
  implicit none
  integer,                                      intent(in)  :: unit
  integer(__IOTK_HEADER_KIND),                  intent(out) :: header
  integer,                            optional, intent(in)  :: setpos
  integer,                            optional, intent(out) :: getpos
  integer,                            optional, intent(out) :: ierr
  integer :: aa(1)
  call iotk_stream_read(unit,header,aa,setpos,getpos,.true.,ierr)
end subroutine iotk_stream_read_x


# 51 "iotk_stream.spp"

#ifdef __IOTK_LOGICAL1
# 54 "iotk_stream.spp"
subroutine iotk_stream_read_LOGICAL1(unit,header,val,setpos,getpos,noval,ierr)
  use iotk_base
  use iotk_error_interf
  implicit none
  integer,                                      intent(in)  :: unit
  integer(iotk_header_kind),                    intent(out) :: header
  LOGICAL(kind=__IOTK_LOGICAL1),      intent(out) :: val(:)
  integer,                            optional, intent(in)  :: setpos
  integer,                            optional, intent(out) :: getpos
  logical,                            optional, intent(in)  :: noval
  integer,                            optional, intent(out) :: ierr
#ifdef __IOTK_STREAMS
  integer(iotk_record_kind) :: rec,rec1
#endif
  logical :: lnoval
  integer :: lpos,ierrl,iostat
  lnoval = .false.
  if(present(noval)) lnoval = noval
  ierrl = 0
#ifdef __IOTK_STREAMS
  if(present(setpos)) then
    lpos=setpos
  else
    inquire(unit,pos=lpos,iostat=iostat)
    if(iostat/=0) then
      call iotk_error_issue(ierrl,"iotk_stream_read",__FILE__,__LINE__)
# 79 "iotk_stream.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.4 ")
# 79 "iotk_stream.spp"
call iotk_error_msg(ierrl,'""')
# 79 "iotk_stream.spp"
call iotk_error_write(ierrl,"iostat",iostat)
      goto 1
    end if
  end if
  if(present(getpos)) getpos = lpos
  read(unit,pos=lpos,iostat=iostat) rec
  if(iostat/=0) then
    call iotk_error_issue(ierrl,"iotk_stream_read",__FILE__,__LINE__)
# 86 "iotk_stream.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.4 ")
# 86 "iotk_stream.spp"
call iotk_error_msg(ierrl,'""')
# 86 "iotk_stream.spp"
call iotk_error_write(ierrl,"iostat",iostat)
    goto 1
  end if
  read(unit,iostat=iostat) header
  if(iostat/=0) then
    call iotk_error_issue(ierrl,"iotk_stream_read",__FILE__,__LINE__)
# 91 "iotk_stream.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.4 ")
# 91 "iotk_stream.spp"
call iotk_error_msg(ierrl,'""')
# 91 "iotk_stream.spp"
call iotk_error_write(ierrl,"iostat",iostat)
    goto 1
  end if
  if(.not.lnoval) then
    read(unit,iostat=iostat) val
    if(iostat/=0) then
      call iotk_error_issue(ierrl,"iotk_stream_read",__FILE__,__LINE__)
# 97 "iotk_stream.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.4 ")
# 97 "iotk_stream.spp"
call iotk_error_msg(ierrl,'""')
# 97 "iotk_stream.spp"
call iotk_error_write(ierrl,"iostat",iostat)
      goto 1
    end if
  end if
  read(unit,pos=lpos+iotk_record_length+rec,iostat=iostat) rec1
  if(iostat/=0) then
    call iotk_error_issue(ierrl,"iotk_stream_read",__FILE__,__LINE__)
# 103 "iotk_stream.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.4 ")
# 103 "iotk_stream.spp"
call iotk_error_msg(ierrl,'""')
# 103 "iotk_stream.spp"
call iotk_error_write(ierrl,"iostat",iostat)
    goto 1
  end if
  if(rec1/=rec) then
    call iotk_error_issue(ierrl,"iotk_stream_read",__FILE__,__LINE__)
# 107 "iotk_stream.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.4 ")
# 107 "iotk_stream.spp"
call iotk_error_msg(ierrl,'""')
    goto 1
  end if
#else
  call iotk_error_issue(ierrl,"iotk_stream_read",__FILE__,__LINE__)
# 111 "iotk_stream.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.4 ")
# 111 "iotk_stream.spp"
call iotk_error_msg(ierrl,'Streams are not implemented')
#endif
1 continue
  if(present(ierr)) then
    ierr = ierrl
  else
    if(ierrl/=0) call iotk_error_handler(ierrl)
  end if
end subroutine iotk_stream_read_LOGICAL1
#endif
# 51 "iotk_stream.spp"

#ifdef __IOTK_LOGICAL2
# 54 "iotk_stream.spp"
subroutine iotk_stream_read_LOGICAL2(unit,header,val,setpos,getpos,noval,ierr)
  use iotk_base
  use iotk_error_interf
  implicit none
  integer,                                      intent(in)  :: unit
  integer(iotk_header_kind),                    intent(out) :: header
  LOGICAL(kind=__IOTK_LOGICAL2),      intent(out) :: val(:)
  integer,                            optional, intent(in)  :: setpos
  integer,                            optional, intent(out) :: getpos
  logical,                            optional, intent(in)  :: noval
  integer,                            optional, intent(out) :: ierr
#ifdef __IOTK_STREAMS
  integer(iotk_record_kind) :: rec,rec1
#endif
  logical :: lnoval
  integer :: lpos,ierrl,iostat
  lnoval = .false.
  if(present(noval)) lnoval = noval
  ierrl = 0
#ifdef __IOTK_STREAMS
  if(present(setpos)) then
    lpos=setpos
  else
    inquire(unit,pos=lpos,iostat=iostat)
    if(iostat/=0) then
      call iotk_error_issue(ierrl,"iotk_stream_read",__FILE__,__LINE__)
# 79 "iotk_stream.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.4 ")
# 79 "iotk_stream.spp"
call iotk_error_msg(ierrl,'""')
# 79 "iotk_stream.spp"
call iotk_error_write(ierrl,"iostat",iostat)
      goto 1
    end if
  end if
  if(present(getpos)) getpos = lpos
  read(unit,pos=lpos,iostat=iostat) rec
  if(iostat/=0) then
    call iotk_error_issue(ierrl,"iotk_stream_read",__FILE__,__LINE__)
# 86 "iotk_stream.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.4 ")
# 86 "iotk_stream.spp"
call iotk_error_msg(ierrl,'""')
# 86 "iotk_stream.spp"
call iotk_error_write(ierrl,"iostat",iostat)
    goto 1
  end if
  read(unit,iostat=iostat) header
  if(iostat/=0) then
    call iotk_error_issue(ierrl,"iotk_stream_read",__FILE__,__LINE__)
# 91 "iotk_stream.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.4 ")
# 91 "iotk_stream.spp"
call iotk_error_msg(ierrl,'""')
# 91 "iotk_stream.spp"
call iotk_error_write(ierrl,"iostat",iostat)
    goto 1
  end if
  if(.not.lnoval) then
    read(unit,iostat=iostat) val
    if(iostat/=0) then
      call iotk_error_issue(ierrl,"iotk_stream_read",__FILE__,__LINE__)
# 97 "iotk_stream.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.4 ")
# 97 "iotk_stream.spp"
call iotk_error_msg(ierrl,'""')
# 97 "iotk_stream.spp"
call iotk_error_write(ierrl,"iostat",iostat)
      goto 1
    end if
  end if
  read(unit,pos=lpos+iotk_record_length+rec,iostat=iostat) rec1
  if(iostat/=0) then
    call iotk_error_issue(ierrl,"iotk_stream_read",__FILE__,__LINE__)
# 103 "iotk_stream.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.4 ")
# 103 "iotk_stream.spp"
call iotk_error_msg(ierrl,'""')
# 103 "iotk_stream.spp"
call iotk_error_write(ierrl,"iostat",iostat)
    goto 1
  end if
  if(rec1/=rec) then
    call iotk_error_issue(ierrl,"iotk_stream_read",__FILE__,__LINE__)
# 107 "iotk_stream.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.4 ")
# 107 "iotk_stream.spp"
call iotk_error_msg(ierrl,'""')
    goto 1
  end if
#else
  call iotk_error_issue(ierrl,"iotk_stream_read",__FILE__,__LINE__)
# 111 "iotk_stream.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.4 ")
# 111 "iotk_stream.spp"
call iotk_error_msg(ierrl,'Streams are not implemented')
#endif
1 continue
  if(present(ierr)) then
    ierr = ierrl
  else
    if(ierrl/=0) call iotk_error_handler(ierrl)
  end if
end subroutine iotk_stream_read_LOGICAL2
#endif
# 51 "iotk_stream.spp"

#ifdef __IOTK_LOGICAL3
# 54 "iotk_stream.spp"
subroutine iotk_stream_read_LOGICAL3(unit,header,val,setpos,getpos,noval,ierr)
  use iotk_base
  use iotk_error_interf
  implicit none
  integer,                                      intent(in)  :: unit
  integer(iotk_header_kind),                    intent(out) :: header
  LOGICAL(kind=__IOTK_LOGICAL3),      intent(out) :: val(:)
  integer,                            optional, intent(in)  :: setpos
  integer,                            optional, intent(out) :: getpos
  logical,                            optional, intent(in)  :: noval
  integer,                            optional, intent(out) :: ierr
#ifdef __IOTK_STREAMS
  integer(iotk_record_kind) :: rec,rec1
#endif
  logical :: lnoval
  integer :: lpos,ierrl,iostat
  lnoval = .false.
  if(present(noval)) lnoval = noval
  ierrl = 0
#ifdef __IOTK_STREAMS
  if(present(setpos)) then
    lpos=setpos
  else
    inquire(unit,pos=lpos,iostat=iostat)
    if(iostat/=0) then
      call iotk_error_issue(ierrl,"iotk_stream_read",__FILE__,__LINE__)
# 79 "iotk_stream.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.4 ")
# 79 "iotk_stream.spp"
call iotk_error_msg(ierrl,'""')
# 79 "iotk_stream.spp"
call iotk_error_write(ierrl,"iostat",iostat)
      goto 1
    end if
  end if
  if(present(getpos)) getpos = lpos
  read(unit,pos=lpos,iostat=iostat) rec
  if(iostat/=0) then
    call iotk_error_issue(ierrl,"iotk_stream_read",__FILE__,__LINE__)
# 86 "iotk_stream.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.4 ")
# 86 "iotk_stream.spp"
call iotk_error_msg(ierrl,'""')
# 86 "iotk_stream.spp"
call iotk_error_write(ierrl,"iostat",iostat)
    goto 1
  end if
  read(unit,iostat=iostat) header
  if(iostat/=0) then
    call iotk_error_issue(ierrl,"iotk_stream_read",__FILE__,__LINE__)
# 91 "iotk_stream.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.4 ")
# 91 "iotk_stream.spp"
call iotk_error_msg(ierrl,'""')
# 91 "iotk_stream.spp"
call iotk_error_write(ierrl,"iostat",iostat)
    goto 1
  end if
  if(.not.lnoval) then
    read(unit,iostat=iostat) val
    if(iostat/=0) then
      call iotk_error_issue(ierrl,"iotk_stream_read",__FILE__,__LINE__)
# 97 "iotk_stream.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.4 ")
# 97 "iotk_stream.spp"
call iotk_error_msg(ierrl,'""')
# 97 "iotk_stream.spp"
call iotk_error_write(ierrl,"iostat",iostat)
      goto 1
    end if
  end if
  read(unit,pos=lpos+iotk_record_length+rec,iostat=iostat) rec1
  if(iostat/=0) then
    call iotk_error_issue(ierrl,"iotk_stream_read",__FILE__,__LINE__)
# 103 "iotk_stream.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.4 ")
# 103 "iotk_stream.spp"
call iotk_error_msg(ierrl,'""')
# 103 "iotk_stream.spp"
call iotk_error_write(ierrl,"iostat",iostat)
    goto 1
  end if
  if(rec1/=rec) then
    call iotk_error_issue(ierrl,"iotk_stream_read",__FILE__,__LINE__)
# 107 "iotk_stream.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.4 ")
# 107 "iotk_stream.spp"
call iotk_error_msg(ierrl,'""')
    goto 1
  end if
#else
  call iotk_error_issue(ierrl,"iotk_stream_read",__FILE__,__LINE__)
# 111 "iotk_stream.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.4 ")
# 111 "iotk_stream.spp"
call iotk_error_msg(ierrl,'Streams are not implemented')
#endif
1 continue
  if(present(ierr)) then
    ierr = ierrl
  else
    if(ierrl/=0) call iotk_error_handler(ierrl)
  end if
end subroutine iotk_stream_read_LOGICAL3
#endif
# 51 "iotk_stream.spp"

#ifdef __IOTK_LOGICAL4
# 54 "iotk_stream.spp"
subroutine iotk_stream_read_LOGICAL4(unit,header,val,setpos,getpos,noval,ierr)
  use iotk_base
  use iotk_error_interf
  implicit none
  integer,                                      intent(in)  :: unit
  integer(iotk_header_kind),                    intent(out) :: header
  LOGICAL(kind=__IOTK_LOGICAL4),      intent(out) :: val(:)
  integer,                            optional, intent(in)  :: setpos
  integer,                            optional, intent(out) :: getpos
  logical,                            optional, intent(in)  :: noval
  integer,                            optional, intent(out) :: ierr
#ifdef __IOTK_STREAMS
  integer(iotk_record_kind) :: rec,rec1
#endif
  logical :: lnoval
  integer :: lpos,ierrl,iostat
  lnoval = .false.
  if(present(noval)) lnoval = noval
  ierrl = 0
#ifdef __IOTK_STREAMS
  if(present(setpos)) then
    lpos=setpos
  else
    inquire(unit,pos=lpos,iostat=iostat)
    if(iostat/=0) then
      call iotk_error_issue(ierrl,"iotk_stream_read",__FILE__,__LINE__)
# 79 "iotk_stream.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.4 ")
# 79 "iotk_stream.spp"
call iotk_error_msg(ierrl,'""')
# 79 "iotk_stream.spp"
call iotk_error_write(ierrl,"iostat",iostat)
      goto 1
    end if
  end if
  if(present(getpos)) getpos = lpos
  read(unit,pos=lpos,iostat=iostat) rec
  if(iostat/=0) then
    call iotk_error_issue(ierrl,"iotk_stream_read",__FILE__,__LINE__)
# 86 "iotk_stream.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.4 ")
# 86 "iotk_stream.spp"
call iotk_error_msg(ierrl,'""')
# 86 "iotk_stream.spp"
call iotk_error_write(ierrl,"iostat",iostat)
    goto 1
  end if
  read(unit,iostat=iostat) header
  if(iostat/=0) then
    call iotk_error_issue(ierrl,"iotk_stream_read",__FILE__,__LINE__)
# 91 "iotk_stream.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.4 ")
# 91 "iotk_stream.spp"
call iotk_error_msg(ierrl,'""')
# 91 "iotk_stream.spp"
call iotk_error_write(ierrl,"iostat",iostat)
    goto 1
  end if
  if(.not.lnoval) then
    read(unit,iostat=iostat) val
    if(iostat/=0) then
      call iotk_error_issue(ierrl,"iotk_stream_read",__FILE__,__LINE__)
# 97 "iotk_stream.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.4 ")
# 97 "iotk_stream.spp"
call iotk_error_msg(ierrl,'""')
# 97 "iotk_stream.spp"
call iotk_error_write(ierrl,"iostat",iostat)
      goto 1
    end if
  end if
  read(unit,pos=lpos+iotk_record_length+rec,iostat=iostat) rec1
  if(iostat/=0) then
    call iotk_error_issue(ierrl,"iotk_stream_read",__FILE__,__LINE__)
# 103 "iotk_stream.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.4 ")
# 103 "iotk_stream.spp"
call iotk_error_msg(ierrl,'""')
# 103 "iotk_stream.spp"
call iotk_error_write(ierrl,"iostat",iostat)
    goto 1
  end if
  if(rec1/=rec) then
    call iotk_error_issue(ierrl,"iotk_stream_read",__FILE__,__LINE__)
# 107 "iotk_stream.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.4 ")
# 107 "iotk_stream.spp"
call iotk_error_msg(ierrl,'""')
    goto 1
  end if
#else
  call iotk_error_issue(ierrl,"iotk_stream_read",__FILE__,__LINE__)
# 111 "iotk_stream.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.4 ")
# 111 "iotk_stream.spp"
call iotk_error_msg(ierrl,'Streams are not implemented')
#endif
1 continue
  if(present(ierr)) then
    ierr = ierrl
  else
    if(ierrl/=0) call iotk_error_handler(ierrl)
  end if
end subroutine iotk_stream_read_LOGICAL4
#endif
# 51 "iotk_stream.spp"

#ifdef __IOTK_INTEGER1
# 54 "iotk_stream.spp"
subroutine iotk_stream_read_INTEGER1(unit,header,val,setpos,getpos,noval,ierr)
  use iotk_base
  use iotk_error_interf
  implicit none
  integer,                                      intent(in)  :: unit
  integer(iotk_header_kind),                    intent(out) :: header
  INTEGER(kind=__IOTK_INTEGER1),      intent(out) :: val(:)
  integer,                            optional, intent(in)  :: setpos
  integer,                            optional, intent(out) :: getpos
  logical,                            optional, intent(in)  :: noval
  integer,                            optional, intent(out) :: ierr
#ifdef __IOTK_STREAMS
  integer(iotk_record_kind) :: rec,rec1
#endif
  logical :: lnoval
  integer :: lpos,ierrl,iostat
  lnoval = .false.
  if(present(noval)) lnoval = noval
  ierrl = 0
#ifdef __IOTK_STREAMS
  if(present(setpos)) then
    lpos=setpos
  else
    inquire(unit,pos=lpos,iostat=iostat)
    if(iostat/=0) then
      call iotk_error_issue(ierrl,"iotk_stream_read",__FILE__,__LINE__)
# 79 "iotk_stream.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.4 ")
# 79 "iotk_stream.spp"
call iotk_error_msg(ierrl,'""')
# 79 "iotk_stream.spp"
call iotk_error_write(ierrl,"iostat",iostat)
      goto 1
    end if
  end if
  if(present(getpos)) getpos = lpos
  read(unit,pos=lpos,iostat=iostat) rec
  if(iostat/=0) then
    call iotk_error_issue(ierrl,"iotk_stream_read",__FILE__,__LINE__)
# 86 "iotk_stream.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.4 ")
# 86 "iotk_stream.spp"
call iotk_error_msg(ierrl,'""')
# 86 "iotk_stream.spp"
call iotk_error_write(ierrl,"iostat",iostat)
    goto 1
  end if
  read(unit,iostat=iostat) header
  if(iostat/=0) then
    call iotk_error_issue(ierrl,"iotk_stream_read",__FILE__,__LINE__)
# 91 "iotk_stream.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.4 ")
# 91 "iotk_stream.spp"
call iotk_error_msg(ierrl,'""')
# 91 "iotk_stream.spp"
call iotk_error_write(ierrl,"iostat",iostat)
    goto 1
  end if
  if(.not.lnoval) then
    read(unit,iostat=iostat) val
    if(iostat/=0) then
      call iotk_error_issue(ierrl,"iotk_stream_read",__FILE__,__LINE__)
# 97 "iotk_stream.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.4 ")
# 97 "iotk_stream.spp"
call iotk_error_msg(ierrl,'""')
# 97 "iotk_stream.spp"
call iotk_error_write(ierrl,"iostat",iostat)
      goto 1
    end if
  end if
  read(unit,pos=lpos+iotk_record_length+rec,iostat=iostat) rec1
  if(iostat/=0) then
    call iotk_error_issue(ierrl,"iotk_stream_read",__FILE__,__LINE__)
# 103 "iotk_stream.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.4 ")
# 103 "iotk_stream.spp"
call iotk_error_msg(ierrl,'""')
# 103 "iotk_stream.spp"
call iotk_error_write(ierrl,"iostat",iostat)
    goto 1
  end if
  if(rec1/=rec) then
    call iotk_error_issue(ierrl,"iotk_stream_read",__FILE__,__LINE__)
# 107 "iotk_stream.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.4 ")
# 107 "iotk_stream.spp"
call iotk_error_msg(ierrl,'""')
    goto 1
  end if
#else
  call iotk_error_issue(ierrl,"iotk_stream_read",__FILE__,__LINE__)
# 111 "iotk_stream.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.4 ")
# 111 "iotk_stream.spp"
call iotk_error_msg(ierrl,'Streams are not implemented')
#endif
1 continue
  if(present(ierr)) then
    ierr = ierrl
  else
    if(ierrl/=0) call iotk_error_handler(ierrl)
  end if
end subroutine iotk_stream_read_INTEGER1
#endif
# 51 "iotk_stream.spp"

#ifdef __IOTK_INTEGER2
# 54 "iotk_stream.spp"
subroutine iotk_stream_read_INTEGER2(unit,header,val,setpos,getpos,noval,ierr)
  use iotk_base
  use iotk_error_interf
  implicit none
  integer,                                      intent(in)  :: unit
  integer(iotk_header_kind),                    intent(out) :: header
  INTEGER(kind=__IOTK_INTEGER2),      intent(out) :: val(:)
  integer,                            optional, intent(in)  :: setpos
  integer,                            optional, intent(out) :: getpos
  logical,                            optional, intent(in)  :: noval
  integer,                            optional, intent(out) :: ierr
#ifdef __IOTK_STREAMS
  integer(iotk_record_kind) :: rec,rec1
#endif
  logical :: lnoval
  integer :: lpos,ierrl,iostat
  lnoval = .false.
  if(present(noval)) lnoval = noval
  ierrl = 0
#ifdef __IOTK_STREAMS
  if(present(setpos)) then
    lpos=setpos
  else
    inquire(unit,pos=lpos,iostat=iostat)
    if(iostat/=0) then
      call iotk_error_issue(ierrl,"iotk_stream_read",__FILE__,__LINE__)
# 79 "iotk_stream.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.4 ")
# 79 "iotk_stream.spp"
call iotk_error_msg(ierrl,'""')
# 79 "iotk_stream.spp"
call iotk_error_write(ierrl,"iostat",iostat)
      goto 1
    end if
  end if
  if(present(getpos)) getpos = lpos
  read(unit,pos=lpos,iostat=iostat) rec
  if(iostat/=0) then
    call iotk_error_issue(ierrl,"iotk_stream_read",__FILE__,__LINE__)
# 86 "iotk_stream.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.4 ")
# 86 "iotk_stream.spp"
call iotk_error_msg(ierrl,'""')
# 86 "iotk_stream.spp"
call iotk_error_write(ierrl,"iostat",iostat)
    goto 1
  end if
  read(unit,iostat=iostat) header
  if(iostat/=0) then
    call iotk_error_issue(ierrl,"iotk_stream_read",__FILE__,__LINE__)
# 91 "iotk_stream.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.4 ")
# 91 "iotk_stream.spp"
call iotk_error_msg(ierrl,'""')
# 91 "iotk_stream.spp"
call iotk_error_write(ierrl,"iostat",iostat)
    goto 1
  end if
  if(.not.lnoval) then
    read(unit,iostat=iostat) val
    if(iostat/=0) then
      call iotk_error_issue(ierrl,"iotk_stream_read",__FILE__,__LINE__)
# 97 "iotk_stream.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.4 ")
# 97 "iotk_stream.spp"
call iotk_error_msg(ierrl,'""')
# 97 "iotk_stream.spp"
call iotk_error_write(ierrl,"iostat",iostat)
      goto 1
    end if
  end if
  read(unit,pos=lpos+iotk_record_length+rec,iostat=iostat) rec1
  if(iostat/=0) then
    call iotk_error_issue(ierrl,"iotk_stream_read",__FILE__,__LINE__)
# 103 "iotk_stream.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.4 ")
# 103 "iotk_stream.spp"
call iotk_error_msg(ierrl,'""')
# 103 "iotk_stream.spp"
call iotk_error_write(ierrl,"iostat",iostat)
    goto 1
  end if
  if(rec1/=rec) then
    call iotk_error_issue(ierrl,"iotk_stream_read",__FILE__,__LINE__)
# 107 "iotk_stream.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.4 ")
# 107 "iotk_stream.spp"
call iotk_error_msg(ierrl,'""')
    goto 1
  end if
#else
  call iotk_error_issue(ierrl,"iotk_stream_read",__FILE__,__LINE__)
# 111 "iotk_stream.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.4 ")
# 111 "iotk_stream.spp"
call iotk_error_msg(ierrl,'Streams are not implemented')
#endif
1 continue
  if(present(ierr)) then
    ierr = ierrl
  else
    if(ierrl/=0) call iotk_error_handler(ierrl)
  end if
end subroutine iotk_stream_read_INTEGER2
#endif
# 51 "iotk_stream.spp"

#ifdef __IOTK_INTEGER3
# 54 "iotk_stream.spp"
subroutine iotk_stream_read_INTEGER3(unit,header,val,setpos,getpos,noval,ierr)
  use iotk_base
  use iotk_error_interf
  implicit none
  integer,                                      intent(in)  :: unit
  integer(iotk_header_kind),                    intent(out) :: header
  INTEGER(kind=__IOTK_INTEGER3),      intent(out) :: val(:)
  integer,                            optional, intent(in)  :: setpos
  integer,                            optional, intent(out) :: getpos
  logical,                            optional, intent(in)  :: noval
  integer,                            optional, intent(out) :: ierr
#ifdef __IOTK_STREAMS
  integer(iotk_record_kind) :: rec,rec1
#endif
  logical :: lnoval
  integer :: lpos,ierrl,iostat
  lnoval = .false.
  if(present(noval)) lnoval = noval
  ierrl = 0
#ifdef __IOTK_STREAMS
  if(present(setpos)) then
    lpos=setpos
  else
    inquire(unit,pos=lpos,iostat=iostat)
    if(iostat/=0) then
      call iotk_error_issue(ierrl,"iotk_stream_read",__FILE__,__LINE__)
# 79 "iotk_stream.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.4 ")
# 79 "iotk_stream.spp"
call iotk_error_msg(ierrl,'""')
# 79 "iotk_stream.spp"
call iotk_error_write(ierrl,"iostat",iostat)
      goto 1
    end if
  end if
  if(present(getpos)) getpos = lpos
  read(unit,pos=lpos,iostat=iostat) rec
  if(iostat/=0) then
    call iotk_error_issue(ierrl,"iotk_stream_read",__FILE__,__LINE__)
# 86 "iotk_stream.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.4 ")
# 86 "iotk_stream.spp"
call iotk_error_msg(ierrl,'""')
# 86 "iotk_stream.spp"
call iotk_error_write(ierrl,"iostat",iostat)
    goto 1
  end if
  read(unit,iostat=iostat) header
  if(iostat/=0) then
    call iotk_error_issue(ierrl,"iotk_stream_read",__FILE__,__LINE__)
# 91 "iotk_stream.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.4 ")
# 91 "iotk_stream.spp"
call iotk_error_msg(ierrl,'""')
# 91 "iotk_stream.spp"
call iotk_error_write(ierrl,"iostat",iostat)
    goto 1
  end if
  if(.not.lnoval) then
    read(unit,iostat=iostat) val
    if(iostat/=0) then
      call iotk_error_issue(ierrl,"iotk_stream_read",__FILE__,__LINE__)
# 97 "iotk_stream.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.4 ")
# 97 "iotk_stream.spp"
call iotk_error_msg(ierrl,'""')
# 97 "iotk_stream.spp"
call iotk_error_write(ierrl,"iostat",iostat)
      goto 1
    end if
  end if
  read(unit,pos=lpos+iotk_record_length+rec,iostat=iostat) rec1
  if(iostat/=0) then
    call iotk_error_issue(ierrl,"iotk_stream_read",__FILE__,__LINE__)
# 103 "iotk_stream.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.4 ")
# 103 "iotk_stream.spp"
call iotk_error_msg(ierrl,'""')
# 103 "iotk_stream.spp"
call iotk_error_write(ierrl,"iostat",iostat)
    goto 1
  end if
  if(rec1/=rec) then
    call iotk_error_issue(ierrl,"iotk_stream_read",__FILE__,__LINE__)
# 107 "iotk_stream.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.4 ")
# 107 "iotk_stream.spp"
call iotk_error_msg(ierrl,'""')
    goto 1
  end if
#else
  call iotk_error_issue(ierrl,"iotk_stream_read",__FILE__,__LINE__)
# 111 "iotk_stream.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.4 ")
# 111 "iotk_stream.spp"
call iotk_error_msg(ierrl,'Streams are not implemented')
#endif
1 continue
  if(present(ierr)) then
    ierr = ierrl
  else
    if(ierrl/=0) call iotk_error_handler(ierrl)
  end if
end subroutine iotk_stream_read_INTEGER3
#endif
# 51 "iotk_stream.spp"

#ifdef __IOTK_INTEGER4
# 54 "iotk_stream.spp"
subroutine iotk_stream_read_INTEGER4(unit,header,val,setpos,getpos,noval,ierr)
  use iotk_base
  use iotk_error_interf
  implicit none
  integer,                                      intent(in)  :: unit
  integer(iotk_header_kind),                    intent(out) :: header
  INTEGER(kind=__IOTK_INTEGER4),      intent(out) :: val(:)
  integer,                            optional, intent(in)  :: setpos
  integer,                            optional, intent(out) :: getpos
  logical,                            optional, intent(in)  :: noval
  integer,                            optional, intent(out) :: ierr
#ifdef __IOTK_STREAMS
  integer(iotk_record_kind) :: rec,rec1
#endif
  logical :: lnoval
  integer :: lpos,ierrl,iostat
  lnoval = .false.
  if(present(noval)) lnoval = noval
  ierrl = 0
#ifdef __IOTK_STREAMS
  if(present(setpos)) then
    lpos=setpos
  else
    inquire(unit,pos=lpos,iostat=iostat)
    if(iostat/=0) then
      call iotk_error_issue(ierrl,"iotk_stream_read",__FILE__,__LINE__)
# 79 "iotk_stream.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.4 ")
# 79 "iotk_stream.spp"
call iotk_error_msg(ierrl,'""')
# 79 "iotk_stream.spp"
call iotk_error_write(ierrl,"iostat",iostat)
      goto 1
    end if
  end if
  if(present(getpos)) getpos = lpos
  read(unit,pos=lpos,iostat=iostat) rec
  if(iostat/=0) then
    call iotk_error_issue(ierrl,"iotk_stream_read",__FILE__,__LINE__)
# 86 "iotk_stream.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.4 ")
# 86 "iotk_stream.spp"
call iotk_error_msg(ierrl,'""')
# 86 "iotk_stream.spp"
call iotk_error_write(ierrl,"iostat",iostat)
    goto 1
  end if
  read(unit,iostat=iostat) header
  if(iostat/=0) then
    call iotk_error_issue(ierrl,"iotk_stream_read",__FILE__,__LINE__)
# 91 "iotk_stream.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.4 ")
# 91 "iotk_stream.spp"
call iotk_error_msg(ierrl,'""')
# 91 "iotk_stream.spp"
call iotk_error_write(ierrl,"iostat",iostat)
    goto 1
  end if
  if(.not.lnoval) then
    read(unit,iostat=iostat) val
    if(iostat/=0) then
      call iotk_error_issue(ierrl,"iotk_stream_read",__FILE__,__LINE__)
# 97 "iotk_stream.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.4 ")
# 97 "iotk_stream.spp"
call iotk_error_msg(ierrl,'""')
# 97 "iotk_stream.spp"
call iotk_error_write(ierrl,"iostat",iostat)
      goto 1
    end if
  end if
  read(unit,pos=lpos+iotk_record_length+rec,iostat=iostat) rec1
  if(iostat/=0) then
    call iotk_error_issue(ierrl,"iotk_stream_read",__FILE__,__LINE__)
# 103 "iotk_stream.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.4 ")
# 103 "iotk_stream.spp"
call iotk_error_msg(ierrl,'""')
# 103 "iotk_stream.spp"
call iotk_error_write(ierrl,"iostat",iostat)
    goto 1
  end if
  if(rec1/=rec) then
    call iotk_error_issue(ierrl,"iotk_stream_read",__FILE__,__LINE__)
# 107 "iotk_stream.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.4 ")
# 107 "iotk_stream.spp"
call iotk_error_msg(ierrl,'""')
    goto 1
  end if
#else
  call iotk_error_issue(ierrl,"iotk_stream_read",__FILE__,__LINE__)
# 111 "iotk_stream.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.4 ")
# 111 "iotk_stream.spp"
call iotk_error_msg(ierrl,'Streams are not implemented')
#endif
1 continue
  if(present(ierr)) then
    ierr = ierrl
  else
    if(ierrl/=0) call iotk_error_handler(ierrl)
  end if
end subroutine iotk_stream_read_INTEGER4
#endif
# 51 "iotk_stream.spp"

#ifdef __IOTK_REAL1
# 54 "iotk_stream.spp"
subroutine iotk_stream_read_REAL1(unit,header,val,setpos,getpos,noval,ierr)
  use iotk_base
  use iotk_error_interf
  implicit none
  integer,                                      intent(in)  :: unit
  integer(iotk_header_kind),                    intent(out) :: header
  REAL(kind=__IOTK_REAL1),      intent(out) :: val(:)
  integer,                            optional, intent(in)  :: setpos
  integer,                            optional, intent(out) :: getpos
  logical,                            optional, intent(in)  :: noval
  integer,                            optional, intent(out) :: ierr
#ifdef __IOTK_STREAMS
  integer(iotk_record_kind) :: rec,rec1
#endif
  logical :: lnoval
  integer :: lpos,ierrl,iostat
  lnoval = .false.
  if(present(noval)) lnoval = noval
  ierrl = 0
#ifdef __IOTK_STREAMS
  if(present(setpos)) then
    lpos=setpos
  else
    inquire(unit,pos=lpos,iostat=iostat)
    if(iostat/=0) then
      call iotk_error_issue(ierrl,"iotk_stream_read",__FILE__,__LINE__)
# 79 "iotk_stream.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.4 ")
# 79 "iotk_stream.spp"
call iotk_error_msg(ierrl,'""')
# 79 "iotk_stream.spp"
call iotk_error_write(ierrl,"iostat",iostat)
      goto 1
    end if
  end if
  if(present(getpos)) getpos = lpos
  read(unit,pos=lpos,iostat=iostat) rec
  if(iostat/=0) then
    call iotk_error_issue(ierrl,"iotk_stream_read",__FILE__,__LINE__)
# 86 "iotk_stream.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.4 ")
# 86 "iotk_stream.spp"
call iotk_error_msg(ierrl,'""')
# 86 "iotk_stream.spp"
call iotk_error_write(ierrl,"iostat",iostat)
    goto 1
  end if
  read(unit,iostat=iostat) header
  if(iostat/=0) then
    call iotk_error_issue(ierrl,"iotk_stream_read",__FILE__,__LINE__)
# 91 "iotk_stream.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.4 ")
# 91 "iotk_stream.spp"
call iotk_error_msg(ierrl,'""')
# 91 "iotk_stream.spp"
call iotk_error_write(ierrl,"iostat",iostat)
    goto 1
  end if
  if(.not.lnoval) then
    read(unit,iostat=iostat) val
    if(iostat/=0) then
      call iotk_error_issue(ierrl,"iotk_stream_read",__FILE__,__LINE__)
# 97 "iotk_stream.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.4 ")
# 97 "iotk_stream.spp"
call iotk_error_msg(ierrl,'""')
# 97 "iotk_stream.spp"
call iotk_error_write(ierrl,"iostat",iostat)
      goto 1
    end if
  end if
  read(unit,pos=lpos+iotk_record_length+rec,iostat=iostat) rec1
  if(iostat/=0) then
    call iotk_error_issue(ierrl,"iotk_stream_read",__FILE__,__LINE__)
# 103 "iotk_stream.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.4 ")
# 103 "iotk_stream.spp"
call iotk_error_msg(ierrl,'""')
# 103 "iotk_stream.spp"
call iotk_error_write(ierrl,"iostat",iostat)
    goto 1
  end if
  if(rec1/=rec) then
    call iotk_error_issue(ierrl,"iotk_stream_read",__FILE__,__LINE__)
# 107 "iotk_stream.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.4 ")
# 107 "iotk_stream.spp"
call iotk_error_msg(ierrl,'""')
    goto 1
  end if
#else
  call iotk_error_issue(ierrl,"iotk_stream_read",__FILE__,__LINE__)
# 111 "iotk_stream.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.4 ")
# 111 "iotk_stream.spp"
call iotk_error_msg(ierrl,'Streams are not implemented')
#endif
1 continue
  if(present(ierr)) then
    ierr = ierrl
  else
    if(ierrl/=0) call iotk_error_handler(ierrl)
  end if
end subroutine iotk_stream_read_REAL1
#endif
# 51 "iotk_stream.spp"

#ifdef __IOTK_REAL2
# 54 "iotk_stream.spp"
subroutine iotk_stream_read_REAL2(unit,header,val,setpos,getpos,noval,ierr)
  use iotk_base
  use iotk_error_interf
  implicit none
  integer,                                      intent(in)  :: unit
  integer(iotk_header_kind),                    intent(out) :: header
  REAL(kind=__IOTK_REAL2),      intent(out) :: val(:)
  integer,                            optional, intent(in)  :: setpos
  integer,                            optional, intent(out) :: getpos
  logical,                            optional, intent(in)  :: noval
  integer,                            optional, intent(out) :: ierr
#ifdef __IOTK_STREAMS
  integer(iotk_record_kind) :: rec,rec1
#endif
  logical :: lnoval
  integer :: lpos,ierrl,iostat
  lnoval = .false.
  if(present(noval)) lnoval = noval
  ierrl = 0
#ifdef __IOTK_STREAMS
  if(present(setpos)) then
    lpos=setpos
  else
    inquire(unit,pos=lpos,iostat=iostat)
    if(iostat/=0) then
      call iotk_error_issue(ierrl,"iotk_stream_read",__FILE__,__LINE__)
# 79 "iotk_stream.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.4 ")
# 79 "iotk_stream.spp"
call iotk_error_msg(ierrl,'""')
# 79 "iotk_stream.spp"
call iotk_error_write(ierrl,"iostat",iostat)
      goto 1
    end if
  end if
  if(present(getpos)) getpos = lpos
  read(unit,pos=lpos,iostat=iostat) rec
  if(iostat/=0) then
    call iotk_error_issue(ierrl,"iotk_stream_read",__FILE__,__LINE__)
# 86 "iotk_stream.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.4 ")
# 86 "iotk_stream.spp"
call iotk_error_msg(ierrl,'""')
# 86 "iotk_stream.spp"
call iotk_error_write(ierrl,"iostat",iostat)
    goto 1
  end if
  read(unit,iostat=iostat) header
  if(iostat/=0) then
    call iotk_error_issue(ierrl,"iotk_stream_read",__FILE__,__LINE__)
# 91 "iotk_stream.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.4 ")
# 91 "iotk_stream.spp"
call iotk_error_msg(ierrl,'""')
# 91 "iotk_stream.spp"
call iotk_error_write(ierrl,"iostat",iostat)
    goto 1
  end if
  if(.not.lnoval) then
    read(unit,iostat=iostat) val
    if(iostat/=0) then
      call iotk_error_issue(ierrl,"iotk_stream_read",__FILE__,__LINE__)
# 97 "iotk_stream.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.4 ")
# 97 "iotk_stream.spp"
call iotk_error_msg(ierrl,'""')
# 97 "iotk_stream.spp"
call iotk_error_write(ierrl,"iostat",iostat)
      goto 1
    end if
  end if
  read(unit,pos=lpos+iotk_record_length+rec,iostat=iostat) rec1
  if(iostat/=0) then
    call iotk_error_issue(ierrl,"iotk_stream_read",__FILE__,__LINE__)
# 103 "iotk_stream.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.4 ")
# 103 "iotk_stream.spp"
call iotk_error_msg(ierrl,'""')
# 103 "iotk_stream.spp"
call iotk_error_write(ierrl,"iostat",iostat)
    goto 1
  end if
  if(rec1/=rec) then
    call iotk_error_issue(ierrl,"iotk_stream_read",__FILE__,__LINE__)
# 107 "iotk_stream.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.4 ")
# 107 "iotk_stream.spp"
call iotk_error_msg(ierrl,'""')
    goto 1
  end if
#else
  call iotk_error_issue(ierrl,"iotk_stream_read",__FILE__,__LINE__)
# 111 "iotk_stream.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.4 ")
# 111 "iotk_stream.spp"
call iotk_error_msg(ierrl,'Streams are not implemented')
#endif
1 continue
  if(present(ierr)) then
    ierr = ierrl
  else
    if(ierrl/=0) call iotk_error_handler(ierrl)
  end if
end subroutine iotk_stream_read_REAL2
#endif
# 51 "iotk_stream.spp"

#ifdef __IOTK_REAL3
# 54 "iotk_stream.spp"
subroutine iotk_stream_read_REAL3(unit,header,val,setpos,getpos,noval,ierr)
  use iotk_base
  use iotk_error_interf
  implicit none
  integer,                                      intent(in)  :: unit
  integer(iotk_header_kind),                    intent(out) :: header
  REAL(kind=__IOTK_REAL3),      intent(out) :: val(:)
  integer,                            optional, intent(in)  :: setpos
  integer,                            optional, intent(out) :: getpos
  logical,                            optional, intent(in)  :: noval
  integer,                            optional, intent(out) :: ierr
#ifdef __IOTK_STREAMS
  integer(iotk_record_kind) :: rec,rec1
#endif
  logical :: lnoval
  integer :: lpos,ierrl,iostat
  lnoval = .false.
  if(present(noval)) lnoval = noval
  ierrl = 0
#ifdef __IOTK_STREAMS
  if(present(setpos)) then
    lpos=setpos
  else
    inquire(unit,pos=lpos,iostat=iostat)
    if(iostat/=0) then
      call iotk_error_issue(ierrl,"iotk_stream_read",__FILE__,__LINE__)
# 79 "iotk_stream.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.4 ")
# 79 "iotk_stream.spp"
call iotk_error_msg(ierrl,'""')
# 79 "iotk_stream.spp"
call iotk_error_write(ierrl,"iostat",iostat)
      goto 1
    end if
  end if
  if(present(getpos)) getpos = lpos
  read(unit,pos=lpos,iostat=iostat) rec
  if(iostat/=0) then
    call iotk_error_issue(ierrl,"iotk_stream_read",__FILE__,__LINE__)
# 86 "iotk_stream.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.4 ")
# 86 "iotk_stream.spp"
call iotk_error_msg(ierrl,'""')
# 86 "iotk_stream.spp"
call iotk_error_write(ierrl,"iostat",iostat)
    goto 1
  end if
  read(unit,iostat=iostat) header
  if(iostat/=0) then
    call iotk_error_issue(ierrl,"iotk_stream_read",__FILE__,__LINE__)
# 91 "iotk_stream.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.4 ")
# 91 "iotk_stream.spp"
call iotk_error_msg(ierrl,'""')
# 91 "iotk_stream.spp"
call iotk_error_write(ierrl,"iostat",iostat)
    goto 1
  end if
  if(.not.lnoval) then
    read(unit,iostat=iostat) val
    if(iostat/=0) then
      call iotk_error_issue(ierrl,"iotk_stream_read",__FILE__,__LINE__)
# 97 "iotk_stream.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.4 ")
# 97 "iotk_stream.spp"
call iotk_error_msg(ierrl,'""')
# 97 "iotk_stream.spp"
call iotk_error_write(ierrl,"iostat",iostat)
      goto 1
    end if
  end if
  read(unit,pos=lpos+iotk_record_length+rec,iostat=iostat) rec1
  if(iostat/=0) then
    call iotk_error_issue(ierrl,"iotk_stream_read",__FILE__,__LINE__)
# 103 "iotk_stream.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.4 ")
# 103 "iotk_stream.spp"
call iotk_error_msg(ierrl,'""')
# 103 "iotk_stream.spp"
call iotk_error_write(ierrl,"iostat",iostat)
    goto 1
  end if
  if(rec1/=rec) then
    call iotk_error_issue(ierrl,"iotk_stream_read",__FILE__,__LINE__)
# 107 "iotk_stream.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.4 ")
# 107 "iotk_stream.spp"
call iotk_error_msg(ierrl,'""')
    goto 1
  end if
#else
  call iotk_error_issue(ierrl,"iotk_stream_read",__FILE__,__LINE__)
# 111 "iotk_stream.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.4 ")
# 111 "iotk_stream.spp"
call iotk_error_msg(ierrl,'Streams are not implemented')
#endif
1 continue
  if(present(ierr)) then
    ierr = ierrl
  else
    if(ierrl/=0) call iotk_error_handler(ierrl)
  end if
end subroutine iotk_stream_read_REAL3
#endif
# 51 "iotk_stream.spp"

#ifdef __IOTK_REAL4
# 54 "iotk_stream.spp"
subroutine iotk_stream_read_REAL4(unit,header,val,setpos,getpos,noval,ierr)
  use iotk_base
  use iotk_error_interf
  implicit none
  integer,                                      intent(in)  :: unit
  integer(iotk_header_kind),                    intent(out) :: header
  REAL(kind=__IOTK_REAL4),      intent(out) :: val(:)
  integer,                            optional, intent(in)  :: setpos
  integer,                            optional, intent(out) :: getpos
  logical,                            optional, intent(in)  :: noval
  integer,                            optional, intent(out) :: ierr
#ifdef __IOTK_STREAMS
  integer(iotk_record_kind) :: rec,rec1
#endif
  logical :: lnoval
  integer :: lpos,ierrl,iostat
  lnoval = .false.
  if(present(noval)) lnoval = noval
  ierrl = 0
#ifdef __IOTK_STREAMS
  if(present(setpos)) then
    lpos=setpos
  else
    inquire(unit,pos=lpos,iostat=iostat)
    if(iostat/=0) then
      call iotk_error_issue(ierrl,"iotk_stream_read",__FILE__,__LINE__)
# 79 "iotk_stream.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.4 ")
# 79 "iotk_stream.spp"
call iotk_error_msg(ierrl,'""')
# 79 "iotk_stream.spp"
call iotk_error_write(ierrl,"iostat",iostat)
      goto 1
    end if
  end if
  if(present(getpos)) getpos = lpos
  read(unit,pos=lpos,iostat=iostat) rec
  if(iostat/=0) then
    call iotk_error_issue(ierrl,"iotk_stream_read",__FILE__,__LINE__)
# 86 "iotk_stream.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.4 ")
# 86 "iotk_stream.spp"
call iotk_error_msg(ierrl,'""')
# 86 "iotk_stream.spp"
call iotk_error_write(ierrl,"iostat",iostat)
    goto 1
  end if
  read(unit,iostat=iostat) header
  if(iostat/=0) then
    call iotk_error_issue(ierrl,"iotk_stream_read",__FILE__,__LINE__)
# 91 "iotk_stream.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.4 ")
# 91 "iotk_stream.spp"
call iotk_error_msg(ierrl,'""')
# 91 "iotk_stream.spp"
call iotk_error_write(ierrl,"iostat",iostat)
    goto 1
  end if
  if(.not.lnoval) then
    read(unit,iostat=iostat) val
    if(iostat/=0) then
      call iotk_error_issue(ierrl,"iotk_stream_read",__FILE__,__LINE__)
# 97 "iotk_stream.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.4 ")
# 97 "iotk_stream.spp"
call iotk_error_msg(ierrl,'""')
# 97 "iotk_stream.spp"
call iotk_error_write(ierrl,"iostat",iostat)
      goto 1
    end if
  end if
  read(unit,pos=lpos+iotk_record_length+rec,iostat=iostat) rec1
  if(iostat/=0) then
    call iotk_error_issue(ierrl,"iotk_stream_read",__FILE__,__LINE__)
# 103 "iotk_stream.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.4 ")
# 103 "iotk_stream.spp"
call iotk_error_msg(ierrl,'""')
# 103 "iotk_stream.spp"
call iotk_error_write(ierrl,"iostat",iostat)
    goto 1
  end if
  if(rec1/=rec) then
    call iotk_error_issue(ierrl,"iotk_stream_read",__FILE__,__LINE__)
# 107 "iotk_stream.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.4 ")
# 107 "iotk_stream.spp"
call iotk_error_msg(ierrl,'""')
    goto 1
  end if
#else
  call iotk_error_issue(ierrl,"iotk_stream_read",__FILE__,__LINE__)
# 111 "iotk_stream.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.4 ")
# 111 "iotk_stream.spp"
call iotk_error_msg(ierrl,'Streams are not implemented')
#endif
1 continue
  if(present(ierr)) then
    ierr = ierrl
  else
    if(ierrl/=0) call iotk_error_handler(ierrl)
  end if
end subroutine iotk_stream_read_REAL4
#endif
# 51 "iotk_stream.spp"

#ifdef __IOTK_COMPLEX1
# 54 "iotk_stream.spp"
subroutine iotk_stream_read_COMPLEX1(unit,header,val,setpos,getpos,noval,ierr)
  use iotk_base
  use iotk_error_interf
  implicit none
  integer,                                      intent(in)  :: unit
  integer(iotk_header_kind),                    intent(out) :: header
  COMPLEX(kind=__IOTK_COMPLEX1),      intent(out) :: val(:)
  integer,                            optional, intent(in)  :: setpos
  integer,                            optional, intent(out) :: getpos
  logical,                            optional, intent(in)  :: noval
  integer,                            optional, intent(out) :: ierr
#ifdef __IOTK_STREAMS
  integer(iotk_record_kind) :: rec,rec1
#endif
  logical :: lnoval
  integer :: lpos,ierrl,iostat
  lnoval = .false.
  if(present(noval)) lnoval = noval
  ierrl = 0
#ifdef __IOTK_STREAMS
  if(present(setpos)) then
    lpos=setpos
  else
    inquire(unit,pos=lpos,iostat=iostat)
    if(iostat/=0) then
      call iotk_error_issue(ierrl,"iotk_stream_read",__FILE__,__LINE__)
# 79 "iotk_stream.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.4 ")
# 79 "iotk_stream.spp"
call iotk_error_msg(ierrl,'""')
# 79 "iotk_stream.spp"
call iotk_error_write(ierrl,"iostat",iostat)
      goto 1
    end if
  end if
  if(present(getpos)) getpos = lpos
  read(unit,pos=lpos,iostat=iostat) rec
  if(iostat/=0) then
    call iotk_error_issue(ierrl,"iotk_stream_read",__FILE__,__LINE__)
# 86 "iotk_stream.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.4 ")
# 86 "iotk_stream.spp"
call iotk_error_msg(ierrl,'""')
# 86 "iotk_stream.spp"
call iotk_error_write(ierrl,"iostat",iostat)
    goto 1
  end if
  read(unit,iostat=iostat) header
  if(iostat/=0) then
    call iotk_error_issue(ierrl,"iotk_stream_read",__FILE__,__LINE__)
# 91 "iotk_stream.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.4 ")
# 91 "iotk_stream.spp"
call iotk_error_msg(ierrl,'""')
# 91 "iotk_stream.spp"
call iotk_error_write(ierrl,"iostat",iostat)
    goto 1
  end if
  if(.not.lnoval) then
    read(unit,iostat=iostat) val
    if(iostat/=0) then
      call iotk_error_issue(ierrl,"iotk_stream_read",__FILE__,__LINE__)
# 97 "iotk_stream.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.4 ")
# 97 "iotk_stream.spp"
call iotk_error_msg(ierrl,'""')
# 97 "iotk_stream.spp"
call iotk_error_write(ierrl,"iostat",iostat)
      goto 1
    end if
  end if
  read(unit,pos=lpos+iotk_record_length+rec,iostat=iostat) rec1
  if(iostat/=0) then
    call iotk_error_issue(ierrl,"iotk_stream_read",__FILE__,__LINE__)
# 103 "iotk_stream.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.4 ")
# 103 "iotk_stream.spp"
call iotk_error_msg(ierrl,'""')
# 103 "iotk_stream.spp"
call iotk_error_write(ierrl,"iostat",iostat)
    goto 1
  end if
  if(rec1/=rec) then
    call iotk_error_issue(ierrl,"iotk_stream_read",__FILE__,__LINE__)
# 107 "iotk_stream.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.4 ")
# 107 "iotk_stream.spp"
call iotk_error_msg(ierrl,'""')
    goto 1
  end if
#else
  call iotk_error_issue(ierrl,"iotk_stream_read",__FILE__,__LINE__)
# 111 "iotk_stream.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.4 ")
# 111 "iotk_stream.spp"
call iotk_error_msg(ierrl,'Streams are not implemented')
#endif
1 continue
  if(present(ierr)) then
    ierr = ierrl
  else
    if(ierrl/=0) call iotk_error_handler(ierrl)
  end if
end subroutine iotk_stream_read_COMPLEX1
#endif
# 51 "iotk_stream.spp"

#ifdef __IOTK_COMPLEX2
# 54 "iotk_stream.spp"
subroutine iotk_stream_read_COMPLEX2(unit,header,val,setpos,getpos,noval,ierr)
  use iotk_base
  use iotk_error_interf
  implicit none
  integer,                                      intent(in)  :: unit
  integer(iotk_header_kind),                    intent(out) :: header
  COMPLEX(kind=__IOTK_COMPLEX2),      intent(out) :: val(:)
  integer,                            optional, intent(in)  :: setpos
  integer,                            optional, intent(out) :: getpos
  logical,                            optional, intent(in)  :: noval
  integer,                            optional, intent(out) :: ierr
#ifdef __IOTK_STREAMS
  integer(iotk_record_kind) :: rec,rec1
#endif
  logical :: lnoval
  integer :: lpos,ierrl,iostat
  lnoval = .false.
  if(present(noval)) lnoval = noval
  ierrl = 0
#ifdef __IOTK_STREAMS
  if(present(setpos)) then
    lpos=setpos
  else
    inquire(unit,pos=lpos,iostat=iostat)
    if(iostat/=0) then
      call iotk_error_issue(ierrl,"iotk_stream_read",__FILE__,__LINE__)
# 79 "iotk_stream.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.4 ")
# 79 "iotk_stream.spp"
call iotk_error_msg(ierrl,'""')
# 79 "iotk_stream.spp"
call iotk_error_write(ierrl,"iostat",iostat)
      goto 1
    end if
  end if
  if(present(getpos)) getpos = lpos
  read(unit,pos=lpos,iostat=iostat) rec
  if(iostat/=0) then
    call iotk_error_issue(ierrl,"iotk_stream_read",__FILE__,__LINE__)
# 86 "iotk_stream.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.4 ")
# 86 "iotk_stream.spp"
call iotk_error_msg(ierrl,'""')
# 86 "iotk_stream.spp"
call iotk_error_write(ierrl,"iostat",iostat)
    goto 1
  end if
  read(unit,iostat=iostat) header
  if(iostat/=0) then
    call iotk_error_issue(ierrl,"iotk_stream_read",__FILE__,__LINE__)
# 91 "iotk_stream.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.4 ")
# 91 "iotk_stream.spp"
call iotk_error_msg(ierrl,'""')
# 91 "iotk_stream.spp"
call iotk_error_write(ierrl,"iostat",iostat)
    goto 1
  end if
  if(.not.lnoval) then
    read(unit,iostat=iostat) val
    if(iostat/=0) then
      call iotk_error_issue(ierrl,"iotk_stream_read",__FILE__,__LINE__)
# 97 "iotk_stream.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.4 ")
# 97 "iotk_stream.spp"
call iotk_error_msg(ierrl,'""')
# 97 "iotk_stream.spp"
call iotk_error_write(ierrl,"iostat",iostat)
      goto 1
    end if
  end if
  read(unit,pos=lpos+iotk_record_length+rec,iostat=iostat) rec1
  if(iostat/=0) then
    call iotk_error_issue(ierrl,"iotk_stream_read",__FILE__,__LINE__)
# 103 "iotk_stream.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.4 ")
# 103 "iotk_stream.spp"
call iotk_error_msg(ierrl,'""')
# 103 "iotk_stream.spp"
call iotk_error_write(ierrl,"iostat",iostat)
    goto 1
  end if
  if(rec1/=rec) then
    call iotk_error_issue(ierrl,"iotk_stream_read",__FILE__,__LINE__)
# 107 "iotk_stream.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.4 ")
# 107 "iotk_stream.spp"
call iotk_error_msg(ierrl,'""')
    goto 1
  end if
#else
  call iotk_error_issue(ierrl,"iotk_stream_read",__FILE__,__LINE__)
# 111 "iotk_stream.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.4 ")
# 111 "iotk_stream.spp"
call iotk_error_msg(ierrl,'Streams are not implemented')
#endif
1 continue
  if(present(ierr)) then
    ierr = ierrl
  else
    if(ierrl/=0) call iotk_error_handler(ierrl)
  end if
end subroutine iotk_stream_read_COMPLEX2
#endif
# 51 "iotk_stream.spp"

#ifdef __IOTK_COMPLEX3
# 54 "iotk_stream.spp"
subroutine iotk_stream_read_COMPLEX3(unit,header,val,setpos,getpos,noval,ierr)
  use iotk_base
  use iotk_error_interf
  implicit none
  integer,                                      intent(in)  :: unit
  integer(iotk_header_kind),                    intent(out) :: header
  COMPLEX(kind=__IOTK_COMPLEX3),      intent(out) :: val(:)
  integer,                            optional, intent(in)  :: setpos
  integer,                            optional, intent(out) :: getpos
  logical,                            optional, intent(in)  :: noval
  integer,                            optional, intent(out) :: ierr
#ifdef __IOTK_STREAMS
  integer(iotk_record_kind) :: rec,rec1
#endif
  logical :: lnoval
  integer :: lpos,ierrl,iostat
  lnoval = .false.
  if(present(noval)) lnoval = noval
  ierrl = 0
#ifdef __IOTK_STREAMS
  if(present(setpos)) then
    lpos=setpos
  else
    inquire(unit,pos=lpos,iostat=iostat)
    if(iostat/=0) then
      call iotk_error_issue(ierrl,"iotk_stream_read",__FILE__,__LINE__)
# 79 "iotk_stream.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.4 ")
# 79 "iotk_stream.spp"
call iotk_error_msg(ierrl,'""')
# 79 "iotk_stream.spp"
call iotk_error_write(ierrl,"iostat",iostat)
      goto 1
    end if
  end if
  if(present(getpos)) getpos = lpos
  read(unit,pos=lpos,iostat=iostat) rec
  if(iostat/=0) then
    call iotk_error_issue(ierrl,"iotk_stream_read",__FILE__,__LINE__)
# 86 "iotk_stream.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.4 ")
# 86 "iotk_stream.spp"
call iotk_error_msg(ierrl,'""')
# 86 "iotk_stream.spp"
call iotk_error_write(ierrl,"iostat",iostat)
    goto 1
  end if
  read(unit,iostat=iostat) header
  if(iostat/=0) then
    call iotk_error_issue(ierrl,"iotk_stream_read",__FILE__,__LINE__)
# 91 "iotk_stream.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.4 ")
# 91 "iotk_stream.spp"
call iotk_error_msg(ierrl,'""')
# 91 "iotk_stream.spp"
call iotk_error_write(ierrl,"iostat",iostat)
    goto 1
  end if
  if(.not.lnoval) then
    read(unit,iostat=iostat) val
    if(iostat/=0) then
      call iotk_error_issue(ierrl,"iotk_stream_read",__FILE__,__LINE__)
# 97 "iotk_stream.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.4 ")
# 97 "iotk_stream.spp"
call iotk_error_msg(ierrl,'""')
# 97 "iotk_stream.spp"
call iotk_error_write(ierrl,"iostat",iostat)
      goto 1
    end if
  end if
  read(unit,pos=lpos+iotk_record_length+rec,iostat=iostat) rec1
  if(iostat/=0) then
    call iotk_error_issue(ierrl,"iotk_stream_read",__FILE__,__LINE__)
# 103 "iotk_stream.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.4 ")
# 103 "iotk_stream.spp"
call iotk_error_msg(ierrl,'""')
# 103 "iotk_stream.spp"
call iotk_error_write(ierrl,"iostat",iostat)
    goto 1
  end if
  if(rec1/=rec) then
    call iotk_error_issue(ierrl,"iotk_stream_read",__FILE__,__LINE__)
# 107 "iotk_stream.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.4 ")
# 107 "iotk_stream.spp"
call iotk_error_msg(ierrl,'""')
    goto 1
  end if
#else
  call iotk_error_issue(ierrl,"iotk_stream_read",__FILE__,__LINE__)
# 111 "iotk_stream.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.4 ")
# 111 "iotk_stream.spp"
call iotk_error_msg(ierrl,'Streams are not implemented')
#endif
1 continue
  if(present(ierr)) then
    ierr = ierrl
  else
    if(ierrl/=0) call iotk_error_handler(ierrl)
  end if
end subroutine iotk_stream_read_COMPLEX3
#endif
# 51 "iotk_stream.spp"

#ifdef __IOTK_COMPLEX4
# 54 "iotk_stream.spp"
subroutine iotk_stream_read_COMPLEX4(unit,header,val,setpos,getpos,noval,ierr)
  use iotk_base
  use iotk_error_interf
  implicit none
  integer,                                      intent(in)  :: unit
  integer(iotk_header_kind),                    intent(out) :: header
  COMPLEX(kind=__IOTK_COMPLEX4),      intent(out) :: val(:)
  integer,                            optional, intent(in)  :: setpos
  integer,                            optional, intent(out) :: getpos
  logical,                            optional, intent(in)  :: noval
  integer,                            optional, intent(out) :: ierr
#ifdef __IOTK_STREAMS
  integer(iotk_record_kind) :: rec,rec1
#endif
  logical :: lnoval
  integer :: lpos,ierrl,iostat
  lnoval = .false.
  if(present(noval)) lnoval = noval
  ierrl = 0
#ifdef __IOTK_STREAMS
  if(present(setpos)) then
    lpos=setpos
  else
    inquire(unit,pos=lpos,iostat=iostat)
    if(iostat/=0) then
      call iotk_error_issue(ierrl,"iotk_stream_read",__FILE__,__LINE__)
# 79 "iotk_stream.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.4 ")
# 79 "iotk_stream.spp"
call iotk_error_msg(ierrl,'""')
# 79 "iotk_stream.spp"
call iotk_error_write(ierrl,"iostat",iostat)
      goto 1
    end if
  end if
  if(present(getpos)) getpos = lpos
  read(unit,pos=lpos,iostat=iostat) rec
  if(iostat/=0) then
    call iotk_error_issue(ierrl,"iotk_stream_read",__FILE__,__LINE__)
# 86 "iotk_stream.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.4 ")
# 86 "iotk_stream.spp"
call iotk_error_msg(ierrl,'""')
# 86 "iotk_stream.spp"
call iotk_error_write(ierrl,"iostat",iostat)
    goto 1
  end if
  read(unit,iostat=iostat) header
  if(iostat/=0) then
    call iotk_error_issue(ierrl,"iotk_stream_read",__FILE__,__LINE__)
# 91 "iotk_stream.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.4 ")
# 91 "iotk_stream.spp"
call iotk_error_msg(ierrl,'""')
# 91 "iotk_stream.spp"
call iotk_error_write(ierrl,"iostat",iostat)
    goto 1
  end if
  if(.not.lnoval) then
    read(unit,iostat=iostat) val
    if(iostat/=0) then
      call iotk_error_issue(ierrl,"iotk_stream_read",__FILE__,__LINE__)
# 97 "iotk_stream.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.4 ")
# 97 "iotk_stream.spp"
call iotk_error_msg(ierrl,'""')
# 97 "iotk_stream.spp"
call iotk_error_write(ierrl,"iostat",iostat)
      goto 1
    end if
  end if
  read(unit,pos=lpos+iotk_record_length+rec,iostat=iostat) rec1
  if(iostat/=0) then
    call iotk_error_issue(ierrl,"iotk_stream_read",__FILE__,__LINE__)
# 103 "iotk_stream.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.4 ")
# 103 "iotk_stream.spp"
call iotk_error_msg(ierrl,'""')
# 103 "iotk_stream.spp"
call iotk_error_write(ierrl,"iostat",iostat)
    goto 1
  end if
  if(rec1/=rec) then
    call iotk_error_issue(ierrl,"iotk_stream_read",__FILE__,__LINE__)
# 107 "iotk_stream.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.4 ")
# 107 "iotk_stream.spp"
call iotk_error_msg(ierrl,'""')
    goto 1
  end if
#else
  call iotk_error_issue(ierrl,"iotk_stream_read",__FILE__,__LINE__)
# 111 "iotk_stream.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.4 ")
# 111 "iotk_stream.spp"
call iotk_error_msg(ierrl,'Streams are not implemented')
#endif
1 continue
  if(present(ierr)) then
    ierr = ierrl
  else
    if(ierrl/=0) call iotk_error_handler(ierrl)
  end if
end subroutine iotk_stream_read_COMPLEX4
#endif
# 51 "iotk_stream.spp"

#ifdef __IOTK_CHARACTER1
# 54 "iotk_stream.spp"
subroutine iotk_stream_read_CHARACTER1(unit,header,val,setpos,getpos,noval,ierr)
  use iotk_base
  use iotk_error_interf
  implicit none
  integer,                                      intent(in)  :: unit
  integer(iotk_header_kind),                    intent(out) :: header
  CHARACTER(kind=__IOTK_CHARACTER1,len=*),      intent(out) :: val(:)
  integer,                            optional, intent(in)  :: setpos
  integer,                            optional, intent(out) :: getpos
  logical,                            optional, intent(in)  :: noval
  integer,                            optional, intent(out) :: ierr
#ifdef __IOTK_STREAMS
  integer(iotk_record_kind) :: rec,rec1
#endif
  logical :: lnoval
  integer :: lpos,ierrl,iostat
  lnoval = .false.
  if(present(noval)) lnoval = noval
  ierrl = 0
#ifdef __IOTK_STREAMS
  if(present(setpos)) then
    lpos=setpos
  else
    inquire(unit,pos=lpos,iostat=iostat)
    if(iostat/=0) then
      call iotk_error_issue(ierrl,"iotk_stream_read",__FILE__,__LINE__)
# 79 "iotk_stream.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.4 ")
# 79 "iotk_stream.spp"
call iotk_error_msg(ierrl,'""')
# 79 "iotk_stream.spp"
call iotk_error_write(ierrl,"iostat",iostat)
      goto 1
    end if
  end if
  if(present(getpos)) getpos = lpos
  read(unit,pos=lpos,iostat=iostat) rec
  if(iostat/=0) then
    call iotk_error_issue(ierrl,"iotk_stream_read",__FILE__,__LINE__)
# 86 "iotk_stream.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.4 ")
# 86 "iotk_stream.spp"
call iotk_error_msg(ierrl,'""')
# 86 "iotk_stream.spp"
call iotk_error_write(ierrl,"iostat",iostat)
    goto 1
  end if
  read(unit,iostat=iostat) header
  if(iostat/=0) then
    call iotk_error_issue(ierrl,"iotk_stream_read",__FILE__,__LINE__)
# 91 "iotk_stream.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.4 ")
# 91 "iotk_stream.spp"
call iotk_error_msg(ierrl,'""')
# 91 "iotk_stream.spp"
call iotk_error_write(ierrl,"iostat",iostat)
    goto 1
  end if
  if(.not.lnoval) then
    read(unit,iostat=iostat) val
    if(iostat/=0) then
      call iotk_error_issue(ierrl,"iotk_stream_read",__FILE__,__LINE__)
# 97 "iotk_stream.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.4 ")
# 97 "iotk_stream.spp"
call iotk_error_msg(ierrl,'""')
# 97 "iotk_stream.spp"
call iotk_error_write(ierrl,"iostat",iostat)
      goto 1
    end if
  end if
  read(unit,pos=lpos+iotk_record_length+rec,iostat=iostat) rec1
  if(iostat/=0) then
    call iotk_error_issue(ierrl,"iotk_stream_read",__FILE__,__LINE__)
# 103 "iotk_stream.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.4 ")
# 103 "iotk_stream.spp"
call iotk_error_msg(ierrl,'""')
# 103 "iotk_stream.spp"
call iotk_error_write(ierrl,"iostat",iostat)
    goto 1
  end if
  if(rec1/=rec) then
    call iotk_error_issue(ierrl,"iotk_stream_read",__FILE__,__LINE__)
# 107 "iotk_stream.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.4 ")
# 107 "iotk_stream.spp"
call iotk_error_msg(ierrl,'""')
    goto 1
  end if
#else
  call iotk_error_issue(ierrl,"iotk_stream_read",__FILE__,__LINE__)
# 111 "iotk_stream.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.4 ")
# 111 "iotk_stream.spp"
call iotk_error_msg(ierrl,'Streams are not implemented')
#endif
1 continue
  if(present(ierr)) then
    ierr = ierrl
  else
    if(ierrl/=0) call iotk_error_handler(ierrl)
  end if
end subroutine iotk_stream_read_CHARACTER1
#endif
# 51 "iotk_stream.spp"

#ifdef __IOTK_CHARACTER2
# 54 "iotk_stream.spp"
subroutine iotk_stream_read_CHARACTER2(unit,header,val,setpos,getpos,noval,ierr)
  use iotk_base
  use iotk_error_interf
  implicit none
  integer,                                      intent(in)  :: unit
  integer(iotk_header_kind),                    intent(out) :: header
  CHARACTER(kind=__IOTK_CHARACTER2,len=*),      intent(out) :: val(:)
  integer,                            optional, intent(in)  :: setpos
  integer,                            optional, intent(out) :: getpos
  logical,                            optional, intent(in)  :: noval
  integer,                            optional, intent(out) :: ierr
#ifdef __IOTK_STREAMS
  integer(iotk_record_kind) :: rec,rec1
#endif
  logical :: lnoval
  integer :: lpos,ierrl,iostat
  lnoval = .false.
  if(present(noval)) lnoval = noval
  ierrl = 0
#ifdef __IOTK_STREAMS
  if(present(setpos)) then
    lpos=setpos
  else
    inquire(unit,pos=lpos,iostat=iostat)
    if(iostat/=0) then
      call iotk_error_issue(ierrl,"iotk_stream_read",__FILE__,__LINE__)
# 79 "iotk_stream.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.4 ")
# 79 "iotk_stream.spp"
call iotk_error_msg(ierrl,'""')
# 79 "iotk_stream.spp"
call iotk_error_write(ierrl,"iostat",iostat)
      goto 1
    end if
  end if
  if(present(getpos)) getpos = lpos
  read(unit,pos=lpos,iostat=iostat) rec
  if(iostat/=0) then
    call iotk_error_issue(ierrl,"iotk_stream_read",__FILE__,__LINE__)
# 86 "iotk_stream.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.4 ")
# 86 "iotk_stream.spp"
call iotk_error_msg(ierrl,'""')
# 86 "iotk_stream.spp"
call iotk_error_write(ierrl,"iostat",iostat)
    goto 1
  end if
  read(unit,iostat=iostat) header
  if(iostat/=0) then
    call iotk_error_issue(ierrl,"iotk_stream_read",__FILE__,__LINE__)
# 91 "iotk_stream.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.4 ")
# 91 "iotk_stream.spp"
call iotk_error_msg(ierrl,'""')
# 91 "iotk_stream.spp"
call iotk_error_write(ierrl,"iostat",iostat)
    goto 1
  end if
  if(.not.lnoval) then
    read(unit,iostat=iostat) val
    if(iostat/=0) then
      call iotk_error_issue(ierrl,"iotk_stream_read",__FILE__,__LINE__)
# 97 "iotk_stream.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.4 ")
# 97 "iotk_stream.spp"
call iotk_error_msg(ierrl,'""')
# 97 "iotk_stream.spp"
call iotk_error_write(ierrl,"iostat",iostat)
      goto 1
    end if
  end if
  read(unit,pos=lpos+iotk_record_length+rec,iostat=iostat) rec1
  if(iostat/=0) then
    call iotk_error_issue(ierrl,"iotk_stream_read",__FILE__,__LINE__)
# 103 "iotk_stream.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.4 ")
# 103 "iotk_stream.spp"
call iotk_error_msg(ierrl,'""')
# 103 "iotk_stream.spp"
call iotk_error_write(ierrl,"iostat",iostat)
    goto 1
  end if
  if(rec1/=rec) then
    call iotk_error_issue(ierrl,"iotk_stream_read",__FILE__,__LINE__)
# 107 "iotk_stream.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.4 ")
# 107 "iotk_stream.spp"
call iotk_error_msg(ierrl,'""')
    goto 1
  end if
#else
  call iotk_error_issue(ierrl,"iotk_stream_read",__FILE__,__LINE__)
# 111 "iotk_stream.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.4 ")
# 111 "iotk_stream.spp"
call iotk_error_msg(ierrl,'Streams are not implemented')
#endif
1 continue
  if(present(ierr)) then
    ierr = ierrl
  else
    if(ierrl/=0) call iotk_error_handler(ierrl)
  end if
end subroutine iotk_stream_read_CHARACTER2
#endif
# 51 "iotk_stream.spp"

#ifdef __IOTK_CHARACTER3
# 54 "iotk_stream.spp"
subroutine iotk_stream_read_CHARACTER3(unit,header,val,setpos,getpos,noval,ierr)
  use iotk_base
  use iotk_error_interf
  implicit none
  integer,                                      intent(in)  :: unit
  integer(iotk_header_kind),                    intent(out) :: header
  CHARACTER(kind=__IOTK_CHARACTER3,len=*),      intent(out) :: val(:)
  integer,                            optional, intent(in)  :: setpos
  integer,                            optional, intent(out) :: getpos
  logical,                            optional, intent(in)  :: noval
  integer,                            optional, intent(out) :: ierr
#ifdef __IOTK_STREAMS
  integer(iotk_record_kind) :: rec,rec1
#endif
  logical :: lnoval
  integer :: lpos,ierrl,iostat
  lnoval = .false.
  if(present(noval)) lnoval = noval
  ierrl = 0
#ifdef __IOTK_STREAMS
  if(present(setpos)) then
    lpos=setpos
  else
    inquire(unit,pos=lpos,iostat=iostat)
    if(iostat/=0) then
      call iotk_error_issue(ierrl,"iotk_stream_read",__FILE__,__LINE__)
# 79 "iotk_stream.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.4 ")
# 79 "iotk_stream.spp"
call iotk_error_msg(ierrl,'""')
# 79 "iotk_stream.spp"
call iotk_error_write(ierrl,"iostat",iostat)
      goto 1
    end if
  end if
  if(present(getpos)) getpos = lpos
  read(unit,pos=lpos,iostat=iostat) rec
  if(iostat/=0) then
    call iotk_error_issue(ierrl,"iotk_stream_read",__FILE__,__LINE__)
# 86 "iotk_stream.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.4 ")
# 86 "iotk_stream.spp"
call iotk_error_msg(ierrl,'""')
# 86 "iotk_stream.spp"
call iotk_error_write(ierrl,"iostat",iostat)
    goto 1
  end if
  read(unit,iostat=iostat) header
  if(iostat/=0) then
    call iotk_error_issue(ierrl,"iotk_stream_read",__FILE__,__LINE__)
# 91 "iotk_stream.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.4 ")
# 91 "iotk_stream.spp"
call iotk_error_msg(ierrl,'""')
# 91 "iotk_stream.spp"
call iotk_error_write(ierrl,"iostat",iostat)
    goto 1
  end if
  if(.not.lnoval) then
    read(unit,iostat=iostat) val
    if(iostat/=0) then
      call iotk_error_issue(ierrl,"iotk_stream_read",__FILE__,__LINE__)
# 97 "iotk_stream.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.4 ")
# 97 "iotk_stream.spp"
call iotk_error_msg(ierrl,'""')
# 97 "iotk_stream.spp"
call iotk_error_write(ierrl,"iostat",iostat)
      goto 1
    end if
  end if
  read(unit,pos=lpos+iotk_record_length+rec,iostat=iostat) rec1
  if(iostat/=0) then
    call iotk_error_issue(ierrl,"iotk_stream_read",__FILE__,__LINE__)
# 103 "iotk_stream.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.4 ")
# 103 "iotk_stream.spp"
call iotk_error_msg(ierrl,'""')
# 103 "iotk_stream.spp"
call iotk_error_write(ierrl,"iostat",iostat)
    goto 1
  end if
  if(rec1/=rec) then
    call iotk_error_issue(ierrl,"iotk_stream_read",__FILE__,__LINE__)
# 107 "iotk_stream.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.4 ")
# 107 "iotk_stream.spp"
call iotk_error_msg(ierrl,'""')
    goto 1
  end if
#else
  call iotk_error_issue(ierrl,"iotk_stream_read",__FILE__,__LINE__)
# 111 "iotk_stream.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.4 ")
# 111 "iotk_stream.spp"
call iotk_error_msg(ierrl,'Streams are not implemented')
#endif
1 continue
  if(present(ierr)) then
    ierr = ierrl
  else
    if(ierrl/=0) call iotk_error_handler(ierrl)
  end if
end subroutine iotk_stream_read_CHARACTER3
#endif
# 51 "iotk_stream.spp"

#ifdef __IOTK_CHARACTER4
# 54 "iotk_stream.spp"
subroutine iotk_stream_read_CHARACTER4(unit,header,val,setpos,getpos,noval,ierr)
  use iotk_base
  use iotk_error_interf
  implicit none
  integer,                                      intent(in)  :: unit
  integer(iotk_header_kind),                    intent(out) :: header
  CHARACTER(kind=__IOTK_CHARACTER4,len=*),      intent(out) :: val(:)
  integer,                            optional, intent(in)  :: setpos
  integer,                            optional, intent(out) :: getpos
  logical,                            optional, intent(in)  :: noval
  integer,                            optional, intent(out) :: ierr
#ifdef __IOTK_STREAMS
  integer(iotk_record_kind) :: rec,rec1
#endif
  logical :: lnoval
  integer :: lpos,ierrl,iostat
  lnoval = .false.
  if(present(noval)) lnoval = noval
  ierrl = 0
#ifdef __IOTK_STREAMS
  if(present(setpos)) then
    lpos=setpos
  else
    inquire(unit,pos=lpos,iostat=iostat)
    if(iostat/=0) then
      call iotk_error_issue(ierrl,"iotk_stream_read",__FILE__,__LINE__)
# 79 "iotk_stream.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.4 ")
# 79 "iotk_stream.spp"
call iotk_error_msg(ierrl,'""')
# 79 "iotk_stream.spp"
call iotk_error_write(ierrl,"iostat",iostat)
      goto 1
    end if
  end if
  if(present(getpos)) getpos = lpos
  read(unit,pos=lpos,iostat=iostat) rec
  if(iostat/=0) then
    call iotk_error_issue(ierrl,"iotk_stream_read",__FILE__,__LINE__)
# 86 "iotk_stream.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.4 ")
# 86 "iotk_stream.spp"
call iotk_error_msg(ierrl,'""')
# 86 "iotk_stream.spp"
call iotk_error_write(ierrl,"iostat",iostat)
    goto 1
  end if
  read(unit,iostat=iostat) header
  if(iostat/=0) then
    call iotk_error_issue(ierrl,"iotk_stream_read",__FILE__,__LINE__)
# 91 "iotk_stream.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.4 ")
# 91 "iotk_stream.spp"
call iotk_error_msg(ierrl,'""')
# 91 "iotk_stream.spp"
call iotk_error_write(ierrl,"iostat",iostat)
    goto 1
  end if
  if(.not.lnoval) then
    read(unit,iostat=iostat) val
    if(iostat/=0) then
      call iotk_error_issue(ierrl,"iotk_stream_read",__FILE__,__LINE__)
# 97 "iotk_stream.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.4 ")
# 97 "iotk_stream.spp"
call iotk_error_msg(ierrl,'""')
# 97 "iotk_stream.spp"
call iotk_error_write(ierrl,"iostat",iostat)
      goto 1
    end if
  end if
  read(unit,pos=lpos+iotk_record_length+rec,iostat=iostat) rec1
  if(iostat/=0) then
    call iotk_error_issue(ierrl,"iotk_stream_read",__FILE__,__LINE__)
# 103 "iotk_stream.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.4 ")
# 103 "iotk_stream.spp"
call iotk_error_msg(ierrl,'""')
# 103 "iotk_stream.spp"
call iotk_error_write(ierrl,"iostat",iostat)
    goto 1
  end if
  if(rec1/=rec) then
    call iotk_error_issue(ierrl,"iotk_stream_read",__FILE__,__LINE__)
# 107 "iotk_stream.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.4 ")
# 107 "iotk_stream.spp"
call iotk_error_msg(ierrl,'""')
    goto 1
  end if
#else
  call iotk_error_issue(ierrl,"iotk_stream_read",__FILE__,__LINE__)
# 111 "iotk_stream.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.4 ")
# 111 "iotk_stream.spp"
call iotk_error_msg(ierrl,'Streams are not implemented')
#endif
1 continue
  if(present(ierr)) then
    ierr = ierrl
  else
    if(ierrl/=0) call iotk_error_handler(ierrl)
  end if
end subroutine iotk_stream_read_CHARACTER4
#endif
# 123 "iotk_stream.spp"

# 125 "iotk_stream.spp"
subroutine iotk_stream_backspace_x(unit,ierr)
  use iotk_base
  use iotk_error_interf
  implicit none
  integer,           intent(in)  :: unit
  integer, optional, intent(out) :: ierr
#ifdef __IOTK_STREAMS
  integer(iotk_record_kind) :: rec
#endif
  integer :: pos,iostat,ierrl
  ierrl=0
#ifdef __IOTK_STREAMS
  inquire(unit,pos=pos,iostat=iostat)
  if(iostat/=0) then
    call iotk_error_issue(ierrl,"iotk_stream_backspace",__FILE__,__LINE__)
# 139 "iotk_stream.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.4 ")
# 139 "iotk_stream.spp"
call iotk_error_msg(ierrl,'""')
# 139 "iotk_stream.spp"
call iotk_error_write(ierrl,"iostat",iostat)
    goto 1
  end if
  read(unit,pos=pos-iotk_record_length,iostat=iostat) rec
  if(iostat/=0) then
    call iotk_error_issue(ierrl,"iotk_stream_backspace",__FILE__,__LINE__)
# 144 "iotk_stream.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.4 ")
# 144 "iotk_stream.spp"
call iotk_error_msg(ierrl,'""')
# 144 "iotk_stream.spp"
call iotk_error_write(ierrl,"iostat",iostat)
    goto 1
  end if
  read(unit,pos=pos-2*iotk_record_length-rec,iostat=iostat)
  if(iostat/=0) then
    call iotk_error_issue(ierrl,"iotk_stream_backspace",__FILE__,__LINE__)
# 149 "iotk_stream.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.4 ")
# 149 "iotk_stream.spp"
call iotk_error_msg(ierrl,'""')
# 149 "iotk_stream.spp"
call iotk_error_write(ierrl,"iostat",iostat)
    goto 1
  end if
#else
  call iotk_error_issue(ierrl,"iotk_stream_backspace",__FILE__,__LINE__)
# 153 "iotk_stream.spp"
call iotk_error_msg(ierrl,"CVS Revision: 1.4 ")
# 153 "iotk_stream.spp"
call iotk_error_msg(ierrl,'Streams are not implemented')
#endif
1 continue
  if(present(ierr)) then
    ierr = ierrl
  else
    if(ierrl/=0) call iotk_error_handler(ierrl)
  end if
end subroutine iotk_stream_backspace_x

