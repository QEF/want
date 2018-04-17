# 2 "iotk.spp"

program iotk
  use iotk_module
  use iotk_base
  use iotk_error_interf
  implicit none
  integer :: iarg,iostat,pos,ierrl
  character(iotk_linlenx) :: args(iotk_maxargs)

  ierrl = 0
  iarg = 0
  do
    iarg = iarg + 1
    if(iarg>size(args)) then
      call iotk_error_issue(ierrl,"",__FILE__,__LINE__)
      goto 1
    end if
    read(*,"(a)",iostat=iostat) args(iarg)
    if(iostat<0) exit
    if(iostat>0) then
      call iotk_error_issue(ierrl,"",__FILE__,__LINE__)
      goto 1
    end if
    pos = scan(args(iarg),"|",back=.true.)
    if(pos>0 .and. args(iarg)(pos:)=="|") then
      args(iarg)(pos:pos) = iotk_eos
    else
      pos = len_trim(args(iarg)) + 1
      if(pos<=len(args)) args(iarg)(pos:pos) = iotk_eos
    end if
  end do
  iarg = iarg -1
  call iotk_tool(args(1:iarg))
1 continue
  if(ierrl/=0) call iotk_error_handler(ierrl)
end program iotk
