      subroutine io(nunit,inpout)
C N. Marzari, Jan 94
      character*8 fldat
      logical lopen

      if (nunit.le.9) then
        write(*,*) 'ERROR - nunit.le.9'
      else if (nunit.le.99) then
        write(fldat,'(''FILE00'',i2)') nunit
      else if (nunit.le.999) then
        write(fldat,'(''FILE0'',i3)') nunit
      else if (nunit.le.9999) then
        write(fldat,'(''FILE'',i4)') nunit
      else
        write(*,*) 'ERROR - nunit.gt.9999'
      end if

      inpout=20

      inquire(unit=inpout,opened=lopen)
      if (lopen) then
        write(*,*) 'ERROR - connected unit',nunit,inpout
        stop
      end if
      open(unit=inpout,file=fldat,form='unformatted',status='unknown')
      
      return
      end
