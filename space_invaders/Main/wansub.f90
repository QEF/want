!-----------------------------------------------------------------------

      subroutine inv3(a,b,det)
 
      dimension a(3,3),b(3,3)
      dimension z(6,6)

!     quick routine to invert 3x3 matrix
!     inverse is of a(i,j) is b(i,j)/det

      do 20 i=1,2
      do 20 j=1,2
      do 20 k=1,3
      do 20 l=1,3
      kk=3*(i-1)+k
      ll=3*(j-1)+l
   20 z(kk,ll)=a(k,l)
      det=0.
      do 100 i=1,3
  100 det=det+z(1,i)*z(2,i+1)*z(3,i+2)
      do 200 i=4,6
  200 det=det-z(1,i)*z(2,i-1)*z(3,i-2)
      do 300 j=1,3
      do 300 i=1,3
  300 b(j,i)=(z(i+1,j+1)*z(i+2,j+2)-z(i+1,j+2)*z(i+2,j+1))

      return
      end subroutine

!-----------------------------------------------------------------------

      subroutine compar(a,b,ifpos,ifneg)
        implicit none
        real*8 :: a(3),b(3), rrp, rrm
        integer :: ifpos, ifneg
        rrp=(a(1)-b(1))**2+(a(2)-b(2))**2+(a(3)-b(3))**2
        rrm=(a(1)+b(1))**2+(a(2)+b(2))**2+(a(3)+b(3))**2
        ifpos=0
        if (abs(rrp).lt.1.e-08) ifpos=1
        ifneg=0
        if (abs(rrm).lt.1.e-08) ifneg=1
        return
      end subroutine

!-----------------------------------------------------------------------

      subroutine phases(nbands,nkpts,nkpts2,nnmx,nnmxh,nntot,nnh,neigh, &
     &           bk,bka,cm,csheet,sheet,rguide,irguide)
      
      IMPLICIT COMPLEX (C)
      dimension cm(nbands,nbands,nkpts,nnmx), csum(nnmxh)
      dimension bk(3,nkpts,nnmx), bka(3,nnmxh), nntot(nkpts)
      dimension neigh(nkpts,nnmxh), rguide(3,nbands)
      dimension xx(nnmx), smat(3,3),svec(3),sinv(3,3)
      dimension csheet(nbands,nkpts,nnmx),sheet(nbands,nkpts,nnmx)

      dimension rave(3,nbands), rnkb(nbands,nkpts,nnmx)

      ci=(0.,1.)

! report problem to solve
! for each band, csum is determined and then its appropriate
! guiding center rguide(3,nwann)

      do nwann=1,nbands

! get average phase for each unique bk direction

       do na=1,nnh
        csum(na)=(0.0,0.0)
        do nkp=1,nkpts2
         nn=neigh(nkp,na)
         csum(na)=csum(na)+cm(nwann,nwann,nkp,nn)
        end do
       end do

! now analyze that information to get good guess at
! wannier center

!      write (*,*)
!      do na=1,nnh
!       write (*,'(a,3f10.5,a,2f10.5)')
!    &    ' bka=',(bka(j,na),j=1,3),'  csum=',csum(na)
!      end do

! problem is to find a real-space 3-vector rguide such that
!   phase of csum(nn) ~= phase of exp[ -i bka(nn) dot rguide ]
! or, letting
!   xx(nn) = - Im ln csum(nn)  (modulo 2*pi)
! then
!   bka(nn) dot rguide ~= xx(nn)
!
! we take an arbitrary branch cut for first three xx(nn)
! and determine rguide from these; then for each additional bka
! vector, we first determine the most consistent branch cut,
! and then update rguide
!
! rguide is obtained by minimizing
!   sum_nn [ bka(nn) dot rguide - xx(nn) ] ^2
! or, setting the derivative with respect to rcenter to zero,
!   sum_i smat(j,i) * rguide(i,nwann) = svec(j)
! where
!   smat(j,i) = sum_nn bka(j,nn) * bka(i,nn)
!   svec(j)   = sum_nn bka(j,nn) * xx(nn)

! initialize smat and svec
       do j=1,3
        do i=1,3
          smat(j,i)=0.
        end do
        svec(j)=0.
       end do

       write(*,*) ' '

       do nn=1,nnh

        if (nn.le.3) then
!         obtain xx with arbitrary branch cut choice
          xx(nn)=-aimag(log(csum(nn)))
        else
!         obtain xx with branch cut choice guided by rguide
          xx0=0.
          do j=1,3
            xx0=xx0+bka(j,nn)*rguide(j,nwann)
          end do
!         xx0 is expected value for xx
          csumt=cexp(ci*xx0)
!         csumt has opposite of expected phase of csum(nn)
          xx(nn)=xx0-aimag(log(csum(nn)*csumt))
        endif

        write (*,'(a,i5,3f7.3,2f10.5)') 'nn, bka, xx, mag =', &
     &    nn,(bka(j,nn),j=1,3),xx(nn),abs(csum(nn))/float(nkpts2)

!       update smat and svec
        do j=1,3
          do i=1,3
            smat(j,i)=smat(j,i)+bka(j,nn)*bka(i,nn)
          end do
          svec(j)=svec(j)+bka(j,nn)*xx(nn)
        end do

        if (nn.ge.3) then
!         determine rguide
          call inv3(smat,sinv,det)
!         the inverse of smat is sinv/det
          if (abs(det).lt.1.e-06) then
!           evidently first three bka vectors are linearly dependent
!           this is not allowed
            write (*,*) ' error in findr: dependency'
            stop
          endif
          if (irguide.ne.0) then
          do j=1,3
            rguide(j,nwann)=0.
            do i=1,3
              rguide(j,nwann)=rguide(j,nwann)+sinv(j,i)*svec(i)/det
            end do
          end do
          end if
          write (*,'(a,3f10.5)') 'rguide =',(rguide(i,nwann),i=1,3)
        endif

       end do

      end do

!     obtain branch cut choice guided by rguid
  
      do nkp=1,nkpts2
       do nwann=1,nbands
        do nn=1,nntot(nkp)
         sheet(nwann,nkp,nn)=0.0
         do j=1,3
          sheet(nwann,nkp,nn)=sheet(nwann,nkp,nn)+ &
     &                        bk(j,nkp,nn)*rguide(j,nwann)
         end do
         csheet(nwann,nkp,nn)=cexp(ci*sheet(nwann,nkp,nn))
        end do
       end do
      end do

! now check that we picked the proper sheet for the log
! of cm. criterion: q_n^{k,b}=Im(ln(M_nn^{k,b})) + b \cdot r_n are
! circa 0 for a good solution, circa multiples of 2 pi  for a bad one.
! I use the guiding center, instead of r_n, to understand which could be
! right sheet

!     write(*,*) ' '
!     write(*,*) 'Approximate q_n^{k,b}, using the guiding centers'
!     write(*,*) ' '

      do nkp=1,nkpts2
       do m=1,nbands
        do nn=1,nntot(nkp)
         rnkb(m,nkp,nn)=0.0
         brn=0.0
         do ind=1,3
          brn=brn+bk(ind,nkp,nn)*rguide(ind,m)
         end do
         rnkb(m,nkp,nn)=rnkb(m,nkp,nn)+brn
        end do
       end do
      end do
      do nkp=1,nkpts2
       do n=1,nbands
        do nn=1,nntot(nkp)
         pherr=aimag(log(csheet(n,nkp,nn)*cm(n,n,nkp,nn))) &
     &   -sheet(n,nkp,nn)+rnkb(n,nkp,nn)-aimag(log(cm(n,n,nkp,nn)))
         if (abs(pherr).gt.3.1416) then
      write(*,'(3i4,f18.9,3f10.5)') nkp,n,nn,pherr,(bk(i,nkp,nn),i=1,3)
         end if
        end do
100     continue
       end do
      end do

      return
      end

!-----------------------------------------------------------------------

      subroutine omega  &
     &   (nbands, nkpts, nkpts2, nntot, nnmx, nnlist, bk, wb, cm, &
     &   csheet, sheet, rave, r2ave, func_om1, func_om2, func_om3, func_o)
      
      IMPLICIT COMPLEX (C)
      dimension nnlist(nkpts,nnmx),nntot(nkpts)
      dimension wb(nkpts,nnmx)
      dimension bk(3,nkpts,nnmx), rave(3,nbands), rtot(3)
      dimension r2ave(nbands), rave2(nbands), bim(3), csumt(3)
      dimension cm(nbands,nbands,nkpts,nnmx)
      dimension csheet(nbands,nkpts,nnmx),sheet(nbands,nkpts,nnmx)
! - prova
      dimension craven(3,nbands), crtotn(3)
      dimension r2aven(nbands), rave2n(nbands)
! - prova

      ! write(*,*) '------------'
      ! write(*,*) nbands
      ! write(*,*) '------------'
      ! write(*,*) nkpts 
      ! write(*,*) '------------'
      ! write(*,*) nkpts2 
      ! write(*,*) '------------'
      ! write(*,*) nntot
      ! write(*,*) '------------'
      ! write(*,*) nnmx
      ! write(*,*) '------------'
      ! write(*,*) nnlist
      ! write(*,*) '------------'
      ! write(*,*) bk
      ! write(*,*) '------------'
      ! write(*,*) wb
      ! write(*,*) '------------'
      ! write(*,*) cm
      ! write(*,*) '------------'
      ! write(*,*) csheet 
      ! write(*,*) '------------'
      ! write(*,*) sheet

      do nwann=1,nbands
       do ind=1,3
        rave(ind,nwann)=0.0 
        do nkp=1,nkpts2
         do nn=1,nntot(nkp)
          rave(ind,nwann)=rave(ind,nwann)+wb(nkp,nn)*bk(ind,nkp,nn)* &
     &     ( aimag(log(csheet(nwann,nkp,nn)*cm(nwann,nwann,nkp,nn))) &
     &     -sheet(nwann,nkp,nn) )
         end do
        end do
        rave(ind,nwann)=-rave(ind,nwann)/float(nkpts2)
       end do
      end do
      do nwann=1,nbands
       rave2(nwann)=0.0
       do ind=1,3
        rave2(nwann)=rave2(nwann)+rave(ind,nwann)**2
       end do
      end do
      do ind=1,3
       rtot(ind)=0.0
       do nwann=1,nbands
        rtot(ind)=rtot(ind)+rave(ind,nwann)
       end do
      end do

      do nwann=1,nbands
       r2ave(nwann)=0.0 
       do nkp=1,nkpts2
        do nn=1,nntot(nkp)
!        write(*,'(3i4,3x,f15.9)') nwann,nkp,nn,
!    &   aimag(log(cm(nwann,nwann,nkp,nn)))
         r2ave(nwann)=r2ave(nwann)+wb(nkp,nn)*(1.0- &
     &   cm(nwann,nwann,nkp,nn)*conjg(cm(nwann,nwann,nkp,nn))+  &
     &   ( aimag(log(csheet(nwann,nkp,nn)*cm(nwann,nwann,nkp,nn))) &
     &     -sheet(nwann,nkp,nn) )**2)
        end do
       end do
       r2ave(nwann)=r2ave(nwann)/float(nkpts2)
       write(*,1000) nwann,(rave(ind,nwann),ind=1,3), &
     &                      r2ave(nwann)-rave2(nwann)
      end do
      r2tot=0.0
      do nwann=1,nbands
       r2tot=r2tot+r2ave(nwann)-rave2(nwann)
      end do
      write(*,*) ' '
      write(*,1001) (rtot(ind),ind=1,3),r2tot
      write(*,*) ' '
1000  format(1x,'Wannier center and spread', &
     &       i3,2x,'(',f10.6,',',f10.6,',',f10.6,')',f15.8)
1001  format(1x,'Sum of centers and spreads' &
     &       4x,'(',f10.6,',',f10.6,',',f10.6,')',f15.8)

! - prova -------------------------

!     write(*,*) ' '
      do nwann=1,nbands
       do ind=1,3
        craven(ind,nwann)=0.0 
        do nkp=1,nkpts2
         do nn=1,nntot(nkp)
          craven(ind,nwann)=craven(ind,nwann)+wb(nkp,nn)* &
     &     bk(ind,nkp,nn)*cm(nwann,nwann,nkp,nn)
         end do
        end do
        craven(ind,nwann)= &
     &   (0.0,1.0)*craven(ind,nwann)/float(nkpts2)
       end do
      end do
      do nwann=1,nbands
       rave2n(nwann)=0.0
       do ind=1,3
        rave2n(nwann)=rave2n(nwann)+ &
     &   craven(ind,nwann)*conjg(craven(ind,nwann))
       end do
      end do
      do ind=1,3
       crtotn(ind)=0.0
       do nwann=1,nbands
        crtotn(ind)=crtotn(ind)+craven(ind,nwann)
       end do
      end do
      
      do nwann=1,nbands
!      write(*,2000) nwann,(craven(ind,nwann),ind=1,3)
      end do
!     write(*,*) ' '
!     write(*,2001) (crtotn(ind),ind=1,3),r2totn
!     write(*,*) ' '

2000  format(1x,'Wannier center and spread', &
     &       i3,2x,'(',2f10.6,',',2f10.6,',',2f10.6,')')
2001  format(1x,'Sum of centers and spreads' &
     &       4x,'(',2f10.6,',',2f10.6,',',2f10.6,')',f15.8)
! - prova -------------------------

      func_om=0.0
      do nwann=1,nbands
       rave2(nwann)=0.0
       do i=1,3
        rave2(nwann)=rave2(nwann)+rave(i,nwann)*rave(i,nwann)
       end do
       func_om=func_om+r2ave(nwann)-rave2(nwann)
      end do

      func_om1=0.0
      do nkp=1,nkpts2
       do nn=1,nntot(nkp)
        do nwann=1,nbands
         func_om1=func_om1+wb(nkp,nn)* &
     &   (1.0-cm(nwann,nwann,nkp,nn)*conjg(cm(nwann,nwann,nkp,nn)))
        end do
       end do
      end do
      func_om1=func_om1/float(nkpts2)

      func_om2=0.0
      do nwann=1,nbands
       sqim=0.0
       do nkp=1,nkpts2
        do nn=1,nntot(nkp)
         sqim=sqim+wb(nkp,nn)*(( aimag(log(csheet(nwann,nkp,nn)* &
     &    cm(nwann,nwann,nkp,nn)))-sheet(nwann,nkp,nn) )**2)
        end do
       end do
       sqim=sqim/float(nkpts2)
       func_om2=func_om2+sqim
      end do

      func_om3=0.0
      do nwann=1,nbands
       do ind=1,3
        bim(ind)=0.0
        do nkp=1,nkpts2
         do nn=1,nntot(nkp)
          bim(ind)=bim(ind)+wb(nkp,nn)*bk(ind,nkp,nn)* &
     &    ( aimag(log(csheet(nwann,nkp,nn)*cm(nwann,nwann,nkp,nn))) &
     &    -sheet(nwann,nkp,nn) )
         end do
        end do
        bim(ind)=bim(ind)/float(nkpts2)
       end do
       bim2=0.0 
       do ind=1,3
        bim2=bim2+bim(ind)*bim(ind)
       end do
       func_om3=func_om3-bim2
      end do

! - prova ----------------------

      func_n=0.0
      do m=1,nbands
       do n=1,nbands
        if (n.ne.m) then
         do nkp=1,nkpts2
          do nn=1,nntot(nkp)
           func_n=func_n+wb(nkp,nn)* &
     &     conjg(cm(m,n,nkp,nn))*cm(m,n,nkp,nn)
          end do
         end do
        end if
       end do
      end do
      func_n=func_n/float(nkpts2)
      write(*,*) '------ func_n',func_n

      func_n=0.0
      do m=1,nbands
       do n=1,nbands
        if (n.ne.m) then
         do nkp=1,nkpts2
          sum=0.0
          do ind=1,3
           csumt(ind)=0.0
           do nn=1,nntot(nkp)
            csumt(ind)=csumt(ind)+ &
     &      wb(nkp,nn)*bk(ind,nkp,nn)*cm(m,n,nkp,nn)
           end do
           sum=sum+csumt(ind)*conjg(csumt(ind))
          end do
          func_n=func_n+sum
         end do
        end if
       end do
      end do
      func_n=func_n/float(nkpts2)
      write(*,*) '------ func_n',func_n
      
! - prova ----------------------

      func_n=0.0
      do m=1,nbands
       do n=1,nbands
        do nkp=1,nkpts2
         sum=0.0
         do ind=1,3
          csumt(ind)=0.0
          do nn=1,nntot(nkp)
           if (m.eq.n) then
            csumt(ind)=csumt(ind)+wb(nkp,nn)*bk(ind,nkp,nn)*(0.0,1.0)* &
     &       ( aimag(log(csheet(n,nkp,nn)*cm(n,n,nkp,nn))) &
     &       -sheet(n,nkp,nn) )
           else
            csumt(ind)=csumt(ind)+ &
     &       wb(nkp,nn)*bk(ind,nkp,nn)*cm(m,n,nkp,nn)
           end if
          end do
          sum=sum+csumt(ind)*conjg(csumt(ind))
         end do
!        do nn=1,nntot(nkp)
!         sum=sum+wb(nkp,nn)*cm(m,n,nkp,nn)*conjg(cm(m,n,nkp,nn))
!        end do
!        if (m.ne.n) func_n=func_n+sum
         func_n=func_n+sum
        end do
       end do
      end do
      func_n=func_n/float(nkpts2)
      func_i=0.0
      do n=1,nbands
       func_n=func_n-rave2(n)
       func_i=func_i+r2ave(n)
      end do
!     write(*,1029) func_i
      func_i=func_i-func_n
      write(*,1029) func_i
      write(*,1030) func_n

      write(*,1005) func_om1
      write(*,1006) func_om2
      write(*,1007) func_om3
      write(*,1008) func_om2+func_om3
      write(*,*) ' '
1005  format(1x,'Omega1   is   ',f15.9)
1006  format(1x,'Omega2   is   ',f15.9)
1007  format(1x,'Omega3   is   ',f15.9)
1008  format(1x,'Omega2+3 is   ',f15.9)
1029  format(1x,'Omega_nI is   ',f15.9)
1030  format(1x,'Omega_n~ is   ',f15.9)

      func_i=0.0
      do nkp=1,nkpts2
       do nn=1,nntot(nkp)
        sum=0.0
        do m=1,nbands
         do n=1,nbands
          sum=sum+cm(m,n,nkp,nn)*conjg(cm(m,n,nkp,nn))
         end do
        end do
        func_i=func_i+wb(nkp,nn)*(float(nbands)-sum)
       end do
      end do
      func_i=func_i/float(nkpts2)

      func_od=0.0
      do nkp=1,nkpts2
       do nn=1,nntot(nkp)
        sum=0.0
        do m=1,nbands
         do n=1,nbands
          sum=sum+wb(nkp,nn)*cm(m,n,nkp,nn)*conjg(cm(m,n,nkp,nn))
          if (m.eq.n) sum=sum- &
     &     wb(nkp,nn)*cm(m,n,nkp,nn)*conjg(cm(m,n,nkp,nn))
         end do
        end do
        func_od=func_od+sum
       end do
      end do
      func_od=func_od/float(nkpts2)

      func_d=0.0
      do nkp=1,nkpts2
       do nn=1,nntot(nkp)
        sum=0.0
        do n=1,nbands
         brn=0.0
         do ind=1,3
          brn=brn+bk(ind,nkp,nn)*rave(ind,n)
         end do
         sum=sum+wb(nkp,nn)* &
     &            (aimag(log(csheet(n,nkp,nn)*cm(n,n,nkp,nn))) &
     &            -sheet(n,nkp,nn)+brn)**2
        end do
        func_d=func_d+sum
       end do
      end do
      func_d=func_d/float(nkpts2)

      write(*,1010) func_i
      write(*,1011) func_d
      write(*,1012) func_od
      write(*,1014) func_od+func_d
      write(*,*) ' '
      func_o=func_i+func_d+func_od
      write(*,1013) func_o

      write(*,*) ' '
1010  format(1x,'Omega I  is   ',f25.19)
1011  format(1x,'Omega D  is   ',f15.9)
1012  format(1x,'Omega OD is   ',f15.9)
1014  format(1x,'Omegatld is   ',f15.9)
1013  format(1x,'Omega    is   ',f15.9)

      return
      end

!-----------------------------------------------------------------------

      subroutine domega(nbands,nkpts,nkpts2,nntot,nnmx,nnlist,bk,wb, &
     &cm,csheet,sheet,cr,crt,rave,r2ave,rnkb,cdodq1,cdodq2,cdodq3, &
     &cdodq)
      
      IMPLICIT COMPLEX (C)
      dimension nnlist(nkpts,nnmx), nntot(nkpts)
      dimension wb(nkpts,nnmx)
      dimension bk(3,nkpts,nnmx), rave(3,nbands)
      dimension r2ave(nbands), rnkb(nbands,nkpts,nnmx)
      dimension csheet(nbands,nkpts,nnmx),sheet(nbands,nkpts,nnmx)
      dimension cm(nbands,nbands,nkpts,nnmx), &
     & cr(nbands,nbands,nkpts,nnmx), &
     & crt(nbands,nbands,nkpts,nnmx), &
     & cdodq(nbands,nbands,nkpts), &
     & cdodq1(nbands,nbands,nkpts), &
     & cdodq2(nbands,nbands,nkpts), &
     & cdodq3(nbands,nbands,nkpts)

! recalculate rave

      do nwann=1,nbands
       do ind=1,3
        rave(ind,nwann)=0.0 
        do nkp=1,nkpts2
         do nn=1,nntot(nkp)
          rave(ind,nwann)=rave(ind,nwann)+wb(nkp,nn)*bk(ind,nkp,nn)* &
     &    ( aimag(log(csheet(nwann,nkp,nn)*cm(nwann,nwann,nkp,nn))) &
     &      -sheet(nwann,nkp,nn) )
         end do
        end do
        rave(ind,nwann)=-rave(ind,nwann)/float(nkpts2)
       end do
      end do

! R_mn=M_mn/M_nn and q_m^{k,b} = Im phi_m^{k,b} + b.r_n are calculated

      do nkp=1,nkpts2
       do m=1,nbands
        do nn=1,nntot(nkp)
         do n=1,nbands
! old minimization 
          crt(m,n,nkp,nn)=cm(m,n,nkp,nn)/cm(n,n,nkp,nn)
! new minimization 
          cr(m,n,nkp,nn)=cm(m,n,nkp,nn)*conjg(cm(n,n,nkp,nn))
         end do
         rnkb(m,nkp,nn)=0.0
         brn=0.0
         do ind=1,3
          brn=brn+bk(ind,nkp,nn)*rave(ind,m)
         end do
         rnkb(m,nkp,nn)=rnkb(m,nkp,nn)+brn
        end do
       end do
      end do

! cd0dq(m,n,nkp) is calculated

      do nkp=1,nkpts2
       do m=1,nbands
        do n=1,nbands
         cdodq1(m,n,nkp)=(0.0,0.0)
         cdodq2(m,n,nkp)=(0.0,0.0)
         cdodq3(m,n,nkp)=(0.0,0.0)
         do nn=1,nntot(nkp)
! A[R^{k,b}]=(R-Rdag)/2
          cdodq1(m,n,nkp)=cdodq1(m,n,nkp)+wb(nkp,nn)* &
     &     (cr(m,n,nkp,nn)/2.0- &
     &     conjg(cr(n,m,nkp,nn))/2.0)
! -S[T^{k,b}]=-(T+Tdag)/2i ; T_mn = Rt_mn q_n
          cdodq2(m,n,nkp)=cdodq2(m,n,nkp)-wb(nkp,nn)* &
     &     ( crt(m,n,nkp,nn) * ( aimag(log(csheet(n,nkp,nn) &
     &      *cm(n,n,nkp,nn)))-sheet(n,nkp,nn) ) + &
     &      conjg( crt(n,m,nkp,nn) * ( aimag(log(csheet(m,nkp,nn) &
     &      *cm(m,m,nkp,nn)))-sheet(m,nkp,nn) ) ) ) &
     &     /(0.0,2.0)
          cdodq3(m,n,nkp)=cdodq3(m,n,nkp)-wb(nkp,nn)* &
     &     (crt(m,n,nkp,nn)*rnkb(n,nkp,nn)+ &
     &      conjg(crt(n,m,nkp,nn)*rnkb(m,nkp,nn))) &
     &      /(0.0,2.0)
         end do
        cdodq1(m,n,nkp)=4.0*cdodq1(m,n,nkp)/float(nkpts2)
        cdodq2(m,n,nkp)=4.0*cdodq2(m,n,nkp)/float(nkpts2)
        cdodq3(m,n,nkp)=4.0*cdodq3(m,n,nkp)/float(nkpts2)
        cdodq(m,n,nkp)=cdodq1(m,n,nkp)+cdodq2(m,n,nkp)+cdodq3(m,n,nkp)
        end do
       end do
      end do

      return
      end

!-----------------------------------------------------------------------

      subroutine spline(x,y,n,y2)
      parameter(nmax=500)
      dimension x(n),y(n),y2(n),u(nmax)

      y2(1)=0.0
      u(1)=0.0
      qn=0.0
      un=0.0

      do i=2,n-1
       sig=(x(i)-x(i-1))/(x(i+1)-x(i-1))
       p=sig*y2(i-1)+2.0
       y2(i)=(sig-1.0)/p
       u(i)=(6.0*((y(i+1)-y(i))/(x(i+1)-x(i))-(y(i)-y(i-1)) &
     &  /(x(i)-x(i-1)))/(x(i+1)-x(i-1))-sig*u(i-1))/p
      end do

      y2(n)=(un-qn*u(n-1))/(qn*y2(n-1)+1.0)
      do k=n-1,1,-1
       y2(k)=y2(k)*y2(k+1)+u(k)
      end do
  
      return
      end

!-----------------------------------------------------------------------

      subroutine splint(xa,ya,y2a,n,x,y)
      dimension xa(n),ya(n),y2a(n)

      klo=1
      khi=n
1     if (khi-klo.gt.1) then
       k=(khi+klo)/2
       if (xa(k).gt.x) then
        khi=k
       else
        klo=k
       end if
       goto 1
      end if
      h=xa(khi)-xa(klo)
      if (h.eq.0.) pause 'bad xa input in splint'
      a=(xa(khi)-x)/h
      b=(x-xa(klo))/h
      y=a*ya(klo)+b*ya(khi)+ &
     & ((a**3-a)*y2a(klo)+(b**3-b)*y2a(khi))*(h**2)/6.0
 
      return
      end
!--------------------------------------------------------------------
!
! Mathematical Subroutines, from Castep 
!
! to be compiled with a double precision option !!!!!!!!!!!
!
!--------------------------------------------------------------------
      subroutine io(nunit,inpout)
! N. Marzari, Jan 94
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
!--------------------------------------------------------------------
      FUNCTION RANF(IDUM)
      PARAMETER (M=714025,IA=1366,IC=150889,RM=1.0/M)
      DIMENSION IR(97)
      DATA IFF /0/
      IF(IDUM.LT.0.OR.IFF.EQ.0) THEN
        IFF=1
        IDUM=MOD(IC-IDUM,M)
        DO 11 J=1,97
           IDUM=MOD(IA*IDUM+IC,M)
           IR(J)=IDUM
  11    CONTINUE
        IDUM=MOD(IA*IDUM+IC,M)
        IY=IDUM
      ENDIF
      J=1+(97*IY)/M
      IF(J.GT.97.OR.J.LT.1) THEN
      PRINT *,' RANF: J=',J
      STOP
      ENDIF
      IY=IR(J)
      RANF=IY*RM
      IDUM=MOD(IA*IDUM+IC,M)
      IR(J)=IDUM
      RETURN
      END
!--------------------------------------------------------------------
      SUBROUTINE BASTR(DIR,REC,VOL)
!
!                       18/2/90: WRITTEN BY BH
!
! THIS SUBROUTINE CALCULATES THE SET OF BASIS VECTORS IN RECIPROCAL
! SPACE.
!                                          
! IN REAL SPACE THE BASIS VECTORS A1, A2 AND A3 ARE STORED IN DIR(i,j):
!
!                ( DIR(1,1) DIR(2,1) DIR(3,1) )   ( '  '  ' )
!     DIR(i,j) = ( DIR(1,2) DIR(2,2) DIR(3,2) ) = ( A1 A2 A3)
!                ( DIR(1,3) DIR(2,3) DIR(3,3) )   ( '  '  ' )
!     
!     A1 = ( DIR(1,1) , DIR(1,2) , DIR(1,3) )
!     
!     A2 = ( DIR(2,1) , DIR(2,2) , DIR(2,3) )
!     
!     A2 = ( DIR(3,1) , DIR(3,2) , DIR(3,3) )
!
! THE VOLUME OF THE UNIT CELL IS GIVEN BY:
!                     
!     VOL = | A1*( A2 x A3) |
!
! THE BASISVECTORS IN RECIPROCAL SPACE WILL BE GIVEN BY:
!                      
!     B1 = 2*PI*( A2 x A3 ) / VOL
!                      
!     B2 = 2*PI*( A3 x A1 ) / VOL
!                      
!     B3 = 2*PI*( A1 x A2 ) / VOL
!
! WHICH IS STORED IN REC(i,j):
!
!                ( REC(1,1) REC(2,1) REC(3,1) )   ( '  '  ' )
!     REC(i,j) = ( REC(1,2) REC(2,2) REC(3,2) ) = ( B1 B2 B3)
!                ( REC(1,3) REC(2,3) REC(3,3) )   ( '  '  ' )
!
!========================================================================
      IMPLICIT COMPLEX (C)
      DIMENSION DIR(3,3)
      DIMENSION REC(3,3)
      DATA TPI /6.2831853072/
!
      REC(1,1)=DIR(2,2)*DIR(3,3)-DIR(3,2)*DIR(2,3)
      REC(1,2)=DIR(2,3)*DIR(3,1)-DIR(3,3)*DIR(2,1)
      REC(1,3)=DIR(2,1)*DIR(3,2)-DIR(3,1)*DIR(2,2)
      REC(2,1)=DIR(3,2)*DIR(1,3)-DIR(1,2)*DIR(3,3)
      REC(2,2)=DIR(3,3)*DIR(1,1)-DIR(1,3)*DIR(3,1)
      REC(2,3)=DIR(3,1)*DIR(1,2)-DIR(1,1)*DIR(3,2)
      REC(3,1)=DIR(1,2)*DIR(2,3)-DIR(2,2)*DIR(1,3)
      REC(3,2)=DIR(1,3)*DIR(2,1)-DIR(2,3)*DIR(1,1)
      REC(3,3)=DIR(1,1)*DIR(2,2)-DIR(2,1)*DIR(1,2)
      VOL=DIR(1,1)*REC(1,1)+DIR(1,2)*REC(1,2)+DIR(1,3)*REC(1,3)
      DO 100 I=1,3
        DO 50 J=1,3
          REC(I,J)=TPI*REC(I,J)/VOL
50      CONTINUE
100   CONTINUE
      VOL=ABS(VOL)
      RETURN
      END

