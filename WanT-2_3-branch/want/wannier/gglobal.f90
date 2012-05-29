!
! Copyright (C) 2002-2005 FPMD-CPV groups
! This file is distributed under the terms of the
! GNU General Public License. See the file `License'
! in the root directory of the present distribution,
! or http://www.gnu.org/copyleft/gpl.txt .
!
! This subroutine has been adapted (2009) by A. Ferretti from the 
! routine gglobal in ~espresso/CPV/cp_fpmd.f90
!
!-------------------------------------------------------------------------
SUBROUTINE gglobal( ng, gvect, gmap, b1, b2, b3, nr1, nr2, nr3, gcut, lgam )
!-------------------------------------------------------------------------

  USE kinds,     ONLY: dbl

  IMPLICIT NONE

  INTEGER,    INTENT(OUT) :: ng
  INTEGER,    INTENT(OUT) :: gvect(3,*)
  INTEGER,    INTENT(IN)  :: nr1, nr2, nr3
  INTEGER,    INTENT(OUT) :: gmap(-nr1:nr1, -nr2:nr2, -nr3:nr3)
  REAL(dbl),  INTENT(IN)  :: b1(3), b2(3), b3(3), gcut
  LOGICAL,    INTENT(IN)  :: lgam

  INTEGER   :: nr1m1, nr2m1, nr3m1
  INTEGER   :: i, j, k, ir
  INTEGER, ALLOCATABLE :: idx(:)

  REAL(dbl) :: g2, g2_g(nr1*nr2*nr3), t(3)


      nr1m1=nr1-1
      nr2m1=nr2-1
      nr3m1=nr3-1

      ng = 0
      gmap(-nr1:nr1,-nr2:nr2,-nr3:nr3)= 0
!
!     exclude space with x<0
!
      loopx: do i= -nr1m1,nr1m1
         if( lgam .AND. ( i < 0 ) ) cycle loopx
         loopy: do j=-nr2m1,nr2m1
! ...       exclude plane with x=0, y<0
            if( lgam .AND. ( i.eq.0.and.j.lt.0) ) cycle loopy
            loopz: do k=-nr3m1,nr3m1
! ...          exclude line with x=0, y=0, z<0
               if( lgam .AND. (i.eq.0.and.j.eq.0.and.k.lt.0)) cycle loopz
               g2=0.d0
               do ir=1,3
                  t(ir) = DBLE(i)*b1(ir)+DBLE(j)*b2(ir)+DBLE(k)*b3(ir)
                  g2=g2+t(ir)*t(ir)
               end do
               if(g2 <= gcut) then
                 ng=ng+1
                 g2_g(ng)=g2
                 gvect(1,ng)=i
                 gvect(2,ng)=j
                 gvect(3,ng)=k
                 gmap(i,j,k)=ng
               end if
            end do loopz
         end do loopy
      end do loopx
      !
      ALLOCATE( idx( ng ) )
      !
      CALL sort_gvec( ng, g2_g, gvect, idx )
      !
      DO i = 1, ng
         gmap( gvect(1,i), gvect(2,i), gvect(3,i) ) = i
      ENDDO
      !
      DEALLOCATE( idx )
      !
  RETURN
END SUBROUTINE gglobal

