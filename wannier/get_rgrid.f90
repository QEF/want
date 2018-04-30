! 
! Copyright (C) 2005 WanT Group
! 
! This file is distributed under the terms of the 
! GNU General Public License. See the file `License' 
! in the root directory of the present distribution, 
! or http://www.gnu.org/copyleft/gpl.txt . 
! 
!*********************************************************
SUBROUTINE get_rgrid(nr, nrtot, wr, vr, avec)
   !*********************************************************
   USE kinds
   USE constants,         ONLY : ONE
   USE converters_module, ONLY : cry2cart
   IMPLICIT NONE

! <INFO>
! This subroutine generates the regular R-vector grid according to
! the input generators nr(1:3).
!

   INTEGER,  INTENT(in)      :: nr(3)
   INTEGER,  INTENT(in)      :: nrtot
   REAL(dbl),INTENT(out)     :: wr(nrtot)
   REAL(dbl),INTENT(out)     :: vr(3,nrtot)
   REAL(dbl),INTENT(in)      :: avec(3,3)

! </INFO>
! ... local variables

   CHARACTER(9)              :: subname="get_rgrid"
   INTEGER                   :: ir
   INTEGER                   :: i,j,k

!
! ... end of declarations
!-------------------------------------------------------------
!

   IF ( ANY(nr(:) <= 0 )  ) CALL errore(subname,'invalid nr',1)
   
   !
   ! setup vr in crystal coordinates
   !
   ir = 0

   DO k=1,nr(3)
   DO j=1,nr(2)
   DO i=1,nr(1)
        !
        ir = ir + 1
        ! 
        vr(1,ir) = REAL( i - ( nr(1)+1)/2, dbl )
        vr(2,ir) = REAL( j - ( nr(2)+1)/2, dbl )
        vr(3,ir) = REAL( k - ( nr(3)+1)/2, dbl )
        !
        ! the sum rule on the weights depends on the definition 
        ! of the kpt-weight sum
        !
        wr( ir ) = ONE
   ENDDO
   ENDDO
   ENDDO
 
   IF ( nrtot /= ir ) CALL errore(subname,'something nasty happened',2)

   !
   ! move to cartesian coord (bohr)
   !
   CALL cry2cart( vr(1:3,1:nrtot), avec )


END SUBROUTINE get_rgrid


