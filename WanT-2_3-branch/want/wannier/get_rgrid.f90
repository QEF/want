! 
! Copyright (C) 2005 WanT Group
! 
! This file is distributed under the terms of the 
! GNU General Public License. See the file `License' 
! in the root directory of the present distribution, 
! or http://www.gnu.org/copyleft/gpl.txt . 
! 
!*********************************************************
SUBROUTINE get_rgrid(nr, nrtot, wr, ivr)
   !*********************************************************
   !
   ! Given the three generators nr(:), this subroutine defines
   ! a R-grid in real space. Output is in crystal coords
   !
   USE kinds
   USE constants,         ONLY : ONE
   !
   IMPLICIT NONE

! <INFO>
!
! This subroutine generates the regular R-vector grid according to
! the input generators nr(1:3).
!

   INTEGER,      INTENT(IN)      :: nr(3), nrtot
   REAL(dbl),    INTENT(OUT)     :: wr(nrtot)
   INTEGER,      INTENT(OUT)     :: ivr(3,nrtot)

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
   ! setup ivr in crystal coordinates
   !
   ir = 0

   DO k=1,nr(3)
   DO j=1,nr(2)
   DO i=1,nr(1)
       !
       ir = ir + 1
       ! 
       ivr(1,ir) =  i -( nr(1) +1 ) / 2
       ivr(2,ir) =  j -( nr(2) +1 ) / 2
       ivr(3,ir) =  k -( nr(3) +1 ) / 2
       !
       ! the sum rule on the weights depends on the definition 
       ! of the kpt-weight sum
       !
       wr( ir ) = ONE
       !
   ENDDO
   ENDDO
   ENDDO
 
   IF ( nrtot /= ir ) CALL errore(subname,'something nasty happened',2)

END SUBROUTINE get_rgrid


