      SUBROUTINE bastr( dir, rec, vol )

!................................................................
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
!................................................................

      IMPLICIT NONE

      REAL*8 :: dir(3,3)
      REAL*8 :: rec(3,3)
      REAL*8 :: vol
      REAL*8 :: pi, twopi
      PARAMETER ( pi = 3.14159265358979323846d0 )
      PARAMETER ( twopi = 2.0d0 * pi )
 
      INTEGER :: i, j
 

      rec(1,1) = dir(2,2) * dir(3,3) - dir(3,2) * dir(2,3)
      rec(1,2) = dir(2,3) * dir(3,1) - dir(3,3) * dir(2,1)
      rec(1,3) = dir(2,1) * dir(3,2) - dir(3,1) * dir(2,2)
      rec(2,1) = dir(3,2) * dir(1,3) - dir(1,2) * dir(3,3)
      rec(2,2) = dir(3,3) * dir(1,1) - dir(1,3) * dir(3,1)
      rec(2,3) = dir(3,1) * dir(1,2) - dir(1,1) * dir(3,2)
      rec(3,1) = dir(1,2) * dir(2,3) - dir(2,2) * dir(1,3)
      rec(3,2) = dir(1,3) * dir(2,1) - dir(2,3) * dir(1,1)
      rec(3,3) = dir(1,1) * dir(2,2) - dir(2,1) * dir(1,2)

      vol = dir(1,1) * rec(1,1) + dir(1,2) * rec(1,2) + dir(1,3) * rec(1,3)

      DO i = 1, 3
        DO j = 1, 3
          rec(i,j) = twopi * rec(i,j) / vol
        END DO
      END DO
      vol = ABS(vol)

      RETURN
      END SUBROUTINE
