!
! Copyright (C) 2004 WanT Group
!
! This file is distributed under the terms of the
! GNU General Public License. See the file `License'
! in the root directory of the present distribution,
! or http://www.gnu.org/copyleft/gpl.txt .
!
! Taken from Numerical Recipies
!
!=----------------------------------------------------------------------------------=
      SUBROUTINE indexx( n, arr, indx )
!=----------------------------------------------------------------------------------=
 
      USE kinds
      IMPLICIT NONE

      INTEGER :: m, nstack
      PARAMETER ( M = 7 )
      PARAMETER ( NSTACK = 50 )

      INTEGER :: n, indx( n )
      REAL(dbl) :: arr(n)
      INTEGER :: i, j, k, l
      INTEGER :: indxt, ir, itemp
      INTEGER :: jstack, istack(nstack)
      REAL(dbl) :: a

      DO 11 j = 1, n
        indx(j) = j
11    CONTINUE

      jstack = 0
      l = 1
      ir = n

1     IF ( ir - l < m )THEN

        DO 13 j = l + 1, ir
          indxt = indx(j)
          a = arr(indxt)

          DO 12 i = j-1, l, -1
            IF ( arr(indx(i) ) <=  a ) GOTO 2
            indx(i+1) = indx(i)
12        CONTINUE

          i = l - 1
2         indx(i+1) = indxt
13      CONTINUE

        IF ( jstack == 0 ) RETURN

        ir = istack(jstack)
        l = istack( jstack-1 )
        jstack = jstack - 2

      ELSE

        k = ( l + ir ) / 2
        itemp = indx( k )
        indx(k) = indx( l+1 )
        indx(l+1) = itemp

        IF ( arr(indx(l) ) > arr( indx(ir) ) ) THEN
          itemp = indx(l)
          indx(l) = indx(ir)
          indx(ir) = itemp
        END IF

        IF ( arr( indx(l+1 ) ) > arr( indx(ir) ) ) THEN
          itemp = indx(l+1)
          indx(l+1) = indx(ir)
          indx(ir) = itemp
        END IF

        IF ( arr( indx(l) ) > arr( indx( l+1) ) ) THEN
          itemp = indx(l)
          indx(l) = indx(l+1)
          indx(l+1) = itemp
        END IF

        i = l + 1
        j = ir
        indxt = indx( l + 1 )
        a = arr( indxt )
3       CONTINUE

        i = i + 1

        IF ( arr( indx(i) ) < a) GOTO 3

4       CONTINUE

        j = j - 1

        IF ( arr(indx(j) ) > a ) GOTO 4

        IF ( j < i ) GOTO 5

        itemp = indx(i)
        indx(i) = indx(j)
        indx(j) = itemp
        GOTO 3

5       indx(l+1) = indx(j)
        indx(j) = indxt
        jstack = jstack + 2

        IF ( jstack > nstack) CALL errore('indexx', 'NSTACK too small in indexx', jstack)

        IF ( ir - i+1 >= j-l) THEN

          istack(jstack) = ir
          istack(jstack-1) = i
          ir = j - 1

        ELSE

          istack(jstack) = j - 1
          istack(jstack-1) = l
          l = i
        END IF

      END IF

      GOTO 1

      END SUBROUTINE
