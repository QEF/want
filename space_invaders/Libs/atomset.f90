       SUBROUTINE atomset( alatt, avec, ntype, natom, nameat, rat, mxdtyp, mxdatm )

       IMPLICIT NONE
 
       INTEGER :: mxdtyp,mxdatm
       REAL*8  :: avec(3,3)
       INTEGER :: ntype
       INTEGER :: natom(mxdtyp)
       CHARACTER( LEN=2 ) :: nameat(mxdtyp)
       REAL*8 :: rat(3,mxdatm,mxdtyp)
 
       REAL*8 :: car(3)
 
       REAL*8 :: alatt,sgn
       INTEGER :: i, j, ja, jmax
       INTEGER :: nt, ntt
       REAL*8 :: zero,um,tres
       REAL*8 :: pi,twopi
       PARAMETER ( pi = 3.14159265358979323846d0 )
       PARAMETER ( twopi = 2.0d0 * pi )
       PARAMETER ( zero = 0.0d0 )
       PARAMETER ( um = 1.0d0 )
       PARAMETER ( tres = 3.0d0 )
 
       WRITE(6,200) alatt

       DO j = 1, 3
         DO i = 1, 3
           sgn = um
           IF ( avec(i,j) < zero ) sgn = -sgn
           avec(i,j) = ABS(avec(i,j)) * sgn * alatt
         END DO
       END DO

       WRITE(6,201)
       DO J=1,3
         WRITE(6,202) j, ( avec(i,j), i=1,3 ), ( avec(i,j) / alatt, i=1,3 )
       END DO
 
       IF ( ( ntype > mxdtyp ) .OR. ( ntype <= 0 ) ) THEN
         WRITE(6,300) ntype
         STOP
       END IF

       DO nt=1,ntype
         IF( ( natom(nt) > mxdatm ) .OR. ( natom(nt) <= 0 ) ) THEN
           WRITE(6,301) natom(nt)
           STOP
         END IF

         jmax = natom(nt)
         DO ja=1,jmax
           DO i=1,3
             sgn = um
             IF (rat(i,ja,nt) <  zero) sgn = -sgn
             rat(i,ja,nt) = ABS( rat(i,ja,nt) ) * sgn
           END DO
         END DO
       END DO

       WRITE(6,206)
       ntt = 0
       DO nt = 1, ntype
         jmax = natom(nt)
         DO ja = 1, jmax
           ntt = ntt + 1
           car(1) = avec(1,1)*rat(1,ja,nt) +  avec(1,2)*rat(2,ja,nt) + avec(1,3)*rat(3,ja,nt)
           car(2) = avec(2,1)*rat(1,ja,nt) +  avec(2,2)*rat(2,ja,nt) + avec(2,3)*rat(3,ja,nt)
           car(3) = avec(3,1)*rat(1,ja,nt) +  avec(3,2)*rat(2,ja,nt) + avec(3,3)*rat(3,ja,nt)
           WRITE(6,207) ntt, nameat(nt), ( rat(i,ja,nt), i=1,3 ), (car(i), i=1,3 )
         END DO
       END DO
 
! ...  Scales the coordinates by twopi and convert mass
 
       DO nt = 1, ntype
         DO ja = 1, natom(nt)
           DO i = 1, 3
             rat(i,ja,nt) = twopi * rat(i,ja,nt)
           END DO
         END DO
       END DO
 
 100   FORMAT(F12.6)
 101   FORMAT(3(2X,A1,F12.6))
 102   FORMAT(I5)
 103   FORMAT(I5,3X,A2)
 200   FORMAT(//,'  CRYSTAL STRUCTURE:',//,'  LATTICE CONSTANT ',  F12.5,' (A.U.)',/)
 201   FORMAT(/,'  PRIMITIVE TRANSLATION VECTORS',/,25X,'IN A.U.',  22X,'IN LATTICE UNITS')
 202   FORMAT('  A',I1,'=',3(2X,E11.5),5X,3(2X,F7.3))
 206   FORMAT(/,'  NO. TYPE     POSITION(LATTICE COORD.)  ', '  POSITION(CARTESIAN COORD.)',5X,'MASS'/)
 207   FORMAT(1X,I3,3X,A2,2X,3(2X,F9.5),3X,3(2X,E11.5),5X)
 300   FORMAT('  ***  STOPPED IN READAT   ',     '  INCREASE MXDTYP TO ',I6)
 301   FORMAT('  ***  STOPPED IN READAT   ',    '  INCREASE MXDATM TO ',I6)

       RETURN
       END SUBROUTINE
