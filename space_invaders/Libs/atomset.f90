       SUBROUTINE atomset( alat, avec, ntype, natom, nameat, rat, mxdtyp, mxdatm )

       USE kinds
       USE constants, ONLY: pi, twopi => tpi
       USE io_global, ONLY : stdout

       IMPLICIT NONE
 
       INTEGER :: mxdtyp,mxdatm
       REAL(dbl)  :: avec(3,3)
       INTEGER :: ntype
       INTEGER :: natom(mxdtyp)
       CHARACTER( LEN=2 ) :: nameat(mxdtyp)
       REAL(dbl) :: rat(3,mxdatm,mxdtyp)
 
       REAL(dbl) :: car(3)
 
       REAL(dbl) :: alat,sgn
       INTEGER :: i, j, ja, jmax
       INTEGER :: nt, ntt
       REAL(dbl) :: zero,um,tres
       PARAMETER ( zero = 0.0d0 )
       PARAMETER ( um = 1.0d0 )
       PARAMETER ( tres = 3.0d0 )
 
       WRITE(stdout , fmt= " (//,2x,'Crystal structure:',//,2x, &
             'lattice constant = ', F8.4, ' (Bohr)' )" ) alat

       DO j = 1, 3
         DO i = 1, 3
           sgn = um
           IF ( avec(i,j) < zero ) sgn = -sgn
           avec(i,j) = ABS(avec(i,j)) * sgn * alat
         END DO
       END DO

       WRITE(stdout , fmt= " (/,'  Primitive translation vectors',/,25X, &
            'in a.u.',  22x,'in lattice units' ) " ) 
       DO J=1,3
         WRITE(stdout , fmt= " ( 2x, 'a', I1, '=', 3(2X,E11.5), 5X, 3(2X,F7.3) ) " )  &
               j, ( avec(i,j), i=1,3 ), ( avec(i,j) / alat, i=1,3 )
       END DO
 
       IF ( ( ntype > mxdtyp ) .OR. ( ntype <= 0 ) ) &
              CALL errore(' atomset ', ' increase max atom type (mxdtyp) ', ntype )

       DO nt = 1, ntype
         IF( ( natom(nt) > mxdatm ) .OR. ( natom(nt) <= 0 ) )  &
               CALL errore(' atomset ', ' increase max number of atoms (mxdatm) ', natom(nt) )

         jmax = natom(nt)
         DO ja = 1, jmax
           DO i = 1, 3
             sgn = um
             IF (rat(i,ja,nt) <  zero) sgn = -sgn
             rat(i,ja,nt) = ABS( rat(i,ja,nt) ) * sgn
           END DO
         END DO
       END DO

       WRITE(stdout , fmt= " (/,2x, 'No. type     position(lattice coord.)  ', &
                            '  position(cartesian coord.)', 5x, 'mass'/ )")
       ntt = 0
       DO nt = 1, ntype
         jmax = natom(nt)
         DO ja = 1, jmax
           ntt = ntt + 1
           car(1) = avec(1,1)*rat(1,ja,nt) +  avec(1,2)*rat(2,ja,nt) + avec(1,3)*rat(3,ja,nt)
           car(2) = avec(2,1)*rat(1,ja,nt) +  avec(2,2)*rat(2,ja,nt) + avec(2,3)*rat(3,ja,nt)
           car(3) = avec(3,1)*rat(1,ja,nt) +  avec(3,2)*rat(2,ja,nt) + avec(3,3)*rat(3,ja,nt)
           WRITE(stdout , fmt= " ( 1x, i3, 3x, a2, 2x, 3( 2x, f9.5 ), 3x, 3( 2x, e11.5 ), 5x )" ) &
               ntt, nameat(nt), ( rat(i,ja,nt), i=1,3 ), ( car(i), i = 1, 3 )
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
 
       RETURN
       END SUBROUTINE
