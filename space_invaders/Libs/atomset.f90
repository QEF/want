! 
! Copyright (C) 2004 Arrigo Calzolari, Carlo Cavazzoni, Marco Buongiorno Nardelli 
! Copyright (C) 2002 Nicola Marzari, Ivo Souza, David Vanderbilt 
! 
! This file is distributed under the terms of the 
! GNU General Public License. See the file `License' 
! in the root directory of the present distribution, 
! or http://www.gnu.org/copyleft/gpl.txt . 
! 
!=---------------------------------------------------------------------------------
       SUBROUTINE atomset( alat, avec, ntype, natom, nameat, rat, mxdtyp, mxdatm )
!=---------------------------------------------------------------------------------

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
       REAL(dbl) :: zero,one,three
       PARAMETER ( zero = 0.0d0 )
       PARAMETER ( one = 1.0d0 )
 

       DO j = 1, 3
         DO i = 1, 3
           sgn = one
           IF ( avec(i,j) < zero ) sgn = -sgn
           avec(i,j) = ABS(avec(i,j)) * sgn * alat
         END DO
       END DO

 
       IF ( ( ntype > mxdtyp ) .OR. ( ntype <= 0 ) ) &
              CALL errore(' atomset ', ' increase max atom type (mxdtyp) ', ntype )

       DO nt = 1, ntype
         IF( ( natom(nt) > mxdatm ) .OR. ( natom(nt) <= 0 ) )  &
               CALL errore(' atomset ', ' increase max number of atoms (mxdatm) ', natom(nt) )

         jmax = natom(nt)
         DO ja = 1, jmax
           DO i = 1, 3
             sgn = one
             IF (rat(i,ja,nt) <  zero) sgn = -sgn
             rat(i,ja,nt) = ABS( rat(i,ja,nt) ) * sgn
           END DO
         END DO
       END DO

       ntt = 0
       DO nt = 1, ntype
         jmax = natom(nt)
         DO ja = 1, jmax
           ntt = ntt + 1
           car(1) = avec(1,1)*rat(1,ja,nt) +  avec(1,2)*rat(2,ja,nt) + avec(1,3)*rat(3,ja,nt)
           car(2) = avec(2,1)*rat(1,ja,nt) +  avec(2,2)*rat(2,ja,nt) + avec(2,3)*rat(3,ja,nt)
           car(3) = avec(3,1)*rat(1,ja,nt) +  avec(3,2)*rat(2,ja,nt) + avec(3,3)*rat(3,ja,nt)
         END DO
       END DO
 
! ...  Scales the coordinates by twopi 
 
       DO nt = 1, ntype
         DO ja = 1, natom(nt)
           DO i = 1, 3
             rat(i,ja,nt) = twopi * rat(i,ja,nt)
           END DO
         END DO
       END DO
 
       RETURN
       END SUBROUTINE
