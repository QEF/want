!
! Copyright (C) 2004 Arrigo Calzolari, Carlo Cavazzoni, Marco Buongiorno Nardelli
! Copyright (C) 2002 Nicola Marzari, Ivo Souza, David Vanderbilt
! Copyright (C) 1993 Sverre Froyen
!
! This file is distributed under the terms of the
! GNU General Public License. See the file `License'
! in the root directory of the present distribution,
! or http://www.gnu.org/copyleft/gpl.txt .
!
!=----------------------------------------------------------------------------------=
       SUBROUTINE LATTI(AVEC,BVEC,VCELL,BDOT,AMINV,ADOT)
!=----------------------------------------------------------------------------------=

       USE kinds
       USE constants, ONLY: pi, twopi => tpi

       IMPLICIT NONE
 
       REAL(dbl) :: avec(3,3)
       REAL(dbl) :: bvec(3,3), bdot(3,3), aminv(3), adot(3,3)
       REAL(dbl) :: vcell
 
       INTEGER :: i, j, nt, jmax, ja
 
       REAL(dbl) eps
       PARAMETER ( eps = 1.0d-10 )
 
! ...  Compute the lattice wave-vectors, the cell volume and the inner products.
 
       bvec(1,1) = avec(2,2) * avec(3,3) - avec(3,2) * avec(2,3)
       bvec(2,1) = avec(3,2) * avec(1,3) - avec(1,2) * avec(3,3)
       bvec(3,1) = avec(1,2) * avec(2,3) - avec(2,2) * avec(1,3)
       bvec(1,2) = avec(2,3) * avec(3,1) - avec(3,3) * avec(2,1)
       bvec(2,2) = avec(3,3) * avec(1,1) - avec(1,3) * avec(3,1)
       bvec(3,2) = avec(1,3) * avec(2,1) - avec(2,3) * avec(1,1)
       bvec(1,3) = avec(2,1) * avec(3,2) - avec(3,1) * avec(2,2)
       bvec(2,3) = avec(3,1) * avec(1,2) - avec(1,1) * avec(3,2)
       bvec(3,3) = avec(1,1) * avec(2,2) - avec(2,1) * avec(1,2)
 
! ...  Cell volume
 
       vcell = bvec(1,1) * avec(1,1) + bvec(2,1) * avec(2,1) + bvec(3,1) * avec(3,1)
       IF ( ABS(vcell) < eps ) CALL errore(' latti ', ' cell volume ', vcell )
 
       DO j = 1, 3
         aminv(j) = avec(1,j) * avec(1,j) + avec(2,j) * avec(2,j) + avec(3,j) * avec(3,j)
         aminv(j) = twopi / SQRT( aminv(j) )
       END DO

       DO J = 1, 3
         bvec(1,j) = twopi * bvec(1,j) / vcell
         bvec(2,j) = twopi * bvec(2,j) / vcell
         bvec(3,j) = twopi * bvec(3,j) / vcell
       END DO
       vcell = ABS(vcell)
 
! ...  Compute metric bdot(i,j)
 
       bdot(1,1) = bvec(1,1) * bvec(1,1) + bvec(2,1) * bvec(2,1) + bvec(3,1) * bvec(3,1)
       bdot(2,2) = bvec(1,2) * bvec(1,2) + bvec(2,2) * bvec(2,2) + bvec(3,2) * bvec(3,2)
       bdot(3,3) = bvec(1,3) * bvec(1,3) + bvec(2,3) * bvec(2,3) + bvec(3,3) * bvec(3,3)
       bdot(1,2) = bvec(1,1) * bvec(1,2) + bvec(2,1) * bvec(2,2) + bvec(3,1) * bvec(3,2)
       bdot(1,3) = bvec(1,1) * bvec(1,3) + bvec(2,1) * bvec(2,3) + bvec(3,1) * bvec(3,3)
       bdot(2,3) = bvec(1,2) * bvec(1,3) + bvec(2,2) * bvec(2,3) + bvec(3,2) * bvec(3,3)
       bdot(2,1) = bdot(1,2)
       bdot(3,1) = bdot(1,3)
       bdot(3,2) = bdot(2,3)
 
! ...  Compute metric in real space
 
       adot(1,1) = avec(1,1) * avec(1,1) + avec(2,1) * avec(2,1) + avec(3,1) * avec(3,1)
       adot(2,2) = avec(1,2) * avec(1,2) + avec(2,2) * avec(2,2) + avec(3,2) * avec(3,2)
       adot(3,3) = avec(1,3) * avec(1,3) + avec(2,3) * avec(2,3) + avec(3,3) * avec(3,3)
       adot(1,2) = avec(1,1) * avec(1,2) + avec(2,1) * avec(2,2) + avec(3,1) * avec(3,2)
       adot(1,3) = avec(1,1) * avec(1,3) + avec(2,1) * avec(2,3) + avec(3,1) * avec(3,3)
       adot(2,3) = avec(1,2) * avec(1,3) + avec(2,2) * avec(2,3) + avec(3,2) * avec(3,3)
       adot(2,1) = adot(1,2)
       adot(3,1) = adot(1,3)
       adot(3,2) = adot(2,3)
 
       RETURN
       END SUBROUTINE
