! 
! Copyright (C) 2004 Arrigo Calzolari, Carlo Cavazzoni, Marco Buongiorno Nardelli 
! Copyright (C) 2002 Nicola Marzari, Ivo Souza, David Vanderbilt 
! Copyright (C) 1997 Nicola Marzari, David Vanderbilt 
! 
! This file is distributed under the terms of the 
! GNU General Public License. See the file `License' 
! in the root directory of the present distribution, 
! or http://www.gnu.org/copyleft/gpl.txt . 
! 
!=-----------------------------------------------------------------------------------
       FUNCTION dot_bloch( vec1, vec2, isort1, isort2, mtxd1, mtxd2, mxddim, npw )
!=-----------------------------------------------------------------------------------

       USE kinds   
       USE parameters, ONLY : npwx
       IMPLICIT NONE
 
       COMPLEX(dbl) :: dot_bloch

       INTEGER :: mxddim
       COMPLEX(dbl) :: vec1(mxddim+1)
       COMPLEX(dbl) :: vec2(mxddim+1)
       INTEGER      :: isort1(mxddim)
       INTEGER      :: isort2(mxddim)

       INTEGER :: npw 
       INTEGER :: mtxd1, mtxd2 
       INTEGER :: gmax1,gmax2,gmax
       INTEGER, ALLOCATABLE :: indx1(:),indx2(:)
       INTEGER :: i, j, ierr

!
!
 
! ...  Calculate gmax

       IF ( npw <= 0) CALL errore('dot_bloch','Invalid npw',ABS(npw)+1)
       IF ( npw >  npwx) CALL errore('dot_bloch','npw too large',npw)
       ALLOCATE( indx1(npw), indx2(npw), STAT=ierr )
          IF ( ierr/=0 ) CALL errore('dot_bloch','allocating indx1,indx2',2*npw)
       IF ( mxddim <= 0) CALL errore('dot_bloch','Invalid mxddim',ABS(mxddim)+1)
 
       gmax1 = isort1(1)
       DO i = 2, mtxd1
         IF ( isort1(i) >  gmax1 ) gmax1 = isort1(i)
       END DO

       gmax2 = isort2(1)
       DO i = 2, mtxd2
         IF ( isort2(i) >  gmax2 ) gmax2 = isort2(i)
       END DO             

       gmax = MAX( gmax1, gmax2 )
       DO j = 1, gmax
         indx1(j) = mxddim + 1
         indx2(j) = mxddim + 1
       END DO

       DO i = 1, mtxd1
         indx1( isort1(i) ) = i
       END DO

       DO i = 1, mtxd2
         indx2( isort2(i) ) = i
       END DO            
 
! ...  Calculate dotproduct
 
       dot_bloch = CMPLX( 0.0d0, 0.0d0 )
       DO j = 1, gmax
         dot_bloch = dot_bloch + CONJG( vec1( indx1(j) ) ) * vec2( indx2(j) )
       END DO
 
       RETURN
       END FUNCTION
