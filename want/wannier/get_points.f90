!
! Copyright (C) 2004 Arrigo Calzolari, Carlo Cavazzoni, Marco Buongiorno Nardelli
! Copyright (C) 2002 Nicola Marzari, Ivo Souza, David Vanderbilt
!
! This file is distributed under the terms of the
! GNU General Public License. See the file `License'
! in the root directory of the present distribution,
! or http://www.gnu.org/copyleft/gpl.txt .
!
!=------------------------------------------------------------------------------------------------=
       SUBROUTINE get_points( maxspts, maxpts, nspts, npts, bvec, skpt, kpt, xval, sxval, tnkpts )
!=------------------------------------------------------------------------------------------------=
!
!      Determines the k-points for calculating the band structure
!
       USE kinds

       IMPLICIT NONE
 
       INTEGER :: maxspts, maxpts
       INTEGER :: nspts, npts, tnkpts
       REAL(dbl) :: bvec(3,3)
       REAL(dbl) :: skpt(3,maxpts)
       REAL(dbl) :: xval(maxspts*maxpts)
       REAL(dbl) :: sxval(maxspts)
       REAL(dbl) :: kpt(3,maxspts*maxpts)
 
       INTEGER :: i, j, n
       REAL(dbl) :: length0, length
       REAL(dbl) :: vec(3)
       REAL(dbl) :: eps
       REAL(dbl) :: bdot(3,3)
       PARAMETER( eps = 1.e-6 )

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
 
       vec(1) = skpt(1,2) - skpt(1,1)
       vec(2) = skpt(2,2) - skpt(2,1)
       vec(3) = skpt(3,2) - skpt(3,1)
       length0 = vec(1) * ( bdot(1,1) * vec(1) + bdot(1,2) * vec(2) + bdot(1,3) * vec(3) ) +   &
                 vec(2) * ( bdot(2,1) * vec(1) + bdot(2,2) * vec(2) + bdot(2,3) * vec(3) ) +   &
                 vec(3) * ( bdot(3,1) * vec(1) + bdot(3,2) * vec(2) + bdot(3,3) * vec(3) ) 
       length0 = SQRT( length0 )

       IF ( length0 < eps ) CALL errore(' get_points ', ' length0 too small ', length0 )
 
       tnkpts = 0
       DO i = 1, nspts - 1
         vec(1) = skpt(1,i+1) - skpt(1,i)
         vec(2) = skpt(2,i+1) - skpt(2,i)
         vec(3) = skpt(3,i+1) - skpt(3,i)
         length = vec(1) * ( bdot(1,1) * vec(1) + bdot(1,2) * vec(2) + bdot(1,3) * vec(3) ) +  &
                  vec(2) * ( bdot(2,1) * vec(1) + bdot(2,2) * vec(2) + bdot(2,3) * vec(3) ) +  &
                  vec(3) * ( bdot(3,1) * vec(1) + bdot(3,2) * vec(2) + bdot(3,3) * vec(3) ) 
         length = SQRT( length )

         IF ( length0 < eps ) CALL errore(' get_points ', ' length too small ', length0 )
 
         n = nint( DBLE(npts) * length / length0 )
         IF ( n ==  0 ) CALL errore(' get_points ', ' nint=0  ', n )
 
         DO j = 1, n
           tnkpts = tnkpts + 1
           IF ( tnkpts+1 > maxspts*maxpts ) CALL errore(' get_points ', ' tnkpts too large  ', tnkpts )

           IF ( tnkpts ==  1 ) THEN
             xval(tnkpts) = 0.d0
           ELSE
             xval(tnkpts) = xval(tnkpts-1) + length/DBLE(n)
           END IF

           IF ( j ==  1 ) sxval(i) = xval(tnkpts)

           kpt(1,tnkpts) = skpt(1,i) + ( skpt(1,i+1) - skpt(1,i) ) * DBLE(j-1) / DBLE(n)  
           kpt(2,tnkpts) = skpt(2,i) + ( skpt(2,i+1) - skpt(2,i) ) * DBLE(j-1) / DBLE(n)  
           kpt(3,tnkpts) = skpt(3,i) + ( skpt(3,i+1) - skpt(3,i) ) * DBLE(j-1) / DBLE(n)  
         END DO

       END DO
       
! ...  Last point
 
       tnkpts = tnkpts + 1
       xval(tnkpts) = xval(tnkpts-1) + length/DBLE(n)
       kpt(1,tnkpts) = skpt(1,nspts)
       kpt(2,tnkpts) = skpt(2,nspts)
       kpt(3,tnkpts) = skpt(3,nspts)
       sxval(nspts) = xval(tnkpts) 
       
       RETURN
       END SUBROUTINE
                    
