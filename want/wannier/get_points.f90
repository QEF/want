!
! Copyright (C) 2004 Arrigo Calzolari, Carlo Cavazzoni, Marco Buongiorno Nardelli
! Copyright (C) 2002 Nicola Marzari, Ivo Souza, David Vanderbilt
!
! This file is distributed under the terms of the
! GNU General Public License. See the file `License'
! in the root directory of the present distribution,
! or http://www.gnu.org/copyleft/gpl.txt .
!
!=----------------------------------------------------------------------------------------=
       SUBROUTINE get_points( nspts, npts, bvec, skpt, kpt, xval, sxval, tnkpts )
!=----------------------------------------------------------------------------------------=
!
!      Determines the k-points for calculating the band structure
!
       USE kinds
       USE constants, ONLY : ZERO
       USE io_module, ONLY : stdout

       IMPLICIT NONE

       REAL(dbl), PARAMETER :: eps = 1.d-6
 
       INTEGER :: nspts        ! Number of k-points generating the line (edges)
       INTEGER :: npts         ! maximum number of points in the line
       INTEGER :: tnkpts       ! actual number of point in the line
       REAL(dbl) :: bvec(3,3)
       REAL(dbl) :: skpt(3,npts)
       REAL(dbl) :: xval(npts)
       REAL(dbl) :: sxval(nspts)
       REAL(dbl) :: kpt(3,npts)
 
       INTEGER :: i, j, n
       INTEGER :: knum(nspts-1)
       REAL(dbl) :: length0, length(nspts-1)
       REAL(dbl) :: vec(3)
       REAL(dbl) :: bdot(3,3)

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
 

       tnkpts = 0
       length0 = 0
       DO i = 1, nspts - 1
         vec(1) = skpt(1,i+1) - skpt(1,i)
         vec(2) = skpt(2,i+1) - skpt(2,i)
         vec(3) = skpt(3,i+1) - skpt(3,i)
         length(i) = vec(1) *( bdot(1,1) * vec(1) +bdot(1,2) * vec(2) +bdot(1,3) * vec(3))+&
                     vec(2) *( bdot(2,1) * vec(1) +bdot(2,2) * vec(2) +bdot(2,3) * vec(3))+&
                     vec(3) *( bdot(3,1) * vec(1) +bdot(3,2) * vec(2) +bdot(3,3) * vec(3)) 
         length(i) = SQRT( length(i) )
         IF ( length(i) < eps ) CALL errore(' get_points ', ' length(i) too small ', i )

       ENDDO
       length0 = SUM(length(:))


       DO i = 1, nspts - 1
         n = INT( npts * length(i) / length0 )
         knum(i) = n
         IF ( n ==  0 ) CALL errore(' get_points ', ' nint=0  ', n )
 
         DO j = 1, n-1
           tnkpts = tnkpts + 1
           !
           ! ANDREA: changed MAXSPTS*MAXPTS to MAXPTS
           !
           IF ( tnkpts+1 > npts ) &
                CALL errore(' get_points ', ' tnkpts too large  ', tnkpts )

           IF ( tnkpts ==  1 ) THEN
             xval(tnkpts) = ZERO
           ELSE
             xval(tnkpts) = xval(tnkpts-1) + length(i)/DBLE(n)
           END IF

           IF ( j ==  1 ) sxval(i) = xval(tnkpts)

           kpt(1,tnkpts) = skpt(1,i) + ( skpt(1,i+1) - skpt(1,i) ) * DBLE(j-1) / DBLE(n)  
           kpt(2,tnkpts) = skpt(2,i) + ( skpt(2,i+1) - skpt(2,i) ) * DBLE(j-1) / DBLE(n)  
           kpt(3,tnkpts) = skpt(3,i) + ( skpt(3,i+1) - skpt(3,i) ) * DBLE(j-1) / DBLE(n)  
         END DO

       END DO
       
! ...  Last point
 
       tnkpts = tnkpts + 1
       xval(tnkpts) = xval(tnkpts-1) + length(nspts-1)/DBLE(n)
       kpt(1,tnkpts) = skpt(1,nspts)
       kpt(2,tnkpts) = skpt(2,nspts)
       kpt(3,tnkpts) = skpt(3,nspts)
       sxval(nspts) = xval(tnkpts) 

       WRITE(stdout, "(/,2x,'Generating kpts: ',i3,6x,'Segments: ',i3)") nspts, nspts-1
       WRITE(stdout, "(2x,'Total kpts number: ',i3,4x,'Max kpt number: ',i3)") &
                      tnkpts, npts

       WRITE(stdout, "(2/,2x,'Generating kpts [crystal coord.]')" )
       DO i=1,nspts
          WRITE(stdout, "(6x, 'k point', i4, ':   ( ',3f9.5, ' ) ') " ) i, skpt(:,i)
       ENDDO
       WRITE(stdout, "(/,2x,'Number of kpts in each segment')" )
       DO i=1,nspts-1
          WRITE(stdout, "(6x, 'line', i4, ':   ',i5 ) ') " ) i, knum(i)
       ENDDO
       WRITE(stdout, "(2/,2x,'Generated kpts [crystal coord.]')" )
       DO i=1,tnkpts
          WRITE(stdout, "(6x, 'k point', i4, ':   ( ',3f9.5, ' ) ') " ) i, kpt(:,i)
       ENDDO

       RETURN
       END SUBROUTINE
                    
