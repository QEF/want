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
       SUBROUTINE get_points( nkpts_in, nkpts_max, bvec, kpt_in, xval_in, point, kpt, xval, nkpts_tot )
!=----------------------------------------------------------------------------------------=
!
!      Determines the k-points for calculating the band structure
!
       USE kinds
       USE constants, ONLY : ZERO, EPS_m6
       USE io_module, ONLY : stdout

       IMPLICIT NONE

       INTEGER,      INTENT(in)  :: nkpts_in   ! Number of k-points generating the line (edges)
       INTEGER,      INTENT(in)  :: nkpts_max  ! maximum number of interpolated point
       INTEGER,      INTENT(out) :: nkpts_tot ! actual number of point in the line
       REAL(dbl),    INTENT(in)  :: bvec(3,3)
       REAL(dbl),    INTENT(in)  :: kpt_in(3,nkpts_in)
       REAL(dbl),    INTENT(out) :: kpt(3,nkpts_max)
       REAL(dbl),    INTENT(out) :: xval(nkpts_max)
       REAL(dbl),    INTENT(out) :: xval_in(nkpts_in)
       CHARACTER(2), INTENT(in)  :: point(nkpts_in)
 
       INTEGER   :: i, j, n
       INTEGER   :: knum(nkpts_in-1)
       REAL(dbl) :: length0, length(nkpts_in-1)
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
 

       nkpts_tot = 0
       length0 = 0
       DO i = 1, nkpts_in - 1
         vec(1) = kpt_in(1,i+1) - kpt_in(1,i)
         vec(2) = kpt_in(2,i+1) - kpt_in(2,i)
         vec(3) = kpt_in(3,i+1) - kpt_in(3,i)
         length(i) = vec(1) *( bdot(1,1) * vec(1) +bdot(1,2) * vec(2) +bdot(1,3) * vec(3))+&
                     vec(2) *( bdot(2,1) * vec(1) +bdot(2,2) * vec(2) +bdot(2,3) * vec(3))+&
                     vec(3) *( bdot(3,1) * vec(1) +bdot(3,2) * vec(2) +bdot(3,3) * vec(3)) 
         length(i) = SQRT( length(i) )
         IF ( length(i) < EPS_m6 ) CALL errore(' get_points ', ' length(i) too small ', i )

       ENDDO
       length0 = SUM(length(:))


       DO i = 1, nkpts_in - 1
         n = INT( nkpts_max * length(i) / length0 )
         knum(i) = n
         IF ( n ==  0 ) CALL errore(' get_points ', ' nint=0  ', n )
 
         DO j = 1, n-1
           nkpts_tot = nkpts_tot + 1
           !
           ! ANDREA: changed MAXSPTS*MAXPTS to MAXPTS
           !
           IF ( nkpts_tot+1 > nkpts_max ) &
                CALL errore(' get_points ', ' nkpts_tot too large  ', nkpts_tot )

           IF ( nkpts_tot ==  1 ) THEN
             xval(nkpts_tot) = ZERO
           ELSE
             xval(nkpts_tot) = xval(nkpts_tot-1) + length(i)/DBLE(n)
           END IF

           IF ( j ==  1 ) xval_in(i) = xval(nkpts_tot)

           kpt(1,nkpts_tot) = kpt_in(1,i) + ( kpt_in(1,i+1) - kpt_in(1,i) ) * DBLE(j-1) / DBLE(n)  
           kpt(2,nkpts_tot) = kpt_in(2,i) + ( kpt_in(2,i+1) - kpt_in(2,i) ) * DBLE(j-1) / DBLE(n)  
           kpt(3,nkpts_tot) = kpt_in(3,i) + ( kpt_in(3,i+1) - kpt_in(3,i) ) * DBLE(j-1) / DBLE(n)  
         END DO

       END DO
       
! ...  Last point
 
       nkpts_tot = nkpts_tot + 1
       xval(nkpts_tot)   = xval(nkpts_tot-1) + length(nkpts_in-1)/DBLE(n)
       kpt(1,nkpts_tot)  = kpt_in(1,nkpts_in)
       kpt(2,nkpts_tot)  = kpt_in(2,nkpts_in)
       kpt(3,nkpts_tot)  = kpt_in(3,nkpts_in)
       xval_in(nkpts_in) = xval(nkpts_tot) 

       WRITE(stdout, "(/,2x,'Generating kpts: ',i3,6x,'Segments: ',i3)") nkpts_in, nkpts_in-1
       WRITE(stdout, "(2x,'Total kpts number: ',i3,4x,'Max kpt number: ',i3)") &
                      nkpts_tot, nkpts_max

       WRITE(stdout, "(2/,2x,'Generating kpts [crystal coord.]')" )
       DO i=1,nkpts_in
           WRITE(stdout, "(6x, 'k point', i4, ':   ( ',3f9.5, ' ) ',3x,a2) ") &
                           i, kpt_in(:,i), point(i)
       ENDDO
       WRITE(stdout, "(/,2x,'Number of kpts in each segment')" )
       DO i=1,nkpts_in-1
          WRITE(stdout, "(6x, 'line', i4, ':   ',i5 ) ') " ) i, knum(i)
       ENDDO
       WRITE(stdout, "(2/,2x,'Generated kpts [crystal coord.]')" )
       DO i=1,nkpts_tot
          WRITE(stdout, "(6x, 'k point', i4, ':   ( ',3f9.5, ' ) ') " ) i, kpt(:,i)
       ENDDO

       RETURN
       END SUBROUTINE
                    
