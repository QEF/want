       SUBROUTINE wigner_seitz( adot, nk, indxws, nws, degen, mxdnrk )

!..............................................................................
! Calculates a grid of points that fall inside of (and eventually on the 
! surface of) the Wigner-Seitz supercell centered on the origin of the Bravais
! lattice with primitive translations nk(1)*a_1+nk(2)*a_2+nk(3)*a_3
! INPUT:
! ADOT(I,J)        Real space metric
! NK(I)            Number of points along the direction of the i-th lattice
!                  vector
! MXDNRK
! OUTPUT:

! INDXWS(I,IWS)    The iws-th Wigner-Seitz grid point has components
!                  indxws(1:3,iws) in the basis of the lattice vectors 
! DEGEN(IWS)       Weight of the iws-th point is 1/degen(iws)
!
! Written 15 Feb 2001 by Ivo Souza
! Modified April 18,2002 by MBN and AC (space invaders!)
!..............................................................................

     
       USE kinds
       USE io_global, ONLY : stdout

       IMPLICIT NONE

       REAL(dbl) :: adot(3,3)
       INTEGER :: nk(3)
       INTEGER :: mxdnrk

       INTEGER :: indxws(3,3*mxdnrk)
       INTEGER :: degen(3*mxdnrk)

       INTEGER :: nws, n1, n2, n3
       INTEGER :: icnt, i, j, i1, i2, i3
       INTEGER :: ndiff(3), indx(27), ifnd
       INTEGER :: nn, ndeg
       REAL(dbl) :: dist(27), dist_min
       REAL(dbl) :: tot

! ...  Loop over grid points r on a unit cell that is 8 times larger than a 
!      primitive supercell. In the end nws contains the total number of grids 
!      points that have been found in the Wigner-Seitz cell

       nws = 0
       DO n1 = 0, 2 * nk(1)
         DO n2 = 0, 2 * nk(2)
           DO n3 = 0, 2 * nk(3)

             WRITE(stdout, *)  ' '
             WRITE(stdout, 321) 'r point: ', n1, n2, n3
 321         FORMAT( a9, 3(i2,1x) )

! ...        Loop over the 27 points R. R=0 corresponds to i1=i2=i3=1, or icnt=14

             ICNT = 0
             DO i1 = 0, 2
               DO i2 = 0, 2
                 DO i3 = 0, 2
                   icnt = icnt + 1

! ...              Calculate distance |r-R| 
         
                   ndiff(1) = n1 - i1 * nk(1)
                   ndiff(2) = n2 - i2 * nk(2)
                   ndiff(3) = n3 - i3 * nk(3)
                   dist(icnt) = 0.0d0

                   DO i = 1, 3
                     DO j = 1, 3
                       dist(icnt) = dist(icnt) + ndiff(i) * adot(i,j) * ndiff(j)
                     END DO
                   END DO
           
                   WRITE(stdout, 123) 'icnt=', icnt, ' dist=', dist(icnt)
 123               FORMAT(a5, i3, a6, f14.7)

                 END DO
               END DO
             END DO

! ...        Sort the 27 vectors R by increasing value of |r-R| (from Numerical Recipes)

             CALL INDEXX(27,DIST,INDX)

! ...        Find all the vectors R with the (same) smallest |r-R|;
!            if R=0 is one of them, then the current point r belongs to 
!            Wignez-Seitz cell => set ifnd to 1

! ...        Jeremia's axe (MBN-AC)

             dist_min = dist(indx(1))
             IF ( ABS( dist(14) - dist_min ) < 1.e-8 ) THEN
               nws = nws + 1

               IF ( nws > 3*mxdnrk ) CALL errore(' wigner_size ', ' wrong dimension ', nws )

               ndeg = 0
               DO nn = 1,27
                 IF( ABS( dist(nn) - dist_min) < 1.e-8 ) ndeg = ndeg + 1
               END DO
               degen(nws) = ndeg

               indxws(1,nws) = n1 - nk(1)
               indxws(2,nws) = n2 - nk(2)
               indxws(3,nws) = n3 - nk(3)
               WRITE( stdout,*)  'This point is in!'
               WRITE( stdout,*) 'degeneracy:',nws,degen(nws)
             END IF

           END DO !n3
         END DO !n2
       END DO !n1

!      Check the "sum rule"

       tot = 0.0d0
       DO i = 1, nws
         tot = tot + 1.0d0 / DBLE(degen(i))
         WRITE( stdout,*) 'i=', i, ' degen(i)=', degen(i)
       END DO

       IF( ABS( tot - DBLE( nk(1) * nk(2) * nk(3) ) ) > 1.0e-8 ) THEN
         WRITE( stdout, *) '*** ERROR *** in finding Wigner-Seitz points'
         WRITE( stdout, *) 'TOT=', tot
         WRITE( stdout, *) 'NK(1)*NK(2)*NK(3)=', nk(1)*nk(2)*nk(3)
         CALL errore(' wigner_size ', ' wrong total number of points ', tot )
       ELSE
         WRITE( stdout, *) '*** SUCCESS!!! ***'
         WRITE( stdout, *) 'TOT=', tot
         WRITE( stdout, *) 'NK(1)*NK(2)*NK(3)=', nk(1)*nk(2)*nk(3)
       END IF

       RETURN
       END SUBROUTINE
