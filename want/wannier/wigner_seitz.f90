!
! Copyright (C) 2004 WanT Group
! Copyright (C) 2002 Nicola Marzari, Ivo Souza, David Vanderbilt
!
! This file is distributed under the terms of the
! GNU General Public License. See the file `License'
! in the root directory of the present distribution,
! or http://www.gnu.org/copyleft/gpl.txt .
!
!********************************************************************
   SUBROUTINE wigner_seitz( avec, nk, indxws, nws, degen )
   !********************************************************************
   USE kinds
   USE io_module, ONLY : stdout
   USE constants, ONLY : ONE, ZERO, EPS_m8

   IMPLICIT NONE
   !
   ! input/output variables
   !
   REAL(dbl), INTENT(in) :: avec(3,3)
   INTEGER,   INTENT(in) :: nk(3)
   INTEGER,   INTENT(out):: nws
   INTEGER,   INTENT(out):: indxws(3,*)
   INTEGER,   INTENT(out):: degen(*)

   !
   ! local variables
   !
   INTEGER :: n1, n2, n3
   INTEGER :: icnt, i, j, i1, i2, i3
   INTEGER :: ndiff(3), indx(27), ifnd
   INTEGER :: nn, ndeg
   REAL(dbl) :: dist(27), dist_min
   REAL(dbl) :: tot
   REAL(dbl) :: adot(3,3)


!
!------------------------------
! main body
!------------------------------
!
       nws = 0

       !
       ! Compute metric in real space
       !
       adot(1,1) = avec(1,1) * avec(1,1) + avec(2,1) * avec(2,1) + avec(3,1) * avec(3,1)
       adot(2,2) = avec(1,2) * avec(1,2) + avec(2,2) * avec(2,2) + avec(3,2) * avec(3,2)
       adot(3,3) = avec(1,3) * avec(1,3) + avec(2,3) * avec(2,3) + avec(3,3) * avec(3,3)
       adot(1,2) = avec(1,1) * avec(1,2) + avec(2,1) * avec(2,2) + avec(3,1) * avec(3,2)
       adot(1,3) = avec(1,1) * avec(1,3) + avec(2,1) * avec(2,3) + avec(3,1) * avec(3,3)
       adot(2,3) = avec(1,2) * avec(1,3) + avec(2,2) * avec(2,3) + avec(3,2) * avec(3,3)
       adot(2,1) = adot(1,2)
       adot(3,1) = adot(1,3)
       adot(3,2) = adot(2,3)


       !
       ! Loop over grid points r on a unit cell that is 8 times larger than a 
       ! primitive supercell. In the end nws contains the total number of grids 
       ! points that have been found in the Wigner-Seitz cell
       !
       nws = 0
       DO n1 = 0, 2 * nk(1)
         DO n2 = 0, 2 * nk(2)
           DO n3 = 0, 2 * nk(3)
             !
             ! Loop over the 27 points R. R=0 corresponds to i1=i2=i3=1, or icnt=14
             !
             icnt = 0
             DO i1 = 0, 2
               DO i2 = 0, 2
                 DO i3 = 0, 2
                   icnt = icnt + 1
                   !
                   !  Calculate distance |r-R| 
                   ! 
                   ndiff(1) = n1 - i1 * nk(1)
                   ndiff(2) = n2 - i2 * nk(2)
                   ndiff(3) = n3 - i3 * nk(3)
                   dist(icnt) = ZERO

                   DO i = 1, 3
                     DO j = 1, 3
                       dist(icnt) = dist(icnt) + ndiff(i) * adot(i,j) * ndiff(j)
                     ENDDO
                   ENDDO
           

                 ENDDO
               ENDDO
             ENDDO

             !
             ! Sort the 27 vectors R by increasing value of |r-R| (from Numerical Recipes)
             !
             CALL indexx(27,dist,indx)

             !
             ! Find all the vectors R with the (same) smallest |r-R|;
             ! if R=0 is one of them, then the current point r belongs to 
             ! Wignez-Seitz cell => set ifnd to 1
             !

             !
             ! Jeremia's axe (MBN-AC)
             !
             dist_min = dist(indx(1))
             IF ( ABS( dist(14) - dist_min ) < EPS_m8 ) THEN
               nws = nws + 1

               ndeg = 0
               DO nn = 1,27
                 IF( ABS( dist(nn) - dist_min) < EPS_m8 ) ndeg = ndeg + 1
               END DO
               degen(nws) = ndeg

               indxws(1,nws) = n1 - nk(1)
               indxws(2,nws) = n2 - nk(2)
               indxws(3,nws) = n3 - nk(3)
             END IF

           END DO !n3
         END DO !n2
       END DO !n1

       !
       ! Check the "sum rule"
       !
       tot = ZERO
       DO i = 1, nws
         tot = tot + ONE / DBLE(degen(i))
       END DO

       IF( ABS( tot - DBLE( nk(1) * nk(2) * nk(3) ) ) > EPS_m8 ) &
           CALL errore('wigner_size', 'wrong total number of points ', NINT(tot) )

    RETURN
    END SUBROUTINE wigner_seitz

