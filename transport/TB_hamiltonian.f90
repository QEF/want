!
!      Copyright (C) 2004 Arrigo Calzolari, Marco Buongiorno Nardelli
!      Copyright (C) 1999 Marco Buongiorno Nardelli
!
!      This file is distributed under the terms of the
!      GNU General Public License. See the file `License'
!      in the root directory of the present distribution,
!      or http://www.gnu.org/copyleft/gpl.txt .
!
!=--------------------------------------------------------------------=
      SUBROUTINE TB_hamiltonian( r, natmax, rcut, h0, h1, nmx, nmax,    &
                             gamma0, model, efieldx, efieldy, efieldz )
!=--------------------------------------------------------------------=

      USE kinds

      IMPLICIT NONE

      INTEGER :: i,j,k,l,m,n,model,nw
      INTEGER :: natmax,nmx,nmax

      REAL(dbl) :: r(3,natmax), tbpar(6)
      REAL(dbl) :: h0(nmax,nmax), h1(nmax,nmax)

      REAL(dbl) :: gamma0, rcut
      REAL(dbl) :: efieldx, efieldy, efieldz
      REAL(dbl) :: x, y, z
      REAL(dbl) :: rr, apu, d2, d2q, rsq
      REAL(dbl) :: smooth

      EXTERNAL smooth, scale

      IF( model == 1 ) GO TO 10
      IF( model == 2 ) GO TO 20
      IF( model == 3 ) GO TO 30
      IF( model == 4 ) GO TO 40

!=--------------------------------------------------------------------=
 
 10   CONTINUE

!...  Pi orbital model TB

      IF ( nmx /= nmax ) CALL errore(' TB_hamiltonian ', ' ERROR in nmx - nmax ', nmx )

      apu = 1.0

!...  Generate on-site hamiltonian h0

      DO i = 1, nmx 
         DO j = 1, nmx
            d2 = ( r(1,j) - r(1,i) )**2 + ( r(2,j) - r(2,i) )**2 + ( r(3,j) - r(3,i) )**2
            d2q = SQRT( d2 )

            IF ( d2q < rcut .AND. d2q > 1.d-7 ) THEN
!              apu = smooth( d2q )
               h0(i,j) = -gamma0 * apu

!...        Patch for 2nd n.n. interactions
!           ELSE IF ( d2q > rcut .AND. d2q < 2 * rcut ) THEN
!              apu = smooth( d2q )
!              h0(i,j) = -gamma0 * apu

            ELSE
               h0(i,j) = 0.d0
            END IF

         END DO 
      END DO 

      DO i = 1, nmax
         h0(i,i) = 0.d0
      END DO

!...  Generate hopping hamiltonian h1

      DO i = 1, nmx 
         DO j = nmx+1, natmax
            d2 = ( r(1,j) - r(1,i) )**2 + ( r(2,j) - r(2,i) )**2+ ( r(3,j)-r(3,i) )**2
            d2q=sqrt( d2 )

            IF ( d2q < rcut .AND. d2q > 1.d-7 ) THEN
!              apu = smooth( d2q )
               h1( i, j-nmx ) = -gamma0 * apu

!...        Patch for 2nd n.n. interactions
!           ELSE IF ( d2q > rcut .AND. d2q < 2*rcut ) THEN
!              apu = smooth( d2q )
!              h1( i, j-nmx ) = -gamma0 * apu

            ELSE
               h1( i ,j-nmx ) = 0.d0
            END IF

         END DO 
      END DO 

!...  Check: write hamiltonian h0 and h1
!     DO i = 1, nmax
!        WRITE( *, '(12(f6.2))' ) ( h0(i,j), j = 1, nmax )
!     END DO
!     PRINT*,'=============================================='

!     DO i = 1, nmax
!        WRITE( *, '(12(f6.2))' ) ( h1(i,j), j = 1, nmax )
!     END DO
!     PRINT*,'=============================================='

      RETURN
!............................................................



20    CONTINUE

!...  Tight-binding matrix elements from
!     Slater and Koster  Phys. Rev.  94, 1498 (1954).
!
!     H(i,j) = sum_R <f(i,0) | H | f(j,R)> e^(ik.R)
!
!...  TB parametrization from:
!     Xu, Wang, Chan, and Ho, J. Phys.: Condens. Matter 4 6047 (1992).

      tbpar(1) = -2.99
      tbpar(2) = 3.71
      tbpar(3) = -5.0
      tbpar(4) = 4.7
      tbpar(5) = 5.5
      tbpar(6) = -1.55

!...  Dimensions of H are (numb.of atoms)*(numb.of basis)
!     for C =  4*(numb.of atoms) because minimal basis (s,px,py,pz) is used

!...  Generate on-site hamiltonian h0:

      h0(:,:) = 0.0d0

!...  Diagonal elements Es and Ep

      DO i = 0, nmx-1
         h0( i*4+1, i*4 +1 ) = tbpar( 1 ) + efieldx * r( 1, i+1 ) +          &
                               efieldy * r( 2, i+1 ) + efieldz * r( 3, i+1 )
         h0( i*4+2, i*4 +2 ) = tbpar( 2 ) + efieldx * r( 1, i+1 ) +          &
                               efieldy * r( 2, i+1 ) + efieldz * r( 3, i+1 )
         h0( i*4+3, i*4 +3 ) = tbpar( 2 ) + efieldx * r( 1, i+1 ) +          &
                               efieldy * r( 2, i+1 ) + efieldz * r( 3, i+1 )
         h0( i*4+4, i*4 +4 ) = tbpar( 2 ) + efieldx * r( 1, i+1 ) +          &
                               efieldy * r( 2, i+1 ) + efieldz * r( 3, i+1 )
      END DO

      DO i = 0, nmx-1
         DO j = 0, nmx-1
            x  = r(1,i+1) - r(1,j+1)
            y  = r(2,i+1) - r(2,j+1)
            z  = r(3,i+1) - r(3,j+1)
            rsq = x**2 + y**2 + z**2
            rr  = SQRT( rsq )

            IF ( rr < 2.0*rcut .AND. rr > 1.e-7 ) THEN
!...           Divide by r**2 done in function smooth 
               apu = smooth(rr)
               x = x/rr
               y = y/rr
               z = z/rr

               h0( j*4+1, i*4+1 ) = apu * tbpar(3)
               h0( j*4+2, i*4+1 ) = apu * tbpar(4) * x
               h0( j*4+3, i*4+1 ) = apu * tbpar(4) * y
               h0( j*4+4, i*4+1 ) = apu * tbpar(4) * z
               h0( j*4+2, i*4+2 ) = apu * ( tbpar(5) * (x**2) + tbpar(6) * ( 1 - (x**2) ) )
               h0( j*4+3, i*4+3 ) = apu * ( tbpar(5) * (y**2) + tbpar(6) * ( 1 - (y**2) ) )
               h0( j*4+4, i*4+4 ) = apu * ( tbpar(5) * (z**2) + tbpar(6) * ( 1 - (z**2) ) )

               h0( j*4+3, i*4+2 ) = apu * ( tbpar(5) - tbpar(6) )* x* y
               h0( j*4+4, i*4+2 ) = apu * ( tbpar(5) - tbpar(6) )* x* z
               h0( j*4+4, i*4+3 ) = apu * ( tbpar(5) - tbpar(6) )* y* z

               h0( j*4+1, i*4+2 ) = -h0( j*4+2, i*4+1 )
               h0( j*4+1, i*4+3 ) = -h0( j*4+3, i*4+1 )
               h0( j*4+1, i*4+4 ) = -h0( j*4+4, i*4+1 )

               h0( j*4+2, i*4+3 ) =  h0( j*4+3, i*4+2 )
               h0( j*4+2, i*4+4 ) =  h0( j*4+4, i*4+2 )
               h0( j*4+3, i*4+4 ) =  h0( j*4+4, i*4+3 )

            END If

         END DO
      END DO

!...  Generate hopping hamiltonian h1 

      h1(:,:) = 0.0d0

!...  Diagonal elements of h1 are = zero

      DO i = 0, nmx-1
         DO k = nmx+1, natmax
            x  = r( 1, i+nmx+1 ) - r( 1, k-nmx )
            y  = r( 2, i+nmx+1 ) - r( 2, k-nmx )
            z  = r( 3, i+nmx+1 ) - r( 3, k-nmx )
            rsq = x**2+y**2+z**2
            rr  = SQRT(rsq)
            j = k - nmx - 1

            IF ( rr < 2.0*rcut .AND. rr > 1.e-7 ) THEN
!...           Divide by r**2 done in function smooth 
               apu = smooth(rr)
               x = x / rr
               y = y / rr
               z = z / rr

               h1( j*4+1, i*4+1 ) = apu * tbpar(3)
               h1( j*4+2, i*4+1 ) = apu * tbpar(4) * x
               h1( j*4+3, i*4+1 ) = apu * tbpar(4) * y
               h1( j*4+4, i*4+1 ) = apu * tbpar(4) * z
               h1( j*4+2, i*4+2 ) = apu * ( tbpar(5) * ( x**2 ) + tbpar(6) * ( 1 - ( x**2 ) ) )
               h1( j*4+3, i*4+3 ) = apu * ( tbpar(5) * ( y**2 ) + tbpar(6) * ( 1 - ( y**2 ) ) )
               h1( j*4+4, i*4+4 ) = apu * ( tbpar(5) * ( z**2 ) + tbpar(6) * ( 1 - ( z**2 ) ) )

               h1( j*4+3, i*4+2 ) = apu * ( tbpar(5) - tbpar(6) ) * x * y
               h1( j*4+4, i*4+2 ) = apu * ( tbpar(5) - tbpar(6) ) * x * z
               h1( j*4+4, i*4+3 ) = apu * ( tbpar(5) - tbpar(6) ) * y * z
 
               h1( j*4+1, i*4+2 ) = -h1( j*4+2, i*4+1 )
               h1( j*4+1, i*4+3 ) = -h1( j*4+3, i*4+1 )
               h1( j*4+1, i*4+4 ) = -h1( j*4+4, i*4+1 )

               h1( j*4+2, i*4+3 ) =  h1( j*4+3, i*4+2 )
               h1( j*4+2, i*4+4 ) =  h1( j*4+4, i*4+2 )
               h1( j*4+3, i*4+4 ) =  h1( j*4+4, i*4+3 )

            END IF

         END DO
      END DO

!...  Check: write hamiltonian h0 and h1
!     DO i = 1, nmax
!        WRITE ( *, '(16(f6.2))' ) ( h0(i,j), j = 1, nmax )
!     END DO
!     PRINT*,'=============================================='

!     DO i = 1, nmax
!        WRITE ( *, '(16(f6.2))' ) ( h1(i,j), j = 1, nmax )
!     END DO
!     PRINT*,'=============================================='
!     STOP

      RETURN
!............................................................



30    CONTINUE

!...  Tight-binding matrix elements from
!     Slater and Koster  Phys. Rev.  94, 1498 (1954).
!
!...  TB parametrization from:
!     Charlier, Lambin and Ebbesen, Phys. Rev. B, 54, R8377 (1997).
!
!...  Dimensions of H are (numb.of atoms)*(numb.of basis)
!     for C =  4*(numb.of atoms) because minimal basis (s,px,py,pz) is used

      tbpar(1) = -7.30
      tbpar(2) = 0.00

!...  Generate on-site hamiltonian h0 

      h0(:,:) = 0.0d0

!...  Diagonal elements Es and Ep
      DO i = 0, nmx-1
         h0( i*4+1, i*4+1 ) = tbpar(1) + efieldx * r(1,i+1) + efieldy * r(2,i+1) + efieldz * r(3,i+1)
         h0( i*4+2, i*4+2 ) = tbpar(2) + efieldx * r(1,i+1) + efieldy * r(2,i+1) + efieldz * r(3,i+1)
         h0( i*4+3, i*4+3 ) = tbpar(2) + efieldx * r(1,i+1) + efieldy * r(2,i+1) + efieldz * r(3,i+1)
         h0( i*4+4, i*4+4 ) = tbpar(2) + efieldx * r(1,i+1) + efieldy * r(2,i+1) + efieldz * r(3,i+1)
      END DO

      DO i = 0, nmx-1
         DO j = 0, nmx-1
            x  = r(1,i+1) - r(1,j+1)
            y  = r(2,i+1) - r(2,j+1)
            z  = r(3,i+1) - r(3,j+1)
            rsq = x**2+y**2+z**2
            rr  = SQRT( rsq )
            IF ( rr < 2.0*rcut .AND. rr > 1.e-7 ) THEN

! ...       Divide by r**2 done in function smooth 
               CALL scale( rr, apu, tbpar )
               x = x / rr
               y = y / rr
               z = z / rr

               h0( j*4+1, i*4+1 ) = apu * tbpar(3)
               h0( j*4+2, i*4+1 ) = apu * tbpar(4) * x
               h0( j*4+3, i*4+1 ) = apu * tbpar(4) * y
               h0( j*4+4, i*4+1 ) = apu * tbpar(4) * z
               h0( j*4+2, i*4+2 ) = apu * ( tbpar(5) * ( x**2 ) + tbpar(6) * ( 1- ( x**2 ) ) )
               h0( j*4+3, i*4+3 ) = apu * ( tbpar(5) * ( y**2 ) + tbpar(6) * ( 1- ( y**2 ) ) )
               h0( j*4+4, i*4+4 ) = apu * ( tbpar(5) * ( z**2 ) + tbpar(6) * ( 1- ( z**2 ) ) )

               h0( j*4+3, i*4+2 ) = apu * ( tbpar(5) - tbpar(6) ) * x * y
               h0( j*4+4, i*4+2 ) = apu * ( tbpar(5) - tbpar(6) ) * x * z
               h0( j*4+4, i*4+3 ) = apu * ( tbpar(5) - tbpar(6) ) * y * z

               h0( j*4+1, i*4+2 ) = -h0( j*4+2, i*4+1 )
               h0( j*4+1, i*4+3 ) = -h0( j*4+3, i*4+1 )
               h0( j*4+1, i*4+4 ) = -h0( j*4+4, i*4+1 )

               h0( j*4+2, i*4+3 ) =  h0( j*4+3, i*4+2 )
               h0( j*4+2, i*4+4 ) =  h0( j*4+4, i*4+2 )
               h0( j*4+3, i*4+4 ) =  h0( j*4+4, i*4+3 )

            END IF

         END DO
      END DO

!...  Generate hopping hamiltonian h1 

      h1(:,:) = 0.0d0

!...  Diagonal elements of h1 are = zero
      Do i = 0, nmx-1
         DO k = nmx+1, natmax
            x  = r( 1, i+nmx+1 ) -r( 1, k-nmx )
            y  = r( 2, i+nmx+1 ) -r( 2, k-nmx )
            z  = r( 3, i+nmx+1 ) -r( 3, k-nmx )
            rsq = x**2 + y**2 + z**2
            rr  = SQRT( rsq )
            j = k - nmx - 1
            IF ( rr < 2.0*rcut .AND. rr > 1.e-7 ) THEN
!...        Divide by r**2 done in function smooth 
               CALL scale( rr, apu, tbpar )
               x = x / rr
               y = y / rr
               z = z / rr

               h1( j*4+1, i*4+1 ) = apu * tbpar(3)
               h1( j*4+2, i*4+1 ) = apu * tbpar(4) * x
               h1( j*4+3, i*4+1 ) = apu * tbpar(4) * y
               h1( j*4+4, i*4+1 ) = apu * tbpar(4) * z
               h1( j*4+2, i*4+2 ) = apu * ( tbpar(5) * ( x**2 ) + tbpar(6) * ( 1- ( x**2 ) ) )
               h1( j*4+3, i*4+3 ) = apu * ( tbpar(5) * ( y**2 ) + tbpar(6) * ( 1- ( y**2 ) ) )
               h1( j*4+4, i*4+4 ) = apu * ( tbpar(5) * ( z**2 ) + tbpar(6) * ( 1- ( z**2 ) ) )

               h1( j*4+3, i*4+2 ) = apu * ( tbpar(5) - tbpar(6) ) * x * y
               h1( j*4+4, i*4+2 ) = apu * ( tbpar(5) - tbpar(6) ) * x * z
               h1( j*4+4, i*4+3 ) = apu * ( tbpar(5) - tbpar(6) ) * y * z
 
               h1( j*4+1, i*4+2 ) = -h1( j*4+2, i*4+1 )
               h1( j*4+1, i*4+3 ) = -h1( j*4+3, i*4+1 )
               h1( j*4+1, i*4+4 ) = -h1( j*4+4, i*4+1 )

               h1( j*4+2, i*4+3 ) =  h1( j*4+3, i*4+2 )
               h1( j*4+2, i*4+4 ) =  h1( j*4+4, i*4+2 )
               h1( j*4+3, i*4+4 ) =  h1( j*4+4, i*4+3 )

            END IF

         END DO
      END DO

!...  Check: write hamiltonian h0 and h1
!     DO i = 1, nmax
!        WRITE ( *, '(16(f6.2))' ) ( h0(i,j), j = 1, nmax )
!     END DO
!     PRINT*,'=============================================='

!     DO i = 1, nmax
!        WRITE ( *, '(16(f6.2))' ) ( h1(i,j), j = 1, nmax )
!     END DO
!     PRINT*,'=============================================='
!     STOP

      RETURN
!............................................................



 40   CONTINUE

!...  Read from external file

      OPEN ( 30, FILE="H00.dat", STATUS='OLD' )
      REWIND 30
      READ ( 30, * ) nw
      IF ( nw /= nmax  )  CALL errore(' TB_hamiltonian ', ' wrong nmax in reading (I)', nw )

      DO j = 1, nmax
         READ( 30, * ) 
         DO i = 1, nmax
            READ( 30, * ) h0(i,j)
         END DO
      END DO
      CLOSE ( 30 )

      OPEN ( 40, FILE="H01.dat", STATUS='OLD' )
      REWIND 40
      READ ( 40, * ) nw
      IF ( nw /= nmax ) CALL errore(' TB_hamiltonian ', ' wrong nmax in reading (II)', nw )

      DO j = 1, nmax
         READ ( 40, * ) 
         DO i = 1, nmax
            READ ( 40, * ) h1(i,j)
         END DO
      END DO
      CLOSE ( 40 )


!
!!...  Check: write hamiltonian h0 and h1
!     DO i = 1, nmax
!        WRITE ( *, '(6(f6.2))' ) ( h0(i,j), j = 1, nmax )
!     END DO
!     PRINT*,'=============================================='
!
!     DO i = 1, nmax
!        WRITE ( *, '(6(f6.2))' ) ( h1(i,j), j = 1, nmax )
!     END DO
!     PRINT*,'=============================================='
!     STOP
!
!............................................................

      RETURN
      END
