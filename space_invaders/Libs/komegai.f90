       FUNCTION komegai( kpt, lamp, kcm, wb, wbtot, nnlist, nshells,       &
                         nnshell, dimwann, dimwin, mxdbnd, mxdnrk, mxdnn )

!.................................................................................
!
! CALCULATES THE CONTRIBUTION OF A GIVEN K-POINT TO OMEGA_I
!
! WRITTEN 27 OCT 2000 BY IVO SOUZA
! MODIFIED (MADE FASTER) 23 JAN 2001
!
! INPUT:
!
! KPT               K-POINT AT WHICH THE MATRIX IS BEING CALCULATED
! LAMP(J,L,NKP)     AMPLITUDE OF THE J-TH ENERGY EIGENVECTOR INSIDE THE 
!                   ENERGY WINDOW AT THE NKP-TH K-POINT IN THE EXPANSION OF THE
!                   L-TH LAMBDA EIGENVECTOR AT THE SAME K-POINT
! LAMP(J,L,NKP)     AMPLITUDE OF THE J-TH ENERGY EIGENVECTOR INSIDE THE 
!                   ENERGY WINDOW AT THE NKP-TH K-POINT IN THE EXPANSION OF THE
!                   L-TH LAMBDA EIGENVECTOR AT THE SAME K-POINT
! wb(nkp,nnx)       weight of the nnx-th b-vector (ordered along shells 
!                   of increasing length) associated with the nkp-th k-point
! wbtot             sum of the weights of all b-vectors associated with a
!                   given k-point (k-point 1 is used in calculation)
! nnlist(nkp,nnx)   vkpt(1:3,nnlist(nkp,nnx)) is the nnx-th neighboring
!                   k-point of the nkp-th k-point vkpt(1:3,nkp) (or its
!                   periodic image in the "home Brillouin zone")
! NSHELLS           number of shells of k-points to be used in the 
!                   finite-difference formulas for the k-derivatives
! nnshell(nkp,ndnn) population of the ndnn-th shell around the nkp-th k-point
! DIMWANN           dimensionality of the subspace at each k-point 
!                   (number of Wannier functions per unit cell that we want)
! DIMWIN(NKP)       number of bands at the nkp-th k-point that fall 
!                   within the "sharp" energy window               
! MXDBND            ARRAY DIMENSION FOR BANDS
! MXDNRK            ARRAY DIMENSION FOR K-POINTS
! MXDNN             maximum possible number of nearest-neighbor k-points (12); 
!                   turns out to equal the maximum possible number of b-vectors
!                   that may be needed in the finite-difference formulas
!                   for the k-derivatives
!
!.................................................................................
 

       IMPLICIT NONE
 
       REAL*8 :: komegai
       INTEGER :: mxdbnd, mxdnrk, mxdnn
       INTEGER :: kpt, nnlist(mxdnrk,mxdnn)
       INTEGER :: nshells, nnshell(mxdnrk,mxdnn)
       INTEGER :: dimwann, dimwin(mxdnrk)
       REAL*8 ::  wb(mxdnrk,mxdnn), wbtot
       COMPLEX*16 :: lamp(mxdbnd,mxdbnd,mxdnrk)
       COMPLEX*16 :: kcm(mxdbnd,mxdbnd,mxdnn)
 
       INTEGER :: j, l, m, n 
       INTEGER :: ndnn, nnsh, nnx, k_pls_b
       COMPLEX*16 :: dot_bloch1
       COMPLEX*16 :: czero
       PARAMETER( CZERO = ( 0.0d0, 0.0d0 ) )
 

       komegai = DBLE(dimwann) * wbtot
 
       DO m = 1, dimwann      ! index of lambda eigenvector at k
         DO n = 1, dimwann      ! index of lambda eigenvector at k+b
 
! ...    LOOP OVER B-VECTORS
 
           nnx=0
           DO ndnn=1,nshells
             DO nnsh=1,nnshell(kpt,ndnn)
               nnx=nnx+1
               k_pls_b=nnlist(kpt,nnx)
 
! ...          CALCULATE THE DOTPRODUCT
 
               dot_bloch1 = czero
               DO j = 1, dimwin(kpt)
                 DO l = 1, dimwin(k_pls_b)
                   dot_bloch1 = dot_bloch1 + CONJG( lamp(j,m,kpt) ) * lamp(l,n,k_pls_b) * kcm(j,l,nnx)
                 END DO
               END DO

! ...          Add to total

               komegai = komegai - wb(kpt,nnx) * REAL( CONJG( dot_bloch1 ) * dot_bloch1 )
 
             END DO ! NNSH
           END DO ! NDNN
         END DO ! N 
       END DO ! M
 
       RETURN
       END FUNCTION

