       FUNCTION lambda_avg( m, kpt, lamp, kcm, nnlist, nshells,  &
                nnshell, wb, dimwann, dimwin, mxdbnd, mxdnrk, mxdnn )

!..................................................................................
!
! COMPUTES <lambda> = sum_{n=1}^N sum_b w_b |<u_{mk}^{i}|u_{n,k+b}^{i-1}>|^2
!
! WRITTEN 31 JAN 2001 BY IVO SOUZA
!
!
! INPUT:
!
! M                 INDEX OF THE FROZEN BAND FOR WHICH <LAMBDA> IS COMPUTED
! KPT               K-POINT AT WHICH THE OVERLAP MATRIX IS BEING CALCULATED
! LAMP(J,L,NKP)     AMPLITUDE OF THE J-TH ENERGY EIGENVECTOR INSIDE THE 
!                   ENERGY WINDOW AT THE NKP-TH K-POINT IN THE EXPANSION OF THE
!                   L-TH LAMBDA EIGENVECTOR AT THE SAME K-POINT
! KCM(N,M,NNX)      OVERLAP MATRIX <u_nk|u_{m,k+b}> WHERE VKPT(1:3,KPT) IS k 
!                   AND VKPT(1:3,NNLIST(KPT,NNX) IS k+b (OR ITS PERIODIC IMAGE
!                   IN THE "HOME BRILLOUIN ZONE")  
! nnlist(nkp,nnx)   vkpt(1:3,nnlist(nkp,nnx)) is the nnx-th neighboring
!                   k-point of the nkp-th k-point vkpt(1:3,nkp) (or its
!                   periodic image in the "home Brillouin zone")
! NSHELLS           number of shells of k-points to be used in the 
!                   finite-difference formulas for the k-derivatives
! nnshell(nkp,ndnn) population of the ndnn-th shell around the nkp-th k-point
! wb(nkp,nnx)       weight of the nnx-th b-vector (ordered along shells 
!                   of increasing length) associated with the nkp-th k-point
! DIMWANN           dimensionality of the subspace at each k-point 
!                   (number of Wannier functions per unit cell that we want)
! DIMWIN(NKP)       number of bands at the nkp-th k-point that fall 
!                   within the outer energy window               
! MXDBND            ARRAY DIMENSION FOR BANDS
! MXDNRK            ARRAY DIMENSION FOR K-POINTS
! MXDNN             maximum possible number of nearest-neighbor k-points (12); 
!                   turns out to equal the maximum possible number of b-vectors
!                   that may be needed in the finite-difference formulas
!                   for the k-derivatives
!
!.......................................................................................

       USE kinds   

       IMPLICIT NONE

       REAL(dbl) :: lambda_avg

       INTEGER :: mxdbnd, mxdnrk, mxdnn
       INTEGER :: m, kpt, nnlist(mxdnrk,mxdnn)
       INTEGER :: nshells, nnshell(mxdnrk,mxdnn)
       INTEGER :: dimwann, dimwin(mxdnrk)      
       REAL(dbl) :: wb(mxdnrk,mxdnn)
       COMPLEX(dbl) :: lamp(mxdbnd,mxdbnd,mxdnrk)
       COMPLEX(dbl) :: kcm(mxdbnd,mxdbnd,mxdnn)
 
       INTEGER :: n, l, j
       INTEGER :: nnx, ndnn, nnsh, k_pls_b
       COMPLEX(dbl) :: dot_bloch


       lambda_avg = 0.0d0

       DO n = 1, dimwann
 
! ...    Loop over b-vectors
 
         nnx = 0
         DO ndnn = 1, nshells
           DO nnsh = 1, nnshell(kpt,ndnn)
             nnx = nnx + 1
             k_pls_b = nnlist(kpt,nnx)
 
! ...        Calculate the dotproduct
 
             dot_bloch = CMPLX( 0.0d0, 0.0d0 )

             DO l = 1, dimwin(kpt)
               DO j = 1, dimwin(k_pls_b)
                 dot_bloch = dot_bloch + CONJG( lamp(l,m,kpt) ) * lamp(j,n,k_pls_b) * kcm(l,j,nnx)
               END DO
             END DO
  
             lambda_avg = lambda_avg + wb(kpt,nnx) * ABS(dot_bloch)**2
           END DO
         END DO

       END DO
 
       RETURN
       END FUNCTION
