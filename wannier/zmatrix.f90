       SUBROUTINE zmatrix( kpt, nnlist, nshells, nnshell, wb, lamp, kcm, mtrx,   &
                  dimwann, dimwin, dimfroz, indxnfroz, mxdbnd, mxdnrk, mxdnn )


!...........................................................................
! WRITTEN 20 DEC 2000 BY IVO SOUZA 
! CALCULATES THE Z_mn(k) MATRIX AT k AT THE PRESENT ITERATION
!
! INPUT:
!
! KPT               K-POINT AT WHICH THE Z-MATRIX IS BEING CALCULATED
! nnlist(nkp,nnx)   vkpt(1:3,nnlist(nkp,nnx)) is the nnx-th neighboring
!                   k-point of the nkp-th k-point vkpt(1:3,nkp) (or its
!                   periodic image in the "home Brillouin zone")
! NSHELLS           number of shells of k-points to be used in the 
!                   finite-difference formulas for the k-derivatives
! nnshell(nkp,ndnn) population of the ndnn-th shell around the nkp-th k-point
! wb(nkp,nnx)       weight of the nnx-th b-vector (ordered along shells 
!                   of increasing length) associated with the nkp-th k-point
! LAMP(J,L,NKP)     AMPLITUDE OF THE J-TH ENERGY EIGENVECTOR INSIDE THE 
!                   ENERGY WINDOW AT THE NKP-TH K-POINT IN THE EXPANSION OF THE
!                   L-TH LAMBDA EIGENVECTOR AT THE SAME K-POINT
! KCM(N,M,NNX)      OVERLAP MATRIX <u_nk|u_{m,k+b}> WHERE VKPT(1:3,KPT) IS k 
!                   AND VKPT(1:3,NNLIST(KPT,NNX) IS k+b (OR ITS PERIODIC IMAGE
!                   IN THE "HOME BRILLOUIN ZONE")  
! DIMWANN           dimensionality of the subspace at each k-point 
!                   (number of Wannier functions per unit cell that we want)
! DIMWIN(NKP)       number of bands at the nkp-th k-point that fall 
!                   within the "sharp" energy window               
! DIMFROZ(NKP)      number of bands at the nkp-th k-point that fall 
!                   within the inner energy window               
! INDXNFROZ(I,NKP)  INDEX (BETWEEN 1 AND DIMWIN(NKP)) OF THE I-TH NON-FROZEN
!                   ORIGINAL BAND STATE AT THE NKP-TH K-POINT (STARTING FROM
!                   THE BOTTOM OF THE OUTER WINDOW)
! MXDBND            ARRAY DIMENSION FOR BANDS
! MXDNRK            ARRAY DIMENSION FOR K-POINTS
! MXDNN             maximum possible number of nearest-neighbor k-points (12); 
!                   turns out to equal the maximum possible number of b-vectors
!                   that may be needed in the finite-difference formulas
!                   for the k-derivatives
!
! OUTPUT:
!
! MTRX(M,N)         (M,N)-TH ENTRY IN THE 
!                   (DIMWIN(KPT)-DIMFROZ(KPT)) x (DIMWIN(KPT)-DIMFROZ(KPT)) 
!                   HERMITIAN MATRIX AT THE KPT-TH K-POINT
!
!...........................................................................

       USE kinds
 
       IMPLICIT NONE

       INTEGER :: mxdbnd
       INTEGER :: mxdnrk
       INTEGER :: mxdnn
       INTEGER :: kpt
       INTEGER :: nnlist(mxdnrk,mxdnn)
       INTEGER :: nshells, nnshell(mxdnrk,mxdnn)
       INTEGER :: dimwann, dimwin(mxdnrk)
       INTEGER :: dimfroz(mxdnrk), indxnfroz(mxdbnd,mxdnrk)
       REAL(dbl) :: wb(mxdnrk,mxdnn)
       COMPLEX(dbl) :: lamp(mxdbnd,mxdbnd,mxdnrk)
       COMPLEX(dbl) :: kcm(mxdbnd,mxdbnd,mxdnn)

       COMPLEX(dbl) :: mtrx(mxdbnd,mxdbnd)
 
       INTEGER :: m, n, l, j
       INTEGER :: nnx, ndnn, nnsh, k_pls_b
       COMPLEX(dbl) :: dot_bloch1, dot_bloch2
       COMPLEX(dbl) :: czero 
       PARAMETER( czero = ( 0.0d0, 0.0d0 ) )

! ...  Loop over independent matrix entries
 
       DO m = 1, dimwin(kpt) - dimfroz(kpt)
         DO n = 1, m
           mtrx(m,n) = czero
           do l = 1, dimwann
 
! ...        LOOP OVER B-VECTORS
 
             nnx = 0
             DO ndnn = 1, nshells
               DO nnsh = 1, nnshell(kpt,ndnn)
                 nnx = nnx + 1
                 k_pls_b = nnlist(kpt,nnx)

! ...            CALCULATE THE DOTPRODUCTS
 
                 dot_bloch1 = czero
                 dot_bloch2 = czero
                 DO j = 1, dimwin(k_pls_b)
                   dot_bloch1 = dot_bloch1 + lamp(j,l,k_pls_b) * kcm( indxnfroz(m,kpt), j, nnx )
                   dot_bloch2 = dot_bloch2 + CONJG( lamp( j, l, k_pls_b ) * kcm( indxnfroz(n,kpt), j, nnx ) )
                 END DO
 
! ...            Add contribution to mtrx(m,n)            
 
                 mtrx(m,n) = mtrx(m,n) + wb(kpt,nnx) * dot_bloch1 * dot_bloch2
 
               END DO ! NNSH
             END DO ! NDNN
           END DO ! L
           mtrx(n,m) = CONJG( mtrx(m,n) )   ! hermiticity
         END DO ! N
       END DO ! M
 
       RETURN
       END
