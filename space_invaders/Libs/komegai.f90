!
! Copyright (C) 2004 Arrigo Calzolari, Carlo Cavazzoni, Marco Buongiorno Nardelli
! Copyright (C) 2002 Nicola Marzari, Ivo Souza, David Vanderbilt
!
! This file is distributed under the terms of the
! GNU General Public License. See the file `License'
! in the root directory of the present distribution,
! or http://www.gnu.org/copyleft/gpl.txt .
!
!=----------------------------------------------------------------------------------=
       FUNCTION komegai( kpt, lamp, kcm, wb, wbtot, nnlist, nshells, nwhich,    &
                         nnshell, dimwann, dimwin, mxdbnd, mxdnrk, mxdnn )
!=----------------------------------------------------------------------------------=
!
!...   Calculates the contribution of a given k-point to Omega_I
!

       USE kinds
       USE constants, ONLY : CZERO

       IMPLICIT NONE
 
       INTEGER :: mxdbnd, mxdnrk, mxdnn
       INTEGER :: kpt, nnlist(mxdnrk,mxdnn)
       INTEGER :: nshells, nwhich(nshells)
       INTEGER :: nnshell(mxdnrk,mxdnn)
       INTEGER :: dimwann, dimwin(mxdnrk)
       REAL(dbl) :: komegai
       REAL(dbl) ::  wb(mxdnrk,mxdnn), wbtot
       COMPLEX(dbl) :: lamp(mxdbnd,mxdbnd,mxdnrk)
       COMPLEX(dbl) :: kcm(mxdbnd,mxdbnd,mxdnn)
 
       INTEGER :: j, l, m, n 
       INTEGER :: ndnn, ndnc, nnsh, nnx, k_pls_b
       COMPLEX(dbl) :: dot_bloch1
 

       komegai = DBLE(dimwann) * wbtot
 
       DO m = 1, dimwann      ! index of lambda eigenvector at k
       DO n = 1, dimwann      ! index of lambda eigenvector at k+b
 
! ...    LOOP OVER B-VECTORS
 
           nnx=0
           DO ndnc=1,nshells
               ndnn = nwhich(ndnc)
               DO nnsh=1,nnshell(kpt,ndnn)
                   nnx=nnx+1
                   k_pls_b=nnlist(kpt,nnx)
 
! ...              CALCULATE THE DOTPRODUCT
 
                   dot_bloch1 = czero
                   DO j = 1, dimwin(kpt)
                   DO l = 1, dimwin(k_pls_b)
                         dot_bloch1 = dot_bloch1 + CONJG( lamp(j,m,kpt) ) * &
                                                   lamp(l,n,k_pls_b) * kcm(j,l,nnx)
                   ENDDO
                   ENDDO

! ...              Add to total

                   komegai = komegai - wb(kpt,nnx) * REAL( CONJG( dot_bloch1 ) * dot_bloch1 )
 
               ENDDO ! NNSH
           ENDDO ! NDNN

       ENDDO ! N 
       ENDDO ! M
 
       RETURN
       END FUNCTION

