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
       SUBROUTINE zmatrix( kpt, nnlist, nshells, nwhich, nnshell, wb, lamp, kcm, mtrx,   &
                  dimwann, dimwin, dimfroz, indxnfroz, mxdbnd, mxdnrk, mxdnn )
!=----------------------------------------------------------------------------------=
       USE kinds
       USE constants, ONLY : CZERO
 
       IMPLICIT NONE

       INTEGER :: mxdbnd
       INTEGER :: mxdnrk
       INTEGER :: mxdnn
       INTEGER :: kpt
       INTEGER :: nnlist(mxdnrk,mxdnn)
       INTEGER :: nshells, nwhich(nshells)
       INTEGER :: nnshell(mxdnrk,mxdnn)
       INTEGER :: dimwann, dimwin(mxdnrk)
       INTEGER :: dimfroz(mxdnrk), indxnfroz(mxdbnd,mxdnrk)
       REAL(dbl) :: wb(mxdnrk,mxdnn)
       COMPLEX(dbl) :: lamp(mxdbnd,mxdbnd,mxdnrk)
       COMPLEX(dbl) :: kcm(mxdbnd,mxdbnd,mxdnn)

       COMPLEX(dbl) :: mtrx(mxdbnd,mxdbnd)
 
       INTEGER :: m, n, l, j
       INTEGER :: nnx, ndnc, ndnn, nnsh, k_pls_b
       COMPLEX(dbl) :: dot_bloch1, dot_bloch2

! ...  Loop over independent matrix entries
 
       DO m = 1, dimwin(kpt) - dimfroz(kpt)
         DO n = 1, m
           mtrx(m,n) = czero
           DO l = 1, dimwann
 
! ...        LOOP OVER B-VECTORS
 
             nnx = 0
             DO ndnc = 1, nshells
                 ndnn = nwhich(ndnc)
                 DO nnsh = 1, nnshell(kpt,ndnn)
                     nnx = nnx + 1
                     k_pls_b = nnlist(kpt,nnx)

! ...                CALCULATE THE DOTPRODUCTS
 
                     dot_bloch1 = czero
                     dot_bloch2 = czero
                     DO j = 1, dimwin(k_pls_b)
                        dot_bloch1 = dot_bloch1 + lamp(j,l,k_pls_b) * &
                                                  kcm( indxnfroz(m,kpt), j, nnx )
                        dot_bloch2 = dot_bloch2 + CONJG( lamp( j, l, k_pls_b ) * &
                                                  kcm( indxnfroz(n,kpt), j, nnx ) )
                      ENDDO
 
! ...                 Add contribution to mtrx(m,n)            
 
                      mtrx(m,n) = mtrx(m,n) + wb(kpt,nnx) * dot_bloch1 * dot_bloch2
 
                 ENDDO ! NNSH
             ENDDO ! NDNN

           ENDDO ! L
           mtrx(n,m) = CONJG( mtrx(m,n) )   ! hermiticity
         ENDDO ! N
       ENDDO ! M
 
       RETURN
       END
