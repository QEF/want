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
       SUBROUTINE zmatrix( kpt, nnlist, nshells, nnshell, wb, lamp, kcm, mtrx,   &
                  dimwann, dimwin, dimfroz, indxnfroz, mxdbnd, mxdnrk, mxdnn )
!=----------------------------------------------------------------------------------=
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
