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
                  dimwann, dimwin, dimwinx, dimfroz, indxnfroz, nbnd, mxdnrk, nnx )
!=----------------------------------------------------------------------------------=
       USE kinds
       USE constants, ONLY : CZERO
 
       IMPLICIT NONE

       INTEGER :: nbnd
       INTEGER :: mxdnrk
       INTEGER :: nnx
       INTEGER :: kpt
       INTEGER :: nnlist(mxdnrk,nnx)
       INTEGER :: nshells, nwhich(nshells)
       INTEGER :: nnshell(mxdnrk,nnx)
       INTEGER :: dimwann, dimwin(mxdnrk), dimwinx
       INTEGER :: dimfroz(mxdnrk), indxnfroz(nbnd,mxdnrk)
       REAL(dbl) :: wb(mxdnrk,nnx)
       COMPLEX(dbl) :: lamp(dimwinx,dimwinx,mxdnrk)
       COMPLEX(dbl) :: kcm(dimwinx,dimwinx,nnx)

       COMPLEX(dbl) :: mtrx(dimwinx,dimwinx)
 
       INTEGER :: m, n, l, j
       INTEGER :: inn, ndnc, ndnn, nnsh, k_pls_b
       COMPLEX(dbl) :: dot_bloch1, dot_bloch2

! ...  Loop over independent matrix entries
 
       DO m = 1, dimwin(kpt) - dimfroz(kpt)
         DO n = 1, m
           mtrx(m,n) = CZERO
           DO l = 1, dimwann
 
! ...        LOOP OVER B-VECTORS
 
             inn = 0
             DO ndnc = 1, nshells
                 ndnn = nwhich(ndnc)
                 DO nnsh = 1, nnshell(kpt,ndnn)
                     inn = inn + 1
                     k_pls_b = nnlist(kpt,inn)

! ...                CALCULATE THE DOTPRODUCTS
 
                     dot_bloch1 = CZERO
                     dot_bloch2 = CZERO
                     DO j = 1, dimwin(k_pls_b)
                        dot_bloch1 = dot_bloch1 + lamp(j,l,k_pls_b) * &
                                                  kcm( indxnfroz(m,kpt), j, inn )
                        dot_bloch2 = dot_bloch2 + CONJG( lamp( j, l, k_pls_b ) * &
                                                  kcm( indxnfroz(n,kpt), j, inn ) )
                      ENDDO
 
! ...                 Add contribution to mtrx(m,n)            
 
                      mtrx(m,n) = mtrx(m,n) + wb(kpt,inn) * dot_bloch1 * dot_bloch2
 
                 ENDDO ! NNSH
             ENDDO ! NDNN

           ENDDO ! L
           mtrx(n,m) = CONJG( mtrx(m,n) )   ! hermiticity
         ENDDO ! N
       ENDDO ! M
 
       RETURN
       END
