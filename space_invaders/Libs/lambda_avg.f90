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
       FUNCTION lambda_avg( m, kpt, lamp, kcm, nnlist, nshells, nwhich, &
                nnshell, wb, dimwann, dimwin, dimwinx, mxdnrk, nnx )
!=----------------------------------------------------------------------------------=

       USE kinds, ONLY : dbl   
       USE constants, ONLY : CZERO

       IMPLICIT NONE

       REAL(dbl) :: lambda_avg

       INTEGER :: dimwinx, mxdnrk, nnx
       INTEGER :: m, kpt, nnlist(mxdnrk,nnx)
       INTEGER :: nshells, nwhich(nshells)
       INTEGER :: nnshell(mxdnrk,nnx)
       INTEGER :: dimwann, dimwin(mxdnrk)      
       REAL(dbl) :: wb(mxdnrk,nnx)
       COMPLEX(dbl) :: lamp(dimwinx,dimwinx,mxdnrk)
       COMPLEX(dbl) :: kcm(dimwinx,dimwinx,nnx)
 
       INTEGER :: n, l, j
       INTEGER :: inn, ndnc, ndnn, nnsh, k_pls_b
       COMPLEX(dbl) :: dot_bloch


       lambda_avg = 0.0d0

       DO n = 1, dimwann
 
! ...    Loop over b-vectors
 
         inn = 0
         DO ndnc = 1, nshells
             ndnn = nwhich(ndnc)
             DO nnsh = 1, nnshell(kpt,ndnn)
                 inn = inn + 1
                 k_pls_b = nnlist(kpt,inn)
 
! ...            Calculate the dotproduct
 
                 dot_bloch = CZERO
  
                 DO l = 1, dimwin(kpt)
                     DO j = 1, dimwin(k_pls_b)
                     dot_bloch = dot_bloch + CONJG( lamp(l,m,kpt) ) * &
                                             lamp(j,n,k_pls_b) * kcm(l,j,inn)
                 ENDDO
             ENDDO
  
             lambda_avg = lambda_avg + wb(kpt,inn) * ABS(dot_bloch)**2
             ENDDO
         ENDDO

       ENDDO
 
       RETURN
       END FUNCTION
