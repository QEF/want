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
                nnshell, wb, dimwann, dimwin, mxdbnd, mxdnrk, mxdnn )
!=----------------------------------------------------------------------------------=

       USE kinds   

       IMPLICIT NONE

       REAL(dbl) :: lambda_avg

       INTEGER :: mxdbnd, mxdnrk, mxdnn
       INTEGER :: m, kpt, nnlist(mxdnrk,mxdnn)
       INTEGER :: nshells, nwhich(nshells)
       INTEGER :: nnshell(mxdnrk,mxdnn)
       INTEGER :: dimwann, dimwin(mxdnrk)      
       REAL(dbl) :: wb(mxdnrk,mxdnn)
       COMPLEX(dbl) :: lamp(mxdbnd,mxdbnd,mxdnrk)
       COMPLEX(dbl) :: kcm(mxdbnd,mxdbnd,mxdnn)
 
       INTEGER :: n, l, j
       INTEGER :: nnx, ndnc, ndnn, nnsh, k_pls_b
       COMPLEX(dbl) :: dot_bloch


       lambda_avg = 0.0d0

       DO n = 1, dimwann
 
! ...    Loop over b-vectors
 
         nnx = 0
         DO ndnc = 1, nshells
             ndnn = nwhich(ndnc)
             DO nnsh = 1, nnshell(kpt,ndnn)
                 nnx = nnx + 1
                 k_pls_b = nnlist(kpt,nnx)
 
! ...            Calculate the dotproduct
 
                 dot_bloch = CMPLX( 0.0d0, 0.0d0 )
  
                 DO l = 1, dimwin(kpt)
                     DO j = 1, dimwin(k_pls_b)
                     dot_bloch = dot_bloch + CONJG( lamp(l,m,kpt) ) * &
                                             lamp(j,n,k_pls_b) * kcm(l,j,nnx)
                 ENDDO
             ENDDO
  
             lambda_avg = lambda_avg + wb(kpt,nnx) * ABS(dot_bloch)**2
             ENDDO
         ENDDO

       ENDDO
 
       RETURN
       END FUNCTION
