!
! Copyright (C) 2004 WanT Group
!
! This file is distributed under the terms of the
! GNU General Public License. See the file `License'
! in the root directory of the present distribution,
! or http://www.gnu.org/copyleft/gpl.txt .
!
!************************************************
SUBROUTINE zmatrix( ik, dimwann, dimwin, dimwinx, Akb, mtrx, dimfroz, indxnfroz)
  !************************************************
  !
  ! Compute the Z matrix according to the formula:
  ! Z_mn = \Sum_b wb  \Sum_l  a_{m,l}^{k,b} * CONJG ( a_{n,l}^{k,b} )
  ! where
  ! a_{m,l}^{k,b} = \Sum_j lamp(j,l,ikb) * Mkb_{m,j}
  ! (l is the index over the generators of the wannier subspace, while j is on 
  !  the bloch states) 
  !
  USE kinds
  USE constants, ONLY : CZERO
  USE timing_module
  USE kpoints_module, ONLY : nb, wb
 
  IMPLICIT NONE

  INTEGER,      INTENT(in) :: ik
  INTEGER,      INTENT(in) :: dimwann, dimwin(*), dimwinx
  INTEGER,      INTENT(in) :: dimfroz, indxnfroz(*)
  COMPLEX(dbl), INTENT(in) :: Akb(dimwinx,dimwann,*)
  COMPLEX(dbl), INTENT(inout) :: mtrx(dimwinx,dimwinx)

  !
  ! few local variables
  !
  INTEGER :: ib
  INTEGER :: m, n, mf, nf, l

!
!--------------------------------------------------------------------
!

   mtrx(:,:) = CZERO
   IF ( dimwann == dimfroz ) RETURN
   !
   CALL timing('zmatrix',OPR='start')

   !
   ! ...  Loop over b-vectors
   ! 
   DO ib = 1, nb

       !
       ! loop over the generators of the subspace
       !
       DO l = 1, dimwann

            !
            ! update mtrx
            !
            DO n = 1, dimwin(ik) - dimfroz
                nf = indxnfroz(n)
                !
                DO m = 1, n
                    mf = indxnfroz(m)
                    !
                    mtrx(m,n) = mtrx(m,n) +  &
                                wb(ib) * Akb(mf,l,ib) * CONJG( Akb(nf,l,ib) )

                ENDDO
            ENDDO
       ENDDO 
   ENDDO 

   !
   ! use hermiticity
   !
   DO n = 1, dimwin(ik) - dimfroz
   DO m = 1, n
        mtrx(n,m) = CONJG( mtrx(m,n) )
   ENDDO
   ENDDO
 
   CALL timing('zmatrix',OPR='stop')
 
END SUBROUTINE zmatrix

