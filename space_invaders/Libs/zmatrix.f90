!
! Copyright (C) 2004 WanT Group
!
! This file is distributed under the terms of the
! GNU General Public License. See the file `License'
! in the root directory of the present distribution,
! or http://www.gnu.org/copyleft/gpl.txt .
!
!************************************************
SUBROUTINE zmatrix( ik, lamp, Mkb, mtrx, dimwann, dimwin, dimwinx, dimfroz, indxnfroz)
  !************************************************
  USE kinds
  USE constants, ONLY : CZERO
  USE timing_module
  USE kpoints_module, ONLY : nnlist, nntot, wb
 
  IMPLICIT NONE

  INTEGER,      INTENT(in) :: ik
  INTEGER,      INTENT(in) :: dimwann, dimwin(*), dimwinx
  INTEGER,      INTENT(in) :: dimfroz, indxnfroz(*)
  COMPLEX(dbl), INTENT(in) :: lamp(dimwinx,dimwinx,*)
  COMPLEX(dbl), INTENT(in) :: Mkb(dimwinx,dimwinx,*)
  COMPLEX(dbl), INTENT(inout) :: mtrx(dimwinx,dimwinx)

  !
  ! few local variables
  !
  INTEGER :: inn, ikb
  INTEGER :: m, n, l, j, ierr
  COMPLEX(dbl), ALLOCATABLE :: a(:)

!
!--------------------------------------------------------------------
!
   CALL timing('zmatrix',OPR='start')

   mtrx(:,:) = CZERO
   ALLOCATE( a(dimwinx), STAT=ierr )
       IF (ierr/=0) CALL errore('zmatrix','allocating a',ABS(ierr))

   !
   ! ...  Loop over independent matrix entries
   ! 
   DO inn = 1, nntot(ik)
       ikb = nnlist(ik, inn)

       !
       ! loop over the generators of the subspace
       !
       DO l = 1, dimwann

            !
            ! Calculate the quantities  a_{m,l}^{k,b} = \Sum_j lamp(j,l,ikb) * Mkb_{m,j}
            !
            DO m = 1, dimwin(ik) - dimfroz
                a(m) = CZERO
                DO j = 1, dimwin(ikb)
                     a(m) = a(m) + lamp(j,l,ikb) * Mkb( indxnfroz(m), j, inn )
                ENDDO
            ENDDO

            !
            ! update mtrx
            !
            DO n = 1, dimwin(ik) - dimfroz
            DO m = 1, n
                 mtrx(m,n) = mtrx(m,n) + wb(ik,inn) * a(m) * CONJG( a(n) )
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
 
   DEALLOCATE( a, STAT=ierr )
       IF (ierr/=0) CALL errore('zmatrix','deallocating a',ABS(ierr))
   CALL timing('zmatrix',OPR='stop')
 
END SUBROUTINE zmatrix

