!
! Copyright (C) 2004 WanT Group
!
! This file is distributed under the terms of the
! GNU General Public License. See the file `License'
! in the root directory of the present distribution,
! or http://www.gnu.org/copyleft/gpl.txt .
!
!************************************************
SUBROUTINE zmatrix( ik_g, dimwann, dimwin, dimwinx, Akb, mtrx, dimfroz, indxnfroz)
  !************************************************
  !
  ! Compute the Z matrix according to the formula:
  ! Z_mn = \Sum_b wb  \Sum_l  a_{m,l}^{k,b} * CONJG ( a_{n,l}^{k,b} )
  ! where
  ! a_{m,l}^{k,b} = \Sum_j  Mkb_{m,j} * lamp(j,l,ikb)
  ! (l is the index over the generators of the wannier subspace, while j is on 
  !  the bloch states) 
  !
  USE kinds
  USE constants,      ONLY : CZERO, CONE
  USE kpoints_module, ONLY : nb, wb
  USE timing_module
 
  IMPLICIT NONE

  INTEGER,      INTENT(in)    :: ik_g
  INTEGER,      INTENT(in)    :: dimwann, dimwin(*), dimwinx
  INTEGER,      INTENT(in)    :: dimfroz, indxnfroz(*)
  COMPLEX(dbl), INTENT(in)    :: Akb(dimwinx,dimwann,*)
  COMPLEX(dbl), INTENT(inout) :: mtrx(dimwinx,dimwinx)

  !
  ! few local variables
  !
  INTEGER :: dimaux
  INTEGER :: ib, m, l, ierr
  COMPLEX(dbl), ALLOCATABLE :: caux(:,:)

!
!---------------------------------------------------------
!

   mtrx(:,:) = CZERO
   IF ( dimwann == dimfroz ) RETURN
   !
   CALL timing('zmatrix',OPR='start')

   !
   ! set auxiliary quantities
   !
   dimaux = dimwin(ik_g)-dimfroz
   !
   ALLOCATE( caux( dimaux, dimwann), STAT=ierr )
   IF ( ierr/=0 ) CALL errore('zmatrix','allocating caux', ABS(ierr))

   !
   ! Loop over b-vectors
   ! here we need to consider both b, -b because 
   ! Akb and not Mkb enter the expression for z-matrix
   !
   ! In fact, Akb breaks the [ M^{k,b}_mn ]^* = M^{k+b,-b}_nm
   ! symmetry
   ! 
   DO ib = 1, nb

       !
       ! setup the auxiliary matrix CAUX to allow for the usage of ZGEMM
       !
       DO l = 1, dimwann
           !
           caux(:,l) = CZERO
           !
           DO m = 1, dimaux     ! dimwin(ik_g) - dimwann
               !
               caux( m, l ) = Akb( indxnfroz(m), l, ib)
               !
           ENDDO
           !
       ENDDO
       !
       ! Here we use ZGEMM instead of the driver zmat_mul 
       ! to directly increment mtrx avoiding further auxiliary memory.
       ! Note: the usage of ZHEMM instead of ZGEMM might be evaluated. 
       !
       CALL ZGEMM( 'N', 'C', dimaux, dimaux, dimwann,   &
                   wb(ib)*CONE, caux, dimaux, caux, dimaux, CONE, mtrx, dimwinx)
       !
   ENDDO
   !
   !
   DEALLOCATE( caux, STAT=ierr )
   IF ( ierr/=0 ) CALL errore('zmatrix','deallocating caux', ABS(ierr))
   !
   CALL timing('zmatrix',OPR='stop')
 
END SUBROUTINE zmatrix

