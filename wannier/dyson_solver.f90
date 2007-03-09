!
! Copyright (C) 2007 WanT Group
!
! This file is distributed under the terms of the
! GNU General Public License. See the file `License'
! in the root directory of the present distribution,
! or http://www.gnu.org/copyleft/gpl.txt .
!
!********************************************************
   SUBROUTINE dyson_solver( ze, dimwann, ham, sgm, GF0, GF )
   !********************************************************
   !
   ! Solve Dyson equation to compute the interacting (GF)
   ! and the non-interacting (GF0) Green's functions related
   ! to the input Hamiltonian HAM and self-energy SGM
   !
   ! Smearing stuff not yet included. (AF)
   !
   USE kinds
   USE util_module,      ONLY : mat_inv
   USE timing_module,    ONLY : timing
   USE log_module,       ONLY : log_push, log_pop
   IMPLICIT NONE

   !
   ! input variables
   !
   INTEGER,      INTENT(in)  :: dimwann
   COMPLEX(dbl), INTENT(in)  :: ze
   COMPLEX(dbl), INTENT(in)  :: ham(dimwann,dimwann)
   COMPLEX(dbl), INTENT(in)  :: sgm(dimwann,dimwann)
   !
   COMPLEX(dbl), INTENT(out) :: GF0(dimwann,dimwann)
   COMPLEX(dbl), INTENT(out) :: GF(dimwann,dimwann)

   !
   ! local variables
   !
   INTEGER   :: i, ierr
   COMPLEX(dbl), ALLOCATABLE :: caux(:,:)
   !
   ! end of declariations
   !

!
!------------------------------
! main body 
!------------------------------
!
   CALL timing('dyson_solver',OPR='start')
   CALL log_push('dyson_solver')

   !
   ! we need to make an inversion both for GF0 and GF
   !
   ALLOCATE( caux(dimwann, dimwann), STAT=ierr )
   IF ( ierr/=0 ) CALL errore('dyson_solver','allocating caux', ABS(ierr))

   !
   ! compute GF0
   !
   caux = - ham(:,:)
   !
   DO i = 1, dimwann
      !
      caux(i,i) = caux(i,i) + ze 
      !
   ENDDO
   !
   CALL mat_inv( dimwann, caux, GF0)

   !
   ! compute GF
   !
   caux = - ham(:,:) - sgm(:,:)
   !
   DO i = 1, dimwann
      !
      caux(i,i) = caux(i,i) + ze 
      !
   ENDDO
   !
   CALL mat_inv( dimwann, caux, GF)

   !
   ! local cleaning
   !
   DEALLOCATE( caux, STAT=ierr )
   IF ( ierr/=0 ) CALL errore('dyson_solver','allocating caux', ABS(ierr))
   !
   CALL timing('dyson_solver',OPR='stop')
   CALL log_pop('dyson_solver')
   !
END SUBROUTINE dyson_solver


