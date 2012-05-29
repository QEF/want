!
! Copyright (C) 2007 WanT Group
!
! This file is distributed under the terms of the
! GNU General Public License. See the file `License'
! in the root directory of the present distribution,
! or http://www.gnu.org/copyleft/gpl.txt .
!
!********************************************************
   MODULE dyson_solver_module
   !********************************************************
   !
   ! contains the routine used to solve the dyson equation
   ! for both the overlap and non-overlap cases
   !
   IMPLICIT NONE
   PRIVATE

   PUBLIC :: dyson_solver

CONTAINS

!********************************************************
   SUBROUTINE dyson_solver( GF0, GF, ze, dimwann, ham, sgm, ovp )
   !********************************************************
   !
   ! Solve Dyson equation to compute the interacting (GF)
   ! and the non-interacting (GF0) Green's functions related
   ! to the input Hamiltonian HAM and self-energy SGM
   !
   ! Smearing stuff not yet included. (AF)
   !
   USE kinds
   USE constants,        ONLY : CONE, CZERO
   USE util_module,      ONLY : mat_inv
   USE timing_module,    ONLY : timing
   USE log_module,       ONLY : log_push, log_pop
   IMPLICIT NONE

   !
   ! input variables
   !
   INTEGER,                INTENT(IN)  :: dimwann
   !
   COMPLEX(dbl),           INTENT(OUT) :: GF0(dimwann,dimwann)
   COMPLEX(dbl),           INTENT(OUT) :: GF(dimwann,dimwann)
   !
   COMPLEX(dbl),           INTENT(IN)  :: ze
   COMPLEX(dbl),           INTENT(IN)  :: ham(dimwann,dimwann)
   COMPLEX(dbl),           INTENT(IN)  :: sgm(dimwann,dimwann)
   COMPLEX(dbl), OPTIONAL, INTENT(in)  :: ovp(dimwann,dimwann)

   !
   ! local variables
   !
   INTEGER   :: i, j, ierr
   COMPLEX(dbl), ALLOCATABLE :: caux(:,:)
   COMPLEX(dbl), ALLOCATABLE :: lovp(:,:)
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
   ALLOCATE( lovp(dimwann, dimwann), STAT=ierr )
   IF ( ierr/=0 ) CALL errore('dyson_solver','allocating lovp', ABS(ierr))

   !
   ! deal with overlaps
   !
   IF ( PRESENT( ovp ) ) THEN
       !
       lovp (:,:) = ovp(:,:)
       !
   ELSE
       !
       lovp(:,:) = CZERO
       !
       DO i = 1, dimwann
          lovp(i,i) = CONE
       ENDDO
       !
   ENDIF


   !
   ! compute GF0
   !
   DO j = 1, dimwann
   DO i = 1, dimwann
       !
       caux(i,j) = ze * lovp(i,j) - ham(i,j)
       !
   ENDDO
   ENDDO
   !
   CALL mat_inv( dimwann, caux, GF0)

   !
   ! compute GF
   !
   DO j = 1, dimwann
   DO i = 1, dimwann
       !
       caux(i,j) = ze * lovp(i,j) - ham(i,j) -sgm(i,j)
       !
   ENDDO
   ENDDO
   !
   CALL mat_inv( dimwann, caux, GF)

   !
   ! local cleaning
   !
   DEALLOCATE( caux, STAT=ierr )
   IF ( ierr/=0 ) CALL errore('dyson_solver','allocating caux', ABS(ierr))
   DEALLOCATE( lovp, STAT=ierr )
   IF ( ierr/=0 ) CALL errore('dyson_solver','allocating lovp', ABS(ierr))
   !
   CALL timing('dyson_solver',OPR='stop')
   CALL log_pop('dyson_solver')
   !
END SUBROUTINE dyson_solver

END MODULE dyson_solver_module

