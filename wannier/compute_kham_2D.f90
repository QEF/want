!
! Copyright (C) 2006 WanT Group
!
! This file is distributed under the terms of the
! GNU General Public License. See the file `License'
! in the root directory of the present distribution,
! or http://www.gnu.org/copyleft/gpl.txt .
!
!***************************************************
    SUBROUTINE compute_kham_2D( dimwann, idir, vr_cry_1D, nr, vr, wr, rham, vkpt, rham_1D )
   !***************************************************
   !
   ! Calculates the hamiltonian on the bloch basis 
   ! once given on the Wannier basis
   !
   ! ham(:,:, R_1D) = sum_R_2D  w(R_2D) * e^( i k_2D * R_2D ) ham(:,:,R_2D)    
   !
   ! R_1D is along the crystal component idir,
   ! and the specific value of R_1D (crystal units) is given by vr_1D
   !
   ! vr and vkpt (3D vectors) are in cartesian coordinates 
   ! bohr and bohr^-1 units
   !
   USE kinds
   USE constants,              ONLY : CZERO, EPS_m6
   USE log_module,             ONLY : log_push, log_pop
   USE lattice_module,         ONLY : avec
   USE converters_module,      ONLY : cart2cry
   USE timing_module,          ONLY : timing
   !
   IMPLICIT NONE
 
   !
   ! input variables
   !
   INTEGER,      INTENT(in)  :: dimwann, idir, nr
   REAL(dbl),    INTENT(in)  :: vr( 3, nr), wr(nr), vkpt(3)
   REAL(dbl),    INTENT(in)  :: vr_cry_1D
   COMPLEX(dbl), INTENT(in)  :: rham( dimwann, dimwann, nr)
   COMPLEX(dbl), INTENT(out) :: rham_1D( dimwann, dimwann)
 
   !
   ! local variables
   !
   INTEGER          :: i, j, ir 
   INTEGER          :: nr_2D
   INTEGER          :: map(nr)
   REAL(dbl)        :: vr_cry(3,nr)
   REAL(dbl)        :: arg
   COMPLEX(dbl)     :: phase
   !
   ! end of declariations
   !

!
!------------------------------
! main body
!------------------------------
!
   CALL timing('compute_kham_2D',OPR='start') 
   CALL log_push('compute_kham_2D')

   !
   ! select the R vectors belonging to the
   ! required plane of R vectors
   !
   vr_cry   = vr
   CALL cart2cry( vr_cry, avec)
   !
   nr_2D     = 0
   map( : ) = -1 
   !
   DO ir = 1, nr
       !
       IF ( ABS( vr_cry(idir,ir) - vr_cry_1D) < EPS_m6 ) THEN
           !
           nr_2D = nr_2D + 1
           map( nr_2D ) = ir
           !
       ENDIF
       !
   ENDDO
  

   rham_1D( :, :) = CZERO
   !
   DO ir = 1, nr_2D
       !
       arg =   DOT_PRODUCT( vkpt(:), vr(:, map(ir) ) )
       phase = CMPLX( COS(arg), SIN(arg), dbl ) * wr( map(ir) )
       !
       DO j = 1, dimwann
       DO i = 1, dimwann
           rham_1D(i,j) = rham_1D(i,j) + phase * rham(i, j, map(ir) )
       ENDDO
       ENDDO
       !
   ENDDO

   CALL timing('compute_kham_2D',OPR='stop') 
   CALL log_pop ('compute_kham_2D')
    
END SUBROUTINE compute_kham_2D

