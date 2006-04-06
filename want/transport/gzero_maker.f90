! 
! Copyright (C) 2005 WanT Group
! 
! This file is distributed under the terms of the 
! GNU General Public License. See the file `License' 
! in the root directory of the present distribution, 
! or http://www.gnu.org/copyleft/gpl.txt . 
!
!*****************************************************************
   SUBROUTINE gzero_maker( dim, omgS_ham, S, gzero )
   !*****************************************************************
   !
   ! Given \omega S - H, the routine compute the non-interacting Green func (gzero)
   ! according to the input smearing type.
   !
   ! the implemented formula is
   ! gzero = g_smear( omgS_ham )
   !
   ! where f is numerically computed (according to smearing type and smearing) in
   ! smearing_module. See the module file for more details.
   !
   ! NOTE: omgS_ham MUST be hermitean (not checked in the routine)
   !
   USE kinds
   USE constants,        ONLY : ONE, CONE, CZERO, CI
   USE smearing_module,  ONLY : smear_alloc => alloc, smearing, smearing_type, &
                                g_smear, ggrid_smear, n_smear, dx_smear
   USE timing_module,    ONLY : timing
   USE util_module,      ONLY : mat_hdiag, mat_mul, mat_sv
   IMPLICIT NONE 

   !  
   ! input variables 
   !  
   INTEGER,      INTENT(in)  :: dim
   COMPLEX(dbl), INTENT(in)  :: omgS_ham(dim,dim), S(dim,dim)
   COMPLEX(dbl), INTENT(out) :: gzero(dim,dim)

   !
   ! local variables
   !
   INTEGER         :: i, j, ig, ierr
   CHARACTER(11)   :: subname = 'gzero_maker'
   REAL(dbl)       :: dx
   COMPLEX(dbl)    :: g1, g2
   REAL(dbl),    ALLOCATABLE :: w(:)
   COMPLEX(dbl), ALLOCATABLE :: aux(:,:), z(:,:), gw(:)
   !
   ! end of declarations 
   !

!
!----------------------------------------
! main Body
!----------------------------------------
!
   CALL timing('gzero_maker',OPR='start')
      
   IF ( .NOT. smear_alloc ) CALL errore(subname,'smearing module not allocated',1)
   !
   ALLOCATE( aux(dim,dim), z(dim,dim), STAT=ierr )
   IF (ierr/=0) CALL errore(subname,'allocating aux, z',ABS(ierr))
   ALLOCATE( w(dim), gw(dim), STAT=ierr )
   IF (ierr/=0) CALL errore(subname,'allocating w, gw',ABS(ierr))


   !
   ! If smearing_type is lorentzian, the usual technique is used, 
   ! otherwise a numerical interpolation is adopted.
   !
   SELECT CASE ( TRIM(smearing_type) )
   CASE ( 'lorentzian' )
        
        !
        ! gzero = ( omega S - H + i sigma S)^-1
        !
        gzero = CZERO
        DO i=1,dim
            gzero(i,i) = CONE
        ENDDO
        !
        DO j=1,dim 
        DO i=1,dim 
            !
            aux(i,j) = omgS_ham(i,j) + CI * smearing * S(i,j)
            !
        ENDDO
        ENDDO

        !
        ! now invert the matrix
        !
        CALL mat_sv( dim, dim , aux, gzero, IERR=ierr)
        IF (ierr/=0) CALL errore(subname,'inverting aux for lorentzian smearing',ABS(ierr))

   CASE ('gaussian')  

        !
        ! diagonalize the matrix and apply the function to the eigenvalues
        !
        CALL mat_hdiag( z, w, omgS_ham, dim) 

        !
        ! now, apply the g_smear function (numerically defined) to all eigv 
        ! this is done interpolating g_smear on the eigv 
        !
        DO i=1, dim

            CALL locate( ggrid_smear, n_smear, w(i), ig )
            !
            ! linear interpolation between ig and ig+1
            !
            g1 = g_smear(ig)
            g2 = g_smear(ig+1)
            dx = (w(i) - ggrid_smear(ig)) / dx_smear
            !
            gw(i) = CMPLX(dx, dbl) * g2 + CMPLX(ONE-dx, dbl) * g1

        ENDDO

        !
        ! gzero = z * gw * z^{dag} 
        ! first we set aux = z * gw and then aux * z^{dag} using BLAS
        !
        DO j = 1, dim
        DO i = 1, dim
             aux (i,j) = z(i,j) * gw(j)
        ENDDO
        ENDDO
        !
        CALL mat_mul( gzero, aux, 'N', z, 'C', dim, dim, dim)


   CASE DEFAULT
        CALL errore(subname,'invalid smearing_type '//TRIM(smearing_type),7)

   END SELECT


   !
   ! clean up
   !
   DEALLOCATE( aux, z, STAT=ierr )
   IF (ierr/=0) CALL errore(subname,'deallocating aux, z',ABS(ierr))
   DEALLOCATE( w, gw, STAT=ierr )
   IF (ierr/=0) CALL errore(subname,'deallocating w, gw',ABS(ierr))

   CALL timing('gzero_maker',OPR='stop')
END SUBROUTINE gzero_maker

