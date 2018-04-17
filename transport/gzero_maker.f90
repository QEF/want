! 
! Copyright (C) 2005 WanT Group
! 
! This file is distributed under the terms of the 
! GNU General Public License. See the file `License' 
! in the root directory of the present distribution, 
! or http://www.gnu.org/copyleft/gpl.txt . 
!
!*****************************************************************
   SUBROUTINE gzero_maker( ndim, opr, gzero, calc )
   !*****************************************************************
   !
   ! Given \omega S - H, the routine compute the non-interacting 
   ! Green func Gzero (when calc == "direct" ) or Gzero^-1 (when calc == "inverse")
   ! according to the input smearing type.
   !
   ! the implemented formula is
   ! gzero    = g_smear( omg S -H )
   ! gzero^-1 = 1/g_smear( omg S -H )
   !
   ! where f is numerically computed (according to smearing type and smearing) in
   ! smearing_module. See the module file for more details.
   !
   ! NOTE: \omega *S - H is hermitean  (no immaginary part included)
   !
   USE kinds
   USE constants,              ONLY : ONE, CONE, CZERO, CI, PI, EPS_m8
   USE timing_module,          ONLY : timing
   USE log_module,             ONLY : log_push, log_pop
   USE util_module,            ONLY : mat_hdiag, mat_mul, mat_inv, zmat_is_herm
   USE T_smearing_module,      ONLY : smear_alloc => alloc, delta, smearing_type, g_smear, &
                                      xgrid_smear => xgrid, &
                                      nx_smear    => nx,    &
                                      dx_smear    => dx
   USE T_operator_blc_module
   !
   IMPLICIT NONE 

   !  
   ! input variables 
   !  
   INTEGER,             INTENT(IN)  :: ndim
   TYPE( operator_blc), INTENT(IN)  :: opr
   COMPLEX(dbl),        INTENT(OUT) :: gzero(ndim,ndim)
   CHARACTER(*),        INTENT(IN)  :: calc
   !
   ! local variables
   !
   INTEGER         :: i, j, ig, ik, ierr
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
   CALL timing(subname,OPR='start')
   CALL log_push(subname)
      
   IF ( .NOT. smear_alloc ) CALL errore(subname,'smearing module not allocated',1)
   IF ( .NOT. opr%alloc )   CALL errore(subname,'opr not alloc',1)
   !
   ALLOCATE( aux(ndim,ndim), z(ndim,ndim), STAT=ierr )
   IF (ierr/=0) CALL errore(subname,'allocating aux, z',ABS(ierr))
   ALLOCATE( w(ndim), gw(ndim), STAT=ierr )
   IF (ierr/=0) CALL errore(subname,'allocating w, gw',ABS(ierr))



   !
   ! If smearing_type is lorentzian, the usual technique is used, 
   ! otherwise a numerical interpolation is adopted.
   !
   ! opr%aux = \omega S - H    at the opr%ik kpt
   !
   ik = opr%ik
   !
   SELECT CASE ( TRIM(smearing_type) )
   CASE ( 'lorentzian' )
        
        !
        ! gzero = ( omega S - H + i delta S)^-1
        !
        aux(:,:) = opr%aux(:,:) + CI * delta * opr%S(:,:,ik)
        !
        SELECT CASE (TRIM(calc))
        CASE ("direct")
            ! calculate the gzero function
            !
            CALL mat_inv( ndim, aux, gzero, IERR=ierr)
            IF (ierr/=0) CALL errore(subname,'inverting aux for lorentzian smearing',ABS(ierr))
            !
        CASE ("inverse")
            ! calculate the gzero^{-1} function
            !
            gzero = aux
            !
        CASE DEFAULT
            CALL errore(subname, 'invalid calculation = '//TRIM(calc), 5)
        END SELECT
        

   CASE DEFAULT
        !
        ! opr%aux = \omega S - H    at the opr%ik kpt
        ! 
        aux =   opr%aux(:,:)

        !
        ! This part is incompatible with having non-hermitean
        ! correlation. A check is performed and an in case an error is issued
        !
        IF ( .NOT. zmat_is_herm( ndim, aux, TOLL=EPS_m8 ) ) &
            CALL errore(subname,'non hermitean opr is incompatible',10)
        ! 
        !
        ! Numeric smearing:
        ! diagonalize the matrix and apply 
        ! the function to the eigenvalues
        !
        CALL mat_hdiag( z, w, aux, ndim) 
        w(:) = w(:)/delta

        !
        ! now, apply the g_smear function (numerically defined) to all eigv 
        ! this is done interpolating g_smear on the eigv 
        !
        DO i=1, ndim

            CALL locate( xgrid_smear, nx_smear, w(i), ig )

            !
            ! treat the case of eigenvalue out of the grid
            !
            IF ( ig == 0 .OR. ig == nx_smear ) THEN 
                !
                ! all functions are equal;
                ! use the lorentzian broad anyway
                !
                SELECT CASE ( (TRIM(calc)) )
                CASE ( "direct" )
                    ! calculate the gzero function
                    !
                    gw(i) = REAL( CONE / ( delta * ( w(i) + CI) ), KIND=dbl )
                CASE ( "inverse" )
                    ! calculate the gzero^{-1} function
                    !
                    gw(i) = w(i) * delta
                    !
                CASE DEFAULT
                    CALL errore(subname, 'invalid calculation = '//TRIM(calc), 7)
                END SELECT 

            ELSE
                !
                ! numerically evaluate the function
                !
                g1 = CZERO
                g2 = CZERO
                !
                SELECT CASE (TRIM(calc))
                CASE ( "direct" )
                    ! calculate the gzero function
                    !
                    g1 = g_smear(ig)
                    g2 = g_smear(ig+1)
                    !
                CASE ( "inverse" )
                    ! calculate the gzero^{-1} function
                    !
                    g1 = CONE/g_smear(ig)
                    g2 = CONE/g_smear(ig+1)
                    !
                CASE DEFAULT
                    CALL errore(subname, 'invalid calculation = '//TRIM(calc), 7)
                END SELECT
                !
                ! linear interpolation between ig and ig+1
                ! 
                dx = (w(i) - xgrid_smear(ig)) / dx_smear
                !
                gw(i) = dx * g2 + (ONE-dx) * g1

            ENDIF
        ENDDO

        !
        ! gzero = z * gw * z^{dag} 
        ! first we set aux = z * gw and then aux * z^{dag} using BLAS
        !
        DO j = 1, ndim
        DO i = 1, ndim
             aux (i,j) = z(i,j) * gw(j)
        ENDDO
        ENDDO
        !
        CALL mat_mul( gzero, aux, 'N', z, 'C', ndim, ndim, ndim)

   END SELECT
   !
   ! clean up
   !
   DEALLOCATE( aux, z, STAT=ierr )
   IF (ierr/=0) CALL errore(subname,'deallocating aux, z',ABS(ierr))
   DEALLOCATE( w, gw, STAT=ierr )
   IF (ierr/=0) CALL errore(subname,'deallocating w, gw',ABS(ierr))

   CALL timing(subname,OPR='stop')
   CALL log_pop(subname)
   !
END SUBROUTINE gzero_maker

