!
! Copyright (C) 2005 WanT Group
!
! This file is distributed under the terms of the
! GNU General Public License. See the file `License'
! in the root directory of the present distribution,
! or http://www.gnu.org/copyleft/gpl.txt .
!
!*********************************************************
SUBROUTINE unitary_update(dimwann, nkpts, dq, cU, cdu)
   !*********************************************************
   !
   ! This subroutine computes the variation in the unitary rotation U
   ! and updates it 
   !
   ! dU(k) = e^i dq(k) 
   !  U(k) = U0(k) * dU(k)
   !
   USE kinds,          ONLY : dbl
   USE constants,      ONLY : CZERO
   USE io_module,      ONLY : stdout
   USE timing_module,  ONLY : timing
   USE log_module,     ONLY : log_push, log_pop
   USE util_module,    ONLY : mat_mul, mat_hdiag, zmat_unitary
   USE parser_module,  ONLY : int2char
   USE kpoints_module, ONLY : nkpts_g, iks, ike
   USE mp,             ONLY : mp_sum

#ifdef __CHECK_UNITARY
   USE control_module, ONLY : unitary_thr
#endif
   IMPLICIT NONE

   !
   ! input variables
   !
   INTEGER,         INTENT(in)    :: dimwann, nkpts
   COMPLEX(dbl),    INTENT(in)    :: dq (dimwann,dimwann,nkpts)
   COMPLEX(dbl),    INTENT(inout) :: cU (dimwann,dimwann,nkpts_g)
   COMPLEX(dbl),    INTENT(out)   :: cdu(dimwann,dimwann,nkpts)

   !
   ! local variables
   !
   CHARACTER(14)             :: subname='unitary_update'
   INTEGER                   :: ik, ik_g, i, j, ierr
   REAL(dbl),    ALLOCATABLE :: w(:)
   COMPLEX(dbl), ALLOCATABLE :: z(:,:), cw(:)
   COMPLEX(dbl), ALLOCATABLE :: work(:,:)
   !
   ! ... end of declarations
   !

!
!-----------------------------
! routine Main body
!-----------------------------
!
   CALL timing(subname,OPR='start')
   CALL log_push(subname)


   ALLOCATE( w(dimwann), z(dimwann, dimwann), STAT=ierr )
   IF( ierr /=0 ) CALL errore(subname, 'allocating w, z', ABS(ierr))
   !
   ALLOCATE( cw(dimwann), STAT=ierr )
   IF( ierr /=0 ) CALL errore(subname, 'allocating cw', ABS(ierr))
   !
   ALLOCATE( work(dimwann,dimwann), STAT=ierr )
   IF( ierr /=0 ) CALL errore(subname, 'allocating work', ABS(ierr))

   !
   ! nullify cU for ik_g not in the current pool
   ! needed to use mp_sum for pool recovering
   !
   cU( :,:, 1:iks-1 )        = CZERO
   cU( :,:, ike+1:nkpts_g )  = CZERO

   !
   ! compute the change in the unitary matrix dU = e^(i * dq)
   !
   DO ik = 1, nkpts
        !
        ik_g = ik + iks -1
        !
        CALL mat_hdiag( z, w, dq(:,:,ik), dimwann)

        !
        ! compute dU on the spectral basis of dq
        ! cw = e^(i * w)
        !
        cw = CMPLX( COS(w), SIN(w), dbl )

        !
        ! we want to compute z * cw * z^{dag} now construct  
        ! first we set work = z * cw and then work * z^{dag} using BLAS
        !
        DO j = 1, dimwann
        DO i = 1, dimwann
             work (i,j) = z(i,j) * cw(j)
        ENDDO
        ENDDO
        !
        CALL mat_mul( cdU(:,:,ik), work, 'N', z, 'C', dimwann, dimwann, dimwann)

        !
        ! The orbitals are rotated 
        !
        CALL mat_mul( work(:,:), cU(:,:,ik_g), 'N', cdU(:,:,ik), 'N', &
                      dimwann, dimwann, dimwann )
        cU(:,:,ik_g) = work(:,:)

#ifdef __CHECK_UNITARY
        IF (  .NOT. zmat_unitary( dimwann, dimwann, cU(:,:,ik_g), &
                                  SIDE='both', TOLL=unitary_thr )  )  &
           CALL warning( subname, 'U matrix NOT unitary (II) at ikpt = '//TRIM(int2char(ik_g)))
#endif
   ENDDO
   !
   ! pool recovering
   !
   CALL mp_sum( cU )
   

   !
   ! cleaning
   !
   DEALLOCATE( w, z, STAT=ierr )
   IF( ierr /=0 ) CALL errore(subname, 'deallocating w, z', ABS(ierr))
   !
   DEALLOCATE( cw, STAT=ierr )
   IF( ierr /=0 ) CALL errore(subname, 'deallocating cw', ABS(ierr))
   !
   DEALLOCATE( work, STAT=ierr )
   IF( ierr /=0 ) CALL errore(subname, 'deallocating work', ABS(ierr))


   CALL timing(subname,OPR='stop')
   CALL log_pop(subname)
   !
END SUBROUTINE unitary_update

