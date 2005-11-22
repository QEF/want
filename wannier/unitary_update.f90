!
! Copyright (C) 2005 WanT Group
!
! This file is distributed under the terms of the
! GNU General Public License. See the file `License'
! in the root directory of the present distribution,
! or http://www.gnu.org/copyleft/gpl.txt .
!
!*********************************************************
SUBROUTINE unitary_update(dimwann, nkpts, dq, cu, cdu)
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
   USE util_module,    ONLY : mat_mul, mat_hdiag, zmat_unitary
   USE control_module, ONLY : unitary_thr
   IMPLICIT NONE

   !
   ! input variables
   !
   INTEGER,         INTENT(in)    :: dimwann, nkpts
   COMPLEX(dbl),    INTENT(in)    :: dq (dimwann,dimwann,nkpts)
   COMPLEX(dbl),    INTENT(inout) :: cu (dimwann,dimwann,nkpts)
   COMPLEX(dbl),    INTENT(out)   :: cdu(dimwann,dimwann,nkpts)

   !
   ! local variables
   !
   INTEGER                   :: ik, i, j, ierr
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
   CALL timing('unitary_update',OPR='start')


   ALLOCATE( w(dimwann), z(dimwann, dimwann), STAT=ierr )
      IF( ierr /=0 ) CALL errore('unitary_update', 'allocating w, z', ABS(ierr))
   ALLOCATE( cw(dimwann), STAT=ierr )
      IF( ierr /=0 ) CALL errore('unitary_update', 'allocating cw', ABS(ierr))
   ALLOCATE( work(dimwann,dimwann), STAT=ierr )
      IF( ierr /=0 ) CALL errore('unitary_update', 'allocating work', ABS(ierr))

   !
   ! compute the change in the unitary matrix dU = e^(i * dq)
   !
   DO ik = 1, nkpts

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
        CALL mat_mul( work(:,:), cu(:,:,ik), 'N', cdU(:,:,ik), 'N', &
                      dimwann, dimwann, dimwann )
        cu(:,:,ik) = work(:,:)

#ifdef __CHECK_UNITARY
        IF (  .NOT. zmat_unitary( dimwann, dimwann, cu(:,:,ik), &
                                  SIDE='both', TOLL=unitary_thr )  )  &
           WRITE (stdout,"(2x,'WARNING: U matrix NOT unitary (II) at ikpt = ',i4)")ik
#endif
   ENDDO
   

   !
   ! cleaning
   !
   DEALLOCATE( w, z, STAT=ierr )
      IF( ierr /=0 ) CALL errore('unitary_update', 'deallocating w, z', ABS(ierr))
   DEALLOCATE( cw, STAT=ierr )
      IF( ierr /=0 ) CALL errore('unitary_update', 'deallocating cw', ABS(ierr))
   DEALLOCATE( work, STAT=ierr )
      IF( ierr /=0 ) CALL errore('unitary_update', 'deallocating work', ABS(ierr))


   CALL timing('unitary_update',OPR='stop')
END SUBROUTINE unitary_update

