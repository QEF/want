!
!      Copyright (C) 2004 WanT Group
!      Copyright (C) 1999 Marco Buongiorno Nardelli
!
!      This file is distributed under the terms of the
!      GNU General Public License. See the file `License'
!      in the root directory of the present distribution,
!      or http://www.gnu.org/copyleft/gpl.txt .
!
!***********************************************************************
   SUBROUTINE transfer( hdim, niterx, tot, tott, c00, c01, niter, ierr )
   !***********************************************************************
   !
   !...  Iterative construction of the transfer matrix
   !     as Lopez-Sancho and Rubio, J.Phys.F:Met.Phys., v.14, 1205 (1984)
   !     and ibid. v.15, 851 (1985)
   !
   ! these are "effective quantities", see define_smear_ham.f90
   !     c00 = ene * S00 - H00_eff       
   !     c01 = ene * S01 - H01_eff 
   !
   ! where "eff" means including the smearing
   !
   USE kinds
   USE io_global_module,  ONLY : stdout
   USE constants,         ONLY : CZERO, CONE, ZERO, EPS_m7, CI
   USE timing_module,     ONLY : timing
   USE util_module,       ONLY : mat_mul, mat_sv
   USE T_smearing_module, ONLY : delta
   IMPLICIT NONE


      !
      ! I/O variables
      !
      INTEGER,      INTENT(in)    :: hdim, niterx
      COMPLEX(dbl), INTENT(in)    :: c00(hdim,hdim)
      COMPLEX(dbl), INTENT(in)    :: c01(hdim,hdim)
      COMPLEX(dbl), INTENT(inout) :: tot(hdim,hdim)
      COMPLEX(dbl), INTENT(inout) :: tott(hdim,hdim)
      INTEGER,      INTENT(out)   :: niter, ierr



      !
      ! local variables
      !
      INTEGER      :: i, j, m, ierrl
      REAL(dbl)    :: conver, conver2
      LOGICAL      :: lconverged
      COMPLEX(dbl), ALLOCATABLE :: tau(:,:,:)
      COMPLEX(dbl), ALLOCATABLE :: taut(:,:,:)
      COMPLEX(dbl), ALLOCATABLE :: tsum(:,:)
      COMPLEX(dbl), ALLOCATABLE :: tsumt(:,:)
      COMPLEX(dbl), ALLOCATABLE :: t11(:,:), t12(:,:)
      COMPLEX(dbl), ALLOCATABLE :: s1(:,:), s2(:,:)
      CHARACTER(8)              :: subname = 'transfer'

!
!----------------------------------------
! main Body
!----------------------------------------
!
      CALL timing('transfer',OPR='start')
      !
      ierr = 0
      !
      ALLOCATE( tau(hdim, hdim, 2), taut(hdim, hdim, 2), STAT=ierrl)
         IF (ierrl/=0) CALL errore(subname,'allocating tau, taut',ABS(ierrl))
      ALLOCATE( tsum(hdim, hdim), tsumt(hdim, hdim), STAT=ierrl)
         IF (ierrl/=0) CALL errore(subname,'allocating tsum, tsumt',ABS(ierrl))
      ALLOCATE( t11(hdim, hdim), t12(hdim, hdim), STAT=ierrl)
         IF (ierrl/=0) CALL errore(subname,'allocating t11, t12',ABS(ierrl))
      ALLOCATE( s1(hdim, hdim), s2(hdim, hdim), STAT=ierrl)
         IF (ierrl/=0) CALL errore(subname,'allocating s1, s2',ABS(ierrl))


      !
      ! Compute (ene * s00 - h00 + "smear")^-1 and store it in t11 
      ! see the routine header for the definition of "smear"
      !
      t11(:,:) = CZERO
      t12(:,:) = c00(:,:)

      DO i = 1, hdim
          t11(i,i) = CONE
      ENDDO

      CALL mat_sv(hdim, hdim, t12, t11)

      !
      ! Compute intermediate t-matrices (defined as tau(hdim,hdim,niter)
      ! and taut(...))

      CALL mat_mul(tau(:,:,1),  t11, 'N', c01, 'C', hdim, hdim, hdim)
      CALL mat_mul(taut(:,:,1), t11, 'N', c01, 'N', hdim, hdim, hdim)

      !
      ! Initialize T
      !
      tot ( :, :) = tau ( :, :, 1)
      tsum( :, :) = taut( :, :, 1)

      !
      ! Initialize T^bar
      !
      tott(:,:) = taut(:,:,1)
      tsumt(:,:) = tau(:,:,1)


      !
      ! Main loop
      !
      lconverged = .FALSE.

      convergence_loop: &
      DO m = 1, niterx

         CALL mat_mul(t11, tau(:,:,1),  'N', taut(:,:,1), 'N', hdim, hdim, hdim)
         CALL mat_mul(t12, taut(:,:,1), 'N', tau(:,:,1),  'N', hdim, hdim, hdim)  


         s1(:,:) = -( t11(:,:) + t12(:,:) )
         s2(:,:) = CZERO

         !
         ! invert the matrix
         !
         DO i=1,hdim
             s1(i,i) = CONE + s1(i,i)
             s2(i,i) = CONE
         ENDDO
         !
         CALL mat_sv(hdim, hdim, s1, s2, IERR=ierr)
         !
         ! exit the main loop, 
         ! set all the matrices to be computed to zero
         ! and print a warning
         !
         IF ( ierr/=0 ) THEN
              !
              tot  = CZERO
              tott = CZERO
              !
              WRITE(stdout, "(2x, 'WARNING: singular matrix at iteration', i4)" ) m
              !
              lconverged = .TRUE.
              EXIT convergence_loop
              !
         ENDIF


         CALL mat_mul(t11, tau(:,:,1),  'N', tau(:,:,1),  'N', hdim, hdim, hdim)
         CALL mat_mul(t12, taut(:,:,1), 'N', taut(:,:,1), 'N', hdim, hdim, hdim) 
         CALL mat_mul(tau(:,:,2), s2,   'N', t11, 'N', hdim, hdim, hdim)
         CALL mat_mul(taut(:,:,2), s2,  'N', t12, 'N', hdim, hdim, hdim)

         !
         ! Put the transfer matrices together
         !
         CALL mat_mul(t11, tsum,  'N', tau(:,:,2),  'N', hdim, hdim, hdim)
         CALL mat_mul(s1, tsum,   'N', taut(:,:,2), 'N', hdim, hdim, hdim)
  
         tot = tot + t11
         tsum = s1


         CALL mat_mul(t11, tsumt, 'N', taut(:,:,2), 'N', hdim, hdim, hdim)
         CALL mat_mul(s1, tsumt,  'N', tau(:,:,2),  'N', hdim, hdim, hdim)

         tott  = tott + t11
         tsumt = s1
         !
         tau(:,:,1) = tau(:,:,2)
         taut(:,:,1) = taut(:,:,2)


         !
         ! Convergence chech on the t-matrices
         !
         conver = ZERO
         conver2 = ZERO

         DO j = 1, hdim
         DO i = 1, hdim
              conver = conver +   REAL( tau(i,j,2) * CONJG( tau(i,j,2) )) 
              conver2 = conver2 + REAL( taut(i,j,2) * CONJG( taut(i,j,2) )) 
         ENDDO
         ENDDO
         IF ( conver < EPS_m7 .AND. conver2 < EPS_m7 ) THEN 
              lconverged = .TRUE.
              EXIT
         ENDIF
         niter = m

      ENDDO convergence_loop

      ! 
      ! if not converged, print a WARNING but do not crash
      ! 
      IF ( .NOT. lconverged ) THEN 
          !
          ! de-comment here if you want to allow for a hard crash
          ! CALL errore(subname, 'bad t-matrix convergence', 10 )
          !
          ierr = niterx
          !
          tot  = CZERO
          tott = CZERO
          !
          WRITE(stdout, "(2x, 'WARNING: t-matrix not converged after', i4, ' iterations')" ) niterx
          !
      ENDIF

!
! ... local cleaning
!
      DEALLOCATE( tau, taut, STAT=ierrl)
         IF (ierrl/=0) CALL errore(subname,'deallocating tau, taut',ABS(ierrl))
      DEALLOCATE( tsum, tsumt, STAT=ierrl)
         IF (ierrl/=0) CALL errore(subname,'deallocating tsum, tsumt',ABS(ierrl))
      DEALLOCATE( t11, t12, STAT=ierrl)
         IF (ierrl/=0) CALL errore(subname,'deallocating t11, t12',ABS(ierrl))
      DEALLOCATE( s1, s2, STAT=ierrl)
         IF (ierrl/=0) CALL errore(subname,'deallocating s1, s2',ABS(ierrl))

      CALL timing('transfer',OPR='stop')

   END SUBROUTINE transfer

