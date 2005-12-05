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
   SUBROUTINE transfer( dim, niterx, tot, tott, c00, c01 )
   !***********************************************************************
   !
   !...  Iterative construction of the transfer matrix
   !     as Lopez-Sancho and Rubio, J.Phys.F:Met.Phys., v.14, 1205 (1984)
   !     and ibid. v.15, 851 (1985)
   !
   USE kinds
   USE io_global_module, ONLY : stdout
   USE constants,        ONLY : CZERO, CONE, ZERO, EPS_m7
   USE timing_module,    ONLY : timing
   USE util_module,      ONLY : mat_mul, mat_sv
   IMPLICIT NONE


      !
      ! I/O variables
      !
      INTEGER,      INTENT(in) :: dim, niterx
      COMPLEX(dbl), INTENT(in) :: c00(dim,dim)
      COMPLEX(dbl), INTENT(in) :: c01(dim,dim)
      COMPLEX(dbl), INTENT(inout) :: tot(dim,dim)
      COMPLEX(dbl), INTENT(inout) :: tott(dim,dim)


      !
      ! local variables
      !
      INTEGER      :: i, j, m, ierr
      REAL(dbl)    :: conver, conver2
      LOGICAL      :: lconverged
      COMPLEX(dbl), ALLOCATABLE :: tau(:,:,:)
      COMPLEX(dbl), ALLOCATABLE :: taut(:,:,:)
      COMPLEX(dbl), ALLOCATABLE :: tsum(:,:)
      COMPLEX(dbl), ALLOCATABLE :: tsumt(:,:)
      COMPLEX(dbl), ALLOCATABLE :: t11(:,:), t12(:,:)
      COMPLEX(dbl), ALLOCATABLE :: s1(:,:), s2(:,:)


!
!----------------------------------------
! main Body
!----------------------------------------
!
      CALL timing('transfer',OPR='start')


      ALLOCATE( tau(dim, dim, 2), taut(dim, dim, 2), STAT=ierr)
         IF (ierr/=0) CALL errore('transfer','allocating tau, taut',ABS(ierr))
      ALLOCATE( tsum(dim, dim), tsumt(dim, dim), STAT=ierr)
         IF (ierr/=0) CALL errore('transfer','allocating tsum, tsumt',ABS(ierr))
      ALLOCATE( t11(dim, dim), t12(dim, dim), STAT=ierr)
         IF (ierr/=0) CALL errore('transfer','allocating t11, t12',ABS(ierr))
      ALLOCATE( s1(dim, dim), s2(dim, dim), STAT=ierr)
         IF (ierr/=0) CALL errore('transfer','allocating s1, s2',ABS(ierr))


      !
      ! Construction of the transfer matrix
      !

      !
      ! Compute (enei * s00 - h00)^-1 and store it in t11 
      ! here c00 = h00 - ene * s00
      !
      t11(:,:) = CZERO
      t12(:,:) = -c00(:,:)

      DO i = 1, dim
          t11(i,i) = CONE
      ENDDO

      CALL mat_sv(dim, dim, t12, t11)

      !
      ! Compute intermediate t-matrices (defined as tau(dim,dim,niter)
      ! and taut(...))

      CALL mat_mul(tau(:,:,1), t11, 'N', c01, 'C', dim, dim, dim)
      CALL mat_mul(taut(:,:,1), t11, 'N', c01, 'N', dim, dim, dim)

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

         CALL mat_mul(t11, tau(:,:,1), 'N', taut(:,:,1), 'N', dim, dim, dim)
         CALL mat_mul(t12, taut(:,:,1), 'N', tau(:,:,1), 'N', dim, dim, dim)  


         s1(:,:) = -( t11(:,:) + t12(:,:) )
         s2(:,:) = CZERO

         !
         ! invert the matrix
         !
         DO i=1,dim
             s1(i,i) = CONE + s1(i,i)
             s2(i,i) = CONE
         ENDDO
         !
         CALL mat_sv(dim, dim, s1, s2, IERR=ierr)
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
              WRITE(stdout, "(2x, '         energy descarted')" )
              !
              lconverged = .TRUE.
              EXIT convergence_loop
         ENDIF


         CALL mat_mul(t11, tau(:,:,1), 'N', tau(:,:,1), 'N', dim, dim, dim)
         CALL mat_mul(t12, taut(:,:,1), 'N', taut(:,:,1), 'N', dim, dim, dim) 
         CALL mat_mul(tau(:,:,2), s2, 'N', t11, 'N', dim, dim, dim)
         CALL mat_mul(taut(:,:,2), s2, 'N', t12, 'N', dim, dim, dim)

         !
         ! Put the transfer matrices together
         !
         CALL mat_mul(t11, tsum, 'N', tau(:,:,2), 'N', dim, dim, dim)
         CALL mat_mul(s1, tsum, 'N', taut(:,:,2), 'N', dim, dim, dim)
  
         tot = tot + t11
         tsum = s1


         CALL mat_mul(t11, tsumt, 'N', taut(:,:,2), 'N', dim, dim, dim)
         CALL mat_mul(s1, tsumt, 'N', tau(:,:,2), 'N', dim, dim, dim)

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

         DO j = 1, dim
         DO i = 1, dim
              conver = conver +   REAL( tau(i,j,2) * CONJG( tau(i,j,2) )) 
              conver2 = conver2 + REAL( taut(i,j,2) * CONJG( taut(i,j,2) )) 
         ENDDO
         ENDDO
         IF ( conver < EPS_m7 .AND. conver2 < EPS_m7 ) THEN 
              lconverged = .TRUE.
              EXIT
         ENDIF

      ENDDO convergence_loop

      IF ( .NOT. lconverged ) &
          CALL errore('transfer', 'bad t-matrix convergence', 10 )

!
! ... local cleaning
!
      DEALLOCATE( tau, taut, STAT=ierr)
         IF (ierr/=0) CALL errore('transfer','deallocating tau, taut',ABS(ierr))
      DEALLOCATE( tsum, tsumt, STAT=ierr)
         IF (ierr/=0) CALL errore('transfer','deallocating tsum, tsumt',ABS(ierr))
      DEALLOCATE( t11, t12, STAT=ierr)
         IF (ierr/=0) CALL errore('transfer','deallocating t11, t12',ABS(ierr))
      DEALLOCATE( s1, s2, STAT=ierr)
         IF (ierr/=0) CALL errore('transfer','deallocating s1, s2',ABS(ierr))

      CALL timing('transfer',OPR='stop')

   END SUBROUTINE transfer

