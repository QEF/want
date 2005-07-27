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
   SUBROUTINE transfer( nmax, nterx, tot, tott, c00, c01 )
   !***********************************************************************
   !
   !...  Iterative construction of the transfer matrix
   !     as Lopez-Sancho and Rubio, J.Phys.F:Met.Phys., v.14, 1205 (1984)
   !     and ibid. v.15, 851 (1985)
   !
   USE kinds
   USE constants, ONLY : CZERO, CONE, ZERO, EPS_m7
   USE timing_module, ONLY : timing
   IMPLICIT NONE


      !
      ! I/O variables
      !
      INTEGER,      INTENT(in) :: nmax, nterx
      COMPLEX(dbl), INTENT(in) :: c00(nmax,nmax)
      COMPLEX(dbl), INTENT(in) :: c01(nmax,nmax)
      COMPLEX(dbl), INTENT(inout) :: tot(nmax,nmax)
      COMPLEX(dbl), INTENT(inout) :: tott(nmax,nmax)


      !
      ! local variables
      !
      INTEGER      :: i, j, k, l, m, info, ierr
      REAL(dbl)    :: conver, conver2
      LOGICAL      :: lconverged
      INTEGER,      ALLOCATABLE :: ipiv(:)
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


      ALLOCATE( ipiv(nmax), STAT=ierr)
         IF (ierr/=0) CALL errore('transfer','allocating ipiv',ABS(ierr))
      ALLOCATE( tau(nmax, nmax, 2), taut(nmax, nmax, 2), STAT=ierr)
         IF (ierr/=0) CALL errore('transfer','allocating tau, taut',ABS(ierr))
      ALLOCATE( tsum(nmax, nmax), tsumt(nmax, nmax), STAT=ierr)
         IF (ierr/=0) CALL errore('transfer','allocating tsum, tsumt',ABS(ierr))
      ALLOCATE( t11(nmax, nmax), t12(nmax, nmax), STAT=ierr)
         IF (ierr/=0) CALL errore('transfer','allocating t11, t12',ABS(ierr))
      ALLOCATE( s1(nmax, nmax), s2(nmax, nmax), STAT=ierr)
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

      DO i = 1, nmax
          t11(i,i) = CONE
      ENDDO

      CALL ZGESV( nmax, nmax, t12, nmax, ipiv, t11, nmax, info )
      IF ( info /= 0 )  CALL errore(' Stransfreb ', ' ZGESV (I) ', info )

      !
      ! Compute intermediate t-matrices (defined as tau(nmax,nmax,niter)
      ! and taut(...))

      CALL ZGEMM( 'N', 'C', nmax, nmax, nmax, CONE, t11, nmax, c01, nmax,    &
                  CZERO, tau, nmax )
      CALL ZGEMM( 'N', 'N', nmax, nmax, nmax, CONE, t11, nmax, c01, nmax,    &
                  CZERO, taut, nmax )

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
      DO m = 1, nterx

         CALL ZGEMM( 'N', 'N', nmax, nmax, nmax, CONE, tau, nmax, taut, nmax, &
                     CZERO, t11, nmax )
         CALL ZGEMM( 'N', 'N', nmax, nmax, nmax, CONE, taut, nmax, tau, nmax, &
                     CZERO, t12, nmax )


         s1(:,:) = -( t11(:,:) + t12(:,:) )
         s2(:,:) = CZERO

         DO i=1,nmax
             s1(i,i) = CONE + s1(i,i)
             s2(i,i) = CONE
         ENDDO


         CALL ZGESV( nmax, nmax, s1, nmax, ipiv, s2, nmax, info )
         IF ( info /= 0 )  CALL errore(' Stransfreb ', ' ZGESV (II) ', info )


         CALL ZGEMM( 'N', 'N', nmax, nmax, nmax, CONE, tau(1,1,1), nmax, tau(1,1,1), nmax, &
                     CZERO, t11, nmax )
         CALL ZGEMM( 'N', 'N', nmax, nmax, nmax, CONE, taut(1,1,1), nmax, taut(1,1,1), nmax,&
                     CZERO, t12, nmax )
         CALL ZGEMM( 'N', 'N', nmax, nmax, nmax, CONE, s2, nmax, t11, nmax, &
                     CZERO, tau(1,1,2), nmax )
         CALL ZGEMM( 'N', 'N', nmax, nmax, nmax, CONE, s2, nmax, t12, nmax, &
                     CZERO, taut(1,1,2), nmax )

         !
         ! Put the transfer matrices together
         !
         CALL ZGEMM( 'N', 'N', nmax, nmax, nmax, CONE, tsum, nmax, tau(1,1,2), nmax,    &
                     CZERO, t11, nmax )
         CALL ZGEMM( 'N', 'N', nmax, nmax, nmax, CONE, tsum, nmax, taut(1,1,2), nmax,   & 
                     CZERO, s1, nmax )
         CALL ZCOPY( nmax*nmax, t11, 1, s2, 1 )
         CALL ZAXPY( nmax*nmax, CONE, tot, 1, s2, 1 )

         tot(:,:) = s2(:,:)
         tsum(:,:) = s1(:,:)


         CALL ZGEMM( 'N', 'N', nmax, nmax, nmax, CONE, tsumt, nmax, taut(1,1,2), nmax, &
                     CZERO, t11, nmax )
         CALL ZGEMM( 'N', 'N', nmax, nmax, nmax, CONE, tsumt, nmax, tau(1,1,2), nmax, &
                     CZERO, s1, nmax )
         CALL ZCOPY( nmax*nmax, t11, 1, s2, 1 )
         CALL ZAXPY( nmax*nmax, CONE, tott, 1, s2, 1 )

         tott(:,:) = s2(:,:)
         tsumt(:,:) = s1(:,:)
         tau(:,:,1) = tau(:,:,2)
         taut(:,:,1) = taut(:,:,2)


         !
         ! Convergence chech on the t-matrices
         !
         conver = ZERO
         conver2 = ZERO

         DO j = 1, nmax
         DO i = 1, nmax
              conver = conver + SQRT( REAL( tau(i,j,2) * CONJG( tau(i,j,2) )) )
              conver2 = conver2 + SQRT( REAL( taut(i,j,2) * CONJG( taut(i,j,2) )) )
         ENDDO
         ENDDO
         IF ( conver < EPS_m7 .AND. conver2 < EPS_m7 ) THEN 
              lconverged = .TRUE.
              EXIT
         ENDIF

      ENDDO 

      IF ( .NOT. lconverged ) &
          CALL errore('transfreb', 'bad t-matrix convergence', 10 )

!
! ... local cleaning
!
      DEALLOCATE( ipiv, STAT=ierr)
         IF (ierr/=0) CALL errore('transfer','deallocating ipiv',ABS(ierr))
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

