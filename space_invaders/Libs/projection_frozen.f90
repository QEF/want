!
! Copyright (C) 2004 Arrigo Calzolari, Carlo Cavazzoni, Marco Buongiorno Nardelli
! Copyright (C) 2002 Nicola Marzari, Ivo Souza, David Vanderbilt
!
! This file is distributed under the terms of the
! GNU General Public License. See the file `License'
! in the root directory of the present distribution,
! or http://www.gnu.org/copyleft/gpl.txt .
!
!=----------------------------------------------------------------------------------=
       SUBROUTINE projection_frozen( lamp, dimwann, dimwin,  &
                  dimfroz, frozen, nkpts, mxdbnd, mxdnrk)
!=----------------------------------------------------------------------------------=

       USE kinds
       USE constants, ONLY : ZERO, CZERO, ONE
       USE timing_module, ONLY : timing
       USE io_module, ONLY : stdout

       IMPLICIT NONE

       INTEGER :: mxdbnd 
       INTEGER :: mxdnrk 
       INTEGER :: dimwann 
       INTEGER :: nkpts
       INTEGER :: dimwin(mxdnrk) 
       INTEGER :: dimfroz(mxdnrk)
       COMPLEX(dbl) :: lamp(mxdbnd,mxdbnd,mxdnrk)
       LOGICAL :: frozen(mxdbnd,mxdnrk)

       COMPLEX(dbl), ALLOCATABLE :: ap(:)
       COMPLEX(dbl), ALLOCATABLE :: z(:,:)
       COMPLEX(dbl), ALLOCATABLE :: work(:)
       COMPLEX(dbl), ALLOCATABLE :: p_s(:,:)
       COMPLEX(dbl), ALLOCATABLE :: q_froz(:,:)
       COMPLEX(dbl), ALLOCATABLE :: pq(:,:)
       COMPLEX(dbl), ALLOCATABLE :: qpq(:,:)
       INTEGER, ALLOCATABLE :: iwork(:), ifail(:)
       REAL(dbl), ALLOCATABLE :: w(:) ,rwork(:)
 
       INTEGER :: nkp, j, l, n
       INTEGER :: info
       INTEGER :: m, il, iu, ierr
       COMPLEX(dbl) :: ctmp

! ...  End of declarations

       CALL timing('projection_frozen',OPR='start')
 
       ALLOCATE( ap((mxdbnd*(mxdbnd+1))/2), STAT = ierr )
       IF( ierr /= 0 ) THEN
         CALL errore( ' projection_frozen ', ' allocating ap ', (mxdbnd*(mxdbnd+1))/2 )
       END IF

       ALLOCATE( z(mxdbnd,mxdbnd), STAT = ierr )
       IF( ierr /= 0 ) THEN
         CALL errore( ' projection_frozen ', ' allocating z ', (mxdbnd*mxdbnd) )
       END IF

       ALLOCATE( work(2*mxdbnd), STAT = ierr )
       IF( ierr /= 0 ) THEN
         CALL errore( ' projection_frozen ', ' allocating work ', (mxdbnd*mxdbnd) )
       END IF

       ALLOCATE( p_s(mxdbnd,mxdbnd), STAT = ierr )
       IF( ierr /= 0 ) THEN
         CALL errore( ' projection_frozen ', ' allocating p_s ', (mxdbnd*mxdbnd) )
       END IF

       ALLOCATE( q_froz(mxdbnd,mxdbnd), STAT = ierr )
       IF( ierr /= 0 ) THEN
         CALL errore( ' projection_frozen ', ' allocating q_froz ', (mxdbnd*mxdbnd) )
       END IF

       ALLOCATE( pq(mxdbnd,mxdbnd), STAT = ierr )
       IF( ierr /= 0 ) THEN
         CALL errore( ' projection_frozen ', ' allocating pq ', (mxdbnd*mxdbnd) )
       END IF

       ALLOCATE( qpq(mxdbnd,mxdbnd), STAT = ierr )
       IF( ierr /= 0 ) THEN
         CALL errore( ' projection_frozen ', ' allocating qpq ', (mxdbnd*mxdbnd) )
       END IF

       ALLOCATE( iwork(5*mxdbnd), STAT = ierr )
       IF( ierr /= 0 ) THEN
         CALL errore( ' projection_frozen ', ' allocating iwork ', (5*mxdbnd) )
       END IF

       ALLOCATE( ifail(mxdbnd), STAT = ierr )
       IF( ierr /= 0 ) THEN
         CALL errore( ' projection_frozen ', ' allocating ifail ', (mxdbnd) )
       END IF

       ALLOCATE( w(mxdbnd), STAT = ierr )
       IF( ierr /= 0 ) THEN
         CALL errore( ' projection_frozen ', ' allocating w ', (mxdbnd) )
       END IF

       ALLOCATE( rwork(7*mxdbnd), STAT = ierr )
       IF( ierr /= 0 ) THEN
         CALL errore( ' projection_frozen ', ' allocating rwork ', 7*mxdbnd )
       END IF


       DO nkp =1, nkpts
 
! ...    If there are less frozen states than the target number of bands at the 
!        present k-point, compute the dimwann-dimfroz(nkp) leading eigenvectors of the matrix qpq
 
         IF ( dimwann > dimfroz(nkp) ) THEN
 
           DO n = 1, dimwin(nkp)
             DO m = 1, dimwin(nkp)
               q_froz(m,n) = czero
               p_s(m,n) = czero
               DO l = 1, dimwann
                 p_s(m,n) = p_s(m,n) + lamp(m,l,nkp) * CONJG( lamp(n,l,nkp) )
               END DO
             END DO
             IF( frozen(n,nkp) .EQV. .false. )  q_froz(n,n) = CMPLX( 1.0d0, 0.0d0 )
           END DO
 
           DO n = 1, dimwin(nkp)
             DO m = 1, dimwin(nkp)
               pq(m,n) = czero
               DO l = 1, dimwin(nkp)
                 pq(m,n) = pq(m,n) + p_s(m,l) * q_froz(l,n)
               END DO
             END DO
           END DO

           DO n = 1, dimwin(nkp)
             DO m = 1, dimwin(nkp)
               qpq(m,n) = czero
               DO l = 1, dimwin(nkp)
                 qpq(m,n) = qpq(m,n) + q_froz(m,l) * pq(l,n)
               END DO
             END DO
           END DO


! ...      Check hermiticity of qpq

           DO n = 1, dimwin(nkp)
             DO m = 1, n
               ap( m + (n-1) * n / 2 ) = qpq(m,n)
             END DO
           END DO

           il = dimwin(nkp) - ( dimwann - dimfroz(nkp) ) + 1
           iu = dimwin(nkp)

           CALL zhpevx( 'v', 'a', 'u', dimwin(nkp), ap(1), ZERO, ZERO, il, iu,     &
                -ONE, m, w(1), z(1,1), mxdbnd, work(1), rwork(1), iwork(1), ifail(1), info )

           IF ( INFO < 0 ) THEN
             CALL errore(' projection_frozen ', ' the info argument of zhpevx had an illegal value ', info )
           ELSE IF ( INFO > 0 ) THEN
             CALL errore(' projection_frozen ', ' eigenvectors failed to converge ', info )
           END IF

           IF( m /= dimwin(nkp) ) &
             CALL errore(' projection_frozen ', ' number of eigenstates different from required ', m )
         
! ...      Pick the dimwann-dimfroz(nkp) leading eigenvectors to be trial states; 
!          put them right after the frozen states in lamp

           DO l = dimfroz(nkp) + 1, dimwann
             DO j = 1, dimwin(nkp)
               lamp(j,l,nkp) = z(j,il)  
             END DO
             il = il + 1   
           END DO

           IF( il - 1 /= iu ) CALL errore(' projection_frozen ', ' check failed ', il )

           DO l = dimfroz(nkp) + 1, dimwann
             DO m = dimfroz(nkp) + 1, l
               ctmp = czero
               DO j = 1, dimwin(nkp)
                 ctmp = ctmp + CONJG( lamp(j,m,nkp) ) * lamp(j,l,nkp)
               END DO
!              WRITE(stdout ,'(i2,2x,i2,f16.12,1x,f16.12)') l, m, ctmp
               IF ( l == m ) THEN
                 IF ( ABS( ctmp - CMPLX( 1.0d0, 0.0d0 ) ) > 1.0e-8 ) &
                   CALL errore(' projection_frozen ', 'projected gaussians in lamp not orthonormal (I)',l)
               ELSE
                 IF ( ABS(ctmp) > 1.0e-8 ) &
                   CALL errore(' projection_frozen ', 'projected gaussians in lamp not orthonormal (II)',l)
               END IF
             END DO
           END DO

         END IF ! DIMWANN>DIMFROZ(NKP)

       END DO ! NKP

       DEALLOCATE( ap, STAT=ierr )
          IF (ierr/=0) CALL errore('projection_frozen','deallocating ap',ABS(ierr))
       DEALLOCATE( z, STAT=ierr )
          IF (ierr/=0) CALL errore('projection_frozen','deallocating z',ABS(ierr))
       DEALLOCATE( work, STAT=ierr )
          IF (ierr/=0) CALL errore('projection_frozen','deallocating work',ABS(ierr))
       DEALLOCATE( p_s, STAT=ierr )
          IF (ierr/=0) CALL errore('projection_frozen','deallocating p_s',ABS(ierr))
       DEALLOCATE( q_froz, STAT=ierr )
          IF (ierr/=0) CALL errore('projection_frozen','deallocating q_froz',ABS(ierr))
       DEALLOCATE( pq, STAT=ierr )
          IF (ierr/=0) CALL errore('projection_frozen','deallocating pq',ABS(ierr))
       DEALLOCATE( qpq, STAT=ierr )
          IF (ierr/=0) CALL errore('projection_frozen','deallocating qpq',ABS(ierr))
       DEALLOCATE( iwork, STAT=ierr )
          IF (ierr/=0) CALL errore('projection_frozen','deallocating iwork',ABS(ierr))
       DEALLOCATE( ifail, STAT=ierr )
          IF (ierr/=0) CALL errore('projection_frozen','deallocating ifail',ABS(ierr))
       DEALLOCATE( w, STAT=ierr )
          IF (ierr/=0) CALL errore('projection_frozen','deallocating w',ABS(ierr))
       DEALLOCATE( rwork, STAT=ierr )
          IF (ierr/=0) CALL errore('projection_frozen','deallocating rwork',ABS(ierr))

       CALL timing('projection_frozen',OPR='stop')

       RETURN
       END SUBROUTINE







