       SUBROUTINE projection_frozen( lamp, dimwann, dimwin,  &
                  dimfroz, frozen, nkpts, mxdbnd, mxdnrk)



!.............................................................................
! Computes the leading eigenvectors of q_FROZ . p_S . q_FROZ, where p_S is the
! projector operator onto the subspace s of the projected gaussians, p_FROZ iS
! the projector onto the frozen states, and q_FROZ = 1 - p_FROZ, all expressed
! in the basis of the bloch eigenstates inside the outer energy window
! written by ivo souza 30 jan 2001
! INPUT:
!
! LAMP              OUTPUT FROM THE SUBROUTINE PROJECTION (GAUSSIANS PROJECTED
!                   ONTO THE STATES INSIDE THE OUTER WINDOW)
! DIMWANN           dimensionality of the subspace at each k-point 
!                   (number of Wannier functions per unit cell that we want)
! DIMWIN(NKP)       number of bands at the nkp-th k-point that fall 
!                   within the outer energy window               
! DIMFROZ(NKP)      number of bands at the nkp-th k-point that fall 
!                   within the inner energy window               
! FROZEN(I,NKP)     TRUE IF THE I-TH BAND (W.R.T. THE LOWEST BAND INSIDE 
!                   OUTER WINDOW) AT THE NKP-TH K-POINT IS FROZEN, FALSE
!                   OTHERWISE
! NKPTS             total number of k-points in the mesh
! MXDBND            ARRAY DIMENSION FOR BANDS
! MXDNRK            ARRAY DIMENSION FOR K-POINTS
!
!
! OUTPUT:
!
! LAMP(J,L,NKP)     AT THOSE K-POINTS WHERE DIMWANN>DIMFROZ(NKP):
!                   AMPLITUDE OF THE J-TH ENERGY EIGENVECTOR INSIDE THE OUTER
!                   ENERGY WINDOW AT THE NKP-TH K-POINT IN THE EXPANSION OF 
!                   THE L-TH TRIAL LAMBDA EIGENVECTOR AT THE SAME K-POINT
!                   (ONLY THE TRIAL EIGENVECTORS OUTSIDE THE SPACE OF FROZEN
!                   STATES  ARE COMPUTED HERE, I.E., FROM L=DIMFROZ(NKP)+1 TO
!                   DIMWANN)
!
!.............................................................................


       IMPLICIT NONE

       INTEGER :: mxdbnd 
       INTEGER :: mxdnrk 
       INTEGER :: dimwann 
       INTEGER :: nkpts
       INTEGER :: dimwin(mxdnrk) 
       INTEGER :: dimfroz(mxdnrk)
       COMPLEX*16 :: lamp(mxdbnd,mxdbnd,mxdnrk)
       LOGICAL :: frozen(mxdbnd,mxdnrk)

       COMPLEX*16, ALLOCATABLE :: ap(:)
       COMPLEX*16, ALLOCATABLE :: z(:,:)
       COMPLEX*16, ALLOCATABLE :: work(:)
       COMPLEX*16, ALLOCATABLE :: p_s(:,:)
       COMPLEX*16, ALLOCATABLE :: q_froz(:,:)
       COMPLEX*16, ALLOCATABLE :: pq(:,:)
       COMPLEX*16, ALLOCATABLE :: qpq(:,:)
       INTEGER, ALLOCATABLE :: iwork(:), ifail(:)
       REAL*8, ALLOCATABLE :: w(:) ,rwork(:)
 
       INTEGER :: nkp, j, l, n
       INTEGER :: info
       INTEGER :: m, il, iu, ierr
       COMPLEX*16 :: ctmp
       COMPLEX*16 :: czero
       PARAMETER( czero = ( 0.0d0, 0.0d0 ) ) 

! ...  End of declarations
 
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
         CALL errore( ' projection_frozen ', ' allocating rwork ', (7*mxdbnd) )
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

           CALL zhpevx( 'v', 'a', 'u', dimwin(nkp), ap(1), 0.0d0, 0.0d0, il, iu,     &
                -1, m, w(1), z(1,1), mxdbnd, work(1), rwork(1), iwork(1), ifail(1), info )

           IF ( INFO < 0 ) THEN
             WRITE(6,*) '*** ERROR *** ZHPEVX WHILE DIAGONALIZING QPQ MATRIX'
             WRITE(6,*) 'THE ',-info, ' ARGUMENT OF ZHPEVX HAD AN ILLEGAL VALUE'
             STOP
           ELSE IF ( INFO > 0 ) THEN
             WRITE(6,*) '*** ERROR *** ZHPEVX WHILE DIAGONALIZING QPQ MATRIX'
             WRITE(6,*) info, 'EIGENVECTORS FAILED TO CONVERGE'
             STOP
           END IF

           IF( m /= dimwin(nkp) ) THEN
             WRITE(*,*) '*** ERROR *** in projection_frozen.f'
             WRITE(*,*) 'number of eigenvalues/vectors obtained is',     &
                        m,' not equal to the number asked,', dimwin(nkp)
             STOP
           END IF
         
! ...      Pick the dimwann-dimfroz(nkp) leading eigenvectors to be trial states; 
!          put them right after the frozen states in lamp

           DO l = dimfroz(nkp) + 1, dimwann
             DO j = 1, dimwin(nkp)
               lamp(j,l,nkp) = z(j,il)  
             END DO
             il = il + 1   
           END DO

           IF( il - 1 /= iu ) THEN
             WRITE(*,*) '*** ERROR *** in projection_frozen.f'
             WRITE(*,*) 'il-1.ne.iu'
             STOP
           END IF

           DO l = dimfroz(nkp) + 1, dimwann
             DO m = dimfroz(nkp) + 1, l
               ctmp = czero
               DO j = 1, dimwin(nkp)
                 ctmp = ctmp + CONJG( lamp(j,m,nkp) ) * lamp(j,l,nkp)
               END DO
               WRITE(*,'(i2,2x,i2,f16.12,1x,f16.12)') l, m, ctmp
               IF ( l == m ) THEN
                 IF ( ABS( ctmp - CMPLX( 1.0d0, 0.0d0 ) ) > 1.0e-8 ) THEN
                   WRITE(*,*) '*** ERROR *** in projection_frozen.f'
                   WRITE(*,*) 'projected gaussians in lamp not orthonormal'
                   WRITE(*,'(a11,i4)') 'at k-point ', nkp
                   STOP
                 END IF
               ELSE
                 IF ( ABS(ctmp) > 1.0e-8 ) THEN
                   WRITE(*,*) '*** ERROR *** in projection_frozen.f'
                   WRITE(*,*) 'projected gaussians in lamp not orthonormal'
                   WRITE(*,'(a11,i4)') 'at k-point ', nkp
                   STOp
                 END IF
               END IF
             END DO
           END DO

         END IF ! DIMWANN>DIMFROZ(NKP)

       END DO ! NKP

       DEALLOCATE( ap )
       DEALLOCATE( z )
       DEALLOCATE( work )
       DEALLOCATE( p_s )
       DEALLOCATE( q_froz )
       DEALLOCATE( pq )
       DEALLOCATE( qpq )
       DEALLOCATE( iwork )
       DEALLOCATE( ifail )
       DEALLOCATE( w )
       DEALLOCATE( rwork )

       RETURN
       END SUBROUTINE







