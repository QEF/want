!
! Copyright (C) 2004 WanT Group
!
! This file is distributed under the terms of the
! GNU General Public License. See the file `License'
! in the root directory of the present distribution,
! or http://www.gnu.org/copyleft/gpl.txt .
!
! <INFO>
!*********************************************
   MODULE util_module
!*********************************************
  USE kinds
  USE constants, ONLY : ZERO, ONE, CZERO, CONE
  IMPLICIT NONE
  PRIVATE

! General purpose utilities
!
! routines in this module:
! SUBROUTINE  zmat_pack( zp, z, n)
! SUBROUTINE  zmat_unpack( z, zp, n)
! SUBROUTINE  zmat_svd( m, n, a, s, u, vt)
! SUBROUTINE  dmat_svd( m, n, a, s, u, vt)
! SUBROUTINE  zmat_mul( c, a, opa, b, opb, m, n, k)
! SUBROUTINE  zmat_hdiag( z, w, a, n)
! LOGICAL FUNCTION  zmat_unitary( z [,side] [,toll])
! 
! </INFO>
!

INTERFACE mat_svd
   MODULE PROCEDURE zmat_svd
   MODULE PROCEDURE dmat_svd
END INTERFACE


PUBLIC :: zmat_pack
PUBLIC :: zmat_unpack
PUBLIC ::  mat_svd
PUBLIC :: zmat_mul
PUBLIC :: zmat_hdiag
PUBLIC :: zmat_unitary

CONTAINS

!
! Subroutines
!

!**********************************************************
   SUBROUTINE zmat_pack( zp, z, n )
   !**********************************************************
    IMPLICIT NONE
    COMPLEX(dbl), INTENT(OUT) :: zp(:)
    COMPLEX(dbl), INTENT(IN) :: z(:,:)
    INTEGER :: n
    INTEGER :: i, j, ind

    ind = 1
    DO j = 1, n
      DO i = 1, n
        zp(ind) = z(i,j)
        ind = ind + 1
      END DO
    END DO

    RETURN
  END SUBROUTINE


!**********************************************************
   SUBROUTINE zmat_unpack( z, zp, n )
   !**********************************************************
    IMPLICIT NONE
    COMPLEX(dbl), INTENT(IN) :: zp(:)
    COMPLEX(dbl), INTENT(OUT) :: z(:,:)
    INTEGER :: n
    INTEGER :: i, j, ind

    ind = 1
    DO j = 1, n
      DO i = 1, n
        z(i,j) = zp(ind)
        ind = ind + 1
      END DO
    END DO

    RETURN
  END SUBROUTINE


!**********************************************************
   SUBROUTINE dmat_svd(m, n, a, s, u, vt)
   !**********************************************************
   !
   !  computes the singular value decomposition (SVD) of a REAL(DP)
   !  M-by-N matrix A. The SVD is written
   !
   !       A = U * SIGMA * transpose(V)
   !
   !  where SIGMA is an M-by-N matrix which is zero except for its
   !  min(m,n) diagonal elements, U is an M-by-M unitary matrix, and
   !  V is an N-by-N unitary matrix.  The diagonal elements of SIGMA
   !  are the singular values of A; they are real and non-negative, and
   !  are returned in descending order. 
   !  Note that the routine returns V**T, not V.
   !
   IMPLICIT NONE
   INTEGER, INTENT(IN)    :: m, n
   REAL(dbl), INTENT(IN)  :: a(:,:)
   REAL(dbl), INTENT(OUT) :: s(:)
   REAL(dbl), INTENT(OUT) :: u(:,:), vt(:,:)

   INTEGER :: ierr, info, lwork
   REAL(dbl), ALLOCATABLE :: atmp(:,:), work(:)

   IF ( m <= 0 .OR. n<=0 ) CALL errore('dmat_svd','Invalid DIMs',1)
   IF ( m > SIZE(a,1) .OR. m > SIZE(u,1) .OR. m > SIZE(u,2) ) &
           CALL errore('dmat_svd','m too large',m)
   IF ( n > SIZE(a,2) .OR. n > SIZE(vt,1) .OR. n > SIZE(vt,2) ) &
           CALL errore('dmat_svd','n too large',n)
   IF ( SIZE(s) < MIN(m,n) ) CALL errore('dmat_svd','s dimension too small',1)

   !
   ! allocate local variables and workspace
   !
   lwork = MAX( 3*MIN(m,n) + MAX(m,n), 5*MIN(m,n) )
   ALLOCATE( atmp(m,n), STAT=ierr )
      IF (ierr/=0)  CALL errore('dmat_svd','allocating atmp',ABS(ierr))
   ALLOCATE( work(lwork), STAT=ierr )
      IF (ierr/=0)  CALL errore('dmat_svd','allocating work',ABS(ierr))

   !
   ! save A (which is intent IN)
   atmp(:,:) = a(:,:)

   CALL DGESVD('A','A', m, n, atmp, m, s, u, SIZE(u,1), vt, SIZE(vt,1), &
                work, lwork, info)

   IF ( info < 0 ) CALL errore('dmat_svd', 'DGESVD: info illegal value', -info )
   IF ( info > 0 ) CALL errore('dmat_svd', 'DGESVD: DBESQR not converged', info )
    
   DEALLOCATE( atmp, work, STAT=ierr)
      IF(ierr/=0) CALL errore('dmat_svd','deallocating atpm, work',ABS(ierr))

   RETURN
END SUBROUTINE dmat_svd


!**********************************************************
   SUBROUTINE zmat_svd(m, n, a, s, u, vt)
   !**********************************************************
   !
   !  computes the singular value decomposition (SVD) of a complex
   !  M-by-N matrix A. The SVD is written
   !
   !       A = U * SIGMA * conjugate-transpose(V)
   !
   !  where SIGMA is an M-by-N matrix which is zero except for its
   !  min(m,n) diagonal elements, U is an M-by-M unitary matrix, and
   !  V is an N-by-N unitary matrix.  The diagonal elements of SIGMA
   !  are the singular values of A; they are real and non-negative, and
   !  are returned in descending order. 
   !  Note that the routine returns V**H, not V.
   !
   IMPLICIT NONE
   INTEGER, INTENT(IN)       :: m, n
   COMPLEX(dbl), INTENT(IN)  :: a(:,:)
   REAL(dbl),    INTENT(OUT) :: s(:)
   COMPLEX(dbl), INTENT(OUT) :: u(:,:), vt(:,:)

   INTEGER :: ierr, info, lwork
   REAL(dbl),    ALLOCATABLE :: rwork(:)
   COMPLEX(dbl), ALLOCATABLE :: atmp(:,:), work(:)

   IF ( m <= 0 .OR. n<=0 ) CALL errore('zmat_svd','Invalid DIMs',1)
   IF ( m > SIZE(a,1) .OR. m > SIZE(u,1) .OR. m > SIZE(u,2) ) &
           CALL errore('zmat_svd','m too large',m)
   IF ( n > SIZE(a,2) .OR. n > SIZE(vt,1) .OR. n > SIZE(vt,2) ) &
           CALL errore('zmat_svd','n too large',n)
   IF ( SIZE(s) < MIN(m,n) ) CALL errore('zmat_svd','s dimension too small',1)

   !
   ! allocate local variables and workspace
   !
   lwork = 2 * MIN(m,n) + MAX(m,n)
   ALLOCATE( atmp(m,n), STAT=ierr )
      IF (ierr/=0)  CALL errore('zmat_svd','allocating atmp',ABS(ierr))
   ALLOCATE( work(lwork), STAT=ierr )
      IF (ierr/=0)  CALL errore('zmat_svd','allocating work',ABS(ierr))
   ALLOCATE( rwork(5 * MIN(m,n) ), STAT=ierr )
      IF (ierr/=0)  CALL errore('zmat_svd','allocating rwork',ABS(ierr))

   !
   ! save A (which is intent IN)
   atmp(:,:) = a(:,:)

   CALL ZGESVD('A','A', m, n, atmp, m, s, u, SIZE(u,1), vt, SIZE(vt,1), &
                work, lwork, rwork, info)

   IF ( info < 0 ) CALL errore('zmat_svd', 'ZGESVD: info illegal value', -info )
   IF ( info > 0 ) CALL errore('zmat_svd', 'ZGESVD: ZBESQR not converged', info )
    
   DEALLOCATE( atmp, work, rwork, STAT=ierr)
      IF(ierr/=0) CALL errore('zmat_svd','deallocating atpm, work, rwork',ABS(ierr))

   RETURN
END SUBROUTINE zmat_svd






!**********************************************************
   SUBROUTINE zmat_mul( c, a, opa, b, opb, m, n, k )
   !**********************************************************
   IMPLICIT NONE
   COMPLEX(dbl), INTENT(IN)  :: a(:,:)
   COMPLEX(dbl), INTENT(IN)  :: b(:,:)
   COMPLEX(dbl), INTENT(OUT) :: c(:,:)
   CHARACTER, INTENT(IN) :: opa, opb
   INTEGER, INTENT(IN) :: m, n, k
   INTEGER :: i, j, l
   !
   ! According to BLAS convention:
   ! C = opa(A) * opb(B)      op* = 'N' normal, 'C' complx conjg (i.e. herm conjg)
   !                          C is m*n,   opa(A) is m*k, opb(B) = k*n
   !
   IF ( m <= 0 .OR. n<=0 .OR. k<=0) CALL errore('zmat_mul','Invalid DIM',1)
   IF( opb /= 'N' .AND. opb /= 'C' ) &
     CALL errore( ' zmat_mul ', ' argument value not allowed ', 5 )

   IF( k < 20 ) THEN
       IF( ( opb == 'N' ) .AND. ( opa == 'N' ) ) THEN
           !
           IF ( m > SIZE(c,1) .OR. m > SIZE(a,1) ) CALL  errore('zmat_mul','Invalid C,A',m)
           IF ( n > SIZE(c,2) .OR. n > SIZE(b,2) ) CALL  errore('zmat_mul','Invalid C,B',n)
           IF ( k > SIZE(a,2) .OR. k > SIZE(b,1) ) CALL  errore('zmat_mul','Invalid A,B',k)
           !
           DO j = 1, n
           DO i = 1, m
               c(i,j) = CZERO
               DO l = 1, k
                   c(i,j) = c(i,j) + a(i,l) * b(l,j)
               ENDDO
           ENDDO
           ENDDO
       ELSE IF( ( opa == 'N' ) .AND. ( opb == 'C' ) ) THEN
           !
           IF ( m > SIZE(c,1) .OR. m > SIZE(a,1) ) CALL  errore('zmat_mul','Invalid C,A',m)
           IF ( n > SIZE(c,2) .OR. n > SIZE(b,1) ) CALL  errore('zmat_mul','Invalid C,B',n)
           IF ( k > SIZE(a,2) .OR. k > SIZE(b,2) ) CALL  errore('zmat_mul','Invalid A,B',k)
           !
           DO j = 1, n
           DO i = 1, m
               c(i,j) = CZERO
               DO l = 1, k
                   c(i,j) = c(i,j) + a(i,l) * CONJG( b(j,l) )
               ENDDO
           ENDDO
           ENDDO
       ELSE IF( ( opa == 'C' ) .AND. ( opb == 'N' ) ) THEN
           !
           IF ( m > SIZE(c,1) .OR. m > SIZE(a,2) ) CALL  errore('zmat_mul','Invalid C,A',m)
           IF ( n > SIZE(c,2) .OR. n > SIZE(b,2) ) CALL  errore('zmat_mul','Invalid C,B',n)
           IF ( k > SIZE(a,1) .OR. k > SIZE(b,1) ) CALL  errore('zmat_mul','Invalid A,B',k)
           !
           DO j = 1, n
           DO i = 1, m
               c(i,j) = CZERO
               DO l = 1, k
                   c(i,j) = c(i,j) + CONJG( a(l,i) )* b(l,j) 
               ENDDO
           ENDDO
           ENDDO
       ELSE IF( ( opb == 'C' ) .AND. ( opa == 'C' ) ) THEN
           !
           IF ( m > SIZE(c,1) .OR. m > SIZE(a,2) ) CALL  errore('zmat_mul','Invalid C,A',m)
           IF ( n > SIZE(c,2) .OR. n > SIZE(b,1) ) CALL  errore('zmat_mul','Invalid C,B',n)
           IF ( k > SIZE(a,1) .OR. k > SIZE(b,2) ) CALL  errore('zmat_mul','Invalid A,B',k)
           !
           DO j = 1, n
           DO i = 1, m
               c(i,j) = CZERO
               DO l = 1, k
                   c(i,j) = c(i,j) + CONJG( a(l,i) )* CONJG( b(j,l) )
               ENDDO
           ENDDO
           ENDDO
       ENDIF
   ELSE
       CALL ZGEMM( opa, opb, m, n, k, CONE, a(1,1), &
       SIZE(a,1), b(1,1), SIZE(b,1), CZERO, c(1,1), SIZE(c,1) )
   ENDIF
   RETURN
END SUBROUTINE


!**********************************************************
   SUBROUTINE zmat_hdiag( z, w, a, n )
   !**********************************************************
   IMPLICIT NONE
   COMPLEX(dbl), INTENT(IN)  :: a(:,:)
   COMPLEX(dbl), INTENT(OUT) :: z(:,:)
   REAL(dbl),    INTENT(OUT) :: w(:)
   INTEGER,      INTENT(in)  :: n

   INTEGER :: i, j, ierr, info
   COMPLEX(dbl), ALLOCATABLE :: ap(:)
   COMPLEX(dbl), ALLOCATABLE :: work(:)
   REAL(dbl), ALLOCATABLE :: rwork(:)
   INTEGER, ALLOCATABLE :: ifail(:)
   INTEGER, ALLOCATABLE :: iwork(:)

   ! get the dimension of the problem
   IF ( n <= 0 ) CALL errore('zmat_hdiag','Invalid N',ABS(n)+1)
   IF ( n > SIZE(a,1) .OR. n > SIZE(a,2) ) &
        CALL errore('zmat_hdiag','Invalid A dimensions',ABS(n)+1)
   IF ( n > SIZE(z,1) .OR. n > SIZE(z,2) ) &
        CALL errore('zmat_hdiag','Invalid Z dimensions',ABS(n)+1)
   
   ALLOCATE( ap(n*(n+1)/2), STAT=ierr )
      IF(ierr/=0) CALL errore('zmat_hdiag','allocating ap',ABS(ierr))
   ALLOCATE( work(2*n), STAT=ierr )
      IF(ierr/=0) CALL errore('zmat_hdiag','allocating work',ABS(ierr))
   ALLOCATE( rwork(7*n), STAT=ierr )
      IF(ierr/=0) CALL errore('zmat_hdiag','allocating rwork',ABS(ierr))
   ALLOCATE( ifail(n), STAT=ierr )
      IF(ierr/=0) CALL errore('zmat_hdiag','allocating ifail',ABS(ierr))
   ALLOCATE( iwork(5*n), STAT=ierr )
      IF(ierr/=0) CALL errore('zmat_hdiag','allocating iwork',ABS(ierr))

   DO j = 1, n
   DO i = 1, j
      ap(i + ( (j-1)*j)/2 ) = a(i,j)
   ENDDO
   ENDDO

   CALL ZHPEVX( 'v', 'a', 'u', n, ap(1), ZERO, ZERO, 0, 0, -ONE, i, w(1), &
                 z(1,1), SIZE(z,1), work(1), rwork(1), iwork(1), ifail(1), info )

   IF ( info < 0 ) CALL errore('zmat_hdiag', 'zhpevx: info illegal value', -info )
   IF ( info > 0 ) &
        CALL errore('zmat_hdiag', 'zhpevx: eigenvectors not converged', info )
    
   DEALLOCATE( ap, work, rwork, iwork, ifail, STAT=ierr)
      IF(ierr/=0) CALL errore('zmat_hdiag','deallocating ap...ifail',ABS(ierr))

   RETURN
END SUBROUTINE zmat_hdiag


!**********************************************************
   FUNCTION  zmat_unitary( z, side, toll )
   !**********************************************************
   IMPLICIT NONE
   LOGICAL                            :: zmat_unitary
   COMPLEX(dbl),           INTENT(in) :: z(:,:)
   CHARACTER(*), OPTIONAL, INTENT(in) :: side 
   REAL(dbl), OPTIONAL,    INTENT(in) :: toll
   !
   ! check if a complex matrix is unitary.
   ! SIDE='left'  only   A^{\dag} * A = I
   ! SIDE='right' only   A * A^{\dag} = I
   ! SIDE='both'  both sides (DEFAULT)
   !
   REAL(dbl)     :: toll_
   CHARACTER(10) :: side_
   INTEGER       :: dim1,dim2
   INTEGER       :: i,j,l, ierr
   COMPLEX(dbl), ALLOCATABLE  :: result(:,:),z_loc(:,:) 
   
   zmat_unitary = .TRUE. 

   toll_ = TINY(ZERO)  
   side_ = 'both'
   IF ( PRESENT(side) ) side_ = TRIM(side)
   IF ( PRESENT(toll) ) toll_ = toll
   IF ( toll_ <= 0 ) CALL errore('zmat_unitary','Invalid TOLL',1)
  
   dim1 = SIZE( z, 1)
   dim2 = SIZE( z, 2)
   IF ( dim1 <= 0) CALL errore('zmat_unitary','Invalid dim1',ABS(dim1)+1)
   IF ( dim2 <= 0) CALL errore('zmat_unitary','Invalid dim2',ABS(dim2)+1)

   !
   ! workaround for a possible bug in the INTEL compiler
   ! If the actual z matrix were passed to the BLAS routine nasty things occur
   ! 
   ALLOCATE( z_loc(dim1,dim2), STAT=ierr )
      IF ( ierr /= 0 ) CALL errore('zmat_unitary','allocating z_loc',ABS(ierr))
   z_loc = z

   !
   ! check side LEFT
   !
   IF ( TRIM(side_) == 'both' .OR. TRIM(side_) == 'BOTH' .OR. &
        TRIM(side_) == 'left' .OR. TRIM(side_) == 'LEFT'  ) THEN 

       ALLOCATE( result(dim2,dim2), STAT=ierr )
          IF ( ierr /= 0 ) CALL errore('zmat_unitary','allocating result',ABS(ierr))
       result = CZERO
       ! 
       ! matrix mult
       CALL ZGEMM( 'C','N', dim2, dim2, dim1, CONE, z_loc(1,1), dim1, z_loc(1,1), &
                   dim1, CZERO, result(1,1), dim2 )

       DO j=1,dim2
       DO i=1,dim2
           IF ( i==j ) THEN
                IF ( ABS( result(i,j) -CONE ) > toll_ ) zmat_unitary = .FALSE.
           ELSE
                IF ( ABS( result(i,j) ) > toll_ ) zmat_unitary = .FALSE.
           ENDIF
       ENDDO
       ENDDO

       DEALLOCATE( result, STAT=ierr)
          IF ( ierr /= 0 ) CALL errore('zmat_unitary','deallocating result',ABS(ierr))
   ENDIF
       
   !
   ! check side RIGHT
   !
   IF ( TRIM(side_) == 'both' .OR. TRIM(side_) == 'BOTH' .OR. &
        TRIM(side_) == 'right'.OR. TRIM(side_) == 'RIGHT' ) THEN 

       ALLOCATE( result(dim1,dim1), STAT=ierr )
          IF ( ierr /= 0 ) CALL errore('zmat_unitary','allocating result',ABS(ierr))
       ! 
       ! matrix mult
       CALL ZGEMM( 'N','C', dim1, dim1, dim2, CONE, z_loc(1,1), dim1, z_loc(1,1), &
                   dim1, CZERO, result(1,1), dim1 )
       
       DO j=1,dim1
       DO i=1,dim1
           IF ( i==j ) THEN
                IF ( ABS( result(i,j) -CONE ) > toll_ ) zmat_unitary = .FALSE.
           ELSE
                IF ( ABS( result(i,j) ) > toll_ ) zmat_unitary = .FALSE.
           ENDIF
       ENDDO
       ENDDO

       DEALLOCATE( result, STAT=ierr)
          IF ( ierr /= 0 ) CALL errore('zmat_unitary','deallocating result',ABS(ierr))
   ENDIF
      
   DEALLOCATE(z_loc, STAT=ierr)
      IF ( ierr /= 0 ) CALL errore('zmat_unitary','deallocating z_loc',ABS(ierr))
       
   END FUNCTION zmat_unitary

END MODULE util_module


