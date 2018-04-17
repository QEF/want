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
! SUBROUTINE   mat_svd( m, n, a, s, u, vt)
! SUBROUTINE   mat_sv ( n, nrhs, a, b [,ierr])
! SUBROUTINE   mat_mul( c, a, opa, b, opb, m, n, k)
! SUBROUTINE   mat_hdiag( z, w, a, n)
! SUBROUTINE  zmat_diag( z, w, a, n, side)
! LOGICAL FUNCTION  zmat_unitary( m, n, z [,side] [,toll])
! INTEGER FUNCTION   mat_rank( m, n, a, toll)
! 
! </INFO>
!

!
! matrix multiplication
INTERFACE mat_mul
   MODULE PROCEDURE zmat_mul
   MODULE PROCEDURE dmat_mul
END INTERFACE
!
! singular value decomposition
INTERFACE mat_svd
   MODULE PROCEDURE zmat_svd
   MODULE PROCEDURE dmat_svd
END INTERFACE
!
! simple linear system solver
INTERFACE mat_sv
   MODULE PROCEDURE zmat_sv
   MODULE PROCEDURE zmat_sv_1
   MODULE PROCEDURE dmat_sv
   MODULE PROCEDURE dmat_sv_1
END INTERFACE
!
! rank calculation
INTERFACE mat_rank
   MODULE PROCEDURE zmat_rank
   MODULE PROCEDURE dmat_rank
END INTERFACE
!
! matrix diagonalization
INTERFACE mat_hdiag
   MODULE PROCEDURE zmat_hdiag
   MODULE PROCEDURE dmat_hdiag
END INTERFACE



PUBLIC :: zmat_pack
PUBLIC :: zmat_unpack
PUBLIC ::  mat_svd
PUBLIC ::  mat_sv
PUBLIC ::  mat_mul
PUBLIC ::  mat_hdiag
PUBLIC :: zmat_diag
PUBLIC :: zmat_unitary
PUBLIC ::  mat_rank

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
   INTEGER,   INTENT(IN)  :: m, n
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
   atmp(:,:) = a(1:m, 1:n)

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
   atmp(:,:) = a(1:m, 1:n)

   CALL ZGESVD('A','A', m, n, atmp, m, s, u, SIZE(u,1), vt, SIZE(vt,1), &
                work, lwork, rwork, info)

   IF ( info < 0 ) CALL errore('zmat_svd', 'ZGESVD: info illegal value', -info )
   IF ( info > 0 ) CALL errore('zmat_svd', 'ZGESVD: ZBESQR not converged', info )
    
   DEALLOCATE( atmp, work, rwork, STAT=ierr)
      IF(ierr/=0) CALL errore('zmat_svd','deallocating atmp, work, rwork',ABS(ierr))

   RETURN
END SUBROUTINE zmat_svd


!**********************************************************
   SUBROUTINE dmat_sv(n, nrhs, a, b, ierr)
   !**********************************************************
   !
   !  Computes the solution of the real system of linear equations
   !     A * X = B,
   !  where A is an N-by-N matrix and X and B are N-by-NRHS matrices.
   !
   !  The LU decomposition with partial pivoting and row interchanges is
   !  used to factor A as
   !     A = P * L * U,
   !  where P is a permutation matrix, L is unit lower triangular, and U is
   !  upper triangular.  The factored form of A is then used to solve the
   !  system of equations A * X = B.
   !
   IMPLICIT NONE
   INTEGER,   INTENT(IN)       :: n, nrhs
   REAL(dbl), INTENT(IN)       :: a(:,:)
   REAL(dbl), INTENT(INOUT)    :: b(:,:)
   INTEGER, OPTIONAL, INTENT(out) :: ierr

   INTEGER :: ierr_, info
   INTEGER,      ALLOCATABLE :: ipiv(:)
   REAL(dbl), ALLOCATABLE    :: atmp(:,:)

   IF ( PRESENT(ierr) ) ierr=0
   !
   IF ( n > SIZE(a,1) .OR. n > SIZE(a,2) ) CALL errore('dmat_sv','matrix A too small',1)
   IF ( n > SIZE(b,1) ) CALL errore('dmat_sv','matrix B too small (I)',1)
   IF ( nrhs > SIZE(b,2) ) CALL errore('dmat_sv','matrix B too small (II)',1)

   ALLOCATE( atmp(n,n), ipiv(n), STAT=ierr_ )
     IF (ierr_/=0) CALL errore('dmat_sv','allocating atmp, ipiv',ABS(ierr_))
   !
   ! make a local copy of a
   atmp(:,:) = a(1:n,1:n) 

   CALL DGESV( n, nrhs, atmp, n, ipiv, b, SIZE(b,1), info)

   IF ( PRESENT(ierr) ) THEN
        IF (info/=0) ierr= info
   ELSE
        IF ( info < 0 ) CALL errore('dmat_sv', 'DGESV: info illegal value', -info )
        IF ( info > 0 ) CALL errore('dmat_sv', 'DGESV: singular matrix', info )
   ENDIF
    
   DEALLOCATE( atmp, ipiv, STAT=ierr_)
      IF(ierr_/=0) CALL errore('dmat_sv','deallocating atmp, ipiv',ABS(ierr_))

   RETURN
END SUBROUTINE dmat_sv


!**********************************************************
   SUBROUTINE dmat_sv_1(n, nrhs, a, b, ierr)
   !**********************************************************
   !
   ! Interface to dmat_sv when nrhs = 1
   !
   IMPLICIT NONE
   INTEGER, INTENT(IN)      :: n, nrhs
   REAL(dbl), INTENT(IN)    :: a(:,:)
   REAL(dbl), INTENT(INOUT) :: b(:)
   INTEGER, OPTIONAL, INTENT(out) :: ierr
   !
   INTEGER  :: ierr_, info
   REAL(dbl),   ALLOCATABLE :: bl(:,:)

   IF ( PRESENT(ierr) ) ierr=0
   !
   IF ( nrhs /= 1) CALL errore('dmat_sv_1','more than 1 rhs ? ',ABS(nrhs)+1)
   IF ( n > SIZE(b,1) ) CALL errore('dmat_sv_1','vector b too small',2)
   !
   ALLOCATE( bl(n,1), STAT=ierr_ )
   IF (ierr_/=0) CALL errore('dmat_sv_1','allocating bl',ABS(ierr_))

   bl(:,1) = b(1:n)
   CALL dmat_sv( n, 1, a, bl, IERR=info)
   !
   IF ( PRESENT(ierr) ) THEN 
       ierr=info
   ELSE
       IF ( info /=0 ) CALL errore('dmat_sv_1','info/=0',ABS(info))
   ENDIF
   !
   b(1:n) = bl(1:n,1)

   DEALLOCATE( bl, STAT=ierr_)
   IF (ierr_/=0) CALL errore('dmat_sv_1','deallocating bl',ABS(ierr_))

   RETURN
END SUBROUTINE dmat_sv_1


!**********************************************************
   SUBROUTINE zmat_sv(n, nrhs, a, b, ierr)
   !**********************************************************
   !
   !  Computes the solution of the complex system of linear equations
   !     A * X = B,
   !  where A is an N-by-N matrix and X and B are N-by-NRHS matrices.
   !
   !  The LU decomposition with partial pivoting and row interchanges is
   !  used to factor A as
   !     A = P * L * U,
   !  where P is a permutation matrix, L is unit lower triangular, and U is
   !  upper triangular.  The factored form of A is then used to solve the
   !  system of equations A * X = B.
   !
   IMPLICIT NONE
   INTEGER, INTENT(IN)         :: n, nrhs
   COMPLEX(dbl), INTENT(IN)    :: a(:,:)
   COMPLEX(dbl), INTENT(INOUT) :: b(:,:)
   INTEGER, OPTIONAL, INTENT(out) :: ierr

   INTEGER :: ierr_, info
   INTEGER,      ALLOCATABLE :: ipiv(:)
   COMPLEX(dbl), ALLOCATABLE :: atmp(:,:)

   IF ( PRESENT(ierr) ) ierr=0
   !
   IF ( n > SIZE(a,1) .OR. n > SIZE(a,2) ) CALL errore('zmat_sv','matrix A too small',1)
   IF ( n > SIZE(b,1) ) CALL errore('zmat_sv','matrix B too small (I)',1)
   IF ( nrhs > SIZE(b,2) ) CALL errore('zmat_sv','matrix B too small (II)',1)

   ALLOCATE( atmp(n,n), ipiv(n), STAT=ierr_ )
     IF (ierr_/=0) CALL errore('zmat_sv','allocating atmp, ipiv',ABS(ierr_))
   !
   ! make a local copy of a
   atmp(:,:) = a(1:n,1:n) 

   CALL ZGESV( n, nrhs, atmp, n, ipiv, b, SIZE(b,1), info)

   IF ( PRESENT(ierr) ) THEN
        IF (info/=0) ierr= info
   ELSE
        IF ( info < 0 ) CALL errore('zmat_sv', 'ZGESV: info illegal value', -info )
        IF ( info > 0 ) CALL errore('zmat_sv', 'ZGESV: singular matrix', info )
   ENDIF

   DEALLOCATE( atmp, ipiv, STAT=ierr_)
      IF(ierr_/=0) CALL errore('zmat_sv','deallocating atmp, ipiv',ABS(ierr_))

   RETURN
END SUBROUTINE zmat_sv


!**********************************************************
   SUBROUTINE zmat_sv_1(n, nrhs, a, b, ierr)
   !**********************************************************
   !
   ! Interface to zmat_sv when nrhs = 1
   !
   IMPLICIT NONE
   INTEGER, INTENT(IN)         :: n, nrhs
   COMPLEX(dbl), INTENT(IN)    :: a(:,:)
   COMPLEX(dbl), INTENT(INOUT) :: b(:)
   INTEGER, OPTIONAL, INTENT(out) :: ierr
   !
   INTEGER  :: ierr_, info
   COMPLEX(dbl),   ALLOCATABLE :: bl(:,:)

   IF ( PRESENT(ierr) ) ierr = 0
   !
   IF ( nrhs /= 1) CALL errore('zmat_sv_1','more than 1 rhs ?',ABS(nrhs)+1)
   IF ( n > SIZE(b,1) ) CALL errore('zmat_sv_1','vector b too small',2)
   !
   ALLOCATE( bl(n,1), STAT=ierr_ )
   IF (ierr_/=0) CALL errore('zmat_sv_1','allocating bl',ABS(ierr_))

   bl(:,1) = b(1:n)
   CALL zmat_sv( n, 1, a, bl, IERR=info)
   !
   IF ( PRESENT(ierr) ) THEN 
       ierr=info
   ELSE
       IF ( info /=0 ) CALL errore('zmat_sv_1','info/=0',ABS(info))
   ENDIF
   !
   b(1:n) = bl(1:n,1)

   DEALLOCATE( bl, STAT=ierr_)
   IF (ierr_/=0) CALL errore('zmat_sv_1','deallocating bl',ABS(ierr_))

   RETURN
END SUBROUTINE zmat_sv_1


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
     CALL errore('zmat_mul','argument value not allowed', 5 )

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

       CALL ZGEMM( opa, opb, m, n, k, CONE, a, SIZE(a,1), &
                   b, SIZE(b,1), CZERO, c, SIZE(c,1) )
   ENDIF
END SUBROUTINE zmat_mul


!**********************************************************
   SUBROUTINE dmat_mul( c, a, opa, b, opb, m, n, k )
   !**********************************************************
   IMPLICIT NONE
   REAL(dbl), INTENT(IN)  :: a(:,:)
   REAL(dbl), INTENT(IN)  :: b(:,:)
   REAL(dbl), INTENT(OUT) :: c(:,:)
   CHARACTER, INTENT(IN) :: opa, opb
   INTEGER, INTENT(IN) :: m, n, k
   INTEGER :: i, j, l
   !
   ! According to BLAS convention:
   ! C = opa(A) * opb(B)      op* = 'N' normal, 'T' transpose 
   !                          C is m*n,   opa(A) is m*k, opb(B) = k*n
   !
   IF ( m <= 0 .OR. n<=0 .OR. k<=0) CALL errore('dmat_mul','Invalid DIM',1)
   IF( opb /= 'N' .AND. opb /= 'T' ) &
     CALL errore('dmat_mul','argument value not allowed ', 5 )

   IF( k < 20 ) THEN
       IF( ( opb == 'N' ) .AND. ( opa == 'N' ) ) THEN
           !
           IF ( m > SIZE(c,1) .OR. m > SIZE(a,1) ) CALL  errore('dmat_mul','Invalid C,A',m)
           IF ( n > SIZE(c,2) .OR. n > SIZE(b,2) ) CALL  errore('dmat_mul','Invalid C,B',n)
           IF ( k > SIZE(a,2) .OR. k > SIZE(b,1) ) CALL  errore('dmat_mul','Invalid A,B',k)
           !
           DO j = 1, n
           DO i = 1, m
               c(i,j) = ZERO
               DO l = 1, k
                   c(i,j) = c(i,j) + a(i,l) * b(l,j)
               ENDDO
           ENDDO
           ENDDO
       ELSE IF( ( opa == 'N' ) .AND. ( opb == 'T' ) ) THEN
           !
           IF ( m > SIZE(c,1) .OR. m > SIZE(a,1) ) CALL  errore('dmat_mul','Invalid C,A',m)
           IF ( n > SIZE(c,2) .OR. n > SIZE(b,1) ) CALL  errore('dmat_mul','Invalid C,B',n)
           IF ( k > SIZE(a,2) .OR. k > SIZE(b,2) ) CALL  errore('dmat_mul','Invalid A,B',k)
           !
           DO j = 1, n
           DO i = 1, m
               c(i,j) = ZERO
               DO l = 1, k
                   c(i,j) = c(i,j) + a(i,l) * b(j,l)
               ENDDO
           ENDDO
           ENDDO
       ELSE IF( ( opa == 'T' ) .AND. ( opb == 'N' ) ) THEN
           !
           IF ( m > SIZE(c,1) .OR. m > SIZE(a,2) ) CALL  errore('dmat_mul','Invalid C,A',m)
           IF ( n > SIZE(c,2) .OR. n > SIZE(b,2) ) CALL  errore('dmat_mul','Invalid C,B',n)
           IF ( k > SIZE(a,1) .OR. k > SIZE(b,1) ) CALL  errore('dmat_mul','Invalid A,B',k)
           !
           DO j = 1, n
           DO i = 1, m
               c(i,j) = ZERO
               DO l = 1, k
                   c(i,j) = c(i,j) + a(l,i) * b(l,j) 
               ENDDO
           ENDDO
           ENDDO
       ELSE IF( ( opb == 'T' ) .AND. ( opa == 'T' ) ) THEN
           !
           IF ( m > SIZE(c,1) .OR. m > SIZE(a,2) ) CALL  errore('dmat_mul','Invalid C,A',m)
           IF ( n > SIZE(c,2) .OR. n > SIZE(b,1) ) CALL  errore('dmat_mul','Invalid C,B',n)
           IF ( k > SIZE(a,1) .OR. k > SIZE(b,2) ) CALL  errore('dmat_mul','Invalid A,B',k)
           !
           DO j = 1, n
           DO i = 1, m
               c(i,j) = ZERO
               DO l = 1, k
                   c(i,j) = c(i,j) + a(l,i) * b(j,l) 
               ENDDO
           ENDDO
           ENDDO
       ENDIF
   ELSE

       CALL DGEMM( opa, opb, m, n, k, ONE, a, SIZE(a,1), &
                   b, SIZE(b,1), ZERO, c, SIZE(c,1) )
   ENDIF
END SUBROUTINE dmat_mul


!**********************************************************
   SUBROUTINE zmat_hdiag( z, w, a, n )
   !**********************************************************
   !
   ! utility to diagonalize complex hermitean matrices
   !
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
   SUBROUTINE dmat_hdiag( z, w, a, n )
   !**********************************************************
   !
   ! utility to diagonalize real symmetric matrices
   !
   IMPLICIT NONE
   REAL(dbl),    INTENT(IN)  :: a(:,:)
   REAL(dbl),    INTENT(OUT) :: z(:,:)
   REAL(dbl),    INTENT(OUT) :: w(:)
   INTEGER,      INTENT(in)  :: n

   INTEGER :: i, j, ierr, info
   REAL(dbl), ALLOCATABLE :: ap(:)
   REAL(dbl), ALLOCATABLE :: work(:)
   INTEGER,   ALLOCATABLE :: ifail(:)
   INTEGER,   ALLOCATABLE :: iwork(:)

   ! get the dimension of the problem
   IF ( n <= 0 ) CALL errore('dmat_hdiag','Invalid N',ABS(n)+1)
   IF ( n > SIZE(a,1) .OR. n > SIZE(a,2) ) &
        CALL errore('dmat_hdiag','Invalid A dimensions',ABS(n)+1)
   IF ( n > SIZE(z,1) .OR. n > SIZE(z,2) ) &
        CALL errore('dmat_hdiag','Invalid Z dimensions',ABS(n)+1)
   
   ALLOCATE( ap(n*(n+1)/2), STAT=ierr )
      IF(ierr/=0) CALL errore('dmat_hdiag','allocating ap',ABS(ierr))
   ALLOCATE( work(8*n), STAT=ierr )
      IF(ierr/=0) CALL errore('dmat_hdiag','allocating work',ABS(ierr))
   ALLOCATE( ifail(n), STAT=ierr )
      IF(ierr/=0) CALL errore('dmat_hdiag','allocating ifail',ABS(ierr))
   ALLOCATE( iwork(5*n), STAT=ierr )
      IF(ierr/=0) CALL errore('dmat_hdiag','allocating iwork',ABS(ierr))

   DO j = 1, n
   DO i = 1, j
      ap(i + ( (j-1)*j)/2 ) = a(i,j)
   ENDDO
   ENDDO

   CALL DSPEVX( 'v', 'a', 'u', n, ap(1), ZERO, ZERO, 0, 0, -ONE, i, w(1), &
                 z(1,1), SIZE(z,1), work(1), iwork(1), ifail(1), info )

   IF ( info < 0 ) CALL errore('dmat_hdiag', 'zhpevx: info illegal value', -info )
   IF ( info > 0 ) &
        CALL errore('dmat_hdiag', 'zhpevx: eigenvectors not converged', info )
    
   DEALLOCATE( ap, work, iwork, ifail, STAT=ierr)
      IF(ierr/=0) CALL errore('dmat_hdiag','deallocating ap...ifail',ABS(ierr))

   RETURN
END SUBROUTINE dmat_hdiag


!**********************************************************
   SUBROUTINE zmat_diag( z, w, a, n, side )
   !**********************************************************
   !
   ! utility to diagonalize complex non-hermitean matrices
   !
   IMPLICIT NONE
   COMPLEX(dbl),        INTENT(IN)  :: a(:,:)
   COMPLEX(dbl),        INTENT(OUT) :: z(:,:)
   COMPLEX(dbl),        INTENT(OUT) :: w(:)
   INTEGER,             INTENT(in)  :: n
   CHARACTER,           INTENT(in)  :: side

   INTEGER   :: ierr, info, lwork
   CHARACTER :: jobvl, jobvr
   COMPLEX(dbl), ALLOCATABLE :: work(:), vl(:,:), vr(:,:)
   REAL(dbl),    ALLOCATABLE :: rwork(:)

   ! get the dimension of the problem
   IF ( n <= 0 ) CALL errore('zmat_diag','Invalid N',ABS(n)+1)
   IF ( n > SIZE(a,1) .OR. n > SIZE(a,2) ) &
        CALL errore('zmat_diag','Invalid A dimensions',ABS(n)+1)
   IF ( n > SIZE(z,1) .OR. n > SIZE(z,2) ) &
        CALL errore('zmat_diag','Invalid Z dimensions',ABS(n)+1)

   SELECT CASE ( side )
   CASE ( 'L', 'l' )
       jobvl = 'V'
       jobvr = 'N'
   CASE ( 'R', 'r' )
       jobvl = 'N'
       jobvr = 'V'
   CASE DEFAULT   
       CALL errore('zmat_diag','Invalid side',3)
   END SELECT
   
   lwork = 2 * n
   ALLOCATE( work(lwork), STAT=ierr )
      IF(ierr/=0) CALL errore('zmat_diag','allocating work',ABS(ierr))
   ALLOCATE( rwork(2*n), STAT=ierr )
      IF(ierr/=0) CALL errore('zmat_diag','allocating rwork',ABS(ierr))
   ALLOCATE( vl(n,n), STAT=ierr )
      IF(ierr/=0) CALL errore('zmat_diag','allocating vl',ABS(ierr))
   ALLOCATE( vr(n,n), STAT=ierr )
      IF(ierr/=0) CALL errore('zmat_diag','allocating vr',ABS(ierr))

   CALL ZGEEV( jobvl, jobvr, n, a, SIZE(a,1), w, vl, n, vr, n, work, &
               lwork, rwork, info )

   IF ( info < 0 ) CALL errore('zmat_diag', 'zgeev: info illegal value', -info )
   IF ( info > 0 ) CALL errore('zmat_diag', 'zgeev: eigenvectors not converged', info )

   SELECT CASE ( side )
   CASE ( 'L', 'l' )
       z(1:n,1:n) = vl
   CASE ( 'R', 'r' )
       z(1:n,1:n) = vr
   END SELECT
    
   DEALLOCATE( work, rwork, vl, vr, STAT=ierr)
      IF(ierr/=0) CALL errore('zmat_diag','deallocating work--vr',ABS(ierr))

   RETURN
END SUBROUTINE zmat_diag


!**********************************************************
   FUNCTION  zmat_unitary( m, n, z, side, toll )
   !**********************************************************
   IMPLICIT NONE
   LOGICAL                            :: zmat_unitary
   INTEGER,                INTENT(in) :: m,n
   COMPLEX(dbl),           INTENT(in) :: z(:,:)
   CHARACTER(*), OPTIONAL, INTENT(in) :: side 
   REAL(dbl), OPTIONAL,    INTENT(in) :: toll
   !
   ! m, n : actual dimensions of A
   ! check if a complex matrix is unitary.
   ! SIDE='left'  only   A^{\dag} * A = I
   ! SIDE='right' only   A * A^{\dag} = I
   ! SIDE='both'  both sides (DEFAULT)
   !
   REAL(dbl)     :: toll_
   CHARACTER(10) :: side_
   INTEGER       :: dim1,dim2
   INTEGER       :: i, j, ierr
   COMPLEX(dbl), ALLOCATABLE  :: result(:,:)
   
   zmat_unitary = .TRUE. 

   toll_ = TINY(ZERO)  
   side_ = 'both'
   IF ( PRESENT(side) ) side_ = TRIM(side)
   IF ( PRESENT(toll) ) toll_ = toll
   IF ( toll_ <= 0 ) CALL errore('zmat_unitary','Invalid TOLL',1)
  
   IF ( m > SIZE( z, 1) ) CALL errore('zmat_unitary','Invalid m',m)
   IF ( n > SIZE( z, 2) ) CALL errore('zmat_unitary','Invalid n',n)
   dim1 = m
   dim2 = n
   IF ( dim1 <= 0) CALL errore('zmat_unitary','Invalid dim1',ABS(dim1)+1)
   IF ( dim2 <= 0) CALL errore('zmat_unitary','Invalid dim2',ABS(dim2)+1)


   !
   ! check side LEFT
   !
   IF ( TRIM(side_) == 'both' .OR. TRIM(side_) == 'BOTH' .OR. &
        TRIM(side_) == 'left' .OR. TRIM(side_) == 'LEFT'  ) THEN 

       ALLOCATE( result(dim2,dim2), STAT=ierr )
          IF ( ierr /= 0 ) CALL errore('zmat_unitary','allocating result',ABS(ierr))
       ! 
       ! matrix mult
       CALL zmat_mul( result, z, 'C', z, 'N', dim2,dim2,dim1)

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
       CALL zmat_mul( result, z, 'N', z, 'C', dim1,dim1,dim2)

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
   RETURN
END FUNCTION zmat_unitary


!**********************************************************
   FUNCTION  zmat_rank( m, n, a, toll )
   !**********************************************************
   IMPLICIT NONE
   INTEGER          :: zmat_rank
   INTEGER          :: m,n
   COMPLEX(dbl)     :: a(:,:)
   REAL(dbl)        :: toll
   !
   INTEGER :: i,ierr 
   REAL(dbl),    ALLOCATABLE :: s(:)
   COMPLEX(dbl), ALLOCATABLE :: atmp(:,:), u(:,:), vt(:,:)

   IF ( m > SIZE(a,1) ) CALL errore('zmat_rank','Invalid m',ABS(m)+1)
   IF ( n > SIZE(a,2) ) CALL errore('zmat_rank','Invalid n',ABS(n)+1)

   ALLOCATE( atmp(m,n), u(m,m), vt(n,n), s(MIN(m,n)), STAT=ierr )
   IF ( ierr /=0 ) CALL errore('zmat_rank','allocating atmp--s',ABS(ierr))

   !
   ! local copy
   atmp(:,:) = a(1:m,1:n)
   !
   ! svd decomposition
   CALL mat_svd(m,n,atmp,s,u,vt)
   !
   ! TO BE SUBSTITUTED TO A CALL TO LOCATE
   zmat_rank = MIN(m,n)
   DO i=1, MIN(m,n)
       IF ( ABS(s(i)) < toll ) THEN
          zmat_rank = i-1
          EXIT
       ENDIF
   ENDDO
   !
   DEALLOCATE( atmp, u, vt, s, STAT=ierr )
   IF ( ierr /=0 ) CALL errore('zmat_rank','deallocating atmp--s',ABS(ierr))
   !
END FUNCTION zmat_rank


!**********************************************************
   FUNCTION  dmat_rank( m, n, a, toll )
   !**********************************************************
   IMPLICIT NONE
   INTEGER            :: dmat_rank
   INTEGER            :: m,n
   REAL(dbl)          :: a(:,:)
   REAL(dbl)          :: toll
   !
   INTEGER :: i,ierr 
   REAL(dbl), ALLOCATABLE :: s(:)
   REAL(dbl), ALLOCATABLE :: atmp(:,:), u(:,:), vt(:,:)


   IF ( m > SIZE(a,1) ) CALL errore('dmat_rank','Invalid m',ABS(m)+1)
   IF ( n > SIZE(a,2) ) CALL errore('dmat_rank','Invalid n',ABS(n)+1)

   ALLOCATE( atmp(m,n), u(m,m), vt(n,n), s(MIN(m,n)), STAT=ierr )
   IF ( ierr /=0 ) CALL errore('dmat_rank','allocating atmp--s',ABS(ierr))

   !
   ! local copy
   atmp(:,:) = a(1:m,1:n)

   !
   ! svd decomposition
   CALL mat_svd(m,n,atmp,s,u,vt)
   !
   ! TO BE SUBSTITUTED TO A CALL TO LOCATE
   dmat_rank = MIN(m,n)
   DO i=1, MIN(m,n)
       IF ( ABS(s(i)) < toll ) THEN
          dmat_rank = i-1
          EXIT
       ENDIF
   ENDDO
   !
   DEALLOCATE( atmp, u, vt, s, STAT=ierr )
   IF ( ierr /=0 ) CALL errore('dmat_rank','deallocating atmp--s',ABS(ierr))
   !
END FUNCTION dmat_rank


END MODULE util_module


