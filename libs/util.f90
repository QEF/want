!
! Copyright (C) 2004 Carlo Cavazzoni, Andrea Ferretti
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
! SUBROUTINE  zmat_mul( c, a, opa, b, opb, n )
! LOGICAL FUNCTION  zmat_unitary( z [,side] [,toll] )
! SUBROUTINE  gv_indexes( igv, igsort, npwk, nr1, nr2, nr3, ninvpw, nindpw )
! 
! </INFO>
!


PUBLIC :: zmat_pack
PUBLIC :: zmat_unpack
PUBLIC :: zmat_mul
PUBLIC :: zmat_unitary
PUBLIC :: gv_indexes

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
   SUBROUTINE zmat_mul( c, a, opa, b, opb, n )
   !**********************************************************
    IMPLICIT NONE
    COMPLEX(dbl), INTENT(IN)  :: a(:,:)
    COMPLEX(dbl), INTENT(IN)  :: b(:,:)
    COMPLEX(dbl), INTENT(OUT) :: c(:,:)
    CHARACTER, INTENT(IN) :: opa, opb
    INTEGER, INTENT(IN) :: n
    INTEGER :: i, j, k

    IF( opb /= 'N' .AND. opb /= 'C' ) THEN
      CALL errore( ' zmat_mul ', ' argument value not allowed ', 5 )
    END IF
    IF( n < 20 ) THEN
      IF( ( opb == 'N' ) .AND. ( opa == 'N' ) ) THEN
        DO j = 1, n
          DO i = 1, n
            c(i,j) = CZERO
              DO k = 1, n
                c(i,j) = c(i,j) + a(i,k) * b(k,j)
              END DO
          END DO
        END DO
      ELSE IF( ( opb == 'C' ) .AND. ( opa == 'N' ) ) THEN
        DO j = 1, n
          DO i = 1, n
            c(i,j) = CZERO
              DO k = 1, n
                c(i,j) = c(i,j) + a(i,k) * CONJG( b(j,k) )
              END DO
          END DO
        END DO
      ELSE IF( ( opb == 'N' ) .AND. ( opa == 'C' ) ) THEN
        DO j = 1, n
          DO i = 1, n
            c(i,j) = CZERO
              DO k = 1, n
                c(i,j) = c(i,j) + CONJG( a(k,i) )* b(k,j) 
              END DO
          END DO
        END DO
      ELSE IF( ( opb == 'C' ) .AND. ( opa == 'C' ) ) THEN
        DO j = 1, n
          DO i = 1, n
            c(i,j) = CZERO
              DO k = 1, n
                c(i,j) = c(i,j) + CONJG( a(k,i) )* CONJG( b(j,k) )
              END DO
          END DO
        END DO
      END IF
    ELSE
      CALL ZGEMM( opa, opb, n, n, n, CONE, a(1,1), &
      SIZE(a,1), b(1,1), SIZE(b,1), CZERO, c(1,1), SIZE(c,1) )
    END IF
    RETURN
  END SUBROUTINE


!**********************************************************
   SUBROUTINE gv_indexes( igv, igsort, npwk, nr1, nr2, nr3, ninvpw, nindpw )
   !**********************************************************
    IMPLICIT NONE
    INTEGER, INTENT(IN) :: igv(:,:)
    INTEGER, INTENT(IN) :: igsort(:)
    INTEGER, INTENT(IN) :: npwk, nr1, nr2, nr3
    INTEGER, OPTIONAL, INTENT(OUT) :: ninvpw(0:)
    INTEGER, OPTIONAL, INTENT(OUT) :: nindpw(:)
    INTEGER :: igk, np, nx, ny, nz, npoint

    DO np = 1, npwk

      igk = igsort( np )
      IF ( igv(1,igk) >= 0 ) nx = igv(1,igk) + 1
      IF ( igv(1,igk) <  0 ) nx = igv(1,igk) + 1 + nr1
      IF ( igv(2,igk) >= 0 ) ny = igv(2,igk) + 1
      IF ( igv(2,igk) <  0 ) ny = igv(2,igk) + 1 + nr2
      IF ( igv(3,igk) >= 0 ) nz = igv(3,igk) + 1
      IF ( igv(3,igk) <  0 ) nz = igv(3,igk) + 1 + nr3

      npoint = nx + (ny-1)*nr1 + (nz-1)*nr1*nr2

      IF( PRESENT( nindpw ) ) nindpw(np) = npoint  ! index
      IF( PRESENT( ninvpw ) ) ninvpw(npoint) = np  ! index

    END DO
   
    RETURN
  END SUBROUTINE

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


