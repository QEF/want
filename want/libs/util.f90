MODULE util

  USE kinds

  IMPLICIT NONE
  SAVE

CONTAINS

  SUBROUTINE zmat_pack( zp, z, n )
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

  SUBROUTINE zmat_unpack( z, zp, n )
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


  SUBROUTINE zmat_mul( c, a, opa, b, opb, n )
    COMPLEX(dbl), INTENT(IN)  :: a(:,:)
    COMPLEX(dbl), INTENT(IN)  :: b(:,:)
    COMPLEX(dbl), INTENT(OUT) :: c(:,:)
    CHARACTER, INTENT(IN) :: opa, opb
    INTEGER, INTENT(IN) :: n
    INTEGER :: i, j, k
    COMPLEX(dbl) :: czero, cone
    PARAMETER ( czero = ( 0.0d0, 0.0d0 ) )
    PARAMETER ( cone  = ( 1.0d0, 0.0d0 ) )
    IF( opa /= 'N' ) THEN
      CALL errore( ' zmat_mul ', ' argument value not allowed ', 3 )
    END IF
    IF( opb /= 'N' .AND. opb /= 'C' ) THEN
      CALL errore( ' zmat_mul ', ' argument value not allowed ', 5 )
    END IF
    IF( n < 20 ) THEN
      IF( ( opb == 'N' ) .AND. ( opa == 'N' ) ) THEN
        DO j = 1, n
          DO i = 1, n
            c(i,j) = ( 0.d0, 0.d0 )
              DO k = 1, n
                c(i,j) = c(i,j) + a(i,k) * b(k,j)
              END DO
          END DO
        END DO
      ELSE IF( ( opb == 'C' ) .AND. ( opa == 'N' ) ) THEN
        DO j = 1, n
          DO i = 1, n
            c(i,j) = ( 0.d0, 0.d0 )
              DO k = 1, n
                c(i,j) = c(i,j) + a(i,k) * CONJG( b(j,k) )
              END DO
          END DO
        END DO
      END IF
    ELSE
      CALL ZGEMM( opa, opb, n, n, n, cone, a(1,1), &
      SIZE(a,1), b(1,1), SIZE(b,1), czero, c(1,1), SIZE(c,1) )
    END IF
    RETURN
  END SUBROUTINE


  SUBROUTINE gv_indexes( igv, igsort, npwk, nr1, nr2, nr3, ninvpw, nindpw )
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


END MODULE
