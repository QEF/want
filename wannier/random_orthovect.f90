!
! Copyright (C) 2005 WanT Group
!
! This file is distributed under the terms of the
! GNU General Public License. See the file `License'
! in the root directory of the present distribution,
! or http://www.gnu.org/copyleft/gpl.txt .
!
!********************************************************
   SUBROUTINE random_orthovect( n, dim, ldx, vect)
   !********************************************************
   !
   ! This subroutines generates N randomized orthonormal vectors 
   ! with dimension DIM. Vectors are orthonormalized throught a
   ! simple Gram Schmidt procedure.
   ! The rndm() function is used as random number generator.
   !
   USE kinds, ONLY : dbl
   USE constants, ONLY : ZERO, ONE, CZERO, EPS_m6
   USE util_module, ONLY : mat_mul
   IMPLICIT NONE

   !
   ! input variables
   !
   INTEGER,         INTENT(in)  :: n, dim, ldx 
   COMPLEX(dbl),    INTENT(out) :: vect(ldx, n)

   !
   ! local variables
   !
   INTEGER, PARAMETER :: nmax = 10     ! after NMAX attempt if fails
   INTEGER   :: i, j, ierr
   INTEGER   :: ncount
   LOGICAL   :: found
   REAL(dbl) :: norm
   COMPLEX(dbl), ALLOCATABLE :: aux(:,:)
   !
   ! end of declariations
   !

!
!------------------------------
! main body 
!------------------------------
!

   ALLOCATE( aux(dim,1), STAT=ierr ) 
      IF (ierr/=0) CALL errore('random_orthovect','allocating aux',ABS(ierr))
   
   vect(:,:) = CZERO 

   DO i = 1, n
       
       ncount = 0
       found = .FALSE.
       DO WHILE( ncount < nmax .AND. .NOT. found )
           
           ncount = ncount + 1
           !
           ! generate a new non-null vector
           CALL random_vect( dim, vect(1,i) ) 
           !
           ! compute the scalar product and 
           ! check if the i-1 vectors and the new one
           ! are linearly indipendent
           !
           CALL mat_mul( aux, vect(1:dim, 1:i), 'C', vect(1:dim,i:i), 'N', i, 1, dim )
           !
           ! doing this way, aux(1:i-1) contains the projections on the i-1 ON already
           ! found vectors, while aux(i,i) is the square norm of the new vector
           ! If the sum of the squared projections is lower than the squared norm
           ! vectors are then linearly indip
           !
           norm = ZERO
           DO j=1, i-1
              norm = norm + REAL( aux(j,1) * CONJG(aux(j,1)) )
           ENDDO
           IF ( norm < REAL (aux(i,1)) - EPS_m6 ) THEN
                found = .TRUE.
                ! 
                ! then we orthonormalize the new vector
                ! 
                DO j = 1, i-1 
                     vect(:,i) = vect(:,i) - aux(j,1) * vect(:,j)
                ENDDO
                norm = ZERO
                DO j=1, dim
                   norm = norm + REAL( vect(j,i) * CONJG(vect(j,i)) )
                ENDDO
                norm = ONE / SQRT(norm)
                vect (:,i) = vect(:,i) * norm
           ENDIF
       ENDDO         

       IF ( .NOT. found )  &
            CALL errore('random_orthovect','maximum number of attempts reached',i)
           
   ENDDO

   !
   ! clean
   DEALLOCATE( aux, STAT=ierr ) 
      IF (ierr/=0) CALL errore('random_orthovect','deallocating aux',ABS(ierr))

END SUBROUTINE random_orthovect


!********************************************************
   SUBROUTINE random_vect(ndim, z)
   !********************************************************
   !
   ! generates a non-null random vector
   !
   USE kinds, ONLY : dbl
   USE constants, ONLY : ZERO, EPS_m6
   IMPLICIT NONE
   INTEGER,      INTENT(in) :: ndim
   COMPLEX(dbl), INTENT(out):: z(ndim)

   ! external function
   REAL(dbl) :: rndm  

   INTEGER, PARAMETER :: nmax = 10     ! after NMAX attempt if fails
   INTEGER   :: i, ncount
   LOGICAL   :: found
   REAL(dbl) :: norm

   IF ( ndim <= 0 ) RETURN
 
   ncount = 0
   found = .FALSE.  
   DO WHILE( ncount < nmax .AND. .NOT. found )

      ncount = ncount + 1
      !
      ! a new vector is generated
      norm = ZERO
      DO i = 1, ndim
          z(i) = CMPLX( rndm(), rndm(), dbl )
          norm = norm + REAL ( z(i) * CONJG( z(i) )  )
      ENDDO
      !
      ! check on the norm
      IF ( norm > EPS_m6 ) found = .TRUE.
   ENDDO

   IF ( .NOT. found ) CALL errore('random_vect','maximum number of attempts reached',nmax)
   RETURN   
END SUBROUTINE random_vect

        
   

   
   

   
