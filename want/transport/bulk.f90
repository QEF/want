!=---------------------------------------------------------------------=
      PROGRAM bulk
!=---------------------------------------------------------------------=

      IMPLICIT NONE

      INTEGER :: nmx, natmax, norb, nterx
      INTEGER :: ne, model, nmax
      INTEGER :: i, j, k, l, m, n

      REAL*8 :: pi, gamma0, rcut
      REAL*8 :: bias, efieldx, efieldy, efieldz
      REAL*8, ALLOCATABLE :: r(:,:)        ! r( 3, natmax )
      REAL*8, ALLOCATABLE :: h0(:,:)       ! h0( nmax, nmax )
      REAL*8, ALLOCATABLE :: h1(:,:)       ! h1( nmax, nmax )
      REAL*8 :: emin, emax, de, enep

      COMPLEX*16, ALLOCATABLE :: tot(:,:)  ! tot( nmax, nmax )
      COMPLEX*16, ALLOCATABLE :: tott(:,:) ! tott( nmax, nmax )
      COMPLEX*16, ALLOCATABLE :: g(:,:)    ! g( nmax, nmax )
      COMPLEX*16, ALLOCATABLE :: gR(:,:)   ! gR( nmax,nmax )
      COMPLEX*16, ALLOCATABLE :: gL(:,:)   ! gL( nmax, nmax )
      COMPLEX*16, ALLOCATABLE :: gAa(:,:)  ! gAa( nmax, nmax )
      COMPLEX*16, ALLOCATABLE :: gAr(:,:)  ! gAr( nmax, nmax )
      COMPLEX*16, ALLOCATABLE :: gAm1(:,:) ! gAm1( nmax, nmax )
      COMPLEX*16, ALLOCATABLE :: sLa(:,:)  ! sLa( nmax, nmax )
      COMPLEX*16, ALLOCATABLE :: sRa(:,:)  ! sRa( nmax, nmax )
      COMPLEX*16, ALLOCATABLE :: sLr(:,:)  ! sLr( nmax, nmax )
      COMPLEX*16, ALLOCATABLE :: sRr(:,:)  ! sRr( nmax, nmax )
      COMPLEX*16, ALLOCATABLE :: s1(:,:)   ! s1( nmax, nmax )
      COMPLEX*16, ALLOCATABLE :: s2(:,:)   ! s2( nmax, nmax )
      COMPLEX*16, ALLOCATABLE :: c1(:,:)   ! c1( nmax, nmax )
      COMPLEX*16, ALLOCATABLE :: tran(:,:) ! tran( nmax, nmax )
      COMPLEX*16 :: ene, dos, conduct
      COMPLEX*16 :: alpha, beta
     
      PARAMETER ( pi = 3.14159265358979323846d0 )

      EXTERNAL hamiltonian, transfer, green, setv0
      

!...  Scalar for BLAS calls
      alpha = (1.d0,0.d0)
      beta = (0.d0,0.d0)

      READ ( 5, * ) nmx
      READ ( 5, * ) norb

!...  Hamiltonian dimension
      nmax = norb * nmx
      natmax = 2 * nmx

      READ ( 5, * ) ne
      READ ( 5, * ) nterx
      READ ( 5, * ) gamma0, emin, emax
      READ ( 5, * ) bias, efieldx, efieldy, efieldz
      READ ( 5, * ) model
!     WRITE ( 6, * ) 'nterx= ', nterx, 'ne= ', ne
!     WRITE ( 6, * ) 'gamma0= ', gamma0, 'emin= ', emin, 'emax= ', emax
!     WRITE ( 6, * ) 'model= ', model 

      ALLOCATE ( r( 3, natmax ) )
      rcut = 0.0d00
      r(:,:) = 0.0d0

      IF ( model /= 4 ) THEN

!...    Reads in the coordinates of the principal layers (NB there are
!       two principal layers per superlayer: nmax atoms per layer for a
!       total of natmax atoms)

        READ ( 5, * ) rcut
        DO i = 1, natmax
           READ ( 5, * ) ( r( j, i ), j = 1, 3 )
        END DO

      END IF

!...  Set up the layer hamiltonians

      ALLOCATE ( h0( nmax, nmax ) )
      ALLOCATE ( h1( nmax, nmax ) )

      CALL hamiltonian( r, natmax, rcut, h0, h1, nmx, nmax, gamma0,      &
                       model, efieldx, efieldy, efieldz )

!...  Loop over the energies

      ALLOCATE ( tot( nmax, nmax ) )
      ALLOCATE ( tott( nmax, nmax ) )
      ALLOCATE ( g( nmax, nmax ) )
      ALLOCATE ( gR( nmax,nmax ) )
      ALLOCATE ( gL( nmax, nmax ) )
      ALLOCATE ( gAa( nmax, nmax ) )
      ALLOCATE ( gAr( nmax, nmax ) )
      ALLOCATE ( gAm1( nmax, nmax ) )
      ALLOCATE ( sLa( nmax, nmax ) )
      ALLOCATE ( sRa( nmax, nmax ) )
      ALLOCATE ( sLr( nmax, nmax ) )
      ALLOCATE ( sRr( nmax, nmax ) )
      ALLOCATE ( s1( nmax, nmax ) )
      ALLOCATE ( s2( nmax, nmax ) )
      ALLOCATE ( c1( nmax, nmax ) )
      ALLOCATE ( tran( nmax, nmax ) )

      OPEN ( UNIT=24, FILE='cond.out', STATUS='UNKNOWN', FORM='FORMATTED' )
      OPEN ( UNIT=22, FILE='dos.out', STATUS='UNKNOWN', FORM='FORMATTED' )

      de = ( emax - emin ) / DBLE(ne-1)
 
      DO n = 1, ne

         enep = ( DBLE(n) - DBLE(ne+1) / 2.d0 ) * de
 
!...     Compute conductance according to Fisher and Lee

!...     Retarded
         ene = enep + ( 0.d0, 0.00001d0 )

         CALL transfer( nmax, nterx, tot, tott, h0, h1, ene )
         CALL green( nmax, tot, tott, h0, h1, ene, gAm1, gAr, 0, 1 ) 
 
!...     Compute S_Lr and S_Rr

         DO i = 1, nmax
            DO j = 1, nmax
               c1(i,j) = ( 1.d0, 0.d0 ) * h1( i, j )
            END DO
         END DO

         CALL setv0( nmax*nmax, sRr )
         CALL zgemm( 'N', 'N', nmax, nmax, nmax, alpha, c1, nmax, tot, nmax,  &
                     beta, sRr, nmax )

         CALL setv0( nmax*nmax, sLr )
         CALL zgemm( 'C', 'N', nmax, nmax, nmax, alpha, c1, nmax, tott, nmax, &
                     beta, sLr, nmax )

!...     Advanced
         DO i = 1, nmax
            DO j = 1, nmax
               gAa(i,j) = CONJG( gAr(j,i) )
            END DO
         END DO
 
!...     Compute S_La and S_Ra

         DO i = 1, nmax
            DO j = 1, nmax
               sLa(i,j) = CONJG( sLr(j,i) )
               sRa(i,j) = CONJG( sRr(j,i) )
            END DO
         END DO

         DO i = 1, nmax
            DO j = 1, nmax
               gL(i,j) = ( 0.0d0, 1.0d0 ) * ( sLr(i,j) - sLa(i,j) )
               gR(i,j) = ( 0.0d0, 1.0d0 ) * ( sRr(i,j) - sRa(i,j) )
            END DO
         END DO

         CALL setv0( nmax*nmax, s1 )
         CALL zgemm( 'N', 'N', nmax, nmax, nmax, alpha, gL, nmax, gAr, nmax,   &
                     beta, s1, nmax )

         CALL setv0(nmax*nmax,s2)
         CALL zgemm( 'N', 'N', nmax, nmax, nmax, alpha, s1, nmax, gR, nmax,    &
                     beta, s2, nmax )

         CALL setv0( nmax*nmax, tran )
         CALL zgemm( 'N', 'N', nmax, nmax, nmax, alpha, s2, nmax, gAa, nmax,   &
                     beta, tran, nmax )

         conduct = ( 0.d0, 0.d0 )         


         DO i = 1, nmax
            conduct = conduct + tran(i,i)
         END DO
         WRITE( 24, '( 2(1x,f12.6) )' ) REAL(ene), REAL(conduct)


!...     Compute density of states for the bulk layer


         dos = ( 0.d0, 0.d0 )         
         DO i=1,nmax
            dos = dos + gAr(i,i)
         END DO
         WRITE( 22, '( 2(1x,f12.6) )' ) REAL(ene), -AIMAG(dos) / pi


      END DO  ! end do on the energies

      CLOSE ( 24 )
      CLOSE ( 22 )
      
      DEALLOCATE ( r )
      DEALLOCATE ( h0 )
      DEALLOCATE ( h1 )
      DEALLOCATE ( tot )
      DEALLOCATE ( tott )
      DEALLOCATE ( g )
      DEALLOCATE ( gR)
      DEALLOCATE ( gL )
      DEALLOCATE ( gAa )
      DEALLOCATE ( gAr )
      DEALLOCATE ( gAm1 )
      DEALLOCATE ( sLa )
      DEALLOCATE ( sRa )
      DEALLOCATE ( sLr )
      DEALLOCATE ( sRr )
      DEALLOCATE ( s1 )
      DEALLOCATE ( s2 )
      DEALLOCATE ( c1 )
      DEALLOCATE ( tran )

      STOP
      END 
