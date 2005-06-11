! 
!      Copyright (C) 2004 WanT Group
!      Copyright (C) 1999 Marco Buongiorno Nardelli 
! 
!      This file is distributed under the terms of the 
!      GNU General Public License. See the file `License' 
!      in the root directory of the present distribution, 
!      or http://www.gnu.org/copyleft/gpl.txt . 
! 
!******************************************
   PROGRAM bulk
   !******************************************
   USE kinds   
   USE constants, ONLY: PI, ZERO, CZERO, CONE, CI, EPS_m5
   USE startup_module, ONLY : startup
   USE version_module, ONLY : version_number
   USE cleanup_module, ONLY : cleanup
   IMPLICIT NONE

      INTEGER :: ierr
      INTEGER :: nmx, natmax, norb, niterx
      INTEGER :: ne, model, nmax
      INTEGER :: i, j, k, l, m, n

      REAL(dbl) :: rcut
      REAL(dbl) :: efieldx, efieldy, efieldz
      REAL(dbl), ALLOCATABLE :: r(:,:)        ! r( 3, natmax )
      REAL(dbl), ALLOCATABLE :: h0(:,:)       ! h0( nmax, nmax )
      REAL(dbl), ALLOCATABLE :: h1(:,:)       ! h1( nmax, nmax )
      REAL(dbl) :: emin, emax, de, enep

      COMPLEX(dbl), ALLOCATABLE :: tot(:,:)  ! tot( nmax, nmax )
      COMPLEX(dbl), ALLOCATABLE :: tott(:,:) ! tott( nmax, nmax )
      COMPLEX(dbl), ALLOCATABLE :: g(:,:)    ! g( nmax, nmax )
      COMPLEX(dbl), ALLOCATABLE :: gR(:,:)   ! gR( nmax,nmax )
      COMPLEX(dbl), ALLOCATABLE :: gL(:,:)   ! gL( nmax, nmax )
      COMPLEX(dbl), ALLOCATABLE :: gAa(:,:)  ! gAa( nmax, nmax )
      COMPLEX(dbl), ALLOCATABLE :: gAr(:,:)  ! gAr( nmax, nmax )
      COMPLEX(dbl), ALLOCATABLE :: sLa(:,:)  ! sLa( nmax, nmax )
      COMPLEX(dbl), ALLOCATABLE :: sRa(:,:)  ! sRa( nmax, nmax )
      COMPLEX(dbl), ALLOCATABLE :: sLr(:,:)  ! sLr( nmax, nmax )
      COMPLEX(dbl), ALLOCATABLE :: sRr(:,:)  ! sRr( nmax, nmax )
      COMPLEX(dbl), ALLOCATABLE :: s1(:,:)   ! s1( nmax, nmax )
      COMPLEX(dbl), ALLOCATABLE :: s2(:,:)   ! s2( nmax, nmax )
      COMPLEX(dbl), ALLOCATABLE :: c1(:,:)   ! c1( nmax, nmax )
      COMPLEX(dbl), ALLOCATABLE :: tran(:,:) ! tran( nmax, nmax )
      COMPLEX(dbl) :: ene, dos, conduct
     

      EXTERNAL TB_hamiltonian, transfer, green

      NAMELIST /INPUT_BULK/ nmx, norb, ne, niterx, emin, emax, &
                            efieldx, efieldy, efieldz, model
      
!
! ... Startup
!
      CALL startup(version_number,MAIN_NAME='bulk')


!...  Read namelist
      nmx = 0
      norb = 1
      ne = 1000
      niterx = 200
      emin = -10.0_dbl
      emax =  10.0_dbl
      efieldx = ZERO
      efieldy = ZERO
      efieldy = ZERO
      model = 4

      READ( 5, input_bulk, IOSTAT=ierr)
      IF ( ierr/= 0) CALL errore('bulk','reading input namelist',ABS(ierr))

      IF ( nmx <= 0) CALL errore('bulk','Invalid NMX',1)
      IF ( emax <= emin ) CALL errore('bulk','Invalid EMIN EMAX',1)
      IF ( ne <= 0 ) CALL errore('bulk','Invalid NE',1)
      IF ( niterx <= 0 ) CALL errore('bulk','Invalid NITERX',1)
      IF ( model < 1 .OR. model > 4 ) CALL  errore('bulk','Invalid MODEL',1)


!...  Hamiltonian dimension
      nmax = norb * nmx
      natmax = 2 * nmx
      ALLOCATE ( r( 3, natmax ) )
      rcut = ZERO
      r(:,:) = ZERO

      IF ( model /= 4 ) THEN

!...    Reads in the coordinates of the principal layers (NB there are
!       two principal layers per superlayer: nmax atoms per layer for a
!       total of natmax atoms)

        READ ( 5, * ) rcut
        DO i = 1, natmax
           READ ( 5, * ) ( r( j, i ), j = 1, 3 )
        ENDDO

      ENDIF

!...  Set up the layer hamiltonians

      ALLOCATE ( h0( nmax, nmax ) )
      ALLOCATE ( h1( nmax, nmax ) )

      CALL TB_hamiltonian( r, natmax, rcut, h0, h1, nmx, nmax, ZERO,      &
                       model, efieldx, efieldy, efieldz )

!...  Loop over the energies

      ALLOCATE ( tot( nmax, nmax ) )
      ALLOCATE ( tott( nmax, nmax ) )
      ALLOCATE ( g( nmax, nmax ) )
      ALLOCATE ( gR( nmax,nmax ) )
      ALLOCATE ( gL( nmax, nmax ) )
      ALLOCATE ( gAa( nmax, nmax ) )
      ALLOCATE ( gAr( nmax, nmax ) )
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

         enep = emin + DBLE(n -1) * de
 
!...     Compute conductance according to Fisher and Lee

!...     Retarded
         ene = enep + EPS_m5 * CI

         CALL transfer( nmax, niterx, tot, tott, h0, h1, ene )
         CALL green( nmax, tot, tott, h0, h1, ene, gAr, 0, 1 ) 
 
!...     Compute S_Lr and S_Rr
         c1(:,:) = h1( :, : )

         sRr(:,:) = CZERO
         CALL zgemm( 'N', 'N', nmax, nmax, nmax, CONE, c1, nmax, tot, nmax,  &
                     CZERO, sRr, nmax )

         sLr(:,:) = CZERO
         CALL zgemm( 'C', 'N', nmax, nmax, nmax, CONE, c1, nmax, tott, nmax, &
                     CZERO, sLr, nmax )


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
               gL(i,j) = CI * ( sLr(i,j) - sLa(i,j) )
               gR(i,j) = CI * ( sRr(i,j) - sRa(i,j) )
            END DO
         END DO


         s1 = CZERO
         CALL zgemm( 'N', 'N', nmax, nmax, nmax, CONE, gL, nmax, gAr, nmax,   &
                     CZERO, s1, nmax )

         s2 = CZERO
         CALL zgemm( 'N', 'N', nmax, nmax, nmax, CONE, s1, nmax, gR, nmax,    &
                     CZERO, s2, nmax )

         tran = CZERO
         CALL zgemm( 'N', 'N', nmax, nmax, nmax, CONE, s2, nmax, gAa, nmax,   &
                     CZERO, tran, nmax )

         conduct = CZERO


         DO i = 1, nmax
            conduct = conduct + tran(i,i)
         END DO
         WRITE( 24, '( 2(1x,f12.6) )' ) REAL(ene), REAL(conduct)


!...     Compute density of states for the bulk layer


         dos = CZERO
         DO i=1,nmax
            dos = dos + gAr(i,i)
         END DO
         WRITE( 22, '( 2(1x,f12.6) )' ) REAL(ene), -AIMAG(dos) / PI


      END DO  ! end do on the energies

      CLOSE ( 24 )
      CLOSE ( 22 )
      
      DEALLOCATE ( r, STAT=ierr )
          IF( ierr /=0 ) CALL errore(' bulk ', ' deallocating r', ABS(ierr) )
      DEALLOCATE ( h0, STAT=ierr )
          IF( ierr /=0 ) CALL errore(' bulk ', ' deallocating h0', ABS(ierr) )
      DEALLOCATE ( h1, STAT=ierr )
          IF( ierr /=0 ) CALL errore(' bulk ', ' deallocating h1', ABS(ierr) )
      DEALLOCATE ( tot, STAT=ierr )
          IF( ierr /=0 ) CALL errore(' bulk ', ' deallocating tot', ABS(ierr) )
      DEALLOCATE ( tott, STAT=ierr )
          IF( ierr /=0 ) CALL errore(' bulk ', ' deallocating tott', ABS(ierr) )
      DEALLOCATE ( g, STAT=ierr )
          IF( ierr /=0 ) CALL errore(' bulk ', ' deallocating g', ABS(ierr) )
      DEALLOCATE ( gR, STAT=ierr)
          IF( ierr /=0 ) CALL errore(' bulk ', ' deallocating gR', ABS(ierr) )
      DEALLOCATE ( gL, STAT=ierr )
          IF( ierr /=0 ) CALL errore(' bulk ', ' deallocating gL', ABS(ierr) )
      DEALLOCATE ( gAa, STAT=ierr )
          IF( ierr /=0 ) CALL errore(' bulk ', ' deallocating gAa', ABS(ierr) )
      DEALLOCATE ( gAr, STAT=ierr )
          IF( ierr /=0 ) CALL errore(' bulk ', ' deallocating gAr', ABS(ierr) )
      DEALLOCATE ( sLa, STAT=ierr )
          IF( ierr /=0 ) CALL errore(' bulk ', ' deallocating sLa', ABS(ierr) )
      DEALLOCATE ( sRa, STAT=ierr )
          IF( ierr /=0 ) CALL errore(' bulk ', ' deallocating sRa', ABS(ierr) )
      DEALLOCATE ( sLr, STAT=ierr )
          IF( ierr /=0 ) CALL errore(' bulk ', ' deallocating sLr', ABS(ierr) )
      DEALLOCATE ( sRr, STAT=ierr )
          IF( ierr /=0 ) CALL errore(' bulk ', ' deallocating sRr', ABS(ierr) )
      DEALLOCATE ( s1, STAT=ierr )
          IF( ierr /=0 ) CALL errore(' bulk ', ' deallocating s1', ABS(ierr) )
      DEALLOCATE ( s2, STAT=ierr )
          IF( ierr /=0 ) CALL errore(' bulk ', ' deallocating s2', ABS(ierr) )
      DEALLOCATE ( c1, STAT=ierr )
          IF( ierr /=0 ) CALL errore(' bulk ', ' deallocating c1', ABS(ierr) )
      DEALLOCATE ( tran, STAT=ierr )
          IF( ierr /=0 ) CALL errore(' bulk ', ' deallocating tran', ABS(ierr) )

      CALL cleanup()

      STOP '*** THE END *** (bulk.x)'
      END 

