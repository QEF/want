!
!      Copyright (C) 2004 Arrigo Calzolari, Marco Buongiorno Nardelli
!      Copyright (C) 1999 Marco Buongiorno Nardelli
!
!      This file is distributed under the terms of the
!      GNU General Public License. See the file `License'
!      in the root directory of the present distribution,
!      or http://www.gnu.org/copyleft/gpl.txt .
!
!----------------------------------------------------------------------
      PROGRAM sconductor
!----------------------------------------------------------------------
   
      USE kinds
      USE constants, ONLY: pi
      USE startup_module, ONLY : startup
      USE version_module, ONLY : version_number

      IMPLICIT NONE

!...  Link with BLAS, LAPACK

      INTEGER :: i, j, k, l, m, n, info
      INTEGER, ALLOCATABLE :: ipiv2(:)     ! ipiv2(nmaxc)
      INTEGER :: nmxa, nmxb, nmxc 
      INTEGER :: norb, ne, nterx
      INTEGER :: nmaxa, nmaxb, nmaxc

      REAL(dbl) :: emin, emax, de, enep
      REAL(dbl) :: gamma0, bias
      
      REAL(dbl), ALLOCATABLE :: h00_a(:,:)                 ! h00_a(nmaxa,nmaxa)
      REAL(dbl), ALLOCATABLE :: h01_a(:,:)                 ! h01_a(nmaxa,nmaxa)
      REAL(dbl), ALLOCATABLE :: h00_b(:,:)                 ! h00_b(nmaxb,nmaxb)
      REAL(dbl), ALLOCATABLE :: h01_b(:,:)                 ! h01_b(nmaxb,nmaxb)
      REAL(dbl), ALLOCATABLE :: h00_c(:,:)                 ! h00_c(nmaxc,nmaxc)
      REAL(dbl), ALLOCATABLE :: hci_ac(:,:)                ! hci_ac(nmaxa,nmaxc)
      REAL(dbl), ALLOCATABLE :: hci_cb(:,:)                ! hci_cb(nmaxc,nmaxb)

      REAL(dbl), ALLOCATABLE :: hci_ca(:,:)                ! hci_ca(nmaxc,nmaxa)
      REAL(dbl), ALLOCATABLE :: hci_bc(:,:)                ! hci_bc(nmaxb,nmaxc)
      
      REAL(dbl), ALLOCATABLE :: s00_a(:,:)                 ! s00_a(nmaxa,nmaxa)
      REAL(dbl), ALLOCATABLE :: s01_a(:,:)                 ! s01_a(nmaxa,nmaxa)
      REAL(dbl), ALLOCATABLE :: s00_b(:,:)                 ! s00_b(nmaxb,nmaxb)
      REAL(dbl), ALLOCATABLE :: s01_b(:,:)                 ! s01_b(nmaxb,nmaxb)
      REAL(dbl), ALLOCATABLE :: s00_c(:,:)                 ! s00_c(nmaxc,nmaxc)
      REAL(dbl), ALLOCATABLE :: sci_ac(:,:)                ! sci_ac(nmaxa,nmaxc)
      REAL(dbl), ALLOCATABLE :: sci_cb(:,:)                ! sci_cb(nmaxc,nmaxb)

      REAL(dbl), ALLOCATABLE :: sci_ca(:,:)                ! sci_ca(nmaxc,nmaxa)
      REAL(dbl), ALLOCATABLE :: sci_bc(:,:)                ! sci_bc(nmaxb,nmaxc)

      COMPLEX(dbl), ALLOCATABLE :: c00_a(:,:)             ! c00_a(nmaxa,nmaxa)
      COMPLEX(dbl), ALLOCATABLE :: c01_a(:,:)             ! c01_a(nmaxa,nmaxa)
      COMPLEX(dbl), ALLOCATABLE :: c00_b(:,:)             ! c00_b(nmaxb,nmaxb)
      COMPLEX(dbl), ALLOCATABLE :: c01_b(:,:)             ! c01_b(nmaxb,nmaxb)
      COMPLEX(dbl), ALLOCATABLE :: c00_c(:,:)             ! c00_c(nmaxc,nmaxc)
      COMPLEX(dbl), ALLOCATABLE :: cci_ac(:,:)            ! cci_ac(nmaxa,nmaxc)
      COMPLEX(dbl), ALLOCATABLE :: cci_cb(:,:)            ! cci_cb(nmaxc,nmaxb)

      COMPLEX(dbl), ALLOCATABLE :: cci_ca(:,:)            ! cci_ca(nmaxc,nmaxa)
      COMPLEX(dbl), ALLOCATABLE :: cci_bc(:,:)            ! cci_bc(nmaxb,nmaxc)

      COMPLEX(dbl), ALLOCATABLE :: totA(:,:)              ! totA(nmaxa,nmaxa)
      COMPLEX(dbl), ALLOCATABLE :: totB(:,:)              ! totB(nmaxb,nmaxb)
      COMPLEX(dbl), ALLOCATABLE :: tottA(:,:)             ! tottA(nmaxa,nmaxa)
      COMPLEX(dbl), ALLOCATABLE :: tottB(:,:)             ! tottB(nmaxb,nmaxb)
      COMPLEX(dbl), ALLOCATABLE :: gR(:,:)                ! gR(nmaxc,nmaxc)
      COMPLEX(dbl), ALLOCATABLE :: gL(:,:)                ! gL(nmaxc,nmaxc)
      COMPLEX(dbl), ALLOCATABLE :: gA(:,:)                ! gA(nmaxa,nmaxa)
      COMPLEX(dbl), ALLOCATABLE :: gB(:,:)                ! gB(nmaxb,nmaxb)
      COMPLEX(dbl), ALLOCATABLE :: gAm1(:,:)              ! gAm1(nmaxa,nmaxa)
      COMPLEX(dbl), ALLOCATABLE :: gBm1(:,:)              ! gBm1(nmaxb,nmaxb)
      COMPLEX(dbl), ALLOCATABLE :: gintr(:,:)             ! gintr(nmaxc,nmaxc)
      COMPLEX(dbl), ALLOCATABLE :: ginta(:,:)             ! ginta(nmaxc,nmaxc)
      COMPLEX(dbl), ALLOCATABLE :: gintm1(:,:)            ! gintm1(nmaxc,nmaxc)
      COMPLEX(dbl), ALLOCATABLE :: sLa(:,:)               ! sLa(nmaxc,nmaxc)
      COMPLEX(dbl), ALLOCATABLE :: sRa(:,:)               ! sRa(nmaxc,nmaxc)
      COMPLEX(dbl), ALLOCATABLE :: sLr(:,:)               ! sLr(nmaxc,nmaxc)
      COMPLEX(dbl), ALLOCATABLE :: sRr(:,:)               ! sRr(nmaxc,nmaxc)
      COMPLEX(dbl), ALLOCATABLE :: s1(:,:)                ! s1(nmaxc,nmaxc)
      COMPLEX(dbl), ALLOCATABLE :: s2(:,:)                ! s2(nmaxc,nmaxc)
      COMPLEX(dbl), ALLOCATABLE :: c1(:,:)                ! c1(nmaxc,nmaxa)
      COMPLEX(dbl), ALLOCATABLE :: c2(:,:)                ! c2(nmaxc,nmaxb)
      COMPLEX(dbl), ALLOCATABLE :: tran(:,:)              ! tran(nmaxc,nmaxc)
      COMPLEX(dbl) :: ene, dos, conduct
      COMPLEX(dbl) :: alpha, beta
     
      LOGICAL :: l_overlap
     
!
! ...  Startup
!
       CALL startup(version_number,MAIN_NAME='Sconductor')

!...  Scalar for BLAS calls and other initializations
      alpha = ( 1.d0, 0.d0 )
      beta = ( 0.d0, 0.d0 ) 

!...  Read standard input
      READ ( 5, * ) nmxa, nmxb, nmxc
      READ ( 5, * ) norb
      READ ( 5, * ) ne
      READ ( 5, * ) nterx
      READ ( 5, * ) gamma0, emin, emax
      READ ( 5, *)  l_overlap
      READ ( 5, * ) bias
      
      nmaxa=norb*nmxa
      nmaxb=norb*nmxb
      nmaxc=norb*nmxc

!...  Allocate arrays

      ALLOCATE ( ipiv2(nmaxc) )

      ALLOCATE ( h00_a(nmaxa,nmaxa) )
      ALLOCATE ( h01_a(nmaxa,nmaxa) )
      ALLOCATE ( h00_b(nmaxb,nmaxb) )
      ALLOCATE ( h01_b(nmaxb,nmaxb) )
      ALLOCATE ( h00_c(nmaxc,nmaxc) )
      ALLOCATE ( hci_ac(nmaxa,nmaxc) )
      ALLOCATE ( hci_cb(nmaxc,nmaxb) )

      ALLOCATE ( hci_ca(nmaxc,nmaxa) )
      ALLOCATE ( hci_bc(nmaxb,nmaxc) )

      ALLOCATE ( s00_a(nmaxa,nmaxa) )
      ALLOCATE ( s01_a(nmaxa,nmaxa) )
      ALLOCATE ( s00_b(nmaxb,nmaxb) )
      ALLOCATE ( s01_b(nmaxb,nmaxb) )
      ALLOCATE ( s00_c(nmaxc,nmaxc) )
      ALLOCATE ( sci_ac(nmaxa,nmaxc) )
      ALLOCATE ( sci_cb(nmaxc,nmaxb) )

      ALLOCATE ( sci_ca(nmaxc,nmaxa) )
      ALLOCATE ( sci_bc(nmaxb,nmaxc) )

      ALLOCATE ( c00_a(nmaxa,nmaxa) )
      ALLOCATE ( c01_a(nmaxa,nmaxa) )
      ALLOCATE ( c00_b(nmaxb,nmaxb) )
      ALLOCATE ( c01_b(nmaxb,nmaxb) )
      ALLOCATE ( c00_c(nmaxc,nmaxc) )
      ALLOCATE ( cci_ac(nmaxa,nmaxc) )
      ALLOCATE ( cci_cb(nmaxc,nmaxb) )
      ALLOCATE ( cci_ca(nmaxc,nmaxa) )
      ALLOCATE ( cci_bc(nmaxb,nmaxc) )

      ALLOCATE ( totA(nmaxa,nmaxa) )
      ALLOCATE ( totB(nmaxb,nmaxb) )
      ALLOCATE ( tottA(nmaxa,nmaxa) )
      ALLOCATE ( tottB(nmaxb,nmaxb) )
      ALLOCATE ( gR(nmaxc,nmaxc) )
      ALLOCATE ( gL(nmaxc,nmaxc) )
      ALLOCATE ( gA(nmaxa,nmaxa) )
      ALLOCATE ( gB(nmaxb,nmaxb) )
      ALLOCATE ( gAm1(nmaxa,nmaxa) )
      ALLOCATE ( gBm1(nmaxb,nmaxb) )
      ALLOCATE ( gintr(nmaxc,nmaxc) )
      ALLOCATE ( ginta(nmaxc,nmaxc) )
      ALLOCATE ( gintm1(nmaxc,nmaxc) )
      ALLOCATE ( sLa(nmaxc,nmaxc) )
      ALLOCATE ( sRa(nmaxc,nmaxc) )
      ALLOCATE ( sLr(nmaxc,nmaxc) )
      ALLOCATE ( sRr(nmaxc,nmaxc) )
      ALLOCATE ( s1(nmaxc,nmaxc) )
      ALLOCATE ( s2(nmaxc,nmaxc) )
      ALLOCATE ( c1(nmaxc,nmaxa) )
      ALLOCATE ( c2(nmaxc,nmaxb) )
      ALLOCATE ( tran(nmaxc,nmaxc) )

!...  Set up the layer hamiltonians
!     different layers in different files - read only version

      CALL SreadH( nmaxa, nmaxb, nmaxc, h00_a, h01_a, s00_a, s01_a,   &
                   h00_b, h01_b, s00_b, s01_b, h00_c, s00_c, hci_ac,  &
                   sci_ac, hci_cb, sci_cb , l_overlap )

      DO i = 1, nmaxc
         DO j = 1, nmaxa
            hci_ca(i,j) = hci_ac(j,i)
            sci_ca(i,j) = sci_ac(j,i)
         END DO
      END DO

      DO i = 1, nmaxb
         DO j = 1, nmaxc
            hci_bc(i,j) = hci_cb(j,i)
            sci_bc(i,j) = sci_cb(j,i)
         END DO
      END DO

!...  Loop over the energies

      IF ( ne /= 1 ) THEN
         de = ( emax - emin ) / DBLE( ne - 1 )
      ELSE
         de = 0.d0
      END IF
 
      OPEN ( UNIT=24, FILE='cond.out', STATUS='UNKNOWN', FORM='FORMATTED' )
      OPEN ( UNIT=22, FILE='dos.out', STATUS='UNKNOWN', FORM='FORMATTED' )

      DO n = 1, ne

         enep = ( DBLE(n) - DBLE(ne+1) / 2.d0 ) * de
 
!...     Compute conductance according to Fisher and Lee
!        Compute self-energies following Datta

!...     Retarded

         ene = enep + ( 0.d0, 0.00001d0 )

         DO i = 1, nmaxa
            DO j = 1, nmaxa
               c00_a(i,j) = ( 1.0, 0.0 ) * h00_a(i,j) - ene * s00_a(i,j)
               c01_a(i,j) = ( 1.0, 0.0 ) * h01_a(i,j) - ene * s01_a(i,j)
            END DO
         END DO
         DO i = 1, nmaxb
            DO j = 1, nmaxb
               c00_b(i,j) = ( 1.0, 0.0 ) * h00_b(i,j) - ene * s00_b(i,j)
               c01_b(i,j) = ( 1.0, 0.0 ) * h01_b(i,j) - ene * s01_b(i,j)
            END DO
         END DO
         DO i = 1, nmaxc
            DO j = 1, nmaxc
               c00_c(i,j) = ( 1.0, 0.0 ) * h00_c(i,j) - ene * s00_c(i,j)
            END DO
         END DO
         DO i = 1, nmaxa
            DO j = 1, nmaxc
               cci_ac(i,j) = ( 1.0, 0.0 ) * hci_ac(i,j) - ene * sci_ac(i,j)
            END DO
         END DO
         DO i = 1, nmaxb
            DO j = 1, nmaxc
               cci_bc(i,j) = ( 1.0, 0.0 ) * hci_bc(i,j) - ene * sci_bc(i,j)
            END DO
         END DO
         DO i = 1, nmaxc
            DO j = 1, nmaxa
               cci_ca(i,j) = ( 1.0, 0.0 ) * hci_ca(i,j) - ene * sci_ca(i,j)
            END DO
         END DO
         DO i = 1, nmaxc
            DO j = 1, nmaxb
               cci_cb(i,j) = ( 1.0, 0.0 ) * hci_cb(i,j) - ene * sci_cb(i,j)
            END DO
         END DO

         CALL stransferb( nmaxb, nterx, totB, tottB, c00_b, c01_b, ene+bias )
         CALL Sgreenb( nmaxb, totB, tottB, c00_b, c01_b, ene+bias, gBm1, gB, 1, 1 )

         CALL setv0( nmaxc*nmaxb, c2 )
         CALL zgemm( 'N', 'N', nmaxc, nmaxb, nmaxb, alpha, cci_cb, nmaxc, gB, nmaxb, beta, c2, nmaxc )
         CALL setv0( nmaxc*nmaxc, sRr )
         CALL zgemm( 'N', 'N', nmaxc, nmaxc, nmaxb, alpha, c2, nmaxc, cci_bc, nmaxb, beta, sRr, nmaxc )

         CALL stransfera( nmaxa, nterx, totA, tottA, c00_a, c01_a, ene )
         CALL sgreena( nmaxa, totA, tottA, c00_a, c01_a, ene, gAm1, gA, -1, 1 )

         CALL setv0( nmaxc*nmaxa, c1 )
         CALL zgemm( 'N', 'N', nmaxc, nmaxa, nmaxa, alpha, cci_ca, nmaxc, gA, nmaxa, beta, c1, nmaxc )
         CALL setv0( nmaxc*nmaxc, sLr )
         CALL zgemm( 'N', 'N', nmaxc, nmaxc, nmaxa, alpha, c1, nmaxc, cci_ac, nmaxa, beta, sLr, nmaxc )

!...     Advanced

         DO i = 1, nmaxc
            DO j = 1, nmaxc
               sLa(i,j) = CONJG( sLr(j,i) )
               sRa(i,j) = CONJG( sRr(j,i) )
            END DO
         END DO

         DO i = 1, nmaxc
            DO j = 1, nmaxc
               gL(i,j) = ( 0.0, 1.0 ) * ( sLr(i,j) - sLa(i,j) )
               gR(i,j) = ( 0.0, 1.0 ) * ( sRr(i,j) - sRa(i,j) )
            END DO
         END DO

!...     Construct the conductor green's function

!...     G_C (retarded)

!...     Initialization
         gintm1(:,:) = ( 0.0d0, 0.0d0 )

         DO i = 1, nmaxc
            DO j = 1, nmaxc
               gintm1(i,j) = ( -1.0, 0.0 ) * c00_c(i,j) - sLr(i,j) - sRr(i,j)
            END DO
         END DO
 
         DO i = 1, nmaxc
            DO j = 1, nmaxc
               gintr(i,j) = ( 0.d0, 0.d0 )
               IF ( i == j ) gintr(i,j)=( 1.d0, 0.d0 )
            END DO
         END DO

         CALL zgesv( nmaxc, nmaxc, gintm1, nmaxc, ipiv2, gintr, nmaxc, info )
            IF ( info /= 0 )  CALL errore(' Sconductor ', ' zgesv (I) ', info )

!     G_C (advanced)

         DO i = 1, nmaxc
            DO j = 1, nmaxc
               ginta(i,j) = CONJG( gintr(j,i) )
            END DO
         END DO

         CALL setv0( nmaxc*nmaxc, s1 )
         CALL zgemm( 'N', 'N', nmaxc, nmaxc, nmaxc, alpha, gL, nmaxc, gintr, nmaxc, beta, s1, nmaxc )
         CALL setv0( nmaxc*nmaxc, s2 )
         CALL zgemm( 'N', 'N', nmaxc, nmaxc, nmaxc, alpha, s1, nmaxc, gR, nmaxc, beta, s2, nmaxc )
         CALL setv0( nmaxc*nmaxc, tran )
         CALL zgemm( 'N', 'N', nmaxc, nmaxc, nmaxc, alpha, s2, nmaxc, ginta, nmaxc, beta, tran, nmaxc )

         conduct = ( 0.d0, 0.d0 )         
         DO i=1,nmaxc
            conduct = conduct + tran(i,i)
         END DO
         WRITE ( 24, '(2(1x,f12.6))' ) REAL(ene), REAL(conduct)

!...     Compute density of states for the conductor layer

         dos = ( 0.d0, 0.d0 )         
         DO i = 1, nmaxc
            dos = dos + gintr(i,i)
         END DO
         WRITE ( 22, '(2(1x,f12.6))' ) REAL(ene), -AIMAG(dos)/pi

      END DO  ! end do on the energies

      CLOSE ( 24 )
      CLOSE ( 22 )

      DEALLOCATE ( ipiv2 )

      DEALLOCATE ( h00_a )
      DEALLOCATE ( h01_a )
      DEALLOCATE ( h00_b )
      DEALLOCATE ( h01_b )
      DEALLOCATE ( h00_c )
      DEALLOCATE ( hci_ac )
      DEALLOCATE ( hci_cb )

      DEALLOCATE ( hci_ca )
      DEALLOCATE ( hci_bc )

      DEALLOCATE ( s00_a )
      DEALLOCATE ( s01_a )
      DEALLOCATE ( s00_b )
      DEALLOCATE ( s01_b )
      DEALLOCATE ( s00_c )
      DEALLOCATE ( sci_ac )
      DEALLOCATE ( sci_cb )

      DEALLOCATE ( sci_ca )
      DEALLOCATE ( sci_bc )

      DEALLOCATE ( c00_a )     
      DEALLOCATE ( c01_a )     
      DEALLOCATE ( c00_b )     
      DEALLOCATE ( c01_b )     
      DEALLOCATE ( c00_c )
      DEALLOCATE ( cci_ac )    
      DEALLOCATE ( cci_cb )    

      DEALLOCATE ( cci_ca )    
      DEALLOCATE ( cci_bc )    

      DEALLOCATE ( totA )
      DEALLOCATE ( totB )
      DEALLOCATE ( tottA )
      DEALLOCATE ( tottB )
      DEALLOCATE ( gR )
      DEALLOCATE ( gL )
      DEALLOCATE ( gA )
      DEALLOCATE ( gB )
      DEALLOCATE ( gAm1 )
      DEALLOCATE ( gBm1 )
      DEALLOCATE ( gintr )
      DEALLOCATE ( ginta )
      DEALLOCATE ( gintm1 )
      DEALLOCATE ( sLa )
      DEALLOCATE ( sRa )
      DEALLOCATE ( sLr )
      DEALLOCATE ( sRr )
      DEALLOCATE ( s1 )
      DEALLOCATE ( s2 )
      DEALLOCATE ( c1 )
      DEALLOCATE ( c2 )
      DEALLOCATE ( tran )


      END 
