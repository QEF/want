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
      PROGRAM conductor
!----------------------------------------------------------------------
   
      USE kinds
      USE constants, ONLY: pi
      USE startup_module, ONLY : startup
      USE version_module, ONLY : version_number
      USE cleanup_module, ONLY : cleanup

      IMPLICIT NONE

!...  Link with BLAS, LAPACK

      INTEGER :: ierr
      INTEGER :: i, j, k, l, m, n, info
      INTEGER, ALLOCATABLE :: ipiv2(:)     ! ipiv2(nmaxc)
      INTEGER :: nmxa, nmxb, nmxc 
      INTEGER :: norb, ne, nterx
      INTEGER :: nmaxa, nmaxb, nmaxc

      REAL(dbl) :: emin, emax, de, enep
      REAL(dbl) :: bias, gamma0
      
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
     
      NAMELIST /INPUT_CONDUCTOR/ nmxa, nmxb, nmxc, norb, ne, nterx, emin,  &
                                 emax, l_overlap, bias
!
! ... Startup
!
      CALL startup(version_number,MAIN_NAME='conductor')

!...  Scalar for BLAS calls and other initializations
      alpha = ( 1.d0, 0.d0 )
      beta = ( 0.d0, 0.d0 ) 

!...  Read standard input
!     READ ( 5, * ) nmxa, nmxc, nmxb
!     READ ( 5, * ) norb
!     READ ( 5, * ) ne
!     READ ( 5, * ) nterx
!     READ ( 5, * ) gamma0, emin, emax
!     READ ( 5, *)  l_overlap
!     READ ( 5, * ) bias

!...  Read namelist
      nmxa = 0
      nmxb = 0
      nmxc = 0
      norb = 1
      ne = 1000
      nterx = 200
      emin = -10.0
      emax =  10.0
      l_overlap = .FALSE.
      bias = 0.0

      READ( 5, input_conductor, IOSTAT=ierr)
      IF ( ierr/= 0) CALL errore('conductor','reading input namelist',ABS(ierr))
      
      IF ( nmxa <= 0) CALL errore('conductor','Invalid NMXA',1)
      IF ( nmxb <= 0) CALL errore('conductor','Invalid NMXB',1)
      IF ( nmxc <= 0) CALL errore('conductor','Invalid NMXB',1)
      IF ( emax <= emin ) CALL errore('conductor','Invalid EMIN EMAX',1)
      IF ( ne <= 0 ) CALL errore('conductor','Invalid NE',1)
      IF ( nterx <= 0 ) CALL errore('conductor','Invalid NTERX',1)
      
      nmaxa=norb*nmxa
      nmaxb=norb*nmxb
      nmaxc=norb*nmxc

!...  Allocate arrays

      ALLOCATE ( ipiv2(nmaxc), STAT=ierr )
           IF( ierr /=0 ) CALL errore(' conductor ', ' allocating ipiv2 ', 1 ) 

      ALLOCATE ( h00_a(nmaxa,nmaxa), STAT=ierr )
           IF( ierr /=0 ) CALL errore(' conductor ', ' allocating h00_a ', 1 )
      ALLOCATE ( h01_a(nmaxa,nmaxa), STAT=ierr )
           IF( ierr /=0 ) CALL errore(' conductor ', ' allocating h01_a ', 1 )
      ALLOCATE ( h00_b(nmaxb,nmaxb), STAT=ierr )
           IF( ierr /=0 ) CALL errore(' conductor ', ' allocating h00_b ', 1 )
      ALLOCATE ( h01_b(nmaxb,nmaxb), STAT=ierr )
           IF( ierr /=0 ) CALL errore(' conductor ', ' allocating h01_b ', 1 )
      ALLOCATE ( h00_c(nmaxc,nmaxc), STAT=ierr )
           IF( ierr /=0 ) CALL errore(' conductor ', ' allocating h00_c ', 1 )
      ALLOCATE ( hci_ac(nmaxa,nmaxc), STAT=ierr )
           IF( ierr /=0 ) CALL errore(' conductor ', ' allocating hci_ac ', 1 )
      ALLOCATE ( hci_cb(nmaxc,nmaxb), STAT=ierr )
           IF( ierr /=0 ) CALL errore(' conductor ', ' allocating hac_cb ', 1 )

      ALLOCATE ( hci_ca(nmaxc,nmaxa), STAT=ierr )
           IF( ierr /=0 ) CALL errore(' conductor ', ' allocating hci_ca ', 1 )
      ALLOCATE ( hci_bc(nmaxb,nmaxc), STAT=ierr )
           IF( ierr /=0 ) CALL errore(' conductor ', ' allocating hci_bc ', 1 )

      ALLOCATE ( s00_a(nmaxa,nmaxa), STAT=ierr )
           IF( ierr /=0 ) CALL errore(' conductor ', ' allocating s00_a ', 1 )
      ALLOCATE ( s01_a(nmaxa,nmaxa), STAT=ierr )
           IF( ierr /=0 ) CALL errore(' conductor ', ' allocating s01_a ', 1 )
      ALLOCATE ( s00_b(nmaxb,nmaxb), STAT=ierr )
           IF( ierr /=0 ) CALL errore(' conductor ', ' allocating s00_b ', 1 )
      ALLOCATE ( s01_b(nmaxb,nmaxb), STAT=ierr )
           IF( ierr /=0 ) CALL errore(' conductor ', ' allocating s01_b ', 1 )
      ALLOCATE ( s00_c(nmaxc,nmaxc), STAT=ierr )
           IF( ierr /=0 ) CALL errore(' conductor ', ' allocating s00_c ', 1 )
      ALLOCATE ( sci_ac(nmaxa,nmaxc), STAT=ierr )
           IF( ierr /=0 ) CALL errore(' conductor ', ' allocating sci_ac ', 1 )
      ALLOCATE ( sci_cb(nmaxc,nmaxb), STAT=ierr )
           IF( ierr /=0 ) CALL errore(' conductor ', ' allocating sci_cb ', 1 )

      ALLOCATE ( sci_ca(nmaxc,nmaxa), STAT=ierr )
           IF( ierr /=0 ) CALL errore(' conductor ', ' allocating sci_ca ', 1 )
      ALLOCATE ( sci_bc(nmaxb,nmaxc), STAT=ierr )
           IF( ierr /=0 ) CALL errore(' conductor ', ' allocating sci_bc ', 1 )

      ALLOCATE ( c00_a(nmaxa,nmaxa), STAT=ierr )
           IF( ierr /=0 ) CALL errore(' conductor ', ' allocating c00_a ', 1 )
      ALLOCATE ( c01_a(nmaxa,nmaxa), STAT=ierr )
           IF( ierr /=0 ) CALL errore(' conductor ', ' allocating c01_a ', 1 )
      ALLOCATE ( c00_b(nmaxb,nmaxb), STAT=ierr )
           IF( ierr /=0 ) CALL errore(' conductor ', ' allocating c00_b ', 1 )
      ALLOCATE ( c01_b(nmaxb,nmaxb), STAT=ierr )
           IF( ierr /=0 ) CALL errore(' conductor ', ' allocating c01_b ', 1 )
      ALLOCATE ( c00_c(nmaxc,nmaxc), STAT=ierr )
           IF( ierr /=0 ) CALL errore(' conductor ', ' allocating c00_c ', 1 )
      ALLOCATE ( cci_ac(nmaxa,nmaxc), STAT=ierr )
           IF( ierr /=0 ) CALL errore(' conductor ', ' allocating cci_ac ', 1 )
      ALLOCATE ( cci_cb(nmaxc,nmaxb), STAT=ierr )
           IF( ierr /=0 ) CALL errore(' conductor ', ' allocating cci_cb ', 1 )
      ALLOCATE ( cci_ca(nmaxc,nmaxa), STAT=ierr )
           IF( ierr /=0 ) CALL errore(' conductor ', ' allocating cci_ca ', 1 )
      ALLOCATE ( cci_bc(nmaxb,nmaxc), STAT=ierr )
           IF( ierr /=0 ) CALL errore(' conductor ', ' allocating cci_bc ', 1 )

      ALLOCATE ( totA(nmaxa,nmaxa), STAT=ierr )
           IF( ierr /=0 ) CALL errore(' conductor ', ' allocating totA ', 1 )
      ALLOCATE ( totB(nmaxb,nmaxb), STAT=ierr )
           IF( ierr /=0 ) CALL errore(' conductor ', ' allocating totB ', 1 )
      ALLOCATE ( tottA(nmaxa,nmaxa), STAT=ierr )
           IF( ierr /=0 ) CALL errore(' conductor ', ' allocating tottA ', 1 )
      ALLOCATE ( tottB(nmaxb,nmaxb), STAT=ierr )
           IF( ierr /=0 ) CALL errore(' conductor ', ' allocating tottB ', 1 )

      ALLOCATE ( gR(nmaxc,nmaxc), STAT=ierr )
           IF( ierr /=0 ) CALL errore(' conductor ', ' allocating gR ', 1 )
      ALLOCATE ( gL(nmaxc,nmaxc), STAT=ierr )
           IF( ierr /=0 ) CALL errore(' conductor ', ' allocating gL ', 1 )
      ALLOCATE ( gA(nmaxa,nmaxa), STAT=ierr )
           IF( ierr /=0 ) CALL errore(' conductor ', ' allocating gA ', 1 )
      ALLOCATE ( gB(nmaxb,nmaxb), STAT=ierr )
           IF( ierr /=0 ) CALL errore(' conductor ', ' allocating gB ', 1 )

      ALLOCATE ( gAm1(nmaxa,nmaxa), STAT=ierr )
           IF( ierr /=0 ) CALL errore(' conductor ', ' allocating gAml ', 1 )
      ALLOCATE ( gBm1(nmaxb,nmaxb), STAT=ierr )
           IF( ierr /=0 ) CALL errore(' conductor ', ' allocating gBml ', 1 )
      ALLOCATE ( gintr(nmaxc,nmaxc), STAT=ierr )
           IF( ierr /=0 ) CALL errore(' conductor ', ' allocating gintr ', 1 )
      ALLOCATE ( ginta(nmaxc,nmaxc), STAT=ierr )
           IF( ierr /=0 ) CALL errore(' conductor ', ' allocating ginta ', 1 )
      ALLOCATE ( gintm1(nmaxc,nmaxc), STAT=ierr )
           IF( ierr /=0 ) CALL errore(' conductor ', ' allocating gintml ', 1 )
      ALLOCATE ( sLa(nmaxc,nmaxc), STAT=ierr )
           IF( ierr /=0 ) CALL errore(' conductor ', ' allocating sLa ', 1 )
      ALLOCATE ( sRa(nmaxc,nmaxc), STAT=ierr )
           IF( ierr /=0 ) CALL errore(' conductor ', ' allocating sRa ', 1 )
      ALLOCATE ( sLr(nmaxc,nmaxc), STAT=ierr )
           IF( ierr /=0 ) CALL errore(' conductor ', ' allocating sLr ', 1 )
      ALLOCATE ( sRr(nmaxc,nmaxc), STAT=ierr )
           IF( ierr /=0 ) CALL errore(' conductor ', ' allocating sRr ', 1 )
      ALLOCATE ( s1(nmaxc,nmaxc), STAT=ierr )
           IF( ierr /=0 ) CALL errore(' conductor ', ' allocating s1 ', 1 )
      ALLOCATE ( s2(nmaxc,nmaxc), STAT=ierr )
           IF( ierr /=0 ) CALL errore(' conductor ', ' allocating s2 ', 1 )
      ALLOCATE ( c1(nmaxc,nmaxa), STAT=ierr )
           IF( ierr /=0 ) CALL errore(' conductor ', ' allocating c1 ', 1 )
      ALLOCATE ( c2(nmaxc,nmaxb), STAT=ierr )
           IF( ierr /=0 ) CALL errore(' conductor ', ' allocating c2 ', 1 )
      ALLOCATE ( tran(nmaxc,nmaxc), STAT=ierr )
           IF( ierr /=0 ) CALL errore(' conductor ', ' allocating tran ', 1 )

!...  Set up the layer hamiltonians
!     different layers in different files - read only version

      CALL readH( nmaxa, nmaxb, nmaxc, h00_a, h01_a, s00_a, s01_a,   &
                   h00_b, h01_b, s00_b, s01_b, h00_c, s00_c, hci_ac,  &
                   sci_ac, hci_cb, sci_cb , l_overlap )

      DO j = 1, nmaxc
         DO i = 1, nmaxa
            hci_ca(j,i) = hci_ac(i,j)
            sci_ca(j,i) = sci_ac(i,j)
         END DO
      END DO

      DO j = 1, nmaxb
         DO i = 1, nmaxc
            hci_bc(j,i) = hci_cb(i,j)
            sci_bc(j,i) = sci_cb(i,j)
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

         enep = emin +  DBLE(n-1) * de
 
!...     Compute conductance according to Fisher and Lee
!        Compute self-energies following Datta

!...     Retarded

         ene = enep + ( 0.d0, 0.00001d0 )

         DO j = 1, nmaxa
            DO i = 1, nmaxa
               c00_a(i,j) = ( 1.d0, 0.d0 ) * h00_a(i,j) - ene * s00_a(i,j)
               c01_a(i,j) = ( 1.d0, 0.d0 ) * h01_a(i,j) - ene * s01_a(i,j)
            END DO
         END DO
         DO j = 1, nmaxb
            DO i = 1, nmaxb
               c00_b(i,j) = ( 1.d0, 0.d0 ) * h00_b(i,j) - ene * s00_b(i,j)
               c01_b(i,j) = ( 1.d0, 0.d0 ) * h01_b(i,j) - ene * s01_b(i,j)
            END DO
         END DO
         DO j = 1, nmaxc
            DO i = 1, nmaxc
               c00_c(i,j) = ( 1.d0, 0.d0 ) * h00_c(i,j) - ene * s00_c(i,j)
            END DO
         END DO
         DO j = 1, nmaxc
            DO i = 1, nmaxa
               cci_ac(i,j) = ( 1.d0, 0.d0 ) * hci_ac(i,j) - ene * sci_ac(i,j)
            END DO
         END DO
         DO j = 1, nmaxc
            DO i = 1, nmaxb
               cci_bc(i,j) = ( 1.d0, 0.d0 ) * hci_bc(i,j) - ene * sci_bc(i,j)
            END DO
         END DO
         DO j = 1, nmaxa
            DO i = 1, nmaxc
               cci_ca(i,j) = ( 1.d0, 0.d0 ) * hci_ca(i,j) - ene * sci_ca(i,j)
            END DO
         END DO
         DO j = 1, nmaxb
            DO i = 1, nmaxc
               cci_cb(i,j) = ( 1.d0, 0.d0 ) * hci_cb(i,j) - ene * sci_cb(i,j)
            END DO
         END DO

         CALL transferb( nmaxb, nterx, totB, tottB, c00_b, c01_b, ene+bias )
         CALL greenb( nmaxb, totB, tottB, c00_b, c01_b, ene+bias, gBm1, gB, 1, 1 )

         CALL setv0( nmaxc*nmaxb, c2 )
         CALL zgemm( 'N', 'N', nmaxc, nmaxb, nmaxb, alpha, cci_cb, nmaxc, gB, nmaxb, beta, c2, nmaxc )
         CALL setv0( nmaxc*nmaxc, sRr )
         CALL zgemm( 'N', 'N', nmaxc, nmaxc, nmaxb, alpha, c2, nmaxc, cci_bc, nmaxb, beta, sRr, nmaxc )

         CALL transfera( nmaxa, nterx, totA, tottA, c00_a, c01_a, ene )
         CALL greena( nmaxa, totA, tottA, c00_a, c01_a, ene, gAm1, gA, -1, 1 )

         CALL setv0( nmaxc*nmaxa, c1 )
         CALL zgemm( 'N', 'N', nmaxc, nmaxa, nmaxa, alpha, cci_ca, nmaxc, gA, nmaxa, beta, c1, nmaxc )
         CALL setv0( nmaxc*nmaxc, sLr )
         CALL zgemm( 'N', 'N', nmaxc, nmaxc, nmaxa, alpha, c1, nmaxc, cci_ac, nmaxa, beta, sLr, nmaxc )

!...     Advanced

         DO j = 1, nmaxc
            DO i = 1, nmaxc
               sLa(i,j) = CONJG( sLr(j,i) )
               sRa(i,j) = CONJG( sRr(j,i) )
            END DO
         END DO

         DO j = 1, nmaxc
            DO i = 1, nmaxc
               gL(i,j) = ( 0.d0, 1.d0 ) * ( sLr(i,j) - sLa(i,j) )
               gR(i,j) = ( 0.d0, 1.d0 ) * ( sRr(i,j) - sRa(i,j) )
            END DO
         END DO

!...     Construct the conductor green's function

!...     G_C (retarded)

!...     Initialization
         gintm1(:,:) = ( 0.0d0, 0.0d0 )

         DO j = 1, nmaxc
            DO i = 1, nmaxc
               gintm1(i,j) = ( -1.d0, 0.d0 ) * c00_c(i,j) - sLr(i,j) - sRr(i,j)
            END DO
         END DO
 
         DO j = 1, nmaxc
            DO i = 1, nmaxc
               gintr(i,j) = ( 0.d0, 0.d0 )
               IF ( i == j ) gintr(i,j)=( 1.d0, 0.d0 )
            END DO
         END DO

         CALL zgesv( nmaxc, nmaxc, gintm1, nmaxc, ipiv2, gintr, nmaxc, info )
            IF ( info /= 0 )  CALL errore(' Sconductor ', ' zgesv (I) ', info )

!     G_C (advanced)

         DO j = 1, nmaxc
            DO i = 1, nmaxc
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

!...  Deallocate arrays

      DEALLOCATE ( ipiv2, STAT=ierr )
           IF( ierr /=0 ) CALL errore(' conductor ', ' deallocating ipiv2 ', 1 )

      DEALLOCATE ( h00_a, STAT=ierr )
           IF( ierr /=0 ) CALL errore(' conductor ', ' deallocating h00_a ', 1 )
      DEALLOCATE ( h01_a, STAT=ierr )
           IF( ierr /=0 ) CALL errore(' conductor ', ' deallocating h01_a ', 1 )
      DEALLOCATE ( h00_b, STAT=ierr )
           IF( ierr /=0 ) CALL errore(' conductor ', ' deallocating h00_b ', 1 )
      DEALLOCATE ( h01_b, STAT=ierr )
           IF( ierr /=0 ) CALL errore(' conductor ', ' deallocating h01_b ', 1 )
      DEALLOCATE ( h00_c, STAT=ierr )
           IF( ierr /=0 ) CALL errore(' conductor ', ' deallocating h00_c ', 1 )
      DEALLOCATE ( hci_ac, STAT=ierr )
           IF( ierr /=0 ) CALL errore(' conductor ', ' deallocating hci_ac ', 1 )
      DEALLOCATE ( hci_cb, STAT=ierr )
           IF( ierr /=0 ) CALL errore(' conductor ', ' deallocating hac_cb ', 1 )

!     DEALLOCATE ( hci_ca, STAT=ierr )
!          IF( ierr /=0 ) CALL errore(' conductor ', ' deallocating hci_ca ', 1 )
      DEALLOCATE ( hci_bc, STAT=ierr )
           IF( ierr /=0 ) CALL errore(' conductor ', ' deallocating hci_bc ', 1 )


      DEALLOCATE ( s00_a, STAT=ierr )
           IF( ierr /=0 ) CALL errore(' conductor ', ' deallocating s00_a ', 1 )
      DEALLOCATE ( s01_a, STAT=ierr )
           IF( ierr /=0 ) CALL errore(' conductor ', ' deallocating s01_a ', 1 )
      DEALLOCATE ( s00_b, STAT=ierr )
           IF( ierr /=0 ) CALL errore(' conductor ', ' deallocating s00_b ', 1 )
      DEALLOCATE ( s01_b, STAT=ierr )
           IF( ierr /=0 ) CALL errore(' conductor ', ' deallocating s01_b ', 1 )
      DEALLOCATE ( s00_c, STAT=ierr )
           IF( ierr /=0 ) CALL errore(' conductor ', ' deallocating s00_c ', 1 )
      DEALLOCATE ( sci_ac, STAT=ierr )
           IF( ierr /=0 ) CALL errore(' conductor ', ' deallocating sci_ac ', 1 )
      DEALLOCATE ( sci_cb, STAT=ierr )
           IF( ierr /=0 ) CALL errore(' conductor ', ' deallocating sci_cb ', 1 )

      DEALLOCATE ( sci_ca, STAT=ierr )
           IF( ierr /=0 ) CALL errore(' conductor ', ' deallocating sci_ca ', 1 )
!     DEALLOCATE ( sci_bc, STAT=ierr )
!          IF( ierr /=0 ) CALL errore(' conductor ', ' deallocating sci_bc ', 1 )

      DEALLOCATE ( c00_a, STAT=ierr )
           IF( ierr /=0 ) CALL errore(' conductor ', ' deallocating c00_a ', 1 )
      DEALLOCATE ( c01_a, STAT=ierr )
           IF( ierr /=0 ) CALL errore(' conductor ', ' deallocating c01_a ', 1 )
      DEALLOCATE ( c00_b, STAT=ierr )
           IF( ierr /=0 ) CALL errore(' conductor ', ' deallocating c00_b ', 1 )
      DEALLOCATE ( c01_b, STAT=ierr )
           IF( ierr /=0 ) CALL errore(' conductor ', ' deallocating c01_b ', 1 )
      DEALLOCATE ( c00_c, STAT=ierr )
           IF( ierr /=0 ) CALL errore(' conductor ', ' deallocating c00_c ', 1 )
      DEALLOCATE ( cci_ac, STAT=ierr )
           IF( ierr /=0 ) CALL errore(' conductor ', ' deallocating cci_ac ', 1 )
      DEALLOCATE ( cci_cb, STAT=ierr )
           IF( ierr /=0 ) CALL errore(' conductor ', ' deallocating cci_cb ', 1 )
      DEALLOCATE ( cci_ca, STAT=ierr )
           IF( ierr /=0 ) CALL errore(' conductor ', ' deallocating cci_ca ', 1 )
      DEALLOCATE ( cci_bc, STAT=ierr )
           IF( ierr /=0 ) CALL errore(' conductor ', ' deallocating cci_bc ', 1 )

      DEALLOCATE ( totA, STAT=ierr )
           IF( ierr /=0 ) CALL errore(' conductor ', ' deallocating totA ', 1 )
      DEALLOCATE ( totB, STAT=ierr )
           IF( ierr /=0 ) CALL errore(' conductor ', ' deallocating totB ', 1 )
      DEALLOCATE ( tottA, STAT=ierr )
           IF( ierr /=0 ) CALL errore(' conductor ', ' deallocating tottA ', 1 )
      DEALLOCATE ( tottB, STAT=ierr )
           IF( ierr /=0 ) CALL errore(' conductor ', ' deallocating tottB ', 1 )

      DEALLOCATE ( gR, STAT=ierr )
           IF( ierr /=0 ) CALL errore(' conductor ', ' deallocating gR ', 1 )
      DEALLOCATE ( gL, STAT=ierr )
           IF( ierr /=0 ) CALL errore(' conductor ', ' deallocating gL ', 1 )
      DEALLOCATE ( gA, STAT=ierr )
           IF( ierr /=0 ) CALL errore(' conductor ', ' deallocating gA ', 1 )
      DEALLOCATE ( gB, STAT=ierr )
           IF( ierr /=0 ) CALL errore(' conductor ', ' deallocating gB ', 1 )

      DEALLOCATE ( gAm1, STAT=ierr )
           IF( ierr /=0 ) CALL errore(' conductor ', ' deallocating gAml ', 1 )
      DEALLOCATE ( gBm1, STAT=ierr )
           IF( ierr /=0 ) CALL errore(' conductor ', ' deallocating gBml ', 1 )
      DEALLOCATE ( gintr, STAT=ierr )
           IF( ierr /=0 ) CALL errore(' conductor ', ' deallocating gintr ', 1 )
      DEALLOCATE ( ginta, STAT=ierr )
           IF( ierr /=0 ) CALL errore(' conductor ', ' deallocating ginta ', 1 )
      DEALLOCATE ( gintm1, STAT=ierr )
           IF( ierr /=0 ) CALL errore(' conductor ', ' deallocating gintml ', 1 )
      DEALLOCATE ( sLa, STAT=ierr )
           IF( ierr /=0 ) CALL errore(' conductor ', ' deallocating sLa ', 1 )
      DEALLOCATE ( sRa, STAT=ierr )
           IF( ierr /=0 ) CALL errore(' conductor ', ' deallocating sRa ', 1 )
      DEALLOCATE ( sLr, STAT=ierr )
           IF( ierr /=0 ) CALL errore(' conductor ', ' deallocating sLr ', 1 )
      DEALLOCATE ( sRr, STAT=ierr )
           IF( ierr /=0 ) CALL errore(' conductor ', ' deallocating sRr ', 1 )
      DEALLOCATE ( s1, STAT=ierr )
           IF( ierr /=0 ) CALL errore(' conductor ', ' deallocating s1 ', 1 )
      DEALLOCATE ( s2, STAT=ierr )
           IF( ierr /=0 ) CALL errore(' conductor ', ' deallocating s2 ', 1 )
      DEALLOCATE ( c1, STAT=ierr )
           IF( ierr /=0 ) CALL errore(' conductor ', ' deallocating c1 ', 1 )
      DEALLOCATE ( c2, STAT=ierr )
           IF( ierr /=0 ) CALL errore(' conductor ', ' deallocating c2 ', 1 )
      DEALLOCATE ( tran, STAT=ierr )
           IF( ierr /=0 ) CALL errore(' conductor ', ' deallocating tran ', 1 )

      CALL cleanup()

      STOP '*** THE END *** (conductor.x)'
      END PROGRAM 
  
