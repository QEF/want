      PROGRAM bandS
  
! ... Calculates the band structure and  the
!     matrix elements H(R) in a Wigner-Seitz supercell
 
! ... Originally written by IVO SOUZA 9 nov 2000
!     New Version by A. Calzolari & C. Cavazzoni 2003

! ... Input files: pw.dat, energies.dat, unitary, kg_grids.dat
!     Output files: band.gp, band.dat

      USE kinds
      USE parameters, ONLY: mxdtyp => npsx, mxdatm => natx


      IMPLICIT none

      INTEGER :: mxdnrk, mxdbnd
      INTEGER :: nshells
 
      INTEGER :: maxspts, maxpts
      PARAMETER ( maxspts = 8 )
      PARAMETER ( maxpts = 500 )

      REAL(dbl) :: vcell, avec(3,3), bvec(3,3) 
      REAL(dbl) :: aminv(3),adot(3,3),bdot(3,3)
      INTEGER :: nkpts, dimwann
      INTEGER :: ntype
      INTEGER :: natom(mxdtyp)  
      CHARACTER(LEN=2) :: nameat(mxdtyp)
      CHARACTER(LEN=4) :: vdriv
      CHARACTER(LEN=2), ALLOCATABLE :: point(:) ! point(maxspts)
      CHARACTER(LEN=6) :: verbosity = 'none'    ! none, low, medium, high

      REAL(dbl) :: rat(3,mxdatm,mxdtyp)
      REAL(dbl) :: atmass(mxdtyp)
      REAL(dbl) :: e_min, e_max
      REAL(dbl), ALLOCATABLE :: ei(:,:)   !ei(mxdbnd,mxdnrk)
      COMPLEX(dbl) :: expo
      COMPLEX(dbl), ALLOCATABLE :: cu(:,:,:)      ! cu(mxdbnd,mxdbnd,mxdnrk)
      COMPLEX(dbl), ALLOCATABLE :: kham(:,:,:)    ! kham(mxdbnd,mxdbnd,mxdnrk)
      COMPLEX(dbl), ALLOCATABLE :: rham(:,:,:)    ! rham(mxdbnd,mxdbnd,mxdnrk)
      COMPLEX(dbl), ALLOCATABLE :: ham_tmp(:,:)   ! ham_tmp(mxdbnd,mxdbnd)

      INTEGER :: nk(3)
      INTEGER :: nspts, npts, tnkpts, nbands
      INTEGER :: nws
      REAL(dbl) :: s(3)
      REAL(dbl), ALLOCATABLE :: skpt(:,:) , xval(:) ! skpt(3,maxpts), xval(maxspts*maxpts)
      REAL(dbl), ALLOCATABLE :: sxval(:), kpt(:,:)  ! sxval(maxspts), kpt(3,maxspts*maxpts)
      REAL(dbl), ALLOCATABLE :: vkpt(:,:)          ! vkpt(3,mxdnrk)
      REAL(dbl), ALLOCATABLE :: en_band(:,:)       ! en_band(mxdbnd,maxspts*maxpts)
      INTEGER, ALLOCATABLE :: indxws(:,:)       ! indxws(3,3*mxdnrk)
      INTEGER, ALLOCATABLE :: degen(:)          ! degen(3*mxdnrk)
      INTEGER :: i, j, m, n, nkp, irk, nrp, idum
      INTEGER :: i1, i2, i3, j1, j2, j3, jj1, jj2, jj3
      INTEGER :: j1min, j2min, j3min, j1max, j2max, j3max, iws
      REAL(dbl) :: rdum, rmod, vec(3)
      COMPLEX(dbl) :: ctmp
      INTEGER, ALLOCATABLE :: idummy(:)          ! idummy(nshells)
      CHARACTER(LEN=80) :: stringa, stringa2
 
      COMPLEX(dbl), ALLOCATABLE :: ap(:)            ! ap((mxdbnd*(mxdbnd+1))/2)
      COMPLEX(dbl), ALLOCATABLE :: z(:,:)           ! z(mxdbnd,mxdbnd) 
      COMPLEX(dbl), ALLOCATABLE :: work(:)          ! work(2*mxdbnd)
      REAL(dbl), ALLOCATABLE :: w(:)                 ! w(mxdbnd)
      REAL(dbl), ALLOCATABLE :: rwork(:)             ! rwork(7*mxdbnd)
      INTEGER, ALLOCATABLE :: iwork(:)             ! iwork(5*mxdbnd) 
      INTEGER, ALLOCATABLE :: ifail(:)            ! ifail(mxdbnd)
      INTEGER :: info

      REAL(dbl) :: twopi
      REAL(dbl) :: alatt
      REAL(dbl) :: zero, um
      COMPLEX(dbl) :: czero, ci
      PARAMETER ( twopi = 2.0d0 * 3.141592653589793d0)
      PARAMETER ( zero = 0.0d0 )
      PARAMETER ( um = 1.0d0 )
      PARAMETER ( czero = ( 0.0d0, 0.0d0 ) )
      PARAMETER ( ci = ( 0.0d0, 1.0d0 ) )

      INTEGER :: nt
      INTEGER :: nbandi
      REAL(dbl) :: win_min, win_max, froz_min, froz_max
      REAL(dbl) :: emax, sgn

      REAL(dbl) :: dk, dkl
!
! ... Next lines added by ANDREA (28 jan 2004) 
!     PRINT_SGM_START and PRINT_SGM_END are energy indeces 
!     for check self-energy matrix elements
!
      LOGICAL   :: convert_self_energy
      LOGICAL   :: check_self_energy
      LOGICAL   :: calculate_spectral_func
      INTEGER   :: print_sgm_start
      INTEGER   :: print_sgm_end
      INTEGER   :: spin_component
      REAL(dbl) :: Efermi

      NAMELIST /INPUT/ nspts, npts, nbands, convert_self_energy, check_self_energy, & 
                 calculate_spectral_func, print_sgm_start, print_sgm_end, spin_component, efermi     

!
! ... End declarations and dimensions
!

      OPEN( UNIT=19, FILE='takeoff.dat', STATUS='OLD', FORM='UNFORMATTED' )

       READ(19) alatt
       READ(19) ( avec(i,1), i=1,3 )
       READ(19) ( avec(i,2), i=1,3 )
       READ(19) ( avec(i,3), i=1,3 )
       READ(19) ntype
       DO nt = 1, ntype
         READ(19) natom(nt), nameat(nt)
         DO j = 1, natom(nt)
           READ(19) ( rat( i, j, nt ), i=1,3 )
         END DO
       END DO
       READ(19) emax, nbandi
       READ(19) ( nk(i), i=1,3 ), ( s(i), i=1,3 )
       READ(19) win_min, win_max, froz_min, froz_max, dimwann

       CLOSE(19)

       mxdnrk = nk(1) * nk(2) * nk(3)
       mxdbnd = nbandi

       DO j=1,3
         DO i=1,3
           sgn = 1.0d0
           IF ( avec(i,j) < zero ) sgn = -sgn
           avec(i,j) = ABS(avec(i,j))
           avec(i,j) = sgn * alatt * avec(i,j)
         END DO
       END DO
       WRITE(6,201)
 201   FORMAT(/, '  PRIMITIVE TRANSLATION VECTORS',/,25x,'IN A.U.', 22x,'IN LATTICE UNITS' )

       DO j=1,3
         WRITE(6,213) j,( avec(i,j), i=1,3 ), ( avec(i,j)/alatt, i=1,3 )
       END DO
 213   FORMAT( '  A',i1,'=',3(2x,E11.5), 5x, 3(2x,F7.3) )

       WRITE(6,120) nshells
 120   FORMAT( 'NUMBER OF SHELLS: NSHELLS=',i2,/ )

!
! ...  Get crystal data

       CALL latti( avec, bvec, vcell, bdot, aminv, adot )

      IF ( ( s(1) /= zero ) .OR. ( s(2) /= zero ) .OR. ( s(3) /= zero ) ) THEN
        WRITE(6,*) 'S(I) IS NOT ZERO ==> H(R) NOT PERIODIC'
        STOP
      END IF

      WRITE (6,121) dimwann
 121  FORMAT(/,'DIMENSION OF SUBSPACE: DIMWANN=',I2,/)
 


! ...  Read (from stdin) information for plotting band structure
!      and for eventually converting a self energy to wannier basis             
!
!     Next lines (namelist reading) added by ANDREA (28 jan 2004)
      
      convert_self_energy         = .FALSE.
      check_self_energy           = .FALSE.
      calculate_spectral_func     = .FALSE.
      print_sgm_start             = 0
      print_sgm_end               = 0
      spin_component              = 1
      efermi                      = 0.0
      
      READ(5, INPUT, IOSTAT=i)
      IF ( i /= 0 )  CALL errore('bands','Unable to read namelist INPUT',ABS(i))


! ... Usually nbands equals dimwann 
      IF ( nbands > dimwann ) STOP 'DECREASE NBANDS IN PW.DAT'
      WRITE(6,*) nspts, ' SPECIAL K-POINTS:'
      IF ( nspts > maxspts ) STOP 'INCREASE MAXSPTS'
      IF ( nbands > 100 ) STOP 'CHANGE FORMAT IN LINE 650'

      ALLOCATE( point( nspts ) )
      ALLOCATE( skpt( 3, nspts ) )

      DO j = 1, nspts
        READ (5,202) point(j)
 202    FORMAT (A2)
       READ (5,*) ( skpt(i,j), i=1,3 )
        WRITE (6,203) point(j), ( skpt(i,j), i=1,3 )
 203    FORMAT(a1,2x,3(f10.8,1x))
      END DO

      WRITE (6,*) npts, ' K-POINTS IN EACH SEGMENT'
      WRITE (6,*) nbands, ' BANDS'
 
      CLOSE (5)
 
! ... Calculate grid of k-points

      ALLOCATE( vkpt( 3, mxdnrk) )
 
      nkp = 0
      DO i1 = 0, nk(1)-1
        DO i2 = 0, nk(2)-1
          DO i3 = 0, nk(3)-1
            nkp=nkp+1
            vkpt(1,nkp)= DBLE(i1) / DBLE( nk(1) ) + s(1)
            vkpt(2,nkp)= DBLE(i2) / DBLE( nk(2) ) + s(2)
            vkpt(3,nkp)= DBLE(i3) / DBLE( nk(3) ) + s(3)
          END DO
        END DO
      END DO
      nkpts = nkp
  
! ... Read energy eigenvalues in electron-volt

      IF ( ( dimwann > 200 ) .or. ( nkpts > 99999 ) ) THEN
        WRITE (*,*) '*** error ***'
        WRITE (*,*) 'modify format in line number 102'
        STOP '*** error ***'
      ENDIF

      OPEN ( 7, FILE='energies.dat', STATUS='OLD', FORM='FORMATTED' )

      ALLOCATE( ei( dimwann, mxdnrk ) )

      DO nkp = 1, nkpts
        READ( 7, * ) idum, ( ei( i, nkp ), i = 1, dimwann )
      END DO

      CLOSE (7)

!
! ... Energy zero settings, added by ANDREA (28 jan 2004)
!     For coherence with self-energy translate Fermi energy
!     to zero (i.e. Efermi is the energy reference)
!
      ei(:,:) = ei(:,:) - Efermi


! ... Read unitary matrices U(k) that rotate the bloch states
 
      OPEN (29, FILE='unitary.dat', STATUS='OLD', FORM='UNFORMATTED' )

      ALLOCATE( cu( dimwann, dimwann, nkpts ) )
      READ (29) ( ( ( CU(J,I,NKP), J=1,DIMWANN ), I=1,DIMWANN ), NKP=1,NKPTS )

      CLOSE(29)

 
! ... Set unitary matrices to identity
!     This corresponds to using the non-rotated WFs for the interpolation. Should
!     give worse results (more wiggly bands) except in the single band case, where
!     it should give exactly the same results, since the band is then 
!     gauge-invariant.
!
!     DO nkp = 1, nkpts
!       DO j = 1, dimwann
!         DO i = 1, dimwann
!           IF ( i ==  j ) THEN
!             cu(j,i,nkp) = CMPLX( 1.0d0, 0.0d0 )
!           ELSE
!             cu(j,i,nkp) = czero
!           END IF
!          END DO
!       END DO
!      END DO
 
! ... Check unitarity
      IF ( verbosity == 'high' ) THEN
        WRITE(6,*) ' '
        WRITE(6,*) 'checking unitarity...'
        DO nkp = 1, nkpts
          DO i = 1, dimwann
            DO j = 1, dimwann
              ctmp = czero
              DO m = 1, dimwann
                ctmp = ctmp + cu(i,m,nkp) * CONJG( cu(j,m,nkp) )
              END DO
              WRITE (6,'(2i4,2f15.10)') i, j, ctmp
            END DO
          END DO
          WRITE (6,*)
        END DO
      END IF

 
! ... Calculate H(k)=U^{dagger}(k).H_0(k).U(k)
!     (hamiltonian matrix elements between the rotated bloch states)

      ALLOCATE( kham( dimwann, dimwann, nkpts ) )
 
      DO nkp = 1, nkpts
        DO j = 1, dimwann
          DO i = 1, j
            kham(i,j,nkp) = czero
            DO m = 1, dimwann
              kham(i,j,nkp) = kham(i,j,nkp) + ei(m,nkp) * CONJG( cu(m,i,nkp) ) * cu(m,j,nkp)                  
            END DO
! ...       use hermiticity
            kham(j,i,nkp) = CONJG( kham(i,j,nkp) )
          END DO
        END DO
      END DO


! ... Check that eigenvalues of H(k) are the same as those of H_0(k)
      IF ( verbosity == 'high' ) THEN
        WRITE (6,*)
        WRITE (6,*) 'CHECKING EIGENVALUES...'
        ALLOCATE( ap( dimwann * ( dimwann + 1 ) / 2 ) )
        ALLOCATE( w( dimwann ), ifail( dimwann ) )
        ALLOCATE( z( dimwann, dimwann ), work( 2 * dimwann ) )
        ALLOCATE( rwork( 7 * dimwann ), iwork( 5 * dimwann ) )
        DO nkp = 1, nkpts
          DO j = 1, dimwann
            DO i = 1 ,j
              ap( i + ( (j-1)*j ) / 2 ) = kham(i,j,nkp)
            END DO
          END DO
          CALL zhpevx( 'n', 'a', 'u', dimwann, ap(1), zero, zero, 0, 0, -um,            &
               m, w(1), z(1,1), dimwann, work(1), rwork(1), iwork(1), ifail(1), info )

          IF( info < 0 ) THEN
            WRITE(6,*) '*** ERROR *** zhpevx while diagonalizing hamiltonian'
            WRITE(6,*) 'the ', -info, ' argument of zhpevx had an illegal value'
            STOP
          END IF

          IF( info > 0 ) THEN
            WRITE(6,*) '*** ERROR *** zhpevx while diagonalizing hamiltonian'
            WRITE(6,*) info,'eigenvectors failed to converge'
            STOP
          END IF

          WRITE (6,743) nkp, ( w(m), m=1,dimwann ), ( ei(m,nkp), m=1,dimwann )
 743      FORMAT(i4,2x,10(f10.5,1x))
        END DO
        DEALLOCATE( ap, w, z, work, rwork, iwork, ifail )
      END IF

 
! ... Fourier transform it: H_ij(k) --> H_ij(R) = (1/N_kpts) sum_k e^{-ikR} H_ij(k)

! ... Find real-space grid points R in Wigner-Seitz supercell

      ALLOCATE( indxws( 3, 3*nkpts ) )
      ALLOCATE( degen( 3*nkpts ) )

      CALL wigner_seitz( adot, nk, indxws, nws, degen, nkpts  )        

      ALLOCATE( rham( dimwann, dimwann, nws ) )

      DO iws = 1, nws
        DO j = 1, dimwann
          DO i = 1, dimwann
            rham(i,j,iws) = czero
            DO nkp = 1, nkpts
              expo = EXP( -ci * twopi * (vkpt(1,nkp) * DBLE( indxws(1,iws) ) +   &
              vkpt(2,nkp) * DBLE( indxws(2,iws) ) +                              &
              vkpt(3,nkp) * DBLE( indxws(3,iws) ) ) )
              rham(i,j,iws)=rham(i,j,iws)+expo*kham(i,j,nkp)
            END DO
            rham(i,j,iws) = rham(i,j,iws) / DBLE(nkpts)
          END DO
        END DO
      END DO
 

!
!     self energy conversion ( added by ANDREA 28 jan 2004)
!
! ... If CONVERT_SELF_ENERGY=.TRUE. reads the self-energy and changes
!     its basis from BLOCH to WANNIER states; then writes the new
!     matrix to file to be used later by the transport code
!
      IF (convert_self_energy)  THEN
           CALL do_self_energy(dimwann,nkpts,nws,spin_component,cu,vkpt,indxws,bvec,     &
                               'sigma.blc','sigma.wan')
      END IF

!
! ... checking, if CHECK_SELF_ENERGY=.TRUE.  (added by ANDREA 28 jan 2004)
!
      IF (check_self_energy)  THEN
           CALL check_sgm_wan(dimwann,nws,nk,spin_component,rham,           &
                              print_sgm_start,print_sgm_end,calculate_spectral_func)
      END IF



! ... Arrigo
!     standard output for conduction calculations

      WRITE (stringa,"(a,i2,a)") "(",dimwann,"f7.3)"
      WRITE (6,"(a)") stringa

      OPEN( 82, file='matrix.dat', status='unknown', form='formatted' )
      OPEN( 83, file='diagonal.dat', status='unknown', form='formatted' )

      DO iws = 1, nws
        IF ( ( ( indxws(1,iws) .eq.  0 ) .and. ( indxws(2,iws) .eq. 0 ) .and. ( indxws(3,iws) .eq. 0 ) ) .or.  &
             ( ( indxws(1,iws) .eq.  1 ) .and. ( indxws(2,iws) .eq. 0 ) .and. ( indxws(3,iws) .eq. 0 ) ) .or.  &
             ( ( indxws(1,iws) .eq. -1 ) .and. ( indxws(2,iws) .eq. 0 ) .and. ( indxws(3,iws) .eq. 0 ) ) .or.  &
             ( ( indxws(1,iws) .eq.  0 ) .and. ( indxws(2,iws) .eq. 1 ) .and. ( indxws(3,iws) .eq. 0 ) ) .or.  &
             ( ( indxws(1,iws) .eq. 0 ) .and. ( indxws(2,iws) .eq. 0 ) .and. ( indxws(3,iws) .eq. 1 ) ) ) THEN
          WRITE (100+iws,*) dimwann, ( indxws(i,iws), i=1,3 )
          WRITE (82,*)' '
          WRITE (82,*) dimwann, ( indxws(i,iws), i=1,3 )
          WRITE (82,*)' '
          WRITE (83,*)' '
          WRITE (83,*) dimwann, ( indxws(i,iws), i=1,3 )
          WRITE (83,*)' '
          DO i = 1, dimwann
            WRITE (100+iws,*)
            DO j = 1, dimwann
              WRITE(100+iws,*) REAL( rham(i,j,iws) )
            END DO
            WRITE(82,stringa)( REAL( rham(i,j,iws) ), j=1,dimwann )
            WRITE(83,*)REAL( rham(i,i,iws) )
          END DO
        END IF
      END DO

      CLOSE(82)
      CLOSE(83)


! ... Check that magnitude of matrix elements |H_ij(R)| decreases with |R|.
!     Should expect it to decrease *faster* using the rotated Bloch functions
!     (again, except in the single-band case, where it should be exactly the same)


      OPEN( 2, FILE = 'decay.dat', STATUS='UNKNOWN', FORM='FORMATTED')
      DO iws = 1, nws
        vec(1) = dble( indxws(1,iws) ) * avec(1,1) +     &
                 dble( indxws(2,iws) ) * avec(1,2) +     &
                 dble( indxws(3,iws) ) * avec(1,3)
        vec(2) = dble( indxws(1,iws) ) * avec(2,1) +     &
                 dble( indxws(2,iws) ) * avec(2,2) +     &
                 dble( indxws(3,iws) ) * avec(2,3)
        vec(3) = dble( indxws(1,iws) ) * avec(3,1) +     &
                 dble( indxws(2,iws) ) * avec(3,2) +     &
                 dble( indxws(3,iws) ) * avec(3,3)
        rmod = SQRT( vec(1)**2 + vec(2)**2 + vec(3)**2 )
        WRITE(2,*) rmod, ABS( rham(1,1,iws) )
      END DO
      CLOSE(2)

 
! ... Determine the k-points used in the band structure plot

      ALLOCATE( xval( nspts * npts ) )
      ALLOCATE( sxval( nspts ) )
      ALLOCATE( kpt(3,nspts*npts) )
 
      CALL get_points( nspts, npts, nspts, npts, bdot, skpt, kpt, xval, sxval, tnkpts )
 
! ... Estimate H_ij(k') at those k-points by fourier interpolation
!     H_ij(k') ~ sum_R e^{ik'R} H_ij(R)/degen(R), where the sum over R is over a 
!     finite grid (truncation)
 
      ALLOCATE( ham_tmp( dimwann, dimwann ) )
 
      e_min = 1.e8         ! some large number
      e_max = -1.e8

      ALLOCATE( en_band( nbands, tnkpts ) )
 
      DO irk = 1, tnkpts

        DO j = 1, dimwann
          DO i = 1, dimwann
            ham_tmp(i,j) = czero
            DO iws = 1, nws
              expo = EXP( ci * twopi * ( kpt(1,irk) * DBLE( indxws(1,iws) ) +  &     
              kpt(2,irk) * DBLE( indxws(2,iws) ) +                             &
              kpt(3,irk) * DBLE( indxws(3,iws) ) ) ) 
              ham_tmp(i,j) = ham_tmp(i,j) + expo * rham(i,j,iws) / degen(iws)
            ENDDO
          ENDDO
        ENDDO
 
! ...   Check that it's hermitian
!       DO j= 1, dimwann
!         DO i = 1, j
!           IF ( ABS(ham_tmp(i,j) - CONJG(ham_tmp(j,i) ) ) > 1.0e-8 ) THEN
!             WRITE(6,*) '*** ERROR *** not hermitian!'
!             WRITE(6,*) irk, i, j, ham_tmp(i,j) - CONJG( ham_tmp(j,i) )
!             STOP
!           END IF
!         END DO
!       END DO

! ...   Diagonalize the hamiltonian at the present k-point

        ALLOCATE( ap( dimwann * ( dimwann + 1 ) / 2 ) )
        ALLOCATE( w( dimwann ), ifail( dimwann ) )
        ALLOCATE( z( dimwann, dimwann ), work( 2 * dimwann ) )
        ALLOCATE( rwork( 7 * dimwann ), iwork( 5 * dimwann ) )
 
        DO j = 1, dimwann
          DO i = 1, j
            ap( i + ( (j-1)*j ) / 2 ) = ham_tmp(i,j)
          END DO
        END DO
        CALL zhpevx( 'n', 'i', 'u', dimwann, ap(1), zero, zero, 1, nbands, -um,     &
             m, w(1), z(1,1), mxdbnd, work(1), rwork(1), iwork(1), ifail(1), info )
        IF ( info < 0 ) THEN
          WRITE(6,*) '*** ERROR *** zhpevx while diagonalizing hamiltonian'
          WRITE(6,*) 'THE ', -info, ' ARGUMENT OF ZHPEVX HAD AN ILLEGAL VALUE'
          STOP
        END IF
        IF( info > 0 ) THEN
          WRITE(6,*) '*** ERROR *** zhpevx while diagonalizing hamiltonian'
          WRITE(6,*) info, 'EIGENVECTORS FAILED TO CONVERGE'
          STOP
        END IF
        DO i = 1, nbands
          en_band(i,irk) = w(i)
          IF( w(i) < e_min ) e_min = w(i)
          IF( w(i) > e_max ) e_max = w(i)
        END DO

        DEALLOCATE( ap, w, z, work, rwork, iwork, ifail )

      END DO ! IRK

 
      OPEN( 27, FILE='band.dat', STATUS='UNKNOWN', FORM='FORMATTED' )

      DO i = 1, nbands
        DO irk = 1, tnkpts
          WRITE (27,650) xval(irk), en_band(i,irk)
        END DO
      END DO
 650  FORMAT(2e16.8)

      CLOSE( 27 )
 

! ... Make a gnuplot data file
 
      OPEN( 28, FILE='band.gp', STATUS='UNKNOWN', FORM='FORMATTED')

      WRITE (28,701) xval(tnkpts),e_min,e_max
      DO i = 2, nspts
        WRITE(28,705) sxval(i), e_min,sxval(i), e_maX
      END DO
!      WRITE (28,702) ( point(i), sxval(i), i=1,nspts-1 )
!      WRITE (28,703) point(nspts), sxval(nspts)
!      WRITE (28,702) ( point(i), sxval(i), i=1,nspts )
      WRITE (stringa2,706) nspts-1
      WRITE (*,*) stringa2
      WRITE (28,stringa2) ( point(i), sxval(i), i=1,nspts )
      WRITE (28,704)

      CLOSE( 28 )

 701  FORMAT('set data style dots',/,'set nokey',/,'set xrange [0:',F8.5,']',/,'set yrange [',F9.5,' :',F9.5,']')
! 702  FORMAT('set xtics (',:20('"',A2,'" ',F8.5,','))
 702  FORMAT('set xtics (',:20('"',A2,'" ',F8.5,','),')')
 703  FORMAT( A2,'" ',F8.5,')' )
 704  FORMAT('plot "band.dat"',/,'pause -1',/,'set output "band.ps"',/, 'set terminal postscript color',/,'replot')
 705  FORMAT('set arrow from ',F8.5,',',F10.5,' to ',F8.5,',',F10.5, ' nohead')
 706  FORMAT('(''set xtics ('',', I4,'( ''"'',A2,''"'',F8.5,'',''), ''"'',A2,''"'',F8.5,'')'')')

      STOP '*** THE END *** (bands.f90)'
      END
