! 
! Copyright (C) 2004 Arrigo Calzolari, Carlo Cavazzoni, Marco Buongiorno Nardelli
! Copyright (C) 2002 Nicola Marzari, Ivo Souza, David Vanderbilt
! Copyright (C) 1997 Nicola Marzari, David Vanderbilt
!
! This file is distributed under the terms of the
! GNU General Public License. See the file `License'
! in the root directory of the present distribution,
! or http://www.gnu.org/copyleft/gpl.txt .
!
!=----------------------------------------------------------------------------------=
      PROGRAM hamiltonian
!=----------------------------------------------------------------------------------=
  
! ... Calculates the band structure and  the
!     matrix elements H(R) in a Wigner-Seitz supercell
 
! ... Input files: DFT_DATA, subspace data, wannier data
!     Output files: band.gp, band.dat, matrix.dat, diagonal.dat

      USE kinds
      USE constants, ONLY: PI, TPI, ZERO, CZERO, CI, ONE
      USE input_module, ONLY : verbosity
      USE parameters, ONLY : nstrx, nsptsx
      USE io_module, ONLY : stdout, ioname, dft_unit, space_unit, wan_unit
      USE io_module, ONLY : work_dir, prefix, postfix
      USE files_module, ONLY : file_open, file_close
      USE timing_module, ONLY : timing, timing_deallocate, timing_overview, global_list
      USE startup_module, ONLY : startup
      USE cleanup_module, ONLY : cleanup
      USE version_module, ONLY : version_number
      USE util_module, ONLY : zmat_unitary

      USE lattice
      USE kpoints_module,       ONLY : nkpts, nk, s, vkpt, &
                                       kpoints_init, kpoints_deallocate
      USE windows_module,       ONLY : windows_read, windows_deallocate
      USE subspace_module,      ONLY : wan_eig, subspace_read, subspace_deallocate
      USE localization_module,  ONLY : dimwann, cu, & 
                                       localization_read, localization_deallocate 


      IMPLICIT NONE 

      COMPLEX(dbl) :: expo
      COMPLEX(dbl), ALLOCATABLE :: kham(:,:,:)    ! kham(dimwann,dimwann,nkpts)
      COMPLEX(dbl), ALLOCATABLE :: rham(:,:,:)    ! rham(dimwann,dimwann,nkpts)
      COMPLEX(dbl), ALLOCATABLE :: ham_tmp(:,:)   ! ham_tmp(dimwann,dimwann)

      INTEGER :: ntype
      INTEGER :: nspts, npts, tnkpts
      INTEGER :: nws
      REAL(dbl), ALLOCATABLE :: skpt(:,:) , xval(:) 
      REAL(dbl), ALLOCATABLE :: sxval(:), kpt(:,:)  
      REAL(dbl), ALLOCATABLE :: en_band(:,:)      
      INTEGER, ALLOCATABLE :: indxws(:,:)       
      INTEGER, ALLOCATABLE :: degen(:)          
      INTEGER :: i, j, m, n, nkp, irk, idum
      INTEGER :: i1, i2, i3
      INTEGER :: iws
      REAL(dbl) :: rmod, rdum, vec(3)
      CHARACTER(LEN=80)             :: stringa
      CHARACTER(LEN=2), ALLOCATABLE :: point(:)    
      CHARACTER(LEN=nstrx)          :: filename
 
      COMPLEX(dbl), ALLOCATABLE :: ap(:)            ! ap((dimwann*(dimwann+1))/2)
      COMPLEX(dbl), ALLOCATABLE :: z(:,:)           ! z(dimwann,dimwann) 
      COMPLEX(dbl), ALLOCATABLE :: work(:)          ! work(2*dimwann)
      REAL(dbl), ALLOCATABLE :: w(:)                 ! w(dimwann)
      REAL(dbl), ALLOCATABLE :: rwork(:)             ! rwork(7*dimwann)
      INTEGER, ALLOCATABLE :: iwork(:)             ! iwork(5*dimwann) 
      INTEGER, ALLOCATABLE :: ifail(:)            ! ifail(dimwann)
      INTEGER :: info

      INTEGER :: nt
      INTEGER   :: ierr
      LOGICAL   :: lfound
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

      NAMELIST /INPUT/ prefix, postfix, work_dir, verbosity, &
                       nspts, npts, convert_self_energy, check_self_energy, & 
                       calculate_spectral_func, print_sgm_start, print_sgm_end,  &
                       spin_component, efermi     

!
! ... End declarations and dimensions
!
!=-------------------------------------------------------------------------------=

!
! ...  Startup
!
      CALL startup(version_number,MAIN_NAME='hamiltonian')
      WRITE(stdout,*)


!...  Read (from stdin) information for plotting band structure
!     and for eventually converting a self energy to wannier basis             
!
      prefix                      = 'WanT' 
      postfix                     = ' ' 
      work_dir                    = './' 
      verbosity                   = 'medium' 
      nspts                       = 0
      npts                        = 100
      convert_self_energy         = .FALSE.
      check_self_energy           = .FALSE.
      calculate_spectral_func     = .FALSE.
      print_sgm_start             = 0
      print_sgm_end               = 0
      spin_component              = 1
      Efermi                      = 0.0d0
      
      READ(5, INPUT, IOSTAT=i)
      IF ( i /= 0 )  CALL errore('hamiltonian','Unable to read namelist INPUT',ABS(i))


! ... Some checks (but many more should be included)
      IF ( nspts > nsptsx ) CALL errore('hamiltonian', 'nspts too large',  nspts)
      IF ( nspts <= 0 ) CALL errore('hamiltonian', 'Invalid nspts', ABS(nspts)+1)
 
      ALLOCATE( point( nspts ), STAT=ierr )
          IF( ierr /=0 ) CALL errore(' hamiltonian ', ' allocating point ', nspts )
      ALLOCATE( skpt( 3, nspts ), STAT=ierr )
          IF( ierr /=0 ) CALL errore(' hamiltonian ', ' allocating skpt ', 3*nspts )

      DO j = 1, nspts
        READ (5, fmt="(a2)") point(j)
        READ (5,*) ( skpt(i,j), i=1,3 )
      END DO

!
! ... Read from file
!
      CALL ioname('dft_data',filename)
      OPEN( UNIT=dft_unit, FILE=TRIM(filename), STATUS='OLD', FORM='UNFORMATTED' )

      READ(dft_unit) alat
      READ(dft_unit) ( avec(i,1), i=1,3 )
      READ(dft_unit) ( avec(i,2), i=1,3 )
      READ(dft_unit) ( avec(i,3), i=1,3 )
      READ(dft_unit) ntype
      DO nt = 1, ntype
          READ(dft_unit) idum
          DO j = 1, idum
             READ(dft_unit) rdum
          ENDDO
      ENDDO
      READ(dft_unit) rdum
      READ(dft_unit) ( nk(i), i=1,3 ), ( s(i), i=1,3 )

      CLOSE(dft_unit)


! ... Get crystal data
      CALL lattice_init()
 
! ... Get K-point mesh and data
      nkpts = PRODUCT(nk(:))
      CALL kpoints_init( nkpts, BSHELL=.FALSE. )

! ... Read energy eigenvalues in electron-volt
      CALL ioname('subspace',filename)
      CALL file_open(space_unit,TRIM(filename),PATH="/",ACTION="read",FORM="formatted")
          CALL windows_read(space_unit,"WINDOWS",lfound)
          IF ( .NOT. lfound ) CALL errore('hamiltonian',"unable to find WINDOWS",1)
          CALL subspace_read(space_unit,"SUBSPACE",lfound)
          IF ( .NOT. lfound ) CALL errore('hamiltonian',"unable to find SUBSPACE",1)
      CALL file_close(space_unit,PATH="/",ACTION="read")

      CALL ioname('subspace',filename,LPATH=.FALSE.)
      WRITE( stdout,"(/,'  Subspace data read from file: ',a)") TRIM(filename)
      !
      ! ... Energy zero settings, added by ANDREA (28 jan 2004)
      !     For coherence with self-energy translate Fermi energy
      !     to zero (i.e. Efermi is the energy reference)
      !
      wan_eig(:,:) = wan_eig(:,:) - Efermi

!
! ... Read unitary matrices U(k) that rotate the bloch states
      CALL ioname('wannier',filename)
      CALL file_open(wan_unit,TRIM(filename),PATH="/",ACTION="read",FORM="formatted")
          CALL localization_read(wan_unit,"WANNIER_LOCALIZATION",lfound)
          IF ( .NOT. lfound ) &
             CALL errore('hamiltonian',"unable to find WANNIER_LOCALIZATION",1)
      CALL file_close(wan_unit,PATH="/",ACTION="read")

      CALL ioname('wannier',filename,LPATH=.FALSE.)
      WRITE( stdout,"(/,'  Wannier data read from file: ',a)") TRIM(filename)
 
      DO nkp = 1,nkpts
         IF ( .NOT. zmat_unitary( cu(:,:,nkp), SIDE='both', TOLL=1.0d-8)  ) &
             CALL errore('hamiltonian',"U matrices not orthogonal",nkp)
      ENDDO

!
! ... ENd of input reading      


!
! ... Calculate H(k)=U^{dagger}(k).H_0(k).U(k)
!     (hamiltonian matrix elements between the rotated bloch states)

      ALLOCATE( kham( dimwann, dimwann, nkpts ), STAT=ierr )
          IF( ierr /=0 ) CALL errore(' hamiltonian ', ' allocating khan ',dimwann**2 *nkpts)
 
      DO nkp = 1, nkpts
         DO j = 1, dimwann
         DO i = 1, j
            kham(i,j,nkp) = CZERO
            DO m = 1, dimwann
              kham(i,j,nkp) = kham(i,j,nkp) + wan_eig(m,nkp) * &
                                              CONJG( cu(m,i,nkp) ) * cu(m,j,nkp)
            ENDDO
! ...       use hermiticity
            kham(j,i,nkp) = CONJG( kham(i,j,nkp) )
         ENDDO
         ENDDO
      ENDDO


! ... Check that eigenvalues of H(k) are the same as those of H_0(k)
      IF ( verbosity == 'high' ) THEN

        ALLOCATE( ap( dimwann * ( dimwann + 1 ) / 2 ), STAT=ierr )
            IF( ierr /=0 ) &
            CALL errore(' hamiltonian ', ' allocating ap',dimwann*(dimwann+1)/2 )
        ALLOCATE( w( dimwann ), ifail( dimwann ), STAT=ierr )
            IF( ierr /=0 ) CALL errore(' hamiltonian ', ' allocating w ifail ', 2*dimwann )
        ALLOCATE( z( dimwann, dimwann ), work( 2 * dimwann ), STAT=ierr )
            IF( ierr /=0 ) &
            CALL errore(' hamiltonian ', ' allocating z work ', 2*dimwann + dimwann**2 )
        ALLOCATE( rwork( 7 * dimwann ), iwork( 5 * dimwann ), STAT=ierr )
            IF( ierr /=0 ) CALL errore(' hamiltonian ', 'allocating rwork iwork',12*dimwann)

        DO nkp = 1, nkpts
          DO j = 1, dimwann
            DO i = 1 ,j
              ap( i + ( (j-1)*j ) / 2 ) = kham(i,j,nkp)
            END DO
          END DO
          CALL zhpevx( 'n', 'a', 'u', dimwann, ap(1), ZERO, ZERO, 0, 0, -ONE,            &
               m, w(1), z(1,1), dimwann, work(1), rwork(1), iwork(1), ifail(1), info )

          IF( info < 0 ) CALL errore('hamiltonian','zhpevx had an illegal value (I)', info)

          IF( info > 0 ) CALL errore('hamiltonian','zhpevx diagonalization failed (I)', info)

        END DO
        DEALLOCATE( ap, w, z, work, rwork, iwork, ifail, STAT=ierr )
            IF( ierr /=0 ) CALL errore(' hamiltonian ', 'deallocating ap...ifail', ABS(ierr))
      END IF

 
! ... Fourier transform it: H_ij(k) --> H_ij(R) = (1/N_kpts) sum_k e^{-ikR} H_ij(k)
! ... Find real-space grid points R in Wigner-Seitz supercell

      ALLOCATE( indxws( 3, 3*nkpts ), STAT=ierr )
          IF( ierr /=0 ) CALL errore(' hamiltonian ', ' allocating indxws', 9*nkpts )
      ALLOCATE( degen( 3*nkpts ), STAT=ierr )
          IF( ierr /=0 ) CALL errore(' hamiltonian ', ' allocating degen', 3*nkpts )

      CALL wigner_seitz( avec, nk, indxws, nws, degen, nkpts  )        

      ALLOCATE( rham( dimwann, dimwann, nws ), STAT=ierr )
          IF( ierr /=0 ) CALL errore(' hamiltonian ', ' allocating rham', dimwann**2 *nws )

      DO iws = 1, nws
         DO j = 1, dimwann
         DO i = 1, dimwann
            rham(i,j,iws) = CZERO
            DO nkp = 1, nkpts
              expo = EXP( -CI * TPI * (vkpt(1,nkp) * DBLE( indxws(1,iws) ) +   &
              vkpt(2,nkp) * DBLE( indxws(2,iws) ) +                              &
              vkpt(3,nkp) * DBLE( indxws(3,iws) ) ) )
              rham(i,j,iws)=rham(i,j,iws)+expo*kham(i,j,nkp)
            ENDDO
            rham(i,j,iws) = rham(i,j,iws) / DBLE(nkpts)
         ENDDO
         ENDDO
      ENDDO
 

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

      OPEN( 82, file='matrix.dat', status='unknown', form='formatted' )
      OPEN( 83, file='diagonal.dat', status='unknown', form='formatted' )

      DO iws = 1, nws
        IF ( ( (indxws(1,iws) ==  0) .AND. &
               (indxws(2,iws) ==  0) .AND. &
               (indxws(3,iws) ==  0) )         .OR. &
             ( (indxws(1,iws) ==  1) .AND. &
               (indxws(2,iws) ==  0) .AND. &
               (indxws(3,iws) ==  0) )         .OR. &
             ( (indxws(1,iws) == -1) .AND. &
               (indxws(2,iws) ==  0) .AND. &  
               (indxws(3,iws) ==  0) )         .OR. &
             ( (indxws(1,iws) ==  0) .AND. &
               (indxws(2,iws) ==  1) .AND. &
               (indxws(3,iws) ==  0) )         .OR. &
             ( (indxws(1,iws) ==  0) .AND. &
               (indxws(2,iws) == -1) .AND. &
               (indxws(3,iws) ==  0) )         .OR. &
             ( (indxws(1,iws) ==  0) .AND. &
               (indxws(2,iws) ==  0) .AND. &
               (indxws(3,iws) == -1) )         .OR. &
             ( (indxws(1,iws) ==  0) .AND. &
               (indxws(2,iws) ==  0) .AND. & 
               (indxws(3,iws) ==  1) )              ) THEN

          WRITE (100+iws,*) dimwann, dimwann, ( indxws(i,iws), i=1,3 )
          WRITE (82,*)' '
          WRITE (82,*) dimwann, ( indxws(i,iws), i=1,3 )
          WRITE (82,*)' '
          WRITE (83,*)' '
          WRITE (83,*) dimwann, ( indxws(i,iws), i=1,3 )
          WRITE (83,*)' '

          DO j = 1, dimwann
            WRITE (100+iws,*)' '
            DO i = 1, dimwann
              WRITE( 100+iws, * ) REAL( rham(i,j,iws) )
            END DO
          END DO
          DO i = 1, dimwann
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

      WRITE(stdout,"(/,2x,'Decay of the real space Hamiltonian:')") 
      WRITE(stdout,"(  5x,'R [cry]     |R| [Bohr]      Norm of H(R) [eV]')") 
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
        !
        ! compute the 2-norm of H_ij(R)
        rdum = ZERO
        DO j=1,dimwann
        DO i=1,dimwann
             rdum = rdum + REAL( CONJG( rham(i,j,iws)) * rham(i,j,iws) )
        ENDDO
        ENDDO
        WRITE(stdout,"(1x,3i4,3x,f11.7,4x,f15.9)") indxws(:,iws), rmod, SQRT(rdum)
      ENDDO
      WRITE(stdout,*) 

 
! ... Determine the k-points used in the band structure plot

! XXX
      ALLOCATE( xval( npts ), STAT=ierr )
          IF( ierr /=0 ) CALL errore(' hamiltonian ', ' allocating xval', npts )
      ALLOCATE( sxval( nspts ), STAT=ierr )
          IF( ierr /=0 ) CALL errore(' hamiltonian ', ' allocating sxval', nspts )
      ALLOCATE( kpt(3, npts), STAT=ierr )
          IF( ierr /=0 ) CALL errore(' hamiltonian ', ' allocating kpt', 3* npts )
 
      CALL get_points( nspts, npts, bvec, skpt, kpt, xval, sxval, tnkpts )
 
! ... Estimate H_ij(k') at those k-points by fourier interpolation
!     H_ij(k') ~ sum_R e^{ik'R} H_ij(R)/degen(R), where the sum over R is over a 
!     finite grid (truncation)
 
      ALLOCATE( ham_tmp( dimwann, dimwann ), STAT=ierr )
          IF( ierr /=0 ) CALL errore(' hamiltonian ', ' allocating ham_tmp', dimwann**2 )
 
      ALLOCATE( en_band( dimwann, tnkpts ), STAT=ierr )
          IF( ierr /=0 ) CALL errore(' hamiltonian ', ' allocating en_band', dimwann*tnkpts )
 
      DO irk = 1, tnkpts

        DO j = 1, dimwann
          DO i = 1, dimwann
            ham_tmp(i,j) = czero
            DO iws = 1, nws
              expo = EXP( CI * TPI * ( kpt(1,irk) * DBLE( indxws(1,iws) ) +  &     
              kpt(2,irk) * DBLE( indxws(2,iws) ) +                             &
              kpt(3,irk) * DBLE( indxws(3,iws) ) ) ) 
              ham_tmp(i,j) = ham_tmp(i,j) + expo * rham(i,j,iws) / degen(iws)
            ENDDO
          ENDDO
        ENDDO
 
! ...   Diagonalize the hamiltonian at the present k-point

        ALLOCATE( ap( dimwann * ( dimwann + 1 ) / 2 ), STAT=ierr )
            IF( ierr /=0 ) & 
            CALL errore(' hamiltonian ', ' allocating ap', dimwann * (dimwann+1)/2 )
        ALLOCATE( w( dimwann ), ifail( dimwann ), STAT=ierr )
            IF( ierr /=0 ) CALL errore(' hamiltonian ', ' allocating w ifail', 2*dimwann )
        ALLOCATE( z( dimwann, dimwann ), STAT=ierr )
            IF( ierr /=0 ) CALL errore(' hamiltonian ', ' allocating z', dimwann**2 )
        ALLOCATE( work(2*dimwann), rwork( 7 * dimwann ), iwork( 5 * dimwann ), STAT=ierr )
            IF( ierr /=0 ) &
            CALL errore(' hamiltonian ', ' allocating work rwork iwork', 14*dimwann )
 
        DO j = 1, dimwann
          DO i = 1, j
            ap( i + ( (j-1)*j ) / 2 ) = ham_tmp(i,j)
          END DO
        END DO

        CALL ZHPEVX( 'n', 'i', 'u', dimwann, ap(1), ZERO, ZERO, 1, dimwann, -ONE,     &
             m, w(1), z(1,1), dimwann, work(1), rwork(1), iwork(1), ifail(1), info )
        IF ( info < 0 ) CALL errore('hamiltonian', 'zhpevx had an illegal value (II)',-info)
        IF ( info > 0 ) CALL errore('hamiltonian', 'zhpevx diagonalization failed (II)',info)

        en_band(:,irk) = w(:)

        DEALLOCATE( ap, w, z, work, rwork, iwork, ifail, STAT=ierr )
            IF( ierr /=0 ) CALL errore(' hamiltonian ', ' deallocating ap-ifail', ABS(ierr))

      ENDDO ! IRK

 
      OPEN( 27, FILE='band.dat', STATUS='UNKNOWN', FORM='FORMATTED' )

      DO i = 1, dimwann
        DO irk = 1, tnkpts
          WRITE (27, fmt="(2e16.8)") xval(irk), en_band(i,irk)
        END DO
        WRITE( 27, *) 
      END DO

      CLOSE( 27 )
 
!
! ... Finalize timing
      CALL timing('hamiltonian',OPR='stop')
      CALL timing_overview(stdout,LIST=global_list,MAIN_NAME='hamiltonian')

!
! ... Clean memory
      DEALLOCATE( point, STAT=ierr)
          IF( ierr /=0 ) CALL errore(' hamiltonian ', ' deallocating point', ABS(ierr) )
      DEALLOCATE( skpt, STAT=ierr)
          IF( ierr /=0 ) CALL errore(' hamiltonian ', ' deallocating skpt', ABS(ierr) )
      DEALLOCATE( kham, STAT=ierr)
          IF( ierr /=0 ) CALL errore(' hamiltonian ', ' deallocating kham', ABS(ierr) )
      DEALLOCATE( indxws, STAT=ierr)
          IF( ierr /=0 ) CALL errore(' hamiltonian ', ' deallocating indxws', ABS(ierr) )
      DEALLOCATE( degen, STAT=ierr)
          IF( ierr /=0 ) CALL errore(' hamiltonian ', ' deallocating degen', ABS(ierr) )
      DEALLOCATE( rham, STAT=ierr)
          IF( ierr /=0 ) CALL errore(' hamiltonian ', ' deallocating rham', ABS(ierr) )
      DEALLOCATE( xval, STAT=ierr)
          IF( ierr /=0 ) CALL errore(' hamiltonian ', ' deallocating xval', ABS(ierr) )
      DEALLOCATE( sxval, STAT=ierr)
          IF( ierr /=0 ) CALL errore(' hamiltonian ', ' deallocating sxval', ABS(ierr) )
      DEALLOCATE( kpt, STAT=ierr)
          IF( ierr /=0 ) CALL errore(' hamiltonian ', ' deallocating kpt', ABS(ierr) )
      DEALLOCATE( ham_tmp, STAT=ierr)
          IF( ierr /=0 ) CALL errore(' hamiltonian ', ' deallocating ham_tmp', ABS(ierr) )
      DEALLOCATE( en_band, STAT=ierr)
          IF( ierr /=0 ) CALL errore(' hamiltonian ', ' deallocating en_band', ABS(ierr) )

      CALL cleanup()

! XXX sistemare MPI environment
!       call mp_end()

      STOP '*** THE END *** (hamiltonian.x)'
      END






