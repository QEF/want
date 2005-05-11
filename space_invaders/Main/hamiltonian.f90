! 
! Copyright (C) 2004 WanT Group
! Copyright (C) 2002 Nicola Marzari, Ivo Souza, David Vanderbilt
! Copyright (C) 1997 Nicola Marzari, David Vanderbilt
!
! This file is distributed under the terms of the
! GNU General Public License. See the file `License'
! in the root directory of the present distribution,
! or http://www.gnu.org/copyleft/gpl.txt .
!
!*****************************************************
      PROGRAM hamiltonian
!*****************************************************
!  
! ... Calculates the band structure and  the
!     matrix elements H(R) in a Wigner-Seitz supercell
! 
!     NOTA: the output fmts should be improved in a small number of files within
!           different structure very soon.
!

      USE kinds
      USE constants, ONLY: PI, TPI, ZERO, CZERO, CI, ONE, TWO, EPS_m6
      USE control_module, ONLY : verbosity
      USE parameters, ONLY : nstrx, nkpts_inx
      USE io_module, ONLY : stdout, stdin, ioname, ham_unit, space_unit, wan_unit
      USE io_module, ONLY : work_dir, prefix, postfix
      USE files_module, ONLY : file_open, file_close
      USE timing_module, ONLY : timing, timing_deallocate, timing_overview, global_list
      USE startup_module, ONLY : startup
      USE cleanup_module, ONLY : cleanup
      USE want_init_module, ONLY : want_init
      USE summary_module, ONLY : summary
      USE version_module, ONLY : version_number
      USE util_module, ONLY : zmat_unitary, zmat_hdiag
      USE parser_module, ONLY : int2char, change_case
      USE converters_module, ONLY : cart2cry

      USE lattice_module, ONLY : avec, bvec
      USE kpoints_module,       ONLY : nkpts, nk, vkpt
      USE windows_module,       ONLY : imin, imax, nbnd, eig, efermi, &
                                       windows_read, spin_component
      USE subspace_module,      ONLY : wan_eig, subspace_read
      USE localization_module,  ONLY : dimwann, cu, & 
                                       localization_read


      IMPLICIT NONE 

      COMPLEX(dbl) :: expo
      COMPLEX(dbl), ALLOCATABLE :: kham(:,:,:)   
      COMPLEX(dbl), ALLOCATABLE :: rham(:,:,:)   
      COMPLEX(dbl), ALLOCATABLE :: ham_tmp(:,:)  
      COMPLEX(dbl), ALLOCATABLE :: z(:,:)        

      INTEGER :: nkpts_in     ! Number of k-points generating the line (edges)
      INTEGER :: nkpts_max    ! maximum number of interpolated points
      INTEGER :: nkpts_tot    ! actual number of point in the line
      !
      INTEGER :: nws
      REAL(dbl), ALLOCATABLE :: kpt_in(:,:), xval_in(:) 
      REAL(dbl), ALLOCATABLE :: vkpt_cry(:,:)
      REAL(dbl), ALLOCATABLE :: kpt(:,:),    xval(:) 
      REAL(dbl), ALLOCATABLE :: eig_int(:,:)  ! interpolated band structure   
      INTEGER, ALLOCATABLE :: indxws(:,:)       
      INTEGER, ALLOCATABLE :: degen(:)          
      INTEGER :: i, j, m, n, ik, idum
      INTEGER :: iws
      REAL(dbl) :: rmod, rdum, vec(3)
      CHARACTER(LEN=2), ALLOCATABLE :: point(:)    
      CHARACTER(LEN=nstrx)          :: filename
      REAL(dbl) :: unitary_thr
 
      INTEGER   :: ierr
      LOGICAL   :: lfound, opened

      LOGICAL   :: convert_self_energy
      LOGICAL   :: check_self_energy
      LOGICAL   :: calculate_spectral_func
      INTEGER   :: print_sgm_start
      INTEGER   :: print_sgm_end

      NAMELIST /INPUT/ prefix, postfix, work_dir, verbosity, &
                       nkpts_in, nkpts_max, convert_self_energy, check_self_energy, & 
                       calculate_spectral_func, print_sgm_start, print_sgm_end,  &
                       unitary_thr, spin_component

!
! ... End declarations and dimensions
!
!=-------------------------------------------------------------------------------=

!
! ...  Startup
!
      CALL startup(version_number,MAIN_NAME='hamiltonian')


!...  Read (from stdin) information for plotting band structure
!     and for eventually converting a self energy to wannier basis             
!
      prefix                      = 'WanT' 
      postfix                     = ' ' 
      work_dir                    = './' 
      verbosity                   = 'medium' 
      nkpts_in                    = 0
      nkpts_max                   = 100
      convert_self_energy         = .FALSE.
      check_self_energy           = .FALSE.
      calculate_spectral_func     = .FALSE.
      unitary_thr                 = EPS_m6
      print_sgm_start             = 0
      print_sgm_end               = 0
      spin_component              = 'none'
      
      READ(stdin, INPUT, IOSTAT=ierr)
      IF ( ierr /= 0 )  CALL errore('hamiltonian','Unable to read namelist INPUT',ABS(i))


! ... Some checks (but many more should be included)
      IF ( nkpts_in > nkpts_inx ) CALL errore('hamiltonian', 'nkpts_in too large',  nkpts_in)
      IF ( nkpts_in <= 0 ) CALL errore('hamiltonian', 'Invalid nkpts_in', ABS(nkpts_in)+1)
      IF ( unitary_thr <=0 ) CALL errore('hamiltonian', 'invalid unitary_thr', 3)

      CALL change_case(verbosity,'lower')
      IF ( TRIM(verbosity) /= 'low' .AND. TRIM(verbosity) /= 'medium' .AND. &
           TRIM(verbosity) /= 'high' ) &
           CALL errore('hamiltonian','Invalid verbosity = '//TRIM(verbosity),1)

      CALL change_case(spin_component,'lower')
      IF ( TRIM(spin_component) /= 'up' .AND. TRIM(spin_component) /= 'down' .AND. &
           TRIM(spin_component) /= 'none' ) &
           CALL errore('hamiltonian','Invalid spin_component = '//TRIM(spin_component),1)
 
      ALLOCATE( point( nkpts_in ), STAT=ierr )
          IF( ierr /=0 ) CALL errore('hamiltonian', 'allocating point', ABS(ierr) )
      ALLOCATE( kpt_in( 3, nkpts_in ), STAT=ierr )
          IF( ierr /=0 ) CALL errore('hamiltonian', 'allocating kpt_in', ABS(ierr) )

      DO j = 1, nkpts_in
        READ (stdin, fmt="(a2)") point(j)
        READ (stdin,*) ( kpt_in(i,j), i=1,3 )
      ENDDO

!
! ... DFT data
!
      CALL want_init( WANT_INPUT = .FALSE., WINDOWS=.FALSE., BSHELLS=.FALSE. )

      IF ( PRODUCT(nk(:)) /= nkpts ) CALL errore('hamiltonian','Invalid kpt number', &
                                     ABS( PRODUCT(nk(:)) - nkpts) )

!
! ... Read energy eigenvalues in electron-volt
!
      CALL ioname('subspace',filename)
      CALL file_open(space_unit,TRIM(filename),PATH="/",ACTION="read",FORM="formatted")
          CALL windows_read(space_unit,"WINDOWS",lfound)
          IF ( .NOT. lfound ) CALL errore('hamiltonian',"unable to find WINDOWS",1)
          CALL subspace_read(space_unit,"SUBSPACE",lfound)
          IF ( .NOT. lfound ) CALL errore('hamiltonian',"unable to find SUBSPACE",1)
      CALL file_close(space_unit,PATH="/",ACTION="read")

      CALL ioname('subspace',filename,LPATH=.FALSE.)
      WRITE( stdout,"(2x,'Subspace data read from file: ',a)") TRIM(filename)
      !
      ! ... Energy zero settings, added by ANDREA (28 jan 2004)
      !     For coherence with self-energy translate Fermi energy
      !     to zero (i.e. Efermi is the energy reference)
      !
      wan_eig(:,:) = wan_eig(:,:) - Efermi
          eig(:,:) =     eig(:,:) - Efermi

      !
      ! convert kpts to crystal units
      !
      ALLOCATE( vkpt_cry( 3, nkpts ), STAT=ierr )
          IF( ierr /=0 ) CALL errore('hamiltonian', 'allocating vkpt_cry', ABS(ierr) )
      vkpt_cry(:,:) = vkpt(:,:)
      CALL cart2cry(vkpt_cry, bvec)


!
! ... printing data to output
!
      CALL summary( stdout, LINPUT=.FALSE., LATOMS=.FALSE., LEIG=.FALSE. )

!
! ... Read unitary matrices U(k) that rotate the bloch states
      CALL ioname('wannier',filename)
      CALL file_open(wan_unit,TRIM(filename),PATH="/",ACTION="read",FORM="formatted")
          CALL localization_read(wan_unit,"WANNIER_LOCALIZATION",lfound)
          IF ( .NOT. lfound ) &
             CALL errore('hamiltonian',"unable to find WANNIER_LOCALIZATION",1)
      CALL file_close(wan_unit,PATH="/",ACTION="read")

      CALL ioname('wannier',filename,LPATH=.FALSE.)
      WRITE( stdout,"('  Wannier data read from file: ',a,/)") TRIM(filename)
 
      DO ik = 1,nkpts
         IF ( .NOT. zmat_unitary( cu(:,:,ik), SIDE='both', TOLL=unitary_thr)  ) &
             CALL errore('hamiltonian',"U matrices not orthogonal",ik)
      ENDDO

!
! ... End of input reading      
!-----------------------------------------------------------------------------------
!

      WRITE( stdout, "(/,2x,70('='))" )
      WRITE( stdout, "(2x,'=',17x,'Hamiltonian and Band interpolation',17x,'=')" )
      WRITE( stdout, "(2x,70('='),/)" )


!
! ... Calculate H(k)=U^{dagger}(k).H_0(k).U(k)
!     (hamiltonian matrix elements between the rotated bloch states)

      ALLOCATE( kham( dimwann, dimwann, nkpts ), STAT=ierr )
          IF( ierr /=0 ) CALL errore(' hamiltonian ', ' allocating khan ',dimwann**2 *nkpts)
 
      DO ik = 1, nkpts
         DO j = 1, dimwann
         DO i = 1, j
            kham(i,j,ik) = CZERO
            DO m = 1, dimwann
                kham(i,j,ik) = kham(i,j,ik) + wan_eig(m,ik) * &
                                              CONJG( cu(m,i,ik) ) * cu(m,j,ik)
            ENDDO
! ...       use hermiticity
            kham(j,i,ik) = CONJG( kham(i,j,ik) )
         ENDDO
         ENDDO
      ENDDO

 
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
            DO ik = 1, nkpts
              expo = EXP( -CI * TPI * (vkpt_cry(1,ik) * DBLE( indxws(1,iws) ) +   &
                                       vkpt_cry(2,ik) * DBLE( indxws(2,iws) ) +   &
                                       vkpt_cry(3,ik) * DBLE( indxws(3,iws) ) )   )
              rham(i,j,iws)=rham(i,j,iws)+expo*kham(i,j,ik)
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

! XXX this part should be updated
!      IF (convert_self_energy)  THEN
!           CALL do_self_energy(dimwann,nkpts,nws,spin_component,cu,vkpt_cry,indxws,bvec,     &
!                               'sigma.blc','sigma.wan')
!      END IF

!
! ... checking, if CHECK_SELF_ENERGY=.TRUE.  (added by ANDREA 28 jan 2004)
!

!      IF (check_self_energy)  THEN
!           CALL check_sgm_wan(dimwann,nws,nk,spin_component,rham,           &
!                              print_sgm_start,print_sgm_end,calculate_spectral_func)
!      END IF


!
! ... writing matrix elements to file (and partially to stdout)
!     for transport calculation
!

      WRITE(stdout,"(2/,2x,'Diagonal matrix elements of H on Wannier basis')")
      WRITE(stdout,"(   2x,'dimwann = ',i5)") dimwann

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

          INQUIRE(ham_unit, OPENED=opened)
          IF ( opened ) CALL errore('hamiltonian','Unit already opened',ham_unit)

          idum = 100+iws
          filename=TRIM(work_dir)//'/RHAM.'//TRIM(int2char(idum))
          OPEN(UNIT=ham_unit,FILE=TRIM(filename),  STATUS='unknown', FORM='formatted' )
  
              WRITE (ham_unit,"(2i5,3x,3i4)") dimwann, dimwann, ( indxws(i,iws), i=1,3 )
              DO j = 1, dimwann
                  WRITE (ham_unit,"()")
                  DO i = 1, dimwann
                     WRITE(ham_unit, "(2f20.12)" ) rham(i,j,iws)
                  ENDDO
              ENDDO
          CLOSE(ham_unit)

          !
          ! stdout (diagonal elements)
          !
          WRITE (stdout,"(/,4x,'R = (',3i4,' )')") ( indxws(i,iws), i=1,3 )
          DO i = 1, dimwann
              WRITE(stdout,"(2f15.9)") rham(i,i,iws)
          ENDDO

        ENDIF
      ENDDO
      WRITE (stdout,"()") 


! ... Check that magnitude of matrix elements |H_ij(R)| decreases with |R|.
!     Should expect it to decrease *faster* using the rotated Bloch functions
!     (again, except in the single-band case, where it should be exactly the same)

      WRITE(stdout,"(/,2x,'Decay of the real space Hamiltonian:',/)") 
      WRITE(stdout,"(  5x,'R [cry]     |R| [Bohr]      Norm of H(R) [eV]')") 
      DO iws = 1, nws
        vec(1) = DBLE( indxws(1,iws) ) * avec(1,1) +     &
                 DBLE( indxws(2,iws) ) * avec(1,2) +     &
                 DBLE( indxws(3,iws) ) * avec(1,3)
        vec(2) = DBLE( indxws(1,iws) ) * avec(2,1) +     &
                 DBLE( indxws(2,iws) ) * avec(2,2) +     &
                 DBLE( indxws(3,iws) ) * avec(2,3)
        vec(3) = DBLE( indxws(1,iws) ) * avec(3,1) +     &
                 DBLE( indxws(2,iws) ) * avec(3,2) +     &
                 DBLE( indxws(3,iws) ) * avec(3,3)
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

      ALLOCATE( xval( nkpts_max ), STAT=ierr )
          IF( ierr /=0 ) CALL errore(' hamiltonian ', ' allocating xval', nkpts_max )
      ALLOCATE( xval_in( nkpts_in ), STAT=ierr )
          IF( ierr /=0 ) CALL errore(' hamiltonian ', ' allocating xval_in', nkpts_in )
      ALLOCATE( kpt(3, nkpts_max), STAT=ierr )
          IF( ierr /=0 ) CALL errore(' hamiltonian ', ' allocating kpt', 3* nkpts_max )
 
      CALL get_points(nkpts_in, nkpts_max, bvec, kpt_in, xval_in, point, kpt, xval, nkpts_tot )
 
! ... Estimate H_ij(k') at those k-points by fourier interpolation
!     H_ij(k') ~ sum_R e^{ik'R} H_ij(R)/degen(R), where the sum over R is over a 
!     finite grid (truncation)
 
      ALLOCATE( ham_tmp( dimwann, dimwann ), STAT=ierr )
          IF( ierr /=0 ) CALL errore('hamiltonian','allocating ham_tmp',dimwann**2 )
      ALLOCATE( eig_int( dimwann, nkpts_tot ), STAT=ierr )
          IF( ierr /=0 ) CALL errore('hamiltonian','allocating eig_int',dimwann*nkpts_tot )
 
      DO ik = 1, nkpts_tot

         DO j = 1, dimwann
         DO i = 1, dimwann
            ham_tmp(i,j) = CZERO
            DO iws = 1, nws
               expo = EXP( CI * TPI * ( kpt(1,ik) * DBLE( indxws(1,iws) ) +  &     
               kpt(2,ik) * DBLE( indxws(2,iws) ) +                             &
               kpt(3,ik) * DBLE( indxws(3,iws) ) ) ) 
               ham_tmp(i,j) = ham_tmp(i,j) + expo * rham(i,j,iws) / degen(iws)
            ENDDO
         ENDDO
         ENDDO
 
         !
         ! ... Diagonalize the hamiltonian at the present k-point
         ALLOCATE( z( dimwann, dimwann ), STAT=ierr )
             IF( ierr /=0 ) CALL errore(' hamiltonian ', ' allocating z', dimwann**2 )
 
         CALL zmat_hdiag( z, eig_int(:,ik), ham_tmp(:,:), dimwann)

         DEALLOCATE( z, STAT=ierr )
             IF( ierr /=0 ) CALL errore(' hamiltonian ', ' deallocating z', ABS(ierr))

      ENDDO 

! 
! ... to be updated soon or later (?)
! 
      filename=TRIM(work_dir)//TRIM(prefix)//TRIM(postfix)//'_intband.dat'
      OPEN( ham_unit, FILE=TRIM(filename), STATUS='unknown', FORM='formatted' )
      DO i = 1, dimwann
          DO ik = 1, nkpts_tot
            WRITE (ham_unit, "(2e16.8)") xval(ik)/(TWO*xval(nkpts_tot)), eig_int(i,ik)
            !
            ! Note that the factor TWO appear in order to be consistent with the 
            ! x units of the other two plots (wanband and dftband) in the case where
            ! a 1D BZ is treated, dft kpts are uniform in the BZ and the interpolated
            ! kpts go from Gamma to X,Y, or Z.
            ! 
            ! This is a particular case, bu the most common in WF calculation for transport
            !
          ENDDO
          WRITE( ham_unit, "()") 
      ENDDO
      CLOSE( ham_unit )

      !
      ! as a check
      !
      filename=TRIM(work_dir)//TRIM(prefix)//TRIM(postfix)//'_wanband.dat'
      OPEN( ham_unit, FILE=TRIM(filename), STATUS='unknown', FORM='formatted' )
      DO i = 1, dimwann
          DO ik = 1, nkpts
            WRITE (ham_unit, fmt="(2e16.8)") REAL(ik-1)/REAL(nkpts), wan_eig(i,ik)
          ENDDO
          WRITE( ham_unit, "()") 
      ENDDO
      CLOSE( ham_unit )

      filename=TRIM(work_dir)//TRIM(prefix)//TRIM(postfix)//'_dftband.dat'
      OPEN( ham_unit, FILE=TRIM(filename), STATUS='unknown', FORM='formatted' )
      DO i = 1, nbnd
          DO ik = 1, nkpts
             IF ( i >= imin(ik) .AND. i <= imax(ik) ) THEN
                 WRITE (ham_unit, fmt="(2e16.8)") REAL(ik-1)/REAL(nkpts), eig(i,ik)
             ELSE
                 WRITE (ham_unit, "()")
             ENDIF
          ENDDO
          WRITE( ham_unit, "()") 
      ENDDO
      CLOSE( ham_unit )

!
! ... Finalize timing
      CALL timing('hamiltonian',OPR='stop')
      CALL timing_overview(stdout,LIST=global_list,MAIN_NAME='hamiltonian')

!
! ... Clean memory
      DEALLOCATE( vkpt_cry, STAT=ierr)
          IF( ierr /=0 ) CALL errore(' hamiltonian ', ' deallocating vkpt_cry', ABS(ierr) )
      DEALLOCATE( point, STAT=ierr)
          IF( ierr /=0 ) CALL errore(' hamiltonian ', ' deallocating point', ABS(ierr) )
      DEALLOCATE( kpt_in, STAT=ierr)
          IF( ierr /=0 ) CALL errore(' hamiltonian ', ' deallocating kpt_in', ABS(ierr) )
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
      DEALLOCATE( xval_in, STAT=ierr)
          IF( ierr /=0 ) CALL errore(' hamiltonian ', ' deallocating xval_in', ABS(ierr) )
      DEALLOCATE( kpt, STAT=ierr)
          IF( ierr /=0 ) CALL errore(' hamiltonian ', ' deallocating kpt', ABS(ierr) )
      DEALLOCATE( ham_tmp, STAT=ierr)
          IF( ierr /=0 ) CALL errore(' hamiltonian ', ' deallocating ham_tmp', ABS(ierr) )
      DEALLOCATE( eig_int, STAT=ierr)
          IF( ierr /=0 ) CALL errore(' hamiltonian ', ' deallocating eig_int', ABS(ierr) )

      CALL cleanup()

      STOP '*** THE END *** (hamiltonian.x)'
      END






