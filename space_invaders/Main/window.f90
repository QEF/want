       PROGRAM window
 
       USE kinds
       USE constants, ONLY: ryd => ry, har => au, amu => uma_au
       USE parameters, ONLY: mxdtyp => npsx, mxdatm => natx, dp
       USE timing_module, ONLY : timing, timing_deallocate, timing_overview
       USE io_global, ONLY : stdout
       USE startup_module, ONLY : startup
       USE version_module, ONLY : version_number
       USE input_wannier

       IMPLICIT NONE
 
       COMPLEX(kind=DP), ALLOCATABLE, TARGET :: zvec_k(:,:,:)
       COMPLEX(kind=DP), ALLOCATABLE, TARGET :: wtmp(:)
       REAL(dbl), ALLOCATABLE :: ei_k(:,:)
       INTEGER, ALLOCATABLE :: isort_k(:,:)
       INTEGER, ALLOCATABLE :: mtxd_k(:)
       INTEGER, ALLOCATABLE :: neig_k(:)
       INTEGER, ALLOCATABLE :: indxfroz(:,:)
       INTEGER, ALLOCATABLE :: indxnfroz(:,:)
       INTEGER, ALLOCATABLE :: dimfroz(:)
       LOGICAL, ALLOCATABLE :: frozen(:,:)

       INTEGER, ALLOCATABLE :: ig1(:), ig2(:), ig3(:)

       INTEGER, ALLOCATABLE :: natom(:)
       REAL(dbl), ALLOCATABLE :: rat(:,:,:)
       CHARACTER(LEN=2), ALLOCATABLE :: nameat(:)
       CHARACTER(LEN=3) :: namtmp
 
       INTEGER :: nwann

       REAL(dbl) :: s(3)
       INTEGER :: nk(3)
       INTEGER :: i, j, i1, i2, i3
       INTEGER :: ierr
       INTEGER :: nkp
       INTEGER :: mtxd, neig, imax, imin, kdimwin
       INTEGER :: nkp_tot, kifroz_min, kifroz_max

       REAL(dbl) :: emax, rdum
       INTEGER :: ntype
       INTEGER :: nr1, nr2, nr3, nbandi 
       
       INTEGER :: ngwx
       INTEGER :: mxddim       ! max number of G-vectors
       INTEGER :: nkpts       ! max number of k-points
       INTEGER :: mxdbnd       ! max number of bands 
       INTEGER :: ngm
       INTEGER :: ngm0

       REAL(dbl) :: avec(3,3)
       REAL(dbl) :: alat, sgn
       REAL(dbl) :: zero, um, tres
       INTEGER :: ja, jmax, nt, ig
       PARAMETER ( zero = 0.0d0, um = 1.0d0, tres = 3.0d0 )

! ...  Test versione parallela

       CHARACTER(LEN=20) :: section_name = 'wfc'
       INTEGER :: file_version
       LOGICAL :: twrite, t0_
       INTEGER :: ngw_, nbnd_, ik_, nk_, kunit_, ispin_, nspin_
       REAL(dbl) :: scal_
       INTEGER :: igwx_, idum_
       COMPLEX(dbl) :: wf_sum
!
! ...  End declarations and dimensions
!
!=----------------------------------------------------------------------------=!

!
! ...  Startup
!
       CALL startup(version_number,MAIN_NAME='window')


!
! ...  Read input parameters
!
       ALLOCATE ( natom(mxdtyp), STAT=ierr )
           IF( ierr /=0 ) CALL errore(' window ', ' allocating natom ', mxdtyp )
       ALLOCATE ( nameat(mxdtyp), STAT=ierr )
           IF( ierr /=0 ) CALL errore(' window ', ' allocating nameat ', mxdtyp )
       ALLOCATE ( rat(3,mxdatm,mxdtyp), STAT=ierr )
           IF( ierr /=0 ) CALL errore(' window ', ' allocating rat ', 3*mxdatm*mxdtyp )

!
       CALL read_input()

!
! ...  Read wavefunctions and eigenvalues from first principle calculation
!

       OPEN ( UNIT=54, FILE='launch.dat', STATUS='OLD', FORM='UNFORMATTED' )

       READ( 54 ) alat
       READ( 54 ) ( avec(i,1),i=1,3 )
       READ( 54 ) ( avec(i,2),i=1,3 )
       READ( 54 ) ( avec(i,3),i=1,3 )


       READ( 54 ) ntype
       IF ( ntype > mxdtyp .OR. ntype < 0 ) THEN
         WRITE ( stdout, fmt="('*** error: ntype out of range ***')" )
         STOP
       END IF

       !  for each atomic specie read the number of atoms (natom)
       !  the name of the specie (nameat) and the atomic coordinate
       !  (rat) in lattice coordinate

       DO nt = 1, ntype
         READ ( 54 ) natom(nt), namtmp

         IF ( natom(nt) > mxdatm .OR. natom(nt) < 0 ) THEN
           WRITE(stdout, fmt="('*** error: natom out of range ***')")
           STOP
         END IF
         nameat( nt ) = namtmp(1:2)
         READ ( 54 ) ( ( rat( i, ja, nt ), i = 1, 3 ), ja = 1, natom( nt ) )
       END DO

       !  read the energy cutoff (emax) in Rydberg unit 
       !  nbandi is the number of electronic bands
       !  nk(1), nk(2), nk(3) are the number of k-points
       !  s(1), s(2), s(3) is the origin of the k-points grid

       READ(54) emax, nbandi

       READ(54) (nk(i), i = 1, 3 ), ( s(i), i = 1, 3), ngm0
       ALLOCATE( ig1( ngm0 ), ig2( ngm0 ), ig3( ngm0 ), STAT=ierr )
           IF( ierr /=0 ) CALL errore(' window ', ' allocating ig1 ig2 ig3 ', ngm0 )
       READ(54) ( ig1(ig), ig2(ig), ig3(ig), ig = 1, ngm0 )

       emax = emax / 2.0   !  convert to Hartree
       nkp_tot = nk(1) * nk(2) * nk(3)

       READ(54) mxddim, mxdbnd, nkpts

       ALLOCATE( isort_k( mxddim, nkpts ), STAT=ierr )
           IF( ierr /=0 ) CALL errore(' window ', ' allocating isort_k ', mxddim*nkpts )
       ALLOCATE( zvec_k(mxddim,mxdbnd,nkpts), STAT=ierr )
           IF( ierr /=0 ) &
           CALL errore(' window ', ' allocating zvec_k ', mxddim*nkpts*mxdbnd )
       ALLOCATE( ei_k(mxddim,nkpts), STAT=ierr )
           IF( ierr /=0 ) CALL errore(' window ', ' allocating ei_k ', mxddim*nkpts )
       ALLOCATE( mtxd_k(nkpts), STAT=ierr )
           IF( ierr /=0 ) CALL errore(' window ', ' allocating mtxd_k ', nkpts )
       ALLOCATE( neig_k(nkpts), STAT=ierr )
           IF( ierr /=0 ) CALL errore(' window ', ' allocating neig_k ', nkpts )
       ALLOCATE( indxfroz(mxdbnd,nkpts), STAT=ierr )
           IF( ierr /=0 ) CALL errore(' window ', ' allocating indxfroz ', mxddim*nkpts )
       ALLOCATE( indxnfroz(mxdbnd,nkpts), STAT=ierr )
           IF( ierr /=0 ) CALL errore(' window ', ' allocating indxnfroz ', mxddim*nkpts )
       ALLOCATE( dimfroz(nkpts), STAT=ierr )
           IF( ierr /=0 ) CALL errore(' window ', ' allocating dimfroz ', nkpts )
       ALLOCATE( frozen(mxdbnd,nkpts), STAT=ierr )
           IF( ierr /=0 ) CALL errore(' window ', ' allocating frozen ', mxdbnd*nkpts )

       DO nkp = 1, nkp_tot
         READ(54) ( isort_k( i, nkp ), i = 1, mxddim )
       END DO
       READ(54) ( (ei_k(i,nkp), i=1,mxdbnd ), nkp=1,nkp_tot )
       READ(54) ( mtxd_k(nkp), nkp=1,nkp_tot )
       READ(54) ( neig_k(nkp), nkp=1,nkp_tot )
       READ(54) nr1, nr2, nr3, ngm, ngwx

       zvec_k = 0.0d0
       DO nkp = 1, nkp_tot
         READ(54) twrite, file_version, section_name
         READ(54) ngw_, nbnd_, ik_, nk_, kunit_, ispin_, nspin_, scal_
         READ(54) igwx_
         READ(54) t0_

         ALLOCATE( wtmp( igwx_ ), STAT=ierr )
            IF( ierr /=0 ) CALL errore(' window ', ' allocating wtmp ', igwx_ )
         wtmp = 0.0d0
         DO i = 1, nbnd_
           READ(54) ( wtmp(ig), ig=1,igwx_ )
           DO ig = 1, mtxd_k( nkp )
             zvec_k( ig, i, nkp ) = wtmp( isort_k( ig, nkp ) )
           END DO
         END DO
         READ(54) t0_
         DO i = 1, nbnd_
           READ(54) idum_
!          WRITE(stdout,*) '          ', idum_
         END DO
         DEALLOCATE( wtmp, STAT=ierr )
            IF( ierr /=0 ) CALL errore(' window ', ' deallocating wtmp ', ABS(ierr) )
       END DO

       CLOSE(54)


! ...  End reading

!
! ...  Write input parameters
!
       WRITE( stdout, fmt= " (/,' *Input parameters*',/ )")
       WRITE( stdout, fmt= " (' alat = ', F10.6,/)" ) alat
       DO i = 1, 3
         WRITE ( stdout, fmt="(' a(',I1,')  ',3F10.6)" ) i, avec(1,i), avec(2,i), avec(3,i)
       END DO

       WRITE( stdout,*) ' ' 
       DO nt = 1, ntype
         DO ja = 1, natom( nt )
           WRITE( stdout, fmt="(' tau(',I1,')',3F10.6)" ) ja, (rat( i, ja, nt ), i = 1, 3)
         END DO
       END DO

       WRITE(stdout,*) ' ' 
       WRITE(stdout, fmt= " (' emax = ', F5.2, ' (Hartee)' )" ) emax
       WRITE(stdout,*) ' ' 
       WRITE(stdout,*) 'Uniform grid used in wannier calculations'
       WRITE(stdout, fmt= " (' NK = (', 3i3, ' )    S = (', 3f7.4, ' )' )" ) &
                               nk(1), nk(2), nk(3), s(1), s(2), s(3)
       WRITE(stdout, fmt= " ('nkp_tot = ',i3 )" ) nkp_tot
       WRITE(stdout,*) ' ' 
       WRITE(stdout, fmt= " (' Outer Window: win_min  = ', f9.4, '  win_max  = ',f9.4 )" ) &
                               win_min, win_max
       WRITE(stdout, fmt= " (' Inner Window: froz_min = ', f9.4, '  froz_max = ',f9.4 )" ) &
                               froz_min, froz_max
       WRITE(stdout,*) ' ' 
       WRITE(stdout, fmt= " (' Number of Wannier functions required: dimwann = ', i5 )" ) &
                               dimwann
       WRITE(stdout,*) ' ' 
       WRITE(stdout,*) 'Grid dimensions:'
       WRITE(stdout, fmt= " (' mxddim = ', i7, '    ngm = ', i7 )" )  mxddim, ngm
       WRITE(stdout, fmt= " (' FFT grid ( ', 3i3, ' )' )" )  nr1, nr2, nr3

       WRITE(stdout,*) ' ' 
       WRITE(stdout,*) ' ' 
       WRITE(stdout,*) '==================== ' 
       WRITE(stdout,*) '  Band structure ' 
       WRITE(stdout,*) '==================== ' 
       WRITE(stdout,*) ' ' 
 
 
!=----------------------------------------------------------------------------=!
  
! ...  Open the output file takeoff.dat (UNIT 19) 
!      window.out will be read by all further programs of the chain
 

       OPEN ( UNIT=19, FILE='takeoff.dat', STATUS='UNKNOWN', FORM='UNFORMATTED' )

       WRITE(19) alat
       WRITE(19) ( avec(i,1), i=1,3 )
       WRITE(19) ( avec(i,2), i=1,3 )
       WRITE(19) ( avec(i,3), i=1,3 )
       WRITE(19) ntype
       DO nt = 1, ntype
         WRITE(19) natom(nt), nameat(nt)
         DO ja = 1, natom(nt)
           WRITE(19) ( rat(i,ja,nt), i=1,3 )
         END DO
       END DO
       WRITE(19) emax, nbandi
       WRITE(19) ( nk(i), i=1,3 ), ( s(i), i=1,3 )
       WRITE(19) win_min, win_max, froz_min, froz_max, dimwann
       WRITE(19) alpha, maxiter
       WRITE(19) iphase
       WRITE(19) niter0, alphafix0
       WRITE(19) niter, alphafix, ncg
       WRITE(19) itrial, nshells
       WRITE(19) ( nwhich(i), i=1,nshells )
       WRITE(19) nkp_tot, mxddim, mxdbnd
       WRITE(19) nr1, nr2, nr3, ngm
       WRITE(19) gauss_typ( 1:dimwann )
       WRITE(19) rphiimx1( 1:3, 1:dimwann )
       WRITE(19) rphiimx2( 1:3, 1:dimwann )
       WRITE(19) l_wann( 1:dimwann )
       WRITE(19) m_wann( 1:dimwann )
       WRITE(19) ndir_wann( 1:dimwann )
       WRITE(19) rloc( 1:dimwann )

       WRITE(19) ngm0
       WRITE(19)( ig1(ig), ig2(ig), ig3(ig), ig = 1, ngm0 )
 
! ...  Start K-loop
 
       nkp = 0
       loop_x: DO i1 = 0, nk(1)-1
       loop_y: DO i2 = 0, nk(2)-1
       loop_z: DO i3 = 0, nk(3)-1

         nkp = nkp + 1
         neig = neig_k(nkp)
         mtxd = mtxd_k(nkp)

! ...    Check which eigenvalues fall within the outer energy window
 
         IF ( (har * ei_k(1,nkp) > WIN_MAX ) .OR. ( har * ei_k(neig,nkp) < win_min ) ) THEN
           WRITE(stdout,*) '*** ENERGY WINDOW CONTAINS NO EIGENSTATES! ***'
           WRITE(stdout,*) '  '
           WRITE(stdout, fmt="(' K-point = ', i3 )" ) nkp
           WRITE(stdout, fmt="(' energy window (eV):    ( ',  f9.4, ',', f9.4,' )' )") &
                               win_min, win_max
           WRITE(stdout, fmt="(' eigenvalue range (eV): ( ',  f9.4, ',', f9.4,' )' )") &
                               har*ei_k(1,nkp), har*ei_k(neig,nkp)
           STOP
         END IF

         imin = 0
         DO i = 1, neig
           IF ( imin == 0 ) THEN
             IF ( ( har*ei_k(i,nkp) >= win_min ) .AND. ( har*ei_k(i,nkp) <= win_max ) ) &
                        imin = i
             imax = i
           END IF
           IF ( har*ei_k(i,nkp) <= win_max ) imax = i
         END DO

         kdimwin = imax - imin + 1
         WRITE(stdout,*)' ' 
         WRITE(stdout,fmt= " (' kpt =', i3, ' ( ',3f6.3,' )    dimwin = ', i3, &
                           & '    mtxd = ', i7 )" )  nkp, dble(I1)/dble(NK(1)), &
                           dble(I2)/dble(NK(2)), dble(I3)/dble(NK(3)), kdimwin, mtxd
         WRITE(stdout,*) 'Eigenvalues:'
         WRITE(stdout,'(8f12.6)') ( har * ei_k(i,nkp), i=1,neig )

         IF ( kdimwin < dimwann ) THEN
           WRITE(stdout,*) '*** ERROR ***'
           WRITE (stdout, fmt="( ' dimwin (', i3, ')  <  dimwann (', i3,')' )") &
                           kdimwin, dimwann
           STOP '*** PROBLEM! ***'
         END IF

         IF ( ( IMAX < IMIN ) .OR.( IMIN < 1 ) ) THEN
           WRITE(stdout,*) '*** ERROR *** IN ENERGY WINDOW AT K-POINT', nkp
           WRITE(stdout,*) '  '
           WRITE(stdout, fmt="(' imin = ', i3 )" ) imin
           WRITE(stdout, fmt="(' imax = ', i3 )" ) imax
           WRITE(stdout, fmt="(' energy window (eV):    ( ',  f9.4, ',', f9.4,' )' )") &
                             win_min, win_max
           WRITE(stdout, fmt="(' eigenvalue range (eV): ( ',  f9.4, ',', f9.4,' )' )") &
                             har*ei_k(1,nkp), har*ei_k(neig,nkp)
           WRITE(stdout,*) 'Eigenvalues:'
           DO i = 1, neig
             WRITE(stdout,*) har*ei_k(i,nkp)
           END DO
           STOP
         END IF
!
! ...    Write in the output  file window.out the eigenvectors, energy eigenvalues, 
!        and related information for the bands (for each k-point), that fall within 
!        the outer energy window
!
!        IMAX and IMIN are needed for an eventual self-energy conversion to wannier basis.
!
!        Last change by carlo, first all dimensions, then all k-dependent vectors
!
         WRITE(19) mtxd, imin, imax, imax-imin+1 

       END DO loop_z
       END DO loop_y
       END DO loop_x


 
! ...  Start K-loop again
 
       nkp = 0
       loop_xx: DO i1 = 0, nk(1)-1
       loop_yy: DO i2 = 0, nk(2)-1
       loop_zz: DO i3 = 0, nk(3)-1

         nkp = nkp + 1
         neig = neig_k(nkp)
         mtxd = mtxd_k(nkp)

         imin = 0
         DO i = 1, neig
           IF ( imin == 0 ) THEN
             IF ( ( har*ei_k(i,nkp) >= win_min ) .AND. ( har*ei_k(i,nkp) <= win_max ) ) &
                       imin = i
             imax = i
           END IF
           IF ( har*ei_k(i,nkp) <= win_max ) imax = i
         END DO

         kdimwin = imax - imin + 1

         WRITE(19) ( isort_k(j,nkp), j=1,mtxd )
         WRITE(19) ( ei_k(j,nkp), j=imin,imax )
         WRITE(19) ( ( REAL(zvec_k(j,i,nkp)), j=1,mtxd ), i=imin,imax )
         WRITE(19) ( ( 1.0d0*AIMAG(zvec_k(j,i,nkp)), j=1,mtxd ), i=imin,imax )

         frozen(:,nkp) = .false.


! ...    Check which eigenvalues (if any) fall within the inner energy window

         kifroz_min = 0
         kifroz_max = -1

! ...    Note that the above obeys kifroz_max-kifroz_min+1=kdimfroz=0, as we want

         DO i = imin, imax
           IF ( kifroz_min == 0 ) THEN
             IF ( ( har*ei_k(i,nkp) >= froz_min ).AND.( har*ei_k(i,nkp) <= froz_max ) ) THEN
               kifroz_min = i - imin + 1        ! relative to bottom of outer window
               kifroz_max = i - imin + 1
             END IF
           ELSE IF ( har*ei_k(i,nkp) <= froz_max ) THEN
              kifroz_max = kifroz_max + 1
           END IF
         END DO

         dimfroz(nkp) = kifroz_max - kifroz_min + 1
         IF ( dimfroz(nkp) > dimwann ) THEN
           WRITE(stdout,*) '*** ERROR ***' 
           WRITE(stdout,*) 'The number of band in the inner window is grater than dimwann'
           WRITE(stdout, fmt=" ( ' at K-point ', i4, ' there are ', i2, &
                         &   ' bands inside the inner window and only ', i2,   &
                         &   ' target bands')" ) nkp, dimfroz(nkp), dimwann
           WRITE(stdout,402) ( har*ei_k(i,nkp), i=imin,imax )
 402       FORMAT( 'BANDS: (eV)', 10(f10.5,1x) )
           STOP
         END IF

! ...    Generate index array for frozen states inside inner window

         IF ( dimfroz(nkp) > 0 ) THEN
           DO i = 1, dimfroz(nkp)
             indxfroz(i,nkp) = kifroz_min + i - 1
             frozen(indxfroz(i,nkp),nkp) = .true.
           END DO
           IF ( indxfroz(dimfroz(nkp),nkp) /= kifroz_max ) THEN
             WRITE(stdout,*) 'something fishy...'
             WRITE(stdout,*) 'k-point ', nkp, ' frozen band #', i
             WRITE(stdout,*) 'dimfroz=', dimfroz(nkp)
             WRITE(stdout,*) 'kifroz_min=', kifroz_min
             WRITE(stdout,*) 'kifroz_max=', kifroz_max
             WRITE(stdout,*) 'indxfroz(i,nkp)=', indxfroz(i,nkp)
             STOP '*** ERROR ***'
           END IF
         END IF


! ...    Generate index array for non-frozen states

         i = 0
         DO j = 1, kdimwin
           IF( frozen(j,nkp) .EQV. .false. ) THEN
             i = i + 1
             indxnfroz(i,nkp) = j
           END IF
         END DO
         IF ( i /= kdimwin-dimfroz(nkp) )  STOP 'something is wrong...'

         WRITE(19) dimfroz(nkp),( frozen(i,nkp), i=1,kdimwin )
         IF ( dimfroz(nkp) > 0 ) THEN
!          WRITE(stdout,*) 'dimfroz= ', dimfroz
           WRITE(stdout,*) 'There are frozen states at k-point '
           WRITE(19) ( indxfroz(i,nkp), i=1,dimfroz(nkp) )
         END IF

         IF ( dimfroz(nkp) < kdimwin ) &
           WRITE(19) ( indxnfroz(i,nkp), i=1,kdimwin-dimfroz(nkp) )

       END DO loop_zz
       END DO loop_yy
       END DO loop_xx

       CLOSE(19)
!
       DEALLOCATE( isort_k, STAT=ierr )
           IF( ierr /=0 ) CALL errore(' window ', ' deallocating isort_k ', ABS(ierr) )
       DEALLOCATE( natom, STAT=ierr )
           IF( ierr /=0 ) CALL errore(' window ', ' deallocating natom ', ABS(ierr) )
       DEALLOCATE( nameat, STAT=ierr )
           IF( ierr /=0 ) CALL errore(' window ', ' deallocating nameat ', ABS(ierr) )
       DEALLOCATE( rat, STAT=ierr )
           IF( ierr /=0 ) CALL errore(' window ', ' deallocating rat ', ABS(ierr) )
       DEALLOCATE( zvec_k, STAT=ierr )
           IF( ierr /=0 ) CALL errore(' window ', ' deallocating zvec_k ', ABS(ierr) )
       DEALLOCATE( ei_k, STAT=ierr )
           IF( ierr /=0 ) CALL errore(' window ', ' deallocating ei_k ', ABS(ierr) )
       DEALLOCATE( mtxd_k, STAT=ierr )
           IF( ierr /=0 ) CALL errore(' window ', ' deallocating mtxd_k ', ABS(ierr) )
       DEALLOCATE( neig_k, STAT=ierr )
           IF( ierr /=0 ) CALL errore(' window ', ' deallocating neig_k ', ABS(ierr) )
       DEALLOCATE( indxfroz, STAT=ierr )
           IF( ierr /=0 ) CALL errore(' window ', ' deallocating indxfroz ', ABS(ierr) )
       DEALLOCATE( indxnfroz, STAT=ierr )
           IF( ierr /=0 ) CALL errore(' window ', ' deallocating indxnfroz', ABS(ierr) )
       DEALLOCATE( dimfroz, STAT=ierr )
           IF( ierr /=0 ) CALL errore(' window ', ' deallocating dimfroz ', ABS(ierr) )
       DEALLOCATE( frozen, STAT=ierr )
           IF( ierr /=0 ) CALL errore(' window ', ' deallocating frozen ', ABS(ierr) )
       DEALLOCATE( ig1, STAT=ierr )
           IF( ierr /=0 ) CALL errore(' window ', ' deallocating ig1 ', ABS(ierr) )
       DEALLOCATE( ig2, STAT=ierr )
           IF( ierr /=0 ) CALL errore(' window ', ' deallocating ig2 ', ABS(ierr) )
       DEALLOCATE( ig3, STAT=ierr )
           IF( ierr /=0 ) CALL errore(' window ', ' deallocating ig3 ', ABS(ierr) )

       CALL deallocate_input()

       CALL timing('window',OPR='stop')
       CALL timing('global',OPR='stop')
       CALL timing_overview(stdout,MAIN_NAME='window')
       CALL timing_deallocate()
!
       STOP '*** THE END *** (window.f90)'
       END

!=----------------------------------------------------------------------------=!
