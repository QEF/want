!
! Copyright (C) 2004 Arrigo Calzolari, Carlo Cavazzoni, Marco Buongiorno Nardelli
! Copyright (C) 2002 Nicola Marzari, Ivo Souza, David Vanderbilt
!
! This file is distributed under the terms of the
! GNU General Public License. See the file `License'
! in the root directory of the present distribution,
! or http://www.gnu.org/copyleft/gpl.txt .
!
!=----------------------------------------------------------------------------------=
       PROGRAM window
!=----------------------------------------------------------------------------------=
 
       USE kinds
       USE constants, ONLY: ryd => ry, har => au, amu => uma_au, bohr => bohr_radius_angs, &
                            ZERO, ONE, TWO, CZERO
       USE parameters, ONLY: mxdtyp => npsx, mxdatm => natx, nstrx
       USE timing_module, ONLY : timing, timing_deallocate, timing_overview, global_list
       USE io_module, ONLY : stdout, ioname, work_dir
       USE startup_module, ONLY : startup
       USE version_module, ONLY : version_number
       USE input_module
       USE converters_module, ONLY : cart2cry
       USE ions, ONLY: natom, rat, nameat
       USE kpoints_module, ONLY: nk, s

       IMPLICIT NONE
 
       COMPLEX(dbl), ALLOCATABLE, TARGET :: zvec_k(:,:,:)
       COMPLEX(dbl), ALLOCATABLE, TARGET :: wtmp(:)
       REAL(dbl), ALLOCATABLE :: ei_k(:,:)
       INTEGER, ALLOCATABLE :: isort_k(:,:)
       INTEGER, ALLOCATABLE :: npwk(:)
       INTEGER, ALLOCATABLE :: neig_k(:)
       INTEGER, ALLOCATABLE :: indxfroz(:,:)
       INTEGER, ALLOCATABLE :: indxnfroz(:,:)
       INTEGER, ALLOCATABLE :: dimfroz(:)
       LOGICAL, ALLOCATABLE :: frozen(:,:)

       INTEGER, ALLOCATABLE :: ig1(:), ig2(:), ig3(:)

       CHARACTER(LEN=3) :: namtmp
       CHARACTER(LEN=nstrx) :: filename
 
       INTEGER :: nwann

       INTEGER :: i, j, i1, i2, i3
       INTEGER :: ierr
       INTEGER :: nkp
       INTEGER :: npw, neig, imax, imin, kdimwin
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
       INTEGER :: ja, jmax, nt, ig

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
       CALL read_input()

!
! ...  Read first principles calculation DATA (UNIT= 54)
!

       OPEN (54, FILE=TRIM(work_dir)//'/'//'launch.dat', STATUS='old', FORM='unformatted' )

       READ( 54 ) alat
       READ( 54 ) ( avec(i,1),i=1,3 )
       READ( 54 ) ( avec(i,2),i=1,3 )
       READ( 54 ) ( avec(i,3),i=1,3 )

       READ( 54 ) ntype
       IF ( ntype > mxdtyp .OR. ntype < 0 ) THEN
         CALL errore(' window ', ' ntype out of range ', ntype )
       END IF

       !  for each atomic specie read the number of atoms (natom)
       !  the name of the specie (nameat) and the atomic coordinate
       !  (rat) in lattice coordinate

       DO nt = 1, ntype
         READ ( 54 ) natom(nt), namtmp

         IF ( natom(nt) > mxdatm .OR. natom(nt) < 0 ) THEN
           CALL errore(' window ', ' natom out of range ', natom(nt) )
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

       emax = emax / TWO   !  convert to Hartree
       nkp_tot = nk(1) * nk(2) * nk(3)

       READ(54) mxddim, mxdbnd, nkpts

       ALLOCATE( isort_k( mxddim, nkpts ), STAT=ierr )
           IF( ierr /=0 ) CALL errore(' window ', ' allocating isort_k ', mxddim*nkpts )
       ALLOCATE( zvec_k(mxddim,mxdbnd,nkpts), STAT=ierr )
           IF( ierr /=0 ) &
           CALL errore(' window ', ' allocating zvec_k ', mxddim*nkpts*mxdbnd )
       ALLOCATE( ei_k(mxddim,nkpts), STAT=ierr )
           IF( ierr /=0 ) CALL errore(' window ', ' allocating ei_k ', mxddim*nkpts )
       ALLOCATE( npwk(nkpts), STAT=ierr )
           IF( ierr /=0 ) CALL errore(' window ', ' allocating npwk ', nkpts )
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
       READ(54) ( npwk(nkp), nkp=1,nkp_tot )
       READ(54) ( neig_k(nkp), nkp=1,nkp_tot )
       READ(54) nr1, nr2, nr3, ngm, ngwx

       zvec_k = CZERO
       DO nkp = 1, nkp_tot
         READ(54) twrite, file_version, section_name
         READ(54) ngw_, nbnd_, ik_, nk_, kunit_, ispin_, nspin_, scal_
         READ(54) igwx_
         READ(54) t0_

         ALLOCATE( wtmp( igwx_ ), STAT=ierr )
            IF( ierr /=0 ) CALL errore(' window ', ' allocating wtmp ', igwx_ )
         wtmp = ZERO
         DO i = 1, nbnd_
           READ(54) ( wtmp(ig), ig=1,igwx_ )
           DO ig = 1, npwk( nkp )
             zvec_k( ig, i, nkp ) = wtmp( isort_k( ig, nkp ) )
           END DO
         END DO
         READ(54) t0_
         DO i = 1, nbnd_
             READ(54) idum_
         END DO
         DEALLOCATE( wtmp, STAT=ierr )
            IF( ierr /=0 ) CALL errore(' window ', ' deallocating wtmp ', ABS(ierr) )
       END DO

       CLOSE(54)

! ...  End reading


!
! ...  Converting WANNIER centers from INPUT to CRYSTAL units
!      AVEC is in units of ALAT which is in Bohr
!
       SELECT CASE ( TRIM(wannier_center_units) )
       CASE ( 'angstrom' )
           CALL cart2cry(rphiimx1,alat*bohr*avec(:,:),wannier_center_units)
           CALL cart2cry(rphiimx2,alat*bohr*avec(:,:),wannier_center_units)
       CASE ( 'bohr' )
           CALL cart2cry(rphiimx1,alat*avec(:,:),wannier_center_units)
           CALL cart2cry(rphiimx2,alat*avec(:,:),wannier_center_units)
       CASE ( 'crystal' )
       CASE DEFAULT
           CALL errore('window','Invalid wannier center units : '  &
                                 //TRIM(wannier_center_units),1 )
       END SELECT

!
! ...  Write input parameters
!
       WRITE( stdout, "(2x,70('='))" )
       WRITE( stdout, "('  =',19x,'Input data from PW calculation',19x,'=')" )
       WRITE( stdout, "(2x,70('='))" )
       WRITE( stdout, " (/,2x,'Alat = ', F8.4, ' (Bohr)' )" ) alat
       WRITE( stdout, " (/,2x,'Crystal axes: (cart. coord. in units of a_0)' ) " )
       DO i = 1, 3
           WRITE ( stdout, "(10x,'a(',I1,') = (', 3F8.4, ' )' )" ) &
                  i, avec(1,i), avec(2,i), avec(3,i)
       END DO
       
       WRITE( stdout, " (/,2x,'Number of chemical species =', i3 ) " ) ntype
       WRITE( stdout, " (/,2x,'Atomic positions: (cart. coord. in units of crystal)' ) " )
       DO nt = 1, ntype
         DO ja = 1, natom( nt )
           WRITE( stdout, "(4x, a, 2x,'tau( ',I3,' ) = (', 3F8.4, ' )' )" ) &
                  nameat( nt ), ja, (rat( i, ja, nt ), i = 1, 3)
         END DO
       END DO

       ! NOTA BENE
       ! emax is reported in output in Rydberg, but it used in Hartree in the code
       WRITE( stdout, " (/,2x, 'Kinetic energy cut-off = ', F7.2, ' (Ry)' ) " ) emax * 2.0
       WRITE( stdout, " (/,2x, 'Uniform grid used in wannier calculations:' ) " )
       WRITE( stdout, " (4x,'nk = (', 3i3, ' )      s = (', 3f6.2, ' )' )" ) &
                               nk(1), nk(2), nk(3), s(1), s(2), s(3)
       WRITE( stdout, " (4x,'total number of k points =',i5 )" ) nkp_tot

       WRITE( stdout, " (/,2x,'Number of energy bands =', i5 ) " ) nbandi
       WRITE( stdout, " (/,2x, 'Grids and array dimensions:'  )" )
       WRITE( stdout, " (4x, 'FFT grid ( ', 3i3, ' )' )" )  nr1, nr2, nr3
       WRITE( stdout, " (4x, 'max dimension of hamiltonian rows = ', i7 )" )  mxddim
       WRITE( stdout, " (4x, 'max number of G-vectors = ', i7 )" )  ngm


       WRITE( stdout, "(2/,2x,70('='))" )
       WRITE( stdout,"('  =',10x,'Input parameters for Wannier func. calculation',9x,'=')")
       WRITE( stdout, "(2x,70('='))" )
       WRITE( stdout," (/,2x, 'Definition of energy windows: (energies in eV)' ) " )
       WRITE( stdout, " (4x, 'outer window: E  = (', f8.4, ' , ',f8.4, ' )' )" ) &
                              win_min, win_max
       IF ( froz_max < win_min  .OR. froz_min > win_max ) THEN
         WRITE(stdout," (4x,'inner window: NOT used --> NO FROZEN STATES' )" )
       ELSE
         WRITE(stdout," (4x,'inner window: E  = (', f8.4, ' , ',f8.4, &
                      & ' ) --> FROZEN STATES' )" ) froz_min, froz_max
       END IF

       WRITE(stdout, " (/,2x,'Number of Wannier functions required = ', i4 )" ) dimwann
       WRITE(stdout, " (2x,'Number of k-point shells = ', i3 )" ) nshells

       WRITE( stdout, "(2/,2x,70('='))" )
       WRITE( stdout, "('  =',25x,'Band structure',29x,'=')" )
       WRITE( stdout, "(2x,70('='))" )
 
 
!=----------------------------------------------------------------------------=!
  
! ...  Open the output file DFT_DATA (UNIT 19) 
!      that will be read by all further programs of the chain
 
       CALL ioname('dft_data',FILENAME=filename)
       OPEN ( UNIT=19, FILE=TRIM(filename), STATUS='UNKNOWN', FORM='UNFORMATTED' )

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

!
! ...  Write G grid data
!      position in the DFT_DATA file changed by ANDREA, 23 Nov 2004

       WRITE(19) ngm0
       WRITE(19)( ig1(ig), ig2(ig), ig3(ig), ig = 1, ngm0 )



! ...  Start K-loop
 
       nkp = 0
       loop_x: DO i1 = 0, nk(1)-1
       loop_y: DO i2 = 0, nk(2)-1
       loop_z: DO i3 = 0, nk(3)-1

         nkp = nkp + 1
         neig = neig_k(nkp)
         npw = npwk(nkp)

! ...    Check which eigenvalues fall within the outer energy window
 
         IF ( (har * ei_k(1,nkp) > WIN_MAX ) .OR. ( har * ei_k(neig,nkp) < win_min ) ) &
           CALL errore(' window ', ' energy window contains no eigenvalues ', 1 )

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
         WRITE(stdout, " (/,2x,'kpt =', i3, ' ( ',3f6.3,' )    dimwin = ', i4, &
                           & '   npw = ', i7 )" )  nkp, dble(I1)/dble(NK(1)), &
                           dble(I2)/dble(NK(2)), dble(I3)/dble(NK(3)), kdimwin, npw
         WRITE(stdout, " (37x,'  imin = ', i4, '   imax = ', i4)" ) imin, imax
         WRITE(stdout, "(' Eigenvalues:')"  ) 
         WRITE(stdout,'(8f9.4)') ( har * ei_k(i,nkp), i=1,neig )

         IF ( kdimwin < dimwann ) CALL errore(' window ', ' dimwin < dimwann ', 1 )
         IF ( imax < imin ) CALL errore(' window ', ' imax < imin ' , ABS(imin-imax) )
         IF ( imin < 1 )    CALL errore(' window ', ' imin < 1    ' , ABS(imin)+1 )

!
! ...    Last change by carlo, first all dimensions, then all k-dependent vectors
         WRITE(19) npw, imin, imax, imax-imin+1 

       END DO loop_z
       END DO loop_y
       END DO loop_x

!
! ...  Stdout writing
       WRITE( stdout, "(2/,2x,70('='))" )
       WRITE( stdout, "(' =',23x,'FROZEN states analysis',23x,'=')" ) 
       WRITE( stdout, "(2x,70('='),/)" )
       IF ( froz_max < win_min  .OR. froz_min > win_max ) THEN
         WRITE( stdout, fmt= " ( 2x,'No frozen states ',/ ) " )
       ELSE
         WRITE( stdout, fmt= " ( 2x,'Counting frozen states: ',/ ) " ) 
       END IF


!
! ...  Start K-loop again for wfcs and eigs
 
       nkp = 0
       loop_xx: DO i1 = 0, nk(1)-1
       loop_yy: DO i2 = 0, nk(2)-1
       loop_zz: DO i3 = 0, nk(3)-1

         nkp = nkp + 1
         neig = neig_k(nkp)
         npw = npwk(nkp)

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

         WRITE(19) ( isort_k(j,nkp), j = 1, npw )
         WRITE(19) ( ei_k(j,nkp), j = imin, imax )
         WRITE(19) ( ( zvec_k(j,i,nkp), j = 1, npw ), i = imin, imax )

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
         IF ( dimfroz(nkp) > dimwann ) CALL errore('window','dimfroz(nkp) > dimwann ',nkp)


! ...    Generate index array for frozen states inside inner window

         IF ( dimfroz(nkp) > 0 ) THEN
           DO i = 1, dimfroz(nkp)
             indxfroz(i,nkp) = kifroz_min + i - 1
             frozen(indxfroz(i,nkp),nkp) = .TRUE.
           END DO
           IF ( indxfroz(dimfroz(nkp),nkp) /= kifroz_max ) &
               CALL errore('window','wrong number of frozen states',ABS(indxfroz(i,nkp))+1 )
         END IF


! ...    Generate index array for non-frozen states

         i = 0
         DO j = 1, kdimwin
           IF( frozen(j,nkp) .EQV. .false. ) THEN
             i = i + 1
             indxnfroz(i,nkp) = j
           END IF
         END DO
         IF ( i /= kdimwin-dimfroz(nkp) )  &
           CALL errore(' window ', ' wrong number of non-frozen states ', i )

         WRITE(19) dimfroz(nkp),( frozen(i,nkp), i=1,kdimwin )
         IF ( dimfroz(nkp) > 0 ) THEN
           WRITE( stdout, fmt= " ( 4x, 'there are ', i3, ' frozen states at k-point = ', i5 ) " ) &
                                 dimfroz(nkp), nkp
           WRITE(19) ( indxfroz(i,nkp), i=1,dimfroz(nkp) )
         END IF

         IF ( dimfroz(nkp) < kdimwin ) &
           WRITE(19) ( indxnfroz(i,nkp), i=1,kdimwin-dimfroz(nkp) )

       END DO loop_zz
       END DO loop_yy
       END DO loop_xx

       WRITE( stdout, * ) ' ' 
       WRITE( stdout, * ) ' ' 
       WRITE( stdout, * ) ' ======================================================================'
       WRITE( stdout, * ) ' =                            END OF WINDOW                           ='
       WRITE( stdout, * ) ' ======================================================================'
       WRITE( stdout, * ) ' ' 

       CLOSE(19)
!
       DEALLOCATE( isort_k, STAT=ierr )
           IF( ierr /=0 ) CALL errore(' window ', ' deallocating isort_k ', ABS(ierr) )
       DEALLOCATE( zvec_k, STAT=ierr )
           IF( ierr /=0 ) CALL errore(' window ', ' deallocating zvec_k ', ABS(ierr) )
       DEALLOCATE( ei_k, STAT=ierr )
           IF( ierr /=0 ) CALL errore(' window ', ' deallocating ei_k ', ABS(ierr) )
       DEALLOCATE( npwk, STAT=ierr )
           IF( ierr /=0 ) CALL errore(' window ', ' deallocating npwk ', ABS(ierr) )
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
       CALL timing_overview(stdout,LIST=global_list,MAIN_NAME='window')
       CALL timing_deallocate()
!
       STOP '*** THE END *** (window.x)'
       END

!=----------------------------------------------------------------------------=!
