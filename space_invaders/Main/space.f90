       PROGRAM space

       USE kinds
       USE constants, ONLY: ryd => ry, har => au, bohr => bohr_radius_angs
       USE parameters, ONLY: mxdtyp => npsx, mxdatm => natx
       USE mp, ONLY: mp_start, mp_end, mp_env
       USE mp_global, ONLY: mp_global_start
       USE io_global, ONLY: io_global_start, io_global_getionode


       IMPLICIT NONE

       INTEGER :: mxdbnd, mxddim
       INTEGER :: nshells
       INTEGER :: mxdgve

       INTEGER, PARAMETER :: mxdnn = 72
       INTEGER, PARAMETER :: mxdnnh = mxdnn/2

       REAL(dbl) :: vcell, avec(3,3), bvec(3,3), aminv(3)
       REAL(dbl) :: adot(3,3), bdot(3,3)

       REAL(dbl) :: rat(3,mxdatm,mxdtyp), atmass(mxdtyp)
       INTEGER   :: ntype, natom(mxdtyp)
       CHARACTER(LEN=2) :: nameat(mxdtyp)
 
       INTEGER :: nkpts, nk(3)
       INTEGER, ALLOCATABLE :: kgv(:,:)
       INTEGER, ALLOCATABLE :: isort(:,:)
       INTEGER, ALLOCATABLE :: mtxd(:)
       REAL(dbl), ALLOCATABLE :: vkpt(:,:)
       REAL(dbl), ALLOCATABLE :: evecr(:,:,:)
       REAL(dbl), ALLOCATABLE :: eveci(:,:,:)
       REAL(dbl), ALLOCATABLE :: eiw(:,:)
       REAL(dbl) :: s(3)
       REAL(dbl) :: emax
 
       INTEGER, ALLOCATABLE :: nwhich(:)
       INTEGER, ALLOCATABLE :: nnshell(:,:)
       INTEGER, ALLOCATABLE :: nnlist(:,:)
       INTEGER, ALLOCATABLE :: nncell(:,:,:)
       INTEGER, ALLOCATABLE :: nntot(:)
       INTEGER, ALLOCATABLE :: neigh(:,:)
       REAL(dbl), ALLOCATABLE :: bk(:,:,:)
       REAL(dbl), ALLOCATABLE :: wb(:,:)
       REAL(dbl), ALLOCATABLE :: dnn(:)
       REAL(dbl), ALLOCATABLE :: bka(:,:)
       REAL(dbl) :: wbtot
       COMPLEX(dbl), ALLOCATABLE :: cm(:,:,:,:)
 
       EXTERNAL komegai

       REAL(dbl) :: klambda, alpha
       REAL(dbl) :: omega_i, omega_i_est, komegai
       REAL(dbl) :: o_error
       REAL(dbl), ALLOCATABLE :: komega_i_est(:)
       COMPLEX(dbl), ALLOCATABLE :: lamp(:,:,:)
       COMPLEX(dbl), ALLOCATABLE :: camp(:,:,:)
       COMPLEX(dbl), ALLOCATABLE :: eamp(:,:,:)
       COMPLEX(dbl), ALLOCATABLE :: mtrx_in(:,:,:)
       COMPLEX(dbl), ALLOCATABLE :: mtrx_out(:,:,:)
       INTEGER, ALLOCATABLE :: dimwin(:)

! ...  Next 2 lines added by ANDREA (28 jan 2004) 
       INTEGER, ALLOCATABLE :: imin(:)
       INTEGER, ALLOCATABLE :: imax(:)

       INTEGER, ALLOCATABLE :: dimfroz(:)
       INTEGER, ALLOCATABLE :: indxfroz(:,:)
       INTEGER, ALLOCATABLE :: indxnfroz(:,:)
       INTEGER :: froz_flag, maxiter 
       INTEGER :: itrial, cflag
       INTEGER :: dimwann
       LOGICAL, ALLOCATABLE :: frozen(:,:)

       INTEGER :: iphase
       REAL(dbl)  :: alphafix0, alphafix
       INTEGER :: niter, niter0, ncg
 
       COMPLEX(dbl), ALLOCATABLE :: ap(:)
       COMPLEX(dbl), ALLOCATABLE :: z(:,:)
       COMPLEX(dbl), ALLOCATABLE :: work(:)
       REAL(dbl), ALLOCATABLE :: w(:)
       REAL(dbl), ALLOCATABLE :: rwork(:)
       INTEGER, ALLOCATABLE :: ifail(:)
       INTEGER, ALLOCATABLE :: iwork(:)
       INTEGER :: info, m
 
       COMPLEX(dbl), ALLOCATABLE :: ham(:,:,:)
 
       INTEGER :: i, j, l, i1, i2, i3 
       INTEGER :: nkp, iter

       EXTERNAL lambda_avg

       REAL(dbl) :: recc(3,3), aux
       REAL(dbl) :: lambda_avg, alatt
       REAL(dbl) :: win_min, win_max
       REAL(dbl) :: froz_min, froz_max

       COMPLEX(dbl) :: ctmp


       REAL(dbl) :: zero, sgn, um
       COMPLEX(dbl) :: czero
       PARAMETER ( zero = 0.0d0, um = 1.0d0 )
       PARAMETER ( czero = ( 0.0d0, 0.0d0 ) )

       INTEGER :: nt, ja, nbandi
       INTEGER :: ngx, ngy, ngz
       INTEGER :: ngm
       
       INTEGER, ALLOCATABLE :: gauss_typ(:)
       INTEGER, ALLOCATABLE :: l_wann(:)
       INTEGER, ALLOCATABLE :: m_wann(:)
       INTEGER, ALLOCATABLE :: ndir_wann(:)
       REAL(dbl), ALLOCATABLE :: rphiimx1(:,:)
       REAL(dbl), ALLOCATABLE :: rphiimx2(:,:)
       REAL(dbl), ALLOCATABLE :: rloc(:)
       INTEGER :: nwann
       CHARACTER(LEN=6) :: verbosity = 'none' ! none, low, medium, high

       INTEGER :: root, mpime, gid, nproc
       LOGICAL :: ionode
       INTEGER :: ionode_id
       INTEGER :: ierr
       INTEGER :: ndwinx

!      
! ...  End declarations and dimensions
!      

       root = 0
       CALL mp_start()
       CALL mp_env( nproc, mpime, gid )
       CALL mp_global_start( root, mpime, gid, nproc )

! ... mpime = processor number, starting from 0
! ... nproc = number of processors
! ... gid   = group index
! ... root  = index of the root processor

! ... initialize input output

       CALL io_global_start( mpime, root )
       CALL io_global_getionode( ionode, ionode_id )

!=----------------------------------------------------------------------------=!
!      
! ...  Read input parameters from takeoff.dat
!

       WRITE(*,*)
       WRITE(*,*) ' Starting Space '

       OPEN( UNIT=19, FILE='takeoff.dat', STATUS='OLD', FORM='UNFORMATTED' )
!
       READ(19) alatt
       READ(19) ( avec(i,1), i=1,3 )
       READ(19) ( avec(i,2), i=1,3 )
       READ(19) ( avec(i,3), i=1,3 )
       READ(19) ntype
       DO nt=1,ntype
         READ(19) natom(nt),nameat(nt)
         DO ja=1, natom(nt)
           READ(19) ( rat(i,ja,nt), i=1,3 )
         END DO
       END DO
       READ(19) emax, nbandi
       READ(19) ( nk(i), i=1,3 ), ( s(i), i=1,3 )
       READ(19) win_min, win_max, froz_min, froz_max, dimwann

       ALLOCATE( gauss_typ(dimwann) )
       ALLOCATE( rphiimx1(3,dimwann) )
       ALLOCATE( rphiimx2(3,dimwann) )
       ALLOCATE( l_wann(dimwann) )
       ALLOCATE( m_wann(dimwann) )
       ALLOCATE( ndir_wann(dimwann) )
       ALLOCATE( rloc(dimwann) )

       READ(19) alpha, maxiter 
       READ(19) iphase
       READ(19) niter0, alphafix0
       READ(19) niter, alphafix, ncg
       READ(19) itrial, nshells

       ALLOCATE( nwhich(nshells) )
       READ(19) ( nwhich(i), i=1,nshells )
       READ(19) nkpts, mxddim, mxdbnd
       READ(19) ngx, ngy, ngz, ngm
      
       READ(19) gauss_typ(1:dimwann)
       READ(19) rphiimx1(1:3,1:dimwann)
       READ(19) rphiimx2(1:3,1:dimwann)
       READ(19) l_wann(1:dimwann)
       READ(19) m_wann(1:dimwann)
       READ(19) ndir_wann(1:dimwann)
       READ(19) rloc(1:dimwann)

       IF ( dimwann > 80 .OR. nkpts > 99999) THEN
         WRITE(*,*) '*** ERROR ***'
         WRITE(*,*) 'MODIFY FORMAT 102 WRITING FILE energy.dat (to be used in bands.f)'
         STOP '*** ERROR ***'
       END IF

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
         WRITE(6,202) j,( avec(i,j), i=1,3 ), ( avec(i,j)/alatt, i=1,3 )
       END DO
 202   FORMAT( '  A',i1,'=',3(2x,E11.5), 5x, 3(2x,F7.3) )

       WRITE(6,120) nshells
 120   FORMAT( 'NUMBER OF SHELLS: NSHELLS=',i2,/ )

!
! ...  Get crystal data
 
       CALL latti( avec, bvec, vcell, bdot, aminv, adot )
       WRITE(6,*) ' '
       WRITE(6, fmt="('ENERGY CUTOFF (Hartree) = ',F12.4)") emax
       WRITE(6,*) ' '

! 
! ...  Write energy windows and band-space minimization parameters

       WRITE(6,121) dimwann
 121   FORMAT(/,'DIMENSION OF SUBSPACE: DIMWANN=',I2)
       WRITE(6,122) alpha
 122   FORMAT(/,'MIXING PARAMETER: ALPHA =',F6.3,/)

!
! ...  Read grid information, and G-vectors 
 
       READ( 19 ) mxdgve 
       ALLOCATE( kgv(3,mxdgve) )
       READ( 19 ) ( ( kgv(i,j), i=1,3 ), j=1, mxdgve )
!
! ...  Calculate grid of K-pointS
 
       nkp = 0
       ALLOCATE( vkpt(3,nkpts) )
       DO i1 = 0, nk(1)-1
         DO i2 = 0, nk(2)-1
           DO i3 = 0, nk(3)-1
             nkp = nkp+1
             vkpt(1,nkp) = DBLE(i1)/DBLE(nk(1)) + s(1)
             vkpt(2,nkp) = DBLE(i2)/DBLE(nk(2)) + s(2)
             vkpt(3,nkp) = DBLE(i3)/DBLE(nk(3)) + s(3)
           ENDDO
         ENDDO
       ENDDO
       nkpts = nkp

       ALLOCATE( ap((mxdbnd*(mxdbnd+1))/2), STAT = ierr )
       IF( ierr /=0 ) THEN
         CALL errore(' space ', ' allocateing ap ', ((mxdbnd*(mxdbnd+1))/2) )
       END IF
       ALLOCATE( z(mxdbnd,mxdbnd), STAT = ierr )
       IF( ierr /=0 ) THEN
         CALL errore(' space ', ' allocateing z ', (mxdbnd*mxdbnd) )
       END IF
       ALLOCATE( work(2*mxdbnd), STAT = ierr )
       IF( ierr /=0 ) THEN
         CALL errore(' space ', ' allocateing work ', 2*mxdbnd )
       END IF
       ALLOCATE( w(mxdbnd), STAT = ierr )
       IF( ierr /=0 ) THEN
         CALL errore(' space ', ' allocateing w ', mxdbnd )
       END IF
       ALLOCATE( rwork(7*mxdbnd), STAT = ierr )
       IF( ierr /=0 ) THEN
         CALL errore(' space ', ' allocateing rwork ', 7*mxdbnd )
       END IF
       ALLOCATE( ifail(mxdbnd), STAT = ierr )
       IF( ierr /=0 ) THEN
         CALL errore(' space ', ' allocateing ifail ', mxdbnd )
       END IF
       ALLOCATE( iwork(5*mxdbnd), STAT = ierr )
       IF( ierr /=0 ) THEN
         CALL errore(' space ', ' allocateing iwork ', 5*mxdbnd )
       END IF
       ALLOCATE( isort(mxddim,nkpts), STAT = ierr )
       IF( ierr /=0 ) THEN
         CALL errore(' space ', ' allocateing isort ', mxddim*nkpts )
       END IF
       ALLOCATE( mtxd(nkpts), STAT = ierr )
       IF( ierr /=0 ) THEN
         CALL errore(' space ', ' allocateing mtxd ', nkpts )
       END IF

       ALLOCATE( eiw(mxdbnd,nkpts), STAT = ierr )
       IF( ierr /=0 ) THEN
         CALL errore(' space ', ' allocateing eiw ', mxdbnd*nkpts )
       END IF

       ALLOCATE( nnshell(nkpts,mxdnn), STAT = ierr )
       IF( ierr /=0 ) THEN
         CALL errore(' space ', ' allocateing nnshell ', nkpts*mxdnn )
       END IF
       ALLOCATE( nnlist(nkpts,mxdnn), STAT = ierr )
       IF( ierr /=0 ) THEN
         CALL errore(' space ', ' allocateing nnlist ', nkpts*mxdnn )
       END IF
       ALLOCATE( nncell(3,nkpts,mxdnn), STAT = ierr )
       IF( ierr /=0 ) THEN
         CALL errore(' space ', ' allocateing nncell ', 3*nkpts*mxdnn )
       END IF
       ALLOCATE( nntot(nkpts), STAT = ierr )
       IF( ierr /=0 ) THEN
         CALL errore(' space ', ' allocateing nntot ', nkpts )
       END IF
       ALLOCATE( neigh(nkpts,mxdnnh), STAT = ierr )
       IF( ierr /=0 ) THEN
         CALL errore(' space ', ' allocateing neigh ', nkpts*mxdnnh )
       END IF
       ALLOCATE( bk(3,nkpts,mxdnn), STAT = ierr ) 
       IF( ierr /=0 ) THEN
         CALL errore(' space ', ' allocateing bk ', 3*nkpts*mxdnn )
       END IF
       ALLOCATE( dnn(mxdnn), STAT = ierr ) 
       IF( ierr /=0 ) THEN
         CALL errore(' space ', ' allocateing dnn ', mxdnn )
       END IF
       ALLOCATE( wb(nkpts,mxdnn), STAT = ierr )
       IF( ierr /=0 ) THEN
         CALL errore(' space ', ' allocateing wb ', nkpts*mxdnn )
       END IF
       ALLOCATE( bka(3,mxdnnh), STAT = ierr )
       IF( ierr /=0 ) THEN
         CALL errore(' space ', ' allocateing bka ', 3*mxdnnh )
       END IF
       ALLOCATE( cm(mxdbnd,mxdbnd,mxdnn,nkpts), STAT = ierr )
       IF( ierr /=0 ) THEN
         CALL errore(' space ', ' allocateing cm ', mxdbnd*mxdbnd*mxdnn*nkpts )
       END IF

       ALLOCATE( komega_i_est(nkpts), STAT = ierr )
       IF( ierr /=0 ) THEN
         CALL errore(' space ', ' allocateing komega_i_est ', nkpts )
       END IF
       ALLOCATE( lamp(mxdbnd,mxdbnd,nkpts), STAT = ierr )
       IF( ierr /=0 ) THEN
         CALL errore(' space ', ' allocateing lamp ', mxdbnd*mxdbnd*nkpts )
       END IF
       ALLOCATE( camp(mxdbnd,mxdbnd,nkpts), STAT = ierr )
       IF( ierr /=0 ) THEN
         CALL errore(' space ', ' allocateing camp ', mxdbnd*mxdbnd*nkpts )
       END IF
       ALLOCATE( eamp(mxdbnd,mxdbnd,nkpts), STAT = ierr )
       IF( ierr /=0 ) THEN
         CALL errore(' space ', ' allocateing eamp ', mxdbnd*mxdbnd*nkpts )
       END IF
       ALLOCATE( mtrx_in(mxdbnd,mxdbnd,nkpts), STAT = ierr )
       IF( ierr /=0 ) THEN
         CALL errore(' space ', ' allocateing mtrx_in ', mxdbnd*mxdbnd*nkpts )
       END IF
       ALLOCATE( mtrx_out(mxdbnd,mxdbnd,nkpts), STAT = ierr )
       IF( ierr /=0 ) THEN
         CALL errore(' space ', ' allocateing mtrx_out ', mxdbnd*mxdbnd*nkpts )
       END IF
       ALLOCATE( dimwin(nkpts), STAT = ierr )
       IF( ierr /=0 ) THEN
         CALL errore(' space ', ' allocateing dimwin ', nkpts )
       END IF
       ALLOCATE( dimfroz(nkpts), STAT = ierr )
       IF( ierr /=0 ) THEN
         CALL errore(' space ', ' allocateing dimfroz ', nkpts )
       END IF
       ALLOCATE( indxfroz(mxdbnd,nkpts), STAT = ierr )
       IF( ierr /=0 ) THEN
         CALL errore(' space ', ' allocateing indxfroz ', mxdbnd*nkpts )
       END IF
       ALLOCATE( indxnfroz(mxdbnd,nkpts), STAT = ierr )
       IF( ierr /=0 ) THEN
         CALL errore(' space ', ' allocateing indxnfroz ', mxdbnd*nkpts )
       END IF
       ALLOCATE( frozen(mxdbnd,nkpts), STAT = ierr )
       IF( ierr /=0 ) THEN
         CALL errore(' space ', ' allocateing frozen ', mxdbnd*nkpts )
       END IF
       ALLOCATE( ham(mxdbnd,mxdbnd,nkpts), STAT = ierr )
       IF( ierr /=0 ) THEN
         CALL errore(' space ', ' allocateing ham ', mxdbnd*mxdbnd*nkpts )
       END IF
!
! ...  Next line added by ANDREA (28 jan 2004) 
       ALLOCATE( imin(nkpts), imax(nkpts) )

!
! ...  Read wave functions and energy eigenvalues defining the "window space"

       froz_flag = 0


       DO nkp = 1, nkpts
!
!        Read all dimensions first, then k-dependent arrais
!
         READ(19) mtxd(nkp), imin(nkp), imax(nkp), dimwin(nkp)

         WRITE(6,125) nkp, dimwin(nkp), mtxd(nkp)
 125     FORMAT('k-point',i5,2x,'dimwin ',i2,' ngwk ',i5)

         IF ( dimwin(nkp) < dimwann ) THEN
           WRITE(6,*) '*** ERROR *** AT K-POINT ',nkp,' DIMWIN= ', dimwin(nkp), '< DIMWANN=', dimwann
           STOP
         END IF

         IF ( dimwin(nkp) > mxdbnd ) THEN
           WRITE(6,*) '*** ERROR *** INCREASE MXDBND TO AT LEAST ', dimwin(nkp)
           STOP
         END IF

       END DO

       ndwinx = MAXVAL( dimwin(1:nkpts) )

       WRITE(*,*) ' Number of bands               (mxdbnd) : ', mxdbnd
       WRITE(*,*) ' Number of bands within window (ndwinx) : ', ndwinx

       ALLOCATE( evecr(mxddim,ndwinx,nkpts), STAT = ierr )
       IF( ierr /=0 ) THEN
         CALL errore(' space ', ' allocateing evecr ', mxddim*mxdbnd*nkpts )
       END IF
       ALLOCATE( eveci(mxddim,ndwinx,nkpts), STAT = ierr )
       IF( ierr /=0 ) THEN
         CALL errore(' space ', ' allocateing eveci ', mxddim*mxdbnd*nkpts )
       END IF

       DO nkp = 1, nkpts

         READ(19) ( isort(j,nkp), j=1, mtxd(nkp) ) 
         READ(19) ( eiw(j,nkp), j=1, dimwin(nkp) )
         READ(19) ( ( evecr(j,i,nkp), j=1, mtxd(nkp) ), i=1, dimwin(nkp) )
         READ(19) ( ( eveci(j,i,nkp), j=1, mtxd(nkp) ), i=1, dimwin(nkp) )
         READ(19) dimfroz(nkp), ( frozen(i,nkp), i=1, dimwin(nkp) )

         IF ( dimfroz(nkp) > 0 )  READ(19) ( indxfroz(i,nkp), i=1, dimfroz(nkp) )

         IF ( dimfroz(nkp) < dimwin(nkp) )  READ(19) ( indxnfroz(i,nkp), i=1, dimwin(nkp)-dimfroz(nkp) )

         IF ( dimfroz(nkp) /= 0 ) froz_flag = 1

       END DO  ! nkp

       CLOSE(19)


!
! ...  Setup the shells of b-vectors around each K-point
!      NOTE: transpose bvec (opposite convention between cpw and castep...)
 
       DO i = 1, 3
         DO j = 1, 3
           recc(i,j) = bvec(j,i)
         END DO
       END DO
       CALL bshells( vkpt, nkpts, recc, nshells, nwhich, nnshell, bk,       &
            dnn, wb, wbtot, nnlist, nncell, nntot, bka, neigh, nkpts )

!
! ...  Compute the overlap matrix cm between each K-point and its shell of neighbors

       call overlap( kgv, vkpt, avec, evecr, eveci, isort, mtxd, dimwin,    &
            nntot, nnlist, nncell, cm, emax, mxdgve, mxddim, nkpts,        &
            mxdnn, mxdbnd, ngx, ngy, ngz, mxddim, nkpts, ndwinx )
!
!=----------------------------------------------------------------------------------------------=
!
! ...  Start iteration loop

       cflag = 0
       DO iter = 1, maxiter
         WRITE(*,*) ' '
         WRITE(*,*) '***************'
         WRITE(*,'(a10,i5)') ' Iteration ', iter
         WRITE(*,*) '***************'
         WRITE(*,*) ' '
         IF ( iter == 1 ) THEN

! ...    Choose an initial trial subspace at each K

           IF ( froz_flag == 0 ) THEN

! ...      No frozen states

             IF ( ITRIAL == 1 ) THEN
               WRITE(6,*) ' '
               WRITE(6,*) 'INITIAL TRIAL SUBSPACE: LOWEST ENERGY EIGENVECTORS'
               WRITE(6,*) ' '
               DO nkp=1, nkpts
                 DO l=1, dimwann
                   DO j=1,dimwin(nkp)
                     lamp(j,l,nkp) = czero
                     IF ( j == l ) lamp(j,l,nkp) = cmplx(1.0d0,0.0d0)
                   END DO
                 END DO
               END DO
             ELSE IF ( itrial == 2 ) THEN
               WRITE(6,*) ' '
               WRITE(6,*) 'INITIAL TRIAL SUBSPACE: HIGHEST ENERGY EIGENVECTORS'
               WRITE(6,*) ' '
               DO nkp=1, nkpts
                 DO l=1, dimwann
                   DO j=1, dimwin(nkp)  
                     lamp(j,l,nkp) = czero
                     IF ( j == l+dimwin(nkp)-dimwann ) lamp(j,l,nkp) = cmplx(1.0d0,0.0d0)
                   END DO
                 END DO
               END DO
             ELSE IF ( ITRIAL == 3 ) THEN
               WRITE(6,*) ' ' 
               WRITE(6,*) 'INITIAL TRIAL SUBSPACE: PROJECTED LOCALIZED ORBITALS'
               WRITE(6,*) ' '
               CALL projection( avec, lamp, evecr, eveci, vkpt,             &
                    kgv, isort, mtxd, dimwin, dimwann, dimfroz,             &
                    mxddim, mxdbnd, nkpts, mxdgve, ngx, ngy, ngz, nkpts,   &
                    gauss_typ, rphiimx1, rphiimx2, l_wann,                  &
                    m_wann, ndir_wann, rloc, ndwinx)
             ELSE
               WRITE(6,*) ' ' 
               WRITE(6,*) 'INVALID CHOICE OF ITRIAL:',ITRIAL
               STOP
             END IF     !   No frozen states

           ELSE

! ...      There are frozen states. 
!          Choose the non-frozen trial states using the modified projection technique

             WRITE(*,*) ' '
             WRITE(*,*) 'THERE ARE FROZEN STATES'
             WRITE(*,*) ' '
             WRITE(*,*) 'INITIAL TRIAL SUBSPACE: PROJECTED GAUSSIANS+FROZEN STATES'
             WRITE(*,*) ' '

             DO nkp = 1, nkpts
               IF ( dimfroz(nkp) == 0 ) THEN
                 WRITE(*,'( a7, 1x, i4, a23 )') 'k-point',nkp, ' frozen bands:     none'
               ELSE
                 WRITE(*,'( a7, 1x, i4, a1, i2, a15, 1x, 20(i2,1x) )')          &
                      'k-point', nkp,':', dimfroz(nkp), ' frozen bands: ',      &
                      ( indxfroz(i,nkp), i=1, dimfroz(nkp) )
               END IF
             END DO

! ...        First find the dimwmann-dimensional subspace s with maximal overlap onto the
!            dimwann gaussians

             IF ( ITRIAL == 3 ) THEN
               CALL projection( avec, lamp, evecr, eveci, vkpt,              &
                    kgv, isort, mtxd, dimwin, dimwann, dimfroz,              &
                    mxddim, mxdbnd, nkpts, mxdgve, ngx, ngy, ngz, nkpts,    &
                    gauss_typ, rphiimx1, rphiimx2, l_wann,                   &
                    m_wann, ndir_wann, rloc, ndwinx )
             ELSE
               WRITE(6,*) ' ' 
               WRITE(6,*) 'INVALID CHOICE OF ITRIAL WITH FROZEN STATES:', itrial
               STOP
             END IF

! ...        Next find the (dimwann-dimfroz(nkp))-dimensional space of non-frozen states
!            with largest overlap with s, and include it in the trial subspace 
!           (put them above the frozen states in lamp)

             CALL projection_frozen( lamp, dimwann, dimwin,     &
                  dimfroz, frozen, nkpts, mxdbnd, nkpts)

! ...        Finally include the frozen states (if any) in the trial 
!            subspace at each k-point (put them at the bottom of lamp)
!            NOTE that this must come last, because calling the subroutine projection.f
!            would override it
!
             DO nkp = 1, nkpts
               IF ( dimfroz(nkp) > 0 ) THEN
                 DO l = 1, dimfroz(nkp)
                   DO j = 1, dimwin(nkp)
                     lamp(j,l,nkp)=czero
                   END DO
                   lamp(indxfroz(l,nkp),l,nkp) = cmplx(1.0d0,0.0d0)
                 END DO
               END IF
             END DO

! ...        Check that the states in the columns of the final matrix lamp are orthonormal
!            at every k-point (i.e., that the matrix is unitary in the sense that
!            conjg(lamp) . lamp = 1 - but not lamp . conjg(lamp) = 1 - )
!            In particular, this checks whether the projected gaussians are indeed 
!            orthogonal to the frozen states, at those k-points where both are present in
!            the trial subspace.

             DO nkp = 1, nkpts
               WRITE(*,*) ' '
               WRITE(*,'(a8,i4)') 'k-point ', nkp
               DO l = 1, dimwann
                 DO m = 1, l
                   ctmp = czero
                   DO j = 1, dimwin(nkp)
                     ctmp = ctmp + CONJG(lamp(j,m,nkp)) * lamp(j,l,nkp)
                   END DO
                   WRITE(*,'(i2, 2x, i2, f16.12, 1x, f16.12)') l, m, ctmp
                   IF ( l == m ) THEN
                     IF ( ABS(ctmp-cmplx(1.0d0,0.0d0)) > 1.0e-8 ) THEN
                       WRITE(*,*) '*** ERROR *** WITH TRIAL SUBSPACE'
                       WRITE(*,*) 'VECTORS IN LAMP NOT ORTHONORMAL'
                       WRITE(*,'(a11,i4)') 'AT K-POINT ', nkp
                       STOP
                     END IF
                   ELSE
                     IF ( ABS(ctmp) > 1.0e-8 ) THEN
                       WRITE(*,*) '*** ERROR *** WITH TRIAL SUBSPACE'
                       WRITE(*,*) 'VECTORS IN LAMP NOT ORTHONORMAL'
                       WRITE(*,'(A11,I4)') 'AT K-POINT ', nkp
                       STOP
                     END IF
                   END IF
                 END DO
               END DO
             END DO
                   
           ENDIF ! there are frozen states

! ...      Compute the initial z matrix mtrx_in at all relevant K-points

           DO nkp = 1, nkpts
             IF ( dimwann > dimfroz(nkp) )  THEN
               CALL zmatrix( nkp, nnlist, nshells, nnshell, wb, lamp,     &
                    cm(1,1,1,nkp), mtrx_in(1,1,nkp), dimwann, dimwin,     &
                    dimfroz, indxnfroz, mxdbnd, nkpts, mxdnn )
             END IF
           END DO
!
         ELSE     !   iter .ne. 1

! ...    Compute the current z-matrix at each relevant K-point using the mixing scheme
 
           DO nkp = 1, nkpts
             IF ( dimwann > dimfroz(nkp) )  THEN
               DO i = 1, dimwin(nkp)-dimfroz(nkp)
                 DO j = 1, i
                   mtrx_in(j,i,nkp) = alpha*mtrx_out(j,i,nkp) + (um-alpha)*mtrx_in(j,i,nkp)
                   mtrx_in(i,j,nkp) = conjg(mtrx_in(j,i,nkp))         ! hermiticity
                 END DO
               END DO
             END IF
           END DO
         ENDIF    !   iter = 1
!
         omega_i_est = zero

         DO nkp = 1, nkpts
 
! ...    Diagonalize z matrix mtrx_in at all relevant K-points
 
           IF ( dimwann > dimfroz(nkp) )  THEN
             DO j = 1, dimwin(nkp)-dimfroz(nkp)
               DO i = 1, j
                 ap(i + ( (j-1)*j)/2 ) = mtrx_in(i,j,nkp)
               END DO
             END DO

             CALL zhpevx( 'v', 'a', 'u', dimwin(nkp)-dimfroz(nkp), ap(1),             &
                  zero, zero, 0, 0, -um, m, w(1), z(1,1), mxdbnd, work(1), rwork(1),  &
                  iwork(1), ifail(1), info )

             IF ( info < 0 ) THEN
               WRITE(6,*) '*** ERROR *** ZHPEVX WHILE DIAGONALIZING Z MATRIX'
               WRITE(6,*) 'THE ', -info, ' ARGUMENT OF ZHPEVX HAD AN ILLEGAL VALUE'
               STOP
             END IF
             IF ( info > 0 ) THEN
               WRITE(6,*) '*** ERROR *** ZHPEVX WHILE DIAGONALIZING Z MATRIX'
               WRITE(6,*) 'INFO= ', info, 'EIGENVECTORS FAILED TO CONVERGE'
               STOP
             END IF
           END IF
 
! ...      Calculate K-point contribution to omega_i_est
 
           komega_i_est(nkp) = DBLE(dimwann)*wbtot
 
! ...      Contribution from frozen states (if any)
 
           IF ( dimfroz(nkp) > 0 )  THEN
             DO m = 1, dimfroz(nkp)
 
! ...          Note that at this point lamp for the non-frozen states pertains to the 
!              previous iteration step, which is exactly what we need as an input for 
!              the subroutine lambda_avg
 
               klambda = lambda_avg( m, nkp, lamp, cm(1,1,1,nkp), nnlist,               &
                         nshells, nnshell, wb, dimwann, dimwin, mxdbnd, nkpts, mxdnn )
               komega_i_est(nkp) = komega_i_est(nkp) - klambda
             END DO
           END IF
 
! ...      Contribution from non-frozen states (if any). 
!          pick the dimwann-dimfroz(nkp) leading eigenvectors of the z-matrix to build the
!          optimal subspace for the next iteration
 
           IF ( dimwann > dimfroz(nkp) )  THEN
             m = dimfroz(nkp)
             DO j = dimwin(nkp)-dimwann+1, dimwin(nkp)-dimfroz(nkp)
               m = m+1
               komega_i_est(nkp) = komega_i_est(nkp) - w(j)
               DO i = 1, dimwin(nkp)
                 lamp(i,m,nkp) = czerO
               END DO
               DO i = 1, dimwin(nkp)-dimfroz(nkp)
                 lamp(indxnfroz(i,nkp),m,nkp) = z(i,j)     ! *** CHECK!!! ***
               END DO
             END DO
             IF ( verbosity == 'high' ) THEN
                WRITE(*,*)
                WRITE(*,*) 'All eigenvalues:'
                DO j = 1, dimwin(nkp)-dimfroz(nkp)
                  WRITE(*,747) j, w(j)
 747              FORMAT('j=',i2,', lambda(j)=', f10.5)
                END DO
                WRITE(*,'(a6,f10.5)') 'WBTOT= ', wbtot
             END IF
           ENDIF
 
           OMEGA_I_EST=OMEGA_I_EST+KOMEGA_I_EST(NKP)
 
! ...      At the last iteration find a basis for the (dimwin(nkp)-dimwann)-dimensional
!          complement space
 
           IF ( iter == maxiter )  THEN
             IF ( dimwin(nkp) > dimwann )  THEN
                DO j = 1, dimwin(nkp)-dimwann
                   IF ( dimwann > dimfroz(nkp) )  THEN
 
! ...              Use the non-leading eigenvectors of the z-matrix
 
                      DO i = 1, dimwin(nkp)
                        camp(i,j,nkp) = z(i,j)
                      END DO
                   ELSE        ! dimwann=dimfroz(nkp)
 
! ...              Use the original non-frozen bloch eigenstates
 
                      DO i = 1, dimwin(nkp)
                         camp(i,j,nkp) = czero
                         IF ( i == indxnfroz(j,nkp) )  camp(i,j,nkp) = cmplx(1.0d0,0.0d0)
                      END DO
                   END IF
                END DO
               
             ELSE
                cflag = 1
                WRITE(*,*)
                WRITE(*,*) '*** WARNING ***:'
                WRITE(*,*) 'AT K-POINT ',NKP,' THE COMPLEMENT SUBSPACE HAS ZERO DIMENSIONS'
             END IF
           END IF
 
         END DO ! nkp


         omega_i_est = omega_i_est/DBLE(nkpts)

!        Compute omega_i using the updated subspaces at all K
 
         omega_i = zero
         o_error = zero
         DO nkp = 1, nkpts
           aux = komegai( nkp, lamp, cm(1,1,1,nkp),                     &
                 wb, wbtot, nnlist, nshells, nnshell, dimwann, dimwin,  &
                 mxdbnd, nkpts, mxdnn )
           omega_i = omega_i + aux
           WRITE(6,123) nkp, aux, komega_i_est(nkp), (komega_i_est(nkp)-aux)/aux
 123       FORMAT('K-POINT ',i4,' KOMEGA_I ',f16.8,' KOMEGA_I_EST ',f16.8,' ERROR ',f16.8)
         END DO
         omega_i = omega_i/DBLE(nkpts)
 
         WRITE(6,124) iter, omega_i, omega_i_est, (omega_i_est - omega_i)/omega_i
         o_error = ABS( (OMEGA_I_EST-OMEGA_I)/OMEGA_I )
 124     FORMAT(//,'ITERATION ',i4,' OMEGA_I=',f16.8,'  OMEGA_I_EST=',f16.8,'  ERROR=',f16.8,//)

 
! ...    Construct the new z-matrix mtrx_out at the relevant K-points
   
         DO nkp = 1, nkptS
           IF ( dimwann > dimfroz(nkp) )  THEN
             CALL zmatrix( nkp, nnlist, nshells, nnshell, wb, lamp,   &
                  cm(1,1,1,nkp), mtrx_out(1,1,nkp), dimwann, dimwin,  &
                  dimfroz, indxnfroz, mxdbnd, nkpts, mxdnn )
           END IF
         END DO

         IF ( o_error < 1.e-8 ) GO TO 9999

       ENDDO ! iter
!
! ...  End of iter loop
!=-------------------------------------------------------------------------------------------------=

! ...  Convergence achieved

 9999  continue
       WRITE(*,*) 'CONVERGENCE ACHIEVED'



! ...  Write the final omega_i. This should equal the one given by wannier
 
       WRITE(6,100) omega_i,omega_i*bohr**2
 100   FORMAT(/,'FINAL OMEGA_I (BOHR^2, ANGSTROM^2)', f16.8, 2x, f16.8)


 
! ...  Diagonalize the hamiltonian within the optimized subspace at each K
 
       OPEN( UNIT=7, FILE='energies.dat', STATUS='UNKNOWN', FORM='FORMATTED' )

!      nkp loop
       DO nkp = 1, nkpts
         DO j = 1, dimwann
           DO i = 1, dimwann
             ham(i,j,nkp) = czero
             DO l = 1, dimwin(nkp)
               ham(i,j,nkp) = ham(i,j,nkp) + conjg(lamp(l,i,nkp))*lamp(l,j,nkp)*eiw(l,nkp)
             END DO
           END DO
         END DO

         DO j=1,dimwann
           DO i = 1, j
             ap(i+((j-1)*j)/2) = ham(i,j,nkp)
           END DO
         END DO

         CALL zhpevx( 'v', 'a', 'u', dimwann, ap(1), zero, zero, 0, 0, -um,           &
              m, w(1), z(1,1), mxdbnd, work(1), rwork(1), iwork(1), ifail(1), info )
         IF ( info < 0 )  THEN
           WRITE(6,*) '*** ERROR *** ZHPEVX WHILE DIAGONALIZING HAMILTONIAN'
           WRITE(6,*) 'THE ',-INFO,' ARGUMENT OF ZHPEVX HAD AN ILLEGAL VALUE'
           STOP
         END IF
         IF ( INFO > 0 )  THEN
           WRITE(6,*) '*** ERROR *** ZHPEVX WHILE DIAGONALIZING HAMILTONIAN'
           WRITE(6,*) 'INFO= ', info, 'EIGENVECTORS FAILED TO CONVERGE'
           STOP
         END IF
 
! ...    Write the optimal subspace energy eigenvalues in eV (to be used in bands.f)
 
         WRITE(7,102) nkp,( har*w(i), i=1, dimwann )
 102     FORMAT(i5,4x,80f16.8)
 
! ...    Calculate amplitudes of the corresponding energy eigenvectors in terms of 
!        the original ("window space") energy eigenvectors
 
         DO j = 1, dimwann
           DO i = 1, dimwin(nkp)
             eamp(i,j,nkp) = czero
             DO l = 1, dimwann
               eamp(i,j,nkp) = eamp(i,j,nkp) + z(l,j)*lamp(i,l,nkp)
             END DO
           END DO
         END DO
 
       ENDDO ! end of nkpa loop

       CLOSE(7)

! ...  Write to a file the optimal subspace energy eigenvectors
!      in the basis of the "window space" energy eigenvectors

! ...  Note: for the purpose of minimizing Omegatld in wannier.f we could have simply
!      have used the lambda eigenvectors as the basis set for the optimal subspace,
!      i.e., write lamp instead of eamp. However, in order to calculate the
!      interpolated band structure we need to assume that the unitary rotations
!      obtained in wannier.f are done starting from the energy eigenvectors.
!      Of course, if we were only interested in the maxloc WFs, not in the 
!      interpolated band structure, we could have used lamp. I have check that
!      the resulting spread and average location of the maxloc WFs are exactly the 
!      same if we replace eamp by lamp in the write statement below.
 
       OPEN( UNIT=8, FILE='subspace.dat', STATUS='UNKNOWN', FORM='UNFORMATTED' )

!
! ...  Slight change in the format by ANDREA (28 jan 2004)
!      REPETITA IUVANT for self-energy conversion to wannier basis  
!      next 2 lines added
!
       WRITE(8) nkpts     
       WRITE(8) ( imin(nkp), imax(nkp), nkp=1,nkpts )


       DO nkp = 1, nkpts
         WRITE(8) dimwann, ( (eamp(j,i,nkp), j=1, dimwin(nkp) ), i=1, dimwann )
       END DO

       CLOSE(8)

       IF ( cflag == 1 )  THEN
         WRITE(*,*)
         WRITE(*,*) '*** WARNING ***'
         WRITE(*,*) 'AT SOME K-POINT(S) COMPLEMENT SUBSPACE HAS ZERO DIMENSIONALITY'
         WRITE(*,*) '=> DID NOT CREATE FILE compspace.dat'         
       ELSE
 
! ...  Diagonalize the hamiltonian in the complement subspace, write the
!      corresponding eigenfunctions and energy eigenvalues
 
!        nkp loop
         DO nkp = 1, nkpts
           DO j = 1, dimwin(nkp)-dimwann
             DO i = 1, dimwin(nkp)-dimwann
               ham(i,j,nkp) = czero
               DO l = 1, dimwin(nkp)
                 ham(i,j,nkp) = ham(i,j,nkp) + CONJG(camp(l,i,nkp))*camp(l,j,nkp)*eiw(l,nkp)
               END DO
             END DO
           END DO

           DO j = 1, dimwin(nkp)-dimwann
             DO i = 1, j
               ap(i+((j-1)*j)/2) = ham(i,j,nkp)
             END DO
           END DO

           CALL zhpevx( 'v', 'a', 'u', dimwin(nkp)-dimwann, ap(1),             &
                zero, zero, 0, 0, -um, m, w(1), z(1,1), mxdbnd, work(1),       &
                rwork(1), iwork(1), ifail(1), info )
           IF ( info < 0 )  THEN
             WRITE(6,*) '*** ERROR *** ZHPEVX WHILE DIAGONALIZING HAMILTONIAN'
             WRITE(6,*) 'THE ',-INFO,' ARGUMENT OF ZHPEVX HAD AN ILLEGAL VALUE'
             STOP
           END IF
           IF ( info > 0 )  THEN
             WRITE(6,*) '*** ERROR *** ZHPEVX WHILE DIAGONALIZING HAMILTONIAN'
             WRITE(6,*) 'INFO= ', info, 'EIGENVECTORS FAILED TO CONVERGE'
             STOP
           END IF
 
! ...      Calculate amplitudes of the energy eigenvectors in the complement subspace in
!          terms of the original energy eigenvectors
 
           DO j = 1, dimwin(nkp)-dimwann
             DO i = 1, dimwin(nkp)
               eamp(i,j,nkp) = czero
               do l = 1, dimwin(nkp)-dimwann
                 eamp(i,j,nkp) = eamp(i,j,nkp)+z(l,j)*camp(i,l,nkp)
               END DO
             END DO
           END DO

         END DO ! end ok nkp loop

       ENDIF    ! cflag=1


! ...  Write to a file the energy "eigenvectors" in the complement subspace 
!      in the basis of the "window space" energy eigenvectors

       OPEN( UNIT=9, FILE='compspace.dat', STATUS='UNKNOWN', FORM='UNFORMATTED' )

       DO nkp = 1, nkpts
         write(9) dimwin(nkp)-dimwann, ( ( eamp(j,i,nkp), j=1, dimwin(nkp)), i=1, dimwin(nkp)-dimwann )
       END DO

       CLOSE(9)

       DEALLOCATE( kgv )
       DEALLOCATE( isort )
       DEALLOCATE( mtxd )
       DEALLOCATE( vkpt )
       DEALLOCATE( evecr )
       DEALLOCATE( eveci )
       DEALLOCATE( eiw )

       DEALLOCATE( nwhich )
       DEALLOCATE( nnshell )
       DEALLOCATE( nnlist )
       DEALLOCATE( nncell )
       DEALLOCATE( nntot )
       DEALLOCATE( neigh )
       DEALLOCATE( bk ) 
       DEALLOCATE( dnn ) 
       DEALLOCATE( wb )
       DEALLOCATE( bka )
       DEALLOCATE( cm )

       DEALLOCATE( komega_i_est )
       DEALLOCATE( lamp )
       DEALLOCATE( camp )
       DEALLOCATE( eamp )
       DEALLOCATE( mtrx_in )
       DEALLOCATE( mtrx_out )
       DEALLOCATE( dimwin )
       DEALLOCATE( dimfroz )
       DEALLOCATE( indxfroz )
       DEALLOCATE( indxnfroz )
       DEALLOCATE( frozen )

       DEALLOCATE( ap )
       DEALLOCATE( z )
       DEALLOCATE( work )
       DEALLOCATE( w )
       DEALLOCATE( rwork )
       DEALLOCATE( ifail )
       DEALLOCATE( iwork )

       DEALLOCATE( ham )

      CALL mp_end()

!=---------------------------------------------------------------=

       STOP '*** THE END *** (space.f90)'
       END

