       SUBROUTINE projection( avec, lamp, evecr, eveci, vkpt,              & 
                  kgv, isort, mtxd, dimwin, dimwann, dimfroz,              &
                  mxddim, mxdbnd, mxdnrk, mxdgve, ngx, ngy, ngz, nkpts,    &
                  gauss_typ, rphiimx1, rphiimx2, l_wann,                   &
                  m_wann, ndir_wann, rloc, ndwinx)

!...............................................................................
! ******
! INPUT:
! ******
!
! AVEC(I,J)         I-TH COMPONENT OF J-TH DIRECT LATTICE VECTOR
! EVECR(I,J,NKP)    REAL PART OF THE AMPLITUDE OF THE G-VECTOR 
!                   KGV(1:3,ISORT(I,NKP)) IN THE EXPANSION OF THE J-TH ENERGY 
!                   EIGENVECTOR INSIDE THE ENERGY WINDOW AT THE NKP-TH K-POINT 
! EVECI(I,J,NKP)    IMAGINARY PART OF THE AMPLITUDE
! VKPT(I,NKP)       i-th component in reciprocal lattice coordinates
!                   of the nkp-th k-point
! KGV(I,N)          I-TH COMPONENT (RECIPROCAL LATTICE COORDINATES) OF
!                   THE N-TH G-VECTOR ORDERED BY STARS OF INCREASING LENGTH
! ISORT(I,NKP)      G-VECTOR ASSOCIATED WITH ROW/COLUMN I OF HAMILTONIAN AT
! MTXD(NKP)         DIMENSION OF THE HAMILTONIAN AT THE NKP-TH K-POINT
!                   THE NKP-TH K-POINT IS KGV(1:3,ISORT(I,NKP))
! DIMWIN(NKP)       number of bands at the nkp-th k-point that fall 
!                   within the outer energy window               
! DIMWANN           dimensionality of the subspace at each k-point 
!                   (number of Wannier functions per unit cell that we want)
! DIMFROZ(NKP)      number of frozen bands at the nkp-th k-point 
! MXDDIM            ARRAY DIMENSION OF HAMILTONIAN ROWS
! MXDBND            ARRAY DIMENSION FOR BANDS
! MXDNRK            ARRAY DIMENSION FOR K-POINTS
! MXDGVE            ARRAY DIMENSION FOR G-SPACE VECTORS
!...............................................................................

       USE kinds
       USE fft_scalar
       USE io_global, ONLY : stdout
       USE timing_module, ONLY : timing

       IMPLICIT NONE

       ! ... arguments

       INTEGER :: mxddim, mxdbnd, mxdnrk
       INTEGER :: mxdgve, ndwinx
       INTEGER :: ngx, ngy, ngz, nkpts
       INTEGER :: mtxd(mxdnrk)
       INTEGER :: kgv(3,mxdgve)
       INTEGER :: isort(mxddim,mxdnrk)
       INTEGER :: dimwann
       INTEGER :: dimwin(mxdnrk)
       INTEGER :: dimfroz(mxdnrk)
       REAL(dbl) :: avec(3,3)
       REAL(dbl) :: evecr(mxddim,ndwinx,mxdnrk)
       REAL(dbl) :: eveci(mxddim,ndwinx,mxdnrk)
       REAL(dbl) :: vkpt(3,mxdnrk)
       COMPLEX(dbl) :: lamp(mxdbnd,mxdbnd,mxdnrk)

       INTEGER :: gauss_typ(dimwann)
       REAL(dbl) :: rphiimx1(3,dimwann)
       REAL(dbl) :: rphiimx2(3,dimwann)
       INTEGER :: l_wann(dimwann)
       INTEGER :: m_wann(dimwann)
       INTEGER :: ndir_wann(dimwann)
       REAL(dbl) :: rloc(dimwann)

 
       ! ... local variables

       INTEGER :: mplwv
       INTEGER :: nb, i, j, l, m 
       INTEGER :: nkp, npoint
       INTEGER :: ngdim(3)
       REAL(dbl) :: aside, asidemin
       COMPLEX(dbl) :: catmp

       REAL(dbl) :: bohr, ryd, pi, twopi, zero
       COMPLEX(dbl) :: czero, ci, citpi
       PARAMETER ( pi = 3.14159265358979323846d0 )
       PARAMETER ( twopi = 2.0d0 * pi )
       PARAMETER ( zero = 0.0d0 )
       PARAMETER ( bohr = 0.52917715d0 )
       PARAMETER ( ryd  = 13.605826d0 )
       PARAMETER( czero = ( 0.0d0, 0.0d0 ) )
       PARAMETER( ci = ( 0.0d0, 1.0d0 ) )
       PARAMETER( citpi = ( zero, twopi ) )

       COMPLEX(dbl) :: ctmp, cphi
       INTEGER :: nwann 
       INTEGER :: nxx, nyy, nzz 
       INTEGER :: nx, ny, nz
       REAL(dbl) :: rx, ry, rz
       REAL(dbl) :: rpos1(3), rpos2(3)
       REAL(dbl) :: dist1, dist2
       REAL(dbl) :: dist_pl, dist_cos 
       REAL(dbl) :: th_cos, th_sin 
       REAL(dbl) :: ph_cos, ph_sin
       REAL(dbl) :: scalf
       REAL(dbl) :: sph00, sph1m1, sph10
       REAL(dbl) :: sph11, sph2m2, sph2m1
       REAL(dbl) :: sph20, sph21, sph22        
 
       INTEGER :: info

       COMPLEX(dbl), ALLOCATABLE :: u(:,:)
       COMPLEX(dbl), ALLOCATABLE :: vt(:,:)
       COMPLEX(dbl), ALLOCATABLE :: work(:)
       REAL(dbl), ALLOCATABLE :: s(:)
       REAL(dbl), ALLOCATABLE :: rwork1(:)
       REAL(dbl), ALLOCATABLE :: rwork2(:)
       REAL(dbl), ALLOCATABLE :: rphicmx1(:,:)
       REAL(dbl), ALLOCATABLE :: rphicmx2(:,:)
       INTEGER, ALLOCATABLE :: nphimx1(:,:)
       INTEGER, ALLOCATABLE :: nphimx2(:,:)
       INTEGER, ALLOCATABLE :: nphir(:)

       COMPLEX(dbl), ALLOCATABLE :: cptwr(:)
       COMPLEX(dbl), ALLOCATABLE :: cwork2(:)
       COMPLEX(dbl), ALLOCATABLE :: ca(:,:,:)
       COMPLEX(dbl), ALLOCATABLE :: cu(:,:)

       CHARACTER( LEN=6 ) :: verbosity = 'none'    ! none, low, medium, high
       INTEGER :: ierr

! ...  End of declaration

       CALL timing('projection',OPR='start')

       mplwv = ngx * ngy * ( ngz+1 )

       ALLOCATE( cptwr(ngx*ngy*(ngz+1)), STAT = ierr )
       IF( ierr /= 0 ) THEN
         CALL errore( ' projection ', ' allocating cptwr ', ngx*ngy*(ngz+1) )
       END IF
       ALLOCATE( cwork2(ngx*ngy*(ngz+1)), STAT = ierr )
       IF( ierr /= 0 ) THEN
         CALL errore( ' projection ', ' allocating cwork2 ', ngx*ngy*(ngz+1) )
       END IF
       ALLOCATE( ca(mxdbnd,dimwann,nkpts), STAT = ierr )
       IF( ierr /= 0 ) THEN
         CALL errore( ' projection ', ' allocating ca ', mxdbnd*dimwann*nkpts )
       END IF
       ALLOCATE( cu(mxdbnd,dimwann), STAT = ierr )
       IF( ierr /= 0 ) THEN
         CALL errore( ' projection ', ' allocating cu ', mxdbnd*dimwann )
       END IF
       ALLOCATE( u(mxdbnd,mxdbnd), STAT = ierr )
       IF( ierr /= 0 ) THEN
         CALL errore( ' projection ', ' allocating u ', mxdbnd*mxdbnd )
       END IF
       ALLOCATE( vt(dimwann,dimwann), STAT = ierr )
       IF( ierr /= 0 ) THEN
         CALL errore( ' projection ', ' allocating vt ', dimwann*dimwann )
       END IF

       ALLOCATE( work(4*mxdbnd), STAT = ierr )
       IF( ierr /= 0 ) THEN
         CALL errore( ' projection ', ' allocating work ', 4*mxdbnd )
       END IF

       ALLOCATE( s(dimwann), STAT = ierr )
       IF( ierr /= 0 ) THEN
         CALL errore( ' projection ', ' allocating s ', dimwann )
       END IF

       ALLOCATE( rwork1(3*dimwann), STAT = ierr )
       IF( ierr /= 0 ) THEN
         CALL errore( ' projection ', ' allocating rwork1 ', 3*dimwann )
       END IF

       ALLOCATE( rwork2(5*dimwann), STAT = ierr )
       IF( ierr /= 0 ) THEN
         CALL errore( ' projection ', ' allocating rwork2 ', 5*dimwann )
       END IF

       ALLOCATE( rphicmx1(3,dimwann), STAT = ierr )
       IF( ierr /= 0 ) THEN
         CALL errore( ' projection ', ' allocating rphicmx1 ', 3*dimwann )
       END IF

       ALLOCATE( rphicmx2(3,dimwann), STAT = ierr )
       IF( ierr /= 0 ) THEN
         CALL errore( ' projection ', ' allocating rphicmx2 ', 3*dimwann )
       END IF

       ALLOCATE( nphimx1(3,dimwann), STAT = ierr )
       IF( ierr /= 0 ) THEN
         CALL errore( ' projection ', ' allocating nphimx1 ', 3*dimwann )
       END IF

       ALLOCATE( nphimx2(3,dimwann), STAT = ierr )
       IF( ierr /= 0 ) THEN
         CALL errore( ' projection ', ' allocating nphimx2 ', 3*dimwann )
       END IF

       ALLOCATE( nphir(dimwann), STAT = ierr )
       IF( ierr /= 0 ) THEN
         CALL errore( ' projection ', ' allocating nphir ', dimwann )
       END IF



       DO nwann = 1, dimwann
         DO m = 1, 3
           rphicmx1(m,nwann) = 0.0d0
           rphicmx2(m,nwann) = 0.0d0
           DO j = 1, 3
             rphicmx1(m,nwann) = rphicmx1(m,nwann) + rphiimx1(j,nwann) * avec(m,j)
             rphicmx2(m,nwann) = rphicmx2(m,nwann) + rphiimx2(j,nwann) * avec(m,j)
           END DO
         END DO
       END DO

       ngdim(1) = ngx
       ngdim(2) = ngy
       ngdim(3) = ngz
       DO nwann = 1, dimwann
        DO m = 1, 3
         nphimx1(m,nwann)= INT( rphiimx1(m,nwann) * DBLE( ngdim(m) ) + 1.001d0 )
         nphimx2(m,nwann)= INT( rphiimx2(m,nwann) * DBLE( ngdim(m) ) + 1.001d0 )
        END DO
       END DO
       
       WRITE(stdout,*) ' Gaussian centers, in cartesian coordinates'
       WRITE(stdout,*) ' '
       DO nwann = 1, dimwann
         WRITE( stdout,'(a12,i4,3f10.5)' ) 'Gaussian 1: ', nwann, ( rphicmx1(m,nwann), m=1,3 )
         IF  ( gauss_typ(nwann) == 2 ) &
         WRITE( stdout,'(a12,i4,3f10.5)' ) 'Gaussian 2: ', nwann, ( rphicmx2(m,nwann), m=1,3 )
       END DO

       WRITE( stdout, *) ' '
       WRITE( stdout, *) ' Gaussian centers, in relative coordinates'
       WRITE( stdout, *) ' '

       DO nwann = 1, dimwann
        WRITE( stdout, '(a12,i4,3f10.5)' )  'Gaussian 1: ', nwann,( rphiimx1(m,nwann), m=1,3 )
        IF  ( gauss_typ(nwann) == 2 )  &
        WRITE( stdout ,'(a12,i4,3f10.5)' )  'Gaussian 2: ', nwann,( rphiimx2(m,nwann), m=1,3 )
       END DO

       WRITE(stdout, *) ' '
       WRITE(stdout, *) ' Gaussian centers, nearest grid coordinates'
       WRITE(stdout, *) ' '

       DO nwann = 1, dimwann
         WRITE(stdout,'(a12,i6,3i4)')    'Gaussian 1: ', nwann,( nphimx1(m,nwann), m=1,3 )
         IF  ( gauss_typ(nwann) ==  2 )    &
         WRITE(stdout,'(a12,i6,3i4)')    'Gaussian 2: ', nwann,( nphimx2(m,nwann), m=1,3 )
       END DO

       DO nwann = 1, dimwann
         asidemin = 100000.0d0 * rloc(nwann)
         DO j = 1, 3
           aside = SQRT( avec(1,j)**2 + avec(2,j)**2 + avec(3,j)**2 )
           asidemin = MIN( aside, asidemin )
         END DO
         nphir(nwann) = nint( 2 * ( rloc(nwann) / asidemin ) * MIN( ngx,ngy,ngz ) )
         WRITE(stdout,*) ' '
         WRITE(stdout,'(a14,i3)') 'Trial orbital ', nwann
         WRITE(stdout,*) ' Gaussian width, in atomic units  ', rloc(nwann)
         WRITE(stdout,*) ' Half-width of integration region ', nphir(nwann)
         WRITE(stdout,*) ' '
         WRITE(stdout,8000)
8000     FORMAT (1x,'-------------------------------------------------------------------------------')
       END DO

! ...  Calculate the projection of the gaussians on the bloch eigenstates inside 
!      energy window: store it in dimwin(nkp) X dimwann overlap matrix ca

! ...  Mostly copied from turkey.f, w/ some modIF ications and the replacements 

! ...  nbands --> dimwann or dimwin(nkp), and dirc(i,j) --> avec(j,i), 

! ...  Since we are working in bohr, not angstroms (Recall that rloc was also 
!      converted to bohr.), and cexp --> exp 

! ...  Now pick up a consistent phase for the u_nk (the initial ones
!      are provided by the ab-initio code, and so have almost random rotations)
     
       sph00  = 1.0d0 / SQRT( 2.0d0 * twopi )
       sph1m1 = SQRT( 1.5d0 / twopi )
       sph10  = SQRT( 1.5d0 / twopi )
       sph11  = SQRT( 1.5d0 / twopi )
       sph2m2 = SQRT( 15.0d0 / 2.0d0 / twopi )
       sph2m1 = SQRT( 15.0d0 / 2.0d0 / twopi )
       sph20  = SQRT(  5.0d0 / 8.0d0 / twopi )
       sph21  = SQRT( 15.0d0 / 2.0d0 / twopi )
       sph22  = SQRT( 15.0d0 / 2.0d0 / twopi )

       DO nkp = 1, nkpts
         IF  ( dimwann >  dimfroz(nkp) ) THEN  !IF  not, don't need to waste CPU time!
           DO nb = 1, dimwin(nkp)

             DO m = 1 ,mplwv
               cptwr(m) = czero
               cwork2(m) = czero
             END DO

! ...        Go through all the g-vectors inside the cutoff radius at the present k-point
 
             DO J=1,MTXD(NKP)           

! ...          Use IF  ...THEN ...ELSE instead! (also in interface.f and intf_mwf.f) 

               IF  ( kgv( 1, isort(j,nkp) ) >=  0 ) nx = kgv( 1, isort(j,nkp) ) + 1
               IF  ( kgv( 1, isort(j,nkp) ) <   0 ) nx = kgv( 1, isort(j,nkp) ) + 1 + ngx

               IF  ( kgv( 2, isort(j,nkp) ) >=  0 ) ny = kgv( 2, isort(j,nkp) ) + 1
               IF  ( kgv( 2, isort(j,nkp) ) <   0 ) ny = kgv( 2, isort(j,nkp) ) + 1 + ngy

               IF  ( kgv( 3, isort(j,nkp) ) >=  0 ) nz = kgv( 3, isort(j,nkp) ) + 1
               IF  ( kgv( 3, isort(j,nkp) ) <   0 ) nz = kgv( 3, isort(j,nkp) ) + 1 + ngZ

               npoint = nx + (ny-1)*ngx + (nz-1)*ngx*ngy

! ...          Npoint is the absolute (k-independent) castep index of the gvector 
!              kgv(*,isort(j,nkp)). Note that in general it is not equal to nindpw(j,nkp), 
!              since both the ordering of the g-vectors at any given k-point and their 
!              absolute orderinging is dIF ferent in cpw and in castep. (In cpw the 
!              absolute ordering is by stars of increasing length.) Think more about this,
!              to make sure all that I said is correct.
! ...          Bloch state in the g-space grid

 
               cptwr(npoint) = CMPLX( evecr(j,nb,nkp), eveci(j,nb,nkp) )
 
             END DO ! g-vectors at present k (J)
 
! ...        Compute the bloch state in the real space grid via fft from the g-space grid
 
! ...        last argument is isign=-1 since we are using the usual bloch convention.
!            isign = -1 : backward fft: recip --> real : exp(+iqr)
 
             CALL cfft3d( cptwr, ngx, ngy, ngz, ngx, ngy, ngz, 1)
 
! ...        ca: the phase <u_nk(r) exp(ikr) | phi_nwann(r)>, given a real space
!            localized function phi_nwann (e.g. a gaussian centered on the bond)
! ...        nxx, nyy, nzz span a parallelogram in the real space mesh, of side
!            2*nphir, and centered around the maximum of phi_i, nphimax(i,1:3)
! ...        nx ny nz are the nxx nyy nzz brought back to the unit cell in
!            which u_nk(r)=cptwr is represented
! ...        Loop over trial localized orbitals

             DO nwann = 1, dimwann

               ca(nb,nwann,nkp) = czero    

! ...          First gaussian

               DO nzz = nphimx1(3,nwann) - nphir(nwann), nphimx1(3,nwann) + nphir(nwann)
                 nz = MOD( nzz, ngz )
                 IF  ( nz < 1 ) nz = nz + ngz
                 DO nyy = nphimx1(2,nwann) - nphir(nwann), nphimx1(2,nwann) + nphir(nwann)
                   ny = MOD( nyy, ngy )
                   IF  ( ny < 1 ) ny = ny + ngy
                   DO nxx = nphimx1(1,nwann) - nphir(nwann), nphimx1(1,nwann) + nphir(nwann)
                     nx = MOD( nxx, ngx )
                     IF  (nx < 1 ) nx = nx + ngx

! ...                Here it calculates <exp(i*k.r) u_nk(r)|

                     rx = DBLE(nxx-1) / DBLE(ngx)
                     ry = DBLE(nyy-1) / DBLE(ngy)
                     rz = DBLE(nzz-1) / DBLE(ngz)
                     scalf = vkpt(1,nkp)*rx + vkpt(2,nkp)*ry + vkpt(3,nkp)*rz
                     npoint = nx + (ny-1)*ngx + (nz-1)*ngy*ngx
                     catmp = CONJG( EXP( citpi * scalf ) * cptwr(npoint) )
         
                     DO m = 1, 3
                       rpos1(m) = ( rx - rphiimx1(1,nwann) ) * avec(m,1) +  &
                                  ( ry - rphiimx1(2,nwann) ) * avec(m,2) +  &
                                  ( rz - rphiimx1(3,nwann) ) * avec(m,3)
                     END DO

                     dist1 = 0.0d0
                     DO m = 1, 3
                       dist1 = dist1 + rpos1(m)**2
                     END DO
                     dist1 = SQRT(dist1)
 
! ...                Positive gaussian

                     cphi = CMPLX( EXP( -( dist1 / rloc(nwann) )**2 ), 0.0d0 )

                     IF  ( gauss_typ(nwann) == 1 ) THEN

                       IF  ( ndir_wann(nwann) == 3 ) THEN
                         dist_pl  = SQRT( rpos1(1)**2 + rpos1(2)**2 )
                         dist_cos = rpos1(3)
                       ELSE IF  ( ndir_wann(nwann) == 2 ) THEN
                         dist_pl  = SQRT( rpos1(1)**2 + rpos1(3)**2 )
                         dist_cos = rpos1(2)
                       ELSE IF  ( ndir_wann(nwann) == 1 ) THEN
                         dist_pl  = SQRT( rpos1(2)**2 + rpos1(3)**2 )
                         dist_cos = rpos1(1)
                       ELSE
                         WRITE(stdout,*) 'ERROR: Wrong z-direction'
                         CALL errore(' projection ', ' wrong z- direction ', ndir_wann(nwann) )
                       END IF 

! ...                  IF  rpos is on the origin, or on the z axis, I give arbitrary
!                      values to cos/sin of theta, or of phi, respectively
  
                       IF  ( ABS( dist1 ) <= 1.e-10 ) THEN
                         th_cos = 0.0d0
                         th_sin = 0.0d0
                       ELSE
                         th_cos = dist_cos / dist1
                         th_sin = dist_pl / dist1
                       END IF 

                       IF  (ABS( dist_pl ) <= 1.e-10 ) THEN
                         ph_cos = 0.0d0
                         ph_sin = 0.0d0
                       ELSE
                         IF ( ndir_wann(nwann) == 3 ) THEN
                           ph_cos = rpos1(1) / dist_pl
                           ph_sin = rpos1(2) / dist_pl
                         ELSE IF ( ndir_wann(nwann) == 2 ) THEN
                           ph_cos = rpos1(3) / dist_pl
                           ph_sin = rpos1(1) / dist_pl
                         ELSE 
                           ph_cos = rpos1(2) / dist_pl
                           ph_sin = rpos1(3) / dist_pl
                         END IF 
                       END IF 

                       IF ( l_wann(nwann) == 2 ) THEN
  
                         IF ( m_wann(nwann) == -2 ) THEN
                           cphi = sph2m2 * cphi * ( th_sin**2 ) * ( ph_cos**2 - ph_sin**2 )
                         ELSE IF ( m_wann(nwann) == -1 ) THEN
                           cphi = sph2m1 * cphi * th_sin * th_cos * ph_cos
                         ELSE IF ( m_wann(nwann) == 0 ) THEN
                           cphi = sph20 * cphi * ( 3.0d0 * th_cos**2 - 1.0d0 )
                         ELSE IF ( m_wann(nwann) == 1 ) THEN
                           cphi = sph21 * cphi * th_sin * th_cos * ph_sin
                         ELSE IF ( m_wann(nwann) == 2 ) THEN
                           cphi = sph22 * cphi * ( th_sin**2 ) * 2.0d0 * ph_sin * ph_cos
                         ELSE 
                           WRITE(stdout,*) 'ERROR: check the spherical harmonics (I)'
                           CALL errore(' projection ', ' check the spherical harmonics (I)', m_wann(nwann) )
                         END IF 
  
                       ELSE IF ( l_wann(nwann) == 1 ) THEN

                         IF ( m_wann(nwann) == -1 ) THEN
                           cphi = sph1m1 * cphi * th_sin * ph_cos
                         ELSE IF ( m_wann(nwann) == 0 ) THEN
                           cphi = sph10 * cphi * th_cos
                         ELSE IF ( m_wann(nwann) == 1 ) THEN
                           cphi = sph11 * cphi * th_sin * ph_sin
                         ELSE 
                           WRITE(stdout,*) 'ERROR: check the spherical harmonics (II)'
                           CALL errore(' projection ', ' check the spherical harmonics (II)', m_wann(nwann) )
                         END IF 
  
                       ELSE IF ( l_wann(nwann) == 0 ) THEN
                         cphi = sph00 * cphi

                       ELSE IF ( l_wann(nwann) == -1 ) THEN
! ...                    sp^3 orbitals

                         IF ( m_wann(nwann) == 1 ) THEN 
! ...                      sp^3 along 111 direction IF  ndir_wann(nwann)=3
                           cphi = cphi * ( sph00 + sph1m1 * th_sin * ph_cos +        &
                                  sph11 * th_sin * ph_sin + sph10 * th_cos ) / 2.0d0
  
                         ELSE IF ( m_wann(nwann) == 2 ) THEN
! ...                      sp^3 along 1,-1,-1 direction IF  ndir_wann(nwann)=3
                           cphi = cphi * ( sph00 + sph1m1 * th_sin * ph_cos -        &
                                  sph11 * th_sin * ph_sin - sph10 * th_cos ) / 2.0d0
  
                         ELSE IF ( m_wann(nwann) == 3 ) THEN
! ...                      sp^3 along -1,1,-1 direction IF  ndir_wann(nwann)=3
                           cphi = cphi * ( sph00 - sph1m1 * th_sin * ph_cos +        &
                                  sph11 * th_sin * ph_sin - sph10 * th_cos ) / 2.0d0

                         ELSE IF ( m_wann(nwann) == 4 ) THEN
! ...                      sp^3 along -1,-1,1 direction IF  ndir_wann(nwann)=3
                           cphi = cphi * ( sph00 - sph1m1 * th_sin * ph_cos -        &
                                  sph11 * th_sin * ph_sin + sph10 * th_cos ) / 2.0d0

                         ELSE IF ( m_wann(nwann) == -1 ) THEN 
! ...                      sp^3 along -1,-1,-1 direction IF  ndir_wann(nwann)=3
                           cphi = cphi * ( sph00 - sph1m1 * th_sin * ph_cos -        &
                                  sph11 * th_sin * ph_sin - sph10 * th_cos ) / 2.0d0
  
                         ELSE IF ( m_wann(nwann) == -2 ) THEN
! ...                      sp^3 along -1,1,1 direction IF  ndir_wann(nwann)=3
                           cphi = cphi * ( sph00 - sph1m1 * th_sin * ph_cos +        &
                                  sph11 * th_sin * ph_sin + sph10 * th_cos ) / 2.0d0

                         ELSE IF ( m_wann(nwann) == -3 ) THEN
! ...                      sp^3 along 1,-1,1 direction IF  ndir_wann(nwann)=3
                           cphi = cphi * ( sph00 + sph1m1 * th_sin * ph_cos -        &
                                  sph11 * th_sin * ph_sin + sph10 * th_cos ) / 2.0d0
  
                         ELSE IF ( m_wann(nwann) == -4 ) THEN
! ...                      sp^3 along 1,1,-1 direction IF  ndir_wann(nwann)=3
                           cphi = cphi * ( sph00 + sph1m1 * th_sin * ph_cos +        &
                                  sph11 * th_sin * ph_sin - sph10 * th_cos ) / 2.0d0

                         ELSE
                           WRITE (stdout, *)  '*** ERROR *** in sp^3 hybrid gaussian: check m_wann'
                           CALL errore(' projection ', ' sp^3 hybrid gaussian ', m_wann(nwann) )
                         END IF 

                       ELSE 
                         WRITE(stdout,*) '*** ERROR *** : check the spherical harmonics (III)'
                         CALL errore(' projection ', ' check the spherical harmonics (III)', m_wann(nwann) )
                       END IF 

                     END IF  ! orbital is of type 1

                     ca(nb,nwann,nkp) = ca(nb,nwann,nkp) + catmp * cphi

                   END DO ! nxx
                 END DO  ! nyy
               END DO   ! nzz

               IF ( gauss_typ(nwann) == 2 ) THEN
! ...            second gaussian

                 DO nzz = nphimx2(3,nwann) - nphir(nwann), nphimx2(3,nwann) + nphir(nwann)
                   nz = MOD( nzz, ngz )
                   IF ( nz < 1 ) nz = nz + ngz
                   DO nyy = nphimx2(2,nwann) - nphir(nwann), nphimx2(2,nwann) + nphir(nwann)
                     ny = MOD( nyy, ngy )
                     IF ( ny < 1 ) ny = ny + ngy
                     DO nxx = nphimx2(1,nwann) - nphir(nwann), nphimx2(1,nwann) + nphir(nwann)
                       nx = MOD( nxx, ngx )
                       IF  (nx < 1) nx = nx + ngx

! ...                  here it calculates <exp(i*k.r) u_nk(r)|

                       rx = DBLE(nxx-1) / DBLE(ngx)
                       ry = DBLE(nyy-1) / DBLE(ngy)
                       rz = DBLE(nzz-1) / DBLE(ngz)
                       scalf = vkpt(1,nkp)*rx + vkpt(2,nkp)*ry + vkpt(3,nkp)*rz
                       npoint = nx + (ny-1)*ngx + (nz-1)*ngy*ngx
                       catmp=CONJG( EXP( citpi * scalf ) * cptwr(npoint) )
          
                       DO m = 1, 3
                         rpos2(m) = ( rx - rphiimx2(1,nwann) ) * avec(m,1) +    &
                                    ( ry - rphiimx2(2,nwann) ) * avec(m,2) +    &
                                    ( rz - rphiimx2(3,nwann) ) * avec(m,3)
                       END DO

                       dist2 = 0.0d0
                       DO m = 1, 3
                         dist2 = dist2 + rpos2(m)**2
                       END DO
                       dist2 = SQRT(dist2)
 
! ...                  Negative gaussian

                       cphi = -CMPLX( EXP( -( dist2/rloc(nwann) )**2 ), 0.0d0 )

                       ca(nb,nwann,nkp) = ca(nb,nwann,nkp) + catmp * cphi

                     END DO ! nxx
                   END DO  ! nyy
                 END DO   ! nzz
               END IF 
             END DO    ! nwann

           END DO    ! nb
      
           WRITE(stdout,*) '  '
           WRITE(stdout,'(a18,i4)') ' Matrix A, k-point', nkp
           IF  (dimwin(nkp) <= 8) THEN
             WRITE(stdout,*) '  '
             DO i = 1, dimwin(nkp)
               WRITE(stdout,'(14(0pe10.2))') ( ca(i,j,nkp), j=1,dimwann )
             END DO

           END IF 

         END IF  

       END DO ! NKP

       WRITE(stdout,*) ' '
       WRITE(stdout,8000)
 
! ...  Compute the dimwin(k) x dimwann matrix cu that yields, from the dimwin(k) 
!      original bloch states, the dimwann bloch-like states with maximal projection
!      onto the dimwann gaussians:
!      cu = ca.cs^{-1/2}, cs = transpose(ca).ca
 
! ...  use the singular-value decomposition of the matrix ca: 
!      ca = cz.cd.cv  
!      which yields
!      cu = cz.cd.cd^{-1}.cv
! ...  where cz is dimwin(nkp) x dimwin(nkp) and unitary, cd is 
!      dimwin(nkp) X dimwann and diagonal, cd^{-1} is dimwann X dimwann and 
!      diagonal, and cv is dimwann x dimwann and unitary.
 
       DO nkp=1,nkpts
         IF ( dimwann > dimfroz(nkp) ) THEN
 
! ...      SINGULAR VALUE DECOMPOSITION
 
           CALL zgesvd( 'a', 'a', dimwin(nkp), dimwann, ca(1,1,nkp),              &
                mxdbnd, s, u, mxdbnd, vt, dimwann, work, 4*mxdbnd, rwork2, info )

           IF ( info /= 0 ) THEN
             WRITE(stdout,*) '*** ERROR *** IN ZGESVD IN projection.f'
             WRITE(stdout,*) 'K-POINT NKP=', nkp, ' INFO=', info
             IF ( info < 0 ) CALL errore(' projection ', ' zgesvd: info has illegal value ', info )
           END IF 
 
           DO j=1,dimwann
             DO i=1,dimwin(nkp)
               cu(i,j)=czero
               DO l=1,dimwann
                 cu(i,j)=cu(i,j)+u(i,l)*CONJG(vt(j,l))
               END DO
             END DO
           END DO
 
           IF ( verbosity == 'high' ) THEN
! ...        Check unitariety
             WRITE(stdout,*) ' '
             WRITE(stdout,*) ' '
             WRITE(stdout,*) 'k-point',nkp
 
! ...        Note that cu.transpose(cu) is *NOT* an identity dimwin(nkp) by dimwin(nkp) 
!            matrix, but transpose(cu).cu is a dimwann by dimwann identity matrix. 
!            I have once checked the former statement, now I will just leave here the code
!            for the latter (what this means is that the columns of cu are orthonormal
!            vectors).
 
             WRITE(stdout,*) ' '
             WRITE(stdout,*) 'transpose(cu).cu:'
             DO i = 1, dimwann
               DO j = 1, dimwann
                 ctmp = czero
                 DO m = 1, dimwin(nkp)
                   ctmp = ctmp + CONJG( cu(m,i) ) * cu(m,j)
                 END DO
                 WRITE(stdout,'(2i4,2f10.7)') i, j, ctmp
               END DO
             END DO
           END IF
 
           DO j = 1, dimwann
             DO i = 1, dimwin(nkp)
               lamp(i,j,nkp) = cu(i,j)
             END DO
           END DO
!
         END IF 
       END DO ! NKP

       DEALLOCATE( cptwr, STAT=ierr )
           IF (ierr/=0) CALL errore(' projection ',' deallocating cptwr',ABS(ierr))
       DEALLOCATE( cwork2, STAT=ierr )
           IF (ierr/=0) CALL errore(' projection ',' deallocating cwork2',ABS(ierr))
       DEALLOCATE( ca, STAT=ierr )
           IF (ierr/=0) CALL errore(' projection ',' deallocating ca',ABS(ierr))
       DEALLOCATE( cu, STAT=ierr )
           IF (ierr/=0) CALL errore(' projection ',' deallocating cu',ABS(ierr))
       DEALLOCATE( u, STAT=ierr )
           IF (ierr/=0) CALL errore(' projection ',' deallocating u',ABS(ierr))
       DEALLOCATE( vt, STAT=ierr )
           IF (ierr/=0) CALL errore(' projection ',' deallocating vt',ABS(ierr))
       DEALLOCATE( work, STAT=ierr )
           IF (ierr/=0) CALL errore(' projection ',' deallocating work',ABS(ierr))
       DEALLOCATE( s, STAT=ierr )
           IF (ierr/=0) CALL errore(' projection ',' deallocating s',ABS(ierr))
       DEALLOCATE( rwork1, STAT=ierr )
           IF (ierr/=0) CALL errore(' projection ',' deallocating rwork1',ABS(ierr))
       DEALLOCATE( rwork2, STAT=ierr )
           IF (ierr/=0) CALL errore(' projection ',' deallocating rwork2',ABS(ierr))
       DEALLOCATE( rphicmx1, STAT=ierr )
           IF (ierr/=0) CALL errore(' projection ',' deallocating rphicmx1',ABS(ierr))
       DEALLOCATE( rphicmx2, STAT=ierr )
           IF (ierr/=0) CALL errore(' projection ',' deallocating rphicmx2',ABS(ierr))
       DEALLOCATE( nphimx1, STAT=ierr )
           IF (ierr/=0) CALL errore(' projection ',' deallocating nphimx1',ABS(ierr))
       DEALLOCATE( nphimx2, STAT=ierr )
           IF (ierr/=0) CALL errore(' projection ',' deallocating nphimx2',ABS(ierr))
       DEALLOCATE( nphir, STAT=ierr )
           IF (ierr/=0) CALL errore(' projection ',' deallocating nphir',ABS(ierr))

       CALL timing('projection',OPR='stop')

       RETURN
       END SUBROUTINE
