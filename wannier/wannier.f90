      PROGRAM wannier

      USE kinds
      USE constants, ONLY: pi, twopi => tpi, &
         ryd => ry, har => au, bohr => bohr_radius_angs
      USE parameters, ONLY: mxdtyp => npsx, mxdatm => natx
      USE fft_scalar, ONLY: cfft3d
      USE input_wannier

      IMPLICIT NONE

      ! 12 is the maximum number of nearest neighbours

      INTEGER, PARAMETER :: nnmx  = 24 
      INTEGER, PARAMETER :: nnmxh = nnmx/2 

      COMPLEX(dbl) :: czero
      COMPLEX(dbl) :: ci, citpi
      PARAMETER ( czero = ( 0.0d0, 0.0d0 ) )
      PARAMETER ( ci = ( 0.0, 1.0 ) )
      PARAMETER ( citpi = ( 0.0, 6.283185307179 ) )

      CHARACTER( LEN=6 ) :: verbosity = 'none'    ! none, low, medium, high

      ! external functions

      REAL(dbl) :: ranf

      INTEGER :: ngx, ngy, ngz
      INTEGER :: ng
      INTEGER :: nkpts
    
      INTEGER :: nplwv, mplwv

      INTEGER :: iprint, nsp, idummy
      INTEGER :: i, j, k
      INTEGER :: ni, m, nkp
      INTEGER :: nkp2, nx, ny, nz
      INTEGER :: np, npoint
      INTEGER :: iib, iseed
      INTEGER :: ndnntot, nlist, l, n, ndnn
      INTEGER :: nnx, ndnc, ind, nnsh, info, nn, nnh, na
      INTEGER :: ifound, nap, ifpos, ifneg
      INTEGER :: nwann, nb
      INTEGER :: npoint2
      INTEGER, ALLOCATABLE :: nx2(:), ny2(:), nz2(:)
      INTEGER :: nzz, nyy, nxx
      INTEGER :: nsdim, irguide
      INTEGER :: nrguide, ncgfix, ncount
      LOGICAL :: lrguide, lcg
      REAL(dbl)  :: enmax
      REAL(dbl)  :: volc, voli, wtktot
      REAL(dbl)  :: epsilon, eta, eps, dnn0, dnn1, dist
      REAL(dbl)  :: bb1, bbn, factor, wbtot
      REAL(dbl)  :: asidemin, aside
      REAL(dbl)  :: func_om1, func_om2, func_om3, func_o
      REAL(dbl)  :: func_old1, func_old2, func_old3
      REAL(dbl)  :: sph00, sph1m1, sph10, sph11, sph2m2
      REAL(dbl)  :: sph2m1, sph20, sph21, sph22
      REAL(dbl)  :: rx, ry, rz, scalf
      REAL(dbl)  :: dist1, dist_pl, dist_cos
      REAL(dbl)  :: th_cos, th_sin, ph_cos, ph_sin, dist2, select
      REAL(dbl)  :: rre, rri, omt1, omt2, omt3, omiloc
      REAL(dbl)  :: func_del1, func_del2, func_del3, func0
      REAL(dbl)  :: gcnorm1, gcfac, gcnorm0, doda0
      REAL(dbl)  :: funca, func_del, eqc, eqb, eqa, alphamin, falphamin
      COMPLEX(dbl) :: catmp, cphi, calpha, cbeta
      COMPLEX(dbl) :: cfunc_exp1, cfunc_exp2, cfunc_exp3, cfunc_exp

      COMPLEX(dbl), ALLOCATABLE ::  ca(:,:,:) ! ca(dimwann,dimwann,nkpts)
      COMPLEX(dbl), ALLOCATABLE ::  cs(:,:,:) ! cs(dimwann,dimwann,nkpts)
      COMPLEX(dbl), ALLOCATABLE ::  cu(:,:,:) ! cu(dimwann,dimwann,nkpts)
      COMPLEX(dbl), ALLOCATABLE ::  cu0(:,:,:) ! cu0(dimwann,dimwann,nkpts)
      COMPLEX(dbl), ALLOCATABLE ::  csheet(:,:,:) ! csheet(dimwann,nkpts,nnmx)
      COMPLEX(dbl), ALLOCATABLE ::  cm(:,:,:,:) ! cm(dimwann,dimwann,nkpts,nnmx)
      COMPLEX(dbl), ALLOCATABLE ::  cm0(:,:,:,:) ! cm0(dimwann,dimwann,nkpts,nnmx)
      COMPLEX(dbl), ALLOCATABLE ::  cr(:,:,:,:) ! cr(dimwann,dimwann,nkpts,nnmx)
      COMPLEX(dbl), ALLOCATABLE ::  crt(:,:,:,:) ! crt(dimwann,dimwann,nkpts,nnmx)
      COMPLEX(dbl), ALLOCATABLE ::  cmtmp(:,:) ! cmtmp(dimwann,dimwann)
      COMPLEX(dbl), ALLOCATABLE ::  cdodq(:,:,:) ! cdodq(dimwann,dimwann,nkpts)
      COMPLEX(dbl), ALLOCATABLE ::  cdqkeep(:,:,:) ! cdqkeep(dimwann,dimwann,nkpts)
      COMPLEX(dbl), ALLOCATABLE ::  cdodq1(:,:,:) ! cdodq1(dimwann,dimwann,nkpts)
      COMPLEX(dbl), ALLOCATABLE ::  cdodq2(:,:,:) ! cdodq2(dimwann,dimwann,nkpts)
      COMPLEX(dbl), ALLOCATABLE ::  cdodq3(:,:,:) ! cdodq3(dimwann,dimwann,nkpts)
      COMPLEX(dbl), ALLOCATABLE ::  cdq(:,:,:) ! cdq(dimwann,dimwann,nkpts)
      COMPLEX(dbl), ALLOCATABLE ::  cz(:,:) ! cz(dimwann,dimwann)
      COMPLEX(dbl), ALLOCATABLE ::  cv1(:,:) ! cv1(dimwann,dimwann)
      COMPLEX(dbl), ALLOCATABLE ::  cv2(:,:) ! cv2(dimwann,dimwann)
      COMPLEX(dbl), ALLOCATABLE ::  cv3(:,:) ! cv3(dimwann,dimwann)
      COMPLEX(dbl), ALLOCATABLE ::  cpad1(:) ! cpad1(dimwann*dimwann)
      COMPLEX(dbl), ALLOCATABLE ::  cpad2(:) ! cpad2(dimwann*dimwann)
      COMPLEX(dbl), ALLOCATABLE ::  cw1(:) ! cw1(10*dimwann)
      COMPLEX(dbl), ALLOCATABLE ::  cw2(:) ! cw2(10*dimwann)
      REAL(dbl), ALLOCATABLE ::  singvd(:) !  singvd(dimwann)
      REAL(dbl), ALLOCATABLE ::  sheet(:,:,:) ! sheet(dimwann,nkpts,nnmx)
      REAL(dbl), ALLOCATABLE ::  rnkb(:,:,:) ! rnkb(dimwann,nkpts,nnmx)
      REAL(dbl), ALLOCATABLE ::  v1(:,:) ! v1(nnmx,nnmx)
      REAL(dbl), ALLOCATABLE ::  v2(:,:) ! v2(nnmx,nnmx)
      REAL(dbl), ALLOCATABLE ::  w1(:) !  w1(10*nnmx)

      INTEGER, ALLOCATABLE :: ndim(:) !  dimsingvd(nnmx),dimbk(3,nnmx),ndim(nnmx)
      INTEGER, ALLOCATABLE :: nkp_inv(:) ! l_wann(dimwann),m_wann(dimwann),nkp_inv(nkpts)
      INTEGER, ALLOCATABLE :: nnshell(:,:) ! nnshell(nkpts,nnmx)
      INTEGER, ALLOCATABLE ::  neigh(:,:) ! neigh(nkpts,nnmxh)
      REAL(dbl), ALLOCATABLE ::  dimsingvd(:), dimbk(:,:)
      REAL(dbl), ALLOCATABLE ::  wb(:,:), dnn(:) ! wb(nkpts,nnmx),dnn(nnmx)
      REAL(dbl), ALLOCATABLE ::  bk(:,:,:), rave(:,:), r2ave(:) !  bk(3,nkpts,nnmx), rave(3,dimwann), r2ave(dimwann)
      REAL(dbl), ALLOCATABLE ::  bka(:,:) !  bka(3,nnmxh)

      REAL(dbl), ALLOCATABLE :: rguide(:,:) ! rguide(3,dimwann)

      REAL(dbl) :: rpos1(3), rpos2(3)

      INTEGER, ALLOCATABLE :: nphimx1(:,:) ! nphimx1(3,dimwann)
      INTEGER, ALLOCATABLE :: nphimx2(:,:) ! nphimx2(3,dimwann)

      INTEGER :: ngdim(3)

      REAL(dbl), ALLOCATABLE ::  rphicmx1(:,:) ! rphicmx1(3,dimwann)
      REAL(dbl), ALLOCATABLE ::  rphicmx2(:,:) ! rphicmx2(3,dimwann)

      INTEGER, ALLOCATABLE :: nnlist(:,:), nntot(:) ! nnlist(nkpts,nnmx), nntot(nkpts)
      INTEGER, ALLOCATABLE :: nncell(:,:,:) ! nncell(3,nkpts,nnmx)

      REAL(dbl), ALLOCATABLE :: vkpr(:,:) ! vkpr(3,nkpts)
      REAL(dbl) :: vkpp(3)

      COMPLEX(dbl), ALLOCATABLE :: cwschur1(:), cwschur2(:) !  cwschur1(dimwann),cwschur2(10*dimwann)

      REAL(dbl), ALLOCATABLE :: cwschur3(:) ! cwschur3(dimwann)
      LOGICAL, ALLOCATABLE :: cwschur4(:) ! cwschur4(dimwann)

      COMPLEX(dbl), ALLOCATABLE ::  cptwr(:) ! cptwr(mplwv)

      REAL(dbl), ALLOCATABLE :: vkpt(:,:), wtkpt(:) ! vkpt(3,nkpts), wtkpt(nkpts)
      INTEGER, ALLOCATABLE :: nfile(:)         !  nfile(nkpts)
      COMPLEX(dbl), ALLOCATABLE :: cptwfp(:,:,:) !  cptwfp(mxddim+1,dimwann,nkpts)

      INTEGER, ALLOCATABLE :: nindpw(:,:) !  nindpw(mxddim,nkpts)
      INTEGER, ALLOCATABLE :: ninvpw(:,:) !  ninvpw(0:nplwv,nkpts)
      INTEGER, ALLOCATABLE :: nplwkp(:)   !  nplwkp(nkpts)

      INTEGER, ALLOCATABLE ::  lpctx(:) !  lpctx(ngx),lpcty(ngy),lpctz(ngz)
      INTEGER, ALLOCATABLE ::  lpcty(:) 
      INTEGER, ALLOCATABLE ::  lpctz(:) 

      REAL(dbl), ALLOCATABLE :: datake(:,:,:) ! datake(7,mxddim,nkpts)
      REAL(dbl), ALLOCATABLE :: dnlg(:,:,:)   ! dnlg(mxddim,3,nkpts)
      REAL(dbl), ALLOCATABLE :: dnlkg(:,:,:)  ! dnlkg(mxddim,0:3,nkpts)

      REAL(dbl) :: dirc(3,3), recc(3,3), dirl(3,3)
      REAL(dbl) :: diri(3,3), reci(3,3)
      REAL(dbl) :: avec(3,3), bvec(3,3)
      REAL(dbl) :: vcell, aminv(3), adot(3,3), bdot(3,3)

      INTEGER, ALLOCATABLE :: nphir(:) ! nphir(dimwann)

      CHARACTER( LEN = 2 ) :: nameat(mxdtyp)
      INTEGER :: ntype, natom(mxdtyp)
      REAL(dbl) :: rat(3,mxdatm,mxdtyp)
      REAL(dbl) :: posion(3,mxdatm,mxdtyp) ! posion(3,nions,ntype)
      REAL(dbl) :: poscart(3,mxdatm,mxdtyp) ! poscart(3,nions,ntype)

      REAL(dbl) :: alatt
      REAL(dbl) :: s(3)
      INTEGER :: nt, ja, nbandi
      INTEGER :: nk(3)
      INTEGER :: mxddim, mxdbnd
      INTEGER :: ngm

      INTEGER, ALLOCATABLE :: kgv(:,:)
      INTEGER :: mxdgve, nkpts1
      INTEGER :: i1, i2, i3
      INTEGER :: nsiz1_, nsiz2_, nsiz3_, ngk_, nbnd_ 

      LOGICAL :: lselect
      INTEGER :: nwork

      COMPLEX(dbl) :: ctmp1, ctmp2

      REAL(dbl) :: cclock
      EXTERNAL :: cclock
      REAL(dbl) :: s0, s1, s2, s3, s4, s5, sf
 
      INTEGER :: idum, rdum

!
! ... End declarations and dimensions
!
!=----------------------------------------------------------------------------=!


      s0 = cclock()

      WRITE (*,*)'                                                    '
      WRITE (*,*)'                                                    '
      WRITE (*,*)'   -------------------------------------------------'
      WRITE (*,*)'   =      Welcome to the Maximally Localized       ='
      WRITE (*,*)'   =      Generalized Wannier Functions code       ='
      WRITE (*,*)'   -------------------------------------------------'
      WRITE (*,*)'  '
      WRITE (*,*)'  '
      WRITE (*,*)'  '


!
! ...  Read input parameters from window.out
!

      CALL read_input()

      OPEN( UNIT=19, FILE='takeoff.dat', STATUS='OLD', FORM='UNFORMATTED' )

      READ(19) alatt
      READ(19) (avec(i,1),i=1,3)
      READ(19) (avec(i,2),i=1,3)
      READ(19) (avec(i,3),i=1,3)
      READ(19) ntype
      DO nt = 1, ntype
        READ(19) natom(nt), nameat(nt)
        DO ja=1, natom(nt)
          READ(19) (rat(i,ja,nt),i=1,3)
        END DO
      END DO

      READ(19) enmax, nbandi
      READ(19) (nk(i),i=1,3), (s(i),i=1,3)

      READ(19) rdum ! win_min, win_max, froz_min, froz_max, dimwann

      READ(19) rdum ! alpha, maxiter 
      READ(19) idum ! iphase
      READ(19) idum ! niter0, alphafix0
      READ(19) idum ! niter, alphafix, ncg
      READ(19) idum ! itrial, nshells

      !  ALLOCATE( nwhich(nshells) )
      READ(19) idum ! (nwhich(i),i=1,nshells)

      READ(19) nkpts, mxddim, mxdbnd
      READ(19) ngx, ngy, ngz, ngm

      ! ALLOCATE( gauss_typ( dimwann ) )
      ! ALLOCATE( rphiimx1( 3, dimwann ) )
      ! ALLOCATE( rphiimx2( 3, dimwann ) )
      ! ALLOCATE( l_wann( dimwann ) )
      ! ALLOCATE( m_wann( dimwann ) )
      ! ALLOCATE( ndir_wann( dimwann ) )
      ! ALLOCATE( rloc( dimwann ) )

      READ(19) idum ! gauss_typ(1:dimwann)
      READ(19) rdum ! rphiimx1(1:3,1:dimwann)
      READ(19) rdum ! rphiimx2(1:3,1:dimwann)
      READ(19) idum ! l_wann(1:dimwann)
      READ(19) idum ! m_wann(1:dimwann)
      READ(19) idum ! ndir_wann(1:dimwann)
      READ(19) rdum ! rloc(1:dimwann)

! ... Read grid information, and G-vectors

      READ(19) mxdgve 
      ALLOCATE( kgv( 3, mxdgve ) )
      READ(19) ( ( kgv(i,j), i=1,3 ), j=1,mxdgve )

      CLOSE(19)


      natom = natom
      enmax  = enmax * har

      CALL atomset( alatt, avec, ntype, natom, nameat, rat, mxdtyp, mxdatm )
      CALL latti( avec, bvec, vcell, bdot, aminv, adot )

      IPRINT = 1
      
      DO i = 1, 3
        DO j = 1, 3
          dirc(i,j) = avec(j,i) * bohr
        END DO
      END DO
      CALL bastr( dirc, recc, volc )

      DO i = 1, 3
        DO j = 1, 3
          diri(i,j) = avec(j,i) * bohr
        END DO
      END DO
      CALL bastr( diri, reci, voli )
 

      WRITE(*,*)' THE BASIS-SET USED WHEN PLACING ATOMS ( DIRC(I,J) ):'
      WRITE(*,*) ' '
      WRITE(*,101) ( ( dirc(i,j), i=1,3 ), j=1,3 )
      WRITE(*,*) ' '
      WRITE(*,*) ' THE RECIPROCAL SPACE BASIS ( RECC(I,J) ): '
      WRITE(*,*) ' '
      WRITE(*,101) ( ( recc(i,j), i=1,3 ), j=1,3 )
 101  FORMAT(1X,3F10.6)
      WRITE (*,*) ' '
      WRITE(*,*) ' NOTE: VECTORS A1 A2 A3 AND B1 B2 B3 ARE GIVEN IN *COLUMNS* '
      WRITE (*,*) ' '
      WRITE (*,*) ' VOLUME OF CURRENT UNIT CELL:', volc

      ng = 1

      WRITE (*,*) ' '
      DO nsp = 1 , ntype
        DO ni = 1 , natom(nsp)
          posion(:,ni,nsp) = rat(:,ni,nsp)/twopi
          WRITE (*,7004) ni, nsp, ( posion(i,ni,nsp), i=1,3 )
          ng = ng + 1
        END DO
      END DO
 7004 FORMAT( 2x, 'ION ', i3, ' TYPE ', i1, ' AT (',3f12.7,') (A_i UNITS)' )

      write(*,*) ' '
      DO nsp = 1 , ntype
        DO ni = 1 , natom(nsp)
          DO m = 1, 3
            poscart(m,ni,nsp) = 0.d0
            DO j=1,3
              poscart(m,ni,nsp) = poscart(m,ni,nsp) + posion(j,ni,nsp) * dirc(j,m)
            END DO
          END DO
          WRITE (*,7034) ni, nsp, ( poscart(i,ni,nsp), i=1,3 )
        END DO
      END DO
 7034 FORMAT( 2x, 'ION ', i3, ' TYPE ', i1, ' AT (',3F12.7,') (CARTESIAN)' )

 
 

      nkpts1 = nk(1)*nk(2)*nk(3)

      ALLOCATE( vkpt(3, nkpts) )
      ALLOCATE( wtkpt(nkpts) )
      ALLOCATE( nfile( nkpts ) )

      nkp = 0
      DO i1 = 0, nk(1)-1
        DO i2 = 0, nk(2)-1
          DO i3 = 0, nk(3)-1
            nkp = nkp + 1
            vkpt(1,nkp) = DBLE(i1)/DBLE(nk(1)) + s(1)
            vkpt(2,nkp) = DBLE(i2)/DBLE(nk(2)) + s(2)
            vkpt(3,nkp) = DBLE(i3)/DBLE(nk(3)) + s(3)
          END DO
        END DO
      END DO

      WRITE (*,*) ' '

      DO nkp = 1 , nkpts
        wtkpt(nkp) =  1.0d0/DBLE(nkpts)
        WRITE (*,7014) nkp, vkpt(1,nkp), vkpt(2,nkp), vkpt(3,nkp), wtkpt(nkp)
      END DO
 7014 FORMAT( '  SPECIAL K-POINT', i6, ':',3F10.5, '   WEIGHT =', F10.5 )

      wtktot = 0.d0
      DO nkp = 1 , nkpts
        wtktot = wtktot + wtkpt(nkp)
      END DO

! ... Do destinato a morire visto che legge da un file unico

      DO nkp = 1, nkpts
        nfile(nkp) = nkp
      END DO


      ALLOCATE( lpctx(ngx), lpcty(ngy), lpctz(ngz) )


! ... Initialize the loop counters lpctx,lpcty,lpctz that
!     label the number of the reciprocal lattice vectors in the x,y,z
!     directions, respectively. For the x direction the reciprocal lattice
!     vectors corresponding to the first,second,...,ngxth elements in all
!     of the reciprocal lattice arrays are 0,1,..,(ngx/2),-((ngx/2-1),..,-1
!     times the x reciprocal lattice vector recc(1,*)

      DO nx = 1 , (ngx/2)+1
        lpctx(nx)  = nx - 1
      END DO
      DO nx = (ngx/2)+2 , ngx
        lpctx(nx)  = nx - 1 - ngx
      END DO
      DO ny = 1 , (ngy/2)+1
        lpcty(ny)  = ny - 1
      END DO
      DO ny = (ngy/2)+2 , ngy
        lpcty(ny)  = ny - 1 - ngy
      END DO
      DO nz = 1 , (ngz/2)+1
        lpctz(nz)  = nz - 1
      END DO
      DO nz = (ngz/2)+2 , ngz
        lpctz(nz)  = nz - 1 - ngz
      END DO


! ... Subroutine genbtr calculate the g-vectors for each K_point
!     within the kinetic energy cutoff

      ALLOCATE( nindpw( mxddim, nkpts ) )

      nindpw = 0

      ALLOCATE( nplwkp( nkpts ) )
      ALLOCATE( datake( 7, mxddim, nkpts ) )
      ALLOCATE( dnlg( mxddim, 3, nkpts ) )
      ALLOCATE( dnlkg( mxddim, 0:3, nkpts ) )


      CALL genbtr( mxddim, ngx, ngy, ngz, nkpts, enmax, nindpw, nplwkp, vkpt,   &
           lpctx, lpcty, lpctz, datake, recc, reci, iprint, dnlg, dnlkg )

! ... First set all elements of ninvpw to point to the "extra" plane wave
!     (the mxddim+1 one) for which the wavefunction has been set to zero
!     by hand (see next loop, read wavefunctions), and then fill up ninvpw 
!     with those elements that point to plane-waves that are inside the cutoff
!     note that ninvpw has also a zero element, that also points to the
!     "extra" plane wave, corresponding to those plane waves that are not
!     inside the cutoff radius (i.e. that go between nplwkp(nkp)+1 and
!     mxddim) and thus for which there is no nindpw (i.e. its value has been
!     set to zero) and thus for which nindv(nindpw) points to the mxddim1+1
!     coefficient, that also has been set to zero

      nplwv = NGX*NGY*NGZ
      mplwv = NGX*NGY*(NGZ+1)
      ALLOCATE( ninvpw(0:nplwv,nkpts) )

      DO nkp = 1, nkpts
        DO np = 0, nplwv
          ninvpw(np,nkp) = mxddim + 1
        END DO
        DO np = 1, nplwkp(nkp)
          npoint = nindpw(np,nkp)
          ninvpw(npoint,nkp) = np
        END DO
        IF ( verbosity == 'high' ) THEN
          DO np = 1, mxddim
            WRITE(*,*) np, nindpw( np, nkp ), ninvpw( nindpw(np,nkp), nkp )
          END DO
        END IF
      END DO

! ... Read the energy eigenfunctions within the energy window at each K-point,
!     the subspace basis vectors, from file "intf.out"

      ALLOCATE( cptwfp( mxddim + 1, dimwann, nkpts ) )

      OPEN( UNIT=20, FILE='onfly.dat', STATUS='OLD' , FORM='UNFORMATTED' )


!     Starting k-loop

      K_POINTS:  DO nkp = 1, nkpts

        WRITE(*,*) 'nkp = ', nkp

        ! SIZE( cptwfp, 1), SIZE( cptwfp, 2), SIZE( cptwfp, 3), nplwkp( nkp ), nbande
        READ(20) nsiz1_, nsiz2_, nsiz3_, ngk_, nbnd_ 

        IF( ( SIZE( cptwfp, 1) < nsiz1_ ) .OR. ( SIZE( cptwfp, 2) < nsiz2_ ) .OR. ( SIZE( cptwfp, 3) < nsiz3_ ) ) THEN
          WRITE( *, * ) '*** READING cptwfp WRONG DIMENSIONS ***' 
          STOP
        END IF

        READ(20) ( ( cptwfp( np, iib, nkp ), np=1, nplwkp(nkp) ), iib=1, dimwann )

        DO iib = 1, dimwann
          DO np = nplwkp(nkp) + 1, mxddim + 1
            cptwfp(np, iib, nkp) = ( 0.d0, 0.d0 )
          END DO
        END DO

      END DO K_POINTS

      CLOSE(20)

      s1 = cclock()

      WRITE(*,*) ' '

!
!-------------------------------------------------------------------------
! ... Wannier Functions localization procedure
!

      iseed   = -1
      epsilon = 1.d0


      WRITE(*,*) ' GEOMETRY PRELIMS ----------------------------------'

      ! pass the k-points in cartesian coordinates

      ALLOCATE( vkpr(3,nkpts) )

      WRITE(*,*) ' '
      DO nkp = 1, nkpts
        DO i = 1, 3
          vkpr(i,nkp) = 0.d0
          DO j = 1, 3
            vkpr(i,nkp) = vkpr(i,nkp) + vkpt(j,nkp) * recc(j,i)
          END DO
        END DO 
        WRITE(*,7017) nkp, ( vkpr(i,nkp), i=1,3 ), wtkpt(nkp)
      END DO
 7017 FORMAT('  K-POINT (CARTESIAN)',i6,':',3F10.5,'   WEIGHT =', F10.5)

! ... Find the distance between k-point 1 and its nearest-neighbour shells
!     if we have only one k-point, the n-neighbours are its periodic images 

      eta = 99999999.d0
      eps = 0.000001
      dnn0 = 0.d0
      dnn1 = eta
      ndnntot = 0

! 
! AC & MBN (April 2002) generic k grid allowed
!
      ALLOCATE( dnn(nnmx) )

      DO nlist = 1, nnmx
        DO nkp = 1, nkpts
          DO l = -5, 5
            DO m = -5, 5
              DO n = -5, 5
                vkpp(1) = vkpr(1,nkp) + l*recc(1,1) + m*recc(2,1) + n*recc(3,1)
                vkpp(2) = vkpr(2,nkp) + l*recc(1,2) + m*recc(2,2) + n*recc(3,2)
                vkpp(3) = vkpr(3,nkp) + l*recc(1,3) + m*recc(2,3) + n*recc(3,3)
                dist = sqrt( (vkpr(1,1)-vkpp(1))**2 + (vkpr(2,1)-vkpp(2))**2 + (vkpr(3,1)-vkpp(3))**2 )
                IF ( ( dist > eps) .AND. ( dist > dnn0+eps ) ) dnn1 = min(dnn1,dist)
              END DO
            END DO
          END DO
        END DO
        IF ( dnn1 < eta-eps ) ndnntot = ndnntot + 1
        dnn(nlist) = dnn1
        dnn0 = dnn1
        dnn1 = eta
      END DO

      WRITE(*,*) ' '
      WRITE(*,*) ' Nearest-neighbour shells for k-point 1 are (in Angstrom^-1)'
      WRITE(*,*) ' '

      DO ndnn = 1, ndnntot
        WRITE(*,*) ndnn, dnn(ndnn)
      END DO
      WRITE(*,*) ' '

! ... Now build up the list of nearest-neighbour shells for each k-point.
!     nnlist(nkp,1...nnx) points to the nnx neighbours (ordered along increasing shells)
!     of the k-point nkp. nncell(i,nkp,nnth) tells us in which BZ is the nnth 
!     nearest-neighbour of the k-point nkp. Construct the nnx b-vectors that go from k-point
!     nkp to each neighbour bk(1:3,nkp,1...nnx).

      ALLOCATE ( nnshell(nkpts,nnmx) )
      ALLOCATE ( nnlist(nkpts,nnmx), nntot(nkpts) ) 
      ALLOCATE ( nncell(3,nkpts,nnmx) ) 
      ALLOCATE ( bk(3,nkpts,nnmx) ) 

      DO nkp = 1, nkpts
        nnx = 0
        DO ndnc = 1, nshells
          ndnn = nwhich(ndnc)
          nnshell(nkp,ndnn) = 0
          DO nkp2 = 1, nkpts
            DO l = -5, 5
              DO m = -5, 5
                DO n = -5, 5
                  vkpp(1) = vkpr(1,nkp2) + l*recc(1,1) + m*recc(2,1) + n*recc(3,1)
                  vkpp(2) = vkpr(2,nkp2) + l*recc(1,2) + m*recc(2,2) + n*recc(3,2)
                  vkpp(3) = vkpr(3,nkp2) + l*recc(1,3) + m*recc(2,3) + n*recc(3,3)
                  dist = sqrt( (vkpr(1,nkp)-vkpp(1))**2 + (vkpr(2,nkp)-vkpp(2))**2 + (vkpr(3,nkp)-vkpp(3))**2 )
                  IF ( ( dist >= dnn(ndnn)*0.9999 ) .AND. ( dist <= dnn(ndnn)*1.0001 ) )  THEN
                    nnx = nnx + 1   
                    nnshell(nkp,ndnn) = nnshell(nkp,ndnn) + 1
                    nnlist(nkp,nnx) = nkp2
                    nncell(1,nkp,nnx) = l
                    nncell(2,nkp,nnx) = m
                    nncell(3,nkp,nnx) = n
                    DO ind = 1, 3
                      bk(ind,nkp,nnx) = vkpp(ind) - vkpr(ind,nkp)
                    END DO
                  END IF
                END DO
              END DO
            END DO
          END DO
          IF ( nnshell(nkp,ndnn) <= 0 ) THEN
            WRITE(*,*) ' Shell is empty ! ', nkp, nnx, nnshell(nkp,ndnn), ndnn
            STOP
          END IF
          IF ( ( nnshell(nkp,ndnn) /= nnshell(1,ndnn) ) .AND. ( wtkpt(nkp) > 1d-10 ) ) THEN
            WRITE(*,*) ' Non uniform neighbours !', nkp, nnx, nnshell(nkp,ndnn), ndnn
            STOP
          END IF
        END DO
        IF ( nnx > nnmx ) THEN
          WRITE(*,*) ' Too many neighbours !'
          STOP
        END IF
        nntot(nkp) = nnx
      END DO

! ... Check that the moduli of the b-vectors inside a shell are all identical

      DO nkp = 1, nkpts
        nnx = 0
        DO ndnc = 1, nshells
          ndnn = nwhich(ndnc)
          DO nnsh = 1, nnshell(nkp,ndnn)
            bb1 = 0.d0
            bbn = 0.d0
            nnx = nnx + 1
            DO i = 1, 3
              bb1 = bb1 + bk(i,1,nnx)*bk(i,1,nnx)
              bbn = bbn + bk(i,nkp,nnx)*bk(i,nkp,nnx)
            END DO
            IF ( abs(sqrt(bb1) - sqrt(bbn) ) > eps ) THEN
              WRITE(*,*) ' Non-symmetric k-point neighbours !', bb1, bbn
              STOP
            END IF
          END DO
        END DO
      END DO

! ... Now find the dimensionality of each shell of neighbours

      ALLOCATE( dimsingvd(MAX(nnmx,3)), dimbk(3,nnmx), ndim(nnmx) )
      ALLOCATE( v1(nnmx,nnmx) )
      ALLOCATE( v2(nnmx,nnmx) )
      ALLOCATE( w1(10*nnmx) )

      nnx = 0
      DO ndnc = 1, nshells
        ndnn = nwhich(ndnc)
        ndim(ndnn) = 0
        DO nnsh = 1, nnshell(1,ndnn)
          nnx = nnx + 1
          DO i = 1, 3
            dimbk(i,nnsh) = bk(i,1,nnx)
          END Do
        END DO
        nnsh = nnshell(1,ndnn)
        IF( nnsh > nnmx ) &
          CALL errore(' wannier ',' nnsh too big ', nnsh )
        dimsingvd = 0.d0

        CALL dgesvd( 'A', 'A', 3, nnsh, dimbk, 3, dimsingvd, v1, 3, v2, nnsh, w1, 10*nnsh, info )

        IF ( info /= 0 ) THEN
          WRITE(*,*) 'Singular value decomposition dgesvd failed 1'
          STOP
        END IF
        DO nn = 1, nnsh
          IF ( ABS(dimsingvd(nn) ) > 1d-05 ) ndim(ndnn) = ndim(ndnn) + 1
        END DO
        factor = DBLE(ndim(ndnn))/nnshell(1,ndnn)
        WRITE(*,*) ndnn, ' shell '
        WRITE(*,*) ' '
        WRITE(*,*) ' dimensionality is        ', ndim(ndnn)
        WRITE(*,*) ' w_b weight is 1/b^2 times', factor
        WRITE(*,*) ' '
      END DO

      DEALLOCATE( v1 )
      DEALLOCATE( v2 )
      DEALLOCATE( w1 )

      ALLOCATE( wb(nkpts, nnmx) )

      DO nkp = 1, nkpts
        nnx = 0
        DO ndnc = 1, nshells
          ndnn = nwhich(ndnc)
          DO nnsh = 1, nnshell(nkp,ndnn)
            bb1 = 0.d0
            bbn = 0.d0
            nnx = nnx + 1
            DO i = 1, 3
              bb1 = bb1 + bk(i,1,nnx) * bk(i,1,nnx)
              bbn = bbn + bk(i,nkp,nnx) * bk(i,nkp,nnx)
            END DO
            wb(nkp,nnx) = DBLE( ndim(ndnn) ) / bbn / nnshell(nkp,ndnn)
          END DO
        END DO
      END DO

      wbtot = 0.d0
      nnx = 0
      DO ndnc = 1, nshells
        ndnn = nwhich(ndnc)
       DO nnsh = 1, nnshell(1,ndnn)
        nnx = nnx + 1
        wbtot = wbtot + wb(1,nnx)
       END DO
      END DO

! ... Now it regroups the bk, in order to have an index that runs along
!     consistent directions

      DO ndnc = 1, nshells
        ndnn = nwhich(ndnc)
       nnh = nnshell(1,ndnn)/2
       IF ( nnh*2 /= nnshell(1,ndnn) ) THEN
        WRITE (*,*) 'The number of neighbours in each shell must be even', ndnn, nnshell(1,ndnn)
        STOP
       END IF
      END DO
      nnh = nntot(1)/2

! ... Make list of bka vectors from neighbours of first k-point
!     delete any inverse vectors as you collect them

      ALLOCATE( bka(3,nnmxh) )

      na = 0
      DO nn = 1, nntot(1)
        ifound = 0
        IF ( na /= 0 ) THEN
          DO nap = 1, na
            CALL compar( bka(1,nap), bk(1,1,nn), ifpos, ifneg )
            IF ( ifneg == 1 ) ifound = 1
          END DO
        ENDIF
        IF ( ifound == 0 ) THEN    ! found new vector to add to set
          na = na + 1
          bka(1,na) = bk(1,1,nn)
          bka(2,na) = bk(2,1,nn)
          bka(3,na) = bk(3,1,nn)
        END IF
      END DO

      IF ( na /= nnh ) THEN
       WRITE (*,*) ' NA = ', na, ' NNH = ', nnh
       WRITE (*,*) ' Did not find right number of bk directions'
       STOP
      END IF

      WRITE (*,*) ' The vectors b_k are ', nntot(1)
      WRITE (*,*) ' '
      DO i = 1, nntot(1)
        WRITE (*, '(4f10.5)') ( bk(j,1,i), j=1,3 ), wb(1,i)
      END DO
      WRITE (*,*) ' '
      WRITE (*,*) ' The bk directions are ', nnh
      WRITE (*,*) ' '
      DO i = 1, nnh
        WRITE (*,'(3f10.5)') ( bka(j,i), j=1,3 )
      END DO
      WRITE (*,*) ' '


! ... Find index array

      ALLOCATE( neigh(nkpts,nnmxh) )

      DO nkp = 1, nkpts
        DO na = 1, nnh

! ...     first, zero the index array so we can check it gets filled

          neigh(nkp,na) = 0

! ...     now search through list of neighbours of this k-point

          DO nn = 1, nntot(nkp)
            CALL compar( bka(1,na), bk(1,nkp,nn), ifpos, ifneg )
            IF ( ifpos == 1 ) neigh(nkp,na) = nn
          END DO

! ...     check found

          IF ( neigh(nkp,na) == 0 ) THEN
            WRITE (*,*) ' nkp,na=', nkp, na
            STOP
          ENDIF
        END DO

      END DO
      WRITE(*,*) ' '


!     check we got it right -- should just see bka vectors
      IF ( verbosity == 'high' ) THEN
        WRITE(*,*) 'Check we got it right -- should just see bka vectors'
        DO nkp = 1, nkpts
          WRITE (*,*)
          DO na = 1, nnh
            nn = neigh(nkp,na)
            WRITE (*,'(3f10.5)') ( bk(j,nkp,nn), j=1,3 )
          END DO
        END DO
      END IF
 
! ... now it defines the centers of the localized functions (e.g. gaussians)
!     that are used to pick up the phases. this is system dependent 
!     rphiimx is the center of the Gaussians in relative coordinates (as posion)

       DO nwann = 1, dimwann

         rloc(nwann) = rloc(nwann) * bohr

         IF ( gauss_typ(nwann) == 1 ) THEN
!          values below don't really matter, since rphiimx2 is not used when  gauss_typ=1
           rphiimx2(1,nwann) = 0.d0
           rphiimx2(2,nwann) = 0.d0
           rphiimx2(3,nwann) = 0.d0
         ELSE IF( gauss_typ(nwann) == 2 ) THEN
           continue 
         ELSE
           WRITE(*,*) 'error in trial centers: wrong gauss_typ'
           STOP
         END IF

       END DO

       ALLOCATE( rphicmx1(3,dimwann) )
       ALLOCATE( rphicmx2(3,dimwann) )

       DO nb = 1, dimwann
         DO m = 1, 3
           rphicmx1(m,nb) = 0.d0
           rphicmx2(m,nb) = 0.d0
           DO j = 1, 3
             rphicmx1(m,nb) = rphicmx1(m,nb) + rphiimx1(j,nb) * dirc(j,m)
             rphicmx2(m,nb) = rphicmx2(m,nb) + rphiimx2(j,nb) * dirc(j,m)
           END DO
         END DO
       END DO

      ALLOCATE( nphimx1(3,dimwann) )
      ALLOCATE( nphimx2(3,dimwann) )

      ngdim(1) = ngx
      ngdim(2) = ngy
      ngdim(3) = ngz
      DO nwann = 1, dimwann
         DO m = 1, 3
           nphimx1(m,nwann) = INT( rphiimx1(m,nwann) * DBLE( ngdim(m) ) + 1.001 )
           nphimx2(m,nwann) = INT( rphiimx2(m,nwann) * DBLE( ngdim(m) ) + 1.001 )
         END DO
      END DO
       
      WRITE(*,*) ' Gaussian centers, in cartesian coordinates'
      WRITE(*,*) ' '
      DO nwann = 1, dimwann
        WRITE(*,'(a12,i4,3f10.5)') 'Gaussian 1: ', nwann, ( rphicmx1(m,nwann), m=1,3 )
        IF ( gauss_typ(nwann) == 2 ) WRITE(*,'(a12,i4,3f10.5)') 'Gaussian 2: ', nwann,( rphicmx2(m,nwann), m=1,3 )
      END DO

      WRITE(*,*) ' '
      WRITE(*,*)  ' Gaussian centers, in relative coordinates'
      WRITE(*,*) ' '
      DO nwann = 1, dimwann
        WRITE(*,'(a12,i4,3f10.5)') 'Gaussian 1: ', nwann, ( rphiimx1(m,nwann), m=1,3 )
        IF ( gauss_typ(nwann) == 2 )  WRITE(*,'(a12,i4,3f10.5)') 'Gaussian 2: ', nwann, ( rphiimx2(m,nwann), m=1,3 )
      END DO

      WRITE(*,*) ' '
      WRITE(*,*) ' Gaussian centers, nearest grid coordinates'
      WRITE(*,*) ' '
      DO nwann = 1, dimwann
        WRITE(*,'(a12,i6,3i4)')  'Gaussian 1: ', nwann, ( nphimx1(m,nwann), m=1,3 )
        IF ( gauss_typ(nwann) == 2 )  &
          WRITE(*,'(a12,i6,3i4)')  'Gaussian 2: ', nwann, ( nphimx2(m,nwann), m=1,3 )
      END DO

      ALLOCATE( nphir(dimwann) )

      DO nwann = 1, dimwann
        asidemin = 100000.d0 * rloc( nwann )
        DO j = 1, 3
          aside    = SQRT( dirc(j,1)**2 + dirc(j,2)**2 + dirc(j,3)**2 )
          asidemin = MIN( aside, asidemin )
        END DO
        nphir(nwann) = nint( 2 * ( rloc( nwann ) / asidemin ) * MIN( ngx, ngy, ngz ) )
        WRITE(*,*) ' '
        WRITE(*,'(a14,i3)') 'Trial orbital ', nwann
        WRITE(*,*) ' Gaussian width, in angstrom      ', rloc(nwann)
        WRITE(*,*) ' Half-width of integration region ', nphir(nwann)
        WRITE(*,*) ' '
        WRITE(*,8000)
8000    FORMAT (1x,'-------------------------------------------------------------------------------')
      END DO


! --- transform the wavefunctions (u_nk) into real space:
!
! NB! The conjg is introduced to bring in the standard convention
! for the Bloch theorem and the FFT.
! 
! For Castep: psi_nk(r) = u_nk(r) exp(-ikr) (Bloch theorem with a minus sign)
!             u_nk(r) = sum_G c_nk,G exp(-iGr)
!             c_nk,G = sum_r u_nk(r) exp(iGr)
!
! the transform with -i is usually called forward transform, 
! the one with i backward transform 
!
! in Castep the forward transform (i.e. -i) brings you from reciprocal to
! real space, and is called with an FFT with isign=+1 (in the fft3d 
! driver); the backward transform (with i) goes from real to reciprocal, 
! and is called with isign=-1 in fft3d
!
! Usually: psi_nk(r) = u_nk(r) exp(ikr) (standard Bloch theorem)
!          u_nk(r)= sum_G c_nk,G exp(iGr)
!          c_nk,G = sum_r u_nk(r) exp(-iGr)
! 
! and the forward transform (i.e. -i) brings you from real space to 
! reciprocal, and the backward from reciprocal to real space, as
! described in the intro to fft3d.f --- note that fft3d.f describes
! this usual convention, that is *not* actually used in Castep itself.
!
! talking about normalizations, nothing is needed in Castep when going
! from reciprocal to real (isign=+1, exp(-iGr)), while a 1/N factor is needed
! before using the output of the real to reciprocal transform (isign=-1,
! exp(iGr)). In the ideal world nothing would be needed in either
! directions (if our conventions were more standard); that is 
! why MJR's alpha's driver has to multiply internally by N the array before 
! a backward transform, since that Castep inevitably postprocesses it,
! multiplying it by 1/N.  
!
! check what the scalar products in real and reciprocal space do,
! and thus what is the charge density in real and reciprocal space

! INVERSION SYMMETRY IS ALL TO BE SET UP IF WE ARE USING THE TWO MESHES
! RESORT OUT ALL NKPH, NKPTS, NKPTS, nkpts
!     do nkp=1,nkpts
!      do nb=1,dimwann
!       do m=1,mplwv
!        cptwr(m,nb,nkp)=(0.,0.)
!        cwork2(m)=(0.,0.)
!       enddo
!       do m=1,nplwkp(nkp)
!        cptwr(nindpw(m,nkp),nb,nkp)=cptwfp(m,nb,nkp)
!       enddo
!      call fft3d(cptwr(1,nb,nkp),cwork2,ngptar,1)
!      end do
!     end do
!
!     ncheck=0
!     if(invsymm == 1) then
!      do nkp=nkpts+1,nkpts
!       do n=1,nkpts
!        call compar(vkpt(1,nkp),vkpt(1,n),ifpos,ifneg)
!        if (ifneg == 1) then
!         ncheck=ncheck+1
!         write(*,'(2i4,x3f12.5,x,3f12.5)') nkp,n,(vkpt(i,nkp),i=1,3),
!    &                                 (vkpt(i,n),i=1,3)
!         nkp_inv(nkp)=n
!         nkp_inv(n)=nkp
!         goto 1010
!        end if
!       end do
!1010    continue
!       write(*,*) 'nkp, nkp_inv'
!       do nb=1,dimwann
!        write(*,*) nkp,nkp_inv(nkp)
!        do m=1,mplwv
!         cptwr(m,nb,nkp)=conjg(cptwr(m,nb,nkp_inv(nkp)))
!        end do
!       end do
!      end do
!      if (ncheck /= nkpts) then
!       write(*,*) "ERROR: not enough inversion-related k-points"
!       stop
!      end if
!     end if
 
! so, here is the conjg. Think again on this; what we are doing,
! is going to psi_nk*=u_nk* exp(ikr)
!
!     do nkp=1,nkpts
!      do nb=1,dimwann
!       do m=1,mplwv
!        cptwr(m,nb,nkp)=conjg(cptwr(m,nb,nkp))
!       enddo
!      end do
!     end do
 
! calculate cm(i,j,nkp,nn)=<u_i k|u_j k+dk> (keeping into account 
! that if k+dk is outside (or should be outside) the first BZ it must be 
! brought from there (or there)
! with a exp(-iG.r) factor (given the proper convention for the sign)
! for a standard, non-Castep convention:
! psi_nk=psi_nk+G -> u_nk exp(ikr)=u_nk+G exp(ikr) exp (iGr) ->
!                    u_nk+G = u_nk exp(-iGr)
!
! Now, we are using the Castep u_nk, so the conjugation to go to the
! standard Bloch formalism (and that was done in real space above) 
! implies that we deal with v_nk=u_nk*=sum_G c_nk,G* exp(iGr).
! Additionally, we might have a exp (iG_0r) that has to be introduced,
! if we need a nkp2 that lies outside the BZ. In our reciprocal space
! products, that means that we have <v_m,k1|v_n,k2>=\sum_G1,G2 c_m,k1,G1
! c_n,k2,G2* \int exp [i(G2-G1-G0)r], if G0 is the vector that, say,
! brings a k2 inside the BZ just outside it, to be a neighbour of a 
! k1 that lies just inside the boundary (and the u are now in standard
! Bloch notation). The integral gives a delta, and so we take G2s that
! are G1s+G0, i.e. nx+nncell, etc...

      ALLOCATE( cm(dimwann,dimwann,nkpts,nnmx) )
      cm(:,:,:,:) = (0.d0, 0.d0)
      ALLOCATE( nx2(ngx) )
      ALLOCATE( ny2(ngy) )
      ALLOCATE( nz2(ngz) )

      DO nkp = 1, nkpts
        DO nn = 1, nntot(nkp)
          nkp2 = nnlist(nkp,nn)

          ! set up indices
          DO nx = 1, ngx
            nx2(nx) = nx + nncell(1,nkp,nn)
            IF( nx2(nx) < 1 ) nx2(nx) = nx2(nx) + ngx
            IF( nx2(nx) > ngx ) nx2(nx) = nx2(nx) - ngx
          END DO
          DO ny = 1, ngy
            ny2(ny) = ny + nncell(2,nkp,nn)
            IF( ny2(ny) < 1 ) ny2(ny) = ny2(ny) + ngy
            IF( ny2(ny) > ngy ) ny2(ny) = ny2(ny) - ngy
            ny2(ny) = (ny2(ny) - 1) * ngx
          END DO
          DO nz = 1, ngz
            nz2(nz) = nz + nncell(3,nkp,nn)
            IF( nz2(nz) < 1 ) nz2(nz) = nz2(nz) + ngz
            IF( nz2(nz) > ngz ) nz2(nz) = nz2(nz) - ngz
            nz2(nz) = (nz2(nz) - 1) * ngx * ngy
          END DO

          npoint = 0
          DO nz = 1, ngz
            DO ny = 1, ngy
              DO nx = 1 , ngx
                npoint = npoint + 1
                npoint2 = nx2(nx) + ny2(ny) + nz2(nz)

                DO j = 1, dimwann
                  cm(1:dimwann, j, nkp, nn) = &
                      cm(1:dimwann, j, nkp, nn) + &
                      cptwfp(ninvpw(npoint,nkp), 1:dimwann ,nkp) * &
                      CONJG( cptwfp(ninvpw(npoint2,nkp2), j, nkp2) )
                END DO

              END DO
            END DO
          END DO

        END DO
      END DO

      DEALLOCATE( nx2 )
      DEALLOCATE( ny2 )
      DEALLOCATE( nz2 )

      ALLOCATE( csheet(dimwann,nkpts,nnmx) )
      ALLOCATE( sheet(dimwann,nkpts,nnmx) )

      DO nkp = 1, nkpts
        DO nb = 1, dimwann
          DO nn = 1, nntot(nkp)
            sheet(nb,nkp,nn)  = 0.d0 
            csheet(nb,nkp,nn) = exp(ci*sheet(nb,nkp,nn))
            ! WRITE(*,fmt="(3I5,2F18.12)" ) nb, nkp, nn, csheet(nb,nkp,nn)
          END DO
        END DO
      END DO

! ... Now calculate the average positions of the Wanns.

      ALLOCATE( rave(3,dimwann), r2ave(dimwann) )

      CALL omega( dimwann, nkpts, nkpts, nntot(1), nnmx, nnlist(1,1), bk(1,1,1), wb(1,1),   &
                  cm(1,1,1,1), csheet(1,1,1), sheet(1,1,1), rave(1,1), r2ave(1), func_om1,  &
                  func_om2, func_om3, func_o )
      func_old1 = func_om1
      func_old2 = func_om2
      func_old3 = func_om3

! ... Now pick up a consistent phase for the u_nk (the initial ones
!     are provided by the ab-initio code, and so have almost random rotations)
     
      sph00 = 1.d0/sqrt( 2.d0 * twopi )
      sph1m1 = sqrt( 1.5 / twopi )
      sph10 = sqrt( 1.5 / twopi )
      sph11 = sqrt( 1.5 / twopi )
      sph2m2 = sqrt( 15.d0 / 2.d0 / twopi )
      sph2m1 = sqrt( 15.d0 / 2.d0 / twopi )
      sph20 = sqrt(  5.d0 / 8.d0 / twopi )
      sph21 = sqrt( 15.d0 / 2.d0 / twopi )
      sph22 = sqrt( 15.d0 / 2.d0 / twopi )

      ALLOCATE( cptwr(mplwv) )
      ALLOCATE( ca(dimwann,dimwann,nkpts) )

      DO nkp = 1, nkpts

        DO nb = 1, dimwann

          cptwr(:) = czero
          DO m = 1, nplwkp(nkp)
            cptwr(nindpw(m,nkp)) = cptwfp(m,nb,nkp)
          END DO

          ! write(6,*) 'DEBUG: ', SUM(cptwr(1:mplwv))
          CALL cfft3d( cptwr, ngx, ngy, ngz, ngx, ngy, ngz, -1 )

          DO m = 1, mplwv
            cptwr(m) = conjg(cptwr(m)) * ngx * ngy * ngz
          END DO

! ...     ca: the phase <u_nk(r) exp(ikr) | phi_nwann(r)>, given a real space
!         localized function phi_nwann (e.g. a gaussian centered on the bond)
!         note that the Castep convention would require |psi_nk>=|u_nk exp(-ikr)>
!         but we have adopted the standard convention in entering this part
!         of the program
!
! ...     nxx, nyy, nzz span a parallelogram in the real space mesh, of side
!         2*nphir, and centered around the maximum of phi_i, nphimax(i,1:3)
!
! ...     nx ny nz are the nxx nyy nzz brought back to the unit cell in
!         which u_nk(r)=cptwr is represented

          DO nwann = 1, dimwann

            ca(nb,nwann,nkp) = ( 0.d0,0.d0 )

! ...       First gaussian

            DO nzz = nphimx1(3,nwann) - nphir(nwann),  nphimx1(3,nwann) + nphir(nwann)
              nz = MOD(nzz,ngz)
              IF ( nz < 1 ) nz = nz + ngz
              DO nyy = nphimx1(2,nwann) - nphir(nwann),  nphimx1(2,nwann) + nphir(nwann)
                ny = MOD(nyy,ngy)
                IF( ny  <  1 ) ny = ny + ngy
                DO nxx = nphimx1(1,nwann) - nphir(nwann), nphimx1(1,nwann) +  nphir(nwann)
                  nx = MOD(nxx,ngx)
                  IF (nx < 1) nx = nx + ngx

! ...               Here it calculates <exp(i*k.r) u_nk(r)|

                  rx = DBLE(nxx-1)/DBLE(ngx)
                  ry = DBLE(nyy-1)/DBLE(ngy)
                  rz = DBLE(nzz-1)/DBLE(ngz)
                  scalf = vkpt(1,nkp) * rx + vkpt(2,nkp) * ry + vkpt(3,nkp) * rz
                  npoint = nx + (ny-1) * ngx + (nz-1) * ngy * ngx
                  catmp = CONJG( EXP( citpi*scalf ) * cptwr(npoint) )
       
                  DO m = 1, 3
                    rpos1(m) = ( rx - rphiimx1(1,nwann) ) * dirc(1,m) +    &
                               ( ry - rphiimx1(2,nwann) ) * dirc(2,m) +    &
                               ( rz - rphiimx1(3,nwann) ) * dirc(3,m)
                  END DO
  
                  dist1 = 0.d0
                  DO m = 1, 3
                    dist1 = dist1 + rpos1(m)**2
                  END DO
                  dist1 = SQRT(dist1)

! ...               The phi_nwann are defined here as gaussians centered on the max of the
!                   bond. note that the distance from the max of the bond has to be
!                   in cartesian coordinates (neglecting a factor 2*pi/a_0).
 
! ...               positive gaussian
 
                  cphi = CMPLX( EXP( -(dist1/rloc(nwann))**2 ), 0.d0 )

                  IF ( gauss_typ(nwann) == 1 ) then
 
                    IF ( ndir_wann(nwann) == 3 ) THEN
                      dist_pl = SQRT( rpos1(1)**2 + rpos1(2)**2 )
                      dist_cos = rpos1(3)
                    ELSE IF ( ndir_wann(nwann) == 2 ) THEN
                      dist_pl = SQRT( rpos1(1)**2 + rpos1(3)**2 )
                      dist_cos = rpos1(2)
                    ELSE IF ( ndir_wann(nwann) == 1 ) THEN
                      dist_pl = SQRT( rpos1(2)**2 + rpos1(3)**2 )
                      dist_cos = rpos1(1)
                    ELSE
                      WRITE (*,*) 'ERROR: Wrong z-direction'
                      STOP
                    END IF

! ...               If rpos is on the origin, or on the z axis, I give arbitrary
!                   values to cos/sin of theta, or of phi, respectively

                    IF ( ABS(dist1) <= 1.e-10 ) THEN
                      th_cos = 0.d0
                      th_sin = 0.d0
                    ELSE
                      th_cos = dist_cos/dist1
                      th_sin = dist_pl/dist1
                    END IF
 
                    IF ( ABS(dist_pl) <= 1.e-10 ) THEN
                      ph_cos = 0.d0
                      ph_sin = 0.d0
                    ELSE
                      IF ( ndir_wann(nwann) == 3 ) THEN
                        ph_cos = rpos1(1)/dist_pl
                        ph_sin = rpos1(2)/dist_pl
                      ELSE IF ( ndir_wann(nwann) == 2 ) THEN
                        ph_cos = rpos1(3)/dist_pl
                        ph_sin = rpos1(1)/dist_pl
                      ELSE 
                        ph_cos = rpos1(2)/dist_pl
                        ph_sin = rpos1(3)/dist_pl
                      END IF
                    END IF

                    IF ( l_wann(nwann) == 2 ) THEN

                      IF ( m_wann(nwann) == -2 ) THEN
                        cphi = sph2m2 * cphi * (th_sin**2) * ( ph_cos**2 - ph_sin**2 )
                      ELSE IF ( m_wann(nwann) == -1 ) THEN
                        cphi = sph2m1 * cphi * th_sin * th_cos * ph_cos
                      ELSE IF ( m_wann(nwann) == 0 ) THEN
                        cphi = sph20 * cphi * ( 3.0 * th_cos**2 - 1.0 )
                      ELSE IF ( m_wann(nwann) == 1 ) THEN
                        cphi = sph21 * cphi * th_sin * th_cos * ph_sin
                      ELSE IF ( m_wann(nwann) == 2 ) THEN
                        cphi = sph22 * cphi * (th_sin**2) * 2.0 * ph_sin * ph_cos
                      ELSE 
                        WRITE (*,*) 'ERROR: check the spherical harmonics'
                        STOP
                      END IF

                    ELSE IF ( l_wann(nwann) == 1 ) THEN

                      IF ( m_wann(nwann) == -1 ) THEN
                        cphi = sph1m1 * cphi * th_sin * ph_cos
                      ELSE IF ( m_wann(nwann) == 0 ) THEN
                        cphi = sph10 * cphi * th_cos
                      ELSE IF ( m_wann(nwann) == 1 ) THEN
                        cphi = sph11 * cphi * th_sin * ph_sin
                      ELSE 
                        WRITE(*,*) 'ERROR: check the spherical harmonics'
                        STOP
                      END IF

                    ELSE IF ( l_wann(nwann) == 0 ) THEN

                      cphi = sph00 * cphi

! ...                 sp^3 orbitals
                    ELSE If(l_wann(nwann) == -1) THEN
   
                      IF ( m_wann(nwann) == 1 ) THEN 
! ...                   sp^3 along 111 direction if ndir_wann(nwann)=3
                        cphi = cphi * ( sph00 + sph1m1*th_sin*ph_cos + sph11*th_sin*ph_sin + sph10*th_cos )/2.0d0

                      ELSE IF ( m_wann(nwann) == 2 ) THEN
! ...                   sp^3 along 1,-1,-1 direction if ndir_wann(nwann)=3
                        cphi = cphi * ( sph00 + sph1m1*th_sin*ph_cos - sph11*th_sin*ph_sin - sph10*th_cos )/2.0d0

                      ELSE IF ( m_wann(nwann) == 3 ) THEN
! ...                   sp^3 along -1,1,-1 direction if ndir_wann(nwann)=3
                        cphi = cphi * ( sph00 - sph1m1*th_sin*ph_cos + sph11*th_sin*ph_sin - sph10*th_cos )/2.0d0

                      ELSE IF ( m_wann(nwann) == 4 ) THEN
! ...                   sp^3 along -1,-1,1 direction if ndir_wann(nwann)=3
                        cphi = cphi * ( sph00 - sph1m1*th_sin*ph_cos - sph11*th_sin*ph_sin + sph10*th_cos )/2.0d0

                      ELSE IF ( m_wann(nwann) == -1 ) THEN 
! ...                   sp^3 along -1,-1,-1 direction if ndir_wann(nwann)=3
                        cphi = cphi * ( sph00 - sph1m1*th_sin*ph_cos - sph11*th_sin*ph_sin - sph10*th_cos )/2.0d0

                      ELSE IF ( m_wann(nwann) == -2 ) THEN
! ...                   sp^3 along -1,1,1 direction if ndir_wann(nwann)=3
                        cphi = cphi * ( sph00 - sph1m1*th_sin*ph_cos + sph11*th_sin*ph_sin + sph10*th_cos )/2.0d0

                      ELSE IF ( m_wann(nwann) == -3 ) THEN
! ...                   sp^3 along 1,-1,1 direction if ndir_wann(nwann)=3
                        cphi = cphi * ( sph00 + sph1m1*th_sin*ph_cos - sph11*th_sin*ph_sin + sph10*th_cos )/2.0d0

                      ELSE IF ( m_wann(nwann) == -4 ) THEN
! ...                   sp^3 along 1,1,-1 direction if ndir_wann(nwann)=3
                        cphi = cphi * ( sph00 + sph1m1*th_sin*ph_cos + sph11*th_sin*ph_sin - sph10*th_cos )/2.0d0

                      ELSE
                        STOP 'ERROR in sp^3 hybrid gaussian: check m_wann'
                      END IF

                    ELSE 
                      WRITE (*,*) 'ERROR: check the spherical harmonics'
                      STOP
                    END IF

                  END IF ! orbital is of type 1
                  ca(nb,nwann,nkp)=ca(nb,nwann,nkp)+catmp*cphi
 
                END DO
              END DO
            END DO



! ...       Second gaussian
            IF ( gauss_typ(nwann) == 2 ) THEN

              DO nzz = nphimx2(3,nwann) - nphir(nwann), nphimx2(3,nwann) + nphir(nwann)
                nz = MOD(nzz,ngz)
                IF ( nz < 1 ) nz = nz + ngz
                DO nyy = nphimx2(2,nwann) - nphir(nwann), nphimx2(2,nwann) + nphir(nwann)
                  ny = MOD(nyy,ngy)
                  IF ( ny < 1 ) ny = ny + ngy
                  DO nxx = nphimx2(1,nwann) - nphir(nwann), nphimx2(1,nwann) + nphir(nwann)
                    nx = MOD(nxx,ngx)
                    IF ( nx < 1 ) nx = nx + ngx

! ...               Here it calculates <exp(i*k.r) u_nk(r)|

                    rx = DBLE(nxx-1)/DBLE(ngx)
                    ry = DBLE(nyy-1)/DBLE(ngy)
                    rz = DBLE(nzz-1)/DBLE(ngz)
                    scalf = vkpt(1,nkp)*rx +  vkpt(2,nkp)*ry + vkpt(3,nkp)*rz
                    npoint = nx + (ny-1)*ngx + (nz-1)*ngy*ngx
                    catmp = CONJG(EXP(citpi*scalf)*cptwr(npoint))
              
                    DO m = 1, 3
                      rpos2(m) = (rx-rphiimx2(1,nwann)) * dirc(1,m) +          &
                                 (ry-rphiimx2(2,nwann)) * dirc(2,m) +          &
                                 (rz-rphiimx2(3,nwann)) * dirc(3,m)
                    END DO

                    dist2 = 0.d0
                    DO m = 1, 3
                      dist2 = dist2 + rpos2(m)**2
                    END DO
                    dist2 = SQRT(dist2)

! ...               The phi_nwann are defined here as gaussians centered on the max of the
!                   bond. note that the distance from the max of the bond has to be
!                   in cartesian coordinates (neglecting a factor 2*pi/a_0).
 
! ...               Negative gaussian
 
                    cphi = - CMPLX( EXP( -( dist2/rloc(nwann))**2 ), 0.d0 )

                    ca(nb,nwann,nkp) = ca(nb,nwann,nkp) + catmp*cphi

                  END DO ! nxx
                END DO  ! nyy
              END DO   ! nzz

            END IF ! gauss_typ=2

          END DO ! nwann

        END DO ! nb

      
        WRITE(*,*) '  '
        WRITE(*,'(a18,i4)') ' Matrix A, k-point', nkp
        WRITE(*,*) '  '
        IF ( dimwann <= 8 ) THEN
          DO i = 1, dimwann
            WRITE(*,'(8(0pe10.2))') ( ca(i,j,nkp), j=1,dimwann )
          END DO
        END IF

      END DO  !END k loop

      write(*,*) ' '
      write(*,8000)

      DEALLOCATE( cptwr )



! ... Now I calculate the transformation matrix CU = CS^(-1/2).CA,
!     where CS = CA.CA^\dagger. CS is diagonalized with a Schur
!     factorization, to be on the safe side of numerical stability.

! ... From LAPACK:
!     ZGEES computes for an N-by-N complex nonsymmetric matrix Y, the eigen-
!     values, the Schur form T, and, optionally, the matrix of Schur vectors Z.
!     This gives the Schur factorization Y = Z*T*(Z**H).
!     Optionally, it also orders the eigenvalues on the diagonal of the Schur
!     form so that selected eigenvalues are at the top left.  The leading columns
!     of Z then form an orthonormal basis for the invariant subspace correspond-
!     ing to the selected eigenvalues.
 
! ... A complex matrix is in Schur form if it is upper triangular.

      ALLOCATE( cs(dimwann,dimwann,nkpts) )

      nwork = dimwann * 10
      ALLOCATE( cwschur1(dimwann), cwschur2( nwork ) )
      ALLOCATE( cz(dimwann, dimwann) )
      ALLOCATE( cwschur3(dimwann) )
      ALLOCATE( cwschur4(dimwann) )
      ALLOCATE( cu(dimwann,dimwann,nkpts) )

      DO nkp = 1, nkpts

        DO na = 1, dimwann
          DO nb = 1, dimwann
            cs(na,nb,nkp) = ( 0.d0, 0.d0 )
            DO ni = 1, dimwann
              cs(na,nb,nkp) = cs(na,nb,nkp) + ca(na,ni,nkp) * CONJG( ca(nb,ni,nkp) )
            END DO
           END DO
        END DO

        IF ( verbosity == 'high' ) THEN
          IF (nkp == 1) THEN
            WRITE(*,*) '  '
            WRITE(*,'(a18,i4)') ' Matrix S before zgees, k-point',nkp
            WRITE(*,*) '  '
            IF ( dimwann <= 8 ) THEN
              DO i = 1, dimwann
                write(*,'(8(0pe10.2))') ( cs(i,j,nkp), j=1,dimwann )
              END DO
            END IF
          END IF
        END IF

        CALL zgees( 'V', 'N', lselect, dimwann, cs(1,1,nkp), dimwann, nsdim,            &
             cwschur1, cz, dimwann, cwschur2, nwork, cwschur3, cwschur4, info )

        DO i=1,dimwann
          DO j=1,dimwann
            cs(i,j,nkp)=(0.0,0.0)
            DO m=1,dimwann
              cs(i,j,nkp) = cs(i,j,nkp) + cz(i,m) * ( 1.d0/SQRT( REAL( cwschur1(m) ) ) ) * CONJG( cz(j,m) )
            END DO
          END DO
        END DO

        IF ( verbosity == 'high' ) THEN
          IF (nkp == 1) THEN
            WRITE(*,*) '  '
            WRITE(*,'(a18,i4)') ' Matrix S after zgees, k-point',nkp
            WRITE(*,*) '  '
            IF ( dimwann <= 8 ) THEN
              DO i = 1, dimwann
                write(*,'(8(0pe10.2))') ( cs(i,j,nkp), j=1,dimwann )
              END DO
            END IF
          END IF
        END IF

        DO i = 1, dimwann
          DO j = 1, dimwann
            cu(i,j,nkp) = ( 0.d0, 0.d0 )
            DO l = 1, dimwann
              cu(i,j,nkp) = cu(i,j,nkp) + cs(i,l,nkp) * ca(l,j,nkp)
            END DO
          END DO
        END DO

        IF ( verbosity == 'high' ) THEN
          WRITE (*,*) '  '
          WRITE (*,'(a18,i4)') ' Matrix U, k-point', nkp
          WRITE (*,*) '  '
          DO i = 1, dimwann
            WRITE(*,'(2f9.5,2x,2f9.5,2x,2f9.5,2x,2f9.5)') ( cu(i,j,nkp), j=1,dimwann )
          END DO
! ...     unitariety is checked
          WRITE (*,*) ' '
          WRITE (*,*) '*** check unitariery of U matrix ***'
          DO i = 1, dimwann
            DO j = 1, dimwann
              ctmp1 = ( 0.d0, 0.d0 )
              ctmp2 = ( 0.d0, 0.d0 )
              DO m = 1, dimwann
                ctmp1 = ctmp1 + cu(i,m,nkp) * CONJG( cu(j,m,nkp) )
                ctmp2 = ctmp2 + cu(m,j,nkp) * CONJG( cu(m,i,nkp) )
              END DO
              WRITE (*,'(2i4,4f10.7)') i, j, ctmp1, ctmp2
            END DO
          END DO
        END IF

        IF (iphase /= 1) THEN

          IF ( iphase == 2 ) THEN
! ...       check what happens if the heuristic phase is taken away
            WRITE (*,*) 'NB: phase is taken away'
            DO I = 1, dimwann
              DO J = 1, dimwann
                cu(i,j,nkp) = ( 0.d0, 0.d0 )
                IF ( i == j ) cu(i,j,nkp) = ( 1.d0, 0.d0 )
              END DO
            END DO
          ELSE
! ...       check what happens if a random phase is given
            WRITE(*,*) 'NB: RANDOM phase is given'
            DO m = 1, dimwann
              DO n = m, dimwann
                rre = ranf(iseed) * 10.d0 - 5.d0
                rri = ranf(iseed) * 10.d0 - 5.d0
                cu(m,n,nkp) = epsilon * CMPLX(rre,rri)
                cu(n,m,nkp) = -CONJG( cu(m,n,nkp) )
                IF ( m == n ) cu(n,m,nkp) = CMPLX( 0.d0, AIMAG( cu(m,n,nkp) ) )
              END DO
            END DO
            CALL zgees( 'V', 'N', select, dimwann, cu(1,1,nkp), dimwann, nsdim,            &
                 cwschur1, cz, dimwann, cwschur2, SIZE(cwschur2), cwschur3, cwschur4, info )
            DO i = 1, dimwann
              DO j = 1, dimwann
                cu(i,j,nkp) = ( 0.d0, 0.d0 )
                DO m = 1, dimwann
                  cu(i,j,nkp) = cu(i,j,nkp) + cz(i,m) * ( EXP( cwschur1(m) ) ) * CONJG( cz(j,m) )
                END DO
              END DO
            END DO
          END IF

        END IF

        IF ( verbosity == 'high' ) THEN
          WRITE (*,*) '  '
          WRITE (*,'(a18,i4)') ' Matrix U after zgees, k-point',nkp
          WRITE (*,*) '  '
          DO i = 1, dimwann
            WRITE(*,'(2f9.5,2x,2f9.5,2x,2f9.5,2x,2f9.5)') ( cu(i,j,nkp), j=1,dimwann )
          END DO
        END IF

      END DO  ! k point loop


      ! STOP 'qui'
    
      ALLOCATE( cmtmp(dimwann,dimwann) )

! ... So now we have the U's that rotate the wavefunctions at each k-point.
!     the matrix elements M_ij have also to be updated 

!     write (50,*) ((((cm(i,j,k,l), i=1,dimwann), j=1,dimwann), k=1,nkpts), l=1,nnmx)
      DO nkp = 1, nkpts

        DO nn = 1, nntot(nkp)
          nkp2 = nnlist(nkp,nn)
          DO i = 1, dimwann
            DO j = 1 ,dimwann
              cmtmp(i,j) = ( 0.d0, 0.d0 )
              DO m = 1, dimwann
                DO n = 1, dimwann
                  cmtmp(i,j) = cmtmp(i,j) + CONJG(cu(m,i,nkp)) * cu(n,j,nkp2) * cm(m,n,nkp,nn)
                END DO
              END DO
            END DO
          END DO
          DO i = 1, dimwann
            DO j = 1, dimwann
              cm(i,j,nkp,nn) = cmtmp(i,j)
            END DO
          END DO
        END DO

      END DO
!     write (50,*) '----****----'
!     write (50,*) ((((cm(i,j,k,l), i=1,dimwann), j=1,dimwann), k=1,nkpts), l=1,nnmx)


! ... Singular value decomposition

      nwork = dimwann * 10
      ALLOCATE( singvd(dimwann) )
      ALLOCATE( cv1(dimwann,dimwann) )
      ALLOCATE( cv2(dimwann,dimwann) )
      ALLOCATE( cv3(dimwann,dimwann) )
      ALLOCATE( cw1( nwork ) )
      ALLOCATE( cw2( nwork ) )
 
      ! OPEN( 9, file='omega_i.local', status='unknown' )

      omt1 = 0.d0
      omt2 = 0.d0
      omt3 = 0.d0

      DO nkp = 1, nkpts
        omiloc = 0.d0
        DO nn = 1, nntot(nkp)
          DO na = 1, dimwann
            DO nb = 1, dimwann
              cmtmp(na,nb) = cm(na,nb,nkp,nn)
            END DO
          END DO
          WRITE (*,*) 'dimwann=', dimwann, ' nwork=', nwork
!         write (50,*) '----****----'
!         write (50,*) ((cmtmp(i,j), i=1,dimwann), j=1,dimwann)
          CALL zgesvd( 'A', 'A', dimwann, dimwann, cmtmp, dimwann,      &
               singvd, cv1, dimwann, cv2, dimwann, cw1, nwork, cw2, info )
          IF ( info /= 0 ) THEN
            WRITE (*,*) 'Singular value decomposition zgesvd failed 2', &
                        ' info = ', info
            STOP
          END IF
          DO nb = 1, dimwann
            omiloc = omiloc + wb(nkp,nn) * ( 1.d0 - singvd(nb)**2 )
          END DO
          DO nb = 1, dimwann
            omt1 = omt1 + wb(nkp,nn) * ( 1.d0 - singvd(nb)**2 )
            omt2 = omt2 - wb(nkp,nn) * ( 2.d0 * LOG( singvd(nb) ) )
            omt3 = omt3 + wb(nkp,nn) * ( ACOS( singvd(nb) )**2 )
          END DO
        END DO
        ! WRITE(9,'(3f15.9,3x,f15.9)') ( vkpt(i,nkp), i=1,3 ), omiloc
      END DO

      omt1 = omt1/DBLE(nkpts)
      omt2 = omt2/DBLE(nkpts)
      omt3 = omt3/DBLE(nkpts)

      WRITE (*,*) ' '
      WRITE (*,*) ' Omega invariant (1-s^2, -2 log s, acos^2 s) '
      WRITE (*,*) ' '
      WRITE (*,'(3f10.5)') omt1, omt2, omt3
      WRITE (*,*) ' '

      ! CLOSE (9)

! ... Recalculate the average positions of the Wanns.

      CALL omega( dimwann, nkpts, nkpts, nntot, nnmx, nnlist, bk, wb, cm,       &
           csheet, sheet, rave, r2ave, func_om1, func_om2, func_om3, func_o )

      func_del1 = func_om1 - func_old1
      func_del2 = func_om2 - func_old2
      func_del3 = func_om3 - func_old3
      func_old1 = func_om1
      func_old2 = func_om2
      func_old3 = func_om3

      WRITE (*,8001) func_del1
      WRITE (*,8005) func_del2
      WRITE (*,8007) func_del3

8001  FORMAT(1x,'Delta Omega 1   is  ',0pe16.8)
8005  FORMAT(1x,'Delta Omega 2   is  ',0pe16.8)
8007  FORMAT(1x,'Delta Omega 3   is  ',0pe16.8)
8009  FORMAT(1x,'Delta Omega tot is  ',0pe16.8)

! ... Find the guiding centers, and set up the 'best' Riemannian sheets for 
!     the complex logarithms

      irguide = 0
!     CALL phases( dimwann, nkpts, nkpts, nnmx, nnmxh, nntot, nnh, neigh,        &
!          bk, bka, cm, csheet, sheet, rguide, irguide )
      irguide = 1

! ... Recalculate the average positions of the Wanns.

      CALL omega( dimwann, nkpts, nkpts, nntot, nnmx, nnlist, bk, wb, cm,        &
           csheet, sheet, rave, r2ave, func_om1, func_om2, func_om3, func_o )

      func0 = func_om1 + func_om2 + func_om3
      func_del1 = func_om1 - func_old1
      func_del2 = func_om2 - func_old2
      func_del3 = func_om3 - func_old3
      func_old1 = func_om1
      func_old2 = func_om2
      func_old3 = func_om3

      WRITE(*,8001) func_del1
      WRITE(*,8005) func_del2
      WRITE(*,8007) func_del3


!      OPEN( 20, file='wannier.rot', form='formatted', status='unknown' )
!      DO nkp=1,nkpts
!        DO i=1,dimwann
!          DO j=1,dimwann
!            write(20,*) cu(i,j,nkp)
!            DO nn=1,nntot(nkp)
!              write(20,*) cm(i,j,nkp,nn)
!            END DO
!          END DO
!        END DO
!      END DO
!      CLOSE(20)

! ... Singular value decomposition

      omt1 = 0.d0
      omt2 = 0.d0
      omt3 = 0.d0

      DO nkp = 1, nkpts
        DO nn = 1, nntot(nkp)
          DO na = 1, dimwann
            DO nb = 1, dimwann
              cmtmp(na,nb) = cm(na,nb,nkp,nn)
            END DO
          END DO
          CALL zgesvd( 'A', 'A', dimwann, dimwann, cmtmp, dimwann,      &
               singvd, cv1, dimwann, cv2, dimwann, cw1, 10*dimwann, cw2, info )
          IF ( info /= 0 ) THEN
            WRITE (*,*) 'Singular value decomposition zgesvd failed 3'
            STOP
          END IF
          calpha = ( 1.d0, 0.d0 )
          cbeta = ( 0.d0, 0.d0 )
          CALL zgemm( 'N', 'N', dimwann, dimwann, dimwann, calpha, cv1, dimwann,   &
               cv2, dimwann, cbeta, cv3, dimwann)

! ...     Uncomment to have the purified m's
!         DO na = 1, dimwann
!           DO nb = 1, dimwann
!             cm(na,nb,nkp,nn) = cv3(na,nb)
!           END DO
!         END DO

          DO nb = 1, dimwann
            omt1 = omt1 + wb(nkp,nn) * ( 1.d0 - singvd(nb)**2 )
            omt2 = omt2 - wb(nkp,nn) * ( 2.d0 * LOG( singvd(nb) ) )
            omt3 = omt3 + wb(nkp,nn) * ( ACOS( singvd(nb) )**2)
!           WRITE (*,'(i4,4x,f10.5,4x,3f10.5)') nb, singvd(nb), omt1, omt2, omt3
          END DO

        END DO     ! loop nn
      END DO       ! loop nkp


      omt1 = omt1/DBLE(nkpts)
      omt2 = omt2/DBLE(nkpts)
      omt3 = omt3/DBLE(nkpts)
      WRITE (*,*) ' '
      WRITE (*,*) ' Omega invariant (1-s^2, -2 log s, acos^2 s) '
      WRITE (*,*) ' '
      WRITE (*,'(3f10.5)') omt1, omt2, omt3
      WRITE (*,*) ' '

! ... Now that the consistent phase factors have been chosen
!     the wannier functions (for the R=0 cell) are calculated

      WRITE (*,*) ' '
      WRITE (*,8000)

      lrguide = .false.
      nrguide = 10

      lcg = .true.
      if ( ncg < 1 ) lcg = .false.

! ... But first it starts the iterative cycle to solve "Poisson" phase
    
      IF ( ncg == 0 ) ncg = ncg + 1
      ncgfix = ncg

      ALLOCATE( cu0(dimwann,dimwann,nkpts) )
      ALLOCATE( cm0(dimwann,dimwann,nkpts,nnmx) )
      ALLOCATE( cdodq(dimwann,dimwann,nkpts) )
      ALLOCATE( cdqkeep(dimwann,dimwann,nkpts) )
      ALLOCATE( cdodq1(dimwann,dimwann,nkpts) )
      ALLOCATE( cdodq2(dimwann,dimwann,nkpts) )
      ALLOCATE( cdodq3(dimwann,dimwann,nkpts) )
      ALLOCATE( cdq(dimwann,dimwann,nkpts) )
      ALLOCATE( cpad1(dimwann*dimwann) )
      ALLOCATE( cpad2(dimwann*dimwann) )

      cdqkeep = ( 0.0d0, 0.0d0 )
      cdodq1  = ( 0.0d0, 0.0d0 )
      cdodq2  = ( 0.0d0, 0.0d0 )
      cdodq3  = ( 0.0d0, 0.0d0 )
      cdq     = ( 0.0d0, 0.0d0 )
      cdodq   = ( 0.0d0, 0.0d0 )

      s2 = cclock()

      !
      !
      !  here start the iterative loop
      !
      !

      DO ncount = 1, niter + niter0     ! finira' alla riga 2245!!!

        IF ( ncount <= niter0 ) THEN
          ncg   = 1
          alpha = alphafix0
        ELSE
          ncg   = ncgfix
          alpha = alphafix
        END IF

! ...   Store cu and cm


        DO nkp = 1, nkpts
          DO i = 1, dimwann
            DO j = 1, dimwann
              cu0(i,j,nkp) = cu(i,j,nkp)
              DO nn = 1, nntot(nkp)
                cm0(i,j,nkp,nn) = cm(i,j,nkp,nn)
              END DO
            END DO
          END DO
        END DO

        IF ( lrguide ) THEN
          IF ( ( ( ncount / 10 ) * 10 == ncount ) .and. ( ncount >= nrguide ) )            &
            CALL phases( dimwann, nkpts, nkpts, nnmx, nnmxh, nntot, nnh, neigh,       &
                 bk, bka, cm, csheet, sheet, rguide, irguide )
        END IF

        ALLOCATE( cr(dimwann,dimwann,nkpts,nnmx) )
        ALLOCATE( crt(dimwann,dimwann,nkpts,nnmx) )
        ALLOCATE( rnkb(dimwann,nkpts,nnmx) )

        CALL domega( dimwann, nkpts, nkpts, nntot, nnmx, nnlist, bk, wb,              &
             cm, csheet, sheet, cr, crt, rave, r2ave, rnkb, cdodq1, cdodq2, cdodq3,   &
             cdodq)

        DEALLOCATE( cr )
        DEALLOCATE( crt )
        DEALLOCATE( rnkb )

        DO nkp = 1, nkpts
          DO m= 1, dimwann
            DO n = 1, dimwann
              gcnorm1 = gcnorm1 + REAL( cdodq(m,n,nkp) * CONJG( cdodq(m,n,nkp) ) )
              ! WRITE(*,*) cdodq(m,n,nkp)
            END DO
          END DO
        END DO

        IF ( MOD( (ncount-1), ncg ) == 0 ) THEN
          DO nkp = 1, nkpts
            DO m = 1, dimwann
              DO n = 1, dimwann
                cdq(m,n,nkp) = cdodq(m,n,nkp)
              END DO
            END DO
          END DO
        ELSE
          gcfac = gcnorm1/gcnorm0
          DO nkp = 1, nkpts
            DO m = 1, dimwann
              DO n = 1, dimwann
                cdq(m,n,nkp) = cdodq(m,n,nkp) + gcfac * cdqkeep(m,n,nkp)
              END DO
            END DO
          END DO
        END IF

        gcnorm0 = gcnorm1
        DO nkp = 1, nkpts
          DO m = 1, dimwann
            DO n= 1, dimwann
              cdqkeep(m,n,nkp) = cdq(m,n,nkp)
            END DO
          END DO
        END DO

        doda0 = 0.d0
        DO nkp = 1, nkpts
          DO m = 1, dimwann
            DO n = 1, dimwann
              doda0 = doda0 + REAL( cdq(m,n,nkp) * cdodq(n,m,nkp) )
            END DO
          END DO
        END DO
        doda0 = doda0 / wbtot / 4.d0

        WRITE (*,*) ' '
        WRITE (*,*) ' alpha_trial ', alpha
        WRITE (*,*) ' '

! ...   The cg step is calculated

        DO nkp = 1, nkpts
          DO m = 1, dimwann
            DO n = 1, dimwann
              cdq(m,n,nkp) = alpha / wbtot / 4.d0 * ( cdq(m,n,nkp) )
            END DO
          END DO
        END DO

        cfunc_exp1 = ( 0.d0, 0.d0 )
        cfunc_exp2 = ( 0.d0, 0.d0 )
        cfunc_exp3 = ( 0.d0, 0.d0 )
        DO nkp = 1, nkpts
          DO i = 1, dimwann
            DO j = 1, dimwann
              cfunc_exp1 = cfunc_exp1 + cdodq1(i,j,nkp) * cdq(j,i,nkp)
              cfunc_exp2 = cfunc_exp2 + cdodq2(i,j,nkp) * cdq(j,i,nkp)
              cfunc_exp3 = cfunc_exp3 + cdodq3(i,j,nkp) * cdq(j,i,nkp)
            END DO
          END DO
        END DO

!       cfunc_exp = cfunc_exp1 + cfunc_exp2+cfunc_exp3
!       WRITE(*,8002) cfunc_exp1
!       WRITE(*,8006) cfunc_exp2
!       WRITE(*,8008) cfunc_exp3
!       WRITE(*,8010) cfunc_exp

!       DO nkp = 1, nkpts
! ...     What if a random antihermitian dq is chosen
!         WRITE (*,*) 'NB: RANDOM ANTIHERMITIAN dQ IS CHOSEN !  '
!         DO m = 1, dimwann
!           DO n = m, dimwann
!             rre = ranf(iseed) * 10.d0- 5.d0
!             rri = ranf(iseed) * 10.d0- 5.d0
!             cdq(m,n,nkp) = epsilon * CMPLX(rre,rri)
!             cdq(n,m,nkp) = -CONJG( cdq(m,n,nkp) )
!             IF (m == n) cdq(n,m,nkp) = CMPLX( 0.d0, AIMAG( cdq(m,n,nkp) ) )
!           END DO
!         END DO
!       END DO

!       DO nkp = 1, nkpts
!         WRITE (*,*) '  '
!         WRITE (*,'(a19,i4)') ' Matrix dQ, k-point', nkp
!         WRITE (*,*) '  '
!         DO i = 1, dimwann
!           WRITE (*,'(8(pe10.2))') ( cdq(i,j,nkp), j=1,dimwann )
!         END DO
!       END DO

        DO nkp = 1, nkpts

          ind = 1
          DO j = 1, dimwann
            DO i = 1, dimwann
              cpad1(ind) = cdq(i,j,nkp)
              ind = ind + 1
            END DO
          END DO
          CALL zgees( 'V', 'N', lselect, dimwann, cpad1, dimwann, nsdim,     &
               cwschur1, cpad2, dimwann, cwschur2, SIZE( cwschur2 ), cwschur3,    &
               cwschur4, info )
          IF ( info /= 0 ) THEN
            WRITE (*,*) 'SCHUR: ', info
            STOP
          END IF

          ind = 1
          DO j = 1, dimwann
            DO i = 1, dimwann
              cdq(i,j,nkp) = cpad1(ind)
              cz(i,j)      = cpad2(ind)
              ind = ind + 1
            END DO
          END DO
          DO i = 1, dimwann
            DO j = 1, dimwann
              cdq(i,j,nkp) = ( 0.d0, 0.d0 )
              DO m = 1, dimwann
                cdq(i,j,nkp) = cdq(i,j,nkp) + cz(i,m) * ( EXP( cwschur1(m) ) ) * CONJG( cz(j,m) )
              END DO
            END DO
          END DO

! ...     dq is antihermitian, exp(dq) is unitary
!         WRITE(*,*) '  '
!         WRITE(*,'(a24,i4)') ' Matrix exp(dQ), k-point', nkp
!         WRITE(*,*) '  '
!         DO i = 1, dimwann
!           WRITE (*,'(8(pe10.2))') ( cdq(i,j,nkp), j=1,dimwann )
!         END DO

        END DO


        WRITE (*,*) '  '

! ...   The expected change in the functional is calculated

        cfunc_exp1 = ( 0.d0, 0.d0 )
        cfunc_exp2 = ( 0.d0, 0.d0 )
        cfunc_exp3 = ( 0.d0, 0.d0 )

        DO nkp = 1, nkpts
          DO i = 1, dimwann
            DO j = 1, dimwann
              cfunc_exp1 = cfunc_exp1 + cdodq1(i,j,nkp) * cdq(j,i,nkp)
              cfunc_exp2 = cfunc_exp2 + cdodq2(i,j,nkp) * cdq(j,i,nkp)
              cfunc_exp3 = cfunc_exp3 + cdodq3(i,j,nkp) * cdq(j,i,nkp)
            END DO
          END DO
        END DO

        cfunc_exp = cfunc_exp1 + cfunc_exp2 + cfunc_exp3
        WRITE (*,8002) cfunc_exp1
        WRITE (*,8006) cfunc_exp2
        WRITE (*,8008) cfunc_exp3
        WRITE (*,8010) cfunc_exp

8002    FORMAT(1x,'Delta0 Omega 1   (',0pe16.8,',',0pe16.8,')')
8006    FORMAT(1x,'Delta0 Omega 2   (',0pe16.8,',',0pe16.8,')')
8008    FORMAT(1x,'Delta0 Omega 3   (',0pe16.8,',',0pe16.8,')')
8010    FORMAT(1x,'Delta0 Omega tot (',0pe16.8,',',0pe16.8,')')

! ...   The orbitals are rotated 

        DO nkp = 1, nkpts
          DO i = 1, dimwann
            DO j = 1, dimwann
              cmtmp(i,j) = ( 0.d0, 0.d0 )
              DO m = 1, dimwann
                cmtmp(i,j) = cmtmp(i,j) + cu(i,m,nkp) * cdq(m,j,nkp)
              END DO
            END DO
          END DO
          DO i = 1, dimwann
            DO j = 1, dimwann
              cu(i,j,nkp) = cmtmp(i,j)
            END DO
          END DO
        END DO


! ...   And the M_ij are updated

        DO nkp = 1, nkpts
          DO nn = 1, nntot(nkp)
            nkp2 = nnlist(nkp,nn)
            DO i = 1, dimwann
              DO j = 1, dimwann
                cmtmp(i,j) = ( 0.d0, 0.d0 )
                DO m = 1, dimwann
                  DO n = 1, dimwann
                    cmtmp(i,j) = cmtmp(i,j) + CONJG( cdq(m,i,nkp) ) * cdq(n,j,nkp2) * cm(m,n,nkp,nn)
                  END DO
                END DO
              END DO
            END DO
            DO i = 1, dimwann
              DO j = 1, dimwann
                cm(i,j,nkp,nn) = cmtmp(i,j)
              END DO
            END DO
          END DO
        END DO

! ...   And the functional is recalculated

        CALL omega( dimwann, nkpts, nkpts, nntot, nnmx, nnlist, bk, wb, cm,       &
             csheet, sheet, rave, r2ave, func_om1, func_om2, func_om3, func_o )
        funca = func_om1 + func_om2 + func_om3
        func_del1 = func_om1 - func_old1
        func_del2 = func_om2 - func_old2
        func_del3 = func_om3 - func_old3
        func_del = func_del1 + func_del2 + func_del3
        WRITE (*,8001) func_del1
        WRITE (*,8005) func_del2
        WRITE (*,8007) func_del3
        WRITE (*,8009) func_del
       
        WRITE (*,*) ' '
        WRITE (*,'(a12,2e12.4)') ' derivative ', funca - func0, doda0 * alpha

! ...   If lcg is false, or still in the first niter0 iterations, 
!       it skips the optimal alpha paraphernalia 

        IF ( ( lcg ) .and. ( ncount > niter0 ) ) THEN

          eqc = func0
          eqb = doda0
          eqa = ( funca - func0 - eqb * alpha ) / alpha

          IF ( ABS(eqa) > 1.0e-8 ) THEN
            alphamin = -eqb / 2.d0 / eqa
          ELSE
            alphamin = alpha
          ENDIF
          IF ( alphamin < 0.d0 ) alphamin = alpha * 2.d0
          IF ( alphamin > 3.d0 * alpha ) alphamin = 3.d0 * alpha
          falphamin = eqa * alphamin**2 + eqb * alphamin + eqc

          WRITE(*,*) ' '
          WRITE(*,*) 'alpha_min, f(alpha_min)', alphamin, falphamin
          WRITE(*,*) ' '

! ...     Restore cu and cm

          DO nkp=1,nkpts
            DO i=1,dimwann
              DO j=1,dimwann
                cu(i,j,nkp)=cu0(i,j,nkp)
                DO nn=1,nntot(nkp)
                  cm(i,j,nkp,nn)=cm0(i,j,nkp,nn)
                END DO
              END DO
            END DO
          END DO

! ...     Take now optimal parabolic step

          DO nkp = 1, nkpts
            DO m = 1, dimwann
              DO n = 1, dimwann
                cdq(m,n,nkp) = alphamin / wbtot / 4.d0 * cdqkeep(m,n,nkp)
              END DO
            END DO
          END DO

! ...     The expected change in the functional is calculated

          cfunc_exp1 = ( 0.d0, 0.d0 )
          cfunc_exp2 = ( 0.d0, 0.d0 )
          cfunc_exp3 = ( 0.d0, 0.d0 )
          DO nkp = 1, nkpts
            DO i = 1, dimwann
              DO j = 1, dimwann
                cfunc_exp1 = cfunc_exp1 + cdodq1(i,j,nkp) * cdq(j,i,nkp)
                cfunc_exp2 = cfunc_exp2 + cdodq2(i,j,nkp) * cdq(j,i,nkp)
                cfunc_exp3 = cfunc_exp3 + cdodq3(i,j,nkp) * cdq(j,i,nkp)
              END DO
            END DO
          END DO

!         cfunc_exp = cfunc_exp1 + cfunc_exp2 + cfunc_exp3
!         WRITE (*,8002) cfunc_exp1
!         WRITE (*,8006) cfunc_exp2
!         WRITE (*,8008) cfunc_exp3

          DO nkp = 1, nkpts

            ind = 1
            DO j = 1, dimwann
              DO i = 1, dimwann
                cpad1(ind) = cdq(i,j,nkp)
                ind = ind + 1 
              END DO
            END DO

            CALL zgees( 'V', 'N', lselect, dimwann, cpad1, dimwann, nsdim,       &
                 cwschur1, cpad2, dimwann, cwschur2, SIZE( cwschur2 ), cwschur3,      &
                 cwschur4, info )

            IF ( info /= 0 ) THEN
              WRITE(*,*) 'SCHUR: ', info
              STOP
            END IF

            ind = 1
            DO j = 1, dimwann
              DO i = 1, dimwann
                cdq(i,j,nkp) = cpad1(ind)
                cz(i,j) = cpad2(ind)
                ind = ind + 1
              END DO
            END DO
            DO i = 1, dimwann
              DO j = 1, dimwann
                cdq(i,j,nkp) = ( 0.d0, 0.d0 )
                DO m = 1, dimwann
                  cdq(i,j,nkp) = cdq(i,j,nkp) + cz(i,m) * ( EXP( cwschur1(m) ) ) * CONJG( cz(j,m) )
                END DO
              END DO
            END DO

          END DO

          WRITE (*,*) '  '

! ...     The expected change in the functional is calculated

          cfunc_exp1 = ( 0.d0, 0.d0 )
          cfunc_exp2 = ( 0.d0, 0.d0 )
          cfunc_exp3 = ( 0.d0 ,0.d0 )
          DO nkp = 1, nkpts
            DO i = 1, dimwann
              DO j = 1, dimwann
                cfunc_exp1 = cfunc_exp1 + cdodq1(i,j,nkp) * cdq(j,i,nkp)
                cfunc_exp2 = cfunc_exp2 + cdodq2(i,j,nkp) * cdq(j,i,nkp)
                cfunc_exp3 = cfunc_exp3 + cdodq3(i,j,nkp) * cdq(j,i,nkp)
              END DO
            END DO
          END DO

          cfunc_exp = cfunc_exp1 + cfunc_exp2 + cfunc_exp3
          WRITE (*,8002) cfunc_exp1
          WRITE (*,8006) cfunc_exp2
          WRITE (*,8008) cfunc_exp3
          WRITE (*,8010) cfunc_exp

! ...     The orbitals are rotated 

          DO nkp = 1, nkpts
            DO i = 1, dimwann
              DO j = 1, dimwann
                cmtmp(i,j) = ( 0.d0, 0.d0 )
                DO m = 1, dimwann
                  cmtmp(i,j) = cmtmp(i,j) + cu(i,m,nkp) * cdq(m,j,nkp)
                END DO
              END DO
            END DO
            DO i = 1, dimwann
              DO j = 1, dimwann
                cu(i,j,nkp) = cmtmp(i,j)
              END DO
            END DO
          END DO

! ...     And the M_ij are updated

          DO nkp = 1, nkpts
            DO nn = 1, nntot(nkp)
              nkp2 = nnlist(nkp,nn)
              DO i = 1, dimwann
                DO j = 1, dimwann
                  cmtmp(i,j) = ( 0.d0, 0.d0 )
                  DO m = 1, dimwann
                    DO n = 1, dimwann
                      cmtmp(i,j) = cmtmp(i,j) + CONJG( cdq(m,i,nkp) ) * cdq(n,j,nkp2) * cm(m,n,nkp,nn)
                    END DO
                  END DO
                END DO
              END DO
              DO i = 1, dimwann
                DO j = 1, dimwann
                  cm(i,j,nkp,nn) = cmtmp(i,j)
                END DO
              END DO
            END DO
          END DO

! ...     And the functional is recalculated

          CALL omega( dimwann, nkpts, nkpts, nntot, nnmx, nnlist, bk, wb, cm,       &
     &         csheet, sheet, rave, r2ave, func_om1, func_om2, func_om3, func_o )

          funca = func_om1 + func_om2 + func_om3
          func_del1 = func_om1 - func_old1
          func_del2 = func_om2 - func_old2
          func_del3 = func_om3 - func_old3
          func_del = func_del1 + func_del2 + func_del3
          WRITE (*,8001) func_del1
          WRITE (*,8005) func_del2
          WRITE (*,8007) func_del3
          WRITE (*,8009) func_del

! ...   end of the lcg skip of the optimal alpha paraphernalia 
        END IF


        func_old1 = func_om1
        func_old2 = func_om2
        func_old3 = func_om3

        func0 = funca
     
        WRITE (*,*) ' '
        WRITE (*,8020) ncount, func0
        WRITE (*,*) ' '
        WRITE (*,8000)
8020    FORMAT(1x,'Omega at iteration ',i7,' is ',f22.15)

! ...   Check convergence

        IF ( ABS( func_del ) < 1.e-8 ) EXIT
        ! EXIT

      END DO !end loop ncount

! ... End of the minimization loop

      s3 = cclock()


      IF ( verbosity == 'high' ) THEN
! ... Unitariety is checked
        DO nkp = 1, nkpts
          DO i = 1, dimwann
            DO j = 1, dimwann
              ctmp1 = ( 0.d0, 0.d0 )
              ctmp2 = ( 0.d0, 0.d0 )
              DO m = 1, dimwann
                ctmp1 = ctmp1 + cu(i,m,nkp) * CONJG( cu(j,m,nkp) )
                ctmp2 = ctmp2 + cu(m,j,nkp) * CONJG( cu(m,i,nkp) )
              END DO
              WRITE(*,'(2i4,4f15.10)') i, j, ctmp1, ctmp2
            END DO
          END DO
        END DO
      END IF

! ... Singular value decomposition

      omt1 = 0.d0
      omt2 = 0.d0
      omt3 = 0.d0

      DO nkp = 1, nkpts
        DO nn = 1, nntot(nkp)

          DO na = 1, dimwann
            DO nb = 1, dimwann
              cmtmp(na,nb) = cm(na,nb,nkp,nn)
            END DO
          END DO

          CALL zgesvd( 'A', 'A', dimwann, dimwann, cmtmp, dimwann,      &
     &         singvd, cv1, dimwann, cv2, dimwann, cw1, 10*dimwann, cw2, info )
          IF ( info /= 0 ) THEN
            WRITE(*,*) 'Singular value decomposition zgesvd failed 4'
            STOP
          END IF

          DO nb = 1, dimwann
            omt1 = omt1 + wb(nkp,nn) * ( 1.d0 - singvd(nb)**2 )
            omt2 = omt2 - wb(nkp,nn) * ( 2.d0 * LOG( singvd(nb) ) )
            omt3 = omt3 + wb(nkp,nn) * ( ACOS( singvd(nb) )**2 )
!           WRITE(*,'(i4,4x,f10.5,4x,3f10.5)') nb, singvd(nb), omt1, omt2, omt3
          END DO

        END DO
      END DO

      omt1 = omt1/DBLE(nkpts)
      omt2 = omt2/DBLE(nkpts)
      omt3 = omt3/DBLE(nkpts)
      WRITE (*,*) ' '
      WRITE (*,*) ' Omega invariant (1-s^2, -2 log s, acos^2 s) '
      WRITE (*,*) ' '
      WRITE (*,'(3f10.5)') omt1, omt2, omt3
      WRITE (*,*) ' '

 
! ... Write the final unitary transformations into a file
 
      OPEN( 29, FILE='unitary.dat', STATUS='UNKNOWN', FORM='UNFORMATTED' )

      WRITE(29) ( ( ( CU(J,I,N), J=1,dimwann ), I=1,dimwann ), N=1,NKPTS )

      CLOSE(29)

! ... Write input file for plot wannier functions
!
      OPEN( 21, FILE='landing.dat', STATUS='UNKNOWN', FORM='UNFORMATTED' )

      WRITE (21) nkpts, ngx, ngy, ngz, mplwv, dimwann, mxddim, ntype
      WRITE (21) recc(1,1), recc(2,1), recc(3,1)
      WRITE (21) recc(1,2), recc(2,2), recc(3,2)
      WRITE (21) recc(1,3), recc(2,3), recc(3,3)
      WRITE (21) dirc(1,1), dirc(2,1), dirc(3,1)
      WRITE (21) dirc(1,2), dirc(2,2), dirc(3,2)
      WRITE (21) dirc(1,3), dirc(2,3), dirc(3,3)

      DO  nkp = 1 , nkpts
        WRITE (21) vkpt(1,nkp), vkpt(2,nkp), vkpt(3,nkp)
      END DO

      DO nsp = 1, ntype
        write(21) natom(nsp), nameat(nsp)
      END DO

      DO nsp = 1, ntype
        DO ni = 1, natom(nsp)
          WRITE(21)( posion(i,ni,nsp), i=1,3 )
        END DO
      END DO

      DO nkp = 1, nkpts
        WRITE (21) nplwkp(nkp)
      ENDDO

      DO nkp = 1, nkpts
        DO n = 1, mplwv
          WRITE (21) nindpw(n,nkp)
        END DO
      END DO

      DO nkp = 1, nkpts
        DO nb= 1, dimwann
          DO m = 1, nplwkp(nkp)
            WRITE(21) cptwfp(m,nb,nkp)
          END DO
        END DO
      END DO

      CLOSE(21)

! ... Deallocate arrays

      DEALLOCATE( cwschur1, cwschur2 )
      DEALLOCATE( cz )
      DEALLOCATE( cwschur3 )
      DEALLOCATE( cwschur4 )


      DEALLOCATE( vkpt )
      DEALLOCATE( kgv )
      DEALLOCATE( wtkpt )
      DEALLOCATE( nfile )
      DEALLOCATE( nplwkp )
      DEALLOCATE( datake )
      DEALLOCATE( dnlg )
      DEALLOCATE( dnlkg )
      DEALLOCATE( ninvpw )
      DEALLOCATE( cptwfp )
      DEALLOCATE( vkpr )
      DEALLOCATE( dnn )
      DEALLOCATE( nnshell )
      DEALLOCATE( nnlist, nntot ) 
      DEALLOCATE( nncell ) 
      DEALLOCATE( bk ) 
      DEALLOCATE( dimsingvd, dimbk, ndim )
      DEALLOCATE( wb )
      DEALLOCATE( bka )
      DEALLOCATE( neigh )

      DEALLOCATE( rphicmx1 )
      DEALLOCATE( rphicmx2 )
      DEALLOCATE( nphimx1 )
      DEALLOCATE( nphimx2 )
      DEALLOCATE( nphir )
      DEALLOCATE( cm )
      DEALLOCATE( csheet )
      DEALLOCATE( sheet )
      DEALLOCATE( rave, r2ave )
      DEALLOCATE( ca, cs )
      DEALLOCATE( cu )
      DEALLOCATE( cmtmp )
      DEALLOCATE( singvd )
      DEALLOCATE( cv1 )
      DEALLOCATE( cv2 )
      DEALLOCATE( cv3 )
      DEALLOCATE( cu0 )
      DEALLOCATE( cm0 )
      DEALLOCATE( cdodq )
      DEALLOCATE( cdqkeep )
      DEALLOCATE( cdodq1 )
      DEALLOCATE( cdodq2 )
      DEALLOCATE( cdodq3 )
      DEALLOCATE( cdq )
      DEALLOCATE( cpad1 )
      DEALLOCATE( cpad2 )

      CALL deallocate_input()

      sf = cclock()
      WRITE( *, * ) 'Total Time (sec) : ', sf - s0
      WRITE( *, * ) 'Init  Time (sec) : ', s1 - s0
      WRITE( *, * ) 'Trasf Time (sec) : ', s2 - s1
      WRITE( *, * ) 'Iter  Time (sec) : ', s3 - s2
      WRITE( *, * ) 'Write Time (sec) : ', sf - s3

      STOP '*** THE END *** (wannier.f90)'
      END PROGRAM wannier
