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
      PROGRAM wannier
!=----------------------------------------------------------------------------------=

      USE kinds
      USE constants, ONLY: pi, twopi => tpi, &
         ryd => ry, har => au, bohr => bohr_radius_angs
      USE parameters, ONLY: mxdtyp => npsx, mxdatm => natx
      USE fft_scalar, ONLY: cfft3d
      USE input_wannier
      USE timing_module, ONLY : timing, timing_deallocate, timing_overview, global_list
      USE io_global, ONLY : stdout
      USE startup_module, ONLY : startup
      USE version_module, ONLY : version_number

      IMPLICIT NONE

      ! 12 is the maximum number of nearest neighbours

      INTEGER, PARAMETER :: nnmx  = 12 
      INTEGER, PARAMETER :: nnmxh = nnmx/2 
      INTEGER, PARAMETER :: nprint = 10

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
      REAL(dbl) :: enmax
      REAL(dbl) :: ddelta
      REAL(dbl) :: volc, voli, wtktot
      REAL(dbl) :: epsilon, eta, eps, dnn0, dnn1, dist
      REAL(dbl) :: bb1, bbn, factor, wbtot
      REAL(dbl) :: asidemin, aside
      REAL(dbl) :: func_om1, func_om2, func_om3, func_o
      REAL(dbl) :: func_old1, func_old2, func_old3
      REAL(dbl) :: sph00, sph1m1, sph10, sph11, sph2m2
      REAL(dbl) :: sph2m1, sph20, sph21, sph22
      REAL(dbl) :: rx, ry, rz, scalf
      REAL(dbl) :: dist1, dist_pl, dist_cos
      REAL(dbl) :: th_cos, th_sin, ph_cos, ph_sin, dist2, select
      REAL(dbl) :: rre, rri, omt1, omt2, omt3, omiloc
      REAL(dbl) :: func_del1, func_del2, func_del3, func0
      REAL(dbl) :: gcnorm1, gcfac, gcnorm0, doda0
      REAL(dbl) :: funca, func_del, eqc, eqb, eqa, alphamin, falphamin
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
      REAL(dbl), ALLOCATABLE ::  bk(:,:,:)   !  bk(3,nkpts,nnmx)
      REAL(dbl), ALLOCATABLE ::  rave(:,:), r2ave(:) ,rave2(:)
                                 ! rave(3,dimwann), r2ave(dimwann), rave2(dimwann)
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

      COMPLEX(dbl), ALLOCATABLE :: cwschur1(:), cwschur2(:) 
                                   !  cwschur1(dimwann),cwschur2(10*dimwann)

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

      REAL(dbl) :: alat
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

      REAL(dbl) :: rtot(3), r2tot
      REAL(dbl) :: func_i
      REAL(dbl) :: func_od
      REAL(dbl) :: func_d

      INTEGER :: idum, rdum, ierr


!
! ... End declarations and dimensions
!
!=----------------------------------------------------------------------------=!


!
! ...  Startup
!
       CALL startup(version_number,MAIN_NAME='wannier')
       CALL timing('init',OPR='start')

!
! ...  Read input parameters from window.out
!

      CALL read_input()

      OPEN( UNIT=19, FILE='takeoff.dat', STATUS='OLD', FORM='UNFORMATTED' )

      READ(19) alat
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
      READ(19) idum ! (nwhich(i),i=1,nshells)

      READ(19) nkpts, mxddim, mxdbnd
      READ(19) ngx, ngy, ngz, ngm

      READ(19) idum ! gauss_typ(1:dimwann)
      READ(19) rdum ! rphiimx1(1:3,1:dimwann)
      READ(19) rdum ! rphiimx2(1:3,1:dimwann)
      READ(19) idum ! l_wann(1:dimwann)
      READ(19) idum ! m_wann(1:dimwann)
      READ(19) idum ! ndir_wann(1:dimwann)
      READ(19) rdum ! rloc(1:dimwann)

! ... Read grid information, and G-vectors

      READ(19) mxdgve 
      ALLOCATE( kgv( 3, mxdgve ), STAT=ierr )
         IF ( ierr/=0 ) CALL errore(' wannier ', ' allocating kgv ', 3*mxdgve )
      READ(19) ( ( kgv(i,j), i=1,3 ), j=1,mxdgve )

      CLOSE(19)


      natom = natom
      enmax  = enmax * har

      CALL atomset( alat, avec, ntype, natom, nameat, rat, mxdtyp, mxdatm )
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

!...  Start writing output
      WRITE( stdout, * ) ' ======================================================================'
      WRITE( stdout, * ) ' =                         Input parameters                           ='
      WRITE( stdout, * ) ' ======================================================================'
      WRITE( stdout, * ) '  '
      WRITE( stdout, fmt= " (2x,'Alat = ', F8.4, ' (Ang)' )" ) alat * bohr
      WRITE( stdout, * ) '  '
      WRITE( stdout, fmt= " (2x, 'Crystal axes: (Ang)' ) ")
      DO j=1,3
        WRITE ( stdout, fmt="(4x,'a(',I1,') = (', 3F8.4, ' )'  )" ) &
               j, ( dirc(j,i), i=1,3 )
      END DO
 
      WRITE( stdout,*) ' '
      WRITE( stdout, fmt= " (2x, ' Reciprocal lattice vectors:' ) " )
      DO j=1,3
        WRITE ( stdout, fmt="(4x,'b(',I1,') = (', 3F8.4, ' )'  )" ) &
               j,  ( recc(j,i), i=1,3 )
      END DO
      WRITE( stdout, * ) ' '
      WRITE( stdout, fmt="(2x, 'Cell volume = ', F12.6, ' (Ang)^3' )" ) volc
      WRITE( stdout,*) ' '

      ng = 1

      WRITE( stdout, fmt="(2x, 'Atomic positions: (cart. coord.)' ) " )
      DO nsp = 1 , ntype
        DO ni = 1 , natom(nsp)
          posion(:,ni,nsp) = rat(:,ni,nsp)/twopi
!          WRITE( stdout, fmt="(4x, a, 2x,'tau(',I1,') = (', 3F8.4, ' )' )" ) &
!                 nameat( nsp ), ni, ( posion(i,ni,nsp), i=1,3 )
!         WRITE (*,7004) ni, nsp, ( posion(i,ni,nsp), i=1,3 )
          ng = ng + 1
        END DO
      END DO
!7004 FORMAT( 2x, 'ION ', i3, ' TYPE ', i1, ' AT (',3f12.7,') (A_i UNITS)' )

      DO nsp = 1 , ntype
        DO ni = 1 , natom(nsp)
          DO m = 1, 3
            poscart(m,ni,nsp) = 0.d0
            DO j=1,3
              poscart(m,ni,nsp) = poscart(m,ni,nsp) + posion(j,ni,nsp) * dirc(j,m)
            END DO
          END DO
          WRITE( stdout, fmt="(4x, a, 2x,'tau( ',I3,' ) = (', 3F8.4, ' )' )" ) &
          nameat( nsp ), ni, ( poscart(i,ni,nsp), i=1,3 )
        END DO
      END DO


      nkpts1 = nk(1)*nk(2)*nk(3)

      ALLOCATE( vkpt(3, nkpts), STAT=ierr )
         IF( ierr /=0 ) CALL errore(' wannier ', ' allocating vkpt ', (3*nkpts) )
      ALLOCATE( wtkpt(nkpts), STAT=ierr )
         IF( ierr /=0 ) CALL errore(' wannier ', ' allocating wtkpt ', (nkpts) )
      ALLOCATE( nfile( nkpts ), STAT=ierr )
         IF( ierr /=0 ) CALL errore(' wannier ', ' allocating nfile ', (nkpts) )

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

      DO nkp = 1 , nkpts
        wtkpt(nkp) =  1.0d0/DBLE(nkpts)
      END DO

      wtktot = 0.d0
      DO nkp = 1 , nkpts
        wtktot = wtktot + wtkpt(nkp)
      END DO

! ... Do destinato a morire visto che legge da un file unico

      DO nkp = 1, nkpts
        nfile(nkp) = nkp
      END DO


      ALLOCATE( lpctx(ngx), lpcty(ngy), lpctz(ngz), STAT=ierr )
         IF( ierr/=0 )  &
         CALL errore(' wannier ', ' allocating lpctx lpcty lpctz ', ngx+ngy+ngz )



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

      ALLOCATE( nindpw( mxddim, nkpts ), STAT=ierr )
         IF( ierr /=0 ) CALL errore(' wannier ', ' allocating nindpw ', mxddim*nkpts )


      nindpw = 0

      ALLOCATE( nplwkp( nkpts ), STAT=ierr )
         IF( ierr /=0 ) CALL errore(' wannier ', ' allocating nplwkp ', nkpts )
      ALLOCATE( datake( 7, mxddim, nkpts ), STAT=ierr )
         IF( ierr /=0 ) CALL errore(' wannier ', ' allocating datake ', 7*mxddim*nkpts )
      ALLOCATE( dnlg( mxddim, 3, nkpts ), STAT=ierr )
         IF( ierr /=0 ) CALL errore(' wannier ', ' allocating dnlg ', 3*mxddim*nkpts )
      ALLOCATE( dnlkg( mxddim, 0:3, nkpts ), STAT=ierr )
         IF( ierr /=0 ) CALL errore(' wannier ', ' allocating dnlkg ', 4*mxddim*nkpts )


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
      ALLOCATE( ninvpw(0:nplwv,nkpts), STAT=ierr )
         IF( ierr /=0 ) CALL errore(' wannier ', ' allocating ninvpw ', (nplwv+1)*nkpts )

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

      ALLOCATE( cptwfp( mxddim + 1, dimwann, nkpts ), STAT=ierr )
         IF( ierr /=0 ) &
         CALL errore(' wannier ', ' allocating cptwfp ', (mxddim+1)*dimwann*nkpts )

      OPEN( UNIT=20, FILE='onfly.dat', STATUS='OLD' , FORM='UNFORMATTED' )


!     Starting k-loop

      K_POINTS:  DO nkp = 1, nkpts

        ! SIZE( cptwfp, 1), SIZE( cptwfp, 2), SIZE( cptwfp, 3), nplwkp( nkp ), nbande
        READ(20) nsiz1_, nsiz2_, nsiz3_, ngk_, nbnd_ 

        IF( ( SIZE( cptwfp, 1) < nsiz1_ ) .OR. ( SIZE( cptwfp, 2) < nsiz2_ ) &
            .OR. ( SIZE( cptwfp, 3) < nsiz3_ ) ) THEN
          WRITE( stdout, * ) '*** READING cptwfp WRONG DIMENSIONS ***' 
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

      CALL timing('init',OPR='stop')
      CALL timing('trasf',OPR='start')

!     WRITE(*,*) ' '

      iseed   = -1
      epsilon = 1.d0
!
!
!...  Wannier Functions localization procedure

      ALLOCATE( vkpr(3,nkpts), STAT=ierr )
         IF( ierr /=0 ) CALL errore(' wannier ', ' allocating vkpr ', 3*nkpts )
      ALLOCATE ( nnshell(nkpts,nnmx), STAT=ierr )
         IF( ierr /=0 ) CALL errore(' wannier ', ' allocating nnshell ', nkpts*nnmx )
      ALLOCATE ( nnlist(nkpts,nnmx), nntot(nkpts), STAT=ierr ) 
         IF( ierr /=0 ) CALL errore(' wannier ', ' allocating nnlist nntot ',nkpts*(nnmx+1))
      ALLOCATE ( nncell(3,nkpts,nnmx), STAT=ierr ) 
         IF( ierr /=0 ) CALL errore(' wannier ', ' allocating nncell ',3*nkpts*nnmx )
      ALLOCATE ( bk(3,nkpts,nnmx), STAT=ierr ) 
         IF( ierr /=0 ) CALL errore(' wannier ', ' allocating bk ', 3*nkpts*nnmx )
      ALLOCATE( dimsingvd(MAX(nnmx,3)), STAT=ierr )
         IF( ierr /=0 ) CALL errore(' wannier ', ' allocating dimsingvd ', MAX(nnmx,3) )
      ALLOCATE( dimbk(3,nnmx), STAT=ierr )
         IF( ierr /=0 ) CALL errore(' wannier ', ' allocating dimbk ', 3*nnmx )
      ALLOCATE( ndim(nnmx), STAT=ierr )
         IF( ierr /=0 ) CALL errore(' wannier ', ' allocating ndim ', nnmx )
      ALLOCATE( v1(nnmx,nnmx), STAT=ierr )
         IF( ierr /=0 ) CALL errore(' wannier ', ' allocating v1 ', nnmx**2 )
      ALLOCATE( v2(nnmx,nnmx), STAT=ierr )
         IF( ierr /=0 ) CALL errore(' wannier ', ' allocating v2 ', nnmx**2 )
      ALLOCATE( w1(10*nnmx), STAT=ierr )
         IF( ierr /=0 ) CALL errore(' wannier ', ' allocating w1 ', 10*nnmx )
      ALLOCATE( bka(3,nnmxh), STAT=ierr )
         IF( ierr /=0 ) CALL errore(' wannier ', ' allocating bka ', 3*nnmxh )
      ALLOCATE( neigh(nkpts,nnmxh), STAT=ierr )
         IF( ierr /=0 ) CALL errore(' wannier ', ' allocating neigh ', nkpts*nnmxh )
      ALLOCATE( dnn(nnmx), STAT=ierr )
         IF( ierr /=0 ) CALL errore(' wannier ', ' allocating dnn', nnmx )
      ALLOCATE( wb(nkpts,nnmx), STAT=ierr )
         IF( ierr /=0 ) CALL errore(' wannier ', ' allocating wb', nnmx )

!
!...  Call b-shell
      CALL bshells( vkpt, nkpts, recc, nshells, nwhich, nnshell, bk,       &
            dnn, wb, wbtot, nnlist, nncell, nntot, bka, neigh, nkpts )

      DEALLOCATE( v1, STAT=ierr )
         IF( ierr /=0 ) CALL errore(' wannier ', ' deallocating v1 ', ABS(ierr) )
      DEALLOCATE( v2, STAT=ierr )
         IF( ierr /=0 ) CALL errore(' wannier ', ' deallocating v2 ', ABS(ierr) )
      DEALLOCATE( w1, STAT=ierr )
         IF( ierr /=0 ) CALL errore(' wannier ', ' deallocating w1 ', ABS(ierr) )

!     check we got it right -- should just see bka vectors
      IF ( verbosity == 'high' ) THEN
        WRITE(stdout, fmt="( 'Check we got it right -- should just see bka vectors')")
        DO nkp = 1, nkpts
          WRITE (*,*)
          DO na = 1, nnh
            nn = neigh(nkp,na)
            WRITE (stdout, fmt="(3f10.5)") ( bk(j,nkp,nn), j=1,3 )
          END DO
        END DO
      END IF
 
! ... now it defines the centers of the localized functions (e.g. gaussians)
!     that are used to pick up the phases. this is system dependent 
!     rphiimx is the center of the Gaussians in relative coordinates (as posion)

       DO nwann = 1, dimwann

         rloc(nwann) = rloc(nwann) * bohr

         IF ( gauss_typ(nwann) == 1 ) THEN
!...       values below don't really matter, since rphiimx2 is not used when  gauss_typ=1
           rphiimx2(1,nwann) = 0.d0
           rphiimx2(2,nwann) = 0.d0
           rphiimx2(3,nwann) = 0.d0
         ELSE IF( gauss_typ(nwann) == 2 ) THEN
           continue 
         ELSE
           CALL errore(' wannier ', 'error in trial centers: wrong gauss_typ', gauss_typ(nwann) )
         END IF

       END DO

       ALLOCATE( rphicmx1(3,dimwann), STAT=ierr )
         IF( ierr /=0 ) CALL errore(' wannier ', ' allocating rphicmx1 ', 3*dimwann )
       ALLOCATE( rphicmx2(3,dimwann), STAT=ierr )
         IF( ierr /=0 ) CALL errore(' wannier ', ' allocating rphicmx2 ', 3*dimwann )

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

      ALLOCATE( nphimx1(3,dimwann), STAT=ierr )
         IF( ierr /=0 ) CALL errore(' wannier ', ' allocating nphimx1 ', 3*dimwann )
      ALLOCATE( nphimx2(3,dimwann), STAT=ierr )
         IF( ierr /=0 ) CALL errore(' wannier ', ' allocating nphimx2 ', 3*dimwann )

      ngdim(1) = ngx
      ngdim(2) = ngy
      ngdim(3) = ngz
      DO nwann = 1, dimwann
         DO m = 1, 3
           nphimx1(m,nwann) = INT( rphiimx1(m,nwann) * DBLE( ngdim(m) ) + 1.001 )
           nphimx2(m,nwann) = INT( rphiimx2(m,nwann) * DBLE( ngdim(m) ) + 1.001 )
         END DO
      END DO

      WRITE( stdout, fmt= " (2x, 'Gaussian centers: (cart. coord. in Ang)' ) " )
      DO nwann = 1, dimwann
        WRITE( stdout, fmt="(4x,'Center = ', i3, ' Type =',i2,' Gaussian  = (', 3F10.6, ' ) '  )" ) &
               nwann, gauss_typ(nwann), ( rphicmx1(m,nwann), m=1,3 )
        IF  ( gauss_typ(nwann) == 2 ) THEN
          WRITE( stdout, fmt="(26x,'Gaussian2 = (', 3F10.6, ' ) '  )" ) &
                  ( rphicmx2(m,nwann), m=1,3 )
        END IF
      END DO
      WRITE( stdout, fmt= " (' ' ) " )
       
      ALLOCATE( nphir(dimwann), STAT=ierr )
         IF( ierr /=0 ) CALL errore(' wannier ', ' allocating nphir ', dimwann )

      DO nwann = 1, dimwann
        asidemin = 100000.d0 * rloc( nwann )
        DO j = 1, 3
          aside    = SQRT( dirc(j,1)**2 + dirc(j,2)**2 + dirc(j,3)**2 )
          asidemin = MIN( aside, asidemin )
        END DO
        nphir(nwann) = nint( 2 * ( rloc( nwann ) / asidemin ) * MIN( ngx, ngy, ngz ) )

      END DO

      WRITE( stdout, * ) '  '
      WRITE( stdout, * ) ' ======================================================================'
      WRITE( stdout, * ) ' =                  Starting localization procedure                   ='
      WRITE( stdout, * ) ' ======================================================================'
      WRITE( stdout, * ) '  '


! --- Transform the wavefunctions (u_nk) into real space:
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
 
      ALLOCATE( cm(dimwann,dimwann,nkpts,nnmx), STAT=ierr )
         IF( ierr /=0 ) CALL errore(' wannier ', ' allocating cm ', dimwann**2 * nkpts*nnmx)
      ALLOCATE( nx2(ngx), STAT=ierr )
         IF( ierr /=0 ) CALL errore(' wannier ', ' allocating nx2 ', ngx)
      ALLOCATE( ny2(ngy), STAT=ierr )
         IF( ierr /=0 ) CALL errore(' wannier ', ' allocating ny2 ', ngy)
      ALLOCATE( nz2(ngz), STAT=ierr )
         IF( ierr /=0 ) CALL errore(' wannier ', ' allocating nz2 ', ngz)

      cm(:,:,:,:) = (0.d0, 0.d0)

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

      DEALLOCATE( nx2, STAT=ierr )
         IF( ierr /=0 ) CALL errore(' wannier ', ' deallocating nx2 ', ABS(ierr))
      DEALLOCATE( ny2, STAT=ierr )
         IF( ierr /=0 ) CALL errore(' wannier ', ' deallocating ny2 ', ABS(ierr))
      DEALLOCATE( nz2, STAT=ierr )
         IF( ierr /=0 ) CALL errore(' wannier ', ' deallocating nz2 ', ABS(ierr))

      ALLOCATE( csheet(dimwann,nkpts,nnmx), STAT=ierr )
         IF( ierr /=0 ) CALL errore(' wannier ', ' allocating csheet ', dimwann*nkpts*nnmx)
      ALLOCATE( sheet(dimwann,nkpts,nnmx), STAT=ierr )
         IF( ierr /=0 ) CALL errore(' wannier ', ' allocating sheet ', dimwann*nkpts*nnmx)

      DO nkp = 1, nkpts
        DO nb = 1, dimwann
          DO nn = 1, nntot(nkp)
            sheet(nb,nkp,nn)  = 0.d0 
            csheet(nb,nkp,nn) = exp(ci*sheet(nb,nkp,nn))
          END DO
        END DO
      END DO

! ... Now calculate the average positions of the Wanns.

      ALLOCATE( rave(3,dimwann), r2ave(dimwann), rave2(dimwann), STAT=ierr )
         IF( ierr /=0 ) CALL errore(' wannier ', ' allocating rave r2ave ', 4*dimwann)
      

      WRITE(stdout, fmt=" (2x, 'Trial Wannier centers and Spreads (Omega)')")

      CALL omega( dimwann, nkpts, nkpts, nntot(:), nnmx, nnlist(:,:), bk(:,:,:), wb(:,:), &
                  cm(:,:,:,:), csheet(:,:,:), sheet(:,:,:), rave(:,:), r2ave(:), rave2(:), &
                  func_om1, func_om2, func_om3, func_o, rtot, r2tot, func_i, func_d, & 
                  func_od )

!...  Write centers and spread
      DO nwann = 1, dimwann
        WRITE( stdout, fmt= " ( 4x, 'Center ', i3, 1x, '= (',f12.6,',',f12.6,',',f12.6,  &
           &' )  Omega = ', f13.6 )" )  nwann,( rave(ind,nwann), ind=1,3 ),  &
                                        r2ave(nwann) - rave2(nwann)
      END DO  
      WRITE( stdout, * ) '  ' 
      WRITE( stdout, fmt= " ( 2x, '! Center Sum', &
           &  1x, '= (',f12.6,',',f12.6,',',f12.6,' )  Omega = ', f13.6 )" )     &
           (rtot(i),i=1,3), r2tot
              
      WRITE( stdout, * ) '  '
      WRITE( stdout, fmt="(2x, 'Spread Operator decomposition: ')")
      WRITE( stdout, fmt="(4x,'OmegaI    =   ', f15.9 ) " ) func_i
      WRITE( stdout, fmt="(4x,'OmegaD    =   ', f15.9 ) " ) func_d
      WRITE( stdout, fmt="(4x,'OmegaOD   =   ', f15.9 ) " ) func_od
      WRITE( stdout, * ) '  '
      WRITE( stdout, fmt="(4x,'Omega Tot =   ', f15.9 ) " ) func_o

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

      ALLOCATE( cptwr(mplwv), STAT=ierr )
         IF( ierr /=0 ) CALL errore(' wannier ', ' allocating cptwr ', mplwv)
      ALLOCATE( ca(dimwann,dimwann,nkpts), STAT=ierr )
         IF( ierr /=0 ) CALL errore(' wannier ', ' allocating ca ', dimwann**2 * nkpts)

      DO nkp = 1, nkpts

        DO nb = 1, dimwann

          cptwr(:) = czero
          DO m = 1, nplwkp(nkp)
            cptwr(nindpw(m,nkp)) = cptwfp(m,nb,nkp)
          END DO

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
                      WRITE(stdout,*) 'ERROR: Wrong z-direction'
                      CALL errore(' wannier ', ' wrong z- direction ', ndir_wann(nwann) )
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
                        WRITE(stdout,*) 'ERROR: check the spherical harmonics (I)'
                        CALL errore(' wannier ', ' check the spherical harmonics (I)', m_wann(nwann) )
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
                        CALL errore(' wannier ', ' check the spherical harmonics (II)', m_wann(nwann) )
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
                        WRITE (stdout, *)  '*** ERROR *** in sp^3 hybrid gaussian: check m_wann'
                        CALL errore(' wannier ', ' sp^3 hybrid gaussian ', m_wann(nwann) )
                      END IF

                    ELSE 
                      WRITE(stdout,*) '*** ERROR *** : check the spherical harmonics (III)'
                      CALL errore(' wannier ', ' check the spherical harmonics (III)', m_wann(nwann) )
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

      
!       WRITE(*,*) '  '
!       WRITE(*,'(a18,i4)') ' Matrix A, k-point', nkp
!       WRITE(*,*) '  '
!       IF ( dimwann <= 8 ) THEN
!         DO i = 1, dimwann
!           WRITE(*,'(8(0pe10.2))') ( ca(i,j,nkp), j=1,dimwann )
!         END DO
!       END IF

      END DO  !END k loop

!     write(*,*) ' '
!     write(*,8000)

      DEALLOCATE( cptwr, STAT=ierr )
         IF( ierr /=0 ) CALL errore(' wannier ', ' deallocating cptwr ', ABS(ierr))



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

      ALLOCATE( cs(dimwann,dimwann,nkpts), STAT=ierr )
         IF( ierr /=0 ) CALL errore(' wannier ', ' allocating cs ', dimwann**2 * nkpts)

      nwork = dimwann * 10
      ALLOCATE( cwschur1(dimwann), cwschur2( nwork ), STAT=ierr )
         IF( ierr /=0 ) &
         CALL errore(' wannier ', ' allocating cwschur1 cwschur2 ', dimwann+nwork)
      ALLOCATE( cz(dimwann, dimwann), STAT=ierr )
         IF( ierr /=0 ) CALL errore(' wannier ', ' allocating cz ', dimwann**2)
      ALLOCATE( cwschur3(dimwann), STAT=ierr )
         IF( ierr /=0 ) CALL errore(' wannier ', ' allocating cwschur3 ', dimwann)
      ALLOCATE( cwschur4(dimwann), STAT=ierr )
         IF( ierr /=0 ) CALL errore(' wannier ', ' allocating cwschur4 ', dimwann)
      ALLOCATE( cu(dimwann,dimwann,nkpts), STAT=ierr )
         IF( ierr /=0 ) CALL errore(' wannier ', ' allocating cu ', dimwann**2 * nkpts)

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
            WRITE(stdout,*) '  '
            WRITE(stdout,'(a18,i4)') ' Matrix S before zgees, k-point',nkp
            WRITE(stdout,*) '  '
            IF ( dimwann <= 8 ) THEN
              DO i = 1, dimwann
                write(stdout, fmt="( 8(0pe10.2))") ( cs(i,j,nkp), j=1,dimwann )
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
            WRITE(stdout,*) '  '
            WRITE(stdout,'(a18,i4)') ' Matrix S after zgees, k-point',nkp
            WRITE(stdout,*) '  '
            IF ( dimwann <= 8 ) THEN
              DO i = 1, dimwann
                write(stdout, fmt="( 8(0pe10.2) )") ( cs(i,j,nkp), j=1,dimwann )
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
          WRITE (stdout,*) '  '
          WRITE (stdout, fmt="(2x, ' Matrix U, k-point', i3)") nkp
          WRITE (stdout,*) '  '
          DO i = 1, dimwann
            WRITE(stdout, fmt="(4x,2f9.5,2x,2f9.5,2x,2f9.5,2x,2f9.5) ") ( cu(i,j,nkp), j=1,dimwann )
          END DO
! ...     unitariety is checked
          WRITE (stdout,*) ' '
          WRITE (stdout, fmt=" (2x, 'Check unitariery of U matrix ')")
          DO i = 1, dimwann
            DO j = 1, dimwann
              ctmp1 = ( 0.d0, 0.d0 )
              ctmp2 = ( 0.d0, 0.d0 )
              DO m = 1, dimwann
                ctmp1 = ctmp1 + cu(i,m,nkp) * CONJG( cu(j,m,nkp) )
                ctmp2 = ctmp2 + cu(m,j,nkp) * CONJG( cu(m,i,nkp) )
              END DO
              WRITE (stdout,fmt="(4x,2i4,4f10.7)") i, j, ctmp1, ctmp2
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
            WRITE(stdout, fmt=" (2x, 'NB: RANDOM phase is given' )")
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
          WRITE (stdout,*) '  '
          WRITE (stdout, fmt=" (2x,' Matrix U after zgees, k-point',i3)") nkp
          WRITE (stdout,*) '  '
          DO i = 1, dimwann
            WRITE(stdout, fmt="(4x,2f9.5,2x,2f9.5,2x,2f9.5,2x,2f9.5) ")  ( cu(i,j,nkp), j=1,dimwann )
          END DO
        END IF

      END DO  ! k point loop

      ALLOCATE( cmtmp(dimwann,dimwann), STAT=ierr )
         IF( ierr /=0 ) CALL errore(' wannier ', ' allocating cu ', dimwann**2 )

! ... So now we have the U's that rotate the wavefunctions at each k-point.
!     the matrix elements M_ij have also to be updated 

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

! ... Singular value decomposition

      nwork = dimwann * 10
      ALLOCATE( singvd(dimwann), STAT=ierr )
         IF( ierr /=0 ) CALL errore(' wannier ', ' allocating singvd ', dimwann )
      ALLOCATE( cv1(dimwann,dimwann), STAT=ierr )
         IF( ierr /=0 ) CALL errore(' wannier ', ' allocating cv1 ', dimwann**2 )
      ALLOCATE( cv2(dimwann,dimwann), STAT=ierr )
         IF( ierr /=0 ) CALL errore(' wannier ', ' allocating cv2 ', dimwann**2 )
      ALLOCATE( cv3(dimwann,dimwann), STAT=ierr )
         IF( ierr /=0 ) CALL errore(' wannier ', ' allocating cv3 ', dimwann**2 )
      ALLOCATE( cw1( nwork ), STAT=ierr )
         IF( ierr /=0 ) CALL errore(' wannier ', ' allocating cw1 ', nwork )
      ALLOCATE( cw2( nwork ), STAT=ierr )
         IF( ierr /=0 ) CALL errore(' wannier ', ' allocating cw2 ', nwork )
 
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
          CALL zgesvd( 'A', 'A', dimwann, dimwann, cmtmp, dimwann,      &
               singvd, cv1, dimwann, cv2, dimwann, cw1, nwork, cw2, info )
          IF ( info /= 0 ) CALL errore(' wannier ', ' Singular value decomposition zgesvd failed 2 ', info )

          DO nb = 1, dimwann
            omiloc = omiloc + wb(nkp,nn) * ( 1.d0 - singvd(nb)**2 )
          END DO
          DO nb = 1, dimwann
            omt1 = omt1 + wb(nkp,nn) * ( 1.d0 - singvd(nb)**2 )
            omt2 = omt2 - wb(nkp,nn) * ( 2.d0 * LOG( singvd(nb) ) )
            omt3 = omt3 + wb(nkp,nn) * ( ACOS( singvd(nb) )**2 )
          END DO
        END DO
      END DO

      omt1 = omt1/DBLE(nkpts)
      omt2 = omt2/DBLE(nkpts)
      omt3 = omt3/DBLE(nkpts)

! ... Recalculate the average positions of the Wanns.

      CALL omega( dimwann, nkpts, nkpts, nntot, nnmx, nnlist, bk, wb, cm,  &
           csheet, sheet, rave, r2ave, rave2, func_om1, func_om2, func_om3, func_o , &
           rtot, r2tot, func_i, func_d, func_od)

      func_del1 = func_om1 - func_old1
      func_del2 = func_om2 - func_old2
      func_del3 = func_om3 - func_old3
      func_old1 = func_om1
      func_old2 = func_om2
      func_old3 = func_om3


! ... Find the guiding centers, and set up the 'best' Riemannian sheets for 
!     the complex logarithms

      irguide = 0
!     CALL phases( dimwann, nkpts, nkpts, nnmx, nnmxh, nntot, nnh, neigh,        &
!          bk, bka, cm, csheet, sheet, rguide, irguide )
      irguide = 1

! ... Recalculate the average positions of the Wanns.

      CALL omega( dimwann, nkpts, nkpts, nntot, nnmx, nnlist, bk, wb, cm,        &
           csheet, sheet, rave, r2ave, rave2, func_om1, func_om2, func_om3, func_o,     &
           rtot, r2tot , func_i, func_d, func_od)

      func0 = func_om1 + func_om2 + func_om3
      func_del1 = func_om1 - func_old1
      func_del2 = func_om2 - func_old2
      func_del3 = func_om3 - func_old3
      func_old1 = func_om1
      func_old2 = func_om2
      func_old3 = func_om3

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
          IF ( info /= 0 ) CALL errore(' wannier ', ' Singular value decomposition zgesvd failed 3 ', info )
          calpha = ( 1.d0, 0.d0 )
          cbeta = ( 0.d0, 0.d0 )
          CALL zgemm( 'N', 'N', dimwann, dimwann, dimwann, calpha, cv1, dimwann,   &
               cv2, dimwann, cbeta, cv3, dimwann)

          DO nb = 1, dimwann
            omt1 = omt1 + wb(nkp,nn) * ( 1.d0 - singvd(nb)**2 )
            omt2 = omt2 - wb(nkp,nn) * ( 2.d0 * LOG( singvd(nb) ) )
            omt3 = omt3 + wb(nkp,nn) * ( ACOS( singvd(nb) )**2)
          END DO

        END DO     ! loop nn
      END DO       ! loop nkp


      omt1 = omt1/DBLE(nkpts)
      omt2 = omt2/DBLE(nkpts)
      omt3 = omt3/DBLE(nkpts)

! ... Now that the consistent phase factors have been chosen
!     the wannier functions (for the R=0 cell) are calculated


      lrguide = .false.
      nrguide = 10

      lcg = .true.
      if ( ncg < 1 ) lcg = .false.

! ... But first it starts the iterative cycle to solve "Poisson" phase
    
      IF ( ncg == 0 ) ncg = ncg + 1
      ncgfix = ncg

      ALLOCATE( cu0(dimwann,dimwann,nkpts), STAT=ierr )
         IF( ierr /=0 ) CALL errore(' wannier ', ' allocating cu0 ', dimwann*2 * nkpts )
      ALLOCATE( cm0(dimwann,dimwann,nkpts,nnmx), STAT=ierr )
         IF( ierr /=0 ) CALL errore(' wannier ', ' allocating cm0 ', dimwann**2*nkpts*nnmx )
      ALLOCATE( cdodq(dimwann,dimwann,nkpts), STAT=ierr )
         IF( ierr /=0 ) CALL errore(' wannier ', ' allocating cdodq ', dimwann*2 * nkpts )
      ALLOCATE( cdqkeep(dimwann,dimwann,nkpts), STAT=ierr )
         IF( ierr /=0 ) CALL errore(' wannier ', ' allocating cdqkeep ', dimwann*2 * nkpts )
      ALLOCATE( cdodq1(dimwann,dimwann,nkpts), STAT=ierr )
         IF( ierr /=0 ) CALL errore(' wannier ', ' allocating cdodq1 ', dimwann*2 * nkpts )
      ALLOCATE( cdodq2(dimwann,dimwann,nkpts), STAT=ierr )
         IF( ierr /=0 ) CALL errore(' wannier ', ' allocating cdodq2 ', dimwann*2 * nkpts )
      ALLOCATE( cdodq3(dimwann,dimwann,nkpts), STAT=ierr )
         IF( ierr /=0 ) CALL errore(' wannier ', ' allocating cdodq3 ', dimwann*2 * nkpts )
      ALLOCATE( cdq(dimwann,dimwann,nkpts), STAT=ierr )
         IF( ierr /=0 ) CALL errore(' wannier ', ' allocating cdq ', dimwann*2 * nkpts )
      ALLOCATE( cpad1(dimwann*dimwann), STAT=ierr )
         IF( ierr /=0 ) CALL errore(' wannier ', ' allocating cpad1 ', dimwann*2 )
      ALLOCATE( cpad2(dimwann*dimwann), STAT=ierr )
         IF( ierr /=0 ) CALL errore(' wannier ', ' allocating cpad2 ', dimwann*2 )

      cdqkeep = ( 0.0d0, 0.0d0 )
      cdodq1  = ( 0.0d0, 0.0d0 )
      cdodq2  = ( 0.0d0, 0.0d0 )
      cdodq3  = ( 0.0d0, 0.0d0 )
      cdq     = ( 0.0d0, 0.0d0 )
      cdodq   = ( 0.0d0, 0.0d0 )

      CALL timing('trasf',OPR='stop')
      CALL timing('iterations',OPR='start')

      !
      !
      !  here start the iterative loop
      !
      !
      WRITE( stdout, * ) '  '
      WRITE( stdout, * ) ' ======================================================================'
      WRITE( stdout, * ) ' =                     Starting iteration loop                        ='
      WRITE( stdout, * ) ' ======================================================================'
      WRITE( stdout, * ) '  '

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

        ALLOCATE( cr(dimwann,dimwann,nkpts,nnmx), STAT=ierr )
           IF( ierr /=0 ) CALL errore(' wannier ', ' allocating cr ', dimwann*2*nkpts*nnmx )
        ALLOCATE( crt(dimwann,dimwann,nkpts,nnmx), STAT=ierr )
           IF( ierr /=0 ) CALL errore(' wannier ', ' allocating crt ', dimwann*2*nkpts*nnmx )
        ALLOCATE( rnkb(dimwann,nkpts,nnmx), STAT=ierr )
           IF( ierr /=0 ) CALL errore(' wannier ', ' allocating rnkb ', dimwann*nkpts*nnmx )

        CALL domega( dimwann, nkpts, nkpts, nntot, nnmx, nnlist, bk, wb,              &
             cm, csheet, sheet, cr, crt, rave, r2ave, rnkb, cdodq1, cdodq2, cdodq3,   &
             cdodq)

        DEALLOCATE( cr, STAT=ierr )
           IF( ierr /=0 ) CALL errore(' wannier ', ' deallocating cr ', ABS(ierr) )
        DEALLOCATE( crt, STAT=ierr )
           IF( ierr /=0 ) CALL errore(' wannier ', ' deallocating crt ', ABS(ierr) )
        DEALLOCATE( rnkb, STAT=ierr )
           IF( ierr /=0 ) CALL errore(' wannier ', ' deallocating rnkb ', ABS(ierr) )

        DO nkp = 1, nkpts
          DO m= 1, dimwann
            DO n = 1, dimwann
              gcnorm1 = gcnorm1 + REAL( cdodq(m,n,nkp) * CONJG( cdodq(m,n,nkp) ) )
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
          IF ( info /= 0 ) CALL errore ('wannier', 'wrong schur procedure', info)

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

        END DO


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
             csheet, sheet, rave, r2ave, rave2, func_om1, func_om2, func_om3, func_o,    &
             rtot, r2tot, func_i, func_d, func_od)

        IF ( ncount <= niter0 ) THEN
          IF ( ( (ncount/nprint) * nprint +1) == ncount )  THEN
            WRITE( stdout, * ) '  '
            WRITE( stdout, fmt=" (2x,'Iteration = ',i5) ") ncount
            WRITE(stdout, fmt=" (2x, 'Wannier centers and Spreads (Omega)')")
            DO nwann = 1, dimwann
               WRITE( stdout, fmt= " ( 4x, 'Center ', i3, 1x, '= (',f12.6,',',f12.6,',',&
                    &  f12.6, ' )  Omega = ', f13.6 )" )  &
                    nwann,( rave(ind,nwann), ind=1,3 ), r2ave(nwann) - rave2(nwann)
            END DO
            WRITE( stdout, * ) '  '
            WRITE( stdout, fmt= " ( 2x, '! Center Sum',    &
                 & 1x, '= (',f12.6,',',f12.6,',',f12.6,' )  Omega = ', f13.6 )" )     &
                 (rtot(i),i=1,3), r2tot
          END IF
        END IF

        funca = func_om1 + func_om2 + func_om3
        func_del1 = func_om1 - func_old1
        func_del2 = func_om2 - func_old2
        func_del3 = func_om3 - func_old3
        func_del = func_del1 + func_del2 + func_del3

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

!         WRITE(*,*) ' '
!         WRITE(*,*) 'alpha_min, f(alpha_min)', alphamin, falphamin
!         WRITE(*,*) ' '

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
            IF ( info /= 0 ) CALL errore('wannier', 'wrong Schur procedure (II)', info)

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
!         WRITE (*,8002) cfunc_exp1
!         WRITE (*,8006) cfunc_exp2
!         WRITE (*,8008) cfunc_exp3
!         WRITE (*,8010) cfunc_exp

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
               csheet, sheet, rave, r2ave, rave2, func_om1, func_om2, func_om3, func_o,    &
               rtot, r2tot, func_i , func_d, func_od)

          IF ( ( (ncount/nprint) * nprint +1) == ncount ) THEN
            WRITE( stdout, * ) '  '
            WRITE( stdout, fmt=" (2x,'Iteration = ',i5) ") ncount
            WRITE(stdout, fmt=" (2x, 'Wannier centers and Spreads (Omega)')")
            DO nwann = 1, dimwann
               WRITE( stdout, fmt= " ( 4x, 'Center ', i3, 1x, '= (',f12.6,',',f12.6,',', &
                 &  f12.6, ' )  Omega = ', f13.6 )" )  &
                 nwann,( rave(ind,nwann), ind=1,3 ), r2ave(nwann) - rave2(nwann)
            END DO
            WRITE( stdout, * ) '  '
            WRITE( stdout, fmt= " ( 2x, '! Center Sum',    &
                   & 1x, '= (',f12.6,',',f12.6,',',f12.6,' )  Omega = ', f13.6 )" )     &
                   (rtot(i),i=1,3), r2tot
          END IF

          funca = func_om1 + func_om2 + func_om3
          func_del1 = func_om1 - func_old1
          func_del2 = func_om2 - func_old2
          func_del3 = func_om3 - func_old3
          func_del = func_del1 + func_del2 + func_del3

! ...   end of the lcg skip of the optimal alpha paraphernalia 
        END IF


        func_old1 = func_om1
        func_old2 = func_om2
        func_old3 = func_om3

        func0 = funca
     
! ...   Check convergence

        IF ( ABS( func_del ) < 1.e-6 ) THEN
          WRITE( stdout, * ) '  '
          WRITE( stdout, * ) ' ======================================================================'
          WRITE( stdout, * ) ' =                     Convergence Achieved                           ='
          WRITE( stdout, * ) ' ======================================================================'
          WRITE( stdout, * ) '  '
          WRITE(stdout, fmt=" (2x, 'Final Wannier centers and Spreads (Omega)')")
          DO nwann = 1, dimwann
            WRITE( stdout, fmt= " ( 4x, 'Center ', i3, 1x, '= (',f12.6,',',f12.6,',', &
               & f12.6,' )  Omega = ', f13.6 )" )  &
               nwann,( rave(ind,nwann), ind=1,3 ), r2ave(nwann) - rave2(nwann)
          END DO
          WRITE( stdout, * ) '  '
          WRITE( stdout, fmt= " ( 2x, '! Center Sum',    &
               & 1x, '= (',f12.6,',',f12.6,',',f12.6,' )  Omega = ', f13.6 )" )     &
              (rtot(i),i=1,3), r2tot

          WRITE( stdout, * ) '  '
          WRITE( stdout, fmt="(2x, 'Spread Operator decomposition: ')")
          WRITE( stdout, fmt="(4x,'OmegaI    =   ', f15.9 ) " ) func_i
          WRITE( stdout, fmt="(4x,'OmegaD    =   ', f15.9 ) " ) func_d
          WRITE( stdout, fmt="(4x,'OmegaOD   =   ', f15.9 ) " ) func_od
          WRITE( stdout, * ) '  '
          WRITE( stdout, fmt="(4x,'Omega Tot =   ', f15.9 ) " ) func_o

          WRITE (stdout, fmt="(2x,' ')")
          WRITE (stdout, fmt="(2x,'Omega variation:')")
          WRITE (stdout, fmt="(4x,'Delta Omega 1   = ',0pe16.8)") func_del1
          WRITE (stdout, fmt="(4x,'Delta Omega 2   = ',0pe16.8)") func_del2
          WRITE (stdout, fmt="(4x,'Delta Omega 3   = ',0pe16.8)") func_del3
          WRITE (stdout, fmt="(4x,'Delta Omega Tot = ',0pe16.8)") func_del
          WRITE( stdout, * ) '  '
          WRITE (stdout, fmt="(2x,'Derivative = ', 2e12.4) ") funca-func0,doda0*alpha
          WRITE( stdout, * ) '  '
          WRITE( stdout, * ) ' ======================================================================'
          GO TO 8100
        END IF  
        ! EXIT

      END DO !end loop ncount

      WRITE( stdout, * ) '  '
      WRITE( stdout, * ) ' ======================================================================'
      WRITE( stdout, * ) ' =                 Max Number of iteration reached                    ='
      WRITE( stdout, * ) ' ======================================================================'
      WRITE( stdout, * ) '  '
      WRITE(stdout, fmt=" (2x, 'Final Wannier centers and Spreads (Omega)')")
      DO nwann = 1, dimwann
        WRITE( stdout, fmt= " ( 4x, 'Center ', i3, 1x, '= (',f12.6,',',f12.6,',',f12.6,  &
           & ' )  Omega = ', f13.6 )" )  nwann,( rave(ind,nwann), ind=1,3 ), &
                                         r2ave(nwann) - rave2(nwann)
      END DO
      WRITE( stdout, * ) '  '
      WRITE( stdout, fmt= " ( 2x, '! Center Sum',    &
           & 1x, '= (',f12.6,',',f12.6,',',f12.6,' )  Omega = ', f13.6 )" )     &
             (rtot(i),i=1,3), r2tot

      WRITE( stdout, * ) '  '
      WRITE( stdout, fmt="(2x, 'Spread Operator decomposition: ')")
      WRITE( stdout, fmt="(4x,'OmegaI    =   ', f15.9 ) " ) func_i
      WRITE( stdout, fmt="(4x,'OmegaD    =   ', f15.9 ) " ) func_d
      WRITE( stdout, fmt="(4x,'OmegaOD   =   ', f15.9 ) " ) func_od
      WRITE( stdout, * ) '  '
      WRITE( stdout, fmt="(4x,'Omega Tot =   ', f15.9 ) " ) func_o

      WRITE (stdout, fmt="(2x,' ')")
      WRITE (stdout, fmt="(2x,'Omega variation:')")
      WRITE (stdout, fmt="(4x,'Delta Omega 1   = ',0pe16.8)") func_del1
      WRITE (stdout, fmt="(4x,'Delta Omega 2   = ',0pe16.8)") func_del2
      WRITE (stdout, fmt="(4x,'Delta Omega 3   = ',0pe16.8)") func_del3
      WRITE (stdout, fmt="(4x,'Delta Omega Tot = ',0pe16.8)") func_del
      WRITE( stdout, * ) '  '
      WRITE (stdout, fmt="(2x,'Derivative = ', 2e12.4) ") funca-func0,doda0*alpha
      WRITE( stdout, * ) '  '
      WRITE( stdout, * ) ' ======================================================================'


! ... End of the minimization loop

8100  CONTINUE

      CALL timing('iterations',OPR='stop')
      CALL timing('write',OPR='start')


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
              WRITE(stdout,'(2i4,4f15.10)') i, j, ctmp1, ctmp2
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
          IF ( info /= 0 ) CALL errore('wannier', 'Singular value decomposition zgesvd failed 4', info)

          DO nb = 1, dimwann
            omt1 = omt1 + wb(nkp,nn) * ( 1.d0 - singvd(nb)**2 )
            omt2 = omt2 - wb(nkp,nn) * ( 2.d0 * LOG( singvd(nb) ) )
            omt3 = omt3 + wb(nkp,nn) * ( ACOS( singvd(nb) )**2 )
          END DO

        END DO
      END DO

      omt1 = omt1/DBLE(nkpts)
      omt2 = omt2/DBLE(nkpts)
      omt3 = omt3/DBLE(nkpts)

 
! ... Write the final unitary transformations into a file
 
      OPEN( 29, FILE='unitary.dat', STATUS='UNKNOWN', FORM='UNFORMATTED' )

      WRITE(29) ( ( ( CU(J,I,N), J=1,dimwann ), i=1,dimwann ), n=1,nkpts )

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

      DEALLOCATE( cwschur1, cwschur2, STAT=ierr )
           IF( ierr /=0 )&
           CALL errore(' wannier ', ' deallocating cwschur1 cwschur1 ', ABS(ierr) )
      DEALLOCATE( cz, STAT=ierr )
           IF( ierr /=0 ) CALL errore(' wannier ', ' deallocating cz ', ABS(ierr) )
      DEALLOCATE( cwschur3, STAT=ierr )
           IF( ierr /=0 ) CALL errore(' wannier ', ' deallocating cwschur3 ', ABS(ierr) )
      DEALLOCATE( cwschur4, STAT=ierr )
           IF( ierr /=0 ) CALL errore(' wannier ', ' deallocating cwschur4 ', ABS(ierr) )

      DEALLOCATE( lpctx, lpcty, lpctz, STAT=ierr)
           IF( ierr /=0 )  &
           CALL errore(' wannier ', ' deallocating lpctx lpcty lpctz ', ABS(ierr) )
      DEALLOCATE( nplwkp, STAT=ierr )
           IF( ierr /=0 ) CALL errore(' wannier ', ' deallocating nplwkp ', ABS(ierr) )
      DEALLOCATE( dnlkg, STAT=ierr )
           IF( ierr /=0 ) CALL errore(' wannier ', ' deallocating dnlkg ', ABS(ierr) )
      DEALLOCATE( cw1, STAT=ierr )
           IF( ierr /=0 ) CALL errore(' wannier ', ' deallocating cw1 ', ABS(ierr) )
      DEALLOCATE( cw2, STAT=ierr )
           IF( ierr /=0 ) CALL errore(' wannier ', ' deallocating cw2 ', ABS(ierr) )
      DEALLOCATE( vkpt, STAT=ierr )
           IF( ierr /=0 ) CALL errore(' wannier ', ' deallocating vkpt ', ABS(ierr) )
      DEALLOCATE( kgv, STAT=ierr )
           IF( ierr /=0 ) CALL errore(' wannier ', ' deallocating kgv ', ABS(ierr) )
      DEALLOCATE( wtkpt, STAT=ierr )
           IF( ierr /=0 ) CALL errore(' wannier ', ' deallocating wtkpt ', ABS(ierr) )
      DEALLOCATE( nfile, STAT=ierr )
           IF( ierr /=0 ) CALL errore(' wannier ', ' deallocating nfile ', ABS(ierr) )
!     DEALLOCATE( nplwkp, STAT=ierr )
!          IF( ierr /=0 ) CALL errore(' wannier ', ' deallocating nplwkp ', ABS(ierr) )
      DEALLOCATE( datake, STAT=ierr )
           IF( ierr /=0 ) CALL errore(' wannier ', ' deallocating datake ', ABS(ierr) )
      DEALLOCATE( dnlg, STAT=ierr )
           IF( ierr /=0 ) CALL errore(' wannier ', ' deallocating dnlg ', ABS(ierr) )
!     DEALLOCATE( dnlkg, STAT=ierr )
!          IF( ierr /=0 ) CALL errore(' wannier ', ' deallocating dnlgk ', ABS(ierr) )
      DEALLOCATE( ninvpw, STAT=ierr )
           IF( ierr /=0 ) CALL errore(' wannier ', ' deallocating ninvpw ', ABS(ierr) )
      DEALLOCATE( cptwfp, STAT=ierr )
           IF( ierr /=0 ) CALL errore(' wannier ', ' deallocating cptwfp ', ABS(ierr) )
      DEALLOCATE( vkpr, STAT=ierr )
           IF( ierr /=0 ) CALL errore(' wannier ', ' deallocating vkpr ', ABS(ierr) )
      DEALLOCATE( dnn, STAT=ierr )
           IF( ierr /=0 ) CALL errore(' wannier ', ' deallocating dnn ', ABS(ierr) )
      DEALLOCATE( nnshell, STAT=ierr )
           IF( ierr /=0 ) CALL errore(' wannier ', ' deallocating nnshell ', ABS(ierr) )
      DEALLOCATE( nnlist, nntot, STAT=ierr ) 
           IF( ierr /=0 ) CALL errore(' wannier ', ' deallocating nnlist ', ABS(ierr) )
      DEALLOCATE( nncell, STAT=ierr ) 
           IF( ierr /=0 ) CALL errore(' wannier ', ' deallocating nncell ', ABS(ierr) )
      DEALLOCATE( bk, STAT=ierr ) 
           IF( ierr /=0 ) CALL errore(' wannier ', ' deallocating bk ', ABS(ierr) )
      DEALLOCATE( dimsingvd, dimbk, ndim, STAT=ierr )
           IF( ierr /=0 ) CALL errore(' wannier ', ' deallocating dimsingvd ', ABS(ierr) )
      DEALLOCATE( wb, STAT=ierr )
           IF( ierr /=0 ) CALL errore(' wannier ', ' deallocating wb ', ABS(ierr) )
      DEALLOCATE( bka, STAT=ierr )
           IF( ierr /=0 ) CALL errore(' wannier ', ' deallocating bka ', ABS(ierr) )
      DEALLOCATE( neigh, STAT=ierr )
           IF( ierr /=0 ) CALL errore(' wannier ', ' deallocating neigh ', ABS(ierr) )

      DEALLOCATE( rphicmx1, STAT=ierr )
           IF( ierr /=0 ) CALL errore(' wannier ', ' deallocating rphicmx1 ', ABS(ierr) )
      DEALLOCATE( rphicmx2, STAT=ierr )
           IF( ierr /=0 ) CALL errore(' wannier ', ' deallocating rphicmx2 ', ABS(ierr) )
      DEALLOCATE( nphimx1, STAT=ierr )
           IF( ierr /=0 ) CALL errore(' wannier ', ' deallocating nphimx1 ', ABS(ierr) )
      DEALLOCATE( nphimx2, STAT=ierr )
           IF( ierr /=0 ) CALL errore(' wannier ', ' deallocating nphimx2 ', ABS(ierr) )
      DEALLOCATE( nphir, STAT=ierr )
           IF( ierr /=0 ) CALL errore(' wannier ', ' deallocating nphir ', ABS(ierr) )
      DEALLOCATE( cm, STAT=ierr )
           IF( ierr /=0 ) CALL errore(' wannier ', ' deallocating cm ', ABS(ierr) )
      DEALLOCATE( csheet, STAT=ierr )
           IF( ierr /=0 ) CALL errore(' wannier ', ' deallocating csheet ', ABS(ierr) )
      DEALLOCATE( sheet, STAT=ierr )
           IF( ierr /=0 ) CALL errore(' wannier ', ' deallocating sheet ', ABS(ierr) )
      DEALLOCATE( rave, r2ave, STAT=ierr )
           IF( ierr /=0 ) CALL errore(' wannier ', ' deallocating rave r2ave ', ABS(ierr) )
      DEALLOCATE( ca, cs, STAT=ierr )
           IF( ierr /=0 ) CALL errore(' wannier ', ' deallocating ca cs ', ABS(ierr) )
      DEALLOCATE( cu, STAT=ierr )
           IF( ierr /=0 ) CALL errore(' wannier ', ' deallocating cu ', ABS(ierr) )
      DEALLOCATE( cmtmp, STAT=ierr )
           IF( ierr /=0 ) CALL errore(' wannier ', ' deallocating cmtmp ', ABS(ierr) )
      DEALLOCATE( singvd, STAT=ierr )
           IF( ierr /=0 ) CALL errore(' wannier ', ' deallocating singvd ', ABS(ierr) )
      DEALLOCATE( cv1, STAT=ierr )
           IF( ierr /=0 ) CALL errore(' wannier ', ' deallocating cv1 ', ABS(ierr) )
      DEALLOCATE( cv2, STAT=ierr )
           IF( ierr /=0 ) CALL errore(' wannier ', ' deallocating cv2 ', ABS(ierr) )
      DEALLOCATE( cv3, STAT=ierr )
           IF( ierr /=0 ) CALL errore(' wannier ', ' deallocating cv3 ', ABS(ierr) )
      DEALLOCATE( cu0, STAT=ierr )
           IF( ierr /=0 ) CALL errore(' wannier ', ' deallocating cu0 ', ABS(ierr) )
      DEALLOCATE( cm0, STAT=ierr )
           IF( ierr /=0 ) CALL errore(' wannier ', ' deallocating cm0 ', ABS(ierr) )
      DEALLOCATE( cdodq, STAT=ierr )
           IF( ierr /=0 ) CALL errore(' wannier ', ' deallocating cdodq ', ABS(ierr) )
      DEALLOCATE( cdqkeep, STAT=ierr )
           IF( ierr /=0 ) CALL errore(' wannier ', ' deallocating cdqkeep ', ABS(ierr) )
      DEALLOCATE( cdodq1, STAT=ierr )
           IF( ierr /=0 ) CALL errore(' wannier ', ' deallocating cdodq1 ', ABS(ierr) )
      DEALLOCATE( cdodq2, STAT=ierr )
           IF( ierr /=0 ) CALL errore(' wannier ', ' deallocating cdodq2 ', ABS(ierr) )
      DEALLOCATE( cdodq3, STAT=ierr )
           IF( ierr /=0 ) CALL errore(' wannier ', ' deallocating cdodq3 ', ABS(ierr) )
      DEALLOCATE( cdq, STAT=ierr )
           IF( ierr /=0 ) CALL errore(' wannier ', ' deallocating cdq ', ABS(ierr) )
      DEALLOCATE( cpad1, STAT=ierr )
           IF( ierr /=0 ) CALL errore(' wannier ', ' deallocating cpad1 ', ABS(ierr) )
      DEALLOCATE( cpad2, STAT=ierr )
           IF( ierr /=0 ) CALL errore(' wannier ', ' deallocating cpad2 ', ABS(ierr) )

      CALL deallocate_input()

      CALL timing('write',OPR='stop')
      CALL timing('wannier',OPR='stop')
      CALL timing_overview(stdout,LIST=global_list,MAIN_NAME='wannier')
      CALL timing_deallocate()

      STOP '*** THE END *** (wannier.f90)'
      END PROGRAM wannier
