       PROGRAM intf

       USE kinds
       USE constants, ONLY: pi, twopi => tpi, &
         ryd => ry, har => au, bohr => bohr_radius_angs

       USE parameters, ONLY: mxdtyp => npsx, mxdatm => natx

       IMPLICIT NONE

       INTEGER :: mxddim, mxdbnd, nkpts, mxdgve
 
       REAL(dbl) :: emax, enmax
       INTEGER :: nkpts1

       CHARACTER( LEN=2 ) :: nameat(mxdtyp)
       CHARACTER( LEN=4 ) :: vdriv
       INTEGER :: ntype, natom(mxdtyp)
       REAL(dbl) :: rat(3,mxdatm,mxdtyp)
       REAL(dbl), ALLOCATABLE :: evecr(:,:,:), eveci(:,:,:)
 
       INTEGER :: mtxd
       INTEGER :: neig,n_paral(3), n_perpend(3)
       INTEGER :: i1, i2, i3, i, j, ind(3)
       INTEGER, ALLOCATABLE :: kgv(:,:), kisort(:) ! kgv(3,mxdgve), kisort(mxddim)
       REAL(dbl) :: s(3)
       REAL(dbl) :: temp(3,3), ibvect(3,3)
       COMPLEX(dbl), ALLOCATABLE :: vec(:,:) ! vec(mxddim,mxdbnd) 
 
       INTEGER :: ngx, ngy, ngz, nionst 
       INTEGER :: nions, nbands, npspts
       INTEGER :: nrlpts, nrgrpt, mxrlnl, mxrlsh

       INTEGER :: nplwv
 
       INTEGER :: iprint
       PARAMETER ( iprint = 1 )

       REAL(dbl) :: dirc(3,3), recc(3,3), volc
       REAL(dbl), ALLOCATABLE :: wtkpt(:) 
       REAL(dbl), ALLOCATABLE :: vkpt(:,:), dnlg(:,:,:)     ! vkpt(3,nkpts), dnlg(mxddim,3,nkpts)
       REAL(dbl), ALLOCATABLE :: dnlkg(:,:,:),datake(:,:,:) ! dnlkg(mxddim,0:3,nkpts),datake(7,mxddim,nkpts)
       INTEGER, ALLOCATABLE :: lpctx(:),lpcty(:),lpctz(:)   ! lpctx(ngx),lpcty(ngy),lpctz(ngz)
       INTEGER, ALLOCATABLE :: nindpw(:,:)                  ! nindpw(mxddim,nkpts)
       INTEGER, ALLOCATABLE :: nplwkp(:)                    ! nplwkp(nkpts) 
       INTEGER, ALLOCATABLE :: ninvpw(:,:)                  ! ninvpw(0:nplwv,nkpts)
       INTEGER, ALLOCATABLE :: nfile(:)                     ! nfile(nkpts)

       COMPLEX(dbl), ALLOCATABLE :: cptwfp(:,:,:)
       INTEGER :: ni, nj, nx, ny, nz, nsp 
       INTEGER :: m, nkp, np, npoint, nunit, inpout, iib
       INTEGER :: idum, l
       REAL(dbl) :: wtktot
 
       INTEGER :: dimwann
       INTEGER :: kdimwin, nk(3)
       COMPLEX(dbl), ALLOCATABLE :: amp(:,:,:) !  amp(mxdbnd,mxdbnd,nkpts)
 
       REAL(dbl) :: rdum
       REAL(dbl), ALLOCATABLE :: keiw(:) ! keiw(mxdbnd)
       COMPLEX(dbl), ALLOCATABLE :: lvec(:,:) !  lvec(mxddim,mxdbnd)
     
       REAL(dbl) :: alatt
       REAL(dbl) :: vcell, avec(3,3), bvec(3,3), aminv(3) 
       REAL(dbl) :: adot(3,3), bdot(3,3), rkpt(3)
       REAL(dbl) :: win_min, win_max, froz_min, froz_max
       REAL(dbl) :: sgn
       REAL(dbl) :: alpha
       INTEGER :: nbandi
       INTEGER :: nt, ja
       INTEGER :: maxiter, itrial, nshelLs, ngm
       INTEGER, ALLOCATABLE :: nwhich(:)

       INTEGER :: iphase
       REAL(dbl)  :: alphafix0, alphafix
       INTEGER :: niter, niter0, ncg

       LOGICAL, ALLOCATABLE :: frozen(:)
       INTEGER, ALLOCATABLE :: ifrozdum(:)
       INTEGER :: dimfroz
!
! ...  End declarations and dimensions
!
!=----------------------------------------------------------------------------=!
!
! ...  Read input parameters from window.out
!
       rat    = 0.0d0
       natom  = 0
       nameat = ' '

       OPEN( UNIT=19, FILE='takeoff.dat', STATUS='OLD', FORM='UNFORMATTED' )

       READ(19) alatt
       READ(19) (avec(i,1),i=1,3)
       READ(19) (avec(i,2),i=1,3)
       READ(19) (avec(i,3),i=1,3)
       READ(19) ntype
       DO nt = 1, ntype
         READ(19) natom(nt), nameat(nt)
         DO ja=1, natom(nt)
           READ(19) ( rat(i,ja,nt), i=1,3 )
         END DO
       END DO
       READ(19) emax, nbandi
       READ(19) (nk(i),i=1,3), (s(i),i=1,3)
       READ(19) win_min, win_max, froz_min, froz_max, dimwann

       CALL atomset(alatt, avec, ntype, natom, nameat, rat, mxdtyp, mxdatm)
       CALL latti(avec, bvec, vcell, bdot, aminv, adot)

       READ(19) alpha, maxiter 
       READ(19) iphase
       READ(19) niter0, alphafix0
       READ(19) niter, alphafix, ncg
       READ(19) itrial, nshells

       ALLOCATE( nwhich(nshells) )
       READ(19) (nwhich(i),i=1,nshells)
       READ(19) nkpts, mxddim, mxdbnd
       READ(19) ngx, ngy, ngz, ngm
       DEALLOCATE ( nwhich )

       READ(19) rdum
       READ(19) rdum
       READ(19) rdum
       READ(19) idum
       READ(19) idum
       READ(19) idum
       READ(19) rdum


! ...  Convert emax to eV

       enmax = emax * har
       WRITE(*,*) ' '
       WRITE(*,*) 'EMAX = ', emax, ' HAR', ', ENMAX = ', enmax, ' eV'
       WRITE(*,*) ' '

! ...  Read grid information, and G-vectors

       READ(19) mxdgve 
       ALLOCATE( kgv(3,mxdgve) )
       READ(19) ( ( kgv(i,j), i=1,3 ), j=1,mxdgve )

       nkpts1 = nk(1) * nk(2) * nk(3)

       IF( nkpts1 /= nkpts ) THEN
         WRITE(*,*) 'NKPTS1 = ', nkpts1, ', NKPTS =', nkpts
         WRITE(*,*) 'Change NKPTS'
         STOP
       END IF

       IF( DIMWANN > MXDBND ) THEN
         WRITE(6,*) '*** INCREASE MXDBND ***'
         STOP
       END IF

      DO i = 1, 3
        DO j = 1, 3
          dirc(i,j) = avec(j,i) * bohr
        END DO
      END DO

      CALL bastr( dirc, recc, volc )

      WRITE(*,*)' THE BASIS-SET USED WHEN PLACING ATOMS ( DIRC(I,J) ):'
      WRITE(*,*) ' '
      WRITE(*,101) ( ( dirc(i,j), i=1,3 ), j=1,3 )
      WRITE(*,*) ' '
      WRITE(*,*) ' THE RECIPROCAL SPACE BASIS ( RECC(I,J) ): '
      WRITE(*,*) ' '
      WRITE(*,101) ( ( recc(i,j), i=1,3 ), j=1,3 )
 101  FORMAT(1x,3f10.6)

      WRITE (*,*) ' '
      WRITE(*,*) ' NOTE: VECTORS A1 A2 A3 AND B1 B2 B3 ARE GIVEN IN *COLUMNS* '
      WRITE (*,*) ' '
      WRITE (*,*) ' VOLUME OF CURRENT UNIT CELL:', VOLC

! ... Read in atomic positions (same as payne's original one)

      WRITE (*,*) ' '

      ALLOCATE( vkpt( 3, nkpts ) )
      ALLOCATE( wtkpt( nkpts ) )

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

      wtkpt(1:nkpts) = 1.0d0/DBLE(nkpts)

      DO nkp = 1 , nkptS
         WRITE (*,7014) nkp, vkpt(1,nkp), vkpt(2,nkp), vkpt(3,nkp), wtkpt(nkp)
      END DO
 7014 FORMAT('  SPECIAL K-POINT', i6, ':', 3f10.5, '   WEIGHT =', f10.5)

      wtktot = sum( wtkpt( 1:nkpts ) ) 

      IF ( ABS(wtktot - 1.0d0) >= 1.0d-8 ) THEN
        WRITE(*,*) ' '
        WRITE(*,*) ' ERROR: k-points weights are not normalized', wtktot
        STOP
      END IF

      WRITE (*,*) ' '
 
 
! ... Generate the array ninvpw
!     The next chunck of code is copied from wannier

! ... Initialize the loop counters lpctx,lpcty,lpctz that
!     label the number of the reciprocal lattice vectors in the x,y,z
!     directions, respectively. for the x direction the reciprocal lattice
!     vectors corresponding to the first,second,...,ngxth elements in all
!     of the reciprocal lattice arrays are 0,1,..,(ngx/2),-((ngx/2-1),..,-1
!     times the x reciprocal lattice vector recc(1,*)

      ALLOCATE ( lpctx(ngx), lpcty(ngy), lpctz(ngz) )

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

      ALLOCATE( cptwfp( mxddim+1, dimwann, nkpts ) )
      ALLOCATE( nplwkp( nkpts ) )
      ALLOCATE( datake( 7, mxddim, nkpts ) )
      ALLOCATE( dnlg( mxddim, 3, nkpts ) )
      ALLOCATE( dnlkg( mxddim, 0:3, nkpts ) ) 

      CALL genbtr( mxddim, ngx, ngy, ngz, nkpts, enmax, nindpw, nplwkp, vkpt,  &
           lpctx, lpcty, lpctz, datake, recc, recc, iprint, dnlg, dnlkg )



! ... Check G-space dimensions 

       DO nkp = 1, nkpts
         IF( nplwkp(nkp) > mxddim ) THEN
           WRITE(*,*) '*** INCREASE MXDDIM ***'
           WRITE(*,*) 'FOR NKP = ', nkp,', NPLWKP = ', nplwkp(nkp), 'AND MXDDIM = ', mxddim
           STOP
         END IF
       END DO


      nplwv = ngx * ngy * ngz
      WRITE(6,*) 'DEBUG nplwv = ', nplwv

      ALLOCATE( ninvpw(0:nplwv,nkpts) )

      DO nkp = 1, nkpts
        DO np = 0, nplwv
          ninvpw( np, nkp ) = mxddim + 1
        END DO
        DO np = 1, nplwkp( nkp )
          npoint = nindpw( np, nkp )
          ninvpw( npoint, nkp ) = np
        END DO
      END DO

      ! write(99,fmt='(I5)') ninvpw

! ... Read the energy eigenfunctions within the energy window at each K-point, 
!     the subspace basis vectors, and convert the latter to CASTEP format and write
!     them in output file "intf.out", that will be read by wannier

      OPEN( UNIT=8, FILE='subspace.dat', STATUS='OLD', FORM='UNFORMATTED' )
      OPEN( UNIT=20, FILE='onfly.dat', FORM='UNFORMATTED')

! ... The following 2 lines are have been added by ANDREA (28 jan 2004)
!     in order to account for a FORMAT change needed to manage self-energies
!     conversion to wannier basis
! 
!     some data are skipped
! 
      READ(8)
      READ(8)

      ALLOCATE( kisort(mxddim) )
      ALLOCATE( keiw(mxdbnd) )
      ALLOCATE( evecr(mxddim,mxdbnd,nkpts) )
      ALLOCATE( eveci(mxddim,mxdbnd,nkpts) )
      ALLOCATE( amp(mxdbnd,mxdbnd,nkpts) )
      ALLOCATE( lvec(mxddim,mxdbnd) )

! ... K-point loop

       K_POINTS_DIM: DO nkp = 1, nkpts
         !
         ! format changes by ANDREA (28 jan 2004) in the following 2 lines 
         !
         READ(19) mtxd,( kisort(j), j=1, mtxd )
         READ(19) idum, idum, kdimwin, ( keiw(j), j=1, kdimwin )

         WRITE(*,*) 'KPT= ', nkp, ' mtxd = ', mtxd, ' dimwin= ', kdimwin

         IF ( mtxd > mxddim ) THEN
           WRITE(*,*) '*** SOMETHING WRONG ***' 
           WRITE(*,*) 'THE NUMBER OF G-VECTORS MTXD (', mtxd,'MTXD) AT K-POINT=', nkp
           WRITE(*,*) 'IS GREATER THAN THE MAXIMUN NUMBER OF AVAILABLE G-VECTORS!!'
           STOP
         END IF
         IF ( mtxd /= nplwkp(nkp) ) THEN
           WRITE(*,*) 'FOR K-POINT ', nkp, ' MTXD = ', mtxd, ', NPLWKP = ', nplwkp(nkp)
           WRITE(*,*) 'POSSIBLE PROBLEM: NGX,NGY,NGZ TOO SMALL'
           STOP
         END IF

       END DO K_POINTS_DIM

       K_POINTS: DO nkp = 1, nkpts

         READ(19)( ( evecr(j,i,nkp), j=1,mtxd ), i=1, kdimwin )
         READ(19)( ( eveci(j,i,nkp), j=1,mtxd ), i=1, kdimwin )

         ALLOCATE( frozen( kdimwin ) )
         READ(19) dimfroz, ( frozen(i), i=1, kdimwin )
         ALLOCATE( ifrozdum( MAX( kdimwin, dimfroz ) ) )
         IF ( dimfroz > 0 )        READ(19) ( ifrozdum(i), i=1, dimfroz )
         IF ( dimfroz < kdimwin )  READ(19) ( ifrozdum(i), i=1, kdimwin-dimfroz )
         DEALLOCATE( frozen )
         DEALLOCATE( ifrozdum )


 
! ...    Read optimal subspace basis vectors at present K-point 
 
         READ(8) idum, ( ( amp(j,i,nkp), j=1,kdimwin ), i=1,dimwann )

         IF ( idum /= dimwann ) STOP '*** SOMETHING WRONG ***'

! ...    Compute fourier coefficients of basis vectors at present K-point 
 
         DO l = 1, dimwann
           DO i = 1, mtxd
             lvec(i,l) = cmplx(0.0d0,0.0d0)
             DO j = 1, kdimwin
               lvec(i,l) = lvec(i,l) + amp(j,l,nkp) * cmplx( evecr(i,j,nkp), eveci(i,j,nkp) )
             END DO
           END DO
         END DO
 
! ...    Go through all the g-vectors inside the cutoff radius at the present K-Point
 
         G_VECTORS: DO j = 1, nplwkp(nkp)           ! *** SAME AS MTXD ***

           IF ( ABS(kgv(1,kisort(j))) > ngx ) THEN
             WRITE(*,*) 'KGV(1,', kisort(j), ') GRATER THAN NGX'
             STOP
           END IF
           IF ( kgv(1,kisort(j)) >= 0 ) nx = kgv(1,kisort(j)) + 1
           IF ( kgv(1,kisort(j)) <  0 ) nx = kgv(1,kisort(j)) + 1 + ngx

           IF ( ABS(kgv(2,kisort(j))) > ngy ) THEN
             WRITE(*,*) 'KGV(2,', kisort(j), ') GRATER THAN NGY'
             STOP
           END IF
           IF ( kgv(2,kisort(j)) >= 0 ) ny = kgv(2,kisort(j)) + 1
           IF ( kgv(2,kisort(j)) <  0 ) ny = kgv(2,kisort(j)) + 1 + ngy

           IF ( ABS(kgv(3,kisort(j))) > ngz ) THEN
             WRITE(*,*) 'KGV(3,', kisort(j), ') GRATER THAN NGZ'
             STOP
           END IF
           IF ( kgv(3,kisort(j)) >= 0 ) nz = kgv(3,kisort(j)) + 1
           IF ( kgv(3,kisort(j)) <  0 ) nz = kgv(3,kisort(j)) + 1 + ngz

           npoint = nx + (ny-1)*ngx + (nz-1)*ngx*ngy

           ! write(6,*) ' - npoint ', npoint, ninvpw( npoint, nkp ), nx, ny, nz

           BANDS: DO I=1,dimwann                     
 
! ...      Conjg is due to the opposite bloch convention in CASTEP 
 
             cptwfp(ninvpw(npoint,nkp),i,nkp) = conjg(lvec(j,i))

             !  WRITE(50,1122) CPTWFP(NINVPW(NPOINT,NKP),I,NKP), NINVPW(NPOINT,NKP),I,NKP
1122         FORMAT(2D23.15,3I6) 

             IF ( ninvpw( npoint, nkp ) > nplwkp( nkp ) .OR. ninvpw(npoint,nkp) <= 0 ) THEN
               WRITE(*,*) ninvpw(npoint,nkp)
               WRITE(*,*) 'NINVPW OUT OF BOUNDS'
               STOP
             END IF          

           END DO BANDS

         END DO G_VECTORS

         WRITE(20) SIZE( cptwfp, 1), SIZE( cptwfp, 2), SIZE( cptwfp, 3), &
           nplwkp( nkp ), dimwann
         WRITE(20) ( ( cptwfp(np,iib,nkp), np=1, nplwkp(nkp) ), iib=1, dimwann )  




       END DO K_POINTS

       CLOSE(8)
       CLOSE(19)
       CLOSE(20)

       DEALLOCATE( cptwfp )
       DEALLOCATE( kgv )
       DEALLOCATE( vkpt )
       DEALLOCATE( wtkpt )
       DEALLOCATE( lpctx, lpcty, lpctz )
       DEALLOCATE( nindpw )
       DEALLOCATE( nplwkp )
       DEALLOCATE( datake )
       DEALLOCATE( dnlg )
       DEALLOCATE( dnlkg ) 
       DEALLOCATE( ninvpw )
       DEALLOCATE( kisort )
       DEALLOCATE( keiw )
       DEALLOCATE( evecr )
       DEALLOCATE( eveci )
       DEALLOCATE( amp )
       DEALLOCATE( lvec )

! *****************************************************************************

       STOP '*** THE END *** (intf.f90)'
       END

