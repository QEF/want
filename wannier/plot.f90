      PROGRAM plot

      USE kinds
      USE constants, ONLY: pi, twopi => tpi, bohr => bohr_radius_angs
      USE parameters, ONLY: npsx, natx
      USE fft_scalar, ONLY: cfft3d

      IMPLICIT NONE

      INTEGER :: ngt, ngs, ngrid
      PARAMETER ( ngt = 2, ngs = ngt-1, ngrid = 4 )

      INTEGER :: ngx, ngy, ngz
      INTEGER :: nrplwv
      INTEGER :: nionst, nions, nspec, nbands, nkpts
      INTEGER :: nplwv
      INTEGER :: mplwv
      INTEGER :: nionbig
      INTEGER :: npoint, iplwv, jplwv
      INTEGER :: nwann, nzz, nyy, nxx
      INTEGER :: nx, ny, nz, nw1, nw2, nw, l
      INTEGER :: nx2, ny2, nz2, nii
      INTEGER :: nkp, nb, m, nsp
      INTEGER :: ngptar(3)
      INTEGER :: ngptwann(3)
      INTEGER :: n, j, nrx, nry, ni
      INTEGER :: i, nmod, nt, nlim
      INTEGER, ALLOCATABLE :: nplwkp(:)           ! nplwkp(nkpts)
      INTEGER, ALLOCATABLE :: nindpw(:,:)         ! nindpw(nrplwv,nkpts)
      INTEGER :: nionsp(npsx)           ! nionsp(nspec)

      COMPLEX(dbl), ALLOCATABLE :: cptwfp(:,:,:)  ! cptwfp(nrplwv+1,nbands,nkpts)
      COMPLEX(dbl), ALLOCATABLE :: cwann(:,:,:)   ! cwann(-ngx:ngs*ngx-1,-ngy:ngs*ngy-1,-ngz:ngs*ngz-1)
      COMPLEX(dbl), ALLOCATABLE :: cptwr(:)       ! cptwr(mplwv)
      COMPLEX(dbl), ALLOCATABLE :: cwork2(:)      ! cwork2(mplwv)
      COMPLEX(dbl), ALLOCATABLE :: cu(:,:,:)      ! cu(nbands,nbands,nkpts)
      COMPLEX(dbl), ALLOCATABLE :: cwfft1(:)      ! cwfft1(iplwv)
      COMPLEX(dbl), ALLOCATABLE :: cwfft2(:)      ! cwfft2(iplwv)
      COMPLEX(dbl) :: catmp
      COMPLEX(dbl) :: cmod
      COMPLEX(dbl) :: citpi

      REAL(dbl), ALLOCATABLE :: vkpt(:,:)         ! vkpt(3,nkpts)

      REAL(dbl) :: scalf, tmaxx, tmax, ratmax, ratio
      REAL(dbl) :: poscar( 3, ngrid+1, ngrid+1 )
      REAL(dbl) :: posrel( 3, ngrid+1, ngrid+1 )
      REAL(dbl) :: gridx, gridzx, gridy, gridzy
      REAL(dbl) :: dirc( 3, 3 ), recc( 3, 3 ), dirl( 3, 3 )
      REAL(dbl) :: x
      REAL(dbl) :: gxx, gyy, gzz, testcubic
      REAL(dbl) :: x_0ang, y_0ang, z_0ang 
      REAL(dbl) :: poscartbig( 3, natx*ngt, npsx )
      REAL(dbl) :: posion( 3, natx, npsx )

      CHARACTER( LEN=11 ) :: frfft
      CHARACTER( LEN=11 ) :: fifft

      INTEGER :: dimwann, mxddim

      PARAMETER ( citpi = ( 0.0d0, twopi) )

! ... End declarations and dimensions

      OPEN ( 21, FILE = 'landing.dat', STATUS='old', FORM='UNFORMATTED' )

      READ (21) nkpts, ngx, ngy, ngz, mplwv, dimwann, mxddim, nspec
      READ (21) recc(1,1), recc(2,1), recc(3,1)
      READ (21) recc(1,2), recc(2,2), recc(3,2)
      READ (21) recc(1,3), recc(2,3), recc(3,3)
      READ (21) dirc(1,1), dirc(2,1), dirc(3,1)
      READ (21) dirc(1,2), dirc(2,2), dirc(3,2)
      READ (21) dirc(1,3), dirc(2,3), dirc(3,3)

      nbands = dimwann
      nrplwv = mxddim
      

      ALLOCATE( vkpt( 3, nkpts ) )
      DO  nkp = 1 , nkpts
        READ (21) vkpt(1,nkp), vkpt(2,nkp), vkpt(3,nkp)
      END DO

      DO nsp = 1, nspec
        READ(21) nionsp(nsp)
      END DO

      DO nsp = 1, nspec
        DO ni  = 1, nionsp(nsp)
          READ(21) ( posion( i, ni, nsp ), i=1,3 )
        END DO
      END DO

      ALLOCATE( nplwkp( nkpts ) )
      DO nkp = 1, nkpts
        READ(21) nplwkp(nkp)
      END DO

      ALLOCATE( nindpw( MAX(nrplwv,mplwv),nkpts) )
      DO nkp = 1, nkpts
        DO n = 1, mplwv
          READ(21) nindpw(n,nkp)
        END DO
      END DO

      ALLOCATE( cptwfp( nrplwv + 1, nbands, nkpts ) )
      DO nkp = 1, nkpts
        DO nb = 1, nbands
          DO m = 1, nplwkp(nkp)
            READ(21) cptwfp(m,nb,nkp)
          END DO
        END DO
      END DO

      CLOSE(21)

!     PARAMETER ( nplwv = ngx*ngy*ngz, mplwv=ngx*ngy*(ngz+1) )
!     PARAMETER ( iplwv = ( ngt*ngx ) * ( ngt*ngy ) * ( ngt*ngz+1) )
!     PARAMETER ( jplwv = ( ngt*ngx ) * ( ngt*ngy ) * ( ngt*ngz ) ) 
!     PARAMETER ( nionbig = nions * ngt * ngt * ngt)

      OPEN( 29, FILE='unitary.dat', STATUS='OLD', FORM='UNFORMATTED' )

      ALLOCATE( cu( nbands, nbands, nkpts ) )
      READ(29) ( ( ( cu(j,i,n), j=1,nbands ), i=1,nbands ), n=1,nkpts )

      CLOSE(29)

! ... Initialize the data used for the fast fourier transforms

      ngptar(1) = ngx
      ngptar(2) = ngy
      ngptar(3) = ngz

      READ(5,*) nwann
      PRINT*, 'plotting WF n. ',nwann

      ALLOCATE ( cwann( -ngx:ngs*ngx-1, -ngy:ngs*ngy-1, -ngz:ngs*ngz-1 ) ) 
      cwann = ( 0.0d0, 0.0d0 )
!
      ALLOCATE( cptwr(mplwv) )
      ALLOCATE( cwork2(mplwv) )

      DO nkp = 1, nkpts
        DO nb = 1, nbands

          DO m = 1, mplwv
            cptwr(m) = ( 0.0d0, 0.0d0 )
            cwork2(m) = ( 0.0d0, 0.0d0 )
          ENDDO

          DO m = 1, nplwkp(nkp)
            cptwr( nindpw(m,nkp) ) = cptwfp( m, nb, nkp )
          END DO
 
!          CALL fft3d( cptwr(1,nb), cwork2, ngptar, 1 )
          CALL cfft3d( cptwr, ngptar(1), ngptar(2), ngptar(3), ngptar(1), ngptar(2), ngptar(3), -1 )

          DO m = 1, mplwv
            cptwr(m) = conjg(cptwr(m)) * ngptar(1) * ngptar(2) * ngptar(3)
          END DO

          DO nzz = -ngz, ngs*ngz-1
            nz = MOD(nzz,ngz)
            IF( nz < 1 ) nz = nz + ngz
            DO nyy = -ngy, ngs*ngy-1
              ny = MOD( nyy, ngy )
              IF ( ny < 1 ) ny = ny + ngy
              DO nxx = -ngx, ngs*ngx-1
                nx = MOD( nxx, ngx )
                IF ( nx < 1 ) nx = nx + ngx

                scalf = vkpt(1,nkp) * DBLE(nxx-1) / DBLE(ngx) + &
                        vkpt(2,nkp) * DBLE(nyy-1) / DBLE(ngy) + &
                        vkpt(3,nkp) * DBLE(nzz-1) / DBLE(ngz)
                npoint = nx + (ny-1) * ngx + (nz-1) * ngy * ngx
                catmp = EXP( citpi*scalf ) * cptwr(npoint) * cu(nb,nwann,nkp)
                IF ( scalf /= 0.0d0 ) THEN
                ELSE
                 continue
                ENDIF
 
! ...           Here it increments the wannier funcion in nx,ny,nz with the current nb,nkp bloch orbitals 

                cwann(nxx,nyy,nzz) = cwann(nxx,nyy,nzz) + catmp
              END DO
            END DO
          END DO
         
        END DO  ! nband
      END DO    ! nkp

! ... Fix the global phase by setting the wannier to be real at the point where it has max modulus

      tmaxx = 0.0d0
      cmod = ( 1.0d0, 0.0d0 )

      DO nzz = -ngz, ngs*ngz-1
        DO nyy = -ngy, ngs*ngy-1
          DO nxx= -ngx, ngs*ngx-1
            cwann(nxx,nyy,nzz) = cwann(nxx,nyy,nzz) / DBLE(nkpts)
            tmax = cwann(nxx,nyy,nzz) * CONJG(cwann(nxx,nyy,nzz))
            IF ( tmax > tmaxx ) THEN
              tmaxx = tmax
              cmod = cwann(nxx,nyy,nzz)
            END IF
          END DO
        END DO
      END DO

      cmod = cmod / SQRT( REAL( cmod )**2 + AIMAG ( cmod )**2 )
      DO nzz = -ngz, ngs*ngz-1
        DO nyy = -ngy, ngs*ngy-1
          DO nxx = -ngx, ngs*ngx-1
            cwann(nxx,nyy,nzz) = cwann(nxx,nyy,nzz) / cmod
          END DO
        END DO
      END DO

! ... Now define the cartesian grid of points poscar, that is going to be  plotted 

      write(*,*) ' '
      write(*,*) ' POSCAR ---  ' 
      write(*,*) ' '

! ... This is to be made more user-friendly

      DO nry = 1, ngrid+1
        DO nrx = 1, ngrid+1
          poscar(1,nrx,nry) = 10.0d0 * DBLE(nrx-1) / DBLE(ngrid)
          poscar(2,nrx,nry) = 10.0d0 * DBLE(nry-1) / DBLE(ngrid)
          poscar(3,nrx,nry) = 0.0d0
       END DO
      END DO

      gridx = 10.0d0
      gridzx = 0.0d0
      gridy = 10.0d0
      gridzy = 0.0d0

! ... And pass it into reciprocal coordinates 

      write(*,*) ' '
      write(*,*) ' POSREL ---  ' 
      write(*,*) ' '

      DO nrx = 1, ngrid+1
        DO nry = 1, ngrid+1
          DO m = 1, 3
            posrel( m, nrx, nry ) = 0.0d0
            DO j = 1, 3
              posrel(m,nrx,nry) = posrel(m,nrx,nry) + poscar(j,nrx,nry) * recc(m,j)
            END DO
            posrel(m,nrx,nry) = posrel(m,nrx,nry) / twopi
            posrel(m,nrx,nry) = ( posrel(m,nrx,nry) + 1.0d0 ) / DBLE(ngt)
          END DO
        END DO
      END DO

! ... Now pass the wannier functions in reciprocal space, to do the Fourier interpolation
      iplwv = ( ngt*ngx ) * ( ngt*ngy ) * ( ngt*ngz+1)
      jplwv = ( ngt*ngx ) * ( ngt*ngy ) * ( ngt*ngz )

      ngptwann(1) = ngt * ngx
      ngptwann(2) = ngt * ngy
      ngptwann(3) = ngt * ngz

      ALLOCATE( cwfft1(iplwv) )
      ALLOCATE( cwfft2(iplwv) )
      cwfft1 = ( 0.0d0, 0.0d0 )
      cwfft2 = ( 0.0d0, 0.0d0 )

      m = 1
      DO nzz = -ngz, ngs*ngz-1
        DO nyy = -ngy, ngs*ngy-1
          DO nxx = -ngx, ngs*ngx-1

! ...       One could here impose the reality of the wannier functions cwfft1(m)=real(cwann....

            cwfft1(m) = cwann(nxx,nyy,nzz) / jplwv
            m = m + 1
          END DO
        END DO
      END DO

!      CALL fft3d( cwfft1, cwfft2, ngptwann, -1 )
      CALL cfft3d( cwfft1, ngptwann(1), ngptwann(2), ngptwann(3), ngptwann(1), ngptwann(2), ngptwann(3), 1)

      WRITE(*,*) ' '
      WRITE(*,*) 'FFT(1)',cwfft1(1),m-1,jplwv
      WRITE(*,*) ' '

! ... We prepare a graphic output for gopenmol or for dan, depending on the geometry of the cell; 
!     if it is orthorombic, gopenmol is used (this includes the cubic case); otherwise, dan is used.

! ... This is to create the WF...gau output, to be read by gOpenMol (coordinates + isosurfaces)

      DO nsp = 1, nspec
        DO nii = 1, nionsp(nsp)
          DO nx = -1, ngs-1
            DO ny = -1, ngs-1
              DO nz = -1, ngs-1
                ni = ( 1 + (nii-1) * ngt**3 ) + (nx+1) * ngt * ngt + (ny+1) * ngt + nz + 1
                DO m = 1, 3
                  poscartbig(m,ni,nsp) = &
                  ( posion(1,nii,nsp) + DBLE(nx) ) * dirc(1,m) + &
                  ( posion(2,nii,nsp) + DBLE(ny) ) * dirc(2,m) + &
                  ( posion(3,nii,nsp) + DBLE(nz) ) * dirc(3,m)
                  poscartbig(m,ni,nsp) = poscartbig(m,ni,nsp) / bohr
                END DO
              END DO
            END DO
          END DO
        END DO
      END DO

      x_0ang = -DBLE(ngx+1) / DBLE(ngx) * dirc(1,1) - &
                DBLE(ngy+1) / DBLE(ngy) * dirc(2,1) - &
                DBLE(ngz+1) / DBLE(ngz) * dirc(3,1)
      y_0ang = -DBLE(ngx+1) / DBLE(ngx) * dirc(1,2) - &
                DBLE(ngy+1) / DBLE(ngy) * dirc(2,2) - &
                DBLE(ngz+1) / DBLE(ngz) * dirc(3,2)
      z_0ang = -DBLE(ngx+1) / DBLE(ngx) * dirc(1,3) - &
                DBLE(ngy+1) / DBLE(ngy) * dirc(2,3) - &
                DBLE(ngz+1) / DBLE(ngz) * dirc(3,3)

      x_0ang = x_0ang / bohr
      y_0ang = y_0ang / bohr
      z_0ang = z_0ang / bohr

      DO i = 1, 3
        dirl(1,i) = dirc(1,i) / ngx / bohr
        dirl(2,i) = dirc(2,i) / ngy / bohr
        dirl(3,i) = dirc(3,i) / ngz / bohr
      END DO

      nb = nwann

      IF ( nb <= 9 ) THEN
        WRITE( frfft, ' ( ''WFR00'', i1, ''.gau'' ) ' ) nb
        WRITE( fifft, ' ( ''WFI00'', i1, ''.gau'' ) ' ) nb
      ELSE IF ( nb <= 99 ) THEN
        WRITE( frfft, ' ( ''WFR0'', i2, ''.gau'' ) ' ) nb
        WRITE( fifft, ' ( ''WFI0'', i2, ''.gau'' ) ' ) nb
      ELSE IF ( nb <= 999 ) THEN
        WRITE( frfft, ' ( ''WFR'', i3, ''.gau'' ) ' ) nb
        WRITE( fifft, ' ( ''WFI'', i3, ''.gau'' ) ' ) nb
      ELSE
        WRITE(*,*) ' ERROR - nb .gt. 999'
      END IF


      OPEN ( 39, file=frfft, form='formatted', status='unknown' )

      nionst = SUM( nionsp( 1:nspec ) )

      WRITE(39, '(i4,3f12.6)' ) nionst*ngt**3, x_0ang, y_0ang, z_0ang
      WRITE(39, '(i4,3f12.6)' ) ngt*ngx, ( dirl(1,i), i=1,3 )
      WRITE(39, '(i4,3f12.6)' ) ngt*ngy, ( dirl(2,i), i=1,3 )
      WRITE(39, '(i4,3f12.6)' ) ngt*ngz, ( dirl(3,i), i=1,3 )

      DO nsp = 1 , nspec
        DO ni = 1 , nionsp(nsp) * ngt**3
          WRITE(39, '(i4,4e13.5)' ) 5 + nsp, 1.d0, ( poscartbig(i,ni,nsp), i=1,3 )
        END DO
      END DO

      nmod = MOD( ngt*ngz, 6 )
      nt = ngt * ngz
      nt = nt / 6
      nlim = -ngz + (nt-1) * 6

      DO nx = -ngx, ngs*ngx-1
        DO ny = -ngy,ngs*ngy-1
          DO nz = -ngz, nlim, 6
            WRITE( 39,'(6e13.5)' ) ( REAL( cwann(nx,ny,nzz) ), nzz = nz, nz+5 )
          END DO
          IF ( nmod == 1 ) THEN
            WRITE(39,' (e13.5)' )  REAL( cwann(nx,ny,nlim+6) )
          ELSE IF ( nmod == 2 ) THEN
            WRITE(39,' (2e13.5)' ) ( REAL( cwann(nx,ny,nzz) ), nzz = nlim+6, nlim+7 )
          ELSE IF ( nmod == 3 ) THEN
            WRITE(39,' (3e13.5)' ) ( REAL( cwann(nx,ny,nzz) ), nzz = nlim+6, nlim+8 )
          ELSE IF ( nmod == 4 ) THEN
            WRITE(39,' (4e13.5)' ) ( REAL( cwann(nx,ny,nzz) ), nzz = nlim+6, nlim+9 )
          ELSE IF ( nmod == 5 ) THEN
            WRITE(39,' (5e13.5)' ) ( REAL( cwann(nx,ny,nzz) ), nzz = nlim+6, nlim+10 )
          END IF
        END DO
      END DO

      CLOSE(39)

      STOP '*** THE END *** (plot.f90)'
      END
