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
      COMPLEX(dbl) :: catmp
      COMPLEX(dbl) :: cmod
      COMPLEX(dbl) :: citpi

      REAL(dbl), ALLOCATABLE :: vkpt(:,:)         ! vkpt(3,nkpts)

      REAL(dbl) :: scalf, tmaxx, tmax, ratmax, ratio
      REAL(dbl) :: dirc( 3, 3 ), recc( 3, 3 ), dirl( 3, 3 )
      REAL(dbl) :: pos( 3 )
      REAL(dbl) :: gxx, gyy, gzz, testcubic
      REAL(dbl) :: x_0ang, y_0ang, z_0ang 
      REAL(dbl), ALLOCATABLE :: poscarwin( :, :, : )
      REAL(dbl) :: posion( 3, natx, npsx )
      CHARACTER( LEN=2 ) :: nameat( npsx )
      INTEGER :: indat( npsx )
      INTEGER :: natwin( npsx )

      CHARACTER( LEN=11 ) :: frfft
      CHARACTER( LEN=11 ) :: fifft

      INTEGER :: dimwann, mxddim
      INTEGER :: nrxl, nryl, nrzl
      INTEGER :: nrxh, nryh, nrzh
      INTEGER :: nrxd, nryd, nrzd
      INTEGER :: nnrx, nnry, nnrz
      LOGICAL :: okp( 3 )
      REAL(dbl) :: aa

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
        READ(21) nionsp(nsp), nameat(nsp)
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

      OPEN( 29, FILE='unitary.dat', STATUS='OLD', FORM='UNFORMATTED' )

      ALLOCATE( cu( nbands, nbands, nkpts ) )
      READ(29) ( ( ( cu(j,i,n), j=1,nbands ), i=1,nbands ), n=1,nkpts )

      CLOSE(29)

! ... Initialize the data used for the fast fourier transforms

      READ(5,*) nwann, nrxl, nrxh, nryl, nryh, nrzl, nrzh
      PRINT *, 'plotting WF n. ', nwann
      PRINT *, 'plot grid      ', nrxl, nrxh, nryl, nryh, nrzl, nrzh
      PRINT *, 'total grid     ', 1, ngx, 1, ngy, 1, ngz

      nrxd = ( nrxh - nrxl + 1 )
      nryd = ( nryh - nryl + 1 ) 
      nrzd = ( nrzh - nrzl + 1 )

      nnrx = ABS( nrxl / ngx ) + 2
      nnry = ABS( nryl / ngy ) + 2
      nnrz = ABS( nrzl / ngz ) + 2

      IF( nrxd < 1 ) THEN
        WRITE( *, * ) ' nrxl, nrxh = ', nrxl, nrxh
        CALL errore( ' plot ', ' wrong nrxl and nrxh ', 1 )
      END IF

      IF( nryd < 1 ) THEN
        WRITE( *, * ) ' nryl, nryh = ', nryl, nryh
        CALL errore( ' plot ', ' wrong nryl and nryh ', 1 )
      END IF

      IF( nrzd < 1 ) THEN
        WRITE( *, * ) ' nrzl, nrzh = ', nrzl, nrzh
        CALL errore( ' plot ', ' wrong nrzl and nrzh ', 1 )
      END IF
      

      ALLOCATE ( cwann( nrxl:nrxh, nryl:nryh, nrzl:nrzh ) ) 
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
 
          CALL cfft3d( cptwr, ngx, ngy, ngz, ngx, ngy, ngz, -1 )

          DO m = 1, mplwv
            cptwr(m) = conjg(cptwr(m)) * ngx * ngy * ngz
          END DO

          DO nzz = nrzl, nrzh
            nz = MOD( nzz + nnrz * ngz , ngz ) + 1
            DO nyy = nryl, nryh
              ny = MOD( nyy + nnry * ngy , ngy ) + 1
              DO nxx = nrxl, nrxh
                nx = MOD( nxx + nnrx * ngx , ngx ) + 1

                npoint = nx + (ny-1) * ngx + (nz-1) * ngy * ngx

                scalf = vkpt(1,nkp) * DBLE(nxx) / DBLE(ngx) + &
                        vkpt(2,nkp) * DBLE(nyy) / DBLE(ngy) + &
                        vkpt(3,nkp) * DBLE(nzz) / DBLE(ngz)

                catmp = EXP( citpi * scalf ) * cptwr( npoint ) * cu( nb, nwann, nkp )
 
! ...           Here it increments the wannier funcion in nx,ny,nz with the current nb,nkp bloch orbitals 

                cwann( nxx, nyy, nzz ) = cwann( nxx, nyy, nzz ) + catmp
              END DO
            END DO
          END DO
         
        END DO  ! nband
      END DO    ! nkp

! ... Fix the global phase by setting the wannier to be real at the point where it has max modulus

      tmaxx = 0.0d0
      cmod = ( 1.0d0, 0.0d0 )

      DO nzz = nrzl, nrzh
        DO nyy = nryl, nryh
          DO nxx= nrxl, nrxh
            cwann( nxx, nyy, nzz ) = cwann( nxx, nyy, nzz ) / DBLE(nkpts)
            tmax = cwann( nxx, nyy, nzz ) * CONJG( cwann( nxx, nyy, nzz ) )
            IF ( tmax > tmaxx ) THEN
              tmaxx = tmax
              cmod = cwann( nxx, nyy, nzz )
            END IF
          END DO
        END DO
      END DO

      cmod = cmod / SQRT( REAL( cmod )**2 + AIMAG ( cmod )**2 )
      DO nzz = nrzl, nrzh
        DO nyy = nryl, nryh
          DO nxx = nrxl, nryh
            cwann(nxx,nyy,nzz) = cwann(nxx,nyy,nzz) / cmod
          END DO
        END DO
      END DO

! ... We prepare a graphic output for gopenmol or for dan, depending on the geometry of the cell; 
!     if it is orthorombic, gopenmol is used (this includes the cubic case); otherwise, dan is used.

! ... This is to create the WF...gau output, to be read by gOpenMol (coordinates + isosurfaces)

      natwin = 0
      DO nsp = 1, nspec
        DO ni = 1, nionsp(nsp)
          DO nx = -2, 2
            DO ny = -2, 2
              DO nz = -2, 2
                pos(1) = ( posion(1,ni,nsp) + DBLE(nx) ) * ngx
                pos(2) = ( posion(2,ni,nsp) + DBLE(ny) ) * ngy
                pos(3) = ( posion(3,ni,nsp) + DBLE(nz) ) * ngz
                okp(1) = ( pos(1) >= (nrxl - 1) ) .AND. ( pos(1) < nrxh )
                okp(2) = ( pos(2) >= (nryl - 1) ) .AND. ( pos(2) < nryh ) 
                okp(3) = ( pos(3) >= (nrzl - 1) ) .AND. ( pos(3) < nrzh ) 
                IF( okp(1) .AND. okp(2) .AND. okp(3) ) THEN
                  natwin( nsp ) = natwin( nsp ) + 1
                END IF
              END DO
            END DO
          END DO
        END DO
      END DO

      ALLOCATE( poscarwin( 3, MAXVAL( natwin ), nspec ) )

      natwin = 0
      DO nsp = 1, nspec
        DO ni = 1, nionsp(nsp)
          DO nx = -2, 2
            DO ny = -2, 2
              DO nz = -2, 2
                pos(1) = ( posion(1,ni,nsp) + DBLE(nx) ) * ngx
                pos(2) = ( posion(2,ni,nsp) + DBLE(ny) ) * ngy
                pos(3) = ( posion(3,ni,nsp) + DBLE(nz) ) * ngz
                okp(1) = ( pos(1) >= (nrxl - 1) ) .AND. ( pos(1) < nrxh )
                okp(2) = ( pos(2) >= (nryl - 1) ) .AND. ( pos(2) < nryh ) 
                okp(3) = ( pos(3) >= (nrzl - 1) ) .AND. ( pos(3) < nrzh ) 
                IF( okp(1) .AND. okp(2) .AND. okp(3) ) THEN
                  natwin( nsp ) = natwin( nsp ) + 1
                  DO m = 1, 3
                    poscarwin( m, natwin( nsp ), nsp ) = &
                      ( pos(1) * dirc(1,m) / ngx + pos(2) * dirc(2,m) / ngy + pos(3) * dirc(3,m) / ngz ) / bohr
                  END DO
                END IF
              END DO
            END DO
          END DO
        END DO
      END DO

      DO i = 1, 3
        dirl(1,i) = dirc(1,i) / ngx / bohr
        dirl(2,i) = dirc(2,i) / ngy / bohr
        dirl(3,i) = dirc(3,i) / ngz / bohr
      END DO

      aa = 0.0
      x_0ang = ( nrxl - aa ) * dirl( 1, 1 ) + ( nryl - aa ) * dirl( 2, 1 ) + ( nrzl - aa ) * dirl( 3, 1 )
      y_0ang = ( nrxl - aa ) * dirl( 1, 2 ) + ( nryl - aa ) * dirl( 2, 2 ) + ( nrzl - aa ) * dirl( 3, 2 )
      z_0ang = ( nrxl - aa ) * dirl( 1, 3 ) + ( nryl - aa ) * dirl( 2, 3 ) + ( nrzl - aa ) * dirl( 3, 3 )

      IF ( nwann <= 9 ) THEN
        WRITE( frfft, ' ( ''WFR00'', i1, ''.gau'' ) ' ) nwann
        WRITE( fifft, ' ( ''WFI00'', i1, ''.gau'' ) ' ) nwann
      ELSE IF ( nwann <= 99 ) THEN
        WRITE( frfft, ' ( ''WFR0'', i2, ''.gau'' ) ' ) nwann
        WRITE( fifft, ' ( ''WFI0'', i2, ''.gau'' ) ' ) nwann
      ELSE IF ( nwann <= 999 ) THEN
        WRITE( frfft, ' ( ''WFR'', i3, ''.gau'' ) ' ) nwann
        WRITE( fifft, ' ( ''WFI'', i3, ''.gau'' ) ' ) nwann
      ELSE
        WRITE(*,*) ' ERROR - nwann .gt. 999'
      END IF


      OPEN ( 39, file=frfft, form='formatted', status='unknown' )

      nionst = SUM( nionsp( 1:nspec ) )

      WRITE(39, '(i4,3f12.6)' ) SUM( natwin ), x_0ang, y_0ang, z_0ang
      WRITE(39, '(i4,3f12.6)' ) (nrxh-nrxl+1), ( dirl(1,i), i=1,3 )
      WRITE(39, '(i4,3f12.6)' ) (nryh-nryl+1), ( dirl(2,i), i=1,3 )
      WRITE(39, '(i4,3f12.6)' ) (nrzh-nrzl+1), ( dirl(3,i), i=1,3 )

      CALL convert_label( nameat, indat, nspec )
      DO nsp = 1 , nspec
        DO ni = 1 , natwin( nsp )
          WRITE(39, '(i4,4e13.5)' ) indat( nsp ), 1.d0, ( poscarwin( i, ni, nsp ), i=1,3 )
        END DO
      END DO

      nmod = MOD( ( nrzh - nrzl + 1 ), 6 )
      nt   = ( nrzh - nrzl + 1 ) / 6
      nlim = nrzl + ( nt * 6 ) - 1

      DO nx = nrxl, nrxh
        DO ny = nryl, nryh
          DO nz = nrzl, nlim , 6
            WRITE( 39, fmt = '(6e13.5)' ) ( REAL( cwann( nx, ny, nzz ) ), nzz = nz, nz + 5 )
          END DO
          WRITE( 39, fmt = '(5e13.5)' ) ( REAL( cwann( nx, ny, nzz) ), nzz = nlim + 1, nlim + nmod )
        END DO
      END DO

      CLOSE(39)

      DEALLOCATE( cwann )
      DEALLOCATE( poscarwin )

      call gcube2plt( nwann )

      STOP '*** THE END *** (plot.f90)'
      END


      SUBROUTINE convert_label( nameat, indat, ntyp )
        USE parameters, ONLY: npsx, natx
        CHARACTER( LEN=2 ) :: nameat( npsx )
        INTEGER :: indat( npsx )
        INTEGER :: ntyp

        DO is = 1, ntyp
          SELECT CASE ( TRIM( nameat( is ) ) )
            CASE ( 'H' )
              indat( is ) = 1
            CASE ( 'He' )
              indat( is ) = 2
            CASE ( 'Li' )
              indat( is ) = 3
            CASE DEFAULT
              indat( is ) = 6
          END SELECT
        END DO

!"H",                                                                                "He",
!"Li","Be",                                                  "B","C","N","O","F","Ne",
!"Na","Mg",                                                  "Al","Si","P","S","Cl","Ar",
!"K","Ca","Sc","Ti","V","Cr","Mn","Fe","Co","Ni","Cu","Zn","Ga","Ge","As","Se","Br","Kr",
!"Rb","Sr","Y","Zr","Nb","Mo","Tc","Ru","Rh","Pd","Ag","Cd","In","St","Sb","Te","I","Xe",
!"Cs","Ba","La","Hf","Ta","W","Re","Os","Ir","Pt","Au","Hg","Tl","Pb","Bi","Po","At","Rn",
!"Fr","Ra","Ac",
!               "Ce","Pr","Nd","Pm","Sm","Eu","Gd","Tb","Dy","Ho","Er","Tm","Yb","Lu",
!               "Th","Pa","U","Np","Pu","Am","Cm","Bk","Cf","Es","Fm","Md","No","Lr"


        RETURN
      END SUBROUTINE
