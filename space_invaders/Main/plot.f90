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
      PROGRAM plot
!=----------------------------------------------------------------------------------=

      USE kinds
      USE constants, ONLY: pi, twopi => tpi, bohr => bohr_radius_angs
      USE parameters, ONLY: npsx, natx
      USE fft_scalar, ONLY: cfft3d
      USE timing_module, ONLY : timing, timing_deallocate, timing_overview
      USE io_global, ONLY : stdout
      USE startup_module, ONLY : startup
      USE version_module, ONLY : version_number

      IMPLICIT NONE

      INTEGER :: ngt, ngs
      PARAMETER ( ngt = 2, ngs = ngt-1)

      INTEGER :: ngx, ngy, ngz
      INTEGER :: nionst, nions, nspec, nkpts
      INTEGER :: mplwv
      INTEGER :: npoint
      INTEGER :: nwann, nzz, nyy, nxx
      INTEGER :: nx, ny, nz
      INTEGER :: nkp, nb, m, nsp
      INTEGER :: n, j, ni
      INTEGER :: i, nmod, nt, nlim
      INTEGER, ALLOCATABLE :: nplwkp(:)           ! nplwkp(nkpts)
      INTEGER, ALLOCATABLE :: nindpw(:,:)         ! nindpw(mxddim,nkpts)
      INTEGER :: nionsp(npsx)           ! nionsp(nspec)

      COMPLEX(dbl), ALLOCATABLE :: cptwfp(:,:,:)  ! cptwfp(mxddim+1,dimwann,nkpts)
      COMPLEX(dbl), ALLOCATABLE :: cwann(:,:,:)   ! cwann(-ngx:ngs*ngx-1,-ngy:ngs*ngy-1,-ngz:ngs*ngz-1)
      COMPLEX(dbl), ALLOCATABLE :: cptwr(:)       ! cptwr(mplwv)
      COMPLEX(dbl), ALLOCATABLE :: cu(:,:,:)      ! cu(dimwann,dimwann,nkpts)
      COMPLEX(dbl) :: catmp
      COMPLEX(dbl) :: cmod
      COMPLEX(dbl) :: citpi

      REAL(dbl), ALLOCATABLE :: vkpt(:,:)         ! vkpt(3,nkpts)

      REAL(dbl) :: scalf, tmaxx, tmax
      REAL(dbl) :: dirc( 3, 3 ), recc( 3, 3 ), dirl( 3, 3 )
      REAL(dbl) :: pos( 3 )
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
      REAL(dbl) :: off
      INTEGER :: ierr

      PARAMETER ( citpi = ( 0.0d0, twopi) )

! ... End declarations and dimensions

!
! ...  Startup
!
       CALL startup(version_number,MAIN_NAME='plot')

!
! ...  Reading from file
!
      OPEN ( 21, FILE = 'landing.dat', STATUS='old', FORM='UNFORMATTED' )

      READ (21) nkpts, ngx, ngy, ngz, mplwv, dimwann, mxddim, nspec
      READ (21) recc(1,1), recc(2,1), recc(3,1)
      READ (21) recc(1,2), recc(2,2), recc(3,2)
      READ (21) recc(1,3), recc(2,3), recc(3,3)
      READ (21) dirc(1,1), dirc(2,1), dirc(3,1)
      READ (21) dirc(1,2), dirc(2,2), dirc(3,2)
      READ (21) dirc(1,3), dirc(2,3), dirc(3,3)


      ALLOCATE( vkpt( 3, nkpts ), STAT=ierr )
         IF( ierr /=0 ) CALL errore(' plot ', ' allocating vkpt ', 3*nkpts )

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

      ALLOCATE( nplwkp( nkpts ), STAT=ierr )
         IF( ierr /=0 ) CALL errore(' plot ', ' allocating nplwkp ', nkpts )
      DO nkp = 1, nkpts
        READ(21) nplwkp(nkp)
      END DO

      ALLOCATE( nindpw( MAX(mxddim,mplwv),nkpts), STAT=ierr )
         IF( ierr /=0 ) CALL errore(' plot ', ' allocating nindpw ', MAX(mxddim,mplwv)*nkpts )
      DO nkp = 1, nkpts
        DO n = 1, mplwv
          READ(21) nindpw(n,nkp)
        END DO
      END DO

      ALLOCATE( cptwfp( mxddim + 1, dimwann, nkpts ), STAT=ierr )
         IF( ierr /=0 ) &
         CALL errore(' plot ', ' allocating cptwfp ', (mxddim+1)*dimwann*nkpts )
      DO nkp = 1, nkpts
        DO nb = 1, dimwann
          DO m = 1, nplwkp(nkp)
            READ(21) cptwfp(m,nb,nkp)
          END DO
        END DO
      END DO

      CLOSE(21)

      OPEN( 29, FILE='unitary.dat', STATUS='OLD', FORM='UNFORMATTED' )

      ALLOCATE( cu( dimwann, dimwann, nkpts ), STAT=ierr )
         IF( ierr /=0 ) CALL errore(' plot ', ' allocating cu ', dimwann**2 *nkpts )
      READ(29) ( ( ( cu(j,i,n), j=1,dimwann ), i=1,dimwann ), n=1,nkpts )

      CLOSE(29)

! ... Initialize the data used for the fast fourier transforms

      READ(5,*) nwann, nrxl, nrxh, nryl, nryh, nrzl, nrzh
      WRITE(stdout,*) 'plotting WF n. ', nwann

      nrxd = ( nrxh - nrxl + 1 )
      nryd = ( nryh - nryl + 1 ) 
      nrzd = ( nrzh - nrzl + 1 )

      nnrx = ABS( nrxl / ngx ) + 2
      nnry = ABS( nryl / ngy ) + 2
      nnrz = ABS( nrzl / ngz ) + 2

      IF( nrxd < 1 ) THEN
        WRITE( stdout, * ) ' nrxl, nrxh = ', nrxl, nrxh
        CALL errore( ' plot ', ' wrong nrxl and nrxh ', 1 )
      END IF

      IF( nryd < 1 ) THEN
        WRITE( stdout, * ) ' nryl, nryh = ', nryl, nryh
        CALL errore( ' plot ', ' wrong nryl and nryh ', 1 )
      END IF

      IF( nrzd < 1 ) THEN
        WRITE( stdout, * ) ' nrzl, nrzh = ', nrzl, nrzh
        CALL errore( ' plot ', ' wrong nrzl and nrzh ', 1 )
      END IF
      

      ALLOCATE ( cwann( nrxl:nrxh, nryl:nryh, nrzl:nrzh ), STAT=ierr ) 
         IF( ierr /=0 ) CALL errore(' plot ', ' allocating cwann ', &
                        (nrxh-nrxl+1)*(nryh-nryl+1)*(nrzh-nrzl+1)    )
      cwann = ( 0.0d0, 0.0d0 )
!
      ALLOCATE( cptwr(mplwv), STAT=ierr )
         IF( ierr /=0 ) CALL errore(' plot ', ' allocating cptwr ', mplwv )
     

      DO nkp = 1, nkpts
        DO nb = 1, dimwann

          DO m = 1, mplwv
            cptwr(m) = ( 0.0d0, 0.0d0 )
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

      ALLOCATE( poscarwin( 3, MAXVAL( natwin ), nspec ), STAT=ierr )
         IF( ierr /=0 ) CALL errore(' plot ', ' allocating poscarwin ', &
                        3 * MAXVAL( natwin ) * nspec ) 

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

!     Offset for position and WF's allignment
      off = 0.0
      x_0ang = ( nrxl - off ) * dirl( 1, 1 ) + ( nryl - off ) * dirl( 2, 1 ) + ( nrzl - off ) * dirl( 3, 1 )
      y_0ang = ( nrxl - off ) * dirl( 1, 2 ) + ( nryl - off ) * dirl( 2, 2 ) + ( nrzl - off ) * dirl( 3, 2 )
      z_0ang = ( nrxl - off ) * dirl( 1, 3 ) + ( nryl - off ) * dirl( 2, 3 ) + ( nrzl - off ) * dirl( 3, 3 )

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
        WRITE(stdout,*) ' ERROR - nwann .gt. 999'
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

      DEALLOCATE( vkpt, STAT=ierr )
         IF( ierr /=0 ) CALL errore(' plot ', ' deallocating vkpt ', ABS(ierr) )
      DEALLOCATE( nplwkp, STAT=ierr )
         IF( ierr /=0 ) CALL errore(' plot ', ' deallocating nplwkp ', ABS(ierr) )
      DEALLOCATE( nindpw, STAT=ierr )
         IF( ierr /=0 ) CALL errore(' plot ', ' deallocating nindpw ', ABS(ierr) )
      DEALLOCATE( cptwfp, STAT=ierr )
         IF( ierr /=0 ) CALL errore(' plot ', ' deallocating cptwfp ', ABS(ierr) )
      DEALLOCATE( cu, STAT=ierr )
         IF( ierr /=0 ) CALL errore(' plot ', ' deallocating cu ', ABS(ierr) )
      DEALLOCATE( cptwr, STAT=ierr )
         IF( ierr /=0 ) CALL errore(' plot ', ' deallocating cptwr ', ABS(ierr) )
      DEALLOCATE( cwann, STAT=ierr )
         IF( ierr /=0 ) CALL errore(' plot ', ' deallocating cwann ', ABS(ierr) )
      DEALLOCATE( poscarwin, STAT=ierr )
         IF( ierr /=0 ) CALL errore(' plot ', ' deallocating poscarwin ', ABS(ierr) )

      call gcube2plt( nwann )

      CALL timing('plot',OPR='stop')
      CALL timing('global',OPR='stop')
      CALL timing_overview(stdout,MAIN_NAME='plot')
      CALL timing_deallocate()

      STOP '*** THE END *** (plot.x)'
      END


      SUBROUTINE convert_label( nameat, indat, ntyp )
        USE parameters, ONLY: npsx, natx
        CHARACTER( LEN=2 ) :: nameat( npsx )
        INTEGER :: indat( npsx )
        INTEGER :: ntyp

!       The case loop is implemented only for a few
!       selected chemical species, otherwise the label
!       is set equal to carbon by default
        DO is = 1, ntyp
          SELECT CASE ( TRIM( nameat( is ) ) )
            CASE ( 'H' )
              indat( is ) = 1
            CASE ( 'He' )
              indat( is ) = 2
            CASE ( 'Li' )
              indat( is ) = 3
            CASE ( 'Be' )
              indat( is ) = 4
            CASE ( 'B' )
              indat( is ) = 5
            CASE ( 'C' )
              indat( is ) = 6
            CASE ( 'N' )
              indat( is ) = 7
            CASE ( 'O' )
              indat( is ) = 8
            CASE ( 'F' )
              indat( is ) = 9
            CASE ( 'Ne' )
              indat( is ) = 10
            CASE ( 'Na' )
              indat( is ) = 11
            CASE ( 'Mg' )
              indat( is ) = 12
            CASE ( 'Al' )
              indat( is ) = 13
            CASE ( 'Si' )
              indat( is ) = 14
            CASE ( 'P' )
              indat( is ) = 15
            CASE ( 'S' )
              indat( is ) = 16
            CASE ( 'Cl' )
              indat( is ) = 17
            CASE ( 'Ar' )
              indat( is ) = 18
            CASE ( 'K' )
              indat( is ) = 19
            CASE ( 'Ca' )
              indat( is ) = 20
!           ......
            CASE ( 'Mn' )
              indat( is ) = 25
            CASE ( 'Fe' )
              indat( is ) = 26
            CASE ( 'Co' )
              indat( is ) = 27
            CASE ( 'Ni' )
              indat( is ) = 28
            CASE ( 'Cu' )
              indat( is ) = 29
            CASE ( 'Zn' )
              indat( is ) = 30
            CASE ( 'Ga' )
              indat( is ) = 31
!           ......
            CASE ( 'As' )
              indat( is ) = 33
            CASE ( 'Sr' )
              indat( is ) = 38
            CASE ( 'Ag' )
              indat( is ) = 47
            CASE ( 'In' )
              indat( is ) = 49
            CASE ( 'Cs' )
              indat( is ) = 55
            CASE ( 'Pt' )
              indat( is ) = 78
            CASE ( 'Au' )
              indat( is ) = 79
!           ......
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
