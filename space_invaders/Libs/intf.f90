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
       SUBROUTINE intf( bvec, emax, nk, s, dimwann, nshells, nwhich, nkpts, mxddim, &
         ndwinx, mxdbnd, nr1, nr2, nr3, ngm, igv, ngwk, dimwin, evecr, eveci, amp,  &
         vkpt, igsort )
!=----------------------------------------------------------------------------------=

       USE kinds
       USE constants, ONLY: pi, twopi => tpi, ryd => ry, har => au, bohr => bohr_radius_angs
       ! USE constants, ONLY: pi, twopi => tpi, ryd => ry, har => au
       USE timing_module, ONLY : timing, timing_deallocate, timing_overview
       USE io_global, ONLY : stdout
       USE startup_module, ONLY : startup
       USE version_module, ONLY : version_number

       IMPLICIT NONE

       REAL(dbl) :: bvec(3,3) 
       REAL(dbl) :: emax
       INTEGER :: nk(3)
       REAL(dbl) :: s(3)
       INTEGER :: dimwann
       INTEGER :: nshells
       INTEGER :: nwhich( nshells )
       INTEGER :: nkpts, mxddim, mxdbnd, ndwinx
       INTEGER :: nr1, nr2, nr3, ngm
       INTEGER :: igv( 3, ngm )
       INTEGER :: ngwk( nkpts ) 
       REAL(dbl) :: evecr( mxddim, ndwinx, nkpts )
       REAL(dbl) :: eveci( mxddim, ndwinx, nkpts )
       INTEGER   :: igsort( mxddim, nkpts )
       INTEGER :: dimwin( nkpts )
       COMPLEX(dbl) :: amp( mxdbnd, mxdbnd, nkpts )
       REAL(dbl) :: vkpt( 3, nkpts )


       REAL(dbl) :: enmax
       INTEGER :: lpctx(nr1), lpcty(nr2), lpctz(nr3)
 
       REAL(dbl) :: recc(3,3)

       REAL(dbl), ALLOCATABLE :: dnlg(:,:,:) 
       REAL(dbl), ALLOCATABLE :: dnlkg(:,:,:) 
       REAL(dbl), ALLOCATABLE :: datake(:,:,:) 
       INTEGER, ALLOCATABLE :: nindpw(:,:)  
       INTEGER, ALLOCATABLE :: nplwkp(:)   
       INTEGER, ALLOCATABLE :: ninvpw(:,:)
       COMPLEX(dbl), ALLOCATABLE :: cptwfp(:,:)
       COMPLEX(dbl), ALLOCATABLE :: lvec(:,:)

       INTEGER :: nx, ny, nz
       INTEGER :: m, nkp, np, npoint, iib, igk
       INTEGER :: ierr
       INTEGER :: i1, i2, i3, i, j, l
       INTEGER :: nnr
       INTEGER :: iprint = 1

      !REAL(dbl) :: bohr
      !PARAMETER ( bohr = 0.52917715d0 )

!
! ...  End declarations and dimensions
!
!=----------------------------------------------------------------------------=!

!
! ...  Convert emax to eV

       enmax = emax * har

! ...  Convert to angstrom

       recc = TRANSPOSE( bvec ) / bohr

! ... Generate the array ninvpw
!     The next chunck of code is copied from wannier

! ... Initialize the loop counters lpctx,lpcty,lpctz that
!     label the number of the reciprocal lattice vectors in the x,y,z
!     directions, respectively. for the x direction the reciprocal lattice
!     vectors corresponding to the first,second,...,nr1th elements in all
!     of the reciprocal lattice arrays are 0,1,..,(nr1/2),-((nr1/2-1),..,-1
!     times the x reciprocal lattice vector recc(1,*)

      DO nx = 1 , (nr1/2)+1
        lpctx(nx)  = nx - 1
      END DO
      DO nx = (nr1/2)+2 , nr1
        lpctx(nx)  = nx - 1 - nr1
      END DO
      DO ny = 1 , (nr2/2)+1
        lpcty(ny)  = ny - 1
      END DO
      DO ny = (nr2/2)+2 , nr2
        lpcty(ny)  = ny - 1 - nr2
      END DO
      DO nz = 1 , (nr3/2)+1
        lpctz(nz)  = nz - 1
      END DO
      DO nz = (nr3/2)+2 , nr3
        lpctz(nz)  = nz - 1 - nr3
      END DO

! ... Subroutine genbtr calculate the g-vectors for each K_point
!     within the kinetic energy cutoff

      ALLOCATE( nindpw( mxddim, nkpts ), STAT=ierr )
         IF( ierr /=0 ) CALL errore(' intf ', ' allocating nindpw ', mxddim*nkpts )
      ALLOCATE( nplwkp( nkpts ), STAT=ierr )
         IF( ierr /=0 ) CALL errore(' intf ', ' allocating nplwkp ', nkpts )
      ALLOCATE( datake( 7, mxddim, nkpts ), STAT=ierr )
         IF( ierr /=0 ) CALL errore(' intf ', ' allocating datake ', 7*mxddim*nkpts )
      ALLOCATE( dnlg( mxddim, 3, nkpts ), STAT=ierr )
         IF( ierr /=0 ) CALL errore(' intf ', ' allocating dnlg ', 3*mxddim*nkpts )
      ALLOCATE( dnlkg( mxddim, 0:3, nkpts ), STAT=ierr ) 
         IF( ierr /=0 ) CALL errore(' intf ', ' allocating dnlkg ', 4*mxddim*nkpts )

      nindpw = 0

      CALL genbtr( mxddim, nr1, nr2, nr3, nkpts, enmax, nindpw, nplwkp, vkpt,  &
           lpctx, lpcty, lpctz, datake, recc, recc, iprint, dnlg, dnlkg )

! ... Check G-space dimensions 

      DO nkp = 1, nkpts
        IF( nplwkp( nkp ) > mxddim ) THEN
          WRITE(*,*) '*** INCREASE MXDDIM ***'
          WRITE(*,*) 'FOR NKP = ', nkp,', NPLWKP = ', nplwkp(nkp), 'AND MXDDIM = ', mxddim
          STOP
        END IF
      END DO

      nnr = nr1 * nr2 * nr3

      ALLOCATE( ninvpw( 0:nnr, nkpts ), STAT=ierr )
         IF( ierr /=0 ) CALL errore(' intf ', ' allocating ninvpw ', (nnr+1)*nkpts )

      DO nkp = 1, nkpts
        DO np = 0, nnr
          ninvpw( np, nkp ) = mxddim + 1
        END DO
        DO np = 1, nplwkp( nkp )
          npoint = nindpw( np, nkp )
          ninvpw( npoint, nkp ) = np
        END DO
      END DO

! ... Read the energy eigenfunctions within the energy window at each K-point, 
!     the subspace basis vectors, and convert the latter to CASTEP format and write
!     them in output file "intf.out", that will be read by wannier

      OPEN( UNIT=20, FILE='onfly.dat', FORM='UNFORMATTED')

      ALLOCATE( lvec( mxddim, mxdbnd ), STAT=ierr )
         IF( ierr /=0 ) CALL errore(' intf ', ' allocating lvec ', mxddim * mxdbnd )

      ALLOCATE( cptwfp( mxddim+1, dimwann ), STAT=ierr )
         IF( ierr /=0 ) CALL errore(' intf ', ' allocating cptwfp ', (mxddim+1)*dimwann )

      K_POINTS: DO nkp = 1, nkpts

         DO l = 1, dimwann
           DO i = 1, ngwk( nkp )
             lvec(i,l) = cmplx(0.0d0,0.0d0)
             DO j = 1, dimwin( nkp )
               lvec(i,l) = lvec(i,l) + amp(j,l,nkp) * cmplx( evecr( i, j, nkp ), eveci( i, j, nkp ) )
             END DO
           END DO
         END DO
 
! ...    Go through all the g-vectors inside the cutoff radius at the present K-Point
 
         G_VECTORS: DO j = 1, nplwkp( nkp )

           igk = igsort( j, nkp )
           IF ( igv(1,igk) >= 0 ) nx = igv(1,igk) + 1
           IF ( igv(1,igk) <  0 ) nx = igv(1,igk) + 1 + nr1
           IF ( igv(2,igk) >= 0 ) ny = igv(2,igk) + 1
           IF ( igv(2,igk) <  0 ) ny = igv(2,igk) + 1 + nr2
           IF ( igv(3,igk) >= 0 ) nz = igv(3,igk) + 1
           IF ( igv(3,igk) <  0 ) nz = igv(3,igk) + 1 + nr3

           npoint = nx + (ny-1)*nr1 + (nz-1)*nr1*nr2

           BANDS: DO I = 1, dimwann                     
 
             ! ...      Conjg is due to the opposite bloch convention in CASTEP 
 
             cptwfp( ninvpw( npoint, nkp ), i ) = conjg( lvec(j,i) )

             IF ( ninvpw( npoint, nkp ) > nplwkp( nkp ) .OR. ninvpw(npoint,nkp) <= 0 ) THEN
               WRITE(*,*) ninvpw(npoint,nkp)
               WRITE(*,*) 'NINVPW OUT OF BOUNDS'
               STOP
             END IF          

           END DO BANDS

         END DO G_VECTORS

         WRITE(20) SIZE( cptwfp, 1), SIZE( cptwfp, 2), nkpts, nplwkp( nkp ), dimwann
         WRITE(20) ( ( cptwfp( np, iib ), np=1, nplwkp(nkp) ), iib=1, dimwann )  

       END DO K_POINTS

       CLOSE(20)

       DEALLOCATE( cptwfp, STAT=ierr )
          IF( ierr /=0 ) CALL errore(' intf ', ' deallocating cptwfp ', ABS(ierr) )
       DEALLOCATE( nindpw, STAT=ierr )
          IF( ierr /=0 ) CALL errore(' intf ', ' deallocating nindpw ', ABS(ierr) )
       DEALLOCATE( nplwkp, STAT=ierr )
          IF( ierr /=0 ) CALL errore(' intf ', ' deallocating nplwkp ', ABS(ierr) )
       DEALLOCATE( datake, STAT=ierr )
          IF( ierr /=0 ) CALL errore(' intf ', ' deallocating datake ', ABS(ierr) )
       DEALLOCATE( dnlg, STAT=ierr )
          IF( ierr /=0 ) CALL errore(' intf ', ' deallocating dnlg ', ABS(ierr) )
       DEALLOCATE( dnlkg, STAT=ierr ) 
          IF( ierr /=0 ) CALL errore(' intf ', ' deallocating dnlkg ', ABS(ierr) )
       DEALLOCATE( ninvpw, STAT=ierr )
          IF( ierr /=0 ) CALL errore(' intf ', ' deallocating ninvpw ', ABS(ierr) )
       DEALLOCATE( lvec, STAT=ierr )
          IF( ierr /=0 ) CALL errore(' intf ', ' deallocating lvec ', ABS(ierr) )

       RETURN
       END SUBROUTINE

