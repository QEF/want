       SUBROUTINE overlap( kgv, vkpt, avec, evecr, eveci, isort, mtxd, &
                           dimwin, nntot, nnlist, nncell, cm, enmax, &
                           mxdgve, mxddim, mxdnrk, mxdnn, mxdbnd, &
                           ngx, ngy, ngz, nrplwv, nkpts )
 
!.....................................................................
! april 2002: rewritten for improved performance based on the original
! version by Ivo Souza, using the wannier.f convention (MBN)
!
! input:
!
! nkpts             # of k-points where the overlap matrix is being calculated
! kgv(i,n)          i-th component (reciprocal lattice coordinates) of
!                   the n-th g-vector ordered by stars of increasing length
! vkpt(i,nkp)       special k-points in reciprocal lattice coordinates
! evecr(i,j,nkp)    real part of the plane wave component of the g-vector
!                   kgv(1:3,isort(i,nkp)) for the j-th energy eigenvector 
!                   inside the energy window at the nkp-th k-point 
! eveci(i,j,nkp)    imaginary part of the plane wave component
! isort(i,nkp)      g-vector associated with row/column i of hamiltonian at
!                   the nkp-th k-point is kgv(1:3,isort(i,nkp))
! mtxd(nkp)         dimension of the hamiltonian at the nkp-th k-point
! nntot(nkp)        total number of b-vectors in all shells around the
!                   nkp-th k-point
! nnlist(nkp,nnx)   vkpt(1:3,nnlist(nkp,nnx)) is the nnx-th neighboring
!                   k-point of the nkp-th k-point vkpt(1:3,nkp) (or its
!                   periodic image in the "home brillouin zone")
! nncell(i,nkp,nnx) the nnx-th nearest neighbor of the nkp-th k-point
!                   is located in the reciprocal space primitive cell 
!                   labelled by the three integers nncell(1:3,nkp,nnx)
! dimwin(nkp)       # of bands that fall in the " sharp" energy window
! mxddim            array dimension of hamiltonian rows
! mxdbnd            array dimension for bands
! mxdnrk            array dimension for k-points
! mxdnn             maximum possible number of nearest-neighbor k-points (12); 
!                   turns out to equal the maximum possible number of b-vectors
!                   that may be needed in the finite-difference formulas
!                   for the k-derivatives
! output:
!
! cm(n,m,nnx,nkp)   overlap matrix <u_nk|u_{m,k+b}> where vkpt(1:3,kpt) is k 
!                   and vkpt(1:3,nnlist(kpt,nnx) is k+b (or its periodic image
!                   in the "home brillouin zone")
!
!.....................................................................

 
      IMPLICIT NONE

      INTEGER :: mxdgve, mxddim, mxdnrk, mxdnn, mxdbnd 
      INTEGER :: ngx, ngy, ngz, nrplwv, nkpts
      INTEGER :: kgv(3,mxdgve), isort(mxddim,mxdnrk)
      INTEGER :: mtxd(mxdnrk)
      INTEGER :: nnlist(mxdnrk,mxdnn)
      INTEGER :: nntot(mxdnrk)
      INTEGER :: nncell(3,mxdnrk,mxdnn)
      REAL*8 :: evecr(mxddim,mxdbnd,mxdnrk)
      REAL*8 :: eveci(mxddim,mxdbnd,mxdnrk)
 
      COMPLEX*16 :: cptwfp(nrplwv+1,mxdbnd,nkpts)
      COMPLEX*16 :: cm(mxdbnd,mxdbnd,mxdnn,mxdnrk)
      REAL*8 :: dnlg(nrplwv,3,nkpts)
      REAL*8 :: dnlkg(nrplwv,0:3,nkpts)
      REAL*8 :: datake(7,nrplwv,nkpts)
      REAL*8 :: dirc(3,3),recc(3,3)
      REAL*8 :: avec(3,3)
      REAL*8 :: diri(3,3),reci(3,3)

      INTEGER :: nplwv,mplwv

      COMPLEX*16 :: czero
      PARAMETER( czero = ( 0.0d0, 0.0d0 ) )

      INTEGER :: nnx, ndnn, nnsh
      INTEGER :: l, m, n, i, j ,nx, ny, nz
      INTEGER :: nkp, np, npoint
      INTEGER :: nkb
      INTEGER :: nkp2, npoint2, nn, iprint, nb
      INTEGER, ALLOCATABLE :: nx2(:), ny2(:), nz2(:)

      INTEGER :: ninvpw(0:(ngx*ngy*ngz),mxdnrk)
      INTEGER :: nindpw(nrplwv,nkpts)
      INTEGER :: lpctx(ngx), lpcty(ngy), lpctz(ngz)
      INTEGER :: nplwkp(mxdnrk)

      REAL*8 :: vkpt(3,mxdnrk)
      INTEGER :: dimwin(mxdnrk)

      REAL*8 :: volc, voli, enmax
      REAL*8 :: bohr, har, ryd
      PARAMETER ( ryd  = 13.605826d0 )
      PARAMETER ( har  = 2.d0 * ryd )
      PARAMETER ( bohr = 0.52917715d0 )

! ... END declarations

      WRITE(*,*) 
      WRITE(*,*) ' Starting Overlap '


      nplwv = ngx * ngy * ngz
      mplwv = ngx * ngy * (ngz+1)

      enmax = enmax * har


! ... do the dot product as in wannier and define indeces for G vectors

      DO i = 1, 3
        DO j = 1, 3
          dirc(i,j) = avec(i,j) * bohr
          diri(i,j) = avec(i,j) * bohr
        END DO
      END DO
      CALL bastr( dirc, recc, volc )
      CALL bastr( diri, reci, voli )

! ... Generate the array ninvpw (taken from wannier)
 
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

! ... Subroutine gensp performs a number of tasks. 
!     the indexing system for padding the spheres of plane waves at 
!     each k point into the box used for the fast fourier transforms 
!     is also computed as are the kinetic energies of the plane wave 
!     basis states at each k point

      DO nkp = 1, nkpts
       Do np = 1, nrplwv
        nindpw(np,nkp) = 0
       END DO
      END DO

      IPRINT=1

!     WRITE(*,*) ' DEBUG intf RECC ', recc
!     WRITE(*,*) ' DEBUG intf RECI ', reci
!     WRITE(*,*) ' DEBUG intf VKPT ', vkpt
!     WRITE(*,*) ' DEBUG intf LPCTX ', lpctx
!     WRITE(*,*) ' DEBUG intf LPCTY ', lpcty
!     WRITE(*,*) ' DEBUG intf LPCTZ ', lpctz
!     WRITE(*,*) ' DEBUG intf ENMAX ', enmax
!     WRITE(*,*) ' DEBUG intf NRPLWV ', nrplwv

      CALL genbtr( nrplwv, ngx, ngy, ngz, nkpts, enmax, nindpw, nplwkp, vkpt, &
           lpctx, lpcty, lpctz, datake, recc, reci, iprint, dnlg, dnlkg )

      DO nkp = 1, nkpts
        IF ( nplwkp(nkp) > mxddim ) THEN
          WRITE(*,*) 'For nkp = ', nkp, ', nplwkp = ', nplwkp(nkp), 'and mxddim = ', mxddim, '. Increase mxddim'
          STOP
        END IF
      END DO

      DO nkp = 1, nkpts
        DO np = 0, nplwv
          ninvpw(np,nkp) = nrplwv + 1
        END DO
        DO np = 1, nplwkp(nkp)
          npoint = nindpw(np,nkp)
          ninvpw(npoint,nkp) = np
        END DO
      END DO

! ... Transform wave-functions in CASTEP format

      DO nkp = 1, nkpts
        DO nb = 1, dimwin(nkp)

          DO m = 1, nrplwv+1
            cptwfp(m,nb,nkp) = czero
          END DO
 
! ...     Go through all the g-vectors inside the cutoff radius at the present k-point
 
          DO j = 1, mtxd(nkp)

            IF ( kgv(1,isort(j,nkp) ) >= 0 ) nx = kgv( 1,isort(j,nkp) ) + 1
            IF ( kgv(1,isort(j,nkp) ) <  0 ) nx = kgv( 1,isort(j,nkp) ) + 1 + ngx

            IF ( kgv(2,isort(j,nkp) ) >= 0 ) ny = kgv( 2,isort(j,nkp) ) + 1
            IF ( kgv(2,isort(j,nkp) ) <  0 ) ny = kgv( 2,isort(j,nkp) ) + 1 + ngy

            IF ( kgv(3,isort(j,nkp) ) >= 0 ) nz = kgv( 3,isort(j,nkp) ) + 1
            IF ( kgv(3,isort(j,nkp) ) <  0 ) nz = kgv( 3,isort(j,nkp) ) + 1 + ngz

            npoint = nx + (ny-1) * ngx + (nz-1) * ngx * ngy

! ...       Npoint is the absolute (k-independent) castep index of the gvector
!           kgv(*,isort(j,nkp)). Note that in general it is not equal to nindpw(j,nkp),
!           since both the ordering of the g-vectors at any given k-point and their
!           absolute orderinging is different in cpw and in castep. (In cpw the
!           absolute ordering is by stars of increasing length.) Think more about this,
!           to make sure all that I said is correct.
 
! ...       Bloch state in the g-space grid
 
            cptwfp(ninvpw(npoint,nkp),nb,nkp) = CONJG( CMPLX( evecr(j,nb,nkp), eveci(j,nb,nkp) ) )
 
          END DO ! g-vectors at present k (J)

        END DO   ! nb  loop 
      END DO     ! nkp loop


! ... Calculate cm(i,j,nkp,nn)=<u_i k|u_j k+dk> (keeping into account
!     that if k+dk is outside (or should be outside) the first BZ it must be
!     brought from there (or there)
!     with a exp(-iG.r) factor (given the proper convention for the sign)
!     for a standard, non-Castep convention:
!     psi_nk=psi_nk+G -> u_nk exp(ikr)=u_nk+G exp(ikr) exp (iGr) ->
!                        u_nk+G = u_nk exp(-iGr)
 
! ... Now, we are using the Castep u_nk, so the conjugation to go to the
!     standard Bloch formalism (and that was done in real space above)
!     implies that we deal with v_nk=u_nk*=sum_G c_nk,G* exp(iGr).
!     Additionally, we might have a exp (iG_0r) that has to be introduced,
!     if we need a nkp2 that lies outside the BZ. In our reciprocal space
!     products, that means that we have <v_m,k1|v_n,k2>=\sum_G1,G2 c_m,k1,G1
!     c_n,k2,G2* \int exp [i(G2-G1-G0)r], if G0 is the vector that, say,
!     brings a k2 inside the BZ just outside it, to be a neighbour of a
!     k1 that lies just inside the boundary (and the u are now in standard
!     Bloch notation). The integral gives a delta, and so we take G2s that
!     are G1s+G0, i.e. nx+nncell, etc...

      cm(:,:,:,:) = (0.d0, 0.d0)
      ALLOCATE( nx2(ngx) )
      ALLOCATE( ny2(ngy) )
      ALLOCATE( nz2(ngz) )

      DO nkp = 1, nkpts
        DO nn = 1 ,nntot(nkp)
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
              DO nx = 1, ngx
                npoint = npoint + 1
                npoint2 = nx2(nx) + ny2(ny) + nz2(nz)

                DO j = 1, dimwin(nkp2)
                  cm(1:dimwin(nkp), j, nn, nkp) = &
                      cm(1:dimwin(nkp), j, nn, nkp) + &
                      cptwfp(ninvpw(npoint,nkp), 1:dimwin(nkp), nkp) * &
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
 
      WRITE(*,*) ' Overlap, done. '
      WRITE(*,*) 

      RETURN
      END SUBROUTINE
