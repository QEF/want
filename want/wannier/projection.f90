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
       SUBROUTINE projection( avec, lamp, ca, evec, vkpt,                  & 
                  kgv, isort, npwk, dimwin, dimwann, dimfroz,              &
                  npwkx, mxdbnd, mxdnrk, npw, ngx, ngy, ngz, nkpts,      &
                  gauss_typ, rphiimx1, rphiimx2, l_wann,                   &
                  m_wann, ndir_wann, rloc, ndwinx)
!=----------------------------------------------------------------------------------=

       USE kinds
       USE constants, ONLY : ZERO, CZERO, ONE, PI, TPI, RYD , CI, &
                             bohr => BOHR_RADIUS_ANGS, EPS_m8
       USE fft_scalar
       USE timing_module, ONLY : timing
       USE input_module,  ONLY : verbosity
       USE sph_har,       ONLY : gauss1
       USE util_module,   ONLY : gv_indexes, zmat_mul, zmat_unitary

       IMPLICIT NONE

       ! ... arguments

       INTEGER :: npwkx, mxdbnd, mxdnrk
       INTEGER :: npw, ndwinx
       INTEGER :: ngx, ngy, ngz, nkpts
       INTEGER :: npwk(mxdnrk)
       INTEGER :: kgv(3,npw)
       INTEGER :: isort(npwkx,mxdnrk)
       INTEGER :: dimwann
       INTEGER :: dimwin(mxdnrk)
       INTEGER :: dimfroz(mxdnrk)
       REAL(dbl) :: avec(3,3)
       COMPLEX(dbl) :: evec( npwkx + 1, ndwinx, mxdnrk )
       REAL(dbl) :: vkpt(3,mxdnrk)
       COMPLEX(dbl) :: lamp(mxdbnd,mxdbnd,mxdnrk)
       COMPLEX(dbl) :: ca(mxdbnd,dimwann,nkpts)

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
       INTEGER, ALLOCATABLE :: nindpw(:)
       INTEGER :: ngdim(3)
       REAL(dbl) :: aside, asidemin
       COMPLEX(dbl) :: catmp

       COMPLEX(dbl), PARAMETER :: citpi = TPI * CI

       COMPLEX(dbl) :: ctmp, cphi
       INTEGER :: nwann 
       INTEGER :: nxx, nyy, nzz 
       INTEGER :: nx, ny, nz
       REAL(dbl) :: rx, ry, rz
       REAL(dbl) :: rpos1(3), rpos2(3)
       REAL(dbl) :: dist1, dist2
       REAL(dbl) :: scalf
 
       INTEGER :: info

       COMPLEX(dbl), ALLOCATABLE :: tmp(:,:)
       COMPLEX(dbl), ALLOCATABLE :: u(:,:)
       COMPLEX(dbl), ALLOCATABLE :: vt(:,:)
       COMPLEX(dbl), ALLOCATABLE :: work(:)
       REAL(dbl), ALLOCATABLE :: s(:)
       REAL(dbl), ALLOCATABLE :: rwork2(:)
       REAL(dbl), ALLOCATABLE :: rphicmx1(:,:)
       REAL(dbl), ALLOCATABLE :: rphicmx2(:,:)
       INTEGER, ALLOCATABLE :: nphimx1(:,:)
       INTEGER, ALLOCATABLE :: nphimx2(:,:)
       INTEGER, ALLOCATABLE :: nphir(:)

       COMPLEX(dbl), ALLOCATABLE :: cptwr(:)
       COMPLEX(dbl), ALLOCATABLE :: cu(:,:)

       INTEGER :: ierr

! ...  End of declaration

       CALL timing('projection',OPR='start')

       mplwv = ngx * ngy * ( ngz+1 )

       ALLOCATE( tmp(mxdbnd,dimwann), STAT = ierr )
       IF( ierr /= 0 ) CALL errore( ' projection ', ' allocating tmp ', mxdbnd*dimwann )

       ALLOCATE( cptwr(ngx*ngy*(ngz+1)), STAT = ierr )
       IF( ierr /= 0 ) CALL errore( ' projection ', ' allocating cptwr ', ngx*ngy*(ngz+1) )

       ALLOCATE( cu(mxdbnd,dimwann), STAT = ierr )
       IF( ierr /= 0 ) CALL errore( ' projection ', ' allocating cu ', mxdbnd*dimwann )

       ALLOCATE( u(mxdbnd,mxdbnd), STAT = ierr )
       IF( ierr /= 0 ) CALL errore( ' projection ', ' allocating u ', mxdbnd*mxdbnd )

       ALLOCATE( vt(dimwann,dimwann), STAT = ierr )
       IF( ierr /= 0 ) CALL errore( ' projection ', ' allocating vt ', dimwann*dimwann )

       ALLOCATE( work(4*mxdbnd), STAT = ierr )
       IF( ierr /= 0 ) CALL errore( ' projection ', ' allocating work ', 4*mxdbnd )

       ALLOCATE( s(dimwann), STAT = ierr )
       IF( ierr /= 0 ) CALL errore( ' projection ', ' allocating s ', dimwann )

       ALLOCATE( rwork2(5*dimwann), STAT = ierr )
       IF( ierr /= 0 ) CALL errore( ' projection ', ' allocating rwork2 ', 5*dimwann )

       ALLOCATE( rphicmx1(3,dimwann), STAT = ierr )
       IF( ierr /= 0 ) CALL errore( ' projection ', ' allocating rphicmx1 ', 3*dimwann )

       ALLOCATE( rphicmx2(3,dimwann), STAT = ierr )
       IF( ierr /= 0 ) CALL errore( ' projection ', ' allocating rphicmx2 ', 3*dimwann )

       ALLOCATE( nphimx1(3,dimwann), STAT = ierr )
       IF( ierr /= 0 ) CALL errore( ' projection ', ' allocating nphimx1 ', 3*dimwann )

       ALLOCATE( nphimx2(3,dimwann), STAT = ierr )
       IF( ierr /= 0 ) CALL errore( ' projection ', ' allocating nphimx2 ', 3*dimwann )

       ALLOCATE( nphir(dimwann), STAT = ierr )
       IF( ierr /= 0 ) CALL errore( ' projection ', ' allocating nphir ', dimwann )



       DO nwann = 1, dimwann
         DO m = 1, 3
           rphicmx1(m,nwann) = ZERO
           rphicmx2(m,nwann) = ZERO
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
         nphimx1(m,nwann)= INT( rphiimx1(m,nwann) * DBLE( ngdim(m) ) + 1.001*ONE )
         nphimx2(m,nwann)= INT( rphiimx2(m,nwann) * DBLE( ngdim(m) ) + 1.001*ONE )
        END DO
       END DO
       
       DO nwann = 1, dimwann
         asidemin = 100000.0d0 * rloc(nwann)
         DO j = 1, 3
           aside = SQRT( avec(1,j)**2 + avec(2,j)**2 + avec(3,j)**2 )
           asidemin = MIN( aside, asidemin )
         END DO
         nphir(nwann) = nint( 2 * ( rloc(nwann) / asidemin ) * MIN( ngx,ngy,ngz ) )
       END DO

! ...  Calculate the projection of the gaussians on the bloch eigenstates inside 
!      energy window: store it in dimwin(nkp) X dimwann overlap matrix ca

! ...  nbands --> dimwann or dimwin(nkp), and dirc(i,j) --> avec(j,i), 

! ...  Since we are working in bohr, not angstroms (Recall that rloc was also 
!      converted to bohr.), and cexp --> exp 

! ...  Now pick up a consistent phase for the u_nk (the initial ones
!      are provided by the ab-initio code, and so have almost random rotations)

       ALLOCATE( nindpw( npwkx ) )
     
       DO nkp = 1, nkpts
         IF  ( dimwann >  dimfroz(nkp) ) THEN  !IF  not, don't need to waste CPU time!
           DO nb = 1, dimwin(nkp)

             cptwr = CZERO

! ...        Go through all the g-vectors inside the cutoff radius at the present k-point

             CALL gv_indexes( kgv, isort(:,nkp), npwk(nkp), ngx, ngy, ngz, NINDPW=nindpw(:) )
 
             DO J=1,npwk(NKP)           

               npoint = nindpw(j)
               cptwr(npoint) = evec(j,nb,nkp)
 
             ENDDO ! g-vectors at present k (J)
 
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

               ca(nb,nwann,nkp) = CZERO    

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

                     dist1 = ZERO
                     DO m = 1, 3
                       dist1 = dist1 + rpos1(m)**2
                     END DO
                     dist1 = SQRT(dist1)
!
! ...                Positive gaussian
!                    Both dist1 and rloc in Bohr
!
                     cphi = CMPLX( EXP( -( dist1 / rloc(nwann) )**2 ) )

                     IF  ( gauss_typ(nwann) == 1 ) THEN

                       CALL gauss1( cphi, ndir_wann(nwann), l_wann(nwann), &
                                             m_wann(nwann), rpos1, dist1 )

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
                     DO nxx = nphimx2(1,nwann) - nphir(nwann), nphimx2(1,nwann) +nphir(nwann)
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

                       dist2 = ZERO
                       DO m = 1, 3
                         dist2 = dist2 + rpos2(m)**2
                       END DO
                       dist2 = SQRT(dist2)
!
! ...                  Negative gaussian
!                      Both dist2 and rloc in Bohr
!
                       cphi = -CMPLX( EXP( -( dist2/rloc(nwann) )**2 ) )

                       ca(nb,nwann,nkp) = ca(nb,nwann,nkp) + catmp * cphi

                     ENDDO ! nxx
                   ENDDO  ! nyy
                 ENDDO   ! nzz
               END IF 

             ENDDO    ! nwann
           ENDDO    ! nb
      
         END IF  

       ENDDO ! NKP
       DEALLOCATE( nindpw )

 
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
 
! ...      Singular value decomposition
           tmp(:,:) = ca(:,:,nkp)
 
           CALL zgesvd( 'a', 'a', dimwin(nkp), dimwann, tmp(1,1),              &
                mxdbnd, s, u, mxdbnd, vt, dimwann, work, 4*mxdbnd, rwork2, info )

           IF ( info /= 0 )  &
             CALL errore( ' projection ', ' zgesvd: info has illegal value ', info )
 
           DO j=1,dimwann
             DO i=1,dimwin(nkp)
               cu(i,j)=CZERO
               DO l=1,dimwann
                 cu(i,j)=cu(i,j)+u(i,l)*CONJG(vt(j,l))
               END DO
             END DO
           END DO

! ...        Note that cu.transpose(cu) is *NOT* an identity dimwin(nkp) by dimwin(nkp) 
!            matrix, but transpose(cu).cu is a dimwann by dimwann identity matrix. 
!            I have once checked the former statement, now I will just leave here the code
!            for the latter (what this means is that the columns of cu are orthonormal
!            vectors). For this reasons SIDE is set = 'left' in the next function call
 
           IF ( .NOT. zmat_unitary( cu(1:dimwin(nkp),1:dimwann), &
                                    SIDE='left', TOLL=EPS_m8 ) ) &
                  CALL errore('projection', 'Vectors in CU not orthonormal ',nkp)
           lamp(  1:dimwin(nkp), 1:dimwann , nkp) = cu( 1:dimwin(nkp), 1:dimwann )
         ENDIF
       ENDDO ! kpoints nkp

       DEALLOCATE( cptwr, STAT=ierr )
           IF (ierr/=0) CALL errore(' projection ',' deallocating cptwr',ABS(ierr))
       DEALLOCATE( tmp, STAT=ierr )
           IF (ierr/=0) CALL errore(' projection ',' deallocating tmp',ABS(ierr))
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
