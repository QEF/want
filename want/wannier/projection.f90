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
       SUBROUTINE projection( avec, lamp, ca, evc, vkpt,                   & 
                  kgv, isort, npwk, dimwin, dimwann, dimfroz,              &
                  npwkx, nbnd, npw, ngx, ngy, ngz, nkpts,                  &
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
       USE uspp,          ONLY : qq

       IMPLICIT NONE

       ! ... arguments

       INTEGER :: npwkx, nbnd
       INTEGER :: npw, ndwinx
       INTEGER :: ngx, ngy, ngz, nkpts
       INTEGER :: npwk(nkpts)
       INTEGER :: kgv(3,npw)
       INTEGER :: isort(npwkx,nkpts)
       INTEGER :: dimwann
       INTEGER :: dimwin(nkpts)
       INTEGER :: dimfroz(nkpts)
       REAL(dbl) :: avec(3,3)
       COMPLEX(dbl) :: evc( npwkx, ndwinx, nkpts )
       REAL(dbl) :: vkpt(3,nkpts)
       COMPLEX(dbl) :: lamp(nbnd,nbnd,nkpts)
       COMPLEX(dbl) :: ca(nbnd,dimwann,nkpts)

       INTEGER :: gauss_typ(dimwann)
       REAL(dbl) :: rphiimx1(3,dimwann)
       REAL(dbl) :: rphiimx2(3,dimwann)
       INTEGER :: l_wann(dimwann)
       INTEGER :: m_wann(dimwann)
       INTEGER :: ndir_wann(dimwann)
       REAL(dbl) :: rloc(dimwann)

 
       ! ... local variables

       INTEGER :: ib, i, j, l, m 
       INTEGER :: ik, npoint
       INTEGER, ALLOCATABLE :: nindpw(:)
       INTEGER :: ngdim(3)
       REAL(dbl) :: aside, asidemin
       COMPLEX(dbl) :: catmp

       COMPLEX(dbl), PARAMETER :: citpi = TPI * CI

       COMPLEX(dbl) :: ctmp, cphi
       INTEGER :: iwann 
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
       COMPLEX(dbl), ALLOCATABLE :: aux(:)
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

       ALLOCATE( tmp(nbnd,dimwann), STAT = ierr )
         IF( ierr /= 0 ) CALL errore( 'projection', 'allocating tmp ', nbnd*dimwann )
       ALLOCATE( cptwr(ngx*ngy*(ngz+1)), STAT = ierr )
         IF( ierr /= 0 ) CALL errore( 'projection', 'allocating cptwr ', ngx*ngy*(ngz+1) )
       ALLOCATE( cu(nbnd,dimwann), STAT = ierr )
         IF( ierr /= 0 ) CALL errore( 'projection', 'allocating cu ', nbnd*dimwann )
       ALLOCATE( u(nbnd,nbnd), STAT = ierr )
         IF( ierr /= 0 ) CALL errore( 'projection', 'allocating u ', nbnd*nbnd )
       ALLOCATE( vt(dimwann,dimwann), STAT = ierr )
         IF( ierr /= 0 ) CALL errore( 'projection', 'allocating vt ', dimwann*dimwann )
       ALLOCATE( work(4*nbnd), STAT = ierr )
         IF( ierr /= 0 ) CALL errore( 'projection', 'allocating work ', 4*nbnd )
       ALLOCATE( aux(npwkx), STAT = ierr )
         IF( ierr /= 0 ) CALL errore( 'projection', 'allocating aux ', npwkx )
       ALLOCATE( s(dimwann), STAT = ierr )
         IF( ierr /= 0 ) CALL errore( 'projection', 'allocating s ', dimwann )
       ALLOCATE( rwork2(5*dimwann), STAT = ierr )
         IF( ierr /= 0 ) CALL errore( 'projection', 'allocating rwork2 ', 5*dimwann )
       ALLOCATE( rphicmx1(3,dimwann), STAT = ierr )
         IF( ierr /= 0 ) CALL errore( 'projection', 'allocating rphicmx1 ', 3*dimwann )
       ALLOCATE( rphicmx2(3,dimwann), STAT = ierr )
         IF( ierr /= 0 ) CALL errore( 'projection', 'allocating rphicmx2 ', 3*dimwann )
       ALLOCATE( nphimx1(3,dimwann), STAT = ierr )
         IF( ierr /= 0 ) CALL errore( 'projection', 'allocating nphimx1 ', 3*dimwann )
       ALLOCATE( nphimx2(3,dimwann), STAT = ierr )
         IF( ierr /= 0 ) CALL errore( 'projection', 'allocating nphimx2 ', 3*dimwann )
       ALLOCATE( nphir(dimwann), STAT = ierr )
         IF( ierr /= 0 ) CALL errore( 'projection', 'allocating nphir ', dimwann )


       DO iwann = 1, dimwann
         DO m = 1, 3
           rphicmx1(m,iwann) = ZERO
           rphicmx2(m,iwann) = ZERO
           DO j = 1, 3
             rphicmx1(m,iwann) = rphicmx1(m,iwann) + rphiimx1(j,iwann) * avec(m,j)
             rphicmx2(m,iwann) = rphicmx2(m,iwann) + rphiimx2(j,iwann) * avec(m,j)
           END DO
         END DO
       END DO

       ngdim(1) = ngx
       ngdim(2) = ngy
       ngdim(3) = ngz
       DO iwann = 1, dimwann
        DO m = 1, 3
         nphimx1(m,iwann)= INT( rphiimx1(m,iwann) * DBLE( ngdim(m) ) + 1.001*ONE )
         nphimx2(m,iwann)= INT( rphiimx2(m,iwann) * DBLE( ngdim(m) ) + 1.001*ONE )
        END DO
       END DO
       
       DO iwann = 1, dimwann
         asidemin = 100000.0d0 * rloc(iwann)
         DO j = 1, 3
           aside = SQRT( avec(1,j)**2 + avec(2,j)**2 + avec(3,j)**2 )
           asidemin = MIN( aside, asidemin )
         END DO
         nphir(iwann) = nint( 2 * ( rloc(iwann) / asidemin ) * MIN( ngx,ngy,ngz ) )
       END DO

! ...  Calculate the projection of the gaussians on the bloch eigenstates inside 
!      energy window: store it in dimwin(ik) X dimwann overlap matrix ca

! ...  nbands --> dimwann or dimwin(ik), and dirc(i,j) --> avec(j,i), 

! ...  Since we are working in bohr, not angstroms (Recall that rloc was also 
!      converted to bohr.), and cexp --> exp 

! ...  Now pick up a consistent phase for the u_nk (the initial ones
!      are provided by the ab-initio code, and so have almost random rotations)

       ALLOCATE( nindpw( npwkx ) )

       ca(:,:,:) = CZERO
       DO ik = 1, nkpts
         IF  ( dimwann >  dimfroz(ik) ) THEN  !IF  not, don't need to waste CPU time!
           DO ib = 1, dimwin(ik)
             !
             ! ... Go through all the g-vectors inside the cutoff radius 
             !
             CALL gv_indexes( kgv, isort(:,ik), npwk(ik), ngx, ngy, ngz, NINDPW=nindpw(:) )
             !
             ! ... apply the US augmentation
             !
! XXX qui si puo' fare di meglio
             CALL augment_psi( npwkx, npwk(ik), 1, ik, qq, evc(1,ib,ik), aux )
!
! DEBUG sostituisco con questo
!             aux(1:npwk(ik)) = evc(1:npwk(ik),ib,ik)
             aux(npwk(ik)+1:npwkx) = CZERO

             cptwr = CZERO
             DO j=1,npwk(ik)           
                 npoint = nindpw(j)
                 cptwr(npoint) = aux(j)
             ENDDO 
 
! ...        Compute the bloch state in the real space grid via fft from the g-space grid
! ...        last argument is isign=-1 since we are using the usual bloch convention.
!            isign = -1 : backward fft: recip --> real : exp(+iqr)
 
             CALL cfft3d( cptwr, ngx, ngy, ngz, ngx, ngy, ngz, 1)
 
! ...        ca: the phase <u_nk(r) exp(ikr) | phi_iwann(r)>, given a real space
!            localized function phi_iwann (e.g. a gaussian centered on the bond)
! ...        nxx, nyy, nzz span a parallelogram in the real space mesh, of side
!            2*nphir, and centered around the maximum of phi_i, nphimax(i,1:3)
! ...        nx ny nz are the nxx nyy nzz brought back to the unit cell in
!            which u_nk(r)=cptwr is represented
! ...        Loop over trial localized orbitals

             DO iwann = 1, dimwann

               ca(ib,iwann,ik) = CZERO    
               !
               ! ... First gaussian
               !
               DO nzz = nphimx1(3,iwann) - nphir(iwann), nphimx1(3,iwann) + nphir(iwann)
                 nz = MOD( nzz, ngz )
                 IF  ( nz < 1 ) nz = nz + ngz
                 DO nyy = nphimx1(2,iwann) - nphir(iwann), nphimx1(2,iwann) + nphir(iwann)
                   ny = MOD( nyy, ngy )
                   IF  ( ny < 1 ) ny = ny + ngy
                   DO nxx = nphimx1(1,iwann) - nphir(iwann), nphimx1(1,iwann) + nphir(iwann)
                     nx = MOD( nxx, ngx )
                     IF  (nx < 1 ) nx = nx + ngx
                 
                     !
                     ! ... Here it calculates <exp(i*k.r) u_nk(r)|
                     !
                     rx = DBLE(nxx-1) / DBLE(ngx)
                     ry = DBLE(nyy-1) / DBLE(ngy)
                     rz = DBLE(nzz-1) / DBLE(ngz)
                     scalf = vkpt(1,ik)*rx + vkpt(2,ik)*ry + vkpt(3,ik)*rz
                     npoint = nx + (ny-1)*ngx + (nz-1)*ngy*ngx
                     catmp = CONJG( EXP( citpi * scalf ) * cptwr(npoint) )
         
                     DO m = 1, 3
                       rpos1(m) = ( rx - rphiimx1(1,iwann) ) * avec(m,1) +  &
                                  ( ry - rphiimx1(2,iwann) ) * avec(m,2) +  &
                                  ( rz - rphiimx1(3,iwann) ) * avec(m,3)
                     END DO

                     dist1 = ZERO
                     DO m = 1, 3
                       dist1 = dist1 + rpos1(m)**2
                     END DO
                     dist1 = SQRT(dist1)
                     !
                     ! ...  Positive gaussian
                     !      Both dist1 and rloc in Bohr
                     !
                     cphi = CMPLX( EXP( -( dist1 / rloc(iwann) )**2 ) )

                     IF  ( gauss_typ(iwann) == 1 ) THEN

                       CALL gauss1( cphi, ndir_wann(iwann), l_wann(iwann), &
                                             m_wann(iwann), rpos1, dist1 )

                     END IF  ! orbital is of type 1

                     ca(ib,iwann,ik) = ca(ib,iwann,ik) + catmp * cphi

                   END DO ! nxx
                 END DO  ! nyy
               END DO   ! nzz

               IF ( gauss_typ(iwann) == 2 ) THEN
                 ! ... second gaussian

                 DO nzz = nphimx2(3,iwann) - nphir(iwann), nphimx2(3,iwann) + nphir(iwann)
                   nz = MOD( nzz, ngz )
                   IF ( nz < 1 ) nz = nz + ngz
                   DO nyy = nphimx2(2,iwann) - nphir(iwann), nphimx2(2,iwann) + nphir(iwann)
                     ny = MOD( nyy, ngy )
                     IF ( ny < 1 ) ny = ny + ngy
                     DO nxx = nphimx2(1,iwann) - nphir(iwann), nphimx2(1,iwann) +nphir(iwann)
                       nx = MOD( nxx, ngx )
                       IF  (nx < 1) nx = nx + ngx

                       ! ... here it calculates <exp(i*k.r) u_nk(r)|

                       rx = DBLE(nxx-1) / DBLE(ngx)
                       ry = DBLE(nyy-1) / DBLE(ngy)
                       rz = DBLE(nzz-1) / DBLE(ngz)
                       scalf = vkpt(1,ik)*rx + vkpt(2,ik)*ry + vkpt(3,ik)*rz
                       npoint = nx + (ny-1)*ngx + (nz-1)*ngy*ngx
                       catmp=CONJG( EXP( citpi * scalf ) * cptwr(npoint) )
          
                       DO m = 1, 3
                         rpos2(m) = ( rx - rphiimx2(1,iwann) ) * avec(m,1) +    &
                                    ( ry - rphiimx2(2,iwann) ) * avec(m,2) +    &
                                    ( rz - rphiimx2(3,iwann) ) * avec(m,3)
                       END DO

                       dist2 = ZERO
                       DO m = 1, 3
                         dist2 = dist2 + rpos2(m)**2
                       END DO
                       dist2 = SQRT(dist2)
                       !
                       ! ... Negative gaussian
                       !     Both dist2 and rloc in Bohr
                       !
                       cphi = -CMPLX( EXP( -( dist2/rloc(iwann) )**2 ) )

                       ca(ib,iwann,ik) = ca(ib,iwann,ik) + catmp * cphi

                     ENDDO ! nxx
                   ENDDO  ! nyy
                 ENDDO   ! nzz
               END IF 

             ENDDO    ! iwann
           ENDDO    ! ib
      
         END IF  

       ENDDO ! kpoints ik
       DEALLOCATE( nindpw )

 
! ...  Compute the dimwin(k) x dimwann matrix cu that yields, from the dimwin(k) 
!      original bloch states, the dimwann bloch-like states with maximal projection
!      onto the dimwann gaussians:
!      cu = ca.cs^{-1/2}, cs = transpose(ca).ca
 
! ...  use the singular-value decomposition of the matrix ca: 
!      ca = cz.cd.cv  
!      which yields
!      cu = cz.cd.cd^{-1}.cv
! ...  where cz is dimwin(ik) x dimwin(ik) and unitary, cd is 
!      dimwin(ik) X dimwann and diagonal, cd^{-1} is dimwann X dimwann and 
!      diagonal, and cv is dimwann x dimwann and unitary.
 
       DO ik=1,nkpts
         IF ( dimwann > dimfroz(ik) ) THEN
 
           ! ... Singular value decomposition
           tmp(:,:) = ca(:,:,ik)
 
           CALL zgesvd( 'a', 'a', dimwin(ik), dimwann, tmp(1,1),              &
                nbnd, s, u, nbnd, vt, dimwann, work, 4*nbnd, rwork2, info )

           IF ( info /= 0 )  &
             CALL errore( ' projection ', ' zgesvd: info has illegal value ', info )
 
           CALL zmat_mul( cu, u, 'N', vt, 'C', dimwin(ik), dimwann, dimwann ) 

           ! NOTA BENE:  cu.transpose(cu) is *NOT* an identity dimwin(ik) by dimwin(ik) 
           ! matrix, but transpose(cu).cu is a dimwann by dimwann identity matrix. 
           ! I have once checked the former statement, now I will just leave here the code
           ! for the latter (what this means is that the columns of cu are orthonormal
           ! vectors). For this reasons SIDE is set = 'left' in the next function call
           ! 
           IF ( .NOT. zmat_unitary( cu(1:dimwin(ik),1:dimwann), &
                                    SIDE='left', TOLL=EPS_m8 ) ) &
                  CALL errore('projection', 'Vectors in CU not orthonormal ',ik)
           lamp(  1:dimwin(ik), 1:dimwann , ik) = cu( 1:dimwin(ik), 1:dimwann )
         ENDIF
       ENDDO ! kpoints ik

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
