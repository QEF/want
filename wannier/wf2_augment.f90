!
! Copyright (C) 2007 WanT Group
!
! This file is distributed under the terms of the
! GNU General Public License. See the file `License'
! in the root directory of the present distribution,
! or http://www.gnu.org/copyleft/gpl.txt .
!
!***************************************************************
SUBROUTINE wf2_augment( nrxl, nrxh, nryl, nryh, nrzl, nrzh, &
                        nr1, nr2, nr3, nwf, wf2_aug)
   !***************************************************************
   !
   !    This routine computes the augmentation part (real space FFT grid) 
   !    due to USPP of the square modulus of WFs.
   !    Requires the products of phi with all beta functions
   !    in array becp(nkb,n,ik) 
   !
   !  The impemented formula is:
   !
   ! wf2_aug(r) = 1/N^2 \sum_{kk'} \sum_{ijI} <phi_nk|beta_iI> Q_ijI_q(r) *
   !                                           <beta_jI|phi_nk'>
   !
   ! where q=k-k', and |phi_nk> is already contains the U^{k} rotations, i.e. 
   !
   ! |wf> = 1/N \sum_k e^-ikR | phi_nk >
   !
   ! INPUT: 
   !   nrxl, nrxh, nryl, nryh, nrzl, nrzh
   !   nr1, nr2, nr3                        dimensions of the grids
   !   nwf                                  number of WFs
   ! OUTPUT:
   !   wf2_aug         this is actually IN/OUT: the routine adds to
   !                   wf2_aug the corrections that are computed here
   !                   
   !
   USE kinds,                       ONLY : dbl
   USE constants,                   ONLY : CZERO, TPI, ONE, EPS_m6
   USE kpoints_module,              ONLY : nkpts_g, vkpt_g
   USE us_module,                   ONLY : okvan
   USE uspp,                        ONLY : nkb
   USE uspp_param,                  ONLY : upf, lmaxq, nh, nhm
   USE lattice_module,              ONLY : tpiba, bvec
   USE ions_module,                 ONLY : nat, nsp, ityp, tau
   USE ggrids_module,               ONLY : g, igv, npw_rho, ggrids_gv_indexes
   USE struct_fact_data_module,     ONLY : eigts1, eigts2, eigts3
   USE becmod,                      ONLY : becp
   USE log_module,                  ONLY : log_push, log_pop
   USE fft_scalar,                  ONLY : cfft3d
   USE converters_module,           ONLY : cart2cry, cry2cart
   USE timing_module
   !
   IMPLICIT NONE
   !
   ! ... input variables
   !
   INTEGER,           INTENT(in)    :: nrxl, nrxh, nryl, nryh, nrzl, nrzh
   INTEGER,           INTENT(in)    :: nr1, nr2, nr3
   INTEGER,           INTENT(in)    :: nwf
   COMPLEX(KIND=dbl), INTENT(inout) :: wf2_aug(nrxl:nrxh, nryl:nryh, nrzl:nrzh, nwf)
   !
   ! ... local variables
   !
   CHARACTER(11) :: subname="wf2_augment"
   INTEGER       :: nx, ny, nz, nxx, nyy, nzz
   INTEGER       :: nnrx, nnry, nnrz, ir
   INTEGER       :: i, j, ih, jh, ikb, na, nt, ig, iwf
   INTEGER       :: ierr, ik1_g, ik2_g, nq, iq
   LOGICAL       :: lfound
   REAL(dbl)     :: arg, xq(3), xq_aux(3), cost, rtmp(3)
   COMPLEX(dbl)  :: phase, cwork
   !
   INTEGER,      ALLOCATABLE :: map(:), becmap(:,:), qmap(:,:)
   REAL(dbl),    ALLOCATABLE :: gq(:,:), gqmod(:), gqmod2(:), ylmk0(:,:)  
   REAL(dbl),    ALLOCATABLE :: vkpt_cry(:,:), vq(:,:)
   COMPLEX(dbl), ALLOCATABLE :: Qij_m(:), sk(:), caux(:,:), caux_fft(:,:) 


!
!--------------------------
! routine body
!--------------------------
!

   IF ( nkb == 0 .OR. .NOT. okvan ) RETURN
   !
   CALL timing( 'wf2_augment', OPR='start' )  
   CALL log_push( 'wf2_augment' )  
    

   !
   ! setup auxiliary quantities
   !
   ALLOCATE ( gq( 3, npw_rho ), STAT=ierr)
   IF (ierr/=0 ) CALL errore(subname, 'allocating gq', ABS(ierr) )
   !
   ALLOCATE ( gqmod( npw_rho ), gqmod2( npw_rho ), STAT=ierr)
   IF (ierr/=0 ) CALL errore(subname, 'allocating gqmod, gqmod2', ABS(ierr) )
   !
   ALLOCATE ( Qij_m( npw_rho ), STAT=ierr )
   IF (ierr/=0 ) CALL errore(subname, 'allocating Qij_m', ABS(ierr) )
   !
   ALLOCATE ( sk( npw_rho ), STAT=ierr )
   IF (ierr/=0 ) CALL errore(subname, 'allocating sk', ABS(ierr) )
   !
   ALLOCATE ( caux( npw_rho, nwf ), STAT=ierr )
   IF (ierr/=0 ) CALL errore(subname, 'allocating caux', ABS(ierr) )
   !
   ALLOCATE ( caux_fft( nr1*nr2*nr3, nwf ), STAT=ierr )
   IF (ierr/=0 ) CALL errore(subname, 'allocating caux_fft', ABS(ierr) )
   !
   ALLOCATE ( ylmk0( npw_rho, lmaxq * lmaxq), STAT=ierr) 
   IF (ierr/=0 ) CALL errore(subname, 'allocating ylmk0', ABS(ierr) )
   !
   ALLOCATE ( map( npw_rho ), STAT=ierr ) 
   IF (ierr/=0 ) CALL errore(subname, 'allocating map', ABS(ierr) )
   !
   ALLOCATE ( becmap( nhm, nat ), STAT=ierr ) 
   IF (ierr/=0 ) CALL errore(subname, 'allocating becmap', ABS(ierr) )
   !
   ALLOCATE ( qmap( nkpts_g, nkpts_g ), STAT=ierr ) 
   IF (ierr/=0 ) CALL errore(subname, 'allocating qmap', ABS(ierr) )
   !
   ALLOCATE ( vq( 3, nkpts_g**2 ), STAT=ierr ) 
   IF (ierr/=0 ) CALL errore(subname, 'allocating vq', ABS(ierr) )
   !
   ALLOCATE ( vkpt_cry( 3, nkpts_g ), STAT=ierr ) 
   IF (ierr/=0 ) CALL errore(subname, 'allocating vkpt_cry', ABS(ierr) )

   !
   ! aux quantities to work on the real space grid
   !
   nnrx = ABS( nrxl / nr1 ) + 2
   nnry = ABS( nryl / nr2 ) + 2
   nnrz = ABS( nrzl / nr3 ) + 2
   !
   ! normalization factor
   !
   cost =  ONE / ( REAL(nkpts_g**2, dbl) )


   !
   ! set a map between the rho G sphere (igv) and the
   ! adopted fft mesh
   !
   CALL ggrids_gv_indexes( igv, npw_rho, nr1, nr2, nr3, GV2FFT=map )
  
   !
   ! We need to set a map linking (ih, na) to the respective
   ! index of the beta projectors vkb
   !
   ikb = 0
   becmap( :, :) = -1
   !
   DO nt = 1, nsp
       IF ( upf(nt)%tvanp ) THEN
           ! 
           DO na = 1, nat
               IF ( ityp (na) == nt) THEN
                   !
                   DO ih = 1, nh (nt)
                       ikb = ikb + 1
                       becmap( ih, na ) = ikb
                   ENDDO
                   !
               ENDIF
           ENDDO
           ! 
       ENDIF
   ENDDO

   !
   ! find the indipendent q-vectors in BZ
   ! q = k1 - k2
   !
   vkpt_cry(:,:) = vkpt_g(:,:)
   CALL cart2cry( vkpt_cry, bvec )
   !
   nq = 0
   !
   DO ik2_g = 1, nkpts_g
   DO ik1_g = 1, nkpts_g
       !
       ! crystal units
       ! xq are in the BZ centered around 0
       ! this is needed to minimize the errors due to the fact
       ! that we are using the G-set corresponding to the charge density
       ! instead of centering the cutoff sphere around each q-vector
       !
       xq(:) = MODULO( vkpt_cry(:,ik1_g) - vkpt_cry(:,ik2_g) +0.5_dbl, ONE ) -0.5_dbl
       !
       lfound = .FALSE.
       DO iq = 1, nq
           !
           rtmp(1) = MODULO( xq(1)-vq(1,iq), ONE )
           rtmp(2) = MODULO( xq(2)-vq(2,iq), ONE )
           rtmp(3) = MODULO( xq(3)-vq(3,iq), ONE )
           !
           ! the second condition ( after .OR. ) should be useless,
           ! but some compilers seem to allow for MODULO(x, ONE) to
           ! retur ONE, and therefore we need to workaround the problem.
           !
           IF ( ( rtmp(1) < EPS_m6 .OR. rtmp(1) > ONE-EPS_m6 )  .AND.  &
                ( rtmp(2) < EPS_m6 .OR. rtmp(2) > ONE-EPS_m6 )  .AND.  &
                ( rtmp(3) < EPS_m6 .OR. rtmp(3) > ONE-EPS_m6 )  ) THEN 
              !
              lfound = .TRUE.
              qmap( ik1_g, ik2_g) = iq
              !
           ENDIF
           !
       ENDDO
       !
       IF ( .NOT. lfound ) THEN
          nq = nq +1 
          vq(:, nq) = xq(:)
          qmap(ik1_g, ik2_g) = nq
       ENDIF
       !
   ENDDO 
   ENDDO 
   !
   DEALLOCATE (vkpt_cry, STAT=ierr)
   IF (ierr/=0 ) CALL errore(subname, 'deallocating vkpt_cry', ABS(ierr) )


!
! main loop
!
   q_vector_loop:&
   DO iq = 1, nq
       !
       ! tpiba units
       xq(:) = vq(:,iq)
       CALL cry2cart( xq, bvec)
       xq(:) = xq(:) / tpiba
       
       !
       DO ig = 1, npw_rho
          gq( :, ig)  = g(:, ig) + xq(:)
          gqmod2( ig) = DOT_PRODUCT( gq(:, ig), gq(:, ig) )
          gqmod ( ig) = SQRT( gqmod2( ig) )
       ENDDO
       !
       ! setting spherical harmonics
       CALL ylmr2 (lmaxq * lmaxq, npw_rho, gq, gqmod2, ylmk0)
       !
       !
       caux( :, :) = CZERO
       !
       DO nt = 1, nsp
          IF ( upf(nt)%tvanp ) THEN
             !
             !
             DO ih = 1, nh (nt)
                DO jh = 1, nh (nt)
                   !
                   ! qgm contains the Q_{ij,q}^I( G ) where G is on the
                   ! reciprocal space grid of the density
                   !
                   CALL qvan2 (npw_rho, ih, jh, nt, gqmod, Qij_m, ylmk0)
                   !
                   !
                   DO na = 1, nat
                       IF (ityp (na) == nt) THEN

                          !
                          ! xq in tpiba, tau in alat: just 2pi is missing
                          !
                          arg = DOT_PRODUCT( xq(:), tau(:,na) ) * TPI
                          phase = CMPLX( COS(arg), -SIN(arg), dbl )   
                          !
                          DO ig = 1, npw_rho
                             !
                             sk(ig) = eigts1 ( igv(1,ig), na) * &
                                      eigts2 ( igv(2,ig), na) * &
                                      eigts3 ( igv(3,ig), na)
                             !
                          ENDDO
                          !
                          !
                          DO iwf = 1, nwf
                              !
                              i = becmap( ih, na )
                              j = becmap( jh, na )
                              !
                              cwork = CZERO
                              DO ik2_g = 1, nkpts_g 
                              DO ik1_g = 1, nkpts_g
                                  !
                                  IF ( qmap(ik1_g, ik2_g) == iq ) THEN 
                                      !
                                      cwork = cwork + becp( i, iwf, ik1_g) * &
                                                      CONJG( becp( j, iwf, ik2_g) )
                                  ENDIF 
                                  !
                              ENDDO
                              ENDDO
                              !
                              !
                              DO ig = 1, npw_rho
                                 ! 
                                 caux( ig, iwf) = caux( ig, iwf) + sk(ig) * Qij_m(ig) * &
                                                                   cwork * phase
                                 !
                              ENDDO
                              !
                          ENDDO
                          !
                       ENDIF
                       !
                       !
                   ENDDO
                   !
                ENDDO
             ENDDO
             !
          ENDIF
       ENDDO
       !
       !
       ! FFT to real space
       !
       xq_aux(1) = vq(1, iq) / REAL( nr1, dbl )
       xq_aux(2) = vq(2, iq) / REAL( nr2, dbl )
       xq_aux(3) = vq(3, iq) / REAL( nr3, dbl )
       !
       !
       DO iwf = 1, nwf 
           !
           caux_fft( :, iwf ) = CZERO
           !
           DO ig = 1, npw_rho
              caux_fft( map(ig), iwf ) = caux( ig , iwf)
           ENDDO
           !
           !
           CALL timing('cfft3d',OPR='start')
           CALL cfft3d( caux_fft(:, iwf), nr1, nr2, nr3,  &
                                          nr1, nr2, nr3,  1 )
           CALL timing('cfft3d',OPR='stop')
           !
           !
           DO nzz = nrzl, nrzh
               nz = MOD( nzz + nnrz * nr3 , nr3 ) + 1
               !
               DO nyy = nryl, nryh
                   ny = MOD( nyy + nnry * nr2 , nr2 ) + 1
                   !
                   DO nxx = nrxl, nrxh
                       nx = MOD( nxx + nnrx * nr1 , nr1 ) + 1
                       !
                       ir = nx + (ny-1) * nr1 + (nz-1) * nr1 * nr2
                       !
                       arg   = ( xq_aux(1) * REAL(nxx, dbl) + &
                                 xq_aux(2) * REAL(nyy, dbl) + &
                                 xq_aux(3) * REAL(nzz, dbl) ) * TPI 
                               !
                       phase = CMPLX( COS( arg ), SIN( arg), dbl )
                       !
                       wf2_aug( nxx, nyy, nzz, iwf) = wf2_aug( nxx, nyy, nzz, iwf) + &
                                                      cost * phase * caux_fft( ir, iwf )
                       !
                   ENDDO
               ENDDO
           ENDDO
           !
       ENDDO
       !
   ENDDO q_vector_loop
   !
   !
   DEALLOCATE (ylmk0, STAT=ierr)
   IF (ierr/=0 ) CALL errore(subname, 'deallocating ylmk0', ABS(ierr) )
   !
   DEALLOCATE (Qij_m, STAT=ierr)
   IF (ierr/=0 ) CALL errore(subname, 'deallocating Qij_m', ABS(ierr) )
   !
   DEALLOCATE (sk, STAT=ierr)
   IF (ierr/=0 ) CALL errore(subname, 'deallocating sk', ABS(ierr) )
   !
   DEALLOCATE (gq, gqmod, gqmod2, STAT=ierr)
   IF (ierr/=0 ) CALL errore(subname, 'deallocating gq, gqmod, gqmod2', ABS(ierr) )
   !
   DEALLOCATE (caux, caux_fft, STAT=ierr)
   IF (ierr/=0 ) CALL errore(subname, 'deallocating caux, caux_fft', ABS(ierr) )
   !
   DEALLOCATE (map, STAT=ierr)
   IF (ierr/=0 ) CALL errore(subname, 'deallocating map', ABS(ierr) )
   !
   DEALLOCATE (becmap, STAT=ierr)
   IF (ierr/=0 ) CALL errore(subname, 'deallocating becmap', ABS(ierr) )
   !
   DEALLOCATE (qmap, STAT=ierr)
   IF (ierr/=0 ) CALL errore(subname, 'deallocating qmap', ABS(ierr) )
   !
   DEALLOCATE (vq, STAT=ierr)
   IF (ierr/=0 ) CALL errore(subname, 'deallocating vq', ABS(ierr) )
   !
   !
   CALL timing( 'wf2_augment',OPR='stop' )
   CALL log_pop( 'wf2_augment' )
   !
   RETURN
   !
END SUBROUTINE wf2_augment

