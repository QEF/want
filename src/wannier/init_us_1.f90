!
! Copyright (C) 2001 PWSCF group
! This file is distributed under the terms of the
! GNU General Public License. See the file `License'
! in the root directory of the present distribution,
! or http://www.gnu.org/copyleft/gpl.txt .
!
!----------------------------------------------------------------------
SUBROUTINE init_us_1
  !----------------------------------------------------------------------
  !
  !   This routine performs the following tasks:
  !   a) For each non vanderbilt pseudopotential it computes the D and
  !      the betar in the same form of the Vanderbilt pseudopotential.
  !   b) It computes the indices indv which establish the correspondence
  !      nh <-> beta in the atom
  !   c) It computes the indices nhtol which establish the correspondence
  !      nh <-> angular momentum of the beta function
  !   d) It computes the indices nhtolm which establish the correspondence
  !      nh <-> combined (l,m) index for the beta function.
  !   e) It computes the coefficients c_{LM}^{nm} which relates the
  !      spherical harmonics in the Q expansion
  !   f) It computes the radial fourier transform of the Q function on
  !      all the g vectors
  !   g) It computes the q terms which define the S matrix.
  !   h) It fills the interpolation table for the beta functions
  !
  !   i) computes also the terms qb = \int dr e^{-ibr} Q_ij(r) used for 
  !      the augmentation of the overlaps in wannier function calc.
  !
  USE kinds,           ONLY : dbl
  USE parameters,      ONLY : lmaxx, nbrx, lqmax
  USE constants,       ONLY : FPI, ZERO, CZERO, ONE, CONE, SQRT2_constant => SQRT2, EPS_m8
  USE log_module,      ONLY : log_push, log_pop
  USE atom_module,     ONLY : rgrid
  USE ions_module,     ONLY : nsp
  USE lattice_module,  ONLY : omega, tpiba
  USE ggrids_module,   ONLY : g, gg
  USE us_module,       ONLY : okvan, nqxq, dq, nqx, tab, qrad
  USE uspp,            ONLY : nhtol, nhtolm, dvan, qq, qb, indv, ap, aainit
  USE uspp_param,      ONLY : upf, lmaxq, nbetam, lmaxkb, nh, nhm
  !
!  USE uspp,            ONLY : ijtoh
!#ifdef __PARA
!  USE uspp_param,      ONLY : nhm
!#endif
  USE control_module,  ONLY : use_blimit
  !
  ! added for WFs
  USE kpoints_module,  ONLY : nb, vb
  USE timing_module
  !
  IMPLICIT NONE
  !
  !     here a few local variables
  !

  INTEGER :: nt, ih, jh, ib, jb, ijv, l, m, ir, iq, is, startq, &
       lastq, ilast, ndm
  ! various counters
  REAL(kind=dbl), ALLOCATABLE :: aux (:), aux1 (:), besr (:), qtot (:,:)
  REAL(kind=dbl), ALLOCATABLE :: bkvect(:,:), bkmod(:), bkmod2(:)
  ! various work space
  REAL(kind=dbl) :: prefr, pref, q, qi
  ! the prefactor of the q functions
  ! the prefactor of the beta functions
  ! the modulus of g for each shell
  ! q-point grid for interpolation
  REAL(kind=dbl), ALLOCATABLE :: ylmk0 (:,:)
  ! the spherical harmonics
  REAL(kind=dbl) ::  vqint
  ! the denominator in KB case
  ! interpolated value
  INTEGER :: nn, ierr
  COMPLEX(kind=dbl), ALLOCATABLE :: qgm(:)

  CALL timing('init_us_1',OPR='start')
  CALL log_push('init_us_1')
  !
  !    Initialization of the variables
  !
  ndm    = MAXVAL ( upf(:)%kkbeta )
  nhm    = MAXVAL (nh (1:nsp)) 
  nbetam = MAXVAL ( upf(:)%nbeta )
  !
  ALLOCATE (aux ( ndm ), STAT=ierr)    
  IF ( ierr/=0) CALL errore('init_us_1','allocating aux',ABS(ierr))
  !
  ALLOCATE (aux1( ndm ), STAT=ierr)    
  IF ( ierr/=0) CALL errore('init_us_1','allocating aux1',ABS(ierr))
  !
  !
  ALLOCATE (qgm( nb ), STAT=ierr)    
  IF ( ierr/=0) CALL errore('init_us_1','allocating qgm',ABS(ierr))
  !
  ALLOCATE (bkvect( 3, nb ), STAT=ierr)    
  IF ( ierr/=0) CALL errore('init_us_1','allocating bkvect',ABS(ierr))
  !
  ALLOCATE (bkmod( nb ), STAT=ierr)    
  IF ( ierr/=0) CALL errore('init_us_1','allocating bkmod',ABS(ierr))
  !
  ALLOCATE (bkmod2( nb ), STAT=ierr)    
  IF ( ierr/=0) CALL errore('init_us_1','allocating bkmod2',ABS(ierr))
  !
  ALLOCATE (besr( ndm), STAT=ierr)    
  IF ( ierr/=0) CALL errore('init_us_1','allocating besr',ABS(ierr))
  !
  ALLOCATE (qtot( ndm ,  nbetam*(nbetam+1)/2 ), STAT=ierr)    
  IF ( ierr/=0) CALL errore('init_us_1','allocating qtot',ABS(ierr))

  ap (:,:,:)   = ZERO
  IF (lmaxq > 0) qrad(:,:,:,:)= ZERO

  !
  ! the following prevents an out-of-bound error: upf(nt)%nqlc=2*lmax+1
  ! but in some versions of the PP files lmax is not set to the maximum
  ! l of the beta functions but includes the l of the local potential
  !
  DO nt=1,nsp
     upf(nt)%nqlc = MIN ( upf(nt)%nqlc, lmaxq )
     IF ( upf(nt)%nqlc < 0 )  upf(nt)%nqlc = 0 
  ENDDO

  prefr = FPI / omega
  qq (:,:,:)   = ZERO
  dvan = ZERO
  !
  !   For each pseudopotential we initialize the indices nhtol, nhtolm,
  !   nhtoj, indv, and if the pseudopotential is of KB type we initialize the
  !   atomic D terms
  !
  do nt = 1, nsp
     ih = 1 
     do ib = 1, upf(nt)%nbeta
        l = upf(nt)%lll (ib)
        do m = 1, 2 * l + 1 
           nhtol (ih, nt) = l 
           nhtolm(ih, nt) = l*l+m
           indv  (ih, nt) = ib
           ih = ih + 1 
        enddo
     enddo
!     ! ijtoh map augmentation channel indexes ih and jh to composite
!     ! "triangular" index ijh
!     ijtoh(:,:,nt) = -1
!     ijv = 0
!     do ih = 1,nh(nt)
!         do jh = ih,nh(nt)
!             ijv = ijv+1
!             ijtoh(ih,jh,nt) = ijv
!             ijtoh(jh,ih,nt) = ijv
!         enddo
!     enddo
     !
     !    From now on the only difference between KB and US pseudopotentials
     !    is in the presence of the q and Q functions.
     !
     !    Here we initialize the D of the solid
     !    in the not spin_orbit case
     !
     do ih = 1, nh (nt)
     do jh = 1, nh (nt)
         if (nhtol (ih, nt) == nhtol (jh, nt) .and. &
             nhtolm(ih, nt) == nhtolm(jh, nt) ) then
             ir = indv (ih, nt)
             is = indv (jh, nt)
             dvan (ih, jh, nt) = upf(nt)%dion (ir, is)
         endif
     enddo
     enddo
  enddo
  !
  !  compute Clebsch-Gordan coefficients
  !
  if (okvan) call aainit (lmaxkb + 1)
  !
  !   here for the US types we compute the Fourier transform of the
  !   Q functions.
  !

  !
  !    this routine is used in parallel implementation. 
  !    here it is not the case
  !
  ! CALL divide (nqxq, startq, lastq)
  !
  !    the routine is serially replace with the follwing lines
  startq = 1
  lastq = nqxq
  !
  
  do nt = 1, nsp
     if ( upf(nt)%tvanp ) then
        do l = 0, upf(nt)%nqlc - 1
           !
           !     first we build for each ib,jb,l the total Q(|r|) function
           !     note that l is the true angular momentum, and the arrays
           !     have dimensions 1..l+1
           !
           do ib = 1, upf(nt)%nbeta
              do jb = ib, upf(nt)%nbeta
                 !
                 respect_sum_rule : &
                 if ( (l >= abs (upf(nt)%lll(ib) - upf(nt)%lll(jb) ) ) .and. &
                      (l <=      upf(nt)%lll(ib) + upf(nt)%lll(jb) )   .and. &
                      (mod (l+upf(nt)%lll(ib) + upf(nt)%lll(jb), 2) == 0) ) then
                    ijv = jb * (jb-1) / 2 + ib
                    !
                    paw : & ! in PAW formalism aug. charge is computed elsewhere
                    if (upf(nt)%q_with_l .or. upf(nt)%tpawp) then
                         qtot(1:upf(nt)%kkbeta,ijv) =&
                                     upf(nt)%qfuncl(1:upf(nt)%kkbeta,ijv,l)
                    else
                        do ir = 1, upf(nt)%kkbeta
                           if ( rgrid(nt)%r(ir) >= upf(nt)%rinner (l+1) ) then
                              qtot (ir, ijv) = upf(nt)%qfunc (ir, ijv)
                           else
                              ilast = ir
                           endif
                        enddo
                        !
                        if ( upf(nt)%rinner (l+1) > ZERO ) &
                             call setqf(upf(nt)%qfcoef (1, l+1, ib, jb), &
                                        qtot(1,ijv), rgrid(nt)%r, upf(nt)%nqf,l,ilast)
                    endif paw
                    !
                 endif respect_sum_rule
              enddo
           enddo
           !
           !     here we compute the spherical bessel function for each |g|
           !
           do iq = startq, lastq
              q = (iq - 1) * dq * tpiba
              call sph_bes (upf(nt)%kkbeta, rgrid(nt)%r, q, l, aux)
              !
              !   and then we integrate with all the Q functions
              !
              do ib = 1, upf(nt)%nbeta
                 !
                 !    the Q are symmetric with respect to indices
                 !
                 do jb = ib, upf(nt)%nbeta
                    ijv = jb * (jb - 1) / 2 + ib
                    if ( (l >= abs ( upf(nt)%lll (ib) - upf(nt)%lll (jb) ) ) .and. &
                         (l <=       upf(nt)%lll (ib) + upf(nt)%lll (jb) )   .and. &
                         (mod (l+upf(nt)%lll(ib) + upf(nt)%lll(jb), 2) == 0) ) then
                       do ir = 1, upf(nt)%kkbeta
                          aux1 (ir) = aux (ir) * qtot (ir, ijv)
                       enddo
                       call simpson (upf(nt)%kkbeta, aux1, rgrid(nt)%rab, &
                                     qrad(iq,ijv,l + 1, nt) )
                    endif
                 enddo
              enddo
              ! igl
           enddo
           ! l
        enddo
        qrad (:, :, :, nt) = qrad (:, :, :, nt)*prefr
!#ifdef __PARA
!        call reduce (nqxq * nbrx * (nbrx + 1) / 2 * lmaxq, qrad (1, 1, 1, nt) )
!#endif
     endif
     ! nsp

  enddo
  !
  !   and finally we compute the qq coefficients by integrating the Q.
  !   q are the g=0 components of Q.
  !
!#ifdef __PARA
!  if (gg (1) > EPS_m8 ) goto 100
!#endif
  ALLOCATE (ylmk0( 1, lmaxq * lmaxq), STAT=ierr)    
    IF ( ierr/=0) CALL errore('init_us_1','allocating aux',ABS(ierr))
  !
  call ylmr2 (lmaxq * lmaxq, 1, g, gg, ylmk0)
  do nt = 1, nsp
    if ( upf(nt)%tvanp ) then
        do ih = 1, nh (nt)
          do jh = ih, nh (nt)
             call qvan2 (1, ih, jh, nt, gg, qgm, ylmk0)
             qq (ih, jh, nt) = omega * REAL (qgm (1), dbl )
             qq (jh, ih, nt) = qq (ih, jh, nt)
          enddo
        enddo
    endif
  enddo
  DEALLOCATE (ylmk0, STAT=ierr)    
    IF ( ierr/=0) CALL errore('init_us_1','deallocating aux',ABS(ierr))

  !
  ! ... qb computation, added for WFs
  !     Conversion from bohr^-1 to tpiba units is performed for the b vecotrs.
  !
  DO nn=1,nb
       bkvect(1:3,nn) = vb(1:3,nn) / tpiba
       bkmod2(nn)     = bkvect(1,nn)**2 + bkvect(2,nn)**2 + bkvect(3,nn)**2 
       bkmod(nn)      = SQRT( bkmod2(nn) )
  ENDDO

  !
  ! using this option allows for the calculation of overlap
  ! augmentations using b = 0 (thermodynamic limit)
  !
  IF ( use_blimit) THEN
       bkvect(:,1:nb) = ZERO
       bkmod2(1:nb)   = ZERO
       bkmod(1:nb)    = ZERO
  ENDIF

  ALLOCATE (ylmk0( nb, lmaxq * lmaxq), STAT=ierr)    
  IF ( ierr/=0) CALL errore('init_us_1','allocating aux',ABS(ierr))

  CALL ylmr2 (lmaxq * lmaxq, nb, bkvect, bkmod2, ylmk0)
  DO nt = 1, nsp
     IF ( upf(nt)%tvanp ) THEN
         DO ih = 1, nh (nt)
         DO jh = 1, nh (nt)
            CALL qvan2 (nb, ih, jh, nt, bkmod, qgm, ylmk0)
            qb (ih, jh, nt, 1:nb) = omega * qgm (1:nb) 
         ENDDO
         ENDDO
     ENDIF
  ENDDO
  !
  DEALLOCATE (ylmk0, STAT=ierr)    
  IF ( ierr/=0) CALL errore('init_us_1','deallocating aux',ABS(ierr))
  


!#ifdef __PARA
!100 continue
!  call reduce ( nhm * nhm * nsp, qq )
!#endif
  !
  !     fill the interpolation table tab
  !
  pref = FPI / sqrt (omega)
  !
  !     as before, because of the current serial implementation
  !     the call to DIVIDE is replace
  !
  ! CALL divide (nqx, startq, lastq)
  !
  startq = 1
  lastq = nqx
  !

  tab (:,:,:) = ZERO
  do nt = 1, nsp
     do ib = 1, upf(nt)%nbeta
        l = upf(nt)%lll(ib)
        do iq = startq, lastq
           qi = (iq - 1) * dq
           call sph_bes (upf(nt)%kkbeta, rgrid(nt)%r, qi, l, besr)
           do ir = 1, upf(nt)%kkbeta
              aux (ir) = upf(nt)%beta (ir, ib) * besr (ir) * rgrid(nt)%r(ir)
           enddo
           call simpson (upf(nt)%kkbeta, aux, rgrid(nt)%rab, vqint)
           tab (iq, ib, nt) = vqint * pref
        enddo
     enddo
  enddo
!#ifdef __PARA
!  call reduce (nqx * nbrx * nsp, tab)
!#endif
  DEALLOCATE (qtot)
  DEALLOCATE (besr)
  DEALLOCATE (aux1)
  DEALLOCATE (aux)
  DEALLOCATE (qgm)
  DEALLOCATE (bkvect)
  DEALLOCATE (bkmod)
  DEALLOCATE (bkmod2)

  CALL timing('init_us_1',OPR='stop')
  CALL log_pop('init_us_1')
  !
END SUBROUTINE init_us_1

