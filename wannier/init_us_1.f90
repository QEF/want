!
! Copyright (C) 2001 PWSCF group
! This file is distributed under the terms of the
! GNU General Public License. See the file `License'
! in the root directory of the present distribution,
! or http://www.gnu.org/copyleft/gpl.txt .
!
#include "machine.h"
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
  USE atom_module,     ONLY : r, rab
  USE ions_module,     ONLY : ntyp => nsp
  USE lattice_module,  ONLY : omega, tpiba
  USE ggrids_module,   ONLY : g, gg
  USE us_module,       ONLY : okvan, nqxq, dq, nqx, tab, qrad
  USE uspp,            ONLY : nhtol, nhtoj, nhtolm, dvan, qq, qb, indv, ap, aainit
  USE uspp_param,      ONLY : lmaxq, dion, betar, qfunc, qfcoef, rinner, nbeta, &
                              kkbeta, nqf, nqlc, lll, jjj, lmaxkb, nh, tvanp, nhm
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

  INTEGER :: nt, ih, jh, ib, jb, nmb, l, m, ir, iq, is, startq, &
       lastq, ilast, ndm
  ! various counters
  REAL(kind=dbl), ALLOCATABLE :: aux (:), aux1 (:), besr (:), qtot (:,:,:)
  REAL(kind=dbl), ALLOCATABLE :: bkvect(:,:), bkmod(:)
  ! various work space
  REAL(kind=dbl) :: prefr, pref, q, qi
  ! the prefactor of the q functions
  ! the prefactor of the beta functions
  ! the modulus of g for each shell
  ! q-point grid for interpolation
  REAL(kind=dbl), ALLOCATABLE :: ylmk0 (:,:)
  ! the spherical harmonics
  REAL(kind=dbl) ::  vqint, j
  ! the denominator in KB case
  ! interpolated value
  INTEGER :: nn, ierr
  COMPLEX(kind=dbl), ALLOCATABLE :: qgm(:)

  CALL timing('init_us_1',OPR='start')
  !
  !    Initialization of the variables
  !
  ndm = MAXVAL (kkbeta(1:ntyp))
  ALLOCATE (aux ( ndm), STAT=ierr)    
    IF ( ierr/=0) CALL errore('init_us_1','allocating aux',ABS(ierr))
  ALLOCATE (aux1( ndm), STAT=ierr)    
    IF ( ierr/=0) CALL errore('init_us_1','allocating aux1',ABS(ierr))
    !
  ALLOCATE (qgm( nb ), STAT=ierr)    
    IF ( ierr/=0) CALL errore('init_us_1','allocating qgm',ABS(ierr))
  ALLOCATE (bkvect( 3, nb ), STAT=ierr)    
    IF ( ierr/=0) CALL errore('init_us_1','allocating bkvect',ABS(ierr))
  ALLOCATE (bkmod( nb ), STAT=ierr)    
    IF ( ierr/=0) CALL errore('init_us_1','allocating bkmod',ABS(ierr))
  ALLOCATE (besr( ndm), STAT=ierr)    
    IF ( ierr/=0) CALL errore('init_us_1','allocating besr',ABS(ierr))
  ALLOCATE (qtot( ndm , nbrx , nbrx), STAT=ierr)    
    IF ( ierr/=0) CALL errore('init_us_1','allocating qtot',ABS(ierr))

  ap (:,:,:)   = ZERO
  if (lmaxq > 0) qrad(:,:,:,:)= ZERO

  prefr = FPI / omega
  qq (:,:,:)   = ZERO
  dvan = ZERO
  !
  !   For each pseudopotential we initialize the indices nhtol, nhtolm,
  !   nhtoj, indv, and if the pseudopotential is of KB type we initialize the
  !   atomic D terms
  !
  do nt = 1, ntyp
     ih = 1
     do ib = 1, nbeta (nt)
        l = lll (ib, nt)
        j = jjj (ib, nt)
        do m = 1, 2 * l + 1
           nhtol (ih, nt) = l
           nhtolm(ih, nt) = l*l+m
           nhtoj (ih, nt) = j
           indv  (ih, nt) = ib
           ih = ih + 1
        enddo
     enddo
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
             dvan (ih, jh, nt) = dion (ir, is, nt)
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
  
  do nt = 1, ntyp
     if (tvanp (nt) ) then
        do l = 0, nqlc (nt) - 1
           !
           !     first we build for each ib,jb,l the total Q(|r|) function
           !     note that l is the true angular momentum, and the arrays
           !     have dimensions 1..l+1
           !
           do ib = 1, nbeta (nt)
              do jb = ib, nbeta (nt)
                 if ( (l >= abs (lll (ib, nt) - lll (jb, nt) ) ) .and. &
                      (l <= lll (ib, nt) + lll (jb, nt) )        .and. &
                      (mod (l + lll (ib, nt) + lll (jb, nt), 2) == 0) ) then
                    do ir = 1, kkbeta (nt)
                       if (r (ir, nt) >= rinner (l + 1, nt) ) then
                          qtot (ir, ib, jb) = qfunc (ir, ib, jb, nt)
                       else
                          ilast = ir
                       endif
                    enddo
                   if (rinner (l + 1, nt) > ZERO ) &
                         call setqf(qfcoef (1, l+1, ib, jb, nt), &
                                    qtot(1,ib,jb), r(1,nt), nqf(nt),l,ilast)
                 endif
              enddo
           enddo
           !
           !     here we compute the spherical bessel function for each |g|
           !
           do iq = startq, lastq
              q = (iq - 1) * dq * tpiba
              call sph_bes (kkbeta (nt), r (1, nt), q, l, aux)
              !
              !   and then we integrate with all the Q functions
              !
              do ib = 1, nbeta (nt)
                 !
                 !    the Q are symmetric with respect to indices
                 !
                 do jb = ib, nbeta (nt)
                    nmb = jb * (jb - 1) / 2 + ib
                    if ( (l >= abs (lll (ib, nt) - lll (jb, nt) ) ) .and. &
                         (l <= lll (ib, nt) + lll (jb, nt) )        .and. &
                         (mod (l + lll(ib, nt) + lll(jb, nt), 2) == 0) ) then
                       do ir = 1, kkbeta (nt)
                          aux1 (ir) = aux (ir) * qtot (ir, ib, jb)
                       enddo
                       call simpson (kkbeta(nt), aux1, rab(1, nt), &
                                     qrad(iq,nmb,l + 1, nt) )
                    endif
                 enddo
              enddo
              ! igl
           enddo
           ! l
        enddo
        qrad (:, :, :, nt) = qrad (:, :, :, nt)*prefr
#ifdef __PARA
        call reduce (nqxq * nbrx * (nbrx + 1) / 2 * lmaxq, qrad (1, 1, 1, nt) )
#endif
     endif
     ! ntyp

  enddo
  !
  !   and finally we compute the qq coefficients by integrating the Q.
  !   q are the g=0 components of Q.
  !
#ifdef __PARA
  if (gg (1) > EPS_m8 ) goto 100
#endif
  ALLOCATE (ylmk0( 1, lmaxq * lmaxq), STAT=ierr)    
    IF ( ierr/=0) CALL errore('init_us_1','allocating aux',ABS(ierr))
  !
  call ylmr2 (lmaxq * lmaxq, 1, g, gg, ylmk0)
  do nt = 1, ntyp
    if (tvanp (nt) ) then
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
  !     Before gg is used instead of SQRT(gg) because there we were interested
  !     only in the first element which is gg = 0
  !
  DO nn=1,nb
         bkvect(1:3,nn) = vb(1:3,nn) / tpiba
         bkmod(nn) = SQRT( bkvect(1,nn)**2 + bkvect(2,nn)**2 + bkvect(3,nn)**2 )
  ENDDO

  !
  ! using this option allows for the calculation of overlap
  ! augmentations using b = 0 (thermodynamic limit)
  !
  IF ( use_blimit) THEN
       bkvect(:,1:nb) = ZERO
       bkmod(1:nb) = ZERO
  ENDIF

  ALLOCATE (ylmk0( nb, lmaxq * lmaxq), STAT=ierr)    
  IF ( ierr/=0) CALL errore('init_us_1','allocating aux',ABS(ierr))

  CALL ylmr2 (lmaxq * lmaxq, nb, bkvect, bkmod, ylmk0)
  DO nt = 1, ntyp
     IF (tvanp (nt) ) THEN
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
  


#ifdef __PARA
100 continue
  call reduce ( nhm * nhm * ntyp, qq )
#endif
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
  do nt = 1, ntyp
     do ib = 1, nbeta (nt)
        l = lll (ib, nt)
        do iq = startq, lastq
           qi = (iq - 1) * dq
           call sph_bes (kkbeta (nt), r (1, nt), qi, l, besr)
           do ir = 1, kkbeta (nt)
              aux (ir) = betar (ir, ib, nt) * besr (ir) * r (ir, nt)
           enddo
           call simpson (kkbeta (nt), aux, rab (1, nt), vqint)
           tab (iq, ib, nt) = vqint * pref
        enddo
     enddo
  enddo
#ifdef __PARA
  call reduce (nqx * nbrx * ntyp, tab)
#endif
  deallocate (qtot)
  deallocate (besr)
  deallocate (aux1)
  deallocate (aux)
  deallocate (qgm)
  deallocate (bkvect)
  deallocate (bkmod)

  CALL timing('init_us_1',OPR='stop')
  RETURN
END SUBROUTINE init_us_1

