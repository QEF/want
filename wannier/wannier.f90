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
      PROGRAM wannier
!=----------------------------------------------------------------------------------=

      USE kinds
      USE constants, ONLY: CZERO, CONE, CI, ZERO, ONE, TWO, THREE, FOUR
      USE parameters, ONLY : nstrx
      USE input_module, ONLY : input_manager
      USE control_module, ONLY : ordering_mode, nprint, unitary_thr, verbosity, iphase
      USE timing_module, ONLY : timing, timing_deallocate, timing_overview, global_list
      USE io_module, ONLY : stdout, wan_unit, ioname
      USE files_module, ONLY : file_open, file_close
      USE startup_module, ONLY : startup
      USE cleanup_module, ONLY : cleanup
      USE version_module, ONLY : version_number
      USE util_module, ONLY: zmat_mul, zmat_unitary

      USE want_init_module, ONLY : want_init
      USE summary_module, ONLY : summary
      USE kpoints_module, ONLY: nkpts, nnx, nnhx, &
                          nntot, nnlist, nncell, neigh, bk, wb, bka, wbtot
      USE overlap_module,  ONLY : dimwann, ca, cm
      USE localization_module, ONLY : maxiter0_wan, maxiter1_wan, alpha0_wan, alpha1_wan,&
                       ncg, wannier_thr,  &
                       cu, rave, rave2, r2ave, &
                       Omega_I, Omega_OD, Omega_D, Omega_V, Omega_tot, &
                       localization_allocate, localization_write

!
! ... 
!
      IMPLICIT NONE

      ! external functions
      LOGICAL   :: lselect  ! external function non defined
      REAL(dbl) :: rndm     ! external function giving random numbers (from NR)

      INTEGER :: ik, ik2
      INTEGER :: i, j, k
      INTEGER :: l, m, n
      INTEGER :: info, nn, nnh
      INTEGER :: nwann, nb
      INTEGER :: nsdim, irguide
      INTEGER :: nrguide, ncgfix, ncount
      LOGICAL :: lrguide, lcg
      REAL(dbl) :: epsilon, alpha
      REAL(dbl) :: func_om1, func_om2, func_om3
      REAL(dbl) :: func_old1, func_old2, func_old3
      REAL(dbl) :: rre, rri, omt1, omt2, omt3, omiloc
      REAL(dbl) :: func_del, func_del1, func_del2, func_del3, func0
      REAL(dbl) :: gcnorm1, gcfac, gcnorm0, doda0
      REAL(dbl) :: funca, eqc, eqb, eqa, alphamin, falphamin
      COMPLEX(dbl) :: cfunc_exp1, cfunc_exp2, cfunc_exp3, cfunc_exp

      COMPLEX(dbl), ALLOCATABLE ::  cs(:,:,:) ! cs(dimwann,dimwann,nkpts)
      COMPLEX(dbl), ALLOCATABLE ::  cu0(:,:,:) ! cu0(dimwann,dimwann,nkpts)
      COMPLEX(dbl), ALLOCATABLE ::  csheet(:,:,:) ! csheet(dimwann,nkpts,nnx)
      COMPLEX(dbl), ALLOCATABLE ::  cm0(:,:,:,:) ! cm0(dimwann,dimwann,nkpts,nnx)
      COMPLEX(dbl), ALLOCATABLE ::  cmtmp(:,:) ! cmtmp(dimwann,dimwann)
      COMPLEX(dbl), ALLOCATABLE ::  cdodq(:,:,:) ! cdodq(dimwann,dimwann,nkpts)
      COMPLEX(dbl), ALLOCATABLE ::  cdqkeep(:,:,:) ! cdqkeep(dimwann,dimwann,nkpts)
      COMPLEX(dbl), ALLOCATABLE ::  cdodq1(:,:,:) ! cdodq1(dimwann,dimwann,nkpts)
      COMPLEX(dbl), ALLOCATABLE ::  cdodq2(:,:,:) ! cdodq2(dimwann,dimwann,nkpts)
      COMPLEX(dbl), ALLOCATABLE ::  cdodq3(:,:,:) ! cdodq3(dimwann,dimwann,nkpts)
      COMPLEX(dbl), ALLOCATABLE ::  cdq(:,:,:) ! cdq(dimwann,dimwann,nkpts)
      COMPLEX(dbl), ALLOCATABLE ::  cz(:,:) ! cz(dimwann,dimwann)
      COMPLEX(dbl), ALLOCATABLE ::  cv1(:,:) ! cv1(dimwann,dimwann)
      COMPLEX(dbl), ALLOCATABLE ::  cv2(:,:) ! cv2(dimwann,dimwann)
      COMPLEX(dbl), ALLOCATABLE ::  cv3(:,:) ! cv3(dimwann,dimwann)
      COMPLEX(dbl), ALLOCATABLE ::  cw1(:) ! cw1(10*dimwann)
      COMPLEX(dbl), ALLOCATABLE ::  cw2(:) ! cw2(10*dimwann)
      REAL(dbl), ALLOCATABLE ::  singvd(:) !  singvd(dimwann)
      REAL(dbl), ALLOCATABLE ::  sheet(:,:,:) ! sheet(dimwann,nkpts,nnx)

      REAL(dbl), ALLOCATABLE :: rguide(:,:)!  rguide(3,dimwann)
      COMPLEX(dbl), ALLOCATABLE :: cwschur1(:) !  cwschur1(dimwann)
      COMPLEX(dbl), ALLOCATABLE :: cwschur2(:) !  cwschur2(10*dimwann)
      REAL(dbl),    ALLOCATABLE :: cwschur3(:) !  cwschur3(dimwann)
      LOGICAL,      ALLOCATABLE :: cwschur4(:) !  cwschur4(dimwann)

      INTEGER      :: nwork
      REAL(dbl)    :: rtot(3), r2tot
      COMPLEX(dbl) :: cfact
      CHARACTER( LEN=nstrx )  :: filename
      INTEGER :: idum, rdum, ierr

!
! ... End declarations and dimensions
!
!=----------------------------------------------------------------------------=!


!
! ... Startup
!
      CALL startup(version_number,MAIN_NAME='wannier')

!
! ... Read input parameters from DFT_DATA file
!
      CALL input_manager()

!
! ... Global data init
!
      CALL want_init(WANT_INPUT=.TRUE., WINDOWS=.TRUE., BSHELLS=.TRUE.)

!
! ... Summary of the input and DFT data
!
      CALL summary( stdout )

!
! ... wannier-specific variables init
      CALL localization_allocate()

!
! ... import overlap and projections from the disentangle sotred data
      CALL overlap_extract(dimwann)


      CALL timing('init',OPR='start')
!
!...  Wannier Functions localization procedure
! 
      WRITE(stdout,"(2/,2x,70('='))")
      WRITE(stdout,"(2x,'=',18x,'Starting localization procedure',19x,'=')")
      WRITE(stdout,"(2x,70('=')),/")

      !
      ! ... Now calculate the average positions of the Wanns.

      ALLOCATE( csheet(dimwann,nkpts,nnx), STAT=ierr )
         IF( ierr /=0 ) CALL errore(' wannier ', ' allocating csheet ', dimwann*nkpts*nnx)
      ALLOCATE( sheet(dimwann,nkpts,nnx), STAT=ierr )
         IF( ierr /=0 ) CALL errore(' wannier ', ' allocating sheet ', dimwann*nkpts*nnx)

      sheet(:,:,:) = ZERO
      csheet(:,:,:) = CONE

      CALL omega( dimwann, nkpts, nkpts, nntot(:), nnx, nnlist(:,:), bk(:,:,:), wb(:,:), &
                  cm(:,:,:,:), csheet(:,:,:), sheet(:,:,:), rave(:,:), r2ave(:), rave2(:), &
                  func_om1, func_om2, func_om3, Omega_tot, rtot, r2tot, Omega_I, Omega_D, & 
                  Omega_OD, Omega_V )

      !
      !...  Write centers and spread

      WRITE( stdout, "(/,2x, 'Center coord in Bohr, Spreads (Omega) in Bohr^2 ')")
      DO nwann = 1, dimwann
        WRITE( stdout, " ( 4x, 'Center ', i3, 1x, '= (',f12.6,',',f12.6,',',f12.6,  &
           &' )  Omega = ', f13.6 )" )  nwann,( rave(i,nwann), i=1,3 ),  &
                                        r2ave(nwann) - rave2(nwann)
      END DO  
      WRITE( stdout," ( /,2x, '! Center Sum', &
           &  1x, '= (',f12.6,',',f12.6,',',f12.6,' )  Omega = ', f13.6 )" )     &
           (rtot(i),i=1,3), r2tot
              

      WRITE( stdout, fmt="(/,2x, 'Spread Operator decomposition (Bohr^2) : ')")
      WRITE( stdout, fmt="(  4x,'OmegaI    =   ', f12.6 ) " ) Omega_I
      WRITE( stdout, fmt="(  4x,'OmegaD    =   ', f12.6 ) " ) Omega_D
      WRITE( stdout, fmt="(  4x,'OmegaOD   =   ', f12.6 ) " ) Omega_OD
      WRITE( stdout, fmt="(/,4x,'Omega Tot =   ', f12.6 ) " ) Omega_tot

      func_old1 = func_om1
      func_old2 = func_om2
      func_old3 = func_om3


! ... Now I calculate the transformation matrix CU = CS^(-1/2).CA,
!     where CS = CA.CA^\dagger. CS is diagonalized with a Schur
!     factorization, to be on the safe side of numerical stability.

! ... From LAPACK:
!     ZGEES computes for an N-by-N complex nonsymmetric matrix Y, the eigen-
!     values, the Schur form T, and, optionally, the matrix of Schur vectors Z.
!     This gives the Schur factorization Y = Z*T*(Z**H).
!     Optionally, it also orders the eigenvalues on the diagonal of the Schur
!     form so that selected eigenvalues are at the top left.  The leading columns
!     of Z then form an orthonormal basis for the invariant subspace correspond-
!     ing to the selected eigenvalues.
 
! ... A complex matrix is in Schur form if it is upper triangular.

      ALLOCATE( cs(dimwann,dimwann,nkpts), STAT=ierr )
         IF( ierr /=0 ) CALL errore(' wannier ', ' allocating cs ', dimwann**2 * nkpts)

      nwork = dimwann * 10
      ALLOCATE( cwschur1(dimwann), cwschur2( nwork ), STAT=ierr )
         IF( ierr /=0 ) &
         CALL errore(' wannier ', ' allocating cwschur1 cwschur2 ', dimwann+nwork)
      ALLOCATE( cz(dimwann, dimwann), STAT=ierr )
         IF( ierr /=0 ) CALL errore(' wannier ', ' allocating cz ', dimwann**2)
      ALLOCATE( cwschur3(dimwann), STAT=ierr )
         IF( ierr /=0 ) CALL errore(' wannier ', ' allocating cwschur3 ', dimwann)
      ALLOCATE( cwschur4(dimwann), STAT=ierr )
         IF( ierr /=0 ) CALL errore(' wannier ', ' allocating cwschur4 ', dimwann)

      !
      ! .. loop over kpts
      !
      DO ik = 1, nkpts
        CALL zmat_mul( cs(:,:,ik), ca(:,:,ik), 'N', ca(:,:,ik), 'C', & 
                       dimwann, dimwann, dimwann )

        CALL zgees( 'V', 'N', lselect, dimwann, cs(1,1,ik), dimwann, nsdim,            &
             cwschur1, cz(1,1), dimwann, cwschur2, nwork, cwschur3, cwschur4, info )

        cs(:,:,ik)=CZERO

        DO m=1,dimwann
           cfact = ONE/SQRT( REAL( cwschur1(m) ) )
           DO j=1,dimwann
           DO i=1,dimwann
              cs(i,j,ik) = cs(i,j,ik) + cz(i,m) * cfact * CONJG( cz(j,m) )
           ENDDO
           ENDDO
        ENDDO

        CALL zmat_mul( cu(:,:,ik), cs(:,:,ik), 'N', ca(:,:,ik), 'N', & 
                       dimwann, dimwann, dimwann )

! ...  Unitariety is checked
        IF ( .NOT. zmat_unitary( cu(:,:,ik), SIDE='both', TOLL=unitary_thr )  ) &
             WRITE (stdout, " (/,2x, 'WARNING: U matrix NOT unitary at ikpt = ',i4)") ik

!
! ... Phases
!

        IF (iphase /= 1) THEN

          IF ( iphase == 2 ) THEN
! ...         check what happens if the heuristic phase is taken away
              WRITE (stdout,*) 'NB: phase is taken away'
              DO j = 1, dimwann
              DO i = 1, dimwann
                 cu(i,j,ik) = CZERO
                 IF ( i == j ) cu(i,j,ik) = CONE
              ENDDO
              ENDDO
          ELSE
! ...         check what happens if a random phase is given

              epsilon = ONE
              WRITE(stdout, fmt=" (2x, 'NB: RANDOM phase is given' )")
              DO m = 1, dimwann
              DO n = m, dimwann
                  rre = rndm() * 10*ONE - 5*ONE
                  rri = rndm() * 10*ONE - 5*ONE
                  cu(m,n,ik) = epsilon * CMPLX(rre,rri)
                  cu(n,m,ik) = -CONJG( cu(m,n,ik) )
                  IF ( m == n ) cu(n,m,ik) = CMPLX( ZERO , AIMAG( cu(m,n,ik) ) )
              ENDDO
              ENDDO

              CALL zgees( 'V', 'N', lselect, dimwann, cu(1,1,ik), dimwann, nsdim,   &
                 cwschur1, cz(1,1), dimwann, cwschur2, SIZE(cwschur2), cwschur3,    &
                 cwschur4, info )

              cu( :, :, ik ) = CZERO
              DO m = 1, dimwann
                 cfact = EXP( cwschur1(m) )
                 DO j = 1, dimwann
                 DO i = 1, dimwann
                     cu(i,j,ik) = cu(i,j,ik) + cz(i,m) * cfact * CONJG( cz(j,m) )
                 ENDDO
                 ENDDO
              ENDDO

          ENDIF

        ENDIF   ! phases

        IF ( verbosity == 'high' ) THEN
            WRITE (stdout, fmt=" (/,2x,' Matrix U after zgees, k-point',i3,/)") ik
            DO i = 1, dimwann
                WRITE(stdout, fmt="(4x,2f9.5,2x,2f9.5,2x,2f9.5,2x,2f9.5) ")  &
                                   ( cu(i,j,ik), j=1,dimwann )
            ENDDO
        ENDIF

      ENDDO  ! k point loop

      ALLOCATE( cmtmp(dimwann,dimwann), STAT=ierr )
         IF( ierr /=0 ) CALL errore(' wannier ', ' allocating cu ', dimwann**2 )

! ... So now we have the U's that rotate the wavefunctions at each k-point.
!     the matrix elements M_ij have also to be updated 

      DO ik = 1, nkpts

        DO nn = 1, nntot(ik)
          ik2 = nnlist(ik,nn)
          DO i = 1, dimwann
          DO j = 1 ,dimwann
              cmtmp(i,j) = CZERO
              DO m = 1, dimwann
              DO n = 1, dimwann
                 cmtmp(i,j) = cmtmp(i,j) + CONJG(cu(m,i,ik)) * &
                                                 cu(n,j,ik2) * cm(m,n,nn,ik)
              ENDDO
              ENDDO
          ENDDO
          ENDDO
          cm(:,:,nn,ik) = cmtmp(:,:)
        ENDDO

      ENDDO

! ... Singular value decomposition

      nwork = dimwann * 10
      ALLOCATE( singvd(dimwann), STAT=ierr )
         IF( ierr /=0 ) CALL errore(' wannier ', ' allocating singvd ', dimwann )
      ALLOCATE( cv1(dimwann,dimwann), STAT=ierr )
         IF( ierr /=0 ) CALL errore(' wannier ', ' allocating cv1 ', dimwann**2 )
      ALLOCATE( cv2(dimwann,dimwann), STAT=ierr )
         IF( ierr /=0 ) CALL errore(' wannier ', ' allocating cv2 ', dimwann**2 )
      ALLOCATE( cv3(dimwann,dimwann), STAT=ierr )
         IF( ierr /=0 ) CALL errore(' wannier ', ' allocating cv3 ', dimwann**2 )
      ALLOCATE( cw1( nwork ), STAT=ierr )
         IF( ierr /=0 ) CALL errore(' wannier ', ' allocating cw1 ', nwork )
      ALLOCATE( cw2( nwork ), STAT=ierr )
         IF( ierr /=0 ) CALL errore(' wannier ', ' allocating cw2 ', nwork )
 
      omt1 = ZERO
      omt2 = ZERO
      omt3 = ZERO

      DO ik = 1, nkpts
        omiloc = ZERO
        DO nn = 1, nntot(ik)
          cmtmp(:,:) = cm(:,:,nn,ik)
          CALL zgesvd( 'A', 'A', dimwann, dimwann, cmtmp, dimwann,      &
               singvd, cv1, dimwann, cv2, dimwann, cw1, nwork, cw2, info )
          IF ( info /= 0 ) &
              CALL errore(' wannier ', ' Singular value decomposition failed 2 ', info )

          DO nb = 1, dimwann
              omiloc = omiloc + wb(ik,nn) * ( ONE - singvd(nb)**2 )
          ENDDO
          DO nb = 1, dimwann
              omt1 = omt1 + wb(ik,nn) * ( ONE - singvd(nb)**2 )
              omt2 = omt2 - wb(ik,nn) * ( TWO * LOG( singvd(nb) ) )
              omt3 = omt3 + wb(ik,nn) * ( ACOS( singvd(nb) )**2 )
          ENDDO
        ENDDO
      ENDDO

      omt1 = omt1/DBLE(nkpts)
      omt2 = omt2/DBLE(nkpts)
      omt3 = omt3/DBLE(nkpts)

! ... Recalculate the average positions of the Wanns.

      CALL omega( dimwann, nkpts, nkpts, nntot, nnx, nnlist, bk, wb, cm,  &
           csheet, sheet, rave, r2ave, rave2, func_om1, func_om2, func_om3, Omega_tot , &
           rtot, r2tot, Omega_I, Omega_D, Omega_OD, Omega_V)

      func_del1 = func_om1 - func_old1
      func_del2 = func_om2 - func_old2
      func_del3 = func_om3 - func_old3
      func_old1 = func_om1
      func_old2 = func_om2
      func_old3 = func_om3


!! ... Find the guiding centers, and set up the 'best' Riemannian sheets for 
!!     the complex logarithms
!
!      irguide = 0
!!     CALL phases( dimwann, nkpts, nkpts, nnx, nnhx, nntot, nnh, neigh,        &
!!          bk, bka, cm, csheet, sheet, rguide, irguide )
!      irguide = 1
!
!! ... Recalculate the average positions of the Wanns.

      CALL omega( dimwann, nkpts, nkpts, nntot, nnx, nnlist, bk, wb, cm,        &
           csheet, sheet, rave, r2ave, rave2, func_om1, func_om2, func_om3, Omega_tot,     &
           rtot, r2tot , Omega_I, Omega_D, Omega_OD, Omega_V)

      func0 = func_om1 + func_om2 + func_om3
      func_del1 = func_om1 - func_old1
      func_del2 = func_om2 - func_old2
      func_del3 = func_om3 - func_old3
      func_old1 = func_om1
      func_old2 = func_om2
      func_old3 = func_om3

! ... Singular value decomposition

      omt1 = ZERO
      omt2 = ZERO
      omt3 = ZERO

      DO ik = 1, nkpts
        DO nn = 1, nntot(ik)

          cmtmp(:,:) = cm(:,:,nn,ik)
          CALL zgesvd( 'A', 'A', dimwann, dimwann, cmtmp, dimwann,      &
               singvd, cv1, dimwann, cv2, dimwann, cw1, 10*dimwann, cw2, info )

          IF ( info /= 0 ) &
          CALL errore(' wannier ', ' Singular value decomposition failed 3 ', info )

          CALL zmat_mul( cv3, cv1, 'N', cv2, 'N', dimwann, dimwann, dimwann )

          DO nb = 1, dimwann
              omt1 = omt1 + wb(ik,nn) * ( ONE - singvd(nb)**2 )
              omt2 = omt2 - wb(ik,nn) * ( TWO * LOG( singvd(nb) ) )
              omt3 = omt3 + wb(ik,nn) * ( ACOS( singvd(nb) )**2)
          ENDDO

        ENDDO     ! loop nn
      ENDDO       ! loop ik


      omt1 = omt1/DBLE(nkpts)
      omt2 = omt2/DBLE(nkpts)
      omt3 = omt3/DBLE(nkpts)

! ... Now that the consistent phase factors have been chosen
!     the wannier functions (for the R=0 cell) are calculated


      lrguide = .FALSE.
      nrguide = 10

      lcg = .true.
      if ( ncg < 1 ) lcg = .FALSE.

! ... But first it starts the iterative cycle to solve "Poisson" phase
    
      IF ( ncg == 0 ) ncg = ncg + 1
      ncgfix = ncg

      ALLOCATE( cu0(dimwann,dimwann,nkpts), STAT=ierr )
         IF( ierr /=0 ) CALL errore(' wannier ', ' allocating cu0 ', dimwann*2 * nkpts )
      ALLOCATE( cm0(dimwann,dimwann,nnx,nkpts), STAT=ierr )
         IF( ierr /=0 ) CALL errore(' wannier ', ' allocating cm0 ', dimwann**2*nkpts*nnx )
      ALLOCATE( cdodq(dimwann,dimwann,nkpts), STAT=ierr )
         IF( ierr /=0 ) CALL errore(' wannier ', ' allocating cdodq ', dimwann*2 * nkpts )
      ALLOCATE( cdqkeep(dimwann,dimwann,nkpts), STAT=ierr )
         IF( ierr /=0 ) CALL errore(' wannier ', ' allocating cdqkeep ', dimwann*2 * nkpts )
      ALLOCATE( cdodq1(dimwann,dimwann,nkpts), STAT=ierr )
         IF( ierr /=0 ) CALL errore(' wannier ', ' allocating cdodq1 ', dimwann*2 * nkpts )
      ALLOCATE( cdodq2(dimwann,dimwann,nkpts), STAT=ierr )
         IF( ierr /=0 ) CALL errore(' wannier ', ' allocating cdodq2 ', dimwann*2 * nkpts )
      ALLOCATE( cdodq3(dimwann,dimwann,nkpts), STAT=ierr )
         IF( ierr /=0 ) CALL errore(' wannier ', ' allocating cdodq3 ', dimwann*2 * nkpts )
      ALLOCATE( cdq(dimwann,dimwann,nkpts), STAT=ierr )
         IF( ierr /=0 ) CALL errore(' wannier ', ' allocating cdq ', dimwann*2 * nkpts )

      cdqkeep = CZERO
      cdodq1  = CZERO
      cdodq2  = CZERO
      cdodq3  = CZERO
      cdq     = CZERO
      cdodq   = CZERO

      CALL timing('init',OPR='stop')
      CALL timing('iterations',OPR='start')

!
!
!  ... Here start the iterative loop
!
!
      WRITE( stdout, "(2/,2x,70('='))" ) 
      WRITE( stdout, "(2x,'=',21x,'Starting iteration loop',24x,'=')" )
      WRITE( stdout, "(2x,70('='),/)" ) 


!
! ... Main ITERATION loop
!
      iteration_loop : DO ncount = 1, maxiter0_wan + maxiter1_wan

        IF ( ncount <= maxiter0_wan ) THEN
            ncg   = 1
            alpha = alpha0_wan
        ELSE
            ncg   = ncgfix
            alpha = alpha1_wan
        ENDIF


! ...   Store cu and cm

        cu0 = cu
        cm0 = cm

        IF ( lrguide ) THEN
! XXXX  nnh = nntot(1)/2, vedi Marzari... to be fixed here
          IF ( ( ( ncount / 10 ) * 10 == ncount ) .and. ( ncount >= nrguide ) )       &
            CALL phases( dimwann, nkpts, nkpts, nnx, nnhx, nntot, nnh, neigh,       &
                 bk, bka, cm, csheet, sheet, rguide, irguide )
        ENDIF

        CALL domega( dimwann, nkpts, nkpts, nntot, nnx, nnlist, bk, wb,              &
             cm, csheet, sheet, rave, r2ave, cdodq1, cdodq2, cdodq3, cdodq)

        gcnorm1 = ZERO
        DO ik = 1, nkpts
            DO n = 1, dimwann
            DO m= 1, dimwann
                gcnorm1 = gcnorm1 + REAL( cdodq(m,n,ik) * CONJG( cdodq(m,n,ik) ) )
            ENDDO
            ENDDO
        ENDDO

        IF ( MOD( (ncount-1), ncg ) == 0 ) THEN
            cdq = cdodq
        ELSE
            gcfac = gcnorm1/gcnorm0
            cdq = cdodq + gcfac * cdqkeep
        ENDIF

        gcnorm0 = gcnorm1
        cdqkeep = cdq

        doda0 = ZERO
        DO ik = 1, nkpts
            DO m = 1, dimwann
            DO n = 1, dimwann
                doda0 = doda0 + REAL( cdq(m,n,ik) * cdodq(n,m,ik) )
            ENDDO
            ENDDO
        ENDDO
        doda0 = doda0 / wbtot / FOUR


! ...   The cg step is calculated
        cdq = alpha / wbtot / FOUR * cdq

        cfunc_exp1 = CZERO
        cfunc_exp2 = CZERO
        cfunc_exp3 = CZERO
        DO ik = 1, nkpts
            DO i = 1, dimwann
            DO j = 1, dimwann
              cfunc_exp1 = cfunc_exp1 + cdodq1(i,j,ik) * cdq(j,i,ik)
              cfunc_exp2 = cfunc_exp2 + cdodq2(i,j,ik) * cdq(j,i,ik)
              cfunc_exp3 = cfunc_exp3 + cdodq3(i,j,ik) * cdq(j,i,ik)
            ENDDO
            ENDDO
        ENDDO

        DO ik = 1, nkpts

            CALL zgees( 'V', 'N', lselect, dimwann, cdq(1,1,ik), dimwann, nsdim,     &
                 cwschur1, cz(1,1), dimwann, cwschur2, SIZE( cwschur2 ), cwschur3,    &
                 cwschur4, info )

            IF ( info /= 0 ) CALL errore ('wannier', 'wrong schur procedure', info)

            cdq( :, :, ik ) = CZERO
            DO m = 1, dimwann
                cfact = EXP( cwschur1(m) )
                DO j = 1, dimwann
                DO i = 1, dimwann
                     cdq(i,j,ik) = cdq(i,j,ik) + cz(i,m) * cfact * CONJG( cz(j,m) )
                ENDDO
                ENDDO
            ENDDO

        ENDDO


! ...   The expected change in the functional is calculated

        cfunc_exp1 = CZERO
        cfunc_exp2 = CZERO
        cfunc_exp3 = CZERO

        DO ik = 1, nkpts
            DO i = 1, dimwann
            DO j = 1, dimwann
                cfunc_exp1 = cfunc_exp1 + cdodq1(i,j,ik) * cdq(j,i,ik)
                cfunc_exp2 = cfunc_exp2 + cdodq2(i,j,ik) * cdq(j,i,ik)
                cfunc_exp3 = cfunc_exp3 + cdodq3(i,j,ik) * cdq(j,i,ik)
            ENDDO
            ENDDO
        ENDDO
        cfunc_exp = cfunc_exp1 + cfunc_exp2 + cfunc_exp3


! ...   The orbitals are rotated 

        DO ik = 1, nkpts
            CALL zmat_mul( cmtmp(:,:), cu(:,:,ik), 'N', cdq(:,:,ik), 'N', &
                           dimwann, dimwann, dimwann )
            cu(:,:,ik) = cmtmp(:,:)
        ENDDO


! ...   And the M_ij are updated

        DO ik = 1, nkpts
            DO nn = 1, nntot(ik)
                ik2 = nnlist(ik,nn)
                DO i = 1, dimwann
                DO j = 1, dimwann
                    cmtmp(i,j) = CZERO
                    DO m = 1, dimwann
                    DO n = 1, dimwann
                        cmtmp(i,j) = cmtmp(i,j) + CONJG( cdq(m,i,ik) ) * &
                                                          cdq(n,j,ik2) * cm(m,n,nn,ik)
                    ENDDO
                    ENDDO
                ENDDO
                ENDDO
                cm(:,:,nn,ik) = cmtmp(:,:)
            ENDDO
        ENDDO


! ...   And the functional is recalculated

        CALL omega( dimwann, nkpts, nkpts, nntot, nnx, nnlist, bk, wb, cm,       &
             csheet, sheet, rave, r2ave, rave2, func_om1, func_om2, func_om3, Omega_tot, &
             rtot, r2tot, Omega_I, Omega_D, Omega_OD, Omega_V)

        IF ( ncount <= maxiter0_wan ) THEN
            IF ( ( (ncount/nprint) * nprint +1) == ncount )  THEN
                WRITE( stdout," (/,2x,'Iteration = ',i5) ") ncount
                WRITE(stdout, " (2x, 'Wannier centers (Bohr) and Spreads Omega (Bohr^2)')")
                DO nwann = 1, dimwann
                    WRITE( stdout, " ( 4x, 'Center ', i3, 1x, '= (',f12.6,',',f12.6,',',&
                               &  f12.6, ' )  Omega = ', f13.6 )" )  &
                               nwann,( rave(i,nwann), i=1,3 ), r2ave(nwann) - rave2(nwann)
                ENDDO
                WRITE( stdout, " (/, 2x, '! Center Sum', 1x, '= (',  & 
                               &  f12.6,',',f12.6,',',f12.6,' )  Omega = ', f13.6 )" )  &
                               (rtot(i),i=1,3), r2tot
             ENDIF
        ENDIF

        funca = func_om1 + func_om2 + func_om3
        func_del1 = func_om1 - func_old1
        func_del2 = func_om2 - func_old2
        func_del3 = func_om3 - func_old3
        func_del = func_del1 + func_del2 + func_del3

! ...   If lcg is false, or still in the first maxiter0_wan iterations, 
!       it skips the optimal alpha paraphernalia 

        IF ( ( lcg ) .and. ( ncount > maxiter0_wan ) ) THEN

          eqc = func0
          eqb = doda0
          eqa = ( funca - func0 - eqb * alpha ) / alpha

          IF ( ABS(eqa) > 1.0e-8 ) THEN
            alphamin = -eqb / TWO / eqa
          ELSE
            alphamin = alpha
          ENDIF
          IF ( alphamin < ZERO ) alphamin = alpha * TWO
          IF ( alphamin > THREE * alpha ) alphamin = THREE * alpha
          falphamin = eqa * alphamin**2 + eqb * alphamin + eqc


! ...     Restore cu and cm

          cu = cu0
          cm = cm0

! ...     Take now optimal parabolic step

           cdq = alphamin / wbtot / FOUR * cdqkeep


! ...     The expected change in the functional is calculated

          cfunc_exp1 = CZERO
          cfunc_exp2 = CZERO
          cfunc_exp3 = CZERO
          DO ik = 1, nkpts
              DO i = 1, dimwann
              DO j = 1, dimwann
                  cfunc_exp1 = cfunc_exp1 + cdodq1(i,j,ik) * cdq(j,i,ik)
                  cfunc_exp2 = cfunc_exp2 + cdodq2(i,j,ik) * cdq(j,i,ik)
                  cfunc_exp3 = cfunc_exp3 + cdodq3(i,j,ik) * cdq(j,i,ik)
              ENDDO
              ENDDO
          ENDDO


          DO ik = 1, nkpts

              CALL zgees( 'V', 'N', lselect, dimwann, cdq(1,1,ik), dimwann, nsdim,       &
                   cwschur1, cz(1,1), dimwann, cwschur2, SIZE( cwschur2 ), cwschur3,      &
                   cwschur4, info )

              IF ( info /= 0 ) CALL errore('wannier', 'wrong Schur procedure (II)', info)

              cdq(:,:,ik) = CZERO
              DO m = 1, dimwann
                  cfact =  EXP( cwschur1(m) ) 
                  DO j = 1, dimwann
                  DO i = 1, dimwann
                      cdq(i,j,ik) = cdq(i,j,ik) + cz(i,m) * cfact * CONJG( cz(j,m) )
                  ENDDO
                  ENDDO
              ENDDO

          ENDDO


! ...     The expected change in the functional is calculated

          cfunc_exp1 = CZERO
          cfunc_exp2 = CZERO
          cfunc_exp3 = CZERO
          DO ik = 1, nkpts
            DO i = 1, dimwann
              DO j = 1, dimwann
                cfunc_exp1 = cfunc_exp1 + cdodq1(i,j,ik) * cdq(j,i,ik)
                cfunc_exp2 = cfunc_exp2 + cdodq2(i,j,ik) * cdq(j,i,ik)
                cfunc_exp3 = cfunc_exp3 + cdodq3(i,j,ik) * cdq(j,i,ik)
              END DO
            END DO
          END DO
          cfunc_exp = cfunc_exp1 + cfunc_exp2 + cfunc_exp3


! ...     The orbitals are rotated 

          DO ik = 1, nkpts
              CALL zmat_mul( cmtmp(:,:), cu(:,:,ik), 'N', cdq(:,:,ik), 'N', &
                             dimwann, dimwann, dimwann )
              cu(:,:,ik) = cmtmp(:,:)
          END DO

! ...     And the M_ij are updated

          DO ik = 1, nkpts
             DO nn = 1, nntot(ik)
                ik2 = nnlist(ik,nn)
                DO i = 1, dimwann
                DO j = 1, dimwann
                    cmtmp(i,j) = CZERO
                    DO m = 1, dimwann
                    DO n = 1, dimwann
                        cmtmp(i,j) = cmtmp(i,j) + CONJG( cdq(m,i,ik) ) * &
                                                         cdq(n,j,ik2) * cm(m,n,nn,ik)
                    ENDDO
                    ENDDO
                ENDDO
                ENDDO
                cm(:,:,nn,ik) = cmtmp(:,:)
             ENDDO
          ENDDO

! ...     And the functional is recalculated
           
          CALL omega( dimwann, nkpts, nkpts, nntot, nnx, nnlist, bk, wb, cm,       &
               csheet, sheet, rave, r2ave, rave2, func_om1, func_om2, func_om3, Omega_tot, &
               rtot, r2tot, Omega_I , Omega_D, Omega_OD, Omega_V)

          IF ( ( (ncount/nprint) * nprint +1) == ncount ) THEN
              WRITE( stdout, " (/,2x,'Iteration = ',i5) ") ncount
              WRITE(stdout,  " (  2x, 'Wannier centers (Bohr) and Spreads Omega (Bohr^2)')")
              DO nwann = 1, dimwann
                  WRITE( stdout, " ( 4x, 'Center ', i3, 1x, '= (',f12.6,',',f12.6,',', &
                         &  f12.6, ' )  Omega = ', f13.6 )" )  &
                         nwann,( rave(i,nwann), i=1,3 ), r2ave(nwann) - rave2(nwann)
              ENDDO
              WRITE( stdout, " ( /,2x, '! Center Sum',    &
                         & 1x, '= (',f12.6,',',f12.6,',',f12.6,' )  Omega = ', f13.6 )" ) &
                         (rtot(i),i=1,3), r2tot
          ENDIF

          funca = func_om1 + func_om2 + func_om3
          func_del1 = func_om1 - func_old1
          func_del2 = func_om2 - func_old2
          func_del3 = func_om3 - func_old3
          func_del = func_del1 + func_del2 + func_del3

! ...   end of the lcg skip of the optimal alpha paraphernalia 
        END IF


        func_old1 = func_om1
        func_old2 = func_om2
        func_old3 = func_om3

        func0 = funca
     
! ...   Check convergence

        IF ( ABS( func_del ) < wannier_thr ) THEN
          !
          ! ... ordering wannier centers
          !
          CALL ordering(dimwann,nkpts,rave,rave2,r2ave,cu, ordering_mode)

          WRITE( stdout, "(/,2x,70('='))" ) 
          WRITE( stdout, "(2x,'=',24x,'Convergence Achieved',24x,'=')" )
          WRITE( stdout, "(2x,70('='),/)" ) 

          WRITE( stdout, "(/,2x,'Wannier function ordering : ',a,/)") TRIM(ordering_mode)
          WRITE( stdout, " (2x, 'Final Wannier centers (Bohr) and Spreads Omega (Bohr^2)')")
          DO nwann = 1, dimwann
            WRITE( stdout, " ( 4x, 'Center ', i3, 1x, '= (',f12.6,',',f12.6,',', &
               & f12.6,' )  Omega = ', f13.6 )" )  &
               nwann,( rave(i,nwann), i=1,3 ), r2ave(nwann) - rave2(nwann)
          END DO
          WRITE( stdout, " ( /, 2x, '! Center Sum',    &
               & 1x, '= (',f12.6,',',f12.6,',',f12.6,' )  Omega = ', f13.6 )" )     &
              (rtot(i),i=1,3), r2tot

          WRITE( stdout, "(/,2x, 'Spread Operator decomposition (Bohr^2): ')")
          WRITE( stdout, "(  4x,'OmegaI    =   ', f12.6 ) " ) Omega_I
          WRITE( stdout, "(  4x,'OmegaD    =   ', f12.6 ) " ) Omega_D
          WRITE( stdout, "(  4x,'OmegaOD   =   ', f12.6 ) " ) Omega_OD
          WRITE( stdout, "(  4x,'Omega Tot =   ', f12.6 ) " ) Omega_tot

          WRITE (stdout, "(/, 2x,'Omega variation (Bohr^2):')")
!          WRITE (stdout, "(  4x,'Delta Omega 1   = ',0pe16.8)") func_del1
!          WRITE (stdout, "(  4x,'Delta Omega 2   = ',0pe16.8)") func_del2
!          WRITE (stdout, "(  4x,'Delta Omega 3   = ',0pe16.8)") func_del3
!          WRITE (stdout, "(  4x,'Delta Omega Tot = ',0pe16.8)") func_del
!          WRITE (stdout, "(/,2x,'Derivative = ', 2e12.4) ") funca-func0,doda0*alpha

          WRITE (stdout, "(  4x,'Delta Omega Tot = ',f15.9)") func_del
          WRITE( stdout, "(/,2x,70('='))" ) 

          GO TO 8100
        END IF  

!
! ...  End of the ITERATION loop
!
      ENDDO iteration_loop


      !
      ! ... ordering wannier centers
      !
      CALL ordering(dimwann,nkpts,rave,rave2,r2ave,cu,ordering_mode)

      WRITE (stdout, "(/,2x,70('='))")
      WRITE( stdout, "(2x,'=',18x,'Max Number of iteration reached',19x,'=')" ) 
      WRITE (stdout, "(2x,70('='),/)")

      WRITE( stdout, "(2x,'Wannier function ordering : ',a,/)") TRIM(ordering_mode)
      WRITE(stdout,  " (2x, 'Final Wannier centers (Bohr) and Spreads Omega (Bohr^2)')")
      DO nwann = 1, dimwann
        WRITE( stdout, " ( 4x, 'Center ', i3, 1x, '= (',f12.6,',',f12.6,',',f12.6,  &
           & ' )  Omega = ', f13.6 )" )  nwann,( rave(i,nwann), i=1,3 ), &
                                         r2ave(nwann) - rave2(nwann)
      END DO
      WRITE( stdout, " (/,2x, '! Center Sum',    &
           & 1x, '= (',f12.6,',',f12.6,',',f12.6,' )  Omega = ', f13.6 )" )     &
             (rtot(i),i=1,3), r2tot

      WRITE( stdout, "(/,2x, 'Spread Operator decomposition (Bohr^2): ')")
      WRITE( stdout, "(  4x,'OmegaI    =   ', f12.6 ) " ) Omega_I
      WRITE( stdout, "(  4x,'OmegaD    =   ', f12.6 ) " ) Omega_D
      WRITE( stdout, "(  4x,'OmegaOD   =   ', f12.6 ) " ) Omega_OD
      WRITE( stdout, "(  4x,'Omega Tot =   ', f12.6 ) " ) Omega_tot

      WRITE (stdout, "(/,2x,'Omega variation (Bohr^2):')")
!      WRITE (stdout, "(  4x,'Delta Omega 1   = ',0pe16.8)") func_del1
!      WRITE (stdout, "(  4x,'Delta Omega 2   = ',0pe16.8)") func_del2
!      WRITE (stdout, "(  4x,'Delta Omega 3   = ',0pe16.8)") func_del3
!      WRITE (stdout, "(  4x,'Delta Omega Tot = ',0pe16.8)") func_del
!      WRITE (stdout, "(/,2x,'Derivative = ', 2e12.4) ") funca-func0,doda0*alpha

      WRITE (stdout, "(  4x,'Delta Omega Tot = ',f15.9)") func_del
      WRITE (stdout, "(/,2x,70('='))")


! ... End of the minimization loop

8100  CONTINUE

      CALL timing('iterations',OPR='stop')
      CALL timing('write',OPR='start')

!
! ... Unitariery of U matrix is checked
      DO ik = 1, nkpts
          IF (  .NOT. zmat_unitary( cu(:,:,ik), SIDE='both', TOLL=unitary_thr )  )  &
               WRITE (stdout, " (/,2x, 'WARNING: U matrix NOT unitary at ikpt = ',i4)")ik
      ENDDO

! ... Singular value decomposition

      omt1 = ZERO
      omt2 = ZERO
      omt3 = ZERO

      DO ik = 1, nkpts
        DO nn = 1, nntot(ik)

          cmtmp(:,:) = cm(:,:,nn,ik)
          CALL zgesvd( 'A', 'A', dimwann, dimwann, cmtmp, dimwann,      &
                       singvd, cv1, dimwann, cv2, dimwann, cw1, 10*dimwann, cw2, info )
          IF ( info /= 0 ) &
               CALL errore('wannier', 'Singular value decomposition failed 4', info)

          DO nb = 1, dimwann
               omt1 = omt1 + wb(ik,nn) * ( ONE - singvd(nb)**2 )
               omt2 = omt2 - wb(ik,nn) * ( TWO * LOG( singvd(nb) ) )
               omt3 = omt3 + wb(ik,nn) * ( ACOS( singvd(nb) )**2 )
          END DO

        END DO
      END DO

      omt1 = omt1/DBLE(nkpts)
      omt2 = omt2/DBLE(nkpts)
      omt3 = omt3/DBLE(nkpts)

 
! ... Write the final unitary transformations and all other data referring
!     to the Wannier localization procedure to a file
 
      CALL ioname('wannier',filename)
      CALL file_open(wan_unit,TRIM(filename),PATH="/",ACTION="write",FORM="formatted")
           CALL localization_write(wan_unit,"WANNIER_LOCALIZATION")
      CALL file_close(wan_unit,PATH="/",ACTION="write")

      CALL ioname('wannier',filename,LPATH=.FALSE.)
      WRITE( stdout,"(/,'  Wannier transformation data written on file: ',a)") TRIM(filename)

!
! ... Finalize timing
      CALL timing('write',OPR='stop')
      CALL timing('wannier',OPR='stop')
      CALL timing_overview(stdout,LIST=global_list,MAIN_NAME='wannier')
     
!
! ... Deallocate local arrays

      DEALLOCATE( cwschur1, cwschur2, STAT=ierr )
           IF( ierr /=0 )&
           CALL errore(' wannier ', ' deallocating cwschur1 cwschur1 ', ABS(ierr) )
      DEALLOCATE( cz, STAT=ierr )
           IF( ierr /=0 ) CALL errore(' wannier ', ' deallocating cz ', ABS(ierr) )
      DEALLOCATE( cwschur3, STAT=ierr )
           IF( ierr /=0 ) CALL errore(' wannier ', ' deallocating cwschur3 ', ABS(ierr) )
      DEALLOCATE( cwschur4, STAT=ierr )
           IF( ierr /=0 ) CALL errore(' wannier ', ' deallocating cwschur4 ', ABS(ierr) )
      DEALLOCATE( cw1, STAT=ierr )
           IF( ierr /=0 ) CALL errore(' wannier ', ' deallocating cw1 ', ABS(ierr) )
      DEALLOCATE( cw2, STAT=ierr )
           IF( ierr /=0 ) CALL errore(' wannier ', ' deallocating cw2 ', ABS(ierr) )

      DEALLOCATE( csheet, STAT=ierr )
           IF( ierr /=0 ) CALL errore(' wannier ', ' deallocating csheet ', ABS(ierr) )
      DEALLOCATE( sheet, STAT=ierr )
           IF( ierr /=0 ) CALL errore(' wannier ', ' deallocating sheet ', ABS(ierr) )
      DEALLOCATE( cs, STAT=ierr )
           IF( ierr /=0 ) CALL errore(' wannier ', ' deallocating cs ', ABS(ierr) )
      DEALLOCATE( cmtmp, STAT=ierr )
           IF( ierr /=0 ) CALL errore(' wannier ', ' deallocating cmtmp ', ABS(ierr) )
      DEALLOCATE( singvd, STAT=ierr )
           IF( ierr /=0 ) CALL errore(' wannier ', ' deallocating singvd ', ABS(ierr) )
      DEALLOCATE( cv1, STAT=ierr )
           IF( ierr /=0 ) CALL errore(' wannier ', ' deallocating cv1 ', ABS(ierr) )
      DEALLOCATE( cv2, STAT=ierr )
           IF( ierr /=0 ) CALL errore(' wannier ', ' deallocating cv2 ', ABS(ierr) )
      DEALLOCATE( cv3, STAT=ierr )
           IF( ierr /=0 ) CALL errore(' wannier ', ' deallocating cv3 ', ABS(ierr) )
      DEALLOCATE( cu0, STAT=ierr )
           IF( ierr /=0 ) CALL errore(' wannier ', ' deallocating cu0 ', ABS(ierr) )
      DEALLOCATE( cm0, STAT=ierr )
           IF( ierr /=0 ) CALL errore(' wannier ', ' deallocating cm0 ', ABS(ierr) )
      DEALLOCATE( cdodq, STAT=ierr )
           IF( ierr /=0 ) CALL errore(' wannier ', ' deallocating cdodq ', ABS(ierr) )
      DEALLOCATE( cdqkeep, STAT=ierr )
           IF( ierr /=0 ) CALL errore(' wannier ', ' deallocating cdqkeep ', ABS(ierr) )
      DEALLOCATE( cdodq1, STAT=ierr )
           IF( ierr /=0 ) CALL errore(' wannier ', ' deallocating cdodq1 ', ABS(ierr) )
      DEALLOCATE( cdodq2, STAT=ierr )
           IF( ierr /=0 ) CALL errore(' wannier ', ' deallocating cdodq2 ', ABS(ierr) )
      DEALLOCATE( cdodq3, STAT=ierr )
           IF( ierr /=0 ) CALL errore(' wannier ', ' deallocating cdodq3 ', ABS(ierr) )
      DEALLOCATE( cdq, STAT=ierr )
           IF( ierr /=0 ) CALL errore(' wannier ', ' deallocating cdq ', ABS(ierr) )

      CALL cleanup

      STOP '*** THE END *** (wannier.f90)'
      END PROGRAM wannier
