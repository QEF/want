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
      USE constants, ONLY: ryd => ry, har => au, bohr => bohr_radius_angs, &
                           CZERO, CONE, CI, ZERO, ONE, TWO, THREE, FOUR
      USE parameters, ONLY : nstrx
      USE input_module
      USE timing_module, ONLY : timing, timing_deallocate, timing_overview, global_list
      USE io_module, ONLY : stdout, dft_unit, wan_unit, ioname
      USE files_module, ONLY : file_open, file_close
      USE startup_module, ONLY : startup
      USE version_module, ONLY : version_number
      USE converters_module, ONLY : cart2cry
      USE util_module, ONLY: zmat_mul, zmat_unitary

      USE lattice, ONLY: avec, dirc, recc, alat, lattice_init
      USE ions, ONLY: rat, ntype, natom, nameat, poscart, poscart_set
      USE kpoints_module, ONLY: nk, s, nkpts, vkpt, &
                          nnmx => mxdnn, nnmxh => mxdnnh, &
                          nntot, nnlist, nncell, neigh, bk, wb, dnn, bka, wbtot, &
                          kpoints_init, kpoints_deallocate
 
      USE windows_module, ONLY : dimwin, mxdbnd, &
                          windows_allocate, windows_deallocate
      USE subspace_module, ONLY : subspace_allocate, subspace_deallocate
      USE overlap_module,  ONLY : ca, cm, overlap_deallocate
      USE localization_module, ONLY : cu, rave, rave2, r2ave, &
                       Omega_I, Omega_OD, Omega_D, Omega_V, Omega_tot, &
                       localization_allocate, localization_deallocate, localization_write

!
! ... 
!
      IMPLICIT NONE

      INTEGER,      PARAMETER :: nprint = 10
      CHARACTER( LEN=6 )      :: verbosity = 'none'    ! none, low, medium, high
      CHARACTER( LEN=nstrx )  :: filename

      ! external functions
      REAL(dbl) :: ranf
      EXTERNAL  :: ranf
      LOGICAL   :: lselect
!      EXTERNAL  :: lselect

      INTEGER :: nkp, nkp2
      INTEGER :: i, j, k
      INTEGER :: l, m, n
      INTEGER :: iseed
      INTEGER :: info, nn, nnh
      INTEGER :: nwann, nb
      INTEGER :: nsdim, irguide
      INTEGER :: nrguide, ncgfix, ncount
      LOGICAL :: lrguide, lcg
      REAL(dbl) :: epsilon
      REAL(dbl) :: func_om1, func_om2, func_om3
      REAL(dbl) :: func_old1, func_old2, func_old3
      REAL(dbl) :: rre, rri, omt1, omt2, omt3, omiloc
      REAL(dbl) :: func_del, func_del1, func_del2, func_del3, func0
      REAL(dbl) :: gcnorm1, gcfac, gcnorm0, doda0
      REAL(dbl) :: funca, eqc, eqb, eqa, alphamin, falphamin
      COMPLEX(dbl) :: cfunc_exp1, cfunc_exp2, cfunc_exp3, cfunc_exp

      COMPLEX(dbl), ALLOCATABLE ::  cs(:,:,:) ! cs(dimwann,dimwann,nkpts)
      COMPLEX(dbl), ALLOCATABLE ::  cu0(:,:,:) ! cu0(dimwann,dimwann,nkpts)
      COMPLEX(dbl), ALLOCATABLE ::  csheet(:,:,:) ! csheet(dimwann,nkpts,nnmx)
      COMPLEX(dbl), ALLOCATABLE ::  cm0(:,:,:,:) ! cm0(dimwann,dimwann,nkpts,nnmx)
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
      REAL(dbl), ALLOCATABLE ::  sheet(:,:,:) ! sheet(dimwann,nkpts,nnmx)

      REAL(dbl), ALLOCATABLE :: rguide(:,:)!  rguide(3,dimwann)
      COMPLEX(dbl), ALLOCATABLE :: cwschur1(:) !  cwschur1(dimwann)
      COMPLEX(dbl), ALLOCATABLE :: cwschur2(:) !  cwschur2(10*dimwann)
      REAL(dbl),    ALLOCATABLE :: cwschur3(:) !  cwschur3(dimwann)
      LOGICAL,      ALLOCATABLE :: cwschur4(:) !  cwschur4(dimwann)

      INTEGER      :: nwork
      REAL(dbl)    :: rtot(3), r2tot
      COMPLEX(dbl) :: cfact

      INTEGER :: idum, rdum, ierr

!
! ... End declarations and dimensions
!
!=----------------------------------------------------------------------------=!


!
! ...  Startup
!
       CALL startup(version_number,MAIN_NAME='wannier')
       CALL timing('overlap',OPR='start')

!
! ...  Read input parameters from window.out
!

      CALL read_input()

      CALL ioname('dft_data',filename)
      OPEN( UNIT=dft_unit, FILE=TRIM(filename), STATUS='OLD', FORM='UNFORMATTED' )

      READ(dft_unit) alat
      READ(dft_unit) (avec(i,1),i=1,3)
      READ(dft_unit) (avec(i,2),i=1,3)
      READ(dft_unit) (avec(i,3),i=1,3)

      READ(dft_unit) ntype
      DO i = 1, ntype
          READ(dft_unit) natom(i), nameat(i)
          DO j=1, natom(i)
            READ(dft_unit) (rat(k,j,i),k=1,3)
          ENDDO
      ENDDO

      READ(dft_unit) rdum
      READ(dft_unit) (nk(i),i=1,3), (s(i),i=1,3)

      READ(dft_unit) rdum ! win_min, win_max, froz_min, froz_max, dimwann

      READ(dft_unit) rdum ! alpha, maxiter 
      READ(dft_unit) idum ! iphase
      READ(dft_unit) idum ! niter0, alphafix0
      READ(dft_unit) idum ! niter, alphafix, ncg
      READ(dft_unit) idum ! itrial, nshells
      READ(dft_unit) idum ! (nwhich(i),i=1,nshells)

      READ(dft_unit) nkpts, idum, mxdbnd


!
! ... Allocations and initializations
      CALL wannier_center_init( alat, avec )
      CALL lattice_init()
      !
      ! positions conversion
      CALL poscart_set( avec )

!
! ... Calculate grid of K-points and allocations (including bshells)
      CALL kpoints_init( nkpts )
!
! ... windows dimensions
      CALL windows_allocate()
!
! ... wannier-specific variables init
      CALL localization_allocate()

!
! ... import overlap and projections from the disentangle sotred data
      CALL overlap_extract(dimwann)

      CALL timing('overlap',OPR='stop')
      CALL timing('init',OPR='start')

!
!...  Wannier Functions localization procedure
! 

      WRITE(stdout,"(/,2x,70('='))")
      WRITE(stdout,"(2x,'=',18x,'Starting localization procedure',19x,'=')")
      WRITE(stdout,"(2x,70('=')),/")

      !
      ! ... Now calculate the average positions of the Wanns.

      ALLOCATE( csheet(dimwann,nkpts,nnmx), STAT=ierr )
         IF( ierr /=0 ) CALL errore(' wannier ', ' allocating csheet ', dimwann*nkpts*nnmx)
      ALLOCATE( sheet(dimwann,nkpts,nnmx), STAT=ierr )
         IF( ierr /=0 ) CALL errore(' wannier ', ' allocating sheet ', dimwann*nkpts*nnmx)

      sheet(:,:,:) = ZERO
      csheet(:,:,:) = CONE

      CALL omega( dimwann, nkpts, nkpts, nntot(:), nnmx, nnlist(:,:), bk(:,:,:), wb(:,:), &
                  cm(:,:,:,:), csheet(:,:,:), sheet(:,:,:), rave(:,:), r2ave(:), rave2(:), &
                  func_om1, func_om2, func_om3, Omega_tot, rtot, r2tot, Omega_I, Omega_D, & 
                  Omega_OD, Omega_V )

      !
      !...  Write centers and spread

      DO nwann = 1, dimwann
        WRITE( stdout, fmt= " ( 4x, 'Center ', i3, 1x, '= (',f12.6,',',f12.6,',',f12.6,  &
           &' )  Omega = ', f13.6 )" )  nwann,( rave(i,nwann), i=1,3 ),  &
                                        r2ave(nwann) - rave2(nwann)
      END DO  
      WRITE( stdout, fmt= " ( /,2x, '! Center Sum', &
           &  1x, '= (',f12.6,',',f12.6,',',f12.6,' )  Omega = ', f13.6 )" )     &
           (rtot(i),i=1,3), r2tot
              
      WRITE( stdout, fmt="(/,2x, 'Spread Operator decomposition: ')")
      WRITE( stdout, fmt="(  4x,'OmegaI    =   ', f15.9 ) " ) Omega_I
      WRITE( stdout, fmt="(  4x,'OmegaD    =   ', f15.9 ) " ) Omega_D
      WRITE( stdout, fmt="(  4x,'OmegaOD   =   ', f15.9i,/ ) " ) Omega_OD
      WRITE( stdout, fmt="(  4x,'Omega Tot =   ', f15.9 ) " ) Omega_tot

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
      DO nkp = 1, nkpts
        CALL zmat_mul( cs(:,:,nkp), ca(:,:,nkp), 'N', ca(:,:,nkp), 'C', dimwann )

        CALL zgees( 'V', 'N', lselect, dimwann, cs(1,1,nkp), dimwann, nsdim,            &
             cwschur1, cz(1,1), dimwann, cwschur2, nwork, cwschur3, cwschur4, info )

        cs(:,:,nkp)=CZERO

        DO m=1,dimwann
           cfact = ONE/SQRT( REAL( cwschur1(m) ) )
           DO j=1,dimwann
           DO i=1,dimwann
              cs(i,j,nkp) = cs(i,j,nkp) + cz(i,m) * cfact * CONJG( cz(j,m) )
           ENDDO
           ENDDO
        ENDDO

        CALL zmat_mul( cu(:,:,nkp), cs(:,:,nkp), 'N', ca(:,:,nkp), 'N', dimwann )

! ...  Unitariety is checked
        IF ( .NOT. zmat_unitary( cu(:,:,nkp), SIDE='both', TOLL=1.0d-8 )  ) &
             WRITE (stdout, " (/,2x, 'WARNING: U matrix NOT unitary at ikpt = ',i4)") nkp

!
! ... Phases
!

        IF (iphase /= 1) THEN

          IF ( iphase == 2 ) THEN
! ...         check what happens if the heuristic phase is taken away
              WRITE (stdout,*) 'NB: phase is taken away'
              DO j = 1, dimwann
              DO i = 1, dimwann
                 cu(i,j,nkp) = CZERO
                 IF ( i == j ) cu(i,j,nkp) = CONE
              ENDDO
              ENDDO
          ELSE
! ...         check what happens if a random phase is given

              iseed   = -1
              epsilon = ONE

              WRITE(stdout, fmt=" (2x, 'NB: RANDOM phase is given' )")
              DO m = 1, dimwann
              DO n = m, dimwann
                  rre = ranf(iseed) * 10.d0 - 5.d0
                  rri = ranf(iseed) * 10.d0 - 5.d0
                  cu(m,n,nkp) = epsilon * CMPLX(rre,rri)
                  cu(n,m,nkp) = -CONJG( cu(m,n,nkp) )
                  IF ( m == n ) cu(n,m,nkp) = CMPLX( ZERO , AIMAG( cu(m,n,nkp) ) )
              ENDDO
              ENDDO

              CALL zgees( 'V', 'N', lselect, dimwann, cu(1,1,nkp), dimwann, nsdim,   &
                 cwschur1, cz(1,1), dimwann, cwschur2, SIZE(cwschur2), cwschur3,    &
                 cwschur4, info )

              cu( :, :, nkp ) = CZERO
              DO m = 1, dimwann
                 cfact = EXP( cwschur1(m) )
                 DO j = 1, dimwann
                 DO i = 1, dimwann
                     cu(i,j,nkp) = cu(i,j,nkp) + cz(i,m) * cfact * CONJG( cz(j,m) )
                 ENDDO
                 ENDDO
              ENDDO

          ENDIF

        ENDIF

        IF ( verbosity == 'high' ) THEN
            WRITE (stdout, fmt=" (/,2x,' Matrix U after zgees, k-point',i3,/)") nkp
            DO i = 1, dimwann
                WRITE(stdout, fmt="(4x,2f9.5,2x,2f9.5,2x,2f9.5,2x,2f9.5) ")  &
                                   ( cu(i,j,nkp), j=1,dimwann )
            ENDDO
        ENDIF

      ENDDO  ! k point loop

      ALLOCATE( cmtmp(dimwann,dimwann), STAT=ierr )
         IF( ierr /=0 ) CALL errore(' wannier ', ' allocating cu ', dimwann**2 )

! ... So now we have the U's that rotate the wavefunctions at each k-point.
!     the matrix elements M_ij have also to be updated 

      DO nkp = 1, nkpts

        DO nn = 1, nntot(nkp)
          nkp2 = nnlist(nkp,nn)
          DO i = 1, dimwann
          DO j = 1 ,dimwann
              cmtmp(i,j) = CZERO
              DO m = 1, dimwann
              DO n = 1, dimwann
                 cmtmp(i,j) = cmtmp(i,j) + CONJG(cu(m,i,nkp)) * &
                                                 cu(n,j,nkp2) * cm(m,n,nn,nkp)
              ENDDO
              ENDDO
          ENDDO
          ENDDO
          cm(:,:,nn,nkp) = cmtmp(:,:)
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

      DO nkp = 1, nkpts
        omiloc = ZERO
        DO nn = 1, nntot(nkp)
          cmtmp(:,:) = cm(:,:,nn,nkp)
          CALL zgesvd( 'A', 'A', dimwann, dimwann, cmtmp, dimwann,      &
               singvd, cv1, dimwann, cv2, dimwann, cw1, nwork, cw2, info )
          IF ( info /= 0 ) &
              CALL errore(' wannier ', ' Singular value decomposition failed 2 ', info )

          DO nb = 1, dimwann
              omiloc = omiloc + wb(nkp,nn) * ( ONE - singvd(nb)**2 )
          ENDDO
          DO nb = 1, dimwann
              omt1 = omt1 + wb(nkp,nn) * ( ONE - singvd(nb)**2 )
              omt2 = omt2 - wb(nkp,nn) * ( TWO * LOG( singvd(nb) ) )
              omt3 = omt3 + wb(nkp,nn) * ( ACOS( singvd(nb) )**2 )
          ENDDO
        ENDDO
      ENDDO

      omt1 = omt1/DBLE(nkpts)
      omt2 = omt2/DBLE(nkpts)
      omt3 = omt3/DBLE(nkpts)

! ... Recalculate the average positions of the Wanns.

      CALL omega( dimwann, nkpts, nkpts, nntot, nnmx, nnlist, bk, wb, cm,  &
           csheet, sheet, rave, r2ave, rave2, func_om1, func_om2, func_om3, Omega_tot , &
           rtot, r2tot, Omega_I, Omega_D, Omega_OD, Omega_V)

      func_del1 = func_om1 - func_old1
      func_del2 = func_om2 - func_old2
      func_del3 = func_om3 - func_old3
      func_old1 = func_om1
      func_old2 = func_om2
      func_old3 = func_om3


! ... Find the guiding centers, and set up the 'best' Riemannian sheets for 
!     the complex logarithms

      irguide = 0
!     CALL phases( dimwann, nkpts, nkpts, nnmx, nnmxh, nntot, nnh, neigh,        &
!          bk, bka, cm, csheet, sheet, rguide, irguide )
      irguide = 1

! ... Recalculate the average positions of the Wanns.

      CALL omega( dimwann, nkpts, nkpts, nntot, nnmx, nnlist, bk, wb, cm,        &
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

      DO nkp = 1, nkpts
        DO nn = 1, nntot(nkp)

          cmtmp(:,:) = cm(:,:,nn,nkp)
          CALL zgesvd( 'A', 'A', dimwann, dimwann, cmtmp, dimwann,      &
               singvd, cv1, dimwann, cv2, dimwann, cw1, 10*dimwann, cw2, info )

          IF ( info /= 0 ) &
          CALL errore(' wannier ', ' Singular value decomposition failed 3 ', info )

          CALL zmat_mul( cv3, cv1, 'N', cv2, 'N', dimwann )

          DO nb = 1, dimwann
              omt1 = omt1 + wb(nkp,nn) * ( ONE - singvd(nb)**2 )
              omt2 = omt2 - wb(nkp,nn) * ( TWO * LOG( singvd(nb) ) )
              omt3 = omt3 + wb(nkp,nn) * ( ACOS( singvd(nb) )**2)
          ENDDO

        ENDDO     ! loop nn
      ENDDO       ! loop nkp


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
      ALLOCATE( cm0(dimwann,dimwann,nnmx,nkpts), STAT=ierr )
         IF( ierr /=0 ) CALL errore(' wannier ', ' allocating cm0 ', dimwann**2*nkpts*nnmx )
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
      WRITE( stdout, "(/,2x,70('='))" ) 
      WRITE( stdout, "(2x,'=',21x,'Starting iteration loop',24x,'=')" )
      WRITE( stdout, "(2x,70('='),/)" ) 


!
! ... Main ITERATION loop
!
      iteration_loop : DO ncount = 1, niter + niter0     

        IF ( ncount <= niter0 ) THEN
            ncg   = 1
            alpha = alphafix0
        ELSE
            ncg   = ncgfix
            alpha = alphafix
        END IF


! ...   Store cu and cm

        cu0 = cu
        cm0 = cm

        IF ( lrguide ) THEN
! XXXX  nnh = nntot(1)/2, vedi Marzari... to be fixed here
          IF ( ( ( ncount / 10 ) * 10 == ncount ) .and. ( ncount >= nrguide ) )            &
            CALL phases( dimwann, nkpts, nkpts, nnmx, nnmxh, nntot, nnh, neigh,       &
                 bk, bka, cm, csheet, sheet, rguide, irguide )
        ENDIF

        CALL domega( dimwann, nkpts, nkpts, nntot, nnmx, nnlist, bk, wb,              &
             cm, csheet, sheet, rave, r2ave, cdodq1, cdodq2, cdodq3, cdodq)

        gcnorm1 = ZERO
        DO nkp = 1, nkpts
            DO n = 1, dimwann
            DO m= 1, dimwann
                gcnorm1 = gcnorm1 + REAL( cdodq(m,n,nkp) * CONJG( cdodq(m,n,nkp) ) )
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
        DO nkp = 1, nkpts
            DO m = 1, dimwann
            DO n = 1, dimwann
                doda0 = doda0 + REAL( cdq(m,n,nkp) * cdodq(n,m,nkp) )
            ENDDO
            ENDDO
        ENDDO
        doda0 = doda0 / wbtot / FOUR


! ...   The cg step is calculated
        cdq = alpha / wbtot / FOUR * cdq

        cfunc_exp1 = CZERO
        cfunc_exp2 = CZERO
        cfunc_exp3 = CZERO
        DO nkp = 1, nkpts
            DO i = 1, dimwann
            DO j = 1, dimwann
              cfunc_exp1 = cfunc_exp1 + cdodq1(i,j,nkp) * cdq(j,i,nkp)
              cfunc_exp2 = cfunc_exp2 + cdodq2(i,j,nkp) * cdq(j,i,nkp)
              cfunc_exp3 = cfunc_exp3 + cdodq3(i,j,nkp) * cdq(j,i,nkp)
            ENDDO
            ENDDO
        ENDDO

        DO nkp = 1, nkpts

            CALL zgees( 'V', 'N', lselect, dimwann, cdq(1,1,nkp), dimwann, nsdim,     &
                 cwschur1, cz(1,1), dimwann, cwschur2, SIZE( cwschur2 ), cwschur3,    &
                 cwschur4, info )

            IF ( info /= 0 ) CALL errore ('wannier', 'wrong schur procedure', info)

            cdq( :, :, nkp ) = CZERO
            DO m = 1, dimwann
                cfact = EXP( cwschur1(m) )
                DO j = 1, dimwann
                DO i = 1, dimwann
                     cdq(i,j,nkp) = cdq(i,j,nkp) + cz(i,m) * cfact * CONJG( cz(j,m) )
                ENDDO
                ENDDO
            ENDDO

        ENDDO


! ...   The expected change in the functional is calculated

        cfunc_exp1 = CZERO
        cfunc_exp2 = CZERO
        cfunc_exp3 = CZERO

        DO nkp = 1, nkpts
            DO i = 1, dimwann
            DO j = 1, dimwann
                cfunc_exp1 = cfunc_exp1 + cdodq1(i,j,nkp) * cdq(j,i,nkp)
                cfunc_exp2 = cfunc_exp2 + cdodq2(i,j,nkp) * cdq(j,i,nkp)
                cfunc_exp3 = cfunc_exp3 + cdodq3(i,j,nkp) * cdq(j,i,nkp)
            ENDDO
            ENDDO
        ENDDO
        cfunc_exp = cfunc_exp1 + cfunc_exp2 + cfunc_exp3


! ...   The orbitals are rotated 

        DO nkp = 1, nkpts
            CALL zmat_mul( cmtmp(:,:), cu(:,:,nkp), 'N', cdq(:,:,nkp), 'N', dimwann )
            cu(:,:,nkp) = cmtmp(:,:)
        ENDDO


! ...   And the M_ij are updated

        DO nkp = 1, nkpts
            DO nn = 1, nntot(nkp)
                nkp2 = nnlist(nkp,nn)
                DO i = 1, dimwann
                DO j = 1, dimwann
                    cmtmp(i,j) = CZERO
                    DO m = 1, dimwann
                    DO n = 1, dimwann
                        cmtmp(i,j) = cmtmp(i,j) + CONJG( cdq(m,i,nkp) ) * &
                                                          cdq(n,j,nkp2) * cm(m,n,nn,nkp)
                    ENDDO
                    ENDDO
                ENDDO
                ENDDO
                cm(:,:,nn,nkp) = cmtmp(:,:)
            ENDDO
        ENDDO


! ...   And the functional is recalculated

        CALL omega( dimwann, nkpts, nkpts, nntot, nnmx, nnlist, bk, wb, cm,       &
             csheet, sheet, rave, r2ave, rave2, func_om1, func_om2, func_om3, Omega_tot, &
             rtot, r2tot, Omega_I, Omega_D, Omega_OD, Omega_V)

        IF ( ncount <= niter0 ) THEN
            IF ( ( (ncount/nprint) * nprint +1) == ncount )  THEN
                WRITE( stdout," (/,2x,'Iteration = ',i5) ") ncount
                WRITE(stdout, " (2x, 'Wannier centers and Spreads (Omega)')")
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

! ...   If lcg is false, or still in the first niter0 iterations, 
!       it skips the optimal alpha paraphernalia 

        IF ( ( lcg ) .and. ( ncount > niter0 ) ) THEN

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
          DO nkp = 1, nkpts
              DO i = 1, dimwann
              DO j = 1, dimwann
                  cfunc_exp1 = cfunc_exp1 + cdodq1(i,j,nkp) * cdq(j,i,nkp)
                  cfunc_exp2 = cfunc_exp2 + cdodq2(i,j,nkp) * cdq(j,i,nkp)
                  cfunc_exp3 = cfunc_exp3 + cdodq3(i,j,nkp) * cdq(j,i,nkp)
              ENDDO
              ENDDO
          ENDDO


          DO nkp = 1, nkpts

              CALL zgees( 'V', 'N', lselect, dimwann, cdq(1,1,nkp), dimwann, nsdim,       &
                   cwschur1, cz(1,1), dimwann, cwschur2, SIZE( cwschur2 ), cwschur3,      &
                   cwschur4, info )

              IF ( info /= 0 ) CALL errore('wannier', 'wrong Schur procedure (II)', info)

              cdq(:,:,nkp) = CZERO
              DO m = 1, dimwann
                  cfact =  EXP( cwschur1(m) ) 
                  DO j = 1, dimwann
                  DO i = 1, dimwann
                      cdq(i,j,nkp) = cdq(i,j,nkp) + cz(i,m) * cfact * CONJG( cz(j,m) )
                  ENDDO
                  ENDDO
              ENDDO

          ENDDO


! ...     The expected change in the functional is calculated

          cfunc_exp1 = CZERO
          cfunc_exp2 = CZERO
          cfunc_exp3 = CZERO
          DO nkp = 1, nkpts
            DO i = 1, dimwann
              DO j = 1, dimwann
                cfunc_exp1 = cfunc_exp1 + cdodq1(i,j,nkp) * cdq(j,i,nkp)
                cfunc_exp2 = cfunc_exp2 + cdodq2(i,j,nkp) * cdq(j,i,nkp)
                cfunc_exp3 = cfunc_exp3 + cdodq3(i,j,nkp) * cdq(j,i,nkp)
              END DO
            END DO
          END DO
          cfunc_exp = cfunc_exp1 + cfunc_exp2 + cfunc_exp3


! ...     The orbitals are rotated 

          DO nkp = 1, nkpts
              CALL zmat_mul( cmtmp(:,:), cu(:,:,nkp), 'N', cdq(:,:,nkp), 'N', dimwann )
              cu(:,:,nkp) = cmtmp(:,:)
          END DO

! ...     And the M_ij are updated

          DO nkp = 1, nkpts
             DO nn = 1, nntot(nkp)
                nkp2 = nnlist(nkp,nn)
                DO i = 1, dimwann
                DO j = 1, dimwann
                    cmtmp(i,j) = CZERO
                    DO m = 1, dimwann
                    DO n = 1, dimwann
                        cmtmp(i,j) = cmtmp(i,j) + CONJG( cdq(m,i,nkp) ) * &
                                                         cdq(n,j,nkp2) * cm(m,n,nn,nkp)
                    ENDDO
                    ENDDO
                ENDDO
                ENDDO
                cm(:,:,nn,nkp) = cmtmp(:,:)
             ENDDO
          ENDDO

! ...     And the functional is recalculated
           
          CALL omega( dimwann, nkpts, nkpts, nntot, nnmx, nnlist, bk, wb, cm,       &
               csheet, sheet, rave, r2ave, rave2, func_om1, func_om2, func_om3, Omega_tot, &
               rtot, r2tot, Omega_I , Omega_D, Omega_OD, Omega_V)

          IF ( ( (ncount/nprint) * nprint +1) == ncount ) THEN
              WRITE( stdout, " (/,2x,'Iteration = ',i5) ") ncount
              WRITE(stdout,  " (  2x, 'Wannier centers and Spreads (Omega)')")
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
          CALL ordering(dimwann,nkpts,rave,rave2,r2ave,cu,ordering_type)

          WRITE( stdout, "(/,2x,70('='))" ) 
          WRITE( stdout, "(2x,'=',24x,'Convergence Achieved',24x,'=')" )
          WRITE( stdout, "(2x,70('='),/)" ) 

          WRITE( stdout, "(/,2x,'Wannier function ordering : ',a,/)") TRIM(ordering_type)
          WRITE( stdout, " (2x, 'Final Wannier centers and Spreads (Omega)')")
          DO nwann = 1, dimwann
            WRITE( stdout, " ( 4x, 'Center ', i3, 1x, '= (',f12.6,',',f12.6,',', &
               & f12.6,' )  Omega = ', f13.6 )" )  &
               nwann,( rave(i,nwann), i=1,3 ), r2ave(nwann) - rave2(nwann)
          END DO
          WRITE( stdout, " ( /, 2x, '! Center Sum',    &
               & 1x, '= (',f12.6,',',f12.6,',',f12.6,' )  Omega = ', f13.6 )" )     &
              (rtot(i),i=1,3), r2tot

          WRITE( stdout, "(/,2x, 'Spread Operator decomposition: ')")
          WRITE( stdout, "(  4x,'OmegaI    =   ', f15.9 ) " ) Omega_I
          WRITE( stdout, "(  4x,'OmegaD    =   ', f15.9 ) " ) Omega_D
          WRITE( stdout, "(  4x,'OmegaOD   =   ', f15.9 ) " ) Omega_OD
          WRITE( stdout, "(/,4x,'Omega Tot =   ', f15.9 ) " ) Omega_tot

          WRITE (stdout, "(  2x,'Omega variation:')")
          WRITE (stdout, "(  4x,'Delta Omega 1   = ',0pe16.8)") func_del1
          WRITE (stdout, "(  4x,'Delta Omega 2   = ',0pe16.8)") func_del2
          WRITE (stdout, "(  4x,'Delta Omega 3   = ',0pe16.8)") func_del3
          WRITE (stdout, "(  4x,'Delta Omega Tot = ',0pe16.8)") func_del
          WRITE (stdout, "(/,2x,'Derivative = ', 2e12.4) ") funca-func0,doda0*alpha
          WRITE( stdout, "(2x,70('='))" ) 

          GO TO 8100
        END IF  

!
! ...  End of the ITERATION loop
!
      ENDDO iteration_loop


      !
      ! ... ordering wannier centers
      !
      CALL ordering(dimwann,nkpts,rave,rave2,r2ave,cu,ordering_type)

      WRITE (stdout, "(/,2x,70('='))")
      WRITE( stdout, "(2x,'=',18x,'Max Number of iteration reached',19x,'=')" ) 
      WRITE (stdout, "(2x,70('='),/)")

      WRITE( stdout, "(2x,'Wannier function ordering : ',a,/)") TRIM(ordering_type)
      WRITE(stdout,  " (2x, 'Final Wannier centers and Spreads (Omega)')")
      DO nwann = 1, dimwann
        WRITE( stdout, " ( 4x, 'Center ', i3, 1x, '= (',f12.6,',',f12.6,',',f12.6,  &
           & ' )  Omega = ', f13.6 )" )  nwann,( rave(i,nwann), i=1,3 ), &
                                         r2ave(nwann) - rave2(nwann)
      END DO
      WRITE( stdout, " (/,2x, '! Center Sum',    &
           & 1x, '= (',f12.6,',',f12.6,',',f12.6,' )  Omega = ', f13.6 )" )     &
             (rtot(i),i=1,3), r2tot

      WRITE( stdout, "(/,2x, 'Spread Operator decomposition: ')")
      WRITE( stdout, "(  4x,'OmegaI    =   ', f15.9 ) " ) Omega_I
      WRITE( stdout, "(  4x,'OmegaD    =   ', f15.9 ) " ) Omega_D
      WRITE( stdout, "(  4x,'OmegaOD   =   ', f15.9 ) " ) Omega_OD
      WRITE( stdout, "(/,4x,'Omega Tot =   ', f15.9 ) " ) Omega_tot

      WRITE (stdout, "(/,2x,'Omega variation:')")
      WRITE (stdout, "(  4x,'Delta Omega 1   = ',0pe16.8)") func_del1
      WRITE (stdout, "(  4x,'Delta Omega 2   = ',0pe16.8)") func_del2
      WRITE (stdout, "(  4x,'Delta Omega 3   = ',0pe16.8)") func_del3
      WRITE (stdout, "(  4x,'Delta Omega Tot = ',0pe16.8)") func_del
      WRITE (stdout, "(/,2x,'Derivative = ', 2e12.4) ") funca-func0,doda0*alpha
      WRITE (stdout, "(/,2x,70('='))")


! ... End of the minimization loop

8100  CONTINUE

      CALL timing('iterations',OPR='stop')
      CALL timing('write',OPR='start')

!
! ... Unitariery of U matrix is checked
      DO nkp = 1, nkpts
          IF (  .NOT. zmat_unitary( cu(:,:,nkp), SIDE='both', TOLL=1.0d-8 )  )  &
               WRITE (stdout, " (/,2x, 'WARNING: U matrix NOT unitary at ikpt = ',i4)")nkp
      ENDDO

! ... Singular value decomposition

      omt1 = ZERO
      omt2 = ZERO
      omt3 = ZERO

      DO nkp = 1, nkpts
        DO nn = 1, nntot(nkp)

          cmtmp(:,:) = cm(:,:,nn,nkp)
          CALL zgesvd( 'A', 'A', dimwann, dimwann, cmtmp, dimwann,      &
                       singvd, cv1, dimwann, cv2, dimwann, cw1, 10*dimwann, cw2, info )
          IF ( info /= 0 ) &
               CALL errore('wannier', 'Singular value decomposition failed 4', info)

          DO nb = 1, dimwann
               omt1 = omt1 + wb(nkp,nn) * ( ONE - singvd(nb)**2 )
               omt2 = omt2 - wb(nkp,nn) * ( TWO * LOG( singvd(nb) ) )
               omt3 = omt3 + wb(nkp,nn) * ( ACOS( singvd(nb) )**2 )
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

      CALL deallocate_input()
      CALL windows_deallocate()
      CALL kpoints_deallocate()
      CALL subspace_deallocate()
      CALL overlap_deallocate()

      CALL timing('write',OPR='stop')
      CALL timing('wannier',OPR='stop')
      CALL timing_overview(stdout,LIST=global_list,MAIN_NAME='wannier')
      CALL timing_deallocate()

      STOP '*** THE END *** (wannier.f90)'
      END PROGRAM wannier
