!
! Copyright (C) 2004 WanT Group
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
      USE constants, ONLY: CZERO, CONE, CI, ZERO, ONE, TWO, THREE, FOUR, EPS_m8
      USE parameters, ONLY : nstrx
      USE input_module, ONLY : input_manager
      USE control_module, ONLY : ordering_mode, nprint_wan, nsave_wan,  &
                                 unitary_thr, verbosity, start_mode_wan
      USE timing_module, ONLY : timing, timing_upto_now, timing_overview, global_list
      USE io_module, ONLY : stdout, wan_unit, ioname
      USE files_module, ONLY : file_open, file_close
      USE startup_module, ONLY : startup
      USE cleanup_module, ONLY : cleanup
      USE version_module, ONLY : version_number
      USE util_module, ONLY: zmat_mul, zmat_unitary, mat_svd

      USE want_init_module, ONLY : want_init
      USE summary_module, ONLY : summary
      USE kpoints_module, ONLY: nkpts, nnx, &
                          nntot, nnlist, neigh, bk, wb, bka, wbtot
      USE overlap_module,  ONLY : dimwann, ca, Mkb
      USE localization_module, ONLY : maxiter0_wan, maxiter1_wan, alpha0_wan, alpha1_wan,&
                       ncg, wannier_thr, cu, rave, rave2, r2ave, &
                       Omega_I, Omega_D, Omega_OD, Omega_tot, &
                       localization_allocate, localization_write, localization_print

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
      INTEGER :: info, nn
      INTEGER :: nb
      INTEGER :: nsdim
      INTEGER :: nrguide, ncgfix, ncount, iter
      LOGICAL :: lcg
      REAL(dbl) :: epsilon, alpha
      REAL(dbl) :: Omega_old, Omega_var, Omega0, OmegaA
      REAL(dbl) :: rre, rri
      REAL(dbl) :: gcnorm1, gcfac, gcnorm0, doda0
      REAL(dbl) :: eqc, eqb, eqa, alphamin, falphamin
      COMPLEX(dbl) :: cfunc_exp1, cfunc_exp2, cfunc_exp3, cfunc_exp

      REAL(dbl),    ALLOCATABLE ::  rguide(:,:)
      REAL(dbl),    ALLOCATABLE ::  sheet(:,:,:) 
      COMPLEX(dbl), ALLOCATABLE ::  csheet(:,:,:)
      COMPLEX(dbl), ALLOCATABLE ::  cu0(:,:,:) 
      COMPLEX(dbl), ALLOCATABLE ::  Mkb0(:,:,:,:)
      COMPLEX(dbl), ALLOCATABLE ::  cmtmp(:,:)
      COMPLEX(dbl), ALLOCATABLE ::  cdodq(:,:,:) 
      COMPLEX(dbl), ALLOCATABLE ::  cdqkeep(:,:,:)
      COMPLEX(dbl), ALLOCATABLE ::  cdodq1(:,:,:)
      COMPLEX(dbl), ALLOCATABLE ::  cdodq2(:,:,:) 
      COMPLEX(dbl), ALLOCATABLE ::  cdodq3(:,:,:) 
      COMPLEX(dbl), ALLOCATABLE ::  cdq(:,:,:) 
      COMPLEX(dbl), ALLOCATABLE ::  cz(:,:) 
      COMPLEX(dbl), ALLOCATABLE ::  cv3(:,:) 

      COMPLEX(dbl), ALLOCATABLE :: cwschur1(:) !  cwschur1(dimwann)
      COMPLEX(dbl), ALLOCATABLE :: cwschur2(:) !  cwschur2(10*dimwann)
      REAL(dbl),    ALLOCATABLE :: cwschur3(:) !  cwschur3(dimwann)
      LOGICAL,      ALLOCATABLE :: cwschur4(:) !  cwschur4(dimwann)

      COMPLEX(dbl) :: cfact
      CHARACTER( LEN=nstrx )  :: filename
      INTEGER :: idum, rdum, ierr

!
! ... end of declarations
!

!
!--------------------------------------------
! ... Startup
!--------------------------------------------
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
      !
      CALL localization_allocate()

      !
      ! ... import overlap and projections from the disentangle sotred data
      !
      CALL overlap_extract(dimwann)
      WRITE(stdout,"(/,2x,'Overlaps and projections setup completed')")
      CALL timing_upto_now(stdout)


      !
      ! ... allocate local variables
      !
      ALLOCATE( csheet(dimwann,nnx,nkpts), STAT=ierr )
         IF( ierr /=0 ) CALL errore('wannier', 'allocating csheet ', ABS(ierr))
      ALLOCATE( sheet(dimwann,nnx,nkpts), STAT=ierr )
         IF( ierr /=0 ) CALL errore('wannier', 'allocating sheet ', ABS(ierr))
      sheet(:,:,:) = ZERO
      csheet(:,:,:) = CONE

      ALLOCATE( rguide(3,dimwann), STAT=ierr )
         IF( ierr /=0 ) CALL errore('wannier', 'allocating rguide ', ABS(ierr))
      rguide(:,:) = ZERO

      ALLOCATE( cwschur1(dimwann), cwschur2( 10*dimwann ), STAT=ierr )
         IF( ierr /=0 ) CALL errore('wannier', 'allocating cwschur1 cwschur2 ', ABS(ierr))
      ALLOCATE( cz(dimwann, dimwann), STAT=ierr )
         IF( ierr /=0 ) CALL errore('wannier', 'allocating cz ', ABS(ierr))
      ALLOCATE( cwschur3(dimwann), STAT=ierr )
         IF( ierr /=0 ) CALL errore('wannier', 'allocating cwschur3 ', ABS(ierr))
      ALLOCATE( cwschur4(dimwann), STAT=ierr )
         IF( ierr /=0 ) CALL errore('wannier', 'allocating cwschur4 ', ABS(ierr))

      ALLOCATE( cmtmp(dimwann,dimwann), STAT=ierr )
         IF (ierr/=0) CALL errore('wannier','allocating CMTMP',ABS(ierr))

      ALLOCATE( cu0(dimwann,dimwann,nkpts), STAT=ierr )
         IF( ierr /=0 ) CALL errore('wannier', 'allocating cu0 ', ABS(ierr) )
      ALLOCATE( Mkb0(dimwann,dimwann,nnx,nkpts), STAT=ierr )
         IF( ierr /=0 ) CALL errore('wannier', 'allocating Mkb0 ', ABS(ierr) )
      ALLOCATE( cdodq(dimwann,dimwann,nkpts), STAT=ierr )
         IF( ierr /=0 ) CALL errore('wannier', 'allocating cdodq ', ABS(ierr) )
      ALLOCATE( cdqkeep(dimwann,dimwann,nkpts), STAT=ierr )
         IF( ierr /=0 ) CALL errore('wannier', 'allocating cdqkeep ', ABS(ierr) )
      ALLOCATE( cdodq1(dimwann,dimwann,nkpts), STAT=ierr )
         IF( ierr /=0 ) CALL errore('wannier', 'allocating cdodq1 ', ABS(ierr) )
      ALLOCATE( cdodq2(dimwann,dimwann,nkpts), STAT=ierr )
         IF( ierr /=0 ) CALL errore('wannier', 'allocating cdodq2 ', ABS(ierr) )
      ALLOCATE( cdodq3(dimwann,dimwann,nkpts), STAT=ierr )
         IF( ierr /=0 ) CALL errore('wannier', 'allocating cdodq3 ', ABS(ierr) )
      ALLOCATE( cdq(dimwann,dimwann,nkpts), STAT=ierr )
         IF( ierr /=0 ) CALL errore('wannier ', ' allocating cdq ', ABS(ierr) )

      cdqkeep = CZERO
      cdodq1  = CZERO
      cdodq2  = CZERO
      cdodq3  = CZERO
      cdq     = CZERO
      cdodq   = CZERO
 

!
!--------------------------------------------
!...  Init Wannier Functions localization procedure
!--------------------------------------------
! 

      !
      !
      WRITE( stdout, "(/,2x,70('='))" )
      WRITE( stdout, "(2x,'=',21x,'Init localization procedure',20x,'=')" )
      WRITE( stdout, "(2x,70('='),/)" )

      CALL localization_init(start_mode_wan, dimwann, nkpts, ca, cu, Mkb)


      !
      ! ... if we like to have an idea of how things go before the use of phases
      !
      CALL omega( dimwann, nkpts, Mkb, csheet, sheet, rave, r2ave, rave2, &
                  Omega_I, Omega_D, Omega_OD, Omega_tot )
      CALL localization_print(stdout,FMT="extended")


      ! 
      ! ... Find the guiding centers, and set up the 'best' Riemannian sheets for 
      !     the complex logarithms
      !
      CALL phases( dimwann, nkpts, Mkb, .FALSE. , rguide, csheet, sheet)
     

      !
      ! ... Calculate the average positions of the WFs
      !
      CALL omega( dimwann, nkpts, Mkb, csheet, sheet, rave, r2ave, rave2,  &
                  Omega_I, Omega_D, Omega_OD, Omega_tot )
      CALL localization_print(stdout,FMT="extended")

      Omega0 = Omega_tot
      Omega_var = Omega_tot - Omega_old
      Omega_old = Omega_tot


      !
      ! few details
      !
      lcg = .TRUE.
      IF ( ncg < 1 ) lcg = .FALSE.
      IF ( ncg == 0 ) ncg = ncg + 1
      ncgfix = ncg

      nrguide = 10


!
!----------------------------------------------
!  ... Main iterative loop
!----------------------------------------------
!
      !
      !
      WRITE( stdout, "(/,2x,70('='))" )
      WRITE( stdout, "(2x,'=',22x,'Starting iteration loop',23x,'=')" )
      WRITE( stdout, "(2x,70('='),/)" )

      CALL timing('iterations',OPR='start')

      iteration_loop : &
      DO iter = 1, maxiter0_wan + maxiter1_wan
        ncount = iter

        IF ( ncount <= maxiter0_wan ) THEN
            ncg   = 1
            alpha = alpha0_wan
        ELSE
            ncg   = ncgfix
            alpha = alpha1_wan
        ENDIF

        !
        ! Store cu and Mkb
        !
        cu0 = cu
        Mkb0 = Mkb

        IF ( MOD( ncount, 10 ) == 0 .AND. ( ncount >= nrguide ) )   &
            CALL phases( dimwann, nkpts, Mkb, .TRUE. , rguide, csheet, sheet )

        CALL domega( dimwann, nkpts, nkpts, nntot, nnx, nnlist, bk, wb,              &
             Mkb, csheet, sheet, rave, r2ave, cdodq1, cdodq2, cdodq3, cdodq)

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

        !
        ! The cg step is calculated
        cdq = alpha / wbtot / FOUR * cdq
        !
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

        !
        ! The expected change in the functional is calculated
        !
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

        !
        ! The orbitals are rotated 
        !
        DO ik = 1, nkpts
            CALL zmat_mul( cmtmp(:,:), cu(:,:,ik), 'N', cdq(:,:,ik), 'N', &
                           dimwann, dimwann, dimwann )
            cu(:,:,ik) = cmtmp(:,:)
        ENDDO

        !
        ! And the M_ij are updated
        !
        CALL overlap_update(dimwann, nkpts, cdq, Mkb)


        !
        ! The functional is recalculated
        !
        CALL omega( dimwann, nkpts, Mkb, csheet, sheet, rave, r2ave, rave2, &
                    Omega_I, Omega_D, Omega_OD, Omega_tot )

        OmegaA = Omega_tot
        Omega_var = Omega_tot - Omega_old


        !
        ! If LCG is false, or still in the first maxiter0_wan iterations, 
        ! it skips the optimal alpha paraphernalia 
        ! 
        IF ( lcg .AND. ( ncount > maxiter0_wan ) ) THEN

          eqc = Omega0
          eqb = doda0
          eqa = ( OmegaA - Omega0 - eqb * alpha ) / alpha

          IF ( ABS(eqa) > EPS_m8 ) THEN
            alphamin = -eqb / TWO / eqa
          ELSE
            alphamin = alpha
          ENDIF
          IF ( alphamin < ZERO ) alphamin = alpha * TWO
          IF ( alphamin > THREE * alpha ) alphamin = THREE * alpha
          falphamin = eqa * alphamin**2 + eqb * alphamin + eqc


          !
          ! Restore cu and Mkb
          !
          cu = cu0
          Mkb = Mkb0

          !
          ! Take now optimal parabolic step
          cdq = alphamin / wbtot / FOUR * cdqkeep

          !
          ! The expected change in the functional is calculated
          !
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

          !
          ! The expected change in the functional is calculated
          !
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

          !
          ! The orbitals are rotated 
          !
          DO ik = 1, nkpts
              CALL zmat_mul( cmtmp(:,:), cu(:,:,ik), 'N', cdq(:,:,ik), 'N', &
                             dimwann, dimwann, dimwann )
              cu(:,:,ik) = cmtmp(:,:)
          END DO

          !
          ! M_ij are updated
          !
          CALL overlap_update( dimwann, nkpts, cdq, Mkb)

          !
          ! The functional is recalculated
          !  
          CALL omega( dimwann, nkpts, Mkb, csheet, sheet, rave, r2ave, rave2, &
                      Omega_I, Omega_D, Omega_OD, Omega_tot )
        
          OmegaA = Omega_tot
          Omega_var = Omega_tot - Omega_old


        ! ...   end of the lcg skip of the optimal alpha paraphernalia 
        ENDIF

        Omega_old = Omega_tot
        Omega0 = OmegaA


        !
        ! write info to stdout
        !
        IF ( MOD( ncount, nprint_wan ) == 0 .OR. ncount == 1 ) THEN
             WRITE( stdout, " (/,2x,'Iteration = ',i5) ") ncount
             CALL localization_print(stdout, FMT="standard" )
             WRITE( stdout, " (2x,'Omega variation (Bohr^2):  ',f12.6) ") Omega_var
             
             CALL timing_upto_now(stdout)
        ENDIF


        !
        ! write data to disk
        !
        IF ( MOD( ncount, nsave_wan ) == 0 ) THEN
             CALL ioname('wannier',filename)
             CALL file_open(wan_unit,TRIM(filename),PATH="/",ACTION="write",FORM="formatted")
                  CALL localization_write(wan_unit,"WANNIER_LOCALIZATION")
             CALL file_close(wan_unit,PATH="/",ACTION="write")
        ENDIF
             
     
        !
        ! convergence condition
        !
        IF ( ABS( Omega_var ) < wannier_thr ) EXIT iteration_loop
      ENDDO iteration_loop
      !
      ! ... End of iter loop
      !

!
!--------------------------------------
! ...  Final processing
!--------------------------------------
!

      !
      ! ...  Status of the convergence
      !
      WRITE(stdout, "()")
      CALL timing('iterations',OPR='stop')

      WRITE( stdout, "(/,2x,70('='))" )
      IF ( ncount == maxiter0_wan + maxiter1_wan ) THEN
          WRITE( stdout, "(2x,'=',18x,'Max number of iteration reached',18x,'=')")
      ELSE
          WRITE( stdout, "(2x,'=',24x,'Convergence Achieved',24x,'=')" )
      ENDIF
      WRITE( stdout, "(2x,70('='),/)" )


      !
      ! ... ordering wannier centers
      !
      CALL ordering(dimwann,nkpts,rave,rave2,r2ave,cu, ordering_mode)
      WRITE( stdout, "(/,2x,'Wannier function ordering : ',a,/)") TRIM(ordering_mode)

      CALL localization_print(stdout, FMT="extended")
      CALL timing_upto_now(stdout)


      !
      ! ... Unitariery of U matrix is checked
      !
      DO ik = 1, nkpts
          IF (  .NOT. zmat_unitary( cu(:,:,ik), SIDE='both', TOLL=unitary_thr )  )  &
               WRITE (stdout, " (/,2x, 'WARNING: U matrix NOT unitary at ikpt = ',i4)")ik
      ENDDO


      ! 
      ! ... Write the final unitary transformations and all other data referring
      !     to the Wannier localization procedure to a file
      !
      CALL ioname('wannier',filename)
      CALL file_open(wan_unit,TRIM(filename),PATH="/",ACTION="write",FORM="formatted")
           CALL localization_write(wan_unit,"WANNIER_LOCALIZATION")
      CALL file_close(wan_unit,PATH="/",ACTION="write")

      CALL ioname('wannier',filename,LPATH=.FALSE.)
      WRITE( stdout,"(/,2x,'Unitary transf. matrixes written on file: ',a)") &
                    TRIM(filename)

!
!--------------------------------------
! ...  Shut down
!--------------------------------------
!
      WRITE(stdout,"(2x,70('='))")

      !
      ! ... Finalize timing
      !
      CALL timing('wannier',OPR='stop')
      CALL timing_overview(stdout,LIST=global_list,MAIN_NAME='wannier')
     
      !
      ! ... Deallocate local arrays
      !
      DEALLOCATE( cwschur1, cwschur2, STAT=ierr )
           IF( ierr /=0 )&
           CALL errore(' wannier ', ' deallocating cwschur1 cwschur1 ', ABS(ierr) )
      DEALLOCATE( cz, STAT=ierr )
           IF( ierr /=0 ) CALL errore(' wannier ', ' deallocating cz ', ABS(ierr) )
      DEALLOCATE( cwschur3, STAT=ierr )
           IF( ierr /=0 ) CALL errore(' wannier ', ' deallocating cwschur3 ', ABS(ierr) )
      DEALLOCATE( cwschur4, STAT=ierr )
           IF( ierr /=0 ) CALL errore(' wannier ', ' deallocating cwschur4 ', ABS(ierr) )

      DEALLOCATE( rguide, STAT=ierr )
           IF( ierr /=0 ) CALL errore('wannier', 'deallocating rguide ', ABS(ierr))
      DEALLOCATE( csheet, STAT=ierr )
           IF( ierr /=0 ) CALL errore(' wannier ', ' deallocating csheet ', ABS(ierr) )
      DEALLOCATE( sheet, STAT=ierr )
           IF( ierr /=0 ) CALL errore(' wannier ', ' deallocating sheet ', ABS(ierr) )
      DEALLOCATE( cmtmp, STAT=ierr )
           IF( ierr /=0 ) CALL errore(' wannier ', ' deallocating cmtmp ', ABS(ierr) )
      DEALLOCATE( cu0, STAT=ierr )
           IF( ierr /=0 ) CALL errore(' wannier ', ' deallocating cu0 ', ABS(ierr) )
      DEALLOCATE( Mkb0, STAT=ierr )
           IF( ierr /=0 ) CALL errore(' wannier ', ' deallocating Mkb0 ', ABS(ierr) )
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

