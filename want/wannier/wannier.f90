!
! Copyright (C) 2004 WanT Group
!
! This file is distributed under the terms of the
! GNU General Public License. See the file `License'
! in the root directory of the present distribution,
! or http://www.gnu.org/copyleft/gpl.txt .
!
! Based on a previous version by I. Souza, N. Marzari and D. Vanderbilt
! See the file README in the root directory for a full list of credits
!
!=====================================================
   PROGRAM wannier
   !=====================================================

      USE kinds
      USE constants, ONLY: CZERO, CONE, ZERO, ONE, TWO, THREE, FOUR, EPS_m8, EPS_m4
      USE parameters, ONLY : nstrx
      USE input_module, ONLY : input_manager
      USE control_module, ONLY : ordering_mode, nprint_wan, nsave_wan,  &
                                 unitary_thr, do_condmin, &
                                 localization_init_mode => localization_init
      USE timing_module, ONLY : timing, timing_upto_now, timing_overview, global_list
      USE io_module, ONLY : stdout, wan_unit, ham_unit, ioname
      USE files_module, ONLY : file_open, file_close
      USE version_module, ONLY : version_number
      USE util_module, ONLY: zmat_unitary, mat_mul, mat_svd, mat_hdiag

      USE want_init_module, ONLY : want_init
      USE summary_module, ONLY : summary
      USE kpoints_module, ONLY: nkpts, nb, wbtot
      USE overlap_module,  ONLY : dimwann, Mkb
      USE localization_module, ONLY : maxiter0_wan, maxiter1_wan, alpha0_wan, alpha1_wan,&
                       ncg, wannier_thr, cu, rave, rave2, r2ave, &
                       Omega_I, Omega_D, Omega_OD, Omega_tot, &
                       localization_allocate, localization_write, localization_print, &
                       a_condmin, niter_condmin, dump_condmin
      USE trial_center_data_module, ONLY : trial
      USE hamiltonian_module, ONLY : hamiltonian_write, hamiltonian_init

!
! ... 
!
      IMPLICIT NONE

      INTEGER :: ik, m, n, ierr
      INTEGER :: ncgfix, ncount, iter
      LOGICAL :: lcg, do_conjgrad
      REAL(dbl) :: Omega_old, Omega_var, Omega0, OmegaA
      REAL(dbl) :: gcnorm1, gcnorm0, gcnorm_aux
      REAL(dbl) :: aux1, aux2, eqb, eqa, alpha, alphamin

      REAL(dbl),    ALLOCATABLE ::  sheet(:,:,:) 
      COMPLEX(dbl), ALLOCATABLE ::  domg(:,:,:) 
      COMPLEX(dbl), ALLOCATABLE ::  domg_aux(:,:,:) 
      COMPLEX(dbl), ALLOCATABLE ::  dq(:,:,:) 
      COMPLEX(dbl), ALLOCATABLE ::  dq0(:,:,:)
      COMPLEX(dbl), ALLOCATABLE ::  cdU(:,:,:)
      COMPLEX(dbl), ALLOCATABLE ::  csheet(:,:,:)
      COMPLEX(dbl), ALLOCATABLE ::  cu0(:,:,:) 
      COMPLEX(dbl), ALLOCATABLE ::  Mkb0(:,:,:,:)
      !
      CHARACTER( LEN=nstrx )  :: filename
      !
      ! ... end of declarations
      !

!
!--------------------------------------------
! ... Startup
!--------------------------------------------
!
      CALL startup(version_number,'wannier')

      !
      ! ... Read input parameters from DFT_DATA file
      !
      CALL input_manager()

      !
      ! ... Global data init
      !
      CALL want_init(WANT_INPUT=.TRUE., PSEUDO=.TRUE.)


      !
      ! ... Summary of the input and DFT data
      !
      CALL summary( stdout, LEIG=.FALSE. )

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
      ALLOCATE( csheet(dimwann,nb,nkpts), STAT=ierr )
         IF( ierr /=0 ) CALL errore('wannier', 'allocating csheet', ABS(ierr))
      ALLOCATE( sheet(dimwann,nb,nkpts), STAT=ierr )
         IF( ierr /=0 ) CALL errore('wannier', 'allocating sheet', ABS(ierr))

      ALLOCATE( cu0(dimwann,dimwann,nkpts), STAT=ierr )
         IF( ierr /=0 ) CALL errore('wannier', 'allocating cu0', ABS(ierr) )
      ALLOCATE( Mkb0(dimwann,dimwann,nb,nkpts), STAT=ierr )
         IF( ierr /=0 ) CALL errore('wannier', 'allocating Mkb0', ABS(ierr) )
      ALLOCATE( domg(dimwann,dimwann,nkpts), STAT=ierr )
         IF( ierr /=0 ) CALL errore('wannier', 'allocating domg', ABS(ierr) )
      ALLOCATE( domg_aux(dimwann,dimwann,nkpts), STAT=ierr )
         IF( ierr /=0 ) CALL errore('wannier', 'allocating domg_aux', ABS(ierr) )
      ALLOCATE( dq0(dimwann,dimwann,nkpts), STAT=ierr )
         IF( ierr /=0 ) CALL errore('wannier', 'allocating dq0', ABS(ierr) )
      ALLOCATE( dq(dimwann,dimwann,nkpts), STAT=ierr )
         IF( ierr /=0 ) CALL errore('wannier', 'allocating dq', ABS(ierr) )
      ALLOCATE( cdU(dimwann,dimwann,nkpts), STAT=ierr )
         IF( ierr /=0 ) CALL errore('wannier', 'allocating cdU', ABS(ierr) )

 

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

      CALL localization_init( localization_init_mode )

      sheet(:,:,:)   = ZERO
      csheet(:,:,:)  = CONE


      !
      ! ... Calculate the average positions of the WFs
      !
      CALL omegai( Omega_I, dimwann, nkpts, Mkb )
      CALL omega( dimwann, nkpts, Mkb, csheet, sheet, rave, r2ave, rave2,  &
                  Omega_D, Omega_OD )
      Omega_tot = Omega_I + Omega_D + Omega_OD 
      !
      CALL localization_print(stdout,FMT="extended")

      Omega0 = Omega_tot
      Omega_var = Omega_tot - Omega_old
      Omega_old = Omega_tot


      !
      ! few details
      !
      lcg = .TRUE.
      IF ( ncg < 1 ) lcg = .FALSE.
      IF ( ncg == 0 ) ncg = 1
      ncgfix = ncg

      aux1 = ONE / ( FOUR * wbtot )
      dq0(:,:,:) = CZERO



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

           !
           ! Store cu and Mkb
           !
           cu0(:,:,:)    = cu(:,:,:)
           Mkb0(:,:,:,:) = Mkb(:,:,:,:)

           !
           ! settings
           !
           IF ( ncount <= maxiter0_wan ) THEN
                do_conjgrad = .FALSE.
                ncg   = 1
                alpha = alpha0_wan
           ELSE
                do_conjgrad = lcg
                ncg   = ncgfix
                alpha = alpha1_wan
           ENDIF
           IF ( ncount > niter_condmin ) THEN 
                a_condmin = a_condmin * dump_condmin
                IF ( a_condmin < EPS_m4 ) do_condmin = .FALSE.
           ENDIF


           !
           ! compute the derivative of the functional
           !
           CALL domega( dimwann, nkpts, Mkb, csheet, sheet, rave, domg)

           !
           ! apply conditioned minimization if required
           !
           IF ( do_condmin ) THEN
                CALL domega_aux( dimwann, nkpts, Mkb, rave, trial, a_condmin, domg_aux)
                domg(:,:,:) = domg(:,:,:) + domg_aux(:,:,:)
           ENDIF


           !
           ! compute the norm of domg
           !
           gcnorm1 = ZERO
           DO ik = 1, nkpts
               DO n = 1, dimwann
               DO m = 1, dimwann
                   gcnorm1 = gcnorm1 + REAL( domg(m,n,ik)*CONJG( domg(m,n,ik) ) )
               ENDDO
               ENDDO
           ENDDO


           !
           ! set dq
           !
           dq(:,:,:) = domg(:,:,:) 
           IF ( MOD( (ncount-1), ncg ) /= 0 )  THEN 
                dq(:,:,:) = dq(:,:,:) + gcnorm1/gcnorm0 * dq0(:,:,:)
           ENDIF
           !
           gcnorm0 = gcnorm1
           dq0(:,:,:) = dq(:,:,:)

           
           !
           ! auxiliary norm for cg
           !
           gcnorm_aux = ZERO
           DO ik = 1, nkpts
               !
               DO n = 1, dimwann
               DO m = 1, dimwann
                   gcnorm_aux = gcnorm_aux - REAL( dq(m,n,ik) * CONJG( domg(m,n,ik)) )
               ENDDO
               ENDDO
               !
           ENDDO
           !
           gcnorm_aux = gcnorm_aux * aux1

           !
           ! The cg step is calculated
           !
           aux2 = alpha * aux1
           dq(:,:,:) = aux2 * dq(:,:,:)


           !
           ! compute the change in the unitary matrix dU = e^(i * dq)
           ! and update U
           !
           CALL unitary_update( dimwann, nkpts, dq, cu, cdU ) 


           !
           ! update the overlap Mkb, according to the new dCu
           !
           CALL overlap_update(dimwann, nkpts, cdU, Mkb)


           !
           ! The functional is recalculated
           !
           CALL omega( dimwann, nkpts, Mkb, csheet, sheet, rave, r2ave, rave2, &
                       Omega_D, Omega_OD )
           Omega_tot = Omega_I + Omega_D + Omega_OD
           !
           OmegaA = Omega_tot
           Omega_var = Omega_tot - Omega_old


           !
           ! perform Conjugate-Gradinent minimization if the case
           ! 
           IF ( do_conjgrad ) THEN
               !
               ! recover cu and Mkb
               !
               cu = cu0
               Mkb = Mkb0
   
               !
               ! Take now optimal parabolic step
               !
               eqb = gcnorm_aux
               eqa = ( OmegaA - Omega0 - eqb * alpha ) / alpha
   
               alphamin = alpha
               IF ( ABS(eqa) > EPS_m8 ) alphamin = -eqb / ( TWO * eqa )
               IF ( alphamin < ZERO )   alphamin = TWO * alpha 
               IF ( alphamin > THREE * alpha ) alphamin = THREE * alpha
               !
               aux2 = alphamin * aux1
               dq(:,:,:) = aux2 * dq0(:,:,:)
   

               !
               ! compute the change in the unitary matrix dU = e^(i * dq)
               ! and update U
               !
               CALL unitary_update( dimwann, nkpts, dq, cu, cdU ) 


               !
               ! update the overlap Mkb, according to the new dCu
               !
               CALL overlap_update( dimwann, nkpts, cdU, Mkb)


               !
               ! The functional is recalculated
               !  
               CALL omega( dimwann, nkpts, Mkb, csheet, sheet, rave, r2ave, rave2, &
                           Omega_D, Omega_OD )
               Omega_tot = Omega_I + Omega_D + Omega_OD
           
               OmegaA = Omega_tot
               Omega_var = Omega_tot - Omega_old
   
               !
               ! end of the optimal alpha paraphernalia 
               !
           ENDIF
   
           Omega_old = Omega_tot
           Omega0 = OmegaA
   
   
           !
           ! write info to stdout
           !
           IF ( MOD( ncount, nprint_wan ) == 0 .OR. ncount == 1 ) THEN
                IF ( do_condmin ) THEN 
                     WRITE( stdout,"(/,2x,'Iteration = ',i5,3x, &
                            & '(condit. minim, A = ',f9.4,' )')") ncount, a_condmin
                ELSE
                     WRITE( stdout,"(/,2x,'Iteration = ',i5) ") ncount
                ENDIF
                CALL localization_print(stdout, FMT="standard" )
                WRITE( stdout, " (2x,'Omega variation (Bohr^2):  ',f12.6) ") Omega_var
                
                CALL timing_upto_now(stdout)
           ENDIF
   
   
           !
           ! write data to disk
           !
           IF ( MOD( ncount, nsave_wan ) == 0 ) THEN
                CALL ioname('wannier',filename)
                CALL file_open(wan_unit,TRIM(filename),PATH="/",ACTION="write", &
                               FORM="formatted")
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
      WRITE( stdout, "(2x,70('='),2/)" )
      WRITE( stdout, "(2x,'Iteration # : ',i5)") ncount


      !
      ! ... ordering wannier centers
      !
      CALL ordering(dimwann,nkpts,rave,rave2,r2ave,cu, ordering_mode)
      WRITE( stdout, "(2x,'Wannier function ordering : ',a,/)") TRIM(ordering_mode)

      CALL localization_print(stdout, FMT="extended")
      CALL timing_upto_now(stdout)


      !
      ! ... Unitariery of U matrix is checked
      !
      DO ik = 1, nkpts
          !
          IF (  .NOT. zmat_unitary( dimwann, dimwann, cu(:,:,ik),  &
                                    SIDE='both', TOLL=unitary_thr )  )  &
               WRITE (stdout, " (2x, 'WARNING: U matrix NOT unitary at ikpt = ',i4)")ik
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
      WRITE(stdout,"(2x,70('='))")


      !
      ! ... Convert the Hamiltonian from the bloch basis to the wannier one
      !     and write the results to file
      !
      CALL hamiltonian_init()
      CALL hamiltonian_calc(dimwann, nkpts, cu)

      CALL ioname('hamiltonian',filename)
      CALL file_open(ham_unit,TRIM(filename),PATH="/",ACTION="write", &
                              FORM='formatted')
            CALL hamiltonian_write(ham_unit, "HAMILTONIAN")
      CALL file_close(ham_unit,PATH="/",ACTION="write")

      CALL ioname('hamiltonian',filename,LPATH=.FALSE.)
      WRITE( stdout,"(/,'  Hamiltonian on WF basis written on file : ',a)") TRIM(filename)

!
!--------------------------------------
! ...  Shut down
!--------------------------------------
!
      WRITE(stdout,"(2/,2x,70('='))")

      !
      ! ... Finalize timing
      !
      CALL timing('wannier',OPR='stop')
      CALL timing_overview(stdout,LIST=global_list,MAIN_NAME='wannier')
     
      !
      ! ... Deallocate local arrays
      !
      DEALLOCATE( csheet, STAT=ierr )
           IF( ierr /=0 ) CALL errore('wannier', 'deallocating csheet', ABS(ierr) )
      DEALLOCATE( sheet, STAT=ierr )
           IF( ierr /=0 ) CALL errore('wannier', 'deallocating sheet', ABS(ierr) )
      DEALLOCATE( cu0, STAT=ierr )
           IF( ierr /=0 ) CALL errore('wannier', 'deallocating cu0', ABS(ierr) )
      DEALLOCATE( Mkb0, STAT=ierr )
           IF( ierr /=0 ) CALL errore('wannier', 'deallocating Mkb0', ABS(ierr) )
      DEALLOCATE( domg, domg_aux, STAT=ierr )
           IF( ierr /=0 ) CALL errore('wannier', 'deallocating domg', ABS(ierr) )
      DEALLOCATE( dq0, STAT=ierr )
           IF( ierr /=0 ) CALL errore('wannier', 'deallocating dq0', ABS(ierr) )
      DEALLOCATE( dq, STAT=ierr )
           IF( ierr /=0 ) CALL errore('wannier', 'deallocating dq', ABS(ierr) )
      DEALLOCATE( cdu, STAT=ierr )
           IF( ierr /=0 ) CALL errore('wannier', 'deallocating cdu', ABS(ierr) )

      CALL cleanup

END PROGRAM wannier

