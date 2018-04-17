!
! Copyright (C) 2004 WanT Group
!
! This file is distributed under the terms of the
! GNU General Public License. See the file `License'
! in the root directory of the present distribution,
! or http://www.gnu.org/copyleft/gpl.txt .
!
! Based on a previous version by I. Souza, N. Marzari and D. Vanderbilt
! See the CREDITS file in the ~want directory for a full description
!
!=====================================================
   PROGRAM wannier
   !=====================================================
      !
      USE kinds
      USE constants,           ONLY : CZERO, CONE, ZERO, ONE, TWO, THREE, FOUR, EPS_m8, EPS_m4
      USE parameters,          ONLY : nstrx
      USE control_module,      ONLY : nprint_wan, nsave_wan,  &
                                      unitary_thr, use_condmin, read_pseudo, do_polarization, &
                                      localization_init_mode => localization_init, &
                                      ordering_mode, do_ordering, do_collect_wf 
      USE io_module,           ONLY : stdout, wan_unit, ham_unit, io_name, wantdata_form
      USE io_module,           ONLY : ionode
      USE timing_module,       ONLY : timing, timing_upto_now
      USE log_module,          ONLY : log_push, log_pop
      USE files_module,        ONLY : file_open, file_close
      USE version_module,      ONLY : version_number
      USE util_module,         ONLY : zmat_is_unitary, mat_mul, mat_svd, mat_hdiag, mat_hdotp
      USE kpoints_module,      ONLY : nkpts, nkpts_g, iks, ike, wbtot
      USE overlap_module,      ONLY : dimwann, Mkb
      USE localization_module, ONLY : maxiter0_wan, maxiter1_wan, alpha0_wan, alpha1_wan,&
                                      ncg, wannier_thr, cu, rave, rave2, r2ave, &
                                      Omega_I, Omega_D, Omega_OD, Omega_tot, &
                                      localization_allocate, localization_write, localization_print, &
                                      a_condmin, niter_condmin, dump_condmin, xcell
      USE hamiltonian_module,  ONLY : hamiltonian_write, hamiltonian_allocate
      USE workspace_wan_module,ONLY : sheet, csheet, domg, dq, dq0, cU0, Mkb0, Mkb_aux, &
                                      workspace_wan_allocate
      USE mp,                  ONLY : mp_sum
      !
      USE parser_module
      USE want_interfaces_module
      !
      IMPLICIT NONE

      !
      ! local variables
      !
      INTEGER     :: ik, ik_g, m, ierr
      INTEGER     :: ncgfix, ncount, iter
      LOGICAL     :: lcg, do_conjgrad
      REAL(dbl)   :: Omega_old, Omega_var, Omega0, OmegaA
      REAL(dbl)   :: gcnorm1, gcnorm0, gcnorm_aux
      REAL(dbl)   :: aux1, aux2, eqb, eqa, alpha, alphamin
      !
      REAL(dbl),    ALLOCATABLE :: rave_aux(:,:) 
      !
      CHARACTER( LEN=nstrx )    :: filename
      !
      ! end of declarations
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
      ! ... Read DFT_DATA file and init global data
      !
      CALL want_dftread ( PSEUDO=read_pseudo )
      CALL want_init    ( INPUT=.TRUE., PSEUDO=read_pseudo )


      !
      ! ... Summary of the input and DFT data
      !
      CALL summary( stdout, WINDOWS=.FALSE., MEMORY=.FALSE. )

      !
      ! ... wannier-specific variables init
      !
      CALL localization_allocate()


      !
      ! ... import overlap and projections from the disentangle stored data
      !
      CALL overlap_extract(dimwann)
      !
      IF (ionode) THEN
          !
          WRITE(stdout,"(/,2x,'Overlaps and projections setup completed')")
          !
      ENDIF
      !
      CALL timing_upto_now(stdout)
      CALL flush_unit(stdout)


      !
      ! ... allocate local variables
      !
      CALL workspace_wan_allocate( )


!
!--------------------------------------------
!...  Init Wannier Functions localization procedure
!--------------------------------------------
! 

      CALL write_header( stdout, "Init localization procedure")
      !
      IF ( ionode ) WRITE( stdout, "()" )
      CALL memusage( stdout )
      !
      CALL flush_unit( stdout )

      !
      ! init many quantities, particularly the cU matrix
      !
      CALL localization_init( localization_init_mode )
      !
      ! Mkb_aux will contain tha updated overlaps, while
      ! Mkb keeps trace of thre starting overlaps
      !
      CALL overlap_update(dimwann, nkpts, cU, Mkb, Mkb_aux)


      sheet(:,:,:)   = ZERO
      csheet(:,:,:)  = CONE

      !
      ! ... Calculate the average positions of the WFs
      !
      CALL omegai( Omega_I, dimwann, nkpts, Mkb_aux )
      CALL omega( dimwann, nkpts, Mkb_aux, csheet, sheet, rave, r2ave, rave2,  &
                  Omega_D, Omega_OD )
                  !
      Omega_tot = Omega_I + Omega_D + Omega_OD 
      !
      CALL localization_print(stdout,FMT="extended")
      !
      IF ( do_polarization ) CALL polarization( dimwann, rave )

      Omega0 = Omega_tot
      Omega_old = ZERO
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
      dq0(:,:)   = CZERO
      gcnorm0    = ONE



!
!----------------------------------------------
!  ... Main iterative loop
!----------------------------------------------
!

      CALL write_header( stdout, "Starting iteration loop" )
      CALL flush_unit( stdout )
      !
      CALL timing('iterations',OPR='start')
      !
      !
      iteration_loop : &
      DO iter = 1, maxiter0_wan + maxiter1_wan

           ncount = iter
           CALL log_push( "iteration" )

           !
           ! Store cU and Mkb_aux
           !
           cU0(:,:,1:nkpts_g)    = cU(:,:,1:nkpts_g)
           Mkb0(:,:,:,1:nkpts)   = Mkb_aux(:,:,:,1:nkpts)

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
                IF ( a_condmin < EPS_m4 ) use_condmin = .FALSE.
           ENDIF


           !
           ! compute the derivative of the functional
           ! Includint penalty functional contributions
           !
           CALL domega( dimwann, nkpts, Mkb_aux, csheet, sheet, rave, &
                        use_condmin, a_condmin, domg )


           !
           ! compute the norm of domg
           !
           gcnorm1 = ZERO
           DO ik = 1, nkpts
               !
               ik_g = ik + iks -1
               gcnorm1 = gcnorm1 + REAL( mat_hdotp( dimwann, 'U', domg(:,ik_g), domg(:,ik_g) ), dbl )
               !
           ENDDO
           !
           CALL timing ( 'mp_sum_wan', OPR='start' )
           CALL mp_sum( gcnorm1 )
           CALL timing ( 'mp_sum_wan', OPR='stop' )


           !
           ! set dq
           !
           dq(:,:) = domg(:,iks:ike) 
           !
           IF ( MOD( (ncount-1), ncg ) /= 0 )  THEN 
                dq(:,:) = dq(:,:) + gcnorm1/gcnorm0 * dq0(:,:)
           ENDIF
           !
           gcnorm0 = gcnorm1
           dq0(:,:) = dq(:,:)

           
           !
           ! auxiliary norm for cg
           !
           gcnorm_aux = ZERO
           DO ik = 1, nkpts
               !
               ik_g = ik + iks -1
               !
               gcnorm_aux = gcnorm_aux -REAL( mat_hdotp( dimwann, 'U', domg(:,ik_g), dq(:,ik) ), dbl )
               !
           ENDDO
           !
           CALL timing ( 'mp_sum_wan', OPR='start' )
           CALL mp_sum( gcnorm_aux )
           CALL timing ( 'mp_sum_wan', OPR='stop' )
           !
           gcnorm_aux = gcnorm_aux * aux1

           !
           ! The SD step is calculated
           !
           aux2 = alpha * aux1
           dq(:,:) = aux2 * dq(:,:)


           !
           ! compute the change in the unitary matrix dU = e^(i * dq)
           ! and update U
           !
           CALL unitary_update( dimwann, nkpts, dq, cU ) 


           !
           ! update the overlap Mkb_aux (from Mkb), according to the new cU
           !
           CALL overlap_update(dimwann, nkpts, cU, Mkb, Mkb_aux)


           !
           ! The functional is recalculated
           !
           CALL omega( dimwann, nkpts, Mkb_aux, csheet, sheet, rave, r2ave, rave2, &
                       Omega_D, Omega_OD )
           Omega_tot = Omega_I + Omega_D + Omega_OD
           !
           OmegaA    = Omega_tot
           Omega_var = Omega_tot - Omega_old


           !
           ! perform Conjugate-Gradinent minimization if the case
           ! 
           IF ( do_conjgrad ) THEN
               !
               ! recover cu and Mkb
               !
               cU(:,:,1:nkpts_g)      = cU0(:,:,1:nkpts_g)
               Mkb_aux(:,:,:,1:nkpts) = Mkb0(:,:,:,1:nkpts)
   
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
               dq(:,:) = aux2 * dq0(:,:)
   

               !
               ! compute the change in the unitary matrix dU = e^(i * dq)
               ! and update U
               !
               CALL unitary_update( dimwann, nkpts, dq, cU ) 


               !
               ! update the overlap Mkb_aux (from Mkb), according to the new dCu
               !
               CALL overlap_update( dimwann, nkpts, cU, Mkb, Mkb_aux)


               !
               ! The functional is recalculated
               !  
               CALL omega( dimwann, nkpts, Mkb_aux, csheet, sheet, rave, r2ave, rave2, &
                           Omega_D, Omega_OD )
               Omega_tot = Omega_I + Omega_D + Omega_OD
           
               OmegaA    = Omega_tot
               Omega_var = Omega_tot - Omega_old
   
               !
               ! end of the optimal alpha paraphernalia 
               !
           ENDIF
   
           Omega_old = Omega_tot
           Omega0    = OmegaA
   

           !
           ! write info to stdout
           !
           IF ( MOD( ncount, nprint_wan ) == 0 .OR. ncount == 1 ) THEN
               !
               IF ( ionode ) THEN
                   !
                   IF ( use_condmin ) THEN 
                       WRITE( stdout,"(/,2x,'Iteration = ',i7,3x, &
                               & '(condit. minim, A = ',f9.4,' )')") ncount, a_condmin
                   ELSE
                       WRITE( stdout,"(/,2x,'Iteration = ',i7) ") ncount
                   ENDIF
                   !
                   CALL localization_print(stdout, FMT="standard" )
                   WRITE( stdout, " (2x,'Omega variation (Bohr^2):  ',f13.6) ") Omega_var
                   !
               ENDIF
               ! 
               CALL timing_upto_now(stdout)
               CALL flush_unit(stdout)
               !
           ENDIF
   
   
           !
           ! write data to disk
           !
           IF ( MOD( ncount, nsave_wan ) == 0 .AND. ionode ) THEN
               !
               CALL io_name('wannier',filename)
               CALL file_open(wan_unit,TRIM(filename),PATH="/",ACTION="write", &
                              FORM=TRIM(wantdata_form), IERR=ierr)
               IF ( ierr/=0 ) CALL errore('wannier', 'opening '//TRIM(filename), ABS(ierr) )
                    !
                    CALL localization_write(wan_unit,"WANNIER_LOCALIZATION")
                    !
               CALL file_close(wan_unit,PATH="/",ACTION="write", IERR=ierr)
               IF ( ierr/=0 ) CALL errore('wannier', 'closing '//TRIM(filename), ABS(ierr) )
               !
           ENDIF
           !
           !     
           CALL log_pop( "iteration" )
           !
           ! convergence condition
           !
           IF ( ABS( Omega_var ) < wannier_thr ) EXIT iteration_loop
           !
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
      IF (ionode) WRITE(stdout, "()")
      CALL timing('iterations',OPR='stop')

      IF ( ncount == maxiter0_wan + maxiter1_wan ) THEN
          CALL write_header( stdout, "Max number of iteration reached" )
      ELSE
          CALL write_header( stdout, "Convergence Achieved" )
      ENDIF
      !
      IF (ionode) WRITE( stdout, "(/,2x,'Iteration # : ',i7,/)") ncount


      !
      ! ordering wannier centers
      !
      IF ( do_ordering ) THEN
           !
           IF (ionode) WRITE( stdout, "(2x,'Wannier function ordering : ',a,/)") TRIM(ordering_mode)
           CALL ordering(dimwann, nkpts, rave, rave2, r2ave, cU, ordering_mode)
           !
      ENDIF

      !
      ! collect WF
      !
      IF ( do_collect_wf ) THEN
          !
          IF ( ionode ) THEN
              !
              WRITE( stdout, "(2x,'Collecting WFs')") 
              WRITE( stdout, "(2x,'Selected cell extrema [cryst. coord]:')") 
              WRITE( stdout,"(  3( 6x, ' r',i1,' :  ', f8.4, ' --> ', f8.4, /),/ )" ) &
                             (m, xcell(m), xcell(m) + ONE, m=1,3 ) 
              !
          ENDIF
          !
          ALLOCATE( rave_aux(3, dimwann), STAT=ierr )
          IF ( ierr/=0 ) CALL errore('wannier','allocating rave_aux',ABS(ierr))
          !
          rave_aux = rave
          !
          CALL collect_wf(dimwann, nkpts, rave_aux, xcell, cU)
          !
          CALL overlap_update( dimwann, nkpts, cU, Mkb, Mkb_aux)
          CALL omega( dimwann, nkpts, Mkb_aux, csheet, sheet, rave, r2ave, rave2, &
                      Omega_D, Omega_OD )
          !
          ! this is needed because of the periodicity of the system
          ! under hte periodic boundary conditions
          !
          rave = rave_aux 
          !
          DEALLOCATE( rave_aux, STAT=ierr )
          IF ( ierr/=0 ) CALL errore('wannier','deallocating rave_aux',ABS(ierr))
          !
      ENDIF

      !
      ! final summary
      !
      CALL localization_print(stdout, FMT="extended")
      !
      IF (ionode) WRITE( stdout, " (2x,'Omega variation (Bohr^2):  ',f13.6,/) ") Omega_var 
      !
      IF ( do_polarization ) CALL polarization( dimwann, rave )
      !
      CALL timing_upto_now(stdout)
      CALL flush_unit(stdout)


      !
      ! ... Unitariery of U matrix is checked
      !
      DO ik = 1, nkpts
          !
          ik_g = ik + iks -1
          !
          IF (  .NOT. zmat_is_unitary( dimwann, dimwann, cu(:,:,ik_g),  &
                                       SIDE='both', TOLL=unitary_thr )  )  &
               CALL warning('wannier', 'U matrix NOT unitary at ikpt = '//TRIM(int2char(ik_g)) )
          !
      ENDDO


      ! 
      ! ... Write the final unitary transformations and all other data referring
      !     to the Wannier localization procedure to a file
      !
      IF ( ionode ) THEN
          !
          CALL io_name('wannier',filename)
          CALL file_open(wan_unit,TRIM(filename),PATH="/",ACTION="write",FORM=TRIM(wantdata_form), IERR=ierr)
          IF ( ierr/=0 ) CALL errore('wannier','opening '//TRIM(filename), ABS(ierr)) 
              !
              CALL localization_write(wan_unit,"WANNIER_LOCALIZATION")
              !
          CALL file_close(wan_unit,PATH="/",ACTION="write", IERR=ierr)
          IF ( ierr/=0 ) CALL errore('wannier','closing '//TRIM(filename), ABS(ierr)) 

          CALL io_name('wannier',filename, LPATH=.FALSE., LPROC=.FALSE.)
          !
          WRITE( stdout,"(/,2x,'Unitary transf. matrixes written on file: ',a)") TRIM(filename)
          WRITE(stdout,"(2x,70('='))")
          !
      ENDIF


      !
      ! ... Convert the Hamiltonian from the bloch basis to the wannier one
      !     and write the results to file
      !
      CALL hamiltonian_allocate()
      CALL hamiltonian_calc(dimwann, nkpts, cu)

      IF ( ionode ) THEN 
          !
          CALL io_name('hamiltonian',filename)
          CALL file_open(ham_unit,TRIM(filename),PATH="/",ACTION="write", &
                                  FORM=TRIM(wantdata_form), IERR=ierr)
          IF ( ierr/=0 ) CALL errore('wannier','opening '//TRIM(filename), ABS(ierr)) 
              !
              CALL hamiltonian_write(ham_unit)
              !
          CALL file_close(ham_unit,PATH="/",ACTION="write", IERR=ierr)
          IF ( ierr/=0 ) CALL errore('wannier','closing '//TRIM(filename), ABS(ierr)) 
          !
          CALL io_name('hamiltonian',filename, LPATH=.FALSE., LPROC=.FALSE.)
          WRITE( stdout,"(/,'  Hamiltonian on WF basis written on file : ',a)") TRIM(filename)
          !
      ENDIF

!
!--------------------------------------
! ...  Shut down
!--------------------------------------
!

      !
      ! global cleanup
      !
      CALL cleanup( )

      !
      !  finalize
      !
      CALL shutdown( 'wannier' )   

END PROGRAM wannier

