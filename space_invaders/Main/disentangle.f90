!
! Copyright (C) 2004 Arrigo Calzolari, Carlo Cavazzoni, Marco Buongiorno Nardelli
! Copyright (C) 2002 Nicola Marzari, Ivo Souza, David Vanderbilt
!
! This file is distributed under the terms of the
! GNU General Public License. See the file `License'
! in the root directory of the present distribution,
! or http://www.gnu.org/copyleft/gpl.txt .
!
!=----------------------------------------------------------------------------------=
       PROGRAM disentangle
!=----------------------------------------------------------------------------------=

       USE kinds,          ONLY: dbl
       USE constants,      ONLY: PI, RYD, har => au, bohr => bohr_radius_angs, &
                                 ZERO, ONE, CZERO, CONE, EPS_m8
       USE parameters,     ONLY: nstrx
       USE version_module, ONLY: version_number
       USE startup_module, ONLY: startup
       USE cleanup_module, ONLY: cleanup
       USE io_module,      ONLY: stdout, work_dir, &
                                 ioname, ovp_unit, space_unit, dft_unit
       USE files_module,   ONLY : file_open, file_close
       USE timing_module,  ONLY : timing, timing_deallocate, timing_overview, global_list
       USE input_module,   ONLY : input_manager
       USE control_module, ONLY : trial_mode, verbosity, unitary_thr
       USE want_init_module,  ONLY : want_init
       USE util_module,    ONLY : zmat_unitary, zmat_hdiag
       USE wfc_manager_module, ONLY : wfc_manager
       USE iotk_module
    
       USE kpoints_module, ONLY: nkpts, vkpt
       USE kpoints_module, ONLY: nnx, nnshell, nnlist, wb, wbtot, nshells, nwhich
       USE lattice_module, ONLY: avec
       USE windows_module,  ONLY : nbnd, dimwin, dimwinx, eig, imin, imax, lcompspace, &
                                   dimfroz, indxfroz, indxnfroz, lfrozen, frozen
       USE windows_module,  ONLY : windows_allocate, windows_write
       USE subspace_module, ONLY : dimwann, wan_eig, lamp, camp, eamp, comp_eamp, &
                                   mtrx_in, mtrx_out
       USE subspace_module, ONLY : disentangle_thr, maxiter_dis, alpha_dis, &
                                   subspace_allocate, subspace_write
       USE overlap_module,  ONLY : cm, overlap_allocate
       USE summary_module, ONLY : summary


       IMPLICIT NONE

       EXTERNAL  :: komegai
       EXTERNAL  :: lambda_avg
       REAL(dbl) :: lambda_avg

       CHARACTER(LEN=nstrx) :: filename 
       CHARACTER(LEN=nstrx) :: attr

       REAL(dbl) :: klambda
       REAL(dbl) :: omega_i, omega_i_est, komegai
       REAL(dbl) :: o_error
       REAL(dbl) :: aux, sgn

       REAL(dbl), ALLOCATABLE :: komega_i_est(:)

       COMPLEX(dbl), ALLOCATABLE :: ham(:,:,:)
       COMPLEX(dbl), ALLOCATABLE :: lamp_tmp(:,:,:)
       COMPLEX(dbl), ALLOCATABLE :: z(:,:)
       REAL(dbl), ALLOCATABLE :: w(:)

       INTEGER :: info, m, dim
       INTEGER :: i, j, l, i1, i2, i3 
       INTEGER :: ik, iter
       INTEGER :: nt, ja
       INTEGER :: ngx, ngy, ngz
       INTEGER :: ngm
       INTEGER :: nwann

       INTEGER :: ierr
       INTEGER   :: idum
       REAL(dbl) :: rdum

!      
! ...  End declarations and dimensions
!      
!=----------------------------------------------------------------------------=!

!
! ...  Startup
!
       CALL startup(version_number,MAIN_NAME='disentangle')

!      
! ...  Read input parameters from DFT_DATA file
!
       CALL input_manager()

!
! ...  Global data init
!
       CALL want_init(WANT_INPUT=.TRUE., WINDOWS=.TRUE., BSHELLS=.TRUE.)

!
! ...  Summary of the input and DFT data
!
       CALL summary( stdout )

! 
! ...  other allocations

       CALL subspace_allocate()
       CALL overlap_allocate()


!
! ...  Local allocations

       ALLOCATE( ham(nbnd,nbnd,nkpts), STAT=ierr ) 
           IF( ierr /=0 ) CALL errore(' disentangle ', ' allocating ham ',(nbnd**2*nkpts))
       ALLOCATE( lamp_tmp(dimwinx,dimwinx,nkpts), STAT=ierr )
           IF( ierr /=0 ) &
           CALL errore(' disentangle ', ' allocating lamp_tmp ',(dimwinx**2*nkpts))
       ALLOCATE( z(nbnd,nbnd), STAT = ierr )
           IF( ierr /=0 ) CALL errore(' disentangle ', ' allocating z ', (nbnd*nbnd) )
       ALLOCATE( w(nbnd), STAT = ierr )
           IF( ierr /=0 ) CALL errore(' disentangle ', ' allocating w ', nbnd )
       ALLOCATE( komega_i_est(nkpts), STAT = ierr )
           IF( ierr /=0 ) CALL errore(' disentangle ', ' allocating komega_i_est ', nkpts )

!
! ... Compute the OVERLAP and PROJECTION matrix elements
!
      CALL wfc_manager(lamp_tmp)


!=------------------------------------------------------------------------------------=
!
! ...  Start iteration loop
       CALL timing('iterations',OPR='START')

       DO iter = 1, maxiter_dis
         IF ( iter == 1 ) THEN

! ...    Choose an initial trial subspace at each K

! ...      No frozen states
           IF ( .NOT. lfrozen ) THEN

             IF ( TRIM(trial_mode) == 'lower_states' ) THEN
               WRITE( stdout,"(/,'  Initial trial subspace: lowest energy eigenvectors',/)")
               DO ik=1, nkpts
                 DO l=1, dimwann
                   DO j=1,dimwin(ik)
                     lamp(j,l,ik) = CZERO
                     IF ( j == l ) lamp(j,l,ik) = CONE
                   END DO
                 END DO
               END DO
             ELSE IF ( TRIM(trial_mode) == 'upper_states' ) THEN
               WRITE( stdout,"(/,'  Initial trial subspace: highest energy eigenvectors',/)")
               DO ik=1, nkpts
                 DO l=1, dimwann
                   DO j=1, dimwin(ik)  
                     lamp(j,l,ik) = CZERO
                     IF ( j == l+dimwin(ik)-dimwann ) lamp(j,l,ik) = CONE
                   END DO
                 END DO
               END DO
             ELSE IF ( TRIM(trial_mode) == 'center_projections' ) THEN
               WRITE(stdout,"(/,'  Initial trial subspace: projected localized orbitals',/)")
               lamp(:,:,:) = lamp_tmp(:,:,:)
             ELSE
               CALL errore(' disentangle ', ' Invalid trial (I) = '//TRIM(trial_mode), 1 )

             END IF     !   No frozen states

           ELSE

! ...      There are frozen states. 
!          Choose the non-frozen trial states using the modified projection technique

             WRITE(stdout,"(/,2x, 'There are frozen states',/)")
             WRITE(stdout,"(2x,'Initial trial subspace: projected gaussians+frozen states' &
                   & ,/)")

! ...        First find the dimwmann-dimensional subspace s with maximal overlap onto the
!            dimwann gaussians

             IF ( TRIM(trial_mode) == 'center_projections' ) THEN 
                 lamp(:,:,:) = lamp_tmp(:,:,:)
             ELSE
                  CALL errore(' disentangle ', 'Invalid trial = '//TRIM(trial_mode), 2)
             ENDIF

! ...        Next find the (dimwann-dimfroz(ik))-dimensional space of non-frozen states
!            with largest overlap with s, and include it in the trial subspace 
!            (put them above the frozen states in lamp).
!           NOTA BENE: instead of what happens in projections here no use od wfcs is done
!                      moreover, wfcs have already been deallocated and wasted away

             CALL projection_frozen( lamp, dimwann, dimwin, dimwinx,  &
                  dimfroz, frozen, nkpts, nbnd)

! ...        Finally include the frozen states (if any) in the trial 
!            subspace at each k-point (put them at the bottom of lamp)
!            NOTE that this must come last, because calling the subroutine projection.f
!            would override it
!
             DO ik = 1, nkpts
               IF ( dimfroz(ik) > 0 ) THEN
                 DO l = 1, dimfroz(ik)
                   DO j = 1, dimwin(ik)
                     lamp(j,l,ik)= CZERO
                   END DO
                   lamp(indxfroz(l,ik),l,ik) = CONE
                 END DO
               END IF
             END DO

           ENDIF ! there are frozen states

! ...        Check that the states in the columns of the final matrix lamp are orthonormal
!            at every k-point (i.e., that the matrix is unitary in the sense that
!            conjg(lamp) . lamp = 1 - but not lamp . conjg(lamp) = 1 - )
!            In particular, this checks whether the projected gaussians are indeed 
!            orthogonal to the frozen states, at those k-points where both are present in
!            the trial subspace.
!
!            the subroutine zmat_unitary is called using SIDE='left' to perform A^{\dag}.A
!            while it should be SIDE = 'right' to perform A.A^{\dag}
!

           DO ik = 1, nkpts
               IF ( .NOT. zmat_unitary( lamp(1:dimwin(ik),1:dimwann,ik), &
                                  SIDE='left', TOLL=unitary_thr ) ) &
                   CALL errore(' disentangle ', 'Vectors in lamp not orthonormal',ik)
           ENDDO

!
! ... Iteration LOOP
!
           WRITE( stdout, "(/,2x,70('='))" ) 
           WRITE( stdout, "(2x,'=',19x,'Starting Iteration loop',26x,'=')" ) 
           WRITE( stdout, "(2x,70('='),/)" ) 

! ...      Compute the initial z matrix mtrx_in at all relevant K-points

           DO ik = 1, nkpts
             IF ( dimwann > dimfroz(ik) )  THEN
               CALL zmatrix( ik, nnlist, nshells, nwhich, nnshell, wb, lamp,     &
                    cm(1,1,1,ik), mtrx_in(1,1,ik), dimwann, dimwin, dimwinx,     &
                    dimfroz, indxnfroz, nbnd, nkpts, nnx )
             END IF
           END DO

         ELSE     !   iter .ne. 1

! ...    Compute the current z-matrix at each relevant K-point using the mixing scheme
 
           DO ik = 1, nkpts
             IF ( dimwann > dimfroz(ik) )  THEN
               DO i = 1, dimwin(ik)-dimfroz(ik)
                 DO j = 1, i
                   mtrx_in(j,i,ik) = alpha_dis * mtrx_out(j,i,ik) + &
                                     (ONE-alpha_dis) * mtrx_in(j,i,ik)
                   mtrx_in(i,j,ik) = conjg(mtrx_in(j,i,ik))         ! hermiticity
                 END DO
               END DO
             END IF
           END DO
         ENDIF    !   iter = 1
         omega_i_est = ZERO

         DO ik = 1, nkpts
 
! ...    Diagonalize z matrix mtrx_in at all relevant K-points
 
           IF ( dimwann > dimfroz(ik) )  THEN
                 dim = dimwin(ik)-dimfroz(ik)
                 CALL zmat_hdiag( z(:,:), w(:), mtrx_in(:,:,ik), dim)
           ENDIF
 
! ...      Calculate K-point contribution to omega_i_est
 
           komega_i_est(ik) = DBLE(dimwann)*wbtot
 
! ...      Contribution from frozen states (if any)
 
           IF ( dimfroz(ik) > 0 )  THEN
             DO m = 1, dimfroz(ik)
 
! ...          Note that at this point lamp for the non-frozen states pertains to the 
!              previous iteration step, which is exactly what we need as an input for 
!              the subroutine lambda_avg
 
               klambda = lambda_avg( m, ik, lamp, cm(1,1,1,ik), nnlist, nshells, &
                         nwhich, nnshell, wb, dimwann, dimwin, dimwinx, nkpts, nnx )
               komega_i_est(ik) = komega_i_est(ik) - klambda
             END DO
           END IF
 
! ...      Contribution from non-frozen states (if any). 
!          pick the dimwann-dimfroz(ik) leading eigenvectors of the z-matrix to build the
!          optimal subspace for the next iteration
 
           IF ( dimwann > dimfroz(ik) )  THEN
             m = dimfroz(ik)
             DO j = dimwin(ik)-dimwann+1, dimwin(ik)-dimfroz(ik)
               m = m+1
               komega_i_est(ik) = komega_i_est(ik) - w(j)
               DO i = 1, dimwin(ik)
                 lamp(i,m,ik) = czero
               END DO
               DO i = 1, dimwin(ik)-dimfroz(ik)
                 lamp(indxnfroz(i,ik),m,ik) = z(i,j)     ! *** CHECK!!! ***
               END DO
             END DO
             IF ( verbosity == 'high' ) THEN
                WRITE(stdout, fmt="(/,2x, 'All eigenvalues:' )")
                DO j = 1, dimwin(ik)-dimfroz(ik)
                  WRITE(stdout,fmt="(4x,'j=',i2,', lambda(j)=', f10.5)") j, w(j)
                END DO
                WRITE(stdout,fmt="(4x,'Wbtot = ')")wbtot
             END IF
           ENDIF
 
           omega_i_est=omega_i_est+komega_i_est(ik)
 

! ...      At the last iteration find a basis for the (dimwin(ik)-dimwann)-dimensional
!          complement space
 
           IF ( iter == maxiter_dis )  THEN
             IF ( dimwin(ik) > dimwann )  THEN
                DO j = 1, dimwin(ik)-dimwann
                   IF ( dimwann > dimfroz(ik) )  THEN
 
! ...              Use the non-leading eigenvectors of the z-matrix
 
                      DO i = 1, dimwin(ik)
                        camp(i,j,ik) = z(i,j)
                      ENDDO
                   ELSE        ! dimwann=dimfroz(ik)
 
! ...              Use the original non-frozen bloch eigenstates
 
                      DO i = 1, dimwin(ik)
                         camp(i,j,ik) = czero
                         IF ( i == indxnfroz(j,ik) )  camp(i,j,ik) = CONE
                      END DO
                   END IF
                END DO
               
             ELSE
                lcompspace = .FALSE.
                WRITE( stdout, fmt="(/,2x, 'Warning!' )")
                WRITE( stdout, fmt="(4x, 'at k-point ',i4,' the complement subspace '// &
                       & 'has zero dimensions' )") ik
             END IF
           END IF
 
         END DO ! ik


         omega_i_est = omega_i_est/DBLE(nkpts)

!        Compute omega_i using the updated subspaces at all K
 
         omega_i = ZERO
         o_error = ZERO
         DO ik = 1, nkpts
           aux = komegai( ik, lamp, cm(1,1,1,ik), wb, wbtot, nnlist, nshells, &
                          nwhich, nnshell, dimwann, dimwin, dimwinx, nkpts, nnx )
           omega_i = omega_i + aux
         END DO
         omega_i = omega_i/DBLE(nkpts)
 
         WRITE( stdout, fmt=" (2x, 'Iteration = ',i5,'   Omega_I =',f16.8, &
                              & 4x, 'Error =',f16.8 )") iter, omega_i_est, &
                              (omega_i_est - omega_i)/omega_i
         o_error = ABS( (OMEGA_I_EST-OMEGA_I)/OMEGA_I )

 
! ...    Construct the new z-matrix mtrx_out at the relevant K-points
   
         DO ik = 1, nkptS
           IF ( dimwann > dimfroz(ik) )  THEN
             CALL zmatrix( ik, nnlist, nshells, nwhich, nnshell, wb, lamp,   &
                  cm(1,1,1,ik), mtrx_out(1,1,ik), dimwann, dimwin, dimwinx,  &
                  dimfroz, indxnfroz, nbnd, nkpts, nnx )
           END IF
         END DO

         IF ( o_error < disentangle_thr ) GO TO 9999

       ENDDO ! iter
!
! ...  End of iter loop
!=----------------------------------------------------------------------------------------=

! ...  Convergence achieved

 9999  continue
       WRITE( stdout, fmt="(/,2x, 'Convergence achieved!!',/)")
       CALL timing('iterations',OPR='STOP')


! ...  Write the final omega_i. This should equal the one given by wannier
       WRITE( stdout, fmt=" (2x, 'Final Omega_I (Bohr^2, Angstrom^2)', f16.8,2x,f16.8)") &
                      omega_i,omega_i*bohr**2
       WRITE( stdout, "(' ',70('='))" ) 

!
! ...  writing main data related to the obtained subspace on the .SPACES datafile
!      ( IOTK formatted )
 
! ...  Diagonalize the hamiltonian within the optimized subspace at each kpoint
!      in order to re-define eigenvalues and eigenvectors

       DO ik = 1, nkpts
           DO j = 1, dimwann
           DO i = 1, dimwann
               ham(i,j,ik) = CZERO
               DO l = 1, dimwin(ik)
                  ham(i,j,ik) = ham(i,j,ik) + conjg(lamp(l,i,ik))*lamp(l,j,ik)*eig(l,ik)
               ENDDO
           ENDDO
           ENDDO
           CALL zmat_hdiag(z(:,:), wan_eig(:,ik), ham(:,:,ik), dimwann)
 
           !
           ! ...  Calculate amplitudes of the corresponding energy eigenvectors in terms of 
           !      the original ("window space") energy eigenvectors
           !
 
           eamp(:,:,ik) = CZERO
           DO j = 1, dimwann
           DO i = 1, dimwin(ik)
               DO l = 1, dimwann
                    eamp(i,j,ik) = eamp(i,j,ik) + z(l,j)*lamp(i,l,ik)
               END DO
           ENDDO
           ENDDO
 
       ENDDO ! end of ik loop

! ...  Note: for the purpose of minimizing Omegatld in wannier.f we could have simply
!      have used the lambda eigenvectors as the basis set for the optimal subspace,
!      i.e., write lamp instead of eamp. However, in order to calculate the
!      interpolated band structure we need to assume that the unitary rotations
!      obtained in wannier.f are done starting from the energy eigenvectors.
!      Of course, if we were only interested in the maxloc WFs, not in the 
!      interpolated band structure, we could have used lamp. I have check that
!      the resulting spread and average location of the maxloc WFs are exactly the 
!      same if we replace eamp by lamp in the write statement below.
 
       IF ( .NOT. lcompspace )  THEN
         WRITE(stdout,"(/,2x,'Warning')")
         WRITE(stdout,"('  at some k-point(s) complement subspace has zero dimensionality')")
       ELSE
 
! ...  Diagonalize the hamiltonian in the complement subspace, write the
!      corresponding eigenfunctions and energy eigenvalues
 
!        ik loop
         DO ik = 1, nkpts

            IF ( dimwin(ik)-dimwann > 0 ) THEN
               DO j = 1, dimwin(ik)-dimwann
               DO i = 1, dimwin(ik)-dimwann
                   ham(i,j,ik) = czero
                   DO l = 1, dimwin(ik)
                      ham(i,j,ik) = ham(i,j,ik) + CONJG(camp(l,i,ik)) * &
                                                    camp(l,j,ik) * eig(l,ik)
                   ENDDO
               ENDDO
               ENDDO

               dim = dimwin(ik)-dimwann
               CALL zmat_hdiag( z(:,:), w(:), ham(:,:,ik), dim )
 
               ! ... Calculate amplitudes of the energy eigenvectors in the complement 
               !     subspace in terms of the original energy eigenvectors
               !  
               comp_eamp(:,:,ik) = CZERO
               DO j = 1, dimwin(ik)-dimwann
               DO i = 1, dimwin(ik)
                  DO l = 1, dimwin(ik)-dimwann
                     comp_eamp(i,j,ik) = comp_eamp(i,j,ik)+z(l,j)*camp(i,l,ik)
                  ENDDO
               ENDDO
               ENDDO

            ELSE
               comp_eamp(:,:,ik) = CZERO
            ENDIF

         ENDDO ! end ok ik loop

       ENDIF    ! lcompspace

!
! ...  actual writing procedures
       
       CALL ioname('subspace',filename)
       CALL file_open(space_unit,TRIM(filename),PATH="/",ACTION="write",FORM="formatted")
            CALL windows_write(space_unit,"WINDOWS")
            CALL subspace_write(space_unit,"SUBSPACE")
       CALL file_close(space_unit,PATH="/",ACTION="write")

       CALL ioname('subspace',filename,LPATH=.FALSE.)
       WRITE( stdout,"(/,'  Subspace data written on file: ',a)") TRIM(filename)

!
! ...  Finalize timing

       CALL timing('disentangle',OPR='stop')
       CALL timing_overview(stdout,LIST=global_list,MAIN_NAME='disentangle')

!
! ...  Deallocate local arrays

       DEALLOCATE( komega_i_est, STAT=ierr )
           IF( ierr /=0 ) &
           CALL errore(' disentangle ', ' deallocating k_omega_i_est ', ABS(ierr) )
       DEALLOCATE( lamp_tmp, STAT=ierr )
           IF (ierr/=0)  CALL errore('disentangle', 'deallocating LAMP_TMP', ABS(ierr))

       DEALLOCATE( z, STAT=ierr )
           IF( ierr /=0 ) CALL errore(' disentangle ', ' deallocating z ', ABS(ierr) )
       DEALLOCATE( w, STAT=ierr )
           IF( ierr /=0 ) CALL errore(' disentangle ', ' deallocating w ', ABS(ierr) )

       CALL cleanup()

       STOP '*** THE END *** (disentangle.x)'
       END

