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

       USE kinds
       USE constants, ONLY: PI, RYD, har => au, bohr => bohr_radius_angs, &
                            ZERO, ONE, CZERO, CONE, EPS_m8
       USE parameters, ONLY : nstrx
       USE io_module, ONLY: stdout, work_dir, &
                            ioname, ovp_unit, space_unit, dft_unit
       USE files_module, ONLY : file_open, file_close
       USE timing_module, ONLY : timing, timing_deallocate, timing_overview, global_list
       USE startup_module, ONLY : startup
       USE cleanup_module, ONLY : cleanup
       USE version_module, ONLY : version_number
       USE input_module
       USE want_init_module, ONLY : want_init
       USE converters_module, ONLY : cart2cry
       USE util_module, ONLY : zmat_unitary, zmat_hdiag
       USE summary_module, ONLY : summary
       USE wfc_data_module, ONLY : wfc_data
       USE iotk_module
    
       USE kpoints_module, ONLY: nkpts, vkpt
       USE kpoints_module, ONLY: mxdnn, mxdnnh, nntot, nnshell, nnlist, nncell, &
                                 neigh, bk, wb, dnn, bka, wbtot
       USE lattice_module, ONLY: avec
       USE windows_module,  ONLY : mxdbnd, dimwin, dimwinx, eiw, imin, imax, lcompspace, &
                                   dimfroz, indxfroz, indxnfroz, lfrozen, frozen
       USE windows_module,  ONLY : windows_allocate, windows_write
       USE subspace_module, ONLY : wan_eig, lamp, camp, eamp, comp_eamp, &
                                   mtrx_in, mtrx_out
       USE subspace_module, ONLY : subspace_allocate, subspace_write
       USE overlap_module,  ONLY : cm, ca, overlap_allocate, &
                                   overlap_read, overlap_write


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
       INTEGER :: nkp, iter
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
       CALL input_read()

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

       ALLOCATE( ham(mxdbnd,mxdbnd,nkpts), STAT=ierr ) 
           IF( ierr /=0 ) CALL errore(' disentangle ', ' allocating ham ',(mxdbnd**2*nkpts))
       ALLOCATE( lamp_tmp(mxdbnd,mxdbnd,nkpts), STAT=ierr )
           IF( ierr /=0 ) &
           CALL errore(' disentangle ', ' allocating lamp_tmp ',(mxdbnd**2*nkpts))
       ALLOCATE( z(mxdbnd,mxdbnd), STAT = ierr )
           IF( ierr /=0 ) CALL errore(' disentangle ', ' allocating z ', (mxdbnd*mxdbnd) )
       ALLOCATE( w(mxdbnd), STAT = ierr )
           IF( ierr /=0 ) CALL errore(' disentangle ', ' allocating w ', mxdbnd )
       ALLOCATE( komega_i_est(nkpts), STAT = ierr )
           IF( ierr /=0 ) CALL errore(' disentangle ', ' allocating komega_i_est ', nkpts )

!
! ... Compute the OVERLAP and PROJECTION matrix elements
!
      CALL wfc_data(lamp_tmp)


!=------------------------------------------------------------------------------------=
!
! ...  Start iteration loop

       DO iter = 1, maxiter
         IF ( iter == 1 ) THEN

! ...    Choose an initial trial subspace at each K

! ...      No frozen states
           IF ( .NOT. lfrozen ) THEN

             IF ( ITRIAL == 1 ) THEN
               WRITE( stdout,"(/,'  Initial trial subspace: lowest energy eigenvectors',/)")
               DO nkp=1, nkpts
                 DO l=1, dimwann
                   DO j=1,dimwin(nkp)
                     lamp(j,l,nkp) = CZERO
                     IF ( j == l ) lamp(j,l,nkp) = CONE
                   END DO
                 END DO
               END DO
             ELSE IF ( itrial == 2 ) THEN
               WRITE( stdout,"(/,'  Initial trial subspace: highest energy eigenvectors',/)")
               DO nkp=1, nkpts
                 DO l=1, dimwann
                   DO j=1, dimwin(nkp)  
                     lamp(j,l,nkp) = CZERO
                     IF ( j == l+dimwin(nkp)-dimwann ) lamp(j,l,nkp) = CONE
                   END DO
                 END DO
               END DO
             ELSE IF ( ITRIAL == 3 ) THEN
               WRITE(stdout,"(/,'  Initial trial subspace: projected localized orbitals',/)")
               lamp(:,:,:) = lamp_tmp(:,:,:)
             ELSE
               WRITE( stdout, fmt= "(/,2x, 'Invalid choice of itrial' )")
               CALL errore(' disentangle ', ' Invalid choice of itrial (I)', (itrial) )

             END IF     !   No frozen states

           ELSE

! ...      There are frozen states. 
!          Choose the non-frozen trial states using the modified projection technique

             WRITE(stdout,"(/,2x, 'There are frozen states',/)")
             WRITE(stdout,"(2x,'Initial trial subspace: projected gaussians+frozen states' &
                   & ,/)")

! ...        First find the dimwmann-dimensional subspace s with maximal overlap onto the
!            dimwann gaussians

             IF ( ITRIAL == 3 ) THEN 
                 lamp(:,:,:) = lamp_tmp(:,:,:)
             ELSE
                  WRITE( stdout, fmt= "(/,2x, 'Invalid choice of itrial' )")
                  CALL errore(' disentangle ', ' Invalid choice of itrial (II)', (itrial) )
             END IF

! ...        Next find the (dimwann-dimfroz(nkp))-dimensional space of non-frozen states
!            with largest overlap with s, and include it in the trial subspace 
!            (put them above the frozen states in lamp)

             CALL projection_frozen( lamp, dimwann, dimwin,     &
                  dimfroz, frozen, nkpts, mxdbnd, nkpts)

! ...        Finally include the frozen states (if any) in the trial 
!            subspace at each k-point (put them at the bottom of lamp)
!            NOTE that this must come last, because calling the subroutine projection.f
!            would override it
!
             DO nkp = 1, nkpts
               IF ( dimfroz(nkp) > 0 ) THEN
                 DO l = 1, dimfroz(nkp)
                   DO j = 1, dimwin(nkp)
                     lamp(j,l,nkp)= CZERO
                   END DO
                   lamp(indxfroz(l,nkp),l,nkp) = CONE
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

           DO nkp = 1, nkpts
               IF ( .NOT. zmat_unitary( lamp(1:dimwin(nkp),1:dimwann,nkp), &
                                  SIDE='left', TOLL=EPS_m8 ) ) &
                   CALL errore(' disentangle ', 'Vectors in lamp not orthonormal',nkp)
           ENDDO

!
! ... Iteration LOOP
!
           WRITE( stdout, "(/,2x,70('='))" ) 
           WRITE( stdout, "(2x,'=',19x,'Starting Iteration loop',26x,'=')" ) 
           WRITE( stdout, "(2x,70('='),/)" ) 

! ...      Compute the initial z matrix mtrx_in at all relevant K-points

           DO nkp = 1, nkpts
             IF ( dimwann > dimfroz(nkp) )  THEN
               CALL zmatrix( nkp, nnlist, nshells, nwhich, nnshell, wb, lamp,     &
                    cm(1,1,1,nkp), mtrx_in(1,1,nkp), dimwann, dimwin,     &
                    dimfroz, indxnfroz, mxdbnd, nkpts, mxdnn )
             END IF
           END DO

         ELSE     !   iter .ne. 1

! ...    Compute the current z-matrix at each relevant K-point using the mixing scheme
 
           DO nkp = 1, nkpts
             IF ( dimwann > dimfroz(nkp) )  THEN
               DO i = 1, dimwin(nkp)-dimfroz(nkp)
                 DO j = 1, i
                   mtrx_in(j,i,nkp) = alpha*mtrx_out(j,i,nkp) + (ONE-alpha)*mtrx_in(j,i,nkp)
                   mtrx_in(i,j,nkp) = conjg(mtrx_in(j,i,nkp))         ! hermiticity
                 END DO
               END DO
             END IF
           END DO
         ENDIF    !   iter = 1
         omega_i_est = ZERO

         DO nkp = 1, nkpts
 
! ...    Diagonalize z matrix mtrx_in at all relevant K-points
 
           IF ( dimwann > dimfroz(nkp) )  THEN
                 dim = dimwin(nkp)-dimfroz(nkp)
                 CALL zmat_hdiag( z(:,:), w(:), mtrx_in(:,:,nkp), dim)
           ENDIF
 
! ...      Calculate K-point contribution to omega_i_est
 
           komega_i_est(nkp) = DBLE(dimwann)*wbtot
 
! ...      Contribution from frozen states (if any)
 
           IF ( dimfroz(nkp) > 0 )  THEN
             DO m = 1, dimfroz(nkp)
 
! ...          Note that at this point lamp for the non-frozen states pertains to the 
!              previous iteration step, which is exactly what we need as an input for 
!              the subroutine lambda_avg
 
               klambda = lambda_avg( m, nkp, lamp, cm(1,1,1,nkp), nnlist, nshells, &
                         nwhich, nnshell, wb, dimwann, dimwin, mxdbnd, nkpts, mxdnn )
               komega_i_est(nkp) = komega_i_est(nkp) - klambda
             END DO
           END IF
 
! ...      Contribution from non-frozen states (if any). 
!          pick the dimwann-dimfroz(nkp) leading eigenvectors of the z-matrix to build the
!          optimal subspace for the next iteration
 
           IF ( dimwann > dimfroz(nkp) )  THEN
             m = dimfroz(nkp)
             DO j = dimwin(nkp)-dimwann+1, dimwin(nkp)-dimfroz(nkp)
               m = m+1
               komega_i_est(nkp) = komega_i_est(nkp) - w(j)
               DO i = 1, dimwin(nkp)
                 lamp(i,m,nkp) = czero
               END DO
               DO i = 1, dimwin(nkp)-dimfroz(nkp)
                 lamp(indxnfroz(i,nkp),m,nkp) = z(i,j)     ! *** CHECK!!! ***
               END DO
             END DO
             IF ( verbosity == 'high' ) THEN
                WRITE(stdout, fmt="(/,2x, 'All eigenvalues:' )")
                DO j = 1, dimwin(nkp)-dimfroz(nkp)
                  WRITE(stdout,fmt="(4x,'j=',i2,', lambda(j)=', f10.5)") j, w(j)
                END DO
                WRITE(stdout,fmt="(4x,'Wbtot = ')")wbtot
             END IF
           ENDIF
 
           OMEGA_I_EST=OMEGA_I_EST+KOMEGA_I_EST(NKP)
 

! ...      At the last iteration find a basis for the (dimwin(nkp)-dimwann)-dimensional
!          complement space
 
           IF ( iter == maxiter )  THEN
             IF ( dimwin(nkp) > dimwann )  THEN
                DO j = 1, dimwin(nkp)-dimwann
                   IF ( dimwann > dimfroz(nkp) )  THEN
 
! ...              Use the non-leading eigenvectors of the z-matrix
 
                      DO i = 1, dimwin(nkp)
                        camp(i,j,nkp) = z(i,j)
                      END DO
                   ELSE        ! dimwann=dimfroz(nkp)
 
! ...              Use the original non-frozen bloch eigenstates
 
                      DO i = 1, dimwin(nkp)
                         camp(i,j,nkp) = czero
                         IF ( i == indxnfroz(j,nkp) )  camp(i,j,nkp) = CONE
                      END DO
                   END IF
                END DO
               
             ELSE
                lcompspace = .FALSE.
                WRITE( stdout, fmt="(/,2x, 'Warning!' )")
                WRITE( stdout, fmt="(4x, 'at k-point ',i4,' the complement subspace '// &
                       & 'has zero dimensions' )") nkp
             END IF
           END IF
 
         END DO ! nkp


         omega_i_est = omega_i_est/DBLE(nkpts)

!        Compute omega_i using the updated subspaces at all K
 
         omega_i = ZERO
         o_error = ZERO
         DO nkp = 1, nkpts
           aux = komegai( nkp, lamp, cm(1,1,1,nkp), wb, wbtot, nnlist, nshells, &
                          nwhich, nnshell, dimwann, dimwin, mxdbnd, nkpts, mxdnn )
           omega_i = omega_i + aux
!          IF ( ( iter - INT (iter / DBLE(10) ) ) == 1 ) THEN
!            WRITE( stdout, fmt=" (4x, 'K-point',i3, ' )     Komega_I Error =',f16.8 )") &
!                   nkp, (komega_i_est(nkp)-aux)/aux
!          END IF
         END DO
         omega_i = omega_i/DBLE(nkpts)
 
         WRITE( stdout, fmt=" (2x, 'Iteration = ',i5,'   Omega_I Error =',f16.8 )") &
                iter, (omega_i_est - omega_i)/omega_i
         o_error = ABS( (OMEGA_I_EST-OMEGA_I)/OMEGA_I )

 
! ...    Construct the new z-matrix mtrx_out at the relevant K-points
   
         DO nkp = 1, nkptS
           IF ( dimwann > dimfroz(nkp) )  THEN
             CALL zmatrix( nkp, nnlist, nshells, nwhich, nnshell, wb, lamp,   &
                  cm(1,1,1,nkp), mtrx_out(1,1,nkp), dimwann, dimwin,  &
                  dimfroz, indxnfroz, mxdbnd, nkpts, mxdnn )
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


! ...  Write the final omega_i. This should equal the one given by wannier
       WRITE( stdout, fmt=" (2x, 'Final Omega_I (Bohr^2, Angstrom^2)', f16.8,2x,f16.8)") &
                      omega_i,omega_i*bohr**2
       WRITE( stdout, "(' ',70('='))" ) 

!
! ...  writing main data related to the obtained subspace on the .SPACES datafile
!      ( IOTK formatted )
 
! ...  Diagonalize the hamiltonian within the optimized subspace at each kpoint
!      in order to re-define eigenvalues and eigenvectors

       DO nkp = 1, nkpts
           DO j = 1, dimwann
           DO i = 1, dimwann
               ham(i,j,nkp) = czero
               DO l = 1, dimwin(nkp)
                  ham(i,j,nkp) = ham(i,j,nkp) + conjg(lamp(l,i,nkp))*lamp(l,j,nkp)*eiw(l,nkp)
               ENDDO
           ENDDO
           ENDDO
           CALL zmat_hdiag(z(:,:), w(:), ham(:,:,nkp), dimwann)
 
           !
           ! ...  Calculate amplitudes of the corresponding energy eigenvectors in terms of 
           !      the original ("window space") energy eigenvectors
           !
 
           eamp(:,:,nkp) = CZERO
           DO j = 1, dimwann
           DO i = 1, dimwin(nkp)
               DO l = 1, dimwann
                    eamp(i,j,nkp) = eamp(i,j,nkp) + z(l,j)*lamp(i,l,nkp)
               END DO
           ENDDO
           ENDDO
 
       ENDDO ! end of nkp loop
       !
       ! ...  Convert the optimal subspace energy eigenvalues in eV 
       !      to be used later on for reconstructing the hamiltonian on the 
       !      Wannier basis
! XXXXXXX
       wan_eig(:,:) = har * wan_eig(:,:)

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
 
!        nkp loop
         DO nkp = 1, nkpts

            IF ( dimwin(nkp)-dimwann > 0 ) THEN
               DO j = 1, dimwin(nkp)-dimwann
               DO i = 1, dimwin(nkp)-dimwann
                   ham(i,j,nkp) = czero
                   DO l = 1, dimwin(nkp)
                      ham(i,j,nkp) = ham(i,j,nkp) + CONJG(camp(l,i,nkp)) * &
                                                    camp(l,j,nkp) * eiw(l,nkp)
                   ENDDO
               ENDDO
               ENDDO

               dim = dimwin(nkp)-dimwann
               CALL zmat_hdiag( z(:,:), w(:), ham(:,:,nkp), dim )
 
               ! ... Calculate amplitudes of the energy eigenvectors in the complement 
               !     subspace in terms of the original energy eigenvectors
               !  
               DO j = 1, dimwin(nkp)-dimwann
               DO i = 1, dimwin(nkp)
                  comp_eamp(i,j,nkp) = CZERO
                  DO l = 1, dimwin(nkp)-dimwann
                     comp_eamp(i,j,nkp) = comp_eamp(i,j,nkp)+z(l,j)*camp(i,l,nkp)
                  ENDDO
               ENDDO
               ENDDO

            ELSE
               comp_eamp(:,:,nkp) = CZERO
            ENDIF

         ENDDO ! end ok nkp loop

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

! XXX sistemare MPI environment
!      CALL mp_end()

!=---------------------------------------------------------------=

       STOP '*** THE END *** (disentangle.x)'
       END

