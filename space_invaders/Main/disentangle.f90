! 
! Copyright (C) 2004 WanT Group
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
       USE timing_module,  ONLY : timing, timing_upto_now, timing_overview, global_list
       USE input_module,   ONLY : input_manager
       USE want_init_module, ONLY : want_init
       USE control_module, ONLY : start_mode_dis, verbosity, unitary_thr, &
                                  nprint_dis, nsave_dis
       USE util_module,    ONLY : zmat_unitary, zmat_hdiag
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
       USE overlap_module,  ONLY : Mkb, ca, overlap_allocate
       USE summary_module, ONLY : summary

       IMPLICIT NONE

!
! ... local variables
!
       EXTERNAL  :: komegai
       REAL(dbl) :: komegai
       EXTERNAL  :: lambda_avg
       REAL(dbl) :: lambda_avg

       REAL(dbl) :: omega_i, omega_i_est
       REAL(dbl) :: omega_err
       REAL(dbl) :: aux

       REAL(dbl), ALLOCATABLE :: komega_i_est(:)
       REAL(dbl), ALLOCATABLE :: w(:)
       COMPLEX(dbl), ALLOCATABLE :: ham(:,:,:)
       COMPLEX(dbl), ALLOCATABLE :: z(:,:)
       CHARACTER(LEN=nstrx) :: filename 
       CHARACTER(LEN=nstrx) :: attr

       INTEGER :: i, j, l, m, ierr
       INTEGER :: ik, iter, ncount

!      
! ...  end of declarations
!      

!
!--------------------------------------------
! ...  Startup
!--------------------------------------------
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
       !
       CALL subspace_allocate()
       CALL overlap_allocate()


       !
       ! ...  Local allocations
       !
       ALLOCATE( ham(nbnd,nbnd,nkpts), STAT=ierr ) 
           IF( ierr /=0 ) CALL errore('disentangle', 'allocating ham', ABS(ierr) )
       ALLOCATE( z(nbnd,nbnd), STAT = ierr )
           IF( ierr /=0 ) CALL errore('disentangle', 'allocating z', ABS(ierr) )
       ALLOCATE( w(nbnd), STAT = ierr )
           IF( ierr /=0 ) CALL errore('disentangle', 'allocating w', ABS(ierr) )
       ALLOCATE( komega_i_est(nkpts), STAT = ierr )
           IF( ierr /=0 ) CALL errore('disentangle', 'allocating komega_i_est', ABS(ierr) )



!
!--------------------------------------------
! ...  Compute the OVERLAP and PROJECTION matrix elements
!--------------------------------------------
!
       WRITE(stdout, "(2/,2x,70('='))" )
       WRITE(stdout, "(2x,'=',22x,'Overlaps and Projections',22x,'=')" )
       WRITE(stdout, "(2x,70('='))" )

       CALL wfc_manager()
     


!
!--------------------------------------------
! ...  Start iteration loop
!--------------------------------------------
!
       CALL timing('iterations',OPR='START')
       !
       !
       WRITE( stdout, "(/,2x,70('='))" ) 
       WRITE( stdout, "(2x,'=',22x,'Starting Iteration loop',22x,'=')" ) 
       WRITE( stdout, "(2x,70('='),/)" ) 

      
       !
       ! ...  Initialize the starting subspace
       !
       CALL subspace_init(start_mode_dis, dimwann, dimwin, dimwinx, nkpts, ca, lamp)


       iteration_loop : &
       DO iter = 1, maxiter_dis
           ncount = iter


           ! 
           ! ... Construct the new z-matrix mtrx_out at the relevant K-points
           ! 
           DO ik = 1, nkpts
                IF ( dimwann > dimfroz(ik) )  THEN
                    CALL zmatrix( ik, lamp, Mkb(1,1,1,ik), mtrx_out(1,1,ik), dimwann, &
                                  dimwin, dimwinx, dimfroz(ik), indxnfroz(1,ik), nbnd )
                ENDIF
           ENDDO



           !
           ! Compute the current z-matrix at each relevant K-point 
           ! using the mixing scheme
           ! 
           IF ( iter == 1 ) THEN
                mtrx_in(:,:,:) = mtrx_out(:,:,:)
           ENDIF
           !
           DO ik = 1, nkpts
                IF ( dimwann > dimfroz(ik) )  THEN
                    DO i = 1, dimwin(ik)-dimfroz(ik)
                    DO j = 1, i
                         mtrx_in(j,i,ik) = alpha_dis * mtrx_out(j,i,ik) + &
                                           (ONE-alpha_dis) * mtrx_in(j,i,ik)
                         !
                         ! use hermiticity
                         !
                         mtrx_in(i,j,ik) = CONJG(mtrx_in(j,i,ik))     
                    ENDDO
                    ENDDO
                ENDIF
           ENDDO


           !
           ! body of the loop
           !
           DO ik = 1, nkpts
               ! 
               ! Diagonalize z matrix mtrx_in at all relevant k-points
               ! 
               IF ( dimwann > dimfroz(ik) )  THEN
                    !
                    CALL timing('zmat_hdiag', OPR='start')
                    !
                    m = dimwin(ik)-dimfroz(ik)
                    CALL zmat_hdiag( z(:,:), w(:), mtrx_in(:,:,ik), m)
                    !
                    CALL timing('zmat_hdiag', OPR='stop')
               ENDIF

               ! 
               !  Calculate k-point contribution to omega_i_est
               komega_i_est(ik) = DBLE(dimwann)*wbtot
 

               ! Contribution from frozen states (if any)
               ! 
               IF ( dimfroz(ik) > 0 )  THEN
                   ! 
                   ! Note that at this point lamp for the non-frozen states pertains 
                   ! to the previous iteration step, which is exactly what we need 
                   ! as an input for the subroutine komegai
                   ! 
                   aux = komegai( ik, dimfroz(ik), dimwann, dimwin, dimwinx,  &
                                  lamp, Mkb(1,1,1,ik))
                   komega_i_est(ik) = komega_i_est(ik) + aux
               ENDIF
               ! 
               ! Contribution from non-frozen states (if any). 
               ! pick the dimwann-dimfroz(ik) leading eigenvectors of the z-matrix 
               ! to build the optimal subspace for the next iteration
               ! 
               IF ( dimwann > dimfroz(ik) )  THEN
                   m = dimfroz(ik)
                   DO j = dimwin(ik)-dimwann+1, dimwin(ik)-dimfroz(ik)
                        m = m+1
                        komega_i_est(ik) = komega_i_est(ik) - w(j)
                        DO i = 1, dimwin(ik)
                            lamp(i,m,ik) = CZERO
                        ENDDO
                        DO i = 1, dimwin(ik)-dimfroz(ik)
                            lamp(indxnfroz(i,ik),m,ik) = z(i,j)     ! *** CHECK!!! ***
                        ENDDO
                   ENDDO
               ENDIF
 
            ENDDO
            omega_i_est = SUM( komega_i_est(1:nkpts) ) /DBLE(nkpts)


           
            !
            ! ... Compute omega_i using the updated subspaces at all k
            ! 
            omega_i = DBLE(nkpts * dimwann) * wbtot
            DO ik = 1, nkpts
                aux = komegai( ik, dimwann, dimwann, dimwin, dimwinx, lamp, Mkb(1,1,1,ik) )
                omega_i = omega_i + aux
            ENDDO
            omega_i = omega_i/DBLE(nkpts)
            omega_err = ( omega_i_est -omega_i ) / omega_i 
    

            !
            ! write info on stdout
            !
            WRITE( stdout, fmt=" (2x, 'Iteration = ',i5,'   Omega_I =',f16.8, &
                         & 4x, 'Error =',f16.8 )") iter, omega_i_est, omega_err

            IF ( MOD(ncount, nprint_dis) == 0 )  THEN
                 WRITE(stdout, "()")
                 CALL timing_upto_now(stdout) 
            ENDIF


            !
            ! write data to disk
            !
            IF ( MOD(ncount, nsave_dis) == 0 )  THEN
                 CALL ioname('subspace',filename)
                 CALL file_open(space_unit,TRIM(filename),PATH="/",ACTION="write", &
                                FORM="formatted")
                    CALL windows_write(space_unit,"WINDOWS")
                    CALL subspace_write(space_unit,"SUBSPACE")
                 CALL file_close(space_unit,PATH="/",ACTION="write")
            ENDIF


            !
            ! convergence condition
            !
            IF ( ABS( omega_err ) < disentangle_thr ) EXIT iteration_loop
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
       CALL timing('iterations',OPR='STOP')

       WRITE( stdout, "(/,2x,70('='))" )
       IF ( ncount == maxiter_dis ) THEN 
           WRITE( stdout, "(2x,'=',18x,'Max number of iteration reached',19x,'=')")
       ELSE
           WRITE( stdout, "(2x,'=',24x,'Convergence Achieved',24x,'=')" )
       ENDIF
       WRITE( stdout, "(2x,70('='),/)" )


       !
       ! ...  Write the final omega_i. This should equal the one given by wannier
       !
       WRITE( stdout,"(2x,'Final Omega_I (Bohr^2, Angstrom^2)', f16.8,2x,f16.8)") &
                      omega_i, omega_i*bohr**2
       WRITE( stdout,"()" ) 
       CALL timing_upto_now(stdout) 


!
!--------------------------------------
! ...  writing main data related to the obtained subspace on the .SPACES datafile
!      ( IOTK formatted ) after some postprocessing on subspace quantities
!--------------------------------------
!
      
       DO ik= 1, nkpts
           !
           ! find a basis for the (dimwin(ik)-dimwann)-dimensional
           ! complement space
           ! 
           camp(:,:,ik) = CZERO
           IF ( dimwin(ik) > dimwann )  THEN
               DO j = 1, dimwin(ik)-dimwann
                   IF ( dimwann > dimfroz(ik) )  THEN
                       ! 
                       !  Use the non-leading eigenvectors of the z-matrix
                       ! 
                       DO i = 1, dimwin(ik)
                            camp(i,j,ik) = z(i,j)
                       ENDDO
                   ELSE   
                       !
                       ! dimwann=dimfroz(ik) 
                       ! Use the original non-frozen bloch eigenstates
                       ! 
                       DO i = 1, dimwin(ik)
                           camp(i,j,ik) = CZERO
                           IF ( i == indxnfroz(j,ik) )  camp(i,j,ik) = CONE
                       ENDDO
                   ENDIF
               ENDDO
               
            ELSE
               lcompspace = .FALSE.
            ENDIF
       ENDDO

       ! ...  Diagonalize the hamiltonian within the optimized subspace at each kpoint
       !      in order to re-define eigenvalues and eigenvectors
       !
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

       !
       ! Note: for the purpose of minimizing Omegatld in wannier.f we could have simply
       ! have used the lambda eigenvectors as the basis set for the optimal subspace,
       ! i.e., write lamp instead of eamp. However, in order to calculate the
       ! interpolated band structure we need to assume that the unitary rotations
       ! obtained in wannier.f are done starting from the energy eigenvectors.
       ! Of course, if we were only interested in the maxloc WFs, not in the 
       ! interpolated band structure, we could have used lamp. I have check that
       ! the resulting spread and average location of the maxloc WFs are exactly the 
       ! same if we replace eamp by lamp in the write statement below.
 
       IF ( lcompspace )  THEN
           !
           ! Diagonalize the hamiltonian in the complement subspace, write the
           ! corresponding eigenfunctions and energy eigenvalues
           !
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

                   m = dimwin(ik)-dimwann
                   CALL zmat_hdiag( z(:,:), w(:), ham(:,:,ik), m )
 
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
           ENDDO 
       ENDIF

       !
       ! ...  actual writing procedures
       ! 
       CALL ioname('subspace',filename)
       CALL file_open(space_unit,TRIM(filename),PATH="/",ACTION="write",FORM="formatted")
            CALL windows_write(space_unit,"WINDOWS")
            CALL subspace_write(space_unit,"SUBSPACE")
       CALL file_close(space_unit,PATH="/",ACTION="write")

       CALL ioname('subspace',filename,LPATH=.FALSE.)
       WRITE( stdout,"(/,2x,'Subspace data written on file: ',a)") TRIM(filename)


!
!--------------------------------------
! ...  Shut down
!--------------------------------------
!
       WRITE( stdout,"(2x, 70('='))")

       !
       ! ...  Finalize timing
       !
       CALL timing('disentangle',OPR='stop')
       CALL timing_overview(stdout,LIST=global_list,MAIN_NAME='disentangle')

       !
       ! ...  Deallocate local arrays
       !
       DEALLOCATE( komega_i_est, STAT=ierr )
           IF( ierr/=0 ) CALL errore('disentangle', 'deallocating k_omega_i_est',ABS(ierr) )

       DEALLOCATE( z, STAT=ierr )
           IF( ierr /=0 ) CALL errore('disentangle', 'deallocating z', ABS(ierr) )
       DEALLOCATE( w, STAT=ierr )
           IF( ierr /=0 ) CALL errore('disentangle', 'deallocating w', ABS(ierr) )

       CALL cleanup()

   STOP '*** THE END *** (disentangle.x)'
   END PROGRAM disentangle

