! 
! Copyright (C) 2004 WanT Group
!
! This file is distributed under the terms of the
! GNU General Public License. See the file `License'
! in the root directory of the present distribution,
! or http://www.gnu.org/copyleft/gpl.txt .
!
! Based on a previous version by I.Souza, N.Marzari and D.Vanderbilt
! See the file README in the root directory for a full list of credits
!
!=====================================================
   PROGRAM disentangle
   !=====================================================

       USE kinds,            ONLY: dbl
       USE constants,        ONLY: PI, RYD, har => au, bohr => bohr_radius_angs, &
                                   ZERO, ONE, CZERO, CONE, EPS_m8
       USE parameters,       ONLY: nstrx
       USE version_module,   ONLY: version_number
       USE io_module,        ONLY: stdout, ioname, space_unit
       USE files_module,     ONLY: file_open, file_close
       USE timing_module,    ONLY: timing, timing_upto_now, timing_overview, global_list
       USE input_module,     ONLY: input_manager
       USE want_init_module, ONLY: want_init
       USE control_module,   ONLY: subspace_init_mode => subspace_init, verbosity, &
                                   unitary_thr, nprint_dis, nsave_dis
       USE util_module,      ONLY: zmat_unitary, mat_hdiag, mat_mul
       USE kpoints_module,   ONLY: nkpts, vkpt, nb, nnlist
       USE windows_module,   ONLY: dimwin, dimwinx, eig, lcompspace, &
                                   dimfroz, indxnfroz
       USE windows_module,   ONLY: windows_allocate, windows_write
       USE subspace_module,  ONLY: dimwann, wan_eig, lamp, camp, eamp, comp_eamp, &
                                   mtrx_in, mtrx_out
       USE subspace_module,  ONLY: disentangle_thr, maxiter_dis, alpha_dis, &
                                   subspace_allocate, subspace_write
       USE overlap_module,   ONLY: Mkb, overlap_allocate
       USE summary_module,   ONLY: summary

       IMPLICIT NONE

!
! ... local variables
!
       REAL(dbl) :: omega_i, omega_i_save, omega_i_err

       REAL(dbl), ALLOCATABLE :: w(:)
       COMPLEX(dbl), ALLOCATABLE :: ham(:,:,:)
       COMPLEX(dbl), ALLOCATABLE :: z(:,:)
       COMPLEX(dbl), ALLOCATABLE :: Akb_aux(:,:,:,:), Mkb_aux(:,:,:,:)
       CHARACTER(LEN=nstrx) :: filename 

       INTEGER :: i, j, l, m, ierr
       INTEGER :: ik, ib, ikb, iter, ncount

!      
! ...  end of declarations
!      

!
!--------------------------------------------
! ...  Startup
!--------------------------------------------
!
       CALL startup(version_number,'disentangle')

       !      
       ! ...  Read input parameters from DFT_DATA file
       !
       CALL input_manager()

       !
       ! ...  Global data init
       !
       CALL want_init(WANT_INPUT=.TRUE., PSEUDO=.TRUE.)

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
       ALLOCATE( ham(dimwinx,dimwinx,nkpts), STAT=ierr ) 
           IF( ierr /=0 ) CALL errore('disentangle', 'allocating ham', ABS(ierr) )
       ALLOCATE( z(dimwinx,dimwinx), w(dimwinx), STAT = ierr )
           IF( ierr /=0 ) CALL errore('disentangle', 'allocating z, w', ABS(ierr) )
       ALLOCATE( Mkb_aux(dimwann,dimwann,nb,nkpts), STAT=ierr ) 
           IF( ierr /=0 ) CALL errore('disentangle', 'allocating Mkb_aux', ABS(ierr) )
       ALLOCATE( Akb_aux(dimwinx,dimwann,nb,nkpts), STAT=ierr ) 
           IF( ierr /=0 ) CALL errore('disentangle', 'allocating Akb_aux', ABS(ierr) )



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
       CALL subspace_init( subspace_init_mode )
       omega_i_save = ZERO


       iteration_loop : &
       DO iter = 0, maxiter_dis
           ncount = iter


           DO ik = 1, nkpts
               !
               ! ... update Mkb_aux, according to lamp
               !     Mkb_aux = Lamp(k)^{dag} * Mkb(k,b) * Lamp(k+b)
               ! 
               !     Akb     = Mkb * Lamp(ikb)
               !     Mkb_aux = Lamp(ik)^{\dag} * Akb
               !
               DO ib = 1, nb
                    ikb = nnlist(ib, ik)
                    CALL mat_mul(Akb_aux(:,:,ib,ik), Mkb(:,:,ib,ik), 'N',  &
                                 lamp(:,:,ikb), 'N', dimwin(ik), dimwann, dimwin(ikb) )
                    CALL mat_mul(Mkb_aux(:,:,ib,ik), lamp(:,:,ik), 'C', & 
                                 Akb_aux(:,:,ib,ik), 'N', dimwann, dimwann, dimwin(ik) )
               ENDDO
           ENDDO
      

           !
           ! Compute Omega_I using the updated subspaces at all k
           ! 
           CALL omegai( omega_i, dimwann, nkpts, Mkb_aux)
    

           !
           ! write info on stdout
           !
           IF ( ncount > 0 ) THEN
                !
                omega_i_err  = ( omega_i_save - omega_i ) / omega_i  
                !
                WRITE( stdout, " (2x, 'Iteration = ',i5,5x,'Omega_I =',f12.6, &
                                & 6x, 'Error =',f16.8 )") iter, omega_i, omega_i_err

                !
                ! timing
                !
                IF ( MOD(ncount, nprint_dis) == 0 )  THEN
                    WRITE(stdout, "()")
                    CALL timing_upto_now(stdout) 
                ENDIF

                !
                ! write data to disk
                !
                IF ( MOD(ncount, nsave_dis) == 0 )  THEN
                    CALL ioname('space',filename)
                    CALL file_open(space_unit,TRIM(filename),PATH="/",ACTION="write", &
                                   FORM="formatted")
                       CALL windows_write(space_unit,"WINDOWS")
                       CALL subspace_write(space_unit,"SUBSPACE")
                    CALL file_close(space_unit,PATH="/",ACTION="write")
                ENDIF

                !
                ! convergence condition
                !
                IF ( ABS( omega_i_err ) < disentangle_thr ) EXIT iteration_loop
           ENDIF
           !     
           omega_i_save = omega_i


      !
      ! update lamp
      !

           ! 
           ! Construct the new z-matrix mtrx_out at the relevant k-points
           ! 
           DO ik = 1, nkpts
                CALL zmatrix( ik, dimwann, dimwin, dimwinx, Akb_aux(1,1,1,ik), &
                              mtrx_out(1,1,ik), dimfroz(ik), indxnfroz(1,ik) )
           ENDDO

           !
           ! Compute the current z-matrix at each relevant k-point 
           ! using the mixing scheme
           ! 
           IF ( iter == 0 ) THEN
               mtrx_in(:,:,:) = mtrx_out(:,:,:)
               !
           ELSE
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
           ENDIF


           !
           ! update the subspace (lamp) using zmatrix
           !
           DO ik = 1, nkpts
               ! 
               ! Diagonalize z matrix mtrx_in at all relevant k-points
               ! 
               IF ( dimwann > dimfroz(ik) )  THEN
                    !
                    CALL timing('mat_hdiag', OPR='start')
                    !
                    m = dimwin(ik)-dimfroz(ik)
                    CALL mat_hdiag( z(:,:), w(:), mtrx_in(:,:,ik), m)
                    !
                    CALL timing('mat_hdiag', OPR='stop')
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
                        lamp( 1:dimwin(ik), m, ik) = CZERO
                        DO i = 1, dimwin(ik)-dimfroz(ik)
                            lamp( indxnfroz(i,ik), m, ik) = z(i,j) 
                        ENDDO
                   ENDDO
               ENDIF
           ENDDO


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
       IF ( ncount == maxiter_dis ) THEN 
           WRITE( stdout, "(2x,'=',18x,'Max number of iteration reached',19x,'=')")
       ELSE
           WRITE( stdout, "(2x,'=',24x,'Convergence Achieved',24x,'=')" )
       ENDIF
       WRITE( stdout, "(2x,70('='),/)" )


       !
       ! ... Write the final omega_I. This should equal the one given by wannier
       !
       WRITE( stdout, "(2x,'Iteration # : ',i5)") ncount
       WRITE( stdout,"(2x,'Final Omega_I (Bohr^2, Angstrom^2):', f15.6,2x,f15.6)") &
                      omega_i, omega_i*bohr**2
       WRITE( stdout,"(2x,' Avrg Omega_I                     :', f15.6,2x,f15.6)") &
                      omega_i/REAL(dimwann, dbl), omega_i*bohr**2/REAL(dimwann, dbl)
       WRITE( stdout,"()" ) 
       CALL timing_upto_now(stdout) 

     
       !
       ! ... As a description of the found subspace write the norm of each bloch
       !     state projected on the subspace 
       !     in practice we write the dimwann diagonal elements of lamp^{dag}*lamp
       !
       IF ( TRIM(verbosity) == "high" ) THEN
            WRITE( stdout,"(/,2x,'Subspace decomposition:')" ) 
            WRITE( stdout,"(  2x,'Norms of the projected Bloch functions',/)" ) 
            DO ik=1,nkpts
                  WRITE( stdout,"(1x,'!',6x,'kpt =', i3,' ( ',3f6.3,' )    dimwin = ',i4)" ) &
                        ik, vkpt(:,ik), dimwin(ik)
                  CALL mat_mul(z, lamp(:,:,ik), 'N', lamp(:,:,ik), 'C', &
                               dimwin(ik), dimwin(ik), dimwann )
                  WRITE( stdout,"(1x,'!',2x, 8f9.5)") ( REAL(z(i,i)), i=1,dimwin(ik) )
                  WRITE( stdout,"(1x,'!')" ) 
            ENDDO
            WRITE( stdout,"()" ) 
       ENDIF


       ! ...  Diagonalize the hamiltonian within the optimized subspace at each kpoint
       !      in order to re-define eigenvalues and eigenvectors
       !
       DO ik = 1, nkpts
           ! 
           ! check the unitariry of lamp
           !
           IF ( .NOT. zmat_unitary( dimwin(ik), dimwann, lamp(:,:,ik), &
                                    SIDE='left', TOLL=unitary_thr) )&
                 CALL errore('disentangle',"Lamp matrices not orthogonal",ik)

           DO j = 1, dimwann
           DO i = 1, dimwann
               ham(i,j,ik) = CZERO
               DO l = 1, dimwin(ik)
                  ham(i,j,ik) = ham(i,j,ik) + CONJG(lamp(l,i,ik))*lamp(l,j,ik)*eig(l,ik)
               ENDDO
           ENDDO
           ENDDO
           !
           CALL timing('mat_hdiag', OPR='start')
           CALL mat_hdiag(z(:,:), wan_eig(:,ik), ham(:,:,ik), dimwann)
           CALL timing('mat_hdiag', OPR='stop')
 
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
 
       ENDDO 
                  
       !
       ! NOTE: for the purpose of minimizing Omega_D + Omega_OD in wannier.x 
       ! we could have simply
       ! used the lambda eigenvectors as the basis set for the optimal subspace,
       ! i.e., write lamp instead of eamp. However, in order to calculate the
       ! interpolated band structure we need to assume that the unitary rotations
       ! obtained in wannier.f are done starting from the energy eigenvectors.
       ! Of course, if we were only interested in the maxloc WFs, not in the 
       ! interpolated band structure, we could have used lamp. I have check that
       ! the resulting spread and average location of the maxloc WFs are exactly the 
       ! same if we replace eamp by lamp in the write statement below.
       ! 

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


       IF ( lcompspace )  THEN
           !
           ! Diagonalize the hamiltonian in the complement subspace, write the
           ! corresponding eigenfunctions and energy eigenvalues
           !
           DO ik = 1, nkpts

               IF ( dimwin(ik)-dimwann > 0 ) THEN
                   DO j = 1, dimwin(ik)-dimwann
                   DO i = 1, dimwin(ik)-dimwann
                       ham(i,j,ik) = CZERO
                       DO l = 1, dimwin(ik)
                             ham(i,j,ik) = ham(i,j,ik) + CONJG(camp(l,i,ik)) * &
                                                         camp(l,j,ik) * eig(l,ik)
                       ENDDO
                   ENDDO
                   ENDDO

                   m = dimwin(ik)-dimwann
                   !
                   CALL timing('mat_hdiag', OPR='start')
                   CALL mat_hdiag( z(:,:), w(:), ham(:,:,ik), m )
                   CALL timing('mat_hdiag', OPR='stop')
 
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
       CALL ioname('space',filename)
       CALL file_open(space_unit,TRIM(filename),PATH="/",ACTION="write",FORM="formatted")
            CALL windows_write(space_unit,"WINDOWS")
            CALL subspace_write(space_unit,"SUBSPACE")
       CALL file_close(space_unit,PATH="/",ACTION="write")

       CALL ioname('space',filename,LPATH=.FALSE.)
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
       DEALLOCATE( ham, STAT=ierr )
           IF( ierr /=0 ) CALL errore('disentangle', 'deallocating ham', ABS(ierr) )
       DEALLOCATE( z, w, STAT=ierr )
           IF( ierr /=0 ) CALL errore('disentangle', 'deallocating z, w', ABS(ierr) )
       DEALLOCATE( Mkb_aux, Akb_aux, STAT=ierr )
           IF( ierr /=0 ) CALL errore('disentangle', 'deallocating Mkb_ Akb_aux', ABS(ierr) )

       CALL cleanup()

END PROGRAM disentangle

