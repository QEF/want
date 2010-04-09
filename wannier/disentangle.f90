! 
! Copyright (C) 2004 WanT Group
!
! This file is distributed under the terms of the
! GNU General Public License. See the file `License'
! in the root directory of the present distribution,
! or http://www.gnu.org/copyleft/gpl.txt .
!
! Based on a previous version by I.Souza, N.Marzari and D.Vanderbilt
! See the CREDITS file in the ~want directory for a full description
!
!=====================================================
   PROGRAM disentangle
   !=====================================================

       USE kinds,                 ONLY : dbl
       USE parameters,            ONLY : nstrx
       USE constants,             ONLY : bohr => BOHR_RADIUS_ANGS, ZERO, ONE, CZERO, CONE
       USE version_module,        ONLY : version_number
       USE io_module,             ONLY : stdout, ionode, io_name, space_unit, wantdata_form
       USE log_module,            ONLY : log_push, log_pop
       USE files_module,          ONLY : file_open, file_close
       USE timing_module,         ONLY : timing, timing_upto_now
       USE control_module,        ONLY : subspace_init_mode => subspace_init, verbosity, &
                                         unitary_thr, nprint_dis, nsave_dis, read_pseudo, &
                                         read_symmetry
       USE util_module,           ONLY : zmat_is_unitary, mat_hdiag, mat_mul
       USE kpoints_module,        ONLY : nkpts, nkpts_g, iks, ike, vkpt_g, &
                                         nb, nnlist, nnpos, nnrev
       USE windows_module,        ONLY : imin, dimwin, dimwinx, eig, dimfroz, indxnfroz
       USE windows_module,        ONLY : windows_allocate, windows_write
       USE subspace_module,       ONLY : dimwann, wan_eig, lamp, eamp
       USE subspace_module,       ONLY : disentangle_thr, maxiter_dis, alpha_dis, &
                                         subspace_allocate, subspace_write
       USE workspace_dis_module,  ONLY : mtrx_in, mtrx_out, Mkb_aux, Akb_aux, &
                                         workspace_dis_allocate
       USE overlap_module,        ONLY : Mkb, overlap_allocate
       USE paratools_module,      ONLY : para_poolrecover
       USE mp,                    ONLY : mp_sum
       USE want_interfaces_module
       !
       IMPLICIT NONE

!
! ... local variables
!
       CHARACTER(11)             :: subname='disentangle'
       !
       REAL(dbl),    ALLOCATABLE :: w(:)
       COMPLEX(dbl), ALLOCATABLE :: ham(:,:)
       COMPLEX(dbl), ALLOCATABLE :: z(:,:)
       CHARACTER(LEN=nstrx)      :: filename 
       !
       REAL(dbl)  :: omega_i, omega_i_save, omega_i_err
       INTEGER    :: i, j, l, m, ierr
       INTEGER    :: ik, ik_g, inn, ipos, ib, ikb_g, iter, ncount

!      
! ...  end of declarations
!      

!
!--------------------------------------------
! ...  Startup
!--------------------------------------------
!
       CALL startup(version_number,"disentangle")

       !      
       ! ...  Read input parameters 
       !
       CALL input_manager()

       !
       ! ...  Read DFT_DATA file and init global data
       !
       CALL want_dftread ( PSEUDO=read_pseudo, SYMMETRY=read_symmetry )
       CALL want_init    ( INPUT=.TRUE., PSEUDO=read_pseudo )

       !
       ! ...  Summary of the input and DFT data
       !
       CALL summary( stdout )

       ! 
       ! ...  other allocations
       !
       CALL subspace_allocate( LEIG=.FALSE., LEAMP=.FALSE.)
       CALL overlap_allocate()
       !
       CALL workspace_dis_allocate()


       !
       ! ...  Local allocations
       !
       ALLOCATE( ham(dimwinx,dimwinx), STAT=ierr ) 
       IF( ierr /=0 ) CALL errore(subname, 'allocating ham', ABS(ierr) )
       !
       ALLOCATE( z(dimwinx,dimwinx), w(dimwinx), STAT = ierr )
       IF( ierr /=0 ) CALL errore(subname, 'allocating z, w', ABS(ierr) )

!
!--------------------------------------------
! ...  Compute the OVERLAP and PROJECTION matrix elements
!--------------------------------------------
!
       CALL write_header( stdout, "Overlaps and Projections" )
       CALL flush_unit( stdout )
       !
       CALL wfc_manager()
       !
       CALL memusage( stdout )

!
!--------------------------------------------
! ...  Start iteration loop
!--------------------------------------------
!
       CALL timing('iterations',OPR='START')
       !
       CALL write_header( stdout, "Starting Iteration loop" )
       CALL flush_unit( stdout )

      
       !
       ! ...  Initialize the starting subspace
       !
       CALL subspace_init( subspace_init_mode )
       omega_i_save = ZERO


       iteration_loop : &
       DO iter = 0, maxiter_dis
           !
           ncount = iter
           !
           CALL log_push( "iteration" )


           DO ik = 1, nkpts
               !
               ! ... update Mkb_aux, according to lamp
               !     Mkb_aux = Lamp(k)^{dag} * Mkb(k,b) * Lamp(k+b)
               ! 
               !     Akb     = Mkb * Lamp(ikb)
               !     Mkb_aux = Lamp(ik)^{\dag} * Akb
               !
               ik_g = ik +iks -1
               !
               DO inn = 1, nb/2
                   !
                   DO ipos = 1, 2 
                      !
                      SELECT CASE( ipos )   
                      CASE( 1 )
                        ib = nnpos( inn )  
                      CASE( 2 )
                        ib = nnrev( nnpos( inn ) )
                      END SELECT   
                      ! 
                      ikb_g = nnlist(ib, ik_g) 

                      !
                      !  A^k,b     =  M^kb * Lamp^(k+b)
                      !
                      CALL mat_mul(Akb_aux(:,:,ib,ik), Mkb(:,:,ib,ik), 'N',  &
                                   lamp(:,:,ikb_g), 'N', dimwin(ik_g), dimwann, dimwin(ikb_g) )

                   ENDDO
                   !
                   ! Here we compute the updated overlap integrals (M^kb_aux)
                   ! but only for positive b vectors
                   !
                   ib = nnpos( inn )  
                   !
                   ! M^kb_aux  =  Lamp^k^dag * A^kb = Lamp^k^dag * M^kb * Lamp^(k+b)
                   !
                   CALL mat_mul(Mkb_aux(:,:,inn,ik), lamp(:,:,ik_g), 'C', & 
                                Akb_aux(:,:,ib,ik), 'N', dimwann, dimwann, dimwin(ik_g) )
                   !
               ENDDO
               !
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
                IF (ionode ) WRITE( stdout, " (2x, 'Iteration = ',i5,5x,'Omega_I =',f12.6, &
                                & 6x, 'Error =',f16.8 )") iter, omega_i, omega_i_err

                !
                ! timing
                !
                IF ( MOD(ncount, nprint_dis) == 0 )  THEN
                    !
                    IF ( ionode ) WRITE(stdout, "()")
                    CALL timing_upto_now( stdout ) 
                    CALL flush_unit ( stdout )
                    !
                ENDIF

                !
                ! write data to disk
                !
                IF ( MOD(ncount, nsave_dis) == 0 .AND. ionode )  THEN
                    !
                    CALL io_name('space',filename)
                    CALL file_open(space_unit,TRIM(filename),PATH="/",ACTION="write", &
                                   FORM=TRIM(wantdata_form), IERR=ierr )
                    IF ( ierr/=0 ) CALL errore(subname, 'opening '//TRIM(filename), ABS(ierr) )
                       !
                       CALL windows_write(space_unit,"WINDOWS")
                       CALL subspace_write(space_unit,"SUBSPACE")
                       !
                    CALL file_close(space_unit,PATH="/",ACTION="write", IERR=ierr)
                    IF ( ierr/=0 ) CALL errore(subname, 'closing '//TRIM(filename), ABS(ierr) )
                    !
                ENDIF

                !
                ! convergence condition
                !
                IF ( ABS( omega_i_err ) < disentangle_thr ) THEN
                     !
                     CALL log_pop ( 'iteration' )
                     EXIT iteration_loop
                     !
                ENDIF
           ENDIF
           !     
           omega_i_save = omega_i


      !
      ! update lamp
      !

           ! 
           ! Construct the new z-matrix mtrx_out at the relevant k-points
           ! 
           CALL log_push( 'zmatrix' )
           !
           DO ik = 1, nkpts
               !
               ik_g = ik + iks -1
               !
               CALL zmatrix( ik_g, dimwann, dimwin, dimwinx, Akb_aux(1,1,1,ik), &
                             mtrx_out(1,1,ik), dimfroz(ik_g), indxnfroz(1,ik_g) )
               !
           ENDDO
           !
           CALL log_pop ( 'zmatrix' )


           !
           ! Compute the current z-matrix at each relevant k-point 
           ! using the mixing scheme
           ! 
           IF ( iter == 0 ) THEN
               !
               mtrx_in(:,:,:) = mtrx_out(:,:,:)
               !
           ELSE
               ! 
               DO ik = 1, nkpts
                   !
                   ik_g = ik + iks -1
                   !
                   IF ( dimwann > dimfroz(ik_g) )  THEN
                       !
                       DO i = 1, dimwin(ik_g)-dimfroz(ik_g)
                       DO j = 1, i
                           !
                           mtrx_in(j,i,ik) = alpha_dis * mtrx_out(j,i,ik) + &
                                             (ONE-alpha_dis) * mtrx_in(j,i,ik)
                           !
                           ! use hermiticity
                           !
                           mtrx_in(i,j,ik) = CONJG( mtrx_in(j,i,ik) )     
                           !
                       ENDDO
                       ENDDO
                       !
                   ENDIF
               ENDDO
               ! 
           ENDIF


           !
           ! update the subspace (lamp) using zmatrix
           !
           CALL log_push( 'lamp' )
           !
           !
           !
           DO ik_g = 1, nkpts_g

               !
               ! first nullify lamp for kpts out of the current pool
               !
               IF ( ik_g < iks .OR. ik_g > ike ) THEN
                   !
                   lamp( :, :, ik_g ) = CZERO
                   CYCLE
                   !
               ENDIF

               !
               ! NOTE: this part of the loop is done only in the local pool
               !
               ik = ik_g -iks +1
               ! 
               ! Diagonalize z matrix mtrx_in at all relevant k-points
               ! 
               IF ( dimwann > dimfroz(ik_g) )  THEN
                   !
                   CALL timing('mat_hdiag', OPR='start')
                   !
                   m = dimwin(ik_g)-dimfroz(ik_g)
                   CALL mat_hdiag( z(:,:), w(:), mtrx_in(:,:,ik), m)
                   !
                   CALL timing('mat_hdiag', OPR='stop')
                   !
               ENDIF

               ! 
               ! Contribution from non-frozen states (if any). 
               ! pick the dimwann-dimfroz(ik) leading eigenvectors of the z-matrix 
               ! to build the optimal subspace for the next iteration
               ! 
               IF ( dimwann > dimfroz(ik_g) )  THEN
                   !
                   m = dimfroz(ik_g)
                   !
                   DO j = dimwin(ik_g)-dimwann+1, dimwin(ik_g)-dimfroz(ik_g)
                       !
                       m = m+1
                       lamp( 1:dimwin(ik_g), m, ik_g) = CZERO
                       !
                       DO i = 1, dimwin(ik_g)-dimfroz(ik_g)
                           !
                           lamp( indxnfroz(i,ik_g), m, ik_g) = z(i,j) 
                           !
                       ENDDO
                       !
                   ENDDO
                   !
               ENDIF
               !
           ENDDO
           !
           ! take care of parallelism
           !
           CALL timing( 'mp_sum_lamp', OPR='start')
           !
           DO ik_g = 1, nkpts_g
               CALL mp_sum( lamp(:,:,ik_g))
           ENDDO
           !
           CALL timing( 'mp_sum_lamp', OPR='stop')
           !
           !CALL para_poolrecover( lamp )

           !
           !
           CALL log_pop ( 'lamp' )


           CALL log_pop ( 'iteration')
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

       IF ( ncount == maxiter_dis ) THEN 
           CALL write_header( stdout, 'Max number of iteration reached' )
       ELSE
           CALL write_header( stdout, 'Convergence Achieved' )
       ENDIF


       !
       ! ... Write the final omega_I. This should equal the one given by wannier
       !
       IF ( ionode ) THEN 
           !
           WRITE( stdout, "(2x,'Iteration # : ',i5)") ncount
           WRITE( stdout,"(2x,'Final Omega_I (Bohr^2, Angstrom^2):', f15.6,2x,f15.6)") &
                         omega_i, omega_i*bohr**2
           WRITE( stdout,"(2x,' Avrg Omega_I                     :', f15.6,2x,f15.6)") &
                         omega_i/REAL(dimwann, dbl), omega_i*bohr**2/REAL(dimwann, dbl)
           WRITE( stdout,"()" ) 
           !
           !
       ENDIF
       !
       CALL timing_upto_now(stdout) 
       CALL flush_unit(stdout) 

     
       !
       ! ... As a description of the found subspace write the norm of each bloch
       !     state projected on the subspace 
       !     in practice we write the dimwann diagonal elements of lamp^{dag}*lamp
       !
       IF ( TRIM(verbosity) == "high" .AND. ionode ) THEN
            !
            WRITE( stdout,"(/,2x,'Subspace decomposition:')" ) 
            WRITE( stdout,"(  2x,'Norms of the projected Bloch functions',/)" ) 
            !
            DO ik_g = 1, nkpts_g
                !
                WRITE( stdout,"(1x,'!',6x,'kpt =', i5,' ( ',3f6.3,' )    dimwin = ',i4)") &
                      ik_g, vkpt_g(:,ik_g), dimwin(ik_g)

                CALL mat_mul(z, lamp(:,:,ik_g), 'N', lamp(:,:,ik_g), 'C', &
                             dimwin(ik_g), dimwin(ik_g), dimwann )

                WRITE( stdout,"(1x,'!',2x, 8f9.5)") ( REAL(z(i,i)), i=1,dimwin(ik_g) )
                WRITE( stdout,"(1x,'!')" ) 
                !
            ENDDO
            WRITE( stdout,"()" ) 
            !
       ENDIF


       !
       ! Diagonalize the hamiltonian within the optimized subspace at each kpoint
       ! in order to re-define eigenvalues and eigenvectors
       !
       ! allocate eamp
       CALL subspace_allocate( LEIG=.TRUE., LEAMP=.TRUE. )
     
       !
       ! nullify eamp and wan_eig for all global kpts
       !
       eamp( :, :, 1:nkpts_g ) = CZERO
       wan_eig( :, 1:nkpts_g ) = ZERO
       !
       DO ik = 1, nkpts
           !
           ik_g = ik +iks - 1
           !
           ! check the unitariry of lamp
           !
           IF ( .NOT. zmat_is_unitary( dimwin(ik_g), dimwann, lamp(:,:,ik_g), &
                                       SIDE='left', TOLL=unitary_thr ) )&
                 CALL errore(subname,"Lamp matrices not orthogonal",ik_g)

           DO j = 1, dimwann
           DO i = 1, dimwann
               !
               ham(i,j) = CZERO
               DO l = 1, dimwin(ik_g)
                   !
                   ham(i,j) = ham(i,j) + &
                                 CONJG(lamp(l,i,ik_g)) * lamp(l,j,ik_g) * eig(imin(ik_g)+l-1,ik_g)
                   !
               ENDDO
               !
           ENDDO
           ENDDO
           !
           CALL timing('mat_hdiag', OPR='start')
           CALL mat_hdiag(z(:,:), wan_eig(:,ik_g), ham(:,:), dimwann)
           CALL timing('mat_hdiag', OPR='stop')
 
           !
           ! ...  Calculate amplitudes of the corresponding energy eigenvectors in terms of 
           !      the original ("window space") energy eigenvectors
           !
           eamp(:,:,ik_g) = CZERO
           !
           DO j = 1, dimwann
           DO i = 1, dimwin(ik_g)
               !
               DO l = 1, dimwann
                   eamp(i,j,ik_g) = eamp(i,j,ik_g) + z(l,j)*lamp(i,l,ik_g)
               ENDDO
               !
           ENDDO
           ENDDO
           ! 
       ENDDO 
       !
       ! get rid of parallelism
       !
       CALL para_poolrecover( wan_eig )
       CALL para_poolrecover( eamp )
                  
       !
       ! NOTE: for the purpose of minimizing Omega_D + Omega_OD in wannier.x 
       ! we could have simply
       ! used the lambda eigenvectors as the basis set for the optimal subspace,
       ! i.e., write lamp instead of eamp. However, in order to calculate the
       ! interpolated band structure we need to assume that the unitary rotations
       ! obtained in wannier.f90 are done starting from the energy eigenvectors.
       ! Of course, if we were only interested in the maxloc WFs, not in the 
       ! interpolated band structure, we could have used lamp. I have check that
       ! the resulting spread and average location of the maxloc WFs are exactly the 
       ! same if we replace eamp by lamp in the write statement below.
       ! 

!
!--------------------------------------
! ...  writing main data related to the obtained subspace on the .SPACE datafile
!      ( IOTK formatted ) after some postprocessing on subspace quantities
!--------------------------------------
!

       !
       ! ...  actual writing procedures
       ! 
       IF ( ionode ) THEN 
           !
           CALL io_name('space',filename)
           CALL file_open(space_unit,TRIM(filename),PATH="/",ACTION="write", &
                          FORM=TRIM(wantdata_form), IERR=ierr)
           IF ( ierr/=0 ) CALL errore(subname,'opening '//TRIM(filename), ABS(ierr))
                !
                CALL windows_write(space_unit,"WINDOWS")
                CALL subspace_write(space_unit,"SUBSPACE")
                !
           CALL file_close(space_unit,PATH="/",ACTION="write", IERR=ierr)
           IF ( ierr/=0 ) CALL errore(subname,'closing '//TRIM(filename), ABS(ierr))

           CALL io_name('space',filename,LPATH=.FALSE.)
           WRITE( stdout,"(/,2x,'Subspace data written on file: ',a)") TRIM(filename)
           !
       ENDIF


!
!--------------------------------------
! ...  Shut down
!--------------------------------------
!

       !
       ! Cleanup of local memory
       !
       DEALLOCATE( ham, STAT=ierr )
       IF( ierr /=0 ) CALL errore(subname, 'deallocating ham', ABS(ierr) )
       !
       DEALLOCATE( z, w, STAT=ierr )
       IF( ierr /=0 ) CALL errore(subname, 'deallocating z, w', ABS(ierr) )

       !
       ! global cleanup
       !
       CALL cleanup()

       !
       ! finalize
       !
       CALL shutdown( subname )

END PROGRAM disentangle

