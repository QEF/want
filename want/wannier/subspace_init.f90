!
! Copyright (C) 2005 WanT Group
!
! This file is distributed under the terms of the
! GNU General Public License. See the file `License'
! in the root directory of the present distribution,
! or http://www.gnu.org/copyleft/gpl.txt .
!
!*******************************************************************
   SUBROUTINE subspace_init( mode )
   !*******************************************************************
   !
   ! ...  Initialize the starting subspace for disentangle minimization
   !      according to the input choice of mode (and frozen windows):
   !
   !      * 'randomized'     self explaining
   !      * 'lower_states'   select the dimwann lower bands
   !      * 'upper_states'   select the dimwann upper bands
   !      * 'center_projections'  uses the CA matrix to extract a subspace
   !
   USE kinds,           ONLY : dbl
   USE constants,       ONLY : CZERO, CONE, EPS_m8
   USE parameters,      ONLY : nstrx
   USE timing_module,   ONLY : timing
   USE io_module,       ONLY : stdout, io_name, wantdata_form, space_unit, ionode
   USE log_module,      ONLY : log_push, log_pop
   USE files_module,    ONLY : file_open, file_close
   USE util_module,     ONLY : zmat_unitary, mat_mul, mat_svd
   !
   USE windows_module,  ONLY : lfrozen, dimfroz, indxfroz, frozen, dimwin, dimwinx
   USE kpoints_module,  ONLY : vkpt_g, nkpts, iks
   USE control_module,  ONLY : unitary_thr, verbosity
   USE subspace_module, ONLY : subspace_read, lamp, dimwann
   USE overlap_module,  ONLY : ca 
   IMPLICIT NONE

   !
   ! input variables
   !
   CHARACTER(*),    INTENT(in)  :: mode      
   !
   ! local variables
   !
   CHARACTER(13)                :: subname="subspace_init"
   CHARACTER(nstrx)             :: filename
   REAL(dbl),       ALLOCATABLE :: s(:)
   COMPLEX(dbl),    ALLOCATABLE :: cu(:,:)
   COMPLEX(dbl),    ALLOCATABLE :: vt(:,:), u(:,:)
   LOGICAL                      :: lfound
   INTEGER                      :: i, j, l, ik, ik_g, ierr

!
!------------------------------
! main body
!------------------------------
!

   CALL timing( subname , OPR='start')
   CALL log_push( subname )

   !
   ! few checks
   IF ( dimwann <=0 ) CALL errore(subname,'invalid dimwann',-dimwann+1)
   !
   ! this may be generalized
   IF ( lfrozen .AND. ( TRIM( mode ) /= 'center_projections' .AND. &
                        TRIM( mode ) /= 'from_file'  )        ) &
        CALL errore(subname,'center projs. are required if frozen states are present',2)


   !
   ! here set LAMP
   !
   SELECT CASE ( TRIM( mode ) )
   CASE DEFAULT
      CALL errore(subname,'Invalid MODE = "'//TRIM(mode)//'"',1)

   CASE ( 'from_file' )

        IF (ionode) WRITE( stdout,"('  Initial trial subspace: from_file')")
        ! 
        CALL io_name('space',filename)
        CALL file_open(space_unit,TRIM(filename),PATH="/",ACTION="read", &
                       FORM=TRIM(wantdata_form), IERR=ierr)
        IF ( ierr/=0 ) CALL errore(subname, 'opening '//TRIM(filename), ABS(ierr))
           !
           CALL subspace_read(space_unit,"SUBSPACE", lfound)
           IF ( .NOT. lfound ) CALL errore(subname,'searching tag "SUBSPACE"',1)
           !
        CALL file_close(space_unit,PATH="/",ACTION="read", IERR=ierr)
        IF ( ierr/=0 ) CALL errore(subname, 'closing '//TRIM(filename), ABS(ierr))
        !
        CALL io_name('space',filename,LPATH=.FALSE.)
        IF (ionode) WRITE( stdout,"(2x,'Subspace data read from file: ',a,/)") TRIM(filename)
        ! 
   CASE ( 'randomized' )

        IF (ionode) WRITE( stdout,"('  Initial trial subspace: random Unit transform',/)")
        !
        DO ik=1, nkpts
            !
            ik_g = ik + iks -1
            !
            CALL random_orthovect(dimwann, dimwin(ik_g), dimwinx, lamp(1,1,ik) )
            !
        ENDDO           
        !   
   CASE ( 'lower_states' )
        !
        IF (ionode) WRITE( stdout,"('  Initial trial subspace: lowest energy eigenvectors',/)")
        !
        DO ik=1, nkpts
            !
            ik_g = ik + iks -1
            !
            DO l=1, dimwann
            DO j=1,dimwin(ik_g)
                !
                lamp(j,l,ik) = CZERO
                IF ( j == l ) lamp(j,l,ik) = CONE
                !
            ENDDO
            ENDDO
        ENDDO           
        !   
   CASE ( 'upper_states' )
        !
        IF (ionode) WRITE( stdout,"('  Initial trial subspace: highest energy eigenvectors',/)")
        !
        DO ik=1, nkpts
            !
            ik_g = ik + iks -1
            !
            DO l=1, dimwann
            DO j=1,dimwin(ik_g)
                !
                lamp(j,l,ik) = CZERO
                IF ( j == l+dimwin(ik_g)-dimwann ) lamp(j,l,ik) = CONE
                !
            ENDDO
            ENDDO
        ENDDO           
        ! 
   CASE ( 'center_projections' )
        !
        IF (ionode) WRITE( stdout,"('  Initial trial subspace: projected localized orbitals')")

        !
        ! If CENTER_PROJECTIONS are selected for the starting subspace
        ! compute the dimwann bloch like states that have maximal projection
        ! on the input trial centers. 
        !
        ! cu = ca * cs^{-1/2},      where cs = ca^{\dag} * ca
        !
        ! Using the singular-value decomposition of the matrix ca:
        !
        ! ca = cz * cd * cv^{\dag}  ,      which gives
        ! cu = cz * cv^{\dag}
        !
        ! where cz is dimwin(ik) x dimwin(ik) and unitary, cd is
        ! dimwin(ik) x dimwann and diagonal, cd^{-1} is dimwann x dimwann and
        ! diagonal, and cv is dimwann x dimwann and unitary.
        ! NOTE that lapack routine returns cv^{\dag} directly
        !
        ALLOCATE( cu(dimwinx, dimwinx), STAT=ierr )
        IF(ierr/=0) CALL errore(subname,'allocating CU',ABS(ierr))
        !
        ALLOCATE( u(dimwinx, dimwinx), STAT=ierr )
        IF(ierr/=0) CALL errore(subname,'allocating U',ABS(ierr))
        !
        ALLOCATE( vt(dimwann, dimwann), STAT=ierr )
        IF(ierr/=0) CALL errore(subname,'allocating VT',ABS(ierr))
        !
        ALLOCATE( s(dimwann), STAT = ierr )
        IF( ierr /= 0 ) CALL errore(subname, 'allocating s ', dimwann )
  
        DO ik=1,nkpts
            !
            ik_g = ik + iks -1
            !
            IF ( dimwann > dimfroz(ik_g) ) THEN
                !
                CALL mat_svd( dimwin(ik_g), dimwann, ca(:,:,ik), s, u, vt )
                !
                CALL mat_mul( cu, u, 'N', vt, 'N', dimwin(ik_g), dimwann, dimwann )
                lamp(  1:dimwin(ik_g), 1:dimwann , ik) = cu( 1:dimwin(ik_g), 1:dimwann )
                !
            ENDIF
            !
        ENDDO

        !
        ! cleaning
        DEALLOCATE( cu, STAT=ierr )
        IF(ierr/=0) CALL errore(subname,'deallocating CU',ABS(ierr))
        !
        DEALLOCATE( u, STAT=ierr )
        IF(ierr/=0) CALL errore(subname,'deallocating U',ABS(ierr))
        !
        DEALLOCATE( s, STAT=ierr )
        IF(ierr/=0) CALL errore(subname,'deallocating S',ABS(ierr))
        !
        DEALLOCATE( vt, STAT=ierr )
        IF(ierr/=0) CALL errore(subname,'deallocating VT',ABS(ierr))

        !
        ! In case of frozen states
        !
        IF ( lfrozen ) THEN
            !
            IF (ionode) WRITE(stdout,"(2x, 'There are frozen states')")
            !
            ! Now find the (dimwann-dimfroz(ik))-dimensional space of non-frozen states
            ! with largest overlap with s, and include it in the trial subspace
            ! (put them above the frozen states in lamp).
            !
            ! NOTA BENE: instead of what happens in projections here no use od wfcs is done
            !            moreover, wfcs have already been deallocated and wasted away
            !
            CALL projection_frozen( lamp, dimwann, dimwin, dimwinx, nkpts, & 
                                    dimfroz, frozen )

            ! Finally include the frozen states (if any) in the trial
            ! subspace at each k-point (put them at the bottom of lamp)
            ! NOTE that this must come last, because calling the subroutine projection.f
            ! would override it
            !
            DO ik = 1, nkpts
                !
                ik_g = ik + iks -1
                !
                IF ( dimfroz(ik_g) > 0 ) THEN
                    !
                    DO l = 1, dimfroz(ik_g)
                       DO j = 1, dimwin(ik_g)
                           lamp(j,l,ik)= CZERO
                       ENDDO
                       lamp(indxfroz(l,ik_g),l,ik) = CONE
                    ENDDO
                    !
                ENDIF
            ENDDO
        ENDIF
        !
        IF (ionode) WRITE(stdout,"()")

   END SELECT

   !
   ! Finally check that the states in the columns of the final matrix lamp are orthonormal
   ! at every k-point, i.e. that the matrix is unitary in the sense that
   !    lamp^{\dag} * lamp = 1 
   ! but not 
   !    lamp * lamp^{\dag} = 1
   !
   ! In particular, this checks whether the projected gaussians are indeed
   ! orthogonal to the frozen states, at those k-points where both are present in
   ! the trial subspace.
   !
   ! the subroutine zmat_unitary is called using SIDE='left' to perform A^{\dag}.A
   ! while it should be SIDE = 'right' to perform A.A^{\dag}
   !
   DO ik = 1, nkpts
       !
       ik_g = ik + iks -1
       !
       IF ( .NOT. zmat_unitary( dimwin(ik_g), dimwann, lamp(:,:,ik), &
                                SIDE='left', TOLL=unitary_thr ) ) &
       CALL errore(subname, 'Vectors in lamp not orthonormal',ik_g)
       !
   ENDDO

   IF ( TRIM(verbosity) == "high" .AND. ionode ) THEN
       !
       ALLOCATE(cu(dimwinx,dimwinx), STAT=ierr)
       IF(ierr/=0) CALL errore(subname,'allocating cu (II)',ABS(ierr))
       !   
       WRITE( stdout,"(/,2x,'Subspace decomposition:')" )
       WRITE( stdout,"(  2x,'Norms of the projected Bloch functions',/)" )
       !
       DO ik=1,nkpts
           !
           ik_g = ik + iks -1
           !
           WRITE(stdout,"(1x,'!',6x,'kpt =', i3, ' ( ',3f6.3,' )    dimwin = ', i4)" ) &
                 ik, vkpt_g(:,ik_g), dimwin(ik_g)
           CALL mat_mul(cu, lamp(:,:,ik), 'N', lamp(:,:,ik), 'C', &
                         dimwin(ik_g), dimwin(ik_g), dimwann )
           !
           WRITE(stdout,"(1x,'!',2x, 8f9.5)") ( REAL(cu(i,i)), i=1,dimwin(ik_g) )
           WRITE(stdout,"(1x,'!')" )
           !
       ENDDO
       WRITE( stdout,"(2/)" )
       !
       DEALLOCATE( cu, STAT=ierr)
       IF(ierr/=0) CALL errore(subname,'deallocating cu (II)',ABS(ierr))
       !
   ENDIF


   CALL timing( subname , OPR='stop')
   CALL log_pop( subname )

END SUBROUTINE subspace_init

