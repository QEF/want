!
! Copyright (C) 2005 WanT Group
!
! This file is distributed under the terms of the
! GNU General Public License. See the file `License'
! in the root directory of the present distribution,
! or http://www.gnu.org/copyleft/gpl.txt .
!
!*******************************************************************
   SUBROUTINE subspace_init( trial_mode, dimwann, dimwin, dimwinx, nkpts, ca, lamp)
   !*******************************************************************
   !
   ! ...  Initialize the starting subspace for disentangle minimization
   !      according to the input choice of trial_mode (and frozen windows):
   !
   !      * 'lower_states'   select the dimwann lower bands
   !      * 'upper_states'   select the dimwann upper bands
   !      * 'center_projections'  uses the CA matrix to extract a subspace
   !
   USE kinds, ONLY : dbl
   USE constants, ONLY : CZERO, CONE, EPS_m8
   USE timing_module, ONLY : timing
   USE io_module, ONLY : stdout
   USE util_module, ONLY : zmat_unitary, zmat_mul, mat_svd
   !
   USE windows_module, ONLY : lfrozen, dimfroz, indxfroz, frozen, nbnd
   USE control_module, ONLY : unitary_thr
   IMPLICIT NONE

   CHARACTER(*),    INTENT(in)  :: trial_mode      
   INTEGER,         INTENT(in)  :: dimwann, dimwinx, nkpts
   INTEGER,         INTENT(in)  :: dimwin(nkpts)
   COMPLEX(dbl),    INTENT(in)  :: ca(dimwinx,dimwann,nkpts)
   COMPLEX(dbl),    INTENT(out) :: lamp(dimwinx,dimwinx,nkpts)


   !
   ! local variables
   !
   CHARACTER(13)                :: subname="subspace_init"
   REAL(dbl),       ALLOCATABLE :: s(:)
   COMPLEX(dbl),    ALLOCATABLE :: cu(:,:)
   COMPLEX(dbl),    ALLOCATABLE :: vt(:,:), u(:,:)
   INTEGER                      :: i, j, l, ik, ierr, info


!
!---------------------------------------------------------------------


   CALL timing('subapce_init', OPR='start')

   !
   ! few checks
   IF ( dimwann <=0 ) CALL errore(subname,'invalid dimwann',-dimwann+1)
   IF ( dimwinx /= MAXVAL(dimwin) ) CALL errore(subname,'invalid dimwinx',ABS(dimwinx)+1)
   !
   ! this may be generalized
   IF ( lfrozen .AND. TRIM( trial_mode ) == 'center_projections' ) &
        CALL errore(subname,'center proj. are required if frozen states are present',2)


   !
   ! here set LAMP
   !
   SELECT CASE ( TRIM( trial_mode ) )
   CASE DEFAULT
      CALL errore(subname,'Invalid TRIAL_MODE = "'//TRIM(trial_mode)//'"',1)

   CASE ( 'lower_states' )

        WRITE( stdout,"(/,'  Initial trial subspace: lowest energy eigenvectors',/)")
        DO ik=1, nkpts
            DO l=1, dimwann
            DO j=1,dimwin(ik)
                lamp(j,l,ik) = CZERO
                IF ( j == l ) lamp(j,l,ik) = CONE
            ENDDO
            ENDDO
        ENDDO           
           
   CASE ( 'upper_states' )

        WRITE( stdout,"(/,'  Initial trial subspace: highest energy eigenvectors',/)")
        DO ik=1, nkpts
            DO l=1, dimwann
            DO j=1,dimwin(ik)
                lamp(j,l,ik) = CZERO
                IF ( j == l+dimwin(ik)-dimwann ) lamp(j,l,ik) = CONE
            ENDDO
            ENDDO
        ENDDO           
   
   CASE ( 'center_projections' )
        WRITE( stdout,"(/,'  Initial trial subspace: projected localized orbitals')")

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
        ALLOCATE( u(dimwinx, dimwinx), STAT=ierr )
            IF(ierr/=0) CALL errore(subname,'allocating U',ABS(ierr))
        ALLOCATE( vt(dimwann, dimwann), STAT=ierr )
            IF(ierr/=0) CALL errore(subname,'allocating VT',ABS(ierr))
        ALLOCATE( s(dimwann), STAT = ierr )
            IF( ierr /= 0 ) CALL errore(subname, 'allocating s ', dimwann )
  
        DO ik=1,nkpts

           lamp(:,:,ik) = CZERO
           IF ( dimwann > dimfroz(ik) ) THEN
                !
                CALL mat_svd( dimwin(ik), dimwann, ca(:,:,ik), s, u, vt )
                !
                CALL zmat_mul( cu, u, 'N', vt, 'N', dimwin(ik), dimwann, dimwann )
                lamp(  1:dimwin(ik), 1:dimwann , ik) = cu( 1:dimwin(ik), 1:dimwann )
           ENDIF
        ENDDO

        !
        ! cleaning
        DEALLOCATE( cu, STAT=ierr )
            IF(ierr/=0) CALL errore(subname,'deallocating CU',ABS(ierr))
        DEALLOCATE( u, STAT=ierr )
            IF(ierr/=0) CALL errore(subname,'deallocating U',ABS(ierr))
        DEALLOCATE( s, STAT=ierr )
            IF(ierr/=0) CALL errore(subname,'deallocating S',ABS(ierr))
        DEALLOCATE( vt, STAT=ierr )
            IF(ierr/=0) CALL errore(subname,'deallocating VT',ABS(ierr))

        !
        ! In case of frozen states
        !
        IF ( lfrozen ) THEN
            WRITE(stdout,"(2x, 'There are frozen states')")
            !
            ! Now find the (dimwann-dimfroz(ik))-dimensional space of non-frozen states
            ! with largest overlap with s, and include it in the trial subspace
            ! (put them above the frozen states in lamp).
            !
            ! NOTA BENE: instead of what happens in projections here no use od wfcs is done
            !            moreover, wfcs have already been deallocated and wasted away
            !
            CALL projection_frozen( lamp, dimwann, dimwin, dimwinx, dimfroz, &
                                    frozen, nkpts, nbnd)

            ! Finally include the frozen states (if any) in the trial
            ! subspace at each k-point (put them at the bottom of lamp)
            ! NOTE that this must come last, because calling the subroutine projection.f
            ! would override it
            !
            DO ik = 1, nkpts
                IF ( dimfroz(ik) > 0 ) THEN
                    DO l = 1, dimfroz(ik)
                       DO j = 1, dimwin(ik)
                           lamp(j,l,ik)= CZERO
                       ENDDO
                       lamp(indxfroz(l,ik),l,ik) = CONE
                    ENDDO
                ENDIF
            ENDDO
        ENDIF
        WRITE(stdout,"()")

   END SELECT
        
   
   !
   ! Finally check that the states in the columns of the final matrix lamp are orthonormal
   ! at every k-point (i.e., that the matrix is unitary in the sense that
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
       IF ( .NOT. zmat_unitary( lamp(1:dimwin(ik),1:dimwann,ik), &
                                SIDE='left', TOLL=unitary_thr ) ) &
       CALL errore(' disentangle ', 'Vectors in lamp not orthonormal',ik)
   ENDDO


   CALL timing('subapce_init', OPR='stop')
   RETURN

END SUBROUTINE subspace_init



