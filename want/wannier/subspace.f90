!
! Copyright (C) 2004 WanT Group
!
! This file is distributed under the terms of the
! GNU General Public License. See the file `License\'
! in the root directory of the present distribution,
! or http://www.gnu.org/copyleft/gpl.txt .
!

!*********************************************
   MODULE subspace_module
   !*********************************************
   !
   USE kinds,          ONLY : dbl
   USE constants,      ONLY : ZERO, CZERO
   USE parameters,     ONLY : nstrx
   USE timing_module,  ONLY : timing
   USE log_module,     ONLY : log_push, log_pop
   USE windows_module, ONLY : nbnd, dimwin, dimwinx, windows_allocate, &
                              windows_alloc => alloc
   USE kpoints_module, ONLY : nkpts, nkpts_g, iks, kpoints_alloc
   USE io_module,      ONLY : ionode, ionode_id
   USE mp,             ONLY : mp_bcast
   USE iotk_module
   !
   IMPLICIT NONE
   PRIVATE
   SAVE

! This module handles data referring to the definition of 
! the subspace spanned by the Wannier functions which will
! be computed
!
! routines in this module:
! SUBROUTINE subspace_allocate()
! SUBROUTINE subspace_deallocate()
! SUBROUTINE subspace_write(unit,name)
! SUBROUTINE subspace_read(unit,name,found)

!
! declarations of common variables
!   

   !
   ! ... the number of Wannier functions
   INTEGER                     :: dimwann
   !
   ! ... iterative disentangle procedure parameters
   INTEGER                     :: maxiter_dis        ! maximun number of iterations
   REAL(dbl)                   :: alpha_dis          ! mixing factor in the iterative proc
   REAL(dbl)                   :: disentangle_thr    ! convergence threshold
   !
   ! ... the hamiltonian eigs in the final subspace
   REAL(dbl),    ALLOCATABLE   :: wan_eig(:,:)       ! the eigenvalues in the new subspace
   !
   ! ... rotations defining the chosen subspace
   COMPLEX(dbl), ALLOCATABLE   :: lamp(:,:,:)        ! dimwinx, dimwann, nkpts
   COMPLEX(dbl), ALLOCATABLE   :: eamp(:,:,:)        ! equal
   !
   ! NOTE: the second dimension should be DIMWANN instead of DIMWINX but, 
   !       for coherence with the old notation about frozen states 
   !       (the total number of states is DIMWANN + DIMFROZ_max) matrixes are
   !       overallocated.
   !
   COMPLEX(dbl), ALLOCATABLE   :: mtrx_in(:,:,:)   ! equal
   COMPLEX(dbl), ALLOCATABLE   :: mtrx_out(:,:,:)  ! equal
   
   LOGICAL :: alloc = .FALSE.

!
! end of declarations
!

   PUBLIC :: nkpts, nkpts_g, dimwinx
   PUBLIC :: dimwann
   PUBLIC :: maxiter_dis, alpha_dis, disentangle_thr
   PUBLIC :: wan_eig
   PUBLIC :: lamp, eamp
   PUBLIC :: mtrx_in, mtrx_out
   PUBLIC :: alloc

   PUBLIC :: subspace_allocate
   PUBLIC :: subspace_deallocate
   PUBLIC :: subspace_memusage
   PUBLIC :: subspace_write
   PUBLIC :: subspace_read

CONTAINS

!
! subroutines
!

!**********************************************************
   SUBROUTINE subspace_allocate()
   !**********************************************************
   IMPLICIT NONE
       CHARACTER(17)      :: subname="subspace_allocate"
       INTEGER            :: ierr 
      
       CALL log_push( subname )
       !
       IF ( dimwinx <= 0 ) CALL errore(subname,'Invalid dimwinx',2)
       IF ( nkpts   <= 0 ) CALL errore(subname,'Invalid nkpts',3)
       IF ( nkpts_g <= 0 ) CALL errore(subname,'Invalid nkpts_g',3)
       IF ( dimwann <= 0 ) CALL errore(subname,'Invalid dimwann', ABS(dimwann)+1)

       ALLOCATE( wan_eig(dimwann,nkpts_g), STAT=ierr )
       IF ( ierr/=0 ) CALL errore(subname,'allocating wan_eig', ABS(ierr) )

       ALLOCATE( lamp(dimwinx,dimwann,nkpts_g), STAT = ierr )
       IF( ierr /=0 ) CALL errore(subname, 'allocating lamp', ABS(ierr) )

       ALLOCATE( mtrx_in(dimwinx,dimwinx,nkpts), STAT = ierr )
       IF( ierr /=0 ) CALL errore(subname, 'allocating mtrx_in', ABS(ierr) )
       ALLOCATE( mtrx_out(dimwinx,dimwinx,nkpts), STAT = ierr )
       IF( ierr /=0 ) CALL errore(subname, 'allocating mtrx_out', ABS(ierr) )
       !
       alloc = .TRUE.
       !
       CALL log_pop ( subname )
       !
   END SUBROUTINE subspace_allocate


!**********************************************************
   SUBROUTINE subspace_deallocate()
   !**********************************************************
   IMPLICIT NONE
       CHARACTER(19)      :: subname="subspace_deallocate"
       INTEGER            :: ierr 
      
       CALL log_push( subname )
       !
       IF ( ALLOCATED(wan_eig) ) THEN 
            DEALLOCATE(wan_eig, STAT=ierr)
            IF (ierr/=0)  CALL errore(subname,' deallocating wan_eig ',ABS(ierr))
       ENDIF
       IF ( ALLOCATED(lamp) ) THEN 
            DEALLOCATE(lamp, STAT=ierr)
            IF (ierr/=0)  CALL errore(subname,' deallocating lamp ',ABS(ierr))
       ENDIF
       IF ( ALLOCATED(eamp) ) THEN 
            DEALLOCATE(eamp, STAT=ierr)
            IF (ierr/=0)  CALL errore(subname,' deallocating eamp ',ABS(ierr))
       ENDIF
       IF ( ALLOCATED(mtrx_in) ) THEN 
            DEALLOCATE(mtrx_in, STAT=ierr)
            IF (ierr/=0)  CALL errore(subname,' deallocating mtrx_in ',ABS(ierr))
       ENDIF
       IF ( ALLOCATED(mtrx_out) ) THEN 
            DEALLOCATE(mtrx_out, STAT=ierr)
            IF (ierr/=0)  CALL errore(subname,' deallocating mtrx_out ',ABS(ierr))
       ENDIF
       !
       alloc = .FALSE.
       !
       CALL log_pop( subname )
       !
   END SUBROUTINE subspace_deallocate


!**********************************************************
   REAL(dbl) FUNCTION subspace_memusage()
   !**********************************************************
   IMPLICIT NONE
       !
       REAL(dbl) :: cost
       !
       cost = ZERO
       IF ( ALLOCATED(wan_eig) )  cost = cost + REAL(SIZE(wan_eig))    *  8.0_dbl
       IF ( ALLOCATED(lamp) )     cost = cost + REAL(SIZE(lamp))       * 16.0_dbl
       IF ( ALLOCATED(eamp) )     cost = cost + REAL(SIZE(eamp))       * 16.0_dbl
       IF ( ALLOCATED(mtrx_in) )  cost = cost + REAL(SIZE(mtrx_in))    * 16.0_dbl
       IF ( ALLOCATED(mtrx_out) ) cost = cost + REAL(SIZE(mtrx_out))   * 16.0_dbl
       !
       subspace_memusage = cost / 1000000.0_dbl
       !
   END FUNCTION subspace_memusage


!**********************************************************
   SUBROUTINE subspace_write(unit,name)
   !**********************************************************
   IMPLICIT NONE
       INTEGER,         INTENT(in) :: unit
       CHARACTER(*),    INTENT(in) :: name
       INTEGER            :: ik, ik_g
       CHARACTER(nstrx)   :: attr
       CHARACTER(14)      :: subname="subspace_write"

       IF ( .NOT. alloc ) RETURN
       CALL timing( subname, OPR='start' )
       CALL log_push( subname )
       !
       IF ( .NOT. windows_alloc ) CALL errore(subname,'windows module not alloc',1)
       !
       ! every processor writes to a different file
       !
       CALL iotk_write_begin(unit,TRIM(name))
       CALL iotk_write_attr(attr,"dimwinx",dimwinx,FIRST=.TRUE.) 
       CALL iotk_write_attr(attr,"nkpts",nkpts_g) 
       CALL iotk_write_attr(attr,"dimwann",dimwann) 
       CALL iotk_write_empty(unit,"DATA",ATTR=attr)
       !
       CALL iotk_write_dat(unit,"DIMWIN",dimwin) 
       CALL iotk_write_dat(unit,"WAN_EIGENVALUES",wan_eig)
       !
       !
       DO ik_g = 1, nkpts_g
           !
           CALL iotk_write_dat(unit,"LAMP"//TRIM(iotk_index(ik_g)), &
                               lamp(1:dimwin(ik_g),1:dimwann,ik_g))
           !
       ENDDO
       !
       IF ( ALLOCATED( eamp ) ) THEN
           !
           DO ik_g = 1, nkpts_g
               !
               CALL iotk_write_dat(unit,"EAMP"//TRIM(iotk_index(ik_g)), &
                                   eamp(1:dimwin(ik_g),1:dimwann,ik_g))
               !
           ENDDO
           !
       ENDIF

       CALL iotk_write_end(unit,TRIM(name))
       !
       CALL timing( subname, OPR='stop' )
       CALL log_pop( subname )
       !
   END SUBROUTINE subspace_write

!**********************************************************
   SUBROUTINE subspace_read(unit,name,found)
   !**********************************************************
   IMPLICIT NONE
       INTEGER,           INTENT(in) :: unit
       CHARACTER(*),      INTENT(in) :: name
       LOGICAL,           INTENT(out):: found
       LOGICAL            :: lfound
       CHARACTER(nstrx)   :: attr
       CHARACTER(13)      :: subname="subspace_read"
       INTEGER            :: nkpts_g_, dimwinx_
       INTEGER            :: ik, ik_g, ierr
       COMPLEX(dbl), ALLOCATABLE :: eamp_(:,:,:)

       CALL timing( subname, OPR='start' )
       CALL log_push( subname )
       !
       IF ( alloc ) CALL subspace_deallocate()

       IF ( ionode ) THEN
           !
           CALL iotk_scan_begin(unit,TRIM(name),FOUND=found,IERR=ierr)
           IF (.NOT. found) RETURN
           IF (ierr>0)  CALL errore(subname,'Wrong format in tag '//TRIM(name),ierr)
           found = .TRUE.

           CALL iotk_scan_empty(unit,'DATA',ATTR=attr,IERR=ierr)
           IF (ierr/=0) CALL errore(subname,'Unable to find tag DATA',ABS(ierr))
           CALL iotk_scan_attr(attr,'dimwinx',dimwinx_,IERR=ierr)
           IF (ierr/=0) CALL errore(subname,'Unable to find attr DIMWINX',ABS(ierr))
           CALL iotk_scan_attr(attr,'nkpts',nkpts_g_,IERR=ierr)
           IF (ierr/=0) CALL errore(subname,'Unable to find attr NKPTS',ABS(ierr))
           CALL iotk_scan_attr(attr,'dimwann',dimwann,IERR=ierr)
           IF (ierr/=0) CALL errore(subname,'Unable to find attr dimwann',ABS(ierr))
           !
       ENDIF
       !
       CALL mp_bcast(  found,      ionode_id )
       CALL mp_bcast(  dimwinx_,   ionode_id )
       CALL mp_bcast(  nkpts_g_,   ionode_id )
       CALL mp_bcast(  dimwann,    ionode_id )

       IF ( kpoints_alloc ) THEN
           IF ( nkpts_g_ /= nkpts_g ) CALL errore(subname,'Invalid NKPTS_G',ABS(nkpts_g-nkpts_g_))
       ELSE
           nkpts_g = nkpts_g_
       ENDIF
       !
       IF ( windows_alloc ) THEN
          IF (dimwinx_/=dimwinx) CALL errore(subname,'Invalid DIMWINX',ABS(dimwinx-dimwinx_))
       ELSE
          dimwinx = dimwinx_
          nbnd    = dimwinx_
          CALL windows_allocate()
       ENDIF

       !
       ! no check is done for dimwin
       !
       IF ( ionode ) THEN
           !
           CALL iotk_scan_dat(unit,'DIMWIN',dimwin,IERR=ierr)
           IF (ierr/=0) CALL errore(subname,'Unable to find tag DIMWIN',ABS(ierr))
           !
       ENDIF
       !
       CALL mp_bcast( dimwin,   ionode_id )

       CALL subspace_allocate()
       !
       ALLOCATE( eamp_( dimwinx, dimwann, nkpts_g ), STAT=ierr )
       IF ( ierr/=0 ) CALL errore(subname,'allocating eamp_ ',ABS(ierr))
       !
       IF ( ionode ) THEN
           !
           CALL iotk_scan_dat(unit,'WAN_EIGENVALUES',wan_eig,IERR=ierr)
           IF (ierr/=0) CALL errore(subname,'Unable to find EIGENVALUES',ABS(ierr))
           !
           DO ik_g=1,nkpts_g
               !
               CALL iotk_scan_dat( unit,'LAMP'//TRIM(iotk_index(ik_g)), &
                                   lamp(1:dimwin(ik_g), 1:dimwann, ik_g), FOUND=lfound, IERR=ierr)
               IF (ierr>0) CALL errore(subname,'Unable to find LAMP at ik',ik_g)
               !
               IF ( .NOT. lfound ) lamp(1:dimwin(ik_g), 1:dimwann, ik_g) = CZERO
               !
           ENDDO
           !
           !
           DO ik_g = 1, nkpts_g
               !
               eamp_(:,:, ik_g ) = CZERO
               !
               CALL iotk_scan_dat( unit,'EAMP'//TRIM(iotk_index(ik_g)), &
                                   eamp_(1:dimwin(ik_g), 1:dimwann, ik_g), FOUND=lfound, IERR=ierr)
               IF (ierr>0) CALL errore(subname,'Unable to find EAMP at ik',ik_g)
               !
               IF ( .NOT. lfound ) THEN
                   eamp_(:,:,:) = CZERO
                   EXIT
               ENDIF
               !
           ENDDO
           !
           CALL iotk_scan_end(unit,TRIM(name),IERR=ierr)
           IF (ierr/=0)  CALL errore(subname,'Unable to end tag '//TRIM(name),ABS(ierr))
           !
       ENDIF
       !
       CALL mp_bcast( wan_eig, ionode_id )
       !
       DO ik_g = 1, nkpts_g
           CALL mp_bcast( lamp(:,:,ik_g),    ionode_id )
       ENDDO
       !
       DO ik_g = 1, nkpts_g
           CALL mp_bcast( eamp_(:,:,ik_g),   ionode_id )
       ENDDO
       !
       IF ( lfound ) THEN
           !
           ALLOCATE( eamp( dimwinx, dimwann, nkpts_g ), STAT=ierr )
           IF ( ierr/=0 ) CALL errore(subname,'allocating eamp',ABS(ierr))
           !
           eamp(:,:,:) = eamp_(:,:,:) 
           !
       ENDIF
       !
       DEALLOCATE( eamp_, STAT=ierr )
       IF ( ierr/=0 ) CALL errore(subname,'deallocating eamp_',ABS(ierr))
       

       CALL timing( subname, OPR='stop' )
       CALL log_pop ( subname )
       !
   END SUBROUTINE subspace_read

END MODULE subspace_module
