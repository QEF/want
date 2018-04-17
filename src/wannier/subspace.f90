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
   USE kpoints_module, ONLY : nkpts_g, kpoints_alloc
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
! SUBROUTINE subspace_write(iun,tag)
! SUBROUTINE subspace_read(iun,tag,found)

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
   COMPLEX(dbl), ALLOCATABLE   :: lamp(:,:,:)        ! dimwinx, dimwann, nkpts_g
   COMPLEX(dbl), ALLOCATABLE   :: eamp(:,:,:)        ! equal
   !
   LOGICAL :: alloc = .FALSE.

!
! end of declarations
!

   PUBLIC :: nkpts_g, dimwinx
   PUBLIC :: dimwann, dimwin
   PUBLIC :: maxiter_dis, alpha_dis, disentangle_thr
   PUBLIC :: wan_eig
   PUBLIC :: lamp, eamp
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
   SUBROUTINE subspace_allocate(leig, llamp, leamp)
   !**********************************************************
   IMPLICIT NONE
       !
       LOGICAL, OPTIONAL, INTENT(IN) :: leig, llamp, leamp
       !
       CHARACTER(17)      :: subname="subspace_allocate"
       LOGICAL            :: leig_, llamp_, leamp_
       INTEGER            :: ierr 
      
       CALL log_push( subname )
       !
       leig_  = .TRUE.
       llamp_ = .TRUE.
       leamp_ = .TRUE.
       IF ( PRESENT( leig ) )   leig_ = leig
       IF ( PRESENT( llamp ) ) llamp_ = llamp
       IF ( PRESENT( leamp ) ) leamp_ = leamp
       !
       !
       IF ( dimwinx <= 0 ) CALL errore(subname,'Invalid dimwinx',2)
       IF ( nkpts_g <= 0 ) CALL errore(subname,'Invalid nkpts_g',3)
       IF ( dimwann <= 0 ) CALL errore(subname,'Invalid dimwann', ABS(dimwann)+1)
       !
       IF ( leig_ .AND. .NOT. ALLOCATED( wan_eig ) ) THEN
           ALLOCATE( wan_eig(dimwann,nkpts_g), STAT=ierr )
           IF ( ierr/=0 ) CALL errore(subname,'allocating wan_eig', ABS(ierr) )
       ENDIF
       !
       IF ( llamp_ .AND. .NOT. ALLOCATED( lamp ) ) THEN
           ALLOCATE( lamp(dimwinx,dimwann,nkpts_g), STAT = ierr )
           IF( ierr /=0 ) CALL errore(subname, 'allocating lamp', ABS(ierr) )
       ENDIF
       !
       IF ( leamp_ .AND. .NOT. ALLOCATED( eamp) ) THEN
           ALLOCATE( eamp(dimwinx,dimwann,nkpts_g), STAT = ierr )
           IF( ierr /=0 ) CALL errore(subname, 'allocating eamp', ABS(ierr) )
       ENDIF
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
       !
       subspace_memusage = cost / 1000000.0_dbl
       !
   END FUNCTION subspace_memusage


!**********************************************************
   SUBROUTINE subspace_write(iun,tag)
   !**********************************************************
   IMPLICIT NONE
       INTEGER,         INTENT(in) :: iun
       CHARACTER(*),    INTENT(in) :: tag
       INTEGER            :: ik_g
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
       CALL iotk_write_begin(iun,TRIM(tag))
       CALL iotk_write_attr(attr,"dimwinx",dimwinx,FIRST=.TRUE.) 
       CALL iotk_write_attr(attr,"nkpts",nkpts_g) 
       CALL iotk_write_attr(attr,"dimwann",dimwann) 
       CALL iotk_write_empty(iun,"DATA",ATTR=attr)
       !
       CALL iotk_write_dat(iun,"DIMWIN",dimwin, COLUMNS=8) 
       !
       IF ( ALLOCATED( wan_eig ) ) THEN
           CALL iotk_write_dat(iun,"WAN_EIGENVALUES",wan_eig, COLUMNS=8)
       ENDIF
       !
       !
       IF ( ALLOCATED( lamp) ) THEN
           !
           DO ik_g = 1, nkpts_g
               !
               CALL iotk_write_dat(iun,"LAMP"//TRIM(iotk_index(ik_g)), &
                                   lamp(1:dimwin(ik_g),1:dimwann,ik_g))
               !
           ENDDO
           !
       ENDIF
       !
       IF ( ALLOCATED( eamp ) ) THEN
           !
           DO ik_g = 1, nkpts_g
               !
               CALL iotk_write_dat(iun,"EAMP"//TRIM(iotk_index(ik_g)), &
                                   eamp(1:dimwin(ik_g),1:dimwann,ik_g))
               !
           ENDDO
           !
       ENDIF

       CALL iotk_write_end(iun,TRIM(tag))
       !
       CALL timing( subname, OPR='stop' )
       CALL log_pop( subname )
       !
   END SUBROUTINE subspace_write

!**********************************************************
   SUBROUTINE subspace_read(iun,tag,found, leig, llamp,leamp)
   !**********************************************************
   IMPLICIT NONE
       INTEGER,           INTENT(in) :: iun
       CHARACTER(*),      INTENT(in) :: tag
       LOGICAL,           INTENT(out):: found
       LOGICAL, OPTIONAL, INTENT(IN) :: leig, llamp, leamp
       !
       LOGICAL            :: lfound
       LOGICAL            :: leig_, llamp_, leamp_
       CHARACTER(nstrx)   :: attr
       CHARACTER(13)      :: subname="subspace_read"
       INTEGER            :: nkpts_g_, dimwinx_
       INTEGER            :: ik_g, ierr

       CALL timing( subname, OPR='start' )
       CALL log_push( subname )
       !
       IF ( alloc ) CALL subspace_deallocate()
       !
       !
       leig_  = .TRUE.
       llamp_ = .TRUE.
       leamp_ = .TRUE.
       IF ( PRESENT( leig ) )   leig_ = leig
       IF ( PRESENT( llamp ) ) llamp_ = llamp
       IF ( PRESENT( leamp ) ) leamp_ = leamp
       !
       !
       IF ( ionode ) THEN
           !
           CALL iotk_scan_begin(iun,TRIM(tag),FOUND=found,IERR=ierr)
           IF (.NOT. found) RETURN
           IF (ierr>0)  CALL errore(subname,'Wrong format in tag '//TRIM(tag),ierr)
           found = .TRUE.

           CALL iotk_scan_empty(iun,'DATA',ATTR=attr,IERR=ierr)
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
           CALL iotk_scan_dat(iun,'DIMWIN',dimwin,IERR=ierr)
           IF (ierr/=0) CALL errore(subname,'Unable to find tag DIMWIN',ABS(ierr))
           !
       ENDIF
       !
       CALL mp_bcast( dimwin,   ionode_id )

       CALL subspace_allocate( LEIG=leig_, LLAMP=llamp_, LEAMP=leamp_ )
       !
       IF ( ionode ) THEN
           !
           IF ( leig_ ) THEN
               !
               CALL iotk_scan_dat(iun,'WAN_EIGENVALUES',wan_eig,IERR=ierr)
               IF (ierr/=0) CALL errore(subname,'Unable to find EIGENVALUES',ABS(ierr))
               !
           ENDIF
           !
           IF ( llamp_ ) THEN
               !
               DO ik_g=1,nkpts_g
                   !
                   CALL iotk_scan_dat( iun,'LAMP'//TRIM(iotk_index(ik_g)), &
                                       lamp(1:dimwin(ik_g), 1:dimwann, ik_g), FOUND=lfound, IERR=ierr)
                   IF (ierr>0) CALL errore(subname,'Unable to find LAMP at ik',ik_g)
                   !
               ENDDO
               !
           ENDIF
           !
           !
           IF ( leamp_ ) THEN
               !
               DO ik_g = 1, nkpts_g
                   !
                   eamp(:,:, ik_g ) = CZERO
                   !
                   CALL iotk_scan_dat( iun,'EAMP'//TRIM(iotk_index(ik_g)), &
                                       eamp(1:dimwin(ik_g), 1:dimwann, ik_g), IERR=ierr)
                   IF (ierr/=0) CALL errore(subname,'Unable to find EAMP at ik',ik_g)
                   !
               ENDDO
               !
           ENDIF
           !
           CALL iotk_scan_end(iun,TRIM(tag),IERR=ierr)
           IF (ierr/=0)  CALL errore(subname,'Unable to end tag '//TRIM(tag),ABS(ierr))
           !
       ENDIF
       !
       IF ( leig_ ) THEN
           CALL mp_bcast( wan_eig, ionode_id )
       ENDIF
       !
       IF ( llamp_ ) THEN
           !
           DO ik_g = 1, nkpts_g
               CALL mp_bcast( lamp(:,:,ik_g),    ionode_id )
           ENDDO
           !
       ENDIF
       !
       IF ( leamp_ ) THEN
           !
           DO ik_g = 1, nkpts_g
               CALL mp_bcast( eamp(:,:,ik_g),   ionode_id )
           ENDDO
           !
       ENDIF
       !
       !
       CALL timing( subname, OPR='stop' )
       CALL log_pop ( subname )
       !
   END SUBROUTINE subspace_read

END MODULE subspace_module
