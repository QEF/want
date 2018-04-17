!
! Copyright (C) 2008 WanT Group
!
! This file is distributed under the terms of the
! GNU General Public License. See the file `License\'
! in the root directory of the present distribution,
! or http://www.gnu.org/copyleft/gpl.txt .
!
!*********************************************
   MODULE workspace_dis_module
   !*********************************************
   !
   ! Workspace used in disentanglement
   !
   USE kinds,                ONLY : dbl 
   USE constants,            ONLY : ZERO
   USE kpoints_module,       ONLY : nkpts, nb
   USE subspace_module,      ONLY : dimwann, dimwinx
   !
   IMPLICIT NONE
   PRIVATE 
   SAVE
   !
   !
   COMPLEX(dbl), ALLOCATABLE   :: mtrx_in(:,:,:)   ! dimwinx, dimwinx, nkpts
   COMPLEX(dbl), ALLOCATABLE   :: mtrx_out(:,:,:)  ! the same
   !
   COMPLEX(dbl), ALLOCATABLE   :: Akb_aux(:,:,:,:) ! dimwann,dimwann,nb/2,nkpts 
   COMPLEX(dbl), ALLOCATABLE   :: Mkb_aux(:,:,:,:) ! dimwinx,dimwann,nb,nkpts
   !
   LOGICAL :: alloc = .FALSE.

   !
   PUBLIC :: mtrx_in, mtrx_out
   PUBLIC :: Akb_aux, Mkb_aux
   !
   PUBLIC :: workspace_dis_allocate
   PUBLIC :: workspace_dis_deallocate
   PUBLIC :: workspace_dis_memusage
   PUBLIC :: alloc

CONTAINS

!**********************************************************
   SUBROUTINE workspace_dis_allocate()
   !**********************************************************
   IMPLICIT NONE
      CHARACTER(22)      :: subname="workspace_dis_allocate"
      INTEGER  :: ierr

      IF ( alloc )       CALL errore(subname,'already allocated', 1 )
      IF ( nkpts <0 )    CALL errore(subname,'invalid nkpts', 1 )
      IF ( nb <=0 )      CALL errore(subname,'invalid nb', 1 )
      IF ( dimwann <=0 ) CALL errore(subname,'invalid dimwann', 1 )
      IF ( dimwinx <=0 ) CALL errore(subname,'invalid dimwinx', 1 )

      ALLOCATE( mtrx_in(dimwinx,dimwinx,nkpts), STAT = ierr )
      IF( ierr /=0 ) CALL errore(subname, 'allocating mtrx_in', ABS(ierr) )
      !
      ALLOCATE( mtrx_out(dimwinx,dimwinx,nkpts), STAT = ierr )
      IF( ierr /=0 ) CALL errore(subname, 'allocating mtrx_out', ABS(ierr) )
      !
      ALLOCATE( Mkb_aux(dimwann,dimwann,nb/2,nkpts), STAT=ierr )
      IF( ierr /=0 ) CALL errore(subname, 'allocating Mkb_aux', ABS(ierr) )
      !
      ALLOCATE( Akb_aux(dimwinx,dimwann,nb,nkpts), STAT=ierr )
      IF( ierr /=0 ) CALL errore(subname, 'allocating Akb_aux', ABS(ierr) )
      !
      !
      alloc = .TRUE.
      !
   END SUBROUTINE workspace_dis_allocate


!**********************************************************
   SUBROUTINE workspace_dis_deallocate()
   !**********************************************************
   IMPLICIT NONE
      CHARACTER(24)      :: subname="workspace_dis_deallocate"
      INTEGER  :: ierr

      IF ( .NOT. alloc )  RETURN
      !
      !
      IF ( ALLOCATED(mtrx_in) ) THEN
          DEALLOCATE(mtrx_in, STAT=ierr)
          IF (ierr/=0)  CALL errore(subname,'deallocating mtrx_in',ABS(ierr))
      ENDIF
      IF ( ALLOCATED(mtrx_out) ) THEN
           DEALLOCATE(mtrx_out, STAT=ierr)
           IF (ierr/=0)  CALL errore(subname,'deallocating mtrx_out',ABS(ierr))
      ENDIF
      IF ( ALLOCATED(Mkb_aux) ) THEN
           DEALLOCATE(Mkb_aux, STAT=ierr)
           IF (ierr/=0)  CALL errore(subname,'deallocating Mkb_aux',ABS(ierr))
      ENDIF
      IF ( ALLOCATED(Akb_aux) ) THEN
           DEALLOCATE(Akb_aux, STAT=ierr)
           IF (ierr/=0)  CALL errore(subname,'deallocating Akb_aux',ABS(ierr))
      ENDIF
      !
      alloc = .FALSE.
      !
   END SUBROUTINE workspace_dis_deallocate


!**********************************************************
   REAL(dbl) FUNCTION workspace_dis_memusage()
   !**********************************************************
   IMPLICIT NONE
       !
       REAL(dbl) :: cost
       !
       cost = ZERO
       IF ( ALLOCATED(mtrx_in) )  cost = cost + REAL(SIZE(mtrx_in))    * 16.0_dbl
       IF ( ALLOCATED(mtrx_out) ) cost = cost + REAL(SIZE(mtrx_out))   * 16.0_dbl
       IF ( ALLOCATED(Mkb_aux) )  cost = cost + REAL(SIZE(Mkb_aux))    * 16.0_dbl
       IF ( ALLOCATED(Akb_aux) )  cost = cost + REAL(SIZE(Akb_aux))    * 16.0_dbl
       !
       workspace_dis_memusage = cost / 1000000.0_dbl
       !
   END FUNCTION workspace_dis_memusage

END MODULE workspace_dis_module

