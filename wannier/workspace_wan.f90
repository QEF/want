! Copyright (C) 2008 WanT Group
!
! This file is distributed under the terms of the
! GNU General Public License. See the file `License\'
! in the root directory of the present distribution,
! or http://www.gnu.org/copyleft/gpl.txt .
!
!*********************************************
   MODULE workspace_wan_module
   !*********************************************
   !
   ! Workspace used in wannier calculations
   !
   USE kinds,                ONLY : dbl 
   USE constants,            ONLY : ZERO
   USE kpoints_module,       ONLY : nkpts, nb
   USE subspace_module,      ONLY : dimwann
   !
   IMPLICIT NONE
   PRIVATE 
   SAVE
   !
   REAL(dbl),    ALLOCATABLE ::  sheet(:,:,:)
   COMPLEX(dbl), ALLOCATABLE ::  domg(:,:,:) 
   COMPLEX(dbl), ALLOCATABLE ::  domg_aux(:,:,:) 
   COMPLEX(dbl), ALLOCATABLE ::  dq(:,:,:) 
   COMPLEX(dbl), ALLOCATABLE ::  dq0(:,:,:)
   COMPLEX(dbl), ALLOCATABLE ::  cdU(:,:,:)
   COMPLEX(dbl), ALLOCATABLE ::  csheet(:,:,:)
   COMPLEX(dbl), ALLOCATABLE ::  cu0(:,:,:) 
   COMPLEX(dbl), ALLOCATABLE ::  Mkb0(:,:,:,:)
   COMPLEX(dbl), ALLOCATABLE ::  Mkb_aux(:,:,:,:)   
   !
   LOGICAL :: alloc = .FALSE.

   !
   PUBLIC :: sheet, csheet
   PUBLIC :: domg, domg_aux
   PUBLIC :: dq, dq0
   PUBLIC :: cdU, cu0
   PUBLIC :: Mkb0, Mkb_aux
   !
   PUBLIC :: workspace_wan_allocate
   PUBLIC :: workspace_wan_deallocate
   PUBLIC :: workspace_wan_memusage
   PUBLIC :: alloc

CONTAINS

!**********************************************************
   SUBROUTINE workspace_wan_allocate()
   !**********************************************************
   IMPLICIT NONE
      CHARACTER(22)      :: subname="workspace_wan_allocate"
      INTEGER  :: ierr

      IF ( alloc )       CALL errore(subname,'already allocated', 1 )
      IF ( nkpts <=0  )  CALL errore(subname,'invalid nkpts', 1 )
      IF ( nb <=0  )     CALL errore(subname,'invalid nb', 1 )
      IF ( dimwann <=0 ) CALL errore(subname,'invalid dimwann', 1 )

      ALLOCATE( csheet(dimwann,nb,nkpts), STAT=ierr )
      IF( ierr /=0 ) CALL errore(subname, 'allocating csheet', ABS(ierr))
      !
      ALLOCATE( sheet(dimwann,nb,nkpts), STAT=ierr )
      IF( ierr /=0 ) CALL errore(subname, 'allocating sheet', ABS(ierr))

      ALLOCATE( cu0(dimwann,dimwann,nkpts), STAT=ierr )
      IF( ierr /=0 ) CALL errore(subname, 'allocating cu0', ABS(ierr) )
      !
      ALLOCATE( Mkb0(dimwann,dimwann,nb/2,nkpts), STAT=ierr )
      IF( ierr /=0 ) CALL errore(subname, 'allocating Mkb0', ABS(ierr) )
      !
      ALLOCATE( Mkb_aux(dimwann,dimwann,nb/2,nkpts), STAT=ierr )
      IF( ierr /=0 ) CALL errore(subname, 'allocating Mkb_aux', ABS(ierr) )
      !
      ALLOCATE( domg(dimwann,dimwann,nkpts), STAT=ierr )
      IF( ierr /=0 ) CALL errore(subname, 'allocating domg', ABS(ierr) )
      !
      ALLOCATE( domg_aux(dimwann,dimwann,nkpts), STAT=ierr )
      IF( ierr /=0 ) CALL errore(subname, 'allocating domg_aux', ABS(ierr) )
      !
      ALLOCATE( dq0(dimwann,dimwann,nkpts), STAT=ierr )
      IF( ierr /=0 ) CALL errore(subname, 'allocating dq0', ABS(ierr) )
      !
      ALLOCATE( dq(dimwann,dimwann,nkpts), STAT=ierr )
      IF( ierr /=0 ) CALL errore(subname, 'allocating dq', ABS(ierr) )
      !
      ALLOCATE( cdU(dimwann,dimwann,nkpts), STAT=ierr )
      IF( ierr /=0 ) CALL errore(subname, 'allocating cdU', ABS(ierr) )

      !
      alloc = .TRUE.
      !
   END SUBROUTINE workspace_wan_allocate


!**********************************************************
   SUBROUTINE workspace_wan_deallocate()
   !**********************************************************
   IMPLICIT NONE
      CHARACTER(24)      :: subname="workspace_wan_deallocate"
      INTEGER  :: ierr

      IF ( .NOT. alloc )  RETURN
      !
      !
      IF ( ALLOCATED( csheet) ) THEN
          DEALLOCATE( csheet, STAT=ierr )
          IF( ierr /=0 ) CALL errore(subname, 'deallocating csheet', ABS(ierr) )
      ENDIF
      !
      IF ( ALLOCATED( sheet) ) THEN
          DEALLOCATE( sheet, STAT=ierr )
          IF( ierr /=0 ) CALL errore(subname, 'deallocating sheet', ABS(ierr) )
      ENDIF
      !
      IF ( ALLOCATED( cu0) ) THEN
          DEALLOCATE( cu0, STAT=ierr )
          IF( ierr /=0 ) CALL errore(subname, 'deallocating cu0', ABS(ierr) )
      ENDIF
      !
      IF ( ALLOCATED( Mkb0) ) THEN
         DEALLOCATE( Mkb0, STAT=ierr )
         IF( ierr /=0 ) CALL errore(subname, 'deallocating Mkb0', ABS(ierr) )
      ENDIF
      !
      IF ( ALLOCATED( Mkb_aux ) ) THEN
         DEALLOCATE( Mkb_aux, STAT=ierr )
         IF( ierr /=0 ) CALL errore(subname, 'deallocating Mkb_aux', ABS(ierr) )
      ENDIF
      !
      IF ( ALLOCATED( domg ) ) THEN
         DEALLOCATE( domg, STAT=ierr )
         IF( ierr /=0 ) CALL errore(subname, 'deallocating domg', ABS(ierr) )
      ENDIF
      !
      IF ( ALLOCATED( domg_aux ) ) THEN
         DEALLOCATE( domg_aux,  STAT=ierr )
         IF( ierr /=0 ) CALL errore(subname, 'deallocating domg_aux', ABS(ierr) )
      ENDIF
      !
      IF ( ALLOCATED( dq0 ) ) THEN
         DEALLOCATE( dq0, STAT=ierr )
         IF( ierr /=0 ) CALL errore(subname, 'deallocating dq0', ABS(ierr) )
      ENDIF
      !
      IF ( ALLOCATED( dq ) ) THEN
         DEALLOCATE( dq, STAT=ierr )
         IF( ierr /=0 ) CALL errore(subname, 'deallocating dq', ABS(ierr) )
      ENDIF
      !
      IF ( ALLOCATED( cdU ) ) THEN
         DEALLOCATE( cdU, STAT=ierr )
         IF( ierr /=0 ) CALL errore(subname, 'deallocating cdU', ABS(ierr) ) 
      ENDIF

      !
      alloc = .FALSE.
      !
   END SUBROUTINE workspace_wan_deallocate


!**********************************************************
   REAL(dbl) FUNCTION workspace_wan_memusage()
   !**********************************************************
   IMPLICIT NONE
       !
       REAL(dbl) :: cost
       !
       cost = ZERO
       IF ( ALLOCATED(sheet) )    cost = cost + REAL(SIZE(sheet))      *  8.0_dbl
       IF ( ALLOCATED(csheet) )   cost = cost + REAL(SIZE(csheet))     * 16.0_dbl
       IF ( ALLOCATED(domg) )     cost = cost + REAL(SIZE(domg))       * 16.0_dbl
       IF ( ALLOCATED(domg_aux) ) cost = cost + REAL(SIZE(domg_aux))   * 16.0_dbl
       IF ( ALLOCATED(dq) )       cost = cost + REAL(SIZE(dq))         * 16.0_dbl
       IF ( ALLOCATED(dq0) )      cost = cost + REAL(SIZE(dq0))        * 16.0_dbl
       IF ( ALLOCATED(cdU) )      cost = cost + REAL(SIZE(cdU))        * 16.0_dbl
       IF ( ALLOCATED(cu0) )      cost = cost + REAL(SIZE(cu0))        * 16.0_dbl
       IF ( ALLOCATED(Mkb0) )     cost = cost + REAL(SIZE(Mkb0))       * 16.0_dbl
       IF ( ALLOCATED(Mkb_aux) )  cost = cost + REAL(SIZE(Mkb_aux))    * 16.0_dbl
       !
       workspace_wan_memusage = cost / 1000000.0_dbl
       !
   END FUNCTION workspace_wan_memusage

END MODULE workspace_wan_module

