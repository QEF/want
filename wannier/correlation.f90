!
! Copyright (C) 2008 WanT Group
!
! This file is distributed under the terms of the
! GNU General Public License. See the file `License\'
! in the root directory of the present distribution,
! or http://www.gnu.org/copyleft/gpl.txt .
!

!*********************************************
   MODULE correlation_module
!*********************************************
   !
   ! This module contains basic variables to handle
   ! correlation self-energies
   !
   USE kinds,             ONLY : dbl
   USE constants,         ONLY : ZERO
   USE parameters,        ONLY : nstrx
   USE log_module,        ONLY : log_push, log_pop
   USE lattice_module,    ONLY : lattice_alloc => alloc
   USE kpoints_module,    ONLY : nkpts, nrtot,  kpoints_alloc 
   USE subspace_module,   ONLY : dimwann, subspace_alloc => alloc
   USE iotk_module
   !
   IMPLICIT NONE
   PRIVATE
   SAVE

!
! declarations of common variables
!   

   !
   ! ... self-energies
   COMPLEX(dbl), ALLOCATABLE   :: rsgm(:,:,:)  ! (dimwann, dimwann, nrtot), real space self-energy
   COMPLEX(dbl), ALLOCATABLE   :: ksgm(:,:,:)  ! (dimwann, dimwann, nkpts), kpt-symm self-energy
   !
   LOGICAL                     :: lhave_sgm   = .FALSE.
   LOGICAL                     :: ldynam_sgm  = .FALSE.
   !
   ! ... data for omega grids (used only if sgm is dynamical)
   INTEGER                     :: omg_nint
   INTEGER                     :: omg_index
   REAL(dbl),    ALLOCATABLE   :: omg_grid(:)     
   REAL(dbl)                   :: omg_min
   REAL(dbl)                   :: omg_max
   !
   LOGICAL :: alloc = .FALSE.

!
! end of declarations
!

   PUBLIC :: nkpts, dimwann
   PUBLIC :: nrtot
   PUBLIC :: rsgm, ksgm
   PUBLIC :: lhave_sgm
   PUBLIC :: ldynam_sgm
   !
   PUBLIC :: omg_nint, omg_index
   PUBLIC :: omg_grid
   PUBLIC :: omg_min, omg_max
   !
   PUBLIC :: alloc

   PUBLIC :: correlation_allocate
   PUBLIC :: correlation_deallocate
   PUBLIC :: correlation_memusage

CONTAINS

!
! subroutines
!

!**********************************************************
   SUBROUTINE correlation_allocate()
   !**********************************************************
   IMPLICIT NONE
       CHARACTER(20)      :: subname="correlation_allocate"
       INTEGER            :: ierr 

       CALL log_push ( subname )
       !
       IF ( .NOT. lattice_alloc )   CALL errore(subname,'lattice NOT alloc',1)
       IF ( .NOT. kpoints_alloc )   CALL errore(subname,'kpoints NOT alloc',1)
       
       IF ( dimwann <= 0 )  CALL errore(subname,'Invalid DIMWANN',1)
       IF ( nkpts <= 0 )    CALL errore(subname,'Invalid NKPTS',1)
       IF ( nrtot <= 0 )    CALL errore(subname,'Invalid NRTOT',1)

       !
       ! main data
       !
       ALLOCATE( rsgm(dimwann,dimwann,nrtot), STAT=ierr )
       IF( ierr /=0 ) CALL errore(subname, 'allocating rsgm', ABS(ierr) )
       !
       ALLOCATE( ksgm(dimwann,dimwann,nkpts), STAT=ierr )
       IF( ierr /=0 ) CALL errore(subname, 'allocating ksgm', ABS(ierr) )
       !
       ! allocate grid quantities
       !
       IF ( ldynam_sgm ) THEN
           !
           IF ( omg_nint <= 0 )    CALL errore(subname,'Invalid OMG_NINT',1)
           ! 
           ALLOCATE( omg_grid(omg_nint), STAT=ierr )
           IF( ierr /=0 ) CALL errore(subname, 'allocating omg_grid', ABS(ierr) )
           !
       ENDIF
       !
       !
       alloc = .TRUE.
       !
       CALL log_pop ( subname )
       !
   END SUBROUTINE correlation_allocate


!**********************************************************
   SUBROUTINE correlation_deallocate()
   !**********************************************************
   IMPLICIT NONE
       CHARACTER(22)      :: subname="correlation_deallocate"
       INTEGER            :: ierr 
      
       CALL log_push ( subname )
       !
       IF ( ALLOCATED(rsgm) ) THEN 
            DEALLOCATE(rsgm, STAT=ierr)
            IF (ierr/=0)  CALL errore(subname,'deallocating rsgm ',ABS(ierr))
       ENDIF
       IF ( ALLOCATED(ksgm) ) THEN 
            DEALLOCATE(ksgm, STAT=ierr)
            IF (ierr/=0)  CALL errore(subname,'deallocating ksgm ',ABS(ierr))
       ENDIF
       IF ( ALLOCATED(omg_grid) ) THEN 
            DEALLOCATE(omg_grid, STAT=ierr)
            IF (ierr/=0)  CALL errore(subname,'deallocating omg_grid ',ABS(ierr))
       ENDIF
       !
       CALL log_pop ( subname )
       !
   END SUBROUTINE correlation_deallocate

!**********************************************************
   REAL(dbl) FUNCTION correlation_memusage()
   !**********************************************************
   IMPLICIT NONE
       !
       REAL(dbl) :: cost
       !
       cost = ZERO
       IF ( ALLOCATED(rsgm) )     cost = cost + REAL(SIZE(rsgm))      * 16.0_dbl
       IF ( ALLOCATED(ksgm) )     cost = cost + REAL(SIZE(ksgm))      * 16.0_dbl
       IF ( ALLOCATED(omg_grid) ) cost = cost + REAL(SIZE(omg_grid))  *  8.0_dbl
       !
       correlation_memusage = cost / 1000000.0_dbl
       !
   END FUNCTION correlation_memusage

END MODULE correlation_module

