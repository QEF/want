!
! Copyright (C) 2004 WanT Group
!
! This file is distributed under the terms of the
! GNU General Public License. See the file `License\'
! in the root directory of the present distribution,
! or http://www.gnu.org/copyleft/gpl.txt .
!

!*********************************************
   MODULE struct_fact_data_module
!*********************************************
   USE kinds, ONLY : dbl
   USE ions_module,       ONLY : ions_alloc => alloc, nat, nsp, ityp, tau
   USE lattice_module,    ONLY : lattice_alloc => alloc, bvec, tpiba
   USE ggrids_module,     ONLY : ggrids_alloc => alloc, npw => npw_rho, nr => nfft, g, igv
   IMPLICIT NONE
   PRIVATE
   SAVE

! This module handles data related to structure factors,
! i.e. the quantities  
!                      eigts(G) = e^(-i G*tau_s)
!                  strf(G,ityp) = \sum_{ia \in ityp)} e^(-i G*tau_s(ia) )
!
! routines in this module:
! SUBROUTINE struct_fact_data_allocate()
! SUBROUTINE struct_fact_data_deallocate()
! SUBROUTINE struct_fact_data_init()

!
! declarations of common variables
!   

   COMPLEX(dbl), ALLOCATABLE :: strf(:,:)        ! DIM( npw, nsp )
   COMPLEX(dbl), ALLOCATABLE :: eigts1(:,:)      ! DIM( -nr1:nr1, nat )
   COMPLEX(dbl), ALLOCATABLE :: eigts2(:,:)      ! DIM( -nr2:nr2, nat )
   COMPLEX(dbl), ALLOCATABLE :: eigts3(:,:)      ! DIM( -nr3:nr3, nat )

   LOGICAL :: alloc = .FALSE.

!
! end of declarations
!

   PUBLIC :: nat, nsp, nr, npw 
   PUBLIC :: strf
   PUBLIC :: eigts1, eigts2, eigts3
   PUBLIC :: alloc
   PUBLIC :: struct_fact_data_allocate
   PUBLIC :: struct_fact_data_deallocate
   PUBLIC :: struct_fact_data_init

CONTAINS

!
! subroutines
!

!**********************************************************
   SUBROUTINE struct_fact_data_allocate()
   !**********************************************************
   IMPLICIT NONE
       CHARACTER(25)      :: subname="struct_fact_data_allocate"
       INTEGER            :: ierr 

       IF ( .NOT. lattice_alloc ) CALL errore(subname,'Lattice not alloc',1) 
       IF ( .NOT. ions_alloc )    CALL errore(subname,'ions not alloc',1) 
       IF ( .NOT. ggrids_alloc )  CALL errore(subname,'ggrids not alloc',1) 
       IF ( nat <= 0 )  CALL errore(subname,'nat <= 0',ABS(nat)+1)
       IF ( nsp <= 0 )  CALL errore(subname,'nsp <= 0',ABS(nsp)+1)
       IF ( npw <= 0 )  CALL errore(subname,'npw <= 0',ABS(npw)+1)
       IF ( nr(1) <= 0 )  CALL errore(subname,'nr(1) <= 0',ABS(nr(1))+1)
       IF ( nr(2) <= 0 )  CALL errore(subname,'nr(2) <= 0',ABS(nr(2))+1)
       IF ( nr(3) <= 0 )  CALL errore(subname,'nr(3) <= 0',ABS(nr(3))+1)

       ALLOCATE( strf(npw,nsp), STAT=ierr )
          IF (ierr/=0) CALL errore(subname,'allocating strf',npw*nsp)
       ALLOCATE( eigts1(-nr(1):nr(1),nat), STAT=ierr )
          IF (ierr/=0) CALL errore(subname,'allocating eigts1',ABS(ierr))
       ALLOCATE( eigts2(-nr(2):nr(2),nat), STAT=ierr )
          IF (ierr/=0) CALL errore(subname,'allocating eigts2',ABS(ierr))
       ALLOCATE( eigts3(-nr(3):nr(3),nat), STAT=ierr )
          IF (ierr/=0) CALL errore(subname,'allocating eigts3',ABS(ierr))

       alloc = .TRUE.
      
   END SUBROUTINE struct_fact_data_allocate


!**********************************************************
   SUBROUTINE struct_fact_data_deallocate()
   !**********************************************************
   IMPLICIT NONE
       CHARACTER(27)      :: subname="struct_fact_data_deallocate"
       INTEGER            :: ierr

       IF ( ALLOCATED(strf) ) THEN
            DEALLOCATE(strf, STAT=ierr)
            IF (ierr/=0)  CALL errore(subname,' deallocating strf ',ABS(ierr))
       ENDIF
       IF ( ALLOCATED(eigts1) ) THEN
            DEALLOCATE(eigts1, STAT=ierr)
            IF (ierr/=0)  CALL errore(subname,' deallocating eigts1 ',ABS(ierr))
       ENDIF
       IF ( ALLOCATED(eigts2) ) THEN
            DEALLOCATE(eigts2, STAT=ierr)
            IF (ierr/=0)  CALL errore(subname,' deallocating eigts2 ',ABS(ierr))
       ENDIF
       IF ( ALLOCATED(eigts3) ) THEN
            DEALLOCATE(eigts3, STAT=ierr)
            IF (ierr/=0)  CALL errore(subname,' deallocating eigts3 ',ABS(ierr))
       ENDIF
       alloc = .FALSE.

   END SUBROUTINE struct_fact_data_deallocate


!*********************************************************
   SUBROUTINE struct_fact_data_init()
   !*********************************************************
   !
   ! assumes the allocation is done somewhere else
   !
   IMPLICIT NONE
       CHARACTER(21)      :: subname="struct_fact_data_init"
       REAL(dbl)          :: bg_(3,3)

       IF ( .NOT. lattice_alloc ) CALL errore(subname,'Lattice not alloc',1) 
       IF ( .NOT. ions_alloc )    CALL errore(subname,'ions not alloc',1) 
       IF ( .NOT. ggrids_alloc )  CALL errore(subname,'ggrids not alloc',1) 

       IF ( alloc ) CALL errore(subname,'structure factors already alloc',1)
       CALL struct_fact_data_allocate()
       
       !
       ! use the Espresso routine
       bg_ = bvec/tpiba
       CALL struct_fact( nat, tau, nsp, ityp, npw, g, bg_, &
                         nr(1), nr(2), nr(3), strf, eigts1, eigts2, eigts3)

   END SUBROUTINE struct_fact_data_init

END MODULE struct_fact_data_module

