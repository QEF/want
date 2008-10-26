!
! Copyright (C) 2005 WanT Group
!
! This file is distributed under the terms of the
! GNU General Public License. See the file `License\'
! in the root directory of the present distribution,
! or http://www.gnu.org/copyleft/gpl.txt .
!
!*********************************************
   MODULE T_egrid_module
   !*********************************************
   !
   USE kinds,           ONLY : dbl
   USE log_module,      ONLY : log_push, log_pop
   !
   IMPLICIT NONE
   PRIVATE 
   SAVE
!
! Contains transport energy-grid data
! 
   
   INTEGER                :: ne        ! dimension of the energy grid
   REAL(dbl)              :: emin      !
   REAL(dbl)              :: emax      ! egrid extrema 
   !
   REAL(dbl):: de
   REAL(dbl), ALLOCATABLE :: egrid(:)  ! grid values
   !
   LOGICAL :: alloc = .FALSE.

!
! end delcarations
!

   PUBLIC :: ne, emin, emax
   PUBLIC :: egrid
   PUBLIC :: alloc
   !
   PUBLIC :: egrid_init, de
   PUBLIC :: egrid_deallocate


CONTAINS

!
! subroutines
!

!**********************************************************
   SUBROUTINE egrid_init()
   !**********************************************************
   IMPLICIT NONE
       CHARACTER(10) :: subname="egrid_init"
       INTEGER       :: ie, ierr
       !
       CALL log_push ( 'egrid_init' )

       IF ( alloc )   CALL errore(subname,'already allocated', 1 )
       IF ( ne <= 0 ) CALL errore(subname,'invalid ne', -ne+1 )
       
       ALLOCATE( egrid(ne), STAT=ierr )
       IF (ierr/=0) CALL errore(subname,'allocating egrid', ABS(ierr))

       !
       ! setting the energy grid
       !
       de = (emax - emin) / REAL(ne -1, dbl)
       !
       DO ie = 1, ne
          egrid(ie) = emin + REAL(ie -1, dbl) * de
       ENDDO

       alloc = .TRUE.
       CALL log_pop ( 'egrid_init' )
       !
   END SUBROUTINE egrid_init


!**********************************************************
   SUBROUTINE egrid_deallocate()
   !**********************************************************
   IMPLICIT NONE
       CHARACTER(16)      :: subname="egrid_deallocate"
       INTEGER :: ierr
       CALL log_push ( 'egrid_deallocate' )

       IF ( ALLOCATED(egrid) ) THEN
           DEALLOCATE(egrid, STAT=ierr)
           IF (ierr/=0) CALL errore(subname,'deallocating egrid',ABS(ierr))
       ENDIF
       !
       alloc = .FALSE.
       CALL log_pop ( 'egrid_deallocate' )
       !
   END SUBROUTINE egrid_deallocate


END MODULE T_egrid_module

