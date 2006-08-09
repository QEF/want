!
! Copyright (C) 2006 WanT Group
!
! This file is distributed under the terms of the
! GNU General Public License. See the file `License\'
! in the root directory of the present distribution,
! or http://www.gnu.org/copyleft/gpl.txt .
!

!*********************************************
   MODULE symmetry_module
!*********************************************
   USE kinds,           ONLY : dbl
   USE parameters,      ONLY : nstrx
   USE qexml_module
   USE qexpt_module
   IMPLICIT NONE
   PRIVATE
   SAVE

! This module handles symmetry data.
! Symmetry operations are not computed, but read from DFT datafile
!
! routines in this module:
! SUBROUTINE symmetry_allocate()
! SUBROUTINE symmetry_deallocate()
! SUBROUTINE symmetry_read_ext( filefmt )

!
! declarations of common variables
!   

   !
   INTEGER                       :: nsym          ! number of allowed symm operations
   INTEGER,          ALLOCATABLE :: s(:,:,:)      ! operations, 3x3xNsym, cryst. units 
   REAL(dbl),        ALLOCATABLE :: ftau(:,:)     ! frac. traslations, 3xNym, cryst. units
   !
   CHARACTER(nstrx), ALLOCATABLE :: sname(:)      ! symmetry names
   
   LOGICAL :: alloc = .FALSE.

!
! end of declarations
!

   PUBLIC :: nsym
   PUBLIC :: s
   PUBLIC :: ftau
   PUBLIC :: sname
   PUBLIC :: alloc

   PUBLIC :: symmetry_allocate
   PUBLIC :: symmetry_deallocate
   PUBLIC :: symmetry_read_ext

CONTAINS

!
! subroutines
!

!**********************************************************
   SUBROUTINE symmetry_allocate()
   !**********************************************************
   IMPLICIT NONE
       CHARACTER(17)      :: subname="symmetry_allocate"
       INTEGER            :: ierr 
      
       IF ( nsym <= 0 ) CALL errore(subname,' Invalid nsym',ABS(nsym)+1)

       ALLOCATE( s(3, 3, nsym ), STAT = ierr )
       IF( ierr /=0 ) CALL errore(subname, ' allocating nsym ', ABS(ierr) )
       ALLOCATE( ftau( 3, nsym ), STAT = ierr )
       IF( ierr /=0 ) CALL errore(subname, ' allocating ftau ', ABS(ierr) )
       ALLOCATE( sname( nsym ), STAT = ierr )
       IF( ierr /=0 ) CALL errore(subname, ' allocating sname ', ABS(ierr) )

       alloc = .TRUE.

   END SUBROUTINE symmetry_allocate


!**********************************************************
   SUBROUTINE symmetry_deallocate()
   !**********************************************************
   IMPLICIT NONE
       CHARACTER(19)      :: subname="symmetry_deallocate"
       INTEGER            :: ierr 
      
       IF ( ALLOCATED(s) ) THEN 
            DEALLOCATE(s, STAT=ierr)
            IF (ierr/=0)  CALL errore(subname,' deallocating s',ABS(ierr))
       ENDIF
       IF ( ALLOCATED(ftau) ) THEN 
            DEALLOCATE(ftau, STAT=ierr)
            IF (ierr/=0)  CALL errore(subname,' deallocating ftau',ABS(ierr))
       ENDIF
       IF ( ALLOCATED(sname) ) THEN 
            DEALLOCATE(sname, STAT=ierr)
            IF (ierr/=0)  CALL errore(subname,' deallocating sname',ABS(ierr))
       ENDIF
       !
       alloc = .FALSE.
   END SUBROUTINE symmetry_deallocate


!*********************************************************
   SUBROUTINE symmetry_read_ext( filefmt )
   !*********************************************************
   IMPLICIT NONE
       CHARACTER(*),      INTENT(in) :: filefmt
       !
       CHARACTER(17)        :: subname="symmetry_read_ext"
       INTEGER              :: ierr


       SELECT CASE ( TRIM(filefmt) )
       !
       CASE ( 'qexml' )
            !
            CALL qexml_read_symmetry( NSYM=nsym, IERR=ierr )
            !
       CASE ( 'pw_export' )
            !
            CALL qexpt_read_symmetry( NSYM=nsym, IERR=ierr )
            !
       CASE DEFAULT
            !
            CALL errore(subname,'invalid filefmt = '//TRIM(filefmt), 1)
       END SELECT
       !
       IF ( ierr/=0) CALL errore(subname,'getting symmetry dimensions',ABS(ierr))
       !

       !
       ! module allocate 
       !
       CALL symmetry_allocate( ) 

       !
       ! read all the symmetry data
       !
       SELECT CASE ( TRIM(filefmt) )
       !
       CASE ( 'qexml' )
            !
            CALL qexml_read_symmetry( S=s, IERR=ierr )
            !
       CASE ( 'pw_export' )
            !
            CALL qexpt_read_symmetry( S=s, IERR=ierr )
            !
       CASE DEFAULT
            !
            CALL errore(subname,'invalid filefmt = '//TRIM(filefmt), 2)
       END SELECT
       !
       IF ( ierr/=0) CALL errore(subname,'getting symmetry data',ABS(ierr))

   END SUBROUTINE symmetry_read_ext

END MODULE symmetry_module

