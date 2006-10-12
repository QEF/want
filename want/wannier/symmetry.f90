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
   USE log_module,      ONLY : log_push, log_pop
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
! SUBROUTINE symmetry_write ( unit, isym, srot, tau, sname)

!
! declarations of common variables
!   

   !
   INTEGER                       :: nsym          ! number of allowed symm operations
   INTEGER,          ALLOCATABLE :: srot(:,:,:)   ! operations, 3x3xNsym, cryst. units 
   REAL(dbl),        ALLOCATABLE :: strasl(:,:)   ! frac. traslations, 3xNym, cryst. units
   !
   CHARACTER(nstrx), ALLOCATABLE :: sname(:)      ! symmetry names
   
   LOGICAL :: alloc = .FALSE.

!
! end of declarations
!

   PUBLIC :: nsym
   PUBLIC :: srot
   PUBLIC :: strasl
   PUBLIC :: sname
   PUBLIC :: alloc

   PUBLIC :: symmetry_allocate
   PUBLIC :: symmetry_deallocate
   PUBLIC :: symmetry_read_ext
   PUBLIC :: symmetry_write

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
      
       CALL log_push( subname )
       !
       IF ( nsym <= 0 ) CALL errore(subname,' Invalid nsym',ABS(nsym)+1)

       ALLOCATE( srot(3, 3, nsym ), STAT = ierr )
       IF( ierr /=0 ) CALL errore(subname, ' allocating nsym ', ABS(ierr) )
       ALLOCATE( strasl( 3, nsym ), STAT = ierr )
       IF( ierr /=0 ) CALL errore(subname, ' allocating strasl ', ABS(ierr) )
       ALLOCATE( sname( nsym ), STAT = ierr )
       IF( ierr /=0 ) CALL errore(subname, ' allocating sname ', ABS(ierr) )

       alloc = .TRUE.
       !
       CALL log_pop ( subname )
       !
   END SUBROUTINE symmetry_allocate


!**********************************************************
   SUBROUTINE symmetry_deallocate()
   !**********************************************************
   IMPLICIT NONE
       CHARACTER(19)      :: subname="symmetry_deallocate"
       INTEGER            :: ierr 
      
       CALL log_push( subname )
       !
       IF ( ALLOCATED(srot) ) THEN 
            DEALLOCATE(srot, STAT=ierr)
            IF (ierr/=0)  CALL errore(subname,' deallocating srot',ABS(ierr))
       ENDIF
       IF ( ALLOCATED(strasl) ) THEN 
            DEALLOCATE(strasl, STAT=ierr)
            IF (ierr/=0)  CALL errore(subname,' deallocating strasl',ABS(ierr))
       ENDIF
       IF ( ALLOCATED(sname) ) THEN 
            DEALLOCATE(sname, STAT=ierr)
            IF (ierr/=0)  CALL errore(subname,' deallocating sname',ABS(ierr))
       ENDIF
       !
       alloc = .FALSE.
       !
       CALL log_pop ( subname )
       !
   END SUBROUTINE symmetry_deallocate


!*********************************************************
   SUBROUTINE symmetry_read_ext( filefmt )
   !*********************************************************
   IMPLICIT NONE
       CHARACTER(*),      INTENT(in) :: filefmt
       !
       CHARACTER(17)        :: subname="symmetry_read_ext"
       INTEGER              :: ierr

       CALL log_push( subname )
       !
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
            CALL qexml_read_symmetry( S=srot, TRASL=strasl, SNAME=sname, IERR=ierr )
            !
       CASE ( 'pw_export' )
            !
            CALL qexpt_read_symmetry( S=srot, TRASL=strasl, SNAME=sname, IERR=ierr )
            !
       CASE DEFAULT
            !
            CALL errore(subname,'invalid filefmt = '//TRIM(filefmt), 2)
       END SELECT
       !
       IF ( ierr/=0) CALL errore(subname,'getting symmetry data',ABS(ierr))

       CALL log_pop ( subname )
       !
   END SUBROUTINE symmetry_read_ext


!*********************************************************
   SUBROUTINE symmetry_write( unit, isym, srot_, strasl_, sname_ )
   !*********************************************************
   !
   ! routine to summarize symmetry operations
   ! crystal coords are assumed
   !
   IMPLICIT NONE
      !
      INTEGER,       INTENT(IN) :: unit, isym
      INTEGER,       INTENT(IN) :: srot_(3,3)
      REAL(dbl),     INTENT(IN) :: strasl_(3)
      CHARACTER(*),  INTENT(IN) :: sname_
      !
      INTEGER :: ipol
      !
      !
      CALL log_push( 'symmetry_write' )
      !
      WRITE( unit, '(/6x,"isym = ",i2,5x,a45/)') isym, TRIM(sname_)
      !
      WRITE( unit, '(1x,"cryst.",3x,"s(",i2,") = (",3(i6,5x), &
            &        " )    f =( ",f10.7," )")') &
            isym, ( srot_(1,ipol), ipol=1,3 ), strasl_(1)
      WRITE( unit, '(17x," (",3(i6,5x), " )       ( ",f10.7," )")') &
                  ( srot_(2,ipol), ipol=1,3 ), strasl_(2)
      WRITE( unit, '(17x," (",3(i6,5x), " )       ( ",f10.7," )"/)') &
                  ( srot_(3,ipol), ipol=1,3 ), strasl_(3)
       
      CALL log_pop( 'symmetry_write' )
      !
   END SUBROUTINE symmetry_write

END MODULE symmetry_module

