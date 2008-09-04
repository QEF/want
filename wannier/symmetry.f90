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
   !
   USE kinds,             ONLY : dbl
   USE constants,         ONLY : ZERO
   USE parameters,        ONLY : nstrx
   USE log_module,        ONLY : log_push, log_pop
   USE io_global_module,  ONLY : ionode, ionode_id
   USE mp,                ONLY : mp_bcast
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
! SUBROUTINE symmetry_rotate( vect, opr )
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
   PUBLIC :: symmetry_memusage
   PUBLIC :: symmetry_read_ext
   PUBLIC :: symmetry_rotate
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


!**********************************************************
   REAL(dbl) FUNCTION symmetry_memusage()
   !**********************************************************
   IMPLICIT NONE
       !
       REAL(dbl) :: cost
       !
       cost = ZERO
       IF ( ALLOCATED(srot) )     cost = cost + REAL(SIZE(srot))       * 4.0_dbl
       IF ( ALLOCATED(strasl) )   cost = cost + REAL(SIZE(strasl))     * 8.0_dbl
       IF ( ALLOCATED(sname) )    cost = cost + REAL(SIZE(sname))      * nstrx * 4.0_dbl
       !
       symmetry_memusage = cost / 1000000.0_dbl
       !
   END FUNCTION symmetry_memusage


!**********************************************************
   SUBROUTINE symmetry_rotate(vect, opr)
   !**********************************************************
   !
   ! this subrotuine apply rotates a given vector according to 
   ! the specified symmetry operation.
   ! Crystal units for both the symmetry rotation and the vector
   ! are assumed.
   ! The input vector is overwritten
   !
   IMPLICIT NONE
       !
       REAL(dbl), INTENT(INOUT) :: vect(3)
       INTEGER,   INTENT(IN)    :: opr(3,3)
       ! 
       REAL(dbl) :: rtmp(3)
 
       rtmp(1) = DOT_PRODUCT( REAL(opr(1,:), dbl), vect(:) )
       rtmp(2) = DOT_PRODUCT( REAL(opr(2,:), dbl), vect(:) )
       rtmp(3) = DOT_PRODUCT( REAL(opr(3,:), dbl), vect(:) )
       !
       vect(:) = rtmp(:)
       ! 
   END SUBROUTINE symmetry_rotate


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
            IF (ionode) CALL qexml_read_symmetry( NSYM=nsym, IERR=ierr )
            CALL mp_bcast( nsym, ionode_id)
            CALL mp_bcast( ierr, ionode_id)
            !
       CASE ( 'pw_export' )
            !
            IF (ionode) CALL qexpt_read_symmetry( NSYM=nsym, IERR=ierr )
            CALL mp_bcast( nsym, ionode_id)
            CALL mp_bcast( ierr, ionode_id)
            !
       CASE ( 'crystal' )
            !
            CALL errore(subname,'crystal readin not yet implemeted', 1)
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
            IF (ionode) CALL qexml_read_symmetry( S=srot, TRASL=strasl, SNAME=sname, IERR=ierr )
            !
            CALL mp_bcast( srot,    ionode_id)
            CALL mp_bcast( strasl,  ionode_id)
            CALL mp_bcast( sname,   ionode_id)
            CALL mp_bcast( ierr,    ionode_id)
            !
       CASE ( 'pw_export' )
            !
            CALL qexpt_read_symmetry( S=srot, TRASL=strasl, SNAME=sname, IERR=ierr )
            !
            CALL mp_bcast( srot,    ionode_id)
            CALL mp_bcast( strasl,  ionode_id)
            CALL mp_bcast( sname,   ionode_id)
            CALL mp_bcast( ierr,    ionode_id)
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

