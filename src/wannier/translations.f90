!
! Copyright (C) 2012 WanT Group
!
! This file is distributed under the terms of the
! GNU General Public License. See the file `License\'
! in the root directory of the present distribution,
! or http://www.gnu.org/copyleft/gpl.txt .
!

!*********************************************
   MODULE translations_module
   !*********************************************
   !
   USE kinds,            ONLY : dbl
   USE constants,        ONLY : ZERO
   USE parameters,       ONLY : nstrx
   USE io_module,        ONLY : ionode, ionode_id
   USE timing_module,    ONLY : timing
   USE log_module,       ONLY : log_push, log_pop
   USE iotk_module
   !
   IMPLICIT NONE
   PRIVATE
   SAVE

! This module handles data referring to OVERLAP among
! the periodic part of bloch wfcs and thier projections
! onto the localized starting orbitals.
!
! routines in this module:
! SUBROUTINE translations_allocate()
! SUBROUTINE translations_deallocate()
! SUBROUTINE translations_write(iunit)
! SUBROUTINE translations_read(iunit)

!
! declarations of common variables
!   

   INTEGER                     :: ndim            ! dimensions
   INTEGER                     :: ndimx           ! leading dims
   INTEGER                     :: nvect           ! number of vectors
   REAL(dbl),    ALLOCATABLE   :: rvect(:,:)      ! translation vectors
   COMPLEX(dbl), ALLOCATABLE   :: transl(:,:,:)   ! 
   CHARACTER(256)              :: basis           ! basis of the matrix elements
                                                  ! DIM: ndimx,ndimx,nvect
   LOGICAL :: alloc = .FALSE.
   

!
! end of declarations
!

   PUBLIC :: ndimx, ndim, nvect, rvect, transl, basis
   !
   PUBLIC :: translations_allocate
   PUBLIC :: translations_deallocate
   PUBLIC :: translations_memusage
   PUBLIC :: translations_write
   PUBLIC :: translations_read
   PUBLIC :: alloc

CONTAINS

!
! subroutines
!

!**********************************************************
   SUBROUTINE translations_allocate(ndimx_, nvect_)
   !**********************************************************
   !
   IMPLICIT NONE
       !
       INTEGER,   INTENT(IN) :: ndimx_, nvect_
       !
       CHARACTER(21)      :: subname="translations_allocate"
       INTEGER            :: ierr 

       CALL log_push( subname )
       !
       IF ( ndimx_ <= 0 )   CALL errore(subname,'Invalid ndimx_',1)
       IF ( nvect_ < 0    ) CALL errore(subname,'Invalid nvect_',1)
       !
       ndimx = ndimx_
       nvect = nvect_
       !
       ALLOCATE( rvect(3,nvect), STAT=ierr )
       IF ( ierr/=0 ) CALL errore(subname,'allocating rvect', ABS(ierr) )
       ALLOCATE( transl(ndimx,ndimx,nvect), STAT=ierr )
       IF ( ierr/=0 ) CALL errore(subname,'allocating transl', ABS(ierr) )

       alloc = .TRUE. 
       !
       CALL log_pop( subname )
       ! 
   END SUBROUTINE translations_allocate


!**********************************************************
   SUBROUTINE translations_deallocate()
   !**********************************************************
   IMPLICIT NONE
       CHARACTER(23)      :: subname="translations_deallocate"
       INTEGER            :: ierr 

       CALL log_push( subname )
       !
       IF ( ALLOCATED(rvect) ) THEN
            DEALLOCATE(rvect, STAT=ierr)
            IF (ierr/=0)  CALL errore(subname,'deallocating rvect',ABS(ierr))
       ENDIF
       IF ( ALLOCATED(transl) ) THEN
            DEALLOCATE(transl, STAT=ierr)
            IF (ierr/=0)  CALL errore(subname,'deallocating transl',ABS(ierr))
       ENDIF
       !
       alloc = .FALSE.
       !
       CALL log_pop( subname )
       !
   END SUBROUTINE translations_deallocate


!**********************************************************
   REAL(dbl) FUNCTION translations_memusage()
   !**********************************************************
   IMPLICIT NONE
       !
       REAL(dbl) :: cost
       !
       cost = ZERO
       IF ( ALLOCATED(rvect) )  cost = cost + REAL(SIZE(rvect))  *  8.0_dbl
       IF ( ALLOCATED(transl) ) cost = cost + REAL(SIZE(transl)) * 16.0_dbl
       !
       translations_memusage = cost / 1000000.0_dbl
       !
   END FUNCTION translations_memusage


!**********************************************************
   SUBROUTINE translations_write(iunit)
   !**********************************************************
   !
   IMPLICIT NONE
       INTEGER,         INTENT(in) :: iunit
       !
       CHARACTER(18)               :: subname="translations_write"
       CHARACTER(nstrx)            :: attr
       INTEGER                     :: j
       

       IF ( .NOT. alloc ) RETURN
       !
       CALL timing   ( subname, OPR='start' )
       CALL log_push ( subname )
       !
       IF ( ionode ) THEN
           !
           CALL iotk_write_begin( iunit, "TRANSLATION_OPERATORS")
           !   
           CALL iotk_write_attr( attr, "ndim",  ndim, FIRST=.TRUE.)
           CALL iotk_write_attr( attr, "ndimx", ndimx )
           CALL iotk_write_attr( attr, "nvect", nvect )
           CALL iotk_write_attr( attr, "basis", TRIM(basis) )
           CALL iotk_write_empty(iunit, "DATA", ATTR=attr )
           !   
           CALL iotk_write_attr( attr, "units", "bohr", FIRST=.TRUE.)
           CALL iotk_write_dat(iunit, "TRASL_VECTORS", rvect, ATTR=attr, COLUMNS=3 )
           !   
           DO j = 1, nvect
               CALL iotk_write_dat(iunit, "TRANSLATION"//TRIM(iotk_index(j)), transl(:,:,j) )
           ENDDO
           !   
           CALL iotk_write_end( iunit, "TRANSLATION_OPERATORS")
           !
       ENDIF
       !
   END SUBROUTINE translations_write


!**********************************************************
   SUBROUTINE translations_read( iunit, nvect_in, rvect_in )
   !**********************************************************
   !
   ! allocate (if necessary) and read translations data
   !
   USE constants,  ONLY : EPS_m6
   USE mp,         ONLY : mp_bcast
   !
   IMPLICIT NONE
       !
       INTEGER,              INTENT(in) :: iunit
       INTEGER,   OPTIONAL,  INTENT(in) :: nvect_in
       REAL(dbl), OPTIONAL,  INTENT(in) :: rvect_in(3,*)
       !
       CHARACTER(17)    :: subname="translations_read"
       CHARACTER(nstrx) :: attr, basis_
       INTEGER          :: i, j, ierr
       INTEGER          :: ndimx_, ndim_, nvect_
       LOGICAL          :: select_rvect = .FALSE.
       LOGICAL          :: lfound
       REAL(dbl)        :: rtmp
       !
       REAL(dbl),    ALLOCATABLE  :: rvect_(:,:)
       COMPLEX(dbl), ALLOCATABLE  :: transl_(:,:,:)
      


       !
       ! main body
       !
       CALL timing( subname, OPR='start' )
       CALL log_push( subname )

       IF ( alloc ) CALL translations_deallocate( )

       IF ( PRESENT( nvect_in ) .AND. PRESENT( rvect_in ) ) select_rvect = .TRUE.
       !
       !
       IF ( ionode ) THEN
           !
           CALL iotk_scan_begin(iunit, "TRANSLATION_OPERATORS", IERR=ierr)
           IF ( ierr/=0 ) CALL errore(subname,"searching TRANSLATION_OPERATORS",ABS(ierr))
           !
           CALL iotk_scan_empty(iunit, "DATA", ATTR=attr, IERR=ierr)
           IF ( ierr/=0 ) CALL errore(subname,"searching DATA",ABS(ierr))
           !
           CALL iotk_scan_attr(attr, "ndim", ndim_, IERR=ierr)
           IF ( ierr/=0 ) CALL errore(subname,"searching ndim",ABS(ierr))
           CALL iotk_scan_attr(attr, "ndimx", ndimx_, IERR=ierr)
           IF ( ierr/=0 ) CALL errore(subname,"searching ndimx",ABS(ierr))
           CALL iotk_scan_attr(attr, "nvect", nvect_, IERR=ierr)
           IF ( ierr/=0 ) CALL errore(subname,"searching nvect",ABS(ierr))
           !
       ENDIF
       !
       CALL mp_bcast( ndim_,   ionode_id)
       CALL mp_bcast( ndimx_,  ionode_id)
       CALL mp_bcast( nvect_,  ionode_id)
       !
       ALLOCATE( rvect_(3, nvect_), STAT=ierr )
       IF ( ierr/=0 ) CALL errore(subname,"allocating rvect_",ABS(ierr))
       ALLOCATE( transl_(ndimx_, ndimx_, nvect_), STAT=ierr )
       IF ( ierr/=0 ) CALL errore(subname,"allocating transl_",ABS(ierr))
       !
       !
       IF ( ionode ) THEN
           !
           CALL iotk_scan_attr(attr, "basis", basis_, IERR=ierr)
           IF ( ierr/=0 ) CALL errore(subname,"searching basis",ABS(ierr))
           !
           CALL iotk_scan_dat(iunit, "TRASL_VECTORS",  rvect_, IERR=ierr)
           IF ( ierr/=0 ) CALL errore(subname,"searching TRASL_VECTORS",ABS(ierr))
           !
       ENDIF
       !
       CALL mp_bcast( basis_,  ionode_id)
       CALL mp_bcast( rvect_,  ionode_id)

      
       !
       ! read the translation operators
       !
       IF ( ionode ) THEN
           !
           DO j = 1, nvect_ 
               !
               CALL iotk_scan_dat( iunit, "TRANSLATION"//TRIM(iotk_index(j)), transl_(:,:,j), IERR=ierr )    
               IF ( ierr/=0 ) CALL errore(subname,"scanning TRANSLATION",j)
               !
           ENDDO
           !
           CALL iotk_scan_end(iunit, "TRANSLATION_OPERATORS", IERR=ierr)
           IF ( ierr/=0 ) CALL errore(subname,"searching end TRANSLATION_OPERATORS",ABS(ierr))
           !
       ENDIF
       !
       CALL mp_bcast( transl_,  ionode_id)

       !
       ! alloc module
       !
       IF ( select_rvect ) THEN
           !
           CALL translations_allocate(ndimx_, nvect_in)
           !
           rvect(:,1:nvect_in)  = rvect_in(:,1:nvect_in)
           !
           DO j = 1, nvect_in
               !
               lfound = .FALSE.
               DO i = 1, nvect_
                   !
                   rtmp = DOT_PRODUCT( rvect_(:,i)-rvect_in(:,j), rvect_(:,i)-rvect_in(:,j))
                   !
                   IF ( rtmp < EPS_m6 ) THEN
                       lfound=.TRUE.
                       transl(:,:,j) = transl_(:,:,i) 
                   ENDIF
                   !
               ENDDO
               !
               IF ( .NOT. lfound ) CALL errore(subname,"R index not found",j)
               !
           ENDDO
           !
       ELSE
           !
           CALL translations_allocate(ndimx_, nvect_)
           !
           rvect  = rvect_
           transl = transl_
           !
       ENDIF
       !
       ndim   = ndim_
       basis  = basis_

       !
       ! cleanup
       !
       DEALLOCATE( rvect_, STAT=ierr )
       IF ( ierr/=0 ) CALL errore(subname,"deallocating rvect_",ABS(ierr))
       DEALLOCATE( transl_, STAT=ierr )
       IF ( ierr/=0 ) CALL errore(subname,"deallocating transl_",ABS(ierr))
       !
       !
       CALL timing  ( subname, OPR='stop' )
       CALL log_pop ( subname )
       !
   END SUBROUTINE translations_read

END MODULE translations_module

