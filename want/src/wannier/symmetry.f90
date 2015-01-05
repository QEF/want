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
   USE constants,         ONLY : ZERO, TPI
   USE parameters,        ONLY : nstrx
   USE log_module,        ONLY : log_push, log_pop
   USE io_global_module,  ONLY : ionode, ionode_id
   USE ions_module,       ONLY : nat_ => nat, tau, ions_alloc => alloc
   USE lattice_module,    ONLY : avec, bvec, alat, lattice_alloc => alloc
   USE mp,                ONLY : mp_bcast
   USE converters_module, ONLY : cry2cart, cart2cry
   USE util_module
   USE qexml_module
   USE qexpt_module
   !
#ifdef __ETSF_IO
   USE etsf_io
   USE etsf_io_tools
   USE etsf_io_data_module
#endif
   ! 
   IMPLICIT NONE
   PRIVATE
   SAVE
!
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
   REAL(dbl),        ALLOCATABLE :: srrot(:,:,:)  ! operations, 3x3xNsym, cart. units 
   REAL(dbl),        ALLOCATABLE :: strasl(:,:)   ! frac. traslations, 3xNym, cryst. units
   REAL(dbl),        ALLOCATABLE :: srtrasl(:,:)  ! frac. traslations, 3xNym, cryst. units
   !
   CHARACTER(nstrx), ALLOCATABLE :: sname(:)      ! symmetry names
   INTEGER                       :: nat=0         ! natoms
   INTEGER,          ALLOCATABLE :: irt(:,:)      ! (nsym,nat) atom map upon the action of symmetries
   INTEGER,          ALLOCATABLE :: icell(:,:,:)  ! (3,nsym,nat) gives the reference cell after the symmetry is operated
   ! 
   REAL(dbl),        ALLOCATABLE :: d1(:,:,:)     ! (3,3,nsym), (5,5,nsym), (7,7,nsym)
   REAL(dbl),        ALLOCATABLE :: d2(:,:,:)     ! matrices for rotating spherical
   REAL(dbl),        ALLOCATABLE :: d3(:,:,:)     ! harmonics (d1 for l=1, ...)
   !
   LOGICAL :: alloc = .FALSE.

!
! end of declarations
!

   PUBLIC :: nsym
   PUBLIC :: srot
   PUBLIC :: strasl
   PUBLIC :: sname
   PUBLIC :: irt, nat, icell
   PUBLIC :: d1, d2, d3
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
       IF ( nsym <= 0 ) CALL errore(subname,'Invalid nsym',ABS(nsym)+1)

       ALLOCATE( srot(3, 3, nsym ), STAT = ierr )
       IF( ierr /=0 ) CALL errore(subname, 'allocating srot', ABS(ierr) )
       ALLOCATE( srrot(3, 3, nsym ), STAT = ierr )
       IF( ierr /=0 ) CALL errore(subname, 'allocating srrot', ABS(ierr) )
       ALLOCATE( strasl( 3, nsym ), STAT = ierr )
       IF( ierr /=0 ) CALL errore(subname, 'allocating strasl', ABS(ierr) )
       ALLOCATE( srtrasl( 3, nsym ), STAT = ierr )
       IF( ierr /=0 ) CALL errore(subname, 'allocating srtrasl', ABS(ierr) )
       ALLOCATE( sname( nsym ), STAT = ierr )
       IF( ierr /=0 ) CALL errore(subname, 'allocating sname', ABS(ierr) )
       !
       IF ( nat > 0 ) THEN
           ALLOCATE( irt( nsym, nat ), STAT = ierr )
           IF( ierr /=0 ) CALL errore(subname, 'allocating irt', ABS(ierr) )
           ALLOCATE( icell( 3, nsym, nat ), STAT = ierr )
           IF( ierr /=0 ) CALL errore(subname, 'allocating icell', ABS(ierr) )
       ENDIF
       !
       ALLOCATE( d1(3,3,nsym), d2(5,5,nsym), d3(7,7,nsym), STAT=ierr )
       IF( ierr /=0 ) CALL errore(subname, 'allocating d1,d2,d3', ABS(ierr) )
       !
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
           IF (ierr/=0)  CALL errore(subname,'deallocating srot',ABS(ierr))
       ENDIF
       IF ( ALLOCATED(srrot) ) THEN 
           DEALLOCATE(srrot, STAT=ierr)
           IF (ierr/=0)  CALL errore(subname,'deallocating srrot',ABS(ierr))
       ENDIF
       IF ( ALLOCATED(strasl) ) THEN 
           DEALLOCATE(strasl, STAT=ierr)
           IF (ierr/=0)  CALL errore(subname,'deallocating strasl',ABS(ierr))
       ENDIF
       IF ( ALLOCATED(srtrasl) ) THEN 
           DEALLOCATE(srtrasl, STAT=ierr)
           IF (ierr/=0)  CALL errore(subname,'deallocating srtrasl',ABS(ierr))
       ENDIF
       IF ( ALLOCATED(sname) ) THEN 
           DEALLOCATE(sname, STAT=ierr)
           IF (ierr/=0)  CALL errore(subname,'deallocating sname',ABS(ierr))
       ENDIF
       IF ( ALLOCATED(irt) ) THEN 
           DEALLOCATE(irt, STAT=ierr)
           IF (ierr/=0)  CALL errore(subname,'deallocating irt',ABS(ierr))
       ENDIF
       IF ( ALLOCATED(icell) ) THEN 
           DEALLOCATE(icell, STAT=ierr)
           IF (ierr/=0)  CALL errore(subname,'deallocating icell',ABS(ierr))
       ENDIF
       IF ( ALLOCATED(d1) ) THEN 
           DEALLOCATE(d1, d2, d3, STAT=ierr)
           IF (ierr/=0)  CALL errore(subname,'deallocating d1,d2,d3',ABS(ierr))
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
       IF ( ALLOCATED(srrot) )    cost = cost + REAL(SIZE(srrot))      * 8.0_dbl
       IF ( ALLOCATED(strasl) )   cost = cost + REAL(SIZE(strasl))     * 8.0_dbl
       IF ( ALLOCATED(srtrasl) )  cost = cost + REAL(SIZE(srtrasl))    * 8.0_dbl
       IF ( ALLOCATED(irt) )      cost = cost + REAL(SIZE(irt))        * 4.0_dbl
       IF ( ALLOCATED(icell) )    cost = cost + REAL(SIZE(icell))      * 4.0_dbl
       IF ( ALLOCATED(d1) )       cost = cost + REAL(SIZE(d1))         * 8.0_dbl
       IF ( ALLOCATED(d2) )       cost = cost + REAL(SIZE(d2))         * 8.0_dbl
       IF ( ALLOCATED(d3) )       cost = cost + REAL(SIZE(d3))         * 8.0_dbl
       IF ( ALLOCATED(sname) )    cost = cost + REAL(SIZE(sname))      * nstrx * 4.0_dbl
       !
       symmetry_memusage = cost / 1000000.0_dbl
       !
   END FUNCTION symmetry_memusage


!**********************************************************
   SUBROUTINE symmetry_rotate(vect, iopr)
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
       INTEGER,   INTENT(IN)    :: iopr(3,3)
       ! 
       REAL(dbl) :: rtmp(3)
 
       rtmp(1) = DOT_PRODUCT( REAL(iopr(1,:), dbl), vect(:) )
       rtmp(2) = DOT_PRODUCT( REAL(iopr(2,:), dbl), vect(:) )
       rtmp(3) = DOT_PRODUCT( REAL(iopr(3,:), dbl), vect(:) )
       !
       vect(:) = rtmp(:)
       ! 
   END SUBROUTINE symmetry_rotate


!*********************************************************
   SUBROUTINE symmetry_read_ext( filefmt )
   !*********************************************************
   IMPLICIT NONE
       CHARACTER(*),  INTENT(in) :: filefmt
       !
       CHARACTER(17)  :: subname="symmetry_read_ext"
       INTEGER        :: ierr, is, ia, iaeq
       REAL(dbl)      :: rvec(3)
       REAL(dbl), ALLOCATABLE :: tau_cry(:,:)
       !
#ifdef __ETSF_IO
       TYPE(etsf_geometry)  :: geometry
       INTEGER,          ALLOCATABLE, TARGET :: reduced_symmetry_matrices(:,:,:)
       DOUBLE PRECISION, ALLOCATABLE, TARGET :: reduced_symmetry_translations(:,:)
#endif

       CALL log_push( subname )
       !
       SELECT CASE ( TRIM(filefmt) )
       !
       CASE ( 'qexml' )
            !
            IF (ionode) CALL qexml_read_symmetry( NSYM=nsym, NAT=nat, IERR=ierr )
            CALL mp_bcast( nsym, ionode_id)
            CALL mp_bcast( nat,  ionode_id)
            CALL mp_bcast( ierr, ionode_id)
            !
       CASE ( 'pw_export' )
            !
            IF (ionode) CALL qexpt_read_symmetry( NSYM=nsym, IERR=ierr )
            CALL mp_bcast( nsym, ionode_id)
            CALL mp_bcast( ierr, ionode_id)
            !
       CASE ( 'etsf_io' )
            !
#ifdef __ETSF_IO
            !
            nsym  = dims%number_of_symmetry_operations
            ierr = 0
            !
#else
            CALL errore(subname,'ETSF_IO fmt not configured',71)
#endif
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
       IF ( nat > 0 .AND. ions_alloc ) THEN
           IF ( nat /= nat_ ) CALL errore(subname,'invalid nat',10)
       ENDIF

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
            IF (ionode) THEN 
               IF (nat > 0  ) CALL qexml_read_symmetry( S=srot, TRASL=strasl, SNAME=sname, IRT=irt, IERR=ierr )
               IF (nat <= 0 ) CALL qexml_read_symmetry( S=srot, TRASL=strasl, SNAME=sname, IERR=ierr )
            ENDIF
            !
            CALL mp_bcast( srot,    ionode_id)
            CALL mp_bcast( strasl,  ionode_id)
            CALL mp_bcast( sname,   ionode_id)
            CALL mp_bcast( ierr,    ionode_id)
            !
            IF (nat > 0 ) CALL mp_bcast( irt,     ionode_id)
            !
       CASE ( 'pw_export' )
            !
            IF (ionode) CALL qexpt_read_symmetry( S=srot, TRASL=strasl, SNAME=sname, IERR=ierr )
            !
            CALL mp_bcast( srot,    ionode_id)
            CALL mp_bcast( strasl,  ionode_id)
            CALL mp_bcast( sname,   ionode_id)
            CALL mp_bcast( ierr,    ionode_id)
            !
       CASE( 'etsf_io' )
            !
#ifdef __ETSF_IO
            !
            ALLOCATE( reduced_symmetry_matrices(dims%number_of_reduced_dimensions,     &
                                                dims%number_of_reduced_dimensions,     &
                                                dims%number_of_symmetry_operations) )
            ALLOCATE( reduced_symmetry_translations(dims%number_of_reduced_dimensions, &
                                                dims%number_of_symmetry_operations) )
            !
            geometry%reduced_symmetry_matrices       => reduced_symmetry_matrices
            geometry%reduced_symmetry_translations   => reduced_symmetry_translations
            !
            IF (ionode) CALL etsf_io_geometry_get(ncid, geometry, lstat, error_data)
            !
            geometry%reduced_symmetry_matrices       => null()
            geometry%reduced_symmetry_translations   => null()
            !
            CALL mp_bcast( reduced_symmetry_matrices,      ionode_id)
            CALL mp_bcast( reduced_symmetry_translations,  ionode_id)
            CALL mp_bcast( lstat,    ionode_id)
            !
            ierr = 0
            IF ( .NOT. lstat) THEN
                 ierr = 10
                 CALL etsf_error(error_data,subname,'reading ETSF_IO data',ierr)
            ENDIF
            !
            strasl(:,:) = reduced_symmetry_translations(:,:)
            srot(:,:,:) = reduced_symmetry_matrices(:,:,:)
            sname(:)    = " "
            !
            DEALLOCATE( reduced_symmetry_matrices )
            DEALLOCATE( reduced_symmetry_translations )
            !
#endif
            !
       CASE DEFAULT
            !
            CALL errore(subname,'invalid filefmt = '//TRIM(filefmt), 2)
       END SELECT
       !
       IF ( ierr/=0) CALL errore(subname,'getting symmetry data',ABS(ierr))

       !
       ! further init if possible
       !
       IF ( lattice_alloc ) THEN
           !
           ! srrot = avec * srot * bvec^dag / 2pi
           srrot(:,:,:) = REAL(srot(:,:,:), dbl)
           !
           DO is = 1, nsym
               CALL mat_mul( srrot(:,:,is), srrot(:,:,is), 'N', avec, 'T', 3,3,3) 
               CALL mat_mul( srrot(:,:,is), bvec, 'N', srrot(:,:,is), 'N', 3,3,3) 
           ENDDO 
           !
           srrot = srrot / TPI
           !
           srtrasl = strasl
           CALL cry2cart( srtrasl, avec )

           !
           ! init rotation matrices for spherical harmonics
           !
           CALL d_matrix( nsym, srrot, d1, d2, d3)
           !
       ELSE
           srrot = 0.0
           d1 = 0.0
           d2 = 0.0
           d3 = 0.0
       ENDIF
       !
       ! we need irt and other specific inputs from the DFT engine
       !
       IF ( ions_alloc .AND. lattice_alloc .AND. TRIM(filefmt)== "qexml" ) THEN
           !
           ALLOCATE( tau_cry(3,nat) )
           tau_cry = tau
           !
           CALL cart2cry( tau_cry, avec )
           !
           DO ia = 1, nat
               !
               DO is = 1, nsym
                   !
                   CALL mat_mul( rvec(:), REAL(srot(:,:,is),dbl), "T", tau_cry(:,ia), 3, 3 )
                   !
                   iaeq=irt(is,ia)
                   !
                   icell(:,is,ia) = NINT( rvec(:)-tau_cry(:,iaeq) ) 
! XXXX
!WRITE(0,"(a,2i4,3f15.9)") "is, ia: rcell", is, ia, rvec(:)-tau_cry(:,iaeq)
!WRITE(0,"(a,2i4,3i6)") "is, ia: icell", is, ia,  icell(:,is,ia)
                   !
               ENDDO
           ENDDO
           !
       ENDIF
      
       ! 
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

