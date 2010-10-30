
! Copyright (C) 2008 WanT Group
!
! This file is distributed under the terms of the
! GNU General Public License. See the file `License\'
! in the root directory of the present distribution,
! or http://www.gnu.org/copyleft/gpl.txt .
!

!*********************************************
   MODULE T_operator_blc_module
!*********************************************
   !   
   USE kinds,               ONLY : dbl 
   USE parameters,          ONLY : nstrx
   USE parser_module,       ONLY : change_case, log2char
   USE log_module,          ONLY : log_push, log_pop
   !
   IMPLICIT NONE
   PRIVATE
   !
   ! This module defines the TYPE operator_blc, used to
   ! managed hamiltonian, overlaps and sigma blocks in 
   ! transport
   !
   ! routines in this module:
   ! SUBROUTINE operator_blc_init(obj)
   ! SUBROUTINE operator_blc_allocate()
   ! SUBROUTINE operator_blc_deallocate()
   !

   TYPE operator_blc
        !
        CHARACTER(50)           :: blc_name     ! name of the block
        INTEGER                 :: dim1         ! dims
        INTEGER                 :: dim2         !
        INTEGER                 :: nkpts        !
        INTEGER                 :: nrtot        ! number of 3D R-vects for H and S
        INTEGER                 :: nrtot_sgm    ! number of 3D R-vects for SGM
        LOGICAL                 :: lhave_aux    ! have aux workspace
        LOGICAL                 :: lhave_sgm_aux! have sgm_aux workspace
        LOGICAL                 :: lhave_ham    ! have hamiltonian
        LOGICAL                 :: lhave_ovp    ! have overlap
        LOGICAL                 :: lhave_corr   ! have correlation
        LOGICAL                 :: ldynam_corr  ! dynamical correlation
        INTEGER                 :: ie           ! index of current energy-point
        INTEGER                 :: ik           ! index of current k-point
        CHARACTER(nstrx)        :: tag          ! attr tag from stdin
        !
        INTEGER,      POINTER   :: icols(:)     ! indexes used to read data 
        INTEGER,      POINTER   :: irows(:)     !
        INTEGER,      POINTER   :: ivr(:,:)     ! 3D lattice vectors for H and S
        !
        INTEGER,      POINTER   :: icols_sgm(:) ! indexes used to read data 
        INTEGER,      POINTER   :: irows_sgm(:) !
        INTEGER,      POINTER   :: ivr_sgm(:,:) ! 3D lattice vectors for Sgm
        !
        COMPLEX(dbl), POINTER   :: H(:,:,:)     ! hamiltonian mtrx elements
        COMPLEX(dbl), POINTER   :: S(:,:,:)     ! overlaps
        COMPLEX(dbl), POINTER   :: sgm(:,:,:)   ! correlation self-energy
        COMPLEX(dbl), POINTER   :: aux(:,:)     ! correlation self-energy
        COMPLEX(dbl), POINTER   :: sgm_aux(:,:) ! auxiliary (smearing) self-energy
        !
        LOGICAL                 :: alloc
        !
   END TYPE operator_blc

   INTERFACE ASSIGNMENT(=)
      MODULE PROCEDURE operator_blc_copy
   END INTERFACE
   
   PUBLIC :: operator_blc
   PUBLIC :: ASSIGNMENT(=)
   PUBLIC :: operator_blc_init
   PUBLIC :: operator_blc_copy
   PUBLIC :: operator_blc_update
   PUBLIC :: operator_blc_allocate
   PUBLIC :: operator_blc_deallocate
   PUBLIC :: operator_blc_write


CONTAINS

!****************************************************
   SUBROUTINE operator_blc_init(obj,blc_name)
   !****************************************************
   IMPLICIT NONE
      TYPE( operator_blc ),   INTENT(OUT) :: obj 
      CHARACTER(*), OPTIONAL, INTENT(IN)  :: blc_name
      !
      obj%blc_name=" "
      IF ( PRESENT( blc_name ) ) obj%blc_name = TRIM( blc_name )
      obj%dim1=0
      obj%dim2=0
      obj%nkpts=0
      obj%nrtot=0
      obj%nrtot_sgm=0
      obj%lhave_aux=.FALSE.
      obj%lhave_sgm_aux=.FALSE.
      obj%lhave_ovp=.FALSE.
      obj%lhave_corr=.FALSE.
      obj%ldynam_corr=.FALSE.
      obj%ie=0
      obj%ik=0
      obj%tag=" " 
      NULLIFY( obj%icols )
      NULLIFY( obj%irows )
      NULLIFY( obj%icols_sgm )
      NULLIFY( obj%irows_sgm )
      NULLIFY( obj%H )
      NULLIFY( obj%S )
      NULLIFY( obj%sgm )
      NULLIFY( obj%aux )
      NULLIFY( obj%sgm_aux )
      NULLIFY( obj%ivr )
      NULLIFY( obj%ivr_sgm )
      obj%alloc=.FALSE.
      !
   END SUBROUTINE operator_blc_init


!****************************************************
   SUBROUTINE operator_blc_copy( obj2, obj1)
   !****************************************************
   !
   ! assign obj1 to obj2 
   ! obj2 = obj1
   !
   IMPLICIT NONE
      !
      TYPE(operator_blc),   INTENT(IN)    :: obj1
      TYPE(operator_blc),   INTENT(INOUT) :: obj2
      !
      CHARACTER(17) :: subname='operator_blc_copy'
      !
      CALL log_push(subname)
      IF ( .NOT. obj1%alloc ) CALL errore(subname,'obj1 not alloc',10)
      !
      CALL operator_blc_deallocate( obj2 )
      !
      CALL operator_blc_allocate( obj1%dim1, obj1%dim2, obj1%nkpts, &
                                  NRTOT=obj1%nrtot, NRTOT_SGM=obj1%nrtot_sgm, &
                                  LHAVE_AUX=obj1%lhave_aux, LHAVE_SGM_AUX=obj1%lhave_sgm_aux, &
                                  LHAVE_HAM=obj1%lhave_ham, &
                                  LHAVE_OVP=obj1%lhave_ovp, LHAVE_CORR=obj1%lhave_corr, &
                                  BLC_NAME=TRIM(obj1%blc_name), OBJ=obj2 )
      !
      obj2%ldynam_corr = obj1%ldynam_corr
      obj2%ie = obj1%ie
      obj2%ik = obj1%ik
      obj2%tag = TRIM( obj1%tag )
      !
      IF ( ASSOCIATED( obj1%icols ) )      obj2%icols = obj1%icols
      IF ( ASSOCIATED( obj1%irows ) )      obj2%irows = obj1%irows
      IF ( ASSOCIATED( obj1%icols_sgm ) )  obj2%icols_sgm = obj1%icols_sgm
      IF ( ASSOCIATED( obj1%irows_sgm ) )  obj2%irows_sgm = obj1%irows_sgm
      IF ( ASSOCIATED( obj1%H ) )          obj2%H = obj1%H
      IF ( ASSOCIATED( obj1%S ) )          obj2%S = obj1%S
      IF ( ASSOCIATED( obj1%sgm ) )        obj2%sgm = obj1%sgm
      IF ( ASSOCIATED( obj1%aux ) )        obj2%aux = obj1%aux
      IF ( ASSOCIATED( obj1%sgm_aux ) )    obj2%sgm_aux = obj1%sgm_aux
      IF ( ASSOCIATED( obj1%ivr ) )        obj2%ivr = obj1%ivr
      IF ( ASSOCIATED( obj1%ivr_sgm ) )    obj2%ivr_sgm = obj1%ivr_sgm
      !
      obj2%alloc = obj1%alloc
      !
      CALL log_pop(subname)
      RETURN
   END SUBROUTINE operator_blc_copy


!****************************************************
   SUBROUTINE operator_blc_update(nrtot, nrtot_sgm, ie, ik, ldynam_corr, tag, blc_name, obj )
   !****************************************************
   IMPLICIT NONE
      !
      INTEGER,       OPTIONAL, INTENT(IN) :: nrtot, nrtot_sgm, ie, ik
      LOGICAL,       OPTIONAL, INTENT(IN) :: ldynam_corr
      CHARACTER(*),  OPTIONAL, INTENT(IN) :: blc_name, tag
      TYPE(operator_blc),   INTENT(INOUT) :: obj
      !
      CHARACTER(19) :: subname='operator_blc_update'
      !
      CALL log_push(subname)
      IF ( .NOT. obj%alloc ) CALL errore(subname,'obj not alloc',10)
      !
      IF ( PRESENT( nrtot ) )      obj%nrtot = nrtot
      IF ( PRESENT( nrtot_sgm ) )  obj%nrtot_sgm = nrtot_sgm
      IF ( PRESENT( ie ) )         obj%ie = ie
      IF ( PRESENT( ik ) )         obj%ik = ik
      IF ( PRESENT( ldynam_corr )) obj%ldynam_corr = ldynam_corr
      IF ( PRESENT( tag ))         obj%tag = TRIM(tag)
      IF ( PRESENT( blc_name ))    obj%blc_name = TRIM(blc_name)
      !
      CALL log_pop(subname)
      RETURN
      !
   END SUBROUTINE operator_blc_update


!****************************************************
   SUBROUTINE operator_blc_write( ounit, obj)
   !****************************************************
   !
   ! writes a summary of the data contained in obj
   !
   IMPLICIT NONE
      !
      INTEGER,              INTENT(IN)    :: ounit
      TYPE(operator_blc),   INTENT(IN)    :: obj
      !
      WRITE( ounit, "(/,2x,'         name : ',a)") TRIM(obj%blc_name)
      WRITE( ounit, "(  2x,'         dim1 : ',i5)") obj%dim1
      WRITE( ounit, "(  2x,'         dim2 : ',i5)") obj%dim2
      WRITE( ounit, "(  2x,'        nkpts : ',i5)") obj%nkpts
      WRITE( ounit, "(  2x,'        nrtot : ',i5)") obj%nrtot
      WRITE( ounit, "(  2x,'    nrtot_sgm : ',i5)") obj%nrtot_sgm
      WRITE( ounit, "(  2x,'     have_aux : ',a)") TRIM( log2char(obj%lhave_aux) )
      WRITE( ounit, "(  2x,' have_sgm_aux : ',a)") TRIM( log2char(obj%lhave_sgm_aux) )
      WRITE( ounit, "(  2x,'     have_ham : ',a)") TRIM( log2char(obj%lhave_ham) )
      WRITE( ounit, "(  2x,'     have_ovp : ',a)") TRIM( log2char(obj%lhave_ovp) )
      WRITE( ounit, "(  2x,'    have_corr : ',a)") TRIM( log2char(obj%lhave_corr) )
      WRITE( ounit, "(  2x,'     dyn_corr : ',a)") TRIM( log2char(obj%ldynam_corr) )
      WRITE( ounit, "(  2x,'           ie : ',i5)") obj%ie
      WRITE( ounit, "(  2x,'           ik : ',i5)") obj%ik
      WRITE( ounit, "(  2x,'          tag : ',a)") TRIM(obj%tag)
      !
   END SUBROUTINE operator_blc_write


!****************************************************
   SUBROUTINE operator_blc_allocate(dim1, dim2, nkpts, nrtot, nrtot_sgm, &
                                    lhave_aux, lhave_sgm_aux, lhave_ham, &
                                    lhave_ovp, lhave_corr, blc_name, obj)
   !****************************************************
   IMPLICIT NONE
      !
      INTEGER,                 INTENT(IN) :: dim1, dim2, nkpts 
      INTEGER,       OPTIONAL, INTENT(IN) :: nrtot, nrtot_sgm
      LOGICAL,       OPTIONAL, INTENT(IN) :: lhave_aux, lhave_sgm_aux, lhave_ham, lhave_ovp, lhave_corr
      CHARACTER(*),  OPTIONAL, INTENT(IN) :: blc_name
      TYPE(operator_blc),   INTENT(INOUT) :: obj
      !
      CHARACTER(21) :: subname='operator_blc_allocate'
      LOGICAL       :: lhave_aux_, lhave_sgm_aux_, lhave_ham_, lhave_ovp_, lhave_corr_
      INTEGER       :: ierr
   
      CALL log_push(subname)
      !
      ! some checks
      !
      IF ( dim1 <= 0 )  CALL errore(subname,'invalid dim1',10)
      IF ( dim2 <= 0 )  CALL errore(subname,'invalid dim2',10)
      IF ( nkpts <= 0 ) CALL errore(subname,'invalid nkpts',10)
 
      !
      ! modify & check some arguments
      !
      IF ( .NOT. obj%alloc ) THEN
          !
          obj%dim1        = dim1
          obj%dim2        = dim2
          obj%nkpts       = nkpts
          obj%nrtot       = 0
          obj%nrtot_sgm   = 0
          !
          IF ( PRESENT( nrtot) )           obj%nrtot = nrtot
          IF ( PRESENT( nrtot_sgm) )   obj%nrtot_sgm = nrtot_sgm
          !
      ELSE
          !
          IF ( obj%dim1 /= dim1 )   CALL errore(subname,'invalid dim1',11)
          IF ( obj%dim2 /= dim2 )   CALL errore(subname,'invalid dim2',11)
          IF ( obj%nkpts /= nkpts ) CALL errore(subname,'invalid nkpts',11)
          !
          IF ( PRESENT(nrtot)     .AND. .NOT. ASSOCIATED(obj%ivr) ) &
               obj%nrtot = nrtot
          IF ( PRESENT(nrtot_sgm) .AND. .NOT. ASSOCIATED(obj%ivr_sgm) ) &
               obj%nrtot_sgm = nrtot_sgm
          !
      ENDIF
      !
      lhave_aux_     = .TRUE.
      lhave_sgm_aux_ = .FALSE.
      lhave_ham_     = .TRUE.
      lhave_ovp_     = .TRUE.
      lhave_corr_    = .FALSE.
      !
      IF ( PRESENT( lhave_aux ) )          lhave_aux_ = lhave_aux
      IF ( PRESENT( lhave_sgm_aux ) )  lhave_sgm_aux_ = lhave_sgm_aux
      IF ( PRESENT( lhave_ham ) )          lhave_ham_ = lhave_ham
      IF ( PRESENT( lhave_ovp ) )          lhave_ovp_ = lhave_ovp 
      IF ( PRESENT( lhave_corr ) )        lhave_corr_ = lhave_corr
      !
      IF ( PRESENT( blc_name ) ) obj%blc_name = TRIM( blc_name )
 
      !
      ! allocations
      !
      IF ( .NOT. ASSOCIATED( obj%irows) ) THEN
          ALLOCATE( obj%irows(dim1), STAT=ierr )
          IF ( ierr/=0 ) CALL errore(subname,'allocating irows',ABS(ierr))
      ENDIF
      !
      IF ( .NOT. ASSOCIATED( obj%icols) ) THEN
          ALLOCATE( obj%icols(dim2), STAT=ierr )
          IF ( ierr/=0 ) CALL errore(subname,'allocating icols',ABS(ierr))
      ENDIF
      !
      IF ( .NOT. ASSOCIATED( obj%irows_sgm) ) THEN
          ALLOCATE( obj%irows_sgm(dim1), STAT=ierr )
          IF ( ierr/=0 ) CALL errore(subname,'allocating irows_sgm',ABS(ierr))
      ENDIF
      !
      IF ( .NOT. ASSOCIATED( obj%icols_sgm) ) THEN
          ALLOCATE( obj%icols_sgm(dim2), STAT=ierr )
          IF ( ierr/=0 ) CALL errore(subname,'allocating icols_sgm',ABS(ierr))
      ENDIF
      !
      IF ( lhave_aux_ .AND. .NOT. ASSOCIATED( obj%aux ) ) THEN
          !
          ALLOCATE( obj%aux(dim1,dim2), STAT=ierr )
          IF ( ierr/=0 ) CALL errore(subname,'allocating aux',ABS(ierr))
          !
          obj%lhave_aux = .TRUE.
          !
      ENDIF
      !
      IF ( lhave_sgm_aux_ .AND. .NOT. ASSOCIATED( obj%sgm_aux ) ) THEN
          !
          ALLOCATE( obj%sgm_aux(dim1,dim2), STAT=ierr )
          IF ( ierr/=0 ) CALL errore(subname,'allocating sgm_aux',ABS(ierr))
          !
          obj%lhave_sgm_aux = .TRUE.
          !
      ENDIF
      !
      IF ( lhave_ham_ .AND. .NOT. ASSOCIATED( obj%H ) ) THEN
          !
          ALLOCATE( obj%H(dim1,dim2,nkpts), STAT=ierr )
          IF ( ierr/=0 ) CALL errore(subname,'allocating H',ABS(ierr))
          !
          obj%lhave_ham = .TRUE.
          !
      ENDIF
      !
      IF ( lhave_ovp_ .AND. .NOT. ASSOCIATED( obj%S ) ) THEN
          !
          ALLOCATE( obj%S(dim1,dim2,nkpts), STAT=ierr )
          IF ( ierr/=0 ) CALL errore(subname,'allocating S',ABS(ierr))
          ! 
          obj%lhave_ovp = .TRUE.
          !
      ENDIF
      !
      IF ( lhave_corr_ .AND. .NOT. ASSOCIATED( obj%sgm ) ) THEN
          !
          ALLOCATE( obj%sgm(dim1,dim2,nkpts), STAT=ierr )
          IF ( ierr/=0 ) CALL errore(subname,'allocating sgm',ABS(ierr))
          !
          obj%lhave_corr = .TRUE.
          !
      ENDIF
      !
      IF ( PRESENT( nrtot ) .AND. nrtot /= 0 ) THEN
          !
          IF ( ASSOCIATED( obj%ivr ) ) CALL errore(subname,'ivr already alloc', 10)
          !
          ALLOCATE( obj%ivr(3,nrtot), STAT=ierr )
          IF ( ierr/=0 ) CALL errore(subname,'allocating ivr',ABS(ierr))
          !
      ENDIF
      !
      IF ( PRESENT( nrtot_sgm ) .AND. nrtot_sgm /= 0 ) THEN
          !
          IF ( ASSOCIATED( obj%ivr_sgm ) ) CALL errore(subname,'ivr_sgm already alloc', 10)
          !
          ALLOCATE( obj%ivr_sgm(3,nrtot_sgm), STAT=ierr )
          IF ( ierr/=0 ) CALL errore(subname,'allocating ivr_sgm',ABS(ierr))
          !
      ENDIF
      !
      obj%alloc = .TRUE.
      !
      CALL log_pop(subname)
      !
      RETURN
   END SUBROUTINE operator_blc_allocate
   !
   !
!****************************************************
   SUBROUTINE operator_blc_deallocate(obj)
   !****************************************************
   IMPLICIT NONE
      !
      TYPE(operator_blc),  INTENT(INOUT) :: obj
      !
      CHARACTER(23) :: subname='operator_blc_deallocate'
      INTEGER       :: ierr
      !
      IF ( .NOT. obj%alloc ) RETURN
      CALL log_push(subname)

      IF ( ASSOCIATED( obj%irows ) ) THEN
          DEALLOCATE( obj%irows, STAT=ierr)
          IF ( ierr/=0 ) CALL errore(subname,'deallocating irows',ABS(ierr))
      ENDIF
      IF ( ASSOCIATED( obj%icols ) ) THEN
          DEALLOCATE( obj%icols, STAT=ierr)
          IF ( ierr/=0 ) CALL errore(subname,'deallocating icols',ABS(ierr))
      ENDIF
      IF ( ASSOCIATED( obj%irows_sgm ) ) THEN
          DEALLOCATE( obj%irows_sgm, STAT=ierr)
          IF ( ierr/=0 ) CALL errore(subname,'deallocating irows_sgm',ABS(ierr))
      ENDIF
      IF ( ASSOCIATED( obj%icols_sgm ) ) THEN
          DEALLOCATE( obj%icols_sgm, STAT=ierr)
          IF ( ierr/=0 ) CALL errore(subname,'deallocating icols_sgm',ABS(ierr))
      ENDIF
      IF ( ASSOCIATED( obj%aux ) ) THEN
          DEALLOCATE( obj%aux, STAT=ierr)
          IF ( ierr/=0 ) CALL errore(subname,'deallocating aux',ABS(ierr))
      ENDIF
      IF ( ASSOCIATED( obj%sgm_aux ) ) THEN
          DEALLOCATE( obj%sgm_aux, STAT=ierr)
          IF ( ierr/=0 ) CALL errore(subname,'deallocating sgm_aux',ABS(ierr))
      ENDIF
      IF ( ASSOCIATED( obj%H ) ) THEN
          DEALLOCATE( obj%H, STAT=ierr)
          IF ( ierr/=0 ) CALL errore(subname,'deallocating H',ABS(ierr))
      ENDIF
      IF ( ASSOCIATED( obj%S ) ) THEN
          DEALLOCATE( obj%S, STAT=ierr)
          IF ( ierr/=0 ) CALL errore(subname,'deallocating S',ABS(ierr))
      ENDIF
      IF ( ASSOCIATED( obj%sgm ) ) THEN
          DEALLOCATE( obj%sgm, STAT=ierr)
          IF ( ierr/=0 ) CALL errore(subname,'deallocating sgm',ABS(ierr))
      ENDIF
      IF ( ASSOCIATED( obj%ivr ) ) THEN
          DEALLOCATE( obj%ivr, STAT=ierr)
          IF ( ierr/=0 ) CALL errore(subname,'deallocating ivr',ABS(ierr))
      ENDIF
      IF ( ASSOCIATED( obj%ivr_sgm ) ) THEN
          DEALLOCATE( obj%ivr_sgm, STAT=ierr)
          IF ( ierr/=0 ) CALL errore(subname,'deallocating ivr_sgm',ABS(ierr))
      ENDIF
      !
      obj%alloc = .FALSE.
      CALL operator_blc_init( obj )
      !
      CALL log_pop(subname)
      !
      RETURN
      !
   END SUBROUTINE operator_blc_deallocate

END MODULE T_operator_blc_module

