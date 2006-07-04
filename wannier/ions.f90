!
! Copyright (C) 2004 WanT Group
! 
! This file is distributed under the terms of the
! GNU General Public License. See the file `License'
! in the root directory of the present distribution,
! or http://www.gnu.org/copyleft/gpl.txt .
!
! Based on a previous version from the Qantum Espresso package
!
! <INFO>
!*********************************************************
MODULE ions_module
   !*********************************************************
   USE kinds,          ONLY : dbl
   USE constants,      ONLY : ZERO, BOHR => bohr_radius_angs
   USE parameters,     ONLY : ntypx, natx, nstrx
   USE io_module,      ONLY : pseudo_dir, prefix, work_dir
   USE qexml_module
   USE qexpt_module
   IMPLICIT NONE
   PRIVATE
   SAVE
!
! This module contains all the data related to atomic positions
! and atomic species.
!
! Subroutines in this module:
! SUBROUTINE ions_allocate( nat_, nsp_ )
! SUBROUTINE ions_deallocate()
! SUBROUTINE ions_init()
! SUBROUTINE ions_read_ext( filefmt )
! SUBROUTINE ions_tau_sort( tausrt, isrt, tau_, isp, na_ )
!
! </INFO>
!

      !     nsp       = number of species
      !     na(is)    = number of atoms of species is
      !     nax       = max number of atoms of a given species
      !     nat       = total number of atoms of all species

      INTEGER              :: nsp     = 0
      INTEGER, ALLOCATABLE :: na(:) 
      INTEGER              :: nax     = 0
      INTEGER              :: nat     = 0

      !     ityp( i ) = the type of i-th atom 
      !     atm_symb( j )  = name of the type of the j-th atomic specie
      !
      INTEGER,   ALLOCATABLE :: ityp(:)
      LOGICAL                :: uspp_calculation = .FALSE. 
      REAL(dbl)              :: ion_charge = ZERO      !  total ionic charge
      REAL(dbl), ALLOCATABLE :: zv(:)                  !  pseudo atomic charge
      REAL(dbl), ALLOCATABLE :: tau(:,:)               !  atomic positions (alat)
      REAL(dbl), ALLOCATABLE :: tau_srt(:,:)           !  tau sorted by specie (alat)
      INTEGER,   ALLOCATABLE :: ind_srt( : )           !  index of tau sorted by sp.

      CHARACTER(3),     ALLOCATABLE :: atm_symb(:)     !  DIM: nsp
      CHARACTER(3),     ALLOCATABLE :: symb(:)         !  DIM: nat
      CHARACTER(nstrx), ALLOCATABLE :: psfile(:)       !  DIM: nsp

      LOGICAL :: alloc = .FALSE.

!
! end of declaration scope
!

   PUBLIC :: nsp, na, nax, nat
   PUBLIC :: tau
   PUBLIC :: tau_srt, ind_srt
   PUBLIC :: zv, ion_charge
   PUBLIC :: ityp, atm_symb, symb
   PUBLIC :: uspp_calculation
   PUBLIC :: psfile
   PUBLIC :: alloc

   PUBLIC :: ions_allocate, ions_deallocate
   PUBLIC :: ions_read_ext, ions_init


CONTAINS

!**********************************************************
   SUBROUTINE ions_allocate( nat_, nsp_)
   !**********************************************************
   IMPLICIT NONE
      INTEGER :: nat_, nsp_
      CHARACTER(13)  :: subname='ions_allocate'
      INTEGER :: ierr

      IF ( nat_ <= 0) CALL errore(subname,'Invalid nat_',ABS(nat_)+1)
      IF ( nsp_ <= 0) CALL errore(subname,'Invalid nsp_',ABS(nsp_)+1)
      IF ( nat_ > natx ) CALL errore(subname,'Nat too large',nat_)
      IF ( nsp_ > ntypx ) CALL errore(subname,'Nsp too large',nsp_)
      nat = nat_
      nsp = nsp_

      ALLOCATE(na(nsp), STAT=ierr) 
         IF(ierr/=0) CALL errore(subname,'allocating na',nsp)
      ALLOCATE(zv(nsp), STAT=ierr) 
         IF(ierr/=0) CALL errore(subname,'allocating zv',nsp)
      ALLOCATE(ityp(nat), STAT=ierr) 
         IF(ierr/=0) CALL errore(subname,'allocating ityp',nat)
      ALLOCATE(symb(nat), STAT=ierr) 
         IF(ierr/=0) CALL errore(subname,'allocating symb',nat)
      ALLOCATE(tau(3,nat), STAT=ierr) 
         IF(ierr/=0) CALL errore(subname,'allocating tau',nat*nsp)
      ALLOCATE(tau_srt(3,nat), STAT=ierr) 
         IF(ierr/=0) CALL errore(subname,'allocating tau_srt',nat*nsp)
      ALLOCATE(ind_srt(nat), STAT=ierr) 
         IF(ierr/=0) CALL errore(subname,'allocating ind_srt',nat)
      ALLOCATE(atm_symb(nsp), STAT=ierr) 
         IF(ierr/=0) CALL errore(subname,'allocating atm_symb',nsp)
      ALLOCATE(psfile(nsp), STAT=ierr) 
         IF(ierr/=0) CALL errore(subname,'allocating psfile',nsp)

      alloc = .TRUE.
  END SUBROUTINE ions_allocate

!**********************************************************
   SUBROUTINE ions_deallocate( )
   !**********************************************************
   IMPLICIT NONE
      CHARACTER(15)  :: subname='ions_deallocate'
      INTEGER :: ierr

      IF ( ALLOCATED( na ) ) THEN
          DEALLOCATE( na, STAT=ierr )
          IF (ierr/=0) CALL errore(subname,'deallocating na',ABS(ierr))
      ENDIF
      IF ( ALLOCATED( zv ) ) THEN
          DEALLOCATE( zv, STAT=ierr )
          IF (ierr/=0) CALL errore(subname,'deallocating zv',ABS(ierr))
      ENDIF
      IF ( ALLOCATED( ityp ) ) THEN
          DEALLOCATE( ityp, STAT=ierr )
          IF (ierr/=0) CALL errore(subname,'deallocating ityp',ABS(ierr))
      ENDIF
      IF ( ALLOCATED( symb ) ) THEN
          DEALLOCATE( symb, STAT=ierr )
          IF (ierr/=0) CALL errore(subname,'deallocating symb',ABS(ierr))
      ENDIF
      IF ( ALLOCATED( tau ) ) THEN
          DEALLOCATE( tau, STAT=ierr )
          IF (ierr/=0) CALL errore(subname,'deallocating tau',ABS(ierr))
      ENDIF
      IF ( ALLOCATED( tau_srt ) ) THEN
          DEALLOCATE( tau_srt, STAT=ierr )
          IF (ierr/=0) CALL errore(subname,'deallocating tau_srt',ABS(ierr))
      ENDIF
      IF ( ALLOCATED( ind_srt ) ) THEN
          DEALLOCATE( ind_srt, STAT=ierr )
          IF (ierr/=0) CALL errore(subname,'deallocating ind_srt',ABS(ierr))
      ENDIF
      IF ( ALLOCATED( atm_symb ) ) THEN
          DEALLOCATE( atm_symb, STAT=ierr )
          IF (ierr/=0) CALL errore(subname,'deallocating atm_symb',ABS(ierr))
      ENDIF
      IF ( ALLOCATED( psfile ) ) THEN
          DEALLOCATE( psfile, STAT=ierr )
          IF (ierr/=0) CALL errore(subname,'deallocating psfile',ABS(ierr))
      ENDIF

      alloc = .FALSE.
  END SUBROUTINE ions_deallocate


!*********************************************************
   SUBROUTINE ions_read_ext( filefmt )
   !*********************************************************
   IMPLICIT NONE
       CHARACTER(*),      INTENT(in) :: filefmt
       !
       CHARACTER(13)        :: subname="ions_read_ext"
       INTEGER, ALLOCATABLE :: ityp(:)
       INTEGER              :: i, ierr


       SELECT CASE ( TRIM(filefmt) )
       !
       CASE ( 'qexml' )
            !
            CALL qexml_read_ions( NAT=nat, NSP=nsp, IERR=ierr )
            !
       CASE ( 'pw_export' )
            !
            CALL qexpt_read_ions( NAT=nat, NSP=nsp, IERR=ierr )
            !
       CASE DEFAULT
            !
            CALL errore(subname,'invalid filefmt = '//TRIM(filefmt), 1)
       END SELECT
       !
       IF ( ierr/=0) CALL errore(subname,'getting ion dimensions',ABS(ierr))
       !
       !
       CALL ions_allocate( nat, nsp ) 
       !
       ! some simple initializations
       zv(:) = ZERO

       !
       ! read massive data
       ! NOTE: atomic positions are read in bohr
       !
       !
       SELECT CASE ( TRIM(filefmt) )
       !
       CASE ( 'qexml' )
            !
            ALLOCATE( ityp(nat), STAT=ierr )
            IF ( ierr/=0) CALL errore(subname,'allocating ityp', ABS(ierr))
            !
            CALL qexml_read_ions( ATM=atm_symb, ITYP=ityp,                        &
                                  PSFILE=psfile, TAU=tau,  IERR=ierr )
            !
            IF (ierr/=0) CALL errore(subname,'reading qexml file' , ABS(ierr))
            !
            ! pseudo pot are in the .save directory
            !
            pseudo_dir = TRIM(work_dir)// "/" // TRIM(prefix) // ".save/" 
            !
            ! conversions
            DO i = 1, nat
                 symb( i ) = atm_symb( ityp (i) )
            ENDDO
            !
            DEALLOCATE( ityp, STAT=ierr )
            IF ( ierr/=0) CALL errore(subname,'deallocating ityp', ABS(ierr))
            !
       CASE ( 'pw_export' )
            !
            CALL qexpt_read_ions( ATM=atm_symb, SYMB=symb, PSEUDO_DIR=pseudo_dir, &
                                  PSFILE=psfile, TAU=tau,  IERR=ierr )
            !
            IF (ierr/=0) CALL errore(subname,'reading qexpt file' , ABS(ierr))
            !
       CASE DEFAULT
            !
            CALL errore(subname,'invalid filefmt = '//TRIM(filefmt), 1)
       END SELECT

   END SUBROUTINE ions_read_ext


!*********************************************************
   SUBROUTINE ions_init( )
   !*********************************************************
   !
   ! get the internal ordering of atoms and species
   ! ATM(1:ntyp) contains the symbols of atoms in the same order
   ! of pseudopot files. This will be assumed as internal ordering 
   !
   IMPLICIT NONE
       CHARACTER(9)       :: subname="ions_init"
       INTEGER            :: ia, i
 
       IF ( .NOT. alloc ) CALL errore(subname,'IONS not allocated',1)

       !
       ! setting species and atomic symbols
       !
       na(:) = 0
       DO i=1,nsp
          na(i) = 0
          DO ia=1,nat
             IF ( symb(ia) == atm_symb(i) ) THEN
                  ityp(ia) = i
                  na(i) = na(i) + 1
             ENDIF
          ENDDO
       ENDDO
       IF ( SUM( na(1:nsp) ) /= nat ) CALL errore(subname,'some species are missing',1)
       nax = MAXVAL( na(:) )
     
       !
       ! sorting atoms by species
       !
       CALL ions_sort_tau(tau_srt, ind_srt, tau, ityp, na )

   END SUBROUTINE ions_init


!*********************************************************
   SUBROUTINE ions_sort_tau( tausrt, isrt, tau_, isp, na_ )
   !*********************************************************
   !
   ! Freely inspired to the similar subroutine in ESPRESSO package
   !
      IMPLICIT NONE
      REAL(dbl),   INTENT(OUT) :: tausrt( :, : )
      INTEGER,     INTENT(OUT) :: isrt( : )
      REAL(dbl),    INTENT(IN) :: tau_( :, : )
      INTEGER,      INTENT(IN) :: isp( : ), na_(:)
      INTEGER :: ina( SIZE(na_) ), na_tmp( SIZE(na_) )
      INTEGER :: nsp_, is, ia

      nsp_ = SIZE(na_)
      IF ( nsp_ /= nsp) CALL errore('ions_sort_tau','Invalid nsp',ABS(nsp-nsp_))

      ! ... compute the index of the first atom in each specie
      ina( 1 ) = 0
      DO is = 2, nsp_
        ina( is ) = ina( is - 1 ) + na_( is - 1 )
      END DO

      ! ... sort the position according to atomic specie
      na_tmp  = 0
      DO ia = 1, nat
        is  =  isp( ia )
        na_tmp( is ) = na_tmp( is ) + 1
        tausrt( :, na_tmp(is) + ina(is) ) = tau_(:, ia )
        isrt  (    na_tmp(is) + ina(is) ) = ia
      END DO
      RETURN
    END SUBROUTINE ions_sort_tau


END MODULE ions_module

