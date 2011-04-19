!
! Copyright (C) 2005 WanT Group
!
! This file is distributed under the terms of the
! GNU General Public License. See the file `License\'
! in the root directory of the present distribution,
! or http://www.gnu.org/copyleft/gpl.txt .
!
!*********************************************
   MODULE E_hamiltonian_module
   !*********************************************
   !
   USE kinds,                  ONLY : dbl
   USE constants,              ONLY : ZERO
   USE parameters,             ONLY : nstrx
   USE log_module,             ONLY : log_push, log_pop
   !
   USE T_smearing_module,      ONLY : smearing_type
   USE T_kpoints_module,       ONLY : nkpts_par
   USE T_operator_blc_module
   !
   IMPLICIT NONE
   PRIVATE 
   SAVE
!
! Contains the description of the system in terms of 
! Hamiltonian, Overlap, and correlation Sigma blocks
! 
    ! 
    ! dimensions
    !
    INTEGER                   :: dimT
    INTEGER                   :: dimE, dimB
    INTEGER                   :: dimx
    !
    INTEGER                   :: nspin
    INTEGER                   :: ispin
    REAL(dbl)                 :: shift_T
    !
    TYPE( operator_blc )      :: blc_T
    TYPE( operator_blc )      :: blc_E
    TYPE( operator_blc )      :: blc_B
    TYPE( operator_blc )      :: blc_EB
    TYPE( operator_blc )      :: blc_BE
    !
    LOGICAL :: alloc = .FALSE.


!
! end delcarations
!

   PUBLIC :: dimx, dimT, dimE, dimB
   PUBLIC :: nspin, ispin
   PUBLIC :: nkpts_par
   PUBLIC :: shift_T
   !
   PUBLIC :: blc_T
   PUBLIC :: blc_E
   PUBLIC :: blc_B
   PUBLIC :: blc_EB
   PUBLIC :: blc_BE
   !
   PUBLIC :: alloc
   !
   PUBLIC :: hamiltonian_allocate
   PUBLIC :: hamiltonian_deallocate
   PUBLIC :: hamiltonian_memusage


CONTAINS

!
! subroutines
!

!**********************************************************
   SUBROUTINE hamiltonian_allocate()
   !**********************************************************
   IMPLICIT NONE
      CHARACTER(20)      :: subname="hamiltonian_allocate"
      LOGICAL            :: lsgm_aux

      CALL log_push( subname )

      IF ( alloc )            CALL errore(subname,'already allocated', 1 )
      IF ( dimT <= 0 )        CALL errore(subname,'invalid dimT', 1 )
      IF ( dimE <= 0 )        CALL errore(subname,'invalid dimE', 1 )
      IF ( dimE > dimT)       CALL errore(subname,'Invalid dimE > dimT',1)
      !
      IF ( dimB /= dimT-dimE) CALL errore(subname,'Invalid dimB,dimE,dimT',1)
      !
      IF ( nkpts_par <= 0 )   CALL errore(subname,'invalid nkpts_par', 1 )
      !
      dimx = dimT

      lsgm_aux = .FALSE.
      IF ( TRIM(smearing_type) /= "lorentzian" ) lsgm_aux=.TRUE. 

      !
      ! init data
      !
      CALL operator_blc_init( blc_T,   "block_T")
      CALL operator_blc_init( blc_E,   "block_E")
      CALL operator_blc_init( blc_B,   "block_B")
      CALL operator_blc_init( blc_EB,  "block_EB")
      CALL operator_blc_init( blc_BE,  "block_BE")

      !
      ! allocations
      !
      CALL operator_blc_allocate( dimT, dimT, nkpts_par, LHAVE_SGM_AUX=lsgm_aux, OBJ=blc_T )
      CALL operator_blc_allocate( dimE, dimE, nkpts_par, LHAVE_SGM_AUX=lsgm_aux, OBJ=blc_E )
      CALL operator_blc_allocate( dimB, dimB, nkpts_par, LHAVE_SGM_AUX=lsgm_aux, OBJ=blc_B )
      CALL operator_blc_allocate( dimE, dimB, nkpts_par, LHAVE_SGM_AUX=lsgm_aux, OBJ=blc_EB )
      CALL operator_blc_allocate( dimB, dimE, nkpts_par, LHAVE_SGM_AUX=lsgm_aux, OBJ=blc_BE )
      !
      alloc = .TRUE.

      CALL log_pop( subname )

   END SUBROUTINE hamiltonian_allocate


!**********************************************************
   SUBROUTINE hamiltonian_deallocate()
   !**********************************************************
   IMPLICIT NONE
      CHARACTER(22)      :: subname="hamiltonian_deallocate"

      CALL log_push( subname )

      IF ( .NOT. alloc ) RETURN

      CALL operator_blc_deallocate( OBJ=blc_T )
      CALL operator_blc_deallocate( OBJ=blc_E )
      CALL operator_blc_deallocate( OBJ=blc_B )
      CALL operator_blc_deallocate( OBJ=blc_EB )
      CALL operator_blc_deallocate( OBJ=blc_BE )
      !
      alloc = .FALSE.   

      CALL log_pop( subname )

   END SUBROUTINE hamiltonian_deallocate


!**********************************************************
   REAL(dbl) FUNCTION hamiltonian_memusage(memtype)
   !**********************************************************
   IMPLICIT NONE
       !
       CHARACTER(*)   :: memtype
       !
       REAL(dbl) :: cost
       !
       cost = ZERO
       IF ( blc_T%alloc )      cost = cost + operator_blc_memusage(blc_T,  memtype)
       IF ( blc_E%alloc )      cost = cost + operator_blc_memusage(blc_E,  memtype)
       IF ( blc_B%alloc )      cost = cost + operator_blc_memusage(blc_B,  memtype)
       IF ( blc_EB%alloc )     cost = cost + operator_blc_memusage(blc_EB, memtype)
       IF ( blc_BE%alloc )     cost = cost + operator_blc_memusage(blc_BE, memtype)
       !
       hamiltonian_memusage = cost 
       !
   END FUNCTION hamiltonian_memusage


END MODULE E_hamiltonian_module

