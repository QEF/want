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
   USE parameters,             ONLY : nstrx
   USE log_module,             ONLY : log_push, log_pop
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
    INTEGER                   :: dimC       
    INTEGER                   :: dim_emb
    INTEGER                   :: dimx
    !
    INTEGER                   :: nspin
    INTEGER                   :: ispin
    REAL(dbl)                 :: shift_C
    !
    TYPE( operator_blc )      :: blc_C
    TYPE( operator_blc )      :: blc_emb
    !
    LOGICAL :: alloc = .FALSE.


!
! end delcarations
!

   PUBLIC :: dimC, dim_emb
   PUBLIC :: nspin, ispin
   PUBLIC :: nkpts_par
   !
   PUBLIC :: shift_C
   !
   PUBLIC :: blc_C
   PUBLIC :: blc_emb
   !
   PUBLIC :: alloc
   !
   PUBLIC :: hamiltonian_allocate
   PUBLIC :: hamiltonian_deallocate


CONTAINS

!
! subroutines
!

!**********************************************************
   SUBROUTINE hamiltonian_allocate()
   !**********************************************************
   IMPLICIT NONE
      CHARACTER(20)      :: subname="hamiltonian_allocate"

      CALL log_push( subname )

      IF ( alloc )            CALL errore(subname,'already allocated', 1 )
      IF ( dimC <= 0 )        CALL errore(subname,'invalid dimC', 1 )
      IF ( dim_emb <= 0 )     CALL errore(subname,'invalid dim_emb', 1 )
      IF ( dim_emb >= dimC)   CALL errore(subname,'Invalid dim_emb >= dimC',1)
      !
      IF ( nkpts_par <= 0 )   CALL errore(subname,'invalid nkpts_par', 1 )
      !
      dimx = dimC

      !
      ! init data
      !
      CALL operator_blc_init( blc_C,   "block_C")
      CALL operator_blc_init( blc_emb, "block_emb")

      !
      ! allocations
      !
      CALL operator_blc_allocate( dimC,    dimC,    nkpts_par, OBJ=blc_C )
      CALL operator_blc_allocate( dim_emb, dim_emb, nkpts_par, OBJ=blc_emb )
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

      CALL operator_blc_deallocate( OBJ=blc_C )
      CALL operator_blc_deallocate( OBJ=blc_emb )
      !
      alloc = .FALSE.   

      CALL log_pop( subname )

   END SUBROUTINE hamiltonian_deallocate

END MODULE E_hamiltonian_module

