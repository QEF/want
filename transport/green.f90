!
!      Copyright (C) 2004 WanT Group
!      Copyright (C) 1999 Marco Buongiorno Nardelli
!
!      This file is distributed under the terms of the
!      GNU General Public License. See the file `License'
!      in the root directory of the present distribution,
!      or http://www.gnu.org/copyleft/gpl.txt .
!
!***********************************************************************************
   SUBROUTINE green( ndim, opr00, opr01, tot, tott, g, igreen)
   !***********************************************************************************
   !
   !  Construct green's functions
   !     
   !  igreen = -1  right surface - left transfer
   !  igreen =  1  left surface - right transfer
   !  igreen =  0  bulk
   !
   USE kinds
   USE constants,               ONLY : CZERO, CONE, CI
   USE timing_module,           ONLY : timing
   USE log_module,              ONLY : log_push, log_pop
   USE util_module,             ONLY : mat_mul, mat_inv
   USE T_operator_blc_module
   !
   IMPLICIT NONE

   !
   ! I/O variables
   !
   INTEGER,                 INTENT(IN)    :: ndim
   TYPE(operator_blc),      INTENT(IN)    :: opr00, opr01
   INTEGER,                 INTENT(IN)    :: igreen
   COMPLEX(dbl),            INTENT(IN)    :: tot(ndim,ndim), tott(ndim,ndim)
   COMPLEX(dbl),            INTENT(OUT)   :: g(ndim, ndim)

   !
   ! local variables
   !
   CHARACTER(5)              :: subname='green'
   INTEGER                   :: ierr
   COMPLEX(dbl), ALLOCATABLE :: eh0(:,:), s1(:,:), s2(:,:)
   COMPLEX(dbl)              :: g0inv(ndim,ndim)

!
!----------------------------------------
! main Body
!----------------------------------------
!
   CALL timing(subname,OPR='start')
   CALL log_push(subname)

   IF ( .NOT. opr00%alloc )   CALL errore(subname,'opr00 not alloc',1)
   IF ( .NOT. opr01%alloc )   CALL errore(subname,'opr01 not alloc',1)

   ALLOCATE( eh0(ndim,ndim), s1(ndim,ndim), s2(ndim,ndim), STAT=ierr)
   IF (ierr/=0) CALL errore(subname,'allocating eh0,s1,s2',ABS(ierr))

   !
   ! opr00%aux = ene * s00 - h00
   !
   CALL gzero_maker(ndim, opr00, ndim, g0inv, 'inverse', ' ')

   SELECT CASE ( igreen )

   CASE ( 1 )
      !
      ! Construct the surface green's function g00 
      !

      CALL mat_mul(s1, opr01%aux, 'N', tot, 'N', ndim, ndim, ndim)
      eh0(:,:) = g0inv(:,:) -s1(:,:)
    
   CASE( -1 )
      !
      ! Construct the dual surface green's function gbar00 
      !

      CALL mat_mul(s1, opr01%aux, 'C', tott, 'N', ndim, ndim, ndim)
      eh0(:,:) = g0inv(:,:) -s1(:,:)
    
   CASE ( 0 )
      !
      ! Construct the bulk green's function gnn or (if surface=.true.) the
      ! sub-surface green's function
      !
      CALL mat_mul(s1, opr01%aux, 'N', tot,  'N', ndim, ndim, ndim)
      CALL mat_mul(s2, opr01%aux, 'C', tott, 'N', ndim, ndim, ndim)
      !
      eh0(:,:) = g0inv(:,:) -s1(:,:) -s2(:,:)
    
   CASE DEFAULT 
      CALL errore(subname,'invalid igreen', ABS(igreen))

   END SELECT

   
   !
   ! set the identity and compute G inverting eh0
   !
   CALL mat_inv( ndim, eh0, g)

   !
   ! local cleaning
   !
   DEALLOCATE( eh0, s1, s2, STAT=ierr)
   IF (ierr/=0) CALL errore(subname,'deallocating eh0, s1, s2',ABS(ierr))

   CALL timing(subname,OPR='stop')
   CALL log_pop(subname)

END SUBROUTINE green

