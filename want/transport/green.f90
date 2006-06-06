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
   SUBROUTINE green( dim, tot, tott, c00, c01, s00, g, igreen)
   !***********************************************************************************
   !
   !  Construct green's functions
   !     
   !  igreen = -1  right surface - left transfer
   !  igreen =  1  left surface - right transfer
   !  igreen =  0  bulk
   !
   USE kinds
   USE constants,         ONLY : CZERO, CONE, CI
   USE timing_module,     ONLY : timing
   USE util_module,       ONLY : mat_mul, mat_sv
   USE T_smearing_module, ONLY : delta
   IMPLICIT NONE

   !
   ! I/O variables
   !
   INTEGER,      INTENT(in)    :: dim
   INTEGER,      INTENT(in)    :: igreen
   COMPLEX(dbl), INTENT(in)    :: tot(dim,dim), tott(dim,dim)
   COMPLEX(dbl), INTENT(in)    :: c00(dim,dim)
   COMPLEX(dbl), INTENT(in)    :: c01(dim,dim)
   COMPLEX(dbl), INTENT(in)    :: s00(dim,dim)
   COMPLEX(dbl), INTENT(out)   :: g(dim, dim)

   !
   ! local variables
   !
   INTEGER                   :: i, ierr
   COMPLEX(dbl), ALLOCATABLE :: eh0(:,:), s1(:,:), s2(:,:)
   COMPLEX(dbl)              :: g0inv(dim,dim)

!
!----------------------------------------
! main Body
!----------------------------------------
!
   CALL timing('green',OPR='start')

   ALLOCATE( eh0(dim,dim), s1(dim,dim), s2(dim,dim), STAT=ierr)
      IF (ierr/=0) CALL errore('green','allocating eh0,s1,s2',ABS(ierr))

   CALL gzero_maker(dim, -c00, s00, g0inv, 'inverse')

   SELECT CASE ( igreen )

   CASE ( 1 )
      !
      ! Construct the surface green's function g00 
      !

      CALL mat_mul(s1, c01, 'N', tot, 'N', dim, dim, dim)
      eh0(:,:) = g0inv(:,:) -s1(:,:)
    
   CASE( -1 )
      !
      ! Construct the dual surface green's function gbar00 
      !

      CALL mat_mul(s1, c01, 'C', tott, 'N', dim, dim, dim)
      eh0(:,:) = g0inv(:,:) -s1(:,:)
    
   CASE ( 0 )
      !
      ! Construct the bulk green's function gnn or (if surface=.true.) the
      ! sub-surface green's function
      !
      CALL mat_mul(s1, c01, 'N', tot, 'N', dim, dim, dim)
      CALL mat_mul(s2, c01, 'C', tott, 'N', dim, dim, dim)
      !
      eh0(:,:) = g0inv(:,:) -s1(:,:) -s2(:,:)
    
   CASE DEFAULT 
      CALL errore('green','invalid igreen', ABS(igreen))

   END SELECT

   
!
! set the identity and compute G inverting eh0
!
   g(:,:) = CZERO
   DO i = 1, dim
      g(i,i) = CONE
   ENDDO
   !
   CALL mat_sv(dim, dim, eh0, g)
!
! ... local cleaning
!
   DEALLOCATE( eh0, s1, s2, STAT=ierr)
      IF (ierr/=0) CALL errore('green','deallocating eh0, s1, s2',ABS(ierr))

   CALL timing('green',OPR='stop')

END SUBROUTINE green

