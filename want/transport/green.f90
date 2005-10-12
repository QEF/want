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
   SUBROUTINE green( nmax, tot, tott, c00, c01, ene, g, igreen, invert)
   !***********************************************************************************
   !
   !...  Construct green's functions
   !     
   !     igreen = -1  right surface - left transfer
   !     igreen =  1  left surface - right transfer
   !     igreen =  0  bulk
   !
   !...  invert = 0 computes g^-1
   !     invert = 1 computes g^-1 and g
   !
   USE kinds
   USE constants, ONLY : CZERO, CONE
   USE timing_module, ONLY : timing
   USE util_module,      ONLY : zmat_mul, mat_sv
   IMPLICIT NONE

      !
      ! I/O variables
      !
      INTEGER,      INTENT(in)    :: nmax
      INTEGER,      INTENT(in)    :: igreen, invert
      COMPLEX(dbl), INTENT(in)    :: tot(nmax,nmax), tott(nmax,nmax)
      COMPLEX(dbl), INTENT(in)    :: c00(nmax,nmax)
      COMPLEX(dbl), INTENT(in)    :: c01(nmax,nmax)
      COMPLEX(dbl), INTENT(out)   :: g(nmax, nmax)

      !
      ! local variables
      !
      INTEGER                   :: i, j, k, l, m, info, ierr
      INTEGER,      ALLOCATABLE :: ipiv(:)
      COMPLEX(dbl)              :: ene, dos
      COMPLEX(dbl), ALLOCATABLE :: eh0(:,:), s1(:,:), s2(:,:)

!
!----------------------------------------
! main Body
!----------------------------------------
!
      CALL timing('green',OPR='start')

   
      ALLOCATE( eh0(nmax,nmax), s1(nmax,nmax), s2(nmax,nmax), STAT=ierr)
         IF (ierr/=0) CALL errore('green','allocating eh0,s1,s2',ABS(ierr))
      ALLOCATE( ipiv(nmax), STAT=ierr)
         IF (ierr/=0) CALL errore('green','allocating ipiv',ABS(ierr))


      IF ( igreen == 1) THEN 
!
!...     Construct the surface green's function g00 
!


         CALL zmat_mul(s1, c01, 'N', tot, 'N', nmax, nmax, nmax)
         eh0(:,:) = -c00(:,:) -s1(:,:)
       
         g(:,:) = CZERO
         DO i = 1, nmax
             g(i,i) = CONE
         ENDDO
      
         IF ( invert == 1 ) THEN
            CALL mat_sv(nmax, nmax, eh0, g)
         ENDIF

      ENDIF

      IF ( igreen == -1 ) THEN
!
!...  Construct the dual surface green's function gbar00 
!
         CALL zmat_mul(s1, c01, 'C', tott, 'N', nmax, nmax, nmax)

         eh0(:,:) = -c00(:,:) -s1(:,:)
       
         DO j = 1, nmax
         DO i = 1, nmax
             g(i,j) = CZERO
             IF ( i == j ) g(i,j) = CONE
         ENDDO
         ENDDO

         IF ( invert == 1 ) THEN
            CALL mat_sv(nmax, nmax, eh0, g)
         ENDIF
      
      ENDIF

      IF ( igreen == 0 ) THEN
!
!...  Construct the bulk green's function gnn or (if surface=.true.) the
!     sub-surface green's function
!
         CALL zmat_mul(s1, c01, 'N', tot, 'N', nmax, nmax, nmax)
         CALL zmat_mul(s2, c01, 'C', tott, 'N', nmax, nmax, nmax)

         eh0(:,:) = -c00(:,:) -s1(:,:) -s2(:,:)
       
         DO j = 1, nmax
         DO i = 1, nmax
             g(i,j) = CZERO
             IF ( i == j ) g(i,j) = CONE
         ENDDO
         ENDDO

         IF ( invert == 1 ) THEN
            CALL mat_sv(nmax, nmax, eh0, g)
         ENDIF

      ENDIF


!
! ... local cleaning
!
      DEALLOCATE( eh0, s1, s2, STAT=ierr)
         IF (ierr/=0) CALL errore('green','deallocating eh0, s1, s2',ABS(ierr))
      DEALLOCATE( ipiv, STAT=ierr)
         IF (ierr/=0) CALL errore('green','deallocating ipiv',ABS(ierr))

      CALL timing('green',OPR='stop')

   END SUBROUTINE green

