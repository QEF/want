!
!      Copyright (C) 2004 WanT Group
!      Copyright (C) 1999 Marco Buongiorno Nardelli
!
!      This file is distributed under the terms of the
!      GNU General Public License. See the file `License'
!      in the root directory of the present distribution,
!      or http://www.gnu.org/copyleft/gpl.txt .
!
!****************************************************************************
   SUBROUTINE green( nmax, tot, tott, h0, h1, ene, g, igreen, invert )
   !****************************************************************************
   USE kinds
   USE constants, ONLY : CZERO, CONE
   IMPLICIT NONE

!...  Construct green's functions
!     
!     igreen = -1  left surface
!     igreen =  1  right surface
!     igreen =  0  bulk

!     invert = 0 computes g^-1
!     invert = 1 computes g^-1 and g

      INTEGER :: nmax
      INTEGER :: i, j, k, l, m
      INTEGER :: igreen, invert, info
      INTEGER :: ipiv(nmax)

      REAL(dbl) :: h0(nmax,nmax)
      REAL(dbl) :: h1(nmax,nmax)

      COMPLEX(dbl) :: tot(nmax,nmax), tott(nmax,nmax)
      COMPLEX(dbl) :: eh0(nmax,nmax), c1(nmax,nmax)
      COMPLEX(dbl) :: s1(nmax,nmax), s2(nmax,nmax)
      COMPLEX(dbl) :: g(nmax,nmax)
      COMPLEX(dbl) :: ene, dos

!------------------------------------------------------------

      c1(:,:) = h1(:,:)

      IF ( igreen == 1 ) THEN 

!...  Construct the surface green's function g00 

         CALL zgemm( 'N', 'N', nmax, nmax, nmax, CONE, c1, nmax, tot, nmax, CZERO, s1, nmax )

         eh0(:,:) = -h0(:,:) -s1(:,:)
         DO i = 1, nmax
            eh0(i,i) = ene + eh0(i,i)
         END DO
       
         DO i = 1, nmax
         DO j = 1, nmax
              g(i,j) = CZERO
              IF ( i == j ) g(i,j) = CONE
         ENDDO
         ENDDO
      
         IF ( invert == 1 ) THEN
            CALL zgesv( nmax, nmax, eh0, nmax, ipiv, g, nmax, info )
            IF ( info /= 0 )  CALL errore(' green ', ' zgesv (I) ', info )
         ENDIF

      ENDIF

      IF ( igreen == -1 ) THEN

!...  Construct the dual surface green's function gbar00 

         CALL zgemm( 'C', 'N', nmax, nmax, nmax, CONE, c1, nmax, tott, nmax, CZERO, s1, nmax )

         eh0(:,:) = -h0(:,:) -s1(:,:)
         DO i = 1, nmax
            eh0(i,i) = ene + eh0(i,i)
         ENDDO
       
         DO i = 1, nmax
         DO j = 1, nmax
               g(i,j) = CZERO
               IF ( i == j ) g(i,j) = CONE
         ENDDO
         ENDDO

         IF ( invert == 1 ) THEN
            CALL zgesv( nmax, nmax, eh0, nmax, ipiv, g, nmax, info )
            IF ( info /= 0 )  CALL errore('green', 'zgesv (II)', info )
         ENDIF
      ENDIF

      IF ( igreen == 0 ) THEN
!
!...  Construct the bulk green's function gnn or (if surface=.true.) the
!     sub-surface green's function
!
         CALL zgemm( 'N', 'N', nmax, nmax, nmax, CONE, c1, nmax, tot, nmax, CZERO, s1, nmax )
         CALL zgemm( 'C', 'N', nmax, nmax, nmax, CONE, c1, nmax, tott, nmax, CZERO, s2, nmax )

         eh0(:,:) = -h0(:,:) -s1(:,:) -s2(:,:)
         DO i = 1, nmax
            eh0(i,i) = ene + eh0(i,i)
         ENDDO
       
         DO i = 1, nmax
         DO j =1, nmax
               g(i,j) = CZERO
               IF ( i == j ) g(i,j) = CONE
         ENDDO
         ENDDO

         IF ( invert == 1 ) THEN
            CALL zgesv( nmax, nmax, eh0, nmax, ipiv, g, nmax, info )
            IF ( info /= 0 )  CALL errore('green', 'zgesv (III)', info )
         ENDIF
      ENDIF

      RETURN
   END subroutine green
