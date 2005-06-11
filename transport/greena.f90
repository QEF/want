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
   SUBROUTINE greena( nmaxa, tot, tott, c00_a, c01_a, ene, g, igreen, invert)
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
   IMPLICIT NONE


      INTEGER :: nmaxa
      INTEGER :: i, j, k, l, m
      INTEGER :: igreen, invert, info
      INTEGER :: ipiv(nmaxa)

      COMPLEX(dbl) :: c00_a(nmaxa,nmaxa)
      COMPLEX(dbl) :: c01_a(nmaxa,nmaxa)

      COMPLEX(dbl) :: tot(nmaxa,nmaxa)
      COMPLEX(dbl) :: tott(nmaxa,nmaxa)
      COMPLEX(dbl) :: eh0(nmaxa,nmaxa)
      COMPLEX(dbl) :: s1(nmaxa,nmaxa)
      COMPLEX(dbl) :: s2(nmaxa,nmaxa)
      COMPLEX(dbl) :: g(nmaxa,nmaxa)
      COMPLEX(dbl) :: ene, dos


      IF ( igreen == 1) THEN 
!
!...     Construct the surface green's function g00 
!

         CALL zgemm('N','N', nmaxa, nmaxa, nmaxa, CONE, c01_a, nmaxa, tot, &
                     nmaxa, CZERO, s1, nmaxa )
         eh0(:,:) = -c00_a(:,:) -s1(:,:)
       
         DO i = 1, nmaxa
         DO j = 1, nmaxa
             g(i,j) = CZERO
             IF ( i == j ) g(i,j) = CONE
         ENDDO
         ENDDO
      
         IF ( invert == 1 ) THEN
            CALL zgesv( nmaxa, nmaxa, eh0, nmaxa, ipiv, g, nmaxa, info )
            IF ( info /= 0 )  CALL errore('greena', ' zgesv (I) ', info )
         ENDIF

      ENDIF

      IF ( igreen == -1 ) THEN
!
!...  Construct the dual surface green's function gbar00 
!
         CALL zgemm( 'C', 'N', nmaxa, nmaxa, nmaxa, CONE, c01_a, nmaxa, tott, &
                      nmaxa,  CZERO, s1, nmaxa )

         eh0(:,:) = -c00_a(:,:) -s1(:,:)
       
         DO j = 1, nmaxa
         DO i = 1, nmaxa
             g(i,j) = CZERO
             IF ( i == j ) g(i,j) = CONE
         ENDDO
         ENDDO

         IF ( invert == 1 ) THEN
            CALL zgesv( nmaxa, nmaxa, eh0, nmaxa, ipiv, g, nmaxa, info )
            IF ( info /= 0 )  CALL errore('greena', 'zgesv (II)', info )
         ENDIF
      
      ENDIF

      IF ( igreen == 0 ) THEN
!
!...  Construct the bulk green's function gnn or (if surface=.true.) the
!     sub-surface green's function
!
         CALL zgemm( 'N', 'N', nmaxa, nmaxa, nmaxa, CONE, c01_a, nmaxa, tot, &
                      nmaxa, CZERO, s1, nmaxa )
         CALL zgemm( 'C', 'N', nmaxa, nmaxa, nmaxa, CONE, c01_a, nmaxa, tott,&
                      nmaxa, CZERO, s2, nmaxa )

         eh0(:,:) = -c00_a(:,:) -s1(:,:) -s2(:,:)
       
         DO j = 1, nmaxa
         DO i = 1, nmaxa
             g(i,j) = CZERO
             IF ( i == j ) g(i,j) = CONE
         ENDDO
         ENDDO

         IF ( invert == 1 ) THEN
            CALL zgesv( nmaxa, nmaxa, eh0, nmaxa, ipiv, g, nmaxa, info )
            IF ( info /= 0 )  CALL errore('greena', 'zgesv (III)', info )
         ENDIF

      ENDIF

   END SUBROUTINE greena

