!
!      Copyright (C) 2004 WanT Group
!      Copyright (C) 1999 Marco Buongiorno Nardelli
!
!      This file is distributed under the terms of the
!      GNU General Public License. See the file `License'
!      in the root directory of the present distribution,
!      or http://www.gnu.org/copyleft/gpl.txt .
!
!**************************************************************************************
   SUBROUTINE greenb( nmaxb, tot, tott, c00_b, c01_b, ene, g, igreen, invert )
   !**************************************************************************************
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

      INTEGER :: nmaxb
      INTEGER :: ipiv(nmaxb)
      INTEGER :: i, j, k, l, m
      INTEGER :: igreen, invert, info

      COMPLEX(dbl) c00_b(nmaxb,nmaxb)
      COMPLEX(dbl) c01_b(nmaxb,nmaxb)

      COMPLEX(dbl) :: tot(nmaxb,nmaxb)
      COMPLEX(dbl) :: tott(nmaxb,nmaxb)
      COMPLEX(dbl) :: eh0(nmaxb,nmaxb)
      COMPLEX(dbl) :: s1(nmaxb,nmaxb)
      COMPLEX(dbl) :: s2(nmaxb,nmaxb)
      COMPLEX(dbl) :: g(nmaxb,nmaxb)
      COMPLEX(dbl) :: ene, dos

!--------------------------------------------------------

      IF ( igreen == 1 ) THEN 
!
!...  Construct the surface green's function g00 
!

         CALL zgemm( 'N', 'N', nmaxb, nmaxb, nmaxb, CONE, c01_b, nmaxb, tot, nmaxb,     &
                     CZERO, s1, nmaxb )

         eh0(:,:) = -c00_b(:,:) -s1(:,:)

       
         DO j = 1, nmaxb
         DO i = 1, nmaxb
             g(i,j) = CZERO
             IF ( i == j ) g(i,i) = CONE
         ENDDO
         ENDDO
      
         IF ( invert == 1 ) THEN
            CALL zgesv( nmaxb, nmaxb, eh0, nmaxb, ipiv, g, nmaxb, info )
            IF ( info /= 0 )  CALL errore('greenb', ' zgesv (I) ', info )
         ENDIF
      ENDIF

      IF ( igreen == -1 ) THEN
!
!...  Construct the dual surface green's function gbar00 
!
         CALL zgemm( 'C', 'N', nmaxb, nmaxb, nmaxb, CONE, c01_b, nmaxb, tott, nmaxb,    &
                     CZERO, s1, nmaxb )

         eh0(:,:) = -c00_b(:,:) -s1(:,:)
       
         DO j = 1, nmaxb
         DO i = 1, nmaxb
             g(i,j) = CZERO
             IF ( i == j ) g(i,i) = CONE
         ENDDO
         ENDDO

         IF ( invert == 1 ) THEN
            CALL zgesv( nmaxb, nmaxb, eh0, nmaxb, ipiv, g, nmaxb, info )
            IF ( info /= 0 )  CALL errore('greenb', 'zgesv (II)', info )
         ENDIF
      
      ENDIF

      IF ( igreen == 0 ) THEN
!
!    Construct the bulk green's function gnn or (if surface=.true.) the
!    sub-surface green's function
!

         CALL zgemm( 'N', 'N', nmaxb, nmaxb, nmaxb, CONE, c01_b, nmaxb, tot, nmaxb,   &
                     CZERO, s1, nmaxb )
         CALL zgemm( 'C', 'N', nmaxb, nmaxb, nmaxb, CONE, c01_b, nmaxb, tott, nmaxb,  &
                     CZERO, s2, nmaxb )

         eh0(:,:) = -c00_b(:,:) -s1(:,:) -s2(:,:)
       
         DO j = 1, nmaxb
         DO i = 1, nmaxb
             g(i,j) = CZERO
             IF( i == j ) g(i,i) = CONE
         ENDDO
         ENDDO

         IF ( invert == 1 ) THEN
            CALL zgesv( nmaxb, nmaxb, eh0, nmaxb, ipiv, g, nmaxb, info )
            IF ( info /= 0 )  CALL errore('greenb', 'zgesv (III)', info )
         ENDIF
      ENDIF

      RETURN
   END SUBROUTINE greenb

