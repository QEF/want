! 
! Copyright (C) 2004 Andrea Ferretti
! 
! This file is distributed under the terms of the 
! GNU General Public License. See the file `License' 
! in the root directory of the present distribution, 
! or http://www.gnu.org/copyleft/gpl.txt . 
! 
! <INFO>
!*********************************************
   MODULE converters_module
!*********************************************
   USE kinds, ONLY : dbl
   USE constants, ONLY : ZERO
   IMPLICIT NONE
   PRIVATE

! This module contains some utilities to convert 
! coordinates in different units.
! In the case of direct lattice vectors, BASIS contains
! the direct lattice basis columnwise, while for
! reciprocal lattice vectors BASIS is the reciprocal
! lattice basis matrix.
! 
! routines in this module:
! SUBROUTINE  cart2cry(coord(3,:),basis(3,3)[,unit_str])
! SUBROUTINE  cry2cart(coord(3,:),basis(3,3)[,unit_str])
! </INFO>
!
 
   PUBLIC :: cart2cry
   PUBLIC :: cry2cart

CONTAINS

!**********************************************************
   SUBROUTINE cart2cry(coord,basis,unit_str)
   !**********************************************************
      IMPLICIT NONE
      REAL(dbl),   INTENT(inout)   :: coord(:,:)
      REAL(dbl),   INTENT(in)      :: basis(3,3)
      CHARACTER(*), OPTIONAL, INTENT(out)     :: unit_str

      REAL(dbl):: dtmp(3)
      REAL(dbl):: transf(3,3), det
      INTEGER  :: nvect 
      INTEGER  :: i,j,l, ierr

      nvect = SIZE(coord(:,:),2)
      IF ( SIZE( coord(:,:),1 ) /= 3 ) CALL errore('cart2cry','Invalid COORD lead DIM',1)

      !
      ! TRANSF is the inverse of the basis matrix because
      ! vcart(i) = \Sum_{j} vcry(j) * basis(j,i)
      !
      CALL inv3( basis, transf, det )
      IF ( det == 0 ) CALL errore('cart2cry','basis vectors are linearly dependent',1)
      !
      ! this last operation is due to the INV3 routine 
      !
      transf(:,:) = transf(:,:)/det

      DO j=1,nvect 
          DO i=1,3
             dtmp(i) = ZERO
             DO l=1,3
                 dtmp(i) = dtmp(i) + transf(i,l) * coord(l,j)
             ENDDO
          ENDDO
          coord(:,j) = dtmp(:)
      ENDDO

      IF ( PRESENT(unit_str) ) unit_str='crystal'
   END SUBROUTINE cart2cry


!**********************************************************
   SUBROUTINE cry2cart(coord,basis,unit_str)
   !**********************************************************
      IMPLICIT NONE
      REAL(dbl),   INTENT(inout)   :: coord(:,:)
      REAL(dbl),   INTENT(in)      :: basis(3,3)
      CHARACTER(*),OPTIONAL,INTENT(out) :: unit_str

      REAL(dbl):: dtmp(3)
      INTEGER  :: nvect 
      INTEGER  :: i,j,l, ierr

      nvect = SIZE(coord(:,:),2)
      IF ( SIZE( coord(:,:),1 ) /= 3 ) CALL errore('cry2cart','Invalid COORD lead DIM',1)

      !
      ! use the direct transformation
      !
      DO j=1,nvect 
          DO i=1,3
             dtmp(i) = ZERO
             DO l=1,3
                 dtmp(i) = dtmp(i) + basis(i,l) * coord(l,j)
             ENDDO
          ENDDO
          coord(:,j) = dtmp(:)
      ENDDO

      IF ( PRESENT(unit_str) ) unit_str='cartesian'
   END SUBROUTINE cry2cart

END MODULE converters_module
    
    






