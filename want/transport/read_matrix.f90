!
!      Copyright (C) 2005 WanT Group
!
!      This file is distributed under the terms of the
!      GNU General Public License. See the file `License'
!      in the root directory of the present distribution,
!      or http://www.gnu.org/copyleft/gpl.txt .
!
!***************************************************************************
   SUBROUTINE read_matrix( dim1, dim2, a, filename )
   !***************************************************************************
   USE KINDS
   USE io_module, ONLY : unit => aux_unit
   IMPLICIT NONE

   ! 
   ! input variables
   !
   INTEGER,      INTENT(in)  :: dim1, dim2
   CHARACTER(*), INTENT(in)  :: filename 
   COMPLEX(dbl), INTENT(out) :: a(dim1,dim2)

   !
   ! local variables
   !
   REAL(dbl) :: aux1, aux2
   INTEGER   :: nw1, nw2
   INTEGER   :: i, j, ierr

   !
   ! end of declarations
   !

!
!----------------------------------------
! main Body
!----------------------------------------
!

   OPEN( unit, FILE=TRIM(filename), STATUS='old', IOSTAT=ierr)
   IF (ierr/=0) CALL  errore('read_matrix', 'opening file = '//TRIM(filename),ABS(ierr) )

      READ( unit, *, IOSTAT=ierr ) nw1, nw2
      IF (ierr/=0) &
           CALL  errore('read_matrix', 'reading nw1,nw2 in '//TRIM(filename), ABS(ierr) )
      IF ( nw1 /= dim1 ) CALL errore('read_matrix', 'dim1 wrong in '//TRIM(filename), 1)
      IF ( nw2 /= dim2 ) CALL errore('read_matrix', 'dim2 wrong in '//TRIM(filename), 2)

      DO j = 1, dim2
         READ ( unit, * ) 
         DO i = 1, dim1
            READ ( unit, *, IOSTAT=ierr ) aux1, aux2 
            IF (ierr/=0) CALL  errore('read_matrix', 'reading aux1,aux2',ABS(ierr) )     
            a(i,j) = CMPLX(aux1,aux2)
         ENDDO
      ENDDO

   CLOSE(unit)

END SUBROUTINE read_matrix

