! 
! Copyright (C) 2004 WanT Group
! 
! This file is distributed under the terms of the 
! GNU General Public License. See the file `License' 
! in the root directory of the present distribution, 
! or http://www.gnu.org/copyleft/gpl.txt . 
! 
!*********************************************************
SUBROUTINE write_op(unit,dim,energy,op,string)
   !*********************************************************
   USE kinds, ONLY : dbl
   IMPLICIT NONE

! <INFO>
! Writes the matrics elements of a generic operator
! to file; if file it is not opened, the write will be
! formatted on std unit file (fort.X).   
! 
! </INFO>

   INTEGER, INTENT(in)                 :: unit
   INTEGER, INTENT(in)                 :: dim
   COMPLEX(dbl), INTENT(in)            :: op(dim,dim) 
   CHARACTER(*), INTENT(in)            :: string
   REAL(dbl),    INTENT(in)            :: energy

   LOGICAL                             :: open
   LOGICAL                             :: formatted
   CHARACTER(3)                        :: fmt
   INTEGER                             :: i,j


!------------------------------------------------
   
   INQUIRE(unit, OPENED=open, FORMATTED=fmt )
   IF ( .NOT. open ) THEN
      formatted = .TRUE.
   ELSEIF ( TRIM(fmt) == "YES" .OR. TRIM(fmt) == "yes") THEN
      formatted = .TRUE.
   ELSE
      formatted = .FALSE.
   ENDIF
       
   IF ( formatted ) THEN
      WRITE(unit,*) TRIM(string)
      WRITE(unit,*) energy
      WRITE(unit,*) 
      DO j=1,dim
         WRITE(unit,"(4(2f15.8,3x))") ( op(i,j), i=1,dim ) 
      ENDDO
      WRITE(unit,*) 
   ELSE
      WRITE(unit) TRIM(string)
      WRITE(unit) energy
      DO j=1,dim
         WRITE(unit) ( op(i,j), i=1,dim ) 
      ENDDO
   ENDIF


   END SUBROUTINE  write_op



