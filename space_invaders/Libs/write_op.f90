
!*********************************************************
SUBROUTINE write_op(dim,energy,op,string,filout,format)
   !*********************************************************
   USE kinds, ONLY : dbl
   IMPLICIT NONE

! <INFO>
! Writes the matrics elements of a generic operator
! to file; data are APPENDED to the file.
! 
! </INFO>

   INTEGER, PARAMETER                  :: out=10

   INTEGER, INTENT(in)                 :: dim
   COMPLEX(dbl), INTENT(in)            :: op(dim,dim) 
   CHARACTER(*), INTENT(in)            :: string
   CHARACTER(*), INTENT(in)            :: filout
   CHARACTER(*), INTENT(in)            :: format
   REAL(dbl),    INTENT(in)            :: energy

   INTEGER                             :: i,j


!------------------------------------------------

   IF ( TRIM(format) /= "formatted" .AND. TRIM(format) /= "unformatted" .AND. &
        TRIM(format) /= "FORMATTED" .AND. TRIM(format) /= "UNFORMATTED"   )   &
        CALL errore('write_op','Invalid formta '//TRIM(format),1)

   IF ( TRIM(format) == 'formatted' .OR. TRIM(format) == 'FORMATTED' ) THEN
      OPEN(UNIT=out, FILE=filout, POSITION='append', STATUS='unknown', FORM=TRIM(format) )
         WRITE(out,*) string
         WRITE(out,*) energy
         WRITE(out,*) 
         DO j=1,dim
            WRITE(out,"(4(2f15.8,3x))") ( op(i,j), i=1,dim ) 
         ENDDO
         WRITE(out,*) 
      CLOSE(out)
   ELSE
      OPEN(UNIT=out, FILE=filout, POSITION='append', STATUS='unknown', FORM=TRIM(format) )
         WRITE(out) string
         WRITE(out) energy
         DO j=1,dim
            WRITE(out) ( op(i,j), i=1,dim ) 
         ENDDO
      CLOSE(out)
   ENDIF


   END SUBROUTINE  write_op



