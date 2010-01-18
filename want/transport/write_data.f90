!
! Copyright (C) 2009 WanT Group
!
! This file is distributed under the terms of the
! GNU General Public License. See the file `License\'
! in the root directory of the present distribution,
! or http://www.gnu.org/copyleft/gpl.txt .
!
!*********************************************
   MODULE T_write_data_module
   !*********************************************
   !
   ! This module contains basic routines to write
   ! the main data produed by the transport calculation
   !
   USE kinds,                   ONLY : dbl
   USE parameters,              ONLY : nstrx
   USE timing_module,           ONLY : timing
   USE log_module,              ONLY : log_push, log_pop
   USE io_module,               ONLY : ionode, stdout, io_name
   USE parser_module,           ONLY : int2char
   USE iotk_module
   !
   IMPLICIT NONE
   PRIVATE 

   CHARACTER(nstrx) :: filename

   PUBLIC :: wd_write_data
   PUBLIC :: wd_write_eigplot


CONTAINS

!
! subroutines
!

!**********************************************************
   SUBROUTINE wd_write_data(iunit, ne, egrid, dim, mydata, data_type)
   !**********************************************************
   !
   IMPLICIT NONE
      !
      INTEGER,      INTENT(IN) :: iunit
      INTEGER,      INTENT(IN) :: ne, dim
      REAL(dbl),    INTENT(IN) :: mydata(ne,dim)
      REAL(dbl),    INTENT(IN) :: egrid(ne)
      CHARACTER(*), INTENT(IN) :: data_type
      !
      INTEGER       :: ie
      CHARACTER(20) :: str
      !
      CALL log_push( 'wd_write_data' )
      !
      IF ( ionode ) THEN 
          !
          CALL io_name( TRIM(data_type), filename )
          !
          OPEN ( iunit, FILE=TRIM(filename), FORM='formatted' )
          !
          str = TRIM( int2char(dim+1) )
          DO ie = 1, ne
              WRITE ( iunit, '('//TRIM(str)//'(f15.9))' ) egrid(ie), mydata(ie,:)
          ENDDO
          !
          CLOSE( iunit )
          !
          CALL io_name( TRIM(data_type), filename, LPATH=.FALSE. )
          WRITE(stdout,"(/,2x,a,' written on file: ',3x,a)") TRIM(data_type), TRIM(filename)
          !
      ENDIF
      !
      CALL log_pop( 'wd_write_data' )
      RETURN
      !
END SUBROUTINE wd_write_data


!**********************************************************
   SUBROUTINE wd_write_eigplot(iunit, ie, ik, dim1, dim2, mydata)
   !**********************************************************
   !
   IMPLICIT NONE
      !
      INTEGER,      INTENT(IN) :: iunit
      INTEGER,      INTENT(IN) :: ie, ik
      INTEGER,      INTENT(IN) :: dim1, dim2
      COMPLEX(dbl), INTENT(IN) :: mydata(dim1,dim2)
      !
      CHARACTER(20) :: str
      !
      CALL log_push( 'wd_write_eigplot' )
      !
      CALL io_name( "miao", filename )
      !
      CALL io_name( "Miao", filename, LPATH=.FALSE. )
      IF (ionode) WRITE(stdout,"(/,2x,a,'Miao written on file: ',3x,a)") TRIM(filename)
      !
      CALL log_pop( 'wd_write_eigplot' )
      RETURN
      !
END SUBROUTINE wd_write_eigplot

END MODULE T_write_data_module


