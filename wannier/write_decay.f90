! 
! Copyright (C) 2008 WanT Group
!
! This file is distributed under the terms of the
! GNU General Public License. See the file `License'
! in the root directory of the present distribution,
! or http://www.gnu.org/copyleft/gpl.txt .
!
!********************************************************
   SUBROUTINE write_decay( filename, dimwann, nrtot, rave, vr, opr )
   !********************************************************
   !  
   USE kinds,                ONLY : dbl
   USE io_module,            ONLY : aux_unit
   USE timing_module,        ONLY : timing
   USE log_module,           ONLY : log_push, log_pop
   !
   IMPLICIT NONE 

   !
   ! input variables
   !
   CHARACTER(*),   INTENT(IN) :: filename
   INTEGER,        INTENT(IN) :: dimwann, nrtot
   REAL(dbl),      INTENT(IN) :: rave(3,dimwann)
   REAL(dbl),      INTENT(IN) :: vr(3,nrtot)
   COMPLEX(dbl),   INTENT(IN) :: opr(dimwann, dimwann, nrtot )

   !
   ! local variables
   !
   CHARACTER(11)  :: subname="write_decay"
   !
   REAL(dbl)      :: rvec(3), rnorm
   INTEGER        :: i, j, ir, ierr
   !
   ! end of declariations
   !   

!
!------------------------------
! main body
!------------------------------
!
      CALL timing( subname, OPR="start" )
      CALL log_push( subname )


      !
      ! write full information about decay
      !
      OPEN( aux_unit, FILE=TRIM(filename), FORM='formatted', IOSTAT=ierr )
      IF (ierr/=0) CALL errore(subname,'opening '//TRIM(filename),ABS(ierr))
      !   
      WRITE( aux_unit, "( '#    |R|[Bohr]       |opr|^2             R[bohr]' )" )
      !
      DO ir = 1, nrtot
          !
          DO j = 1, dimwann
          DO i = 1, dimwann
             !
             ! bohr units
             rvec(:) = rave(:,j) - rave(:,i) + vr(:,ir)
             rnorm   = SQRT( DOT_PRODUCT( rvec, rvec) )
             !
             WRITE ( aux_unit, "(2x, 2f15.9, 2x, 3f15.9)") rnorm, &
                     REAL( opr(i,j,ir) * CONJG( opr(i,j,ir)) ), &
                     rvec(:)
          ENDDO
          ENDDO
          !
      ENDDO
      !
      CLOSE( aux_unit )
      !
      !
      CALL timing( subname, OPR="STOP" )
      CALL log_pop( subname )
      !
END SUBROUTINE write_decay

