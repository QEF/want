!
! Copyright (C) 2004 Arrigo Calzolari, Carlo Cavazzoni, Marco Buongiorno Nardelli
! Copyright (C) 2002 Nicola Marzari, Ivo Souza, David Vanderbilt
! Copyright (C) 1997 Nicola Marzari, David Vanderbilt
!
! This file is distributed under the terms of the
! GNU General Public License. See the file `License'
! in the root directory of the present distribution,
! or http://www.gnu.org/copyleft/gpl.txt .
!
!

MODULE kpoints_module
  !

  USE kinds, ONLY: dbl
  USE parameters, ONLY : npkx => npk

  IMPLICIT NONE
  PRIVATE
  SAVE

!
! declarations
!
  LOGICAL :: first = .TRUE.

  INTEGER                :: nk(3)
  INTEGER                :: nkpts
  REAL(dbl)              :: s(3)
  REAL(dbl), ALLOCATABLE :: vkpt(:,:)
  REAL(dbl), ALLOCATABLE :: wtkpt(:)
  REAL(dbl)              :: wtktot

!
! end of declaration scope 
!

  PUBLIC :: nk, s, nkpts
  PUBLIC :: vkpt
  PUBLIC :: wtkpt, wtktot

  PUBLIC :: kpoints_init
  PUBLIC :: kpoints_deallocate


CONTAINS

!**********************************************************
   SUBROUTINE kpoints_allocate()
   !**********************************************************
   IMPLICIT NONE
    INTEGER   :: ierr

    IF ( nkpts <= 0) CALL errore('kpoints_allocate','Invalid NKPTS',1)
    ALLOCATE( vkpt(3,nkpts),STAT=ierr )
       IF (ierr/=0) CALL errore('kpoints_allocate','allocating VKPT',ABS(ierr))
    ALLOCATE( wtkpt(nkpts),STAT=ierr )
       IF (ierr/=0) CALL errore('kpoints_allocate','allocating WTKPT',ABS(ierr))

   END SUBROUTINE kpoints_allocate


!**********************************************************
   SUBROUTINE kpoints_init( nkpts_ )
   !**********************************************************
    IMPLICIT NONE
    INTEGER :: nkpts_
    INTEGER :: i1, i2, i3, nkp

      nkpts = nkpts_
      IF ( nkpts > npkx ) CALL errore('kpoints_init','Nkpts too large',nkpts)
      CALL kpoints_allocate()

      nkp = 0
      DO i1 = 0, nk(1)-1
        DO i2 = 0, nk(2)-1
          DO i3 = 0, nk(3)-1
            nkp = nkp + 1
            vkpt(1,nkp) = DBLE(i1)/DBLE(nk(1)) + s(1)
            vkpt(2,nkp) = DBLE(i2)/DBLE(nk(2)) + s(2)
            vkpt(3,nkp) = DBLE(i3)/DBLE(nk(3)) + s(3)
          END DO
        END DO
      END DO

      IF( nkp /= nkpts ) &
        CALL errore( ' kpoints_init ', ' nkp and nkpts differs ', nkpts )
      wtkpt( 1 : nkpts ) = 1.0d0/DBLE( nkpts )
      wtktot = SUM( wtkpt( 1 : nkpts ) )

    RETURN
   END SUBROUTINE


!**********************************************************
   SUBROUTINE kpoints_deallocate()
   !**********************************************************
   IMPLICIT NONE
    INTEGER   :: ierr

    IF ( ALLOCATED( vkpt ) ) THEN
       DEALLOCATE( vkpt, STAT=ierr )
       IF (ierr/=0) CALL errore('kpoints_deallocate','deallocating VKPT',ABS(ierr))
    ENDIF
    IF ( ALLOCATED( wtkpt ) ) THEN
       DEALLOCATE( wtkpt, STAT=ierr )
       IF (ierr/=0) CALL errore('kpoints_allocate','deallocating WTKPT',ABS(ierr))
    ENDIF

   END SUBROUTINE kpoints_deallocate

END MODULE kpoints_module
