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

MODULE ions
  !

  USE kinds, ONLY: dbl
  USE parameters, ONLY: npsx, natx

  IMPLICIT NONE
  SAVE

  LOGICAL :: first = .TRUE.

  REAL(dbl) :: rat(3,natx,npsx), atmass(npsx)
  INTEGER   :: ntype, natom(npsx)
  CHARACTER(LEN=2) :: nameat(npsx)
  REAL(dbl) :: poscart(3,natx,npsx)

CONTAINS

  SUBROUTINE ions_init(  )
    RETURN
  END SUBROUTINE

  SUBROUTINE poscart_set( avec )
    !
    USE io_global, ONLY: stdout
    USE constants, ONLY: bohr => bohr_radius_angs
    !
    IMPLICIT NONE
    !
    REAL(dbl) :: avec(3,3)
    INTEGER :: nsp, ni, m, j, i

    WRITE( stdout, fmt="(2x, 'Atomic positions: (cart. coord.)' ) " )

    DO nsp = 1 , ntype
      DO ni = 1 , natom(nsp)
        DO m = 1, 3
          poscart(m,ni,nsp) = 0.d0
          DO j=1,3
            !poscart(m,ni,nsp) = poscart(m,ni,nsp) + rat(j,ni,nsp) * dirc(j,m)
            poscart(m,ni,nsp) = poscart(m,ni,nsp) + rat(j,ni,nsp) * avec(m,j) * bohr
          END DO
        END DO
        WRITE( stdout, fmt="(4x, a, 2x,'tau( ',I3,' ) = (', 3F8.4, ' )' )" ) &
        nameat( nsp ), ni, ( poscart(i,ni,nsp), i=1,3 )
      END DO
    END DO

    RETURN
  END SUBROUTINE

END MODULE
