!
! Copyright (C) 2001 PWSCF group
! This file is distributed under the terms of the
! GNU General Public License. See the file `License'
! in the root directory of the present distribution,
! or http://www.gnu.org/copyleft/gpl.txt .
!
!
!----------------------------------------------------------------------
SUBROUTINE struct_fact (nat, tau, ntyp, ityp, ngm, g, bg, nr1, nr2, &
     nr3, strf, eigts1, eigts2, eigts3)
  !----------------------------------------------------------------------
  !
  !   calculate the structure factors for each type of atoms in the unit
  !   cell
  !
  USE kinds,     ONLY : dbl
  USE constants, ONLY : PI, TPI, ZERO, CZERO
  USE timing_module, ONLY : timing
  IMPLICIT NONE
  !
  !   Here the dummy variables
  !

  integer :: nat, ntyp, ityp (nat), ngm, nr1, nr2, nr3
  ! input: the number of atom in the unit cel
  ! input: the number of atom types
  ! input: for each atom gives the type
  ! input: the number of G vectors
  ! input: fft dimension along x
  ! input: fft dimension along y
  ! input: fft dimension along z

  real(kind=dbl) :: bg (3, 3), tau (3, nat), g (3, ngm)
  ! input: reciprocal crystal basis vectors
  ! input: the positions of the atoms in the c
  ! input: the coordinates of the g vectors

  COMPLEX(kind=dbl) :: strf (ngm, ntyp),        &
                      eigts1 ( -nr1:nr1, nat), &
                      eigts2 ( -nr2:nr2, nat), &
                      eigts3 ( -nr3:nr3, nat)
  ! output: the structure factor
  !
  ! output: the phases e^{-iG\tau_s}
  !
  !
  !    here the local variables
  !
  integer :: nt, na, ng, n1, n2, n3, ipol
  ! counter over atom type
  ! counter over atoms
  ! counter over G vectors
  ! counter over fft dimension along x
  ! counter over fft dimension along y
  ! counter over fft dimension along z
  ! counter over polarizations

  REAL(kind=dbl) :: arg, bgtau (3)
  ! the argument of the exponent
  ! scalar product of bg and tau

  CALL timing('struct_fact',OPR='start')

  strf(:,:) = CZERO
  DO nt = 1, ntyp
     DO na = 1, nat
        IF (ityp (na) == nt) THEN
           DO ng = 1, ngm
              arg = (g (1, ng) * tau (1, na) + g (2, ng) * tau (2, na) &
                   + g (3, ng) * tau (3, na) ) * TPI
              strf (ng, nt) = strf (ng, nt) + CMPLX( COS(arg), -SIN(arg), dbl )
           ENDDO
        ENDIF
     ENDDO
  ENDDO

  DO na = 1, nat
     DO ipol = 1, 3
        bgtau (ipol) = bg (1, ipol) * tau (1, na) + &
                       bg (2, ipol) * tau (2, na) + &
                       bg (3, ipol) * tau (3, na)
     ENDDO
     DO n1 = - nr1, nr1
        arg = TPI * n1 * bgtau (1)
        eigts1 (n1, na) = CMPLX( COS(arg), -SIN(arg), dbl )
     ENDDO
     DO n2 = - nr2, nr2
        arg = TPI * n2 * bgtau (2)
        eigts2 (n2, na) = CMPLX( COS(arg), -SIN(arg), dbl )
     ENDDO
     DO n3 = - nr3, nr3
        arg = TPI * n3 * bgtau (3)
        eigts3 (n3, na) = CMPLX( COS(arg), -SIN(arg), dbl )
     ENDDO
  ENDDO

  CALL timing('struct_fact',OPR='stop')
  RETURN
END SUBROUTINE struct_fact

