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
!=----------------------------------------------------------------------------------=
      SUBROUTINE genbtr( npwx, ngx, ngy, ngz, nkpts, enmax, nindpw, nplwkp, vkpt, &
                 lpctx, lpcty, lpctz, recc, iprint )
!=----------------------------------------------------------------------------------=

      USE kinds
      USE constants, ONLY: har => au, bohr => bohr_radius_angs
      USE timing_module, ONLY : timing
      USE io_module, ONLY : stdout

      IMPLICIT none

      INTEGER :: nindpw(*)
      INTEGER :: nplwkp(*)
      REAL(dbl) :: vkpt(3,*)
      INTEGER :: lpctx(*), lpcty(*), lpctz(*)
      REAL(dbl) :: recc(3,*)
      INTEGER :: npwx
      INTEGER :: nkpts, iprint

      INTEGER :: ngx, ngy, ngz
      INTEGER :: n
      INTEGER :: nlboxi, nsboxi
      INTEGER :: nx, ny, nz
      REAL(dbl) :: enmax
      REAL(dbl) :: gzx, gzy, gzz
      REAL(dbl) :: gyx, gyy, gyz
      REAL(dbl) :: gxx, gxy, gxz
      REAL(dbl) :: gx, gy, gz
      REAL(dbl) :: energx, energy, energz
      REAL(dbl) :: energ
      REAL(dbl) :: dum

      REAL(dbl) :: rhsqdtm

      ! DATA rhsqdtm /3.81001845d0/  !  27.211652 * ( 0.529177 )**2 / 2

! ... The subroutine calculates the index in the
!     reciprocal lattice grid of each plane wave basis states at each
!     k point. The kinetic energies of the basis states and the x,y and z
!     components of the kinetic energy are calculated. If the cut-off energy
!     enmax is too large so that there are more than npwx plane wave basis
!     states of energy less than enmax at any k point, enmax is
!     automatically reduced

      CALL timing('genbtr',OPR='start')

      rhsqdtm =  har * bohr**2 / 2.0d0


      DO n = 1, nkpts

        nlboxi = 0
        nsboxi = 0

        DO nz = 1, ngz

          gzx = recc(3,1) * ( lpctz(nz) + vkpt(3,n) )
          gzy = recc(3,2) * ( lpctz(nz) + vkpt(3,n) )
          gzz = recc(3,3) * ( lpctz(nz) + vkpt(3,n) )

          DO ny = 1, ngy

            gyx = recc(2,1) * ( lpcty(ny) + vkpt(2,n) )
            gyy = recc(2,2) * ( lpcty(ny) + vkpt(2,n) )
            gyz = recc(2,3) * ( lpcty(ny) + vkpt(2,n) )

            DO nx = 1, ngx

              gxx = recc(1,1) * ( lpctx(nx) + vkpt(1,n) )
              gxy = recc(1,2) * ( lpctx(nx) + vkpt(1,n) )
              gxz = recc(1,3) * ( lpctx(nx) + vkpt(1,n) )

              nlboxi = nlboxi + 1 

              gx = gxx + gyx + gzx
              gy = gxy + gyy + gzy
              gz = gxz + gyz + gzz

              energx = rhsqdtm * ( gx**2 )
              energy = rhsqdtm * ( gy**2 )
              energz = rhsqdtm * ( gz**2 )

              energ = energx + energy + energz

! ...         Check to see if the kinetic energy of the plane wave is less than
!             enmax in which case the plane wave is included in the set of basis
!             states for the k point

              IF( energ <= enmax ) THEN 

                nsboxi = nsboxi + 1
                IF ( nsboxi > npwx ) &
                    CALL errore(' genbtr ', '  npwx too small ! ', npwx )

                nindpw( nsboxi + ( (n-1) * npwx ) ) = nlboxi

              END IF

            END DO
          END DO
        END DO

! ...   Check to see if there are less than npwx basis states at this k point
!       if not reduce enmax and start the whole process again

        nplwkp(n) = nsboxi

        IF( iprint > 1 ) WRITE(stdout , fmt= " (2x,'Genbtr: ',i5, &
                               &  'plane waves for k-point No',i5 )") nsboxi, n

      END DO

      CALL timing('genbtr',OPR='stop')
 
      RETURN
      END SUBROUTINE
