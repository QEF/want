      SUBROUTINE genbtr( nrplwv, ngx, ngy, ngz, nkpts, enmax, nindpw, nplwkp, vkpt, &
                 lpctx, lpcty, lpctz, datake, recc, reci, iprint, dnlg, dnlkg )

      USE kinds
      USE timing_module, ONLY : timing
      USE io_global, ONLY : stdout

      IMPLICIT none

      INTEGER :: nindpw(*)
      INTEGER :: nplwkp(*)
      REAL(dbl) :: vkpt(3,*)
      INTEGER :: lpctx(*), lpcty(*), lpctz(*)
      REAL(dbl) :: datake(*)
      REAL(dbl) :: recc(3,*)
      REAL(dbl) :: reci(3,*)
      INTEGER :: nrplwv
      INTEGER :: nkpts, iprint
      REAL(dbl) :: dnlg(nrplwv,3,nkpts), dnlkg(nrplwv,0:3,nkpts)

      INTEGER :: ngx, ngy, ngz
      INTEGER :: n
      INTEGER :: nlboxi, nsboxi
      INTEGER :: nx, ny, nz
      INTEGER :: nindx
      REAL(dbl) :: enmax
      REAL(dbl) :: tpi, rhsqdtm
      REAL(dbl) :: testmx
      REAL(dbl) :: accmxc, accmxi
      REAL(dbl) :: gzx, gzy, gzz
      REAL(dbl) :: gizx, gizy, gizz
      REAL(dbl) :: gyx, gyy, gyz
      REAL(dbl) :: giyx, giyy, giyz
      REAL(dbl) :: gxx, gxy, gxz
      REAL(dbl) :: gx, gy, gz
      REAL(dbl) :: gixx, gixy, gixz
      REAL(dbl) :: g1, g2, g3
      REAL(dbl) :: energx, energy, energz
      REAL(dbl) :: energ, energi
      REAL(dbl) :: enerix, eneriy, eneriz
      REAL(dbl) :: dum

      DATA rhsqdtm /3.81001845d0/
      testmx = 0.d0


! ... The subroutine calculates the index in the
!     reciprocal lattice grid of each plane wave basis states at each
!     k point. The kinetic energies of the basis states and the x,y and z
!     components of the kinetic energy are calculated. If the cut-off energy
!     enmax is too large so that there are more than nrplwv plane wave basis
!     states of energy less than enmax at any k point, enmax is
!     automatically reduced

      CALL timing('genbtr',OPR='start')

      accmxc = 0.d0
      accmxi = 0.d0

      DO n = 1, nkpts

        nlboxi = 0
        nsboxi = 0

        DO nz = 1, ngz
          gzx = recc(3,1) * ( lpctz(nz) + vkpt(3,n) )
          gzy = recc(3,2) * ( lpctz(nz) + vkpt(3,n) )
          gzz = recc(3,3) * ( lpctz(nz) + vkpt(3,n) )
          gizx = reci(3,1) * ( lpctz(nz) + vkpt(3,n) )
          gizy = reci(3,2) * ( lpctz(nz) + vkpt(3,n) )
          gizz = reci(3,3) * ( lpctz(nz) + vkpt(3,n) )
          DO ny = 1, ngy
            gyx = recc(2,1) * ( lpcty(ny) + vkpt(2,n) )
            gyy = recc(2,2) * ( lpcty(ny) + vkpt(2,n) )
            gyz = recc(2,3) * ( lpcty(ny) + vkpt(2,n) )
            giyx = reci(2,1) * ( lpcty(ny) + vkpt(2,n) )
            giyy = reci(2,2) * ( lpcty(ny) + vkpt(2,n) )
            giyz = reci(2,3) * ( lpcty(ny) + vkpt(2,n) )
            DO nx = 1, ngx

              nlboxi = nlboxi + 1 

              gxx = recc(1,1) * ( lpctx(nx) + vkpt(1,n) )
              gxy = recc(1,2) * ( lpctx(nx) + vkpt(1,n) )
              gxz = recc(1,3) * ( lpctx(nx) + vkpt(1,n) )
              gx = gxx + gyx + gzx
              gy = gxy + gyy + gzy
              gz = gxz + gyz + gzz
              gixx = reci(1,1) * ( lpctx(nx) + vkpt(1,n) )
              gixy = reci(1,2) * ( lpctx(nx) + vkpt(1,n) )
              gixz = reci(1,3) * ( lpctx(nx) + vkpt(1,n) )
              g1 = gx
              g2 = gy
              g3 = gz
              energx = rhsqdtm * ( gx**2 )
              energy = rhsqdtm * ( gy**2 )
              energz = rhsqdtm * ( gz**2 )
              energ = energx + energy + energz
              enerix = rhsqdtm * ( ( gixx + giyx + gizx )**2 )
              eneriy = rhsqdtm * ( ( gixy + giyy + gizy )**2 )
              eneriz = rhsqdtm * ( ( gixz + giyz + gizz )**2 )
              energi = enerix + eneriy + eneriz

              IF ( energi > testmx ) testmx = energi

! ...         Check to see if the kinetic energy of the plane wave is less than
!             enmax in which case the plane wave is included in the set of basis
!             states for the k point

              IF( energi <= enmax ) THEN 

                nsboxi = nsboxi + 1
                IF ( nsboxi > nrplwv ) &
                  CALL errore(' genbtr ', '  nrplwv too small ! ', nrplwv )

                IF( energ > accmxc ) accmxc = energ
                IF( energi > accmxi ) accmxi = energi
                nindpw( nsboxi + ( (n-1) * nrplwv ) ) = nlboxi
                nindx = 7 * ( nsboxi - 1 + ( (n-1) * nrplwv ) )
                datake( 1 + nindx ) = energ
                datake( 2 + nindx ) = rhsqdtm * ( g1**2 )
                datake( 3 + nindx ) = rhsqdtm * ( g2**2 )
                datake( 4 + nindx ) = rhsqdtm * ( g3**2 )
                datake( 5 + nindx ) = rhsqdtm * g2 * g3
                datake( 6 + nindx ) = rhsqdtm * g3 * g1
                datake( 7 + nindx ) = rhsqdtm * g1 * g2


                dnlg(nsboxi,1,n) = gx
                dnlg(nsboxi,2,n) = gy
                dnlg(nsboxi,3,n) = gz

! ...           This is in cartesian coodination, too.

                dum = SQRT( gx**2 + gy**2 + gz**2 )
                dnlkg(nsboxi,0,n) = dum

                IF( dum <= 1e-6 ) THEN
                  dnlkg(nsboxi,1,n) = 0.57735027
                  dnlkg(nsboxi,2,n) = 0.57735027
                  dnlkg(nsboxi,3,n) = 0.57735027
                ELSE 
                  dnlkg(nsboxi,1,n) = gx / dum
                  dnlkg(nsboxi,2,n) = gy / dum
                  dnlkg(nsboxi,3,n) = gz / dum
                END IF

              END IF

            END DO
          END DO
        END DO

! ...   Check to see if there are less than nrplwv basis states at this k point
!       if not reduce enmax and start the whole process again

        nplwkp(n) = nsboxi
        IF( iprint >= 1 ) &
         WRITE(stdout , fmt= " (2x,'Genbtr: ',i5, 'plane waves for k-point No',i5 )") nsboxi, n

      END DO

      WRITE(stdout, *) ' '
      WRITE(stdout, fmt= " (2x, 'Plane-waves up to ', F8.2, ' eV in original cell ', & 
            'have been accepted ' )  )" ) accmxi
      WRITE(stdout, fmt= " (2x, 'Plane-waves up to ', F8.2, ' eV in   new    cell ', & 
            'have been accepted ' )  )" ) accmxc
      WRITE(stdout, *) ' '

      CALL timing('genbtr',OPR='stop')
 
      RETURN
      END SUBROUTINE
