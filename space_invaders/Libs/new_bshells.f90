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
      SUBROUTINE bshells( vkpt, nkpts, recc, nshells, nwhich, nnshell,   &
                  bk, dnn, wb, wbtot, nnlist, nncell, nntot, bka, neigh, mxdnrk )
!=----------------------------------------------------------------------------------=

      USE kinds
      USE timing_module, ONLY : timing
      USE io_global, ONLY: stdout
 
! ... Computes the shells of b-vectors connecting every k-point to its
!     neighbors, as well as their weights for the finite-difference formulas
!     for the k-derivatives
 
      IMPLICIT none
 
      INTEGER :: nnmx, nnmxh
      PARAMETER( nnmx = 12 )
      PARAMETER( nnmxh = nnmx/2 )
      REAL(dbl) :: eta, eps
      PARAMETER( eta = 99999999.0d0 )
      PARAMETER( eps = 1d-6 )
 
      INTEGER :: nkpts, nshells
      INTEGER :: mxdnrk, ndnc
      REAL(dbl) :: vkpt(3,mxdnrk), recc(3,3)
 
      REAL(dbl) :: bk(3,mxdnrk,nnmx)
      REAL(dbl) :: ddelta
      REAL(dbl) :: dnn(nnmx)
      REAL(dbl) :: wb(mxdnrk,nnmx)
      REAL(dbl) :: wbtot
      REAL(dbl) :: bka(3,nnmxh)
      INTEGER :: nnlist(mxdnrk,nnmx)
      INTEGER :: nntot(mxdnrk)
      INTEGER :: nwhich(nshells)
      INTEGER :: nncell(3,mxdnrk,nnmx)
      INTEGER :: neigh(mxdnrk,nnmxh), ndim(nnmx)
      INTEGER :: nnshell(mxdnrk,nnmx)
 
      INTEGER :: i, j, l, m, n, nn, nnx, na, nap
      INTEGER :: nkp, nkp2, nkpts2
      INTEGER :: nlist, ndnn, nddn
      INTEGER :: ndnntot, nnsh, nnh
      INTEGER :: ifpos, ifneg, ifound, info, ind

      INTEGER :: nkpts_loc
      PARAMETER( nkpts_loc = 2000 )

      REAL(dbl) :: vkpr(3,nkpts_loc)
      REAL(dbl) :: vkpp(3)
      REAL(dbl) :: dimsingvd(nnmx)
      REAL(dbl) :: dimbk(3,nnmx)
      REAL(dbl) :: v1(nnmx,nnmx), v2(nnmx,nnmx)
      REAL(dbl) :: w1(10*nnmx)
      REAL(dbl) :: wtkpt(nkpts_loc)
      REAL(dbl) :: dnn0, dnn1, dist, bb1, bbn, factor

!

      CALL timing('bshells',OPR='start')

      IF ( nkpts_loc < nkpts ) THEN
        WRITE( stdout, * ) '  '
        WRITE( stdout, * ) '  '
        WRITE( stdout, * ) ' ******************* ERROR MESSAGE ******************'
        WRITE( stdout, * ) '  '
        WRITE( stdout, fmt= " (16x, ' NKPTS_LOC TOO SMALL ' )")
        WRITE( stdout, fmt= " (18x, 'nkpts_loc = ', i3 )" ) nkpts_loc
        WRITE( stdout, fmt= " (18x, 'nkpts = ', i3 )" ) nkpts
        WRITE( stdout, * ) '  '
        WRITE( stdout, * ) ' ****************************************************'
        WRITE( stdout, * ) '  '
        WRITE( stdout, * ) '  '
        CALL errore(' new_bshell ', ' nkpts_loc too small ', nkpts_loc )
      END IF

! ... Just so that knpt2 and wtkpt(nkp) are used properly later on
 
      nkpts2 = nkpts
      DO nkp = 1, nkpts
        wtkpt(nkp) = 1.0d0 / DBLE(nkpts)
      END DO


      WRITE( stdout, *) ' ' 
      WRITE( stdout, * ) ' ======================================================================'
      WRITE( stdout, * ) ' =                      B-shell calculations                          ='
      WRITE( stdout, * ) ' ======================================================================'
      WRITE( stdout, *) ' ' 
      WRITE( stdout, fmt= " (2x, 'K-point calculation: (cartesian coordinates in Ang^-1)' ) " )

! ... Pass the k-points in cartesian coordinates

      DO nkp = 1, nkpts
        DO i = 1, 3
          vkpr(i,nkp) = 0.0d0
          DO j = 1, 3
            vkpr(i,nkp) = vkpr(i,nkp) + vkpt(j,nkp) * recc(j,i)
          END DO
        END DO 
      WRITE( stdout, fmt= " (4x, 'k point', i4, ':   ( ',3f9.5, ' ),   weight = ', f8.4 ) " ) & 
            nkp, ( vkpr(i,nkp), i=1,3 ), wtkpt(nkp)
      END DO

! ... Find the distance between k-point 1 and its nearest-neighbour shells
!     if we have only one k-point, the n-neighbours are its periodic images 

      dnn0 = 0.0d0
      dnn1 = eta
      ndnntot = 0

! ... AC & MBN (April 2002) generic k grid allowed
 
      DO nlist = 1, nnmx
        DO nkp = 1, nkpts
          DO l = -5, 5
            DO m = -5, 5
              DO n = -5, 5
                vkpp(1) = vkpr(1,nkp) + l*recc(1,1) + m*recc(2,1) + n*recc(3,1)
                vkpp(2) = vkpr(2,nkp) + l*recc(1,2) + m*recc(2,2) + n*recc(3,2)
                vkpp(3) = vkpr(3,nkp) + l*recc(1,3) + m*recc(2,3) + n*recc(3,3)
                dist=SQRT( (vkpr(1,1)-vkpp(1))**2 + (vkpr(2,1)-vkpp(2))**2 + (vkpr(3,1)-vkpp(3))**2 )
                IF ( (dist > eps) .AND. (dist > dnn0+eps) ) dnn1 = MIN( dnn1, dist )
              END DO
            END DO
          END DO
        END DO
        IF ( dnn1 < eta-eps ) ndnntot = ndnntot + 1
        dnn(nlist) = dnn1
        dnn0 = dnn1
        dnn1 = eta
      END DO

      WRITE( stdout,*) ' '
      WRITE( stdout, fmt= " (2x, 'Nearest-neighbour shells for k-point 1: (in Ang^-1)' ) " )

      DO ndnn = 1, ndnntot
      WRITE( stdout, fmt= " (4x, 'shell (',i3,' )    radius = ', f9.5 )")  ndnn, dnn(ndnn)
      END DO
      WRITE( stdout,*) ' '

! ... Now build up the list of nearest-neighbour shells for each k-point.
!     nnlist(nkp,1...nnx) points to the nnx neighbours (ordered along increasing shells)
!     of the k-point nkp. nncell(i,nkp,nnth) tells us in which BZ is the nnth 
!     nearest-neighbour of the k-point nkp. Construct the nnx b-vectors that go from k-point
!     nkp to each neighbour bk(1:3,nkp,1...nnx).

      WRITE(stdout, fmt=" (2x,'Number of nearest-neighbours for K-point 1 (representetive for the BZ):')")

      DO nkp = 1, nkpts
        nnx = 0
        DO ndnc = 1, nshells
          ndnn = nwhich(ndnc)
          nnshell(nkp,ndnn) = 0
          DO nkp2 = 1, nkpts
            DO l = -5, 5
              DO m = -5, 5
                DO n = -5, 5
                  vkpp(1) = vkpr(1,nkp2) + l*recc(1,1) + m*recc(2,1) + n*recc(3,1)
                  vkpp(2) = vkpr(2,nkp2) + l*recc(1,2) + m*recc(2,2) + n*recc(3,2)
                  vkpp(3) = vkpr(3,nkp2) + l*recc(1,3) + m*recc(2,3) + n*recc(3,3)
                  dist = SQRT( ( vkpr(1,nkp) - vkpp(1) )**2 +( vkpr(2,nkp)-vkpp(2) )**2 +     &
                               ( vkpr(3,nkp) - vkpp(3) )**2 )
                  IF ( ( dist >=  dnn(ndnn) * 0.9999d0 ) .AND. ( dist <= dnn(ndnn) * 1.0001d0 ) )  THEN
                    nnx = nnx + 1   
                    nnshell(nkp,ndnn) = nnshell(nkp,ndnn) + 1
                    nnlist(nkp,nnx) = nkp2
                    nncell(1,nkp,nnx) = l
                    nncell(2,nkp,nnx) = m
                    nncell(3,nkp,nnx) = n
                    DO ind = 1, 3
                      bk(ind,nkp,nnx) = vkpp(ind) - vkpr(ind,nkp)
                    END DO
                  END IF
                END DO
              END DO
            END DO
          END DO
          IF ( nkp == 1) THEN
            WRITE( stdout, fmt= " (4x, 'shell (', i3, ' )    neighbours  = ', i3 )")  ndnn, nnshell(nkp,ndnn)
          END IF

          IF ( nnshell(nkp,ndnn) <= 0 ) &
            CALL errore(' new_bshell ', ' Shell is empty! ', nnshell(nkp,ndnn) )
          IF ( ( nnshell(nkp,ndnn) /= nnshell(1,ndnn) ) .AND. ( WTKPT(NKP) > 1e-10) ) &
            CALL errore(' new_bshell ', ' Non uniform neighbours! ', nnshell(nkp,ndnn) )

        END DO

        IF ( nnx > nnmx ) CALL errore(' new_bshell ', ' Too many neighbours !', nnx )

        nntot(nkp) = nnx
      END DO

! ... Check that the moduli of the b-vectors inside a shell are all identical

      DO nkp = 1, nkpts2
        nnx = 0
        DO ndnc = 1, nshells
          ndnn = nwhich(ndnc)
          DO nnsh = 1, nnshell(nkp,ndnn)
            bb1 = 0.0d0
            bbn = 0.0d0
            nnx = nnx + 1
            DO i = 1, 3
              bb1 = bb1 + bk(i,1,nnx) * bk(i,1,nnx)
              bbn = bbn + bk(i,nkp,nnx) * bk(i,nkp,nnx)
            END DO

            IF ( ABS( SQRT(bb1) - SQRT(bbn) ) > eps ) &
              CALL errore(' new_bshell ', ' Non-symmetric k-point neighbours!', bb1 )

          END DO
        END DO
      END DO

! ... Now find the dimensionality of each shell of neighbours

      nnx = 0
      DO ndnc = 1, nshells
        ndnn = nwhich(ndnc)
        ndim(ndnn) = 0

        DO nnsh = 1, nnshell(1,ndnn)
          nnx = nnx + 1
          DO i = 1, 3
            dimbk(i,nnsh) = bk(i,1,nnx)
          END DO
        END DO

        nnsh = nnshell(1,ndnn)

        IF( nnsh > nnmx ) &
          CALL errore(' wannier ',' nnsh too big ', nnsh )

        dimsingvd(:) = 0.d0

        CALL dgesvd( 'A', 'A', 3, nnsh, dimbk, 3, dimsingvd, v1, 3, v2, nnsh, w1, 10*nnsh, info )

        IF ( info /=0 ) &
          CALL errore(' new_bshell ', ' Singular value decomposition dgesvd failed ', info )

        DO nn = 1, nnsh
          IF ( ABS( dimsingvd(nn) ) > 1e-5 ) ndim(ndnn) = ndim(ndnn) + 1
        END DO

        factor = DBLE( ndim(ndnn) ) / nnshell(1,ndnn)
        WRITE( stdout, *) ' '
        WRITE( stdout, fmt= " (2x,'Check shell dimensionality and weights:')")
        WRITE( stdout, fmt= " (4x, 'shell (', i3, ' )    dimensionality  = ', i3 )")  ndnn, ndim(ndnn)
        WRITE( stdout, fmt= " (4x, 'w_b weight is 1/b^2 times', f8.4 )")  factor
        WRITE( stdout, *) ' '

!...    17/06/2004
        IF ( ( nshells == 1 ) .AND. ( ndim(1) == 3 ) ) THEN
          IF ( ( nnshell(1,ndnn) /= 6 ) .AND. ( nnshell(1,ndnn) /= 8) .AND. &
               ( nnshell(1,ndnn) /= 12 ) ) THEN
            WRITE( stdout, *) ' '
            WRITE(*, fmt=" (2x, 'Warning: weights must be as defined in Ref: PRB 56 12847 (1997)' )")
            WRITE(*, fmt=" (2x, 'otherwise code will stop! ')")
            END IF
        END IF
        IF ( ( nshells /= 1 ) .OR. ( ndim(1) /= 3 ) ) THEN
          WRITE( stdout, *) ' '
          WRITE(*, fmt=" (2x, 'Warning: weights must be as defined in Ref: PRB 56 12847 (1997)' )")
          WRITE(*, fmt=" (2x, 'otherwise code will stop! ')")
        END IF
!...

      END DO

      DO nkp = 1, nkpts2
        nnx = 0
        DO ndnc = 1, nshells
          ndnn = nwhich(ndnc)
          DO nnsh = 1, nnshell(nkp,ndnn)
            bb1 = 0.0d0
            bbn = 0.0d0
            nnx = nnx + 1
            DO i = 1, 3
              bb1 = bb1 + bk(i,1,nnx) * bk(i,1,nnx)
              bbn = bbn + bk(i,nkp,nnx) * bk(i,nkp,nnx)
            END DO
            wb(nkp,nnx) = DBLE( ndim(ndnn) ) / bbn / nnshell(nkp,ndnn)
          END DO
        END DO
      END DO

!...  17/06/2004
!     now check that the completeness relation is satisfied
!     Eq. B1 in Appendix  B PRB 56 12847 (1997)

      DO nkp = 1, nkpts2

        DO i = 1, 3
          DO j = 1, 3
            ddelta = 0.0
            nnx = 0

            DO ndnc = 1, nshells
              ndnn = nwhich(ndnc)
              DO nnsh =1, nnshell(1,ndnn)
                nnx = nnx + 1
                ddelta = ddelta + wb(nkp,nnx) * bk(i,nkp,nnx) * bk(j,nkp,nnx)
              END DO
            END DO

            IF ( ( i == j ) .AND. ( ABS( ddelta - 1.0d0 ) > eps ) ) &
              CALL errore(' new_bshell ', ' B1 not satisfied (I)', 1 )
         
            IF ( ( i /= j ) .AND. ( ABS( ddelta ) > eps ) ) &
              CALL errore(' new_bshell ', ' B1 not satisfied (II)', 1 )
        
          END DO
        END DO

      END DO

      WRITE( stdout, fmt=" (2x, 'Completeness relation is fully satisfied! (see Appendix B, PRB 56 12847 (1997)' )")
      WRITE(*,*) ' '
!...  

      wbtot = 0.0d0
      nnx = 0
      DO ndnc = 1, nshells
        ndnn = nwhich(ndnc)
        DO nnsh = 1, nnshell(1,ndnn)
          nnx = nnx + 1
          wbtot = wbtot + wb(1,nnx)
        END DO
      END DO

! ... Now it regroups the bk, in order to have an index that runs along
!     consistent directions

      DO ndnc = 1, nshells

        ndnn = nwhich(ndnc)
        nnh = nnshell(1,ndnn) / 2
        IF ( nnh*2 /= nnshell(1,ndnn) ) THEN
           WRITE( stdout, * ) ' ******************* ERROR MESSAGE ******************'
           WRITE( stdout, * ) '  '
           WRITE( stdout, fmt= " (6x, 'The number of neighbours in each shell must be even '&
                                  &, 2i5 )")  ndnn, nnshell(1,ndnn)
           WRITE( stdout, * ) '  '
           WRITE( stdout, * ) ' ****************************************************'
          CALL errore(' new_bshell ', ' Number of neighbours in each shell is too small !', nnh*2 )
        END IF

      END DO
      nnh = nntot(1) / 2

! ... Make list of bka vectors from neighbours of first k-point
!     delete any inverse vectors as you collect them

      na = 0
      DO nn = 1, nntot(1)

        ifound = 0
        IF ( na /= 0 ) THEN
          DO nap = 1, na
            CALL compar( bka(1,nap), bk(1,1,nn), ifpos, ifneg )
            IF ( ifneg == 1 ) ifound = 1
          END DO
        ENDIF

        IF ( ifound == 0 ) THEN
! ...     Found new vector to add to set
          na = na + 1
          bka(1,na) = bk(1,1,nn)
          bka(2,na) = bk(2,1,nn)
          bka(3,na) = bk(3,1,nn)
        END IF

      END DO

      IF ( na /= nnh ) CALL errore(' new_bshell ', ' Did not find right number of bk directions', na )

      WRITE (stdout , fmt="(2x, 'List of the ' , i2, ' vectors b_k: (Ang^-1) ') ") nntot(1)

      DO i = 1, nntot(1)
        WRITE( stdout, fmt= " (4x, 'b_k', i4, ':   ( ',3f9.5, ' ),   weight = ', f8.4 ) " ) &
        i, ( bk(j,1,i), j=1,3 ), wb(1,i)
      END DO

      WRITE (stdout, *) ' '
      WRITE (stdout , fmt="(2x, 'The ',i2, '  bk directions are:' )") nnh

      DO i=1,nnh
        WRITE( stdout, fmt= " (4x, 'dir', i2, ':   ( ',3f9.5, ' ) ' )" ) i, ( bka(j,i), j=1,3 )
      END DO
      WRITE (stdout, *) ' '

! ... Find index array

      DO nkp = 1, nkpts2

        DO na = 1, nnh

! ...     first, zero the index array so we can check it gets filled
          neigh(nkp,na) = 0

! ...     now search through list of neighbours of this k-point
          DO nn = 1, nntot(nkp)
            CALL compar( bka(1,na), bk(1,nkp,nn), ifpos, ifneg )
            IF ( ifpos == 1 ) neigh(nkp,na) = nn
          END DO

! ...     check found
          IF ( neigh(nkp,na) == 0 ) CALL errore(' new_bshell ', ' Check failed ', na )

        END DO

      END DO

      CALL timing('bshells',OPR='stop')

      RETURN
      END SUBROUTINE
