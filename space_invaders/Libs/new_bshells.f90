      SUBROUTINE bshells( vkpt, nkpts, recc, nshells, nwhich, nnshell,   &
                  bk, dnn, wb, wbtot, nnlist, nncell, nntot, bka, neigh, mxdnrk )
 
! ... Computes the shells of b-vectors connecting every k-point to its
!     neighbors, as well as their weights for the finite-difference formulas
!     for the k-derivatives
 
      IMPLICIT none
 
      INTEGER :: nnmx, nnmxh
      PARAMETER( nnmx = 24 )
      PARAMETER( nnmxh = nnmx/2 )
      REAL*8 :: eta, eps
      PARAMETER( eta = 99999999.0d0 )
      PARAMETER( eps = 1d-6 )
 
      INTEGER :: nkpts, nshells
      INTEGER :: mxdnrk, ndnc
      REAL*8 :: vkpt(3,mxdnrk), recc(3,3)
 
      REAL*8 :: bk(3,mxdnrk,nnmx)
      REAL*8 :: dnn(nnmx)
      REAL*8 :: wb(mxdnrk,nnmx)
      REAL*8 :: wbtot
      REAL*8 :: bka(3,nnmxh)
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

      REAL*8 :: vkpr(3,nkpts_loc)
      REAL*8 :: vkpp(3)
      REAL*8 :: dimsingvd(nnmx)
      REAL*8 :: dimbk(3,nnmx)
      REAL*8 :: v1(nnmx,nnmx), v2(nnmx,nnmx)
      REAL*8 :: w1(10*nnmx)
      REAL*8 :: wtkpt(nkpts_loc)
      REAL*8 :: dnn0, dnn1, dist, bb1, bbn, factor

!

      IF ( nkpts_loc < nkpts ) THEN
        WRITE(6,*) ' '
        WRITe(6,*) 'In bshells.f'
        WRITE(6,*) 'nkpts_loc=', nkpts_loc
        WRITE(6,*) 'nkpts=', nkpts
        WRITE(6,*) 'increase nkpts_loc'
        STOP
      END IF
 
! ... Just so that knpt2 and wtkpt(nkp) are used properly later on
 
      nkpts2 = nkpts
      DO nkp = 1, nkpts
        wtkpt(nkp) = 1.0d0 / DBLE(nkpts)
      END DO


      WRITE(*,*) 
      WRITE(*,*) ' Starting BSHELL '
      WRITE(*,*) ' GEOMETRY PRELIMS ---------------------------------'

! ... Pass the k-points in cartesian coordinates

      WRITE(*,*) ' '
      DO nkp = 1, nkpts
        DO i = 1, 3
          vkpr(i,nkp) = 0.0d0
          DO j = 1, 3
            vkpr(i,nkp) = vkpr(i,nkp) + vkpt(j,nkp) * recc(j,i)
          END DO
        END DO 
        WRITE(*,7017) nkp, ( vkpr(i,nkp), i=1,3 ), wtkpt(nkp)
      END DO
 7017 FORMAT( '  K-POINT (CARTESIAN)', i6, ':',3F10.5, '   WEIGHT =', F10.5 )

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

      WRITE(*,*) ' '
      WRITE(*,*) ' Nearest-neighbour shells for k-point 1 are (in Bohr^-1)'
      WRITE(*,*) ' '

      DO ndnn = 1, ndnntot
        WRITE(*,*) ndnn, dnn(ndnn)
      END DO
      WRITE(*,*) ' '

! ... Now build up the list of nearest-neighbour shells for each k-point.
!     nnlist(nkp,1...nnx) points to the nnx neighbours (ordered along increasing shells)
!     of the k-point nkp. nncell(i,nkp,nnth) tells us in which BZ is the nnth 
!     nearest-neighbour of the k-point nkp. Construct the nnx b-vectors that go from k-point
!     nkp to each neighbour bk(1:3,nkp,1...nnx).

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
          WRITE(*,8003) nkp, nnshell(nkp,ndnn), ndnn
          IF ( nnshell(nkp,ndnn) <= 0 ) THEN
            WRITE(*,*) ' Shell is empty ! ', nkp, nnx, nnshell(nkp,ndnn), ndnn
            STOP
          END IF
          IF (( nnshell(nkp,ndnn) /= nnshell(1,ndnn) ) .AND. ( WTKPT(NKP) > 1e-10) ) THEN
            WRITE(*,*) ' Non uniform neighbours !', nkp,nnx,nnshell(nkp,ndnn), ndnn
            STOP
          END IF
        END DO
        IF ( nnx > nnmx ) THEN
          WRITE(*,*) ' Too many neighbours !'
          STOP
        END IF
        nntot(nkp) = nnx
      END DO

8003  FORMAT ( 2x, 'K-point', i5, ' has',i3, ' neighbours in the', i3, ' shell' )
      
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
            IF ( ABS( SQRT(bb1) - SQRT(bbn) ) > eps ) THEN
              WRITE(*,*) ' Non-symmetric k-point neighbours !',bb1,bbn
              STOP
            END IF
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

        DO i = 1, nnmx
          dimsingvd(i) = 0.d0
        END DO

        CALL dgesvd( 'A', 'A', 3, nnsh, dimbk, 3, dimsingvd, v1, 3, v2, nnsh, w1, 10*nnsh, info )

        IF ( info /=0 ) THEN
          WRITE(*,*) 'Singular value decomposition dgesvd failed'
          STOP
        END IF

        DO nn = 1, nnsh
          IF ( ABS( dimsingvd(nn) ) > 1e-5 ) ndim(ndnn) = ndim(ndnn) + 1
        END DO

        factor = DBLE( ndim(ndnn) ) / nnshell(1,ndnn)
        WRITE(*,*) ndnn, ' shell '
        WRITE(*,*) ' '
        WRITE(*,*) ' dimensionality is        ', ndim(ndnn)
        WRITE(*,*) ' w_b weight is 1/b^2 times', factor
        WRITE(*,*) ' '
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
          WRITE (*,*) 'The number of neighbours in each shell must be even',ndnn,nnshell(1,ndnn)
          STOP
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

      IF ( na /= nnh ) THEN
        WRITE (*,*) ' Did not find right number of bk directions'
        STOP
      END IF

      WRITE (*,*) ' The vectors b_k are ',nntot(1)
      WRITE (*,*) ' '

      DO i = 1, nntot(1)
        WRITE (*,'(4f10.5)') ( bk(j,1,i), j=1,3 ), wb(1,i)
      END DO

      WRITE (*,*) ' '
      WRITE (*,*) ' The bk directions are ', nnh
      WRITE (*,*) ' '

      DO i=1,nnh
        WRITE (*,'(3f10.5)') ( bka(j,i), j=1,3 )
      END DO
      WRITE (*,*) ' '


! ... Find index array

      WRITE(*,*) ' K-point, neighbor directions'
      WRITE(*,*) ' '

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
          IF ( neigh(nkp,na) == 0 ) THEN
            WRITE (*,*) ' nkp,na=', nkp, na
            STOP
          END IF

        END DO
        WRITE (*,'(i6,6x,6i6)') nkp, ( neigh(nkp,na), na=1,nnh )

      END DO
      WRITE(*,*) ' '

      WRITE(*,*) 
      WRITE(*,*) ' BSHELL, done. '
      WRITE(*,*) 

      RETURN
      END SUBROUTINE
