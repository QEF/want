!
! Copyright (C) 2004 WanT Group
! Copyright (C) 2002 Nicola Marzari, Ivo Souza, David Vanderbilt
! Copyright (C) 1997 Nicola Marzari, David Vanderbilt
!
! This file is distributed under the terms of the
! GNU General Public License. See the file `License'
! in the root directory of the present distribution,
! or http://www.gnu.org/copyleft/gpl.txt .
!

!=----------------------------------------------------------------------------------=
      SUBROUTINE bshells( bvec, nshells, nwhich )
!=----------------------------------------------------------------------------------=

      USE kinds
      USE constants, ONLY : ZERO, CZERO, ONE, EPS_m10, EPS_m6
      USE timing_module, ONLY : timing

      USE kpoints_module, ONLY : vkpt, nk, s, nkpts, wk, nnshell, &
                                 bk, dnn, ndnntot, wb, wbtot, nnlist, nncell, &
                                 nntot, bka, neigh, nnx, &
                                 kpoints_alloc 
 
! ... Computes the shells of b-vectors connecting every k-point to its
!     neighbors, as well as their weights for the finite-difference formulas
!     for the k-derivatives
 
      IMPLICIT NONE

      INTEGER, INTENT(in)   :: nshells
      INTEGER, INTENT(in)   :: nwhich(nshells)
      REAL(dbl), INTENT(in) :: bvec(3,3)
 
      REAL(dbl) :: eta, eps
      PARAMETER( eta = 99999999.0d0 )
      PARAMETER( eps = EPS_m6 )
 
 
      REAL(dbl) :: ddelta
 
      INTEGER :: ndnc
      INTEGER :: i, j, l, m, n, nn, inx, na, nap, ierr
      INTEGER :: nkp, nkp2, nkpts2
      INTEGER :: nlist, ndnn, nddn
      INTEGER :: nnsh, nnh
      INTEGER :: ifpos, ifneg, ifound, info, ind
      INTEGER :: ndim(nnx)

      REAL(dbl), ALLOCATABLE :: vkpr(:,:)
      REAL(dbl) :: vkpp(3)
      REAL(dbl) :: dimsingvd(nnx)
      REAL(dbl) :: dimbk(3,nnx)
      REAL(dbl) :: v1(nnx,nnx), v2(nnx,nnx)
      REAL(dbl) :: w1(10*nnx)
      REAL(dbl) :: dnn0, dnn1, dist, bb1, bbn, factor

!

      CALL timing('bshells',OPR='start')

      IF ( .NOT. kpoints_alloc ) CALL errore('bshell', 'Kpoints NOT alloc', 1 )
      IF ( nkpts <= 0) CALL errore('bshell', 'Invaid nkpts', ABS(nkpts)+1 )

      ALLOCATE( vkpr(3,nkpts), STAT=ierr )
      IF ( ierr /= 0) CALL errore('bshell', 'allocating vkpr', ABS(ierr))

! ... Just so that nkpt2 is used properly later on
      nkpts2 = nkpts

! ... Pass the k-points in cartesian coordinates

      DO nkp = 1, nkpts
         DO i = 1, 3
            vkpr(i,nkp) = ZERO
            DO j = 1, 3
              vkpr(i,nkp) = vkpr(i,nkp) + vkpt(j,nkp) * bvec(i,j)  
            ENDDO
         ENDDO 
      ENDDO

! ... Find the distance between k-point 1 and its nearest-neighbour shells
!     if we have only one k-point, the n-neighbours are its periodic images 

      dnn0 = ZERO
      dnn1 = eta
      ndnntot = 0


! ... AC & MBN (April 2002) generic k grid allowed
!     everything in bohr^-1
 
      DO nlist = 1, nnx
        DO nkp = 1, nkpts
          DO l = -5, 5
            DO m = -5, 5
              DO n = -5, 5
                vkpp(1) = vkpr(1,nkp) + l*bvec(1,1) + m*bvec(1,2) + n*bvec(1,3)
                vkpp(2) = vkpr(2,nkp) + l*bvec(2,1) + m*bvec(2,2) + n*bvec(2,3)
                vkpp(3) = vkpr(3,nkp) + l*bvec(3,1) + m*bvec(3,2) + n*bvec(3,3)
                dist=SQRT( (vkpr(1,1)-vkpp(1))**2 + &
                           (vkpr(2,1)-vkpp(2))**2 + &
                           (vkpr(3,1)-vkpp(3))**2 ) 
                IF ( (dist > eps) .AND. (dist > dnn0+eps) ) dnn1 = MIN( dnn1, dist )
              END DO
            END DO
          END DO
        END DO
        IF ( dnn1 < eta-eps ) ndnntot = ndnntot + 1
        dnn(nlist) = dnn1
        dnn0 = dnn1
        dnn1 = eta
      ENDDO

! ... Now build up the list of nearest-neighbour shells for each k-point.
!     nnlist(nkp,1...nnx) points to the nnx neighbours (ordered along increasing shells)
!     of the k-point nkp. nncell(i,nnth,nkp) tells us in which BZ is the nnth 
!     nearest-neighbour of the k-point nkp. Construct the nnx b-vectors that go from k-point
!     nkp to each neighbour bk(1:3,nkp,1...nnx).
!
!     kvect in bohr^-1

      DO nkp = 1, nkpts
        inx = 0
        DO ndnc = 1, nshells
          ndnn = nwhich(ndnc)
          nnshell(nkp,ndnn) = 0
          DO nkp2 = 1, nkpts
            DO l = -5, 5
              DO m = -5, 5
                DO n = -5, 5
                  vkpp(1) = vkpr(1,nkp2) + l*bvec(1,1) + m*bvec(1,2) + n*bvec(1,3)
                  vkpp(2) = vkpr(2,nkp2) + l*bvec(2,1) + m*bvec(2,2) + n*bvec(2,3)
                  vkpp(3) = vkpr(3,nkp2) + l*bvec(3,1) + m*bvec(3,2) + n*bvec(3,3)
                  dist = SQRT( ( vkpr(1,nkp) - vkpp(1) )**2 + &
                               ( vkpr(2,nkp) - vkpp(2) )**2 + &
                               ( vkpr(3,nkp) - vkpp(3) )**2 ) 
                  IF ( ( dist >=  dnn(ndnn) * 0.9999d0 )  .AND. &
                       ( dist <= dnn(ndnn) * 1.0001d0 ) )  THEN
                    inx = inx + 1   
                    nnshell(nkp,ndnn) = nnshell(nkp,ndnn) + 1
                    nnlist(nkp,inx) = nkp2
                    nncell(1,inx,nkp) = l
                    nncell(2,inx,nkp) = m
                    nncell(3,inx,nkp) = n
                    !
                    ! units are in bohr-1
                    bk(:,nkp,inx) = ( vkpp(:) - vkpr(:,nkp) )
                  ENDIF
                ENDDO
              ENDDO
            ENDDO
          ENDDO

          IF ( nnshell(nkp,ndnn) <= 0 ) &
              CALL errore(' bshell ', ' Shell is empty! ', ABS(nnshell(nkp,ndnn))+1 )
          IF ( ( nnshell(nkp,ndnn) /= nnshell(1,ndnn) ) .AND. ( wk(nkp) > EPS_m10) ) &
              CALL errore(' bshell ', ' Non uniform neighbours! ', &
                       ABS(nnshell(nkp,ndnn)-nnshell(1,ndnn)) )

        ENDDO  ! kpoints

        IF ( inx > nnx ) CALL errore(' bshell ', ' Too many neighbours !', inx )

        nntot(nkp) = inx
      END DO

! ... Check that the moduli of the b-vectors inside a shell are all identical

      DO nkp = 1, nkpts2
        inx = 0
        DO ndnc = 1, nshells
          ndnn = nwhich(ndnc)
          DO nnsh = 1, nnshell(nkp,ndnn)
            bb1 = 0.0d0
            bbn = 0.0d0
            inx = inx + 1
            DO i = 1, 3
              bb1 = bb1 + bk(i,1,inx) * bk(i,1,inx)
              bbn = bbn + bk(i,nkp,inx) * bk(i,nkp,inx)
            END DO

            IF ( ABS( SQRT(bb1) - SQRT(bbn) ) > eps ) &
              CALL errore(' bshell ', ' Non-symmetric k-point neighbours!', bb1 )

          END DO
        END DO
      END DO

! ... Now find the dimensionality of each shell of neighbours

      inx = 0
      DO ndnc = 1, nshells
        ndnn = nwhich(ndnc)
        ndim(ndnn) = 0

        DO nnsh = 1, nnshell(1,ndnn)
          inx = inx + 1
          DO i = 1, 3
            dimbk(i,nnsh) = bk(i,1,inx)
          END DO
        END DO

        nnsh = nnshell(1,ndnn)

        IF( nnsh > nnx ) &
          CALL errore(' wannier ',' nnsh too big ', nnsh )

        dimsingvd(:) = ZERO

        CALL dgesvd( 'A', 'A', 3, nnsh, dimbk, 3, dimsingvd, v1, 3, v2, &
                    nnsh, w1, 10*nnsh, info )

        IF ( info /=0 ) &
          CALL errore(' bshell ', ' Singular value decomposition dgesvd failed ', info )

        DO nn = 1, nnsh
          IF ( ABS( dimsingvd(nn) ) > 1e-5 ) ndim(ndnn) = ndim(ndnn) + 1
        END DO


!
!...    Some checks on the NNs
!
        IF ( ( nshells == 1 ) .AND. ( ndim(1) == 3 ) ) THEN
          IF ( ( nnshell(1,ndnn) /= 6 ) .AND. ( nnshell(1,ndnn) /= 8) .AND. &
               ( nnshell(1,ndnn) /= 12 ) ) THEN
               !
               ! for a more detailed treatment see Ref. PRB 56, 12847 (1997)
               !
               CALL errore('bshells','Invalid number of NNeighb.',ABS(nnshell(1,ndnn))+1)
            ENDIF
       !  ELSE
       !    CALL errore('bshells','Invalid number of NNeighb. II',ABS(nshells)+1)
        ENDIF
!...

      ENDDO

      DO nkp = 1, nkpts2
        inx = 0
        DO ndnc = 1, nshells
          ndnn = nwhich(ndnc)
          DO nnsh = 1, nnshell(nkp,ndnn)
            bb1 = ZERO
            bbn = ZERO
            inx = inx + 1
            DO i = 1, 3
              bb1 = bb1 + bk(i,1,inx) * bk(i,1,inx)
              bbn = bbn + bk(i,nkp,inx) * bk(i,nkp,inx)
            END DO
            wb(nkp,inx) = DBLE( ndim(ndnn) ) / bbn / nnshell(nkp,ndnn)
          END DO
        END DO
      END DO

!...  17/06/2004
!     now check that the completeness relation is satisfied
!     Eq. B1 in Appendix  B PRB 56 12847 (1997)

      DO nkp = 1, nkpts2

        DO i = 1, 3
          DO j = 1, 3
            ddelta = ZERO
            inx = 0

            DO ndnc = 1, nshells
              ndnn = nwhich(ndnc)
              DO nnsh =1, nnshell(1,ndnn)
                inx = inx + 1
                ddelta = ddelta + wb(nkp,inx) * bk(i,nkp,inx) * bk(j,nkp,inx)
              END DO
            END DO

            IF ( ( i == j ) .AND. ( ABS( ddelta - 1.0d0 ) > eps ) ) &
              CALL errore('bshell', 'B1 not satisfied (I)', 1 )
         
            IF ( ( i /= j ) .AND. ( ABS( ddelta ) > eps ) ) &
              CALL errore('bshell', 'B1 not satisfied (II)', 1 )
        
          END DO
        END DO

      END DO
      !
      ! Completeness relation is fully satisfied!
      ! ( see Appendix B, PRB 56 12847 (1997) for more details )
      !

!...  

      wbtot = ZERO
      inx = 0
      DO ndnc = 1, nshells
        ndnn = nwhich(ndnc)
        DO nnsh = 1, nnshell(1,ndnn)
          inx = inx + 1
          wbtot = wbtot + wb(1,inx)
        END DO
      END DO

! ... Now it regroups the bk, in order to have an index that runs along
!     consistent directions

      DO ndnc = 1, nshells

        ndnn = nwhich(ndnc)
        nnh = nnshell(1,ndnn) / 2
        IF ( nnh*2 /= nnshell(1,ndnn) ) &
          CALL errore('bshell', 'Number of neighbours not EVEN in each shell',ABS(nnh)+1 )

      ENDDO
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
      IF ( na /= nnh ) CALL errore(' bshell ', ' Wrong number of bk directions', ABS(na-nnh))


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
          IF ( neigh(nkp,na) == 0 ) CALL errore(' bshell ', ' Check failed ', na )

        END DO

      END DO

      DEALLOCATE( vkpr, STAT=ierr )
      IF ( ierr /= 0) CALL errore('bshell', 'deallocating vkpr', ABS(ierr))

      CALL timing('bshells',OPR='stop')

      RETURN
      END SUBROUTINE


