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
!******************************************************
   SUBROUTINE bshells( bvec, nshells, nwhich )
   !******************************************************
   !
   !  Computes the shells of b-vectors connecting every k-point to its
   !  neighbors, as well as their weights for the finite-difference formulas
   !  for the k-derivatives
   ! 
   USE kinds
   USE constants, ONLY : ZERO, CZERO, ONE, EPS_m10, EPS_m6
   USE util_module, ONLY : mat_svd
   USE timing_module, ONLY : timing
   USE summary_module, ONLY: summary 
   USE io_module, ONLY: stdout

   USE kpoints_module, ONLY : vkpt, nkpts, wk, nnshell, nreverse, &
                              bk, dnn, wb, wbtot, nnlist, nncell, &
                              nntot, bka, neigh, nnx, &
                              kpoints_alloc 
   IMPLICIT NONE

   !
   ! input and local variables
   !
      INTEGER, INTENT(in)   :: nshells
      INTEGER, INTENT(in)   :: nwhich(nshells)
      REAL(dbl), INTENT(in) :: bvec(3,3)
 

      REAL(dbl), PARAMETER :: eps = EPS_m6
 
      INTEGER :: ndnc
      INTEGER :: i, j, l, m, n, nn, in, na, nap
      INTEGER :: nkp, nkp2
      INTEGER :: nlist, ndnn
      INTEGER :: nnsh, nnh
      INTEGER :: ifpos, ifneg, ifound
      INTEGER :: ndim(nnx)

      REAL(dbl) :: ddelta
      REAL(dbl) :: vkpp(3), vshells(3,10*nnx)
      REAL(dbl) :: singvd(nnx)
      REAL(dbl) :: dimbk(3,nnx)
      REAL(dbl) :: v1(nnx,nnx), v2(nnx,nnx)
      REAL(dbl) :: eta, dnn0, dnn1, dist, bb1, bbn


      CALL timing('bshells',OPR='start')

      IF ( .NOT. kpoints_alloc ) CALL errore('bshell', 'Kpoints NOT alloc', 1 )
      IF ( nkpts <= 0) CALL errore('bshell', 'Invaid nkpts', ABS(nkpts)+1 )


!
! ... Find the distance between k-point 1 and its nearest-neighbour shells
!     if we have only one k-point, the n-neighbours are its periodic images 

      !
      ! this is a sup for the shell radius
      !
      eta = MAXVAL( ABS(bvec) ) * 100.0 
      !
      dnn0 = ZERO
      dnn1 = eta

      !
      ! ... AC & MBN (April 2002) generic k grid allowed
      !     AF (Jul 2005), updates
      !     everything in bohr^-1
      !
      DO nlist = 1, nnx
          DO nkp = 1, nkpts
              DO l = -5, 5
              DO m = -5, 5
              DO n = -5, 5
                !
                vkpp(:) = vkpt(:,nkp) + l*bvec(:,1) + m*bvec(:,2) + n*bvec(:,3)
                !
                dist=SQRT( (vkpt(1,1)-vkpp(1))**2 + &
                           (vkpt(2,1)-vkpp(2))**2 + &
                           (vkpt(3,1)-vkpp(3))**2 ) 
                IF ( dist > dnn0+eps ) dnn1 = MIN( dnn1, dist )
              ENDDO
              ENDDO
              ENDDO
          ENDDO
          !
          dnn(nlist) = dnn1
          dnn0 = dnn1
          dnn1 = eta
      ENDDO

!
! ... Now build up the list of nearest-neighbour shells for each k-point.
!     nnlist(nkp,1...nnx) points to the nnx neighbours (ordered along increasing shells)
!     of the k-point nkp. nncell(i,nnth,nkp) tells us in which BZ is the nnth 
!     nearest-neighbour of the k-point nkp. Construct the nnx b-vectors that go from k-point
!     nkp to each neighbour bk(1:3,nkp,1...nnx).
!
!     kvect in bohr^-1
!

      DO nkp = 1, nkpts
        nntot(nkp) = 0

        DO ndnc = 1, nshells
            ndnn = nwhich(ndnc)
            nnshell(nkp,ndnn) = 0
            DO nkp2 = 1, nkpts
                DO l = -5, 5
                DO m = -5, 5
                DO n = -5, 5
                    !
                    vkpp(:) = vkpt(:,nkp2) + l*bvec(:,1) + m*bvec(:,2) + n*bvec(:,3)
                    !
                    dist = SQRT( ( vkpt(1,nkp) - vkpp(1) )**2 + &
                                 ( vkpt(2,nkp) - vkpp(2) )**2 + &
                                 ( vkpt(3,nkp) - vkpp(3) )**2 ) 
                    IF ( ABS( dist - dnn(ndnn)) < eps )  THEN
                        nntot(nkp) = nntot(nkp) + 1   
                        !
                        IF ( nntot(nkp) > nnx ) THEN 
                            nntot(nkp) = 0
                            CALL summary(stdout, LEIG=.FALSE., LATOMS=.FALSE., LPSEUDO=.FALSE.)
                            CALL errore('bshell', 'Too many neighbours', nkp )
                        ENDIF
                        !
                        nnshell(nkp,ndnn) = nnshell(nkp,ndnn) + 1
 
                        in = nntot(nkp)
                        nnlist(nkp, in ) = nkp2
                        nncell(1, in, nkp) = l
                        nncell(2, in, nkp) = m
                        nncell(3, in, nkp) = n
                        !
                        ! units are in bohr-1
                        bk(:, nkp, in) = ( vkpp(:) - vkpt(:,nkp) )
                    ENDIF
                ENDDO
                ENDDO
                ENDDO
            ENDDO

            IF ( nnshell(nkp,ndnn) <= 0 ) &
                CALL errore('bshell', 'Shell is empty!', ABS(nnshell(nkp,ndnn))+1 )
            IF ( ( nnshell(nkp,ndnn) /= nnshell(1,ndnn) ) .AND. ( wk(nkp) > EPS_m10) ) &
                CALL errore('bshell ', ' Non uniform neighbours! ', &
                             ABS(nnshell(nkp,ndnn)-nnshell(1,ndnn)) )

        ENDDO 
      ENDDO
      !
      ! ... Check that the moduli of the b-vectors inside a shell are all identical
      !
      DO nkp = 1, nkpts
          in = 0
          DO ndnc = 1, nshells
              ndnn = nwhich(ndnc)
              IF ( ndnn > nnx ) CALL errore('bshell','invalid nwhich',ndnn)
              DO nnsh = 1, nnshell(nkp,ndnn)
                  in = in + 1
                  bb1 = DOT_PRODUCT( bk(:,1,in), bk(:,1,in) )
                  bbn = DOT_PRODUCT( bk(:,nkp,in), bk(:,nkp,in) )
            
                  IF ( ABS( SQRT(bb1) - SQRT(bbn) ) > eps ) &
                     CALL errore('bshell', ' Non-symmetric k-point neighbours!', bb1 )
              ENDDO
          ENDDO
      ENDDO

      !
      ! ... Now find the dimensionality of each shell of neighbours
      !
      in = 0
      DO ndnc = 1, nshells
          ndnn = nwhich(ndnc)
          ndim(ndnn) = 0

          DO nnsh = 1, nnshell(1,ndnn)
              in = in + 1
              dimbk(:,nnsh) = bk(:,1,in)
          ENDDO

          nnsh = nnshell(1,ndnn)
          IF( nnsh > nnx ) CALL errore(' wannier ',' nnsh too big ', nnsh )

          !
          ! use SVD decomposition
          !
          CALL mat_svd( 3, nnsh, dimbk, singvd, v1, v2)
          DO nn = 1, nnsh
             IF ( ABS( singvd(nn) ) > eps ) ndim(ndnn) = ndim(ndnn) + 1
          ENDDO

          !
          ! Some checks on the NNs
          !
          IF ( ( nshells == 1 ) .AND. ( ndim(1) == 3 ) ) THEN
              IF ( ( nnshell(1,ndnn) /= 6 ) .AND. ( nnshell(1,ndnn) /= 8) .AND. &
                   ( nnshell(1,ndnn) /= 12 ) ) THEN
                   !
                   ! for a more detailed treatment see Ref. PRB 56, 12847 (1997)
                   CALL errore('bshells','Invalid number of NNeighb.',ABS(nnshell(1,ndnn))+1)
              ENDIF
       !  ELSE
       !    CALL errore('bshells','Invalid number of NNeighb. II',ABS(nshells)+1)
          ENDIF
      ENDDO


      DO nkp = 1, nkpts
          in = 0
          DO ndnc = 1, nshells
              ndnn = nwhich(ndnc)
              DO nnsh = 1, nnshell(nkp,ndnn)
                  in = in + 1
                  bbn = DOT_PRODUCT( bk(:,nkp,in), bk(:,nkp,in))
                  wb(nkp,in) = DBLE( ndim(ndnn) ) / bbn / nnshell(nkp,ndnn)
              ENDDO
          ENDDO
      ENDDO

!
!    now check that the completeness relation is satisfied
!  ( see Appendix B, PRB 56 12847 (1997) for more details )
!
      DO nkp = 1, nkpts

          DO i = 1, 3
          DO j = 1, 3

              ddelta = ZERO
              in = 0

              DO ndnc = 1, nshells
                  ndnn = nwhich(ndnc)
                  DO nnsh =1, nnshell(1,ndnn)
                      in = in + 1
                      ddelta = ddelta + wb(nkp,in) * bk(i,nkp,in) * bk(j,nkp,in)
                  ENDDO
              ENDDO

              IF ( ( i == j ) .AND. ( ABS( ddelta - ONE ) > eps ) ) THEN
                  CALL summary(stdout, LEIG=.FALSE., LATOMS=.FALSE., LPSEUDO=.FALSE.)
                  CALL errore('bshell', 'B1 not satisfied (I)', 1 )
              ENDIF
              ! 
              IF ( ( i /= j ) .AND. ( ABS( ddelta ) > eps ) ) THEN
                  CALL summary(stdout, LEIG=.FALSE., LATOMS=.FALSE., LPSEUDO=.FALSE.)
                  CALL errore('bshell', 'B1 not satisfied (II)', 1 )
              ENDIF
        
          ENDDO
          ENDDO
      ENDDO


!
! Total weight
!
      wbtot = ZERO
      in = 0
      DO ndnc = 1, nshells
          ndnn = nwhich(ndnc)
          DO nnsh = 1, nnshell(1,ndnn)
              in = in + 1
              wbtot = wbtot + wb(1,in)
          ENDDO
      ENDDO


!
! Now it regroups the bk, in order to have an index that runs along
! consistent directions
!
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
            !
            ! Found new vector to add to set
            !
            na = na + 1
            bka(1,na) = bk(1,1,nn)
            bka(2,na) = bk(2,1,nn)
            bka(3,na) = bk(3,1,nn)
        ENDIF
      ENDDO
      IF ( na /= nnh )  &
         CALL errore(' bshell ', ' Wrong number of bk directions', ABS(na-nnh))


      !
      ! Find index array
      !
      DO nkp = 1, nkpts
          DO na = 1, nnh
              !
              ! first, zero the index array so we can check it gets filled
              !
              neigh(nkp,na) = 0
              !
              ! now search through list of neighbours of this k-point
              !
              DO nn = 1, nntot(nkp)
                  CALL compar( bka(1,na), bk(1,nkp,nn), ifpos, ifneg )
                  IF ( ifpos == 1 ) neigh(nkp,na) = nn
              ENDDO
              !
              ! check found
              IF ( neigh(nkp,na) == 0 ) &
                    CALL errore(' bshell ', ' Check on neigh failed ', na )
          ENDDO

      ENDDO


      !
      ! Built up nreverse(nkpts, nnx) array index, which gives for each
      ! IK and INN (related to k, b) the index corresponding to the NN
      ! -b for the k+b kpt. It is used to symmetrize the Mkb overlap int
      !
      DO nkp = 1, nkpts
          DO nn = 1, nntot(nkp)

             nreverse(nn,nkp) = 0
             !
             ! get the k+b index
             nkp2 = nnlist(nkp, nn)

             !
             ! now search the -b vecotr corresponding to k+b
             DO na=1,nntot(nkp2)
                 CALL compar( bk(1,nkp2,na), bk(1,nkp,nn), ifpos, ifneg )
                 IF ( ifneg == 1 ) nreverse(nn,nkp) = na
             ENDDO
             !
             ! check found
             IF (  nreverse(nn,nkp) == 0 ) &
                    CALL errore(' bshell ', ' Check on nreverse failed ', nkp )
          ENDDO
      ENDDO



      CALL timing('bshells',OPR='stop')

      RETURN
   END SUBROUTINE


