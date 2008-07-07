!
! Copyright (C) 2005 WanT Group
!
! This file is distributed under the terms of the
! GNU General Public License. See the file `License'
! in the root directory of the present distribution,
! or http://www.gnu.org/copyleft/gpl.txt .
!
!******************************************************
   SUBROUTINE bshells_init( )
   !******************************************************
   !
   !  Computes the shells of b-vectors connecting every k-point to its
   !  neighbors, as well as their weights for the finite-difference formulas
   !  for the k-derivatives
   ! 
   USE kinds
   USE want_interfaces_module
   USE constants,       ONLY : ZERO, CZERO, ONE, TWO, EPS_m10, EPS_m6
   USE util_module,     ONLY : mat_svd, mat_sv, mat_rank
   USE timing_module,   ONLY : timing
   USE io_module,       ONLY : stdout
   USE log_module,      ONLY : log_push, log_pop
   !
   USE parameters,      ONLY : nnx
   USE lattice_module,  ONLY : bvec, lattice_alloc => alloc
   USE kpoints_module,  ONLY : vkpt, nkpts, nk, s, nb, vb, wb, wbtot, &
                               nnlist, nncell, nnrev, nnpos,          &
                               kpoints_alloc, bshells_allocate 
   !
   IMPLICIT NONE

   !
   ! local variables
   !
   
   REAL(dbl), PARAMETER :: eps = EPS_m6
   ! 
   INTEGER       :: ik, ik1, ib, ierr
   INTEGER       :: i, j, l, m, n
   INTEGER       :: nq, nqx, rank
   INTEGER       :: nk_(3), s_(3)
   INTEGER       :: lwork_aux, info, ipiv(6)
   LOGICAL       :: eqv_found, found
   REAL(dbl)     :: aux, aux1, vkb(3), vtmp(3)
   !
   INTEGER,   ALLOCATABLE :: ivb(:), index(:)
   REAL(dbl), ALLOCATABLE :: vb_aux(:,:), vq(:,:), vqq(:)
   REAL(dbl), ALLOCATABLE :: work(:,:), work1(:,:), rhs(:)
   REAL(dbl), ALLOCATABLE :: work_aux(:)
   !
   CHARACTER(12) :: subname = 'bshells_init'
   !
   ! end of declariations
   !

!
!------------------------------
! main body
!------------------------------
!
   CALL timing( subname, OPR='start')
   CALL log_push( subname )

   IF ( .NOT. lattice_alloc ) CALL errore(subname, 'lattice NOT alloc', 1 )
   IF ( .NOT. kpoints_alloc ) CALL errore(subname, 'kpoints NOT alloc', 1 )
   IF ( nkpts <= 0) CALL errore(subname, 'Invaid nkpts', ABS(nkpts)+1 )

   !
   ! if not already done, check that the kpt grid is Monkhorst-pack
   !
   CALL get_monkpack(nk_, s_, nkpts, vkpt, 'CARTESIAN', bvec, ierr)
   IF ( ierr /= 0) CALL errore(subname,'kpt grid not Monkhorst-Pack',ABS(ierr))
   !
   IF ( ANY( nk(:) /= nk_(:)) ) CALL errore(subname, 'invalid nk from kpt grid', 71) 
   IF ( ANY( s(:) /= s_(:)) )   CALL errore(subname, 'invalid s from kpt grid', 71) 

!
! Find the distance between k-point 1 and its nearest-neighbour shells
! if we have only one k-point, the n-neighbours are its periodic images 
!

   !
   ! allocate the trial kpoints, searching for the shells
   ! 125 = 5 **3 
   !
   nq  = 125*nkpts
   nqx = 125

   ALLOCATE( vq(3,nq), vqq(nq), index(nq),  STAT=ierr )
   IF (ierr/=0) CALL errore(subname,'allocating vq--index',ABS(ierr))
   ALLOCATE( vb_aux(3, nnx), ivb(nnx),  STAT=ierr )
   IF (ierr/=0) CALL errore(subname,'allocating vb_aux, ivb',ABS(ierr))

   !
   ! loop over 125 different cells in rec space
   i = 0
   DO l = -2, 2
   DO m = -2, 2
   DO n = -2, 2
        !
        ! loop over kpts in each cell
        DO ik = 1, nkpts
             !
             i = i+1
             vq(:,i) = vkpt(:,ik) -vkpt(:,1) + l*bvec(:,1) + m*bvec(:,2) + n*bvec(:,3)
             !
             vqq(i)= DOT_PRODUCT( vq(:,i), vq(:,i) )
        ENDDO
   ENDDO
   ENDDO
   ENDDO

   !
   ! order the vq vectors for increasing moduli
   !
   index (:) = 0
   CALL hpsort_eps(nq, vqq(:), index(:), EPS_m6 )

!
! Now we deternine the possible b-vectors.
! Eventual useless b vects will be deleted after weight calculation if 
! zero-weight is found.
!

   ALLOCATE( work( 6, nq ), STAT=ierr )
   IF (ierr/=0 ) CALL errore(subname,'allocating work ',ABS(ierr))
   ALLOCATE( rhs( 6 ), STAT=ierr )
   IF (ierr/=0 ) CALL errore(subname,'allocating rhs ',ABS(ierr))
   ALLOCATE( work1( 6 , 6), STAT=ierr )
   IF (ierr/=0 ) CALL errore(subname,'allocating work1 ',ABS(ierr))


   !
   ! then impose the sum rule to find the weights
   ! according to App.B in PRB 56, 12847 (1997)
   ! SumRule:  \sum_b wb |b >< b| = I  
   ! which are 6 eqs in wb, 
   !
   !
   DO i=1,nq
      j = index(i)
      work( 1, i ) = vq( 1, j) * vq( 1, j)
      work( 2, i ) = vq( 1, j) * vq( 2, j)
      work( 3, i ) = vq( 1, j) * vq( 3, j)
      work( 4, i ) = vq( 2, j) * vq( 2, j)
      work( 5, i ) = vq( 2, j) * vq( 3, j)
      work( 6, i ) = vq( 3, j) * vq( 3, j)
   ENDDO
   !
   rhs(1) = ONE
   rhs(2) = ZERO
   rhs(3) = ZERO
   rhs(4) = ONE
   rhs(5) = ZERO
   rhs(6) = ONE

   !
   ! since we sum over 6 vectors only
   ! basically we are considering only one vector for each
   ! b, -b couple
   rhs  = rhs / TWO

!   rank =  mat_rank( 6, nq-1, work, EPS_m6 )
!   IF ( rank/=6 ) CALL errore(subname,'rank /= 6 ',ABS(rank)+1)

!
! determine the number of b-vectors:
! * we find out a full-rank (6) submatrix from work,
!   selecting suitable b vectors (columns), 
! * we solve the linear system above
! * select the bvectors
!   corresponding to non negligible weights
!

   !
   ! first found the candidates b-vectors:
   ! search for 6 vectors not parallel to each other,
   ! always include the 3 vectors parallel to the generators of
   ! the reciprocal lattice
   !

   !
   work1(:,:) = ZERO

   !
   ! found the 3 vectors parallel to the reciprocal lattice gen.
   !
   DO j = 1, 3

       ! squared norm of the generators
       aux1 = DOT_PRODUCT( bvec(:,j), bvec(:,j) )
 
       found = .FALSE.
       !
       inner_b_loop: &
       DO l = 2, nq
           !
           aux = DOT_PRODUCT( bvec(:,j), vq(:,index(l)) ) /  &
                     SQRT( aux1 * vqq(l) )
           aux = ABS(aux)
           !
           IF ( ABS( aux - ONE ) < EPS_m6 ) THEN
               found = .TRUE.
               !
               ivb(j)  = index(l)
               !
               vb_aux(:,j) = vq(:, index(l) )
               work1(:,j)  = work(:, l ) 
               !
               EXIT inner_b_loop
               !
           ENDIF
           !
       ENDDO inner_b_loop
       !
       IF ( .NOT. found ) CALL errore(subname,'vb parallel to bvec not found',j)
       !
   ENDDO

   IF ( mat_rank( 6, 3, work1, EPS_m6 ) /= 3 ) &
      CALL errore(subname,'work1 is a rank-deficient matrix I',3)

   !
   ! now search for the remaining vectors
   !
   outer_column_loop: &
   DO i = 4, 6
       !
       found = .FALSE.
       !
       inner_column_loop: &
       DO l = 2, nq
           !
           work1(:,i) = work(:,l)
           IF ( mat_rank( 6, i, work1, EPS_m6 ) == i ) THEN
               !
               found = .TRUE.
               ivb(i)  = index(l)
               !
               vb_aux(:,i) = vq(:, index(l) )
               !
               EXIT inner_column_loop
               !
           ENDIF
           !
       ENDDO inner_column_loop
       !
       IF ( .NOT. found ) CALL errore(subname, 'unable to complete te rank', i )
       !
   ENDDO outer_column_loop
   !
   !
   IF ( mat_rank( 6, 6, work1, EPS_m6 ) /= 6 ) &
        CALL errore(subname,'work1 is a rank-deficient matrix II',3)

   !
   ! solve the 6 x 6 full-rank system of linear equations
   !
   lwork_aux = 2 * 6 * 6 
   ALLOCATE( work_aux(lwork_aux) )
   !
   CALL DGELS( 'N', 6, 6, 1, work1, 6, rhs, 6, work_aux, lwork_aux, info)
   !
   IF ( info < 0 )  CALL errore(subname,'invalid value in DGELS',-info)
   IF ( info > 0 )  CALL errore(subname,'DGELS: solving system', info)
   !
   DEALLOCATE( work_aux )
   !

   !
   ! We used to impose that weights must be positive...
   ! in principles there is no indication this should be the case
   ! as an experimental change, we try to relax the constraint
   !
   !   IF ( ANY( rhs(1:rank) < -EPS_m6 ) ) &
   !      CALL errore(subname,'negative weights',4)
    
   !
   ! found the non negligible weights, and complete the b, -b pairs
   !
   nb = 0 
   DO i=1, 6
      IF ( ABS(rhs(i)) > EPS_m6 ) nb = nb + 1
   ENDDO
   nb = 2 * nb


   !
   ! allocating b-vector data
   !
   CALL bshells_allocate()
   
   !
   ! get the b vects and complete the b, -b pairs
   !
   j = 0
   l = 0
   DO i = 1, 6
      !
      IF ( ABS(rhs(i)) > EPS_m6 ) THEN    
         !
         l = l + 1
         j = j + 1
         !
         wb(j)        = rhs(i)
         wb(j+nb/2)   = rhs(i)
         !
         vb(:,j)      =  vb_aux(:, i)
         vb(:,j+nb/2) = -vb_aux(:, i)
         !
         ! b vectors excluding -b
         nnpos(l)       =  j
         !
         ! once b is fixed, report the index of -b
         nnrev(j)      = j+nb/2
         nnrev(j+nb/2) = j
         !
      ENDIF
      !
   ENDDO
   !
   IF( j /= nb/2 ) CALL errore(subname,'j /= nb/2, unexpected',ABS(j)+1)

   !
   ! get the tsum of the weights
   !
   wbtot = SUM( wb(1:nb) )

!
!  now check that the completeness relation is satisfied
!  (see Appendix B, PRB 56 12847 (1997) for more details )
!
   DO i = 1, 3
   DO j = 1, 3
       !
       aux = ZERO
       DO ib = 1, nb
           aux = aux + wb(ib) * vb(i,ib) * vb(j,ib)
       ENDDO
 
       IF ( i == j ) aux = aux - ONE
       !
       IF ( ABS( aux ) > EPS_m6 ) THEN
           !
           CALL summary( stdout, INPUT=.FALSE.,    IONS=.FALSE.,     &
                                 WINDOWS=.FALSE.,  SYMMETRY=.FALSE., &
                                 KPOINTS=.TRUE.,   BSHELLS=.TRUE.,   &
                                 PSEUDO=.FALSE.    )
           CALL errore(subname, 'sum rule not satisfied', i+j )
           ! 
       ENDIF
       !   
   ENDDO
   ENDDO

!
! Now build up the list of b-vectors for each k-point.
!
! nnlist(ib,ik) points to the ib-th neighbour of the ik-th kpt
! nncell(1:3,ib,ik) tells us in which BZ is the ib-th bvect of ik-th kpt
!
! k- and b-vect in bohr^-1
!
   DO ik = 1, nkpts
   DO ib = 1, nb

       vkb(:) = vkpt(:,ik) + vb(:,ib)

       !
       ! now find who is k+b, and the cell in which it is set
       !
       found = .FALSE.
       !
       inner_kpt_loop :&
       DO ik1 = 1, nkpts
          DO l = -1, 1
          DO m = -1, 1
          DO n = -1, 1
             !
             vtmp(:) = vkpt(:,ik1) + l*bvec(:,1) + m*bvec(:,2) + n*bvec(:,3)
             !
             aux = DOT_PRODUCT( vtmp(:)-vkb(:), vtmp(:)-vkb(:) )  
             !      
             IF ( ABS(aux) < EPS_m6 ) THEN
                 !
                 ! the vector is found
                 !
                 found = .TRUE.
                 nnlist(ib, ik ) = ik1
                 nncell(1, ib, ik) = l
                 nncell(2, ib, ik) = m
                 nncell(3, ib, ik) = n
                 !
                 EXIT inner_kpt_loop
             ENDIF
          ENDDO
          ENDDO
          ENDDO
       ENDDO inner_kpt_loop
       !
       IF ( .NOT. found ) CALL errore(subname,'k+b not found',ib+ik)
       !
   ENDDO 
   ENDDO

   !
   ! local cleanup
   !
   DEALLOCATE( work, STAT=ierr )
   IF (ierr/=0 ) CALL errore(subname,'deallocating work ',ABS(ierr))
   DEALLOCATE( rhs, work1, STAT=ierr )
   IF (ierr/=0 ) CALL errore(subname,'deallocating rhs, work1',ABS(ierr))
   DEALLOCATE( vq, vqq, index,  STAT=ierr )
   IF (ierr/=0) CALL errore(subname,'deallocating vq--index',ABS(ierr))
   DEALLOCATE( vb_aux, ivb, STAT=ierr )
   IF (ierr/=0) CALL errore(subname,'deallocating vb_aux--ivb',ABS(ierr))

   !
   !
   CALL timing(subname,OPR='stop')
   CALL log_pop(subname)
   !
END SUBROUTINE bshells_init


