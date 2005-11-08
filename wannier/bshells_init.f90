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
   USE constants, ONLY : ZERO, CZERO, ONE, TWO, EPS_m10, EPS_m6
   USE util_module, ONLY : mat_svd, mat_sv, mat_rank
   USE timing_module, ONLY : timing
   USE summary_module, ONLY: summary 
   USE io_module, ONLY: stdout

   USE parameters,     ONLY : nnx
   USE lattice_module, ONLY : bvec, lattice_alloc => alloc
   USE kpoints_module, ONLY : vkpt, nkpts, nb, vb, wb, wbtot, &
                              nnlist, nncell, nnrev, nnpos, &
                              kpoints_alloc, bshells_allocate 
   IMPLICIT NONE

   !
   ! local variables
   !

   REAL(dbl), PARAMETER :: eps = EPS_m6
 
   INTEGER   :: ik, ik1, ib, inum, ierr
   INTEGER   :: i, j, l, m, n
   INTEGER   :: nq, rank
   LOGICAL   :: eqv_found, found
   !
   REAL(dbl) :: aux, aux1, vkb(3), vtmp(3)
   REAL(dbl) :: work(6,6), work1(6,6), rhs(6)
   !
   INTEGER   :: lwork_aux, info, ipiv(6)
   INTEGER,   ALLOCATABLE :: ivb(:), index(:)
   REAL(dbl), ALLOCATABLE :: vq(:,:), vqq(:)
   REAL(dbl), ALLOCATABLE :: vb_aux(:,:), vbb(:)
   REAL(dbl), ALLOCATABLE :: work_aux(:)
   !
   ! end of declariations
   !

!
!------------------------------
! main body
!------------------------------
!
   CALL timing('bshells_init',OPR='start')

   IF ( .NOT. lattice_alloc ) CALL errore('bshells_init', 'lattice NOT alloc', 1 )
   IF ( .NOT. kpoints_alloc ) CALL errore('bshells_init', 'kpoints NOT alloc', 1 )
   IF ( nkpts <= 0) CALL errore('bshells_init', 'Invaid nkpts', ABS(nkpts)+1 )


!
! First, find the distance between k-point 1 and its nearest-neighbour shells
! if we have only one k-point, the n-neighbours are its periodic images 
!

   !
   ! allocate the trial kpoints, searching for the shells
   ! 125 = 5 **3 
   !
   nq = 125*nkpts

   ALLOCATE( vq(3,nq), vqq(nq), index(nq),  STAT=ierr )
     IF (ierr/=0) CALL errore('bshells_init','allocating vq--index',ABS(ierr))

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
! Now we deternine the possible b-vectors, adding non-proportional vectors
! in order of increasing module, up to a max number of nnx = 12.
! Eventual useless b vects will be deleted after weight calculation if 
! zero-weight is found.
!
   ALLOCATE( vb_aux(3,nnx), vbb(nnx), ivb(nnx), STAT=ierr )
      IF (ierr/=0) CALL errore('bshells_init','allocating vb_aux, vbb, ivb ',ABS(ierr))

   !
   ! first found the candidates:
   ! search for 6 vectors not parallel to each other
   ! always include the 3 vectors parallel to the generators of 
   ! the reciprocal lattice
   !

   inum = 0
   !
   ! found the 3 vectors parallel to the reciprocal lattice gen.
   !
   DO j = 1, 3

      ! suqared norm of the generators
      aux1 = DOT_PRODUCT( bvec(:,j), bvec(:,j) ) 

      found = .FALSE.
      DO i = 2, nq
          !
          aux = DOT_PRODUCT( bvec(:,j),vq(:,index(i)) ) /  &
                    SQRT( aux1 * vqq(i) )
          aux = ABS(aux)
          !
          IF ( ABS( aux - ONE ) < EPS_m6 ) THEN
             found = .TRUE.
             !
             inum = inum +1
             ivb(inum)  = index(i)
             vb_aux(:,inum) = vq(:, ivb(inum) )
             vbb(inum)  = vqq( i )
             !
             EXIT
          ENDIF 
      ENDDO
      !
      IF ( .NOT. found ) CALL errore('bshells_init','vb parallel to bvec not found',j)
   ENDDO
    
   !
   ! now search for the reamining vectors
   !
   DO i = 3, nq      
       eqv_found = .FALSE.
       DO j = 1, inum 
          !
          ! check whether the new vector is parallel to anyone
          ! already found, compute the dir cosine
          !
          aux = DOT_PRODUCT( vb_aux(:,j), vq(:,index(i)) ) /  &
                    SQRT( vbb(j) * vqq(i) )
          aux = ABS(aux)
          !
          IF ( ABS( aux - ONE ) < EPS_m6 ) THEN 
             eqv_found = .TRUE.
             EXIT
          ENDIF
       ENDDO
       IF ( .NOT. eqv_found )  THEN
          inum = inum +1
          ivb(inum)  = index(i)
          vb_aux(:,inum) = vq(:, ivb(inum) )
          vbb(inum)  = vqq( i )
       ENDIF
       IF ( inum == 6 ) EXIT
   ENDDO
   !
   DEALLOCATE( ivb, vbb, vq, vqq, index, STAT=ierr )
      IF (ierr/=0) CALL errore('bshells_init','deallocating ivb--index ',ABS(ierr))

   !
   ! then impose the sum rule to find the weights
   ! according to App.B in PRB 56, 12847 (1997)
   ! SumRule:  \sum_b wb |b >< b| = I  
   ! which are 6 eqs in wb, 
   !
   DO i=1,6   
      work( 1, i ) = vb_aux( 1, i) * vb_aux( 1, i )
      work( 2, i ) = vb_aux( 1, i) * vb_aux( 2, i )
      work( 3, i ) = vb_aux( 1, i) * vb_aux( 3, i )
      work( 4, i ) = vb_aux( 2, i) * vb_aux( 2, i )
      work( 5, i ) = vb_aux( 2, i) * vb_aux( 3, i )
      work( 6, i ) = vb_aux( 3, i) * vb_aux( 3, i )
   ENDDO
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
   rhs = rhs / TWO

   !
   ! determine the number of b-vectors, which is the rank
   ! of the matrix, and get a full rank submatrix (work1) from work 
   ! starting from the first column (smallest b-vector) we add
   ! columns up to the work rank is reached
   !
   rank =  mat_rank( 6, 6, work, EPS_m6 )
   !
   work1(:,:) = ZERO
   work1(:,1) = work(:,1) 
   ipiv(1) = 1
   !
   ! this is a counter on the work columns
   j = 2
   !
   outer_column_loop: &
   DO i = 2, rank
      !
      inner_column_loop: &
      DO l = j,6
         work1(:,i) = work(:,l)
         IF ( mat_rank( 6, i, work1, EPS_m6 ) == i ) THEN
            ipiv(i) = j
            j = j+1
            EXIT inner_column_loop
         ENDIF
         !
      ENDDO inner_column_loop
   ENDDO outer_column_loop

   IF ( mat_rank( 6, rank, work1, EPS_m6 ) /= rank ) &
        CALL errore('bshells_init','work still a rank-deficient matrix',3)

   !
   ! solve the 6 x rank overdetermined system of linear equations
   lwork_aux = 2 * rank
   ALLOCATE( work_aux(lwork_aux) )
   !
   CALL DGELS( 'N', 6, rank, 1, work1, 6, rhs, 6, work_aux, lwork_aux, info)
   !
   IF ( info < 0 )  CALL errore('bshells_init','invalid value in DGELS',-info)
   DEALLOCATE( work_aux )
   !

   IF ( ANY( rhs(1:rank) < -EPS_m6 ) ) &
      CALL errore('bshells_init','negative weights',4)
    
   !
   ! found the non negligible weights, and complete the b, -b pairs
   !
   nb = 0 
   DO i=1, rank
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
   DO i=1,rank
      IF ( ABS(rhs(i)) > EPS_m6 ) THEN    
         !
         l = l + 1
         j = j + 2
         !
         wb(j-1) = rhs(i)
         wb(j)   = rhs(i)
         !
         vb(:,j-1) =  vb_aux(:,ipiv(i))
         vb(:,j)   = -vb_aux(:,ipiv(i))
         !
         ! b vectors excluding -b
         nnpos(l)  =  j-1
         !
         ! once b is fixed, report the index of -b
         nnrev(j-1) = j 
         nnrev(j) = j-1 
         !
      ENDIF
   ENDDO
   IF( j/=nb ) CALL errore('bshells_init','j /= nb, unexpected',ABS(j)+1)
   !
   DEALLOCATE( vb_aux, STAT=ierr)
      IF (ierr/=0) CALL errore('bshells_init','deallocating vb_aux',ABS(ierr))

   !
   ! get the tsum of the weights
   !
   wbtot = SUM( wb(1:nb) )

!
!  now check that the completeness relation is satisfied
!  (see Appendix B, PRB 56 12847 (1997) for more details )
!
!  Even if this is not necessary, we check it again for safety
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
      IF ( ABS( aux ) > EPS_m6 ) &
        CALL errore('bshells_init', 'sum rule not satisfied', i+j )
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
       IF ( .NOT. found ) CALL errore('bshells_init','k+b not found',ib+ik)
       !
   ENDDO 
   ENDDO

   CALL timing('bshells_init',OPR='stop')
END SUBROUTINE bshells_init


