!
! Copyright (C) 2001-2003 PWSCF group
! This file is distributed under the terms of the
! GNU General Public License. See the file `License'
! in the root directory of the present distribution,
! or http://www.gnu.org/copyleft/gpl.txt .
!
!----------------------------------------------------------------------------
SUBROUTINE s_psi( lda, n, m, ik, psi, spsi, ldb, becp, nstep_kb, ibetas, ibetae)
  !----------------------------------------------------------------------------
  !
  !    This routine applies the S matrix to m wavefunctions psi
  !    and puts the results in spsi.
  !    Requires the products of psi with all beta functions
  !    in array becp(nkb,m) (calculated in h_psi or by ccalbec)
  !
  ! input:
  !     lda      leading dimension of arrays psi, spsi
  !     n        true dimension of psi, spsi
  !     m        number of states psi
  !     ik       index of the current kpt
  !     psi
  !     ldb      leading dimension of array becp
  !     becp     see above, < beta_i | psi_j >
  !     nstep    # of steps of buffering for vkb
  !     ibetas
  !     ibetae   starting and ending idexes for each step
  !
  ! output:
  !     spsi  S*psi
  !
  USE kinds,           ONLY : dbl
  USE constants,       ONLY : ZERO, CZERO
  USE us_module,       ONLY : okvan
  USE uspp,            ONLY : nkb, vkb, qq
  USE uspp_param,      ONLY : upf, nh
  USE ions_module,     ONLY : nat, nsp, ityp
  USE lattice_module,  ONLY : tpiba
  USE kpoints_module,  ONLY : vkpt_g
  USE wfc_data_module, ONLY : igsort
  USE log_module,      ONLY : log_push, log_pop
  USE timing_module
  !
  IMPLICIT NONE
  !
  ! I/O variables
  !
  INTEGER,           INTENT(in) :: lda, n, m, ik, ldb
  INTEGER,           INTENT(in) :: nstep_kb
  INTEGER,           INTENT(in) :: ibetas(*), ibetae(*)
  COMPLEX(KIND=dbl), INTENT(in) :: psi(lda,m)
  COMPLEX(KIND=dbl), INTENT(in) :: becp(ldb,m)
  COMPLEX(KIND=dbl), INTENT(out):: spsi(lda,m)
  ! 
  !
  CALL timing( 's_psi', OPR='start' )  
  CALL log_push( 's_psi' )  
     !
     CALL s_psi_k()
     !
  !
  CALL timing( 's_psi',OPR='stop' )
  CALL log_pop( 's_psi' )  
  !
  RETURN
  !
  CONTAINS
     !
     !-----------------------------------------------------------------------
     SUBROUTINE s_psi_k()
       !-----------------------------------------------------------------------
       !
       ! ... k-points version
       !
       IMPLICIT NONE
       !
       ! ... local variables
       !
       INTEGER   :: ikb, jkb, ih, jh, na, nt, ijkb0, ibnd
       INTEGER   :: ks, kbs, kbe, ldc, ierr
       REAL(dbl) :: xk(3)
       !
       ! counters
       COMPLEX(KIND=dbl), ALLOCATABLE :: ps(:,:), caux(:,:)
       ! the product vkb and psi
       !
       !
       ! ... initialize  spsi
       !
       CALL ZCOPY( lda * m, psi, 1, spsi, 1 )
       !
       ! ... The product with the beta functions
       !
       IF ( nkb == 0 .OR. .NOT. okvan ) RETURN
       IF ( ldb < nkb ) CALL errore('s_psi','unexpected dimensions', 10)
       !
       !
       ldc = MAXVAL( ibetae(1:nstep_kb)-ibetas(1:nstep_kb) +1 )
       !
       ALLOCATE( ps( nkb, m ), STAT=ierr )    
       IF ( ierr/=0 ) CALL errore('s_psi', 'allocating ps', ABS(ierr))
       ALLOCATE( caux( ldc, m ), STAT=ierr )    
       IF ( ierr/=0 ) CALL errore('s_psi', 'allocating caux', ABS(ierr))
       !
       ps(:,:) = CZERO
       caux(:,:) = CZERO
       !
       ijkb0 = 0
       DO nt = 1, nsp
          IF ( upf(nt)%tvanp ) THEN
             DO na = 1, nat
                IF ( ityp(na) == nt ) THEN
                   !
                   DO ibnd = 1, m
                      DO jh = 1, nh(nt)
                         jkb = ijkb0 + jh
                         DO ih = 1, nh(nt)
                            ikb = ijkb0 + ih
                            ps(ikb,ibnd) = ps(ikb,ibnd) + &
                                           qq(ih,jh,nt) * becp(jkb,ibnd)
                         END DO
                      END DO
                   END DO
                   !
                   ijkb0 = ijkb0 + nh(nt)
                END IF
             END DO
          ELSE
             DO na = 1, nat
                IF ( ityp(na) == nt ) ijkb0 = ijkb0 + nh(nt)
             END DO
          END IF
       END DO
       !
       ! the following loop is due to the bufferization of vkb
       !
       DO ks = 1, nstep_kb
           !
           kbs = ibetas( ks )
           kbe = ibetae( ks )
           !
           xk(:) = vkpt_g(:,ik) / tpiba
           CALL init_us_2( n, igsort(:,ik), xk, kbs, kbe, vkb )
           !
           caux(1:kbe-kbs+1, 1:m) = ps( kbs:kbe, 1:m ) 
           !
           CALL ZGEMM( 'N', 'N', n, m, kbe-kbs+1, (1.D0, 0.D0), vkb, &
                       lda, caux, ldc, (1.D0, 0.D0), spsi, lda )
       ENDDO
       !
       DEALLOCATE( ps, STAT=ierr )
       IF ( ierr/=0 ) CALL errore('s_psi', 'deallocating ps', ABS(ierr))
       DEALLOCATE( caux, STAT=ierr )
       IF ( ierr/=0 ) CALL errore('s_psi', 'deallocating caux', ABS(ierr))
       !
       RETURN
       !
     END SUBROUTINE s_psi_k     
     !
END subroutine s_psi
