!
! Copyright (C) 2013 WanT Group
!
! This file is distributed under the terms of the
! GNU General Public License. See the file `License'
! in the root directory of the present distribution,
! or http://www.gnu.org/copyleft/gpl.txt .
!
!************************************************************
   SUBROUTINE translations_calc_atmproj( rvect, dimwann, ndimx, rovp, transl )
   !************************************************************
   !
   ! This routines exploit the fact that atomic orbitals from QE
   ! are ordered atom by atom
   !
   !
   USE kinds
   USE constants,              ONLY : CZERO, EPS_m6, EPS_m8
   USE timing_module,          ONLY : timing
   USE log_module,             ONLY : log_push, log_pop
   USE converters_module,      ONLY : cart2cry
   USE ions_module,            ONLY : ions_alloc => alloc, nat, nsp, tau, ityp, symb
   USE lattice_module,         ONLY : lattice_alloc => alloc, avec, alat
   USE kpoints_module,         ONLY : kpoints_alloc, nkpts_g
   USE uspp_param,             ONLY : upf
   USE atmproj_tools_module,   ONLY : atmproj_get_index, atmproj_get_natomwfc
   !
   IMPLICIT NONE
      !
      ! ... Input Variables
      !
      INTEGER,        INTENT(IN) :: dimwann, ndimx
      REAL(dbl),      INTENT(IN) :: rvect(3)             ! cartesian, bohr units
      COMPLEX(dbl),   INTENT(IN) :: rovp( ndimx, ndimx )
      COMPLEX(dbl),   INTENT(OUT):: transl( ndimx, ndimx )

      !
      ! ... Local Variables
      !
      CHARACTER(25)  :: subname='translations_calc_atmproj'
      !
      INTEGER        :: ierr
      INTEGER        :: i, j, ia, ib, ic
      INTEGER        :: ia_trasl, ma, mb, ma_tr
      INTEGER        :: nt, nb
      LOGICAL        :: lfound
      !
      REAL(dbl)      :: raux
      REAL(dbl)      :: rvect_cry(3), vaux(3)
      !
      REAL(dbl), ALLOCATABLE :: tau_cry(:,:)
      INTEGER,   ALLOCATABLE :: natomwfc(:)
      !
      ! ... end declarations
      !

!
!------------------------------
! main body
!------------------------------
!
      CALL timing(subname,OPR='start')
      CALL log_push(subname)

      !
      ! checks
      !
      IF ( .NOT. ions_alloc )     CALL errore(subname,"ions module not alloc", 10)
      IF ( .NOT. kpoints_alloc )  CALL errore(subname,"kpoints module not alloc", 10)
      IF ( .NOT. lattice_alloc )  CALL errore(subname,"lattice module not alloc", 10)
      !
      IF ( nkpts_g /= 1 ) CALL errore(subname,"nkpts > 1 not implemented", 10)


      !
      ! take atomic coords to crystal units
      !
      ALLOCATE( tau_cry(3,nat), STAT=ierr )
      IF ( ierr/=0 ) CALL errore(subname,"allocating tau_cry",ABS(ierr))
      !
      tau_cry = tau * alat
      CALL cart2cry( tau_cry, avec )
      !
      ! shift all positions in the interval [0,1]
      tau_cry(:,:) = MOD( tau_cry(:,:), 1.0d0 )
      DO ia = 1, nat
      DO i  = 1, 3
          IF ( tau_cry(i,ia) < 0.0d0 ) tau_cry(i,ia) = tau_cry(i,ia) + 1.0d0
      ENDDO
      ENDDO
      !
      rvect_cry(:) = rvect(:)
      CALL cart2cry( rvect_cry, avec )


      !
      ! build other atomic properties
      !
      ALLOCATE( natomwfc(nsp), STAT=ierr )
      IF ( ierr/=0 ) CALL errore(subname,"allocating natomwfc",ABS(ierr))
      !
      CALL atmproj_get_natomwfc( nsp, upf(1:nsp), natomwfc )
 

      !
      ! main loop
      !
      transl(:,:) = CZERO
      !
      DO ia = 1, nat
          !
          ! determine the position of the translated atom
          ! all positions are shifted by tau(:,1)
          ! this works only for gamma sampling
          !
          vaux(:) = MOD( tau_cry(:,ia) + rvect_cry(:) + EPS_m6, 1.0d0 )
          DO i = 1, 3
              IF ( vaux(i) < 0.0d0 ) vaux(i) = vaux(i) + 1.0d0
          ENDDO
          vaux(:) = vaux(:) -EPS_m6

          !
          lfound = .FALSE.
          ia_trasl = 0
          !
          loop_search: &
          DO ic = 1, nat
              !
              raux = DOT_PRODUCT( vaux-tau_cry(:,ic), vaux-tau_cry(:,ic) )
              IF ( raux < EPS_m8 .AND. ityp(ia) == ityp(ic) ) THEN
                  !
                  ia_trasl = ic
                  lfound = .TRUE.
                  EXIT loop_search
                  !
              ENDIF
              !
          ENDDO loop_search
          !
          IF ( .NOT. lfound ) CALL errore(subname,"pairing atom not found", ia)


          DO ib = 1, nat
              !
              DO j = 1, natomwfc( ityp(ia) )
              DO i = 1, natomwfc( ityp(ib) )
                  !
                  ma    = atmproj_get_index( j, ia, ityp, natomwfc )
                  ma_tr = atmproj_get_index( j, ia_trasl, ityp, natomwfc )
                  mb    = atmproj_get_index( i, ib, ityp, natomwfc )
                  !
                  transl(mb,ma) = rovp(mb,ma_tr)
                  !
              ENDDO
              ENDDO
              !
          ENDDO
          !
      ENDDO

      !
      ! cleanup
      !
      DEALLOCATE( tau_cry, STAT=ierr )
      IF ( ierr/=0 ) CALL errore(subname,"deallocating tau_cry",ABS(ierr))
      DEALLOCATE( natomwfc, STAT=ierr )
      IF ( ierr/=0 ) CALL errore(subname,"deallocating natomwfc",ABS(ierr))
               
      !
      CALL timing(subname,OPR='stop')
      CALL log_pop(subname)
      !
   END SUBROUTINE translations_calc_atmproj


