!
! Copyright (C) 2012 WanT Group
!
! This file is distributed under the terms of the
! GNU General Public License. See the file `License'
! in the root directory of the present distribution,
! or http://www.gnu.org/copyleft/gpl.txt .
!
!************************************************************
   SUBROUTINE translations_calc( ik_g, dimw1, dimw2, imin1, imin2, dimwinx, &
                                 evc, evc_info, igsort, nvect, rvect, transl )
   !************************************************************
   !
   USE kinds
   USE constants,      ONLY : CZERO, CONE
   USE timing_module,  ONLY : timing
   USE log_module,     ONLY : log_push, log_pop
   USE lattice_module, ONLY : tpiba
   USE ggrids_module,  ONLY : g, gamma_only
   USE wfc_info_module 
   !
   IMPLICIT NONE
      !
      ! ... Input Variables
      !
      TYPE(wfc_info), INTENT(in) :: evc_info
      COMPLEX(dbl),   INTENT(in) :: evc( evc_info%npwx, *)
      !
      INTEGER,        INTENT(in) :: ik_g
      INTEGER,        INTENT(in) :: dimw1, dimw2, dimwinx
      INTEGER,        INTENT(in) :: imin1, imin2
      INTEGER,        INTENT(in) :: igsort( evc_info%npwx )
      INTEGER,        INTENT(in) :: nvect
      REAL(dbl),      INTENT(in) :: rvect(3,nvect)
      COMPLEX(dbl),   INTENT(out):: transl( dimwinx, dimwinx, nvect )

      !
      ! ... Local Variables
      !
      INTEGER      :: ierr
      INTEGER      :: i, ig
      INTEGER      :: npwk, npwkx
      INTEGER      :: j1, ind1
      INTEGER      :: j2, ind2
      REAL(dbl)    :: arg
      COMPLEX(dbl) :: phase
      !
      COMPLEX(dbl), ALLOCATABLE :: aux1(:), aux2(:)
      COMPLEX(dbl),    EXTERNAL :: ZDOTC
      CHARACTER(17)             :: subname='translations_calc'
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
      ! the maximun number of PW for wfcs
      npwkx = evc_info%npwx
      !
      ind1 = wfc_info_getindex(imin1, ik_g, "SPSI_IK", evc_info)
      ind2 = wfc_info_getindex(imin2, ik_g, "IK_RIGHT", evc_info)
      !
      npwk = evc_info%npw(ind1)

      IF ( dimw1 > dimwinx ) CALL errore(subname, 'Invalid dimw1', dimw1)
      IF ( dimw2 > dimwinx ) CALL errore(subname, 'Invalid dimw2', dimw2)


      !
      ! local workspace
      !
      ALLOCATE( aux2(npwkx), STAT=ierr )
      IF (ierr/=0) CALL errore(subname,'allocating aux2',npwkx)


      !
      ! loop over translations
      !
      translations_loop: &
      DO i = 1, nvect
          
          !
          ! loops over bands
          !
          DO j2 = 1, dimw2
              !
              aux2(:) = CZERO
              ind2 = wfc_info_getindex(imin2 +j2 -1, ik_g, "IK_RIGHT", evc_info)
              !
              ! apply translation
              !
!$omp parallel do privae( arg, phase)
              DO ig = 1, npwk
                  !
                  arg   = DOT_PRODUCT( rvect(:,i), g( :, igsort(ig) ) ) * tpiba
                  phase = CMPLX( COS(arg), -SIN(arg), KIND=dbl )
                  !
                  aux2( ig ) = evc( ig, ind2) * phase
                  !
              ENDDO
!$omp end parallel do

              ! perform the scalar produce < S psi1 | T psi2 > 
              ! for all the bands in the selected energy window
              !
#ifdef __INTEL
              !
              ! workaround for a strange behavior (bug?) of the intel compiler
              !
              CALL ZGEMV ( 'C', npwkx, dimw1, CONE, evc(1:npwkx, ind1:ind1+dimw1-1 ), npwkx, &
                           aux2, 1, CZERO, transl(:, j2, i), 1 )
#else
              CALL ZGEMV ( 'C', npwkx, dimw1, CONE, evc(:, ind1:ind1+dimw1-1 ), npwkx, &
                           aux2, 1, CZERO, transl(:, j2, i), 1 )
#endif
              !
              IF ( gamma_only ) THEN
              !
!$omp parallel do private(ind1)
              DO j1 = 1, dimw1
                  !
                  transl(j1,j2,i) = transl(j1,j2,i) + CONJG( transl(j1,j2,i) ) &
                                  - evc(1, ind1+j1-1 ) * CONJG( aux2(1) )
              ENDDO
!$omp end parallel do
              !
          ENDIF
          !

          ENDDO
          !
      ENDDO translations_loop


      !
      ! local cleanup
      !
      DEALLOCATE( aux2, STAT=ierr)
      IF (ierr/=0) CALL errore(subname,'deallocating aux2',ABS(ierr))
      !
      CALL timing(subname,OPR='stop')
      CALL log_pop(subname)
      !
   END SUBROUTINE translations_calc

