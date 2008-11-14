!
! Copyright (C) 2005 WanT Group
!
! This file is distributed under the terms of the
! GNU General Public License. See the file `License'
! in the root directory of the present distribution,
! or http://www.gnu.org/copyleft/gpl.txt .
!
!********************************************************
   SUBROUTINE collect_wf( dimwann, nkpts, rave, xcell, cU )
   !********************************************************
   !
   ! this subroutine moves the actual WFs in the cell specified
   ! by xcell. The translation is performed by changing some pahses
   ! in the U(k) matrices (cu in the routine).
   !
   USE kinds
   USE constants,         ONLY : ZERO, CZERO, ONE, TPI
   USE timing_module,     ONLY : timing
   USE log_module,        ONLY : log_push, log_pop
   USE converters_module, ONLY : cart2cry, cry2cart
   USE lattice_module,    ONLY : avec, bvec
   USE kpoints_module,    ONLY : vkpt_g, nkpts_g, iks, ike
   USE mp,                ONLY : mp_sum
   !
   IMPLICIT NONE

   !
   ! input variables
   !
   INTEGER,      INTENT(in)    :: dimwann, nkpts     ! dimensions
   REAL(dbl),    INTENT(inout) :: rave(3,dimwann)    ! the WF centers
   REAL(dbl),    INTENT(in)    :: xcell(3)           ! the corner of the selected cell, 
                                                     ! cryst units
   COMPLEX(dbl), INTENT(inout) :: cU(dimwann,dimwann,nkpts_g)

   !
   ! local variables
   !
   CHARACTER(10):: subname='collect_wf'
   INTEGER      :: i, j, ik, ik_g, m, n, ierr
   REAL(dbl)    :: arg
   COMPLEX(dbl) :: phase
   REAL(dbl), ALLOCATABLE :: vkpt_cry(:,:), rave_cry(:,:)
   REAL(dbl), ALLOCATABLE :: rave_new(:,:)

   !
   ! end of declariations
   !

!
!------------------------------
! main body 
!------------------------------
!
      CALL timing(subname,OPR='start')
      CALL log_push(subname)

      !
      ! aux data
      !
      ALLOCATE( vkpt_cry(3, nkpts), rave_cry(3, dimwann), STAT=ierr )
      IF (ierr/=0) CALL errore(subname, 'allocating vkpt_cry, rave_cry', ABS(ierr))
      !
      ALLOCATE( rave_new(3, dimwann), STAT=ierr )
      IF (ierr/=0) CALL errore(subname, 'allocating rave_new', ABS(ierr))

      !
      ! convert vkpt from cart coord (bohr^-1) to cryst
      ! the same for rave, from cart coord (bohr) to cryst
      !
      vkpt_cry(:,:) = vkpt_g(:,iks:ike)
      rave_cry(:,:) = rave(:,:)
      CALL cart2cry( vkpt_cry, bvec )
      CALL cart2cry( rave_cry, avec)

      !
      ! determine the shift to rave to set the WF in the selected cell
      ! the edge of this cell is given by xcell
      !
      ! as a convention: rave_new = rave + rave_shift
      !
      rave_new(:,:) = ZERO
      !
      DO m=1,dimwann
         !
         DO i=1,3
            rave_new(i,m) = MODULO( rave_cry(i,m) -xcell(i), ONE ) + xcell(i)
         ENDDO
         !
      ENDDO


      !   
      ! nullify cU for ik_g not in the current pool
      ! needed to use mp_sum for pool recovering
      !   
      cU( :,:, 1:iks-1 )        = CZERO
      cU( :,:, ike+1:nkpts_g )  = CZERO

      !
      ! apply the required shift to the bloch functions:
      ! this is done changing the phases in cU
      !
      ! | WF_mk > = \sum_n cU_nm | nk > 
      !
      !
      DO ik = 1, nkpts
      DO m  = 1, dimwann
          !
          ik_g = ik + iks -1

          !
          ! compute the phase for the global shift
          !
          arg = TPI * DOT_PRODUCT( vkpt_cry(:,ik), rave_new(:,m) - rave(:,m) )
          phase = CMPLX( COS(arg), -SIN(arg), dbl )

          !
          ! apply the shift
          !
          DO n = 1, dimwann
              !
              cU(n,m,ik_g) = cU(n,m,ik_g) * phase 
              !
          ENDDO
          !
      ENDDO
      ENDDO
      !
      ! pool recovering
      !
      CALL timing ( 'mp_sum', OPR='start' )
      DO ik_g = 1, nkpts_g 
          !
          CALL mp_sum( cU(:,:,ik_g) )
          !
      ENDDO
      CALL timing ( 'mp_sum', OPR='stop' )

      !
      ! output updated centers
      !
      CALL cry2cart( rave_new, avec )
      rave(:,:) = rave_new(:,:)

      !
      ! cleanup
      !
      DEALLOCATE( vkpt_cry, rave_cry, STAT=ierr )
      IF (ierr/=0) CALL errore(subname, 'deallocating vkpt_cry, rave_cry', ABS(ierr))
      !
      DEALLOCATE( rave_new, STAT=ierr )
      IF (ierr/=0) CALL errore(subname, 'deallocating rave_new', ABS(ierr))

      CALL timing(subname,OPR='stop')
      CALL log_pop(subname)
      !
   END SUBROUTINE collect_wf

