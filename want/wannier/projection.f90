!
! Copyright (C) 2005 WanT Group
!
! This file is distributed under the terms of the
! GNU General Public License. See the file `License'
! in the root directory of the present distribution,
! or http://www.gnu.org/copyleft/gpl.txt .
!
!*******************************************************************
   SUBROUTINE projection( ik, dimw, imin, dimwinx, evc, evc_info, dimwann, trial, ca)
   !*******************************************************************
   !
   ! ...  Calculate the projection of the gaussians on the bloch eigenstates inside 
   !      energy window: store it in dimwin(ik) X dimwann overlap matrix CA
   !
   !      CA(iwann,ib,ik) = < ib, ik | iwann >
   !
   !      The scalr product is directly done in reciprocal space, providing an
   !      analytical form for the FT of the gaussian orbitals.
   !
   USE kinds,           ONLY : dbl
   USE constants,       ONLY : CZERO
   USE timing_module,   ONLY : timing
   USE sph_har_module,  ONLY : sph_har_index 
   USE trial_center_module, ONLY : trial_center, trial_center_setup

   USE lattice_module,  ONLY : tpiba
   USE kpoints_module,  ONLY : vkpt
   USE wfc_info_module
   USE wfc_data_module, ONLY : igsort
   USE ggrids_module,   ONLY : g

   IMPLICIT NONE

   ! ... arguments
   TYPE(wfc_info), INTENT(in) :: evc_info
   COMPLEX(dbl),   INTENT(in) :: evc( evc_info%npwx, evc_info%nwfc )

   INTEGER,  INTENT(in) :: ik, dimw, imin
   INTEGER,  INTENT(in) :: dimwinx
   INTEGER,  INTENT(in) :: dimwann
   TYPE(trial_center), INTENT(in) :: trial(dimwann)
   COMPLEX(dbl),    INTENT(inout) :: ca(dimwinx,dimwann)

   ! ... local variables

   INTEGER :: npwk
   INTEGER :: lmax
   INTEGER :: iwann, ib, ig, ind 
   INTEGER :: ierr
   INTEGER,      ALLOCATABLE :: ylm_info(:,:)
   REAL(dbl),    ALLOCATABLE :: ylm(:,:), vkg(:,:), vkgg(:)
   COMPLEX(dbl), ALLOCATABLE :: trial_vect(:)


!
! ...  End of declaration
!


      CALL timing('projection',OPR='start')

      ind = wfc_info_getindex(imin, ik, "SPSI_IK", evc_info)
      npwk = evc_info%npw(ind)

      ALLOCATE( trial_vect(npwk), STAT = ierr )
        IF( ierr /= 0 ) CALL errore( 'projection', 'allocating trial_vect', npwk )

      !
      ! set the maximum l for the spherical harmonics
      ! Here we use ABS( l ) because l = -1 is used for sp3
      ! hybrid orbitals which are combinations of s and p Y_lm
      !
      lmax = 0
      DO iwann=1,dimwann
         lmax = MAX( lmax, ABS( trial(iwann)%l )  )
      ENDDO

      ALLOCATE( ylm_info(-lmax:lmax, 0:lmax ), STAT=ierr )
        IF( ierr /= 0 ) CALL errore( 'projection', 'allocating ylm_info', ABS(ierr) )
      ALLOCATE( vkg(3,npwk), STAT=ierr )
        IF( ierr /= 0 ) CALL errore( 'projection', 'allocating vkg', ABS(ierr) )
      ALLOCATE( vkgg(npwk), STAT=ierr )
        IF( ierr /= 0 ) CALL errore( 'projection', 'allocating vkgg', ABS(ierr) )
      ALLOCATE( ylm(npwk,(lmax+1)**2), STAT=ierr )
        IF( ierr /= 0 ) CALL errore( 'projection', 'allocating ylm', ABS(ierr) )

      !
      ! compute the needed spherical harmonics
      ! vkg in bohr^-1
      !
      DO ig = 1, npwk
          vkg(:,ig) = - ( vkpt(:,ik) + g(:, igsort(ig,ik))*tpiba )  
          vkgg(ig)  = DOT_PRODUCT( vkg(:,ig) , vkg(:,ig) ) 
      ENDDO
      CALL ylmr2( (lmax+1)**2, npwk, vkg, vkgg, ylm ) 
      CALL sph_har_index(lmax, ylm_info)


      !
      ! ... wannier trials
      DO iwann = 1, dimwann
          !
          ! set the trial centers in PW represent.
          CALL trial_center_setup(ik, npwk, vkgg, lmax, ylm, ylm_info, &
                                  trial(iwann), trial_vect)

          !
          ! ... bands 
          DO ib = 1, dimw

             ind = wfc_info_getindex(imin +ib -1, ik, "SPSI_IK", evc_info)

             ca(ib,iwann) = CZERO    
             DO ig = 1, npwk
                 ca(ib,iwann) = ca(ib,iwann) +  &
                        CONJG( evc(ig,ind) ) * trial_vect(ig)
             ENDDO

          ENDDO 
      ENDDO    

      !
      ! local cleaning
      ! 
      DEALLOCATE( trial_vect, STAT=ierr )
          IF (ierr/=0) CALL errore('projection','deallocating trial_vect',ABS(ierr))
      DEALLOCATE( ylm_info, STAT=ierr )
          IF (ierr/=0) CALL errore('projection','deallocating ylm_info',ABS(ierr))
      DEALLOCATE( vkg, STAT=ierr )
          IF (ierr/=0) CALL errore('projection','deallocating vkg', ABS(ierr) )
      DEALLOCATE( vkgg, STAT=ierr )
          IF (ierr/=0) CALL errore('projection','deallocating vkgg', ABS(ierr) )
      DEALLOCATE( ylm, STAT=ierr )
          IF (ierr/=0) CALL errore('projection','deallocating ylm', ABS(ierr) )

      CALL timing('projection',OPR='stop')

   RETURN
   END SUBROUTINE projection

