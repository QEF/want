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
   USE kinds, ONLY : dbl
   USE constants, ONLY : CZERO
   USE timing_module, ONLY : timing
   USE wfc_info_module
   USE trial_center_module, ONLY : trial_center, trial_center_setup

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
   INTEGER :: iwann 
   INTEGER :: ib, ig, ind
   INTEGER :: ierr
   COMPLEX(dbl), ALLOCATABLE :: trial_vect(:)


!
! ...  End of declaration
!


      CALL timing('projection',OPR='start')

      ind = wfc_info_getindex(imin, ik, "SPSI_IK", evc_info)
      npwk = evc_info%npw(ind)

      ALLOCATE( trial_vect(npwk), STAT = ierr )
        IF( ierr /= 0 ) CALL errore( 'projection', 'allocating gauss ', npwk )

      !
      ! ... wannier trials
      DO iwann = 1, dimwann
          !
          ! set the trial centers in PW represent.
          CALL trial_center_setup(ik, trial(iwann), npwk, trial_vect)

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
           IF (ierr/=0) CALL errore(' projection ',' deallocating gauss',ABS(ierr))

       CALL timing('projection',OPR='stop')

   RETURN
   END SUBROUTINE projection

