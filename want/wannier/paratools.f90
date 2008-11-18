!
! Copyright (C) 2008 Andrea Ferretti
!
! This file is distributed under the terms of the
! GNU General Public License. See the file `License'
! in the root directory of the present distribution,
! or http://www.gnu.org/copyleft/gpl.txt .
!
!*********************************************
   MODULE paratools_module
   !*********************************************
   !
   USE kinds,          ONLY : dbl
   USE mp_global,      ONLY : nproc, mpime
   USE mp,             ONLY : mp_sum, mp_allgather
   USE timing_module,  ONLY : timing
   USE log_module,     ONLY : log_push, log_pop
   !
   IMPLICIT NONE
   PRIVATE
   !
   PUBLIC :: para_get_poolindex
   PUBLIC :: para_poolrecover
   !
CONTAINS
!
!*********************************************
   SUBROUTINE para_get_poolindex( iproc, ik_g, nkpts_g )
   !*********************************************
   !
   IMPLICIT NONE
   !
   INTEGER,  INTENT(OUT) :: iproc
   INTEGER,  INTENT(IN)  :: ik_g, nkpts_g
   !
   CHARACTER(18)  :: subname='para_get_poolindex'
   INTEGER        :: ip, lks(nproc), lke(nproc)

!
!----------------
! main body
!----------------
!
   !
   ! trivial case
   !
   IF ( nproc == 1 ) THEN 
       iproc = 0
       RETURN
   ENDIF


   CALL timing( subname, OPR="start")
   CALL log_push( subname )
   
   !
   ! some checks
   !
   IF ( nproc < 1 ) CALL errore(subname,'invalid nproc_',1)
   IF ( mpime < 0 ) CALL errore(subname,'invalid mpime',1)
   IF ( mpime >= nproc ) CALL errore(subname,'mpime too large',mpime)

   
   !
   ! main task
   !
   lks(:) = 0
   lke(:) = 0
   !
   CALL divide_et_impera( 1, nkpts_g, lks(mpime+1), lke(mpime+1), mpime, nproc )
   !
   CALL mp_sum( lks )
   CALL mp_sum( lke )
   !
   iproc = -1
   !
   DO ip = 1, nproc
       !
       IF ( ik_g >= lks(ip) .AND. ik_g <= lke(ip) ) iproc = ip-1
       !
   ENDDO
   !
   IF ( iproc < 0 ) CALL errore(subname,'unable to find index', 10 )
   !
   !
   CALL timing( subname, OPR="stop")
   CALL log_pop( subname )
   !
   RETURN
   !
END SUBROUTINE para_get_poolindex


!*********************************************
   SUBROUTINE para_poolrecover( data_l, data_g, iks, ike)
   !*********************************************
   !  
   IMPLICIT NONE
   !
   COMPLEX(dbl),   INTENT(IN)  :: data_l(:,:,:)
   COMPLEX(dbl),   INTENT(OUT) :: data_g(:,:,:)
   INTEGER,        INTENT(IN)  :: iks(:), ike(:)
   !
   CHARACTER(16)  :: subname='para_poolrecover'
   !
   CALL timing( subname, OPR="start")
   CALL log_push( subname )
   !
   data_g = 0.0
   !
   CALL timing( subname, OPR="stop")
   CALL log_pop( subname )
   !
   RETURN
   !
END SUBROUTINE para_poolrecover

END MODULE paratools_module

