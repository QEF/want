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
   SAVE
   !
   ! data for internal use
   !
   INTEGER, ALLOCATABLE   ::  iks_g(:)
   INTEGER, ALLOCATABLE   ::  ike_g(:)
   LOGICAL                ::  alloc = .FALSE.
   !
   !
   INTERFACE para_poolrecover
      MODULE PROCEDURE para_poolrecover_cm
   END INTERFACE
  
   !
   PUBLIC :: para_get_poolindex
   PUBLIC :: para_poolrecover
   PUBLIC :: alloc
   !
CONTAINS
!
!*********************************************
   SUBROUTINE para_init( nkpts_g )
   !*********************************************
   !
   IMPLICIT NONE
   !
   INTEGER,  INTENT(IN)  :: nkpts_g
   !
   CHARACTER(9)  :: subname='para_init'
   INTEGER       :: ip, ierr
   !
   !
   IF ( alloc ) RETURN
   IF ( nkpts_g < 1 ) CALL errore(subname,'invalid nkpts_g',1)
   !
   ALLOCATE( iks_g(nproc), STAT=ierr )
   IF ( ierr/=0 ) CALL errore(subname,'allocating iks_g',ABS(ierr))
   ALLOCATE( ike_g(nproc), STAT=ierr )
   IF ( ierr/=0 ) CALL errore(subname,'allocating ike_g',ABS(ierr))
   !
   iks_g(:) = 0
   ike_g(:) = 0
   !
   DO ip = 0, nproc-1
       !
       CALL divide_et_impera( 1, nkpts_g, iks_g(ip+1), ike_g(ip+1), ip, nproc )
       !
   ENDDO
   !
   alloc = .TRUE.
   !
   RETURN
   !
END SUBROUTINE para_init
!
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
   INTEGER        :: ip

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
   IF ( nproc < 1 )      CALL errore(subname,'invalid nproc_',1)
   IF ( nkpts_g < 1 )    CALL errore(subname,'invalid nkpts_g',1)
   IF ( mpime < 0 )      CALL errore(subname,'invalid mpime',1)
   IF ( mpime >= nproc ) CALL errore(subname,'mpime too large',mpime)

   
   !
   ! main task
   !
   IF ( .NOT. alloc ) CALL para_init( nkpts_g )

   !
   iproc = -1
   !
   DO ip = 1, nproc
       !
       IF ( ik_g >= iks_g(ip) .AND. ik_g <= ike_g(ip) ) iproc = ip-1
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
   SUBROUTINE para_poolrecover_cm( data_l, data_g )
   !*********************************************
   !  
   IMPLICIT NONE
   !
   COMPLEX(dbl),   INTENT(IN)    :: data_l(:,:,:)
   COMPLEX(dbl),   INTENT(INOUT) :: data_g(:,:,:)
   !
   CHARACTER(16)        :: subname='para_poolrecover'
   INTEGER, ALLOCATABLE :: displs(:)
   INTEGER              :: nkpts_g, ip, ierr


   !
   CALL timing( subname, OPR="start")
   CALL log_push( subname )
   !
   !
   nkpts_g = SIZE( data_g, 3)
   IF ( .NOT. alloc ) CALL para_init( nkpts_g )
   !
   ALLOCATE( displs( nkpts_g ), STAT=ierr )
   IF ( ierr/=0 ) CALL errore(subname,'allocating displs', ABS(ierr))
   !
   displs(1) = 1
   DO ip = 2, nproc
       !
       displs(ip) = SIZE( data_g, 1) * SIZE(data_g, 2) * ike_g(ip-1) +1
       !
   ENDDO
   !
   CALL mp_allgather( data_l, data_g, displs )
   !
   DEALLOCATE( displs, STAT=ierr )
   IF ( ierr/=0 ) CALL errore(subname,'allocating displs', ABS(ierr))
   !
   CALL timing( subname, OPR="stop")
   CALL log_pop( subname )
   !
   RETURN
   !
END SUBROUTINE para_poolrecover_cm

END MODULE paratools_module

