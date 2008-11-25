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
   INTEGER, ALLOCATABLE   ::  displs_aux(:)
   INTEGER, ALLOCATABLE   ::  msglen_aux(:)
   INTEGER, ALLOCATABLE   ::  displs(:)
   INTEGER, ALLOCATABLE   ::  msglen(:)
   !
   LOGICAL                ::  alloc = .FALSE.
   !
   !
   INTERFACE para_poolrecover
      MODULE PROCEDURE para_poolrecover_rm
      MODULE PROCEDURE para_poolrecover_ct
   END INTERFACE
  
   !
   PUBLIC :: para_get_poolindex
   PUBLIC :: para_poolrecover
   PUBLIC :: paratools_deallocate
   PUBLIC :: alloc
   !
CONTAINS
!
!*********************************************
   SUBROUTINE paratools_init( nkpts_g )
   !*********************************************
   !
   IMPLICIT NONE
   !
   INTEGER,  INTENT(IN)  :: nkpts_g
   !
   CHARACTER(14) :: subname='paratools_init'
   INTEGER       :: ip, ierr
   !
   !
   IF ( alloc ) RETURN
   CALL log_push( subname )
   !
   IF ( nkpts_g < 1 ) CALL errore(subname,'invalid nkpts_g',1)
   !
   ALLOCATE( iks_g(0:nproc-1), STAT=ierr )
   IF ( ierr/=0 ) CALL errore(subname,'allocating iks_g',ABS(ierr))
   ALLOCATE( ike_g(0:nproc-1), STAT=ierr )
   IF ( ierr/=0 ) CALL errore(subname,'allocating ike_g',ABS(ierr))
   !
   ALLOCATE( displs( 0:nproc-1 ), STAT=ierr )
   IF ( ierr/=0 ) CALL errore(subname,'allocating displs', ABS(ierr))
   ALLOCATE( msglen( 0:nproc-1 ), STAT=ierr )
   IF ( ierr/=0 ) CALL errore(subname,'allocating msglen', ABS(ierr))
   ALLOCATE( displs_aux( 0:nproc-1 ), STAT=ierr )
   IF ( ierr/=0 ) CALL errore(subname,'allocating displs_aux', ABS(ierr))
   ALLOCATE( msglen_aux( 0:nproc-1 ), STAT=ierr )
   IF ( ierr/=0 ) CALL errore(subname,'allocating msglen_aux', ABS(ierr))
   !
   iks_g(:) = 0
   ike_g(:) = 0
   !
   DO ip = 0, nproc-1
       !
       CALL divide_et_impera( 1, nkpts_g, iks_g(ip), ike_g(ip), ip, nproc )
       !
   ENDDO
   !
   displs_aux(0) = 0
   msglen_aux(0) = ike_g(0)-iks_g(0)+1
   !
   DO ip = 1, nproc-1
       !
       displs_aux(ip) = ike_g(ip-1)
       msglen_aux(ip) = ike_g(ip)-iks_g(ip)+1
       !
   ENDDO
   !
   !
   alloc = .TRUE.
   !
   CALL log_pop( subname )
   RETURN
   !
END SUBROUTINE paratools_init
!
!
!*********************************************
   SUBROUTINE paratools_deallocate( )
   !*********************************************
   !
   IMPLICIT NONE
   !
   CHARACTER(20) :: subname='paratools_deallocate'
   INTEGER       :: ierr
   !
   IF ( .NOT. alloc ) RETURN
   !
   CALL log_push( subname )
   !
   IF ( ALLOCATED( iks_g ) ) THEN
       DEALLOCATE( iks_g, STAT=ierr )
       IF (ierr/=0) CALL errore(subname,'deallocating iks_g',ABS(ierr))
   ENDIF
   IF ( ALLOCATED( ike_g ) ) THEN
       DEALLOCATE( ike_g, STAT=ierr )
       IF (ierr/=0) CALL errore(subname,'deallocating ike_g',ABS(ierr))
   ENDIF
   IF ( ALLOCATED( displs ) ) THEN
       DEALLOCATE( displs, STAT=ierr )
       IF (ierr/=0) CALL errore(subname,'deallocating displs',ABS(ierr))
   ENDIF
   IF ( ALLOCATED( msglen ) ) THEN
       DEALLOCATE( msglen, STAT=ierr )
       IF (ierr/=0) CALL errore(subname,'deallocating msglen',ABS(ierr))
   ENDIF
   IF ( ALLOCATED( displs_aux ) ) THEN
       DEALLOCATE( displs_aux, STAT=ierr )
       IF (ierr/=0) CALL errore(subname,'deallocating displs_aux',ABS(ierr))
   ENDIF
   IF ( ALLOCATED( msglen_aux ) ) THEN
       DEALLOCATE( msglen_aux, STAT=ierr )
       IF (ierr/=0) CALL errore(subname,'deallocating msglen_aux',ABS(ierr))
   ENDIF
   !
   alloc = .FALSE.
   CALL log_pop( subname )
   !
END SUBROUTINE paratools_deallocate
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
   IF ( .NOT. alloc ) CALL paratools_init( nkpts_g )

   !
   iproc = -1
   !
   DO ip = 0, nproc-1
       !
       IF ( ik_g >= iks_g(ip) .AND. ik_g <= ike_g(ip) ) iproc = ip
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
   SUBROUTINE para_poolrecover_ct( mydata )
   !*********************************************
   !  
   IMPLICIT NONE
   !
   COMPLEX(dbl),   INTENT(INOUT) :: mydata(:,:,:)
   !
   CHARACTER(16)        :: subname='para_poolrecover'
   INTEGER              :: nkpts_g, datalen, ip, ik_g, ierr


   CALL timing( subname, OPR="start")
   CALL log_push( subname )
   !
   !
   nkpts_g = SIZE( mydata, 3)
   IF ( .NOT. alloc ) CALL paratools_init( nkpts_g )
   !
#define __TEST
!
#ifdef __TEST
   !
   CALL timing( 'mp_sum', OPR="start")
   !
   DO ik_g = 1, nkpts_g
       CALL mp_sum( mydata(:,:,ik_g) )
   ENDDO
   !
   CALL timing( 'mp_sum', OPR="stop")
   !
#else
   !
   datalen = SIZE( mydata, 1) * SIZE( mydata, 2)
   !
   displs(:) = displs_aux(:) * datalen
   msglen(:) = msglen_aux(:) * datalen
   !
   CALL timing( 'mp_allgather', OPR="start")
   CALL mp_allgather( mydata, displs, msglen )
   CALL timing( 'mp_allgather', OPR="stop")
   !
#endif
   
   CALL timing( subname, OPR="stop")
   CALL log_pop( subname )
   !
   RETURN
   !
END SUBROUTINE para_poolrecover_ct

!*********************************************
   SUBROUTINE para_poolrecover_rm( mydata )
   !*********************************************
   !  
   IMPLICIT NONE
   !
   REAL(dbl),   INTENT(INOUT) :: mydata(:,:)
   !
   CHARACTER(16)        :: subname='para_poolrecover'
   INTEGER              :: nkpts_g, datalen, ip, ik_g, ierr

   
   CALL timing( subname, OPR="start")
   CALL log_push( subname )
   !
   !
   nkpts_g = SIZE( mydata, 2)
   IF ( .NOT. alloc ) CALL paratools_init( nkpts_g )
   !
#define __TEST
!
#ifdef __TEST
   !
   CALL timing( 'mp_sum', OPR="start")
   !
   DO ik_g = 1, nkpts_g
       CALL mp_sum( mydata(:,ik_g) )
   ENDDO
   !
   CALL timing( 'mp_sum', OPR="stop")
   !
#else
   !
   datalen = SIZE( mydata, 1)
   !
   displs(:) = displs_aux(:) * datalen
   msglen(:) = msglen_aux(:) * datalen
   !
   CALL timing( 'mp_allgather', OPR="start")
   CALL mp_allgather( mydata, displs, msglen )
   CALL timing( 'mp_allgather', OPR="stop")
   !
#endif
   
   CALL timing( subname, OPR="stop")
   CALL log_pop( subname )
   !
   RETURN
   !
END SUBROUTINE para_poolrecover_rm

END MODULE paratools_module

