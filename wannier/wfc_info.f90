!
! Copyright (C) 2004 WanT Group
!
! This file is distributed under the terms of the
! GNU General Public License. See the file `License\'
! in the root directory of the present distribution,
! or http://www.gnu.org/copyleft/gpl.txt .
!

!*********************************************
   MODULE wfc_info_module
!*********************************************
   USE parameters, ONLY : nstrx
   IMPLICIT NONE
   PRIVATE

! This module define the types handling the description 
! of WFC data
!
! routines in this module:
! SUBROUTINE wfc_info_allocate(npwx,nbnd,nkpts,nwfc,obj)
! SUBROUTINE wfc_info_deallocate(obj)
! SUBROUTINE wfc_info_getfreeindex(index,obj)
! INTEGER FUNCTION wfc_info_getindex(ibnd,ik,label,obj)
! SUBROUTINE wfc_info_add(npw,ibnd,ik,label,obj[,index])
! SUBROUTINE wfc_info_delete(obj[,ibnd][,ik][,label][,index])
! SUBROUTINE wfc_info_print(unit,obj)
!

   !
   ! ... the TYPE
   TYPE wfc_info
        INTEGER                 :: nwfc          ! total number of allocated wfc
        INTEGER                 :: nwfc_used     ! current number of occupied places
        LOGICAL,      POINTER   :: used(:)       ! whether a wfc place is occupied 
        INTEGER                 :: nbnd          ! number of bands
        INTEGER                 :: nkpts         ! number of kpoints
        INTEGER                 :: npwx          ! the maximum num of pw
        INTEGER,      POINTER   :: npw(:)        ! the actual num of pws for each wfc
        INTEGER,      POINTER   :: ibnd(:)       ! the actual band index for each wfc
        INTEGER,      POINTER   :: ik(:)         ! the actual kpt index for each wfc
        CHARACTER(nstrx), POINTER :: label(:)    ! wfc label
                                                 ! a given band and kpt
        LOGICAL                 :: alloc
   END TYPE wfc_info


   PUBLIC :: wfc_info_allocate
   PUBLIC :: wfc_info_deallocate
   PUBLIC :: wfc_info_add
   PUBLIC :: wfc_info_getindex
   PUBLIC :: wfc_info_getfreeindex
   PUBLIC :: wfc_info_delete
   PUBLIC :: wfc_info_print
   PUBLIC :: wfc_info

CONTAINS

!
! subroutines
!

!**********************************************************
   SUBROUTINE wfc_info_allocate(npwx, nbnd, nkpts, nwfc,obj)
   !**********************************************************
   IMPLICIT NONE
       INTEGER,    INTENT(in)    :: npwx
       INTEGER,    INTENT(in)    :: nwfc
       INTEGER,    INTENT(in)    :: nbnd, nkpts
       TYPE(wfc_info),  INTENT(inout) :: obj

       CHARACTER(17)      :: subname="wfc_info_allocate"
       INTEGER            :: ierr 

       IF ( npwx <= 0 )  CALL errore(subname,'npwx <= 0',ABS(npwx)+1)
       IF ( nwfc <= 0 )  CALL errore(subname,'nwfc <= 0',ABS(nwfc)+1)
       IF ( nbnd <= 0 )  CALL errore(subname,'nbnd <= 0',ABS(nbnd)+1)
       IF ( nkpts <= 0 )  CALL errore(subname,'nkpts <= 0',ABS(nkpts)+1)

       ALLOCATE( obj%ibnd(nwfc), STAT=ierr )
          IF (ierr/=0) CALL errore(subname,'allocating ibnd',ABS(ierr))
       ALLOCATE( obj%ik(nwfc), STAT=ierr )
          IF (ierr/=0) CALL errore(subname,'allocating ik',ABS(ierr))
       ALLOCATE( obj%npw(nwfc), STAT=ierr )
          IF (ierr/=0) CALL errore(subname,'allocating npw',ABS(ierr))
       ALLOCATE( obj%used(nwfc), STAT=ierr )
          IF (ierr/=0) CALL errore(subname,'allocating used',ABS(ierr))
       ALLOCATE( obj%label(nwfc), STAT=ierr )
          IF (ierr/=0) CALL errore(subname,'allocating label',ABS(ierr))
 
       obj%nwfc  = nwfc
       obj%npwx  = npwx
       obj%nbnd  = nbnd
       obj%nkpts = nkpts
       obj%nwfc_used = 0
       obj%used(:) = .FALSE.
       obj%label(:) = " "
       obj%npw(:) = 0
       obj%ibnd(:) = 0
       obj%ik(:) = 0
       obj%alloc = .TRUE.
   END SUBROUTINE wfc_info_allocate


!**********************************************************
   SUBROUTINE wfc_info_deallocate(obj)
   !**********************************************************
   IMPLICIT NONE
       TYPE(wfc_info),  INTENT(inout) :: obj
       CHARACTER(19)      :: subname="wfc_info_deallocate"
       INTEGER            :: ierr

       IF ( .NOT. obj%alloc ) CALL errore(subname,'obj NOT allocated',1) 
       DEALLOCATE( obj%npw, STAT=ierr)
           IF(ierr/=0) CALL errore(subname,'deallocating npw',ABS(ierr))
       DEALLOCATE( obj%used, STAT=ierr)
           IF(ierr/=0) CALL errore(subname,'deallocating used',ABS(ierr))
       DEALLOCATE( obj%ibnd, STAT=ierr )
          IF (ierr/=0) CALL errore(subname,'deallocating ibnd',ABS(ierr))
       DEALLOCATE( obj%ik, STAT=ierr )
          IF (ierr/=0) CALL errore(subname,'deallocating ik',ABS(ierr))
       DEALLOCATE( obj%label, STAT=ierr)
           IF(ierr/=0) CALL errore(subname,'deallocating label',ABS(ierr))
       
       obj%nwfc = 0
       obj%npwx = 0
       obj%nbnd = 0
       obj%nkpts = 0
       obj%nwfc_used = 0
       NULLIFY(obj%npw)
       NULLIFY(obj%used)
       NULLIFY(obj%ibnd)
       NULLIFY(obj%ik)
       obj%alloc = .FALSE.
   END SUBROUTINE wfc_info_deallocate


!**********************************************************
   FUNCTION wfc_info_getindex(ibnd,ik,label,obj)
   !**********************************************************
   !
   ! writes the internal index of the first wfc in OBJ matching
   ! the given IBND, IK and LABEL. Further matching are ignored.
   ! If not matching is found hte returned index is 0
   !
   IMPLICIT NONE
       INTEGER         :: wfc_info_getindex
       INTEGER         :: ibnd, ik
       CHARACTER(*)    :: label
       TYPE(wfc_info)  :: obj

       CHARACTER(17)   :: subname="wfc_info_getindex"
       INTEGER         :: i      

       IF ( .NOT. obj%alloc) CALL errore(subname,'descriptor not allocated',1)

       wfc_info_getindex = 0
       DO i=1,obj%nwfc
            IF ( obj%ibnd(i) == ibnd .AND. obj%ik(i) == ik .AND. &
                 TRIM(obj%label(i)) == TRIM(label) ) THEN
                 wfc_info_getindex = i
                 EXIT
            ENDIF
       ENDDO
   RETURN
   END FUNCTION wfc_info_getindex


!**********************************************************
   SUBROUTINE wfc_info_getfreeindex(index,obj)
   !**********************************************************
   !
   ! get the first free index in the descriptor OBJ
   ! if no index is found INDEX is set to 0
   IMPLICIT NONE
       INTEGER,         INTENT(out)   :: index
       TYPE(wfc_info),  INTENT(in)    :: obj
       INTEGER            :: i

       index = 0
       DO i=1,obj%nwfc
           IF ( .NOT. obj%used(i) ) THEN
              index = i
              EXIT
           ENDIF
       ENDDO
   RETURN
   END SUBROUTINE wfc_info_getfreeindex


!**********************************************************
   SUBROUTINE wfc_info_add(npw,ibnd,ik,label,obj,INDEX)
   !**********************************************************
   !
   ! the optional IBND,IK values are eventually set in the
   ! proper internal vectors
   !
   IMPLICIT NONE
       INTEGER,           INTENT(in)    :: npw
       CHARACTER(*),      INTENT(in)    :: label
       INTEGER,           INTENT(in)    :: ibnd, ik
       INTEGER, OPTIONAL, INTENT(out)   :: INDEX
       TYPE(wfc_info),    INTENT(inout) :: obj
       CHARACTER(12)      :: subname="wfc_info_add"
       INTEGER            :: lindex

       IF ( .NOT. obj%alloc ) CALL errore(subname,'obj NOT allocated',1) 
       IF ( npw > obj%npwx ) CALL errore(subname,'npw too large',npw)

       CALL wfc_info_getfreeindex(lindex, obj)
       IF (lindex ==0) CALL errore(subname,'no available free slot',2)
       IF ( PRESENT(index) ) index = lindex

       obj%nwfc_used = obj%nwfc_used + 1
       obj%npw(lindex) = npw
       obj%used(lindex) = .TRUE.
       obj%label(lindex) = TRIM(label)
       obj%ibnd(lindex) = ibnd
       obj%ik(lindex)   = ik

   END SUBROUTINE wfc_info_add


!**********************************************************
   SUBROUTINE wfc_info_delete_base(index,obj)
   !**********************************************************
   !
   ! delete the index-th wfc from the wfc list obj
   ! in practice obj%used(index) is set to .FALSE.
   ! 
   IMPLICIT NONE
       INTEGER,         INTENT(in)    :: index
       TYPE(wfc_info),  INTENT(inout) :: obj
       CHARACTER(20)      :: subname="wfc_info_delete_base"

       IF ( .NOT. obj%used(index) ) CALL errore(subname,'index wfc not used',1)
       obj%nwfc_used = obj%nwfc_used - 1
       obj%used(index) = .FALSE.
       obj%label(index) = " "
       obj%npw(index) = 0
       obj%ibnd(index) = 0
       obj%ik(index) = 0
   END SUBROUTINE wfc_info_delete_base


!*********************************************************
   SUBROUTINE wfc_info_delete(obj,ibnd,ik,label,index)
   !*********************************************************
   !
   ! delete the data (but do not free the memory) of all the
   ! wfcs corresponding to the given value of the selected parameter(s)
   ! If more than one field is specified only the wfcs matching all the conditions
   ! will be deleted
   !
   IMPLICIT NONE
       INTEGER, OPTIONAL,      INTENT(in) :: ibnd, ik, index
       CHARACTER(*), OPTIONAL, INTENT(in) :: label
       TYPE(wfc_info),      INTENT(inout) :: obj
       CHARACTER(15)      :: subname="wfc_info_delete"
       INTEGER, ALLOCATABLE :: list(:)
       INTEGER  :: i, ierr, nlist

       IF ( .NOT. obj%alloc ) CALL errore(subname,'obj NOT yet allocated',1)
       IF ( .NOT. ( PRESENT(ibnd) .OR. PRESENT(ik) .OR. PRESENT(label) .OR. PRESENT(index)) ) &
            CALL errore(subname,'no field specified for matching',1)
       IF ( PRESENT(index) .AND. ( PRESENT(ibnd) .OR. PRESENT(ik) .OR. PRESENT(label)) ) &
            CALL errore(subname,'index should be specified alone',2)

       ! 
       ! these are some singular cases
       IF ( obj%nwfc_used == 0 ) RETURN
       IF ( PRESENT(index) ) THEN 
            CALL wfc_info_delete_base(index,obj)
            RETURN
       ENDIF

       !
       ! the general case
       !
       ALLOCATE( list(obj%nwfc_used), STAT=ierr )
           IF(ierr/=0) CALL errore(subname,'allocating LIST',ABS(ierr))
       list = 0
       nlist = 0
       DO i=1,obj%nwfc
          IF ( obj%used(i) ) THEN
             nlist = nlist + 1
             list(nlist) = i
          ENDIF
       ENDDO
       IF ( nlist /= obj%nwfc_used ) CALL errore(subname,'unexpected on NLIST',nlist+1)

       !
       ! apply the conditions
       !
       IF( PRESENT(ibnd) ) THEN
          DO i=1,nlist
             IF ( list(i) /= 0 ) THEN
                 IF ( obj%ibnd(list(i)) /= ibnd ) list(i) = 0
             ENDIF
          ENDDO
       ENDIF

       IF( PRESENT(ik) ) THEN
          DO i=1,nlist
             IF ( list(i) /= 0 ) THEN
                 IF ( obj%ik(list(i)) /= ik ) list(i) = 0
             ENDIF
          ENDDO
       ENDIF

       IF( PRESENT(label) ) THEN
          DO i=1,nlist
             IF ( list(i) /= 0 ) THEN
                IF ( TRIM( obj%label(list(i)) ) /= TRIM(label) ) list(i) = 0
             ENDIF
          ENDDO
       ENDIF

       !
       ! elimination of the selected wfcs
       DO i=1,nlist
          IF ( list(i) /= 0 ) CALL wfc_info_delete_base(list(i), obj)
       ENDDO 
       RETURN
   END SUBROUTINE wfc_info_delete



!**********************************************************
   SUBROUTINE wfc_info_print(unit,name,obj)
   !**********************************************************
   !
   ! printout information about the wfc descriptor OBJ
   !
   IMPLICIT NONE
       INTEGER,         INTENT(in) :: unit
       CHARACTER(*),    INTENT(in) :: name
       TYPE(wfc_info),  INTENT(in) :: obj
       INTEGER :: i
       

       IF ( .NOT. obj%alloc ) THEN 
           WRITE(unit,"(/,2x,A,' Wfc descriptor NOT allocated')") TRIM(name)
           RETURN
       ENDIF

       WRITE(unit, 100 ) TRIM(name), obj%nwfc, obj%nwfc_used, &
                         obj%nbnd, obj%nkpts
       100 FORMAT( /,2x, A,' Wfc descriptor:', / , &  
                     2x, 'Number of wfc (allocated, init)    : ', 2I5, /,& 
                     2x, 'Physical dimensions (nbnd, nkpts)  : ', 2I5 )

       IF ( obj%nwfc_used > 0 ) THEN 
           WRITE(unit, 200)
           200 FORMAT( 2x, 3x,'Wfc #', 7x, 'ibnd', 5x, 'kpt' , 5x, 'npw', 5x, 'label',/,&
                       2x,  50('-'))
       
           DO i=1,obj%nwfc
              IF ( obj%used(i) ) &
                  WRITE(unit, 300 ) i, obj%ibnd(i), obj%ik(i), obj%npw(i), TRIM(obj%label(i))
           ENDDO    
           300 FORMAT( 2x, 2x,i4, 8x,i4, 5x,i3, 1x,i8,6x,A)
       ENDIF

       WRITE(unit,"()")

   END SUBROUTINE wfc_info_print

END MODULE wfc_info_module

