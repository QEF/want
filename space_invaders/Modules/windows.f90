!
! Copyright (C) 2004 WanT Group
!
! This file is distributed under the terms of the
! GNU General Public License. See the file `License\'
! in the root directory of the present distribution,
! or http://www.gnu.org/copyleft/gpl.txt .
!

!*********************************************
   MODULE windows_module
!*********************************************
   USE kinds, ONLY : dbl
   USE constants, ONLY : RYD
   USE parameters, ONLY : nstrx
   USE parser_module, ONLY : change_case
   USE kpoints_module, ONLY : iks, ike, nkpts, nkpts_tot, kpoints_alloc
   USE iotk_module
   IMPLICIT NONE
   PRIVATE
   SAVE

! This module handles data referring to the definition of 
! the initial windows (actual and frozen) given by input.
! Windows_init routine assume that the eigenvalues from PW-DFT
! have already been read and compute all the other quantities.
!
! routines in this module:
! SUBROUTINE windows_allocate()
! SUBROUTINE windows_deallocate()
! SUBROUTINE windows_init(eig, dimwann)
! SUBROUTINE windows_write(unit,name)
! SUBROUTINE windows_read(unit,name,found)
! SUBROUTINE windows_read_ext(unit,name,found)

!
! declarations of common variables
!   

   INTEGER                     :: nbnd               ! number of DFT bands
   INTEGER                     :: nspin              ! number of spin channels
   INTEGER                     :: dimwinx            ! MAX (dimwin(:)) over kpts
   CHARACTER(10)               :: spin_component = 'none' ! 'up', 'down', 'none'
   !
   ! ... starting states within the energy window
   REAL(dbl)                   :: win_min, win_max   ! outer energy window
   REAL(dbl)                   :: froz_min, froz_max ! inner energy window

   INTEGER,      ALLOCATABLE   :: dimwin(:)          ! define which eigenv are in the
   INTEGER,      ALLOCATABLE   :: imin(:)            ! chosen energy window
   INTEGER,      ALLOCATABLE   :: imax(:)            ! dim: nkpts
   REAL(dbl),    ALLOCATABLE   :: eig(:,:)           ! DFT eigenv; dim: nbnd, nkpts
   REAL(dbl)                   :: efermi             ! Fermi energy (from DFT)
   LOGICAL                     :: lcompspace=.TRUE.  ! whether COMPLEMENT space is NOT null
   !
   LOGICAL                     :: alloc=.FALSE.      

   !
   ! ... frozen states
   INTEGER,      ALLOCATABLE   :: dimfroz(:)         ! variable for using frozen (dim: nkpts)
   INTEGER,      ALLOCATABLE   :: indxfroz(:,:)      ! states which are kept equal
   INTEGER,      ALLOCATABLE   :: indxnfroz(:,:)     ! dim: dimwinx nkpts
   LOGICAL                     :: lfrozen =.FALSE.   ! whether FROZEN states are present
   LOGICAL,      ALLOCATABLE   :: frozen(:,:)        ! which are the frozen states
                                                     ! dim: dimwinx, nkpts
!
! end of declarations
!

   PUBLIC :: nkpts, nbnd, nspin, spin_component, dimwinx
   PUBLIC :: win_min, win_max, froz_min, froz_max
   PUBLIC :: dimwin, imin, imax, eig, efermi, lcompspace
   PUBLIC :: dimfroz, indxfroz, indxnfroz, lfrozen, frozen
   PUBLIC :: alloc

   PUBLIC :: windows_allocate
   PUBLIC :: windows_deallocate
   PUBLIC :: windows_init
   PUBLIC :: windows_write
   PUBLIC :: windows_read
   PUBLIC :: windows_read_ext

CONTAINS

!
! subroutines
!

!**********************************************************
   SUBROUTINE windows_init( eig_, dimwann )
   !**********************************************************
   IMPLICIT NONE
       REAL(dbl), INTENT(in) :: eig_(:,:)
       INTEGER,   INTENT(in) :: dimwann
       CHARACTER(12)         :: subname="windows_init"
       INTEGER               :: kifroz_max, kifroz_min, idum
       INTEGER               :: i, ik, ierr
       !
       ! nbnd and nkpts are supposed to be already setted

       IF ( .NOT. alloc ) CALL errore(subname,'windows module not allocated',1)
       IF ( nkpts <= 0) CALL errore(subname,'Invalid nkpts',ABS(nkpts)+1)
       IF ( nbnd <= 0) CALL errore(subname,'Invalid nbnd',ABS(nbnd)+1)
       IF ( SIZE(eig_,1) /= nbnd ) CALL errore(subname,'Invalid EIG size1',ABS(nbnd)+1)
       IF ( SIZE(eig_,2) /= nkpts ) CALL errore(subname,'Invalid EIG size2',ABS(nkpts)+1)
      
!
! ... windows dimensions
!
       kpoints_loop: &
       DO ik = 1,nkpts

          !
          ! ... Check which eigenvalues fall within the outer energy window
          IF ( eig_(1,ik) > win_max .OR. eig_(nbnd,ik) < win_min ) &
              CALL errore(subname, ' energy window contains no eigenvalues ',1)

          imin(ik) = 0
          DO i = 1, nbnd
              IF ( imin(ik) == 0 ) THEN
                  IF ( ( eig_(i,ik) >= win_min ) .AND. ( eig_(i,ik) <= win_max )) THEN
                      imin(ik) = i
                      imax(ik) = i
                  ENDIF
              ENDIF
              IF ( eig_(i,ik) <= win_max ) imax(ik) = i
          ENDDO
          !
          dimwin(ik) = imax(ik) - imin(ik) + 1       
          !
          IF ( dimwin(ik) < dimwann) CALL errore(subname,'dimwin < dimwann ', ik )
          IF ( dimwin(ik) > nbnd) CALL errore(subname,'dimwin > nbnd ', ik )
          IF ( imax(ik) < imin(ik) ) CALL errore(subname,'imax < imin ',ik)
          IF ( imin(ik) < 1 ) CALL errore(subname,' imin < 1 ',ik)
          !
       ENDDO kpoints_loop


!
! ... frozen states
!
       dimwinx = MAXVAL( dimwin(:) )
       !
       ALLOCATE( dimfroz(nkpts), STAT=ierr )
           IF ( ierr/=0 ) CALL errore(subname,' allocating dimfroz ',ABS(ierr))
       ALLOCATE( indxfroz(dimwinx,nkpts), STAT=ierr )
           IF ( ierr/=0 ) CALL errore(subname,' allocating indxfroz ',ABS(ierr))
       ALLOCATE( indxnfroz(dimwinx,nkpts), STAT=ierr )
           IF ( ierr/=0 ) CALL errore(subname,' allocating indxnfroz ',ABS(ierr))
       ALLOCATE( frozen(dimwinx,nkpts), STAT=ierr )
           IF ( ierr/=0 ) CALL errore(subname,' allocating frozen ',ABS(ierr))
       lfrozen = .FALSE.


       kpoints_frozen_loop: &
       DO ik = 1, nkpts
          !
          ! ... frozen states
          frozen(:,ik) = .FALSE.
        
          kifroz_min = 0
          kifroz_max = -1
          ! Note that the above obeys kifroz_max-kifroz_min+1=kdimfroz=0,
          ! as required

          DO i = imin(ik), imax(ik)
              IF ( kifroz_min == 0 ) THEN
                  IF ( ( eig_(i,ik) >= froz_min ).AND.( eig_(i,ik) <= froz_max )) THEN
                      !    relative to bottom of outer window
                      kifroz_min = i - imin(ik) + 1   
                      kifroz_max = i - imin(ik) + 1
                  ENDIF
              ELSE IF ( eig_(i,ik) <= froz_max ) THEN
                  kifroz_max = kifroz_max + 1
              ENDIF
          ENDDO
    
          dimfroz(ik) = kifroz_max - kifroz_min + 1
          IF ( dimfroz(ik) > dimwann ) CALL errore(subname,'dimfroz > dimwann',ik)
          !
          ! ... Generate index array for frozen states inside inner window
          ! 
          indxfroz(:,ik) = 0
          IF ( dimfroz(ik) > 0 ) THEN
               lfrozen = .TRUE.
               DO i = 1, dimfroz(ik)
                   indxfroz(i,ik) = kifroz_min + i - 1
                   frozen(indxfroz(i,ik),ik) = .TRUE.
               ENDDO
               IF ( indxfroz(dimfroz(ik),ik) /= kifroz_max ) &
                   CALL errore(subname,'wrong number of frozen states',ik )
          ENDIF
   
          !
          ! ... Generate index array for non-frozen states
          !
          idum = 0
          indxnfroz(:,ik) = 0
          DO i = 1, dimwin(ik)
              IF( .NOT. frozen(i,ik) ) THEN
                  idum = idum + 1
                  indxnfroz(idum,ik) = i
              ENDIF
          ENDDO
          IF ( idum /= dimwin(ik)-dimfroz(ik) )  &
              CALL errore(subname, 'wrong number of non-frozen states', ik)

       ENDDO kpoints_frozen_loop   

   END SUBROUTINE windows_init


!**********************************************************
   SUBROUTINE windows_allocate()
   !**********************************************************
   IMPLICIT NONE
       CHARACTER(16)      :: subname="windows_allocate"
       INTEGER            :: ierr

       IF ( nbnd <= 0 .OR. nkpts <= 0 ) &
           CALL errore(subname,' Invalid NBND or NKPTS ',1)
       IF ( nspin /= 1 .AND. nspin /=2 ) CALL errore(subname,'Invalid NSPIN',ABS(nspin)+1)
       !
       ALLOCATE( dimwin(nkpts), STAT=ierr )
           IF ( ierr/=0 ) CALL errore(subname,' allocating dimwin ',nkpts)      
       ALLOCATE( imin(nkpts), STAT=ierr )
           IF ( ierr/=0 ) CALL errore(subname,' allocating imin ',nkpts)      
       ALLOCATE( imax(nkpts), STAT=ierr )
           IF ( ierr/=0 ) CALL errore(subname,' allocating imax ',nkpts)      
       ALLOCATE( eig(nbnd,nkpts), STAT=ierr )
           IF ( ierr/=0 ) CALL errore(subname,' allocating eig ',nbnd*nkpts)

       alloc = .TRUE.
   END SUBROUTINE windows_allocate


!**********************************************************
   SUBROUTINE windows_deallocate()
   !**********************************************************
   IMPLICIT NONE
       CHARACTER(18)      :: subname="windows_deallocate"
       INTEGER            :: ierr

       IF ( ALLOCATED(dimwin) ) THEN
            DEALLOCATE(dimwin, STAT=ierr)
            IF (ierr/=0)  CALL errore(subname,' deallocating dimwin ',ABS(ierr))
       ENDIF
       IF ( ALLOCATED(imin) ) THEN
            DEALLOCATE(imin, STAT=ierr)
            IF (ierr/=0)  CALL errore(subname,' deallocating imin ',ABS(ierr))
       ENDIF
       IF ( ALLOCATED(imax) ) THEN
            DEALLOCATE(imax, STAT=ierr)
            IF (ierr/=0)  CALL errore(subname,' deallocating imax ',ABS(ierr))
       ENDIF
       IF ( ALLOCATED(eig) ) THEN
            DEALLOCATE(eig, STAT=ierr)
            IF (ierr/=0)  CALL errore(subname,' deallocating eig ',ABS(ierr))
       ENDIF
       IF ( ALLOCATED(dimfroz) ) THEN
            DEALLOCATE(dimfroz, STAT=ierr)
            IF (ierr/=0)  CALL errore(subname,' deallocating dimfroz ',ABS(ierr))
       ENDIF
       IF ( ALLOCATED(indxfroz) ) THEN
            DEALLOCATE(indxfroz, STAT=ierr)
            IF (ierr/=0)  CALL errore(subname,' deallocating indxfroz ',ABS(ierr))
       ENDIF
       IF ( ALLOCATED(indxnfroz) ) THEN
            DEALLOCATE(indxnfroz, STAT=ierr)
            IF (ierr/=0)  CALL errore(subname,' deallocating indxnfroz ',ABS(ierr))
       ENDIF
       IF ( ALLOCATED(frozen) ) THEN
            DEALLOCATE(frozen, STAT=ierr)
            IF (ierr/=0)  CALL errore(subname,' deallocating frozen ',ABS(ierr))
       ENDIF
       alloc = .FALSE.
   END SUBROUTINE windows_deallocate


!**********************************************************
   SUBROUTINE windows_write(unit,name)
   !**********************************************************
   IMPLICIT NONE
       INTEGER,         INTENT(in) :: unit
       CHARACTER(*),    INTENT(in) :: name
       CHARACTER(nstrx)   :: attr
       CHARACTER(13)      :: subname="windows_write"
       INTEGER            :: ierr

       IF ( .NOT. alloc ) RETURN
       
       CALL iotk_write_begin(unit,TRIM(name))
       CALL iotk_write_attr(attr,"nbnd",nbnd,FIRST=.TRUE.)
       CALL iotk_write_attr(attr,"nkpts",nkpts)
       CALL iotk_write_attr(attr,"nspin",nspin)
       CALL iotk_write_attr(attr,"spin_component",TRIM(spin_component))
       CALL iotk_write_attr(attr,"efermi",efermi)
       CALL iotk_write_attr(attr,"dimwinx",dimwinx)
       CALL iotk_write_attr(attr,"lcompspace",lcompspace)
       CALL iotk_write_attr(attr,"lfrozen",lfrozen)
       CALL iotk_write_empty(unit,"DATA",ATTR=attr, IERR=ierr)
       IF (ierr/=0) CALL errore(subname,'writing DATA',ABS(ierr))

       CALL iotk_write_dat(unit,"DIMWIN",dimwin)
       CALL iotk_write_dat(unit,"IMIN",imin)
       CALL iotk_write_dat(unit,"IMAX",imax)
       CALL iotk_write_dat(unit,"EIG",eig)

       CALL iotk_write_dat(unit,"DIMFROZ",dimfroz)
       CALL iotk_write_dat(unit,"INDXFROZ",indxfroz)
       CALL iotk_write_dat(unit,"INDXNFROZ",indxnfroz)
       CALL iotk_write_dat(unit,"FROZEN",frozen)

       CALL iotk_write_end(unit,TRIM(name))
   END SUBROUTINE windows_write
   

!**********************************************************
   SUBROUTINE windows_read(unit,name,found)
   !**********************************************************
   IMPLICIT NONE
       INTEGER,           INTENT(in) :: unit
       CHARACTER(*),      INTENT(in) :: name
       LOGICAL,           INTENT(out):: found
       CHARACTER(nstrx)   :: attr
       CHARACTER(12)      :: subname="windows_read"
       INTEGER            :: nkpts_
       INTEGER            :: ierr

       IF ( alloc ) CALL windows_deallocate()
    
       CALL iotk_scan_begin(unit,TRIM(name),FOUND=found,IERR=ierr)
       IF (.NOT. found) RETURN
       IF (ierr>0)  CALL errore(subname,'Wrong format in tag '//TRIM(name),ierr)
       found = .TRUE.

       CALL iotk_scan_empty(unit,'DATA',ATTR=attr,IERR=ierr)
       IF (ierr/=0) CALL errore(subname,'Unable to find tag DATA',ABS(ierr))
       CALL iotk_scan_attr(attr,'nbnd',nbnd,IERR=ierr)
       IF (ierr/=0) CALL errore(subname,'Unable to find attr NBND',ABS(ierr))
       CALL iotk_scan_attr(attr,'nkpts',nkpts_,IERR=ierr)
       IF (ierr/=0) CALL errore(subname,'Unable to find attr NKPTS',ABS(ierr))
       CALL iotk_scan_attr(attr,'nspin',nspin,IERR=ierr)
       IF (ierr/=0) CALL errore(subname,'Unable to find attr NSPIN',ABS(ierr))
       CALL iotk_scan_attr(attr,'efermi',efermi,IERR=ierr)
       IF (ierr/=0) CALL errore(subname,'Unable to find attr EFERMI',ABS(ierr))
       CALL iotk_scan_attr(attr,'dimwinx',dimwinx,IERR=ierr)
       IF (ierr/=0) CALL errore(subname,'Unable to find attr DIMWINX',ABS(ierr))
       CALL iotk_scan_attr(attr,'lcompspace',lcompspace,IERR=ierr)
       IF (ierr/=0) CALL errore(subname,'Unable to find attr LCOMPSPACE',ABS(ierr))
       CALL iotk_scan_attr(attr,'lfrozen',lfrozen,IERR=ierr)
       IF (ierr/=0) CALL errore(subname,'Unable to find attr LFROZEN',ABS(ierr))

       IF ( kpoints_alloc ) THEN
            IF ( nkpts_ /= nkpts ) CALL errore(subname,'Invalid NKPTS',ABS(nkpts_)+1)
       ELSE
            nkpts = nkpts_
       ENDIF
       !
       CALL windows_allocate()
       !
       ALLOCATE( dimfroz(nkpts), STAT=ierr )
           IF ( ierr/=0 ) CALL errore(subname,' allocating dimfroz ',ABS(ierr))
       ALLOCATE( indxfroz(dimwinx,nkpts), STAT=ierr )
           IF ( ierr/=0 ) CALL errore(subname,' allocating indxfroz ',ABS(ierr))
       ALLOCATE( indxnfroz(dimwinx,nkpts), STAT=ierr )
           IF ( ierr/=0 ) CALL errore(subname,' allocating indxnfroz ',ABS(ierr))
       ALLOCATE( frozen(dimwinx,nkpts), STAT=ierr )
           IF ( ierr/=0 ) CALL errore(subname,' allocating frozen ',ABS(ierr))

       CALL iotk_scan_dat(unit,'DIMWIN',dimwin,IERR=ierr)
       IF (ierr/=0) CALL errore(subname,'Unable to find DIMWIN',ABS(ierr))
       CALL iotk_scan_dat(unit,'IMIN',imin,IERR=ierr)
       IF (ierr/=0) CALL errore(subname,'Unable to find IMIN',ABS(ierr))
       CALL iotk_scan_dat(unit,'IMAX',imax,IERR=ierr)
       IF (ierr/=0) CALL errore(subname,'Unable to find IMAX',ABS(ierr))
       CALL iotk_scan_dat(unit,'EIG',eig,IERR=ierr)
       IF (ierr/=0) CALL errore(subname,'Unable to find EIG',ABS(ierr))

       CALL iotk_scan_dat(unit,'DIMFROZ',dimfroz,IERR=ierr)
       IF (ierr/=0) CALL errore(subname,'Unable to find DIMFROZ',ABS(ierr))
       CALL iotk_scan_dat(unit,'INDXFROZ',indxfroz,IERR=ierr)
       IF (ierr/=0) CALL errore(subname,'Unable to find INDXFROZ',ABS(ierr))
       CALL iotk_scan_dat(unit,'INDXNFROZ',indxnfroz,IERR=ierr)
       IF (ierr/=0) CALL errore(subname,'Unable to find INDXNFROZ',ABS(ierr))
       CALL iotk_scan_dat(unit,'FROZEN',frozen,IERR=ierr)
       IF (ierr/=0) CALL errore(subname,'Unable to find FROZEN',ABS(ierr))

       CALL iotk_scan_end(unit,TRIM(name),IERR=ierr)
       IF (ierr/=0)  CALL errore(subname,'Unable to end tag '//TRIM(name),ABS(ierr))
   END SUBROUTINE windows_read


!**********************************************************
   SUBROUTINE windows_read_ext(unit,name,found)
   !**********************************************************
   IMPLICIT NONE
       INTEGER,           INTENT(in) :: unit
       CHARACTER(*),      INTENT(in) :: name
       LOGICAL,           INTENT(out):: found
       CHARACTER(16)      :: subname="windows_read_ext"
       CHARACTER(nstrx)   :: attr, str
       LOGICAL            :: lfound
       INTEGER            :: idum, lindex, ik, ierr

       CALL iotk_scan_begin(unit,TRIM(name),ATTR=attr,FOUND=found,IERR=ierr)
       IF (.NOT. found) RETURN
       IF (ierr>0)  CALL errore(subname,'Wrong format in tag '//TRIM(name),ierr)
       found = .TRUE.

       CALL iotk_scan_attr(attr,'nspin',nspin,IERR=ierr)
       IF (ierr/=0)  CALL errore(subname,'Unable to find NSPIN',ABS(ierr))
       CALL iotk_scan_attr(attr,'nk',idum,IERR=ierr)
       IF (ierr/=0)  CALL errore(subname,'Unable to find NK',ABS(ierr))
       IF ( nkpts_tot /= idum ) CALL errore(subname,'nkpts_tot /= nk',ABS(idum)+1)

       CALL iotk_scan_attr(attr,'nbnd',nbnd,IERR=ierr)
       IF (ierr/=0)  CALL errore(subname,'Unable to find nbnd',ABS(ierr))
       CALL iotk_scan_attr(attr,'efermi',efermi,IERR=ierr)
       IF (ierr/=0)  CALL errore(subname,'Unable to find efermi',ABS(ierr))
       CALL iotk_scan_attr(attr,'units',str,FOUND=lfound, IERR=ierr)
       IF (ierr>0)  CALL errore(subname,'Wrong fmt in units',ABS(ierr))
       IF ( lfound ) THEN
           CALL change_case(str,'UPPER')
           IF (TRIM(str) /= 'RYDBERG' .AND. TRIM(str) /= 'RY' .AND. &
               TRIM(str) /= 'RYD')&
               CALL errore(subname,'Wrong units in Energies',5)
       ENDIF
 
       !
       ! ... allocating windows
       CALL windows_allocate()
   
       !
       ! take into account spin polarization by using the
       ! predefined iks, ike
       !
       lindex = 0
       DO ik= iks, ike
           lindex = lindex + 1
           CALL iotk_scan_dat(unit,'e'//TRIM(iotk_index(ik)),eig(:,lindex),IERR=ierr)
           IF (ierr/=0)  CALL errore(subname,'Unable to find EIGVAL',ik)
       ENDDO
       IF ( lindex /= nkpts ) CALL errore(subname,'problems with spin and kpt ?',5)
       ! conversion to eV
       eig(:,:) = RYD * eig(:,:)
       efermi   = RYD * efermi

       CALL iotk_scan_end(unit,TRIM(name),IERR=ierr)
       IF (ierr/=0)  CALL errore(subname,'Unable to end tag Eigenvalues',ABS(ierr))
   END SUBROUTINE windows_read_ext

END MODULE windows_module
