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
   !
   USE kinds,                    ONLY : dbl
   USE constants,                ONLY : RYD, TWO, ZERO
   USE parameters,               ONLY : nstrx
   USE timing_module,            ONLY : timing
   USE log_module,               ONLY : log_push, log_pop
   USE parser_module,            ONLY : change_case
   USE kpoints_module,           ONLY : nkpts_g, kpoints_alloc
   USE io_global_module,         ONLY : ionode, ionode_id
   USE control_module,           ONLY : read_efermi
   USE input_parameters_module,  ONLY : iwin_min, iwin_max, ifroz_min, ifroz_max
   USE mp,                       ONLY : mp_bcast
   !
   USE iotk_module
   USE qexml_module 
   USE qexpt_module 
   !
#ifdef __ETSF_IO
   USE etsf_io
   USE etsf_io_tools
   USE etsf_io_data_module
#endif
   !
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
! SUBROUTINE windows_write(iun,tag)
! SUBROUTINE windows_read(iun,tag,found)
! SUBROUTINE windows_read_ext(filefmt)

!
! declarations of common variables
!   

   INTEGER                     :: nbnd               ! number of DFT bands
   INTEGER                     :: nspin              ! number of spin channels
   INTEGER                     :: dimwinx            ! MAX (dimwin(:)) over kpts
   INTEGER                     :: ispin              ! index of the spin component
   CHARACTER(10)               :: spin_component = 'none' ! 'up', 'down','dw', 'none'
   !
   ! ... starting states within the energy window
   REAL(dbl)                   :: win_min, win_max   ! outer energy window
   REAL(dbl)                   :: froz_min, froz_max ! inner energy window

   INTEGER,      ALLOCATABLE   :: dimwin(:)          ! define which eigenv are in the
   INTEGER,      ALLOCATABLE   :: imin(:)            ! chosen energy window
   INTEGER,      ALLOCATABLE   :: imax(:)            ! dim: nkpts_g
   REAL(dbl),    ALLOCATABLE   :: eig(:,:)           ! DFT eigenv; dim: nbnd, nkpts_g
   REAL(dbl)                   :: efermi             ! Fermi energy
   REAL(dbl)                   :: nelec              ! total number of electrons
   !
   LOGICAL                     :: alloc=.FALSE.      

   !
   ! ... frozen states
   INTEGER,      ALLOCATABLE   :: dimfroz(:)         ! variable for using frozen (dim: nkpts_g)
   INTEGER,      ALLOCATABLE   :: indxfroz(:,:)      ! states which are kept equal
   INTEGER,      ALLOCATABLE   :: indxnfroz(:,:)     ! dim: dimwinx nkpts_g
   LOGICAL                     :: lfrozen =.FALSE.   ! whether FROZEN states are present
   LOGICAL,      ALLOCATABLE   :: frozen(:,:)        ! which are the frozen states
                                                     ! dim: dimwinx, nkpts_g
!
! end of declarations
!

   PUBLIC :: nkpts_g, nbnd, nspin, ispin, spin_component, dimwinx
   PUBLIC :: win_min, win_max, froz_min, froz_max
   PUBLIC :: dimwin, imin, imax, eig, efermi, nelec
   PUBLIC :: dimfroz, indxfroz, indxnfroz, lfrozen, frozen
   PUBLIC :: alloc

   PUBLIC :: windows_allocate
   PUBLIC :: windows_deallocate
   PUBLIC :: windows_memusage
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
   !
   ! nbnd and nkpts_g are supposed to be already setted
   !
   IMPLICIT NONE
       REAL(dbl), INTENT(in) :: eig_(:,:)
       INTEGER,   INTENT(in) :: dimwann
       CHARACTER(12)         :: subname="windows_init"
       LOGICAL               :: lhave_min, lhave_max
       INTEGER               :: kifroz_max, kifroz_min, idum
       INTEGER               :: i, ik_g, ierr

       !
       CALL timing( subname, OPR='start' )
       CALL log_push( subname )
       !
       IF ( .NOT. alloc ) CALL errore(subname,'windows module not allocated',1)
       IF ( nkpts_g <= 0)   CALL errore(subname,'Invalid nkpts_g',ABS(nkpts_g)+1)
       IF ( nbnd <= 0)    CALL errore(subname,'Invalid nbnd',ABS(nbnd)+1)
       IF ( SIZE(eig_,1) /= nbnd ) CALL errore(subname,'Invalid EIG size1',ABS(nbnd)+1)
       IF ( SIZE(eig_,2) /= nkpts_g ) CALL errore(subname,'Invalid EIG size2',ABS(nkpts_g)+1)
      
!
! ... windows dimensions
!
       kpoints_loop: &
       DO ik_g = 1,nkpts_g

          !
          ! Check which eigenvalues fall within the outer energy window
          !
          IF ( eig_(1,ik_g) > win_max .OR. eig_(nbnd,ik_g) < win_min ) &
               CALL errore(subname, 'energy window contains no eigenvalues ',1)

          !
          ! iwin_min/iwin_max are either 0 (do the
          ! standard initizalization) or > 0 if set from input
          !
          imin(ik_g) = iwin_min( ik_g )
          imax(ik_g) = iwin_max( ik_g )
          !
          lhave_min = ( imin( ik_g ) /= 0 )
          lhave_max = ( imax( ik_g ) /= 0 )
          !
          DO i = 1, nbnd
              !
              IF ( .NOT. lhave_min ) THEN
                  !
                  IF ( ( eig_(i,ik_g) >= win_min ) .AND. ( eig_(i,ik_g) <= win_max )) THEN
                      !
                      imin(ik_g) = i
                      lhave_min = .TRUE.
                      !
                  ENDIF
                  !
              ENDIF
              !
              IF ( .NOT. lhave_max ) THEN
                  !
                  IF ( eig_(i,ik_g) <= win_max ) imax(ik_g) = i
                  !
              ENDIF
              !
          ENDDO
          !
          dimwin(ik_g) = imax(ik_g) -imin(ik_g) +1       
          !
          IF ( dimwin(ik_g) < dimwann)     CALL errore(subname,'dimwin < dimwann', ik_g )
          IF ( dimwin(ik_g) > nbnd)        CALL errore(subname,'dimwin > nbnd', ik_g )
          IF ( imax(ik_g)   < imin(ik_g) ) CALL errore(subname,'imax < imin',ik_g)
          IF ( imin(ik_g)   < 1 )          CALL errore(subname,'imin < 1',ik_g)
          !
       ENDDO kpoints_loop


!
! ... frozen states
!
       dimwinx = MAXVAL( dimwin(:) )
       !
       ALLOCATE( dimfroz(nkpts_g), STAT=ierr )
       IF ( ierr/=0 ) CALL errore(subname,'allocating dimfroz',ABS(ierr))
       !
       ALLOCATE( indxfroz(dimwinx,nkpts_g), STAT=ierr )
       IF ( ierr/=0 ) CALL errore(subname,'allocating indxfroz',ABS(ierr))
       !
       ALLOCATE( indxnfroz(dimwinx,nkpts_g), STAT=ierr )
       IF ( ierr/=0 ) CALL errore(subname,'allocating indxnfroz',ABS(ierr))
       !
       ALLOCATE( frozen(dimwinx,nkpts_g), STAT=ierr )
       IF ( ierr/=0 ) CALL errore(subname,'allocating frozen',ABS(ierr))
       !

       !
       ! set vars for frozen states
       !
       lfrozen = .FALSE.
       !
       kpoints_frozen_loop: &
       DO ik_g = 1, nkpts_g
          !
          frozen(:,ik_g) = .FALSE.
          ! 
          kifroz_min = ifroz_min( ik_g ) 
          kifroz_max = ifroz_max( ik_g )
          !
          lhave_min = ( ifroz_min( ik_g ) /=  0 )
          lhave_max = ( ifroz_max( ik_g ) /= -1 )

          !
          ! Note that the above obeys kifroz_max-kifroz_min+1=kdimfroz=0,
          ! as required
          !
          DO i = imin(ik_g), imax(ik_g)
              !
              IF ( .NOT. lhave_min ) THEN
                  !
                  IF ( ( eig_(i,ik_g) >= froz_min ).AND.( eig_(i,ik_g) <= froz_max )) THEN
                      !
                      kifroz_min = i
                      lhave_min = .TRUE.
                      !
                  ENDIF
                  !
              ENDIF
              !
              IF ( .NOT. lhave_max ) THEN 
                  !
                  IF ( eig_(i,ik_g) <= froz_max ) kifroz_max = i
                  !
              ENDIF
              !
          ENDDO
          !
          ! set these vales relative to the bottom of the outer window
          !
          kifroz_min = kifroz_min -imin(ik_g) + 1   
          kifroz_max = kifroz_max -imin(ik_g) + 1
    
          dimfroz(ik_g) = kifroz_max - kifroz_min + 1
          !
          IF ( dimfroz(ik_g) > dimwann ) CALL errore(subname,'dimfroz > dimwann',ik_g)


          !
          ! Generate index array for frozen states inside inner window
          ! 
          indxfroz(:,ik_g) = 0
          !
          IF ( dimfroz(ik_g) > 0 ) THEN
               !
               lfrozen = .TRUE.
               !
               DO i = 1, dimfroz(ik_g)
                   !
                   indxfroz(i,ik_g) = kifroz_min + i - 1
                   !
                   frozen( indxfroz(i,ik_g), ik_g ) = .TRUE.
                   !
               ENDDO
               !
               IF ( indxfroz(dimfroz(ik_g),ik_g) /= kifroz_max ) &
                   CALL errore(subname,'wrong number of frozen states',ik_g )
          ENDIF
   
          !
          ! Generate index array for non-frozen states
          !
          idum = 0
          indxnfroz(:,ik_g) = 0
          !
          DO i = 1, dimwin(ik_g)
              !
              IF( .NOT. frozen(i,ik_g) ) THEN
                  !
                  idum = idum + 1
                  indxnfroz(idum,ik_g) = i
                  !
              ENDIF
              !
          ENDDO
          !
          IF ( idum /= dimwin(ik_g)-dimfroz(ik_g) )  &
              CALL errore(subname, 'wrong number of non-frozen states', ik_g)

       ENDDO kpoints_frozen_loop   
       !
       CALL timing ( subname, OPR='stop' )
       CALL log_pop ( subname )
       !
   END SUBROUTINE windows_init


!**********************************************************
   SUBROUTINE windows_allocate()
   !**********************************************************
   IMPLICIT NONE
       CHARACTER(16)      :: subname="windows_allocate"
       INTEGER            :: ierr

       CALL log_push ( subname )
       !
       IF ( nbnd <= 0 .OR. nkpts_g <= 0 ) &
           CALL errore(subname,'Invalid NBND or NKPTS_G ',1)
       IF ( nspin /= 1 .AND. nspin /=2 ) CALL errore(subname,'Invalid NSPIN',ABS(nspin)+1)
       !
       ALLOCATE( dimwin(nkpts_g), STAT=ierr )
       IF ( ierr/=0 ) CALL errore(subname,'allocating dimwin', ABS(ierr) )
       !
       ALLOCATE( imin(nkpts_g), STAT=ierr )
       IF ( ierr/=0 ) CALL errore(subname,'allocating imin', ABS(ierr) )      
       !
       ALLOCATE( imax(nkpts_g), STAT=ierr )
       IF ( ierr/=0 ) CALL errore(subname,'allocating imax', ABS(ierr) )      
       !
       ALLOCATE( eig(nbnd,nkpts_g), STAT=ierr )
       IF ( ierr/=0 ) CALL errore(subname,'allocating eig', ABS(ierr) )

       alloc = .TRUE.
       !
       CALL log_pop ( subname )
       !
   END SUBROUTINE windows_allocate


!**********************************************************
   SUBROUTINE windows_deallocate()
   !**********************************************************
   IMPLICIT NONE
       CHARACTER(18)      :: subname="windows_deallocate"
       INTEGER            :: ierr

       CALL log_push ( subname )
       !
       IF ( ALLOCATED(dimwin) ) THEN
            DEALLOCATE(dimwin, STAT=ierr)
            IF (ierr/=0)  CALL errore(subname,'deallocating dimwin',ABS(ierr))
       ENDIF
       IF ( ALLOCATED(imin) ) THEN
            DEALLOCATE(imin, STAT=ierr)
            IF (ierr/=0)  CALL errore(subname,'deallocating imin',ABS(ierr))
       ENDIF
       IF ( ALLOCATED(imax) ) THEN
            DEALLOCATE(imax, STAT=ierr)
            IF (ierr/=0)  CALL errore(subname,'deallocating imax',ABS(ierr))
       ENDIF
       IF ( ALLOCATED(eig) ) THEN
            DEALLOCATE(eig, STAT=ierr)
            IF (ierr/=0)  CALL errore(subname,'deallocating eig',ABS(ierr))
       ENDIF
       IF ( ALLOCATED(dimfroz) ) THEN
            DEALLOCATE(dimfroz, STAT=ierr)
            IF (ierr/=0)  CALL errore(subname,'deallocating dimfroz',ABS(ierr))
       ENDIF
       IF ( ALLOCATED(indxfroz) ) THEN
            DEALLOCATE(indxfroz, STAT=ierr)
            IF (ierr/=0)  CALL errore(subname,'deallocating indxfroz',ABS(ierr))
       ENDIF
       IF ( ALLOCATED(indxnfroz) ) THEN
            DEALLOCATE(indxnfroz, STAT=ierr)
            IF (ierr/=0)  CALL errore(subname,'deallocating indxnfroz',ABS(ierr))
       ENDIF
       IF ( ALLOCATED(frozen) ) THEN
            DEALLOCATE(frozen, STAT=ierr)
            IF (ierr/=0)  CALL errore(subname,'deallocating frozen',ABS(ierr))
       ENDIF
       !
       alloc = .FALSE.
       !
       CALL log_pop ( subname )
       !
   END SUBROUTINE windows_deallocate


!**********************************************************
   REAL(dbl) FUNCTION windows_memusage()
   !**********************************************************
   IMPLICIT NONE
       !
       REAL(dbl) :: cost
       !
       cost = ZERO
       IF ( ALLOCATED(dimwin) )    cost = cost + REAL(SIZE(dimwin))       * 4.0_dbl
       IF ( ALLOCATED(imin) )      cost = cost + REAL(SIZE(imin))         * 4.0_dbl
       IF ( ALLOCATED(imax) )      cost = cost + REAL(SIZE(imax))         * 4.0_dbl
       IF ( ALLOCATED(eig) )       cost = cost + REAL(SIZE(eig))          * 8.0_dbl
       IF ( ALLOCATED(dimfroz) )   cost = cost + REAL(SIZE(dimfroz))      * 4.0_dbl
       IF ( ALLOCATED(indxnfroz) ) cost = cost + REAL(SIZE(indxnfroz))    * 4.0_dbl
       IF ( ALLOCATED(frozen) )    cost = cost + REAL(SIZE(frozen))       * 4.0_dbl
       !
       windows_memusage = cost / 1000000.0_dbl
       !
   END FUNCTION windows_memusage


!**********************************************************
   SUBROUTINE windows_write(iun,tag)
   !**********************************************************
   IMPLICIT NONE
       !
       INTEGER,         INTENT(IN) :: iun
       CHARACTER(*),    INTENT(IN) :: tag
       !
       CHARACTER(13)      :: subname="windows_write"
       INTEGER            :: isp
       CHARACTER(nstrx)   :: attr

       !
       ! even if all the quantities are global, 
       ! every processor writes to a different file
       !
       IF ( .NOT. alloc ) RETURN
       !
       CALL timing( subname, OPR='start')
       CALL log_push ( subname )
       ! 
       isp = 0
       IF ( nspin == 2 .AND.   TRIM(spin_component) == "up" )   isp = 1
       IF ( nspin == 2 .AND. ( TRIM(spin_component) == "dw" .OR. &
                               TRIM(spin_component) == "down" )  ) isp = 2
       !
       IF ( ionode ) THEN
           !
           CALL iotk_write_begin(iun,TRIM(tag))
           CALL iotk_write_attr(attr,"nbnd",nbnd,FIRST=.TRUE.)
           CALL iotk_write_attr(attr,"nkpts",nkpts_g)
           !
           CALL iotk_write_attr(attr,"nspin",nspin)
           CALL iotk_write_attr(attr,"spin_component",TRIM(spin_component))
           CALL iotk_write_attr(attr,"efermi",efermi)
           CALL iotk_write_attr(attr,"dimwinx",dimwinx)
           CALL iotk_write_attr(attr,"lfrozen",lfrozen)
           CALL iotk_write_empty(iun,"DATA",ATTR=attr)
           !
           CALL iotk_write_dat(iun,"DIMWIN",dimwin, COLUMNS=8)
           CALL iotk_write_dat(iun,"IMIN",imin, COLUMNS=8)
           CALL iotk_write_dat(iun,"IMAX",imax, COLUMNS=8)
           !
           IF ( nspin == 2 ) THEN
               !
               CALL iotk_write_end(iun,"SPIN"//TRIM(iotk_index(isp)) )
               !
           ENDIF
           !
           CALL iotk_write_dat(iun,"EIG",eig, COLUMNS=4)
           !
           IF ( nspin == 2 ) THEN
               !
               CALL iotk_write_end(iun,"SPIN"//TRIM(iotk_index(isp)) )
               !
           ENDIF
           !
           CALL iotk_write_dat(iun,"DIMFROZ",    dimfroz, COLUMNS=8)
           CALL iotk_write_dat(iun,"INDXFROZ",  indxfroz, COLUMNS=8)
           CALL iotk_write_dat(iun,"INDXNFROZ",indxnfroz, COLUMNS=8)
           CALL iotk_write_dat(iun,"FROZEN",      frozen, COLUMNS=8)
           !
           CALL iotk_write_end(iun,TRIM(tag))
           !
       ENDIF
       !
       CALL timing( subname, OPR='stop')
       CALL log_pop ( subname )
       !
   END SUBROUTINE windows_write
   

!**********************************************************
   SUBROUTINE windows_read(iun,tag,found)
   !**********************************************************
   IMPLICIT NONE
       INTEGER,           INTENT(in) :: iun
       CHARACTER(*),      INTENT(in) :: tag
       LOGICAL,           INTENT(out):: found
       !
       CHARACTER(nstrx)   :: attr
       CHARACTER(12)      :: subname="windows_read"
       INTEGER            :: isp, nkpts_g_, ierr
       LOGICAL            :: lfound

       CALL timing( subname, OPR='start')
       CALL log_push ( subname )
       !
       IF ( alloc ) CALL windows_deallocate()
       ! 
       IF ( ionode ) THEN
           !
           CALL iotk_scan_begin(iun,TRIM(tag),FOUND=found,IERR=ierr)
           !
       ENDIF
       !
       CALL mp_bcast( found,    ionode_id )       
       CALL mp_bcast( ierr,     ionode_id )       
       !
       IF (.NOT. found) RETURN
       IF (ierr>0)  CALL errore(subname,'Wrong format in tag '//TRIM(tag),ierr)
       found = .TRUE.
       !
       IF ( ionode ) THEN
           !
           CALL iotk_scan_empty(iun,'DATA',ATTR=attr,IERR=ierr)
           IF (ierr/=0) CALL errore(subname,'Unable to find tag DATA',ABS(ierr))
           CALL iotk_scan_attr(attr,'nbnd',nbnd,IERR=ierr)
           IF (ierr/=0) CALL errore(subname,'Unable to find attr NBND',ABS(ierr))
           CALL iotk_scan_attr(attr,'nkpts',nkpts_g_,IERR=ierr)
           IF (ierr/=0) CALL errore(subname,'Unable to find attr NKPTS',ABS(ierr))
           !
           CALL iotk_scan_attr(attr,'nspin',nspin,IERR=ierr)
           IF (ierr/=0) CALL errore(subname,'Unable to find attr NSPIN',ABS(ierr))
           CALL iotk_scan_attr(attr,'spin_component',spin_component,IERR=ierr)
           IF (ierr/=0) CALL errore(subname,'Unable to find attr SPIN_COMPONENT',ABS(ierr))
           CALL iotk_scan_attr(attr,'efermi',efermi,IERR=ierr)
           IF (ierr/=0) CALL errore(subname,'Unable to find attr EFERMI',ABS(ierr))
           CALL iotk_scan_attr(attr,'dimwinx',dimwinx,IERR=ierr)
           IF (ierr/=0) CALL errore(subname,'Unable to find attr DIMWINX',ABS(ierr))
           !
           CALL iotk_scan_attr(attr,'lfrozen',lfrozen, FOUND=lfound, IERR=ierr)
           IF (ierr>0) CALL errore(subname,'Unable to find attr LFROZEN',ABS(ierr))
           IF ( .NOT. lfound ) lfrozen = .FALSE.
           !
       ENDIF
       !
       CALL mp_bcast( found,               ionode_id)
       CALL mp_bcast( nbnd,                ionode_id)
       CALL mp_bcast( nkpts_g_,            ionode_id)
       CALL mp_bcast( nspin,               ionode_id)
       CALL mp_bcast( spin_component,      ionode_id)
       CALL mp_bcast( efermi,              ionode_id)
       CALL mp_bcast( dimwinx,             ionode_id)
       CALL mp_bcast( lfrozen,             ionode_id)

       isp = 0
       IF ( nspin == 2 .AND.   TRIM(spin_component) == "up" )   isp = 1
       IF ( nspin == 2 .AND. ( TRIM(spin_component) == "dw" .OR. &
                               TRIM(spin_component) == "down" )  ) isp = 2

       IF ( kpoints_alloc ) THEN
            IF ( nkpts_g_ /= nkpts_g ) CALL errore(subname,'Invalid NKPTS_G',ABS(nkpts_g_)+1)
       ELSE
            nkpts_g = nkpts_g_
       ENDIF
       !
       CALL windows_allocate()
       !
       ALLOCATE( dimfroz(nkpts_g), STAT=ierr )
       IF ( ierr/=0 ) CALL errore(subname,'allocating dimfroz',ABS(ierr))
       !
       ALLOCATE( indxfroz(dimwinx,nkpts_g), STAT=ierr )
       IF ( ierr/=0 ) CALL errore(subname,'allocating indxfroz',ABS(ierr))
       !
       ALLOCATE( indxnfroz(dimwinx,nkpts_g), STAT=ierr )
       IF ( ierr/=0 ) CALL errore(subname,'allocating indxnfroz',ABS(ierr))
       !
       ALLOCATE( frozen(dimwinx,nkpts_g), STAT=ierr )
       IF ( ierr/=0 ) CALL errore(subname,'allocating frozen',ABS(ierr))

       IF ( ionode ) THEN
           !
           CALL iotk_scan_dat(iun,'DIMWIN',dimwin,IERR=ierr)
           IF (ierr/=0) CALL errore(subname,'Unable to find DIMWIN',ABS(ierr))
           CALL iotk_scan_dat(iun,'IMIN',imin,IERR=ierr)
           IF (ierr/=0) CALL errore(subname,'Unable to find IMIN',ABS(ierr))
           CALL iotk_scan_dat(iun,'IMAX',imax,IERR=ierr)
           IF (ierr/=0) CALL errore(subname,'Unable to find IMAX',ABS(ierr))
           !
           IF ( nspin == 2 ) THEN
               !
               CALL iotk_scan_begin(iun,'SPIN'//TRIM(iotk_index(isp)), IERR=ierr )
               IF (ierr/=0) CALL errore(subname,'Unable to find SPIN'// &
                                                 TRIM(iotk_index(isp)),ABS(ierr))
               !
           ENDIF
           !
           CALL iotk_scan_dat(iun,'EIG',eig,IERR=ierr)
           IF (ierr/=0) CALL errore(subname,'Unable to find EIG',ABS(ierr))
           !
           IF ( nspin == 2 ) THEN
               !
               CALL iotk_scan_end(iun,'SPIN'//TRIM(iotk_index(isp)), IERR=ierr )
               IF (ierr/=0) CALL errore(subname,'Unable to find end of SPIN'// & 
                                                 TRIM(iotk_index(isp)),ABS(ierr))
               !
           ENDIF
           !
           CALL iotk_scan_dat(iun,'DIMFROZ',dimfroz, FOUND=lfound ,IERR=ierr)
           IF (ierr>0) CALL errore(subname,'Unable to find DIMFROZ',ABS(ierr))
           IF ( .NOT. lfound ) dimfroz = 0
           !
           CALL iotk_scan_dat(iun,'INDXFROZ',indxfroz,IERR=ierr)
           IF (ierr>0) CALL errore(subname,'Unable to find INDXFROZ',ABS(ierr))
           IF ( .NOT. lfound ) indxfroz = 0
           !
           CALL iotk_scan_dat(iun,'INDXNFROZ',indxnfroz,IERR=ierr)
           IF (ierr>0) CALL errore(subname,'Unable to find INDXNFROZ',ABS(ierr))
           IF ( .NOT. lfound ) indxnfroz = 0
           !
           CALL iotk_scan_dat(iun,'FROZEN',frozen,IERR=ierr)
           IF (ierr>0) CALL errore(subname,'Unable to find FROZEN',ABS(ierr))
           IF ( .NOT. lfound ) frozen = .FALSE.

           CALL iotk_scan_end(iun,TRIM(tag),IERR=ierr)
           IF (ierr/=0)  CALL errore(subname,'Unable to end tag '//TRIM(tag),ABS(ierr))
           !
       ENDIF
       !
       CALL mp_bcast( dimwin,              ionode_id)
       CALL mp_bcast( imin,                ionode_id)
       CALL mp_bcast( imax,                ionode_id)
       CALL mp_bcast( eig,                 ionode_id)
       CALL mp_bcast( dimfroz,             ionode_id)
       CALL mp_bcast( indxfroz,            ionode_id)
       CALL mp_bcast( indxnfroz,           ionode_id)
       CALL mp_bcast( frozen,              ionode_id)

       !
       ! set the auxiliary quantities IKE, IKS, ISPIN
       !
       CALL windows_setspin( spin_component, nspin, ispin )

       !
       ! close
       CALL timing( subname, OPR='stop')
       CALL log_pop ( subname )
       !
   END SUBROUTINE windows_read


!**********************************************************
   SUBROUTINE windows_read_ext( filefmt )
   !**********************************************************
   IMPLICIT NONE
       CHARACTER(*),      INTENT(in) :: filefmt
       CHARACTER(16)      :: subname="windows_read_ext"
       CHARACTER(nstrx)   :: str
       INTEGER            :: lnkpts, ierr, ik
       REAL(dbl)          :: lefermi
       REAL(dbl), ALLOCATABLE :: leig(:,:,:)
       !
#ifdef __ETSF_IO
       TYPE(etsf_electrons)                  :: electrons
       DOUBLE PRECISION,              TARGET :: fermi_energy
       DOUBLE PRECISION, ALLOCATABLE, TARGET :: eigenvalues(:,:,:)
#endif

       CALL timing ( subname, OPR='start' )
       CALL log_push ( subname )
       !
       IF ( alloc ) CALL windows_deallocate()
       !
       !
       SELECT CASE ( TRIM(filefmt) )
       !
       CASE ( 'qexml' )
            !
            IF ( ionode ) &
            CALL qexml_read_bands_info( NBND=nbnd, NUM_K_POINTS=lnkpts, &
                                        NSPIN=nspin, EF=lefermi, &
                                        NELEC=nelec, IERR=ierr )
            !
            CALL mp_bcast( nbnd,    ionode_id )
            CALL mp_bcast( lnkpts,  ionode_id )
            CALL mp_bcast( nspin,   ionode_id )
            CALL mp_bcast( lefermi, ionode_id )
            CALL mp_bcast( nelec,   ionode_id )
            CALL mp_bcast( ierr,    ionode_id )
            !
       CASE ( 'pw_export' )
            !
            CALL qexpt_read_bands( NBND=nbnd, NUM_K_POINTS=lnkpts, &
                                   NSPIN=nspin, EF=lefermi, &
                                   NELEC=nelec, IERR=ierr )
            !
            CALL mp_bcast( nbnd,    ionode_id )
            CALL mp_bcast( lnkpts,  ionode_id )
            CALL mp_bcast( nspin,   ionode_id )
            CALL mp_bcast( lefermi, ionode_id )
            CALL mp_bcast( nelec,   ionode_id )
            CALL mp_bcast( ierr,    ionode_id )
            !
       CASE ( 'etsf_io' )
            !
#ifdef __ETSF_IO
            !
            nbnd   = dims%max_number_of_states  
            lnkpts = dims%number_of_kpoints
            !
            nspin  = 1
            IF ( dims%number_of_spins == 2 ) nspin = 2
            IF ( dims%number_of_spinor_components == 2 ) nspin = 4
            !
            ! Fermi energy will be read in the next section
            lefermi = 0.0  
            ierr   = 0
            !
#else
            CALL errore(subname,'ETSF_IO not configured',10)
#endif
            !
       CASE DEFAULT
            !
            CALL errore(subname,'invalid filefmt = '//TRIM(filefmt), 1)
       END SELECT
       !
       IF ( ierr/=0) CALL errore(subname,'getting bands dimensions',ABS(ierr))
       IF ( nkpts_g /= lnkpts ) CALL errore(subname,'invalid nkpts on read',2)
       !

       !
       ! ... allocating windows
       CALL windows_allocate()
       !
       ALLOCATE( leig( nbnd, nkpts_g, nspin), STAT=ierr )
       IF (ierr/=0) CALL errore(subname, 'allocating LEIG', ABS(ierr))
       !
       leig (:,:,:) = ZERO

       !
       ! setting the auxiliary quantity ISPIN
       !
       CALL windows_setspin( spin_component, nspin, ispin )


       !
       ! read data
       !
       SELECT CASE ( TRIM(filefmt) )
       !
       CASE ( 'qexml' )
            !
            !
            IF ( nspin == 1 ) THEN
                !
                DO ik = 1, nkpts_g
                   ! 
                   IF ( ionode ) &
                   CALL qexml_read_bands( IK=ik, EIG=leig(1:nbnd, ik, 1), &
                                          ENERGY_UNITS=str, IERR=ierr )
                   !
                   CALL mp_bcast( leig(1:nbnd, ik, 1),  ionode_id )
                   CALL mp_bcast( str,    ionode_id )
                   CALL mp_bcast( ierr,   ionode_id )
                   !
                   IF ( ierr/=0 ) CALL errore(subname,'QEXML reading bands I',ik)
                   !
                ENDDO
                !
            ELSE
                !
                DO ik = 1, nkpts_g
                   ! 
                   IF ( ionode ) &
                   CALL qexml_read_bands( IK=ik, ISPIN=ispin, EIG=leig(1:nbnd, ik, ispin), &
                                          ENERGY_UNITS=str, IERR=ierr )
                   !
                   CALL mp_bcast( leig(1:nbnd, ik, ispin),  ionode_id )
                   CALL mp_bcast( str,    ionode_id )
                   CALL mp_bcast( ierr,   ionode_id )
                   !
                   IF ( ierr/=0 ) CALL errore(subname,'QEXML reading bands II',ik)
                   !
                ENDDO
                !
            ENDIF
            !
            !
       CASE ( 'pw_export' )
            !
            IF (ionode) CALL qexpt_read_bands( EIG_S=leig, ENERGY_UNITS=str, IERR=ierr )
            !
            CALL mp_bcast( leig,   ionode_id )
            CALL mp_bcast( str,    ionode_id )
            CALL mp_bcast( ierr,   ionode_id )
            !
            IF ( ierr/=0 ) CALL errore(subname,'QEXPT: reading bands',10)
            !
       CASE ( 'etsf_io' )
            !
#ifdef __ETSF_IO
            !
            ALLOCATE( eigenvalues(dims%max_number_of_states, &
                                  dims%number_of_kpoints,    &   
                                  dims%number_of_spins )     )
            !
            fermi_energy = 0.0
            IF ( read_efermi ) electrons%fermi_energy  => fermi_energy 
            !
            electrons%eigenvalues%data3d      => eigenvalues
            !
            IF ( ionode ) CALL etsf_io_electrons_get(ncid, electrons, lstat, error_data)
            !
            electrons%fermi_energy            => null() 
            electrons%eigenvalues%data3d      => null()
            !
            CALL mp_bcast( fermi_energy,  ionode_id )
            CALL mp_bcast( eigenvalues,   ionode_id )
            CALL mp_bcast( lstat,         ionode_id )
            !
            IF ( .NOT. lstat ) CALL etsf_error(error_data,subname,'ETSF_IO: reading bands',10)
            !
            str = "Hartree"
            !
            leig( 1:nbnd, 1:nkpts_g, 1:nspin) = eigenvalues(:,:,:)
            lefermi = fermi_energy
            !
            DEALLOCATE( eigenvalues )
            !
#endif
            !
       CASE DEFAULT
            !
            CALL errore(subname,'invalid filefmt = '//TRIM(filefmt), 1)
            !
       END SELECT

       !
       ! check energy units
       !
       CALL change_case(str,'lower')
       !
       SELECT CASE ( TRIM(str) )
       CASE ( 'rydberg', 'ryd', 'ry' )
           !
           leig(:,:,:) = leig(:,:,:) * RYD
           lefermi     = lefermi * RYD
           !
       CASE ( 'hartree', 'ha')
           !
           leig(:,:,:) = leig(:,:,:) * TWO * RYD
           lefermi     = lefermi * TWO * RYD
           !
       CASE ( 'elettronvolt', 'elettron-volt', 'ev')
           !
           ! do nothing
           !
       CASE DEFAULT
           CALL errore(subname,'Wrong units in Energies',5)
       END SELECT
 
       !
       ! check whether fermi energy is read from dftdata_file
       !
       IF ( read_efermi ) efermi = lefermi

       !
       ! define EIG, which contains only the kpts related to the current pool
       !
       eig(:,1:nkpts_g) = leig(:, 1:nkpts_g, ispin)
       !

       DEALLOCATE( leig, STAT=ierr)
       IF (ierr/=0) CALL errore(subname, 'deallocating LEIG', ABS(ierr))

       CALL timing ( subname, OPR='stop' )
       CALL log_pop ( subname )
       !
   END SUBROUTINE windows_read_ext


!**********************************************************
   SUBROUTINE windows_setspin( spin_component, nspin, ispin )
   !**********************************************************
   IMPLICIT NONE
       CHARACTER(*),  INTENT(IN)  :: spin_component
       INTEGER,       INTENT(IN)  :: nspin
       INTEGER,       INTENT(OUT) :: ispin
       !
       CHARACTER(17)  :: subname="windows_setspin"

       !
       !
       ! setting the auxiliary quantity ISPIN
       !
       SELECT CASE ( nspin ) 
       CASE( 1 )
            !
            ispin = 1
            !
            IF ( TRIM(spin_component) /= 'none' ) &
                 CALL errore(subname,'Invalid spin component = '//TRIM(spin_component),1 )
            !
       CASE( 2 )
            !
            SELECT CASE ( TRIM(spin_component) )
            CASE ( 'up' )
                !
                ispin = 1
                !
            CASE ( 'down', 'dw' )
                !
                ispin = 2
                !
            CASE DEFAULT
                CALL errore(subname,'Invalid spin component = '//TRIM(spin_component),2 )
            END SELECT
            !
       CASE DEFAULT
            CALL errore(subname,'Invalid nspin',ABS(nspin)+1 )
       END SELECT
   
   END SUBROUTINE windows_setspin

END MODULE windows_module

