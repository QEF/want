!
! Copyright (C) 2004 Andrea Ferretti
!
! This file is distributed under the terms of the
! GNU General Public License. See the file `License\'
! in the root directory of the present distribution,
! or http://www.gnu.org/copyleft/gpl.txt .
!

!*********************************************
   MODULE subspace_module
!*********************************************
   USE kinds, ONLY : dbl
   USE windows_module, ONLY : dimwin, nbnd, lcompspace, windows_alloc => alloc, &
                              efermi, windows_allocate
   USE kpoints_module, ONLY : nkpts, kpoints_alloc
   USE input_module, ONLY : dimwann, input_alloc => alloc
   USE iotk_module
   USE parameters, ONLY : nstrx
   IMPLICIT NONE
   PRIVATE
   SAVE

! This module handles data referring to the definition of 
! the subspace spanned by the Wannier functions which will
! be computed
!
! routines in this module:
! SUBROUTINE subspace_allocate()
! SUBROUTINE subspace_deallocate()
! SUBROUTINE subspace_write(unit,name)
! SUBROUTINE subspace_read(unit,name,found)

!
! declarations of common variables
!   

   !
   ! ... the hamiltonian eigs in the final subspace
   REAL(dbl),    ALLOCATABLE   :: wan_eig(:,:)       ! the eigenvalues in the new subspace
   !
   ! ... rotations defining the chosen subspace
   COMPLEX(dbl), ALLOCATABLE   :: lamp(:,:,:)        ! nbnd, nbnd, nkpts
   COMPLEX(dbl), ALLOCATABLE   :: camp(:,:,:)        ! equal
   COMPLEX(dbl), ALLOCATABLE   :: eamp(:,:,:)        ! equal
   COMPLEX(dbl), ALLOCATABLE   :: comp_eamp(:,:,:)   ! equal
   !
   ! NOTE: the second dimension should be DIMWANN instead of NBND but, 
   !       for coherence with the old notation about frozen states 
   !       (the total number of states is DIMWANN + DIMFROZ_max) matrixes are
   !       overallocated.
   !
   COMPLEX(dbl), ALLOCATABLE   :: mtrx_in(:,:,:)   ! equal
   COMPLEX(dbl), ALLOCATABLE   :: mtrx_out(:,:,:)  ! equal
   
   LOGICAL :: alloc = .FALSE.

!
! end of declarations
!

   PUBLIC :: nkpts, nbnd     
   PUBLIC :: dimwann
   PUBLIC :: wan_eig, efermi
   PUBLIC :: lamp, camp, eamp, comp_eamp
   PUBLIC :: mtrx_in, mtrx_out
   PUBLIC :: alloc

   PUBLIC :: subspace_allocate
   PUBLIC :: subspace_deallocate
   PUBLIC :: subspace_write
   PUBLIC :: subspace_read

CONTAINS

!
! subroutines
!

!**********************************************************
   SUBROUTINE subspace_allocate()
   !**********************************************************
   IMPLICIT NONE
       CHARACTER(17)      :: subname="subspace_allocate"
       INTEGER            :: ierr 
      
       IF ( nbnd <= 0 .OR. nkpts <= 0 ) &
           CALL errore(subname,' Invalid NBND or NKPTS ',1)
       IF ( dimwann <= 0 ) CALL errore(subname,' Invalid DIMWANN ',ABS(dimwann)+1)

       ALLOCATE( wan_eig(dimwann,nkpts), STAT=ierr )
           IF ( ierr/=0 ) CALL errore(subname,' allocating wan_eig ',dimwann*nkpts)

       ALLOCATE( lamp(nbnd,nbnd,nkpts), STAT = ierr )
           IF( ierr /=0 ) CALL errore(subname, ' allocating lamp ', nbnd*nbnd*nkpts )
       ALLOCATE( camp(nbnd,nbnd,nkpts), STAT = ierr )
           IF( ierr /=0 ) CALL errore(subname, ' allocating camp ', nbnd*nbnd*nkpts )
       ALLOCATE( eamp(nbnd,nbnd,nkpts), STAT = ierr )
           IF( ierr /=0 ) CALL errore(subname, ' allocating eamp ', nbnd*nbnd*nkpts )
       ALLOCATE( comp_eamp(nbnd,nbnd,nkpts), STAT = ierr )
           IF( ierr /=0 ) CALL errore(subname, ' allocating comp_eamp', nbnd*nbnd*nkpts)

       ALLOCATE( mtrx_in(nbnd,nbnd,nkpts), STAT = ierr )
           IF( ierr /=0 ) CALL errore(subname, ' allocating mtrx_in ',nbnd*nbnd*nkpts )
       ALLOCATE( mtrx_out(nbnd,nbnd,nkpts), STAT = ierr )
           IF( ierr /=0 ) CALL errore(subname, ' allocating mtrx_out ',nbnd*nbnd*nkpts )
       alloc = .TRUE.

   END SUBROUTINE subspace_allocate


!**********************************************************
   SUBROUTINE subspace_deallocate()
   !**********************************************************
   IMPLICIT NONE
       CHARACTER(19)      :: subname="subspace_deallocate"
       INTEGER            :: ierr 
      
       IF ( ALLOCATED(wan_eig) ) THEN 
            DEALLOCATE(wan_eig, STAT=ierr)
            IF (ierr/=0)  CALL errore(subname,' deallocating wan_eig ',ABS(ierr))
       ENDIF
       IF ( ALLOCATED(lamp) ) THEN 
            DEALLOCATE(lamp, STAT=ierr)
            IF (ierr/=0)  CALL errore(subname,' deallocating lamp ',ABS(ierr))
       ENDIF
       IF ( ALLOCATED(camp) ) THEN 
            DEALLOCATE(camp, STAT=ierr)
            IF (ierr/=0)  CALL errore(subname,' deallocating camp ',ABS(ierr))
       ENDIF
       IF ( ALLOCATED(eamp) ) THEN 
            DEALLOCATE(eamp, STAT=ierr)
            IF (ierr/=0)  CALL errore(subname,' deallocating eamp ',ABS(ierr))
       ENDIF
       IF ( ALLOCATED(comp_eamp) ) THEN 
            DEALLOCATE(comp_eamp, STAT=ierr)
            IF (ierr/=0)  CALL errore(subname,' deallocating comp_eamp ',ABS(ierr))
       ENDIF
       IF ( ALLOCATED(mtrx_in) ) THEN 
            DEALLOCATE(mtrx_in, STAT=ierr)
            IF (ierr/=0)  CALL errore(subname,' deallocating mtrx_in ',ABS(ierr))
       ENDIF
       IF ( ALLOCATED(mtrx_out) ) THEN 
            DEALLOCATE(mtrx_out, STAT=ierr)
            IF (ierr/=0)  CALL errore(subname,' deallocating mtrx_out ',ABS(ierr))
       ENDIF
       alloc = .FALSE.
   END SUBROUTINE subspace_deallocate


!**********************************************************
   SUBROUTINE subspace_write(unit,name)
   !**********************************************************
   IMPLICIT NONE
       INTEGER,         INTENT(in) :: unit
       CHARACTER(*),    INTENT(in) :: name
       CHARACTER(nstrx)   :: attr
       CHARACTER(14)      :: subname="subspace_write"

       IF ( .NOT. alloc ) RETURN
       IF ( .NOT. windows_alloc ) CALL errore(subname,'windows module not alloc',1)

       CALL iotk_write_begin(unit,TRIM(name))
       CALL iotk_write_attr(attr,"nbnd",nbnd,FIRST=.TRUE.) 
       CALL iotk_write_attr(attr,"nkpts",nkpts) 
       CALL iotk_write_attr(attr,"dimwann",dimwann) 
       CALL iotk_write_empty(unit,"DATA",ATTR=attr)

       CALL iotk_write_dat(unit,"DIMWIN",dimwin) 
       CALL iotk_write_dat(unit,"WAN_EIGENVALUES",wan_eig)
       CALL iotk_write_dat(unit,"LAMP",lamp)
       CALL iotk_write_dat(unit,"CAMP",camp)
       CALL iotk_write_dat(unit,"EAMP",eamp)
       IF ( lcompspace ) CALL iotk_write_dat(unit,"COMP_EAMP",comp_eamp)

       CALL iotk_write_end(unit,TRIM(name))
   END SUBROUTINE subspace_write

!**********************************************************
   SUBROUTINE subspace_read(unit,name,found)
   !**********************************************************
   IMPLICIT NONE
       INTEGER,           INTENT(in) :: unit
       CHARACTER(*),      INTENT(in) :: name
       LOGICAL,           INTENT(out):: found
       CHARACTER(nstrx)   :: attr
       CHARACTER(13)      :: subname="subspace_read"
       INTEGER            :: nkpts_, nbnd_, dimwann_
       INTEGER            :: ierr

       IF ( alloc ) CALL subspace_deallocate()

       CALL iotk_scan_begin(unit,TRIM(name),FOUND=found,IERR=ierr)
       IF (.NOT. found) RETURN
       IF (ierr>0)  CALL errore(subname,'Wrong format in tag '//TRIM(name),ierr)
       found = .TRUE.

       CALL iotk_scan_empty(unit,'DATA',ATTR=attr,IERR=ierr)
       IF (ierr/=0) CALL errore(subname,'Unable to find tag DATA',ABS(ierr))
       CALL iotk_scan_attr(attr,'nbnd',nbnd_,IERR=ierr)
       IF (ierr/=0) CALL errore(subname,'Unable to find attr NBND',ABS(ierr))
       CALL iotk_scan_attr(attr,'nkpts',nkpts_,IERR=ierr)
       IF (ierr/=0) CALL errore(subname,'Unable to find attr NKPTS',ABS(ierr))
       CALL iotk_scan_attr(attr,'dimwann',dimwann_,IERR=ierr)
       IF (ierr/=0) CALL errore(subname,'Unable to find attr dimwann',ABS(ierr))

       IF ( kpoints_alloc ) THEN
           IF ( nkpts_ /= nkpts ) CALL errore(subname,'Invalid NKPTS',ABS(nkpts-nkpts_))
       ELSE
           nkpts = nkpts_
       ENDIF
       IF ( windows_alloc ) THEN
           IF ( nbnd_ /=nbnd) CALL errore(subname,'Invalid NBND',ABS(nbnd-nbnd_))
       ELSE
           nbnd = nbnd_
           CALL windows_allocate()
       ENDIF
       IF ( input_alloc ) THEN
           IF ( dimwann_ /= dimwann) CALL errore(subname,'Invalid DIMWANN',ABS(dimwann_)+1)
       ELSE
           dimwann = dimwann_
       ENDIF

       !
       ! no check is done for dimwin
       CALL iotk_scan_dat(unit,'DIMWIN',dimwin,IERR=ierr)
       IF (ierr/=0) CALL errore(subname,'Unable to find tag DIMWIN',ABS(ierr))

       CALL subspace_allocate()
       CALL iotk_scan_dat(unit,'WAN_EIGENVALUES',wan_eig,IERR=ierr)
       IF (ierr/=0) CALL errore(subname,'Unable to find EIGENVALUES',ABS(ierr))
       CALL iotk_scan_dat(unit,'LAMP',lamp,IERR=ierr)
       IF (ierr/=0) CALL errore(subname,'Unable to find LAMP',ABS(ierr))
       CALL iotk_scan_dat(unit,'CAMP',camp,IERR=ierr)
       IF (ierr/=0) CALL errore(subname,'Unable to find CAMP',ABS(ierr))
       CALL iotk_scan_dat(unit,'EAMP',eamp,IERR=ierr)
       IF (ierr/=0) CALL errore(subname,'Unable to find EAMP',ABS(ierr))
       CALL iotk_scan_dat(unit,'COMP_EAMP',comp_eamp,IERR=ierr)
       IF (ierr/=0) CALL errore(subname,'Unable to find COMP_EAMP',ABS(ierr))

       CALL iotk_scan_end(unit,TRIM(name),IERR=ierr)
       IF (ierr/=0)  CALL errore(subname,'Unable to end tag '//TRIM(name),ABS(ierr))
   END SUBROUTINE subspace_read

END MODULE subspace_module
