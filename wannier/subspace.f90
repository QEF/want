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
   USE windows_module, ONLY : mxdbnd, nkpts
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
   ! ... the hamiltonian in the final subspace
   REAL(dbl),    POINTER       :: s_eig(:,:)         ! the eigenvalues in the new subspace
   COMPLEX(dbl), POINTER       :: ham(:,:,:)         ! the hamiltonian in the subspace
                                                     ! mxdbnd, mxdbnd, nkpts
   !
   ! ... rotations defining the chosen subspace
   COMPLEX(dbl), POINTER       :: lamp(:,:,:)        ! mxdbnd, mxdbnd, nkpts
   COMPLEX(dbl), POINTER       :: camp(:,:,:)        ! equal
   COMPLEX(dbl), POINTER       :: eamp(:,:,:)        ! equal
   COMPLEX(dbl), POINTER       :: eamp_save(:,:,:)        ! equal
   !
   ! NOTE: the second dimension should be DIMWANN instead of MXDBND but, 
   !       for coherence with the old notation about frozen states 
   !       (the total number of states is DIMWANN + DIMFROZ_max) matrixes are
   !       overallocated.
   !
   COMPLEX(dbl), POINTER       :: mtrx_in(:,:,:)   ! equal
   COMPLEX(dbl), POINTER       :: mtrx_out(:,:,:)  ! equal
   

!
! end of declarations
!

   PUBLIC :: nkpts, mxdbnd     
   PUBLIC :: ham, s_eig
   PUBLIC :: lamp, camp, eamp, eamp_save 
   PUBLIC :: mtrx_in, mtrx_out

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
      
       IF ( mxdbnd <= 0 .OR. nkpts <= 0 ) &
           CALL errore(subname,' Invalid MXDBND or NKPTS ',1)

       ALLOCATE( s_eig(mxdbnd,nkpts), STAT=ierr )
           IF ( ierr/=0 ) CALL errore(subname,' allocating s_eig ',mxdbnd*nkpts)
       ALLOCATE( ham(mxdbnd,mxdbnd,nkpts), STAT = ierr )
           IF( ierr /=0 ) CALL errore(subname, ' allocating ham ', mxdbnd*mxdbnd*nkpts )

       ALLOCATE( lamp(mxdbnd,mxdbnd,nkpts), STAT = ierr )
           IF( ierr /=0 ) CALL errore(subname, ' allocating lamp ', mxdbnd*mxdbnd*nkpts )
       ALLOCATE( camp(mxdbnd,mxdbnd,nkpts), STAT = ierr )
           IF( ierr /=0 ) CALL errore(subname, ' allocating camp ', mxdbnd*mxdbnd*nkpts )
       ALLOCATE( eamp(mxdbnd,mxdbnd,nkpts), STAT = ierr )
           IF( ierr /=0 ) CALL errore(subname, ' allocating eamp ', mxdbnd*mxdbnd*nkpts )
       ALLOCATE( eamp_save(mxdbnd,mxdbnd,nkpts), STAT = ierr )
           IF( ierr /=0 ) CALL errore(subname, ' allocating eamp_save', mxdbnd*mxdbnd*nkpts)

       ALLOCATE( mtrx_in(mxdbnd,mxdbnd,nkpts), STAT = ierr )
           IF( ierr /=0 ) CALL errore(subname, ' allocating mtrx_in ',mxdbnd*mxdbnd*nkpts )
       ALLOCATE( mtrx_out(mxdbnd,mxdbnd,nkpts), STAT = ierr )
           IF( ierr /=0 ) CALL errore(subname, ' allocating mtrx_out ',mxdbnd*mxdbnd*nkpts )

   END SUBROUTINE subspace_allocate


!**********************************************************
   SUBROUTINE subspace_deallocate()
   !**********************************************************
   IMPLICIT NONE
       CHARACTER(19)      :: subname="subspace_deallocate"
       INTEGER            :: ierr 
      
       IF ( ASSOCIATED(s_eig) ) THEN 
            DEALLOCATE(s_eig, STAT=ierr)
            IF (ierr/=0)  CALL errore(subname,' deallocating s_eig ',ABS(ierr))
       ENDIF
       IF ( ASSOCIATED(ham) ) THEN 
            DEALLOCATE(ham, STAT=ierr)
            IF (ierr/=0)  CALL errore(subname,' deallocating ham ',ABS(ierr))
       ENDIF
       IF ( ASSOCIATED(lamp) ) THEN 
            DEALLOCATE(lamp, STAT=ierr)
            IF (ierr/=0)  CALL errore(subname,' deallocating lamp ',ABS(ierr))
       ENDIF
       IF ( ASSOCIATED(camp) ) THEN 
            DEALLOCATE(camp, STAT=ierr)
            IF (ierr/=0)  CALL errore(subname,' deallocating camp ',ABS(ierr))
       ENDIF
       IF ( ASSOCIATED(eamp) ) THEN 
            DEALLOCATE(eamp, STAT=ierr)
            IF (ierr/=0)  CALL errore(subname,' deallocating eamp ',ABS(ierr))
       ENDIF
       IF ( ASSOCIATED(eamp_save) ) THEN 
            DEALLOCATE(eamp_save, STAT=ierr)
            IF (ierr/=0)  CALL errore(subname,' deallocating eamp_save ',ABS(ierr))
       ENDIF
       IF ( ASSOCIATED(mtrx_in) ) THEN 
            DEALLOCATE(mtrx_in, STAT=ierr)
            IF (ierr/=0)  CALL errore(subname,' deallocating mtrx_in ',ABS(ierr))
       ENDIF
       IF ( ASSOCIATED(mtrx_out) ) THEN 
            DEALLOCATE(mtrx_out, STAT=ierr)
            IF (ierr/=0)  CALL errore(subname,' deallocating mtrx_out ',ABS(ierr))
       ENDIF
   END SUBROUTINE subspace_deallocate


!**********************************************************
   SUBROUTINE subspace_write(unit,name)
   !**********************************************************
   IMPLICIT NONE
       INTEGER,         INTENT(in) :: unit
       CHARACTER(*),    INTENT(in) :: name
       CHARACTER(nstrx)   :: attr
       CHARACTER(14)      :: subname="subspace_write"

       CALL iotk_write_begin(unit,TRIM(name))
       CALL iotk_write_attr(attr,"mxdbnd",mxdbnd,FIRST=.TRUE.) 
       CALL iotk_write_attr(attr,"nkpts",nkpts) 
       CALL iotk_write_empty(unit,"DATA",ATTR=attr)

       CALL iotk_write_dat(unit,"EIGENVALUES",s_eig)
       CALL iotk_write_dat(unit,"HAMILTONIAN",ham)
       CALL iotk_write_dat(unit,"LAMP",lamp)
       CALL iotk_write_dat(unit,"CAMP",camp)
       CALL iotk_write_dat(unit,"EAMP",eamp)

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
       INTEGER            :: ierr

       CALL iotk_scan_begin(unit,TRIM(name),FOUND=found,IERR=ierr)
       IF (.NOT. found) RETURN
       IF (ierr>0)  CALL errore(subname,'Wrong format in tag '//TRIM(name),ierr)

       CALL iotk_scan_empty(unit,'DATA',ATTR=attr,IERR=ierr)
       IF (ierr/=0) CALL errore(subname,'Unable to find tag DATA',ABS(ierr))
       CALL iotk_scan_attr(attr,'mxdbnd',mxdbnd,IERR=ierr)
       IF (ierr/=0) CALL errore(subname,'Unable to find attr MXDBND',ABS(ierr))
       CALL iotk_scan_attr(attr,'nkpts',nkpts,IERR=ierr)
       IF (ierr/=0) CALL errore(subname,'Unable to find attr NKPTS',ABS(ierr))

       CALL subspace_allocate()
       CALL iotk_scan_dat(unit,'EIGENVALUES',s_eig,IERR=ierr)
       IF (ierr/=0) CALL errore(subname,'Unable to find EIGENVALUES',ABS(ierr))
       CALL iotk_scan_dat(unit,'HAMILTONIAN',ham,IERR=ierr)
       IF (ierr/=0) CALL errore(subname,'Unable to find HAMILTONIAN',ABS(ierr))
       CALL iotk_scan_dat(unit,'LAMP',lamp,IERR=ierr)
       IF (ierr/=0) CALL errore(subname,'Unable to find LAMP',ABS(ierr))
       CALL iotk_scan_dat(unit,'CAMP',camp,IERR=ierr)
       IF (ierr/=0) CALL errore(subname,'Unable to find CAMP',ABS(ierr))
       CALL iotk_scan_dat(unit,'EAMP',eamp,IERR=ierr)
       IF (ierr/=0) CALL errore(subname,'Unable to find EAMP',ABS(ierr))

       CALL iotk_scan_end(unit,TRIM(name),IERR=ierr)
       IF (ierr/=0)  CALL errore(subname,'Unable to end tag '//TRIM(name),ABS(ierr))
   END SUBROUTINE subspace_read

END MODULE subspace_module
