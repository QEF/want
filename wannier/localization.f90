!
! Copyright (C) 2004 WanT Group
!
! This file is distributed under the terms of the
! GNU General Public License. See the file `License\'
! in the root directory of the present distribution,
! or http://www.gnu.org/copyleft/gpl.txt .
!

!*********************************************
   MODULE localization_module
!*********************************************
   USE kinds, ONLY : dbl
   USE constants, ONLY : ZERO
   USE kpoints_module, ONLY : nkpts, kpoints_alloc
   USE subspace_module, ONLY : dimwann, subspace_alloc => alloc
   USE iotk_module
   USE parameters, ONLY : nstrx
   IMPLICIT NONE
   PRIVATE
   SAVE

! This module handles data referring to the Wannier Localization
! procedure. Particularly it contains the U(k) unitary matrices
! which rotate the bloch wfc in order to minimize the spread functional.
!
! routines in this module:
! SUBROUTINE localization_allocate()
! SUBROUTINE localization_deallocate()
! SUBROUTINE localization_write(unit,name)
! SUBROUTINE localization_read(unit,name,found)

!
! declarations of common variables
!   

   !
   ! ... some dimensions are taken from other moduli
   !     INTEGER :: dimwann
   !     INTEGER :: nkpts
   !
   ! ... iterative localization procedure parameters
   INTEGER                     :: maxiter0_wan  ! maximun num of iterations (part1)
   INTEGER                     :: maxiter1_wan  ! maximun num of iterations (part2)
   INTEGER                     :: ncg           ! a CG step every ncg is performed in part2
   REAL(dbl)                   :: alpha0_wan    ! mixing factor in part1
   REAL(dbl)                   :: alpha1_wan    ! mixing factor in part2
   REAL(dbl)                   :: wannier_thr   ! convergence threshold

   ! ... unitary rotation matrices
   COMPLEX(dbl), ALLOCATABLE   :: cu(:,:,:)     ! dimwann, dimwann, nkpts
   
   ! ... <r>, <r>^2, <r^2> and spreads of the single WFs
   REAL(dbl), ALLOCATABLE      :: rave(:,:)     ! 3 * dimwann,   <r>  (Bohr)
   REAL(dbl), ALLOCATABLE      :: r2ave(:)      ! dimwann,     <r^2>  (Bohr^2)
   REAL(dbl), ALLOCATABLE      :: rave2(:)      ! dimwann,     <r>^2  (Bohr^2)

   !
   ! ... decomposition of the spread functional
   REAL(dbl)                   :: Omega_I       ! Invariant part of the spread
   REAL(dbl)                   :: Omega_OD      ! Off diagonal part
   REAL(dbl)                   :: Omega_D       ! Diagonal part
   REAL(dbl)                   :: Omega_V       ! Variant (tilde) part = Omega_D + Omega_OD
   REAL(dbl)                   :: Omega_tot     ! = Omega_I + Omega_D + Omega_OD
   
   LOGICAL :: alloc = .FALSE.

!
! end of declarations
!

   PUBLIC :: nkpts, dimwann
   PUBLIC :: wannier_thr, alpha0_wan, alpha1_wan, maxiter0_wan, maxiter1_wan, ncg
   PUBLIC :: cu
   PUBLIC :: rave, r2ave, rave2
   PUBLIC :: omega_I, omega_OD, omega_D, omega_V, omega_tot
   PUBLIC :: alloc

   PUBLIC :: localization_allocate
   PUBLIC :: localization_deallocate
   PUBLIC :: localization_write
   PUBLIC :: localization_read

CONTAINS

!
! subroutines
!

!**********************************************************
   SUBROUTINE localization_allocate()
   !**********************************************************
   IMPLICIT NONE
       CHARACTER(21)      :: subname="localization_allocate"
       INTEGER            :: ierr 
      
       IF ( dimwann <= 0 ) CALL errore(subname,' Invalid DIMWANN ',ABS(dimwann)+1)
       IF ( nkpts <= 0 )   CALL errore(subname,' Invalid NKPTS ',ABS(nkpts)+1)

       ALLOCATE( cu(dimwann,dimwann,nkpts), STAT = ierr )
           IF( ierr /=0 ) CALL errore(subname, ' allocating cu ',dimwann*dimwann*nkpts )
       ALLOCATE( rave(3,dimwann), STAT = ierr )
           IF( ierr /=0 ) CALL errore(subname, ' allocating rave ',3*dimwann )
       ALLOCATE( rave2(dimwann), STAT = ierr )
           IF( ierr /=0 ) CALL errore(subname, ' allocating rave2 ',dimwann )
       ALLOCATE( r2ave(dimwann), STAT = ierr )
           IF( ierr /=0 ) CALL errore(subname, ' allocating r2ave ',dimwann )
       
       Omega_I   = ZERO
       Omega_OD  = ZERO
       Omega_D   = ZERO  
       Omega_V   = ZERO 
       Omega_tot = ZERO
       alloc = .TRUE.

   END SUBROUTINE localization_allocate


!**********************************************************
   SUBROUTINE localization_deallocate()
   !**********************************************************
   IMPLICIT NONE
       CHARACTER(23)      :: subname="localization_deallocate"
       INTEGER            :: ierr 
      
       IF ( ALLOCATED(cu) ) THEN 
            DEALLOCATE(cu, STAT=ierr)
            IF (ierr/=0)  CALL errore(subname,' deallocating cu ',ABS(ierr))
       ENDIF
       IF ( ALLOCATED(rave) ) THEN 
            DEALLOCATE(rave, STAT=ierr)
            IF (ierr/=0)  CALL errore(subname,' deallocating rave ',ABS(ierr))
       ENDIF
       IF ( ALLOCATED(r2ave) ) THEN 
            DEALLOCATE(r2ave, STAT=ierr)
            IF (ierr/=0)  CALL errore(subname,' deallocating r2ave ',ABS(ierr))
       ENDIF
       IF ( ALLOCATED(rave2) ) THEN 
            DEALLOCATE(rave2, STAT=ierr)
            IF (ierr/=0)  CALL errore(subname,' deallocating rave2 ',ABS(ierr))
       ENDIF
       alloc = .FALSE.
   END SUBROUTINE localization_deallocate


!**********************************************************
   SUBROUTINE localization_write(unit,name)
   !**********************************************************
   IMPLICIT NONE
       INTEGER,         INTENT(in) :: unit
       CHARACTER(*),    INTENT(in) :: name
       CHARACTER(nstrx)   :: attr
       CHARACTER(18)      :: subname="localization_write"

       IF ( .NOT. alloc ) RETURN

       CALL iotk_write_begin(unit,TRIM(name))
       CALL iotk_write_attr(attr,"dimwann",dimwann,FIRST=.TRUE.) 
       CALL iotk_write_attr(attr,"nkpts",nkpts) 
       CALL iotk_write_empty(unit,"DATA",ATTR=attr)

       CALL iotk_write_attr(attr,"Omega_I",Omega_I,FIRST=.TRUE.) 
       CALL iotk_write_attr(attr,"Omega_D",Omega_D) 
       CALL iotk_write_attr(attr,"Omega_OD",Omega_OD) 
       CALL iotk_write_attr(attr,"Omega_V",Omega_V) 
       CALL iotk_write_attr(attr,"Omega_tot",Omega_tot) 
       CALL iotk_write_empty(unit,"SPREADS",ATTR=attr)

       CALL iotk_write_dat(unit,"CU",cu) 
       CALL iotk_write_dat(unit,"RAVE",rave,FMT="(3f20.11,2x)")
       CALL iotk_write_dat(unit,"RAVE2",rave2)
       CALL iotk_write_dat(unit,"R2AVE",r2ave)

       CALL iotk_write_end(unit,TRIM(name))
   END SUBROUTINE localization_write

!**********************************************************
   SUBROUTINE localization_read(unit,name,found)
   !**********************************************************
   IMPLICIT NONE
       INTEGER,           INTENT(in) :: unit
       CHARACTER(*),      INTENT(in) :: name
       LOGICAL,           INTENT(out):: found
       CHARACTER(nstrx)   :: attr
       CHARACTER(17)      :: subname="localization_read"
       INTEGER            :: nkpts_, dimwann_
       INTEGER            :: ierr

       IF ( alloc ) CALL localization_deallocate()

       CALL iotk_scan_begin(unit,TRIM(name),FOUND=found,IERR=ierr)
       IF (.NOT. found) RETURN
       IF (ierr>0)  CALL errore(subname,'Wrong format in tag '//TRIM(name),ierr)
       found = .TRUE.

       !
       ! ... dimensions
       CALL iotk_scan_empty(unit,'DATA',ATTR=attr,IERR=ierr)
       IF (ierr/=0) CALL errore(subname,'Unable to find tag DATA',ABS(ierr))

       CALL iotk_scan_attr(attr,'dimwann',dimwann_,IERR=ierr)
       IF (ierr/=0) CALL errore(subname,'Unable to find attr dimwann',ABS(ierr))

       CALL iotk_scan_attr(attr,'nkpts',nkpts_,IERR=ierr)
       IF (ierr/=0) CALL errore(subname,'Unable to find attr NKPTS',ABS(ierr))
       IF ( kpoints_alloc ) THEN
           IF ( nkpts_ /= nkpts ) CALL errore(subname,'Invalid NKPTS',ABS(nkpts-nkpts_))
       ELSE
          nkpts = nkpts_
       ENDIF
       IF ( subspace_alloc ) THEN
           IF ( dimwann_ /= dimwann )  &
              CALL errore(subname,'Invalid dimwann',ABS(dimwann-dimwann_))
       ELSE
          dimwann = dimwann_
       ENDIF

       !
       ! ... spreads
       CALL iotk_scan_empty(unit,'SPREADS',ATTR=attr,IERR=ierr)
       IF (ierr/=0) CALL errore(subname,'Unable to find tag SPREADS',ABS(ierr))

       CALL iotk_scan_attr(attr,'Omega_I',Omega_I,IERR=ierr)
       IF (ierr/=0) CALL errore(subname,'Unable to find attr Omega_I',ABS(ierr))
       CALL iotk_scan_attr(attr,'Omega_D',Omega_D,IERR=ierr)
       IF (ierr/=0) CALL errore(subname,'Unable to find attr Omega_D',ABS(ierr))
       CALL iotk_scan_attr(attr,'Omega_OD',Omega_OD,IERR=ierr)
       IF (ierr/=0) CALL errore(subname,'Unable to find attr Omega_OD',ABS(ierr))
       CALL iotk_scan_attr(attr,'Omega_V',Omega_V,IERR=ierr)
       IF (ierr/=0) CALL errore(subname,'Unable to find attr Omega_V',ABS(ierr))
       CALL iotk_scan_attr(attr,'Omega_tot',Omega_tot,IERR=ierr)
       IF (ierr/=0) CALL errore(subname,'Unable to find attr Omega_tot',ABS(ierr))

       !
       ! ... major data
       CALL localization_allocate()
       CALL iotk_scan_dat(unit,'CU',cu,IERR=ierr)
       IF (ierr/=0) CALL errore(subname,'Unable to find CU',ABS(ierr))
       CALL iotk_scan_dat(unit,'RAVE',rave,IERR=ierr)
       IF (ierr/=0) CALL errore(subname,'Unable to find RAVE',ABS(ierr))
       CALL iotk_scan_dat(unit,'RAVE2',rave2,IERR=ierr)
       IF (ierr/=0) CALL errore(subname,'Unable to find RAVE2',ABS(ierr))
       CALL iotk_scan_dat(unit,'R2AVE',r2ave,IERR=ierr)
       IF (ierr/=0) CALL errore(subname,'Unable to find R2AVE',ABS(ierr))

       CALL iotk_scan_end(unit,TRIM(name),IERR=ierr)
       IF (ierr/=0)  CALL errore(subname,'Unable to end tag '//TRIM(name),ABS(ierr))
   END SUBROUTINE localization_read

END MODULE localization_module
