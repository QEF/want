!
! Copyright (C) 2005 WanT Group
!
! This file is distributed under the terms of the
! GNU General Public License. See the file `License\'
! in the root directory of the present distribution,
! or http://www.gnu.org/copyleft/gpl.txt .
!

!*********************************************
   MODULE hamiltonian_module
!*********************************************
   USE kinds, ONLY : dbl
   USE lattice_module, ONLY : avec, bvec, lattice_alloc => alloc
   USE kpoints_module, ONLY : nkpts, nk, vkpt, wk,  &
                              nrtot, nr, vr, ivr,  wr,  kpoints_alloc 
   USE subspace_module, ONLY : dimwann, wan_eig, efermi, subspace_alloc => alloc
   USE iotk_module
   USE parameters, ONLY : nstrx
   USE converters_module, ONLY : cry2cart, cart2cry
   IMPLICIT NONE
   PRIVATE
   SAVE

! This module handles data referring to the hamiltonian
! of the system in the Wannier function representation.
!
! routines in this module:
! SUBROUTINE hamiltonian_allocate()
! SUBROUTINE hamiltonian_deallocate()
! SUBROUTINE hamiltonian_write(unit,name)
! SUBROUTINE hamiltonian_read(unit,name,found)
!

!
! declarations of common variables
!   

   !
   ! ... hamiltonians
   COMPLEX(dbl), ALLOCATABLE   :: rham(:,:,:)  ! DIM: dimwann, dimwann, nrtot
                                               ! real space hamiltonian
   COMPLEX(dbl), ALLOCATABLE   :: kham(:,:,:)  ! DIM: dimwann, dimwann, nkpts
                                               ! kpt-symm hamiltonian rotated according to
                                               ! WF transform
   !
   LOGICAL :: alloc = .FALSE.

!
! end of declarations
!

   PUBLIC :: nkpts, dimwann
   PUBLIC :: nrtot
   PUBLIC :: wan_eig, efermi
   PUBLIC :: rham
   PUBLIC :: kham
   PUBLIC :: alloc

   PUBLIC :: hamiltonian_init
   PUBLIC :: hamiltonian_deallocate
   PUBLIC :: hamiltonian_write
   PUBLIC :: hamiltonian_read

CONTAINS

!
! subroutines
!

!**********************************************************
   SUBROUTINE hamiltonian_init()
   !**********************************************************
   IMPLICIT NONE
       CHARACTER(16)      :: subname="hamiltonian_init"
       INTEGER            :: ierr 

       IF ( .NOT. lattice_alloc )   CALL errore(subname,'lattice NOT alloc',1)
       IF ( .NOT. kpoints_alloc )   CALL errore(subname,'kpoints NOT alloc',1)
       IF ( .NOT. subspace_alloc )  CALL errore(subname,'subspace NOT alloc',1)
       
       IF ( dimwann <= 0 )  CALL errore(subname,'Invalid DIMWANN',1)
       IF ( nkpts <= 0 )    CALL errore(subname,'Invalid NKPTS',1)
       IF ( nrtot <= 0 )    CALL errore(subname,'Invalid NRTOT',1)
       IF ( nrtot < nkpts ) CALL errore(subname,'Invalid NRTOT < NKPTS',1)

       !
       ! other allocations
       !
       ALLOCATE( rham(dimwann,dimwann,nrtot), STAT=ierr )
           IF( ierr /=0 ) CALL errore(subname, 'allocating rham', ABS(ierr) )
       ALLOCATE( kham(dimwann,dimwann,nkpts), STAT=ierr )
           IF( ierr /=0 ) CALL errore(subname, 'allocating kham', ABS(ierr) )

       alloc = .TRUE.

   END SUBROUTINE hamiltonian_init


!**********************************************************
   SUBROUTINE hamiltonian_deallocate()
   !**********************************************************
   IMPLICIT NONE
       CHARACTER(22)      :: subname="hamiltonian_deallocate"
       INTEGER            :: ierr 
      
       IF ( ALLOCATED(rham) ) THEN 
            DEALLOCATE(rham, STAT=ierr)
            IF (ierr/=0)  CALL errore(subname,'deallocating rham ',ABS(ierr))
       ENDIF
       IF ( ALLOCATED(kham) ) THEN 
            DEALLOCATE(kham, STAT=ierr)
            IF (ierr/=0)  CALL errore(subname,'deallocating kham ',ABS(ierr))
       ENDIF
   END SUBROUTINE hamiltonian_deallocate


!**********************************************************
   SUBROUTINE hamiltonian_write(unit,name)
   !**********************************************************
   IMPLICIT NONE
       INTEGER,         INTENT(in) :: unit
       CHARACTER(*),    INTENT(in) :: name
       CHARACTER(nstrx)   :: attr
       CHARACTER(17)      :: subname="hamiltonian_write"
       REAL(dbl), ALLOCATABLE :: vkpt_cry(:,:)
       INTEGER            :: ik, ir, ierr

       IF ( .NOT. alloc ) RETURN
       IF ( .NOT. kpoints_alloc ) CALL errore(subname,'kpoints NOT alloc',1)
       IF ( .NOT. subspace_alloc ) CALL errore(subname,'subspace NOT alloc',1)

       CALL iotk_write_begin(unit,TRIM(name))
       CALL iotk_write_attr(attr,"dimwann",dimwann,FIRST=.TRUE.) 
       CALL iotk_write_attr(attr,"nkpts",nkpts) 
       CALL iotk_write_attr(attr,"nk",nk) 
       CALL iotk_write_attr(attr,"nrtot",nrtot) 
       CALL iotk_write_attr(attr,"nr",nr) 
       CALL iotk_write_empty(unit,"DATA",ATTR=attr)

       ALLOCATE( vkpt_cry(3, nkpts), STAT=ierr )
       IF (ierr/=0) CALL errore(subname,'allocating vkpt_cry',ABS(ierr))   
       vkpt_cry = vkpt
       CALL cart2cry(vkpt_cry, bvec)

       CALL iotk_write_attr(attr,"units","bohr",FIRST=.TRUE.)
       CALL iotk_write_dat(unit,"DIRECT_LATTICE", avec, ATTR=attr, COLUMNS=3) 
       !
       CALL iotk_write_attr(attr,"units","bohr^-1",FIRST=.TRUE.)
       CALL iotk_write_dat(unit,"RECIPROCAL_LATTICE", bvec, ATTR=attr, COLUMNS=3) 
       !
       CALL iotk_write_attr(attr,"units","crystal",FIRST=.TRUE.)
       CALL iotk_write_dat(unit,"VKPT", vkpt_cry, ATTR=attr, COLUMNS=3, IERR=ierr) 
            IF (ierr/=0) CALL errore(subname,'writing VKPT',ABS(ierr))
            !
       CALL iotk_write_dat(unit,"WK", wk, IERR=ierr) 
            IF (ierr/=0) CALL errore(subname,'writing WK',ABS(ierr))
            !
       CALL iotk_write_dat(unit,"IVR", ivr, ATTR=attr, COLUMNS=3, IERR=ierr) 
            IF (ierr/=0) CALL errore(subname,'writing ivr',ABS(ierr))
            !
       CALL iotk_write_dat(unit,"WR", wr, IERR=ierr) 
            IF (ierr/=0) CALL errore(subname,'writing wr',ABS(ierr))

       DEALLOCATE( vkpt_cry, STAT=ierr )
       IF (ierr/=0) CALL errore(subname,'deallocating vkpt_cry',ABS(ierr))   

       CALL iotk_write_dat(unit,"WAN_EIGENVALUES",wan_eig)
       !
       CALL iotk_write_begin(unit,"KHAM")
       DO ik = 1, nkpts
             CALL iotk_write_dat(unit,"KPT"//TRIM(iotk_index(ik)), kham(:,:,ik))
       ENDDO
       CALL iotk_write_end(unit,"KHAM")
       !
       CALL iotk_write_begin(unit,"RHAM")
       DO ir = 1, nrtot
             CALL iotk_write_dat(unit,"VR"//TRIM(iotk_index(ir)), rham(:,:,ir))
       ENDDO
       CALL iotk_write_end(unit,"RHAM")

       CALL iotk_write_end(unit,TRIM(name))
   END SUBROUTINE hamiltonian_write


!**********************************************************
   SUBROUTINE hamiltonian_read(unit,name,found)
   !**********************************************************
   IMPLICIT NONE
       INTEGER,           INTENT(in) :: unit
       CHARACTER(*),      INTENT(in) :: name
       LOGICAL,           INTENT(out):: found
       CHARACTER(nstrx)   :: attr
       CHARACTER(16)      :: subname="hamiltonian_read"
       INTEGER            :: nkpts_, nrtot_, dimwann_
       INTEGER            :: ik, ir, ierr

       IF ( alloc ) CALL hamiltonian_deallocate()
       CALL hamiltonian_init()

       CALL iotk_scan_begin(unit,TRIM(name),FOUND=found,IERR=ierr)
       IF (.NOT. found) RETURN
       IF (ierr>0)  CALL errore(subname,'Wrong format in tag '//TRIM(name),ierr)
       found = .TRUE.

       CALL iotk_scan_empty(unit,'DATA',ATTR=attr,IERR=ierr)
       IF (ierr/=0) CALL errore(subname,'Unable to find tag DATA',ABS(ierr))
       CALL iotk_scan_attr(attr,'dimwann',dimwann_,IERR=ierr)
       IF (ierr/=0) CALL errore(subname,'Unable to find attr DIMWANN',ABS(ierr))
       CALL iotk_scan_attr(attr,'nkpts',nkpts_,IERR=ierr)
       IF (ierr/=0) CALL errore(subname,'Unable to find attr NKPTS',ABS(ierr))
       CALL iotk_scan_attr(attr,'nrtot',nrtot_,IERR=ierr)
       IF (ierr/=0) CALL errore(subname,'Unable to find attr NRTOT',ABS(ierr))

       IF ( dimwann_ /= dimwann) CALL errore(subname,'invalid dimwann',1)
       IF ( nkpts_ /= nkpts) CALL errore(subname,'invalid nkpts',2)
       IF ( nrtot_ /= nrtot) CALL errore(subname,'invalid nrtot',3)

       CALL iotk_scan_begin(unit,"KHAM", IERR=ierr)
       IF (ierr/=0) CALL errore(subname,'Unable to find tag KHAM',ABS(ierr))
       DO ik = 1, nkpts
             CALL iotk_scan_dat(unit,"KPT"//TRIM(iotk_index(ik)), kham(:,:,ik), IERR=ierr)
             IF (ierr/=0) CALL errore(subname,'Unable to find dat KPT in KHAM',ik)
       ENDDO
       CALL iotk_scan_end(unit,"KHAM", IERR=ierr)
       IF (ierr/=0)  CALL errore(subname,'Unable to end tag KHAM',ABS(ierr))
       !
       CALL iotk_scan_begin(unit,"RHAM", IERR=ierr)
       IF (ierr/=0) CALL errore(subname,'Unable to find tag RHAM',ABS(ierr))
       DO ir = 1, nrtot
             CALL iotk_scan_dat(unit,"VR"//TRIM(iotk_index(ir)), rham(:,:,ir), IERR=ierr)
             IF (ierr/=0) CALL errore(subname,'Unable to find dat VR in RHAM',ir)
       ENDDO
       CALL iotk_scan_end(unit,"RHAM", IERR=ierr)
       IF (ierr/=0)  CALL errore(subname,'Unable to end tag RHAM',ABS(ierr))

       CALL iotk_scan_end(unit,TRIM(name),IERR=ierr)
       IF (ierr/=0)  CALL errore(subname,'Unable to end tag '//TRIM(name),ABS(ierr))
   END SUBROUTINE hamiltonian_read

END MODULE hamiltonian_module
