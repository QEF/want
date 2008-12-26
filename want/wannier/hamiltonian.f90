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
   !
   USE kinds,             ONLY : dbl
   USE constants,         ONLY : ZERO
   USE parameters,        ONLY : nstrx
   USE log_module,        ONLY : log_push, log_pop
   USE lattice_module,    ONLY : avec, bvec, lattice_alloc => alloc
   USE kpoints_module,    ONLY : nkpts, nkpts_g, nk, vkpt_g, wk_g,  &
                                 nrtot, nr, ivr,  wr,  kpoints_alloc 
   USE subspace_module,   ONLY : dimwann, wan_eig, subspace_alloc => alloc
   USE converters_module, ONLY : cry2cart, cart2cry
   USE iotk_module
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
   COMPLEX(dbl), ALLOCATABLE   :: rovp(:,:,:)  ! overlap integrals, DIMS as rham
   !
   LOGICAL :: lhave_overlap = .FALSE.
   !
   LOGICAL :: alloc = .FALSE.

!
! end of declarations
!

   PUBLIC :: nkpts, dimwann
   PUBLIC :: nrtot
   PUBLIC :: wan_eig
   PUBLIC :: rham
   PUBLIC :: rovp
   PUBLIC :: lhave_overlap
   PUBLIC :: alloc

   PUBLIC :: hamiltonian_allocate
   PUBLIC :: hamiltonian_deallocate
   PUBLIC :: hamiltonian_memusage
   PUBLIC :: hamiltonian_write
   PUBLIC :: hamiltonian_read

CONTAINS

!
! subroutines
!

!**********************************************************
   SUBROUTINE hamiltonian_allocate()
   !**********************************************************
   IMPLICIT NONE
       CHARACTER(20)      :: subname="hamiltonian_allocate"
       INTEGER            :: ierr 

       CALL log_push ( subname )
       !
       IF ( .NOT. lattice_alloc )   CALL errore(subname,'lattice NOT alloc',1)
       IF ( .NOT. kpoints_alloc )   CALL errore(subname,'kpoints NOT alloc',1)
       IF ( .NOT. subspace_alloc )  CALL errore(subname,'subspace NOT alloc',1)
       
       IF ( dimwann <= 0 )  CALL errore(subname,'Invalid DIMWANN',1)
       IF ( nkpts <= 0 )    CALL errore(subname,'Invalid NKPTS',1)
       IF ( nrtot <= 0 )    CALL errore(subname,'Invalid NRTOT',1)

       !
       ! other allocations
       !
       ALLOCATE( rham(dimwann,dimwann,nrtot), STAT=ierr )
       IF( ierr /=0 ) CALL errore(subname, 'allocating rham', ABS(ierr) )
       !
       IF ( lhave_overlap ) THEN
           !
           ALLOCATE( rovp(dimwann,dimwann,nrtot), STAT=ierr )
           IF( ierr /=0 ) CALL errore(subname, 'allocating rovp', ABS(ierr) )
           !
       ENDIF
       !
       alloc = .TRUE.
       !
       CALL log_pop ( subname )
       !
   END SUBROUTINE hamiltonian_allocate


!**********************************************************
   SUBROUTINE hamiltonian_deallocate()
   !**********************************************************
   IMPLICIT NONE
       CHARACTER(22)      :: subname="hamiltonian_deallocate"
       INTEGER            :: ierr 
      
       CALL log_push ( subname )
       !
       IF ( ALLOCATED(rham) ) THEN 
            DEALLOCATE(rham, STAT=ierr)
            IF (ierr/=0)  CALL errore(subname,'deallocating rham ',ABS(ierr))
       ENDIF
       IF ( ALLOCATED(rovp) ) THEN 
            DEALLOCATE(rovp, STAT=ierr)
            IF (ierr/=0)  CALL errore(subname,'deallocating rovp ',ABS(ierr))
       ENDIF
       !
       CALL log_pop ( subname )
       !
   END SUBROUTINE hamiltonian_deallocate


!**********************************************************
   REAL(dbl) FUNCTION hamiltonian_memusage()
   !**********************************************************
   IMPLICIT NONE
       !
       REAL(dbl) :: cost
       !
       cost = ZERO
       IF ( ALLOCATED(rham) )     cost = cost + REAL(SIZE(rham))       * 16.0_dbl
       IF ( ALLOCATED(rovp) )     cost = cost + REAL(SIZE(rovp))       * 16.0_dbl
       !
       hamiltonian_memusage = cost / 1000000.0_dbl
       !
   END FUNCTION hamiltonian_memusage


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
       !
       CALL log_push ( subname )
       !
       IF ( .NOT. kpoints_alloc ) CALL errore(subname,'kpoints NOT alloc',1)
       IF ( .NOT. subspace_alloc ) CALL errore(subname,'subspace NOT alloc',1)

       CALL iotk_write_begin(unit,TRIM(name))
       CALL iotk_write_attr(attr,"dimwann",dimwann,FIRST=.TRUE.) 
       CALL iotk_write_attr(attr,"nkpts",nkpts_g) 
       CALL iotk_write_attr(attr,"nk",nk) 
       CALL iotk_write_attr(attr,"nrtot",nrtot) 
       CALL iotk_write_attr(attr,"nr",nr) 
       CALL iotk_write_attr(attr,"have_overlap",lhave_overlap) 
       CALL iotk_write_attr(attr,"fermi_energy",0.0_dbl) 
       CALL iotk_write_empty(unit,"DATA",ATTR=attr)

       ALLOCATE( vkpt_cry(3, nkpts_g), STAT=ierr )
       IF (ierr/=0) CALL errore(subname,'allocating vkpt_cry',ABS(ierr))   
       vkpt_cry = vkpt_g
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
       CALL iotk_write_dat(unit,"WK", wk_g, IERR=ierr) 
            IF (ierr/=0) CALL errore(subname,'writing WK',ABS(ierr))
            !
       CALL iotk_write_dat(unit,"IVR", ivr, ATTR=attr, COLUMNS=3, IERR=ierr) 
            IF (ierr/=0) CALL errore(subname,'writing ivr',ABS(ierr))
            !
       CALL iotk_write_dat(unit,"WR", wr, IERR=ierr) 
            IF (ierr/=0) CALL errore(subname,'writing wr',ABS(ierr))

       DEALLOCATE( vkpt_cry, STAT=ierr )
       IF (ierr/=0) CALL errore(subname,'deallocating vkpt_cry',ABS(ierr))   


       CALL iotk_write_begin(unit,"RHAM")
       !
       DO ir = 1, nrtot
           !
           CALL iotk_write_dat(unit,"VR"//TRIM(iotk_index(ir)), rham(:,:,ir))
           !
           IF ( lhave_overlap ) THEN
               !
               CALL iotk_write_dat(unit,"OVERLAP"//TRIM(iotk_index(ir)), rovp(:,:,ir))
               !
           ENDIF
           !
       ENDDO
       CALL iotk_write_end(unit,"RHAM")

       CALL iotk_write_end(unit,TRIM(name))
       !
       CALL log_pop ( subname )
       !
   END SUBROUTINE hamiltonian_write


!**********************************************************
   SUBROUTINE hamiltonian_read(unit,name,found)
   !**********************************************************
   IMPLICIT NONE
       INTEGER,           INTENT(in) :: unit
       CHARACTER(*),      INTENT(in) :: name
       LOGICAL,           INTENT(out):: found
       LOGICAL            :: lfound
       CHARACTER(nstrx)   :: attr
       CHARACTER(16)      :: subname="hamiltonian_read"
       INTEGER            :: nkpts_g_, nrtot_, dimwann_
       INTEGER            :: ik, ir, ierr

       CALL log_push ( subname )
       !
       IF ( alloc ) CALL hamiltonian_deallocate()

       CALL iotk_scan_begin(unit,TRIM(name),FOUND=found,IERR=ierr)
       IF (.NOT. found) RETURN
       IF (ierr>0)  CALL errore(subname,'Wrong format in tag '//TRIM(name),ierr)
       found = .TRUE.

       CALL iotk_scan_empty(unit,'DATA',ATTR=attr,IERR=ierr)
       IF (ierr/=0) CALL errore(subname,'Unable to find tag DATA',ABS(ierr))
       CALL iotk_scan_attr(attr,'dimwann',dimwann_,IERR=ierr)
       IF (ierr/=0) CALL errore(subname,'Unable to find attr DIMWANN',ABS(ierr))
       CALL iotk_scan_attr(attr,'nkpts',nkpts_g_,IERR=ierr)
       IF (ierr/=0) CALL errore(subname,'Unable to find attr NKPTS',ABS(ierr))
       CALL iotk_scan_attr(attr,'nrtot',nrtot_,IERR=ierr)
       IF (ierr/=0) CALL errore(subname,'Unable to find attr NRTOT',ABS(ierr))
       !
       CALL iotk_scan_attr(attr,'have_overlap',lhave_overlap, FOUND=lfound ,IERR=ierr)
       IF (ierr>0) CALL errore(subname,'Unable to find attr HAVE_OVERLAP',ABS(ierr))
       IF ( .NOT. lfound ) lhave_overlap = .FALSE.

       !
       ! few compatibility checks
       IF ( dimwann_ /= dimwann) CALL errore(subname,'invalid dimwann',1)
       IF ( nkpts_g_ /= nkpts)   CALL errore(subname,'invalid nkpts',2)
       IF ( nrtot_   /= nrtot)   CALL errore(subname,'invalid nrtot',3)
       !
       ! allocations
       CALL hamiltonian_allocate()

       !
       ! massive data read
       !
       CALL iotk_scan_begin(unit,"RHAM", IERR=ierr)
       IF (ierr/=0) CALL errore(subname,'Unable to find tag RHAM',ABS(ierr))
       !
       DO ir = 1, nrtot
           !
           CALL iotk_scan_dat(unit,"VR"//TRIM(iotk_index(ir)), rham(:,:,ir), IERR=ierr)
           IF (ierr/=0) CALL errore(subname,'Unable to find dat VR in RHAM',ir)
           !
           IF ( lhave_overlap ) THEN
               !
               CALL iotk_scan_dat(unit,"OVERLAP"//TRIM(iotk_index(ir)), rovp(:,:,ir), IERR=ierr)
               IF (ierr/=0) CALL errore(subname,'Unable to find dat OVERLAP in RHAM',ir)
               !
           ENDIF
           !
       ENDDO
       !
       CALL iotk_scan_end(unit,"RHAM", IERR=ierr)
       IF (ierr/=0)  CALL errore(subname,'Unable to end tag RHAM',ABS(ierr))

       CALL iotk_scan_end(unit,TRIM(name),IERR=ierr)
       IF (ierr/=0)  CALL errore(subname,'Unable to end tag '//TRIM(name),ABS(ierr))
       !
       CALL log_pop ( subname )
       !
   END SUBROUTINE hamiltonian_read

END MODULE hamiltonian_module
