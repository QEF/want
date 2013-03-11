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
   USE io_module,         ONLY : ionode, ionode_id
   USE log_module,        ONLY : log_push, log_pop
   USE lattice_module,    ONLY : avec, bvec, lattice_alloc => alloc
   USE kpoints_module,    ONLY : nkpts_g, nk, vkpt_g, wk_g,  &
                                 nrtot, nr, ivr,  wr,  kpoints_alloc 
   USE subspace_module,   ONLY : dimwann, subspace_alloc => alloc
   USE converters_module, ONLY : cry2cart, cart2cry
   USE mp,                ONLY : mp_bcast
   USE iotk_module
   !
   IMPLICIT NONE
   PRIVATE
   SAVE

! This module handles data referring to the hamiltonian
! of the system in the Wannier function representation.
!
! routines in this module:
! SUBROUTINE hamiltonian_allocate()
! SUBROUTINE hamiltonian_deallocate()
! SUBROUTINE hamiltonian_write(iun)
! SUBROUTINE hamiltonian_read(iun,found)
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

   PUBLIC :: nkpts_g, dimwann
   PUBLIC :: nrtot
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
   SUBROUTINE hamiltonian_write(iun)
   !**********************************************************
   IMPLICIT NONE
       INTEGER,         INTENT(in) :: iun
       CHARACTER(nstrx)   :: attr
       CHARACTER(17)      :: subname="hamiltonian_write"
       REAL(dbl), ALLOCATABLE :: vkpt_cry(:,:)
       INTEGER            :: ir, ierr

       IF ( .NOT. alloc ) RETURN
       !
       CALL log_push ( subname )
       !
       IF ( .NOT. kpoints_alloc ) CALL errore(subname,'kpoints NOT alloc',1)
       IF ( .NOT. subspace_alloc ) CALL errore(subname,'subspace NOT alloc',1)

       CALL iotk_write_begin(iun,"HAMILTONIAN")
       CALL iotk_write_attr(attr,"dimwann",dimwann,FIRST=.TRUE.) 
       CALL iotk_write_attr(attr,"nkpts",nkpts_g) 
       CALL iotk_write_attr(attr,"nk",nk) 
       CALL iotk_write_attr(attr,"nrtot",nrtot) 
       CALL iotk_write_attr(attr,"nr",nr) 
       CALL iotk_write_attr(attr,"have_overlap",lhave_overlap) 
       CALL iotk_write_attr(attr,"fermi_energy",0.0_dbl) 
       CALL iotk_write_empty(iun,"DATA",ATTR=attr)

       ALLOCATE( vkpt_cry(3, nkpts_g), STAT=ierr )
       IF (ierr/=0) CALL errore(subname,'allocating vkpt_cry',ABS(ierr))   
       vkpt_cry = vkpt_g
       CALL cart2cry(vkpt_cry, bvec)

       CALL iotk_write_attr(attr,"units","bohr",FIRST=.TRUE.)
       CALL iotk_write_dat(iun,"DIRECT_LATTICE", avec, ATTR=attr, COLUMNS=3) 
       !
       CALL iotk_write_attr(attr,"units","bohr^-1",FIRST=.TRUE.)
       CALL iotk_write_dat(iun,"RECIPROCAL_LATTICE", bvec, ATTR=attr, COLUMNS=3) 
       !
       CALL iotk_write_attr(attr,"units","crystal",FIRST=.TRUE.)
       CALL iotk_write_dat(iun,"VKPT", vkpt_cry, ATTR=attr, COLUMNS=3, IERR=ierr) 
            IF (ierr/=0) CALL errore(subname,'writing VKPT',ABS(ierr))
            !
       CALL iotk_write_dat(iun,"WK", wk_g, IERR=ierr) 
            IF (ierr/=0) CALL errore(subname,'writing WK',ABS(ierr))
            !
       CALL iotk_write_dat(iun,"IVR", ivr, ATTR=attr, COLUMNS=3, IERR=ierr) 
            IF (ierr/=0) CALL errore(subname,'writing ivr',ABS(ierr))
            !
       CALL iotk_write_dat(iun,"WR", wr, IERR=ierr) 
            IF (ierr/=0) CALL errore(subname,'writing wr',ABS(ierr))

       DEALLOCATE( vkpt_cry, STAT=ierr )
       IF (ierr/=0) CALL errore(subname,'deallocating vkpt_cry',ABS(ierr))   


       CALL iotk_write_begin(iun,"RHAM")
       !
       DO ir = 1, nrtot
           !
           CALL iotk_write_dat(iun,"VR"//TRIM(iotk_index(ir)), rham(:,:,ir))
           !
           IF ( lhave_overlap ) THEN
               !
               CALL iotk_write_dat(iun,"OVERLAP"//TRIM(iotk_index(ir)), rovp(:,:,ir))
               !
           ENDIF
           !
       ENDDO
       CALL iotk_write_end(iun,"RHAM")

       CALL iotk_write_end(iun,"HAMILTONIAN")
       !
       CALL log_pop ( subname )
       !
   END SUBROUTINE hamiltonian_write


!**********************************************************
   SUBROUTINE hamiltonian_read(iun,found)
   !**********************************************************
   !
   IMPLICIT NONE
       INTEGER,           INTENT(in) :: iun
       LOGICAL,           INTENT(out):: found
       LOGICAL            :: lfound
       CHARACTER(nstrx)   :: attr
       CHARACTER(16)      :: subname="hamiltonian_read"
!       INTEGER            :: nkpts_g_
       INTEGER            :: nrtot_, dimwann_
       INTEGER            :: ir, ierr

       CALL log_push ( subname )
       !
       IF ( alloc ) CALL hamiltonian_deallocate()

       IF ( ionode ) THEN
           !
           CALL iotk_scan_begin(iun,"HAMILTONIAN",FOUND=found,IERR=ierr)
           !
       ENDIF
       !
       CALL mp_bcast( found,    ionode_id )
       CALL mp_bcast( ierr,     ionode_id )
       !
       IF (.NOT. found) RETURN
       IF (ierr>0)  CALL errore(subname,'Wrong format in tag HAMILTONIAN',ierr)
       found = .TRUE.
       !
       IF ( ionode ) THEN
           !
           CALL iotk_scan_empty(iun,'DATA',ATTR=attr,IERR=ierr)
           IF (ierr/=0) CALL errore(subname,'Unable to find tag DATA',ABS(ierr))
           CALL iotk_scan_attr(attr,'dimwann',dimwann_,IERR=ierr)
           IF (ierr/=0) CALL errore(subname,'Unable to find attr DIMWANN',ABS(ierr))
!           CALL iotk_scan_attr(attr,'nkpts',nkpts_g_,IERR=ierr)
!           IF (ierr/=0) CALL errore(subname,'Unable to find attr NKPTS',ABS(ierr))
           CALL iotk_scan_attr(attr,'nrtot',nrtot_,IERR=ierr)
           IF (ierr/=0) CALL errore(subname,'Unable to find attr NRTOT',ABS(ierr))
           !
           CALL iotk_scan_attr(attr,'have_overlap',lhave_overlap, FOUND=lfound ,IERR=ierr)
           IF (ierr>0) CALL errore(subname,'Unable to find attr HAVE_OVERLAP',ABS(ierr))
           IF ( .NOT. lfound ) lhave_overlap = .FALSE.
           !
       ENDIF
       !
       CALL mp_bcast( dimwann_,      ionode_id )
!       CALL mp_bcast( nkpts_g_,      ionode_id )
       CALL mp_bcast( nrtot_,        ionode_id )
       CALL mp_bcast( lhave_overlap, ionode_id )

       !
       ! few compatibility checks
       IF ( dimwann_ /= dimwann) CALL errore(subname,'invalid dimwann',1)
!       IF ( nkpts_g_ /= nkpts_g)   CALL errore(subname,'invalid nkpts',2)
       IF ( nrtot_   /= nrtot)     CALL errore(subname,'invalid nrtot',3)
       !
       ! allocations
       !
       CALL hamiltonian_allocate()

       !
       ! massive data read
       !
       IF ( ionode ) THEN
           !
           CALL iotk_scan_begin(iun,"RHAM", IERR=ierr)
           IF (ierr/=0) CALL errore(subname,'Unable to find tag RHAM',ABS(ierr))
           !
           DO ir = 1, nrtot
               !
               CALL iotk_scan_dat(iun,"VR"//TRIM(iotk_index(ir)), rham(:,:,ir), IERR=ierr)
               IF (ierr/=0) CALL errore(subname,'Unable to find dat VR in RHAM',ir)
               !
               IF ( lhave_overlap ) THEN
                   !
                   CALL iotk_scan_dat(iun,"OVERLAP"//TRIM(iotk_index(ir)), rovp(:,:,ir), IERR=ierr)
                   IF (ierr/=0) CALL errore(subname,'Unable to find dat OVERLAP in RHAM',ir)
                   !
               ENDIF
               !
           ENDDO
           !
           CALL iotk_scan_end(iun,"RHAM", IERR=ierr)
           IF (ierr/=0)  CALL errore(subname,'Unable to end tag RHAM',ABS(ierr))

           CALL iotk_scan_end(iun,"HAMILTONIAN",IERR=ierr)
           IF (ierr/=0)  CALL errore(subname,'Unable to end tag HAMILTONIAN',ABS(ierr))
           !
       ENDIF
       !
       DO ir = 1, nrtot
           !
           CALL mp_bcast( rham(:,:,ir),   ionode_id )
           !
           IF ( lhave_overlap ) THEN
               CALL mp_bcast( rovp(:,:,ir),   ionode_id )
           ENDIF
           !
       ENDDO
       !
       CALL log_pop ( subname )
       !
       RETURN
       !
   END SUBROUTINE hamiltonian_read

END MODULE hamiltonian_module
