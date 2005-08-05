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
   USE kpoints_module, ONLY : nkpts, nk, vkpt, wk, kpoints_alloc 
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

   ! ... dimensions
   INTEGER                     :: nws          ! number of direct lattive vectors
   INTEGER                     :: nwsx         ! max numb of nws ( = 3 * nkpts)
   !
   ! ... auxiliary data
   INTEGER, ALLOCATABLE        :: indxws(:,:)  ! R lattice vector (cryst coord), DIM 3 * nwsx
   INTEGER, ALLOCATABLE        :: degen(:)     ! R latt vect degeneracy, DIM: nwsx
   REAL(dbl), ALLOCATABLE      :: vws(:,:)     ! R lattice vector (cart coord), DIM 3 * nws
   !
   ! ... hamiltonians
   COMPLEX(dbl), ALLOCATABLE   :: rham(:,:,:)  ! DIM: dimwann, dimwann, nws
                                               ! real space hamiltonian
   COMPLEX(dbl), ALLOCATABLE   :: kham(:,:,:)  ! DIM: dimwann, dimwann, nkpts
                                               ! kpt-symm hamiltonian rotated according to
                                               ! WF transform
   !
   LOGICAL :: alloc = .FALSE.

!
! end of declarations
!

   PUBLIC :: nkpts, nws, dimwann
   PUBLIC :: wan_eig, efermi
   PUBLIC :: vws, indxws, degen
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
      
       IF ( dimwann <= 0 ) CALL errore(subname,'Invalid DIMWANN',1)
       IF ( nkpts <= 0 )   CALL errore(subname,'Invalid NKPTS',1)
       nwsx = ( INT( REAL(nkpts)**0.35 ) + 2 )**3

       IF ( .NOT. lattice_alloc )   CALL errore(subname,'lattice NOT alloc',1)
       IF ( .NOT. kpoints_alloc )   CALL errore(subname,'kpoints NOT alloc',1)
       IF ( .NOT. subspace_alloc )  CALL errore(subname,'subspace NOT alloc',1)
       
       ALLOCATE( indxws(3,nwsx), STAT = ierr )
           IF( ierr /=0 ) CALL errore(subname, 'allocating indxws', ABS(ierr) )
       ALLOCATE( degen(nwsx), STAT = ierr )
           IF( ierr /=0 ) CALL errore(subname, 'allocating degen', ABS(ierr) )

       !
       ! get the R vectors correspoinding to the chosen kpts
       ! in particular determin nws
       !
       degen(:) = 0
       indxws(:,:) = 0
       CALL wigner_seitz( avec, nk, indxws, nws, degen )

       !
       ! other allocations
       !
       ALLOCATE( rham(dimwann,dimwann,nws), STAT=ierr )
           IF( ierr /=0 ) CALL errore(subname, 'allocating rham', ABS(ierr) )
       ALLOCATE( kham(dimwann,dimwann,nkpts), STAT=ierr )
           IF( ierr /=0 ) CALL errore(subname, 'allocating kham', ABS(ierr) )
       ALLOCATE( vws(3,nws), STAT=ierr )
           IF( ierr /=0 ) CALL errore(subname, 'allocating kham', ABS(ierr) )

       !
       ! convert indxws to cart coord (bohr)
       !
       vws(:,1:nws) = REAL(indxws(:,1:nws))
       CALL cry2cart(vws, avec)

       alloc = .TRUE.

   END SUBROUTINE hamiltonian_init


!**********************************************************
   SUBROUTINE hamiltonian_deallocate()
   !**********************************************************
   IMPLICIT NONE
       CHARACTER(22)      :: subname="hamiltonian_deallocate"
       INTEGER            :: ierr 
      
       IF ( ALLOCATED(indxws) ) THEN 
            DEALLOCATE(indxws, STAT=ierr)
            IF (ierr/=0)  CALL errore(subname,'deallocating indxws ',ABS(ierr))
       ENDIF
       IF ( ALLOCATED(degen) ) THEN 
            DEALLOCATE(degen, STAT=ierr)
            IF (ierr/=0)  CALL errore(subname,'deallocating degen ',ABS(ierr))
       ENDIF
       IF ( ALLOCATED(vws) ) THEN 
            DEALLOCATE(vws, STAT=ierr)
            IF (ierr/=0)  CALL errore(subname,'deallocating vws ',ABS(ierr))
       ENDIF
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
       INTEGER            :: ik, iws, ierr

       IF ( .NOT. alloc ) RETURN
       IF ( .NOT. kpoints_alloc ) CALL errore(subname,'kpoints NOT alloc',1)
       IF ( .NOT. subspace_alloc ) CALL errore(subname,'subspace NOT alloc',1)

       CALL iotk_write_begin(unit,TRIM(name))
       CALL iotk_write_attr(attr,"dimwann",dimwann,FIRST=.TRUE.) 
       CALL iotk_write_attr(attr,"nkpts",nkpts) 
       CALL iotk_write_attr(attr,"nws",nws) 
       CALL iotk_write_empty(unit,"DATA",ATTR=attr)

       ALLOCATE( vkpt_cry(3, nkpts), STAT=ierr )
       IF (ierr/=0) CALL errore(subname,'allocating vkpt_cry',ABS(ierr))   
       vkpt_cry = vkpt
       CALL cart2cry(vkpt_cry, bvec)

       CALL iotk_write_dat(unit,"VKPT", vkpt_cry, COLUMNS=3, IERR=ierr) 
            IF (ierr/=0) CALL errore(subname,'writing VKPT',ABS(ierr))
       CALL iotk_write_dat(unit,"WK", wk, IERR=ierr) 
            IF (ierr/=0) CALL errore(subname,'writing WK',ABS(ierr))
       CALL iotk_write_dat(unit,"VWS", vws, COLUMNS=3, IERR=ierr) 
            IF (ierr/=0) CALL errore(subname,'writing VWS',ABS(ierr))
       CALL iotk_write_dat(unit,"INDXWS",indxws(:,1:nws), COLUMNS=3, IERR=ierr) 
            IF (ierr/=0) CALL errore(subname,'writing INDXWS',ABS(ierr))
       CALL iotk_write_dat(unit,"DEGEN",degen(1:nws), IERR=ierr) 
            IF (ierr/=0) CALL errore(subname,'writing DEGEN',ABS(ierr))

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
       DO iws = 1, nws
             CALL iotk_write_dat(unit,"WS"//TRIM(iotk_index(iws)), rham(:,:,iws))
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
       INTEGER            :: nkpts_, nws_, dimwann_
       INTEGER            :: ik, iws, ierr

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
       CALL iotk_scan_attr(attr,'nws',nws_,IERR=ierr)
       IF (ierr/=0) CALL errore(subname,'Unable to find attr NWS',ABS(ierr))

       IF ( dimwann_ /= dimwann) CALL errore(subname,'invalid dimwann',1)
       IF ( nkpts_ /= nkpts) CALL errore(subname,'invalid nkpts',2)
       IF ( nws_ /= nws) CALL errore(subname,'invalid nws',3)

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
       DO iws = 1, nws
             CALL iotk_scan_dat(unit,"WS"//TRIM(iotk_index(iws)), rham(:,:,iws), IERR=ierr)
             IF (ierr/=0) CALL errore(subname,'Unable to find dat WS in RHAM',iws)
       ENDDO
       CALL iotk_scan_end(unit,"RHAM", IERR=ierr)
       IF (ierr/=0)  CALL errore(subname,'Unable to end tag RHAM',ABS(ierr))

       CALL iotk_scan_end(unit,TRIM(name),IERR=ierr)
       IF (ierr/=0)  CALL errore(subname,'Unable to end tag '//TRIM(name),ABS(ierr))
   END SUBROUTINE hamiltonian_read

END MODULE hamiltonian_module
