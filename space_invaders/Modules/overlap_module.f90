!
! Copyright (C) 2004 WanT Group
!
! This file is distributed under the terms of the
! GNU General Public License. See the file `License\'
! in the root directory of the present distribution,
! or http://www.gnu.org/copyleft/gpl.txt .
!

!*********************************************
   MODULE overlap_module
!*********************************************
   USE kinds, ONLY : dbl
   USE constants, ONLY : CZERO
   USE windows_module, ONLY : dimwinx, dimwin, windows_alloc => alloc
   USE kpoints_module, ONLY : nkpts, nb, nnlist, kpoints_alloc
   USE subspace_module,  ONLY : dimwann, subspace_alloc => alloc
   USE iotk_module
   USE parameters, ONLY : nstrx
   IMPLICIT NONE
   PRIVATE
   SAVE

! This module handles data referring to OVERLAP among
! the periodic part of bloch wfcs and thier projections
! onto the localized starting orbitals.
!
! routines in this module:
! SUBROUTINE overlap_allocate()
! SUBROUTINE overlap_deallocate()
! SUBROUTINE overlap_write(unit,name)
! SUBROUTINE overlap_read(unit,name,found)

!
! declarations of common variables
!   

   COMPLEX(dbl), ALLOCATABLE   :: Mkb(:,:,:,:)   ! <u_nk|u_mk+b> overlap
                                                 ! DIM: dimwinx,dimwinx,nb,nkpts
   COMPLEX(dbl), ALLOCATABLE   :: ca(:,:,:)      ! <u_nk|phi_lk> projection
                                                 ! DIM: dimwinx,dimwann,nkpts
   LOGICAL :: alloc = .FALSE.
   

!
! end of declarations
!

   PUBLIC :: Mkb, ca 
   PUBLIC :: dimwinx, dimwin, nkpts, nb, dimwann
   PUBLIC :: overlap_allocate
   PUBLIC :: overlap_deallocate
   PUBLIC :: overlap_write
   PUBLIC :: overlap_read
   PUBLIC :: alloc

CONTAINS

!
! subroutines
!

!**********************************************************
   SUBROUTINE overlap_allocate()
   !**********************************************************
   IMPLICIT NONE
       CHARACTER(16)      :: subname="overlap_allocate"
       INTEGER            :: ierr 

       IF ( dimwinx <= 0 ) CALL errore(subname,'Invalid DIMWINX',1)
       IF ( nkpts <= 0   ) CALL errore(subname,'Invalid NKPTS',1)
       IF ( dimwann <= 0 ) CALL errore(subname,'Invalid DIMWANN',1)
       IF ( nb <= 0      ) CALL errore(subname,'Invalid NB',1)

       ALLOCATE( Mkb(dimwinx,dimwinx,nb,nkpts), STAT=ierr )       
           IF ( ierr/=0 ) CALL errore(subname,' allocating Mkb', ABS(ierr) )

       ALLOCATE( ca(dimwinx,dimwann,nkpts), STAT=ierr )       
           IF ( ierr/=0 ) CALL errore(subname,' allocating ca ', ABS(ierr) )

       alloc = .TRUE. 
      
   END SUBROUTINE overlap_allocate


!**********************************************************
   SUBROUTINE overlap_deallocate()
   !**********************************************************
   IMPLICIT NONE
       CHARACTER(18)      :: subname="overlap_deallocate"
       INTEGER            :: ierr 

       IF ( ALLOCATED(Mkb) ) THEN
            DEALLOCATE(Mkb, STAT=ierr)
            IF (ierr/=0)  CALL errore(subname,' deallocating Mkb ',ABS(ierr))
       ENDIF
       IF ( ALLOCATED(ca) ) THEN
            DEALLOCATE(ca, STAT=ierr)
            IF (ierr/=0)  CALL errore(subname,' deallocating ca ',ABS(ierr))
       ENDIF
       alloc = .FALSE.

   END SUBROUTINE overlap_deallocate


!**********************************************************
   SUBROUTINE overlap_write(unit,name)
   !**********************************************************
   IMPLICIT NONE
       INTEGER,         INTENT(in) :: unit
       CHARACTER(*),    INTENT(in) :: name
       CHARACTER(nstrx)   :: attr
       INTEGER            :: iwann, ib, ik, ikb

       IF ( .NOT. alloc ) RETURN
       CALL iotk_write_begin(unit,TRIM(name))
       CALL iotk_write_attr(attr,"dimwinx",dimwinx,FIRST=.TRUE.)
       CALL iotk_write_attr(attr,"dimwann",dimwann)
       CALL iotk_write_attr(attr,"nb",nb)
       CALL iotk_write_attr(attr,"nkpts",nkpts)
       CALL iotk_write_empty(unit,"DATA",ATTR=attr)

       !  
       ! writing overlap
       !  
       CALL iotk_write_begin(unit,'OVERLAP')
       DO ik=1,nkpts
          CALL iotk_write_attr(attr,'dimwin_k',dimwin(ik),FIRST=.TRUE.)
          CALL iotk_write_attr(attr,'nneigh',nb)
          CALL iotk_write_begin(unit,'kpoint'//TRIM(iotk_index(ik)), ATTR=attr)
          !
          ! neighbours
          DO ib=1, nb
             ikb = nnlist(ib, ik)
             CALL iotk_write_attr(attr,'dimwin_kb',dimwin(ikb),FIRST=.TRUE.)
             CALL iotk_write_empty(unit, 'data', ATTR=attr)

             CALL iotk_write_dat(unit,'nn'//TRIM(iotk_index(ib)), &
                                       Mkb(1:dimwin(ik),1:dimwin(ikb),ib,ik) )
             CALL iotk_write_dat(unit,'nn_abs'//TRIM(iotk_index(ib)), &
                                       ABS(Mkb(1:dimwin(ik),1:dimwin(ikb),ib,ik)) )
          ENDDO
          CALL iotk_write_end(unit,'kpoint'//TRIM(iotk_index(ik)))
       ENDDO
       CALL iotk_write_end(unit,'OVERLAP')
    
       !  
       ! writing projections  
       !  
       CALL iotk_write_begin(unit,'PROJECTIONS')
       DO ik=1,nkpts
          CALL iotk_write_attr(attr,'dimwin',dimwin(ik),FIRST=.TRUE.)
          CALL iotk_write_begin(unit,'kpoint'//TRIM(iotk_index(ik)), ATTR=attr)
          DO iwann=1,dimwann
             CALL iotk_write_dat(unit,'wannier'//TRIM(iotk_index(iwann)), &
                                       ca(1:dimwin(ik),iwann,ik) )
             CALL iotk_write_dat(unit,'wannier_abs'//TRIM(iotk_index(iwann)), &
                                       ABS(ca(1:dimwin(ik),iwann,ik)) )
          ENDDO
          CALL iotk_write_end(unit,'kpoint'//TRIM(iotk_index(ik)))
       ENDDO
       CALL iotk_write_end(unit,'PROJECTIONS')
         
       CALL iotk_write_end(unit,TRIM(name))
   END SUBROUTINE overlap_write


!**********************************************************
   SUBROUTINE overlap_read(unit,name,found, loverlap, lprojection )
   !**********************************************************
   !
   ! allocate (if necessary) and read projections and overlaps integrals
   ! LOVERLAP and LPROJECTION make the routine read or not the specified
   ! quantities (default is .TRUE. for both)
   !
   IMPLICIT NONE
       INTEGER,           INTENT(in) :: unit
       CHARACTER(*),      INTENT(in) :: name
       LOGICAL,           INTENT(out):: found
       LOGICAL, OPTIONAL, INTENT(in) :: loverlap, lprojection

       CHARACTER(nstrx)   :: attr
       CHARACTER(12)      :: subname="overlap_read"
       LOGICAL            :: loverlap_, lprojection_
       INTEGER            :: dimwinx_, dimwann_, nb_, nkpts_
       INTEGER            :: ik, iwann, dimwin_, dimwin_k, dimwin_kb, ib, nneigh_
       INTEGER            :: ierr

       !
       ! define the default
       !
       loverlap_ = .TRUE.
       lprojection_ = .TRUE.
       IF ( PRESENT(loverlap) ) loverlap_ = loverlap
       IF ( PRESENT(lprojection) ) lprojection_ = lprojection
 
       
       CALL iotk_scan_begin(unit,TRIM(name),FOUND=found,IERR=ierr)
       IF (.NOT. found) RETURN
       IF (ierr>0)  CALL errore(subname,'Wrong format in tag '//TRIM(name),ierr)
       found = .TRUE.

       CALL iotk_scan_empty(unit,'DATA',ATTR=attr,IERR=ierr)
       IF (ierr/=0) CALL errore(subname,'Unable to find tag DATA',ABS(ierr))

       CALL iotk_scan_attr(attr,'dimwinx',dimwinx_,IERR=ierr)
          IF (ierr/=0) CALL errore(subname,'Unable to find attr DIMWINX',ABS(ierr))
       CALL iotk_scan_attr(attr,'dimwann',dimwann_,IERR=ierr)
          IF (ierr/=0) CALL errore(subname,'Unable to find attr DIMWANN',ABS(ierr))
       CALL iotk_scan_attr(attr,'nb',nb_,IERR=ierr)
          IF (ierr/=0) CALL errore(subname,'Unable to find attr NNX',ABS(ierr))
       CALL iotk_scan_attr(attr,'nkpts',nkpts_,IERR=ierr)
          IF (ierr/=0) CALL errore(subname,'Unable to find attr NKPTS',ABS(ierr))

       !
       ! ... various checks
       IF ( windows_alloc ) THEN
          IF (dimwinx_/=dimwinx) CALL errore(subname,'Invalid DIMWINX',ABS(dimwinx_-dimwinx))
       ELSE
          dimwinx = dimwinx_
       ENDIF
       !
       IF ( kpoints_alloc ) THEN
          IF ( nkpts_ /= nkpts) CALL errore(subname,'Invalid NKPTS',ABS(nkpts_-nkpts))
          IF ( nb_ /= nb) CALL errore(subname,'Invalid NB',ABS(nb_-nb))
       ELSE
          nkpts = nkpts_
       ENDIF
       IF ( subspace_alloc ) THEN
           IF ( dimwann_ /= dimwann) &
                CALL errore(subname,'Invalid DIMWANN',ABS(dimwann_-dimwann))
       ELSE
          dimwann = dimwann_
       ENDIF

       !
       IF ( .NOT. alloc ) CALL overlap_allocate()       

       !
       ! read overlap
       !
       IF ( loverlap_ ) THEN
           Mkb(:,:,:,:) = CZERO
           CALL iotk_scan_begin(unit,'OVERLAP',IERR=ierr)
           IF (ierr/=0) CALL errore(subname,'scanning for OVERLAP',ABS(ierr))
          
           DO ik=1,nkpts
               CALL iotk_scan_begin(unit,'kpoint'//TRIM(iotk_index(ik)), ATTR=attr, IERR=ierr)
               IF (ierr/=0) CALL errore(subname,'scanning for kpoint',ik)
               CALL iotk_scan_attr(attr,'dimwin_k',dimwin_k,IERR=ierr)
               IF (ierr/=0) CALL errore(subname,'scanning for dimwin',ik)
               IF ( dimwin_k > dimwinx ) CALL errore(subname,'dimwin too large',dimwin_k)
               CALL iotk_scan_attr(attr,'nneigh',nneigh_,IERR=ierr)
               IF (ierr/=0) CALL errore(subname,'scanning for nneigh_',ik)
               IF ( nneigh_ /= nb ) CALL errore(subname,'nniegh too large',nneigh_)

               DO ib=1,nneigh_
                   CALL iotk_scan_empty(unit, 'data', ATTR=attr, IERR=ierr)
                   IF (ierr/=0) CALL errore(subname,'scanning for data',ib)
                   CALL iotk_scan_attr(attr,'dimwin_kb',dimwin_kb,IERR=ierr)
                   IF (ierr/=0) CALL errore(subname,'scanning for dimwin_kb',ib)
                   IF ( dimwin_kb > dimwinx ) CALL errore(subname,'dimwin too large',dimwin_kb)
                 
                   CALL iotk_scan_dat(unit,'nn'//TRIM(iotk_index(ib)), &
                            Mkb(1:dimwin_k,1:dimwin_kb,ib,ik),IERR=ierr )
                   IF (ierr/=0) CALL errore(subname,'scanning for nn',ib)
               ENDDO
               CALL iotk_scan_end(unit,'kpoint'//TRIM(iotk_index(ik)), IERR=ierr)
               IF (ierr/=0) CALL errore(subname,'scanning for ending kpoint',ik)
           ENDDO
           
           CALL iotk_scan_end(unit,'OVERLAP',IERR=ierr)
           IF (ierr/=0) CALL errore(subname,'scanning for ending OVERLAP',ABS(ierr))
       ENDIF

       !
       ! read projections
       !
       IF ( lprojection_ ) THEN
           ca(:,:,:) = CZERO
           CALL iotk_scan_begin(unit,'PROJECTIONS',IERR=ierr)
           IF (ierr/=0) CALL errore(subname,'scanning for PROJECTIONS',ABS(ierr))
          
           DO ik=1,nkpts
               CALL iotk_scan_begin(unit,'kpoint'//TRIM(iotk_index(ik)), ATTR=attr, IERR=ierr)
               IF (ierr/=0) CALL errore(subname,'scanning for kpoint',ik)
               CALL iotk_scan_attr(attr,'dimwin',dimwin_,IERR=ierr)
               IF (ierr/=0) CALL errore(subname,'scanning for dimwin',ik)
               IF ( dimwin_ > dimwinx ) CALL errore(subname,'dimwin too large',dimwin_)

               DO iwann=1,dimwann
                     CALL iotk_scan_dat(unit,'wannier'//TRIM(iotk_index(iwann)), &
                                             ca(1:dimwin_,iwann,ik),IERR=ierr )
                     IF (ierr/=0) CALL errore(subname,'scanning for wannier',iwann)
               ENDDO
               CALL iotk_scan_end(unit,'kpoint'//TRIM(iotk_index(ik)), IERR=ierr)
               IF (ierr/=0) CALL errore(subname,'scanning for ending kpoint',ik)
           ENDDO
           
           CALL iotk_scan_end(unit,'PROJECTIONS',IERR=ierr)
           IF (ierr/=0) CALL errore(subname,'scanning for ending PROJECTIONS',ABS(ierr))
       ENDIF

       CALL iotk_scan_end(unit,TRIM(name),IERR=ierr)
       IF (ierr/=0)  CALL errore(subname,'Unable to end tag '//TRIM(name),ABS(ierr))
   END SUBROUTINE overlap_read

END MODULE overlap_module

