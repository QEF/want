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
   !
   USE kinds,            ONLY : dbl
   USE constants,        ONLY : ZERO, CZERO
   USE io_module,        ONLY : ionode, ionode_id
   USE parameters,       ONLY : nstrx
   USE log_module,       ONLY : log_push, log_pop
   USE windows_module,   ONLY : dimwinx, dimwin, windows_alloc => alloc
   USE kpoints_module,   ONLY : nkpts, nkpts_g, iks, ike, iproc_g, nb, &
                                nnlist, nnpos, nnrev, kpoints_alloc
   USE subspace_module,  ONLY : dimwann, subspace_alloc => alloc
   USE mp_global,        ONLY : mpime
   USE mp,               ONLY : mp_get, mp_barrier
   USE iotk_module
   !
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
! SUBROUTINE overlap_write(iun,tag)
! SUBROUTINE overlap_read(iun,tag,found)

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
   !
   PUBLIC :: overlap_allocate
   PUBLIC :: overlap_deallocate
   PUBLIC :: overlap_memusage
   PUBLIC :: overlap_write
   PUBLIC :: overlap_read
   PUBLIC :: alloc

CONTAINS

!
! subroutines
!

!**********************************************************
   SUBROUTINE overlap_allocate(memusage)
   !**********************************************************
   !
   ! memusgae = "full | high"   ->  allocates Mkb for all b vectors
   !          = "low"           ->  allocates Mkb for positive b vectors only
   ! DEFAULT: memusage = "full"
   !
   IMPLICIT NONE
       CHARACTER(*), OPTIONAL, INTENT(IN) :: memusage
       !
       CHARACTER(16)      :: subname="overlap_allocate"
       LOGICAL            :: lfull
       INTEGER            :: ierr 

       CALL log_push( subname )
       !
       IF ( dimwinx <= 0 ) CALL errore(subname,'Invalid dimwinx',1)
       IF ( nkpts <= 0   ) CALL errore(subname,'Invalid nkpts',1)
       IF ( dimwann <= 0 ) CALL errore(subname,'Invalid dimwann',1)
       IF ( nb <= 0      ) CALL errore(subname,'Invalid nb',1)

       lfull = .TRUE.
       IF ( PRESENT(memusage) ) THEN
          !
          SELECT CASE ( TRIM(memusage) )
          CASE ( "full", "FULL", "high", "HIGH")
             lfull = .TRUE.
          CASE ( "low", "LOW" )
             lfull = .FALSE.
          END SELECT
          !
       ENDIF
       
       IF ( lfull ) THEN
          ALLOCATE( Mkb(dimwinx,dimwinx,nb,nkpts), STAT=ierr )       
       ELSE
          ALLOCATE( Mkb(dimwinx,dimwinx,nb/2,nkpts), STAT=ierr )       
       ENDIF
       IF ( ierr/=0 ) CALL errore(subname,'allocating Mkb', ABS(ierr) )
       !
       !
       ALLOCATE( ca(dimwinx,dimwann,nkpts), STAT=ierr )       
       IF ( ierr/=0 ) CALL errore(subname,'allocating ca', ABS(ierr) )

       alloc = .TRUE. 
       !
       CALL log_pop( subname )
       ! 
   END SUBROUTINE overlap_allocate


!**********************************************************
   SUBROUTINE overlap_deallocate()
   !**********************************************************
   IMPLICIT NONE
       CHARACTER(18)      :: subname="overlap_deallocate"
       INTEGER            :: ierr 

       CALL log_push( subname )
       !
       IF ( ALLOCATED(Mkb) ) THEN
            DEALLOCATE(Mkb, STAT=ierr)
            IF (ierr/=0)  CALL errore(subname,' deallocating Mkb ',ABS(ierr))
       ENDIF
       IF ( ALLOCATED(ca) ) THEN
            DEALLOCATE(ca, STAT=ierr)
            IF (ierr/=0)  CALL errore(subname,' deallocating ca ',ABS(ierr))
       ENDIF
       !
       alloc = .FALSE.
       !
       CALL log_pop( subname )
       !
   END SUBROUTINE overlap_deallocate


!**********************************************************
   REAL(dbl) FUNCTION overlap_memusage()
   !**********************************************************
   IMPLICIT NONE
       !
       REAL(dbl) :: cost
       !
       cost = ZERO
       IF ( ALLOCATED(Mkb) )    cost = cost + REAL(SIZE(Mkb))    * 16.0_dbl
       IF ( ALLOCATED(ca) )     cost = cost + REAL(SIZE(ca))     * 16.0_dbl
       !
       overlap_memusage = cost / 1000000.0_dbl
       !
   END FUNCTION overlap_memusage


!**********************************************************
   SUBROUTINE overlap_write(iun,tag)
   !**********************************************************
   IMPLICIT NONE
       INTEGER,         INTENT(in) :: iun
       CHARACTER(*),    INTENT(in) :: tag
       !
       CHARACTER(13)               :: subname="overlap_write"
       CHARACTER(nstrx)            :: attr
       COMPLEX(dbl),   ALLOCATABLE :: Mkb_aux(:,:,:), ca_aux(:,:)
       INTEGER                     :: iwann, ib, ik, ik_g, ikb, ikb_g, inn
       INTEGER                     :: ierr

       IF ( .NOT. alloc ) RETURN
       CALL log_push ( 'overlap_write' )
       !
       IF ( ionode ) THEN
           !
           CALL iotk_write_begin(iun,TRIM(tag))
           CALL iotk_write_attr(attr,"dimwinx",dimwinx,FIRST=.TRUE.)
           CALL iotk_write_attr(attr,"dimwann",dimwann)
           CALL iotk_write_attr(attr,"nb",nb)
           CALL iotk_write_attr(attr,"nkpts",nkpts_g)
           CALL iotk_write_empty(iun,"DATA",ATTR=attr)
           !
       ENDIF

       !  
       ! writing overlap
       !  
       ALLOCATE( Mkb_aux(dimwinx, dimwinx, nb/2), STAT=ierr )
       IF ( ierr/=0 ) CALL errore(subname,"allocating Mkb_aux", ABS(ierr))

       IF (ionode) CALL iotk_write_begin(iun,'OVERLAP')
       !
       DO ik_g = 1, nkpts_g
           !
           ik = ik_g -iks +1

           !
           ! get data
           !
           IF ( mpime == iproc_g( ik_g ) ) THEN
               Mkb_aux = Mkb(:,:,:,ik)
           ENDIF
           ! 
           CALL mp_get( Mkb_aux, Mkb_aux, mpime, ionode_id, iproc_g(ik_g), 1 )

           IF ( ionode ) THEN
               !
               CALL iotk_write_attr(attr,'dimwin_k',dimwin(ik_g),FIRST=.TRUE.)
               CALL iotk_write_attr(attr,'nneigh',nb)
               CALL iotk_write_begin(iun,'kpoint'//TRIM(iotk_index(ik_g)), ATTR=attr)
               !
               ! neighbours
               !
               DO inn = 1, nb / 2
                   !
                   ib    = nnpos( inn )
                   ikb_g = nnlist(ib, ik_g)
                   !
                   CALL iotk_write_begin(iun, "b-vect"//TRIM(iotk_index(ib)) )
                   !
                   CALL iotk_write_attr(attr,'dimwin_k',dimwin(ik_g),FIRST=.TRUE.)
                   CALL iotk_write_attr(attr,'dimwin_kb',dimwin(ikb_g))
                   CALL iotk_write_empty(iun, 'data', ATTR=attr)
                   !
                   CALL iotk_write_dat(iun,'mkb', Mkb_aux(1:dimwin(ik_g),1:dimwin(ikb_g),inn) )
                   CALL iotk_write_dat(iun,'mkb_abs', ABS(Mkb_aux(1:dimwin(ik_g),1:dimwin(ikb_g),inn)) )
                   !
                   CALL iotk_write_end(iun, "b-vect"//TRIM(iotk_index(ib)) )
                   !
               ENDDO
               !
               CALL iotk_write_end(iun,'kpoint'//TRIM(iotk_index(ik_g)))
               !
           ENDIF
           !
       ENDDO
       !
       CALL mp_barrier()
       !
       IF (ionode) CALL iotk_write_end(iun,'OVERLAP')
       !
       DEALLOCATE( Mkb_aux, STAT=ierr )
       IF ( ierr/=0 ) CALL errore(subname,"deallocating Mkb_aux", ABS(ierr))
    

       !  
       ! writing projections  
       !  
       ALLOCATE( ca_aux(dimwinx, dimwinx), STAT=ierr )
       IF ( ierr/=0 ) CALL errore(subname,"allocating ca_aux", ABS(ierr))
       !
       IF (ionode) CALL iotk_write_begin(iun,'PROJECTIONS')
       !
       DO ik_g = 1, nkpts_g
           !
           ik = ik_g -iks +1
        
           !
           ! get data
           !
           IF ( mpime == iproc_g( ik_g ) ) THEN
               ca_aux = ca(:,:,ik)
           ENDIF
           ! 
           CALL mp_get( ca_aux, ca_aux, mpime, ionode_id, iproc_g(ik_g), 1 )
           !
           IF ( ionode ) THEN
               !
               CALL iotk_write_attr(attr,'dimwin',dimwin(ik_g),FIRST=.TRUE.)
               CALL iotk_write_begin(iun,'kpoint'//TRIM(iotk_index(ik_g)), ATTR=attr)
               !
               DO iwann=1,dimwann
                   !
                   CALL iotk_write_dat(iun,'wannier'//TRIM(iotk_index(iwann)), &
                                            ca_aux(1:dimwin(ik_g),iwann) )
                   CALL iotk_write_dat(iun,'wannier_abs'//TRIM(iotk_index(iwann)), &
                                           ABS(ca_aux(1:dimwin(ik_g),iwann)) )
               ENDDO
               !
               CALL iotk_write_end(iun,'kpoint'//TRIM(iotk_index(ik_g)))
               !
           ENDIF
           !
       ENDDO
       !
       CALL mp_barrier()
       !
       IF ( ionode ) THEN
           !
           CALL iotk_write_end(iun,'PROJECTIONS')
           CALL iotk_write_end(iun,TRIM(tag))
           !
       ENDIF
       !
       DEALLOCATE( ca_aux, STAT=ierr )
       IF ( ierr/=0 ) CALL errore(subname,"deallocating ca_aux", ABS(ierr))
       !
       CALL log_pop ( 'overlap_write' )
       !
   END SUBROUTINE overlap_write


!**********************************************************
   SUBROUTINE overlap_read( iun, tag, found, loverlap, lprojection )
   !**********************************************************
   !
   ! allocate (if necessary) and read projections and overlaps integrals
   ! LOVERLAP and LPROJECTION make the routine read or not the specified
   ! quantities (default is .TRUE. for both)
   !
   IMPLICIT NONE
       INTEGER,           INTENT(in) :: iun
       CHARACTER(*),      INTENT(in) :: tag
       LOGICAL,           INTENT(out):: found
       LOGICAL, OPTIONAL, INTENT(in) :: loverlap, lprojection

       CHARACTER(nstrx)   :: attr
       CHARACTER(12)      :: subname="overlap_read"
       LOGICAL            :: loverlap_, lprojection_
       INTEGER            :: dimwinx_, dimwann_, nb_, nkpts_g_
       INTEGER            :: ik, ik_g, ib, inn
       INTEGER            :: iwann, dimwin_, dimwin_k, dimwin_kb, nneigh_
       INTEGER            :: ierr

       CALL log_push( subname )
       !
       ! define the default
       !
       loverlap_ = .TRUE.
       lprojection_ = .TRUE.
       !
       IF ( PRESENT(loverlap) ) loverlap_ = loverlap
       IF ( PRESENT(lprojection) ) lprojection_ = lprojection
 
       
       CALL iotk_scan_begin(iun,TRIM(tag),FOUND=found,IERR=ierr)
       IF (.NOT. found) RETURN
       IF (ierr>0)  CALL errore(subname,'Wrong format in tag '//TRIM(tag),ierr)
       !
       found = .TRUE.

       CALL iotk_scan_empty(iun,'DATA',ATTR=attr,IERR=ierr)
       IF (ierr/=0) CALL errore(subname,'Unable to find tag DATA',ABS(ierr))

       CALL iotk_scan_attr(attr,'dimwinx',dimwinx_,IERR=ierr)
       IF (ierr/=0) CALL errore(subname,'Unable to find attr DIMWINX',ABS(ierr))
       !
       CALL iotk_scan_attr(attr,'dimwann',dimwann_,IERR=ierr)
       IF (ierr/=0) CALL errore(subname,'Unable to find attr DIMWANN',ABS(ierr))
       !
       CALL iotk_scan_attr(attr,'nb',nb_,IERR=ierr)
       IF (ierr/=0) CALL errore(subname,'Unable to find attr NNX',ABS(ierr))
       !
       CALL iotk_scan_attr(attr,'nkpts',nkpts_g_,IERR=ierr)
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
          IF ( nkpts_g_ /= nkpts_g) CALL errore(subname,'Invalid NKPTS',ABS(nkpts_g_-nkpts_g))
          IF ( nb_ /= nb)           CALL errore(subname,'Invalid NB',ABS(nb_-nb))
       ELSE
          nkpts_g = nkpts_g_
          !
          ! in the actual implementation of b-vector stuff, we need
          ! some initializations coming from kpoints module
          !
          CALL errore(subname,'kpoints should be allocated', 71)
       ENDIF
       !
       IF ( subspace_alloc ) THEN
           IF ( dimwann_ /= dimwann) &
                CALL errore(subname,'Invalid DIMWANN',ABS(dimwann_-dimwann))
       ELSE
          dimwann = dimwann_
       ENDIF
       !
       !
       IF ( .NOT. alloc ) CALL overlap_allocate()       

       !
       ! read overlap
       !
       IF ( loverlap_ ) THEN
           !
           Mkb(:,:,:,:) = CZERO
           !
           CALL iotk_scan_begin(iun,'OVERLAP',IERR=ierr)
           IF (ierr/=0) CALL errore(subname,'scanning for OVERLAP',ABS(ierr))
           ! 
           DO ik_g= iks, ike
               !
               ik = ik_g -iks +1
               !
               CALL iotk_scan_begin(iun,'kpoint'//TRIM(iotk_index(ik_g)), ATTR=attr, IERR=ierr)
               IF (ierr/=0) CALL errore(subname,'scanning for kpoint',ik_g)
               !
               CALL iotk_scan_attr(attr,'dimwin_k',dimwin_k,IERR=ierr)
               IF (ierr/=0) CALL errore(subname,'scanning for dimwin',ik_g)
               !
               IF ( dimwin_k > dimwinx ) CALL errore(subname,'dimwin too large',dimwin_k)
               !
               CALL iotk_scan_attr(attr,'nneigh',nneigh_,IERR=ierr)
               IF (ierr/=0) CALL errore(subname,'scanning for nneigh_',ik_g)
               !
               IF ( nneigh_ /= nb ) CALL errore(subname,'nniegh too large',nneigh_)

               DO inn = 1,nneigh_ / 2
                   !
                   ib  = nnpos( inn )
                   !
                   CALL iotk_scan_begin(iun, 'b-vect'//TRIM(iotk_index(ib)), IERR=ierr)
                   IF (ierr/=0) CALL errore(subname,'scanning for b-vect',inn)
                   !
                   CALL iotk_scan_empty(iun, 'data', ATTR=attr, IERR=ierr)
                   IF (ierr/=0) CALL errore(subname,'scanning for data',inn)
                   !
                   CALL iotk_scan_attr(attr,'dimwin_kb',dimwin_kb,IERR=ierr)
                   IF (ierr/=0) CALL errore(subname,'scanning for dimwin_kb',inn)
                   !
                   IF ( dimwin_kb > dimwinx ) CALL errore(subname,'dimwin too large',dimwin_kb)
                   ! 
                   CALL iotk_scan_dat(iun,'mkb', Mkb(1:dimwin_k,1:dimwin_kb,inn,ik),IERR=ierr )
                   IF (ierr/=0) CALL errore(subname,'scanning for mkb',inn)
                   !
                   CALL iotk_scan_end(iun, 'b-vect'//TRIM(iotk_index(ib)), IERR=ierr)
                   IF (ierr/=0) CALL errore(subname,'scanning end for b-vect',inn)
                   !
               ENDDO
               !
               CALL iotk_scan_end(iun,'kpoint'//TRIM(iotk_index(ik_g)), IERR=ierr)
               IF (ierr/=0) CALL errore(subname,'scanning for ending kpoint',ik_g)
               !
           ENDDO
           ! 
           CALL iotk_scan_end(iun,'OVERLAP',IERR=ierr)
           IF (ierr/=0) CALL errore(subname,'scanning for ending OVERLAP',ABS(ierr))
           !
       ENDIF

       !
       ! read projections
       !
       IF ( lprojection_ ) THEN
           !
           ca(:,:,:) = CZERO
           !
           CALL iotk_scan_begin(iun,'PROJECTIONS',IERR=ierr)
           IF (ierr/=0) CALL errore(subname,'scanning for PROJECTIONS',ABS(ierr))
          
           DO ik_g = iks, ike
               !
               ik = ik_g -iks +1
               !
               CALL iotk_scan_begin(iun,'kpoint'//TRIM(iotk_index(ik_g)), ATTR=attr, IERR=ierr)
               IF (ierr/=0) CALL errore(subname,'scanning for kpoint',ik_g)
               !
               CALL iotk_scan_attr(attr,'dimwin',dimwin_,IERR=ierr)
               IF (ierr/=0) CALL errore(subname,'scanning for dimwin',ik_g)
               !
               IF ( dimwin_ > dimwinx ) CALL errore(subname,'dimwin too large',dimwin_)
               !
               DO iwann=1,dimwann
                   !
                   CALL iotk_scan_dat(iun,'wannier'//TRIM(iotk_index(iwann)), &
                                            ca(1:dimwin_,iwann,ik),IERR=ierr )
                   IF (ierr/=0) CALL errore(subname,'scanning for wannier',iwann)
                   !
               ENDDO
               !
               CALL iotk_scan_end(iun,'kpoint'//TRIM(iotk_index(ik_g)), IERR=ierr)
               IF (ierr/=0) CALL errore(subname,'scanning for ending kpoint',ik_g)
               !
           ENDDO
           ! 
           CALL iotk_scan_end(iun,'PROJECTIONS',IERR=ierr)
           IF (ierr/=0) CALL errore(subname,'scanning for ending PROJECTIONS',ABS(ierr))
           !
       ENDIF
       !
       !
       CALL iotk_scan_end(iun,TRIM(tag),IERR=ierr)
       IF (ierr/=0)  CALL errore(subname,'Unable to end tag '//TRIM(tag),ABS(ierr))
       !
       CALL log_pop ( subname )
       !
   END SUBROUTINE overlap_read

END MODULE overlap_module

