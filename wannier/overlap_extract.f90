! 
! Copyright (C) 2004 WanT Group
! 
! This file is distributed under the terms of the 
! GNU General Public License. See the file `License' 
! in the root directory of the present distribution, 
! or http://www.gnu.org/copyleft/gpl.txt . 
! 
!*********************************************************
SUBROUTINE overlap_extract(dimwann)
   !*********************************************************
   !
   ! This subroutine extract the overlap integrals from the already
   ! calculated overlaps written in the disentangle procedure
   !
   ! The disentangle overlaps < u_mk | u_nk+b > are calculated
   ! for the bare DFT wavefunctions. Here the overlap related to
   ! the wfcs obtained by disentangle are computed. 
   ! The main task is essentially a moltiplication of the 
   ! old overlaps by the matrix defining the new subspace.
   !
   ! The same procedure is done here in order to extract the 
   ! projection onto the guessed Wannier functions already
   ! calculated in disentangle as well.
   !
   USE kinds
   USE constants,         ONLY : CZERO
   USE parameters,        ONLY : nstrx
   USE io_module,         ONLY : stdout, ionode, ovp_unit, space_unit, io_name
   USE timing_module,     ONLY : timing
   USE log_module,        ONLY : log_push, log_pop
   USE files_module,      ONLY : file_open, file_close
   USE util_module,       ONLY : mat_mul
   USE subspace_module,   ONLY : eamp, subspace_read, subspace_deallocate
   USE windows_module,    ONLY : dimwinx, dimwin, windows_read
   USE kpoints_module,    ONLY : nkpts, nkpts_g, iks, iproc_g, nb, nnlist, nnpos, nnrev 
   USE overlap_module,    ONLY : Mkb, ca, overlap_allocate, overlap_deallocate, overlap_read 
   
   !
   IMPLICIT NONE

   !
   ! I/O variables
   !
   INTEGER,       INTENT(IN) :: dimwann
   !
   ! local variables
   !
   CHARACTER(15)             :: subname="overlap_extract"
   !
   COMPLEX(dbl), ALLOCATABLE :: Mkb_tmp(:,:,:,:)
   COMPLEX(dbl), ALLOCATABLE :: ca_tmp(:,:,:)
   COMPLEX(dbl), ALLOCATABLE :: caux1(:,:)
   !
   LOGICAL                   :: lfound
   CHARACTER(nstrx)          :: filename 
   INTEGER                   :: ik_proc, ikb_proc
   INTEGER                   :: ik, ik_g, ikb, ikb_g, ib, inn
   INTEGER                   :: ierr
   ! 
   ! end of declarations
   ! 

!
!-----------------------------
! main body
!-----------------------------
!
   CALL timing( subname, OPR='start')
   CALL log_push( subname )

   !
   ! reading subspace and windows data
   !
   CALL io_name('space',filename)
   CALL file_open(space_unit,TRIM(filename),PATH="/",ACTION="read", IERR=ierr)
   IF ( ierr/=0 ) CALL errore(subname,"opening "//TRIM(filename), ABS(ierr) )
        !
        CALL windows_read(space_unit,"WINDOWS",lfound)
        IF ( .NOT. lfound ) CALL errore(subname,"unable to find WINDOWS",1) 
        !
        CALL subspace_read(space_unit,"SUBSPACE",lfound, LLAMP=.FALSE.)
        IF ( .NOT. lfound ) CALL errore(subname,"unable to find SUBSPACE",1) 
        !
   CALL file_close(space_unit,PATH="/",ACTION="read", IERR=ierr)
   IF ( ierr/=0 ) CALL errore(subname,"closing "//TRIM(filename), ABS(ierr) )

   CALL io_name('space',filename,LPATH=.FALSE., LPROC=.FALSE.)
   IF (ionode) WRITE( stdout,"(/,'  Subspace data read from file: ',a)") TRIM(filename)   
    
   !
   ! reading overlap and projections
   !
   CALL io_name('overlap_projection',filename)
   CALL file_open(ovp_unit,TRIM(filename),PATH="/", ACTION="read", IERR=ierr)
   IF ( ierr/=0 ) CALL errore(subname,"opening "//TRIM(filename), ABS(ierr) )
        !
        CALL overlap_read(ovp_unit,"OVERLAP_PROJECTION",lfound)
        IF ( .NOT. lfound ) CALL errore(subname,"unable to find OVERLAP_PROJECTION",1) 
        !
   CALL file_close(ovp_unit,PATH="/", ACTION="read", IERR=ierr)        
   IF ( ierr/=0 ) CALL errore(subname,"opening "//TRIM(filename), ABS(ierr) )

   CALL io_name('overlap_projection',filename,LPATH=.FALSE., LPROC=.FALSE.)
   IF (ionode) WRITE( stdout,"('  Overlap and projections read from file: ',a)") TRIM(filename)   


   !
   ! here allocate the temporary variables for the extracted CM and CA 
   !
   ALLOCATE( Mkb_tmp(dimwann,dimwann,nb/2,nkpts), STAT=ierr ) 
   IF (ierr/=0) CALL errore(subname,"allocating Mkb_tmp",ABS(ierr))
   !
   ALLOCATE( ca_tmp(dimwann,dimwann,nkpts), STAT=ierr ) 
   IF (ierr/=0) CALL errore(subname,"allocating ca_tmp",ABS(ierr))
   !
   ALLOCATE( caux1(dimwinx,dimwinx), STAT=ierr ) 
   IF (ierr/=0) CALL errore(subname,"allocating caux1",ABS(ierr))


   !
   ! Transform the original overlaps |u0 nk> according to the wfc transformation rule:
   ! 
   !   | u mk > = \sum_n |u0 nk > * EAMP_nm k)
   !
   ! where | u mk > are the eigenvector of the hamiltonian on the Wannier subspace
   ! The new overlaps become:
   !
   !   < u mk | u nk+b > = \sum_{ij} EAMP^{daga}_mi (k) * < u0 ik | u0 jk+b > * EAMP_jn (k+b)
   !
   ! As well, the projection on the input localized orbitals are given:
   ! ca(m,i,k) = < u mk | phi_i > and thus
   !     
   ! ca(m,i,k) = \sum_l EAMP^{daga}_ml * ca0(l,i,k)
   !

   !
   ! Overlap integrals
   !
   DO ik = 1, nkpts
       !
       ik_g = ik +iks -1
       !
       DO inn = 1, nb / 2
           !
           ib    = nnpos ( inn )
           ikb_g = nnlist( ib, ik_g )

           !
           ! perform the main task
           !
           CALL mat_mul(caux1, eamp(:,:,ik_g), 'C', Mkb(:,:,inn,ik), 'N', &
                        dimwann, dimwin(ikb_g), dimwin(ik_g) )
           CALL mat_mul(Mkb_tmp(:,:,inn,ik), caux1, 'N', eamp(:,:,ikb_g), 'N', & 
                        dimwann, dimwann, dimwin(ikb_g) )
           !
       ENDDO
       !
   ENDDO

   !
   ! Projections
   !
   DO ik = 1, nkpts
       !
       ik_g = ik + iks -1
       !
       CALL mat_mul( ca_tmp(:,:,ik), eamp(:,:,ik_g), 'C', ca(:,:,ik), 'N',  &
                     dimwann, dimwann, dimwin(ik_g) )
       !
   ENDDO
   !
   ! memory cleanup
   ! lamp is not allocated, we just leave wan_eig alloc
   DEALLOCATE( eamp, STAT=ierr )
   IF ( ierr/=0 ) CALL errore(subname,'deallocating eamp', ABS(ierr))


   !
   ! finally redefine the CM iand CA variables 
   !
   CALL overlap_deallocate()
   !
   dimwinx = dimwann
   !
   CALL overlap_allocate( MEMUSAGE="low" )
   !
   !
   Mkb(:,:,:,:) = Mkb_tmp(:,:,:,:)
   !
   ca(:,:,:)    = ca_tmp(:,:,:)

   !
   ! cleaning
   !
   DEALLOCATE( Mkb_tmp, ca_tmp, STAT=ierr) 
   IF (ierr/=0) CALL errore(subname,"deallocating Mkb_tmp, ca_tmp",ABS(ierr))
   !
   DEALLOCATE( caux1, STAT=ierr ) 
   IF (ierr/=0) CALL errore(subname,"deallocating caux1",ABS(ierr))

   CALL timing( subname, OPR='stop')
   CALL log_pop( subname )

END SUBROUTINE overlap_extract

