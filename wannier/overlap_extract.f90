! 
! Copyright (C) 2004 Andrea Ferretti
! 
! This file is distributed under the terms of the 
! GNU General Public License. See the file `License' 
! in the root directory of the present distribution, 
! or http://www.gnu.org/copyleft/gpl.txt . 
! 
!*********************************************************
SUBROUTINE overlap_extract(dimwann)
   !*********************************************************
   USE kinds
   USE constants,  ONLY : CZERO
   USE parameters, ONLY : nstrx
   USE timing_module, ONLY : timing
   USE io_module,  ONLY : stdout, ovp_unit, space_unit, ioname
   USE files_module, ONLY : file_open, file_close
   USE subspace_module, ONLY : eamp, subspace_read
   USE windows_module,  ONLY : dimwinx, dimwin, windows_read
   USE kpoints_module,  ONLY : nnx, nkpts, nntot, nnlist 
   USE overlap_module,  ONLY : cm, ca, overlap_allocate, overlap_deallocate, overlap_read 
   IMPLICIT NONE

! <INFO>
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
! </INFO>

   INTEGER,  INTENT(in)      :: dimwann
   CHARACTER(15)             :: subname="overlap_extract"

   COMPLEX(dbl), ALLOCATABLE :: cm_tmp(:,:,:,:)
   COMPLEX(dbl), ALLOCATABLE :: ca_tmp(:,:,:)

   LOGICAL                   :: lfound
   CHARACTER(nstrx)          :: filename 
   INTEGER                   :: ik1,ik2,nn
   INTEGER                   :: m,n,i,j,l
   INTEGER                   :: ierr
   

! ... end of declarations

   CALL timing('overlap_extract',OPR='start')

!
! ... reading subspace and windows data
!
   CALL ioname('subspace',filename)
   CALL file_open(space_unit,TRIM(filename),PATH="/",ACTION="read",FORM="formatted")
        CALL windows_read(space_unit,"WINDOWS",lfound)
        IF ( .NOT. lfound ) CALL errore(subname,"unable to find WINDOWS",1) 
        CALL subspace_read(space_unit,"SUBSPACE",lfound)
        IF ( .NOT. lfound ) CALL errore(subname,"unable to find SUBSPACE",1) 
   CALL file_close(space_unit,PATH="/",ACTION="read")

   CALL ioname('subspace',filename,LPATH=.FALSE.)
   WRITE( stdout,"(/,'  Subspace data read from file: ',a)") TRIM(filename)   
    
!
! ... reading overlap and projections
!
   CALL ioname('overlap_projection',filename)
   CALL file_open(ovp_unit,TRIM(filename),PATH="/", ACTION="read",FORM="formatted")
        CALL overlap_read(ovp_unit,"OVERLAP_PROJECTION",lfound)
        IF ( .NOT. lfound ) CALL errore(subname,"unable to find OVERLAP_PROJECTION",1) 
   CALL file_close(ovp_unit,PATH="/", ACTION="read")        

   CALL ioname('overlap_projection',filename,LPATH=.FALSE.)
   WRITE( stdout,"('  Overlap and projections read from file: ',a)") TRIM(filename)   

!
! ... here allocate the temporary variables for the extracted CM and CA 
!
   ALLOCATE( cm_tmp(dimwann,dimwann,nnx,nkpts), STAT=ierr ) 
      IF (ierr/=0) CALL errore(subname,"allocating cm_tmp",ABS(ierr))
   ALLOCATE( ca_tmp(dimwann,dimwann,nkpts), STAT=ierr ) 
      IF (ierr/=0) CALL errore(subname,"allocating cm_tmp",ABS(ierr))


!
! ... Transform the original overlaps |u0 nk> according to the wfc transformation rule:
!
!     | u mk > = \sum_n |u0 nk > * EAMP_nm k)
!
!     where | u mk > are the eigenvector of the hamiltonian on the Wannier subspace
!     The new overlaps become:
!
!     < u mk | u nk+b > = \sum_{ij} EAMP^{daga}_mi (k) * < u0 ik | u0 jk+b > * EAMP_jn (k+b)
!
!     As well, the projection on the input localized orbitals are given:
!     ca(m,i,k) = < u mk | phi_i > and thus
!     
!     ca(m,i,k) = \sum_l EAMP^{daga}_ml * ca0(l,i,k)
!


   !
   ! CM
   !
   DO ik1 = 1, nkpts
      DO nn= 1, nntot( ik1 )
         ik2 = nnlist( ik1, nn )

         DO n=1,dimwann  
         DO m=1,dimwann  
             
              cm_tmp(m,n,nn,ik1) = CZERO
              DO j=1,dimwin(ik2)
              DO i=1,dimwin(ik1)
                  cm_tmp( m,n,nn,ik1 ) = cm_tmp( m,n,nn,ik1 )  +                 &
                          CONJG( eamp(i,m,ik1)) * cm(i,j,nn,ik1) * eamp(j,n,ik2)
              ENDDO
              ENDDO
         ENDDO    
         ENDDO    
      ENDDO
   ENDDO

   !
   ! CA 
   !
   DO ik1 = 1, nkpts
      DO i=1,dimwann    ! | phi_i >     localized orb
      DO m=1,dimwann    ! < u_mk |      bloch eigenstate

         ca_tmp(m,i,ik1) = CZERO
         DO l=1,dimwin(ik1)
              ca_tmp( m,i,ik1 ) = ca_tmp( m,i,ik1 ) + CONJG( eamp(l,m,ik1) ) * ca(l,i,ik1) 
         ENDDO    
      ENDDO
      ENDDO
   ENDDO


!
! ... finally redefine the CM variable 
!
   CALL overlap_deallocate()
   dimwinx = dimwann
   CALL overlap_allocate()

   cm(:,:,:,:) = cm_tmp(:,:,:,:)
   ca(:,:,:) =   ca_tmp(:,:,:)

!
! ... cleaning
   DEALLOCATE( cm_tmp, ca_tmp, STAT=ierr) 
      IF (ierr/=0) CALL errore(subname,"deallocating cm_tmp or ca_tmp",ABS(ierr))

   CALL timing('overlap_extract',OPR='stop')

END SUBROUTINE overlap_extract


