! 
! Copyright (C) 2004 Andrea Ferretti
! 
! This file is distributed under the terms of the 
! GNU General Public License. See the file `License' 
! in the root directory of the present distribution, 
! or http://www.gnu.org/copyleft/gpl.txt . 
! 
   MODULE dyn_op_read

   USE kinds, ONLY: sgl, i4b, dbl
   USE timing_module, ONLY : timing

   IMPLICIT NONE
   SAVE

! <INFO>
!  
! This module reads an energy dependent operator to file
! Different routines are available for the cases
! FORM=DIAGONAL or FULL_MATRIX
!
! the format is the following:
!
!-----------------------------------------------------
!
! Nv dim Nisp Nomega
! index_band_start  ! the absolute index of the first band
! analiticity       ! integer FLAG (see below)
! form              ! integer FLAG (see below)
! basis             ! integer FLAG (see below)
!
! Nomega
! (energy(i), i=1,Nomega)
!
!
!
! Nv                ! R or k number depending on BASIS
! do i=1,Nv
!     (Vct(j,i), j=1,3)
! end do
!
! Nv  dim  Nisp  Nomega          ! repetita iuvant
!   do ie= 1,Nomega
!     do isp=1,Nisp      
!       do k=1,Nv
!         do i2=1,dim
!         do i1=1,dim            ! the second loop is done if the
!            op(i1,i2,k,isp,ie)  ! form=FULL_MATRIX
!         enddo
!         enddo
!       enddo
!     enddo
!   enddo
!
!-----------------------------------------------------
!
! NOTA: in the case WANNIER, DIM is matrix dimension, Nv
!       plays the role of number of lattice sites
!
!-----------------------------------------------------
! INTEGER FLAGS
!
! analiticity:             1   time_ord
!                          2   retarded
!                          3   advanced
!                          4   greater
!                          5   lesser
!
! form :                   0   diagonal
!                          1   full_matrix
!
! basis:                   0   bloch
!                          1   wannier
!
!-----------------------------------------------------
!
! </INFO>
   

CONTAINS


!*********************************************************
   SUBROUTINE read_dyn_op( Nv, Vct, dim, index_band, Nisp, Nomega, E, Opr, name, analit, form, basis )
   !*********************************************************
   IMPLICIT NONE

   INTEGER, PARAMETER               :: in=10

   CHARACTER(LEN=*),INTENT(in)      :: name
   CHARACTER(LEN=*),INTENT(out)     :: analit, form, basis
   INTEGER,INTENT(out)              :: Nv, dim, index_band, Nisp, Nomega 
   REAL(dbl),POINTER    :: E(:), Vct(:,:)
   COMPLEX(dbl),POINTER :: Opr(:,:,:,:,:)

   INTEGER                          :: dum(4)
   INTEGER                          :: ianalit, iform, ibasis 
   REAL(sgl),ALLOCATABLE            :: E_fmt(:), Vct_fmt(:,:)
   COMPLEX(sgl),ALLOCATABLE         :: Opr_fmt(:,:,:,:,:)
 
   INTEGER                          :: isp,i,j,k,ios
   INTEGER                          :: j1,j2, ierr



!-------------------------------------------

   CALL timing('read_dyn_op',OPR='start')

!
! reading
!
   OPEN(unit=in,file=name,form='unformatted',status='old',IOSTAT=ios)
      IF (ios /= 0) CALL errore('read_dyn_op','Unable to read from '//name,ios)

      READ(in) Nv,dim,Nisp,Nomega
      READ(in) index_band
      READ(in) ianalit
      READ(in) iform
      READ(in) ibasis


!
! form check
!
     IF ( iform /= 0 .AND. iform /= 1 )   &
           CALL errore('read_dyn_op','Invalid form',3)


!
! allocation
!
      ALLOCATE( E(Nomega), STAT=ierr )
         IF (ierr/=0) CALL errore('read_dyn_op','allocating E', Nomega)
      ALLOCATE( E_fmt(Nomega), STAT=ierr )   
         IF (ierr/=0) CALL errore('read_dyn_op','allocating E_fmt', Nomega)
      ALLOCATE( Opr(dim,dim,Nv,Nisp,Nomega), STAT=ierr )
         IF (ierr/=0) CALL errore('read_dyn_op','allocating Opr', & 
                           dim**2 * Nv * Nisp * Nomega)
      ALLOCATE( Opr_fmt(dim,dim,Nv,Nisp,Nomega), STAT=ierr ) 
         IF (ierr/=0) CALL errore('read_dyn_op','allocating Opr_fmt', & 
                           dim**2 * Nv * Nisp * Nomega)
      ALLOCATE( Vct(3,Nv), STAT=ierr )
         IF (ierr/=0) CALL errore('read_dyn_op','allocating Vct', 3* Nv)
      ALLOCATE( Vct_fmt(3,Nv), STAT=ierr ) 
         IF (ierr/=0) CALL errore('read_dyn_op','allocating Vct_fmt', 3* Nv)


      READ(in) dum(1)
      IF ( dum(1) /= Nomega )          &
           CALL errore('read_dyn_op','Wrong format of '//TRIM(name)//' in Nomega' ,4)
      READ(in) (E_fmt(i), i=1,Nomega)

    
      
      READ(in) dum(1)
      IF ( dum(1) /= Nv )          &
          CALL errore('read_dyn_op','Wrong format of '//TRIM(name)//' in Nv' ,4)
      DO k=1,Nv
          READ(in) (Vct_fmt(i,k), i=1,3)
      ENDDO
   

      READ(in) (dum(i),i=1,4)
      IF ( dum(1) /= Nv .OR. dum(2) /= dim .OR.          &
           dum(3) /= Nisp .OR. dum(4) /= Nomega    )     &
           CALL errore('read_dyn_op','Wrong format of '//TRIM(name)//' : inconsistency' ,4)


      Opr_fmt(:,:,:,:,:) = 0.0
      IF ( iform == 0 )  THEN

          DO i=1,Nomega
              DO isp=1,Nisp
                  DO k=1,Nv
                      READ(in) (Opr_fmt(j,j,k,isp,i), j=1,dim)
                  ENDDO
              ENDDO
          ENDDO

      ELSE

          DO i=1,Nomega
              DO isp=1,Nisp
                  DO k=1,Nv
                      DO j2=1,dim
                          READ(in) (Opr_fmt(j1,j2,k,isp,i), j1=1,dim)
                      END DO
                  ENDDO
              ENDDO
          ENDDO

      ENDIF


   CLOSE(unit=in)


!
! converting flags
!

   SELECT CASE ( ianalit )
   CASE ( 1 )
       analit='time_ord'
   CASE ( 2 )
       analit='retarded'
   CASE ( 3 )
       analit='advanced'
   CASE ( 4 )
       analit='greater'  
   CASE ( 5 )
       analit='lesser'
   CASE DEFAULT
       CALL errore('read_dyn_op','Unknown analiticity',5)
   END SELECT


   SELECT CASE ( iform )
   CASE ( 0 )
      form='diagonal'
   CASE ( 1 )
      form='full_matrix'
   CASE DEFAULT
      CALL errore('read_dyn_op','Unknown matrix format',6)
   END SELECT


   SELECT CASE ( ibasis )
   CASE ( 0 )
      basis='bloch'
   CASE ( 1 )
      basis='wannier'
   CASE DEFAULT
      CALL errore('read_dyn_op','Unknown basis',7)
   END SELECT


!
! changing format from input
!
   E(:)            = E_fmt(:)
   Opr(:,:,:,:,:)  = Opr_fmt(:,:,:,:,:)
   Vct(:,:)        = Vct_fmt(:,:)


   DEALLOCATE(  E_fmt, STAT=ierr )
       IF (ierr/=0) CALL errore('read_dyn_op','deallocating E_fmt', ABS(ierr))
   DEALLOCATE(  Opr_fmt, STAT=ierr )
       IF (ierr/=0) CALL errore('read_dyn_op','deallocating Opr_fmt', ABS(ierr))
   DEALLOCATE(  Vct_fmt, STAT=ierr )  
       IF (ierr/=0) CALL errore('read_dyn_op','deallocating Vct_fmt', ABS(ierr))

   CALL timing('read_dyn_op',OPR='stop')

   END SUBROUTINE read_dyn_op


!   SUBROUTINE dyn_dum( a )
!     COMPLEX(dbl),POINTER :: a(:,:,:,:,:)
!     ALLOCATE( a(2,2,2,2,2) )
!     RETURN
!   END SUBROUTINE dyn_dum


END MODULE dyn_op_read
