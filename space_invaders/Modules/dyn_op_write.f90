
   MODULE dyn_op_write
   USE kinds, ONLY: sgl, dbl, i4b
   IMPLICIT NONE

! <INFO>
!
! This module writes an energy dependent operator to file
! Different routines are available for the cases
! FORM=DIAGONAL or FULL_MATRIX
!
! the format is the following:
!
!-----------------------------------------------------
!
! Nv dim Nisp Nomega
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
! Nv  dim  Nisp  Nomega           ! repetita iuvant
!   do ie= 1,Nomega
!     do isp=1,Nisp
!       do k=1,Nv
!         do i2=1,dim
!         do i1=1,dim             ! the second loop is done if the
!            op(i1,i2,k,isp,ie)   ! form=FULL_MATRIX
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
   SUBROUTINE write_dyn_op(Nv,Vct,dim,Nisp,Nomega,E,Opr,name,analit,form,basis)
   !*********************************************************
   IMPLICIT NONE

   INTEGER, PARAMETER               :: out=10

   CHARACTER(*),INTENT(in)          :: name
   CHARACTER(*),INTENT(in)          :: analit,         &
                                       form,           &
                                       basis
 
   INTEGER,INTENT(in)               :: Nv,             &
                                       dim,            &
                                       Nisp,           &
                                       Nomega 

   INTEGER(i4b)                     :: Nv_fmt,         &
                                       dim_fmt,        &
                                       Nisp_fmt,       &
                                       Nomega_fmt 

   INTEGER(i4b)                     :: ianalit,        &
                                       iform,          &
                                       ibasis 

   REAL(dbl),INTENT(in)             :: E(Nomega),      &
                                       Vct(3,Nv)
   REAL(sgl)                        :: E_fmt(Nomega),  &
                                       Vct_fmt(3,Nv)

   COMPLEX(dbl),INTENT(in)          :: Opr(dim,dim,Nv,Nisp,Nomega)
   COMPLEX(sgl)                     :: Opr_fmt(dim,dim,Nv,Nisp,Nomega)
 

   INTEGER                          :: isp,i,j,k,ios
   INTEGER                          :: j1,j2



!-------------------------------------------




   SELECT CASE ( trim(analit) )
   CASE ( 'time_ord' )
       ianalit=1   
   CASE ( 'retarded' )
       ianalit=2   
   CASE ( 'advanced' )
       ianalit=3   
   CASE ( 'greater' )
       ianalit=4   
   CASE ( 'lesser' )
       ianalit=5   
   CASE DEFAULT
       CALL errore('write_dyn_op','Unknown analiticity',1)
   END SELECT


   SELECT CASE ( trim(form) )
   CASE ( 'diagonal' )
      iform=0
   CASE ( 'full_matrix' )
      iform=1
   CASE DEFAULT
       CALL errore('write_dyn_op','Unknown matrix format',2)
   END SELECT


   SELECT CASE ( trim(basis) )
   CASE ( 'bloch' )
      ibasis=0
   CASE ( 'wannier' )
      ibasis=1
   CASE DEFAULT
       CALL errore('write_dyn_op','Unknown basis',3)
   END SELECT



!
! imposing format for output
!
   E_fmt(:)            = E(:)
   Vct_fmt(:,:)        = Vct(:,:)
   Opr_fmt(:,:,:,:,:)  = Opr(:,:,:,:,:)
   Nv_fmt              = Nv       
   dim_fmt             = dim       
   Nisp_fmt            = Nisp
   Nomega_fmt          = Nomega


!
! writing
!
   OPEN(unit=out,file=name,form='unformatted',status='unknown',IOSTAT=ios)
      IF (ios /= 0) CALL errore('write_dyn_op','Unexpected error opening '//name,ios)

      WRITE(out) Nv_fmt,dim_fmt,Nisp_fmt,Nomega_fmt
      WRITE(out) ianalit
      WRITE(out) iform
      WRITE(out) ibasis

      WRITE(out) Nomega_fmt
      WRITE(out) (E_fmt(i),i=1,Nomega)

      WRITE(out) Nv_fmt
      DO k=1,Nv
          WRITE(out) (Vct_fmt(i,k) , i=1,3)
      END DO
   
      WRITE(out) Nv_fmt,dim_fmt,Nisp_fmt,Nomega_fmt

      IF ( TRIM(form) == 'diagonal' ) THEN

          DO i=1,Nomega
              DO isp=1,Nisp
                  DO k=1,Nv
                     WRITE(out) (Opr_fmt(j,j,k,isp,i), j=1,dim)
                  ENDDO
              ENDDO
          ENDDO

      ELSE

          DO i=1,Nomega
              DO isp=1,Nisp
                  DO k=1,Nv
                      DO j2=1,dim
                          WRITE(out) (Opr_fmt(j1,j2,k,isp,i), j1=1,dim)
                      ENDDO
                  ENDDO
              ENDDO
          ENDDO

      ENDIF
   CLOSE(unit=out)

  

   END SUBROUTINE write_dyn_op



   END MODULE dyn_op_write




