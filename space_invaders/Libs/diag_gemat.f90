
!*********************************************************
SUBROUTINE diag_gemat(matrix,dim,eig,vcts,vcts_flag)
  !*********************************************************
   USE kinds, ONLY: dbl
   IMPLICIT NONE

! <INFO>
!
! General utility interfacing to Lapack ZGEEV
! Diagonalizes complex matrices in general form
! INPUT: DIM, MATRIX, VCTS_FLAG 
! OUTPUT: EIG, VCTS
!
! NOTA BENE1: VCTS_FLAG = "N" no eigenvectors are
!                             computed
!                         "R" right eigenvectors required
!                         "L" left eigenvectors required
! NOTA BENE2: if VCTS_FLAG is "L" or "R"
!             output eigenvectors are set 
!             columnwise in VCTS
! </INFO>
  
   INTEGER, INTENT(in)              :: dim
   CHARACTER(1), INTENT(in)         :: vcts_flag
   COMPLEX(dbl), INTENT(in)         :: matrix(dim,dim)
   COMPLEX(dbl), INTENT(out)        :: eig(dim),        &
                                       vcts(dim,dim) 
 
   INTEGER                          :: info
   REAL(dbl)                        :: RWORK(2*dim)
   COMPLEX(dbl)                     :: matrix_work(dim,dim) 
   COMPLEX(dbl)                     :: eig_work(dim) 
   COMPLEX(dbl)                     :: vcts_work_L(dim,dim) 
   COMPLEX(dbl)                     :: vcts_work_R(dim,dim) 
   COMPLEX(dbl)                     :: WORK(2*dim)

!-----------------------------------


   matrix_work(:,:) = matrix(:,:)
  
   SELECT CASE (vcts_flag)
   CASE ('N') 
       CALL ZGEEV('N','N',dim,matrix_work,dim,eig_work,vcts_work_L,dim,   &
                   vcts_work_R,dim,WORK,2*dim,RWORK,info)
   CASE ('L') 
       CALL ZGEEV('V','N',dim,matrix_work,dim,eig_work,vcts_work_L,dim,   &
                   vcts_work_R,dim,WORK,2*dim,RWORK,info)
   CASE ('R') 
       CALL ZGEEV('N','V',dim,matrix_work,dim,eig_work,vcts_work_L,dim,   &
                   vcts_work_R,dim,WORK,2*dim,RWORK,info)

   CASE DEFAULT
       CALL errore('diag_gemat','Illegal VCTS_FLAG in input',1)

   END SELECT




   IF ( info < 0 ) THEN
       CALL errore('diag_gemat','An input parameter of ZGEEV has an illegal value',-info)
   END IF
   IF( info > 0 ) THEN
       CALL errore('diag_gemat','Convergence failure in ZGEEV',info)
   END IF



   eig(:) = eig_work(:) 
   vcts(:,:) = 0.0
   IF ( vcts_flag == 'L' )  vcts(:,:) = vcts_work_L(:,:) 
   IF ( vcts_flag == 'R' )  vcts(:,:) = vcts_work_R(:,:) 


   END SUBROUTINE diag_gemat





