!
! Copyright (C) 2005 WanT Group
!
! This file is distributed under the terms of the
! GNU General Public License. See the file `License'
! in the root directory of the present distribution,
! or http://www.gnu.org/copyleft/gpl.txt .
!
!*******************************************************************
   SUBROUTINE localization_init( mode, dimwann, nkpts, ca, cu, Mkb)
   !*******************************************************************
   !
   ! ...  Initialize the localization procedure in Wannier
   !      according to the input choice of mode:
   !
   !      * 'center_projections'  uses the Ca matrix to find Cu and update Mkb
   !      * 'from_file'  read info from file (used in restart)
   !
   USE kinds, ONLY : dbl
   USE parameters, ONLY : nstrx
   USE timing_module, ONLY : timing
   USE io_module, ONLY : stdout, ioname, wan_unit
   USE files_module, ONLY : file_open, file_close
   USE util_module, ONLY : zmat_unitary, zmat_mul, mat_svd
   USE localization_module, ONLY : localization_read
   USE control_module, ONLY : unitary_thr
   USE kpoints_module, ONLY : nnx
   IMPLICIT NONE

   CHARACTER(*),    INTENT(in)  :: mode      
   INTEGER,         INTENT(in)  :: dimwann, nkpts
   COMPLEX(dbl),    INTENT(in)  :: ca(dimwann,dimwann,nkpts)
   COMPLEX(dbl),    INTENT(out) :: cu(dimwann,dimwann,nkpts)
   COMPLEX(dbl),    INTENT(inout) :: Mkb(dimwann,dimwann,nnx,nkpts)


   !
   ! local variables
   !
   CHARACTER(17)                :: subname="localization_init"
   CHARACTER(nstrx)             :: filename
   REAL(dbl),       ALLOCATABLE :: singvd(:)
   COMPLEX(dbl),    ALLOCATABLE :: cv1(:,:), cv2(:,:)
   LOGICAL                      :: lfound
   INTEGER                      :: i, j, l, ik, ierr, info


!
!---------------------------------------------------------------------


   CALL timing('localization_init', OPR='start')

   !
   ! few checks
   IF ( dimwann <=0 ) CALL errore(subname,'invalid dimwann',-dimwann+1)


   !
   ! here set CU
   !
   SELECT CASE ( TRIM( mode ) )
   CASE DEFAULT
      CALL errore(subname,'Invalid MODE = "'//TRIM(mode)//'"',1)

   CASE ( 'from_file' )

        WRITE( stdout,"(/,'  Initial unitary rotations : from_file')")
            CALL ioname('wannier',filename)
            CALL file_open(wan_unit,TRIM(filename),PATH="/",ACTION="read", &
                           FORM="formatted")
            CALL localization_read(wan_unit,"WANNIER_LOCALIZATION", lfound)
            IF ( .NOT. lfound ) CALL errore(subname,'searching tag "WANNIER_LOCALIZATION"',1)
        CALL file_close(wan_unit,PATH="/",ACTION="read")
        !
        CALL ioname('wannier',filename,LPATH=.FALSE.)
        WRITE( stdout,"(2x,'Unitary matrices read from file: ',a,/)") TRIM(filename)
        

   CASE ( 'center_projections' )
        WRITE( stdout,"(/,'  Initial unitary rotations : projected localized orbitals',/)")
        !
        ! Here we calculate the transformation matrix
        !
        !    cu = cs^{-1/2} * ca,       where  cs = ca*ca^{\dag}.
        !
        ! Using the SVD factorization fo the CA matrix we have
        !
        ! ca = cz * cd * cv^{\dag},      which gives
        ! cu = cz * cv^{dag}
        !
        ! NOTE that lapack routine returns cv^{\dag} directly
        !
        ALLOCATE( singvd(dimwann), STAT=ierr )
           IF( ierr /=0 ) CALL errore('wannier', 'allocating singvd', ABS(ierr) )
        ALLOCATE( cv1(dimwann,dimwann), STAT=ierr )
           IF( ierr /=0 ) CALL errore('wannier', 'allocating cv1 ', ABS(ierr) )
        ALLOCATE( cv2(dimwann,dimwann), STAT=ierr )
           IF( ierr /=0 ) CALL errore('wannier', 'allocating cv2 ', ABS(ierr) )

        DO ik = 1, nkpts
             !
             !
             CALL mat_svd( dimwann, dimwann, ca(:,:,ik), singvd, cv1, cv2 )
             CALL zmat_mul( cu(:,:,ik), cv1, 'N', cv2, 'N', dimwann, dimwann, dimwann )
             !
             ! Unitariery is checked
             !
             IF ( .NOT. zmat_unitary( cu(:,:,ik), SIDE='both', TOLL=unitary_thr )  ) &
                  CALL errore('wannier','SVD yields non unitary matrices', ik)
        ENDDO

        DEALLOCATE( singvd, cv1, cv2, STAT=ierr )
           IF( ierr /=0 ) CALL errore(subname,'deallocating SVD aux', ABS(ierr))

   END SELECT


   !
   ! ... So now we have the U's that rotate the wavefunctions at each k-point.
   !     the matrix elements M_ij have also to be updated
   !
   CALL overlap_update(dimwann, nkpts, cu, Mkb)


   CALL timing('localization_init', OPR='stop')
   RETURN

END SUBROUTINE localization_init



