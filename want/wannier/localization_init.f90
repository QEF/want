!
! Copyright (C) 2005 WanT Group
!
! This file is distributed under the terms of the
! GNU General Public License. See the file `License'
! in the root directory of the present distribution,
! or http://www.gnu.org/copyleft/gpl.txt .
!
!*******************************************************************
   SUBROUTINE localization_init( mode )
   !*******************************************************************
   !
   ! ...  Initialize the localization procedure in Wannier
   !      according to the input choice of mode:
   !
   !      * 'center_projections'  uses the Ca matrix to find Cu
   !      * 'no_guess'  Cu's are set equal to the identity
   !      * 'randomized' Cu's are random unitary matrixes
   !      * 'from_file'  read info from file (used in restart)
   !
   USE kinds,               ONLY : dbl
   USE parameters,          ONLY : nstrx
   USE constants,           ONLY : CZERO, CONE
   USE io_module,           ONLY : stdout, io_name, wan_unit, ionode
   USE timing_module,       ONLY : timing
   USE log_module,          ONLY : log_push, log_pop
   USE files_module,        ONLY : file_open, file_close
   USE util_module,         ONLY : zmat_unitary, mat_mul, mat_svd
   USE kpoints_module,      ONLY : nkpts, nkpts_g, iks
   USE localization_module, ONLY : localization_read, cU
   USE overlap_module,      ONLY : ca, dimwann
   USE control_module,      ONLY : unitary_thr
   USE mp,                  ONLY : mp_sum
   !
   IMPLICIT NONE

   !
   ! input variables
   !
   CHARACTER(*),    INTENT(IN)  :: mode      

   !
   ! local variables
   !
   CHARACTER(17)                :: subname="localization_init"
   CHARACTER(nstrx)             :: filename
   REAL(dbl),       ALLOCATABLE :: singvd(:)
   COMPLEX(dbl),    ALLOCATABLE :: cv1(:,:), cv2(:,:)
   LOGICAL                      :: lfound
   INTEGER                      :: i, ik, ik_g, ierr
   !
   ! end of declarations
   !

!
!------------------------------
! main body
!------------------------------
!
   CALL timing('localization_init', OPR='start')
   CALL log_push('localization_init')


   !
   ! few checks
   !
   IF ( dimwann <=0 ) CALL errore(subname,'invalid dimwann',-dimwann+1)

   !
   ! here set CU
   !
   SELECT CASE ( TRIM( mode ) )
   CASE DEFAULT
      CALL errore(subname,'Invalid MODE = "'//TRIM(mode)//'"',1)

   CASE ( 'from_file' )

        IF (ionode) WRITE( stdout,"(/,'  Initial unitary rotations : from_file')")
        !
        CALL io_name('wannier',filename)
        CALL file_open(wan_unit,TRIM(filename),PATH="/",ACTION="read", IERR=ierr)
        IF ( ierr/=0 ) CALL errore(subname,'opening '//TRIM(filename),ABS(ierr))
        !
        CALL localization_read(wan_unit,"WANNIER_LOCALIZATION", lfound)
        IF ( .NOT. lfound ) CALL errore(subname,'searching tag "WANNIER_LOCALIZATION"',1)
        !
        CALL file_close(wan_unit,PATH="/",ACTION="read", IERR=ierr)
        IF ( ierr/=0 ) CALL errore(subname,'closing '//TRIM(filename),ABS(ierr))
        !
        CALL io_name('wannier',filename,LPATH=.FALSE.)
        IF (ionode) WRITE( stdout,"(2x,'Unitary matrices read from file: ',a,/)") TRIM(filename)
        
   CASE ( 'center_projections' )
        !
        IF (ionode) WRITE( stdout,"(/,'  Initial unitary rotations : projected localized orbitals',/)")
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
        !
        ALLOCATE( cv1(dimwann,dimwann), STAT=ierr )
        IF( ierr /=0 ) CALL errore('wannier', 'allocating cv1 ', ABS(ierr) )
        !
        ALLOCATE( cv2(dimwann,dimwann), STAT=ierr )
        IF( ierr /=0 ) CALL errore('wannier', 'allocating cv2 ', ABS(ierr) )

        !
        ! nullify the whoel vector
        !
        cu( :,:, 1:nkpts_g ) = CZERO
        !
        DO ik = 1, nkpts
            !
            ik_g = ik + iks -1
            !
            CALL mat_svd( dimwann, dimwann, ca(:,:,ik), singvd, cv1, cv2 )
            CALL mat_mul( cU(:,:,ik_g), cv1, 'N', cv2, 'N', dimwann, dimwann, dimwann )
            !
        ENDDO
        !
        ! recover over pool
        !
        CALL mp_sum( cU )

        !
        DEALLOCATE( singvd, cv1, cv2, STAT=ierr )
        IF( ierr /=0 ) CALL errore(subname,'deallocating SVD aux', ABS(ierr))

   CASE( 'no_guess' )
        !
        IF (ionode) WRITE( stdout,"(/,'  Initial unitary rotations : identities',/)")
        !
        ! The Cu(k) matrices are set equal to the identity, therefore the
        ! starting wfc from dsentangle are used as they are
        !
        cU( :,:, 1:nkpts_g ) = CZERO
        !
        DO ik = 1, nkpts
            !
            ik_g = ik + iks -1
            !
            cU(:,:,ik_g) = CZERO
            !
            DO i = 1, dimwann
                cU(i,i,ik_g) = CONE
            ENDDO
        ENDDO
        !
        CALL mp_sum( cU )
        !
   CASE( 'randomized' )
        !
        IF (ionode) WRITE( stdout,"(/,'  Initial unitary rotations : random',/)")
        !
        ! The Cu(k) matrices unitary random matrixes
        !
        cU( :,:, 1:nkpts_g ) = CZERO
        !
        DO ik = 1, nkpts
            !
            ik_g = ik + iks -1
            !
            cU(:,:,ik_g) = CZERO
            CALL random_orthovect(dimwann,dimwann,dimwann,cu(1,1,ik_g))
            !
        ENDDO
        !
        CALL mp_sum( cU )
        !
   END SELECT


   !
   ! check unitariery of cU
   !
   DO ik = 1, nkpts
       !
       ik_g = ik + iks -1
       !
       IF ( .NOT. zmat_unitary( dimwann, dimwann, cU(:,:,ik_g), &
                  SIDE='both', TOLL=unitary_thr )  ) &
                  CALL errore(subname,'U matrix not unitary', ik_g)
       !
   ENDDO


   CALL timing('localization_init', OPR='stop')
   CALL log_pop('localization_init')
   !
END SUBROUTINE localization_init

