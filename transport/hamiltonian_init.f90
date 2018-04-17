!
!      Copyright (C) 2005 WanT Group
!
!      This file is distributed under the terms of the
!      GNU General Public License. See the file `License'
!      in the root directory of the present distribution,
!      or http://www.gnu.org/copyleft/gpl.txt .
!
!*******************************************************************
   SUBROUTINE hamiltonian_init( use_overlap, calculation_type )
   !*******************************************************************
   !
   ! Initialize hamiltonian data:
   !
   !...  Matrix definition
   !
   !     Given a conductor (C) bonded to a right lead (A) and a left lead (B)
   !
   !       H01_L    H00_L   H_LC    H00_C     H_CR   H00_R   H01_R
   !       S01_L    S00_L   S_LC    S00_C     S_CR   S00_R   S01_R
   !   ...--------------------------------------------------------------...
   !         |                |                   |                | 
   !         |     LEAD L     |    CONDUCTOR C    |     LEAD R     |
   !         |                |                   |                | 
   !   ...--------------------------------------------------------------...
   !
   !     H00_L, H00_R    = on site hamiltonian of the leads (from bulk calculation)
   !     H01_L, H01_R    = hopping hamiltonian of the leads (from bulk calculation)
   !     H00_C           = on site hamiltonian of the conductor (from supercell calculation)
   !     H_LC, H_CR  = coupling matrix between leads and conductor 
   !                       (from supercell calculation)
   !
   !     S00_L, S00_R, S00_C  = on site overlap matrices
   !     S01_L, S01_R         = hopping overlap matrices
   !     S_LC, S_CR           = coupling overlap matrices
   !
   !...  Overlap
   !     if  use_overlap = .FALSE. (default) the basis set is orthonormal =>
   !         S00_L, S00_R, S00_C = Identity
   !         S01_L, S01_R, S_LC, S_CR = Zero
   !     if  use_overlap=  .TRUE. => external reading  
   !                (not implemented within the current interface)
   !
   !...  Units
   !     energies are supposed to be in eV
   !
   USE kinds
   USE constants,            ONLY : CZERO, CONE
   USE io_global_module,     ONLY : stdin
   USE T_control_module,     ONLY : datafile_L, datafile_C, datafile_R
   USE T_kpoints_module,     ONLY : kpoints_init, nrtot_par
   USE T_hamiltonian_module, ONLY : hamiltonian_allocate,   &
                                    dimL, dimR, dimC, dimx, &
                                    h00_L, h01_L, h00_R, h01_R, h00_C, &
                                    s00_L, s01_L, s00_R, s01_R, s00_C, &
                                    h_LC, h_CR, s_LC, s_CR
   USE iotk_module
   IMPLICIT NONE

   ! 
   ! input variables
   !
   LOGICAL,      INTENT(in)  :: use_overlap
   CHARACTER(*), INTENT(in)  :: calculation_type
   !
   ! local variables
   !
   CHARACTER(16) :: subname="hamiltonian_init"
   COMPLEX(dbl), ALLOCATABLE :: aux(:,:,:)
   INTEGER       :: i, ierr

   !
   ! end of declarations
   !

!
!----------------------------------------
! main Body
!----------------------------------------
!

   !
   ! missing implementation
   !
   IF ( use_overlap ) CALL errore(subname,'overlaps not currently implemented',1)

   !
   ! allocations
   !
   CALL hamiltonian_allocate()
   !
   ! dimx = MAX( dimL, dimC, dimR) 
   ALLOCATE( aux(dimx,dimx,nrtot_par), STAT=ierr)
      IF ( ierr/=0 ) CALL errore(subname,'allocating aux',ABS(ierr))

   !
   ! set defaults for overlaps ( already in the fourier transformed, R_par to k_par)
   ! NOTE: at the moment overlaps are not impemented 
   !
   s00_L(:,:,:)  = CZERO
   s01_L(:,:,:)  = CZERO
   s00_R(:,:,:)  = CZERO
   s01_R(:,:,:)  = CZERO
   s00_C(:,:,:)  = CZERO
   s_LC(:,:,:) = CZERO
   s_CR(:,:,:) = CZERO

   DO i = 1, dimL
       s00_L(i,i,:) = CONE
   ENDDO
   DO i = 1, dimR
       s00_R(i,i,:) = CONE
   ENDDO
   DO i = 1, dimC
       s00_C(i,i,:) = CONE
   ENDDO


   !
   ! open the IOTK tag
   !
   CALL iotk_scan_begin( stdin, 'HAMILTONIAN_DATA', IERR=ierr )
      IF (ierr/=0) CALL errore(subname,'searching HAMILTONIAN_DATA',ABS(ierr))


   !
   ! read basic quantities
   !
   CALL read_matrix( datafile_C, 'H00_C', dimC, dimC, aux, dimx, dimx)
   CALL fourier_par(h00_C, dimC, dimC, aux, dimx, dimx)   
   !
   CALL read_matrix( datafile_C, 'H_CR', dimC, dimR, aux, dimx, dimx)
   CALL fourier_par(h_CR, dimC, dimR, aux, dimx, dimx)   

   IF ( use_overlap ) THEN
       !
       CALL read_matrix( datafile_C, 'S00_C', dimC, dimC, aux, dimx, dimx)
       CALL fourier_par (s00_C, dimC, dimC, aux, dimx, dimx)   
       !
       CALL read_matrix( datafile_C, 'S_CR', dimC, dimR, aux, dimx, dimx)
       CALL fourier_par (s_CR, dimC, dimR, aux, dimx, dimx)   
   ENDIF

   !
   ! chose whether to do 'conductor' or 'bulk'
   !
   SELECT CASE ( TRIM(calculation_type) )

   CASE ( "conductor" )
       !
       ! read the missing data
       !
       CALL read_matrix( datafile_C, 'H_LC', dimL, dimC, aux, dimx, dimx)
       CALL fourier_par (h_LC, dimL, dimC, aux, dimx, dimx)   
       !
       CALL read_matrix( datafile_L, 'H00_L', dimL, dimL, aux, dimx, dimx)
       CALL fourier_par (h00_L, dimL, dimL, aux, dimx, dimx)   
       !
       CALL read_matrix( datafile_L, 'H01_L', dimL, dimL, aux, dimx, dimx)
       CALL fourier_par (h01_L, dimL, dimL, aux, dimx, dimx)   
       !
       CALL read_matrix( datafile_R, 'H00_R', dimR, dimR, aux, dimx, dimx)
       CALL fourier_par (h00_R, dimR, dimR, aux, dimx, dimx)   
       !
       CALL read_matrix( datafile_R, 'H01_R', dimR, dimR, aux, dimx, dimx)
       CALL fourier_par (h01_R, dimR, dimR, aux, dimx, dimx)   
       !
       IF ( use_overlap ) THEN
           !
           CALL read_matrix( datafile_C, 'S_LC', dimL, dimC, aux, dimx, dimx)
           CALL fourier_par (s_LC, dimL, dimC, aux, dimx, dimx)   
           !
           CALL read_matrix( datafile_L, 'S00_L',  dimL, dimL, aux, dimx, dimx)
           CALL fourier_par (s00_L, dimL, dimL, aux, dimx, dimx)   
           !
           CALL read_matrix( datafile_L, 'S01_L',  dimL, dimL, aux, dimx, dimx)
           CALL fourier_par (s01_L, dimL, dimL, aux, dimx, dimx)   
           !
           CALL read_matrix( datafile_R, 'S00_R',  dimR, dimR, aux, dimx, dimx)
           CALL fourier_par (s00_R, dimR, dimR, aux, dimx, dimx)   
           !
           CALL read_matrix( datafile_R, 'S01_R',  dimR, dimR, aux, dimx, dimx)
           CALL fourier_par (s01_R, dimR, dimR, aux, dimx, dimx)   
       ENDIF

   CASE ( "bulk" )
       !
       ! rearrange the data already read
       !
       h00_L(:,:,:) = h00_C(:,:,:)
       h00_R(:,:,:) = h00_C(:,:,:)
       h01_L(:,:,:) = h_CR(:,:,:)
       h01_R(:,:,:) = h_CR(:,:,:)
       h_LC(:,:,:)  = h_CR(:,:,:)
       !
       IF ( use_overlap ) THEN
           s00_L(:,:,:) = s00_C(:,:,:)
           s00_R(:,:,:) = s00_C(:,:,:)
           s01_L(:,:,:) = s_CR(:,:,:)
           s01_R(:,:,:) = s_CR(:,:,:)
           s_LC(:,:,:)= s_CR(:,:,:)
       ENDIF

   CASE DEFAULT
       CALL errore(subname,'Invalid calculation_type = '// &
                   TRIM(calculation_type),5)
   END SELECT

   CALL iotk_scan_end( stdin, 'HAMILTONIAN_DATA', IERR=ierr )
      IF (ierr/=0) CALL errore(subname,'searching end for HAMILTONIAN_DATA',ABS(ierr))


   !
   ! local cleaning
   !
   DEALLOCATE( aux, STAT=ierr)
      IF ( ierr/=0 ) CALL errore(subname,'deallocating aux',ABS(ierr))

END SUBROUTINE hamiltonian_init


