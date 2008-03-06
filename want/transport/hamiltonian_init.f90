!
!      Copyright (C) 2005 WanT Group
!
!      This file is distributed under the terms of the
!      GNU General Public License. See the file `License'
!      in the root directory of the present distribution,
!      or http://www.gnu.org/copyleft/gpl.txt .
!
!*******************************************************************
   SUBROUTINE hamiltonian_init( calculation_type )
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
   USE io_module,            ONLY : stdin, ionode, ionode_id
   USE log_module,           ONLY : log_push, log_pop
   USE mp,                   ONLY : mp_bcast
   USE T_control_module,     ONLY : datafile_L, datafile_C, datafile_R, &
                                    use_overlap
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
   CHARACTER(*), INTENT(in)  :: calculation_type
   !
   ! local variables
   !
   CHARACTER(16) :: subname="hamiltonian_init"
   INTEGER       :: i, ierr
   LOGICAL       :: lhave_overlap
   !
   COMPLEX(dbl), ALLOCATABLE :: haux(:,:,:), saux(:,:,:)

   !
   ! end of declarations
   !

!
!----------------------------------------
! main Body
!----------------------------------------
!
   CALL log_push( 'hamiltonian_init' )

   !
   ! allocations
   !
   CALL hamiltonian_allocate()

   !
   ! dimx = MAX( dimL, dimC, dimR) 
   !
   ALLOCATE( haux(dimx,dimx,nrtot_par), STAT=ierr)
   IF ( ierr/=0 ) CALL errore(subname,'allocating haux',ABS(ierr))
   !
   ALLOCATE( saux(dimx,dimx,nrtot_par), STAT=ierr)
   IF ( ierr/=0 ) CALL errore(subname,'allocating saux',ABS(ierr))

   !
   ! set defaults for overlaps 
   !
   use_overlap = .FALSE.


   !
   ! open the IOTK tag
   !
   IF ( ionode ) THEN
      !
      CALL iotk_scan_begin( stdin, 'HAMILTONIAN_DATA', IERR=ierr )
      IF (ierr/=0) CALL errore(subname,'searching HAMILTONIAN_DATA',ABS(ierr))
      !
   ENDIF

   !
   ! read basic quantities
   !
   IF ( ionode ) THEN 
       CALL read_matrix( datafile_C, 'H00_C', dimC, dimC, haux, dimx, dimx, &
                         lhave_overlap, saux, dimx, dimx)
   ENDIF
   !
   CALL mp_bcast( haux, ionode_id )
   CALL mp_bcast( saux, ionode_id )
   CALL mp_bcast( lhave_overlap, ionode_id )
   !
   CALL fourier_par( h00_C, dimC, dimC, haux, dimx, dimx)   
   CALL fourier_par( s00_C, dimC, dimC, saux, dimx, dimx)   
   !
   use_overlap = use_overlap .OR. lhave_overlap
   !
   !
   !
   IF ( ionode ) THEN
       CALL read_matrix( datafile_C, 'H_CR', dimC, dimR, haux, dimx, dimx, &
                         lhave_overlap, saux, dimx, dimx)
   ENDIF
   !
   CALL mp_bcast( haux, ionode_id )
   CALL mp_bcast( saux, ionode_id )
   CALL mp_bcast( lhave_overlap, ionode_id )
   !
   CALL fourier_par( h_CR, dimC, dimR, haux, dimx, dimx)   
   CALL fourier_par( s_CR, dimC, dimR, saux, dimx, dimx)   
   !
   use_overlap = use_overlap .OR. lhave_overlap


   !
   ! chose whether to do 'conductor' or 'bulk'
   !
   SELECT CASE ( TRIM(calculation_type) )

   CASE ( "conductor" )
       !
       ! read the missing data
       !
       IF ( ionode ) THEN
           CALL read_matrix( datafile_C, 'H_LC', dimL, dimC, haux, dimx, dimx, &
                             lhave_overlap, saux, dimx, dimx )
       ENDIF
       !
       CALL mp_bcast( haux, ionode_id )
       CALL mp_bcast( saux, ionode_id )
       CALL mp_bcast( lhave_overlap, ionode_id )
       !
       CALL fourier_par( h_LC, dimL, dimC, haux, dimx, dimx)   
       CALL fourier_par( s_LC, dimL, dimC, saux, dimx, dimx)   
       !
       use_overlap = use_overlap .OR. lhave_overlap
       !
       !
       !
       IF ( ionode ) THEN 
           CALL read_matrix( datafile_L, 'H00_L', dimL, dimL, haux, dimx, dimx, &
                             lhave_overlap, saux, dimx, dimx )
       ENDIF
       !
       CALL mp_bcast( haux, ionode_id )
       CALL mp_bcast( saux, ionode_id )
       CALL mp_bcast( lhave_overlap, ionode_id )
       !
       CALL fourier_par( h00_L, dimL, dimL, haux, dimx, dimx)   
       CALL fourier_par( s00_L, dimL, dimL, saux, dimx, dimx)   
       !
       use_overlap = use_overlap .OR. lhave_overlap
       !
       !
       !
       IF ( ionode ) THEN 
           CALL read_matrix( datafile_L, 'H01_L', dimL, dimL, haux, dimx, dimx, &
                             lhave_overlap, saux, dimx, dimx )
       ENDIF
       !
       CALL mp_bcast( haux, ionode_id )
       CALL mp_bcast( saux, ionode_id )
       CALL mp_bcast( lhave_overlap, ionode_id )
       !
       CALL fourier_par( h01_L, dimL, dimL, haux, dimx, dimx)   
       CALL fourier_par( s01_L, dimL, dimL, saux, dimx, dimx)   
       !
       use_overlap = use_overlap .OR. lhave_overlap
       !
       !
       !
       IF ( ionode ) THEN
           CALL read_matrix( datafile_R, 'H00_R', dimR, dimR, haux, dimx, dimx, &
                             lhave_overlap, saux, dimx, dimx )
       ENDIF
       !
       CALL mp_bcast( haux, ionode_id )
       CALL mp_bcast( saux, ionode_id )
       CALL mp_bcast( lhave_overlap, ionode_id )
       !
       CALL fourier_par( h00_R, dimR, dimR, haux, dimx, dimx)   
       CALL fourier_par( s00_R, dimR, dimR, saux, dimx, dimx)   
       !
       use_overlap = use_overlap .OR. lhave_overlap
       !
       !
       !
       IF ( ionode ) THEN 
           CALL read_matrix( datafile_R, 'H01_R', dimR, dimR, haux, dimx, dimx, &
                             lhave_overlap, saux, dimx, dimx )
       ENDIF
       !
       CALL mp_bcast( haux, ionode_id )
       CALL mp_bcast( saux, ionode_id )
       CALL mp_bcast( lhave_overlap, ionode_id ) 
       !
       CALL fourier_par( h01_R, dimR, dimR, haux, dimx, dimx)   
       CALL fourier_par( s01_R, dimR, dimR, saux, dimx, dimx)   
       !
       use_overlap = use_overlap .OR. lhave_overlap
       !
       !
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
       s00_L(:,:,:) = s00_C(:,:,:)
       s00_R(:,:,:) = s00_C(:,:,:)
       s01_L(:,:,:) = s_CR(:,:,:)
       s01_R(:,:,:) = s_CR(:,:,:)
       s_LC(:,:,:)  = s_CR(:,:,:)
       !
   CASE DEFAULT
       CALL errore(subname,'Invalid calculation_type = '// TRIM(calculation_type),5)
   END SELECT


   IF ( ionode ) THEN
       !
       CALL iotk_scan_end( stdin, 'HAMILTONIAN_DATA', IERR=ierr )
       IF (ierr/=0) CALL errore(subname,'searching end for HAMILTONIAN_DATA',ABS(ierr))
       !
   ENDIF


   !
   ! local cleaning
   !
   DEALLOCATE( haux, saux, STAT=ierr)
   IF ( ierr/=0 ) CALL errore(subname,'deallocating haux, saux',ABS(ierr))
   !
   CALL log_pop( 'hamiltonian_init' )

END SUBROUTINE hamiltonian_init

