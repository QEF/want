!
! Copyright (C) 2009 WanT Group
!
! This file is distributed under the terms of the
! GNU General Public License. See the file `License\'
! in the root directory of the present distribution,
! or http://www.gnu.org/copyleft/gpl.txt .
! 
! Important contributions to this module have been given
! by Tonatiuh Rangel Gordillo
!
!*********************************************
   MODULE wannier90_tools_module
!*********************************************
   !
   USE kinds,              ONLY : dbl
   USE constants,          ONLY : BOHR => bohr_radius_angs, ZERO, ONE, TWO, RYD
   USE parameters,         ONLY : nstrx
   USE timing_module,      ONLY : timing
   USE log_module,         ONLY : log_push, log_pop
   USE converters_module,  ONLY : cart2cry, cry2cart
   USE parser_module,      ONLY : change_case
   USE iotk_module
   !
   IMPLICIT NONE 
   PRIVATE
   SAVE

   !
   ! global variables of the module
   !
   CHARACTER(nstrx)   :: prefix
   CHARACTER(nstrx)   :: file_chk
   CHARACTER(nstrx)   :: file_eig
   !
   LOGICAL            :: init = .FALSE.

   !
   ! contains:
   ! SUBROUTINE  wannier90_tools_init( prefix_ )
   ! SUBROUTINE  wannier90_tools_get_prefix( filein, prefix_ )
   ! SUBROUTINE  wannier90_to_internal( filein, fileout, filetype )
   ! FUNCTION    file_is_wannier90( filein )
   !
   PUBLIC :: wannier90_to_internal
   PUBLIC :: file_is_wannier90

CONTAINS


!**********************************************************
   SUBROUTINE wannier90_tools_init( prefix_ )
   !**********************************************************
   !
   ! define module global variables
   !
   IMPLICIT NONE
   CHARACTER(*),   INTENT(IN) :: prefix_ 
   !
   prefix   = TRIM( prefix_ )
   file_chk = TRIM(prefix)//'.chk'
   file_eig = TRIM(prefix)//'.eig'
   !
   init     = .TRUE.
   !
END SUBROUTINE wannier90_tools_init
   

!**********************************************************
   SUBROUTINE wannier90_tools_get_prefix( filein, prefix_ )
   !**********************************************************
   !
   ! extract the prefix (basename) of the input file.
   ! If the extension of the file is not ".chk" an
   ! empty prefix is issued
   !
   IMPLICIT NONE
   CHARACTER(*),   INTENT(IN)  :: filein
   CHARACTER(*),   INTENT(OUT) :: prefix_ 
   !
   INTEGER      :: ilen
   CHARACTER(4) :: suffix='.chk'
   !
   prefix_  = ' '
   !
   ilen = LEN_TRIM( filein )
   !
   IF ( filein(ilen-3:ilen) == suffix ) THEN
       !
       prefix_ = filein(1:ilen-4)
       !
   ENDIF
   !
END SUBROUTINE wannier90_tools_get_prefix
   

!**********************************************************
   SUBROUTINE wannier90_tools_get_dims( nbnd, nkpts, dimwann )
   !**********************************************************
   !
   ! get the dimensions of the problem
   ! need to have the module initialized
   !
   IMPLICIT NONE
   ! 
   INTEGER,   INTENT(OUT) :: nbnd, nkpts, dimwann
   !
   CHARACTER(24) :: subname='wannier90_tools_get_dims'
   LOGICAL       :: lread
   INTEGER       :: iunit
   INTEGER       :: i_old, i_new, j, ierr
   
   IF ( .NOT. init ) CALL errore(subname,'module not init',10)

   CALL iotk_free_unit( iunit )

   !
   ! get dimensions from .chk
   !
   OPEN( iunit, FILE=file_chk, STATUS='old', FORM='unformatted', IOSTAT=ierr )
   IF ( ierr/=0 ) CALL errore(subname,'opening '//TRIM(file_chk),ABS(ierr) )
   !
   READ(iunit, IOSTAT=ierr) 
   IF ( ierr/=0 ) CALL errore(subname,'skipping header',ABS(ierr) )
   !
   READ(iunit, IOSTAT=ierr) 
   IF ( ierr/=0 ) CALL errore(subname,'skipping lattice',ABS(ierr) )
   READ(iunit, IOSTAT=ierr) 
   IF ( ierr/=0 ) CALL errore(subname,'skipping recipr lattice',ABS(ierr) )
   !
   READ(iunit, IOSTAT=ierr) nkpts
   IF ( ierr/=0 ) CALL errore(subname,'reading nkpts',ABS(ierr) )
   !
   READ(iunit, IOSTAT=ierr) 
   IF ( ierr/=0 ) CALL errore(subname,'skipping vkpt',ABS(ierr) )
   !
   READ(iunit, IOSTAT=ierr) 
   IF ( ierr/=0 ) CALL errore(subname,'skipping nntot',ABS(ierr) )
   READ(iunit, IOSTAT=ierr) dimwann
   IF ( ierr/=0 ) CALL errore(subname,'reading dimwann',ABS(ierr) )
   !
   CLOSE( iunit, IOSTAT=ierr )
   IF ( ierr/=0 ) CALL errore(subname,'closing '//TRIM(file_chk),ABS(ierr) )
  
   !
   ! get dimensions from .eig
   !
   OPEN( iunit, FILE=file_eig, STATUS='old', FORM='formatted', IOSTAT=ierr )
   IF ( ierr/=0 ) CALL errore(subname,'opening '//TRIM(file_eig),ABS(ierr) )
   !
   lread = .TRUE.
   i_old = 0
   i_new = 0
   DO WHILE( lread )
       !
       i_old = i_new
       READ(iunit, *, IOSTAT=ierr) i_new, j
       IF ( ierr/=0 ) CALL errore(subname,'reading indexes', ABS(ierr) )
       !
       IF ( j == 2 ) lread = .FALSE.
       !
   ENDDO
   !
   nbnd = i_old
   !
   !
   CLOSE( iunit, IOSTAT=ierr )
   IF ( ierr/=0 ) CALL errore(subname,'closing '//TRIM(file_eig),ABS(ierr) )
   !
   RETURN
   !
END SUBROUTINE wannier90_tools_get_dims


!**********************************************************
   SUBROUTINE wannier90_to_internal( filein, fileout, filetype )
   !**********************************************************
   !
   ! Convert the datafile written by the wannier90 program to
   ! the internal representation.
   !
   ! FILETYPE values are:
   !  - ham, hamiltonian
   !  - space, subspace
   !
   IMPLICIT NONE

   LOGICAL, PARAMETER :: binary = .FALSE.
   
   !
   ! input variables
   !
   CHARACTER(*), INTENT(IN) :: filein
   CHARACTER(*), INTENT(IN) :: fileout
   CHARACTER(*), INTENT(IN) :: filetype
   
   !
   ! local variables
   !
   INTEGER                     :: iunit, ounit
   !
   CHARACTER(21)               :: subname="wannier90_to_internal"
   !
   CHARACTER(nstrx)            :: attr, r_units, a_units, b_units, k_units, h_units, e_units
   CHARACTER(nstrx)            :: filetype_
   INTEGER                     :: dimwann, nkpts, nk(3), shift(3), nrtot, nr(3), nspin, nbnd 
   INTEGER                     :: auxdim1, auxdim2, auxdim3
   INTEGER                     :: ierr, ir, ik, isp
   !
   LOGICAL                     :: write_ham, write_space
   !
   REAL(dbl)                   :: dlatt(3,3), rlatt(3,3), norm, efermi
   INTEGER,        ALLOCATABLE :: ivr(:,:)
   INTEGER,        ALLOCATABLE :: itmp(:)
   REAL(dbl),      ALLOCATABLE :: vkpt_cry(:,:), vkpt(:,:), wk(:), wr(:), vr(:,:)
   REAL(dbl),      ALLOCATABLE :: eig(:,:,:)
   REAL(dbl),      ALLOCATABLE :: rham(:,:,:,:), rovp(:,:,:)
   COMPLEX(dbl),   ALLOCATABLE :: kham(:,:,:,:), kovp(:,:,:)
   COMPLEX(dbl),   ALLOCATABLE :: caux(:,:,:)

!
!------------------------------
! main body
!------------------------------
!
   CALL timing( subname, OPR='start' )
   CALL log_push( subname )


   !
   ! search for units indipendently of io_module
   !
   CALL iotk_free_unit( iunit )
   CALL iotk_free_unit( ounit )

   !
   ! select the operation to do
   !
   write_ham     = .FALSE.
   write_space   = .FALSE.
   !
   filetype_ = TRIM( filetype )
   CALL change_case( filetype_, 'lower' )
   !
   SELECT CASE( TRIM( filetype_ ) )
   !
   CASE( 'ham', 'hamiltonian' )
      write_ham   = .TRUE.
   CASE( 'space', 'subspace' )
      write_space = .TRUE.
   CASE DEFAULT
      CALL errore(subname, 'invalid filetype: '//TRIM(filetype_), 71 )
   END SELECT


!
!---------------------------------
! read from filein (WANNIER90 fmt)
!---------------------------------
!

   CALL wannier90_tools_get_dims( nbnd, nkpts, dimwann )
   !
   WRITE(0,*) "Reading dims"
   WRITE(0,*) "     nbnd : ", nbnd
   WRITE(0,*) "    nkpts : ", nkpts
   WRITE(0,*) "  dimwann : ", dimwann

   !
   ! allocate main quantities to be readed
   !
   ALLOCATE( vkpt_cry(3, nkpts), STAT=ierr )
   IF ( ierr/=0 ) CALL errore(subname, 'allocating vkpt_cry', ABS(ierr) )
   ALLOCATE( vkpt(3, nkpts), STAT=ierr )
   IF ( ierr/=0 ) CALL errore(subname, 'allocating vkpt', ABS(ierr) )
   !
   ALLOCATE( wk(nkpts), STAT=ierr )
   IF ( ierr/=0 ) CALL errore(subname, 'allocating wk', ABS(ierr) )


   !
   ! read main dataset
   !
!   CALL wannier90_tools_get_data( nbnd, nkpts, dimwann, &
!         rlatt, dlatt, vkpt, wk, lwindow, u_matrix_opt, u_matrix, ndimwin, )

   !
   ! real-space lattice vectors
   !
   !
   ALLOCATE( ivr(3, nrtot), vr(3, nrtot), STAT=ierr )
   IF ( ierr/=0 ) CALL errore(subname, 'allocating ivr, vr', ABS(ierr) )
   !
   ALLOCATE( wr(nrtot), STAT=ierr )
   IF ( ierr/=0 ) CALL errore(subname, 'allocating wr', ABS(ierr) )
   
   !
   ! define IVR
   !
   DO ir = 1, nrtot
      !
      ivr(:, ir ) = NINT( vr(:, ir) )
      !
   ENDDO

   !
   ! as a default, set wr to 1.0; to be checked
   !
   wr (1:nrtot) = ONE


   !
   ! check the normalization of the weights
   !
   norm   = SUM( wk )
   wk(:)  = wk(:) / norm


   ALLOCATE( eig(dimwann, nkpts, nspin), STAT=ierr )
   IF ( ierr/=0 ) CALL errore(subname,'allocating eig', ABS(ierr))
   !
   eig(:,:,:) = ZERO 


   IF ( write_ham ) THEN
       !
       ALLOCATE( rham(dimwann, dimwann, nrtot, nspin), STAT=ierr )
       IF ( ierr/=0 ) CALL errore(subname, 'allocating rham', ABS(ierr) )
       ALLOCATE( kham(dimwann, dimwann, nkpts, nspin), STAT=ierr )
       IF ( ierr/=0 ) CALL errore(subname, 'allocating kham', ABS(ierr) )

       !
       ! compute kham to complete the dataset present in the .ham file
       !
       ALLOCATE( caux( dimwann, dimwann, nrtot), STAT=ierr)
       IF ( ierr/=0 ) CALL errore(subname, 'allocating caux', ABS(ierr))
       !
       DO isp = 1, nspin
           !
           caux = CMPLX( rham(:,:,:,isp), ZERO, KIND=dbl )
           CALL compute_kham( dimwann, nrtot, vr, wr, caux, vkpt, &
                              kham(:,:,:,isp) )
           !
       ENDDO
       !
       DEALLOCATE( caux, STAT=ierr)
       IF ( ierr/=0 ) CALL errore(subname, 'deallocating caux', ABS(ierr))
       !
   ENDIF


!
!---------------------------------
! write to fileout (internal fmt)
!---------------------------------
!
   IF ( write_ham ) THEN
       !
       CALL iotk_open_write( ounit, FILE=TRIM(fileout), BINARY=binary )
       CALL iotk_write_begin( ounit, "HAMILTONIAN" )
       !
       !
       CALL iotk_write_attr( attr,"dimwann",dimwann,FIRST=.TRUE.)
       CALL iotk_write_attr( attr,"nkpts",nkpts)
       CALL iotk_write_attr( attr,"nspin",nspin)
       CALL iotk_write_attr( attr,"nk",nk)
       CALL iotk_write_attr( attr,"shift",shift)
       CALL iotk_write_attr( attr,"nrtot",nrtot)
       CALL iotk_write_attr( attr,"nr",nr)
       CALL iotk_write_attr( attr,"have_overlap", .TRUE. )
       CALL iotk_write_attr( attr,"fermi_energy", 0.0 )
       CALL iotk_write_empty( ounit,"DATA",ATTR=attr)
       !
       CALL iotk_write_attr( attr,"units","bohr",FIRST=.TRUE.)
       CALL iotk_write_dat( ounit,"DIRECT_LATTICE", dlatt, ATTR=attr, COLUMNS=3)
       !
       CALL iotk_write_attr( attr,"units","bohr^-1",FIRST=.TRUE.)
       CALL iotk_write_dat( ounit,"RECIPROCAL_LATTICE", rlatt, ATTR=attr, COLUMNS=3)
       !
       CALL iotk_write_attr( attr,"units","crystal",FIRST=.TRUE.)
       CALL iotk_write_dat( ounit,"VKPT", vkpt_cry, ATTR=attr, COLUMNS=3)
       CALL iotk_write_dat( ounit,"WK", wk)
       !
       CALL iotk_write_dat( ounit,"IVR", ivr, ATTR=attr, COLUMNS=3)
       CALL iotk_write_dat( ounit,"WR", wr)
       !
       !
       spin_loop: & 
       DO isp = 1, nspin
          !
          IF ( nspin == 2 ) THEN
              !
              CALL iotk_write_begin( ounit, "SPIN"//TRIM(iotk_index(isp)) )
              !
          ENDIF
          !
          !
          ! Eigenvalues are not read, and are temporarily not written
          ! when converting from Crystal
          !
          CALL iotk_write_dat( ounit,"WAN_EIGENVALUES", eig(:,:,isp))
          !
          CALL iotk_write_begin( ounit,"KHAM")
          !
          DO ik = 1, nkpts
              !
              CALL iotk_write_dat( ounit,"KPT"//TRIM(iotk_index(ik)), &
                                   kham( :, :, ik, isp) )
              CALL iotk_write_dat( ounit,"OVERLAP"//TRIM(iotk_index(ik)), &
                                   kovp( :, :, ik) )
              !
          ENDDO
          ! 
          CALL iotk_write_end( ounit,"KHAM")
          !
          CALL iotk_write_begin( ounit,"RHAM")
          !
          DO ir = 1, nrtot
              !
              CALL iotk_write_dat( ounit,"VR"//TRIM(iotk_index(ir)), &
                                   CMPLX(rham( :, :, ir, isp), ZERO, dbl) )
              CALL iotk_write_dat( ounit,"OVERLAP"//TRIM(iotk_index(ir)), &
                                   CMPLX(rovp( :, :, ir), ZERO, dbl) )
              !
          ENDDO
          !
          CALL iotk_write_end( ounit,"RHAM")
          !
          IF ( nspin == 2 ) THEN
              !
              CALL iotk_write_end( ounit, "SPIN"//TRIM(iotk_index(isp)) )
              !
          ENDIF
          !
       ENDDO spin_loop
       !
       CALL iotk_write_end( ounit, "HAMILTONIAN" )
       CALL iotk_close_write( ounit )
       !
   ENDIF

   IF ( write_space ) THEN
       !
       CALL iotk_open_write( ounit, FILE=TRIM(fileout), BINARY=binary )
       !
       !
       CALL iotk_write_begin( ounit, "WINDOWS" )
       !
       !
       CALL iotk_write_attr( attr,"nbnd",dimwann,FIRST=.TRUE.)
       CALL iotk_write_attr( attr,"nkpts",nkpts)
       CALL iotk_write_attr( attr,"nspin",nspin)
       CALL iotk_write_attr( attr,"spin_component","none")
       CALL iotk_write_attr( attr,"efermi", 0.0 )
       CALL iotk_write_attr( attr,"dimwinx", dimwann )
       CALL iotk_write_empty( ounit,"DATA",ATTR=attr)
       !
       ALLOCATE( itmp(nkpts), STAT=ierr )
       IF ( ierr/=0 ) CALL errore(subname, 'allocating itmp', ABS(ierr))
       !
       itmp(:) = dimwann 
       CALL iotk_write_dat( ounit, "DIMWIN", itmp, COLUMNS=8 )
       itmp(:) = 1
       CALL iotk_write_dat( ounit, "IMIN", itmp, COLUMNS=8 )
       itmp(:) = dimwann
       CALL iotk_write_dat( ounit, "IMAX", itmp, COLUMNS=8 )
       CALL iotk_write_dat( ounit, "EIG", eig, COLUMNS=4)
       !
       CALL iotk_write_end( ounit, "WINDOWS" )
       !
       !
       CALL iotk_write_begin( ounit, "SUBSPACE" )
       !
       CALL iotk_write_attr( attr,"dimwinx",dimwann,FIRST=.TRUE.)
       CALL iotk_write_attr( attr,"nkpts",nkpts)
       CALL iotk_write_attr( attr,"dimwann", dimwann)
       CALL iotk_write_empty( ounit,"DATA",ATTR=attr)
       !
       itmp(:) = dimwann 
       CALL iotk_write_dat( ounit, "DIMWIN", itmp, COLUMNS=8 )
       CALL iotk_write_dat( ounit, "WAN_EIGENVALUES", eig, COLUMNS=4)
       !
       CALL iotk_write_end( ounit, "SUBSPACE" )
       !
       !
       CALL iotk_close_write( ounit )
       !
       DEALLOCATE( itmp, STAT=ierr )
       IF ( ierr/=0 ) CALL errore(subname, 'deallocating itmp', ABS(ierr))
       !
   ENDIF

!
! local cleaning
!

   DEALLOCATE( vkpt_cry, wk, STAT=ierr )
   IF ( ierr/=0 ) CALL errore(subname, 'deallocating vkpt_cry, wk', ABS(ierr) )
   DEALLOCATE( vkpt, STAT=ierr )
   IF ( ierr/=0 ) CALL errore(subname, 'deallocating vkpt', ABS(ierr) )
   !
   DEALLOCATE( ivr, wr, STAT=ierr )
   IF ( ierr/=0 ) CALL errore(subname, 'deallocating ivr, wr', ABS(ierr) )
   !
   DEALLOCATE( eig, STAT=ierr )
   IF ( ierr/=0 ) CALL errore(subname, 'deallocating eig', ABS(ierr) )
   !
   IF( ALLOCATED( rham ) ) THEN
       DEALLOCATE( rham, STAT=ierr )
       IF ( ierr/=0 ) CALL errore(subname, 'deallocating rham', ABS(ierr) )
   ENDIF
   IF( ALLOCATED( kham ) ) THEN
       DEALLOCATE( kham, STAT=ierr )
       IF ( ierr/=0 ) CALL errore(subname, 'deallocating kham', ABS(ierr) )
   ENDIF
   IF( ALLOCATED( rovp ) ) THEN
       DEALLOCATE( rovp, STAT=ierr )
       IF ( ierr/=0 ) CALL errore(subname, 'deallocating rovp', ABS(ierr) )
   ENDIF
   IF( ALLOCATED( kovp ) ) THEN
       DEALLOCATE( kovp, STAT=ierr )
       IF ( ierr/=0 ) CALL errore(subname, 'deallocating kovp', ABS(ierr) )
   ENDIF
   

   CALL log_pop( subname )
   CALL timing( subname, OPR='stop' )
   !
   RETURN
   !
END SUBROUTINE wannier90_to_internal


!**********************************************************
   LOGICAL FUNCTION file_is_wannier90( filename )
   !**********************************************************
   !
   IMPLICIT NONE
   !
   ! Check for wannier90 fmt
   ! To do this, we check that the main datafile
   ! is called $prefix.chk, and that a second file
   ! named $prefix.eig exists
   !
   CHARACTER(*), INTENT(IN) :: filename
   !
   CHARACTER(nstrx) :: prefix_ 
   INTEGER          :: iunit
   INTEGER          :: ierr
   LOGICAL          :: lerror, lexist
     !
     !
     CALL iotk_free_unit( iunit )
     !
     file_is_wannier90 = .FALSE.
     lerror = .FALSE.
     !
     INQUIRE( FILE=filename, EXIST=lexist ) 
     IF ( .NOT. lexist ) lerror = .TRUE.
     
     CALL wannier90_tools_get_prefix( filename, prefix_ )
     !
     IF ( LEN_TRIM( prefix_ ) /= 0 ) THEN
         CALL wannier90_tools_init( prefix_ )
     ELSE
         lerror = .TRUE.
         RETURN
     ENDIF

     !
     ! check the existence of the second needed datafile
     ! even if redundant, we re-check also the first file
     !
     INQUIRE( FILE=file_chk, EXIST=lexist )
     IF ( .NOT. lexist ) lerror = .TRUE.
     !
     INQUIRE( FILE=file_eig, EXIST=lexist )
     IF ( .NOT. lexist ) lerror = .TRUE.
     
     !
     ! further check on the content of the files 
     ! can be performed if the case
     !

     IF ( lerror ) THEN
         RETURN
     ENDIF
     !
     file_is_wannier90 = .TRUE.
     !
  END FUNCTION file_is_wannier90

END MODULE wannier90_tools_module

