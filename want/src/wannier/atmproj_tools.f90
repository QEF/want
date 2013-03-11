!
! Copyright (C) 2012 WanT Group
!
! This file is distributed under the terms of the
! GNU General Public License. See the file `License\'
! in the root directory of the present distribution,
! or http://www.gnu.org/copyleft/gpl.txt .
!
!*********************************************
   MODULE atmproj_tools_module
!*********************************************
   !
   USE kinds,              ONLY : dbl
   USE constants,          ONLY : BOHR => bohr_radius_angs, ZERO, ONE, TWO, &
                                  RYD, EPS_m8, TPI
   USE parameters,         ONLY : nstrx
   USE timing_module,      ONLY : timing
   USE log_module,         ONLY : log_push, log_pop
   USE converters_module,  ONLY : cart2cry, cry2cart
   USE parser_module,      ONLY : change_case
   USE util_module,        ONLY : mat_is_herm
   USE grids_module,       ONLY : grids_get_rgrid
   USE files_module,       ONLY : file_exist
   USE iotk_module
   USE qexml_module
   !
   IMPLICIT NONE 
   PRIVATE
   SAVE
   
   !
   ! global variables of the module
   !
   CHARACTER(nstrx)   :: savedir
   CHARACTER(nstrx)   :: file_proj
   CHARACTER(nstrx)   :: file_data
   !
   LOGICAL            :: init = .FALSE.
   !
   ! parameters for the reconstruction 
   ! of the Hamiltonian
   !
   REAL(dbl)          :: eshift = 20.0d0
   CHARACTER(256)     :: spin_component = "all"


   ! contains:
   ! SUBROUTINE  atmproj_to_internal( filein, fileout, filetype, do_orthoovp )
   ! FUNCTION    file_is_atmproj( filein )
   !
   PUBLIC :: atmproj_to_internal
   PUBLIC :: file_is_atmproj
   PUBLIC :: eshift
   PUBLIC :: spin_component

CONTAINS


!**********************************************************
   SUBROUTINE atmproj_tools_init( file_proj_, ierr )
   !**********************************************************
   !
   ! define module global variables
   !
   IMPLICIT NONE
   CHARACTER(*),   INTENT(IN)  :: file_proj_
   INTEGER,        INTENT(OUT) :: ierr
   !
   CHARACTER(18)  :: subname="atmproj_tools_init"
   INTEGER        :: ilen
   !
   file_proj = TRIM( file_proj_ )
   !
   IF ( .NOT. file_exist( file_proj ) ) &
        CALL errore(subname,"file proj not found: "//TRIM(file_proj), 10 )

   ierr = 1
    
   !
   ! define save_dir
   !
   savedir  = ' '
   !
   ilen = LEN_TRIM( file_proj_ )
   IF ( ilen <= 14 ) RETURN
   
   !
   IF ( file_proj_(ilen-14:ilen) == "atomic_proj.xml" .OR. &
        file_proj_(ilen-14:ilen) == "atomic_proj.dat" ) THEN
       !
       savedir = file_proj_(1:ilen-15)
       !
   ENDIF
   !
   IF ( LEN_TRIM(savedir) == 0 ) RETURN
   !
   file_data = TRIM(savedir) // "/data-file.xml"
   !
   IF ( .NOT. file_exist( file_data ) ) RETURN
   !

   !WRITE(0,*) "file_proj: ", TRIM(file_proj)
   !WRITE(0,*) "file_data: ", TRIM(file_data)
   !WRITE(0,*) "savedir:   ", TRIM(savedir)

   ierr     =  0
   init     = .TRUE.
   !
END SUBROUTINE atmproj_tools_init


!**********************************************************
   SUBROUTINE atmproj_to_internal( filein, fileout, filetype, do_orthoovp )
   !**********************************************************
   !
   ! Convert the datafile written by the projwfc program (QE suite) to
   ! the internal representation.
   !
   ! FILETYPE values are:
   !  - ham, hamiltonian
   !  - space, subspace
   !
   USE util_module
   !
   IMPLICIT NONE

   LOGICAL, PARAMETER :: binary = .TRUE.
   
   !
   ! input variables
   !
   CHARACTER(*), INTENT(IN) :: filein
   CHARACTER(*), INTENT(IN) :: fileout
   CHARACTER(*), INTENT(IN) :: filetype
   LOGICAL, OPTIONAL, INTENT(IN) :: do_orthoovp
   
   !
   ! local variables
   !
   CHARACTER(19)     :: subname="atmproj_to_internal"
   INTEGER           :: iunit, ounit
   LOGICAL           :: do_orthoovp_
   !
   CHARACTER(nstrx)  :: attr, energy_units
   CHARACTER(nstrx)  :: filetype_
   LOGICAL           :: write_ham, write_space, write_loc
   REAL(dbl)         :: avec(3,3), bvec(3,3), norm, alat, efermi, nelec
   INTEGER           :: dimwann, natomwfc, nkpts, nspin, nbnd
   INTEGER           :: nk(3), shift(3), nrtot, nr(3)
   INTEGER           :: i, j, ir, ik, ib, isp
   INTEGER           :: ierr
   !
   INTEGER,        ALLOCATABLE :: ivr(:,:), itmp(:)
   REAL(dbl),      ALLOCATABLE :: vkpt_cry(:,:), vkpt(:,:), wk(:), wr(:), vr(:,:)
   REAL(dbl),      ALLOCATABLE :: eig(:,:,:)
   REAL(dbl),      ALLOCATABLE :: rtmp(:,:)
   COMPLEX(dbl),   ALLOCATABLE :: rham(:,:,:,:), kham(:,:,:)
   COMPLEX(dbl),   ALLOCATABLE :: proj(:,:,:,:)
   COMPLEX(dbl),   ALLOCATABLE :: kovp(:,:,:,:), rovp(:,:,:,:)
   COMPLEX(dbl),   ALLOCATABLE :: cu_tmp(:,:,:)
   !
   COMPLEX(dbl),   ALLOCATABLE :: zaux(:,:), ztmp(:,:)
   COMPLEX(dbl),   ALLOCATABLE :: kovp_sq(:,:)
   REAL(dbl),      ALLOCATABLE :: w(:)
 
!
!------------------------------
! main body
!------------------------------
!
   CALL timing( subname, OPR='start' )
   CALL log_push( subname )


   !
   ! re-initialize all the times
   !
   CALL atmproj_tools_init( filein, ierr )
   IF ( ierr/=0 ) CALL errore(subname,'initializing atmproj',10)


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
   write_loc     = .FALSE.
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
   CASE( 'loc', 'localization' )
      write_loc = .TRUE.
   CASE DEFAULT
      CALL errore(subname, 'invalid filetype: '//TRIM(filetype_), 71 )
   END SELECT

   !
   ! orthogonalization controlled by input
   ! NOTE: states are non-orthogonal by default
   !
   do_orthoovp_ = .FALSE.
   IF ( PRESENT( do_orthoovp ) ) do_orthoovp_ = do_orthoovp

!
!---------------------------------
! read data from filein (by projwfc, QE suite)
!---------------------------------
!
 
   !
   ! get lattice information
   !
   !CALL qexml_init( iunit, DIR=savedir)
   CALL qexml_openfile( file_data, "read", IERR=ierr )
   IF ( ierr/=0 ) CALL errore(subname,"opening "//TRIM(file_data), ABS(ierr) )
   ! 
   CALL qexml_read_cell( A1=avec(:,1), A2=avec(:,2), A3=avec(:,3), &
                         B1=bvec(:,1), B2=bvec(:,2), B3=bvec(:,3), IERR=ierr )
   IF ( ierr/=0 ) CALL errore(subname,"reading avec, bvec", ABS(ierr) )
   !
   CALL qexml_closefile( "read", IERR=ierr )
   IF ( ierr/=0 ) CALL errore(subname,"closing "//TRIM(file_data), ABS(ierr) )

   !
   ! bvec is in 2pi/a units
   ! convert it to bohr^-1
   !
   alat = DOT_PRODUCT( avec(:,1), avec(:,1) )
   alat = SQRT(alat)
   !
   bvec = bvec * TPI / alat


   !
   ! reading dimensions
   ! and small data
   !
   CALL atmproj_read_ext( filein, nbnd, nkpts, nspin, natomwfc, &
                          nelec, efermi, energy_units, IERR=ierr)

   IF ( ierr/=0 ) CALL errore(subname, "reading dimensions I", ABS(ierr))

   dimwann = natomwfc
   
   !
   ! allocations
   !
   ALLOCATE( vkpt(3,nkpts), wk(nkpts), STAT=ierr )
   IF (ierr/=0) CALL errore(subname, 'allocating vkpt, wk', ABS(ierr))
   ALLOCATE( vkpt_cry(3, nkpts), STAT=ierr )
   IF ( ierr/=0 ) CALL errore(subname, 'allocating vkpt_cry', ABS(ierr) )
   !
   ALLOCATE( eig(nbnd,nkpts,nspin), STAT=ierr )
   IF (ierr/=0) CALL errore(subname, 'allocating eig', ABS(ierr))
   !
   ALLOCATE( proj(nbnd,natomwfc,nkpts,nspin), STAT=ierr )
   IF (ierr/=0) CALL errore(subname, 'allocating proj', ABS(ierr))

   !
   ! read-in massive data
   !
   IF ( do_orthoovp_ ) THEN
       !
       ALLOCATE( kovp(1,1,1,1), STAT=ierr )
       IF (ierr/=0) CALL errore(subname, 'allocating kovp I', ABS(ierr))
       !
       CALL atmproj_read_ext( filein, VKPT=vkpt, WK=wk, EIG=eig, PROJ=proj, IERR=ierr )
       !
   ELSE
       !
       ALLOCATE( kovp(natomwfc,natomwfc,nkpts,nspin), STAT=ierr )
       IF (ierr/=0) CALL errore(subname, 'allocating kovp II', ABS(ierr))
       !
       ! reading < beta_i | evc_n >
       !
       CALL atmproj_read_ext( filein, VKPT=vkpt, WK=wk, EIG=eig, PROJ=proj, KOVP=kovp, IERR=ierr )
       !
   ENDIF
   !
   IF ( ierr/=0 ) CALL errore(subname, "reading data II", ABS(ierr))


   !
   ! units (first we convert to bohr^-1, 
   ! then we want vkpt to be re-written in crystal units)
   !
   vkpt = vkpt * TPI / alat
   !
   vkpt_cry = vkpt
   CALL cart2cry( vkpt_cry, bvec ) 
   !
   CALL get_monkpack( nk, shift, nkpts, vkpt_cry, 'CRYSTAL', bvec, ierr)
   IF ( ierr/=0 ) CALL errore(subname,'kpt grid not Monkhorst-Pack',ABS(ierr))

   !
   ! check the normalization of the weights
   !
   norm   = SUM( wk )
   wk(:)  = wk(:) / norm


   !
   ! kpts and real-space lattice vectors
   !
   nr(1:3) = nk(1:3)
   !
   ! get the grid dimension
   !
   CALL grids_get_rgrid(nr, NRTOT=nrtot )
   !
   ALLOCATE( ivr(3, nrtot), vr(3, nrtot), STAT=ierr )
   IF ( ierr/=0 ) CALL errore(subname, 'allocating ivr, vr', ABS(ierr) )
   ALLOCATE( wr(nrtot), STAT=ierr )
   IF ( ierr/=0 ) CALL errore(subname, 'allocating wr', ABS(ierr) )
   !
   CALL grids_get_rgrid(nr, WR=wr, IVR=ivr )
   !
   vr(:,:) = REAL( ivr, dbl)
   CALL cry2cart( vr, avec)
 
   

   !
   ! efermi and eigs are converted to eV's
   !
   CALL change_case( energy_units, 'lower' )
   !
   SELECT CASE( ADJUSTL(TRIM(energy_units)) )
   CASE ( "ha", "hartree", "au" )
      !
      efermi = efermi * TWO * RYD 
      eig    = eig    * TWO * RYD
      !
   CASE ( "ry", "ryd", "rydberg" )
      !
      efermi = efermi * RYD 
      eig    = eig    * RYD
      !
   CASE ( "ev", "electronvolt" )
      !
      ! do nothing
   CASE DEFAULT
      CALL errore( subname, 'unknown units for efermi: '//TRIM(energy_units), 72)
   END SELECT

   !
   ! apply the energy shift eshift,
   ! meant to set the zero of the energy scale (where we may have
   ! spurious 0 eigenvalues) far from any physical energy region of interest
   !
   eig    = eig    -eshift
   efermi = efermi -eshift



   ! 
   ! build the Hamiltonian in real space
   ! 
   IF ( write_ham ) THEN

       ALLOCATE( rham(dimwann, dimwann, nrtot, nspin), STAT=ierr )
       IF ( ierr/=0 ) CALL errore(subname, 'allocating rham', ABS(ierr) )
       ALLOCATE( kham(dimwann, dimwann, nkpts), STAT=ierr )
       IF ( ierr/=0 ) CALL errore(subname, 'allocating kham', ABS(ierr) )
       !
       IF ( .NOT. do_orthoovp_ ) THEN
           ALLOCATE( rovp(natomwfc,natomwfc,nrtot,nspin), STAT=ierr )
           IF (ierr/=0) CALL errore(subname, 'allocating rovp', ABS(ierr))
       ENDIF


       DO isp = 1, nspin

           IF ( TRIM(spin_component) == "up"   .AND. isp == 2 ) CYCLE
           IF ( TRIM(spin_component) == "down" .AND. isp == 1 ) CYCLE
           IF ( TRIM(spin_component) == "dw"   .AND. isp == 1 ) CYCLE

           !
           ! build kham
           !
           DO ik = 1, nkpts
               !
               DO j = 1, dimwann
               DO i = 1, dimwann
                   !
                   kham(i,j,ik) = ZERO
                   !
                   DO ib = 1, nbnd
                       !
                       kham(i,j,ik) = kham(i,j,ik) + &
                                                ( proj(ib,i,ik,isp) ) * eig(ib,ik,isp) * &
                                           CONJG( proj(ib,j,ik,isp) )
                       !
                   ENDDO
                   !
               ENDDO
               ENDDO
               !
               !
               IF ( .NOT. mat_is_herm( dimwann, kham(:,:,ik), TOLL=EPS_m8 ) ) &
                   CALL errore(subname,'kham not hermitean',10)


               !
               ! fermi energy is taken into accout
               ! The energy shift is performed on the final matrix
               ! and not on the DFT eigenvalues            
               ! as     eig(:,:,:) = eig(:,:,:) -efermi
               ! because the atomic basis is typically very large 
               ! and would require a lot of bands to be described
               !
               DO i = 1, dimwann
                   !
                   kham(i,i,ik) = kham(i,i,ik) -efermi
                   !
               ENDDO

               !
               ! overlaps
               ! projections are read orthogonals, if non-orthogonality
               ! is required, we multiply by S^1/2
               !
               IF ( .NOT. do_orthoovp_ ) THEN
                   !
                   ALLOCATE( zaux(dimwann,dimwann), ztmp(dimwann,dimwann), STAT=ierr )
                   IF ( ierr/=0 ) CALL errore(subname, 'allocating raux-rtmp', ABS(ierr) )
                   ALLOCATE( w(dimwann), kovp_sq(dimwann,dimwann), STAT=ierr )
                   IF ( ierr/=0 ) CALL errore(subname, 'allocating w, kovp_sq', ABS(ierr) )
                   !
                   CALL mat_hdiag( zaux, w(:), kovp(:,:,ik,isp), dimwann)
                   !              
                   DO i = 1, dimwann
                       !
                       IF ( w(i) <= ZERO ) CALL errore(subname,'unexpected eig < = 0 ',i)
                       w(i) = SQRT( w(i) )
                       !
                   ENDDO
                   !
                   DO j = 1, dimwann
                   DO i = 1, dimwann
                       !
                       ztmp(i,j) = zaux(i,j) * w(j)
                       !
                   ENDDO
                   ENDDO
                   !
                   CALL mat_mul( kovp_sq, ztmp, 'N', zaux, 'C', dimwann, dimwann, dimwann)
                   !
                   IF ( .NOT. mat_is_herm( dimwann, kovp_sq, TOLL=EPS_m8 ) ) &
                       CALL errore(subname,'kovp_sq not hermitean',10)

                   !
                   ! apply the basis change to the Hamiltonian
                   ! multiply kovp_sq (S^1/2) to the right and the left of kham
                   !
                   CALL mat_mul( zaux, kovp_sq,      'N', kham(:,:,ik), 'N', dimwann, dimwann, dimwann)
                   CALL mat_mul( kham(:,:,ik), zaux, 'N', kovp_sq,      'N', dimwann, dimwann, dimwann)
                   !
                   !
                   DEALLOCATE( zaux, ztmp, STAT=ierr)
                   IF ( ierr/=0 ) CALL errore(subname,'deallocating zaux, ztmp',ABS(ierr))
                   DEALLOCATE( w, kovp_sq, STAT=ierr)
                   IF ( ierr/=0 ) CALL errore(subname,'deallocating w, kovp_sq',ABS(ierr))
                   !
               ENDIF
               !
           ENDDO

           ! 
           ! convert to real space
           !
           DO ir = 1, nrtot
               !
               CALL compute_rham( dimwann, vr(:,ir), rham(:,:,ir,isp), &
                                  nkpts, vkpt, wk, kham )
               !
               IF ( .NOT. do_orthoovp_ ) THEN
                   !
                    CALL compute_rham( dimwann, vr(:,ir), rovp(:,:,ir,isp), &
                                       nkpts, vkpt, wk, kovp(:,:,:,isp) )
                   !
               ENDIF
               !
           ENDDO
           !
       ENDDO 
       ! 
       DEALLOCATE( kham, STAT=ierr )
       IF ( ierr/=0 ) CALL errore(subname, 'deallocating kham', ABS(ierr) )
       !
   ENDIF
   !
   IF ( ALLOCATED( kovp ) ) THEN
       DEALLOCATE( kovp, STAT=ierr)
       IF ( ierr/=0 ) CALL errore(subname, 'deallocating kovp', ABS(ierr) )
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
       CALL iotk_write_attr( attr,"spin_component",TRIM(spin_component))
       CALL iotk_write_attr( attr,"nk",nk)
       CALL iotk_write_attr( attr,"shift",shift)
       CALL iotk_write_attr( attr,"nrtot",nrtot)
       CALL iotk_write_attr( attr,"nr",nr)
       CALL iotk_write_attr( attr,"have_overlap", .NOT. do_orthoovp_ )
       CALL iotk_write_attr( attr,"fermi_energy", 0.0_dbl )
       CALL iotk_write_empty( ounit,"DATA",ATTR=attr)
       !
       CALL iotk_write_attr( attr,"units","bohr",FIRST=.TRUE.)
       CALL iotk_write_dat( ounit,"DIRECT_LATTICE", avec, ATTR=attr, COLUMNS=3)
       !
       CALL iotk_write_attr( attr,"units","bohr^-1",FIRST=.TRUE.)
       CALL iotk_write_dat( ounit,"RECIPROCAL_LATTICE", bvec, ATTR=attr, COLUMNS=3)
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
          IF ( nspin == 2 .AND. TRIM(spin_component) == "all" ) THEN
              !
              CALL iotk_write_begin( ounit, "SPIN"//TRIM(iotk_index(isp)) )
              !
          ENDIF
          !
          !
          CALL iotk_write_begin( ounit,"RHAM")
          !
          DO ir = 1, nrtot
              !
              CALL iotk_write_dat( ounit,"VR"//TRIM(iotk_index(ir)), rham(:,:, ir, isp) )
              !
              IF ( .NOT. do_orthoovp_ ) THEN
                  CALL iotk_write_dat( ounit,"OVERLAP"//TRIM(iotk_index(ir)), &
                                       rovp( :, :, ir, isp) )
              ENDIF
              !
              !
          ENDDO
          !
          CALL iotk_write_end( ounit,"RHAM")
          !
          IF ( nspin == 2 .AND. TRIM(spin_component) == "all" ) THEN
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
       CALL iotk_write_attr( attr,"nbnd",nbnd,FIRST=.TRUE.)
       CALL iotk_write_attr( attr,"nkpts",nkpts)
       CALL iotk_write_attr( attr,"nspin",nspin)
       CALL iotk_write_attr( attr,"spin_component",TRIM(spin_component))
       CALL iotk_write_attr( attr,"efermi", 0.0_dbl )
       CALL iotk_write_attr( attr,"dimwinx", nbnd )
       CALL iotk_write_empty( ounit,"DATA",ATTR=attr)
       !
       ALLOCATE( itmp(nkpts), STAT=ierr )
       IF ( ierr/=0 ) CALL errore(subname, 'allocating itmp', ABS(ierr))
       !
       itmp(:) = nbnd
       CALL iotk_write_dat( ounit, "DIMWIN", itmp, COLUMNS=8 )
       itmp(:) = 1
       CALL iotk_write_dat( ounit, "IMIN", itmp, COLUMNS=8 )
       itmp(:) = nbnd
       CALL iotk_write_dat( ounit, "IMAX", itmp, COLUMNS=8 )
       !
       DO isp = 1, nspin
           !
           IF ( nspin == 2 ) THEN
               CALL iotk_write_begin( ounit, "SPIN"//TRIM(iotk_index(isp)) )
           ENDIF
           !
           CALL iotk_write_dat( ounit, "EIG", eig(:,:,isp), COLUMNS=4)
           !
           IF ( nspin == 2 ) THEN
               CALL iotk_write_end( ounit, "SPIN"//TRIM(iotk_index(isp)) )
           ENDIF
           !
       ENDDO
       !
       CALL iotk_write_end( ounit, "WINDOWS" )
       !
       !
       CALL iotk_write_begin( ounit, "SUBSPACE" )
       !
       CALL iotk_write_attr( attr,"dimwinx",nbnd,FIRST=.TRUE.)
       CALL iotk_write_attr( attr,"nkpts",nkpts)
       CALL iotk_write_attr( attr,"dimwann", dimwann)
       CALL iotk_write_empty( ounit,"DATA",ATTR=attr)
       !
       itmp(:) = nbnd 
       CALL iotk_write_dat( ounit, "DIMWIN", itmp, COLUMNS=8 )
       !
       DO isp = 1, nspin
           !
           IF ( nspin == 2 ) THEN
               CALL iotk_write_begin( ounit, "SPIN"//TRIM(iotk_index(isp)) )
           ENDIF
           !
           DO ik = 1, nkpts
               !
               CALL iotk_write_dat( ounit, "EAMP"//TRIM(iotk_index(ik)), &
                                    proj(1:nbnd,1:dimwann,ik,isp) ) 
               !
           ENDDO
           !
           IF ( nspin == 2 ) THEN
               CALL iotk_write_end( ounit, "SPIN"//TRIM(iotk_index(isp)) )
           ENDIF
           !
       ENDDO
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


   IF ( write_loc ) THEN
       !
       CALL iotk_open_write( ounit, FILE=TRIM(fileout), BINARY=binary )
       !
       CALL iotk_write_begin( ounit, "WANNIER_LOCALIZATION" )
       !
       CALL iotk_write_attr( attr,"dimwann",dimwann,FIRST=.TRUE.)
       CALL iotk_write_attr( attr,"nkpts",nkpts)
       CALL iotk_write_empty( ounit,"DATA",ATTR=attr)
       !
       CALL iotk_write_attr( attr,"Omega_I",0.0d0,FIRST=.TRUE.)
       CALL iotk_write_attr( attr,"Omega_D",0.0d0)
       CALL iotk_write_attr( attr,"Omega_OD",0.0d0)
       CALL iotk_write_attr( attr,"Omega_tot",0.0d0)
       CALL iotk_write_empty( ounit,"SPREADS",ATTR=attr)
       !
       ALLOCATE( rtmp(3,dimwann), STAT=ierr )
       IF ( ierr/=0 ) CALL errore(subname, 'allocating rtmp', ABS(ierr))
       ALLOCATE( cu_tmp(dimwann,dimwann,nkpts), STAT=ierr )
       IF ( ierr/=0 ) CALL errore(subname, 'allocating cu_tmp', ABS(ierr))
       !
       cu_tmp(:,:,:) = 0.0d0
       !
       DO ik = 1, nkpts
       DO i  = 1, dimwann
           cu_tmp(i,i,ik) = 1.0d0
       ENDDO
       ENDDO
       !
       rtmp(:,:)=0.0d0
       !
       CALL iotk_write_dat(ounit,"CU",cu_tmp)
       CALL iotk_write_dat(ounit,"RAVE",rtmp,COLUMNS=3)
       CALL iotk_write_dat(ounit,"RAVE2",rtmp(1,:))
       CALL iotk_write_dat(ounit,"R2AVE",rtmp(1,:))
       !
       DEALLOCATE( rtmp, cu_tmp, STAT=ierr )
       IF ( ierr/=0 ) CALL errore(subname, 'deallocating rtmp, cu_tmp', ABS(ierr))
       !
       CALL iotk_write_end( ounit, "WANNIER_LOCALIZATION" )
       !
       CALL iotk_close_write( ounit )
       !
   ENDIF




!
! local cleaning
!

   DEALLOCATE( proj, STAT=ierr )
   IF ( ierr/=0 ) CALL errore(subname, 'deallocating proj', ABS(ierr) )
   !
   DEALLOCATE( vkpt, wk, STAT=ierr )
   IF ( ierr/=0 ) CALL errore(subname, 'deallocating vkpt, wk', ABS(ierr) )
   DEALLOCATE( vkpt_cry, STAT=ierr )
   IF ( ierr/=0 ) CALL errore(subname, 'deallocating vkpt_cry', ABS(ierr) )
   !
   DEALLOCATE( ivr, wr, STAT=ierr )
   IF ( ierr/=0 ) CALL errore(subname, 'deallocating ivr, wr', ABS(ierr) )
   DEALLOCATE( vr, STAT=ierr )
   IF ( ierr/=0 ) CALL errore(subname, 'deallocating vr', ABS(ierr) )
   !
   DEALLOCATE( eig, STAT=ierr )
   IF ( ierr/=0 ) CALL errore(subname, 'deallocating eig', ABS(ierr) )
   !
   IF( ALLOCATED( rham ) ) THEN
       DEALLOCATE( rham, STAT=ierr )
       IF ( ierr/=0 ) CALL errore(subname, 'deallocating rham', ABS(ierr) )
   ENDIF
   IF( ALLOCATED( rovp ) ) THEN
       DEALLOCATE( rovp, STAT=ierr )
       IF ( ierr/=0 ) CALL errore(subname, 'deallocating rovp', ABS(ierr) )
   ENDIF
   

   CALL log_pop( subname )
   CALL timing( subname, OPR='stop' )
   !
   RETURN
   !
END SUBROUTINE atmproj_to_internal


!**********************************************************
   LOGICAL FUNCTION file_is_atmproj( filename )
   !**********************************************************
   !
   IMPLICIT NONE
     !
     ! check for atmproj fmt
     !
     CHARACTER(*)     :: filename
     !
     INTEGER          :: nbnd, nkpts, nspin, natomwfc
     INTEGER          :: ierr
     LOGICAL          :: lerror
    

     file_is_atmproj = .FALSE.
     lerror = .FALSE.
     !
     IF ( .NOT. init ) THEN 
         !
         CALL atmproj_tools_init( filename, ierr )
         IF ( ierr/=0 ) lerror = .TRUE. 
         !
     ENDIF
     !
     IF ( lerror ) RETURN
     !
     CALL atmproj_read_ext( filename, NBND=nbnd, NKPT=nkpts, &
                            NSPIN=nspin, NATOMWFC=natomwfc,  IERR=ierr )
     IF ( ierr/=0 ) lerror = .TRUE.

     IF ( lerror ) RETURN
     !
     file_is_atmproj = .TRUE.
     !
  END FUNCTION file_is_atmproj


!*************************************************
SUBROUTINE atmproj_read_ext ( filein, nbnd, nkpt, nspin, natomwfc, nelec, &
                              efermi, energy_units, vkpt, wk, eig, proj, kovp, ierr )
   !*************************************************
   !
   USE kinds,          ONLY : dbl 
   USE iotk_module
   !
   IMPLICIT NONE
   !
   CHARACTER(*),           INTENT(IN)   :: filein
   INTEGER,      OPTIONAL, INTENT(OUT)  :: nbnd, nkpt, nspin, natomwfc
   REAL(dbl),    OPTIONAL, INTENT(OUT)  :: nelec, efermi
   CHARACTER(*), OPTIONAL, INTENT(OUT)  :: energy_units
   REAL(dbl),    OPTIONAL, INTENT(OUT)  :: vkpt(:,:), wk(:), eig(:,:,:)
   COMPLEX(dbl), OPTIONAL, INTENT(OUT)  :: proj(:,:,:,:)
   COMPLEX(dbl), OPTIONAL, INTENT(OUT)  :: kovp(:,:,:,:)
   INTEGER,                INTENT(OUT)  :: ierr
   !
   !
   CHARACTER(256)    :: attr, str
   INTEGER           :: iunit
   INTEGER           :: ik, isp, ias
   !
   INTEGER           :: nbnd_, nkpt_, nspin_, natomwfc_ 
   REAL(dbl)         :: nelec_, efermi_
   CHARACTER(20)     :: energy_units_


   CALL iotk_free_unit( iunit )
   ierr = 0

   CALL iotk_open_read( iunit, FILE=TRIM(filein), IERR=ierr )
   IF ( ierr/=0 ) RETURN
   !
   !
   CALL iotk_scan_begin( iunit, "HEADER", IERR=ierr) 
   IF ( ierr/=0 ) RETURN
   !
   CALL iotk_scan_dat( iunit, "NUMBER_OF_BANDS", nbnd_, IERR=ierr) 
   IF ( ierr/=0 ) RETURN
   CALL iotk_scan_dat( iunit, "NUMBER_OF_K-POINTS", nkpt_, IERR=ierr) 
   IF ( ierr/=0 ) RETURN
   CALL iotk_scan_dat( iunit, "NUMBER_OF_SPIN_COMPONENTS", nspin_, IERR=ierr) 
   IF ( ierr/=0 ) RETURN
   CALL iotk_scan_dat( iunit, "NUMBER_OF_ATOMIC_WFC", natomwfc_, IERR=ierr) 
   IF ( ierr/=0 ) RETURN
   CALL iotk_scan_dat( iunit, "NUMBER_OF_ELECTRONS", nelec_, IERR=ierr) 
   IF ( ierr/=0 ) RETURN
   !
   CALL iotk_scan_empty( iunit, "UNITS_FOR_ENERGY", ATTR=attr, IERR=ierr) 
   IF ( ierr/=0 ) RETURN
   CALL iotk_scan_attr( attr, "UNITS", energy_units_, IERR=ierr) 
   IF ( ierr/=0 ) RETURN
   !
   CALL iotk_scan_dat( iunit, "FERMI_ENERGY", efermi_, IERR=ierr) 
   IF ( ierr/=0 ) RETURN
   !
   CALL iotk_scan_end( iunit, "HEADER", IERR=ierr) 
   IF ( ierr/=0 ) RETURN
   
   ! 
   ! reading kpoints and weights 
   ! 
   IF ( PRESENT( vkpt ) ) THEN
       !
       CALL iotk_scan_dat( iunit, "K-POINTS", vkpt(:,:), IERR=ierr )
       IF ( ierr/=0 ) RETURN
       !
   ENDIF
   !
   IF ( PRESENT (wk) ) THEN
       !
       CALL iotk_scan_dat( iunit, "WEIGHT_OF_K-POINTS", wk(:), IERR=ierr )
       IF ( ierr/=0 ) RETURN
       !
   ENDIF
   
   ! 
   ! reading eigenvalues
   ! 
   IF ( PRESENT( eig ) ) THEN
       ! 
       CALL iotk_scan_begin( iunit, "EIGENVALUES", IERR=ierr )
       IF ( ierr/=0 ) RETURN
       !
       !
       DO ik = 1, nkpt_
           !
           CALL iotk_scan_begin( iunit, "K-POINT"//TRIM(iotk_index(ik)), IERR=ierr )
           IF ( ierr/=0 ) RETURN
           !
           IF ( nspin_ == 1 ) THEN
                !
                isp = 1
                !
                CALL iotk_scan_dat(iunit, "EIG" , eig(:,ik, isp ), IERR=ierr)
                IF ( ierr /= 0 ) RETURN
                !
           ELSE
                !
                DO isp=1,nspin_
                   !
                   str = "EIG"//TRIM(iotk_index(isp))
                   !
                   CALL iotk_scan_dat(iunit, TRIM(str) , eig(:,ik,isp), IERR=ierr)
                   IF ( ierr /= 0 ) RETURN
                   !
                ENDDO
                !
           ENDIF       
           !
           !
           CALL iotk_scan_end( iunit, "K-POINT"//TRIM(iotk_index(ik)), IERR=ierr )
           IF ( ierr/=0 ) RETURN
           !
       ENDDO
       !
       !
       CALL iotk_scan_end( iunit, "EIGENVALUES", IERR=ierr )
       IF ( ierr/=0 ) RETURN
       !
   ENDIF


   ! 
   ! reading projections
   ! 
   IF ( PRESENT( proj ) ) THEN
       !
       CALL iotk_scan_begin( iunit, "PROJECTIONS", IERR=ierr )
       IF ( ierr/=0 ) RETURN
       !
       !
       DO ik = 1, nkpt_
           !
           !
           CALL iotk_scan_begin( iunit, "K-POINT"//TRIM(iotk_index(ik)), IERR=ierr )
           IF ( ierr/=0 ) RETURN
           !
           DO isp = 1, nspin_
               !
               IF ( nspin_ == 2 ) THEN
                  !
                  CALL iotk_scan_begin( iunit, "SPIN"//TRIM(iotk_index(isp)), IERR=ierr )
                  IF ( ierr/=0 ) RETURN
                  !
               ENDIF
               !
               DO ias = 1, natomwfc_
                  !
                  str= "ATMWFC"//TRIM( iotk_index( ias ) )
                  !
                  CALL iotk_scan_dat(iunit, TRIM(str) , proj( :, ias, ik, isp ), IERR=ierr)
                  IF ( ierr /= 0 ) RETURN
                  !
               ENDDO
               !
               IF ( nspin_ == 2 ) THEN
                  !
                  CALL iotk_scan_end( iunit, "SPIN"//TRIM(iotk_index(isp)), IERR=ierr )
                  IF ( ierr/=0 ) RETURN
                  !
               ENDIF
               !
           ENDDO
           !
           !
           CALL iotk_scan_end( iunit, "K-POINT"//TRIM(iotk_index(ik)), IERR=ierr )
           IF ( ierr/=0 ) RETURN
           !
       ENDDO
       !
       !
       CALL iotk_scan_end( iunit, "PROJECTIONS", IERR=ierr )
       IF ( ierr/=0 ) RETURN
       !
   ENDIF

   ! 
   ! reading overlaps
   ! 
   IF ( PRESENT( kovp ) ) THEN
       !
       CALL iotk_scan_begin( iunit, "OVERLAPS", IERR=ierr )
       IF ( ierr/=0 ) RETURN
       !
       !
       DO ik = 1, nkpt_
           !
           !
           CALL iotk_scan_begin( iunit, "K-POINT"//TRIM(iotk_index(ik)), IERR=ierr )
           IF ( ierr/=0 ) RETURN
           !
           DO isp = 1, nspin_
               !
               CALL iotk_scan_dat(iunit, "OVERLAP"//TRIM(iotk_index(isp)), kovp( :, :, ik, isp ), IERR=ierr)
               IF ( ierr/=0 ) RETURN
               !
           ENDDO
           !
           CALL iotk_scan_end( iunit, "K-POINT"//TRIM(iotk_index(ik)), IERR=ierr )
           IF ( ierr/=0 ) RETURN
           !
       ENDDO
       !
       CALL iotk_scan_end( iunit, "OVERLAPS", IERR=ierr )
       IF ( ierr/=0 ) RETURN
       !
   ENDIF
   !
   CALL iotk_close_read( iunit, IERR=ierr )
   IF ( ierr/=0 ) RETURN
   !
   !
   IF ( PRESENT( nbnd ) )         nbnd = nbnd_
   IF ( PRESENT( nkpt ) )         nkpt = nkpt_
   IF ( PRESENT( nspin ) )        nspin = nspin_
   IF ( PRESENT( natomwfc ) )     natomwfc = natomwfc_
   IF ( PRESENT( nelec ) )        nelec = nelec_
   IF ( PRESENT( efermi ) )       efermi = efermi_
   IF ( PRESENT( energy_units ) ) energy_units = TRIM(energy_units_)
   !
   RETURN
   !
END SUBROUTINE atmproj_read_ext


END MODULE atmproj_tools_module
