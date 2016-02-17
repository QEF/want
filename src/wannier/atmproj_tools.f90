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
   USE kinds,               ONLY : dbl
   USE constants,           ONLY : BOHR => bohr_radius_angs, ZERO, ONE, TWO, &
                                   RYD, EPS_m8, TPI, CZERO 
   USE parameters,          ONLY : nstrx
   USE timing_module,       ONLY : timing
   USE log_module,          ONLY : log_push, log_pop
   USE io_global_module,    ONLY : stdout
   USE converters_module,   ONLY : cart2cry, cry2cart
   USE parser_module,       ONLY : change_case
   USE util_module,         ONLY : mat_is_herm, mat_mul
   USE grids_module,        ONLY : grids_get_rgrid
   USE files_module,        ONLY : file_exist
   USE pseudo_types_module, ONLY : pseudo_upf 
   USE uspp_param,          ONLY : upf
   USE lattice_module,      ONLY : lattice_read_ext, lattice_init
   USE symmetry_module,     ONLY : symmetry_read_ext, nsym, symmetry_deallocate
   USE ions_module,         ONLY : ions_read_ext, ions_init, ions_deallocate
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
   REAL(dbl)          :: atmproj_sh = 10.0d0
   REAL(dbl)          :: atmproj_thr = 0.0d0    ! 0.9d0
   INTEGER            :: atmproj_nbnd = 0
   INTEGER            :: atmproj_nbndmin = 1
   CHARACTER(256)     :: spin_component = "all"
   !
   INTEGER, PARAMETER :: nwfcx = 50

   ! contains:
   ! SUBROUTINE  atmproj_to_internal( filein, fileham, filespace, filewan, do_orthoovp )
   ! FUNCTION    file_is_atmproj( filein )
   ! SUBROUTINE  atmproj_get_natomwfc( nsp, psfile, natomwfc )
   ! FUNCTION    atmproj_get_index( i, ia, natomwfc(:) )
   !
   PUBLIC :: atmproj_to_internal
   PUBLIC :: file_is_atmproj
   PUBLIC :: atmproj_get_index
   PUBLIC :: atmproj_get_natomwfc
   !
   PUBLIC :: atmproj_sh
   PUBLIC :: atmproj_thr
   PUBLIC :: atmproj_nbnd
   PUBLIC :: atmproj_nbndmin
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
   ierr     =  0
   init     = .TRUE.
   !
END SUBROUTINE atmproj_tools_init


!**********************************************************
   SUBROUTINE atmproj_to_internal( filein, fileqp, fileham, filespace, filewan, do_orthoovp )
   !**********************************************************
   !
   ! Convert the datafile written by the projwfc program (QE suite) to
   ! the internal representation.
   !
   ! 3 files are creates: fileham, filespace, filewan
   !
   USE symmetrize_kgrid_module,    ONLY : symmetrize_kgrid
   USE lattice_module,             ONLY : avec, bvec, alat
   USE util_module
   !
   IMPLICIT NONE

   LOGICAL, PARAMETER :: binary = .TRUE.
   
   !
   ! input variables
   !
   CHARACTER(*), INTENT(IN) :: filein
   CHARACTER(*), OPTIONAL, INTENT(IN) :: fileham, filespace, filewan
   CHARACTER(*), OPTIONAL, INTENT(IN) :: fileqp
   LOGICAL,      OPTIONAL, INTENT(IN) :: do_orthoovp
   
   !
   ! local variables
   !
   CHARACTER(19)     :: subname="atmproj_to_internal"
   INTEGER           :: iunit, ounit
   LOGICAL           :: do_orthoovp_
   INTEGER           :: atmproj_nbnd_
   !
   CHARACTER(nstrx)  :: attr, energy_units
   CHARACTER(nstrx)  :: filetype_
   LOGICAL           :: write_ham, write_space, write_loc
   REAL(dbl)         :: norm, efermi, nelec
   LOGICAL           :: spin_noncollinear
   REAL(dbl)         :: proj_wgt
   INTEGER           :: dimwann, natomwfc, nkpts, nspin, nbnd
   INTEGER           :: nkpts_all, nspin_
   INTEGER           :: nk(3), shift(3), nrtot, nr(3)
   INTEGER           :: i, j, ir, ik, ikeq, ib, isp
   INTEGER           :: ierr
   !
   INTEGER,        ALLOCATABLE :: ivr(:,:), itmp(:)
   INTEGER,        ALLOCATABLE :: kpteq_map(:), kpteq_symm(:)
   REAL(dbl),      ALLOCATABLE :: vkpt_cry(:,:), vkpt(:,:), wk(:), wr(:), vr(:,:)
   REAL(dbl),      ALLOCATABLE :: vkpt_all(:,:), wk_all(:)
   REAL(dbl),      ALLOCATABLE :: eig(:,:,:)
   REAL(dbl),      ALLOCATABLE :: rtmp(:,:)
   COMPLEX(dbl),   ALLOCATABLE :: rham(:,:,:,:), kham(:,:,:)
   COMPLEX(dbl),   ALLOCATABLE :: proj(:,:,:,:)
   COMPLEX(dbl),   ALLOCATABLE :: kovp(:,:,:,:), rovp(:,:,:,:)
   COMPLEX(dbl),   ALLOCATABLE :: cu_tmp(:,:,:), eamp_tmp(:,:)
   !
   COMPLEX(dbl),   ALLOCATABLE :: zaux(:,:), ztmp(:,:)
   COMPLEX(dbl),   ALLOCATABLE :: kovp_sq(:,:)
   REAL(dbl),      ALLOCATABLE :: w(:)

#if defined __SHIFT_TEST
   !Luis 3: Changes related to Sohrab's shifting scheme 
   !PA   = A^dagger * A 
   !I_PA = inv(PA)
   COMPLEX(dbl),   ALLOCATABLE :: A(:,:),PA(:,:), IPA(:,:), kham_aux(:,:), E(:,:)
   INTEGER           :: shifting_scheme = 2
#endif
  
#if defined __WRITE_ASCIIHAM
   CHARACTER(100)    :: kham_file
   INTEGER           :: iw,jw
#endif
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
   ! what files are to be written
   !
   write_ham = .FALSE.
   write_space = .FALSE.
   write_loc = .FALSE.
   IF ( PRESENT(fileham) )    write_ham = .TRUE.
   IF ( PRESENT(filespace) )  write_space = .TRUE.
   IF ( PRESENT(filewan) )    write_loc = .TRUE.

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
   ! get DFT data
   ! qexml fmt is assumed (otherwise one should use the var "dftdata_fmt")
   ! note that the parallelism inside *_read_ext routines may be dangerous
   !
   !CALL qexml_init( iunit, DIR=savedir)
   CALL qexml_openfile( file_data, "read", IERR=ierr )
   IF ( ierr/=0 ) CALL errore(subname,"opening "//TRIM(file_data), ABS(ierr) )
   ! 
   CALL lattice_read_ext( "qexml", LPARA=.FALSE. )
   CALL lattice_init( )
   !
   CALL ions_read_ext( "qexml", LPARA=.FALSE. )
   CALL ions_init()
   !
   CALL symmetry_read_ext( "qexml", LPARA=.FALSE. )
   !
   CALL qexml_closefile( "read", IERR=ierr )
   IF ( ierr/=0 ) CALL errore(subname,"closing "//TRIM(file_data), ABS(ierr) )

   !
   ! read pseudos
   !
   CALL readpp()

   !
   ! reading dimensions
   ! and small data
   !
   CALL atmproj_read_ext( filein, nbnd, nkpts, nspin, natomwfc, &
                          nelec, efermi, energy_units, IERR=ierr)

   IF ( ierr/=0 ) CALL errore(subname, "reading dimensions I", ABS(ierr))

   dimwann = natomwfc
   !
   atmproj_nbnd_ = nbnd
   IF ( atmproj_nbnd > 0 ) atmproj_nbnd_ = MIN(atmproj_nbnd, nbnd)

   !Luis 2 begin --> 
   WRITE( stdout, "(2x, ' Dimensions found in atomic_proj.{dat,xml}: ')")
   WRITE( stdout, "(2x, '   nbnd     :  ',i5 )") nbnd
   WRITE( stdout, "(2x, '   nkpts    :  ',i5 )") nkpts
   WRITE( stdout, "(2x, '   nspin    :  ',i5 )") nspin
   WRITE( stdout, "(2x, '   natomwfc :  ',i5 )") natomwfc
   WRITE( stdout, "(2x, '   nelec    :  ',f12.6)") nelec
   WRITE( stdout, "(2x, '   efermi   :  ',f12.6 )") efermi 
   WRITE( stdout, "(2x, '   energy_units :  ',a10 )") energy_units 
   WRITE( stdout, "()" )
   IF ( nspin == 4 ) THEN
      nspin_ = 1
      spin_noncollinear = .true.
   ELSE
      spin_noncollinear = .false.
      nspin_ = nspin
   END IF
   !Luis 2 end   <--

   !
   ! quick report
   !
   WRITE( stdout, "(2x, ' ATMPROJ conversion to be done using: ')")
   WRITE( stdout, "(2x, '   atmproj_nbnd :  ',i5 )") atmproj_nbnd_
   WRITE( stdout, "(2x, '   atmproj_thr  :  ',f12.6 )") atmproj_thr
   WRITE( stdout, "(2x, '   atmproj_sh   :  ',f12.6 )") atmproj_sh
   WRITE( stdout, "()" )

   !
   ! allocations
   !
   ALLOCATE( vkpt(3,nkpts), wk(nkpts), STAT=ierr )
   IF (ierr/=0) CALL errore(subname, 'allocating vkpt, wk', ABS(ierr))
   ALLOCATE( vkpt_cry(3, nkpts), STAT=ierr )
   IF ( ierr/=0 ) CALL errore(subname, 'allocating vkpt_cry', ABS(ierr) )
   !
   !ALLOCATE( eig(nbnd,nkpts,nspin), STAT=ierr )                       !Luis 2
   ALLOCATE( eig(nbnd,nkpts,nspin_), STAT=ierr )                       !Luis 2
   IF (ierr/=0) CALL errore(subname, 'allocating eig', ABS(ierr))
   !ALLOCATE( proj(natomwfc,nbnd,nkpts,nspin), STAT=ierr )             !Luis 2
   ALLOCATE( proj(natomwfc,nbnd,nkpts,nspin_), STAT=ierr )             !Luis 2
   IF (ierr/=0) CALL errore(subname, 'allocating proj', ABS(ierr))

   !
   ! read-in massive data
   !
   IF ( do_orthoovp_ ) THEN
       !
       ALLOCATE( kovp(1,1,1,1), STAT=ierr )
       IF (ierr/=0) CALL errore(subname, 'allocating kovp I', ABS(ierr))
       !
       write(*,*) 'do_orthoovp' !Luis 2
       CALL atmproj_read_ext( filein, VKPT=vkpt, WK=wk, EIG=eig, PROJ=proj, IERR=ierr )
       !
   ELSE
       !
       ALLOCATE( kovp(natomwfc,natomwfc,nkpts,nspin), STAT=ierr )
       IF (ierr/=0) CALL errore(subname, 'allocating kovp II', ABS(ierr))
       !
       ! reading  proj(i,b)  = < phi^at_i | evc_b >
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
   !
   ! if kpts are not MP, try to symmetrize the grid
   !
   IF ( ierr/= 0 ) THEN
       !
       CALL symmetrize_kgrid( nkpts, vkpt, bvec, nkpts_all )
       !
       ALLOCATE( vkpt_all(3,nkpts_all), wk_all(nkpts_all) )
       ALLOCATE( kpteq_map(nkpts_all) )
       ALLOCATE( kpteq_symm(nkpts_all) )
       !
       wk_all(1:nkpts_all)=1.0d0/REAL(nkpts_all,dbl)
       !
       CALL symmetrize_kgrid( nkpts, vkpt, bvec, nkpts_all, vkpt_all, kpteq_map, kpteq_symm )
       !
       CALL get_monkpack( nk, shift, nkpts_all, vkpt_all, 'CARTESIAN', bvec, ierr)
       IF ( ierr/=0 ) CALL errore(subname,'kpt grid not Monkhorst-Pack',ABS(ierr))
       !
   ELSE
       !
       nkpts_all = nkpts
       ALLOCATE( vkpt_all(3,nkpts_all), wk_all(nkpts_all) )
       ALLOCATE( kpteq_map(nkpts_all) )
       ALLOCATE( kpteq_symm(nkpts_all) )
       !
       vkpt_all(:,1:nkpts)  = vkpt(:,1:nkpts)
       wk_all(1:nkpts)      = wk(1:nkpts)
       !
       DO ik = 1, nkpts
          kpteq_map(ik)=ik
          kpteq_symm(ik)=1
       ENDDO
       !
   ENDIF

   !
   ! normalize the k-weights to 1.0
   !
   norm   = SUM( wk )
   wk(:)  = wk(:) / norm
   norm   = SUM( wk_all )
   wk_all(:) = wk_all(:) / norm


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
   ! shifting
   !
   ! apply the energy shift,
   ! meant to set the zero of the energy scale (where we may have
   ! spurious 0 eigenvalues) far from any physical energy region of interest
   !
   eig    = eig  -efermi


   ! 
   ! build the Hamiltonian in real space
   ! 
   IF ( write_ham ) THEN

       !ALLOCATE( rham(dimwann, dimwann, nrtot, nspin), STAT=ierr )    !Luis 2
       ALLOCATE( rham(dimwann, dimwann, nrtot, nspin_), STAT=ierr )    !Luis 2
       IF ( ierr/=0 ) CALL errore(subname, 'allocating rham', ABS(ierr) )
       ALLOCATE( kham(dimwann, dimwann, nkpts_all), STAT=ierr )
       IF ( ierr/=0 ) CALL errore(subname, 'allocating kham', ABS(ierr) )
       !
       IF ( .NOT. do_orthoovp_ ) THEN
           ALLOCATE( rovp(natomwfc,natomwfc,nrtot,nspin), STAT=ierr )
           IF (ierr/=0) CALL errore(subname, 'allocating rovp', ABS(ierr))
       ENDIF
       
#if defined __SHIFT_TEST
       !Luis 3
       IF ( shifting_scheme .EQ. 2 ) THEN
           WRITE( stdout, "(2x, ' ATMPROJ uses the experimental shifting: ')")
           !PA = A'*A
           ALLOCATE( A(natomwfc, atmproj_nbnd_), STAT=ierr )
           IF ( ierr/=0 ) CALL errore(subname, 'allocating Space-A Projector', ABS(ierr))
           ALLOCATE( PA(atmproj_nbnd_, atmproj_nbnd_), STAT=ierr )
           IF ( ierr/=0 ) CALL errore(subname, 'allocating Space-A Projector', ABS(ierr))
           ALLOCATE( IPA(atmproj_nbnd_, atmproj_nbnd_), STAT=ierr )
           IF ( ierr/=0 ) CALL errore(subname, 'allocating inv(P_A)', ABS(ierr))
           ALLOCATE( E(atmproj_nbnd_, atmproj_nbnd_), STAT=ierr )
           IF ( ierr/=0 ) CALL errore(subname, 'allocating E', ABS(ierr))
           ALLOCATE( kham_aux(atmproj_nbnd_, natomwfc), STAT=ierr )
           IF ( ierr/=0 ) CALL errore(subname, 'allocating kham_aux', ABS(ierr))
       ENDIF
#endif
          
       !DO isp = 1, nspin                                              !Luis 2
       DO isp = 1, nspin_                                              !Luis 2

           IF ( TRIM(spin_component) == "up"   .AND. isp == 2 ) CYCLE
           IF ( TRIM(spin_component) == "down" .AND. isp == 1 ) CYCLE
           IF ( TRIM(spin_component) == "dw"   .AND. isp == 1 ) CYCLE


           !
           ! build kham
           !
           kpt_loop:&
           DO ik = 1, nkpts_all
               !
               ikeq = kpteq_map(ik)
               !
               kham(:,:,ik) = ZERO
               !
               ALLOCATE( ztmp(natomwfc,nbnd), STAT=ierr)
               IF ( ierr/=0 ) CALL errore(subname,'allocating ztmp', ABS(ierr) )

               !
               ! ztmp(i, b) = < phi^at_i | evc_b >
               !
               !IF ( kpteq_symm(ik) <= nsym ) THEN
                   ! no time-reversal involved
                   ztmp(1:natomwfc,1:nbnd) = proj(1:natomwfc,1:nbnd,ikeq,isp) 
               !ELSE
               !    ztmp(1:natomwfc,1:nbnd) = CONJG( proj(1:natomwfc,1:nbnd,ikeq,isp) )
               !ENDIF
                !
               CALL atmproj_rotate_proj( natomwfc, nbnd, kpteq_symm(ik), vkpt_cry(:,ikeq), ztmp)
       
#if defined __SHIFT_TEST
               !Luis 3       
               IF ( shifting_scheme .EQ. 2 ) THEN
                   !temporary solution. It assumes the columns of proj are in
                   !ascending order of projectability
                   A  = proj(1:natomwfc,1:atmproj_nbnd_,ik,isp) 
                   PA = ZERO
                   IPA= ZERO
                   !PA = A' * A
                   CALL mat_mul( PA, A, 'C', A, 'N', atmproj_nbnd_, atmproj_nbnd_, natomwfc)
                   CALL mat_inv( atmproj_nbnd_, PA, IPA)
               
                   E  = CZERO 
                   kham_aux = CZERO
                   !TO DO, 
                   !are the eigs in ascending order. where?
                   !bands.x, nscf.x can change the order of the eigenvalues
                   DO ib = 1, atmproj_nbnd_
                      E(ib,ib) = eig(ib,ik,isp) 
                      !E(ib,ib) = eig(ib,ik,isp) - atmproj_sh 
                   ENDDO
                   !HKS_aux = (E - kappa*IPA)*A'
                   CALL mat_mul( kham_aux, E -atmproj_sh*IPA, 'N', A, 'C', atmproj_nbnd_, natomwfc, atmproj_nbnd_)
                   !CALL mat_mul( kham_aux, E, 'N', A, 'C', atmproj_nbnd_, natomwfc, atmproj_nbnd_)
                   !HKS = A*HKS_aux = A*(E - kappa*IPA)*A'
                   CALL mat_mul( kham(:,:,ik), A, 'N', kham_aux, 'N', natomwfc, natomwfc, atmproj_nbnd_)
               
               ELSE
#endif
                  
               !
               ibnd_loop:&
               DO ib = 1, atmproj_nbnd_
                   !
                   ! filtering
                   ! Note: - This is one way of doing the filtering.
                   !       It filters within the atmproj_nbnd bands.
                   !       Useful if used with atmproj_bnd == Inf (i.e. QE's nbnd)
                   !       so that the filter is controlled only by atmproj_thr
                   !
                   !       - Another way is to set atmproj_thr<=0 
                   !       and then the filtering is controlled only by atmproj_nbnd
                   IF ( atmproj_thr > 0.0d0 ) THEN
                       !
                       proj_wgt = REAL( DOT_PRODUCT( ztmp(:,ib), ztmp(:,ib) ) )
                       IF ( proj_wgt < atmproj_thr ) CYCLE ibnd_loop
                       !
                   ENDIF
                   !
                   !
                   DO j = 1, dimwann
                   DO i = 1, dimwann
                       !
                       kham(i,j,ik) = kham(i,j,ik) &
                                        + ztmp(i,ib) * (eig(ib,ikeq,isp)-atmproj_sh) * CONJG( ztmp(j,ib) )
                       !
                   ENDDO
                   ENDDO
                   !
               ENDDO ibnd_loop

#if defined __SHIFT_TEST
               ENDIF
#endif
               !
               IF ( .NOT. mat_is_herm( dimwann, kham(:,:,ik), TOLL=EPS_m8 ) ) &
                   CALL errore(subname,'kham not hermitian',10)
               !
!! XXXX
!ALLOCATE(w(dimwann))
!WRITE(0,*) "DEBUG"
!WRITE(0,*) "ik, ikeq", ik, ikeq
!CALL mat_hdiag( ztmp, w(:), kham(:,:,ik), dimwann)
!DO i = 1, dimwann
!   WRITE(0,"(i5,2f15.9,3x,f15.9)") i, w(i), eig(i,ikeq,isp)-atmproj_sh, ABS(w(i)- eig(i,ikeq,isp)+atmproj_sh)
!ENDDO
!WRITE(0,*)
!WRITE(0,"(8f12.6)") kham(1:4,1:4,ik)
!WRITE(0,*)
!DEALLOCATE(w)

               DEALLOCATE( ztmp, STAT=ierr)
               IF ( ierr/=0 ) CALL errore(subname,'deallocating ztmp', ABS(ierr) )


               !
               ! overlaps
               ! projections are read orthogonal, if non-orthogonality
               ! is required, we multiply by S^1/2
               !
               IF ( .NOT. do_orthoovp_ ) THEN
                   !
                   ALLOCATE( zaux(dimwann,dimwann), ztmp(dimwann,dimwann), STAT=ierr )
                   IF ( ierr/=0 ) CALL errore(subname, 'allocating raux-rtmp', ABS(ierr) )
                   ALLOCATE( w(dimwann), kovp_sq(dimwann,dimwann), STAT=ierr )
                   IF ( ierr/=0 ) CALL errore(subname, 'allocating w, kovp_sq', ABS(ierr) )
                   !
! XXX
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
               ! fermi energy is taken into accout
               ! The energy shift is performed on the final matrix
               ! and not on the DFT eigenvalues            
               ! as     eig(:,:,:) = eig(:,:,:) -efermi
               ! because the atomic basis is typically very large 
               ! and would require a lot of bands to be described
               !
               IF ( .NOT. do_orthoovp_ ) THEN
                   !
                   DO j = 1, dimwann
                   DO i = 1, dimwann
                       kham(i,j,ik) = kham(i,j,ik) + atmproj_sh * kovp(i,j,ik,isp)
                   ENDDO
                   ENDDO
                   !
               ELSE
                   !
                   DO i = 1, dimwann
                       kham(i,i,ik) = kham(i,i,ik) + atmproj_sh
                   ENDDO
                   !
               ENDIF
               !
           ENDDO kpt_loop

#if defined __WRITE_ASCIIHAM
           ! 
           if (isp == 1 .and. nspin_==1) kham_file = "kham.txt"
           if (isp == 1 .and. nspin_==2) kham_file = "kham_up.txt"
           if (isp == 2) kham_file = "kham_down.txt"

           IF (isp ==1) THEN !
              OPEN (unit = 14, file = "k.txt")
              DO ik =1, nkpts
                 WRITE(14,"(3f20.13)") vkpt_cry(1,ik), vkpt_cry(2,ik), vkpt_cry(3,ik)
              ENDDO
              CLOSE(14)

              OPEN (unit = 14, file = "wk.txt")
              DO ik =1, nkpts
                 WRITE(14,"(f20.13)") wk(ik)
              ENDDO
              CLOSE(14)

              OPEN (unit = 14, file = "kovp.txt")
              DO ik =1, nkpts
                  DO iw=1,dimwann
                     DO jw=1,dimwann
                        WRITE(14,"(2f20.13)") real(kovp(iw,jw,ik,isp)),aimag(kovp(iw,jw,ik,isp))
                     ENDDO
                  ENDDO
              ENDDO
              CLOSE(14)
           ENDIF

           OPEN (unit = 14, file = trim(kham_file))
           DO ik =1, nkpts
               DO iw=1,dimwann
                  DO jw=1,dimwann
                     WRITE(14,"(2f20.13)") real(kham(iw,jw,ik)),aimag(kham(iw,jw,ik))
                  ENDDO
               ENDDO
           ENDDO
           CLOSE(14)
#endif


           ! 
           ! convert to real space
           !
           DO ir = 1, nrtot
               !
               CALL compute_rham( dimwann, vr(:,ir), rham(:,:,ir,isp), &
                                  nkpts_all, vkpt_all, wk_all, kham )
               !
               IF ( .NOT. do_orthoovp_ ) THEN
                   !
                    CALL compute_rham( dimwann, vr(:,ir), rovp(:,:,ir,isp), &
                                       nkpts_all, vkpt_all, wk_all, kovp(:,:,:,isp) )
                   !
               ENDIF
               !
           ENDDO
           !
       ENDDO 
 
#if defined __SHIFT_TEST
       !Luis 3
       IF ( shifting_scheme .EQ. 2 ) THEN
           DEALLOCATE( A )
           DEALLOCATE( PA )
           DEALLOCATE( IPA )
           DEALLOCATE( E )
           DEALLOCATE( kham_aux )
       ENDIF
#endif

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
   DEALLOCATE( vkpt_all, kpteq_map, kpteq_symm, STAT=ierr)
   IF ( ierr/=0 ) CALL errore(subname, 'deallocating vkpt_all, kpteq_map, kpteq_symm', ABS(ierr) )


!
!---------------------------------
! write to fileout (internal fmt)
!---------------------------------
!
   IF ( write_ham ) THEN
       !
       CALL iotk_open_write( ounit, FILE=TRIM(fileham), BINARY=binary )
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
          IF ( TRIM(spin_component) == "up"   .AND. isp == 2 ) CYCLE
          IF ( TRIM(spin_component) == "down" .AND. isp == 1 ) CYCLE
          IF ( TRIM(spin_component) == "dw"   .AND. isp == 1 ) CYCLE
          !
          IF ( TRIM(spin_component) == "all" .AND. nspin == 2 ) THEN
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
       CALL iotk_open_write( ounit, FILE=TRIM(filespace), BINARY=binary )
       !
       !
       CALL iotk_write_begin( ounit, "WINDOWS" )
       !
       !
       CALL iotk_write_attr( attr,"nbnd",atmproj_nbnd_,FIRST=.TRUE.)
       CALL iotk_write_attr( attr,"nkpts",nkpts)
       CALL iotk_write_attr( attr,"nspin",nspin)
       CALL iotk_write_attr( attr,"spin_component",TRIM(spin_component))
       CALL iotk_write_attr( attr,"efermi", 0.0_dbl )
       CALL iotk_write_attr( attr,"dimwinx", atmproj_nbnd_ )
       CALL iotk_write_empty( ounit,"DATA",ATTR=attr)
       !
       ALLOCATE( itmp(nkpts), STAT=ierr )
       IF ( ierr/=0 ) CALL errore(subname, 'allocating itmp', ABS(ierr))
       ALLOCATE( eamp_tmp(atmproj_nbnd_,dimwann), STAT=ierr )
       IF ( ierr/=0 ) CALL errore(subname, 'allocating eamp_tmp', ABS(ierr))
       !
       itmp(:) = atmproj_nbnd_
       CALL iotk_write_dat( ounit, "DIMWIN", itmp, COLUMNS=8 )
       itmp(:) = 1
       CALL iotk_write_dat( ounit, "IMIN", itmp, COLUMNS=8 )
       itmp(:) = atmproj_nbnd_
       CALL iotk_write_dat( ounit, "IMAX", itmp, COLUMNS=8 )
       !
       DO isp = 1, nspin
           !
           IF ( nspin == 2 ) THEN
               CALL iotk_write_begin( ounit, "SPIN"//TRIM(iotk_index(isp)) )
           ENDIF
           !
           CALL iotk_write_dat( ounit, "EIG", eig(1:atmproj_nbnd_,:,isp), COLUMNS=4)
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
       CALL iotk_write_attr( attr,"dimwinx",atmproj_nbnd_,FIRST=.TRUE.)
       CALL iotk_write_attr( attr,"nkpts",nkpts)
       CALL iotk_write_attr( attr,"dimwann", dimwann)
       CALL iotk_write_empty( ounit,"DATA",ATTR=attr)
       !
       itmp(:) = atmproj_nbnd_
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
               eamp_tmp(1:atmproj_nbnd_, 1:dimwann) = &
                        CONJG(TRANSPOSE(proj(1:dimwann,1:atmproj_nbnd_,ik,isp) )) 
               !
               DO ib = 1, atmproj_nbnd_
                   proj_wgt = REAL( DOT_PRODUCT( proj(:,ib,ik,isp ), proj(:,ib,ik,isp ) ) )
                   IF ( proj_wgt < atmproj_thr ) eamp_tmp( ib, :) = 0.0d0
               ENDDO
               !
               CALL iotk_write_dat( ounit, "EAMP"//TRIM(iotk_index(ik)), eamp_tmp )
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
       DEALLOCATE( itmp, eamp_tmp, STAT=ierr )
       IF ( ierr/=0 ) CALL errore(subname, 'deallocating itmp, eamp', ABS(ierr))
       !
   ENDIF


   IF ( write_loc ) THEN
       !
       CALL iotk_open_write( ounit, FILE=TRIM(filewan), BINARY=binary )
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
   CALL ions_deallocate()
   CALL symmetry_deallocate()

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
   !
   COMPLEX(dbl), ALLOCATABLE :: ztmp(:,:)


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

   !Luis 2 begin -->
   IF ( nspin_ == 4 ) THEN
      nspin_ = 1
   END IF
   !Luis 2 end   <--

   write(*,*) 'about to read eigenvalues' !Luis 2
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
       ALLOCATE( ztmp(nbnd_, natomwfc_) )
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
                   CALL iotk_scan_dat(iunit, TRIM(str) , ztmp( :, ias ), IERR=ierr)
                   IF ( ierr /= 0 ) RETURN
                   !
               ENDDO
               !
               proj( 1:natomwfc_, 1:nbnd_, ik, isp ) = TRANSPOSE( ztmp(1:nbnd_,1:natomwfc_) ) 
               !
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
           !
       ENDDO
       !
       DEALLOCATE( ztmp )
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
       IF ( ierr/=0 ) THEN
           WRITE(stdout,"(/,'OVERLAPS data not found in file. Crashing ...')")
           RETURN
       ENDIF
       !
       DO ik = 1, nkpt_
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


!************************************************************
INTEGER FUNCTION atmproj_get_index( i, ia, ityp, natomwfc )
   !************************************************************
   !
   IMPLICIT NONE
   !
   INTEGER       :: i, ia, ityp(*), natomwfc(*)
   !
   INTEGER       :: ind, iatm, nt
   CHARACTER(17) :: subname="atmproj_get_index"
   !
   IF ( i > natomwfc( ityp(ia)) ) CALL errore(subname,"invalid i",i)
   !
   ind = i
   DO iatm = 1, ia-1
       !
       nt = ityp(iatm)
       ind = ind + natomwfc(nt)
       !
   ENDDO
   !
   atmproj_get_index = ind
   !
END FUNCTION atmproj_get_index


!************************************************************
SUBROUTINE  atmproj_get_natomwfc( nsp, psfile, natomwfc, itypl )
   !************************************************************
   !
   IMPLICIT NONE
   !
   INTEGER,           INTENT(IN)  :: nsp
   TYPE(pseudo_upf),  INTENT(IN)  :: psfile(nsp)
   INTEGER,           INTENT(OUT) :: natomwfc(nsp)
   INTEGER, OPTIONAL, INTENT(OUT) :: itypl(nwfcx,nsp)
   !
   CHARACTER(20) :: subname="atmproj_get_natomwfc"
   INTEGER :: nt, nb, il
   !
   DO nt = 1, nsp
       !
       !IF (PRESENT(indxl)) indxl(0:3,nt) = 0
       IF (PRESENT(itypl)) itypl(:,nsp) = -1
       natomwfc(nt)  = 0
       !
       DO nb = 1, psfile(nt)%nwfc
           il = psfile(nt)%lchi(nb)
           IF ( il > 3 ) CALL errore(subname,"invalid il",il)
           IF ( psfile(nt)%oc(nb) >= 0.0d0 ) THEN
               !IF (PRESENT(indxl)) indxl(il,nt) = natomwfc(nt) + 1 
               IF (PRESENT(itypl)) itypl(natomwfc(nt)+1:natomwfc(nt)+2*il+1,nt) = il
               natomwfc(nt) = natomwfc(nt) + 2 * il + 1
           ENDIF
       ENDDO
       !
   ENDDO
   !
END SUBROUTINE atmproj_get_natomwfc


!************************************************************
SUBROUTINE  atmproj_rotate_proj( natomwfc, nbnd, isym, vkpt_c, proj)
   !************************************************************
   !
   USE symmetry_module, ONLY : d1, d2, d3, srot, strasl, nsym, &
                               irt, icell, symm_alloc => alloc
   USE ions_module,     ONLY : nat, ityp, nsp, ions_alloc => alloc
   !
   IMPLICIT NONE
   !
   INTEGER,      INTENT(IN)    :: natomwfc, nbnd, isym
   REAL(dbl),    INTENT(IN)    :: vkpt_c(3)     ! crystal units are expected
   COMPLEX(dbl), INTENT(INOUT) :: proj(natomwfc,nbnd)
   !
   CHARACTER(19) :: subname="atmproj_rotate_proj"
   INTEGER       :: ia, iaeq, nt, ierr
   INTEGER       :: isym_, il, is, iseq, isn, ien, n
   LOGICAL       :: use_trev
   REAL(dbl)     :: rvkpt_c(3), arg
   COMPLEX(dbl)  :: c1(3,3), c2(5,5), c3(7,7), phase
   !
   INTEGER,      ALLOCATABLE :: iatom_map(:), itypl(:,:)
   INTEGER,      ALLOCATABLE :: natomwfc_sp(:)
   INTEGER,      ALLOCATABLE :: check(:)
   COMPLEX(dbl), ALLOCATABLE :: proj0(:,:)


   !
   ! if we deal with the identity, nothing to do
   IF ( isym == 1 ) RETURN
   !
   IF ( .NOT. symm_alloc ) CALL errore(subname,"symmetry module not alloc",10)
   IF ( .NOT. ions_alloc ) CALL errore(subname,"ions module not alloc",10)

   !
   ! whether to use time-reversal
   !
   isym_ = isym
   use_trev = .FALSE.
   !
   IF ( isym_ > nsym ) THEN
       isym_=isym_-nsym
       use_trev = .TRUE.
   ENDIF
   !
   IF ( isym_ <= 0 .OR. isym_ > nsym ) CALL errore(subname,"invalid isym index",10)
   !
   ! local workspace
   !
   ALLOCATE( iatom_map(nat), STAT=ierr )
   IF (ierr/=0) CALL errore(subname,"allocating iatom_map",10)
   ALLOCATE( proj0(natomwfc,nbnd), STAT=ierr )
   IF (ierr/=0) CALL errore(subname,"allocating proj0",10)
   ALLOCATE( natomwfc_sp(nsp), STAT=ierr )
   IF (ierr/=0) CALL errore(subname,"allocating natomwfc_sp",10)
   ALLOCATE( itypl(nwfcx,nsp), STAT=ierr )
   IF (ierr/=0) CALL errore(subname,"allocating itypl",10)
   ALLOCATE(check(natomwfc), STAT=ierr)
   IF (ierr/=0) CALL errore(subname,"allocating check",10)

   !
   ! build iatom_map
   !
   CALL atmproj_get_natomwfc( nsp, upf(1:nsp), natomwfc_sp, itypl )
   !
   iatom_map(1)=1
   !
   DO ia = 2, nat
       !
       nt=ityp(ia-1)
       iatom_map(ia) = iatom_map(ia-1) + natomwfc_sp(nt) 
       !
   ENDDO
   !
   !CALL mat_mul( rvkpt_c, REAL(srot(:,:,isym_),dbl), "T", vkpt_c, 3, 3)
   rvkpt_c = vkpt_c

   !
   ! main loop
   !
   proj0 = proj
   c1    = d1(:,:,isym_) 
   c2    = d2(:,:,isym_) 
   c3    = d3(:,:,isym_) 
   !
   check(:) = 0
   !
   DO ia = 1, nat
       !
       iaeq = irt(isym_,ia)
       nt   = ityp(ia)
       !
       is   = iatom_map(ia)
       iseq = iatom_map(iaeq)
       !
       isn  = 0
       ien  = 0
       !
       DO n = 1, upf(nt)%nwfc
           !
           isn = ien+1 
           il  = itypl(isn,nt)
           ien = isn+(2*il+1) -1
           !
           SELECT CASE ( il)
           CASE ( 0 )    
              !
              proj(is+isn-1,:) = proj0(iseq+isn-1,:)
              check(is+isn-1)  = check(is+isn-1)+1
              !
           CASE ( 1 )
              !
              CALL mat_mul( proj(is+isn-1:is+isn+1,:), c1, "N", &
                            proj0(iseq+isn-1:iseq+isn+1,:), "N", 3, nbnd, 3)
              check(is+isn-1:is+isn+1) = check(is+isn-1:is+isn+1)+1
              !
           CASE ( 2 )
              !
              CALL mat_mul( proj(is+isn-1:is+isn+3,:), c2, "N", &
                            proj0(iseq+isn-1:iseq+isn+3,:), "N", 5, nbnd, 5)
              check(is+isn-1:is+isn+3)  = check(is+isn-1:is+isn+3)+1
              !
           CASE ( 3 )
              !
              CALL mat_mul( proj(is+isn-1:is+isn+5,:), c3, "N", &
                            proj0(iseq+isn-1:iseq+isn+5,:), "N", 7, nbnd, 7)
              check(is+isn-1:is+isn+5)  = check(is+isn-1:is+isn+5)+1
              !
           CASE DEFAULT
               CALL errore(subname,"invalid il",ABS(il))
           END SELECT
           !
       ENDDO
       !
       ! add a phase in case atom iia is shifted into a different cell
       !
       arg   = TPI * DOT_PRODUCT( rvkpt_c, REAL(icell(:,isym_,ia)) )
       !
       IF ( .NOT. use_trev ) THEN
           phase = CMPLX( COS(arg),  SIN(arg), dbl )
       ELSE
           phase = CMPLX( COS(arg), -SIN(arg), dbl )
       ENDIF
       !
       proj(is:is+natomwfc_sp(nt)-1,:) = proj(is:is+natomwfc_sp(nt)-1,:) * phase
       !
   ENDDO
   !
   IF ( ANY(check(:)== 0 ) ) CALL errore(subname,"unexpected error in atmwfc mapping",10)
   
   !
   ! cleanup
   !
   DEALLOCATE( iatom_map, proj0, STAT=ierr) 
   IF ( ierr/=0 ) CALL errore(subname,"deallocating iatom_map, proj0",ABS(ierr))
   DEALLOCATE( natomwfc_sp, STAT=ierr) 
   IF ( ierr/=0 ) CALL errore(subname,"deallocating natomwfc_sp",ABS(ierr))
   DEALLOCATE( itypl, STAT=ierr) 
   IF ( ierr/=0 ) CALL errore(subname,"deallocating itypl",ABS(ierr))
   DEALLOCATE( check, STAT=ierr) 
   IF ( ierr/=0 ) CALL errore(subname,"deallocating check",ABS(ierr))
   !
   RETURN
   !
END SUBROUTINE atmproj_rotate_proj


END MODULE atmproj_tools_module
