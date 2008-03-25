!
! Copyright (C) 2008 WanT Group
!
! This file is distributed under the terms of the
! GNU General Public License. See the file `License\'
! in the root directory of the present distribution,
! or http://www.gnu.org/copyleft/gpl.txt .
!
!**********************************************************
   SUBROUTINE crystal_to_internal( filein, fileout )
   !**********************************************************
   !
   ! Convert datafiles written by the CRYSTAL06 program to
   ! the internal representation
   !
   USE kinds,              ONLY : dbl
   USE constants,          ONLY : BOHR => bohr_radius_angs, ZERO, ONE, TWO, RYD
   USE parameters,         ONLY : nstrx
   USE io_module,          ONLY : ham_unit, aux_unit
   USE timing_module,      ONLY : timing
   USE log_module,         ONLY : log_push, log_pop
   USE converters_module,  ONLY : cart2cry
   USE parser_module,      ONLY : change_case
   USE crystal_io_module
   USE iotk_module

   !
   IMPLICIT NONE

   LOGICAL, PARAMETER :: binary = .FALSE.
   
   !
   ! input variables
   !
   CHARACTER(*), INTENT(IN) :: filein
   CHARACTER(*), INTENT(IN) :: fileout
   
   !
   ! local variables
   !
   CHARACTER(19)               :: subname="crystal_to_internal"
   !
   CHARACTER(nstrx)            :: attr, r_units, a_units, b_units, k_units, h_units
   INTEGER                     :: dimwann, nkpts, nk(3), shift(3), nrtot, nr(3) 
   INTEGER                     :: auxdim1, auxdim2
   INTEGER                     :: ierr, ir
   !
   REAL(dbl)                   :: dlatt(3,3), rlatt(3,3), norm
   INTEGER,        ALLOCATABLE :: ivr(:,:)
   REAL(dbl),      ALLOCATABLE :: vkpt_cry(:,:), wk(:), wr(:), vr(:,:)
!   REAL(dbl),      ALLOCATABLE :: wan_eig(:,:)
   REAL(dbl),      ALLOCATABLE :: rham(:,:,:), rovp(:,:,:)

!
!------------------------------
! main body
!------------------------------
!
   CALL timing( subname, OPR='start' )
   CALL log_push( subname )

!
!---------------------------------
! read from filein (CRYSTAL06 fmt)
!---------------------------------
!
   CALL crio_open_file( UNIT=aux_unit, FILENAME=filein, ACTION='read', IERR=ierr )
   IF ( ierr/=0 ) CALL errore(subname, 'opening'//TRIM(filein), ABS(ierr) )
   !
   !
   CALL crio_open_section( "GEOMETRY", ACTION='read', IERR=ierr )
   IF ( ierr/=0 ) CALL errore(subname, 'opening sec. GEOMETRY', ABS(ierr) )
   !
   CALL crio_read_cell( AVEC=dlatt, UNITS=a_units, IERR=ierr)
   IF ( ierr/=0 ) CALL errore(subname, 'reading direct lattice', ABS(ierr) )
   !
   CALL crio_read_cell_reciprocal( BVEC=rlatt, UNITS=b_units, IERR=ierr)
   IF ( ierr/=0 ) CALL errore(subname, 'reading reciprocal lattice', ABS(ierr) )
   ! 
   ! 
   CALL crio_close_section( "GEOMETRY", ACTION='read', IERR=ierr )
   IF ( ierr/=0 ) CALL errore(subname, 'closing sec. GEOMETRY', ABS(ierr) )
   !
   !
   ! convert units if the case
   !
   CALL change_case( a_units, 'lower' )
   CALL change_case( b_units, 'lower' )
   !
   !
   SELECT CASE( ADJUSTL(TRIM(a_units)) )
   CASE ( "b", "bohr", "au" )
      !
      ! do nothing
   CASE ( "ang", "angstrom" )
      !
      dlatt = dlatt / BOHR
      !
   CASE DEFAULT
      CALL errore( subname, 'unknown units for A: '//TRIM(a_units), 71)
   END SELECT
   !
   !
   SELECT CASE( ADJUSTL(TRIM(b_units)) )
   CASE ( "bohr^-1", "bohr-1", "au" )
      !
      ! do nothing
   CASE ( "ang-1", "angstrom-1", "ang^-1", "angstrom^-1" )
      !
      rlatt = rlatt * BOHR
      !
   CASE DEFAULT
      CALL errore( subname, 'unknown units for B: '//TRIM(b_units), 71)
   END SELECT

   !
   ! enter section METHOD
   !
   CALL crio_open_section( "METHOD", ACTION='read', IERR=ierr )
   IF ( ierr/=0 ) CALL errore(subname, 'opening sec. METHOD', ABS(ierr) )

   !
   ! real-space lattice vectors
   !
   !
   CALL crio_read_direct_lattice( NRTOT=nrtot, IERR=ierr )
   IF ( ierr/=0 ) CALL errore(subname, 'reading direct lattice dims', ABS(ierr) )
   !
   ALLOCATE( ivr(3, nrtot), vr(3, nrtot), STAT=ierr )
   IF ( ierr/=0 ) CALL errore(subname, 'allocating ivr, vr', ABS(ierr) )
   !
   ALLOCATE( wr(nrtot), STAT=ierr )
   IF ( ierr/=0 ) CALL errore(subname, 'allocating wr', ABS(ierr) )
   !
   CALL crio_read_direct_lattice( RVEC=vr, R_UNITS=r_units, IERR=ierr )
   IF ( ierr/=0 ) CALL errore(subname, 'reading RVEC', ABS(ierr) )

   !
   ! units
   !
   CALL change_case( r_units, 'lower' )
   !
   SELECT CASE( ADJUSTL(TRIM(r_units)) )
   CASE ( "b", "bohr", "au" )
      !
      CALL cart2cry( vr, dlatt )
      !
   CASE ( "ang", "angstrom" )
      !
      vr = vr / BOHR
      CALL cart2cry( vr, dlatt )
      !
   CASE ( "cry", "crystal", "relative" )
      !
      ! do nothing
   CASE DEFAULT
      CALL errore( subname, 'unknown units for R: '//TRIM(r_units), 71)
   END SELECT
 
   !
   ! define IVR
   !
   DO ir = 1, nrtot
      !
      ivr(:, ir ) = NINT( vr(:, ir) )
      !
   ENDDO

   !
   ! nr should be got from ivr
   !
   nr ( 1 ) = MAXVAL( ivr(1,:) ) - MINVAL( ivr(1,:) ) +1
   nr ( 2 ) = MAXVAL( ivr(2,:) ) - MINVAL( ivr(2,:) ) +1
   nr ( 3 ) = MAXVAL( ivr(3,:) ) - MINVAL( ivr(3,:) ) +1

   !
   ! as a default, set wr to 1.0; to be checked
   !
   wr (1:nrtot) = ONE


   !
   ! read bz information
   !
   CALL crio_read_bz( NUM_K_POINTS=nkpts, NK=nk, K_UNITS=k_units, IERR=ierr )
   IF ( ierr/=0 ) CALL errore(subname, 'reading bz dimensions', ABS(ierr) )
   !
   ! crystal does not allow for shifted k-meshes
   !
   shift(1:3) = 0

   !
   ! maybe we need some weird initialization
   ! about nkpts
   !
   
   ALLOCATE( vkpt_cry(3, nkpts), STAT=ierr )
   IF ( ierr/=0 ) CALL errore(subname, 'allocating vkpt_cry', ABS(ierr) )
   !
   ALLOCATE( wk(nkpts), STAT=ierr )
   IF ( ierr/=0 ) CALL errore(subname, 'allocating wk', ABS(ierr) )

   CALL crio_read_bz( XK=vkpt_cry, WK=wk, IERR=ierr )
   IF ( ierr/=0 ) CALL errore(subname, 'reading bz dimensions', ABS(ierr) )

   !
   ! units (we want kpt to be re-written in crystal units)
   !
   CALL change_case( k_units, 'lower' )
   !
   SELECT CASE( ADJUSTL(TRIM(k_units)) )
   CASE ( "bohr^-1", "bohr-1", "au" )
      !
      CALL cart2cry( vkpt_cry, rlatt ) 
      !
   CASE ( "ang-1", "angstrom-1", "ang^-1", "angstrom^-1" )
      !
      vkpt_cry = vkpt_cry * BOHR
      CALL cart2cry( vkpt_cry, rlatt ) 
      !
   CASE ( "cry", "crystal", "relative", "lattice" )
      !
      ! do nothing
   CASE DEFAULT
      CALL errore( subname, 'unknown units for kpts: '//TRIM(k_units), 71)
   END SELECT

   !
   ! check the normalization of the weights
   !
   norm   = SUM( wk )
   wk(:)  = wk(:) / norm

   !
   ! exit section METHOD, enter section OUTPUT_DATA
   !
   CALL crio_close_section( "METHOD", ACTION='read', IERR=ierr )
   IF ( ierr/=0 ) CALL errore(subname, 'closing sec. METHOD', ABS(ierr) )
   !
   CALL crio_open_section( "OUTPUT_DATA", ACTION='read', IERR=ierr )
   IF ( ierr/=0 ) CALL errore(subname, 'opening sec. OUTPUT_DATA', ABS(ierr) )
   

   !
   ! read Overlaps
   !
   CALL crio_read_overlap( DIM_BASIS=dimwann, NRTOT=auxdim2, IERR=ierr)
   IF ( ierr/=0 ) CALL errore(subname, 'reading ovp dimensions', ABS(ierr) )
   !
   IF ( auxdim2 /= nrtot )   CALL errore(subname, 'inconsistent dimensions', 72)
   !
   ALLOCATE( rovp(dimwann, dimwann, nrtot), STAT=ierr )
   IF ( ierr/=0 ) CALL errore(subname, 'allocating rovp', ABS(ierr) )
   ! 
   CALL crio_read_overlap( OVP_MATRIX=rovp, IERR=ierr )
   IF ( ierr/=0 ) CALL errore(subname, 'reading ovp matrix', ABS(ierr) )


   !
   ! read Hamiltonian 
   !
   CALL crio_read_hamiltonian( UNITS=h_units, DIM_BASIS=auxdim1, &
                               NRTOT=auxdim2, IERR=ierr)
   IF ( ierr/=0 ) CALL errore(subname, 'reading ham dimensions', ABS(ierr) )
   IF ( auxdim1 /= dimwann ) CALL errore(subname, 'inconsistent dimensions', 74)
   IF ( auxdim2 /= nrtot )   CALL errore(subname, 'inconsistent dimensions', 73)
   !
   !
   ALLOCATE( rham(dimwann, dimwann, nrtot), STAT=ierr )
   IF ( ierr/=0 ) CALL errore(subname, 'allocating rham', ABS(ierr) )
   !
   CALL crio_read_hamiltonian( HAM_MATRIX=rham, IERR=ierr )
   IF ( ierr/=0 ) CALL errore(subname, 'reading ham matrix', ABS(ierr) )
   !
   ! units are converted to eV's
   !
   CALL change_case( h_units, 'lower' )
   !
   SELECT CASE( ADJUSTL(TRIM(h_units)) )
   CASE ( "ha", "hartree" )
      !
      rham(:,:,:) = rham (:,:,:) * TWO * RYD 
      !
   CASE ( "ry", "ryd", "rydberg" )
      !
      rham(:,:,:) = rham (:,:,:) * RYD 
      !
   CASE ( "ev", "electronvolt" )
      !
      ! do nothing
   CASE DEFAULT
      CALL errore( subname, 'unknown units for ham: '//TRIM(h_units), 71)
   END SELECT


   !
   ! exit sec. OUTPUT_DATA and close the file
   !
   CALL crio_close_section( "OUTPUT_DATA", ACTION='read', IERR=ierr )
   IF ( ierr/=0 ) CALL errore(subname, 'closing sec. OUTPUT_DATA', ABS(ierr) )
   !
   CALL crio_close_file( ACTION='read', IERR=ierr )
   IF ( ierr/=0 ) CALL errore(subname, 'closing'//TRIM(filein), ABS(ierr))

!
!---------------------------------
! write to fileout (internal fmt)
!---------------------------------
!
   CALL iotk_open_write( ham_unit, FILE=TRIM(fileout), BINARY=binary )
   CALL iotk_write_begin( ham_unit, "HAMILTONIAN" )
   !
   !
   CALL iotk_write_attr( attr,"dimwann",dimwann,FIRST=.TRUE.)
   CALL iotk_write_attr( attr,"nkpts",nkpts)
   CALL iotk_write_attr( attr,"nk",nk)
   CALL iotk_write_attr( attr,"shift",shift)
   CALL iotk_write_attr( attr,"nrtot",nrtot)
   CALL iotk_write_attr( attr,"nr",nr)
   CALL iotk_write_attr( attr,"have_overlap", .TRUE. )
   CALL iotk_write_empty( ham_unit,"DATA",ATTR=attr)
   !
   CALL iotk_write_attr( attr,"units","bohr",FIRST=.TRUE.)
   CALL iotk_write_dat( ham_unit,"DIRECT_LATTICE", dlatt, ATTR=attr, COLUMNS=3)
   !
   CALL iotk_write_attr( attr,"units","bohr^-1",FIRST=.TRUE.)
   CALL iotk_write_dat( ham_unit,"RECIPROCAL_LATTICE", rlatt, ATTR=attr, COLUMNS=3)
   !
   CALL iotk_write_attr( attr,"units","crystal",FIRST=.TRUE.)
   CALL iotk_write_dat( ham_unit,"VKPT", vkpt_cry, ATTR=attr, COLUMNS=3)
   CALL iotk_write_dat( ham_unit,"WK", wk)
   !
   CALL iotk_write_dat( ham_unit,"IVR", ivr, ATTR=attr, COLUMNS=3)
   CALL iotk_write_dat( ham_unit,"WR", wr)
   !
   !
   ! Eigenvalues are not read, and are temporarily not written
   ! when converting from Crystal
   !
   ! CALL iotk_write_dat( ham_unit,"WAN_EIGENVALUES", wan_eig)
   !
   CALL iotk_write_begin( ham_unit,"RHAM")
   !
   DO ir = 1, nrtot
       !
       CALL iotk_write_dat( ham_unit,"VR"//TRIM(iotk_index(ir)), &
                            CMPLX(rham(:,:,ir), ZERO, dbl) )
       CALL iotk_write_dat( ham_unit,"OVERLAP"//TRIM(iotk_index(ir)), &
                            CMPLX(rovp(:,:,ir), ZERO, dbl) )
       !
   ENDDO
   !
   CALL iotk_write_end( ham_unit,"RHAM")
   !
   !
   CALL iotk_write_end( ham_unit, "HAMILTONIAN" )
   CALL iotk_close_write( ham_unit )

!
! local cleaning
!

   DEALLOCATE( vkpt_cry, wk, STAT=ierr )
   IF ( ierr/=0 ) CALL errore(subname, 'deallocating vkpt_cry, wk', ABS(ierr) )
   !
   DEALLOCATE( ivr, wr, STAT=ierr )
   IF ( ierr/=0 ) CALL errore(subname, 'deallocating ivr, wr', ABS(ierr) )
   !
   DEALLOCATE( rham, STAT=ierr )
   IF ( ierr/=0 ) CALL errore(subname, 'deallocating rham', ABS(ierr) )
   

   CALL log_pop( subname )
   CALL timing( subname, OPR='stop' )
   !
   RETURN
   !
END SUBROUTINE crystal_to_internal

