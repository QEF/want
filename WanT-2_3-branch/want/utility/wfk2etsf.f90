!
! Copyright (C) 2009 Tonatiuh Rangel
!
! This file is distributed under the terms of the
! GNU General Public License. See the file `License'
! in the root directory of the present distribution,
! or http://www.gnu.org/copyleft/gpl.txt .
!
!****************************
program wfk2etsf
!****************************
#ifdef __ETSF_IO
 use etsf_io
 use etsf_io_tools
 use atomic_module, only : atomic_num2name

 implicit none
 integer, parameter :: dp=kind(1.0d0)
 real(dp), PARAMETER :: HaeV=27.2113961d0
 character(len=1), parameter :: ch10 = char(10)
 character(len=80) :: wffile,msj,sufix,outfile
 integer::iost
 integer :: i, ncid
!
!etsf variables
 logical                 :: lstat
 type(etsf_io_low_error) :: error_data

 ! Specific variables required by etsf
 type(etsf_groups_flags) :: flags
 type(etsf_dims)         :: dims
 type(etsf_kpoints)      :: kpoints
 type(etsf_basisdata)    :: basisdata
 type(etsf_main)         :: main
 type(etsf_electrons)    :: electrons
 type(etsf_geometry)     :: geometry

 ! Variables that are declared in the main program in a real case
 real(dp), allocatable, target :: coef_pw_k(:, :)

 ! Variables that will be used in the basisdata group.
 real(dp), target                       :: kinetic_energy_cutoff
 integer, allocatable, target          :: number_of_coefficients(:)
 integer, allocatable, target          :: red_coord_pw_k(:, :)

 ! Variables that will be used in the kpoints group.
 real(dp), allocatable, target :: red_coord_kpt(:, :)
 real(dp), allocatable, target :: kpoint_weights(:)

 ! Variable to store the definition of the basis set
 character(len = etsf_charlen), target :: basis

 ! Variables for the electrons group
  character(len=etsf_charlen),   target :: smearing_scheme
  real(dp), allocatable,target :: eigenvalues(:,:,:), occupations(:,:,:)
  integer, allocatable, target :: number_of_states(:,:)
  real(dp),             target :: smearing_width, fermi_energy

 ! Variables for the geometry group
  real(dp), target :: primitive_vectors(3,3)
  integer,          allocatable, target :: reduced_symmetry_matrices(:,:,:)
  real(dp), allocatable, target :: reduced_symmetry_translations(:,:)
  integer,          allocatable, target :: atom_species(:)
  real(dp), allocatable, target :: reduced_atom_positions(:,:)
!  real(dp), allocatable, target :: valence_charges(:)
  real(dp), allocatable, target :: atomic_numbers(:)
  character(len=etsf_chemlen), allocatable, target :: chemical_symbols(:)
  character(len=etsf_charlen), allocatable, target :: atom_species_names(:)
!  character(len=etsf_charlen), allocatable, target :: pseudopotential_types(:)



! Variables to read _WFK file
 character*6 :: codvsn
 character*132 :: title
 integer:: ipsp,lmn_size,isppol,ii,ij,iband,ibandk,icg,ik,iatom,jj,kk
 integer :: headform,fform
 integer :: bantot,date,intxc,ixc,ngfft(3),npsp
 integer :: occopt,pertcase,usepaw
 integer :: pspso,pspdat,pspcod,pspxc,lmax,lloc
 integer :: nbandk
 integer :: npw
 real(dp) :: acell(3),ecutdg,ecutsm,ecut_eff
 real(dp) :: kpttmp(3),qptn(3),stmbias,tphysel
 real(dp) :: residm,etotal
 real(dp) :: znuclpsp,zionpsp
 integer,allocatable::istwfk(:),nband(:),so_psp(:),rhoijselect(:,:)
 integer,allocatable:: symafm(:),nrhoijsel(:)
 real(dp),allocatable ::occ(:)
 real(dp),allocatable::occk(:),cg(:,:),eigen(:),eigentmp(:)
 !

! !!!!!! Initizalizing !!!!!!!!!!!!!!!!!!!!!!!!!!!!!

 CALL checkCommandLineInputs(wffile)
 sufix='-etsf.nc'
 outfile=trim(wffile)//trim(sufix)     !output file name

 OPEN(UNIT=1, FILE=wffile, iostat=iost, STATUS="old", FORM="UNFORMATTED")
 IF (iost .NE. 0) call errore('wfk2etsf','Error opening file'//trim(wffile),1)

!Read header
!Start reading wavefunction
 call readheader()

!Allocate basic quantities
 call allocate_abi()

!Read wavefunction basic quantities
!as nband,atoms information, symmetries, k-points and pseudopotentials.
 call read_wf_basic()

!Extract ETSF variables
 call extract_etsf_variables()

!Write out information to show it to the user
 call show_etsf_variables() 

!Initialize etsf arrays. (flags)
 call init_etsf()

!Read pseudopotentials info
 do ipsp=1,npsp
! (npsp lines, 1 for each pseudopotential ; npsp=dims%number_of_atom_species, except if alchemical pseudo-atoms)
  read(1) title,znuclpsp,zionpsp,pspso,pspdat,pspcod,pspxc,lmn_size
 enddo

!(in case of usepaw==0, final record: residm, coordinates, total energy, Fermi energy)
 read(1) residm,reduced_atom_positions(1:3,1:dims%number_of_atoms),etotal,fermi_energy

!Deallocate some unused quantities
 DEALLOCATE( istwfk, nband )
 DEALLOCATE( so_psp, symafm)
 DEALLOCATE( occ)


!Read eigenvalues, occupations, G vectors and wavefunction coefficients
!in a big kpt_loop
!Write this info into the etsf output file
call read_kpt_loop()

!Close input file
 CLOSE(1)

!ETSF association of variables
 call associate_etsf()

!ETSF write data
! We call the group level write routines.
 call write_etsf()

! We close etsf file.
 call etsf_io_low_close(ncid, lstat, error_data)
 if (.not. lstat) then
   call etsf_io_low_error_handle(error_data)
   stop
 end if
 
!Deallocate etsf variables
 call deallocate_etsf() 


contains

subroutine read_kpt_loop()
!
! big kpt loop to read G vectors, wave coefficients, 
! eigenvalues and occupations
!
 DO isppol=1, dims%number_of_spins
  DO ik=1, dims%number_of_kpoints
   write(*,*)'spin ',isppol,'kpt ',ik,'/',dims%number_of_kpoints
   kpttmp(:)=red_coord_kpt(:,ik)
   READ(1)npw,dims%number_of_spinor_components,nbandk      ! for each k point
   !check that the number of plane waves for this k-point
   !corresponds to the value already stored in number_of coefficients
   if(npw .ne. number_of_coefficients(ik))&
       & call errore('wfk2etsf','npw read is different from the stored value',1)

! We have to allocate the coefficients of wavefunctions and the
! G vectors for every k-point because there might be a different number of
! plane waves at every k-point
    allocate(coef_pw_k(2, &
         & npw *dims%number_of_spinor_components * nbandk))
    main%wfs_coeff__kpoint_access = ik
    main%wfs_coeff__spin_access = isppol
    main%wfs_coeff__number_of_states = nbandk
    main%wfs_coeff__number_of_coefficients = npw *dims%number_of_spinor_components

    allocate(red_coord_pw_k(3, npw))
    basisdata%red_coord_pw__kpoint_access = ik
    basisdata%red_coord_pw__number_of_coefficients = npw

!Read G vectors
   READ(1)red_coord_pw_k(1:3,1:npw)

!Read eigenvalues and occupations
   ALLOCATE(eigen(nbandk),occk(nbandk))
   READ(1)eigen(1:nbandk),occk(1:nbandk)
!Fill the etsf variables containing the eigenvalues and the occupations   
   do ibandk=1,nbandk
    eigenvalues( ibandk,ik,isppol)=eigen(ibandk)
    occupations( ibandk,ik,isppol)=occk(ibandk)
   end do
   DEALLOCATE(eigen,occk)

!  reads the wavefunction coefficients for each band
   ALLOCATE(cg(2,nbandk*dims%number_of_spinor_components*npw))
   kk=0
   DO iband=1,nbandk
    READ(1)((cg(ii,ij),ii=1,2),&
      &ij=1+(iband-1)*npw*dims%number_of_spinor_components,&
      &iband*npw*dims%number_of_spinor_components)
! Now we have to fill the etsf variable for the wavefunctions coefficients
    do ij= 1+(iband-1)*npw*dims%number_of_spinor_components, &
      &iband*npw*dims%number_of_spinor_components
     kk=kk+1
     coef_pw_k(:,kk)=cg(:,ij)
    end do
   END DO!iband
   DEALLOCATE(cg)

!
!ETSF associations inside this loop
!
!We associate the pointers of groups we want to write with the data in memory.
! We associate the data
 main%coefficients_of_wavefunctions%data2D => coef_pw_k
 basisdata%reduced_coordinates_of_plane_waves%data2D => red_coord_pw_k


!Now that all the arrays we want to write are associated,
! we can call the write routine. 
! Write ETSF arrays dependent of the k-points
 ! We use the group level write routine.
 call etsf_io_main_put(ncid, main, lstat, error_data)
 if (.not. lstat) then
    call etsf_io_low_error_handle(error_data)
    stop
 end if
 call etsf_io_basisdata_put(ncid, basisdata, lstat, error_data)
 if (.not. lstat) then
    call etsf_io_low_error_handle(error_data)
    stop
 end if


 main%coefficients_of_wavefunctions%data2D => null()
 basisdata%reduced_coordinates_of_plane_waves%data2D => null()
 deallocate(coef_pw_k)
 deallocate(red_coord_pw_k)

  END DO !ik
 END DO !dims%number_of_spins
end subroutine read_kpt_loop


subroutine show_etsf_variables()
 write(*,'("number of grid points vectors=",3i6)')&
  &dims%number_of_grid_points_vector1,&
  &dims%number_of_grid_points_vector2,&
  &dims%number_of_grid_points_vector3
 write(*,'("number of atoms = ",i3)')dims%number_of_atoms
 write(*,'("number of atom species = ",i3)')dims%number_of_atom_species

 write(*,'("chemical_symbols ",10000a3)')chemical_symbols(:)

 write(*,'("max number of coefficients (plane waves)=",&
  &i6,a,"max number of states (bands)=",i5,a,"number of kpoints=",&
  &i5,a,"number of spinor components=",i2,a,&
  &"number of spins=",i2)')  dims%max_number_of_coefficients,ch10 &
  &,dims%max_number_of_states,ch10,dims%number_of_kpoints,ch10,&
  &dims%number_of_spinor_components&
  &,ch10,dims%number_of_spins

end subroutine show_etsf_variables

subroutine extract_etsf_variables()
!Get some ETSF arrays
 dims%number_of_grid_points_vector1=ngfft(1)
 dims%number_of_grid_points_vector2=ngfft(2)
 dims%number_of_grid_points_vector3=ngfft(3)

!Get the chemical symbols from the atomic numbers
 do iatom=1,dims%number_of_atom_species
  call atomic_num2name(nint(atomic_numbers(iatom)),chemical_symbols(iatom)) 
  !write(*,*)nint(atomic_numbers(iatom)),chemical_symbols(iatom)
 end do
 ii=0
 do isppol=1,dims%number_of_spins
  do ik=1,dims%number_of_kpoints
  ii=ii+1
  number_of_states(ik,isppol)=nband(ii)
  end do
 end do
 !write(*,*)'number of states', number_of_states

 dims%max_number_of_coefficients = maxval(number_of_coefficients(:))
 dims%max_number_of_states = maxval(nband(:))
 dims%real_or_complex_coefficients = 2

end subroutine extract_etsf_variables


subroutine read_wf_basic()
 read(1)  istwfk(1:dims%number_of_kpoints),&
  &nband(1:dims%number_of_kpoints*dims%number_of_spins),&
  &number_of_coefficients(1:dims%number_of_kpoints),so_psp(1:npsp),&
  &symafm(1:dims%number_of_symmetry_operations),&
  &reduced_symmetry_matrices(1:3,1:3,1:dims%number_of_symmetry_operations),&
  &atom_species(1:dims%number_of_atoms),&
  &red_coord_kpt(1:3,1:dims%number_of_kpoints),&
  & occ(1:bantot),reduced_symmetry_translations(1:3,1:dims%number_of_symmetry_operations),&
  &atomic_numbers(1:dims%number_of_atom_species),kpoint_weights(1:dims%number_of_kpoints)


end subroutine read_wf_basic


subroutine allocate_abi()
 ALLOCATE( istwfk(dims%number_of_kpoints))
 ALLOCATE ( nband(dims%number_of_kpoints*dims%number_of_spins))
 ALLOCATE(number_of_coefficients(dims%number_of_kpoints) )
 ALLOCATE(number_of_states(dims%number_of_kpoints,dims%number_of_spins) )
 ALLOCATE( so_psp(npsp), symafm(dims%number_of_symmetry_operations))
 ALLOCATE( reduced_symmetry_matrices(3,3,dims%number_of_symmetry_operations))
 ALLOCATE( atom_species(dims%number_of_atoms) )
 ALLOCATE( chemical_symbols(dims%number_of_atom_species) )
 allocate( red_coord_kpt(3, dims%number_of_kpoints))
 ALLOCATE( occ(bantot))
 ALLOCATE( reduced_symmetry_translations(3,dims%number_of_symmetry_operations))
 ALLOCATE( atomic_numbers(dims%number_of_atom_species))
 ALLOCATE( kpoint_weights(dims%number_of_kpoints) )
 ALLOCATE( reduced_atom_positions(3,dims%number_of_atoms) )

end subroutine allocate_abi


subroutine readheader()
 READ(1) codvsn,headform,fform
 write(*,*)'Verify that the format you are using: ',codvsn,'is compatible with ABNIT-5.3: '
 READ(unit=1) bantot,date,intxc,ixc,dims%number_of_atoms,ngfft(1:3),&
  &dims%number_of_kpoints,dims%number_of_components,dims%number_of_spinor_components,&
  &dims%number_of_spins,dims%number_of_symmetry_operations,npsp,&
  &dims%number_of_atom_species,occopt,pertcase,usepaw,&
  &kinetic_energy_cutoff,ecutdg,ecutsm,ecut_eff,qptn(1:3),primitive_vectors(1:3,1:3)&
  &,stmbias,tphysel,smearing_width

end subroutine readheader


subroutine write_etsf()
 call etsf_io_kpoints_put(ncid, kpoints, lstat, error_data)
 if (.not. lstat) then
    call etsf_io_low_error_handle(error_data)
    stop
 end if
 call etsf_io_basisdata_put(ncid, basisdata, lstat, error_data)
 if (.not. lstat) then
    call etsf_io_low_error_handle(error_data)
    stop
 end if
 call etsf_io_electrons_put(ncid, electrons, lstat, error_data)
 if (.not. lstat) then
    call etsf_io_low_error_handle(error_data)
    stop
 end if
 call etsf_io_geometry_put(ncid, geometry, lstat, error_data)
 if (.not. lstat) then
    call etsf_io_low_error_handle(error_data)
    stop
 end if
end subroutine write_etsf


subroutine associate_etsf()
 ! We set the associations.
 kpoints%reduced_coordinates_of_kpoints => red_coord_kpt
 kpoints%kpoint_weights => kpoint_weights
 basisdata%basis_set => basis
 basisdata%reduced_coordinates_of_plane_waves%data2D => null()
 basisdata%number_of_coefficients => number_of_coefficients
 basisdata%kinetic_energy_cutoff => kinetic_energy_cutoff
 electrons%fermi_energy            => fermi_energy
 electrons%smearing_width          => smearing_width
 number_of_states                  = dims%max_number_of_states    ! before splitting
 electrons%number_of_states%data2d => number_of_states   ! NB this is a type!
 electrons%eigenvalues%data3d      => eigenvalues  
 electrons%occupations%data3d      => occupations
 geometry%primitive_vectors         => primitive_vectors
 geometry%reduced_symmetry_matrices => reduced_symmetry_matrices
 geometry%reduced_symmetry_translations => reduced_symmetry_translations
 geometry%atom_species              => atom_species
 geometry%reduced_atom_positions    => reduced_atom_positions
 geometry%chemical_symbols          => chemical_symbols
end subroutine associate_etsf


subroutine init_etsf()
!ETSF flags
 flags%basisdata =    etsf_basisdata_basis_set            + &
&                     etsf_basisdata_red_coord_pw         + &
&                     etsf_basisdata_n_coeff              + &
&                     etsf_basisdata_kin_cutoff
 flags%kpoints   =    etsf_kpoints_red_coord_kpt          + &
&                     etsf_kpoints_kpoint_weights
 flags%main      =    etsf_main_wfs_coeff
 flags%electrons =    etsf_electrons_fermi_energy         + &
&                     etsf_electrons_smearing_width       + &
&                     etsf_electrons_number_of_states     + &
&                     etsf_electrons_eigenvalues          + &
&                     etsf_electrons_occupations
 flags%geometry  =    etsf_geometry_primitive_vectors     + &
&                     etsf_geometry_red_sym_matrices      + &
&                     etsf_geometry_red_sym_trans         + &
&                     etsf_geometry_atom_species          + &
&                     etsf_geometry_red_at_pos            + &
&                     etsf_geometry_chemical_symbols



  call etsf_io_data_init(trim(outfile), flags, dims, &
                       & "ETSF_IO file read from an ABINIT WFK file", &
                       & "conversion thanks by wfk2etsf.f90",&
                       & lstat, error_data)
  if (.not. lstat) then
    call etsf_io_low_error_handle(error_data)
    stop
  end if
  
  write(basis, "(A)") "plane_waves"

  ! The main program allocate memory for its computation.
!  allocate(coef_pw_k(2, dims%max_number_of_coefficients * dims%max_number_of_states))
!  allocate(red_coord_pw_k(3, dims%max_number_of_coefficients))
  allocate(   eigenvalues(dims%max_number_of_states,&
&                         dims%number_of_kpoints,&
&                         dims%number_of_spins ) )
  allocate( occupations(dims%max_number_of_states, &
&                         dims%number_of_kpoints,    &
&                         dims%number_of_spins ) )

 
 ! Open file for writing
 call etsf_io_low_open_modify(ncid, trim(outfile), &
      & lstat, error_data = error_data)
 if (.not. lstat) then
   call etsf_io_low_error_handle(error_data)
   stop
 end if
  
  ! We switch to write mode.
 call etsf_io_low_set_write_mode(ncid, lstat, error_data = error_data)
 if (.not. lstat) then
   call etsf_io_low_error_handle(error_data)
   stop
 end if

!ETSF

end subroutine init_etsf

subroutine deallocate_etsf()
 deallocate(number_of_coefficients)
 deallocate(red_coord_kpt)
 deallocate(kpoint_weights)
 deallocate(number_of_states)
 deallocate(reduced_symmetry_matrices)
 deallocate(reduced_symmetry_translations)
 deallocate(atomic_numbers)
 deallocate(atom_species)
 deallocate(reduced_atom_positions)
 deallocate(chemical_symbols)
end subroutine deallocate_etsf

#else
   CALL errore('wfk2etsf','ETSF_IO not supported', 10)
#endif
end program wfk2etsf


!!!#############################
SUBROUTINE checkCommandLineInputs(infile)
!!!#############################

  IMPLICIT NONE
  character(80):: infile

  call getarg(1,infile)

  if ( trim(infile).eq.'') then

     write(*,*) 'Usage'
     write(*,*) './wfk2etsf.x  <WFK_file>'
     write(*,*) 
     call exit(1)
  end if
END SUBROUTINE checkCommandLineInputs

