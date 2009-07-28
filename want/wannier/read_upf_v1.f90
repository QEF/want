
! Copyright (C) 2002-2008 Quantum-Espresso group
! This file is distributed under the terms of the
! GNU General Public License. See the file `License'
! in the root directory of the present distribution,
! or http://www.gnu.org/copyleft/gpl.txt .
!

!=----------------------------------------------------------------------------=!
      MODULE read_upf_v1_module
!=----------------------------------------------------------------------------=!

!  this module handles the reading of pseudopotential data

! ...   declare modules
        USE kinds,               ONLY : DP => dbl
        USE radial_grids_module, ONLY : allocate_radial_grid
        USE parser_module,       ONLY : matches 
        IMPLICIT NONE
        SAVE
        PRIVATE
        PUBLIC :: read_upf_v1, scan_begin, scan_end
      CONTAINS
!
!---------------------------------------------------------------------
subroutine read_upf_v1 (iunps, upf, grid, ierr, header_only)
  !---------------------------------------------------------------------
  !
  !   read pseudopotential "upf" in the Unified Pseudopotential Format
  !   from unit "iunps" - return error code in "ierr" (success: ierr=0)
  !
  use pseudo_types_module
  use radial_grids_module, ONLY : radial_grid_type
  !
  implicit none
  !
  INTEGER, INTENT(IN) :: iunps
  INTEGER, INTENT(OUT) :: ierr 
  LOGICAL, INTENT(IN), OPTIONAL :: header_only
  TYPE (pseudo_upf), INTENT(INOUT)       :: upf
  TYPE (radial_grid_type), TARGET, INTENT(INOUT) :: grid
  !
  !     Local variables
  !
  integer :: ios
  character (len=80) :: dummy  
  !
  ! Prepare the pointers
  ! CALL nullify_pseudo_upf( upf ) should be nullified when instantiated
  !
  upf%grid => grid
  !
  ! First check if this pseudo-potential has spin-orbit information 
  !
  ierr = 1  
  ios = 0
  upf%q_with_l=.false.
  upf%has_so=.false.
  upf%has_gipaw = .false.
  addinfo_loop: do while (ios == 0)  
     read (iunps, *, iostat = ios, err = 200) dummy  
     if (matches ("<PP_ADDINFO>", dummy) ) then
        upf%has_so=.true.
     endif
     if ( matches ( "<PP_GIPAW_RECONSTRUCTION_DATA>", dummy ) ) then
        upf%has_gipaw = .true.
     endif
     if (matches ("<PP_QIJ_WITH_L>", dummy) ) then
        upf%q_with_l=.true. 
     endif
  enddo addinfo_loop
  
  !------->Search for Header
  !     This version doesn't use the new routine scan_begin
  !     because this search must set extra flags for
  !     compatibility with other pp format reading
  ierr = 1  
  ios = 0
  rewind(iunps)
  header_loop: do while (ios == 0)  
     read (iunps, *, iostat = ios, err = 200) dummy  
     if (matches ("<PP_HEADER>", dummy) ) then  
        ierr = 0
        call read_pseudo_header (upf, iunps)  
        exit header_loop
     endif
  enddo header_loop
  !
  ! this should be read from the PP_INFO section
  !
  upf%generated='Generated by new atomic code, or converted to UPF format'

  IF ( PRESENT (header_only) ) THEN
     IF ( header_only ) RETURN
  END IF
  if (ierr .ne. 0) return
  
  call scan_end (iunps, "HEADER")  

  ! WRITE( stdout, * ) "Reading pseudopotential file in UPF format"  

  !-------->Search for mesh information
  call scan_begin (iunps, "MESH", .true.)  
  call read_pseudo_mesh (upf, iunps)  
  call scan_end (iunps, "MESH")  
  !-------->If  present, search for nlcc
  if ( upf%nlcc ) then  
     call scan_begin (iunps, "NLCC", .true.)  
     call read_pseudo_nlcc (upf, iunps)  
     call scan_end (iunps, "NLCC")  
  else
     ALLOCATE( upf%rho_atc( upf%mesh ) )
     upf%rho_atc = 0.0_DP
  endif
  !-------->Fake 1/r potential: do not read PP
  if (.not. matches ("1/r", upf%typ) ) then
  !-------->Search for Local potential
     call scan_begin (iunps, "LOCAL", .true.)  
     call read_pseudo_local (upf, iunps)  
     call scan_end (iunps, "LOCAL")  
  !-------->Search for Nonlocal potential
     call scan_begin (iunps, "NONLOCAL", .true.)  
     call read_pseudo_nl (upf, iunps)  
     call scan_end (iunps, "NONLOCAL")  
  !--------
  end if
  !-------->Search for atomic wavefunctions
  call scan_begin (iunps, "PSWFC", .true.)  
  call read_pseudo_pswfc (upf, iunps)  
  call scan_end (iunps, "PSWFC")  
  !-------->Search for atomic charge
  call scan_begin (iunps, "RHOATOM", .true.)  
  call read_pseudo_rhoatom (upf, iunps)  
  call scan_end (iunps, "RHOATOM")  
  !-------->Search for add_info
  if (upf%has_so) then
     call scan_begin (iunps, "ADDINFO", .true.)  
     call read_pseudo_addinfo (upf, iunps)  
     call scan_end (iunps, "ADDINFO")  
  endif
  !-------->GIPAW data
  IF ( upf%has_gipaw ) then
     CALL scan_begin ( iunps, "GIPAW_RECONSTRUCTION_DATA", .false. )
     CALL read_pseudo_gipaw ( upf, iunps )
     CALL scan_end ( iunps, "GIPAW_RECONSTRUCTION_DATA" )
  END IF
  !--- Try to get the core radius if not present. Needed by the 
  !    atomic code for old pseudo files
  IF (upf%nbeta>0) THEN ! rcutus may be unallocated if nbeta=0
  IF(upf%rcutus(1)<1.e-9_DP) THEN 
     call scan_begin (iunps, "INFO", .true.)  
     call read_pseudo_ppinfo (upf, iunps)  
     call scan_end (iunps, "INFO")
  ENDIF
  ENDIF

200 return

end subroutine read_upf_v1
!---------------------------------------------------------------------


subroutine scan_begin (iunps, string, rew)  
  !---------------------------------------------------------------------
  !
  implicit none
  ! Unit of the input file
  integer :: iunps  
  ! Label to be matched
  character (len=*) :: string  
  ! String read from file
  character (len=75) :: rstring  
  ! Flag if .true. rewind the file
  logical :: rew  
  integer :: ios

  ios = 0
  if (rew) rewind (iunps)  
  do while (ios==0)  
     read (iunps, *, iostat = ios, err = 300) rstring  
     if (matches ("<PP_"//string//">", rstring) ) return  
  enddo
  return
300 call errore ('scan_begin', 'No '//string//' block', abs (ios) )  
end subroutine scan_begin
!---------------------------------------------------------------------

subroutine scan_end (iunps, string)  
  !---------------------------------------------------------------------
  implicit none
  ! Unit of the input file
  integer :: iunps
  ! Label to be matched
  character (len=*) :: string  
  ! String read from file
  character (len=75) :: rstring

  read (iunps, '(a)', end = 300, err = 300) rstring  
  if (matches ("</PP_"//string//">", rstring) ) return  
  return
300 call errore ('scan_end', &
       'No '//string//' block end statement, possibly corrupted file',  -1)
end subroutine scan_end
!
!---------------------------------------------------------------------

subroutine read_pseudo_header (upf, iunps)  
  !---------------------------------------------------------------------
  !
  USE pseudo_types_module, ONLY : pseudo_upf
  USE kinds,               ONLY : DP => dbl

  implicit none
  !
  TYPE (pseudo_upf), INTENT(INOUT) :: upf
  integer :: iunps  
  !
  integer :: nw
  character (len=80) :: dummy

  ! Version number (presently ignored)
  read (iunps, *, err = 100, end = 100) upf%nv , dummy  
  ! Element label
  read (iunps, *, err = 100, end = 100) upf%psd , dummy  
  ! Type of pseudo (1/r cannot be read with default format!!!)
  read (iunps, '(a80)', err = 100, end = 100) dummy
  upf%typ=trim(adjustl(dummy))
  !
  if (matches ('US', upf%typ) ) then
     upf%tvanp = .true.  
     upf%tpawp = .false.  
     upf%tcoulombp = .false.
  else if (matches ('PAW', upf%typ) ) then
     ! Note: if tvanp is set to false the results are wrong!
     upf%tvanp = .true.  
     upf%tpawp = .true.  
     upf%tcoulombp = .false.
  else if (matches ('NC', upf%typ) ) then
     upf%tvanp = .false.  
     upf%tpawp = .false.  
     upf%tcoulombp = .false.
  else if (matches ('1/r', upf%typ) ) then
     upf%tvanp = .false.  
     upf%tpawp = .false.
     upf%tcoulombp = .true.
  else
     call errore ('read_pseudo_header', 'unknown pseudo type', 1)
  endif

  read (iunps, *, err = 100, end = 100) upf%nlcc , dummy  

  read (iunps, '(a20,t24,a)', err = 100, end = 100) upf%dft, dummy  

  read (iunps, * ) upf%zp , dummy  
  read (iunps, * ) upf%etotps, dummy  
  read (iunps, * ) upf%ecutwfc, upf%ecutrho
  read (iunps, * ) upf%lmax , dummy
  read (iunps, *, err = 100, end = 100) upf%mesh , dummy
  upf%grid%mesh = upf%mesh
  call allocate_radial_grid(upf%grid,upf%grid%mesh)
!   IF ( upf%grid%mesh > SIZE (upf%grid%r) ) &
!      CALL errore('read_pseudo_header', 'too many grid points', 1)

  read (iunps, *, err = 100, end = 100) upf%nwfc, upf%nbeta , dummy
  read (iunps, '(a)', err = 100, end = 100) dummy
  ALLOCATE( upf%els( upf%nwfc ), upf%lchi( upf%nwfc ), upf%oc( upf%nwfc ) )
  do nw = 1, upf%nwfc  
     read (iunps, * ) upf%els (nw), upf%lchi (nw), upf%oc (nw)  
  enddo

  return  

100  call errore ('read_pseudo_header', 'Reading pseudo file', 1 )
end subroutine read_pseudo_header

!---------------------------------------------------------------------

subroutine read_pseudo_mesh (upf, iunps)  
  !---------------------------------------------------------------------
  !
  USE kinds,               ONLY : DP => dbl
  USE pseudo_types_module, ONLY : pseudo_upf

  implicit none
  !
  integer :: iunps  
  TYPE (pseudo_upf), INTENT(INOUT) :: upf
  !
  integer :: ir

  IF(associated(upf%grid)) THEN
     upf%r   => upf%grid%r
     upf%rab => upf%grid%rab
  ELSE
     ALLOCATE( upf%r( upf%mesh ), upf%rab( upf%mesh ) )
  ENDIF
  upf%r   = 0.0_DP
  upf%rab = 0.0_DP

  call scan_begin (iunps, "R", .false.)  
  read (iunps, *, err = 100, end = 100) (upf%r(ir), ir=1,upf%mesh )
  call scan_end (iunps, "R")  
  call scan_begin (iunps, "RAB", .false.)  
  read (iunps, *, err = 101, end = 101) (upf%rab(ir), ir=1,upf%mesh )
  call scan_end (iunps, "RAB")  
!   upf%grid%r(1:upf%mesh)   = upf%r(1:upf%mesh)
!   upf%grid%rab(1:upf%mesh) = upf%rab(1:upf%mesh)

  return  

100 call errore ('read_pseudo_mesh', 'Reading pseudo file (R) for '//upf%psd,1)
101 call errore ('read_pseudo_mesh', 'Reading pseudo file (RAB) for '//upf%psd,2)  
end subroutine read_pseudo_mesh


!---------------------------------------------------------------------
subroutine read_pseudo_nlcc (upf, iunps)
  !---------------------------------------------------------------------
  !
  USE kinds,               ONLY : DP => dbl
  USE pseudo_types_module, ONLY : pseudo_upf

  implicit none
  !
  integer :: iunps  
  TYPE (pseudo_upf), INTENT(INOUT) :: upf
  !
  integer :: ir
  !
  ALLOCATE( upf%rho_atc( upf%mesh ) )
  upf%rho_atc = 0.0_DP

  read (iunps, *, err = 100, end = 100) (upf%rho_atc(ir), ir=1,upf%mesh )
  !
  return

100 call errore ('read_pseudo_nlcc', 'Reading pseudo file', 1)
  return
end subroutine read_pseudo_nlcc

!---------------------------------------------------------------------
subroutine read_pseudo_local (upf, iunps)
  !---------------------------------------------------------------------
  !
  USE kinds,               ONLY : DP => dbl
  USE pseudo_types_module, ONLY : pseudo_upf

  implicit none
  !
  integer :: iunps  
  TYPE (pseudo_upf), INTENT(INOUT) :: upf
  !
  integer :: ir
  !
  ALLOCATE( upf%vloc( upf%mesh ) )
  upf%vloc = 0.0_DP

  read (iunps, *, err=100, end=100) (upf%vloc(ir) , ir=1,upf%mesh )

  return

100 call errore ('read_pseudo_local','Reading pseudo file', 1)
  return
end subroutine read_pseudo_local

!---------------------------------------------------------------------

subroutine read_pseudo_nl (upf, iunps)  
  !---------------------------------------------------------------------
  !
  USE kinds,               ONLY : DP => dbl
  USE pseudo_types_module, ONLY : pseudo_upf

  implicit none
  !
  integer :: iunps  
  TYPE (pseudo_upf), INTENT(INOUT) :: upf
  !
  integer :: nb, mb, ijv, n, ir, ios, idum, ldum, icon, lp, i, ikk, l, l1,l2, nd
  ! counters
  character (len=75) :: dummy  
  !
  ! Threshold for qfunc to be considered zero (inserted in version UPF v2)
  upf%qqq_eps = -1._dp
  !
  if ( upf%nbeta == 0) then
     upf%nqf = 0
     upf%nqlc= 0
     upf%kkbeta = 0
     ALLOCATE( upf%kbeta( 1 ) )
     ALLOCATE( upf%lll( 1 ) )
     ALLOCATE( upf%beta( upf%mesh, 1 ) )
     ALLOCATE( upf%dion( 1, 1 ) )
     ALLOCATE( upf%rinner( 1 ) )
     ALLOCATE( upf%qqq   ( 1, 1 ) )
     ALLOCATE( upf%qfunc ( upf%mesh, 1 ) )
     ALLOCATE( upf%qfcoef( 1, 1, 1, 1 ) )
     ALLOCATE( upf%rcut( 1 ) )
     ALLOCATE( upf%rcutus( 1 ) )
     ALLOCATE( upf%els_beta( 1 ) )
     return
  end if
  ALLOCATE( upf%kbeta( upf%nbeta ) )
  ALLOCATE( upf%lll( upf%nbeta ) )
  ALLOCATE( upf%beta( upf%mesh, upf%nbeta ) )
  ALLOCATE( upf%dion( upf%nbeta, upf%nbeta ) )
  ALLOCATE( upf%rcut( upf%nbeta ) )
  ALLOCATE( upf%rcutus( upf%nbeta ) )
  ALLOCATE( upf%els_beta( upf%nbeta ) )

  upf%kkbeta = 0  
  upf%lll    = 0  
  upf%beta   = 0.0_DP
  upf%dion   = 0.0_DP
  upf%rcut   = 0.0_DP
  upf%rcutus = 0.0_DP
  upf%els_beta = '  '

  do nb = 1, upf%nbeta 
     call scan_begin (iunps, "BETA", .false.)  
     read (iunps, *, err = 100, end = 100) idum, upf%lll(nb), dummy
     read (iunps, *, err = 100, end = 100) ikk  
     upf%kbeta(nb) = ikk
     upf%kkbeta = MAX ( upf%kkbeta, upf%kbeta(nb) )  
     read (iunps, *, err = 100, end = 100) (upf%beta(ir,nb), ir=1,ikk)

     read (iunps, *, err=200,iostat=ios) upf%rcut(nb), upf%rcutus(nb)
     read (iunps, *, err=200,iostat=ios) upf%els_beta(nb)
     call scan_end (iunps, "BETA")  
200  continue
  enddo


  call scan_begin (iunps, "DIJ", .false.)  
  read (iunps, *, err = 101, end = 101) nd, dummy  
  do icon = 1, nd
     read (iunps, *, err = 101, end = 101) nb, mb, upf%dion(nb,mb)
     upf%dion (mb,nb) = upf%dion (nb,mb)  
  enddo
  call scan_end (iunps, "DIJ")  


  if ( upf%tvanp .or. upf%tpawp) then
     call scan_begin (iunps, "QIJ", .false.)  
     read (iunps, *, err = 102, end = 102) upf%nqf
     upf%nqlc = 2 * upf%lmax  + 1
     ALLOCATE( upf%rinner( upf%nqlc ) )
     ALLOCATE( upf%qqq   ( upf%nbeta, upf%nbeta ) )
     IF (upf%q_with_l .or. upf%tpawp) then
        ALLOCATE( upf%qfuncl ( upf%mesh, upf%nbeta*(upf%nbeta+1)/2, 0:2*upf%lmax ) )
        upf%qfuncl  = 0.0_DP
     ELSE
        ALLOCATE( upf%qfunc ( upf%mesh, upf%nbeta*(upf%nbeta+1)/2 ) )
        upf%qfunc  = 0.0_DP
     ENDIF
     ALLOCATE( upf%qfcoef( MAX( upf%nqf,1 ), upf%nqlc, upf%nbeta, upf%nbeta ) )
     upf%rinner = 0.0_DP
     upf%qqq    = 0.0_DP
     upf%qfcoef = 0.0_DP
     if ( upf%nqf /= 0) then
        call scan_begin (iunps, "RINNER", .false.)  
        read (iunps,*,err=103,end=103) ( idum, upf%rinner(i), i=1,upf%nqlc )
        call scan_end (iunps, "RINNER")  
     end if
     do nb = 1, upf%nbeta
        do mb = nb, upf%nbeta

           read (iunps,*,err=102,end=102) idum, idum, ldum, dummy
           !"  i    j   (l)"
           if (ldum /= upf%lll(mb) ) then
             call errore ('read_pseudo_nl','inconsistent angular momentum for Q_ij', 1)
           end if

           read (iunps,*,err=104,end=104) upf%qqq(nb,mb), dummy
           ! "Q_int"
           upf%qqq(mb,nb) = upf%qqq(nb,mb)  
           ! ijv is the combined (nb,mb) index
           ijv = mb * (mb-1) / 2 + nb
           IF (upf%q_with_l .or. upf%tpawp) THEN
              l1=upf%lll(nb)
              l2=upf%lll(mb)
              DO l=abs(l1-l2),l1+l2
                 read (iunps, *, err=105, end=105) (upf%qfuncl(n,ijv,l), &
                                                    n=1,upf%mesh)
              END DO
           ELSE
              read (iunps, *, err=105, end=105) (upf%qfunc(n,ijv), n=1,upf%mesh)
           ENDIF

           if ( upf%nqf > 0 ) then
              call scan_begin (iunps, "QFCOEF", .false.)  
              read (iunps,*,err=106,end=106) &
                        ( ( upf%qfcoef(i,lp,nb,mb), i=1,upf%nqf ), lp=1,upf%nqlc )
              do i = 1, upf%nqf
                 do lp = 1, upf%nqlc
                    upf%qfcoef(i,lp,mb,nb) = upf%qfcoef(i,lp,nb,mb)
                 end do
              end do
              call scan_end (iunps, "QFCOEF")  
           end if

        enddo
     enddo
     call scan_end (iunps, "QIJ")  
  else
     upf%nqf  = 1
     upf%nqlc = 2 * upf%lmax  + 1
     ALLOCATE( upf%rinner( upf%nqlc ) )
     ALLOCATE( upf%qqq   ( upf%nbeta, upf%nbeta ) )
     ALLOCATE( upf%qfunc ( upf%mesh, upf%nbeta*(upf%nbeta+1)/2 ) )
     ALLOCATE( upf%qfcoef( upf%nqf, upf%nqlc, upf%nbeta, upf%nbeta ) )
     upf%rinner = 0.0_DP
     upf%qqq    = 0.0_DP
     upf%qfunc  = 0.0_DP
     upf%qfcoef = 0.0_DP
  endif


  return  

100 call errore ('read_pseudo_nl', 'Reading pseudo file (BETA)', 1 )  
101 call errore ('read_pseudo_nl', 'Reading pseudo file (DIJ)',  2 )  
102 call errore ('read_pseudo_nl', 'Reading pseudo file (QIJ)',  3 )
103 call errore ('read_pseudo_nl', 'Reading pseudo file (RINNER)',4)
104 call errore ('read_pseudo_nl', 'Reading pseudo file (qqq)',  5 )
105 call errore ('read_pseudo_nl', 'Reading pseudo file (qfunc)',6 )
106 call errore ('read_pseudo_nl', 'Reading pseudo file (qfcoef)',7)
end subroutine read_pseudo_nl


!---------------------------------------------------------------------
subroutine read_pseudo_pswfc (upf, iunps)  
  !---------------------------------------------------------------------
  !
  USE kinds,               ONLY : DP => dbl
  USE pseudo_types_module, ONLY : pseudo_upf
  !
  implicit none
  !
  integer :: iunps
  TYPE (pseudo_upf), INTENT(INOUT) :: upf
  !
  character (len=75) :: dummy  
  integer :: nb, ir

  ALLOCATE( upf%chi( upf%mesh, MAX( upf%nwfc, 1 ) ) )
  upf%chi = 0.0_DP
  do nb = 1, upf%nwfc  
     read (iunps, *, err=100, end=100) dummy  !Wavefunction labels
     read (iunps, *, err=100, end=100) ( upf%chi(ir,nb), ir=1,upf%mesh )
  enddo

  return  

100 call errore ('read_pseudo_pswfc', 'Reading pseudo file', 1)
end subroutine read_pseudo_pswfc

!---------------------------------------------------------------------
subroutine read_pseudo_rhoatom (upf, iunps)  
  !---------------------------------------------------------------------
  !
  USE kinds,               ONLY : DP => dbl 
  USE pseudo_types_module, ONLY : pseudo_upf
  !
  implicit none
  !
  integer :: iunps
  TYPE (pseudo_upf), INTENT(INOUT) :: upf
  !
  integer :: ir
  !
  ALLOCATE( upf%rho_at( upf%mesh ) )
  upf%rho_at = 0.0_DP
  read (iunps,*,err=100,end=100) ( upf%rho_at(ir), ir=1,upf%mesh )
  !
  return  

100 call errore ('read_pseudo_rhoatom','Reading pseudo file', 1)
end subroutine read_pseudo_rhoatom
!
!---------------------------------------------------------------------
subroutine read_pseudo_addinfo (upf, iunps)
!---------------------------------------------------------------------
!
!     This routine reads from the new UPF file,
!     and the total angular momentum jjj of the beta and jchi of the
!     wave-functions.
!
  USE pseudo_types_module, ONLY : pseudo_upf
  USE kinds,               ONLY : DP => dbl
  implicit none
  integer :: iunps
  
  TYPE (pseudo_upf), INTENT(INOUT) :: upf
  integer :: nb
  
  ALLOCATE( upf%nn(upf%nwfc) )
  ALLOCATE( upf%epseu(upf%nwfc), upf%jchi(upf%nwfc) )
  ALLOCATE( upf%jjj(upf%nbeta) )

  upf%nn=0
  upf%epseu=0.0_DP
  upf%jchi=0.0_DP
  do nb = 1, upf%nwfc
     read (iunps, *,err=100,end=100) upf%els(nb),  &
          upf%nn(nb), upf%lchi(nb), upf%jchi(nb), upf%oc(nb)
     if ( abs ( upf%jchi(nb)-upf%lchi(nb)-0.5_dp ) > 1.0d-7 .and. &
          abs ( upf%jchi(nb)-upf%lchi(nb)+0.5_dp ) > 1.0d-7      ) then
        call warning ('read_pseudo_upf', 'obsolete ADDINFO section ignored')
        upf%has_so = .false.
        return
     end if
  enddo
  
  upf%jjj=0.0_DP
  do nb = 1, upf%nbeta
     read (iunps, *, err=100,end=100) upf%lll(nb), upf%jjj(nb)
     if ( abs ( upf%lll(nb)-upf%jjj(nb)-0.5_dp) > 1.0d-7 .and. &
          abs ( upf%lll(nb)-upf%jjj(nb)+0.5_dp) > 1.0d-7       ) then
        call warning ( 'read_pseudo_upf', 'obsolete ADDINFO section ignored')
        upf%has_so = .false.
        return
     end if
  enddo
  
  read(iunps, *) upf%xmin, upf%rmax, upf%zmesh, upf%dx
  upf%grid%dx   = upf%dx
  upf%grid%xmin = upf%xmin
  upf%grid%zmesh= upf%zmesh
  upf%grid%mesh = upf%mesh

  return
100 call errore ('read_pseudo_addinfo','Reading pseudo file', 1)
end subroutine read_pseudo_addinfo
!
!---------------------------------------------------------------------
SUBROUTINE read_pseudo_gipaw ( upf, iunps )
  !---------------------------------------------------------------------
  !
  USE kinds,               ONLY : DP => dbl
  USE pseudo_types_module, ONLY : pseudo_upf
  !
  implicit none
  !
  INTEGER :: iunps
  TYPE ( pseudo_upf ), INTENT ( INOUT ) :: upf
  !
  
  CALL scan_begin ( iunps, "GIPAW_FORMAT_VERSION", .false. )
  READ ( iunps, *, err=100, end=100 ) upf%gipaw_data_format
  CALL scan_end ( iunps, "GIPAW_FORMAT_VERSION" )
  
  IF ( upf%gipaw_data_format == 1 ) THEN
     CALL read_pseudo_gipaw_core_orbitals ( upf, iunps )
     CALL read_pseudo_gipaw_local ( upf, iunps )
     CALL read_pseudo_gipaw_orbitals ( upf, iunps )
  ELSE
     CALL errore ( 'read_pseudo_gipaw', 'UPF/GIPAW in unknown format', 1 )
  END IF
  
  RETURN
  
100 CALL errore ( 'read_pseudo_gipaw', 'Reading pseudo file', 1 )
END SUBROUTINE read_pseudo_gipaw

!---------------------------------------------------------------------
SUBROUTINE read_pseudo_gipaw_core_orbitals ( upf, iunps )
  !---------------------------------------------------------------------
  !
  USE kinds,               ONLY : DP => dbl
  USE pseudo_types_module, ONLY : pseudo_upf
  !
  IMPLICIT NONE
  !
  INTEGER :: iunps
  TYPE ( pseudo_upf ), INTENT ( INOUT ) :: upf
  !
  CHARACTER ( LEN = 75 ) :: dummy1, dummy2
  INTEGER :: nb, ir
  
  CALL scan_begin ( iunps, "GIPAW_CORE_ORBITALS", .false. )
  READ ( iunps, *, err=100, end=100 ) upf%gipaw_ncore_orbitals
  
  ALLOCATE ( upf%gipaw_core_orbital_n(upf%gipaw_ncore_orbitals) )
  ALLOCATE ( upf%gipaw_core_orbital_l(upf%gipaw_ncore_orbitals) )
  ALLOCATE ( upf%gipaw_core_orbital_el(upf%gipaw_ncore_orbitals) )
  ALLOCATE ( upf%gipaw_core_orbital(upf%mesh,upf%gipaw_ncore_orbitals) )
  upf%gipaw_core_orbital = 0.0_dp
  
  DO nb = 1, upf%gipaw_ncore_orbitals
     CALL scan_begin ( iunps, "GIPAW_CORE_ORBITAL", .false. )
     READ (iunps, *, err=100, end=100) &
          upf%gipaw_core_orbital_n(nb), upf%gipaw_core_orbital_l(nb), &
          dummy1, dummy2, upf%gipaw_core_orbital_el(nb)
     READ ( iunps, *, err=100, end=100 ) &
          ( upf%gipaw_core_orbital(ir,nb), ir = 1, upf%mesh )
     CALL scan_end ( iunps, "GIPAW_CORE_ORBITAL" )
  END DO
  
  CALL scan_end ( iunps, "GIPAW_CORE_ORBITALS" )
  
  RETURN
  
100 CALL errore ( 'read_pseudo_gipaw_core_orbitals', 'Reading pseudo file', 1 )
END SUBROUTINE read_pseudo_gipaw_core_orbitals

!---------------------------------------------------------------------
SUBROUTINE read_pseudo_gipaw_local ( upf, iunps )
  !---------------------------------------------------------------------
  !
  USE kinds,               ONLY : DP => dbl
  USE pseudo_types_module, ONLY : pseudo_upf
  !
  IMPLICIT NONE
  !
  INTEGER :: iunps
  TYPE ( pseudo_upf ), INTENT ( INOUT ) :: upf
  !
  INTEGER :: ir
  
  CALL scan_begin ( iunps, "GIPAW_LOCAL_DATA", .false. )
  
  ALLOCATE ( upf%gipaw_vlocal_ae(upf%mesh) )
  ALLOCATE ( upf%gipaw_vlocal_ps(upf%mesh) )
  
  CALL scan_begin ( iunps, "GIPAW_VLOCAL_AE", .false. )
  
  READ ( iunps, *, err=100, end=100 ) &
       ( upf%gipaw_vlocal_ae(ir), ir = 1, upf%mesh )
  
  CALL scan_end ( iunps, "GIPAW_VLOCAL_AE" )
  
  CALL scan_begin ( iunps, "GIPAW_VLOCAL_PS", .false. )
  
  READ ( iunps, *, err=100, end=100 ) &
       ( upf%gipaw_vlocal_ps(ir), ir = 1, upf%mesh )
  
  CALL scan_end ( iunps, "GIPAW_VLOCAL_PS" )
  
  CALL scan_end ( iunps, "GIPAW_LOCAL_DATA" )
  
  RETURN
  
100 CALL errore ( 'read_pseudo_gipaw_local', 'Reading pseudo file', 1 )
END SUBROUTINE read_pseudo_gipaw_local

!---------------------------------------------------------------------
SUBROUTINE read_pseudo_gipaw_orbitals ( upf, iunps )
  !---------------------------------------------------------------------
  !
  USE kinds,               ONLY : DP => dbl
  USE pseudo_types_module, ONLY : pseudo_upf
  !
  IMPLICIT NONE
  !
  INTEGER :: iunps
  TYPE ( pseudo_upf ), INTENT ( INOUT ) :: upf
  !
  CHARACTER ( LEN = 75 ) :: dummy
  INTEGER :: nb, ir
  
  CALL scan_begin ( iunps, "GIPAW_ORBITALS", .false. )
  READ ( iunps, *, err=100, end=100 ) upf%gipaw_wfs_nchannels
  
  ALLOCATE ( upf%gipaw_wfs_el(upf%gipaw_wfs_nchannels) )
  ALLOCATE ( upf%gipaw_wfs_ll(upf%gipaw_wfs_nchannels) )
  ALLOCATE ( upf%gipaw_wfs_rcut(upf%gipaw_wfs_nchannels) )
  ALLOCATE ( upf%gipaw_wfs_rcutus(upf%gipaw_wfs_nchannels) )
  ALLOCATE ( upf%gipaw_wfs_ae(upf%mesh,upf%gipaw_wfs_nchannels) )
  ALLOCATE ( upf%gipaw_wfs_ps(upf%mesh,upf%gipaw_wfs_nchannels) )
  
  inquire ( unit = iunps, name = dummy )
  DO nb = 1, upf%gipaw_wfs_nchannels
     CALL scan_begin ( iunps, "GIPAW_AE_ORBITAL", .false. )
     READ (iunps, *, err=100, end=100) &
          upf%gipaw_wfs_el(nb), upf%gipaw_wfs_ll(nb)
     READ ( iunps, *, err=100, end=100 ) &
          ( upf%gipaw_wfs_ae(ir,nb), ir = 1, upf%mesh )
     CALL scan_end ( iunps, "GIPAW_AE_ORBITAL" )
     
     CALL scan_begin ( iunps, "GIPAW_PS_ORBITAL", .false. )
     READ (iunps, *, err=100, end=100) &
          upf%gipaw_wfs_rcut(nb), upf%gipaw_wfs_rcutus(nb)
     READ ( iunps, *, err=100, end=100 ) &
          ( upf%gipaw_wfs_ps(ir,nb), ir = 1, upf%mesh )
     CALL scan_end ( iunps, "GIPAW_PS_ORBITAL" )
  END DO
  
  CALL scan_end ( iunps, "GIPAW_ORBITALS" )
  
  RETURN
  
100 CALL errore ( 'read_pseudo_gipaw_orbitals', 'Reading pseudo file', 1 )
END SUBROUTINE read_pseudo_gipaw_orbitals
!</apsi>

subroutine read_pseudo_ppinfo (upf, iunps)  
  !---------------------------------------------------------------------
  !
  USE pseudo_types_module, ONLY : pseudo_upf
  USE kinds,               ONLY : DP => dbl

  implicit none
  !
  TYPE (pseudo_upf), INTENT(INOUT) :: upf
  integer :: iunps  
  character (len=80) :: dummy  
  real(dp) :: rdummy
  integer :: idummy, nb, ios

  ios=0
  DO while (ios==0) 
     READ (iunps, '(a)', err = 100, end = 100, iostat=ios) dummy  
     IF (matches ("Rcut", dummy) ) THEN
        DO nb=1,upf%nbeta
           READ (iunps, '(a2,2i3,f6.2,3f19.11)',err=100, end=100,iostat=ios) &
               upf%els_beta(nb), idummy, &
               idummy, rdummy, upf%rcut(nb), upf%rcutus (nb), rdummy
        ENDDO
        ios=100
     ENDIF
  ENDDO
100  RETURN
  END SUBROUTINE read_pseudo_ppinfo

!=----------------------------------------------------------------------------=!
      END MODULE read_upf_v1_module
!=----------------------------------------------------------------------------=!
