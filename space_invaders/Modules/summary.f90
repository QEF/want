!
! Copyright (C) 2004 WanT Group
!
! This file is distributed under the terms of the
! GNU General Public License. See the file `License\'
! in the root directory of the present distribution,
! or http://www.gnu.org/copyleft/gpl.txt .
!

! <INFO>
!*********************************************
   MODULE summary_module
!*********************************************
   USE kinds, ONLY : dbl
   USE parameters, ONLY : nstrx
   USE constants, ONLY : PI, TPI, BOHR => bohr_radius_angs
   USE parser_module, ONLY: log2char
   USE converters_module, ONLY: cry2cart
   USE io_module, ONLY : title, prefix, postfix, work_dir
   USE control_module, ONLY : ordering_mode, verbosity, do_pseudo, &
                              unitary_thr, trial_mode, nprint
   USE trial_center_data_module, ONLY : trial
   USE lattice_module, ONLY : lattice_alloc => alloc, avec, bvec, alat, omega
   USE ions_module, ONLY : ions_alloc => alloc, nat, nsp, symb, tau, psfile
   USE kpoints_module, ONLY : kpoints_alloc, nkpts, vkpt, wk, nk, s, nshells, nwhich, &
                              bshells_alloc, dnn, ndnntot, nnshell, nntot, bk, wb, bka
   USE windows_module, ONLY : windows_alloc => alloc, dimwin, eig, efermi, nbnd, imin, imax,&
                              dimfroz, lfrozen, dimwinx, nspin, spin_component, &
                              win_min, win_max, froz_min, froz_max
   USE subspace_module,ONLY : dimwann, disentangle_thr, alpha_dis, maxiter_dis
   USE localization_module, ONLY : alpha0_wan, alpha1_wan, maxiter0_wan, maxiter1_wan, ncg, &
                             wannier_thr
   !
   ! pseudopotential modules
   USE ions_module,     ONLY : uspp_calculation
   USE pseud_module,    ONLY : zp, alps, alpc, cc, aps, nlc, nnl, lmax, lloc, &
                              a_nlcc, b_nlcc, alpha_nlcc
   USE atom_module,     ONLY : mesh, xmin, dx, numeric, nlcc
   USE uspp_param,      ONLY : nqf, rinner, nqlc, nbeta, iver, lll, psd, tvanp
   USE spin_orb_module, ONLY : lspinorb
   USE funct_module
   
   IMPLICIT NONE
   PRIVATE

! 
! Print out all the informnatins obtained from the 
! input and initialization routines.
!
! output layout :
!
! *  general data
! *  calculation purposes
! *  input data
! *  DFT data
!    - lattice
!    - atoms
!    - kpoints
!    - eigenvalues
!
! contains:
! SUBROUTINE  summary(unit)
! </INFO>
!

   PUBLIC :: summary 
             
!
! end delcarations
!

   CONTAINS

!
! subroutines
!

!**********************************************************
   SUBROUTINE summary(unit,input)
   !**********************************************************
      IMPLICIT NONE
      INTEGER,   INTENT(in)         :: unit
      LOGICAL, OPTIONAL, INTENT(in) :: input     ! if true summarize the WanT input
      LOGICAL                :: linput
      INTEGER                :: ik, ia, ib, idnn 
      INTEGER                :: i, j, m, is, nt, l
      REAL(dbl), ALLOCATABLE :: kpt_cart(:,:)
      REAL(dbl), ALLOCATABLE :: center_cart1(:,:), center_cart2(:,:)
      INTEGER                :: ierr
      CHARACTER(5)           :: ps


   !
   ! <MAIN & INPUT> section
   !
      linput = .TRUE.
      IF ( PRESENT(input) ) linput = input
      IF ( linput ) THEN
          WRITE(unit,"()")      
          WRITE( unit, " (2x,70('='))" )
          WRITE( unit, " (2x,'=',32x,'Main',32x,'=')" )
          WRITE( unit, " (2x,70('='),/)" )
          WRITE(unit,"(  7x,'     Calculation Title :',5x,a)") TRIM(title)
          WRITE(unit,"(  7x,'                Prefix :',5x,a)") TRIM(prefix)
          WRITE(unit,"(  7x,'               Postfix :',5x,a)") TRIM(postfix)
          IF ( LEN_TRIM(work_dir) <= 65 ) THEN
             WRITE(unit,"(  7x,'     Working directory :',5x,a)") TRIM(work_dir)
          ELSE
             WRITE(unit,"(  7x,'     Working directory :',5x,/,10x,a)") TRIM(work_dir)
          ENDIF


          WRITE( unit, " ( /, '<WANNIER_FUNCTIONS>')" )
          WRITE(unit, " (2x,'Input parameters for Wannier func. calculation')")
          WRITE(unit, " (4x,'Number of Wannier functions required = ', i4 )" ) dimwann
          WRITE( unit,"(4x,'CG minim: Mixing parameter (alpha0_wan)= ', f6.3 )" ) alpha0_wan
          WRITE( unit,"(4x,'CG minim: Max iteration number = ', i5 )" ) maxiter0_wan
          WRITE( unit,"(4x,'Minimization convergence threshold = ', f15.9 )") wannier_thr
! XXX check whether it is true
          WRITE( unit,"(4x,'SD minim: Mixing parameter (alpha1_wan) = ', f6.3 )" ) alpha1_wan
          WRITE( unit,"(4x,'SD minim: Max iteration number = ', i5 )" ) maxiter1_wan
          WRITE( unit,"(4x,'Every ',i3,' iteration perform a CG minimization (ncg)')" ) ncg
          WRITE(unit, " (4x,'Number of k-point shells = ', i3 )" ) nshells
          WRITE(unit, " (4x,'Chosen shells (indexes) = ', 100i3 )" ) nwhich(1:nshells)
          WRITE(unit, " (4x,'Print each ', i3,' iterations' )" ) nprint
          WRITE(unit, " (4x,'Ordering mode = ', a )" ) TRIM(ordering_mode)
          WRITE( unit," (4x,'Verbosity = ', a )" ) TRIM(verbosity)
          WRITE( unit," (4x,'Unitariery check threshold = ', f15.9 )") unitary_thr
          WRITE( unit, " ( '</WANNIER_FUNCTIONS>',/)" )

          WRITE( unit, " ( '<DISENTANGLE>')" )
          WRITE(unit, " (2x,'Input parameters for subspace definition')")
          WRITE( unit,"(4x,'Spin component = ', a )" ) TRIM(spin_component)
          WRITE( unit,"(4x,'Mixing parameter (alpha_dis)= ', f6.3 )" ) alpha_dis
          WRITE( unit,"(4x,'Max iteration number = ', i5 )" ) maxiter_dis
          WRITE( unit,"(4x,'Starting guess orbitals (trial_mode) = ', a )" ) TRIM(trial_mode)
          WRITE( unit,"(4x,'Disentangle convergence threshold = ', f15.9 )") disentangle_thr
          WRITE( unit, " ( '</DISENTANGLE>',/)" )

          WRITE( unit, " ( '<TRIAL_CENTERS>')" )
          ALLOCATE( center_cart1(3,dimwann), center_cart2(3,dimwann), STAT=ierr )
             IF (ierr/=0) CALL errore('summary','allocating center_cart*',ABS(ierr))
          ! ... initialize with crystal coordinates and then convert
! XXX atomic case
          DO i = 1, dimwann
             center_cart1(:,i) = trial(i)%x1
             center_cart2(:,i) = trial(i)%x2
          ENDDO
          CALL cry2cart(center_cart1, avec )
          CALL cry2cart(center_cart2, avec )

          WRITE( unit, "(2x, 'Trial centers: (cart. coord. in Bohr)' ) " )
          DO i = 1, dimwann
              WRITE( unit, "(4x,'Center = ',i3,' Type = ',a,'  Center  = (',3F15.9,' )')")&
                     i, TRIM(trial(i)%type), center_cart1(:,i)
              IF  ( TRIM(trial(i)%type) == "2gauss" ) THEN
                  WRITE( unit, fmt="(26x,' Center2 = (', 3F10.6, ' ) ' )" ) &
                     center_cart2(:,i)
              ENDIF
          ENDDO
          DEALLOCATE( center_cart1, center_cart2, STAT=ierr )
             IF (ierr/=0) CALL errore('summary','deallocating center_cart*',ABS(ierr))
          WRITE( unit, " ( '</TRIAL_CENTERS>',/)" )

      ENDIF
      WRITE(unit,"()")      

   !
   ! <DFT> section
   !
      WRITE( unit, " (2x,70('='))" )
      WRITE( unit, " (2x,'=',30x,'DFT data',30x,'=')" )
      WRITE( unit, " (2x,70('='),/)" )

      !
      ! ... Lattice
      IF ( lattice_alloc ) THEN
          WRITE( unit, " (  '<LATTICE>')" )
          WRITE( unit, " (2x,'Alat  = ', F15.7, ' (Bohr)' )" ) alat
          WRITE( unit, " (2x,'Alat  = ', F15.7, ' (Ang )' )" ) alat * BOHR
          WRITE( unit, " (2x,'Omega = ', F15.7, ' (Bohr^3)' )" ) omega
          WRITE( unit, " (2x,'Omega = ', F15.7, ' (Ang^3 )',/ )" ) omega * BOHR**3
          WRITE( unit, " (2x, 'Crystal axes:' ) ")
          WRITE( unit, " (16x,'in units of Bohr',17x,'in Alat units' )")
          DO j=1,3
             WRITE ( unit, fmt="(4x,'a(',I1,') = (', 3F8.4, ' )     ( ',3F8.4, ' )'  )" ) &
                      j, ( avec(i,j), i=1,3 ), ( avec(i,j)/alat, i=1,3 )
          ENDDO
          !
          WRITE( unit, fmt= " (2x, 'Crystal axes: (Ang)' ) ")
          DO j=1,3
             WRITE ( unit, fmt="(4x,'a(',I1,') = (', 3F8.4, ' )'  )" ) &
                     j, ( avec(i,j)*BOHR , i=1,3 )
          ENDDO
          !
          WRITE( unit, " (/,2x, ' Reciprocal lattice vectors:' ) " )
          WRITE( unit, " (16x,'in units of Bohr^-1',14x,'in 2Pi/Alat units' )")
          DO j=1,3
             WRITE ( unit, fmt="(4x,'b(',I1,') = (', 3F8.4, ' )     ( ',3F8.4, ' )'  )" ) &
                      j, ( bvec(i,j), i=1,3 ), ( bvec(i,j)*alat / TPI, i=1,3 )
          ENDDO
          WRITE( unit, " (  '</LATTICE>',/)" )
      ENDIF

      !
      ! ... ions
      IF ( ions_alloc ) THEN 
          WRITE( unit, " (  '<IONS>')" )
          WRITE( unit, " (2x,'Number of chemical species =', i3 ) " ) nsp
          IF ( .NOT. do_pseudo )  THEN
             WRITE( unit, " (2x,'WARNING: Pseudopots not read, assumed to be norm cons.')") 
          ELSEIF ( uspp_calculation ) THEN
             WRITE( unit, " (2x,'Calculation is done within US pseudopot.',/)") 
          ENDIF
          DO is=1, nsp
             WRITE( unit, "(5x, 'Pseudo(',i2,') = ',a)") is, TRIM(psfile(is))
          ENDDO
          !
          !
          ! ... pseudo summary from Espresso
          !
          IF ( do_pseudo ) THEN 
             DO nt = 1, nsp
                IF (tvanp (nt) ) THEN
                   ps = '(US)'
                   WRITE( unit, '(/5x,"Pseudo(",i2,") is ",a2, &
                        &        1x,a5,"   zval =",f5.1,"   lmax=",i2, &
                        &        "   lloc=",i2)') nt, psd (nt) , ps, zp (nt) , lmax (nt) &
                        &, lloc (nt)
                   WRITE( unit, '(5x,"Version ", 3i3, " of US pseudo code")') &
                        (iver (i, nt) , i = 1, 3)
                   WRITE( unit, '(5x,"Using log mesh of ", i5, " points")') mesh (nt)
                   WRITE( unit, '(5x,"The pseudopotential has ",i2, &
                        &       " beta functions with: ")') nbeta (nt)
                   DO ib = 1, nbeta (nt)
                      WRITE( unit, '(15x," l(",i1,") = ",i3)') ib, lll (ib, nt)
                   ENDDO
                   WRITE( unit, '(5x,"Q(r) pseudized with ", &
                        &          i2," coefficients,  rinner = ",3f8.3,/ &
                        &          52x,3f8.3,/ &
                        &          52x,3f8.3)') nqf(nt), (rinner(i,nt), i=1,nqlc(nt) )
                ELSE
                   IF (nlc(nt) == 1 .AND. nnl(nt) == 1) THEN
                       ps = '(vbc)'
                   ELSEIF (nlc(nt) == 2 .AND. nnl(nt) == 3) THEN
                       ps = '(bhs)'
                   ELSEIF (nlc(nt) == 1 .AND. nnl(nt) == 3) THEN
                       ps = '(our)'
                   ELSE
                       ps = '     '
                   ENDIF
  
                   WRITE( unit, '(/5x,"Pseudo(",i2,") is ",a2, 1x,a5,"   zval =",f5.1,&
                        &      "   lmax=",i2,"   lloc=",i2)') &
                                   nt, psd(nt), ps, zp(nt), lmax(nt), lloc(nt)
                   IF (numeric (nt) ) THEN
                       WRITE( unit, '(5x,"(in numerical form: ",i5,&
                            &" grid points",", xmin = ",f5.2,", dx = ",f6.4,")")')&
                            & mesh (nt) , xmin (nt) , dx (nt)
                   ELSE
                       WRITE( unit, '(/14x,"i=",7x,"1",13x,"2",10x,"3")')
                       WRITE( unit, '(/5x,"core")')
                       WRITE( unit, '(5x,"alpha =",4x,3g13.5)') (alpc (i, nt) , i = 1, 2)
                       WRITE( unit, '(5x,"a(i)  =",4x,3g13.5)') (cc (i, nt) , i = 1, 2)
                       DO l = 0, lmax (nt)
                           WRITE( unit, '(/5x,"l = ",i2)') l
                           WRITE( unit, '(5x,"alpha =",4x,3g13.5)') (alps (i, l, nt), i=1,3)
                           WRITE( unit, '(5x,"a(i)  =",4x,3g13.5)') (aps (i, l, nt) , i=1,3)
                           WRITE( unit, '(5x,"a(i+3)=",4x,3g13.5)') (aps (i, l, nt) , i=4,6)
                       ENDDO
                       IF (nlcc(nt)) WRITE(unit,200) a_nlcc(nt), b_nlcc(nt), alpha_nlcc(nt)
                       200 FORMAT(/5x,'nonlinear core correction: ', &
                            &     'rho(r) = ( a + b r^2) exp(-alpha r^2)', &
                            & /,5x,'a    =',4x,g11.5, &
                            & /,5x,'b    =',4x,g11.5, &
                            & /,5x,'alpha=',4x,g11.5)
                   ENDIF
                ENDIF  ! PP type
             ENDDO     ! atomic species
          ENDIF        ! whether PP are read
          !
          ! ... end of pseudo summary from Espresso
          !
          !
          WRITE( unit, " (/,2x,'Atomic positions: (cart. coord. in units of alat)' ) " )
          DO ia = 1, nat
             WRITE( unit, "(5x, a, 2x,'tau( ',I3,' ) = (', 3F12.7, ' )' )" ) &
                      symb(ia), ia, (tau( i, ia ), i = 1, 3)
          ENDDO
          WRITE( unit, " (  '</IONS>',/)" )
      ENDIF
          
      !
      ! ... kpoints
      IF ( kpoints_alloc ) THEN 
          WRITE( unit, " (  '<K-POINTS>')" )
          WRITE( unit, "(2x, 'nkpts = ',i4 ) " ) nkpts
          WRITE( unit, "(2x, 'Monkhorst-Pack grid:      nk = (',3i3,' ),', &
                        & 6x,'shift = (',3i3,' )' ) " ) & 
                          nk(:), NINT( s(:) )
          WRITE( unit, "(2x, 'K-point calculation: (cart. coord. in Bohr^-1)' ) " )
          
          ALLOCATE( kpt_cart(3,nkpts), STAT=ierr)
             IF ( ierr/=0 ) CALL errore('summary','allocating kpt_cart',ABS(ierr))
          kpt_cart(:,:) = vkpt(:,:)
          CALL cry2cart( kpt_cart, bvec )

          DO ik=1,nkpts
             WRITE( unit, " (4x, 'k point', i4, ':   ( ',3f9.5,' ),   weight = ', f8.4 )") &
             ik, ( kpt_cart(i,ik), i=1,3 ), wk(ik)
          ENDDO
          DEALLOCATE( kpt_cart, STAT=ierr)
             IF ( ierr/=0 ) CALL errore('summary','deallocating kpt_cart',ABS(ierr))
          WRITE( unit, " (  '</K-POINTS>',/)" )
      ENDIF

      IF ( bshells_alloc ) THEN
          WRITE( unit, " (  '<B-SHELL>')" )
          WRITE( unit,"(2x, 'Nearest-neighbour shells for k-point 1: (in Bohr^-1)' ) " )
          DO i = 1, ndnntot
             WRITE( unit, "(4x, 'shell (',i3,' )    radius = ', f9.5 )")  i, dnn(i)
          ENDDO
          WRITE( unit,"()")
          WRITE( unit,"(2x,'Number of nearest-neighbours for K-point 1', &
                          &' (representetive for the BZ):')")
          DO i=1, nshells
              WRITE( unit, " (4x,'shell (', i3, ' )    neighbours  = ', i4 )")  &
                   nwhich(i), nnshell(1,nwhich(i))
          ENDDO
          !
          WRITE (unit, "(/,2x, 'List of the ' , i2, ' vectors b_k: (Bohr^-1) ') ") nntot(1)
          DO i = 1, nntot(1)
              WRITE(unit, " (4x, 'b_k', i4, ':   ( ',3f9.5, ' ),   weight = ',f8.4 )")&
                             i, ( bk(j,1,i), j=1,3 ), wb(1,i)
          ENDDO
          !
          WRITE (unit, "(/,2x, 'The ',i2, '  bk directions are (Bohr^-1):' )") nntot(1)/2
          DO i=1,nntot(1)/2
              WRITE(unit, "(4x,'dir',i2,':   ( ',3f9.5, ' ) ')" ) i, ( bka(j,i), j=1,3 )
          ENDDO
          WRITE( unit, " (  '</B-SHELL>',/)" )
      ENDIF
      !
      ! ... eigs and windows
      IF ( windows_alloc ) THEN 
          IF ( .NOT. kpoints_alloc ) CALL errore('summary','Unexpectedly kpts NOT alloc',1)
          WRITE( unit, " (  '<WINDOWS>')" )
          WRITE( unit," (2x, 'Definition of energy windows: (energies in eV)' ) " )
          WRITE( unit, " (4x, 'outer window: E  = ( ', f8.4, ' , ',f8.4, ' )' )" ) &
                              win_min, win_max
          WRITE( unit,"(4x,'Max number of bands within the energy window = ',i5)") dimwinx
          WRITE( unit,"(/,2x,'Electronic Structure from DFT calculation:')")
          WRITE( unit,"(  4x,'nkpts =',i4,',     ','nbnd =',i4,',')") nkpts, nbnd
          WRITE( unit,"(  4x,'nspin =',i4 ) " ) nspin
          WRITE( unit,"(  4x,'Fermi energy =',f15.9,' eV')") efermi
          DO ik=1,nkpts
              WRITE(unit, " (/,4x,'kpt =', i3, ' ( ',3f6.3,' )    dimwin = ', i4)" ) &
                              ik, vkpt(:,ik), dimwin(ik)
              WRITE(unit, " (41x,'imin = ', i4, '  imax = ', i4)" ) imin(ik), imax(ik)
              WRITE(unit, "(3x,'Eigenvalues:')"  )
              WRITE(unit,'(2x, 8f9.4)') ( eig(i,ik), i=1,nbnd )
          ENDDO

          IF ( .NOT. lfrozen ) THEN
               WRITE(unit," (/,4x,'inner window: NOT used --> NO FROZEN STATES' )" )
          ELSE
               WRITE(unit," (/,4x,'inner window: E  = (', f8.4, ' , ',f8.4, &
                      & ' ) --> FROZEN STATES' )" ) froz_min, froz_max
               DO ik=1,nkpts
                   WRITE(unit, "(4x, 'there are ', i3,' frozen states at k-point = ',i5)") &
                         dimfroz(ik), ik
               ENDDO
          ENDIF

          WRITE( unit, " (  '</WINDOWS>',/)" )
      ENDIF

  END SUBROUTINE summary

END MODULE summary_module

