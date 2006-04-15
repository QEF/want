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
   !
   ! print out a summary of both the input parameters and the 
   ! initialization procedure
   !
   USE kinds,             ONLY : dbl
   USE parameters,        ONLY : nstrx
   USE constants,         ONLY : PI, TPI, BOHR => bohr_radius_angs
   USE parser_module,     ONLY : log2char
   USE io_module,         ONLY : title, prefix, postfix, work_dir
   USE converters_module, ONLY : cart2cry
   USE control_module,    ONLY : ordering_mode, verbosity, restart_mode, & 
                                 use_pseudo, use_uspp, use_blimit, &
                                 do_overlaps, do_projections, do_collect_wf, &
                                 read_subspace, read_unitary, read_pseudo, &
                                 unitary_thr, subspace_init, localization_init, &
                                 nprint_dis, nprint_wan, nsave_dis, nsave_wan
   USE control_module,    ONLY : do_condmin
   USE trial_center_data_module, ONLY : trial
   USE lattice_module,    ONLY : lattice_alloc => alloc, avec, bvec, alat, omega
   USE ions_module,       ONLY : ions_alloc => alloc, nat, nsp, symb, tau, psfile
   USE kpoints_module,    ONLY : kpoints_alloc, nkpts, vkpt, wk, nk, s, &
                                 bshells_alloc, nb, vb, wb, wbtot
   USE windows_module,    ONLY : windows_alloc => alloc, dimwin, eig, efermi, nbnd, imin, imax,&
                                 dimfroz, lfrozen, dimwinx, nspin, spin_component, &
                                 win_min, win_max, froz_min, froz_max
   USE subspace_module,   ONLY : dimwann, disentangle_thr, alpha_dis, maxiter_dis
   USE localization_module, ONLY : alpha0_wan, alpha1_wan, maxiter0_wan, maxiter1_wan, ncg, &
                                 wannier_thr, a_condmin, niter_condmin, dump_condmin, xcell
   !
   ! pseudopotential modules
   USE pseud_module,      ONLY : zp, alps, alpc, cc, aps, nlc, nnl, lmax, lloc, &
                                 a_nlcc, b_nlcc, alpha_nlcc
   USE atom_module,       ONLY : mesh, xmin, dx, numeric, nlcc
   USE uspp_param,        ONLY : nqf, rinner, nqlc, nbeta, iver, lll, psd, tvanp
   USE spin_orb_module,   ONLY : lspinorb
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
! SUBROUTINE  summary(unit[,linput][,llattice][,latoms][,lpseudo][,lkpoints][,leig])
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
   SUBROUTINE summary(unit, linput, llattice, latoms, lpseudo, lkpoints, leig)
   !**********************************************************
   !
   ! manage init data summary
   !
   IMPLICIT NONE
      !
      ! input variables
      !
      INTEGER,           INTENT(in) :: unit
      LOGICAL, OPTIONAL, INTENT(in) :: linput     ! if TRUE summ input
      LOGICAL, OPTIONAL, INTENT(in) :: llattice   ! if TRUE summ lattice
      LOGICAL, OPTIONAL, INTENT(in) :: latoms     ! if TRUE summ atoms
      LOGICAL, OPTIONAL, INTENT(in) :: lpseudo    ! if TRUE summ pseudos
      LOGICAL, OPTIONAL, INTENT(in) :: lkpoints   ! if TRUE summ kpoints
      LOGICAL, OPTIONAL, INTENT(in) :: leig       ! if TRUE summ eigenvalues

      !
      ! local variables
      !
      LOGICAL                :: linput_
      LOGICAL                :: llattice_
      LOGICAL                :: latoms_
      LOGICAL                :: lpseudo_
      LOGICAL                :: lkpoints_
      LOGICAL                :: leig_
      LOGICAL                :: ldft_

      INTEGER                :: ik, ia, ib
      INTEGER                :: i, j, is, nt, l
      REAL(dbl), ALLOCATABLE :: center_cart1(:,:), center_cart2(:,:), tau_cry(:,:)
      INTEGER                :: ierr
      CHARACTER(5)           :: ps
      CHARACTER(2)           :: str


   !
   ! set defaults and switches
   !
      linput_   = .TRUE.
      llattice_ = .TRUE. 
      latoms_   = .TRUE. 
      lpseudo_  = .TRUE. 
      lkpoints_ = .TRUE. 
      leig_     = .TRUE. 
      IF ( PRESENT(linput) )   linput_   = linput
      IF ( PRESENT(llattice) ) llattice_ = llattice
      IF ( PRESENT(latoms) )   latoms_   = latoms
      IF ( PRESENT(lpseudo) )  lpseudo_  = lpseudo
      IF ( PRESENT(lkpoints) ) lkpoints_ = lkpoints
      IF ( PRESENT(leig) )     leig_     = leig
      ldft_ = llattice_ .OR. latoms_ .OR. lpseudo_ .OR. lkpoints_ .OR. leig_

   !
   ! <MAIN & INPUT> section
   !
      WRITE(unit,"()")      
      IF ( linput_ ) THEN
          WRITE(unit,"()")      
          WRITE(unit,"(2x,70('='))" )
          WRITE(unit,"(2x,'=',27x,'INPUT Summary',28x,'=')" )
          WRITE(unit,"(2x,70('='),/)" )
          WRITE(unit,"( ' <CONTROL>')" )
          WRITE(unit,"(   7x,'     Calculation title :',5x,   a)") TRIM(title)
          WRITE(unit,"(   7x,'                prefix :',5x,   a)") TRIM(prefix)
          WRITE(unit,"(   7x,'               postfix :',5x,   a)") TRIM(postfix)
          IF ( LEN_TRIM(work_dir) <= 65 ) THEN
             WRITE(unit,"(7x,'              work_dir :',5x,   a)") TRIM(work_dir)
          ELSE
             WRITE(unit,"(7x,'              work_dir :',5x,/,10x,a)") TRIM(work_dir)
          ENDIF
          WRITE(unit,"(   )")
          WRITE(unit,"(   7x,'           unitary_thr :',5x,e12.4)") unitary_thr
          WRITE(unit,"(   7x,'             verbosity :',5x,   a)") TRIM(verbosity)
          WRITE(unit,"(   7x,'          restart_mode :',5x,   a)") TRIM(restart_mode)
          WRITE(unit,"(   7x,'        Calc. overlaps :',5x,   a)") log2char(do_overlaps)
          WRITE(unit,"(   7x,'     Calc. projections :',5x,   a)") log2char(do_projections)
          WRITE(unit,"(   )")
          WRITE(unit,"(   7x,'    Read init subspace :',5x,   a)") log2char(read_subspace)
          WRITE(unit,"(   7x,'  Read init unit. mat. :',5x,   a)") log2char(read_unitary)
          WRITE(unit,"(   7x,'       Read pseudopot. :',5x,   a)") log2char(read_pseudo)
          WRITE(unit,"(   7x,'    Use penalty funct. :',5x,   a)") log2char(do_condmin)
          WRITE(unit,"( ' <CONTROL>',/)" )


          WRITE(unit," ( ' <SUBSPACE>')" )
          WRITE(unit,"(   7x,'               dimwann :',5x,  i8)") dimwann
          WRITE(unit,"(   7x,'         subspace_init :',5x,   a)") TRIM(subspace_init)
          WRITE(unit,"(   7x,'        spin_component :',5x,   a)") TRIM(spin_component)
          WRITE(unit,"(   7x,'       disentangle_thr :',5x,e12.4)") disentangle_thr
          !
          IF ( win_min > -10000.0 ) &
             WRITE(unit,"(7x,'               win_min :',5x,f8.4)") win_min
          IF ( win_max <  10000.0 ) &
             WRITE(unit,"(7x,'               win_max :',5x,f8.4)") win_max
          IF ( froz_min > -10000.0 ) &
             WRITE(unit,"(7x,'              froz_min :',5x,f8.4)") froz_min
          IF ( froz_max <  10000.0 ) &
             WRITE(unit,"(7x,'              froz_max :',5x,f8.4)") froz_max
             !
          WRITE(unit,"(   7x,'             alpha_dis :',5x,f8.4)") alpha_dis
          WRITE(unit,"(   7x,'           maxiter_dis :',5x,  i8)") maxiter_dis
          WRITE(unit,"(   7x,'            nprint_dis :',5x,  i8)") nprint_dis
          WRITE(unit,"(   7x,'             nsave_dis :',5x,  i8)") nsave_dis
          IF ( use_blimit ) &
             WRITE(unit,"(/,7x,'WARNING     use_blimit :',5x, a)") log2char(use_blimit)
          WRITE(unit, " ( ' </SUBSPACE>',/)" )


          WRITE(unit,"( ' <LOCALIZATION>')" )
          WRITE(unit,"(   7x,'     localization_init :',5x,   a)") TRIM(localization_init)
          WRITE(unit,"(   7x,'         ordering_mode :',5x,   a)") TRIM(ordering_mode)
          WRITE(unit,"(   7x,'            collect_wf :',5x,   a)") log2char(do_collect_wf)
          IF ( do_collect_wf ) THEN
             WRITE(unit,"(7x,'              xcell(1) :',5x,f8.4)") xcell(1)
             WRITE(unit,"(7x,'              xcell(2) :',5x,f8.4)") xcell(2)
             WRITE(unit,"(7x,'              xcell(3) :',5x,f8.4)") xcell(3)
          ENDIF
          WRITE(unit,"(   7x,'           wannier_thr :',5x,e12.4)") wannier_thr
          WRITE(unit,"(   7x,'            alpha0_wan :',5x,f8.4)") alpha0_wan
          WRITE(unit,"(   7x,'            alpha1_wan :',5x,f8.4)") alpha1_wan
          WRITE(unit,"(   7x,'          maxiter0_wan :',5x,  i8)") maxiter0_wan
          WRITE(unit,"(   7x,'          maxiter1_wan :',5x,  i8)") maxiter1_wan
          WRITE(unit,"(   7x,'                   ncg :',5x,  i8)") ncg
          WRITE(unit,"(   7x,'            nprint_wan :',5x,  i8)") nprint_wan
          WRITE(unit,"(   7x,'             nsave_wan :',5x,  i8)") nsave_wan
          IF ( do_condmin ) THEN
             WRITE(unit,"(7x,'             a_condmin :',5x,f8.4)") a_condmin
             WRITE(unit,"(7x,'          dump_condmin :',5x,f8.4)") dump_condmin
             WRITE(unit,"(7x,'         niter_condmin :',5x,  i8)") niter_condmin
          ENDIF
          WRITE(unit,"( ' </LOCALIZATION>',/)" )


          WRITE(unit, " ( ' <TRIAL_CENTERS>')" )
          ALLOCATE( center_cart1(3,dimwann), center_cart2(3,dimwann), STAT=ierr )
             IF (ierr/=0) CALL errore('summary','allocating center_cart*',ABS(ierr))
          ! ... initialize with crystal coordinates and then convert
          DO i = 1, dimwann
             center_cart1(:,i) = trial(i)%x1
             center_cart2(:,i) = trial(i)%x2
          ENDDO

          WRITE(unit, '(2x, "Trial centers: (cart. coord. in Bohr)" ) ' )
          WRITE(unit, '(/,6x,"#",5x,"Type",10x,"l",3x,"m",7x,"Position",30x,"Decay" )')
          WRITE(unit, '(4x, 4("-"), 2x, 12("-"), 2x, 8("-"), 3x, 36("-"),3x,9("-"))')
          DO i = 1, dimwann
              str = "  "
              IF ( TRIM(trial(i)%type) == 'atomic' ) str = symb(trial(i)%iatom)
              WRITE(unit, "(4x,i3,4x,a6, 2x,a2, 3x, i3,1x,i3,4x,'(',3F11.6,' )',2x,f9.5)") &
                        i, TRIM(trial(i)%type), str, trial(i)%l, trial(i)%m,  &
                        center_cart1(:,i), trial(i)%decay
              IF  ( TRIM(trial(i)%type) == "2gauss" ) THEN
                    WRITE(unit, "(35x,'(',3F11.6,' )')") center_cart2(:,i)
              ENDIF
          ENDDO
          DEALLOCATE( center_cart1, center_cart2, STAT=ierr )
             IF (ierr/=0) CALL errore('summary','deallocating center_cart*',ABS(ierr))
          WRITE(unit, " (/, ' </TRIAL_CENTERS>',/)" )

      ENDIF
      WRITE(unit,"()")      

   !
   ! <DFT> section
   !
      IF ( ldft_ ) THEN 
          WRITE(unit, " (2x,70('='))" )
          WRITE(unit, " (2x,'=',30x,'DFT data',30x,'=')" )
          WRITE(unit, " (2x,70('='),/)" )
      ENDIF

      !
      ! ... Lattice
      IF ( lattice_alloc .AND. llattice_ ) THEN
          WRITE(unit, " (  ' <LATTICE>')" )
          WRITE(unit, " (2x,'Alat  = ', F15.7, ' (Bohr)' )" ) alat
          WRITE(unit, " (2x,'Alat  = ', F15.7, ' (Ang )' )" ) alat * BOHR
          WRITE(unit, " (2x,'Omega = ', F15.7, ' (Bohr^3)' )" ) omega
          WRITE(unit, " (2x,'Omega = ', F15.7, ' (Ang^3 )',/ )" ) omega * BOHR**3
          WRITE(unit, " (2x, 'Crystal axes:' ) ")
          WRITE(unit, " (16x,'in units of Bohr',17x,'in Alat units' )")
          DO j=1,3
             WRITE (unit, fmt="(4x,'a(',I1,') = (', 3F8.4, ' )     ( ',3F8.4, ' )'  )" ) &
                      j, ( avec(i,j), i=1,3 ), ( avec(i,j)/alat, i=1,3 )
          ENDDO
          !
          WRITE(unit, fmt= " (2x, 'Crystal axes: (Ang)' ) ")
          DO j=1,3
             WRITE (unit, fmt="(4x,'a(',I1,') = (', 3F8.4, ' )'  )" ) &
                     j, ( avec(i,j)*BOHR , i=1,3 )
          ENDDO
          !
          WRITE(unit, " (/,2x, ' Reciprocal lattice vectors:' ) " )
          WRITE(unit, " (16x,'in units of Bohr^-1',14x,'in 2Pi/Alat units' )")
          DO j=1,3
             WRITE (unit, fmt="(4x,'b(',I1,') = (', 3F8.4, ' )     ( ',3F8.4, ' )'  )" ) &
                      j, ( bvec(i,j), i=1,3 ), ( bvec(i,j)*alat / TPI, i=1,3 )
          ENDDO
          WRITE(unit, " (  ' </LATTICE>',/)" )
      ENDIF

      !
      ! ... ions
      IF ( ions_alloc .AND. latoms_ ) THEN 
          WRITE(unit, " (  ' <IONS>')" )
          WRITE(unit, " (2x,'Number of chemical species =', i3 ) " ) nsp

          IF ( lpseudo_ ) THEN
              !
              IF ( .NOT. use_pseudo )  THEN
                  CALL warning('Pseudopots not read, assumed to be norm cons.')
              ELSEIF ( use_uspp ) THEN
                  WRITE(unit, " (2x,'Calculation is done within US pseudopot.',/)") 
              ENDIF
              !
              DO is=1, nsp
                  WRITE(unit, "(5x, 'Pseudo(',i2,') = ',a)") is, TRIM(psfile(is))
              ENDDO
              !
              !
              ! ... pseudo summary from Espresso
              !
              IF ( use_pseudo ) THEN 
                  DO nt = 1, nsp
                    IF (tvanp (nt) ) THEN
                       ps = '(US)'
                       WRITE(unit, '(/5x,"Pseudo(",i2,") is ",a2, &
                            &        1x,a5,"   zval =",f5.1,"   lmax=",i2, &
                            &        "   lloc=",i2)') nt, psd (nt) , ps, zp (nt) , lmax (nt) &
                            &, lloc (nt)
                       WRITE(unit, '(5x,"Version ", 3i3, " of US pseudo code")') &
                            (iver (i, nt) , i = 1, 3)
                       WRITE(unit, '(5x,"Using log mesh of ", i5, " points")') mesh (nt)
                       WRITE(unit, '(5x,"The pseudopotential has ",i2, &
                            &       " beta functions with: ")') nbeta (nt)
                       DO ib = 1, nbeta (nt)
                          WRITE(unit, '(15x," l(",i1,") = ",i3)') ib, lll (ib, nt)
                       ENDDO
                       WRITE(unit, '(5x,"Q(r) pseudized with ", &
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
      
                       WRITE(unit, '(/5x,"Pseudo(",i2,") is ",a2, 1x,a5,"   zval =",f5.1,&
                            &      "   lmax=",i2,"   lloc=",i2)') &
                                       nt, psd(nt), ps, zp(nt), lmax(nt), lloc(nt)
                       IF (numeric (nt) ) THEN
                           WRITE(unit, '(5x,"(in numerical form: ",i5,&
                                &" grid points",", xmin = ",f5.2,", dx = ",f6.4,")")')&
                                & mesh (nt) , xmin (nt) , dx (nt)
                       ELSE
                           WRITE(unit, '(/14x,"i=",7x,"1",13x,"2",10x,"3")')
                           WRITE(unit, '(/5x,"core")')
                           WRITE(unit, '(5x,"alpha =",4x,3g13.5)') (alpc (i, nt) , i = 1, 2)
                           WRITE(unit, '(5x,"a(i)  =",4x,3g13.5)') (cc (i, nt) , i = 1, 2)
                           DO l = 0, lmax (nt)
                               WRITE(unit,'(/5x,"l = ",i2)') l
                               WRITE(unit,'(5x,"alpha =",4x,3g13.5)') (alps (i, l, nt), i=1,3)
                               WRITE(unit,'(5x,"a(i)  =",4x,3g13.5)') (aps (i, l, nt) , i=1,3)
                               WRITE(unit,'(5x,"a(i+3)=",4x,3g13.5)') (aps (i, l, nt) , i=4,6)
                           ENDDO
                           IF (nlcc(nt)) WRITE(unit,20) a_nlcc(nt), b_nlcc(nt), alpha_nlcc(nt)
                           20 FORMAT(/5x,'nonlinear core correction: ', &
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
          ENDIF  ! lpseudo

          WRITE(unit, " (/,2x,'Atomic positions: (cart. coord. in Bohr)' ) " )
          DO ia = 1, nat
             WRITE(unit, "(5x, a, 2x,'tau( ',I3,' ) = (', 3F12.7, ' )' )" ) &
                      symb(ia), ia, (tau( i, ia )*alat, i = 1, 3)
          ENDDO
          !
          IF ( TRIM(verbosity) == 'high') THEN
              !
              ALLOCATE( tau_cry(3,nat), STAT=ierr )
              IF (ierr/=0) CALL errore('summary','allocating tau_cry',ABS(ierr))
              !
              tau_cry(:,:) = tau(:,:)
              CALL cart2cry(tau_cry, avec)
              !
              WRITE(unit, " (/,2x,'Atomic positions: (crystal coord.)' ) " )
              DO ia = 1, nat
                  WRITE(unit, "(5x, a, 2x,'tau( ',I3,' ) = (', 3F12.7, ' )' )" ) &
                         symb(ia), ia, (tau_cry( i, ia ), i = 1, 3)
              ENDDO
              !
              DEALLOCATE( tau_cry, STAT=ierr )
              IF (ierr/=0) CALL errore('summary','deallocating tau_cry',ABS(ierr))
              !
          ENDIF
               
          WRITE(unit, " (  ' </IONS>',/)" )
      ENDIF
          
      !
      ! ... kpoints
      IF ( kpoints_alloc .AND. lkpoints_ ) THEN 
          WRITE(unit, " (  ' <K-POINTS>')" )
          WRITE(unit, "(2x, 'nkpts = ',i4 ) " ) nkpts
          WRITE(unit, "(2x, 'Monkhorst-Pack grid:      nk = (',3i3,' ),', &
                       & 6x,'shift = (',3i3,' )' ) " ) & 
                         nk(:), s(:) 
          WRITE(unit, "(2x, 'K-point calculation: (cart. coord. in Bohr^-1)' ) " )
          

          DO ik=1,nkpts
             WRITE(unit, " (4x, 'k point', i4, ':   ( ',3f9.5,' ),   weight = ', f11.7 )") &
             ik, ( vkpt(i,ik), i=1,3 ), wk(ik)
          ENDDO
          WRITE(unit, " (  ' </K-POINTS>',/)" )
      ENDIF

      IF ( bshells_alloc .AND. lkpoints_ ) THEN
          WRITE(unit, " (  ' <B-SHELL>')" )
          !
          WRITE (unit, "(2x, 'List of the ' , i2, ' b-vectors : (Bohr^-1) ') ") nb
          DO i = 1, nb
              WRITE(unit, " (4x, 'b (', i2, ') =    ( ',3f9.5, ' ),   weight = ',f11.7 )")&
                             i, ( vb(j,i), j=1,3 ), wb(i)
          ENDDO
          !
          WRITE(unit, " (/,2x, 'Total weight = ' , f15.7) ") wbtot
          WRITE(unit, " (  ' </B-SHELL>',/)" )
      ENDIF
      !
      ! ... eigs and windows
      IF ( windows_alloc .AND. leig_ ) THEN 
          IF ( .NOT. kpoints_alloc ) CALL errore('summary','Unexpectedly kpts NOT alloc',1)
          WRITE(unit, " (  ' <WINDOWS>')" )
          WRITE(unit," (2x, 'Definition of energy windows: (energies in eV)' ) " )
          IF ( win_min < 10000.0 .AND. win_max > 10000.0 ) THEN
              WRITE(unit, " (4x, 'outer window: E  = (  -inf ,  inf  )' )" ) 
          ELSEIF ( win_min < 10000.0 ) THEN
              WRITE(unit, " (4x, 'outer window: E  = (  -inf , ',f9.4, ' )' )" ) &
                                  win_max
          ELSEIF ( win_max > 10000.0 ) THEN
              WRITE(unit, " (4x, 'outer window: E  = ( ', f9.4, ' ,  inf  )' )" ) &
                                   win_min
          ELSE  
             ! std case
             WRITE(unit, " (4x, 'outer window: E  = ( ', f9.4, ' , ',f9.4, ' )' )" ) &
                                  win_min, win_max
          ENDIF
          WRITE(unit,"(4x,'Max number of bands within the energy window = ',i5)") dimwinx
          WRITE(unit,"(/,2x,'Electronic Structure from DFT calculation:')")
          WRITE(unit,"(  4x,'nkpts =',i4,',     ','nbnd =',i4,',')") nkpts, nbnd
          WRITE(unit,"(  4x,'nspin =',i4 ) " ) nspin
          WRITE(unit,"(  4x,'Fermi energy =',f15.9,' eV')") efermi

          IF ( TRIM(verbosity) == "medium" .OR. TRIM(verbosity) == "high" ) THEN
              DO ik=1,nkpts
                  WRITE(unit,"(1x,'!')")
                  WRITE(unit,"(1x,'!',4x,'kpt = ',i4,' ( ',3f9.5,' )    dimwin = ',i4)") &
                               ik, vkpt(:,ik), dimwin(ik)
                  WRITE(unit,"(1x,'!',39x,'imin = ',i4,'  imax = ',i4)") imin(ik), imax(ik)
                  WRITE(unit,"(1x,'!',3x,'Eigenvalues:')"  )
                  WRITE(unit,"(1x,'!',2x, 8f9.4)") ( eig(i,ik), i=1,nbnd )
              ENDDO
          ENDIF

          IF ( .NOT. lfrozen ) THEN
               WRITE(unit," (/,4x,'inner window: NOT used --> NO FROZEN STATES' )" )
          ELSE
               IF ( froz_min < 10000.0 ) THEN
                   WRITE(unit," (/,4x,'inner window: E  = (  -inf , ',f8.4, &
                      & ' ) --> FROZEN STATES' )" ) froz_max
               ELSE
                   WRITE(unit," (/,4x,'inner window: E  = (', f8.4, ' , ',f8.4, &
                      & ' ) --> FROZEN STATES' )" ) froz_min, froz_max
               ENDIF
               DO ik=1,nkpts
                   WRITE(unit, "(4x, 'there are ', i3,' frozen states at k-point = ',i5)") &
                         dimfroz(ik), ik
               ENDDO
          ENDIF

          WRITE(unit, " (  ' </WINDOWS>',/)" )
      ENDIF

  END SUBROUTINE summary

END MODULE summary_module

