!
! Copyright (C) 2004 WanT Group
!
! This file is distributed under the terms of the
! GNU General Public License. See the file `License\'
! in the root directory of the present distribution,
! or http://www.gnu.org/copyleft/gpl.txt .
!
!**********************************************************
   MODULE summary_module
   !**********************************************************
   !
   IMPLICIT NONE
   PRIVATE
   !
   PUBLIC :: summary
   ! 
CONTAINS
!
!**********************************************************
   SUBROUTINE summary(iunit, input,   lattice, ions,    windows, symmetry, &
                             kpoints, rgrid,   bshells, pseudo,  memory )
   !**********************************************************
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
   !    - ions
   !    - symmetry
   !    - kpoints and bshells
   !    - rgrid
   !    - windows (eigenvalues)
   !    - pseudo
   !    - memory usage
   !
   USE kinds,             ONLY : dbl
   USE parameters,        ONLY : nstrx
   USE constants,         ONLY : ZERO, PI, TPI, BOHR => bohr_radius_angs
   USE parser_module,     ONLY : log2char
   USE io_module,         ONLY : title, prefix, postfix, work_dir, dftdata_fmt, &
                                 dftdata_fmt_version, wantdata_fmt, ionode
   USE mp_global,         ONLY : nproc
   USE log_module,        ONLY : log_push, log_pop
   USE converters_module, ONLY : cart2cry
   USE control_module,    ONLY : ordering_mode, verbosity, restart_mode, & 
                                 use_pseudo, use_uspp, use_blimit, &
                                 do_overlaps, do_projections, do_collect_wf, &
                                 read_subspace, read_unitary, read_pseudo, &
                                 unitary_thr, subspace_init, localization_init, &
                                 nprint_dis, nprint_wan, nsave_dis, nsave_wan, &
                                 use_debug_mode, debug_level
   USE control_module,    ONLY : use_condmin
   USE trial_center_data_module, ONLY : trial
   USE lattice_module,    ONLY : lattice_alloc => alloc, avec, bvec, alat, omega
   USE ions_module,       ONLY : ions_alloc => alloc, nat, nsp, symb, tau, psfile
   USE symmetry_module,   ONLY : symmetry_alloc => alloc, nsym, srot, strasl, sname, &
                                 symmetry_write
   USE kpoints_module,    ONLY : kpoints_alloc, nkpts, nkpts_g, vkpt_g, wk_g, &
                                 nk, s, bshells_alloc, nb, vb, wb, wbtot, &
                                 rgrid_alloc, nrtot, nr, ivr, wr
   USE windows_module,    ONLY : windows_alloc => alloc, dimwin, eig, efermi, nbnd, &
                                 imin, imax, dimfroz, lfrozen, dimwinx, nspin, &
                                 spin_component, win_min, win_max, froz_min, froz_max, &
                                 iwin_min, iwin_max, ifroz_min, ifroz_max
   USE subspace_module,   ONLY : dimwann, disentangle_thr, alpha_dis, maxiter_dis
   USE localization_module, ONLY : alpha0_wan, alpha1_wan, maxiter0_wan, maxiter1_wan, ncg, &
                                 wannier_thr, a_condmin, niter_condmin, dump_condmin, xcell
   ! 
   IMPLICIT NONE


   !
   ! input variables
   !
   INTEGER,           INTENT(IN) :: iunit
   LOGICAL, OPTIONAL, INTENT(IN) :: input     ! if TRUE summ input
   LOGICAL, OPTIONAL, INTENT(IN) :: lattice   ! if TRUE summ lattice
   LOGICAL, OPTIONAL, INTENT(IN) :: ions      ! if TRUE summ ions
   LOGICAL, OPTIONAL, INTENT(IN) :: windows   ! if TRUE summ eigenvalues (windows)
   LOGICAL, OPTIONAL, INTENT(IN) :: symmetry  ! if TRUE summ symmetries
   LOGICAL, OPTIONAL, INTENT(IN) :: kpoints   ! if TRUE summ kpoints
   LOGICAL, OPTIONAL, INTENT(IN) :: rgrid     ! if TRUE summ kpoints
   LOGICAL, OPTIONAL, INTENT(IN) :: bshells   ! if TRUE summ bshells
   LOGICAL, OPTIONAL, INTENT(IN) :: pseudo    ! if TRUE summ pseudos
   LOGICAL, OPTIONAL, INTENT(IN) :: memory    ! if TRUE summ memory usage

   !
   ! local variables
   !
   LOGICAL                :: linput
   LOGICAL                :: llattice
   LOGICAL                :: lions
   LOGICAL                :: lwindows
   LOGICAL                :: lsymmetry
   LOGICAL                :: lkpoints
   LOGICAL                :: lrgrid
   LOGICAL                :: lbshells
   LOGICAL                :: lpseudo
   LOGICAL                :: lmemory
   LOGICAL                :: ldft

   INTEGER                :: ir, ik, ia
   INTEGER                :: i, j, is, nt, isym
   REAL(dbl), ALLOCATABLE :: center_cart1(:,:), center_cart2(:,:)
   REAL(dbl), ALLOCATABLE :: vkpt_cry(:,:), tau_cry(:,:)
   INTEGER                :: ierr
   CHARACTER(2)           :: str
   !
   ! end of declaration
   !

!
!------------------------------
! main body
!------------------------------
!
   CALL log_push( 'summary' )
   
   !
   ! set defaults and switches
   !
   linput    = .TRUE.
   llattice  = .TRUE. 
   lions     = .TRUE. 
   lwindows  = .TRUE. 
   lsymmetry = .TRUE. 
   lkpoints  = .TRUE. 
   lrgrid    = .TRUE. 
   lpseudo   = .TRUE. 
   lmemory   = .FALSE. 
   !
   IF ( PRESENT(input) )    linput    = input
   IF ( PRESENT(lattice) )  llattice  = lattice
   IF ( PRESENT(ions) )     lions     = ions
   IF ( PRESENT(windows) )  lwindows  = windows
   IF ( PRESENT(symmetry) ) lsymmetry = symmetry
   IF ( PRESENT(kpoints) )  lkpoints  = kpoints
   IF ( PRESENT(rgrid) )    lrgrid    = rgrid
   lbshells  = lkpoints
   IF ( PRESENT(bshells) )  lbshells  = bshells
   IF ( PRESENT(pseudo) )   lpseudo   = pseudo
   IF ( PRESENT(memory) )   lmemory   = memory
   !
   ldft  = llattice .OR. lions .OR. lwindows .OR. lpseudo .OR. lkpoints .OR. lbshells

!
! <MAIN & INPUT> section
!

   !
   IF (ionode) WRITE(iunit,"()" )
   !
   IF ( linput .AND. ionode ) THEN
       !
       CALL write_header( iunit, "INPUT Summary" )
       !
       WRITE(iunit,"( ' <CONTROL>')" )
       WRITE(iunit,"(   7x,'     Calculation title :',5x,   a)") TRIM(title)
       WRITE(iunit,"(   7x,'                prefix :',5x,   a)") TRIM(prefix)
       WRITE(iunit,"(   7x,'               postfix :',5x,   a)") TRIM(postfix)
       IF ( LEN_TRIM(work_dir) <= 65 ) THEN
          WRITE(iunit,"(7x,'              work_dir :',5x,   a)") TRIM(work_dir)
       ELSE
          WRITE(iunit,"(7x,'              work_dir :',5x,/,10x,a)") TRIM(work_dir)
       ENDIF
       WRITE(iunit,"(   7x,'           dftdata_fmt :',5x,   a)") TRIM(dftdata_fmt) //  &
                                                                "  v" // TRIM( dftdata_fmt_version)
       WRITE(iunit,"(   )")
       WRITE(iunit,"(   7x,'          wantdata_fmt :',5x,   a)") TRIM(wantdata_fmt)
       WRITE(iunit,"(   7x,'             verbosity :',5x,   a)") TRIM(verbosity)
       WRITE(iunit,"(   7x,'          restart_mode :',5x,   a)") TRIM(restart_mode)
       WRITE(iunit,"(   7x,'           unitary_thr :',5x,e12.4)") unitary_thr
       WRITE(iunit,"(   7x,'        Calc. overlaps :',5x,   a)") log2char(do_overlaps)
       WRITE(iunit,"(   7x,'     Calc. projections :',5x,   a)") log2char(do_projections)
       WRITE(iunit,"(   )")
       WRITE(iunit,"(   7x,'    Read init subspace :',5x,   a)") log2char(read_subspace)
       WRITE(iunit,"(   7x,'  Read init unit. mat. :',5x,   a)") log2char(read_unitary)
       WRITE(iunit,"(   7x,'       Read pseudopot. :',5x,   a)") log2char(read_pseudo)
       WRITE(iunit,"(   7x,'    Use penalty funct. :',5x,   a)") log2char(use_condmin)
       WRITE(iunit,"(   )")
       WRITE(iunit,"(   7x,'        Use debug mode :',5x,   a)") log2char(use_debug_mode)
       IF ( use_debug_mode ) THEN
          WRITE(iunit,"(7x,'           debug_level :',5x,  i3)") debug_level
       ENDIF
       WRITE(iunit,"( ' <CONTROL>',/)" )


       WRITE(iunit," ( ' <SUBSPACE>')" )
       WRITE(iunit,"(   7x,'               dimwann :',5x,  i8)") dimwann
       WRITE(iunit,"(   7x,'         subspace_init :',5x,   a)") TRIM(subspace_init)
       WRITE(iunit,"(   7x,'        spin_component :',5x,   a)") TRIM(spin_component)
       WRITE(iunit,"(   7x,'       disentangle_thr :',5x,e12.4)") disentangle_thr
       !
       IF ( win_min > -10000.0 ) &
          WRITE(iunit,"(7x,'               win_min :',1x,f12.4)") win_min
       IF ( win_max <  10000.0 ) &
          WRITE(iunit,"(7x,'               win_max :',1x,f12.4)") win_max
       IF ( froz_min > -10000.0 ) &
          WRITE(iunit,"(7x,'              froz_min :',1x,f12.4)") froz_min
       IF ( froz_max > -10000.0 ) &
          WRITE(iunit,"(7x,'              froz_max :',1x,f12.4)") froz_max
          !
       WRITE(iunit,"(   7x,'             alpha_dis :',1x,f12.4)") alpha_dis
       WRITE(iunit,"(   7x,'           maxiter_dis :',5x,   i8)") maxiter_dis
       WRITE(iunit,"(   7x,'            nprint_dis :',5x,   i8)") nprint_dis
       WRITE(iunit,"(   7x,'             nsave_dis :',5x,   i8)") nsave_dis
       IF ( use_blimit ) &
          WRITE(iunit,"(/,7x,'WARNING     use_blimit :',5x, a)") log2char(use_blimit)
       WRITE(iunit, " ( ' </SUBSPACE>',/)" )


       WRITE(iunit,"( ' <LOCALIZATION>')" )
       WRITE(iunit,"(   7x,'     localization_init :',5x,   a)") TRIM(localization_init)
       WRITE(iunit,"(   7x,'         ordering_mode :',5x,   a)") TRIM(ordering_mode)
       WRITE(iunit,"(   7x,'            collect_wf :',5x,   a)") log2char(do_collect_wf)
       IF ( do_collect_wf ) THEN
          WRITE(iunit,"(7x,'              xcell(1) :',1x,f12.4)") xcell(1)
          WRITE(iunit,"(7x,'              xcell(2) :',1x,f12.4)") xcell(2)
          WRITE(iunit,"(7x,'              xcell(3) :',1x,f12.4)") xcell(3)
       ENDIF
       WRITE(iunit,"(   7x,'           wannier_thr :',5x,e12.4)") wannier_thr
       WRITE(iunit,"(   7x,'            alpha0_wan :',1x,f12.4)") alpha0_wan
       WRITE(iunit,"(   7x,'            alpha1_wan :',1x,f12.4)") alpha1_wan
       WRITE(iunit,"(   7x,'          maxiter0_wan :',5x,  i8)") maxiter0_wan
       WRITE(iunit,"(   7x,'          maxiter1_wan :',5x,  i8)") maxiter1_wan
       WRITE(iunit,"(   7x,'                   ncg :',5x,  i8)") ncg
       WRITE(iunit,"(   7x,'            nprint_wan :',5x,  i8)") nprint_wan
       WRITE(iunit,"(   7x,'             nsave_wan :',5x,  i8)") nsave_wan
       IF ( use_condmin ) THEN
          WRITE(iunit,"(7x,'             a_condmin :',1x,f12.4)") a_condmin
          WRITE(iunit,"(7x,'          dump_condmin :',1x,f12.4)") dump_condmin
          WRITE(iunit,"(7x,'         niter_condmin :',5x,  i8)") niter_condmin
       ENDIF
       WRITE(iunit,"( ' </LOCALIZATION>',/)" )


       WRITE(iunit, " ( ' <TRIAL_CENTERS>')" )
       ALLOCATE( center_cart1(3,dimwann), center_cart2(3,dimwann), STAT=ierr )
          IF (ierr/=0) CALL errore('summary','allocating center_cart*',ABS(ierr))
       ! ... initialize with crystal coordinates and then convert
       DO i = 1, dimwann
          center_cart1(:,i) = trial(i)%x1
          center_cart2(:,i) = trial(i)%x2
       ENDDO

       WRITE(iunit, '(2x, "Trial centers: (cart. coord. in Bohr)" ) ' )
       IF ( use_condmin ) THEN
           WRITE(iunit, '(/,6x,"#",4x,"Type",8x,"l",3x,"m",4x,"Position",29x,"Decay",4x,"Weight")')
           WRITE(iunit, '(4x, 4("-"), 1x, 11("-"), 1x, 8("-"), 1x, 36("-"),1x,9("-"),1x,7("-"))')
           DO i = 1, dimwann
              str = "  "
              IF ( TRIM(trial(i)%type) == 'atomic' ) str = TRIM(symb(trial(i)%iatom))
              WRITE(iunit, "(4x,i3,3x,a6, 2x,a2,1x,i3,1x,i3,3x,'(',3f11.6,' )',f9.5,1x,f7.3)") &
                     i, TRIM(trial(i)%type), str, trial(i)%l, trial(i)%m,  &
                     center_cart1(:,i), trial(i)%decay, trial(i)%weight
              IF ( TRIM(trial(i)%type) == "2gauss" ) THEN
                 WRITE(iunit, "(31x,'(',3f11.6,' )')") center_cart2(:,i)
              ENDIF
           ENDDO
       ELSE
           WRITE(iunit, '(/,6x,"#",5x,"Type",10x,"l",3x,"m",7x,"Position",30x,"Decay" )')
           WRITE(iunit, '(4x, 4("-"), 2x, 12("-"), 2x, 8("-"), 3x, 36("-"),3x,9("-"))')
           DO i = 1, dimwann
              str = "  "
              IF ( TRIM(trial(i)%type) == 'atomic' ) str = TRIM(symb(trial(i)%iatom))
              WRITE(iunit, "(4x,i3,4x,a6, 2x,a2, 3x, i3,1x,i3,4x,'(',3f11.6,' )',2x,f9.5)") &
                     i, TRIM(trial(i)%type), str, trial(i)%l, trial(i)%m,  &
                     center_cart1(:,i), trial(i)%decay
              IF ( TRIM(trial(i)%type) == "2gauss" ) THEN
                 WRITE(iunit, "(35x,'(',3f11.6,' )')") center_cart2(:,i)
              ENDIF
           ENDDO
       ENDIF
       DEALLOCATE( center_cart1, center_cart2, STAT=ierr )
          IF (ierr/=0) CALL errore('summary','deallocating center_cart*',ABS(ierr))
       WRITE(iunit, " (/, ' </TRIAL_CENTERS>',/)" )

   ENDIF

!
! <DFT> section
!
   IF ( ldft .AND. ionode ) THEN
       ! 
       CALL write_header( iunit, "DFT data" )
       !
   ENDIF

   !
   ! ... Lattice
   IF ( lattice_alloc .AND. llattice .AND. ionode ) THEN
       !
       WRITE(iunit, " (  ' <LATTICE>')" )
       WRITE(iunit, " (2x,'Alat  = ', F15.7, ' (Bohr)' )" ) alat
       WRITE(iunit, " (2x,'Alat  = ', F15.7, ' (Ang )' )" ) alat * BOHR
       WRITE(iunit, " (2x,'Omega = ', F15.7, ' (Bohr^3)' )" ) omega
       WRITE(iunit, " (2x,'Omega = ', F15.7, ' (Ang^3 )',/ )" ) omega * BOHR**3
       WRITE(iunit, " (2x, 'Crystal axes:' ) ")
       WRITE(iunit, " (16x,'in Bohr units',27x,'in Alat units' )")
       DO j=1,3
          WRITE (iunit, fmt="(4x,'a(',I1,') = (', 3F11.5, ' )    (',3F11.5, ' )'  )" ) &
                   j, ( avec(i,j), i=1,3 ), ( avec(i,j)/alat, i=1,3 )
       ENDDO
       !
       WRITE(iunit, " (16x,'in Ang units' )")
       DO j=1,3
          WRITE (iunit, fmt="(4x,'a(',I1,') = (', 3F11.5, ' )'  )" ) &
                  j, ( avec(i,j)*BOHR , i=1,3 )
       ENDDO
       !
       WRITE(iunit, " (/,2x, ' Reciprocal lattice vectors:' ) " )
       WRITE(iunit, " (16x,'in Bohr^-1 units',24x,'in 2Pi/Alat units' )")
       DO j=1,3
          WRITE (iunit, fmt="(4x,'b(',I1,') = (', 3F11.5, ' )    (',3F11.5, ' )'  )" ) &
                   j, ( bvec(i,j), i=1,3 ), ( bvec(i,j)*alat / TPI, i=1,3 )
       ENDDO
       WRITE(iunit, " (  ' </LATTICE>',/)" )
   ENDIF

   !
   ! ... ions
   IF ( ions_alloc .AND. lions .AND. ionode ) THEN 
       WRITE(iunit, " (  ' <IONS>')" )
       WRITE(iunit, " (2x,'Number of chemical species =', i3 ) " ) nsp

       IF ( lpseudo ) THEN
           !
           IF ( .NOT. use_pseudo )  THEN
               CALL warning( 'summary', 'Pseudopots not read, assumed to be norm cons.')
           ELSEIF ( use_uspp ) THEN
               WRITE(iunit, " (2x,'Calculation is done within US pseudopot.',/)") 
           ENDIF
           !
           DO is=1, nsp
               WRITE(iunit, "(5x, 'Pseudo(',i2,') = ',a)") is, TRIM(psfile(is))
           ENDDO
           !
           !
           ! ... pseudo summary from Espresso
           !
           IF ( use_pseudo ) THEN 
               !
               DO nt = 1, nsp
                 !
                 CALL print_ps_info( iunit, nt )
                 !
               ENDDO
               !
           ENDIF
           !
           ! ... end of pseudo summary from Espresso
           !
       ENDIF  ! lpseudo

       WRITE(iunit, " (/,2x,'Atomic positions: (cart. coord. in Bohr)' ) " )
       DO ia = 1, nat
          WRITE(iunit, "(5x, a, 2x,'tau( ',I3,' ) = (', 3F12.7, ' )' )" ) &
                   symb(ia), ia, (tau( i, ia )*alat, i = 1, 3)
       ENDDO
       !
       IF ( TRIM(verbosity) == 'high') THEN
           !
           ALLOCATE( tau_cry(3,nat) )
           !
           tau_cry(:,:) = tau(:,:) * alat
           CALL cart2cry(tau_cry, avec)
           !
           WRITE(iunit, " (/,2x,'Atomic positions: (crystal coord.)' ) " )
           DO ia = 1, nat
               WRITE(iunit, "(5x, a, 2x,'tau( ',I3,' ) = (', 3F12.7, ' )' )" ) &
                      symb(ia), ia, (tau_cry( i, ia ), i = 1, 3)
           ENDDO
           !
           DEALLOCATE( tau_cry )
           !
       ENDIF
       !
       WRITE(iunit, " (  ' </IONS>',/)" )
   ENDIF
       
   !
   ! ... symmetry
   IF ( symmetry_alloc .AND. lsymmetry .AND. ionode ) THEN 
       !
       WRITE(iunit, " (  ' <SYMMETRY>')" )
       WRITE(iunit, " (2x,'Number of symmetry operations =', i3,/ ) " ) nsym
       !
       IF ( TRIM(verbosity) == "high" ) THEN
          !
          DO isym = 1, nsym
             !
             CALL symmetry_write( iunit, isym, srot(:,:,isym), strasl(:,isym), TRIM(sname(isym)) )
             !
          ENDDO
          !
       ENDIF
       !
       WRITE(iunit, " (  ' </SYMMETRY>',/)" )
       !
   ENDIF

   !
   ! ... kpoints
   !
   IF ( kpoints_alloc .AND. lkpoints .AND. ionode ) THEN 
       WRITE(iunit, " (  ' <K-POINTS>')" )
       WRITE(iunit, "(2x, '       nproc = ',i5, '   (Parallelism over kpts)' ) " ) nproc
       WRITE(iunit, "(2x, 'global nkpts = ',i5 ) " ) nkpts_g
       WRITE(iunit, "(2x, ' local nkpts = ',i5 ) " ) nkpts
       WRITE(iunit, "(2x, 'Monkhorst-Pack grid:      nk = (',3i4,' ),', &
                                         & 6x,'shift = (',3i4,' )' ) " ) nk(:), s(:) 
       WRITE(iunit, "(/,2x, 'K-point grid: (cart. coord. in Bohr^-1)' ) " )
       !
       DO ik=1,nkpts_g
           !
           WRITE(iunit, " (4x, 'k (', i5, ') =    ( ',3f12.7,' ),   weight = ', f14.7 )") &
                      ik, ( vkpt_g(i,ik), i=1,3 ), wk_g(ik)
           !
       ENDDO
       !
       IF ( TRIM(verbosity) == 'high') THEN
           !
           ALLOCATE( vkpt_cry(3,nkpts_g) )
           !
           vkpt_cry(:,:) = vkpt_g(:,:)
           CALL cart2cry(vkpt_cry, bvec)
           !
           WRITE(iunit, " (/,2x,'K-point grid: (crystal coord.)' ) " )
           DO ik = 1, nkpts_g
               !
               WRITE(iunit, " (4x, 'k (', i5, ') =    ( ',3f12.7,' ),   weight = ', f14.7 )") &
                      ik, ( vkpt_cry(i,ik), i=1,3 ), wk_g(ik)
               !
           ENDDO
           !
           DEALLOCATE( vkpt_cry )
           !
       ENDIF
       !
       WRITE(iunit, " (  ' </K-POINTS>',/)" )
   ENDIF

   !
   ! ... rgrid
   !
   IF ( rgrid_alloc .AND. lrgrid .AND. ionode ) THEN 
       WRITE(iunit, " (  ' <R-GRID>')" )
       WRITE(iunit, "(  2x, '       nrtot = ',i5 ) " ) nrtot
       WRITE(iunit, "(  2x, 'R-grid generators:      nr = (',3i4,' )' ) " ) nr(:)
       WRITE(iunit, "(/,2x, 'R-grid vectors:         (crystal units)' ) " )
       !
       DO ir=1,nrtot
          WRITE(iunit, " (4x, 'R (', i5, ') =    ( ',3i7,' ),   wr = ', f14.7 )") &
          ir, ( ivr(i,ir), i=1,3 ), wr(ir)
       ENDDO
       WRITE(iunit, " (  ' </R-GRID>',/)" )
   ENDIF

   ! 
   ! ... bshells 
   ! 
   IF ( bshells_alloc .AND. lbshells .AND. ionode ) THEN
       WRITE(iunit, " (  ' <B-SHELL>')" )
       !
       WRITE (iunit, "(2x, 'List of the ' , i2, ' b-vectors : (Bohr^-1) ') ") nb
       DO i = 1, nb
          WRITE(iunit, " (4x, 'b (', i5, ') =    ( ',3f12.7, ' ),   weight = ',f14.7 )")&
                         i, ( vb(j,i), j=1,3 ), wb(i)
       ENDDO
       !
       WRITE(iunit, " (/,2x, 'Total weight = ' , f15.7) ") wbtot
       WRITE(iunit, " (  ' </B-SHELL>',/)" )
   ENDIF

   !
   ! ... eigs and windows
   !
   IF ( windows_alloc .AND. lwindows ) THEN 
       !
       IF ( .NOT. kpoints_alloc ) CALL errore('summary','Unexpectedly kpts NOT alloc',1)
       !
       IF ( ionode ) THEN
           !
           WRITE(iunit, " (  ' <WINDOWS>')" )
           WRITE(iunit," (2x, 'Definition of energy windows: [eV]',/ ) " )
           !
           IF ( win_min < -10000.0 .AND. win_max > 10000.0 ) THEN
               WRITE(iunit, " (4x, 'outer window: E  = (  -inf ,  inf  )' )" ) 
           ELSEIF ( win_min < -10000.0 ) THEN
               WRITE(iunit, " (4x, 'outer window: E  = (  -inf , ',f9.4, ' )' )" ) &
                                   win_max
           ELSEIF ( win_max > 10000.0 ) THEN
               WRITE(iunit, " (4x, 'outer window: E  = ( ', f9.4, ' ,  inf  )' )" ) &
                                   win_min
           ELSE  
              ! std case
              WRITE(iunit, " (4x, 'outer window: E  = ( ', f9.4, ' , ',f9.4, ' )' )" ) &
                                   win_min, win_max
           ENDIF
           WRITE(iunit,"(4x,'Max number of bands in the outer window (dimwinx) = ',i5)") dimwinx
           WRITE(iunit,"(/,2x,'Electronic Structure from DFT calculation:')")
           WRITE(iunit,"(  4x,'nkpts =',i4,',     ','nbnd =',i4,',')") nkpts_g, nbnd
           WRITE(iunit,"(  4x,'nspin =',i4 ) " ) nspin
           WRITE(iunit,"(  4x,'Fermi energy =',f15.9,' eV')") efermi

           IF ( TRIM(verbosity) == "medium" .OR. TRIM(verbosity) == "high" ) THEN
               DO ik=1,nkpts_g
                   WRITE(iunit,"(1x,'!')")
                   WRITE(iunit,"(1x,'!',4x,'kpt = ',i5,' ( ',3f9.5,' )    dimwin = ',i4)") &
                                ik, vkpt_g(:,ik), dimwin(ik)
                   WRITE(iunit,"(1x,'!',39x,'imin = ',i4,'  imax = ',i4)") imin(ik), imax(ik)
                   WRITE(iunit,"(1x,'!',3x,'Eigenvalues:')"  )
                  WRITE(iunit,"(1x,'!',2x, 8f9.4)") ( eig(i,ik), i=1,nbnd )
               ENDDO
           ENDIF

           IF ( .NOT. lfrozen ) THEN
               WRITE(iunit," (/,4x,'inner window: NOT used --> NO FROZEN STATES' )" )
           ELSE
               !
               WRITE(iunit," (/,4x,'inner window:',/)")
               IF ( froz_min > -10000.0 ) THEN
                   WRITE(iunit," (  7x,'froz_min = ', f8.4)") froz_min
               ELSE
                   WRITE(iunit," (  7x,'froz_min = -inf')") 
               ENDIF
               !
               IF ( froz_max > -10000.0 ) THEN
                   WRITE(iunit," (  7x,'froz_max = ', f8.4)") froz_max
               ELSE
                   WRITE(iunit," (  7x,'froz_max = +inf')") 
               ENDIF
               !
               WRITE( iunit, "()" )
               DO ik=1,nkpts_g
                   WRITE(iunit, "(4x, 'k(', i5, ' )  --> ', i5, '  frozen states')") ik, dimfroz(ik)
               ENDDO
               !
           ENDIF

           WRITE(iunit, " ( /, ' </WINDOWS>',/)" )
           !
       ENDIF
       !
   ENDIF

   !
   ! memory usage
   !
   IF ( lmemory ) THEN 
       !
       CALL memusage( iunit )
       !
   ENDIF

   !
   CALL flush_unit( iunit )
   CALL log_pop( 'summary' )
   !
END SUBROUTINE summary
!
!
!************************************************************
SUBROUTINE print_ps_info( iunit, nt )
  !************************************************************
  !
  ! Copyright (C) 2001-2007 PWSCF group
  ! This file is distributed under the terms of the
  ! GNU General Public License. See the file `License'
  ! in the root directory of the present distribution,
  ! or http://www.gnu.org/copyleft/gpl.txt .
  !
  USE ions_module,     ONLY : psfile
  USE atom_module,     ONLY : rgrid
  USE uspp_param,      ONLY : upf
  USE funct_module,    ONLY : dft_is_gradient

  !
  INTEGER, INTENT(IN) :: iunit, nt
  !
  !
  INTEGER           :: ib, i
  CHARACTER(LEN=35) :: ps
     !
     IF ( upf(nt)%tpawp ) THEN
        ! Note: for PAW pseudo also tvanp is .true.
        ps="Projector augmented-wave"
     ELSE IF ( upf(nt)%tvanp ) THEN
        ps='Ultrasoft'
     ELSE
        ps='Norm-conserving'
     END IF
     !
     IF ( upf(nt)%nlcc ) ps = TRIM(ps) // ' + core correction'
     !
     WRITE( iunit, '(/5x,"PseudoPot. #",i2," for ",a2," read from file ",a)')&
             nt, upf(nt)%psd, TRIM (psfile(nt))
     !
     WRITE( iunit, '( 5x,"Pseudo is ",a,", Zval =",f5.1)') &
            TRIM (ps), upf(nt)%zp
     !
     WRITE( iunit, '(5x,A)') TRIM(upf(nt)%generated)
     !
     IF(upf(nt)%tpawp) &
        WRITE( iunit, '(5x,a,a)') &
               "Shape of augmentation charge: ", TRIM(upf(nt)%paw%augshape)
     WRITE( iunit, '(5x,"Using radial grid of ", i4, " points, ", &
         &i2," beta functions with: ")') rgrid(nt)%mesh, upf(nt)%nbeta
     DO ib = 1, upf(nt)%nbeta
        IF (ib<10) THEN
           WRITE( iunit, '(15x," l(",i1,") = ",i3)') ib, upf(nt)%lll(ib)
        ELSE
           WRITE( iunit, '(14x," l(",i2,") = ",i3)') ib, upf(nt)%lll(ib)
        ENDIF
     END DO

     IF ( upf(nt)%tvanp ) THEN
        IF (upf(nt)%nqf==0) THEN
           WRITE( iunit, '(5x,"Q(r) pseudized with 0 coefficients ",/)') 
        ELSE
           WRITE( iunit, '(5x,"Q(r) pseudized with ", &
           &          i2," coefficients,  rinner = ",3f8.3,/ &
           &          52x,3f8.3,/ 52x,3f8.3)') &
           &          upf(nt)%nqf, (upf(nt)%rinner(i), i=1,upf(nt)%nqlc)
        END IF
     ENDIF

END SUBROUTINE print_ps_info

END MODULE summary_module

