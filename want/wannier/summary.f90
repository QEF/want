!
! Copyright (C) 2004 Andrea Ferretti
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
   USE io_module, ONLY : title, prefix, work_dir
   USE input_module, ONLY : input_alloc => alloc
   USE input_module
! XXX mettere a posto input_module
   USE lattice_module, ONLY : lattice_alloc => alloc, avec, bvec, alat
   USE ions_module, ONLY : ions_alloc => alloc, nat, nsp, symb, tau, psfile
   USE kpoints_module, ONLY : kpoints_alloc, nkpts, vkpt, wk, &
                              bshells_alloc, dnn, ndnntot, nnshell, nntot, bk, wb, bka
   USE windows_module, ONLY : windows_alloc => alloc, dimwin, eiw, mxdbnd, imin, imax, &
                              dimfroz, lfrozen, dimwinx
   
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
   SUBROUTINE summary(unit)
   !**********************************************************
      IMPLICIT NONE
      INTEGER,   INTENT(in)  :: unit
      INTEGER                :: ik, ia, idnn 
      INTEGER                :: i, j, m, is
      REAL(dbl), ALLOCATABLE :: kpt_cart(:,:)
      REAL(dbl), ALLOCATABLE :: center_cart1(:,:), center_cart2(:,:)
      INTEGER                :: ierr

   !
   ! <MAIN & INPUT> section
   !
      IF ( input_alloc ) THEN
          WRITE(unit,"()")      
          WRITE( unit, " (2x,70('='))" )
          WRITE( unit, " (2x,'=',32x,'Main',32x,'=')" )
          WRITE( unit, " (2x,70('='),/)" )
          WRITE(unit,"(  7x,'     Calculation Title :',5x,a)") TRIM(title)
          WRITE(unit,"(  7x,'                Prefix :',5x,a)") TRIM(prefix)
          IF ( LEN_TRIM(work_dir) <= 65 ) THEN
             WRITE(unit,"(  7x,'     Working directory :',5x,a)") TRIM(work_dir)
          ELSE
             WRITE(unit,"(  7x,'     Working directory :',5x,/,10x,a)") TRIM(work_dir)
          ENDIF

          WRITE( unit, " ( /, '<WANNIER_FUNCTIONS>')" )
          WRITE(unit, " (2x,'Input parameters for Wannier func. calculation')")
          WRITE(unit, " (4x,'Number of Wannier functions required = ', i4 )" ) dimwann
          WRITE( unit,"(4x,'CG minim: Mixing parameter (alphafix0)= ', f6.3 )" ) alphafix0
          WRITE( unit,"(4x,'CG minim: Max iteration number = ', i5 )" ) niter0
          WRITE( unit,"(4x,'XX minim: Mixing parameter (alphafix) = ', f6.3 )" ) alphafix
          WRITE( unit,"(4x,'XX minim: Max iteration number = ', i5 )" ) niter
          WRITE( unit,"(4x,'Every ',i3,' XX iteration perform a CG minimi (ncg)')" ) ncg
          WRITE(unit, " (4x,'Number of k-point shells = ', i3 )" ) nshells
          WRITE(unit, " (4x,'Chosen shells (indexes) = ', 100i3 )" ) nwhich(1:nshells)
          WRITE(unit, " (4x,'Print each ', i3,' iterations' )" ) nprint
          WRITE(unit, " (4x,'Ordering type = ', a )" ) TRIM(ordering_type)
          WRITE( unit," (4x,'Verbosity = ', a )" ) TRIM(verbosity)
          WRITE( unit, " ( '</WANNIER_FUNCTIONS>',/)" )

          WRITE( unit, " ( '<DISENTANGLE>')" )
          WRITE(unit, " (2x,'Input parameters for disentangling subspaces')")
          WRITE( unit,"(4x,'Mixing parameter (alpha)= ', f6.3 )" ) alpha
          WRITE( unit,"(4x,'Max iter = ', i5 )" ) maxiter
          WRITE( unit,"(4x,'Starting guess orbitals (itrial) = ', i2 )" ) itrial
          WRITE( unit,"(4x,'Disentangle convergence threshold = ', f15.9 )") disentangle_thr
          WRITE( unit, " ( '</DISENTANGLE>',/)" )

          WRITE( unit, " ( '<WANNIER_CENTERS>')" )
          ALLOCATE( center_cart1(3,dimwann), center_cart2(3,dimwann), STAT=ierr )
             IF (ierr/=0) CALL errore('summary','allocating center_cart*',ABS(ierr))
          ! ... initialize with crystal coordinates and then convert
          center_cart1(:,:) = rphiimx1(:,:)
          center_cart2(:,:) = rphiimx2(:,:)
          CALL cry2cart(center_cart1, avec )
          CALL cry2cart(center_cart2, avec )

          WRITE( unit, "(2x, 'Gaussian centers: (cart. coord. in Bohr)' ) " )
          DO i = 1, dimwann
              WRITE( unit, "(4x,'Center = ', i3,' Type =',i2,' Gaussian  = (',3F10.6,' ) ')") &
                     i, gauss_typ(i), center_cart1(:,i)
              IF  ( gauss_typ(i) == 2 ) THEN
                  WRITE( unit, fmt="(26x,'Gaussian2 = (', 3F10.6, ' ) ' )" ) &
                     center_cart2(:,i)
              ENDIF
          ENDDO
          DEALLOCATE( center_cart1, center_cart2, STAT=ierr )
             IF (ierr/=0) CALL errore('summary','deallocating center_cart*',ABS(ierr))
          WRITE( unit, " ( '</WANNIER_CENTERS>',/)" )

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
          WRITE( unit, " (2x,'Alat = ', F8.4, ' (Bohr)' )" ) alat
          WRITE( unit, " (2x,'Alat = ', F8.4, ' (Ang )',/ )" ) alat * bohr
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
          DO is=1, nsp
             WRITE( unit, "(4x, 'Pseudo(',i3,') = ',a)") is, TRIM(psfile(is))
          ENDDO
          WRITE( unit, " (/,2x,'Atomic positions: (cart. coord. in units of alat)' ) " )
          DO ia = 1, nat
             WRITE( unit, "(4x, a, 2x,'tau( ',I3,' ) = (', 3F8.4, ' )' )" ) &
                      symb(ia), ia, (tau( i, ia ), i = 1, 3)
          ENDDO
          WRITE( unit, " (  '</IONS>',/)" )
      ENDIF
          
      !
      ! ... kpoints
      IF ( kpoints_alloc ) THEN 
          WRITE( unit, " (  '<K-POINTS>')" )
          WRITE( unit, "(2x, 'nkpts = ',i4 ) " ) nkpts
          WRITE( unit, "(2x, 'K-point calculation: (cart. coord. in Ang^-1)' ) " )
          
          ALLOCATE( kpt_cart(3,nkpts), STAT=ierr)
             IF ( ierr/=0 ) CALL errore('summary','allocating kpt_cart',ABS(ierr))
          kpt_cart(:,:) = vkpt(:,:)
          CALL cry2cart( kpt_cart, bvec / BOHR)

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
          WRITE( unit,"(2x, 'Nearest-neighbour shells for k-point 1: (in Ang^-1)' ) " )
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
          WRITE (unit, "(/,2x, 'List of the ' , i2, ' vectors b_k: (Ang^-1) ') ") nntot(1)
          DO i = 1, nntot(1)
              WRITE(unit, " (4x, 'b_k', i4, ':   ( ',3f9.5, ' ),   weight = ',f8.4 )")&
                             i, ( bk(j,1,i), j=1,3 ), wb(1,i)
          ENDDO
          !
          WRITE (unit, "(/,2x, 'The ',i2, '  bk directions are:' )") nntot(1)/2
          DO i=1,nntot(1)/2
              WRITE(unit, "(4x,'dir',i2,':   ( ',3f9.5, ' ) ' )" ) i, ( bka(j,i), j=1,3 )
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
          WRITE( unit,"(  4x,'nkpts =',i4,',     ','nbnd =',i4)") nkpts, mxdbnd
          DO ik=1,nkpts
              WRITE(unit, " (/,4x,'kpt =', i3, ' ( ',3f6.3,' )    dimwin = ', i4)" ) &
                              ik, vkpt(:,ik), dimwin(ik)
              WRITE(unit, " (36x,'  imin = ', i4, '  imax = ', i4)" ) imin(ik), imax(ik)
              WRITE(unit, "(3x,'Eigenvalues:')"  )
              WRITE(unit,'(2x, 8f9.4)') ( eiw(i,ik), i=1,mxdbnd )
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

