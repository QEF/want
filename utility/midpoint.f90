!
! Copyright (C) 2005 WanT Group
!
! This file is distributed under the terms of the
! GNU General Public License. See the file `License'
! in the root directory of the present distribution,
! or http://www.gnu.org/copyleft/gpl.txt .

!
!----------------------------------
   MODULE bond_module
!----------------------------------
   !
   ! contains basic dafinitions to define bonds
   !
   USE kinds,     ONLY : dbl
   USE constants, ONLY : EPS_m6
   IMPLICIT NONE
   !
   PRIVATE
   SAVE
   !
   !
   TYPE bond_type
      !
      INTEGER     :: atom1
      INTEGER     :: atom2
      INTEGER     :: cell1(3)
      INTEGER     :: cell2(3)
      INTEGER     :: itype
      REAL(dbl)   :: r2
      REAL(dbl)   :: midcoord(3)
      !
      TYPE( bond_type ), POINTER :: next
      !
   END TYPE bond_type
   !
   ! 
   INTERFACE OPERATOR (==)
       MODULE PROCEDURE bond_type_compare
   END INTERFACE
   !
   PUBLIC :: bond_type
   PUBLIC :: OPERATOR(==)
   !
CONTAINS
   !
!------------------------------------------------------
   LOGICAL FUNCTION bond_type_compare( bond1, bond2)
   !------------------------------------------------------
   IMPLICIT NONE
      !
      TYPE( bond_type ), INTENT(IN) :: bond1, bond2
      LOGICAL :: ldirect, linverse
      !
      bond_type_compare = .FALSE.
      !  
      !
      ! check direct equality
      !
      ldirect  = .FALSE.
      !
      IF ( bond1%atom1  ==  bond2%atom1  .AND.   &
           bond1%atom2  ==  bond2%atom2  .AND.   &
           ALL ( bond1%cell1(:) - bond1%cell2(:) == bond2%cell1(:) - bond2%cell2(:) ) .AND. &
           ABS( bond1%r2 - bond2%r2 ) < EPS_m6 ) &
           ldirect  = .TRUE.
      !
      !
      ! check inverse equality
      !
      linverse = .FALSE.
      !
      IF ( bond1%atom1  ==  bond2%atom2  .AND.   &
           bond1%atom2  ==  bond2%atom1  .AND.   &
           ALL ( bond1%cell1(:) - bond1%cell2(:) == bond2%cell2(:) - bond2%cell1(:) ) .AND. &
           ABS( bond1%r2 - bond2%r2 ) < EPS_m6 ) &
           linverse  = .TRUE.
      !
      !
      bond_type_compare = ldirect .OR. linverse
      !
   END FUNCTION bond_type_compare
   !
END MODULE

 

!
!=====================================================
   PROGRAM midpoint
   !=====================================================
   !
   ! this is a simple utility to find the (inequivalent) bond midpoints
   ! of a given periodic structure.
   !
   ! INPUT:    prefix, work_dir, output_fmt, toll
   ! OUTPUT:   cartesian bond coordinates
   !
   ! input file layout
   !
   !   &INPUT
   !      prefix           the prefix of the DFT calculation we want to analyze
   !      work_dir         the directory where the DFT datafile are stored.
   !                       the code search for the file
   !                             $work_dir/$prefix.export/index.xml
   !      output_fmt       the fmt used to write the found midbond positions on output
   !                       ( "angstrom" | "bohr" | "alat" | "crystal" )
   !      rcut             cutoff radius: bonds which are longer of rcut [Ang] are deleted
   !      toll             the tolerance on bond length used to distinguish between
   !                       nearest and next nearest neigbors. (given in percentage on the
   !                       bond length)
   !   /
   !
   USE kinds
   USE parameters,         ONLY : nstrx
   USE constants,          ONLY : ONE, TWO, bohr => bohr_radius_angs, EPS_m2
   USE io_module,          ONLY : prefix, work_dir, stdin, stdout
   USE version_module,     ONLY : version_number
   USE converters_module,  ONLY : cry2cart, cart2cry
   USE lattice_module,     ONLY : avec, alat
   USE ions_module,        ONLY : atm_symb, ityp, tau, nat, nsp
   USE want_interfaces_module
   USE parser_module 
   USE timing_module
   !
   USE bond_module
   !
   IMPLICIT NONE

   !
   ! input variables
   !
   CHARACTER( 20 )  :: output_fmt    ! ( "angstrom" | "bohr" | "alat" | "crystal" )
   REAL(dbl)        :: toll, rcut
   !
   ! input namelist
   !
   NAMELIST /INPUT/ prefix, work_dir, output_fmt, toll, rcut

   !
   ! local variables
   !
   INTEGER   :: ia, ipair, ia1, ia2
   INTEGER   :: m, n, i, j, k, ierr
   INTEGER   :: npair, nmid
   REAL(dbl) :: aux, conv, vect(3)
   LOGICAL   :: lfound
   !
   INTEGER,      ALLOCATABLE :: map(:,:)
   REAL(dbl),    ALLOCATABLE :: length(:)
   CHARACTER(3), ALLOCATABLE :: symb1(:), symb2(:)
   LOGICAL,      ALLOCATABLE :: pair_is_valid(:)
   !
   TYPE( bond_type ), POINTER :: list, current, runner
   !
   ! end of declariations
   !

!
!------------------------------
! main body
!------------------------------
!
      CALL startup(version_number,'midpoint')


!
! ... Read INPUT namelist from stdin
!
      prefix                      = 'WanT'
      work_dir                    = './'
      output_fmt                  = 'angstrom'
      rcut                        = 5.0     ! [Ang]
      toll                        = 0.10
      

      CALL input_from_file ( stdin, ierr )
      IF ( ierr /= 0 )  CALL errore('midpoint','error in input from file',ABS(ierr))
      !
      READ(stdin, INPUT, IOSTAT=ierr)
      IF ( ierr /= 0 )  CALL errore('midpoint','Unable to read namelist INPUT',ABS(ierr))

      !
      ! Some checks
      !
      CALL change_case(output_fmt,'lower')
      IF ( TRIM(output_fmt) /= "angstrom" .AND. TRIM(output_fmt) /= "bohr" .AND. &
           TRIM(output_fmt) /= "alat" .AND. TRIM(output_fmt) /= "crystal" ) &
           CALL errore('midpoint', 'Invalid output_fmt = '//TRIM(output_fmt), 4)

      IF ( toll < 0.0 ) CALL errore('midpoint','invalid toll',3)
      IF ( rcut < 0.0 ) CALL errore('midpoint','invalid rcut',4)

      !
      ! convert rcut to bohr
      ! to be consistent with internal units
      !
      rcut = rcut / bohr

!
! Getting DFT data
!
      CALL want_dftread ( LATTICE=.TRUE.,  IONS=.TRUE.,    WINDOWS=.FALSE., KPOINTS=.FALSE.)
      CALL want_init    ( INPUT=  .FALSE., LATTICE=.TRUE., IONS=.TRUE., &
                          WINDOWS=.FALSE., KPOINTS=.FALSE., &
                          BSHELLS=.FALSE., PSEUDO =.FALSE. )

      !
      ! Print data to output
      !
      CALL summary( stdout, INPUT=.FALSE.,  LATTICE=.TRUE.,  IONS=.TRUE., &
                            PSEUDO=.FALSE., KPOINTS=.FALSE., WINDOWS=.FALSE. )


      !
      ! summary of the input
      !
      WRITE( stdout, "(2/,2x,70('='))" )
      WRITE( stdout, "(2x,'=',19x,'Computing mid points of bonds',20x,'=')" )
      WRITE( stdout, "(2x,70('='),2/)" )

      WRITE( stdout,"(  2x,'      Output fmt :',3x,a)") TRIM(output_fmt)
      WRITE( stdout,"(  2x,'  Bond len. toll :',3x,f9.4)") toll
      WRITE( stdout,"(  2x,'   Cutoff radius :',3x,f9.4,' [Ang]')") rcut * bohr
      WRITE( stdout,"(/)")


!
! starting the actual calculation
!

      !
      ! compute the number of different bond types
      ! and init quantities
      !
      npair = nsp * (nsp+1) /2

      ALLOCATE( length(npair), symb1(npair), symb2(npair), map(nsp,nsp), STAT=ierr )
      IF (ierr/=0) CALL errore('midpoint','allocating length, symb1, symb2',ABS(ierr))
      ALLOCATE( pair_is_valid(npair), STAT=ierr )
      IF (ierr/=0) CALL errore('midpoint','allocating pair_is_valid',ABS(ierr))
      
      !
      ! set the 
      !
      ipair=0
      !
      DO m=1, nsp
         DO n = m, nsp
            !
            ipair = ipair+1
            !
            symb1(ipair) = atm_symb(m)
            symb2(ipair) = atm_symb(n)
            !
            map( n, m) = ipair 
         ENDDO
      ENDDO
      !
      DO m=1,nsp
         DO n=1, m-1
            ! 
            map(n,m) = map(m,n)
         ENDDO
      ENDDO


      !
      ! FIRST: loop over the atom pairs to find the minimum bond length 
      ! for each bond type
      !
      ! the first replicated atoms should be those in the 0-th cell
      !

      !
      ! tau is converted to bohr
      !
      tau(:,:) = tau(:,:) * alat


      length(:)        =  1000000.0
      pair_is_valid(:) =  .FALSE. 
      !
      DO ia1 = 1, nat
      DO ia2 = 1, nat
          !
          ! here we consider only ia2 moving to the neighbor cells
          !
          DO k= -1, 1
          DO j= -1, 1
          DO i= -1, 1
             !
             ipair = map( ityp(ia1) , ityp(ia2) )
             !
             ! compute the squared distance
             !
             vect(:) = tau(:,ia1) -tau(:,ia2) -i * avec(:,1) -j * avec(:,2) -k * avec(:,3)
             !
             aux = DOT_PRODUCT( vect(:), vect(:) )
             !
             IF ( aux < length(ipair) .AND. aux > EPS_m2 .AND. aux < rcut**2 ) THEN
                 !
                 length( ipair )        = aux
                 pair_is_valid( ipair ) = .TRUE.
                 !
             ENDIF
             ! 
          ENDDO
          ENDDO
          ENDDO
          !
      ENDDO
      ENDDO

      !
      ! report about 
      !
      WRITE( stdout,"(  2x,'# Total bond types: ', i5)") npair
      WRITE( stdout,"(  2x,'Minimum bond length [Ang]:')")
      !
      DO ipair = 1, npair
         !
         IF ( pair_is_valid( ipair ) ) THEN
            !
            WRITE( stdout,"(4x,a3,'-- ',a3,'  : ',f9.6)") symb1(ipair), symb2(ipair), &
                                                         SQRT(length(ipair)) * bohr
            !
         ENDIF
         !
      ENDDO
      !
      WRITE( stdout,"()")


      !
      ! final loop searching for the nearest neighbor of each atom
      ! add the found bond to the linked list "list"
      !
      NULLIFY( list )
      nmid = 0
      !
      DO ia1 = 1, nat
      DO ia2 = 1, nat
          !
          ! here we consider only ia2 moving to the neighbor cells
          !
          DO k= -1, 1
          DO j= -1, 1
          DO i= -1, 1
              !
              ipair = map( ityp(ia1) , ityp(ia2) )

              !
              ! compute the squared distance
              !
              vect(:) = tau(:,ia1) -tau(:,ia2) -i * avec(:,1) -j * avec(:,2) -k * avec(:,3)
              !
              aux = DOT_PRODUCT( vect(:), vect(:) )
 
              !
              ! check whether we found a "good" bond
              !
              IF ( aux <= MIN ( length( ipair )*( ONE+toll ) , rcut**2 ) .AND. &
                   aux > EPS_m2 ) THEN
                 !
                 ALLOCATE( current, STAT=ierr)
                 IF ( ierr/=0 ) CALL errore('midpoint','allocating current',ABS(ierr))
                 !
                 current%atom1    =ia1
                 current%atom2    =ia2
                 !
                 current%cell1(:) = 0
                 current%cell2(1) = i 
                 current%cell2(2) = j 
                 current%cell2(3) = k 
                 !
                 current%itype    = ipair  
                 current%r2       = aux
                 !
                 current%midcoord(:) = 0.5 * ( tau(:,ia1) +tau(:,ia2) &
                                       +i * avec(:,1) +j * avec(:,2) +k * avec(:,3) )
                 !
                 ! check whether this bond is already in the list
                 !
                 lfound = .FALSE.
                 runner => list 
                 !
                 DO 
                     IF ( .NOT. ASSOCIATED( runner ) ) EXIT
                     IF ( current == runner ) THEN 
                        lfound = .TRUE.
                        EXIT
                     ENDIF
                     !
                     runner => runner%next
                     !
                 ENDDO
                 !
                 ! adding a new node to the list
                 !
                 IF ( .NOT. lfound ) THEN 
                     !
                     current%next => list
                     !
                     ! updating 
                     list         => current
                     !
                     nmid = nmid + 1
                     !
                 ELSE
                     ! 
                     DEALLOCATE( current, STAT=ierr)
                     IF ( ierr/=0 ) CALL errore('midpoint','deallocating current',ABS(ierr))
                     !
                 ENDIF
                 !
              ENDIF
              !
          ENDDO
          ENDDO
          ENDDO
          !
      ENDDO
      ENDDO


      !
      ! write results
      !
      conv = ONE
      !
      SELECT CASE ( TRIM(output_fmt) )
      !
      CASE ( 'angstrom' )
         !
         conv = bohr
         !
      CASE ( 'bohr' )
         !
         conv = ONE
         !
      CASE ( 'alat' )
         !
         conv = ONE/alat
         !
      CASE ( 'crystal' )
         !
         CALL cart2cry(tau,avec)
         !
         runner => list
         DO 
            IF ( .NOT. ASSOCIATED( runner ) ) EXIT 
            CALL cart2cry( runner%midcoord(:),avec)
            !
            runner => runner%next 
         ENDDO
         conv = ONE
         !
      END SELECT
      !
      !
      WRITE( stdout,"(/,2x,70('='))" )
      WRITE( stdout,"(  2x,'Atomic positions [',a,']:',/)") TRIM(output_fmt)
      !
      WRITE( stdout, "(i5,/)") nat
      DO ia=1,nat
          WRITE( stdout, "(2x,a3, 2x, 3f15.9)") atm_symb(ityp(ia)), tau(:,ia)*conv
      ENDDO
      WRITE( stdout,"(/,2x,70('='))" )
      !
      !
      WRITE( stdout,"(  2x,'Bond mid points [',a,']:',/)") TRIM(output_fmt)
      WRITE( stdout, "(i5,/)") nmid
      !
      runner => list
      !
      DO 
          IF ( .NOT. ASSOCIATED( runner ) ) EXIT
          !
          WRITE( stdout, "(2x,a3, 2x, 3f15.9, 3x,a3,'-- ',a3)") 'H  ', &
                    runner%midcoord(:)*conv, &
                    symb1( runner%itype ), symb2( runner%itype )
          !
          runner => runner%next
          !
      ENDDO
      
!
! closing the run
!

      WRITE( stdout, "()" )

      DEALLOCATE( length, symb1, symb2, map, STAT=ierr )
      IF (ierr/=0) CALL errore('midpoint','deallocating length--map', ABS(ierr))
      DEALLOCATE( pair_is_valid, STAT=ierr )
      IF (ierr/=0) CALL errore('midpoint','deallocating pair_is_valid', ABS(ierr))
      !
      runner => list
      DO 
          IF ( .NOT. ASSOCIATED( runner ) ) EXIT
          list => runner%next 
          DEALLOCATE( runner )
          runner => list
      ENDDO

      !
      ! global cleanup
      !
      CALL cleanup 

      !
      ! finalize
      !
      CALL shutdown( 'midpoint' )

END PROGRAM midpoint

