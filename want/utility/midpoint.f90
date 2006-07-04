!
! Copyright (C) 2005 WanT Group
!
! This file is distributed under the terms of the
! GNU General Public License. See the file `License'
! in the root directory of the present distribution,
! or http://www.gnu.org/copyleft/gpl.txt .
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
   USE want_init_module,   ONLY : want_init
   USE summary_module,     ONLY : summary
   USE parser_module 
   USE timing_module
   !
   USE lattice_module,     ONLY : avec, alat
   USE ions_module,        ONLY : atm_symb, ityp, tau, nat, nsp
      
   IMPLICIT NONE

   !
   ! input variables
   !
   CHARACTER( 20 )  :: output_fmt    ! ( "angstrom" | "bohr" | "alat" | "crystal" )
   REAL(dbl)        :: toll
   !
   ! input namelist
   !
   NAMELIST /INPUT/ prefix, work_dir, output_fmt, toll

   !
   ! local variables
   !
   INTEGER   :: ia, ib, ipair
   INTEGER   :: m, n, i, j, k, ierr
   INTEGER   :: npair, nmid, natom_tot
   REAL(dbl) :: aux, conv
   !
   INTEGER,      ALLOCATABLE :: map(:,:), ityp_tot(:), bond_type(:)
   REAL(dbl),    ALLOCATABLE :: tau_tot(:,:), midcoord(:,:)
   REAL(dbl),    ALLOCATABLE :: length(:)
   CHARACTER(3), ALLOCATABLE :: start(:), end(:)

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
      toll                        = 0.10
      

      READ(stdin, INPUT, IOSTAT=ierr)
      IF ( ierr /= 0 )  CALL errore('midpoint','Unable to read namelist INPUT',ABS(ierr))

      !
      ! Some checks
      !
      CALL change_case(output_fmt,'lower')
      IF ( TRIM(output_fmt) /= "angstrom" .AND. TRIM(output_fmt) /= "bohr" .AND. &
           TRIM(output_fmt) /= "alat" .AND. TRIM(output_fmt) /= "crystal" ) &
           CALL errore('midpoint', 'Invalid output_fmt = '//TRIM(output_fmt), 4)



!
! Getting DFT data
!
      CALL want_init( WANT_INPUT= .FALSE., WINDOWS=.FALSE., KPOINTS=.FALSE., &
                      BSHELLS=.FALSE.,     PSEUDO=.FALSE. )

      !
      ! Print data to output
      !
      CALL summary( stdout, LINPUT=.FALSE.,  LLATTICE=.TRUE., LATOMS=.TRUE., &
                            LPSEUDO=.FALSE., LKPOINTS=.FALSE.,LEIG=.FALSE. )


      !
      ! summary of the input
      !
      WRITE( stdout, "(2/,2x,70('='))" )
      WRITE( stdout, "(2x,'=',19x,'Computing mid points of bonds',20x,'=')" )
      WRITE( stdout, "(2x,70('='),2/)" )

      WRITE( stdout,"(  2x,'      Output fmt :',3x,a)") TRIM(output_fmt)
      WRITE( stdout,"(  2x,'  Bond len. toll :',3x,f9.4)") toll
      WRITE( stdout,"(/)")


!
! starting the actual calculation
!

      !
      ! compute the number of different bond types
      ! and init quantities
      !
      npair = nsp * (nsp+1) /2

      ALLOCATE( length(npair), start(npair), end(npair), map(nsp,nsp), STAT=ierr )
      IF (ierr/=0) CALL errore('midpoint','allocating length,start,end',ABS(ierr))
      
      length(:) =  1000000.0

      ipair=0
      DO m=1, nsp
         DO n = m, nsp
            !
            ipair = ipair+1
            !
            start(ipair) = atm_symb(m)
            end(ipair)   = atm_symb(n)
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
      ! repeat the unit cell in the positive direction
      ! of each axis up to the nearest enighbors (8 cells) 
      !
      natom_tot = 8 * nat
      !
      ALLOCATE( tau_tot(3,natom_tot), ityp_tot(natom_tot), STAT=ierr )
      IF (ierr/=0) CALL errore('midpoint','allocating tau_tot -- ityp_tot',ABS(ierr))
      !
      ! no more than 20 bonds are allowed for each atom, must be enough
      ALLOCATE( midcoord(3,20*nat), bond_type(20*nat), STAT=ierr )
      IF (ierr/=0) CALL errore('midpoint','allocating midcoord, bond_type',ABS(ierr))

      ib = 0
      DO k=1,2
      DO j=1,2
      DO i=1,2
           !
           DO ia = 1, nat
               !
               ib = ib + 1
               !
               tau_tot(:,ib) = tau(:,ia) * alat + &
                              (i-1)*avec(:,1) + (j-1)*avec(:,2) + (k-1)*avec(:,3)
                              !
               ityp_tot(ib) = ityp(ia)
           ENDDO
      ENDDO
      ENDDO
      ENDDO


      !
      ! loop over the atom pairs to find the minimum bond length 
      ! for each bond type
      !
      DO i = 1, natom_tot
      DO j = i+1, natom_tot

           ipair = map( ityp_tot(j) , ityp_tot(i) )

           !
           ! compute the squared distance
           !
           aux = DOT_PRODUCT( tau_tot(:,i)-tau_tot(:,j), tau_tot(:,i)-tau_tot(:,j) )

           IF ( aux < length(ipair) .AND. aux > EPS_m2 ) length(ipair) = aux
      ENDDO
      ENDDO


      WRITE( stdout,"(  2x,'# bond types: ', i5)") npair
      WRITE( stdout,"(  2x,'Minimum bond length [Ang]:')")
      !
      DO ipair = 1, npair
         !
         WRITE( stdout,"(4x,a3,'-- ',a3,'  : ',f9.6)") start(ipair), end(ipair), &
                                                      SQRT(length(ipair)) * bohr
      ENDDO
      WRITE( stdout,"()")


      !
      ! final loop searching for the nearest neighbor of each atom
      !
      nmid = 0
      DO i = 1, nat
      DO j = i+1, natom_tot

           ipair = map( ityp_tot(j) , ityp_tot(i) )

           !
           ! compute the squared distance
           !
           aux = DOT_PRODUCT( tau_tot(:,i)-tau_tot(:,j), tau_tot(:,i)-tau_tot(:,j) )

           IF ( aux <= length(ipair)*(ONE+toll) .AND. aux > EPS_m2 ) THEN
              nmid = nmid + 1   
              !
              midcoord(:,nmid) = ( tau_tot(:,i) + tau_tot(:,j) ) / TWO
              bond_type(nmid)  = ipair  
           ENDIF
      ENDDO
      ENDDO


      !
      ! write results
      !
      conv = ONE
      SELECT CASE ( TRIM(output_fmt) )
      CASE ( 'angstrom' )
         conv = bohr
      CASE ( 'bohr' )
         conv = ONE
      CASE ( 'alat' )
         conv = ONE/alat
      CASE ( 'crystal' )
         CALL cart2cry(tau_tot,avec)
         CALL cart2cry(midcoord(:,1:nmid),avec)
         conv = ONE
      END SELECT

      WRITE( stdout,"(/,2x,70('='))" )
      WRITE( stdout,"(  2x,'2x2x2 Replicated atoms [',a,']:',/)") TRIM(output_fmt)
      !
      WRITE( stdout, "(i5,/)") natom_tot 
      DO ia=1,natom_tot
          WRITE( stdout, "(2x,a3, 2x, 3f15.9)") atm_symb(ityp_tot(ia)), tau_tot(:,ia)*conv
      ENDDO
      WRITE( stdout,"(/,2x,70('='))" )
      
      WRITE( stdout,"(  2x,'Bond mid points [',a,']:',/)") TRIM(output_fmt)
      !
      WRITE( stdout, "(i5,/)") nmid
      DO ia=1,nmid
          WRITE( stdout, "(2x,a3, 2x, 3f15.9, 3x,a3,'-- ',a3)") 'H  ', midcoord(:,ia)*conv, &
               start( bond_type(ia) ), end( bond_type(ia) )
      ENDDO
      
!
! closing the run
!

      WRITE( stdout, "(/,2x,70('='))" )

      !
      ! Finalize timing
      !
      CALL timing('midpoint',OPR='stop')
      CALL timing_overview(stdout,LIST=global_list,MAIN_NAME='midpoint')

      CALL cleanup 

      DEALLOCATE( length, start, end, map, STAT=ierr )
      IF (ierr/=0) CALL errore('midpoint','deallocating length--map', ABS(ierr))
      DEALLOCATE( tau_tot, ityp_tot, STAT=ierr )
      IF (ierr/=0) CALL errore('midpoint','deallocating tau_tot--ityp_tot', ABS(ierr))
      DEALLOCATE( midcoord, bond_type, STAT=ierr )
      IF (ierr/=0) CALL errore('midpoint','deallocating midcoord--bond_type', ABS(ierr))

END PROGRAM midpoint

