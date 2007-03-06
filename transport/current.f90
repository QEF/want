!
!      Copyright (C) 2005 WanT Group
!
!      This file is distributed under the terms of the
!      GNU General Public License. See the file `License\'
!      in the root directory of the present distribution,
!      or http://www.gnu.org/copyleft/gpl.txt .
!
!***********************************************
   PROGRAM current
   !***********************************************
   USE kinds,                ONLY : dbl
   USE parameters,           ONLY : nstrx 
   USE version_module,       ONLY : version_number
   USE timing_module,        ONLY : timing, timing_overview, global_list, timing_upto_now
   USE io_module,            ONLY : stdout, stdin, curr_unit => aux_unit,   &
                                    cond_unit => aux1_unit,                 &
                                    work_dir, prefix, postfix
!   USE T_smearing_module,    ONLY : smearing_init

   IMPLICIT NONE

   !
   ! local variables
   !
   CHARACTER(nstrx)         :: filename
   INTEGER                  :: ie, iv, ierr, ios,          &
                               i_start, i_end, ndim          ! integration extrema
   REAL(dbl), ALLOCATABLE   :: funct(:), rab(:)              ! auxiliary vectors for integration
   !
   REAL(dbl)                :: mu_L, mu_R                    !
   REAL(dbl), ALLOCATABLE   :: mu_L_aux(:), mu_R_aux(:),   & ! chemical potentials
                               ftemp_L(:,:), ftemp_R(:,:)    ! temperature smearing functions
   REAL(dbl)                :: sigma                         ! broadening
   REAL(dbl), ALLOCATABLE   :: transm(:)                     ! transmittance from data file
   !
   REAL(dbl), ALLOCATABLE   :: curr(:)                       ! current
   CHARACTER(nstrx)         :: fileout                       ! output filename

   !
   ! energy grid
   !
   INTEGER                  :: ne         ! dimension of the energy grid
   REAL(dbl)                :: emin       ! egrid extrema
   REAL(dbl)                :: emax       !
   REAL(dbl)                :: de         
   REAL(dbl), ALLOCATABLE   :: egrid(:)   ! energy grid

   !
   ! bias grid
   !
   INTEGER                  :: nV         ! dimension of the bias grid
   REAL(dbl)                :: Vmin       ! Vgrid extrema
   REAL(dbl)                :: Vmax       !
   REAL(dbl)                :: dV         
   REAL(dbl), ALLOCATABLE   :: Vgrid(:)   ! bias grid

   !
   ! input namelist
   !
   NAMELIST /INPUT/ prefix, postfix, work_dir, mu_L, mu_R, sigma, fileout, &
                    Vmin, Vmax, nV

!
!------------------------------
! main body
!------------------------------
!
   CALL startup(version_number,'current')

!
! ... Read INPUT namelist from stdin
!
   prefix                      = 'T'
   postfix                     = '_WanT'
   work_dir                    = './'
   fileout                     = ' '
   mu_L                        =  -0.5
   mu_R                        =   0.5
   sigma                       =   0.1    ! eV
   Vmin                        =  -1.0
   Vmax                        =   1.0
   nV                          =  1000
                                                                                                                  
   CALL input_from_file ( stdin, ierr )
   IF ( ierr /= 0 )  CALL errore('current','error in input from file',ABS(ierr))
   !
   READ(stdin, INPUT, IOSTAT=ierr)
   IF ( ierr /= 0 )  CALL errore('current','Unable to read namelist INPUT',ABS(ierr))

!
! init
!
   !
   ! get energy grid and transmittance from data file
   !
   filename = TRIM(work_dir)//'/'//TRIM(prefix)//'cond'//TRIM(postfix)//'.dat'
   OPEN ( cond_unit, FILE=TRIM(filename), FORM='formatted' )
   !
   ie = 0
   !
   DO WHILE ( .TRUE. ) 
      !
      READ ( cond_unit, *, IOSTAT = ios )
      IF ( ios /= 0 ) EXIT
      ie = ie + 1
      !
   ENDDO
   !
   ne = ie 
   !
   ALLOCATE ( egrid(ne), STAT=ierr )
   IF( ierr /=0 ) CALL errore('current','allocating egrid', ABS(ierr) )
   !
   ALLOCATE ( transm(ne), STAT=ierr )
   IF( ierr /=0 ) CALL errore('current','allocating transmittance', ABS(ierr) )
   !
   REWIND ( cond_unit )
   !
   DO ie = 1, ne
       READ ( cond_unit, *, IOSTAT=ios ) egrid(ie), transm(ie)
       IF ( ios/=0 ) CALL errore('current','reading T',ie)  
   ENDDO
   !
   CLOSE( cond_unit )

   !
   ! allocate
   !
   ALLOCATE ( Vgrid(nV), STAT=ierr )
   IF( ierr /=0 ) CALL errore('current','allocating Vgrid', ABS(ierr) )
   !
   ALLOCATE ( mu_L_aux(nV), mu_R_aux(nV), STAT=ierr )
   IF( ierr /=0 ) CALL errore('current','allocating mu', ABS(ierr) )
   !
   ALLOCATE ( curr(nV), STAT=ierr )
   IF( ierr /=0 ) CALL errore('current','allocating current', ABS(ierr) )
   !
   ALLOCATE ( ftemp_L(ne,nV), ftemp_R(ne,nV), STAT=ierr )
   IF( ierr /=0 ) CALL errore('current','allocating ftemp', ABS(ierr) )

   !
   ! bias grid
   !
   dV = (Vmax - Vmin)/REAL(nV-1, dbl)
   !
   DO iv = 1, nV
      Vgrid(iv) = Vmin + REAL(iv-1, dbl) * dV
   ENDDO 
   !

!
! current calculation
!
   !
   curr(:) = 0.0
   emin = egrid(1)
   emax = egrid(ne)
   !
   DO iv = 1, nV
      !
      mu_L_aux(iv) = mu_L * Vgrid(iv)
      mu_R_aux(iv) = mu_R * Vgrid(iv)
      !
      ! integration extrema
      CALL locate( egrid, ne, MIN( mu_L_aux(iv), mu_R_aux(iv) ) -10.0*sigma, i_start)
      IF ( i_start == 0 .OR. i_start == ne ) CALL errore('current','invalid i_start',4)
      !
      CALL locate( egrid, ne, MAX( mu_R_aux(iv), mu_L_aux(iv) ) +10.0*sigma, i_end)
      IF ( i_end == 0 .OR. i_end == ne ) CALL errore('current','invalid i_end',5)
      !
      ! simpson routine requires that ndim is an odd number
      IF (REAL(ndim/2, dbl) == REAL(INT(REAL(ndim/2, dbl)), dbl)) THEN
         i_end = i_end - 1
      ENDIF
      ndim = i_end - i_start + 1
      de = (egrid(i_end) - egrid(i_start))/REAL(ndim-1, dbl)
      !
      ! auxiliary vectors for integral calculation
      ALLOCATE ( funct(ndim), STAT=ierr )
      IF( ierr /=0 ) CALL errore('current','allocating funct', ABS(ierr) )
      ALLOCATE ( rab(ndim), STAT=ierr )
      IF( ierr /=0 ) CALL errore('current','allocating rab', ABS(ierr) )
      !
      DO ie = i_start, i_end
          !
          ftemp_L(ie, iv) = 1.0 / ( EXP(-(egrid(ie)-mu_L_aux(iv))/sigma) + 1.0 )
          ftemp_R(ie, iv) = 1.0 / ( EXP(-(egrid(ie)-mu_R_aux(iv))/sigma) + 1.0 )
          !
          rab(ie) = de
          !
      ENDDO
      !
!XXXXXXXXXXXXXX
!          curr(iv) = curr(iv) + (ftemp_L(ie, iv) - ftemp_R(ie, iv)) * transm(ie) * de
      funct = (ftemp_L(i_start:i_end, iv) - ftemp_R(i_start:i_end, iv)) * transm(i_start:i_end)
      CALL simpson (ndim, funct, rab, curr(iv))
      !
      DEALLOCATE ( funct, STAT=ierr )
      IF( ierr /=0 ) CALL errore('current','deallocating funct', ABS(ierr) )
      DEALLOCATE ( rab, STAT=ierr )
      IF( ierr /=0 ) CALL errore('current','deallocating rab', ABS(ierr) )
      !
   ENDDO
   !

   !
   ! write input data on the output file
   !
   IF ( LEN_TRIM(fileout) == 0 ) &
        fileout = TRIM(work_dir)//'/'//TRIM(prefix)//'current'//TRIM(postfix)//'.dat'
   !
   OPEN ( curr_unit, FILE=TRIM(fileout), FORM='formatted' )
   !
   DO iv = 1, nV
       WRITE ( curr_unit, '(2(f15.9))' ) Vgrid(iv), curr(iv)
   ENDDO
   !
   CLOSE( curr_unit )
   !

   
   !
   ! deallocate
   !
   DEALLOCATE ( egrid, STAT=ierr )
   IF( ierr /=0 ) CALL errore('current','deallocating egrid', ABS(ierr) )
   !
   DEALLOCATE ( Vgrid, STAT=ierr )
   IF( ierr /=0 ) CALL errore('current','deallocating Vgrid', ABS(ierr) )
   !
   DEALLOCATE ( mu_L_aux, mu_R_aux, STAT=ierr )
   IF( ierr /=0 ) CALL errore('current','deallocating mu', ABS(ierr) )
   !
   DEALLOCATE ( curr, STAT=ierr )
   IF( ierr /=0 ) CALL errore('current','deallocating current', ABS(ierr) )
   !
   DEALLOCATE ( ftemp_L, ftemp_R, STAT=ierr )
   IF( ierr /=0 ) CALL errore('current','deallocating ftemp', ABS(ierr) )
   !
   DEALLOCATE ( transm, STAT=ierr )
   IF( ierr /=0 ) CALL errore('current','deallocating transmittance', ABS(ierr) )

   CALL cleanup()

END PROGRAM current
  
