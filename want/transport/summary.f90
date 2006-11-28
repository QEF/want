!
! Copyright (C) 2005 WanT Group
!
! This file is distributed under the terms of the
! GNU General Public License. See the file `License\'
! in the root directory of the present distribution,
! or http://www.gnu.org/copyleft/gpl.txt .
!

!**********************************************************
   SUBROUTINE summary(unit)
   !**********************************************************
   ! 
   ! Print out all the informnatins obtained from the 
   ! input and initialization routines.
   !
   USE kinds,             ONLY : dbl
   USE constants,         ONLY : ZERO
   USE parser_module,     ONLY : log2char
   USE T_control_module,  ONLY : calculation_type, conduct_formula,  &
                                 datafile_C, datafile_L, datafile_R, &
                                 datafile_sgm,                       &
                                 transport_dir, niterx, nprint,      & 
                                 use_overlap, use_correlation                       
   USE T_egrid_module,    ONLY : ne, emin, emax, de
   USE T_smearing_module, ONLY : delta, smearing_type, nx_smear => nx, xmax
   USE T_kpoints_module,  ONLY : nkpts_par, nk_par, vkpt_par, wk_par, &
                                 kpoints_alloc => alloc
   USE T_kpoints_module,  ONLY : nrtot_par, nr_par, vr_par, wr_par
   USE io_module,         ONLY : work_dir, prefix
   IMPLICIT NONE

   !
   ! input variables
   !
   INTEGER,   INTENT(in)         :: unit

   !
   ! local variables
   !
   INTEGER      :: i, ik
   INTEGER      :: nk_par3D(3)       ! 3D kpt mesh generator
   INTEGER      :: nr_par3D(3)       ! 3D R-vect mesh generator
   REAL(dbl)    :: vkpt_par3D(3)     ! 3D kpt-vectors
   REAL(dbl)    :: vr_par3D(3)       ! 3D R-vectors

!--------------------------------------------------------

   WRITE(unit,"(/,2x,70('='))" )
   WRITE(unit,"(2x,'=',27x,'INPUT Summary',28x,'=')" )
   WRITE(unit,"(2x,70('='),/)" )

   !
   ! <INPUT> section
   !
   WRITE(unit,"( 2x,'<INPUT>')" )
   WRITE(unit,"( 7x,'Calculation Type    :',5x,a)") TRIM(calculation_type)
   WRITE(unit,"(   7x,'prefix              :',5x,   a)") TRIM(prefix)
   IF ( LEN_TRIM(work_dir) <= 65 ) THEN
      WRITE(unit,"(7x,'work_dir            :',5x,   a)") TRIM(work_dir)
   ELSE
      WRITE(unit,"(7x,'work_dir :',5x,/,10x,a)") TRIM(work_dir)
   ENDIF
   WRITE(unit,"( 7x,'Conductance Formula :',5x,a)") TRIM(conduct_formula)
   WRITE(unit,"( 7x,'Transport Direction :',5x,i2)") transport_dir
   WRITE(unit,"( 7x,'Use Overlap         :',5x,a)") log2char(use_overlap)
   WRITE(unit,"( 7x,'Use Correlation     :',5x,a)") log2char(use_correlation)
   WRITE(unit,"( 7x,'Max iteration number:',5x,i4)") niterx
   WRITE(unit,"( )")
   WRITE(unit,"( 7x,'Print info each ', i3,' energy step' )" ) nprint
   WRITE(unit,"( )")
   WRITE(unit,"( 7x,'Conductor data read from file  :',5x,a)") TRIM(datafile_C)
   IF (calculation_type == 'conductor') THEN
      WRITE(unit,"( 7x,'Left lead data read from file  :',5x,a)") TRIM(datafile_L)
      WRITE(unit,"( 7x,'Right lead data read from file :',5x,a)") TRIM(datafile_R)
   ENDIF
   IF (use_correlation) THEN
      WRITE(unit,"( 7x,'Self-energy data read from file:',5x,a)") TRIM(datafile_sgm)
   ENDIF
   WRITE( unit,"( 2x,'</INPUT>',2/)" )

   WRITE(unit,"( 2x,'<ENERGY_GRID>')" )
   WRITE(unit,"( 7x,'Dimension     :',5x,i6)")    ne
   WRITE(unit,"( 7x,'Min Energy    :',5x,f10.5)") emin
   WRITE(unit,"( 7x,'Max Energy    :',5x,f10.5)") emax
   WRITE(unit,"( 7x,'Energy Step   :',5x,f10.5)") de
   WRITE(unit,"( 7x,'Delta         :',5x,f10.5)") delta
   WRITE(unit,"( 7x,'Smearing Type :',5x,a)")     TRIM(smearing_type)
   WRITE(unit,"( 7x,'Smearing grid Dimension :',5x,i6)")    nx_smear
   WRITE(unit,"( 7x,'Smearing grid Extrema   :',5x,f10.5)") xmax
   WRITE(unit,"( 2x,'</ENERGY_GRID>',/)" )

   IF ( kpoints_alloc ) THEN
       !
       WRITE( unit, "( /,2x,'<K-POINTS>')" )
       WRITE( unit, "( 7x, 'nkpts_par = ',i4 ) " ) nkpts_par
       WRITE( unit, "( 7x, 'nrtot_par = ',i4 ) " ) nrtot_par
       !
       ! 3D vectors initialization
       !
       nk_par3D(:) = 1
       nr_par3D(:) = 1
       vkpt_par3D(:) = ZERO
       vr_par3D(:) = ZERO
       !
       SELECT CASE (transport_dir)

       CASE (1)
           !
           ! transport direction along the x axis
           !
           nk_par3D(2) = nk_par(1) 
           nk_par3D(3) = nk_par(2) 
           !
           WRITE( unit, "(/,7x, 'Parallel kpoints grid:      nk = (',3i3,' )') " ) nk_par3D(:) 
           !
           DO ik=1,nkpts_par
              vkpt_par3D(2) = vkpt_par(1,ik)
              vkpt_par3D(3) = vkpt_par(2,ik)
              WRITE( unit, "(7x, 'k (', i4, ') =    ( ',3f9.5,' ),   weight = ', f8.4 )") &
                     ik, ( vkpt_par3D(i), i=1,3 ), wk_par(ik)
           ENDDO
           !
           nr_par3D(2) = nr_par(1) 
           nr_par3D(3) = nr_par(2) 
           !
           WRITE( unit, "(/,7x, 'Parallel R vector grid:      nr = (',3i3,' )') " ) nr_par3D(:) 
           !
           DO ik=1,nrtot_par
              vr_par3D(2) = vr_par(1,ik)
              vr_par3D(3) = vr_par(2,ik)
              WRITE( unit, "(7x, 'R (', i4, ') =    ( ',3f9.5,' ),   weight = ', f8.4 )") &
                     ik, ( vr_par3D(i), i=1,3 ), wr_par(ik)
           ENDDO
    
       CASE (2)
           !
           ! transport direction along the y axis
           !
           nk_par3D(1) = nk_par(1) 
           nk_par3D(3) = nk_par(2) 
           !
           WRITE( unit, "(/,7x, 'Parallel kpoints grid:      nk = (',3i3,' )') " ) nk_par3D(:) 
           !
           DO ik=1,nkpts_par
              vkpt_par3D(1) = vkpt_par(1,ik)
              vkpt_par3D(3) = vkpt_par(2,ik)
              WRITE( unit, " (7x, 'k (', i4, ') =    ( ',3f9.5,' ),   weight = ', f8.4 )") &
                     ik, ( vkpt_par3D(i), i=1,3 ), wk_par(ik)
           ENDDO
           !
           nr_par3D(1) = nr_par(1) 
           nr_par3D(3) = nr_par(2) 
           !
           WRITE( unit, "(/,7x, 'Parallel R vector grid:      nr = (',3i3,' )') " ) nr_par3D(:) 
           !
           DO ik=1,nrtot_par
              vr_par3D(1) = vr_par(1,ik)
              vr_par3D(3) = vr_par(2,ik)
              WRITE( unit, " (7x, 'R (', i4, ') =    ( ',3f9.5,' ),   weight = ', f8.4 )") &
                     ik, ( vr_par3D(i), i=1,3 ), wr_par(ik)
           ENDDO
    
       CASE (3)
           !
           ! transport direction along the z axis
           !
           nk_par3D(1) = nk_par(1) 
           nk_par3D(2) = nk_par(2) 
           !
           WRITE( unit, "(/,7x, 'Parallel kpoints grid:      nk = (',3i3,' )') " ) nk_par3D(:) 
           !
           DO ik=1,nkpts_par
              vkpt_par3D(1) = vkpt_par(1,ik)
              vkpt_par3D(2) = vkpt_par(2,ik)
              WRITE( unit, " (7x, 'k (', i4, ') =    ( ',3f9.5,' ),   weight = ', f8.4 )") &
                     ik, ( vkpt_par3D(i), i=1,3 ), wk_par(ik)
           ENDDO
           !
           nr_par3D(1) = nr_par(1) 
           nr_par3D(2) = nr_par(2) 
           !
           WRITE( unit, "(/,7x, 'Parallel R vector grid:      nr = (',3i3,' )') " ) nr_par3D(:) 
           !
           DO ik=1,nrtot_par
              vr_par3D(1) = vr_par(1,ik)
              vr_par3D(2) = vr_par(2,ik)
              WRITE( unit, " (7x, 'R (', i4, ') =    ( ',3f9.5,' ),   weight = ', f8.4 )") &
                     ik, ( vr_par3D(i), i=1,3 ), wr_par(ik)
           ENDDO
       END SELECT
   ENDIF
   WRITE( unit, " ( 2x,'</K-POINTS>',/)" )

END SUBROUTINE summary

