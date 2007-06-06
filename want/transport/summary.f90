!
! Copyright (C) 2005 WanT Group
!
! This file is distributed under the terms of the
! GNU General Public License. See the file `License\'
! in the root directory of the present distribution,
! or http://www.gnu.org/copyleft/gpl.txt .
!

!**********************************************************
   SUBROUTINE summary(iunit)
   !**********************************************************
   ! 
   ! Print out all the informnatins obtained from the 
   ! input and initialization routines.
   !
   USE kinds,                ONLY : dbl
   USE constants,            ONLY : ZERO
   USE parser_module,        ONLY : log2char
   USE mp_global,            ONLY : nproc
   USE T_hamiltonian_module, ONLY : dimL, dimC, dimR
   USE T_control_module,     ONLY : calculation_type, conduct_formula,  &
                                    datafile_C, datafile_L, datafile_R, &
                                    datafile_sgm,                       &
                                    transport_dir, niterx, nprint,      & 
                                    use_overlap, use_correlation, write_kdata
   USE T_egrid_module,       ONLY : ne, emin, emax, de
   USE T_smearing_module,    ONLY : delta, smearing_type, nx_smear => nx, xmax
   USE T_kpoints_module,     ONLY : nkpts_par, nk_par, s_par, vkpt_par, wk_par, use_symm, &
                                    kpoints_alloc => alloc
   USE T_kpoints_module,     ONLY : nrtot_par, nr_par, vr_par, wr_par
   USE io_module,            ONLY : work_dir, prefix, postfix
   IMPLICIT NONE

   !
   ! input variables
   !
   INTEGER,   INTENT(in)  :: iunit

   !
   ! local variables
   !
   INTEGER      :: ik, ir
   INTEGER      :: nk_par3D(3)       ! 3D kpt mesh generator
   INTEGER      :: s_par3D(3)        ! 3D shifts
   INTEGER      :: nr_par3D(3)       ! 3D R-vect mesh generator
   REAL(dbl)    :: vkpt_par3D(3)     ! 3D kpt-vectors
   REAL(dbl)    :: vr_par3D(3)       ! 3D R-vectors

!--------------------------------------------------------

   CALL write_header( iunit, "INPUT Summary" )

   !
   ! <INPUT> section
   !
   WRITE(iunit,"( 2x,'<INPUT>')" )
   WRITE(iunit,"( 7x,'Calculation Type    :',5x,a)") TRIM(calculation_type)
   WRITE(iunit,"(   7x,'prefix              :',5x,   a)") TRIM(prefix)
   WRITE(iunit,"(   7x,'postfix             :',5x,   a)") TRIM(postfix)
   IF ( LEN_TRIM(work_dir) <= 65 ) THEN
      WRITE(iunit,"(7x,'work_dir            :',5x,   a)") TRIM(work_dir)
   ELSE
      WRITE(iunit,"(7x,'work_dir :',5x,/,10x,a)") TRIM(work_dir)
   ENDIF
   WRITE(iunit,"( 7x,'        L-lead dim. :',5x,i5)") dimL
   WRITE(iunit,"( 7x,'     conductor dim. :',5x,i5)") dimC
   WRITE(iunit,"( 7x,'        R-lead dim. :',5x,i5)") dimR
   WRITE(iunit,"( 7x,'Conductance Formula :',5x,a)") TRIM(conduct_formula)
   WRITE(iunit,"( 7x,'Transport Direction :',8x,i2)") transport_dir
   WRITE(iunit,"( 7x,'Use Overlap         :',5x,a)") log2char(use_overlap)
   WRITE(iunit,"( 7x,'Use Correlation     :',5x,a)") log2char(use_correlation)
   WRITE(iunit,"( 7x,'Write k-data        :',5x,a)") log2char(write_kdata)
   WRITE(iunit,"( 7x,'Max iteration number:',5x,i5)") niterx
   WRITE(iunit,"( )")
   WRITE(iunit,"( 7x,'Print info each ', i3,' energy step' )" ) nprint
   WRITE(iunit,"( )")
   WRITE(iunit,"( 7x,'Conductor data read from file  :',5x,a)") TRIM(datafile_C)
   IF (calculation_type == 'conductor') THEN
      WRITE(iunit,"( 7x,'Left lead data read from file  :',5x,a)") TRIM(datafile_L)
      WRITE(iunit,"( 7x,'Right lead data read from file :',5x,a)") TRIM(datafile_R)
   ENDIF
   IF (use_correlation) THEN
      WRITE(iunit,"( 7x,'Self-energy data read from file:',5x,a)") TRIM(datafile_sgm)
   ENDIF
   WRITE( iunit,"( 2x,'</INPUT>',2/)" )

   WRITE(iunit,"( 2x,'<ENERGY_GRID>')" )
   WRITE(iunit,"( 7x,'Dimension     :',5x,i6)")    ne
   WRITE(iunit,"( 7x,'Min Energy    :',5x,f10.5)") emin
   WRITE(iunit,"( 7x,'Max Energy    :',5x,f10.5)") emax
   WRITE(iunit,"( 7x,'Energy Step   :',5x,f10.5)") de
   WRITE(iunit,"( 7x,'Delta         :',5x,f10.5)") delta
   WRITE(iunit,"( 7x,'Smearing Type :',5x,a)")     TRIM(smearing_type)
   WRITE(iunit,"( 7x,'Smearing grid :',5x,i6)")    nx_smear
   WRITE(iunit,"( 7x,'Smearing gmax :',5x,f10.5)") xmax
   WRITE(iunit,"( 2x,'</ENERGY_GRID>',/)" )

   IF ( kpoints_alloc ) THEN
       !
       WRITE( iunit, "( /,2x,'<K-POINTS>')" )
       WRITE( iunit, "( 7x, 'nkpts_par = ',i4 ) " ) nkpts_par
       WRITE( iunit, "( 7x, 'nrtot_par = ',i4 ) " ) nrtot_par
       WRITE( iunit, "( 7x, ' use_symm = ',a  ) " ) TRIM(log2char(use_symm))
       !
       !
       nk_par3D(:) = imask( nk_par, 1, transport_dir )
       s_par3D(:)  = imask(  s_par, 0, transport_dir )
       !
       WRITE( iunit, "(/,7x, 'Parallel kpoints grid:',8x, &
                           &'nk = (',3i3,' )',3x,'s = (',3i3,' )') " ) nk_par3D(:), s_par3D(:) 
       !
       DO ik=1,nkpts_par
          !
          vkpt_par3D(:) = rmask( vkpt_par(:,ik), ZERO, transport_dir )
          !
          WRITE( iunit, "(7x, 'k (', i4, ') =    ( ',3f9.5,' ),   weight = ', f8.4 )") &
                 ik, vkpt_par3D(:), wk_par(ik)
       ENDDO
       !
       nr_par3D(:) = imask( nr_par, 1, transport_dir )
       !
       WRITE( iunit, "(/,7x, 'Parallel R vector grid:      nr = (',3i3,' )') " ) nr_par3D(:) 
       !
       DO ir=1,nrtot_par
          !
          vr_par3D(:) = rmask( vr_par(:,ir), ZERO, transport_dir )
          !
          WRITE( iunit, "(7x, 'R (', i4, ') =    ( ',3f9.5,' ),   weight = ', f8.4 )") &
                 ir, vr_par3D(:), wr_par(ir)
       ENDDO
       !    
       WRITE( iunit, " ( 2x,'</K-POINTS>',/)" )
       !
   ENDIF
   !
   !
   WRITE( iunit, "( /,2x,'<PARALLELISM>')" )
   WRITE( iunit, "(   7x, 'Paralellization over frequencies' ) " )
   WRITE( iunit, "(   7x, '# of processes: ', i5 ) " ) nproc
   WRITE( iunit, "(   2x,'</PARALLELISM>',/)" )
   !
   CALL flush_unit( iunit )


CONTAINS

!*****************************
   FUNCTION imask ( ivect, init, dir )
   !*****************************
   IMPLICIT NONE
      INTEGER :: imask(3)
      INTEGER :: ivect(2), init
      INTEGER :: dir 
      !
      imask( : ) = init
      !
      SELECT CASE ( dir )
      CASE ( 1 )
         imask( 2 ) = ivect(1) 
         imask( 3 ) = ivect(2) 
      CASE ( 2 )
         imask( 1 ) = ivect(1) 
         imask( 3 ) = ivect(2) 
      CASE ( 3 )
         imask( 1 ) = ivect(1) 
         imask( 2 ) = ivect(2) 
      CASE DEFAULT
         CALL errore('summary','invalid dir',21)
      END SELECT
      !
   END FUNCTION imask

!*****************************
   FUNCTION rmask ( rvect, init, dir )
   !*****************************
   IMPLICIT NONE
      REAL(dbl) :: rmask(3)
      REAL(dbl) :: rvect(2), init
      INTEGER   :: dir 
      !
      rmask( : ) = init
      !
      SELECT CASE ( dir )
      CASE ( 1 )
         rmask( 2 ) = rvect(1) 
         rmask( 3 ) = rvect(2) 
      CASE ( 2 )
         rmask( 1 ) = rvect(1) 
         rmask( 3 ) = rvect(2) 
      CASE ( 3 )
         rmask( 1 ) = rvect(1) 
         rmask( 2 ) = rvect(2) 
      CASE DEFAULT
         CALL errore('summary','invalid dir',21)
      END SELECT
      !
   END FUNCTION

END SUBROUTINE summary

