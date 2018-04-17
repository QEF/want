!
! Copyright (C) 2004 WanT Group
!
! This file is distributed under the terms of the
! GNU General Public License. See the file `License'
! in the root directory of the present distribution,
! or http://www.gnu.org/copyleft/gpl.txt .
!
!************************************************************
   SUBROUTINE get_points( nkpts_in, nkpts_max, kpt_in, xval_in, kptname_in, &
                          kpt, xval, nkpts_tot )
   !***********************************************************
   !
   ! Determines the k-points for calculating the band structure
   ! kpt_in are in cartesian coordinates (bohr^-1)
   !
   USE kinds
   USE constants, ONLY : ZERO, EPS_m6
   USE io_module, ONLY : stdout
   IMPLICIT NONE

   !
   ! I/O variables
   !
   INTEGER,      INTENT(in)  :: nkpts_in ! Number of k-points generating the line (edges)
   INTEGER,      INTENT(in)  :: nkpts_max ! maximum number of interpolated point
   INTEGER,      INTENT(out) :: nkpts_tot ! actual number of point in the line
   REAL(dbl),    INTENT(in)  :: kpt_in(3,nkpts_in)
   REAL(dbl),    INTENT(out) :: kpt(3,nkpts_max)
   REAL(dbl),    INTENT(out) :: xval(nkpts_max)
   REAL(dbl),    INTENT(out) :: xval_in(nkpts_in)
   CHARACTER(2), INTENT(in)  :: kptname_in(nkpts_in)
   !
   ! few local variables
   !
   INTEGER   :: i, j, n
   INTEGER   :: knum(nkpts_in-1)
   REAL(dbl) :: length0, length(nkpts_in-1)
   REAL(dbl) :: vec(3)
   !
   ! end of declariations
   !

!
!------------------------------
! main body
!------------------------------
!

       nkpts_tot = 0
       length0 = 0
       DO i = 1, nkpts_in - 1
           vec(:) = kpt_in(:,i+1) - kpt_in(:,i)
           length(i) = SQRT( DOT_PRODUCT( vec, vec) )
           IF ( length(i) < EPS_m6 ) CALL errore('get_points', 'length(i) too small', i )
       ENDDO
       length0 = SUM(length(:))


       DO i = 1, nkpts_in - 1
           n = INT( nkpts_max * length(i) / length0 )
           knum(i) = n
           IF ( n ==  0 ) CALL errore('get_points', 'nint=0', n )
 
           DO j = 1, n-1
               nkpts_tot = nkpts_tot + 1
               !
               IF ( nkpts_tot+1 > nkpts_max ) &
                   CALL errore('get_points', 'nkpts_tot too large  ', nkpts_tot )

               IF ( nkpts_tot ==  1 ) THEN
                   xval(nkpts_tot) = ZERO
               ELSE
                   xval(nkpts_tot) = xval(nkpts_tot-1) + length(i)/REAL(n, dbl)
               ENDIF

               IF ( j ==  1 ) xval_in(i) = xval(nkpts_tot)

               kpt(:,nkpts_tot) = kpt_in(:,i) + ( kpt_in(:,i+1) - kpt_in(:,i) ) * &
                                                  REAL(j-1, dbl) / REAL(n, dbl)  
           ENDDO
       ENDDO
       
       !
       ! Last point
       !
       nkpts_tot = nkpts_tot + 1
       xval(nkpts_tot)   = xval(nkpts_tot-1) + length(nkpts_in-1) / REAL(n, dbl)
       kpt(:,nkpts_tot)  = kpt_in(:,nkpts_in)
       xval_in(nkpts_in) = xval(nkpts_tot) 

       !
       ! summary
       !
       WRITE(stdout, "(/,2x,'Generating kpts: ',i3,6x,'Segments: ',i3)") nkpts_in, nkpts_in-1
       WRITE(stdout, "(2x,'Total kpts number: ',i3,4x,'Max kpt number: ',i3)") &
                      nkpts_tot, nkpts_max

       WRITE(stdout, "(2/,2x,'Generating kpts [cart. coord. Bohr^-1]')" )
       DO i=1,nkpts_in
           WRITE(stdout, "(6x, 'k point', i4, ':   ( ',3f9.5, ' ) ',3x,a2) ") &
                           i, kpt_in(:,i), kptname_in(i)
       ENDDO
       WRITE(stdout, "(/,2x,'Number of kpts in each segment')" )
       DO i=1,nkpts_in-1
          WRITE(stdout, "(6x, 'line', i4, ':   ',i5 ) ') " ) i, knum(i)
       ENDDO
       WRITE(stdout, "(2/,2x,'Generated kpts  [cart. coord. Bohr^-1]')" )
       DO i=1,nkpts_tot
          WRITE(stdout, "(6x, 'k point', i4, ':   ( ',3f9.5, ' ) ') " ) i, kpt(:,i)
       ENDDO

   END SUBROUTINE get_points
                    
