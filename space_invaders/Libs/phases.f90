!
! Copyright (C) 2004 WanT Group
! Copyright (C) 2002 Nicola Marzari, Ivo Souza, David Vanderbilt
! Copyright (C) 1997 Nicola Marzari, David Vanderbilt
!
! This file is distributed under the terms of the
! GNU General Public License. See the file `License'
! in the root directory of the present distribution,
! or http://www.gnu.org/copyleft/gpl.txt .
!
!********************************************************
   SUBROUTINE phases( dimwann, nkpts, Mkb, csheet, sheet )
   !********************************************************
   !
   ! here  the problem is to find a real-space 3-vector rguide such that
   ! the avarage phase of Mkb, i.e. the phase of csum(nn) = \Sum_m,ik ( Mkb(m,m,nn,ik) )
   ! is around the phase of exp[ -i bka(nn) dot rguide_m ]
   ! or letting
   !    xx(nn) = - Im ln csum(nn)  (modulo 2*pi)
   ! then
   !    bka(nn) dot rguide ~= xx(nn)
   !
   ! The requested rguide is obtained by minimizing
   !   \Sum_nn [ bka(nn) dot rguide - xx(nn) ] ^2
   ! or, setting the derivative with respect to rcenter to zero, by solving
   !   \Sum_i smat(j,i) * rguide(i,nwann) = svec(j)
   ! where
   !    smat(j,i) = sum_nn bka(j,nn) * bka(i,nn)
   !    svec(j)   = sum_nn bka(j,nn) * xx(nn)
   !
   USE kinds
   USE timing_module, ONLY : timing 
   USE constants, ONLY: PI, CI, CZERO, ZERO, TWO, EPS_m6
   USE io_module, ONLY : stdout
   USE kpoints_module, ONLY : nnx, nnhx, nntot, neigh, bk, bka

   IMPLICIT NONE

      !
      ! input variables
      !
      INTEGER, INTENT(in) :: dimwann
      INTEGER, INTENT(in) :: nkpts
      COMPLEX(dbl), INTENT(in) :: Mkb(dimwann,dimwann,nnx,nkpts)
      COMPLEX(dbl), INTENT(out) :: csheet(dimwann,nnx,nkpts)
      REAL(dbl),    INTENT(out) :: sheet(dimwann,nnx,nkpts)

      !
      ! local variables
      !
      INTEGER :: nnh
      INTEGER :: ik, inn, ina
      INTEGER :: i, j, m, ierr

      REAL(dbl), ALLOCATABLE :: rguide(:,:)
      COMPLEX(dbl) :: csum(nnhx), csumt
      REAL(dbl)    :: xx(nnhx), xx0
      REAL(dbl)    :: smat(3,3), svec(3), sinv(3,3), det
      REAL(dbl)    :: pherr
      !
      ! end of declarations
      !

!
!------------------------------
! main body
!------------------------------
!
      CALL timing('phases',OPR='start')

      nnh = NINT( nntot(1) / TWO )

      ALLOCATE( rguide(3,dimwann), STAT=ierr )
         IF(ierr/=0) CALL errore('phases','allocating rguide',ABS(ierr))

!
! ... csum is determined and then its appropriate
!     guiding center rguide(3,iwann)

      DO m =1,dimwann

           !
           ! get average phase for each unique bk direction (given by bka)
           ! here m and ina are kept fixed, out of the sum, while we
           ! sum over kpt. Compare with the definition of Omega_D
           !
           ! Omega_D = 1/Nk \Sum_{ik, nn} wb(ik,nn) * 
           !                \Sum_m ( Im Log Mkb(m,m) + b * <r>_m )**2
           !
           ! thus here we are avaraging over ik and considering each m and nn term
           ! separately
           !
           DO inn=1,nnh
               csum(inn)=CZERO
               DO ik=1,nkpts
                  ina=neigh(ik,inn)
                  csum(inn)=csum(inn)+Mkb(m,m,ina,ik)
               ENDDO
           ENDDO

           ! Initialize smat and svec
           !
           smat(:,:) = ZERO
           svec(:) = ZERO

           DO inn = 1, nnh
               IF ( inn <= 3 ) THEN
                   !
                   ! Obtain xx with arbitrary branch cut choice
                   !
                   xx(inn) = -AIMAG( LOG( csum(inn) ) )
               ELSE
                   !
                   ! Obtain xx with branch cut choice guided by rguide
                   !
                   xx0 = DOT_PRODUCT( bka(:,inn), rguide(:,m) )
                   !
                   ! xx0 is the expected value for xx
                   csumt = EXP( CI * xx0 )
                   !
                   ! csumt has opposite of expected phase of csum(inn)
                   xx(inn) = xx0 - AIMAG( LOG( csum(inn) * csumt ) )
               ENDIF

               !
               ! Update smat and svec
               !
               DO j = 1, 3
                   DO i = 1, 3
                       smat(i,j) = smat(i,j) + bka(i,inn) * bka(j,inn)
                   ENDDO
                   svec(j) = svec(j) + bka(j,inn) * xx(inn)
               ENDDO


               IF ( inn >= 3 ) THEN
                   !
                   ! Determine rguide
                   !
                   !
                   ! if first three bka vectors are linearly dependent this is not allowed
                   ! check
                   CALL invmat( 3, smat, sinv, det )

                   IF ( ABS(det) < EPS_m6 ) &
                        CALL errore('phases', 'linear dependent vectors found', 3 )

                   DO j = 1, 3
                       rguide(j,m) = DOT_PRODUCT( sinv(j,:), svec(:) )
                   ENDDO
               ENDIF

           ENDDO
      ENDDO

!
! ... Obtain branch cut choice guided by rguide
!  
      DO ik = 1, nkpts
      DO inn = 1, nntot(ik)
           DO m = 1, dimwann
                sheet(m,inn,ik) = DOT_PRODUCT( bk(:,ik,inn), rguide(:,m) )
                csheet(m,inn,ik) = EXP( CI * sheet(m,inn,ik) )
           ENDDO
      ENDDO
      ENDDO

!
! ... Now check that we picked the proper sheet for the log
!     of Mkb. The criterion is: 
!         q_n^{k,b}=Im(ln(M_nn^{k,b})) + b \cdot r_n are
!         circa 0 for a good solution, circa multiples of 2 pi  for a bad one.
!     I use the guiding center, instead of r_n, to understand which could be
!     the right sheet
!

      DO ik = 1, nkpts
      DO inn = 1, nntot(ik)
          DO m = 1, dimwann
               pherr =  AIMAG( LOG( csheet(m,inn,ik) * Mkb(m,m,inn,ik) ) ) &
                       -AIMAG( LOG( Mkb(m,m,inn,ik) ) ) 
               IF ( ABS(pherr) > PI ) &
                    WRITE(stdout,"(2x,'WARNING: phases problem at: ',3i4,f18.9,3f10.5)")  &
                                       ik, m, inn, pherr, ( bk(i,ik,inn), i=1,3 )
         ENDDO
      ENDDO
      ENDDO

      DEALLOCATE( rguide, STAT=ierr )
         IF(ierr/=0) CALL errore('phases','deallocating rguide',ABS(ierr))

      CALL timing('phases',OPR='stop')
   END SUBROUTINE phases

