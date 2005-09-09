! 
! Copyright (C) 2004 WanT Group
!
! This file is distributed under the terms of the
! GNU General Public License. See the file `License'
! in the root directory of the present distribution,
! or http://www.gnu.org/copyleft/gpl.txt .
!
! Based on a previous version by Nicola Marzari and David Vanderbilt
!
!*****************************************************
   SUBROUTINE hamiltonian_calc( dimwann, nkpts, cu )
   !*****************************************************
   !    
   ! Calculates the matrix elements of the hamiltonian on the
   ! Wannier basis
   ! 
   USE kinds
   USE constants, ONLY: ZERO, CZERO, CI, TPI
   USE parameters, ONLY : nstrx
   USE io_module, ONLY : stdout, ham_unit, work_dir, prefix, postfix 
   USE timing_module, ONLY : timing
   USE parser_module, ONLY : int2char

   USE control_module,       ONLY : verbosity
   USE kpoints_module,       ONLY : vkpt
   USE windows_module,       ONLY : efermi
   USE hamiltonian_module,   ONLY : wan_eig, efermi, rham, kham, nws, indxws, vws, &
                                    ham_alloc => alloc 


   IMPLICIT NONE 
   !
   ! input variables
   ! 
   INTEGER,      INTENT(in) :: dimwann, nkpts
   COMPLEX(dbl), INTENT(in) :: cu(dimwann,dimwann, nkpts)
   
   !
   ! local variables
   !
   CHARACTER(16) :: subname = 'hamiltonian_calc'

   INTEGER :: i, j, m, ik, iws, inorm
   REAL(dbl)     :: norm, rmod
   REAL(dbl)     :: arg
   COMPLEX(dbl)  :: phase
 
   INTEGER   :: ierr
   CHARACTER(nstrx) :: filename
   !
   ! end of declariations
   !

!
!------------------------------
! main body
!------------------------------
!
      CALL timing('hamiltonian_calc',OPR='start')
      IF ( .NOT. ham_alloc ) CALL errore(subname,'hamiltonian data NOT alloc',1)

      !
      ! set the zero as the fermi energy
      !
      wan_eig(:,:) = wan_eig(:,:) - efermi


      !
      ! Calculate KHAM: H(k)=U^{dagger}(k) * H_0(k) * U(k)
      ! (hamiltonian matrix elements between the rotated bloch states)
      !
      DO ik = 1, nkpts
         DO j = 1, dimwann
         DO i = 1, j
            kham(i,j,ik) = CZERO
            DO m = 1, dimwann
                kham(i,j,ik) = kham(i,j,ik) + wan_eig(m,ik) * &
                                              CONJG( cu(m,i,ik) ) * cu(m,j,ik)
            ENDDO
            !
            ! use hermiticity
            ! 
            kham(j,i,ik) = CONJG( kham(i,j,ik) )
         ENDDO
         ENDDO
      ENDDO

      ! 
      ! Fourier transform it: H_ij(k) --> H_ij(R) = (1/N_kpts) sum_k e^{-ikR} H_ij(k)
      !
      DO iws = 1, nws
         DO j = 1, dimwann
         DO i = 1, dimwann
            rham(i,j,iws) = CZERO
            DO ik = 1, nkpts
                arg = DOT_PRODUCT( vkpt(:,ik), vws(:,iws) )
                phase = CMPLX( COS(arg), -SIN(arg) )
                rham(i,j,iws) = rham(i,j,iws) + phase * kham(i,j,ik)
            ENDDO
            rham(i,j,iws) = rham(i,j,iws) / DBLE(nkpts)
         ENDDO
         ENDDO
      ENDDO


      !
      ! ... writing matrix elements to file (and partially to stdout)
      !     for transport calculation
      !
      IF ( TRIM(verbosity) == 'high' ) THEN
          WRITE(stdout,"(/,2x,'Diagonal matrix elements of H on Wannier basis (n.n.)')")
          WRITE(stdout,"(  2x,'dimwann = ',i5)") dimwann
      ENDIF

      DO iws = 1, nws
          !
          ! chose nearest neighbours
          !
          inorm = indxws(1,iws)**2 + indxws(2,iws)**2 + indxws(3,iws)**2
          IF ( inorm <= 1 ) THEN

               filename=TRIM(work_dir)//'/'//TRIM(prefix)//TRIM(postfix)// &
                        '_RHAM.'//TRIM(int2char( 100 + iws))
               OPEN( UNIT=ham_unit,FILE=TRIM(filename),  STATUS='unknown', &
                     FORM='formatted', IOSTAT=ierr )
                   IF (ierr/=0) CALL errore(subname,'opening file '//TRIM(filename),ham_unit)
  
                   WRITE (ham_unit,"(2i5,3x,3i4)") dimwann, dimwann, ( indxws(i,iws), i=1,3 )
                   DO j = 1, dimwann
                       WRITE (ham_unit,"()")
                       DO i = 1, dimwann
                           WRITE(ham_unit, "(2f20.12)" ) rham(i,j,iws)
                       ENDDO
                   ENDDO
               CLOSE(ham_unit)

               IF ( TRIM(verbosity) == "high" .AND. inorm /=0 ) THEN
                   !
                   ! stdout (diagonal elements), avoiding R=0
                   !
                   WRITE (stdout,"(/,4x,'R = (',3i4,' )')") ( indxws(i,iws), i=1,3 )
                   WRITE(stdout,"( 3( 2f11.6',',2x) )") ( rham(i,i,iws), i =1,dimwann )
               ENDIF
          ENDIF
      ENDDO
      WRITE (stdout,"()") 

      !
      ! Check that magnitude of matrix elements |H_ij(R)| decreases with |R|.
      ! Should expect it to decrease *faster* using the rotated Bloch functions
      ! (again, except in the single-band case, where it should be exactly the same)
      !
      WRITE(stdout,"(/,2x,'Decay of the real space Hamiltonian:')") 
      WRITE(stdout,"(  2x,'  (number of R vectors (nws) :',i5,/)") nws
      WRITE(stdout,"(  4x,'#       R [cry]     |R| [Bohr]      Norm of H(R) [eV]')") 
      !
      DO iws = 1, nws
          !
          rmod = SQRT( DOT_PRODUCT( vws(:,iws), vws(:,iws) ))
          !
          ! compute the 2-norm of H_ij(R)
          !
          norm = ZERO
          DO j=1,dimwann
          DO i=1,dimwann
               norm = norm + REAL( CONJG( rham(i,j,iws)) * rham(i,j,iws) )
          ENDDO
          ENDDO
          WRITE(stdout,"(1x,i4,3x,3i4,3x,f10.6,5x,f12.6)") &
                         iws,indxws(:,iws), rmod, SQRT( norm / REAL(dimwann) )
      ENDDO

      CALL timing('hamiltonian_calc',OPR='stop')
END SUBROUTINE hamiltonian_calc

