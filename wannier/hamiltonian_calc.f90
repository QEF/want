! 
! Copyright (C) 2004 WanT Group
!
! This file is distributed under the terms of the
! GNU General Public License. See the file `License'
! in the root directory of the present distribution,
! or http://www.gnu.org/copyleft/gpl.txt .
!
! Based on a previous version by Nicola Marzari and David Vanderbilt
! See the CREDITS file in the ~want directory for a full description
!
!*****************************************************
   SUBROUTINE hamiltonian_calc( dimwann, nkpts, cU )
   !*****************************************************
   !    
   ! Calculates the matrix elements of the hamiltonian on the
   ! Wannier basis
   ! 
   USE kinds,                ONLY : dbl 
   USE constants,            ONLY : ZERO, ONE, CZERO, CI, TPI
   USE parameters,           ONLY : nstrx
   USE io_module,            ONLY : stdout, ham_unit, ionode
   USE timing_module,        ONLY : timing
   USE log_module,           ONLY : log_push, log_pop
   USE parser_module,        ONLY : int2char
   USE control_module,       ONLY : verbosity
   USE kpoints_module,       ONLY : vkpt, nkpts_g, iks, nrtot, vr, ivr
   USE windows_module,       ONLY : efermi
   USE subspace_module,      ONLY : wan_eig
   USE hamiltonian_module,   ONLY : rham, ham_alloc => alloc 
   USE mp,                   ONLY : mp_sum

#ifdef __CHECK_HAMILTONIAN
   USE io_module, ONLY : work_dir, prefix, postfix 
#endif


   IMPLICIT NONE 
   !
   ! input variables
   ! 
   INTEGER,      INTENT(in) :: dimwann, nkpts
   COMPLEX(dbl), INTENT(in) :: cU(dimwann,dimwann,nkpts_g)
   
   !
   ! local variables
   !
   CHARACTER(16) :: subname = 'hamiltonian_calc'

   INTEGER       :: i, j, m, ik, ik_g, ir, inorm, imax, imin
   INTEGER       :: ierr
   REAL(dbl)     :: norm, rmod, fact
   REAL(dbl)     :: arg
   COMPLEX(dbl)  :: phase
   COMPLEX(dbl), ALLOCATABLE :: kham(:,:,:)
 
#ifdef __CHECK_HAMILTONIAN
   CHARACTER(nstrx)   :: filename
#endif
   !
   ! end of declariations
   !

!
!------------------------------
! main body
!------------------------------
!
      CALL timing(subname,OPR='start')
      CALL log_push(subname)
      !
      IF ( .NOT. ham_alloc ) CALL errore(subname,'hamiltonian data NOT alloc',1)

      !
      ! set the zero as the fermi energy
      !
      wan_eig(:,:) = wan_eig(:,:) - efermi

      ALLOCATE( kham(dimwann, dimwann, nkpts), STAT=ierr )
      IF ( ierr/=0 ) CALL errore(subname,'allocating kham', ABS(ierr) )


      !
      ! Calculate KHAM: H(k)=U^{dagger}(k) * H_0(k) * U(k)
      ! (hamiltonian matrix elements between the rotated bloch states)
      !
      DO ik = 1, nkpts
          !
          ik_g = ik + iks -1
          !
          DO j = 1, dimwann
          DO i = 1, j
              !
              kham(i,j,ik) = CZERO
              !
              DO m = 1, dimwann
                  !
                  kham(i,j,ik) = kham(i,j,ik) + wan_eig(m,ik_g) * &
                                                CONJG( cU(m,i,ik_g) ) * cU(m,j,ik_g)
              ENDDO
              !
              ! use hermiticity
              ! 
              kham(j,i,ik) = CONJG( kham(i,j,ik) )
              !
          ENDDO
          ENDDO
          !
      ENDDO

      ! 
      ! Fourier transform it: H_ij(k) --> H_ij(R) = (1/N_kpts) sum_k e^{-ikR} H_ij(k)
      !
      fact = ONE / REAL(nkpts_g, dbl)
      !
      DO ir = 1, nrtot
          !
          DO j = 1, dimwann
          DO i = 1, dimwann
              !
              rham(i,j,ir) = CZERO
              !
              DO ik = 1, nkpts
                  !
                  arg = DOT_PRODUCT( vkpt(:,ik), vr(:,ir) )
                  phase = CMPLX( COS(arg), -SIN(arg), dbl )
                  !
                  rham(i,j,ir) = rham(i,j,ir) + phase * kham(i,j,ik)
                  !
              ENDDO
              !
              rham(i,j,ir) = rham(i,j,ir) * fact
              !
          ENDDO
          ENDDO
          !
          ! recover parallelism
          CALL timing ( 'mp_sum_rham', OPR='start' )
          CALL mp_sum ( rham(:,:,ir) )
          CALL timing ( 'mp_sum_rham', OPR='stop' )
          !
      ENDDO
      
      DEALLOCATE( kham, STAT=ierr )
      IF ( ierr/=0 ) CALL errore(subname,'deallocating kham', ABS(ierr) )


      !
      ! ... writing matrix elements to file (and partially to stdout)
      !     for transport calculation
      !
      IF ( TRIM(verbosity) == 'high' .AND. ionode ) THEN
          !
          WRITE(stdout,"(/,2x,'Diagonal matrix elements of H on Wannier basis (n.n.)')")
          WRITE(stdout,"(  2x,'dimwann = ',i5)") dimwann
          !
      ENDIF

      IF ( ionode ) THEN
          !
          DO ir = 1, nrtot
              !
              ! chose nearest neighbours
              !
              inorm =  ivr(1,ir)**2 + ivr(2,ir)**2 + ivr(3,ir)**2 
              IF ( inorm <= 1 ) THEN
    
!
! added for further check purposes
!
#ifdef __CHECK_HAMILTONIAN

                   filename=TRIM(work_dir)//'/'//TRIM(prefix)//TRIM(postfix)// &
                            '_RHAM.'//TRIM(int2char( 100 + ir))
                   OPEN( UNIT=ham_unit,FILE=TRIM(filename),  STATUS='unknown', &
                         FORM='formatted', IOSTAT=ierr )
                       IF (ierr/=0) CALL errore(subname,'opening file '//TRIM(filename),ham_unit)
      
                       WRITE (ham_unit,"(2i5,3x,3i4)") dimwann, dimwann, &
                                        ( ivr(i,ir) , i=1,3 )
                       DO j = 1, dimwann
                           WRITE (ham_unit,"()")
                           DO i = 1, dimwann
                               WRITE(ham_unit, "(2f20.12)" ) rham(i,j,ir)
                           ENDDO
                       ENDDO
                   CLOSE(ham_unit)
#endif
    
                   IF ( TRIM(verbosity) == "medium" .OR. TRIM(verbosity) == "high" ) THEN
                       !
                       ! stdout (diagonal elements)
                       !
                       WRITE (stdout,"(1x,'!')")
                       WRITE (stdout,"(1x,'!',4x,'R = (',3i4,' )')") ( ivr(i,ir), i=1,3 )
                       WRITE (stdout,"( (1x,'!',3(2f11.6,',',2x)) )") (rham(i,i,ir),i =1,dimwann)
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
          WRITE(stdout,"(  2x,'  (number of R vectors (nrtot) :',i5,/)") nrtot
          WRITE(stdout,"(  4x,'#       R [cry]     |R| [Bohr]      Norm of H(R) [eV]')") 
          !
          imax = 3
          IF ( TRIM(verbosity) == "high" ) imax = MAXVAL( ivr(:,:) )
          !
          imin = 0
          IF ( TRIM(verbosity) == "high" ) imin = MINVAL( ivr(:,:) )
          !
          DO ir = 1, nrtot
              !
              IF ( ALL( ivr(:,ir) >= imin .AND. ivr(:,ir) <= imax )  ) THEN
                  !
                  ! consider only positive directions, and a cutoff of 4 cells
                  ! for each dir
                  !
                  rmod = SQRT( DOT_PRODUCT( vr(:,ir), vr(:,ir) ))
                  !
                  ! compute the 2-norm of H_ij(R)
                  !
                  norm = ZERO
                  DO j=1,dimwann
                  DO i=1,dimwann
                       norm = norm + REAL( CONJG( rham(i,j,ir)) * rham(i,j,ir) )
                  ENDDO
                  ENDDO
                  WRITE(stdout,"(1x,i4,3x,3i4,3x,f10.6,5x,f12.6)") &
                                 ir,ivr(:,ir), rmod, SQRT( norm / REAL(dimwann, dbl) )
              ENDIF
          ENDDO
          !
          !
      ENDIF

      CALL timing(subname,OPR='stop')
      CALL log_pop(subname)
      !
END SUBROUTINE hamiltonian_calc

