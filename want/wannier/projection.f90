!
! Copyright (C) 2005 Andrea Ferretti
!
! This file is distributed under the terms of the
! GNU General Public License. See the file `License'
! in the root directory of the present distribution,
! or http://www.gnu.org/copyleft/gpl.txt .
!
!*******************************************************************
   SUBROUTINE projection( lamp, ca, evc,               & 
              npwk, dimwin, dimwann, dimfroz,                &
              npwkx, nkpts, dimwinx, trial)
   !*******************************************************************
   !
   ! ...  Calculate the projection of the gaussians on the bloch eigenstates inside 
   !      energy window: store it in dimwin(ik) X dimwann overlap matrix CA
   !
   !      CA(iwann,ib,ik) = < ib, ik | iwann >
   !
   !      The scalr product is directly done in reciprocal space, providing an
   !      analytical form for the FT of the gaussian orbitals.
   !
   USE kinds
   USE constants, ONLY : CZERO, EPS_m8
   USE timing_module, ONLY : timing
   USE control_module,ONLY : verbosity
   USE util_module,   ONLY : zmat_mul, zmat_unitary
   USE becmod,        ONLY : becp
   USE trial_center_module, ONLY : trial_center, trial_center_setup

   IMPLICIT NONE

   ! ... arguments

   INTEGER :: npwkx
   INTEGER :: dimwinx
   INTEGER :: nkpts
   INTEGER :: npwk(nkpts)
   INTEGER :: dimwann
   INTEGER :: dimwin(nkpts)
   INTEGER :: dimfroz(nkpts)
   COMPLEX(dbl) :: evc( npwkx, dimwinx, nkpts )
   COMPLEX(dbl) :: lamp(dimwinx,dimwinx,nkpts)
   COMPLEX(dbl) :: ca(dimwinx,dimwann,nkpts)
   TYPE(trial_center) :: trial(dimwann)

   ! ... local variables

   INTEGER :: iwann 
   INTEGER :: ib, ik, ig
   INTEGER :: i, j, l, m 
   INTEGER :: info, ierr

      REAL(dbl), ALLOCATABLE :: s(:)
      REAL(dbl), ALLOCATABLE :: rwork2(:)
   COMPLEX(dbl), ALLOCATABLE :: aux(:), trial_vect(:)
   COMPLEX(dbl), ALLOCATABLE :: tmp(:,:), work(:)
   COMPLEX(dbl), ALLOCATABLE :: cu(:,:), u(:,:), vt(:,:)


!
! ...  End of declaration
!


      CALL timing('projection',OPR='start')

      ALLOCATE( trial_vect(npwkx), STAT = ierr )
        IF( ierr /= 0 ) CALL errore( 'projection', 'allocating gauss ', npwkx )
      ALLOCATE( aux(npwkx), STAT = ierr )
        IF( ierr /= 0 ) CALL errore( 'projection', 'allocating aux ', npwkx )

      ALLOCATE( tmp(dimwinx,dimwann), STAT = ierr )
        IF( ierr /= 0 ) CALL errore( 'projection', 'allocating tmp ', dimwann*dimwinx )
      ALLOCATE( cu(dimwinx,dimwann), STAT = ierr )
        IF( ierr /= 0 ) CALL errore( 'projection', 'allocating cu ', dimwinx*dimwann )
      ALLOCATE( u(dimwinx,dimwinx), STAT = ierr )
        IF( ierr /= 0 ) CALL errore( 'projection', 'allocating u ', dimwinx*dimwinx )
      ALLOCATE( vt(dimwann,dimwann), STAT = ierr )
        IF( ierr /= 0 ) CALL errore( 'projection', 'allocating vt ', dimwann*dimwann )

      ALLOCATE( work(4*dimwinx), STAT = ierr )
        IF( ierr /= 0 ) CALL errore( 'projection', 'allocating work ', 4*dimwinx )
      ALLOCATE( s(dimwann), STAT = ierr )
        IF( ierr /= 0 ) CALL errore( 'projection', 'allocating s ', dimwann )
      ALLOCATE( rwork2(5*dimwann), STAT = ierr )
        IF( ierr /= 0 ) CALL errore( 'projection', 'allocating rwork2 ', 5*dimwann )


       !
       ! ... kpts
       DO ik = 1, nkpts
           IF  ( dimwann >  dimfroz(ik) ) THEN  !IF  not, don't need to waste CPU time!

               !
               ! ... wannier trials
               DO iwann = 1, dimwann
                  !
                  ! set the trial centers in PW represent.
                  CALL trial_center_setup(ik, trial(iwann), npwk(ik), trial_vect)

                  !
                  ! ... bands 
                  DO ib = 1, dimwin(ik)

                     !
                     ! ... apply the US augmentation, supposing the input
                     !     localized functions behave as US orbitals
                     !
                     IF ( .NOT. ALLOCATED(becp) ) ALLOCATE( becp(1,dimwinx,nkpts))

                     CALL s_psi( npwkx, npwk(ik), 1, ik, becp(1,ib,ik), evc(1,ib,ik), aux )
                     aux(npwk(ik)+1:npwkx) = CZERO
 

                     ca(ib,iwann,ik) = CZERO    
                     DO ig = 1, npwk(ik)
                         ca(ib,iwann,ik) = ca(ib,iwann,ik) +  &
                                CONJG( aux(ig)) * trial_vect(ig)
                     ENDDO

                  ENDDO 
               ENDDO    
      
           ENDIF  
       ENDDO 

! 
! ...  Compute the dimwin(k) x dimwann matrix cu that yields, from the dimwin(k) 
!      original bloch states, the dimwann bloch-like states with maximal projection
!      onto the dimwann gaussians:
!      cu = ca.cs^{-1/2}, cs = transpose(ca).ca
! 
!      Use the singular-value decomposition of the matrix ca: 
!
!      ca = cz * cd *& cv  ,      which gives
!      cu = cz * cd * cd^{-1} * cv
!
!      where cz is dimwin(ik) x dimwin(ik) and unitary, cd is 
!      dimwin(ik) X dimwann and diagonal, cd^{-1} is dimwann X dimwann and 
!      diagonal, and cv is dimwann x dimwann and unitary.
!

       DO ik=1,nkpts
         IF ( dimwann > dimfroz(ik) ) THEN
 
           ! ... Singular value decomposition
           tmp(:,:) = ca(:,:,ik)
 
           CALL zgesvd( 'a', 'a', dimwin(ik), dimwann, tmp(1,1),              &
                dimwinx, s, u, dimwinx, vt, dimwann, work, 4*dimwinx, rwork2, info )

           IF ( info /= 0 )  &
             CALL errore( ' projection ', ' zgesvd: info has illegal value ', info )
 
           CALL zmat_mul( cu, u, 'N', vt, 'C', dimwin(ik), dimwann, dimwann ) 

           ! NOTA BENE:  cu.transpose(cu) is *NOT* an identity dimwin(ik) by dimwin(ik) 
           ! matrix, but transpose(cu).cu is a dimwann by dimwann identity matrix. 
           ! I have once checked the former statement, now I will just leave here the code
           ! for the latter (what this means is that the columns of cu are orthonormal
           ! vectors). For this reasons SIDE is set = 'left' in the next function call
           ! 
           IF ( .NOT. zmat_unitary( cu(1:dimwin(ik),1:dimwann), &
                                    SIDE='left', TOLL=EPS_m8 ) ) &
                  CALL errore('projection', 'Vectors in CU not orthonormal ',ik)
           lamp(  1:dimwin(ik), 1:dimwann , ik) = cu( 1:dimwin(ik), 1:dimwann )
         ENDIF
       ENDDO ! kpoints ik

       !
       ! local cleaning
       ! 
       DEALLOCATE( aux, STAT=ierr )
           IF (ierr/=0) CALL errore(' projection ',' deallocating aux',ABS(ierr))
       DEALLOCATE( trial_vect, STAT=ierr )
           IF (ierr/=0) CALL errore(' projection ',' deallocating gauss',ABS(ierr))
       DEALLOCATE( tmp, STAT=ierr )
           IF (ierr/=0) CALL errore(' projection ',' deallocating tmp',ABS(ierr))

       DEALLOCATE( cu, STAT=ierr )
           IF (ierr/=0) CALL errore(' projection ',' deallocating cu',ABS(ierr))
       DEALLOCATE( u, STAT=ierr )
           IF (ierr/=0) CALL errore(' projection ',' deallocating u',ABS(ierr))
       DEALLOCATE( vt, STAT=ierr )
           IF (ierr/=0) CALL errore(' projection ',' deallocating vt',ABS(ierr))
       DEALLOCATE( work, STAT=ierr )
           IF (ierr/=0) CALL errore(' projection ',' deallocating work',ABS(ierr))
       DEALLOCATE( s, STAT=ierr )
           IF (ierr/=0) CALL errore(' projection ',' deallocating s',ABS(ierr))
       DEALLOCATE( rwork2, STAT=ierr )
           IF (ierr/=0) CALL errore(' projection ',' deallocating rwork2',ABS(ierr))

       CALL timing('projection',OPR='stop')

   RETURN
   END SUBROUTINE projection
