! 
! Copyright (C) 2004 Andrea Ferretti
! 
! This file is distributed under the terms of the 
! GNU General Public License. See the file `License' 
! in the root directory of the present distribution, 
! or http://www.gnu.org/copyleft/gpl.txt . 
! 
!*********************************************************
SUBROUTINE do_self_energy(dimwann,nkpts,nws,ispin,cu,vkpt,indxws, &
                          bvec, namein, nameout)
   !*********************************************************
   USE kinds
   USE dyn_op_write
   USE dyn_op_read
   USE timing_module, ONLY : timing
   IMPLICIT NONE

! <INFO>
! Calculates the rotation self_energy operator
! following  \Sigma(k) = U^{dagger} * EAMP(k)^{dagger} \Sigma_in(k) * EAMP(k) * U(k)
! then       \Sigma(R) = 1/Nk \Sum_{k} e^{-ikR} \Sigma(k)
!
! NOTA BENE1: due to the unfortunate Castep convention, the WFC with k
!             are redefined as those in -k. This makes the need to
!             CONJG the atomic proj coeff from PWSCF (to be done before this package). 
! NOTA BENE2: \Sigma is by definition NOT hermitean.  
! NOTA BENE3: At the moment ISPIN indicates the spin component
!             to manage and the other, if eventually present, 
!             has SE set to 0.
!
! </INFO>

   REAL(dbl), PARAMETER               :: twopi = 2.0 * 3.141592653589793
   COMPLEX(dbl), PARAMETER            :: ci = ( 0.0, 1.0 )
   

   INTEGER,INTENT(in)                 :: dimwann,         &
                                         nkpts,           &
                                         ispin,           &
                                         nws,             &
                                         indxws(3,nws)
   REAL(dbl), INTENT(in)              :: bvec(3,3),       &
                                         vkpt(3,nkpts)
   CHARACTER(*), INTENT(in)           :: namein,          &             
                                         nameout
   COMPLEX(dbl), INTENT(in)           :: cu(dimwann, dimwann, nkpts) 

   
   INTEGER                            :: Nk,              &
                                         Nbands,          &
                                         Nisp,            &
                                         Nomega,          &
                                         ios,             &
                                         idum,            &
                                         iband_start,     &
                                         dimwin(nkpts),   &
                                         mxdimwin,        & 
                                         imin(nkpts),     &
                                         imax(nkpts)

   REAL(dbl), POINTER                 :: Vct(:,:),        & 
                                         E(:)

   CHARACTER(20)                      :: analit,          &
                                         form,            &
                                         basis
   CHARACTER(80)                      :: string

! the first next line should remain DBL anyway
   COMPLEX(dbl), ALLOCATABLE          :: eamp(:,:,:)
   COMPLEX(dbl), ALLOCATABLE          :: bas_rot(:,:,:)

   COMPLEX(dbl), ALLOCATABLE          :: expo(:,:)
   COMPLEX(dbl), POINTER              :: Sgm_in(:,:,:,:,:)
   COMPLEX(dbl), ALLOCATABLE          :: Sgmk(:,:,:)
   COMPLEX(dbl), ALLOCATABLE          :: Sgm_out(:,:,:,:,:)
   COMPLEX                            :: ctmp
   
   INTEGER                            :: isp,ie,iws, ierr
   INTEGER                            :: i,j,k, l,l1,l2, m,m1,m2
   
    
!--------------------------------------------------

   CALL timing('do_self_energy',OPR='start')

!
! reading EAMP matrices from 'subspace.dat'
!
   OPEN(UNIT=8,FILE='subspace.dat',FORM='unformatted',STATUS='old',IOSTAT=ios)
      IF ( ios /= 0 ) CALL errore('do_self_energy','Unable to open SUBSPACE.DAT',ios)

      READ(8) idum
      IF ( idum /= nkpts )       &
           CALL errore('do_self_energy','Wrong NKPTS from SUBSPACE.DAT',1)
      READ(8) ( imin(k), imax(k), k=1,nkpts )

      dimwin(:) = imax(:) -imin(:) +1
      mxdimwin = MAXVAL( dimwin(:) )
      ALLOCATE( eamp(mxdimwin,dimwann,nkpts), STAT=ierr )
          IF (ierr/=0) CALL errore('do_self_energy','allocating eamp', &
                       mxdimwin*dimwann*nkpts)
      eamp(:,:,:) = 0.0

      DO k=1,nkpts
          READ(8,IOSTAT=ios) idum, ( (eamp(j,i,k), j=1,dimwin(k) ), i=1,idum )
          IF ( ios /= 0 )  &
             CALL errore('do_self_energy','Unable to read from SUBSPACE.DAT',ABS(ios))
          IF ( idum /= dimwann)  &
             CALL errore('do_self_energy','Wrong DIMWANN from SUBSPACE.DAT',1)
      END DO

   WRITE(*,"(/,' File SUBSPACE.DAT successfully read',/)")
   CLOSE(8)


!
!  reading self_energy from input file
!
   CALL read_dyn_op(Nk,Vct,Nbands,iband_start,Nisp,Nomega,E,Sgm_in,namein,   &
                    analit,form,basis)

! XXX
   ie=240
   WRITE(9,"(6f15.9)") sgm_in(:,:,:,:,ie)


!
! some checks
!
   IF ( MINVAL(imin(:)) <= 0  .OR. MAXVAL(imax(:)) >=  Nbands + iband_start -1 ) &
        CALL errore('do_self_energy','Nbands inconsistent with SUBSPACE.DAT',2)

   IF ( Nk /= nkpts .OR. ispin > Nisp)    &
        CALL errore('do_self_energy','Input self energy dimensions wrong',2)

   IF ( basis /= 'bloch' )     &
        CALL errore('do_self_energy','Input self energy: wrong basis ',2)

   WRITE(*,*) 'SELF-ENERGY from file '//TRIM(namein) ,' successfully read'
   WRITE(*,*) 'in '//TRIM(form)//' FORM'

   CALL check_sgm_kpt(Nk,Vct,vkpt,bvec)
   WRITE(*,*) 'KPT grid from file '//TRIM(namein),' successfully checked'

!
! Defines the matirces BAS_ROT which will be used for each freq.
! to rotate the Bloch basis into the one leading (after Fourier Transform)
! to the maximally localized Wannier basis.
! It's just a mixing between Bloch states at the same k-pt.
!
! BAS_ROT = EAMP * CU
!
   ALLOCATE( bas_rot(mxdimwin,dimwann,nkpts), STAT=ierr )
       IF (ierr/=0) CALL errore('do_self_energy','allocating bas_rot', &
                    mxdimwin*dimwann*nkpts)
   bas_rot(:,:,:) = 0.0

   DO k=1,nkpts
      DO j=1,dimwann
      DO i=1,dimwin(k)

         DO l=1,dimwann
            bas_rot(i,j,k) = bas_rot(i,j,k) + eamp(i,l,k) * cu(l,j,k) 
         ENDDO

      ENDDO
      ENDDO
   ENDDO    

      
!
! Fourier transform staff
!
   ALLOCATE( expo(nkpts,nws), STAT=ierr )
       IF (ierr/=0) CALL errore('do_self_energy','allocating expo', nkpts*nws)
   DO iws = 1,nws
       DO k=1,nkpts
       
          expo(k,iws) = EXP( -ci * twopi * (                          &
                              vkpt(1,k) * REAL( indxws(1,iws) )     + &
                              vkpt(2,k) * REAL( indxws(2,iws) )     + &
                              vkpt(3,k) * REAL( indxws(3,iws) )   )   )
       ENDDO
   ENDDO



!
! convert Sgm_in to Sgmk having the same dimensions used
! in the code SPACE ( and those depending on it) for
! the hamiltonian matrix
!
! First eliminates the eigenvalues ot out the energy
! windows defined by IMIN and IMAX (WINDOW code)
! Then rotates the basis with the BAS_ROT matrices
! defining a self energy with DIMWANN dimension
! ( Consider the DIAGONAL case of sigma_in separatly )
!
! Then fourier transform from k to R
!
!

   ALLOCATE( Sgmk(dimwann,dimwann,nkpts), STAT=ierr)
       IF (ierr/=0) CALL errore('do_self_energy','allocating sgmk', nkpts*dimwann**2)
   ALLOCATE( Sgm_out(dimwann,dimwann,nws,Nisp,Nomega), STAT=ierr )
       IF (ierr/=0) CALL errore('do_self_energy','allocating sgm_out',  &
                                dimwann**2 * nws *nisp * nomega)


   omega: DO ie=1,Nomega
   spin:  DO isp=ispin,ispin

!
! band mixing
!
          IF ( TRIM(form) == 'diagonal' ) THEN

                 DO k=1,nkpts

                    DO j=1,dimwann
                    DO i=1,dimwann
                       Sgmk(i,j,k) = 0.0
                   
                       DO l=1,dimwin(k)
                          !
                          ! band index of Sgm_in is consistent
                          ! with PWSCF and NOT with SPACE_INVADERS
                          ! index "m" makes the conversion
                          !
                          ! IBAND_START takes into account the fact that 
                          ! we are eventually eliminating IBAND_START-1 low bands 
                          ! wrt PWSCF convention
                          !
                          m=l+imin(k) -iband_start
                          Sgmk(i,j,k) = Sgmk(i,j,k) +             &
                              CONJG( bas_rot(l,i,k) ) * Sgm_in(m,m,k,isp,ie) *  &
                              bas_rot(l,j,k)
                       
                       ENDDO
                    ENDDO
                    ENDDO
                 ENDDO

          ELSE

                 DO k=1,nkpts
    
                    DO j=1,dimwann
                    DO i=1,dimwann
                       Sgmk(i,j,k) = 0.0
                       
                       DO l1=1,dimwin(k)
                       DO l2=1,dimwin(k)
                          !
                          ! band index of Sgm_in is consistent
                          ! with PWSCF and NOT with SPACE_INVADERS
                          ! indeces "m_i" make the conversion
                          !
                          ! IBAND_START takes into account the fact that 
                          ! we are eventually eliminating IBAND_START-1 low bands 
                          ! wrt PWSCF convention
                          !
                          m1=l1+imin(k) -iband_start 
                          m2=l2+imin(k) -iband_start
                          Sgmk(i,j,k) = Sgmk(i,j,k) +                                   &
                                  CONJG( bas_rot(l1,i,k) ) * Sgm_in(m1,m2,k,isp,ie) *   &
                                  bas_rot(l2,j,k)
                   
                       ENDDO
                       ENDDO
                    ENDDO
                    ENDDO
                 ENDDO

          ENDIF



!
! Fourier transforming
!

          DO iws=1,nws
               DO j=1,dimwann
               DO i=1,dimwann
                    Sgm_out(i,j,iws,isp,ie) = 0.0

                    DO k=1,nkpts
                        Sgm_out(i,j,iws,isp,ie) = Sgm_out(i,j,iws,isp,ie)       + &
                                   expo(k,iws)*Sgmk(i,j,k)
                    ENDDO
                    Sgm_out(i,j,iws,isp,ie) = Sgm_out(i,j,iws,isp,ie) / REAL(nkpts)
               ENDDO
               ENDDO
          ENDDO


   ENDDO spin
   ENDDO omega


!
!  writing Sgm_out to file
!

   CALL write_dyn_op(nws,REAL(indxws,dbl),dimwann,iband_start,Nisp,Nomega,E,Sgm_out, &
                     nameout, 'time_ord','full_matrix','wannier')
   WRITE(*,*) 'SELF-ENERGY converted and written on file '//TRIM(nameout)




!
!  writing a second file with the elements
!  related to R=0 only to be used for transport
!  calculation
!
   DO iws=1,nws
      IF( indxws(1,iws)==0 .AND. indxws(2,iws)==0 .AND. indxws(3,iws)==0  )  THEN

          WRITE(*,"('IWS OUTPUT',i4)") iws
          OPEN(unit=80,FILE='sigma00.dat',STATUS='unknown',FORM='unformatted')
              WRITE(80) dimwann, Nisp, Nomega

          DO isp=1,Nisp
          DO ie=1,Nomega
              CALL write_op(80,dimwann,E(ie),Sgm_out(:,:,iws,isp,ie)," ")
          ENDDO
          ENDDO

          CLOSE(80)

      ENDIF
   ENDDO

!
! cleaning
!
   DEALLOCATE( expo, bas_rot, eamp, E, STAT=ierr)
       IF (ierr/=0) CALL errore('do_self_energy','deallocating expo -- E', ABS(ierr))
   DEALLOCATE( Sgm_in, Sgmk, Sgm_out, STAT=ierr)
       IF (ierr/=0) CALL errore('do_self_energy','deallocating Sgm_in Sgmk Sgm_out', ABS(ierr))

   CALL timing('do_self_energy',OPR='stop')


   END SUBROUTINE do_self_energy

