!
! Copyright (C) 2005 Andrea Ferretti
!
! This file is distributed under the terms of the
! GNU General Public License. See the file `License\'
! in the root directory of the present distribution,
! or http://www.gnu.org/copyleft/gpl.txt .
!

!*********************************************
   MODULE trial_center_module
!*********************************************
   USE kinds, ONLY : dbl
   USE constants, ONLY: bohr => bohr_radius_angs
   USE converters_module, ONLY : cart2cry
   USE parser_module, ONLY : change_case
   IMPLICIT NONE
   PRIVATE

! This module define the TYPE trial_center, used to
! managed the WFs trial localization orbitals.
!
! routines in this module:
! SUBROUTINE trial_center_init(obj)
! SUBROUTINE trial_center_convert(avec, obj)
! SUBROUTINE trial_center_setup(ik, obj, npwk, vect)
!

!
   TYPE trial_center
        CHARACTER(10)           :: type        ! center type, ("1gauss","2gauss","atomic")
        INTEGER                 :: iatom       ! atom index if "atomic" center
        INTEGER                 :: l           ! sph-harmonic l-channel
        INTEGER                 :: m           ! sph-harmonic m-channel
        INTEGER                 :: ndir        ! sph-harmonic polar-direction (1=x,2=y,3=z)
        REAL(dbl)               :: decay       ! gaussian decay factor if "?gauss" center
        REAL(dbl)               :: x1(3)       ! position of the first gaussian
        REAL(dbl)               :: x2(3)       ! position of the second gaussian
        CHARACTER(10)           :: units       ! coord units ("crystal"|"bohr"|"angstrom")
        LOGICAL                 :: alloc
   END TYPE trial_center

!
! end of declarations
!

   PUBLIC :: trial_center
   PUBLIC :: trial_center_init
   PUBLIC :: trial_center_convert
   PUBLIC :: trial_center_setup

CONTAINS

!****************************************************
   SUBROUTINE trial_center_init(obj)
   !****************************************************
   IMPLICIT NONE
   TYPE( trial_center ), INTENT(out) :: obj
      obj%type=" "
      obj%iatom=0
      obj%l=0
      obj%m=0
      obj%ndir=3
      obj%decay=0.0
      obj%x1=0.0
      obj%x2=0.0
      obj%units=" "
      obj%alloc=.FALSE.
   END SUBROUTINE trial_center_init


!****************************************************
   SUBROUTINE trial_center_convert(avec, obj)
   !****************************************************
   IMPLICIT NONE
   REAL(dbl),            INTENT(in)    :: avec(3,3)
   TYPE( trial_center ), INTENT(inout) :: obj
   CHARACTER(10)    :: units
   !
   ! ... Converting WANNIER centers to CRYSTAL units
   !     AVEC is in Bohr
   !     DECAY should be in Bohr and is converted here if is the case
   !     if units == crystal it is supposed to be already in Bohr
   !
      units=TRIM(obj%units)
      CALL change_case(units,'UPPER')
      SELECT CASE ( TRIM(units) )
      CASE ( 'ANGSTROM' )
           CALL cart2cry(obj%x1, bohr*avec, obj%units)
           CALL cart2cry(obj%x2, bohr*avec, obj%units)
           obj%decay = obj%decay / bohr
      CASE ( 'BOHR' )
           CALL cart2cry(obj%x1, avec, obj%units)
           CALL cart2cry(obj%x2, avec, obj%units)
      CASE ( 'CRYSTAL' )
      CASE DEFAULT
          CALL errore('trial_center_convert','Invalid units : '  &
                                 //TRIM(obj%units),1 )
      END SELECT

   END SUBROUTINE trial_center_convert


!****************************************************
   SUBROUTINE trial_center_setup(ik, obj, npwk, vect)
   !****************************************************
   !
   ! this routine set the G representation of the trial wannier
   ! center in input (obj) according to the chosen kpt
   ! Implemented formulas are reported below
   !
   USE constants,         ONLY : TPI, PI, ZERO, ONE, TWO, CZERO, CI
   USE kinds,             ONLY : dbl
   USE lattice_module,    ONLY : alat, tpiba, avec, bvec, omega 
   USE kpoints_module,    ONLY : vkpt     
   USE ggrids_module,     ONLY : g, igv
   USE wfc_module,        ONLY : igsort
   USE timing_module,     ONLY : timing
   USE sph_har_module,    ONLY : sph_har_setup
   USE converters_module, ONLY : cry2cart

   IMPLICIT NONE
      INTEGER,            INTENT(in) :: ik, npwk
      TYPE(trial_center), INTENT(in) :: obj
      COMPLEX(dbl),       INTENT(out):: vect(npwk)

      INTEGER                    :: i,j, ig, ierr, l, ilm, lmax2
      INTEGER                    :: igvect(3)
      REAL(dbl)                  :: decay, x1(3), x2(3), vk(3)
      REAL(dbl)                  :: arg, prefactor 
      REAL(dbl),    ALLOCATABLE  :: vkg(:,:), vkgg(:)
      REAL(dbl),    ALLOCATABLE  :: ylm(:,:)
      COMPLEX(dbl)               :: bphase(3), kphase
      COMPLEX(dbl), ALLOCATABLE  :: phase(:)

   !-------------------------------------------------------

      CALL timing('trial_center_setup',OPR='start')
      !
      ! loop over plane waves
      !
      IF ( npwk <0 ) CALL errore('trial_center_setup','Invalid npwk',-npwk)
      IF ( npwk == 0 ) RETURN

      ! TO BE eliminated XXX
      IF ( obj%l < 0 ) CALL errore('trial_center_setup','Invalid Y_l value',-obj%l)

      decay   = obj%decay 
      lmax2   = (obj%l+1)**2
      ! 
      ! go from crystal to cart (units of bohr and bohr^-1 respectively)
      x1(:) = obj%x1
      x2(:) = obj%x2
      vk(:) = vkpt(:,ik)

      CALL cry2cart(x1, avec )
      CALL cry2cart(x2, avec )
      CALL cry2cart(vk, bvec )


      SELECT CASE ( TRIM(obj%type) )
      CASE DEFAULT 
           CALL errore('trial_center_setup','invalid center TYPE'//TRIM(obj%type),1)
      CASE ('atomic' )
           CALL errore('trial_center_setup','Atomic centers not yet implemented' ,2)
      CASE ('1gauss','2gauss')

           !
           ! ... Setting the phases
           ! 
           ALLOCATE( phase(npwk), STAT=ierr )
              IF (ierr/=0) CALL errore('trial_center_setup','Allocating phase' ,ABS(ierr))
           ALLOCATE( vkg(3,npwk), STAT=ierr )
              IF (ierr/=0) CALL errore('trial_center_setup','Allocating vkg' ,ABS(ierr))
           ALLOCATE( vkgg(npwk), STAT=ierr )
              IF (ierr/=0) CALL errore('trial_center_setup','Allocating vkg2' ,ABS(ierr))
           ALLOCATE( ylm(npwk,lmax2), STAT=ierr )
              IF (ierr/=0) CALL errore('trial_center_setup','Allocating ylm' ,ABS(ierr))


           !
           ! ... bphase = e^-i b*x1
           DO i=1,3
               arg = DOT_PRODUCT( bvec(:,i) , x1(:) )
               bphase(i) = CMPLX( COS(arg), -SIN(arg) )
           ENDDO

           !
           ! ... kphase = e^-i vk*x1
           arg = DOT_PRODUCT( vk(:) , x1(:) )
           kphase = CMPLX( COS(arg), -SIN(arg) )

           !
           ! ... construct the pahses as follows:
           !     G = n1*b1 + n2*b2 + n3*b3
           !     e^{-i (k+G)*x1 ) = e^{-i k*x1} *  &
           !                  (e^{-i b1*x1})^n1 * (e^{-i b2*x1})^n2 * (e^{-i b3*x1})^n3
           DO ig = 1, npwk
                vkg(:,ig) = vk(:) + g(:,igsort(ig,ik) ) * tpiba
                vkgg(ig)  = SQRT ( DOT_PRODUCT( vkg(:,ig) , vkg(:,ig) ) )
                !
                igvect(:) = igv(:,igsort(ig,ik))
                phase(ig) = kphase * ( bphase(1) )**igvect(1) * &
                                     ( bphase(2) )**igvect(2) * &
                                     ( bphase(3) )**igvect(3) 
           ENDDO

 
           !
           ! Second Gaussian
           !
           IF ( TRIM(obj%type) == "2gauss" ) THEN
               !
               ! ... bphase = e^-i b*x2
               DO i=1,3
                   arg = DOT_PRODUCT( bvec(:,i) , x2(:) )
                   bphase(i) = CMPLX( COS(arg), -SIN(arg) )
               ENDDO
               !
               ! ... kphase = e^-i vk*x2
               arg = DOT_PRODUCT( vk(:) , x2(:) )
               kphase = CMPLX( COS(arg), -SIN(arg) )

               DO ig = 1, npwk
                   igvect(:) = igv(:,igsort(ig,ik))
                   phase(ig) = phase(ig) - kphase * ( bphase(1) )**igvect(1) * &
                                                    ( bphase(2) )**igvect(2) * &
                                                    ( bphase(3) )**igvect(3) 
               ENDDO
           ENDIF


           CALL sph_har_setup( npwk, -vkg, vkgg, obj%ndir, obj%l, obj%m, ylm(:,1) )
           ilm = 1



           !
           ! ... construct the vector
           !     Single gaussians are normalized to 1.0, the units a are as follows
           !     decay -> bohr
           !     omega -> bohr^3
           !     vkgg  -> bohr^-1
           ! 
           prefactor = ( TWO * PI**5 )**(0.25_dbl) * SQRT( 8.0_dbl * decay**3/ omega )
           arg = decay**2/ 4.0_dbl

           SELECT CASE ( obj%l ) 
           !
           ! mod = |k+G| = vkgg(ig)
           ! g_rad_l( mod ) = prefactor * e^{ -(mod*dec)^2/4.0} * ( mod * dec)^{l} * CI^l
           !
           CASE ( 0 )
               DO ig = 1, npwk
                    vect(ig) = prefactor * EXP( - vkgg(ig)**2 * arg )
                    vect(ig) =  vect(ig) * ylm(ig, ilm ) * phase(ig)
               ENDDO
           CASE ( 1 )
               prefactor = prefactor / SQRT( 3.0_dbl)
               DO ig = 1, npwk
                    vect(ig) = prefactor * EXP( - vkgg(ig)**2 * arg ) * decay * vkgg(ig)
                    vect(ig) =  vect(ig) * ylm(ig, ilm ) * phase(ig) * CI
               ENDDO
           CASE ( 2 )
               prefactor = prefactor / SQRT( 15.0_dbl)
               DO ig = 1, npwk
                    vect(ig) = prefactor * EXP( - vkgg(ig)**2 * arg ) * (decay*vkgg(ig))**2
                    vect(ig) =  vect(ig) * ylm(ig, ilm ) * phase(ig) * (-ONE)
               ENDDO
           CASE ( 3 )
               prefactor = prefactor / SQRT( 105.0_dbl)
               DO ig = 1, npwk
                    vect(ig) = prefactor * EXP( - vkgg(ig)**2 * arg ) * (decay*vkgg(ig))**3
                    vect(ig) =  vect(ig) * ylm(ig, ilm ) * phase(ig) * (-CI)
               ENDDO
           CASE DEFAULT
               CALL errore('trial_center_setup','Invalid l channel' ,ABS(obj%l) +1)
           END SELECT 

               
      END SELECT


      !
      ! ... cleaning
      ! 
      DEALLOCATE( phase, STAT=ierr )
          IF (ierr/=0) CALL errore('trial_center_setup','deallocating phase' ,ABS(ierr))
      DEALLOCATE( vkg, STAT=ierr )
          IF (ierr/=0) CALL errore('trial_center_setup','deallocating vkg' ,ABS(ierr))
      DEALLOCATE( vkgg, STAT=ierr )
          IF (ierr/=0) CALL errore('trial_center_setup','deallocating vkg2' ,ABS(ierr))
      DEALLOCATE( ylm, STAT=ierr )
          IF (ierr/=0) CALL errore('trial_center_setup','deallocating ylm' ,ABS(ierr))
      
      
      CALL timing('trial_center_setup',OPR='stop')
   END SUBROUTINE trial_center_setup

END MODULE trial_center_module

