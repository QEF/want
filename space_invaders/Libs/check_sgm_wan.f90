! 
! Copyright (C) 2004 WanT Group
! 
! This file is distributed under the terms of the 
! GNU General Public License. See the file `License' 
! in the root directory of the present distribution, 
! or http://www.gnu.org/copyleft/gpl.txt . 
! 
!*********************************************************
SUBROUTINE check_sgm_wan(dimwann,nws,nk,ispin,rham,ie1,ie2,spectral_flag)
   !*********************************************************
   USE kinds, ONLY : dbl
   USE io_module, ONLY : stdout, work_dir, prefix, postfix, ioname
   USE constants, ONLY : PI
   USE dyn_op_read, ONLY :  read_dyn_op
   USE timing_module, ONLY : timing
   IMPLICIT NONE

! <INFO>
! General checks on the SELF-ENERGY on the wannier basis
! 
! 1) checks on the spatial decading of the \Sigma matrix elements
!    for each omega;
!    OUTPUT:            norm of each R-matrix at each omega            
! 2) prints the matrix elements for some chosen energies
!    for nearest neighbours R
! 3) knowing the hamiltonian invert the full dyson eq.
!    to obtain the spectral function (if SPECTR_FLAG == .TRUE.)
!
! NOTA BENE1: change the \delta parameter (loretzian broadening)
!             as you need
! NOTA BENE2: spin treatment as for DO_SELF_ENERGY
! 
! </INFO>
   
   REAL(dbl), PARAMETER                :: delta=0.0400
   COMPLEX(dbl), PARAMETER             :: ci=(0.0, 1.0)
   CHARACTER(9), SAVE                  :: name='sigma.wan' 

   INTEGER, INTENT(in)                 :: dimwann
   INTEGER, INTENT(in)                 :: nws
   INTEGER, INTENT(in)                 :: nk(3)
   INTEGER, INTENT(in)                 :: ispin
   INTEGER, INTENT(in)                 :: ie1, ie2
   LOGICAL, INTENT(in)                 :: spectral_flag 
   COMPLEX(dbl), INTENT(in)            :: rham(dimwann,dimwann,nws) 

   INTEGER                             :: Nv,                  &
                                          dim,                 &
                                          Nisp,                &
                                          nkpts,               &
                                          Nomega,              &
                                          Hdim,                &
                                          info

   INTEGER                             :: rvect(3),            &
                                          Nr(3)
   INTEGER, ALLOCATABLE                :: indxws(:,:),         &
                                          indxws_ind(:,:),     &
                                          ipiv(:),             &
                                          imap(:)

   REAL(dbl), ALLOCATABLE              :: dec(:,:,:)
   REAL(dbl), POINTER                  :: E(:), Vct(:,:)
   REAL(dbl), POINTER                  :: As(:,:)

   COMPLEX(dbl), POINTER               :: Sgm(:,:,:,:,:)
   COMPLEX(dbl), POINTER               :: G_ret(:,:,:,:),      &
                                          tmp(:,:),            &
                                          ident(:,:)

   CHARACTER(20)                       :: analit,              &
                                          form,                &
                                          basis
   CHARACTER(80)                       :: string

   LOGICAL                             :: INDEPENDENT
   INTEGER                             :: ie, idum
   INTEGER                             :: isp,iws, ios
   INTEGER                             :: i,j,k,l,m
   INTEGER                             :: r1,r2, i1,i2
   INTEGER                             :: index1, index2
   INTEGER                             :: ierr


!------------------------------------------------

   CALL timing('check_sgm_wan',OPR='start')
   WRITE(stdout,"(/,'Subroutine CHECK_SGM_WAN',/)") 

!
!  reading self_energy from input file
!
   CALL read_dyn_op(Nv, Vct, dim, idum, Nisp, Nomega, E, Sgm, name, analit, form, basis)


!
! some checks
!
   WRITE(stdout,"(a,4i5)") 'nws dimwann ispin Nomega: ', nws,dimwann,ispin,Nomega

   IF ( Nv /= nws .OR. ispin > Nisp .OR. dim /= dimwann)    &
        CALL errore('check_sgm_wan','Input self energy dimensions wrong',1)

   IF ( form /= 'full_matrix' )     &
        CALL errore('check_sgm_wan','Input self energy not FULL_MATRIX',2)

   IF ( basis /= 'wannier' )     &
        CALL errore('check_sgm_wan','Input self energy: wrong basis ',3)

!
! ... INDXWS_IND contains only the inequivalent R vectors (for whom the
!                DEGEN vect shoulb be all equal to 1 
!

   nkpts = PRODUCT( nk(:) )
   ALLOCATE(indxws(3,nws), STAT=ierr)
      IF(ierr/=0) CALL errore('check_sgm_wan',' allocating indxws ',3*nws)
   ALLOCATE(indxws_ind(3,nkpts), STAT=ierr)
      IF(ierr/=0) CALL errore('check_sgm_wan',' allocating indxws_ind ',3*nkpts)
   ALLOCATE(imap(nkpts), STAT=ierr)
      IF(ierr/=0) CALL errore('check_sgm_wan',' allocating imap ',nkpts)
   indxws(:,:) = NINT(Vct(:,:))
   

!
! ... determine INDXWS_IND
!

   indxws_ind(:,1) = indxws(:,1)
   imap(1) = 1
   DO i=2,nkpts   
       DO j=i,nws     
          
          INDEPENDENT=.TRUE.
          DO k=1,i-1           
              IF (  MOD( ABS(indxws(1,j)-indxws_ind(1,k)), nk(1)) == 0 .AND.    & 
                    MOD( ABS(indxws(2,j)-indxws_ind(2,k)), nk(2)) == 0 .AND.    & 
                    MOD( ABS(indxws(3,j)-indxws_ind(3,k)), nk(3)) == 0     )    &
                 INDEPENDENT=.FALSE.
          ENDDO

          IF ( INDEPENDENT ) THEN
             indxws_ind(:,i) = indxws(:,j)
             imap(i) = j
             EXIT
          ENDIF

 !         IF ( j == nws ) CALL errore('check_sgm_wan','Unable to find INDXWS_IND',1) 
       ENDDO
   ENDDO
           

   WRITE(stdout,"('SELF-ENERGY from file ',a,' successfully read',/)") TRIM(name) 
   WRITE(stdout,"('Kpt grid: ',3i4)") nk(:)
   WRITE(stdout,"(/,'R grid: total # available (crystal basis)')") 
   DO iws = 1,nws
      WRITE(stdout,"(a,i3,5x,3i3)") 'iws = ',iws, ( indxws(i,iws) , i=1,3) 
   ENDDO
   WRITE(stdout,"(/,'R grid: inequivalent vectors (crystal basis)')")
   DO iws = 1,nkpts
      WRITE(stdout,"(a,i3,5x,3i3)") 'iws = ',iws, ( indxws_ind(i,iws) , i=1,3) 
   ENDDO
   WRITE(stdout,"()") 


!
!-------------------------------------------------
! 1) SPATIAL DECADING
!    R and \omega depending norm of the Sigma sub blocks
!-------------------------------------------------
!
   ALLOCATE( dec(nws,Nisp,Nomega), STAT=ierr )
      IF(ierr/=0) CALL errore('check_sgm_wan',' allocating dec ',nws*nisp*nomega)
   
   DO ie=1,Nomega
       DO isp=ispin,ispin
       DO iws=1,nws
           
! ... norm_2 calculation
           dec(iws,isp,ie) = 0.0 
           DO j=1,dimwann
           DO i=1,dimwann
             dec(iws,isp,ie) = dec(iws,isp,ie) + Sgm(i,j,iws,isp,ie) *  &
                                          CONJG( Sgm(i,j,iws,isp,ie) )

           ENDDO
           ENDDO
      
!! ... norm_\inf calculation
!           dec(iws,isp,ie) = MAXVAL( ABS(Sgm(:,:,iws,isp,ie)) )

       ENDDO
       ENDDO
   ENDDO



   OPEN(80,FILE=TRIM(work_dir)//'/dec.dat',STATUS='unknown')
      DO ie=1,Nomega
          WRITE(80,*) E(ie), ((dec(iws,isp,ie), iws=1,nws), isp=ispin,ispin) 
      END DO
   CLOSE(80)

   DEALLOCATE( dec, STAT=ierr )
      IF(ierr/=0) CALL errore('check_sgm_wan',' deallocating dec ',ABS(ierr))



!
!-------------------------------------------------
! 2) Explicit writing of chosen MATRIX ELEMENTS
!    Plot matrix corresponding to R=0 and R=n.n.
!-------------------------------------------------
!

   IF ( ie1 > 0 .AND. ie2 <= Nomega )  THEN

      WRITE(stdout,"('Writing SIGMA NN between energy indeces:')") 
      WRITE(stdout,"(i5,'  -> ',f15.9)") ie1, E(ie1)
      WRITE(stdout,"(i5,'  -> ',f15.9)") ie2, E(ie2)
      WRITE(stdout,"()")

      OPEN(UNIT=10,FILE=TRIM(work_dir)//'/sigmaNN.dat',POSITION='rewind',STATUS='unknown')
         WRITE(10,"('SIGMA components on Wannnier basis')") 
         WRITE(10,"()")

      isp = ispin 
      DO iws=1,nkpts
         IF( ( indxws_ind(1,iws)==0 .AND.   &
               indxws_ind(2,iws)==0 .AND.   &
               indxws_ind(3,iws)==0 ) .OR.  &
             ( indxws_ind(1,iws)==1 .AND.   &
               indxws_ind(2,iws)==0 .AND.   &
               indxws_ind(3,iws)==0 ) .OR.  &
             ( indxws_ind(1,iws)==0 .AND.   &
               indxws_ind(2,iws)==1 .AND.   &
               indxws_ind(3,iws)==0 ) .OR.  &
             ( indxws_ind(1,iws)==0 .AND.   &
               indxws_ind(2,iws)==0 .AND.   &
               indxws_ind(3,iws)==1 )   )   &
         THEN
             DO ie=ie1,ie2
                  WRITE(string,"(a4,3i3,a13,f15.9)") 'R= (', (indxws_ind(i,iws),i=1,3), & 
                                                  ')     omega= ', E(ie) 
                  CALL write_op(10,dimwann,E(ie),Sgm(:,:,iws,isp,ie),TRIM(string))
             ENDDO
         ENDIF
      ENDDO

      CLOSE(10)

   ENDIF
         

!
!-------------------------------------------------
! 3) SPECTRAL FUNCTION calculation
!    Reconstruction of the Hamiltonian matrix and
!    calculation of the GREEN'S FUNCTION
!    To be consistent we use an equal number of KPTS and
!    lattice vectors R, thus we use NKPTS instead of NWS
!    Inequivalent R are stored in INDXWS_IND
!-------------------------------------------------
!

   IF ( spectral_flag )  THEN

       Hdim=dimwann*nkpts

       ALLOCATE( G_ret(Hdim,Hdim,Nisp,Nomega), STAT=ierr )
          IF(ierr/=0) CALL errore('check_sgm_wan',' allocating G_ret ',hdim**2*nisp*nomega)
       ALLOCATE(              As(Nisp,Nomega), STAT=ierr )
          IF(ierr/=0) CALL errore('check_sgm_wan',' allocating As ',nisp*nomega)
       ALLOCATE(               tmp(Hdim,Hdim), STAT=ierr )
          IF(ierr/=0) CALL errore('check_sgm_wan',' allocating tmp ',hdim**2)
       ALLOCATE(             ident(Hdim,Hdim), STAT=ierr )
          IF(ierr/=0) CALL errore('check_sgm_wan',' allocating ident ',hdim**2)
       ALLOCATE(                   ipiv(Hdim), STAT=ierr )
          IF(ierr/=0) CALL errore('check_sgm_wan',' allocating ipiv ',hdim)

       !
       ! number of lattice vectors in each direction      
       !
       Nr(:) = nk(:)

       !
       ! setting up identity
       !
       DO i2=1,Hdim
       DO i1=1,Hdim
           IF (i1==i2)  THEN
               ident(i1,i2) = 1.0
           ELSE
               ident(i1,i2) = 0.0
           ENDIF
       ENDDO
       ENDDO


       !
       ! setting up hamiltonian
       ! sigma and calculating G_ret
       !
       DO ie=1,Nomega
       DO isp=ispin,ispin

           DO r2=1,nkpts
           DO r1=1,nkpts
               DO i2=1,dimwann
               DO i1=1,dimwann
                   index1= ( r1-1)*dimwann + i1  
                   index2= ( r2-1)*dimwann + i2  

                   DO i=1,3
                       rvect(i) = MOD( indxws_ind(i,r2)-indxws_ind(i,r1)              &
                                       -MINVAL(indxws_ind(i,:)) + Nr(i), Nr(i) )      &
                                       +MINVAL(indxws_ind(i,:))
                   ENDDO
                   !
                   ! search the right index IWS
                   !
                   iws = 0
                   DO i=1,nkpts
                      IF ( indxws_ind(1,i)==rvect(1) .AND. indxws_ind(2,i)==rvect(2) .AND. &
                           indxws_ind(3,i)==rvect(3) )   iws = imap(i)
                   ENDDO
                   IF ( iws == 0) CALL errore('check_sgm_wan','Unable to find INDXWS_IND',1)
                   !
                   ! \omega*ID - H(i,j) -Sgm(i,j,\omega)
                   !
                   tmp(index1,index2) = ( E(ie) +ci*delta )* ident(index1,index2)   &
                                        -rham(i1,i2,iws) -Sgm(i1,i2,iws,isp,ie) 
               ENDDO
               ENDDO
           ENDDO
           ENDDO

           !
           ! inversion of TMP -> G_ret
           ! depending on OMEGA and SPIN
           ! 
           G_ret(:,:,isp,ie) = ident(:,:)
           CALL ZGESV(Hdim, Hdim, tmp, Hdim, ipiv, G_ret(1,1,isp,ie), Hdim, info) 
           IF ( info < 0 )   &
               CALL errore('check_sgm_wan','ZGEVS failed : wrong input param',-info)
           IF ( info > 0 )   &  
               CALL errore('check_sgm_wan','Convergence failure in ZGESV',info)

           !
           ! spectral function 
           !  
           As(isp,ie) = 0.0
           DO i1=1,Hdim 
               As(isp,ie) = As(isp,ie) - 2.0/(PI*nkpts) * AIMAG( G_ret(i1,i1,isp,ie) )
           ENDDO
 
       ENDDO
       ENDDO
       

       !
       ! writing on file 
       !
       OPEN(80,FILE=TRIM(work_dir)//'/spectral.dat',STATUS='unknown')
       DO ie=1,Nomega
           WRITE(80,"(3f15.9)") E(ie), (As(isp,ie), isp=ispin,ispin)
       ENDDO
       CLOSE(80)

       !
       ! cleaning
       !
       DEALLOCATE( G_ret, STAT=ierr )
          IF(ierr/=0) CALL errore('check_sgm_wan',' deallocating G_ret ',ABS(ierr))
       DEALLOCATE( As, STAT=ierr )
          IF(ierr/=0) CALL errore('check_sgm_wan',' deallocating As ',ABS(ierr))
       DEALLOCATE( tmp, STAT=ierr )
          IF(ierr/=0) CALL errore('check_sgm_wan',' deallocating tmp ',ABS(ierr))
       DEALLOCATE( ident, STAT=ierr )
          IF(ierr/=0) CALL errore('check_sgm_wan',' deallocating ident ',ABS(ierr))
       DEALLOCATE( ipiv, STAT=ierr )
          IF(ierr/=0) CALL errore('check_sgm_wan',' deallocating ipiv ',ABS(ierr))
   ENDIF

   DEALLOCATE( E, STAT=ierr )
       IF(ierr/=0) CALL errore('check_sgm_wan',' deallocating E ',ABS(ierr))
   DEALLOCATE( Sgm, STAT=ierr )
       IF(ierr/=0) CALL errore('check_sgm_wan',' deallocating Sgm ',ABS(ierr))
   DEALLOCATE( indxws, STAT=ierr )
       IF(ierr/=0) CALL errore('check_sgm_wan',' deallocating indxws ',ABS(ierr))
   DEALLOCATE( indxws_ind, STAT=ierr )
       IF(ierr/=0) CALL errore('check_sgm_wan',' deallocating indxws_ind ',ABS(ierr))
   DEALLOCATE( Vct, STAT=ierr )
       IF(ierr/=0) CALL errore('check_sgm_wan',' deallocating Vct',ABS(ierr))


   CALL timing('check_sgm_wan',OPR='stop')

END SUBROUTINE  check_sgm_wan



