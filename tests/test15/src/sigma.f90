
!============================
   PROGRAM sigma
!============================
   USE iotk_module
   IMPLICIT NONE

   INTEGER,      PARAMETER :: dbl=KIND(1.0d0)
   COMPLEX(dbl), PARAMETER :: CI=(0.0_dbl,1.0_dbl)
   !
   INTEGER  :: dimwann  = 77
   INTEGER  :: nspin    = 1
   INTEGER  :: nomega   = 1
   INTEGER  :: imol_s   = 25
   INTEGER  :: imol_e   = 53
   LOGICAL  :: ldynam   = .FALSE.
   LOGICAL  :: binary   = .TRUE.
   !
   !
   CHARACTER(256)       :: fileout = "sigma_mol.sgm"
   CHARACTER(256)       :: filegrid = "rgrid.xml"
   !
   !
   INTEGER  :: nrtot
   INTEGER  :: i, ir, ie, ierr
   REAL(dbl):: emin, emax
   !
   INTEGER,      ALLOCATABLE :: ivr(:,:)
   REAL(dbl),    ALLOCATABLE :: wr(:), vr(:,:), grid(:)
   COMPLEX(dbl), ALLOCATABLE :: sgm(:,:,:,:)
   CHARACTER(600)       :: attr
   !
   !--------------------------------------


   !
   ! get data about R grid
   !
   CALL iotk_open_read( 10, FILE=TRIM(filegrid), IERR=ierr )
   IF ( ierr/=0 ) STOP "opening filegrid"

   CALL iotk_scan_empty( 10, "DATA", ATTR=attr)
   CALL iotk_scan_attr( attr, "nrtot", nrtot)

   ALLOCATE( ivr(3, nrtot) )
   ALLOCATE( vr(3, nrtot) )
   ALLOCATE( wr( nrtot) )

   CALL iotk_scan_dat( 10, "IVR", ivr )
   CALL iotk_scan_dat( 10, "WR", wr )
   !
   vr(:,:) = REAL( ivr(:,:) )

   CALL iotk_close_read( 10 )



   !
   ! other init and allocations
   !
   emin     = -5.0
   emax     =  5.0

   IF ( ldynam .AND. nomega == 1 ) STOP "invalid nomega"

   ALLOCATE( sgm(dimwann,dimwann, nrtot, nomega), STAT=ierr )
   IF ( ierr/=0 ) STOP "allocating sgm_diag"
 
   !
   ALLOCATE( grid(nomega) )


   !
   ! define grid
   !
   IF ( ldynam ) THEN
      !
      DO ie = 1, nomega 
         !
         grid( ie ) = emin + ( ie -1 ) * ( emax - emin ) / REAL( nomega -1 ) 
         !
      ENDDO
      !
   ELSE
      grid( : ) =  0.0
   ENDIF


   !
   ! define sgm
   !
   DO ie = 1, nomega
   DO ir = 1, nrtot
       !
       sgm( :, :, ir, ie) = 0.0
       !
       IF ( ALL( ivr(:,ir) == 0 ) ) THEN
           !
           DO i = imol_s, imol_e
              sgm( i, i, ir, ie ) = -0.60
           ENDDO
           !
       ENDIF
       !
   ENDDO
   ENDDO
   
   
   ! 
   ! write fo tile 
   ! 
   CALL iotk_open_write( 10, FILE=TRIM(fileout), BINARY=binary)
   !
   CALL iotk_write_attr( attr, "dimwann", dimwann, FIRST=.TRUE. )
   CALL iotk_write_attr( attr, "nrtot", nrtot )
   CALL iotk_write_attr( attr, "dynamical", ldynam )
   CALL iotk_write_attr( attr, "nomega", nomega )
   !
   CALL iotk_write_empty( 10, "DATA", ATTR=attr)
   !
   !
   CALL iotk_write_attr( attr, "units", "crystal", FIRST=.TRUE. )
   CALL iotk_write_dat( 10, "VR", vr, COLUMNS=3, ATTR=attr )
   !
   CALL iotk_write_attr( attr, "units", "crystal", FIRST=.TRUE. )
   CALL iotk_write_dat( 10, "IVR", ivr, COLUMNS=3, ATTR=attr )
   !
   CALL iotk_write_dat( 10, "WR", wr, ATTR=attr )
   !
   IF ( ldynam ) THEN
      !
      CALL iotk_write_attr( attr, "units", "eV", FIRST=.TRUE. )
      CALL iotk_write_dat( 10, "GRID", grid, ATTR=attr )
      !
   ENDIF
   !
   DO ie = 1, nomega
      !
      IF ( ldynam ) THEN
          CALL iotk_write_begin( 10, "OPR"//TRIM(iotk_index(ie)) )
      ELSE
          CALL iotk_write_begin( 10, "OPR" )
          IF ( ie /= 1 ) STOP "invalid ie"
      ENDIF
      !
      DO ir = 1, nrtot
          !
          CALL iotk_write_dat( 10, "VR"//TRIM(iotk_index(ir)), sgm(:,:,ir,ie) )  
          !
      ENDDO
      !
      IF ( ldynam ) THEN
         CALL iotk_write_end( 10, "OPR"//TRIM(iotk_index(ie)) )
      ELSE
         CALL iotk_write_end( 10, "OPR" )
      ENDIF
      !
   ENDDO
   !
   !
   CALL iotk_close_write( 10 )
      

   !
   ! cleaning
   !
   DEALLOCATE( sgm, grid )
   DEALLOCATE( vr, ivr, wr )

END PROGRAM sigma
