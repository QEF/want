       SUBROUTINE get_points( maxspts, maxpts, nspts, npts, bdot, skpt, kpt, xval, sxval, tnkpts )

!..................................................................................................
!
!      DETERMINES THE K-POINTS FOR CALCULATING THE BAND STRUCTURE
!
!      WRITTEN DECEMBER 15, 1997 BY IVO SOUZA
!
!      INPUT:
!
!      MAXSPTS     MAXIMUM NUMBER OF SPECIAL K-POINTS
!      MAXPTS      MAXIMUM NUMBER OF K-POINTS BETWEEN TWO CONSECUTIVE
!                  SPECIAL K-POINTS
!      NSPTS       NUMBER OF SPECIAL K-POINTS
!      NPTS        NUMBER OF (EVENLY SPACED) K-POINTS BETWEEN THE FIRST TWO
!                  SPECIAL K-POINTS (THIS FIXES THE DENSITY OF K-POINTS FOR
!                  THE WHOLE PLOT) 
!      BDOT(I,J)   METRIC IN RECIPROCAL SPACE
!      SKPT(I,J)   I-TH COMPONENT (RECIPROCAL LATTICE COORDINATES) OF THE
!                  J-TH SPECIAL K-POINT
!
!      OUTPUT:
!
!      KPT(I,J)    I-TH COMPONENT (RECIPROCAL LATTICE COORDINATES) OF THE
!                  J-TH K-POINT USED TO PLOT THE BAND STRUCTURE
!      XVAL(K)     ABCISSA VALUE ON THE BAND STRUCTURE PLOT FOR THE K-TH
!                  K-POINT
!      SXVAL(J)    ABCISSA VALUE ON THE BAND STRUCTURE PLOT FOR THE J-TH
!                  SPECIAL K-POINT
!      TNKPTS      NUMBER OF K-POINTS USED TO PLOT THE BAND STRUCTURE
!
!..................................................................................................


       IMPLICIT NONE
 
       INTEGER :: maxspts, maxpts
       INTEGER :: nspts, npts, tnkpts
       REAL*8 :: bdot(3,3)
       REAL*8 :: skpt(3,maxpts)
       REAL*8 :: xval(maxspts*maxpts)
       REAL*8 :: sxval(maxspts)
       REAL*8 :: kpt(3,maxspts*maxpts)
 
       INTEGER :: i, j, n
       REAL*8 :: length0, length
       REAL*8 :: vec(3)
       REAL*8 :: eps
       PARAMETER( eps = 1.e-6 )


 
       vec(1) = skpt(1,2) - skpt(1,1)
       vec(2) = skpt(2,2) - skpt(2,1)
       vec(3) = skpt(3,2) - skpt(3,1)
       length0 = vec(1) * ( bdot(1,1) * vec(1) + bdot(1,2) * vec(2) + bdot(1,3) * vec(3) ) +   &
                 vec(2) * ( bdot(2,1) * vec(1) + bdot(2,2) * vec(2) + bdot(2,3) * vec(3) ) +   &
                 vec(3) * ( bdot(3,1) * vec(1) + bdot(3,2) * vec(2) + bdot(3,3) * vec(3) ) 
       length0 = SQRT( length0 )

       IF ( length0 < eps ) STOP '*** ERROR *** IN GET_POINTS: LENGTH0 TOO SMALL'
 
       tnkpts = 0
       DO i = 1, nspts - 1
         vec(1) = skpt(1,i+1) - skpt(1,i)
         vec(2) = skpt(2,i+1) - skpt(2,i)
         vec(3) = skpt(3,i+1) - skpt(3,i)
         length = vec(1) * ( bdot(1,1) * vec(1) + bdot(1,2) * vec(2) + bdot(1,3) * vec(3) ) +  &
                  vec(2) * ( bdot(2,1) * vec(1) + bdot(2,2) * vec(2) + bdot(2,3) * vec(3) ) +  &
                  vec(3) * ( bdot(3,1) * vec(1) + bdot(3,2) * vec(2) + bdot(3,3) * vec(3) ) 
         length = SQRT( length )

         IF ( LENGTH <  EPS ) STOP '*** ERROR *** IN GET_POINTS: LENGTH TOO SMALL'
 
         n = nint( DBLE(npts) * length / length0 )
         IF ( n ==  0 ) STOP '*** ERROR *** IN get_points.f'
 
         DO j = 1, n
           tnkpts = tnkpts + 1
           IF ( tnkpts+1 > maxspts*maxpts ) STOP '*** ERROR *** IN GET_POINTS'

           IF ( tnkpts ==  1 ) THEN
             xval(tnkpts) = 0.d0
           ELSE
             xval(tnkpts) = xval(tnkpts-1) + length/DBLE(n)
           END IF

           IF ( j ==  1 ) sxval(i) = xval(tnkpts)

           kpt(1,tnkpts) = skpt(1,i) + ( skpt(1,i+1) - skpt(1,i) ) * DBLE(j-1) / DBLE(n)  
           kpt(2,tnkpts) = skpt(2,i) + ( skpt(2,i+1) - skpt(2,i) ) * DBLE(j-1) / DBLE(n)  
           kpt(3,tnkpts) = skpt(3,i) + ( skpt(3,i+1) - skpt(3,i) ) * DBLE(j-1) / DBLE(n)  
         END DO

       END DO
       
! ...  Last point
 
       tnkpts = tnkpts + 1
       xval(tnkpts) = xval(tnkpts-1) + length/DBLE(n)
       kpt(1,tnkpts) = skpt(1,nspts)
       kpt(2,tnkpts) = skpt(2,nspts)
       kpt(3,tnkpts) = skpt(3,nspts)
       sxval(nspts) = xval(tnkpts) 
       
       RETURN
       END SUBROUTINE
                    
