       FUNCTION dot_bloch( vec1, vec2, isort1, isort2, mtxd1, mtxd2, mxddim, mxdgve )

!....................................................................................
!
! CALCULATES THE DOTPRODUCT <u_{n1,k1}|u_{n2,k2}> BETWEEN THE CELL-PERIODIC
! PARTS OF THE BLOCH STATES
!
! WRITTEN BY IVO SOUZA 4 OCT 2000 (BASED ON PREVIOUS VERSION OF phase.f)
!
!      INPUT:
!
!      MXDDIM      ARRAY DIMENSION OF HAMILTONIAN ROWS
!      MXDGVE      ARRAY DIMENSION FOR G-SPACE VECTORS
!      MTXD1,2     DIMENSION OF THE HAMILTONIAN AT k1,2  
!      ISORT1,2(I) G-VECTOR ASSOCIATED WITH ROW/COLUMN I OF HAMILTONIAN AT k1
!                  IS KGV(*,ISORT1(I))  
!      VEC1,2(I)   FOURIER COMPONENT OF BLOCH STATE AT K1 ASSOCIATED WITH 
!                  ROW/COLUMN I OF HAMILTONIAN, I.E., WITH G-VECTOR 
!                  KGV(*,ISORT1(I)). FOR I=MXDDIM+1 BOTH VEC1(I) AND VEC2(I)
!                  SHOULD BE SET TO ZERO AT INPUT
!               
!   
!      OUTPUT:
!
!      DOT_BLOCH   DOTPRODUCT <u_{n1,k1}|u_{n2,k2}>
!
!
!      INTERNAL:
!
!      GMAX        UPPER BOUND FOR THE G-VECTOR INDEX (THE INDEX N IN KGV(*,N))
!                  USED IN THE DOTPRODUCT 
!      INDX1,2(N)  ROW/COLUMN OF HAMILTONIAN AT k1 TO WHICH CORRESPONDS THE
!                  N-TH G-VECTOR (I.E., KGV(*,N)). IF THAT G-VECTOR IS NOT
!                  USED IN THE EXPANSION OF THE WAVE FUNCTION AT k1, THEN
!                  INDX1(N)=MXDDIM+1, WHICH IS LIKE "POINTING TO NOWHERE",
!                  SINCE VEC1(MXDDIM+1)=0. IN OTHER WORDS, INDX1 IS SORT OF 
!                  THE INVERSE OF ISORT1, OBEYING THE RELATION 
!                  INDX1(ISORT1(I))=I AT THE POINTS I OF INTEREST
!
!....................................................................................

       USE kinds   

       IMPLICIT NONE
 
       COMPLEX(dbl) :: dot_bloch

       INTEGER :: mxddim
       COMPLEX(dbl) :: vec1(mxddim+1)
       COMPLEX(dbl) :: vec2(mxddim+1)

       INTEGER :: mxdgve 
       INTEGER :: mtxd1, mtxd2 
       INTEGER :: isort1(mxddim), isort2(mxddim)

       INTEGER ::  mxdgve_loc
       PARAMETER ( mxdgve_loc = 140000 )
 
       INTEGER INDX1(MXDGVE_loc),INDX2(MXDGVE_loc),GMAX1,GMAX2,GMAX
 
       INTEGER I, J       

!
!
 
       IF ( mxdgve_loc < mxdgve ) &
            CALL errore(' dot_bloch ', ' wrong mxdgve_loc in dot_bloch ',  mxdgve_loc )
 
! ...  Calculate gmax
 
       gmax1 = isort1(1)
       DO i = 2, mtxd1
         IF ( isort1(i) >  gmax1 ) gmax1 = isort1(i)
       END DO

       gmax2 = isort2(1)
       DO i = 2, mtxd2
         IF ( isort2(i) >  gmax2 ) gmax2 = isort2(i)
       END DO             

       gmax = MAX( gmax1, gmax2 )
       DO j = 1, gmax
         indx1(j) = mxddim + 1
         indx2(j) = mxddim + 1
       END DO

       DO i = 1, mtxd1
         indx1( isort1(i) ) = i
       END DO

       DO i = 1, mtxd2
         indx2( isort2(i) ) = i
       END DO            
 
! ...  Calculate dotproduct
 
       dot_bloch = CMPLX( 0.0d0, 0.0d0 )
       DO j = 1, gmax
         dot_bloch = dot_bloch + CONJG( vec1( indx1(j) ) ) * vec2( indx2(j) )
       END DO
 
       RETURN
       END FUNCTION
