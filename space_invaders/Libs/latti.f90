       SUBROUTINE LATTI(AVEC,BVEC,VCELL,BDOT,AMINV,ADOT)

!.....................................................................
!      COMPUTES VARIOUS LATTICE PARAMETERS.
!      ADAPTED FROM SVERRE FROYEN PLANE WAVE PROGRAM
!      VERSION 4.0. 21 OCTOBER 93. JLM
!
!      INPUT:
!      AVEC(I,J)   I-TH COMPONENT OF J-TH DIRECT LATTICE VECTOR
!
!      OUTPUT:
!      AMINV(I)    2*PI/ MODULE OF LATTICE VECTOR I
!      BVEC(I,J)   I-TH COMPONENT OF J-TH RECIPROCAL LATTICE VECTOR
!      VCELL       CELL VOLUME
!      BDOT(I,J)   METRIC IN RECIPROCAL SPACE
!      ADOT(I,J)   METRIC IN DIRECT SPACE
!.....................................................................


       IMPLICIT NONE
 
       REAL*8 :: avec(3,3)
       REAL*8 :: bvec(3,3), bdot(3,3), aminv(3), adot(3,3)
       REAL*8 :: vcell
 
       INTEGER :: i, j, nt, jmax, ja
 
       REAL*8 pi,twopi,eps
       PARAMETER ( pi = 3.141592653589793d0 )
       PARAMETER ( twopi = 2.0d0*pi )
       PARAMETER ( eps = 1.0d-10 )
 
! ...  Compute the lattice wave-vectors, the cell volume and the inner products.
 
       bvec(1,1) = avec(2,2) * avec(3,3) - avec(3,2) * avec(2,3)
       bvec(2,1) = avec(3,2) * avec(1,3) - avec(1,2) * avec(3,3)
       bvec(3,1) = avec(1,2) * avec(2,3) - avec(2,2) * avec(1,3)
       bvec(1,2) = avec(2,3) * avec(3,1) - avec(3,3) * avec(2,1)
       bvec(2,2) = avec(3,3) * avec(1,1) - avec(1,3) * avec(3,1)
       bvec(3,2) = avec(1,3) * avec(2,1) - avec(2,3) * avec(1,1)
       bvec(1,3) = avec(2,1) * avec(3,2) - avec(3,1) * avec(2,2)
       bvec(2,3) = avec(3,1) * avec(1,2) - avec(1,1) * avec(3,2)
       bvec(3,3) = avec(1,1) * avec(2,2) - avec(2,1) * avec(1,2)
 
! ...  Cell volume
 
       vcell = bvec(1,1) * avec(1,1) + bvec(2,1) * avec(2,1) + bvec(3,1) * avec(3,1)
       IF ( ABS(vcell) < eps ) THEN
         WRITE (6,300) vcell
         STOP
       END IF
 
       DO j = 1, 3
         aminv(j) = avec(1,j) * avec(1,j) + avec(2,j) * avec(2,j) + avec(3,j) * avec(3,j)
         aminv(j) = twopi / SQRT( aminv(j) )
       END DO

       DO J = 1, 3
         bvec(1,j) = twopi * bvec(1,j) / vcell
         bvec(2,j) = twopi * bvec(2,j) / vcell
         bvec(3,j) = twopi * bvec(3,j) / vcell
       END DO
       vcell = ABS(vcell)
 
! ...  Printout

       WRITE(6,203)
       DO j=1,3
         WRITE(6,204) j, ( bvec(i,j), i=1,3 )
       END DO
 
       WRITE(6,205) vcell
 
! ...  Compute metric bdot(i,j)
 
       bdot(1,1) = bvec(1,1) * bvec(1,1) + bvec(2,1) * bvec(2,1) + bvec(3,1) * bvec(3,1)
       bdot(2,2) = bvec(1,2) * bvec(1,2) + bvec(2,2) * bvec(2,2) + bvec(3,2) * bvec(3,2)
       bdot(3,3) = bvec(1,3) * bvec(1,3) + bvec(2,3) * bvec(2,3) + bvec(3,3) * bvec(3,3)
       bdot(1,2) = bvec(1,1) * bvec(1,2) + bvec(2,1) * bvec(2,2) + bvec(3,1) * bvec(3,2)
       bdot(1,3) = bvec(1,1) * bvec(1,3) + bvec(2,1) * bvec(2,3) + bvec(3,1) * bvec(3,3)
       bdot(2,3) = bvec(1,2) * bvec(1,3) + bvec(2,2) * bvec(2,3) + bvec(3,2) * bvec(3,3)
       bdot(2,1) = bdot(1,2)
       bdot(3,1) = bdot(1,3)
       bdot(3,2) = bdot(2,3)
 
! ...  Compute metric in real space
 
       adot(1,1) = avec(1,1) * avec(1,1) + avec(2,1) * avec(2,1) + avec(3,1) * avec(3,1)
       adot(2,2) = avec(1,2) * avec(1,2) + avec(2,2) * avec(2,2) + avec(3,2) * avec(3,2)
       adot(3,3) = avec(1,3) * avec(1,3) + avec(2,3) * avec(2,3) + avec(3,3) * avec(3,3)
       adot(1,2) = avec(1,1) * avec(1,2) + avec(2,1) * avec(2,2) + avec(3,1) * avec(3,2)
       adot(1,3) = avec(1,1) * avec(1,3) + avec(2,1) * avec(2,3) + avec(3,1) * avec(3,3)
       adot(2,3) = avec(1,2) * avec(1,3) + avec(2,2) * avec(2,3) + avec(3,2) * avec(3,3)
       adot(2,1) = adot(1,2)
       adot(3,1) = adot(1,3)
       adot(3,2) = adot(2,3)
 
 100   FORMAT(F12.6)
 101   FORMAT(3(2X,A1,F12.6))
 102   FORMAT(I5)
 103   FORMAT(I5,3X,A2)
 203   FORMAT(/,'  RECIPROCAL LATTICE VECTORS',/,25X,'IN A.U.')
 204   FORMAT('  B',I1,'=',3(2X,E12.6))
 205   FORMAT(/,F16.6,'  CELL VOLUME')
 300   FORMAT('  *** STOPPED IN LATTI   CELL VOLUME= ',E12.4)

       RETURN
       END SUBROUTINE
