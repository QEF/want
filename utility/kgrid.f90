! 
! Copyright (C) 2005 WanT Group
!
! This file is distributed under the terms of the
! GNU General Public License. See the file `License'
! in the root directory of the present distribution,
! or http://www.gnu.org/copyleft/gpl.txt .
!
!*****************************
   PROGRAM kgrid
   !*****************************
   IMPLICIT NONE
   !
   ! This simple program computes Monkhorst-Pack kpt grids
   !
   ! INPUT: 
   !   n1 n2 n3,        INTEGER,  dimensions of the MP mesh
   !   s1 s2 s3         INETEGR,  shifts of the mesh, must be 0 or 1          
   !
   ! OUTPUT:
   !   kpoint in crystal coords, according to the formula:
   !
   ! xk_i(j) = (2*j + si)/2*ni 
   ! i   lattice comp of the kpt (i=1,3)
   ! j   index of the kpt
   !
   
   INTEGER            :: n(3)     ! dimensions of the mesh
   INTEGER            :: s(3)     ! shift
   !
   REAL, ALLOCATABLE  :: vkpt(:,:), w(:)
   !
   INTEGER            :: i,j,k, ik


   !
   !-------------------------------------------------
   !

! 
! Reading grid dimensions and shifts
! 
   WRITE(0, "( 3x, 'grid  dims (n1 n2 n3):   ', $) ")
   READ (5, *) n(1:3)
   !
   IF ( ANY( n(:) <=0 ) ) THEN
      WRITE(0,"(3x, 'grid dim should be positive')")
      STOP
   ENDIF
   !
   !
   WRITE(0, "( 3x, 'grid shift (s1 s2 s3):   ', $) ")
   READ (5, *) s(1:3)
   !
   IF ( ANY( s(:) /=0 .AND. s(:) /=1 ) ) THEN
      WRITE(0,"(3x, 'shift must be 0 or 1')")
      STOP
   ENDIF

!
! computing the grid
!
   ALLOCATE( vkpt(3, PRODUCT(n(:)) ) )
   ALLOCATE( w( PRODUCT(n(:)) ) )
   !
   ik = 0
   !
   DO k=0,n(3)-1
   DO j=0,n(2)-1
   DO i=0,n(1)-1
       !
       ik = ik + 1
       !
       vkpt(1,ik) = REAL( 2*i + s(1) )/ REAL(2*n(1))
       vkpt(2,ik) = REAL( 2*j + s(2) )/ REAL(2*n(2))
       vkpt(3,ik) = REAL( 2*k + s(3) )/ REAL(2*n(3))
       w(ik) =  2.0 / REAL(PRODUCT(n))
       !
   ENDDO
   ENDDO
   ENDDO


!
! final writing
!
   WRITE(6, "(i6)") ik
   DO i=1,ik
       WRITE(6,"(3f15.9,5x,f9.6)") vkpt(:,i), w(i)
   ENDDO

  
   DEALLOCATE( vkpt, w)
END PROGRAM kgrid


    
      
