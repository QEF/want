MODULE lattice

  USE kinds

  IMPLICIT NONE
  SAVE

  REAL(dbl) :: dirc(3,3), recc(3,3)
  REAL(dbl) :: avec(3,3), bvec(3,3)
  REAL(dbl) :: alat = 0.0d0

CONTAINS

  SUBROUTINE lattice_init()

    USE constants, ONLY: pi, bohr => bohr_radius_angs
    USE io_module, ONLY: stdout

    INTEGER :: i,j

    !  alat read from file should be in bohr
    !  avec(:,1) == a1(:)  should be in unit of alat
    !  avec(:,2) == a2(:)  should be in unit of alat
    !  avec(:,3) == a3(:)  should be in unit of alat

    avec = avec * alat

    CALL recips( avec(:,1), avec(:,2), avec(:,3), bvec(:,1), bvec(:,2), bvec(:,3) )
    bvec = bvec * 2.0d0 * pi

    dirc = TRANSPOSE( avec ) * bohr
    recc = TRANSPOSE( bvec ) / bohr

    WRITE( stdout, * ) ' ======================================================================'
    WRITE( stdout, * ) ' =                         Lattice parameters                         ='
    WRITE( stdout, * ) ' ======================================================================'
    WRITE( stdout, * ) '  '
    WRITE( stdout, fmt= " (2x,'Alat = ', F8.4, ' (Bohr)' )" ) alat
    WRITE( stdout, fmt= " (2x,'Alat = ', F8.4, ' (Ang )' )" ) alat * bohr
    WRITE( stdout, * ) '  '
    WRITE( stdout, fmt= " (2x, 'Crystal axes:' ) ")
    WRITE( stdout, fmt="(16x,'in units of Bohr',17x,'in lattice units' )")
    DO j=1,3
       WRITE ( stdout, fmt="(4x,'a(',I1,') = (', 3F8.4, ' )     ( ',3F8.4, ' )'  )" ) &
                j, ( avec(i,j), i=1,3 ), ( avec(i,j)/alat, i=1,3 )
    END DO
    WRITE( stdout, fmt= " (2x, 'Crystal axes: (Ang)' ) ")
    DO j=1,3
       WRITE ( stdout, fmt="(4x,'a(',I1,') = (', 3F8.4, ' )'  )" ) &
               j, ( dirc(j,i), i=1,3 )
    END DO
!
    WRITE( stdout,*) ' '
    WRITE( stdout, fmt= " (2x, ' Reciprocal lattice vectors:' ) " )
    WRITE( stdout, fmt="(16x,'in units of Bohr^-1',14x,'in lattice units' )")
    DO j=1,3
       WRITE ( stdout, fmt="(4x,'b(',I1,') = (', 3F8.4, ' )     ( ',3F8.4, ' )'  )" ) &
                j, ( bvec(i,j), i=1,3 ), ( bvec(i,j)*alat / (2* pi), i=1,3 )
    END DO
    WRITE( stdout, * ) ' '


    RETURN
  END SUBROUTINE

END MODULE lattice
