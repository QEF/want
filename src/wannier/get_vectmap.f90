! 
! Copyright (C) 2009 WanT Group
! 
! This file is distributed under the terms of the 
! GNU General Public License. See the file `License' 
! in the root directory of the present distribution, 
! or http://www.gnu.org/copyleft/gpl.txt . 
! 
!*********************************************************
SUBROUTINE get_vectmap(nv, vect, nv_in, vect_in, use_inversion, toll2, map)
   !*********************************************************
   !
   ! generates the map giving the index of a vector equivalent to 
   ! {vect} in the set of vectors {vect_in}.
   ! Optionally, inversion symmetry can be used to define 
   ! (within toll2) whether two vectors are equivalent.
   !
   USE kinds
   USE log_module,       ONLY : log_push, log_pop
   !
   IMPLICIT NONE

   !
   ! input vars
   !
   INTEGER,      INTENT(IN)      :: nv, nv_in
   REAL(dbl),    INTENT(IN)      :: vect(3,nv), vect_in(3,nv_in)
   REAL(dbl),    INTENT(IN)      :: toll2
   LOGICAL,      INTENT(IN)      :: use_inversion
   INTEGER,      INTENT(OUT)     :: map(nv)

   !
   ! local vars
   !
   CHARACTER(11)   :: subname="get_vectmap"
   INTEGER         :: i, j
   REAL(dbl)       :: raux, vaux(3)    
   LOGICAL         :: lfound

!
!------------------------------
! main body 
!------------------------------
!
   CALL log_push(subname)

   IF ( nv    <= 0 ) CALL errore(subname,'invalid nv',10)
   IF ( nv_in <= 0 ) CALL errore(subname,'invalid nv_in',10)

   !
   ! main loop
   !
   DO i = 1, nv
       !
       map(i) = 0
       lfound = .FALSE.
       !
       DO j = 1, nv_in
           !
           ! check vectors as they are
           !
           vaux(:) = vect(:,i) - vect_in(:,j)
           raux    = DOT_PRODUCT( vaux, vaux)
           !
           IF ( ABS( raux ) < toll2 ) THEN
               lfound   = .TRUE.
               map( i ) = j 
               EXIT
           ENDIF
           
           !
           ! check vectors applying inversion if the case
           !
           IF ( .NOT. use_inversion ) CYCLE
           !
           vaux(:) = vect(:,i) + vect_in(:,j)
           raux    = DOT_PRODUCT( vaux, vaux)
           !
           IF ( ABS( raux ) < toll2 ) THEN
               lfound = .TRUE.
               map( i ) = j 
               EXIT
           ENDIF
           !
       ENDDO
       !
       IF ( .NOT. lfound ) CALL errore(subname,'unable to find vector',i) 
       !
   ENDDO


   CALL log_pop(subname)

END SUBROUTINE get_vectmap

