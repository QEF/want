!
! Copyright (C) 2005 WanT Group
!
! This file is distributed under the terms of the
! GNU General Public License. See the file `License'
! in the root directory of the present distribution,
! or http://www.gnu.org/copyleft/gpl.txt .
!
! <INFO>
!*********************************************
   MODULE atomic_module
!*********************************************
   IMPLICIT NONE
   PRIVATE

!
! this module contains converters for atomic names
! and positions in the periodic tables:
!
! routines in this module:
! SUBROUTINE  atomic_name2num(symbol, zatom)
! SUBROUTINE  atomic_num2name(zatom, symbol)
!
!</INFO>


   !
   ! global data
   !
   CHARACTER(10) :: numbers='0123456789'
   CHARACTER(2)  :: elements(103)
   !
   ! set elements symbols
   !
   DATA elements /  &
 "H ",                                                                                "He",&
 "Li","Be",                                                  "B ","C ","N ","O ","F ","Ne",&
 "Na","Mg",                                                  "Al","Si","P ","S ","Cl","Ar",&
 "K ","Ca","Sc","Ti","V ","Cr","Mn","Fe","Co","Ni","Cu","Zn","Ga","Ge","As","Se","Br","Kr",&
 "Rb","Sr","Y ","Zr","Nb","Mo","Tc","Ru","Rh","Pd","Ag","Cd","In","St","Sb","Te","I ","Xe",&
 "Cs","Ba","La","Hf","Ta","W ","Re","Os","Ir","Pt","Au","Hg","Tl","Pb","Bi","Po","At","Rn",&
 "Fr","Ra","Ac", &
                "Ce","Pr","Nd","Pm","Sm","Eu","Gd","Tb","Dy","Ho","Er","Tm","Yb","Lu",&
                "Th","Pa","U ","Np","Pu","Am","Cm","Bk","Cf","Es","Fm","Md","No","Lr" /


PUBLIC :: elements
PUBLIC :: atomic_name2num
PUBLIC :: atomic_num2name

CONTAINS


!********************************************************
   SUBROUTINE atomic_name2num( symbol, zatom )
   !********************************************************
   !
   ! Convert atomic symbol in input to its related Z
   ! atomic number. symbol is a str of len 3 because 
   ! one further character is allowed. It will be deleted. 
   ! Also numbers in the symbol ('H1' etc ) will be set
   ! to blanks.
   !
   IMPLICIT NONE
   CHARACTER( 3 ),   INTENT(in)  :: symbol
   INTEGER,          INTENT(out) :: zatom
   INTEGER :: i
   CHARACTER(2)  :: lsymb

   
   !
   ! cut the third char
   !
   lsymb = symbol(1:2) 

   !
   ! if the second digit is a number set it to blank
   !
   DO i = 1, 10
      IF ( lsymb(2:2) == numbers(i:i) ) lsymb(2:2) = ' '
   ENDDO

   ! 
   ! real selection over elements
   ! 
   zatom = 0

   element_loop: &
   DO i = 1, 103
        IF ( TRIM(lsymb) == TRIM(elements(i))  ) THEN
             zatom = i
             EXIT element_loop
        ENDIF
   ENDDO element_loop
   !
   ! check the result
   !
   IF (zatom ==0 ) CALL errore('atomic_name2num','invalid symbol = '//TRIM(lsymb),1)

END SUBROUTINE atomic_name2num


!********************************************************
   SUBROUTINE atomic_num2name( zatom, symbol )
   !********************************************************
   !
   ! Convert atomic number Z to the corresponding atomic symbol
   !
   IMPLICIT NONE
   INTEGER,          INTENT(in)  :: zatom
   CHARACTER( * ),   INTENT(out) :: symbol

   IF ( zatom <= 0  ) CALL errore('atomic_num2name','Zatom <= 0',-zatom+1) 
   IF ( zatom > 103 ) CALL errore('atomic_num2name','Zatom too large',zatom) 

   symbol = TRIM( elements(zatom) )   

END SUBROUTINE atomic_num2name


END MODULE atomic_module






