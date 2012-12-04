#! /bin/bash
#
# Unfolding of the band structure of a H2 chain (kpt)
#
#================================================================
#
# Input flags for this script (./run.sh FLAG): 
#
MANUAL=" Usage
   run.sh [FLAG]

 where FLAG is one of the following 
 (no FLAG will print this manual page) :
 
 scf             DFT self-consistent calculation
 dft             perform SCF + any other DFT calculation
 disentangle     matrix elements and disentanglement
 wannier         wannierization

 unfold          compute translations and unfold the real space Hamiltonian
 bands           interpolate the band structure
 want            perform DISENTANGLE, WANNIER, UNFOLD, BANDS
 all             perform all the above described steps

 check           check results with the reference outputs
 clean           delete all output files and the temporary directory
"
#
#================================================================
#

#
# source common enviroment
. ../environment.conf
#
# source low level macros for test
. ../../script/libtest.sh

#
# macros
SUFFIX="_H2_kpt"

#
# evaluate the starting choice about what is to run 

SCF=
DISENTANGLE=
WANNIER=
UNFOLD=
BANDS=
CHECK=
CLEAN=

if [ $# = 0 ] ; then echo "$MANUAL" ; exit 0 ; fi
INPUT=`echo $1 | tr [:upper:] [:lower:]`

case $INPUT in 
   (scf)            SCF=yes ;;
   (dft)            SCF=yes ;;
   (disentangle)    DISENTANGLE=yes ;;
   (wannier)        WANNIER=yes ;;
   (unfold)         UNFOLD=yes ;;
   (bands)          BANDS=yes ;;
   (want)           DISENTANGLE=yes ; WANNIER=yes ;
                    UNFOLD=yes ; BANDS=yes ;;
   (all)            SCF=yes ;
                    DISENTANGLE=yes ; WANNIER=yes ;
                    UNFOLD=yes ; BANDS=yes ;; 
   (check)          CHECK=yes ;;
   (clean)          CLEAN=yes ;;
   (*)              echo " Invalid input FLAG, type ./run.sh for help" ; exit 1 ;;
esac

#
# switches
#
if [ "$PLOT_SWITCH" = "no" ] ; then PLOT=".FALSE." ; fi


#
# initialize 
#
if [ -z "$CLEAN" ] ; then
   test_init 
fi
#


#-----------------------------------------------------------------------------

#
# running DFT SCF
#
run_dft  NAME=SCF   SUFFIX=$SUFFIX  RUN=$SCF

#
# running DISENTANGLE
#
run_disentangle  SUFFIX=$SUFFIX  RUN=$DISENTANGLE

#
# running WANNIER
#
run_wannier  SUFFIX=$SUFFIX  RUN=$WANNIER

#
# running UNFOLD
#
run_unfold  SUFFIX=$SUFFIX  RUN=$BANDS

#
# running BANDS
#
run_bands  SUFFIX=$SUFFIX  RUN=$BANDS


#
# running CHECK
#
if [ "$CHECK" = yes ] ; then
   echo "running CHECK... "
   #
   cd $TEST_HOME
   list="disentangle$SUFFIX.out wannier$SUFFIX.out"
   #
   for file in $list
   do
      ../../script/check.sh $file
   done
fi


#
# eventually clean
#
run_clean  RUN=$CLEAN


#
# exiting
exit 0


