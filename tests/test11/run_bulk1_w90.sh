#! /bin/bash
#
# Pt chain 1-atom, using wannier90 dataset
#
#================================================================
#
# Input flags for this script (./run.sh FLAG): 
#
MANUAL=" Usage
   run.sh [FLAG]

 where FLAG is one of the following 
 (no FLAG will print this manual page) :
 
 dos             compute DOS using WF interpolation
 bands           compute BANDS using WF interpolation
 conductor       evaluate the transmittance, for the bulk case
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
SUFFIX="_bulk1_w90"

#
# evaluate the starting choice about what is to run 

DOS=
BANDS=
CONDUCTOR=
CHECK=
CLEAN=

if [ $# = 0 ] ; then echo "$MANUAL" ; exit 0 ; fi
INPUT=`echo $1 | tr [:upper:] [:lower:]`

case $INPUT in 
   (dos)            DOS=yes ;;
   (bands)          BANDS=yes ;;
   (conductor)      CONDUCTOR=yes ;;
   (all|want)       DOS=yes ; BANDS=yes ; CONDUCTOR=yes ;;
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

#
# specific init
#
if [ -z "$CLEAN" ] ; then
   test -r ./WANNIER90/wannier90_Pt1.chk &&
         cp ./WANNIER90/wannier90_Pt1.chk ./SCRATCH/ 
   test -r ./WANNIER90/wannier90_Pt1.eig &&
         cp ./WANNIER90/wannier90_Pt1.eig ./SCRATCH/ 
fi

#-----------------------------------------------------------------------------

#
# running DOS
#
run_dos SUFFIX=$SUFFIX  RUN=$DOS

#
# running BANDS
#
run_bands SUFFIX=$SUFFIX  RUN=$BANDS

#
# running CONDUCTOR
#
run_conductor SUFFIX=$SUFFIX  RUN=$CONDUCTOR


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


