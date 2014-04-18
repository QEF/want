#! /bin/bash
#
# PA (polyacetylene, van Faassen geometry, PRL 88 186401 (2002) )
# Compare with test16 PA results
# 
#================================================================
#
# Input flags for this script (./run.sh FLAG): 
#
MANUAL=" Usage
   run.sh [FLAG]

 where FLAG is one of the following 
 (no FLAG will print this manual page) :
 
 bands           interpolate the band structure from CRYSTAL datafile (LDA xc)
 dos             interpolate DOS from CRYSTAL datafile (LDA xc)

 all             perform all the above described steps

 check           check results with the reference outputs
 clean           delete all output files and the temporary directory
"
#
#================================================================
#
# conductor       evaluate the transmittance, for the bulk case (LDA xc)

#
# source common enviroment
. ../environment.conf
#
# source low level macros for test
. ../../script/libtest.sh

#
# macros
SUFFIX="_PA_cry"

#
# evaluate the starting choice about what is to run 

BANDS=
DOS=
CONDUCTOR=

CHECK=
CLEAN=

if [ $# = 0 ] ; then echo "$MANUAL" ; exit 0 ; fi
INPUT=`echo $1 | tr [:upper:] [:lower:]`

case $INPUT in 
   (bands)      BANDS=yes ;;
   (dos)        DOS=yes ;;
   #(conductor)  CONDUCTOR=yes ;;
   (want)       BANDS=yes ; DOS=yes ;;  # CONDUCTOR=yes ;;
                
   (all)        BANDS=yes ; DOS=yes ;;  # CONDUCTOR=yes ;;
   (check)      CHECK=yes ;;
   (clean)      CLEAN=yes ;;
   (*)          echo " Invalid input FLAG, type ./run.sh for help" ; exit 1 ;;
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
# running BANDS
#
run_bands  NAME=BANDS  SUFFIX=${SUFFIX} RUN=$BANDS

#
# running DOS
#
run_dos  NAME=DOS  SUFFIX=${SUFFIX} RUN=$DOS

#
# running CONDUCTOR
#
run_conductor  NAME=CONDUCTOR  SUFFIX=${SUFFIX} RUN=$CONDUCTOR


#
# running CHECK
#
if [ "$CHECK" = yes ] ; then
   echo "running CHECK... "
   #
   cd $TEST_HOME
   list=""
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
if [ -d $TEST_HOME/CRYSTAL ] ; then
   cd $TEST_HOME/CRYSTAL ; rm -f *.xml.ham 2> /dev/null
   cd $TEST_HOME
fi


#
# exiting
exit 0


