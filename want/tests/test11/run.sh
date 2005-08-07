#!/bin/bash
#
# Test6
# 
#================================================================
#
# Input flags for this script (./run.sh FLAG): 
#
MANUAL=" Usage
   run.sh [FLAG]

 where FLAG is one of the following 
 (no FLAG will print this manual page) :
 
 dft             perform SCF, NSCF, PWEXPORT all together
 want            perform DISENTANGLE, WANNIER and BANDS all together
 all             perform all the above described steps

 check           check results with the reference outputs
 clean           delete all output files and the temporary directory

"
#
#================================================================
#

#
# source common enviroment, to be set before running the script
. ../environment.conf
TEST_HOME=`pwd`
TEST_NAME=Test6
PSEUDO_NAME=C.pbe-van_bm.UPF

#
# evaluate the starting choice about what is to run 

DFT=
WANT=
ALL=
CHECK=
CLEAN=




if [ $# = 0 ] ; then echo "$MANUAL" ; exit 0 ; fi
INPUT=`echo $1 | tr [:upper:] [:lower:]`

case $INPUT in
   (dft)              ;;
   (want)             ;;
   (all)              ;;
   (check)            CHECK=".TRUE." ;;
   (clean)            CLEAN=".TRUE." ;;
   (*)                echo " Invalid input FLAG, type ./run.sh for help" ; exit 1 ;;  
esac


#
# preliminaries
#
if [ -z "$CLEAN" ] ; then
   test -e $TMPDIR/$TEST_NAME || mkdir $TMPDIR/$TEST_NAME 
   cd $TMPDIR/$TEST_NAME
   ln -sf $TEST_HOME/../Pseudo/$PSEUDO_NAME .
   if [ ! -e $TEST_HOME/SCRATCH ] ; then
       cd $TEST_HOME
       ln -sf $TMPDIR/$TEST_NAME ./SCRATCH
   fi
   if [ ! -e $TMPDIR/$TEST_NAME/HOME ] ; then
       cd $TMPDIR/$TEST_NAME
       ln -sf $TEST_HOME ./HOME
   fi
   test -e $TMPDIR/$TEST_NAME/CRASH && rm $TMPDIR/$TEST_NAME/CRASH
   cd $TMPDIR/$TEST_NAME
fi

#-----------------------------------------------------------------------------

#
# running CHECK
#
if [ "$CHECK" = ".TRUE." ] ; then
   echo "running CHECK"
   #
   cd $TEST_HOME
   list=
   #
   for file in $list
   do
      $UTILITY_BIN/check.sh $file
   done
fi



#
# eventually clean
#
if [ "$CLEAN" = ".TRUE." ] ; then  
   cd $TEST_HOME
      rm -rf *.dat *.out 2> /dev/null
      test -e SCRATCH && rm SCRATCH
   cd $TMPDIR
      test -d $TEST_NAME && rm -rf $TEST_NAME
   exit 0
fi

#
# exiting
exit 0




