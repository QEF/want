#! /bin/bash 
#
# GaAs bulk, Using Abinit
# 
#================================================================
#
# Input flags for this script (./run.sh FLAG): 
#
MANUAL=" Usage
   run.sh [FLAG]

 where FLAG is one of the following 
 (no FLAG will print this manual page) :
 
 dft             perform SCF & NSCF calcs using Abinit

 disentangle     select the optimal subspace on which perform
                 the wannier minimization
 wannier         perform the above cited minimization
 bands           interpolates the band structure using WFs
 want            perform DISENTANGLE, WANNIER and BANDS all together
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
SUFFIX=_GaAs_abi
if [ -z "$ABINIT_BIN" ] ; then exit 0 ; fi
#
# check for ETSF-IO support
exit_if_no_etsf_support


#
# evaluate the starting choice about what is to run 

ABI=
DISENTANGLE=
WANNIER=
BANDS=
CHECK=
CLEAN=

if [ $# = 0 ] ; then echo "$MANUAL" ; exit 0 ; fi
INPUT=`echo $1 | tr [:upper:] [:lower:]`

case $INPUT in 
   (dft)            ABI=yes ;;
   (disentangle)    DISENTANGLE=yes ;;
   (wannier)        WANNIER=yes ;;
   (bands)          BANDS=yes ;;
   (want)           DISENTANGLE=yes ; WANNIER=yes ;
                    BANDS=yes ;;
   (all)            ABI=yes ; DISENTANGLE=yes ; WANNIER=yes ; 
                    BANDS=yes ;; 
   (check)          CHECK=yes ;;
   (clean)          CLEAN=yes ;;
   (*)              echo " Invalid input FLAG, type ./run.sh for help" ; exit 1 ;;
esac


#
# initialize
#
if [ -z "$CLEAN" ] ; then
   test_init
fi
#


#-----------------------------------------------------------------------------

#
# running DFT SCF & NSCF
#
run_abinit  NAME=DFT  SUFFIX=$SUFFIX  PARALLEL=no  RUN=$ABI  

#
# take care of some naming convention changes
# occured with abinit 7.
if [ "$DISENTANGLE" = "yes" ] ; then
   if [ -e ./SCRATCH/gaas-o_DS2_WFK_0-etsf.nc ] ; then 
      mv ./SCRATCH/gaas-o_DS2_WFK_0-etsf.nc ./SCRATCH/gaas-o_DS2_WFK-etsf.nc 
      echo gaas-o_DS2_WFK_0-etsf.nc moved to gaas-o_DS2_WFK-etsf.nc
   fi
fi

#
# running DISENTANGLE
#
run_disentangle  SUFFIX=$SUFFIX  RUN=$DISENTANGLE

#
# running WANNIER
#
run_wannier  SUFFIX=$SUFFIX  RUN=$WANNIER

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

