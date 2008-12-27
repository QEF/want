#! /bin/bash 
#
# Benzene USPP
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
 nscf            DFT nscf calc (used to obtain more conduction bands)
 pwexport        export DFT data to WanT package in IOTK fmt
 dft             perform SCF, NSCF and PWEXPORT all together
 disentangle     select the optimal subspace on which perform the wannier minimization 
 wannier         perform the above cited minimization 
 plot            compute WFs on real space for plotting
 want            perform DISENTANGLE, WANNIER and BANDS
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
SUFFIX="_US"

#
# evaluate the starting choice about what is to run 

SCF=
NSCF=
PWEXPORT=
DISENTANGLE=
WANNIER=
PLOT=
CHECK=
CLEAN=

if [ $# = 0 ] ; then echo "$MANUAL" ; exit 0 ; fi
INPUT=`echo $1 | tr [:upper:] [:lower:]`

case $INPUT in 
   (scf)            SCF=yes ;;
   (nscf)           NSCF=yes ;;
   (pwexport)       PWEXPORT=yes ;;
   (dft)            SCF=yes; NSCF=yes ; PWEXPORT=yes ;;
   (disentangle)    DISENTANGLE=yes ;;
   (wannier)        WANNIER=yes ;;
   (plot)           PLOT=yes ;;
   (want)           DISENTANGLE=yes ; WANNIER=yes ;
                    PLOT=yes ;;
   (all)            SCF=yes ; NSCF=yes ; PWEXPORT=yes ; 
                    DISENTANGLE=yes ; WANNIER=yes ; 
                    PLOT=yes ;;
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
# running DFT NSCF
#
run_dft  NAME=NSCF  SUFFIX=$SUFFIX  RUN=$NSCF

#
# running DFT PWEXPORT
#
run_export  SUFFIX=$SUFFIX  RUN=$PWEXPORT


#
# running DISENTANGLE
#
run_disentangle  SUFFIX=$SUFFIX  RUN=$DISENTANGLE  PARALLEL=no

#
# running WANNIER
#
run_wannier  SUFFIX=$SUFFIX  RUN=$WANNIER  PARALLEL=no

#
# running PLOT
#
run_plot  SUFFIX=$SUFFIX  RUN=$PLOT  PARALLEL=no



#
# running CHECK
#
if [ "$CHECK" = yes ] ; then
   echo "running CHECK..."
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

