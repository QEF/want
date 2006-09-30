#! /bin/bash
#
# Co Chain CoUS
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
 nscf            DFT non-self-consistent calculation
 pwexport        export DFT data to WanT package in IOTK fmt
 dft             perform SCF, NSCF, PWEXPORT all together
 disentangle     select the optimal subspace on which perform
                 the wannier minimization
 wannier         perform the above cited minimization
 bands           interpolate the band structure using WFs
 plot            compute WFs on real space for plotting
 conductor       evaluate the transmittance, for the bulk case
 want            perform DISENTANGLE, WANNIER, BANDS, PLOT, CONDUCTOR all together 
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
SUFFIX="_CoUS"

#
# evaluate the starting choice about what is to run 

SCF=
NSCF=
PWEXPORT=
DISENTANGLE=
WANNIER=
BANDS=
PLOT=
CONDUCTOR=
CHECK=
CLEAN=

if [ $# = 0 ] ; then echo "$MANUAL" ; exit 0 ; fi
INPUT=`echo $1 | tr [:upper:] [:lower:]`

case $INPUT in 
   (scf)            SCF=yes ;;
   (nscf)           NSCF=yes ;;
   (pwexport)       PWEXPORT=yes ;;
   (dft)            SCF=yes ; NSCF=yes ; PWEXPORT=yes ;;
   (disentangle)    DISENTANGLE=yes ;;
   (wannier)        WANNIER=yes ;;
   (bands)          BANDS=yes ;;
   (plot)           PLOT=yes ;;
   (conductor)      CONDUCTOR=yes ;;
   (want)           DISENTANGLE=yes ; WANNIER=yes ;
                    BANDS=yes ; PLOT=yes; CONDUCTOR=yes ;;
   (all)            SCF=yes ; NSCF=yes ; PWEXPORT=yes ; 
                    DISENTANGLE=yes ; WANNIER=yes ; PLOT=yes ;
                    BANDS=yes ; CONDUCTOR=yes ;;
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
# running PLOT
#
run_plot  SUFFIX=$SUFFIX  RUN=$PLOT

#
# running CONDUCTOR
#
run_conductor SUFFIX=$SUFFIX  RUN=$CONDUCTOR

if [ "$CONDUCTOR" = yes -a ! -e CRASH ] ; then  
    test -e doscond.dat  &&  mv doscond.dat  $TEST_HOME/doscond$SUFFIX.dat
    test -e cond.dat     &&  mv cond.dat     $TEST_HOME/cond$SUFFIX.dat
fi


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


