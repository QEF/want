#! /bin/bash
#
# Au Chain NCPP
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
 dos             compute DOS using WFs
 plot            compute WFs on real space for plotting
 conductor       evaluate the transmittance, for the bulk case
 want            perform DISENTANGLE, WANNIER, BANDS, DOS, PLOT, CONDUCTOR all together 
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
SUFFIX="_Au"

#
# evaluate the starting choice about what is to run 

SCF=
NSCF=
PWEXPORT=
DISENTANGLE=
WANNIER=
BANDS=
DOS=
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
   (dos)            DOS=yes ;;
   (plot)           PLOT=yes ;;
   (conductor)      CONDUCTOR=yes ;;
   (want)           DISENTANGLE=yes ; WANNIER=yes ;
                    BANDS=yes ; DOS=yes ; PLOT=yes; CONDUCTOR=yes ;;
   (all)            SCF=yes ; NSCF=yes ; PWEXPORT=yes ; 
                    DISENTANGLE=yes ; WANNIER=yes ; PLOT=yes ;
                    BANDS=yes ; DOS=yes ; CONDUCTOR=yes ;;
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
if [ "$SCF" = yes ] ; then  
   run_dft  NAME=SCF  SUFFIX=$SUFFIX
fi

#
# running DFT NSCF
#
if [ "$NSCF" = yes ] ; then  
   run_dft  NAME=NSCF  SUFFIX=$SUFFIX
fi
   
#
# running DFT PWEXPORT
#
if [ "$PWEXPORT" = yes ] ; then  
   run_export  SUFFIX=$SUFFIX
fi

#
# running DISENTANGLE
#
if [ "$DISENTANGLE" = yes ] ; then  
   run_disentangle  SUFFIX=$SUFFIX
fi

#
# running WANNIER
#
if [ "$WANNIER" = yes ] ; then  
   run_wannier  SUFFIX=$SUFFIX
fi

#
# running BANDS
#
if [ "$BANDS" = yes ] ; then  
   run_bands  SUFFIX=$SUFFIX
fi

#
# running DOS
#
if [ "$DOS" = yes ] ; then  
   run_dos  SUFFIX=$SUFFIX
fi

#
# running PLOT
#
if [ "$PLOT" = yes ] ; then  
   run_plot  SUFFIX=$SUFFIX
fi

#
# running CONDUCTOR
#
if [ "$CONDUCTOR" = yes ] ; then  
   #
   run_conductor SUFFIX=$SUFFIX
   #
   if [ ! -e CRASH ] ; then 
      echo "$ECHO_T done" 
      #
      test -e doscond.dat  &&  mv doscond.dat  $TEST_HOME/doscond$SUFFIX.dat
      test -e cond.dat     &&  mv cond.dat     $TEST_HOME/cond$SUFFIX.dat
   fi
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
if [ "$CLEAN" = yes ] ; then  
   run_clean
fi

#
# exiting
exit 0


