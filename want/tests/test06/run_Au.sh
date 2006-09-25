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
   (scf)            SCF=".TRUE." ;;
   (nscf)           NSCF=".TRUE." ;;
   (pwexport)       PWEXPORT=".TRUE." ;;
   (dft)            SCF=".TRUE." ; NSCF=".TRUE." ; PWEXPORT=".TRUE." ;;
   (disentangle)    DISENTANGLE=".TRUE." ;;
   (wannier)        WANNIER=".TRUE." ;;
   (bands)          BANDS=".TRUE." ;;
   (dos)            DOS=".TRUE." ;;
   (plot)           PLOT=".TRUE." ;;
   (conductor)      CONDUCTOR=".TRUE." ;;
   (want)           DISENTANGLE=".TRUE." ; WANNIER=".TRUE." ;
                    BANDS=".TRUE." ; DOS=".TRUE." ; PLOT=".TRUE."; CONDUCTOR=".TRUE." ;;
   (all)            SCF=".TRUE." ; NSCF=".TRUE." ; PWEXPORT=".TRUE." ; 
                    DISENTANGLE=".TRUE." ; WANNIER=".TRUE." ; PLOT=".TRUE." ;
                    BANDS=".TRUE." ; DOS=".TRUE." ; CONDUCTOR=".TRUE." ;;
   (check)          CHECK=".TRUE." ;;
   (clean)          CLEAN=".TRUE." ;;
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
if [ "$SCF" = ".TRUE." ] ; then  
   run_dft  NAME=SCF INPUT=scf$SUFFIX.in  OUTPUT=scf$SUFFIX.out
fi

#
# running DFT NSCF
#
if [ "$NSCF" = ".TRUE." ] ; then  
   run_dft  NAME=NSCF INPUT=nscf$SUFFIX.in  OUTPUT=nscf$SUFFIX.out
fi
   
#
# running DFT PWEXPORT
#
if [ "$PWEXPORT" = ".TRUE." ] ; then  
   run_export  INPUT=pwexport$SUFFIX.in  OUTPUT=pwexport$SUFFIX.out
fi

#
# running DISENTANGLE
#
if [ "$DISENTANGLE" = ".TRUE." ] ; then  
   run_disentangle  INPUT=want$SUFFIX.in  OUTPUT=disentangle$SUFFIX.out
fi

#
# running WANNIER
#
if [ "$WANNIER" = ".TRUE." ] ; then  
   run_wannier  INPUT=want$SUFFIX.in  OUTPUT=wannier$SUFFIX.out
fi

#
# running BANDS
#
if [ "$BANDS" = ".TRUE." ] ; then  
   run_bands  INPUT=bands$SUFFIX.in  OUTPUT=bands$SUFFIX.out
fi

#
# running DOS
#
if [ "$DOS" = ".TRUE." ] ; then  
   run_dos  INPUT=dos$SUFFIX.in  OUTPUT=dos$SUFFIX.out
fi

#
# running PLOT
#
if [ "$PLOT" = ".TRUE." ] ; then  
   run_plot  INPUT=plot$SUFFIX.in  OUTPUT=plot$SUFFIX.out
fi

#
# running CONDUCTOR
#
if [ "$CONDUCTOR" = ".TRUE." ] ; then  
   #
   run_conductor INPUT=conductor$SUFFIX.in OUTPUT=conductor$SUFFIX.out
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
if [ "$CHECK" = ".TRUE." ] ; then
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
if [ "$CLEAN" = ".TRUE." ] ; then  
   run_clean
fi

#
# exiting
exit 0









