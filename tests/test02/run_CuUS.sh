#! /bin/bash 
#
# fcc-Copper USPP
#
#================================================================
#
# Input flags for this script (./run.sh FLAG): 
#
MANUAL=" Usage
    run_CuUS.sh [FLAG]

 where FLAG is one of the following calculation to be performed
 (no FLAG will print this manual page) :
 
 scf             DFT self-consistent calculation
 nscf            DFT non-self-consistent calculation
 pwexport        export DFT data to WanT package using IOTK fmt
 dft_bands       compute DFT bands 
 dft             perform SCF, NSCF, PWEXPORT all together
 disentangle     select the optimal subspace on which perform
                 the wannier minimization
 wannier         perform the above cited minimization
 bands           interpolates the band structure using WFs
 dos             compute DOS using WFs
 plot            compute WFs on real space for plotting
 want            perform DISENTANGLE, WANNIER, BANDS and PLOT all together
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
. $UTILITY_BIN/basedef.sh
TEST_HOME=$(pwd)
TEST_NAME=$(echo $TEST_HOME | awk -v FS=\/ '{print $NF}' )
PSEUDO_NAME=Cu.pbe-d-rrkjus.UPF

#
# evaluate the starting choice about what is to run 

SCF=
NSCF=
PWEXPORT=
DFT_BANDS=
DISENTANGLE=
WANNIER=
BANDS=
DOS=
PLOT=
CHECK=
CLEAN=

if [ $# = 0 ] ; then echo "$MANUAL" ; exit 0 ; fi
INPUT=`echo $1 | tr [:upper:] [:lower:]`

case $INPUT in 
   (scf)            SCF=".TRUE." ;;
   (nscf)           NSCF=".TRUE." ;;
   (pwexport)       PWEXPORT=".TRUE." ;;
   (dft_bands)      DFT_BANDS=".TRUE." ;;
   (dft)            SCF=".TRUE." ; NSCF=".TRUE." ; PWEXPORT=".TRUE." ;; 
   (disentangle)    DISENTANGLE=".TRUE." ;;
   (wannier)        WANNIER=".TRUE." ;;
   (bands)          BANDS=".TRUE." ;;
   (dos)            DOS=".TRUE." ;;
   (plot)           PLOT=".TRUE." ;;
   (want)           DISENTANGLE=".TRUE." ; WANNIER=".TRUE." ;
                    BANDS=".TRUE." ; DOS=".TRUE." ; PLOT=".TRUE." ;;
   (all)            SCF=".TRUE." ; NSCF=".TRUE." ; PWEXPORT=".TRUE." ; 
                    DISENTANGLE=".TRUE." ; WANNIER=".TRUE." ; 
                    BANDS=".TRUE." ; DOS=".TRUE." ; PLOT=".TRUE." ;; 
   (check)          CHECK=".TRUE." ;;
   (clean)          CLEAN=".TRUE." ;;
   (*)              echo " Invalid input FLAG, type ./run.sh for help" ; exit 1 ;;
esac

#
# switches
#
if [ "$PLOT_SWITCH" = "no" ] ; then PLOT=".FALSE." ; fi

#
# preliminaries
#
if [ -z "$CLEAN" ] ; then
   test -e $TMPDIR/$TEST_NAME || mkdir $TMPDIR/$TEST_NAME 
   cd $TMPDIR/$TEST_NAME
   ln -sf $TEST_HOME/../pseudo/$PSEUDO_NAME .
   #
   test -e $TEST_HOME/SCRATCH && rm $TEST_HOME/SCRATCH
   cd $TEST_HOME
   ln -sf $TMPDIR/$TEST_NAME ./SCRATCH
   #
   test -e $TMPDIR/$TEST_NAME/HOME && rm $TMPDIR/$TEST_NAME/HOME
   cd $TMPDIR/$TEST_NAME
   ln -sf $TEST_HOME ./HOME
   #
   test -e $TMPDIR/$TEST_NAME/CRASH && rm $TMPDIR/$TEST_NAME/CRASH

   cd $TMPDIR/$TEST_NAME
fi


#-----------------------------------------------------------------------------

#
# running DFT SCF
#
if [ "$SCF" = ".TRUE." ] ; then  
   echo $ECHO_N "running SCF calculation... $ECHO_C" 
   $PARA_PREFIX  $DFT_BIN/pw.x $PARA_POSTFIX \
                  < $TEST_HOME/scf_CuUS.in > $TEST_HOME/scf_CuUS.out
   if [ $? = 0 ] ; then 
      echo "$ECHO_T done" 
   else
      echo "$ECHO_T problems found" ; exit 1
   fi
fi

#
# running DFT NSCF
#
if [ "$NSCF" = ".TRUE." ] ; then  
   echo $ECHO_N "running NSCF calculation... $ECHO_C" 
   $PARA_PREFIX  $DFT_BIN/pw.x $PARA_POSTFIX \
                  < $TEST_HOME/nscf_CuUS.in > $TEST_HOME/nscf_CuUS.out
   if [ $? = 0 ] ; then 
      echo "$ECHO_T done" 
   else
      echo "$ECHO_T problems found" ; exit 1
   fi
fi
   
#
# running DFT PWEXPORT
#
if [ "$PWEXPORT" = ".TRUE." ] ; then  
   echo "running PWEXPORT calculation..." 
   $PARA_PREFIX  $DFT_BIN/pw_export.x $PARA_POSTFIX  \
              <  $TEST_HOME/pwexport_CuUS.in > $TEST_HOME/pwexport_CuUS.out
   if [ $? = 0 ] ; then 
      echo "done" 
   else
      echo "problems found" ; exit 1
   fi
fi

#
# running DFT_BANDS
#
if [ "$DFT_BANDS" = ".TRUE." ] ; then  
   echo $ECHO_N "running DFT_BANDS calculation... $ECHO_C" 
   $PARA_PREFIX  $DFT_BIN/pw.x $PARA_POSTFIX \
               < $TEST_HOME/dft_bands_CuUS.in > $TEST_HOME/dft_bands_CuUS.out
   if [ $? = 0 ] ; then 
      echo "$ECHO_T done" 
   else
      echo "$ECHO_T problems found" ; exit 1
   fi
fi

#
# running DOS
#
if [ "$DOS" = ".TRUE." ] ; then
   echo $ECHO_N "running DOS calculation... $ECHO_C"
   $WANT_BIN/dos.x < $TEST_HOME/dos_CuUS.in > $TEST_HOME/dos_CuUS.out
   if [ ! -e CRASH ] ; then
      echo "$ECHO_T done"
   else
      echo "$ECHO_T problems found" ; cat CRASH ; exit 1
   fi
fi
   
#
# running DISENTANGLE
#
if [ "$DISENTANGLE" = ".TRUE." ] ; then  
   echo $ECHO_N "running DISENTANGLE calculation... $ECHO_C" 
   $WANT_BIN/disentangle.x < $TEST_HOME/want_CuUS.in > $TEST_HOME/disentangle_CuUS.out
   if [ ! -e CRASH ] ; then 
      echo "$ECHO_T done" 
   else
      echo "$ECHO_T problems found" ; cat CRASH ; exit 1
   fi
fi

#
# running WANNIER
#
if [ "$WANNIER" = ".TRUE." ] ; then  
   echo $ECHO_N "running WANNIER calculation... $ECHO_C" 
   $WANT_BIN/wannier.x < $TEST_HOME/want_CuUS.in > $TEST_HOME/wannier_CuUS.out
   if [ ! -e CRASH ] ; then 
      echo "$ECHO_T done" 
   else
      echo "$ECHO_T problems found" ; cat CRASH ; exit 1
   fi
fi

#
# running BANDS
#
if [ "$BANDS" = ".TRUE." ] ; then  
   echo $ECHO_N "running BANDS calculation... $ECHO_C" 
   $WANT_BIN/bands.x < $TEST_HOME/bands_CuUS.in > $TEST_HOME/bands_CuUS.out
   if [ ! -e CRASH ] ; then 
      echo "$ECHO_T done" 
   else
      echo "$ECHO_T problems found" ; cat CRASH ; exit 1
   fi
fi

#
# running PLOT
#
if [ "$PLOT" = ".TRUE." ] ; then
   echo $ECHO_N "running PLOT calculation... $ECHO_C"
   $WANT_BIN/plot.x < $TEST_HOME/plot_CuUS.in > $TEST_HOME/plot_CuUS.out
   if [ ! -e CRASH ] ; then
      echo "$ECHO_T done"
   else
      echo "$ECHO_T problems found" ; cat CRASH ; exit 1
   fi
fi

#
# running CHECK
#
if [ "$CHECK" = ".TRUE." ] ; then
   echo "running CHECK..."
   #
   cd $TEST_HOME
   list="disentangle_CuUS.out wannier_CuUS.out "
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









