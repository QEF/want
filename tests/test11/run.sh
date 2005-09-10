#! /bin/bash 
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
 
 scf             DFT self-consistent calculation
 nscf            DFT non-self-consistent calculation
 pwexport        export DFT data to WanT package
 dft             perform SCF, NSCF, PWEXPORT all together
 disentangle     select the optimal subspace on which perform
                 the wannier minimization
 wannier         perform the above cited minimization
 plot            compute WFs on real space for plotting
 conductor       evaluate the transmittance across the junction
 conductor_bulk  evaluate the bulk transmittance for the leads
 want            perform DISENTANGLE, WANNIER, BANDS, PLOT, CONDUCTOR all together 
 all             perform all the above described steps

 check           check results with the reference outputs
 clean           delete all output files and the temporary directory
"
# bands           interpolates the band structure using WFs
#
#================================================================
#

#
# source common enviroment, to be set before running the script
. ../environment.conf
. $UTILITY_BIN/basedef.sh
TEST_HOME=`pwd`
TEST_NAME=Test6
PSEUDO_LIST="Pt.pz-rrkjus.UPF H.pz-rrkjus.UPF"

#
# evaluate the starting choice about what is to run 

SCF=
NSCF=
PWEXPORT=
DISENTANGLE=
WANNIER=
#BANDS=
PLOT=
CONDUCTOR=
CONDUCTOR_BULK=
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
#   (bands)          BANDS=".TRUE." ;;
   (plot)           PLOT=".TRUE." ;;
   (conductor)      CONDUCTOR=".TRUE." ;;
   (conductor_bulk) CONDUCTOR_BULK=".TRUE." ;;
   (want)           DISENTANGLE=".TRUE." ; WANNIER=".TRUE." ; 
#                   BANDS=".TRUE."; 
                    CONDUCTOR=".TRUE." ; CONDUCTOR_BULK=".TRUE." ; PLOT=".TRUE." ;;
   (all)            SCF=".TRUE." ; NSCF=".TRUE." ; PWEXPORT=".TRUE." ; 
                    DISENTANGLE=".TRUE." ; WANNIER=".TRUE." ; 
                    CONDUCTOR=".TRUE." ; CONDUCTOR_BULK=".TRUE."; PLOT=".TRUE." ;;
#                    BANDS=".TRUE." ; 
   (check)          CHECK=".TRUE." ;;
   (clean)          CLEAN=".TRUE." ;;
   (*)              echo " Invalid input FLAG, type ./run.sh for help" ; exit 1 ;;
esac

#
# preliminaries
#
if [ -z "$CLEAN" ] ; then
   test -e $TMPDIR/$TEST_NAME || mkdir $TMPDIR/$TEST_NAME 
   cd $TMPDIR/$TEST_NAME
   for item in $PSEUDO_LIST ; do
      ln -sf $TEST_HOME/../Pseudo/$item .
   done
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
# running DFT SCF
#
if [ "$SCF" = ".TRUE." ] ; then  
   echo $ECHO_N "running SCF calculation... $ECHO_C" 
   $PARA_PREFIX  $DFT_BIN/pw.x $PARA_POSTFIX < $TEST_HOME/scf.in > $TEST_HOME/scf.out
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
   $PARA_PREFIX  $DFT_BIN/pw.x $PARA_POSTFIX < $TEST_HOME/nscf.in > $TEST_HOME/nscf.out
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
              <  $TEST_HOME/pwexport.in > $TEST_HOME/pwexport.out
   if [ $? = 0 ] ; then 
      echo "done" 
   else
      echo "problems found" ; exit 1
   fi
fi

#
# running DISENTANGLE
#
if [ "$DISENTANGLE" = ".TRUE." ] ; then  
   echo $ECHO_N "running DISENTANGLE calculation... $ECHO_C" 
   $WANT_BIN/disentangle.x < $TEST_HOME/want.in > $TEST_HOME/disentangle.out
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
   $WANT_BIN/wannier.x < $TEST_HOME/want.in > $TEST_HOME/wannier.out
   if [ ! -e CRASH ] ; then 
      echo "$ECHO_T done" 
   else
      echo "$ECHO_T problems found" ; cat CRASH ; exit 1
   fi
fi

##
## running BANDS
##
#if [ "$BANDS" = ".TRUE." ] ; then  
#   echo $ECHO_N "running BANDS calculation... $ECHO_C" 
#   $WANT_BIN/bands.x < $TEST_HOME/bands.in > $TEST_HOME/bands.out
#   if [ ! -e CRASH ] ; then 
#      echo "$ECHO_T done" 
#   else
#      echo "$ECHO_T problems found" ; cat CRASH ; exit 1
#   fi
#fi


#
# running PLOT
#
if [ "$PLOT" = ".TRUE." ] ; then
   echo $ECHO_N "running PLOT calculation... $ECHO_C"
   $WANT_BIN/plot.x < $TEST_HOME/plot.in > $TEST_HOME/plot.out
   if [ ! -e CRASH ] ; then
      echo "$ECHO_T done"
   else
      echo "$ECHO_T problems found" ; cat CRASH ; exit 1
   fi
fi


#
# running CONDUCTOR
#
if [ "$CONDUCTOR" = ".TRUE." ] ; then  
   echo $ECHO_N "running CONDUCTOR calculation... $ECHO_C" 
   $TRANS_BIN/conductor.x < $TEST_HOME/conductor.in > $TEST_HOME/conductor.out
   if [ ! -e CRASH ] ; then 
      echo "$ECHO_T done" 
      mv dos.dat $TEST_HOME/dos.dat
      mv cond.dat $TEST_HOME/cond.dat
   else
      echo "$ECHO_T problems found" ; cat CRASH ; exit 1
   fi
fi


#
# running CONDUCTOR_BULK
#
if [ "$CONDUCTOR_BULK" = ".TRUE." ] ; then  
   echo $ECHO_N "running CONDUCTOR_BULK calculation... $ECHO_C" 
   $TRANS_BIN/conductor.x < $TEST_HOME/conductor_bulk.in > $TEST_HOME/conductor_bulk.out
   if [ ! -e CRASH ] ; then 
      echo "$ECHO_T done" 
      mv dos.dat $TEST_HOME/dos_bulk.dat
      mv cond.dat $TEST_HOME/cond_bulk.dat
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
   list="disentangle.out wannier.out"
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









