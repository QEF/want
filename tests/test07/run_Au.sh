#! /bin/bash 
#
# Au Chain (2 atoms) NCPP
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
 conductor       evaluate the transmittance, for the bulk case
 want            perform DISENTANGLE, WANNIER, BANDS, CONDUCTOR all together 
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
PSEUDO_NAME=Au11pw91.mt.UPF

#
# evaluate the starting choice about what is to run 

SCF=
NSCF=
PWEXPORT=
DISENTANGLE=
WANNIER=
BANDS=
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
   (conductor)      CONDUCTOR=".TRUE." ;;
   (want)           DISENTANGLE=".TRUE." ; WANNIER=".TRUE." ;
                    BANDS=".TRUE." ; CONDUCTOR=".TRUE." ;;
   (all)            SCF=".TRUE." ; NSCF=".TRUE." ; PWEXPORT=".TRUE." ; 
                    DISENTANGLE=".TRUE." ; WANNIER=".TRUE." ; 
                    BANDS=".TRUE." ; CONDUCTOR=".TRUE." ;;
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
   $PARA_PREFIX  $DFT_BIN/pw.x $PARA_POSTFIX < $TEST_HOME/scf_Au.in > $TEST_HOME/scf_Au.out
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
   $PARA_PREFIX  $DFT_BIN/pw.x $PARA_POSTFIX < $TEST_HOME/nscf_Au.in > $TEST_HOME/nscf_Au.out
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
              <  $TEST_HOME/pwexport_Au.in > $TEST_HOME/pwexport_Au.out
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
   $WANT_BIN/disentangle.x < $TEST_HOME/want_Au.in > $TEST_HOME/disentangle_Au.out
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
   $WANT_BIN/wannier.x < $TEST_HOME/want_Au.in > $TEST_HOME/wannier_Au.out
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
   $WANT_BIN/bands.x < $TEST_HOME/bands_Au.in > $TEST_HOME/bands_Au.out
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
   #
   echo $ECHO_N "running CONDUCTOR calculation... $ECHO_C" 
   $WANT_BIN/conductor.x < $TEST_HOME/conductor_Au.in > $TEST_HOME/conductor_Au.out
   if [ ! -e CRASH ] ; then 
      echo "$ECHO_T done" 
      #
      test -e doscond.dat && mv doscond.dat $TEST_HOME/doscond_Au.dat
      test -e cond.dat    && mv cond.dat    $TEST_HOME/cond_Au.dat
   else
      echo "$ECHO_T problems found" ; cat CRASH ; exit 1
   fi
fi

#
# running CHECK
#
if [ "$CHECK" = ".TRUE." ] ; then
   echo "running CHECK... "
   #
   cd $TEST_HOME
   list="disentangle_Au.out wannier_Au.out"
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
      rm -rf *.out *.dat 2> /dev/null
      test -e SCRATCH && rm SCRATCH
   cd $TMPDIR
      test -d $TEST_NAME && rm -rf $TEST_NAME
   exit 0
fi

#
# exiting
exit 0









