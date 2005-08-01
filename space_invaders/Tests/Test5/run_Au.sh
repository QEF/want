#!/bin/bash 
#
# Test5     Au Chain NCPP
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
 bands           interpolates the band structure using WFs
 conductor       evaluate the transmittance, for the bulk case
 want            perform DISENTANGLE, WANNIER, BANDS, CONDUCTOR all together 
 all             perform all the above described steps

 clean           delete all output files and the temporary directory
"
#
#================================================================
#

#
# source common enviroment, to be set before running the script
. ../environment.conf
TEST_HOME=`pwd`
WANT_BIN=$TEST_HOME/../../Main
TRANS_BIN=$TEST_HOME/../../Transport
TEST_NAME=Test5
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
                    BANDS=".TRUE."; CONDUCTOR=".TRUE." ;;
   (all)            SCF=".TRUE." ; NSCF=".TRUE." ; PWEXPORT=".TRUE." ; 
                    DISENTANGLE=".TRUE." ; WANNIER=".TRUE." ; 
                    BANDS=".TRUE." ; CONDUCTOR=".TRUE." ;;
   (clean)          CLEAN=".TRUE." ;;
   (*)              echo " Invalid input FLAG, type ./run.sh for help" ; exit 1 ;;
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
# running DFT SCF
#
if [ "$SCF" = ".TRUE." ] ; then  
   echo "running SCF calculation" 
   $PARA_PREFIX  $DFT_BIN/pw.x $PARA_POSTFIX \
                 < $TEST_HOME/scf_Au.in > $TEST_HOME/scf_Au.out
   if [ $? = 0 ] ; then 
      echo "done" 
   else
      echo "found some problems in SCF calculation, stopping" ; exit 1
   fi
fi

#
# running DFT NSCF
#
if [ "$NSCF" = ".TRUE." ] ; then  
   echo "running NSCF calculation" 
   $PARA_PREFIX  $DFT_BIN/pw.x $PARA_POSTFIX \
                 < $TEST_HOME/nscf_Au.in > $TEST_HOME/nscf_Au.out
   if [ $? = 0 ] ; then 
      echo "done" 
   else
      echo "found some problems in NSCF calculation, stopping" ; exit 1
   fi
fi
   
#
# running DFT PWEXPORT
#
if [ "$PWEXPORT" = ".TRUE." ] ; then  
   echo "running PWEXPORT calculation" 
   $PARA_PREFIX  $DFT_BIN/pw_export.x $PARA_POSTFIX  \
              <  $TEST_HOME/pwexport_Au.in > $TEST_HOME/pwexport_Au.out
   if [ $? = 0 ] ; then 
      echo "done" 
   else
      echo "found some problems in PWEXPORT calculation, stopping" ; exit 1
   fi
fi

#
# running DISENTANGLE
#
if [ "$DISENTANGLE" = ".TRUE." ] ; then  
   echo "running DISENTANGLE calculation" 
   $WANT_BIN/disentangle.x < $TEST_HOME/want_Au.in > $TEST_HOME/disentangle_Au.out
   if [ ! -e CRASH ] ; then 
      echo "done" 
   else
      echo "found some problems in DISENTANGLE calculation, stopping" ; cat CRASH ; exit 1
   fi
fi

#
# running WANNIER
#
if [ "$WANNIER" = ".TRUE." ] ; then  
   echo "running WANNIER calculation" 
   $WANT_BIN/wannier.x < $TEST_HOME/want_Au.in > $TEST_HOME/wannier_Au.out
   if [ ! -e CRASH ] ; then 
      echo "done" 
   else
      echo "found some problems in WANNIER calculation, stopping" ; cat CRASH ; exit 1
   fi
fi

#
# running BANDS
#
if [ "$BANDS" = ".TRUE." ] ; then  
   echo "running BANDS calculation" 
   $WANT_BIN/bands.x < $TEST_HOME/bands_Au.in > $TEST_HOME/bands_Au.out
   if [ ! -e CRASH ] ; then 
      echo "done" 
   else
      echo "found some problems in BANDS calculation, stopping" ; cat CRASH ; exit 1
   fi
fi


#
# running CONDUCTOR
#
if [ "$CONDUCTOR" = ".TRUE." ] ; then  
   #
   ln -sf RHAM.103 H00_C
   ln -sf RHAM.104 HCI_CB
   #
   echo "running CONDUCTOR calculation" 
   $TRANS_BIN/conductor.x < $TEST_HOME/conductor_Au.in > $TEST_HOME/conductor_Au.out
   if [ ! -e CRASH ] ; then 
      echo "done" 
      #
      mv dos.dat $TEST_HOME/dos_Au.dat
      mv cond.dat $TEST_HOME/cond_Au.dat
   else
      echo "found some problems in CONDUCTOR calculation, stopping" ; cat CRASH ; exit 1
   fi
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
echo "run.sh : everything done"
exit 0









