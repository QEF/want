#!/bin/bash 
#
# Test2
# 
#================================================================
#
# This script runs all the codes in order to perform 
# a simple transport calculation, starting from PWSCF
# results, calculating Wannier functions and finally 
# using them to evaluate Landauer transmittance
# according to Fisher & Lee.
#
# Input flags for this script (./run.sh FLAG): 
#
MANUAL=" Usage
   run.sh [FLAG]

 where FLAG is one of the following 
 (no FLAG will print this manual page) :
 
 scf             PWSCF self-consistent calculation
 nscf            PWSCF non-self-consistent calculation
 pwexport        export PWSCF data to WanT package in IOTK fmt
 pwscf           perform SCF, NSCF, PWEXPORT all together
 disentangle     select the optimal subspace on which perform
                 the wannier minimization
 wannier         perform the above cited minimization
 hamiltonian     writes the hamiltonian matrix elements on the Wannier basis
 want            perform DISENTANGLE, WANNIER and HAMILTONIAN all together 
 bulk            evaluate the transmittance, for the bulk case
 conductor       as for BULK but using the general conductor geometry code
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
TEST_NAME=Test2
PSEUDO_NAME=Au11pw91.mt.UPF

#
# evaluate the starting choice about what is to run 

SCF=
NSCF=
PWEXPORT=
DISENTANGLE=
WANNIER=
HAMILTONIAN=
BULK=
CONDUCTOR=
CLEAN=

if [ $# = 0 ] ; then echo "$MANUAL" ; exit 0 ; fi
INPUT=`echo $1 | tr [:upper:] [:lower:]`

case $INPUT in 
   (scf)            SCF=".TRUE." ;;
   (nscf)           NSCF=".TRUE." ;;
   (pwexport)       PWEXPORT=".TRUE." ;;
   (pwscf)          SCF=".TRUE." ; NSCF=".TRUE." ; PWEXPORT=".TRUE." ;;
   (disentangle)    DISENTANGLE=".TRUE." ;;
   (wannier)        WANNIER=".TRUE." ;;
   (hamiltonian)    HAMILTONIAN=".TRUE." ;;
   (want)           DISENTANGLE=".TRUE." ; WANNIER=".TRUE." ;
                    HAMILTONIAN=".TRUE." ;;
   (bulk)           BULK=".TRUE." ;;
   (conductor)      CONDUCTOR=".TRUE." ;;
   (all)            SCF=".TRUE." ; NSCF=".TRUE." ; PWEXPORT=".TRUE." ; 
                    DISENTANGLE=".TRUE." ; WANNIER=".TRUE." ; 
                    HAMILTONIAN=".TRUE." ; BULK=".TRUE." ; CONDUCTOR=".TRUE." ;;
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
# running PWSCF SCF
#
if [ "$SCF" = ".TRUE." ] ; then  
   $PARA_PREFIX  $PWSCF_BIN/pw.x $PARA_POSTFIX < $TEST_HOME/scf.in > $TEST_HOME/scf.out
   if [ $? = 0 ] ; then 
      echo "SCF calculation done" 
   else
      echo "found some problems in SCF calculation, stopping" ; exit 1
   fi
fi

#
# running PWSCF NSCF
#
if [ "$NSCF" = ".TRUE." ] ; then  
   $PARA_PREFIX  $PWSCF_BIN/pw.x $PARA_POSTFIX < $TEST_HOME/nscf.in > $TEST_HOME/nscf.out
   if [ $? = 0 ] ; then 
      echo "NSCF calculation done" 
   else
      echo "found some problems in NSCF calculation, stopping" ; exit 1
   fi
fi
   
#
# running PWSCF PWEXPORT
#
if [ "$PWEXPORT" = ".TRUE." ] ; then  
   $PARA_PREFIX  $PWSCF_BIN/pw_export.x $PARA_POSTFIX  \
              <  $TEST_HOME/pwexport.in > $TEST_HOME/pwexport.out
   if [ $? = 0 ] ; then 
      echo "PWEXPORT calculation done" 
   else
      echo "found some problems in PWEXPORT calculation, stopping" ; exit 1
   fi
fi

#
# running DISENTANGLE
#
if [ "$DISENTANGLE" = ".TRUE." ] ; then  
   $WANT_BIN/disentangle.x < $TEST_HOME/want.in > $TEST_HOME/disentangle.out
   if [ ! -e CRASH ] ; then 
      echo "DISENTANGLE calculation done" 
   else
      echo "found some problems in DISENTANGLE calculation, stopping" ; cat CRASH ; exit 1
   fi
fi

#
# running WANNIER
#
if [ "$WANNIER" = ".TRUE." ] ; then  
   $WANT_BIN/wannier.x < $TEST_HOME/want.in > $TEST_HOME/wannier.out
   if [ ! -e CRASH ] ; then 
      echo "WANNIER calculation done" 
   else
      echo "found some problems in WANNIER calculation, stopping" ; cat CRASH ; exit 1
   fi
fi

#
# running HAMILTONIAN
#
if [ "$HAMILTONIAN" = ".TRUE." ] ; then  
   $WANT_BIN/hamiltonian.x < $TEST_HOME/hamiltonian.in > $TEST_HOME/hamiltonian.out
   if [ ! -e CRASH ] ; then 
      echo "HAMILTONIAN calculation done" 
   else
      echo "found some problems in HAMILTONIAN calculation, stopping" ; cat CRASH ; exit 1
   fi
fi


#
# running BULK
#
if [ "$BULK" = ".TRUE." ] ; then  
   #
   # hopefully will be improoved very soon...
   #
   ln -sf fort.103 H00.dat
   ln -sf fort.104 H01.dat
   #
   $TRANS_BIN/bulk.x < $TEST_HOME/bulk.in > $TEST_HOME/bulk.out
   if [ ! -e CRASH ] ; then 
      echo "BULK calculation done" 
      #
      # also this needs to be improoved
      #
      mv dos.out $TEST_HOME/dos_bulk.out
      mv cond.out $TEST_HOME/cond_bulk.out
   else
      echo "found some problems in BULK calculation, stopping" ; cat CRASH ; exit 1
   fi
fi


#
# running CONDUCTOR
#
if [ "$CONDUCTOR" = ".TRUE." ] ; then  
   #
   # hopefully will be improoved very soon...
   #
   ln -sf fort.103 H00_A
   ln -sf fort.103 H00_B
   ln -sf fort.103 H00_C
   ln -sf fort.104 H01_B
   ln -sf fort.104 H01_A
   ln -sf fort.104 HCI_AC
   ln -sf fort.104 HCI_CB
   #
   $TRANS_BIN/conductor.x < $TEST_HOME/conductor.in > $TEST_HOME/conductor.out
   if [ ! -e CRASH ] ; then 
      echo "CONDUCTOR calculation done" 
      #
      # also this needs to be improoved
      #
      mv dos.out cond.out $TEST_HOME
   else
      echo "found some problems in CONDUCTOR calculation, stopping" ; cat CRASH ; exit 1
   fi
fi


#
# eventually clean
#
if [ "$CLEAN" = ".TRUE." ] ; then  
   cd $TEST_HOME
      rm -rf *.out 2> /dev/null
      test -e SCRATCH && rm SCRATCH
   cd $TMPDIR
      test -d $TEST_NAME && rm -rf $TEST_NAME
   exit 0
fi

#
# exiting
echo "run.sh : everything done"
exit 0









