#!/bin/sh 
#
# Test1
# 
#================================================================
#
# This script runs all the codes in order to perform 
# the calculation fo the Wannier functions for 
# bulk Silicon. The disentangle procedure is not used
# and only the valence band subspace is described.
# No transport calculation is performed.
#
# Input flags for this script (./run.sh FLAG): 
#
MANUAL=" Usage
   run.sh [FLAG]

 where FLAG is one of the following 
 (no FLAG will print this manual page) :
 
 scf             PWSCF self-consistent calculation
 nscf            PWSCF non-self-consistent calculation
 pw2wan          export PWSCF data to WanT package
 pwscf           perform SCF, NSCF, PW2WAN all together
 window          determine which bads are in the energy window
                 of interest and then write data accoding to it
 disentangle     select the optimal subspace on which perform
                 the wannier minimization
 wannier         perform the above cited minimization
 hamiltonian     writes the hamiltonian matrix elements on the Wannier basis
 want            perform WINDOW, DISENTANGLE, WANNIER and HAMILTONIAN all together 
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
TEST_NAME=Test1

#
# evaluate the starting choice about what is to run 

SCF=
NSCF=
PW2WAN=
WINDOW=
DISENTANGLE=
WANNIER=
HAMILTONIAN=
CLEAN=

if [ $# = 0 ] ; then echo "$MANUAL" ; exit 0 ; fi
INPUT=`echo $1 | tr [:upper:] [:lower:]`

case $INPUT in 
   (scf)            SCF=".TRUE." ;;
   (nscf)           NSCF=".TRUE." ;;
   (pw2wan)         PW2WAN=".TRUE." ;;
   (pwscf)          SCF=".TRUE." ; NSCF=".TRUE." ; PW2WAN=".TRUE." ;;
   (window)         WINDOW=".TRUE." ;;
   (disentangle)    DISENTANGLE=".TRUE." ;;
   (wannier)        WANNIER=".TRUE." ;;
   (hamiltonian)    HAMILTONIAN=".TRUE." ;;
   (want)           WINDOW=".TRUE." ; DISENTANGLE=".TRUE." ; WANNIER=".TRUE." ;
                    HAMILTONIAN=".TRUE." ;;
   (all)            SCF=".TRUE." ; NSCF=".TRUE." ; PW2WAN=".TRUE." ; 
                    WINDOW=".TRUE." ; DISENTANGLE=".TRUE." ; WANNIER=".TRUE." ; 
                    HAMILTONIAN=".TRUE." ;; 
   (clean)          CLEAN=".TRUE." ;;
   (*)              echo " Invalid input FLAG, type ./run.sh for help" ; exit 1 ;;
esac

#
# preliminaries
#
if [ -z "$CLEAN" ] ; then
   test -e $TMPDIR/$TEST_NAME || mkdir $TMPDIR/$TEST_NAME 
   cd $TMPDIR/$TEST_NAME
   ln -sf $TEST_HOME/../Pseudo/*.UPF .
   if [ ! -e $TEST_HOME/SCRATCH ] ; then
       cd $TEST_HOME
       ln -sf $TMPDIR/$TEST_NAME ./SCRATCH
   fi
   if [ ! -e $TMPDIR/$TEST_NAME/HOME ] ; then
       cd $TMPDIR/$TEST_NAME
       ln -sf $TEST_HOME ./HOME
   fi
   test -e $TMPDIR/$TEST_NAME/CRASH && rm $TMPDIR/$TEST_NAME/CRASH
fi


#-----------------------------------------------------------------------------
cd $TMPDIR/$TEST_NAME

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
# running PWSCF PW2WAN
#
if [ "$PW2WAN" = ".TRUE." ] ; then  
   $PARA_PREFIX  $PWSCF_BIN/pw2wan.x $PARA_POSTFIX  \
              <  $TEST_HOME/pw2wan.in > $TEST_HOME/pw2wan.out
   if [ $? = 0 ] ; then 
      echo "PW2WAN calculation done" 
   else
      echo "found some problems in PW2WAN calculation, stopping" ; exit 1
   fi
fi

#
# running WINDOW
#
if [ "$WINDOW" = ".TRUE." ] ; then  
   $WANT_BIN/window.x < $TEST_HOME/want.in > $TEST_HOME/window.out
   if [ ! -e CRASH ] ; then 
      echo "WINDOW calculation done" 
   else
      echo "found some problems in WINDOW calculation, stopping" ; cat CRASH ; exit 1
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
# eventually clean
#
if [ "$CLEAN" = ".TRUE." ] ; then  
   cd $TEST_HOME
      rm -rf *.out 2> /dev/null
      test -e SCRATCH && rm SCRATCH
   cd $TMPDIR
      test -e $TEST_NAME/HOME && rm $TEST_NAME/HOME
      test -d $TEST_NAME && rm -rf $TEST_NAME
   exit 0
fi

#
# exiting
echo "run.sh : everything done"
exit 0









