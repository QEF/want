#!/bin/sh 
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
 pw2wan          export PWSCF data to WanT package
 pwscf           perform SCF, NSCF, PW2WAN all together
 window          determine which bads are in the energy window
                 of interest and then write data accoding to it
 disentangle     select the optimal subspace on which perform
                 the wannier minimization
 wannier         perform the above cited minimization
 hamiltonian     writes the hamiltonian matrix elements on the Wannier basis
 want            perform WINDOW, DISENTANGLE, WANNIER and HAMILTONIAN all together 
 bulk            evaluate the transmittance, for the bulk case
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

#
# evaluate the starting choice about what is to run 

SCF=
NSCF=
PW2WAN=
WINDOW=
DISENTANGLE=
WANNIER=
HAMILTONIAN=
BULK=
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
   (bulk)           BULK=".TRUE." ;;
   (all)            SCF=".TRUE." ; NSCF=".TRUE." ; PW2WAN=".TRUE." ; 
                    WINDOW=".TRUE." ; DISENTANGLE=".TRUE." ; WANNIER=".TRUE." ; 
                    BULK=".TRUE." ;;
   (clean)          CLEAN=".TRUE." ;;
esac

#
# preliminaries
#
cd $TMPDIR
test -e $TEST_NAME || mkdir $TEST_NAME 
cd $TEST_NAME
ln -sf $TEST_HOME/../Pseudo/* .
ln -sf $TMPDIR/$TEST_NAME $TEST_HOME/SCRATCH
ln -sf $TEST_HOME $TMPDIR/$TEST_NAME/HOME

##
## running PWSCF SCF
##
#if [ "$SCF" = ".TRUE." ] ; then  
#   $PARA_PREFIX  $PWSCF_BIN/pw.x $PARA_POSTFIX < $TEST_HOME/scf.in > $TEST_HOME/scf.out
#   if [ $? = 0 ] ; then 
#      echo "SCF calculation done" 
#   else
#      echo "found some problems in SCF calculation, stopping" ; exit 1
#   fi
#fi
#
##
## running PWSCF NSCF
##
#if [ "$NSCF" = ".TRUE." ] ; then  
#   $PARA_PREFIX  $PWSCF_BIN/pw.x $PARA_POSTFIX < $TEST_HOME/nscf.in > $TEST_HOME/nscf.out
#   if [ $? = 0 ] ; then 
#      echo "NSCF calculation done" 
#   else
#      echo "found some problems in NSCF calculation, stopping" ; exit 1
#   fi
#fi
#   
##
## running PWSCF PW2WAN
##
#if [ "$PW2WAN" = ".TRUE." ] ; then  
#   $PARA_PREFIX  $PWSCF_BIN/pw2wan.x $PARA_POSTFIX  \
#              <  $TEST_HOME/pw2wan.in > $TEST_HOME/pw2wan.out
#   if [ $? = 0 ] ; then 
#      echo "PW2WAN calculation done" 
#   else
#      echo "found some problems in PW2WAN calculation, stopping" ; exit 1
#   fi
#fi
#
##
## running WINDOW
##
#if [ "$WINDOW" = ".TRUE." ] ; then  
#   $WANT_BIN/window.x < $TEST_HOME/want.in > $TEST_HOME/window.out
#   if [ $? = 0 ] ; then 
#      echo "WINDOW calculation done" 
#   else
#      echo "found some problems in WINDOW calculation, stopping" ; exit 1
#   fi
#fi
#
##
## running DISENTANGLE
##
#if [ "$DISENTANGLE" = ".TRUE." ] ; then  
#   $WANT_BIN/disentangle.x < $TEST_HOME/want.in > $TEST_HOME/disentangle.out
#   if [ $? = 0 ] ; then 
#      echo "DISENTANGLE calculation done" 
#   else
#      echo "found some problems in DISENTANGLE calculation, stopping" ; exit 1
#   fi
#fi
#
##
## running WANNIER
##
#if [ "$WANNIER" = ".TRUE." ] ; then  
#   $WANT_BIN/wannier.x < $TEST_HOME/want.in > $TEST_HOME/wannier.out
#   if [ $? = 0 ] ; then 
#      echo "WANNIER calculation done" 
#   else
#      echo "found some problems in WANNIER calculation, stopping" ; exit 1
#   fi
#fi
#
##
## running HAMILTONIAN
##
#if [ "$HAMILTONIAN" = ".TRUE." ] ; then  
#   $WANT_BIN/hamiltonian.x < $TEST_HOME/hamiltonian.in > $TEST_HOME/hamiltonian.out
#   if [ $? = 0 ] ; then 
#      echo "HAMILTONIAN calculation done" 
#      #
#      # hopefully will be improoved very soon...
#      #
#      ln -sf fort.103 H00.dat
#      ln -sf fort.104 H01.dat
#   else
#      echo "found some problems in HAMILTONIAN calculation, stopping" ; exit 1
#   fi
#fi
#
#
##
## running BULK
##
#if [ "$BULK" = ".TRUE." ] ; then  
#   $TRANS_BIN/bulk.x < $TEST_HOME/bulk.in > $TEST_HOME/bulk.out
#   if [ $? = 0 ] ; then 
#      echo "BULK calculation done" 
#      #
#      # also this needs to be improoved
#      #
#      cp dos.out cond.out $TEST_HOME
#   else
#      echo "found some problems in BULK calculation, stopping" ; exit 1
#   fi
#fi



#
# eventually clean
#
if [ "$CLEAN" = ".TRUE." ] ; then  
   cd $TEST_HOME
      test -e *.out && rm *.out
      test -e SCRATCH && rm SCRATCH
   cd $TMPDIR
      test -e $TEST_NAME/HOME && rm $TEST_NAME/HOME
      test -d $TEST_NAME && rm -rf $TEST_NAME
      echo "CLEAN done" 
fi

#
# exiting
echo "run.sh : everything done"
exit 0









