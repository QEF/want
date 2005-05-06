#!/bin/bash 
#
# Test1
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
 pwexport        export DFT data to WanT package using IOTK fmt
 band            compute Silicon bands 
 dft             perform SCF, NSCF, PWEXPORT all together
 disentangle     select the optimal subspace on which perform
                 the wannier minimization
 wannier         perform the above cited minimization
 hamiltonian     writes the hamiltonian matrix elements on the Wannier basis
 want            perform DISENTANGLE, WANNIER and HAMILTONIAN all together 
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
PSEUDO_NAME=Si.vbc.UPF

#
# evaluate the starting choice about what is to run 

SCF=
NSCF=
PWEXPORT=
BAND=
DISENTANGLE=
WANNIER=
HAMILTONIAN=
CLEAN=

if [ $# = 0 ] ; then echo "$MANUAL" ; exit 0 ; fi
INPUT=`echo $1 | tr [:upper:] [:lower:]`

case $INPUT in 
   (scf)            SCF=".TRUE." ;;
   (nscf)           NSCF=".TRUE." ;;
   (pwexport)       PWEXPORT=".TRUE." ;;
   (band)           BAND=".TRUE." ;;
   (dft)            SCF=".TRUE." ; NSCF=".TRUE." ; PWEXPORT=".TRUE." ;; 
   (disentangle)    DISENTANGLE=".TRUE." ;;
   (wannier)        WANNIER=".TRUE." ;;
   (hamiltonian)    HAMILTONIAN=".TRUE." ;;
   (want)           DISENTANGLE=".TRUE." ; WANNIER=".TRUE." ;
                    HAMILTONIAN=".TRUE." ;;
   (all)            SCF=".TRUE." ; NSCF=".TRUE." ; PWEXPORT=".TRUE." ; 
                    DISENTANGLE=".TRUE." ; WANNIER=".TRUE." ; 
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
   $PARA_PREFIX  $DFT_BIN/pw.x $PARA_POSTFIX < $TEST_HOME/scf.in > $TEST_HOME/scf.out
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
   $PARA_PREFIX  $DFT_BIN/pw.x $PARA_POSTFIX < $TEST_HOME/nscf.in > $TEST_HOME/nscf.out
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
              <  $TEST_HOME/pwexport.in > $TEST_HOME/pwexport.out
   if [ $? = 0 ] ; then 
      echo "done" 
   else
      echo "found some problems in PWEXPORT calculation, stopping" ; exit 1
   fi
fi

#
# running DFT BAND
#
if [ "$BAND" = ".TRUE." ] ; then  
   echo "running BAND calculation" 
   $PARA_PREFIX  $DFT_BIN/pw.x $PARA_POSTFIX \
               < $TEST_HOME/nscf_band.in > $TEST_HOME/nscf_band.out
   if [ $? = 0 ] ; then 
      echo "done" 
   else
      echo "found some problems in BAND calculation, stopping" ; exit 1
   fi
fi
   
#
# running DISENTANGLE
#
if [ "$DISENTANGLE" = ".TRUE." ] ; then  
   echo "running DISENTANGLE calculation" 
   $WANT_BIN/disentangle.x < $TEST_HOME/want.in > $TEST_HOME/disentangle.out
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
   $WANT_BIN/wannier.x < $TEST_HOME/want.in > $TEST_HOME/wannier.out
   if [ ! -e CRASH ] ; then 
      echo "done" 
   else
      echo "found some problems in WANNIER calculation, stopping" ; cat CRASH ; exit 1
   fi
fi

#
# running HAMILTONIAN
#
if [ "$HAMILTONIAN" = ".TRUE." ] ; then  
   echo "running HAMILTONIAN calculation" 
   $WANT_BIN/hamiltonian.x < $TEST_HOME/hamiltonian.in > $TEST_HOME/hamiltonian.out
   if [ ! -e CRASH ] ; then 
      echo "done" 
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
      test -d $TEST_NAME && rm -rf $TEST_NAME
   exit 0
fi

#
# exiting
echo "run.sh : everything done"
exit 0









