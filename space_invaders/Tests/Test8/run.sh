#!/bin/bash 
#
# Test8
# 
#================================================================
# Input flags for this script (./run.sh FLAG): 
#
MANUAL=" Usage
   run.sh [FLAG]

 where FLAG is one of the following 
 (no FLAG will print this manual page) :
 
 scf             DFT self-consistent calculation
 nscf            DFT nscf calc (used to obtain more conduction bands)
 pwexport        export DFT data to WanT package in IOTK fmt
 dft             perform SCF, NSCF and PWEXPORT all together
 disentangle     select the optimal subspace on which perform the wannier minimization 
 wannier         perform the above cited minimization 
 hamiltonian     writes the hamiltonian matrix elements on the Wannier basis 
 want            perform DISENTANGLE, WANNIER and HAMILTONIAN
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
TEST_NAME=Test8
PSEUDO_LIST="C.blyp-mt.UPF  H.blyp-vbc.UPF  C.pbe-van_bm.UPF  H.pbe-van_bm.UPF "

#
# evaluate the starting choice about what is to run 

SCF=
SCF_US=
NSCF=
NSCF_US=
PWEXPORT=
PWEXPORT_US=
DISENTANGLE=
DISENTANGLE_US=
WANNIER=
WANNIER_US=
HAMILTONIAN=
HAMILTONIAN_US=
CLEAN=

if [ $# = 0 ] ; then echo "$MANUAL" ; exit 0 ; fi
INPUT=`echo $1 | tr [:upper:] [:lower:]`

case $INPUT in 
   (scf)            SCF=".TRUE." ;;
   (scf_us)         SCF_US=".TRUE." ;;
   (nscf)           NSCF=".TRUE." ;;
   (nscf_us)        NSCF_US=".TRUE." ;;
   (pwexport)       PWEXPORT=".TRUE." ;;
   (pwexport_us)    PWEXPORT_US=".TRUE." ;;
   (dft)            SCF=".TRUE."; NSCF=".TRUE." ; PWEXPORT=".TRUE." ;;
   (dft_us)         SCF_US=".TRUE."; NSCF_US=".TRUE." ; PWEXPORT_US=".TRUE." ;;
   (disentangle)    DISENTANGLE=".TRUE." ;;
   (disentangle_us) DISENTANGLE_US=".TRUE." ;;
   (wannier)        WANNIER=".TRUE." ;;
   (wannier_us)     WANNIER_US=".TRUE." ;;
   (hamiltonian)    HAMILTONIAN=".TRUE." ;;
   (hamiltonian_us) HAMILTONIAN_US=".TRUE." ;;
   (want)           DISENTANGLE=".TRUE." ; WANNIER=".TRUE." ;
                    HAMILTONIAN=".TRUE." ;;
   (want_us)        DISENTANGLE_US=".TRUE." ; WANNIER_US=".TRUE." ;
                    HAMILTONIAN_US=".TRUE." ;;
   (all)            SCF=".TRUE." ; NSCF=".TRUE." ; PWEXPORT=".TRUE." ; 
                    DISENTANGLE=".TRUE." ; WANNIER=".TRUE." ; 
                    HAMILTONIAN=".TRUE." ;
                    SCF_US=".TRUE." ; NSCF_US=".TRUE." ; PWEXPORT_US=".TRUE." ; 
                    DISENTANGLE_US=".TRUE." ; WANNIER_US=".TRUE." ; 
                    HAMILTONIAN_US=".TRUE." ;;
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
       ln -sf $TEST_HOME/../Pseudo/$item
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
   echo "running SCF calculation" 
   $PARA_PREFIX  $DFT_BIN/pw.x $PARA_POSTFIX < $TEST_HOME/scf.in > $TEST_HOME/scf.out
   if [ $? = 0 ] ; then 
      echo "done" 
   else
      echo "found some problems in SCF calculation, stopping" ; exit 1
   fi
fi
   
#
# running DFT SCF_US
#
if [ "$SCF_US" = ".TRUE." ] ; then  
   echo "running SCF_US calculation" 
   $PARA_PREFIX  $DFT_BIN/pw.x $PARA_POSTFIX < $TEST_HOME/scf_US.in > $TEST_HOME/scf_US.out
   if [ $? = 0 ] ; then 
      echo "done" 
   else
      echo "found some problems in SCF_US calculation, stopping" ; exit 1
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
# running DFT NSCF_US
#
if [ "$NSCF_US" = ".TRUE." ] ; then  
   echo "running NSCF_US calculation" 
   $PARA_PREFIX  $DFT_BIN/pw.x $PARA_POSTFIX < $TEST_HOME/nscf_US.in > $TEST_HOME/nscf_US.out
   if [ $? = 0 ] ; then 
      echo "done" 
   else
      echo "found some problems in NSCF_US calculation, stopping" ; exit 1
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
# running DFT PWEXPORT_US
#
if [ "$PWEXPORT_US" = ".TRUE." ] ; then  
   echo "running PWEXPORT_US calculation" 
   $PARA_PREFIX  $DFT_BIN/pw_export.x $PARA_POSTFIX  \
              <  $TEST_HOME/pwexport_US.in > $TEST_HOME/pwexport_US.out
   if [ $? = 0 ] ; then 
      echo "done" 
   else
      echo "found some problems in PWEXPORT_US calculation, stopping" ; exit 1
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
# running DISENTANGLE_US
#
if [ "$DISENTANGLE_US" = ".TRUE." ] ; then  
   echo "running DISENTANGLE_US calculation" 
   $WANT_BIN/disentangle.x < $TEST_HOME/want_US.in > $TEST_HOME/disentangle_US.out
   if [ ! -e CRASH ] ; then 
      echo "done" 
   else
      echo "found some problems in DISENTANGLE_US calculation, stopping" ; cat CRASH ; exit 1
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
# running WANNIER_US
#
if [ "$WANNIER_US" = ".TRUE." ] ; then  
   echo "running WANNIER_US calculation" 
   $WANT_BIN/wannier.x < $TEST_HOME/want_US.in > $TEST_HOME/wannier_US.out
   if [ ! -e CRASH ] ; then 
      echo "done" 
   else
      echo "found some problems in WANNIER_US calculation, stopping" ; cat CRASH ; exit 1
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
# running HAMILTONIAN_US
#
if [ "$HAMILTONIAN_US" = ".TRUE." ] ; then  
   echo "running HAMILTONIAN_US calculation" 
   $WANT_BIN/hamiltonian.x < $TEST_HOME/hamiltonian_US.in > $TEST_HOME/hamiltonian_US.out
   if [ ! -e CRASH ] ; then 
      echo "done" 
   else
      echo "found some problems in HAMILTONIAN_US calculation, stopping" ; cat CRASH ; exit 1
   fi
   rename RHAM. RHAM_US. RHAM*
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









