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
 
 scf_cond             PWSCF self-consistent calculation (conductor)
 nscf_cond            PWSCF non-self-consistent calculation (conductor)
 pw2wan_cond          export PWSCF data (conductor) to WanT package
 pwscf_cond           perform SCF, NSCF, PW2WAN all together (conductor)
 window_cond          wannier energy window (conductor)
 disentangle_cond     wannier subspace definition (conductor)
 wannier_cond         wannier functions (conductor)
 hamiltonian_cond     writes the hamiltonian on the Wannier basis (conductor)
 want_cond            all the wannier function steps, WINDOW, DISENT., WANNIER (conductor)
 all_cond             PWSCF_COND and WANT_COND tigether

 scf_leads            PWSCF self-consistent calculation (leads)
 nscf_leads           PWSCF non-self-consistent calculation (leads)
 pw2wan_leads         export PWSCF data (leads) to WanT package
 pwscf_leads          perform SCF, NSCF, PW2WAN all together (leads)
 window_leads         wannier energy window (leads)
 disentangle_leads    wannier subspace definition (leads)
 wannier_leads        wannier functions (leads)
 hamiltonian_leads    writes the hamiltonian on the Wannier basis (leads)
 want_leads           all the wannier function steps, WINDOW, DISENT., WANNIER (leads)
 all_leads            PWSCF_LEADS and WANT_LEADS tigether

 pwscf                PWSCF_COND and PWSCF_LEADS toether
 want                 WANT_COND and WANT_LEADS together
 conductor            evaluate the transmittance for the general conductor geometry
 bulk                 evaluate the transmittance for the conductor region treated as a bulk
 all                  perform all the above described steps

 clean                delete all output files and the temporary directory
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
UTILITY_BIN=$TEST_HOME/../../utility
TEST_NAME=Test2

#
# evaluate the starting choice about what is to run 

SCF_COND=
NSCF_COND=
PW2WAN_COND=
WINDOW_COND=
DISENTANGLE_COND=
WANNIER_COND=
HAMILTONIAN_COND=

SCF_LEADS=
NSCF_LEADS=
PW2WAN_LEADS=
WINDOW_LEADS=
DISENTANGLE_LEADS=
WANNIER_LEADS=
HAMILTONIAN_LEADS=

CONDUCTOR=
BULK=
CLEAN=




if [ $# = 0 ] ; then echo "$MANUAL" ; exit 0 ; fi
INPUT=`echo $1 | tr [:upper:] [:lower:]`

case $INPUT in 
   (scf_cond)            SCF_COND=".TRUE." ;;
   (nscf_cond)           NSCF_COND=".TRUE." ;;
   (pw2wan_cond)         PW2WAN_COND=".TRUE." ;;
   (pwscf_cond)          SCF_COND=".TRUE." ; NSCF_COND=".TRUE." ; PW2WAN_COND=".TRUE." ;;
   (window_cond)         WINDOW_COND=".TRUE." ;;
   (disentangle_cond)    DISENTANGLE_COND=".TRUE." ;;
   (wannier_cond)        WANNIER_COND=".TRUE." ;;
   (hamiltonian_cond)    HAMILTONIAN_COND=".TRUE." ;;
   (want_cond)           WINDOW_COND=".TRUE." ; DISENTANGLE_COND=".TRUE." ; 
                         WANNIER_COND=".TRUE." ; HAMILTONIAN_COND=".TRUE." ;;
   (all_cond)            SCF_COND=".TRUE." ; NSCF_COND=".TRUE." ; PW2WAN_COND=".TRUE." ;
                         WINDOW_COND=".TRUE." ; DISENTANGLE_COND=".TRUE." ;
                         WANNIER_COND=".TRUE." ; HAMILTONIAN_COND=".TRUE." ;;

   (scf_leads)           SCF_LEADS=".TRUE." ;;
   (nscf_leads)          NSCF_LEADS=".TRUE." ;;
   (pw2wan_leads)        PW2WAN_LEADS=".TRUE." ;;
   (pwscf_leads)         SCF_LEADS=".TRUE." ; NSCF_LEADS=".TRUE." ; PW2WAN_LEADS=".TRUE." ;;
   (window_leads)        WINDOW_LEADS=".TRUE." ;;
   (disentangle_leads)   DISENTANGLE_LEADS=".TRUE." ;;
   (wannier_leads)       WANNIER_LEADS=".TRUE." ;;
   (hamiltonian_leads)   HAMILTONIAN_LEADS=".TRUE." ;;
   (want_leads)          WINDOW_LEADS=".TRUE." ; DISENTANGLE_LEADS=".TRUE." ; 
                         WANNIER_LEADS=".TRUE." ; HAMILTONIAN_LEADS=".TRUE." ;;
   (all_leads)           SCF_LEADS=".TRUE." ; NSCF_LEADS=".TRUE." ; PW2WAN_LEADS=".TRUE." ;
                         WINDOW_LEADS=".TRUE." ; DISENTANGLE_LEADS=".TRUE." ;
                         WANNIER_LEADS=".TRUE." ; HAMILTONIAN_LEADS=".TRUE." ;;

   (pwscf)               SCF_COND=".TRUE." ; NSCF_COND=".TRUE." ; PW2WAN_COND=".TRUE." ;
                         SCF_LEADS=".TRUE." ; NSCF_LEADS=".TRUE." ; PW2WAN_LEADS=".TRUE." ;;
   (want)                WINDOW_COND=".TRUE." ; DISENTANGLE_COND=".TRUE." ;
                         WANNIER_COND=".TRUE." ; HAMILTONIAN_COND=".TRUE." ;
                         WINDOW_LEADS=".TRUE." ; DISENTANGLE_LEADS=".TRUE." ;
                         WANNIER_LEADS=".TRUE." ; HAMILTONIAN_LEADS=".TRUE." ;;
   (conductor)           CONDUCTOR=".TRUE." ;;
   (bulk)                BULK=".TRUE." ;;
   (all)                 SCF_COND=".TRUE." ; NSCF_COND=".TRUE." ; PW2WAN_COND=".TRUE." ; 
                         WINDOW_COND=".TRUE." ; DISENTANGLE_COND=".TRUE." ; 
                         WANNIER_COND=".TRUE." ;  HAMILTONIAN_COND=".TRUE." ;
                         SCF_LEADS=".TRUE." ; NSCF_LEADS=".TRUE." ; PW2WAN_LEADS=".TRUE." ; 
                         WINDOW_LEADS=".TRUE." ; DISENTANGLE_LEADS=".TRUE." ; 
                         WANNIER_LEADS=".TRUE." ;  HAMILTONIAN_LEADS=".TRUE." ;
                         CONDUCTOR=".TRUE." ;;

   (clean)               CLEAN=".TRUE." ;;
   (*)                   echo " Invalid input FLAG, type ./run.sh for help" ; exit 1 ;;  
esac


#
# preliminaries
#
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

#
# 2 sub scratch directories
test -e $TMPDIR/$TEST_NAME/COND || mkdir $TMPDIR/$TEST_NAME/COND
test -e $TMPDIR/$TEST_NAME/LEADS || mkdir $TMPDIR/$TEST_NAME/LEADS


#-----------------------------------------------------------------------------
cd $TMPDIR/$TEST_NAME

#
# running PWSCF SCF
#
if [ "$SCF_COND" = ".TRUE." ] ; then  
   cd COND
   $PARA_PREFIX  $PWSCF_BIN/pw.x $PARA_POSTFIX < $TEST_HOME/scf_cond.in > $TEST_HOME/scf_cond.out
   if [ $? = 0 ] ; then 
      echo "SCF_COND calculation done" 
   else
      echo "found some problems in SCF_COND calculation, stopping" ; exit 1
   fi
   cd ..
fi
#
if [ "$SCF_LEADS" = ".TRUE." ] ; then  
   cd LEADS
   $PARA_PREFIX  $PWSCF_BIN/pw.x $PARA_POSTFIX < $TEST_HOME/scf_leads.in > $TEST_HOME/scf_leads.out
   if [ $? = 0 ] ; then 
      echo "SCF_LEADS calculation done" 
   else
      echo "found some problems in SCF_LEADS calculation, stopping" ; exit 1
   fi
   cd ..
fi


#
# running PWSCF NSCF
#
if [ "$NSCF_COND" = ".TRUE." ] ; then  
   cd COND
   $PARA_PREFIX  $PWSCF_BIN/pw.x $PARA_POSTFIX  < $TEST_HOME/nscf_cond.in > $TEST_HOME/nscf_cond.out
   if [ $? = 0 ] ; then 
      echo "NSCF_COND calculation done" 
   else
      echo "found some problems in NSCF_COND calculation, stopping" ; exit 1
   fi
   cd ..
fi
#
if [ "$NSCF_LEADS" = ".TRUE." ] ; then  
   cd LEADS
   $PARA_PREFIX  $PWSCF_BIN/pw.x $PARA_POSTFIX < $TEST_HOME/nscf_leads.in > $TEST_HOME/nscf_leads.out
   if [ $? = 0 ] ; then 
      echo "NSCF_LEADS calculation done" 
   else
      echo "found some problems in NSCF_LEADS calculation, stopping" ; exit 1
   fi
   cd ..
fi
   
#
# running PWSCF PW2WAN
#
if [ "$PW2WAN_COND" = ".TRUE." ] ; then  
   cd COND
   $PARA_PREFIX  $PWSCF_BIN/pw2wan.x $PARA_POSTFIX \
              <  $TEST_HOME/pw2wan_cond.in > $TEST_HOME/pw2wan_cond.out
   if [ $? = 0 ] ; then 
      echo "PW2WAN_COND calculation done" 
   else
      echo "found some problems in PW2WAN_COND calculation, stopping" ; exit 1
   fi
   cd ..
fi
#
if [ "$PW2WAN_LEADS" = ".TRUE." ] ; then  
   cd LEADS
   $PARA_PREFIX  $PWSCF_BIN/pw2wan.x $PARA_POSTFIX  \
              <  $TEST_HOME/pw2wan_leads.in > $TEST_HOME/pw2wan_leads.out
   if [ $? = 0 ] ; then 
      echo "PW2WAN_LEADS calculation done" 
   else
      echo "found some problems in PW2WAN_LEADS calculation, stopping" ; exit 1
   fi
   cd ..
fi

#
# running WINDOW
#
if [ "$WINDOW_COND" = ".TRUE." ] ; then  
   cd COND
   $WANT_BIN/window.x < $TEST_HOME/want_cond.in > $TEST_HOME/window_cond.out
   if [ $? = 0 ] ; then 
      echo "WINDOW_COND calculation done" 
   else
      echo "found some problems in WINDOW_COND calculation, stopping" ; exit 1
   fi
   cd ..
fi
if [ "$WINDOW_LEADS" = ".TRUE." ] ; then  
   cd LEADS
   $WANT_BIN/window.x < $TEST_HOME/want_leads.in > $TEST_HOME/window_leads.out
   if [ $? = 0 ] ; then 
      echo "WINDOW_LEADS calculation done" 
   else
      echo "found some problems in WINDOW_LEADS calculation, stopping" ; exit 1
   fi
   cd ..
fi

#
# running DISENTANGLE
#
if [ "$DISENTANGLE_COND" = ".TRUE." ] ; then  
   cd COND
   $WANT_BIN/disentangle.x < $TEST_HOME/want_cond.in > $TEST_HOME/disentangle_cond.out
   if [ $? = 0 ] ; then 
      echo "DISENTANGLE_COND calculation done" 
   else
      echo "found some problems in DISENTANGLE_COND calculation, stopping" ; exit 1
   fi
   cd ..
fi
if [ "$DISENTANGLE_LEADS" = ".TRUE." ] ; then  
   cd LEADS
   $WANT_BIN/disentangle.x < $TEST_HOME/want_leads.in > $TEST_HOME/disentangle_leads.out
   if [ $? = 0 ] ; then 
      echo "DISENTANGLE_LEADS calculation done" 
   else
      echo "found some problems in DISENTANGLE_LEADS calculation, stopping" ; exit 1
   fi
   cd ..
fi

#
# running WANNIER
#
if [ "$WANNIER_COND" = ".TRUE." ] ; then  
   cd COND
   $WANT_BIN/wannier.x < $TEST_HOME/want_cond.in > $TEST_HOME/wannier_cond.out
   if [ $? = 0 ] ; then 
      echo "WANNIER_COND calculation done" 
   else
      echo "found some problems in WANNIER_COND calculation, stopping" ; exit 1
   fi
   cd ..
fi
if [ "$WANNIER_LEADS" = ".TRUE." ] ; then  
   cd LEADS
   $WANT_BIN/wannier.x < $TEST_HOME/want_leads.in > $TEST_HOME/wannier_leads.out
   if [ $? = 0 ] ; then 
      echo "WANNIER_LEADS calculation done" 
   else
      echo "found some problems in WANNIER_LEADS calculation, stopping" ; exit 1
   fi
   cd ..
fi

#
# running HAMILTONIAN
#
if [ "$HAMILTONIAN_COND" = ".TRUE." ] ; then  
   cd COND
   $WANT_BIN/hamiltonian.x < $TEST_HOME/hamiltonian_cond.in  \
                           > $TEST_HOME/hamiltonian_cond.out
   if [ $? = 0 ] ; then 
      echo "HAMILTONIAN_COND calculation done" 
   else
      echo "found some problems in HAMILTONIAN_COND calculation, stopping" ; exit 1
   fi
   cd ..
   cp COND/fort.103 H00_C
# XXXX
   $UTILITY_BIN/matrix_extract.sh COND/fort.104  1  24   1  13  > HCI_CB
   $UTILITY_BIN/matrix_extract.sh COND/fort.104  10 24   1  24  > HCI_AC
fi
if [ "$HAMILTONIAN_LEADS" = ".TRUE." ] ; then  
   cd LEADS
   $WANT_BIN/hamiltonian.x < $TEST_HOME/hamiltonian_leads.in  \
                           > $TEST_HOME/hamiltonian_leads.out
   if [ $? = 0 ] ; then 
      echo "HAMILTONIAN_LEADS calculation done" 
   else
      echo "found some problems in HAMILTONIAN_LEADS calculation, stopping" ; exit 1
   fi
   cd ..
   cp LEADS/fort.103 H00_A
   cp LEADS/fort.103 H00_B
   cp LEADS/fort.104 H01_A
   cp LEADS/fort.104 H01_B
fi


#
# running CONDUCTOR
#
if [ "$CONDUCTOR" = ".TRUE." ] ; then  
   $TRANS_BIN/conductor.x < $TEST_HOME/conductor.in > $TEST_HOME/conductor.out
   if [ $? = 0 ] ; then 
      echo "CODNDUCTOR calculation done" 
      #
      # also this needs to be improoved
      #
      cp dos.out cond.out $TEST_HOME
   else
      echo "found some problems in CONDUCTOR calculation, stopping" ; exit 1
   fi
fi


#
# running BULK  (eventually)
#
if [ "$BULK" = ".TRUE." ] ; then  
   ln -sf COND/fort.103 H00.dat
   ln -sf COND/fort.104 H01.dat
   #ln -sf LEADS/fort.102 H00.dat
   #ln -sf LEADS/fort.103 H01.dat
   #ln -sf LEADS/fort.103 H00.dat
   #ln -sf LEADS/fort.104 H01.dat
   #ln -sf LEADS/fort.105 H00.dat
   #ln -sf LEADS/fort.106 H01.dat
   $TRANS_BIN/bulk.x < $TEST_HOME/bulk.in > $TEST_HOME/bulk.out
   if [ $? = 0 ] ; then 
      echo "BULK calculation done" 
      #
      # also this needs to be improoved
      #
      mv dos.out  $TEST_HOME/dos_bulk.out
      mv cond.out $TEST_HOME/cond_bulk.out
   else
      echo "found some problems in BULK calculation, stopping" ; exit 1
   fi
fi



#
# eventually clean
#
if [ "$CLEAN" = ".TRUE." ] ; then  
   cd $TEST_HOME
      rm -rf *.out
      test -e SCRATCH && rm SCRATCH
   cd $TMPDIR
      test -e $TEST_NAME/HOME && rm $TEST_NAME/HOME
      test -d $TEST_NAME && rm -rf $TEST_NAME
      test -d $TEST_NAME/COND && rm -rf $TEST_NAME/COND
      test -d $TEST_NAME/LEADS && rm -rf $TEST_NAME/LEADS
      echo "CLEAN done" 
fi

#
# exiting
echo "run.sh : everything done"
exit 0




