#!/bin/bash 
#
# Test7
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
 disentangle_up  select the optimal subspace on which perform
                 the wannier minimization for SPINUP states
 disentangle_dw  the same for SPINDW states
 wannier_up      perform the above cited minimization (SPINUP)
 wannier_dw      the same for SPINDW states
 bands_up        interpolates the band structure using WFs
 bands_dw        the same for SPINDW states
 plot_up         compute WFs on real space for plotting (SPINUP)
 plot_dw         the same for SPINDW states
 want_up         perform DISENTANGLE, WANNIER and BANDS for SPINUP
 want_dw         the same for SPINDW
 want            want_up and want_dw
 bulk_up         evaluate the transmittance, for the bulk case (SPINUP)
 bulk_dw         the same for SPINDW
 bulk            bulk_up and bulk_dw
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
TEST_NAME=Test7
PSEUDO_NAME=Ni.pz-nd-rrkjus.UPF

#
# evaluate the starting choice about what is to run 

SCF=
NSCF=
PWEXPORT=
DISENTANGLE_UP=
DISENTANGLE_DW=
WANNIER_UP=
WANNIER_DW=
BANDS_UP=
BANDS_DW=
PLOT_UP=
PLOT_DW=
BULK_UP=
BULK_DW=
CLEAN=

if [ $# = 0 ] ; then echo "$MANUAL" ; exit 0 ; fi
INPUT=`echo $1 | tr [:upper:] [:lower:]`

case $INPUT in 
   (scf)            SCF=".TRUE." ;;
   (nscf)           NSCF=".TRUE." ;;
   (pwexport)       PWEXPORT=".TRUE." ;;
   (dft)            SCF=".TRUE." ; NSCF=".TRUE." ; PWEXPORT=".TRUE." ;;
   (disentangle_up) DISENTANGLE_UP=".TRUE." ;;
   (disentangle_dw) DISENTANGLE_DW=".TRUE." ;;
   (wannier_up)     WANNIER_UP=".TRUE." ;;
   (wannier_dw)     WANNIER_DW=".TRUE." ;;
   (bands_up)       BANDS_UP=".TRUE." ;;
   (bands_dw)       BANDS_DW=".TRUE." ;;
   (plot_up)        PLOT_UP=".TRUE." ;;
   (plot_dw)        PLOT_DW=".TRUE." ;;
   (want_up)        DISENTANGLE_UP=".TRUE." ; WANNIER_UP=".TRUE." ;
                    BANDS_UP=".TRUE." ; PLOT_UP=".TRUE." ;;
   (want_dw)        DISENTANGLE_DW=".TRUE." ; WANNIER_DW=".TRUE." ;
                    BANDS_DW=".TRUE." ; PLOT_DW=".TRUE." ;;
   (want)           DISENTANGLE_UP=".TRUE." ; WANNIER_UP=".TRUE." ;
                    BANDS_UP=".TRUE." ;
                    DISENTANGLE_DW=".TRUE." ; WANNIER_DW=".TRUE." ;
                    BANDS_DW=".TRUE." ; PLOT_UP=".TRUE." ; PLOT_DW=".TRUE." ;;
   (bulk_up)        BULK_UP=".TRUE." ;;
   (bulk_dw)        BULK_DW=".TRUE." ;;
   (bulk)           BULK_UP=".TRUE." ; BULK_DW=".TRUE." ;;
   (all)            SCF=".TRUE." ; NSCF=".TRUE." ; PWEXPORT=".TRUE." ; 
                    DISENTANGLE_UP=".TRUE." ; WANNIER_UP=".TRUE." ; 
                    BANDS_UP=".TRUE." ; BULK_UP=".TRUE." ;
                    DISENTANGLE_DW=".TRUE." ; WANNIER_DW=".TRUE." ; 
                    PLOT_UP=".TRUE." ; PLOT_DW=".TRUE." ;
                    BANDS_DW=".TRUE." ; BULK_DW=".TRUE." ;;
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
# running DISENTANGLE_UP
#
if [ "$DISENTANGLE_UP" = ".TRUE." ] ; then  
   echo "running DISENTANGLE_UP calculation" 
   $WANT_BIN/disentangle.x < $TEST_HOME/want_UP.in > $TEST_HOME/disentangle_UP.out
   if [ ! -e CRASH ] ; then 
      echo "done" 
   else
      echo "found some problems in DISENTANGLE_UP calculation, stopping" ; cat CRASH ; exit 1
   fi
fi

#
# running DISENTANGLE_DW
#
if [ "$DISENTANGLE_DW" = ".TRUE." ] ; then  
   echo "running DISENTANGLE_DW calculation" 
   $WANT_BIN/disentangle.x < $TEST_HOME/want_DW.in > $TEST_HOME/disentangle_DW.out
   if [ ! -e CRASH ] ; then 
      echo "done" 
   else
      echo "found some problems in DISENTANGLE_DW calculation, stopping" ; cat CRASH ; exit 1
   fi
fi

#
# running WANNIER_UP
#
if [ "$WANNIER_UP" = ".TRUE." ] ; then  
   echo "running WANNIER_UP calculation" 
   $WANT_BIN/wannier.x < $TEST_HOME/want_UP.in > $TEST_HOME/wannier_UP.out
   if [ ! -e CRASH ] ; then 
      echo "done" 
   else
      echo "found some problems in WANNIER_UP calculation, stopping" ; cat CRASH ; exit 1
   fi
fi

#
# running WANNIER_DW
#
if [ "$WANNIER_DW" = ".TRUE." ] ; then  
   echo "running WANNIER_DW calculation" 
   $WANT_BIN/wannier.x < $TEST_HOME/want_DW.in > $TEST_HOME/wannier_DW.out
   if [ ! -e CRASH ] ; then 
      echo "done" 
   else
      echo "found some problems in WANNIER_DW calculation, stopping" ; cat CRASH ; exit 1
   fi
fi

#
# running BANDS_UP
#
if [ "$BANDS_UP" = ".TRUE." ] ; then  
   echo "running BANDS_UP calculation" 
   $WANT_BIN/bands.x < $TEST_HOME/bands_UP.in > $TEST_HOME/bands_UP.out
   if [ ! -e CRASH ] ; then 
      echo "done" 
   else
      echo "found some problems in BANDS_UP calculation, stopping" ; cat CRASH ; exit 1
   fi
   rename RHAM. RHAM_UP. RHAM*
fi

#
# running BANDS_DW
#
if [ "$BANDS_DW" = ".TRUE." ] ; then  
   echo "running BANDS_DW calculation" 
   $WANT_BIN/bands.x < $TEST_HOME/bands_DW.in > $TEST_HOME/bands_DW.out
   if [ ! -e CRASH ] ; then 
      echo "done" 
   else
      echo "found some problems in BANDS_DW calculation, stopping" ; cat CRASH ; exit 1
   fi
   rename RHAM. RHAM_DW. RHAM*
fi

#
# running PLOT_UP
#
if [ "$PLOT_UP" = ".TRUE." ] ; then
   echo "running PLOT_UP calculation"
   $WANT_BIN/plot.x < $TEST_HOME/plot_UP.in > $TEST_HOME/plot_UP.out
   if [ ! -e CRASH ] ; then
      echo "done"
   else
      echo "found some problems in PLOT_UP calculation, stopping" ; cat CRASH ; exit 1
   fi
fi

#
# running PLOT_DW
#
if [ "$PLOT_DW" = ".TRUE." ] ; then
   echo "running PLOT_DW calculation"
   $WANT_BIN/plot.x < $TEST_HOME/plot_DW.in > $TEST_HOME/plot_DW.out
   if [ ! -e CRASH ] ; then
      echo "done"
   else
      echo "found some problems in PLOT_DW calculation, stopping" ; cat CRASH ; exit 1
   fi
fi

#
# running BULK_UP
#
if [ "$BULK_UP" = ".TRUE." ] ; then  
   #
   ln -sf RHAM_UP.103 H00.dat
   ln -sf RHAM_UP.104 H01.dat
   #
   echo "running BULK_UP calculation" 
   $TRANS_BIN/bulk.x < $TEST_HOME/bulk.in > $TEST_HOME/bulk_UP.out
   if [ ! -e CRASH ] ; then 
      echo "done" 
      #
      mv dos.dat $TEST_HOME/dos_UP.dat
      mv cond.dat $TEST_HOME/cond_UP.dat
   else
      echo "found some problems in BULK_UP calculation, stopping" ; cat CRASH ; exit 1
   fi
fi


#
# running BULK_DW
#
if [ "$BULK_DW" = ".TRUE." ] ; then  
   #
   ln -sf RHAM_DW.103 H00.dat
   ln -sf RHAM_DW.104 H01.dat
   #
   echo "running BULK_DW calculation" 
   $TRANS_BIN/bulk.x < $TEST_HOME/bulk.in > $TEST_HOME/bulk_DW.out
   if [ ! -e CRASH ] ; then 
      echo "done" 
      #
      mv dos.dat $TEST_HOME/dos_DW.dat
      mv cond.dat $TEST_HOME/cond_DW.dat
   else
      echo "found some problems in BULK_DW calculation, stopping" ; cat CRASH ; exit 1
   fi
fi

#
# running CONDUCTOR
#
if [ "$CONDUCTOR" = ".TRUE." ] ; then  
   #
   # hopefully will be improoved very soon...
   #
   ln -sf RHAM.103 H00_A
   ln -sf RHAM.103 H00_B
   ln -sf RHAM.103 H00_C
   ln -sf RHAM.104 H01_B
   ln -sf RHAM.104 H01_A
   ln -sf RHAM.104 HCI_AC
   ln -sf RHAM.104 HCI_CB
   #
   echo "running CONDUCTOR calculation" 
   $TRANS_BIN/conductor.x < $TEST_HOME/conductor.in > $TEST_HOME/conductor.out
   if [ ! -e CRASH ] ; then 
      echo "done" 
      #
      # also this needs to be improoved
      #
      mv dos.dat cond.dat $TEST_HOME
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









