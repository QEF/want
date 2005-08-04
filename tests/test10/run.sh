#!/bin/bash
#
# Test3
# 
#================================================================
#
# Input flags for this script (./run.sh FLAG): 
#
MANUAL=" Usage
   run.sh [FLAG]

 where FLAG is one of the following 
 (no FLAG will print this manual page) :
 
 scf_cond             DFT self-consistent calculation (conductor)
 nscf_cond            DFT non-self-consistent calculation (conductor)
 pwexport_cond        export DFT data (conductor) to WanT package
 dft_cond             perform SCF, NSCF, PWEXPORT all together (conductor)
 disentangle_cond     wannier subspace definition (conductor)
 wannier_cond         wannier functions (conductor)
 bands_cond           interpolates the band structure using WFs
 plot_cond            compute WFs on real space for plotting (conductor)
 want_cond            all the wannier function steps, DISENT., WANNIER (conductor)
 all_cond             DFT_COND and WANT_COND tigether

 scf_leads            DFT self-consistent calculation (leads)
 nscf_leads           DFT non-self-consistent calculation (leads)
 pwexport_leads       export DFT data (leads) to WanT package
 dft_leads            perform SCF, NSCF, PWEXPORT all together (leads)
 disentangle_leads    wannier subspace definition (leads)
 wannier_leads        wannier functions (leads)
 plot_leads           compute WFs on real space for plotting (leads)
 bands_leads          interpolates the band structure using WFs
 want_leads           all the wannier function steps, DISENT., WANNIER (leads)
 all_leads            DFT_LEADS and WANT_LEADS tigether

 dft                  DFT_COND and DFT_LEADS toether
 want                 WANT_COND, WANT_LEADS, CONDUCTOR and BULK, together
 conductor            evaluate the transmittance for the general conductor geometry
 bulk                 evaluate the transmittance for the conductor region treated as a bulk
 all                  perform all the above described steps

 check                check results with the reference outputs
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
TEST_NAME=Test3
PSEUDO_NAME=C.pbe-van_bm.UPF

#
# evaluate the starting choice about what is to run 

SCF_COND=
NSCF_COND=
PWEXPORT_COND=
DISENTANGLE_COND=
WANNIER_COND=
BANDS_COND=
PLOT_COND=

SCF_LEADS=
NSCF_LEADS=
PWEXPORT_LEADS=
DISENTANGLE_LEADS=
WANNIER_LEADS=
BANDS_LEADS=
PLOT_LEADS=

CONDUCTOR=
BULK=
CHECK=
CLEAN=




if [ $# = 0 ] ; then echo "$MANUAL" ; exit 0 ; fi
INPUT=`echo $1 | tr [:upper:] [:lower:]`

case $INPUT in
   ( scf_cond )          SCF_COND=".TRUE." ;;
   ( nscf_cond )         NSCF_COND=".TRUE." ;;
   ( pwexport_cond )     PWEXPORT_COND=".TRUE." ;;
   ( dft_cond )          SCF_COND=".TRUE." ; NSCF_COND=".TRUE." ; PWEXPORT_COND=".TRUE." ;;
   ( disentangle_cond )  DISENTANGLE_COND=".TRUE." ;;
   ( wannier_cond )      WANNIER_COND=".TRUE." ;;
   ( bands_cond )        BANDS_COND=".TRUE." ;;
   ( plot_cond )         PLOT_COND=".TRUE." ;;
   ( want_cond )         DISENTANGLE_COND=".TRUE." ; 
                         WANNIER_COND=".TRUE." ; BANDS_COND=".TRUE." ; PLOT_COND=".TRUE.";;
   ( all_cond )          SCF_COND=".TRUE." ; NSCF_COND=".TRUE." ; PWEXPORT_COND=".TRUE." ;
                         DISENTANGLE_COND=".TRUE." ;
                         WANNIER_COND=".TRUE." ; BANDS_COND=".TRUE." ; PLOT_COND=".TRUE.";;

   ( scf_leads )         SCF_LEADS=".TRUE." ;;
   ( nscf_leads )        NSCF_LEADS=".TRUE." ;;
   ( pwexport_leads )    PWEXPORT_LEADS=".TRUE." ;;
   ( dft_leads )         SCF_LEADS=".TRUE." ; NSCF_LEADS=".TRUE." ; 
                         PWEXPORT_LEADS=".TRUE." ;;
   ( disentangle_leads ) DISENTANGLE_LEADS=".TRUE." ;;
   ( wannier_leads )     WANNIER_LEADS=".TRUE." ;;
   ( bands_leads )       BANDS_LEADS=".TRUE." ;;
   ( plot_leads )        PLOT_LEADS=".TRUE." ;;
   ( want_leads )        DISENTANGLE_LEADS=".TRUE." ; 
                         WANNIER_LEADS=".TRUE." ; BANDS_LEADS=".TRUE." ; 
                         PLOT_LEADS=".TRUE.";;
   ( all_leads )         SCF_LEADS=".TRUE." ; NSCF_LEADS=".TRUE." ; PWEXPORT_LEADS=".TRUE." ;
                         DISENTANGLE_LEADS=".TRUE." ; WANNIER_LEADS=".TRUE." ; 
                         BANDS_LEADS=".TRUE." ;  PLOT_COND=".TRUE." ;;

   ( dft )               SCF_COND=".TRUE." ; NSCF_COND=".TRUE." ; PWEXPORT_COND=".TRUE." ;
                         SCF_LEADS=".TRUE." ; NSCF_LEADS=".TRUE." ; PWEXPORT_LEADS=".TRUE.";;
   ( conductor )         CONDUCTOR=".TRUE." ;;
   ( bulk )              BULK=".TRUE." ;;
   ( want )              DISENTANGLE_COND=".TRUE." ;
                         WANNIER_COND=".TRUE." ; BANDS_COND=".TRUE." ; PLOT_COND=".TRUE." ;
                         DISENTANGLE_LEADS=".TRUE." ; WANNIER_LEADS=".TRUE." ; 
                         BANDS_LEADS=".TRUE." ;  PLOT_LEADS=".TRUE."; 
                         BULK=".TRUE."; CONDUCTOR=".TRUE." ;;
   ( all )               SCF_COND=".TRUE." ; NSCF_COND=".TRUE." ; PWEXPORT_COND=".TRUE." ; 
                         DISENTANGLE_COND=".TRUE." ; WANNIER_COND=".TRUE." ;  
                         BANDS_COND=".TRUE." ;  PLOT_COND=".TRUE." ;
                         SCF_LEADS=".TRUE." ; NSCF_LEADS=".TRUE." ; PWEXPORT_LEADS=".TRUE."; 
                         DISENTANGLE_LEADS=".TRUE." ; WANNIER_LEADS=".TRUE." ; 
                         BANDS_LEADS=".TRUE." ;  PLOT_LEADS=".TRUE." ;
                         CONDUCTOR=".TRUE." ; BULK=".TRUE." ;;

   ( check )             CHECK=".TRUE." ;;
   ( clean )             CLEAN=".TRUE." ;;
   (*)                   echo " Invalid input FLAG, type ./run.sh for help" ; exit 1 ;;  
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
   test -e $TMPDIR/$TEST_NAME/COND/CRASH && rm $TMPDIR/$TEST_NAME/COND/CRASH
   test -e $TMPDIR/$TEST_NAME/LEADS/CRASH && rm $TMPDIR/$TEST_NAME/LEADS/CRASH
   #
   # 2 sub scratch directories
   test -e $TMPDIR/$TEST_NAME/COND || mkdir $TMPDIR/$TEST_NAME/COND
   test -e $TMPDIR/$TEST_NAME/LEADS || mkdir $TMPDIR/$TEST_NAME/LEADS

   cd $TMPDIR/$TEST_NAME
fi

#-----------------------------------------------------------------------------

#
# running DFT SCF
#
if [ "$SCF_COND" = ".TRUE." ] ; then  
   echo "running SCF_COND calculation" 
   $PARA_PREFIX  $DFT_BIN/pw.x $PARA_POSTFIX < $TEST_HOME/scf_cond.in > $TEST_HOME/scf_cond.out
   if [ $? = 0 ] ; then 
      echo "done" 
   else
      echo "found some problems in SCF_COND calculation, stopping" ; exit 1
   fi
fi
#
if [ "$SCF_LEADS" = ".TRUE." ] ; then  
   echo "running SCF_LEADS calculation" 
   $PARA_PREFIX  $DFT_BIN/pw.x $PARA_POSTFIX < $TEST_HOME/scf_leads.in > $TEST_HOME/scf_leads.out
   if [ $? = 0 ] ; then 
      echo "done" 
   else
      echo "found some problems in SCF_LEADS calculation, stopping" ; exit 1
   fi
fi


#
# running DFT NSCF
#
if [ "$NSCF_COND" = ".TRUE." ] ; then  
   echo "running NSCF_COND calculation" 
   $PARA_PREFIX  $DFT_BIN/pw.x $PARA_POSTFIX  < $TEST_HOME/nscf_cond.in > $TEST_HOME/nscf_cond.out
   if [ $? = 0 ] ; then 
      echo "done" 
   else
      echo "found some problems in NSCF_COND calculation, stopping" ; exit 1
   fi
fi
#
if [ "$NSCF_LEADS" = ".TRUE." ] ; then  
   echo "running NSCF_LEADS calculation" 
   $PARA_PREFIX  $DFT_BIN/pw.x $PARA_POSTFIX < $TEST_HOME/nscf_leads.in > $TEST_HOME/nscf_leads.out
   if [ $? = 0 ] ; then 
      echo "done" 
   else
      echo "found some problems in NSCF_LEADS calculation, stopping" ; exit 1
   fi
fi
   
#
# running DFT PWEXPORT
#
if [ "$PWEXPORT_COND" = ".TRUE." ] ; then  
   echo "running PWEXPORT_COND calculation" 
   $PARA_PREFIX  $DFT_BIN/pw_export.x $PARA_POSTFIX \
              <  $TEST_HOME/pwexport_cond.in > $TEST_HOME/pwexport_cond.out
   if [ $? = 0 ] ; then 
      echo "done" 
   else
      echo "found some problems in PWEXPORT_COND calculation, stopping" ; exit 1
   fi
fi
#
if [ "$PWEXPORT_LEADS" = ".TRUE." ] ; then  
   echo "running PWEXPORT_LEADS calculation" 
   $PARA_PREFIX  $DFT_BIN/pw_export.x $PARA_POSTFIX  \
              <  $TEST_HOME/pwexport_leads.in > $TEST_HOME/pwexport_leads.out
   if [ $? = 0 ] ; then 
      echo "done" 
   else
      echo "found some problems in PWEXPORT_LEADS calculation, stopping" ; exit 1
   fi
fi

#
# running DISENTANGLE
#
if [ "$DISENTANGLE_COND" = ".TRUE." ] ; then  
   echo "running DISENTANGLE_COND calculation" 
   $WANT_BIN/disentangle.x < $TEST_HOME/want_cond.in > $TEST_HOME/disentangle_cond.out
   if [ ! -e CRASH ] ; then 
      echo "done" 
   else
      echo "found some problems in DISENTANGLE_COND calculation, stopping" ; cat CRASH 
      exit 1
   fi
fi
if [ "$DISENTANGLE_LEADS" = ".TRUE." ] ; then  
   echo "running DISENTANGLE_LEADS calculation" 
   $WANT_BIN/disentangle.x < $TEST_HOME/want_leads.in > $TEST_HOME/disentangle_leads.out
   if [ ! -e CRASH ] ; then 
      echo "done" 
   else
      echo "found some problems in DISENTANGLE_LEADS calculation, stopping" ; cat CRASH  
      exit 1
   fi
fi

#
# running WANNIER
#
if [ "$WANNIER_COND" = ".TRUE." ] ; then  
   echo "running WANNIER_COND calculation" 
   $WANT_BIN/wannier.x < $TEST_HOME/want_cond.in > $TEST_HOME/wannier_cond.out
   if [ ! -e CRASH ] ; then 
      echo "done" 
   else
      echo "found some problems in WANNIER_COND calculation, stopping" ; cat CRASH ; exit 1
   fi
fi
if [ "$WANNIER_LEADS" = ".TRUE." ] ; then  
   echo "running WANNIER_LEADS calculation" 
   $WANT_BIN/wannier.x < $TEST_HOME/want_leads.in > $TEST_HOME/wannier_leads.out
   if [ ! -e CRASH ] ; then 
      echo "done" 
   else
      echo "found some problems in WANNIER_LEADS calculation, stopping" ; cat CRASH ; exit 1
   fi
fi

#
# running BANDS
#
if [ "$BANDS_COND" = ".TRUE." ] ; then  
   echo "running BANDS_COND calculation" 
   $WANT_BIN/bands.x < $TEST_HOME/bands_cond.in  \
                           > $TEST_HOME/bands_cond.out
   if [ ! -e CRASH ] ; then 
      echo "done" 
   else
      echo "found some problems in BANDS_COND calculation, stopping" ; cat CRASH 
      exit 1
   fi
fi

if [ "$BANDS_LEADS" = ".TRUE." ] ; then  
   echo "running BANDS_LEADS calculation" 
   $WANT_BIN/bands.x < $TEST_HOME/bands_leads.in  \
                           > $TEST_HOME/bands_leads.out
   if [ ! -e CRASH ] ; then 
      echo "done" 
   else
      echo "found some problems in BANDS_LEADS calculation, stopping" ; cat CRASH
      exit 1
   fi
fi

#
# running PLOT
#
if [ "$PLOT_COND" = ".TRUE." ] ; then  
   echo "running PLOT_COND calculation" 
   $WANT_BIN/plot.x < $TEST_HOME/plot_cond.in  \
                           > $TEST_HOME/plot_cond.out
   if [ ! -e CRASH ] ; then 
      echo "done" 
   else
      echo "found some problems in PLOT_COND calculation, stopping" ; cat CRASH 
      exit 1
   fi
fi

if [ "$PLOT_LEADS" = ".TRUE." ] ; then  
   echo "running PLOT_LEADS calculation" 
   $WANT_BIN/plot.x < $TEST_HOME/plot_leads.in  \
                           > $TEST_HOME/plot_leads.out
   if [ ! -e CRASH ] ; then 
      echo "done" 
   else
      echo "found some problems in PLOT_LEADS calculation, stopping" ; cat CRASH
      exit 1
   fi
fi


#
# running CONDUCTOR
#
if [ "$CONDUCTOR" = ".TRUE." ] ; then  
   #
   $UTILITY_BIN/matrix_extract.sh COND/RHAM.103    1  32   1 32  > H00_C
   
   $UTILITY_BIN/matrix_extract.sh COND/RHAM.104    1  32   1 16  > HCI_CB
   $UTILITY_BIN/matrix_extract.sh COND/RHAM.104    17 32   1 32  > HCI_AC
   
   $UTILITY_BIN/matrix_extract.sh LEADS/RHAM.105   1  16   1 16  > H00_A
   $UTILITY_BIN/matrix_extract.sh LEADS/RHAM.105   1  16   1 16  > H00_B
   $UTILITY_BIN/matrix_extract.sh LEADS/RHAM.106   1  16   1 16  > H01_A
   $UTILITY_BIN/matrix_extract.sh LEADS/RHAM.106   1  16   1 16  > H01_B

   echo "running CODNDUCTOR calculation" 
   $TRANS_BIN/conductor.x < $TEST_HOME/conductor.in > $TEST_HOME/conductor.out
   if [ ! -e CRASH ] ; then 
      echo "done" 
      #
      # also this needs to be improoved
      #
      cp dos.dat cond.dat $TEST_HOME
   else
      echo "found some problems in CONDUCTOR calculation, stopping" ; cat CRASH ; exit 1
   fi
fi


#
# running BULK  (eventually)
#
if [ "$BULK" = ".TRUE." ] ; then  
   ln -sf COND/RHAM.103 H00_C
   ln -sf COND/RHAM.104 HCI_CB
   echo "running BULK calculation" 
   $TRANS_BIN/conductor.x < $TEST_HOME/bulk.in > $TEST_HOME/bulk.out
   if [ ! -e CRASH ] ; then 
      echo "done" 
      #
      # also this needs to be improoved
      #
      mv dos.dat  $TEST_HOME/dos_bulk.dat
      mv cond.dat $TEST_HOME/cond_bulk.dat
   else
      echo "found some problems in BULK calculation, stopping" ; cat CRASH ; exit 1
   fi
fi


#
# running CHECK
#
if [ "$CHECK" = ".TRUE." ] ; then
   echo "running CHECK"
   #
   cd $TEST_HOME
   list="disentangle_leads.out wannier_leads.out bands_leads.out
         disentangle_cond.out  wannier_cond.out  bands_cond.out"
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




