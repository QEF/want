#! /bin/bash
#
# Conductance of C-C chains (general geometry)
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

 conductor_bulk       transmittance for the conductor treated as a bulk
 conductor_auto       transmittance for leads and conductor taken from the same calculation
 conductor            evaluate the transmittance for the general conductor geometry

 dft                  DFT_COND and DFT_LEADS toether
 want                 WANT_COND, WANT_LEADS, CONDUCTOR's together
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
. $UTILITY_BIN/basedef.sh
TEST_HOME=$(pwd)
TEST_NAME=$(echo $TEST_HOME | awk -v FS=\/ '{print $NF}' )
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
CONDUCTOR_BULK=
CONDUCTOR_AUTO=
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
   ( conductor_bulk )    CONDUCTOR_BULK=".TRUE." ;;
   ( conductor_auto )    CONDUCTOR_AUTO=".TRUE." ;;
   ( want )              DISENTANGLE_COND=".TRUE." ;
                         WANNIER_COND=".TRUE." ; BANDS_COND=".TRUE." ; PLOT_COND=".TRUE." ;
                         DISENTANGLE_LEADS=".TRUE." ; WANNIER_LEADS=".TRUE." ; 
                         BANDS_LEADS=".TRUE." ;  PLOT_LEADS=".TRUE."; 
                         CONDUCTOR=".TRUE." ; CONDUCTOR_BULK=".TRUE." ; 
                         CONDUCTOR_AUTO=".TRUE." ;;
   ( all )               SCF_COND=".TRUE." ; NSCF_COND=".TRUE." ; PWEXPORT_COND=".TRUE." ; 
                         DISENTANGLE_COND=".TRUE." ; WANNIER_COND=".TRUE." ;  
                         BANDS_COND=".TRUE." ;  PLOT_COND=".TRUE." ;
                         SCF_LEADS=".TRUE." ; NSCF_LEADS=".TRUE." ; PWEXPORT_LEADS=".TRUE."; 
                         DISENTANGLE_LEADS=".TRUE." ; WANNIER_LEADS=".TRUE." ; 
                         BANDS_LEADS=".TRUE." ;  PLOT_LEADS=".TRUE." ;
                         CONDUCTOR=".TRUE." ; CONDUCTOR_BULK=".TRUE." ; 
                         CONDUCTOR_AUTO=".TRUE." ;;

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
   ln -sf $TEST_HOME/../pseudo/$PSEUDO_NAME .
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
   echo $ECHO_N "running SCF_COND calculation... $ECHO_C" 
   $PARA_PREFIX  $DFT_BIN/pw.x $PARA_POSTFIX < $TEST_HOME/scf_cond.in > $TEST_HOME/scf_cond.out
   if [ $? = 0 ] ; then 
      echo "$ECHO_T done" 
   else
      echo "$ECHO_T problems found" ; exit 1
   fi
fi
#
if [ "$SCF_LEADS" = ".TRUE." ] ; then  
   echo $ECHO_N "running SCF_LEADS calculation... $ECHO_C" 
   $PARA_PREFIX  $DFT_BIN/pw.x $PARA_POSTFIX < $TEST_HOME/scf_leads.in > $TEST_HOME/scf_leads.out
   if [ $? = 0 ] ; then 
      echo "$ECHO_T done" 
   else
      echo "$ECHO_T problems found" ; exit 1
   fi
fi


#
# running DFT NSCF
#
if [ "$NSCF_COND" = ".TRUE." ] ; then  
   echo $ECHO_N "running NSCF_COND calculation... $ECHO_C" 
   $PARA_PREFIX  $DFT_BIN/pw.x $PARA_POSTFIX  < $TEST_HOME/nscf_cond.in > $TEST_HOME/nscf_cond.out
   if [ $? = 0 ] ; then 
      echo "$ECHO_T done" 
   else
      echo "$ECHO_T problems found" ; exit 1
   fi
fi
#
if [ "$NSCF_LEADS" = ".TRUE." ] ; then  
   echo $ECHO_N "running NSCF_LEADS calculation... $ECHO_C" 
   $PARA_PREFIX  $DFT_BIN/pw.x $PARA_POSTFIX < $TEST_HOME/nscf_leads.in > $TEST_HOME/nscf_leads.out
   if [ $? = 0 ] ; then 
      echo "$ECHO_T done" 
   else
      echo "$ECHO_T problems found" ; exit 1
   fi
fi
   
#
# running DFT PWEXPORT
#
if [ "$PWEXPORT_COND" = ".TRUE." ] ; then  
   echo "running PWEXPORT_COND calculation..." 
   $PARA_PREFIX  $DFT_BIN/pw_export.x $PARA_POSTFIX \
              <  $TEST_HOME/pwexport_cond.in > $TEST_HOME/pwexport_cond.out
   if [ $? = 0 ] ; then 
      echo "done" 
   else
      echo "problems found" ; exit 1
   fi
fi
#
if [ "$PWEXPORT_LEADS" = ".TRUE." ] ; then  
   echo "running PWEXPORT_LEADS calculation..." 
   $PARA_PREFIX  $DFT_BIN/pw_export.x $PARA_POSTFIX  \
              <  $TEST_HOME/pwexport_leads.in > $TEST_HOME/pwexport_leads.out
   if [ $? = 0 ] ; then 
      echo "done" 
   else
      echo "problems found" ; exit 1
   fi
fi

#
# running DISENTANGLE
#
if [ "$DISENTANGLE_COND" = ".TRUE." ] ; then  
   echo $ECHO_N "running DISENTANGLE_COND calculation... $ECHO_C" 
   $WANT_BIN/disentangle.x < $TEST_HOME/want_cond.in > $TEST_HOME/disentangle_cond.out
   if [ ! -e CRASH ] ; then 
      echo "$ECHO_T done" 
   else
      echo "$ECHO_T problems found" ; cat CRASH ; exit 1
   fi
fi
if [ "$DISENTANGLE_LEADS" = ".TRUE." ] ; then  
   echo $ECHO_N "running DISENTANGLE_LEADS calculation... $ECHO_C" 
   $WANT_BIN/disentangle.x < $TEST_HOME/want_leads.in > $TEST_HOME/disentangle_leads.out
   if [ ! -e CRASH ] ; then 
      echo "$ECHO_T done" 
   else
      echo "$ECHO_T problems found" ; cat CRASH ; exit 1
   fi
fi

#
# running WANNIER
#
if [ "$WANNIER_COND" = ".TRUE." ] ; then  
   echo $ECHO_N "running WANNIER_COND calculation... $ECHO_C" 
   $WANT_BIN/wannier.x < $TEST_HOME/want_cond.in > $TEST_HOME/wannier_cond.out
   if [ ! -e CRASH ] ; then 
      echo "$ECHO_T done" 
   else
      echo "$ECHO_T problems found" ; cat CRASH ; exit 1 
   fi
fi
if [ "$WANNIER_LEADS" = ".TRUE." ] ; then  
   echo $ECHO_N "running WANNIER_LEADS calculation... $ECHO_C" 
   $WANT_BIN/wannier.x < $TEST_HOME/want_leads.in > $TEST_HOME/wannier_leads.out
   if [ ! -e CRASH ] ; then 
      echo "$ECHO_T done" 
   else
      echo "$ECHO_T problems found" ; cat CRASH ; exit 1
   fi
fi

#
# running BANDS
#
if [ "$BANDS_COND" = ".TRUE." ] ; then  
   echo $ECHO_N "running BANDS_COND calculation... $ECHO_C" 
   $WANT_BIN/bands.x < $TEST_HOME/bands_cond.in  \
                           > $TEST_HOME/bands_cond.out
   if [ ! -e CRASH ] ; then 
      echo "$ECHO_T done" 
   else
      echo "$ECHO_T problems found" ; cat CRASH ; exit 1
   fi
fi

if [ "$BANDS_LEADS" = ".TRUE." ] ; then  
   echo $ECHO_N "running BANDS_LEADS calculation... $ECHO_C" 
   $WANT_BIN/bands.x < $TEST_HOME/bands_leads.in  \
                           > $TEST_HOME/bands_leads.out
   if [ ! -e CRASH ] ; then 
      echo "$ECHO_T done" 
   else
      echo "$ECHO_T problems found" ; cat CRASH ; exit 1
   fi
fi

#
# running PLOT
#
if [ "$PLOT_COND" = ".TRUE." ] ; then  
   echo $ECHO_N "running PLOT_COND calculation... $ECHO_C" 
   $WANT_BIN/plot.x < $TEST_HOME/plot_cond.in  \
                           > $TEST_HOME/plot_cond.out
   if [ ! -e CRASH ] ; then 
      echo "$ECHO_T done" 
   else
      echo "$ECHO_T problems found" ; cat CRASH ; exit 1
   fi
fi

if [ "$PLOT_LEADS" = ".TRUE." ] ; then  
   echo $ECHO_N "running PLOT_LEADS calculation... $ECHO_C" 
   $WANT_BIN/plot.x < $TEST_HOME/plot_leads.in  \
                           > $TEST_HOME/plot_leads.out
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
   echo $ECHO_N "running CODNDUCTOR calculation... $ECHO_C" 
   $WANT_BIN/conductor.x < $TEST_HOME/conductor.in > $TEST_HOME/conductor.out
   if [ ! -e CRASH ] ; then 
      echo "$ECHO_T done" 
      test -e dos.dat && mv dos.dat $TEST_HOME
      test -e cond.dat && mv cond.dat $TEST_HOME
   else
      echo "$ECHO_T problems found" ; cat CRASH ; exit 1
   fi
fi


#
# running CONDUCTOR_BULK 
#
if [ "$CONDUCTOR_BULK" = ".TRUE." ] ; then  
   echo $ECHO_N "running CONDUCTOR_BULK calculation... $ECHO_C" 
   $WANT_BIN/conductor.x < $TEST_HOME/conductor_bulk.in > $TEST_HOME/conductor_bulk.out
   if [ ! -e CRASH ] ; then 
      echo "$ECHO_T done" 
      test -e dos.dat && mv dos.dat  $TEST_HOME/dos_bulk.dat
      test -e cond.dat && mv cond.dat $TEST_HOME/cond_bulk.dat
   else
      echo "$ECHO_T problems found" ; cat CRASH ; exit 1
   fi
fi


#
# running CONDUCTOR_AUTO
#
if [ "$CONDUCTOR_AUTO" = ".TRUE." ] ; then  
   echo $ECHO_N "running CONDUCTOR_AUTO calculation... $ECHO_C" 
   $WANT_BIN/conductor.x < $TEST_HOME/conductor_auto.in > $TEST_HOME/conductor_auto.out
   if [ ! -e CRASH ] ; then 
      echo "$ECHO_T done" 
      test -e dos.dat && mv dos.dat  $TEST_HOME/dos_auto.dat
      test -e cond.dat && mv cond.dat $TEST_HOME/cond_auto.dat
   else
      echo "$ECHO_T problems found" ; cat CRASH ; exit 1
   fi
fi


#
# running CHECK
#
if [ "$CHECK" = ".TRUE." ] ; then
   echo "running CHECK..."
   #
   cd $TEST_HOME
   list="disentangle_leads.out wannier_leads.out 
         disentangle_cond.out  wannier_cond.out "
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




