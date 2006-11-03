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
# source common enviroment
. ../environment.conf
#
# source low level macros for test
. ../../script/libtest.sh

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
   ( scf_cond )          SCF_COND=yes ;;
   ( nscf_cond )         NSCF_COND=yes ;;
   ( pwexport_cond )     PWEXPORT_COND=yes ;;
   ( dft_cond )          SCF_COND=yes ; NSCF_COND=yes ; PWEXPORT_COND=yes ;;
   ( disentangle_cond )  DISENTANGLE_COND=yes ;;
   ( wannier_cond )      WANNIER_COND=yes ;;
   ( bands_cond )        BANDS_COND=yes ;;
   ( plot_cond )         PLOT_COND=yes ;;
   ( want_cond )         DISENTANGLE_COND=yes ; 
                         WANNIER_COND=yes ; BANDS_COND=yes ; PLOT_COND=yes;;
   ( all_cond )          SCF_COND=yes ; NSCF_COND=yes ; PWEXPORT_COND=yes ;
                         DISENTANGLE_COND=yes ;
                         WANNIER_COND=yes ; BANDS_COND=yes ; PLOT_COND=yes;;

   ( scf_leads )         SCF_LEADS=yes ;;
   ( nscf_leads )        NSCF_LEADS=yes ;;
   ( pwexport_leads )    PWEXPORT_LEADS=yes ;;
   ( dft_leads )         SCF_LEADS=yes ; NSCF_LEADS=yes ; 
                         PWEXPORT_LEADS=yes ;;
   ( disentangle_leads ) DISENTANGLE_LEADS=yes ;;
   ( wannier_leads )     WANNIER_LEADS=yes ;;
   ( bands_leads )       BANDS_LEADS=yes ;;
   ( plot_leads )        PLOT_LEADS=yes ;;
   ( want_leads )        DISENTANGLE_LEADS=yes ; 
                         WANNIER_LEADS=yes ; BANDS_LEADS=yes ; 
                         PLOT_LEADS=yes;;
   ( all_leads )         SCF_LEADS=yes ; NSCF_LEADS=yes ; PWEXPORT_LEADS=yes ;
                         DISENTANGLE_LEADS=yes ; WANNIER_LEADS=yes ; 
                         BANDS_LEADS=yes ;  PLOT_COND=yes ;;

   ( dft )               SCF_COND=yes ; NSCF_COND=yes ; PWEXPORT_COND=yes ;
                         SCF_LEADS=yes ; NSCF_LEADS=yes ; PWEXPORT_LEADS=yes;;
   ( conductor )         CONDUCTOR=yes ;;
   ( conductor_bulk )    CONDUCTOR_BULK=yes ;;
   ( conductor_auto )    CONDUCTOR_AUTO=yes ;;
   ( want )              DISENTANGLE_COND=yes ;
                         WANNIER_COND=yes ; BANDS_COND=yes ; PLOT_COND=yes ;
                         DISENTANGLE_LEADS=yes ; WANNIER_LEADS=yes ; 
                         BANDS_LEADS=yes ;  PLOT_LEADS=yes; 
                         CONDUCTOR=yes ; CONDUCTOR_BULK=yes ; 
                         CONDUCTOR_AUTO=yes ;;
   ( all )               SCF_COND=yes ; NSCF_COND=yes ; PWEXPORT_COND=yes ; 
                         DISENTANGLE_COND=yes ; WANNIER_COND=yes ;  
                         BANDS_COND=yes ;  PLOT_COND=yes ;
                         SCF_LEADS=yes ; NSCF_LEADS=yes ; PWEXPORT_LEADS=yes; 
                         DISENTANGLE_LEADS=yes ; WANNIER_LEADS=yes ; 
                         BANDS_LEADS=yes ;  PLOT_LEADS=yes ;
                         CONDUCTOR=yes ; CONDUCTOR_BULK=yes ; 
                         CONDUCTOR_AUTO=yes ;;

   ( check )             CHECK=yes ;;
   ( clean )             CLEAN=yes ;;
   (*)                   echo " Invalid input FLAG, type ./run.sh for help" ; exit 1 ;;  
esac


#
# switches
#
if [ "$PLOT_SWITCH" = "no" ] ; then PLOT=".FALSE." ; fi


#
# initialize
#
if [ -z "$CLEAN" ] ; then
   test_init
fi
#


#-----------------------------------------------------------------------------

#
# running DFT SCF
#
run_dft  NAME=SCF_COND    SUFFIX=   RUN=$SCF_COND
run_dft  NAME=SCF_LEADS   SUFFIX=   RUN=$SCF_LEADS

#
# running DFT NSCF
#
run_dft  NAME=NSCF_COND   SUFFIX=   RUN=$NSCF_COND
run_dft  NAME=NSCF_LEADS  SUFFIX=   RUN=$NSCF_LEADS

#
# running DFT PWEXPORT
#
run_export  NAME=PWEXPORT_COND   SUFFIX="_cond"   RUN=$PWEXPORT_COND
run_export  NAME=PWEXPORT_LEADS  SUFFIX="_leads"  RUN=$PWEXPORT_LEADS


#
# running DISENTANGLE
#
run_disentangle  NAME=DISENTANGLE_COND   SUFFIX="_cond"   RUN=$DISENTANGLE_COND
run_disentangle  NAME=DISENTANGLE_LEADS  SUFFIX="_leads"  RUN=$DISENTANGLE_LEADS

#
# running WANNIER
#
run_wannier  NAME=WANNIER_COND    SUFFIX="_cond"   RUN=$WANNIER_COND
run_wannier  NAME=WANNIER_LEADS   SUFFIX="_leads"  RUN=$WANNIER_LEADS

#
# running BANDS
#
run_bands  NAME=BANDS_COND    SUFFIX="_cond"   RUN=$BANDS_COND
run_bands  NAME=BANDS_LEADS   SUFFIX="_leads"  RUN=$BANDS_LEADS

#
# running PLOT
#
run_plot  NAME=PLOT_COND    SUFFIX="_cond"   RUN=$PLOT_COND
run_plot  NAME=PLOT_LEADS   SUFFIX="_leads"  RUN=$PLOT_LEADS


#
# running CONDUCTOR
#
run_conductor NAME=CONDUCTOR  SUFFIX=""  RUN=$CONDUCTOR

#
# running CONDUCTOR_BULK 
#
run_conductor NAME=CONDUCTOR_BULK  SUFFIX="_bulk"  RUN=$CONDUCTOR_BULK

if [ "$CONDUCTOR_BULK" = yes -a ! -e CRASH ] ; then
    test -e doscond.dat  &&  mv doscond.dat  $TEST_HOME/doscond_bulk.dat
    test -e cond.dat     &&  mv cond.dat     $TEST_HOME/cond_bulk.dat
fi

#
# running CONDUCTOR_AUTO
#
run_conductor NAME=CONDUCTOR_AUTO  SUFFIX="_auto"  RUN=$CONDUCTOR_AUTO

if [ "$CONDUCTOR_AUTO" = yes -a ! -e CRASH ] ; then
    test -e doscond.dat  &&  mv doscond.dat  $TEST_HOME/doscond_auto.dat
    test -e cond.dat     &&  mv cond.dat     $TEST_HOME/cond_auto.dat
fi


#
# running CHECK
#
if [ "$CHECK" = yes ] ; then
   echo "running CHECK..."
   #
   cd $TEST_HOME
   list="disentangle_leads.out wannier_leads.out 
         disentangle_cond.out  wannier_cond.out "
   #
   for file in $list
   do
      ../../script/check.sh $file
   done
fi

#
# eventually clean
#
run_clean  RUN=$CLEAN


#
# exiting
exit 0




