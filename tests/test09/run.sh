#! /bin/bash
#
# spin polarized Ni chain
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
 proj            compute atomic projected DOS
 dft             perform all DFT calculations together
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
 conductor_up    evaluate the transmittance, for the bulk case (SPINUP)
 conductor_dw    the same for SPINDW
 conductor       conductor_up and conductor_dw
 want            want and conductor up & down
 all             perform all the above described steps

 check           check results with the reference outputs
 clean           delete all output files and the temporary directory
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
# macros
SUFFIX=

#
# evaluate the starting choice about what is to run 

SCF=
NSCF=
PWEXPORT=
PROJ=
DISENTANGLE_UP=
DISENTANGLE_DW=
WANNIER_UP=
WANNIER_DW=
BANDS_UP=
BANDS_DW=
PLOT_UP=
PLOT_DW=
CONDUCTOR_UP=
CONDUCTOR_DW=
CHECK=
CLEAN=

if [ $# = 0 ] ; then echo "$MANUAL" ; exit 0 ; fi
INPUT=`echo $1 | tr [:upper:] [:lower:]`

case $INPUT in 
   (scf)            SCF=yes ;;
   (nscf)           NSCF=yes ;;
   (pwexport)       PWEXPORT=yes ;;
   (proj)           PROJ=yes ;;
   (dft)            SCF=yes ; NSCF=yes ; PWEXPORT=yes ; PROJ=no ;;
   (disentangle_up) DISENTANGLE_UP=yes ;;
   (disentangle_dw) DISENTANGLE_DW=yes ;;
   (wannier_up)     WANNIER_UP=yes ;;
   (wannier_dw)     WANNIER_DW=yes ;;
   (bands_up)       BANDS_UP=yes ;;
   (bands_dw)       BANDS_DW=yes ;;
   (plot_up)        PLOT_UP=yes ;;
   (plot_dw)        PLOT_DW=yes ;;
   (want_up)        DISENTANGLE_UP=yes ; WANNIER_UP=yes ;
                    BANDS_UP=yes ; PLOT_UP=yes ;;
   (want_dw)        DISENTANGLE_DW=yes ; WANNIER_DW=yes ;
                    BANDS_DW=yes ; PLOT_DW=yes ;;
   (conductor_up)   CONDUCTOR_UP=yes ;;
   (conductor_dw)   CONDUCTOR_DW=yes ;;
   (conductor)      CONDUCTOR_UP=yes ; CONDUCTOR_DW=yes ;;
   (want)           DISENTANGLE_UP=yes ; WANNIER_UP=yes ; BANDS_UP=yes ;
                    DISENTANGLE_DW=yes ; WANNIER_DW=yes ; BANDS_DW=yes ; 
                    PLOT_UP=yes ; PLOT_DW=yes ;
                    CONDUCTOR_UP=yes ; CONDUCTOR_DW=yes ;;
   (all)            SCF=yes ; NSCF=yes ; PWEXPORT=yes ; PROJ=no ;
                    DISENTANGLE_UP=yes ; WANNIER_UP=yes ; 
                    BANDS_UP=yes ; CONDUCTOR_UP=yes ;
                    DISENTANGLE_DW=yes ; WANNIER_DW=yes ; 
                    PLOT_UP=yes ; PLOT_DW=yes ;
                    BANDS_DW=yes ; CONDUCTOR_DW=yes ;;
   (check)          CHECK=yes ;;
   (clean)          CLEAN=yes ;;
   (*)              echo " Invalid input FLAG, type ./run.sh for help" ; exit 1 ;;
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
run_dft  NAME=SCF   SUFFIX=$SUFFIX  RUN=$SCF

#
# running DFT NSCF
#
run_dft  NAME=NSCF  SUFFIX=$SUFFIX  RUN=$NSCF

#
# running DFT PWEXPORT
#
run_export  SUFFIX=$SUFFIX  RUN=$PWEXPORT

#
# running DFT PROJ
#
if [ "$PROJ" = "yes" ] ; then
   #
   run  NAME="PROJ"  EXEC=$QE_BIN/projwfc.x  INPUT=proj$SUFFIX.in \
        OUTPUT=proj$SUFFIX.out PARALLEL=yes
fi


#
# running DISENTANGLE
#
run_disentangle  NAME=DISENTANGLE_UP  SUFFIX=${SUFFIX}_UP  RUN=$DISENTANGLE_UP
run_disentangle  NAME=DISENTANGLE_DW  SUFFIX=${SUFFIX}_DW  RUN=$DISENTANGLE_DW

#
# running WANNIER
#
run_wannier  NAME=WANNIER_UP  SUFFIX=${SUFFIX}_UP  RUN=$WANNIER_UP
run_wannier  NAME=WANNIER_DW  SUFFIX=${SUFFIX}_DW  RUN=$WANNIER_DW

#
# running BANDS
#
run_bands  NAME=BANDS_UP  SUFFIX=${SUFFIX}_UP  RUN=$BANDS_UP
run_bands  NAME=BANDS_DW  SUFFIX=${SUFFIX}_DW  RUN=$BANDS_DW

#
# running PLOT
#
run_plot  NAME=PLOT_UP  SUFFIX=${SUFFIX}_UP  RUN=$PLOT_UP
run_plot  NAME=PLOT_DW  SUFFIX=${SUFFIX}_DW  RUN=$PLOT_DW

#
# running CONDUCTOR
#
run_conductor NAME=CONDUCTOR_UP SUFFIX=${SUFFIX}_UP  RUN=$CONDUCTOR_UP
run_conductor NAME=CONDUCTOR_DW SUFFIX=${SUFFIX}_DW  RUN=$CONDUCTOR_DW


#
# running CHECK
#
if [ "$CHECK" = yes ] ; then
   echo "running CHECK..."
   #
   cd $TEST_HOME
   list="disentangle${SUFFIX}_UP.out wannier${SUFFIX}_UP.out 
         disentangle${SUFFIX}_DW.out wannier${SUFFIX}_DW.out"
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

