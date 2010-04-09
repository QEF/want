#! /bin/bash
#
# Si bulk 
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
 dft             perform SCF, NSCF all together
 disentangle     select the optimal subspace on which perform
                 the wannier minimization
 wannier         perform the above cited minimization
 dos             compute DOS using WFs
 blc2wan         convert a model self-energy to wannier basis set
 conductor       evaluate the transmittance, for the bulk case
 conductor_sgm   as before, including a model sefl-energy (sigma_graphene.xml)
 want            perform DISENTANGLE, WANNIER, PLOT, CONDUCTOR all together 
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
DISENTANGLE=
WANNIER=
DOS=
BLC2WAN=
CONDUCTOR=
CONDUCTOR_SGM=
CHECK=
CLEAN=

if [ $# = 0 ] ; then echo "$MANUAL" ; exit 0 ; fi
INPUT=`echo $1 | tr [:upper:] [:lower:]`

case $INPUT in 
   (scf)            SCF=yes ;;
   (nscf)           NSCF=yes ;;
   (dft)            SCF=yes ; NSCF=yes ;;
   (disentangle)    DISENTANGLE=yes ;;
   (wannier)        WANNIER=yes ;;
   (dos)            DOS=yes ;;
   (blc2wan)        BLC2WAN=yes ;;
   (conductor)      CONDUCTOR=yes ;;
   (conductor_sgm)  BLC2WAN=yes ; CONDUCTOR_SGM=yes ;;
   (want)           DISENTANGLE=yes ; WANNIER=yes ;
                    DOS=yes ; BLC2WAN=yes ;
                    CONDUCTOR=yes ; CONDUCTOR_SGM=yes ;;
   (all)            SCF=yes ; NSCF=yes ; 
                    DISENTANGLE=yes ; WANNIER=yes ;
                    DOS=yes ; BLC2WAN=yes ;
                    CONDUCTOR=yes ; CONDUCTOR_SGM=yes ;;
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
# running DISENTANGLE
#
run_disentangle  SUFFIX=$SUFFIX  RUN=$DISENTANGLE

#
# running WANNIER
#
run_wannier  SUFFIX=$SUFFIX  RUN=$WANNIER

#
# running DOS
#
run_dos  SUFFIX=$SUFFIX  RUN=$DOS

#
# running DOS
#
run_blc2wan  SUFFIX=$SUFFIX  RUN=$BLC2WAN

#
# running CONDUCTOR
#
run_conductor SUFFIX=$SUFFIX  RUN=$CONDUCTOR
run_conductor  NAME=CONDUCTOR_SGM   SUFFIX=${SUFFIX}_sgm  RUN=$CONDUCTOR_SGM


#
# running CHECK
#
if [ "$CHECK" = yes ] ; then
   echo "running CHECK... "
   #
   cd $TEST_HOME
   list="disentangle$SUFFIX.out wannier$SUFFIX.out"
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


