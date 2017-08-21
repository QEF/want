#! /bin/bash
#
# Polymers computed using the WanT-CRYSTAL interface
#
# PA (polyacetylene, van Faassen geometry, PRL 88 186401 (2002) )
# PE (polytethilene)
# See also PRB 85, 235105 (2012) 
# Compare with test16 for PA results
# 
#================================================================
#
# Input flags for this script (./run.sh FLAG): 
#
MANUAL=" Usage
   run.sh [FLAG]

 where FLAG is one of the following 
 (no FLAG will print this manual page) :
 
 PA_bands        interpolate the PA band structure from CRYSTAL datafile (LDA xc)
 PA_dos          interpolate the PA DOS from CRYSTAL datafile (LDA xc)
 PE_bands        interpolate the PE band structure from CRYSTAL datafile (LDA xc)
 PE_dos          interpolate the PE DOS from CRYSTAL datafile (LDA xc)
 PE_bands_hyb    interpolate the PE band structure from CRYSTAL datafile (B3LYP xc)
 PE_dos_hyb      interpolate the PE DOS from CRYSTAL datafile (B3LYP xc)

 all             perform all the above described steps

 check           check results with the reference outputs
 clean           delete all output files and the temporary directory
"
#
#================================================================
#
# conductor       evaluate the transmittance, for the bulk case (LDA xc)

#
# source common enviroment
. ../environment.conf
#
# source low level macros for test
. ../../script/libtest.sh


#
# evaluate the starting choice about what is to run 

PA_BANDS=
PA_DOS=
PE_BANDS=
PE_DOS=
PE_BANDS_HYB=
PE_DOS_HYB=

CHECK=
CLEAN=

if [ $# = 0 ] ; then echo "$MANUAL" ; exit 0 ; fi
INPUT=`echo $1 | tr [:upper:] [:lower:]`

case $INPUT in 
   (pa_bands)      PA_BANDS=yes ;;
   (pa_dos)        PA_DOS=yes ;;
   (pe_bands)      PE_BANDS=yes ;;
   (pe_dos)        PE_DOS=yes ;;
   (pe_bands_hyb)  PE_BANDS_HYB=yes ;;
   (pe_dos_hyb)    PE_DOS_HYB=yes ;;
   (all|want)      PA_BANDS=yes ; PA_DOS=yes ;
                   PE_BANDS=yes ; PE_DOS=yes ;
                   PE_BANDS_HYB=yes ; PE_DOS_HYB=yes ;;
   (check)         CHECK=yes ;;
   (clean)         CLEAN=yes ;;
   (*)             echo " Invalid input FLAG, type ./run.sh for help" ; exit 1 ;;
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

SUFFIX="_PA_crys"

#
# running PA_BANDS
#
run_bands  NAME=PA_BANDS  SUFFIX=${SUFFIX} RUN=$PA_BANDS
#
# running PA_DOS
#
run_dos  NAME=PA_DOS  SUFFIX=${SUFFIX} RUN=$PA_DOS


SUFFIX="_PE_crys"

#
# running PE_BANDS
#
run_bands  NAME=PE_BANDS  SUFFIX=${SUFFIX} RUN=$PE_BANDS
#
# running PE_DOS
#
run_dos  NAME=PE_DOS  SUFFIX=${SUFFIX} RUN=$PE_DOS


SUFFIX="_PE_B3LYPcrys"

#
# running PE_BANDS_hyb
#
run_bands  NAME=PE_BANDS_HYB  SUFFIX=${SUFFIX} RUN=$PE_BANDS
#
# running PE_DOS_hyb
#
run_dos  NAME=PE_DOS_HYB  SUFFIX=${SUFFIX} RUN=$PE_DOS

#
# running CHECK
#
if [ "$CHECK" = yes ] ; then
   echo "running CHECK... "
   #
   cd $TEST_HOME
   list=""
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
if [ -d $TEST_HOME/CRYSTAL ] ; then
   cd $TEST_HOME/CRYSTAL ; rm -f *.xml.ham 2> /dev/null
   cd $TEST_HOME
fi


#
# exiting
exit 0


