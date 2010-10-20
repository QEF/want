#! /bin/bash
#
# Hydrogen Chain, using CRYSTAL data, spin polarized
# 
#================================================================
#
# Input flags for this script (./run.sh FLAG): 
#
MANUAL=" Usage
   run.sh [FLAG]

 where FLAG is one of the following 
 (no FLAG will print this manual page) :
 
 conductor_LDA   evaluate the transmittance, for the bulk case (LDA xc)
 conductor_HF    evaluate the transmittance, for the bulk case (HF xc)

 all             perform all the above described steps

 check           check results with the reference outputs
 clean           delete all output files and the temporary directory
"
#
#================================================================
#

######
# bands_LDA       interpolate the band structure from CRYSTAL datafile (LDA xc)
# dos_LDA         interpolate DOS from CRYSTAL datafile (LDA xc)
# want_LDA        perform BANDS, DOS, CONDUCTOR all together (LDA xc)
#
# bands_HF        interpolate the band structure from CRYSTAL datafile (HF xc)
# dos_HF          interpolate DOS from CRYSTAL datafile (HF xc)
# want_HF         perform BANDS, DOS, CONDUCTOR all together (HF xc)
######

#
# source common enviroment
. ../environment.conf
#
# source low level macros for test
. ../../script/libtest.sh

#
# macros
SUFFIX="_H6_spin_cry"

#
# evaluate the starting choice about what is to run 

BANDS_LDA=
DOS_LDA=
CONDUCTOR_LDA=

BANDS_HF=
DOS_HF=
CONDUCTOR_HF=

CHECK=
CLEAN=

if [ $# = 0 ] ; then echo "$MANUAL" ; exit 0 ; fi
INPUT=`echo $1 | tr [:upper:] [:lower:]`

case $INPUT in 
#   (bands_lda)      BANDS_LDA=yes ;;
#   (dos_lda)        DOS_LDA=yes ;;
   (conductor_lda)  CONDUCTOR_LDA=yes ;;
#   (want_lda)       BANDS_LDA=yes ; DOS_LDA=yes ; CONDUCTOR_LDA=yes ;;
#   (bands_hf)       BANDS_HF=yes ;;
#   (dos_hf)         DOS_HF=yes ;;
   (conductor_hf)   CONDUCTOR_HF=yes ;;
#   (want_hf)        BANDS_HF=yes ;  DOS_HF=yes ;  CONDUCTOR_HF=yes ;;
#   (all)            BANDS_LDA=yes ; DOS_LDA=yes ; CONDUCTOR_LDA=yes ;
#                    BANDS_HF=yes ;  DOS_HF=yes ;  CONDUCTOR_HF=yes ;;
   (want)           CONDUCTOR_LDA=yes ; CONDUCTOR_HF=yes ;;
   (all)            CONDUCTOR_LDA=yes ; CONDUCTOR_HF=yes ;;
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

##
## running BANDS LDA
##
#run_bands  NAME=BANDS_LDA_UP  SUFFIX=${SUFFIX}LDA_UP  RUN=$BANDS_LDA
#run_bands  NAME=BANDS_LDA_DW  SUFFIX=${SUFFIX}LDA_DW  RUN=$BANDS_LDA

##
## running DOS LDA
##
#run_dos  NAME=DOS_LDA_UP  SUFFIX=${SUFFIX}LDA_UP  RUN=$DOS_LDA
#run_dos  NAME=DOS_LDA_DW  SUFFIX=${SUFFIX}LDA_DW  RUN=$DOS_LDA

#
# running CONDUCTOR LDA
#
run_conductor  NAME=CONDUCTOR_LDA_UP  SUFFIX=${SUFFIX}LDA_UP  RUN=$CONDUCTOR_LDA
run_conductor  NAME=CONDUCTOR_LDA_DW  SUFFIX=${SUFFIX}LDA_DW  RUN=$CONDUCTOR_LDA

##
## running BANDS HF
##
#run_bands  NAME=BANDS_HF_UP  SUFFIX=${SUFFIX}HF_UP  RUN=$BANDS_HF
#run_bands  NAME=BANDS_HF_DW  SUFFIX=${SUFFIX}HF_DW  RUN=$BANDS_HF

##
## running DOS HF
##
#run_dos  NAME=DOS_HF_UP  SUFFIX=${SUFFIX}HF_UP  RUN=$DOS_HF
#run_dos  NAME=DOS_HF_DW  SUFFIX=${SUFFIX}HF_DW  RUN=$DOS_HF

#
# running CONDUCTOR HF
#
run_conductor NAME=CONDUCTOR_HF_UP  SUFFIX=${SUFFIX}HF_UP  RUN=$CONDUCTOR_HF
run_conductor NAME=CONDUCTOR_HF_DW  SUFFIX=${SUFFIX}HF_DW  RUN=$CONDUCTOR_HF



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


