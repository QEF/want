#! /bin/bash 
#
# Pt-H2-Pt junction
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
 scf_bulk1       DFT self-consistent calculation for Pt bulk (1atom)
 nscf_bulk1      DFT non-self-consistent calculation for Pt bulk (1atom)
 scf_bulk4       DFT self-consistent calculation for Pt bulk (4atoms)
 nscf_bulk4      DFT non-self-consistent calculation for Pt bulk (4atoms)
 dft             perform all the above together

 disentangle     select the optimal subspace on which perform
                 the wannier minimization
 wannier         perform the above cited minimization
 plot            compute WFs on real space for plotting
 disentangle_bulk1
 wannier_bulk1
 disentangle_bulk4
 wannier_bulk4   want calculations for Pt bulk (1 and 4 atoms)

 conductor_lead1 evaluate the transmittance across the junction
                 using leads with 1 aton
 conductor_lead4 evaluate the transmittance across the junction
                 using leads with 4 atons
 conductor_auto  evaluate the transmittance taking all the matrix elements
                 from the same calculation (not recommended in general)
 conductor_bulk1 evaluate the bulk transmittance for the leads (1 atom)
 conductor_bulk4 evaluate the bulk transmittance for the leads (4 atoms)
 current_lead1   compute the current from the results of conductor_lead1
 current_lead4   compute the current from the results of conductor_lead4

 want            perform all WANT calculations
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

SUFFIX=

#
# evaluate the starting choice about what is to run 

SCF=
NSCF=
SCF_BULK1=
NSCF_BULK1=
SCF_BULK4=
NSCF_BULK4=

DISENTANGLE=
WANNIER=
PLOT=
DISENTANGLE_BULK1=
WANNIER_BULK1=
DISENTANGLE_BULK4=
WANNIER_BULK4=

CONDUCTOR_BULK1=
CONDUCTOR_BULK4=
CONDUCTOR_LEAD1=
CONDUCTOR_LEAD4=
CONDUCTOR_AUTO=
CURRENT_LEAD1=
CURRENT_LEAD4=

CHECK=
CLEAN=

if [ $# = 0 ] ; then echo "$MANUAL" ; exit 0 ; fi
INPUT=`echo $1 | tr [:upper:] [:lower:]`

case $INPUT in 
   (scf)            SCF=yes ;;
   (nscf)           NSCF=yes ;;
   (scf_bulk1)      SCF_BULK1=yes ;;
   (nscf_bulk1)     NSCF_BULK1=yes ;;
   (scf_bulk4)      SCF_BULK4=yes ;;
   (nscf_bulk4)     NSCF_BULK4=yes ;;

   (dft)            SCF=yes ; NSCF=yes ; 
                    SCF_BULK1=yes ; NSCF_BULK1=yes ;
                    SCF_BULK4=yes ; NSCF_BULK4=yes ;;

   (disentangle)    DISENTANGLE=yes ;;
   (wannier)        WANNIER=yes ;;
   (plot)           PLOT=yes ;;
   (disentangle_bulk1)    DISENTANGLE_BULK1=yes ;;
   (wannier_bulk1)        WANNIER_BULK1=yes ;;
   (disentangle_bulk4)    DISENTANGLE_BULK4=yes ;;
   (wannier_bulk4)        WANNIER_BULK4=yes ;;

   (conductor_bulk1)      CONDUCTOR_BULK1=yes ;;
   (conductor_bulk4)      CONDUCTOR_BULK4=yes ;;
   (conductor_lead1)      CONDUCTOR_LEAD1=yes ;;
   (conductor_lead4)      CONDUCTOR_LEAD4=yes ;;
   (conductor_auto)       CONDUCTOR_AUTO=yes ;;
   (current_lead1)        CURRENT_LEAD1=yes ;;
   (current_lead4)        CURRENT_LEAD4=yes ;;
 
   (want)           DISENTANGLE=yes ; WANNIER=yes ; PLOT=yes ;
                    DISENTANGLE_BULK1=yes ; WANNIER_BULK1=yes ; 
                    DISENTANGLE_BULK4=yes ; WANNIER_BULK4=yes ; 
                    CONDUCTOR_BULK1=yes ; CONDUCTOR_BULK4=yes ; 
                    CONDUCTOR_LEAD1=yes ; CONDUCTOR_LEAD4=yes ; 
                    CONDUCTOR_AUTO=yes ; CONDUCTOR_BULK=yes ; 
                    CURRENT_LEAD1=yes ; CURRENT_LEAD4=yes ;; 

   (all)            SCF=yes ; NSCF=yes ; 
                    SCF_BULK1=yes ; NSCF_BULK1=yes ;
                    SCF_BULK4=yes ; NSCF_BULK4=yes ;
                    DISENTANGLE=yes ; WANNIER=yes ; PLOT=yes ;
                    DISENTANGLE_BULK1=yes ; WANNIER_BULK1=yes ; 
                    DISENTANGLE_BULK4=yes ; WANNIER_BULK4=yes ; 
                    CONDUCTOR_BULK1=yes ; CONDUCTOR_BULK4=yes ; 
                    CONDUCTOR_LEAD1=yes ; CONDUCTOR_LEAD4=yes ; 
                    CONDUCTOR_AUTO=yes ; CONDUCTOR_BULK=yes ; 
                    CURRENT_LEAD1=yes ; CURRENT_LEAD4=yes ;; 

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
# running DFT SCF BULK1
#
run_dft  NAME=SCF_BULK1   SUFFIX=${SUFFIX}  RUN=$SCF_BULK1

#
# running DFT NSCF BULK1
#
run_dft  NAME=NSCF_BULK1   SUFFIX=${SUFFIX}  RUN=$NSCF_BULK1


#
# running DFT SCF BULK4
#
run_dft  NAME=SCF_BULK4   SUFFIX=${SUFFIX}  RUN=$SCF_BULK4

#
# running DFT NSCF BULK4
#
run_dft  NAME=NSCF_BULK4   SUFFIX=${SUFFIX}  RUN=$NSCF_BULK4



#
# running DISENTANGLE
#
run_disentangle  SUFFIX=$SUFFIX  RUN=$DISENTANGLE

#
# running WANNIER
#
run_wannier  SUFFIX=$SUFFIX  RUN=$WANNIER

#
# running PLOT
#
run_plot  SUFFIX=$SUFFIX  RUN=$PLOT


#
# running DISENTANGLE BULK1
#
run_disentangle  NAME=DISENTANGLE_BULK1  SUFFIX=${SUFFIX}_bulk1  RUN=$DISENTANGLE_BULK1

#
# running WANNIER BULK1
#
run_wannier  NAME=WANNIER_BULK1  SUFFIX=${SUFFIX}_bulk1  RUN=$WANNIER_BULK1

#
# running DISENTANGLE BULK4
#
run_disentangle  NAME=DISENTANGLE_BULK1  SUFFIX=${SUFFIX}_bulk4 RUN=$DISENTANGLE_BULK4

#
# running WANNIER BULK4
#
run_wannier  NAME=WANNIER_BULK4  SUFFIX=${SUFFIX}_bulk4  RUN=$WANNIER_BULK4




#
# running CONDUCTOR BULK
#
run_conductor NAME=CONDUCTOR_BULK1  SUFFIX=$SUFFIX  RUN=$CONDUCTOR_BULK1
run_conductor NAME=CONDUCTOR_BULK4  SUFFIX=$SUFFIX  RUN=$CONDUCTOR_BULK4

#
# running CONDUCTOR LEAD
#
run_conductor NAME=CONDUCTOR_LEAD1  SUFFIX=$SUFFIX  RUN=$CONDUCTOR_LEAD1
run_conductor NAME=CONDUCTOR_LEAD4  SUFFIX=$SUFFIX  RUN=$CONDUCTOR_LEAD4

#
# running CONDUCTOR_AUTO
#
run_conductor NAME=CONDUCTOR_AUTO  SUFFIX=${SUFFIX}_auto  RUN=$CONDUCTOR_AUTO

#
# running CURRENT
#
run_current NAME=CURRENT_LEAD1  SUFFIX=$SUFFIX  RUN=$CURRENT_LEAD1
run_current NAME=CURRENT_LEAD4  SUFFIX=$SUFFIX  RUN=$CURRENT_LEAD4



#
# running CHECK
#
if [ "$CHECK" = yes ] ; then
   echo "running CHECK..."
   #
   cd $TEST_HOME
   list="disentangle$SUFFIX.out wannier$SUFFIX.out
         disentangle_bulk1$SUFFIX.out wannier_bulk1$SUFFIX.out
         disentangle_bulk4$SUFFIX.out wannier_bulk4$SUFFIX.out
        "
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


