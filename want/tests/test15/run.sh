#! /bin/bash 
#
# Au-BDA-Au junction
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
 scf_bulk        DFT self-consistent calculation for Au bulk (4atoms)
 nscf_bulk       DFT non-self-consistent calculation for Au bulk (4atoms)
 dft             perform all the above together

 disentangle     select the optimal subspace on which perform
                 the wannier minimization
 wannier         perform the above cited minimization
 dos             DOS for the conductor region

 disentangle_bulk
 wannier_bulk
 dos_bulk        want calculations for Au bulk (4 atoms per cell)

 conductor       evaluate the transmittance across the junction
 conductor_bulk  evaluate the bulk transmittance for the leads
 plot_eigchn     compute and plot eigenchannels in real space

 want            perform all WanT calculations
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
SCF_BULK=
NSCF_BULK=

DISENTANGLE=
WANNIER=
DOS=

DISENTANGLE_BULK=
WANNIER_BULK=
DOS_BULK=

CONDUCTOR=
CONDUCTOR_BULK=
PLOT_EIGCHN=

CHECK=
CLEAN=

if [ $# = 0 ] ; then echo "$MANUAL" ; exit 0 ; fi
INPUT=`echo $1 | tr [:upper:] [:lower:]`

case $INPUT in 
   (scf)            SCF=yes ;;
   (nscf)           NSCF=yes ;;
   (scf_bulk)       SCF_BULK=yes ;;
   (nscf_bulk)      NSCF_BULK=yes ;;

   (dft)            SCF=yes ; NSCF=yes ; 
                    SCF_BULKx1=yes ; NSCF_BULK=yes ;;

   (disentangle)    DISENTANGLE=yes ;;
   (wannier)        WANNIER=yes ;;
   (dos)            DOS=yes ;;

   (disentangle_bulk)    DISENTANGLE_BULK=yes ;;
   (wannier_bulk)        WANNIER_BULK=yes ;;
   (dos_bulk)            DOS_BULK=yes ;;

   (conductor)      CONDUCTOR=yes ;;
   (conductor_bulk) CONDUCTOR_BULK=yes ;;
   (plot_eigchn)    PLOT_EIGCHN=yes ;;

   (want)           DISENTANGLE=yes ; WANNIER=yes ; DOS=yes ; 
                    DISENTANGLE_BULK=yes ; WANNIER_BULK=yes ; DOS_BULK=yes ;
                    CONDUCTOR=yes ; CONDUCTOR_BULK=yes ; PLOT_EIGCHN=yes ;;

   (all)            SCF=yes ; NSCF=yes ; 
                    SCF_BULK=yes ; NSCF_BULK=yes ;
                    DISENTANGLE=yes ; WANNIER=yes ; DOS=yes ;
                    DISENTANGLE_BULK=yes ; WANNIER_BULK=yes ; 
                    DOS_BULK=yes ; 
                    CONDUCTOR=yes ; CONDUCTOR_BULK=yes ; PLOT_EIGCHN=yes ;; 

   (check)          CHECK=yes ;;
   (clean)          CLEAN=yes ;;
   (*)              echo " Invalid input FLAG, type ./run.sh for help" ; exit 1 ;;
esac

#
# switches
#
if [ "$PLOT_SWITCH" = "no" ] ; then 
   PLOT_EIGCHN=".FALSE." 
fi


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
# running DFT SCF BULK
#
run_dft  NAME=SCF_BULK   SUFFIX=${SUFFIX}  RUN=$SCF_BULK

#
# running DFT NSCF BULK
#
run_dft  NAME=NSCF_BULK   SUFFIX=${SUFFIX}  RUN=$NSCF_BULK



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
# compute molecular pdos
if [ "$DOS" = "yes" -a -e pdos_mol.list ] ; then 
   #
   $WANT_BIN/sumpdos -f pdos_mol.list > pdos_mol.dat
   #
fi



#
# running DISENTANGLE BULK
#
run_disentangle  NAME=DISENTANGLE_BULK  SUFFIX=${SUFFIX}_bulk  RUN=$DISENTANGLE_BULK

#
# running WANNIER BULK
#
run_wannier  NAME=WANNIER_BULK  SUFFIX=${SUFFIX}_bulk  RUN=$WANNIER_BULK

#
# running DOS BULK
#
run_dos  NAME=DOS_BULK  SUFFIX=${SUFFIX}_bulk  RUN=$DOS_BULK



#
# running CONDUCTOR BULK
#
run_conductor NAME=CONDUCTOR_BULK  SUFFIX=${SUFFIX}_bulk  RUN=$CONDUCTOR_BULK

#
# running CONDUCTOR
#
run_conductor NAME=CONDUCTOR  SUFFIX=${SUFFIX}  RUN=$CONDUCTOR
if [ -e eigchn.dat ] ; then mv eigchn.dat ./SCRATCH ; fi

#
# running PLOT_EIGCHN
#
run_plot  NAME=PLOT_EIGCHN  SUFFIX=${SUFFIX}_eigchn  RUN=$PLOT_EIGCHN


#
# running CHECK
#
if [ "$CHECK" = yes ] ; then
   echo "running CHECK..."
   #
   cd $TEST_HOME
   list="disentangle$SUFFIX.out wannier$SUFFIX.out
         disentangle_bulk$SUFFIX.out wannier_bulk$SUFFIX.out
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


