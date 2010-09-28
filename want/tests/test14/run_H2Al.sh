#! /bin/bash
#
# h2 molecule @ Al self-standing layer
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
 nscf            DFT nscf calc (used to obtain more conduction bands)
 nscf_dos        DFT nscf calc to compute DFT dos
 dft             perform SCF, NSCF and PWEXPORT all together
 proj            compute atomic projected DOS using QE
 disentangle     select the optimal subspace on which perform the wannier minimization 
 wannier         perform the above cited minimization 
 embed           perform the embedding procedure on the H2 molecule
 want            perform ALL want calculations
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
SUFFIX="_H2Al"

#
# evaluate the starting choice about what is to run 

SCF=
NSCF=
NSCF_DOS=
PROJ=
DISENTANGLE=
WANNIER=
EMBED=
DOS=
DOS_SGM=
CHECK=
CLEAN=

if [ $# = 0 ] ; then echo "$MANUAL" ; exit 0 ; fi
INPUT=`echo $1 | tr [:upper:] [:lower:]`

case $INPUT in 
   (scf)            SCF=yes ;;
   (nscf)           NSCF=yes ;;
   (nscf_dos)       NSCF_DOS=yes ;;
   (proj)           PROJ=yes ;;
   (dft)            SCF=yes; NSCF=yes ;; 
   (disentangle)    DISENTANGLE=yes ;;
   (wannier)        WANNIER=yes ;;
   (embed)          EMBED=yes ;;
   (embed_sgm)      EMBED_SGM=yes ;;
   (dos)            DOS=yes ;;
   (dos_sgm)        DOS_SGM=yes ;;
   (want)           DISENTANGLE=yes ; WANNIER=yes ;
                    EMBED=yes ; EMBED_SGM=yes ; DOS=yes ; DOS_SGM=yes ;;
   (all)            SCF=yes ;   NSCF=yes ;
                    DISENTANGLE=yes ; WANNIER=yes ; 
                    EMBED=yes ; EMBED_SGM=yes ; DOS=yes ; DOS_SGM=yes ;;
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
# running DFT NSCF_DOS
#
run_dft  NAME=NSCF_DOS  SUFFIX=$SUFFIX  RUN=$NSCF_DOS

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
run_disentangle  SUFFIX=$SUFFIX  RUN=$DISENTANGLE  PARALLEL=yes

#
# running WANNIER
#
run_wannier  SUFFIX=$SUFFIX  RUN=$WANNIER  PARALLEL=yes

#
# running EMBED
#
run_embed  SUFFIX=$SUFFIX        RUN=$EMBED      PARALLEL=yes
run_embed  SUFFIX=${SUFFIX}_sgm  RUN=$EMBED_SGM  PARALLEL=yes

#
# running DOS
#
run_dos  SUFFIX=$SUFFIX        RUN=$DOS      PARALLEL=yes
run_dos  SUFFIX=${SUFFIX}_sgm  RUN=$DOS_SGM  PARALLEL=yes



#
# running CHECK
#
if [ "$CHECK" = yes ] ; then
   echo "running CHECK..."
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

