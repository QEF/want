#! /bin/bash
#
# Unfolding of the band structure of a Au chain (USPP, gamma)
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
 nscf            DFT inon-self-consistent calculation
 proj            atomically proj wfcs
 dft             perform SCF, NSCF, PROJ 
 disentangle     matrix elements and disentanglement
 wannier         wannierization

 unfold          compute translations and unfold the real space Hamiltonian
 unfold_proj     as above, but using atmproj hamiltonian
 bands           interpolate the band structure
 bands_proj      as above, but using atmproj hamiltonian
 want            perform DISENTANGLE, WANNIER, UNFOLD, UNFOLD_PROJ, BANDS, BANDS_PROJ
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
SUFFIX="_AuUS"

#
# evaluate the starting choice about what is to run 

SCF=
NSCF=
PROJ=
DISENTANGLE=
WANNIER=
UNFOLD=
BANDS=
CHECK=
CLEAN=

if [ $# = 0 ] ; then echo "$MANUAL" ; exit 0 ; fi
INPUT=`echo $1 | tr [:upper:] [:lower:]`

case $INPUT in 
   (scf)            SCF=yes ;;
   (nscf)           NSCF=yes ;;
   (proj)           PROJ=yes ;;
   (dft)            SCF=yes ; NSCF=yes ; PROJ=yes ;;
   (disentangle)    DISENTANGLE=yes ;;
   (wannier)        WANNIER=yes ;;
   (unfold)         UNFOLD=yes ;;
   (unfold_proj)    UNFOLD_PROJ=yes ;;
   (bands)          BANDS=yes ;;
   (bands_proj)     BANDS_PROJ=yes ;;
   (want)           DISENTANGLE=yes ; WANNIER=yes ;
                    UNFOLD=yes ; BANDS=yes ;
                    UNFOLD_PROJ=yes ; BANDS_PROJ=yes ;;
   (all)            SCF=yes ; NSCF=yes ; PROJ=yes ;
                    DISENTANGLE=yes ; WANNIER=yes ;
                    UNFOLD=yes ; BANDS=yes ;
                    UNFOLD_PROJ=yes ; BANDS_PROJ=yes ;;
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
run_dft  NAME=NSCF   SUFFIX=$SUFFIX  RUN=$NSCF

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
run_disentangle  SUFFIX=$SUFFIX  RUN=$DISENTANGLE

#
# running WANNIER
#
run_wannier  SUFFIX=$SUFFIX  RUN=$WANNIER

#
# running UNFOLD
#
run_unfold  SUFFIX=$SUFFIX  RUN=$UNFOLD
#
run_unfold  NAME=UNFOLD_PROJ  SUFFIX=${SUFFIX}_proj  RUN=$UNFOLD_PROJ

#
# running BANDS
#
run_bands  SUFFIX=$SUFFIX  RUN=$BANDS
#
run_bands  NAME=BANDS_PROJ  SUFFIX=${SUFFIX}_proj  RUN=$BANDS_PROJ


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


