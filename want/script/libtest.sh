#! /bin/bash
#
# script to manage the lowlevel launch of codes
# needed by the test suite
#

# get script homedir
LOCAL_DIR=`echo $0 | sed 's/\(.*\)\/.*/\1/'` # extract pathname

# source environment
source $LOCAL_DIR/../environment.conf

# source base def
source $LOCAL_DIR/../../script/basedef.sh

# few basic definitions
TEST_HOME=$(pwd)
TEST_NAME=$(echo $TEST_HOME | awk -v FS=\/ '{print $NF}' )


#
#----------------------
test_init () {
#----------------------
#
   
   test -e $TMPDIR/$TEST_NAME || mkdir $TMPDIR/$TEST_NAME
   #
   test -e $TEST_HOME/SCRATCH && rm $TEST_HOME/SCRATCH
   cd $TEST_HOME
   ln -sf $TMPDIR/$TEST_NAME ./SCRATCH
   #
   test -e $TMPDIR/$TEST_NAME/HOME && rm $TMPDIR/$TEST_NAME/HOME
   cd $TMPDIR/$TEST_NAME
   ln -sf $TEST_HOME ./HOME
   #
   test -e $TMPDIR/$TEST_NAME/CRASH && rm $TMPDIR/$TEST_NAME/CRASH
   #
   cd $TEST_HOME

}


#
#----------------------
run_clean () {
#----------------------
#

   cd $TEST_HOME
      rm -rf *.out *.dat 2> /dev/null
      test -e SCRATCH && rm SCRATCH

   cd $TMPDIR
      test -d $TEST_NAME && rm -rf $TEST_NAME

   exit 0
}


#
#----------------------
run () {
#----------------------
#
# low level tool to launch generic executables
#
   local NAME=
   local EXEC=
   local INPUT=
   local OUTPUT=
   local PARALLEL=

   for arg 
   do
         [[ "$arg" == NAME=* ]]      && NAME="${arg#NAME=}"
         [[ "$arg" == EXEC=* ]]      && EXEC="${arg#EXEC=}"
         [[ "$arg" == INPUT=* ]]     && INPUT="${arg#INPUT=}"
         [[ "$arg" == OUTPUT=* ]]    && OUTPUT="${arg#OUTPUT=}"
         [[ "$arg" == PARALLEL=* ]]  && PARALLEL="${arg#PARALLEL=}"
   done

   if [ -z "$NAME" ]   ; then echo "empty NAME card"   ; exit 1 ; fi 
   if [ -z "$EXEC" ]   ; then echo "empty EXEC card"   ; exit 1 ; fi 
   if [ -z "$INPUT" ]  ; then echo "empty INPUT card"  ; exit 1 ; fi 
   if [ -z "$OUTPUT" ] ; then echo "empty OUTPUT card" ; exit 1 ; fi 
   
   echo $ECHO_N "running $NAME calculation... $ECHO_C"

   #
   if [ "$PARALLEL" = "yes" ] ; then
      $PARA_PREFIX $EXEC $PARA_SUFFIX < $INPUT > $OUTPUT
   else
      $EXEC < $INPUT > $OUTPUT
   fi
   #
   if [ $? = 0 ] ; then
      echo "$ECHO_T done"
   else
      echo "$ECHO_T problems found" ; exit 1
   fi

}


#
#----------------------
run_dft () {
#----------------------
#
   local NAME=DFT
   local INPUT=$TEST_HOME/dft.in
   local OUTPUT=$TEST_HOME/dft.out
   local EXEC=$DFT_BIN/pw.x
   
   for arg 
   do
         [[ "$arg" == NAME=* ]]      && NAME="${arg#NAME=}"
         [[ "$arg" == INPUT=* ]]     && INPUT="${arg#INPUT=}"
         [[ "$arg" == OUTPUT=* ]]    && OUTPUT="${arg#OUTPUT=}"
   done
   
   run NAME=$NAME INPUT=$INPUT OUTPUT=$OUTPUT EXEC=$EXEC PARALLEL=yes
}


#
#----------------------
run_export () {
#----------------------
#
 
   local NAME=EXPORT
   local INPUT=$TEST_HOME/pwexport.in
   local OUTPUT=$TEST_HOME/pwexport.out
   local EXEC=$DFT_BIN/pw_export.x
   
   for arg 
   do
         [[ "$arg" == NAME=* ]]      && NAME="${arg#NAME=}"
         [[ "$arg" == INPUT=* ]]     && INPUT="${arg#INPUT=}"
         [[ "$arg" == OUTPUT=* ]]    && OUTPUT="${arg#OUTPUT=}"
   done
   
   run NAME=$NAME INPUT=$INPUT OUTPUT=$OUTPUT EXEC=$EXEC PARALLEL=yes
}


#
#----------------------
run_disentangle () {
#----------------------
#
   local NAME=DISENTANGLE
   local INPUT=$TEST_HOME/want.in
   local OUTPUT=$TEST_HOME/disentangle.out
   local EXEC=$WANT_BIN/disentangle.x
   
   for arg 
   do
         [[ "$arg" == NAME=* ]]      && NAME="${arg#NAME=}"
         [[ "$arg" == INPUT=* ]]     && INPUT="${arg#INPUT=}"
         [[ "$arg" == OUTPUT=* ]]    && OUTPUT="${arg#OUTPUT=}"
   done
   
   run NAME=$NAME INPUT=$INPUT OUTPUT=$OUTPUT EXEC=$EXEC 
}


#
#----------------------
run_wannier () {
#----------------------
#
   local NAME=WANNIER
   local INPUT=$TEST_HOME/want.in
   local OUTPUT=$TEST_HOME/wannier.out
   local EXEC=$WANT_BIN/wannier.x
   
   for arg 
   do
         [[ "$arg" == NAME=* ]]      && NAME="${arg#NAME=}"
         [[ "$arg" == INPUT=* ]]     && INPUT="${arg#INPUT=}"
         [[ "$arg" == OUTPUT=* ]]    && OUTPUT="${arg#OUTPUT=}"
   done
   
   run NAME=$NAME INPUT=$INPUT OUTPUT=$OUTPUT EXEC=$EXEC 
}


#
#----------------------
run_bands () {
#----------------------
#
   local NAME=BANDS
   local INPUT=$TEST_HOME/bands.in
   local OUTPUT=$TEST_HOME/bands.out
   local EXEC=$WANT_BIN/bands.x
   
   for arg 
   do
         [[ "$arg" == NAME=* ]]      && NAME="${arg#NAME=}"
         [[ "$arg" == INPUT=* ]]     && INPUT="${arg#INPUT=}"
         [[ "$arg" == OUTPUT=* ]]    && OUTPUT="${arg#OUTPUT=}"
   done
   
   run NAME=$NAME INPUT=$INPUT OUTPUT=$OUTPUT EXEC=$EXEC 
}


#
#----------------------
run_dos () {
#----------------------
#
   local NAME=DOS
   local INPUT=$TEST_HOME/dos.in
   local OUTPUT=$TEST_HOME/dos.out
   local EXEC=$WANT_BIN/dos.x
   
   for arg 
   do
         [[ "$arg" == NAME=* ]]      && NAME="${arg#NAME=}"
         [[ "$arg" == INPUT=* ]]     && INPUT="${arg#INPUT=}"
         [[ "$arg" == OUTPUT=* ]]    && OUTPUT="${arg#OUTPUT=}"
   done
   
   run NAME=$NAME INPUT=$INPUT OUTPUT=$OUTPUT EXEC=$EXEC 
}


#
#----------------------
run_plot () {
#----------------------
#
   local NAME=PLOT
   local INPUT=$TEST_HOME/plot.in
   local OUTPUT=$TEST_HOME/plot.out
   local EXEC=$WANT_BIN/plot.x
   
   for arg 
   do
         [[ "$arg" == NAME=* ]]      && NAME="${arg#NAME=}"
         [[ "$arg" == INPUT=* ]]     && INPUT="${arg#INPUT=}"
         [[ "$arg" == OUTPUT=* ]]    && OUTPUT="${arg#OUTPUT=}"
   done
   
   run NAME=$NAME INPUT=$INPUT OUTPUT=$OUTPUT EXEC=$EXEC 
}


#
#----------------------
run_conductor () {
#----------------------
#
   local NAME=CONDUCTOR
   local INPUT=$TEST_HOME/conductor.in
   local OUTPUT=$TEST_HOME/conductor.out
   local EXEC=$WANT_BIN/conductor.x
   
   for arg 
   do
         [[ "$arg" == NAME=* ]]      && NAME="${arg#NAME=}"
         [[ "$arg" == INPUT=* ]]     && INPUT="${arg#INPUT=}"
         [[ "$arg" == OUTPUT=* ]]    && OUTPUT="${arg#OUTPUT=}"
   done
   
   run NAME=$NAME INPUT=$INPUT OUTPUT=$OUTPUT EXEC=$EXEC 
}

