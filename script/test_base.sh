#! /bin/bash
#
# subscript to manage the lowlevel launch
# of programs needed by the test suite
#

# get script homedir
SCRIPT_DIR=`echo $0 | sed 's/\(.*\)\/.*/\1/'` # extract pathname

# source base def
source $SCRIPT_DIR/basedef.sh

# few basic definitions
TEST_HOME=$(pwd)
TEST_NAME=$(echo $TEST_HOME | awk -v FS=\/ '{print $NF}' )


#
#----------------------
test_init () {
#----------------------
#
   local tmpdir=./   

   for arg 
   do
       [[ "$arg" == TMPDIR=* ]]    && tmpdir="${arg#TMPDIR=}"
   done

   #
   test -e $tmpdir/$TEST_NAME || mkdir $tmpdir/$TEST_NAME
   #
   test -e $TEST_HOME/SCRATCH && rm $TEST_HOME/SCRATCH
   cd $TEST_HOME
   ln -sf $tmpdir/$TEST_NAME ./SCRATCH
   #
   test -e $tmpdir/$TEST_NAME/HOME && rm $tmpdir/$TEST_NAME/HOME
   cd $tmpdir/$TEST_NAME
   ln -sf $TEST_HOME ./HOME
   #
   test -e $tmpdir/$TEST_NAME/CRASH && rm $tmpdir/$TEST_NAME/CRASH

}


#
#----------------------
run_dft () {
#----------------------
#
   local name=DFT
   local input=$TEST_HOME/dft.in
   local output=$TEST_HOME/dft.out
   
   for arg 
   do
         [[ "$arg" == NAME=* ]]      && flag="${arg#NAME=}"
         [[ "$arg" == INPUT=* ]]     && flag="${arg#INPUT=}"
         [[ "$arg" == OUTPUT=* ]]    && flag="${arg#OUTPUT=}"
   done
   
   echo $ECHO_N "running $name calculation... $ECHO_C"
   $PARA_PREFIX  $DFT_BIN/pw.x $PARA_POSTFIX < $input > $output
   if [ $? = 0 ] ; then
      echo "$ECHO_T done"
   else
      echo "$ECHO_T problems found" ; exit 1
   fi

}
