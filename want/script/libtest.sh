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

# set redirection
case $INPUT_TYPE in
 ("from_stdin")       INPUT_REDIRECT="<" ;;
 ("from_file")        INPUT_REDIRECT="-input" ;;
 (*)                  INPUT_REDIRECT="$INPUT_TYPE" ;;
esac

# few basic definitions
TEST_HOME=$(pwd)
TEST_NAME=$(echo $TEST_HOME | awk -v FS=\/ '{print $NF}' )


#
#----------------------
test_init () {
#----------------------
#
   
   #
   # exit if TMPDIR dies not exist
   if [ ! -d $TMPDIR ] ; then 
       echo "TMPDIR = $TMPDIR   does not exist " ; exit 71 
   fi

   #
   # if the case, create local test dir
   test -d $TMPDIR/$TEST_NAME || mkdir $TMPDIR/$TEST_NAME
   #
   # create SCRATCH link
   test -e $TEST_HOME/SCRATCH && rm $TEST_HOME/SCRATCH
   cd $TEST_HOME
   ln -sf $TMPDIR/$TEST_NAME ./SCRATCH
   #
   # create HOME link
   test -e $TMPDIR/$TEST_NAME/HOME && rm $TMPDIR/$TEST_NAME/HOME
   cd $TMPDIR/$TEST_NAME
   ln -sf $TEST_HOME ./HOME
   #
   test -e $TMPDIR/$TEST_NAME/CRASH && rm $TMPDIR/$TEST_NAME/CRASH
   test -e $TEST_HOME/CRASH && rm $TEST_HOME/CRASH
   #
   cd $TEST_HOME

}


#
#----------------------
run_clean () {
#----------------------
#
   local RUN=

   for arg
   do
         [[ "$arg" == RUN=* ]]       && RUN="${arg#RUN=}"
   done

   [[ "$RUN" != "yes" ]]  && return

   #
   # actual clean up
   #
   cd $TEST_HOME
      rm -rf *.out *.dat 2> /dev/null
      test -e SCRATCH && rm SCRATCH
      test -e CRASH   && rm CRASH

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
   
   if [ ! -z $NAME ] ; then
      #
      echo $ECHO_N "running $NAME calculation... $ECHO_C"
      #
   fi

   #
   if [ "$INPUT_TYPE" = "from_stdin" ] ; then
      #
      if [ "$PARALLEL" = "yes" ] ; then
         $PARA_PREFIX $EXEC $PARA_POSTFIX < $INPUT > $OUTPUT
      else
         $EXEC < $INPUT > $OUTPUT
      fi
   fi
   #
   if [ "$INPUT_TYPE" = "from_file" ] ; then
      #
      if [ "$PARALLEL" = "yes" ] ; then
         $PARA_PREFIX $EXEC $PARA_POSTFIX -input $INPUT > $OUTPUT
      else
         $EXEC -input $INPUT > $OUTPUT
      fi
   fi
   #
   if [ $? = 0 ] ; then
      if [ ! -z $NAME ] ; then echo "$ECHO_T done" ; fi
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
   local EXEC=$DFT_BIN/pw.x
   local RUN=yes
   local INPUT=
   local OUTPUT=
   local SUFFIX=
   local name_tmp
   
   for arg 
   do
         [[ "$arg" == NAME=* ]]      && NAME="${arg#NAME=}"
         [[ "$arg" == INPUT=* ]]     && INPUT="${arg#INPUT=}"
         [[ "$arg" == OUTPUT=* ]]    && OUTPUT="${arg#OUTPUT=}"
         [[ "$arg" == SUFFIX=* ]]    && SUFFIX="${arg#SUFFIX=}"
         [[ "$arg" == RUN=* ]]       && RUN="${arg#RUN=}"
   done

   [[ "$RUN" != "yes" ]]  && return
   
   name_tmp=`echo $NAME | tr [:upper:] [:lower:]`
   if [ -z "$INPUT" ]  ; then  INPUT=$TEST_HOME/$name_tmp$SUFFIX.in  ; fi
   if [ -z "$OUTPUT" ] ; then OUTPUT=$TEST_HOME/$name_tmp$SUFFIX.out ; fi

   run NAME=$NAME INPUT=$INPUT OUTPUT=$OUTPUT EXEC=$EXEC PARALLEL=yes
}


#
#----------------------
run_export () {
#----------------------
#
 
   local NAME=EXPORT
   local EXEC=$DFT_BIN/pw_export.x
   local RUN=yes
   local SUFFIX=
   local INPUT=
   local OUTPUT=
   
   for arg 
   do
         [[ "$arg" == NAME=* ]]      && NAME="${arg#NAME=}"
         [[ "$arg" == INPUT=* ]]     && INPUT="${arg#INPUT=}"
         [[ "$arg" == OUTPUT=* ]]    && OUTPUT="${arg#OUTPUT=}"
         [[ "$arg" == SUFFIX=* ]]    && SUFFIX="${arg#SUFFIX=}"
         [[ "$arg" == RUN=* ]]       && RUN="${arg#RUN=}"
   done

   [[ "$RUN" != "yes" ]]  && return

   if [ -z "$INPUT" ]  ; then  INPUT=$TEST_HOME/pwexport$SUFFIX.in  ; fi
   if [ -z "$OUTPUT" ] ; then OUTPUT=$TEST_HOME/pwexport$SUFFIX.out ; fi
   
   echo "running $NAME calculation..."
   
   if [ "$INPUT_TYPE" = "from_stdin" ] ; then
       $PARA_PREFIX $EXEC $PARA_POSTFIX < $INPUT > $OUTPUT
   elif [ "$INPUT_TYPE" = "from_file" ] ; then
       $PARA_PREFIX $EXEC $PARA_POSTFIX -input $INPUT > $OUTPUT
   else
       echo "$ECHO_T Invalid INPUT_TYPE = $INPUT_TYPE" ; exit 1 
   fi
   #
   if [ $? = 0 ] ; then
      echo "${ECHO_T}done"
   else
      echo "$ECHO_T problems found" ; exit 1
   fi
}


#
#----------------------
run_disentangle () {
#----------------------
#
   local NAME=DISENTANGLE
   local EXEC=$WANT_BIN/disentangle.x
   local RUN=yes
   local INPUT=
   local OUTPUT=
   local SUFFIX=
   
   for arg 
   do
         [[ "$arg" == NAME=* ]]      && NAME="${arg#NAME=}"
         [[ "$arg" == INPUT=* ]]     && INPUT="${arg#INPUT=}"
         [[ "$arg" == OUTPUT=* ]]    && OUTPUT="${arg#OUTPUT=}"
         [[ "$arg" == SUFFIX=* ]]    && SUFFIX="${arg#SUFFIX=}"
         [[ "$arg" == RUN=* ]]       && RUN="${arg#RUN=}"
   done

   [[ "$RUN" != "yes" ]]  && return
   
   if [ -z "$INPUT" ]  ; then  INPUT=$TEST_HOME/want$SUFFIX.in  ; fi
   if [ -z "$OUTPUT" ] ; then OUTPUT=$TEST_HOME/disentangle$SUFFIX.out ; fi

   run NAME=$NAME INPUT=$INPUT OUTPUT=$OUTPUT EXEC=$EXEC 
}


#
#----------------------
run_wannier () {
#----------------------
#
   local NAME=WANNIER
   local EXEC=$WANT_BIN/wannier.x
   local RUN=yes
   local INPUT=
   local OUTPUT=
   local SUFFIX=
   
   for arg 
   do
         [[ "$arg" == NAME=* ]]      && NAME="${arg#NAME=}"
         [[ "$arg" == INPUT=* ]]     && INPUT="${arg#INPUT=}"
         [[ "$arg" == OUTPUT=* ]]    && OUTPUT="${arg#OUTPUT=}"
         [[ "$arg" == SUFFIX=* ]]    && SUFFIX="${arg#SUFFIX=}"
         [[ "$arg" == RUN=* ]]       && RUN="${arg#RUN=}"
   done
   
   [[ "$RUN" != "yes" ]]  && return

   if [ -z "$INPUT" ]  ; then  INPUT=$TEST_HOME/want$SUFFIX.in  ; fi
   if [ -z "$OUTPUT" ] ; then OUTPUT=$TEST_HOME/wannier$SUFFIX.out ; fi

   run NAME=$NAME INPUT=$INPUT OUTPUT=$OUTPUT EXEC=$EXEC 
}


#
#----------------------
run_bands () {
#----------------------
#
   local NAME=BANDS
   local EXEC=$WANT_BIN/bands.x
   local RUN=yes
   local INPUT=
   local OUTPUT=
   local SUFFIX=
   local name_tmp
   
   for arg 
   do
         [[ "$arg" == NAME=* ]]      && NAME="${arg#NAME=}"
         [[ "$arg" == INPUT=* ]]     && INPUT="${arg#INPUT=}"
         [[ "$arg" == OUTPUT=* ]]    && OUTPUT="${arg#OUTPUT=}"
         [[ "$arg" == SUFFIX=* ]]    && SUFFIX="${arg#SUFFIX=}"
         [[ "$arg" == RUN=* ]]       && RUN="${arg#RUN=}"
   done
   
   [[ "$RUN" != "yes" ]]  && return

   name_tmp=`echo $NAME | tr [:upper:] [:lower:]`
   if [ -z "$INPUT" ]  ; then  INPUT=$TEST_HOME/bands$SUFFIX.in  ; fi
   if [ -z "$OUTPUT" ] ; then OUTPUT=$TEST_HOME/bands$SUFFIX.out ; fi

   run NAME=$NAME INPUT=$INPUT OUTPUT=$OUTPUT EXEC=$EXEC 
}


#
#----------------------
run_dos () {
#----------------------
#
   local NAME=DOS
   local EXEC=$WANT_BIN/dos.x
   local RUN=yes
   local INPUT=
   local OUTPUT=
   local SUFFIX=
   local name_tmp
   
   for arg 
   do
         [[ "$arg" == NAME=* ]]      && NAME="${arg#NAME=}"
         [[ "$arg" == INPUT=* ]]     && INPUT="${arg#INPUT=}"
         [[ "$arg" == OUTPUT=* ]]    && OUTPUT="${arg#OUTPUT=}"
         [[ "$arg" == SUFFIX=* ]]    && SUFFIX="${arg#SUFFIX=}"
         [[ "$arg" == RUN=* ]]       && RUN="${arg#RUN=}"
   done
   
   [[ "$RUN" != "yes" ]]  && return

   name_tmp=`echo $NAME | tr [:upper:] [:lower:]`
   if [ -z "$INPUT" ]  ; then  INPUT=$TEST_HOME/dos$SUFFIX.in  ; fi
   if [ -z "$OUTPUT" ] ; then OUTPUT=$TEST_HOME/dos$SUFFIX.out ; fi

   run NAME=$NAME INPUT=$INPUT OUTPUT=$OUTPUT EXEC=$EXEC 
}


#
#----------------------
run_blc2wan () {
#----------------------
#
   local NAME=BLC2WAN
   local EXEC=$WANT_BIN/blc2wan.x
   local RUN=yes
   local INPUT=
   local OUTPUT=
   local SUFFIX=
   local name_tmp
   
   for arg 
   do
         [[ "$arg" == NAME=* ]]      && NAME="${arg#NAME=}"
         [[ "$arg" == INPUT=* ]]     && INPUT="${arg#INPUT=}"
         [[ "$arg" == OUTPUT=* ]]    && OUTPUT="${arg#OUTPUT=}"
         [[ "$arg" == SUFFIX=* ]]    && SUFFIX="${arg#SUFFIX=}"
         [[ "$arg" == RUN=* ]]       && RUN="${arg#RUN=}"
   done
   
   [[ "$RUN" != "yes" ]]  && return

   name_tmp=`echo $NAME | tr [:upper:] [:lower:]`
   if [ -z "$INPUT" ]  ; then  INPUT=$TEST_HOME/blc2wan$SUFFIX.in  ; fi
   if [ -z "$OUTPUT" ] ; then OUTPUT=$TEST_HOME/blc2wan$SUFFIX.out ; fi

   run NAME=$NAME INPUT=$INPUT OUTPUT=$OUTPUT EXEC=$EXEC 
}


#
#
#----------------------
run_plot () {
#----------------------
#
   local NAME=PLOT
   local EXEC=$WANT_BIN/plot.x
   local RUN=yes
   local INPUT=
   local OUTPUT=
   local SUFFIX=
   local name_tmp
   
   for arg 
   do
         [[ "$arg" == NAME=* ]]      && NAME="${arg#NAME=}"
         [[ "$arg" == INPUT=* ]]     && INPUT="${arg#INPUT=}"
         [[ "$arg" == OUTPUT=* ]]    && OUTPUT="${arg#OUTPUT=}"
         [[ "$arg" == SUFFIX=* ]]    && SUFFIX="${arg#SUFFIX=}"
         [[ "$arg" == RUN=* ]]       && RUN="${arg#RUN=}"
   done
   
   [[ "$RUN" != "yes" ]]  && return

   name_tmp=`echo $NAME | tr [:upper:] [:lower:]`
   if [ -z "$INPUT" ]  ; then  INPUT=$TEST_HOME/plot$SUFFIX.in  ; fi
   if [ -z "$OUTPUT" ] ; then OUTPUT=$TEST_HOME/plot$SUFFIX.out ; fi

   run NAME=$NAME INPUT=$INPUT OUTPUT=$OUTPUT EXEC=$EXEC 
}


#
#----------------------
run_conductor () {
#----------------------
#
   local NAME=CONDUCTOR
   local EXEC=$WANT_BIN/conductor.x
   local RUN=yes
   local INPUT=
   local OUTPUT=
   local SUFFIX=
   local name_tmp
   
   for arg 
   do
         [[ "$arg" == NAME=* ]]      && NAME="${arg#NAME=}"
         [[ "$arg" == INPUT=* ]]     && INPUT="${arg#INPUT=}"
         [[ "$arg" == OUTPUT=* ]]    && OUTPUT="${arg#OUTPUT=}"
         [[ "$arg" == SUFFIX=* ]]    && SUFFIX="${arg#SUFFIX=}"
         [[ "$arg" == RUN=* ]]       && RUN="${arg#RUN=}"
   done
   
   [[ "$RUN" != "yes" ]]  && return

   name_tmp=`echo $NAME | tr [:upper:] [:lower:]`
   if [ -z "$INPUT" ]  ; then  INPUT=$TEST_HOME/conductor$SUFFIX.in  ; fi
   if [ -z "$OUTPUT" ] ; then OUTPUT=$TEST_HOME/conductor$SUFFIX.out ; fi

   run NAME=$NAME INPUT=$INPUT OUTPUT=$OUTPUT EXEC=$EXEC 
}

