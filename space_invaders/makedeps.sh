#!/bin/sh
# compute dependencies for the PWscf directory tree

# run from directory where this script is
cd `echo $0 | sed 's/\(.*\)\/.*/\1/'` # extract pathname
TOPDIR=`pwd`

for DIR in IOTK_lib Modules Libs Main 
do
    # set inter-directory dependencies
    case $DIR in
        IOTK_lib )    DEPENDS=""                        ;;
        Modules )     DEPENDS="../IOTK_lib"             ;;
        Libs | Main ) DEPENDS="../Modules ../IOTK_lib"  ;;
    esac

    # generate dependencies file
    if test -d $TOPDIR/$DIR
    then
        cd $TOPDIR/$DIR
        $TOPDIR/moduledep.sh $DEPENDS > .dependencies
    fi

    # check for missing dependencies
    if grep @ .dependencies
    then
        echo WARNING: modules not found in directory $DIR
    fi
done

