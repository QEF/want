#!/bin/sh
# compute dependencies for the PWscf directory tree

# run from directory where this script is
cd `echo $0 | sed 's/\(.*\)\/.*/\1/'` # extract pathname
TOPDIR=`pwd`

for DIR in Modules Libs Main
do
    # set inter-directory dependencies
    case $DIR in
        Modules )                         DEPENDS=""                       ;;
        Libs | Main )                      DEPENDS="../Modules"             ;;
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

