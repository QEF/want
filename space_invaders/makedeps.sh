#!/bin/sh
# compute dependencies for the WanT directory tree

# run from directory where this script is
cd `echo $0 | sed 's/\(.*\)\/.*/\1/'` # extract pathname
TOPDIR=`pwd`

for DIR in IOTK_lib Modules Libs Main Transport
do
    # set inter-directory dependencies
    case $DIR in
        IOTK_lib ) DEPENDS="../Include"                        ;;
        Modules )  DEPENDS="../Include ../IOTK_lib"            ;;
        Libs | Main | Transport )
                   DEPENDS="../Include ../IOTK_lib ../Modules" ;;
    esac

    # generate dependencies file
    if test -d $TOPDIR/$DIR
    then
        cd $TOPDIR/$DIR
        $TOPDIR/moduledep.sh $DEPENDS > make.depend
        $TOPDIR/includedep.sh $DEPENDS >> make.depend
    fi

    # handle special cases
    if test "$DIR" = "Libs"
    then
        mv make.depend make.depend.tmp
        sed 's/@fftw.c@/fftw.c/' make.depend.tmp > make.depend
    fi

    rm -f make.depend.tmp

    # check for missing dependencies
    if grep @ make.depend
    then
        notfound=1
        echo WARNING: dependencies not found in directory $DIR
    else
        echo directory $DIR : ok
    fi
done
if test "$notfound" = ""
then
    echo all dependencies updated successfully
fi
