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

    # special case: Libs/fft_stick.c includes Libs/fftw.c
    if test "$DIR" = "Libs"
    then
        mv make.depend make.depend.tmp
        sed 's/@fftw.c@/fftw.c/' make.depend.tmp > make.depend
        rm -f make.depend.tmp
    fi

    # check for missing dependencies
    if grep @ make.depend
    then
        echo WARNING: modules not found in directory $DIR
    fi
done
