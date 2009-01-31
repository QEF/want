#! /bin/sh 
# compute dependencies for the WanT directory tree

# run from directory where this script is
cd `echo $0 | sed 's/\(.*\)\/.*/\1/'` # extract pathname
# come back to WanT HOME
cd ..  
#
TOPDIR=`pwd`
BINDIR=$TOPDIR/conf

SPECIAL_MODULES="etsf_io etsf_io_tools mkl_dfti.f90"

for DIR in iotk libs wannier transport utility
do
    # set inter-directory dependencies
    case $DIR in
        iotk )      DEPENDS="../include"                 ;;
        libs )      DEPENDS="../include ../iotk"         ;;
        wannier )   DEPENDS="../include ../iotk ../libs" ;;
        transport ) DEPENDS="../include ../iotk ../libs ../wannier " ;;
        utility )   DEPENDS="../include ../iotk ../libs ../wannier " ;;
    esac

    # generate dependencies file
    if test -d $TOPDIR/$DIR
    then
        cd $TOPDIR/$DIR
        $BINDIR/moduledep.sh $DEPENDS > make.depend
        $BINDIR/includedep.sh $DEPENDS >> make.depend
    fi

    # handle special cases
    if test "$DIR" = "libs"
    then
        mv make.depend make.depend.tmp
        sed 's/@fftw.c@/fftw.c/' make.depend.tmp > make.depend
    fi

    # eliminate dependencies on special modules
    for module in $SPECIAL_MODULES
    do
        mv make.depend make.depend.tmp
        grep -v "@$module@" make.depend.tmp > make.depend
    done

    test -e make.depend.tmp && rm make.depend.tmp

    # check for missing dependencies
    if grep @ make.depend
    then
        notfound=1
        echo WARNING: dependencies not found in directory $DIR
    else
        echo directory $DIR : ok
    fi
    #
    # eliminate missing deps to make the script working
    mv make.depend make.depend.tmp
    awk '{
           if ( match($0, "@") ) { 
               print "#", $0 
           } else {
               print 
           }
         }' make.depend.tmp > make.depend
    #
    test -e make.depend.tmp && rm make.depend.tmp
    #
done
#
if test "$notfound" = ""
then
    echo all dependencies updated successfully
fi
