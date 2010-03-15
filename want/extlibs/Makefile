#
#===============================
# WanT package
#===============================
#
include ../make.sys


#
# MAIN target
#
all: libblas liblapack libiotk

#
# blas
#
libblas:  libblas_$(BLAS_LIBS_SWITCH)

libblas_internal:
	 if test -d blas ;    then ( cd blas;    $(MAKE) ) ; fi

libblas_external:
libblas_:


#
# lapack
#
liblapack:  liblapack_$(LAPACK_LIBS_SWITCH)

liblapack_internal:
	 if test -d lapack ;  then ( cd lapack;  $(MAKE) ) ; fi

liblapack_external:
liblapack_:


#
# iotk
#
libiotk:  libiotk_$(IOTK_LIBS_SWITCH)

libiotk_internal:
	 if test -d iotk ;  then ( cd iotk;  $(MAKE) ) ; fi

libiotk_external:
libiotk_:


#
# cleaning
#
clean wash:
	 if test -d blas ;    then ( cd blas;    $(MAKE) $@ ) ; fi
	 if test -d lapack ;  then ( cd lapack;  $(MAKE) $@ ) ; fi
	 if test -d iotk ;    then ( cd iotk;    $(MAKE) $@ ) ; fi

