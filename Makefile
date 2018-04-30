#
#===============================
# WanT package
#===============================
#
MAKE=make

#
# manual
#
default: 
	@echo 
	@echo  "  *** Wannier-Transport (WanT) package *** "
	@echo 
	@echo  "  to configure, type:  ./configure [<options>]"
	@echo  "  to compile,   type:  make <target>"
	@echo 
	@echo  "  Possible <target>'s are: "
	@echo  "     wannier            compile Wannier function suite"
	@echo  "     transport          compile transport executables"
	@echo  "     utility            compile utility executables"
	@echo  "     libwant            compile the want utility library"
	@echo  "     libiotk            compile iotk library"
	@echo  "     libctools          compile ctools library"
	@echo 
	@echo  "     all                all the above executables "
	@echo  "     clean              remove executables and objects"
	@echo  "     clean_test         clean up the test examples"
	@echo  "     wash               revert distribution to the original status"
	@echo 

#
# MAIN target
#
all: wannier transport utility

# 
# LIBS and MODULES
#
libiotk:
	if test -d iotk ; then \
	( cd iotk ; $(MAKE) ) ; fi

libctools:
	if test -d ctools ; then \
	( cd ctools ; $(MAKE) ) ; fi

libwant: libiotk
	if test -d libs ; then \
	( cd libs ; $(MAKE) ) ; fi

wannier: libiotk libctools libwant
	if test -d wannier ; then \
	( cd wannier ; $(MAKE) ) ; fi

transport: libiotk libctools libwant
	if test -d transport ; then \
	( cd transport ; $(MAKE) ) ; fi

utility: libiotk libctools libwant
	if test -d utility ; then \
	( cd utility ; $(MAKE) ) ; fi

#
# CLEAN UP
#
clean:
	if test -d iotk ;      then ( cd iotk;      $(MAKE) clean ) ; fi
	if test -d ctools ;    then ( cd ctools;    $(MAKE) clean ) ; fi
	if test -d libs ;      then ( cd libs;      $(MAKE) clean ) ; fi
	if test -d wannier ;   then ( cd wannier;   $(MAKE) clean ) ; fi
	if test -d transport ; then ( cd transport; $(MAKE) clean ) ; fi
	if test -d utility ;   then ( cd utility;   $(MAKE) clean ) ; fi
	- /bin/rm -rf bin/*.x

clean_test:
	if test -d tests ; then \
	( cd tests ; ./run.sh -r clean ) ; fi

wash : clean clean_test
	- /bin/rm -rf make.sys ./conf/configure.msg \
		./conf/config.log ./conf/config.status \
		./include/configure.h ./include/fft_defs.h \
	        ./include/ctools.h ./include/iotk_config.h \
		*/dum1 */dum2 

