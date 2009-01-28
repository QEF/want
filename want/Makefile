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
	@echo  "     libplugins         compile plugins"
	@echo 
	@echo  "     all                all the above executables "
	@echo  "     deps               update fortran90 dependencies "
	@echo  "     clean              remove executables and objects"
	@echo  "     clean_test         clean up the test examples"
	@echo  "     wash               revert distribution to the original status"
	@echo 

#
# MAIN target
#
all: wannier transport utility

deps:
	if test -x ./conf/makedeps.sh ; then ./conf/makedeps.sh ; fi

# 
# LIBS and MODULES
#
libplugins: 
	if test -d plugins ; then \
	( cd plugins ; $(MAKE) ) ; fi

libiotk:
	if test -d iotk ; then \
	( cd iotk ; $(MAKE) ) ; fi

libctools:
	if test -d ctools ; then \
	( cd ctools ; $(MAKE) ) ; fi

libwant: libiotk libplugins
	if test -d libs ; then \
	( cd libs ; $(MAKE) ) ; fi

wannier: libiotk libctools libwant libplugins
	if test -d wannier ; then \
	( cd wannier ; $(MAKE) ) ; fi

transport: libiotk libctools libwant libplugins
	if test -d transport ; then \
	( cd transport ; $(MAKE) ) ; fi

utility: libiotk libctools libwant libplugins
	if test -d utility ; then \
	( cd utility ; $(MAKE) ) ; fi

#
# CLEAN UP
#
clean:
	if test -d plugins ;   then ( cd plugins;   $(MAKE) clean ) ; fi
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

wash_plugins:
	if test -d plugins ;   then ( cd plugins;   $(MAKE) wash ) ; fi
	

wash : wash_plugins clean clean_test
	- /bin/rm -rf make.sys ./conf/configure.msg \
		./conf/config.log ./conf/config.status \
		./conf/*.lineno \
		./include/configure.h ./include/fft_defs.h \
	        ./include/ctools.h ./include/iotk_config.h \
		*/dum1 */dum2 
	- touch make.sys

