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
	@echo  "     embed              compile embedding executables"
	@echo  "     utility            compile utility executables"
	@echo  "     libwant            compile the want utility library"
	@echo  "     libctools          compile ctools library"
	@echo  "     libextlibs         compile internal versions of std libraries (blas, lapack, ...)"
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
all: build_date  wannier transport embed utility

deps:
	if test -x ./config/makedeps.sh ; then ./config/makedeps.sh ; fi

build_date:
	if test -x ./config/make_build_date.sh ; then ./config/make_build_date.sh ; fi

# 
# LIBS and MODULES
#
libextlibs: 
	if test -d extlibs ; then \
	( cd extlibs ; $(MAKE) ) ; fi

libplugins: 
	if test -d plugins ; then \
	( cd plugins ; $(MAKE) ) ; fi

libctools:
	if test -d src/ctools ; then \
	( cd src/ctools ; $(MAKE) ) ; fi

libwant: libextlibs libplugins
	if test -d src/libs ; then \
	( cd src/libs ; $(MAKE) ) ; fi

wannier: libextlibs libctools libwant libplugins
	if test -d src/wannier ; then \
	( cd src/wannier ; $(MAKE) ) ; fi

transport: libextlibs libctools libwant libplugins wannier
	if test -d src/transport ; then \
	( cd src/transport ; $(MAKE) ) ; fi

embed: libextlibs libctools libwant libplugins transport wannier
	if test -d src/embed ; then \
	( cd src/embed ; $(MAKE) ) ; fi

utility: libextlibs libctools libwant libplugins wannier
	if test -d src/utility ; then \
	( cd src/utility ; $(MAKE) ) ; fi

#
# CLEAN UP
#
clean:
	if test -d extlibs ;       then ( cd extlibs;       $(MAKE) clean ) ; fi
	if test -d plugins ;       then ( cd plugins;       $(MAKE) clean ) ; fi
	if test -d src/ctools ;    then ( cd src/ctools;    $(MAKE) clean ) ; fi
	if test -d src/libs ;      then ( cd src/libs;      $(MAKE) clean ) ; fi
	if test -d src/wannier ;   then ( cd src/wannier;   $(MAKE) clean ) ; fi
	if test -d src/transport ; then ( cd src/transport; $(MAKE) clean ) ; fi
	if test -d src/embed ;     then ( cd src/embed;     $(MAKE) clean ) ; fi
	if test -d src/utility ;   then ( cd src/utility;   $(MAKE) clean ) ; fi
	- /bin/rm  ./include/build_date.h
	- /bin/rm -rf ./bin/*.x ./bin/sumpdos ./bin/iotk ./bin/sax2qexml

clean_test:
	if test -d tests ; then \
	( cd tests ; ./run.sh -r clean ) ; fi

wash_extlibs:
	if test -d extlibs ;   then ( cd extlibs;   $(MAKE) wash ) ; fi
	
wash_plugins:
	if test -d plugins ;   then ( cd plugins;   $(MAKE) wash ) ; fi


wash : wash_extlibs wash_plugins clean
	- /bin/rm -rf make.sys ./config/configure.msg \
		./config/config.log ./config/config.status \
		./config/*.lineno \
		./include/configure.h ./include/fft_defs.h \
	        ./include/c_defs.h ./include/iotk_config.h \
		*/dum1 */dum2 
	- touch make.sys

