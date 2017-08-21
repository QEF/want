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
	@echo  "     odd                compile odd executables"
	@echo  "     tools              compile utilities"
	@echo  "     libwant            compile the want internal library"
	@echo  "     libclib            compile the intenral c-library"
	@echo  "     libextlibs         compile internal versions of std libraries (blas, lapack, ...)"
	@echo  "     vdW-WF             compile the vdW-WF plugin"
	@echo 
	@echo  "     all                all the above executables "
	@echo  "     deps               update fortran90 dependencies "
	@echo  "     clean              remove executables and objects"
	@echo  "     clean_test         clean up the test examples"
	@echo  "     clean_all          revert distribution to the original status"
	@echo 

#
# MAIN target
#
all: build_date  wannier transport embed tools

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

vdW-WF: 
	if test -d plugins ; then \
	( cd plugins ; $(MAKE) $@ ) ; fi

libclib:
	if test -d src/clib ; then \
	( cd src/clib ; $(MAKE) ) ; fi

libfftqe:
	if test -d src/fftqe ; then \
	( cd src/fftqe ; $(MAKE) ) ; fi

libwant: libextlibs
	if test -d src/baselib ; then \
	( cd src/baselib ; $(MAKE) ) ; fi

wannier: libextlibs libclib libwant
	if test -d src/wannier ; then \
	( cd src/wannier ; $(MAKE) ) ; fi

transport: libextlibs libclib libwant wannier
	if test -d src/transport ; then \
	( cd src/transport ; $(MAKE) ) ; fi

embed: libextlibs libclib libwant transport wannier
	if test -d src/embed ; then \
	( cd src/embed ; $(MAKE) ) ; fi

odd:   libextlibs libclib libfftqe libwant wannier
	if test -d src/odd ; then \
	( cd src/odd ; $(MAKE) ) ; fi

tools: libextlibs libclib libwant wannier
	if test -d src/tools ; then \
	( cd src/tools ; $(MAKE) ) ; fi

#
# CLEAN UP
#
clean:
	if test -d extlibs ;       then ( cd extlibs;       $(MAKE) clean ) ; fi
	if test -d plugins ;       then ( cd plugins;       $(MAKE) clean ) ; fi
	if test -d src/clib ;      then ( cd src/clib;      $(MAKE) clean ) ; fi
	if test -d src/fftqe ;     then ( cd src/fftqe;     $(MAKE) clean ) ; fi
	if test -d src/baselib ;   then ( cd src/baselib;   $(MAKE) clean ) ; fi
	if test -d src/wannier ;   then ( cd src/wannier;   $(MAKE) clean ) ; fi
	if test -d src/transport ; then ( cd src/transport; $(MAKE) clean ) ; fi
	if test -d src/embed ;     then ( cd src/embed;     $(MAKE) clean ) ; fi
	if test -d src/odd ;       then ( cd src/odd;       $(MAKE) clean ) ; fi
	if test -d src/tools ;     then ( cd src/tools;     $(MAKE) clean ) ; fi
	- /bin/rm  ./include/build_date.h
	- /bin/rm -rf ./bin/*.x ./bin/sumpdos ./bin/iotk ./bin/sax2qexml

clean_test:
	if test -d tests ; then \
	( cd tests ; ./run.sh -r clean ) ; fi

clean_all_extlibs:
	if test -d extlibs ;   then ( cd extlibs;   $(MAKE) clean_all ) ; fi
	
clean_all_plugins:
	if test -d plugins ;   then ( cd plugins;   $(MAKE) clean_all ) ; fi


distclean: clean_all
clean_all : clean_all_extlibs clean_all_plugins clean
	- /bin/rm -rf make.sys ./config/configure.msg \
		./config.log config.status \
		./config/*.lineno \
		./include/configure.h ./include/fftqe_defs.h \
	        ./include/c_defs.h ./include/iotk_config.h \
		*/dum1 */dum2 
	- touch make.sys

