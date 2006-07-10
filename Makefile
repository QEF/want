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

libwant: libiotk
	if test -d libs ; then \
	( cd libs ; $(MAKE) ) ; fi

wannier: libiotk libwant
	if test -d wannier ; then \
	( cd wannier ; $(MAKE) ) ; fi

transport: libiotk libwant
	if test -d transport ; then \
	( cd transport ; $(MAKE) ) ; fi

utility: libiotk libwant
	if test -d utility ; then \
	( cd utility ; $(MAKE) ) ; fi

#
# CLEAN UP
#
clean:
	cd iotk;      $(MAKE) clean;
	cd libs;      $(MAKE) clean;
	cd wannier;   $(MAKE) clean;
	cd transport; $(MAKE) clean;
	cd utility;   $(MAKE) clean;
	- /bin/rm -rf bin/*.x

clean_test:
	if test -d tests ; then \
	( cd tests ; ./run.sh -r clean ) ; fi

wash : clean clean_test
	- /bin/rm -rf make.sys conf/configure.msg */make.depend \
		conf/config.log conf/config.status \
		conf/configure.h include/configure.h \
		*/dum1 */dum2 bin/*.x 

