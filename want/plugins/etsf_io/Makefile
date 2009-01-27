#
#===============================
# WanT package
#===============================
#
include ../../make.sys

LIBNAME=libetsf_io.a
PACKAGE=etsf_io-1.0.0
TARBALL=../archive/$(PACKAGE).tar.gz

#
# MAIN target
#
all: uncompress-stamp configure-stamp

uncompress-stamp:
	../../conf/missing --run tar xf $(TARBALL)
	touch uncompress-stamp

configure-stamp: uncompress-stamp
	FC=$(F90); \
	FCFLAGS=$(F90FLAGS); 
	if test -d $(PACKAGE) ; then \
	   ( cd $(PACKAGE);  ./configure ) ; fi
	touch configure-stamp
	
lib: uncompress-stamp configure-stamp
	if test -d $(PACKAGE) ; then ( cd $(PACKAGE);  $(MAKE) ) ; fi

#
# cleaning
#
clean:
	if test -d $(PACKAGE) ; then ( cd $(PACKAGE);  $(MAKE) clean ) ; fi

wash:
	if test -d $(PACKAGE) ; then ( rm -rf $(PACKAGE) ) ; fi 
	- rm uncompress-stamp configure-stamp $(LIBNAME) *.mod
	

