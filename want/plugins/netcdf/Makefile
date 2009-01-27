#
#===============================
# WanT package
#===============================
#
include ../../make.sys

LIBNAME=libnetcdf.a
PACKAGE=netcdf-3.6.3
TARBALL=../archive/$(PACKAGE).tar.gz

#
# MAIN target
#
all: uncompress-stamp

uncompress-stamp:
	../../conf/missing --run tar xf $(TARBALL)
	touch uncompress-stamp

lib: uncompress-stamp
	if test -d $(PACKAGE) ; then ( cd $(PACKAGE);  $(MAKE) ) ; fi

#
# cleaning
#
clean:
	if test -d $(PACKAGE) ; then ( cd $(PACKAGE);  $(MAKE) clean ) ; fi

wash: 
	if test -d $(PACKAGE) ; then ( rm -rf $(PACKAGE) ) ; fi 
	- rm uncompress-stamp $(LIBNAME) *.mod
	

