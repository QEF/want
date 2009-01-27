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
	

