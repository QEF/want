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
# redefinitions
#
FC=$(F90) 
FCFLAGS=$(F90FLAGS) 
INSTALL_DIR=$(HOMEDIR)/plugins/netcdf/tmp
AUXFLAGS="--prefix=$(INSTALL_DIR)"


#
# MAIN target
#
all: lib

uncompress-stamp:
	../../conf/missing --run tar xf $(TARBALL)
	test -d $(INSTALL_DIR) || mkdir $(INSTALL_DIR)
	touch uncompress-stamp

configure-stamp: uncompress-stamp
	if test -d $(PACKAGE) ; then \
	   ( cd $(PACKAGE);  ./configure $(AUXFLAGS) ) ; fi
	touch configure-stamp

lib: uncompress-stamp configure-stamp
	if test -d $(PACKAGE) ; then \
	( cd $(PACKAGE);  $(MAKE) ; $(MAKE) install ) ; fi
	#
	cp $(PACKAGE)/f90/*.mod .
	mv $(INSTALL_DIR)/lib/*.a .
	mv $(INSTALL_DIR)/bin/* .
	mv $(INSTALL_DIR)/include/* .
	

#
# cleaning
#
clean:
	if test -d $(PACKAGE) ; then ( cd $(PACKAGE);  $(MAKE) clean ) ; fi
	- rm *.mod *.a *.inc *.h *.hh ncdump ncgen

wash: 
	if test -d $(PACKAGE) ; then ( rm -rf $(PACKAGE) ) ; fi 
	- rm uncompress-stamp configure-stamp 
	- rm -rf $(INSTALL_DIR)
	

