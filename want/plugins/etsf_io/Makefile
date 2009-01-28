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
# redefinitions
#
FC=$(F90)
FCFLAGS=$(F90FLAGS)
INSTALL_DIR=$(HOMEDIR)/plugins/etsf_io/tmp
AUXFLAGS= --prefix=$(INSTALL_DIR) \
          --with-netcdf=$(NETCDF_LIBS) \
          --with-netcdf-module-path=$(NETCDF_INCLUDE) \
          --with-netcdf-ldflags="-L$(HOMEDIR)/plugins/netcdf -lnetcdf" 

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
	mv $(INSTALL_DIR)/bin/* .
	mv $(INSTALL_DIR)/lib/* .
	mv $(INSTALL_DIR)/include/*/* .

#
# cleaning
#
clean:
	if test -d $(PACKAGE) ; then ( cd $(PACKAGE);  $(MAKE) clean ) ; fi
	- rm *.a *.mod *.h

wash:
	if test -d $(PACKAGE) ; then ( rm -rf $(PACKAGE) ) ; fi 
	- rm uncompress-stamp configure-stamp
	- rm -rf $(INSTALL_DIR)
	

