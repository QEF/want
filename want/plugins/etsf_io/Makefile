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
INSTALL_DIR=$(TOPDIR)/plugins/etsf_io/tmp
AUXFLAGS= --prefix=$(INSTALL_DIR) \
          --with-moduledir=$(INSTALL_DIR)/include \
          --with-netcdf-module-path="$(NETCDF_INCLUDE)" \
          --with-netcdf-ldflags="-L$(TOPDIR)/plugins/netcdf"

#
# MAIN target
#
all: package-ready-stamp

uncompress-stamp:
	../../conf/missing --run tar xf $(TARBALL)
	test -d $(INSTALL_DIR) || mkdir $(INSTALL_DIR)
	touch uncompress-stamp

configure-stamp: uncompress-stamp
	if test -d $(PACKAGE) ; then ( cd $(PACKAGE);  \
	        ./configure $(AUXFLAGS)  \
	        FC=$(F90) \
	        F77=$(F77) \
	        FFLAGS="$(FFLAGS)" \
	        FCFLAGS="$(F90FLAGS)" \
	        INSTALL_DIR="$(INSTALL_DIR)" ); \
	fi
	touch configure-stamp
	
package-ready-stamp: uncompress-stamp configure-stamp
	if test -d $(PACKAGE) ; then \
	( cd $(PACKAGE);  $(MAKE) ; $(MAKE) install ) ; fi
	#
	test -d $(INSTALL_DIR) && mv $(INSTALL_DIR)/bin/* .
	test -d $(INSTALL_DIR) && mv $(INSTALL_DIR)/lib/* .
	test -d $(INSTALL_DIR) && mv $(INSTALL_DIR)/include/*/*.mod .
	#
	touch package-ready-stamp

#
# cleaning
#
clean:
	if test -d $(PACKAGE) ; then ( cd $(PACKAGE);  $(MAKE) clean ) ; fi
	- rm *.a *.mod *.h package-ready-stamp

wash: clean
	if test -d $(PACKAGE) ; then ( rm -rf $(PACKAGE) ) ; fi 
	- rm uncompress-stamp configure-stamp package-ready-stamp
	- rm -rf $(INSTALL_DIR)
	

