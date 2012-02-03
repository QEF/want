#
#===============================
# WanT package
#===============================
#
include ../../make.sys

LIBNAME=libetsf_io.a
PACKAGE=etsf_io-1.0.2
TARBALL=../archive/$(PACKAGE).tar.gz

#
# redefinitions
#
INSTALL_DIR=$(TOPDIR)/plugins/etsf_io/tmp
AUXFLAGS= --prefix=$(INSTALL_DIR) \
          --with-moduledir=$(INSTALL_DIR)/include \
          --with-netcdf-module-path="$(TOPDIR)/plugins/netcdf/include" \
          --with-netcdf-ldflags="-L$(TOPDIR)/plugins/netcdf/lib"

#
# MAIN target
#
all: package-ready-stamp

uncompress-stamp:
	gunzip < $(TARBALL) | ../../config/missing --run tar xf -
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
	if test -d ./bin      ; then rm -rf ./bin      ; fi
	if test -d ./lib      ; then rm -rf ./lib      ; fi
	if test -d ./include  ; then rm -rf ./include  ; fi
	#
	mv $(INSTALL_DIR)/lib .
	mv $(INSTALL_DIR)/bin .
	-( mv $(INSTALL_DIR)/include/*/*.mod  $(INSTALL_DIR)/include )
	mv $(INSTALL_DIR)/include .
	#
	- ( cd ../../bin ; ln -sf ../plugins/netcdf/bin/* . )
	touch package-ready-stamp

#
# cleaning
#
clean:
	if test -d $(PACKAGE) ; then ( cd $(PACKAGE);  $(MAKE) clean ) ; fi
	- rm -rf ./bin ./lib ./include package-ready-stamp configure-stamp

wash: clean
	if test -d $(PACKAGE) ; then ( rm -rf $(PACKAGE) ) ; fi 
	- rm uncompress-stamp package-ready-stamp
	- rm -rf $(INSTALL_DIR)
	

