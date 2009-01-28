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
INSTALL_DIR=$(TOPDIR)/plugins/netcdf/tmp
AUXFLAGS="--prefix=$(INSTALL_DIR)"


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
	      ./configure $(AUXFLAGS) \
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
	cp $(PACKAGE)/f90/*.mod .
	mv $(INSTALL_DIR)/lib/*.a .
	mv $(INSTALL_DIR)/lib/*.la .
	mv $(INSTALL_DIR)/bin/* .
	mv $(INSTALL_DIR)/include/* .
	#	
	touch package-ready-stamp
	

#
# cleaning
#
clean:
	if test -d $(PACKAGE) ; then ( cd $(PACKAGE);  $(MAKE) clean ) ; fi
	- rm *.mod *.a *.la *.inc *.h *.hh ncdump ncgen package-ready-stamp

wash: clean
	if test -d $(PACKAGE) ; then ( rm -rf $(PACKAGE) ) ; fi 
	- rm uncompress-stamp configure-stamp package-ready-stamp
	- rm -rf $(INSTALL_DIR)
	

