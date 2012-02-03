#
#===============================
# WanT package
#===============================
#
include ../../make.sys

LIBNAME=libphiGEMM.a
PACKAGE=phiGEMM
TARBALL=../archive/$(PACKAGE)-v1.6.tar.gz

#
# redefinitions
#
INSTALL_DIR=$(TOPDIR)/extlibs/phiGEMM/tmp

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
		if test -e ./make.inc ; then rm ./make.inc ; fi; \
		if test -x ./configure ; then ./configure  \
		--prefix=$(INSTALL_DIR) \
	        FC=$(F90) \
	        F77=$(F77) \
	        FFLAGS="$(FFLAGS)" \
	        FCFLAGS="$(F90FLAGS)" ; fi; )\
	fi
	touch configure-stamp
	
package-ready-stamp: uncompress-stamp configure-stamp
	if test -d $(PACKAGE) ; then \
	( cd $(PACKAGE);  $(MAKE) all ) ; fi
	#
	if test -d ./lib      ; then rm -rf ./lib      ; fi    ; mkdir ./lib ;
	if test -d ./include  ; then rm -rf ./include  ; fi    ; mkdir ./include ;
	mv $(PACKAGE)/lib/lib*  ./lib ;
	mv $(PACKAGE)/include/* ./include ;
	#if test -d ./bin      ; then rm -rf ./bin      ; fi    ; mkdir ./bin ;
	#mv $(PACKAGE)/src/*.x ./bin ;
	#
	touch package-ready-stamp

#
# cleaning
#
clean:
	if test -d $(PACKAGE) ; then ( cd $(PACKAGE);  $(MAKE) clean ) ; fi
	- rm -rf ./lib package-ready-stamp configure-stamp 

wash: clean
	if test -d $(PACKAGE) ; then ( rm -rf $(PACKAGE) ) ; fi 
	- rm uncompress-stamp package-ready-stamp
	- rm ../install/make_lapack.inc
	- rm -rf $(INSTALL_DIR)
	

