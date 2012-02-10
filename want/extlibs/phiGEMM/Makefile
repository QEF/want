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
		--enable-parallel \
	        FC="$(F90)" \
	        CC="mpicc" \
	        LD="mpicc" \
	        F77=$(F77) \
	        FFLAGS="$(FFLAGS)" \
	        FCFLAGS="$(F90FLAGS)" ; fi; )\
	fi
	touch configure-stamp

	        #CFLAGS="-O3 --compiler-options '-fPIC'" \
	        #CC="$(NCC)" \
	        #CFLAGS="$(NVCCFLAGS)" \
	
package-ready-stamp: uncompress-stamp configure-stamp
	if test -d $(PACKAGE) ; then \
	( cd $(PACKAGE);  $(MAKE) all ) ; fi
	#
	if test -d ./lib      ; then rm -rf ./lib      ; fi    ; mkdir ./lib ;
	if test -d ./include  ; then rm -rf ./include  ; fi    ; mkdir ./include ;
	#mv $(PACKAGE)/lib/lib*  ./lib ;
	cp $(PACKAGE)/lib/lib*  ./lib ;
	cp $(PACKAGE)/include/* ./include ;
	#if test -d ./bin      ; then rm -rf ./bin      ; fi    ; mkdir ./bin ;
	#mv $(PACKAGE)/src/*.x ./bin ;
	#
	touch package-ready-stamp

#
# cleaning
#
clean:
	#if test -d $(PACKAGE) ; then ( rm -rf $(PACKAGE) ) ; fi
	if test -d $(PACKAGE) ; then ( cd $(PACKAGE); \
		touch include/fake.mod; \
		touch testing/fake.o ; \
		$(MAKE) clean ) ; fi
	- rm -rf ./lib ./include package-ready-stamp configure-stamp 

wash: clean
	if test -d $(PACKAGE) ; then ( rm -rf $(PACKAGE) ) ; fi 
	- rm uncompress-stamp
	- rm -rf $(INSTALL_DIR)
	

