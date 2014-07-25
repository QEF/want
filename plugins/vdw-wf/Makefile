#
#===============================
# WanT package
#===============================
#
include ../../make.sys
include ../archive/package.list

LIBNAME=libvdW-WF.a
PACKAGE=$(pkgname_vdw_wf)
TARBALL=$(tarball_vdw_wf)

#
# redefinitions
#
INSTALL_DIR=$(TOPDIR)/plugins/vdw-wf/tmp
AUXFLAGS="--prefix=$(INSTALL_DIR)"


#
# MAIN target
#
all: package-ready-stamp

uncompress-stamp:
	( cd ../archive ; if ! test -e $(TARBALL) ; then make $(TARBALL) ; fi )
	gunzip < ../archive/$(TARBALL) | ../../config/missing --run tar xf -
	test -d $(INSTALL_DIR) || mkdir $(INSTALL_DIR)
	touch uncompress-stamp

package-ready-stamp: uncompress-stamp
	if test -d $(PACKAGE) ; then \
	( cd $(PACKAGE); $(F77) $(FFLAGS) -I. -o vdW-WF.x vdW-WF.f ) ; fi
	#
	if test -d ./bin      ; then rm -rf ./bin      ; fi
	if test -d ./lib      ; then rm -rf ./lib      ; fi
	if test -d ./include  ; then rm -rf ./include  ; fi
	#
	mkdir ./bin
	cp $(PACKAGE)/*.x  ./bin/
	#
	- ( cd ../../bin ; ln -sf ../plugins/vdw-wf/bin/vdW-WF.x . )
	#	
	touch package-ready-stamp
	

#
# cleaning
#
clean:
	if test -d $(PACKAGE) ; then ( cd $(PACKAGE);  rm -rf *.x *.mod ) ; fi
	- rm -rf ./bin ./lib ./include  package-ready-stamp configure-stamp

clean_all: clean
	if test -d $(PACKAGE) ; then ( rm -rf $(PACKAGE) ) ; fi 
	- rm uncompress-stamp package-ready-stamp
	- rm -rf $(INSTALL_DIR)
	

