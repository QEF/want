#
#===============================
# WanT package
#===============================
#
MAKE=make

#
# manual
#
default: 
	@echo 
	@echo  "  *** Wannier-Transport (WanT) package *** "
	@echo 
	@echo  "  to configure, type:  ./configure <options>"
	@echo  "  to compile,   type:  make <target>"
	@echo 
	@echo  "  Possible <target>'s are: "
	@echo  "     libiotk            compile iotk library"
	@echo  "     disentangle        WanT step 1: subspace determination"
	@echo  "     wannier            WanT step 2: localization procedure"
	@echo  "     bands              interpolate band structure using WFs"
	@echo  "     plot               utility for WF plotting"
	@echo  "     blc2wan            converts operators to WF basis"
	@echo  "     conductor          compile transport code"
	@echo  "     tran               as before"
	@echo 
	@echo  "     all                all the above executables "
	@echo  "     clean              remove executables and objects"
	@echo  "     veryclean          revert distribution to the original status"
	@echo 

#
# MAIN target
#
all: main tran

# 
# LIBS and MODULES
#
libiotk:
	if test -d iotk ; then \
	( cd Main ; $(MAKE) ) ; fi
mod: libiotk
	if test -d Modules ; then \
	( cd Modules ; $(MAKE) ) ; fi
lib: libiotk mod
	if test -d Libs ; then \
	( cd Libs ; $(MAKE) ) ; fi
main: libiotk mod lib 
	if test -d Main ; then \
	( cd Main ; $(MAKE) ) ; fi

#
# EXPLICIT reference to EXECUTABLES
#
disentangle: lib mod
	if test -d Main ; then \
	( cd Main ; $(MAKE) disentangle.x ) ; fi
wannier: lib mod
	if test -d Main ; then \
	( cd Main ; $(MAKE) wannier.x ) ; fi
bands: lib mod
	if test -d Main ; then \
	( cd Main ; $(MAKE) bands.x ) ; fi
plot: lib mod
	if test -d Main ; then \
	( cd Main ; $(MAKE) plot.x ) ; fi
blc2wan: lib mod
	if test -d Main ; then \
	( cd Main ; $(MAKE) blc2wan.x ) ; fi
conductor: tran
tran: lib mod
	if test -d Transport ; then \
	( cd Transport ; $(MAKE) conductor.x ) ; fi


#
# CLEAN UP
#
clean:
	cd iotk;      $(MAKE) clean;
	cd Modules;   $(MAKE) clean;
	cd Libs;      $(MAKE) clean;
	cd Main;      $(MAKE) clean;
	cd Transport; $(MAKE) clean;

clean_test:
	if test -d Tests ; then \
	( cd Tests ; $(MAKE) clean ) ; fi

veryclean : clean
	- /bin/rm -rf make.rules make.sys */.dependencies \
		config.log config.status */dum1 */dum2 bin/*.x \
		intel.pcl */intel.pcl

