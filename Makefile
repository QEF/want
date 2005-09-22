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
	@echo  "     disentangle        WanT step 1: subspace determination"
	@echo  "     wannier            WanT step 2: localization procedure"
	@echo  "     bands              interpolate band structure using WFs"
	@echo  "     plot               utility for WF plotting"
	@echo  "     blc2wan            converts operators to WF basis"
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
iotk:
	cd IOTK_lib; $(MAKE);

mod: iotk
	cd Modules; $(MAKE);

lib: iotk mod
	cd Libs; $(MAKE);

main: iotk mod lib 
	cd Main; $(MAKE);

#
# EXPLICIT reference to EXECUTABLES
#
disentangle: lib mod
	cd Main; $(MAKE) disentangle.x;
wannier: lib mod
	cd Main; $(MAKE) wannier.x;
bands: lib mod
	cd Main; $(MAKE) bands.x;
plot: lib mod
	cd Main; $(MAKE) plot.x;
blc2wan: lib mod
	cd Main; $(MAKE) blc2wan.x;


tran: lib mod
	cd Transport; $(MAKE);
conductor: lib mod
	cd Transport; $(MAKE) conductor.x;

#
# CLEAN UP
#
clean:
	cd IOTK_lib; $(MAKE) clean;
	cd Modules; $(MAKE) clean;
	cd Libs; $(MAKE) clean;
	cd Main; $(MAKE) clean;
	cd Transport; $(MAKE) clean;

clean_test:
	cd Tests ; $(MAKE) clean;

veryclean : clean
	- /bin/rm -rf make.rules make.sys */.dependencies \
		config.log config.status */dum1 */dum2 bin/*.x \
		intel.pcl */intel.pcl

