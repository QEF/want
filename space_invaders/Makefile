#
#===============================
# WanT package
#===============================
#
MAKE=make

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
window: lib mod
	cd Main; $(MAKE) window.x;
disentangle: lib mod
	cd Main; $(MAKE) disentangle.x;
wannier: lib mod
	cd Main; $(MAKE) wannier.x;
hamiltonian: lib mod
	cd Main; $(MAKE) hamiltonian.x;
plot: lib mod
	cd Main; $(MAKE) plot.x;


tran: lib mod
	cd Transport; $(MAKE);
bulk: lib mod
	cd Transport; $(MAKE) bulk.x;
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

veryclean : clean
	- /bin/rm -rf make.rules make.sys */.dependencies \
		config.log config.status */dum1 */dum2 bin/*.x \
		intel.pcl */intel.pcl

