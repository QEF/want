MAKE=make

# this is the main makefile
all: main

mod:
	cd Modules; $(MAKE);

lib: mod
	cd Libs; $(MAKE);

main: lib mod
	cd Main; $(MAKE);

tran: lib mod
	cd Transport; $(MAKE);

clean:
	cd Modules; $(MAKE) clean;
	cd Libs; $(MAKE) clean;
	cd Main; $(MAKE) clean;
	cd Transport; $(MAKE) clean;

veryclean : clean
	- /bin/rm -rf make.rules make.sys */.dependencies \
		config.log config.status */dum1 */dum2 bin/*.x \
		intel.pcl */intel.pcl
	- if test -d GUI ; then ( cd GUI; make veryclean ) ; fi
