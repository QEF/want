MAKE=make

# this is the main makefile
all: main

mod:
	cd Modules; $(MAKE);

lib: mod
	cd Libs; $(MAKE);

main: lib mod
	cd Main; $(MAKE);

clean:
	cd Modules; $(MAKE) clean;
	cd Libs; $(MAKE) clean;
	cd Main; $(MAKE) clean;
