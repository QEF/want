MAKE=make

# this is the main makefile
all: main

mod:
	cd Modules; $(MAKE);

lib: mod
	cd Libs; $(MAKE);

main: lib mod
	cd Main; $(MAKE);

transport: lib mod
	cd Transport; $(MAKE);

clean:
	cd Modules; $(MAKE) clean;
	cd Libs; $(MAKE) clean;
	cd Main; $(MAKE) clean;
	cd Transport; $(MAKE) clean;
