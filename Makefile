MAKE=make

# this is the main makefile
all:
	cd modules; $(MAKE);
	cd lib; $(MAKE);
	cd main; $(MAKE);

clean:
	cd modules; $(MAKE) clean;
	cd lib; $(MAKE) clean;
	cd main; $(MAKE) clean;
