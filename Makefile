# this is the main makefile

all:
	cd lib; make;
	cd main; make;

clean:
	cd band; make clean;
	cd interface; make clean;
	cd lib; make clean;
	cd libutils; make clean;
	cd space; make clean;
	cd main; make clean;
