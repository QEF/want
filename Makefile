# this is the main makefile

all:
	cd jlm/_source/; ./Mklibpw.linux_ifc;
	cd band; make;
	cd interface; make;
	cd lib; make;
	cd libutils; make;
	cd space; make;
	cd main; make;

clean:
	cd band; make clean;
	cd interface; make clean;
	cd lib; make clean;
	cd libutils; make clean;
	cd space; make clean;
	cd main; make clean;
