MAKE=make

#
# manage tests and examples
#

info:
	@echo " "
	@echo " Possible targets are: "
	@echo "     all           perform all tests"
	@echo "     clean         clean up" 
	@echo "     <test>        perform selected test"
	@echo " "
	@echo " List of available TESTs: "
	@echo "     test1         Silicon WFs"
	@echo "     test2         Gold chain (bulk conductance)"
	@echo "     test3         Gold chain (general geometry)"
	@echo " "

all: test1 test2 test3


test1:
	cd Test1 ; $(MAKE) run
test2:
	cd Test2 ; $(MAKE) run
test3:
	cd Test3 ; $(MAKE) run

clean:
	cd Test1; $(MAKE) clean
	cd Test2; $(MAKE) clean
	cd Test3; $(MAKE) clean

