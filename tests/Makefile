MAKE=make

#
# manage tests and examples
#

info:
	@echo " "
	@echo " Possible targets are: "
	@echo "     all           perform all tests"
	@echo "     want          perform WanT calculations for all tests"
	@echo "     clean         clean up" 
	@echo "     <test>        perform selected test"
	@echo " "
	@echo " List of available TESTs: "
	@echo "     test1         Silicon WFs"
	@echo "     test2         Gold chain (bulk conductance)"
	@echo "     test3         Gold chain (general geometry)"
	@echo "     test4         Aluminum chain (within disentangle)"
	@echo " "

all: test1 test2 test3 test4


test1:
	cd Test1 ; $(MAKE) run
test2:
	cd Test2 ; $(MAKE) run
test3:
	cd Test3 ; $(MAKE) run
test4:
	cd Test4 ; $(MAKE) run


want:
	cd Test1 ; $(MAKE) want
	cd Test2 ; $(MAKE) want
	cd Test3 ; $(MAKE) want
	cd Test4 ; $(MAKE) want

clean:
	cd Test1 ; $(MAKE) clean
	cd Test2 ; $(MAKE) clean
	cd Test3 ; $(MAKE) clean
	cd Test4 ; $(MAKE) clean

