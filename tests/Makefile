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
	@echo "     test5         Gold chain within USPP"
	@echo "     test6         Silicon WFs within USPP"
	@echo "     test7         Spin polarized Nichel chain (USPP)"
	@echo "     test8         Isolated bezene molecule (both NC and USPP)"
	@echo " "

all: test1 test2 test3 test4 test5 test6 test7 test8


test1:
	cd Test1 ; $(MAKE) all
test2:
	cd Test2 ; $(MAKE) all
test3:
	cd Test3 ; $(MAKE) all
test4:
	cd Test4 ; $(MAKE) all
test5:
	cd Test5 ; $(MAKE) all
test6:
	cd Test6 ; $(MAKE) all
test7:
	cd Test7 ; $(MAKE) all
test8:
	cd Test8 ; $(MAKE) all


want:
	cd Test1 ; $(MAKE) want
	cd Test2 ; $(MAKE) want
	cd Test3 ; $(MAKE) want
	cd Test4 ; $(MAKE) want
	cd Test5 ; $(MAKE) want
	cd Test6 ; $(MAKE) want
	cd Test7 ; $(MAKE) want
	cd Test8 ; $(MAKE) want

clean:
	cd Test1 ; $(MAKE) clean
	cd Test2 ; $(MAKE) clean
	cd Test3 ; $(MAKE) clean
	cd Test4 ; $(MAKE) clean
	cd Test5 ; $(MAKE) clean
	cd Test6 ; $(MAKE) clean
	cd Test7 ; $(MAKE) clean
	cd Test8 ; $(MAKE) clean

