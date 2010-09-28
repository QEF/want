
#
#===============================
# WanT package
#===============================
#
include ../../../make.sys


#
# manual
#
default:
	@echo
	@echo  "  *** Wannier-Transport (WanT) package *** "
	@echo
	@echo  "     all            compile sigma.f90"
	@echo  "     clean          remove executables and objects"
	@echo

all: sigma.x

sigma.x: sigma.o
	$(F90) $(F90FLAGS) -o $@ sigma.f90  $(IOTK_INCLUDE) $(IOTK_LIBS)

clean:
	- rm sigma.x 



