
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

sigma.x:
	$(F90) $(F90FLAGS) -o $@ sigma.f90 $(MOD_FLAG)../../../iotk -L../../../iotk -liotk

clean:
	- rm sigma.x 



