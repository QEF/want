
#==============================
# IOTK input-output tool kit 
#==============================

#system dipendent stuff
include ../make.sys
include ../make.rules

# specific redefinition
FFLAGS=$(FFLAGS_NOOPT)

OBJS = iotk_base.o  iotk_external.o  iotk_module.o

#----------------------------------------------------

all : lib_iotk.a

lib_iotk.a : $(OBJS)
	$(AR) $(ARFLAGS) $@ $?

clean:
	rm -rf *.o *.a *core *.F90 *.mod work.pc work.pcl

# dependencies
include .dependencies
