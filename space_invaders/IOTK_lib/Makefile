
#==============================
# IOTK input-output tool kit 
#==============================

#system dipendent stuff
include ../make.sys
include ../make.rules

OBJS = iotk_base.o  iotk_external.o  iotk_module.o

#----------------------------------------------------

all : lib_iotk.a

lib_iotk.a : $(OBJS)
	$(AR) $(ARFLAGS) $@ $?

clean:
	rm -rf *.o *.a *core  *.mod work.pc work.pcl

# dependencies
include .dependencies
