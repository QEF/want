
#==============================
# IOTK input-output tool kit 
#==============================

#system dipendent stuff
include ../make.sys
include ../make.rules

# specific redefinition
FFLAGS=$(FFLAGS_NOOPT)

OBJS = iotk_source1.o  iotk_source2.o \
       iotk_source3.o  iotk_source4.o \
       iotk_source5.o  iotk_source6.o \
       iotk_source7.o  iotk_source8.o \
       iotk_source9.o  iotk_source10.o \
       iotk_source11.o iotk_source12.o \
       iotk_source13.o iotk_source14.o \
       iotk_source15.o iotk_source16.o \
       iotk_source17.o 

#----------------------------------------------------

all : lib_iotk.a

lib_iotk.a : $(OBJS)
	$(AR) $(ARFLAGS) $@ $?

clean:
	rm -rf *.o *.a *core *.F90 *.mod work.pc work.pcl

# dependencies
include .dependencies
