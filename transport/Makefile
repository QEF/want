#
# Makefile for transport programs
#

include ../make.sys

OBJS= \
conductor.o \
green.o \
hamiltonian_setup.o \
transfer.o \
read_matrix.o \
transmittance.o

#
# main targets
#

all: conductor.x 

conductor.x: $(OBJS)
	$(LD) -o conductor.x $(OBJS) ../Modules/*.o $(MODULES) $(LDFLAGS)
	- ( cd ../bin ; ln -fs ../Transport/$@ . )


clean:
	rm -rf *.o *.a *core  work.pc work.pcl *.mod *.x

# dependencies
include make.depend

