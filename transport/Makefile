#
# Makefile for transport programs
#

include ../make.sys
include ../make.rules

all: conductor.x bulk.x

OBJS=conductor.o greena.o transfera.o greenb.o transferb.o \
readH.o setv0.o

conductor.x: $(OBJS)
	$(LD) -o conductor.x $(OBJS) ../Modules/*.o $(MODULES) $(LDFLAGS)


BOBJS = bulk.o green.o transfer.o TB_hamiltonian.o \
smooth.o scale.o setv0.o

bulk.x: $(BOBJS) 
	$(LD) -o bulk.x $(BOBJS) ../Modules/*.o $(MODULES) $(LDFLAGS)


clean:
	rm -rf *.o *.a *core  work.pc work.pcl *.mod *.x

# dependencies
#include .dependencies
