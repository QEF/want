#
# Makefile for transfer.f
#

include ../make.sys
include ../make.rules

all: Sconductor.x bulk.x

OBJS=Sconductor.o Sgreena.o Stransfera.o Sgreenb.o Stransferb.o \
SreadH.o setv0.o

Sconductor.x: $(OBJS)
	$(LD) -o Sconductor.x $(OBJS) ../Modules/*.o $(MODULES) $(LDFLAGS)


BOBJS = bulk.o green.o transfer.o hamiltonian.o \
smooth.o scale.o setv0.o

bulk.x: $(BOBJS) 
	$(LD) -o bulk.x $(BOBJS) ../Modules/*.o $(MODULES) $(LDFLAGS)


clean:
	rm -rf *.o *.a *core  work.pc work.pcl *.mod *.x

# dependencies
#include .dependencies
