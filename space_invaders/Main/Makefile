include ../make.sys
include ../make.rules

.SUFFIXES : .x
.o.x:
	$(LD) -o $@ $< ../Modules/*.o $(MODULES) $(LDFLAGS)

EXE = disentangle.x hamiltonian.x plot.x wannier.x window.x 
all: $(EXE)

$(EXE): ../Libs/lib_invaders.a

clean: 
	rm -rf *.o *.a *core *.F90 work.pc work.pcl *.mod *.x

# dependencies
include .dependencies
