include ../make.sys
include ../make.rules

.SUFFIXES : .x
.o.x:
	$(LD) $(LDFLAGS) -o $@ $< ../Modules/*.o $(MODULES) -L../Libs -l_invaders $(LIBS)

EXE = bands.x intf.x plot.x space.x wannier.x window.x
all: $(EXE)

clean: 
	rm -rf *.o *.a *core  work.pc work.pcl *.mod *.x

# dependencies
include .dependencies
