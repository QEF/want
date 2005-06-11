include ../make.sys

.SUFFIXES : .x
.o.x:
	$(LD) -o $@ $< ../Modules/*.o $(MODULES) $(LDFLAGS)

EXE = disentangle.x wannier.x bands.x plot.x blc2wan.x
all: $(EXE)

$(EXE): ../Libs/lib_invaders.a ../IOTK_lib/lib_iotk.a ../Modules/*.o

clean: 
	rm -rf *.o *.a *core *.F90 work.pc work.pcl *.mod *.x

# dependencies
include make.depend
