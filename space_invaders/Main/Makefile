include ../make.sys

.SUFFIXES : .x
.o.x:
	$(LD) -o $@ $< ../Modules/*.o $(MODULES) $(LDFLAGS)
	- ( cd ../bin ; ln -fs ../Main/$@ . )

EXE = disentangle.x wannier.x bands.x plot.x blc2wan.x
all: $(EXE)

$(EXE): ../Modules/*.o ../iotk/libiotk.a ../Libs/libwant.a 

clean: 
	rm -rf *.o *.a *core *.F90 work.pc work.pcl *.mod *.x

# dependencies
include make.depend
