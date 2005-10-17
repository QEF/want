#
#===============================
# WanT package
#===============================
#
include ../make.sys


OBJS= \
green.o \
hamiltonian_setup.o \
read_matrix.o \
transfer.o \
transmittance.o

WANNIER_OBJS= \
../wannier/io_module.o \
../wannier/startup.o \
../wannier/version.o


EXTERNAL_LIBS= ../iotk/libiotk.a ../libs/libwant.a
BIN_DEPS=      $(OBJS) $(WANNIER_OBJS) $(EXTERNAL_LIBS)

#
# main targets
#
all: conductor.x 


conductor.x: conductor.o $(BIN_DEPS)
	$(LD) -o $@ conductor.o $(OBJS) $(WANNIER_OBJS) $(LDFLAGS)
	- ( cd ../bin ; ln -sf ../transport/$@ . )

clean:
	rm -rf *.o *.a *core  work.pc work.pcl *.mod *.x

# dependencies
include make.depend

