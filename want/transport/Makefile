#
#===============================
# WanT package
#===============================
#
include ../make.sys


OBJS= \
cleanup.o \
control.o \
correlation.o \
egrid.o \
green.o \
hamiltonian.o \
hamiltonian_init.o \
input.o \
input_parameters.o \
kpoints.o \
fourier_par.o \
read_matrix.o \
summary.o \
transfer.o \
transmittance.o \
workspace.o

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
	rm -rf *.o *.a *core *.F90 work.pc work.pcl *.mod *.x

# dependencies
include make.depend

