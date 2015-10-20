#
#===============================
# WanT package
#===============================
#
include ../../make.sys


OBJS= \
cleanup.o \
control.o \
correlation.o \
datafiles.o \
egrid.o \
fourier_par.o \
green.o \
gzero_maker.o \
hamiltonian.o \
hamiltonian_init.o \
hamiltonian_setup.o \
input.o \
input_parameters.o \
kpoints.o \
memusage.o \
operator_blc.o \
read_matrix.o \
smearing_T.o \
summary.o \
transfer.o \
transmittance.o \
workspace.o \
write_data.o

BIN= \
conductor.x \
current.x 

LIBOBJS=../wannier/libwannier.a ../baselib/libbase.a ../clib/libclib.a
BIN_DEPS=      $(OBJS) $(LIBOBJS)

#
# main targets
#
all: $(BIN)


conductor.x: conductor.o $(BIN_DEPS)
	$(LD) $(LDFLAGS) -o $@ conductor.o \
	     $(OBJS) $(LIBOBJS) $(LIBS)
	- ( cd ../../bin ; ln -sf ../src/transport/$@ . )

current.x: current.o $(BIN_DEPS)
	$(LD) $(LDFLAGS) -o $@ current.o \
	     $(OBJS) $(LIBOBJS) $(LIBS)
	- ( cd ../../bin ; ln -sf ../src/transport/$@ . )

clean:
	rm -rf *.o *.a *core *.F90 work.pc work.pcl *.mod *.x

# dependencies
include make.depend

