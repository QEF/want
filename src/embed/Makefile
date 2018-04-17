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
hamiltonian.o \
hamiltonian_init.o \
hamiltonian_setup.o \
input.o \
input_parameters.o \
memusage.o \
summary.o \
workspace.o

TRANSPORT_OBJS= \
../transport/egrid.o \
../transport/fourier_par.o \
../transport/gzero_maker.o \
../transport/kpoints.o \
../transport/operator_blc.o \
../transport/read_matrix.o \
../transport/smearing_T.o

BIN= \
embed.x 

LIBOBJS=  ../wannier/libwannier.a ../baselib/libbase.a ../clib/libclib.a
BIN_DEPS=  $(OBJS) $(TRANSPORT_OBJS) $(LIBOBJS)
OBJS_ALL=  $(OBJS) $(TRANSPORT_OBJS)

#
# main targets
#
all: $(BIN)


embed.x: $(BIN_DEPS) embed.o $(OBJS_ALL)
	$(LD) $(LDFLAGS) -o $@ embed.o \
	     $(OBJS_ALL) $(LIBOBJS) $(LIBS)
	- ( cd ../../bin ; ln -sf ../src/embed/$@ . )

clean:
	rm -rf *.o *.a *core *.F90 work.pc work.pcl *.mod *.x

# dependencies
include make.depend

