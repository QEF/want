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
smearing.o \
summary.o \
transfer.o \
transmittance.o \
workspace.o \
write_data.o

WANNIER_OBJS= \
../wannier/compute_kham.o \
../wannier/compute_rham.o \
../wannier/control.o \
../wannier/converters.o \
../wannier/crystal_tools.o \
../wannier/datafiles.o \
../wannier/etsf_io_data.o \
../wannier/get_monkpack.o \
../wannier/get_rgrid.o \
../wannier/io_module.o \
../wannier/internal_tools.o \
../wannier/monkpack.o \
../wannier/operator.o \
../wannier/startup.o \
../wannier/smearing.o \
../wannier/shutdown.o \
../wannier/version.o \
../wannier/wannier90_tools.o 

BIN= \
conductor.x \
current.x 

EXTERNAL_LIBS= ../ctools/libctools.a ../libs/libwant.a
BIN_DEPS=      $(OBJS) $(WANNIER_OBJS) $(EXTERNAL_LIBS)

#
# main targets
#
all: $(BIN)


conductor.x: conductor.o $(BIN_DEPS)
	$(LD) $(LDFLAGS) -o $@ conductor.o \
	     $(OBJS) $(WANNIER_OBJS) $(LIBOBJS) $(LIBS)
	- ( cd ../bin ; ln -sf ../transport/$@ . )

current.x: current.o $(BIN_DEPS)
	$(LD) $(LDFLAGS) -o $@ current.o \
	     $(OBJS) $(WANNIER_OBJS) $(LIBOBJS) $(LIBS)
	- ( cd ../bin ; ln -sf ../transport/$@ . )

clean:
	rm -rf *.o *.a *core *.F90 work.pc work.pcl *.mod *.x

# dependencies
include make.depend

